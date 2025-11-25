use axum::{
    extract::{Path, Query, State},
    http::HeaderMap,
    response::{IntoResponse, Response},
    routing::{get, post},
    Json, Router,
};
use clap::Parser;
use std::net::SocketAddr;
use std::time::Instant;
use tokio;
use tower_http::trace::TraceLayer;
use tracing::{info, debug, warn, Level};
use tracing_subscriber;

mod ui;

use louter::config::Config;
use louter::error::ProxyError;
use louter::metrics;
use louter::routing;
use louter::diagnostic;
use louter::backends;
use louter::conversion;

#[derive(Parser, Debug)]
#[command(name = "louter")]
#[command(about = "An intelligent LLM router supporting multiple backends and protocols")]
struct Args {
    /// Backend type: "openai" or "gemini" 
    #[arg(long, default_value = "openai")]
    backend: String,
    
    /// Port to listen on
    #[arg(long, default_value = "8080")]
    port: u16,
    
    /// Configuration file path
    #[arg(long, default_value = "config.toml")]
    config: String,
    
    /// Enable verbose logging
    #[arg(long)]
    verbose: bool,

    /// JSON Lines log file path for request/response logging
    #[arg(long)]
    log_file: Option<String>,
}

#[derive(Clone)]
struct AppState {
    config: Config,
    backend_client: backends::BackendClient,
    backend_type: String,
    verbose: bool,
    logger: Option<std::sync::Arc<louter::logging::JsonLinesLogger>>,
    metrics_handle: Option<std::sync::Arc<metrics_exporter_prometheus::PrometheusHandle>>,
    log_file: Option<String>,
    diagnostics: std::sync::Arc<std::collections::HashMap<String, diagnostic::BackendDiagnostics>>,
    port: u16,  // Port the proxy is running on (for frontend diagnostics)
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();
    
    // Initialize tracing
    let level = if args.verbose { Level::DEBUG } else { Level::INFO };
    tracing_subscriber::fmt()
        .with_max_level(level)
        .init();

    info!("Starting Louter - LLM Router");
    info!("Backend: {}", args.backend);
    info!("Port: {}", args.port);

    // Load configuration
    let config = Config::load(&args.config).unwrap_or_else(|e| {
        info!("Could not load config file '{}': {}. Using defaults.", args.config, e);
        Config::default()
    });

    // Run backend diagnostics to detect capabilities
    info!("Running backend diagnostics...");
    let diagnostics = diagnostic::diagnose_all_backends(&config).await;
    diagnostic::print_diagnostics(&config, &diagnostics);

    // Verify configured capabilities match detected ones
    let warnings = diagnostic::verify_configured_capabilities(&config, &diagnostics);
    for warning in &warnings {
        warn!("{}", warning);
    }

    // Check if any backends are reachable
    let reachable_backends: Vec<_> = diagnostics.iter()
        .filter(|(_, diag)| diag.reachable)
        .map(|(name, _)| name.as_str())
        .collect();

    if reachable_backends.is_empty() {
        warn!("⚠ WARNING: No backends are currently reachable!");
        warn!("The proxy will start but requests will fail until backends are available.");
    } else {
        info!("✓ Reachable backends: {}", reachable_backends.join(", "));
    }

    // Initialize backend client
    let backend_client = backends::BackendClient::new(&args.backend, &config).await?;

    // Initialize logger if log file is specified
    let logger = args.log_file.as_ref().map(|path| {
        info!("JSON Lines logging enabled: {}", path);
        std::sync::Arc::new(louter::logging::JsonLinesLogger::new(path.clone()))
    });

    // Initialize Prometheus metrics
    let metrics_handle = match metrics::init() {
        Ok(handle) => {
            info!("Prometheus metrics initialized at /metrics endpoint");
            Some(std::sync::Arc::new(handle))
        },
        Err(e) => {
            info!("Failed to initialize Prometheus metrics: {}", e);
            None
        }
    };

    let state = AppState {
        config,
        backend_client,
        backend_type: args.backend.clone(),
        verbose: args.verbose,
        logger,
        metrics_handle: metrics_handle.clone(),
        log_file: args.log_file.clone(),
        diagnostics: std::sync::Arc::new(diagnostics),
        port: args.port,
    };

    // Build router
    let app = Router::new()
        .route("/health", get(health_check))
        .route("/metrics", get(metrics_handler))
        // UI routes
        .route("/ui", get(ui_handler_wrapper))
        .route("/ui/test", get(test_ui_handler))
        .route("/api/logs", get(api_logs_wrapper))
        .route("/api/metrics", get(api_metrics_wrapper))
        .route("/api/diagnostics", get(api_diagnostics_wrapper))
        // Gemini API routes - Models endpoint
        .route("/v1beta/models", get(list_models_handler))
        .route("/v1beta/models/*path",
            get(handle_model_get_request)
            .post(handle_model_post_request)
        )
        // OpenAI API routes
        .route("/v1/chat/completions", post(handle_openai_chat_completions))
        // Anthropic (Claude) API routes
        .route("/v1/models", get(list_anthropic_models))
        .route("/v1/messages", post(handle_anthropic_messages))
        // Catch-all route to log unmatched requests (for debugging)
        .fallback(catch_all_handler)
        .layer(TraceLayer::new_for_http())
        .with_state(state);

    // Start server
    let addr = SocketAddr::from(([0, 0, 0, 0], args.port));
    info!("Listening on {}", addr);

    let listener = tokio::net::TcpListener::bind(addr).await?;
    axum::serve(listener, app).await?;

    Ok(())
}

async fn health_check() -> impl IntoResponse {
    Json(serde_json::json!({
        "status": "ok",
        "service": "louter"
    }))
}

async fn metrics_handler(State(state): State<AppState>) -> impl IntoResponse {
    if let Some(handle) = &state.metrics_handle {
        (
            axum::http::StatusCode::OK,
            [("Content-Type", "text/plain; version=0.0.4")],
            handle.render(),
        )
    } else {
        (
            axum::http::StatusCode::SERVICE_UNAVAILABLE,
            [("Content-Type", "text/plain")],
            "Metrics not available".to_string(),
        )
    }
}

// List all available models
async fn list_models_handler(State(state): State<AppState>) -> impl IntoResponse {
    use louter::models::gemini::{Model, ModelsListResponse};

    // Return a list of models from configured backends
    let mut models = Vec::new();

    for (backend_name, backend_config) in state.config.backends.iter() {
        // Create a model entry for each configured backend model mapping
        for (gemini_model, _) in backend_config.model_mapping.iter() {
            models.push(Model {
                name: format!("models/{}", gemini_model),
                base_model_id: None,
                version: "001".to_string(),
                display_name: gemini_model.clone(),
                description: format!("Model {} via {} backend", gemini_model, backend_name),
                input_token_limit: backend_config.max_tokens.unwrap_or(32768),
                output_token_limit: backend_config.max_tokens.unwrap_or(4096),
                supported_generation_methods: vec![
                    "generateContent".to_string(),
                    "streamGenerateContent".to_string(),
                    "countTokens".to_string(),
                ],
                temperature: backend_config.temperature,
                top_p: None,
                top_k: None,
            });
        }
    }

    let response = ModelsListResponse {
        models,
        next_page_token: None,
    };

    Json(response)
}

// Handle GET requests for model info
async fn handle_model_get_request(
    State(state): State<AppState>,
    Path(path): Path<String>
) -> Result<impl IntoResponse, ProxyError> {
    use louter::models::gemini::Model;

    // Path should just be the model name (no action for GET)
    let model = path.trim_start_matches('/');

    // Find the model in the configured backends
    for (backend_name, backend_config) in state.config.backends.iter() {
        if backend_config.model_mapping.contains_key(model) {
            let model_info = Model {
                name: format!("models/{}", model),
                base_model_id: None,
                version: "001".to_string(),
                display_name: model.to_string(),
                description: format!("Model {} via {} backend", model, backend_name),
                input_token_limit: backend_config.max_tokens.unwrap_or(32768),
                output_token_limit: backend_config.max_tokens.unwrap_or(4096),
                supported_generation_methods: vec![
                    "generateContent".to_string(),
                    "streamGenerateContent".to_string(),
                    "countTokens".to_string(),
                ],
                temperature: backend_config.temperature,
                top_p: None,
                top_k: None,
            };

            return Ok(Json(model_info));
        }
    }

    Err(ProxyError::InvalidRequest(format!("Model '{}' not found", model)))
}

// API endpoint to get diagnostics data
async fn api_diagnostics_wrapper(State(state): State<AppState>) -> impl IntoResponse {
    // Run complete diagnostics (backend + frontend)
    let complete_diagnostics = diagnostic::run_complete_diagnostics(
        state.port,
        &state.config,
        (*state.diagnostics).clone()
    ).await;

    Json(complete_diagnostics)
}

// Interactive test UI
async fn test_ui_handler() -> impl IntoResponse {
    let html = include_str!("../static/test-ui.html");
    axum::response::Html(html)
}

// Handle POST requests for model actions (generateContent, streamGenerateContent, countTokens)
async fn handle_model_post_request(
    State(state): State<AppState>,
    Path(path): Path<String>,
    Query(params): Query<std::collections::HashMap<String, String>>,
    headers: HeaderMap,
    body: String,
) -> Result<Response, ProxyError> {
    // Parse the path: model_name:action
    // Path will be like "gemini-2.0-flash:generateContent"
    let path = path.trim_start_matches('/');
    let parts: Vec<&str> = path.split(':').collect();
    if parts.len() != 2 {
        return Err(ProxyError::InvalidRequest("Invalid path format. Expected format: model:action".to_string()));
    }

    let model = parts[0];
    let action = parts[1];
    // Dump body
    debug!("Request body: {}", body);
    
    match action {
        "generateContent" => {
            let request: louter::models::gemini::GeminiRequest = serde_json::from_str(&body)
                .map_err(|e| ProxyError::SerializationError(e))?;
            generate_content_impl(state, model.to_string(), params, headers, request).await
        },
        "streamGenerateContent" => {
            let request: louter::models::gemini::GeminiRequest = serde_json::from_str(&body)
                .map_err(|e| ProxyError::SerializationError(e))?;
            stream_generate_content_impl(state, model.to_string(), params, headers, request).await
        },
        "countTokens" => {
            let request: louter::models::gemini::CountTokensRequest = serde_json::from_str(&body)
                .map_err(|e| ProxyError::SerializationError(e))?;
            count_tokens_impl(state, model.to_string(), params, headers, request).await
        },
        _ => Err(ProxyError::InvalidRequest(format!("Unknown action: {}", action))),
    }
}

async fn generate_content_impl(
    state: AppState,
    model: String,
    params: std::collections::HashMap<String, String>,
    headers: HeaderMap,
    request: louter::models::gemini::GeminiRequest,
) -> Result<Response, ProxyError> {
    let start_time = Instant::now();
    let endpoint = format!("/v1beta/models/{}:generateContent", model);

    info!("Generate content request for model: {} (backend: {})", model, state.backend_type);
    debug!("Request body: {}", serde_json::to_string_pretty(&request).unwrap_or_else(|e| format!("Failed to serialize request: {}", e)));

    // Log client request
    if let Some(logger) = &state.logger {
        if let Ok(request_json) = serde_json::to_value(&request) {
            logger.log_client_request(&endpoint, "gemini", &request_json);
        }
    }

    // Content-based routing: Detect required capabilities from Gemini request
    let required_caps = routing::detect_capabilities_gemini(&request);
    let required_caps_str: Vec<_> = required_caps.iter()
        .map(|c| c.as_str())
        .collect();

    info!("📋 Request requires capabilities: [{}]", required_caps_str.join(", "));

    // Route request to appropriate backend
    // Strategy: Try model-based routing first, then fall back to capability-based routing
    let selected_backend = if let Some(backend_name) = routing::find_backend_for_model(&model, &state.config) {
        // Model found in a backend's model_mapping - use that backend
        info!("✓ Model '{}' mapped to backend '{}'", model, backend_name);

        // Verify the backend supports required capabilities
        if let Some(backend_config) = state.config.backends.get(&backend_name) {
            if routing::backend_supports_capabilities(backend_config, &required_caps) {
                info!("  ✓ Backend '{}' supports required capabilities", backend_name);
                metrics::record_routing_decision(&backend_name, &required_caps_str, "model_mapping", true);
                backend_name
            } else {
                warn!("  ✗ Backend '{}' doesn't support required capabilities", backend_name);
                warn!("  → Falling back to capability-based routing");

                // Fall back to capability-based routing
                match routing::select_backend_for_request(&required_caps, &state.config) {
                    Ok((backend_name, backend_config)) => {
                        let mode = if backend_config.capabilities.is_empty() {
                            "AUTO (accepts all)"
                        } else {
                            "EXPLICIT"
                        };

                        info!("✓ Routing selected: '{}' (mode: {}, url: {}, priority: {})",
                            backend_name, mode, backend_config.url, backend_config.priority);

                        if !backend_config.capabilities.is_empty() {
                            debug!("  Backend capabilities: [{}]", backend_config.capabilities.join(", "));
                        }

                        metrics::record_routing_decision(&backend_name, &required_caps_str, mode, true);
                        backend_name
                    },
                    Err(e) => {
                        warn!("⚠ Routing failed: {}", e);
                        info!("→ Falling back to command-line backend: {}", state.backend_type);
                        metrics::record_routing_decision("none", &required_caps_str, "failed", false);
                        state.backend_type.clone()
                    }
                }
            }
        } else {
            warn!("Backend '{}' not found in config", backend_name);
            info!("→ Falling back to command-line backend: {}", state.backend_type);
            metrics::record_routing_decision("none", &required_caps_str, "failed", false);
            state.backend_type.clone()
        }
    } else {
        // No model mapping found - use capability-based routing
        info!("Model '{}' not found in any backend model_mapping", model);
        info!("→ Using capability-based routing");

        match routing::select_backend_for_request(&required_caps, &state.config) {
            Ok((backend_name, backend_config)) => {
                let mode = if backend_config.capabilities.is_empty() {
                    "AUTO (accepts all)"
                } else {
                    "EXPLICIT"
                };

                info!("✓ Routing selected: '{}' (mode: {}, url: {}, priority: {})",
                    backend_name, mode, backend_config.url, backend_config.priority);

                if !backend_config.capabilities.is_empty() {
                    debug!("  Backend capabilities: [{}]", backend_config.capabilities.join(", "));
                }

                let mode_str = if backend_config.capabilities.is_empty() { "auto" } else { "explicit" };
                metrics::record_routing_decision(&backend_name, &required_caps_str, mode_str, true);

                backend_name
            },
            Err(e) => {
                // Routing failed (e.g., no backends with capabilities configured)
                // Fall back to the command-line specified backend_type
                warn!("⚠ Routing failed: {}", e);
                info!("→ Falling back to command-line backend: {}", state.backend_type);

                // Record failed routing decision
                metrics::record_routing_decision("none", &required_caps_str, "failed", false);

                state.backend_type.clone()
            }
        }
    };

    // Determine backend protocol type from config
    let backend_protocol_type = if let Some(backend_config) = state.config.backends.get(&selected_backend) {
        backend_config.protocol.as_str()
    } else {
        // Fall back to command-line specified backend type if backend not in config
        state.backend_type.as_str()
    };

    let result = match backend_protocol_type {
        "openai" => {
            // OpenAI backend: Convert Gemini request to OpenAI format
            // Use selected_backend for API key lookup (supports routing)
            let openai_api_key = state.config.get_api_key(&selected_backend, "OPENAI_API_KEY")
                .ok_or_else(|| ProxyError::ConfigError(format!("API key not found for backend '{}' in config or OPENAI_API_KEY environment variable", selected_backend)))?;

            let openai_request = conversion::gemini_to_openai_request(request, &model, &selected_backend, &state.config)?;

            // Log the OpenAI request for debugging
            if state.verbose {
                debug!("OpenAI request: {}", serde_json::to_string_pretty(&openai_request).unwrap_or_else(|_| "<failed to serialize>".to_string()));
            }

            // Log backend request
            if let Some(logger) = &state.logger {
                if let Ok(request_json) = serde_json::to_value(&openai_request) {
                    logger.log_backend_request("/v1/chat/completions", "openai", &request_json);
                }
            }

            let openai_response = state.backend_client.chat_completion(&selected_backend, openai_request.clone(), &openai_api_key).await?;

            // Log the OpenAI response for debugging
            if state.verbose {
                debug!("OpenAI response: {}", serde_json::to_string_pretty(&openai_response).unwrap_or_else(|_| "<failed to serialize>".to_string()));
            }

            // Log backend response
            if let Some(logger) = &state.logger {
                if let Ok(response_json) = serde_json::to_value(&openai_response) {
                    logger.log_backend_response("/v1/chat/completions", "openai", &response_json);
                }
            }

            let gemini_response = conversion::openai_to_gemini_response(openai_response.clone())?;

            // Calculate metrics
            let e2e_latency_ms = start_time.elapsed().as_secs_f64() * 1000.0;
            let output_tokens = gemini_response.usage_metadata.as_ref()
                .and_then(|u| u.candidates_token_count);
            let input_tokens = gemini_response.usage_metadata.as_ref()
                .map(|u| u.prompt_token_count);

            // For non-streaming, TTFT ≈ e2e_latency (we get full response at once)
            let ttft_ms = Some(e2e_latency_ms);

            // Log client response with metrics
            if let Some(logger) = &state.logger {
                if let Ok(response_json) = serde_json::to_value(&gemini_response) {
                    logger.log_client_response_with_metrics(
                        &format!("/v1beta/models/{}:generateContent", model),
                        "gemini",
                        &response_json,
                        ttft_ms,
                        Some(e2e_latency_ms),
                        output_tokens,
                        input_tokens,
                    );
                }
            }

            // Print metrics if verbose
            if state.verbose {
                info!("Metrics: e2e={:.2}ms, output_tokens={:?}, input_tokens={:?}",
                    e2e_latency_ms, output_tokens, input_tokens);
                if let (Some(tokens), Some(_ttft)) = (output_tokens, ttft_ms) {
                    let generation_time_s = (e2e_latency_ms - ttft_ms.unwrap_or(0.0)) / 1000.0;
                    if generation_time_s > 0.0 {
                        let tps = tokens as f64 / generation_time_s;
                        info!("TPS: {:.2} tokens/sec", tps);
                    }
                }
            }

            // Record performance metrics to Prometheus
            let ttft_seconds = ttft_ms.map(|ms| ms / 1000.0);
            let tps_value = if let (Some(tokens), Some(ttft)) = (output_tokens, ttft_ms) {
                let generation_time_ms = e2e_latency_ms - ttft;
                if generation_time_ms > 0.001 {
                    Some(tokens as f64 / (generation_time_ms / 1000.0))
                } else {
                    Some(tokens as f64 / (e2e_latency_ms / 1000.0))
                }
            } else {
                None
            };
            let itl_seconds = if let (Some(tokens), Some(ttft)) = (output_tokens, ttft_ms) {
                if tokens > 1 {
                    Some((e2e_latency_ms - ttft) / (tokens as f64 - 1.0) / 1000.0)
                } else {
                    None
                }
            } else {
                None
            };
            metrics::record_performance_metrics(&endpoint, "openai", ttft_seconds, tps_value, itl_seconds);

            Ok(Json(gemini_response).into_response())
        },
        "gemini" => {
            // Gemini backend: Pass-through mode - forward directly to Gemini API
            let gemini_api_key = state.config.get_api_key("gemini", "GEMINI_API_KEY")
                .ok_or_else(|| ProxyError::ConfigError("Gemini API key not found in config or GEMINI_API_KEY environment variable".to_string()))?;

            // Log backend request (same as client request in pass-through mode)
            if let Some(logger) = &state.logger {
                if let Ok(request_json) = serde_json::to_value(&request) {
                    logger.log_backend_request(&format!("/v1beta/models/{}:generateContent", model), "gemini", &request_json);
                }
            }

            let gemini_response = state.backend_client.gemini_generate_content(request, &model, &gemini_api_key).await?;

            // Log backend response
            if let Some(logger) = &state.logger {
                if let Ok(response_json) = serde_json::to_value(&gemini_response) {
                    logger.log_backend_response(&format!("/v1beta/models/{}:generateContent", model), "gemini", &response_json);
                }
            }

            // Log client response (same as backend response in pass-through mode)
            if let Some(logger) = &state.logger {
                if let Ok(response_json) = serde_json::to_value(&gemini_response) {
                    logger.log_client_response(&format!("/v1beta/models/{}:generateContent", model), "gemini", &response_json);
                }
            }

            Ok(Json(gemini_response).into_response())
        },
        _ => {
            Err(ProxyError::ConfigError(format!("Unsupported backend type: {}", state.backend_type)))
        }
    };

    // Record metrics
    let duration = start_time.elapsed();
    let status = if result.is_ok() { 200 } else { 500 };

    metrics::record_request(&endpoint, "POST", status, duration);

    if result.is_err() {
        metrics::record_error("request_error", &endpoint);
    }

    result
}

async fn stream_generate_content_impl(
    state: AppState,
    model: String,
    params: std::collections::HashMap<String, String>,
    headers: HeaderMap,
    request: louter::models::gemini::GeminiRequest,
) -> Result<Response, ProxyError> {
    info!("Stream generate content request for model: {} (backend: {})", model, state.backend_type);
    debug!("Stream request body: {}", serde_json::to_string_pretty(&request).unwrap_or_else(|e| format!("Failed to serialize request: {}", e)));

    // Use routing to select backend based on capabilities
    let selected_backend = {
        let required_caps = routing::detect_capabilities_gemini(&request);
        let required_caps_str: Vec<_> = required_caps.iter()
            .map(|c| c.as_str())
            .collect();

        info!("📋 Stream request requires capabilities: [{}]", required_caps_str.join(", "));

        match routing::select_backend_for_request(&required_caps, &state.config) {
            Ok((backend_name, backend_config)) => {
                let mode = if backend_config.capabilities.is_empty() {
                    "AUTO (accepts all)"
                } else {
                    "EXPLICIT"
                };

                info!("✓ Routing selected: '{}' (mode: {}, url: {}, priority: {})",
                    backend_name, mode, backend_config.url, backend_config.priority);

                metrics::record_routing_decision(&backend_name, &required_caps_str,
                    if backend_config.capabilities.is_empty() { "auto" } else { "explicit" }, true);

                backend_name
            },
            Err(e) => {
                warn!("⚠ Routing failed: {}", e);
                info!("→ Falling back to command-line backend: {}", state.backend_type);
                metrics::record_routing_decision("none", &required_caps_str, "failed", false);
                state.backend_type.clone()
            }
        }
    };

    // Determine backend protocol type from config
    let backend_protocol_type = if let Some(backend_config) = state.config.backends.get(&selected_backend) {
        backend_config.protocol.as_str()
    } else {
        // Fall back to command-line specified backend type if backend not in config
        state.backend_type.as_str()
    };

    match backend_protocol_type {
        "openai" => {
            // OpenAI backend: Convert to OpenAI streaming format
            let openai_api_key = state.config.get_api_key(&selected_backend, "OPENAI_API_KEY")
                .ok_or_else(|| ProxyError::ConfigError("OpenAI API key not found in config or OPENAI_API_KEY environment variable".to_string()))?;

            let mut openai_request = conversion::gemini_to_openai_request(request, &model, &selected_backend, &state.config)?;
            openai_request.stream = Some(true);
            
            let stream = state.backend_client.chat_completion_stream(&selected_backend, openai_request, &openai_api_key).await?;
            let gemini_stream = conversion::openai_stream_to_gemini_sse(stream);
            
            Ok(axum::response::Sse::new(gemini_stream).into_response())
        },
        "gemini" => {
            // Gemini backend: Pass-through streaming
            let gemini_api_key = state.config.get_api_key("gemini", "GEMINI_API_KEY")
                .ok_or_else(|| ProxyError::ConfigError("Gemini API key not found in config or GEMINI_API_KEY environment variable".to_string()))?;

            // Log backend request (same as client request in pass-through mode)
            if let Some(logger) = &state.logger {
                if let Ok(request_json) = serde_json::to_value(&request) {
                    logger.log_backend_request(&format!("/v1beta/models/{}:streamGenerateContent", model), "gemini", &request_json);
                }
            }

            let stream = state.backend_client.gemini_stream_generate_content(request, &model, &gemini_api_key).await?;

            // Convert the raw SSE stream to Axum SSE format
            // Note: Streaming responses are logged as they arrive in the stream
            use futures_util::StreamExt;
            let logger = state.logger.clone();
            let model_clone = model.clone();
            let sse_stream = stream.map(move |result| {
                match result {
                    Ok(line) => {
                        // Log each stream chunk if logger is available
                        if let Some(ref logger) = logger {
                            if let Ok(chunk_json) = serde_json::from_str::<serde_json::Value>(&line) {
                                logger.log_backend_response(&format!("/v1beta/models/{}:streamGenerateContent", model_clone), "gemini", &chunk_json);
                                logger.log_client_response(&format!("/v1beta/models/{}:streamGenerateContent", model_clone), "gemini", &chunk_json);
                            }
                        }
                        Ok(axum::response::sse::Event::default().data(line))
                    },
                    Err(e) => Err(axum::Error::new(e)),
                }
            });

            Ok(axum::response::Sse::new(sse_stream).into_response())
        },
        _ => {
            Err(ProxyError::ConfigError(format!("Unsupported backend type: {}", state.backend_type)))
        }
    }
}

async fn count_tokens_impl(
    state: AppState,
    model: String,
    params: std::collections::HashMap<String, String>,
    headers: HeaderMap,
    request: louter::models::gemini::CountTokensRequest,
) -> Result<Response, ProxyError> {
    info!("Count tokens request for model: {} (backend: {})", model, state.backend_type);
    
    match state.backend_type.as_str() {
        "openai" => {
            // OpenAI backend: Support both estimation and chat modes
            let temp_request = louter::models::gemini::GeminiRequest {
                contents: request.contents,
                system_instruction: request.system_instruction,
                safety_settings: None,
                generation_config: None,
                tools: request.tools,
                tool_config: None,
            };

            let openai_request = conversion::gemini_to_openai_request(temp_request, &model, &state.backend_type, &state.config)?;
            
            // Check token counting mode from config
            let counting_mode = state.config.performance.openai_token_counting_mode
                .as_deref()
                .unwrap_or("estimate");
            
            let total_tokens = match counting_mode {
                "chat" => {
                    // Chat mode: Get actual token count from OpenAI API
                    let openai_api_key = state.config.get_api_key("openai", "OPENAI_API_KEY")
                        .ok_or_else(|| ProxyError::ConfigError("OpenAI API key not found in config or OPENAI_API_KEY environment variable".to_string()))?;
                    
                    let openai_response = state.backend_client.chat_completion(&state.backend_type, openai_request, &openai_api_key).await?;
                    
                    // Log the OpenAI response for debugging
                    if state.verbose {
                        debug!("OpenAI countTokens response: {}", serde_json::to_string_pretty(&openai_response).unwrap_or_else(|_| "<failed to serialize>".to_string()));
                    }
                    openai_response.usage.total_tokens
                },
                "estimate" | _ => {
                    // Estimate mode: Fast token estimation without API call
                    estimate_tokens_from_openai_request(&openai_request)
                }
            };
            
            let response = louter::models::gemini::CountTokensResponse {
                total_tokens,
            };
            
            Ok(Json(response).into_response())
        },
        "gemini" => {
            // Gemini backend: Pass-through to real Gemini countTokens API
            let gemini_api_key = state.config.get_api_key("gemini", "GEMINI_API_KEY")
                .ok_or_else(|| ProxyError::ConfigError("Gemini API key not found in config or GEMINI_API_KEY environment variable".to_string()))?;
            let response = state.backend_client.gemini_count_tokens(request, &model, &gemini_api_key).await?;
            Ok(Json(response).into_response())
        },
        _ => {
            Err(ProxyError::ConfigError(format!("Unsupported backend type: {}", state.backend_type)))
        }
    }
}


fn estimate_tokens_from_openai_request(request: &louter::models::openai::OpenAIRequest) -> i32 {
    let mut token_count = 0;
    
    // Estimate tokens for each message
    for message in &request.messages {
        match message {
            louter::models::openai::Message::System { content, .. } => {
                token_count += estimate_text_tokens(content);
            },
            louter::models::openai::Message::User { content, .. } => {
                token_count += estimate_message_content_tokens(content);
            },
            louter::models::openai::Message::Assistant { content, .. } => {
                if let Some(content) = content {
                    token_count += estimate_text_tokens(content);
                }
            },
            louter::models::openai::Message::Tool { content, .. } => {
                token_count += estimate_text_tokens(content);
            },
        }
        
        // Add overhead for message formatting
        token_count += 4; // Rough overhead per message
    }
    
    // Add tokens for tools if present
    if let Some(tools) = &request.tools {
        for tool in tools {
            token_count += estimate_text_tokens(&tool.function.name);
            if let Some(description) = &tool.function.description {
                token_count += estimate_text_tokens(description);
            }
            if let Some(parameters) = &tool.function.parameters {
                // Rough estimate for JSON schema
                let param_str = serde_json::to_string(parameters).unwrap_or_default();
                token_count += estimate_text_tokens(&param_str);
            }
        }
    }
    
    std::cmp::max(1, token_count) // Ensure at least 1 token
}

fn estimate_text_tokens(text: &str) -> i32 {
    // Simple approximation: 1 token ≈ 4 characters for English text
    // This is a rough estimate - real tokenizers are more complex
    let char_count = text.chars().count() as i32;
    std::cmp::max(1, (char_count + 3) / 4) // Round up
}

fn estimate_message_content_tokens(content: &louter::models::openai::MessageContent) -> i32 {
    match content {
        louter::models::openai::MessageContent::Text(text) => estimate_text_tokens(text),
        louter::models::openai::MessageContent::Array(parts) => {
            let mut total = 0;
            for part in parts {
                match part {
                    louter::models::openai::ContentPart::Text { text } => {
                        total += estimate_text_tokens(text);
                    },
                    louter::models::openai::ContentPart::ImageUrl { .. } => {
                        // Images typically cost around 85-255 tokens depending on size
                        total += 170; // Average estimate
                    },
                }
            }
            total
        }
    }
}

async fn handle_openai_chat_completions(
    State(state): State<AppState>,
    _headers: HeaderMap,
    Json(request): Json<louter::models::openai::OpenAIRequest>,
) -> Result<Response, ProxyError> {
    use louter::ir::traits::{FrontendConverter, BackendConverter};

    let start_time = Instant::now();

    info!("OpenAI chat completions request for model: {}", request.model);
    debug!("OpenAI request body: {}", serde_json::to_string_pretty(&request).unwrap_or_else(|e| format!("Failed to serialize request: {}", e)));

    // Log client request
    if let Some(logger) = &state.logger {
        if let Ok(request_json) = serde_json::to_value(&request) {
            logger.log_client_request("/v1/chat/completions", "openai", &request_json);
        }
    }

    // Content-based routing: Detect required capabilities from OpenAI request
    let required_caps = routing::detect_capabilities_openai(&request);
    let required_caps_str: Vec<_> = required_caps.iter()
        .map(|c| c.as_str())
        .collect();

    info!("📋 Request requires capabilities: [{}]", required_caps_str.join(", "));

    // Route request to appropriate backend
    // Strategy: Try model-based routing first, then fall back to capability-based routing
    let selected_backend = if let Some(backend_name) = routing::find_backend_for_model(&request.model, &state.config) {
        // Model found in a backend's model_mapping - use that backend
        info!("✓ Model '{}' mapped to backend '{}'", request.model, backend_name);

        // Verify the backend supports required capabilities
        if let Some(backend_config) = state.config.backends.get(&backend_name) {
            let supports_caps = routing::backend_supports_capabilities(backend_config, &required_caps);
            if supports_caps {
                info!("  ✓ Backend '{}' supports required capabilities", backend_name);
                metrics::record_routing_decision(&backend_name, &required_caps_str, "model_mapping", true);
                backend_name
            } else {
                warn!("  ✗ Backend '{}' does NOT support required capabilities [{}]", backend_name, required_caps_str.join(", "));
                warn!("  → Falling back to capability-based routing");

                // Fall back to capability-based routing
                match routing::select_backend_for_request(&required_caps, &state.config) {
                    Ok((backend_name, backend_config)) => {
                        let mode = if backend_config.capabilities.is_empty() { "AUTO (accepts all)" } else { "EXPLICIT" };
                        info!("✓ Routing selected: '{}' (mode: {}, url: {}, priority: {})",
                            backend_name, mode, backend_config.url, backend_config.priority);
                        metrics::record_routing_decision(&backend_name, &required_caps_str, "capability", true);
                        backend_name
                    },
                    Err(e) => {
                        warn!("⚠ Routing failed: {}", e);
                        info!("→ Falling back to command-line backend: {}", state.backend_type);
                        metrics::record_routing_decision("none", &required_caps_str, "failed", false);
                        state.backend_type.clone()
                    }
                }
            }
        } else {
            warn!("  ✗ Backend '{}' not found in config", backend_name);
            warn!("  → Falling back to capability-based routing");

            // Backend not in config, fall back to capability-based routing
            match routing::select_backend_for_request(&required_caps, &state.config) {
                Ok((backend_name, backend_config)) => {
                    let mode = if backend_config.capabilities.is_empty() { "AUTO (accepts all)" } else { "EXPLICIT" };
                    info!("✓ Routing selected: '{}' (mode: {}, url: {}, priority: {})",
                        backend_name, mode, backend_config.url, backend_config.priority);
                    metrics::record_routing_decision(&backend_name, &required_caps_str, "capability", true);
                    backend_name
                },
                Err(e) => {
                    warn!("⚠ Routing failed: {}", e);
                    info!("→ Falling back to command-line backend: {}", state.backend_type);
                    metrics::record_routing_decision("none", &required_caps_str, "failed", false);
                    state.backend_type.clone()
                }
            }
        }
    } else {
        // No model mapping found, use capability-based routing
        debug!("No model mapping found for '{}', using capability-based routing", request.model);

        match routing::select_backend_for_request(&required_caps, &state.config) {
            Ok((backend_name, backend_config)) => {
                let mode = if backend_config.capabilities.is_empty() {
                    "AUTO (accepts all)"
                } else {
                    "EXPLICIT"
                };

                info!("✓ Routing selected: '{}' (mode: {}, url: {}, priority: {})",
                    backend_name, mode, backend_config.url, backend_config.priority);

                if !backend_config.capabilities.is_empty() {
                    debug!("  Backend capabilities: [{}]", backend_config.capabilities.join(", "));
                }

                // Record routing decision metrics
                let mode_str = if backend_config.capabilities.is_empty() { "auto" } else { "explicit" };
                metrics::record_routing_decision(&backend_name, &required_caps_str, mode_str, true);

                backend_name
            },
            Err(e) => {
                // Routing failed (e.g., no backends with capabilities configured)
                // Fall back to the command-line specified backend_type
                warn!("⚠ Routing failed: {}", e);
                info!("→ Falling back to command-line backend: {}", state.backend_type);

                // Record failed routing decision
                metrics::record_routing_decision("none", &required_caps_str, "failed", false);

                state.backend_type.clone()
            }
        }
    };

    // Determine backend protocol type from config
    let backend_protocol_type = if let Some(backend_config) = state.config.backends.get(&selected_backend) {
        backend_config.protocol.as_str()
    } else {
        // Fall back to command-line specified backend type if backend not in config
        state.backend_type.as_str()
    };

    match backend_protocol_type {
        "openai" => {
            // OpenAI backend: Pass-through mode
            // Use selected_backend for API key lookup (supports routing)
            let openai_api_key = state.config.get_api_key(&selected_backend, "OPENAI_API_KEY")
                .ok_or_else(|| ProxyError::ConfigError(format!("API key not found for backend '{}' in config or OPENAI_API_KEY environment variable", selected_backend)))?;

            // Map model name if configured (e.g., "gemini-2.0-flash" → "gpt-4")
            let mut mapped_request = request.clone();
            if let Some(backend_config) = state.config.backends.get(&selected_backend) {
                if let Some(mapped_model) = backend_config.model_mapping.get(&request.model) {
                    info!("Mapping model '{}' → '{}' for OpenAI backend", request.model, mapped_model);
                    mapped_request.model = mapped_model.clone();
                }
            }

            // Log backend request (may have mapped model)
            if let Some(logger) = &state.logger {
                if let Ok(request_json) = serde_json::to_value(&mapped_request) {
                    logger.log_backend_request("/v1/chat/completions", "openai", &request_json);
                }
            }

            let is_streaming = mapped_request.stream.unwrap_or(false);

            if is_streaming {
                let stream = state.backend_client.chat_completion_stream(&selected_backend, mapped_request, &openai_api_key).await?;
                let sse_stream = conversion::openai_stream_to_sse(stream);
                Ok(axum::response::Sse::new(sse_stream).into_response())
            } else {
                let response = state.backend_client.chat_completion(&selected_backend, mapped_request, &openai_api_key).await?;

                // Log backend response
                if let Some(logger) = &state.logger {
                    if let Ok(response_json) = serde_json::to_value(&response) {
                        logger.log_backend_response("/v1/chat/completions", "openai", &response_json);
                    }
                }

                // Calculate metrics
                let e2e_latency_ms = start_time.elapsed().as_secs_f64() * 1000.0;
                let output_tokens = Some(response.usage.completion_tokens);
                let input_tokens = Some(response.usage.prompt_tokens);
                let ttft_ms = Some(e2e_latency_ms); // Non-streaming: TTFT ≈ e2e_latency

                // Log client response with metrics
                if let Some(logger) = &state.logger {
                    if let Ok(response_json) = serde_json::to_value(&response) {
                        logger.log_client_response_with_metrics(
                            "/v1/chat/completions",
                            "openai",
                            &response_json,
                            ttft_ms,
                            Some(e2e_latency_ms),
                            output_tokens,
                            input_tokens,
                        );
                    }
                }

                // Print metrics if verbose
                if state.verbose {
                    info!("Metrics: e2e={:.2}ms, output_tokens={:?}, input_tokens={:?}",
                        e2e_latency_ms, output_tokens, input_tokens);
                    if let Some(tokens) = output_tokens {
                        let generation_time_s = e2e_latency_ms / 1000.0;
                        if generation_time_s > 0.0 {
                            let tps = tokens as f64 / generation_time_s;
                            info!("TPS: {:.2} tokens/sec", tps);
                        }
                    }
                }

                // Record performance metrics to Prometheus
                let ttft_seconds = ttft_ms.map(|ms| ms / 1000.0);
                let tps_value = output_tokens.map(|tokens| tokens as f64 / (e2e_latency_ms / 1000.0));
                let itl_seconds = output_tokens.map(|tokens| {
                    if tokens > 1 {
                        e2e_latency_ms / (tokens as f64 - 1.0) / 1000.0
                    } else {
                        0.0
                    }
                });
                metrics::record_performance_metrics("/v1/chat/completions", "openai", ttft_seconds, tps_value, itl_seconds);

                Ok(Json(response).into_response())
            }
        },
        "gemini" => {
            // Gemini backend: Convert OpenAI request to Gemini format
            // Use selected_backend for API key lookup (supports routing)
            let gemini_api_key = state.config.get_api_key(&selected_backend, "GEMINI_API_KEY")
                .ok_or_else(|| ProxyError::ConfigError(format!("API key not found for backend '{}' in config or GEMINI_API_KEY environment variable", selected_backend)))?;

            // Map OpenAI model to Gemini model
            let gemini_model = state.config.map_model("gemini", &request.model)
                .unwrap_or_else(|| "gemini-2.0-flash".to_string());

            // Convert OpenAI request to Gemini format
            let gemini_request = conversion::openai_to_gemini_request(request.clone(), &gemini_model, &state.config)?;

            // Log backend request
            if let Some(logger) = &state.logger {
                if let Ok(request_json) = serde_json::to_value(&gemini_request) {
                    logger.log_backend_request(
                        &format!("/v1beta/models/{}:generateContent", gemini_model),
                        "gemini",
                        &request_json
                    );
                }
            }

            let is_streaming = request.stream.unwrap_or(false);

            if is_streaming {
                let stream = state.backend_client.gemini_stream_generate_content(gemini_request, &gemini_model, &gemini_api_key).await?;

                // Convert Gemini SSE stream to OpenAI SSE stream
                let openai_stream = conversion::gemini_raw_stream_to_openai_sse(stream, &request.model);
                Ok(axum::response::Sse::new(openai_stream).into_response())
            } else {
                let gemini_response = state.backend_client.gemini_generate_content(gemini_request, &gemini_model, &gemini_api_key).await?;

                // Log backend response
                if let Some(logger) = &state.logger {
                    if let Ok(response_json) = serde_json::to_value(&gemini_response) {
                        logger.log_backend_response(
                            &format!("/v1beta/models/{}:generateContent", gemini_model),
                            "gemini",
                            &response_json
                        );
                    }
                }

                // Convert Gemini response to OpenAI format
                let openai_response = conversion::gemini_to_openai_response(gemini_response, &request.model)?;

                // Log client response
                if let Some(logger) = &state.logger {
                    if let Ok(response_json) = serde_json::to_value(&openai_response) {
                        logger.log_client_response("/v1/chat/completions", "openai", &response_json);
                    }
                }

                Ok(Json(openai_response).into_response())
            }
        },
        _ => {
            Err(ProxyError::ConfigError(format!("Unsupported backend type: {}", state.backend_type)))
        }
    }
}

// Anthropic (Claude) API endpoint
async fn handle_anthropic_messages(
    State(state): State<AppState>,
    headers: HeaderMap,
    Json(request): Json<louter::models::anthropic::MessagesRequest>,
) -> Result<Response, ProxyError> {
    use louter::ir::traits::{FrontendConverter, BackendConverter};

    let start_time = Instant::now();
    let original_model = request.model.clone();

    // Generate unique request ID for correlation
    let request_id = uuid::Uuid::new_v4().to_string();

    info!("Anthropic messages request for model: {} (request_id: {})", request.model, request_id);
    debug!("Anthropic request body: {}", serde_json::to_string_pretty(&request).unwrap_or_else(|e| format!("Failed to serialize request: {}", e)));

    // Log client request with full details
    if let Some(logger) = &state.logger {
        if let Ok(request_json) = serde_json::to_value(&request) {
            // Capture headers
            let mut headers_map = std::collections::HashMap::new();
            for (key, value) in headers.iter() {
                if let Ok(value_str) = value.to_str() {
                    headers_map.insert(key.to_string(), value_str.to_string());
                }
            }

            logger.log_client_request_detailed(
                "/v1/messages",
                "anthropic",
                &request_json,
                "POST",
                headers_map,
                request_id.clone(),
                request.stream.unwrap_or(false),
            );
        }
    }

    // Helper to classify error type
    let error_type_from_error = |error: &ProxyError| -> &str {
        match error {
            ProxyError::InvalidRequest(_) => "invalid_request",
            ProxyError::BackendError(_) => "backend_error",
            ProxyError::ConversionError(_) => "conversion_error",
            ProxyError::ConfigError(_) => "config_error",
            ProxyError::SerializationError(_) => "serialization_error",
        }
    };

    // Content-based routing: Detect required capabilities
    let required_caps: std::collections::HashSet<_> = if request.tools.is_some() {
        vec![routing::Capability::FunctionCalling].into_iter().collect()
    } else {
        vec![routing::Capability::Text].into_iter().collect()
    };
    let required_caps_str: Vec<_> = required_caps.iter()
        .map(|c| c.as_str())
        .collect();

    info!("📋 Request requires capabilities: [{}]", required_caps_str.join(", "));

    // Route to appropriate backend
    let selected_backend = match routing::select_backend_for_request(&required_caps, &state.config) {
        Ok((backend_name, backend_config)) => {
            let mode = if backend_config.capabilities.is_empty() { "AUTO (accepts all)" } else { "EXPLICIT" };
            info!("✓ Routing selected: '{}' (mode: {}, url: {}, priority: {})",
                backend_name, mode, backend_config.url, backend_config.priority);
            metrics::record_routing_decision(&backend_name, &required_caps_str, "capability", true);
            backend_name
        },
        Err(e) => {
            warn!("⚠ Routing failed: {}", e);
            info!("→ Falling back to command-line backend: {}", state.backend_type);
            metrics::record_routing_decision("none", &required_caps_str, "failed", false);
            state.backend_type.clone()
        }
    };

    // Convert Anthropic request to OpenAI format using IR converters
    let frontend_converter = louter::ir::converters::anthropic_frontend::AnthropicFrontendConverter;
    let backend_converter = louter::ir::converters::openai_backend::OpenAIBackendConverter;

    // Serialize Anthropic request to bytes
    let request_bytes = match serde_json::to_vec(&request) {
        Ok(bytes) => bytes,
        Err(e) => {
            let error = ProxyError::SerializationError(e);
            if let Some(logger) = &state.logger {
                if let Ok(request_json) = serde_json::to_value(&request) {
                    logger.log_error(
                        "/v1/messages",
                        "anthropic",
                        &error.to_string(),
                        "serialization_error",
                        Some(&request_json),
                        Some(request_id.clone()),
                        None,
                    );
                }
            }
            return Err(error);
        }
    };

    // Parse Anthropic request to IR
    let ir_request = match frontend_converter.parse_request(&request_bytes).await {
        Ok(ir) => ir,
        Err(e) => {
            if let Some(logger) = &state.logger {
                if let Ok(request_json) = serde_json::to_value(&request) {
                    logger.log_error(
                        "/v1/messages",
                        "anthropic",
                        &e.to_string(),
                        error_type_from_error(&e),
                        Some(&request_json),
                        Some(request_id.clone()),
                        None,
                    );
                }
            }
            return Err(e);
        }
    };

    // Format IR to OpenAI request
    let openai_request_bytes = match backend_converter.format_request(&ir_request).await {
        Ok(bytes) => bytes,
        Err(e) => {
            if let Some(logger) = &state.logger {
                if let Ok(request_json) = serde_json::to_value(&request) {
                    logger.log_error(
                        "/v1/messages",
                        "anthropic",
                        &e.to_string(),
                        error_type_from_error(&e),
                        Some(&request_json),
                        Some(request_id.clone()),
                        None,
                    );
                }
            }
            return Err(e);
        }
    };

    // Deserialize OpenAI request
    let openai_request: louter::models::openai::OpenAIRequest = match serde_json::from_slice(&openai_request_bytes) {
        Ok(req) => req,
        Err(e) => {
            let error = ProxyError::SerializationError(e);
            if let Some(logger) = &state.logger {
                if let Ok(request_json) = serde_json::to_value(&request) {
                    logger.log_error(
                        "/v1/messages",
                        "anthropic",
                        &error.to_string(),
                        "serialization_error",
                        Some(&request_json),
                        Some(request_id.clone()),
                        None,
                    );
                }
            }
            return Err(error);
        }
    };

    // Get API key for selected backend
    let openai_api_key = match state.config.get_api_key(&selected_backend, "OPENAI_API_KEY") {
        Some(key) => key,
        None => {
            let error = ProxyError::ConfigError(format!("API key not found for backend '{}' in config or OPENAI_API_KEY environment variable", selected_backend));
            // Log config error
            if let Some(logger) = &state.logger {
                if let Ok(request_json) = serde_json::to_value(&request) {
                    logger.log_error(
                        "/v1/messages",
                        "anthropic",
                        &error.to_string(),
                        "config_error",
                        Some(&request_json),
                        Some(request_id.clone()),
                        None,
                    );
                }
            }
            return Err(error);
        }
    };

    // Log backend request
    if let Some(logger) = &state.logger {
        if let Ok(request_json) = serde_json::to_value(&openai_request) {
            logger.log_backend_request("/v1/chat/completions", "openai", &request_json);
        }
    }

    let is_streaming = request.stream.unwrap_or(false);

    if is_streaming {
        // Streaming mode
        let mut streaming_request = openai_request;
        streaming_request.stream = Some(true);

        let stream = match state.backend_client.chat_completion_stream(&selected_backend, streaming_request, &openai_api_key).await {
            Ok(s) => s,
            Err(e) => {
                // Log backend error
                if let Some(logger) = &state.logger {
                    if let Ok(request_json) = serde_json::to_value(&request) {
                        logger.log_error(
                            "/v1/messages",
                            "anthropic",
                            &e.to_string(),
                            "backend_error",
                            Some(&request_json),
                            Some(request_id.clone()),
                            None,
                        );
                    }
                }
                return Err(e);
            }
        };

        // Convert OpenAI stream to Anthropic SSE format using IR converters
        use futures::StreamExt;

        // Wrap frontend converter in Arc for stream usage
        let frontend_converter_arc = std::sync::Arc::new(frontend_converter);
        let backend_converter_arc = std::sync::Arc::new(backend_converter);
        let original_model_clone = original_model.clone();

        // Convert OpenAI stream responses to IR chunks, then to Anthropic SSE
        let anthropic_stream = stream.map(move |response_result| {
            match response_result {
                Ok(openai_stream_response) => {
                    // Serialize OpenAI stream response
                    let response_bytes = match serde_json::to_vec(&openai_stream_response) {
                        Ok(bytes) => bytes,
                        Err(e) => return Err(std::io::Error::new(std::io::ErrorKind::InvalidData,
                            format!("Failed to serialize OpenAI stream response: {}", e))),
                    };

                    // Parse to IR chunk
                    let ir_chunk_opt = match backend_converter_arc.parse_stream_chunk(&response_bytes) {
                        Ok(opt) => opt,
                        Err(e) => return Err(std::io::Error::new(std::io::ErrorKind::InvalidData,
                            format!("Failed to parse IR chunk: {}", e))),
                    };

                    // Convert IR chunk to Anthropic SSE if present
                    if let Some(ir_chunk) = ir_chunk_opt {
                        match frontend_converter_arc.format_stream_chunk(&ir_chunk) {
                            Ok(sse_data) => Ok(axum::response::sse::Event::default().data(sse_data)),
                            Err(e) => Err(std::io::Error::new(std::io::ErrorKind::InvalidData, e.to_string())),
                        }
                    } else {
                        // Skip if no IR chunk (e.g., empty delta)
                        Ok(axum::response::sse::Event::default().comment("skipped"))
                    }
                },
                Err(e) => Err(std::io::Error::new(std::io::ErrorKind::Other, e.to_string())),
            }
        });

        Ok(axum::response::Sse::new(anthropic_stream).into_response())
    } else {
        // Non-streaming mode
        let openai_response = match state.backend_client.chat_completion(&selected_backend, openai_request, &openai_api_key).await {
            Ok(resp) => resp,
            Err(e) => {
                // Log backend error
                if let Some(logger) = &state.logger {
                    if let Ok(request_json) = serde_json::to_value(&request) {
                        logger.log_error(
                            "/v1/messages",
                            "anthropic",
                            &e.to_string(),
                            "backend_error",
                            Some(&request_json),
                            Some(request_id.clone()),
                            None,
                        );
                    }
                }
                return Err(e);
            }
        };

        // Log backend response
        if let Some(logger) = &state.logger {
            if let Ok(response_json) = serde_json::to_value(&openai_response) {
                logger.log_backend_response("/v1/chat/completions", "openai", &response_json);
            }
        }

        // Convert OpenAI response to Anthropic format using IR converters
        // Serialize OpenAI response
        let openai_response_bytes = match serde_json::to_vec(&openai_response) {
            Ok(bytes) => bytes,
            Err(e) => {
                let error = ProxyError::SerializationError(e);
                if let Some(logger) = &state.logger {
                    if let Ok(request_json) = serde_json::to_value(&request) {
                        logger.log_error(
                            "/v1/messages",
                            "anthropic",
                            &error.to_string(),
                            "serialization_error",
                            Some(&request_json),
                            Some(request_id.clone()),
                            None,
                        );
                    }
                }
                return Err(error);
            }
        };

        // Parse OpenAI response to IR
        let ir_response = match backend_converter.parse_response(&openai_response_bytes).await {
            Ok(ir) => ir,
            Err(e) => {
                if let Some(logger) = &state.logger {
                    if let Ok(request_json) = serde_json::to_value(&request) {
                        logger.log_error(
                            "/v1/messages",
                            "anthropic",
                            &e.to_string(),
                            error_type_from_error(&e),
                            Some(&request_json),
                            Some(request_id.clone()),
                            None,
                        );
                    }
                }
                return Err(e);
            }
        };

        // Format IR to Anthropic response
        let anthropic_response_bytes = match frontend_converter.format_response(&ir_response).await {
            Ok(bytes) => bytes,
            Err(e) => {
                if let Some(logger) = &state.logger {
                    if let Ok(request_json) = serde_json::to_value(&request) {
                        logger.log_error(
                            "/v1/messages",
                            "anthropic",
                            &e.to_string(),
                            error_type_from_error(&e),
                            Some(&request_json),
                            Some(request_id.clone()),
                            None,
                        );
                    }
                }
                return Err(e);
            }
        };

        // Deserialize Anthropic response
        let anthropic_response: louter::models::anthropic::MessagesResponse = match serde_json::from_slice(&anthropic_response_bytes) {
            Ok(resp) => resp,
            Err(e) => {
                let error = ProxyError::SerializationError(e);
                if let Some(logger) = &state.logger {
                    if let Ok(request_json) = serde_json::to_value(&request) {
                        logger.log_error(
                            "/v1/messages",
                            "anthropic",
                            &error.to_string(),
                            "serialization_error",
                            Some(&request_json),
                            Some(request_id.clone()),
                            None,
                        );
                    }
                }
                return Err(error);
            }
        };

        // Calculate metrics
        let e2e_latency_ms = start_time.elapsed().as_secs_f64() * 1000.0;
        let output_tokens = openai_response.usage.completion_tokens;
        let input_tokens = Some(openai_response.usage.prompt_tokens);
        let ttft_ms = Some(e2e_latency_ms);

        // Log client response with metrics
        if let Some(logger) = &state.logger {
            if let Ok(response_json) = serde_json::to_value(&anthropic_response) {
                logger.log_client_response_with_metrics(
                    "/v1/messages",
                    "anthropic",
                    &response_json,
                    ttft_ms,
                    Some(e2e_latency_ms),
                    Some(output_tokens),
                    input_tokens,
                );
            }
        }

        // Print metrics if verbose
        if state.verbose {
            info!("Metrics: e2e={:.2}ms, output_tokens={}, input_tokens={:?}",
                e2e_latency_ms, output_tokens, input_tokens);
        }

        // Record performance metrics to Prometheus
        let ttft_seconds = ttft_ms.map(|ms| ms / 1000.0);
        let tps_value = Some(output_tokens as f64 / (e2e_latency_ms / 1000.0));
        let itl_seconds = Some(if output_tokens > 1 {
            e2e_latency_ms / (output_tokens as f64 - 1.0) / 1000.0
        } else {
            0.0
        });
        metrics::record_performance_metrics("/v1/messages", "anthropic", ttft_seconds, tps_value, itl_seconds);

        Ok(Json(anthropic_response).into_response())
    }
}

// List models for Anthropic API (GET /v1/models)
async fn list_anthropic_models(State(state): State<AppState>) -> impl IntoResponse {
    use serde_json::json;

    // Collect all Claude models from backend configurations
    let mut models = Vec::new();

    for (backend_name, backend_config) in state.config.backends.iter() {
        // Only include models that start with "claude-" from model_mapping keys
        for model_name in backend_config.model_mapping.keys() {
            if model_name.starts_with("claude-") {
                models.push(json!({
                    "type": "model",
                    "id": model_name,
                    "display_name": model_name,
                    "created_at": "2024-01-01T00:00:00Z",
                }));
            }
        }
    }

    // If no Claude models found, return default set
    if models.is_empty() {
        models = vec![
            json!({
                "type": "model",
                "id": "claude-3-5-sonnet-20241022",
                "display_name": "Claude 3.5 Sonnet",
                "created_at": "2024-10-22T00:00:00Z",
            }),
            json!({
                "type": "model",
                "id": "claude-3-sonnet-20240229",
                "display_name": "Claude 3 Sonnet",
                "created_at": "2024-02-29T00:00:00Z",
            }),
            json!({
                "type": "model",
                "id": "claude-3-opus-20240229",
                "display_name": "Claude 3 Opus",
                "created_at": "2024-02-29T00:00:00Z",
            }),
            json!({
                "type": "model",
                "id": "claude-3-haiku-20240307",
                "display_name": "Claude 3 Haiku",
                "created_at": "2024-03-07T00:00:00Z",
            }),
        ];
    }

    Json(json!({
        "data": models,
        "has_more": false,
        "first_id": models.first().and_then(|m| m.get("id")).map(|id| id.as_str()).flatten(),
        "last_id": models.last().and_then(|m| m.get("id")).map(|id| id.as_str()).flatten(),
    }))
}

// Catch-all handler to log unmatched requests
async fn catch_all_handler(
    State(state): State<AppState>,
    method: axum::http::Method,
    uri: axum::http::Uri,
    headers: HeaderMap,
    body: String,
) -> Result<Response, ProxyError> {
    let request_id = uuid::Uuid::new_v4().to_string();

    warn!("⚠ Unmatched request: {} {} (request_id: {})", method, uri, request_id);
    warn!("Headers: {:?}", headers.iter().map(|(k, v)| (k.as_str(), v.to_str().unwrap_or("<binary>"))).collect::<Vec<_>>());
    warn!("Body: {}", if body.is_empty() { "<empty>" } else { &body[..body.len().min(200)] });

    // Log to file with full details
    if let Some(logger) = &state.logger {
        let mut headers_map = std::collections::HashMap::new();
        for (key, value) in headers.iter() {
            if let Ok(value_str) = value.to_str() {
                headers_map.insert(key.to_string(), value_str.to_string());
            }
        }

        let body_json = if body.is_empty() {
            serde_json::json!({})
        } else {
            serde_json::from_str(&body).unwrap_or(serde_json::json!({"raw": body}))
        };

        logger.log_error(
            uri.path(),
            "unknown",
            &format!("404 Not Found: {} {}", method, uri.path()),
            "not_found",
            Some(&body_json),
            Some(request_id),
            Some(404),
        );
    }

    Err(ProxyError::InvalidRequest(format!(
        "Endpoint not found: {} {}. Available endpoints: /v1/messages, /v1/chat/completions, /health, /metrics, /ui",
        method, uri.path()
    )))
}

// UI handler wrappers to convert AppState to UiState
async fn ui_handler_wrapper() -> impl IntoResponse {
    ui::ui_handler().await
}

async fn api_logs_wrapper(
    State(state): State<AppState>,
    query: Query<ui::LogQuery>,
) -> impl IntoResponse {
    let ui_state = ui::UiState {
        log_file: state.log_file.clone(),
        metrics_handle: state.metrics_handle.clone(),
    };
    ui::api_logs_handler(State(ui_state), query).await
}

async fn api_metrics_wrapper(
    State(state): State<AppState>,
) -> impl IntoResponse {
    let ui_state = ui::UiState {
        log_file: state.log_file.clone(),
        metrics_handle: state.metrics_handle.clone(),
    };
    ui::api_metrics_handler(State(ui_state)).await
}
