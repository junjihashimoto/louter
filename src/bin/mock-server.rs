use axum::{
    extract::Path,
    http::StatusCode,
    response::{IntoResponse, Response, Sse},
    routing::{get, post},
    Json, Router,
};
use clap::Parser;
use flate2::read::GzDecoder;
use serde_json::Value;
use std::convert::Infallible;
use std::fs;
use std::io::Read;
use std::net::SocketAddr;
use std::time::Duration;
use tokio::time::sleep;

#[derive(Parser)]
#[command(name = "mock-server")]
#[command(about = "Mock API server using test-data responses")]
struct Cli {
    /// Port to run the mock server on
    #[arg(long, default_value = "8888")]
    port: u16,

    /// Test data directory
    #[arg(long, default_value = "test-data")]
    test_data_dir: String,
}

#[tokio::main]
async fn main() {
    let cli = Cli::parse();

    tracing_subscriber::fmt::init();

    let app = Router::new()
        .route("/health", get(health_handler))
        .route("/v1/chat/completions", post(openai_handler))
        .route("/v1beta/models/:action", post(gemini_handler));

    let addr = SocketAddr::from(([127, 0, 0, 1], cli.port));
    println!("🚀 Mock server listening on http://{}", addr);
    println!("📁 Using test data from: {}", cli.test_data_dir);
    println!();
    println!("Endpoints:");
    println!("  GET  /health");
    println!("  POST /v1/chat/completions (OpenAI API)");
    println!("  POST /v1beta/models/:model (Gemini API)");
    println!();

    let listener = tokio::net::TcpListener::bind(addr)
        .await
        .expect("Failed to bind");

    axum::serve(listener, app)
        .await
        .expect("Server error");
}

async fn health_handler() -> &'static str {
    "OK"
}

async fn openai_handler(Json(payload): Json<Value>) -> Response {
    println!("📥 OpenAI request: {}", serde_json::to_string_pretty(&payload).unwrap_or_default());

    // Check if streaming is requested
    let is_streaming = payload.get("stream").and_then(|v| v.as_bool()).unwrap_or(false);

    // Determine test type based on request content
    let test_type = if has_tools(&payload) {
        "function_calling"
    } else if has_audio(&payload) {
        "audio"
    } else if has_vision(&payload) {
        "vision"
    } else if is_streaming {
        "streaming"
    } else {
        "text"
    };

    if is_streaming {
        let response_path = format!("test-data/openai/{}/response.txt", test_type);
        match load_streaming_response(&response_path) {
            Ok(lines) => {
                println!("📤 OpenAI streaming response ({}):", test_type);
                create_sse_response(lines)
            }
            Err(e) => {
                eprintln!("❌ Error loading streaming response: {}", e);
                (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    format!("Failed to load streaming response: {}", e),
                )
                    .into_response()
            }
        }
    } else {
        let response_path = format!("test-data/openai/{}/response.json", test_type);
        match load_response(&response_path) {
            Ok(response) => {
                println!("📤 OpenAI response ({}):", test_type);
                println!("{}", serde_json::to_string_pretty(&response).unwrap_or_default());
                Json(response).into_response()
            }
            Err(e) => {
                eprintln!("❌ Error loading response: {}", e);
                (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    format!("Failed to load response: {}", e),
                )
                    .into_response()
            }
        }
    }
}

async fn gemini_handler(Path(action): Path<String>, Json(payload): Json<Value>) -> Response {
    println!("📥 Gemini request ({}): {}", action, serde_json::to_string_pretty(&payload).unwrap_or_default());

    // Check if streaming is requested (URL ends with :streamGenerateContent)
    let is_streaming = action.contains(":streamGenerateContent");

    // Determine test type based on request content
    let test_type = if has_gemini_tools(&payload) {
        "function_calling"
    } else if has_gemini_audio(&payload) {
        "audio"
    } else if has_gemini_vision(&payload) {
        "vision"
    } else if is_streaming {
        "streaming"
    } else {
        "text"
    };

    if is_streaming {
        let response_path = format!("test-data/gemini/{}/response.txt", test_type);
        match load_streaming_response(&response_path) {
            Ok(lines) => {
                println!("📤 Gemini streaming response ({}):", test_type);
                create_sse_response(lines)
            }
            Err(e) => {
                eprintln!("❌ Error loading streaming response: {}", e);
                (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    format!("Failed to load streaming response: {}", e),
                )
                    .into_response()
            }
        }
    } else {
        let response_path = format!("test-data/gemini/{}/response.json", test_type);
        match load_response(&response_path) {
            Ok(response) => {
                println!("📤 Gemini response ({}):", test_type);
                println!("{}", serde_json::to_string_pretty(&response).unwrap_or_default());
                Json(response).into_response()
            }
            Err(e) => {
                eprintln!("❌ Error loading response: {}", e);
                (
                    StatusCode::INTERNAL_SERVER_ERROR,
                    format!("Failed to load response: {}", e),
                )
                    .into_response()
            }
        }
    }
}

fn load_response(path: &str) -> Result<Value, String> {
    // Try loading gzipped version first
    let gz_path = format!("{}.gz", path);
    let content = if std::path::Path::new(&gz_path).exists() {
        // Read and decompress gzipped file
        let file = fs::File::open(&gz_path)
            .map_err(|e| format!("Failed to open {}: {}", gz_path, e))?;
        let mut decoder = GzDecoder::new(file);
        let mut decompressed = String::new();
        decoder.read_to_string(&mut decompressed)
            .map_err(|e| format!("Failed to decompress {}: {}", gz_path, e))?;
        decompressed
    } else {
        // Fall back to uncompressed file
        fs::read_to_string(path)
            .map_err(|e| format!("Failed to read {}: {}", path, e))?
    };

    serde_json::from_str(&content)
        .map_err(|e| format!("Failed to parse JSON from {}: {}", path, e))
}

fn load_streaming_response(path: &str) -> Result<Vec<String>, String> {
    // Try loading gzipped version first
    let gz_path = format!("{}.gz", path);
    let content = if std::path::Path::new(&gz_path).exists() {
        // Read and decompress gzipped file
        let file = fs::File::open(&gz_path)
            .map_err(|e| format!("Failed to open {}: {}", gz_path, e))?;
        let mut decoder = GzDecoder::new(file);
        let mut decompressed = String::new();
        decoder.read_to_string(&mut decompressed)
            .map_err(|e| format!("Failed to decompress {}: {}", gz_path, e))?;
        decompressed
    } else {
        // Fall back to uncompressed file
        fs::read_to_string(path)
            .map_err(|e| format!("Failed to read {}: {}", path, e))?
    };

    // Split by lines and filter out empty lines and curl progress output
    // Also strip "data: " prefix since Axum SSE will add it automatically
    Ok(content
        .lines()
        .filter(|line| !line.trim().is_empty() && !line.contains("Total") && !line.contains("%"))
        .map(|line| {
            if line.starts_with("data: ") {
                line["data: ".len()..].to_string()
            } else {
                line.to_string()
            }
        })
        .collect())
}

fn create_sse_response(lines: Vec<String>) -> Response {
    let stream = async_stream::stream! {
        for line in lines {
            // Add a small delay to simulate streaming
            sleep(Duration::from_millis(50)).await;
            yield Ok::<_, Infallible>(axum::response::sse::Event::default().data(line));
        }
    };

    Sse::new(stream).into_response()
}

fn has_tools(payload: &Value) -> bool {
    payload.get("tools").is_some()
}

fn has_audio(payload: &Value) -> bool {
    // Check for audio modality
    if let Some(modalities) = payload.get("modalities").and_then(|m| m.as_array()) {
        if modalities.iter().any(|m| m.as_str() == Some("audio")) {
            return true;
        }
    }

    // Check for audio output settings
    if payload.get("audio").is_some() {
        return true;
    }

    // Check for input_audio in messages
    if let Some(messages) = payload.get("messages").and_then(|m| m.as_array()) {
        for msg in messages {
            if let Some(content) = msg.get("content") {
                if content.is_array() {
                    if let Some(parts) = content.as_array() {
                        for part in parts {
                            if part.get("type").and_then(|t| t.as_str()) == Some("input_audio") {
                                return true;
                            }
                        }
                    }
                }
            }
        }
    }
    false
}

fn has_vision(payload: &Value) -> bool {
    if let Some(messages) = payload.get("messages").and_then(|m| m.as_array()) {
        for msg in messages {
            if let Some(content) = msg.get("content") {
                if content.is_array() {
                    // Check for image_url in content parts
                    if let Some(parts) = content.as_array() {
                        for part in parts {
                            if part.get("type").and_then(|t| t.as_str()) == Some("image_url") {
                                return true;
                            }
                        }
                    }
                }
            }
        }
    }
    false
}

fn has_gemini_tools(payload: &Value) -> bool {
    payload.get("tools").is_some()
}

fn has_gemini_vision(payload: &Value) -> bool {
    if let Some(contents) = payload.get("contents").and_then(|c| c.as_array()) {
        for content in contents {
            if let Some(parts) = content.get("parts").and_then(|p| p.as_array()) {
                for part in parts {
                    if let Some(inline_data) = part.get("inlineData") {
                        if let Some(mime_type) = inline_data.get("mimeType").and_then(|m| m.as_str()) {
                            if mime_type.starts_with("image/") {
                                return true;
                            }
                        }
                    }
                }
            }
        }
    }
    false
}

fn has_gemini_audio(payload: &Value) -> bool {
    if let Some(contents) = payload.get("contents").and_then(|c| c.as_array()) {
        for content in contents {
            if let Some(parts) = content.get("parts").and_then(|p| p.as_array()) {
                for part in parts {
                    if let Some(inline_data) = part.get("inlineData") {
                        if let Some(mime_type) = inline_data.get("mimeType").and_then(|m| m.as_str()) {
                            if mime_type.starts_with("audio/") {
                                return true;
                            }
                        }
                    }
                }
            }
        }
    }
    false
}
