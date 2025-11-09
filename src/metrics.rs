use metrics::{counter, histogram, describe_counter, describe_histogram};
use metrics_exporter_prometheus::{Matcher, PrometheusBuilder, PrometheusHandle};
use std::time::Duration;

/// Initialize the Prometheus metrics exporter and register metrics
pub fn init() -> Result<PrometheusHandle, Box<dyn std::error::Error>> {
    let builder = PrometheusBuilder::new();

    // Configure histogram buckets for request duration (in seconds)
    // Buckets: 10ms, 50ms, 100ms, 250ms, 500ms, 1s, 2.5s, 5s, 10s, 30s
    let builder = builder.set_buckets_for_metric(
        Matcher::Full("http_request_duration_seconds".to_string()),
        &[0.01, 0.05, 0.1, 0.25, 0.5, 1.0, 2.5, 5.0, 10.0, 30.0],
    )?;

    let builder = builder.set_buckets_for_metric(
        Matcher::Full("backend_request_duration_seconds".to_string()),
        &[0.01, 0.05, 0.1, 0.25, 0.5, 1.0, 2.5, 5.0, 10.0, 30.0],
    )?;

    // TTFT (Time to First Token) buckets in seconds
    let builder = builder.set_buckets_for_metric(
        Matcher::Full("ttft_seconds".to_string()),
        &[0.01, 0.05, 0.1, 0.25, 0.5, 1.0, 2.5, 5.0, 10.0, 30.0],
    )?;

    // TPS (Tokens Per Second) buckets
    let builder = builder.set_buckets_for_metric(
        Matcher::Full("tokens_per_second".to_string()),
        &[1.0, 5.0, 10.0, 25.0, 50.0, 100.0, 200.0, 500.0, 1000.0],
    )?;

    // ITL (Inter-token Latency) buckets in seconds
    let builder = builder.set_buckets_for_metric(
        Matcher::Full("inter_token_latency_seconds".to_string()),
        &[0.001, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1.0],
    )?;

    let handle = builder.install_recorder()?;

    // Describe all metrics
    describe_counter!(
        "http_requests_total",
        "Total number of HTTP requests received"
    );
    describe_histogram!(
        "http_request_duration_seconds",
        "HTTP request duration in seconds"
    );
    describe_counter!(
        "backend_requests_total",
        "Total number of backend requests made"
    );
    describe_histogram!(
        "backend_request_duration_seconds",
        "Backend request duration in seconds"
    );
    describe_counter!(
        "tokens_total",
        "Total number of tokens processed"
    );
    describe_counter!(
        "errors_total",
        "Total number of errors encountered"
    );
    describe_counter!(
        "function_calls_total",
        "Total number of function calls processed"
    );
    describe_histogram!(
        "ttft_seconds",
        "Time to first token in seconds"
    );
    describe_histogram!(
        "tokens_per_second",
        "Tokens generated per second (throughput)"
    );
    describe_histogram!(
        "inter_token_latency_seconds",
        "Inter-token latency in seconds"
    );
    describe_counter!(
        "routing_decisions_total",
        "Total number of routing decisions made (backend selections)"
    );

    Ok(handle)
}

/// Record an HTTP request
pub fn record_request(endpoint: &str, method: &str, status: u16, duration: Duration) {
    let labels = [
        ("endpoint", endpoint.to_string()),
        ("method", method.to_string()),
        ("status", status.to_string()),
    ];

    counter!("http_requests_total", &labels).increment(1);
    histogram!("http_request_duration_seconds", &labels[..2]).record(duration.as_secs_f64());
}

/// Record a backend request
pub fn record_backend_request(backend: &str, endpoint: &str, status: u16, duration: Duration) {
    let labels = [
        ("backend", backend.to_string()),
        ("endpoint", endpoint.to_string()),
        ("status", status.to_string()),
    ];

    counter!("backend_requests_total", &labels).increment(1);
    histogram!("backend_request_duration_seconds", &labels[..2]).record(duration.as_secs_f64());
}

/// Record token usage
pub fn record_tokens(token_type: &str, backend: &str, count: u32) {
    let labels = [
        ("type", token_type.to_string()),
        ("backend", backend.to_string()),
    ];

    counter!("tokens_total", &labels).increment(count as u64);
}

/// Record an error
pub fn record_error(error_type: &str, endpoint: &str) {
    let labels = [
        ("type", error_type.to_string()),
        ("endpoint", endpoint.to_string()),
    ];

    counter!("errors_total", &labels).increment(1);
}

/// Record a function call
pub fn record_function_call(backend: &str, function_name: &str) {
    let labels = [
        ("backend", backend.to_string()),
        ("function", function_name.to_string()),
    ];

    counter!("function_calls_total", &labels).increment(1);
}

/// Record performance metrics (TTFT, TPS, ITL)
pub fn record_performance_metrics(
    endpoint: &str,
    backend: &str,
    ttft_seconds: Option<f64>,
    tps: Option<f64>,
    itl_seconds: Option<f64>,
) {
    let labels = [
        ("endpoint", endpoint.to_string()),
        ("backend", backend.to_string()),
    ];

    if let Some(ttft) = ttft_seconds {
        histogram!("ttft_seconds", &labels).record(ttft);
    }

    if let Some(tokens_per_sec) = tps {
        histogram!("tokens_per_second", &labels).record(tokens_per_sec);
    }

    if let Some(itl) = itl_seconds {
        histogram!("inter_token_latency_seconds", &labels).record(itl);
    }
}

/// Record routing decision metrics
pub fn record_routing_decision(
    backend_name: &str,
    required_capabilities: &[&str],
    backend_mode: &str,  // "auto" or "explicit"
    success: bool,
) {
    let caps_str = required_capabilities.join(",");

    let labels = [
        ("backend", backend_name.to_string()),
        ("mode", backend_mode.to_string()),
        ("capabilities", caps_str),
        ("success", success.to_string()),
    ];

    counter!("routing_decisions_total", &labels).increment(1);
}

#[cfg(test)]
mod tests {
    use super::*;

    // Note: test_metrics_init removed due to global state issues with Prometheus
    // Metrics initialization is tested indirectly through integration tests

    #[test]
    fn test_record_request() {
        let _ = init();
        record_request("/v1/chat/completions", "POST", 200, Duration::from_millis(500));
        // No assertion - just ensure it doesn't panic
    }

    #[test]
    fn test_record_backend_request() {
        let _ = init();
        record_backend_request("openai", "/v1/chat/completions", 200, Duration::from_secs(1));
    }

    #[test]
    fn test_record_tokens() {
        let _ = init();
        record_tokens("input", "openai", 100);
        record_tokens("output", "openai", 50);
    }

    #[test]
    fn test_record_error() {
        let _ = init();
        record_error("conversion_error", "/v1/chat/completions");
    }

    #[test]
    fn test_record_function_call() {
        let _ = init();
        record_function_call("openai", "getWeather");
    }
}
