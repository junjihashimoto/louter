use axum::{
    extract::{Query, State},
    response::{Html, IntoResponse},
    Json,
};
use serde::{Deserialize, Serialize};
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::sync::Arc;

#[derive(Clone)]
pub struct UiState {
    pub log_file: Option<String>,
    pub metrics_handle: Option<Arc<metrics_exporter_prometheus::PrometheusHandle>>,
}

#[derive(Deserialize)]
pub struct LogQuery {
    #[serde(default)]
    limit: Option<usize>,
    #[serde(default)]
    offset: Option<usize>,
    #[serde(default)]
    direction: Option<String>,
    #[serde(default)]
    search: Option<String>,
}

#[derive(Serialize, Clone)]
pub struct LogEntry {
    pub timestamp: String,
    pub direction: String,
    pub format: String,
    pub endpoint: String,
    pub payload: serde_json::Value,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ttft_ms: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tps: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub itl_ms: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub e2e_latency_ms: Option<f64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub output_tokens: Option<i32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub input_tokens: Option<i32>,
}

pub async fn ui_handler() -> Html<&'static str> {
    Html(include_str!("../static/ui.html"))
}

pub async fn api_logs_handler(
    State(state): State<UiState>,
    Query(query): Query<LogQuery>,
) -> impl IntoResponse {
    if let Some(ref log_file) = state.log_file {
        match read_logs(log_file, &query) {
            Ok(logs) => Json(serde_json::json!({
                "success": true,
                "logs": logs,
                "count": logs.len()
            })),
            Err(e) => Json(serde_json::json!({
                "success": false,
                "error": format!("Failed to read logs: {}", e)
            })),
        }
    } else {
        Json(serde_json::json!({
            "success": false,
            "error": "Logging not enabled. Start proxy with --log-file flag."
        }))
    }
}

pub async fn api_metrics_handler(State(state): State<UiState>) -> impl IntoResponse {
    if let Some(ref handle) = state.metrics_handle {
        let metrics_text = handle.render();
        Json(serde_json::json!({
            "success": true,
            "metrics": parse_prometheus_metrics(&metrics_text),
            "raw": metrics_text
        }))
    } else {
        Json(serde_json::json!({
            "success": false,
            "error": "Metrics not enabled"
        }))
    }
}

fn read_logs(log_file: &str, query: &LogQuery) -> Result<Vec<LogEntry>, std::io::Error> {
    let file = File::open(log_file)?;
    let reader = BufReader::new(file);

    let mut logs: Vec<LogEntry> = Vec::new();

    for line in reader.lines() {
        let line = line?;
        if line.trim().is_empty() {
            continue;
        }

        if let Ok(value) = serde_json::from_str::<serde_json::Value>(&line) {
            // Filter by direction
            if let Some(ref dir) = query.direction {
                if let Some(log_dir) = value.get("direction").and_then(|v| v.as_str()) {
                    if log_dir != dir {
                        continue;
                    }
                }
            }

            // Filter by search term
            if let Some(ref search) = query.search {
                let line_lower = line.to_lowercase();
                if !line_lower.contains(&search.to_lowercase()) {
                    continue;
                }
            }

            logs.push(LogEntry {
                timestamp: value.get("timestamp")
                    .and_then(|v| v.as_str())
                    .unwrap_or("")
                    .to_string(),
                direction: value.get("direction")
                    .and_then(|v| v.as_str())
                    .unwrap_or("")
                    .to_string(),
                format: value.get("format")
                    .and_then(|v| v.as_str())
                    .unwrap_or("")
                    .to_string(),
                endpoint: value.get("endpoint")
                    .and_then(|v| v.as_str())
                    .unwrap_or("")
                    .to_string(),
                payload: value.get("body").cloned().unwrap_or(serde_json::Value::Null),
                ttft_ms: value.get("ttft_ms").and_then(|v| v.as_f64()),
                tps: value.get("tps").and_then(|v| v.as_f64()),
                itl_ms: value.get("itl_ms").and_then(|v| v.as_f64()),
                e2e_latency_ms: value.get("e2e_latency_ms").and_then(|v| v.as_f64()),
                output_tokens: value.get("output_tokens").and_then(|v| v.as_i64()).map(|v| v as i32),
                input_tokens: value.get("input_tokens").and_then(|v| v.as_i64()).map(|v| v as i32),
            });
        }
    }

    // Apply pagination
    let offset = query.offset.unwrap_or(0);
    let limit = query.limit.unwrap_or(100);

    // Reverse to show newest first
    logs.reverse();

    let end = std::cmp::min(offset + limit, logs.len());
    Ok(logs[offset..end].to_vec())
}

fn parse_prometheus_metrics(metrics_text: &str) -> Vec<serde_json::Value> {
    let mut metrics = Vec::new();

    for line in metrics_text.lines() {
        if line.starts_with('#') || line.trim().is_empty() {
            continue;
        }

        if let Some((name, rest)) = line.split_once('{') {
            if let Some((labels, value)) = rest.split_once('}') {
                let value_str = value.trim();
                metrics.push(serde_json::json!({
                    "name": name,
                    "labels": labels,
                    "value": value_str.parse::<f64>().unwrap_or(0.0)
                }));
            }
        } else if let Some((name, value)) = line.split_once(' ') {
            metrics.push(serde_json::json!({
                "name": name,
                "labels": "",
                "value": value.trim().parse::<f64>().unwrap_or(0.0)
            }));
        }
    }

    metrics
}
