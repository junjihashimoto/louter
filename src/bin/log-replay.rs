use clap::Parser;
use serde_json::Value;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::collections::HashMap;

#[derive(Parser, Debug)]
#[command(name = "log-replay")]
#[command(about = "Replay requests from JSON Lines log file for debugging")]
struct Args {
    /// JSON Lines log file to replay
    #[arg(short, long)]
    file: String,

    /// Filter by error type (optional)
    #[arg(short = 'e', long)]
    error_type: Option<String>,

    /// Filter by endpoint (optional)
    #[arg(short = 'p', long)]
    endpoint: Option<String>,

    /// Show only errors
    #[arg(short = 'E', long)]
    errors_only: bool,

    /// Request ID to replay (optional)
    #[arg(short = 'r', long)]
    request_id: Option<String>,

    /// Proxy URL to replay requests to
    #[arg(short = 'u', long, default_value = "http://localhost:9000")]
    proxy_url: String,

    /// Dry run - only show requests, don't execute
    #[arg(short = 'd', long)]
    dry_run: bool,

    /// Output format: json, curl, summary
    #[arg(short = 'o', long, default_value = "summary")]
    output: String,
}

#[derive(Debug, serde::Deserialize, serde::Serialize)]
struct LogEntry {
    timestamp: String,
    direction: String,
    format: String,
    endpoint: String,
    body: Value,
    method: Option<String>,
    status_code: Option<u16>,
    headers: Option<HashMap<String, String>>,
    error: Option<String>,
    error_type: Option<String>,
    stack_trace: Option<String>,
    is_streaming: Option<bool>,
    stream_events: Option<Vec<String>>,
    request_id: Option<String>,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    let file = File::open(&args.file)?;
    let reader = BufReader::new(file);

    let mut matching_entries = Vec::new();
    let mut error_count = 0;
    let mut request_count = 0;

    // Parse and filter log entries
    for line in reader.lines() {
        let line = line?;
        if line.trim().is_empty() {
            continue;
        }

        match serde_json::from_str::<LogEntry>(&line) {
            Ok(entry) => {
                // Count requests and errors
                if entry.direction == "client_request" {
                    request_count += 1;
                }
                if entry.direction == "error" {
                    error_count += 1;
                }

                // Apply filters
                if args.errors_only && entry.direction != "error" {
                    continue;
                }

                if let Some(ref error_type_filter) = args.error_type {
                    if entry.error_type.as_ref() != Some(error_type_filter) {
                        continue;
                    }
                }

                if let Some(ref endpoint_filter) = args.endpoint {
                    if !entry.endpoint.contains(endpoint_filter) {
                        continue;
                    }
                }

                if let Some(ref request_id_filter) = args.request_id {
                    if entry.request_id.as_ref() != Some(request_id_filter) {
                        continue;
                    }
                }

                matching_entries.push(entry);
            }
            Err(e) => {
                eprintln!("Warning: Failed to parse line: {} - {}", e, &line[..line.len().min(100)]);
            }
        }
    }

    // Output based on format
    match args.output.as_str() {
        "json" => {
            println!("{}", serde_json::to_string_pretty(&matching_entries)?);
        }
        "curl" => {
            for entry in &matching_entries {
                if entry.direction == "client_request" {
                    print_curl_command(&entry, &args.proxy_url);
                }
            }
        }
        "summary" | _ => {
            print_summary(&matching_entries, request_count, error_count);
        }
    }

    // Replay if not dry run
    if !args.dry_run && args.output != "summary" {
        println!("\n--- Replaying requests to {} ---", args.proxy_url);
        replay_requests(&matching_entries, &args.proxy_url)?;
    }

    Ok(())
}

fn print_summary(entries: &[LogEntry], total_requests: usize, total_errors: usize) {
    println!("=== Log Summary ===");
    println!("Total requests: {}", total_requests);
    println!("Total errors: {}", total_errors);
    println!("Matching entries: {}", entries.len());
    println!();

    // Group by direction
    let mut by_direction: HashMap<String, usize> = HashMap::new();
    for entry in entries {
        *by_direction.entry(entry.direction.clone()).or_insert(0) += 1;
    }

    println!("By direction:");
    for (direction, count) in &by_direction {
        println!("  {}: {}", direction, count);
    }
    println!();

    // Show errors
    let errors: Vec<_> = entries.iter().filter(|e| e.error.is_some()).collect();
    if !errors.is_empty() {
        println!("=== Errors ({}) ===", errors.len());
        for (i, error) in errors.iter().enumerate() {
            println!("\nError #{}", i + 1);
            println!("  Timestamp: {}", error.timestamp);
            println!("  Endpoint: {}", error.endpoint);
            if let Some(ref error_type) = error.error_type {
                println!("  Type: {}", error_type);
            }
            if let Some(ref error_msg) = error.error {
                println!("  Message: {}", error_msg);
            }
            if let Some(ref req_id) = error.request_id {
                println!("  Request ID: {}", req_id);
            }
            if let Some(status) = error.status_code {
                println!("  Status Code: {}", status);
            }
        }
        println!();
    }

    // Show streaming issues
    let stream_errors: Vec<_> = entries.iter()
        .filter(|e| e.is_streaming == Some(true) && e.error.is_some())
        .collect();
    if !stream_errors.is_empty() {
        println!("=== Streaming Errors ({}) ===", stream_errors.len());
        for error in stream_errors {
            println!("  {} - {}", error.timestamp, error.error.as_ref().unwrap());
            if let Some(ref events) = error.stream_events {
                println!("    Events received: {}", events.join(", "));
            }
        }
        println!();
    }

    // Show request/response pairs
    let mut pairs: HashMap<String, Vec<&LogEntry>> = HashMap::new();
    for entry in entries {
        if let Some(ref req_id) = entry.request_id {
            pairs.entry(req_id.clone()).or_insert_with(Vec::new).push(entry);
        }
    }

    if !pairs.is_empty() {
        println!("=== Request/Response Pairs ({}) ===", pairs.len());
        for (req_id, pair_entries) in pairs.iter().take(5) {
            println!("\nRequest ID: {}", req_id);
            for entry in pair_entries {
                println!("  {} - {} {}", entry.timestamp, entry.direction, entry.endpoint);
                if let Some(ref error) = entry.error {
                    println!("    ERROR: {}", error);
                }
            }
        }
        if pairs.len() > 5 {
            println!("\n  ... and {} more pairs", pairs.len() - 5);
        }
    }
}

fn print_curl_command(entry: &LogEntry, proxy_url: &str) {
    let url = format!("{}{}", proxy_url, entry.endpoint);
    let method = entry.method.as_deref().unwrap_or("POST");

    println!("# {} - {}", entry.timestamp, entry.endpoint);
    print!("curl -X {} '{}'", method, url);

    // Add headers
    print!(" -H 'Content-Type: application/json'");
    if let Some(ref headers) = entry.headers {
        for (key, value) in headers {
            print!(" -H '{}: {}'", key, value);
        }
    }

    // Add body
    if method == "POST" {
        let body_str = serde_json::to_string(&entry.body).unwrap_or_default();
        print!(" -d '{}'", body_str);
    }

    // Add streaming flag
    if entry.is_streaming == Some(true) {
        print!(" -N");
    }

    println!();
    println!();
}

fn replay_requests(entries: &[LogEntry], proxy_url: &str) -> Result<(), Box<dyn std::error::Error>> {
    let client = reqwest::blocking::Client::new();

    for entry in entries {
        if entry.direction != "client_request" {
            continue;
        }

        println!("Replaying: {} {}", entry.method.as_deref().unwrap_or("POST"), entry.endpoint);

        let url = format!("{}{}", proxy_url, entry.endpoint);
        let method = entry.method.as_deref().unwrap_or("POST");

        let mut request = match method {
            "GET" => client.get(&url),
            "POST" => client.post(&url).json(&entry.body),
            _ => {
                eprintln!("Unsupported method: {}", method);
                continue;
            }
        };

        // Add headers
        if let Some(ref headers) = entry.headers {
            for (key, value) in headers {
                request = request.header(key, value);
            }
        }

        // Execute request
        match request.send() {
            Ok(response) => {
                println!("  Status: {}", response.status());
                if !response.status().is_success() {
                    if let Ok(body) = response.text() {
                        println!("  Error body: {}", &body[..body.len().min(200)]);
                    }
                }
            }
            Err(e) => {
                eprintln!("  Failed to replay: {}", e);
            }
        }
        println!();
    }

    Ok(())
}
