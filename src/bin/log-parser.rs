use clap::{Parser, Subcommand};
use serde_json::Value;
use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(Parser)]
#[command(name = "log-parser")]
#[command(about = "Parse and filter JSON Lines log files for debugging", long_about = None)]
struct Cli {
    /// Path to JSON Lines log file
    #[arg(short, long)]
    file: String,

    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// Show all entries
    All {
        /// Pretty print JSON
        #[arg(short, long)]
        pretty: bool,
    },

    /// Filter by direction
    Filter {
        /// Direction: client_request, backend_request, backend_response, client_response
        #[arg(short, long)]
        direction: Option<String>,

        /// Format: gemini or openai
        #[arg(short = 'f', long)]
        format: Option<String>,

        /// Pretty print JSON
        #[arg(short, long)]
        pretty: bool,
    },

    /// Show only function calling related entries
    Functions {
        /// Pretty print JSON
        #[arg(short, long)]
        pretty: bool,
    },

    /// Show summary statistics
    Stats,

    /// Show request/response pairs
    Pairs {
        /// Show only pairs with function calls
        #[arg(short, long)]
        functions_only: bool,
    },
}

#[derive(serde::Deserialize)]
struct LogEntry {
    timestamp: String,
    direction: String,
    format: String,
    endpoint: String,
    body: Value,
}

fn main() {
    let cli = Cli::parse();

    let command = cli.command.unwrap_or(Commands::All { pretty: false });

    match command {
        Commands::All { pretty } => show_all(&cli.file, pretty),
        Commands::Filter { direction, format, pretty } => {
            filter_entries(&cli.file, direction, format, pretty)
        }
        Commands::Functions { pretty } => show_functions(&cli.file, pretty),
        Commands::Stats => show_stats(&cli.file),
        Commands::Pairs { functions_only } => show_pairs(&cli.file, functions_only),
    }
}

fn read_log_file(file_path: &str) -> Vec<LogEntry> {
    let file = File::open(file_path).expect("Failed to open log file");
    let reader = BufReader::new(file);
    let mut entries = Vec::new();

    for line in reader.lines() {
        if let Ok(line) = line {
            if let Ok(entry) = serde_json::from_str::<LogEntry>(&line) {
                entries.push(entry);
            }
        }
    }

    entries
}

fn show_all(file_path: &str, pretty: bool) {
    let entries = read_log_file(file_path);
    println!("📊 Total entries: {}\n", entries.len());

    for (i, entry) in entries.iter().enumerate() {
        println!("Entry #{}", i + 1);
        print_entry(entry, pretty);
        println!();
    }
}

fn filter_entries(file_path: &str, direction: Option<String>, format: Option<String>, pretty: bool) {
    let entries = read_log_file(file_path);
    let filtered: Vec<&LogEntry> = entries
        .iter()
        .filter(|e| {
            direction.as_ref().map_or(true, |d| &e.direction == d)
                && format.as_ref().map_or(true, |f| &e.format == f)
        })
        .collect();

    println!("📊 Filtered entries: {} / {}\n", filtered.len(), entries.len());

    for (i, entry) in filtered.iter().enumerate() {
        println!("Entry #{}", i + 1);
        print_entry(entry, pretty);
        println!();
    }
}

fn show_functions(file_path: &str, pretty: bool) {
    let entries = read_log_file(file_path);
    let mut count = 0;

    println!("🔧 Function calling related entries:\n");

    for (i, entry) in entries.iter().enumerate() {
        if has_function_call(&entry.body) || has_function_response(&entry.body) || has_tools(&entry.body) {
            count += 1;
            println!("Entry #{}", i + 1);
            print_entry(entry, pretty);

            // Highlight function calls
            if let Some(tools) = extract_tools(&entry.body) {
                println!("  🔧 Tools defined: {}", tools);
            }
            if let Some(call) = extract_function_call(&entry.body) {
                println!("  📞 Function call: {}", call);
            }
            if let Some(response) = extract_function_response(&entry.body) {
                println!("  📬 Function response: {}", response);
            }
            println!();
        }
    }

    println!("\n📊 Total function-related entries: {} / {}", count, entries.len());
}

fn show_stats(file_path: &str) {
    let entries = read_log_file(file_path);

    let mut client_requests = 0;
    let mut backend_requests = 0;
    let mut backend_responses = 0;
    let mut client_responses = 0;
    let mut gemini_format = 0;
    let mut openai_format = 0;
    let mut function_calls = 0;
    let mut function_responses = 0;
    let mut tools_defined = 0;

    for entry in &entries {
        match entry.direction.as_str() {
            "client_request" => client_requests += 1,
            "backend_request" => backend_requests += 1,
            "backend_response" => backend_responses += 1,
            "client_response" => client_responses += 1,
            _ => {}
        }

        match entry.format.as_str() {
            "gemini" => gemini_format += 1,
            "openai" => openai_format += 1,
            _ => {}
        }

        if has_function_call(&entry.body) {
            function_calls += 1;
        }
        if has_function_response(&entry.body) {
            function_responses += 1;
        }
        if has_tools(&entry.body) {
            tools_defined += 1;
        }
    }

    println!("📊 Log Statistics");
    println!("================");
    println!("Total entries:        {}", entries.len());
    println!();
    println!("By Direction:");
    println!("  Client Requests:    {}", client_requests);
    println!("  Backend Requests:   {}", backend_requests);
    println!("  Backend Responses:  {}", backend_responses);
    println!("  Client Responses:   {}", client_responses);
    println!();
    println!("By Format:");
    println!("  Gemini:             {}", gemini_format);
    println!("  OpenAI:             {}", openai_format);
    println!();
    println!("Function Calling:");
    println!("  Tools Defined:      {}", tools_defined);
    println!("  Function Calls:     {}", function_calls);
    println!("  Function Responses: {}", function_responses);
}

fn show_pairs(file_path: &str, functions_only: bool) {
    let entries = read_log_file(file_path);
    let mut i = 0;

    println!("🔗 Request/Response Pairs:\n");

    while i < entries.len() {
        if entries[i].direction == "client_request" {
            // Find matching client_response
            let mut response_idx = None;
            for j in (i + 1)..entries.len() {
                if entries[j].direction == "client_response" {
                    response_idx = Some(j);
                    break;
                }
            }

            if let Some(resp_idx) = response_idx {
                let has_functions = has_function_call(&entries[i].body)
                    || has_function_response(&entries[i].body)
                    || has_tools(&entries[i].body)
                    || has_function_call(&entries[resp_idx].body)
                    || has_function_response(&entries[resp_idx].body);

                if !functions_only || has_functions {
                    println!("──────────────────────────────────────");
                    println!("📥 Request:");
                    print_entry(&entries[i], false);

                    // Show backend request/response if exists
                    for j in (i + 1)..resp_idx {
                        if entries[j].direction == "backend_request" {
                            println!("\n  🔄 Backend Request ({}):", entries[j].format);
                            if let Some(tools) = extract_tools(&entries[j].body) {
                                println!("     Tools: {}", tools);
                            }
                        } else if entries[j].direction == "backend_response" {
                            println!("\n  🔄 Backend Response ({}):", entries[j].format);
                            if let Some(call) = extract_function_call(&entries[j].body) {
                                println!("     Function Call: {}", call);
                            }
                        }
                    }

                    println!("\n📤 Response:");
                    print_entry(&entries[resp_idx], false);
                    if let Some(call) = extract_function_call(&entries[resp_idx].body) {
                        println!("  Function Call: {}", call);
                    }
                    println!();
                }
                i = resp_idx + 1;
            } else {
                i += 1;
            }
        } else {
            i += 1;
        }
    }
}

fn print_entry(entry: &LogEntry, pretty: bool) {
    println!("  Timestamp: {}", entry.timestamp);
    println!("  Direction: {}", entry.direction);
    println!("  Format:    {}", entry.format);
    println!("  Endpoint:  {}", entry.endpoint);

    if pretty {
        println!("  Body:");
        println!("{}", serde_json::to_string_pretty(&entry.body).unwrap_or_else(|_| "{}".to_string()));
    } else {
        let body_str = serde_json::to_string(&entry.body).unwrap_or_else(|_| "{}".to_string());
        let truncated = if body_str.len() > 200 {
            format!("{}...", &body_str[..200])
        } else {
            body_str
        };
        println!("  Body: {}", truncated);
    }
}

fn has_function_call(body: &Value) -> bool {
    // Check for Gemini format functionCall
    if let Some(candidates) = body.get("candidates").and_then(|v| v.as_array()) {
        for candidate in candidates {
            if let Some(parts) = candidate
                .get("content")
                .and_then(|c| c.get("parts"))
                .and_then(|p| p.as_array())
            {
                for part in parts {
                    if part.get("functionCall").is_some() {
                        return true;
                    }
                }
            }
        }
    }

    // Check for OpenAI format tool_calls
    if let Some(choices) = body.get("choices").and_then(|v| v.as_array()) {
        for choice in choices {
            if choice.get("message").and_then(|m| m.get("tool_calls")).is_some() {
                return true;
            }
        }
    }

    false
}

fn has_function_response(body: &Value) -> bool {
    // Check for Gemini format functionResponse
    if let Some(contents) = body.get("contents").and_then(|v| v.as_array()) {
        for content in contents {
            if let Some(parts) = content.get("parts").and_then(|p| p.as_array()) {
                for part in parts {
                    if part.get("functionResponse").is_some() {
                        return true;
                    }
                }
            }
        }
    }

    // Check for OpenAI format tool messages
    if let Some(messages) = body.get("messages").and_then(|v| v.as_array()) {
        for message in messages {
            if message.get("role").and_then(|r| r.as_str()) == Some("tool") {
                return true;
            }
        }
    }

    false
}

fn has_tools(body: &Value) -> bool {
    body.get("tools").is_some()
}

fn extract_tools(body: &Value) -> Option<String> {
    body.get("tools").map(|tools| {
        if let Some(arr) = tools.as_array() {
            let names: Vec<String> = arr
                .iter()
                .filter_map(|t| {
                    // Gemini format
                    t.get("functionDeclarations")
                        .and_then(|fd| fd.as_array())
                        .map(|fds| {
                            fds.iter()
                                .filter_map(|f| f.get("name").and_then(|n| n.as_str()))
                                .collect::<Vec<_>>()
                                .join(", ")
                        })
                        .or_else(|| {
                            // OpenAI format
                            t.get("function")
                                .and_then(|f| f.get("name"))
                                .and_then(|n| n.as_str())
                                .map(|s| s.to_string())
                        })
                })
                .collect();
            names.join(", ")
        } else {
            "unknown".to_string()
        }
    })
}

fn extract_function_call(body: &Value) -> Option<String> {
    // Gemini format
    if let Some(candidates) = body.get("candidates").and_then(|v| v.as_array()) {
        for candidate in candidates {
            if let Some(parts) = candidate
                .get("content")
                .and_then(|c| c.get("parts"))
                .and_then(|p| p.as_array())
            {
                for part in parts {
                    if let Some(fc) = part.get("functionCall") {
                        let name = fc.get("name").and_then(|n| n.as_str()).unwrap_or("unknown");
                        let args = fc.get("args").map(|a| a.to_string()).unwrap_or_else(|| "{}".to_string());
                        return Some(format!("{}({})", name, args));
                    }
                }
            }
        }
    }

    // OpenAI format
    if let Some(choices) = body.get("choices").and_then(|v| v.as_array()) {
        for choice in choices {
            if let Some(tool_calls) = choice
                .get("message")
                .and_then(|m| m.get("tool_calls"))
                .and_then(|tc| tc.as_array())
            {
                let calls: Vec<String> = tool_calls
                    .iter()
                    .filter_map(|tc| {
                        let name = tc
                            .get("function")
                            .and_then(|f| f.get("name"))
                            .and_then(|n| n.as_str())
                            .unwrap_or("unknown");
                        let args = tc
                            .get("function")
                            .and_then(|f| f.get("arguments"))
                            .and_then(|a| a.as_str())
                            .unwrap_or("{}");
                        Some(format!("{}({})", name, args))
                    })
                    .collect();
                if !calls.is_empty() {
                    return Some(calls.join(", "));
                }
            }
        }
    }

    None
}

fn extract_function_response(body: &Value) -> Option<String> {
    // Gemini format
    if let Some(contents) = body.get("contents").and_then(|v| v.as_array()) {
        for content in contents {
            if let Some(parts) = content.get("parts").and_then(|p| p.as_array()) {
                for part in parts {
                    if let Some(fr) = part.get("functionResponse") {
                        let name = fr.get("name").and_then(|n| n.as_str()).unwrap_or("unknown");
                        let response = fr.get("response").map(|r| r.to_string()).unwrap_or_else(|| "{}".to_string());
                        return Some(format!("{} -> {}", name, response));
                    }
                }
            }
        }
    }

    // OpenAI format
    if let Some(messages) = body.get("messages").and_then(|v| v.as_array()) {
        for message in messages {
            if message.get("role").and_then(|r| r.as_str()) == Some("tool") {
                let content = message.get("content").and_then(|c| c.as_str()).unwrap_or("{}");
                let tool_call_id = message.get("tool_call_id").and_then(|id| id.as_str()).unwrap_or("unknown");
                return Some(format!("{} -> {}", tool_call_id, content));
            }
        }
    }

    None
}
