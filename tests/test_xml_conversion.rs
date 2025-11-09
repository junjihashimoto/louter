use louter::conversion::extract_and_convert_xml_tool_calls;

#[test]
fn test_xml_to_json_conversion_single_tool() {
    let xml_content = r#"Sure, I'll help you write to a file.
<tool_call>
  <function=WriteFile>
    <parameter=file_path>test.txt</parameter>
    <parameter=content>Hello World!</parameter>
  </function>
</tool_call>
Let me know if you need anything else."#;

    let (cleaned_text, tool_calls) = extract_and_convert_xml_tool_calls(xml_content);

    // Verify XML tags were removed
    assert!(!cleaned_text.contains("<tool_call>"));
    assert!(!cleaned_text.contains("</tool_call>"));

    // Verify text content remains
    assert!(cleaned_text.contains("Sure, I'll help you write to a file."));
    assert!(cleaned_text.contains("Let me know if you need anything else."));

    // Verify tool call was extracted
    assert_eq!(tool_calls.len(), 1);

    let tool_call = &tool_calls[0];
    assert_eq!(tool_call.function.name, "WriteFile");
    assert_eq!(tool_call.tool_type, "function");
    assert!(tool_call.id.starts_with("call_"));

    // Verify arguments were converted to JSON
    let args: serde_json::Value = serde_json::from_str(&tool_call.function.arguments)
        .expect("Arguments should be valid JSON");

    assert_eq!(args["file_path"], "test.txt");
    assert_eq!(args["content"], "Hello World!");
}

#[test]
fn test_xml_to_json_conversion_multiple_tools() {
    let xml_content = r#"I'll create both files for you.
<tool_call>
  <function=WriteFile>
    <parameter=file_path>file1.txt</parameter>
    <parameter=content>First file</parameter>
  </function>
</tool_call>
<tool_call>
  <function=WriteFile>
    <parameter=file_path>file2.txt</parameter>
    <parameter=content>Second file</parameter>
  </function>
</tool_call>
Done!"#;

    let (cleaned_text, tool_calls) = extract_and_convert_xml_tool_calls(xml_content);

    // Verify all XML tags were removed
    assert!(!cleaned_text.contains("<tool_call>"));

    // Verify we extracted 2 tool calls
    assert_eq!(tool_calls.len(), 2);

    // Verify first tool call
    assert_eq!(tool_calls[0].function.name, "WriteFile");
    let args1: serde_json::Value = serde_json::from_str(&tool_calls[0].function.arguments).unwrap();
    assert_eq!(args1["file_path"], "file1.txt");
    assert_eq!(args1["content"], "First file");

    // Verify second tool call
    assert_eq!(tool_calls[1].function.name, "WriteFile");
    let args2: serde_json::Value = serde_json::from_str(&tool_calls[1].function.arguments).unwrap();
    assert_eq!(args2["file_path"], "file2.txt");
    assert_eq!(args2["content"], "Second file");
}

#[test]
fn test_xml_to_json_conversion_no_tools() {
    let plain_text = "This is just plain text without any tool calls.";

    let (cleaned_text, tool_calls) = extract_and_convert_xml_tool_calls(plain_text);

    // Text should remain unchanged
    assert_eq!(cleaned_text, plain_text);

    // No tool calls should be extracted
    assert_eq!(tool_calls.len(), 0);
}

#[test]
fn test_xml_to_json_conversion_complex_parameters() {
    let xml_content = r#"<tool_call>
  <function=ExecuteCommand>
    <parameter=command>ls -la /tmp</parameter>
    <parameter=working_dir>/home/user</parameter>
    <parameter=timeout>30</parameter>
  </function>
</tool_call>"#;

    let (cleaned_text, tool_calls) = extract_and_convert_xml_tool_calls(xml_content);

    assert_eq!(tool_calls.len(), 1);

    let tool_call = &tool_calls[0];
    assert_eq!(tool_call.function.name, "ExecuteCommand");

    let args: serde_json::Value = serde_json::from_str(&tool_call.function.arguments).unwrap();
    assert_eq!(args["command"], "ls -la /tmp");
    assert_eq!(args["working_dir"], "/home/user");
    assert_eq!(args["timeout"], "30");
}

#[test]
fn test_xml_to_json_conversion_openai_passthrough() {
    // This test verifies that when a backend returns XML format,
    // the proxy correctly converts it to OpenAI JSON format

    let backend_response_with_xml = r#"I'll help you with that.
<tool_call>
  <function=GetWeather>
    <parameter=location>Tokyo</parameter>
    <parameter=units>celsius</parameter>
  </function>
</tool_call>
The weather information will be fetched."#;

    let (cleaned_text, tool_calls) = extract_and_convert_xml_tool_calls(backend_response_with_xml);

    // Verify conversion produces OpenAI-compatible format
    assert_eq!(tool_calls.len(), 1);

    let tool_call = &tool_calls[0];

    // OpenAI format requirements:
    // 1. Must have an ID
    assert!(!tool_call.id.is_empty());

    // 2. Must have type = "function"
    assert_eq!(tool_call.tool_type, "function");

    // 3. Must have function with name and arguments
    assert_eq!(tool_call.function.name, "GetWeather");
    assert!(!tool_call.function.arguments.is_empty());

    // 4. Arguments must be valid JSON string
    let args: serde_json::Value = serde_json::from_str(&tool_call.function.arguments)
        .expect("Arguments must be valid JSON");

    assert_eq!(args["location"], "Tokyo");
    assert_eq!(args["units"], "celsius");

    // 5. Original XML should be removed from text
    assert!(!cleaned_text.contains("<tool_call>"));
    assert!(!cleaned_text.contains("</tool_call>"));
    assert!(cleaned_text.contains("I'll help you with that."));
    assert!(cleaned_text.contains("The weather information will be fetched."));
}

#[test]
fn test_xml_conversion_preserves_text_formatting() {
    let xml_content = r#"Here's the solution:

Step 1: Initialize variables
<tool_call>
  <function=SetVariable>
    <parameter=name>counter</parameter>
    <parameter=value>0</parameter>
  </function>
</tool_call>

Step 2: Process data
Done!"#;

    let (cleaned_text, tool_calls) = extract_and_convert_xml_tool_calls(xml_content);

    // Verify multiline text is preserved
    assert!(cleaned_text.contains("Here's the solution:"));
    assert!(cleaned_text.contains("Step 1: Initialize variables"));
    assert!(cleaned_text.contains("Step 2: Process data"));
    assert!(cleaned_text.contains("Done!"));

    // Verify tool call extracted
    assert_eq!(tool_calls.len(), 1);
    assert_eq!(tool_calls[0].function.name, "SetVariable");
}
