namespace LeanllmTests.Golden

/-- Encodings from `main` at 76c79fe (before structured output), keyed by `Baseline.cases` label. -/
def encodings : List (String × String) := [
  ("anthropic",
    "{\"max_tokens\":256,\"messages\":[{\"content\":[{\"text\":\"Who wrote the Analytical Engine notes?\",\"type\":\"text\"}],\"role\":\"user\"}],\"model\":\"claude-sonnet-5-5\",\"system\":\"Be terse.\",\"tools\":[{\"description\":\"Get the current weather for a city\",\"input_schema\":{\"properties\":{\"city\":{\"type\":\"string\"}},\"required\":[\"city\"],\"type\":\"object\"},\"name\":\"get_weather\"}]}")
  ,("anthropic-oauth",
    "{\"max_tokens\":4096,\"messages\":[{\"content\":[{\"text\":\"Who wrote the Analytical Engine notes?\",\"type\":\"text\"}],\"role\":\"user\"}],\"model\":\"claude-sonnet-5-5\",\"system\":[{\"text\":\"x-anthropic-billing-header: cc_version=2.1.280; cc_entrypoint=sdk-cli; cch=33f85;\",\"type\":\"text\"},{\"text\":\"You are a Claude agent, built on Anthropic's Claude Agent SDK.\",\"type\":\"text\"},{\"text\":\"Be terse.\",\"type\":\"text\"}]}")
  ,("bedrock-invoke",
    "{\"anthropic_version\":\"bedrock-2023-05-31\",\"max_tokens\":4096,\"messages\":[{\"content\":[{\"text\":\"Who wrote the Analytical Engine notes?\",\"type\":\"text\"}],\"role\":\"user\"}],\"system\":\"Be terse.\"}")
  ,("bedrock-converse",
    "{\"messages\":[{\"content\":[{\"text\":\"Who wrote the Analytical Engine notes?\"}],\"role\":\"user\"}],\"system\":[{\"text\":\"Be terse.\"}],\"toolConfig\":{\"tools\":[{\"toolSpec\":{\"description\":\"Get the current weather for a city\",\"inputSchema\":{\"json\":{\"properties\":{\"city\":{\"type\":\"string\"}},\"required\":[\"city\"],\"type\":\"object\"}},\"name\":\"get_weather\"}}]}}")
  ,("openai-chat",
    "{\"messages\":[{\"content\":\"Be terse.\",\"role\":\"system\"},{\"content\":\"Who wrote the Analytical Engine notes?\",\"role\":\"user\"}],\"model\":\"gpt-5.4\",\"temperature\":0.5,\"tools\":[{\"function\":{\"description\":\"Get the current weather for a city\",\"name\":\"get_weather\",\"parameters\":{\"properties\":{\"city\":{\"type\":\"string\"}},\"required\":[\"city\"],\"type\":\"object\"}},\"type\":\"function\"}]}")
  ,("openrouter-chat",
    "{\"messages\":[{\"content\":\"Be terse.\",\"role\":\"system\"},{\"content\":\"Who wrote the Analytical Engine notes?\",\"role\":\"user\"}],\"model\":\"deepseek/deepseek-v4-flash\"}")
  ,("openai-responses",
    "{\"input\":[{\"content\":[{\"text\":\"Who wrote the Analytical Engine notes?\",\"type\":\"input_text\"}],\"role\":\"user\",\"type\":\"message\"}],\"instructions\":\"Be terse.\",\"max_output_tokens\":128,\"model\":\"gpt-5.4\"}")
  ,("chatgpt-oauth-responses",
    "{\"input\":[{\"content\":[{\"text\":\"Who wrote the Analytical Engine notes?\",\"type\":\"input_text\"}],\"role\":\"user\",\"type\":\"message\"}],\"instructions\":\"Be terse.\",\"model\":\"gpt-5.4\",\"parallel_tool_calls\":false,\"store\":false,\"stream\":true,\"tool_choice\":\"auto\",\"tools\":[]}")
  ,("gemini",
    "{\"contents\":[{\"parts\":[{\"text\":\"Who wrote the Analytical Engine notes?\"}],\"role\":\"user\"}],\"generationConfig\":{\"maxOutputTokens\":64},\"systemInstruction\":{\"parts\":[{\"text\":\"Be terse.\"}],\"role\":\"user\"}}")
  ,("gemini-code-assist",
    "{\"model\":\"gemini-3.5-flash\",\"project\":\"proj-1\",\"request\":{\"contents\":[{\"parts\":[{\"text\":\"Who wrote the Analytical Engine notes?\"}],\"role\":\"user\"}],\"systemInstruction\":{\"parts\":[{\"text\":\"Be terse.\"}],\"role\":\"user\"}},\"user_prompt_id\":\"leanllm-turn\"}")
]

end LeanllmTests.Golden
