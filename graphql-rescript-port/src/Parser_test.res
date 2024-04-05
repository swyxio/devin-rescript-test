open Jest

describe("GraphQL Parser", () => {
  test("parse simple query", () => {
    let query = "{ user { id, name } }"
    let parsed = Parser.parse(query)
    expect(parsed).toBeDefined()
  })

  test("parse query with arguments", () => {
    let query = "{ user(id: \"1\") { id, name } }"
    let parsed = Parser.parse(query)
    expect(parsed).toBeDefined()
  })

  test("tokenize simple query", () => {
    let query = "{ user { id, name } }"
    let tokens = Parser.tokenize(query)
    expect(tokens |> Array.length).toBeGreaterThan(0)
    // We expect the first token to be a Punctuator '{'
    expect(tokens[0].kind).toBe(Punctuator("{"))
    // We expect the second token to be a Name 'user'
    expect(tokens[1].kind).toBe(Name("user"))
    // Additional assertions will be added here to check the rest of the tokens
  })

  // Additional tests will be added here as the parser implementation progresses
})
