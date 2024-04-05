type typeNode = {
  kind: string,
}

type valueNode = {
  kind: string,
  value: string,
}

type nameNode = {
  kind: string,
  value: string,
};

type argumentNode = {
  name: nameNode,
  value: valueNode,
};

type directiveNode = {
  kind: string,
  arguments: array<argumentNode>,
}

type selectionSetNode = {
  kind: string,
}

type variableDefinitionNode = {
  kind: string,
};

type operationDefinitionNode = {
  kind: string,
  operation: string,
  name: option<nameNode>,
  variableDefinitions: array<variableDefinitionNode>,
  directives: array<directiveNode>,
  selectionSet: selectionSetNode,
};

type namedTypeNode = {
  kind: string,
  name: nameNode,
}

type fragmentDefinitionNode = {
  kind: string,
  name: nameNode,
  typeCondition: namedTypeNode,
  directives: array<directiveNode>,
  selectionSet: selectionSetNode,
};

type definitionNode =
  | OperationDefinition(operationDefinitionNode)
  | FragmentDefinition(fragmentDefinitionNode)

type documentNode = {
  kind: string,
  definitions: array<definitionNode>,
};

type tokenKind =
  | Name(string)
  | Punctuator(string)
  | Int(string)
  | Float(string)
  | String(string)
  | FragmentSpread
  | InlineFragment

type token = {
  kind: tokenKind,
  start: int,
  end: int,
  value: string,
}

let isNameStart = (ch) => {
  (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_'
};

let isNamePart = (ch) => {
  isNameStart(ch) || (ch >= '0' && ch <= '9')
};

let extractName = (source, start) => {
  let rec loop = (index) => {
    if (index < String.length(source) && isNamePart(String.get(source, index))) {
      loop(index + 1)
    } else {
      (String.sub(source, start, index - start), index)
    }
  }
  loop(start)
};

let isPunctuator = (ch) => {
  ch == '{' || ch == '}' || ch == '(' || ch == ')' || ch == ':' || ch == ','
};

let isDigit = (ch) => {
  ch >= '0' && ch <= '9'
};

let tokenizeString: string => (token, int) = (source) => {
  let rec loop = (index, acc) => {
    if (index < String.length(source) && String.get(source, index) != '"') {
      loop(index + 1, acc ++ String.make(1, String.get(source, index)))
    } else {
      ({kind: String(acc), start: 0, end: index + 1, value: acc}, index + 1)
    }
  }
  if (String.get(source, 0) == '"') {
    loop(1, "")
  } else {
    failwith("String tokenization error: String does not start with a double quote")
  }
};

let tokenizeNumber: string => (token, int) = (source) => {
  let rec loop = (index, isFloat) => {
    if (index < String.length(source) && isDigit(String.get(source, index))) {
      loop(index + 1, isFloat)
    } else if (!isFloat && index < String.length(source) && String.get(source, index) == '.') {
      loop(index + 1, true)
    } else {
      let value = String.sub(source, 0, index)
      let kind = if (isFloat) { Float(value) } else { Int(value) }
      ({kind: kind, start: 0, end: index, value: value}, index)
    }
  }
  loop(0, false)
};

let tokenize: string => array<token> = (source) => {
  let rec loop = (index, tokens) => {
    if (index >= String.length(source)) {
      tokens
    } else {
      let currentChar = String.get(source, index)
      if (isNameStart(currentChar)) {
        let (name, endIndex) = extractName(source, index)
        loop(endIndex, Array.concat(list{tokens, [{kind: Name(name), start: index, end: endIndex, value: name}]}))
      } else if (isPunctuator(currentChar)) {
        let punctuator = String.make(1, currentChar)
        loop(index + 1, Array.concat(list{tokens, [{kind: Punctuator(punctuator), start: index, end: index + 1, value: punctuator}]}))
      } else if (isDigit(currentChar)) {
        let (numberToken, endIndex) = tokenizeNumber(String.sub(source, index, String.length(source) - index))
        loop(endIndex + index, Array.concat(list{tokens, [numberToken]}))
      } else if (currentChar == '"') {
        let (stringToken, endIndex) = tokenizeString(String.sub(source, index, String.length(source) - index))
        loop(endIndex + index, Array.concat(list{tokens, [stringToken]}))
      } else {
        loop(index + 1, tokens)
      }
    }
  }
  loop(0, [])
};

let tokenizeName: string => token = (source) => {
  let (name, endIndex) = extractName(source, 0)
  {kind: Name(name), start: 0, end: endIndex, value: name}
};

let parseValue: token => valueNode = (token) => {
  switch token.kind {
  | Int(value) => {kind: "IntValue", value: value}
  | Float(value) => {kind: "FloatValue", value: value}
  | String(value) => {kind: "StringValue", value: value}
  | Name(value) => {kind: "EnumValue", value: value}
  | _ => failwith("Unsupported value token")
  }
};

let parseArguments = (tokens: array<token>, arguments: list<argumentNode>): (list<argumentNode>, array<token>) => {
  (arguments, tokens)
};

let parseDirectives = (tokens: array<token>, directives: list<directiveNode>): (list<directiveNode>, array<token>) => {
  (directives, tokens)
};

type fieldNode = {
  kind: string,
  name: nameNode,
  // Other fields will be added here as needed
};

type selectionNode =
  | Field(fieldNode)
  | FragmentSpread(fragmentDefinitionNode)
  | InlineFragment(fragmentDefinitionNode);

let parseField = (tokens: array<token>): (fieldNode, array<token>) => {
  switch Array.get(tokens, 0) {
  | {kind: Name(name)} =>
    let field = {kind: "Field", name: {kind: "Name", value: name}}
    (field, Array.sub(tokens, 1, Array.length(tokens) - 1))
  | _ => failwith("Expected a field name token")
  }
};

let rec parseSelections: (array<token>, array<selectionNode>) => (array<selectionNode>, array<token>) = (tokens, selections) => {
  if (Array.length(tokens) > 0) {
    let first = Array.get(tokens, 0)
    switch first.kind {
      | Punctuator("{") =>
        let rest = Array.sub(tokens, 1, Array.length(tokens) - 1)
        parseSelections(rest, selections)
      | Punctuator("}") =>
        (selections, Array.sub(tokens, 1, Array.length(tokens) - 1))
      | Name(name) =>
        let (fieldNode, remainingTokens) = parseField(tokens)
        let newSelections = Array.concat(selections, [|fieldNode|])
        parseSelections(remainingTokens, newSelections)
      | _ => failwith("Unexpected token in selection set")
    }
  } else {
    failwith("Empty token list in selection set")
  }
};

let parseSelectionSet = (tokens: array<token>): selectionSetNode => {
  let (selections, remainingTokens) = parseSelections(tokens, [])
  if (Array.length(remainingTokens) == 0) {
    {
      kind: "SelectionSet",
      selections: selections
    }
  } else {
    failwith("Token list did not end after parsing selection set")
  }
};

let parse: string => documentNode = (source) => {
  let tokens = tokenize(source)
  if (Array.length(tokens) > 0) {
    let first = Array.get(tokens, 0)
    switch first.kind {
      | Name("query") =>
        let rest = Array.sub(tokens, 1, Array.length(tokens) - 1)
        let operationDefinition = {
          kind: "OperationDefinition",
          operation: "query",
          name: None,
          variableDefinitions: [],
          directives: [],
          selectionSet: parseSelectionSet(rest),
        }
        {
          kind: "Document",
          definitions: [OperationDefinition(operationDefinition)]
        }
      | _ => failwith("Unsupported operation or syntax error")
    }
  } else {
    failwith("Empty token list")
  }
};

let parseNestedSelectionSet = (tokens) => {
  if (Array.length(tokens) > 0 && Array.get(tokens, 0).kind == Punctuator("{")) {
    let rest = Array.sub(tokens, 1, Array.length(tokens) - 1)
    let (selectionSet, remainingTokens) = parseSelectionSet(rest)
    (selectionSet, remainingTokens)
  } else {
    failwith("Expected the start of a nested selection set")
  }
};
