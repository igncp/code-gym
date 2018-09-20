// @flow

type T_Token = {|
  type: 'word',
  value: string
|} | {|
  type: 'number',
  value: number
|} | {|
  type: 'newline'
|}

type T_ASTNumberNode = {|
  type: "NumberLiteral",
  value: number
|}

type T_ASTNode = {|
  type: "CallExpression",
  name: "Paper",
  args: Array<T_ASTNumberNode>
|} | {|
  type: "CallExpression",
  name: 'Pen',
  args: Array<T_ASTNumberNode>
|}

type T_ASTRootNode = {|
  type: "Drawing",
  body: Array<T_ASTNode>
|}

type T_lexer = (string) => Array<T_Token>

const lexer: T_lexer = code =>
  code
    .split(/\s+/)
    .filter(t => t.length > 0)
    .map(t => {
      if (t === '\n') {
        return {
          type: 'newline'
        }
      }

      return isNaN(t)
        ? {
            type: "word",
            value: t
          }
        : {
            type: "number",
            value: Number(t)
          };
    });

type T_parser = (Array<T_Token>) => T_ASTRootNode

const parser: T_parser = tokens => {
  const AST = {
    type: "Drawing",
    body: []
  };

  // Extract a token at a time as current_token. Loop until we are out of tokens.
  while (tokens.length > 0) {
    const current_token = tokens.shift();

    if (current_token.type === 'newline') {
      continue;
    }

    // Since number token does not do anything by it self, we only analyze syntax when we find a word.
    if (current_token.type === "word") {
      switch (current_token.value) {
        case "Paper":
          const paperExpression = {
            type: "CallExpression",
            name: "Paper",
            args: []
          };

          const paperArgument = tokens.shift();

          if (!paperArgument) {
            throw "Unexpected end of input";
          }

          if (paperArgument.type === "number") {
            paperExpression.args.push({
              type: "NumberLiteral",
              value: paperArgument.value
            });

            AST.body.push(paperExpression);
          } else {
            throw "Paper command must be followed by a number.";
          }

          if (tokens.length > 0 && tokens[0].type === "number") {
            throw "Paper comman can only have one number";
          }
          break;

        case 'Pen':
          const expression = {
            type: "CallExpression",
            name: "Pen",
            args: []
          };

          const penArgument = tokens.shift();

          if (!penArgument) {
            throw "Unexpected end of input";
          }

          if (penArgument.type === "number") {
            expression.args.push({
              type: "NumberLiteral",
              value: penArgument.value
            });

            AST.body.push(expression);
          } else {
            throw "Paper command must be followed by a number.";
          }

          if (tokens.length > 0 && tokens[0].type === "number") {
            throw "Paper comman can only have one number";
          }
          break;

        /*
         * case 'Line':
         * ...
         */
      }
    }
  }

  return AST;
};

const transformer = ast => {
  const svg_ast = {
    tag: "svg",
    attr: {
      width: 100,
      height: 100,
      viewBox: "0 0 100 100",
      xmlns: "http://www.w3.org/2000/svg",
      version: "1.1"
    },
    body: []
  };

  // Default pen color is black
  let pen_color = 100;

  // Extract a call expression at a time as `node`. Loop until we are out of expressions in body.
  while (ast.body.length > 0) {
    const node = ast.body.shift();
    switch (node.name) {
      case "Paper":
        const paper_color = 100 - (node.args[0].value: any);
        svg_ast.body.push({
          // Add rect element information to svg_ast's body
          tag: "rect",
          attr: {
            x: 0,
            y: 0,
            width: 100,
            height: 100,
            fill: `rgb(${paper_color}%,${paper_color}%,${paper_color}%)`
          }
        });
        break;
      case "Pen":
        // Keep current pen color in `pen_color` variable
        pen_color = 100 - node.args[0].value;
        break;

      case "Line":
        svg_ast.body.push({
          tag: "line",
          attr: {
            x1: "78",
            y1: 73,
            x2: "50",
            y2: 23,
            stroke: pen_color,
            "stroke-linecap": "round"
          }
        });
        break;
    }
  }
  return svg_ast;
};

const generator = svg_ast => {
  // Create attributes string out of attr object
  // { "width": 100, "height": 100 } becomes 'width="100" height="100"'
  const createAttrString = attr => {
    return Object.keys(attr)
      .map(key => `${key}="${attr[(key: any)]}"`)
      .join(" ");
  };

  // Top node is always <svg>. Create attributes string for svg tag
  const svg_attr = createAttrString(svg_ast.attr);

  // For each elements in the body of svg_ast, generate svg tag
  const elements = svg_ast.body
    .map(node => `<${node.tag} ${createAttrString(node.attr)}></${node.tag}>`)
    .join("\n\t");

  const svgRoot = `<svg ${svg_attr}>`;

  if (elements.length === 0) {
    return `${svgRoot}</svg>`;
  }

  return `${svgRoot}\n${elements}\n</svg>`;
};

type T_compile = string => string;

const compile: T_compile = inputCode => {
  const tokens = lexer(inputCode);
  const ast = parser(tokens);
  const svg_ast = transformer(ast);
  const svg_code = generator(svg_ast);

  return svg_code;
};

module.exports.compile = compile;
