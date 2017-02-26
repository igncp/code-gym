module.exports = {
    parser: "babel-eslint",
    env: {
        browser: true,
        node: true,
        es6: true,
    },
    parserOptions: {
        "sourceType": "module",
    },
    globals: {
        cast: false,
    },
    plugins: [
        "import",
        "react",
    ],
    rules: {
        "accessor-pairs": 0, // enforce getter and setter pairs in objects
        "array-bracket-spacing": 2, // enforce consistent spacing inside array brackets
        "array-callback-return": 0, // enforce return statements in callbacks of array methods
        "block-scoped-var": 0, // enforce the use of variables within the scope they are defined
        "block-spacing": 2, // enforce consistent spacing inside single-line blocks
        "brace-style": 2, // enforce consistent brace style for blocks
        "callback-return": 0, // require return statements after callbacks
        "camelcase": [2, {properties: "never"}], // enforce camelcase naming convention
        "capitalized-comments": 0, // enforce or disallow capitalization of the first letter of a comment
        "class-methods-use-this": 0, // enforce that class methods utilize this
        "comma-dangle": [2, "always-multiline"], // require or disallow trailing commas
        "comma-spacing": 2, // enforce consistent spacing before and after commas
        "comma-style": 2, // enforce consistent comma style
        "complexity": 2, // enforce a maximum cyclomatic complexity allowed in a program
        "computed-property-spacing": 2, // enforce consistent spacing inside computed property brackets
        "consistent-return": 2, // require return statements to either always or never specify values
        "consistent-this": 0, // enforce consistent naming when capturing the current execution context
        "curly": 2, // enforce consistent brace style for all control statements
        "default-case": 0, // require default cases in switch statements
        "dot-location": [2, "property"], // enforce consistent newlines before and after dots
        "dot-notation": 2, // enforce dot notation whenever possible
        "eol-last": 2, // require or disallow newline at the end of files
        "eqeqeq": 2, // require the use of === and !==
        "func-call-spacing": 2, // require or disallow spacing between function identifiers and their invocations
        "func-name-matching": 0, // require function names to match the name of the variable or property to which they are assigned
        "func-names": 0, // require or disallow named function expressions
        "func-style": 2, // enforce the consistent use of either function declarations or expressions
        "global-require": 0, // require require() calls to be placed at top-level module scope
        "guard-for-in": 0, // require for-in loops to include an if statement
        "handle-callback-err": 0, // require error handling in callbacks
        "id-blacklist": 0, // disallow specified identifiers
        "id-length": 0, // enforce minimum and maximum identifier lengths
        "id-match": 0, // require identifiers to match a specified regular expression
        "indent": [2, 2], // enforce consistent indentation
        "init-declarations": 0, // require or disallow initialization in variable declarations
        "jsx-quotes": 0, // enforce the consistent use of either double or single quotes in JSX attributes
        "key-spacing": 2, // enforce consistent spacing between keys and values in object literal properties
        "keyword-spacing": 2, // enforce consistent spacing before and after keywords
        "line-comment-position": 0, // enforce position of line comments
        "linebreak-style": 2, // enforce consistent linebreak style
        "lines-around-comment": 0, // require empty lines around comments
        "lines-around-directive": 2, // require or disallow newlines around directives
        "max-depth": [2, 4], // enforce a maximum depth that blocks can be nested
        "max-len": [2, 120], // enforce a maximum line length
        "max-lines": 0, // enforce a maximum number of lines per file
        "max-nested-callbacks": [2, 2], // enforce a maximum depth that callbacks can be nested
        "max-params": 0, // enforce a maximum number of parameters in function definitions
        "max-statements": 0, // enforce a maximum number of statements allowed in function blocks
        "max-statements-per-line": 0, // enforce a maximum number of statements allowed per line
        "multiline-ternary": 0, // enforce newlines between operands of ternary expressions
        "new-cap": 0, // require constructor names to begin with a capital letter
        "new-parens": 2, // require parentheses when invoking a constructor with no arguments
        "newline-after-var": 2, // require or disallow an empty line after variable declarations
        "newline-before-return": 2, // require an empty line before return statements
        "newline-per-chained-call": 2, // require a newline after each call in a method chain
        "no-alert": 0, // disallow the use of alert, confirm, and prompt
        "no-array-constructor": 0, // disallow Array constructors
        "no-await-in-loop": 0, // disallow await inside of loops
        "no-bitwise": 0, // disallow bitwise operators
        "no-caller": 0, // disallow the use of arguments.caller or arguments.callee
        "no-case-declarations": 0, // disallow lexical declarations in case clauses
        "no-catch-shadow": 2, // disallow catch clause parameters from shadowing variables in the outer scope
        "no-cond-assign": 2, // disallow assignment operators in conditional expressions
        "no-console": 0, // disallow the use of console
        "no-constant-condition": 2, // disallow constant expressions in conditions
        "no-continue": 0, // disallow continue statements
        "no-control-regex": 0, // disallow control characters in regular expressions
        "no-debugger": 2, // disallow the use of debugger
        "no-delete-var": 0, // disallow deleting variables
        "no-div-regex": 0, // disallow division operators explicitly at the beginning of regular expressions
        "no-dupe-args": 2, // disallow duplicate arguments in function definitions
        "no-dupe-keys": 2, // disallow duplicate keys in object literals
        "no-duplicate-case": 2, // disallow duplicate case labels
        "no-else-return": 2, // disallow else blocks after return statements in if statements
        "no-empty": 2, // disallow empty block statements
        "no-empty-character-class": 0, // disallow empty character classes in regular expressions
        "no-empty-function": 2, // disallow empty functions
        "no-empty-pattern": 2, // disallow empty destructuring patterns
        "no-eq-null": 2, // disallow null comparisons without type-checking operators
        "no-eval": 2, // disallow the use of eval()
        "no-ex-assign": 2, // disallow reassigning exceptions in catch clauses
        "no-extend-native": 2, // disallow extending native types
        "no-extra-bind": 2, // disallow unnecessary calls to .bind()
        "no-extra-boolean-cast": 2, // disallow unnecessary boolean casts
        "no-extra-label": 2, // disallow unnecessary labels
        "no-extra-parens": 2, // disallow unnecessary parentheses
        "no-extra-semi": 2, // disallow unnecessary semicolons
        "no-fallthrough": 0, // disallow fallthrough of case statements
        "no-floating-decimal": 0, // disallow leading or trailing decimal points in numeric literals
        "no-func-assign": 2, // disallow reassigning function declarations
        "no-global-assign": 0, // disallow assignments to native objects or read-only global variables
        "no-implicit-coercion": 0, // disallow shorthand type conversions
        "no-implicit-globals": 0, // disallow variable and function declarations in the global scope
        "no-implied-eval": 0, // disallow the use of eval()-like methods
        "no-inline-comments": 0, // disallow inline comments after code
        "no-inner-declarations": 0, // disallow variable or function declarations in nested blocks
        "no-invalid-regexp": 0, // disallow invalid regular expression strings in RegExp constructors
        "no-invalid-this": 2, // disallow this keywords outside of classes or class-like objects
        "no-irregular-whitespace": 2, // disallow irregular whitespace outside of strings and comments
        "no-iterator": 0, // disallow the use of the __iterator__ property
        "no-label-var": 0, // disallow labels that share a name with a variable
        "no-labels": 0, // disallow labeled statements
        "no-lone-blocks": 2, // disallow unnecessary nested blocks
        "no-lonely-if": 0, // disallow if statements as the only statement in else blocks
        "no-loop-func": 2, // disallow function declarations and expressions inside loop statements
        "no-magic-numbers": 0, // disallow magic numbers
        "no-mixed-operators": 0, // disallow mixed binary operators
        "no-mixed-requires": 0, // disallow require calls to be mixed with regular variable declarations
        "no-mixed-spaces-and-tabs": 2, // disallow mixed spaces and tabs for indentation
        "no-multi-spaces": 2, // disallow multiple spaces
        "no-multi-str": 0, // disallow multiline strings
        "no-multiple-empty-lines": [2, {
            max: 1
        }], // disallow multiple empty lines
        "no-negated-condition": 0, // disallow negated conditions
        "no-nested-ternary": 0, // disallow nested ternary expressions
        "no-new": 0, // disallow new operators outside of assignments or comparisons
        "no-new-func": 0, // disallow new operators with the Function object
        "no-new-object": 0, // disallow Object constructors
        "no-new-require": 0, // disallow new operators with calls to require
        "no-new-wrappers": 0, // disallow new operators with the String, Number, and Boolean objects
        "no-obj-calls": 0, // disallow calling global object properties as functions
        "no-octal": 0, // disallow octal literals
        "no-octal-escape": 0, // disallow octal escape sequences in string literals
        "no-param-reassign": 2, // disallow reassigning function parameters
        "no-path-concat": 0, // disallow string concatenation with __dirname and __filename
        "no-plusplus": 0, // disallow the unary operators ++ and --
        "no-process-env": 0, // disallow the use of process.env
        "no-process-exit": 0, // disallow the use of process.exit()
        "no-proto": 2, // disallow the use of the __proto__ property
        "no-prototype-builtins": 0, // disallow calling some Object.prototype methods directly on objects
        "no-redeclare": 2, // disallow variable redeclaration
        "no-regex-spaces": 0, // disallow multiple spaces in regular expressions
        "no-restricted-globals": 2, // disallow specified global variables
        "no-restricted-modules": 2, // disallow specified modules when loaded by require
        "no-restricted-properties": 2, // disallow certain properties on certain objects
        "no-restricted-syntax": 2, // disallow specified syntax
        "no-return-assign": 2, // disallow assignment operators in return statements
        "no-return-await": 0, // disallow unnecessary return await
        "no-script-url": 0, // disallow javascript: urls
        "no-self-assign": 2, // disallow assignments where both sides are exactly the same
        "no-self-compare": 2, // disallow comparisons where both sides are exactly the same
        "no-sequences": 2, // disallow comma operators
        "no-shadow": 2, // disallow variable declarations from shadowing variables declared in the outer scope
        "no-shadow-restricted-names": 0, // disallow identifiers from shadowing restricted names
        "no-sparse-arrays": 2, // disallow sparse arrays
        "no-sync": 0, // disallow synchronous methods
        "no-tabs": 2, // disallow all tabs
        "no-template-curly-in-string": 0, // disallow template literal placeholder syntax in regular strings
        "no-ternary": 0, // disallow ternary operators
        "no-throw-literal": 2, // disallow throwing literals as exceptions
        "no-trailing-spaces": 2, // disallow trailing whitespace at the end of lines
        "no-undef": 2, // disallow the use of undeclared variables unless mentioned in /*global */ comments
        "no-undef-init": 2, // disallow initializing variables to undefined
        "no-undefined": 2, // disallow the use of undefined as an identifier
        "no-underscore-dangle": 0, // disallow dangling underscores in identifiers
        "no-unexpected-multiline": 0, // disallow confusing multiline expressions
        "no-unmodified-loop-condition": 2, // disallow unmodified loop conditions
        "no-unneeded-ternary": 2, // disallow ternary operators when simpler alternatives exist
        "no-unreachable": 2, // disallow unreachable code after return, throw, continue, and break statements
        "no-unsafe-finally": 0, // disallow control flow statements in finally blocks
        "no-unsafe-negation": 2, // disallow negating the left operand of relational operators
        "no-unused-expressions": 2, // disallow unused expressions
        "no-unused-labels": 2, // disallow unused labels
        "no-unused-vars": [2, {
            "vars": "local"
        }], // disallow unused variables
        "no-use-before-define": 2, // disallow the use of variables before they are defined
        "no-useless-call": 2, // disallow unnecessary calls to .call() and .apply()
        "no-useless-concat": 2, // disallow unnecessary concatenation of literals or template literals
        "no-useless-escape": 2, // disallow unnecessary escape characters
        "no-useless-return": 2, // disallow redundant return statements
        "no-void": 0, // disallow void operators
        "no-warning-comments": 0, // disallow specified warning terms in comments
        "no-whitespace-before-property": 2, // disallow whitespace before properties
        "no-with": 0, // disallow with statements
        "object-curly-newline": 2, // enforce consistent line breaks inside braces
        "object-curly-spacing": [2, "always"], // enforce consistent spacing inside braces
        "object-property-newline": 2, // enforce placing object properties on separate lines
        "one-var": 0, // enforce variables to be declared either together or separately in functions
        "one-var-declaration-per-line": 2, // require or disallow newlines around variable declarations
        "operator-assignment": 2, // require or disallow assignment operator shorthand where possible
        "operator-linebreak": 2, // enforce consistent linebreak style for operators
        "padded-blocks": [2, "never"], // require or disallow padding within blocks
        "quote-props": [2, "as-needed"], // require quotes around object literal property names
        "quotes": 2, // enforce the consistent use of either backticks, double, or single quotes
        "radix": 0, // enforce the consistent use of the radix argument when using parseInt()
        "require-await": 0, // disallow async functions which have no await expression
        "require-jsdoc": 0, // require JSDoc comments
        "semi": 2, // require or disallow semicolons instead of ASI
        "semi-spacing": 2, // enforce consistent spacing before and after semicolons
        "sort-keys": 2, // require object keys to be sorted
        "sort-vars": 0, // require variables within the same declaration block to be sorted
        "space-before-blocks": 2, // enforce consistent spacing before blocks
        "space-before-function-paren": [2, "never"], // enforce consistent spacing before function definition opening parenthesis
        "space-in-parens": 2, // enforce consistent spacing inside parentheses
        "space-infix-ops": 2, // require spacing around infix operators
        "space-unary-ops": 2, // enforce consistent spacing before or after unary operators
        "spaced-comment": 2, // enforce consistent spacing after the // or /* in a comment
        "strict": 0, // require or disallow strict mode directives
        "unicode-bom": 0, // require or disallow Unicode byte order mark (BOM)
        "use-isnan": 2, // require calls to isNaN() when checking for NaN
        "valid-jsdoc": [2, {
            requireReturn: false,
            requireParamDescription: false,
            requireReturnDescription: false,
            preferType: {
                Object: "object",
                String: "string",
                Number: "number",
                Function: "function",
            },
        }], // enforce valid JSDoc comments
        "valid-typeof": 2, // enforce comparing typeof expressions against valid strings
        "vars-on-top": 0, // require var declarations be placed at the top of their containing scope
        "wrap-iife": 2, // require parentheses around immediate function invocations
        "wrap-regex": 0, // require parenthesis around regex literals
        "yoda": 2, // require or disallow “Yoda” conditions

        // ES6
        "arrow-body-style": 0, // require braces around arrow function bodies
        "arrow-parens": 2, // require parentheses around arrow function arguments
        "arrow-spacing": 2, // enforce consistent spacing before and after the arrow in arrow functions
        "constructor-super": 0, // require super() calls in constructors
        "generator-star-spacing": 0, // enforce consistent spacing around * operators in generator functions
        "no-class-assign": 0, // disallow reassigning class members
        "no-confusing-arrow": 0, // disallow arrow functions where they could be confused with comparisons
        "no-const-assign": 2, // disallow reassigning const variables
        "no-dupe-class-members": 2, // disallow duplicate class members
        "no-duplicate-imports": 2, // disallow duplicate module imports
        "no-new-symbol": 0, // disallow new operators with the Symbol object
        "no-restricted-imports": 0, // disallow specified modules when loaded by import
        "no-this-before-super": 2, // disallow this/super before calling super() in constructors
        "no-useless-computed-key": 2, // disallow unnecessary computed property keys in object literals
        "no-useless-constructor": 2, // disallow unnecessary constructors
        "no-useless-rename": 2, // disallow renaming import, export, and destructured assignments to the same name
        "no-var": 2, // require let or const instead of var
        "object-shorthand": 2, // require or disallow method and property shorthand syntax for object literals
        "prefer-arrow-callback": 2, // require arrow functions as callbacks
        "prefer-const": 2, // require const declarations for variables that are never reassigned after declared
        "prefer-numeric-literals": 0, // disallow parseInt() in favor of binary, octal, and hexadecimal literals
        "prefer-rest-params": 0, // require rest parameters instead of arguments
        "prefer-spread": 0, // require spread operators instead of .apply()
        "prefer-template": 2, // require template literals instead of string concatenation
        "require-yield": 0, // require generator functions to contain yield
        "rest-spread-spacing": 2, // enforce spacing between rest and spread operators and their expressions
        "sort-imports": 0, // enforce sorted import declarations within modules
        "symbol-description": 0, // require symbol descriptions
        "template-curly-spacing": 2, // require or disallow spacing around embedded expressions of template strings
        "yield-star-spacing": 0, // require or disallow spacing around the * in yield* expressions

        "import/no-unresolved": 2, // Ensure imports point to a file/module that can be resolved.
        "import/named": 2, // Ensure named imports correspond to a named export in the remote file.
        "import/default": 2, // Ensure a default export is present, given a default import.
        "import/namespace": 2, // Ensure imported namespaces contain dereferenced properties as they are dereferenced.
        "import/no-restricted-paths": 0, // Restrict which files can be imported in a given folder
        "import/no-absolute-path": 2, // Forbid import of modules using absolute paths
        "import/no-dynamic-require": 2, // Forbid require() calls with expressions
        "import/no-internal-modules": 0, // Prevent importing the submodules of other modules
        "import/no-webpack-loader-syntax": 2, // Forbid Webpack loader syntax in imports
        "import/export": 2, // Report any invalid exports, i.e. re-export of the same name
        "import/no-named-as-default": 2, // Report use of exported name as identifier of default export
        "import/no-named-as-default-member": 2, // Report use of exported name as property of default export
        "import/no-deprecated": 2, // Report imported names marked with @deprecated documentation tag
        "import/no-extraneous-dependencies": 0, // Forbid the use of extraneous packages
        "import/no-mutable-exports": 2, // Forbid the use of mutable exports with var or let.
        "import/unambiguous": 0, // Report potentially ambiguous parse goal (script vs. module)
        "import/no-commonjs": 2, // Report CommonJS require calls and module.exports or exports.*.
        "import/no-amd": 2, // Report AMD require and define calls.
        "import/no-nodejs-modules": 2, // No Node.js builtin modules.
        "import/first": 2, // Ensure all imports appear before other statements
        "import/no-duplicates": 2, // Report repeated import of the same module in multiple places
        "import/no-namespace": 0, // Report namespace imports
        "import/extensions": 0, // Ensure consistent use of file extension within the import path
        "import/order": [0, {
          "newlines-between": "always"// soon a new version will be released with: "always-and-inside-groups",
        }], // Enforce a convention in module import order
        "import/newline-after-import": 2, // Enforce a newline after import statements
        "import/prefer-default-export": 0, // Prefer a default export if module exports a single name
        "import/max-dependencies": 0, // Limit the maximum number of dependencies a module can have
        "import/no-unassigned-import": 0, // Forbid unassigned imports
        "import/no-named-default": 0, // Forbid named default exports

        "react/display-name": 2, // Prevent missing displayName in a React component definition
        "react/forbid-component-props": 2, // Forbid certain props on Components
        "react/forbid-elements": 2, // Forbid certain elements
        "react/forbid-prop-types": 2, // Forbid certain propTypes
        "react/forbid-foreign-prop-types": 2, // Forbid foreign propTypes
        "react/no-array-index-key": 2, // Prevent using Array index in key props
        "react/no-children-prop": 2, // Prevent passing children as props
        "react/no-danger": 2, // Prevent usage of dangerous JSX properties
        "react/no-danger-with-children": 2, // Prevent problem with children and props.dangerouslySetInnerHTML
        "react/no-deprecated": 2, // Prevent usage of deprecated methods
        "react/no-did-mount-set-state": 2, // Prevent usage of setState in componentDidMount
        "react/no-did-update-set-state": 2, // Prevent usage of setState in componentDidUpdate
        "react/no-direct-mutation-state": 2, // Prevent direct mutation of this.state
        "react/no-find-dom-node": 2, // Prevent usage of findDOMNode
        "react/no-is-mounted": 2, // Prevent usage of isMounted
        "react/no-multi-comp": 2, // Prevent multiple component definition per file
        "react/no-render-return-value": 2, // Prevent usage of the return value of React.render
        "react/no-set-state": 0, // Prevent usage of setState
        "react/no-string-refs": 2, // Prevent using string references in ref attribute.
        "react/no-unescaped-entities": 2, // Prevent invalid characters from appearing in markup
        "react/no-unknown-property": 2, // Prevent usage of unknown DOM property (fixable)
        "react/no-unused-prop-types": 2, // Prevent definitions of unused prop types
        "react/prefer-es6-class": 2, // Enforce ES5 or ES6 class for React Components
        "react/prefer-stateless-function": 2, // Enforce stateless React Components to be written as a pure function
        "react/prop-types": 2, // Prevent missing props validation in a React component definition
        "react/react-in-jsx-scope": 2, // Prevent missing React when using JSX
        "react/require-default-props": 2, // Enforce a defaultProps definition for every prop that is not a required prop
        "react/require-optimization": 0, // Enforce React components to have a shouldComponentUpdate method
        "react/require-render-return": 2, // Enforce ES5 or ES6 class for returning value in render function
        "react/self-closing-comp": 2, // Prevent extra closing tags for components without children (fixable)
        "react/sort-comp": 2, // Enforce component methods order
        "react/sort-prop-types": 2, // Enforce propTypes declarations alphabetical sorting
        "react/style-prop-object": 2, // Enforce style prop value being an object
        "react/void-dom-elements-no-children": 2, // Prevent void DOM elements (e.g. <img />, <br />) from receiving children
        "react/jsx-boolean-value": 2, // Enforce boolean attributes notation in JSX (fixable)
        "react/jsx-closing-bracket-location": 2, // Validate closing bracket location in JSX (fixable)
        "react/jsx-curly-spacing": 2, // Enforce or disallow spaces inside of curly braces in JSX attributes (fixable)
        "react/jsx-equals-spacing": 2, // Enforce or disallow spaces around equal signs in JSX attributes (fixable)
        "react/jsx-filename-extension": 0, // Restrict file extensions that may contain JSX
        "react/jsx-first-prop-new-line": 2, // Enforce position of the first prop in JSX (fixable)
        "react/jsx-handler-names": 2, // Enforce event handler naming conventions in JSX
        "react/jsx-indent": [2, 2], // Validate JSX indentation (fixable)
        "react/jsx-indent-props": 2, // Validate props indentation in JSX (fixable)
        "react/jsx-key": 2, // Validate JSX has key prop when in array or iterator
        "react/jsx-max-props-per-line": 2, // Limit maximum of props on a single line in JSX
        "react/jsx-no-bind": 0, // Prevent usage of .bind() and arrow functions in JSX props
        "react/jsx-no-comment-textnodes": 2, // Prevent comments from being inserted as text nodes
        "react/jsx-no-duplicate-props": 2, // Prevent duplicate props in JSX
        "react/jsx-no-literals": 0, // Prevent usage of unwrapped JSX strings
        "react/jsx-no-target-blank": 2, // Prevent usage of unsafe target='_blank'
        "react/jsx-no-undef": 2, // Disallow undeclared variables in JSX
        "react/jsx-pascal-case": 2, // Enforce PascalCase for user-defined JSX components
        "react/jsx-sort-props": 2, // Enforce props alphabetical sorting
        "react/jsx-space-before-closing": 2, // Validate spacing before closing bracket in JSX (fixable)
        "react/jsx-tag-spacing": 2, // Validate whitespace in and around the JSX opening and closing brackets (fixable)
        "react/jsx-uses-react": 2, // Prevent React to be incorrectly marked as unused
        "react/jsx-uses-vars": 2, // Prevent variables used in JSX to be incorrectly marked as unused
        "react/jsx-wrap-multilines": 2, // Prevent missing parentheses around multilines JSX (fixable)
    },
};
