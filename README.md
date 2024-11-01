# Just another scheme-interpreter

A minimalist Scheme interpreter written in Haskell. Built with pure functions and pattern matching.

## What is this?

This is a Scheme interpreter that implements a subset of Scheme/Lisp functionality. It's written in Haskell because functional programming languages are particularly well-suited for building interpreters.

### Features

- Hand-rolled parser built from scratch
- Numbers (both integers and doubles)
- Basic Scheme constructs (`if`, `cond`, `define`, `lambda`)
- Quote support with `car`, `cdr`, and `cons`
- REPL with command history and tab completion
- Standard library with essential functions
- Built with pure functional programming

## Standard Library

The interpreter comes with a standard library (`lib/prelude.scm`) that implements basic functions:

```scheme
; Basic list operations
- append     ; Combines two lists
- reverse    ; Reverses a list
- range      ; Creates a list of numbers

; Higher-order functions
- map        ; Applies a function to each element
- filter     ; Selects elements matching a predicate
- foldl      ; Left fold
- foldr      ; Right fold (no tail-call optimization, unfortunately)

; Utilities
- factorial  ; Because every Scheme implementation needs one
```

## Getting Started

### Running the REPL

```bash
cabal run
```

Or alternatively:
```bash
stack run
```

### Example Programs

Here are some examples of what you can do:

```scheme
; Calculate factorial
(define (factoriel n)
    (if (eq? n 0)
        1
        (* n (factoriel (+ n -1)))))

(factoriel 5) ; => 120

; Create a range of numbers
(define (range n)
    (define (helper i)
    (if (eq? i n)
        (cons n '())
        (cons i (helper (+ i 1)))))
  (helper 1))

(range 5) ; => (1 2 3 4 5)

; Map squares over a list
(map (lambda (x) (* x x)) (range 4))
; => (1 4 9 16)

; Filter numbers
(filter (lambda (x) (eq? (+ x 1) 1)) (range 5))
; => (1)
```

## How It Works

The interpreter is split into three main components:

1. **Parser**: A parser combinator that transforms Scheme code into an AST.

2. **Interpreter**: The evaluation engine, using a State monad to track definitions and evaluate Scheme expressions.

3. **REPL**: An interactive environment with tab completion and history.

### Key Implementation Details

- Built from scratch without parser libraries
- Uses applicative functors for parsing
- Implements lexical scoping via an environment model
- Includes quote support for list manipulation
- Comes with a standard library of essential functions

## Known Limitations

- No tail-call optimization (stack overflow is possible with deep recursion)
- Limited error messages
- Parser may be more permissive than standard Scheme
- No type system

## Why?

- Educational value in implementing interpreters
- Scheme's minimal syntax makes it an ideal target language
- Haskell's strong type system and pattern matching are perfect for language implementation
- To better understand both Scheme and interpreter design

## Future Ideas

- [ ] Proper tail-call optimization
- [ ] Expanded standard library
- [ ] Improved error messages
- [ ] Macro system
- [ ] Better documentation

## Contributing

Found a bug? Want to add a feature? Feel free to submit a PR.

## License

Free to use and modify.

## Implementation Details

## Implementation Details

The core of the interpreter is built around two simple but powerful abstractions:

```haskell
newtype Parser a = Parser {runParser :: String -> Maybe (a,String)}
newtype Eval a = Eval {runEval :: State -> Either String (State, a)}
```

The `Parser` breaks down Scheme code token by token using parser combinators, while `Eval` handles the actual execution using a state monad to manage the environment. This clean separation between parsing and evaluation makes the codebase surprisingly manageable despite implementing a full programming language.

