// This script implements our interactive calculator

// We need to import a couple of modules, including the generated lexer and parser
#r "C:/Users/giddi/.nuget/packages/fslexyacc/10.0.0/build/fsyacc/net46/FsLexYacc.Runtime.dll"
open FSharp.Text.Lexing
open System
#load "GCLTypesAST.fs"
open GCLTypesAST
#load "GCLParser.fs"
open GCLParser
#load "GCLLexer.fs"
open GCLLexer

// We define the evaluation function recursively, by induction on the structure
// of arithmetic expressions (AST of type expr)
let rec eval e =
  match e with
    | Num(x) -> x
    | TimesExpr(x,y) -> eval(x) * eval (y)
    | DivExpr(x,y) -> eval(x) / eval (y)
    | PlusExpr(x,y) -> eval(x) + eval (y)
    | MinusExpr(x,y) -> eval(x) - eval (y)
    | PowExpr(x,y) -> eval(x) ** eval (y)
    | UPlusExpr(x) -> eval(x)
    | UMinusExpr(x) -> - eval(x)

// We
let parse input =
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = GCLParser.start GCLLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr")
    res

// We implement here the function that interacts with the user
let rec compute n =
    if n = 0 then
        printfn "Bye bye"
    else
        printfn "Enter an arithmetic expression: "
        try
        // We parse the input string
        let e = parse (Console.ReadLine())
        // and print the result of evaluating it
        printfn "Result: %f" (eval(e))
        compute n
        with e -> compute (n-1)

// Start interacting with the user
compute 3;;
