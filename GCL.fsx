// This script implements our interactive calculator

// We need to import a couple of modules, including the generated lexer and parser
#load "import.fsx"
//#r "FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"
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
let rec eval e (mem : Map<string, float>) =
  match e with
    | Num(x) -> x
    | Variable(x) -> mem.[x]
    | TimesExpr(x,y) -> (eval x mem) * (eval y mem)
    | DivExpr(x,y) -> (eval x mem) / (eval y mem)
    | PlusExpr(x,y) -> (eval x mem) + (eval y mem)
    | MinusExpr(x,y) -> (eval x mem) - (eval y mem)
    | PowExpr(x,y) -> (eval x mem) ** (eval y mem)
    | UPlusExpr(x) -> (eval x mem)
    | UMinusExpr(x) -> - (eval x mem)

let rec evalCmd cmd (mem : Map<string, float>) =
    match cmd with
        | Assign(x, a) -> mem.Add(x, (eval a mem))
        | Cmds(c1, c2) -> evalCmd c2 (evalCmd c1 mem);;


    
// We He
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
        printfn "Result: %A" (evalCmd e Map.empty)
        compute n
        with e -> compute (n-1)

// Start interacting with the user
compute 3
