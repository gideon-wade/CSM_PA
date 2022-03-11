// This script implements our interactive calculator

// We need to import a couple of modules, including the generated lexer and parser
#load "import.fsx"
#load "GCLTypesAST.fs"
open GCLTypesAST
#load "printer.fsx"
open printer
#load "eval.fsx"
open eval
//#r "FsLexYacc.Runtime.10.0.0/lib/net46/FsLexYacc.Runtime.dll"
open FSharp.Text.Lexing
open System
#load "GCLParser.fs"
open GCLParser
#load "GCLLexer.fs"
open GCLLexer


        

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
        printfn "Enter an expression: "
        try
        let mutable prog = ""
        // We parse the input string
        let mutable read = Console.ReadLine()
        while (read <> "") do
            prog <- prog + " " + read
            read <- Console.ReadLine()
        let e = parse (prog)
       // printfn "AST ☢ SHEEEEEEEEEEEEEEEEEEEEESH: %A" (e)
        // and print the result of evaluating it 
        //printfn "Result: %A" (evalCmd e Map.empty)
        printfn "Result: %A" (printer e)
        compute n
        with e -> compute (n-1)

// Start interacting with the user
compute 20;;
