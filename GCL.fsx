// This script implements our interactive calculator

// We need to import a couple of modules, including the generated lexer and parser
open System.IO;;
#load "import.fsx"
#load "GCLTypesAST.fs"
open GCLTypesAST
#load "graphPrinter.fsx"
open graphPrinter

#load "Compiler.fsx"
open Compiler
#load "Interpreter.fsx"
open Interpreter
#load "Verificator.fsx"
open Verificator

//#load "printer.fsx"
//open printer
//#load "eval.fsx"
//open eval
open FSharp.Text.Lexing
open System
#load "GCLParser.fs"
open GCLParser
#load "GCLLexer.fs"
open GCLLexer

let rec signAnalyser g = 
    match g with
        | []      -> []
        | e::tail -> e::(signAnalyser tail);;

let sign (n : float) =
    if n > 0.0 then
        "+"
    else if n = 0.0 then
        "0"
    else
        "-";;

let plusOperation (n1 : float) (n2 : float) = 
    match (sign n1, sign n2) with
        | (s  , "0") -> Set.ofList [s]
        | ("0", s  ) -> Set.ofList [s]
        | ("-", "-") -> Set.ofList ["-"]
        | ("+", "-") -> Set.ofList ["-"; "0"; "+"]
        | ("-", "+") -> Set.ofList ["-"; "0"; "+"]
        | ("+", "+") -> Set.ofList ["+"];;

let minusOperation (n1 : float) (n2 : float) = 
    match (sign n1, sign n2) with
        | (s  , "0") -> Set.ofList [s]
        | ("+", "-") -> Set.ofList ["+"]
        | ("0", "-") -> Set.ofList ["+"]
        | ("0", "+") -> Set.ofList ["-"]
        | ("-", "+") -> Set.ofList ["-"]
        | ("+", "+") -> Set.ofList ["-"; "0"; "+"]
        | ("-", "-") -> Set.ofList ["-"; "0"; "+"];;

let timesOperation (n1 : float) (n2 : float) = 
    match (sign n1, sign  n2) with
        | (_  , "0") -> Set.ofList ["0"]
        | ("0", _  ) -> Set.ofList ["0"]
        | ("+", "+") -> Set.ofList ["+"]
        | ("-", "+") -> Set.ofList ["-"]
        | ("+", "-") -> Set.ofList ["-"]
        | ("-", "-") -> Set.ofList ["+"];;

let divOperation (n1 : float) (n2 : float) = 
    match (sign n1, sign n2) with
        | ("-", "-") -> Set.ofList ["+"]
        | ("+", "-") -> Set.ofList ["-"]
        | ("0", _  ) -> Set.ofList ["0"]
        | (_  , "0") -> failwith "Division by 0"
        | ("-", "+") -> Set.ofList ["-"]
        | ("+", "+") -> Set.ofList ["+"];;

let rec arrOperation (arrMem : float list) =
    match arrMem with
        | []      -> []
        | e::tail -> (sign e)::(arrOperation tail);;

let rec evalOperator op (varMem : Map<string, float>) (arrMem : Map<string, float list>) =
    match op with

        | Num(n)              -> Set.ofList [sign n]
        | Variable(var)       -> Set.ofList [sign (varMem.[var])]
        | ArrayVariable(s, a) -> if Set.count (Set.intersect (evalOperator a varMem arrMem) (Set.ofList ["0"; "+"])) > 0 then
                                    Set.ofList (arrOperation arrMem.[s])
                                 else
                                    Set.empty
        | PlusExpr(a1, a2)    -> plusOperation  (evalA a1 varMem arrMem) (evalA a2 varMem arrMem)
        | MinusExpr(a1, a2)   -> minusOperation (evalA a1 varMem arrMem) (evalA a2 varMem arrMem)
        | TimesExpr(a1, a2)   -> timesOperation (evalA a1 varMem arrMem) (evalA a2 varMem arrMem)
        | DivExpr(a1 ,a2)     -> divOperation   (evalA a1 varMem arrMem) (evalA a2 varMem arrMem);;
// We He
let parse (input:string) : cmd =   
    // translate string into a buffer of characters
    let lexbuf = LexBuffer<char>.FromString input
    // translate the buffer into a stream of tokens and parse them
    let res = GCLParser.start GCLLexer.tokenize lexbuf
    // return the result of parsing (i.e. value of type "expr") 
    res

// We implement here the function that interacts with the user
let rec compute (n:int) =
    if n = 0 then
        printfn "Bye bye ♿"
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
        
        //printfn "AST ☢ SHEEEEEEEEEEEEEEEEEEEEESH: %A" (e)
        //printfn "Result: %A" (printer e)
        let graph = (edges "q▷" "q◀" e)
        
        //Verificator:
        //num <- 1
        //let nodes = (["q▷"]@(coverNodes "q▷" "q◀" e)@["q◀"])
        //printfn "Cover nodes %A" nodes
        //printfn "Enter conditions:"
        //let conditions = Map.ofList (getConditions nodes)
        //printfn "SPF %A" (SPF graph nodes nodes)
        //printfn "SPF\n%s" (printSPFS (SPF graph nodes nodes) conditions)
        
        //Sign analyser

        
        //printfn "Result:\n%A" e
        printfn "Graph:"
        File.WriteAllText("graph.dot", (makeEdge (bubbleDown (bubbleSort graph (List.length graph)))))
        //printfn "<<<%s>>>" (makeEdge (bubbleDown (bubbleSort graph (List.length graph))))
        let varMem = Map [("n",10.)]
        let arrMem = Map [("A",[6.;3.;2.;-4.;3.;7.;99.;69.;-69.;55555.])]
        let (node, status, newVarMem, newArrMem) = (evalGraph "q▷" graph varMem arrMem)
        //printfn "Status: %s\nNode: %s\nMemory:\n%A\n%A" status node newVarMem newArrMem//(prettyPrintVar newVarMem) (prettyPrintArr newArrMem) 
        compute n
        with e ->
            printfn "Error: %A" e
            compute (n-1);;

// Start interacting with the user
compute 20;;
//             
//                 .
//                .;;:,.
//                 ;iiii;:,.                                   .,:;.
//                 :i;iiiiii:,                            .,:;;iiii.
//                  ;iiiiiiiii;:.                    .,:;;iiiiii;i:
//                   :iiiiiiiiiii:......,,,,,.....,:;iiiiiiiiiiii;
//                    ,iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii:
//                     .:iii;iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii;,
//                       .:;;iiiiiiiiiiiiiiiiiiiiiiiiiii;;ii;,
//                        :iiii;;iiiiiiiiiiiiiii;;iiiiiii;:.
