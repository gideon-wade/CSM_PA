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

let sign n =
    if n > 0.0 then
        "+"
    else if n = 0.0 then
        "0"
    else
        "-";;

let rec varOperation (var : string) = 
    match var with 
        | 

let plusOperation (n1 : float) (n2 : float) = 
    match (sign n1, sign n2) with
        | (s, "0")   -> Set.ofList [s]
        | ("0", s)   -> Set.ofList [s]
        | ("-", "-") -> Set.ofList ["-"]
        | ("+", "-") -> Set.ofList ["-"; "0"; "+"]
        | ("-", "+") -> Set.ofList ["-"; "0"; "+"]
        | ("+", "+") -> Set.ofList ["+"];;

let minusOperation (n1 : float) (n2 : float) = 
    match (sign n1, sign n2) with
        | (s, "0")   -> Set.ofList [s]
        | ("+", "-") -> Set.ofList ["+"]
        | ("0", "-") -> Set.ofList ["+"]
        | ("0", "+") -> Set.ofList ["-"]
        | ("-", "+") -> Set.ofList ["-"]
        | ("+", "+") -> Set.ofList ["-";"0";"+"]
        | ("-", "-") -> Set.ofList ["-";"0";"+"];;

let timesOperation (n1 : float) (n2 : float) = 
    match (sign n1, sign  n2) with
        | (_, "0")   -> Set.ofList ["0"]
        | ("0", _)   -> Set.ofList ["0"]
        | ("+", "+") -> Set.ofList ["+"]
        | ("-", "+") -> Set.ofList ["-"]
        | ("+", "-") -> Set.ofList ["-"]
        | ("-", "-") -> Set.ofList ["+"];;

let divOperation (n1 : float) (n2 : float) = 
    match (sign n1, sign n2) with
        | ("-", "-") -> Set.ofList ["+"]
        | ("+", "-") -> Set.ofList ["-"]
        | ("0", _)   -> Set.ofList ["0"]
        | (_, "0") -> failwith "Division by 0"
        | ("-", "+") -> Set.ofList ["-"]
        | ("+", "+") -> Set.ofList ["+"];;

let rec evalOperator op (varMem : Map<string, float>) (arrMem : Map<string, float list>) =
    match op with
        | Num(n)              -> Set.ofList [sign n]
        | Variable(var)       -> Set.ofList [varOperation var]
        | PlusExpr(a1, a2)    -> plusOperation  (evalA a1 varMem arrMem) (evalA a2 varMem arrMem)
        | MinusExpr(a1, a2)   -> minusOperation (evalA a1 varMem arrMem) (evalA a2 varMem arrMem)
        | TimesExpr(a1, a2)   -> timesOperation (evalA a1 varMem arrMem) (evalA a2 varMem arrMem)
        | DivExpr(a1 ,a2)     -> divOperation   (evalA a1 varMem arrMem) (evalA a2 varMem arrMem)
        | ArrayVariable(s, a) -> arrayOperation 
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
//               ..,,:::;;iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii;::1ffLLLLLLLLLLL
//              ..,,::;iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii;::::iiiiiiii;ii11ttttttttt
//           .,,,:;;;;iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii:. ,:;iiiiiiiiiiiii;;;;;;;;;;
//         .,..,:;iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii:. ,t1iiiiiiiiiiiiiiiiiiiiiiiii
//            :iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii;,  ;C00L1iiiiiiiiiiiiiiiiiiiiiii
//           :iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii;,  :tG0000Gfiiiiiiiiiiiiiiiiiiiiii
// ::::::,,..;iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii:. ,tG00000000Liiiiiiiiiiiiiiiiiiiii
// ;;;;iiii::iiiiiiiiiiiiiiiiiiiii;iiiiiiiiii;.  ;C00000000000C1iiiiiiiiiiiiiiiiiii
// 11i;::;i;;iiiiiiiiiiiiiiii;;;iiii;iiiiii;,  :f0Ct11fC0000000G1;iiiiiiii;;;;i:;ii
// 11111i;:;iiiiiiiiiiiiiiiiiiiiiiii:;iii;,  ,tG00i,. .,iL000000L,:iii;;;;itLCf:ii;
// 1111111i:;iiiiiiiiiiiiiiiiiiii;;iii;:,..,:t0000t,    .:L0000C;,,;;;ifCG88@@L:;;:
// 11111111i;iiiiiiiiiiiiiiiiiiiiiii;;;:::;ii;f000Gt:.. .:f000L;,::;tG8@@@@88Gt:;;i
// 11111111i;iiiiiiiiiiiiiiiiiiiiiii,.:,::;;ii;f0000Cft1tLG0Gf::,,iC8@@8@80CC0C;iii
// 11111111;;iiiiiiiiiiiiiiiiiiii;,.  :;;:;:;ii;1C00000000GLi::,iC8@88@80CG8@@81;;i
// 1111iii;;iiiiiiiiiiiiiiiiiii;,  ..:iiii;:;iii;itCGGGCLti;:::f8@888@8CC8@@@@@8L1i
// 1111;;;;;;;;iiiiiiiiiiiiii;.  .iLCt;;ii;;iiiiii;;i;:::,:::;L8@888@0L0@@@@@@@@8Gf
// 1111111i;;:;iiiiiiiiiiii;,  .1C0000Gt;;iiiiiiiiiii;:;;;;;;G@8888@GC8@@@@@@@8GG08
// 111111i;;;;:iiiiiiiiiii:.  :LGLLLCG00C1;;iiiiiiiiiiiiii;iG@888@@CC@@@@@@@0CG8@@@
// i1111111111;;iiiiiiiii:  .1GL;,..,iL000Li;iiiiiiiiiiii;i0@888@@0C@@@@@@8CC8@@@@@
// .,;i11111111;;iiiiiii:  ;L001,    .:f000Gi:iiiiiiiii;;;C@8888@@@@@@@@8GC8@@@@@@@
//    .,:;iiiii;:;iiiii:  iGG00L:.    .;G000L,;iiii;::::::C8@@8@@@@@@@@0C0@@@@@@@@@
//         ..... .:iiii: .tGGG00L1;,..,iGGGGf.:;iii;,..,. .:f88@@@@@@@CC8@@@@@@@@@@
//                 :iii;.:;tGGGG00GCLffCGGGL:,:iiii;..f0i    L@@@@@@@@8@@@@@88@@@@@
//                  ,:ii;ii;1LGGGGG00GGGGGf;::;ii;;, ;8t.    f@@@@@@@@@@@@@@@8@@@@@
//                    ,;iiiii;1LGGGGGGGGLi,:;:;i1tfi .:      C@@@@@@@@@8GLC8@@@@@@@
//                     .:iiiii;i1fLCCCCt:,:;;:1C08@0i:::::;;i0@@@@@@@0LiiG@@@@@@@@@
//                       ,;iiiiii;iii;:,:;;;;t8@@8@@@88888@@@@@8088G1::f8@@@@@@@@@@
//                        .:iiiiiiii;;;;iii;f8@@@@@@@@@@@@@@@@@@8Gi,,t08G0@@@@@@@@@
//                          ,;iiiiiiiiiiii;i8@80GG@@@@@@@@@@@@@@8t:1G80G88888@@@@@@
//                           .:iiiiiiiiiii;180CC0@@@@@@@@@@@@@@@CL08008@@888@@@@@@@
//                             :iiiiiiiiii;;CG88@@@@@8GG0@@@@@@@@@@@@@888@@@@@@@@@@
//                             ,iiiiiiii;:::0@@8@@8GGG0@@@@@@888@@@@@@@@@@@@@@@@@@@


//                                                            ,1,
//                               ......                     .ifL1
//                            .;1ttttLLf1,                 ;fffff,
//                           :LCLfttfLLLLL:              ,tffffffi
//                          ,CCCLf11ff1tff;            ,1Lffffffff.
//                          iLttt11ifLLLfft.           :iitffffffLi
//                          ;t1111ii1fffff1               ffffft;i1.
//                          ,1iiiiii1tt1it,              ifffff,
//                           ;1i;;;;i1tff:              .fffff1
//                           ,t1ii;:::;i;               ifffff,
//                          .,;11iii;:                 .fffff1
//                     ..,,.,,.,iftitGi...             ifffff,
//                 .,,,::::,,,,..;LLLGt::::,,,.       .fffff1
//                 ;,,,,,,,,,,,,,.,;tfi,::,,::::,     ;fffff,
//                 ::,,,,,,,,..,:,,.:ft,::,,:,,::.   .fffff1
//                 ,i:,,,,,,,,..,,,,,,;,:::,:,,,::   ;fffff,
//                 .1:,,,,,,,,..,,,:,,,::,,,:,,,,:, .fffff1
//                  ;;:,,,,,:, ..,,,::,,,:,,:,,,,::.,1tfff:
//                  ,1;:,,,,;f:.,,,,,,:,..,,:,..,,::,  ..,
//                   ;i::,,,,,:,,:,,:,:::,,,;1:,.,,,:,
//                   .;;;:,,,,,,,,,::::::::::LGC;,,,,:.
//                    .;;:,,.,,,,,,,,,,,,,,:::;;:,,,,,:
//                     ,:::,.....,,,,,,,,,,,:,,,,,,,,,:.
//                     ....,....,,,,,,.....,,,,,,,,,,,,.
//                     ,,..     .,::;:........,,,,,...
//                     :,,......      ............
//                     :,...........   ....:,...,.
//                     ,,............. ...,1;,,,,.
//                                         ..
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
//                       ,iiii;1f:;iiiiiiiiiiii;if;:iiiiiii.
//                      .iiiii:iL..iiiiiiiiiiii;:f: iiiiiiii.
//                      ;iiiiii:.,;iiii;iiiiiiii:..:iiiiiiii:
//                     .i;;;iiiiiiiiii;,,;iiiiiiiiiiii;;iiiii.
//                     ::,,,,:iiiiiiiiiiiiiiiiiiiiii:,,,,:;ii:
//                     ;,,,,,:iiiiiiii;;;;;;;iiiiii;,,,,,,;iii.
//                     ;i;;;;iiiiiiii;:;;;;;:iiiiiii;::::;iiii:
//                     ,iiiiiiiiiiiiii;;;;;;:iiiiiiiiiiiiiiiiii.
//                      .iiiiiiiiiiiiii;;;;;iiiiiiiiiiiiiiiiiii:
//                       .;iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii;
//                        ;iiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiiii.
//                       .;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,
