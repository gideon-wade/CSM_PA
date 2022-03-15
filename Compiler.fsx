module Compiler

open System

type Node = string;;

type Edge = Node * cmd * Node;; 

let mutable num = 1;;

let rec isDone (GC : gCmd) = 
    match GC with
        | Statement(b, _)   -> Not(b)
        | GCmds(gc1, gc2)   -> And(isDone gc1, isDone gc2);;

let rec edges (startNode:Node) (endNode:Node) (edge:cmd) =
    match edge with
        | Skip                   -> [Edge(startNode, edge, endNode)]
        | Assign(x, a)           -> [Edge(startNode, edge, endNode)]
        | AssignArray(x, a1, a2) -> [Edge(startNode, edge, endNode)]
        | Cmds(c1, c2)           -> let q = sprintf "q%i" num
                                    num <- num + 1
                                    let e1 = edges startNode q c1
                                    let e2 = edges q endNode c2
                                    e1 @ e2
        | If(gCmd)               -> edgesGC startNode endNode gCmd
        | Do(gCmd)               -> let b = isDone gCmd
                                    let e = edgesGC startNode startNode gCmd
                                    [Edge(startNode, BoolCmd(b), endNode)] @ e  
        | _                      -> failwith "Nope"
and edgesGC (startNode:Node) (endNode:Node) (edge:gCmd) =
    match edge with
        | Statement(b, cmd)      -> let q = sprintf "q%i" num
                                    num <- num + 1;
                                    let e = edges q endNode cmd
                                    [Edge(startNode, BoolCmd(b), q)] @ e
        | GCmds(gc1, gc2)         -> let e1 = edgesGC startNode endNode gc1
                                     let e2 = edgesGC startNode endNode gc2 
                                     e1 @ e2;;

let rec edgesPrint (graph : Edge list) =
   match graph with
        | []                    -> ""
        | (q0, edge, q1)::tail  -> q0 + " -> " + q1 + " [label = \"" + (graphPrinter edge)+"\"];\n" + (edgesPrint tail)
        //| Edge(q0, Skip, q2)::tail         -> q0 + " -> " + q2 + " [label = skip];"
        //| Cmds(c1, c2)::tail               -> startNode + " -> " + "new [label = \""+ (graphPrinter c1) + "\"];\n" + (edgesPrint "new" endNode c2);;

let makeEdge (graph : Edge list)  =
     "digraph program_graph {rankdir=LR;\n" 
     + "node [shape = circle]; q▷;\n" 
     + "node [shape = doublecircle]; q◀;\n" 
     + "node [shape = circle]\n" 
     + (edgesPrint graph)
     + "}";;
     
let rec removeQ (q : char list) =
    match q with
        | c::tail when c = 'q' -> removeQ tail
        | c::tail when c = '▷' -> "0"
        | _                    -> q |> String.Concat;;
                   
let rec removeAll (q : char list) =
    match q with
        | []                    -> ""
        | c::tail when c <> '◀' -> removeQ tail
        | c::tail when c = '◀'  -> "◀";;     

let rec bubble (graph : Edge list) =
    match graph with
        | []                                 -> []
        | _::[]                              -> graph
        | (q1, e1, q12)::(q2, e2, q22)::tail -> let n1 = (removeQ (q1.ToCharArray() |> List.ofArray))|> int
                                                let n2 = (removeQ (q2.ToCharArray() |> List.ofArray))|> int
                                                if n1 <= n2 then
                                                    (q1, e1, q12)::(bubble ((q2, e2, q22)::tail))
                                                else
                                                    (q2, e2, q22)::(bubble ((q1, e1, q12)::tail));;                                          

let rec bubbleSort (graph : Edge list) (num : int) =
    match num with
        | 0 -> graph
        | _ -> bubbleSort (bubble graph) (num-1);;

let rec bubbleDown (graph : Edge list) =
    match graph with
        | []                                 -> []
        | _::[]                              -> graph
        | (q1, e1, q12)::(q2, e2, q22)::tail -> let n1 = (removeAll (q12.ToCharArray() |> List.ofArray))
                                                let n2 = (removeAll (q22.ToCharArray() |> List.ofArray))
                                                if n1 = "◀" then
                                                    (q2, e2, q22)::tail @ [(q1, e1, q12)]
                                                else
                                                    (q1, e1, q12)::(bubbleDown ((q2, e2, q22)::tail));;
