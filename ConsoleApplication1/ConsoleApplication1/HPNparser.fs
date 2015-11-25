module HPNparser

// Open required libraries
open System
open System.Collections.Generic
open Operators
open FParsec
open HPNtypes

// FParsec helpers
let ws = spaces
let str s = pstring s .>> ws
let char c = pchar c .>> ws

// Parsing HPN numbers
let h, hRef = createParserForwardedToRef()
let pZero = ws >>. (stringReturn "0" Zero) .>> ws
let pPow = between (char '[') (char ']') h |>> Pow
let pSeqn = sepBy (pPow .>> ws) (char '+') |>> Seqn

do hRef := choice [ pZero; pSeqn]

// Parsing cmds
let cmd, cmdRef = createParserForwardedToRef()
let cmdList = sepBy1 cmd ws
let pCmd n = (tuple2 n (ws >>. str "base" >>. pint32))
let pToHpn = pCmd pint32 |>> ToHPN
let pToDec = pCmd h |>> ToDec

do cmdRef := choice [ pToHpn; pToDec ]

// FParsec parser
let hpn = cmdList .>> eof |>> Prog
let parseHPNString str = run hpn str

// hpn2str calls seq2str
let rec hpn2str (h : HPN) =
    match h with
    | Seqn al -> seq2str al
    | Zero -> "0"
// seq2str calls atom2str
and seq2str (al : Atom list) =
    match al with
    | [] -> ""
    | [a] -> "[ " + (atom2str a) + " ]"
    | a :: t -> "[ " + (atom2str a) + " ]" + " + " + (seq2str t)
and atom2str (a : Atom) =
    match a with
    | Pow h -> hpn2str h

let cmd2parsed (c : Cmd) =
    match c with
    | ToHPN (n,b) -> n.ToString() + " base " + b.ToString()
    | ToDec (h,b) -> (hpn2str h) + " base " + b.ToString()

let rec prog2parsed (p : Prog) =
    match p with
    | Prog (c :: t) -> (cmd2parsed c) + "\n" + (prog2parsed (Prog t))
    | Prog [] -> "\n"

let rec runToDec (h : HPN) (b : float) =
    match h with
    | Zero -> float 0
    | Seqn al ->
    match al with
    | (Pow h :: t) ->  (b ** float (runToDec h b)) + (runToDec (Seqn t) b)
    | [] -> float 0


let logb i b = (log i) / (log b)
let remainder i b = (logb i b) - (floor (logb i b))

let rec runToHPN (i : float) (b : float) =
    match i with
    | i when (int (floor i) = 0) -> [Pow Zero]
    | i -> 
    match ((logb i b), floor (remainder i b)) with
    | (l,r) when (int (floor l) >= 1) && (int (floor r) <= 0) -> [Pow(Seqn(runToHPN (floor l) b))]
    | (l,r) when (int (floor l) >= 1) && (int (floor r) > 0) -> (Pow(Seqn(runToHPN (floor l) b)) :: (runToHPN (floor r) b))
    | (l,r) when (int (floor l) < 1) -> (Pow Zero) :: (runToHPN (floor(i-(float 1))) b)
    | _ -> []


let runCmd (c : Cmd) =
    match c with
    | ToHPN (i,b) -> seq2str (runToHPN (float i) (float b)) + " base " + b.ToString()
    | ToDec (h,b) -> (runToDec h (float b)).ToString() + " base 10"

let rec runProg (p : Prog) =
    match p with
    | Prog (c :: t) -> (runCmd c) + "\n" + (runProg (Prog t))
    | Prog [] -> "\n"
