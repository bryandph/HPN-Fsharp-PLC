module HPNtypes

open System.Text

[<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
type Atom = 
    | Pow of HPN
    member private t.StructuredFormatDisplay = 
        match t with
        | Pow h -> box ("[" + (sprintf "%A" h) + "]")
and HPN = 
    | Seqn of Atom list
    | Zero
    member private t.StructuredFormatDisplay = 
        match t with
        | Seqn l -> box ()
        | Zero -> box "0"

[<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
type Cmd = 
    | ToHPN of int * int
    | ToDec of HPN * int
    member private t.StructuredFormatDisplay = 
        match t with
        | ToHPN (n,b) -> box (sprintf "%A" n + " base " + sprintf "%A" b)
        | ToDec (n,b) -> box (sprintf "%A" n + " base " + sprintf "%A" b)

[<StructuredFormatDisplay("{StructuredFormatDisplay}")>]
type Prog =
    | Prog of Cmd list
    member private t.StructuredFormatDisplay = 
        match t with
        | Prog l -> box (List.map (fun x -> sprintf "%A" x) l)