// This is the main program which, when compiled, will convert between HPN types and Decimal types.
open FParsec
open HPNparser

[<EntryPoint>]
let main(argv: string[]) = 
    if argv.Length <> 1 then
            printf "usage: hpnconverter.exe <file>\n"
            exit 1

    let fileName = argv.[0]
    let result = runParserOnFile hpn () fileName System.Text.Encoding.Default

    let myProg = 
        match result with
        | Success (v, _, _) -> System.Console.WriteLine(runProg v)
        | Failure (msg, e, _) -> System.Console.WriteLine(msg + "\n" + e.ToString())

    myProg
    0


