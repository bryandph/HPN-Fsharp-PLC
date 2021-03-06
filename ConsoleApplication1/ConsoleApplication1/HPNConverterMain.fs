﻿// This is the main program which, when compiled, will convert between HPN types and Decimal types.
open FParsec
open HPNparser
open System.IO

[<EntryPoint>]
let main(argv: string[]) = 
    if argv.Length <> 1 then
            System.Console.WriteLine("usage: hpnconverter.exe <file>")
            exit 1

    let fileName = argv.[0]
    let result = 
        if File.Exists(fileName) then parseHPNFile fileName System.Text.Encoding.Default
        else invalidArg "File Name" "File not found"

    let myProg = 
        match result with
        | Success (v, _, _) -> System.Console.WriteLine(runProg v)
        | Failure (msg, e, _) -> System.Console.WriteLine(msg + "\n" + e.ToString())

    myProg

    System.Console.WriteLine("Press Any Key To Exit...")
    System.Console.ReadKey() |> ignore
    0


