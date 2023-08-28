module Basics

open System.Text.RegularExpressions

let (|Rx|_|) regex str =
    let m = Regex(regex).Match(str)

    if m.Success then
        Some(List.tail [ for x in m.Groups -> x.Value ])
    else
        None


type Dice =
    | Int of int
    | Die of int * int
    | Mult of Dice * Dice
    | Add of Dice * Dice
    | Sub of Dice * Dice
    | Div of Dice * Dice
    static member (+)(l, r) = Add(l, r)
    static member (+)(l, r) = Add(l, Int r)


type Effect =
    | Damage of Dice * string
    | Other of string

type Check = 
    | Melee of int 
    

type Attack =
    { Name: string
      Cost: int
      Check : Check
      Traits: string list
      Effects: Effect list }

type Monster =
    { Source: string
      Name: string
      Attacks: Attack list }

module Dice =

    let rec getAverage =
        function
        | Int i -> float i
        | Die (n, s) -> (float n) * (0.5 + (float s) / 2.0)
        | Mult (l, r) -> (getAverage l) * (getAverage r)
        | Add (l, r) -> (getAverage l) + (getAverage r)
        | Sub (l, r) -> (getAverage l) - (getAverage r)
        | Div (l, r) -> (getAverage l) / (getAverage r)

module Parser =
    let effect =
        function
        | Rx @"(\d+)d(\d+)\+(\d+) (\w+)" [ a; b; c; d ] -> Damage((Die(int a, int b)) + (int c), d)
        | Rx @"(\d+)d(\d+) (\w+)" [ a; b; c ] -> Damage(Die(int a, int b), c)
        | s -> Other s

    let attack =
        function
        | Rx @"^Melee\[(one|two|three)-action\] (\w+) \+(\d+).*\((.*)\), Damage (.+)$"
             [ "one"; name; bonus; traits; damage ] ->
            Some
                { Name = name
                  Cost = 1
                  Check = Melee (int bonus)
                  Traits = traits.Split(", ") |> List.ofArray
                  Effects =
                    damage.Split([| "plus"; ","; "and" |], System.StringSplitOptions.TrimEntries)
                    |> List.ofArray
                    |> List.map effect }
        | _ -> None


module Monster =
    open FSharp.Data

    let load (source: string) =
        let html = HtmlDocument.Load source


        { Source = source
          Name =
            html.CssSelect "#ctl00_RadDrawer1_Content_MainContent_DetailedOutput > h1 > a[href='Monsters.aspx?ID=137']"
            |> List.tryHead
            |> Option.map (HtmlNode.innerText)
            |> Option.defaultValue "Unknown"


          Attacks =
              html.CssSelect "#ctl00_RadDrawer1_Content_MainContent_DetailedOutput > span[class='hanging-indent']"
              |> List.filter (
                  HtmlNode.descendants false (HtmlNode.innerText >> (=) "Melee")
                  >> Seq.isEmpty
                  >> not
              )
              |> List.map HtmlNode.innerText
              |> List.choose Parser.attack

        }

    let getAttack name monster =
        monster.Attacks
        |> List.tryFind (fun a -> a.Name = name)
