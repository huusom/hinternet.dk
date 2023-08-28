#r "nuget: FSharp.Data, 6.2.0"
#load "Library.fs"

open Basics


let source = "https://2e.aonprd.com/Monsters.aspx?ID=137"
let dragon = Monster.load source

dragon
|> Monster.getAttack "Jaws"
|> Attack.getDice Success
|> Dice.getAverage

open FSharp.Data

let html = HtmlDocument.Load source



html.CssSelect "#ctl00_RadDrawer1_Content_MainContent_DetailedOutput > h1 > a[href='Monsters.aspx?ID=137']"
|> List.tryHead
|> Option.map (HtmlNode.innerText)
|> Option.defaultValue "Unknown"


html.CssSelect "#ctl00_RadDrawer1_Content_MainContent_DetailedOutput > span[class='hanging-indent']"
|> List.filter (
    HtmlNode.descendants false (HtmlNode.innerText >> (=) "Melee")
    >> Seq.isEmpty
    >> not
)
|> List.map HtmlNode.innerText


open System.Text.RegularExpressions

let (|Rx|_|) regex str =
    let m = Regex(regex).Match(str)

    if m.Success then
        Some(List.tail [ for x in m.Groups -> x.Value ])
    else
        None


type Effect =
    | Damage of Dice * string
    | Other of string

type Attack =
    { Name: string
      Cost: int
      Traits: string list
      Effects: Effect list }


let parseEffect =
    function
    | Rx @"(\d+)d(\d+)\+(\d+) (\w+)" [ a; b; c; d ] -> Damage(Dice.Add(Die(int a, int b), Int(int c)), d)
    | Rx @"(\d+)d(\d+) (\w+)" [a;b;c] -> Damage(Die(int a, int b), c)
    | s -> Other s


let parseAttack =
    function
    | Rx @"^Melee\[(one|two|three)-action\] (\w+) \+(\d+).*\((.*)\), Damage (.+)$"
         [ "one"; name; bonus; traits; damage ] ->
        Some
            { Name = name
              Cost = 1
              Traits = traits.Split(", ") |> List.ofArray
              Effects =
                damage.Split([| "plus"; ","; "and" |], System.StringSplitOptions.TrimEntries)
                |> List.ofArray
                |> List.map parseEffect }
    | _ -> None

[ "Melee[one-action] jaws +29 [+24/+19] (fire, magical, reach 15 feet), Damage 3d12+15 piercing plus 2d6 fire"
  "Melee[one-action] claw +29 [+25/+21] (agile, magical, reach 10 feet), Damage 3d10+15 slashing"
  "Melee[one-action] tail +27 [+22/+17] (magical, reach 20 feet), Damage 3d12+13 slashing"
  "Melee[one-action] wing +27 [+23/+19] (agile, magical, reach 15 feet), Damage 2d10+13 slashing" ]
|> List.choose parseAttack
