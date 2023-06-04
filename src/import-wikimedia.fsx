#r "System.Xml.Linq"

open System.IO
open System.Xml.Linq
open System.Text.RegularExpressions


let resolve_path path =
    Path.Combine(__SOURCE_DIRECTORY__, path)
    |> Path.GetFullPath

let source, name =
    match fsi.CommandLineArgs |> List.ofArray with
    | [ _; _; s; n ] -> (resolve_path s), n
    | [ _; _; s ] -> (resolve_path s), (Path.GetFileNameWithoutExtension s)
    | _ -> @"C:\code\repos\my\hinternet.dk\temp\Burning Sky.xml", "Burning Sky"

let (|Regex|_|) (rx: Regex) str =
    let m = rx.Match(str)

    if m.Success then
        Some(List.tail [ for x in m.Groups -> x.Value ])
    else
        None

let descendants name (x: XElement) =
    x.Descendants(XName.Get(name, x.Name.NamespaceName))

let element name (x: XElement) =
    x.Element(XName.Get(name, x.Name.NamespaceName))

let elements names x =
    List.fold (fun x n -> element n x) x names

let value (x: XElement) = x.Value

let heading text =
    let rx = Regex @"(=+)\s*([\w\s\-\d/!]+)\s*\k<1>"

    let eval (m: Match) =
        String.concat
            " "
            [ System.String('#', m.Groups.Item(1).Value.Length)
              m.Groups.Item(2).Value ]

    rx.Replace(text, eval)

type Gauge =
    { xp: int
      current: string
      next: string }

type Frontmatter =
    { Timestamp: string
      Categories: string list
      Gauge: Gauge option
      Previous: string
      Next: string }

type Page =
    { Frontmatter: Frontmatter
      Title: string
      Body: string }

let frontmatter (x: XElement) =
    { Timestamp =
        x
        |> elements [ "revision"; "timestamp" ]
        |> value
        |> System.Convert.ToDateTime
        |> fun dt -> dt.ToString("yyyy-MM-dd")
      Categories = []
      Previous = ""
      Next = ""
      Gauge = None }

let categorize (p: Page) =
    let rx = Regex @"(=*) *\[{2}:?Category:([\w\s]+).*\]{2} *\k<1>"

    match p.Body with
    | Regex rx categories ->
        { p with
            Body = rx.Replace(p.Body, "").Trim()
            Frontmatter = { p.Frontmatter with Categories = categories |> List.tail } }
    | _ -> p

let navigation (p: Page) =
    let rx = Regex @"\{\{Footer\|previous=(.*)\|next=(.*)\|.*\}\}"

    match p.Body with
    | Regex rx [ prev; next ] ->
        { p with
            Body = rx.Replace(p.Body, "")
            Frontmatter =
                { p.Frontmatter with
                    Previous = prev
                    Next = next } }
    | _ -> p

let gauge (p: Page) =
    let rx = Regex @"\{\{Gauge\|(.*)\|(.*)\|(.*)\}\}"

    match p.Body with
    | Regex rx [ xp; c; n ] ->
        { p with
            Body = rx.Replace(p.Body, "")
            Frontmatter = { p.Frontmatter with Gauge = Some { xp = (int) xp; current = c; next = n } } }
    | _ -> p

let title_case =
    System
        .Globalization
        .CultureInfo(
            "en-us",
            false
        )
        .TextInfo
        .ToTitleCase

let page (x: XElement) =
    { Frontmatter = frontmatter x
      Title = x |> element "title" |> value |> title_case
      Body =
        x
        |> elements [ "revision"; "text" ]
        |> value
        |> heading }
    |> categorize
    |> navigation
    |> gauge

let format p =
    seq {
        yield
            name :: p.Frontmatter.Categories
            |> List.map (fun s -> "#" + s.Replace(" ", "-").ToLowerInvariant())
            |> List.distinct
            |> String.concat " "

        yield ""
        yield! p.Body.Trim().Split('\n')

        if (p.Frontmatter.Next <> ""
            || p.Frontmatter.Previous <> "") then
            yield ""
            yield "| previous | next |"
            yield "| --- | --- |"

            let next =
                sprintf "[%s](./%s.md)" p.Frontmatter.Next (title_case p.Frontmatter.Next)

            let prev =
                sprintf "[%s](./%s.md)" p.Frontmatter.Previous (title_case p.Frontmatter.Previous)

            if (p.Frontmatter.Previous = "") then
                yield sprintf "| | %s |" next
            elif (p.Frontmatter.Next = "") then
                yield sprintf "| %s | |" prev
            else
                yield sprintf "| %s | %s |" prev next
    }

let write p =
    let path = resolve_path (sprintf "..\Wikilog\Campaigns\%s\%s.md" name p.Title)
    let dir = Path.GetDirectoryName path

    if not (Directory.Exists dir) then
        Directory.CreateDirectory(dir) |> ignore

    File.WriteAllLines(path, format p)
    printfn "Wrote %s" path

let pages =
    XElement.Load(source)
    |> descendants "page"
    |> Seq.filter (element "ns" >> value >> (=) "0")
    |> Seq.map page

let titles = pages |> Seq.map (fun p -> p.Title) |> Set.ofSeq

let update (p: Page) =
    let rx = Regex @"\[\[([^]]+)]]"

    let replace (m: Match) =
        let t = title_case m.Groups[1].Value
        let almost (s: string) = s.StartsWith t

        if Seq.contains t titles then
            sprintf "[%s](./%s.md)" t (t.Replace(" ", "%20"))
        elif Seq.exists almost titles then
            let candidates =
                Seq.filter almost titles
                |> Seq.filter (fun s -> s.Length > t.Length && s.[t.Length] = ' ')

            match Seq.tryHead candidates with
            | Some h -> sprintf "[%s](./%s.md)" t (h.Replace(" ", "%20"))
            | _ -> m.Value

        else
            m.Value

    { p with Body = rx.Replace(p.Body, replace) }

pages |> Seq.map update |> Seq.iter write



let x =
    pages
    |> Seq.find (fun x -> x.Title = "2019-05-09")

update x
