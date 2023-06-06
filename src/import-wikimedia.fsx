#r "System.Xml.Linq"

open System.IO
open System.Xml.Linq
open System.Text.RegularExpressions

let resolve_path path =
    Path.Combine(__SOURCE_DIRECTORY__, path)
    |> Path.GetFullPath

let source, name =
    match fsi.CommandLineArgs |> List.ofArray with
    | [ _; s; n ] -> (resolve_path s), n
    | [ _; s ] -> (resolve_path s), (Path.GetFileNameWithoutExtension s)
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

let value (x: XElement) = x.Value

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

let replace_headings p =
    let rx = Regex @"(=+)\s*([^=|]+)\s*\k<1>"

    let eval (m: Match) =
        String.concat
            " "
            [ System.String('#', m.Groups.Item(1).Value.Length)
              m.Groups.Item(2).Value
              System.Environment.NewLine ]

    { p with Body = rx.Replace(p.Body, eval) }

let replace_categories (p: Page) =
    let rx = Regex @"(=*) *\[{2}:?Category:([\w\s]+).*\]{2} *\k<1>"

    match p.Body with
    | Regex rx categories ->
        { p with
            Body = rx.Replace(p.Body, "").Trim()
            Frontmatter = { p.Frontmatter with Categories = categories |> List.tail } }
    | _ -> p

let replace_navigation (p: Page) =
    let rx_1 = Regex @"\{\{Footer\|previous=(.*)\|next=(.*)\|.*\}\}"

    let rx_2 =
        Regex(@"{\|(?:.+)\[\[([^|]+)(?:.+)\[\[([^|]+)(?:.+)\|}", RegexOptions.Singleline)

    match p.Body with
    | Regex rx_1 [ prev; next ] ->
        { p with
            Body = rx_1.Replace(p.Body, "")
            Frontmatter =
                { p.Frontmatter with
                    Previous = prev
                    Next = next } }
    | Regex rx_2 [ prev; next ] ->
        { p with
            Body = rx_2.Replace(p.Body, "")
            Frontmatter =
                { p.Frontmatter with
                    Previous = prev
                    Next = next } }
    | _ -> p

let replace_xp (p: Page) =
    let rx = Regex @"\{\{Gauge\|(.*)\|(.*)\|(.*)\}\}"

    match p.Body with
    | Regex rx [ xp; c; n ] ->
        { p with
            Body = rx.Replace(p.Body, "")
            Frontmatter = { p.Frontmatter with Gauge = Some { xp = (int) xp; current = c; next = n } } }
    | _ -> p

let format_title =
    System
        .Globalization
        .CultureInfo(
            "en-us",
            false
        )
        .TextInfo
        .ToTitleCase
    >> fun s -> s.Replace(':', '_')

let format_link n (t: string) =
    sprintf "[%s](%s.md)" n (t.Replace(" ", "%20"))

let replace_links pages =
    let titles = pages |> Seq.map (fun p -> p.Title) |> Set.ofSeq

    let update (p: Page) =
        let rx = Regex @"\[\[([^]]+)]]"

        let replace (m: Match) =
            let t = format_title m.Groups[1].Value
            let almost (s: string) = s.StartsWith t

            if Seq.contains t titles then
                format_link t t
            elif Seq.exists almost titles then
                let candidates =
                    Seq.filter almost titles
                    |> Seq.filter (fun s -> s.Length > t.Length && s.[t.Length] = ' ')

                match Seq.tryHead candidates with
                | Some h -> format_link t h
                | _ -> m.Value

            else
                m.Value

        { p with Body = rx.Replace(p.Body, replace) }

    pages |> Seq.map update

let to_frontmatter (x: XElement) =
    { Timestamp =
        x
        |> element "revision"
        |> element "timestamp"
        |> value
        |> System.Convert.ToDateTime
        |> fun dt -> dt.ToString("yyyy-MM-dd")
      Categories = []
      Previous = ""
      Next = ""
      Gauge = None }

let to_page (x: XElement) =
    { Frontmatter = to_frontmatter x
      Title = x |> element "title" |> value |> format_title
      Body = x |> element "revision" |> element "text" |> value }
    |> replace_headings
    |> replace_categories
    |> replace_navigation
    |> replace_xp

let format_page p =
    let rx = Regex(@"(\r?\n){2,}", RegexOptions.Multiline)
    let newlines = System.Environment.NewLine

    seq {
        yield
            name :: "log" :: p.Frontmatter.Categories
            |> List.map (fun s -> "#" + s.Replace(" ", "-").ToLowerInvariant())
            |> List.distinct
            |> String.concat " "

        if (not (System.String.IsNullOrEmpty(p.Frontmatter.Previous))
            || not (System.String.IsNullOrEmpty(p.Frontmatter.Next))) then

            yield ""
            yield "```ad-info"
            yield ""

            if (p.Frontmatter.Previous <> "") then
                yield sprintf "* previous page: %s" (format_link p.Frontmatter.Previous p.Frontmatter.Previous)

            match p.Frontmatter.Gauge with
            | Some g ->
                yield sprintf "* current level: %s" g.current
                yield sprintf "* xp: %i %%" g.xp
                yield sprintf "* next level: %s" g.next
            | _ -> ()

            if (p.Frontmatter.Next <> "") then
                yield sprintf "* next page:  %s " (format_link p.Frontmatter.Next p.Frontmatter.Next)

            yield "```"

        yield ""
        yield! rx.Replace(p.Body, newlines).Trim().Split('\n') //p.Body.Trim().Split('\n')
    }

let write p =
    let path = resolve_path (sprintf "..\Wikilog\Campaigns\%s\%s.md" name p.Title)
    let dir = Path.GetDirectoryName path

    if not (Directory.Exists dir) then
        Directory.CreateDirectory(dir) |> ignore

    File.WriteAllLines(path, format_page p)
    printfn "Wrote %s" path

let pages =
    XElement.Load(source)
    |> descendants "page"
    |> Seq.filter (element "ns" >> value >> (=) "0")
    |> Seq.map to_page
    |> replace_links

pages |> Seq.iter write
