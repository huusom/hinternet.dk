open System.Net.Http
open System.Net

let bot_name = "Huusom@migration-bot"
let bot_pwd = "fincpf00b36m588olsfitqdtaigoosnb"

let cookies = CookieContainer()

let handler =
    new HttpClientHandler(AllowAutoRedirect = true, UseCookies = true, CookieContainer = cookies)

let client = new HttpClient(handler)

let invoke request =
    task {
        let! response = client.SendAsync(request)
        return! response.Content.ReadAsStringAsync()
    }
    |> Async.AwaitTask
    |> Async.RunSynchronously

let query args =
    let uri =
        args

        |> String.concat "&"
        |> sprintf "https://hinternet.dk/wikilog/api.php?action=query&formatversion=2&format=json&%s"
        |> System.Uri



    new HttpRequestMessage(HttpMethod.Get, uri)
    |> invoke

query [ "meta=tokens"; "type=login" ]

query [ "lgtoken=aa35d715536beda76a6a7ff7e71cafdd64d731bf"
        sprintf "lgtoken=%s" bot_name
        sprintf "lgpassword=%s" bot_pwd
]
