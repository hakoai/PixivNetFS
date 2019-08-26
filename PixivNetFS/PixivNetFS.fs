namespace PixivNetFS

[<AutoOpen>]
module PixivType =
    open FSharp.Data

    type LoginResProvider = JsonProvider<JsonData.LoginRes>

    type LoginRes = LoginResProvider.Root

    type AccessToken = AccessToken of string

    let getAccessToken (res : LoginRes) = AccessToken res.Response.AccessToken

    type RefreshToken = RefreshToken of string

    let getRefreshToken (res : LoginRes) =
        RefreshToken res.Response.RefreshToken

    type IllustResProvider = JsonProvider<JsonData.Illust, SampleIsList=true>

    type IllustRes = IllustResProvider.Root

    type Illust = IllustResProvider.Illust

    type Illusts = Illust []

    type NextUrl = NextUrl of string

    type RequestParam = RequestParam of string * (string * string) list

    type Restrict =
        | Public
        | Private

    let mkRestrictStr =
        function
        | Public -> "restrict", "public"
        | Private -> "restrict", "private"

    [<Literal>]
    let Host = "https://app-api.pixiv.net"

    let inline mkIllustFollowParam restrict =
        RequestParam(Host + "/v2/illust/follow", [ mkRestrictStr restrict ])

    let inline mkUserBookmarksIllustParam userid restrict =
        RequestParam(Host + "/v1/user/bookmarks/illust",
                     [ "user_id", userid
                       mkRestrictStr restrict ])

module PixivBase =
    open FSharp.Data
    open FSharp.Data.HttpRequestHeaders

    [<Literal>]
    let private ClientId = "MOBrBDS8blbauoSck0ZfDbtuzpyT"

    [<Literal>]
    let private ClientSecret = "lsACyCD94FhDUtGTXi3QzcFE2uU1hqtDaKeqrdwj"

    let baseHeader =
        [ "accept", "'application/json, text/plain, */*'"
          "user-agent", "axios/0.19.0" ]

    type GetFunction = string * (string * string) list * (string * string) list -> Async<string>

    type PostFunction = string * (string * string) list * (string * string) list -> Async<string>

    type RequestFunctions = GetFunction * PostFunction

    let loginPassword (requestPost : PostFunction) username password : Async<LoginRes> =
        async {
            let! res = requestPost ("https://oauth.secure.pixiv.net/auth/token",
                                    baseHeader,
                                    [ "client_id", ClientId
                                      "client_secret", ClientSecret
                                      "get_secure_url", "1"
                                      "grant_type", "password"
                                      "username", username
                                      "password", password ])
            return LoginResProvider.Parse(res)
        }

    let loginRefreshToken (requestPost : PostFunction) (RefreshToken token) : Async<LoginRes> =
        async {
            let! res = requestPost ("https://oauth.secure.pixiv.net/auth/token",
                                    baseHeader,
                                    [ "client_id", ClientId
                                      "client_secret", ClientSecret
                                      "get_secure_url", "1"
                                      "grant_type", "refresh_token"
                                      "refresh_token ", token ])
            return LoginResProvider.Parse(res)
        }

    let requestGet (requestGet : GetFunction) atoken url param =
        let request url param (AccessToken accesstoken) =
            let header =
                List.concat
                    [ baseHeader

                      [ "App-OS", "ios"
                        "App-OS-Version", "9.3.3"
                        "App-Version", "6.0.9"

                        Authorization
                        <| System.String.Format("Bearer {0}", accesstoken) ] ]
            requestGet (url, header, param)
        async { let! res = request url param atoken
                return res }

    let requestPost (requestPost : PostFunction) atoken url param =
        let request url param (AccessToken accesstoken) =
            let header =
                List.concat
                    [ baseHeader

                      [ "App-OS", "ios"
                        "App-OS-Version", "9.3.3"
                        "App-Version", "6.0.9"

                        Authorization
                        <| System.String.Format("Bearer {0}", accesstoken) ] ]
            requestPost (url, header, param)
        async { let! res = request url param atoken
                return res }

    let inline getNextUrl (res : IllustRes) = Option.map NextUrl res.NextUrl

    let requestIllusts request token (RequestParam(url, param)) : Async<Illusts * NextUrl option> =
        let filter = "filter", "for_ios"
        let withFilter param = filter :: param
        async {
            let! res = requestGet request token url (withFilter param)
            let parsed = IllustResProvider.Parse(res)
            return parsed.Illusts, getNextUrl parsed
        }

    let inline getNextIllusts request token (NextUrl nexturl) =
        requestIllusts request token <| RequestParam(nexturl, [])

    let httpRequest : RequestFunctions =
        let getFunc (url, header, param) =
            Http.AsyncRequestString
                (url, headers = header, query = param, httpMethod = "Get")
        let postFunc (url, header, param) =
            Http.AsyncRequestString
                (url, headers = header, body = FormValues param)
        (getFunc, postFunc)

module PixivF =
    open FSharp.Data
    open FSharpPlus
    open FSharpPlus.Data
    open FSharp.Control

    let inline mkLoginFunction login ((_, reqs) : PixivBase.RequestFunctions)
               param = login reqs param

    let inline loginPassword' reqs id password =
        let inline f r (i, p) = PixivBase.loginPassword r i p
        mkLoginFunction f reqs (id, password)

    let inline loginToken' reqs token =
        mkLoginFunction PixivBase.loginRefreshToken reqs token
    let loginPassword = loginPassword' PixivBase.httpRequest
    let loginToken = loginToken' PixivBase.httpRequest

    let getIllustImpl reqestFunc
        ((requestGet, _) as request : PixivBase.RequestFunctions)
        ((atoken, rtoken) as tokens) param =
        async {
            try
                let! res = reqestFunc requestGet atoken param
                return res, tokens
            with _ ->
                let! res = loginToken' request rtoken
                let atoken = getAccessToken res
                let rtoken = getRefreshToken res
                let! res = reqestFunc requestGet atoken param
                return (res, (atoken, rtoken))
        }

    let getIllustFirst' = getIllustImpl PixivBase.requestIllusts
    let getIllustNext' = getIllustImpl PixivBase.getNextIllusts
    let inline getIllustFirstM' request param =
        StateT <| fun t -> getIllustFirst' request t param
    let inline getIllustNextM' request param =
        StateT <| fun t -> getIllustNext' request t param
    let getIllustFirst = getIllustFirst' PixivBase.httpRequest
    let getIllustNext = getIllustNext' PixivBase.httpRequest
    let getIllustFirstM = getIllustFirstM' PixivBase.httpRequest
    let getIllustNextM = getIllustNextM' PixivBase.httpRequest
