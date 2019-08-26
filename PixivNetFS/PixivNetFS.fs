namespace PixivNetFS

[<AutoOpen>]
module PixivType =
    open FSharp.Data

    [<Literal>]
    let LoginResSample = """
{
  "response": {
    "access_token": "token",
    "expires_in": 3600,
    "token_type": "bearer",
    "scope": "",
    "refresh_token": "token",
    "user": {
      "profile_image_urls": {
        "px_16x16": "https:\/\/s.pximg.net\/common\/images\/no_profile_ss.png",
        "px_50x50": "https:\/\/s.pximg.net\/common\/images\/no_profile_s.png",
        "px_170x170": "https:\/\/s.pximg.net\/common\/images\/no_profile.png"
      },
      "id": "12234",
      "name": "user",
      "account": "ac",
      "mail_address": "user@gmail.com",
      "is_premium": false,
      "x_restrict": 1,
      "is_mail_authorized": true
    },
    "device_token": "token"
  }
}
    """

    type LoginResProvider = JsonProvider<LoginResSample>

    type LoginRes = LoginResProvider.Root

    type AccessToken = AccessToken of string

    let getAccessToken (res : LoginRes) = AccessToken res.Response.AccessToken

    type RefreshToken = RefreshToken of string

    let getRefreshToken (res : LoginRes) =
        RefreshToken res.Response.RefreshToken

    type IllustResProvider = JsonProvider<"illust.json", SampleIsList=true>

    type IllustRes = IllustResProvider.Root

    type Illust = IllustResProvider.Illust

    type Illusts = Illust []

    type NextUrl = NextUrl of string

    type Restrict =
        | Public
        | Private

    let mkRestrictStr =
        function
        | Public -> "restrict", "public"
        | Private -> "restrict", "private"

    let host = "https://app-api.pixiv.net"
    let mkIllustFollowParam restrict =
        host + "/v2/illust/follow", [ mkRestrictStr restrict ]

    let mkUserBookmarksIllustParam userid restrict =
        host + "/v1/user/bookmarks/illust",
        [ "user_id", userid
          mkRestrictStr restrict ]

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

    let loginPassword requestPost username password : Async<LoginRes> =
        async {
            let! res = requestPost ("https://oauth.secure.pixiv.net/auth/token",
                                    baseHeader,
                                    FormValues [ "client_id", ClientId
                                                 "client_secret", ClientSecret
                                                 "get_secure_url", "1"
                                                 "grant_type", "password"
                                                 "username", username
                                                 "password", password ])
            return LoginResProvider.Parse(res)
        }

    let loginRefreshToken requestPost (RefreshToken token) : Async<LoginRes> =
        async {
            let! res = requestPost ("https://oauth.secure.pixiv.net/auth/token",
                                    baseHeader,
                                    FormValues [ "client_id", ClientId
                                                 "client_secret", ClientSecret
                                                 "get_secure_url", "1"
                                                 "grant_type", "refresh_token"
                                                 "refresh_token ", token ])
            return LoginResProvider.Parse(res)
        }

    let requestGet requestGet atoken (url : string) param =
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

    let requestPost requestPost atoken url param =
        let request url param (AccessToken accesstoken) =
            let header =
                List.concat
                    [ baseHeader

                      [ "App-OS", "ios"
                        "App-OS-Version", "9.3.3"
                        "App-Version", "6.0.9"

                        Authorization
                        <| System.String.Format("Bearer {0}", accesstoken) ] ]
            requestPost (url, header, FormValues param)
        async { let! res = request url param atoken
                return res }

    let getNextUrl (res : IllustRes) = Option.map NextUrl res.NextUrl

    let requestIllusts request token (url, param) : Async<Illusts * NextUrl option> =
        let filter = "filter", "for_ios"
        let withFilter param = filter :: param
        async {
            let! res = requestGet request token url (withFilter param)
            let parsed = IllustResProvider.Parse(res)
            return parsed.Illusts, getNextUrl parsed
        }

    let getNextIllusts request token (NextUrl nexturl) =
        requestIllusts request token (nexturl, [])

    type RequestFunctions = (string * (string * string) list * (string * string) list -> Async<string>) * (string * (string * string) list * HttpRequestBody -> Async<string>)

    let httpRequest =
        let getFunc (url, header, param) =
            Http.AsyncRequestString
                (url, headers = header, query = param, httpMethod = "Get")
        let postFunc (url, header, param) =
            Http.AsyncRequestString(url, headers = header, body = param)
        (getFunc, postFunc)

module PixivF =
    open FSharp.Data
    open FSharpPlus
    open FSharpPlus.Data
    open FSharp.Control

    let getIllustSeq ((requestGet, _) as request) token cb param =
        let getIllustSeqNext ((reqs, _)) token cb next =
            let getIllustFunc = PixivBase.getNextIllusts reqs token
            AsyncSeq.unfoldAsync (function
                | Some next ->
                    async {
                        let! res, n = getIllustFunc next
                        do! cb res
                        return Some(res, n)
                    }
                | None -> async { return None }) next
        async {
            return asyncSeq {
                       let! res, next = PixivBase.requestIllusts requestGet token
                                            param
                       yield res
                       yield! getIllustSeqNext request token cb next
                   }
                   |> AsyncSeq.concatSeq
                   |> AsyncSeq.toBlockingSeq
        }

    type PixivM<'T> = ReaderT<AccessToken, Async<'T>>

    let requsetIllustM' reqs cb param : PixivM<seq<Illust>> =
        ReaderT <| fun t -> getIllustSeq reqs t cb param
    
    let mkLoginFunction login ((_, reqs)) param = login reqs param

    let loginPassword' reqs id password =
        let f r (i, p) = PixivBase.loginPassword r i p
        mkLoginFunction f reqs (id, password)

    let loginToken' reqs token =
        mkLoginFunction PixivBase.loginRefreshToken reqs token
    let requsetIllustM = requsetIllustM' PixivBase.httpRequest
    let loginPassword = loginPassword' PixivBase.httpRequest
    let loginToken = loginToken' PixivBase.httpRequest

