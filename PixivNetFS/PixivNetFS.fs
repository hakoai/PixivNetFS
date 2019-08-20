namespace PixivNetFS

module PixivBase =
    open FSharp.Data
    open FSharp.Data.HttpRequestHeaders

    [<Literal>]
    let private ClientId = "MOBrBDS8blbauoSck0ZfDbtuzpyT"

    [<Literal>]
    let private ClientSecret = "lsACyCD94FhDUtGTXi3QzcFE2uU1hqtDaKeqrdwj"

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

    let baseHeader =
        [ "accept", "'application/json, text/plain, */*'"
          "user-agent", "axios/0.19.0" ]

    type LoginResProvider = JsonProvider<LoginResSample>

    type LoginRes = LoginResProvider.Root

    let loginPassword username password reqFunc : Async<LoginRes> =
        async {
            let! res = reqFunc ("https://oauth.secure.pixiv.net/auth/token",
                                baseHeader,
                                FormValues [ "client_id", ClientId
                                             "client_secret", ClientSecret
                                             "get_secure_url", "1"
                                             "grant_type", "password"
                                             "username", username
                                             "password", password ])
            return LoginResProvider.Parse(res)
        }

    type AccessToken = AccessToken of string

    let getAccessToken (res : LoginRes) = AccessToken res.Response.AccessToken

    type RefreshToken = RefreshToken of string

    let getRefreshToken (res : LoginRes) =
        RefreshToken res.Response.RefreshToken

    let loginRefreshToken (RefreshToken token) reqFunc =
        async {
            let! res = reqFunc ("https://oauth.secure.pixiv.net/auth/token",
                                baseHeader,
                                FormValues [ "client_id", ClientId
                                             "client_secret", ClientSecret
                                             "get_secure_url", "1"
                                             "grant_type", "refresh_token"
                                             "refresh_token ", token ])
            return LoginResProvider.Parse(res)
        }

    type Tokens = RefreshToken * AccessToken

    type Reqs = (string * (string * string) list * (string * string) list * string -> Async<string>) * (string * (string * string) list * HttpRequestBody -> Async<string>)

    let requestBase url param ((reqFuncGet, reqFuncPost) : Reqs)
        (((rtoken, atoken) as tokens) : Tokens) =
        let requestBaseI url param (AccessToken accesstoken) =
            let header =
                List.concat
                    [ baseHeader

                      [ "App-OS", "ios"
                        "App-OS-Version", "9.3.3"
                        "App-Version", "6.0.9"

                        Authorization
                        <| System.String.Format("Bearer {0}", accesstoken) ] ]
            reqFuncGet ("https://app-api.pixiv.net" + url, header, param, "Get")
        try
            async { let! res = requestBaseI url param atoken
                    return res, tokens }
        with _ ->
            async {
                let! loginRes = loginRefreshToken rtoken reqFuncPost
                let accesstoken = getAccessToken loginRes
                let refreshtoken = getRefreshToken loginRes
                let! res = requestBaseI url param accesstoken
                return res, (refreshtoken, accesstoken)
            }

    type IllustResProvider = JsonProvider<"illust.json", SampleIsList=true>

    type IllustRes = IllustResProvider.Root

    type Illust = IllustResProvider.Illust

    type Illusts = Illust []

    type NextUrl = NextUrl of string

    let getNextUrl (res : IllustRes) = Option.map NextUrl res.NextUrl

    let requestBaseIllust url param req token : Async<(Illusts * NextUrl option) * Tokens> =
        let filter = "filter", "for_ios"
        let withFilter param = filter :: param
        async {
            let! res, tokens = requestBase url (withFilter param) token req
            let parsed = IllustResProvider.Parse(res)
            return (parsed.Illusts, getNextUrl parsed), tokens
        }

    let getNextUrlIllust (NextUrl nexturl) = requestBaseIllust nexturl []

    type Restrict =
        | Public
        | Private

    let mkRestrictStr =
        function
        | Public -> "restrict", "public"
        | Private -> "restrict", "private"

    let getIllustFollowParam restrict =
        "/v2/illust/follow", [ mkRestrictStr restrict ]

    let getUserBookmarksIllustParam userid restrict =
        "/v1/user/bookmarks/illust",
        [ "user_id", userid
          mkRestrictStr restrict ]

module PixivF =
    //open FSharp.Data
    open FSharpPlus
    open FSharpPlus.Data
    open FSharp.Control

    let getSeqNext next cb token reqs =
        AsyncSeq.unfoldAsync (function
            | (Some n), token ->
                async {
                    let! (res, n), t = PixivBase.getNextUrlIllust n token reqs
                    do! cb res token
                    return Some((res, t), (n, t))
                }
            | None, _ -> async { return None }) (next, token)

    let getSeq url param cb token reqs : Async<seq<PixivBase.Illust * PixivBase.Tokens>> =
        async {
            return asyncSeq {
                       let! (res, next), token = PixivBase.requestBaseIllust url
                                                     param token reqs
                       yield (res, token)
                       yield! getSeqNext next cb token reqs
                   }
                   |> AsyncSeq.map
                          (fun (res, token) -> map (fun x -> (x, token)) res)
                   |> AsyncSeq.concatSeq
                   |> AsyncSeq.toBlockingSeq
        }

    type PixivM<'T> = StateT<PixivBase.Tokens, ReaderT<PixivBase.Reqs, SeqT<Async<seq<'T * PixivBase.Tokens>>>>>

    let requestM url param cb : PixivM<PixivBase.Illust> =
        StateT <| fun t -> (ReaderT <| fun r -> SeqT(getSeq url param cb t r))

