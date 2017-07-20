unit Google.API;

{ Google OAuth2 APIs }

{$I Grijjy.inc}

interface

uses
  {$IF Defined(MSWINDOWS) or Defined(LINUX)}
  Grijjy.Http,
  {$ELSE}
  IdHttp,
  IdException,
  {$ENDIF}
  System.Classes,
  Google.Types;

const
  DEFAULT_REDIRECT_URI = 'http://localhost/oauth2callback';

type
  { Google API helper class for client-side OAuth 2.0 }
  TGoogleApi = class
  private
    FClientId: String;
    FClientSecret: String;
    FAccessToken: String;
    FRedirectUri: String;
  private
    { Http }
    function HttpPost(const AUrl, AContentType, ARequestBody: String;
      out AResponse: String): Integer;
    function HttpGet(const AUrl: String; out AResponse: String): Integer;
  public
    { Google APIs }
    function Get(const AUri: String; const AAccessToken: String; out AResponse: String): Integer; overload;
    function Get(const AUri: String; out AResponse: String): Integer; overload;
  public
    { Login & authentication flow }
    function OAuth2_Url(const AAuthScope: TArray<String>): String;
  public
    { Returns the tokeninfo for a given id token or access token }
    { https://developers.google.com/identity/sign-in/android/backend-auth }
    function AccessTokenInfo(const AToken: String; out AAccessTokenHeader: TGoogleAccessTokenHeader;
      out AEmail: String; out AExpiresIn: Integer): Integer;
    function IdTokenInfo(const AToken: String; out AIdTokenHeader: TGoogleIdTokenHeader;
      out AEmail, AName: String; out AResponse: String): Integer;

    { Creates an access token and a refresh token using the provided Firebase id token
      and one-time server autocode }
    { https://developers.google.com/identity/sign-in/android/offline-access }
    function Grant_AuthorizationCode(const AIdToken, AAuthCode: String;
      out AAccessToken, ARefreshToken: String; out AExpiresIn: Integer; out AResponse: String): Boolean;

    { Request an updated access token with the given refresh token }
    function Grant_RefreshToken(const ARefreshToken: String;
      out AAccessToken: String; out AExpiresIn: Integer; out AResponse: String): Boolean;

    { Revokes the given access token }
    function RevokeToken(const AToken: String; out AResponse: String): Boolean;
  public
    { Plus }
    function GetPerson(const AUserId: String; out APerson: TGooglePerson; out AResponse: String): Integer;
  public
    constructor Create(const AClientId: String = ''; const AClientSecret: String = '');
    destructor Destroy; override;
  public
    { Returns the current access token }
    property AccessToken: String read FAccessToken write FAccessToken;

    { Returns the current redirect uri }
    property RedirectUri: String read FRedirectUri write FRedirectUri;
  end;

implementation

uses
  System.SysUtils,
  System.Net.URLClient,
  System.NetEncoding,
  System.DateUtils,
  Grijjy.Bson;

{ TGoogleApi }

constructor TGoogleApi.Create(const AClientId: String; const AClientSecret: String);
begin
  FClientId := AClientId;
  FClientSecret := AClientSecret;
  FRedirectUri := DEFAULT_REDIRECT_URI;
end;

destructor TGoogleApi.Destroy;
begin
  inherited;
end;

function TGoogleApi.OAuth2_Url(const AAuthScope: TArray<String>): String;
var
  Scopes: String;
  I: Integer;
begin
  for I := 0 to Length(AAuthScope) - 1 do
    if I < Length(AAuthScope) - 1 then
      Scopes := Scopes + TNetEncoding.URL.Encode(AAuthScope[I]) + #32
    else
      Scopes := Scopes + TNetEncoding.URL.Encode(AAuthScope[I]);
  Result := 'https://accounts.google.com/o/oauth2/v2/auth' +
    '?scope=' + Scopes +
    '&include_granted_scopes=true' +
    '&state=state_parameter_passthrough_value' +
    '&redirect_uri=' + TNetEncoding.URL.Encode(FRedirectUri) +
    '&response_type=token' +
    '&client_id=' + TNetEncoding.URL.Encode(FClientId);
end;

{$IF Defined(MSWINDOWS) or Defined(LINUX)}
function TGoogleApi.HttpPost(const AUrl, AContentType, ARequestBody: String;
  out AResponse: String): Integer;
var
  HTTP: TgoHTTPClient;
  URI: TURI;
begin
  HTTP := TgoHTTPClient.Create;
  try
    HTTP.ContentType := AContentType;
    HTTP.RequestBody := ARequestBody;
    AResponse := HTTP.Post(AUrl);
    Result := HTTP.ResponseStatusCode;
  finally
    HTTP.Free;
  end;
end;

function TGoogleApi.HttpGet(const AUrl: String; out AResponse: String): Integer;
var
  HTTP: TgoHTTPClient;
  URI: TURI;
begin
  HTTP := TgoHTTPClient.Create;
  try
    AResponse := HTTP.Get(AUrl);
    Result := HTTP.ResponseStatusCode;
  finally
    HTTP.Free;
  end;
end;
{$ELSE}

function TGoogleApi.HttpPost(const AUrl, AContentType, ARequestBody: String;
  out AResponse: String): Integer;
var
  HTTP: TIdHTTP;
  Content: TStringStream;
begin
  Content := TStringStream.Create(ARequestBody, TEncoding.UTF8);
  try
    HTTP := TIdHTTP.Create;
    try
      HTTP.Request.ContentType := AContentType;
      HTTP.Request.CharSet := 'utf-8';
      try
        AResponse := HTTP.Post(AURL, Content);
      except
        on E: EIdException do
          // ignore
      end;
      Result := HTTP.ResponseCode;
    finally
      HTTP.Free;
    end;
  finally
    Content.Free;
  end;
end;

function TGoogleApi.HttpGet(const AUrl: String; out AResponse: String): Integer;
var
  HTTP: TIdHTTP;
begin
  HTTP := TIdHTTP.Create;
  try
    try
      AResponse := HTTP.Get(AURL);
      Result := HTTP.ResponseCode;
    except
      on E: EIdHTTPProtocolException do
        Result := HTTP.ResponseCode;
    end;
  finally
    HTTP.Free;
  end;
end;
{$ENDIF}

function TGoogleApi.Get(const AUri: String; const AAccessToken: String; out AResponse: String): Integer;
var
  URI: TURI;
begin
  URI := TURI.Create('https://www.googleapis.com/' + AUri);
  if URI.Query = '' then
    Result := HttpGet('https://www.googleapis.com/' + AUri + '?access_token=' + AAccessToken, AResponse)
  else
    Result := HttpGet('https://www.googleapis.com/' + AUri + '&access_token=' + AAccessToken, AResponse);
end;

function TGoogleApi.Get(const AUri: String; out AResponse: String): Integer;
begin
  Result := Get(AUri, FAccessToken, AResponse);
end;

function TGoogleApi.AccessTokenInfo(const AToken: String; out AAccessTokenHeader: TGoogleAccessTokenHeader;
  out AEmail: String; out AExpiresIn: Integer): Integer;
var
  Response: String;
  Doc: TgoBsonDocument;
begin
  Result := HttpGet('https://www.googleapis.com/oauth2/v3/tokeninfo?access_token=' + AToken, Response);
  if Result = 200 then
  begin
    Doc := TgoBsonDocument.Parse(Response);

    // access token header
    AAccessTokenHeader.sub := Doc['sub'];
    AAccessTokenHeader.azp := Doc['azp'];
    AAccessTokenHeader.aud := Doc['aud'];
    AAccessTokenHeader.exp := Doc['exp'];

    AEmail:= Doc['email'];
    AExpiresIn := Doc['expires_in'];
  end;
end;

function TGoogleApi.IdTokenInfo(const AToken: String; out AIdTokenHeader: TGoogleIdTokenHeader;
  out AEmail, AName: String; out AResponse: String): Integer;
var
  Doc: TgoBsonDocument;
begin
  Result := HttpGet('https://www.googleapis.com/oauth2/v3/tokeninfo?id_token=' + AToken, AResponse);
  if Result = 200 then
  begin
    Doc := TgoBsonDocument.Parse(AResponse);

    // id token header
    AIdTokenHeader.iss := Doc['iss'];
    AIdTokenHeader.sub := Doc['sub'];
    AIdTokenHeader.azp := Doc['azp'];
    AIdTokenHeader.aud := Doc['aud'];
    AIdTokenHeader.iat := Doc['iat'];
    AIdTokenHeader.exp := Doc['exp'];

    // optional
    AEmail:= Doc['email'];
    AName := Doc['name'];
  end;
end;

function TGoogleApi.Grant_AuthorizationCode(const AIdToken, AAuthCode: String;
  out AAccessToken, ARefreshToken: String; out AExpiresIn: Integer; out AResponse: String): Boolean;
var
  Doc: TgoBsonDocument;
begin
  if HttpPost(
    'https://www.googleapis.com/oauth2/v4/token',
    'application/x-www-form-urlencoded',
    'grant_type=authorization_code' + '&' +
    'client_id=' + TNetEncoding.URL.Encode(FClientId) + '&' +
    'client_secret=' + TNetEncoding.URL.Encode(FClientSecret) + '&' +
    'redirect_uri=' + '&' +
    'code=' + TNetEncoding.URL.Encode(AAuthCode) + '&' +
    'id_token=' + TNetEncoding.URL.Encode(AIdToken),
    AResponse) = 200 then
  begin
    Doc := TgoBsonDocument.Parse(AResponse);
    AAccessToken := Doc['access_token'];
    ARefreshToken := Doc['refresh_token'];
    AExpiresIn := Doc['expires_in'];
    Result := True;
  end
  else
    Result := False;
end;

function TGoogleApi.Grant_RefreshToken(const ARefreshToken: String;
  out AAccessToken: String; out AExpiresIn: Integer; out AResponse: String): Boolean;
var
  Doc: TgoBsonDocument;
begin
  if HttpPost(
    'https://www.googleapis.com/oauth2/v4/token',
    'application/x-www-form-urlencoded',
    'grant_type=refresh_token' + '&' +
    'client_id=' + TNetEncoding.URL.Encode(FClientId) + '&' +
    'client_secret=' + TNetEncoding.URL.Encode(FClientSecret) + '&' +
    'refresh_token=' + TNetEncoding.URL.Encode(ARefreshToken),
    AResponse) = 200 then
  begin
    Doc := TgoBsonDocument.Parse(AResponse);
    AAccessToken := Doc['access_token'];
    AExpiresIn := Doc['expires_in'];
    Result := True;
  end
  else
    Result := False;
end;

function TGoogleApi.RevokeToken(const AToken: String; out AResponse: String): Boolean;
begin
  Result := HttpPost(
    'https://accounts.google.com/o/oauth2/revoke?token=' + AToken,
    'application/x-www-form-urlencoded',
    '',
    AResponse) = 200;
end;

function TGoogleApi.GetPerson(const AUserId: String; out APerson: TGooglePerson; out AResponse: String): Integer;
var
  Doc, EMailDoc, ImageDoc: TgoBsonDocument;
  EMailsValue, EMailValue, ImageValue: TgoBsonValue;
  EmailsArray: TgoBsonArray;
begin
  Result := Get('plus/v1/people/' + AUserId, AResponse);
  if Result = 200 then
  begin
    Doc := TgoBsonDocument.Parse(AResponse);
    APerson.Id := Doc['id'];
    APerson.DisplayName := Doc['displayName'];
    if Doc.TryGetValue('emails', EMailsValue) then
    begin
      EMailsArray := EMailsValue.AsBsonArray;
      for EMailValue in EmailsArray do
      begin
        EMailDoc := EMailValue.AsBsonDocument;
        APerson.EMails := APerson.EMails + [EmailDoc['value']];
      end;
    end;
    APerson.Gender := Doc['gender'];
    APerson.Tagline := Doc['tagline'];
    APerson.Url := Doc['url'];
    if Doc.TryGetValue('image', ImageValue) then
    begin
      ImageDoc := ImageValue.AsBsonDocument;
      APerson.ImageUrl := ImageDoc['url'];
    end;
    APerson.IsPlusUser := Doc['isPlusUser'];
    APerson.Language := Doc['language'];
    APerson.CircledByCount := Doc['circledByCount'];
  end;
end;

end.
