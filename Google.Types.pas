unit Google.Types;

{$I Grijjy.inc}

interface

uses
  System.SysUtils,
  System.Messaging,
  System.DateUtils;

const
  TOKEN_TYPE_OAUTH = 0;
  TOKEN_TYPE_FIREBASE = 1;

type
  { Google access token header }
  TGoogleAccessTokenHeader = record
   sub: String;
   azp: String;
   aud: String;
   exp: Int64;
  end;

  { Google id token header }
  TGoogleIdTokenHeader = record
   iss: String;
   sub: String;
   azp: String;
   aud: String;
   iat: Int64;
   exp: Int64;
  end;

const
  { Number of padding seconds before a token is considered expired }
  REFRESH_TOKEN_SECONDS = 30;

type
  { Google token }
  TGoogleToken = record
    TokenType: Integer;
    AccessToken: String;
    RefreshToken: String;
    Expires: Int64;  { absolute utc time in seconds that the access token will expire }
  public
    procedure Initialize;
  public
    function IsExpired: Boolean;
  end;

type
  TGoogleSignInResult = record
  public
    Result: Boolean;
    Error: Integer;
    ErrorDesc: String;
    Cancelled: Boolean;
    UserId: String;
    TokenType: Integer;
    Token: String;
    AuthCode: String;
    Name: String;
    GivenName: String;
    FamilyName: String;
    Email: String;
  public
    procedure Initialize;
  end;

  TGoogleSignInResultMessage = class(TMessage<TGoogleSignInResult>)
  public
    constructor Create(const AGoogleLoginResult: TGoogleSignInResult);
  end;

type
  TGooglePerson = record
  public
    Id: String;
    DisplayName: String;
    EMails: TArray<String>;
    Gender: String;
    Tagline: String;
    Url: String;
    ImageUrl: String;
    IsPlusUser: Boolean;
    Language: String;
    CircledByCount: Integer;
  public
    procedure Initialize;
  end;

  TGooglePeople = TArray<TGooglePerson>;

implementation

{ TGoogleSignInResult }

procedure TGoogleSignInResult.Initialize;
begin
  Result := False;
  Error := 0;
  Cancelled := False;
  UserId := '';
  TokenType := TOKEN_TYPE_FIREBASE;
  Token := '';
  Name := '';
  GivenName := '';
  FamilyName := '';
  Email := '';
end;

{ TGoogleSignInResultMessage }

constructor TGoogleSignInResultMessage.Create(
  const AGoogleLoginResult: TGoogleSignInResult);
begin
  inherited Create(AGoogleLoginResult);
end;

{ TGooglePerson }

procedure TGooglePerson.Initialize;
begin
  Id := '';
  DisplayName := '';
  EMails := nil;
  Gender := '';
  Tagline := '';
  Url := '';
  ImageUrl := '';
  IsPlusUser := False;
  Language := '';
  CircledByCount := 0;
end;

{ TGoogleToken }

procedure TGoogleToken.Initialize;
begin
  TokenType := TOKEN_TYPE_OAUTH;
  AccessToken := '';
  RefreshToken := '';
  Expires := 0;
end;

function TGoogleToken.IsExpired: Boolean;
begin
  Result := (DateTimeToUnix(TTimeZone.Local.ToUniversalTime(Now)) + REFRESH_TOKEN_SECONDS) >= Expires;
end;

end.
