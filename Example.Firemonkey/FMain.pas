unit FMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Messaging,
  System.DateUtils,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Edit,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Platform,
  FMX.Gestures,
  FMX.TabControl,
  Google.Types,
  Google.Android,
  Google.Api;

type
  TFormMain = class(TForm)
    HeaderToolBar: TToolBar;
    ToolBarLabel: TLabel;
    TabControlGoogle: TTabControl;
    TabItemSignIn: TTabItem;
    TabItemVerify: TTabItem;
    TabItemGrant: TTabItem;
    TabItemOAuth2: TTabItem;
    GestureManager1: TGestureManager;
    ButtonSignIn: TButton;
    ButtonSignOut: TButton;
    EditClientId: TEdit;
    LabelClientId: TLabel;
    CheckBoxRequestServerAuthCode: TCheckBox;
    EditSignInUserId: TEdit;
    LabelUserId: TLabel;
    LabelSignInIdToken: TLabel;
    EditSignInIdToken: TEdit;
    LabelSignInAuthCode: TLabel;
    LabelEmail: TLabel;
    EditSignInEmail: TEdit;
    LabelVerifyIdToken: TLabel;
    EditVerifyIdToken: TEdit;
    ButtonVerify: TButton;
    EditSignInAuthCode: TEdit;
    MemoVerify: TMemo;
    LabelVerifyEmail: TLabel;
    EditVerifyEmail: TEdit;
    EditVerifyName: TEdit;
    LabelVerifyName: TLabel;
    LabelVerifyResult: TLabel;
    LabelAuthCodeIdToken: TLabel;
    EditAuthCodeIdToken: TEdit;
    LabelAuthCode: TLabel;
    EditAuthCode: TEdit;
    ButtonAuthCodeGrantRefreshToken: TButton;
    LabelAuthCodeRefreshToken: TLabel;
    EditAuthCodeRefreshToken: TEdit;
    MemoAuthCode: TMemo;
    LabelClientSecret: TLabel;
    EditClientSecret: TEdit;
    LabelOAuth2RefreshToken: TLabel;
    EditOAuth2RefreshToken: TEdit;
    ButtonOAuth2GrantToken: TButton;
    MemoOAuth2: TMemo;
    EditOAuth2AccessToken: TEdit;
    LabelOAuth2AccessToken: TLabel;
    LabelAuthCodeAccessToken: TLabel;
    EditAuthCodeAccessToken: TEdit;
    ButtonAuthCodeRevokeToken: TButton;
    TabItemAPI: TTabItem;
    LabelAPIAccessToken: TLabel;
    EditAPIAccessToken: TEdit;
    ButtonAPIGetPerson: TButton;
    MemoAPI: TMemo;
    ButtonAPIGetPopularVideos: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormGesture(Sender: TObject; const EventInfo: TGestureEventInfo;
      var Handled: Boolean);
    procedure ButtonSignInClick(Sender: TObject);
    procedure ButtonSignOutClick(Sender: TObject);
    procedure ButtonVerifyClick(Sender: TObject);
    procedure ButtonAuthCodeGrantRefreshTokenClick(Sender: TObject);
    procedure ButtonOAuth2GrantTokenClick(Sender: TObject);
    procedure ButtonAuthCodeRevokeTokenClick(Sender: TObject);
    procedure ButtonAPIGetPersonClick(Sender: TObject);
    procedure ButtonAPIGetPopularVideosClick(Sender: TObject);
  private
    { Private declarations }
    FGoogleSDK: TgoGoogleSDK;
    procedure GoogleSignInResultMessageListener(const Sender: TObject;
      const M: TMessage);
    procedure Reset;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  { This defines the default active tab at runtime }
  TabControlGoogle.ActiveTab := TabItemSignIn;

  TMessageManager.DefaultManager.SubscribeToMessage(TGoogleSignInResultMessage, GoogleSignInResultMessageListener);
  FGoogleSDK := nil;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  TMessageManager.DefaultManager.Unsubscribe(TGoogleSignInResultMessage, GoogleSignInResultMessageListener);
  FreeAndNil(FGoogleSDK);
end;

procedure TFormMain.FormGesture(Sender: TObject;
  const EventInfo: TGestureEventInfo; var Handled: Boolean);
begin
{$IFDEF ANDROID}
  case EventInfo.GestureID of
    sgiLeft:
    begin
      if TabControlGoogle.ActiveTab <> TabControlGoogle.Tabs[TabControlGoogle.TabCount-1] then
        TabControlGoogle.ActiveTab := TabControlGoogle.Tabs[TabControlGoogle.TabIndex+1];
      Handled := True;
    end;

    sgiRight:
    begin
      if TabControlGoogle.ActiveTab <> TabControlGoogle.Tabs[0] then
        TabControlGoogle.ActiveTab := TabControlGoogle.Tabs[TabControlGoogle.TabIndex-1];
      Handled := True;
    end;
  end;
{$ENDIF}
end;

procedure TFormMain.GoogleSignInResultMessageListener(const Sender: TObject;
  const M: TMessage);
var
  GoogleSignInResultMessage: TGoogleSignInResultMessage;
  GoogleSignInResult: TGoogleSignInResult;
begin
  GoogleSignInResultMessage := M as TGoogleSignInResultMessage;
  GoogleSignInResult := GoogleSignInResultMessage.Value;
  if GoogleSignInResult.Result then
  begin
    ShowMessage('Success');
    EditSignInUserId.Text := GoogleSignInResult.UserId;
    EditSignInEmail.Text := GoogleSignInResult.Email;
    EditSignInIdToken.Text := GoogleSignInResult.Token;
    EditSignInAuthCode.Text := GoogleSignInResult.AuthCode;
    EditVerifyIdToken.Text := GoogleSignInResult.Token;
    EditAuthCodeIdToken.Text := GoogleSignInResult.Token;
    EditAuthCode.Text := GoogleSignInResult.AuthCode;
  end
  else
  if GoogleSignInResult.Cancelled then
    ShowMessage('Cancelled')
  else
    ShowMessage(Format('Failed (Error=%d, %s)', [GoogleSignInResult.Error, GoogleSignInResult.ErrorDesc]));
end;

procedure TFormMain.Reset;
begin
  // signin
  EditSignInUserId.Text := '';
  EditSignInEmail.Text := '';
  EditSignInIdToken.Text := '';
  EditSignInAuthCode.Text := '';

  // verify
  EditVerifyIdToken.Text := '';
  EditVerifyEmail.Text := '';
  EditVerifyName.Text := '';
  MemoVerify.Lines.Clear;

  // authcode
  EditAuthCodeIdToken.Text := '';
  EditAuthCode.Text := '';
  EditAuthCodeRefreshToken.Text := '';
  EditAuthCodeAccessToken.Text := '';
  MemoAuthCode.Lines.Clear;

  // oauth2
  EditOAuth2RefreshToken.Text := '';
  EditOAuth2AccessToken.Text := '';
  MemoOAuth2.Lines.Clear;
end;

procedure TFormMain.ButtonSignInClick(Sender: TObject);
begin
  Reset;

  // create TgoGoogleSDK
  if not Assigned(FGoogleSDK) then
    FGoogleSDK := TgoGoogleSDK.Create('',
      TOKEN_TYPE_FIREBASE,
      EditClientId.Text,
      CheckBoxRequestServerAuthCode.IsChecked);

  // sign-in the scopes
  FGoogleSDK.SignIn([ // profile and email scopes are implied
    'https://www.googleapis.com/auth/plus.me',
    'https://www.googleapis.com/auth/plus.circles.read',
    'https://www.googleapis.com/auth/youtube.readonly'
    ]);
end;

procedure TFormMain.ButtonSignOutClick(Sender: TObject);
begin
  Reset;
  if Assigned(FGoogleSDK) then
    FGoogleSDK.SignOut;
  ShowMessage('Signed out')
end;

procedure TFormMain.ButtonVerifyClick(Sender: TObject);
var
  GoogleApi: TGoogleApi;
  IdTokenHeader: TGoogleIdTokenHeader;
  Email, Name: String;
  Response: String;
begin
  GoogleApi := TGoogleApi.Create;
  try
    if GoogleApi.IdTokenInfo(EditVerifyIdToken.Text, IdTokenHeader, Email, Name, Response) = 200 then
    begin
      EditVerifyEmail.Text := Email;
      EditVerifyName.Text := Name;
      MemoVerify.Lines.Text := Response;

      { does the user id for the token match? }
      if (IdTokenHeader.sub = EditSignInUserId.Text) and

        { The value of aud in the ID token is equal to one of your app's client IDs.
          This check is necessary to prevent ID tokens issued to a malicious app being
          used to access data about the same user on your app's backend server.  }
        (IdTokenHeader.aud = EditClientId.Text) and

        { The value of iss in the ID token is equal to accounts.google.com or https://accounts.google.com. }
        (IdTokenHeader.iss = 'https://accounts.google.com') then
      begin
        { The expiry time of the ID token has not passed }
        if (IdTokenHeader.exp > DateTimeToUnix(TTimeZone.Local.ToUniversalTime(Now))) then
          ShowMessage('Verified')
        else
          ShowMessage('Expired');
      end
      else
        ShowMessage('Invalid Token');
    end;
  finally
    GoogleApi.Free;
  end;
end;

procedure TFormMain.ButtonAuthCodeGrantRefreshTokenClick(Sender: TObject);
var
  GoogleApi: TGoogleApi;
  AccessToken, RefreshToken: String;
  ExpiresIn: Integer;
  Response: String;
begin
  GoogleApi := TGoogleApi.Create(EditClientId.Text, EditClientSecret.Text) ;
  try
    if GoogleApi.Grant_AuthorizationCode(EditAuthCodeIdToken.Text, EditAuthCode.Text, AccessToken, RefreshToken, ExpiresIn, Response) then
    begin
      EditAuthCodeRefreshToken.Text := RefreshToken;
      EditAuthCodeAccessToken.Text := AccessToken;
      EditOAuth2RefreshToken.Text := RefreshToken;
    end;
    MemoAuthCode.Lines.Text := Response;
    if RefreshToken = 'null' then
      ShowMessage('RefreshToken=null.  You will have to revoke your token to obtain a new one and sign-in again.');
  finally
    GoogleApi.Free;
  end;
end;

procedure TFormMain.ButtonAuthCodeRevokeTokenClick(Sender: TObject);
var
  GoogleApi: TGoogleApi;
  Response: String;
begin
  GoogleApi := TGoogleApi.Create(EditClientId.Text, EditClientSecret.Text) ;
  try
    if not GoogleApi.RevokeToken(EditAuthCodeAccessToken.Text, Response) then
      MemoAuthCode.Lines.Text := Response
    else
      ShowMessage('Token revoked, please sign-in again.');
  finally
    GoogleApi.Free;
  end;
end;

procedure TFormMain.ButtonOAuth2GrantTokenClick(Sender: TObject);
var
  GoogleApi: TGoogleApi;
  AccessToken: String;
  ExpiresIn: Integer;
  Response: String;
begin
  GoogleApi := TGoogleApi.Create(EditClientId.Text, EditClientSecret.Text) ;
  try
    if GoogleApi.Grant_RefreshToken(EditOAuth2RefreshToken.Text, AccessToken, ExpiresIn, Response) then
    begin
      EditOAuth2AccessToken.Text := AccessToken;
      EditAPIAccessToken.Text := AccessToken;
      ShowMessage('Congratulations! You have an OAuth/2 token to make API calls.');
    end
    else
      ShowMessage('Error obtaining OAuth/2 token.');
    MemoOAuth2.Lines.Text := Response;
  finally
    GoogleApi.Free;
  end;
end;

procedure TFormMain.ButtonAPIGetPersonClick(Sender: TObject);
var
  GoogleApi: TGoogleApi;
  Person: TGooglePerson;
  Response: String;
begin
  GoogleApi := TGoogleApi.Create(EditClientId.Text);
  try
    GoogleApi.AccessToken := EditAPIAccessToken.Text;
    if GoogleApi.GetPerson(EditSignInUserId.Text, Person, Response) = 200 then
      MemoAPI.Lines.Text := Response;
  finally
    GoogleApi.Free;
  end;
end;

procedure TFormMain.ButtonAPIGetPopularVideosClick(Sender: TObject);
var
  GoogleApi: TGoogleApi;
  Response: String;
begin
  GoogleApi := TGoogleApi.Create(EditClientId.Text);
  try
    GoogleApi.AccessToken := EditAPIAccessToken.Text;
    if GoogleApi.Get('youtube/v3/activities?part=snippet,contentDetails&home=true', Response) = 200 then
      MemoAPI.Lines.Text := Response;
  finally
    GoogleApi.Free;
  end;
end;

end.
