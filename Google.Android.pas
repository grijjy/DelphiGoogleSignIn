unit Google.Android;

{ Provides class abstraction for the Google Sign-In libraries for Android }

{$I Grijjy.inc}

interface

uses
  System.SysUtils,
  System.Messaging,
  FMX.Platform,
  Androidapi.JNI.Os,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.App,
  Google.Types,
  Androidapi.Helpers,
  Androidapi.JNIBridge,
  Androidapi.Jni,
  Androidapi.JNI.JavaTypes,
  FMX.Platform.Android,
  FMX.Helpers.Android,
  Grijjy.Androidapi.Google.Helper;

const
  RC_SIGN_IN = 9001;

type
  TgoGoogleSDK = class
  private
    FGoogleSignInHelper: JGoogleSignInHelper;
    FInitialized: Boolean;
    FClientId: String;
    FServerClientId: String;
    FRequestServerAuthCode: Boolean;
    FTokenType: Integer;
    function Initialized(const AScopes: TArray<String>): Boolean;
  private type
    TGoogleSignInCallback = class(TJavaLocal, JGoogleSignInCallback)
    private
      [weak] FImplementation: TgoGoogleSDK;
      procedure QueueMessage(const ASender: TObject; const AMessage: TMessage; const ADispose: Boolean = True);
    public
      procedure StartActivityForResult(P1: JIntent; P2: Integer); cdecl;
      procedure onSuccess(emailAddress: JString; displayName: JString; id: JString; idToken: JString; authCode: JString); cdecl;
      procedure onFailure(errorCode: Integer; errorDesc: JString); cdecl;
    public
      constructor Create(const AImplementation: TgoGoogleSDK);
    end;
  private
    FGoogleSignInCallback: TGoogleSignInCallback;
  private
    procedure QueueMessage(const ASender: TObject; const AMessage: TMessage; const ADispose: Boolean = True);
    procedure HandleActivityMessage(const Sender: TObject; const M: TMessage);
  public
    { Login }
    procedure SignIn(const AScopes: TArray<String>);
    procedure SignOut;
    function CurrentIdToken: String;
    function CurrentUserId: String;
  public
    constructor Create(const AClientId: String; const ATokenType: Integer = TOKEN_TYPE_OAUTH;
      const AServerClientId: String = ''; const ARequestServerAuthCode: Boolean = False);
    destructor Destroy; override;
  end;

implementation

uses
  System.Types,
  System.UITypes,
  System.Classes;

{ Helpers }

//procedure LogExtras(const AIntent: JIntent);
//var
//  Extras: JBundle;
//  ExtrasArray: TJavaObjectArray<AndroidApi.JNI.JavaTypes.JObject>;
//  I: Integer;
//begin
//  Extras := AIntent.getExtras;
//  if Extras <> nil then
//  begin
//    ExtrasArray := Extras.KeySet.toArray;
//    for I := 0 to ExtrasArray.Length - 1 do
//      grLog('Extras', JStringToString(ExtrasArray.Items[I].toString));
//  end;
//end;

{ TgoGoogleSDK }

constructor TgoGoogleSDK.Create(const AClientId: String; const ATokenType: Integer;
  const AServerClientId: String; const ARequestServerAuthCode: Boolean);
begin
  FInitialized := False;
  FClientId := AClientId; { not used on Android }
  FTokenType := ATokenType; { token type is ignored for Android }
  FServerClientId := AServerClientId;
  FRequestServerAuthCode := ARequestServerAuthCode;
  FGoogleSignInHelper := nil;
  TMessageManager.DefaultManager.SubscribeToMessage(TMessageResultNotification, HandleActivityMessage);
end;

destructor TgoGoogleSDK.Destroy;
begin
  TMessageManager.DefaultManager.Unsubscribe(TMessageResultNotification, HandleActivityMessage);
  inherited;
end;

{ This method is fired in the context of another thread and you need to make sure that
  any canvas operations that happen as a result of this callback only happen in the
  main application thread }
procedure TgoGoogleSDK.QueueMessage(const ASender: TObject;
  const AMessage: TMessage; const ADispose: Boolean);
begin
  TThread.Queue(nil,
    procedure
    begin
      TMessageManager.DefaultManager.SendMessage(ASender, AMessage, ADispose);
    end);
end;

procedure TgoGoogleSDK.HandleActivityMessage(const Sender: TObject; const M: TMessage);
var
//  Intent: JIntent;
  SignInResult: TGoogleSignInResult;
  SignInResultMessage: TGoogleSignInResultMessage;
begin
  if M is TMessageResultNotification then
  begin
    if TMessageResultNotification(M).RequestCode = RC_SIGN_IN then // sign-in response
    begin
      { send activity message back to helper }
      if FInitialized then
        FGoogleSignInHelper.HandleActivityResult(
          TMessageResultNotification(M).RequestCode,
          TMessageResultNotification(M).ResultCode,
          TMessageResultNotification(M).Value);

      if TMessageResultNotification(M).ResultCode = TJActivity.JavaClass.RESULT_OK then
      begin
//        Intent := TMessageResultNotification(M).Value;
//        if Intent <> nil then
//        begin
//          LogExtras(Intent);
//          SignInResult.Initialize;
//          SignInResult.Result := True;
//          SignInResultMessage := TGoogleSignInResultMessage.Create(SignInResult);
//
//          { this response is created by the OnActivityResult, so it needs to be handled by the main thread instead }
//          QueueMessage(Self, SignInResultMessage);
//       end;
      end
      else
      if TMessageResultNotification(M).ResultCode = TJActivity.JavaClass.RESULT_CANCELED then
      begin
        SignInResult.Initialize;
        SignInResult.Cancelled := True;
        SignInResultMessage := TGoogleSignInResultMessage.Create(SignInResult);

        { this response is created by the OnActivityResult, so it needs to be handled by the main thread instead }
        QueueMessage(Self, SignInResultMessage);
      end;
    end;
  end;
end;

function TgoGoogleSDK.Initialized(const AScopes: TArray<String>): Boolean;
var
  Scopes: TJavaObjectArray<JString>;
  I: Integer;
begin
  if not FInitialized then
  begin
    if FGoogleSignInCallback = nil then
      FGoogleSignInCallback := TGoogleSignInCallback.Create(Self);
    if FGoogleSignInHelper = nil then
      FGoogleSignInHelper := TJGoogleSignInHelper.JavaClass.init;
    if FGoogleSignInHelper <> nil then
    begin
      Scopes := TJavaObjectArray<JString>.Create(Length(AScopes));
      try
        for I := 0 to Length(AScopes) - 1 do
          Scopes[I] := StringToJString(AScopes[I]);
        FInitialized := FGoogleSignInHelper.initSignIn(
          TAndroidHelper.Activity,
          FGoogleSignInCallback,
          StringToJString(FServerClientId),
          Scopes,
          FRequestServerAuthCode)
      finally
        Scopes.Free;
      end;
    end
    else
      FInitialized := False;
  end;
  Result := FInitialized;
end;

procedure TgoGoogleSDK.SignIn(const AScopes: TArray<String>);
begin
  if Initialized(AScopes) then
    FGoogleSignInHelper.signInSilently;
end;

procedure TgoGoogleSDK.SignOut;
begin
  if FInitialized then
  begin
    FGoogleSignInHelper.signOut;
    FInitialized := False;
  end;
end;

function TgoGoogleSDK.CurrentIdToken: String;
begin
  if FInitialized then
    Result := JStringToString(FGoogleSignInHelper.getIdToken);
end;

function TgoGoogleSDK.CurrentUserId: String;
begin
  if FInitialized then
    Result := JStringToString(FGoogleSignInHelper.getId);
end;

{ TgoGoogleSDK.TGoogleSignInCallback }

constructor TgoGoogleSDK.TGoogleSignInCallback.Create(
  const AImplementation: TgoGoogleSDK);
begin
  Assert(Assigned(AImplementation));
  inherited Create;
  FImplementation := AImplementation;
end;

{ This method is fired in the context of another thread and you need to make sure that
  any canvas operations that happen as a result of this callback only happen in the
  main application thread }
procedure TgoGoogleSDK.TGoogleSignInCallback.QueueMessage(
  const ASender: TObject; const AMessage: TMessage; const ADispose: Boolean);
begin
  TThread.Queue(nil,
    procedure
    begin
      TMessageManager.DefaultManager.SendMessage(ASender, AMessage, ADispose);
    end);
end;

procedure TgoGoogleSDK.TGoogleSignInCallback.onFailure(errorCode: Integer; errorDesc: JString);
var
  SignInResult: TGoogleSignInResult;
  SignInResultMessage: TGoogleSignInResultMessage;
begin
  SignInResult.Initialize;
  SignInResult.Error := errorCode;
  SignInResult.ErrorDesc := JStringToString(errorDesc);
  SignInResultMessage := TGoogleSignInResultMessage.Create(SignInResult);
  QueueMessage(Self, SignInResultMessage);
end;

procedure TgoGoogleSDK.TGoogleSignInCallback.onSuccess(emailAddress,
  displayName, id, idToken, authCode: JString);
var
  SignInResult: TGoogleSignInResult;
  SignInResultMessage: TGoogleSignInResultMessage;
begin
  SignInResult.Initialize;
  SignInResult.Result := True;
  SignInResult.UserId := JStringToString(id);
  SignInResult.Name := JStringToString(displayName);
  SignInResult.TokenType := TOKEN_TYPE_FIREBASE;
  SignInResult.Token := JStringToString(idToken);
  SignInResult.AuthCode := JStringToString(authCode);
  SignInResult.Email := JStringToString(emailAddress);
  SignInResultMessage := TGoogleSignInResultMessage.Create(SignInResult);
  QueueMessage(Self, SignInResultMessage);
end;

procedure TgoGoogleSDK.TGoogleSignInCallback.StartActivityForResult(
  P1: JIntent; P2: Integer);
begin
  { callback to trigger intent within the main firemonkey activity }
  MainActivity.startActivityForResult(P1, P2);
end;

end.
