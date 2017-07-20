unit Grijjy.Androidapi.Google.Helper;

{$I Grijjy.inc}

interface

uses
  Androidapi.JNIBridge,
  Androidapi.JNI.App,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaUtil,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Os,
  Androidapi.JNI.Util;

type
  { GoogleSignIn Helper }

  JGoogleSignInHelper = interface;//com.grijjy.googlehelper.GoogleSignInHelper
  JGoogleSignInCallback = interface;//com.grijjy.googlehelper.GoogleSignInCallback

  // com.grijjy.googlehelper.GoogleSignInCallback
  JGoogleSignInHelperClass = interface(JObjectClass)
    ['{C8521359-D50B-47A3-AEC8-57FBDBCF78B8}']
    {class} function init: JGoogleSignInHelper; cdecl;
  end;

  [JavaSignature('com/grijjy/googlehelper/GoogleSignInHelper')]
  JGoogleSignInHelper = interface(JObject)
    ['{1BEFE240-ED4B-4E1A-B65C-82DA948B682C}']
    function initSignIn(P1: JContext; P2: JGoogleSignInCallback; P3: JString; P4: TJavaObjectArray<JString>; P5: Boolean): Boolean; cdecl;
    procedure signInSilently; cdecl;
    procedure signIn; cdecl;
    procedure HandleActivityResult(requestCode: Integer; resultCode: Integer; intent: JIntent); cdecl;
    procedure signOut; cdecl;
    procedure revokeAccess; cdecl;
    function getIdToken: JString; cdecl;
    function getId: JString; cdecl;
  end;
  TJGoogleSignInHelper = class(TJavaGenericImport<JGoogleSignInHelperClass, JGoogleSignInHelper>) end;

  JGoogleSignInCallbackClass = interface(IJavaClass)
    ['{71D7118E-D1EA-4CBC-98B2-294056405EDF}']
  end;

  [JavaSignature('com/grijjy/googlehelper/GoogleSignInCallback')]
  JGoogleSignInCallback = interface(IJavaInstance)
    ['{E88F5860-A00E-4094-B659-7CEB8186FEAE}']
    procedure StartActivityForResult(P1: JIntent; P2: Integer); cdecl;
    procedure onSuccess(emailAddress: JString; displayName: JString; id: JString; idToken: JString; authCode: JString); cdecl;
    procedure onFailure(errorCode: Integer; errorDesc: JString); cdecl;
  end;
  TJGoogleSignInCallback = class(TJavaGenericImport<JGoogleSignInCallbackClass, JGoogleSignInCallback>) end;

implementation

procedure RegisterTypes;
begin
  TRegTypes.RegisterType('Grijjy.Androidapi.Google.Helper.JGoogleSignInHelper', TypeInfo(Grijjy.Androidapi.Google.Helper.JGoogleSignInHelper));
  TRegTypes.RegisterType('Grijjy.Androidapi.Google.Helper.JGoogleSignInCallback', TypeInfo(Grijjy.Androidapi.Google.Helper.JGoogleSignInCallback));
end;

initialization
  RegisterTypes;
end.
