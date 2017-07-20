# Using Google Sign-In for Firebase SDK on Android
In this article we will show you how to use Google's Firebase SDK for sign-in on Android devices.  While most login implementations launch a web browser to start an OAuth/2 flow even in Windows, iOS and Android apps, we will be demonstrating how to directly use the Google SignIn SDK in your app to present a seamless experience for the user from signing in, all the way to requesting Google authentication scopes and using various Google APIs.

The beauty of this approach is that the experience for the user of your app is smoothly presented.  They simply click a button for sign-in with Google and consent to any authentication scopes you want to use.

For more information about us, our support and services visit the [Grijjy homepage](http://www.grijjy.com) or the [Grijjy developers blog](http://blog.grijjy.com).

The example contained here depends upon part of our [Grijjy Foundation library](https://github.com/grijjy/GrijjyFoundation).

The source code and related example repository are hosted on GitHub at [https://github.com/grijjy/DelphiGoogleSignIn](https://github.com/grijjy/DelphiGoogleSignIn).

# Introduction
If you are building mobile apps for Delphi you are probably already aware of the numerous APIs and services that Google provides for developers.  Google APIs range from cloud storage, cloud platform services, cloud messaging, YouTube data access and much more.

Before you start building an app with the Google SDKs and APIs you really need to answer a few questions.  Do you intend your app to be cross-platform for mobile and desktop?  The approach for Windows applications with Google APIs and mobile apps with Google SDKs is quite different.  You will need to address those variances in your implementation.  Will Google APIs be called from your client app only or is there a server backend for your app that can make some of the API calls instead?  Perhaps you only want to invoke the sign-in process in your mobile app, but all the actual Google API calls occur somewhere else.

Google provides solutions to all of the above scenarios, and we will discuss how to handle these variances in your app implementation, in this article.

Google offers SDKs that you can embed into your iOS and Android applications.  The current method that is widely recommended and used in Delphi is to authenticate using OAuth/2 via a web browser.  This method requires your user launch a browser to authenticate and internally retrieve an OAuth/2 token.  While this works, it is not ideal because users launch the web browser while your app goes into the background and when authentication is completed or fails, must return to your app.  Additionally the user is presented with the same behavior on subsequent launches of your application to help verify your authenticating credentials.  This is fine if necessary and as a backup method but it is hardly ideal when all you want to present a smoother experience for your user.

Once the user approves your app, future app launches can be completely automatic and silent because the SDK abstracts the complexities of managing the access token and approval process for us.

If you are not interested in the journey to make this all work for Google sign-in, you can skip directly to the sections on the TgoGoogleSDK class and the Getting Started Quick Steps.

Before continuing I wanted to say thank you to Dave Nottage and [Delphi Worlds](http://delphiworlds.com) for their excellent work on [Google Firebase and Cloud Messaging](http://delphiworlds.com/2017/05/add-firebase-cloud-messaging-mobile-apps-part-1/).  Their work provided insight and inspiration for writing this article.

# Tokens, tokens, tokens!
Google loves tokens!  While most other third-party APIs follow a basic OAuth/2 token flow to secure access, Google uses different tokens for various operations that in Google's own words, "provides increased security over your standard OAuth 2.0 flow" to their APIs.  In their APIs and documentation you will find references to Auth tokens, Id tokens, Access tokens, Refresh tokens, Server auth codes and more.  I picture a bunch of bored security researchers sitting around at Google and thinking of new ways to make it complicated for developers to access their APIs in the interest of security.  All kidding aside, once you figure out how and where to use the various token methodologies it will all work.

To access the Google APIs in your app from Delphi you first must starting by authenticating with Google.  Once you are authenticated you can interact with the Google APIs with a variety of approaches.  If your application is web based or Windows based, you would obtain an OAuth/2 token from Google and use that token to call the various Google API endpoints using REST/HTTP.

If your application is mobile based on iOS or Android, in the past Google would allow you obtain an OAuth/2 token using the Google SignIn SDK just like you would do on Windows or a web application, so that you could call REST/HTTP APIs directly from your client app.  Google however is depreciating this approach on mobile platforms in favor of using Firebase Id tokens instead and obfuscating the process of calling APIs using their SDK along with Firebase Id tokens.  On iOS it is still possible to request an OAuth/2 token or a Firebase Id token when you use the Google SignIn SDK, but on Android you can now only obtain a Firebase Id token using the Google SDK.  I expect Google to phase out OAuth/2 token access on all mobile platforms in the near future.

If you are developing an app that still wants to call HTTP/REST APIs, perhaps from a backend server process for example, then you still need an OAuth/2 token.  Fortunately Google provides a methodology to obtain an OAuth/2 token from your backend.  We will also discuss this more later in this article.

If we could easily consume the Google SDKs for Android in Delphi and we didn't need to make API calls from a server backend, we would not actually care about Google tokens at all.  Unfortunately due to some limitations in the JNI marshaling of Delphi, this isn't entirely possible.  I will discuss this more later.

# Getting Started with the Google SignIn SDK
To get started with using Google SignIn in your app, you need to take a few steps using the Google Firebase console. 

1.  Create a new project at the Firebase Console,
https://console.firebase.google.com

2.  Under 'Project settings' in the Firebase Console, click 'Add App' to add an Android application.

3.  Choose a package name that matches your Delphi projects Android package name. This is normally something like `com.embarcadero.<ModuleName>`.

4.  Add your 'SHA certificate fingerprint' by clicking 'Add Fingerprint'.  You will need a SHA fingerprint (also called your SHA1 hash) from every developer's PC than uses the Google SignIn SDK.  If you intend to release your application, you will also need the SHA1 hash from the Release keystore configuration for your specific app.  To obtain the correct SHA1 hash value for Delphi Debug configuration applications,
	1.  Locate your `debug.keystore` file.  This is usually under the folder, `C:\Users\<username>\AppData\Roaming\Embarcadero\BDS\<version>` on your PC.
	2.  Locate your `keytool.exe`.  This is typically installed with the Java JDK, so if you don't have the JDK installed somewhere on your PC, you will need it.  
	3.  From the command prompt, run:
	`keytool -list -v -keystore debug.keystore -alias androiddebugkey -storepass android -keypass android`
	4. You will receive an output that includes an SHA1 fingerprint, for example: 
		Certificate fingerprints:
        SHA1: 12 34 56 78 90 12 34 56...  
	5. Copy the SHA1 key from the output and paste this value into the 'Certificate fingerprint' field in the Firebase console.  A quick tip from a command prompt it to hit Ctrl-A, Ctrl-C to copy the output to the clipboard.
	> If you need to create a new debug.keystore, you can also do this with the keytool by running,
	`keytool -genkey -v -keystore debug.keystore -alias androiddebugkey -storepass android -keypass android -keyalg RSA -validity 14000`
  
5.  Download the `google-services.json` from the Firebase console and place this file into your Delphi project folder.  For the purposes of Google sign-in and everything we discuss in this article, you do not need to distribute this file with your app.  However, it contains several strings that we will need to use in our Delphi project.  If you examine the file you will notice there are multiple entries called "client_id".  The one you need for Android is "client_type" 3, also known as the "web client" id if you are using the Google Developers console (https://console.developers.google.com/apis/credentials) instead of the Firebase console (https://console.firebase.google.com/project).   
```Json
      "oauth_client": [
        {
          "client_id": "<android client id>",
          "client_type": 1,
          "android_info": {
            "package_name": "com.embarcadero.<ModuleName>",
            "certificate_hash": "329a1a43907c37f4d56e3200f1234567"
          }
        },
        {
          "client_id": "<Web client id>",
          "client_type": 3
        }
      ],
```
> When you create a project using the Firebase console, Google automatically creates two OAuth/2 Client Ids on your behalf.  The first is for the platform (ex: Android or iOS) for the given package name.  The second Client Id is called the "Web client" id which is used for API calls to REST/HTTP endpoints.  The later is the Client Id we will need for our Android app as well as any HTTP/REST API calls we might make.  If you already have an existing and valid "Web client" id in the Google developers console for the project, then the Firebase console will not create a new one.

6. Enable the APIs you intend to use in the API Manager of the Google Developers Console.  For Google SignIn you will need the Google+ API and probably the Google People API.

The above steps are required for all the various ways we may use Google SignIn, whether we are using it via a web browser for OAuth/2 on Windows, using it for Firebase Id tokens on Android or iOS or using it with our server backend with server auth codes and refresh tokens.  More about this topic later.

#Delphi's Androidapi.JNI.PlayServices unit
> This section covers the reasons why you cannot use the Google SDK directly from Delphi, but instead you must create a helper in Java to assist your Delphi application.  If you are not interested in the reasons why, you can skip this section.   

In order to use the Google SignIn SDK directly from Delphi, we need to be able to access a variety of Java interfaces from Delphi code.  Most of those interfaces exist in Google Play Services.

Newer versions of Delphi ship with a unit called `Androidapi.JNI.PlayServices` to interface with Google's Play Services.  If you examine it, you will notice it contains some of the required methods and interfaces required for Google SignIn, but others are absent or commented out.

For example it contains,
```Delphi
  [JavaSignature('com/google/android/gms/common/api/GoogleApiClient$Builder')]
  JGoogleApiClient_Builder = interface(JObject)
    ['{C40D55BB-AC6D-40FE-BEEB-701588835D98}']
    //function addApi(api: JApi): JGoogleApiClient_Builder; cdecl; overload;
    //function addApi(api: JApi; options: JGoogleApiClient_ApiOptions): JGoogleApiClient_Builder; cdecl; overload;
    function addConnectionCallbacks(listener: JGoogleApiClient_ConnectionCallbacks): JGoogleApiClient_Builder; cdecl;
    function addOnConnectionFailedListener(listener: JGoogleApiClient_OnConnectionFailedListener): JGoogleApiClient_Builder; cdecl;
    function addScope(scope: JScope): JGoogleApiClient_Builder; cdecl;
    function build: JGoogleApiClient; cdecl;
    //function dx: Jee; cdecl;
    function setAccountName(accountName: JString): JGoogleApiClient_Builder; cdecl;
    function setGravityForPopups(gravityForPopups: Integer): JGoogleApiClient_Builder; cdecl;
    function setHandler(handler: JHandler): JGoogleApiClient_Builder; cdecl;
    function setViewForPopups(viewForPopups: JView): JGoogleApiClient_Builder; cdecl;
    function useDefaultAccount: JGoogleApiClient_Builder; cdecl;
  end;
  TJGoogleApiClient_Builder = class(TJavaGenericImport<JGoogleApiClient_BuilderClass, JGoogleApiClient_Builder>) end;
```
You will notice that the addApi() method is commented out.  This is a required method in order to use the Google SDK on Android.   Without access to addApi() you cannot gain access to any of the various APIs through the SDK that Google provides.  Naturally one could simply recreate these interfaces ourselves or add the missing interfaces to our code.  One solution is to simply use Delphi's Java2OP utility to convert the `play-services-base.jar` again.  Because Delphi has an existing header to the interfaces of Play Services, the Java2OP utility will skip converting those interfaces unless we temporarily prevent that.  A simple solution is to temporarily rename cache.txt before calling Java2OP.

One example conversion might look like the following:
```Delphi
  JGoogleApiClient_BuilderClass2 = interface(JObjectClass)
    ['{D991053A-7C8E-4A4A-B007-8083D721C02E}']
    {class} function init(P1: JContext): JGoogleApiClient_Builder2; cdecl; overload;
    {class} function init(P1: JContext; P2: JGoogleApiClient_ConnectionCallbacks2; P3: JGoogleApiClient_OnConnectionFailedListener2): JGoogleApiClient_Builder2; cdecl; overload;
  end;

  [JavaSignature('com/google/android/gms/common/api/GoogleApiClient$Builder')]
  JGoogleApiClient_Builder2 = interface(JObject)
    ['{974868A1-60D3-4A7E-A3FF-86AF25448FD8}']
    function addApi(P1: JApi): JGoogleApiClient_Builder2; cdecl; overload;
    function addApi(P1: JApi; P2: JApiOptions_HasOptions): JGoogleApiClient_Builder2; cdecl; overload;
    function addConnectionCallbacks(P1: JGoogleApiClient_ConnectionCallbacks2): JGoogleApiClient_Builder2; cdecl;
    function addOnConnectionFailedListener(P1: JGoogleApiClient_OnConnectionFailedListener2): JGoogleApiClient_Builder2; cdecl;
    function addScope(P1: JScope): JGoogleApiClient_Builder2; cdecl;
    function build: JGoogleApiClient; cdecl;
    function enableAutoManage(P1: JFragmentActivity; P2: JGoogleApiClient_OnConnectionFailedListener2): JGoogleApiClient_Builder2; cdecl; overload;
    function enableAutoManage(P1: JFragmentActivity; P2: Integer; P3: JGoogleApiClient_OnConnectionFailedListener2): JGoogleApiClient_Builder2; cdecl; overload;
    function setAccountName(P1: JString): JGoogleApiClient_Builder2; cdecl;
    function setGravityForPopups(P1: Integer): JGoogleApiClient_Builder2; cdecl;
    function setHandler(P1: JHandler): JGoogleApiClient_Builder2; cdecl;
    function setViewForPopups(P1: JView): JGoogleApiClient_Builder2; cdecl;
    function useDefaultAccount: JGoogleApiClient_Builder2; cdecl;
  end;
  TJGoogleApiClient_Builder2 = class(TJavaGenericImport<JGoogleApiClient_BuilderClass2, JGoogleApiClient_Builder2>) end;
```
Notice we have more methods exposed now.  Also, we have renamed the base interface to something else like `JGoogleApiClient_Builder2` to avoid a naming conflict with `Androidapi.JNI.PlayServices`.  Delphi will also comment out some of the methods like addApi() so we remove those comments and also add some other missing interfaces contained in `play-services-auth.jar', so that we can make the call to the addApi() method. 
```Delphi
  JApiSigninClass = interface(JObjectClass)
    ['{79F5E010-C889-43CB-ADE4-59D458999706}']
    {class} function getName: JString; cdecl;//Deprecated
  end;

  [JavaSignature('com/google/android/gms/common/api/Api$ApiOptions$HasOptions')]
  JApiSignin = interface(JObject)
    ['{E6B1AE44-A176-4AF6-BB83-38ABD3D0787C}']
  end;

  TJApiSignin = class(TJavaGenericImport<JApiSigninClass, JApiSignin>) end;
  Japi_AuthClass = interface(JObjectClass)
    ['{29585F0D-D62C-4B30-B651-B262DC292AA3}']
    {class} function _GetGOOGLE_SIGN_IN_API: JApiSignin; cdecl;
    {class} function _GetGoogleSignInApi: JGoogleSignInApi; cdecl;
    {class} property GOOGLE_SIGN_IN_API: JApiSignin read _GetGOOGLE_SIGN_IN_API;
    {class} property GoogleSignInApi: JGoogleSignInApi read _GetGoogleSignInApi;
  end;

  [JavaSignature('com/google/android/gms/auth/api/Auth')]
  Japi_Auth = interface(JObject)
    ['{71B3F4E8-B24A-45BF-84DD-BD0C7BE53065}']
  end;
  TJapi_Auth = class(TJavaGenericImport<Japi_AuthClass, Japi_Auth>) end;
```
In this case we expose the property `GOOGLE_SIGN_IN_API` which we will need.  Some of the other Java interfaces we need to call directly include the following.
```Java
GoogleSignInOptions.Builder
GoogleSignInOptions
GoogleApiClient
GoogleApiClient.Builder
GoogleApiClient.ConnectionCallbacks
GoogleApiClient.OnConnectionFailedListener
ApiSignin
api.Auth
```

Google provides examples on their site in Java (https://developers.google.com/identity/sign-in/android/sign-in).  To get started in Java with Android Studio we would do the following,
```Java
// Configure sign-in to request the user's ID, email address, and basic
// profile. ID and basic profile are included in DEFAULT_SIGN_IN.
GoogleSignInOptions gso = new GoogleSignInOptions.Builder(GoogleSignInOptions.DEFAULT_SIGN_IN)
        .requestEmail()
        .build();
```
So a Delphi example of this might look like, 
```Delphi
var
  GoogleSignInOptionsBuilder: JGoogleSignInOptions_Builder;
  GoogleSignInOptions: JGoogleSignInOptions;
begin
  GoogleSignInOptionsBuilder := TJGoogleSignInOptions_Builder.Create;
  GoogleSignInOptions := GoogleSignInOptionsBuilder.requestId.requestProfile.requestEmail.build;
end;
```
That would work in Delphi.  The next step in Java is as follows:
```Java
// Build a GoogleApiClient with access to the Google Sign-In API and the
// options specified by gso.
mGoogleApiClient = new GoogleApiClient.Builder(this)
        .enableAutoManage(this /* FragmentActivity */, this /* OnConnectionFailedListener */)
        .addApi(Auth.GOOGLE_SIGN_IN_API, gso)
        .build();
```
So in Delphi we would do something like the following,
```Delphi
var
  GoogleApiClient: JGoogleApiClient;
  GoogleApiClientBuilder: JGoogleApiClient_Builder2;
  Api: JApiSignin;
begin
  GoogleApiClientBuilder := TJGoogleApiClient_Builder2.JavaClass.Init(TAndroidHelper.Context);
  FConnectionCallbacks := TConnectionCallbacks.Create(Self);
  GoogleApiClientBuilder.addConnectionCallbacks(FConnectionCallbacks);
  FOnConnectionFailedListener := TOnConnectionFailedListener.Create(Self);
  GoogleApiClientBuilder.addOnConnectionFailedListener(FOnConnectionFailedListener);

  Api := TJapi_Auth.JavaClass.GOOGLE_SIGN_IN_API;
  GoogleApiClientBuilder.addApi(Api);

  GoogleApiClient := GoogleApiClientBuilder.build;
  GoogleApiClient.connect;
end;
```
> The above example in Delphi skips the enableAutoManage() call and instead initiates the sign-in process manually which is what we want.  Google provides an automatic and manual way of signing in.  If we use the automatic method we need to include numerous resources and handle some complex fragment activities and signin flow which isn't really required if we handle it ourselves, so for Delphi we are following the manual sign-in approach.

This is where the problem starts.  When we attempt to call addApi() Delphi will complain about an "Invoke error, method not found".  Even though we have exposed this method through our interface, Delphi cannot find it.  If we decompile the Java inside the play-services-base.jar to examine this method, we see the following:
![](http://i.imgur.com/F6c2VU8.png) 

Notice that the call to addApi() requires a parameter that is a Generic type.  In this case we need to pass `GOOGLE_SIGN_IN_API` value from api.Auth to the method.  Delphi's JNI marshaling cannot find the associated method because of the Generic type so the method declaration is invalid and the method fails.  For Google sign-in and most other core Google Firebase SDK methods this is a huge problem.  A good portion of the Google Firebase SDK requires access to methods that will use Generic types as parameters and results.

It would be nice to take this approach, but we have to abandon this effort and reconsider our strategy.

> By the way, we use and recommend an excellent tool called JD-GUI to decompile and examine JARs, that you can find here http://jd.benow.ca.  We highly recommend it for learning about the internals of the JARs.

# Creating our Google-Helper in Java
In order to perform Google sign-in with Delphi we need to construct a helper library to assist the sign-in process.  A good helper library would not only handle the sign-in interaction, but would also allow for silent sign-in in the event the user has already authorized our application and provided their credentials.

Additionally, a good helper would assist us in logging out, revoking access and other basic chores of managing authentication.  In the case of Google sign-in, it would be able to call back to our Delphi application and provide details about the user, such as email address.  We should be able to provide an array of [Google OAuth 2.0 Scopes](https://developers.google.com/identity/protocols/googlescopes) to our helper so we can expand the list of APIs we want to access and use, easily.  In addition to calling Google APIs from the client, the helper should also allow us to make Google API calls from the [server-side by accessing Google's server auth codes](https://developers.google.com/identity/sign-in/web/server-side-flow) for server-side apps so we can offload API work if we choose.

We have created a [Java project that builds the google-helper.jar](https://github.com/grijjy/DelphiGoogleSignIn/tree/master/GoogleHelper) to perform all of these tasks.  This Java project is contained in our repository.  We also include the pre-built [google-helper.jar library in the our repository](https://github.com/grijjy/DelphiGoogleSignIn/tree/master/Libraries).

## Initializing Google SignIn
The first method in our Google sign-in helper class is `initSignIn()`.  This method takes the context and a callback from the Delphi caller, the clientId for our Firebase project, a Delphi array of Google OAuth 2.0 Scopes and a boolean indicating if we want Google server auth codes (more on this topic later). 

```Java
    private GoogleApiClient.Builder rootGoogleApiClientBuilder;
    private GoogleApiClient rootGoogleApiClient;
    private GoogleSignInOptions.Builder signInOptionsBuilder;
    private GoogleSignInOptions signInOptions;
    private GoogleSignInCallback signInCallback;
    private GoogleSignInAccount signInAccount;


    public boolean initSignIn(Context context, GoogleSignInCallback callback, String clientId, String[] scopes, boolean requestServerAuthCode)
    {
        signInCallback = callback;

        signInOptionsBuilder =  new GoogleSignInOptions.Builder(GoogleSignInOptions.DEFAULT_SIGN_IN);
        if (signInOptionsBuilder != null) {
            for (String s : scopes) {
                signInOptionsBuilder.requestScopes(new Scope(s));
            }
            if (requestServerAuthCode) {
                signInOptionsBuilder.requestServerAuthCode(clientId, false);
            }
            signInOptionsBuilder.requestIdToken(clientId);
            signInOptionsBuilder.requestEmail();
            signInOptions = signInOptionsBuilder.build();
            if (signInOptions != null) {

                rootGoogleApiClientBuilder = new GoogleApiClient.Builder(context);
                if (rootGoogleApiClientBuilder != null) {
                    rootGoogleApiClientBuilder.addApi(Auth.GOOGLE_SIGN_IN_API, signInOptions);
                    rootGoogleApiClientBuilder.addOnConnectionFailedListener(new GoogleApiClient.OnConnectionFailedListener() {
                        @Override
                        public void onConnectionFailed(@NonNull ConnectionResult connectionResult) {
                            signInCallback.onFailure(connectionResult.getErrorCode(), connectionResult.getErrorMessage());
                        }
                    });
                    rootGoogleApiClient = rootGoogleApiClientBuilder.build();
                    if (rootGoogleApiClient != null) {
                        rootGoogleApiClient.connect();
                        return true;
                    } else {
                        return false;
                    }
                } else {
                    return false;
                }
            } else {
                return false;
            }
        } else {
            return false;
        }
    }
```
So from Delphi we could call this method like this,
```Delphi
var
  Scopes: TJavaObjectArray<JString>;
  I: Integer;
begin
  FGoogleSignInHelper := TJGoogleSignInHelper.JavaClass.init;
  if FGoogleSignInHelper <> nil then
  begin
    Scopes := TJavaObjectArray<JString>.Create(Length(AScopes));
    try
      for I := 0 to Length(AScopes) - 1 do
        Scopes[I] := StringToJString(AScopes[I]);
      FInitialized := FGoogleSignInHelper.initSignIn(
        TAndroidHelper.Activity,
        FGoogleSignInCallback, { our callback }
        StringToJString(FServerClientId),
        Scopes,
        FRequestServerAuthCode)
    finally
      Scopes.Free;
    end;
  end;
end;
```
In Delphi `TAndroidHelper.Activity` represents our context.  `FGoogleSignInCallback` is of type `TGoogleSignInCallback = class(TJavaLocal, JGoogleSignInCallback)`.  Our FServerClientId is the "web client" id (also known as the "client_type": 3 from the google-services.json) from our list of OAuth/2 Client Ids.  Scopes is a Java Object Array of strings, representing the Google OAuth 2.0 Scopes you want access.   

## Sign-in to Google
To start the SignIn flow in Java we simply request a SignInIntent from the Google SignIn API and call StartActivityForResult.  The intent however, has to be started from within the MainActivity, so we initiate a callback to our Delphi Firemonkey application so it can call StartActivityForResult within it's own context.
```Java
Intent signInIntent = Auth.GoogleSignInApi.getSignInIntent(rootGoogleApiClient);
signInCallback.StartActivityForResult(signInIntent, RC_SIGN_IN);
```
Delphi receives the StartActivityForResult callback and initiates it,
```Delphi
procedure TgoGoogleSDK.TGoogleSignInCallback.StartActivityForResult(
  P1: JIntent; P2: Integer);
begin
  { callback to trigger intent within the main firemonkey activity }
  MainActivity.startActivityForResult(P1, P2);
end;
```
This also means the ActivityResult message will be handled by our Delphi Firemonkey application, but the Google SignIn SDK requires the SignInResult be returned from the Activity.  To accomplish this we implement a HandleActivityMessage in Delphi that sends the information back to our Java class.
```Delphi
procedure TgoGoogleSDK.HandleActivityMessage(const Sender: TObject; const M: TMessage);
var
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
    end;
  end;
end;
``` 
## Silent sign-in to Google
In addition to the regular SignIn method we created in our helper, there is also a version called SignInSilently.  Google has a rather complicated process to determine whether a user needs to sign-in or not and we have implemented that entire process in the SignInSilently method.  
```Java
    public void signInSilently() {
        OptionalPendingResult<GoogleSignInResult> pendingResult = Auth.GoogleSignInApi.silentSignIn(rootGoogleApiClient);

        if (pendingResult != null) {
            if (pendingResult.isDone()) {
                // user credentials are cached already
                GoogleSignInResult signInResult = pendingResult.get();
                signInCompleted(signInResult);
            } else {
                pendingResult.setResultCallback(new ResultCallback<GoogleSignInResult>() {
                    @Override
                    public void onResult(@NonNull GoogleSignInResult signInResult) {
                        if (signInResult.isSuccess()) {
                            signInCompleted(signInResult);
                        } else {
                            // start sign-in interface flow
                            Intent signInIntent = Auth.GoogleSignInApi.getSignInIntent(rootGoogleApiClient);
                            signInCallback.StartActivityForResult(signInIntent, RC_SIGN_IN);
                        }
                    }
                });
            }
        }
    }
```
The method checks for existing credentials and if they don't exist, it will attempt a normal sign-in authentication flow.

# TgoGoogleSDK class
To simplify the process of sign-in for your Delphi application, we created the TgoGoogleSDK class.  This class is constructed to follow the same public interface for all platforms (Windows, iOS and Android) for Google SignIn. 
```Delphi
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
```
## Getting started with TgoGoogleSDK class
To create the TgoGoogleSDK class for Android devices, you only need your client id.  This is the "web client" id (also known as the "client_type": 3 from the google-services.json) from our list of OAuth/2 Client Ids.

You also need to determine the Google OAuth 2.0 Scopes you require.  The [Google OAuth 2.0 Scopes](https://developers.google.com/identity/protocols/googlescopes) determine the APIs that you will be allowed to use once the user of your app has consented.  This should match the APIs you have enabled in the Google Developer's Console, API Manager.

To create the TgoGoogleSDK object,
```Delphi
    FGoogleSDK := TgoGoogleSDK.Create(
	  '', // not used on Android
      TOKEN_TYPE_FIREBASE, // always Firebase tokens on Android
      <ServerClientId>,
      True);
```
And to start the SignIn flow,
```Delphi
  FGoogleSDK.SignIn([ // profile and email scopes are implied
    'https://www.googleapis.com/auth/plus.me',
    'https://www.googleapis.com/auth/plus.circles.read',
    'https://www.googleapis.com/auth/youtube.readonly'
    ]);
```
The GoogleSignIn process is a non-blocking operation and the result from sign-in occurs in another thread.  To receive the result, you need to create a TMessage listener in Delphi.  Consider the following example,
```Delphi
procedure TFormMain.FormCreate(Sender: TObject);
begin
  TMessageManager.DefaultManager.SubscribeToMessage(TGoogleSignInResultMessage, GoogleSignInResultMessageListener);
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  TMessageManager.DefaultManager.Unsubscribe(TGoogleSignInResultMessage, GoogleSignInResultMessageListener);
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
  end
  else
  if GoogleSignInResult.Cancelled then
    ShowMessage('Cancelled')
  else
    ShowMessage(Format('Failed (Error=%d, %s)', [GoogleSignInResult.Error, GoogleSignInResult.ErrorDesc]));
end;
```
Here we subscribe to the message and when the GoogleSignInResultMessageListener is called, it will indicate whether there was success or failure.  

# Rebuilding the Google-Helper.jar in Android Studio
If you want to rebuild the `google-helper.jar` yourself, you will need to start by installing Android Studio.  
> Our helper uses the Android SDK Platform 25 and the build tools 23.0.1 for better compatibility with existing Delphi Android apps.  It targets compatibility with SDK version 23 and we use dependencies for 10.2.1 of the various support libraries.

Once you have installed Android Studio and opened the Google Helper project, you will be prompted to install,
1. Android SDK Platform 25
2. Android SDK Build-Tools 23.0.1

You will also need to install the Google Repository and Android Support Repository.

Once all the supporting modules are installed, you can Rebuild the project.  If the rebuild is successful you need to locate the .aar output file, usually under
\GoogleHelper\app\build\outputs\aar\app-debug.aar and unzip it.  The classes.jar file contained here is your rebuilt google-helper.jar.

> If you modify an existing library for your Delphi project, you need to use the 'Clean' option before compilation and running or Delphi will not always pick up changes you make.

# Getting Started Quick Steps
If you want to make the changes required to an existing project to add Google SignIn and you don't care about how all this works, you can follow these quick steps.

1.  Setup your Firebase project using the steps, in the section above called 'Getting Started with the Google SignIn SDK'.

2.  Add the \DelphiGoogleSignIn\Resources\Android\res to the Delphi Project Deployment list with a target of .\res with all subfolders.  These resources are static and do not change for different Google SignIn projects.  We specially converted anything that is project specific into parameters instead of using constants in resource files.  Since there are numerous resources we would suggest you use our [utility DeployMan](https://github.com/grijjy/DelphiSocialFrameworks/tree/master/DeployMan) which allows you to modify the Delphi .DProj file to easily add resources in bulk.  if you intend to use this utility, [you should read about it in one of our other blog articles](https://github.com/grijjy/DelphiSocialFrameworks/blob/master/Android.md#add-the-resources-to-the-delphi-deployment-manager-for-your-android-project). 

3.  Add meta data to the AndroidManifest.template.xml under the section `<%application-meta-data%>`:
```xml  
  <meta-data android:name="com.google.android.gms.version" android:value="@integer/google_play_services_version"/>
```

4.  Add an activity to the AndroidManifest.template.xml:
```xml
<activity android:name="com.google.android.gms.auth.api.signin.internal.SignInHubActivity" android:screenOrientation="portrait" android:windowSoftInputMode="stateAlwaysHidden|adjustPan" />
```

5. Disable the existing libraries google-play-*.jars, cloud-messaging.dex.jar, google-analytics-v2.dex.jar and android-support-v4.dex.jar in your Delphi Project under Android, Libraries.

6.  Add all the libraries under \DelphiGoogleSignIn\Libraries\Android to your Delphi project.  
`google-helper.jar`
`appcompat-v7.jar`
`firebase-*.jar` (3 of them)
`play-services-*.jar` (5 of them)
`support-*.jar` (4 of them)

7.  At this point you only need your "web client" id and the TgoGoogleSDK class as demonstrated in the Example GoogleSignIn project to SignIn to Google.

# The lifecycle of making Google API calls
As we discussed earlier, in a perfect world Delphi would consume all the Firebase SDKs into Java interfaces and could call them directly.  Fortunately Google provides REST/HTTP endpoints for their Firebase related APIs.  You can use these APIs from not just Android and iOS mobile apps, but Windows, web applications and server-side processes.

From the user's perspective, once they have signed in and you have a valid Firebase Id token, you can make calls directly or on their behalf to the HTTP/REST endpoints and use the various Firebase related APIs without disrupting the user of your app.

The primary problem though, is that the HTTP/REST endpoints require an OAuth/2 token and most of them do not operate with a Firebase Id token, although there are some rare exceptions that work with either token type.  Because of this requirement, we need to obtain an OAuth/2 token using our Firebase Id token.

Sounds straightforward, right?  Well it isn't.

## Lifecycle summary
The lifecycle of the Google sign-in process typically follows:

1.  A user signs into your app and receives a 1) UserId, 2) Email, 3) Firebase Id token and 4) Authentication code.
2.  Your app passes these values to your server-side process to authenticate the user.
3.  Your server-side process verifies the Firebase Id token.
4.  Your server-side process compares the resulting UserId and Email from the verification to the one supplied over the network from your client to see if they match.
5.  Your server-side process examines the expiration time for the Firebase Id token to make sure it hasn't expired.
6.  Your server-side process takes the Firebase Id token and the Authentication code and exchanges it for a Refresh token.
7.  You keep the Refresh token on your backend because you will never receive it again after the first time it is sent to you.
8.  You use the Refresh token to request an OAuth/2 token, usually updated every hour.
9.  Your server-side process uses the OAuth/2 token to make Google API calls.
 
The next sections will explore these steps in more detail.

## Verifying tokens
If your app interacts with a server-side backend, you are going to need to validate tokens.  Since you are using automatic authentication without a password, the token itself becomes the authority.  You cannot rely on a Google user id or email address provided by a client and sent over the network to validate a user.  Instead you must take the token and validate it.  The process of validation provides you the Google user id and email address of the user.

Since the Firebase Id token is simply a [Java Web Token](https://jwt.io/), we could use any commercial library or even OpenSSL APIs to decode it and verify it ourselves.  Google [discusses this approach in an article](https://developers.google.com/identity/sign-in/web/backend-auth).

However, we can also use an HTTP/REST endpoint provided by Google to verify it.  Our example application shows the process of calling the HTTP tokeninfo endpoint to verify a token.  It takes care of the validation so we don't have to also load PEM certificates and perform various other steps to validate it ourselves.  

Once you receive the result from the tokeninfo endpoint, you still need to make sure that what you received matches your expectation and that it is not expired.

## Server authorization codes and Refresh tokens
When you initiate Google SignIn in your app, you can request a server authentication code.  This value is sent along with your Firebase Id token to the token endpoint along with a request to grant_type of authorization_code to obtain a Refresh token.

Google will only provide the Refresh token a single time, all subsequent requests will result in a 'null' response from the API.  Therefore it is very important you retain the Refresh token on your backend.  

The Refresh token is only provided the first-time an Authentication code is used after the user of your app has consented to any scopes that are needed.  

![](http://i.imgur.com/W9Hh6yH.png)

If you loose the token for some reason, you must revoke it and the user will be prompted to consent again.  The token you must revoke is the special access_token value that is returned from the call to the token_info endpoint.

## Obtaining an OAuth/2 token
Now that you have a valid Refresh token, you simply call the token endpoint with a grant_type of refresh_token to request an updated OAuth/2 access token.

This is the token you will use to make Google API calls.

Google tokens typically expire after one hour, so you will need to use the Refresh token each hour to refresh the OAuth/2 token to continue making API calls.  An easy way in Delphi to determine whether you need to refresh the token is to compare the expiration time in the token header to current adjusted system clock,

```Delphi
DateTimeToUnix(TTimeZone.Local.ToUniversalTime(Now))
``` 
If your server-side instance is running from a reasonably time synchronized environment, like we see at Google's Cloud in the Compute Engine or Amazon's Web Services, you are pretty safe that it is accurate.  However, you should always refresh token before the expiration to give the API request and all other processes ample time to finish.

# TGoogleApi class
We created a basic helper unit called TGoogleApi to simplify all of the above mentioned steps and make any Google API call.
```Delphi
  TGoogleApi = class
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

```
This class could be used on any Delphi supported platform including Windows, Linux, iOS and Android.  

This class can be used server-side by providing the Client Id and Client Secret during construction.  This class can be used to clients by setting the property AccessToken and providing the Client Id (not the Client Secret) before making API calls. Some of the Google APIs do not require Client Id, Client Secret or Access Tokens.  In those cases you can use this class as well.

The example project GoogleSignIn in our repository demonstrates all the capabilities of this class.

# Example Project GoogleSignIn
The example project included in the repository is designed to demonstrate the entire lifecycle of handling Google SignIn, from initial sign-in all the way to making API calls.

There are 5 tabs in the example project:  
1.  Sign-in
2.  Token verification and revoking tokens
3.  Obtaining a refresh token from a server auth code
4.  Obtaining an OAuth/2 access token from a refresh token
5.  Making API calls with the OAuth/2 token

In order to fully use the example project you will need your "Client Id", which is your web client id from the Google Developers Console.  You will also need your "Client Secret" from the Google Developers Console.
> **The Client Secret should never be included in your client app**.  Most Google APIs do not need it unless those APIs are intended to be executed from the server-side.  For the purposes of this example we show the entire lifecycle of the Google SignIn process so it is included for demonstration purposes. 

In order for the example application to make API calls to the People API and the YouTube API you must enable those APIs in the Google Developers Console, API Manager.  This step is required in addition to requesting the scopes.

To run the demo, simply paste your "Client Id" and "Client Secret" into the respective edits and click the SignIn button.  

![](http://i.imgur.com/h8Ddxmg.png)

If we have done everything correctly, including setting up your project in Firebase you should be able to sign-in.  You will be given 4 pieces of information:

1.  **User Id** (user id of the Google user)
2.  **Firebase Id Token** (a Firebase Id token for the user)
3.  **AuthCode** (a Server auth code used for server-side API calls)
4.  **Email** (the email address of the Google user).

On the AuthCode tab we can obtain a Refresh token by clicking Grant Refresh token.

![](http://i.imgur.com/79byiaN.png)

On the OAuth2 tab we can use the Refresh token to obtain an OAuth/2 token.

![](http://i.imgur.com/cymCCZy.png)

And finally, we can use the OAuth/2 token to make Google API calls.

![](http://i.imgur.com/RunEm78.png)

# Troubleshooting problems with Google SignIn
If you are receiving errors such as INVALID_AUDIENCE or DEVELOPER_ERROR from Google SignIn when you run the example project or your own project you need to carefully verify that you didn't make any of the common mistakes:

1.  **SHA1 hash is missing or incorrect**.  Make sure you provided the correct SHA1 hash to the Firebase console for the debug.keystore for Debug configuration apps or the application keystore for Release apps.  Make sure you are grabbing the SHA1 hash from the correct installed Delphi version folder.
2.  **SHA1 hash is associated with the wrong Firebase project**.  Make sure you have selected the correct project in the Firebase console when you apply the SHA1 hash.
3.  **The package name associated with the Firebase project does not match the Delphi project package name**.  Check your finalized `AndroidManifest.xml` and compare the package name to one in the Firebase console for your project.  If they don't match you will need to add a new Android App option to your project with the correct name (or correct the Delphi package name) and recreate your google-services.json.  
4.  **The Client Id you are passing to the TgoGoogle constructor is the wrong one**.  This is common mistake as it IS NOT the Android Client Id, as one might expect for an Android application, but it is instead the server-side OAuth/2 id we must use.  This client id is also called the "web client" id in the Google Credentials area of the console (https://console.developers.google.com/apis/credentials) or client_type "3" in your google-services.json.
5.  **Google APIs are not enabled**.  You must enable the various APIs for Google SignIn to operate and the scopes you requested to work.  Google SignIn requires the Google+ API and Google People API. YouTube Data API must be enabled for YouTube calls.  These can all be found in the API Manager of Google Developers Console.
6.  **The authorized redirect URIs for the web client id is missing or incorrect**.  If your actual APIs fail to work correctly it may be because the redirect URI is not specified.  It must match the constant value 'http://localhost/oauth2callback' for our example.

# Conclusion
We appreciate you following us on the exploration and journey of using Google's SDKs for SignIn and accessing the Google APIs.  We hope you find this information useful for your upcoming projects.  Feel free to reach out to us if you have any questions about how to integrate these ideas into your own apps.  

# License
TgrGoogleSDK, TGoogleApi, GoogleHelper along with related units, classes and examples are licensed under the Simplified BSD License. See License.txt for details.