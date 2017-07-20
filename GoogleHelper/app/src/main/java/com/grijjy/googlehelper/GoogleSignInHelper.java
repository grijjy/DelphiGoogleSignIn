package com.grijjy.googlehelper;

import android.content.Context;
import android.content.Intent;
import android.support.annotation.Nullable;
import android.util.Log;
import android.support.annotation.NonNull;

import com.google.android.gms.auth.api.Auth;
import com.google.android.gms.auth.api.signin.GoogleSignInAccount;
import com.google.android.gms.auth.api.signin.GoogleSignInOptions;
import com.google.android.gms.auth.api.signin.GoogleSignInResult;
import com.google.android.gms.auth.api.signin.GoogleSignInStatusCodes;
import com.google.android.gms.common.ConnectionResult;
import com.google.android.gms.common.api.GoogleApiClient;
import com.google.android.gms.common.api.OptionalPendingResult;
import com.google.android.gms.common.api.ResultCallback;
import com.google.android.gms.common.api.Scope;
import com.google.android.gms.common.api.Status;

public class GoogleSignInHelper {

    private static final int RC_SIGN_IN = 9001;

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
                //Log.d("!!!SCOPE", s);
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
//                    rootGoogleApiClientBuilder.addConnectionCallbacks(new GoogleApiClient.ConnectionCallbacks() {
//                        @Override
//                        public void onConnected(@Nullable Bundle bundle) {
//                            Log.d("!!!", "onConnected");
//                        }
//
//                        @Override
//                        public void onConnectionSuspended(int i) {
//                            Log.d("!!!", "onConnectionSuspended");
//                        }
//                    });
                    rootGoogleApiClientBuilder.addOnConnectionFailedListener(new GoogleApiClient.OnConnectionFailedListener() {
                        @Override
                        public void onConnectionFailed(@NonNull ConnectionResult connectionResult) {
//                            Log.d("!!!", "onConnectionFailed");
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

    private void signInCompleted(GoogleSignInResult signInResult) {
//        Log.d("!!!", "signInCompleted");
        if (signInResult != null) {
            if (signInResult.isSuccess()) {
                signInAccount = signInResult.getSignInAccount();
                if (signInAccount != null) {
                    String emailAddress = signInAccount.getEmail();
                    String displayName = signInAccount.getDisplayName();
                    String id = signInAccount.getId();
                    String idToken = signInAccount.getIdToken();
                    String authCode = signInAccount.getServerAuthCode();
//                    Log.d("!!!", "idToken = " + idToken);
//                    Log.d("!!!", "emailAddress = " + emailAddress);
//                    Log.d("!!!", "displayName = " + displayName);
//                    Log.d("!!!", "authCode = " + authCode);
                    signInCallback.onSuccess(emailAddress, displayName, id, idToken, authCode);
                }
            } else {
                int statusCode = signInResult.getStatus().getStatusCode();
//                Log.d("!!!", "Signed out " + GoogleSignInStatusCodes.getStatusCodeString(statusCode));
                signInCallback.onFailure(statusCode, GoogleSignInStatusCodes.getStatusCodeString(statusCode));
            }
        }
    }

    public void signInSilently() {
        OptionalPendingResult<GoogleSignInResult> pendingResult = Auth.GoogleSignInApi.silentSignIn(rootGoogleApiClient);

        if (pendingResult != null) {
            if (pendingResult.isDone()) {
                // user credentials are cached already
//                Log.d("!!!", "pendingResult");
                GoogleSignInResult signInResult = pendingResult.get();
                signInCompleted(signInResult);
            } else {
                pendingResult.setResultCallback(new ResultCallback<GoogleSignInResult>() {
                    @Override
                    public void onResult(@NonNull GoogleSignInResult signInResult) {
//                        Log.d("!!!", "GoogleSignInResult");
                        if (signInResult.isSuccess()) {
//                            Log.d("!!!", "GoogleSignInResult SUCCESS");
                            signInCompleted(signInResult);
                        } else {
//                            Log.d("!!!", "GoogleSignInResult FAILED");
                            // start sign-in interface flow
                            Intent signInIntent = Auth.GoogleSignInApi.getSignInIntent(rootGoogleApiClient);
                            signInCallback.StartActivityForResult(signInIntent, RC_SIGN_IN);
                        }
                    }
                });
            }
        }
    }

    public void signIn() {
        // start sign-in interface flow
        Intent signInIntent = Auth.GoogleSignInApi.getSignInIntent(rootGoogleApiClient);
        signInCallback.StartActivityForResult(signInIntent, RC_SIGN_IN);
    }

    public void HandleActivityResult(int requestCode, int resultCode, Intent data) {
        if (requestCode == RC_SIGN_IN) {
//            Log.d("!!!", "RC_SIGN_IN");
            GoogleSignInResult signInResult = Auth.GoogleSignInApi.getSignInResultFromIntent(data);
            signInCompleted(signInResult);
        }
    }

    public void signOut() {
        Auth.GoogleSignInApi.signOut(rootGoogleApiClient).setResultCallback(
                new ResultCallback<Status>() {
                    @Override
                    public void onResult(Status status) {
                    }
                });
        if (rootGoogleApiClient != null)
            if (rootGoogleApiClient.isConnected()) {
                rootGoogleApiClient.disconnect();
            }
    }

    public void revokeAccess() {
        Auth.GoogleSignInApi.revokeAccess(rootGoogleApiClient).setResultCallback(
                new ResultCallback<Status>() {
                    @Override
                    public void onResult(Status status) {
                    }
                });
    }

    public String getIdToken() {
        if (signInAccount != null) {
            return signInAccount.getIdToken().toString();
        } else {
            return null;
        }
    }

    public String getId() {
        if (signInAccount != null) {
            return signInAccount.getId();
        } else {
            return null;
        }
    }
}