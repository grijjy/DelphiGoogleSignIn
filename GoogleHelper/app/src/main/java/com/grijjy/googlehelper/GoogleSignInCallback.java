package com.grijjy.googlehelper;

import android.content.Intent;

public interface GoogleSignInCallback {
    public void StartActivityForResult(Intent intent, int requestCode);

    public void onSuccess(String emailAddress, String displayName, String id, String idToken, String authCode);

    public void onFailure(int errorCode, String errorDesc);
}