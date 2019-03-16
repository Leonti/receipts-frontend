var webAuth = new auth0.WebAuth({
  domain: 'leonti.au.auth0.com',
  clientID: 'Q4xpr2NkkxykaSde6Bgw1pgYdX9yaBMO',
  responseType: 'token id_token',
  scope: 'openid email',
  audience: 'receipts-backend',
  redirectUri: window.location.href
});

function handleAuthentication() {

  console.log(location.href)

  webAuth.parseHash(function(err, authResult) {
    if (authResult && authResult.accessToken && authResult.idToken) {
      window.location.hash = '';
      setSession(authResult);
    } else if (err) {
      console.log(JSON.stringify(err));
      alert(
        'Error: ' + err.error + '. Check the console for further details.'
      );
    }
  });
}

function setSession(authResult) {
  var expiresAt = JSON.stringify(
    authResult.expiresIn * 1000 + new Date().getTime()
  );
  localStorage.setItem('access_token', authResult.accessToken);
  localStorage.setItem('id_token', authResult.idToken);
  localStorage.setItem('expires_at', expiresAt);
}

function logout() {
  localStorage.removeItem('access_token');
  localStorage.removeItem('id_token');
  localStorage.removeItem('expires_at');
}

function isAuthenticated() {
  var expiresAt = JSON.parse(localStorage.getItem('expires_at'));
  return new Date().getTime() < expiresAt;
}
/*
main.ports.isLoggedIn.subscribe(function() {
  return isAuthenticated();
})

main.ports.login.subscribe(function() {
  webAuth.authorize();
})

main.ports.logout.subscribe(function() {
  logout();
})
*/
window.addEventListener('load', function() {
  console.log('loaded');

  if (location.hash.indexOf('access_token') !== -1) {
    handleAuthentication();
  } else if (!isAuthenticated()) {
    webAuth.authorize();
  }
})
