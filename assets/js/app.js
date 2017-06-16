// Brunch automatically concatenates all files in your
// watched paths. Those paths can be configured at
// config.paths.watched in "brunch-config.js".
//
// However, those files will only be executed if
// explicitly imported. The only exception are files
// in vendor, which are never wrapped in imports and
// therefore are always executed.

// Import dependencies
//
// If you no longer want to use a dependency, remember
// to also remove its path from "config.paths.watched".


// Import local files
//
// Local files can be imported directly using relative
// paths "./socket" or full ones "web/static/js/socket".


// import socket from "./socket"
// require('../css/app.css');
var Elm = require('./main.js');

var root = document.getElementById('root');
var myElmApp = Elm.App.embed(root);
var localStoragePorts = require("elm-localstorage-ports");
localStoragePorts.register(myElmApp.ports);

myElmApp.ports.getToken.subscribe(function(scopes) {
    console.log("Requested token for scopes: ", scopes);
    myElmApp.ports.onToken.send("Not Retrieved");
});
myElmApp.ports.logout.subscribe(function() {
   console.log("Logging out of google session");
    gapi.auth2.getAuthInstance().signOut();
});

window.onSignIn = function(googleUser) {
    var id_token = googleUser.getAuthResponse().id_token;
    var profile = googleUser.getBasicProfile();

    var data = {
        idToken: id_token,
        name: profile.getName(),
        email: profile.getEmail(),
        avatarUrl: profile.getImageUrl()
    };

    console.log("User Data: ",  data);
    myElmApp.ports.onLogin.send(data);
};