//Function that logs user out, called from auto running function
function userLoggedOut() {
    eraseCookie('isLoggedin');
    gigya.accounts.logout();
    //location.reload();
    window.location = window.location.href.split("?")[0]; //+ '?eraseCachonlogoute=true';

}

//Auto running javascipt that prints either 'Login' or users firstname on login menu
(function loginType() {
    function isLoggedIn(response) {
        var userLoginText = document.getElementById("loginText");
        var userLoginTextMobile = document.getElementById("loginTextMobile");
        var userLoginText2 = document.getElementById("loginText2");
        var changeUserIcon = document.getElementById("userLoginIcon");
        var logout = document.getElementById("logoutButton");
        var logoutMobile = document.getElementById("logoutButtonMobile");

        if (response.errorCode == 0) {
            createCookie('isLoggedin', 'True', 7);

            var profile = response['profile'];
            userLoginText.innerHTML = '<a href="/profile/" class="loginClass">Profile</a>';
            userLoginTextMobile.innerHTML = '<a href="/profile/" class="loginClassMobile">Profile</a>';
            var icn = profile['thumbnailURL'];
            logout.innerHTML = '<a onclick="userLoggedOut()" class="loginClass">Logout</a>';
            logoutMobile.innerHTML = '<a onclick="userLoggedOut()" class="loginClassMobile">Logout</a>';

            //userLoginText2.innerHTML = '<ul id="loggedInNav"><li><a href="/profile/" class="loginClass2">- Profile</a></li><li><a onclick="userLoggedOut()" class="loginClass2">- Logout</a></li></ul>';

            if (icn == "http://dev.irishexaminer.com/images/social/icon-user-you.png") {
                gigya.accounts.setAccountInfo({ profile: { thumbnailURL: 'https://irishexaminer.com/images/social/icon-user-you.png' } });
                changeUserIcon.innerHTML = '<img id="login-user-icon2" src="/images/social/icon-user-you.png" />';
            }
            else if(icn != undefined){
                changeUserIcon.innerHTML = '<img id="login-user-icon2" src="' + icn + '" />';
            } else {
                gigya.accounts.setAccountInfo({profile:{thumbnailURL:'https://irishexaminer.com/images/social/icon-user-you.png'}});
            }
        } else {
            //[].forEach.call(document.querySelectorAll('.ctx_content'), function (e) {
            //    //e.parentNode.removeChild(e);
            //    e.parentNode.innerHTML = "This content is blocked.";
            //});
            userLoginText.innerHTML = '<a onclick="toDo()" class="loginClass">Login</a>';
            userLoginTextMobile.innerHTML = '<a onclick="toDo()" class="loginClassMobile">Login</a>';

            //userLoginText2.innerHTML = '<ul><li><a onclick="toDo()" class="mobileLoginClass">- Site Login</a></li></ul>';

            logout.innerHTML = '';
            logoutMobile.innerHTML = '';
            changeUserIcon.innerHTML = '<img id="login-user-icon" src="/images/social/icon-user-you.png" />';
        }
    }
    var logParams = { callback: isLoggedIn };
    gigya.accounts.getAccountInfo(logParams);
})()


function toDo() {
    function isLoggedIn(response) {
        if (response.errorCode == 0) {
            window.location = "/profile/";
        } else {
            //gigya.accounts.showScreenSet({ screenSet: 'Default-RegistrationLogin', mobileScreenSet: 'DefaultMobile-RegistrationLogin' });
            gigya.accounts.showScreenSet({ screenSet: 'default-Login' });

        }
    }
    var logParams = { callback: isLoggedIn };
    gigya.accounts.getAccountInfo(logParams);
    function loginEventHandler(eventObj) {
        //location.reload();
        createCookie('isLoggedin', 'True', 7);
        var domain = url_domain(window.location.href);
        window.open("http://" + domain + "/loginredirect.aspx?page=" + window.location.href,'_self');

    }
    gigya.accounts.addEventHandlers({onLogin: loginEventHandler});
}

//Login via story prompt
function showLoginScreenset() {
    function isLoggedIn(response) {
        // If user is not logged in, display login box
        if (response.errorCode != 0) {
            gigya.accounts.showScreenSet({
                screenSet: 'default-Login'
            });
        }
    }

    // On login create cookie 'userLoggedIn' and set to True, expires in 1 week
    function loginEventHandler(eventObj) {
        createCookie('isLoggedin', 'True', 7);
        var domain = url_domain(window.location.href);
        window.open("http://" + domain + "/loginredirect.aspx?page=" + window.location.href, '_self');

    }

    var logParams = { callback: isLoggedIn };
    gigya.accounts.getAccountInfo(logParams);

    gigya.accounts.addEventHandlers({ onLogin: loginEventHandler });
}

//Register via story prompt
function showRegistrationScreenset() {
    gigya.accounts.showScreenSet({
        screenSet: 'default-Register'
    });
}


function url_domain(data) {
    var a = document.createElement('a');
    a.href = data;
    return a.hostname;
}

function createCookie(name, value, days) {
    if (days) {
        var date = new Date();
        date.setTime(date.getTime() + (days * 24 * 60 * 60 * 1000));
        var expires = "; expires=" + date.toGMTString();
    }
    else var expires = "";
    document.cookie = name + "=" + value + expires + "; path=/";
}

function readCookie(name) {
    var nameEQ = name + "=";
    var ca = document.cookie.split(';');
    for (var i = 0; i < ca.length; i++) {
        var c = ca[i];
        while (c.charAt(0) == ' ') c = c.substring(1, c.length);
        if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length, c.length);
    }
    return null;
}

function eraseCookie(name) {
    createCookie(name, "", -1);
}

function renderLoginCompForm(formID, first, last, email, userID) {
    function isLoggedIn(response) {
        if (response.errorCode == 0) {
            // store user profile data and form info
            var profile = response['profile'];
        
            // Render and auto-populate the form
            var competitionForm;
            (function (d, t) {
                var s = d.createElement(t),
                    options = {
                        'userName': 'irishexaminer',
                        'formHash': formID,
                        'defaultValues': 
                            first + "=" + profile['firstName'] + "&" +
                            last + "=" + profile['lastName'] + "&" +
                            email + "=" + profile['email'] + "&" +
                            userID + "=" + response['UID'],
                        'autoResize': true,
                        'height': '453',
                        'async': true,
                        'host': 'wufoo.com',
                        'header': 'show',
                        'ssl': true
                    };
                s.src = ('https:' == d.location.protocol ? 'https://' : 'http://') + 'www.wufoo.com/scripts/embed/form.js';
                s.onload = s.onreadystatechange = function () {
                    var rs = this.readyState;
                    if (rs) if (rs != 'complete') if (rs != 'loaded') return;
                        try {
                            competitionForm = new WufooForm();
                            competitionForm.initialize(options);
                            competitionForm.display();
                        } catch (e) { }
                    };
                    var scr = d.getElementsByTagName(t)[0],
                        par = scr.parentNode;
                    par.insertBefore(s, scr);
                })(document, 'script');
            }
        }

    var logParams = { callback: isLoggedIn };
    gigya.accounts.getAccountInfo(logParams);

    // On login create cookie 'isLoggedIn' and set to True, expires in 1 week
    function loginEventHandler(eventObj) {
        //location.reload();
        createCookie('isLoggedin', 'True', 7);
        var domain = url_domain(window.location.href);
        window.open("http://" + domain + "/loginredirect.aspx?page=" + window.location.href, '_self');
    }

    gigya.accounts.addEventHandlers({ onLogin: loginEventHandler });
}