function deleteProfilePic() {
    function changeImage(response) {
        if (response.errorCode == 0) {
            var changeImg = document.getElementById('userLoginIcon');
            changeImg.innerHTML = '<img alt="" id="login-user-icon" src="/images/social/icon-user-you.png" />';
        }
    }

    var params = {
        profile: { thumbnailURL: 'http://www.irishexaminer.com/images/social/icon-user-you.png' },
        callback: changeImage
    }
    gigya.accounts.setAccountInfo(params);
}

var connect_params = {
    showTermsLink: 'false',
    hideGigyaLink: 'true',
    showEditLink: 'true',
    height: 70,
    width: 310,
    containerID: 'linkAccountsDiv',
    enabledProviders: 'facebook,twitter,googleplus,linkedin',
    requiredCapabilities: 'login'
}
jQuery(function ($) {
    gigya.socialize.showAddConnectionsUI(connect_params);
});

function updateProfile() {
    gigya.accounts.showScreenSet({ screenSet: 'Default-ProfileUpdate', mobileScreenSet: 'DefaultMobile-ProfileUpdate', containerID: "gigyaContainer", onAfterSubmit: afterProfileSubmit });
}

function afterProfileSubmit() {
    document.location = "/";
}