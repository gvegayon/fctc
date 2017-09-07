// JScript File

function getUrlPath(url) {
    var virtualDirectory = window.location.pathname.split('/')[1];
    if (url.trim().charAt(0) == "/") {
        urlPath = '/' + virtualDirectory + url;
    }
    else {
        urlPath = '/' + virtualDirectory + '/' + url;
    }
    return urlPath;
}

/******************************************************************************
Function Name               fnDisplayMessage
This common function to display message to user. 
It displays the message in a modal dialog window.
The key for message to be displayed to the user and the message type needs to passed as arguments to the function.
The message types are 'i' for info, 'e' for error, 'q' for question, 'w' for warning
******************************************************************************/
function fnDisplayMessage(messageKey, messageType) {
    //var filepath = "TimeSheet/pages/Message.aspx?key="+ messageKey+"&type="+messageType;
    var filepath = "../Message.aspx?key=" + messageKey + "&type=" + messageType;
    var win = window.showModalDialog(filepath, this, "dialogHeight:120px;dialogWidth:370px;toolbar:no;center:yes;resizable:no;status:no;");
    return win;
}

function isInteger(eleName) {
    if ((event.keyCode >= 48 && event.keyCode <= 57) || (event.keyCode == 46)) {
        if ((eleName.value.indexOf('.', 0)) >= 0 && (event.keyCode == 46)) {
            event.returnValue = false;
        }
    }
    else {
        event.returnValue = false;
    }
}


function clearUNTCOnline() {

    try {
        if (document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtTitle').value != null)
            document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtTitle').value = "";
    }
    catch (err) {
    }

    try {
        var lstPopularName = document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_lstPopularName');
        var lstPopularNamebox = lstPopularName.getElementsByTagName("input");
        for (var x = 0; x < lstPopularNamebox.length; x++) {
            if (lstPopularNamebox[x].checked == true)
                lstPopularNamebox[x].checked = false;
        }
    }
    catch (err) {
    }

    try {
        var lstParticipants = document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_lstParticipants');
        var lstParticipantsBox = lstParticipants.getElementsByTagName("input");
        for (var x = 0; x < lstParticipantsBox.length; x++) {
            if (lstParticipantsBox[x].checked == true)
                lstParticipantsBox[x].checked = false;
        }
    }
    catch (err) {
    }

    //if(document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_AjaxPanel1').style.display == 'block')
    // document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_dgSearch').style.display = 'none';
}


function fnIsAlphaNumeric(eleName) {
    if ((event.keyCode >= 48 && event.keyCode <= 57) || (event.keyCode == 46) || (event.keyCode >= 65 && event.keyCode <= 90) || (event.keyCode >= 97 && event.keyCode <= 122) || (event.keyCode == 8) || (event.keyCode == 127) || (event.keyCode == 32)) {
        if ((eleName.value.indexOf('.', 0)) >= 0 && (event.keyCode == 46)) {
            event.returnValue = false;
        }
    }
    else {
        event.returnValue = false;
    }
}

function fnIs_AlphaNumeric(eleName) {
    if ((event.keyCode == 32) || (event.keyCode == 40) || (event.keyCode == 41) || (event.keyCode == 43) || (event.keyCode == 45) || (event.keyCode >= 48 && event.keyCode <= 57)) {
        event.returnValue = true;
    }
    else {
        event.returnValue = false;
    }
}

function fnIsDigitsTelephone(event) {
    if (BrowserDetect.browser == "Explorer") {
        event = window.event;
    }
    if ((event.keyCode == 32) || (event.keyCode == 40) || (event.keyCode == 41) || (event.keyCode == 43) || (event.keyCode == 45) || (event.keyCode >= 48 && event.keyCode <= 57)) {
        event.returnValue = true;
    }
    else {
        event.returnValue = false;
    }
}

function AlertMessage(message) {
    if (message == "en") {
        alert("'<' or '>' are not allowed");
    }
    else if (message == "fr") {
        alert("Le code de sécurité ne peut contenir les symboles '<' ou '>'");
    }
    else {
        alert(message);
    }
}

function checkAngularBarcketsOnkeypress(event, message) {
    var keyCode;
    if (BrowserDetect.browser == "Firefox") {
        keyCode = event.which;
    }
    else {
        keyCode = event.keyCode;
    }
    if (keyCode == 60 || keyCode == 62) {
        AlertMessage(message);
        (event.preventDefault) ? event.preventDefault() : event.returnValue = false;
    }

    return true;
}

function checkAngularBarcketsOnblur(obj, message) {
    if (obj.value.indexOf("<") > -1 || obj.value.indexOf(">") > -1) {
        AlertMessage(message);
        obj.focus();
    }
}

/******************************************************************************
Function Name               returnTrue
It returns true from modal dialog and closes it.
******************************************************************************/
function returnTrue() {
    window.returnValue = true;
    self.close();
    return false;
}
/******************************************************************************
Function Name               returnFalse
It returns false from modal dialog and closes it.
******************************************************************************/
function returnFalse() {
    window.returnValue = false;
    self.close();
    return false;
}
//added by nanda on 26/12/2006
//this code will redirect to error page with error code 
function RedirectToErrorPage(messageKey) {
    window.location = getUrlPath("Pages/Error.aspx?messageKey=" + messageKey);
}

function TrimStringCommon(pstr) {
    pstr = pstr.replace(/^\s+|\s+$/ig, '');
    return pstr;
}

function fnDeleteConfirm() {

    var confirm = fnDisplayMessage("12003", "q");
    if (confirm == true) {
        return true;
    }
    else {
        return false;
    }
    return true;
}

var dtCh = "/";
var minYear = 1900;
var maxYear = 2100;

function isInteger(s) {
    var i;
    for (i = 0; i < s.length; i++) {
        // Check that current character is number.
        var c = s.charAt(i);
        if (((c < "0") || (c > "9"))) return false;
    }
    // All characters are numbers.
    return true;
}

function stripCharsInBag(s, bag) {
    var i;
    var returnString = "";
    // Search through string's characters one by one.
    // If character is not in bag, append to returnString.
    for (i = 0; i < s.length; i++) {
        var c = s.charAt(i);
        if (bag.indexOf(c) == -1) returnString += c;
    }
    return returnString;
}

function daysInFebruary(year) {
    // February has 29 days in any year evenly divisible by four,
    // EXCEPT for centurial years which are not also divisible by 400.
    return (((year % 4 == 0) && ((!(year % 100 == 0)) || (year % 400 == 0))) ? 29 : 28);
}
function DaysArray(n) {
    for (var i = 1; i <= n; i++) {
        this[i] = 31
        if (i == 4 || i == 6 || i == 9 || i == 11) { this[i] = 30 }
        if (i == 2) { this[i] = 29 }
    }
    return this
}

function isDate(dtStr) {
    var daysInMonth = DaysArray(12)
    var pos1 = dtStr.indexOf(dtCh)
    var pos2 = dtStr.indexOf(dtCh, pos1 + 1)
    var strDay = dtStr.substring(0, pos1)
    var strMonth = dtStr.substring(pos1 + 1, pos2)
    var strYear = dtStr.substring(pos2 + 1)
    strYr = strYear
    if (strDay.charAt(0) == "0" && strDay.length > 1) strDay = strDay.substring(1)
    if (strMonth.charAt(0) == "0" && strMonth.length > 1) strMonth = strMonth.substring(1)
    for (var i = 1; i <= 3; i++) {
        if (strYr.charAt(0) == "0" && strYr.length > 1) strYr = strYr.substring(1)
    }
    month = parseInt(strMonth)
    day = parseInt(strDay)
    year = parseInt(strYr)
    if (pos1 == -1 || pos2 == -1) {
        //alert("The date format should be : mm/dd/yyyy")
        fnDisplayMessage("DATE_FT", "e");
        return false
    }
    if (strMonth.length < 1 || month < 1 || month > 12) {
        //alert("Please enter a valid month")
        fnDisplayMessage("DATE_M", "e");
        return false
    }
    if (strDay.length < 1 || day < 1 || day > 31 || (month == 2 && day > daysInFebruary(year)) || day > daysInMonth[month]) {
        //alert("Please enter a valid day")
        fnDisplayMessage("DATE_D", "e");
        return false
    }
    if (strYear.length != 4 || year == 0 || year < minYear || year > maxYear) {
        //alert("Please enter a valid 4 digit year between "+minYear+" and "+maxYear)
        fnDisplayMessage("DATE_Y", "e");
        return false
    }
    if (dtStr.indexOf(dtCh, pos2 + 1) != -1 || isInteger(stripCharsInBag(dtStr, dtCh)) == false) {
        //alert("Please enter a valid date")
        fnDisplayMessage("DATE_INVALID", "e");
        return false
    }


    return true
}




// Browser Detection Code
var BrowserDetect = {
    init: function () {
        this.browser = this.searchString(this.dataBrowser) || "An unknown browser";
        this.version = this.searchVersion(navigator.userAgent)
			|| this.searchVersion(navigator.appVersion)
			|| "an unknown version";
        this.OS = this.searchString(this.dataOS) || "an unknown OS";
    },
    searchString: function (data) {
        for (var i = 0; i < data.length; i++) {
            var dataString = data[i].string;
            var dataProp = data[i].prop;
            this.versionSearchString = data[i].versionSearch || data[i].identity;
            if (dataString) {
                if (dataString.indexOf(data[i].subString) != -1)
                    return data[i].identity;
            }
            else if (dataProp)
                return data[i].identity;
        }
    },
    searchVersion: function (dataString) {
        var index = dataString.indexOf(this.versionSearchString);
        if (index == -1) return;
        return parseFloat(dataString.substring(index + this.versionSearchString.length + 1));
    },
    dataBrowser: [
		{ string: navigator.userAgent,
		    subString: "OmniWeb",
		    versionSearch: "OmniWeb/",
		    identity: "OmniWeb"
		},
		{
		    string: navigator.vendor,
		    subString: "Apple",
		    identity: "Safari"
		},
		{
		    prop: window.opera,
		    identity: "Opera"
		},
		{
		    string: navigator.vendor,
		    subString: "iCab",
		    identity: "iCab"
		},
		{
		    string: navigator.vendor,
		    subString: "KDE",
		    identity: "Konqueror"
		},
		{
		    string: navigator.userAgent,
		    subString: "Firefox",
		    identity: "Firefox"
		},
		{
		    string: navigator.vendor,
		    subString: "Camino",
		    identity: "Camino"
		},
		{		// for newer Netscapes (6+)
		    string: navigator.userAgent,
		    subString: "Netscape",
		    identity: "Netscape"
		},
		{
		    string: navigator.userAgent,
		    subString: "MSIE",
		    identity: "Explorer",
		    versionSearch: "MSIE"
		},
		{
		    string: navigator.userAgent,
		    subString: "Gecko",
		    identity: "Mozilla",
		    versionSearch: "rv"
		},
		{ 		// for older Netscapes (4-)
		    string: navigator.userAgent,
		    subString: "Mozilla",
		    identity: "Netscape",
		    versionSearch: "Mozilla"
		}
	],
    dataOS: [
		{
		    string: navigator.platform,
		    subString: "Win",
		    identity: "Windows"
		},
		{
		    string: navigator.platform,
		    subString: "Mac",
		    identity: "Mac"
		},
		{
		    string: navigator.platform,
		    subString: "Linux",
		    identity: "Linux"
		}
	]

};
BrowserDetect.init();


function getSourceElement(event) {
    if (BrowserDetect.browser == "Firefox") {
        return event.target;
    }
    else {
        return event.srcElement;
    }

}

// Function to encounter Backspace
function IsKeyDown(event) {
    if (BrowserDetect.browser == "Explorer") {
        event = window.event;
        if (event.keyCode > 0 && event.keyCode != 46 && event.keyCode != 8) {
            event.returnValue = false;
        }
    }
    else {
        if (event.which > 0 && event.which != 46 && event.which != 8) {
            event.returnValue = false;
        }
    }
}

function fnIsDigits(event) {
    if (BrowserDetect.browser == "Explorer") {
        event = window.event;
        if (event.keyCode >= 48 && event.keyCode <= 57 || event.keyCode == 46 || event.keyCode == "shift" || event.keyCode == "Tab" || event.keyCode == "9" || event.keyCode == 16 || event.keyCode == 8 || event.keyCode == 26 || event.keyCode == 27 || event.keyCode == 191 || event.keyCode >= 96 && event.keyCode <= 105 || event.keyCode >= 37 && event.keyCode <= 40) {
            event.returnValue = true;
        }
        else {
            event.returnValue = false;
        }
    }
    else {
        if (event.keyCode >= 48 && event.keyCode <= 57 || event.keyCode == 46 || event.keyCode == "shift" || event.keyCode == "Tab" || event.keyCode == "9" || event.keyCode == 16 || event.keyCode == 8 || event.keyCode == 26 || event.keyCode == 27 || event.keyCode == 191 || event.keyCode >= 96 && event.keyCode <= 105 || event.keyCode >= 37 && event.keyCode <= 40) {
            //event.returnValue = true;
            return true;
        }
        else {
            //event.returnValue = false;
            return false;
        }
    }
}
//Added By Avinash
function fnIsDigitsCal(event) {
    if (BrowserDetect.browser == "Explorer") {
        event = window.event;
        if (event.keyCode >= 48 && event.keyCode <= 57 || event.keyCode == 111 || event.keyCode == 46 || event.keyCode == "shift" || event.keyCode == "Tab" || event.keyCode == "9" || event.keyCode == 16 || event.keyCode == 8 || event.keyCode == 26 || event.keyCode == 27 || event.keyCode == 191 || event.keyCode >= 96 && event.keyCode <= 105 || event.keyCode >= 37 && event.keyCode <= 40) {
            event.returnValue = true;
        }
        else {
            event.returnValue = false;
        }
    }
    else {
        if (event.keyCode >= 48 && event.keyCode <= 57 || event.keyCode == 111 || event.keyCode == 46 || event.keyCode == "shift" || event.keyCode == "Tab" || event.keyCode == "9" || event.keyCode == 16 || event.keyCode == 8 || event.keyCode == 26 || event.keyCode == 27 || event.keyCode == 191 || event.keyCode >= 96 && event.keyCode <= 105 || event.keyCode >= 37 && event.keyCode <= 40) {
            //event.returnValue = true;
            return true;
        }
        else {
            //event.returnValue = false;
            return false;
        }
    }
}
function CheckDate(e, Id) {
    if (e.keyCode < 48 || e.keyCode > 57) {
        return false;
    }
    else {
        return true;
    }
}

function ChangeDate(e, txtId) {
    var txt = document.getElementById(txtId)
    var Date = txt.value.toString();
    var Date2 = Date;

    if (Date.length == 8) {
        Date = Date2.charAt(0) + Date2.charAt(1) + "/";
        Date += Date2.charAt(2) + Date2.charAt(3) + "/"
        Date += Date2.charAt(4) + Date2.charAt(5) + Date2.charAt(6) + Date2.charAt(7);
    }
    else
        Date = "";
    txt.value = Date;
    return false;
}
//End Added By Avinash

function getParentElement(obj) {
    if (BrowserDetect.browser == "Firefox" || BrowserDetect.browser == "Netscape") {
        return obj.parentNode;
    }
    else {
        return obj.parentElement;
    }

}

function getInnerText(obj) {
    if (BrowserDetect.browser == "Firefox" || BrowserDetect.browser == "Netscape") {
        return obj.textContent;
    }
    else {
        return obj.innerText;
    }

}


/**
* This function gives the dimension of client area, accordingly a window/frame can be resized.
* @return {Array} There will be two elements in array. 1) width of client area 2) height of client area
* @author Jatan Porecha
*/
function getDocumentDimensions() {
    var dimensions = new Array();
    if (self.innerHeight) // all except Explorer
    {
        dimensions[0] = self.innerWidth;
        dimensions[1] = self.innerHeight;
    }
    else if (document.documentElement && document.documentElement.clientHeight)
    // Explorer 6 Strict Mode
    {
        dimensions[0] = document.documentElement.clientWidth;
        dimensions[1] = document.documentElement.clientHeight;
    }
    else if (document.body) // other Explorers
    {
        dimensions[0] = document.body.clientWidth;
        dimensions[1] = document.body.clientHeight;
    }

    return dimensions;
}


/**
* This function automatically adjusts the size of a dialog window. It calculates the display
* area required by page using clientWidth/clientHieght and scrollWidth/scrollHeight properties
* of body object.
* This function takes care of disparity between multiple browsers and accordingly it choses the 
* proper properties to calculate the visible area.
* It MUST be called on the onload of body tag.
* 
* @author Jatan Porecha
*/

function adjustDialogSize() {

    if ((typeof document.noresize != 'undefined') && document.noresize == 'yes')
        return;

    var db = document.body;
    var diffH, diffW, dH, dW;
    var someOffset = (45 * screen.width) / 1024;
    var someHeightOffset = (55 * screen.height) / 768;
    var nDialogWidth; // dialogWidth.substring(0,dialogWidth.indexOf("px"))*1;
    var nDialogHeight; //dialogHeight.substring(0,dialogHeight.indexOf("px"))*1;

    var main_win_height = screen.height;
    var main_win_width = screen.width;
    var main_win_left = 5;
    var main_win_top = 5;

    var dimension = getDocumentDimensions();

    nDialogWidth = dimension[0];
    nDialogHeight = dimension[1];

    diffW = parseInt(db.scrollWidth) - nDialogWidth;
    diffH = parseInt(db.scrollHeight) - nDialogHeight;

    var finalWidth, finalHeight;

    dW = diffW + parseInt(nDialogWidth) + someOffset;
    finalWidth = (dW) + 'px';
    var dialogWidth = finalWidth;

    dH = diffH + parseInt(nDialogHeight) + someHeightOffset;
    finalHeight = (dH) + 'px';
    var dialogHeight = finalHeight;


    nDialogWidth = dialogWidth.substring(0, dialogWidth.indexOf("px"));
    nDialogHeight = dialogHeight.substring(0, dialogHeight.indexOf("px"));

    if (nDialogHeight > (515 * screen.height) / 768)
        nDialogHeight = (515 * screen.height) / 768;

    var dialogLeft = (main_win_left + (main_win_width - nDialogWidth) / 2) + "px";
    var dialogTop = (main_win_top + (main_win_height - nDialogHeight) / 2) + "px";
    window.resizeTo(nDialogWidth, nDialogHeight);
    window.moveTo(dialogLeft.substring(0, dialogLeft.indexOf("px")), dialogTop.substring(0, dialogTop.indexOf("px")));

}


function fnEnter(btn, event) {
    if (BrowserDetect.browser == "Explorer")
        event = window.event;

    // process only the Enter key
    if (event.keyCode == 13) {
        // cancel the default submit
        //        event.returnValue=false;
        //        event.cancel = true;
        // submit the form by programmatically clicking the specified button

        document.getElementById(btn).click();
        event.keyCode = 0;
    }

}


function fnValidateDate() {
    if (document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtFrom').value != "") {
        if (isDate(document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtFrom').value) == false) {
            document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtFrom').focus();
            return false;
        }
    }
    if (document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtTo').value != "") {
        if (isDate(document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtTo').value) == false) {
            document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtTo').focus();
            return false;
        }
    }

    return true;
}



function fnValidateDateActions() {
    if (document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtFromreceipt').value != "") {
        if (isDate(document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtFromreceipt').value) == false) {
            return false;
        }
    }
    if (document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtToreceipt').value != "") {
        if (isDate(document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtToreceipt').value) == false) {
            return false;
        }
    }

    if (document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtFromeffect').value != "") {
        if (isDate(document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtFromeffect').value) == false) {
            return false;
        }
    }
    if (document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtToeffect').value != "") {
        if (isDate(document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtToeffect').value) == false) {
            return false;
        }
    }

    return true;
}


function isIntegerAction() {
    if (document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtFromNum').value != "") {
        if (document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtFromNum').value > 2147483647) {
            fnDisplayMessage("I_INVALID_NO", "e");
            return false;
        }
    }
    if (document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtToNum').value != "") {
        if (document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtToNum').value > 2147483647) {
            fnDisplayMessage("I_INVALID_NO", "e");
            return false;
        }
    }

    return true;
}

//function showtooltip(DropDownID)
//{
//    var objOption;
//    var objDropDown = document.getElementById(DropDownID);
//    var objOptions = objDropDown.getElementsByTagName("option");
//    
//    for(var i=0; i<objOptions.length; i++)
//    {
//        objOption = objOptions[i];
//        //document.getElementById("tooltip").innerHTML =  objOption.innerText;

//        objOption.onmouseover = function()
//        {   
//            document.getElementById("tooltip").style.display = "inline"
//            document.getElementById("tooltip").style.top = event.y;
//            document.getElementById("tooltip").style.left = event.x;
//        }
//        
//        objOption.onmouseout = function() 
//        {
//            document.getElementById("tooltip").style.display = "none";
//        }
//    }
//}

function showtooltip(obj) {
    //alert(obj)
    var objListItem = obj;
    document.getElementById("tooltip").style.display = "inline"
    document.getElementById("tooltip").style.top = event.y;
    document.getElementById("tooltip").style.left = event.x;
    document.getElementById("tooltip").innerHTML = objListItem;
}

function hidetooltip() {
    document.getElementById("tooltip").style.display = "none";
}


function getPageLeft(el) {
    var left = 0;
    do
        left += el.offsetLeft;
    while ((el = el.offsetParent));
    return left;
}

function getPageTop(el) {
    var top = 0;
    do
        top += el.offsetTop;
    while ((el = el.offsetParent));
    return top;

    //    tt.style.left = (getPageLeft(nextTo) + nextTo.offsetWidth) + 'px';
    //tt.style.top = getPageTop(nextTo) + 'px';
}

function copttocontrol() {

    //var cntr1 = document.getElementById(cnt1);
    //var cntr2 = document.getElementById(cnt2);

    var key_code = window.event.keyCode;
    if (key_code == 9) {
        document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtTo').value = document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtFrom').value;

    }

}



function copttocontrolnum() {

    //var cntr1 = document.getElementById(cnt1);
    //var cntr2 = document.getElementById(cnt2);

    var key_code = window.event.keyCode;
    if (key_code == 9) {
        document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtToNum').value = document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtFromNum').value;

    }

}

//#
function CopytxtToDateonFocus1() {


    if (document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtTo').value == "") {
        document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtTo').value = "DD/MM/YYYY";
        document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtFrom').value = "DD/MM/YYYY";
    }
}
function CopytxtToDateonFocus1_FR() {
    if (document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtTo').value == "") {
        document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtTo').value = "JJ/MM/AAAA";
        document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtFrom').value = "JJ/MM/AAAA";
    }
}

//#
function From_To_blank() {
    if (document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtFrom').value == "DD/MM/YYYY" || document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtFrom').value == "JJ/MM/AAAA") {
        document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtFrom').value = "";
    }
    if (document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtTo').value == "DD/MM/YYYY" || document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtTo').value == "JJ/MM/AAAA") {
        document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtTo').value = "";
    }
}
//#
function From_To_Fill() {
    if (document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtFrom').value == "") {
        document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtFrom').value = "DD/MM/YYYY";
    }
    if (document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtTo').value == "") {
        document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtTo').value = "DD/MM/YYYY";
    }
}
/// #
function From_To_Fill_FR() {
    if (document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtFrom').value == "") {
        document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtFrom').value = "JJ/MM/AAAA";
    }
    if (document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtTo').value == "") {
        document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtTo').value = "JJ/MM/AAAA";
    }
}


function CopytxtToNumOnFocus() {


    //if( document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtToNum').value == "")
    // {
    document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtToNum').value = document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtFromNum').value;
    // }


}

function CopytxtToDateonFocus() {


    //if( document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtTo').value == "")
    // {
    document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtTo').value = document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtFrom').value;
    //}


}


function CopytxtToreceiptOnFocus() {


    //if( document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtToreceipt').value == "")
    // {
    document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtToreceipt').value = document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtFromreceipt').value;
    // }


}

function CopyeffectFromFocus() {


    //if( document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtToeffect').value == "")
    // {
    document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtToeffect').value = document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtFromeffect').value;
    //}


}


function CopyreceiptFromTo() {

    //var cntr1 = document.getElementById(cnt1);
    //var cntr2 = document.getElementById(cnt2);

    var key_code = window.event.keyCode;
    if (key_code == 9) {
        document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtToreceipt').value = document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtFromreceipt').value;

    }

}


function CopyeffectFromTo() {

    //var cntr1 = document.getElementById(cnt1);
    //var cntr2 = document.getElementById(cnt2);

    var key_code = window.event.keyCode;
    if (key_code == 9) {
        document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtToeffect').value = document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_txtFromeffect').value;

    }

}

function alarmWindow() {

    alarmWindow = document.body.appendChild(document.createElement("div"))
    alarmWindow.id = 'processMessage';
    alarmWindow.style.height = document.documentElement.scrollHeight + 'px';
    alarmWindow.style.backgroundColor = '#000';
    alarmWindow.style.position = 'absolute';
    alarmWindow.style.width = '100%';
    alarmWindow.style.top = '0px';
    alarmWindow.style.left = '0px';
    alarmWindow.style.zIndex = '10000';

    div1 = document.createElement("div");
    div1.style.top = document.documentElement.clientHeight / 2 + 'px';
    div1.style.left = ((document.documentElement.clientWidth / 2) - 50) + 'px';
    div1.style.position = 'absolute';


    img = document.createElement("img");
    img.src = "Images/ajax-loader (2).gif";
    div1.appendChild(img);

    br = document.createElement("br");
    div1.appendChild(br);

    lbl = document.createElement("label");
    if (lbl.className != null)
        lbl.className = 'processtitle';

    lbl.innerHTML = 'Please wait....';
    div1.appendChild(lbl);


    alarmWindow.appendChild(div1);
    //IE hack.  Overlap  z-index doo-dads
    //This is a one pixel transparent png.
    //alarmWindow.style.backgroundImage = 'url(images/ajax-loaderMain.gif)';
    return true;
}
//Added by suvidha
function removeDefaultDateFormat(txtObject) {
    if (txtObject.value == "DD/MM/YYYY") {

        txtObject.value = "";
    }
}
   
