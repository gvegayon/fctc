
var unts_breadcrumb = ""; // Holds the default breadcrumb text whenever UNTS Online is loaded.


// JScript File
function MM_openBrWindow(theURL, winName, features) { //v2.0
    window.open(theURL, winName, features);
}

function ShowDeclarations(str, sub, chapter, treaty) {
    str = encodeURI(str);
    if (str.lastIndexOf("%20") == str.length - 3) {
        str = str.substring(0, str.lastIndexOf("%20"));
    }
    var path = '../Pages/Declarations.aspx?index=' + str + '&chapter=' + chapter + "&treaty=" + treaty;
    document.getElementById('ifrm').style.display = 'block';
    if ((window.location.href.indexOf("ViewDetails.aspx") > -1) || (window.location.href.indexOf("ViewDetailsII.aspx") > -1)|| (window.location.href.indexOf("ViewDetailsIII.aspx") > -1)|| (window.location.href.indexOf("ViewDetailsIV.aspx") > -1)|| (window.location.href.indexOf("ViewDetailsV.aspx") > -1)) {
        document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_hlkFrameClose').style.display = 'block';
    }
    else{ document.getElementById('hlkFrameClose').style.display = 'block'; }
    document.getElementById('ifrm').src = path;

    return true;
}


function ShowDeclarationsNew(str, sub,lang, chapter, treaty) {
    str = encodeURI(str);
    if (str.lastIndexOf("%20") == str.length - 3) {
        str = str.substring(0, str.lastIndexOf("%20"));
    }
    var path = '../Pages/Declarations.aspx?index=' + str + '&lang=' + lang + '&chapter=' + chapter + "&treaty=" + treaty;
    document.getElementById('ifrm').style.display = 'block';
    if ((window.location.href.indexOf("ViewDetails.aspx") > -1) || (window.location.href.indexOf("ViewDetailsII.aspx") > -1) || (window.location.href.indexOf("ViewDetailsIII.aspx") > -1) || (window.location.href.indexOf("ViewDetailsIV.aspx") > -1) || (window.location.href.indexOf("ViewDetailsV.aspx") > -1)) {
        document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_hlkFrameClose').style.display = 'block';
    }
    else { document.getElementById('hlkFrameClose').style.display = 'block'; }
    document.getElementById('ifrm').src = path;

    return true;
}

function ShowAll() {
    if (document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_divFlag').value == "1") {
        document.getElementById('divAll').style.display = 'block';
        document.getElementById('divNotes').style.display = 'block';
    }
    return true;
}
function FillIFrame(str, strIndex, chapter, treaty) {
    var index = str.split(",");
    var path = '../Pages/Notes.aspx?index=' + index[0] + '&chapter=' + chapter + "&treaty=" + treaty + '&noresize=yes&flag=Y';
    document.getElementById('ifrm').style.display = 'block';
    document.getElementById('hlkFrameClose').style.display = 'block';
    document.getElementById('ifrm').src = path;
}

function CloseFrame() {
    document.getElementById('ifrm').style.display = 'none';
    document.getElementById('hlkFrameClose').style.display = 'none';
    return false;
}


function CloseDecFrame() {  
    document.getElementById('ifrm').style.display = 'none';
    document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_hlkFrameClose').style.display = 'none';
    return false;
}

function CloseOnChange() {
    document.getElementById('divAll').style.display = 'none';
    document.getElementById('divNotes').style.display = 'none';
    return false;
}
function ShowDetails(objid) {
    window.open('showDetails.aspx?objid=' + objid, null, 'scrollbars=auto,status=no')
    return false;
}

function ShowActionDetails(objid) {
    window.open('showActionDetails.aspx?objid=' + objid, null, 'scrollbars=auto,status=no')
    return false;
}

function ShowNoteAll() {
    //document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_lnkNote').style.display='none';
    //document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_lnkDec').style.display='none';
    document.getElementById('divNotes').style.display = 'block';
}
function ShowNoteAll_LON() {
    //document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_lnkNote').style.display='none';
    //document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_lnkDec').style.display='none';
    //document.getElementById('divNotesAll').style.display='block';     
}

function Print(blocks, strHeading) {
    var prtContent = "<table border='0' width='100%'  cellspacing='0' cellpadding='0'><tr><td align='center'>" + strHeading + "<p></td></tr><tr><td style='border:none'>";
    for (var i = 0; i < blocks.length; i++) {
        prtContent += document.getElementById(blocks[i]).innerHTML;
        prtContent += "</td></tr>";

        if (i < blocks.length - 1)
            prtContent += "<td style='border:none'><tr>";

    }

    prtContent += "</table>";

    var my_window = window.open("", "mywindow1", "status=1,width=550,height=300,scrollbars=yes");
    //var WinPrint = window.open('', '', 'left=2000,top=2000,width=1,height=1,toolbar=0,scrollbars=0,status=1');
    var htmlhead = '<html><head><link href="https://treaties.un.org/Css/styles.css" rel="stylesheet" type="text/css" /><link href="https://treaties.un.org/Css/print.css" rel="stylesheet" type="text/css" /></head><body>';
    var htmlfooter = '</body></html>';
    var WinPrintInnerHTML = htmlhead + prtContent + htmlfooter;
    // my_window.document.open();
    my_window.document.body.innerHTML = WinPrintInnerHTML;
    my_window.document.close();
    my_window.focus();
    my_window.print();
    my_window.close();
}

function PrintMTDSGDetails(ViewNumber) {
    var objBlocks = new Array();
    if (ViewNumber == 1) {
        objBlocks[0] = "headerbox";
        objBlocks[1] = "Tr8";
        objBlocks[2] = "participants";
        objBlocks[3] = "specialtbl";
        objBlocks[4] = "divAll";
        objBlocks[5] = "ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_divNotes";
    }
    else if (ViewNumber == 2) {
        objBlocks[0] = "headerbox";
        objBlocks[1] = "Tr8";
        objBlocks[2] = "participants";
        objBlocks[3] = "specialtbl";
        objBlocks[4] = "divAll";
        objBlocks[5] = "ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_divNotes";
    }
    else if (ViewNumber == 3) {
        objBlocks[0] = "headerbox";
        objBlocks[1] = "Tr8";
        objBlocks[2] = "participants";
        objBlocks[3] = "divAll";
        objBlocks[4] = "specialtbl";
        objBlocks[5] = "ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_divNotes";
    }
    else if (ViewNumber == 4) {
        objBlocks[0] = "headerbox";
        objBlocks[1] = "Tr8";
        objBlocks[2] = "specialtbl";
        objBlocks[3] = "participants";
        objBlocks[4] = "divAll";
        objBlocks[5] = "ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_divNotes";
    }
    else if (ViewNumber == 5) {
        //alert("Print");
        objBlocks[0] = "headerbox";
        objBlocks[1] = "Tr8";
        objBlocks[2] = "participants";
        objBlocks[3] = "specialtbl";
        objBlocks[4] = "divAll";
        objBlocks[5] = "ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_divNotes";
        //alert("NotPrint");
    }
    //var blocks = objBlocks;
    var strHeading = "";
    var mydate = new Date()
    var dayarray = new Array("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
    var montharray = new Array("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
    var daym = mydate.getDate()
    var year = mydate.getYear()
    if (year < 1000)
        year += 1900
    var dt = new Date();
    var day = mydate.getDay()
    var month = mydate.getMonth()
    var ctime = dt.toLocaleTimeString();
    var cdate = dayarray[day] + ", " + daym + " " + montharray[month] + " " + year + " " + ctime
    strHeading = "Today is " + cdate
    var prtContent = "<table border='0' width='100%' cellspacing='0' cellpadding='0'><tr><td align='center'>" + strHeading + "<p></td></tr><tr><td style='border:none'>";
     for (var i = 0; i < objBlocks.length; i++) {
        if (objBlocks[i] != null && document.getElementById(objBlocks[i]) != null && document.getElementById(objBlocks[i]).innerHTML != null && document.getElementById(objBlocks[i]).innerHTML != '') {
            prtContent += document.getElementById(objBlocks[i]).innerHTML;
            prtContent += "</td></tr>";

            if (i < objBlocks.length - 1)
                prtContent += "<td style='border:none'><tr>";
        }

    }
    prtContent += "</table>";
    var my_window = window.open("", "mywindow1", "status=1,width=550,height=300,resizable=yes;fullscreen=yes;scrollbars=yes");
    var WinPrint = window.open('', '', 'left=2000,top=2000,width=1,height=1,toolbar=0,resizable=yes;fullscreen=yes;scrollbars=0,status=1');
    var htmlhead = '<html><head><link href="https://treaties.un.org/Css/styles.css" rel="stylesheet" type="text/css" /><link href="https://treaties.un.org/Css/print.css" rel="stylesheet" type="text/css" /></head><body>';
    var htmlfooter = '</body></html>';
    var WinPrintInnerHTML = htmlhead + prtContent + htmlfooter;
    // my_window.document.open();
    my_window.document.body.innerHTML = WinPrintInnerHTML;
    my_window.document.close();
    my_window.focus();
    my_window.print();
    my_window.close();
    return true;
}


function PrintLonDetails() {   
    var heading = document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_lblSubChapter').innerHTML;

    //var configurationSetting = '<%=ConfigurationManager.AppSettings["PhotographPath"]%>';
    //alert(configurationSetting);

    var objBlocks = new Array();
    objBlocks[0] = "headerbox";
    objBlocks[1] = "divPrint";
    Print(objBlocks, "&nbsp;");
    return true;
}

function UntsSearch() { 
    window.print();
    window.close();
    return true;
}
function OpenTemp(lang) {
    if (document.getElementById("ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_dgSearch") == null)
    {
        if (lang == "_en")
        {
            if (document.getElementById("ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_lblMsg").innerText == "")
            {
                document.getElementById("ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_lblMsg").innerText = "Record Not Found.";
            }
        }
        else if (lang == "_fr")
        {
            if (document.getElementById("ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_lblMsg").innerText == "")
            {
                document.getElementById("ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_lblMsg").innerText = "Ficher n'est pas trouvé.";
            }
        }
        return false;
    }
    else
    {
            window.open('Temp.aspx', null, 'width = 550,scrollbars=yes,status=no');
    }
}

function LONSearch() {
    window.print();
    setTimeout(function () { window.close(); }, 1);
    return true;
}
//function OpenLONTemp() {
//    window.open('LONTemp.aspx', null, 'width = 550,scrollbars=yes,status=no')
//    return true;
//}
function OpenLONTemp(lang) {
    if (document.getElementById("ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_dgSearch") == null) {
        if (lang == "_en") {
            if (document.getElementById("ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_lblMsg").innerText == "") {
                document.getElementById("ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_lblMsg").innerText = "Record Not Found.";
            }
        }
        else if (lang == "_fr") {
            if (document.getElementById("ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_lblMsg").innerText == "") {
                document.getElementById("ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_lblMsg").innerText = "Ficher n'est pas trouvé.";
            }
        }
        return false;
    }
    else {
       var k= window.open('LONTemp.aspx', null, 'width = 550,scrollbars=yes,status=no')

        return true;
    }
}
function OnClickpopularName(labeltext) {
    document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_tabid').value = "1";
    ModifyBreadCrumb2(labeltext);
}

function OnClickTitle(labeltext) {
    document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_tabid').value = "2";
    ModifyBreadCrumb2(labeltext);
}

function OnClickParticipants(labeltext) {
    document.getElementById('ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_tabid').value = "3";
    ModifyBreadCrumb2(labeltext);
}

function ModifyBreadCrumb2(labeltext) {
    var breadcrumb = document.getElementById('ctl00_SiteMapPath1');

    if (unts_breadcrumb == "") {
        if (breadcrumb != null) {
            unts_breadcrumb = breadcrumb.innerHTML;
        }
    }

    //breadcrumb.innerHTML = unts_breadcrumb +  '';    
    //breadcrumb.innerHTML = unts_breadcrumb +  '<span style="color:#5D7B9D;font-family:Arial;font-size:Small;font-weight:bold;"> → </span><span style="color:#333333;">'+labeltext+'</span>';    
}

function ModifyBreadCrumb(labeltext) {
    var breadcrumb = document.getElementById('ctl00_SiteMapPath1');

    if (unts_breadcrumb == "") {
        if (breadcrumb != null) {
            unts_breadcrumb = breadcrumb.innerHTML;
        }
    }

    breadcrumb.innerHTML = unts_breadcrumb + '<span style="color:#5D7B9D;font-family:Arial;font-size:Small;font-weight:bold;"> → </span><span style="color:#333333;">' + labeltext + '</span>';

}

function ShowHeader() {
    document.getElementById('header').style.display = 'block';
}


function ShowMenusinFullTextMS() {
    document.getElementById('UntcOnline').style.display = 'none'
    document.getElementById('MsDatabase').style.display = 'block'
}
function ShowMenusinFullTextLON() {
    document.getElementById('UntcOnline').style.display = 'none'
    document.getElementById('MsDatabase').style.display = 'none'
    document.getElementById('LON').style.display = 'block'
}
function ShowMenusinFullTextCI() {
    document.getElementById('UntcOnline').style.display = 'none'
    document.getElementById('MsDatabase').style.display = 'none'
    document.getElementById('LON').style.display = 'none'
    document.getElementById('CI').style.display = 'block'
}
function ShowMenusinFullTextCN() {
    document.getElementById('UntcOnline').style.display = 'none'
    document.getElementById('MsDatabase').style.display = 'none'
    document.getElementById('LON').style.display = 'none'
    document.getElementById('CI').style.display = 'none'
    document.getElementById('CN').style.display = 'block'
}
function ShowMenusinFullTextCTC() {
    document.getElementById('UntcOnline').style.display = 'none'
    document.getElementById('MsDatabase').style.display = 'none'
    document.getElementById('LON').style.display = 'none'
    document.getElementById('CI').style.display = 'none'
    document.getElementById('CN').style.display = 'none'
    document.getElementById('CTC').style.display = 'block'
}
function ShowMenusinFullTextUN() {
    document.getElementById('UntcOnline').style.display = 'none'
    document.getElementById('MsDatabase').style.display = 'none'
    document.getElementById('LON').style.display = 'none'
    document.getElementById('CI').style.display = 'none'
    document.getElementById('CN').style.display = 'none'
    document.getElementById('CTC').style.display = 'none'
    document.getElementById('UN').style.display = 'block'
}

function ShowMenusinFullTextSEARCH() {
    document.getElementById('UntcOnline').style.display = 'none'
    document.getElementById('MsDatabase').style.display = 'none'
    document.getElementById('LON').style.display = 'none'
    document.getElementById('CI').style.display = 'none'
    document.getElementById('CN').style.display = 'none'
    document.getElementById('CTC').style.display = 'none'
    document.getElementById('UN').style.display = 'none'
    document.getElementById('SEARCH').style.display = 'block'
}
function OpenShowDetails() {
    //debugger;
    var objBlocks = new Array();

    objBlocks[0] = "headerbox";
    objBlocks[1] = "participants";

    var strHeading = "";
    var mydate = new Date()
    var dayarray = new Array("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
    var montharray = new Array("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
    var daym = mydate.getDate()
    var year = mydate.getYear()
    if (year < 1000)
        year += 1900
    var dt = new Date();
    var day = mydate.getDay()
    var month = mydate.getMonth()
    var ctime = dt.toLocaleTimeString();
    var cdate = dayarray[day] + ", " + daym + " " + montharray[month] + " " + year + " " + ctime
    strHeading = "Today is " + cdate

    var prtContent = "<table border='0' width='100%' cellspacing='0' cellpadding='0'><tr><td align='center'>" + strHeading + "<p></td></tr><tr><td style='border:none'>";
    for (var i = 0; i < objBlocks.length; i++) {
        prtContent += document.getElementById(objBlocks[i]).innerHTML;
        prtContent += "</td></tr>";

        if (i < objBlocks.length - 1)
            prtContent += "<td style='border:none'><tr>";
    }
    prtContent += "</table>";

    var my_window = window.open("", "mywindow1", "status=1,width=550,height=300,scrollbars=yes");
    var htmlhead = '<html><head><link href="https://treaties.un.org/Css/styles.css" rel="stylesheet" type="text/css" /><link href="https://treaties.un.org/Css/print.css" rel="stylesheet" type="text/css" /></head><body>';
    var htmlfooter = '</body></html>';
    var WinPrintInnerHTML = htmlhead + prtContent + htmlfooter;
    //my_window.document.open();
    //my_window.document.write(WinPrintInnerHTML);
    my_window.document.body.innerHTML = WinPrintInnerHTML;
    my_window.document.close();
    my_window.focus();
    my_window.print();
    my_window.close();

    return true;
}
////Added for printing showActionDetails
function PrintActionDetails() {
    //window.document.body.innerHTML = document.getElementById("divPrintActionDetails").innerHTML;
    //window.document.close();
    //window.focus();
    //window.print();
    //window.close();

    var objBlocks = new Array();
    //objBlocks[0] = "headerbox";
    objBlocks[0] = "divPrintActionDetails";
    Print(objBlocks, "&nbsp;");
    return true;
}
function OpenActionTemp(lang) {
    if (document.getElementById("ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_dgActions") == null) {
        if (lang == "_en") {
            document.getElementById("ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_lblMsg").innerText = "Record Not Found.";
        }
        else if (lang == "_fr") {
            document.getElementById("ctl00_ctl00_ContentPlaceHolder1_ContentPlaceHolderInnerPage_lblMsg").innerText = "Ficher n'est pas trouvé.";
        }
        return false;
    }
    else {
        window.open('TempAction.aspx', null, 'width = 550,scrollbars=yes,status=no')
        return true;
    }
}
