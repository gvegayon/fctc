var jsAreaShowTime = 1000;
var JsAreas = new Object();

function GetArea(id)
{
	return JsAreas[id] ? JsAreas[id] : (JsAreas[id] = new JsArea(id));
}

// A single popup window
function JsArea(id)
{
	this.AreaId = id;
}

// Function to return the DIV Layer
JsArea.prototype.ContentArea= function()
{
	var divArea = document.getElementById(this.AreaId);
	if (divArea != null)
	{
		return divArea;
	}
	return null;
}

var activeAreaId = null;

// Function to show the DIV Layer
JsArea.prototype.popareaup = function(x, y)
{
if (activeAreaId != null)	
	jsAreaClose(activeAreaId);
	
	var divLayer = this.ContentArea();
	divLayer.style.position = 'absolute';
	divLayer.style.display = 'block';
	divLayer.style.left = 320;
	divLayer.style.top = 302;
	divLayer.onmouseover= JsAreaMouseOver;
	divLayer.onmouseout = jsAreaMouseOut;
	activeAreaId = this.AreaId;
	
	return false;
}

JsArea.prototype.popareaup1 = function(x, y)
{
if (activeAreaId != null)	
	jsAreaClose(activeAreaId);
	
	var divLayer = this.ContentArea();
	divLayer.style.position = 'absolute';
	divLayer.style.display = 'block';
	divLayer.style.left = 320;
	divLayer.style.top = 340;
	divLayer.onmouseover= JsAreaMouseOver;
	divLayer.onmouseout = jsAreaMouseOut;
	activeAreaId = this.AreaId;
	
	return false;
}

JsArea.prototype.popareaup2 = function(x, y)
{
if (activeAreaId != null)	
	jsAreaClose(activeAreaId);
	
	var divLayer = this.ContentArea();
	divLayer.style.position = 'absolute';
	divLayer.style.display = 'block';
	divLayer.style.left = 320;
	divLayer.style.top = 335;
	divLayer.onmouseover= JsAreaMouseOver;
	divLayer.onmouseout = jsAreaMouseOut;
	activeAreaId = this.AreaId;
	
	return false;
}

// Function to hide the DIV Layer
JsArea.prototype.hide = function()
{
	var divLayer = this.ContentArea();
	if (divLayer != null)
		divLayer.style.display = 'none';
		
	return false;
}

// Function to be called
// by Web forms to show the Popup Window
function PopupArea(e, areaId)
{
	if (e.pageX || e.pageY)
	{
		posx = e.pageX;
		posy = e.pageY;
	}
	else 
	if (e.clientX || e.clientY)
	{
		posx = e.clientX + document.body.scrollLeft;
		posy = e.clientY + document.body.scrollTop;
	}
	var area = GetArea(areaId);
	area.popareaup(posx, posy);
}
function PopupArea1(e, areaId)
{
	if (e.pageX || e.pageY)
	{
		posx = e.pageX;
		posy = e.pageY;
	}
	else 
	if (e.clientX || e.clientY)
	{
		posx = e.clientX + document.body.scrollLeft;
		posy = e.clientY + document.body.scrollTop;
	}
	var area = GetArea(areaId);
	area.popareaup1(posx, posy);
}

function PopupArea2(e, areaId)
{
	if (e.pageX || e.pageY)
	{
		posx = e.pageX;
		posy = e.pageY;
	}
	else 
	if (e.clientX || e.clientY)
	{
		posx = e.clientX + document.body.scrollLeft;
		posy = e.clientY + document.body.scrollTop;
	}
	var area = GetArea(areaId);
	area.popareaup2(posx, posy);
}
// Function to hide the DIV Layer
function jsAreaClose(areaId)
{
	GetArea(areaId).hide();
	activeAreaId = divHangTimer = null;	
}

var divHangTimer = null;

// Function to keep the Div Layer
// showing for a "period" of time
// after that period, if the mouse
// has been outside the DIV Layer, 
// it will be hidden automatically
function KeepArea(areaId)
{
	if (areaId == activeAreaId && divHangTimer != null)
	{
		clearTimeout(divHangTimer);
		divHangTimer = null;
	}
}

// Function to release the DIV Layer
function RelArea(areaId)
{
	if (areaId == activeAreaId && divHangTimer == null)
		divHangTimer = setTimeout('jsAreaClose(\'' + areaId + '\')', jsAreaShowTime);
}

// Function fired when mouse is over the 
// DIV Layer, used to keep the layer showing
function JsAreaMouseOver(e)
{
	if (!e) 
		var e = window.event;
	var targ = e.target ? e.target : e.srcElement;
	KeepArea(activeAreaId);
}

// Function that fires when mouse is out of
// the scope of the DIV Layer
function jsAreaMouseOut(e)
{
	if (!e) 
		var e = window.event;
	var targ = e.relatedTarget ? e.relatedTarget : e.toElement;
	var activeAreaView = document.getElementById(activeAreaId);
	if (activeAreaView != null && !jsAreaContains(activeAreaView, targ))
		RelArea(activeAreaId);
}
function jsAreaContains(parent, child)
{
	while(child)
		if (parent == child) return true;
		else 
			child = child.parentNode;
		
		return false;
}