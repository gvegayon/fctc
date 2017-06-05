/*
 Nielsen SDK package v5.1.0.17 
 (c) 2017 The Nielsen Company 
*/
/* PLCMB build v5.0.0.15*/
window.NOLCMB.registerLib("PLCMB",function(e){function n(n){function t(e){Ce.CS[e]=0,Ce.PA[e]=0,Ce.PL[e]=0,Ce.SA[e]=0,Ce.FA[e]=0,Ce.FP[e]=0,Ce.SI[e]=0,Ce.MI[e]=0,Ce.SR[e]=0,Ce.SK[e]=0,Ce.CI[e]=0,Ce.DI[e]=0,Ce.ER[e]=0,Ce.SV[e]=0,Ce.SH[e]=0,Ce.PB[e]=0,Ce.II[e]=0,Ce.VL[e]=0,Ce.PM[e]=0,Ce.SS[e]=0}function r(e,n){var t=Number(e),r=Number(n),i=1;return t<r?i=-1:t==r&&(i=0),i}function o(){Pe=V.split(",");var e,n=[];for(e=0;e<Pe.length;e++)isNaN(Pe[e])&&(n=Pe[e].split("%"),isNaN(n[0])?Pe[e]=0:Pe[e]=Number(n[0])*Ce.VL[ie]/100);for(Pe.sort(r),e=Pe.length;--e>0;)for(;Pe[e]==Pe[e-1];)Pe.splice(e-1,1);0==Pe[0]&&Pe.splice(0,1)}function a(e){return e>86400&&(e=(e-P.gmtOffset)%86400),e}function u(e,n){var t,r,i=-1;return t=1==n?se:ae,"undefined"!=typeof t&&(r=t.indexOf("<"+e+">"),r>=0&&(r+=e.length+2,i=t.indexOf("</"+e+">",r)),r>=0&&i>r)?t.substring(r,i):null}function g(e){var n,t,r,i;return"undefined"!=typeof Ce&&0!==Object.keys(Ce).length||(Ce={Title:["",""],VL:[0,0]}),1==c._ggmsgfmt?n="":(n="",ue.prod.indexOf("iag")>=0&&(n=oe?K:X,2==e?n+="&pr=iag.cp,cep":1==e?n="":n+="&pr=iag.cp,soc"),ue.prod.indexOf("vc")>=0?1==e?n="&ig=1":(t=u("censuscategory",oe),null!=t&&""!=t&&(n+="&cg="+encodeURIComponent(t)),oe&&(n+="&c3=st,a"),1!=e&&(n+=oe?Y:W),n+="&tl="+encodeURIComponent("dav"+e+"-"+Ce.Title[oe].substr(0,128))):ue.prod.indexOf("sc")>=0&&(t=u("censuscategory",oe),null!=t&&""!=t&&(n+="&cg="+encodeURIComponent(t)),oe&&(n+="&c3=st,a"),n+="&ou="+ce+"_"+encodeURIComponent($.substr(0,128)),0==e?Ce.VL[oe]>0&&(n+="&sd="+Math.round(Ce.VL[oe])):(i=Math.round(Ce.PM[oe]),n+="&du="+i),1!=e&&(n+=oe?Y:W,n+="&tl="+encodeURIComponent("dav"+e+"-"+Ce.Title[oe].substr(0,128)))),0==e&&(n+="&rnd="+Math.floor(1e5*Math.random()),r=u("totalchap",oe),null!=r&&""!=r&&(n+="&tc="+r)),n+="&ch="+(u("chapter",oe)||1),n+="&sq="+(u("sequence",oe)||1),n+="&tp=gg"),n}function d(e,n,t){var r=u("iag_"+e,t);return null!=r&&""!=r?"&pr=iag."+e+","+encodeURIComponent(r):null==n?"":n}function p(e,n,t,r,i){var o,a,s,l,c,g,f,p,h,m,v,C,b,w,S,y,P;if(ue.prod.indexOf("iag")>=0){if(o="",e){if(K="",2==ue.iagads)return}else if(X="",2==ue.iagcontent)return;a=void 0!=ue.sid?"&pr=iag.sid,"+ue.sid:"",s=void 0!=ue.tfid?"&pr=iag.tfid,"+ue.tfid:"",l="&pr=iag.bcr,"+ue.clientid,0!=e&&"preroll"==t||(P=u("iagcategory",0),null!=P&&""!=P||(P=u("category",0)),c=null!=P&&""!=P?"&pr=iag.pgm,"+encodeURIComponent(P):"&pr=iag.pgm,general",P=u("title",0),g=null!=P&&""!=P?"&pr=iag.epi,"+encodeURIComponent(P):"&pr=iag.epi,"+encodeURIComponent(Ce.Title[0].substr(0,255)),f="&pr=iag.seg,",f+=i>1?encodeURIComponent(i):"1",P=u("pd",0),null==P&&(P=ue.pd),p=null!=P&&""!=P?"&pr=iag.pd,"+encodeURIComponent(P):"",P=u("oad",0),h=null!=P&&""!=P?"&pr=iag.oad,"+encodeURIComponent(P):"",a=d("sid",a,0),s=d("tfid",s,0),l=d("bcr",l,0),c=d("pgm",c,0),g=d("epi",g,0)),b=d("fp",b,e),y=d("cust1",y,e),e?(m="&pr=iag.brn,"+ue.clientid,v="&pr=iag.cte,"+encodeURIComponent(n),C="midroll"==t?"&pr=iag.ap,mid":"postroll"==t?"&pr=iag.ap,post":"&pr=iag.ap,pre",a=d("sid",a,e),s=d("tfid",s,e),l=d("bcr",l,e),m=d("brn",m,e),v=d("cte",v,e),c=d("pgm",c,e),g=d("epi",g,e),f=d("seg",f,e),p=d("pd",p,e),h=d("oad",h,e),w=d("pod",w,e),S=d("apt",S,e),K=a+s+l+c+g+f+p+m+C+v+h+b+w+S+y):X=a+s+l+c+g+f+p+h+b+y}}function h(e,n,t,r,i){var o="",a,s=0,u=!1,l,c,g,d,f;for(e?Y="":W="",a=1==e?se:ae;0==u;){if(s=a.indexOf("<nol_",s),s<0){u=!0;break}if(l=a.indexOf(">",s),l<0){u=!0;break}if(g=a.substring(s+5,l),c=a.indexOf("</",l+1),c<0){u=!0;break}d=a.substring(l+1,c),s=c+7,f=encodeURIComponent(g),f.indexOf("%")<0&&g.length>32&&(g=g.substring(0,31)),d.length>254&&(d=d.substring(0,253)),g.indexOf("raw_")>-1?(g=g.substr(4),o+="&"+g+"="+d):o+="&"+g+"="+encodeURIComponent(d)}e?Y=o:W=o}function m(){var e="NA";return(k>0||O>0)&&(e=Math.round(100*k/(k+O))+"%",I&&(e="u"+e)),e}function v(e){var n=g(e);_.report(m(),e,n),L=0,we=e}function C(){if(Se){var e=g(1);return void _.reportend(e)}}function b(){A=!1,Ce.FA[0]=Ce.FA[1]=0,Ce.FP[0]=Ce.FP[1]=0}function w(e){var n="",t,r=ie;return t=Ce.FP[r]>0&&1==A?Math.round(100*Ce.FA[r]/Ce.FP[r]):100,n=null!=e&&""!=e?e:Ce.PL[r],n+=","+t,Number(Ce.PM[r])<e&&(Ce.PM[r]=e),n}function S(e){var n,t;try{return n=new ActiveXObject("Microsoft.XMLDOM"),n.async="false",n.loadXML(e),n}catch(r){try{return t=new DOMParser,n=t.parseFromString(e,"text/xml")}catch(e){}}return null}var y,P=n,N=f,M,_,L=0,E,I=!1,A=!1,k=0,O=0,x=5,F=0,V="",B=1300,D,R,G,U=0,T,Q,j,H,J=0,q="",z=0,X="",K="",W="",Y="",Z="",$="",ee=0,ne=!1,te=10,re="0",ie=0,oe=0,ae="",se="",ue={},le=0,ce="",ge=-1,de=0,fe=0,pe=0,he=[0,0],me=0,ve=0,Ce={CS:new Array(0,0),PA:new Array(0,0),PL:new Array(0,0),SA:new Array(0,0),FA:new Array(0,0),SI:new Array(0,0),SR:new Array(0,0),SK:new Array(0,0),CI:new Array(0,0),DI:new Array(0,0),ER:new Array(0,0),SV:new Array(0,0),SH:new Array(0,0),PB:new Array(0,0),II:new Array(0,0),VL:new Array(0,0),PM:new Array(0,0),FP:new Array(0,0),MI:new Array(0,0),Title:new Array("",""),SS:new Array(0,0)},be=!1,we=2,Se=!1,ye=1,Pe=[],Ne=window.location.protocol||"http:",Me=this,_e=[];this.getmetval=function(){return Ce},this.getgp=function(){return ue},this.getpageurl=function(){return $},this.getaorc=function(){return oe},this.getlfunctionType=function(){return U},this.detectsBrowserClose=function(){return be},this.getGGinstance=function(){return P},this.updateFocusNumbers=function(){A=e.globalHasFocus(),1==le&&(1==A&&(Ce.FA[ie]+=1),Ce.FP[ie]+=1)},this.genSummation=function(n,t){var r="",i=0,o=1,a,s,u=0,l,c=-1,g=-1;return n=ie,0!=Ce.CS[n]&&Ce.PA[n]>0&&Ce.VL[n]>0&&(i=Ce.PA[n]>=Ce.VL[n]?99:Ce.VL[n]>0?Math.round(100*Ce.PA[n]/Ce.VL[n]):66,Ce.FP[n]>0&&1==A&&(o=Ce.FA[n]/Ce.FP[n],i=Math.round(.8*i+i*o*.2)),Ce.MI[n]>0&&Ce.SI[n]<=0?i-=10:Ce.SI[n]>0&&(i+=5),Ce.SR[n]>0&&(i+=10),(Ce.SH[n]>0||Ce.SV[n]>0||Ce.PB[n]>0||Ce.CI[n]>0)&&(i=99),Ce.II[n]>0&&(i+=10),Ce.ER[n]>0&&Ce.ER[n]<=5&&(i=20*Ce.ER[n]),i>99&&(i=99),r=t+",50,"+i+","+Math.round(100*o)+"|||",this.getGGinstance().player.PLVBL&&this.getGGinstance().player.PLVBL.getData()&&(e.childGlobal===e.parentFound&&(s=this.getGGinstance().player.PLVBL.getData(),s.cumm.samples>1&&(u+=s.cumm.samples,c=s.cumm.score/Ce.VL[n]*100>=s.cummRule.viewabilitylength?1:0,g=Math.round(s.cumm.totalViewPercent/u))),r=r+t+",53,"+g+","+c+"|||"),Ce.CS[n]=0),de>0&&(r=t+",25,"+de+","+fe+","+pe+"|||"+r,de=0,fe=0,pe=0),r},this.processEvent=function(e,n,r,i,s,l){try{var c,g=0,d,f,C,S,y,P,N,E,I,A,k,O,x=e;if(U==x&&T==r&&Q==i&&j==s&&H==l)return;if(isFinite(r)&&(r=a(Number(r))),isFinite(i)&&(i=a(Number(i))),U=x,T=r,Q=i,j=s,H=l,ne=!1,c=0==e.indexOf("cust:")?M.genericEvent(e,n,r,i,s,l):M.glanceguideEvent(e,n,r,i,s,l),null==c||""==c)return;switch(I=c.split(","),x=Number(I[1]),r=I[2],i=I[3],oe=ie,x){case 1:return Z=c,void(null!=T&&($=T));case 51:if(Z)return Z=n+",1,"+T,null!=Q&&""!=Q&&(Z+=","+Q),void(null!=T&&($=T));c=n+",51,"+T,Q&&(c+=","+Q),$=T;break;case 2:break;case 3:case 15:Se=!1,ee=0,z=0,i=c.toLowerCase().indexOf("<type>content")===-1&&c.toLowerCase().indexOf("<vidtype>content")===-1?"ad":"content","preroll"==i||"postroll"==i||"midroll"==i||"ad"==i?(C=1,se=c):(C=0,ae=c),3===x&&"content"===i&&(_e.push(c.substring(c.indexOf(","))),c="");var F={type:u("type",C),vidtype:u("vidtype",C)};ce=null!==F.type?F.type:null!==F.vidtype?F.vidtype:0,ie=C,oe=C,t(C),15==x&&(Ce.CS[C]=1,le=1),A=unescape(u("length",ie));var V=u("ou",ie);V&&V.length>0&&($=V),A=null==A||isNaN(A)?30:Number(A),Ce.VL[ie]=A,f=u("title",ie),null!=f&&""!=f?Ce.Title[ie]=f:Ce.Title[ie]=r,o(),ve=0,Ce.SS[ie]=ve,ye=u("chapter",ie)||1,p(C,r,i,s,l),h(C,r,i,s,l),Se=!0;break;case 4:ee=0,S=r,S=isNaN(S)||0==S?Ce.PL[ie]:a(Number(r)),C=S-Ce.PL[ie],Ce.PA[ie]+=C,Ce.PL[ie]=S,Number(Ce.PM[ie])<S&&(Ce.PM[ie]=S),d=this.genSummation(ie,n),ie=0;var F={type:u("type",ie),vidtype:u("vidtype",ie)};ce=null!==F.type?F.type:null!==F.vidtype?F.vidtype:"content",le=0,ve=0;break;case 5:if(0===ie&&he&&3==he[ie]&&_e.length>0){var D=String(n-1)+String(_e.pop());ce="content",c=""!==D?D+"|||"+c:c}if(ee++,S=r,ve=0,isNaN(S)){d="";break}S=a(Number(r)),0==S&&0!=Ce.PA[ie]||(Ce.PL[ie]=S),Ce.CS[ie]=1,le=1,_.updateViewState(w(S)),Ce.SS[ie]=ve;break;case 6:if(ee++,S=r,isNaN(S)){d="";break}S=a(Number(r)),C=S-Ce.PL[ie],C>0&&(Ce.PA[ie]+=C),Ce.PL[ie]=S,Ce.CS[ie]=1,le=0,_.updateViewState(w(S));break;case 7:if(Ce.CS[ie]=1,z++,1==Ce.SS[ie]||null==Ce.SS[ie])return;if(S=r,isNaN(S)){d="";break}S=a(Number(r)),C=Number(S)-Ce.PL[ie],C>0&&(Ce.PA[ie]+=C),Ce.PL[ie]=S,Number(Ce.PM[ie])<S&&(Ce.PM[ie]=S),Ce.PA[ie]>0&&(d=this.genSummation(ie,n)),le=0,ve=1,Ce.SS[ie]=ve,Ce.PA[ie]=0,Ce.FA[ie]=0,Ce.FP[ie]=0,0==ie&&o();break;case 8:if(ee++,S=r,isNaN(S)){d="";break}if(S=a(Number(r)),y=i,isNaN(y)){d="";break}y=Number(i),C=S-Ce.PL[ie],C>0&&(Ce.PA[ie]+=C),Ce.PL[ie]=y,Ce.CS[ie]=1,_.updateViewState(w(y));break;case 9:r.toLowerCase().indexOf("true")>-1||r===!0||r.toLowerCase().indexOf("on")>-1||1==r?Ce.MI[ie]+=1:Ce.MI[ie]-=1;break;case 10:(r||r.toLowerCase().indexOf("on")||1==r)&&(Ce.SR[ie]+=1),b();break;case 11:isNaN(r)||(ge<0?ge=r:(r>ge&&(Ce.SI[ie]+=1),ge=r));break;case 12:Ce.DI[ie]+=1;break;case 13:case 14:break;case 16:case 22:Ce.CI[ie]+=1;break;case 17:Ce.SH[ie]+=1;break;case 18:Ce.SV[ie]+=1;break;case 19:case 20:Ce.PB[ie]+=1;break;case 21:Ce.II[ie]+=1;break;case 23:Ce.ER[ie]+=1;break;case 24:break;case 25:return de+=1,fe+=a(Number(r)),void(pe<fe&&(pe=fe));case 49:if(isNaN(r))return;if(S=a(Number(r)),S<=1||1!=le)return;if(_.updateViewState(w(S)),0==Pe.length)return;if(k=Ce.PA[ie]+(S-Ce.PL[ie]),0!==Pe.length&&k<Pe[0])return;Ce.PA[ie]+=S-Ce.PL[ie],Ce.PL[ie]=r,d=this.genSummation(ie,n),Pe.splice(0,1)}if(O=10,O=isNaN(Ce.VL[ie])?10:Ce.VL[ie]/60*te,O>100&&(O=100),O<20&&(O=20),1==ne)return;if(ee>O&&3!=x&&15!=x&&4!=x&&7!=x)return;if(ee>5e3)return;if(null!=d&&d.length>0&&(c=d+c),""!==Z&&""!==c&&(c=Z+"|||"+c,Z=""),N=1!=oe?Number(ue.trackcontent):Number(ue.trackads),!isNaN(N))switch(N){case 0:return;case 2:case 3:if(E=u("ggignr",oe),null!=E&&"1"==E)return}""!==c&&(g=_.addMessage(c,m())),L+=g,P=he[ie],7==x?(v(2),0==ie?me=1:(oe=0,ie=0),P=he[ie]):49==x?(v(1),P=0):15==x?(v(0),P=0,0==ie&&(me=0)):3==x?5==he[ie]?(v(0),P=0,0==ie&&(me=0)):P=3:5==x?3==he[ie]?(v(0),P=0):0==r?1==me&&0==ie?(Ce.PM[ie]=0,v(0),P=0,me=0):P=5:P=0:6==x&&0!=r?P=0:8==x&&0!=i&&(P=0),he[ie]=P,Number(L)>=Number(B)&&v(1),this.processGeneric=function(e,n,t,r,i,o){this.processEvent("cust:"+e,n,t,r,i,o)}}catch(e){}},this.init=function(n,t,r,o,a){var u=S("<vi>"+o+"</vi>"),g=u.firstChild.firstChild,d=0;for(re=t,this._detectsBrowserClose=a;void 0!=g&&d<20;)null!=g.firstChild&&(ue[g.nodeName]=g.firstChild.nodeValue),g=g.nextSibling,d++;1!=c._ggmsgfmt&&(r=void 0==ue.sfcode||ue.sfcode.length<2?Ne+"//secure-gl.imrworldwide.com/cgi-bin/m?":Ne+"//secure-"+ue.sfcode+".imrworldwide.com/cgi-bin/m?",r+="ci="+ue.clientid,void 0!=ue.cisuffix&&""!=ue.cisuffix&&(r+=ue.cisuffix),void 0!=ue.vcid&&""!=ue.vcid&&(r+="&c6=vc,"+ue.vcid),void 0!=ue.partner&&""!=ue.partner&&(r+="&c4=mn,"+encodeURIComponent(ue.partner)),r+="&cc=1"),y=r,void 0==ue.prod&&(ue.prod="vc"),this._ggtrackid=ue.clientid,null!=ue.vcid&&(this._ggtrackid+="."+ue.vcid),M="genjsplayer"==n?new l:new s,this._ggplayername="gj3",null!=ue.msgmax&&ue.msgmax>0&&(B=ue.msgmax),null!=ue.msgint&&""!=ue.msgint&&(V=ue.msgint),null!=ue.focusint&&ue.focusint>=0&&(x=ue.focusint),this._detectsBrowserClose&&(window.navigator.appName.toLowerCase().indexOf("safari")!=-1||window.navigator.userAgent.toLowerCase().indexOf("firefox")!=-1?window.addEventListener("pagehide",C,!0):window&&window.NOLCMB&&window.NOLCMB.browserSafeAddEventListener({element:window,eventType:"beforeunload",func:C,useCapture:!0})),_=new i(N,t,y,(!0),(!0),Me),0!=x&&(I=e.globalHasFocus(),R=window.setInterval(this.updateFocusNumbers,1e3*x)),this.processEvent("1",(new Date).getTime(),window.location.href,window.document.referrer)}}function t(){function e(e,n){}function n(e){}function t(e){}function r(e){var n="",t="",r;for(r=0;r<e;r++)t=Math.floor(36*Math.random()).toString(36),n+=Math.floor(2*Math.random())?t.toUpperCase():t.toLowerCase();return n}var i="|||",o="_ggCvar",a="_ggMCvar",s=45e3,u=null,l=0,c=31536e6;this.makeUserIDCookie=function(e){return u=r(32)},this.makeNewMessageCookie=function(){},this.clearMessageCookie=function(){},this.getMessageCookieSequenceNumber=function(){return l++,l},this.getUserId=function(){return u}}function r(e,n,t,r,i){var o='END"/></GGC>',a="|||",s="^|^^",u="",l=Math.floor(1e5*Math.random())+1,c='<GGC><H value="'+e+","+(new Date).getTimezoneOffset()/-60+","+n+"."+l+","+r+","+i._ggplayername+'"/><L value="';this.getCurrentMessage=function(){return u},this.makePartialMessage=function(e){return""==u?u=c+e+a:u+=e+a,u},this.makeMessages=function(e){var n;return""==u?u:(n=u+o,u="",n)},this.makeUnloadMessages=function(e){var n;return""!=e?(n=e.split(","),i.processEvent("7",(new Date).getTime(),n[0]),null):(""==u&&(u=c),u+(new Date).getTime()+",2"+a+o)}}function i(e,n,i,s,u,l){var c="logthisjs.php",g=i,d="logthisjs.php",f="postjs.php",p=new t,m=null,v="NA",C=!1,b=p.getMessageCookieSequenceNumber(),w,S="GET",y=l,P=new o(i,i+d,i+f,S),N=new a,M,_='|||END"/></GGC>',L='END"/></GGC>',E="^|^^",I="|||",A,k,O="",x=String.fromCharCode(56)+String.fromCharCode(103)+String.fromCharCode(36)+String.fromCharCode(15)+String.fromCharCode(126)+String.fromCharCode(3)+String.fromCharCode(71)+String.fromCharCode(91)+String.fromCharCode(100)+String.fromCharCode(7)+String.fromCharCode(17)+String.fromCharCode(31)+String.fromCharCode(95)+String.fromCharCode(28)+String.fromCharCode(64)+String.fromCharCode(14),F=0;null!=s&&(C=s),C=!1,p.makeUserIDCookie(n),p.makeNewMessageCookie(),null==n&&(n="0"),m=new r(e,n,b,y._ggtrackid,y),M=0==h?0:2,A=N.encrypt(x,L),this.getMessage=function(){if("undefined"!=typeof m)return m.getCurrentMessage()},this.getSeqNumber=function(){return b},this.report=function(e,n,t){v=e,k=m.makeMessages(e),k="<m v="+M+" c="+p.getUserId()+">"+N.encrypt(x,k)+"<%2Fm>",P.report(k,t),0==n&&(F=1)},this.reportend=function(e){k=m.makeUnloadMessages(O),k&&(k="<m v="+M+" c="+p.getUserId()+">"+N.encrypt(x,k)+"<%2Fm>",P.report(k,e))},this.addMessage=function(e,n){var t=m.makePartialMessage(e),r=t.length;return v=n,r},this.updateViewState=function(e){var n;y._detectsBrowserClose&&(n=y.getlfunctionType(),5!=n&&8!=n||(e=""),O=e)}}function o(e,n,t,r){function i(e,n){var t=s,r,i,o,a,u=e;c=!0,"GET"==f&&(p=new Image(1,1)),1==m?p.src=t+n+"?HEX40="+e:p.src=t+n+"HEX40%3D"+e}var o=2,a=10,s=e,u=n,l=t,c=!1,g,d=0,f="GET-CONFIRM",p=new Image(1,1),h=null;null!=r&&(f=r),this.report=function(e,n){null!=e&&""!=e&&"POST"!=f&&i(e,n)}}function a(){function e(e,n){var t="",r,i,o,a,s,u;if(null==e||null==n)return n;for(r=e.split(""),i=r.length,o=n.split(""),a=o.length,u=0;u<a;u++)s=o[u].charCodeAt(0)^(a%10|r[u%i].charCodeAt(0)),t+=0==s?o[u]:String.fromCharCode(s);return t}function n(e){var n,t,r,i;for(i="",r="undefined"!=typeof e&&e.length>0?e.length:0,n=0;n<r;n++)t=e.charCodeAt(n),t<128?i+=String.fromCharCode(t):t>127&&t<2048?(i+=String.fromCharCode(t>>6|192),i+=String.fromCharCode(63&t|128)):t<65536?(i+=String.fromCharCode(t>>12|224),i+=String.fromCharCode(t>>6&63|128),i+=String.fromCharCode(63&t|128)):(i+=String.fromCharCode(t>>18|240),i+=String.fromCharCode(t>>12&63|128),i+=String.fromCharCode(t>>6&63|128),i+=String.fromCharCode(63&t|128));return i}this.urlencode=function(e){var n={},t=[],r,i=e.toString(),o,a=function(e,n,t){var r=[];return r=t.split(e),r.join(n)};n["'"]="%27",n["("]="%28",n[")"]="%29",n["*"]="%2A",n["~"]="%7E",n["!"]="%21",n["%20"]="+",i=encodeURIComponent(i);for(o in n)n.hasOwnProperty(o)&&(r=n[o],i=a(o,r,i));return i.replace(/(\%([a-z0-9]{2}))/g,function(e,n,t){return"%"+t.toUpperCase()})},this.encrypt=function(t,r){return this.urlencode(e(t,n(r)))}}function s(){function e(e){i=e}function n(e,n,t,r,i,o){var a=n+","+e;return null==e||e.length<=5?null:(null!=t&&(a+=","+t),null!=r&&(a+=","+r),null!=i&&(a+=","+i),null!=o&&(a+=","+o),a)}this._currentEvent=-1,this._currentDuration="NA";var t=-1,r=null,i=null,o=null,a=null,s=null,u=null,l=null,c=null,g=null,d=null,f=null,p=null;this.getCurrentEvent=function(){return this._currentEvent},this.genericEvent=function(e,t,r,i,c,g){var d;return o==e&&a==r&&s==i&&u==c&&l==g?null:(d=n(e,t,r,i,c,g),null!=d&&(o=e,a=r,s=i,u=c,l=g),d)},this.glanceguideEvent=function(n,i,o,a,s,u){var l=null;return c==n&&g==o&&d==a&&f==s&&p==u?null:(1==n?(this._currentEvent=n,r=o,l=i+","+this._currentEvent+","+r,null!=a&&(l+=","+a)):10==n||9==n?(this._currentEvent=n,l=0==o?i+","+this._currentEvent+",0":1==o?i+","+this._currentEvent+",1":i+","+this._currentEvent+","+o):12==n?(this._currentEvent=n,l=i+","+this._currentEvent):49==n?(this._currentEvent=n,t=o,l=i+","+this._currentEvent+","+o):11==n?(this._currentEvent=n,l=i+","+this._currentEvent+","+o):"videoInfo"==n?e(o):l=this.processEvent(n,i,o,a,s,u),null!=l&&(c=n,g=o,d=a,f=s,p=u),l)},this.processEvent=function(e,n,t,r,i,o){},this.getVideoInfo=function(e,n){var t="";return null!=e&&(t="<length>"+e+"</length>"),null!=n&&(t+="<uurl>"+n+"</uurl>"),null!=i&&(t+=i,i=null),t}}function u(e,n){var t=e.indexOf("<"+n+">")+n.length+2,r=e.indexOf("</"+n+">",t),i=null;return t>=0&&r>t&&(i=e.substring(t,r)),i}function l(){this.inheritFrom=s,this.inheritFrom();var e=null,n=null;this.processEvent=function(t,r,i,o,a,s){var l="",c,g,d;return 3==t&&3!=this._currentEvent||15==t&&15!=this._currentEvent?(c=u(a,"censuscategory"),g=u(a,"category"),null==g&&null!=c&&""!=c&&(a+="<category>"+c+"</category>"),this._currentDuration=u(a,"length"),l+=","+i+","+o+","+a,l+=s&&""!=s&&!isNaN(s)?","+s:",1",e=15!=t&&e):1==t||51==t||8==t?(l+=","+i,null!=o&&(l+=","+o)):6==t&&1!=e&&"00:00"!=i?(e=!0,n=i,l+=","+n):5==t&&0!=e?(e=!1,l+=","+i):7==t?(e=!0,n="0",l+=","+i):9==t?l+=0==i?",Off":",On":(l+=","+i,null!=o&&o.length>0&&(l+=","+o)),""==l?d=null:(this._currentEvent=t,d=r+","+this._currentEvent+l),d}}var c=e.PLCMB=e.PLCMB||{},g="5.0.0",d="15",f=e.getBaseBuildVer?e.getBaseBuildVer()+d:g+d,p="",h=1,m=2,v=!1;return c.init=function(t){var r={_playerID:null,gmtOffset:60*(new Date).getTimezoneOffset(),ggJsMet:null,ggLoaded:!1,ggEventQue:[],playerType:"genjsplayer",_nolggGlobalParams:{},_detectsBrowserClose:!1,_canDetectBrowser:!0,_bStr:"",_bLoc:"",_isFireFox:!1,bVer:"",player:t,ggInitialize:function(t){var i="",o=t||{};r._nolggGlobalParams=o.ggParams,r._canDetectBrowser=void 0!==o.detectBrowser?o.detectBrowser:r._canDetectBrowser,r._playerID=o.nol_playerId||o.playerId,r._canDetectBrowser&&r.detectBrowser();for(var a in r._nolggGlobalParams)r._nolggGlobalParams.hasOwnProperty(a)&&(i+="<"+a+">"+r._nolggGlobalParams[a]+"</"+a+">");r.ggJsMet=new n(r),r.ggJsMet.init(r.playerType,o.uid,c.GGADDRESS,i,r._detectsBrowserClose),e._listeners.addListener("ggPM",function(e){r.ggPM(e)})},detectBrowser:function(){r._detectsBrowserClose=!0,r._bStr=window.navigator.userAgent,r._bLoc=r._bStr.indexOf("Firefox"),null!=r._bStr&&r._bLoc>=0&&(r._isFireFox=!0,r.bVer=r._bStr.substring(r._bLoc+8),Number(r.bVer)<3&&(r._detectsBrowserClose=!1)),r._bLoc=r._bStr.indexOf("MSIE"),null!=r._bStr&&r._bLoc>=0&&(r.bVer=r._bStr.substring(r._bLoc+5),Number(r.bVer)<6&&(r._detectsBrowserClose=!1))},ggPM:function(e){if(r._playerID==e.target.id){var n=e.target.evtInfo,t=String(n.eventType),i=String(n.param1),o=String(n.param2),a=String(n.param3),s=String(n.param4);if(!("|14|56|48|".indexOf("|"+t+"|")!==-1||isNaN(t)&&"videoInfo"!==t))try{if(null!=r.ggJsMet){if(i.toLowerCase().indexOf("object")!==-1){i=n.param1.uurl||"",o=n.param1.type||n.param1.vidtype,a="";for(var u in n.param1)n.param1.hasOwnProperty(u)&&(a+="<"+u+">"+n.param1[u]+"</"+u+">")}r.ggJsMet.processEvent(t,(new Date).getTime(),i,o,a,s)}else r.ggEventQue.push(t),r.ggEventQue.push((new Date).getTime()),r.ggEventQue.push(i),r.ggEventQue.push(o),r.ggEventQue.push(a),r.ggEventQue.push(s)}catch(e){}}},ggJsLoaded:function(){if(null!=r.ggJsMet){for(var e=0,n=r.ggEventQue.length;e<n;e+=6)r.ggJsMet.processEvent(r.ggEventQue[e],r.ggEventQue[e+1],r.ggEventQue[e+2],r.ggEventQue[e+3],r.ggEventQue[e+4],r.ggEventQue[e+5]);r.ggEventQue=[]}}};r.ggInitialize(t.getConfigParams()),t.hasOwnProperty("emptyQueue")&&t.emptyQueue()},c.built||(c.built=!0,e.bindPlayers(c,"PLCMB")),c});