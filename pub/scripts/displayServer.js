// Copyright (2005) David Ritchie.


function gt(a,b) {return a>b};

// this is a variant of the standard $ function which allows us to get elements by name without having to give them IDs
function $e(a) {
    return (parent.document.getElementById(a) || parent.document.getElementsByName(a)[0]);
}

function formSubmitValue(element) {
    if(!element.name) {return undefined}
    if(element.value === undefined) {return undefined}
    
    // if the button is pressed then the form would be posted anyway. All other elements are undefined...
    if(element.type == "submit") {return undefined}
    
    if(element.type == "select-multiple") 
    {
        return Array.prototype.slice
            .call(element.selectedOptions)
            .map(function (o) { return o.value; });
    }
    if(element.type == "select-one")
    {
        return element.value;
    }
    if(element.type == "checkbox" || element.type == "radio") {
        if(! element.checked) {return undefined}
    }

    // this works for text areas
    return(element.value || element.innerHTML);
}

// now, using the above I can get all the values that would need to be posted...
function allFormSubmitValues(/* form */) {
    // var f = form || document.forms[0]; // default to the first form we find (often the only one)
    var x = {};
    // var elements = f.elements;
    var value = undefined;
    var i;
    var f;
    // I've modified this to get the elements for every form. Really that's the useful thing to do if we want to embed AJAXing monadic stuff into normal pages
    // it makes things simpler (although we might send a lot of stuff)
    for(f=0; f<document.forms.length; f++) {
        elements = document.forms[f];
        for(i=0; i<elements.length; i++) {
            value = formSubmitValue(elements[i]);
            if (!(value === undefined)) {
                x[elements[i].name] = value;
            }
        }
    }
    return x;
}


function triggerServerEvent(handler, event, value) {
  /*
   * // JH 05-05-2015 - we could do the following to handle radio buttons
    var serializedData = "";
    jQuery("form").each(
                  function() {
                      serializedData += jQuery(this).serialize();
                      serializedData += "&";
                  });
    
    serializedData += event + "=" + value;
    serializedData += "&__AJAX=1";
    rcall(handler, serializedData);
   * 
   */
    var x = allFormSubmitValues();
    x[event] = value;
    x['__AJAX'] = 1;
    rcall(handler, glow.data.encodeUrl(x));
}

// !!! This is required to properly evaluate JS instructions. This may have been the cause of many problems...
function unescapeHTML(html) {
   var htmlNode = document.createElement("DIV");
   htmlNode.innerHTML = html;
   if(htmlNode.innerText !== undefined)
      return htmlNode.innerText; // IE
   return htmlNode.textContent; // FF
}

// shorthand
function ev(b,c,action) {triggerServerEvent((action || document.forms[0].action), b, c)}


// This gets an element by ID with far fewer characters. The parent is to make it work from within iframes
function $w2(a) {
    //alert(a);
return ( parent.document.getElementById(a) || parent.document.getElementById('_'+a) );
}

function $$(a) {
    return $w2('_'+a+'_c');
}

// this should probably be moved elsewhere. It makes the popup login window for a page...
function loginWindow(elt) {
    elt.onclick='';
    elt.innerHTML='<table style="position:fixed;top:0;left:0;width:100%;height:100%;z-index:10000"><tr valign="center"><td align="center"><div style="position:relative; width:200; height:100; background-color:black; opacity:0.9; color:white; padding:20px;"><form method="post" action="http://www.websynth.com/apps/login/auth"><div><b>Login</b></div><div><input name="username"></input></div><br/><div><input name="password" type="password"></input></div><div><input type="submit" value="login"></input></div></form></div></td></tr></table>';
}


// set stylesheet styles
function setStyle(e, toChange, changeTo) {
    var r;
    for(i = 0; document.styleSheets.length > i; i++) {
        r = (document.styleSheets[i].rules || document.styleSheets[i].cssRules);
        if(r != undefined) {
            for(j = 0; r.length > j; j++) {
                if(r[j].selectorText.toLowerCase() == e) {
                    r[j].style[toChange] = changeTo; } }
        } else {
            // alert("Your browser won't support the changing of a style sheet by javascript!");
        } } }


// global flag
var isIE = false;

// global request and XML document objects
// *** We have a problem if there is >1 request in flight at a time. We need to handle that methinks.
var req;
var URL;

// new, more betterer rcall using prototype. Hopefully this will handle >1 request at a time
function rcall(url,data, contentType, elem) {
    var spinner;
    if(!(!!window.MSInputMethodContext && !!document.documentMode)) {
        if (elem) {
            spinner = document.createElement('span');
            spinner.innerHTML = ' <i class="fa fa-spinner fa-spin"></i>';
            if (elem.tagName === 'INPUT') {
                elem.parentNode.insertBefore(spinner, elem.nextSibling);
            } else {
                if(typeof(elem.appendChild)==='function') {
                   elem.appendChild(spinner);
                }
            }
        }
    }

    myAjax = new Ajax.Request(url, 
                              {
                                  method: ((data!=undefined)?'post':'get'), 
                                  parameters: '', 
                                  onComplete: function (param) {
                                      if (spinner) {
                                          spinner.remove();
                                      }
                                      processReqChange(param);
                                  },
                                  postBody: data,
                                  contentType: ((contentType!=undefined)?contentType:'application/x-www-form-urlencoded') 
                              });
}

// http://www.html.com/forums/javascript/60030-different-regular-expression-splitting-firefox-ie.html
function mySplit(string, regexp) {
  var offset = 0;
  var collection = [];
  string.replace(regexp, function(m,g,i,s) {
    collection.push(string.substring(offset, i), g);
    offset = i + m.length;
  });
  collection.push(string.substring(offset));
  return collection;
}

DEBUG = false;

// handle onreadystatechange event of req object
function processReqChange(req_param) {
    // if we are passed a request parameter use that.
    var req = req_param || req;
    // only if req shows "loaded" (readyState=="complete")
    if (req.readyState == 4) {
        // only if "OK"
        //alert(req.status);
        // alert(req.responseText);
        //alert(nodeInnerXML(req.responseXML));
        if (req.status == 200) {
            if(DEBUG) {
                console.log('Received response: '+req.responseText);
            }
            //alert(req.responseText);
            // now we have to deal with the reply...
            if (req.responseText.substring(0,1) == "<") {
                // split on commands...
                // Maybe I could make this more efficient like this:-
                var s = req.responseText;
                // alert(s);
                s=s.replace(/^.*<l>/,"");
                s=s.replace(/<\/l>$/,"");
                // ie split doesn't work:-(
                if(inIE()) s=mySplit(s,/(<\/?(?:rp|rm|ap|x).*?>)/g);
                else s=s.split(/(<\/?(?:rp|rm|ap|x).*?>)/);
                // s=s.split(/^(<(?:rm|rp|ap) .*?>.*?<\/(?:rm|rp|ap>))/,"");
                //alert(s);
                var tok;
                var id;
                // a.innerHTML='<p>hi&lt;</p>';
                // <rm id='123'></rm>
                // change protocol???
                // rm|123||rp|45|<p>hello</p>
                // *** MUST COLLECT SCRIPTS...
                var i=0;
                while (i<s.length) {
		    tok = s[i];
                    if(DEBUG) {
                        console.log('Processing: '+tok);
                    }
                    if(tok.substring(0,1)!="<") {
                        // alert(tok + '--' + i);
			//alert('about to execute JS...');
			var u = unescapeHTML(tok);
			//alert('unescaped = ' + u);
                        // console.log('unescape = '+u);
                        eval(u);
			//alert('done');
                    }
                    // alert("|"+tok.substring(0, 4)+"|");
                    if(tok.substring(0, 4) == "<rp ") {
                        // alert(tok);
                        id=tok.replace(/.*[\'\"](.*)[\'\"].*/,"$1");
                        // alert(id);
                        var obj = $w2(id);
                        if(obj) {
                           $w2(id).innerHTML = s[i+1];
                           // evaluate any html <script> tags - TEST ME!
                           s[i+1].evalScripts();
                        } else {
                           console.log("Can't find element "+id);
                        }
                        while (s[i]!="</rp>" && i<=s.length) i=i+1;
                    }
                    if(tok.substring(0, 4) == "<ap ") {
                        //alert(tok);
                        id=tok.replace(/.*[\'\"](.*)[\'\"].*/,"$1");
                        //$w2(id).innerHTML += s[i+1];
                        // NOTE the line above brakes js for existing elements
                        var tmp=document.createElement('div');
                        tmp.innerHTML=s[i+1];
                        //alert(tmp.childNodes.length + "|" + s[i+1]);
                        var len=tmp.childNodes.length;
                        for(var k=0; k<len; k++) {
                          $w2(id).appendChild(tmp.firstChild);
                        }
                        s[i+1].evalScripts();
                        while (s[i]!="</ap>" && i<=s.length) i=i+1;
                    }
                    if(tok.substring(0, 4) == "<rm ") {
                        //alert(tok);
                        id=tok.replace(/.*[\'\"](.*)[\'\"].*/,"$1");
                        var e=$w2(id);
                        //alert(id+"|"+elem+"|"+tok);
                        if(e!=undefined && e!=null) e.parentNode.removeChild(e);
                        // Is it bad if we can't find an element we are asked to remove?
                        // else alert("can't find element " + id);
                        // *** This doesn't account for tag minimization: <rm id='1'/>
                        while (s[i]!="</rm>" && i<=s.length) i=i+1;
                    }
                    i++;
                }
                
            } else {
                // It must just be simple javascript commands. That's easy...
                eval(unescapeHTML(req.responseText));
            }
        } else if (req.status > 0) {
             alert('ERROR - ' + req.responseText);
             /* alert("There was a problem retrieving the XML data:\n" +
                req.statusText + ' -- ' + req.status + ": " + req.responseText + ' -- ' + URL); */
         }
    }
}
