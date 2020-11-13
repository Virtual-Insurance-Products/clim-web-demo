
function inIE() {
    var d = navigator.userAgent.toLowerCase();
    return d.indexOf('msie')+1;
}

// this basically works around a bug in IE
function setShadowHeights() {
    // alert("fix up...");
    if (!inIE()) return;
    var e = document.getElementsByClassName('l');
    for (i=0;i<e.length;i++) {
        // alert(e[i].parentElement.offsetHeight);
        e[i].style.height=e[i].parentElement.offsetHeight-40;
    }
    var e = document.getElementsByClassName('r');
    for (i=0;i<e.length;i++) {
        e[i].style.height=e[i].parentElement.offsetHeight-40;
    }
}

// Event.observe(window, 'load', setShadowHeights, false);

// alert('yo!');
