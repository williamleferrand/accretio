function isSupported() {

    var ua = window.navigator.userAgent;
    var msie = ua.indexOf("MSIE ");

    // if ie, we need at least ie 9
    if (msie > 0 || !!navigator.userAgent.match(/Trident.*rv\:11\./)) {
        var version = parseInt(ua.substring(msie + 5, ua.indexOf(".", msie)));
        return (version >= 9)
    }
    else
        return true;

}


if (!isSupported()) {
    window.location.pathname = "sorry.html";
}


if (!window.console) window.console = {};
if (!window.console.log) window.console.log = function () { };
