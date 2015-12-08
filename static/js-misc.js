var hoverFix = function()
{
    var el = this;
    var par = el.parentNode;
    var next = el.nextSibling;
    par.removeChild(el);
    par.insertBefore(el, next);
}
