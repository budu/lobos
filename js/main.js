$(document).ready(
    function() {
        function rsz_if(elem, sz) {
            if (elem.height() < sz) elem.css('height', sz + 'px'); };
        function rsz() {
            var h = $(window).height();
            rsz_if($('#content'), h * 0.7); };
        SyntaxHighlighter.all();
        window.onresize = function() { rsz(); };
        rsz();
    });
