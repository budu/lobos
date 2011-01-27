$(document).ready(
    function() {
        function scrollbar() {
            var viewportHeight = window.innerHeight
                ? window.innerHeight
                : $(window).height();
            if (jQuery.browser.msie &&
                parseInt(jQuery.browser.version) == 7)
                    viewportHeight -= 3;
            return viewportHeight <= $('#wrapper').height(); };
        function rsz_font(elem) {
            var ws = ($(window).width() + (scrollbar() ? 20 : 0)).toString();
            elem.css('font-size', ws.substring(0, ws.length - 2) + 'pt'); };
        function rsz_if(elem, sz) {
            if (elem.height() < sz) elem.css('height', sz + 'px'); };
        function rsz() {
            rsz_font($('#header > ul'));
            var h = $(window).height();
            rsz_if($('#wrapper'), h - 10);
            rsz_if($('#content'), h * 0.5); };
        SyntaxHighlighter.all();
        window.onresize = function() { rsz_font($('#header > ul')); };
        rsz();
    });
