$(document).ready(
    function() {
        function rsz_if(test, elem, sz) {
            if (test) elem.css('height', sz + 'px'); };
        function rsz() {
            var ws = $(window).width().toString();
            $('body').css('font-size', ws.substring(0, ws.length - 2) + 'pt');
            var h = $(window).height();
            rsz_if($('#wrapper').height() < h - 10, $('#wrapper'), h - 10);
            rsz_if($('#content').height() < h * 0.5, $('#content'), h * 0.5); };
        SyntaxHighlighter.all();
        rsz();
    });
