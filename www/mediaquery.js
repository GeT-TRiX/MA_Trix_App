var currFFZoom = 1;
    var currIEZoom = 100;

    $('#plusBtn').on('click',function(){
        if ($.browser.mozilla){
            var step = 0.02;
            currFFZoom += step;
            $('body').css('MozTransform','scale(' + currFFZoom + ')');
        } else {
            var step = 2;
            currIEZoom += step;
            $('body').css('zoom', ' ' + currIEZoom + '%');
        }
    });

    $('#minusBtn').on('click',function(){
        if ($.browser.mozilla){
            var step = 0.02;
            currFFZoom -= step;
            $('body').css('MozTransform','scale(' + currFFZoom + ')');

        } else {
            var step = 2;
            currIEZoom -= step;
            $('body').css('zoom', ' ' + currIEZoom + '%');
        }
    });
