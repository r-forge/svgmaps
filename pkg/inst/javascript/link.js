
var with_link = $('[link]').filter('[link != "NA"]')

with_link.mouseover(function () {
	$(this).attr('style', 'cursor:pointer');
    });

with_link.click(function () {
	window.open($(this).attr("link"), '_blank', '');
    });

