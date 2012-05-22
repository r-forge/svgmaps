
$('circle[highlight]').mouseover(function() {
    var value = $(this).attr('highlight');
    // search all elements with this value
    var bros = $('circle[highlight="' + value + '"]');
    bros.attr('old', bros.attr('fill'));
    bros.attr('fill', hcolour);
    bros.attr('size_old', bros.attr('r'));
    bros.attr('r', +hsize*bros.attr('r'));
}
);

$('circle[highlight]').mouseout(function() {
	var value = $(this).attr('highlight');
	// search all elements with this value
	var bros = $('circle[highlight="' + value + '"]');
	bros.attr('fill', bros.attr('old'));
    	bros.attr('r', bros.attr('size_old'));
    }
);

// path
$('polyline[highlight]').mouseover(function() {
    var value = $(this).attr('highlight');
    // search all elements with this value
    var bros = $('polyline[highlight="' + value + '"]');
    bros.attr('old', bros.attr('stroke'));
    bros.attr('stroke', hcolour);
    bros.attr('stroke-width_old', bros.attr('stroke-width'));
    bros.attr('stroke-width', +hsize*bros.attr('stroke-width'));
    //add(+hsize*bros.attr('stroke-width'));
    }
);

$('polyline[highlight]').mouseout(function() {
	var value = $(this).attr('highlight');
	// search all elements with this value
	var bros = $('polyline[highlight="' + value + '"]');
	bros.attr('stroke', bros.attr('old'));
    }
);



// polygon
$('g[highlight]:has(polygon)').mouseover(function() {
	var value = $(this).attr('highlight');
	//search all elements with this value
	var bros = $('g[highlight="' + value + '"]:has("polygon")');
	bros.children().attr('old', bros.children().attr('fill'));
	bros.children().attr('fill', hcolour);
    }
);


$('g[highlight]:has(polygon)').mouseout(function() {
	var value = $(this).attr('highlight');
	// search all elements with this value
	var bros = $('g[highlight="' + value + '"]:has(polygon)');
	bros.children().attr('fill', bros.children().attr('old'));
    }
);



//rect 
$('rect[highlight]').mouseover(function() {
	var value = $(this).attr('highlight');
	//search all elements with this value
	var bros = $('rect[highlight="' + value + '"]');
	bros.attr('old', bros.attr('fill'));
	bros.attr('fill', hcolour);
    }
);


$('rect[highlight]').mouseout(function() {
	var value = $(this).attr('highlight');
	// search all elements with this value
	var bros = $('rect[highlight="' + value + '"]');
	bros.attr('fill', bros.attr('old'));
    }
);

