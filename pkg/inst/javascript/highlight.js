
$('circle[highlight]').mouseover(function() {
	var value = $(this).attr('highlight');
	// search all elements with this value
	var bros = $('circle[highlight="' + value + '"]');
	bros.attr('old', bros.attr('fill'));
	bros.attr('fill', 'rgb(1,1,1)');
    }
);

$('circle[highlight]').mouseout(function() {
	var value = $(this).attr('highlight');
	// search all elements with this value
	var bros = $('circle[highlight="' + value + '"]');
	bros.attr('fill', bros.attr('old'));
    }
);

// path
$('polyline[highlight]').mouseover(function() {
	var value = $(this).attr('highlight');
	// search all elements with this value
	var bros = $('polyline[highlight="' + value + '"]');
	bros.attr('old', bros.attr('stroke'));
	bros.attr('stroke', 'rgb(1,1,1)');
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
	//alert("hi");
	var value = $(this).attr('highlight');
	//search all elements with this value
	var bros = $('g[highlight="' + value + '"]:has("polygon")');
	bros.children().attr('old', bros.children().attr('fill'));
	bros.children().attr('fill','rgb(1,1,1)');
    }
);


$('g[highlight]:has(polygon)').mouseout(function() {
	var value = $(this).attr('highlight');
	// search all elements with this value
	var bros = $('g[highlight="' + value + '"]:has(polygon)');
	bros.children().attr('fill', bros.children().attr('old'));
    }
);

