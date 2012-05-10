
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


// +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
// for brushing


// function to find unique values
Array.prototype.getUnique = function(){
    var u = {}, a = [];
    for(var i = 0, l = this.length; i < l; ++i){
	if(this[i] in u)
	    continue;
	a.push(this[i]);
	u[this[i]] = 1;
    }
    return a;
};

// all possible values for tag 'show'
var vs = [];
var show = $('[show]');
show.each(function(index) {
	vs[index] = $(this).attr("show");
    });


// value of the first shown objects
curShow = "0";

// make array of all possible values for show
allShow = vs.getUnique();
var allShow2 = [];
for (var i = 0; i < allShow.length; i++){
    allShow2[i] = +allShow[i];
}
allShow = allShow2.sort();


$('[show]').attr('visibility', 'hidden');
$('[show=' + curShow + ']').attr('visibility', 'visible');


// when clicking on graphic make changes
$('svg').click(function() {
	// walk through array
	var index = allShow.indexOf(curShow);
	if ((index + 1) == allShow.length) {
	    index = 0;
	} else {
	    index += 1
		}
	curShow = allShow[index];
	// make all objects[show] invisible
	$('[show]').attr('visibility', 'hidden');
	// make all objects[show = curShow] visible
	$('[show=' + curShow + ']').attr('visibility', 'visible');
})

