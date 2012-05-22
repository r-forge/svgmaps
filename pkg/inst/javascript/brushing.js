
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
    vs[index] = parseInt($(this).attr("show"));
    });
function Numsort (a, b) {
  return a - b;
}



// make array of all possible values for show
allShow = vs.getUnique().sort(Numsort);

// value of the first shown objects
curShow = +allShow[1];


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

