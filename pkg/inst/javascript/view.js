
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

// all possible values for tag 'view'
var vs = [];
var view = $('[view]');
view.each(function(index) {
    vs[index] = parseInt($(this).attr("view"));
    });
function Numsort (a, b) {
  return a - b;
}



// make array of all possible values for view
allView = vs.getUnique().sort(Numsort);

// value of the first shown objects
curView = +allView[0];


//$('[view').attr('visibility', 'hidden');
//$('[view=0]').attr('visibility', 'visible');
//$('[view=' + curView + ']').attr('visibility', 'visible');


// when clicking on graphic make changes
$('svg').click(function() {
	// make all objects[view] invisible
	$('[view]').attr('visibility', 'hidden');
	$('[view=0]').attr('visibility', 'visible');
      // make all objects[view = curView] visible
	$('[view=' + curView + ']').attr('visibility', 'visible');
	// walk through array
	var index = allView.indexOf(curView);
	if ((index + 1) == allView.length) {
	    index = 0;
	} else {
	    index += 1
		}
	curView = allView[index];
})
