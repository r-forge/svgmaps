window.addEventListener("load", function(){
	
	var objects = document.getElementsByTagName('*');
    for(var i = 0; i < objects.length; i++){
	obj = objects[i]
	
	// Link implementation
	// maybe better 	if (obj.getAttributeNS(null, "link") & obj.getAttributeNS(null, "link") != ""){
	if (obj.getAttributeNS(null, "link")){
	    // Link-like cursor
	    obj.setAttributeNS(null, "style", "cursor:pointer");
	    // href on click
	    obj.addEventListener("click", function(evt){
		window.open(evt.currentTarget.getAttributeNS(null, "link"), '_blank', '');
	    });
	    // Does not work yet
	    obj.addEventListener("onmouseover", function (evt) {
		top.status = evt.getAttributeNS(null, "link");
	    });
	    obj.addEventListener("onmouseout", function (evt) {
		top.status = "";
	    });
	}
	
	// set event listener for tooltips
	if(obj.getAttributeNS(null, "tooltip")){
	    obj.addEventListener("mouseover", function(evt){
		showTooltip(evt);
	    });
	    obj.addEventListener("mouseout", function(evt){
		hideTooltip();
	    });
	}
	
	// set event listener for highlight
	// if(obj.getAttributeNS(null, "highlight")){
	//     obj.addEventListener("mouseover", function(evt){
	// 	highlight(evt);
	//     });
	    
	//     obj.addEventListener("mouseout", function(evt){
	// 	downlight();
	//     });
	//}
    }
});
