window.addEventListener("load", function(){
    
    var objects = document.getElementsByTagName('*');
    for(var i = 0; i < objects.length; i++){
	obj = objects[i];
	
	// set event listener for tooltips
	if(obj.getAttributeNS(null, "tooltip")){
	    obj.addEventListener("mouseover", function(evt){
		showTooltip(evt);
	    });
	    obj.addEventListener("mouseout", function(evt){
		hideTooltip();
	    });
	}
	
    }
    
});


function showTooltip(evt) {
    // Getting rid of any existing tooltips
    hideTooltip();
    var svgNS = "http://www.w3.org/2000/svg";
    var target = evt.currentTarget;

    
    // Create new text node, rect and text for the tooltip
    var content = evt.currentTarget.getAttribute("tooltip");
    if(content == "") {
	return;
    }
    var contentStr = content+'';
    var myText = document.createTextNode(contentStr);
    
    var text = document.createElementNS(svgNS, "text");
    text.setAttribute("id", "tooltipText");
  // Resetting some style attributes
    text.setAttribute("font-size", "16px");
    text.setAttribute("fill", "black");
    text.setAttribute("stroke-width", "0");
    //text.appendChild(contentStr);
    text.appendChild(myText);

  var rect = document.createElementNS(svgNS, "rect");
  rect.setAttribute("id", "tooltipRect");

  // Add rect and text to the bottom of the document.
  // This is because SVG has a rendering order.
  // We want the tooltip to be on top, therefore inserting last.
  var wrappingGroup = document.getElementsByTagName("g")[0];
  wrappingGroup.appendChild(rect);
  wrappingGroup.appendChild(text);

  // Transforming the mouse location to the SVG coordinate system
  // Snippet lifted from: http://tech.groups.yahoo.com/group/svg-developers/message/52701
  var m = target.getScreenCTM();
  var p = document.documentElement.createSVGPoint();
  p.x = evt.clientX;
  p.y = evt.clientY;
  p = p.matrixTransform(m.inverse());

  // Determine position for tooltip based on location of 
  // element that mouse is over
  // AND size of text label
  // Currently the tooltip is offset by (3, 3)
  var tooltipx = p.x + 3;
  var tooltiplabx = tooltipx + 5;
  var tooltipy = p.y + 3;
  var tooltiplaby = tooltipy + 5;

  // Position tooltip rect and text
  text.setAttribute("transform", 
                    "translate(" + tooltiplabx + ", " + tooltiplaby + ") " +
                    "scale(1, -1)");

  rect.setAttribute("x", tooltipx);
  rect.setAttribute("y", tooltipy);
  rect.setAttribute("width", text.getBBox().width + 10);
  rect.setAttribute("height", text.getBBox().height + 5);
  rect.setAttribute("stroke", "black");
  rect.setAttribute("fill", "yellow");
}

function hideTooltip() {
  // Remove tooltip text and rect
  var text = document.getElementById("tooltipText");
  var rect = document.getElementById("tooltipRect");

  if (text !== null && rect !== null) {
    text.parentNode.removeChild(text);
    rect.parentNode.removeChild(rect);
  }
}

//var with_tooltip = $('[tooltip]').filter('[tooltip != ""]');

// with_tooltip.mouseover(function() {
// 	showTooltip($(this));
//     });
// with_tooltip.mouseout( function () {
// 	hideTooltip();
//     });

