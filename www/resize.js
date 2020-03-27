// https://stackoverflow.com/questions/45191999/ggiraph-plot-doesnt-resize-to-fit-the-page
// https://stackoverflow.com/questions/5489946/how-to-wait-for-the-end-of-resize-event-and-only-then-perform-an-action

$(document).on("shiny:connected", function(e) {
	var w = window.innerWidth;
	var h = window.innerHeight;
	var d = document.getElementById("ppitest").offsetWidth;
	Shiny.onInputChange("window", {width: w, height: h, dpi: d});
});

var rtime;
var timeout = false;
var delta = 500;

function resizeend() {
    if (new Date() - rtime < delta) {
        setTimeout(resizeend, delta);
    } else {
        timeout = false;
        var w = $(this).width();
        var h = $(this).height();
        var d =  document.getElementById("ppitest").offsetWidth;
        Shiny.onInputChange("window", {width: w, height: h, dpi: d});
    }               
}

$(window).resize(function(e) {
	rtime = new Date();
    if (timeout === false) {
        timeout = true;
        setTimeout(resizeend, delta);
    }
});