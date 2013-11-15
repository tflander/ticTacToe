
function allowClickOnEmptyCells() {
	$(".Clear").click(function() {
		if ($("#message").text().length == 0) {
			disableClicks();
			$(this).removeClass().addClass("O").html("O");
			var cells = $(".ticTacToe").text().replace(/ /g, "");
			document.location.href = "/" + cells;
		}
	});
}

function disableClicks() {
	$(".Clear").unbind("click");	
}

$(document).ready(function() {

	// start behavior for playing tic-tac-toe
	$(".X").html("X")
	$(".O").html("O")
	$(".Clear").html("A")

	allowClickOnEmptyCells();

	$(".meFirst").click(function() {
		document.location.href = "/";
	});

	$(".youFirst").click(function() {
		$(".ticTacToe td").removeClass().addClass("Clear").html("A");
		$("#message").text("")
		allowClickOnEmptyCells();
	});

	// start behavior for the aibuilder
	$("#usageWrapper").click(function(){
		$("#usage").slideToggle();
	});
	
	$("#usage").slideUp();
	
	$("li.ruleText").click(function() {
		$("#editRule").text($(this).text());
	});
	
});
