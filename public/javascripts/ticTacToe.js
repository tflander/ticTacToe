$(document).ready(function() {
	$(".X").html("X")
	$(".O").html("O")
	$(".Clear").html("A")
	
	$(".Clear").click(function() {
		$(this).removeClass().addClass("O").html("O");
		var cells = $(".ticTacToe").text().replace(/ /g, "");
		document.location.href = "/" + cells;
	});
	
	$(".newGame").click(function() {
		document.location.href = "/";
	});
});	
