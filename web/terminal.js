var greet = "Terminal";

function interpret(line){
    var trimLine = line.trim();
    return trimLine;
}
    

$('#terminal').terminal(interpret, {
    greetings: greet,
    name: "terminal",
    height: 550,
    prompt: 'λ ',
});




