var topLevel = require('./toplevel.js');
var greet = "Terminal\n";

function interpret(line){
    var trimLine = line.trim();
    return topLevel.runExpr(trimLine);

}
    

$('body').terminal(interpret, {
    greetings: greet,
    name: "terminal",
    prompt: 'λ ',
});




