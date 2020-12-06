var formats = [
!!!formats||JsArrayHead
];
var characters = [
!!!characters||JsArrayHead
];
var rulesets = [
!!!rulesets-main||JsArrayHead
,
!!!rulesets-mirror||JsArrayHead
,
!!!rulesets-format||JsArrayHead
];
var playfair = [
!!!rulesets-playfair||JsArrayHead
];

function generate() {
    var operator = document.getElementById("operator").value;
    var result = document.getElementById("result");
    var textArray;
    switch(operator) {
        case "Tournament Style":
        textArray = formats;
        break;
        case "Character":
        textArray = characters;
        break;
        case "Ruleset":
        textArray = rulesets;
        break;
        case "Play Fair":
        textArray = playfair;
        break;
        default:
        textArray = ["uhhhh"];
    }
    var randomNumber = Math.floor(Math.random()*textArray.length);
    result.value = textArray[randomNumber];
}
