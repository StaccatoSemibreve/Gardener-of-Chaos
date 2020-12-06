var teams = [[],[]];
var i = 0;

function updateTeams() {
	if(teams[0].length > 0 && teams[1].length > 0)
		document.getElementById("submitlink").href = `teambattle?seed=${(Math.floor(Math.random()*100000))}&team1=${teams[0].filter(n=>n!==undefined).join()}&team2=${teams[1].filter(n=>n!==undefined).join()}`;
	else
		document.getElementById("submitlink").href = "javascript:void(0);";
}
function createCharElement(charName, teamNumber) {
    i++;
    
    div = document.createElement("DIV");
    div.setAttribute("id",teamNumber+""+teams[teamNumber-1].length);
    div.setAttribute("class","icon");
    
    clicker = document.createElement("A");
    clicker.setAttribute("href", "javascript:void(0);");
    clicker.setAttribute("title", `Remove this ${charName} from Team ${teamNumber}.`);
    clicker.setAttribute("onclick", `removeChar("${teamNumber+""+teams[teamNumber-1].length}",${teamNumber})`);
    clicker.setAttribute("id", `${i}char`);
    div.appendChild(clicker);
    clicker.appendChild(document.getElementById(charName).cloneNode());
    clicker.appendChild(document.getElementById("minus").cloneNode());
    
    return div;
}
function addChar(char, teamNumber) {
    console.log(`Add ${char} to team ${teamNumber}.`);
    teams[teamNumber-1] = teams[teamNumber-1].concat(char);
    
    document.getElementById(`team${teamNumber}div`)
            .appendChild(createCharElement(char, teamNumber));
    
    updateTeams();
}
function removeChar(char, teamNumber) {
    console.log(`Remove ${char} from team ${teamNumber}.`);
    delete teams[teamNumber-1][+char.substring(1)-1]
    document.getElementById(char).remove();
    
    updateTeams();
}
function clearTeam(teamNumber) {
    console.log(`Clear team ${teamNumber}.`);
    teams[teamNumber-1]=[];
    document.getElementById(`team${teamNumber}div`).innerHTML="";
	updateTeams();
}
