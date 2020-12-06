var i = 0;
var params = (new URL(document.location)).searchParams;

var teams = [[],[]];
if (params.get("team1"))
	teams[0] = params.get("team1").split(",").map(function(e, i) {return [e, i+1];});
if (params.get("team2"))
	teams[1] = params.get("team2").split(",").map(function(e, i) {return [e, i+1];});
console.log("team 1: " + teams[0]);
console.log("team 2: " + teams[1]);

var seed = 13645143;
if (params.get("seed"))
	seed = +params.get("seed");
console.log("seed = " + seed);

teams.forEach(function(chars, team) {
	chars.forEach(function(char, index) {
		div = document.createElement("DIV");
		div.setAttribute("id", `team${team+1}member${index}`);
		div.setAttribute("style", `width: ${100/chars.length}%;`);
		
		charIcon = document.getElementById(char[0]).cloneNode();
		charIcon.setAttribute("style", `width:100%`);
		charIcon.setAttribute("id", `team${team+1}icon${index}`);
		div.appendChild(charIcon);
		
		checkIcon = document.getElementById("check2").cloneNode();
		checkIcon.setAttribute("id", `team${team+1}check${index}`);
		div.appendChild(checkIcon);
		
		document.getElementById(`team${team+1}`).appendChild(div);
	});
});

games = [shufflify(teams[0], teams[1].length), shufflify(teams[1], teams[0].length)]
console.log(games);
prevGamesStates = [];
prevPageStates = [];

function seededRandom(min, max) {
    max = max || 1;
    min = min || 0;

    seed = (seed * 9301 + 49297) % 233280;
    var rnd = seed / 233280;

    return min + rnd * (max - min);
}
function shuffle(a) {
    var j, x, i;
    for (i = a.length - 1; i > 0; i--) {
        j = Math.floor(seededRandom(0,i+1));
        x = a[i];
        a[i] = a[j];
        a[j] = x;
    }
    return a;
}
function shufflify(team, size) {
    var result = [];
    while(result.length < team.length*size)
        result = result.concat(shuffle(team.slice(0)));
	return result;
}

function createCharElement(char, teamNumber) {
    i++;
    
    div = document.createElement("DIV");
    div.setAttribute("class","icon");
	div.setAttribute("id", `team${teamNumber}charicon`);
    
    clicker = document.createElement("A");
    clicker.setAttribute("href", "javascript:void(0);");
    clicker.setAttribute("title", `Team ${teamNumber}'s ${char} wins!`);
    clicker.setAttribute("onclick", `winGame("${teamNumber}")`);
    div.appendChild(clicker);
	
    clicker.appendChild(document.getElementById(char).cloneNode());
    clicker.appendChild(document.getElementById("check").cloneNode());
    
    return div;
}

function startMatch() {
    document.getElementById("startbutton").innerHTML = "Undo Game Win";
    document.getElementById("startbutton").setAttribute("onclick", "undo()");
	teams.forEach(function(team, teamNumber) {
		team.forEach(function(char, index) {
			document.getElementById("team"+(teamNumber+1)+"icon"+index).setAttribute("style", `width: 100%; opacity: 50%;`);
		});
	});
	
	nextGame();
}

function nextGame() {
	chars = [games[0][0], games[1][0]];
	console.log(chars.join(" vs "));
	teams.forEach(function(team, teamNumber) {
		team.forEach(function(char, index) {
			if(index == chars[teamNumber][1]-1)
				document.getElementById("team"+(teamNumber+1)+"icon"+index).setAttribute("style", "width: 100%;");
		});
	});
	document.getElementById("team1char").innerHTML = "";
	document.getElementById("team1char").appendChild(createCharElement(chars[0][0], 1));
	document.getElementById("team2char").innerHTML = "";
	document.getElementById("team2char").appendChild(createCharElement(chars[1][0], 2));
}

function winGame(winningTeam) {
    prevGamesStates.push(games.slice().map((x) => x.slice()));
    prevPageStates.push(document.body.innerHTML);
    document.getElementById("startbutton").removeAttribute("disabled");
	archiveGame(winningTeam);
	teams.forEach(function(team, teamNumber) {
		if(teamNumber+1 == winningTeam) {
			document.getElementById("team"+(teamNumber+1)+"icon"+(games[teamNumber][0][1]-1)).setAttribute("style", `width: 100%; opacity: 50%;`);
			document.getElementById(`team${teamNumber+1}check${games[teamNumber][0][1]-1}`).removeAttribute("style");
			games[teamNumber] = games[teamNumber].filter(char => char[1] != games[teamNumber][0][1]);
		}
		else {
			document.getElementById("team"+(teamNumber+1)+"icon"+(games[teamNumber][0][1]-1)).setAttribute("style", `width: 100%; opacity: 50%;`);
			games[teamNumber] = games[teamNumber].splice(1);
		}
	});
	console.log(games);
	
	if(games[0].length == 0)
		winMatch(1);
	else if(games[1].length == 0)
		winMatch(2);
	else
		nextGame();
}

function undo() {
    console.log('undo!');
    console.log(prevGamesStates);
    console.log(prevPageStates);
    games = prevGamesStates.pop();
    document.body.innerHTML = prevPageStates.pop();
	console.log(games);
}

function archiveGame(winningTeam) {
	var sign1;
	var sign2;
	if (winningTeam == 1) {
		sign1 = document.getElementById("check-sign").cloneNode();
		sign2 = document.getElementById("x-sign").cloneNode();
	}
	else {
		sign1 = document.getElementById("x-sign").cloneNode();
		sign2 = document.getElementById("check-sign").cloneNode();
	}
	char1 = document.getElementById(games[0][0][0]).cloneNode();
	char2 = document.getElementById(games[1][0][0]).cloneNode();
	
	div = document.createElement("DIV");
	div.setAttribute("class", "archive");
	div.appendChild(sign1);
	div.appendChild(char1);
	div.appendChild(char2);
	div.appendChild(sign2);
	
	document.getElementById("archive").appendChild(div);
}

function winMatch(winningTeam) {
	console.log(`Team ${winningTeam} wins!`);
	document.getElementById("versus").innerHTML=`<h1 class="title" style="grid-area: all;">Team ${winningTeam} wins!!!</h1>`;
	document.getElementById("versus").setAttribute("id", "versuswin");
}
