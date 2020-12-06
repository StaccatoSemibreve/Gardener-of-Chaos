var charSelect = 
!!!characters||JsOptionsHead
;
var presets = [["Grave", "Jaina", "Geiger", "Argagarg", "Setsuki", "Valerie", "Rook", "Midori", "Lum", "Degrey", "Quince", "Onimaru"], ["Grave", "Jaina", "Geiger", "Argagarg"], ["Setsuki", "Valerie"], ["Rook", "Midori"], ["Lum", "Degrey", "Quince", "Onimaru"], ["Grave", "Geiger", "Setsuki", "Rook", "Lum", "Quince"], ["Jaina", "Argagarg", "Valerie", "Midori", "Degrey", "Onimaru"]];

var seed = 0;
var submit = false;

var team1Size = 0;
var team1 = [];
var team1Sequence = [];
var team2Size = 0;
var team2 = [];
var team2Sequence = [];
var match = 0;

function randomiseSeed() {
document.getElementById('seed').value = Math.floor(Math.random()*Number.MAX_SAFE_INTEGER);
}
function initSeed() {
    seed = document.getElementById('seed').value;
    var seed0 = (seed==0);
    if(seed0)
        seed = Math.floor(Math.random()*Number.MAX_SAFE_INTEGER);
    if(seed0)
        document.getElementById('seeding').outerHTML="<p>Seed: Random (" + seed + ")</p";
    else
        document.getElementById('seeding').outerHTML="<p>Seed: " + seed + "</p>";
    console.log("Seed: "+seed);
    document.getElementById('teams').style="";
}
function seededRandom(min, max) {
    max = max || 1;
    min = min || 0;

    seed = (seed * 9301 + 49297) % 233280;
    var rnd = seed / 233280;

    return min + Math.floor(rnd * (max - min));
}

function zipped(arr) {
    arr = arr.map(function(e, i) {return [e, i+1];});
}

function setTeam1Size() {
    team1Size = document.getElementById('team1Size').value;
    console.log("Team 1 Size: "+team1Size);
    if(team1Size > 0) {
        document.getElementById('team1').innerHTML = '';

        for(var count = 1; count <= team1Size; count++)
            document.getElementById('team1').innerHTML += '<select id="team1Select'+count+'">'+charSelect;
        document.getElementById('team1').innerHTML += '<input type="button" value="Submit Team 1 Members" onclick="setTeam1Members()"/>';
    }
}
function setTeam2Size() {
    team2Size = document.getElementById('team2Size').value;
    console.log("Team 2 Size: "+team2Size);
    if(team2Size > 0) {
        document.getElementById('team2').innerHTML = '';

        for(var count = 1; count <= team2Size; count++)
            document.getElementById('team2').innerHTML += '<select id="team2Select'+count+'">'+charSelect;
        document.getElementById('team2').innerHTML += '<input type="button" value="Submit Team 2 Members" onclick="setTeam2Members()"/>';
    }
}

function setTeam1Members() {
    for(var count = 1; count <= team1Size; count++)
        team1 = team1.concat(document.getElementById('team1Select'+count).value);
    document.getElementById('team1').outerHTML = "<p>"+team1.join(", ")+"</p>";
    team1 = team1.map(function(e, i) {return [e, i+1];});
    console.log("Team 1: "+team1.map(function(e) {return e[0]}).join(", "));
    if(submit) {play();}
    submit = true;
}
function setTeam2Members() {
    for(var count = 1; count <= team2Size; count++)
        team2 = team2.concat(document.getElementById('team2Select'+count).value);
    document.getElementById('team2').outerHTML = "<p>"+team2.join(", ")+"</p>";
    team2 = team2.map(function(e, i) {return [e, i+1];});
    console.log("Team 2: "+team2.map(function(e) {return e[0]}).join(", "));
    if(submit) {play();}
    submit = true;
}

function setTeam1Preset() {
    var preset = document.getElementById('team1Presets').value;
    console.log("Team 1 Preset: " + preset);
    if(preset == "random") {
        team1 = presets[0].slice(0);
        var size = document.getElementById('team1Size').value;
        if(size == 0)
        size = seededRandom(1, team1.length+1);
        while(team1.length < size)
            team1 = team1.concat(team1.slice(0));
        while(team1.length > size)
            team1.splice(seededRandom(0, team1.length), 1);
        team1 = team1.map(function(e, i) {return [e, i+1];});
    }
    else {
        team1 = presets[preset].map(function(e, i) {return [e, i+1];});
    }

    team1Size = team1.length;
    console.log("Team 1: "+team1.map(function(e) {return e[0]}).join(", "));
    document.getElementById('team1').outerHTML = "<p>"+team1.map(function(e) {return e[0]}).join(", ")+"</p>";
    if(submit) {play();}
    submit = true;
    }
function setTeam2Preset() {
    var preset = document.getElementById('team2Presets').value;
    console.log("Team 2 Preset: " + preset);
    if(preset == "random") {
        team2 = presets[0].slice(0);
        var size = document.getElementById('team2Size').value;
        if(size == 0)
        size = seededRandom(1, team2.length+1);
        while(team2.length < size)
            team2 = team2.concat(team2.slice(0));
        while(team2.length > size)
            team2.splice(seededRandom(0, team2.length), 1);
        team2 = team2.map(function(e, i) {return [e, i+1];});
    }
    else {
        team2 = presets[preset].map(function(e, i) {return [e, i+1];});
    }

    team2Size = team2.length;
    console.log("Team 2: "+team2.map(function(e) {return e[0]}).join(", "));
    document.getElementById('team2').outerHTML = "<p>"+team2.map(function(e) {return e[0]}).join(", ")+"</p>";
    if(submit) {play();}
    submit = true;
    }

function shuffle(a) {
    var j, x, i;
    for (i = a.length - 1; i > 0; i--) {
        j = seededRandom(0, i);
        x = a[i];
        a[i] = a[j];
        a[j] = x;
    }
    return a;
}
function play() {
    console.log("Match " + match + ": " + team1Sequence.map(function(e) {return e[0]}).join(", "));
    console.log("Match " + match + ": " + team2Sequence.map(function(e) {return e[0]}).join(", "));
    if(match == 0) {
        while(team1Sequence.length < team1Size*team2Size)
            team1Sequence = team1Sequence.concat(shuffle(team1.slice(0)));
        while(team2Sequence.length < team1Size*team2Size)
            team2Sequence = team2Sequence.concat(shuffle(team2.slice(0)));
        console.log("Initial: " + team1Sequence.map(function(e) {return e[0]}).join(", "));
        console.log("Initial: " + team2Sequence.map(function(e) {return e[0]}).join(", "));
    }

    if(team1Sequence.length == 0)
        document.getElementById('teams').innerHTML += '<hr><br><p>Team 1 wins the match!!!</p>';
    else if(team2Sequence.length == 0)
        document.getElementById('teams').innerHTML += '<hr><br><p>Team 2 wins the match!!!</p>';
    else {
        match++;

        document.getElementById('teams').innerHTML += '<hr><br><p>Match ' + match + ': Team 1 Member ' + (team1Sequence[0][1]) + ' (' + team1Sequence[0][0] + ') vs Team 2 Member ' + (team2Sequence[0][1]) + ' (' + team2Sequence[0][0] + ')<br>Winner: <select id="match'+match+'Winner"><option value="1">Team 1\'s ' + team1Sequence[0][0] + '</option><option value="2">Team 2\'s ' + team2Sequence[0][0] + '</option></select><input type="button" id="MatchButton" value="Submit Winner" onclick="win()""></p>';
    }
}

function win() {
    var winner = document.getElementById("match"+match+"Winner").value;
    console.log("Team "+winner+" wins");
    document.getElementById("MatchButton").outerHTML = "";
    document.getElementById("match"+match+"Winner").outerHTML = "Team "+winner+"<br>";

    if(winner == "1") {
        var char = team1Sequence[0];
        console.log(char+" wins")
        team1Sequence = team1Sequence.filter(member => member != char);
        team2Sequence = team2Sequence.splice(1);
    }
    else {
        var char = team2Sequence[0];
        console.log(char+" wins")
        team1Sequence = team1Sequence.splice(1);
        team2Sequence = team2Sequence.filter(member => member != char);
    }
    play();
}
