function shuffle(a) {
    var j, x, i;
    for (i = a.length - 1; i > 0; i--) {
        j = Math.floor(Math.random() * (i + 1));
        x = a[i];
        a[i] = a[j];
        a[j] = x;
    }
    return a;
}

function submit() {
    var team = document.getElementById("team").value.split("\n");
    var size = document.getElementById("size").value;
    var result = [];
    while(result.length < team.length*size)
        result = result.concat(shuffle(team.slice(0)));
    document.getElementById("result").innerHTML = "Result: " + result.join(", ");
}
