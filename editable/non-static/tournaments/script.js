function addArchive(element, id, videoURL, bracketURL) {
	archive = document.createElement("DIV");
	archive.setAttribute("class", "archive");
	archive.setAttribute("id", id);
	
	video = document.createElement("IFRAME");
	video.setAttribute("class", "video");
	video.setAttribute("src", videoURL);
	video.setAttribute("allow", "encrypted-media; picture-in-picture; fullscreen");
	
	bracket = document.createElement("IFRAME");
	bracket.setAttribute("class", "bracket");
	bracket.setAttribute("src", bracketURL);
	bracket.setAttribute("scrolling", "auto");
	bracket.setAttribute("allowtransparency", "true");
	
	archive.appendChild(video);
	archive.appendChild(bracket);
	element.parentNode.appendChild(archive);
	
	element.setAttribute("onclick", 'removeArchive(this, "'+id+'", "'+videoURL+'", "'+bracketURL+'")');
	element.innerHTML="Stop Watching Archive (will unload the external content!)";
}
function removeArchive(element, id, videoURL, bracketURL) {
	document.getElementById(id).outerHTML="";
	element.setAttribute("onclick", 'addArchive(this, "'+id+'", "'+videoURL+'", "'+bracketURL+'")');
	element.innerHTML="Watch Archive (will load external content!)";
}

function addMatch(element, id, videoURL) {
	archive = document.createElement("DIV");
	archive.setAttribute("class", "archive");
	archive.setAttribute("id", id);
	
	video = document.createElement("IFRAME");
	video.setAttribute("class", "big");
	video.setAttribute("src", videoURL);
	video.setAttribute("allow", "encrypted-media; picture-in-picture; fullscreen");
	
	archive.appendChild(video);
	element.parentNode.appendChild(archive);
	
	element.setAttribute("onclick", 'removeMatch(this, "'+id+'", "'+videoURL+'")');
	element.innerHTML="Stop Watching Match (will unload the external content!)";
}
function removeMatch(element, id, videoURL) {
	document.getElementById(id).outerHTML="";
	element.setAttribute("onclick", 'addMatch(this, "'+id+'", "'+videoURL+'")');
	element.innerHTML="Watch Match (will load external content!)";
}

function addBracket(element, id, bracketURL) {
	archive = document.createElement("DIV");
	archive.setAttribute("class", "archive");
	archive.setAttribute("id", id);
	
	bracket = document.createElement("IFRAME");
	bracket.setAttribute("class", "big");
	bracket.setAttribute("src", bracketURL);
	bracket.setAttribute("scrolling", "auto");
	bracket.setAttribute("allowtransparency", "true");
	
	archive.appendChild(bracket);
	element.parentNode.appendChild(archive);
	
	element.setAttribute("onclick", 'removeBracket(this, "'+id+'", "'+bracketURL+'")');
	element.innerHTML="Stop Viewing Bracket (will unload the external content!)";
}
function removeBracket(element, id, bracketURL) {
	document.getElementById(id).outerHTML="";
	element.setAttribute("onclick", 'addBracket(this, "'+id+'", "'+bracketURL+'")');
	element.innerHTML="View Bracket (will load external content!)";
}
