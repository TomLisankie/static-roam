// Hacky code to support search widget
// TODO should have a visible "no results" indicater
// TODO might want to trim results to n or use threshold

var index = elasticlunr(function () {
    this.addField('title');	// TODO adjust config
    this.addField('body');
    this.setRef('id');		// TODO is this used?
});

function Get(yourUrl){
    var Httpreq = new XMLHttpRequest(); // a new request
    Httpreq.open("GET",yourUrl,false);
    Httpreq.send(null);
    return Httpreq.responseText;          
}

function getDocs() {
    var docs = JSON.parse(Get("/index.js"));
    docs.forEach(function(doc) {
	index.addDoc(doc, false);
    });
}

function keypress(evt) {
// for search on enter, but doing it on all keys is better
//    if (evt.keyCode == 13) {
    doSearch();
}

var config = {
    fields: {
        title: {boost: 2},
        body: {boost: 1}
    },
    expand: true
}

function doSearch() {
    if (index.documentStore.length == 0) {
	getDocs();
    }
    var term = document.getElementById("searcht").value;
    var results = index.search(term, config);
    displayResults(results);
}

function insertText(container, text) {
    var node = document.createTextNode(text);
    container.appendChild(node);
    return node;
}

function insertLink(container, url, title) {
    var div = document.createElement('div');    
    div.setAttribute('class','searchentry');
    var link = document.createElement('a');
    link.setAttribute('href', '/' + url); // kludge alert
    // link.setAttribute('target', '_blank'); 
    insertText(link, title);
    div.appendChild(link);
    container.appendChild(div);
    return link;
}

// TODO should limit to first n
function displayResults(results) {
    var out = document.getElementById('searcho');
    out.style.display = 'block'; 
    out.innerHTML = "";
    if (results.length == 0) {
	insertText(out, "No results") // TODO style
	    .setAttribute('class', 'searchnada')
    } else {
	results.forEach(function(result) {
	    insertLink(out, result.doc.url, result.doc.title);
	})
    }
}
    
// Not search, just here for convenience: persist collaps state of map

function maybeOpenMap() {
    var open = sessionStorage.getItem('map') == 'true';
    // var open = true;
    if (open) {
	document.getElementById('mapgraph').classList.add("show");
    }
}

document.addEventListener("DOMContentLoaded", maybeOpenMap);

function toggleMap() {
    var open = sessionStorage.getItem('map') == 'true';
    console.log('map is', open);
    sessionStorage.setItem('map', !open);
}
