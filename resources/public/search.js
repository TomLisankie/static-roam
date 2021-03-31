// Hacky code to support search widget
// Partly stolen from Librium
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
    var docs = JSON.parse(Get("../index.js"));
    docs.forEach(function(doc) {
	index.addDoc(doc, false);
    });
}

function doSearch() {
    if (index.documentStore.length == 0) {
	getDocs();
    }
    var term = document.getElementById("searcht").value;
    var results = index.search(term);
    // console.log(results);	
    displayResults(results);
}

function insertText(container, text) {
    var node = document.createTextNode(text);
    container.appendChild(node);
}

function insertLink(container, url, title) {
    var div = document.createElement('div');    
    div.setAttribute('class','searchentry');
    var link = document.createElement('a');
    link.setAttribute('href', url);
    link.setAttribute('target', '_blank');
    insertText(link, title);
    div.appendChild(link);
    container.appendChild(div);
    return link;
}

function displayResults(results) {
    var out = document.getElementById('searcho');
    out.innerHTML = "";
    results.forEach(function(result) {
	console.log(result.doc.title);
	insertLink(out, result.doc.url, result.doc.title);
    })
}
