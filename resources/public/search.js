var index = elasticlunr(function () {
    this.addField('title');
    this.addField('body');
    this.setRef('id');
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

function displayResults(results) {
    results.forEach(function(result) {
	console.log(result.doc.title);
    })
}
