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

function getIndex() {
    var docs = JSON.parse(Get("../index.js"));
    docs.forEach(function(doc) {
	index.addDoc(doc, false);
    });
}
