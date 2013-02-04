


InfoMiner = {};

var info_grabber;


// http://www.ethicalhacker.net/component/option,com_rss/feed,RSS0.91/no_html,1/


/* Wettervorhersage - Berlin
use 'http://github.com/yql/yql-tables/raw/master/weather/weather.bylocation.xml' as we;
select * from we where location="berlin,germany" and unit='c'
*/
var info_scraper;


InfoMiner.MiningCommand = function(type, query)
{
}

InfoMiner.Scraper = function(admin)
{
var self = this;
this.actual_query_list = null;



// WIKIMEDIA API - funktioniert noch nicht

this.request = function()
{
// http://en.wikipedia.org/w/api.php?format=json&action=query&titles=Main%20Page&prop=revisions&rvprop=content

var url = 'http://en.wikipedia.org/w/api.php?action=query&format=json&titles=Main%20Page&prop=revisions&rvprop=content';

$.getJSON(
url,
function(json){
alert("angekommen");
});


}




this.newspaper = function(query, organ)
{

switch(organ)
{
case "Welt":
var sel = 'select href, content from html where url="http://welt.de" and xpath="//h4/a"';

break;

case "Freitag":
var sel = 'select href, content from html where url="http://freitag.de" and xpath="//a"';
break;

case "FAZ":
var sel = 'select href, content from html where url="http://faz.net" and xpath="//span"';
break;


}


var s = 'http://query.yahooapis.com/v1/public/yql?q=' + sel + '&&format=json&callback=?';
self.query(s);
}



this.remove_empty_spaces = function(text)
{
var n = text.replace(/ /g,"_");
return n;
}


this.wikipedia_query = function(query)
{
var s = 'de.wikipedia.org/wiki/';
var a = self.remove_empty_spaces(query);

s += a;

var sel = 'select content from html where url="' + s + '" and xpath="//p"';

var s = 'http://query.yahooapis.com/v1/public/yql?q=' + sel + '&&format=json&callback=?';
self.wikipedia_search(s);
}


this.wikipedia_search = function(query)
{
var s = query;

$.getJSON(
s,
function(json){

linklist = new Array();
temp = json.query.results;

var item;
for(var propertyName in temp) {
item = propertyName;
}

var item_list = eval("json.query.results." + item);


for (i = 0; i < json.query.count; i++)
{
temp = item_list[i];
linklist.push(temp);
}

info_scraper.actual_query_list = linklist;

});
}


this.query = function(query)
{
var s = query;

$.getJSON(
s,
function(json){
linklist = new Array();
temp = json.query.results;

var item;
for(var propertyName in temp) {
item = propertyName;
}


var item_list = eval("json.query.results." + item);


for (i = 0; i < json.query.count; i++)
{
temp = item_list[i];
linklist.push(temp);
}



info_scraper.actual_query_list = linklist;


});
}


this.init = function()
{
info_scraper = self;
self.newspaper("Aktie", "FAZ");
// self.wikipedia_query("Willy Brandt");
}




self.init();
}




InfoMiner.Grabber = function(admin)
{
var self = this;
this.admin = admin;

this.special_request = new Array();


this.rss_feeds = new Array();
this.science_feeds = new Array();
this.intelligence_feeds = new Array();
this.warfare_feeds = new Array();
this.technology_feeds = new Array();
this.art_feed = new Array();


this.feed_list = new Array();

this.photo_list = new Array();


this.mining_commands = new Array();

this.you_tube_videos = new Array();

this.start_time = 0;
this.wait = 30000;




this.check_time = function(query)
{
now = new Date().getTime();

if (now > (self.start_time + self.wait) )
{
self.start_time = now;
return true;
}
else return false;
}

this.start_mining = function(query)
{
// SHOW
var el = 'info_miner.start_mining()';
self.admin.system_tracker.add_element(el);
//


// self.you_tube(query);
self.random_rss();
}



this.wikimedia = function(query) // DEFEKT
{
var url = 'http://en.wikipedia.org/w/api.php?action=query&titles=Albert%20Einstein  & prop=info & format=jsonfm';

$.ajax({
type : "GET",
url : url

}).done(function(msg) {

alert("ist angekommen");

});

}



this.onYouTube = function(event)
{
var data = JSON.parse( event.target.responseText );
var entries = data.feed.entry;



for (var i = 0; i < entries.length; i++)
{

self.you_tube_videos.push(entries[i] );
}
}


this.you_tube = function(query)
{

var request = new XMLHttpRequest();
request.addEventListener( 'load', info_grabber.onYouTube, false );
request.open( 'GET', 'https://gdata.youtube.com/feeds/api/videos?v=2&alt=json&max-results=50&q=' + query, true );
request.send( null );

}


this.scrape_info = function(query)
{
// select * from html where url='http://www.nytimes.com/' and xpath='//*[@id="home"]' // das funktioniert
}




this.init_science_feeds = function()
{
// alert("Initialisierung der Science Feeds");

self.science_feeds.push("http://rss.sciam.com/ScientificAmerican-News"); // Scientific American
self.science_feeds.push("http://feeds.sciencedaily.com/sciencedaily/top_news/top_technology"); // Science Daily

self.science_feeds.push("http://www.eurekalert.org/rss/social_behavioral.xml"); // Eureka Alert
self.science_feeds.push("http://www.eurekalert.org/rss/nanotechnology.xml"); // Eureka - Nanotechnologoie
self.science_feeds.push("http://www.eurekalert.org/rss/chemistry_physics.xml"); // Eureka . Chemie


self.science_feeds.push("http://www.sciencemag.org/rss/twis.xml"); // Science - Zusammenfassung
self.science_feeds.push("http://www.sciencemag.org/rss/ec.xml"); // Science - Editor's Choicde'
self.science_feeds.push("http://www.sciencemag.org/rss/podcast.xml"); // Science - Podcasts
self.science_feeds.push("http://feeds.newscientist.com/science-news"); // New Scientist
self.science_feeds.push("http://feeds.latimes.com/latimes/news/science"); // LA Times Science

self.science_feeds.push("http://rss.nytimes.com/services/xml/rss/nyt/Science.xml"); // New York Times Science
self.science_feeds.push("http://www.labspaces.net/labspaces.xml?news=Biological"); // LabSpaces - Biologie
self.science_feeds.push("http://www.labspaces.net/labspaces.xml?news=Physical"); // LabSpaces - Physik

self.science_feeds.push("http://www.labspaces.net/labspacesblogs.xml"); // LabSpaces Blogs
self.science_feeds.push("http://www.labspaces.net/labspaces.xml?news=Misc"); // Lab Spaces - Diverses
self.science_feeds.push("http://www.trb.org/Rss.aspx?sn=PassengerTransportationRSSFeed"); // Transport
self.science_feeds.push("http://www.trb.org/Rss.aspx?sn=ACRPInformationTechnology"); // Informationssysteme
self.science_feeds.push("http://feeds.bbci.co.uk/news/science_and_environment/rss.xml?edition=uk"); // BBC Science
self.science_feeds.push("http://www.npr.org/rss/rss.php?id=1007"); // NPR
self.science_feeds.push("http://feeds.feedburner.com/NewsFromTheScientist"); // The Scientist
self.science_feeds.push("http://hosted2.ap.org/atom/APDEFAULT/b2f0ca3a594644ee9e50a8ec4ce2d6de"); // AP Science
self.science_feeds.push("http://rss.csmonitor.com/feeds/science"); // Christian Science Monitor
self.science_feeds.push("http://pheedo.msnbc.msn.com/id/3033117/device/rss/"); // MSNBC

self.science_feeds.push("http://feeds.pbs.org/pbs/wgbh/nova/nsn-audio"); // NovaScience - Podcasts
self.science_feeds.push("http://feeds.pbs.org/pbs/wgbh/nova-video"); // Nova Science Videocast
self.science_feeds.push("http://www.pbs.org/wgbh/nova/sciencenow/rss/nsn.xml"); // NovaScience

self.science_feeds.push("http://feeds.reuters.com/reuters/scienceNews"); // RFeuters

self.science_feeds.push("http://www.popularmechanics.com/rss/"); // Populart Mechanics
self.science_feeds.push("http://rss.slashdot.org/Slashdot/slashdot"); //Slashdot
self.science_feeds.push("http://feeds.newscientist.com/physics-math"); // New Scientist
self.science_feeds.push("http://feeds.feedburner.com/PhysicsWorld?format=xml"); // Physikworld

self.science_feeds.push("http://www.amnh.org/science/papers/rss.xml"); // Natural History
self.science_feeds.push("http://feeds.feedburner.com/physicstoday/pt1?format=xml"); // Physics Today
self.science_feeds.push("http://scitation.aip.org/rss/chaos1.xml"); // Chaos
self.science_feeds.push("http://blogs.aip.org/ControversiesInChaos/atom.xml"); // nonlinear science

self.science_feeds.push("http://scitation.aip.org/rss/jcp1.xml"); // chemistral physics
self.science_feeds.push("http://scitation.aip.org/rss/pof1.xml"); // physics of fluids
self.science_feeds.push("http://rss.sciam.com/sciam/60secsciencepodcast"); // Scientific American Podcast
self.science_feeds.push("http://rss.sciam.com/expeditions/feed"); // Expeditionen
/*
self.science_feeds.push("");
self.science_feeds.push("");
self.science_feeds.push("");
self.science_feeds.push("");

self.science_feeds.push("");
self.science_feeds.push("");
self.science_feeds.push("");
self.science_feeds.push("");

self.science_feeds.push("");
self.science_feeds.push("");
self.science_feeds.push("");
self.science_feeds.push("");
*/
}



this.init_art_feed = function()
{
self.art_feed.push("http://www.pointemagazine.com/taxonomy/term/487/0/feed");
self.art_feed.push(" http://pointemagazine.com/taxonomy/term/452/0/feed");
self.art_feed.push("http://minnesotaballet.org/feed/");
self.art_feed.push("http://www.danceadvantage.net/category/dance-life/genre/ballet-pointe/feed/");
self.art_feed.push("http://oregonballettheatre.blogspot.com/feeds/posts/default?alt=rss");
}


this.init_wheather_feeeds = function()
{
// http://www.spc.noaa.gov/products/spcrss.xml // sturmwarnung USA
// http://www.bt.cdc.gov/rss/hurricanePSAs.xml // Hurricanes
// http://earthquake.usgs.gov/earthquakes/catalogs/eqs7day-M2.5.xml // Erdbeben
// http://earthobservatory.nasa.gov/Feeds/rss/eo.rss // Erdbeobachtung
}

this.init_intelligence_feeds = function()
{

self.intelligence_feeds.push("http://www.us-cert.gov/channels/cas.atom");

self.intelligence_feeds.push("https://www.cia.gov/news-information/your-news/cia-featured-story/RSS.xml"); // CIA
self.intelligence_feeds.push("http://feeds.feedburner.com/IntelligenceOnline"); // Intelligence Online
self.intelligence_feeds.push("http://feeds.feedburner.com/IntelligenceOnlineMiddleEast"); // Intelligence Online

self.intelligence_feeds.push("http://www.nsa.gov/rss.shtml"); // NSA

self.intelligence_feeds.push("http://tools.cisco.com/security/center/threatOutbreak_20.xml"); // Cisco
self.intelligence_feeds.push("http://publicintelligence.net/category/news/feed/rss/"); // PublicIntelligence
self.intelligence_feeds.push("http://publicintelligence.net/category/documents/feed/rss/");

self.intelligence_feeds.push("http://usahitman.com/rss-feed-infowars-com/"); // dito
self.intelligence_feeds.push("http://www.infowars.com/rss");
self.intelligence_feeds.push("http://rss.infowars.com/Alex.rss");

self.intelligence_feeds.push("http://usahitman.com/abovetopsecret-com-rss-feeds/");
self.intelligence_feeds.push("http://usahitman.com/theintelhub-com-rss-feed/");
self.intelligence_feeds.push("http://usahitman.com/disinfo-rss-feed/");
self.intelligence_feeds.push("http://usahitman.com/vigilantcitizen-com-rss-feed/");

self.intelligence_feeds.push(" http://usahitman.com/ozhouse-org-rss-feed/");

self.intelligence_feeds.push("http://usahitman.com/liberty-or-zombie-rss-feed/");
self.intelligence_feeds.push("http://usahitman.com/rt-news-rss-feed/");


}


this.init_technology_feeds = function()
{
self.technology_feeds.push("http://www.computerweekly.com/rss/All-Computer-Weekly-content.xml");
self.technology_feeds.push("http://www.eurekalert.org/rss/technology_engineering.xml"); // Eureka - Technologie
self.technology_feeds.push("http://rss.cbc.ca/lineup/technology.xml");
self.technology_feeds.push("http://phys.org/rss-feed/");
self.technology_feeds.push("http://www.handelsblatt.com/contentexport/feed/technologie");
self.technology_feeds.push("http://blog.panorama.it/hitechescienza/feed/");
self.technology_feeds.push("http://www.3sat.de/mediathek/rss/mediathek_hitec.xml");
self.technology_feeds.push("http://healthblawg.typepad.com/healthblawg/2009/09/hitech-act-security-breach-rules-now-effective-federales-give-a-sixmonth-pass/comments/rss.xml")
/*
self.technology_feeds.push("");
self.technology_feeds.push("");
*/
}


this.init_warfare_feeds = function()
{
self.warfare_feeds.push("http://feeds.feedburner.com/DefenseTech");
self.warfare_feeds.push("http://longwarjournal.org/index.xml");
self.warfare_feeds.push("http://feeds.newscientist.com/weapons");
self.warfare_feeds.push("http://www.hsdl.org/hslog/?q=taxonomy/term/13/all/feed");
self.warfare_feeds.push(" http://www.freetalklive.com/feed/published/FirearmsGunsandWeapons");

}


this.special_feed = function(type)
{
switch(type)
{
case "science":
r = parseInt(Math.random() * self.science_feeds.length );

var sel = 'select * from rss where url="' + self.science_feeds[r] + '"';
var s = 'http://query.yahooapis.com/v1/public/yql?q=' + sel + '&&format=json&callback=?';

console.log(s);
self.special_yahoo_query(s);
break;

case "intelligence":

r = parseInt(Math.random() * self.intelligence_feeds.length );
var sel = 'select * from rss where url="' + self.intelligence_feeds[r] + '"';
var s = 'http://query.yahooapis.com/v1/public/yql?q=' + sel + '&&format=json&callback=?';

console.log(s);

self.special_yahoo_query(s);
break;

case "technology":


r = parseInt(Math.random() * self.technology_feeds.length );
var sel = 'select * from rss where url="' + self.technology_feeds[r] + '"';
var s = 'http://query.yahooapis.com/v1/public/yql?q=' + sel + '&&format=json&callback=?';

console.log(s);

self.special_yahoo_query(s);
break;


case "warfare":
r = parseInt(Math.random() * self.warfare_feeds.length );
var sel = 'select * from rss where url="' + self.warfare_feeds[r] + '"';
var s = 'http://query.yahooapis.com/v1/public/yql?q=' + sel + '&&format=json&callback=?';

console.log(s);

self.special_yahoo_query(s);
break;

case "politics":
var n = self.feed_list.length - 1;
self.admin.display_selected_feed(self.feed_list[n]);
break;

case "art":
r = parseInt(Math.random() * self.art_feed.length );
var sel = 'select * from rss where url="' + self.art_feed[r] + '"';
var s = 'http://query.yahooapis.com/v1/public/yql?q=' + sel + '&&format=json&callback=?';

console.log(s);

self.special_yahoo_query(s);
break;

}
}


this.special_yahoo_query = function(query)
{
var s = query;

$.getJSON(
s,

function(json){
testvar = json.query;

if (json.query.results)
{
j = json.query.results.item;
self.special_request.push(j);

var n = self.special_request.length - 1;

self.admin.display_selected_feed(self.special_request[n]);


// Nur DEMO
var s = 'info_miner.fetched_results(' + j.length + ')';
self.admin.system_tracker.add_element(s);
}
else alert("keine Resultate");
});
}


this.random_rss = function()
{
self.rss_feeds.push("http://www.newsisfree.com/HPE/xml/feeds/10/1210.xml");
self.rss_feeds.push("http://www.guardian.co.uk/rss/1,,,00.xml"); // Guardian
self.rss_feeds.push("http://www.newsisfree.com/HPE/xml/feeds/87/2087.xml"); // WashingtinPost
self.rss_feeds.push("http://www.newsisfree.com/HPE/xml/feeds/11/3411.xml"); // Newsweek
self.rss_feeds.push("http://www.newsisfree.com/HPE/xml/feeds/15/2315.xml "); // Washington Post
self.rss_feeds.push("http://www.newsisfree.com/HPE/xml/feeds/10/1210.xml"); // Herold Tribune
self.rss_feeds.push("http://www.newsisfree.com/HPE/xml/feeds/23/4623.xml"); // Forbes
self.rss_feeds.push("http://www.newsisfree.com/HPE/xml/feeds/79/79.xml"); // BBC

// http://hosted2.ap.org/atom/APDEFAULT/89ae8247abe8493fae24405546e9a1aa // AP - Politics


self.rss_feeds.push("http://www.faz.net/aktuell/politik/?rssview=1"); // FAZ - Politik
self.rss_feeds.push("http://www.faz.net/aktuell/wirtschaft/?rssview=1"); // FAZ - Wirtschaft
self.rss_feeds.push("http://www.faz.net/aktuell/finanzen/?rssview=1"); // FAZ - Wirtschaft
self.rss_feeds.push("http://www.faz.net/aktuell/gesellschaft/?rssview=1"); // FAZ - Gesellschaft

self.rss_feeds.push("http://pheedo.msnbc.msn.com/id/3033076/device/rss/"); // NBC - Security

self.rss_feeds.push("http://rssfeed.sueddeutsche.de/c/795/f/448238/index.rss"); // Süddeutsche Politik

self.rss_feeds.push("https://www.cia.gov/news-information/your-news/cia-featured-story/RSS.xml"); // CIA
self.rss_feeds.push("http://feeds.feedburner.com/IntelligenceOnline"); // Intelligence Online
self.rss_feeds.push("http://feeds.feedburner.com/IntelligenceOnlineMiddleEast"); // Intelligence Online

self.rss_feeds.push("http://www.nsa.gov/rss.shtml"); // NSA


r = parseInt(Math.random() * self.rss_feeds.length );


var sel = 'select * from rss where url="' + self.rss_feeds[r] + '"';
var s = 'http://query.yahooapis.com/v1/public/yql?q=' + sel + '&&format=json&callback=?';


self.yahoo_query(s);

}






this.yahoo_query = function(query) // funktioniert - d.i. der Workflow
{
// var s = 'http://query.yahooapis.com/v1/public/yql?q=select * from geo.places where text="sunnyvale, ca"&&format=json&callback=?';
var s = query;

$.getJSON(

s,

function(json){
testvar = json.query;

if (json.query.results)
{
j = json.query.results.item;
self.feed_list.push(j);


var s = 'info_miner.fetched_results(' + j.length + ')';
self.admin.system_tracker.add_element(s);
}
});

}




this.test_yahoo_query = function(query) // funktioniert - d.i. der Workflow
{
// var s = 'http://query.yahooapis.com/v1/public/yql?q=select * from geo.places where text="sunnyvale, ca"&&format=json&callback=?';
var s = query;

$.getJSON(

s,

function(json){

if (json.query.results)
{
j = json.query.results.item;
self.special_request.push(j);

var n = self.special_request.length - 1;

self.admin.display_selected_feed(self.special_request[n]);


// Nur DEMO
var s = 'info_miner.fetched_results(' + j.length + ')';
self.admin.system_tracker.add_element(s);
}
else alert("keine Resultate");



});

}


// TESTFUNKTION, um zu sehen, ob die Feeds laufen

this.yahoo_rss = function(url)
{
// var feed = 'http://rss.infowars.com/Alex.rss'; // Intelligence Online
// var feed = "http://www.infowars.com/rss";
// var feed = "http://feeds.feedburner.com/LongWarJournalPhotos";

var feed = "http://longwarjournal.org/index.xml";

var sel = 'select * from rss where url="' + feed + '"';
var s = 'http://query.yahooapis.com/v1/public/yql?q=' + sel + '&&format=json&callback=?';

self.test_yahoo_query(s);


}


this.flickr_search = function(query)
{

var sel = 'select * from flickr.photos.search where text="' + query + '" and api_key="e69d628addf15b082c0edb4ba4576ae1" ';
var s = 'http://query.yahooapis.com/v1/public/yql?q=' + sel + '&&format=json&callback=?';

$.getJSON(
s,

function(json){

testfall = json.query.results;
list = new Array();

for (i = 0; i < testfall.photo.length; i++)
{
var link = 'http://www.flickr.com/photos/' + testfall.photo[i].owner + '/' + testfall.photo[i].id;

}

});

}


this.ask_yahoo = function(query)
{

//
// url = 'http://query.yahooapis.com/v1/public/yql?q=select * from geo.places where text="sunnyvale, ca" ';

url = 'http://query.yahooapis.com/v1/public/yql?q=select title from rss where url="http://www.heise.de/newsticker/heise.rdf" limit 10';


$.ajax({
type : "GET",
url : url

}).done(function(msg) {

alert("ist angekommen");

});


}


this.init_feeds = function()
{
self.init_science_feeds();
self.init_intelligence_feeds();
self.init_technology_feeds();
self.init_warfare_feeds();
self.init_art_feed();
}


this.init = function()
{
info_grabber = self;

self.init_feeds();
// self.you_tube("cia");
}


self.init();
}



Hier bekomme ich verschiedene Divs und Links


select * from html where url="http://welt.de" and xpath='//div/h4/a'



Das ergibt nur die Links

select * from html where url="http://welt.de" and xpath='//a'


Überschrift H4
select * from html where url="http://welt.de" and xpath='//h4'


Hier bekomme ich die Links und die Überschriften
select href, content from html where url="http://welt.de" and xpath='//h4/a/'


Hier kommen alle Paragraphen
select content from html where url="http://welt.de" and xpath='//p'


Das geht auch, offenbar werden die Resultate on eine Liste geschrieben
select * from html where url="http://www.shinty.com/news/" and xpath="//div[@class=\'topStories\']/ul/li"

select * from html where url="http://www.shinty.com/news/" and xpath="//div[@class=\'spSearchResultItem\']"



Test
select href, content from html where url="http://welt.de" and xpath='//h4/a/'


aus dem Firebug
/html/body/div[3]/div[2]/div[2]/div/div/div[7]/h4/a



WIKIPEDIA
=========

select * from html where url="http://en.wikipedia.org/wiki/David_Guetta" and
xpath='//table[@class="infobox vcard"]/tr[@class=""]'

Das obige funktioniert, auch dies

select * from html where url="http://en.wikipedia.org/wiki/Christmas_Islands"and xpath="//table/following-sibling::p" limit 3

ist nicht ganz sauber

select content from html where url="http://de.wikipedia.org/wiki/Essen"and xpath="//table/following-sibling::p" limit 3

Das holt die ganze Seite:

select * from html where url="http://en.wikipedia.org/wiki/Christmas_Islands"and xpath="//body"

funktioniert ganz ordentlich:

select * from html where url="http://de.wikipedia.org/wiki/Hans_Moser"and xpath="//div[@id=\'bodyContent\']"


