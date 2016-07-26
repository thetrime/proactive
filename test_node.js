GLOBAL.window = GLOBAL;

var Proactive = require('./src/proactive.js');
document = require("jsdom").jsdom('<div style="height: 100%; margin: 0; padding: 0; overflow: scroll;" id="container"></div>');
var d0 = new Date().getTime();
var util = require('util');
Proactive.render("http://localhost:8080/react", "trade_entry_proactive", document.getElementById("container"),
                 function()
                 {
                     console.log("Rendered in " + (new Date().getTime() - d0) + "ms");
                     var field = document.getElementById("field_0");
                     console.log("Here is the element: " + util.inspect(field, {depth:null}));
                     if (field != undefined)
                         field.nodeCallback({data:'q', preventDefault:function() {}});
                 });
