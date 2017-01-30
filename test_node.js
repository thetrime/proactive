GLOBAL.window = GLOBAL;

var Proactive = require('./src/proactive.js');
document = require("jsdom").jsdom('<div style="height: 100%; margin: 0; padding: 0; overflow: scroll;" id="container"></div>');
var d0 = new Date().getTime();
var util = require('util');
Proactive.render("http://localhost:8080/react", "trade_entry_proactive", document.getElementById("container"),
                 function()
                 {
                     console.log("Rendered in " + (new Date().getTime() - d0) + "ms");
                     GLOBAL.total = 0;
                     var field = document.getElementById("field_0");
                     console.log(field);
                     var count = 10000;
                     Proactive._qqq();
                     for (var i = 0; i < count; i++)
                     {
                         field.nodeCallback({data:'q', preventDefault:function() {}});
                         if ((i % 500) == 0)
                         {
                             console.log(i);
                             Proactive._qqq();
                         }
                     }
                     console.log("Average after  " + count + ": " + (GLOBAL.total / count) + "ms per event");
                     Proactive._qqq();
                 });
