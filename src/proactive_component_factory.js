var Panel = require('./panel');
var Button = require('./button');
var Broken = require('./broken');
var Label = require('./label');
var List = require('./list');
var ListItem = require('./list_item');
var Table = require('./table');
var TableHeader = require('./table_header');
var Row = require('./row');
module.exports.createElement = function(name)
{
    var element;
    switch(name)
    {
        case "Panel": return new Panel();
        case "Button": return new Button();
        case "Label": return new Label();
        case "List": return new List();
        case "ListItem": return new ListItem();
        case "Table": return new Table();
        case "TableHeader": return new TableHeader();
        case "Row": return new Row();
        default: return new Broken(name);
    }
}

