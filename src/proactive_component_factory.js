var Panel = require('./panel');
var Button = require('./button');
var Broken = require('./broken');
var Label = require('./label');
var List = require('./list');
var ListItem = require('./list_item');
var Table = require('./table');
var TableHeader = require('./table_header');
var TabbedPane = require('./tabbed_pane');
var Tab = require('./tab');
var Row = require('./row');
var Field = require('./field');
var ComboBox = require('./combo_box');
var ComboItem = require('./combo_item');
var Title = require('./title');
var Frame = require('./frame');

var overrides = {};

module.exports.createElement = function(name)
{
    var element;
    if (overrides[name] != undefined)
        return overrides[name]();
    switch(name)
    {
        case "Panel": return new Panel();
        case "Button": return new Button();
        case "Label": return new Label();
        case "List": return new List();
        case "ListItem": return new ListItem();
        case "Table": return new Table();
        case "Title": return new Title();
        case "TableHeader": return new TableHeader();
        case "Row": return new Row();
        case "Field": return new Field();
        case "TabbedPane": return new TabbedPane();
        case "Tab": return new Tab();
        case "ComboBox": return new ComboBox();
        case "ComboItem": return new ComboItem();
        case "Frame": return new Frame();
        default: return new Broken(name);
    }
}

module.exports.registerComponent = function(name, constructor)
{
    overrides[name] = constructor;
}

