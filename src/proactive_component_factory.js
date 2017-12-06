var Panel = require('./panel');
var Button = require('./button');
var Broken = require('./broken');
var Label = require('./label');
var List = require('./list');
var ListItem = require('./list_item');
var Table = require('./table');
var TableHeader = require('./table_header');
var TableFooter = require('./table_footer');
var TabbedPane = require('./tabbed_pane');
var Tab = require('./tab');
var Tree = require('./tree');
var TreeNode = require('./tree_node');
var Row = require('./row');
var Field = require('./field');
var ComboBox = require('./combo_box');
var ComboItem = require('./combo_item');
var Title = require('./title');
var Frame = require('./frame');
var Image = require('./image');
var TextArea = require('./textarea');
var Grid = require('./grid');
var PopupMenu = require('./popup_menu');
var MenuItem = require('./menu_item');
var CallOut = require('./call_out');

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
        case "TableFooter": return new TableFooter();
        case "Row": return new Row();
        case "Field": return new Field();
        case "TabbedPane": return new TabbedPane();
        case "Tab": return new Tab();
        case "ComboBox": return new ComboBox();
        case "ComboItem": return new ComboItem();
        case "Frame": return new Frame();
        case "Image": return new Image();
        case "TextArea": return new TextArea();
        case "Grid": return new Grid();
        case "PopupMenu": return new PopupMenu();
        case "MenuItem": return new MenuItem();
        case "Tree": return new Tree();
        case "TreeNode": return new TreeNode();
        case "CallOut": return new CallOut();
        default: return new Broken(name);
    }
}

module.exports.registerComponent = function(name, constructor)
{
    overrides[name] = constructor;
}

