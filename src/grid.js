var ReactComponent = require('./react_component');
var Prolog = require('proscript');
var Constants = require('./constants.js');

function Grid()
{
    ReactComponent.call(this);
    this.baseClassName = "grid"
    this.setDOMNode(document.createElement("div"));
    this.weights = [0];
    this.padding = null;
}
Grid.prototype = new ReactComponent;

Grid.prototype.setProperties = function(t)
{
    ReactComponent.prototype.setProperties.call(this, t);
    var must_relayout = false;
    if (t.weights !== undefined)
    {
        var list = t.weights;
        var newWeights = [];
        while (Prolog._is_compound(list) && Prolog._term_functor(list) == Constants.listFunctor)
        {
            newWeights.push(ReactComponent.numericValueOr(Prolog._term_arg(list, 0), 0));
            list = Prolog._term_arg(list, 1);
        }
        if (list != Constants.emptyListAtom)
            console.log("Bad weights list!");
        // If the weights have changed, relayout
        if (newWeights.length != this.weights.length || !newWeights.every(function(v,i) { return v === this.weights[i]}.bind(this)))
            must_relayout = true;
        console.log(newWeights);
        this.weights = newWeights;
    }
    if (t.gap !== undefined)
    {
        if (ReactComponent.isNull(t.gap))
            this.getDOMNode().style['grid-gap'] = "";
        else
            this.getDOMNode().style['grid-gap'] = Prolog._atom_chars(t.gap);
    }
    if (t.padding !== undefined)
    {
        this.padding = Prolog._atom_chars(t.padding);
        must_relayout = true;
    }
    if (must_relayout)
        this.relayout();
}

Grid.prototype.relayout = function(t)
{
    var template = "";
    var total = 0;
    for (var i = 0; i < this.weights.length; i++)
    {
        if (this.weights[i] == 0)
            template += "max-content ";
        else
            template += this.weights[i] + "fr ";
    }
    console.log("Grid template columns: " + template);
    this.getDOMNode().style["grid-template-columns"] = template;
}


module.exports = Grid;
