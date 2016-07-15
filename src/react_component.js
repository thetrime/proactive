function ReactComponent()
{
    this.owner = null;
}

ReactComponent.prototype.setProperties = function(t)
{
    console.log("setProperties called with: " + t);
    // FIMXE: implement
}

ReactComponent.prototype.setOwnerDocument = function(d)
{
    this.owner = d;
}

module.exports = ReactComponent;
