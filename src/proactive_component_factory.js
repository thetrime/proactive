module.exports.createElement = function(name, doc)
{
    var element;
    switch(name)
    {
        case "Panel":
        {
            element = document.createElement("div");
            element.className = "panel";
        }
        default:
        {
            element = document.createElement("div");
            element.className = "unknown";
        }
    }
    return element;
}
