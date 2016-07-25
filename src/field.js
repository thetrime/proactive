"use strict";

var Prolog = require('../lib/proscript2/src/core.js');
var ReactComponent = require('./react_component');


function textInputHandler(event)
{
    event.preventDefault();
    var oldValue = this.domNode.value;
    var newValue = oldValue.slice(0, this.domNode.selectionStart) + event.data + oldValue.slice(this.domNode.selectionEnd);
    this.valueWouldChange(newValue);
    return false;
}

function keydownHandler(event)
{
    if (event.keyCode == 8) // backspace
    {
        event.preventDefault();
        var newValue;
        var oldValue = this.domNode.value;
        if (this.domNode.selectionStart === this.domNode.selectionEnd)
            newValue = oldValue.slice(0, this.domNode.selectionStart-1) + oldValue.slice(this.domNode.selectionEnd);
        else
            newValue = oldValue.slice(0, this.domNode.selectionStart) + oldValue.slice(this.domNode.selectionEnd);
        //        console.log(this.domNode.value + "-keydown>" + newValue);
        this.valueWouldChange(newValue);
        return false;
    }
    else if (event.keyCode == 46) // delete
    {
        event.preventDefault();
        var newValue;
        var oldValue = this.domNode.value;
        if (this.domNode.selectionStart === this.domNode.selectionEnd)
            newValue = oldValue.slice(0, this.domNode.selectionStart) + oldValue.slice(this.domNode.selectionEnd+1);
        else
            newValue = oldValue.slice(0, this.domNode.selectionStart) + oldValue.slice(this.domNode.selectionEnd);
        this.valueWouldChange(newValue);
        //console.log(this.domNode.value + "-keydown>" + newValue);
        return false;
    }
    else if (event.keyCode == 32 && (this.type == "radio" || this.type == "checkbox")) // space
    {
        event.preventDefault();
        if (this.value == "true")
            this.valueWouldChange("false");
        else
            this.valueWouldChange("true");
        return false;
    }
    return true;
}

// This handler only fires for Firefox
function keypressHandler(event)
{
    // Firefox (and Opera) fire keyPress for special keys, which they shouldn't. If we cancel those (which should be impossible!) then
    // arrow keys, tab, etc, won't work, which is an awful user experience. Check for those here and bail out immediately.
    if (event.which == 0)
        return true;

    event.preventDefault();
    var oldValue = this.domNode.value;
    var newValue = oldValue.slice(0, this.domNode.selectionStart) + event.key + oldValue.slice(this.domNode.selectionEnd);
    //    console.log(this.domNode.value + "-keypress>" + newValue);
    this.valueWouldChange(newValue);
    return false;
}

function pasteHandler(event)
{
    event.preventDefault();
    console.log(event);
    var oldValue = this.domNode.value;
    var clipboardData = event.clipboardData || window.clipboardData;
    var pastedData = clipboardData.getData('Text');
    var newValue = oldValue.slice(0, this.domNode.selectionStart) + pastedData + oldValue.slice(this.domNode.selectionEnd);
    //    console.log(this.domNode.value + "-paste>" + newValue);
    this.valueWouldChange(newValue);
}

function cutHandler(event)
{
    event.preventDefault();
    var oldValue = this.domNode.value;
    // This would be REALLY hard to implement, but it might be possible if necessary. The important thing is that we cannot copy to the clipboard ourselves
    // so if we want this cut to actually happen, then we must NOT cancel the default event. Instead we must fire the valueWouldChange() then if, when the widget
    // is next rendered, it has the proposed value as its value, then we just let the event go as if the user really did cut. Otherwise we must cancel it and
    // upset the user?
    newValue = oldValue.slice(0, this.domNode.selectionStart) + oldValue.slice(this.domNode.selectionEnd);
    this.valueWouldChange(newValue);
}

function clickHandler(event)
{
    if ((this.type == "checkbox" || this.type == "radio") && !this.domNode.disabled)
    {
        event.preventDefault();
        if (this.value == "true")
            this.valueWouldChange("false");
        else
            this.valueWouldChange("true");
        return false;
    }
}

function handlerCallback(success)
{
    if (success == true)
    {
    }
    else if (success == false)
    {
        console.log("Event failed");
    }
    else
    {
        console.log("Event raised: " + success.toString());
    }

}

function blurHandler(event)
{
    if (this.verifyValue != null)
    {
        this.getOwnerDocument().triggerEvent(this.verifyValue, ReactComponent.serialize({value: new Prolog.AtomTerm(this.domNode.value)}), function(success)
                                             {
                                                 if (success != true)
                                                 {
                                                     if (success == false)
                                                         console.log("Verification failed");
                                                     else
                                                         console.log("Verification raised error: " + success.toString());
                                                     this.domNode.focus();
                                                 }
                                                 else if (this.blurHandler != null)
                                                     this.getOwnerDocument().triggerEvent(this.blurHandler, ReactComponent.serialize({value: new Prolog.AtomTerm(this.domNode.value)}), handlerCallback);
                                             }.bind(this));
    }
    else if (this.blurHandler != null)
    {
        this.getOwnerDocument().triggerEvent(this.blurHandler, ReactComponent.serialize({value: new Prolog.AtomTerm(this.domNode.value)}), handlerCallback);
    }

}

var global_field_id = 0;

function Field()
{
    ReactComponent.call(this);
    this.changeHandler = null;
    var node = document.createElement("input");
    node.id = "field_" + (global_field_id++);
    this.setDOMNode(node);
    this.type = "text";

    this.domNode.onblur = blurHandler.bind(this);

    // What a mess :(
    // onInput is no good for our requirements, since it is not cancellable (it is fired /after/ the field has changed)
    // onKeyPress is broken in Firefox: It fires even for keypresses that do not change the field, like tab and arrow keys. It does not fire at all in Safari.
    // onTextInput does not work on firefox, and only fires if the text is added - delete and cut do not trigger it. It is also not standard.

    // So, first add onTextInput, since if that is around it does most of what we need

    this.domNode.nodeCallback = textInputHandler.bind(this);

    this.domNode.addEventListener("textInput", textInputHandler.bind(this), false);
    // Also add in a handler that JUST listens for deletes
    this.domNode.addEventListener("keydown", keydownHandler.bind(this), false);
    // and one for cut that just prevents the event
    this.domNode.addEventListener("cut", cutHandler.bind(this), false);

    // Now we have the scraps. Firefox will require us to have a keypress event handler that ignores special keys, and a separate paste handler
    // If we want to support Opera then we need to do this for that engine too. I don't have Opera to test, though.
    if (typeof navigator !== 'undefined' && navigator.userAgent.toLowerCase().indexOf('firefox') > -1)
    {
        this.domNode.addEventListener("keypress", keypressHandler.bind(this), false);
        this.domNode.addEventListener("paste", pasteHandler.bind(this), false);
    }

    // Finally, we must deal with checkboxes and radios
    this.domNode.onclick = clickHandler.bind(this);
}
Field.prototype = new ReactComponent;

Field.prototype.valueWouldChange = function(newValue)
{
    var d0 = new Date().getTime();
    if (this.changeHandler != null)
    {
        //this.getOwnerDocument().triggerEvent(this.changeHandler, ReactComponent.serialize({value: new Prolog.AtomTerm(newValue)}), handlerCallback);
        this.getOwnerDocument().debugStuff();
        this.getOwnerDocument().triggerEvent(this.changeHandler, ReactComponent.serialize({value: new Prolog.AtomTerm(newValue)}),
                                             function(result)
                                             {
                                                 var d1 = new Date().getTime();
                                                 console.log("Processing time: " + (d1-d0) + "ms");
                                                 this.getOwnerDocument().debugStuff();
                                             }.bind(this));
    }
    else
        console.log("No change handler. Field will not be able to be changed");
}

Field.prototype.setBlurHandler = function(value)
{
    if (ReactComponent.isNull(value))
        this.blurHandler = null;
    this.blurHandler = value;
}

Field.prototype.setProperties = function(t)
{
    if (t.type !== undefined)
    {
        if (t.type != this.type)
        {
            this.type = t.type;
            this.domNode.type = this.type;
            this.setValue(this.value);
        }
    }
    if (t.value !== undefined)
    {
        this.value = t.value;
        this.setValue(t.value);
    }
    if (t.onBlur !== undefined)
    {
        this.setBlurHandler(t.onBlur);
    }
    if (t.disabled !== undefined)
        this.domNode.disabled = ReactComponent.booleanValue(t.disabled);
    if (t.renderContextMenu !== undefined)
    {
        // FIXME: implement
    }
    if (t.onContextMenu !== undefined)
    {
        // FIXME: implement
    }
    if (t.verifyValue !== undefined)
    {
        this.verifyValue = t.verifyValue;
    }
    if (t.onChange !== undefined)
    {
        if (ReactComponent.isNull(t.onChange))
            this.changeHandler = null;
        else
            this.changeHandler = t.onChange;
    }
    if (t.align !== undefined)
    {
        this.domNode.style["text-align"] = t.align;
    }
}

Field.prototype.setValue = function(text)
{
    if (text === null || ReactComponent.isNull(text))
    {
        if (this.type == "radio" || this.type == "checkbox")
        {
            // For some reason, Chrom will NOT change the value of .checked if we are executing the onclick handler. Since this is when we USUALLY want to change it
            // we have to employ a slightly weird workaround here
            setTimeout(function() {this.domNode.checked = false}.bind(this), 0);
        }
        else
            this.domNode.value = "";
    }
    else
    {
        if (this.type == "radio" || this.type == "checkbox")
        {
            if (text == "true")
            {
                setTimeout(function() {this.domNode.checked = true}.bind(this), 0);
            }
            else
            {
                setTimeout(function() {this.domNode.checked = false}.bind(this), 0);
            }
        }
        else
            this.domNode.value = text;
    }
}


Field.prototype.createWidget = function()
{
    this.domNode.removeChild(this.field);
    switch(this.type)
    {
        case "text":
        {
            this.field = document.createElement("input");
            break;
        }
        case "password":
        {
            this.field = document.createElement("input");
            break;
        }
        case "text":
        {
            this.field = document.createElement("input");
            break;
        }

    }
}




module.exports = Field;
