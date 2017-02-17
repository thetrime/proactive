"use strict";

var ReactComponent = require('./react_component');
var Prolog = require('proscript');


function textInputHandler(event)
{
    event.preventDefault();
    var oldValue = this.domNode.value;
    var newValue = oldValue.slice(0, this.domNode.selectionStart) + event.data + oldValue.slice(this.domNode.selectionEnd);
    this.proposedCaretPosition = this.domNode.selectionStart + event.data.length;
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
        {
            if (this.domNode.selectionStart == 0) // This has no effect anyway, and if we do not return here then oldValue.slice(0, -1) is the entire string!
                return;
            newValue = oldValue.slice(0, this.domNode.selectionStart-1) + oldValue.slice(this.domNode.selectionEnd);
            this.proposedCaretPosition = this.domNode.selectionStart-1
        }
        else
        {
            newValue = oldValue.slice(0, this.domNode.selectionStart) + oldValue.slice(this.domNode.selectionEnd);
            this.proposedCaretPosition = this.domNode.selectionStart;
        }
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
        {
            newValue = oldValue.slice(0, this.domNode.selectionStart) + oldValue.slice(this.domNode.selectionEnd+1);
            this.proposedCaretPosition = this.domNode.selectionStart;
        }
        else
        {
            newValue = oldValue.slice(0, this.domNode.selectionStart) + oldValue.slice(this.domNode.selectionEnd);
            this.proposedCaretPosition = this.domNode.selectionStart;
        }
        this.valueWouldChange(newValue);
        //console.log(this.domNode.value + "-keydown>" + newValue);
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
    var newValue = oldValue.slice(0, this.domNode.selectionStart) + oldValue.slice(this.domNode.selectionEnd);
    this.valueWouldChange(newValue);
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
        this.getOwnerDocument().triggerEvent(this.verifyValue, ReactComponent.serialize({value: Prolog._make_atom(this.domNode.value)}), function(success)
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
                                                 {
                                                     this.getOwnerDocument().triggerEvent(this.blurHandler, ReactComponent.serialize({value: Prolog._make_atom(this.domNode.value)}), handlerCallback);
                                                 }
                                             }.bind(this));
    }
    else if (this.blurHandler != null)
    {
        this.getOwnerDocument().triggerEvent(this.blurHandler, ReactComponent.serialize({value: Prolog._make_atom(this.domNode.value)}), handlerCallback);
    }

}

function TextArea()
{
    ReactComponent.call(this);
    this.baseClassName = "field";
    this.changeHandler = null;
    var node = document.createElement("textarea");
    this.setDOMNode(node);
    this.verifyValue = null;
    this.blurHandler = null;
    this.changeHandler = null;
    this.proposedCaretPosition = null;
    this.domNode.onblur = blurHandler.bind(this);

    // What a mess :(
    // onInput is no good for our requirements, since it is not cancellable (it is fired /after/ the field has changed)
    // onKeyPress is broken in Firefox: It fires even for keypresses that do not change the field, like tab and arrow keys. It does not fire at all in Safari.
    // onTextInput does not work on firefox, and only fires if the text is added - delete and cut do not trigger it. It is also not standard.

    // So, first add onTextInput, since if that is around it does most of what we need

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
}
TextArea.prototype = new ReactComponent;

TextArea.prototype.valueWouldChange = function(newValue)
{
    if (this.changeHandler != null)
        this.getOwnerDocument().triggerEvent(this.changeHandler, ReactComponent.serialize({value: Prolog._make_atom(newValue)}), handlerCallback);
}


TextArea.prototype.setProperties = function(t)
{
    ReactComponent.prototype.setProperties.call(this, t);
    if (t.value !== undefined)
    {
        if (ReactComponent.isNull(t.value))
            this.domNode.value = '';
        else
            this.domNode.value = Prolog._atom_chars(t.value);
        if (this.proposedCaretPosition != null && this.domNode.value.length >= this.proposedCaretPosition)
            this.domNode.setSelectionRange(this.proposedCaretPosition, this.proposedCaretPosition);
    }
    // Reset this between events
    this.proposedCaretPosition = null;
    if (t.disabled !== undefined)
    {
        this.domNode.disabled = ReactComponent.booleanValue(t.disabled);
    }
    if (t.renderContextMenu !== undefined)
    {
        // FIXME: implement
    }
    if (t.onContextMenu !== undefined)
    {
        // FIXME: implement
    }
    if (t.onBlur !== undefined)
    {
        if (this.blurHandler != null)
            Prolog._free_local(this.blurHandler);
        if (ReactComponent.isNull(t.onBlur))
            this.blurHandler = null;
        else
            this.blurHandler = Prolog._make_local(t.onBlur);
    }
    if (t.verifyValue !== undefined)
    {
        if (this.verifyValue != null)
            Prolog._free_local(this.verifyValue);
        if (ReactComponent.isNull(t.verifyValue))
            this.verifyValue = null;
        else
            this.verifyValue = Prolog._make_local(t.verifyValue);
    }
    if (t.onChange !== undefined)
    {
        if (this.changeHandler != null)
            Prolog._free_local(this.changeHandler);
        if (ReactComponent.isNull(t.onChange))
            this.changeHandler = null;
        else
            this.changeHandler = Prolog._make_local(t.onChange);
    }
    if (t.align !== undefined)
    {
        this.domNode.style["text-align"] = Prolog._atom_chars(t.align);
    }
    if (t.maxWidth !== undefined)
    {
        this.domNode.style["max-width"] = Prolog._atom_chars(t.maxWidth);
    }

    if (t.id !== undefined)
    {
        if (ReactComponent.isNull(t.id))
            this.domNode.id = null;
        else
            this.domNode.id = Prolog._portray(t.id);
    }
    if (t.title !== undefined)
    {
        if (ReactComponent.isNull(t.title))
            this.domNode.title = "";
        else
            this.domNode.title = Prolog._atom_chars(t.title);
    }

}


module.exports = TextArea;
