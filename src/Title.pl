:-module('Title', []).

render(_,Props,Form):-
        {|jsx(Form)||
        <Label label={Props.label} className="title"/>|}.