:-module(dom,
         [remove_child/2,
          append_child/2,
          insert_before/3,
          replace_child/3,
          child_nodes/2,
          create_element/3,
          create_text_node/3,
          parent_node/2,
          node_type/2,
	  set_vdom_properties/2,
          replace_node_data/2,
          init_widget/3,
          update_widget/4,
          destroy_widget/2,

          proactive_term_to_state/2,
          get_store_state/2,
	  state_to_term/2,
          get_this/1,
          widget_id/1
         ]).

% ----------------- the real DOM. Implemented in SWI by attributed variables

remove_child(DomNode, Child):-
        DomNode = dom_element(Attributes),
        memberchk(children-ChildrenPtr, Attributes),
        get_attr(ChildrenPtr, react_dom, ChildNodes),
        subtract(ChildNodes, [Child], NewChildNodes),
        move_child_to(Child, {null}),
        put_attr(ChildrenPtr, react_dom, NewChildNodes).

append_child(DomNode, Child):-
        DomNode = dom_element(Attributes),
        memberchk(children-ChildrenPtr, Attributes),
        move_child_to(Child, DomNode),
        get_attr(ChildrenPtr, react_dom, ChildNodes),
        append(ChildNodes, [Child], NewChildNodes),
        put_attr(ChildrenPtr, react_dom, NewChildNodes).

insert_before(DomNode, Child, Sibling):-
        DomNode = dom_element(Attributes),
        memberchk(children-ChildrenPtr, Attributes),
        get_attr(ChildrenPtr, react_dom, ChildNodes),
        move_child_to(Child, DomNode),
        insert_child_ptr(ChildNodes, Child, Sibling, NewChildNodes),
        put_attr(ChildrenPtr, react_dom, NewChildNodes).


replace_child(DomNode, New, Old):-
        DomNode = dom_element(Attributes),
        memberchk(children-ChildrenPtr, Attributes),
        get_attr(ChildrenPtr, react_dom, ChildNodes),
        move_child_to(New, DomNode),
        replace_child_ptr(ChildNodes, New, Old, NewChildNodes),
        put_attr(ChildrenPtr, react_dom, NewChildNodes).

create_element(Document, TagName, DomNode):-
        DomNode = dom_element([tag-TagName,
                               type-node,
                               children-ChildrenPtr,
                               document-Document,
                               properties-PropertiesPtr,
                               parent-ParentPtr]),
        put_attr(ChildrenPtr, react_dom, []),
        put_attr(PropertiesPtr, react_dom, []),
        put_attr(ParentPtr, react_dom, {null}).

set_vdom_properties(DomNode, NewProperties):-
        DomNode = dom_element(Attributes),
        memberchk(properties-PropertiesPtr, Attributes),
        put_attr(PropertiesPtr, react_dom, NewProperties).

create_text_node(Document, Data, DomNode):-
        DomNode = dom_element([type-text,
                               children-ChildrenPtr,
                               parent-ParentPtr,
                               document-Document,
                               data-DataPtr]),
        put_attr(ChildrenPtr, react_dom, []),
        put_attr(ParentPtr, react_dom, {null}),
        put_attr(DataPtr, react_dom, Data).

move_child_to(Child, DomNode):-
        Child = dom_element(ChildAttributes),
        memberchk(parent-ParentPtr, ChildAttributes),
        put_attr(ParentPtr, react_dom, DomNode).

change_attributes([Name=_|Properties], Name, Value, [Name=Value|Properties]):- !.
change_attributes([], Name, Value, [Name=Value]):- !.
change_attributes([X|In], Name, Value, [X|Out]):-
        change_attributes(In, Name, Value, Out).

replace_child_ptr([], _, _, []):- !.
replace_child_ptr([Old|X], New, Old, [New|X]):- !.
replace_child_ptr([X|In], New, Old, [X|Out]):-
        replace_child_ptr(In, New, Old, Out).

insert_child_ptr([], Child, _, [Child]):- !.
insert_child_ptr([Sibling|Tail], Child, Sibling, [Child,Sibling|Tail]):- !.
insert_child_ptr([X|In], Child, Sibling, [X|Out]):-
        insert_child_ptr(In, Child, Sibling, Out).

child_nodes(DomNode, ChildNodes):-
        DomNode = dom_element(Attributes),
        memberchk(children-ChildrenPtr, Attributes),
        get_attr(ChildrenPtr, react_dom, ChildNodes).

parent_node(DomNode, ParentNode):-
        DomNode = dom_element(Attributes),
        memberchk(parent-ParentPtr, Attributes),
        ( get_attr(ParentPtr, react_dom, ParentNode)->
            true
        ; otherwise->
            ParentNode = {null}
        ).

replace_node_data(DomNode, NewData):-
        DomNode = dom_element(Attributes),
        memberchk(data-DataPtr, Attributes),
        put_attr(DataPtr, react_dom, NewData).


destroy_widget(_DomNode, _Widget).

init_widget(_, VNode, DomNode):-
        VNode = widget(Tag, Attributes, _),
        ( current_predicate(Tag:getInitialState/2)->
            Tag:getInitialState(Attributes, StateTerm),
            proactive_term_to_state(StateTerm, State)
        ; otherwise->
            State = proactive{}
        ),
        Tag:render(State, Attributes, VDom),
        create_element_from_vdom([], VDom, DomNode).

update_widget(_Widget, VNode, DomNode, NewNode):-
        VNode = widget(Tag, Attributes, _),
        State = ?, % FIXME: Need to recover this from DomNode, I guess
        Tag:render(State, Attributes, VDom),
	vdiff(VNode, VDom, Patches),
	vpatch(DomNode, Patches, [document(_Document)], NewNode).

node_type(DomNode, Type):-
        DomNode = dom_element(Attributes),
        memberchk(type-Type, Attributes).


get_this({this}).
widget_id({widget}).

% DOM node is represented as dom_node(Attributes)
%  Attributes must contain:
%     * type-Atom
%     * children-AttributedVar
%     * parent-AttributedVar
%     * document-DocumentVar
% And MAY contain these depending on the type (they only need to appear in the right element type)
%     * data-AttributedVar if type is text
%     * tag-Atom if type is node
%     * properties-AttributedVar if type is node



test:-
        Document = doc,
        InitialTree = element('Panel', [], []),
        create_element(Document, 'Panel', InitialRoot),
        Tree1 = element('Panel', [], [element('Field', [label=hello], []),
                                      element('Button', [label=submit], [])]),
        Tree2 = element('Panel', [], [element('Field', [label=hello], []),
                                      element('Field', [label=second], []),
                                      element('Button', [label=submit], [])]),
	vdiff(InitialTree, Tree1, Patch1),
	vdiff(Tree1, Tree2, Patch2),
	debug_message(patch:Patch1),
	debug_message(patch:Patch2),
	vpatch(InitialRoot, Patch1, [document(Document)], IntermediateRoot),
	vpatch(IntermediateRoot, Patch2, [document(Document)], FinalRoot),
        crystalize([FinalRoot]),
	debug_message(FinalRoot).

crystalize([]):- !.
crystalize([DomNode|DomNodes]):-
        DomNode = dom_element(Attributes),
        crystalize_attributes(Attributes),
        crystalize(DomNodes).

crystalize_attributes([]):- !.
crystalize_attributes([parent-_|Attributes]):-
        !,
        crystalize_attributes(Attributes).
crystalize_attributes([children-Ptr|Attributes]):-
        !,
        get_attr(Ptr, react_dom, ChildNodes),
        Ptr = ChildNodes,
        crystalize(ChildNodes),
        crystalize_attributes(Attributes).
crystalize_attributes([_-Value|Attributes]):-
        ( attvar(Value)->
            get_attr(Value, react_dom, Value)
        ; otherwise->
            true
        ),
        crystalize_attributes(Attributes).

react_dom:attr_unify_hook(X, X).

get_state({null}, _, {null}):- !.
get_state(Object, Key, Value):-
        ( atom(Key)->
            ( memberchk(Key=Value, Object)->
                true
            ; otherwise->
                Value = {null}
            )
        ; otherwise->
            Key =.. [Atom|Args],
            ( memberchk(Atom=ValueWithLessArgs, Object)->
                glue_args(ValueWithLessArgs, Args, Value)
            ; otherwise->
                Value = {null}
            )
        ).

glue_args(react_handler(Context, Goal), Args, react_handler(Context, NewGoal)):- !,
        ( Goal = Module:ActualGoal->
            ActualGoal =.. [Name|ExistingArgs],
            append(ExistingArgs, Args, NewArgs),
            NewActualGoal =.. [Name|NewArgs],
            NewGoal = Module:NewActualGoal
        ; otherwise->
            Goal =.. [Name|ExistingArgs],
            append(ExistingArgs, Args, NewArgs),
            NewGoal =.. [Name|NewArgs]
        ).

glue_args(Goal, Args, NewGoal):- !,
        ( Goal = Module:ActualGoal->
            ActualGoal =.. [Name|ExistingArgs],
            append(ExistingArgs, Args, NewArgs),
            NewActualGoal =.. [Name|NewArgs],
            NewGoal = Module:NewActualGoal
        ; otherwise->
            Goal =.. [Name|ExistingArgs],
            append(ExistingArgs, Args, NewArgs),
            NewGoal =.. [Name|NewArgs]
        ).

get_store_state(_, []). % FIXME: Implement Flux!

state_to_term(X, X). % FIXME: This should copy X and instantiate all the vars


:-redefine_system_predicate('.'(_,_,_)).
user:'.'(State,Key,Value):-
	( is_list(State)->
	    get_state(State, Key, Value)
	; State == {null}->
            Value = {null}
        ; is_dict(State, proactive)->
            ( get_dict(Key, State, Value)->
                true
            ; Value = {null}
            )
	; '$dicts':'.'(State, Key, Value)
	).

proactive_term_to_state({null}, proactive{}):- !.
proactive_term_to_state({Values}, State):-
        state_pairs(Values, Pairs),
        dict_pairs(State, proactive, Pairs).

state_pairs((A:B, C), [A-B|D]):- !,
        state_pairs(C, D).
state_pairs(A:B, [A-B]):- !.