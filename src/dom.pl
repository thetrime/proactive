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

          vpath/3,
          proactive_event/3,

          proactive/3,
          render_dom/2,
          render_document/2,
          proactive_term_to_state/2,
          get_store_state/2,
	  state_to_term/2,
          get_this/1,
          widget_id/1
         ]).

% The virtual DOM is the same in all implementations - everything is either an element/3 or a widget/3.
% The whole thing is just a normal Prolog term.

% The real DOM is implemented differently depending on the front-end:
%    * In Proactive/Swing it's implemented as a tree of Swing components.
%    * In Proactive/JS it's implemented as a HTML DOM.
%    * In Proactive/PL it's implemented as a tree of dom_element/1 terms. This is what this file implements

% Note that in addition to the DOM, there's also the logical (widget) tree.
%    * In Proactive/Swing, this is a set of instances of the ReactWidget class.
%    * In Proactive/JS, it's implemented by instances of the react_widget class
%    * In Proactive/PL, it's a series of react_widget/6 terms. The arguments to react_widget/6 are:
%        * Module (ie component name)
%        * State (in dict form)
%        * Props (in dict form)
%        * Id (not currently implemented, but in theory used by widget_id/1)
%        * The current VDOM for the widget
%        * The current DOM for the object

% the widgets in the widget tree do not contain any references to any other widgets directly, but are used by vpatch/4 to update
% the DOM in each widget using predicates like update_widget/4.




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
        get_attr(PropertiesPtr, react_dom, OldProperties),
        merge_vdom_properties(OldProperties, NewProperties, Merged),
        put_attr(PropertiesPtr, react_dom, Merged).

merge_vdom_properties([], Merged, Merged):- !.
merge_vdom_properties([Name=OldValue|Old], NewProperties, Merged):-
        ( select(Name=NewValue, NewProperties, MoreIn)->
            ( NewValue == {null} ->
                MoreOut = Merged
            ; Merged = [Name=NewValue|MoreOut]
            )
        ; Merged = [Name=OldValue|MoreOut],
          MoreIn = NewProperties
        ),
        merge_vdom_properties(Old, MoreIn, MoreOut).

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
        % FIXME: Attach state and props here to the DomNode? Note that not all dom_element/1 terms will have a state, only widgets...

update_widget(_Widget, VNode, DomNode, NewNode):-
        VNode = widget(Tag, Attributes, _),
        State = ?, % FIXME: Need to recover this from DomNode, I guess
        Tag:render(State, Attributes, VDom),
	vdiff(VNode, VDom, Patches),
	vpatch(DomNode, Patches, [document(_Document)], NewNode).

node_type(DomNode, Type):-
        DomNode = dom_element(Attributes),
        memberchk(type-Type, Attributes).


get_this(This):-
        peek_context(This).
widget_id(Id):-
        peek_context(This),
        ( get_attr(This, react_dom, Widget)->
            Widget = react_widget(_Module, _State, _Props, Id, _VDOM, _DOM)
        ; existence_error(widget, This)
        ).

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
        writeln(patch:Patch1),
        writeln(patch:Patch2),
	vpatch(InitialRoot, Patch1, [document(Document)], IntermediateRoot),
        vpatch(IntermediateRoot, Patch2, [document(Document)], FinalRoot),
        render_dom(user_error, [FinalRoot]).

render_dom(Stream, Object):-
        crystalize(Object, XML),
        xml_write(Stream, XML, []).

render_document(Stream, dom_element(Attributes)):-
        memberchk(children-ChildPtr, Attributes),
        get_attr(ChildPtr, react_dom, Children),
        render_dom(Stream, Children).


crystalize([], []):- !.
crystalize([Object|DomNodes], [element(Tag, Attributes, Children)|Siblings]):-
        Object = dom_element(DomAttributes),
        ( memberchk(children-ChildPtr, DomAttributes) ->
            get_attr(ChildPtr, react_dom, ChildNodes),
            crystalize(ChildNodes, Children)
        ; otherwise->
            Children = []
        ),
        retrieve_tag(Object, Tag),
        ( memberchk(properties-PropertiesPtr, DomAttributes)->
            get_attr(PropertiesPtr, react_dom, AttributesWithObjects),
            crystalize_attributes(AttributesWithObjects, Attributes)
        ; otherwise->
            Attributes = []
        ),
        crystalize(DomNodes, Siblings).

crystalize_attributes([], []):- !.
crystalize_attributes([Name=Value|In], [Name=Atom|Out]):-
        ( atomic(Value)->
            Atom = Value
        ; term_to_atom(Value, Atom)
        ),
        crystalize_attributes(In, Out).

proactive(Module, Props, Document):-
        current_module(Module),
        ( current_predicate(Module:getInitialState/2)->
            Module:getInitialState(Props, InitialState)
        ; otherwise->
            InitialState = proactive{}
        ),
        % FIXME: Set a pointer to This somehow and remove it after rendering?
        curly_term_to_state(InitialState, I),
        put_attr(This, react_dom, react_widget(Module, I, Props, id_goes_here, [], [])),
        setup_call_cleanup(push_context(This),
                           Module:render(I, Props, VDOM),
                           pop_context),
        create_element_from_vdom([], VDOM, DOM),
        put_attr(This, react_dom, react_widget(Module, I, Props, id_goes_here, VDOM, DOM)),
        Document = dom_element([tag-{document},
                                children-ChildPtr]),
        put_attr(ChildPtr, react_dom, [DOM]),
        DOM = dom_element(Attributes),
        memberchk(parent-ParentPtr, Attributes),
        put_attr(ParentPtr, react_dom, Document).

push_context(This):-
        ( nb_current(proactive_this, List)->
            b_setval(proactive_this, [This|List])
        ; b_setval(proactive_this, [This])
        ).

pop_context:-
        nb_current(proactive_this, [_|List]),
        b_setval(proactive_this, List).

peek_context(This):-
        nb_current(proactive_this, [This|_]).

% To save on nightmares, never allow these unifications to actually take place
react_dom:attr_unify_hook(X, X):-
        var(X), !.

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

proactive_event(Target, HandlerName, Event):-
        Target = dom_element(DomAttributes),
        ( memberchk(properties-PropertiesPtr, DomAttributes),
          get_attr(PropertiesPtr, react_dom, AttributesWithObjects),
          memberchk(HandlerName=Handler, AttributesWithObjects)->
            fire_event(Handler, Event, Target)
        ; format(user_error, 'Warning: Object has no handler for ~w attached', [HandlerName]),
          fail
        ).

fire_event('$this'(Context, Handler), Event, _This):-
        !,
        fire_event(Handler, Event, Context).

fire_event(Handler, Event, This):-
        ( get_attr(This, react_dom, Widget)->
            Widget = react_widget(Module, State, Props, Id, OldVDOM, OldDOM)
        ; existence_error(widget, This)
        ),
        ( atom(Handler)->
            Goal =.. [Handler, Event, State, Props, Result]
        ; otherwise->
            Handler =.. A,
            append(A, [Event, State, Props, Result], B),
            Goal =.. B
        ),
        Module:Goal,
        curly_term_to_state(Result, NewState),
        merge_states(State, NewState, UpdatedState),
        set_state(This, UpdatedState),
        % Now we must re-render. That means, essentially, changing the children. First, we must render the whole component with the new state
        setup_call_cleanup(push_context(This),
                           Module:render(UpdatedState, Props, NewVDOM),
                           pop_context),
        !,
        % vdiff/3 will not work if the term contains attributed variables. This is because, for example:
        %   ?- put_attr(X, react_dom, qqq), bagof(A, member(A, [X,Y]), As).
        %   X = qqq,
        %   As = [qqq, Y].
        % despite the fact that
        %   ?- bagof(A, member(A, [X,Y]), As).
        %   As = [X, Y].
        % In other words, setof/bagof end up unfreezing attributed variables :(
        % We can fix this with a very /very/ complicated dance
        term_variables(OldVDOM-NewVDOM, OriginalVars),
        copy_term(OriginalVars-OldVDOM-NewVDOM, CopiedVars-CopiedOldVDOM-CopiedNewVDOM, AttributeGoals),
        vdiff(CopiedOldVDOM, CopiedNewVDOM, Patch),
        % Then restore the variables
        OriginalVars = CopiedVars,
        maplist(call, AttributeGoals),
        % Then continue on our way
        vpatch(OldDOM, Patch, [document(This)], NewDOM),
        put_attr(This, react_dom, react_widget(Module, State, Props, Id, NewVDOM, NewDOM)).

curly_term_to_state(Term, State):-
        ( Term == {} ->
            State = proactive{}
        ; otherwise->
            Term = {}(List),
            curly_term_to_pairs(List, Pairs),
            dict_pairs(State, proactive, Pairs)
        ).

curly_term_to_pairs(Name:Value, [Pair]):-
        !,
        curly_item_to_pair(Name, Value, Pair).
curly_term_to_pairs((Name:Value, In), [Pair|Out]):-
        !,
        curly_item_to_pair(Name, Value, Pair),
        curly_term_to_pairs(In, Out).

curly_item_to_pair(Name, Value, Pair):-
        ( Value == {} ->
            Pair = Name-proactive{}
        ; functor(Value, {}, 1)->
            curly_term_to_state(Value, N),
            Pair = Name-N
        ; otherwise->
            Pair = Name-Value
        ).

retrieve_tag(dom_element(Attributes), Tag):-
        ( memberchk(tag-Tag, Attributes)->
            true
        ; throw(error(existence_error(tag, Attributes), _))
        ).

retrieve_parent(dom_element(Attributes), Parent):-
        ( memberchk(parent-ParentPtr, Attributes)->
            get_attr(ParentPtr, react_dom, Parent)
        ; throw(error(existence_error(tag, Attributes), _))
        ).


retrieve_state(dom_element(Attributes), State):-
        ( memberchk(state-StatePtr, Attributes)->
            get_attr(StatePtr, react_dom, State)
        ; throw(error(existence_error(state, Attributes), _))
        ).


retrieve_props(dom_element(Attributes), Props):-
        ( memberchk(state-PropsPtr, Attributes)->
            get_attr(PropsPtr, react_dom, Props)
        ; throw(error(existence_error(state, Attributes), _))
        ).

set_state(This, NewState):-
        ( get_attr(This, react_dom, Widget)->
            Widget = react_widget(Module, _State, Props, Id, VDOM, DOM),
            put_attr(This, react_dom, react_widget(Module, NewState, Props, Id, VDOM, DOM))
        ; existence_error(widget, This)
        ).


replace_child_in_vdom(Parent, OldChild, NewChild):-
        Parent = dom_element(Attributes),
        ( memberchk(children-ChildPtr, Attributes)->
            get_attr(ChildPtr, react_dom, OldChildren),
            replace_child_in_vdom_1(OldChildren, OldChild, NewChild, NewChildren),
            put_attr(ChildPtr, react_dom, NewChildren)
        ; throw(error(existence_error(children, Attributes), _))
        ).

replace_child_in_vdom_1([OldChild|Tail], OldChild, NewChild, [NewChild|Tail]):- !.
replace_child_in_vdom_1([Child|OldChildren], OldChild, NewChild, [Child|NewChildren]):-
        replace_child_in_vdom_1(OldChildren, OldChild, NewChild, NewChildren).

merge_states(OldState, NewState, Merged):-
        dict_pairs(OldState, proactive, OldPairs),
        dict_pairs(NewState, proactive, NewPairs),
        merge_pairs(OldPairs, NewPairs, MergedPairs),
        dict_pairs(Merged, proactive, MergedPairs).

merge_pairs([], N, N):- !.
merge_pairs([Key-OldValue|OldState], NewState, Merged):-
        ( select(Key-NewValue, NewState, N1)->
            ( is_dict(OldValue, proactive),
              is_dict(NewValue, proactive)->
                merge_states(OldValue, NewValue, M),
                Merged = [Key-M|More]
            ; NewValue == {null} ->
                Merged = More
            ; otherwise->
                Merged = [Key-NewValue|More]
            )
        ; otherwise->
            N1 = NewState,
            Merged = [Key-OldValue|More]
        ),
        merge_pairs(OldState, N1, More).

vpath(_DOM, _Spec, _Content):-
        throw(not_implemented).



