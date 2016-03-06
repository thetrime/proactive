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
          set_property/3,
          replace_node_data/2,
          init_widget/3,
          destroy_widget/2]).

% ----------------- the real DOM. Implemented in SWI by attributed variables

remove_child(DomNode, Child):-
        DomNode = dom_element(Attributes),
        memberchk(children-ChildrenPtr, Attributes),
        get_attr(ChildrenPtr, react, ChildNodes),
        subtract(ChildNodes, [Child], NewChildNodes),
        move_child_to(Child, {null}),
        put_attr(ChildrenPtr, react, NewChildNodes).

append_child(DomNode, Child):-
        DomNode = dom_element(Attributes),
        memberchk(children-ChildrenPtr, Attributes),
        move_child_to(Child, DomNode),
        get_attr(ChildrenPtr, react, ChildNodes),
        append(ChildNodes, [Child], NewChildNodes),
        put_attr(ChildrenPtr, react, NewChildNodes).

insert_before(DomNode, Child, Sibling):-
        DomNode = dom_element(Attributes),
        memberchk(children-ChildrenPtr, Attributes),
        get_attr(ChildrenPtr, react, ChildNodes),
        move_child_to(Child, DomNode),
        insert_child_ptr(ChildNodes, Child, Sibling, NewChildNodes),
        put_attr(ChildrenPtr, react, NewChildNodes).


replace_child(DomNode, New, Old):-
        DomNode = dom_element(Attributes),
        memberchk(children-ChildrenPtr, Attributes),
        get_attr(ChildrenPtr, react, ChildNodes),
        move_child_to(New, DomNode),
        replace_child_ptr(ChildNodes, New, Old, NewChildNodes),
        put_attr(ChildrenPtr, react, NewChildNodes).

create_element(Document, TagName, DomNode):-
        DomNode = dom_element([tag-TagName,
                               type-node,
                               children-ChildrenPtr,
                               document-Document,
                               properties-PropertiesPtr,
                               parent-ParentPtr]),
        put_attr(ChildrenPtr, react, []),
        put_attr(PropertiesPtr, react, []),
        put_attr(ParentPtr, react, {null}).

set_property(DomNode, Name, Value):-
        DomNode = dom_element(Attributes),
        memberchk(properties-PropertiesPtr, Attributes),
        get_attr(PropertiesPtr, react, Properties),
        change_attributes(Properties, Name, Value, NewProperties),
        put_attr(PropertiesPtr, react, NewProperties).

create_text_node(Document, Data, DomNode):-
        DomNode = dom_element([type-text,
                               children-ChildrenPtr,
                               parent-ParentPtr,
                               document-Document,
                               data-DataPtr]),
        put_attr(ChildrenPtr, react, []),
        put_attr(ParentPtr, react, {null}),
        put_attr(DataPtr, react, Data).

move_child_to(Child, DomNode):-
        Child = dom_element(ChildAttributes),
        memberchk(parent-ParentPtr, ChildAttributes),
        put_attr(ParentPtr, react, DomNode).

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
        get_attr(ChildrenPtr, react, ChildNodes).

parent_node(DomNode, ParentNode):-
        DomNode = dom_element(Attributes),
        memberchk(parent-ParentPtr, Attributes),
        ( get_attr(ParentPtr, react, ParentNode)->
            true
        ; otherwise->
            ParentNode = {null}
        ).

replace_node_data(DomNode, NewData):-
        DomNode = dom_element(Attributes),
        memberchk(data-DataPtr, Attributes),
        put_attr(DataPtr, react, NewData).


destroy_widget(_DomNode, _Widget).

init_widget(_, VNode, DomNode):-
        VNode = element(Tag, Attributes, _),
        Tag:getInitialState(Attributes, State),
        Tag:render(State, Props, VDom),
        diff(VNode, VDom, Patches),
        create_element(Document, div, FakeDom),
        patch(FakeDom, Patches, [document(Document)], DomNode).


node_type(DomNode, Type):-
        DomNode = dom_element(Attributes),
        memberchk(type-Type, Attributes).





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
        diff(InitialTree, Tree1, Patch1),
        diff(Tree1, Tree2, Patch2),
        writeln(patch:Patch1),
        writeln(patch:Patch2),
        patch(InitialRoot, Patch1, [document(Document)], IntermediateRoot),
        patch(IntermediateRoot, Patch2, [document(Document)], FinalRoot),
        crystalize([FinalRoot]),
        writeln(FinalRoot).

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
        get_attr(Ptr, react, ChildNodes),
        Ptr = ChildNodes,
        crystalize(ChildNodes),
        crystalize_attributes(Attributes).
crystalize_attributes([_-Value|Attributes]):-
        ( attvar(Value)->
            get_attr(Value, react, Value)
        ; otherwise->
            true
        ),
        crystalize_attributes(Attributes).

react:attr_unify_hook(X, X).