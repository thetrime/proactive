:-module(vdiff, [create_element_from_vdom/3,
		 vdiff/3,
		 vpatch/4]).

vdiff(A, B, [a-A|PatchSet]):-
        ( setof(Index-Patches,
                bagof(Patch,
                      walk(A, B, 0, Index-Patch),
                      Patches),
                PatchSet)->
            true
        ; otherwise->
            PatchSet = []
        ),
        %writeln(diff(A, B, [a-A|PatchSet])),
        true.

walk(A, A, _, _):- !, fail.

walk(A, B, Index, Index-Patch):-
        ( is_thunk(A)
        ; is_thunk(B)
        ),
        !,
        thunks(A, B, Index, Patch).

walk(A, {null}, Index, Index-remove_patch(A, {null})).
walk(A, {null}, Index, Index-Patch):-
        !,
        \+is_widget(A),
        clear_state(A, Index, Patch).

walk(A, B, Index, PatchIndex-Patch):-
        B = element(BTag, BProps, _),
        ( A = element(ATag, AProps, _)->
            ( ATag == BTag,
              ( memberchk(key=Key, AProps),
                memberchk(key=Key, BProps)
              ; \+memberchk(key=_, AProps),
                \+memberchk(key=_, BProps)
              )->
                ( diff_props(AProps, BProps, PropsPatch),
                  PatchIndex = Index,
                  Patch = props_patch(A, PropsPatch)
                ; diff_children(A, B, Index, PatchIndex-Patch)
                )
            ; otherwise->
                % Nodes are totally different
                ( PatchIndex = Index,
                  Patch = node_patch(A, B)
                ; clear_state(A, Index, PatchIndex-Patch)
                )
            )
        ; otherwise->
            ( PatchIndex = Index,
              Patch = node_patch(A, B)
            ; clear_state(A, Index, PatchIndex-Patch)
            )
        ).

walk(A, B, Index, Index-text(A,B)):-
        atom(B).
walk(A, B, Index, Patch):-
        % Text must always be different or we wouldnt have gotten to this point, surely?
        atom(B),
        !,
        clear_state(A, Index, Patch).

walk(A, B, Index, Patch):-
        is_widget(B),
        \+is_widget(A),
        clear_state(A, Index, Patch).

walk(A, B, Index, Index-widget_patch(A, B)):-
        is_widget(B).


clear_state(A, Index, Patch):-
        unhook(A, Index, Patch)
        ;
        destroy_widgets(A, Index, Patch).


diff_props(A, B, Diff):-
        findall(Key=NewValue,
                ( ( member(Key=Value, A),
                    memberchk(Key=NewValue, B),
                    Value \== NewValue
                  )
                ; ( member(Key=_DeletedValue, A),
                    \+memberchk(Key=_, B),
                    NewValue = {null}
                  )
                ; ( member(Key=NewValue, B),
                    \+memberchk(Key=_, A)
                  )
                ),
                Diff),
        Diff \== [].

expand_children(widget(_, _, Children), AllChildren):-
        !,
        findall(ChildElement,
                child_element(Children, ChildElement),
                AllChildren).

expand_children(element(_, _, Children), AllChildren):-
        findall(ChildElement,
                child_element(Children, ChildElement),
                AllChildren).

child_element(Children, Child):-
        member(Child, Children).


diff_children(A, B, Index, Patch):-
        expand_children(A, AChildren),
        expand_children(B, BChildren),
        reorder(AChildren, BChildren, Ordered, Moves),
        ( diff_children_1(AChildren, Ordered, Index, Index, Patch)
        ; Moves \== {no_moves},
          Patch = Index-order_patch(A, Moves)
        ).


diff_children_1([], [], _, _, _):- !, fail.
diff_children_1(A, B, Index, ParentIndex, Patch):-
        ( A = [Left|ASiblings]->
            true
        ; otherwise->
            ASiblings = A,
            Left = {null}
        ),
        ( B = [Right|BSiblings]->
            true
        ; otherwise->
            BSiblings = B,
            Right = {null}
        ),
        ( Left == {null},
          Right \== {null},
          Patch = ParentIndex-insert_patch({null}, Right)
        ; II is Index+1,
          ( Left \== {null},
            walk(Left, Right, II, Patch)
          ; get_count(Left, Count),
            III is II + Count,
            diff_children_1(ASiblings, BSiblings, III, ParentIndex, Patch)
          )
        ).

get_count(widget(_, _, Children), Count):-
        !,
        get_count_1(Children, Count).
get_count(element(_, _, Children), Count):-
        !,
        get_count_1(Children, Count).
get_count(_, 0).

get_count_1([], 0):- !.
get_count_1([Child|Children], Count):-
        get_count(Child, C),
        get_count_1(Children, CC),
        Count is CC + C + 1.

thunks(A, B, Index, Index-thunk({null}, PatchSet)):-
        handle_thunk(A, B, ANodes, BNodes),
        vdiff(ANodes, BNodes, PatchSet).


handle_thunk(A, B, ANodes, BNodes):-
        ( is_thunk(B)->
            render_thunk(B, A, BNodes)
        ; otherwise->
            BNodes = B
        ),
        ( is_thunk(A)->
            render_thunk(A, {null}, ANodes)
        ; otherwise->
            ANodes = A
        ).

destroy_widgets(A, Index, Index-remove_patch(A, {null})):-
        is_widget(A), !.
destroy_widgets(A, Index, Patch):-
        A = element(_, _, Children),
        ( has_widgets(A) ; has_thunks(A) ),
        !,
        destroy_widgets_1(Children, Index, Patch).
destroy_widgets(A, Index, Patch):-
        is_thunk(A), !,
        thunks(A, {null}, Index, Patch).

destroy_widgets_1([Child|Children], Index, Patch):-
        II is Index+1,
        ( destroy_widgets(Child, II, Patch)
        ; get_count(Child, Count),
          III is II + Count,
          destroy_widgets_1(Children, III, Patch)
        ).

unhook(A, Index, Index-props_patch(A, UndefinedKeys)):-
        ( A = element(_, Attributes, _)->
            true
        ; A = widget(_, Attributes, _)->
            true
        ),
        has_hooks(A),
        findall(Key={null},
                member(Key=_, Attributes),
                UndefinedKeys).
unhook(A, Index, Patch):-
        A = element(_, _, Children),
        ( has_descendent_hooks(A) ; has_thunks(A) ),
        unhook_1(Children, Index, Patch).

unhook(A, Index, Patch):-
        is_thunk(A),
        !,
        thunks(A, {null}, Index, Patch).

unhook_1([Child|Children], Index, Patch):-
        II is Index+1,
        ( unhook(Child, II, Patch)
        ; get_count(Child, Count),
          III is II + Count,
          unhook_1(Children, III, Patch)
        ).

% reorder puts BChildren into a list that matches AChildren as closely as possible
% inserting nulls or removing items to make them the same length
reorder(AChildren, BChildren, Ordered, Moves):-
        key_index(BChildren, AChildren, 0, BKeys, BFree),
        key_index(AChildren, BChildren, 0, AKeys, AFree),
        ( length(BChildren, Length),
          length(BFree, Length)->
            Ordered = BChildren,
            Moves = {no_moves}
        ; length(AChildren, Length),
          length(AFree, Length)->
            Ordered = BChildren,
            Moves = {no_moves}
        ; otherwise->
            % FIXME: I think reorder_1 is missing at least one level of indirection...
            % FIXME: Return deletedItems from reorder_1 and check it against removes.length
            reorder_1(AChildren, BKeys, BFree, BStillFree, Ordered, Tail),
            reorder_2(BChildren, AKeys, BStillFree, Tail),
            simulate(BChildren, 0, BKeys, 0, Ordered, Removes, Inserts),
            ( Inserts == []->
                Moves = {no_moves}
            ; otherwise->
                Moves = moves(inserts(Inserts),
                              removes(Removes))
            )
        ).

simulate([], _, _, _, [], [], []):- !.

% remove all the remaining nodes from simulate
simulate([], K, BKeys, SimulateIndex, [SimulateItem|SimulateItems], [remove(SimulateIndex, Key)|Removes], Inserts):-
        !,
        get_key_or_null(SimulateItem, Key),
        simulate([], K, BKeys, SimulateIndex, SimulateItems, Removes, Inserts).

% Remove items
simulate([WantedItem|BChildren], K, BKeys, SimulateIndex, [{null}|Simulations], [remove(SimulateIndex, {null})|Removes], Inserts):-
        !,
        simulate([WantedItem|BChildren], K, BKeys, SimulateIndex, Simulations, Removes, Inserts).

simulate([WantedItem|BChildren], K, BKeys, SimulateIndex, [], Removes, Inserts):-
        !,
        % This is the case where simulateItems are finished but WantedItems are not (ie when an item is added)
        get_key_or_null(WantedItem, WantedKey),
        ( WantedKey \== {null} ->
            Inserts = [insert(WantedKey, K)|MoreInserts],
            NextB = BChildren,
            KK is K+1
        ; otherwise->
            NextB = [WantedItem|BChildren],
            KK = K,
            MoreInserts = Inserts
        ),
        simulate(NextB, KK, BKeys, SimulateIndex, [], Removes, MoreInserts).

simulate([WantedItem|BChildren], K, BKeys, SimulateIndex, [SimulateItem|Simulations], Removes, Inserts):-
        ( SimulateItem == {null}
        ; get_key_or_null(SimulateItem, SimulateKey),
          get_key_or_null(WantedItem, WantedKey),
          WantedKey \== SimulateKey
        ),
        !,
        ( WantedKey \== {null}->
            ( SimulateItem \== {null}, SimulateKey \== {null} ->
                ( memberchk(SimulateKey=key(_, Index), BKeys),
                  Index =\= K+1->
                    Removes = [remove(SimulateIndex, SimulateKey)|MoreRemoves],
                    ( Simulations = [NextSimulateItem|MoreSimulations]->
                        true
                    ; otherwise->
                        NextSimulateItem = {null},
                        MoreSimulations = []
                    ),
                    ( ( NextSimulateItem == {null} ; \+get_key_or_null(NextSimulateItem, WantedKey))->
                        Inserts = [insert(WantedKey, K)|MoreInserts],
                        SS = SimulateIndex
                    ; otherwise->
                        Inserts = MoreInserts,
                        SS is SimulateIndex + 1
                    )
                ; otherwise->
                    MoreSimulations = [SimulateItem|Simulations],
                    Removes = MoreRemoves,
                    SS = SimulateIndex,
                    Inserts = [insert(WantedKey, K)|MoreInserts]
                )
            ; otherwise->
                MoreSimulations = [SimulateItem|Simulations],
                Removes = MoreRemoves,
                SS = SimulateIndex,
                Inserts = [insert(WantedKey, K)|MoreInserts]
            ),
            KK is K+1,
            NextB = BChildren
        ; otherwise->
            % A key in simulate has no matching wanted key, remove it
            KK = K,
            % K does not increment
            % simulateIndex does not increment but we DO remove the item from simulations
            MoreSimulations = Simulations,
            SS = SimulateIndex,
            NextB = [WantedItem|BChildren],
            MoreInserts = Inserts,
            Removes = [remove(SimulateIndex, SimulateKey)|MoreRemoves]
        ),
        simulate(NextB, KK, BKeys, SS, MoreSimulations, MoreRemoves, MoreInserts).


simulate([_|BChildren], K, BKeys, SimulateIndex, [_|Simulations], Removes, Inserts):-
        SS is SimulateIndex + 1,
        KK is K+1,
        simulate(BChildren, KK, BKeys, SS, Simulations, Removes, Inserts).


%get_key_if_exists(A, A).
get_key_if_exists(element(_, A, _), Key):- !, memberchk(key=Key, A).
get_key_if_exists(widget(_, A, _), Key):- !, memberchk(key=Key, A).
get_key_or_null(A, Key):-
        ( get_key_if_exists(A, Key)->
            true
        ; Key = {null}
        ).

key_index([], _Objects, _, [], []):- !.
key_index([Child|Children], Objects, Index, Keys, Free):-
        ( Objects = [Object|MoreObjects]->
            true
        ; otherwise->
            Object = {null},
            MoreObjects = []
        ),
        ( get_key_if_exists(Child, Key)->
            Keys = [Key=key(Child, Index)|MoreKeys],
            MoreFree = Free
        ; otherwise->
            Keys = MoreKeys,
            ( Object == {null}->
                Free = MoreFree
            ; otherwise->
                Free = [free(Child, Index)|MoreFree]
            )
        ),
        II is Index + 1,
        key_index(Children, MoreObjects, II, MoreKeys, MoreFree).



reorder_1([], _BKeys, BFree, BFree, T, T):- !.
reorder_1([AItem|Children], BKeys, BFree, BStillFree, NewChildren, Tail):-
        ( get_key_if_exists(AItem, Key)->
            ( memberchk(Key=key(BChild, _), BKeys)->
                NewChildren = [BChild|More]
            ; otherwise->
                NewChildren = [{null}|More]
            ),
            MoreBFree = BFree
        ; otherwise->
            ( BFree = [free(BChild, _)|MoreBFree]->
                NewChildren = [BChild|More]
            ; otherwise->
                BFree = MoreBFree,
                NewChildren = [{null}|More]
            )
        ),
        reorder_1(Children, BKeys, MoreBFree, BStillFree, More, Tail).

% Iterate through b and append any new keys
reorder_2([], _AKeys, _BStillFree, []):- !.
reorder_2([NewItem|BChildren], AKeys, BStillFree, NewChildren):-
        ( get_key_if_exists(NewItem, Key)->
            ( \+memberchk(Key=_, AKeys)->
                NewChildren = [NewItem|More]
            ; otherwise->
                NewChildren = More
            ),
            MoreBFree = BStillFree
        ; otherwise->
            ( BStillFree = [free(BItem, _)|MoreBFree]->
                NewChildren = [BItem|More]
            ; otherwise->
                MoreBFree = BStillFree,
                NewChildren = More
            )
        ),
        reorder_2(BChildren, AKeys, MoreBFree, More).


render_thunk(_, _, _):- fail. % FIXME: Stub
is_thunk(_):- fail. % FIXME: Stub
has_widgets(_):- fail. % FIXME: Stub
has_thunks(_):- fail. % FIXME: Stub
has_hooks(_):- fail. % FIXME: Stub
has_descendent_hooks(_):- fail. % FIXME: Stub

is_widget(widget(_, _, _)).

%----------------------------------------------------------------------------
vpatch(RootNode, Patches, Options, NewRoot):-
        patch_recursive(RootNode, Patches, Options, NewRoot).

% patch_indices/2 used to be a setof(Index, P^(member(Index-P, Patches), Index \== a), Indices)
% but when I came to write Proactive/PL I found that this caused a real problem if Patches
% included any attributed variables. This avoids copying the term and is therefore a lot nicer
% and probably uses less memory as a bonus
patch_indices([], []):- !.
patch_indices([a-_|Patch], Indices):- !,  patch_indices(Patch, Indices).
patch_indices([Index-_|Patch], [Index|Indices]):- patch_indices(Patch, Indices).

patch_recursive(RootNode, PatchSet, Options, NewRoot):-
        ( patch_indices(PatchSet, Indices),
          Indices \== []->
            memberchk(a-A, PatchSet),
            dom_index(RootNode, A, Indices, [], Index),
            %writeln(dom_index(RootNode, A, Indices, [], Index)),
            %owner_document(RootNode, OwnerDocument)
            patch_recursive(Indices, RootNode, Index, PatchSet, Options, NewRoot)
        ; otherwise->
            NewRoot = RootNode
        ).

patch_recursive([], RootNode, _Index, _PatchSet, _Options, RootNode):- !.
patch_recursive([NodeIndex|Indices], RootNode, Index, PatchSet, Options, NewRoot):-
        ( memberchk(NodeIndex-DomNode, Index)->
            true
        ; otherwise->
            DomNode = {null}
        ),
        memberchk(NodeIndex-Patches, PatchSet),
        apply_patch(RootNode, DomNode, Patches, Options, R1),
        patch_recursive(Indices, R1, Index, PatchSet, Options, NewRoot).


apply_patch(RootNode, {null}, _Patches, _Options, RootNode):- !.
apply_patch(RootNode, _DomNode, [], _Options, RootNode):- !.
apply_patch(RootNode, DomNode, [Patch|Patches], Options, NewRoot):-
        ( patch_op(Patch, DomNode, Options, NewNode)->
            true
	; vdiff_warning(failed_to_apply_patch(Patch)), fail
        ),
        ( DomNode == RootNode->
            R1 = NewNode
        ; otherwise->
            R1 = RootNode
        ),
        apply_patch(R1, DomNode, Patches, Options, NewRoot).

dom_index(_RootNode, _Tree, [], _Nodes, []):- !.
dom_index(RootNode, Tree, Indices, Nodes, Index):-
        % Indices is already sorted
        recurse(RootNode, Tree, Indices, Nodes, 0, Index).

recurse({null}, _Tree, _Indices, Nodes, _RootIndex, Nodes):- !.
recurse(RootNode, Tree, Indices, Nodes, RootIndex, Index):-
        ( \+ \+ index_in_range(Indices, RootIndex, RootIndex)->
            N1 = [RootIndex-RootNode|Nodes]
        ; otherwise->
            N1 = Nodes
        ),
        ( Tree = element(_, _, VChildren)->
            child_nodes(RootNode, ChildNodes),
            recurse_1(ChildNodes, VChildren, RootIndex, Indices, N1, Index)
        ; Tree = widget(_, _, VChildren)->
            child_nodes(RootNode, ChildNodes),
            recurse_1(ChildNodes, VChildren, RootIndex, Indices, N1, Index)
        ; otherwise->
            Index = N1
        ).

recurse_1([], _VChildren, _RootIndex, _, N, N):- !.
recurse_1([ChildNode|Tree], VChildren, RootIndex, Indices, Nodes, FinalNodes):-
        R1 is RootIndex+1,
        ( VChildren = [VChild|VSiblings]->
            get_count(VChild, Count)
        ; otherwise->
            VChild = {null},
            VSiblings = [],
            Count = 0
        ),
        NextIndex is R1 + Count,
        ( \+ \+ index_in_range(Indices, R1, NextIndex)->
            recurse(ChildNode, VChild, Indices, Nodes, R1, N1)
        ; otherwise->
            N1 = Nodes
        ),
        recurse_1(Tree, VSiblings, NextIndex, Indices, N1, FinalNodes).

index_in_range([], _, _):- !, fail.
index_in_range(Indices, Left, Right):-
        MinIndex = 0,
        length(Indices, L),
        MaxIndex is L - 1,
        index_in_range_1(MinIndex, MaxIndex, Indices, Left, Right).

index_in_range_1(MinIndex, MaxIndex, Indices, Left, Right):-
        MinIndex =< MaxIndex,
        CurrentIndex is (MaxIndex + MinIndex)//2,
        nth0(CurrentIndex, Indices, CurrentItem),
        ( MinIndex == MaxIndex->
            CurrentItem >= Left,
            CurrentItem =< Right
        ; CurrentItem < Left->
            NewMin is CurrentIndex + 1,
            index_in_range_1(NewMin, MaxIndex, Indices, Left, Right)
        ; CurrentItem > Right ->
            NewMax is CurrentIndex - 1,
            index_in_range_1(MinIndex, NewMax, Indices, Left, Right)
        ; otherwise->
            true
        ).

patch_op(remove_patch(VNode, _), DomNode, _Options, NewNode):-
        !,
        parent_node(DomNode, ParentNode),
        ( ParentNode \== {null} ->
            remove_child(ParentNode, DomNode)
        ; otherwise->
            true
        ),
        destroy_widget(DomNode, VNode),
        NewNode = {null}.

patch_op(insert_patch(_ActualVNode, VNode), ParentNode, Options, ParentNode):-
        render(Options, VNode, NewNode),
        ( ParentNode \== {null}->
            append_child(ParentNode, NewNode)
        ; otherwise->
            true
        ).

patch_op(text_patch(_LeftVNode, VText), DomNode, Options, NewNode):-
        ( node_type(DomNode, text)->
            replace_node_data(DomNode, VText),
            NewNode = DomNode
        ; otherwise->
            parent_node(DomNode, ParentNode),
            render(Options, VText, NewNode),
            ( ParentNode \== {null},
              NewNode \== DomNode->
                replace_child(ParentNode, NewNode, DomNode)
            ; otherwise->
                true
            )
        ).

patch_op(widget_patch(LeftVNode, Widget), DomNode, Options, NewNode):-
        ( LeftVNode = widget(WidgetName, _, _),
          Widget = widget(WidgetName, _, _)->
            Updating = true,
            update_widget(Widget, LeftVNode, DomNode, NewNode)
        ; otherwise->
	    vdiff_warning(cannot_update_widget(LeftVNode)),
            Updating = false,
            render(Options, Widget, NewNode)
        ),
        parent_node(DomNode, ParentNode),
        ( ParentNode \== {null},
          NewNode \== DomNode->
            replace_child(ParentNode, NewNode, DomNode)
        ; otherwise->
            true
        ),
        ( Updating == true ->
            true
        ; otherwise->
            destroy_widget(DomNode, LeftVNode)
        ).


patch_op(node_patch(_LeftVNode, VNode), DomNode, Options, NewNode):-
        parent_node(DomNode, ParentNode),
        render(Options, VNode, NewNode),
        ( ParentNode \== {null}, NewNode \== DomNode->
            replace_child(ParentNode, NewNode, DomNode)
        ; otherwise->
            true
        ).

patch_op(order_patch(_VNode, Moves), DomNode, _Options, DomNode):-
        child_nodes(DomNode, ChildNodes),
        Moves = moves(inserts(Inserts),
                      removes(Removes)),
        reorder_removes(Removes, DomNode, ChildNodes, [], KeyMap),
        child_nodes(DomNode, ChildNodesAfterRemoves),
        reorder_inserts(Inserts, DomNode, ChildNodesAfterRemoves, KeyMap).

patch_op(props_patch(VNode, Patch), DomNode, _Options, DomNode):-
        VNode = element(_, Properties, _),
        apply_properties(DomNode, Patch, Properties).

%patch_op(thunk_patch(VNode, Patch), DomNode, Options, ?) % FIXME: Implement

reorder_removes([], _DomNode, _ChildNodes, T, T):- !.
reorder_removes([remove(From, Key)|Removes], DomNode, ChildNodes, KeyMap, FinalKeyMap):-
        nth0(From, ChildNodes, Node),
        ( Key \== {null}->
            NewKeyMap = [Key-Node|KeyMap]
        ; otherwise->
            NewKeyMap = KeyMap
        ),
        remove_child(DomNode, Node),
        reorder_removes(Removes, DomNode, ChildNodes, NewKeyMap, FinalKeyMap).

reorder_inserts([], _DomNode, _ChildNodes, _KeyMap):- !.
reorder_inserts([insert(Key, Position)|Inserts], DomNode, ChildNodes, KeyMap):-
        memberchk(Key-Node, KeyMap),
        ( nth0(Position, ChildNodes, Sibling)->
            insert_before(DomNode, Node, Sibling)
        ; otherwise->
            insert_before(DomNode, Node, {null})
        ),
        reorder_inserts(Inserts, DomNode, ChildNodes, KeyMap).

render(Options, VNodeIn, DomNode):-
        handle_thunk(VNodeIn, {null}, VNode, _),
        (memberchk(document(Document), Options)->
            true
        ; otherwise->
            Document = {root_document}
        ),
        ( VNode = widget(Tag, Attributes, Children)->
            init_widget(Document, widget(Tag, [children=Children|Attributes], []), DomNode)
        ; atom(VNode)->  % Text node
	    vdiff_warning(text_node(VNode)),
            create_text_node(Document, VNode, DomNode)
        ; VNode = element(Tag, Properties, Children)->
            create_element(Document, Tag, DomNode),
            %render_children(Children, Options, DomNode),
            apply_properties(DomNode, Properties, []),
            render_children(Children, Options, DomNode)
        ).

render_children([], _, _):- !.
render_children([Child|Children], Options, DomNode):-
        render(Options, Child, ChildDomNode),
        ( ChildDomNode \== {null}->
            append_child(DomNode, ChildDomNode)
        ; otherwise->
            true
        ),
        render_children(Children, Options, DomNode).

apply_properties(Node, Attributes, _Previous):-
	set_vdom_properties(Node, Attributes).

/*
apply_properties(_Node, [], _Previous):- !.
apply_properties(Node, [PropName=PropValue|Properties], Previous):-
        ( PropValue == {null} ->
            remove_property(Node, PropName, PropValue, Previous)
        ; is_hook(PropValue)->
            remove_property(Node, PropName, PropValue, Previous),
            ( memberchk(PropName=PreviousValue, Previous)->
                true
            ; otherwise->
                PreviousValue = {null}
            ),
            hook(PropValue, PropName, PreviousValue)
        ; otherwise->
            set_property(Node, PropName, PropValue)
        ),
        apply_properties(Node, Properties, Previous).

remove_property(_Node, _PropName, _PropValue, []):- !.
remove_property(Node, PropName, PropValue, Previous):-
        ( memberchk(PropName=PreviousValue, Previous)->
            true
        ; otherwise->
            PreviousValue = {null}
        ),
        ( \+is_hook(PreviousValue)->
            set_property(Node, PropName, {null})
        ; otherwise->
            unhook(PreviousValue, Node, PropName, PropValue)
        ).
*/

unhook(_, _, _, _):- fail. % FIXME: Stub
is_hook(_):- fail. % FIXME: Stub
hook(_, _, _):- fail. % FIXME: Stub




create_element_from_vdom(Options, VDom, Element):-
        render(Options, VDom, Element).
