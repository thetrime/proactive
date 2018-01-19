% http://docs.lib.purdue.edu/cgi/viewcontent.cgi?article=1377&context=cstech seems like a promising approach to the reordering problem
% http://documents.scribd.com/docs/10ro9oowpo1h81pgh1as.pdf may also shed some helpful ideas

% Next plan:
%   All of this mess could be avoided if we just inserted the new nodes *at the right place*. Surely that isnt such a stupid idea?


:-module(vdiff, [create_element_from_vdom/3,
                 vdiff/3,
                 vmutate/5,
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
        %writeq(diff(A, B, [a-A|PatchSet])), nl,
        true.

vmutate(A, B, DomNode, Options, NewNode):-
%        writeln(vmutate(A,B)),
%        ticks(T0),
        vmutate_1(A, B, DomNode, Options, NewNode),
%        ticks(T1),
%        D is T1 - T0,
%        writeln(mutate_time(D)),
        true.

vmutate_1(A, A, DomNode, _Options, DomNode):-
        % Subtrees match: No action required.
        !.
vmutate_1(A, {null}, DomNode, _Options, NewNode):-
        !,
        % Subtree is deleted
        patch_op(remove_patch(A, {null}), DomNode, Options, NewNode),
        % Also, we may have to do more if A is not a widget:
        ( is_widget(A) ->
            true
        ; otherwise->
            destroy_widgets_quickly(A, DomNode, Options)
        ).

vmutate_1(A, B, DomNode, Options, NewNode):-
        B = element(BTag, BProps, _),
        !,
        ( A = element(ATag, AProps, _)->
            ( ATag == BTag,
              ( memberchk(key=Key, AProps),
                memberchk(key=Key, BProps)
              ; \+memberchk(key=_, AProps),
                \+memberchk(key=_, BProps)
              )->
                % diff_props/3 fails if there are no differences
                ( diff_props(AProps, BProps, PropsPatch)->
                    patch_op(props_patch(A, PropsPatch), DomNode, Options, N1)
                ; otherwise->
                    % In this case, only the children differ
                    N1 = DomNode
                ),
                vmutate_children(A, B, N1, Options, NewNode)
            ; otherwise->
                % Nodes are totally different - different tags
                patch_op(node_patch(A, B), DomNode, Options, NewNode),
                destroy_widgets_quickly(A, DomNode, Options)
            )
        ; otherwise->
            % This is a widget (or nothing at all?) becoming a node
            patch_op(node_patch(A, B), DomNode, Options, NewNode),
            destroy_widgets_quickly(A, DomNode, Options)
        ).
vmutate_1(A, B, DomNode, Options, {null}):-
        is_widget(B),
        !,
        ( \+is_widget(A)->
            % This is a node becoming a widget
            destroy_widgets_quickly(A, DomNode, Options)
        ; otherwise->
            % This is a widget changing to a different widget
            true
        ),
        patch_op(widget_patch(A, B), DomNode, Options, _).

destroy_widgets_quickly(A, DomNode, Options):-
        is_widget(A),
        !,
        patch_op(remove_patch(A, {null}), DomNode, Options, _).

destroy_widgets_quickly(A, DomNode, Options):-
        A = element(_, _, Children),
        ( has_widgets(A)->
            child_nodes(DomNode, ChildNodes),
            destroy_widget_children(Children, ChildNodes, Options)
        ; true
        ).
destroy_widget_children([], [], _).
destroy_widget_children([Element|Elements], [DomNode|DomNodes], Options):-
        destroy_widgets_quickly(Element, DomNode, Options),
        destroy_widget_children(Elements, DomNodes, Options).

vmutate_children(A, B, DomNode, Options, NewNode):-
        children_of(A, AChildren),
        children_of(B, BChildren),
        mutate_reorder(AChildren, BChildren, OrderedA, OrderedB, Moves),
        child_nodes(DomNode, DomChildren),
        !,
%        dump(AChildren, BChildren),
%        writeln(becomes),
%        dump(OrderedA, OrderedB),
        %writeln(Moves),
        % The problem is that vmutate_children_1 expects DomChildren and OrderedA to be the same length.
        % But we pad OrderedA to make space for converting {null} -> {inserted node} operations. Hmm.
        \+(\+(vmutate_children_1(OrderedA, OrderedB, DomChildren, DomNode, Options))),
        ( Moves == {no_moves} ->
            NewNode = DomNode
        ; patch_op(order_patch(_Ignored, Moves), DomNode, Options, NewNode)
        ).

%dump(A, B):-
%        keysof(A, AK),
%        keysof(B, BK),
%        writeln(AK -> BK).

keysof([], []):- !.
keysof([{null}|A], [{null}|Keys]):- !,
        keysof(A, Keys).
keysof([element(_, Attributes, _)|A], [Key|Keys]):-
        !,
        ( memberchk(key=Key, Attributes)->
            true
        ; Key = {element_with_no_key}
        ),
        keysof(A, Keys).
keysof([widget(_, _, _)|A], [{widget}|Keys]):-
        !,
        keysof(A, Keys).


children_of(widget(_,_,Children), Children):- !.
children_of(element(_,_,Children), Children):- !.

vmutate_children_1([], [], _, _, _):- !.
vmutate_children_1(A, B, DomNodes, ParentDomNode, Options):-
        ( A = [Left|ASiblings] ->
            ( Left == {null} ->
                % This is a tombstone
                DomSiblings = DomNodes,
                LeftDom = {null}
            ; otherwise->
                DomNodes = [LeftDom|DomSiblings]
            )
        ; otherwise->
            DomSiblings = DomNodes,
            LeftDom = {null},
            ASiblings = A,
            Left = {null}
        ),
        ( B = [Right|BSiblings]->
            true
        ; otherwise->
            BSiblings = B,
            Right = {null}
        ),
        ( Left == {null}, Right \== {null}->
            ( DomSiblings = [Sibling|_]->
                patch_op(insert_at_patch(Sibling, Right), ParentDomNode, Options, _)
            ; otherwise->
                patch_op(insert_patch({null}, Right), ParentDomNode, Options, _)
            )
        ; Left \== {null}->
            vmutate_1(Left, Right, LeftDom, Options, _)
        ; otherwise->
            true
        ),
        vmutate_children_1(ASiblings, BSiblings, DomSiblings, ParentDomNode, Options).



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
        reorder(AChildren, BChildren, OrderedB, Moves),
        ( diff_children_1(AChildren, OrderedB, Index, Index, Patch)
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

reorder(As, Bs, NewBs, Moves):-
        all_keys(Bs, 0, BKeys),
        all_keys(As, 0, AKeys),
        reorder_1(As, Bs, 0, AKeys, BKeys, NewBs, Removes, UnsortedInserts),
        ( UnsortedInserts == []->
            Moves = {no_moves}
        ; otherwise->
            sort(UnsortedInserts, SortedInserts),
            vstrip_sort_keys(SortedInserts, Inserts),
            Moves = moves(inserts(Inserts),
                          removes(Removes))
        ).
reorder_1([], [], _, _, _, [], [], []):- !.
reorder_1([], [B|Bs], Position, AKeys, BKeys, Out, Removes, Inserts):-
        !,
        % This is the case where the are more items in the new list
        % If they appear in the original list as well then we have already added them by now
        % and we can just skip them. Otherwise, we need to add them now
        get_key_or_null(B, BKey),
        ( memberchk(BKey-_-_, AKeys)->
            Out = More
        ; otherwise->
            Out = [B|More]
        ),
        reorder_1([], Bs, Position, AKeys, BKeys, More, Removes, Inserts).

reorder_1([_A|As], [], Position, AKeys, BKeys, [{null}|Children], Removes, Inserts):-
        !,
        % This is the case when there are more items in the original list than the new one and we cannot make up the
        % difference by inserting new items.
        % In this case, just pad the output with {null}.
        reorder_1(As, [], Position, AKeys, BKeys, Children, Removes, Inserts).


reorder_1([A|As], [B|Bs], Position, AKeys, BKeys, [Child|Children], Removes, Inserts):-
        get_key_or_null(A, AKey),
        get_key_or_null(B, BKey),
        ( AKey == BKey ->
            % This is the happy case - the node is already in the right slot
            Child = B,
            MoreRemoves = Removes,
            MoreInserts = Inserts,
            NextAs = As,
            NextBs = Bs,
            PP is Position + 1
        ; AKey \== {null},
          memberchk(AKey-OriginalPosition-Child, BKeys)->
            % This is the case where the node has been moved around
            Removes = [remove(Position, AKey)|MoreRemoves],
            Inserts = [OriginalPosition-insert(AKey, OriginalPosition)|MoreInserts],
            NextAs = As,
            NextBs = [B|Bs],
            PP = Position
        ; AKey \== {null}->
            % This is the case where the node has been deleted. Just put a {null} in to pad
            Child = {null},
            MoreRemoves = Removes,
            MoreInserts = Inserts,
            NextAs = As,
            NextBs = [B|Bs],
            PP is Position + 1
        ; otherwise->
            % This is the case where we have a keyless node in A, and a keyed one in B. If that keyed entry in B
            % appears later on in A, then we must wait for it, and put in a padding spot here. Otherwise,
            % we can just accept both nodes since the {null} -> {no-longer-used-key} will get flagged in a moment
            % by the subtree comparison
            ( memberchk(BKey-_-_, AKeys)->
                % Just pad then
                Child = {null},
                MoreRemoves = Removes,
                MoreInserts = Inserts,
                NextAs = As,
                NextBs = [B|Bs],
                PP is Position + 1
            ; otherwise->
                % Accept the discrepancy and leave subtree comparison to sort it out
                Child = B,
                MoreRemoves = Removes,
                MoreInserts = Inserts,
                NextAs = As,
                NextBs = Bs,
                PP is Position + 1
            )
        ),
        reorder_1(NextAs, NextBs, PP, AKeys, BKeys, Children, MoreRemoves, MoreInserts).

reorder_1([_A|As], [], Position, AKeys, BKeys, [{null}|Children], Removes, Inserts):-
        !,
        % This is the case when there are more items in the original list than the new one and we cannot make up the
        % difference by inserting new items.
        % In this case, just pad the output with {null}.
        reorder_1(As, [], Position, AKeys, BKeys, Children, Removes, Inserts).


reorder_1([A|As], [B|Bs], Position, AKeys, BKeys, [Child|Children], Removes, Inserts):-
        get_key_or_null(A, AKey),
        get_key_or_null(B, BKey),
        ( AKey == BKey ->
            % This is the happy case - the node is already in the right slot
            Child = B,
            MoreRemoves = Removes,
            MoreInserts = Inserts,
            NextAs = As,
            NextBs = Bs,
            PP is Position + 1
        ; AKey \== {null},
          memberchk(AKey-OriginalPosition-Child, BKeys)->
            % This is the case where the node has been moved around
            Removes = [remove(Position, AKey)|MoreRemoves],
            Inserts = [OriginalPosition-insert(AKey, OriginalPosition)|MoreInserts],
            NextAs = As,
            NextBs = [B|Bs],
            PP = Position
        ; AKey \== {null}->
            % This is the case where the node has been deleted. Just put a {null} in to pad
            Child = {null},
            MoreRemoves = Removes,
            MoreInserts = Inserts,
            NextAs = As,
            NextBs = [B|Bs],
            PP is Position + 1
        ; otherwise->
            % This is the case where we have a keyless node in A, and a keyed one in B. If that keyed entry in B
            % appears later on in A, then we must wait for it, and put in a padding spot here. Otherwise,
            % we can just accept both nodes since the {null} -> {no-longer-used-key} will get flagged in a moment
            % by the subtree comparison
            ( memberchk(BKey-_-_, AKeys)->
                % Just pad then
                Child = {null},
                MoreRemoves = Removes,
                MoreInserts = Inserts,
                NextAs = As,
                NextBs = [B|Bs],
                PP is Position + 1
            ; otherwise->
                % Accept the discrepancy and leave subtree comparison to sort it out
                Child = B,
                MoreRemoves = Removes,
                MoreInserts = Inserts,
                NextAs = As,
                NextBs = Bs,
                PP is Position + 1
            )
        ),
        reorder_1(NextAs, NextBs, PP, AKeys, BKeys, Children, MoreRemoves, MoreInserts).

% reorder puts BChildren into a list that matches AChildren as closely as possible
% Any remaining differences are left to the subtree-differencing process that is about to happen
% Once the subtree diffs are done, we can optionally use the moves/2 term here to move the mutated
% children into the right order
mutate_reorder(As, Bs, NewAs, NewBs, Moves):-
        all_keys(Bs, 0, BKeys),
        all_keys(As, 0, AKeys),
        mutate_reorder_1(As, Bs, 0, AKeys, BKeys, [], NewAs, NewBs, Removes, UnsortedInserts),
        ( UnsortedInserts == []->
            Moves = {no_moves}
        ; otherwise->
            %writeln(reorder_1(D)),
            sort(UnsortedInserts, SortedInserts),
            vstrip_sort_keys(SortedInserts, Inserts),
            Moves = moves(inserts(Inserts),
                          removes(Removes))
        ).

vstrip_sort_keys([], []):- !.
vstrip_sort_keys([_-X|Xs], [X|Ys]):-
        vstrip_sort_keys(Xs, Ys).

mutate_reorder_1([], [], _, _, _, _, [], [], [], []):- !.
mutate_reorder_1([], [B|Bs], Position, AKeys, BKeys, Consumed, NewAs, Out, Removes, Inserts):-
        !,
        % This is the case where the are more items in the new list
        % If they appear in the original list as well then we have already added them by now
        % and we can just skip them. Otherwise, we need to add them now
        get_key_or_null(B, BKey),
        ( memberchk(BKey-_-_, AKeys)->
            Out = [{null}|More]
        ; otherwise->
            Out = [B|More]
        ),
        mutate_reorder_1([], Bs, Position, AKeys, BKeys, Consumed, NewAs, More, Removes, Inserts).

mutate_reorder_1([A|As], [], Position, AKeys, BKeys, Consumed, [A|NewAs], Out, Removes, Inserts):-
        !,
        get_key_or_null(A, AKey),
        ( memberchk(AKey-_-_, BKeys)->
            Out = [A|More]
        ; otherwise->
            Out = [{null}|More]
        ),
        % This is the case when there are more items in the original list than the new one and we cannot make up the
        % difference by inserting new items.
        % In this case, just pad the output with {null}.
        mutate_reorder_1(As, [], Position, AKeys, BKeys, Consumed, NewAs, More, Removes, Inserts).

mutate_reorder_1([A|As], [B|Bs], Position, AKeys, BKeys, ConsumedSoFar, NewA, NewB, Removes, Inserts):-
        get_key_or_null(A, AKey),
        get_key_or_null(B, BKey),
        %format(user_error, 'Considering left child ~w, at position ~w. The next right key is ~w~n', [AKey, Position, BKey]),
        ( AKey == BKey ->
            % This is the happy case - the node is already in the right slot
            NewA = [A|MoreA],
            NewB = [B|MoreB],
            MoreRemoves = Removes,
            MoreInserts = Inserts,
            NextAs = As,
            NextBs = Bs,
            PP is Position + 1,
            ConsumedKeys = ConsumedSoFar
        ; AKey \== {null},
          memberchk(AKey-_-_, BKeys)->
            % In this case, the left item has a key and the key IS present in the right list. There could be two possibilities:
            % 1) The item needs to be shuffled in the right list (For example, [a,b,c,d] -> [a,d,b,c]), or
            % 2) An item has been inserted in the right list. (For example: [a,b,c] -> [d,a,b,c])
            ( BKey \== {null},
              memberchk(BKey-CurrentPosition-_, AKeys)->
                % The item at the head of the right list is present in the AKeys. Consider this to be case 1
                ( memberchk(BKey, ConsumedSoFar)->
                    %format(user_error, 'We have already dealt with this discrepancy~n', []),
                    % If we have already moved the key, then just ignore it.
                    NewA = [A|MoreA],
                    NewB = [A|MoreB],
                    PP is Position + 1,
                    MoreRemoves = Removes,
                    MoreInserts = Inserts,
                    NextAs = As,
                    NextBs = Bs,
                    ConsumedKeys = ConsumedSoFar
                ; otherwise->
                    % NB: This is not necessarily optimal. We have two choices here: Move A, or move B.
                    %format(user_error, 'Moving ~w to ~w~n', [BKey, Position]),
                    Removes = [remove(CurrentPosition, BKey)|MoreRemoves],
                    Inserts = [Position-insert(BKey, Position)|MoreInserts],
                    PP is Position + 1,
                    NextAs = [A|As],
                    NextBs = Bs,
                    ConsumedKeys = [AKey|ConsumedSoFar],
                    NewA = MoreA,
                    NewB = MoreB
                )
            ; otherwise->
                % Otherwise, case 2
                NewA = [{null}|MoreA],
                NewB = [B|MoreB],
                PP is Position + 1,
                MoreRemoves = Removes,
                MoreInserts = Inserts,
                NextAs = [A|As],
                NextBs = Bs,
                ConsumedKeys = ConsumedSoFar
            )
        ; AKey \== {null}->
            % This is the case where the node has been deleted. Just put a {null} in to pad
            NewA = [A|MoreA],
            NewB = [{null}|MoreB],
            MoreRemoves = Removes,
            MoreInserts = Inserts,
            NextAs = As,
            NextBs = [B|Bs],
            PP is Position + 1,
            ConsumedKeys = ConsumedSoFar
        ; otherwise->
            % This is the case where we have a keyless node in A, and a keyed one in B. If that keyed entry in B
            % appears later on in A, then we must wait for it, and put in a padding spot here. Otherwise,
            % we can just accept both nodes since the {null} -> {no-longer-used-key} will get flagged in a moment
            % by the subtree comparison
            ( memberchk(BKey-_-_, AKeys)->
                % Just pad then
                NewA = MoreA,
                NewB = [{null}|MoreB],
                MoreRemoves = Removes,
                MoreInserts = Inserts,
                NextAs = As,
                NextBs = [B|Bs],
                PP is Position + 1,
                ConsumedKeys = ConsumedSoFar
            ; otherwise->
                % Accept the discrepancy and leave subtree comparison to sort it out
                NewA = MoreA,
                NewB = [B|MoreB],
                MoreRemoves = Removes,
                MoreInserts = Inserts,
                NextAs = As,
                NextBs = Bs,
                PP is Position + 1,
                ConsumedKeys = ConsumedSoFar
            )
        ),
        mutate_reorder_1(NextAs, NextBs, PP, AKeys, BKeys, ConsumedKeys, MoreA, MoreB, MoreRemoves, MoreInserts).


all_keys([], _, []):- !.
all_keys([Node|Nodes], N, Keys):-
        get_key_or_null(Node, Key),
        ( Key == {null} ->
            MoreKeys = Keys
        ; Keys = [Key-N-Node|MoreKeys]
        ),
        NN is N+1,
        all_keys(Nodes, NN, MoreKeys).


get_key_or_null(element(_, Attributes, _), Key):-
        !,
        ( memberchk(key=Key, Attributes)->
            true
        ; Key = {null}
        ).

get_key_or_null(widget(_, Attributes, _), Key):-
        ( memberchk(key=Key, Attributes)->
            true
        ; Key = {null}
        ).




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
        ( \+ (\+ index_in_range(Indices, RootIndex, RootIndex))->
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
        ( \+ (\+ index_in_range(Indices, R1, NextIndex))->
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

%patch_op(Patch, _, _, _):-
%        writeln(x(Patch)), fail.

patch_op(remove_patch(VNode, _), DomNode, _Options, NewNode):-
        !,
        parent_node(DomNode, ParentNode),
        ( ParentNode \== {null} ->
            remove_child(ParentNode, DomNode)
        ; otherwise->
            true
        ),
        destroy_component(DomNode, VNode),
        NewNode = {null}.


patch_op(insert_patch(_ActualVNode, VNode), ParentNode, Options, ParentNode):-
        render(Options, VNode, NewNode),
        !,
        ( ParentNode \== {null}->
            append_child(ParentNode, NewNode)
        ; otherwise->
            true
        ).

patch_op(insert_at_patch(Sibling, VNode), ParentNode, Options, ParentNode):-
        render(Options, VNode, NewNode),
        !,
        ( ParentNode \== {null}->
            insert_before(ParentNode, NewNode, Sibling)
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
            % We get here if we are going element -> widget
            % In this case, update_widget/4 is not applicable. We have to cut out the old component
            % discard it, and glue in the new component after rendering it
            % It does not really merit a warning
            %vdiff_warning(cannot_update_widget(LeftVNode)),
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
            % widget -> widget
            true
        ; LeftVNode = widget(_, _, _)->
            % widget -> element
            destroy_widget(DomNode, LeftVNode)
        ; otherwise->
            % element -> widget
            true
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

select_nth0(0, [Node|Tail], Node, Tail):- !.
select_nth0(N, [Node|Tail], Child, [Node|Rest]):-
        NN is N-1,
        select_nth0(NN, Tail, Child, Rest).

insert_nth0(0, Tail, Node, [Node|Tail]):- !.
insert_nth0(N, [Node|Tail], Child, [Node|More]):-
        NN is N-1,
        insert_nth0(NN, Tail, Child, More).

insert_end([], Node, [Node]):- !.
insert_end([Node|Tail], Child, [Node|More]):-
        insert_end(Tail, Child, More).

dump_nodes([]).
dump_nodes([Node|Nodes]):-
        dump_node(Node),
        dump_nodes(Nodes).

reorder_removes([], _DomNode, _ChildNodes, T, T):- !.
reorder_removes([remove(From, Key)|Removes], DomNode, ChildNodes, KeyMap, FinalKeyMap):-
        %dump_nodes(ChildNodes),
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
            insert_nth0(Position, ChildNodes, Node, NewChildNodes),
            insert_before(DomNode, Node, Sibling)
        ; otherwise->
            insert_end(ChildNodes, Node, NewChildNodes),
            insert_before(DomNode, Node, {null})
        ),
        reorder_inserts(Inserts, DomNode, NewChildNodes, KeyMap).

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
