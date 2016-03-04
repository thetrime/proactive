diff(A, B, patchset(A, PatchSet)):-
        ( setof(Index-Patches,
                setof(Patch,
                      walk(A, B, 0, Index-Patch),
                      Patches),
                PatchSet)->
            true
        ; otherwise->
            PatchSet = []
        ).

walk(A, A, _, _, _):- !, fail.

walk(A, B, Index, Index-Patch):-
        ( is_thunk(A)
        ; is_thunk(B)
        ),
        !,
        thunks(A, B, Index, Patch).

walk(A, {null}, Index, Index-remove(A, {null})).
walk(A, {null}, Index, Index-Patch):-
        !,
        \+is_widget(A),
        clear_state(A, Index, Patch).

walk(A, B, Index, PatchIndex-Patch):-
        B = element(BTag, BProps, _),
        \+is_widget(B),
        ( A = element(ATag, AProps, _),
          \+is_widget(A)->
            ( ATag == BTag,
              ( memberchk(key=Key, AProps),
                memberchk(key=Key, BProps)
              ; \+memberchk(key=_, AProps),
                \+memberchk(key=_, BProps)
              )->
                ( diff_props(AProps, BProps, PropsPatch),
                  PatchIndex = Index,
                  tch = props(A, PropsPatch)
                ; diff_children(A, B, Index, PatchIndex-Patch)
                )
            ; otherwise->
                % Nodes are totally different
                clear_state(A, Index, PatchIndex-Patch)
            )
        ; otherwise->
            clear_state(A, Index, PatchIndex-Patch)
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

walk(A, B, Index, Index-widget(A, B)):-
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

diff_children(A, B, Index, Patch):-
        A = element(_, _, AChildren),
        B = element(_, _, BChildren),
        reorder(AChildren, BChildren, Ordered, Moves),
        ( diff_children_1(AChildren, Ordered, Index, Patch)
        ; Moves \== {no_moves},
          Patch = Index-order(A, Moves)
        ).


diff_children_1([], [], _, _):- !, fail.
diff_children_1(A, B, Index, Patch):-
        ( A = [Left|ASiblings]->
            true
        ; otherwise->
            ASiblings = A,
            Left = {null}
        ),
        ( B = [Right|BSiblings]->
            true
        ; BSiblings = B,
          Right = {null}
        ),
        ( Left == {null},
          Right \== {null},
          Patch = Index-insert(Right)
        ; II is Index+1,
          ( Left \== {null},
            walk(Left, Right, II, Patch)
          ; get_count(Left, Count),
            III is II + Count,
            diff_children_1(ASiblings, BSiblings, III, Patch)
          )
        ).


get_count(element(_, _, Children), Count):-
        !,
        aggregate_all(r(sum(N)),
                      ( member(Child, Children),
                        get_count(Child, N)
                      ),
                      r(Sum)),
        Count is Sum+1.
get_count(_, 1).

is_widget(element(Tag, _, _)):-
        \+memberchk(Tag, ['Button', 'EditorPane','Field', 'Frame', 'Label', 'List', 'Panel', 'Tab', 'TabbedPane', 'Table', 'TextArea', 'Tree']).


thunks(A, B, Index, Index-thunk({null}, PatchSet)):-
        handle_thunk(A, B, ANodes, BNodes),
        diff(ANodes, BNodes, PatchSet).


handle_thunk(A, B, ANodes, BNodes):-
        ( is_thunk(B)->
            render_thunk(B, A, BNodes)
        ; otherwise->
            BNodes = {null}
        ),
        ( is_thunk(A)->
            render_thunk(A, {null}, ANodes)
        ; otherwise->
            ANodes = {null}
        ).

destroy_widgets(A, Index, Index-remove(A, {null})):-
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

unhook(A, Index, Index-props(A, UndefinedKeys)):-
        A = element(_, Attributes, _),
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



render_thunk(_, _, _):- fail. % FIXME: Stub
is_thunk(_):- fail. % FIXME: Stub
has_widgets(_):- fail. % FIXME: Stub
has_thunks(_):- fail. % FIXME: Stub
has_hooks(_):- fail. % FIXME: Stub
has_descendent_hooks(_):- fail. % FIXME: Stub
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
simulate([], K, BKeys, SimulateIndex, [SimulateItem|SimulateItems], [remove(SimulateItem, SimulateIndex, Key)|Removes], Inserts):-
        !,
        get_key_or_null(SimulateItem, Key),
        simulate([], K, BKeys, SimulateIndex, SimulateItems, Removes, Inserts).

% Remove items
simulate([WantedItem|BChildren], K, BKeys, SimulateIndex, [{null}|Simulations], [remove({null}, SimulateIndex, {null})|Removes], Inserts):-
        !,
        simulate([WantedItem|BChildren], K, BKeys, SimulateIndex, Simulations, Removes, Inserts).

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
                    Removes = [remove(SimulateItem, SimulateIndex, SimulateKey)|MoreRemoves],
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
            KK = K,
            % K does not increment
            % simulateIndex does not increment
            MoreSimulations = [SimulateItem|Simulations],
            SS = SimulateIndex,
            NextB = [WantedItem|BChildren],
            MoreInserts = Inserts,
            Removes = [remove(SimulateItem, SimulateIndex, SimulateKey)|MoreRemoves]
        ),
        simulate(NextB, KK, BKeys, SS, MoreSimulations, MoreRemoves, MoreInserts).


simulate([_|BChildren], K, BKeys, SimulateIndex, [_|Simulations], Removes, Inserts):-
        SS is SimulateIndex + 1,
        KK is K+1,
        simulate(BChildren, KK, BKeys, SS, Simulations, Removes, Inserts).


%get_key_if_exists(A, A).
get_key_if_exists(element(_, A, _), Key):- memberchk(key=Key, A).
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
                Free = [free(Object, Index)|MoreFree]
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
            ( BFree = [BChild|MoreBFree]->
                BFree = [BChild|MoreBFree],
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
            ( BStillFree = [_|MoreBFree]->
                NewChildren = [NewItem|More]
            ; otherwise->
                MoreBFree = BStillFree,
                NewChildren = More
            )
        ),
        reorder_2(BChildren, AKeys, MoreBFree, More).