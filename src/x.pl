go:-
        vdiff(element('Panel',[],[element('Panel',[],[list([widget('LabelledField',[value=a],[]),
                                                            widget('LabelledField',[value=b],[])])]),
                                  widget('LabelledField',[value=one],[])]),
              element('Panel',[],[element('Panel',[],[list([widget('LabelledField',[value=a],[])])]),
                                  widget('LabelledField',[value=two],[])]), [a-_|Patch]),
        forall(member(P, Patch),
               ( writeq(P), nl)).

go2:-
        vdiff(element('Panel',[],[element('Panel',[],[widget('LabelledField',[value=a],[]),
                                                      widget('LabelledField',[value=b],[])]),
                                  widget('LabelledField',[value=one],[])]),
              element('Panel',[],[element('Panel',[],[widget('LabelledField',[value=a],[])]),
                                  widget('LabelledField',[value=two],[])]), [a-_|Patch]),
        forall(member(P, Patch),
               ( writeq(P), nl)).

expand_children(A, B):-
        expand_children([A], [B], []).

expand_children([], T, T):- !.
expand_children([element(A, B, C)|D], [element(A,B,CC)|DD], T):- !,
        expand_children(C, CC, []),
        expand_children(D, DD, T).
expand_children([widget(A, B, C)|D], [widget(A,B,CC)|DD], T):- !,
        expand_children(C, CC, []),
        expand_children(D, DD, T).
expand_children([list([])|D], AA, T):- !,
        expand_children(D, AA, T).
expand_children([list([A|As])|D], AA, T):- !,
        expand_children([A], AA, T1),
        expand_children(As, T1, T2),
        expand_children(D, T2, T).
expand_children([[[]]|D], AA, T):- !,
        expand_children(D, AA, T).
expand_children([[A|As]|D], AA, T):- !,
        expand_children([A], AA, T1),
        expand_children(As, T1, T2),
        expand_children(D, T2, T).
expand_children(Other, _, _):-
        writeq(failed(Other)), nl, !, fail.


go3:-
        expand_children(element('Panel',[layout=horizontal,'align-children'=end,fill=horizontal,'justify-content'='space-between'],[element('Label',[label='Bank Account(s)',fill=none,for='$widget1'],[]),element('Field',[fill=horizontal,value=a,maxWidth='30em',onChange='$this'(blob,onChange),type=text,onBlur='$this'(blob,onBlur),verifyValue='$this'(blob,verifyValue),allowedValues={null},onContextMenu={null},renderContextMenu={null},align=left,disabled=true],[list([])])]), _).