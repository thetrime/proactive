:-module('SomeStore', []).

handle_event(Key, Data, _State, [event_triggered=true]):-
        writeln(got_event(Key, Data)).
