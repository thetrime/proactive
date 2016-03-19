Flux
====

As in React, your data in Proactive always flows _down_ the tree, and events flow _up_ the tree.

There are two ways to get your events up the tree. You can either attach handlers with references to nodes higher up (using the 'this' notation), or you can use Flux. In React, Flux is a pattern, rather than a framework. In Proactive, because of the strong encapsulation and loose coupling between components, implementing Flux dispatcher requires help from the framework itself. This means that there is slightly less flexibility in how you use the system. However, the same kinds of patterns are possible as with Flux in React.

For example, in Proactive, a Flux event is identified by both a key and a value, whereas in React a Flux event is an arbitrary object.

To use Flux, follow these steps:

### Create a Flux Store
In Proactive, a flux store is a module. Note that this means that, as in React, Flux stores are _singletons_. You can only have one store of each name in your system.

Your module must define a predicate called `handle_event(+Key, +Data, +State, -NewState)`. This will be called whenever events are raised. It is not required that Key is bound in the head.

You _may_ also define a `getInitialStoreState(-InitialState)`. If this is missing, then the initial state is assumed to be [].

### Have your components require the Flux store
You must declare clauses of requires(+StoreName) in your component module if you want to interact with store StoreName.

### Have your components listen to Flux store change notifications
To request that you are notified when a store changes its state, your component should define clauses of `listen_for(+StoreName, +Goal)`. When the store StoreName processes an event successfully, changing its state, Goal will be called as `call(Goal, +StoreState, +State, +Props, -StoreState)`. You can use this to update your components state given the Flux store state.

### Raise some events
In your event callbacks, you can use `raise_event(+Key, +Data)` to trigger an event. Note that it is NOT allowed to raise an event while processing an existing one (as in React's Flux).

### Wait for other stores
Your `handle_event/5` clause is allowed to use `wait_for(+ListOfStoreNames)` if it wants to wait for other stores to finish processing first. This is because of the next point:

### Stores can coordinate with each other
You can use `get_store_state(+StoreName, -State)` to retrieve (but not mutate) the state of another store. This allows dependent interactions like

```
city_store:handle_event('ChangeCountry', _, CurrentState, NewState):-
   wait_for([country_store]),
   get_store_state(country_store, CountryState),
   get_state(CountryState, current_country, Country),
   get_state(CurrentState, current_city, CurrentCity),
   (  check_city(Country, CurrentCity)
   -> NewState = []
   ;  NewState = [current_city='Please select a city']
   ).

country_store:handle_event('ChangeCountry', Event, _, [current_country=Country]):-
```

This is analogous to the Flux canonical example