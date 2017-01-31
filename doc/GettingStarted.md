Getting Started
===============

Quick demo
----------
If you want to just get stuck in, follow these steps:
   1. `make`
   1. `make run-server` (this will start a server on port 10080 and consult demo/App.pl)
   1. In another console, make `run-swing-client` (this will start a client for http://localhost:10080/react/App)



Using Proactive in your project
-------------------------------
### Prolog side
The intention is that you users can include src/react.pl in their existing server environment. src/server.pl provides a stub environment, which just starts a server on port 10080, ensures jsx, react and dom are loaded, and provides two important hooks:
   * When a file is reconsulted, `trigger_react_recompile/1` is called with the module name. It also uses the clauses of requires/1 to load any subordinate modules. Unless you do something else when a file is consulted, you probably want to just copy this clause verbatim into your project.
   * `react:goal_is_safe/1` MUST be defined if you want to use `on_server/1` anywhere.

#### Creating components
Each component is declared in its own module. A component is either native (ie an object provided in Java, like a button or a textfield), or another Proactive component (a 'widget' in virtual-dom terminology). If you use another proactive component in your component, include the fact `requires(+NameOfSubcomponent)` in your module so that they are correctly chained together.

A component has two kinds of internal data: 'state' and 'props'. The difference is quite subtle but critical to grasp.
   * state is mutable. You are given an opportunity to define an initial state for a component, and event handlers can be applied to mutate your own state. You cannot mutate the state of any other component but your own.
   * props are immutable. They are provided by the parent, and there is no facility to change them once they are defined except to create an entirely new child with new props.

In general, you can use either in any case, but there might be serious implications for efficiency. Every time state is mutated, the component and ALL its children are re-rendered. (This is not quite as expensive as it sounds, but it is still not free).

There are several tutorials on React which try to explain the difference betwen the two; these will apply to Proactive as well. (Note that the deprecated setProps() is deliberately missing from Proactive)

##### Rendering
Each component MUST contain a predicate `render(+State, +Props, -Document)` which takes state and props and returns a vdom object. You can use a JSX-like syntax to create Document, or you can create it yourself.

The JSX syntax uses quasiquotation in SWI-Prolog (with the domain jsx) to expand XML-like structures into these `element/3` terms. You can escape to Prolog using {}. Maps are supported indirectly: You can write {State.foo} to mean 'the value that foo would have in the state'. Note this is NOT actually implemented as ./3 to read the map! First of all, if the key is missing, it does not fail, but binds to the special term {null}. Second, the state is not (currently) a real map, but a list of =/2 terms). You can also use the distinguished map 'this' to refer to the current execution context, meaning you can pass this.handler to a subcomponent and the context will be switched back from the subcomponent to the current component when the handler is executed

Example:
```
render(State, _, App):-
   {|jsx(App)||
   <Panel>
     <Button label={State.label} onClick={this.handleClick}/>
     <Field value={Value}/>
   </Panel>|},
   Value = 'This is a value'.
```
Note that it is forbidden to call on_server/1 in render/3.

If you don't want to use JSX, you can create the terms yourself. A document is always[1] an `element/3` term (in future atoms may also be permitted)

An `element/3` term has the following args:
   * First arg is a tag name, and must be an atom
   * Second arg is a list of attributes.
      * An attribute is a term `=/2` where the first argument is an atom, and the second argument is an *arbitrary term*. This differs from the `element/3` case in SWI-Prolog where the second argument must be an atom, and consequently means that `xml_write/2` on the VDOM will not (necessarily) work.
   * Third arg is a list of children, where each child is a Document.

[1] actually, if you want, you can return a jsx/2 term, where the first argument is a goal to run, and the second argument is the Document. This should not be required for hand-written code, but JSX uses it so that constructs like `{|jsx(Foo)||<X>{Var}</X>}, X = []` where we do not know when compiling the JSX whether or not X has any children.

##### State
You MAY declare a predicate `getInitialState(+Props, -State)` in your module. (note that using Props here may be an anti-pattern, depending on exactly what you do. See https://facebook.github.io/react/tips/props-in-getInitialState-as-anti-pattern.html). getInitialState/2 will be called before the component is _first_ instantiated. State is expected to be a term {}(Term), where Term is either:
   * ,(Cell, Term)
   * Cell
And Cell is a term :(Atom, Value).

This looks obtuse, but it means you can write states like {foo: bar, qux: {baz: [], quux:3}}. Why use {} instead of []? There are two reasons:
   * I found myself wanting to write [rows=[a,b,c]] several times, and the recursive nature of the state meant that I had to 'protect' the inner list from being converted into a state. This made it ugly to recover it later
   * In the end, I never found myself building a state piecewise like you might a list - the lack of an | operator simply didn't seem to matter. If it became necessary, a put_state(+State, +Key, +Value, -NewState) could be trivially written

Note that the empty state is {}, but an unbound state is simply ignored. This may come in handy if your version of GNU Prolog for Java does not support {} as a term due to https://github.com/drt24/gnuprologjava/issues/6

You can get at these elements using '.'(+State, +Key, -Value). '.' is already defined by SWI-Prolog to mean something different, but the translation is taken care of between instances. Note that the behaviour is different to SWI-Prolog's definition:
   * The functor of Key is used as the lookup
   * If there is no match in the state, then {null} is returned
   * {null} is a valid state, and always returns {null} for any key. This means you can write State.foo.bar.baz, and if State has no foo, then you'll just get {null} rather than a Prolog version of a null pointer exception.
   * If a value is found:
      * If Key is an atom, then Value is the found value
      * If Key was a compound, then Value is the found value with the arguments of Key consed on to it. FIXME: This was to enable some syntactic sugar, but now I cant find the example I needed it for.

##### Event Handlers
You MAY declare event handlers in a Javascript-esque way: For example,
```
{|jsx(...)||
  <Button label='Click me' onClick={some_goal}/>|}
```
When the button is clicked, some_goal will be called with 3 extra arguments:
   * +Event (which is dependent on the widget generating the event)
   * +State
   * +Props
   * -NewState

You can use this to mutate the state. The NewState is *merged* with the existing state - if you do not mention a key in the output, it will persist from the old state. If you want to delete a key, bind it to the special term {null}.

You will notice that this forces very loose coupling between components - Demeter's law is explicitly enforced. You simply cannot mutate the state of another component or any global state, since you have absolutely no way of getting to it. However, you _can_ pass a predicate bound to an instance into a subcomponent using the _this_ pointer. This takes a bit of mental gymnastics to understand if you are are primarily a Prolog person, but imagine that each component you define is really running in its own compartmentalized environment. You can pass an event handler to another compartment with a pointer to your compartment, and the resulting handler will be executed in your own environment if called.

To help with this, there is a predicate `bubble_event(+Props, +Key, +Event)` you can use to chain event handlers together. Suppose you have a component Foo, which contains a subcomponent Bar, and when Bar encounters a particular event onQux you want to change the state in Foo. You can do this by instantiating Bar as
```
   ...
   <Bar onQux={this.handleQux}/>
   ...

handleQux(Event, State, Props, NewState):-
   ... update state with info from Event ...
```

and then in the Bar module:

```
render(_State, _Props, Bar):-
  {||jsx(Bar)|
  <Widget onQux={this.handleQux}/>|}.

handleQux(Event, _State, Props, _):-
    bubble_event(Props, onQux, Event).
```

If Bar did not want to process the event itself, it could just have

```
   <Widget onQux={Props.handleQux}/>
```

Note that this also gives Bar an opportunity to mutate the event - it can bubble a _different_ event to its parent if it chooses to. This can be used for forcing a field to be all upper-case, for example.

##### Fields
Fields are problematic in React (and Proactive) because they break the declarative nature of the virtual DOM. The output of render/3 is supposed to indicate what the form looks like, such as <input value="foo"/>. But then users can come along and mutate the input, and suddenly the app no longer looks like the virtual DOM. What to do?

React has two solutions to this, and Proactive uses the one called 'Managed Components'. In this model, the user cannot actually change the fields at all - preserving the purity and declarative nature of the form. That sounds useless, but it's not as bad as it seems. If you add an onChange listener to a field, it advises the component when a user has modified the field with the _proposed_ change. The component must then set the state, triggering a re-render with this as the new value (or setting the value to a function of the proposed, if desired, such as making it all-caps, or removing spaces). A crucial side-effect of this approach is that if your field does not have an onChange, _it will be read-only_!

##### Obtaining information from the server
You can use `on_server/1` to call goals on the server. On the server side, `on_server(X)` is simply defined as `call(X)`, so adding `on_server/1` around code has only a tiny performance hit. All goals sent from the client are vetted by a multi-file hook `goal_is_safe/1`. You need to provide such a hook; the demo server simply accepts all goals.

The `on_server/1` goal supports nondeterminism. You can backtrack onto the server, but be aware of the consequences in latency this may introduce.


##### Shipping parts of your code to the client
You can use the directive :-table_predicate(+Indicator) in your module to make that predicate available to the client. For example,
```
:-table_predicate(country_name/1).
:-table_predicate(country_population/2).
```

Note that these will be available in the module where you put the directive.

##### Data flow
Like in React, data always flows down the tree. You CAN use the this pointer to get events to flow up the tree, or you can use the implementation of the Flux dispatcher.

A component can ONLY change its own state.
A component can ONLY provide props to child components. These props may be hard coded or derived from the parents own state.

### Java side
You can either use the widgets in the existing ui package, or provide your own. To provide your own, call ReactComponentFactory.setUIConfiguration() passing in an instance of a class that implements ReactComponentFactoryConfiguration, which maps tag names in the vdom to constructors for classes that implement those.

The java client will load /boilerplate.pl into module(user) when it starts. If you need extra predicates to appear in here, you can replace the file in the jar.
