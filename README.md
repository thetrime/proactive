Proactive
=========

A Prolog and Java implementation of the React concept

Quick demo
----------
If you want to just get stuck in, follow these steps:
   1. make
   1. make run-server (this will start a server on port 10080)
   1. In your server, consult('demo/App').
   1. In another console, make run-client (this will start a client for http://localhost:10080/react/App)

Roadmap
-------
Currently this is just a toy, but the skeleton is almost complete and ready for fleshing out.

### Major tasks
   * I need to implement some equivalent of Flux. Currently there is absolutely no way to get data up the tree.
   * I would like to be able to handle HTML natively. That is, tags which are not otherwise overloaded, but also valid HTML will become PrologHTML nodes in the tree. Components can then do with those what they want.
   * I had to suck quite a bit of code from GPJ into ReactEnvironment and ReactModule. I need to talk to the GPJ people to see if I can make some tiny modifications in their code to allow me to just override the methods I want
   * A lot of documentation needs to be written.
   
### Boring tasks
   * We need a lot more components. 
   * Similarly, the components I have are pretty useless. Only one of them has an event handler you can attach, for example!

### Minor tasks
   * Tidy up the import statements. Many are unnecessary.
   * I would like to investigate a JSX (PLX?) mode for emacs
   * Make on_server illegal from inside render/3
   * Enforce state being a list of =/2 pairs where the first arg of each is an atom


Using Proactive in your project
-------------------------------
### Prolog side
The intention is that you users can include src/react.pl in their existing server environment. src/server.pl provides a stub environment, which just starts a server on port 10080, ensures jsx and react are loaded, and provides two important hooks:
   * When a file is reconsulted, trigger_react_recompile/1 is called with the module name. It also uses the clauses of requires/1 to load any subordinate modules. Unless you do something else when a file is consulted, you probably want to just copy this clause verbatim into your project.
   * react:goal_is_safe/1 MUST be defined if you want to use on_server/1 anywhere.

#### Creating components
Each component is declared in its own module. A component is either a widget (ie a native object provided in Java, like a button or a textfield), or another Proactive component. If you use another proactive component in your component, include the fact requires(+NameOfSubcomponent) in your module so that they are correctly chained together.

A component has two kinds of internal data: 'state' and 'props'. The difference is quite subtle but critical to grasp.
   * state is mutable. You are given an opportunity to define an initial state for a component, and event handlers can be applied to mutate your own state. You cannot mutate the state of any other component but your own.
   * props are immutable. They are provided by the parent, and there is no facility to change them once they are defined except to create an entirely new child with new props.

In general, you can use either in any case, but there might be serious implications for efficiency. Every time state is mutated, the component and ALL its children are re-rendered. (This is not quite as expensive as it sounds, but it is still not free).

There are several tutorials on React which try to explain the difference betwen the two; these will apply to Proactive as well. (Note that the deprecated setProps() is deliberately missing from Proactive)

##### Rendering
Each component MUST contain a predicate render(+State, +Props, -Document) which takes state and props and returns a vdom object. You can use a JSX-like syntax to create Document, or you can create it yourself: Document is either an atom, an element/3 term, or a list/1 term.

An element/3 term has the following args:
   * First arg is a tag name, and must be an atom
   * Second arg is a list of attributes.
      * An attribute is a term =/2 where the first argument is an atom, and the second argument is an *arbitrary term*. This differs from the element/3 case in SWI-Prolog where the second argument must be an atom, and consequently means that xml_write/2 on the VDOM will not (necessarily) work.
   * Third arg is a list of children, where each child is a Document.

A list/1 term has the following args:
   * A list of children, where each child is a Document.

The JSX syntax uses quasiquotation in SWI-Prolog (with the domain jsx) to expand XML-like structures into these element/3 terms. You can escape to Prolog using {}. Maps are supported indirectly: You can write {State.foo} to mean 'the value that foo would have in the state'. The client unpacks this and binds the argument to the correct value just before execution - it is not an actual map (GNU Prolog for Java does not support them).

Example:
render(State, _, App):-
   App = {|jsx||
          <Panel>
            <Button label={State.label}/>
            <Field value={Value}/>
          </Panel>|},
   Value = 'This is a value'.

Note that it is forbidden to call on_server/1 in render/3.

##### State
You MAY declare a predicate getInitialState(-State) in your module. This will be called before the component is first instantiated. State can is expected to be a list of =/2 terms, where each term has an atom for the first argument. This is not currently checked, but I plan to add a check to ensure this is the case, since failing to do so leads to surprising and unexpected problems in event handlers when we try and 'merge' states.

##### Event Handlers
You MAY declare event handlers in the Javascript way: For example,
{|jsx||
  <Button label='Click me' onClick={some_goal}/>|}

When the button is clicked, some_goal will be called with 3 extra arguments:
   * +State
   * +Props
   * -NewState

You can use this to mutate the state. The NewState is *merged* with the existing state - if you do not mention a key in the output, it will persist from the old state. If you want to delete a key, bind it to the special term {null}.

You will notice that this forces very loose coupling between components - Demeter's law is explicitly enforced. You simply cannot mutate the state of another component or any global state, since you have absolutely no way of getting to it.

##### Obtaining information from the server
You can use on_server/1 to call goals on the server. On the server side, on_server(X) is simply defined as call(X), so adding on_server/1 around code has only a tiny performance hit. All goals sent from the client are vetted by a multi-file hook goal_is_safe/1. You need to provide such a hook; the demo server simply accepts all goals.

The on_server/1 goal supports nondeterminism. You can backtrack onto the server, but be aware of the consequences in latency this may introduce.

##### Data flow
Like in React, data always flows down the tree, however in Proactive it is even more rigid since you cannot pass handlers with pointers to parent functions (well you can, but there is no point since the functions contain no references to the state in the parent so you cannot mutate them).

A component can ONLY change its own state.
A component can ONLY provide props to child components. These props may be hard coded or derived from the parents own state.

### Java side
You can either use the widgets in the existing ui package, or provide your own. To provide your own, call ReactComponentFactory.setUIConfiguration() passing in an instance of a class that implements ReactComponentFactoryConfiguration, which maps tag names in the vdom to constructors for classes that implement those.

The java client will load /boilerplate.pl into module(user) when it starts. If you need extra predicates to appear in here, you can replace the file in the jar.




How it works
------------