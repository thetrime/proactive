Proactive
=========

A Prolog and Java implementation of the React concept

Quick demo
----------
If you want to just get stuck in, follow these steps:
   1 make
   1 make run-server (this will start a server on port 10080
   1 In your server, consult('demo/App').
   1 In another console, make run-client (this will start a client for http://localhost:10080/react/App)

Roadmap
-------
Currently this is just a toy, but the skeleton is almost complete and ready for fleshing out.

### Major tasks
   * I need to implement some equivalent of Flux. Currently there is absolutely no way to get data up the tree.
   * I would like to be able to handle HTML natively. That is, tags which are not otherwise overloaded, but also valid HTML will become PrologHTML nodes in the tree. Components can then do with those what they want.
   * I had to suck quite a bit of code from GPJ into ReactEnvironment and ReactModule
   * A lot of documentation needs to be written.
   
### Boring tasks
   * We need a lot more components. Ideally all the components I provide should be in a package like org.proactive.ui, and will allow others to provide their own by calling a static method on ReactComponentFactory to set up a different hash table
   * Similarly, the components I have are pretty useless. Only one of them has an event handler you can attach, for example!

### Minor tasks
   * Tidy up the import statements. Many are unnecessary.
   * I would like to investigate a JSX (PLX?) mode for emacs