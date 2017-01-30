---++ Building Proactive
As of version-0.3.0, proactive incorporates several clients and backends, all using (unfortunately) different build systems (not by choice!)

The requirements for each are combination are given below:

---++ Proactive/Swing
This is the easiest to build and has the lightest requirements. All you need is javac. To compile, run:
```
make CLIENTS="swing-client" package
```

---++ Proactive/JS
This is the hardest in terms of requirements. You need the following:
   * browserify
   * uglifyjs
   * proscript (which itself requires emscripten and gmpjs. See below for instructions on building proscript)
Once you have everything, you can run
```
make CLIENTS="js-client" package
```

---++ Proactive/iOS
This requires just XCode, which in turn means you need to be running OSX. You also need all the normal requirements for provisioning if you want to deploy the built application to an iDevice. To compile, open the provided XCode project and build the library in the usual way. A demonstration application has been provided which links to the library and illustrates how to set up a window to display a Proactive form.

---++ Proactive/swipl
Actually, this is the easiest, since it doesn't require anything at all. If you want to JUST make a package with this in it, then
```
make CLIENTS="" package
```

Note that you can combine clients into a single package like this. To build a package containing both the Proactive/JS and Proactive/Swing clients, run
```
make CLIENTS="js-client swing-client" package
```
or more simply
```
make package
```
since this builds all the clients it knows about by default.

