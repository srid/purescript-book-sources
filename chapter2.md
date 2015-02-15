# Getting Started

## Chapter Goals

In this first chapter, the goal will be to set up a working PureScript development environment, and to write our first PureScript program.

The first code we will write is an example of a library which will use dependencies from NPM and Bower, and which will be built using the Grunt build automation tool. It will provide a single function, to compute the length of the diagonal in a right-angled triangle.

## Introduction

Here are the tools we will be using to set up our PureScript development environment:

- [`psc`](http://purescript.org) - The PureScript compiler itself.
- [`npm`](http://npmjs.org) - The Node Package Manager, which will allow us to install the rest of our development tools.
- [`bower`](http://bower.io/) - A package manager which is used to version various PureScript packages which we will need.
- [`grunt`](http://gruntjs.com/) - An automation tool which we will use to build our PureScript code.

The rest of the chapter will guide you through installing and configuring these tools.

## Installing PureScript

The recommended approach to installing the PureScript compiler is to build the compiler from source. The PureScript compiler can be downloaded as a binary distribution for 64-bit Ubuntu from the [PureScript website](http://purescript.org), but binary distributions are currently only made for major releases. If you would like to stay up-to-date with the latest bug fixes and feature additions, and ensure that the compiler can build the latest packages, you should follow these instructions to build the latest minor release.

The main requirement is a working installation of the [Haskell Platform](http://haskell.org/platform). Depending on your operating system, you may also need to install the `ncurses` development package using your package manager (available in Ubuntu as the `libncurses5-dev` package, for example).

Begin by ensuring that you have a recent version of the Cabal executable installed:

```text
$ cabal install Cabal cabal-install
```

Also make sure the Cabal package list is up-to-date:

```text
$ cabal update
```

The PureScript compiler can either be installed globally, or in a Cabal sandbox in a local directory. This section will cover how to install PureScript globally so that its executables are available on your path.

Install PureScript from Hackage using the `cabal install` command:

```text
$ cabal install purescript
```

The compiler and associated executables will now be available on your path. Try running the PureScript compiler on the command line to verify this:

```text
$ psc
```

## Installing Tools

If you do not have a working installation of [NodeJS](http://nodejs.org/), you should install it. This should also install the `npm` package manager on your system. Make sure you have `npm` installed and available on your path.

Once you have a working copy of `npm` installed, you will need to install Grunt and Bower. It is usually a good idea to install these globally, so that their command line tools will be available to you regardless of which project you are working in.

```text
$ npm install -g grunt-cli bower
```

At this point, you will have all the tools needed to create your first PureScript project.

## Hello, PureScript!

Let's start out simple. We'll use the PureScript compiler `psc` directly to compile a basic Hello World! program. As the chapter progresses, we'll automate more and more of the development process, until we can build our app from scratch including all dependencies with three standard commands.

First of all, create a directory `src` for your source files, and paste the following into a file named `src/Chapter2.purs`:

```haskell
module Chapter2 where

import Debug.Trace

main = trace "Hello, World!"
```

This small sample illustrates a few key ideas:

- Every file begins with a module header. A module name consists of one or more capitalized words separated by dots. In this case, only a single word is used, but `My.First.Module` would be an equally valid module name.
- Modules are imported using their full names, including dots to separate the parts of the module name. Here, we import the `Debug.Trace` module, which provides the `trace` function.
- The `main` program is defined as a function application. In PureScript, function application is indicated with whitespace separating the function name from its arguments.

Let's build and run this code. Invoke the following command:

```text
$ psc src/Chapter2.purs
```

If everything worked, then you will see a relatively large amount of Javascript emitted onto the console. Instead, let's redirect the output to a file with the `--output` command line option:

```text
$ psc src/Chapter2.purs --output dist/Main.js
```

You should now be able to run your code using NodeJS:

```text
$ node dist/Main.js
```

If that worked, NodeJS should execute your code, and correctly print nothing to the console. The reason is that we have not told the PureScript compiler the name of our main module!

```text
$ psc src/Chapter2.purs --output dist/Main.js --main=Chapter2
```

This time, if you run run your code, you should see the words "Hello, World!" printed to the console.

## Removing Unused Code

If you open the `dist/Main.js` file in a text editor, you will see quite a large amount of JavaScript. The reason for this is that the compiler ships with a set of standard functions in a set of modules called the Prelude. The Prelude includes the `Debug.Trace` module that we are using to print to the console.

In fact, almost none of this generated code is being used, and we can remove the unused code with another compiler option:

```text
$ psc src/Chapter2.purs --output dist/Main.js --main=Chapter2 --module Chapter2
```

I've added the `--module Chapter2` option, which tells `psc` only to include JavaScript which is required by the code defined in the `Chapter2` module. This time, if you open the generated code in a text editor, you should see the following:

```javascript
var PS = PS || {};
PS.Debug_Trace = (function () {
    "use strict";
    function trace(s) { 
      return function() {
        console.log(s);
        return {};  
      };
    };
    return {
        trace: trace
    };
})();

var PS = PS || {};
PS.Chapter2 = (function () {
    "use strict";
    var Debug_Trace = PS.Debug_Trace;
    var main = Debug_Trace.trace("Hello, World!");
    return {
        main: main
    };
})();

PS.Chapter2.main();
```

If you run this code using NodeJS, you should see the same text printed onto the console.

This illustrates a few points about the way the PureScript compiler generates Javascript code:

- Every module gets turned into a object, created by a wrapper function, which contains the module's exported members.
- PureScript tries to preserve the names of variables wherever possible
- Function applications in PureScript get turned into function applications in JavaScript.
- The main method is run after all modules have been defined, and is generated as a simple method call with no arguments.
- PureScript code does not rely on any runtime libraries. All of the code that is generated by the compiler originated in a PureScript module somewhere which your code depended on.

These points are important, since they mean that PureScript generates simple, understandable code. In fact, the code generation process in general is quite a shallow transformation. It takes relatively little understanding of the language to predict what JavaScript code will be generated for a particular input.

## Automating the Build with Grunt

Now let's set up Grunt to build our code for us, instead of having to type out the PureScript compiler options by hand every time.

Create a file in the project directory called `Gruntfile.js` and paste the following code:

```javascript
module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({

    srcFiles: ["src/**/*.purs"],

    psc: {
      options: {
        main: "Chapter2",
        modules: ["Chapter2"]
      },
      all: {
        src: ["<%=srcFiles%>"],
        dest: "dist/Main.js"
      }
    }
  });

  grunt.loadNpmTasks("grunt-purescript");
  
  grunt.registerTask("default", ["psc:all"]);
};
```

This file defines a Node module, which uses the `grunt` module as a library to define a build configuration. It uses the `grunt-purescript` plugin, which invokes the PureScript compiler and exposes its command line options as JSON properties.

The `grunt-purescript` plugin also provides other useful capabilities, such as the ability to automatically generate Markdown documentation from your code, or generate configuration files for your libraries for the `psci` interactive compiler. The interested reader is referred to the `grunt-purescript` [project homepage](http://github.com/purescript-contrib/grunt-purescript).

Install the `grunt` library and the `grunt-purescript` plugin into your local modules directory as follows:

```text
$ npm install grunt grunt-purescript@0.6.0
```

With the `Gruntfile.js` file saved, you can now compile your code as follows:

```text
$ grunt
>> Created file dist/Main.js.

Done, without errors.
```

## Creating an NPM Package

Now that you've set up Grunt, you don't have to type out compiler commands every time you want to recompile, but more importantly, the end-users of your code don't need to either. However, we've now added an extra step: we need to install a custom set of NPM packages before we can build. 

Let's define an NPM package of our own, which specifies our dependencies.

In the project directory, run the `npm` executable, specifying the `init` subcommand, to initialize a new project:

```text
$ npm init
```

You will be asked a variety of questions, at the end of which, a file named `package.json` will be added to the project directory. This file specifies our project properties, and we can add our dependencies as an additional property. Open the file in a text editor, and add the following property to the main JSON object:

```javascript
"dependencies": {
  "grunt-purescript": "0.6.0"
}
```

This specifies an exact version of the `grunt-purescript` plugin that we'd like to install.

Now, instead of having to install dependencies by hand, your end-users can simply use `npm` to install everything that is required:

```text
$ npm install
```

## Tracking Dependencies with Bower

To write the `diagonal` function (the goal of this chapter), we will need to be able to compute square roots. The `purescript-math` package contains type definitions for functions defined on the JavaScript `Math` object, so let's install it. Just like we did with our `npm` dependencies, we could download this package directly on the command line, by typing:

```text
$ bower install purescript-math#0.1.0
```

This will install version 0.1.0 of the `purescript-math` library, along with its dependencies.

However, we can set up a `bower.json` file which contains our Bower dependencies, just like we used `npm init` to create `package.json` and control our NPM dependencies.

On the command line, run:

```text
$ bower init
```

Just like in the case of NPM, you will be asked a collection of questions, at the end of which, a `bower.json` file will be placed in the project directory. During this process, you will be asked whether you would like to include existing dependencies in the project file. If you select Yes, you should see a section like this in `bower.json`:

```javascript
"dependencies": {
  "purescript-math": "0.1.0"
}
```

Now, your users will not have to specify dependencies by hand, but instead can pull dependencies by simply invoking:

```text
$ bower update
```

Let's update our Grunt script to include dependencies pulled from Bower. Edit `Gruntfile.js` to change the source files line as follows:

```javascript
srcFiles: ["src/**/*.purs", "bower_components/**/src/**/*.purs"]
```

This line includes source files from the `bower_components` directory. If you have a custom Bower configuration, you may have to change this line accordingly.

Q> ## NPM or Bower?
Q> 
Q> You may be asking yourself: why do we need to use two package managers? Can't the PureScript libraries be included in the NPM registry?
Q> 
Q> The PureScript community has standardized on using Bower for PureScript dependencies for a number of reasons:
Q> 
Q> - PureScript library packages rarely contain JavaScript source code, so are not suitable for deployment into the NPM registry without being compiled first.
Q> - The Bower registry simply maintains a mapping from package names and versions to existing Git repositories, instead of hosting code directly. This allows the community to use existing tools such as GitHub to manage code and releases.
Q> - Bower does not require packages to conform to any particular layout, such as the CommonJS module standard.
Q> 
Q> Of course, you are free to use any package manager of your choice - the PureScript compiler and tools are not dependent on Bower (or NPM or Grunt, for that matter) in any way.

## Computing Diagonals

Let's write the `diagonal` function, which will be an example of using a function from an external library.

First, import the `Math` module by adding the following line at the top of the `src/Chapter2.purs` file:

```haskell
import Math
```

Now define the `diagonal` function as follows:

```haskell
diagonal w h = sqrt (w * w + h * h)
```

Note that there is no need to define a type for our function. The compiler is able to infer that `diagonal` is a function which takes two numbers and returns a number. In general, however, it is a good practice to provide type annotations as a form of documentation.

Let's also modify the `main` function to use the new `diagonal` function:

```haskell
main = print (diagonal 3 4)
```

Now compile the module again, using Grunt:

```text
$ grunt
```

If you run the generated code again, you should see that your code has been invoked successfully:

```text
$ node dist/Main.js 

5
```

## Testing Code Using the Interactive Mode

The PureScript compiler also ships with an interactive REPL called `psci`. This can be very useful for testing your code, and experimenting with new ideas. Let's use `psci` to test the `diagonal` function.

The `grunt-purescript` plugin can be configured to generate a `psci` configuration for your source files. This saves you the trouble of having to load your modules into `psci` manually.

To set this up, add a new build target to your `Gruntfile.js` file, below the `psc` or `pscMake` target:

```javascript,
dotPsci: ["<%=srcFiles%>"]
```

Also, add this target to the default task:

```javascript
grunt.registerTask("default", ["psc:all", "dotPsci"]);
```

Now, if you run `grunt`, a `.psci` file will be generated in the project directory. This file specifies the commands which should be used to configure `psci` when the interactive mode is loaded.

Load `psci` now:

```text
$ psci
> 
```

You can type `:?` to see a list of commands:

```text
> :?
The following commands are available:

    :?              Show this help menu
    :i <module>     Import <module> for use in PSCI
    :m <file>       Load <file> for importing
    :q              Quit PSCi
    :r              Reset
    :t <expr>       Show the type of <expr>
```

By pressing the Tab key, you should be able to see a list of all functions available in your own code, as well as any Bower dependencies and the Prelude modules.

Try evaluating a few expressions now. To evaluate an expression in `psci`, type one or more lines, terminated with Ctrl+D:

```text
> 1 + 2
3

> "Hello, " ++ "World!"
"Hello, World!"
```

Let's try out our new `diagonal` function in `psci`:

```text
> Chapter2.diagonal 5 12

13
```

You can also use `psci` to define functions:

```text
> let double x = x * 2

> double 10
20
```

Don't worry if the syntax of these examples is unclear right now - it will make more sense as you read through the book.

Finally, you can check the type of an expression by using the `:t` command:

```text
> :t true
Prim.Boolean

> :t [1, 2, 3]
[Prim.Number]
```

Try out the interactive mode now. If you get stuck at any point, simply use the Reset command `:r` to unload any modules which may be compiled in memory.

## Optional: Building CommonJS Modules

The PureScript compiler `psc` generates JavaScript code in a single output file, which is suitable for use in a web browser. There is another option for compilation, called `psc-make`, which can generate a separate CommonJS module for every PureScript module which is compiled. This may be preferable if you are targeting a runtime like NodeJS which supports the CommonJS module standard.

To invoke `psc-make` on the command line, specify the input files, and a directory in which CommonJS modules should be created with the `--output` option:

```text
$ psc-make src/Chapter2.purs --output dist/
```

This will create a subdirectory inside the `dist/` directory for every module provided as an input file. If you are using Bower dependencies, don't forget to include source files in the `bower_components/` directory as well!

The `grunt-purescript` plugin also supports compilation using `psc-make`. To use `psc-make` from Grunt: make the following changes in the `Gruntfile.js` file:

- Change the build target from `psc` to `pscMake`.
- Change the destination from a single file `dist/Main.js` to a directory: `dest: "dist/"`
- Change the default task to reference the `pscMake` build target.

Now, the `grunt` command line tool should create a subdirectory under `dist/` for the `Chapter2` module and every one of its dependencies.

## Using Grunt Project Templates

NPM, Grunt, and Bower can be customized in many ways, to support a variety of interesting build processes. However, for simple projects, the steps can be automated by using a Grunt project template.

The `grunt-init` tool provides a way to bootstrap a simple project from a template. The `grunt-init-purescript` project provides a simple template for a PureScript project, including a simple test suite.

To set up a project using `grunt-init`, first install the command line tool using NPM:

```text
$ npm install -g grunt-init
```

Now, clone the PureScript template into your home directory. For example, on Linux, or Mac:

```text
$ mkdir ~/.grunt-init
$ git clone https://github.com/purescript-contrib/grunt-init-purescript.git \
    ~/.grunt-init/purescript
```

Now, you can create a simple project in a new directory:

```text
$ mkdir new-project
$ cd new-project/
$ grunt-init purescript
```

You will be asked a number of simple questions, after which the project will be initialized in the current directory, and ready to build, using the commands we have already seen:

```text
$ npm install
$ bower update
$ grunt
```

The final command will build the source files, and run the test suite.

You can use this project template as the basis of a more complicated project.

X> ## Exercises
X> 
X> 1. (Easy) Use the `Math.pi` constant to write a function `circleArea` which computes the area of a circle with a given radius. Test your function using `psci`.
X> 1. (Medium) Add a Grunt task to the `Gruntfile.js` file to execute the compiled code using NodeJS, so that instead of typing `node dist/Main.js`, the user can simply type `grunt run`. _Hint_: Consider using the `grunt-execute` Grunt plugin. 

## Conclusion 

In this chapter, we set up a development environment from scratch, using standard tools from the JavaScript ecosystem: NPM, Bower and Grunt.

We also wrote our first PureScript function, and a JavaScript program which could be compiled and executed using NodeJS.

We will use this development setup in the following chapters to compile, debug and test our code, so you should make sure that you are comfortable with the tools and techniques involved.

