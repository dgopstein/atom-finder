# atom-finder [![Build Status](https://travis-ci.org/dgopstein/atom-finder.svg?branch=master)](https://travis-ci.org/dgopstein/atom-finder)

A Clojure library for finding [Atoms of Confusion](https://atomsofconfusion.com) in C projects.

Facilities for:

 * Parsing C/C++ with Eclipse CDT library
 * Finding specific patterns in an AST
 * Traversing version histories through git
 * Parsing commit logs for bug/patch IDs

### Working with Clojure

This project is primariy written in Clojure(JVM), and uses many Java libraries.
In order to run this project you should install:

 * [Leiningen](https://leiningen.org/) - The Clojure build manager.
 This tool will automatically download the right version of Clojure, resolve all the necessary libraries, run tests, and execute the program.
 * One of [Emacs](https://www.gnu.org/software/emacs/)/[CIDER](https://cider.readthedocs.io/en/latest/), [Sublime](https://www.sublimetext.com/)/[SublimeREPL](https://packagecontrol.io/packages/SublimeREPL), [Nightcode](https://sekao.net/nightcode/), or anything that offers you a clojure-centric workflow. The way one writes Clojure (and lisp in general) is a bit more interactive than traditional development. It's important to be able to evaluate code as you write it.
