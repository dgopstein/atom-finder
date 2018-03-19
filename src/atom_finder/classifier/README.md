This directory contains *classifiers* for atoms of confusion. For each of the 15
identified confusing patterns
in
[Gopstein 2017](https://atomsofconfusion.com/papers/understanding-misunderstandings-fse-2017.pdf).
That is, each file here uses heuristics to determine whether or not a piece of
code is an example of one of those atoms of confusion.

Each classifier is registered in a master list, `atom-finder.classifier/atoms`,
contained in [classifier.clj](../../src/atom_finder/classifier.clj). From there,
atoms are referenced in `atom-lookup` a map from names to classifiers, and
`find-all-atoms`, a function which given an AST node will find every instance of
every type of atom below it. Together these functions are used extensively in
files like `atom-patch.clj` and the `questions/` directory to find atoms in
files and patches.

Each entry in the `atoms` master list is a 3-valued map:

```clj
{:name Keyword :classifier (=> IASTNode Boolean) :finder (=> IASTTranslationUnit [IASTNode])}
```

i.e. each "atom" contains a:

* _name_: a unique keyword identifier used through the project
* _classifier_: a function that can identify whether or not an individual node
is an atom
* _finder_: a function that given an AST root can find all atoms beneath it

For most atom types, a finder is not necessary to implement by hand, as it can
be created trivially from a classifier using only the `default-finder` function.
A handful of atom types do, however, hand-roll their own finder for performance
reasons.

The simplest classifier is [Conditional Operator](classifier/conditional.clj),
which simply looks at the _type_ of the parsed AST node. If the node represents
a conditional operator (aka ternary, or `?:`), then its automatically an atom.

The most complex classifier
is [Macro Operator Precedence](classifier/macro-operator-precedence.clj), which
tries to parse the expansion of every macro in two ways. First it parses the
macro as the C standard dictates. Then it looks at every name as if it weren't
defined by the preprocessor, but instead by C identifiers, and checks whether or
not the parse would change. There are a lot of heuristics and guesswork involved
in this macro, but since we err'd on the side of conservatism, most AST nodes
deemed an atom by this classifier, are in fact confusing.
