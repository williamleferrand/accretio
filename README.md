accret.io
========

Accretio is an API, a sandbox and a runtime for social playbooks

## Purpose

Getting involved in a community can quickly get tedious.

Accretio mitigates this by helping you automate repetitive tasks. First you write (or fork) a playbook, then the platform runs it the best it can, asking you for manual help when it hits hiccups.

I started to write this tool for people who can't necessarily afford to do X or Y, but can self-organize to achieve X or Y. I hope that we can collectively build a shared repository of effective playbooks for a varied set of activities, from simple friendly gatherings to educative programs.

## Playbooks

Playbooks are a graphical representation of the finite state machine that describes how a social activity could run. They are compiled and executed by the runtime. There are some examples in the ```/playbooks``` folder.


## Install

### Dependencies

Opam makes your life easier:

```
opam pin add eliom.dev   https://github.com/ocsigen/eliom.git
```

```opam pin add js_of_ocaml.dev  https://github.com/ocsigen/js_of_ocaml.git
```

```
opam pin add reactiveData.dev  https://github.com/hhugo/reactiveData.git
```

```
opam pin add tyxml.dev https://github.com/ocsigen/tyxml.git
```

```
opam install camlzip deriving-yojson aliases ocamlgraph yojson mysql leveldb imap aws menhir eliom ocsigenserver
```

You also need [sphinx](http://sphinxsearch.com/) >= 2.2.10

If you want to modifiy the CSS of the web app, you will need Compass.

You might want to create the two directories ```mkdir -p db log```.

### Build

```
make all relink
```

### Configure

You will need to insert plausible values into ```resources/conf-template.xml``` and rename this file ```node.xml```

### Run

Start the sphinx server: ```make sphinx```

Start the ocsigen server: ```make run```


Pointing your browser to ```http://localhost:8080``` should bring you to your local accretio. If not, get in touch!

## Let's get in touch!

If you're interested in sharing thoughts about this project, feel free to [drop me a line](mailto:william.le-ferrand@polytechnique.edu) or to use the issue tracker.
