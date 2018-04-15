# Coal Mine

<img src="mine.jpg" align="right" height="210px" hspace="5px"/>

A large corpus of Clojure(Script) compiler characterization tests derived from submissions by 
the top 1000 users on [4Clojure](http://www.4clojure.com).

This is useful for regression testing compiler patches. Since the codebase is large, it is also 
useful for testing compiler performance.

To run all tests via Clojure:

```
clj -Aclj -m coal-mine.test-runner
```

or via ClojureScript

```
clj -Acljs -m coal-mine.test-runner
```

or via Planck

```
plk -m coal-mine.test-runner
```

Alternatively, 

```
clj -Anode
``` 

will run the all the tests via ClojureScript on Node, split for 
RAM use reduction.
