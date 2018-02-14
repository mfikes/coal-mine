# Coal Mine

A large corpus of Clojure(Script) compiler characterization tests derived from submissions by 
the top 1000 users on [4Clojure](http://www.4clojure.com).

This is useful for regression testing compiler patches. Since the codebase is large, it is also 
useful for testing compiler performance.

To run all tests:

```
clojure -m coal-mine.test-runner
```

or 

```
clojure -m cljs.main -m coal-mine.test-runner
```

Alternatively, 

```
clojure -m coal-mine.script test
``` 

will run the all the tests via ClojureScript on Node, split for 
RAM use reduction.
