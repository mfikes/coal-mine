# Coal Mine

A large corpus of Clojure(Script) compiler characterization tests derived from submissions by 
the top 1000 users on [4Clojure](http://www.4clojure.com).

This is useful for regression testing compiler patches. Since the codebase is large, it is also 
useful for testing compiler performance.

To run all tests: `(coal-mine.test-runner/-main)`

Alternatively, `script/test` will run the all the tests via ClojureScript on Node, split for 
RAM use reduction, while optionally specifying the ClojureScript build to use:

```
CLJS_VERSION=1.9.946 script/test
```
