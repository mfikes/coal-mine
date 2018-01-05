# Coal Mine

A large corpus of compiler characterization tests derived from submissions by 
the top 1000 users on [4Clojure](http://www.4clojure.com), made to work with ClojureScript.

This is useful for regression testing compiler patches. Since the codebase is large, 
it is also useful for testing compiler performance.

You can run `script/test` in order to build and run the all the tests under Node, partitioned so as to reduce memory pressure, optionally specifying the ClojureScript build to use:

```
CLJS_VERSION=1.9.946 script/test
```

Alternatively, you can load and call `coal-mine.test-runner/-main` in any Clojure or ClojureScript environment.