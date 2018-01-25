# Coal Mine

A large corpus of Clojure(Script) compiler characterization tests derived from submissions by 
the top 1000 users on [4Clojure](http://www.4clojure.com).

This is useful for regression testing compiler patches. Since the codebase is large, it is also 
useful for testing compiler performance.

To run all tests: `(coal-mine.test-runner/-main)`

Alternatively, `clojure -m coal-mine.script test` will run the all the tests via ClojureScript on Node, split for 
RAM use reduction.

To run (without manually cloning):

```
clojure -Sdeps '{:deps {coal-mine {:git/url "https://github.com/mfikes/coal-mine" :sha "6de2507b9a3dc2294a68c920d8cef3fe3c89cbaf"}}}' -J-Xmx3G -m coal-mine.script test
```

You can also override the ClojureScript dep as follows:

Shipping version:

```
clojure -Sdeps '{:deps {coal-mine {:git/url "https://github.com/mfikes/coal-mine" :sha "6de2507b9a3dc2294a68c920d8cef3fe3c89cbaf"} org.clojure/clojurescript {:mvn/version "1.9.946"}}}' -J-Xmx3G -m coal-mine.script test
```

Local compiler source tree:

```
clojure -Sdeps '{:deps {coal-mine {:git/url "https://github.com/mfikes/coal-mine" :sha "6de2507b9a3dc2294a68c920d8cef3fe3c89cbaf"} org.clojure/clojurescript {:local/root "/path/to/clojurescript"} com.google.javascript/closure-compiler-unshaded {:mvn/version "v20170910"}}}' -J-Xmx3G -m coal-mine.script test
```

Arbitrary GitHub version of ClojureScript:

```
clojure -Sdeps '{:deps {coal-mine {:git/url "https://github.com/mfikes/coal-mine" :sha "6de2507b9a3dc2294a68c920d8cef3fe3c89cbaf"} org.clojure/clojurescript {:git/url "https://github.com/clojure/clojurescript" :sha "9ddd356d344aa1ebf9bd9443dd36a1911c92d32f"} com.google.javascript/closure-compiler-unshaded {:mvn/version "v20170910"}}}' -J-Xmx3G -m coal-mine.script test
```
