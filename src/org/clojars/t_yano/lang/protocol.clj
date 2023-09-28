(ns org.clojars.t-yano.lang.protocol)

(defmacro defdelegation
  "Enhanced defprotocol Macro in Clojure

   Introducing a powerful macro designed to streamline the use of protocols in Clojure,
   while also ensuring seamless integration with clojure.spec.

   Usage:

   When utilizing this macro, it automatically generates both a regular function and a protocol function.
   The beauty of this approach is that it allows developers to harness the capabilities of protocols
   while maintaining the simplicity and familiarity of regular functions.

   Function Naming Convention:

   Regular Function
   The name of the regular function remains identical to the one you define within the macro.

   Protocol Function
   The protocol function's name is derived from the regular function but has a '-' prefix.
   For instance, if you define a function named process within the macro, the corresponding protocol function will be named -process.

   Integration with clojure.spec:

   A significant advantage of this macro is its ability to work harmoniously with clojure.spec.
   You can easily define specs for the automatically generated regular functions.
   This ensures that tools like instrument function correctly. When the regular function is invoked,
   it's seamlessly forwarded to the corresponding protocol function, allowing for a consistent and predictable behavior.

   Key Takeaway:

   This macro offers a refined way to work with protocols in Clojure.
   By automatically generating both regular and protocol functions,
   it simplifies the development process and ensures compatibility with clojure.spec.

   Examples:
   (defdelegation IProcess
     (process [this] (println \"Processing...\"))))"
  [protocol-name options & body]
  (let [{:keys [prefix suffix] :or {prefix "-" suffix ""}} (if (map? options) options {})
        body (if (map? options) body (cons options body))
        protocol-body (mapv (fn [definition]
                              (let [fn-sym  (first definition)
                                    fn-body (rest definition)]
                                (apply list (symbol (str prefix (name fn-sym) suffix)) fn-body)))
                            body)
        fn-defs (mapv (fn [definition]
                        (let [fn-sym  (first definition)
                              protocol-fn-sym (symbol (str prefix (name fn-sym) suffix))
                              fn-defs (rest definition)
                              maybe-doc (last fn-defs)
                              fn-defs   (if (string? maybe-doc) (drop-last fn-defs) fn-defs)
                              document  (if (string? maybe-doc) maybe-doc "")
                              fn-body   (mapv (fn [arg-list]
                                                (list arg-list (apply list protocol-fn-sym arg-list)))
                                              fn-defs)]
                          (apply list `defn fn-sym document fn-body)))
                      body)]
    `(do
       (defprotocol ~protocol-name ~@protocol-body)
       ~@fn-defs)))