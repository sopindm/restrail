(ns restrail
  (:gen-class :main true)
  (:require [clojure.repl]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [aleph.http :as http]))

(defn- camel->keyword [str]
  (->> (str/split str #"(?<=[a-z])(?=[A-Z])")
       (map str/lower-case)
       (interpose \-)
       str/join
       keyword))

(defn keyword->camel [keyword]
  (let [words (str/split (name keyword) #"-")]
    (str/join (cons (first words) (map str/capitalize (rest words))))))

(defn read-tasks [filename]
  (with-in-str (slurp filename)
    (letfn [(read- []
              (if-let [json (json/read *in* :key-fn camel->keyword :eof-error? false)]
                (cons json (read-))))]
      (read-))))

(defn- read-json [string]
  (try (json/read-str string :key-fn camel->keyword)
       (catch Exception e
         {})))

(defn- write-json [json]
  (with-out-str (json/pprint-json json
                                  :key-fn keyword->camel
                                  :escape-unicode false)))

(defn- read-response [body]
  (-> (java.nio.charset.Charset/defaultCharset)
      (.decode (java.nio.ByteBuffer/wrap (.array body)))
      str/join read-json))

(declare json-map= json-seq= json-value=)
(defn- json= [got expected]
  (cond (map? got) (and (map? expected) (json-map= got expected))
        (sequential? got) (and (sequential? expected) (json-seq= got expected))
        :else (json-value= got expected)))

(defn- json-map= [m1 m2]
  (and (= (count m1) (count m2))
       (every? (fn [key] (if-let [value (get m2 key nil)] 
                           (json= (get m1 key) value)))
               (keys m1))))
                           
(defn- json-seq= [s1 s2]
  (and (= (count s1) (count s2)) (every? (fn [[x y]] (json= x y)) (map vector s1 s2))))

(def ^:dynamic *bindings* (atom {}))

(defn- push-binding [got expected]
  (swap! *bindings* #(assoc % (.substring expected 1) got)))

(defn- json-value= [got expected]
  (if (string? expected)
    (cond (.startsWith expected "?") (do (push-binding got expected) true)
          (.startsWith expected "!") true
          :else (= got expected))
    (= got expected)))

(defn- check-response [got expected]
  (if (json= got expected) [:success (format "OK")]
      [:failure (format "Error: wrong response%nGot: %s%nExpected: %s"
                      (write-json got)
                      (write-json expected))]))

(defn- http-method [method]
  (case method
    "GET" :get
    "POST" :post))

(defn run-task [server-url task]
  (let [{:keys [request response]} task
        {:keys [url method body]} request]
    (let [{:keys [status body]} (http/sync-http-request {:method (http-method method)
                                                         :url (str server-url url)
                                                         :body (write-json body)})]
      (if (not= status 200)
        [:failure (format "Error: wrong result status %s on %s" status (write-json request))]
        (check-response (read-response body) response)))))
          
(defn- run-tasks- [url tasks]
  (if (empty? tasks) (println "Success.")
      (let [[status message] (run-task url (first tasks))]
        (if (= status :success) (do (println message) (recur url (rest tasks)))
            (println message)))))

(defn run-tasks [url file]
  (binding [*bindings* (atom {})]
    (run-tasks- url (read-tasks file))))
      
(defn -main [url file] 
  (try (run-tasks url file)
       (catch Exception e
         (clojure.repl/pst e)))
  (System/exit 0))

