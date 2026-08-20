#!/usr/bin/env nbb
;; Re-fetch every public registry source facts.edn cites, and fail if the live
;; record no longer says what this repository recorded.
;;
;;   nbb scripts/verify-facts.cljs           check (default)
;;   nbb scripts/verify-facts.cljs --write   re-fetch and rewrite facts.edn
;;
;; Exit codes are three, not two, on purpose. A check that could not run must
;; not be indistinguishable from a check that ran and found nothing (CLAUDE.md,
;; "検査を書く前・緑を信じる前の 5 問"):
;;
;;   0  every cited URL answered 2xx and every recorded fact still matches
;;   1  a citation is broken, or a recorded fact drifted from the live source
;;   3  the check could not be performed -- refusing to report a pass
;;
;; Exit 3 covers: facts.edn missing or unreadable, zero facts in it, or every
;; single request failing at the transport level (which means the network is
;; down, not that the citations are dead). Reporting those as 0 would let a
;; machine with no egress publish a green check forever.

(ns verify-facts
  (:require ["fs" :as fs]
            [clojure.edn :as edn]
            [clojure.string :as str]))

(def write? (boolean (some #{"--write"} *command-line-args*)))

(def facts-path "facts.edn")
(def blueprint-path "blueprint.edn")

(def dataset "cloud-itonami-lei-facts")

;; Keys whose value legitimately changes without the record changing. GLEIF
;; republishes the golden copy daily, and every run stamps a new retrieval
;; time; comparing either would make this gate permanently red and therefore
;; permanently ignored.
(def volatile-keys #{:source/retrieved-at :source/golden-copy-publish-date})

(defn die! [code & msg]
  (js/console.error (str/join " " (cons (if (= code 3) "INCONCLUSIVE" "FAIL") msg)))
  (js/process.exit code))

(defn slurp* [p]
  (try (fs/readFileSync p "utf8") (catch :default _ nil)))

(defn fetch-one
  "Resolves to {:url :status :json} on any HTTP answer, {:url :transport-error}
   when the request never got one. The two are different findings: a 404 is a
   dead citation, a DNS failure is an unanswered question."
  [url]
  (-> (js/fetch url #js {:headers #js {"Accept" "application/vnd.api+json"}})
      (.then (fn [r]
               (.then (.text r)
                      (fn [t]
                        {:url url :status (.-status r)
                         :json (try (js->clj (js/JSON.parse t)) (catch :default _ nil))
                         :body-head (subs t 0 (min 200 (count t)))}))))
      (.catch (fn [e] {:url url :transport-error (str e)}))))

(defn fetch-seq
  "Sequential on purpose -- this is a public registry, not our own service."
  [urls]
  (reduce (fn [p url] (.then p (fn [acc] (.then (fetch-one url) #(conj acc %)))))
          (js/Promise.resolve [])
          urls))

;; ---------------------------------------------------------------- build

(defn addr [a]
  (when a
    (str/join ", " (remove str/blank?
                           (concat (get a "addressLines")
                                   [(get a "postalCode") (get a "city")
                                    (get a "region") (get a "country")])))))

(defn prov [r retrieved-at extra]
  (merge {:source/dataset dataset
          :source/url (:url r)
          :source/http-status (:status r)
          :source/retrieved-at retrieved-at}
         (when-let [p (get-in r [:json "meta" "goldenCopy" "publishDate"])]
           {:source/golden-copy-publish-date p})
         extra))

(defn build
  "The single definition of what facts.edn contains. --write emits it, the
   default mode rebuilds it from the live sources and diffs. Both modes go
   through here, so the file cannot drift from its own generator."
  [{:keys [record isins lou ra elf upre kids-1 kids-2]} lei retrieved-at]
  (let [rec  (get-in record [:json "data" "attributes"])
        ent  (get rec "entity")
        reg  (get rec "registration")
        isin (mapv #(get-in % ["attributes" "isin"]) (get-in isins [:json "data"]))
        lou* (get-in lou [:json "data" "attributes"])
        ra*  (get-in ra  [:json "data" "attributes"])
        elf* (get-in elf [:json "data" "attributes"])
        upr* (get-in upre [:json "data" "attributes"])]
    (into
     [(prov record retrieved-at
            {:fact/id "gleif-lei-record"
             :fact/kind :legal-entity
             :company/lei (get rec "lei")
             :company/legal-name (get-in ent ["legalName" "name"])
             :company/legal-name-language (get-in ent ["legalName" "language"])
             :company/jurisdiction (get ent "jurisdiction")
             :company/status (get ent "status")
             :company/registered-as (get ent "registeredAs")
             :company/registration-authority-id (get-in ent ["registeredAt" "id"])
             :company/entity-legal-form-id (get-in ent ["legalForm" "id"])
             :company/entity-category (get ent "category")
             :company/creation-date (get ent "creationDate")
             :company/legal-address (addr (get ent "legalAddress"))
             :company/headquarters-address (addr (get ent "headquartersAddress"))
             :company/bic (vec (get rec "bic"))
             :company/isin isin
             :company/open-corporates-id (get rec "ocid")
             :company/sp-global-id (vec (get rec "spglobal"))
             :registration/initial-date (get reg "initialRegistrationDate")
             :registration/last-update-date (get reg "lastUpdateDate")
             :registration/status (get reg "status")
             :registration/next-renewal-date (get reg "nextRenewalDate")
             :registration/managing-lou-lei (get reg "managingLou")
             :registration/corroboration-level (get reg "corroborationLevel")
             :registration/conformity-flag (get rec "conformityFlag")})

      (prov isins retrieved-at
            {:fact/id "gleif-isins"
             :fact/kind :securities
             :company/lei lei
             :company/isin isin
             :source/note "Every ISIN GLEIF maps to this LEI. Instrument identifiers, not a share count."})

      (prov lou retrieved-at
            {:fact/id "gleif-managing-lou"
             :fact/kind :lei-issuer
             :company/lei lei
             :registration/managing-lou-lei (get lou* "lei")
             :company/legal-name (get-in lou* ["entity" "legalName" "name"])
             :company/jurisdiction (get-in lou* ["entity" "jurisdiction"])
             :source/note "The Local Operating Unit that issues and maintains this LEI."})

      (prov ra retrieved-at
            {:fact/id "gleif-registration-authority"
             :fact/kind :registration-authority
             :company/lei lei
             :authority/code (get ra* "code")
             :authority/international-name (get ra* "internationalName")
             :authority/organization-name (get ra* "internationalOrganizationName")
             :authority/local-organization-name (get ra* "localOrganizationName")
             :authority/website (get ra* "website")
             :authority/country (get-in ra* ["jurisdictions" 0 "country"])
             :company/registered-as (get ent "registeredAs")
             :source/note "Resolves :company/registration-authority-id to the national register that corroborated the record."})

      (prov elf retrieved-at
            {:fact/id "iso-20275-entity-legal-form"
             :fact/kind :entity-legal-form
             :company/lei lei
             :elf/code (get elf* "code")
             :elf/local-name (get-in elf* ["names" 0 "localName"])
             :elf/language (get-in elf* ["names" 0 "languageCode"])
             :elf/country-code (get elf* "countryCode")
             :elf/status (get elf* "status")
             :elf/date-created (get elf* "dateCreated")
             :source/note "Resolves :company/entity-legal-form-id under ISO 20275."})

      (prov upre retrieved-at
            {:fact/id "gleif-ultimate-parent-reporting-exception"
             :fact/kind :parent-reporting-exception
             :company/lei lei
             :relationship/exception-category (get upr* "category")
             :relationship/exception-reason (get upr* "reason")
             :source/note "Why no ultimate accounting consolidation parent is reported."})]

     ;; Each child cites the page it was actually read from, not the unpaginated
     ;; collection URL. A :source/url this script never fetches would be a
     ;; citation nothing verifies.
     (mapcat (fn [page]
               (map (fn [k]
                      (let [a (get k "attributes")
                            e (get a "entity")]
                        (prov page retrieved-at
                              {:fact/id (str "gleif-direct-child-" (str/lower-case (get a "lei")))
                               :fact/kind :direct-child
                               :company/lei (get a "lei")
                               :company/legal-name (get-in e ["legalName" "name"])
                               :company/jurisdiction (get e "jurisdiction")
                               :company/status (get e "status")
                               :relationship/kind "IS_DIRECTLY_CONSOLIDATED_BY"
                               :relationship/parent-lei lei})))
                    (get-in page [:json "data"])))
             [kids-1 kids-2]))))

;; ---------------------------------------------------------------- emit

(def key-order
  [:fact/id :fact/kind :company/lei :company/legal-name :company/legal-name-language
   :company/jurisdiction :company/status :company/registered-as :company/registration-authority-id
   :company/entity-legal-form-id :company/entity-category :company/creation-date
   :company/legal-address :company/headquarters-address :company/bic :company/isin
   :company/open-corporates-id :company/sp-global-id
   :registration/initial-date :registration/last-update-date :registration/status
   :registration/next-renewal-date :registration/managing-lou-lei
   :registration/corroboration-level :registration/conformity-flag
   :authority/code :authority/international-name :authority/organization-name
   :authority/local-organization-name :authority/website :authority/country
   :elf/code :elf/local-name :elf/language :elf/country-code :elf/status :elf/date-created
   :relationship/kind :relationship/parent-lei :relationship/exception-category
   :relationship/exception-reason
   :source/dataset :source/url :source/http-status :source/retrieved-at
   :source/golden-copy-publish-date :source/note])

(defn fmt-map [m]
  (let [ks (concat (filter #(contains? m %) key-order)
                   (sort-by str (remove (set key-order) (keys m))))]
    (str " {" (str/join "\n  " (map #(str (pr-str %) " " (pr-str (get m %))) ks)) "}")))

(def header
  (str ";; Verified public-registry facts about the legal entity this repository archives.\n"
       ";;\n"
       ";; GENERATED by `nbb scripts/verify-facts.cljs --write`. Do not hand-edit: every\n"
       ";; value below was read out of the HTTP response recorded in :source/url at\n"
       ";; :source/retrieved-at, and `nbb scripts/verify-facts.cljs` re-fetches those\n"
       ";; sources and fails if the live record no longer agrees.\n"
       ";;\n"
       ";; Shape is tx-data (a vector of entity maps), so it loads with\n"
       ";; (d/transact conn (edn/read-string (slurp \"facts.edn\"))) like every other EDN\n"
       ";; corpus in this workspace. :company/lei is the join key -- including on the 29\n"
       ";; :fact/kind :direct-child entities, whose LEIs join to their own records.\n"
       ";;\n"
       ";; NOT on the shared query plane yet. manifest/edn-query.cljs (com-junkawasaki/root)\n"
       ";; has loaders for blueprint.edn and 80-data/public/tos.journal.edn and none for\n"
       ";; this file, so these datoms are readable here but not joinable from edn-query.\n"))

(defn emit [entities]
  (str header "[\n" (str/join "\n\n" (map fmt-map entities)) "]\n"))

;; ---------------------------------------------------------------- compare

(defn stable [m] (apply dissoc m volatile-keys))

(defn compare-entities [recorded live]
  (let [by-id  (fn [xs] (into {} (map (juxt :fact/id identity) xs)))
        r      (by-id recorded)
        l      (by-id live)
        gone   (sort (remove (set (keys l)) (keys r)))
        added  (sort (remove (set (keys r)) (keys l)))
        drift  (for [id (sort (filter (set (keys l)) (keys r)))
                     :let [a (stable (get r id)) b (stable (get l id))]
                     :when (not= a b)
                     k (sort-by str (distinct (concat (keys a) (keys b))))
                     :when (not= (get a k) (get b k))]
                 [id k (get a k) (get b k)])]
    {:gone gone :added added :drift drift}))

;; ---------------------------------------------------------------- main

(defn -main []
  (let [bp-text (slurp* blueprint-path)
        bp      (when bp-text (try (edn/read-string bp-text) (catch :default _ nil)))
        lei     (:company/lei bp)]
    (when-not (string? lei)
      (die! 3 blueprint-path "has no :company/lei -- cannot tell which entity to verify"))

    (let [recorded (when-let [t (slurp* facts-path)]
                     (try (edn/read-string t) (catch :default e (die! 3 facts-path "is not readable EDN:" (str e)))))]
      (when (and (not write?) (not (seq recorded)))
        (die! 3 facts-path "is missing or holds no facts -- there is nothing to verify."
              "Refusing to report a pass. Run with --write to create it."))

      (when (and (seq recorded)
                 (not= lei (:company/lei (first (filter #(= "gleif-lei-record" (:fact/id %)) recorded)))))
        (die! 1 "facts.edn records a different :company/lei than blueprint.edn"))

      (let [api (str "https://api.gleif.org/api/v1/lei-records/" lei)]
        (-> (fetch-seq [api (str api "/isins") (str api "/managing-lou")
                        (str api "/direct-children?page%5Bnumber%5D=1&page%5Bsize%5D=15")
                        (str api "/direct-children?page%5Bnumber%5D=2&page%5Bsize%5D=15")
                        (str api "/ultimate-parent-reporting-exception")])
            (.then
             (fn [[record isins lou kids-1 kids-2 upre]]
               (when-not (= 200 (:status record))
                 (if (:transport-error record)
                   (die! 3 "could not reach GLEIF at all:" (:transport-error record)
                         "-- refusing to report a pass")
                   (die! 1 "GLEIF returned" (:status record) "for" (:url record) "--" (:body-head record))))
               (let [ent (get-in record [:json "data" "attributes" "entity"])
                     ra-id  (get-in ent ["registeredAt" "id"])
                     elf-id (get-in ent ["legalForm" "id"])]
                 (-> (fetch-seq [(str "https://api.gleif.org/api/v1/registration-authorities/" ra-id)
                                 (str "https://api.gleif.org/api/v1/entity-legal-forms/" elf-id)])
                     (.then
                      (fn [[ra elf]]
                        (let [responses [record isins lou kids-1 kids-2 upre ra elf]
                              answered  (remove :transport-error responses)
                              bad       (filter #(and (:status %) (not (<= 200 (:status %) 299))) responses)]

                          (println (str "CHECKED\t" (count answered)))
                          (println (str "ENTITIES\t" (count recorded)))

                          (when (zero? (count answered))
                            (die! 3 "every request failed at the transport level"
                                  "-- cannot tell a dead citation from a dead network."
                                  "Refusing to report a pass."))

                          (when (seq bad)
                            (die! 1 (count bad) "cited source(s) did not answer 2xx:\n"
                                  (str/join "\n" (map #(str "  " (:status %) " " (:url %)
                                                            " -- " (:body-head %)) bad))))

                          (let [live (build {:record record :isins isins :lou lou :ra ra :elf elf
                                             :upre upre :kids-1 kids-1 :kids-2 kids-2}
                                            lei (.toISOString (js/Date.)))]
                            (if write?
                              (do (fs/writeFileSync facts-path (emit live))
                                  (println (str "WROTE\t" (count live) "\t" facts-path))
                                  (js/process.exit 0))
                              (let [{:keys [gone added drift]} (compare-entities recorded live)]
                                (if (and (empty? gone) (empty? added) (empty? drift))
                                  (do (println (str "OK\tall " (count recorded)
                                                    " recorded fact(s) still match the live sources"))
                                      (js/process.exit 0))
                                  (do
                                    (doseq [id gone]  (println (str "GONE\t" id)))
                                    (doseq [id added] (println (str "ADDED\t" id)))
                                    (doseq [[id k was now] drift]
                                      (println (str "DRIFT\t" id "\t" k "\n  recorded: " (pr-str was)
                                                    "\n  live:     " (pr-str now))))
                                    (die! 1 (+ (count gone) (count added) (count drift))
                                          "difference(s) between facts.edn and the live registry."
                                          "Re-run with --write once you have read them.")))))))))))))
            (.catch (fn [e] (die! 3 "verification aborted:" (str e)))))))))

(-main)
