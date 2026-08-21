#!/usr/bin/env nbb
;; VENDORED from com-junkawasaki/root (scripts/lei-verify-facts.cljs), root
;; pinned at 6846befd962542757e2decdc95895dcb78956f1b. Do not edit here.
;;
;; This copy is deliberate, not an oversight. What makes this repository an
;; archive is that a single `git clone` of it can be checked against the live
;; registry with no workspace, no west, and no dependency resolution. Depending
;; on a shared file would take that away, so the file is copied instead.
;;
;; The obligation that creates -- that the copy still matches what it names --
;; is checked by `scripts/verify-vendored-copies.cljs` in the superproject,
;; which diffs from the `(ns` form down, so this header is not counted as
;; drift. Fix anything wrong here in the canonical and re-vendor; an edit made
;; only here is a fork, and it will be reported as one.
;;
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
;;   0  every cited URL answered, and every recorded fact still matches
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

;; A reporting exception is a real answer when it is present and a real answer
;; when it is absent -- GLEIF returns 404 for "this entity reports no exception
;; of that category". A 404 here is therefore not a broken citation. It still
;; cannot pass silently: the entity simply stops being emitted, and the
;; recorded-vs-live comparison reports it as GONE, which exits 1.
(def optional-404
  #{"direct-parent-reporting-exception" "ultimate-parent-reporting-exception"})

(defn optional-404? [url]
  (boolean (some #(str/ends-with? url %) optional-404)))

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

(defn present? [r] (= 200 (:status r)))

(defn pagination [r] (get-in r [:json "meta" "pagination"]))

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

(defn exception-fact [r retrieved-at lei id]
  (when (present? r)
    (let [a (get-in r [:json "data" "attributes"])]
      (prov r retrieved-at
            {:fact/id id
             :fact/kind :parent-reporting-exception
             :company/lei lei
             :relationship/exception-category (get a "category")
             :relationship/exception-reason (get a "reason")
             :source/note "Why no parent of this category is reported."}))))

(defn build
  "The single definition of what facts.edn contains. --write emits it, the
   default mode rebuilds it from the live sources and diffs. Both modes go
   through here, so the file cannot drift from its own generator."
  [{:keys [record isins lou issuer ra elf dpre upre kid-pages]} lei retrieved-at]
  (let [rec  (get-in record [:json "data" "attributes"])
        ent  (get rec "entity")
        reg  (get rec "registration")
        lou* (get-in lou [:json "data" "attributes"])
        iss* (get-in issuer [:json "data" "attributes"])
        ra*  (get-in ra  [:json "data" "attributes"])
        elf* (get-in elf [:json "data" "attributes"])
        isin-page (pagination isins)
        kid-page  (pagination (first kid-pages))
        ;; Mirror the instrument identifiers only when they all fit in the single
        ;; page this script actually fetched. Above that, record the count alone:
        ;; at a large issuer's volume the list turns over as instruments mature
        ;; and are issued, which would keep this check red for reasons that are
        ;; not "the citation broke". Which branch was taken is stated in the
        ;; :source/note below, so a bare count is never ambiguous between "too
        ;; many to mirror" and "nobody looked".
        isins-mirrored? (and (present? isins)
                             (<= (or (get isin-page "lastPage") 1) 1))]
    (into
     (filterv
      some?
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
              :company/open-corporates-id (get rec "ocid")
              :company/sp-global-id (vec (get rec "spglobal"))
              :registration/initial-date (get reg "initialRegistrationDate")
              :registration/last-update-date (get reg "lastUpdateDate")
              :registration/status (get reg "status")
              :registration/next-renewal-date (get reg "nextRenewalDate")
              :registration/managing-lou-lei (get reg "managingLou")
              :registration/corroboration-level (get reg "corroborationLevel")
              :registration/conformity-flag (get rec "conformityFlag")
              :source/note (str "Entity status and registration status are two different "
                                "fields and are recorded separately: an ACTIVE company can "
                                "hold a LAPSED LEI registration.")})

       ;; The ISIN list is NOT mirrored here, and that is a decision, not an
       ;; omission -- see :source/note. Recording only the count keeps this
       ;; check red for "the registry changed" and not for "a bond matured".
       (prov isins retrieved-at
             {:fact/id "gleif-isins"
              :fact/kind :securities
              :company/lei lei
              :securities/isin-count (get isin-page "total")
              :securities/page-size (get isin-page "perPage")
              :securities/page-count (get isin-page "lastPage")
              :source/note (if isins-mirrored?
                             (str "Count of instrument identifiers GLEIF maps to this LEI, read "
                                  "from meta.pagination.total of the cited page. The whole list "
                                  "fits in that single page, so each identifier is also recorded "
                                  "below as its own :fact/kind :security entity. This is a "
                                  "count, not a share count.")
                             (str "Count of instrument identifiers GLEIF maps to this LEI, read "
                                  "from meta.pagination.total of the cited page. The individual "
                                  "ISINs are deliberately not mirrored into this repository: at "
                                  "this issuer's volume they turn over as instruments mature and "
                                  "are issued, which would make this check red for reasons that "
                                  "are not 'the citation broke'. Walk the cited URL's page range "
                                  "to enumerate them. This is a count, not a share count."))})

       (prov lou retrieved-at
             {:fact/id "gleif-managing-lou"
              :fact/kind :lei-issuer
              :company/lei lei
              :registration/managing-lou-lei (get lou* "lei")
              :company/legal-name (get-in lou* ["entity" "legalName" "name"])
              :company/jurisdiction (get-in lou* ["entity" "jurisdiction"])
              :source/note "The Local Operating Unit that issues and maintains this LEI."})

       (prov issuer retrieved-at
             {:fact/id "gleif-lei-issuer-accreditation"
              :fact/kind :lei-issuer-accreditation
              :company/lei lei
              :registration/managing-lou-lei (get iss* "lei")
              :issuer/name (get iss* "name")
              :issuer/marketing-name (get iss* "marketingName")
              :issuer/website (get iss* "website")
              :issuer/accreditation-date (get iss* "accreditationDate")
              :source/note "GLEIF's accreditation of the LOU named above, as an issuer of LEIs."})

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
              :authority/jurisdiction (get-in ra* ["jurisdictions" 0 "jurisdiction"])
              :company/registered-as (get ent "registeredAs")
              :source/note (if (= "RA999999" (get ra* "code"))
                             (str "RA999999 is GLEIF's placeholder for \"no registration authority "
                                  "available\": this record is not corroborated against a national "
                                  "business register, which is why :company/registered-as is nil.")
                             "Resolves :company/registration-authority-id to the national register that corroborated the record.")})

       (prov elf retrieved-at
             {:fact/id "iso-20275-entity-legal-form"
              :fact/kind :entity-legal-form
              :company/lei lei
              :elf/code (get elf* "code")
              :elf/local-name (get-in elf* ["names" 0 "localName"])
              :elf/language (get-in elf* ["names" 0 "languageCode"])
              :elf/country-code (get elf* "countryCode")
              :elf/subdivision-code (get elf* "subdivisionCode")
              :elf/status (get elf* "status")
              :elf/date-created (get elf* "dateCreated")
              :source/note "Resolves :company/entity-legal-form-id under ISO 20275."})

       (exception-fact dpre retrieved-at lei "gleif-direct-parent-reporting-exception")
       (exception-fact upre retrieved-at lei "gleif-ultimate-parent-reporting-exception")

       ;; A measured zero. Without this entity, "GLEIF lists no direct children"
       ;; and "nobody asked GLEIF about children" would look identical in
       ;; facts.edn -- and a child appearing later would go unnoticed.
       (prov (first kid-pages) retrieved-at
             {:fact/id "gleif-direct-children-count"
              :fact/kind :direct-children-summary
              :company/lei lei
              :relationship/direct-child-count (get kid-page "total")
              :source/note (str "How many entities GLEIF records as directly consolidated by "
                                "this one, read from meta.pagination.total. Zero here is a "
                                "measured zero, not an unasked question. Each child, if any, "
                                "is recorded as its own :fact/kind :direct-child entity below.")})])

     (concat
      ;; One entity per instrument identifier, each citing the page it was read
      ;; from. Emitted only on the branch chosen above; when the list is too long
      ;; to mirror, the count entity says so rather than standing alone.
      (when isins-mirrored?
        (map (fn [i]
               (let [a (get i "attributes")]
                 (prov isins retrieved-at
                       {:fact/id (str "gleif-isin-" (str/lower-case (get a "isin")))
                        :fact/kind :security
                        :company/lei (get a "lei")
                        :securities/isin (get a "isin")
                        :source/note "An instrument identifier GLEIF maps to this LEI."})))
             (get-in isins [:json "data"])))

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
              kid-pages)))))

;; ---------------------------------------------------------------- emit

(def key-order
  [:fact/id :fact/kind :company/lei :company/legal-name :company/legal-name-language
   :company/jurisdiction :company/status :company/registered-as :company/registration-authority-id
   :company/entity-legal-form-id :company/entity-category :company/creation-date
   :company/legal-address :company/headquarters-address :company/bic
   :company/open-corporates-id :company/sp-global-id
   :registration/initial-date :registration/last-update-date :registration/status
   :registration/next-renewal-date :registration/managing-lou-lei
   :registration/corroboration-level :registration/conformity-flag
   :securities/isin :securities/isin-count :securities/page-size :securities/page-count
   :issuer/name :issuer/marketing-name :issuer/website :issuer/accreditation-date
   :authority/code :authority/international-name :authority/organization-name
   :authority/local-organization-name :authority/website :authority/country
   :authority/jurisdiction
   :elf/code :elf/local-name :elf/language :elf/country-code :elf/subdivision-code
   :elf/status :elf/date-created
   :relationship/kind :relationship/parent-lei :relationship/direct-child-count
   :relationship/exception-category :relationship/exception-reason
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
       ";; corpus in this workspace. :company/lei is the join key.\n"
       ";;\n"
       ";; The two counts here -- :securities/isin-count and\n"
       ";; :relationship/direct-child-count -- are read from meta.pagination.total of a\n"
       ";; page this script actually fetched. Each one's :source/note says whether the\n"
       ";; list underneath it was also mirrored into this file or only counted, so a\n"
       ";; bare count is never ambiguous. A zero here is a measured zero.\n"
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

(defn child-page-url [api n]
  (str api "/direct-children?page%5Bnumber%5D=" n "&page%5Bsize%5D=15"))

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
        (-> (fetch-seq [api
                        (str api "/isins")
                        (str api "/managing-lou")
                        (str api "/lei-issuer")
                        (str api "/direct-parent-reporting-exception")
                        (str api "/ultimate-parent-reporting-exception")
                        (child-page-url api 1)])
            (.then
             (fn [[record isins lou issuer dpre upre kid-1]]
               (when-not (present? record)
                 (if (:transport-error record)
                   (die! 3 "could not reach GLEIF at all:" (:transport-error record)
                         "-- refusing to report a pass")
                   (die! 1 "GLEIF returned" (:status record) "for" (:url record) "--" (:body-head record))))
               (let [ent    (get-in record [:json "data" "attributes" "entity"])
                     ra-id  (get-in ent ["registeredAt" "id"])
                     elf-id (get-in ent ["legalForm" "id"])
                     ;; Walk however many child pages the registry says exist. A
                     ;; hard-coded page count would silently truncate the day a
                     ;; sixteenth child appears.
                     kid-last (if (present? kid-1)
                                (or (get (pagination kid-1) "lastPage") 1)
                                1)
                     kid-rest (map #(child-page-url api %) (range 2 (inc kid-last)))]
                 (-> (fetch-seq (concat [(str "https://api.gleif.org/api/v1/registration-authorities/" ra-id)
                                         (str "https://api.gleif.org/api/v1/entity-legal-forms/" elf-id)]
                                        kid-rest))
                     (.then
                      (fn [[ra elf & kid-more]]
                        (let [kid-pages (into [kid-1] kid-more)
                              responses (into [record isins lou issuer dpre upre ra elf] kid-pages)
                              answered  (remove :transport-error responses)
                              bad       (filter #(and (:status %)
                                                      (not (<= 200 (:status %) 299))
                                                      (not (and (= 404 (:status %)) (optional-404? (:url %)))))
                                                responses)
                              absent    (filter #(and (= 404 (:status %)) (optional-404? (:url %))) responses)]

                          (println (str "CHECKED\t" (count answered)))
                          (println (str "ENTITIES\t" (count recorded)))
                          (doseq [r absent]
                            (println (str "NO-EXCEPTION\t" (:url r)
                                          "\t404 -- GLEIF records no exception of this category")))

                          (when (zero? (count answered))
                            (die! 3 "every request failed at the transport level"
                                  "-- cannot tell a dead citation from a dead network."
                                  "Refusing to report a pass."))

                          (when (seq bad)
                            (die! 1 (count bad) "cited source(s) did not answer 2xx:\n"
                                  (str/join "\n" (map #(str "  " (:status %) " " (:url %)
                                                            " -- " (:body-head %)) bad))))

                          (let [live (build {:record record :isins isins :lou lou :issuer issuer
                                             :ra ra :elf elf :dpre dpre :upre upre
                                             :kid-pages kid-pages}
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
