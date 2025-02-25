#! /usr/bin/guile -s
!#
;; file scripts/ontoconsume.scm

;; 1. Load the RDF and spit out an error if it can't.
;; 2. Annotate the RDF with a version number. This should support emitting both a file annotated directly, and a separate file which will also be annotated with endpoint information.
;; 3. Output either the original format or the original format + other syntaxes.

(use-modules (ice-9 getopt-long)
             (ice-9 receive)
             (ice-9 regex)
             (srfi srfi-1)
	     (srfi srfi-9)
             ((rdf xsd)
              #:prefix xsd:)
             (rdf rdf)
             (turtle fromrdf)
             (turtle tordf)
             (iri iri)
             (semver))

(define (load-rdf path base)
  (catch 'system-error
    (lambda ()
      (call-with-input-file path
        (lambda (port)
          (catch 'match-error
            (lambda ()
              (turtle->rdf port base))
            (lambda (key . args)
              (display "Could not parse input as RDF/TTL: ")
              (display path)
              #f)))))
    (lambda (key . args)
      (display "Non-existent input file: ")
      (display path)
      #f)))

(define skos-prefix "http://www.w3.org/2004/02/skos/core#")
(define endpoint-prefix "http://ontology.spyderisk.org/ns/endpoint#")
(define trix-prefix "http://www.w3.org/2004/03/trix/rdfg-1/")
(define dcterms-prefix "http://purl.org/dc/terms/")
(define shex-prefix "http://www.w3.org/ns/shex#")
(define version-statements
  (map
   (lambda (ref)
     (string-append endpoint-prefix ref))
   (list "Version"
         "major_component" "minor_component" "patch_component"
         "recorded_as" "string_representation"
         "see_previous" "see_also"
         "valid_from" "valid_to"
         "applies_to" "applies_also"
         "has_version" "see_history")))

(define annotated-prefixes
  (append `(("send" . ,endpoint-prefix)
            ("rdfg" . ,trix-prefix)
            ("skos" . ,skos-prefix)
            ("dct"  . ,dcterms-prefix)
            ("shex" . ,shex-prefix))
          common-prefixes))

(define-record-type <version-annotation>
  (version-annotation major-component minor-component patch-component
		      recorded-as see-previous see-also
		      valid-from valid-to
		      applies-to applies-also)
  version-annotation?
  (major-component version-annotation-major-component set-major-component!)
  (minor-component version-annotation-minor-component set-minor-component!)
  (patch-component version-annotation-patch-component set-patch-component!)
  (recorded-as version-annotation-recorded-as set-version-recorded-as!)
  (see-previous version-annotation-see-previous set-version-previous!)
  (see-also version-annotation-see-also set-version-also!)
  (valid-from version-annotation-valid-from set-valid-from!)
  (valid-to version-annotation-valid-to set-valid-to!)
  (applies-to version-annotation-applies-to set-against-graph!)
  (applies-also version-annotation-applies-also set-related-graphs!))

(define (parse-version combined-version)
  (map string->number (string-split combined-version #\.)))

(define (extract-version anno)
  (make-version (version-annotation-major-component anno)
                (version-annotation-minor-component anno)
                (version-annotation-patch-component anno)))

(define (make-version major minor patch)
  (string-append (number->string major) "."
		 (number->string minor) "."
		 (number->string patch)))

(define (compare-versions v0 v1)
  (let ((sem0 (string->semver (extract-version v0)))
        (sem1 (string->semver (extract-version v1))))
    (semver>? sem0 sem1)))

(define (make-check-re rs)
  (lambda (s)
    (let ((re (make-regexp rs)))
      (let ((mo (regexp-exec re s)))
        (and mo (match:substring mo))))))

(define check-version
  (make-check-re "^[0-9]+.[0-9]+.[0-9]+$"))

(define (make-empty-version-annotation)
  (version-annotation #f #f #f
		      #f #f #f
		      #f #f
		      #f #f))

(define (from-date dat fb)
  (with-exception-handler
    (lambda (ex)
      (display
       "Time-stamp should be of form YYYY-MM-DD, components numeric")
      fb)
    (lambda ()
      (car
       (mktime
        (car
         (strptime "%F" dat)))))
    #:unwind? #t))

(define (to-date epoch fb)
  (if (and (integer? epoch) (exact? epoch))
      (strftime "%F" (localtime epoch))
      (let ()
        (display "Epoch must be an exact number.")
        fb)))

(define* (emit-versioning curr delim versioning
                          #:key (previous-version-string #f)
                                (history? #f)
                                (recorded-in #f))
  (let* ((current-version-string
          (extract-version versioning))
         (versioning-maybe-bn
          (if (and history? recorded-in)
              (string-append recorded-in
                             delim
                             current-version-string)       
              (random 10000)))
         (make-rdf-iri (lambda (name)
           (string-append "http://www.w3.org/1999/02/22-rdf-syntax-ns#" name)))
         (make-endpoint-iri (lambda (name)
           (string-append endpoint-prefix name)))
         (graph-subject (string-append curr delim current-version-string))
         (make-xsd (lambda (iri)
                     (string-append "http://www.w3.org/2001/XMLSchema#"
                                    iri)))
         (get-with (lambda (ref proc)
           (let ((maybe-val (proc versioning)))
            (and maybe-val
                 (make-rdf-triple versioning-maybe-bn
                                  (make-endpoint-iri ref)
                                  (make-rdf-literal
                                   (number->string maybe-val)
                                   (make-xsd "integer")
                                   #f))))))
         (get-with-ts (lambda (ref proc)
           (let ((maybe-epoch (proc versioning)))
             (and maybe-epoch
                  (make-rdf-triple versioning-maybe-bn
                                   (make-endpoint-iri ref)
                                   (make-rdf-literal
                                    (to-date maybe-epoch #f)
                                    (make-xsd "date")
                                    #f))))))
         (get-with-sg (lambda (ref proc)
           (let ((maybe-opaque (proc versioning)))
             (and maybe-opaque
                  (make-rdf-triple versioning-maybe-bn
                                   (make-endpoint-iri ref)
                                   maybe-opaque))))))
    (reverse
     (filter (lambda (k) (and k))
            (append
      (list
       (make-rdf-triple graph-subject (make-rdf-iri "type") (string-append trix-prefix "Graph"))
       (make-rdf-triple graph-subject (make-endpoint-iri "has_version") versioning-maybe-bn)
       (and recorded-in (make-rdf-triple graph-subject (make-endpoint-iri "see_history") recorded-in))
       (make-rdf-triple versioning-maybe-bn (make-rdf-iri "type") (make-endpoint-iri "Version"))
       (get-with "major_component" version-annotation-major-component)
       (get-with "minor_component" version-annotation-minor-component)
       (get-with "patch_component" version-annotation-patch-component)
       (get-with-sg "recorded_as" version-annotation-recorded-as)
       (get-with-ts "valid_from" version-annotation-valid-from)
       (get-with-ts "valid_to"   version-annotation-valid-to)
       (get-with-sg "applies_to" version-annotation-applies-to)
       (get-with-sg "see_previous" (lambda (vsn)
                                     (and recorded-in previous-version-string
                                          (string-append recorded-in
                                                         delim
                                                         previous-version-string))))))))))

;; Core assumption is that, given a *base* IRI, this refers to the *current*
;; set of statements. This would be clear in the following:
;;
;; <https://w3id.org/example> rdf:type owl:Ontology ;
;;   owl:versionIRI <https://w3id.org/example/1.0.0> ;
;;   owl:versionInfo "1.0.0" .
;;
;; Here, we're presented with statements like:
;; <https://w3id.org/example> owl:versionIRI <Some IRI>
;; <https://w3id.org/example> owl:versionInfo <Some IRI>
;;
;; This would be what we might encounter in a standalone Turtle file this
;; program is fed. (It would be the same 'base' argument as above, otherwise
;; we have no clue what these statements refer to.)
;;
;; On the individual graph level, the shape of things to come (apologies to Mr Coleman) is like:
;;
;; <http://ontology.spyderisk.org/ns/core/0.1.0> rdf:type rdfg:Graph ;
;;    send:has_version _:b1 ;
;;    send:has_version <http://ontology.spyderisk.org/v/core#0.1.0> ;
;;    send:has_version_history <http://ontology.spyderisk.org/v/core> .
;;
;; _:b1 a send:Version ; 
;;       send:version_represented_as "0.1.0" ;
;;       send:major_component "0"^^xsd:integer ;
;;       send:minor_component "1"^^xsd:integer ;
;;       send:patch_component "0"^^xsd:integer ;
;;       send:see_previous <http://ontology.spyderisk.org/v/core#0.0.9> ;
;;       send:valid_from "2025-01-01"^^xsd:date ;
;;       send:valid_to "2025-01-16"^^xsd:date ;
;;       send:applies_to <http://ontology.spyderisk.org/ns/core> .

(define-syntax divert
  (lambda (stx)
    (syntax-case stx ()
      [(divert proc-reslv pred obj ([ref0 proc-div0] ...))
       #'(cond [(equal? pred (proc-reslv ref0))
                (proc-div0 obj)]
               ...
               [else #f])])))

;; Start by defining a function to take a set of statements and gather them up.
;; So we expect that we're dealing with the blank node in the example above.
(define (coalesce-versioning statements)
  (let* ((target-map (make-empty-version-annotation))
         (target-major #f)
         (target-minor #f)
         (target-patch #f)
         (rejected '())
         (rslv (lambda (ref)
           (string-append endpoint-prefix ref)))
         (proc (lambda (lit)
           (string->number
            (rdf-literal-lexical-form lit)))))
    (letrec ((statement-helper
      (lambda (stmt)
        (let ((subj (rdf-triple-subject stmt))
              (pred (rdf-triple-predicate stmt))
              (obj (rdf-triple-object stmt)))
          (divert rslv pred obj
                  (["major_component" (lambda (a)
                     (set! target-major (proc a)))]
                   ["minor_component" (lambda (a)
                     (set! target-minor (proc a)))]
                   ["patch_component" (lambda (a)
                     (set! target-patch (proc a)))]
                   ["valid_from" (lambda (a)
                     (set-valid-from!
                      target-map
                      (from-date
                       (rdf-literal-lexical-form a) #f)))]
                   ["valid_to" (lambda (a)
                     (set-valid-to!
                      target-map
                      (from-date
                       (rdf-literal-lexical-form a) #f)))]
                   ["recorded_as" (lambda (a)
                     (set-version-recorded-as!
                      target-map
                      a))] ;; not a literal, it's an opaque IRI
                   ))))))
      (for-each statement-helper statements))
    (set-major-component! target-map target-major)
    (set-minor-component! target-map target-minor)
    (set-patch-component! target-map target-patch)
    target-map))

;; 1. Start by finding the version associated with the current document.
;;    This might well be some blank node, but it doesn't have to be.
;;    Statements like: <this graph> send:hasVersion <vsn>
;; 2. Query the graph for the statements associated with the version.
;; 3. Check it matches the current one.

(define* (hash-versioning-scheme graph curr delim target-version-string
                                 #:key (history? #f)
                                       (version-log #f)
                                       (valid-from #f)
                                       (valid-to #f))
  (let ((base (string-append curr delim)))
    (receive (vss nvss)
        (partition (lambda (st)
                     (or
                      ;; most version statements
                      (member (rdf-triple-predicate st)
                              version-statements)
                      ;; blank nodes, `_b0 a send:Version' &c.
                      (member (rdf-triple-object st)
                              version-statements)
                      ;; remaining old version statements:
                      ;; these are composed of base + valid version string,
                      ;; can't reasonably detect these if not of this form
                      (let ((subj (rdf-triple-subject st)))
                        (and
                         (string-prefix? base subj)
                         (check-version
                          (string-drop subj
                                       (string-prefix-length base subj)))))))
                   graph)
      (let* ((recorded-as
              (and version-log
                   (string-append version-log delim target-version-string)))
             (parsed-version (parse-version target-version-string))
             (major (car parsed-version))
             (minor (cadr parsed-version))
             (patch (caddr parsed-version))
             (target-versions
              (map rdf-triple-object
                   (filter (lambda (stmt)
                             (equal?
                              (rdf-triple-predicate stmt)
                                (string-append endpoint-prefix "has_version")))
                           vss)))
             (found-in-graph
              (map (lambda (vsn)
                     (coalesce-versioning
                      (filter (lambda (stmt)
                                (equal? vsn
                                        (rdf-triple-subject stmt)))
                              vss)))
                   target-versions))
             (target-new
              (version-annotation major minor patch
                                  recorded-as #f #f
                                  valid-from valid-to
                                  #f #f)))
          (cond [(null? found-in-graph)
                 (append graph
                         (emit-versioning curr delim target-new
                                          #:history? history?
                                          #:recorded-in version-log))]
                [else
                 (let* ((ordered-versions (sort-list found-in-graph compare-versions))
                        (nominal-latest (car ordered-versions))
                        (apparent-previous (extract-version nominal-latest)))
                   (cond [(and (compare-versions target-new nominal-latest)
                               history?)
                          ;; We also have to reckon with the previous latest!
                          ;; If the valid-to statement doesn't exist (as
                          ;; we'd expect), edit it to be the new valid-from
                          ;; statement -1. Otherwise, assume it still holds.
                          (if (not (version-annotation-valid-to nominal-latest))
                                (set-valid-to! nominal-latest
                                               (- valid-from (* 24 60 60)))
                                ;;(display "Looks like there was no valid-to in the nominal previous! Setting it to 1 day prior to new version's valid-from. ")
                                )
                          ;; (display "Requested version is newer than nominal latest, and this graph records history. Appending the requested version to this graph.")
                          ;; (newline)
                            (append nvss
                                    (apply append
                                           (cons
                                            (emit-versioning curr delim target-new
                                                             #:previous-version-string apparent-previous
                                                             #:history? history?
                                                             #:recorded-in version-log)
                                            (map (lambda (vsn)
                                                   (emit-versioning curr delim vsn
                                                                    #:history? history?
                                                                    #:recorded-in version-log))
                                                       (cons nominal-latest
                                                             (cdr ordered-versions))))))]
                         [(compare-versions target-new nominal-latest)
                          (let ()
                            ;;(display "Requested version is newer than nominal latest, but this graph does not record history. An external version log is provided, so use that. Remove existing versioning statements and annotate with requested.")
                            ;;(newline)
                            (append nvss (emit-versioning curr delim target-new
                                                          #:previous-version-string apparent-previous
                                                          #:history? history?
                                                          #:recorded-in version-log)))] ;; non-versioning statements
                         [else
                          (let ()
                            ;; (display "Requested version is less than latest transcribed in the file. Not overwriting.")
                            graph)]))])))))
           
(let* ((option-spec
	'((target-version (single-char #\V) (value #t))
          (curr (single-char #\b) (value #t))
          (delim (single-char #\d) (value #t))
          (hist (single-char #\l) (value #t))
          (curr-prefix (single-char #\p) (value #t))
          (hist-prefix (single-char #\P) (value #t))
          (vocabulary (single-char #\U) (value #t))
          (provenance (single-char #\L) (value #t))
          (valid-from (single-char #\T) (value #t))
	  (help (single-char #\h) (value #f))))
       (help-msg "ontoconsume [options]
  -V --target-version VSN   Use target version VSN
  -b --curr IRI             Use IRI as graph URI
  -d --delim CHAR           Append CHAR to graph URI to form base
  -l --versions IRI         IRI of external version history (some RDF graph)
  -p --curr-prefix          Short name for regular RDF graph prefix
  -P --hist-prefix          Short name for history RDF graph prefix
  -U --update-vocab FILE    Update versioning in RDF graph FILE, a vocabulary
  -L --update-log   FILE    Append to RDF graph FILE, a log of version history
  -T --valid-from DATE      Version valid from DATE, not current date
  -h --help                 Display this help message
")
       (options (getopt-long (command-line) option-spec))
       (target-version (option-ref options 'target-version #f))
       (curr-iri (option-ref options 'curr #f))
       (hist-iri (option-ref options 'hist #f))
       (delim (option-ref options 'delim #f))
       (curr-prefix (option-ref options 'curr-prefix #f))
       (hist-prefix (option-ref options 'hist-prefix #f))
       (work-on-vocabulary (option-ref options 'vocabulary #f))
       (work-on-history (option-ref options 'provenance #f))
       (valid-from (option-ref options 'valid-from #f))
       (dry-run (option-ref options 'dry-run #f))
       (help (option-ref options 'help #f)))
  (cond [help (display help-msg)]
        [(and work-on-vocabulary work-on-history)
         (display "-U/--update-vocab and -L/--update-log are mutually exclusive options!")
         (newline) (newline) ;; double newline so it's obvious
         (display help-msg)]
        [(not target-version)
         (display "Did not specify a target version!")
         (newline) (newline) ;;
         (display help-msg)]
        [(not (or work-on-vocabulary work-on-history))
         (display "No vocabulary or history file to version supplied!")
         (newline) (newline) ;;
         (display help-msg)]
        [(and work-on-history (not hist-iri))
         (display "When appending to version log, need an IRI for the version log")]
        [(not (check-version target-version))
         (display "Version should be of form MAJOR.MINOR.PATCH, components numeric")]
        [(not delim)
         (display "Must specify delimiter used to form base")]
        [(not curr-iri)
         (display "No graph IRI supplied!")]
        [else
         ;; don't fall back to current epoch UNLESS valid-from is not given!
         (let* ((epoch-proper 
                 (if valid-from
                     (from-date valid-from #f)
                     (current-time)))
                (curr-prefix-proper
                 (and curr-prefix
                      `(,curr-prefix . ,(string-append curr-iri delim))))
                (hist-prefix-proper
                 (and hist-prefix
                      `(,hist-prefix . ,(string-append hist-iri delim))))
                (annotated-prefixes+
                 (cond [(and curr-prefix hist-prefix)
                        (append `(,curr-prefix-proper ,hist-prefix-proper) annotated-prefixes)]
                       [curr-prefix
                        (cons curr-prefix-proper annotated-prefixes)]
                       [hist-prefix
                        (cons hist-prefix-proper annotated-prefixes)]
                       [else annotated-prefixes])))
           (and epoch-proper                
                (let ((target-graph
                       (if work-on-vocabulary
                           (load-rdf work-on-vocabulary curr-iri)
                           (load-rdf work-on-history curr-iri))))
                  (if target-graph
                      (display
                          (rdf->turtle
                           (hash-versioning-scheme target-graph
                                                   curr-iri
                                                   delim
                                                   target-version
                                                   #:history? work-on-history
                                                   #:valid-from epoch-proper
                                                   #:version-log hist-iri)
                           #:prefixes annotated-prefixes+))))))]))
(newline)
