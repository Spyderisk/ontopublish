#! /usr/bin/guile3.0 -s
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
             (iri iri))

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
              (display path) (newline)
              #f)))))
    (lambda (key . args)
      (display "Non-existent input file: ")
      (display path) (newline)
      #f)))

(define endpoint "http://ontology.spyderisk.org/ns/endpoint#")
(define version-statements
  (map
   (lambda (ref)
     (string-append endpoint ref))
   (list "Version"
         "major_component" "minor_component" "patch_component"
         "version_represented_as" "version_history"
         "see_previous" "see_also"
         "valid_from" "valid_to"
         "applies_to" "applies_also"
         "has_version" "has_version_history")))

(define-record-type <version-annotation>
  (version-annotation major-component minor-component patch-component
		      history see-previous see-also
		      valid-from valid-to
		      applies-to applies-also)
  version-annotation?
  (major-component version-annotation-major-component set-major-component!)
  (minor-component version-annotation-minor-component set-minor-component!)
  (patch-component version-annotation-patch-component set-patch-component!)
  (history version-annotation-history set-version-history!)
  (see-previous version-annotation-see-previous set-version-previous!)
  (see-also version-annotation-see-also set-version-also!)
  (valid-from version-annotation-valid-from set-valid-from!)
  (valid-to version-annotation-valid-to set-valid-to!)
  (applies-to version-annotation-applies-to set-against-graph!)
  (applies-also version-annotation-applies-also set-related-graphs!))


(define (parse-version combined-version)
  (map string->number (string-split combined-version #\.)))

(define (make-check-re rs)
  (lambda (s)
    (let ((re (make-regexp rs)))
      (let ((mo (regexp-exec re s)))
        (and mo (match:substring mo))))))

(define check-version
  (make-check-re "^[0-9]+.[0-9]+.[0-9]+$"))

(define check-date
  (make-check-re "^[0-9]{4}-[0-9]{2}-[0-9]{2}$"))

(define (from-date dat fb)
  (if (check-date dat)
      (car
       (mktime
        (car
         (strptime "%F" dat))))
      (begin 
        (display "Time-stamp should be of form YYYY-MM-DD, components numeric")
        fb)))

(define (to-date epoch fb)
  (if (and (integer? epoch) (exact? epoch))
      (strftime "%F" (localtime epoch))
      (begin
        (display "Epoch must be an exact number.")
        fb)))

(define (emit-versioning base versioning)
  (let* ((make-rdf-iri (lambda (name)
           (string-append "http://www.w3.org/1999/02/22-rdf-syntax-ns#" name)))
         (make-endpoint-iri (lambda (name)
           (string-append endpoint name)))
         (subject
          (string-append base
                         (make-version
                          (version-annotation-major-component versioning)
                          (version-annotation-minor-component versioning)
                          (version-annotation-patch-component versioning))))
         (make-xsd (lambda (iri)
                     (string-append "http://www.w3.org/2001/XMLSchema#"
                                    iri)))
         (get-with (lambda (ref proc)
           (let ((maybe-val (proc versioning)))
            (and maybe-val
                 (make-rdf-triple subject
                                  (make-endpoint-iri ref)
                                  (make-rdf-literal
                                   (number->string maybe-val)
                                   (make-xsd "integer")
                                   #f))))))
         (get-with-ts (lambda (ref proc)
           (let ((maybe-epoch (proc versioning)))
             (and maybe-epoch
                  (make-rdf-triple subject
                                   (make-endpoint-iri ref)
                                   (make-rdf-literal
                                    (to-date maybe-epoch #f)
                                    (make-xsd "date")
                                    #f))))))
         (get-with-sg (lambda (ref proc)
           (let ((maybe-opaque (proc versioning)))
             (and maybe-opaque
                  (make-rdf-triple subject
                                   (make-endpoint-iri ref)
                                   maybe-opaque))))))
    (filter (lambda (k) (and k))
            (list
     (get-with "major_component" version-annotation-major-component)
     (get-with "minor_component" version-annotation-minor-component)
     (get-with "patch_component" version-annotation-patch-component)
     (get-with-sg "version_history" version-annotation-history)
     (get-with-sg "see_previous" version-annotation-see-previous)
     (get-with-ts "valid-from" version-annotation-valid-from)
     (get-with-ts "valid-to"   version-annotation-valid-to)
     (get-with-sg "applies-to" version-annotation-applies-to)
     (make-rdf-triple subject (make-rdf-iri "type") (make-endpoint-iri "Version"))))))

;; If they have the same major version, proceed to checking the minor version. GTEq 0 1 does not hold.
;; If they have the same minor version, proceed to checking the patch version.
;; If they have the same patch version, then GTEq does hold.

(define (compare-versions version0 version1)
  (let* ((major0 (version-annotation-major-component version0))
         (minor0 (version-annotation-minor-component version0))
         (patch0 (version-annotation-patch-component version0))
         (major1 (version-annotation-major-component version1))
         (minor1 (version-annotation-minor-component version1))
         (patch1 (version-annotation-patch-component version1)))
    (cond ((not (and major0 minor0 patch0 major1 minor1 patch1)) #f)
          ((and (equal? major0 major1)
               (equal? minor0 minor1)
               (equal? patch0 patch1)) #t)
          ((> major0 major1) #t)
          ((> minor0 minor1) #t)
	  ((> patch0 patch1) #t)
	  (else #f))))

(define (make-empty-version-annotation)
  (version-annotation #f #f #f
		      #f #f #f
		      #f #f
		      #f #f))

(define (make-initial-annotation M m p vf vt)
  (version-annotation M m p
                      #f #f #f
                      vf vt
                      #f #f))

(define (make-version major minor patch)
  (string-append (number->string major) "."
		 (number->string minor) "."
		 (number->string patch)))

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
           (string-append endpoint ref)))
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
                   ["version_history" (lambda (a)
                     (set-version-history! target-map a))]))))))
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

(define* (hash-versioning-scheme graph base target-version-string
                                 #:key (history? #f)
                                       (version-log #f)
                                       (valid-from #f)
                                       (valid-to #f))
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
    (let* ((parsed-version (parse-version target-version-string))
           (major (car parsed-version))
           (minor (cadr parsed-version))
           (patch (caddr parsed-version))
           (target-new (make-initial-annotation major minor patch
                                                valid-from valid-to)))
      ;; Find the matching versions
      (let* ((target-versions
              (map rdf-triple-object
                   (filter (lambda (stmt)
                             (equal?
                              (rdf-triple-predicate stmt)
                              (string-append endpoint "has_version")))
                           vss)))
             (found-in-graph
              (map (lambda (vsn)
                     (coalesce-versioning
                      (filter (lambda (stmt)
                                (equal? vsn
                                        (rdf-triple-subject stmt)))
                              vss)))
                   target-versions)))
        (cond [(and (null? found-in-graph) version-log)
               (let ((history-triple
                      (make-rdf-triple
                       (string-append base target-version-string)
                       (string-append endpoint "version_history")
                       version-log)))
                 ;(display
                 ; "Looks like it's not annotated yet.")
                 (append graph
                         (cons history-triple
                               (emit-versioning base target-new))))]
              [(null? found-in-graph)
               (begin
                 ;(display
                 ; "Looks like it's not annotated yet.")
                 (append graph
                         (emit-versioning base target-new)))]
              [else
               (let* ((ordered-versions (sort-list found-in-graph compare-versions))
                      (nominal-latest (car ordered-versions)))
                 (cond [(and (compare-versions target-new nominal-latest)
                             history?)
                        ;; We also have to reckon with the previous latest!
                        ;; If the valid-to statement doesn't exist (as
                        ;; we'd expect), edit it to be the new valid-from
                        ;; statement -1. Otherwise, assume it still holds.
                        (begin
                          (if (not (version-annotation-valid-to nominal-latest))                  
                              (set-valid-to! nominal-latest
                                             (- valid-from (* 24 60 60))))
                              ;;(display "Looks like there was no valid-to in the nominal previous! Setting it to 1 day prior to new version's valid-from. "))
                              ;;(display "Requested version is newer than nominal latest, and this graph records history. Appending the requested version to this graph.")
                              ;;(newline)
                          (append nvss
                                  (apply append 
                                         (map (lambda (vsn)
                                                (emit-versioning base vsn))
                                              (cons target-new ;; target triples
                                                    (cons nominal-latest
                                                          (cdr ordered-versions)))))))]
                       [(and (compare-versions target-new nominal-latest)
                             version-log)
                        (let ((history-triple
                               (make-rdf-triple
                                (string-append base target-version-string)
                                (string-append endpoint "version_history")
                                version-log)))
                          ;;(display "Requested version is newer than nominal latest, but this graph does not record history. An external version log is provided, so use that. Remove existing versioning statements and annotate with requested.")
                          ;;(newline)
                          (append nvss (cons history-triple (emit-versioning base target-new))))] ;; non-versioning statements
                       [(compare-versions target-new nominal-latest)
                        (begin 
                          ;;(display "Requested version is newer than nominal latest, but this graph does not record history. No external version log is provided, so not adding that triple. Remove existing versioning statements and annotate with requested.")
                          ;;(newline)
                          (append nvss (emit-versioning base target-new)))]
                       [else
                        (begin
                          ;;(display "Requested version is less than latest transcribed in the file. Not overwriting.")
                          graph)]))])))))
           
(let* ((option-spec
	'((target-version (single-char #\V) (value #t))
          (base (single-char #\b) (value #t))
          (hist (single-char #\l) (value #t))
          (vocabulary (single-char #\U) (value #t))
          (provenance (single-char #\L) (value #t))
          (valid-from (single-char #\T) (value #t))
          ;(last-version (single-char #\L) (value #f))
          (dry-run (single-char #\p) (value #f))
	  (help (single-char #\h) (value #f))))
       (help-msg "ontoconsume [options]
  -V --target-version VSN   Use target version VSN
  -b --base IRI             Use IRI as base (append other identifiers to this)
  -l --versions IRI         IRI of external version history (some RDF graph)
  -U --update-vocab FILE    Update versioning in RDF graph FILE, a vocabulary
  -L --update-log   FILE    Append to RDF graph FILE, a log of version history
  -T --valid-from DATE      Version valid from DATE, not current date
  -p --dry-run              Only check whether updating/annotating was feasible
  -h --help                 Display this help message
")
       (options (getopt-long (command-line) option-spec))
       (target-version (option-ref options 'target-version #f))
       (base-iri (option-ref options 'base #f))
       (hist-iri (option-ref options 'hist #f))
       (work-on-vocabulary (option-ref options 'vocabulary #f))
       (work-on-history (option-ref options 'provenance #f))
       (valid-from (option-ref options 'valid-from #f))
       ;(last-version (option-ref options 'last-version #f))
       (dry-run (option-ref options 'dry-run #f))
       (help (option-ref options 'help #f)))
  (cond [help (display help-msg)]
        [(not target-version)
         (display "Did not specify a target version!")
         (newline) (newline)
         (display help-msg)]
        [(not (or work-on-vocabulary work-on-history))
         (display "No vocabulary or history file to version supplied!")
         (newline) (newline)
         (display help-msg)]
        [(not (check-version target-version))
         (display "Version should be of form MAJOR.MINOR.PATCH, components numeric")]
        [else
         ;; don't fall back to current epoch UNLESS valid-from is not given!
         (let ((epoch-proper 
                (if valid-from
                    (from-date valid-from #f)
                    (current-time))))
           (and epoch-proper
                (cond [(and work-on-vocabulary base-iri)
                       (let ((target-graph (load-rdf work-on-vocabulary base-iri)))
                         (cond [(and target-graph base-iri hist-iri)
                                (display
                                 (rdf->turtle
                                  (hash-versioning-scheme target-graph
                                                          base-iri
                                                          target-version
                                                          #:history? #f
                                                          #:valid-from epoch-proper
                                                          #:version-log hist-iri)))]
                               [(and target-graph base-iri)
                                (display
                                 (rdf->turtle
                                  (hash-versioning-scheme target-graph
                                                          base-iri
                                                          target-version
                                                          #:history? #f
                                                          #:valid-from epoch-proper)))]
                               [else
                                (display "Can't continue!")]))]
                      [(and work-on-history base-iri)
                       (let ((target-graph (load-rdf work-on-history base-iri)))
                         (if target-graph
                             (display
                              (rdf->turtle
                               (hash-versioning-scheme target-graph
                                                       base-iri
                                                       target-version
                                                       #:history? #t
                                                       #:valid-from epoch-proper)))
                             (display "Can't continue!")))]
                      [else (display "No base IRI supplied!")
                            (newline) (newline)
                            (display help-msg)])))]))                      
(newline)
