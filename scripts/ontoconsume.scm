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
		      see-previous see-also
		      valid-from valid-to
		      applies-to applies-also)
  version-annotation?
  (major-component version-annotation-major-component set-major-component!)
  (minor-component version-annotation-minor-component set-minor-component!)
  (patch-component version-annotation-patch-component set-patch-component!)
  (see-previous version-annotation-see-previous set-version-previous!)
  (see-also version-annotation-see-also set-version-also!)
  (valid-from version-annotation-valid-from set-valid-from!)
  (valid-to version-annotation-valid-to set-valid-to!)
  (applies-to version-annotation-applies-to set-against-graph!)
  (applies-also version-annotation-applies-also set-related-graphs!))

(define (inject-literal val)
  (let ((make-xsd (lambda (iri)
                    (string-append "http://www.w3.org/2001/XMLSchema#"
                                   iri))))
    (cond [(integer? val)
           (make-rdf-literal (number->string val)
                             (make-xsd "integer")
                             #f)]
          [(real? val) ; Guile stores non-integer reals using C double
           (make-rdf-literal (number->string val)
                             (make-xsd "double")
                             #f)] 
          [(and (boolean? val) val)
           (make-rdf-literal "true"
                             (make-xsd "boolean")
                             #f)]
          [(and (boolean? val) (not val))
           (make-rdf-literal "false"
                             (make-xsd "boolean")
                             #f)]
          [(symbol? val)
           (make-rdf-literal (symbol->string val)
                             (make-xsd "string")
                             #f)]
          [else
           (make-rdf-literal val
                             (make-xsd "string")
                             #f)])))

(define (emit-versioning subject versioning)
  (let* ((make-rdf-iri (lambda (name)
           (string-append "http://www.w3.org/1999/02/22-rdf-syntax-ns#" name)))
         (make-endpoint-iri (lambda (name)
           (string-append endpoint name)))
         (get-with (lambda (ref proc)
           (let ((maybe-elem
                  (proc versioning)))
            (if maybe-elem
                (make-rdf-triple subject
                                 (make-endpoint-iri ref)
                                 (inject-literal maybe-elem))
                #f)))))
    (filter (lambda (k) (and k))
            (list
     (get-with "major_component" version-annotation-major-component)
     (get-with "minor_component" version-annotation-minor-component)
     (get-with "patch_component" version-annotation-patch-component)
     (get-with "see_previous" version-annotation-see-previous)
     (get-with "valid-from" version-annotation-valid-from)
     (get-with "valid-to" version-annotation-valid-to)
     (get-with "applies-to" version-annotation-applies-to)
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
		      #f #f
		      #f #f
		      #f #f))

(define (make-initial-annotation M m p)
  (version-annotation M m p
                      #f #f
                      #f #f
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
         (lexf rdf-literal-lexical-form)
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
                     (set-valid-from! target-map (lexf a)))]
                   ["valid_to" (lambda (a)
                     (set-valid-to! target-map (lexf a)))]))))))
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

(define (parse-version combined-version)
  (map string->number (string-split combined-version #\.)))

(define (check-version vsn)
  (let ((re (make-regexp "^[0-9]+.[0-9]+.[0-9]+$")))
    (let ((mo (regexp-exec re vsn)))
      (and mo (match:substring mo)))))

(define* (hash-versioning-scheme graph base target-version-string
                                 valid-from valid-to
                                 #:key (history? #f)
                                       (version-log #f))
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
           (target-new
            (version-annotation major minor patch
                                #f #f
                                valid-from valid-to
                                #f #f))
           (target-triples
            (emit-versioning (string-append base target-version-string)
                             target-new)))
      (display
       "Looks like this graph is annotated with our versioning scheme already.")
      (newline)
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
        (if (null? found-in-graph)
                (begin (display
                        "Looks like it's not annotated yet.")
                       (append graph target-triples))
                (let ((nominal-latest
                       (car
                        (sort-list found-in-graph compare-versions))))
                  (cond [(and (compare-versions target-new nominal-latest)
                              history?)
                         (begin
                           (display "Requested version is newer than nominal latest, and this graph records history. Appending the requested version to this graph.")
                           (newline)
                           (append graph
                                   ;; vss + nvss:
                                   target-triples))]
                        [(and (compare-versions target-new nominal-latest)
                              version-log)
                         (let ((history-triple
                                (make-rdf-triple
                                 (string-append base target-version-string)
                                 (string-append endpoint "version_history")
                                 version-log)))
                           (display "Requested version is newer than nominal latest, but this graph does not record history. An external version log is provided, so use that. Remove existing versioning statements and annotate with requested.")
                           (newline)
                           (append nvss (cons history-triple target-triples)))] ;; non-versioning statements
                        [(compare-versions target-new nominal-latest)
                         (begin 
                           (display "Requested version is newer than nominal latest, but this graph does not record history. No external version log is provided, so not adding that triple. Remove existing versioning statements and annotate with requested.")
                           (newline)
                           (append nvss target-triples))]
                        [else
                         (begin
                           (display "Requested version is less than latest transcribed in the file. Not overwriting.")
                           graph)])))))))
           
(let* ((option-spec
	'((target-version (single-char #\V) (value #t))
          (base (single-char #\b) (value #t))
          (hist (single-char #\l) (value #t))
          (vocabulary (single-char #\U) (value #t))
          (provenance (single-char #\L) (value #t))
          (last-version (single-char #\L) (value #f))
          (dry-run (single-char #\p) (value #f))
	  (help (single-char #\h) (value #f))))
       (help-msg "ontoconsume [options]
  -V --target-version VSN   Use target version VSN
  -b --base-ir              Use IRI as base (append other identifiers to this)
  -l --versions IRI         IRI of external version history (some RDF graph)
  -U --update-vocab FILE    Update versioning in RDF graph FILE, a vocabulary
  -L --update-log   FILE    Append to RDF graph FILE, a log of version history
  -p --dry-run              Only check whether updating/annotating was feasible
  -L --last-version         Return the last valid version (using -P or -U alone)
  -h --help                 Display this help message
")
       (options (getopt-long (command-line) option-spec))
       (target-version (option-ref options 'target-version #f))
       (base-iri (option-ref options 'base #f))
       (hist-iri (option-ref options 'hist #f))
       (work-on-vocabulary (option-ref options 'vocabulary #f))
       (work-on-history (option-ref options 'provenance #f))
       (last-version (option-ref options 'last-version #f))
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
        [(and work-on-vocabulary base-iri hist-iri)
                (let ((target-graph (load-rdf work-on-vocabulary base-iri)))
                  (display
                   (rdf->turtle
                    (hash-versioning-scheme target-graph
                                            base-iri
                                            target-version
                                            0 (current-time) ;; kludge to demo it working. replace later with up to date...
                                            #:history? #f
                                            #:version-log hist-iri))))]
        [(and work-on-vocabulary base-iri)
         (let ((target-graph (load-rdf work-on-vocabulary base-iri)))
           (display
            (rdf->turtle
             (hash-versioning-scheme target-graph
                                     base-iri
                                     target-version
                                     0 (current-time)))))]
        [(and work-on-history base-iri)
         (let ((target-graph (load-rdf work-on-history base-iri)))
           (display
            (rdf->turtle
             (hash-versioning-scheme target-graph
                                     base-iri
                                     target-version
                                     0 (current-time)
                                     #:history? #t))))]
        [else (display "No base IRI supplied!")
              (newline) (newline)
              (display help-msg)]))
(newline)
