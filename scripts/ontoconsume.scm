;; file scripts/ontoconsume.scm

;; 1. Load the RDF and spit out an error if it can't.
;; 2. Annotate the RDF with a version number. This should support emitting both a file annotated directly, and a separate file which will also be annotated with endpoint information.
;; 3. Output either the original format or the original format + other syntaxes.

(use-modules (ice-9 receive)
             (srfi srfi-1)
	     (srfi srfi-9)
             ((rdf xsd)
              #:prefix xsd:)
             (rdf rdf)
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

(define (emit-versioning subject versioning)
  (let ((get-with (lambda (ref proc)
                    (let ((maybe-elem
                           (proc versioning)))
                      (if maybe-elem
                          (make-rdf-triple subject
                                           (string-append endpoint ref)
                                           maybe-elem)
                          #f)))))
    (filter (lambda (k) (and k))
            (list
     (get-with "major_component" version-annotation-major-component)
     (get-with "minor_component" version-annotation-minor-component)
     (get-with "patch_component" version-annotation-patch-component)
     (get-with "see_previous" version-annotation-see-previous)
     (get-with "valid-from" version-annotation-valid-from)
     (get-with "valid-from" version-annotation-valid-to)
     (get-with "applies-to" version-annotation-applies-to)))))

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

(define (hash-versioning-scheme graph base target-version-string valid-from valid-to history?)
  (receive (vss nvss)
      (partition (lambda (st)
                   (member (rdf-triple-predicate st)
                           version-statements))
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
                           (append graph target-triples))] ;; vss + nvss
                        [(compare-versions target-new nominal-latest)
                         (begin
                           (display "Requested version is newer than nominal latest, but this graph does not record history. Remove existing versioning statements and annotate with requested.")
                           (newline)
                           (append nvss target-triples))] ;; non-versioning statements
                        [else
                         (begin
                           (display "Requested version is less than latest transcribed in the file. Not overwriting.")
                           graph)])))))))


           
