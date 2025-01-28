;; file scripts/ontoconsume.scm

;; 1. Load the RDF and spit out an error if it can't.
;; 2. Annotate the RDF with a version number. This should support emitting both a file annotated directly, and a separate file which will also be annotated with endpoint information.
;; 3. Output either the original format or the original format + other syntaxes.

;; At minimum, there ought to be an interactive CLI mode with Q/A options.

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
         "version_represented_as"
         "see_previous" "see_also"
         "valid_from" "valid_to"
         "applies_to" "applies_also")))

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

(define (make-empty-version-annotation)
  (version-annotation #f #f #f
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
;;       send:applies_to <https://w3id.org/example> .

;; Start by defining a function to take a set of statements and gather them up.
;; So we expect that we're dealing with the blank node in the example above.
(define (coalesce-versioning statements base working-node)
  (let ((target-map (make-empty-version-annotation))
	(target-major #f)
	(target-minor #f)
	(target-patch #f)
	(rejected '())
	(rslv (lambda (ref)
		(string-append endpoint ref))))
    (letrec ((statement-helper
	      (lambda (stmt)
		(let ((subj (rdf-triple-subject stmt))
		      (pred (rdf-triple-predicate stmt))
		      (obj (rdf-triple-object stmt)))
		  ;(if (not (equal? subj working-node))
		  ;    (set! rejected (cons stmt rejected))
		      (cond
		       [(equal? pred (rslv "Version"))
			#f]
		       [(equal? pred (rslv "major_component"))
			(set! target-major
			      (string->number
			       (rdf-literal-lexical-form obj)))]
		       [(equal? pred (rslv "minor_component"))
			(set! target-minor
			      (string->number
			       (rdf-literal-lexical-form obj)))]
		       [(equal? pred (rslv "patch_component"))
			(set! target-patch
			      (string->number
			       (rdf-literal-lexical-form obj)))]
		       [(equal? pred (rslv "valid_from"))
			(set-valid-from! target-map
					 (rdf-literal-lexical-form obj))]
		       [(equal? pred (rslv "valid_to"))
			(set-valid-to! target-map
					 (rdf-literal-lexical-form obj))]
		       [else #f])))))
      (for-each statement-helper statements))
    (set-major-component! target-map target-major)
    (set-minor-component! target-map target-minor)
    (set-patch-component! target-map target-patch)
    target-map))

		      
(define (hash-versioning-scheme graph base)
  (receive (vss nvss)
      (partition (lambda (st)
                   (member (rdf-triple-predicate st)
                           version-statements))
                 graph)
    (if (null? vss)
        (display
         "Looks like it's not annotated yet.")
        (display
         "Looks like this graph is annotated with our versioning scheme already."))
    (newline)))
