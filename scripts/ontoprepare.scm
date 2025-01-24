#! /usr/bin/guile3.0 -s
!#
(use-modules (ice-9 getopt-long)
	     (ice-9 ftw)
             (ice-9 regex))

(define (search-for-latest-version target-dir)
  (let* ((version-dirs (scandir target-dir))
         (re (make-regexp "^[0-9]+.[0-9]+.[0-9]+$"))
         (val-vsn (lambda (vs)
                    (if (string? vs)
                        (let ((mo (regexp-exec re vs)))
                          (and mo (match:substring mo)))))))
    (if version-dirs
        (let ((nominal-versions
               (filter
                (lambda (k) (and k))
                (map val-vsn version-dirs))))
          (car
           (sort nominal-versions compare-versions)))
        version-dirs)))

;; A version is always greater than the other if the major version is larger.
;; If the major version is equal, then if the minor version is greater.
;; If the minor version is equal, then if the patch level is greater.
(define (compare-versions version0 version1)
  (let* ((parsed-version0 (parse-version version0))
	 (parsed-version1 (parse-version version1))
	 (major0 (car parsed-version0))
	 (major1 (car parsed-version1))
	 (minor0 (cadr parsed-version0))
	 (minor1 (cadr parsed-version1))
	 (patch0 (caddr parsed-version0))
	 (patch1 (caddr parsed-version1)))
    (cond ((equal? version0 version1) version0)
	  ((> major0 major1) version0)
	  ((> minor0 minor1) version0)
	  ((> patch0 patch1) version0)
	  (else #f))))

(define (make-version major minor patch)
  (string-append (number->string major) "."
		 (number->string minor) "."
		 (number->string patch)))

(define (extract-ending-file base-directory)
  (car (reverse (string-split base-directory #\/))))

(define (extract-base-directory base-directory-string)
  ;; something like ./ns/beam/ -> ns/beam
  (let* ((rs "[A-z0-9+\\+_~\\-]+(/+[A-z0-9+\\+_~\\-]+)*")
         (re (make-regexp rs regexp/icase))
         (mo (regexp-exec re base-directory-string)))
    (and mo (match:substring mo))))

(define (parse-version combined-version)
  (map string->number (string-split combined-version #\.)))

(define-syntax reset-component
  (lambda (stx)
    (syntax-case stx (major minor patch)
      [(reset-component minor vsn)
       #'(let ((split (parse-version vsn)))
           (make-version (car split)
                         0
                         (caddr split)))]
      [(reset-component patch vsn)
       #'(let ((split (parse-version vsn)))
           (make-version (car split)
                         (cadr split)
                         0))]
      [(reset-component minor+patch vsn)
       #'(reset-component minor
          (reset-component patch vsn))])))
      
(define-syntax increment-component
  (lambda (stx)
    (syntax-case stx (major minor patch major+reset minor+reset)
      [(increment-component major vsn)
       #'(let ((split (parse-version vsn)))
            (apply make-version `(,(+ 1 (car split))
                                . ,(cdr split))))]
      [(increment-component minor vsn)
       #'(let ((split (parse-version vsn)))
            (make-version (car split)
                          (+ 1 (cadr split))
                          (caddr split)))]
      [(increment-component patch vsn)
       #'(let ((split (parse-version vsn)))
           (make-version (car split)
                         (cadr split)
                         (+ 1 (caddr split))))]
      [(increment-component major+reset vsn)
       #'(reset-component minor+patch
           (increment-component major vsn))]
      [(increment-component minor+reset vsn)
       #'(reset-component patch
           (increment-component minor vsn))])))

(define (make-add-type type-pair)
  (let ((media-type (car type-pair))
	(extension  (cdr type-pair)))
  (string-append "AddType "media-type" "
		 "."extension)))

;; opts : cons pairs of sign (+/-) and flag
(define (make-opt opt-pair)
  (string-append " " (car opt-pair) (cdr opt-pair)))

(define (make-lead-in opt-pairs type-pairs dir-base)
  (let ((tgt (list
	      (list (string-concatenate
		     (cons "Options" (map make-opt opt-pairs))))
	      (map make-add-type type-pairs)
	      (list "RewriteEngine On")
	      (list (string-append "RewriteBase" " " dir-base))))	
	(pair-up (lambda (k) (string-append k "\n"))))
    (string-concatenate
     (map pair-up (apply append tgt)))))

(define (make-rewrite-cond proto/media-type)
  (let ((proto (car proto/media-type))
	(media-type (cdr proto/media-type)))
    (string-append "RewriteCond" " " proto " " media-type)))

(define (make-rewrite-rule input-pattern target-production redir)
  (if (number? redir)
      (string-append "RewriteRule" " "
		     input-pattern " "
		     target-production " "
		     "[R=" (number->string redir) ",L]")))

(define (make-rewrite proto/type-pairs input-pattern target-production redir)
  (if (null? proto/type-pairs)
      ""
      (let* ([reversed-type-pairs (reverse proto/type-pairs)]
	     [last-type-pair (car reversed-type-pairs)]
	     [last-rule (make-rewrite-cond last-type-pair)]
	     [leading-type-pairs (cdr reversed-type-pairs)]
	     [pair-up
	      (lambda (k)
		(string-append (make-rewrite-cond k)
			       " [OR]\n"))]
	     [production
	      (make-rewrite-rule input-pattern target-production redir)])
	(reverse
	 (cons (string-append production "\n")
	       (cons (string-append last-rule "\n")
		     (map pair-up leading-type-pairs)))))))

(define (make-rewrite-rules-proper media/file-pairs no-slash-preceding-base latest-version)
  (let* ((incl-redir (lambda (target-file)
	  (string-append "/" no-slash-preceding-base
			 "/" latest-version
			 "/" target-file)))
	 (rewrite-shim
	  (lambda (pair)
	    (make-rewrite `(("%{HTTP_ACCEPT}" . ,(car pair)))
			  (string-append "^" (cdr pair) "$")
			  (incl-redir (cdr pair))
                          303))))
    (string-concatenate
     (apply append
      (map rewrite-shim media/file-pairs)))))

(define (make-htaccess-contents media/file-pairs target-base latest-version exclude-preamble)
  (let* ((media/ext-pairs
          (map (lambda (pair)
		(cons (car pair)
		      (car (reverse
			    (string-split (cdr pair) #\.)))))
	      media/file-pairs))
         (no-slash-preceding-base (extract-base-directory target-base))
         (target-file-prefix (extract-ending-file no-slash-preceding-base))
	(pairs-with-html
	 (append
          (map (lambda (pair)
                 (cons (car pair)
                       (string-append target-file-prefix
                                      "."(cdr pair))))
               media/file-pairs)
          '(("text/html" . "index.html"))))
        (must-include-rules
         (make-rewrite-rules-proper pairs-with-html
                                    no-slash-preceding-base
                                    latest-version)))
    (if exclude-preamble
        must-include-rules
        (string-append (make-lead-in '(["-" . "MultiViews"])
                                     media/ext-pairs
                                     (string-append "/" no-slash-preceding-base))
                       must-include-rules))))

(let* ((option-spec
	'((increment-major (single-char #\I) (value #f))
          (increment-minor (single-char #\i) (value #f))
          (increment-patch (single-char #\p) (value #f))
          (forced-version (single-char #\f) (value #t))
          (directory (single-char #\d) (value #t))
          (version-only (single-char #\V) (value #f))
          (exclude-preamble (single-char #\E) (value #f))
	  (help (single-char #\h) (value #f))))
       (options (getopt-long (command-line) option-spec))
       (exclude-preamble (option-ref options 'exclude-preamble #f))
       (incr-major (option-ref options 'increment-major #f))
       (incr-minor (option-ref options 'increment-minor #f))
       (incr-patch (option-ref options 'increment-patch #f))
       (forced-version (option-ref options 'forced-version #f))
       (set-cwd  (option-ref options 'directory #f))
       (version-only (option-ref options 'version-only #f))
       (help (option-ref options 'help #f))
       (default-type-pairs '(("text/turtle" . "ttl")
                             ("application/n-triples" . "nt")
                             ("application/rdf+xml" . "rdf")))
       (vsn-regexp (make-regexp "^[0-9]+.[0-9]+.[0-9]+$"))
       (get-vsn (lambda (last-version)
                  (cond ((not last-version) last-version)
                        (incr-major (increment-component major+reset last-version))
                        (incr-minor (increment-component minor+reset last-version))
                        (incr-patch (increment-component patch last-version))
                        (else #f))))
       (chk-vsn (lambda (vsn)
                  (let ((mo (regexp-exec vsn-regexp vsn)))
                    (and mo (match:substring mo))))))
  (cond (forced-version
         (cond ((and (chk-vsn forced-version) set-cwd) ;; correct version and Dir exists
                (if version-only
                    (display forced-version)
                    (display (make-htaccess-contents default-type-pairs
                                                     set-cwd
                                                     forced-version
                                                     exclude-preamble))))
               ((access? set-cwd R_OK)
                (display "Version should be of form MAJOR.MINOR.PATCH, components numeric"))
               (else
                (display (string-append "No such file or directory: " set-cwd)))))
        (set-cwd
         (let* ((last-version (search-for-latest-version set-cwd))
                (new-version (get-vsn last-version)))
           (if new-version
               (if version-only
                   (display new-version)
                   (display
                    (make-htaccess-contents default-type-pairs
                                            set-cwd
                                            new-version
                                            exclude-preamble)))
               (display
                (string-append "No such file or directory: " set-cwd)))))
        (else (display "\
ontoprepare [options]
  -I --increment-major     Increment version major component
  -i --increment-minor     Increment version minor component
  -p --increment-patch     Increment version patch-level component
  -f --force-version VSN   Use VSN as new version (MAJOR.MINOR.PATCH)
  -d --directory DIR       Use DIR as ontology deployment directory
  -E --exclude-preamble    Exclude the preamble so that output can be concatenated
  -V --version-only        Return the new, incremented version string only
  -h --help                Display this help message
"))))
(newline)
