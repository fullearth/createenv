;;gauche 0.9.4
;;gosh create_env_bat.scm
;;
(use srfi-13) ;;文字列関係
(define batString "@echo off\n\n%%%IFSECTION%%%\n\n%%%SUBRSECTION%%%\n\n:END\n")  ;;元になる文字列
;;reverseしたリストの最初の要素が#<subr read>なのでcdrで弾く

(define (src->sexpList)
  (with-input-from-file "sexpenv.txt"
    (lambda ()
      (let loop ((sexp read) (ret '()))
        (if (eof-object? sexp)
          (cdr (reverse ret))
          (loop (read) (cons sexp ret)))))))

;;デバッグ
; (define ele (car (src->sexpList)))
; (define ele2 (cadr (src->sexpList)))
; (define names (list "ruby" "python" "gauche"))
; (define paths (list "rubyPATH" "pythonPATH" "gauchePATH"))
; (define descs (list "rubyDESC" "pythonDESC" "gaucheDESC"))
;;S式の要素->各要素
;;element -> string
(define (getName element)
  (symbol->string (car element)))

(define (getPath element)
  (pathList->path (getPathList element) (getFlag element)))

(define (getPathList element)
  (cadr (assq 'path (cadr element))))

(define (pathList->path pl flag)
  (let loop ((pl (reverse pl)) (ret ""))
    (if (null? pl)
      (if flag
        ret
        (string-append ret "%PATH%"))
      (loop (cdr pl) (string-append (car pl) ";" ret)))))

(define (getDesc element)
  (cadr (assq 'desc (cadr element))))

(define (getFlag element)
  (cadr (assq 'clearFlag (cadr element))))

;;[string] -> string
(define (makeIfSection names)
  (let ((first (car names)))
    (let loop ((names names) (ret ""))
      (if (null? names)
        (string-append ret ") else (\n%%%DESCSECTION%%%\ngoto END\n)")
        (let ((name (car names)))
          (if (eq? first name)
            (loop (cdr names) (string-append ret "if \"%1\"==\"" name "\" (\ngoto " (string-upcase name) "\n"))
            (loop (cdr names) (string-append ret ") else if \"%1\"==\"" name "\" (\ngoto " (string-upcase name) "\n"))))))))

(define (makeDescSection names descs)
  (let loop ((names names) (descs descs) (ret ""))
    (if (null? descs)
      ret
      (loop (cdr names) (cdr descs) (string-append ret "echo \"" (car names) " : " (car descs) "\"\n")))))

(define (makeSubrSection names paths)
  (let loop((names names) (paths paths) (ret ""))
    (if (null? paths)
      ret
      (loop (cdr names) (cdr paths) (string-append ret ":" (string-upcase (car names)) "\nset PATH=" (car paths) "\ngoto END\n")))))

;;string->string->string->string->string
(define (makebatString batString ifSection descSection subrSection)
  (regexp-replace* batString #/%%%IFSECTION%%%/ ifSection #/%%%DESCSECTION%%%/ descSection #/%%%SUBRSECTION%%%/ subrSection))

;;(display (makebatString batString (makeIfSection names) (makeDescSection names descs) (makeSubrSection names paths)))

(define (main args)
  (let loop ((elements (src->sexpList)) (names '()) (paths '()) (descs '()))
    (if (not(null? elements))
      (let ((element (car elements)))
        (loop (cdr elements) (cons (getName element) names) (cons (getPath element) paths) (cons (getDesc element) descs)))
    (let ((ifSection (makeIfSection names)) (descSection (makeDescSection names descs)) (subrSection (makeSubrSection names paths)))
      (call-with-output-file "setenv.bat"
        (lambda (p)
          (display (regexp-replace* batString #/%%%IFSECTION%%%/ ifSection #/%%%DESCSECTION%%%/ descSection #/%%%SUBRSECTION%%%/ (regexp-quote subrSection)) p))))))
0)
