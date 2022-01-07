(defpackage cat-parser
  (:use :cl))
(in-package :cat-parser)

(defvar *delim* (list "(" ")"))
(defvar *quotation-marks* (list #\" #\'))

(defun tail-test ()
  (print "here")
  (tail-test))

(defstruct node type start end elts)

;; (defun parse (toks)
;;   (cond
;;     ((null toks) '())
;;     ((start-paren? (car toks))
;;      (multiple-value-bind (child remain) (parse-child (cdr toks))
;;        (cond
;;          ((null remain) (error :missing-close-paren)) ;; todo: error detail
;;          ((end-paren? (car remain)) (cons (make-node :type :sexp
;;                                                       :start (node-start (car toks))
;;                                                       :end (node-end (car remain))
;;                                                       :elts child)
;;                                            (parse (cdr remain))))
;;          (t (error :misssing-close-paren)))))
;;     ((end-paren? (car toks)) (error :missing-open-paren))
;;     (t (cons (car toks) (parse (cdr toks))))))

(defun parse-single-sexp (toks)
  (cond
    ((null toks) (values nil '()))
    ((start-paren? (car toks))
     (multiple-value-bind (child remain) (parse-child (cdr toks) '())
       (cond
         ((null remain) (error "missing-close-paren"))
         ((not (end-paren? (car remain))) (error "missing-close-paren"))
         ;; ((not (null (cdr remain))) (error :trailing-toks)) ;; TODO: show trailing
         (t (values (make-node :type :sexp
                                :start (node-start (car toks))
                                :end (node-end (car remain))
                               :elts child)
                    (cdr remain)))
         )))
    (t (error "missing-open-paren"))))

(defun parse-child (toks res)
  (cond
    ((null toks) (values (reverse res) '()))
    ((end-paren? (car toks)) (values (reverse res) toks))
    ((start-paren? (car toks))
     (multiple-value-bind (sexp remain) (parse-single-sexp toks)
       (parse-child remain (cons sexp res))))
    (t (parse-child (cdr toks) (cons (car toks) res)))
    ))

(defun start-paren? (node)
  (and (eq (node-type node) :delim)
       (string= (node-elts node) "(")))

(defun end-paren? (node)
  (and (eq (node-type node) :delim)
       (string= (node-elts node) ")")))

(defun scan (s)
  (labels ((scan1 (s start)
             (let ((delim ""))
               (cond ((= start (length s))
                      (values :eof start))

                     ((whitespace? (char s start))
                      (scan1 s (+ start 1)))

                     ((let ((res (start-with-one-of s start *delim*)))
                        (setf delim res)
                        res)
                      (values (make-node :type :delim
                                         :start start
                                         :end (+ start (length delim))
                                         :elts delim)
                              (+ start (length delim))))

                     ((start-with-one-of s start *quotation-marks*)
                      (multiple-value-bind (res group) (ppcre:scan-to-strings "^\"([^\"]*)\"" (subseq s start))
                        (if res
                            (values (make-node :type :string
                                               :start start
                                               :end (+ start (length (elt group 0)))
                                               :elts (elt group 0))
                                    (+ start (length res)))
                            (error "invalid string"))))
                     (t (labels ((collect-chars (pos chars)
                                   (cond
                                     ((or (<= (length s) pos)
                                          (whitespace? (char s pos))
                                          (start-with-one-of s pos *delim*)
                                          (start-with-one-of s pos *quotation-marks*))
                                      (values (make-node :type :token
                                                         :start start
                                                         :end pos
                                                         :elts (coerce (reverse chars) 'string))
                                              pos))
                                     (t (collect-chars (+ pos 1) (cons (char s pos) chars))))))
                          (collect-chars start '())))))))

    (labels ((collect-toks (s start toks)
                   (multiple-value-bind (tok newstart) (scan1 s start)
                     (cond
                       ((eq tok :eof)
                        (reverse toks))
                       (t (collect-toks s newstart (cons tok toks)))))))
      (collect-toks s 0 '()))))

(defun whitespace? (char)
  (char= char #\Space))

(defun start-with? (s start prefix)
  (let* ((prefix (if (characterp prefix)
                     (string prefix)
                     prefix))
         (len (length prefix)))
    (cond
      ((= len 0) nil)
      ((< (length s) (+ start len)) nil)
      ((string= s prefix :start1 start :end1 (+ start len)) t)
      (t nil))))

(defun start-with-one-of (s start lst)
  (labels ((in (lst)
             (cond
               ((null lst) nil)
               ((start-with? s start (car lst)) (string (car lst)))
               (t (in (cdr lst))))))
    (in lst)))
