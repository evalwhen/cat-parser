(defsystem "cat-parser"
  :version "0.1.0"
  :author "hujianfeng"
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "learn lisp by writing parser"
  :in-order-to ((test-op (test-op "cat-parser/tests"))))

(defsystem "cat-parser/tests"
  :author "hujianfeng"
  :license ""
  :depends-on ("cat-parser"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cat-parser"
  :perform (test-op (op c) (symbol-call :rove :run c)))
