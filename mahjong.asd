(defsystem "mahjong"
  :version "0.1.0"
  :author "Jake Stothard"
  :license ""
  :depends-on ("alexandria" "trivia")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "mahjong/tests"))))

(defsystem "mahjong/tests"
  :author "Jake Stothard"
  :license ""
  :depends-on ("mahjong"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for mahjong"
  :perform (test-op (op c) (symbol-call :rove :run c)))
