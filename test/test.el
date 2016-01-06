(require 'ert)
(load "~/el-cfg-parser/config-parser.el")
(defvar config-data nil)
(ert-deftest test-read ()
  "read and parse \"test-read.cfg\""
  (should (equal (setq config-data (config-parser-read "test-read.cfg"))
                 '(("" ("key1" . "1") ("key2" . "value2"))
                   ("short" ("k1" . "off") ("k2" . "v2"))))))

(ert-deftest test-sections ()
  ""
  (should (equal (config-parser-sections config-data)
                 '("" "short"))))
(ert-deftest test-has-section ()
  ""
  (should (config-parser-has-section config-data "short"))
  (should (config-parser-has-section config-data ""))
  (should (null (config-parser-has-section config-data "non-exist-section"))))

(ert-deftest test-items ()
  ""
  (should (equal (config-parser-items config-data "")
                 '(("key1" . "1") ("key2" . "value2"))))
  (should (equal (config-parser-items config-data "short")
                 '(("k1" . "off") ("k2" . "v2"))))
  (should (null (config-parser-items config-data "non-exist-section"))))

(ert-deftest test-options ()
  ""
  (should (equal (config-parser-options config-data "short")
                 '("k1" "k2")))
  (should (equal (config-parser-options config-data "")
                 '("key1" "key2")))
  (should (null (config-parser-options config-data "non-exist-section"))))

(ert-deftest test-has-option ()
  ""
  (should (config-parser-has-option config-data "short" "k1"))
  (should (config-parser-has-option config-data "short" "k2"))
  (should (null (config-parser-has-option config-data "short" "k3")))
  (should (config-parser-has-option config-data "" "key1"))
  (should (config-parser-has-option config-data "" "key2"))
  (should (null (config-parser-has-option config-data "" "key3")))
  (should (null (config-parser-has-option config-data "non-exist-section" ""))))

(ert-deftest test-get ()
  ""
  (should (equal (config-parser-get config-data "" "key1")
                 "1"))
  (should (null (config-parser-get config-data "short" "key2")))
  (should (null (config-parser-get config-data "" "non-exist-key"))))

(ert-deftest test-get-number ()
  ""
  (should (equal (config-parser-get-number config-data "" "key1")
                 1))
  (should (equal (config-parser-get-number config-data "" "key2")
                 0))                     ;non-number
  (should (null (config-parser-get-number config-data "" "non-exist-key"))))

(ert-deftest test-get-boolean ()
  ""
  (should (config-parser-get-boolean config-data "" "key1"))
  (should (null (config-parser-get-boolean config-data "short" "k1"))) 
  (should (null (config-parser-get-number config-data "" "non-exist-key"))))

(ert-deftest test-delete-section! ()
  ""
  (let ((config-data-copy (copy-tree config-data)))
    (setq config-data-copy (config-parser-delete-section! config-data-copy "non-exist-section"))
    (should (equal config-data-copy config-data))
    (setq config-data-copy (config-parser-delete-section! config-data-copy ""))
    (should (not (config-parser-has-section config-data-copy "")))))

(ert-deftest test-delete-option! ()
  ""
  (let ((config-data-copy (copy-tree config-data)))
    (setq config-data-copy (config-parser-delete-option! config-data-copy "non-exist-section" "k1"))
    (should (equal config-data-copy config-data))

    (setq config-data-copy (config-parser-delete-option! config-data-copy "" "non-exist-option"))
    (should (equal config-data-copy config-data))

    (setq config-data-copy (config-parser-delete-option! config-data-copy "" "key1"))
    (should (not (config-parser-has-option config-data-copy "" "key1")))

    (setq config-data-copy (config-parser-delete-option! config-data-copy "short" "k2"))
    (should (not (config-parser-has-option config-data-copy "short" "k2")))))

(ert-deftest test-set! ()
  ""
  (let ((config-data-copy (copy-tree config-data)))
    (setq config-data-copy (config-parser-set! config-data-copy "non-exist-section" "k1" "new-v1"))
    (should (equal config-data-copy config-data))

    (setq config-data-copy (config-parser-set! config-data-copy "" "non-exist-option" "new-value"))
    (should (equal config-data-copy config-data))

    (setq config-data-copy (config-parser-set! config-data-copy "" "key1" "new-value1"))
    (should (equal (config-parser-get config-data-copy "" "key1")
                   "new-value1" ))

    (setq config-data-copy (config-parser-set! config-data-copy "short" "k2" "new-k2"))
    (should (equal (config-parser-get config-data-copy "short" "k2")
                   "new-k2"))))
