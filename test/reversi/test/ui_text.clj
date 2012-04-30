(ns reversi.test.ui_text
  (:use [reversi.ui_text])
  (:use [clojure.test]))

(deftest malformed-input-test
  (is (not (malformed-input? "2E")) "2E is valid")
  (is (not (malformed-input? "2e")) "2e is valid")
  (is (not (malformed-input? "e2")) "e2 is valid")
  (is (not (malformed-input? "E2")) "E2 is valid")
  (is (malformed-input? "3X") "There is no column X")
  (is (malformed-input? "9B") "There is no row 9")
  (is (malformed-input? "") "No input specified")
  (is (malformed-input? "2EE") "Too much input"))

(deftest parse-input-test
  (is (= [3 2] (parse-input "3C")) "Row 3 Col 2")
  (is (= [3 2] (parse-input "3c")) "Row 3 Col 2")
  (is (= [3 2] (parse-input "C3")) "Row 3 Col 2")
  (is (= [3 2] (parse-input "c3")) "Row 3 Col 2"))