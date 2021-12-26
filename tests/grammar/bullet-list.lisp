(in-package 3bmd-tests)


(def-grammar-test parse-simple-list
  :text "* Foo
* Bar
"
  :expected '((:BULLET-LIST
               (:LIST-ITEM (:PLAIN "Foo"))
               (:LIST-ITEM (:PLAIN "Bar")))))

(def-grammar-test parse-list-with-separated-items
  :text "* Foo

* Bar
"
  :expected '((:BULLET-LIST
               (:LIST-ITEM (:PARAGRAPH "Foo"))
               (:LIST-ITEM (:PARAGRAPH "Bar")))))


(def-grammar-test parse-list-with-multiline-item
  :text "* Foo

  Second line

* Bar
"
  :expected '((:BULLET-LIST
               (:LIST-ITEM
                ;; TODO:
                ;; Seems in this case this item should be a PARAGRAPH too?
                (:PLAIN "Foo")))
              ;; TODO:
              ;; Why this paragraph is not part of the previos list item?
              ;; It has the same indentation as the list item.
              (:PARAGRAPH "Second" " " "line")
              ;; TODO:
              ;; Why do this "Bar is not a second LIST-ITEM of the first BULLET-LIST?
              (:BULLET-LIST
               (:LIST-ITEM (:PLAIN "Bar")))))


(def-grammar-test parse-list-with-embedded-code
  :text "* Foo

  ```
  This is a code
  ```

* Bar
"
  ;; :really-expected '((:BULLET-LIST
  ;;                     (:LIST-ITEM
  ;;                      (:PARAGRAPH "Foo")
  ;;                      (:PARAGRAPH (:CODE "
  ;;         This is a code
  ;;       ")))
  ;;                     (:LIST-ITEM (:PLAIN "Bar"))))
  :expected '((:BULLET-LIST (:LIST-ITEM (:PLAIN "Foo")))
              ;; TODO:
              ;; This paragraph should be a part of the first list item
              (:PARAGRAPH
               (:CODE "
  This is a code
"))
              ;; And this bullet list should not be generated.
              ;; The Bar list item should be inside the first bullet list.
              (:BULLET-LIST (:LIST-ITEM (:PLAIN "Bar")))))
