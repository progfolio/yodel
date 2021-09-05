;; -*- lexical-binding: t; -*-
(require 'yodel)
(yodel-file "/tmp/test"
  :point "|"
  :contents "te|st"
  :then*
  (insert "nsile ")
  (yodel-file "/tmp/test2"
    :then*
    (insert-file-contents "/tmp/test")
    (buffer-name)))




