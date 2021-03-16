(in-package #:cl-user)

(load "sys/compat.lisp")

(setq *readtable* ags::*ags-readtable*)

(load "sys/ags.asd")
(load "sys/base-keim.asd")
(load "sys/keim.asd")

(asdf:load-system "nd")
