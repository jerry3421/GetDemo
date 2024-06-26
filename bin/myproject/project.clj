(ns myproject.project(:require [instaparse.core :as insta])
  (:import (clojure.asm Opcodes Type ClassWriter)
(clojure.asm.commons Method GeneratorAdapter)))

;Interpreter function

(defn dynamic-eval [interpreter]
 (fn[ast]
 (fn[]
 (insta/transform interpreter ast))))

;;ConstMod

;Parser
(def parser-const-mod 
  (insta/parser "
     prog = moduloExpression
     moduloExpression = number ?space modulo 
     <modulo> = <'mod'> ?space number
     <space> = <#'\\s*'>
     number = #'[0-9]+'    
"))
(parser-const-mod "2mod5")

;Interpreter

(defn const-interpretation [const-instr-interpretation init-env]
 {:prog (fn [& instrs] (:_ret (reduce
 (fn[env instr]
 (insta/transform (const-instr-interpretation env) instr))
 init-env
 instrs)))})


 (defn const-instr-interpretation [env]
 { 
 :moduloExpression (fn[{v1 :_ret :as env1} {v2 :_ret :as env2}]
 (assoc (merge env1 env2) :_ret (mod v1 v2)))
 :number #(assoc env :_ret (Integer/parseInt %))})
 
 (def const-interpretor (dynamic-eval (const-interpretation const-instr-interpretation {:_ret 0})))
 
 (def const-interpretor-test (->> "1004 mod9" parser-const-mod const-interpretor))
 (const-interpretor-test)