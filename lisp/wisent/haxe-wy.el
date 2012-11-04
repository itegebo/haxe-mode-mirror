;;; haxe-wy.el --- Generated parser support file

;; Copyright (C) 2012 wvxvw

;; Author: wvxvw <wvxvw@wvxvw-desktop>
;; Created: 2012-10-16 21:44:42+0200
;; Keywords: syntax
;; X-RCS: $Id$

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; PLEASE DO NOT MANUALLY EDIT THIS FILE!  It is automatically
;; generated from the grammar file haxe.wy.

;;; History:
;;

;;; Code:

(require 'semantic/lex)
(eval-when-compile (require 'semantic/bovine))

;;; Prologue
;;

;;; Declarations
;;
(defconst haxe-wy--keyword-table
  (semantic-lex-make-keyword-table
   '(("case" . CASE)
     ("if" . IF)
     ("in" . IN)
     ("true" . TRUE)
     ("false" . FALSE)
     ("this" . THIS)
     ("super" . SUPER)
     ("catch" . CATCH)
     ("function" . FUNCTION)
     ("var" . VAR)
     ("do" . do)
     ("break" . break)
     ("continue" . CONTINUE)
     ("return" . RETURN)
     ("default" . DEFAULT)
     ("package" . PACKAGE)
     ("import" . IMPORT)
     ("null" . NULL)
     ("else" . ELSE)
     ("class" . CLASS)
     ("interface" . INTERFACE)
     ("enum" . ENUM)
     ("new" . NEW)
     ("try" . TRY)
     ("throw" . THROW)
     ("typedef" . TYPEDEF)
     ("private" . PRIVATE)
     ("public" . PUBLIC)
     ("static" . STATIC)
     ("override" . OVERRIDE)
     ("using" . USING)
     ("inline" . INLINE))
   '(("inline" summary "")
     ("using" summary "")
     ("override" summary "")
     ("static" summary "")
     ("public" summary "")
     ("private" summary "")
     ("typedef" summary "")
     ("throw" summary "")
     ("try" summary "")
     ("new" summary "")
     ("enum" summary "")
     ("interface" summary "")
     ("class" summary "")
     ("else" summary "")
     ("null" summary "")
     ("import" summary "")
     ("package" summary "")
     ("default" summary "")
     ("return" summary "")
     ("continue" summary "")
     ("var" summary "")
     ("function" summary "")
     ("super" summary "")
     ("this" summary "")
     ("false" summary "")
     ("true" summary "")
     ("in" summary "")
     ("if" summary "")
     ("case" summary "A sub-expression of switch-case expression. Note that HaXe does not allow\nfall-through and doesn't require `break' to terminate the case")))
  "Table of language keywords.")

(defconst haxe-wy--token-table
  (semantic-lex-make-type-table
   '(("close-paren"
      (RBRACK . "]")
      (RBRACE . "}")
      (RPAREN . ")"))
     ("open-paren"
      (LBRACK . "[")
      (LBRACE . "{")
      (LPAREN . "("))
     ("semantic-list"
      (BRACE_BLCK . "^{")
      (BRACK_BLCK . "^\\[")
      (PAREN_BLCK . "^("))
     ("number"
      (NUMBER_LITERAL))
     ("string"
      (STRING_LITERAL))
     ("symbol"
      (IDENTIFIER)
      (BOOLEAN_LITERAL . "true")
      (BOOLEAN_LITERAL . "false")
      (NULL_LITERAL . "null"))
     ("punctuation"
      (META . "@")
      (COMP . "~")
      (OR . "|")
      (XOR . "^")
      (QUESTION . "?")
      (URSHIFTEQ . ">>>=")
      (URSHIFT . ">>>")
      (GT . ">")
      (EQ . "=")
      (LT . "<")
      (SEMI . ";")
      (COLON . ":")
      (DIV . "/")
      (DOT . ".")
      (MINUS . "-")
      (COMMA . ",")
      (PLUS . "+")
      (MULT . "*")
      (AND . "&")
      (MOD . "%")
      (NOT . "!")
      (SR . ">>")
      (SL . "<<")
      (AND_AND . "&&")
      (OR_OR . "||")
      (XOR_EQ . "^=")
      (OR_EQ . "|=")
      (AND_EQ . "&=")
      (SR_EQ . ">>=")
      (SL_EQ . "<<=")
      (MOD_EQ . "%=")
      (DIV_EQ . "/=")
      (MUL_EQ . "*=")
      (MINUS_EQ . "-=")
      (PLUS_EQ . "+=")
      (GT_EQ . ">=")
      (LT_EQ . "<=")
      (NOT_EQUAL . "!=")
      (EQ_EQ . "==")
      (MINUS_MINUS . "--")
      (PLUS_PLUS . "++")))
   '(("punctuation" :declared t)))
  "Table of lexical tokens.")

(defconst haxe-wy--parse-table
  (progn
    (eval-when-compile
      (require 'semantic/wisent/comp))
    (wisent-compile-grammar
     '((CASE IF IN TRUE FALSE THIS SUPER CATCH FUNCTION VAR do break CONTINUE RETURN DEFAULT PACKAGE IMPORT NULL ELSE CLASS INTERFACE ENUM NEW TRY THROW TYPEDEF PRIVATE PUBLIC STATIC OVERRIDE USING INLINE PLUS_PLUS MINUS_MINUS EQ_EQ NOT_EQUAL LT_EQ GT_EQ PLUS_EQ MINUS_EQ MUL_EQ DIV_EQ MOD_EQ SL_EQ SR_EQ AND_EQ OR_EQ XOR_EQ OR_OR AND_AND SL SR NOT MOD AND MULT PLUS COMMA MINUS DOT DIV COLON SEMI LT EQ GT URSHIFT URSHIFTEQ QUESTION XOR OR COMP META NULL_LITERAL BOOLEAN_LITERAL IDENTIFIER STRING_LITERAL NUMBER_LITERAL PAREN_BLCK BRACK_BLCK BRACE_BLCK LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK)
       nil
       (literal
        ((NULL_LITERAL))
        ((BOOLEAN_LITERAL))
        ((STRING_LITERAL))
        ((NUMBER_LITERAL)))
       (type
        (nil)
        ((qualified_name type_parameter_opt)
         (concat $1 $2)))
       (type_parameter
        ((LT qualified_name GT)
         (concat $1 $2 $3))
        ((LT qualified_name type_parameter GT)))
       (type_parameter_opt
        (nil)
        ((type_parameter)))
       (qualified_name
        ((qualified_name DOT IDENTIFIER)
         (concat $1 "." $3))
        ((IDENTIFIER)))
       (qualified_name_list
        ((qualified_name_list COMMA qualified_name)
         (cons $3 $1))
        ((qualified_name)
         (list $1)))
       (argument_list_opt
        (nil)
        ((argument_list)))
       (argument_list
        ((argument_list COMMA argument))
        ((argument)))
       (expression
        ((expression term))
        ((term)))
       (term
        ((literal))
        ((operator))
        ((IDENTIFIER))
        ((NEW))
        ((CLASS))
        ((THIS))
        ((SUPER)))
       (operator
        ((NOT))
        ((PLUS))
        ((PLUS_PLUS))
        ((MINUS))
        ((MINUS_MINUS))
        ((NOT_EQ))
        ((MOD))
        ((MOD_EQ))
        ((AND))
        ((AND_AND))
        ((AND_EQ))
        ((MULT))
        ((MULT_EQ))
        ((PLUS_EQ))
        ((MINUS_EQ))
        ((DOT))
        ((DIV))
        ((DIV_EQ))
        ((COLON))
        ((LT))
        ((LSHIFT))
        ((LSHIFT_EQ))
        ((LT_EQ))
        ((EQ))
        ((EQ_EQ))
        ((GT))
        ((GT_EQ))
        ((RSHIFT))
        ((RSHIFT_EQ))
        ((QUESTION))
        ((XOR))
        ((XOREQ))
        ((OR))
        ((OREQ))
        ((OROR))
        ((COMP))
        ((IS)))
       (compilation_unit
        ((using_directive))
        ((package_declaration))
        ((type_declaration)))
       (using_directive
        ((USING IDENTIFIER SEMICOLON)))
       (package_declaration
        ((PACKAGE qualified_name SEMICOLON)))
       (type_declaration
        ((SEMICOLON)
         nil)
        ((class_declaration))
        ((typedef_declaration))
        ((enum_declaration)))
       (modifiers_opt
        (nil)
        ((modifiers)
         (nreverse $1)))
       (modifiers
        ((modifiers modifier)
         (cons $2 $1))
        ((modifier)
         (list $1)))
       (modifier
        ((NEW))
        ((PUBLIC))
        ((PRIVATE))
        ((STATIC))
        ((OVERRIDE))
        ((INLINE))))
     'nil))
  "Parser table.")

(defun haxe-wy--install-parser ()
  "Setup the Semantic Parser."
  (semantic-install-function-overrides
   '((parse-stream . wisent-parse-stream)))
  (setq semantic-parser-name "LALR"
        semantic--parse-table haxe-wy--parse-table
        semantic-debug-parser-source "haxe.wy"
        semantic-flex-keywords-obarray haxe-wy--keyword-table
        semantic-lex-types-obarray haxe-wy--token-table)
  ;; Collect unmatched syntax lexical tokens
  (semantic-make-local-hook 'wisent-discarding-token-functions)
  (add-hook 'wisent-discarding-token-functions
            'wisent-collect-unmatched-syntax nil t))


;;; Analyzers
;;
(define-lex-string-type-analyzer haxe-wy--<punctuation>-string-analyzer
  "string analyzer for <punctuation> tokens."
  "\\(\\s.\\|\\s$\\|\\s'\\)+"
  '((META . "@")
    (COMP . "~")
    (OR . "|")
    (XOR . "^")
    (QUESTION . "?")
    (URSHIFTEQ . ">>>=")
    (URSHIFT . ">>>")
    (GT . ">")
    (EQ . "=")
    (LT . "<")
    (SEMI . ";")
    (COLON . ":")
    (DIV . "/")
    (DOT . ".")
    (MINUS . "-")
    (COMMA . ",")
    (PLUS . "+")
    (MULT . "*")
    (AND . "&")
    (MOD . "%")
    (NOT . "!")
    (SR . ">>")
    (SL . "<<")
    (AND_AND . "&&")
    (OR_OR . "||")
    (XOR_EQ . "^=")
    (OR_EQ . "|=")
    (AND_EQ . "&=")
    (SR_EQ . ">>=")
    (SL_EQ . "<<=")
    (MOD_EQ . "%=")
    (DIV_EQ . "/=")
    (MUL_EQ . "*=")
    (MINUS_EQ . "-=")
    (PLUS_EQ . "+=")
    (GT_EQ . ">=")
    (LT_EQ . "<=")
    (NOT_EQUAL . "!=")
    (EQ_EQ . "==")
    (MINUS_MINUS . "--")
    (PLUS_PLUS . "++"))
  'punctuation)


;;; Epilogue
;;

(provide 'haxe-wy)

;;; haxe-wy.el ends here
