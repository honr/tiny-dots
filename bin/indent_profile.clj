#!/usr/bin/env clove
;; | clojure
;; (remove-ns 'bin.indent-profile)
(ns bin.indent-profile
  (:require [rose.file :as rfile]
            [rose.clu :as clu]
            [rose.xml :as xml]
            [rose.gplot :as gplot])
  (:import [java.io File]
           [org.joda.time
            DateTime Months Weeks MutableDateTime]))

;; Convert from a .indent.pro to a readable form (.clj)
;; Convert from .clj to .indent.pro

(def options
  {:blank-lines-after-declarations {:type Boolean, :comment "Force blank lines after the declarations. See  BLANK LINES.", :short-form "-bad" :inverse :no-blank-lines-after-declarations}
   :no-blank-lines-after-declarations {:type Boolean, :comment "Do not force blank lines after declarations. See  BLANK LINES.", :short-form "-nbad" :inverse :blank-lines-after-declarations}

   :blank-lines-after-procedures {:type Boolean, :comment "Force blank lines after procedure bodies. See  BLANK LINES.", :short-form "-bap", :inverse :no-blank-lines-after-procedures}
   :no-blank-lines-after-procedures {:type Boolean, :comment "Do not force blank lines after procedure bodies. See  BLANK LINES.", :short-form "-nbap" :inverse :blank-lines-after-procedures}

   :break-before-boolean-operator {:type Boolean, :comment "Prefer to break long lines before boolean operators. See  BREAKING LONG LINES.", :short-form "-bbo", :inverse :break-after-boolean-operator}
   :break-after-boolean-operator {:type Boolean, :comment "Do not prefer to break long lines before boolean operators. See  BREAKING LONG LINES.", :short-form "-nbbo" :inverse :break-before-boolean-operator}

   :blank-lines-after-commas {:type Boolean, :comment "Force newline after comma in declaration. See  DECLARATIONS.", :short-form "-bc", :inverse :no-blank-lines-after-commas}
   :no-blank-lines-after-commas {:type Boolean, :comment "Do not force newlines after commas in declarations. See  DECLARATIONS.", :short-form "-nbc" :inverse :blank-lines-after-commas}

   :comment-delimiters-on-blank-lines {:type Boolean, :comment "Put comment delimiters on blank lines. See  COMMENTS.", :short-form "-cdb", :inverse :no-comment-delimiters-on-blank-lines}
   :no-comment-delimiters-on-blank-lines {:type Boolean, :comment "Do not put comment delimiters on blank lines. See  COMMENTS.", :short-form "-ncdb" :inverse :comment-delimiters-on-blank-lines}

   :cuddle-do-while {:type Boolean, :comment "Cuddle while of do {} while; and preceding ‘}’. See  COMMENTS.", :short-form "-cdw", :inverse :dont-cuddle-do-while}
   :dont-cuddle-do-while {:type Boolean, :comment "Do not cuddle } and the while of a do {} while;. See  STATEMENTS.", :short-form "-ncdw" :inverse :cuddle-do-while}

   :cuddle-else {:type Boolean, :comment "Cuddle else and preceding ‘}’. See  COMMENTS.", :short-form "-ce", :inverse :dont-cuddle-else}
   :dont-cuddle-else {:type Boolean, :comment "Do not cuddle } and else. See  STATEMENTS.", :short-form "-nce" :inverse :cuddle-else}

   :space-after-cast {:type Boolean, :comment "Put a space after a cast operator. See  STATEMENTS.", :short-form "-cs", :inverse :no-space-after-casts}
   :no-space-after-casts {:type Boolean, :comment "Do not put a space after cast operators. See  STATEMENTS.", :short-form "-ncs" :inverse :space-after-cast}

   :break-function-decl-args {:type Boolean, :comment "Break the line before all arguments in a declaration. See  DECLARATIONS.", :short-form "-bfda", :inverse :dont-break-function-decl-args}
   :dont-break-function-decl-args {:type Boolean, :comment "Don’t put each argument in a function declaration on a separate line. See  DECLARATIONS.", :short-form "-nbfda" :inverse :break-function-decl-args}

   :left-justify-declarations {:type Boolean, :comment "If -cd 0 is used then comments after declarations are left justified behind the declaration. See  DECLARATIONS.", :short-form "-dj", :inverse :dont-left-justify-declarations}
   :dont-left-justify-declarations {:type Boolean, :comment "Comments after declarations are treated the same as comments after other statements. See  DECLARATIONS.", :short-form "-ndj" :inverse :left-justify-declarations}

   :format-first-column-comments {:type Boolean, :comment "Format comments in the first column. See  COMMENTS.", :short-form "-fc1", :inverse :dont-format-first-column-comments}
   :dont-format-first-column-comments {:type Boolean, :comment "Do not format comments in the first column as normal. See  COMMENTS.", :short-form "-nfc1" :inverse :format-first-column-comments}

   :format-all-comments {:type Boolean, :comment "Do not disable all formatting of comments. See  COMMENTS.", :short-form "-fca", :inverse :dont-format-comments}
   :dont-format-comments {:type Boolean, :comment "Do not format any comments. See  COMMENTS.", :short-form "-nfca" :inverse :format-all-comments}

   :honour-newlines {:type Boolean, :comment "Prefer to break long lines at the position of newlines in the input. See  BREAKING LONG LINES.", :short-form "-hnl", :inverse :ignore-newlines}
   :ignore-newlines {:type Boolean, :comment "Do not prefer to break long lines at the position of newlines in the input. See  BREAKING LONG LINES.", :short-form "-nhnl" :inverse :honour-newlines}

   :space-after-parentheses {:type Boolean, :comment "Put a space after every ’(’ and before every ’)’. See  STATEMENTS.", :short-form "-prs", :inverse :no-space-after-parentheses}
   :no-space-after-parentheses {:type Boolean, :comment "Do not put a space after every ’(’ and before every ’)’. See  STATEMENTS.", :short-form "-nprs" :inverse :space-after-parentheses}

   :space-after-for {:type Boolean, :comment "Put a space after each for. See  STATEMENTS.", :short-form "-saf", :inverse :no-space-after-for}
   :no-space-after-for {:type Boolean, :comment "Do not put a space after every for. See  STATEMENTS.", :short-form "-nsaf" :inverse :space-after-for}

   :space-after-if {:type Boolean, :comment "Put a space after each if. See  STATEMENTS.", :short-form "-sai", :inverse :no-space-after-if}
   :no-space-after-if {:type Boolean, :comment "Do not put a space after every if. See  STATEMENTS.", :short-form "-nsai" :inverse :space-after-if}

   :space-after-procedure-calls {:type Boolean, :comment "Insert a space between the name of the procedure being called and the ‘(’. See  STATEMENTS.", :short-form "-pcs", :inverse :no-space-after-function-call-names}
   :no-space-after-function-call-names {:type Boolean, :comment "Do not put space after the function in function calls. See  STATEMENTS.", :short-form "-npcs" :inverse :space-after-procedure-calls}

   :space-after-while {:type Boolean, :comment "Put a space after each while. See  STATEMENTS.", :short-form "-saw", :inverse :no-space-after-while}
   :no-space-after-while {:type Boolean, :comment "Do not put a space after every while. See  STATEMENTS.", :short-form "-nsaw" :inverse :space-after-while}

   :swallow-optional-blank-lines {:type Boolean, :comment "Swallow optional blank lines. See  BLANK LINES.", :short-form "-sob", :inverse :leave-optional-blank-lines}
   :leave-optional-blank-lines {:type Boolean, :comment "Do not swallow optional blank lines. See  BLANK LINES.", :short-form "-nsob" :inverse :swallow-optional-blank-lines}

   :space-special-semicolon {:type Boolean, :comment "On one-line for and while statements, force a blank before the semicolon. See  STATEMENTS.", :short-form "-ss", :inverse :dont-space-special-semicolon}
   :dont-space-special-semicolon {:type Boolean, :comment "Do not force a space before the semicolon after certain statements.  Disables ‘-ss’. See  STATEMENTS.", :short-form "-nss" :inverse :space-special-semicolon}

   :use-tabs {:type Boolean, :comment "Use tabs. This is the default. See  INDENTATION.", :short-form "-ut", :inverse :no-tabs}
   :no-tabs {:type Boolean, :comment "Use spaces instead of tabs. See  INDENTATION.", :short-form "-nut" :inverse :use-tabs}

   :procnames-start-lines {:type Boolean, :comment "Put the type of a procedure on the line before its name. See  DECLARATIONS.", :short-form "-psl", :inverse :dont-break-procedure-type}
   :dont-break-procedure-type {:type Boolean, :comment "Put the type of a procedure on the same line as its name. See  DECLARATIONS.", :short-form "-npsl" :inverse :procnames-start-lines}

   :braces-after-if-line {:type Boolean, :comment "Put braces on line after if, etc. See  STATEMENTS.", :short-form "-bl", :inverse :braces-on-if-line}
   :braces-on-if-line {:type Boolean, :comment "Put braces on line with if, etc. See  STATEMENTS.", :short-form "-br" :inverse :braces-after-if-line}

   :braces-after-func-def-line {:type Boolean, :comment "Put braces on line following function definition line. See  DECLARATIONS.", :short-form "-blf", :inverse :braces-on-func-def-line}
   :braces-on-func-def-line {:type Boolean, :comment "Put braces on function definition line. See  DECLARATIONS.", :short-form "-brf" :inverse :braces-after-func-def-line}

   :braces-after-struct-decl-line {:type Boolean, :comment "Put braces on the line after struct declaration lines. See  DECLARATIONS.", :short-form "-bls", :inverse :braces-on-struct-decl-line}
   :braces-on-struct-decl-line {:type Boolean, :comment "Put braces on struct declaration line. See  DECLARATIONS.", :short-form "-brs" :inverse :braces-after-struct-decl-line}

   :continue-at-parentheses {:type Boolean, :comment "Line up continued lines at parentheses. See  INDENTATION.", :short-form "-lp", :inverse :dont-line-up-parentheses}
   :dont-line-up-parentheses {:type Boolean, :comment "Do not line up parentheses. See  STATEMENTS.", :short-form "-nlp" :inverse :continue-at-parentheses}

   :start-left-side-of-comments {:type Boolean, :comment "Put the ‘*’ character at the left of comments. See  COMMENTS.", :short-form "-sc", :inverse :dont-star-comments}
   :dont-star-comments {:type Boolean, :comment "Do not put the ‘*’ character at the left of comments. See  COMMENTS.", :short-form "-nsc" :inverse :start-left-side-of-comments}

   :parameter-indentation {:type Integer, :comment "Indent parameter types in old-style function definitions by n spaces. See  INDENTATION.", :short-form "-ip"}
   :no-parameter-indentation {:type Boolean, :comment "Zero width indentation for parameters. See  INDENTATION.", :short-form "-nip"}

   :blank-before-sizeof {:type Boolean, :comment "Put a space between sizeof and its argument. See  STATEMENTS.", :short-form "-bs"}

   :break-function-decl-args-end {:type Boolean, :comment "Break the line after the last argument in a declaration. See  DECLARATIONS.", :short-form "-bfde"}

   :blank-lines-before-block-comments {:type Boolean, :comment "Force blank lines before block comments. See  BLANK LINES.", :short-form "-bbb"}

   :leave-preprocessor-space {:type Boolean, :comment "Leave space between ‘#’ and preprocessor directive. See  INDENTATION.", :short-form "-lps"}
   ;; :T
   ;; {:type String,
   ;;  :comment "Tell indent the name of typenames. See  DECLARATIONS.",
   ;;  :short-form "-T"}
   :brace-indent {:type Integer, :comment "Indent braces n spaces. See  STATEMENTS.", :short-form "-bli"}
   :case-brace-indentation {:type Integer, :comment "Indent braces after a case label N spaces. See  STATEMENTS.", :short-form "-cbi"}
   :comment-indentation {:type Integer, :comment "Put comments to the right of code in column n. See  COMMENTS.", :short-form "-c"}
   :comment-line-length {:type Integer, :comment "Set maximum line length for comment formatting to n. See  COMMENTS.", :short-form "-lc"}
   :continuation-indentation {:type Integer, :comment "Continuation indent of n spaces. See  STATEMENTS.", :short-form "-ci"}
   :case-indentation {:type Integer, :comment "Case label indent of n spaces. See  STATEMENTS.", :short-form "-cli"}
   :declaration-comment-column {:type Integer, :comment "Put comments to the right of the declarations in column n. See  COMMENTS.", :short-form "-cd"}
   :declaration-indentation {:type Integer, :comment "Put variables in column n. See  DECLARATIONS.", :short-form "-di"}
   :else-endif-column {:type Integer, :comment "Put comments to the right of #else and #endif statements in column n. See  COMMENTS.", :short-form "-cp"}
   :indent-label {:type Integer, :comment "Set offset for labels to column n. See  INDENTATION.", :short-form "-il"}
   :indent-level {:type Integer, :comment "Set indentation level to n spaces. See  INDENTATION.", :short-form "-i"}
   :line-comments-indentation {:type Integer, :comment "Set indentation of comments not to the right of code to n spaces. See  COMMENTS.", :short-form "-d"}
   :line-length {:type Integer, :comment "Set maximum line length for non-comment lines to n. See  BREAKING LONG LINES.", :short-form "-l"}
   :struct-brace-indentation {:type Integer, :comment "Indent braces of a struct, union or enum N spaces. See  STATEMENTS.", :short-form "-sbi"}
   :tab-size {:type Integer, :comment "Set tab size to n spaces. See  INDENTATION.", :short-form "-ts"}
   :paren-indentation {:type Integer, :comment "Specify the extra indentation per open parentheses ’(’ when a statement is broken.See  STATEMENTS.", :short-form "-pi"}
   :preprocessor-indentation {:type Integer, :comment "Specify the indentation for preprocessor conditional statements.See  INDENTATION.", :short-form "-ppi"}})

(def options-template
  "{
 :blank-before-sizeof true ;; Put a space between sizeof and its argument. See  STATEMENTS.
 :blank-lines-after-commas true ;; Force newline after comma in declaration. See  DECLARATIONS.
 :blank-lines-after-declarations true ;; Force blank lines after the declarations. See  BLANK LINES.
 :blank-lines-after-procedures true ;; Force blank lines after procedure bodies. See  BLANK LINES.
 :blank-lines-before-block-comments true ;; Force blank lines before block comments. See  BLANK LINES.
 :braces-after-func-def-line true ;; Put braces on line following function definition line. See  DECLARATIONS.
 :braces-after-if-line true ;; Put braces on line after if, etc. See  STATEMENTS.
 :braces-after-struct-decl-line true ;; Put braces on the line after struct declaration lines. See  DECLARATIONS.
 :break-before-boolean-operator true ;; Prefer to break long lines *before* boolean operators. See  BREAKING LONG LINES.
 :break-function-decl-args true ;; Break the line before all arguments in a declaration. See  DECLARATIONS.
 :break-function-decl-args-end true ;; Break the line after the last argument in a declaration. See  DECLARATIONS.
 :comment-delimiters-on-blank-lines true ;; Put comment delimiters on blank lines. See  COMMENTS.
 :continue-at-parentheses true ;; Line up continued lines at parentheses. See  INDENTATION.
 :cuddle-do-while true ;; Cuddle while of do {} while; and preceding ‘}’. See  COMMENTS.
 :cuddle-else true ;; Cuddle else and preceding ‘}’. See  COMMENTS.
 :format-all-comments true ;; Do not disable all formatting of comments. See  COMMENTS.  Inverse: Do not format any comments.
 :format-first-column-comments true ;; Format comments in the first column. See  COMMENTS.
 :honour-newlines true ;; Prefer to break long lines at the position of newlines in the input. See  BREAKING LONG LINES.
 :leave-preprocessor-space true ;; Leave space between ‘#’ and preprocessor directive. See  INDENTATION.
 :left-justify-declarations true ;; If -cd (:declaration-comment-column) 0 is used then comments after declarations are left justified behind the declaration. See  DECLARATIONS.  Inverse: comments after declarations are treated the same as comments after other statements.
 :no-parameter-indentation true ;; Zero width indentation for parameters. See  INDENTATION.
 :procnames-start-lines true ;; Put the type of a procedure on the line before its name. See  DECLARATIONS.  Inverse: Put the type of a procedure on the same line as its name.
 :space-after-cast true ;; Put a space after a cast operator. See  STATEMENTS.
 :space-after-for true ;; Put a space after each for. See  STATEMENTS.
 :space-after-if true ;; Put a space after each if. See  STATEMENTS.
 :space-after-parentheses true ;; Put a space after every ’(’ and before every ’)’. See  STATEMENTS.
 :space-after-procedure-calls true ;; Insert a space between the name of the procedure being called and the ‘(’. See  STATEMENTS.
 :space-after-while true ;; Put a space after each while. See  STATEMENTS.
 :space-special-semicolon true ;; On one-line for and while statements, force a blank before the semicolon. See  STATEMENTS.
 :start-left-side-of-comments true ;; Put the ‘*’ character at the left of comments. See  COMMENTS.
 :swallow-optional-blank-lines true ;; Swallow optional blank lines. See  BLANK LINES.
 :use-tabs true ;; Use tabs. This is the default. See  INDENTATION.  Inverse: Use spaces instead of tabs.


 :brace-indent 0 ;; Indent braces n spaces. See  STATEMENTS.
 :case-brace-indentation 0 ;; Indent braces after a case label N spaces. See  STATEMENTS.
 :case-indentation 0 ;; Case label indent of n spaces. See  STATEMENTS.
 :comment-indentation 0 ;; Put comments to the right of code in column n. See  COMMENTS.
 :comment-line-length 0 ;; Set maximum line length for comment formatting to n. See  COMMENTS.
 :continuation-indentation 0 ;; Continuation indent of n spaces. See  STATEMENTS.
 :declaration-comment-column 0 ;; Put comments to the right of the declarations in column n. See  COMMENTS.
 :declaration-indentation 0 ;; Put variables in column n. See  DECLARATIONS.
 :else-endif-column 0 ;; Put comments to the right of #else and #endif statements in column n. See  COMMENTS.
 :indent-label 0 ;; Set offset for labels to column n. See  INDENTATION.
 :indent-level 0 ;; Set indentation level to n spaces. See  INDENTATION.
 :line-comments-indentation 0 ;; Set indentation of comments not to the right of code to n spaces. See  COMMENTS.
 :line-length 0 ;; Set maximum line length for non-comment lines to n. See  BREAKING LONG LINES.
 :parameter-indentation 0 ;; Indent parameter types in old-style function definitions by n spaces. See  INDENTATION.
 :paren-indentation 0 ;; Specify the extra indentation per open parentheses ’(’ when a statement is broken.See  STATEMENTS.
 :preprocessor-indentation 0 ;; Specify the indentation for preprocessor conditional statements.See  INDENTATION.
 :struct-brace-indentation 0 ;; Indent braces of a struct, union or enum N spaces. See  STATEMENTS.
 :tab-size 0 ;; Set tab size to n spaces. See  INDENTATION.
}")

(def options-short-form-map
  (into {}
        (filter
         identity
         (for [[k v] options]
           (when-let [short-form (v :short-form)]
             [short-form k])))))

(def common-styles
  {"gnu"
   ["-nbad" "-bap" "-nbc" "-bbo" "-bl" "-bli2" "-bls" "-ncdb" "-nce" "-cp1" "-cs" "-di2" "-ndj" "-nfc1" "-nfca" "-hnl" "-i2" "-ip5" "-lp" "-pcs" "-nprs" "-psl" "-saf" "-sai" "-saw" "-nsc" "-nsob"]
   "kr"
   ["-nbad" "-bap" "-bbo" "-nbc" "-br" "-brs" "-c33" "-cd33" "-ncdb" "-ce" "-ci4" "-cli0" "-cp33" "-cs" "-d0" "-di1" "-nfc1" "-nfca" "-hnl" "-i4" "-ip0" "-l75" "-lp" "-npcs" "-nprs" "-npsl" "-saf" "-sai" "-saw" "-nsc" "-nsob" "-nss"]
   "linux"
   ["-nbad" "-bap" "-nbc" "-bbo" "-hnl" "-br" "-brs" "-c33" "-cd33" "-ncdb" "-ce" "-ci4" "-cli0" "-d0" "-di1" "-nfc1" "-i8" "-ip0" "-l80" "-lp" "-npcs" "-nprs" "-npsl" "-sai" "-saf" "-saw" "-ncs" "-nsc" "-sob" "-nfca" "-cp33" "-ss" "-ts8" "-il1"]
   "orig-berkeley-indent"
   ["-nbad" "-nbap" "-bbo" "-bc" "-br" "-brs" "-c33" "-cd33" "-cdb" "-ce" "-ci4" "-cli0" "-cp33" "-di16" "-fc1" "-fca" "-hnl" "-i4" "-ip4" "-l75" "-lp" "-npcs" "-nprs" "-psl" "-saf" "-sai" "-saw" "-sc" "-nsob" "-nss" "-ts8"]})

(def var-num-re #"(-[a-z]*)([0-9]*)")

(defn get-opt-from-short-form [x]
  (let [[_ k-short num]
        (condp = x
          "-nfc1" [nil "-nfc1" nil],
          "-fc1" [nil "-fc1" nil]
          (re-matches var-num-re x))
        k (options-short-form-map k-short)
        opt (options k)]
    [k opt k-short num]))

(defn desc {:cli {}}
  [s] ;; #{"gnu" "kr" "linux" "orig-berkeley-indent"}
  (doseq [x (common-styles s)]
    (let [[k opt k-short num] (get-opt-from-short-form x)]
      (condp = (:type opt)
        Integer (println k num)
        Boolean (println k true)
        ;; String (println k)
        (println [x k-short num k opt])))))

(defn gen {:cli {}}
  []
  (doseq [[k v] (read-string (slurp *in*))]
    (let [opt (options k)]
      (println
       (condp = (:type opt)
         Integer (str (opt :short-form) v)
         Boolean (if v
                   (str (opt :short-form))
                   (str ((options (opt :inverse)) :short-form)))
         String (str (opt :short-form) v))))))

(defn template {:cli {}}
  []
  (println options-template))

(clu/run-command-maybe-ns *ns* "indent_profile.clj")
