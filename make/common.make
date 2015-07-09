%.json :: %.clj
	@clove -c clojure -e "(clojure.data.json/print-json (read-string (slurp *in*)))" < $< > $@

%.html :: %.clj
	@clove -c clojure -e "(doseq [x (clojure.edn/read-string (str \"[\" (slurp *in*) \"]\"))] (rose.ang-html/print-tree x))" < $< > $@

#.htl.html:
%.html :: %.htl
	vulcan <$< >$@

require-clj-modules:
	@clove -c clojure -e "(require ['clojure.edn :reload true])"
	@clove -c clojure -e "(require ['clojure.data.json :reload true])"
	@clove -c clojure -e "(require ['rose.ang-html :reload true])"
