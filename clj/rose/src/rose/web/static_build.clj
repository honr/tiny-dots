(ns rose.web.static-build
  (:require [rose.clu]
	    [rose.file]
	    [rose.inkscape]
            [rose.clu :as clu]))

(defn main [gen-html nom opts]
  (rose.file/make-sure-directory-exists [*cwd* ".out/gen"])
  (when (get #{:all :deco} (opts :gen))
    (println (clu/ascii-color :red :bold ";; [Re]generating decorations ..."))
    (rose.file/make-sure-directory-exists [*cwd* ".out/gen/style"])
    (println (:out (rose.inkscape/export-pngs (rose.file/path "style.svg")
                                              (rose.file/path ".out/gen/style"))))
    (println (clu/ascii-color :red
                              ";; [Re]generating decorations probably done.")))

  (when (get #{:all :html} (opts :gen))
    (println (clu/ascii-color :red :bold ";; [Re]generating html files ..."))
    (gen-html (get opts :url-prefix))
    (println (clu/ascii-color :red
                              ";; [Re]generating html files probably done.")))

  (println (clu/sh "chmod" "-R" "u=rwX,go=rX" [*cwd* ".out/gen"]))
  
  (rose.file/make-sure-directory-exists [*cwd* ".out/stage" nom])
  (println (clu/sh "rsync"
                   :progress "-avc" ;; use checksums
                   [*cwd* "static/"] [*cwd* ".out/gen/"]
                   [*cwd* ".out/stage" nom]))

  (when-let [remote-spec (get opts :remote)]
    (println (clu/ascii-color :red :bold
                              (str ";; Pushing to " remote-spec " ...")))
    (println (clu/sh "rsync" :progress "-avz" :L :delete
                     [*cwd* ".out/stage" nom ""]
                     remote-spec))
    (println (clu/ascii-color :red (str ";; Probably done. ")))))
