(lang dune 2.7)
(name tjr_mmap)

;; for opam
(authors              "Tom Ridge <tom.j.ridge@googlemail.com>")
(maintainers          "Tom Ridge <tom.j.ridge@googlemail.com>")
(source               (github tomjridge/FIXME))
(homepage             https://github.com/tomjridge/FIXME)
(documentation        https://tomjridge.github.io/FIXME/ocamldoc/index.html)
(generate_opam_files  true)


(package 
 (name tjr_mmap)
 (synopsis "Extensible mmap")
 (depends bigstringaf)) 

(package 
 (name unixfile)
 (synopsis "Unix file IO (pread, pwrite)")
 (depends unix))

; (package 
;  (name mmap_value)
;  (synopsis "")
;  (depends mmap)) 

(package 
 (name std_util)
 (synopsis "FIXME")
 (depends )) 


