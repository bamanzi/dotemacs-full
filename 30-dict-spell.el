
;;** DICT protocol
;;*** dictionary.el
;;(setq dictionary-server "localhost")
;;TIP: to searc in specific dictionary, use: C-u M-x dictionary-search
;;(setq dictionary-default-dictionary "xdict")
;;(setq dictionary-default-dictionary "stardict")
(autoload 'dictionary-search "dictionary"
  "Ask for a word and search it in all dictionaries" t)
(autoload 'dictionary-match-words "dictionary"
  "Ask for a word and search all matching words in the dictionaries" t)

;;*** dictem.el
;; external utility `dict' client needed
(setq dictem-server "localhost")
(autoload 'dictem-run-search "dictem" nil t)
(autoload 'dictem-run-match  "dictem" nil t) 

;;*** dict.el (external program `dict' needed)
;;;  (NOTE: it's hard to use it on windows)
(setq dict-servers '("localhost" "dict.org"))
;;(setq dict-enable-key-bindings t)
;;(setq dict-databases '("gcide" "pydict"))
(autoload 'dict "dict" "Lookup a WORD on dict.org." t)


;;** stardict

;;*** sdcv-mode.el
(autoload 'sdcv-search "sdcv-mode" nil t)

;;*** sdcv.el
;;FIXME: problem


;;** spell
;;
