(progn
  (include-lib "clj/include/compose.lfe") ; Clojure threading macros
  (-> (lodox-parse:docs #"lodox")         ; lodox docs map, generated by lodox
      (mref 'modules)                     ; lodox modules (Maclisp)
      (cadddr)                            ; 4th module =:= lodox-parse (CL)
      (mref 'exports)                     ; lodox-parse exports (Maclisp)
      (hd)                                ; first function =:= docs/1 (Erlang)
      (mref 'doc)                         ; docstring of docs/1
      (list_to_binary)                    ; string->binary
      (Elixir.Markdown:to_html            ; Elixir app wrapping C lib
       '[#(fenced_code true)])))          ; as in GitHub Flavored Markdown