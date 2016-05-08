 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("0a5e87ac98b0adfe4e12356fff24d49ffbbe5ef0aa8290752c184e6857d70558" default)))
 '(safe-local-variable-values
   (quote
    ((org-html-postamble)
     (org-html-htmlize-output-type quote css)
     (org-export-html-postamble . "<p style='font-size: smaller'>Copyright &copy; 2013 ShipRise and Avdi Grimm.</p>")
     (encoding . utf-8)
     (org-babel-noweb-wrap-end . ">>")
     (org-babel-noweb-wrap-start . "#<<")
     (org-export-html-postamble . "<p style='font-size: smaller'>Copyright &copy; 2012 ShipRise and Avdi Grimm.</p>")
     (org-export-latex-verbatim-wrap "\\begin{Verbatim}[frame=leftline,label=Output]
" . "\\end{Verbatim}
")
     (org-export-latex-minted-options
      ("frame" "leftline")
      ("linenos" "true")
      ("stepnumber" "2"))
     (org-table-formula-evaluate-inline)
     (org-export-latex-listings . minted)
     (org-latex-to-pdf-process "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
     (org-export-latex-minted-langs
      (html "rhtml")
      (emacs-lisp "common-lisp")
      (cc "c++")
      (cperl "perl")
      (shell-script "bash")
      (caml "ocaml"))
     (org-latex-to-pdf-process quote
			       ("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f" "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
     (org-export-latex-minted-langs quote
				    ((html "rhtml")
				     (emacs-lisp "common-lisp")
				     (cc "c++")
				     (cperl "perl")
				     (shell-script "bash")
				     (caml "ocaml")))
     (org-export-latex-minted . t)
     (org-export-latex-listings quote minted)
     (ruby-compilation-executable . "ruby")
     (ruby-compilation-executable . "ruby1.8")
     (ruby-compilation-executable . "ruby1.9")
     (ruby-compilation-executable . "rbx")
     (ruby-compilation-executable . "jruby")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
