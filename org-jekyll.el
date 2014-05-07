;;;
;;; Emacs org-mode support for bolgging with Jekyll
;;;


(setq org-publish-project-alist
      '(("org-blog"
         ;; Path to your org files.
         :base-directory "~/iprj/junchangchen.github.io/_org"
         :base-extension "org"

         ;; Path to your Jekyll project.
         :publishing-directory "~/iprj/junchangchen.github.io"
         :recursive t
         :publishing-function org-publish-org-to-html
         :headline-levels 4 
         :html-extension "html"
         :body-only t ;; Only export section between <body> </body>
         )


        ("org-static-blog"
         :base-directory "~/iprj/junchangchen.github.io/_org"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf\\|php"
         :publishing-directory "~/iprj/junchangchen.github.io"
         :recursive t
         :publishing-function org-publish-attachment)

        ("blog" :components ("org-blog" "org-static-blog"))

        ))




(provide 'org-jekyll)
