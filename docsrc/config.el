(setq org-publish-project-alist
      '(("docs"
         :base-directory "."
         :publishing-directory "../docs"
         :publishing-function org-html-publish-to-html
         :section-numbers nil
         :with-toc nil
         :html-head "<link rel=\"stylesheet\"
                    href=\"../docs/css/style.css\"
                    type=\"text/css\"/>")))
