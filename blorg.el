;;; init.el -- org-publish project config

;; Author: Eric Bailey <eric@ericb.me>
;; URL http://blorg.ericb.me

;;; Commentary:

;; This package ensures the blorg projects are in `org-publish-project-alist'.

;;; Code:

(require 'org)

(unless (boundp 'org-publish-project-alist)
  (setq org-publish-project-alist '(())))

;; Reference: http://orgmode.org/worg/org-tutorials/org-jekyll.html#text-4
(unless (assoc "blorg" org-publish-project-alist)
  (add-to-list
   'org-publish-project-alist
   '("org-blorg"
     :base-directory "~/src/yurrriq/blorg/org/"
     :base-extension "org"
     :publishing-directory "~/src/yurrriq/blorg/jekyll/"
     :publishing-function org-html-publish-to-html
     :exclude "\\(README\\|setup\\).org"
     :recursive t
     :section-numbers nil
     :html-doctype "html5"
     :html-extension "html"
     :body-only t))
  (add-to-list
   'org-publish-project-alist
   '("org-static-blorg"
     :base-directory "~/src/yurrriq/blorg/org/"
     :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|svg\\|mp3"
     :publishing-directory "~/src/yurrriq/blorg/jekyll/"
     :recursive t
     :publishing-function org-publish-attachment))
  (add-to-list
   'org-publish-project-alist
   '("blorg" :components ("org-blorg" "org-static-blorg"))))

;;; blorg.el ends here
