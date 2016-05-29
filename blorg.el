;;; init.el -- org-publish project config

;; Author: Eric Bailey <eric@ericb.me>
;; URL http://blorg.ericb.me

;;; Commentary:

;; This package ensures the blorg projects are in `org-publish-project-alist'.

;;; Code:

(require 'org)

(unless (boundp 'org-publish-project-alist)
  (setq org-publish-project-alist '(())))

(progn
  ;; Reference: http://orgmode.org/worg/org-tutorials/org-jekyll.html#text-4
  (add-to-list
   'org-publish-project-alist
   '("org-blorg"
     :base-directory "~/src/yurrriq/blorg/org/"
     :base-extension "org"
     :publishing-directory "~/src/yurrriq/blorg/"
     :publishing-function org-html-publish-to-html
     :exclude "\\(README\\|setup\\).org"
     :fontify-natively t
     :recursive t
     :section-numbers nil
     :html-doctype "html5"
     :html-extension "html"
     :htmlized-source t
     :body-only t))
  ;; (add-to-list
  ;;  'org-publish-project-alist
  ;;  '("org-static-blorg"
  ;;    :base-directory "~/src/yurrriq/blorg/org/"
  ;;    :base-extension "css\\|js\\|png\\|jpg\\|gif\\|lfe\\|ly\\|pdf\\|scm\\|svg\\|mp3"
  ;;    :publishing-directory "~/src/yurrriq/blorg/jekyll/"
  ;;    :recursive t
  ;;    :publishing-function org-publish-attachment))
  (add-to-list
   'org-publish-project-alist
   '("blorg" :components ("org-blorg"
                          ;; "org-static-blorg"
                          ))))

;; http://stackoverflow.com/a/14841597/1793234
(defun org-custom-link-img-follow (path)
  (org-open-file-with-emacs
   (format "../../images/%s" path)))

(defun org-custom-link-img-export (path desc format)
  (cond
   ((eq format 'html)
    (format "<img src=\"/images/%s\" alt=\"%s\"/>" path desc))))

(org-add-link-type "img" 'org-custom-link-img-follow 'org-custom-link-img-export)


;;; blorg.el ends here
