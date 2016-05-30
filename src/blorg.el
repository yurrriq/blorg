;;; blorg.el -- blorg.ericb.me ox-publish config

;; Author: Eric Bailey <eric@ericb.me>
;; URL: http://blorg.ericb.me

;;; Commentary:

;; This package ensures the blorg project is in `org-publish-project-alist'.

;;; Code:

(require 'ox-publish)
(require 'ob-lfe)

(unless (boundp 'org-publish-project-alist)
  (setq org-publish-project-alist '(())))

(let* ((base-directory
        (concat (file-name-directory (directory-file-name default-directory))
                "src/"))
       (publishing-directory default-directory))
  (add-to-list
   'org-publish-project-alist
   `("blorg"
     :base-directory       ,base-directory
     :base-extension       "org"
     :publishing-directory ,publishing-directory
     :publishing-function  org-html-publish-to-html
     :exclude              "\\(README\\|setup\\).org"
     :fontify-natively     t
     :recursive            t
     :section-numbers      nil
     :html-doctype         "html5"
     :html-extension       "html"
     :htmlized-source      t
     :body-only            t)))

(defun org-img-follow (path)
  "Given a relative site `PATH', correct it for Emacs."
  (org-open-file-with-emacs (format "../../images/%s" path)))

(defun org-img-export (path desc format)
  "Given a relative site `PATH', `DESC'ription and `FORMAT' return an <IMG>.
N.B. This only works when `FORMAT' is 'html."
  (cond ((eq format 'html)
         (format "<img src=\"/images/%s\" alt=\"%s\" />" path desc))))

(org-add-link-type "img" 'org-img-follow 'org-img-export)

;;; blorg.el ends here
