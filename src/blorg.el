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

(let* ((base-directory default-directory)
       (publishing-directory
        (concat (file-name-directory (directory-file-name base-directory))
                "hakyll/")))
  ;; Reference: http://orgmode.org/worg/org-tutorials/org-jekyll.html#text-4
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

;;; Evaluating the following will remove the "blorg" config from
;;; org-publish-project-alist so we can reset it. with the add-to-list
;;; form above.

;; (setq org-publish-project-alist
;;       (delete (assoc "blorg" org-publish-project-alist)
;;               org-publish-project-alist))

;;; The following is based on work by Kris Jekins and can be found on
;;; Stack Overflow: http://stackoverflow.com/a/14841597/1793234

;; TODO: Consider making the relative image path customizable.
(defun org-img-follow (path)
  "Given a relative site `PATH', correct it for Emacs."
  (org-open-file-with-emacs (format "../../images/%s" path)))

(defun org-img-export (path desc format)
  "Given a relative site `PATH', `DESC'ription and `FORMAT' return an <IMG>.
N.B. This only works when `FORMAT' is 'html."
  (cond ((eq format 'html)
         (format "<img src=\"/images/%s\" alt=\"%s\" />" path desc))))

;; Add "img" to the list of `org-link-types', facilitating proper handling of
;; e.g. [[img:cool.gif]] in blog posts.
(org-add-link-type "img" 'org-img-follow 'org-img-export)

;;; blorg.el ends here
