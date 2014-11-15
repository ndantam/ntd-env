;; -*- mode: emacs-lisp -*-;;
;; Emacs initialization file
;; Author: Neil Dantam
;;
;; This file is released into the public domain.  There is absolutely
;; no warranty expressed or implied.


;;;;;;;;;
;; ORG ;;
;;;;;;;;;
(setq org-export-email-info nil)
(setq org-export-html-preamble nil)
(setq org-export-html-validation-link nil)
(setq org-publish-project-alist
      `(("web"
         :base-directory ,(expand-file-name "~/org/web")
         :publishing-function org-publish-org-to-html
         :base-extension "org"
         :style "<link rel=\"stylesheet\"
                     href=\"web/org.css\"
                     type=\"text/css\"/>
                 <script src=\"web/org.js\"
                     type=\"text/javascript\"></script>"
         :email ""
         :publishing-directory ,(expand-file-name "~/www/"))
        ("web.static"
         :base-directory ,(expand-file-name "~/org/")
         :publishing-function org-publish-attachment
         :base-extension "css|js"
         :recursive t
         :include ("web/org.css" "web/org.js"
                   "img/droidmacs.jpeg"
                   "img/yama.jpeg" "img/ntd-lwa3-chess.jpeg"
                   "img/hydrocar.jpeg")
         :email ""
         :publishing-directory ,(expand-file-name "~/www/"))))
