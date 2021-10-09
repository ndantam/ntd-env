;; -*- mode: emacs-lisp -*-;;
;; .emacs
;; Emacs initialization file
;; Author: Neil Dantam
;;
;; This file is released into the public domain.  There is absolutely
;; no warranty expressed or implied.

;;;;;;;;;;;;
;;  Text  ;;
;;;;;;;;;;;;
(defun ntd/emoticon (&optional mouth eyes )
  (let ((eyes-str (alist-get eyes
                             '((t          .  ":")
                               (normal     .  ":")
                               (big        .  "=")
                               (smiling    .  "8")
                               (squinting  .  "X")
                               (winking    .  ";")
                               (tears      .  ":'")
                               (sweat      . "':")
                               (halo       . "O:")
                               (horns      . ">:")
                               )))
        (mouth-str (alist-get mouth
                              '((t         . ")")
                                (smile     . ")")
                                (grinning  . "D")
                                (beaming   . "D")
                                (neutral   . "|")
                                (confused  . "/")
                                (kiss      . "*")
                                (blow-kiss . "x")
                                (tongue    . "p")))))
    (unless eyes-str (error "Unknown eyes: %s" eyes))
    (unless mouth-str (error "Unknown mouth: %s" mouth))
    (concat eyes-str mouth-str)))


;; TODO: would a translation table be better?
(defvar ntd/asciify-hash nil)

(let ((alist
       `(;; punctuation
         (?\  . ?\s)
         (?\‘ . ?\')
         (?\’ . ?\')
         (?\“ . ?\")
         (?\” . ?\")
         (?\… . "...")
         (?\– . "--")   ; en dash
         (?\— . "---")  ; em dash

         ;; Emoji -> emoticon
         ;; https://unicode.org/emoji/charts/full-emoji-list.html
         ;; https://www.unicode.org/charts/PDF/U1F600.pdf
         ;; https://en.wikipedia.org/wiki/List_of_emoticons

         ;; Faces
         (?😀 . ,(ntd/emoticon 'grinning 'normal))      ; U+1F600, GRINNING FACE
         (?😁 . ,(ntd/emoticon 'beaming 'smiling))      ; U+1F601, BEAMING FACE WITH SMILING EYES
         (?😂 . ,(ntd/emoticon 'smile 'tears))          ; U+1F602, FACE WITH TEARS OF JOY
         (?😃 . ,(ntd/emoticon 'grinning 'big))         ; U+1F603, GRINNING FACE WITH BIG EYES
         (?😄 . ,(ntd/emoticon 'grinning' smiling))     ; U+1F604, GRINNING FACE WITH SMILING EYES
         (?😅 . ,(ntd/emoticon 'grinning 'sweat))       ; U+1F605, GRINNING FACE WITH SWEAT
         (?😆 . ,(ntd/emoticon 'grinning 'squinting))   ; U+1F606, GRINNING SQUINTING FACE
         (?😇 . ,(ntd/emoticon 'smile 'halo))           ; U+1F607, SMILING FACE WITH HALO
         (?😈 . ,(ntd/emoticon 'smile 'horns))          ; U+1F608  SMILING FACE WITH HORNS
         (?😉 . ,(ntd/emoticon 'smile 'winking))        ; U+1F609, WINKING FACE
         (?😊 . ,(ntd/emoticon 'smile 'smiling))        ; U+1F60A, SMILING FACE WITH SMILING EYES

         (?😐 . ,(ntd/emoticon 'neutral 'normal))       ; U+1F610, NEUTRAL FACE
         (?😑 . ,(ntd/emoticon 'neutral 'normal))       ; U+1F611, EXPRESSIONLESS FACE

         (?😕 . ,(ntd/emoticon 'confused 'normal))      ; U+1F615, CONFUSED FACE
         (?😗 . ,(ntd/emoticon 'kiss 'normal))          ; U+1F617, KISSING FACE
         (?😘 . ,(ntd/emoticon 'blow-kiss 'normal))     ; U+1F618, FACE THROWING A KISS
         (?😙 . ,(ntd/emoticon 'kiss 'smiling))         ; U+1F619, KISSING FACE WITH SMILING EYES
         (?😚 . ,(ntd/emoticon 'kiss 'squinting))       ; U+1F61A, KISSING FACE WITH CLOSED EYES

         (?😛  . ,(ntd/emoticon 'tongue 'squinting))    ; 1F61B FACE WITH STUCK-OUT TONGUE
         (?😜  . ,(ntd/emoticon 'tongue 'winking))      ; 1F61C FACE WITH STUCK-OUT TONGUE AND WINKING EYE, (kidding, not serious)
         (?😝  . ,(ntd/emoticon 'tongue 'squinting))    ; 1F61D FACE WITH STUCK-OUT TONGUE AND TIGHTLY-CLOSED EYES

         (?🙂 . ,(ntd/emoticon 'smile 'normal))         ; U+1F642, SLIGHTLY SMILING FACE
         (?🙃 . "(:")                                   ; U+1F643, UPSIDE-DOWN FACE

         (?🤣 . " ROFL")                                ; U+1F923, ROLLING ON THE FLOOR LAUGHING
         ))
       ;; End Alist
      (hash (make-hash-table)))
  (dolist (x alist) (puthash (car x) (cdr x) hash))
  (setq ntd/asciify-hash hash))

(defun ntd/asciify-region (start end)
  (interactive)
  (let ((is-ascii t))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let* ((c (char-after))
               (newtext (gethash c ntd/asciify-hash)))
          (cond (newtext
                 (progn
                   (delete-char 1)
                   (insert newtext)))
                ((and is-ascii
                      (> c 127))
                 (print (format "Could not asciify character: `%c'" c))
                 (setq is-ascii nil)))
          (forward-char))))
      is-ascii))

;;;;;;;;;;;;;;;;;;
;;  Wanderlust  ;;
;;;;;;;;;;;;;;;;;;

;; (add-to-list 'load-path
;;              "~/git/3rdparty/wanderlust/wl/")
;; (add-to-list 'load-path
;;              "~/git/3rdparty/wanderlust/elmo/")

(defun ntd-email-addr (a b c)
  (concatenate 'string a "@" b "." c))

(setq user-mail-address (ntd-email-addr "ndantam" "mines" "edu"))
(setq wl-user-mail-address-list (list user-mail-address))

(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

(defun my-wl-summary-sort-hook ()
  (wl-summary-rescan "date"))

(add-hook 'wl-summary-prepared-hook 'my-wl-summary-sort-hook)


(defun ntd/fill-mail ()
  (interactive)
  (save-excursion
    (mail-text)
    (fill-region (point)
                 (point-max))))


;; SEE ALSO: ~/.wl
