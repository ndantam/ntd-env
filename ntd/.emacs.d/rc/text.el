;; -*- mode: emacs-lisp -*-;;
;; Emacs initialization file
;; Author: Neil Dantam
;;
;; This file is released into the public domain.  There is absolutely
;; no warranty expressed or implied.


;;;;;;;;;;;;
;;  TEXT  ;;
;;;;;;;;;;;;
;; (add-hook 'text-mode-hook 'turn-on-auto-fill)
;; (add-hook 'text-mode-hook 'flyspell-mode)


(defun ntd/text-mode-hook ()
  (turn-on-auto-fill)
  (flyspell-mode)
  ;(flymake-mode)
  ;(flymake-proselint-setup)
  )


(add-hook 'text-mode-hook
          #'ntd/text-mode-hook)


(defun ntd/alist-hash (alist)
  (let ((hash (make-hash-table :test #'equal)))
    (dolist (x alist) (puthash (car x) (cdr x) hash))
    hash))

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
         (,(aref  "​" 0) . "") ;; zero-width space
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
         )))
  (setq ntd/asciify-hash  (ntd/alist-hash  alist)))

(defun ntd/strlen (thing)
  (etypecase thing
    (integer 1)
    (string (length thing))))

(defun ntd/asciify-region (start end)
  (interactive "r")
  (let ((is-ascii t))
    (save-excursion
      (goto-char start)
      (while (re-search-forward (rx nonascii) end t)
        (let ((c (char-before)))
          ;;(print (format "Trying to asciify character: `%c' (%d)" c c))
          (if-let ((newtext (gethash c ntd/asciify-hash)))
              (progn
                (delete-char -1)
                (insert newtext))
            (print (format "Could not asciify character: `%c' (%d)" c c))
            (setq is-ascii nil)))))
    is-ascii))


(progn
  (defvar ntd/style-alist
    `(;; Contractions
      ("isn't" . "is not")
      ("wasn't" . "was not")
      ("aren't" . "are not")
      ("hasn't" . "has not")
      ("doesn't" . "does not")
      ;; Grammar
      ("lidar" . "LIDAR")
      ("boolean" . "Boolean")
      ;; Style and brevity
      ("depends upon" . "depends on")
      ("is dependent on" . "depends on")
      ("based upon" . "based on")
      ("must have" . "need") ;; plurality
      ("find a solution to" . "solve")
      ("in line with" . "consistent with")
      ("In line with" . "Consistent with")
      ("lead to" . "cause") ;; yield, produce
      ))

  (defvar ntd/style-hash (ntd/alist-hash  ntd/style-alist))

  (defvar ntd/style-re
    (rx-to-string `(or ,@(mapcar #'car ntd/style-alist)))))

(defun ntd/stylize-region (start end)
  (interactive "r")
  (save-excursion
    (goto-char start)
    (while (re-search-forward ntd/style-re end t)
      (let* ((oldtext (match-string 0))
             (newtext (gethash oldtext ntd/style-hash)))
        (assert newtext)
        ;; (print (format "Trying to styleize: `%s' -> `%s'" oldtext newtext))
        (delete-char (- (length oldtext)))
        (insert newtext)
        (forward-char (length newtext))))))

(defun ntd/texify-region (start end)
  (interactive "r")
  ;; quotes
  (save-excursion
    (goto-char start)
    (while (re-search-forward "\"\\([^\"]*\\)\"" end t)
      (replace-match "``\\1''"))))
