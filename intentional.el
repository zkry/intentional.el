;;; intentional.el --- UI for configuring what sites you can visit -*- lexical-binding: t -*-

;; Author: Zachary Romero
;; Maintainer: Zachary Romero
;; Version: 0.1.0
;; Package-Requires: ()
;; Homepage: https://github.com/zkry/intentional
;; Keywords: productivity


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;;

(require 'org-clock)
(require 'f)

;;; Code:
(defconst intentional--buffer-name "*intentional*" "Name for intentional buffer.")

(defconst intentional-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (suppress-keymap map)
      (define-key map "q" #'quit-window)
      (define-key map "a" #'intentional-add-temporary-intention)
      (define-key map "m" #'intentional-modify-intention-at-point)
      (define-key map "c" #'intentional-add-clock-intention)
      (define-key map "g" #'intentional-refresh-buffer)
      (define-key map "d" #'intentional-delete-temporary-intention)
      (define-key map "?" #'intentional-display-help)))
  "Keymap for intentional-mode.")

(defvar intentional-file-name
  "~/browse-intentions.json"
  "The name of the file to write browser intentions to.")

(defface intentional-section-heading-face
  '((((class color) (background light)) (:foreground "DarkGoldenrod4"))
    (((class color) (background dark))  (:foreground "DarkGoldenrod1")))
  "Face used to display section headings."
  :group 'intentional)

(defface intentional-active-face
  '((((class color) (background light)) (:foreground "#090"))
    (((class color) (background dark))  (:foreground "#afa")))
  "Face used to display intentions that are currently in effect."
  :group 'intentional)

(defface intentional-inactive-face
  '((((class color) (background light)) (:foreground "#ccc"))
    (((class color) (background dark))  (:foreground "#555")))
  "Face used to display intentions that are not in effect."
  :group 'intentional)

;;; The following variables are for containing the various browser
;;; intentions and should be replaced.
;;; Declarations should take the following form:
;;; (name activation-spec permission-list)
(defvar intentional-global-intentions
  '(("Work on-call" always ("https://*.pagerduty.com/*"))
    ("Work" (between-on-days "08:00" "18:00" "MTuWThF")
     ("https://*.atlassian.net/*"
      "https://github.com/*"
      "https://outlook.office.com/*"
      "https://golang.org/*"))
    ("Relax" (between "19:00" "21:00") ("https://youtube.com/*")))
  "List of intentions that are constantly present.")

(defvar intentional-local-intentions
  '(("Mail my mom" (at (24494 59753 933797 0)) ("https://mail.google.com/*"))))


(defvar intentional-output-file
  "~/browse-intentions.json"
  "The name of the file to output JSON to.")

(defvar intentional-site-groups
  '(("clojure" "https://clojuredocs.org/*" "https://clojure.org/*" "https://cljdoc.org/*" "https://github.com/*" "https://clojureverse.org/*" "https://stackoverflow.com/*")
    ("golang" "https://golang.org/*" "https://github.com/*" "https://godoc.org/*" "https://stackoverflow.com/*"))
  "List of preset groups to add groups of sites easily.")

(defcustom intentional-data-file
  (expand-file-name "intentional-data.eld" user-emacs-directory)
  "Name and location of file to store extension data."
  :group 'intentional
  :type 'string)

(defun intentional ()
  "Open the UI for viewing/configuring current intentions."
  (interactive)
  (switch-to-buffer intentional--buffer-name)

  (if (equal mode-name "intentional")
      (intentional-refresh-buffer)
    (intentional-mode)))

(defun intentional--remove-expired-intentions ()
  "Remove all temporary intentions that have expired."
  (setq intentional-local-intentions
        (seq-filter (lambda (intn)
                      (intentional--active-p (cadr intn)))
                    intentional-local-intentions)))


(defun intentional--remove-temporary-intention (name)
  "Remove the temporary intention with NAME."
  ;; TODO I can probably delete this as temporary intentions aren't
  ;; persisted and when they're created they always have a GC func called.
  (setq intentional-local-intentions
        (seq-filter (lambda (intn)
                      (not (equal (car intn) name)))
                    intentional-local-intentions)))

(defun intentional--fetch-group-sites (name)
  "Return list of sites from site groups by NAME."
  (let ((item (seq-find (lambda (l) (equal (car l) name)) intentional-site-groups)))
    (cdr item)))

(defun intentional-delete-temporary-intention ()
  "Delete the intention found at current point."
  (interactive)
  (let ((intention-name (get-text-property (point) 'intentional-item)))
    (unless intention-name
      (error "No temporary intention found under current point"))
    (intentional--remove-temporary-intention intention-name)
    (intentional-refresh-buffer)
    (intentional--write-intentions-to-file)))

(defun intentional-add-temporary-intention (name expire)
  "Add a temporary intention with NAME, expiray of EXPIRE, and allow SITE."
  (interactive "sWhat do you intend to do? \nsExpiry: ")
  ;; TODO: If name already exists throw error
  (let* ((allow-site-str (completing-read "Allowed site pattern (or group ID): "
                                          (mapcar 'car intentional-site-groups)))
         (allow-site (or (if (string-prefix-p "http" allow-site-str)
                             (list allow-site-str)
                           (and (intentional--fetch-group-sites allow-site-str)
                                (list allow-site-str)))
                         (error "No group found")))
         (name (string-trim name))
         (time-p (string-match "[[:digit:]]?[[:digit:]]:[[:digit:]][[:digit:]]" expire)))
    (if time-p
        ;;(add-to-list 'intentional-local-intentions `(,name (at ,expire) ,allow-site) t)
        (error "Not implemented")
      (let* ((mins-later (org-duration-to-minutes expire))
             (secs-later (* 60 mins-later))
             (later-time (+ (* mins-later 60) (time-convert nil 'integer))))
        (add-to-list 'intentional-local-intentions `(,name (at ,later-time) ,allow-site))
        (run-with-timer secs-later nil
                        (lambda ()
                          (intentional--remove-temporary-intention name)
                          (intentional-refresh-buffer)
                          (intentional--write-intentions-to-file))))))
  (intentional-refresh-buffer)
  (intentional--write-intentions-to-file))

(defun intentional-modify-intention-at-point ()
  "Prompt the user to change the site of intention at point."
  (interactive)
  (unless (equal mode-name "intentional")
    (error "Not in intentional buffer"))
  (let ((section-name (intentional--section-name))
        (intention-name (get-text-property (point) 'intentional-item)))
    (when (or (not section-name) (not (equal section-name 'temporary)) (not intention-name))
      (error "No temporary intention found at point"))
    (let ((intention (seq-find (lambda (intn)
                                 (equal (car intn) intention-name))
                               intentional-local-intentions)))
      (when (not intention) (error "No intention found"))
      (let ((new-site (read-string "New site" (car (nth 2 intention)))))
        (setq intentional-local-intentions
              (seq-map (lambda (intn)
                         (if (equal (car intn) intention-name)
                             (let ((active (cadr intn)))
                               `(,intention-name ,active (,new-site)))
                           intn))
                       intentional-local-intentions))
        (intentional-refresh-buffer)
        (intentional--write-intentions-to-file)))))

(defun intentional-add-clock-intention ()
  "Add an intention property SITE item to the currently clocked in task."
  (interactive)
  (unless org-clock-current-task
    (error "No item is currently being clocked"))
  (let ((site (completing-read "Allowed site pattern (or URL): " (mapcar 'car intentional-site-groups))))
    (intentional--add-org-clock-site site)
    (intentional-refresh-buffer)
    (intentional--write-intentions-to-file)))

(defun intentional--resolve-allow-list (list)
  "Replace groups in LIST with sites that group refers to."
  (when list
    (let* ((first (car list))
           (url-p (string-prefix-p "http" first)))
      (if url-p
          (cons first (intentional--resolve-allow-list (cdr list)))
        (let ((resolved (intentional--fetch-group-sites first)))
          (if resolved
              (append resolved (intentional--resolve-allow-list (cdr list)))
            (append first (intentional--resolve-allow-list (cdr list)))))))))

(defun intentional--write-intentions-to-file ()
  "Write all configured intentions to the configured file."
  (intentional--remove-temporary-intention)
  (let* ((data (make-hash-table))
         (groups-data (make-hash-table))
         (clock-allows (intentional--org-clock-allow-list))
         (filtered-global-intentions (intentional--filter-active intentional-global-intentions))
         (intentions (make-vector (+ (if (zerop (length clock-allows)) 0 1)
                                     (length filtered-global-intentions)
                                     (length intentional-local-intentions))
                                  nil))
         (i 0))
    (seq-do (lambda (intn)
              (let ((intn-hash (make-hash-table)))
                (pcase-let ((`(,name ,active ,allow-list) intn))
                  (puthash "name" name intn-hash)
                  (puthash "allowed_sites" (apply 'vector (intentional--resolve-allow-list allow-list)) intn-hash))
                (aset intentions i intn-hash)
                (setq i (1+ i))))
            filtered-global-intentions)
    (seq-do (lambda (intn)
              (let ((intn-hash (make-hash-table)))
                (pcase-let ((`(,name ,active ,allow-list) intn))
                  (puthash "name" name intn-hash)
                  (puthash "allowed_sites" (apply 'vector (intentional--resolve-allow-list allow-list)) intn-hash))
                (aset intentions i intn-hash)
                (setq i (1+ i))))
            intentional-local-intentions)
    (when (> (length clock-allows) 0)
      (let ((intn-hash (make-hash-table)))
        (puthash "name" "clock" intn-hash)
        (puthash "allowed_sites" (apply #'vector (intentional--resolve-allow-list clock-allows)) intn-hash)
        (aset intentions i intn-hash)
        (setq i (1+ i))))
    (puthash "groups" groups-data data)
    (puthash "intentions" intentions data)
    (let ((json-output (json-serialize data)))
      (f-write-text json-output 'utf-8 intentional-output-file))))

(defun intentional--now-between-time (start end)
  "Return non-nil if now is between START and END times."
  (let* ((start-ok (or (string-match "\\([[:digit:]]?[[:digit:]]\\):\\([[:digit:]][[:digit:]]\\)" start)
                       (error "Bad formatted start time")))
         (start (+ (* (string-to-number (match-string 1 start)) 60)
                     (string-to-number (match-string 2 start))))
         (end-ok (or (string-match "\\([[:digit:]]?[[:digit:]]\\):\\([[:digit:]][[:digit:]]\\)" end)
                     (error "Bad formatted start time")))
         (end (+ (* (string-to-number (match-string 1 end)) 60)
                 (string-to-number (match-string 2 end))))
         (now (decode-time))
         (now (+ (* (nth 2 now) 60)
                 (nth 1 now))))
    (if (< start end)
        (< start now end)
      (or (< start now)
          (< now end)))))

(defun intentional--active-p (a)
  "Return non-nil if A is active now."
  (pcase a
    ('always t)
    (`(between ,start ,end) (intentional--now-between-time start end))
    (`(at ,time) (time-less-p (current-time) time))
    (`(between-on-days ,start ,end ,days) (intentional--now-between-time start end))))

(defun intentional--filter-active (intns)
  "Filter a list of intentions INTNS by whether they are active or not."
  (seq-filter (lambda (intn)
                (pcase-let ((`(,name ,active ,allow-list) intn))
                  (intentional--active-p active)))
              intns))

;;; Display ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun intentional--active-req->string (req)
  "Return the string format of active requirement spec REQ."
  (pcase req
    ('always "always")
    (`(between ,start ,end) (format "%s-%s" start end))
    (`(between-on-days ,start ,end ,days) (format "%s-%s %s" start end days))
    (`(at ,ts) (format "at %s" (format-time-string "%H:%M" ts)))))

(defun intentional--face-name-for-status (active-p)
  "Return the face to use given active status, ACTIVE-P, of an intention."
  (if active-p
      'intentional-active-face
    'intentional-inactive-face))

(defun intentional--format-allow-list-shortened (list)
  "Return an abbreviated string to display to user of site LIST."
  (if (= 1 (length list))
      (let* ((item (car list))
             (url-p (string-prefix-p "http" item))
             (group-found-p (intentional--fetch-group-sites item)))
        (cond
         ((or url-p group-found-p) item)
         (t (propertize item 'face 'intentional-inactive-face))))
    (format "%s and %d more" (car list) (1- (length list)))))

(defun intentional-display-help ()
  "Display a help message showing available commands."
  (interactive)
  (message (string-join '("  c  add intention for clocked item"
                          "  a  add temporary intention"
                          "  d  delete intention at point")
                        "\n")))

(defun intentional--display ()
  "Display the browser intentions in mode buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (propertize "Global Intentions\n" 'face 'intentional-section-heading-face))
    (seq-do (lambda (intn)
              (pcase-let ((`(,name ,active ,allow-list) intn))
                (let* ((active-p (intentional--active-p active))
                       (name-str (format "%-25s" name))
                       (active-str (format "%-25s" (intentional--active-req->string active)))
                       (allow-str (intentional--format-allow-list-shortened allow-list))
                       (line (format "   %s %s %s\n" name-str active-str allow-str)))
                  (insert (propertize line 'face (intentional--face-name-for-status active-p))))))
            intentional-global-intentions)
    (insert "\n\n")
    (insert (propertize "org-clock Intentions\n" 'face 'intentional-section-heading-face))
    (if (not org-clock-current-task)
        (insert "   no clock item\n")
      (let* ((name-str (format "%-25s" (intentional--org-clock-name)))
             (allow-str (intentional--format-allow-list-shortened (intentional--org-clock-allow-list)))
             (line (format "   %s %25s %s\n" name-str "" allow-str)))
        (insert (propertize line 'face 'intentional-active-face))))
    (insert "\n\n")
    (insert (propertize "Temporary Intentions\n" 'face 'intentional-section-heading-face))
    (if (not intentional-local-intentions)
        (insert "   no local intentions")
      (seq-do (lambda (intn)
                (pcase-let ((`(,name ,active ,allow-list) intn))
                  (let* ((name-str (format "%-25s" name))
                         (active-str (format "%-25s" (intentional--active-req->string active)))
                         (allow-str (intentional--format-allow-list-shortened allow-list))
                         (line (format "   %s %s %s\n" name-str active-str allow-str)))
                    (insert (propertize line
                                        'face 'intentional-active-face
                                        'intentional-item name)))))
              intentional-local-intentions))))

(defun intentional--section-name ()
  "Return the section that the current point is in or nil if in no section."
  (unless (equal mode-name "intentional")
    (error "Current buffer not in intentional mode"))
  (save-excursion
    (if (equal "" (string-trim (thing-at-point 'line)))
        nil
      (beginning-of-line 1)
      (while (looking-at " ") (forward-line -1))
      (let ((section-string (string-trim (thing-at-point 'line))))
        (cond
         ((equal section-string "Global Intentions") 'global)
         ((equal section-string "org-clock Intentions") 'org-clock)
         ((equal section-string "Temporary Intentions") 'temporary))))))


;;; External Integrations ;;;;;;;;;;;;;;;;;;;;;;
(defun intentional--org-clock-name ()
  "Retrieve name of current org clock item."
  (substring-no-properties org-clock-current-task))

(defun intentional--org-clock-allow-list ()
  "Retrieve the list of allowed sites from the current org-clock item."
  (when org-clock-current-task
    (save-window-excursion
      (save-excursion ; TODO I needed these nested save excursions so point doesn't get moved if
                      ; I have the todos buffer open when called.
        (org-clock-goto)
        (let* ((allows (org-entry-get (point) "INTENTIONAL_ALLOW"))
               (items (split-string allows " ")))
          items)))))

(defun intentional--add-org-clock-site (site)
  "Add SITE to intentional allow property."
  (unless org-clock-current-taks
    (error "No current clocked in task"))
  (save-window-excursion
    (org-clock-goto)
    (let* ((allows (org-entry-get (point) "INTENTIONAL_ALLOW"))
           (new-val (if allows (concat allows " " site) site)))
      (org-set-property "INTENTIONAL_ALLOW" new-val))))

(defun intentional--clock-update ()
  "Perform all actions that should occurr whith change in clock."
  (intentional--write-intentions-to-file)
  (intentional-refresh-buffer))

(add-hook 'org-clock-in-hook 'intentional--clock-update)
(add-hook 'org-clock-out-hook 'intentional--clock-update)
(add-hook 'org-clock-cancel-hook 'intentional--clock-update)

(defun intentional-mode ()
  "Major mode for viewing/editing browse-intentions.json file."
  (interactive)
  (kill-all-local-variables)
  (use-local-map intentional-mode-map)
  (setq mode-name "intentional"
        buffer-read-only t
        truncate-lines t)
  (buffer-disable-undo)
  (hl-line-mode)
  (intentional--display))

(defun intentional-refresh-buffer ()
  "Re-draw the intentional buffer."
  (interactive)
  (when (get-buffer intentional--buffer-name) ;; only refresh if buffer exists
    (with-current-buffer intentional--buffer-name
      (let ((inhibit-read-only t))
        (intentional--display)))))

(defvar intentional--auto-save-timer nil
  "The timer for writing the config file in the background.")

(define-minor-mode intentional-minor-mode
  "Minor mode for updating the config file in the background."
  nil " Intentional" nil
  :global t
  (when intentional--auto-save-timer
    (cancel-timer intentional--auto-save-timer))
  (setq intentional--auto-save-timer
        (run-with-timer
         60 60
         (lambda ()
           (run-with-idle-timer 1 nil (lambda () (intentional--write-intentions-to-file)))))))

(provide 'intentional)

;;; intentional.el ends here
