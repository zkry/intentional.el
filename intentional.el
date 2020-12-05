;;; intentional.el --- UI for configuring what sites you can visit -*- lexical-binding: t -*-

;; Copyright © 2020 Zachary Romero <zacromero@posteo.net>

;; Author: Zachary Romero <zacromero@posteo.net>
;; Maintainer: Zachary Romero
;; Version: 0.1.0
;; Package-Requires: ((org "9.3") (f "0.17.2"))
;; Homepage: https://github.com/zkry/intentional
;; Keywords: productivity, org-mode


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
;; This file contains all of the code necessary to figure out what
;; "intentions" are currently active, provide interactive interface
;; to manipulate intentions, and write them to a file.
;;
;;; Code:
;;;; Library Requires
(require 'org-clock)
(require 'f)

(defvar intentional--allow-list-history nil
  "History var containing previous used items for allow list.")
(defvar intentional--expiry-history nil
  "History var containing entered durations.")

(defconst intentional--buffer-name "*intentional*" "Name for intentional buffer.")

(defcustom intentional-save-to-journal
  t
  "If non-nil, write all added temporary intentions to journal."
  :group 'intentional
  :type 'boolean)

(defcustom intentional-extract-clock-body-urls
  t
  "If non-nil, in addition to the INTENTIONAL_ALLOW property,allow all urls found in body."
  :group 'intentional
  :type 'boolean)

(defconst intentional-mode-map
  (let ((map (make-sparse-keymap)))
    (prog1 map
      (suppress-keymap map)
      (define-key map "q" #'quit-window)
      (define-key map "t" #'intentional-add-temporary-intention)
      (define-key map "a" #'intentional-add-site-to-intention)
      (define-key map "m" #'intentional-modify-intention-at-point)
      (define-key map "c" #'intentional-add-clock-intention)
      (define-key map "g" #'intentional-refresh-buffer)
      (define-key map "d" #'intentional-delete-temporary-intention)
      (define-key map "n" #'intentional-next-item)
      (define-key map "p" #'intentional-previous-item)
      (define-key map "?" #'intentional-display-help)))
  "Keymap for intentional-mode.")


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

(defvar intentional-tag-intentions
  nil
  "List of intentions that activate when a tag is on the current clocked item.

This variable should take the form '((tag-string (allow-site ...)) ...).")

;;; The following variables are for containing the various browser
;;; intentions and should be replaced.
;;; Declarations should take the following form:
;;; (name activation-spec permission-list)
(defvar intentional-global-intentions
  nil
  "List of intentions that are constantly present.")

(defvar intentional-local-intentions
  nil)


(defvar intentional-output-file
  "~/browse-intentions.json"
  "The name of the file to output JSON to.")

(defvar intentional-site-groups
  nil
  "List of preset groups to add groups of sites easily.

This should be set to a list of lists whose first element is the
name of the group and rest are the site patterns.")

(defun intentional--global-intentions-update-watcher ()
  "Perform refreshing actions needed when global intention list has changed."
  (intentional--write-intentions-to-file)
  (intentional-refresh-buffer))

(add-variable-watcher 'intentional-global-intentions
                      (lambda (symbol newval operation where)
                        (run-with-idle-timer 1 nil #'intentional--global-intentions-update-watcher)))

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

(defun intentional--allow-list-from-tag (tag1)
  "Retrieve all allowed items for TAG1."
  (cadar (seq-filter (lambda (tag-item)
                      (pcase-let ((`(,tag2 ,a)  tag-item))
                        (equal tag1 tag2)))
                   intentional-tag-intentions)))

(defun intentional--allow-list-from-tags (tags)
  "Return all allow sites for all TAGS."
  (let ((allow-list '()))
    (dolist (tag tags allow-list)
      (let ((associated-allows (intentional--allow-list-from-tag tag)))
        (when associated-allows
          (setq allow-list (append allow-list associated-allows)))))))

(defun intentional-delete-temporary-intention ()
  "Delete the intention found at current point."
  (interactive)
  (let ((intention-name (get-text-property (point) 'intentional-item)))
    (unless intention-name
      (error "No temporary intention found under current point"))
    (intentional--remove-temporary-intention intention-name)
    (intentional-refresh-buffer)
    (intentional--write-intentions-to-file)))

(defun intentional--read-allowed-site-patter ()
  "Perform a completing read for allowed site."
  (completing-read "Allowed site pattern (or group ID): "
                   (append (mapcar 'car intentional-site-groups) intentional--allow-list-history)
                   nil
                   nil
                   nil
                   'intentional--allow-list-history))

(defun intentional--parse-duration-string (expire)
  "Determine how minutes later EXPIRE string indicates."
  (org-duration-to-minutes expire))

(defun intentional--parse-time-string (expire)
  "Determine how many minutes until the time string EXPIRE."
  (let* ((expire-parts (split-string expire ":"))
         (m (+ (* 60 (string-to-number (car expire-parts)))
               (string-to-number (cadr expire-parts))))
         (now (decode-time))
         (now-m (+ (* 60 (nth 2 now))
                   (nth 1 now))))
    (cond
     ((> m now-m) (- m now-m))
     ((= m now-m) (* 60 24))
     ((< m now-m) (- (+ m (* 60 24)) now-m)))))

(defun intentional--read-duration-string ()
  "Prompt the user for a valid duration string repeatedly asking if invalid.

Returns a list who's fist element is the time that it should end
and second element is how many seconds later the intention should end."
  (let ((later-time nil)
        (is-error nil)
        (valid))
    (while (not later-time)
      (condition-case nil
          (let* ((prompt (if is-error "Bad Input. Please enter duration (ex. `10m' `1h'): " "Expiry: "))
                 (expire (read-string prompt nil intentional--expiry-history))
                 (expire (if (string-match "^[[:digit:]]+m$" expire)
                             (concat expire "in")
                           expire))
                 (mins-later (if (string-match "[012]?[0-9]:[0-9][0-9]" expire)
                                 (intentional--parse-time-string expire)
                               (intentional--parse-duration-string expire)))
                 (secs-later (* 60 mins-later))
                 (later (+ (* mins-later 60) (time-convert nil 'integer))))
            (setq later-time (list later secs-later)))
        (error (setq is-error t))))
    later-time))

(defun intentional-add-temporary-intention (name)
  "Add a temporary intention with NAME, expiray of EXPIRE, and allow SITE."
  (interactive "sWhat do you intend to do? ")
  ;; TODO: If name already exists throw error
  (let* ((later+secs (intentional--read-duration-string))
         (later-time (car later+secs))
         (secs-later (cadr later+secs))
         (allow-site-str (intentional--read-allowed-site-patter))
         (allow-site (list allow-site-str))
         (name (string-trim name)))
    (add-to-list 'intentional-local-intentions `(,name (at ,later-time) ,allow-site))
    (run-with-timer secs-later nil
                    (lambda ()
                      (intentional--remove-temporary-intention name)
                      (intentional-refresh-buffer)
                      (intentional--write-intentions-to-file)))
    (when intentional-save-to-journal
      (intentional--write-to-journal name allow-site-str)))
  (intentional-refresh-buffer)
  (intentional--write-intentions-to-file))

(defun intentional-add-site-to-intention ()
  "Add a site to the intention at point."
  (interactive)
  (let ((section (get-text-property (point) 'intentional-section))
        (item-name (get-text-property (point) 'intentional-item)))
    (unless section (error "Nothing found under point"))
    (cond
     ((equal 'temporary section)
      (let ((site-str (intentional--read-allowed-site-patter)))
        (setq intentional-local-intentions
              (seq-map (lambda (item)
                         (let ((name (car item))
                               (active (cadr item))
                               (sites (caddr item)))
                           (if (string-equal name item-name)
                               (list name active (cons site-str sites))
                             item)))
                       intentional-local-intentions))
        (intentional-refresh-buffer)
        (intentional--write-intentions-to-file)))
     ((equal 'org-clock section)
      (intentional-add-clock-intention))
     (t (error "Not implemented")))))

(defun intentional-modify-intention-at-point ()
  "Prompt the user to change the site of intention at point."
  (interactive)
  (unless (equal mode-name "intentional")
    (error "Not in intentional buffer"))
  (let ((section-name (get-text-property (point) 'intentional-section))
        (intention-name (get-text-property (point) 'intentional-item)))
    (when (or (not section-name) (not (equal section-name 'temporary)) (not intention-name))
      (error "No temporary intention found at point"))
    (let ((intention (seq-find (lambda (intn)
                                 (equal (car intn) intention-name))
                               intentional-local-intentions)))
      (when (not intention) (error "No intention found"))
      (let ((new-site (read-string "New site " (car (nth 2 intention))))
            (rest-sites (cdr (nth 2 intention))))
        (setq intentional-local-intentions
              (seq-map (lambda (intn)
                         (if (equal (car intn) intention-name)
                             (let ((active (cadr intn)))
                               `(,intention-name ,active ,(cons new-site rest-sites)))
                           intn))
                       intentional-local-intentions))
        (intentional-refresh-buffer)
        (intentional--write-intentions-to-file)))))

(defun intentional-add-clock-intention ()
  "Add an intention property SITE item to the currently clocked in task."
  (interactive)
  (unless org-clock-current-task
    (error "No item is currently being clocked"))
  (let ((site (intentional--read-allowed-site-patter)))
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
            (cons first (intentional--resolve-allow-list (cdr list)))))))))

(defun intentional--write-intentions-to-file ()
  "Write all configured intentions to the configured file."
  ;;(intentional--remove-temporary-intention)
  (message "writing intentions to file")
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
    (`(between-on-days ,start ,end ,days)
     (and (intentional--now-between-time start end)
          (member (nth 6 (decode-time (current-time))) days)))))

(defun intentional--filter-active (intns)
  "Filter a list of intentions INTNS by whether they are active or not."
  (seq-filter (lambda (intn)
                (pcase-let ((`(,name ,active ,allow-list) intn))
                  (intentional--active-p active)))
              intns))

(defun intentional--all-sites-list ()
  "Return a list of all allowed sites."
  (let* ((all-intns (append (intentional--filter-active intentional-global-intentions)
                            intentional-local-intentions) )
         (sites (apply #'append (mapcar (lambda (intn) (caddr intn)) all-intns)))
         (clock-sites (intentional--org-clock-allow-list)))
    (append sites clock-sites)))

;;; Display ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun intentional--format-dow (dow)
  "Format days of week list DOW to be human readable."
  (let ((sorted (sort dow '<))
        (out ""))
    (dotimes (n 7 out)
      (if (and sorted (= (car sorted) n))
          (progn
            (setq out (concat out (cond ((= 0 n) "S") ((= 1 n) "M") ((= 2 n) "T") ((= 3 n) "W")
                                        ((= 4 n) "T") ((= 5 n) "F") ((= 6 n) "S"))))
            (setq sorted (cdr sorted)))
        (setq out (concat out "_")))
      sorted)))

(defun intentional--active-req->string (req)
  "Return the string format of active requirement spec REQ."
  (pcase req
    ('always "always")
    (`(between ,start ,end) (format "%s-%s" start end))
    (`(between-on-days ,start ,end ,days) (format "%s-%s %s" start end (intentional--format-dow days)))
    (`(at ,ts)
     (let ((days-later (/ (float-time (time-subtract ts (current-time))) 60 60 24)))
       (if (> days-later 1)
           (format "at %s+%dd" (format-time-string "%H:%M" ts) (floor days-later))
         (format "at %s" (format-time-string "%H:%M" ts)))))))

(defun intentional--face-name-for-status (active-p)
  "Return the face to use given active status, ACTIVE-P, of an intention."
  (if active-p
      'intentional-active-face
    'intentional-inactive-face))

(defun intentional--format-allow-list-shortened (list)
  "Return an abbreviated string to display to user of site LIST."
  (cond
   ((= 1 (length list))
    (let* ((item (car list))
           (url-p (string-prefix-p "http" item))
           (group-found-p (intentional--fetch-group-sites item)))
      (cond
       ((or url-p group-found-p t) item) ;; TODO: figure out this logic so not t
       (t (propertize item 'face 'intentional-inactive-face)))))
   ((= 0 (length list)) "none")
   (t (format "%s and %d more" (car list) (1- (length list))))))

(defun intentional-next-item ()
  "Move point to next item."
  (interactive)
  (forward-line 1)
  (while (and (not (= (point) (point-max)))
              (or (looking-at " ")
                  (looking-at "\n")
                  (not (get-text-property (point) 'intentional-section))))
    (forward-char))
  (while (not (get-text-property (point) 'intentional-section))
    (intentional-previous-item)))

(defun intentional-previous-item ()
  "Move point to next item."
  (interactive)
  (forward-line -1)
  (while (or (looking-at "\n") (not (get-text-property (point) 'intentional-section)))
    (forward-line -1))
  (while (looking-at " ")
    (forward-char 1)))

(defun intentional-display-help ()
  "Display a help message showing available commands."
  (interactive)
  (message (string-join '("  q  quit window"
                          "  t  add temporary intention"
                          "  a  add site to intention at point"
                          "  m  modify intention at point"
                          "  c  add intention for clocked item"
                          "  g  refresh buffer"
                          "  d  delete temporary intention"
                          "  n  next item"
                          "  p  previous item"
                          "  ?  show help")
                        "\n")))

(defun intentional--display ()
  "Display the browser intentions in mode buffer."
  (let ((inhibit-read-only t)
        (line (line-number-at-pos)))
    (erase-buffer)
    (let ((gi-section-start (point)))
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
      (add-text-properties gi-section-start (point) '(intentional-section
                                                      global)))

    (insert "\n\n")
    (let ((org-clock-section-start (point)))
      (insert (propertize "org-clock Intentions\n" 'face 'intentional-section-heading-face))
      (if (not org-clock-current-task)
          (insert "   no clock item\n")
        (let* ((name-str (format "%-25s" (intentional--org-clock-name)))
               (allow-str (intentional--format-allow-list-shortened (intentional--org-clock-allow-list)))
               (line (format "   %s %25s %s\n" name-str "" allow-str)))
          (insert (propertize line 'face 'intentional-active-face))))
      (add-text-properties org-clock-section-start (point) '(intentional-section org-clock)))

    (insert "\n\n")
    (let ((temporary-section-start (point)))
      (insert (propertize "Temporary Intentions\n" 'face 'intentional-section-heading-face))
      (if (not intentional-local-intentions)
          (insert "   no local intentions")
        (seq-do (lambda (intn)
                  (pcase-let ((`(,name ,active ,allow-list) intn))
                    (let* ((name-str (format "%-25s" (truncate-string-to-width name 25 0 nil t)))
                           (active-str (format "%-25s" (intentional--active-req->string active)))
                           (allow-str (intentional--format-allow-list-shortened allow-list))
                           (line (format "   %s %s %s\n" name-str active-str allow-str)))
                      (insert (propertize line
                                          'face 'intentional-active-face
                                          'intentional-item name)))))
                intentional-local-intentions))
      (add-text-properties temporary-section-start (point) '(intentional-section temporary)))

    ;; list of all sites
    (let ((all-sites (intentional--all-sites-list)))
      (insert (propertize (format "\n\nAll Sites (%d):\n" (length all-sites))
                          'face 'intentional-inactive-face))
      (seq-do (lambda (site)
                (insert "   " (propertize site 'face 'intentional-inactive-face) "\n"))
              all-sites))
    (goto-char (point-min))

    (if line
        (progn (goto-line line)
               (while (looking-at " ")
                 (forward-char 1)))
      (goto-char (point-max))
      (intentional-previous-item))))


;;; External Integrations ;;;;;;;;;;;;;;;;;;;;;;
(defun intentional--org-clock-name ()
  "Retrieve name of current org clock item."
  (substring-no-properties org-clock-current-task))


(defun intentional--org-clock-allow-list ()
  "Retrieve the list of allowed sites from the current org-clock item."
  (when org-clock-current-task
    (let* ((clock-data (intentional--org-clock-fetch-data))
           (allow-list (car clock-data))
           (tag-list (cadr clock-data))
           (tag-allow-list (intentional--allow-list-from-tags tag-list)))
      (append allow-list tag-allow-list))))

(defun intentional--org-clock-fetch-data ()
  "Return all data associated with current clocked task needed for processing.

The purpose of this function is to do everything in one go vs
constantly going in and out of the file.  Returns list as
follows (ALLOW-LIST TAG-LIST)."
  (when org-clock-current-task
    (save-window-excursion
      (org-clock-goto)
      (save-excursion
        (save-restriction
          (org-narrow-to-subtree)
          (let* ((allows (org-entry-get (point) "INTENTIONAL_ALLOW"))
                 (items (and allows (split-string allows " ")))
                 (tags (org-get-tags)))
            (when intentional-extract-clock-body-urls
              (while (< (point) (point-max))
                (when (looking-at "http")
                  (let ((url (thing-at-point 'url)))
                    (when url
                      (setq items (cons url items)))))
                (forward-char 1)))
            (list items tags)))))))

(defun intentional--add-org-clock-site (site)
  "Add SITE to intentional allow property."
  (unless org-clock-current-task
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


(defun intentional--write-to-journal (intention-str site)
  "Write INTENTION-STR and SITE to today's journal entry."
  (save-window-excursion
    (org-journal-new-entry nil)
    (insert intention-str)
    (insert "\n")
    (insert site)
    (org-indent-line)
    (save-buffer)))

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
  (intentional--display)
  (intentional--write-intentions-to-file))

(defun intentional-refresh-buffer ()
  "Re-draw the intentional buffer."
  (interactive)
  (when (get-buffer intentional--buffer-name) ;; only refresh if buffer exists
    (with-current-buffer intentional--buffer-name
      (let ((inhibit-read-only t))
        (intentional--display))))
  (intentional--write-intentions-to-file))

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
