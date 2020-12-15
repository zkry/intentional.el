;;; intentional-install.el --- Installation utilities  -*- lexical-binding: t -*-

;;; Commentary:

;; This file contains installation helpers to assist the user in installation.

;;; Code:

(require 'f)

(defvar intentional-python-code
  "#!/usr/bin/python3 -u

import json
import sys
import struct
import time

# Encode a message for transmission, given its content.
def encode_message():
    with open('%s', 'r') as file:
        data = file.read()
        #encoded_content = json.dumps(message_content).encode('utf-8')
        encoded_content = data.encode('utf-8')
        encoded_length = struct.pack('=I', len(encoded_content))
        #  use struct.pack('10s', bytes), to pack a string of the length of 10 characters
        return {'length': encoded_length, 'content': struct.pack(str(len(encoded_content))+'s',encoded_content)}

def send_message(encoded_message):
    sys.stdout.buffer.write(encoded_message['length'])
    sys.stdout.buffer.write(encoded_message['content'])
    sys.stdout.buffer.flush()

while True:
    send_message(encode_message())
    time.sleep(1)
")

(defvar intentional-firefox-nmh-manifest
  "{
    \"name\": \"intentional\",
    \"description\": \"Simple application to fetch data from Browse by Intention config\",
    \"path\": \"%s\",
    \"type\": \"stdio\",
    \"allowed_extensions\": [ \"intentional@example.org\" ]
}")

(defvar intentional-chrome-nmh-manifest
  "{
    \"name\": \"com.zkry.intentional\",
    \"description\": \"My Application\",
    \"path\": \"%s\",
    \"type\": \"stdio\",
    \"allowed_origins\": [
        \"chrome-extension://%s/\"
    ]
}")

(defun intentional-install-firefox ()
  "Start the assister for installing intentional on Firefox."
  (interactive)
  (when (not (y-or-n-p (format "Is '%s' the directory to save file?" intentional-output-file)))
    (error "%s" "set the variable `intentional-output-file' to where you want to save file."))
  (let* ((bi-file (expand-file-name intentional-output-file))

         (py-file-dir (read-directory-name "Directory to save intentional.py "))
         (py-file (concat py-file-dir "intentional.py"))
         (py-file-contents (format intentional-python-code bi-file)))
    (f-write-text py-file-contents 'utf-8 py-file)
    (message "Python file written to %s" py-file)
    (let* ((msg-host-file-dir (read-directory-name "Directory to write Firefox native messaging host manifest: "
                                                   "~/Library/Application Support/Mozilla/NativeMessagingHosts/"))
           (msg-host-file (concat msg-host-file-dir "intentional.json"))
           (msg-host-file-contents (format intentional-firefox-nmh-manifest py-file)))
      (f-write-text msg-host-file-contents 'utf-8 msg-host-file)
      (message "Manifest installed to %s. You can now install the Firefox extension."
               msg-host-file))))


(defun intentional-install-chrome ()
  "Start the assister for installing intentional on Firefox."
  (interactive)
  (when (not (y-or-n-p (format "Is '%s' the directory to save file?" intentional-output-file)))
    (error "Set the variable `intentional-output-file' to where you want to save file"))
  (let ((bi-file (expand-file-name intentional-output-file))
        (chrome-extension-id (read-string "Enter Chrome extension ID (you must install extension to see this): ")))
    (unless (string-match "[a-z]\\{32\\}" chrome-extension-id)
      (error "Invalid chrome ID.  Should be a string of 32 lower-case letters"))
    (let* ((py-file-dir (read-directory-name "Directory to save intentional.py "))
           (py-file (concat py-file-dir "intentional.py"))
           (py-file-contents (format intentional-python-code bi-file)))
      (f-write-text py-file-contents 'utf-8 py-file)
      (message "Python file written to %s" py-file)
      (let* ((msg-host-file-dir (read-directory-name "Directory to write Chrome native messaging host manifest: "
                                                     "~/Library/Application Support/Google/Chrome/NativeMessagingHosts"))
             (msg-host-file (concat msg-host-file-dir "intentional.json"))
             (msg-host-file-contents (format intentional-chrome-nmh-manifest py-file chrome-extension-id)))
        (f-write-text msg-host-file-contents 'utf-8 msg-host-file)
        (message "Installation complete")))))


(provide 'intentional-install)

;;; intentional-install.el ends here
