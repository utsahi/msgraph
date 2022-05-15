;;; msgraph-email.el

;; Copyright (C) 2022

;; Author: Kishor Datar (kishordatar at gmail)

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'msgraph)

(defcustom
  msgraph-email-home
  '(:type well-known-folder-search :folder "scheduled" :searchtext "isread:false") 
  "Defines the default page displayed. Examples -

'(:type well-known-folder :folder \"inbox\")
'(:type well-known-folder-search :folder \"scheduled\" :searchtext \"isread:false\")
'(:type folder :display-name \"inbox\")
'(:type folder-search :display-name \"inbox\" :searchtext \"isread:false\")

"
  :group 'msgraph-email)

(defcustom
  msgraph-email-page-size
  25
  "Defines the default folder displayed."
  :type '(integer)
  :group 'msgraph-email)

(defcustom
  msgraph-email-max-folder-page-retrievals
  10
  "Defines the maximum number of folder pages.")

(defconst
  msgraph-email-column-parsing-error-indicator
  "<error>")
(defconst msgraph-email-selection-marker "*")

(defconst msgraph-email-known-folder-names
  '("inbox" "scheduled" "sentitems" "deleteditems" "junkemail"))

(defvar msgraph-email-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-n") 'msgraph-email-next-page)
    (define-key map (kbd "&") 'msgraph-email-open-web-link)
    (define-key map (kbd "M-p") 'msgraph-email-previous-page)
    (define-key map "&" 'msgraph-email-open-web-link)
    (define-key map "r" 'msgraph-email-render-body-html)
    (define-key map "s" 'msgraph-email-search)
    (define-key map "^" 'msgraph-email-navigate)
    (define-key map "m" (lambda () (interactive) (msgraph-email-mark-and-move '(next-line) msgraph-email-selection-marker)))
    (define-key map "u" (lambda () (interactive) (msgraph-email-unmark-and-move '(next-line) msgraph-email-selection-marker)))
    (define-key map "G" 'msgraph-email-refetch-current-page)
    (define-key map "F" 'msgraph-email-navigate-to-folder)
    (define-key map "C" 'msgraph-email-open-conversation)
    (define-key map "U" 'msgraph-email-remove-all-marks)
    (define-key map (kbd "M-r") 'msgraph-email-mark-as-read)
    (define-key map (kbd "M-d") 'msgraph-email-delete)
    map))

(defvar
  msgraph-email-mode-line-format
  '(:eval
    (let* ((spec (plist-get msgraph-email-current-page :navigation-status)))
      (cond ((eq 'well-known-folder-search (plist-get spec :type))
	     (format "[(%s) : (%s)]"
		     (propertize (plist-get spec :folder) 'face 'msgraph-email-mode-line-folder)
		     (propertize (plist-get spec :searchtext) 'face 'msgraph-email-mode-line-query)))
	    ((eq 'folder-search (plist-get spec :type))
	     (format "[(%s) : (%s)]"
		     (propertize (plist-get spec :display-name) 'face 'msgraph-email-mode-line-folder)
		     (propertize (plist-get spec :searchtext) 'face 'msgraph-email-mode-line-query)))
	    ((eq 'well-known-folder (plist-get spec :type))
	     (format "[(%s)]" (propertize (plist-get spec :folder) 'face 'msgraph-email-mode-line-folder)))
	    ((eq 'folder (plist-get spec :type))
	     (format "[(%s)]" (propertize (plist-get spec :display-name) 'face 'msgraph-email-mode-line-folder)))
	    ((eq 'conversation (plist-get spec :type))
	     " (Conversation) ")
	    ((eq 'url (plist-get spec :type))
	     "[(Custom URL)]")))))

(defface msgraph-email-read-face '((t)) "Read email face.")
(defface msgraph-email-unread-face '((t :weight bold)) "Unread email face.")
(defface msgraph-email-deleted-face '((t :strike-through t)) "Deleted email face.")
(defface msgraph-email-important-message-face '((t :inherit error)) "Important email face.")
(defface msgraph-email-mode-line-folder '((t :inherit font-lock-type-face)) "Mode line folder name face.")
(defface msgraph-email-mode-line-query '((t :inherit font-lock-comment-face)) "Mode line query string face.")

(define-derived-mode msgraph-email-mode tabulated-list-mode "msgraph-email-mode"
  "Major mode for email access using MSGraph API."
  (setq tabulated-list-format [("M" 3 t)
			       ("Time"  20 t)
                               ("From" 30 nil)
			       ("Subject" 0 t)
                               ])
  (setq tabulated-list-padding 4)
  (setq tabulated-list-sort-key (cons "Time" t))
  (tabulated-list-init-header)
  (add-to-list
   'mode-line-format
   msgraph-auth-status-mode-line-format
   t)
  (add-to-list
   'mode-line-format
   msgraph-email-mode-line-format
   t))

(defun msgraph-email ()
  (interactive)

  (let* ((buffer-name (read-string "Buffer name: " "*msgraph-email*")))
    (if (get-buffer buffer-name)
	(pop-to-buffer-same-window (get-buffer buffer-name))
      (pop-to-buffer-same-window
       (get-buffer-create buffer-name))
      (msgraph-email-mode)
      (hl-line-mode)
      (setq-local msgraph-email-current-page nil)
      (setq-local msgraph-email-navigation-history nil)
      (setq-local msgraph-email-cache-folders-by-display-name nil)
      (setq-local msgraph-email-cache-populated nil)
      (if msgraph-auth-info (msgraph-email-navigate)))))

(defun msgraph-email-construct-url (navigation-spec)
  (let* ((messages-url-fragment
	  "/messages?$select=webLink,receivedDateTime,conversationId,importance,isRead,sender,subject,importance&$top="))
    (cond
     ((eq 'url (plist-get navigation-spec :type))
      (plist-get navigation-spec :url))
     ((eq 'well-known-folder (plist-get navigation-spec :type))
      (concat (msgraph-root)
	      "me/"
	      "mailFolders/" (plist-get navigation-spec :folder)
	      messages-url-fragment
	      (format "%d" msgraph-email-page-size)))
     ((eq 'folder (plist-get navigation-spec :type))
      (concat (msgraph-root)
	      "me/"
	      "mailFolders/"	      
	      (plist-get navigation-spec :folder-id)
	      messages-url-fragment
	      (format "%d" msgraph-email-page-size)))
     ((eq 'folder-search (plist-get navigation-spec :type))
      (concat (msgraph-root)
	      "me/"
	      "mailFolders/"
	      (plist-get navigation-spec :folder-id)
	      messages-url-fragment
	      (format "%d" msgraph-email-page-size)
	      "&$search="
	      (url-hexify-string
	       (concat "\"" (string-replace "\"" "\\\"" (plist-get navigation-spec :searchtext)) "\""))))
     ((eq 'well-known-folder-search (plist-get navigation-spec :type))
      (concat (msgraph-root)
	      "me/"
	      "mailFolders/"
	      (plist-get navigation-spec :folder)
	      messages-url-fragment
	      (format "%d" msgraph-email-page-size)
	      "&$search="
	      (url-hexify-string
	       (concat "\"" (string-replace "\"" "\\\"" (plist-get navigation-spec :searchtext)) "\""))))
     ((eq 'conversation (plist-get navigation-spec :type))
      (concat (msgraph-root)
	      "me/"
	      messages-url-fragment
	      (format "%d" msgraph-email-page-size)
	      "&$filter="
	      "conversationId eq '"
	      (url-hexify-string
	       (plist-get navigation-spec :conversation-id))
	      "'"))
     (t (error "Unrecognized navigation spec")))))

(defun msgraph-email-propertize-row (message row face)
  (let* ((common-faces))
    (when (string-equal (cdr (assoc 'importance message)) "high")
      (setq common-faces (cons 'msgraph-email-important-message-face common-faces)))
    (dotimes (i (length row))
      (let* ((column-with-properties (aref row i))
	     (column (substring-no-properties column-with-properties)))
	(setq column (propertize column 'face face))
	(mapc
	 (lambda (item)
	   (add-face-text-property 0 (length column) item t column))
	 common-faces)
	(aset row i column)))))

(defun msgraph-email-open-web-link ()
  (interactive)
  
  (if (not (tabulated-list-get-id))
      (error "no item selected.")
    (browse-url (cdr (assoc 'webLink (tabulated-list-get-id))))))

(defun msgraph-email-render-body-html ()
  (interactive)

  (let* ((response  
	  (msgraph-get (concat (msgraph-root) "me/messages/" (cdr (assoc 'id (tabulated-list-get-id))))))
	 (body (cdr (assoc 'content (cdr (assoc 'body response))))))

    (pop-to-buffer-same-window
       (get-buffer-create "*msgraph-html-viewer*"))
      (delete-region (point-min) (point-max))
      (insert body)
      (shr-render-region (point-min) (point-max))
      (beginning-of-buffer)
      (view-mode)))

(defun msgraph-email-mark-as-read ()
  (interactive)
  (dolist (row (msgraph-email-get-marked-or-current))
    (let* ((entry-id (plist-get row :id))
	   (entry (plist-get row :entry)))
      (msgraph-read-json-response
       "PATCH"
       (concat (msgraph-root) "me/messages/" (cdr (assoc 'id entry-id)))
       "{ isRead: true }")
      (msgraph-email-propertize-row entry-id entry 'msgraph-email-read-face)))    
  (tabulated-list-print t))

(defun msgraph-email-refetch-current-page ()
  (interactive)
  (msgraph-email-navigate
   (when (plist-get msgraph-email-current-page :url)
       (list
	:type 'url
	:url (plist-get msgraph-email-current-page :url)))))

(defun msgraph-email-get-marked-or-current ()
  (let* ((result))
    (save-excursion
      (beginning-of-buffer)
      (while (tabulated-list-get-entry)
	(when (string-match-p msgraph-email-selection-marker (aref (tabulated-list-get-entry) 0))
	  (push (list :id (tabulated-list-get-id) :entry (tabulated-list-get-entry)) result))
	(next-line))
      )
    (or result
	(list (list :id (tabulated-list-get-id) :entry (tabulated-list-get-entry))))))

(defun msgraph-email-delete ()
  (interactive)
  (when (y-or-n-p "Are you sure you want to delete the selected messages? ")
    (dolist (row (msgraph-email-get-marked-or-current))
      (let* ((entry-id (plist-get row :id))
	     (entry (plist-get row :entry)))
	(msgraph-read-json-response
	   "DELETE"
	   (concat (msgraph-root) "me/messages/" (cdr (assoc 'id entry-id)))
	   nil)
	(msgraph-email-propertize-row entry-id entry 'msgraph-email-deleted-face)))
      (tabulated-list-print t)))

(defun msgraph-email-next-page ()
  (interactive)
  (if (not msgraph-email-current-page)
      (error "no current page")
    (msgraph-email-navigate
     (list
      :type 'url
      :url (or (cdr (assoc '@odata\.nextLink (plist-get msgraph-email-current-page :body)))
	       (error "no next page"))))))

(defun msgraph-email-previous-page ()
  (interactive)
  (if (not msgraph-email-navigation-history)
      (error "no previous page")
    (msgraph-email-navigate
     (list :type 'url
	   :url (plist-get (car msgraph-email-navigation-history) :url)
	   :source 'previouspagecommand))))

(defun msgraph-email-cache-get-folders-by-display-name ()
  (msgraph-email-ensure-cache)
  msgraph-email-cache-folders-by-display-name)

(defun msgraph-email-open-conversation (conversation-id)
  (interactive "i")
  (let* ((conversation-id (or conversation-id
			      (cdr (assoc 'conversationId (tabulated-list-get-id))))))
    (msgraph-email-navigate
     (list
      :type 'conversation
      :conversation-id conversation-id))))

(defun msgraph-email-navigate-to-folder (folder)
  (interactive "i")
  
  (msgraph-email-ensure-cache)
  (let* ((selected-folder (or folder
			      (completing-read
			       "Enter the folder name: "
			       (msgraph-email-cache-get-folders-by-display-name)))))
    (msgraph-email-navigate
     (list
      :type 'folder
      :display-name selected-folder
      :folder-id (plist-get
		  (gethash selected-folder (msgraph-email-cache-get-folders-by-display-name))
		  :id)))))

(defun msgraph-email-navigate (&optional navigation-spec)
  (interactive)
  (let* ((selected-navigation-spec (or navigation-spec msgraph-email-home))
	 (navigation-url (msgraph-email-construct-url selected-navigation-spec))
	 (response (msgraph-get navigation-url))
	 (items (cdr (assoc 'value response))))
    (setq tabulated-list-entries
	  (mapcar
	   (lambda (item)
	     (let* ((row
		     (vector
		      ""
		      (or (cdr (assoc 'receivedDateTime item))
			  msgraph-email-column-parsing-error-indicator)
		      (or (cdr (assoc 'name (cdr (assoc 'emailAddress (cdr (assoc 'sender item))))))
			  msgraph-email-column-parsing-error-indicator)
		      (or (cdr (assoc 'subject item))
			  msgraph-email-column-parsing-error-indicator))))
	       (msgraph-email-propertize-row
		item
		row
		(if (eq (cdr (assoc 'isRead item)) t) 'msgraph-email-read-face 'msgraph-email-unread-face))
	       (list item row)))
	   items))
    (tabulated-list-print t)

    (let* ((navigation-status ; used to display the navigation state
			      ; in the mode line. If we are navigating
			      ; forward by chasing the odata next
			      ; link, we reuse the navigation spec
			      ; from the current page as is, e.g. when
			      ; we are on the second page of the
			      ; 'scheduled' folder, the mode line
			      ; still shows 'scheduled'.
	    (cond ((memq (plist-get selected-navigation-spec :type)
			 '(folder
			   well-known-folder
			   well-known-folder-search
			   folder-search conversation))
		   selected-navigation-spec)
		  (t
		   (plist-get msgraph-email-current-page :navigation-status)))))
      (if (eq 'previouspagecommand (plist-get selected-navigation-spec :source))
	  (setq ; if navigating back, update to the navigation state
		; remembered in history, and do not push the backward
		; navigation on the history stack. E.g., going back
		; from the Inbox to the scheduled folder should change
		; the navigation state to scheduled.
	   navigation-status (plist-get (car msgraph-email-navigation-history) :navigation-status)
	   msgraph-email-navigation-history (cdr msgraph-email-navigation-history))
	(when (and
	       (not (string-equal navigation-url (plist-get msgraph-email-current-page :url)))
	       (plist-get msgraph-email-current-page :url))
	  (setq msgraph-email-navigation-history
		(push
		 (list :url (plist-get msgraph-email-current-page :url)
		       :navigation-status (plist-get msgraph-email-current-page :navigation-status))
		 msgraph-email-navigation-history))))    
      (setq msgraph-email-current-page
	    (list :body response
		  :url navigation-url
		  :navigation-status navigation-status))))
  nil)

(defun msgraph-email-mark-and-move (navigation marker)
  (interactive "i")
  (when (tabulated-list-get-entry)
    (unless (string-match-p marker (aref (tabulated-list-get-entry) 0))
      (aset (tabulated-list-get-entry) 0 (concat marker (aref (tabulated-list-get-entry) 0)))
      (tabulated-list-print t))
    (eval navigation)))

(defun msgraph-email-unmark-and-move (navigation marker)
  (interactive "i")
  (when (tabulated-list-get-entry)
    (when (string-match-p marker (aref (tabulated-list-get-entry) 0))
      (aset (tabulated-list-get-entry) 0 (string-replace marker "" (aref (tabulated-list-get-entry) 0)))
      (tabulated-list-print t))
    (eval navigation)))

(defun msgraph-email-remove-all-marks ()
  (interactive)
  (save-excursion
      (beginning-of-buffer)
      (while (tabulated-list-get-entry)
	(aset (tabulated-list-get-entry) 0 "")
	(next-line)))
      (tabulated-list-print t))

(defun msgraph-email-search (&optional filter)
  (msgraph-email-ensure-cache)
  
  (interactive "MEnter the search string : ")
  (let* ((folder (completing-read
		  "Enter the folder name: "
		  (msgraph-email-cache-get-folders-by-display-name)))
	 (navigation-spec
	  (list :type 'folder-search
		:display-name folder
		:folder-id (plist-get
			    (gethash folder (msgraph-email-cache-get-folders-by-display-name))
			    :id)
		:searchtext filter)))
    (msgraph-email-navigate navigation-spec)))

(defun msgraph-email-open-web-link ()
  (interactive)
  (when (tabulated-list-get-entry)
    (browse-url (cdr (assoc 'webLink (tabulated-list-get-id))))))

(defun msgraph-email-ensure-cache ()
  (unless msgraph-email-cache-populated
    (setq msgraph-email-cache-folders-by-display-name (make-hash-table :test 'equal))
    (mapc (lambda (c)
	    (puthash (plist-get c :display-name)
		     c
		     msgraph-email-cache-folders-by-display-name))
	  (msgraph-email-fetch-folders)))
  (setq msgraph-email-cache-populated t))

(defun msgraph-email-fetch-folders ()
  (let* ((folders)
	 (response)
	 (done)
	 (retrievals 0)
	 (cache)
	 (next-url (concat (msgraph-root) "me/mailfolders/")))
    (while (and (not done) next-url)
      (message "Fetching folder list ...")
      (when (> retrievals msgraph-email-max-folder-page-retrievals)
	(error "Too many folders."))
      (setq response (msgraph-get next-url))
      (push response folders)
      (setq next-url (cdr (assoc '@odata\.nextLink response)))
      (setq retrievals (1+ retrievals)))

    (mapc
     (lambda (f)
       (let* ((fl (cdr (assoc 'value f))))
	 (mapc
	  (lambda (i) (push
		       (list :display-name (cdr (assoc 'displayName i))
			     :id (cdr (assoc 'id i)))
		       cache))
	  fl)))
     folders)
    cache))

(provide 'msgraph-email)
