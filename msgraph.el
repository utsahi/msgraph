;;; msgraph.el --- Library to interact with msgraph

;; Copyright (C) 2020

;; Author: Kishor Datar (kishordatar at gmail)

;; Homepage: http://localhost
;; Keywords: Custom

;; Package-Version: 1.0.0
;; Package-Requires: ((emacs "27.1"))

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

(require 'json)
(require 'url-parse)

(defvar
  msgraph-host
  "graph.microsoft.com")

(defvar
  msgraph-version
  "beta")

(defvar
  msgraph-auth-info
  nil)

(defvar msgraph-batch-size 20)

(defface msgraph-auth-error-face '((t :inherit error)) "MSGraph authentication error face.")

(defvar
  msgraph-auth-status-mode-line-format
  '(:eval
     (let* ((auth-info (assoc-string msgraph-host msgraph-auth-info))
	    (exp (plist-get auth-info :exp))
	    (upn (plist-get auth-info :upn)))
       (format "(%s)"
	       (if upn
		   (format
		    "%s:%s"
		    upn
		    (if (time-less-p
			 (seconds-to-time exp)
			 (current-time))
			(propertize "EXPIRED" 'face 'msgraph-auth-error-face)
		      (format "Expires %s"
			      (format-time-string "%c" (seconds-to-time exp)))))
		 (propertize "No token" 'face 'msgraph-auth-error-face))))))

(defun msgraph-url-base64-decode (u64)
  (let* ((b64-no-padding
	  (replace-regexp-in-string "_" "/" (replace-regexp-in-string
					     "-" "+" u64)))
	 (lenMod4 (% (length b64-no-padding) 4))
	 (padding '["" nil "==" "="])
	 (b64 (concat b64-no-padding (aref padding lenMod4))))
    (if (= 1 lenMod4)
	(error "invalid u64."))
    (base64-decode-string b64)))

(defun msgraph-set-auth-info (host bearer-token)
  (let* ((token-payload
	  (json-read-from-string 
	   (msgraph-url-base64-decode
	    (nth 1 (split-string bearer-token "\\.")))))
	 (exp (cdr (assoc 'exp token-payload)))
	 (upn (cdr (assoc 'upn token-payload))))

      (setq msgraph-auth-info (delq (assoc-string host msgraph-auth-info) msgraph-auth-info))
      (setq msgraph-auth-info
	    (cons
	     (list host nil
		   :raw-token bearer-token
		   :token-payload token-payload
		   :exp exp
		   :upn upn)
	     msgraph-auth-info)))
  nil)

(defun msgraph-ensure-parameters ()
  (if (not msgraph-host)
      (error "`msgraph-host not set"))
  (if (not msgraph-version)
      (error "'msgraph-version not set"))
  (if (not msgraph-auth-info)
      (error "'msgraph-auth-info not set"))
  (if (not (and
	    (plist-get (assoc-string msgraph-host msgraph-auth-info) :exp)
	    (not (time-less-p
		  (seconds-to-time (plist-get (assoc-string msgraph-host msgraph-auth-info) :exp))
		  (current-time)))))
      (error "invalid or expired token")))

(defun msgraph-read-response (method url body &optional headers)
  (msgraph-ensure-parameters)

  (let* ((response-code)
	 (response-body)
	 (url-request-method method)
	 (url-request-data body)
         (url-request-extra-headers
	  (msgraph-append-authorization-header
	   url
	   (or headers
	       (list
		(cons "Accept" "application/json")
		(cons "Content-type" "application/json")))))
	 (response (url-retrieve-synchronously url nil nil 20)))
    (when (not response) (error "URL retrieval failed. Possibly timed out"))
    (with-current-buffer response
      (beginning-of-buffer)
      (search-forward-regexp "\\([0-9][0-9][0-9]\\)" (max (point-max) 20))
      (setq response-code (match-string 1))
      (search-forward-regexp "^$")
      (setq response-body
	    (buffer-substring-no-properties (point) (point-max)))
      (beginning-of-buffer)
      (list :result-code response-code :body response-body :buffer (current-buffer)))))

(defun msgraph-read-response-with-retry (method
					 url
					 body
					 &optional
					 headers
					 bail-on-error
					 max-retries
					 retry-backoff-sec)
  (let* ((retries 0)
	 (done)
	 (retry-backoff-sec (or retry-backoff-sec 30))
	 (max-retries (or max-retries 0))
	 (last-response)
	 (last-response-code))
    (while (not done)
      (setq last-response (msgraph-read-response method url body headers))
      (setq last-response-code (plist-get last-response :result-code))      
      (if (and (> retries max-retries)
	       (or (string-equal "429" last-response-code)
		   (string-equal "504" last-response-code)))
	  (progn
	    (setq retries (1+ retries))
	    (message
	     "Received HTTP code %s. will retry after %d seconds ... "
	     last-response-code
	     retry-backoff-sec)
	    (sleep-for retry-backoff-sec)
	    (setq retry-backoff-sec (* 2 retry-backoff-sec)))
	(if (and bail-on-error
		 (string-match-p "^\\(4\\|5\\)" last-response-code))
	    (error last-response-code))
	(setq done t)))
    last-response))

(defun msgraph-read-json-response (method url body &optional bail-on-error headers)
  (let* ((raw-response
	  (msgraph-read-response-with-retry method url body headers bail-on-error)))
    (if (string-equal "204" (plist-get raw-response :result-code))
	nil
      (json-read-from-string (plist-get raw-response :body)))))

(defun msgraph-get (url)
  (msgraph-read-json-response "GET" url nil t))

(defun msgraph-post (url body)
  (msgraph-read-json-response "POST" url body t))

(defun msgraph-delete (url)
  (msgraph-read-json-response "DELETE" url nil t))

(defun msgraph-root ()
  (concat "https://" msgraph-host "/" msgraph-version "/"))

(defun msgraph-append-authorization-header (url headers)
  (let* ((parsed-url (url-generic-parse-url url))
	 (host (url-host parsed-url))
	 (auth-info (assoc-string host msgraph-auth-info))
	 (msgraph-request-auth-info auth-info)
	 (result
	  (append (if (and (not (assoc-string
				 "Authorization"
				 headers)) 
			   auth-info)
		      (list (cons
			     "Authorization"
			     (concat
			      "Bearer "
			      (plist-get auth-info :raw-token)))
			    ;; (cons
			    ;;  "User-Agent"
			    ;; "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/125.0.0.0 Safari/537.36 Edg/125.0.0.0")
			    )
		    nil)
		  headers)))
    result))

(defun msgraph-delete-batched (urls)
  "Produced ordered output for each of the urls."
  (let* ((idx)
	 (partitions (seq-partition urls msgraph-batch-size))
	 (result))
    (setq idx 0)
    (dolist (partition partitions)
      (let* ((requests))
	(dolist (url partition)
	  (setq requests
		(cons
		 (list (cons 'id (number-to-string idx))
		       (cons 'method "DELETE")
		       (cons 'url url))
		 requests))
	  (setq idx (1+ idx)))

	(setq result
	      (cons
	       (cdr
		(assoc 'responses
		       (msgraph-read-json-response
			"POST"
			(concat (msgraph-root) "$batch")
			(json-encode (list (cons 'requests requests)))
			t)))
	       result))))

    (let* ((responses))
      (seq-doseq (batches result)
	(seq-doseq (e batches)
	  (setq responses (cons e responses))))
      (seq-sort
       (lambda (e1 e2)
	 (< (string-to-number (cdr (assoc 'id e1)))
	    (string-to-number (cdr (assoc 'id e2)))))
       responses))))

(defun msgraph-send-requests-batched (requests)
  "The request IDs are assumed to be ordered unique numbers."
  (let* ((partitions (seq-partition requests msgraph-batch-size))
	 (responses))
    (dolist (requests partitions)
      (let* ((current-batch-responses
	      (cdr
	       (assoc 'responses
		      (msgraph-read-json-response
		       "POST"
		       (concat (msgraph-root) "$batch")
		       (json-encode (list (cons 'requests requests)))
		       t)))))
	(push current-batch-responses responses)))
    (seq-sort
       (lambda (e1 e2)
	 (< (string-to-number (cdr (assoc 'id e1)))
	    (string-to-number (cdr (assoc 'id e2)))))
       (apply 'seq-concatenate (cons 'list responses)))))

(provide 'msgraph)
;;; msgraph.el ends here
