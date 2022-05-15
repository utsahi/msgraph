;;; msgraph.el --- Library to interact with msgraph

;; Copyright (C) 2020

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
		(cons "Content-type" "application/json"))))))
    (with-current-buffer
        (url-retrieve-synchronously url nil nil 20)
      (beginning-of-buffer)
      (search-forward-regexp "\\([0-9][0-9][0-9]\\)" (max (point-max) 20))
      (setq response-code (match-string 1))
      (search-forward-regexp "^$")
      (setq response-body
	    (buffer-substring-no-properties (point) (point-max)))
      (beginning-of-buffer)
      (list :result-code response-code :body response-body :buffer (current-buffer)))))

(defun msgraph-read-response-with-retry (method url body &optional headers max-retries retry-backoff-sec)
  (let* ((retries 0)
	 (done)
	 (retry-backoff-sec (or retry-backoff-sec 30))
	 (max-retries (or max-retries 0))
	 (last-response))
    (while (not done)
      (setq last-response (msgraph-read-response method url body headers))
      (if (and (> retries max-retries)
	       (or (string-equal "429" (plist-get last-response :result-code))
		   (string-equal "504" (plist-get last-response :result-code))))
	  (progn
	    (setq retries (1+ retries))
	    (message
	     "Received HTTP code %s. will retry after %d seconds ... "
	     (plist-get last-response :result-code)
	     retry-backoff-sec)
	    (sleep-for retry-backoff-sec)
	    (setq retry-backoff-sec (* 2 retry-backoff-sec)))
	(setq done t)))
    last-response))

(defun msgraph-read-json-response (method url body &optional headers)
  (let* ((raw-response
	  (msgraph-read-response-with-retry method url body headers)))
    (if (string-equal "204" (plist-get raw-response :result-code))
	nil
      (json-read-from-string (plist-get raw-response :body)))))

(defun msgraph-get (url)
  (msgraph-read-json-response "GET" url nil))

(defun msgraph-post (url body)
  (msgraph-read-json-response "POST" url body))

(defun msgraph-delete (url)
  (msgraph-read-json-response "DELETE" url nil))

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
			      (plist-get auth-info :raw-token))))
		    nil)
		  headers)))
    result))

(provide 'msgraph)
