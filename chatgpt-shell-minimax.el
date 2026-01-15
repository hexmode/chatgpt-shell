;;; chatgpt-shell-minimax.el --- MiniMax support for `chatgpt-shell' -*- lexical-binding: t; -*-

;;; Commentary:

;; Adds MiniMax specifics for `chatgpt-shell'.
;; Uses MiniMax's Anthropic-compatible /v1/messages endpoint.

;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'shell-maker)
(require 'map)

(defvar chatgpt-shell-proxy)
(declare-function chatgpt-shell--unsorted-collection "chatgpt-shell")

(defcustom chatgpt-shell-minimax-key nil
  "MiniMax API key as a string or a function that loads and returns it."
  :type '(choice (function :tag "Function")
                 (string :tag "String"))
  :group 'chatgpt-shell)

(defcustom chatgpt-shell-minimax-api-url-base "https://api.minimax.io/anthropic/v1"
  "MiniMax API's base URL.

API url = base + path.

If you use MiniMax through a proxy service, change the URL base."
  :type 'string
  :safe #'stringp
  :group 'chatgpt-shell)

(cl-defun chatgpt-shell-minimax--make-model (&key version
                                                 short-version
                                                 token-width
                                                 max-tokens
                                                 context-window)
  "Create a MiniMax model.

Set VERSION, SHORT-VERSION, TOKEN-WIDTH, MAX-TOKENS,
and CONTEXT-WINDOW."
  (unless version
    (error "Missing mandatory :version param"))
  (unless token-width
    (error "Missing mandatory :token-width param"))
  (unless max-tokens
    (error "Missing mandatory :max-tokens param"))
  (unless context-window
    (error "Missing mandatory :context-window param"))
  `((:provider . "MiniMax")
    (:label . "MiniMax")
    (:path . "/messages")
    (:version . ,version)
    (:max-tokens . ,max-tokens)
    (:short-version . ,short-version)
    (:token-width . ,token-width)
    (:context-window . ,context-window)
    (:handler . chatgpt-shell-minimax--handle-command)
    (:filter . chatgpt-shell-minimax--extract-response)
    (:payload . chatgpt-shell-minimax--make-payload)
    (:url . chatgpt-shell-minimax--make-url)
    (:headers . chatgpt-shell-minimax--make-headers)
    (:url-base . chatgpt-shell-minimax-api-url-base)
    (:key . chatgpt-shell-minimax-key)
    (:validate-command . chatgpt-shell-minimax--validate-command)
    (:icon . "minimax.png")))

(defun chatgpt-shell-minimax-models ()
  "Build a list of MiniMax LLM models available (early 2026)."
  (list
   (chatgpt-shell-minimax--make-model :version "minimax-m2.1"
                                      :short-version "M2.1"
                                      :token-width 4
                                      :max-tokens 8192
                                      :context-window 200000)
   (chatgpt-shell-minimax--make-model :version "minimax-text-01"
                                      :short-version "Text-01"
                                      :token-width 4
                                      :max-tokens 8192
                                      :context-window 245760)
   (chatgpt-shell-minimax--make-model :version "abab7.5s-chat"
                                      :short-version "abab7.5s"
                                      :token-width 4
                                      :max-tokens 4096
                                      :context-window 65536)))

(cl-defun chatgpt-shell-minimax--make-url (&key _command model _settings)
  "Create the API URL using MODEL and SETTINGS."
  (concat (symbol-value (or (map-elt model :url-base)
                            (error "Model :url-base not found")))
          (or (map-elt model :path)
              (error "Model :path not found"))))

(defun chatgpt-shell-minimax--validate-command (_command _model _settings)
  "Return error string if command/setup isn't valid."
  (unless chatgpt-shell-minimax-key
    "Variable `chatgpt-shell-minimax-key' needs to be set to your key.

Try M-x set-variable chatgpt-shell-minimax-key

or

(setq chatgpt-shell-minimax-key \"mm_sk-...\")"))

(defun chatgpt-shell-minimax-key ()
  "Get the MiniMax API key."
  (cond ((stringp chatgpt-shell-minimax-key)
         chatgpt-shell-minimax-key)
        ((functionp chatgpt-shell-minimax-key)
         (condition-case _err
             (funcall chatgpt-shell-minimax-key)
           (error "KEY-NOT-FOUND")))
        (t
         nil)))

(cl-defun chatgpt-shell-minimax--make-headers (&key _model _settings)
  "Create the API headers for MiniMax Anthropic-compatible endpoint."
  (unless (chatgpt-shell-minimax-key)
    (error "Your chatgpt-shell-minimax-key is missing"))
  (list "Content-Type: application/json; charset=utf-8"
        (concat "Authorization: Bearer " (chatgpt-shell-minimax-key))))

(cl-defun chatgpt-shell-minimax--make-payload (&key model context settings)
  "Create the API payload using MODEL CONTEXT and SETTINGS."
  (let ((context (mapcan (lambda (l)
                           (when (cdr l)
                             `(((role . "user")
                                (content . ,(car l)))
                               ((role . "assistant")
                                (content . ,(cdr l))))))
                         context))
        (command `(((role . "user")
                    (content . ,(caar (last context)))))))
    (append
     (when (map-elt settings :system-prompt)
       `((system . ,(map-elt settings :system-prompt))))
     `((max_tokens . ,(or (map-elt model :max-tokens)
                          (error "Missing %s :max-tokens" (map-elt model :version))))
       (model . ,(map-elt model :version))
       (stream . ,(if (map-elt settings :streaming) 't :false))
       (messages . ,(vconcat
                     (append
                      context
                      command)))))))

(cl-defun chatgpt-shell-minimax--handle-command (&key model command context shell settings)
  "Handle MiniMax COMMAND (prompt) using MODEL, CONTEXT, SHELL, and SETTINGS."
  (message "MiniMax URL: %s" (chatgpt-shell-minimax--make-url :model model :settings settings))
  (shell-maker-make-http-request
   :async t
   :url (chatgpt-shell-minimax--make-url :model model
                                         :settings settings)
   :proxy chatgpt-shell-proxy
   :data (chatgpt-shell-minimax--make-payload :model model
                                              :context
                                              (append
                                               context
                                               (list (cons command nil)))
                                              :settings settings)
   :headers (chatgpt-shell-minimax--make-headers)
   :filter #'chatgpt-shell-minimax--extract-response
   :shell shell))

(defmacro json-get (json &rest keys)
  "Navigate JSON structure with KEYS."
  (let ((result json))
    (dolist (key keys)
      (setq result
            (cond
             ((numberp key)
              `(nth ,key ,result))
             ((and (listp key)
                   (eq (car key) 'quote)
                   (consp (cadr key)))
              (let ((k (cadr key)))
                `(seq-find (lambda (item)
                             (equal (gethash ,(symbol-name (car k)) item) ,(cdr k)))
                           ,result)))
             (t
              `(gethash ,key ,result)))))
    result))

(defun chatgpt-shell-minimax--extract-non-streaming (pending-str)
  "Extract visible text from non-streaming MiniMax PENDING-STR."
  (let ((json (json-parse-string pending-str
                                 :object-type 'hash-table
                                 :array-type  'list)))

    (when (hash-table-p json)
      (when-let* ((text (json-get json "content" '(type . "text") "text")))
        (cond
         ((stringp text)
          text)
         ((listp text)  ; array-type 'list
          (let ((txt ""))
            (dotimes (i (length text))
              (let ((block (aref text i)))
                (when (and (hash-table-p block)
                           (string= (gethash "type" block) "text"))
                  (setq txt (concat txt (or (gethash "text" block) ""))))))
            txt))
         (t "[emacs minimax client doesn't know how to handle this]"))))))

(defun chatgpt-shell-minimax--extract-streaming (pending-str)
  "Extract a new visible text delta from PENDING-STR."
  (let ((new-text "")
        (lines (split-string pending-str "\n" t)))
    (dolist (line lines)
      (when (string-prefix-p "data: " line)
        (let ((data-str (substring line 6)))
          ;; ;; Only attempt parse if it looks like complete JSON
          ;; (when (and (> (length data-str) 1)
          ;;            (string-match-p "\\`{.*}\\z" data-str))  ; starts { ends }
          (let ((json (ignore-errors (json-parse-string data-str
                                                        :object-type 'hash-table
                                                        :array-type  'vector))))
            (when (hash-table-p json)
              (let ((event-type (gethash "type" json)))
                (message "Processing event: %s" (or event-type "nil"))
                (pcase event-type
                  ("content_block_delta"
                   (when-let ((delta (gethash "delta" json)))
                     (let ((delta-type (gethash "type" delta)))
                       (pcase delta-type
                         ("text_delta"
                          (setq new-text (concat new-text (or (gethash "text" delta) ""))))
                         ("thinking_delta" nil)
                         ("signature_delta" nil)))))
                  (_ nil))))))))
    new-text))


(cl-defun chatgpt-shell-minimax--extract-response (output)
  "Extract new visible text delta from MiniMax OUTPUT."
  (let ((pending-str (cond
                      ((and (consp output) (consp (car output)) (eq (caar output) :pending))
                       (cdar output))
                      ((stringp output) output)
                      (t "")))
        (result))

    (when (or (null pending-str) (string-empty-p pending-str))
      (cl-return-from chatgpt-shell-minimax--extract-response ""))

    (setq result
          (if (string-match-p "event:" pending-str)
              (chatgpt-shell-minimax--extract-streaming pending-str)

            ;; Non-streaming: full JSON
            (chatgpt-shell-minimax--extract-non-streaming pending-str)))

    (when (not (string-empty-p result))
      (message "Returning new text: %S" result))
    result))

(provide 'chatgpt-shell-minimax)
;;; chatgpt-shell-minimax.el ends here
