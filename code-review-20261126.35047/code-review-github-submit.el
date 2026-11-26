;;; code-review-github-submit.el --- GitHub submission helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Error handling and reply submission for the GitHub backend.

;;; Code:

(require 'ghub)
(require 'deferred)
(require 'code-review-auth)
(require 'code-review-interfaces)
(require 'code-review-utils)
(require 'a)
(require 'dash)
(require 'subr-x)

(defvar code-review-github-host)
(defvar code-review-github-diffheader)
(defun code-review-github--evict-failed-token (m)
  "Evict the cached token that the failed request in M (a ghub errback list) used.
Clear the whole cache when no request or token can be found."
  (let* ((req (seq-find #'ghub--req-p m))
         (auth (and req (map-elt (ghub--req-headers req) "Authorization")))
         (token (and auth (replace-regexp-in-string "\\`\\(?:token\\|Bearer\\) " "" auth))))
    (if token
        (code-review-auth-evict-token token)
      (code-review-auth-clear-cache))))

(defvar code-review-github-error-functions nil
  "Abnormal hook run with the ghub errorback argument list on a failed request.
Non-interactive callers (`code-review-api-send') use it to capture the
failure of the request they are waiting on.")

(defun code-review-github-errback (&rest m)
  "Error callback, displays the error message M."
  (code-review-utils--log "code-review-github-errback" (prin1-to-string m))
  (with-demoted-errors "code-review-github-error-functions: %S"
    (run-hook-with-args 'code-review-github-error-functions m))
  (if-let* ((first (car m))
            ((listp first))
            ((eq (car-safe first) 'errors))
            (graphql-errors (cdr first))
            (msgs (mapcar (lambda (e) (or (a-get e 'message) (prin1-to-string e)))
                          graphql-errors)))
      (message "GraphQL errors: %s" (string-join msgs "; "))
    (let-alist m
      (let* ((status (-second-item .error)))
        (cond
         ((= status 422)
          (let ((errors (string-join
                         (a-get (-third-item .error) 'errors)
                         " AND "))
                (msg (string-trim (or (a-get (-third-item .error) 'message) ""))))
            (message "Errors: %S" (if (string-empty-p errors)
                                      msg
                                    (string-join (list msg errors) ". ")))))
         ((= status 404)
          (message "Provided URL Not Found"))
         ((= status 401)
          (code-review-github--evict-failed-token m)
          (message "Bad credentials. Documentation to how to setup credentials
https://github.com/wandersoncferreira/code-review#configuration"))
         (t
          (message "Unknown error talking to Github: %s" m)))))))

(defclass code-review-submit-github-replies ()
  ((pr      :initform nil)
   (replies :initform nil
            :type (satisfies
                   (lambda (it)
                     (-all-p #'code-review-submit-reply-p it))))
   (node-ids :initform nil
             :documentation "Set by `code-review-send-replies' before its callback runs: GraphQL node ids of the created replies, parallel to the ids it passes (nil for a failed reply).")))

(cl-defmethod code-review-send-replies ((replies code-review-submit-github-replies) callback)
  "Submit replies to review comments inline given REPLIES and a CALLBACK fn.
CALLBACK runs once every reply request has finished (answered or failed),
not when the requests are merely dispatched.  It is called with one
argument, the list of created comment ids (nil for a reply that failed);
callers that do not care can accept `&rest _'.  Before CALLBACK runs the
`node-ids' slot of REPLIES holds the GraphQL node ids, in the same order."
  (let ((pr (oref replies pr))
        (nodes nil))
    (deferred:$
      (deferred:parallel
        (-map
         (lambda (reply)
           (lambda ()
             (let ((done (deferred:new)))
               (condition-case err
                   (ghub-post (format "/repos/%s/%s/pulls/%s/comments/%s/replies"
                                      (oref pr owner)
                                      (oref pr repo)
                                      (oref pr number)
                                      (oref reply reply-to-id))
                              nil
                              :payload (a-alist 'body (oref reply body))
                              :headers code-review-github-diffheader
                              :auth (code-review-auth-resolve 'github code-review-github-host pr)
                              :username (code-review-auth-username pr)
                              :host code-review-github-host
                              :callback (lambda (&optional value &rest _)
                                          (push (cons reply (ignore-errors (a-get value 'node_id)))
                                                nodes)
                                          (deferred:callback
                                            done
                                            (or (ignore-errors (a-get value 'id)) t)))
                              :errorback (lambda (&rest m)
                                           (unwind-protect
                                               (condition-case err
                                                   (apply #'code-review-github-errback m)
                                                 (error (message "code-review reply errback: %S" err)))
                                             (deferred:callback done nil))))
                 (error
                  (condition-case hook-err
                      (run-hook-with-args 'code-review-github-error-functions
                                          (list (list 'error 'local (error-message-string err))))
                    (error (message "code-review reply error hook: %S" hook-err)))
                  (deferred:callback done nil)))
               done)))
         (oref replies replies)))

      (deferred:nextc it
        (lambda (results)
          (oset replies node-ids
                (-map (lambda (reply)
                        (alist-get reply nodes nil nil #'eq))
                      (oref replies replies)))
          (funcall callback (-map (lambda (r) (and (integerp r) r)) results))))

      (deferred:error it
        (lambda (err)
          (message "Got an error from the Github Reply API %S!" err))))))

(provide 'code-review-github-submit)
;;; code-review-github-submit.el ends here
