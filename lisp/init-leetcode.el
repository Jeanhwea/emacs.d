(defconst lc/query-url "https://leetcode-cn.com/graphql")

(defconst lc/query-question
  (concat
    "{\"query\": \"query problemsetQuestionList($categorySlug: String, "
    "$limit: Int, $skip: Int, $filters: QuestionListFilterInput) {"
    "problemsetQuestionList(categorySlug: $categorySlug limit: $limit"
    " skip: $skip filters: $filters) {questions {title titleCn titleSlug}}}\","
    "\"variables\": { \"limit\": 10,\"filters\": { \"searchKeywords\": "
    "\"%s\" } },\"operationName\": \"problemsetQuestionList\" }"))

(defun lc/request-url (body)
  "Send GraphQL to LeetCode."
  (let
    ((response-string nil)
      (url-request-method "POST")
      (url-request-extra-headers '(("Content-Type" . "application/json")))
      (url-request-data body))
    (switch-to-buffer (url-retrieve-synchronously lc/query-url))
    (goto-char (point-min))
    (re-search-forward "\n\n")
    (setq response-string
      (buffer-substring-no-properties (point) (point-max)))
    (kill-buffer (current-buffer))
    response-string))

(defun lc/query-question (search)
  "Search LeetCode Questions."
  (let* ((resp (lc/request-url (format lc/query-question search))))
    (aref (gethash "questions"
            (gethash "problemsetQuestionList"
              (gethash "data" (json-parse-string resp)))) 0)))

(defun lc/make-url-string (qid slug name)
  "Make url link string."
  (let ((base "https://leetcode-cn.com/problems"))
    (format "[[%s/%s/][LC%03d]] %s" base slug (string-to-number qid) name)))

(defun leetcode-trans ()
  "Trans to leetcode url"
  (interactive)
  (let*
    ((line (jh/current-line))
      (search (jh/re-replace ".*] *" "" line))
      (question (lc/query-question search))
      (qid (gethash "frontendQuestionId" question))
      (name (gethash "titleCn" question))
      (slug (gethash "titleSlug" question)))
    (insert (lc/make-url-string qid slug name))))

(when (require 'leetcode)
  (setq leetcode-prefer-language "java")
  (setq leetcode-prefer-sql "mysql"))

(provide 'init-leetcode)
