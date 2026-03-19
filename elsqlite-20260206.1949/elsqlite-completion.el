;;; elsqlite-completion.el --- Advanced SQL completion for ELSQLite -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Dusan Popovic

;; Author: Dusan Popovic <dpx@binaryapparatus.com>
;; License: GPL-3.0-or-later

;; This program is free software: you can redistribute it and/or modify
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

;; Advanced SQL completion engine for ELSQLite.
;;
;; Features:
;; - Context-aware completion (knows WHERE vs SELECT vs FROM)
;; - Table and column name completion from database schema
;; - Alias support (FROM users u → u.column)
;; - SQL keyword and function completion
;; - Rich annotations (column types, primary key markers)
;; - Schema caching with automatic invalidation

;;; Code:

(require 'elsqlite-db)

;; Forward declarations
(defvar elsqlite--db)

;;; Schema Cache

(defvar-local elsqlite-completion--schema-cache nil
  "Cached schema: alist of (table . ((column . info-plist) ...).
Cache is invalidated automatically after modification queries
\(INSERT/UPDATE/DELETE/CREATE/ALTER/DROP).  For external schema changes,
use \\[elsqlite-completion-invalidate-cache] to manually refresh.")

(defun elsqlite-completion--get-schema ()
  "Get schema, using cache if available."
  (unless elsqlite-completion--schema-cache
    (elsqlite-completion--refresh-schema))
  elsqlite-completion--schema-cache)

(defun elsqlite-completion--refresh-schema ()
  "Refresh schema cache from database."
  (when elsqlite--db
    (let ((tables (elsqlite-db-get-tables elsqlite--db))
          (schema '()))
      (dolist (table tables)
        (let ((columns (elsqlite-completion--get-table-columns table)))
          (push (cons table columns) schema)))
      (setq elsqlite-completion--schema-cache (nreverse schema)))))

(defun elsqlite-completion--get-table-columns (table)
  "Get alist of (column-name . info-plist) for TABLE."
  (let ((info (sqlite-select elsqlite--db
                             (format "PRAGMA table_info(%s)" table))))
    (mapcar (lambda (row)
              (let ((name (nth 1 row))
                    (type (nth 2 row))
                    (notnull (nth 3 row))
                    (pk (nth 5 row)))
                (cons name (list :type type
                                 :notnull (= notnull 1)
                                 :pk (> pk 0)))))
            info)))

(defun elsqlite-completion-invalidate-cache ()
  "Invalidate schema cache.
Call this after modification queries (CREATE, ALTER, DROP, etc.)."
  (setq elsqlite-completion--schema-cache nil))

;;; SQL Parsing

(defun elsqlite-completion--extract-tables (sql)
  "Extract table names referenced in SQL.
Returns list of table names from FROM, JOIN, UPDATE, INTO clauses."
  (let ((tables '())
        (case-fold-search t))
    (with-temp-buffer
      (insert sql)
      (goto-char (point-min))
      ;; Match: FROM table, JOIN table, UPDATE table, INTO table
      (while (re-search-forward
              "\\b\\(?:FROM\\|JOIN\\|UPDATE\\|INTO\\)\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\b"
              nil t)
        (let ((table (match-string 1)))
          (unless (member (upcase table) '("SELECT" "WHERE" "SET" "VALUES"))
            (push table tables)))))
    (delete-dups (nreverse tables))))

(defun elsqlite-completion--extract-aliases (sql)
  "Extract table aliases from SQL.
Returns alist of (alias . table-name)."
  (let ((aliases '())
        (case-fold-search t))
    (with-temp-buffer
      (insert sql)
      (goto-char (point-min))
      ;; Pattern: FROM/JOIN table [AS] alias
      (while (re-search-forward
              "\\b\\(?:FROM\\|JOIN\\)\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\(?:\\s-+AS\\)?\\s-+\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\b"
              nil t)
        (let ((table (match-string 1))
              (alias (match-string 2)))
          ;; Don't treat SQL keywords as aliases
          (unless (member (upcase alias)
                         '("WHERE" "ON" "AND" "OR" "LEFT" "RIGHT" "INNER"
                           "OUTER" "JOIN" "GROUP" "ORDER" "LIMIT" "SET"
                           "HAVING" "UNION" "EXCEPT" "INTERSECT"))
            (push (cons alias table) aliases)))))
    (nreverse aliases)))

(defun elsqlite-completion--current-clause (text-before)
  "Determine which SQL clause point is in based on TEXT-BEFORE cursor.
Returns symbol indicating clause type."
  (let ((text-upper (upcase text-before)))
    (cond
     ;; Check from most specific/recent backward to general
     ((string-match-p "\\bLIMIT\\s-+[^;]*\\'" text-upper) 'limit)
     ((string-match-p "\\bORDER\\s-+BY\\b[^;]*\\'" text-upper) 'order-by)
     ((string-match-p "\\bGROUP\\s-+BY\\b[^;]*\\'" text-upper) 'group-by)
     ((string-match-p "\\bHAVING\\b[^;]*\\'" text-upper) 'having)
     ((string-match-p "\\bWHERE\\b[^;]*\\'" text-upper) 'where)
     ((string-match-p "\\bON\\b[^;]*\\'" text-upper) 'on)
     ;; JOIN followed by whitespace and optional partial table name - offer tables
     ((string-match-p "\\bJOIN\\s-+\\'" text-upper) 'join-table)
     ((string-match-p "\\b\\(?:LEFT\\|RIGHT\\|INNER\\|OUTER\\)\\s-+JOIN\\s-+\\'" text-upper) 'join-table)
     ;; Check if we have complete FROM clause (table name present) - offer next keywords
     ((string-match-p "\\bFROM\\s-+[a-zA-Z_][a-zA-Z0-9_]*\\(?:\\s-+[a-zA-Z_][a-zA-Z0-9_]*\\)?\\s-*\\'" text-upper) 'after-from)
     ((string-match-p "\\bFROM\\s-+[^;]*\\'" text-upper) 'from)
     ((string-match-p "\\bSELECT\\b[^;]*\\'" text-upper) 'select)
     ;; INSERT INTO with optional partial table name - offer tables (check before plain INSERT)
     ((string-match-p "\\bINSERT\\s-+INTO\\s-*\\'" text-upper) 'into-table)
     ((string-match-p "\\bINSERT\\s-+INTO\\s-+[a-zA-Z_][a-zA-Z0-9_]*\\s*\\'" text-upper) 'into-table)
     ;; INSERT followed by whitespace - offer INTO
     ((string-match-p "\\bINSERT\\s-*\\'" text-upper) 'after-insert)
     ;; Check if we have complete UPDATE clause (table name present) - offer SET
     ((string-match-p "\\bUPDATE\\s-+[a-zA-Z_][a-zA-Z0-9_]*\\s-*\\'" text-upper) 'after-update)
     ((string-match-p "\\bUPDATE\\s-+[a-zA-Z_]*\\s*\\'" text-upper) 'update-table)
     ;; Check if we have complete SET clause (no trailing comma means done with SET)
     ;; Pattern: SET ... with assignments but no trailing comma
     ((and (string-match-p "\\bSET\\b[^;]*\\'" text-upper)
           (string-match-p "=\\s-*[^,=;]+\\s-*\\'" text-upper)
           (not (string-match-p ",\\s-*\\'" text-upper))) 'after-set)
     ((string-match-p "\\bSET\\b[^;]*\\'" text-upper) 'set)
     ((string-match-p "\\bVALUES\\b[^;]*\\'" text-upper) 'values)
     ;; INTO with optional partial table name (for other contexts) - offer tables
     ((string-match-p "\\bINTO\\s-+[a-zA-Z_]*\\s*\\'" text-upper) 'into-table)
     ;; Just "UPDATE " with no table yet - offer tables
     ((string-match-p "\\bUPDATE\\s-*\\'" text-upper) 'update-table)
     (t 'statement))))

;;; Context Detection and Completion

(defun elsqlite-completion--get-context ()
  "Get completion context at point.
Returns plist with :clause, :tables, :aliases, :table-dot."
  (let* ((sql (buffer-substring-no-properties (point-min) (point-max)))
         (text-before (buffer-substring-no-properties (point-min) (point)))
         (tables (elsqlite-completion--extract-tables sql))
         (aliases (elsqlite-completion--extract-aliases sql))
         (clause (elsqlite-completion--current-clause text-before))
         (table-dot nil))

    ;; Check if we're after "table." pattern
    (when (eq (char-before) ?.)
      (save-excursion
        (backward-char)
        (when (looking-back "\\b\\([a-zA-Z_][a-zA-Z0-9_]*\\)" (line-beginning-position))
          (let* ((prefix (match-string 1))
                 ;; Resolve alias to actual table name
                 (table (or (cdr (assoc prefix aliases)) prefix)))
            (setq table-dot table)
            (setq clause (cons 'table-dot table))))))

    (list :clause clause
          :tables tables
          :aliases aliases
          :table-dot table-dot)))

;;; Candidate Generation

(defun elsqlite-completion--keyword-candidates (keywords)
  "Create completion candidates for KEYWORDS."
  (mapcar (lambda (kw)
            (propertize kw 'elsqlite-type 'keyword))
          keywords))

(defun elsqlite-completion--table-candidates (schema)
  "Create completion candidates for tables in SCHEMA."
  (mapcar (lambda (entry)
            (propertize (car entry) 'elsqlite-type 'table))
          schema))

(defun elsqlite-completion--column-candidates-for-table (table schema)
  "Create column completion candidates for TABLE using SCHEMA."
  (let* ((table-schema (cdr (assoc-string table schema t)))
         (candidates '()))
    (dolist (col-entry table-schema)
      (let* ((col-name (car col-entry))
             (col-info (cdr col-entry))
             (type (plist-get col-info :type))
             (pk (plist-get col-info :pk)))
        (push (propertize col-name
                          'elsqlite-type 'column
                          'elsqlite-column-type type
                          'elsqlite-pk pk
                          'elsqlite-table table)
              candidates)))
    (nreverse candidates)))

(defun elsqlite-completion--column-candidates-for-tables (tables schema)
  "Create column completion candidates for all TABLES using SCHEMA."
  (let ((candidates '()))
    (dolist (table tables)
      (dolist (cand (elsqlite-completion--column-candidates-for-table table schema))
        (push cand candidates)))
    (delete-dups (nreverse candidates))))

(defun elsqlite-completion--function-candidates ()
  "Create completion candidates for SQL functions."
  (mapcar (lambda (fn)
            (propertize fn 'elsqlite-type 'function))
          '("COUNT" "SUM" "AVG" "MIN" "MAX"
            "COALESCE" "NULLIF" "IFNULL" "IIF"
            "LENGTH" "SUBSTR" "UPPER" "LOWER" "TRIM" "REPLACE" "LTRIM" "RTRIM"
            "ABS" "ROUND" "CEIL" "FLOOR" "RANDOM"
            "DATE" "TIME" "DATETIME" "JULIANDAY" "STRFTIME"
            "TYPEOF" "CAST" "CASE" "GROUP_CONCAT")))

(defun elsqlite-completion--candidates ()
  "Generate completion candidates for current context."
  (let* ((ctx (elsqlite-completion--get-context))
         (clause (plist-get ctx :clause))
         (tables (plist-get ctx :tables))
         (schema (elsqlite-completion--get-schema)))

    (pcase clause
      ;; Statement start - offer main SQL commands
      ('statement
       (elsqlite-completion--keyword-candidates
        '("SELECT" "INSERT" "UPDATE" "DELETE" "CREATE" "ALTER" "DROP"
          "WITH" "EXPLAIN" "PRAGMA")))

      ;; Table name contexts
      ((or 'from 'join-table 'into-table 'update-table)
       (elsqlite-completion--table-candidates schema))

      ;; After FROM table - offer next clause keywords
      ('after-from
       (elsqlite-completion--keyword-candidates
        '("WHERE" "JOIN" "LEFT JOIN" "INNER JOIN" "RIGHT JOIN" "OUTER JOIN"
          "GROUP BY" "ORDER BY" "LIMIT")))

      ;; After UPDATE table - offer SET
      ('after-update
       (elsqlite-completion--keyword-candidates '("SET")))

      ;; After INSERT - offer INTO
      ('after-insert
       (elsqlite-completion--keyword-candidates '("INTO")))

      ;; After SET column = value (no trailing comma) - offer WHERE only
      ('after-set
       (elsqlite-completion--keyword-candidates '("WHERE")))

      ;; SELECT clause - columns, functions, keywords
      ('select
       (append
        (elsqlite-completion--column-candidates-for-tables tables schema)
        (elsqlite-completion--function-candidates)
        (elsqlite-completion--keyword-candidates '("DISTINCT" "FROM" "AS" "*"))))

      ;; WHERE, HAVING, ON - columns and logical operators
      ((or 'where 'having 'on)
       (append
        (elsqlite-completion--column-candidates-for-tables tables schema)
        (elsqlite-completion--keyword-candidates
         '("AND" "OR" "NOT" "IN" "LIKE" "BETWEEN" "IS" "NULL" "EXISTS"
           "=" "<>" "!=" "<" ">" "<=" ">="))))

      ;; ORDER BY, GROUP BY - columns and sort keywords
      ((or 'order-by 'group-by)
       (append
        (elsqlite-completion--column-candidates-for-tables tables schema)
        (when (eq clause 'order-by)
          (elsqlite-completion--keyword-candidates '("ASC" "DESC")))))

      ;; SET clause (UPDATE) - columns from updated table
      ('set
       (when tables
         (elsqlite-completion--column-candidates-for-table (car tables) schema)))

      ;; After table.dot - show columns for specific table
      (`(table-dot . ,table)
       (elsqlite-completion--column-candidates-for-table table schema))

      ;; Default - common keywords
      (_
       (elsqlite-completion--keyword-candidates
        '("SELECT" "FROM" "WHERE" "AND" "OR" "ORDER BY" "GROUP BY"
          "JOIN" "LEFT JOIN" "INNER JOIN" "ON" "AS" "LIMIT" "OFFSET"))))))

;;; Annotation

(defun elsqlite-completion--annotate (candidate)
  "Annotate CANDIDATE with type information."
  (let ((type (get-text-property 0 'elsqlite-type candidate)))
    (pcase type
      ('keyword " <keyword>")
      ('table " <table>")
      ('column
       (let ((col-type (get-text-property 0 'elsqlite-column-type candidate))
             (pk (get-text-property 0 'elsqlite-pk candidate)))
         (format " %s%s"
                 (or col-type "")
                 (if pk " PK" ""))))
      ('function " <func>")
      (_ ""))))

;;; CAPF Entry Point

(defun elsqlite-completion-at-point ()
  "Completion-at-point function for ELSQLite SQL buffers.
Provides context-aware SQL completion including tables, columns,
keywords, and functions."
  (when elsqlite--db
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (or (car bounds) (point)))
           (end (or (cdr bounds) (point)))
           (candidates (elsqlite-completion--candidates)))
      (when candidates
        (list start end candidates
              :annotation-function #'elsqlite-completion--annotate
              :company-kind (lambda (cand)
                              (pcase (get-text-property 0 'elsqlite-type cand)
                                ('keyword 'keyword)
                                ('table 'class)
                                ('column 'field)
                                ('function 'function)
                                (_ 'text)))
              :exclusive 'no)))))

(provide 'elsqlite-completion)
;;; elsqlite-completion.el ends here
