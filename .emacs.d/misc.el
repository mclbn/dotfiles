;;; misc.el --- Miscellaneous reusable utilities -*- lexical-binding: t; -*-

;;; Commentary:
;; Small, self-contained utilities reusable across projects.
;;
;; `mnemonic-keys-assign' / `mnemonic-keys-assign-by': assign short,
;; mnemonic, prefix-free single-key (and, on overflow, two-key) shortcuts
;; to a list of items — intended for transient menus and similar pickers.
;;
;; Assignment is a pure function of its inputs (stable across redisplays)
;; and proceeds in staged, global passes:
;;
;;   Tier 1  each leaf claims the lowercase of its first a-z letter.
;;   Tier 2  each still-unplaced leaf claims the first letter of a later
;;           word/segment (split on [-_/ ] and CamelCase humps).
;;   Tier 3  remaining leaves draw from a home-row-ordered pool.
;;   Overflow two-char keys, uppercase pool char x lowercase pool char.
;;
;; Invariants: single keys are lowercase, two-char keys are uppercase-first,
;; so no key is a prefix of another.  RESERVED chars are blocked in every
;; tier; note a reserved uppercase char (e.g. ?C) does NOT block its
;; lowercase twin (?c) — they are distinct keys.
;;
;; The library takes ready-made display strings ("leaves"); extracting a
;; leaf from a path/filename is the caller's responsibility.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup mnemonic-keys nil
  "Mnemonic prefix-free key assignment for pickers."
  :group 'convenience)

(defcustom mnemonic-keys-home-row-order "asdfghjklqwertyuiopzxcvbnm"
  "Fallback key pool, most-preferred char first.
Used by tier 3 and, for both characters, by the two-char overflow.
Default favours the QWERTY home row; override for other layouts."
  :type 'string :group 'mnemonic-keys)

(defun mnemonic-keys--pool ()
  "Return `mnemonic-keys-home-row-order' as a list of characters."
  (append mnemonic-keys-home-row-order nil))

(defun mnemonic-keys--first-letter (s)
  "Return the first a-z/A-Z letter of string S, or nil."
  (seq-find (lambda (c) (or (<= ?a c ?z) (<= ?A c ?Z))) (append s nil)))

(defun mnemonic-keys--words (leaf)
  "Split LEAF into words on [-_/ ] separators and CamelCase humps."
  (let ((case-fold-search nil))
    (split-string
     (replace-regexp-in-string "\\([a-z0-9]\\)\\([A-Z]\\)" "\\1 \\2" leaf)
     "[-_/ ]+" t)))

(defun mnemonic-keys-assign (leaves &optional reserved)
  "Assign a prefix-free key to each string in LEAVES.
Return a list of key strings positionally aligned to LEAVES.
RESERVED is a list of characters to keep unused.  See the Commentary
for the assignment algorithm and its invariants."
  (let* ((n (length leaves))
         (result (make-vector n nil))
         (used (make-hash-table :test 'equal)))
    (cl-flet* ((freep (c) (and (not (gethash (char-to-string c) used))
                               (not (memq c reserved))))
               (take (i c) (puthash (char-to-string c) t used)
                      (aset result i (char-to-string c))))
      ;; Tier 1: first letter of the leaf.
      (dotimes (i n)
        (let ((c (mnemonic-keys--first-letter (nth i leaves))))
          (when (and c (freep (downcase c))) (take i (downcase c)))))
      ;; Tier 2: first letter of each subsequent word/segment.
      (dotimes (i n)
        (unless (aref result i)
          (let* ((words (mnemonic-keys--words (nth i leaves)))
                 (cands (delq nil (mapcar #'mnemonic-keys--first-letter
                                          (cdr words))))
                 (c (seq-find (lambda (ch) (freep (downcase ch))) cands)))
            (when c (take i (downcase c))))))
      ;; Tier 3: home-row-ordered pool.
      (dotimes (i n)
        (unless (aref result i)
          (let ((c (seq-find #'freep (mnemonic-keys--pool))))
            (when c (take i c)))))
      ;; Overflow: two-char keys, uppercase pool char x lowercase pool char.
      (dotimes (i n)
        (unless (aref result i)
          (catch 'done
            (dolist (f (mnemonic-keys--pool))
              (unless (memq (upcase f) reserved)
                (dolist (s (mnemonic-keys--pool))
                  (let ((k (concat (char-to-string (upcase f))
                                   (char-to-string s))))
                    (unless (gethash k used)
                      (puthash k t used)
                      (aset result i k)
                      (throw 'done nil)))))))))
      (append result nil))))

(defun mnemonic-keys-assign-by (keyfn items &optional reserved)
  "Assign keys to ITEMS, deriving each leaf via KEYFN.
KEYFN is called on each item and must return its display string (leaf).
Return an alist of (ITEM . KEY), order preserved.  RESERVED as in
`mnemonic-keys-assign'."
  (cl-mapcar #'cons items
             (mnemonic-keys-assign (mapcar keyfn items) reserved)))

(provide 'misc)
;;; misc.el ends here
