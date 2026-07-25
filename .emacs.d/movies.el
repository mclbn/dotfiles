;;; movies.el --- gptel tools for the local movies.org lists  -*- lexical-binding: t; -*-

;; Part of the film-recommendation gptel setup (see film-reco-setup.org).
;;
;; These are NATIVE gptel tools (not MCP servers), on purpose: they act on a file
;; in the local Emacs org folder, so they must run in the Emacs process that owns
;; that file. The external-data servers (Jellyfin / TMDB / OMDb) stay as MCP; this
;; is the client-local-file exception to the MCP-only tooling model.
;;
;; They manage three lists inside one file (movies.org):
;;   * To explore   - a flat checklist of topics to look into  (name + one-line note)
;;   * To download  - a flat checklist of specific films to acquire  (name only)
;;   * To GIF       - a TODO sub-heading per film, each with a checklist of scenes
;;
;; The film WATCHLIST is NOT here: it is the Jellyfin collection "Marc", written via
;; the existing `jellyfin_collection_add' MCP tool (see the project prompt fragment).
;;
;; Load (after gptel, because tools are registered at load time):
;;   (with-eval-after-load 'gptel
;;     (require 'movies (locate-user-emacs-file "movies")))
;;
;; Configure the file path in your private perso.el, e.g.:
;;   (setq perso/movies-org-file (expand-file-name "movies.org" org-directory))

;;; Code:

(require 'org)
(require 'json)
(require 'seq)
(require 'subr-x)

(defgroup perso/movies nil
  "gptel tools for the local movies.org lists."
  :group 'gptel)

(defcustom perso/movies-org-file
  (expand-file-name "movies.org"
                    (if (boundp 'org-directory) org-directory "~/org"))
  "Path to the org file holding the To explore / To download / To GIF lists."
  :type 'file
  :group 'perso/movies)

;; ------------------------------------------------------------------------
;; Internal helpers
;; ------------------------------------------------------------------------

(defconst perso/movies--sections '("To explore" "To download" "To GIF")
  "The three canonical level-1 headings.")

(defun perso/movies--file ()
  "Absolute path of the movies file."
  (expand-file-name perso/movies-org-file))

(defun perso/movies--get-buffer (&optional create)
  "Return the buffer visiting the movies file.
If the file is missing and CREATE is non-nil, scaffold it (title + the three
empty sections) first.  If missing and CREATE is nil, return nil."
  (let ((file (perso/movies--file)))
    (cond
     ((file-exists-p file) (find-file-noselect file))
     (create
      (make-directory (file-name-directory file) t)
      (with-temp-file file
        (insert "#+TITLE: Movies\n#+TODO: TODO | DONE\n\n"
                "* To explore\n\n* To download\n\n* To GIF\n"))
      (find-file-noselect file))
     (t nil))))

(defun perso/movies--ci= (a b)
  "Case-insensitive string equality of A and B."
  (eq t (compare-strings (or a "") nil nil (or b "") nil nil t)))

(defun perso/movies--clean (s)
  "Trim S and strip any leading org list bullet, checkbox, or TODO/DONE keyword.
Defensive: lets the tools tolerate a model that pastes '- [ ] Foo' or 'TODO Foo'."
  (let ((x (string-trim (or s ""))))
    (setq x (replace-regexp-in-string
             "\\`[-*+]+[ \t]*\\(\\[[ Xx-]\\][ \t]*\\)?" "" x))
    (setq x (replace-regexp-in-string "\\`\\(TODO\\|DONE\\)[ \t]+" "" x))
    (string-trim x)))

(defun perso/movies--normalize (s)
  "Fold S for fuzzy comparison: downcase, drop (…) and […] qualifiers, keep alnum."
  (let ((x (downcase (or s ""))))
    (setq x (replace-regexp-in-string "([^)]*)" " " x))
    (setq x (replace-regexp-in-string "\\[[^]]*\\]" " " x))
    (setq x (replace-regexp-in-string "[^[:alnum:]]+" " " x))
    (string-trim x)))

(defun perso/movies--tokens (s)
  "Token list of S after normalization."
  (split-string (perso/movies--normalize s) " " t))

(defun perso/movies--jaccard (a b)
  "Jaccard overlap of token lists A and B."
  (let* ((sa (delete-dups (copy-sequence a)))
         (sb (delete-dups (copy-sequence b)))
         (inter (seq-intersection sa sb #'string=))
         (uni (delete-dups (append sa sb))))
    (if (null uni) 0.0 (/ (float (length inter)) (length uni)))))

(defun perso/movies--imdb-id (s)
  "Return the IMDb id (ttNNNNNNN) found in S, downcased, or nil.
The id is a globally unique film key, so it is the strongest identity signal we
have — much more reliable than the title, which may carry hand-written comments."
  (when (and s (string-match "\\(tt[0-9]\\{6,\\}\\)" (downcase s)))
    (match-string 1 (downcase s))))

(defun perso/movies--match (query name)
  "Classify how QUERY matches NAME.
Return nil, or a cons (KIND . REASON) where KIND is \"exact\" or \"fuzzy\".

IMDb id is authoritative: if BOTH strings carry one, equality of ids => the same
film (\"exact\"/same-imdb-id) regardless of any trailing hand-written comment, and
inequality => different films (no match), short-circuiting the title heuristics.
Only when at least one side lacks an id do we fall back to title comparison."
  (let ((qid (perso/movies--imdb-id query))
        (nid (perso/movies--imdb-id name)))
    (cond
     ((and qid nid)
      (if (string= qid nid) (cons "exact" "same-imdb-id") nil))
     ((perso/movies--ci= (string-trim query) (string-trim name))
      (cons "exact" "identical"))
     (t
      (let ((nq (perso/movies--normalize query))
            (nn (perso/movies--normalize name)))
        (cond
         ((or (string-empty-p nq) (string-empty-p nn)) nil)
         ((string= nq nn) (cons "fuzzy" "same-normalized"))
         ((string-match-p (regexp-quote nq) nn) (cons "fuzzy" "query-within-item"))
         ((string-match-p (regexp-quote nn) nq) (cons "fuzzy" "item-within-query"))
         ((>= (perso/movies--jaccard (perso/movies--tokens query)
                                     (perso/movies--tokens name))
              0.5)
          (cons "fuzzy" "token-overlap"))
         (t nil)))))))

(defun perso/movies--goto-section (title)
  "Move point to the level-1 heading whose text is TITLE.  Return t, else nil."
  (goto-char (point-min))
  (let (found)
    (while (and (not found) (re-search-forward "^\\* " nil t))
      (when (string-equal (org-get-heading t t t t) title)
        (beginning-of-line)
        (setq found t)))
    found))

(defun perso/movies--goto-child (title end)
  "From a section heading, find a level-2 child heading whose text is TITLE,
searching forward up to END.  Move point to it (bol) and return t, else nil."
  (let (found)
    (while (and (not found) (re-search-forward "^\\*\\* " end t))
      (when (perso/movies--ci= (org-get-heading t t t t) title)
        (beginning-of-line)
        (setq found t)))
    found))

(defun perso/movies--ensure-section (title)
  "Ensure a level-1 heading TITLE exists; create it at end of buffer if not.
Leave point on the heading."
  (unless (perso/movies--goto-section title)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (insert "* " title "\n")
    (perso/movies--goto-section title)))

(defun perso/movies--subtree-end ()
  "End position of the subtree whose heading is at point."
  (save-excursion (org-end-of-subtree t t) (point)))

(defun perso/movies--append-in-subtree (line)
  "Point is on a heading.  Append LINE as a new line at the end of that subtree's
body (after the last content line, before the next heading)."
  (let ((end (perso/movies--subtree-end)))
    (goto-char end)
    (skip-chars-backward " \t\n")
    (end-of-line)
    (insert "\n" line)))

(defun perso/movies--checkbox-items ()
  "From point (on a heading), collect the text of each checkbox line in the
subtree — i.e. the part after `- [ ] '.  Point is left unmoved by the caller's
`save-excursion' expectation; this function moves point."
  (let ((end (perso/movies--subtree-end))
        items)
    (forward-line 1)
    (while (< (point) end)
      (when (looking-at "^[ \t]*[-+*] \\[[ Xx-]\\] +\\(.*\\)$")
        (push (string-trim (match-string-no-properties 1)) items))
      (forward-line 1))
    (nreverse items)))

(defun perso/movies--checkbox-items-full ()
  "Like `perso/movies--checkbox-items' but keep the done state.
From point (on a heading), return a list of alists
  ((\"text\" . TEXT) (\"done\" . t|:json-false))
for each checkbox line in the subtree.  Moves point."
  (let ((end (perso/movies--subtree-end))
        items)
    (forward-line 1)
    (while (< (point) end)
      (when (looking-at "^[ \t]*[-+*] \\[\\([ Xx-]\\)\\] +\\(.*\\)$")
        (let ((mark (match-string-no-properties 1))
              (text (string-trim (match-string-no-properties 2))))
          (push `(("text" . ,text)
                  ("done" . ,(if (member mark '("X" "x")) t :json-false)))
                items)))
      (forward-line 1))
    (nreverse items)))

(defun perso/movies--explore-split (item)
  "Split an explore ITEM \"name : note\" into (NAME . NOTE).
The delimiter is space-colon-space, so a colon inside a title (e.g.
\"Blade Runner 2049\", \"Mission: Impossible\") is NOT treated as a separator.
NOTE is \"\" when the item has no note."
  (if (string-match "\\`\\(.*?\\)[ \t]+:[ \t]+\\(.*\\)\\'" item)
      (cons (string-trim (match-string 1 item))
            (string-trim (match-string 2 item)))
    (cons (string-trim item) "")))

(defun perso/movies--explore-name (item)
  "Return the NAME part of an explore ITEM formatted as \"name : note\"."
  (car (perso/movies--explore-split item)))

(defun perso/movies--explore-note (item)
  "Return the NOTE part of an explore ITEM formatted as \"name : note\" (\"\" if none)."
  (cdr (perso/movies--explore-split item)))

(defmacro perso/movies--with-write (&rest body)
  "Run BODY in the (scaffolded) movies buffer, widened, then save.  Return BODY's value."
  (declare (indent 0))
  `(with-current-buffer (perso/movies--get-buffer t)
     (org-with-wide-buffer
      (prog1 (progn ,@body)
        (save-buffer)))))

(defun perso/movies--json (alist)
  "Encode ALIST to JSON with vectors=arrays and explicit true/false."
  (let ((json-false :json-false))
    (json-encode alist)))

;; A shared checker for the two flat lists.
(defun perso/movies--check-list (section name-fn query)
  "Return a JSON report of exact/fuzzy matches for QUERY against SECTION.
NAME-FN maps a raw item string to the name to match against."
  (let ((buf (perso/movies--get-buffer nil))
        (q (perso/movies--clean query))
        (list-tag (if (string= section "To explore") "explore" "download")))
    (if (null buf)
        (perso/movies--json
         `(("list" . ,list-tag) ("query" . ,q)
           ("exact_match" . :json-false) ("any_match" . :json-false)
           ("exact" . ,(vector)) ("fuzzy" . ,(vector))
           ("note" . "movies.org does not exist yet")))
      (with-current-buffer buf
        (org-with-wide-buffer
         (let (items exact fuzzy)
           (save-excursion
             (when (perso/movies--goto-section section)
               (setq items (perso/movies--checkbox-items))))
           (dolist (it items)
             (let* ((nm (funcall name-fn it))
                    (m (perso/movies--match q nm)))
               (cond
                ((and m (string= (car m) "exact")) (push it exact))
                (m (push `(("item" . ,it) ("reason" . ,(cdr m))) fuzzy)))))
           (perso/movies--json
            `(("list" . ,list-tag) ("query" . ,q)
              ("exact_match" . ,(if exact t :json-false))
              ("any_match" . ,(if (or exact fuzzy) t :json-false))
              ("exact" . ,(vconcat (nreverse exact)))
              ("fuzzy" . ,(vconcat (nreverse fuzzy)))))))))))

;; A shared adder for the two flat lists, with duplicate SAFETY built in.
;; The tool must not depend on the model behaving: it never silently creates a
;; near-duplicate.  It classifies the new NAME against every existing item with
;; the same matcher the check tools use, then:
;;   - EXACT match (identical, or same IMDb id)      -> already_present, no write
;;   - FUZZY match and CONFIRMED is nil              -> possible_duplicate, no write
;;   - FUZZY match and CONFIRMED is non-nil          -> added (caller overrode)
;;   - no match                                       -> added
;; So a hand-annotated line like
;;   "- [ ] Perfect Days (2023, Wim Wenders, imdb: tt27503384) -> maybe sophie-compatible"
;; is recognised as the SAME film as "Perfect Days (2023, Wim Wenders, imdb:
;; tt27503384)" (same id) and reported already_present instead of duplicated.
(defun perso/movies--flat-add (section name-fn name line confirmed list-tag)
  "Add LINE to SECTION unless NAME duplicates an existing item.
NAME-FN maps a raw existing item to the string to compare NAME against.
CONFIRMED non-nil lets a fuzzy (not exact) match through.  Return JSON."
  (let ((file (perso/movies--file)))
    (perso/movies--with-write
      (perso/movies--ensure-section section)
      (let (exact fuzzy)
        (save-excursion
          (perso/movies--goto-section section)
          (dolist (it (perso/movies--checkbox-items))
            (let ((m (perso/movies--match name (funcall name-fn it))))
              (cond
               ((and m (string= (car m) "exact"))
                (push `(("item" . ,it) ("reason" . ,(cdr m))) exact))
               (m
                (push `(("item" . ,it) ("reason" . ,(cdr m))) fuzzy))))))
        (setq exact (nreverse exact) fuzzy (nreverse fuzzy))
        (cond
         (exact
          (perso/movies--json
           `(("list" . ,list-tag) ("name" . ,name) ("status" . "already_present")
             ("existing" . ,(vconcat exact)) ("file" . ,file))))
         ((and fuzzy (not confirmed))
          (perso/movies--json
           `(("list" . ,list-tag) ("name" . ,name) ("status" . "possible_duplicate")
             ("possible_matches" . ,(vconcat fuzzy))
             ("hint" . ,(concat "This looks like it may already be on the list "
                                "(fuzzy match — NOT added). Do not just retry. Tell me "
                                "what matched and ask whether to add it anyway; if I "
                                "confirm it is a genuinely different entry, call again "
                                "with confirmed=t."))
             ("file" . ,file))))
         (t
          (perso/movies--goto-section section)
          (perso/movies--append-in-subtree line)
          (perso/movies--json
           `(("list" . ,list-tag) ("name" . ,name)
             ("status" . ,(if fuzzy "added_confirmed" "added"))
             ("file" . ,file)))))))))

(defun perso/movies--flat-list (section list-tag &optional decorate-fn)
  "Return JSON for the whole SECTION checklist.
Each item is an alist with at least \"text\" and \"done\"; DECORATE-FN, if given,
receives that alist and returns an augmented one (e.g. adding name/note)."
  (let ((buf (perso/movies--get-buffer nil))
        (file (perso/movies--file)))
    (if (null buf)
        (perso/movies--json
         `(("list" . ,list-tag) ("count" . 0) ("items" . ,(vector))
           ("note" . "movies.org does not exist yet")))
      (with-current-buffer buf
        (org-with-wide-buffer
         (let (raw items)
           (save-excursion
             (when (perso/movies--goto-section section)
               (setq raw (perso/movies--checkbox-items-full))))
           (setq items (if decorate-fn (mapcar decorate-fn raw) raw))
           (perso/movies--json
            `(("list" . ,list-tag) ("count" . ,(length items))
              ("items" . ,(vconcat items)) ("file" . ,file)))))))))

;; ------------------------------------------------------------------------
;; Tool implementations
;; ------------------------------------------------------------------------

(defun perso/movies-explore-add (name &optional note confirmed)
  "Add NAME (+ optional NOTE) to the \"To explore\" list.
Refuses to create a near-duplicate unless CONFIRMED is non-nil.  Return JSON."
  (let* ((name (perso/movies--clean name))
         (note (let ((n (string-trim (or note "")))) (unless (string-empty-p n) n)))
         (line (concat "- [ ] " name (if note (concat " : " note) ""))))
    (when (string-empty-p name) (error "Explore item name is empty"))
    (perso/movies--flat-add "To explore" #'perso/movies--explore-name
                            name line confirmed "explore")))

(defun perso/movies-explore-check (query)
  "Report exact/fuzzy matches for QUERY on the \"To explore\" list.  Return JSON."
  (perso/movies--check-list "To explore" #'perso/movies--explore-name query))

(defun perso/movies-explore-list ()
  "Return the ENTIRE \"To explore\" list as JSON (name, note, done, text)."
  (perso/movies--flat-list
   "To explore" "explore"
   (lambda (it)
     (let* ((text (cdr (assoc "text" it)))
            (pair (perso/movies--explore-split text)))
       `(("name" . ,(car pair)) ("note" . ,(cdr pair))
         ("done" . ,(cdr (assoc "done" it))) ("text" . ,text))))))

(defun perso/movies-download-add (name &optional confirmed)
  "Add NAME (film title + identifying parenthetical) to the \"To download\" list.
Refuses to create a near-duplicate unless CONFIRMED is non-nil.  Return JSON."
  (let* ((name (perso/movies--clean name))
         (line (concat "- [ ] " name)))
    (when (string-empty-p name) (error "Download item name is empty"))
    (perso/movies--flat-add "To download" #'identity name line confirmed "download")))

(defun perso/movies-download-check (query)
  "Report exact/fuzzy matches for QUERY on the \"To download\" list.  Return JSON."
  (perso/movies--check-list "To download" #'identity query))

(defun perso/movies-download-list ()
  "Return the ENTIRE \"To download\" list as JSON (name, done, text)."
  (perso/movies--flat-list
   "To download" "download"
   (lambda (it)
     (let ((text (cdr (assoc "text" it))))
       `(("name" . ,text) ("done" . ,(cdr (assoc "done" it))) ("text" . ,text))))))

(defun perso/movies-gif-add-scene (movie &optional scene)
  "Add SCENE to MOVIE's entry on the \"To GIF\" list, creating the film's TODO
heading if absent.  SCENE is optional (omit to just create the film entry).
Return JSON."
  (let ((movie (perso/movies--clean movie))
        (scene (let ((s (string-trim (or scene ""))))
                 (unless (string-empty-p s) (perso/movies--clean s))))
        (file (perso/movies--file)))
    (when (string-empty-p movie) (error "GIF movie name is empty"))
    (perso/movies--with-write
      (perso/movies--ensure-section "To GIF")
      (let* ((sec-end (save-excursion
                        (perso/movies--goto-section "To GIF")
                        (perso/movies--subtree-end)))
             (created nil))
        ;; Locate or create the film heading; leave point on it.
        (perso/movies--goto-section "To GIF")
        (unless (perso/movies--goto-child movie sec-end)
          (perso/movies--goto-section "To GIF")
          (perso/movies--append-in-subtree (concat "** TODO " movie))
          (beginning-of-line)
          (setq created t))
        ;; Optionally add the scene, deduplicating within the film's subtree.
        (let ((scene-added nil) (scene-dup nil))
          (when scene
            (let ((exists (save-excursion
                            (seq-find (lambda (it) (perso/movies--ci= it scene))
                                      (perso/movies--checkbox-items)))))
              (if exists
                  (setq scene-dup t)
                (perso/movies--append-in-subtree (concat "- [ ] " scene))
                (setq scene-added t))))
          (perso/movies--json
           `(("list" . "gif") ("movie" . ,movie)
             ("movie_created" . ,(if created t :json-false))
             ("scene" . ,(or scene ""))
             ("scene_added" . ,(if scene-added t :json-false))
             ("scene_duplicate" . ,(if scene-dup t :json-false))
             ("file" . ,file))))))))

;; ------------------------------------------------------------------------
;; gptel tool registration
;; ------------------------------------------------------------------------

(with-eval-after-load 'gptel
  (gptel-make-tool
   :name "movies_explore_add"
   :function #'perso/movies-explore-add
   :category "movies"
   :confirm t
   :description
   (concat
    "Add an item to my film EXPLORE list (the \"To explore\" list in movies.org): "
    "things to look INTO later — a director, actor, genre, movement, era, studio, "
    "a piece of trivia, any film topic worth investigating. NOT a specific film to "
    "watch or acquire (use movies_download_add for that). "
    "Trigger on intents like: 'note this to explore', 'I should look into X', "
    "'add X to my explore list', 'remind me to dig into X', 'put X on my radar'. "
    "DUPLICATE SAFETY: the tool checks existing items itself. If it returns "
    "status 'already_present' it did nothing (already there). If it returns "
    "status 'possible_duplicate' it did NOT add — it lists the near-matches; "
    "relay them to me and ask, and only if I confirm it is genuinely different do "
    "you call again with confirmed=t. Do not blindly retry. "
    "Returns JSON {list, name, status: added|already_present|possible_duplicate, "
    "existing|possible_matches, file}.")
   :args
   (list '(:name "name"
           :type string
           :description "The topic to explore (actor, director, genre, movement, trivia…).")
         '(:name "note"
           :type string
           :optional t
           :description "One short sentence: why it is of interest and/or where it came from.")
         '(:name "confirmed"
           :type boolean
           :optional t
           :description "Set true ONLY to override a prior 'possible_duplicate' result after I confirmed the item is genuinely different. Leave unset normally.")))

  (gptel-make-tool
   :name "movies_explore_check"
   :function #'perso/movies-explore-check
   :category "movies"
   :description
   (concat
    "Check whether a topic is already on my film EXPLORE list — to avoid duplicates "
    "before movies_explore_add, or to recall context. Reports exact and fuzzy matches "
    "SEPARATELY. Use when I ask 'is X on my explore list?', 'have I noted X to look into?'. "
    "Returns JSON {list, query, exact_match, any_match, exact:[names], "
    "fuzzy:[{item, reason}]}.")
   :args
   (list '(:name "query"
           :type string
           :description "The topic to look for.")))

  (gptel-make-tool
   :name "movies_explore_list"
   :function #'perso/movies-explore-list
   :category "movies"
   :description
   (concat
    "Retrieve my ENTIRE film EXPLORE list — every topic I have noted to look into, "
    "in file order. Use this whenever I want to see the whole list or work from it, "
    "e.g. 'what's on my explore list?', 'show me everything I want to explore', "
    "'read me my explore list', 'what have I got to dig into?', or when I ask to "
    "explore something only vaguely and you should draw suggestions from it. "
    "Read-only. Returns JSON {list, count, items:[{name, note, done, text}], file}.")
   :args nil)

  (gptel-make-tool
   :name "movies_download_add"
   :function #'perso/movies-download-add
   :category "movies"
   :confirm t
   :description
   (concat
    "Add a specific FILM to my DOWNLOAD list (the \"To download\" list in movies.org): "
    "a movie I want to acquire later. This only RECORDS the title — it does not download "
    "anything. Trigger on: 'add X to my download list', 'I need to grab X', "
    "'put X on the list to get'. The name should be the film's international title plus "
    "any info that pins identification, in parentheses, e.g. 'Alien (1979)', "
    "'Metropolis (imdb: tt0017136)', 'The Thing (the 2011 prequel)'. Include the IMDb id "
    "when you know it — it is the most reliable way to match the right film. "
    "NOTE: 'stash for later' / 'add to my watchlist' usually means the Jellyfin watchlist "
    "(collection 'Marc'), NOT this list — see the project notes for routing. "
    "DUPLICATE SAFETY: the tool matches against existing entries itself, IGNORING any "
    "hand-written comment I appended to a line (it keys on the IMDb id when present, "
    "otherwise the title). status 'already_present' = same film already listed, nothing "
    "done. status 'possible_duplicate' = a near-match was found and NOTHING was added — "
    "report the match(es) to me and ask; only if I confirm it is a different film do you "
    "call again with confirmed=t. Never silently retry to force it in. "
    "Returns JSON {list, name, status: added|already_present|possible_duplicate, "
    "existing|possible_matches, file}.")
   :args
   (list '(:name "name"
           :type string
           :description "Film international title + identifying parenthetical (year, imdb id, or qualifier).")
         '(:name "confirmed"
           :type boolean
           :optional t
           :description "Set true ONLY to override a prior 'possible_duplicate' result after I confirmed the film is genuinely different. Leave unset normally.")))

  (gptel-make-tool
   :name "movies_download_check"
   :function #'perso/movies-download-check
   :category "movies"
   :description
   (concat
    "Check whether a film is already on my DOWNLOAD list — to avoid duplicates or to "
    "confirm it is queued. Reports exact and fuzzy matches SEPARATELY (fuzzy is useful "
    "here because titles carry a year/id, e.g. query 'Alien' vs item 'Alien (1979)'). "
    "Use when I ask 'is X on my download list?', 'am I already getting X?'. "
    "Returns JSON {list, query, exact_match, any_match, exact:[names], fuzzy:[{item, reason}]}.")
   :args
   (list '(:name "query"
           :type string
           :description "The film to look for (title, optionally with year/id).")))

  (gptel-make-tool
   :name "movies_download_list"
   :function #'perso/movies-download-list
   :category "movies"
   :description
   (concat
    "Retrieve my ENTIRE film DOWNLOAD list — every film I have queued to acquire, "
    "in file order, including any hand-written comment I left on a line. Use whenever "
    "I want to see or work from the whole list, e.g. 'what's on my download list?', "
    "'show me everything I want to download', 'read me my download list', "
    "'what am I planning to grab?'. Read-only. "
    "Returns JSON {list, count, items:[{name, done, text}], file}.")
   :args nil)

  (gptel-make-tool
   :name "movies_gif_add_scene"
   :function #'perso/movies-gif-add-scene
   :category "movies"
   :confirm t
   :description
   (concat
    "Add a scene to my GIF list (the \"To GIF\" list in movies.org): scenes I want to "
    "turn into GIFs, grouped per film. Trigger on: 'I want to gif X', 'add a gif idea for X', "
    "'note the scene where … for gifing', 'capture that moment in X'. "
    "'movie' is the film title (+ identifying parenthetical, like the download list). "
    "'scene' is the moment to capture — vague ('when X does Y') or precise "
    "('at 1:34:54 for 3s') — and is OPTIONAL (omit it to just create the film entry). "
    "Creates the film's TODO heading if it does not exist yet, otherwise appends the scene "
    "under the existing one. Deduplicates identical scenes within a film. "
    "Returns JSON {list, movie, movie_created, scene, scene_added, scene_duplicate, file}.")
   :args
   (list '(:name "movie"
           :type string
           :description "Film title + identifying parenthetical (year, imdb id, or qualifier).")
         '(:name "scene"
           :type string
           :optional t
           :description "The scene to capture: vague ('when X does Y') or precise ('at 1:34:54 for 3s').")))
  )

(provide 'movies)
;;; movies.el ends here
