;;; init-ai-movies.el --- Movie assistant: curator preset, movie tools and servers -*- lexical-binding: t -*-

;;; Commentary:
;; Loaded right after init-ai, which it extends.  Disabled with EMACS_NOAIMOVIES=Y.

;;; Code:

;; Values from perso.el (a server whose URL is not set is skipped)
(perso/defsetting perso/mcp-jellyfin-url "URL of the jellyfin MCP server")
(perso/defsetting perso/mcp-omdb-url "URL of the omdb MCP server")
(perso/defsetting perso/mcp-tmdb-url "URL of the tmdb MCP server")

;; MCP servers
(with-eval-after-load 'mcp
  (dolist (server `(("jellyfin" . (:url ,perso/mcp-jellyfin-url :timeout 30))
                    ("omdb"     . (:url ,perso/mcp-omdb-url     :timeout 30))
                    ("tmdb"     . (:url ,perso/mcp-tmdb-url     :timeout 30))))
    (when (plist-get (cdr server) :url)
      (add-to-list 'mcp-hub-servers server t))))

;; Movie tools (lisp/movies.el), registered when gptel loads
(with-eval-after-load 'gptel
  (setq perso/movies-org-file (expand-file-name "movies.org" org-directory))
  (require 'movies))

;; Movie tools that run without asking for confirmation
(with-eval-after-load 'gptel-tool-policy
  (setq gptel-tool-policy-bypass-tools
        (append gptel-tool-policy-bypass-tools
                '("movies_gif_add_scene" "movies_download_list" "movies_download_check" "movies_download_add"
                  "movies_explore_list" "movies_explore_check" "movies_explore_add"
                  "tmdb" "omdb" "jellyfin" "jellyfin_favorite_set" "jellyfin_collection_add" "jellyfin_watched_set" "movie_ratings"))))

;; Curator preset
(with-eval-after-load 'gptel-builder
  (gptel-builder-define-preset 'curator
                               :description "Film curator assistant."
                               :recipe '(:selections ((roles "film_curator.org")
                                                      (skills "film/fact_checking.org" "film/recommendation.org" "film/taste_profiling.org" "information_retrieval.org")
                                                      (projects "film/film_marc.org")
                                                      (outputs "film/film_reco.org"))
                                                     :datetime t :mode frozen :agentic t :subagents ("web_searcher"))
                               :parents '(gptel-agent)
                               :tools '("current_datetime" "tmdb" "movie_ratings" "movies_download_add" "movies_download_check" "movies_explore_add" "movies_explore_check" "jellyfin_favorite_set" "jellyfin_watched_set" "movies_gif_add_scene" "Agent" "jellyfin" "jellyfin_collection_add" "movies_download_list" "movies_explore_list")
                               :use-tools t
                               :confirm-tool-calls 'auto
                               :backend "OpenCode Go"
                               :model 'deepseek-v4-flash
                               :pre (lambda () (gptel-mcp-connect '("tmdb" "omdb" "jellyfin") 'sync nil))))

(provide 'init-ai-movies)
;;; init-ai-movies.el ends here
