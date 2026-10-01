;;; perso.example.el --- Template for perso.el -*- lexical-binding: t -*-

;; perso.el holds private values and is not versioned (see .gitignore).
;; To create it: copy this file to perso.el, then replace every value.
;; The values below are fake; they only let the configuration start.
;; This file lists every value the configuration reads from perso.el.

;;; Org: CalDAV synchronisation (org-caldav, personal profile only)
(setq cloud-caldav-url "https://caldav.example.invalid/remote.php/dav/calendars/user")

;;; Mail: mu4e contexts
(setq protonmail-user-mail-address "user@protonmail.example.invalid")
(setq protonmail-user-full-name "First Last")
(setq gmail-user-mail-address "user@gmail.example.invalid")
(setq gmail-user-full-name "First Last")

;;; AI: MCP servers
(setq perso/mcp-searxng-url   "http://127.0.0.1:8001/mcp")
(setq perso/mcp-wikipedia-url "http://127.0.0.1:8002/mcp")
(setq perso/mcp-datagouv-url  "http://127.0.0.1:8003/mcp")
(setq perso/mcp-jellyfin-url  "http://127.0.0.1:8004/mcp")
(setq perso/mcp-omdb-url      "http://127.0.0.1:8005/mcp")
(setq perso/mcp-tmdb-url      "http://127.0.0.1:8006/mcp")

;;; AI: local llama.cpp servers (host and port are strings)
(setq perso/llm-host-main   "127.0.0.1")
(setq perso/llm-port-main   "8080")
(setq perso/llm-host-backup "127.0.0.1")
(setq perso/llm-port-backup "8081")

;;; perso.example.el ends here
