;; Emacs-musictube - Search and play YouTube music videos with emacs 
;; Copyright (C) 2017 Valts Liepiņš <valts@tase.lv>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'helm)

(defvar musictube-search-cache '()
  "Variable for caching YouTube search results")

(defvar musictube-last-search 0
  "Determins whether there is ongoing search request")

(defvar musictube-youtube-api-key (when (file-exists-p ".yt-api-key")
                          (with-temp-buffer
                            (insert-file-contents ".yt-api-key")
                            (buffer-string)))
  "Youtube API key for querying videos")

;; Utils ;;
;;=======;;

(defun alist-deep-get (symbols alist)
  "Look up the value for the chain of SYMBOLS in ALIST."
  ; If symbols exist
  (if symbols
      ; Start recursion
      (alist-deep-get
       ; Get CDR of symbols
       (cdr symbols)
                 (assoc (car symbols) alist))
    ;; Else, return CDR of alist
    (cdr alist)))

;; VLC ;;
;;=====;;

(defun vlc-get-process ()
  "Find or start a vlc process"
  (let ((process (get-process "vlc")))
    (if process
        process
      (let ((vlc-process (start-process "vlc" "*vlc*" "rvlc"
                                        "--no-video"
                                        ; Give plenty of buffer space
                                        "--network-caching" "10000")))
        (accept-process-output vlc-process)
        vlc-process))))

(defun vlc-play-track (track)
  "Play the TRACK"
  (process-send-string (vlc-get-process)
                       (format "add %s\n" track)))

(defun vlc-enqueue-track (track)
  "Enqueue the TRACK"
  (process-send-string (vlc-get-process)
                       (format "enqueue %s\n" track)))

(defun vlc-is-playing ()
  "Retrieve VLC play state"
  (let ((vlc-process (vlc-get-process)))
    (process-send-string vlc-process "is_playing\n")
    ;; Wait for output
    (accept-process-output vlc-process)
    (with-current-buffer (process-buffer vlc-process)
      (goto-char (point-max))
      (search-backward-regexp "\\([[:digit:]]\\)")
      (string-equal (match-string 1) "1"))))

;; Musictube ;;
;;===========;;

(defun musictube-format-item (item) (let ((video-title (alist-deep-get '(snippet title) item))
        (channel-name (alist-deep-get '(snippet channelTitle) item)))
    (format "%s\n/%s/" channel-name video-title)))

(defun musictube-search (query)
  "Search YouTube for QUERY and return results"
  (let ((a-url "https://www.googleapis.com/youtube/v3/search")
        (url-request-method "GET")
        (url-args
         (format "?q=%s&key=%s&part=snippet&type=video&maxResults=20" query musictube-youtube-api-key)))
    (with-current-buffer
        (url-retrieve-synchronously (concat a-url url-args))
      (goto-char url-http-end-of-headers)
      (json-read))))

(defun musictube-search-formatted (query)
  (mapcar (lambda (item)
            (cons (musictube-format-item item) item))
          (alist-get 'items (musictube-search query))))

(defun musictube-play-item (item &optional enqueue)
  "Play item with VLC"
  (let ((track (format "https://www.youtube.com/watch?v=%s" (alist-deep-get '(id videoId) item)))) (vlc-play-track track)))

(defun musictube-queue-item (item)
  "Queue or play item with VLC"
  (let ((track (format "https://www.youtube.com/watch?v=%s" (alist-deep-get '(id videoId) item))))
    (if (vlc-is-playing)
        (vlc-enqueue-track track)
      (vlc-play-track track))))

(defun musictube-filter-items (query alist)
  (map-filter (lambda (key item)
                (when (helm-mm-match key query) 't))
              alist))

;; HELM ;;
;;======;;

(defun helm-musictube-search ()
  (let ((cache-matches (length (musictube-filter-items helm-pattern
                         musictube-search-cache))))
    ;; YouTube search API calls are expensive, cache when possible
    (if (and (> (length helm-pattern) 4)
             (> (- (time-to-seconds) musictube-last-search) 0.5)
             (< cache-matches 1))
        (progn
          (setq musictube-last-search (time-to-seconds)
                musictube-search-cache (musictube-search-formatted helm-pattern)))
      musictube-search-cache)))

(defun helm-musictube-actions-for-item (actions item)
  `((,(format "Queue and play: %s" (alist-deep-get '(snippet title) item)) . musictube-queue-item)
    (,(format "Play: %s" (alist-deep-get '(snippet title) item)) . musictube-play-item)))

(defun helm-musictube ()
  "Search YouTube for music in helm"
  (interactive)
  (helm :sources (helm-build-async-source "Musictube"
                     :candidates-process 'helm-musictube-search
                     :action-transformer 'helm-musictube-actions-for-item
                     :match 'helm-mm-match
                     :volatile t
                     :multiline t))
  :buffer "*helm-musictube*")
