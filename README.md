# Musictube

Search YouTube with helm interface and play the audio with VLC.
Little project that began as practice emacs extension and turned into pretty
handy music player.

Note that I'm still very new to emacs, so feel free to suggest improvements.

# Requirements

* VLC player
* YouTube API key stored in .yt-api-key file(required for video search API)
* emacs-helm

# Functions


## helm-musictube

Main interface for the extension. Brings up helm and lets you search for YouTube
videos. Primary function queues video or, in case nothing is currently playing,
plays it imidiately.
