(defvar capture-dir
  (if (jh/windows?)
    "e:\\Video\\capture"
    "~/Video/capture"))

(defvar capture-prog "ffmpeg"
  "ffmpeg executable path.")
(defvar capture-buffer "*ffmpeg-capture*")

(defun jh/ffmpeg-capture-filename ()
  "Generate capture filename."
  (concat "ffmpeg" (format-time-string "%Y%m%d%H%M%S") ".mkv"))

(defun jh/ffmpeg-start-on-windows (FILENAME &optional FPS)
  "Start capture command on Windows."
  (unless FPS (setq FPS 30))
  (let ((default-directory capture-dir))
    (start-process "ffmpeg" capture-buffer capture-prog
      "-f" "gdigrab"
      "-framerate" (int-to-string FPS)
      "-i" "desktop"
      FILENAME)))

(defun jh/ffmpeg-start-on-mac (FILENAME &optional FPS)
  "Start capture command on macOS."
  (unless FPS (setq FPS 30))
  (let ((default-directory capture-dir))
    (start-process "ffmpeg" capture-buffer capture-prog
      "-f" "avfoundation"
      "-framerate" (int-to-string FPS)
      "-i" "1:1"
      FILENAME)))

(defun jh/ffmpeg-capture-run ()
  "Run ffmpeg capture process."
  (interactive)
  (unless (executable-find capture-prog)
    (error (concat "capture-prog doesn't exist: " capture-prog)))
  (unless (file-directory-p capture-dir)
    (error (concat "capture-dir doesn't exist: " capture-dir)))
  (let ((filename (jh/ffmpeg-capture-filename)))
    (when (jh/windows?)
      (jh/ffmpeg-start-on-windows filename))
    (when (jh/mac?)
      (jh/ffmpeg-start-on-mac filename))))

(defun jh/ffmpeg-capture-quit ()
  "Quit ffmpeg capture process."
  (interactive)
  (process-send-string capture-buffer "q"))

(provide 'init-ffmpeg)
