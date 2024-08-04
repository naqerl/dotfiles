(provide 'suzu-buffer)

(defun suzu/buffer-prev ()
  "Switch to previous buffer in the current perspective"
  (interactive)
  (let* ((buffers (persp-current-buffer-names t))
	 (prev-buffer-idx (- (cl-position (buffer-name) buffers) 1))
	 (prev-buffer-name (if (< prev-buffer-idx 0) (car (last buffers)) (nth prev-buffer-idx buffers))))
    (persp-switch-to-buffer prev-buffer-name)
    ))

(defun suzu/buffer-next ()
  "Switch to next buffer in the current perspective"
  (interactive)
  (let* ((buffers (persp-current-buffer-names t))
	 (next-buffer-idx (+ (cl-position (buffer-name) buffers) 1))
	 (next-buffer-name (if (>= next-buffer-idx (safe-length buffers)) (nth 0 buffers) (nth next-buffer-idx buffers))))
    (persp-switch-to-buffer next-buffer-name)
    ))

(defun copy-full-path-to-kill-ring ()
  "copy buffer's full path to kill ring"
  (interactive)
  (when buffer-file-name
    (let ((path (file-truename buffer-file-name)))
      (kill-new (file-truename buffer-file-name))
      (message "Path copied [%s]" path)
      )
    ))

(defun suzu/center-buffer ()
  (let ((margin-size (/ (- (frame-width) 80) 2)))
    (set-window-margins nil margin-size margin-size)))

