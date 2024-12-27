;;; grayscale-vterm --- Grayscale theme for vterm -*- lexical-binding: t; -*-

;;; Commentary:
;; Adds grayscale colortheme to vterm buffers

;;; Code:
(require 'grayscale-theme)

(defvar grayscale-vterm-colors
  '(`(vterm-color-black
      nil
      :foreground ,(plist-get grayscale-theme-colors :bg))
    `(vterm-color-red
      nil
      :foreground ,(plist-get grayscale-theme-colors :red))
    `(vterm-color-green
      nil
      :foreground ,(plist-get grayscale-theme-colors :green))
    `(vterm-color-blue
      nil
      :foreground ,(plist-get grayscale-theme-colors :fg-1))
    `(vterm-color-magenta
      nil
      :foreground ,(plist-get grayscale-theme-colors :fg+1))
    `(vterm-color-yellow
      nil
      :foreground ,(plist-get grayscale-theme-colors :fg+1))
    `(vterm-color-cyan
      nil
      :foreground ,(plist-get grayscale-theme-colors :fg))
    `(vterm-color-white
      nil
      :foreground ,(plist-get grayscale-theme-colors :fg))
    `(vterm-color-bright-red
      nil
      :foreground
      ,(plist-get grayscale-theme-colors :red+1))
    `(vterm-color-bright-green
      nil
      :foreground
      ,(plist-get grayscale-theme-colors :green+1))
    `(vterm-color-bright-blue
      nil
      :foreground
      ,(plist-get grayscale-theme-colors :fg-1))
    `(vterm-color-bright-magenta
      nil
      :foreground
      ,(plist-get grayscale-theme-colors :fg+1))
    `(vterm-color-bright-yellow
      nil
      :foreground
      ,(plist-get grayscale-theme-colors :fg+1))
    `(vterm-color-bright-cyan
      nil
      :foreground
      ,(plist-get grayscale-theme-colors :fg))))

(dolist (config grayscale-vterm-colors)
  (apply #'set-face-attribute (eval config)))

(provide 'grayscale-vterm)
;;; grayscale-vterm.el ends here
