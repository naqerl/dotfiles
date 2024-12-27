;;; grayscale-ansi --- Grayscale theme for ansi -*- lexical-binding: t; -*-

;;; Commentary:
;; Adds grayscale colortheme to ansi

;;; Code:
(require 'grayscale-theme)

(defvar grayscale-ansi-colors
  '(`(ansi-color-black
      nil
      :foreground ,(plist-get grayscale-theme-colors :bg))
    `(ansi-color-red
      nil
      :foreground ,(plist-get grayscale-theme-colors :red))
    `(ansi-color-green
      nil
      :foreground ,(plist-get grayscale-theme-colors :green))
    `(ansi-color-yellow
      nil
      :foreground ,(plist-get grayscale-theme-colors :fg+1))
    `(ansi-color-blue
      nil
      :foreground ,(plist-get grayscale-theme-colors :fg-1))
    `(ansi-color-magenta
      nil
      :foreground ,(plist-get grayscale-theme-colors :fg-1))
    `(ansi-color-cyan
      nil
      :foreground ,(plist-get grayscale-theme-colors :fg+1))
    `(ansi-color-white
      nil
      :foreground ,(plist-get grayscale-theme-colors :fg))
    `(ansi-color-bright-black
      nil
      :foreground ,(plist-get grayscale-theme-colors :bg))
    `(ansi-color-bright-red
      nil
      :foreground ,(plist-get grayscale-theme-colors :red))
    `(ansi-color-bright-green
      nil
      :foreground ,(plist-get grayscale-theme-colors :green))
    `(ansi-color-bright-yellow
      nil
      :foreground ,(plist-get grayscale-theme-colors :fg+1))
    `(ansi-color-bright-blue
      nil
      :foreground ,(plist-get grayscale-theme-colors :fg-1))
    `(ansi-color-bright-magenta
      nil
      :foreground ,(plist-get grayscale-theme-colors :fg-1))
    `(ansi-color-bright-cyan
      nil
      :foreground ,(plist-get grayscale-theme-colors :fg+1))
    `(ansi-color-bright-white
      nil
      :foreground ,(plist-get grayscale-theme-colors :fg))))

(dolist (config grayscale-ansi-colors)
  (apply #'set-face-attribute (eval config)))

(provide 'grayscale-ansi)
;;; grayscale-ansi.el ends here
