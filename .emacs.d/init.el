(org-babel-load-file
 (expand-file-name "config.org" user-emacs-directory))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("a2b0b0ab15399b997da4f8958536c0ed71b12127cd47843b83f211f88c78add8"
     "fbaed20abd45b17c65d69dd4314212a2a8180c79a255fa5c1791f2549bdf7a4a"
     "36da0af886eb1ee5f321149c31707a5510dd4cc1ed2f69ed9a3b0276ff82d0ef"
     "59cca2de938ad11f1d419bee254b4498fe375515ab961bde462f1e5c7bea98e7"
     "00ea4b07ac2dde79da23da564d0b8d3974e95701bbe4eede6ac71c1a5f8e9961"
     "23de4de37d1f0bf38f3f64c7c3404ec54fbdb9f07476389d003f19e04234999c"
     "32c79ea2cf873669ad4aa4f4d108623ebea4110c22ab95cbaf71edb717e63459"
     "5b2e154e24611e50b51723f67415ff0f263030c2880519729cdf515fdf1583ee"
     "339d91c01bb8b6faea3bc817ac197ea054e6696f05c31aa44e9e67dc9a6fb633"
     "a64e59486bcd9e2932f3a1ac5e78623b9e0dcd3267b6da9e3ffeb5e79b2ea5b4"
     "e0e1a134e00663bace2421979024088103575fc74039a48a892aa88406a3fb9e"
     "c2bcfcf9424cdcec14b3dd307c9bc304b07360a51fa5c1e7b0a491d23d95719e"
     "0dcdba3915680245a54316827a41ff390aaf4eaa1c169601ad4ae8cb4e5bd949"
     "9596485c45f112a7d2de06c53474832ed59488465c3643ee1f7624e06ecb263f"
     "817069e1547b55c68c3ea7983dcf9f174303e605ae591a44dd0e0b36e6a4cb3e"
     "43b77c36d59e792f6758f4c337c2c8c0eb3bcd3e3b81ba938ceecab0bec3612f"
     "a014cd1abe3691aec2adf800adbdb8f082a0390ccbe7db695e5540a2a9cabfe4"
     "cbfac9d7ad995e2914929ffed6dd9571134e3bcbc767b8a583a28720e16ffde3"
     "39be9906b90832c628aff734d69fa94b1db8e1f5b65a19db96d5104e4e5253e4"
     "70b9c65b450b1ff1c3e64e53880d6648132ad7fb8d91c722620b07c590fa3520"
     "2cb34d08101e179a2d98da9e7dab4905ce37a487fd76dc651d0822b71e46836f"
     "872cbaac5c1849f1ed7f958b1548a4f3af410a85d99bd7ef9e0c6fd12b7aa577"
     "49f2dfc7565776af69fa90c7dcb06e0684a77c7a64b567eb9c9f386e259d9482"
     "d59fbc13a3be1d8a5f4b728a134c40bc02defce8f57cb59a76d7382e43edf5f2"
     "64d1128e7665da4d45c5a1471e565becec0633ef38ca99197f09db733fc86965"
     "5501ea6d24cc1a5b97ced3ab16aef3e2b5bc9f0a0a8a08eaecad117e27d79038"
     "b7a824f277682c4d42ff46c20b20ff92e846916c642a716cebfa5ed58934b362"
     "4f5544cf02e4c4d8468066f56bda0b43d9ff44ed69fc6855a6fc94f6346aedfa"
     "555b5ca6ca7b2625296ef5a25d737b7fa005ca5d42a1bd0f15cf3fa893b9f18c"
     "77be2a2c8a510993f6e6abf8ab62fd5ee6890430545f9058a209188d074a29a3"
     "7f0304fc1adb4bfbf252c3bb63151eb1411289d0d7b5001d62d07686b7ffa23c"
     "32eda95da1c21875322f6b7630d7f0ffd9f90f53cd392002b827477a5adfb573"
     "fc78ad97d2ca7b48158560029d609f1801369822af8ea590a3907bf4d98939b4"
     "f9b72901e4be8f60a68bf29543a54cb7ab2ec26fe2dc264051623e4dc433d547"
     "caf5d8bf180b195b4914601da3532805977479e96080a98de4cdc075cef9f50c"
     "ce6ae55197b5d20afb707fab18b461231b0b7dceee471f464cf85d9b367ca965"
     "c6e1d3e82921c79562511eb4d0af6efb39666e41e309598067c0481c5925cc7f"
     "2f9cff368c07d280a7a766e9f04a0053a17bb74f775504dc49421d1fda2a0797"
     default))
 '(package-selected-packages '(git-gutter-fringe make-project nginx-mode rust-docs))
 '(package-vc-selected-packages '((rust-docs :url "https://github.com/scipunch/rust-docs")))
 '(warning-suppress-types '((comp))))
(put 'downcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-doc-face ((t (:inherit italic :foreground "#868686"))))
 '(font-lock-keyword-face ((t (:inherit italic :foreground "#BEC3C6" :weight bold))))
 '(font-lock-string-face ((t (:inherit italic :foreground "#738678"))))
 '(fringe ((t (:foreground "#BEC3C6" :background "#BEC3C6"))))
 '(org-code ((t (:inherit default :background "black" :foreground "#e6e6e6" :box (:line-width (1 . 1) :color "#868686" :style released-button)))))
 '(org-table ((t (:inherit default :foreground "#b6b6b6"))))
 '(window-divider ((t (:foreground "#424242"))))
 '(window-divider-first-pixel ((t (:foreground "#424242"))))
 '(window-divider-last-pixel ((t (:foreground "#424242")))))
