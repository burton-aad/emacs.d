;; -*- lexical-binding: t; -*-

(use-package moody
  :disabled
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))


(use-package spaceline
  :disabled
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme)
  ) ;;spaceline


(use-package mood-line
  :disabled
  :config
  (mood-line-mode)
  )


(use-package powerline
  :disabled

  :init
  (setq powerline-default-separator (quote wave))
  (setq powerline-display-mule-info t)
  (setq powerline-display-buffer-size nil)
  (setq powerline-height nil)

  :config
  ;; (powerline-default-theme)
  (powerline-center-theme)
  ;; (powerline-vim-theme)
  ;; (powerline-nano-theme)
  (set-face-attribute 'mode-line nil
                      :background "dark slate blue"
                      :foreground "white")
  (set-face-attribute 'powerline-active1 nil
                      :foreground "gray80"
                      :background "navy")
  (set-face-attribute 'powerline-active2 nil
                      :foreground "gray"
                      :background "slate blue")
  ) ;; powerline


(use-package doom-themes
  :disabled
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each
  ;; theme may have their own settings.
  (load-theme 'doom-one t)
  ;; (load-theme 'doom-vibrant t)
  ;; (load-theme 'doom-acario-dark t)
  ;; (load-theme 'doom-acario-light t)
  ;; (load-theme 'doom-ayu-dark t)
  ;; (load-theme 'doom-ayu-light t)
  ;; (load-theme 'doom-city-lights t)
  ;; (load-theme 'doom-challenger-deep t)
  ;; (load-theme 'doom-dark+ t)
  ;; (load-theme 'doom-dracula t)
  ;; (load-theme 'doom-ephemeral t)
  ;; (load-theme 'doom-fairy-floss t)
  ;; (load-theme 'doom-flatwhite t)
  ;; (load-theme 'doom-gruvbox t)
  ;; (load-theme 'doom-gruxbox-light t)
  ;; (load-theme 'doom-henna t)
  ;; (load-theme 'doom-homage-white t)
  ;; (load-theme 'doom-homage-black t)
  ;; (load-theme 'doom-horizon t)
  ;; (load-theme 'doom-Iosvkem t)
  ;; (load-theme 'doom-laserwave t)
  ;; (load-theme 'doom-material t)
  ;; (load-theme 'doom-manegarm t)
  ;; (load-theme 'doom-miramare t)
  ;; (load-theme 'doom-molokai'
  ;; (load-theme 'doom-monokai-classic t)
  ;; (load-theme 'doom-monokai-pro t)
  ;; (load-theme 'doom-moonlight t)
  ;; (load-theme 'doom-nord t)
  ;; (load-theme 'doom-nord-light t)
  ;; (load-theme 'doom-nova t)
  ;; (load-theme 'doom-oceanic-next t)
  ;; (load-theme 'doom-old-hope t)
  ;; (load-theme 'doom-opera t)
  ;; (load-theme 'doom-opera-light t)
  ;; (load-theme 'doom-outrun t)
  ;; (load-theme 'doom-palenight t)
  ;; (load-theme 'doom-peacock t)
  ;; (load-theme 'doom-plain t)
  ;; (load-theme 'doom-plain-dark t)
  ;; (load-theme 'doom-rouge t)
  ;; (load-theme 'doom-snazzy t)
  ;; (load-theme 'doom-solarized-dark t)
  ;; (load-theme 'doom-solarized-light t)
  ;; (load-theme 'doom-sourcerer t)
  ;; (load-theme 'doom-spacegrey t)
  ;; (load-theme 'doom-tomorrow-night t)
  ;; (load-theme 'doom-tomorrow-day t)
  ;; (load-theme 'doom-wilmersdorf t)
  ;; (load-theme 'doom-zenburn t)

  ;; Enable flashing mode-line on errors
  ;; (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme
  (doom-themes-neotree-config)  ; all-the-icons fonts must be installed!
  )

(use-package doom-modeline
  :disabled
  :config
  (doom-modeline-mode 1)
  (setq doom-modeline-bar-width 3)
  )
