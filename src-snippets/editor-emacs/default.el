(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace t)
(setq-default indicate-empty-lines t)
(column-number-mode t)
(global-font-lock-mode t)
(global-linum-mode t)
(show-paren-mode t)
(blink-cursor-mode 0)
(show-paren-mode t)
(setq-default cursor-type 'box)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(require 'iso-transl)

(require 'package)
(package-initialize 'noactivate)
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure nil
      use-package-ensure-function 'ignore
      package-enable-at-startup nil
      package--init-file-ensured t)

(use-package nix-mode
  :mode ("\\.nix\\'"))

(use-package direnv
  :config (direnv-mode))

(put 'dante-project-root 'safe-local-variable #'stringp)
(put 'dante-package-name 'safe-local-variable #'stringp)
(put 'dante-repl-command-line 'safe-local-variable #'listp)
(put 'haskell-language-extensions 'safe-local-variable #'listp)
(put 'dante-target 'safe-local-variable #'stringp)
(put 'haskell-language-extensions 'safe-local-variable #'stringp)

(use-package haskell-mode
    :mode ("\\.hs" . haskell-mode)
    :custom (haskell-language-extensions '("-XRecursiveDo" "-XTypeApplications"))
    :init
    (add-hook 'haskell-mode-hook 'hindent-mode)
    (add-hook 'haskell-mode-hook (lambda () (direnv-update-environment) (dante-mode)))
    (use-package dante
      :ensure t
      :commands 'dante-mode
      :config
      (make-variable-buffer-local 'dante-target)
      (add-to-list 'flycheck-checkers 'haskell-dante 'append)
      (add-hook 'dante-mode-hook
                '(lambda ()
                   (flycheck-add-next-checker 'haskell-dante
                                              '(warning . haskell-hlint))))
      (setq dante-methods '(new-build stack bare-cabal bare-ghci))
      (setq dante-load-flags
            '("+c" "-fno-diagnostics-show-caret" "-Wwarn=missing-home-modules" "-ferror-spans" "-fdiagnostics-color=never")))
    (use-package hindent
      :diminish
      :commands 'hindent-mode))

(use-package hasky-extensions
  :custom
  (hasky-extensions-aligning nil)
  (hasky-extensions
   '("OverloadedStrings"
    "CPP"
    "FlexibleContexts"
    "FlexibleInstances"
    "AllowAmbiguousTypes"
    "BangPatterns"
    "DataKinds"
    "DeriveAnyClass"
    "DeriveDataTypeable"
    "DeriveFoldable"
    "DeriveFunctor"
    "DeriveGeneric"
    "DeriveTraversable"
    "DerivingStrategies"
    "EmptyDataDecls"
    "ExistentialQuantification"
    "ExplicitForAll"
    "FunctionalDependencies"
    "GADTs"
    "GeneralizedNewtypeDeriving"
    "InstanceSigs"
    "KindSignatures"
    "LambdaCase"
    "MultiParamTypeClasses"
    "MultiWayIf"
    "PatternSynonyms"
    "PolyKinds"
    "QuasiQuotes"
    "RankNTypes"
    "RecordWildCards"
    "RecursiveDo"
    "ScopedTypeVariables"
    "StandaloneDeriving"
    "TemplateHaskell"
    "TupleSections"
    "TypeApplications"
    "TypeFamilies"
    "TypeOperators"
    "TypeSynonymInstances"
    "UndecidableInstances"
    "ViewPatterns"))
  :bind ("C-c y" . hasky-extensions))

(use-package magit
  :defer 0.5
  :diminish auto-revert-mode
  :custom (magit-completing-read-function 'ivy-completing-read)
  :config
  (global-magit-file-mode))

(use-package undo-tree
  :diminish undo-tree-mode
  :custom (undo-tree-enable-undo-in-region nil)
  :config
  (global-undo-tree-mode))

(use-package flycheck
  :custom (flycheck-check-syntax-automatically '(mode-enabled save))
  :config
  (global-flycheck-mode)
  (setq flycheck-executable-find
        (lambda (cmd) (direnv-update-environment default-directory) (executable-find cmd))))
