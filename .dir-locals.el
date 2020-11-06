((nil . ((dante-methods . (bare-cabal)))))
;; You will also want to put a `.dir-locals.el` file in every top Haskell
;; directory, with contents like:
((nil . ((dante-method . bare-cabal)
        (dante-target . "dist"))))
(after! dante
  (setq dante-methods-alist
        `((bare-cabal ,(lambda (d) (directory-files d t "..cabal$")) ("cabal" "v1-repl" dante-target "--builddir=dist/dante")))))
