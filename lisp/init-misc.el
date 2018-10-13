;; -----------------------------------------------------------------------------
;; doc-view-mode support https://www.emacswiki.org/emacs/DocViewMode
;; 1. Set up png support, you’ll have to look elsewhere for instructions on this.
;; 2. Install ghostscript and add the bin and lib directories to your path.
;; 3. Get xpdf for windows and put the executables somewhere on your path.
;; 4. Set this in your .emacs: (setq doc-view-ghostscript-program “gswin32c”)
;; 5. That should be it.
;; -----------------------------------------------------------------------------
(when (jh/windows?)
  (setq doc-view-ghostscript-program "gswin32c"))

(provide 'init-misc)
