;;; init.el -*- lexical-binding: t; -*-

(doom! :private
       my-chinese          ; personal chinese module

       :completion
       corfu              ; complete with cap(f), cape and a flying feather!
       vertico           ; the search engine of the future

       :ui
       ;;deft              ; notational velocity for Emacs
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       ;;doom-quit         ; DOOM quit-message prompts when you quit Emacs
       ;;(emoji +unicode)  ; 🙂
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       ;;indent-guides     ; highlighted indent columns
       ;;ligatures         ; ligatures and symbols to make your code pretty again
       ;;minimap           ; show a map of the code on the side
       (modeline +light)          ; snazzy, Atom-inspired modeline, plus API
       ;;nav-flash         ; blink cursor line after big motions
       ;;neotree           ; a project drawer, like NERDTree for vim
       ophints           ; highlight the region an operation acts on
       (popup +defaults)   ; tame sudden yet inevitable temporary windows
       smooth-scroll     ; So smooth you won't believe it's not butter
       ;;tabs              ; a tab bar for Emacs
       ;;treemacs          ; a project drawer, like neotree but cooler
       ;;unicode           ; extended unicode support for various languages
       (vc-gutter +pretty) ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       ;;window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces
       ;;zen               ; distraction-free coding or writing

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       format            ; automated prettiness
       ;;multiple-cursors  ; editing in many places at once
       ;;objed             ; text object editing for the innocent
       ;;parinfer          ; turn lisp into python, sort of
       ;;rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       word-wrap         ; soft wrapping with language-aware indent

       :emacs
       (dired +dirvish)             ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       ;;ibuffer         ; interactive buffer management
       undo              ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       ;;eshell            ; the elisp shell that works everywhere
       ;;shell             ; simple shell REPL for Emacs
       ;;term              ; basic terminal emulator for Emacs
       ;;vterm             ; the best terminal emulation in Emacs

       :checkers
       (syntax +flymake +childframe)              ; tasing you for every semicolon you forget
       ;;(spell +flyspell) ; tasing you for misspelling mispelling
       ;;grammar           ; tasing grammar mistake every you make

       :tools
       ;;ansible
       biblio            ; Writes a PhD for you (citation needed)
       ;;editorconfig      ; let someone else argue about tabs vs spaces
       (eval +overlay)     ; run code, run (also, repls)
       lookup              ; navigate your code and its documentation
       magit             ; a git porcelain for Emacs
       tree-sitter       ; syntax and parsing, sitting in a tree...

       :os
       ;;tty               ; improve the terminal Emacs experience

       :lang
       data              ; config/data formats
       emacs-lisp        ; drown in parentheses
       ;;ess               ; emacs speaks statistics
       (go +tree-sitter)         ; the hipster dialect
       ;;(graphql +lsp)    ; Give queries a REST
       (json +tree-sitter)              ; At least it ain't XML
       ;;(java +lsp)       ; the poster child for carpal tunnel syndrome
       ;;javascript        ; all(hope(abandon(ye(who(enter(here))))))
       ;;(kotlin +lsp)            ; a better, slicker Java(Script)
       markdown          ; writing docs for people to ignore
       (org               ; organize your plain life in plain text
        +crypt
        +roam2
        +hugo
        +pretty
        +present
        +dragndrop)
       ;;php               ; perl's insecure younger brother
       ;;plantuml          ; diagrams for confusing people more
       (python +tree-sitter)            ; beautiful is better than ugly
       ;;rest              ; Emacs as a REST client
       ;;(rust +lsp)       ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       (sh +tree-sitter)                ; she sells {ba,z,fi}sh shells on the C xor
       ;;web               ; the tubes
       (yaml +tree-sitter)              ; JSON, but readable

       :app
       ;;everywhere        ; *leave* Emacs!? You must be joking

       :config
       ;;literate
       (default +bindings +smartparens))
