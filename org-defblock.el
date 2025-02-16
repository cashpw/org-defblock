;;; org-defblock.el ---  -*- lexical-binding: t; -*-
;;
;; Created: December 19, 2024
;; Modified: December 19, 2024
;; Version: 0.0.1
;; Keywords: org, blocks, colors, convenience
;; Homepage: https://github.com/cashweaver/org-defblock
;; Package-Requires: ((s "1.13.1") (dash "2.18.1") (emacs "27.1") (org "9.1") (lf "1.0") (seq "2.0"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'cl-lib)
(require 'dash)
(require 'lf)
(require 'org)
(require 'ox-html)
(require 'ox-latex)
(require 's)
(require 'seq)
(require 'subr-x)

(defconst org-defblock-version (package-get-version))

(defun org-defblock-version ()
  "Print the current version of the package in the minibuffer."
  (interactive)
  (message org-defblock-version))

;;;###autoload
(define-minor-mode org-defblock-mode
  "Provide 30 new custom blocks & 34 link types for Org-mode.

All relevant Lisp functions are prefixed ‘org-’; e.g., `org-docs-insert'."
  :lighter
  " OSPE"
  (if org-defblock-mode
      (progn
        ;; https://orgmode.org/manual/Advanced-Export-Configuration.html
        (add-hook
         'org-export-before-parsing-hook 'org--support-special-blocks-with-args)
        (setq org-export-allow-bind-keywords t))
    (remove-hook
     'org-export-before-parsing-hook 'org--support-special-blocks-with-args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; We define a parent keymap that org-deflink keymaps inherit from.
;; We also define a few useful functions that we then bind to this parent map.

(defvar org-defblock-mode-map (make-keymap)
  "A keymap of actions, on link types, that is inherited by all `org-deflink' link keymaps.

To learn about keymap inheritance, run:  C-h i m elisp RETURN m Inheritance and Keymaps RETURN.

This keymap has the following bindings setup:

    (define-key org-defblock-mode-map (kbd \"C-n\") #'org-this-link-next)
    (define-key org-defblock-mode-map (kbd \"C-p\") #'org-this-link-previous)
    (define-key org-defblock-mode-map (kbd \"C-h\") #'org-this-link-show-docs)

The use of `C-n' and `C-p' may be a nuisance to some users, since they override `forward-line'
and `previous-line' when the cursor is on an org-link type. As such, place something like the following
in your initialisation file.

    ;; Use  C-c C-f  to move to the next link of the same link-type as the one under the cursor
    (define-key org-defblock-mode-map (kbd \"C-c C-f\") #'org-this-link-next)

Alternatively, if you don't find much value in these basic bindings, you can remove them all:

    ;; Disable basic org-defblock link keybindings
    (setcdr org-defblock-mode-map nil)

    ;; Or, remove a single binding
    (define-key org-defblock-mode-map (kbd \"C-n\") nil)")

(defvar org-defblock-mode-map--link-keymap-docs nil
  "An alist referencing key bindings for Org links; used in `org-this-link-show-docs'.")

(defun org-link-at-point ()
  "Get the Org link type at point, with suffix colon."
  (interactive)
  (let ((working-line (line-number-at-pos)))
    (save-excursion
      ;; Account for cursour being on anywhere on the links “name:key”.
      (backward-word 2)
      (unless (= working-line (line-number-at-pos))
        (goto-line working-line))
      (let* ((here-to-eol
              (buffer-substring-no-properties (point) (point-at-eol)))
             ;; E.g., “kbd:”, the name part of an Org link
             (link-name (cl-second (s-match "\\([^ ]+:\\).+" here-to-eol))))
        link-name))))

(defun org-this-link-next ()
  "Go to the next Org link that is similar to the link at point."
  (interactive)
  (re-search-forward (org-link-at-point) nil t))
(defun org-this-link-previous ()
  "Go to the previous Org link that is similar to the link at point."
  (interactive)
  (re-search-backward (org-link-at-point) nil t))
(defun org-this-link-show-docs ()
  "Show documentation for the Org link at point in a read-only buffer.

                     Press ‘q’ to kill the resulting buffer and window."
  (interactive)
  (let*
      ((link (s-chop-suffix ":" (org-link-at-point)))
       (msg
        (ignore-errors
          (concat
           (documentation (intern (format "org-link/%s" link)))
           "\nKEY BINDINGS:\n"
           "\nUnless indicated below otherwise..."
           "\n\tC-h: Shows this helpful message buffer"
           "\n\tC-n/C-p on the link to jump to next/previous links of this type;"
           "\n\tC-c C-x C-n/p for moving between arbitrary link types.\n\n"
           (pp-to-string
            (cdr
             (assoc
              link org-defblock-mode-map--link-keymap-docs))))))
       ;; i.e., insist on displaying in a dedicated buffer
       (max-mini-window-height 0))
    (display-message-or-buffer msg)
    (switch-to-buffer-other-window "*Message*")
    (rename-buffer (format "Help: Org Link “%s”" link))
    (read-only-mode)
    (local-set-key "q" #'kill-buffer-and-window)
    (message "Read-only; “q” to kill buffer and window.")))

(define-key org-defblock-mode-map (kbd "C-n") #'org-this-link-next)
(define-key
 org-defblock-mode-map (kbd "C-p") #'org-this-link-previous)
(define-key
 org-defblock-mode-map (kbd "C-h") #'org-this-link-show-docs)


(defvar org--supported-blocks nil
  "Which special blocks, defined with DEFBLOCK, are supported.")

(cl-defmacro
    org-defblock (name kwds &optional link-display docstring &rest body)
  "Declare a new special block, and link, in the style of DEFUN.

A full featured example is at the end of this documentation string.

This is an anaphoric macro that provides export support for
special blocks *and* links named NAME. Just as an Org-mode
src-block consumes as main argument the language for the src
block, our special blocks too consume a MAIN-ARG; it may be a
symbol or a cons-list consisting of a symbolic name (with which
to refer to the main argument in the definition of the block)
followed by a default value, then, optionally, any information
for a one-time setup of the associated link type.

The main arg may be a sequence of symbols separated by spaces,
and a few punctuation with the exception of comma ‘,’ since it is
a special Lisp operator. In doubt, enclose the main arg in
quotes.

Then, just as Org-mode src blocks consume key-value pairs, our
special blocks consume a number of KWDS, which is a list of the
form (key₀ value₀ … keyₙ valueₙ).

After that is an optional DOCSTRING, a familar feature of DEFUN.
The docstring is displayed as part of the tooltip for the
produced link type.

Finally, the BODY is a (sequence of) Lisp forms ---no progn
needed--- that may refer to the names BACKEND and CONTENTS which
refer to the current export backend and the contents of the
special block ---or the description clause of a link.

CONTENTS refers to an Org-mode parsed string; i.e., Org-markup is
acknowledged.

In, hopefully, rare circumstances, one may refer to RAW-CONTENTS
to look at the fully unparsed contents.

Finally, this macro exposes two functions:
+ ORG-EXPORT: Wrap the argument in an export block for the current backend.
+ ORG-PARSE: This should ONLY be called within an ORG-EXPORT call,
             to escape text to Org, and out of the export block.

⇄ We use “@@html:⋯:@@” when altering CONTENTS, but otherwise use raw HTML *around* CONTENTS.
⇄ For example: (format \"<div>%s</div>\" (s-replace \"#+columnbreak:\" \"@@html:<hr>@@\" contents))

----------------------------------------------------------------------

The relationship between links and special blocks:

  [ [type:label][description]]
≈
   #+begin_type label
    description
   #+end_type

----------------------------------------------------------------------

Example declaration, with all possible features shown:

   ;; We can use variable values when defining new blocks
   (setq angry-red '(:foreground \"red\" :weight bold))

   (org-defblock remark
     (editor \"Editor Remark\" :face angry-red) (color \"red\" signoff \"\")
     \"Top level (HTML & LaTeX)O-RESPECT-NEWLINES? editorial remarks; in Emacs they're angry red.\"
     (format (if (equal backend 'html)
               \"<strong style=\\\"color: %s;\\\">⟦%s:  %s%s⟧</strong>\"
               \"{\\color{%s}\\bfseries %s:  %s%s}\")
             color editor contents signoff))

   ;; I don't want to change the definition, but I'd like to have
   ;; the following as personalised defaults for the “remark” block.
   ;; OR, I'd like to set this for links, which do not have argument options.
   (defblock-header-args remark :main-arg \"Jasim Jameson\" :signoff \"( Aim for success! )\")

Three example uses:

    ;; ⟨0⟩ As a special blocks with arguments given.
    #+begin_remark Bobbert Barakallah :signoff \"Thank-you for pointing this out!\" :color green
    I was trying to explain that ${\large (n × (n + 1) \over 2}$ is always an integer.
    #+end_remark

    ;; ⟨1⟩ As a terse link, using default values for the args.
    ;;     Notice that Org-mode formatting is recoqgnised even in links.
    [ [remark:Jasim Jameson][Why are you taking about “$\mathsf{even}$” here?]]

    ;; ⟨2⟩ So terse that no editor name is provided.
    [ [remark:][Please improve your transition sentences.]]

    ;; ⟨★⟩ Unlike 0, examples 1 and 2 will have the default SIGNOFF
    ;; catenated as well as the default red color."
  ;; ⇨ The special block support
  ;;
  (add-to-list 'org--supported-blocks name) ;; global var

  ;; TODO: Relocate
  (defvar org--block--link-display nil
    "Association list of block name symbols to link display vectors.")

  ;; Identify which of the optional features is present...
  (cl-destructuring-bind
      (link-display docstring body)
      (lf-extract-optionals-from-rest
       link-display #'vectorp docstring #'stringp body)
    `(progn
       (when ,(not (null link-display))
         (push (cons (quote ,name) ,link-display) org--block--link-display))
       ,(org--create-defmethod-of-defblock
         name docstring (plist-get kwds :backend) kwds body))))

;; WHERE ...

(cl-defmethod org--create-defmethod-of-defblock
  ((name symbol) docstring backend-type (kwds list) (body list))
  "Helper method to produce an associated Lisp function for org-defblock.

+ NAME: The name of the block type.
+ DOCSTRING, string|null: Documentation of block.
+ KWDS: Keyword-value pairs
+ BODY: Code to be executed"
  (cl-assert (or (stringp docstring) (null docstring)))
  (cl-assert (or (symbolp backend-type) (null backend-type)))

  (let ((main-arg-name (or (cl-first kwds) 'main-arg))
        (main-arg-value (cl-second kwds))
        (kwds (cddr kwds)))
    ;; Unless we've already set the docs for the generic function, don't re-declare it.
    `(if ,(null body)
         (cl-defgeneric ,(intern (format "org-block/%s" name))
             (backend raw-contents &rest _)
           ,docstring)

       (cl-defmethod ,(intern (format "org-block/%s" name))
         ((backend
           ,(if backend-type
                `(eql ,backend-type)
              t))
          (raw-contents string)
          &optional
          ,main-arg-name
          &rest
          _
          &key
          (o-link? nil)
          ,@
          (--reject (keywordp (car it)) (-partition 2 kwds))
          &allow-other-keys)
         ,docstring
         ;; Use default for main argument
         (when (and ',main-arg-name (s-blank-p ,main-arg-name))
           (--if-let (plist-get
                      (cdr (assoc ',name org--header-args))
                      :main-arg)
               (setq ,main-arg-name it)
             (setq ,main-arg-name ,main-arg-value)))

         (cl-letf
             (((symbol-function 'org-export)
               (lambda (x)
                 "Wrap the given X in an export block for the current backend."
                 (if o-link?
                     x
                   (format "#+begin_export %s \n%s\n#+end_export" backend x))))
              ((symbol-function 'org-parse)
               (lambda (x)
                 "This should ONLY be called within an ORG-EXPORT call."
                 (if o-link?
                     x
                   (format "\n#+end_export\n%s\n#+begin_export %s\n"
                           x
                           backend)))))

           ;; Use any headers for this block type, if no local value is passed
           ,@
           (cl-loop
            for k in (mapcar #'car (-partition 2 kwds)) collect
            `(--when-let (plist-get
                          (cdr (assoc ',name org--header-args))
                          ,(intern (format ":%s" k)))
               (when (s-blank-p ,k)
                 (setq ,k it))))

           (org-export
            (let ((contents (org-parse raw-contents)))
              ,@body)))))))

(defun org--pp-list (xs)
  "Given XS as (x₁ x₂ … xₙ), yield the string “x₁ x₂ … xₙ”, no parens.
  When n = 0, yield the empty string “”."
  (s-chop-suffix ")" (s-chop-prefix "(" (format "%s" (or xs "")))))

(defvar org--current-backend nil
  "A message-passing channel updated by
org--support-special-blocks-with-args
and used by DEFBLOCK.")

(defun org--support-special-blocks-with-args (backend)
  "Remove all headlines in the current buffer.
BACKEND is the export back-end being used, as a symbol."
  (setq org--current-backend backend)
  (let
      (blk-start ;; The point at which the user's block begins.
       header-start ;; The point at which the user's block header & args begin.
       kwdargs ;; The actual key-value arguments for the header.
       main-arg ;; The first (non-keyed) value to the block.
       blk-column ;; The column at which the user's block begins.
       body-start ;; The starting line of the user's block.
       blk-contents ;; The actual body string.
       ;; ⟨blk-start/column⟩#+begin_⟨header-start⟩blk main-arg :key₀ val ₀ … :keyₙ valₙ  ;; ⟵ ⟨kwdargs⟩
       ;; ⟨body-start⟩ body
       ;; #+end_blk
       )
    (cl-loop
     for blk in org--supported-blocks do (goto-char (point-min))
     (while (ignore-errors
              (re-search-forward (format "^\s*\\#\\+begin_%s" blk)))
       ;; MA: HACK: Instead of a space, it should be any non-whitespace, optionally;
       ;; otherwise it may accidentlly rewrite blocks with one being a prefix of the other!
       (setq header-start (point))
       ;; Save indentation
       (re-search-backward (format "\\#\\+begin_%s" blk))
       (setq blk-start (point))
       (setq blk-column (current-column))
       ;; actually process body
       (goto-char header-start)
       (setq body-start (1+ (line-end-position)))
       (thread-last
         (buffer-substring-no-properties header-start (line-end-position))
         (format "(%s)")
         read
         (--split-with (not (keywordp it)))
         (setq kwdargs))
       (setq main-arg (org--pp-list (car kwdargs)))
       (setq kwdargs (cadr kwdargs))
       (forward-line -1)
       (re-search-forward (format "^\s*\\#\\+end_%s" blk))
       (setq blk-contents
             (buffer-substring-no-properties
              body-start (line-beginning-position)))
       (kill-region blk-start (point))
       (insert
        (eval
         `(,(intern (format "org-block/%s" blk))
           (quote ,backend)
           ,blk-contents
           ,main-arg
           ,@
           (--map (list 'quote it) kwdargs))))
       ;; See: https://github.com/alhassy/org-special-blocks-extras/issues/8
       ;; (indent-region blk-start (point) blk-column) ;; Actually, this may be needed...
       ;; (indent-line-to blk-column) ;; #+end...
       ;; (goto-char blk-start) (indent-line-to blk-column) ;; #+begin...
       ;; the --map is so that arguments may be passed
       ;; as "this" or just ‘this’ (raw symbols)
       ))))

(defvar org--header-args nil
  "Alist (name plist) where “:main-arg” is a special plist key.

  It serves a similar role to that of Org's src ‘header-args’.

  See doc of SET-BLOCK-HEADER-ARGS for more information.")

(defmacro org-set-block-header-args (blk &rest kvs)
  "Set default valuts for special block arguments.

This is similar to, and inspired by, Org-src block header-args.

Example src use:
    #+PROPERTY: header-args:Language :key value

Example block use:
    (set-block-header-args Block :main-arg mainvalue :key value)

A full, working, example can be seen by “C-h o RET defblock”.
"
  `(add-to-list 'org--header-args (list (quote ,blk) ,@kvs)))

(cl-defmacro
    org--blockcall
    (blk &optional main-arg &rest keyword-args-then-contents)
  "An anaologue to `funcall` but for blocks.

Usage: (blockcall blk-name main-arg even-many:key-values raw-contents)

One should rarely use this directly; instead use
o-thread-blockcall.
"
  `(concat
    "#+end_export\n"
    (,(intern (format "org-block/%s" blk))
     backend ;; defblock internal
                                        ; (format "\n#+begin_export html\n\n%s\n#+end_export\n" ,(car (last keyword-args-then-contents))) ;; contents
     ,@ (last keyword-args-then-contents) ;; contents
     ,main-arg ,@ (-drop-last 1 keyword-args-then-contents))
    "\n#+begin_export"))

(defmacro org-thread-blockcall (body &rest forms)
  "Thread text through a number of blocks.

BODY is likely to be ‘raw-contents’, possibly with user manipulations.

Each FORMS is of the shape “(block-name main-argument
:key-value-pairs)”

(thread-blockcall x)       = x
(thread-blockcall x (f a)) = (blockcall f a x)
(thread-blockcall x f₁ f₂) ≈ (f₂ (f₁ x))

The third is a ‘≈’, and not ‘=’, because the RHS contains
‘blockcall’s as well as massages the export matter
between conseqeuctive blockcalls.

A full example:

    (org-defblock nesting (name nil)
      \"Show text in a box, within details, which contains a box.\"

      (org-thread-blockcall raw-contents
                        (box name)
                        (details (upcase name) :title-color \"green\")
                        (box (format \"⇨ %s ⇦\" name) :background-color \"blue\")
                        ))
"
  (if (not forms)
      body
    `(-let [result
            (org--blockcall ,@ (car forms) ,body)]
       ,@
       (cl-loop
        for b in (cdr forms) collect
        `(setq result
               (org--blockcall
                ,@b (concat "#+begin_export\n" result "\n#+end_export"))))
       result)))

(cl-defmacro org-deflink
    (name &optional docstring display &rest body)
  "Make a new Org-link NAME that exports using form BODY.

Since Org links are essentially string-valued functions,
a function ‘org-link/NAME’ is created.

DOCSTRING is optional; it is visible with
   (documentation 'org-link/NAME)

BODY is a string-valued expression, that may make use of the names
o-label, o-description, o-backend. The final one refers to the export
backend, such as 'html or 'latex. The first two are obtained from uses:

   [[name:o-label][o-description]]

In particular, the use case “name:o-label” means that o-description is nil.

---------------------------------------------------------------------------

Example use:

   ;; In a Lisp buffer, press “C-x C-e” to load this definition
   (org-deflink shout (upcase (or o-description o-label)))

   ;; In an Org-buffer, press “C-c C-e h o” to see how this exports
   <shout: hello world!>

   ;; Or using the bracket format
   [[shout:][hello world!]]
   [[shout: hello world!]]

   ;; or using the plain format
   shout:hello_world

Here is a more complex, involved, example that makes use of
‘:let’ for local declarations. For instance, “define:hello”
renders as the word “hello” with a tooltip defining the word; the
definition is obtained from the command line tool ‘wn’.

  (org-deflink define
    \"Define the given word using WordNet, along with synonyms and coordinate terms.\"
    [:let (definition (shell-command-to-string (format \"wn %s -over -synsn -coorn\" o-label)))
     :help-echo definition]
    (--> definition
      (s-replace-regexp \"\\\\\\\"\" \"''\" it) ;; The presence of ‘\\\"’ in tooltips breaks things, so omit them.
      (s-replace-regexp \"\\n\" \"<br>\" it)
      (format \"<abbr class=\\\"tooltip\\\" title=\\\"%s\\\">%s</abbr>\" it o-label)))

For HTML tooltips, see `org-ospe-html-export-preserving-whitespace'.

More generally, org-special-block-extra's “doc” link type
supports, in order of precedence: User definitions, Emacs Lisp
documentation of functions & variables, and definitions of
English words. For example, “doc:existential_angst” for an entry
‘existential_angst’ whose associated documentation-glossary is
user-defined in a ‘#+documentation’ Org-block, or
“doc:thread-first” for the Emacs Lisp documentation of the
function `thread-first', or “doc:user-mail-address” for the Emacs
Lisp documentation of the variable `user-mail-address', or
“doc:hello” for the definition of the English word ‘hello’.

DISPLAY is a vector consisting of key-value pairs that affects how the link
is displayed in Emacs Org buffers. The keys are as follows.

+ :help-echo is a string-valued expression of the tooltip that should accompany
  the new link in Org buffers. It has access to o-format being one of ‘plain’,
  ‘angle’, ‘bracket’ which indicates the format of the link, as shown above.
  It also has access to o-label and o-description.

  By default, the tooltip is the link name followed by the documentation
  of the link, and, finally, the HTML export of the link.
  That way, upon hover, users can visually see the link contents,
  know what/how the link exports, and actually see the HTML export.

  That is to say, for the ‘shout’ example aboce, the default display is essentially:
  [:help-echo (org-link/shout o-label o-description 'html)]

  You may want to add the following to your Emacs init file:

    ;; Nearly instantaneous display of tooltips.
    (setq tooltip-delay 0)
    ;; Give user 30 seconds before tooltip automatically disappears.
    (setq tooltip-hide-delay 300)

+ :face specifies how should these links be displayed within Emacs.
   It is a list-valued expression.
   As usual, it may make use of O-LABEL (but O-DESCRIPTION has value nil).
   Example:
               :face '(:underline \"green\")

   See https://www.gnu.org/software/emacs/manual/html_node/elisp/Face-Attributes.html

+ [:display 'full] if you do not want bracket links to be
  folded away in Org buffers; i.e., “[[X][Y]]” does not render as just “Y”.

+ :follow is a form that is executed when you click on such links; e.g., to open
   another buffer, browser, or other action. It makes use of (an implicit argument) ‘o-label’.
   Be aware that ‘o-label’ is a string that may contain spaces; e.g., when the action is to open
   a URL in a browser.

   If you are in need of providing similar, related, actions on a single link
   then your :follow can condition on the current prefix argument via ‘o-prefix’
   (which is essentially `current-prefix-arg').
   For instance, a user presses “C-u RET” on your link to do one thing
   but “C-u 72 RET” to do another action.

+ :keymap is an alternating list of keys and actions to be
  performed when those keys are pressed while point is on the link.
  For example:
      [:keymap (C-h (message-box \"hola\"))]

  By default, C-n and C-p are for moving to next and previous occruances of the same link type.

+ :let is a list of alternating variable symbol name and value, which are then used to form
  a concrete `let*' clause. This is useful for introducing local variables for use in the DISPLAY
  as well as in the CONTENTS. Such local declarations may make use of O-LABEL and O-DESCRIPTION, as usual."
  (cl-destructuring-bind (docstring display body)
      (lf-extract-optionals-from-rest docstring #'stringp
                                      display   #'vectorp
                                      body)
    (setq display (seq--into-list display))
    (let ((org-link/NAME (intern (format "org-link/%s" name)))
          (navigation "Press “C-h” to see possible actions on this link type.")
          (lets (cl-loop for (variable value)
                         on (cl-getf display :let)
                         by #'cddr
                         collect (list variable value))))
      `(progn
         ;; Declare the underlying function and documentation
         (cl-defun ,org-link/NAME ;; function name
             (o-label o-description o-backend)         ;; function args
           ;; new function documentation
           ,docstring
           ;; function body
           (let* ,lets ,@body))
         ;; Construct the Org-link
         (org-link-set-parameters
          ,(format "%s" name)
          :export (quote ,org-link/NAME)
          ;; How should these links be displayed?
          ;; (We augment the namespace with the missing o-description that local variables may be using.)
          :face (lambda (o-label)  (let (o-description) (let* ,lets ,(cl-getf display :face))))
          ;; When you click on such links, what should happen?
          ;; (We augment the namespace with the missing o-description that local variables may be using.)
          :follow (lambda (o-label o-prefix) (let (o-description) (let* ,lets ,(cl-getf display :follow))))
          ;; These links should *never* be folded in descriptive display;
          ;; i.e., “[[example:lable][description]]” will always appear verbatim
          ;; and not hide the first pair […].
          :display (cl-the symbol ,(cl-getf display :display)) ;; e.g.,: 'full
          ;; Any special keybindings when cursour is on this link type?
          ;; On ‘NAME:’ links, C-n/p to go to the next/previous such links.
          :keymap (let ((o-keymap (copy-keymap org-mouse-map))
                        (pattern (format "%s:" (quote ,name))))

                    ;; If this Org-link has additional key bindings, then save
                    ;; them in an alist for reference in `org-this-link-show-docs'.
                    (when (quote ,(cl-getf display :keymap))
                      (push (cons (format "%s" (quote ,name)) (quote ,(cl-getf display :keymap)))
                            org-defblock-mode-map--link-keymap-docs))

                    ;; Let's inherit some possibly useful key bindings.
                    (set-keymap-parent o-keymap org-defblock-mode-map)

                    ;; Populate the keymap
                    (cl-loop for (key action) on (quote ,(cl-getf display :keymap))
                             by #'cddr
                             do (define-key o-keymap (kbd (format "%s" key))
                                            `(lambda () (interactive) ,action)))
                    ;; Return the keymap
                    o-keymap)
          ;; The tooltip alongside a link
          :help-echo (lambda (window object position)
                       (save-excursion
                         (goto-char position)
                         (-let* (((&plist :path :format :contents-begin :contents-end)
                                  (cadr (org-element-context)))
                                 (org-format format)
                                 (o-label path)
                                 (o-description
                                  (when (equal format 'bracket)
                                    (copy-region-as-kill contents-begin contents-end)
                                    (substring-no-properties (car kill-ring)))))
                           (or (let* ,lets ,(cl-getf display :help-echo))
                               (format "%s:%s\n\n%s\nHTML Export:\n\n%s"
                                       (quote ,name)
                                       (or o-description o-label)
                                       ,(concat (or docstring "") "\n\n" navigation "\n")
                                       (,org-link/NAME o-label o-description 'html)))))))
         ;; Return value is the name of the underlying function.
         ;; We do this to be consistent with `defun'.
         (quote ,org-link/NAME)))))

(provide 'org-defblock)
;;; org-defblock.el ends here
