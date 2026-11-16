;;; agenda-test.scm --- morg-agenda against real files.
;;;
;;; The buffer text is the plain listing and the card view is a projection
;;; of the same lines, so a test reads the text, the locals and the block
;;; tree. These were agenda_test.exs; a key there is its command here.

(require 'agenda)

(domain! 'testing)
(effects! '(write))

(define t--agd-buf "*Agenda*")
(define t--agd-dir (string-append (compos-home) "/zz-agenda"))

(define (t--agd-pad n) (if (< n 10) (string-append "0" (number->string n)) (number->string n)))

;; local dates, because the agenda reads `date +%F`
(define (t--agd-day off)
  (let ((c (agenda--civil (+ (agenda--today-z) off))))
    (string-append (number->string (car c)) "-" (t--agd-pad (cadr c)) "-" (t--agd-pad (caddr c)))))

(define (t--agd-stamp off . time)
  (string-append "<" (t--agd-day off) (if (pair? time) (string-append " " (car time)) "") ">"))

(define (t--agd-path name) (string-append t--agd-dir "/" name))

(define (t--agd-find pred l)
  (cond ((null? l) #f) ((pred (car l)) (car l)) (else (t--agd-find pred (cdr l)))))

(define (t--agd-clean!)
  (set! *agenda-cache* '())
  (when (buffer-known? t--agd-buf) (buffer-kill! t--agd-buf))
  (shell-command->string (string-append "rm -rf " t--agd-dir)))

;; FILES is ((NAME TEXT) ...). THUNK runs in a fresh frame with the agenda
;; open on those files and current.
(define (t--agd-with files thunk)
  (t--agd-clean!)
  (make-directory! t--agd-dir)
  (for-each (lambda (f) (write-file! (t--agd-path (car f)) (cadr f))) files)
  (with-custom 'morg-agenda-files (list t--agd-dir)
    (lambda ()
      (with-frame
        (lambda (frame)
          (run-command "morg-agenda")
          (check-equal! (current-buffer) t--agd-buf "the agenda is current")
          (thunk)
          (for-each (lambda (f) (let ((p (t--agd-path (car f))))
                                  (when (buffer-known? p) (buffer-kill! p))))
                    files)))))
  (t--agd-clean!))

(define (t--agd-text) (buffer-text t--agd-buf))
(define (t--agd-local key) (buffer-local-value key t--agd-buf))

;; (LINE FILE POS) per entry
(define (t--agd-index) (or (t--agd-local 'agenda-index) '()))

(define (t--agd-walk b)
  (cons b (apply append (map t--agd-walk (or (plist-get b 'children) '())))))

(define (t--agd-blocks-of class)
  (filter (lambda (b) (string-contains? (or (plist-get b 'class) "") class))
          (apply append (map t--agd-walk (or (t--agd-local 'render-blocks) '())))))

(define (t--agd-row-with text)
  (t--agd-find (lambda (r) (string-contains? (value->string (plist-get r 'segs)) text))
              (t--agd-blocks-of "agenda-row")))

(define (t--agd-today) (t--agd-find (lambda (c) #t) (t--agd-blocks-of "agenda-today")))

(define (t--agd-goto-line! n)
  (buffer-goto! t--agd-buf (with-current-buffer t--agd-buf (lambda () (line-start-position n)))))

(define (t--agd-point-line) (car (buffer-line-at-point t--agd-buf)))

;;; --- the scan -------------------------------------------------------------------

(deftest 'a-dated-heading-lands-on-its-day-with-state-tags-time-and-file
  "the text and the card carry the same facts"
  (lambda ()
    (t--agd-with
      (list (list "work.md" (string-append "# TODO Standup with the team " (t--agd-stamp 0 "09:30") " :work:\n"
                                            "notes under it\n"
                                            "## DONE Old thing " (t--agd-stamp 0) "\n")))
      (lambda ()
        (let ((text (t--agd-text)))
          (check-contains! text "Standup with the team" "the title")
          (check-contains! text "09:30" "the time")
          (check-contains! text ":work:" "the tags")
          (check-contains! text "— work.md" "the file"))
        (check-equal! (t--agd-local 'render-root) '(tag "morg-agenda") "the card root")
        (let* ((row (t--agd-row-with "Standup"))
               (segs (map (lambda (s) (list (car s) (cadr s))) (plist-get row 'segs))))
          (check-equal! (plist-get row 'tag) "agenda-entry" "an entry row")
          (check-true! (member '("agenda-time" "09:30") segs) "the time seg")
          (check-true! (member '("agenda-badge agenda-todo" "TODO") segs) "the state seg")
          (check-true! (member '("agenda-tags" ":work:") segs) "the tags seg")
          (check-true! (member '("agenda-file" "work.md") segs) "the file seg"))
        (let ((done (t--agd-row-with "Old thing")))
          (check-contains! (plist-get done 'class) "agenda-row-done" "DONE is struck through")
          (check-false! (string-contains? (value->string (plist-get done 'segs)) "<")
                        "and no timestamp shows in a title"))))))

(deftest 'scheduled-and-deadline-lines-attach-to-the-heading-above
  "the entry points at the heading, not the planning line"
  (lambda ()
    (t--agd-with
      (list (list "proj.md" (string-append "# TODO Ship the release\nDEADLINE: " (t--agd-stamp 2) "\nbody text\n")))
      (lambda ()
        (check-contains! (t--agd-text) "DEADLINE" "the badge")
        (check-contains! (t--agd-text) "Ship the release" "the title")
        (check-equal! (map caddr (t--agd-index)) '(0) "one entry, at the heading")))))

(deftest 'an-overdue-deadline-shows-on-today-as-late-and-done-stays-away
  "a late TODO is today's business; a DONE one is not"
  (lambda ()
    (t--agd-with
      (list (list "late.md" (string-append "# TODO Pay the invoice\nDEADLINE: " (t--agd-stamp -3) "\n"
                                            "# DONE Already paid\nDEADLINE: " (t--agd-stamp -3) "\n")))
      (lambda ()
        (check-contains! (t--agd-text) "DEADLINE 3d late" "late")
        (check-contains! (t--agd-text) "Pay the invoice" "the open one")
        (check-false! (string-contains? (t--agd-text) "Already paid") "not the done one")))))

(deftest 'a-hash-inside-a-code-fence-is-not-a-heading
  "the scan is fence-aware"
  (lambda ()
    (t--agd-with
      (list (list "code.md" (string-append "```sh\n# TODO fake heading " (t--agd-stamp 0) "\n```\n"
                                            "# TODO Real entry " (t--agd-stamp 0) "\n")))
      (lambda ()
        (check-false! (string-contains? (t--agd-text) "fake heading") "the fenced line")
        (check-contains! (t--agd-text) "Real entry" "the real one")))))

;;; --- commands -------------------------------------------------------------------

(deftest 'agenda-next-steps-onto-the-entry-and-visit-opens-its-heading
  "point lands on the entry's line; the visit lands on its heading"
  (lambda ()
    (t--agd-with
      (list (list "work.md" (string-append "some preamble\n# TODO Water the plants " (t--agd-stamp 0) "\n")))
      (lambda ()
        (let ((entry (car (t--agd-index)))
              (path (t--agd-path "work.md")))
          (t--agd-goto-line! 1)
          (run-command "agenda-next")
          (check-equal! (t--agd-point-line) (car entry) "point is on the entry")
          (run-command "agenda-visit")
          (check-equal! (current-buffer) path "the file is current")
          (check-equal! (buffer-point path) (caddr entry) "at the heading")
          (check-equal! (substring-bytes (buffer-text path) (caddr entry) (+ (caddr entry) 6)) "# TODO"
                        "which is the heading"))))))

(deftest 'the-week-commands-move-by-week-and-today-returns
  "next week, back, the week before, and today"
  (lambda ()
    (t--agd-with
      (list (list "weeks.md" (string-append "# TODO Previous " (t--agd-stamp -7) "\n"
                                             "# TODO Current " (t--agd-stamp 0) "\n"
                                             "# TODO Next " (t--agd-stamp 7) "\n")))
      (lambda ()
        (check-contains! (t--agd-text) "Current" "this week")
        (check-false! (string-contains? (t--agd-text) "Previous") "not the last")
        (check-false! (string-contains? (t--agd-text) "Next") "not the next")
        (run-command "agenda-week-next")
        (check-equal! (t--agd-local 'agenda-start-offset) 7 "a week on")
        (check-contains! (t--agd-text) "Next" "next week")
        (check-false! (string-contains? (t--agd-text) "Current") "without this one")
        (run-command "agenda-week-prev")
        (check-equal! (t--agd-local 'agenda-start-offset) 0 "back")
        (check-contains! (t--agd-text) "Current" "this week again")
        (run-command "agenda-week-prev")
        (check-equal! (t--agd-local 'agenda-start-offset) -7 "a week back")
        (check-contains! (t--agd-text) "Previous" "last week")
        (check-false! (string-contains? (t--agd-text) "Current") "without this one")
        (run-command "agenda-today")
        (check-equal! (t--agd-local 'agenda-start-offset) 0 "today")
        (check-contains! (t--agd-text) "Current" "this week")))))

(deftest 'the-selected-week-survives-refresh-and-mode-restore
  "the offset is a local, so a restore keeps it"
  (lambda ()
    (t--agd-with
      (list (list "next.md" (string-append "# TODO Next week " (t--agd-stamp 7) "\n")))
      (lambda ()
        (run-command "agenda-week-next")
        (check-contains! (t--agd-text) "Next week" "shown")
        (run-command "agenda-refresh")
        (check-equal! (t--agd-local 'agenda-start-offset) 7 "refresh keeps the week")
        (check-contains! (t--agd-text) "Next week" "and the entry")
        (buffer-set-local! t--agd-buf 'render-blocks '())
        (with-current-buffer t--agd-buf (lambda () (set-mode! "morg-agenda-mode")))
        (check-equal! (t--agd-local 'agenda-start-offset) 7 "a restore keeps the week")
        (check-contains! (t--agd-text) "Next week" "and the entry")))))

(deftest 'a-previous-week-keeps-overdue-planning-on-its-original-day
  "late is about today; last week shows the day it was due"
  (lambda ()
    (t--agd-with
      (list (list "past.md" (string-append "# TODO Historical deadline\nDEADLINE: " (t--agd-stamp -7) "\n")))
      (lambda ()
        (check-contains! (t--agd-text) "7d late" "late today")
        (run-command "agenda-week-prev")
        (check-contains! (t--agd-text) "Historical deadline" "on its day")
        (check-false! (string-contains? (t--agd-text) "late") "and not late there")))))

(deftest 'toggle-day-folds-the-day-and-the-local-card-and-text-agree
  "the closed days, the card and the hidden range"
  (lambda ()
    (t--agd-with
      (list (list "work.md" (string-append "# TODO One thing " (t--agd-stamp 0) "\n")))
      (lambda ()
        (t--agd-goto-line! (car (car (t--agd-index))))
        (run-command "agenda-toggle-day")
        (check-equal! (length (t--agd-local 'agenda-closed-days)) 1 "one day closed")
        (check-false! (string-contains? (value->string (t--agd-today)) "agenda-row") "the card hides its rows")
        (check-equal! (length (buffer-hidden t--agd-buf)) 1 "the text hides one range")
        (run-command "agenda-toggle-day")
        (check-equal! (t--agd-local 'agenda-closed-days) '() "open again")
        (check-contains! (value->string (t--agd-today)) "agenda-row" "the card shows its rows")
        (check-equal! (buffer-hidden t--agd-buf) '() "the text hides nothing")))))

(deftest 'the-closed-day-survives-a-mode-rerun
  "what a desktop restore does: locals stay, projections rebuild"
  (lambda ()
    (t--agd-with
      (list (list "work.md" (string-append "# TODO One thing " (t--agd-stamp 0) "\n")))
      (lambda ()
        (t--agd-goto-line! (car (car (t--agd-index))))
        (run-command "agenda-toggle-day")
        (let ((closed (t--agd-local 'agenda-closed-days)))
          (buffer-set-local! t--agd-buf 'render-blocks '())
          (with-current-buffer t--agd-buf (lambda () (set-mode! "morg-agenda-mode")))
          (check-equal! (t--agd-local 'agenda-closed-days) closed "the day stays closed")
          (check-false! (string-contains? (value->string (t--agd-today)) "agenda-row")
                        "and its card stays folded"))))))

;;; --- clicks ---------------------------------------------------------------------

(deftest 'a-day-header-click-folds-and-an-entry-click-visits
  "clicks go through the block-click hook, as the client sends them"
  (lambda ()
    (t--agd-with
      (list (list "work.md" (string-append "# TODO Click me " (t--agd-stamp 0) "\n")))
      (lambda ()
        (let* ((entry (car (t--agd-index)))
               (head (t--agd-find (lambda (b) (equal? (plist-get b 'class) "c-fold-head"))
                                 (t--agd-walk (t--agd-today)))))
          (run-hook-with-args-until-success 'block-click t--agd-buf (plist-get head 'click))
          (check-equal! (length (t--agd-local 'agenda-closed-days)) 1 "the header folds")
          (run-hook-with-args-until-success 'block-click t--agd-buf
                                            (string-append "e-" (number->string (car entry))))
          (check-equal! (current-buffer) (t--agd-path "work.md") "the entry visits"))))))

;;; --- views and refresh ----------------------------------------------------------

(deftest 'toggle-view-switches-to-the-plain-listing-and-back
  "the cards are a render mode over the same text"
  (lambda ()
    (t--agd-with
      (list (list "work.md" (string-append "# TODO One thing " (t--agd-stamp 0) "\n")))
      (lambda ()
        (check-equal! (t--agd-local 'render-mode) "blocks" "cards")
        (run-command "agenda-toggle-view")
        (check-false! (t--agd-local 'render-mode) "plain")
        (run-command "agenda-toggle-view")
        (check-equal! (t--agd-local 'render-mode) "blocks" "cards again")))))

(deftest 'refresh-rereads-a-changed-file-past-the-mtime-cache
  "the cache is keyed by mtime; a newer file is read again"
  (lambda ()
    (t--agd-with
      (list (list "work.md" (string-append "# TODO First title " (t--agd-stamp 0) "\n")))
      (lambda ()
        (let ((path (t--agd-path "work.md")))
          (check-contains! (t--agd-text) "First title" "the first read")
          (write-file! path (string-append "# TODO Second title " (t--agd-stamp 0) "\n"))
          ;; mtime has one-second resolution; move it forward so the cache misses
          (shell-command->string (string-append "perl -e '$t=time+5; utime $t,$t,$ARGV[0]' " path))
          (run-command "agenda-refresh")
          (check-contains! (t--agd-text) "Second title" "the new title")
          (check-false! (string-contains? (t--agd-text) "First title") "not the old one"))))))

(deftest 'no-configured-files-shows-the-hint-not-an-empty-week
  "a person who has not set morg-agenda-files learns how"
  (lambda ()
    (t--agd-clean!)
    (with-custom 'morg-agenda-files '()
      (lambda ()
        (with-frame
          (lambda (frame)
            (run-command "morg-agenda")
            (let ((blocks (t--agd-local 'render-blocks)))
              (check-equal! (length blocks) 1 "one block")
              (check-contains! (plist-get (car blocks) 'class) "agenda-empty" "the empty block")
              (check-contains! (plist-get (car blocks) 'text) "morg-agenda-files" "naming the setting"))))))
    (t--agd-clean!)))
