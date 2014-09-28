;; environment variables that are used throughout the configuration.
(setq
 is-windows (eq system-type 'windows-nt)
 is-mac (eq system-type 'darwin))

;; user-emacs-directory wasn't defined until later
;; versions. We'll try to fix it up here by defaulting
;; to ${HOME}/.emacs.d
(if (not (boundp 'user-emacs-directory))
    (setq user-emacs-directory
          (concat (getenv "HOME") "/.emacs.d/")))

(cd (getenv "HOME"))

;; send my backups here
(add-to-list 'backup-directory-alist
             (cons "" (concat user-emacs-directory "backups/")))

;; setup load path
(add-to-list 'load-path (concat user-emacs-directory "elisp/"))
(let ((default-directory (concat user-emacs-directory "elisp/")))
  (normal-top-level-add-subdirs-to-load-path))

(defun load-init-el (filename)
  (load (concat user-emacs-directory filename)))

;; use package management when in emacs >= 24. Is this a good idea? I don't know.
(when (>= emacs-major-version 24)
  (load-init-el "packages.el"))

(load-init-el "funcs.el")
(load-init-el "hooks.el")
(load-init-el "keys.el")
(load-init-el "local-prefs.el")

(load-init-el "erc.el")


(cond
 (is-windows
  (setq-default ispell-program-name "c:/Program Files (x86)/Aspell/bin/aspell.exe")

  (if (file-exists-p "c:/Cygwin/bin")
      (setq cygwin-mount-cygwin-bin-directory "c:/Cygwin/bin")
    (setq explicit-shell-file-name "c:/Cygwin/bin/bash")
    (require 'cygwin-mount)
    (require 'setup-cygwin)))

 (is-mac
  (setenv "PATH" (concat "/usr/local/bin" path-separator (getenv "PATH")))
  (add-to-list 'exec-path "/usr/local/bin")

  (setq-default ispell-program-name
                (first-existing-file '("/opt/local/bin/ispell" "/usr/local/bin/ispell")))

  ;; Work around a bug (?) on OS X where system-name is FQDN

  (setq system-name (car (split-string system-name "\\."))
        explicit-shell-file-name "/bin/bash"
        latex-run-command "/usr/texbin/latex"

        ;; I despise Mountain Lion's Full-Screen mode
        ns-use-native-fullscreen nil)))

;; auto load list for the modes.
(setq auto-mode-alist
      (append
       '(("\\.html\\'" . nxml-mode)
         ("\\.jj\\'" . javacc-mode))
       auto-mode-alist))

;; Required packages
(require 'cl)
(require 'column-marker)
(require 'faces)
(require 'fill-column-indicator)
(require 'font-lock)
(require 'ido)
(require 'flx-ido)
(require 'inf-haskell)
(require 'javacc-mode)
(require 'javadoc-lookup)
(require 'linum)
(require 'magit)
(require 'projectile)
(require 'rails-autoload)
(require 'saveplace)
(require 'tls)
(require 'tramp)
(require 'uniquify)
(require 'whattf-dt)
(require 'yasnippet)

;; semantic is part of the cedet suite of tools prior to emacs 23.2, this was a
;; separate package. I don't want to bring all of CEDET into the repo now due to
;; the setup process.
(when (require 'semantic/ia nil 'noerror)
  (require 'semantic/bovine/gcc)

  (setq semantic-default-submodes
        (list 'global-semanticdb-minor-mode
              'global-semantic-highlight-func-mode
              'global-semantic-stickyfunc-mode
              'global-semantic-decoration-mode
              'global-semantic-idle-local-symbol-highlight-mode
              'global-semantic-idle-scheduler-mode
              'global-semantic-idle-completions-mode
              'global-semantic-idle-summary-mode
              'global-ede-mode))

  (semanticdb-enable-gnu-global-databases 'c-mode t)
  (semanticdb-enable-gnu-global-databases 'c++-mode t)
  (semantic-mode 1))

;; setup relaxNG to know where html5 schemas are.
(eval-after-load "rng-loc"
  '(progn
     (add-to-list 'rng-schema-locating-files
                  (concat user-emacs-directory "elisp/html5-el/schemas.xml"))))

(delete ".svn/" completion-ignored-extensions)
(delete ".hg/" completion-ignored-extensions)
(delete ".git/" completion-ignored-extensions)

;; this prevents IDO from hiding IRC log files
(delete "\\`#" ido-ignore-files)

(set-face-attribute 'default nil
                    :family "Anonymous Pro" :weight 'normal :width 'normal :height 200)

(setq
 android-mode-sdk-dir (concat (getenv "HOME") "/Development/android-sdk-macosx")

 ;; some bibtex settings that I like
 bibtex-entry-format '(opts-or-alts realign last-comma delimiters page-dashes)
 bibtex-autokey-year-length 4
 bibtex-autokey-year-title-separator ""
 bibtex-autokey-titleword-length nil

 default-frame-alist (append
                      '((width . 110)
                        (height . 40)
                        (left . 200)
                        (top . 50))
                      default-frame-alist)
 ;;fci-handle-truncate-lines nil
 font-lock-maximum-decoration t
 frame-title-format '(buffer-file-name
                      "%f"
                      (dired-directory dired-directory "%b"))
 ido-enable-flex-matching t
 ido-use-faces nil
 inhibit-startup-screen t
 linum-format "%d "
 matlab-indent-function t
 next-line-add-newlines nil
 save-place-file (concat user-emacs-directory "places")
 show-paren-style 'parenthesis
 standard-indent 4
 tex-dvi-view-command "xdvi"
 tramp-default-method "sshx"
 transient-mark-mode t
 truncate-partial-width-windows nil
 uniquify-buffer-name-style 'post-forward-angle-brackets
 user-full-name "Mitchell Peabody"
 visible-bell t
 whitespace-line-column 80
 whitespace-style '(face trailing)
 x-select-enable-clipboard t)

;; set defaults for buffer local variables
(setq-default
 buffer-file-coding-system 'undecided-unix
 c-basic-offset 4
 fci-rule-column 120
 fill-column 80
 highlight-changes-mode t
 indicate-empty-lines t
 indent-tabs-mode nil
 save-place t
 savehist-mode t
 truncate-lines t
 tab-width 4)

;; coding system
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;;
(global-font-lock-mode 1)
(global-linum-mode t)

(column-number-mode t)
(line-number-mode t)

;; line highlighting
(toggle-hl-line-when-idle t)
(global-hl-line-mode -1)

(global-semantic-highlight-func-mode t)
(global-semantic-highlight-edits-mode t)
(global-semantic-stickyfunc-mode -1)
(global-whitespace-mode t)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)

(show-paren-mode 1)

(tool-bar-mode -1)
(turn-on-font-lock)

;; set up YASnippet
(add-to-list 'yas/snippet-dirs (concat user-emacs-directory "snippets"))
(setq yas/also-auto-indent-first-line t)
(yas/global-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("f0ea6118d1414b24c2e4babdc8e252707727e7b4ff2e791129f240a2b3093e32" default)))
 '(newsticker-url-list
   (quote
    (("Groklaw" "http://www.groklaw.net/backend/GrokLaw.rdf" nil nil nil)
     ("The Baseline Scenario" "http://feeds2.feedburner.com/BaselineScenario" nil nil nil)
     ("Joe. My. God." "http://joemygod.blogspot.com/feeds/posts/default" nil nil nil)
     ("The Daily Dish | By Andrew Sullivan" "http://feeds.feedburner.com/andrewsullivan/rApM" nil nil nil)
     ("FiveThirtyEight" "http://fivethirtyeight.blogs.nytimes.com/feed/" nil nil nil)
     ("Groklaw NewsPicks" "http://www.groklaw.net/backend/GLNewsPicks.rdf" nil nil nil)
     ("Ezra Klein" "http://feeds.voices.washingtonpost.com/wp/ezra-klein/index" nil nil nil)
     ("NYT > Opinion" "http://www.nytimes.com/services/xml/rss/nyt/Opinion.xml" nil nil nil)
     ("Danger Room" "http://www.wired.com/dangerroom/feed/" nil nil nil)
     ("The Best Defense" "http://ricks.foreignpolicy.com/node/feed" nil nil nil)
     ("The Tech - MIT's Student Newspaper" "http://tech.mit.edu/rss/tech.xml" nil nil nil)
     ("TED Blog" "http://tedblog.typepad.com/tedblog/atom.xml" nil nil nil)
     ("Google Operating System" "http://feeds.feedburner.com/GoogleOperatingSystem" nil nil nil)
     ("Signal vs. Noise" "http://feeds.feedburner.com/37signals/beMH" nil nil nil)
     ("All Things Distributed" "http://www.allthingsdistributed.com/atom.xml" nil nil nil)
     ("Perspectives" "http://perspectives.mvdirona.com/SyndicationService.asmx/GetRss" nil nil nil)
     ("Benno's Blog" "http://benno.id.au/blog/feed/" nil nil nil)
     ("Building Feedly" "http://blog.feedly.com/feed/" nil nil nil)
     ("The Official Google Blog" "http://googleblog.blogspot.com/feeds/posts/default" nil nil nil)
     ("Washington Post: Breaking News, Wor..." "http://www.washingtonpost.com/rss/homepage" nil nil nil)
     ("NYT > Asia Pacific" "http://www.nytimes.com/services/xml/rss/nyt/AsiaPacific.xml" nil nil nil)
     ("The New York Times" "http://www.nytimes.com/services/xml/rss/nyt/HomePage.xml" nil nil nil)
     ("Entrepreneur" "http://feeds.feedburner.com/entrepreneur/latest" nil nil nil)
     ("VentureBeat" "http://feeds.feedburner.com/venturebeat" nil nil nil)
     ("The Billfold" "http://feeds.feedburner.com/TheBillfold" nil nil nil)
     ("HBR.org" "http://feeds.harvardbusiness.org/harvardbusiness?format=xml" nil nil nil)
     ("OnStartups" "http://feed.onstartups.com/onstartups" nil nil nil)
     ("YoungEntrepreneur.com" "http://feeds.feedburner.com/YoungentrepreneurcomBlog" nil nil nil)
     ("Fast Company" "http://www.fastcompany.com/rss.xml" nil nil nil)
     ("Planet Money" "http://www.npr.org/rss/rss.php?id=93559255" nil nil nil)
     ("Boing Boing" "http://feeds.boingboing.net/boingboing/iBag" nil nil nil)
     ("segfault zen: core dumps" "http://www.segfaultzen.com/blog/feed/" nil nil nil)
     ("FluentU Chinese" "http://www.fluentflix.com/blog/feed/" nil nil nil)
     ("Chinese Hacks" "http://chinesehacks.com/feed/" nil nil nil)
     ("Beginner | Chinese Reading Practice" "http://chinesereadingpractice.com/category/beginner/feed/" nil nil nil)
     ("Fluent in 3 months - Language Hacking and Travel Tips" "http://feeds.feedburner.com/fluentin3months" nil nil nil)
     ("ChinaHush" "http://www.chinahush.com/feed/" nil nil nil)
     ("Zen Habits" "http://zenhabits.net/feed/" nil nil nil)
     ("PsyBlog" "http://www.spring.org.uk/feed" nil nil nil)
     ("Presentation Zen" "http://www.presentationzen.com/presentationzen/atom.xml" nil nil nil)
     ("Write to Done" "http://writetodone.com/feed/" nil nil nil)
     ("Office Buddha" "http://www.officebuddha.org/feeds/posts/default" nil nil nil)
     ("Farnam Street" "http://www.farnamstreetblog.com/feed/" nil nil nil)
     ("Programming in the 21st Century" "http://prog21.dadgum.com/atom.xml" nil nil nil)
     ("10x Software Development" "http://blogs.construx.com/blogs/stevemcc/rss.aspx" nil nil nil)
     ("The Perils of Parallel" "http://perilsofparallel.blogspot.com/feeds/posts/default" nil nil nil)
     ("DevOps.com" "http://devops.com/feed/" nil nil nil)
     ("ongoing by Tim Bray" "http://www.tbray.org/ongoing/ongoing.atom" nil nil nil)
     ("LeadingAnswers: Leadership and Agile Project Management Blog" "http://leadinganswers.typepad.com/leading_answers/atom.xml" nil nil nil)
     ("The History of Python" "http://python-history.blogspot.com/feeds/posts/default" nil nil nil)
     ("#AltDevBlogADay" "http://www.altdevblogaday.com/feed/" nil nil nil)
     ("Google Developers Blog" "http://code.google.com/feeds/updates.xml" nil nil nil)
     ("Devlicio.us - Just the Tasty Bits" "http://devlicio.us/blogs/MainFeed.aspx" nil nil nil)
     ("Pawel Brodzinski on Software Project Management" "http://feeds.feedburner.com/SoftwareProjectManagement" nil nil nil)
     ("The Daily WTF" "http://syndication.thedailywtf.com/TheDailyWtf" nil nil nil)
     ("All About Agile | Agile Development Made Easy!" "http://feeds.feedburner.com/allaboutagile?format=xml" nil nil nil)
     ("Joel on Software" "http://www.joelonsoftware.com/rss.xml" nil nil nil)
     ("Destroy All Software Screencasts" "https://www.destroyallsoftware.com/screencasts/feed" nil nil nil)
     ("Martin Fowler" "http://martinfowler.com/bliki/bliki.atom" nil nil nil)
     ("Dr. Dobb's Articles" "http://www.drdobbs.com/articles/rss" nil nil nil)
     ("Coding Horror" "http://feeds.feedburner.com/codinghorror/" nil nil nil)
     ("Agile Pain Relief" "http://agilepainrelief.com/feed" nil nil nil)
     ("Performance Zone" "http://www.dzone.com/mz/110215/rss" nil nil nil)
     ("On the Mark...The IT Market in Austin, Texas" "http://markcunningham91.blogspot.com/feeds/posts/default" nil nil nil)
     ("Coding Wisdom" "http://www.codingwisdom.com/codingwisdom/atom.xml" nil nil nil)
     ("The Codeless Code" "http://thecodelesscode.com/rss" nil nil nil)
     ("CodeBetter.Com" "http://codebetter.com/blogs/MainFeed.aspx" nil nil nil)
     ("High Scalability" "http://feeds.feedburner.com/HighScalability" nil nil nil)
     ("Lambda the Ultimate - Programming Languages Weblog" "http://lambda-the-ultimate.org/rss.xml" nil nil nil)
     ("Rands In Repose" "http://www.randsinrepose.com/index.xml" nil nil nil)
     ("The Endeavour" "http://www.johndcook.com/blog/feed/" nil nil nil)
     ("Android Developers Blog" "http://feedproxy.google.com/blogspot/hsDu" nil nil nil)
     ("Null Byte « WonderHowTo" "http://null-byte.wonderhowto.com/rss.xml" nil nil nil)
     ("Information Is Beautiful" "http://feeds.feedburner.com/InformationIsBeautiful" nil nil nil)
     ("BetterExplained" "http://feeds.feedburner.com/Betterexplained" nil nil nil)
     ("NatureNews - Most recent articles - nature.com science feeds" "http://www.nature.com/news/rss.rdf" nil nil nil)
     ("You Are Not So Smart" "http://youarenotsosmart.com/feed/" nil nil nil)
     ("PHD Comics" "http://www.phdcomics.com/gradfeed.php" nil nil nil)
     ("From PhD to Life" "http://fromphdtolife.com/feed/" nil nil nil)
     ("Malaphors" "http://malaphors.com/feed/" nil nil nil)
     ("Dilbert Daily Strip" "http://feed.dilbert.com/dilbert/daily_strip" nil nil nil)
     ("The Oatmeal - Comics, Quizzes, & Stories" "http://theoatmeal.com/feed/rss" nil nil nil)
     ("Saturday Morning Breakfast Cereal (updated daily)" "http://www.smbc-comics.com/rss.php" nil nil nil)
     ("Fredo and Pidjin. The Webcomic. » Fredo & Pid’Jin" "http://feeds.feedburner.com/Pidjin" nil nil nil)
     ("PostSecret" "http://postsecret.blogspot.com/feeds/posts/default" nil nil nil)
     ("TEDTalks (video)" "http://feeds.feedburner.com/tedtalks_video" nil nil nil)
     ("ZEN PENCILS" "http://feeds.feedburner.com/zenpencils" nil nil nil)
     ("The Duffel Blog" "http://feeds.feedburner.com/duffelblog" nil nil nil)
     ("The Stranger, Seattle's Only Newspaper: Savage Love" "http://www.thestranger.com/seattle/Rss.xml?category=oid%3A258" nil nil nil)
     ("xkcd.com" "http://www.xkcd.com/rss.xml" nil nil nil)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(when (display-graphic-p)
  (load-theme 'afternoon))
