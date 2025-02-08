;;; module-hugo --- Hugo configurations.
;;
;; Author: jouyouyun <jouyouwen717@gmail.com>

;;; Commentary
;;
;; This file sets up hugo.

;;; Code:

(use-package easy-hugo
  :init
  (setq easy-hugo-previewtime "300")
  (setq easy-hugo-markdown-extension "markdown")
  (setq easy-hugo-bloglist
	    '(
	      ;; blog setting for github pages
          ;; ~/Documents/Hugo
          ((easy-hugo-basedir . wen-hugo-github-repo)
           ;; https://yourid.github.io
	       (easy-hugo-url . wen-hugo-github-url))
	      ;; ;; blog setting for ssh
          ;; ((easy-hugo-basedir . "~/src/github.com/masasam/hugo2/")
	      ;;  (easy-hugo-url . "http://example2.com")
	      ;;  (easy-hugo-sshdomain . "myblogdomain")
	      ;;  (easy-hugo-root . "/home/hugo/"))
	      ;; ;; blog setting for aws s3
	      ;; ((easy-hugo-basedir . "~/src/github.com/masasam/hugo3/")
	      ;;  (easy-hugo-url . "http://example3.net")
	      ;;  (easy-hugo-amazon-s3-bucket-name . "yourS3bucketname"))
	      ;; ;; blog setting for google cloud strage
	      ;; ((easy-hugo-basedir . "~/src/github.com/masasam/hugo4/")
	      ;;  (easy-hugo-url . "http://example4.net")
	      ;;  (easy-hugo-google-cloud-storage-bucket-name . "yourGCPbucketname")
	      ;;  (easy-hugo-image-directory . "img"))	      
	      ;; ;; blog setting for firebase hosting
	      ;; ((easy-hugo-basedir . "~/src/github.com/masasam/firebase/")
	      ;;  (easy-hugo-url . "https://yourproject.firebaseapp.com")))
        ))
  ;; :bind ("C-c C-e" . easy-hugo)
  )

(provide 'module-hugo)

;;; module-hugo.el ends here
