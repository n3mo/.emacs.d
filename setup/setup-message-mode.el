;; General settings ----------------------------------------

;; Turn on spell checking when sending messages
(add-hook 'message-mode-hook 'flyspell-mode)


;; Account settings ----------------------------------------

;; Setup the mail address and use name
(setq mail-user-agent 'message-user-agent)
(setq user-mail-address "nvanhorn@capital.edu"
      user-full-name "Nicholas Van Horn")
;; smtp config
;; (setq smtpmail-default-smtp-server "mail.capital.edu"
;;       message-send-mail-function 'message-smtpmail-send-it)
(setq smtpmail-default-smtp-server "smtp.office365.com"
      message-send-mail-function 'message-smtpmail-send-it)

;; Encryption setup
;; (setq send-mail-function    'message-smtpmail-send-it
;;       smtpmail-smtp-server  "mail.capital.edu"
;;       smtpmail-stream-type  'starttls
;;       smtpmail-smtp-service 587)
(setq send-mail-function    'message-smtpmail-send-it
      smtpmail-smtp-server  "smtp.office365.com"
      smtpmail-stream-type  'starttls
      smtpmail-smtp-service 587)

;; Report problems with the smtp server
(setq smtpmail-debug-info t)
;; Add Cc and Bcc headers to the message buffer
(setq message-default-mail-headers "Cc: \nBcc: \n")
;; Postponed message is put in the following draft directory
(setq message-auto-save-directory "~/mail/draft")
(setq message-kill-buffer-on-exit t)
;; Change the directory to store the sent mail
(setq message-directory "~/mail/")

(provide 'setup-message-mode)
