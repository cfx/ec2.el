(require 'cl)
(load "~/.emacs.d/ec2/parser.el")

(defvar *ec2-login* nil)
(defvar *ec2-ssh-cmd* nil)
(defvar *ec2-instances* nil)

(setq *ec2-login* "vzaar")
(setq *ec2-ssh-cmd* "vzp ")
(setq *ec2-rcmd-c* "hostname")

(define-derived-mode ec2-menu-mode tabulated-list-mode "ec2 Menu"
  (setq tabulated-list-format
        `[("public ip" 16 nil)
          ("name" 10 nil)])
  (setq tabulated-list-padding 2))

(defun ec2-create-main-buf (instances)
  (interactive "P")
  (let ((buf (get-buffer-create "ec2")))
    (with-current-buffer buf
      (ec2-menu-mode)
      (setq tabulated-list-entries instances)
      (tabulated-list-init-header)
      (tabulated-list-print)
      (local-set-key (kbd "q") 'kill-this-buffer))

    (switch-to-buffer buf)))

(defvar ec2-menu-mode-map nil)
(setq ec2-menu-mode-map
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map tabulated-list-mode-map)
        (define-key map "s" 'ec2/menu-ssh)
        (define-key map "m" 'ec2/menu-mark)
        (define-key map "u" 'ec2/menu-unmark)
        (define-key map "i" 'ec2/menu-info)
        (define-key map "x" 'ec2/shell)
        map))


(defun ec2/menu-ssh (&optional _num)
  (interactive "p")
  (let* ((ip (tabulated-list-get-id))
         (buf (concat *ec2-login* "@" ip)))

    (ansi-term (getenv "SHELL") buf)
    (with-current-buffer (concat "*" buf "*")
      (let ((p (get-buffer-process (current-buffer))))
        (goto-char (process-mark p))
        (apply comint-input-sender
               (list p (concat *ec2-ssh-cmd* buf)))))))


(defun ec2/menu-mark ()
  (interactive)
  (let ((ip (tabulated-list-get-id)))
    (setq *ec2-selected-hosts* (cons ip *ec2-selected-hosts*))
    (tabulated-list-put-tag "*" t)))


(defun ec2/menu-unmark ()
  (interactive)
  (let ((ip (tabulated-list-get-id)))
    (setq *ec2-selected-hosts* (delete ip *ec2-selected-hosts*))
    (tabulated-list-put-tag " " t)))


(defun ec2/menu-info (&optional _num)
  (interactive "p")
  (let* ((ip (tabulated-list-get-id))
         (host-props (car (ec2--find-val ip *ec2-instances*)))
         (buf (get-buffer-create ip)))

    (with-current-buffer buf
      (insert (format "public-ip: %s\n" ip)
              (ec2---host-props-str host-props))
      (local-set-key (kbd "q") 'kill-this-buffer)
      (read-only-mode t))
    (switch-to-buffer buf)))


(defun ec2---host-props-str (host-props)
  (mapconcat (lambda (e)
               (format "%s: %s\n" (car e) (cdr e)))
             host-props ""))

(defun ec2--build-host (ip host)
  (list ip (vector ip (ec2--find-val "instance-name" (cadr host)))))

(defun ec2-build-instances-list ()
  (let ((running-hosts nil))

    (dolist (host *ec2-instances*)
      (let ((ip (car host)))
        (if ip
            (setq running-hosts
                  (cons (ec2--build-host ip host)running-hosts)))))

    running-hosts))



(defun ec2--find-val (k l)
  (cond ((eq l nil) nil)
        ((equal k (caar l)) (cdar l))
        (t (ec2--find-val k (cdr l)))))


(defun ec2/shell ()
  (interactive)
  (lexical-let ((buf-name "*ec2-shell-output*")
        (proc-name "ec2-proc")
        (ips (mapconcat 'identity *ec2-selected-hosts* ",")))

    (if (equal ips "")
        (message "No host(s) selected")

      (with-output-to-temp-buffer buf-name
        (apply 'start-process (list proc-name
                                    buf-name
                                    "rcmd" "-H" ips "-c" *ec2-rcmd-c* "-q"))

        (pop-to-buffer buf-name)
        (local-set-key (kbd "k")
                       (lambda ()
                         (interactive)
                         (delete-process proc-name)))

        (local-set-key (kbd "q") 'kill-this-buffer)))))



(defun ec2 ()
  (interactive)
  (setq *ec2-selected-hosts* nil)

  (message "Connecting...")
  (setq *ec2-instances* (ec2/get-hosts))
  (ec2-create-main-buf (ec2-build-instances-list)))
