(require 'json)

(defvar *ec2-login* nil)
(defvar *ec2-ssh-cmd* nil)
(defvar *ec2-instances* nil)

(setq *ec2-login* "vzaar")
(setq *ec2-ssh-cmd* "vzp ")
(setq *ec2-shell-cmd* "aws ec2 describe-instances")
(setq *ec2-selected-hosts* nil)
(setq *ec2-rcmd-c* "hostname")

;;(setq *ec2-shell-cmd* "cat o2.txt")


(defun ec2-json-find (x l &optional fn)
  (if (vectorp l)
      (let* ((lst (append l nil))
             (v (ec2-json-find x lst fn)))
        (if v v (ec2-json-find x (cdr lst) fn)))

    (if (eq l '())
        nil
      (let ((e (car l)))
        (cond ((listp e)
               (let ((v (ec2-json-find x e fn)))
                 (if v v (ec2-json-find x (cdr l) fn))))
              ((vectorp e)
               (let ((v (ec2-json-find x (append e nil) fn)))
                 (if v v (ec2-json-find x (cdr l) fn))))

              (t (if (eq e x)
                     (if fn
                         (let ((v (funcall fn e l)))
                           (if v v (ec2-json-find x (cdr l) fn)))
                       e)
                   (ec2-json-find x (cdr l) fn))))))))



(defun ec2-json-find-val (x l)
  (ec2-json-find x l (lambda (el lst) (cadr l))))


(ec2-json-find-val :Key '(:Key "Name" :Value "woof"))

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
      (tabulated-list-print))

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
  (let* ((ip (ec2-find-instance-ip
              (ec2-find-instance (tabulated-list-get-id) *ec2-instances*)))
         (buf (concat *ec2-login* "@" ip)))

    (ansi-term (getenv "SHELL") buf)
    (with-current-buffer (concat "*" buf "*")
      (let ((p (get-buffer-process (current-buffer))))
        (goto-char (process-mark p))
        (apply comint-input-sender
               (list p (concat *ec2-ssh-cmd* buf)))))))


(defun ec2/menu-mark ()
  (interactive)
  (let ((ip (ec2-find-instance-ip
             (ec2-find-instance (tabulated-list-get-id) *ec2-instances*))))

    (setq *ec2-selected-hosts* (cons ip *ec2-selected-hosts*))
    (tabulated-list-put-tag "*" t)))


(defun ec2/menu-unmark ()
  (interactive)
  (let ((ip (ec2-find-instance-ip
             (ec2-find-instance (tabulated-list-get-id) *ec2-instances*))))
    (setq *ec2-selected-hosts* (delete ip *ec2-selected-hosts*))
    (tabulated-list-put-tag " " t)))


(defun ec2/menu-info (&optional _num)
  (interactive "p")
  (let* ((id (tabulated-list-get-id))
         (inst (ec2-find-instance id *ec2-instances*))
         (buf (get-buffer-create id)))

    (with-current-buffer buf
      (let ((template
             (concat "name:         %s\n"
                     "public api:   %s\n"
                     "private api:  %s\n"
                     "instace id:   %s\n"
                     "image id:     %s\n"
                     "instace type: %s\n"
                     "launch time:  %s\n"
                     "zone:         %s\n"
                     )))
        (insert
         (format template
                 (ec2-find-instance-name inst)
                 (ec2-json-find-val :PublicIpAddress inst)
                 (ec2-json-find-val :PrivateIpAddress inst)
                 id
                 (ec2-json-find-val :ImageId inst)
                 (ec2-json-find-val :InstanceType inst)
                 (ec2-json-find-val :LaunchTime inst)
                 (ec2-json-find-val :AvailabilityZone inst)))

        (local-set-key (kbd "q") 'kill-this-buffer)))
    (switch-to-buffer buf)))

(defun ec2-find-instance-ip (instance)
  (concat "" (ec2-json-find-val :PublicIpAddress instance)))


(defun ec2-find-instance-name (instance)
  (let ((tags (ec2-json-find-val :Tags instance))
        (res nil))

    (mapc
     (lambda (pair)
       (ec2-json-find :Key
                      pair
                      (lambda (_ l)
                        (if (equal (cadr l) "Name")
                            (setq res (ec2-json-find-val :Value pair))))))
     tags) (if res res "")))



(defun ec2-extract-instances (instances)
  (mapcar
   (lambda (inst)
     (list (ec2-json-find-val :InstanceId inst) inst)) instances))


(defun ec2-build-instances-list ()
  (mapcar
   (lambda (inst)
     (let ((id (car inst))
           (ip (ec2-find-instance-ip inst))
           (name (ec2-find-instance-name inst)))
       (list id (vector ip name)))) *ec2-instances*))



(defun ec2-find-instance (instance-id instances)
  (if (eq nil instances)
      nil
    (if (eq (caar instances) instance-id)
        (cadr (car instances))
      (ec2-find-instance instance-id (cdr instances)))))



(defun ec2/shell ()
  (interactive)
  (let ((buf-name "*ec2-shell-output*")
        (proc-name "ec2-proc")
        (ips (mapconcat 'identity *ec2-selected-hosts* ",")))

    (with-output-to-temp-buffer buf-name
      (apply 'start-process (list proc-name
                                  buf-name
                                  "rcmd" "-H" ips "-c" *ec2-rcmd-c* "-q"))

      (pop-to-buffer buf-name)

      (local-set-key (kbd "k")
                     (lambda ()
                       (interactive)
                       (delete-process proc-name)))

      (local-set-key (kbd "q") 'kill-this-buffer))))


(defun ec2 ()
  (interactive)
  (message "Connecting...")
  (let* ((str (shell-command-to-string *ec2-shell-cmd*))
         (json-object-type 'plist)
         (instances (ec2-json-find-val :Reservations
                                       (json-read-from-string str))))

    (setq *ec2-selected-hosts* nil)
    (setq *ec2-instances* (ec2-extract-instances instances))
    (ec2-create-main-buf (ec2-build-instances-list))))
