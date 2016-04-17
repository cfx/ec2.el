(require 'json)


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


(defun ec2-find-instance (instance-id instances)
  (if (eq nil instances)
      nil
    (if (eq (caar instances) instance-id)
        (cadr (car instances))
      (ec2-find-instance instance-id (cdr instances)))))




(defun ec2/get-hosts ()
  ;; "aws ec2 describe-instances"
  ;; "cat /Users/cfx/.emacs.d/ec2/o2.txt"

  (let* ((str (shell-command-to-string "aws ec2 describe-instances"))
         (json-object-type 'plist)
         (instances (ec2-json-find-val :Reservations
                                       (json-read-from-string str)))
         (hosts nil))

    (mapcar
     (lambda (inst)
       (list (ec2-json-find-val :PublicIpAddress inst)
             (list (cons "instance-name"
                         (ec2-find-instance-name inst))

                   (cons "private-ip-address"
                         (ec2-json-find-val :PrivateIpAddress inst))

                   (cons "image-id"
                         (ec2-json-find-val :ImageId inst))

                   (cons "instance-type"
                         (ec2-json-find-val :InstanceType inst))

                   (cons "launch-time"
                         (ec2-json-find-val :LaunchTime inst))

                   (cons "availability-zone"
                         (ec2-json-find-val :AvailabilityZone inst))

                   ))) instances)))
