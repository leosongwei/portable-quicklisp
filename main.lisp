
(defun map-tree (f tree)
  (if (atom tree)
      (funcall f tree)
      (mapcar (lambda (e)
                (map-tree f e))
              tree)))

(defun make-str ()
  (make-array '(0) :element-type 'base-char
              :fill-pointer 0 :adjustable t))

(defun cut-path (pathname)
  (let ((lst nil)
        (path-string (namestring pathname)))
    (let ((seg (make-str)))
      (dotimes (i (length path-string))
        (let ((c (aref path-string i)))
          (with-output-to-string (s seg)
            (if (equal c #\/ )
                (progn (push seg lst)
                       (setf seg (make-str)))
                (princ c s)))))
      (if (< 0 (length seg)) (push seg lst)))
    (reverse lst)))
;;(cut-path #p"/home/leo")

(defstruct dir name d content)

(defun merge-path (path-lst)
  (let* ((path-str-lst (mapcar #'namestring path-lst))
         (path-stack-lst
          (mapcar (lambda (e)
                    (cons (cut-path e)
                          e))
                  path-str-lst))
         (tree (make-dir :name "")))
    (labels
        ((hang-f (tree stack d) ;; hang-F
           (if (string= (dir-name tree) (car stack))
               (if (null (cdr stack))
                   (setf (dir-d tree) d) ;; reach
                   (let (find?)          ;; not reach
                     (dolist (tree (dir-content tree))
                       (if (string= (dir-name tree) (cadr stack))
                           (progn
                             (setf find? t)
                             (hang-f tree (cdr stack) d)
                             (return))
                           nil))
                     (if (not find?)
                         (let ((new-tree (make-dir :name (cadr stack))))
                           (push new-tree (dir-content tree))
                           (hang-f new-tree (cdr stack) d))))))))
      (mapcar (lambda (e)
                (hang-f tree (car e) (cdr e)))
              path-stack-lst))
    (let (result)
      (labels ((pop-tree-f (tree)
                 (if (dir-d tree)
                     (push (dir-d tree) result)
                     (mapcar #'pop-tree-f (dir-content tree)))))
        (pop-tree-f tree)
        result))))
;; (merge-path '(#p"/home/leo/game/cs/"
;;               #p"/home/ftp/"
;;               #p"/home/stalin/"
;;               #p"/home/leo/game/"
;;               #p"/home/leo/"))

(defun depend-dir-list (system)
  (let* ((tree (ql::dependency-tree system))
         (dir-tree (map-tree (lambda (e)
                               (ql::where-is-system
                                (ql::short-description e)))
                             tree))
         (dir-list nil))
    (labels ((flatten-tree (tree)
               (if (atom tree)
                   (push tree dir-list)
                   (mapcar #'flatten-tree tree))))
      (flatten-tree dir-tree))
    (mapcar #'namestring dir-list)))

(defun depend-dir-list-systems (system-list)
  (merge-path
   (apply #'append (mapcar #'depend-dir-list system-list))))
;; (depend-dir-list-systems '(cl-markdown cl-ncurses cl-ppcre))

(defun make-depend-dir ()
  #+sbcl
  (sb-ext:run-program "/bin/mkdir" '("depend")
                      :output *standard-output*)
  #- (or sbcl)
  (error "make-depend-dir: Don't know how to mkdir!"))

(defun copy-depend (dir-list)
  #+sbcl
  (sb-ext:run-program "/bin/cp" `("-v" "-r" ,@dir-list "./depend")
                      :output *standard-output*))


