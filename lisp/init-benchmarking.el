;;; init-benchmarking.el --- Measure startup and require times -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun chujin/time-subtract-millis (b a)
  (* 1000.0 (float-time (time-subtract b a))))

(defun chujin/show-init-time ()
  (message "init completed in %.2fms"
           (chujin/time-subtract-millis after-init-time before-init-time)))

(add-hook 'after-init-hook 'chujin/show-init-time)

(provide 'init-benchmarking)
;;; init-benchmarking.el ends here
