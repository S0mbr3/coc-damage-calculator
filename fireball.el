;;; package -- Summary
;;; Commentary:
;;; -*- lexical-binding: t; -*-
;;; Code:

(defun earthquake-calculator (number damage)
  "Calculate earthquake damage depending of NUMBER DAMAGE."
  (let* (( i 2)
	 (first damage)
	 (total first))
    (while (<= i number)
      (setq total (+ total (/ first (- (* 2 i) 1))))
      (setq i (1+ i)))
    total)
  )
(earthquake-calculator 3 1711.0)
(provide 'fireball)
;;; fireball.el ends here
