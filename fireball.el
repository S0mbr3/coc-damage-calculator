;;; package -- Summary
;;; Commentary:
;;; -*- lexical-binding: t; -*-
;;; Code:

(defun earthquake-level-picker (level)
  "Pick the good percentage for the earthquake LEVEL."
  (let ((levels '(14.5 17 21 25 29)))
    (elt levels (1- level))))

(defun earthquake-damage-calculator (level number hp)
  "Calculate earthquake damage depending of NUMBER LEVEL and HP."
  (interactive" nPress the level of the earthquake: \nnPress the number of earthquakes: \nnPress the hp of the building")
  (let* (( i 2)
	 (damage
	  (/ (* hp (earthquake-level-picker level)) 100) )
	 (first damage)
	 (total first))
    (while (<= i number)
      (setq total (+ total (/ first (- (* 2 i) 1))))
      (setq i (1+ i)))
    total))

(defun fireball-level-picker (level)
  "Pick the good damage for the fireball LEVEL."
  (let ((levels '(1500 1500 1700 1700 1800 1950 1950 2050 2200 2200 2350 2650 2650 2750 3100 3100 3250 3400 3400 3500 3650 3650 3750 3900 3900 3950 4100))) (elt levels (1- level))))


(defun fireball-hp-building-left (level hp)
  "Calculate fireball damage by it's LEVEL building HP."
  (interactive "nPress the level of the fireball: \nnPress the hp of the building: ")
  (- hp (fireball-level-picker level)))

(defun fireball-and-earthquake-calculator (fireball-level earthquake-level earthquake-number hp)
  "Calculate damage FIREBALL-LEVEL EARTHQUAKE-LEVEL EARTHQUAKE-NUMBER HP."
  (interactive "nPress the level of the fireball: \nnPress the level of the earthquake: \nnPress the number of earthquakes: \nnPress the hp of the building: ")
  (print (- hp (fireball-level-picker fireball-level ) (earthquake-damage-calculator earthquake-level earthquake-number hp))))

(provide 'fireball)
;;; fireball.el ends here
