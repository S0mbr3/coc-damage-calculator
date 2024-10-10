;;; coc-damage-calculator.el -- A Clash of Clans damage calculator -*- lexical-binding: t; -*-

;; Copyright (C) 2024  Benjamin Denis

;; Author: Benjamin Denis <benjamin_denis@outlook.com>
;; Keywords: coc damage calculator
;; Homepage: https://github.com/S0mbr3/coc-damage-calculator
;; Version: 0.1
;; Package-Requires: ((emacs "27.2") (hydra "0.14.0")

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package help finding good combinations of spells, equipements to deal damage to buildings in Clash of Clans.

;;; Code:

;;;###autoload
(defun earthquake-level-picker (level)
  "Pick the good percentage for the earthquake LEVEL."
  (let ((levels '(14.5 17 21 25 29)))
    (elt levels (1- level))))

;;;###autoload
(defun earthquake-damage-calculator (level number hp)
  "Calculate earthquake damage depending of NUMBER LEVEL and HP."
  (let* (( i 2)
	 (damage
	  (/ (* hp (earthquake-level-picker level)) 100) )
	 (first damage)
	 (total first))
    (while (<= i number)
      (setq total (+ total (/ first (- (* 2 i) 1))))
      (setq i (1+ i)))
    total))

;;;###autoload
(defun earthquake-hp-building-left (level number hp)
  "Calculate remaining building HP after NUMBER earthquakes of some LEVEL."
  (interactive "nPress the level of the earthquake:\nnPress the number of earthquakes:\nnPress the hp of the building:")
  (print (format "earthquakes building hp left: %d" (- hp(earthquake-damage-calculator level number hp)))))

;;;###autoload
(defun fireball-level-picker (level)
  "Pick the good damage for the fireball LEVEL."
  (let ((levels '(1500 1500 1700 1700 1800 1950 1950 2050 2200 2200 2350 2650 2650 2750 3100 3100 3250 3400 3400 3500 3650 3650 3750 3900 3900 3950 4100))) (elt levels (1- level))))


;;;###autoload
(defun fireball-hp-building-left (level hp)
  "Calculate fireball damage by it's LEVEL building HP."
  (interactive "nPress the level of the fireball: \nnPress the hp of the building: ")
  (print (format "building hp left after fireball: %d" (- hp (fireball-level-picker level)))))

;;;###autoload
(defun fireball-and-earthquake-calculator (fireball-level earthquake-level earthquake-number hp)
  "Calculate damage FIREBALL-LEVEL EARTHQUAKE-LEVEL EARTHQUAKE-NUMBER HP."
  (interactive "nPress the level of the fireball: \nnPress the level of the earthquake: \nnPress the number of earthquakes: \nnPress the hp of the building: ")
  (print (- hp (fireball-level-picker fireball-level ) (earthquake-damage-calculator earthquake-level earthquake-number hp))))

;;;###autoload
(defun rocket-spear-level-picker (level)
  "Pick the good damage for the rocket spear LEVEL."
  (let ((levels '(350  350 420 420 420 490 490 490 560 560 560 630 630 630 700 700 700 770 770 770 840 840 840 910 910 910 980))) (elt levels (1- level))))

;;;###autoload
(defun rocket-hp-building-left (level hp)
  "Calculate rocket-spear damage by it's LEVEL and building HP."
  (interactive "nPress the level of the rocket spear: \nnPress the hp of the building: ")
  (print (format "building hp left after rocket spear: %d" (- hp (rocket-spear-level-picker level)))))

;;;###autoload
(defun lightning-level-picker (level)
  "Pick the good percentage for the lightning LEVEL."
  (let ((levels '(150 180 210 240 270 320 400 480 560 600 640)))
    (elt levels (1- level))))

;;;###autoload
(defun lightning-damage-calculator (level number)
  "Calculate lightning damage depending of NUMBER LEVEL."
  (let* ((i 2)
	 (damage (lightning-level-picker level))
	 (total damage))
    (while (<= i number)
      (setq total (+ total damage))
      (setq i (1+ i)))
    total))

;;;###autoload
(defun lightning-hp-building-left (level hp)
  "Calculate lightning damage by it's LEVEL and building HP."
  (interactive "nPress the level of the lightning: \nnPress the hp of the building: ")
  (print (format "building hp left after lightning: %d" (- hp (lightning-level-picker level)))))

(provide 'coc-damage-calculator)
;;; coc-damage-calculator.el ends here
