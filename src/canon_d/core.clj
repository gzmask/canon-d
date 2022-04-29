(ns canon-d.core
  (:require [overtone.live :as ol]))

(ol/demo (ol/saw 440))
(ol/stop)

(require '[overtone.inst.sampled-piano :as p])

(p/sampled-piano (ol/note :c4))
(p/sampled-piano (ol/note :d4))
(p/sampled-piano (ol/note :e4))
(def piano-sound-f4
  (p/sampled-piano (ol/note :f4)))
(ol/ctl piano-sound-f4 :amp 0.1 :note (ol/note :c4))

(def dirty-kick
  (ol/sample "~/Downloads/drum/OH/kick_OH_P_2.wav"))
(def semi-open-hat
  (ol/sample
   "/home/gzmask/Downloads/drum/OH/hihatSemiOpen5_OH_P_1.wav"))
(ol/defsample semi-open-hat2 "/home/gzmask/Downloads/drum/OH/hihatSemiOpen5_OH_P_2.wav")


(def ring-hat (ol/freesound 12912))
(def snare (ol/freesound 26903))
(def click (ol/freesound 406))
(def wop (ol/freesound 85291))
(def subby (ol/freesound 25649))
(dirty-kick :amp 1.0)
(semi-open-hat)
(semi-open-hat2)
(ring-hat)
(snare)
(click)
(click :decay 0.05 :amp 0.3)
(wop :amp 1.0)
(subby)

(def m (ol/metronome 128))

(defn player
  [beat]
  (let [next-beat (inc beat)]
    (when (zero? (mod beat 16))
      (ol/at (m beat) (p/sampled-piano (ol/note :c4) 0.2))
      (ol/at (m (+ beat 0.1)) (p/sampled-piano (ol/note :e3) 0.1))
      (ol/at (m (+ beat 0.2)) (p/sampled-piano (ol/note :g4) 0.1))
      (ol/at (m (+ beat 2)) (p/sampled-piano (ol/note :g3) 0.2))
      (ol/at (m (+ beat 2.1)) (p/sampled-piano (ol/note :b3) 0.1))
      (ol/at (m (+ beat 2.2)) (p/sampled-piano (ol/note :d3) 0.1))
      (ol/at (m (+ beat 4)) (p/sampled-piano (ol/note :a4) 0.2))
      (ol/at (m (+ beat 4.1)) (p/sampled-piano (ol/note :c4) 0.1))
      (ol/at (m (+ beat 4.2)) (p/sampled-piano (ol/note :e4) 0.1)))
    (when (zero? (mod beat 4))
      (ol/at (m beat) (dirty-kick :amp 0.8))
      (ol/at (m (+ beat 1/4)) (dirty-kick :amp 0.8))
      (ol/at (m (+ beat 1)) (wop :amp 0.8))
      (ol/at (m (+ beat 2)) (subby :amp 1.0))
      (ol/at (m (+ beat 3)) (dirty-kick :amp 0.8))
      (ol/at (m (+ beat 3.5)) (dirty-kick :amp 0.8)))
    (ol/at (m beat) (click :amp 0.2))
    (ol/at (m (+ beat 1/2)) (dirty-kick :amp 0.3))
    (ol/at (m (+ beat 2/3)) (wop :amp 0.1))
    (ol/at (m (+ beat 3/4)) (subby :amp 0.1))
    (ol/apply-by (m next-beat) #'player [next-beat])))

(player (m))
(ol/stop)

(ol/definst guitar-solo [] (ol/sound-in [0 1]))
(def guitar-solo-track (guitar-solo :amp 0.5))
(ol/ctl guitar-solo-track :amp 0.8)
(ol/stop)

(ol/recording-start "guitar.wav")
(ol/recording-stop)
;(def guitar-solo (ol/sample "/home/gzmask/project/canon-d/guitar.wav"))
(ol/defsample guitar-solo "/home/gzmask/project/canon-d/guitar.wav" :start (* 4 (ol/server-sample-rate)))
(guitar-solo :amp 10.0)
(ol/free-all-loaded-samples)
(ol/free-sample guitar-solo)
