(ns canon-d.raining-car
  (:require [overtone.core :as o]))

(def internal-server
  (o/boot-internal-server))

(o/demo (o/saw 440))
(o/stop)

(meta #'o/demo)

(require '[overtone.inst.sampled-piano :as p])

(p/sampled-piano (o/note :A3))

(def dirty-kick (o/freesound 344757 :size (int (* 1.1 (o/server-sample-rate)))))
(def ring-hat (o/freesound 387347))
(def snare (o/freesound 26903))
(def click (o/freesound 406))
(def wop (o/freesound 85291))
(def subby (o/freesound 25649))
(dirty-kick)
(ring-hat)
(snare)
(click)
(wop)
(subby)

(o/buffer-info dirty-kick)
(o/buffer-info ring-hat)
(o/buffer-info subby)

(meta #'o/sample-rate)
(o/sample-rate:ir)

(require '[overtone.inst.synth :as sth])

(sth/overpad (o/note :A3))

(def a-buffer (o/buffer (* 2 (o/server-sample-rate))))
(o/free-sample a-buffer)
(def a-recorder
  (o/record-buf:ar (o/sound-in) a-buffer))
(o/definst go-recorder [freq 440 attack 0.01 sustain 0.4 release 0.1 vol 0.4]
  (* 10 vol a-recorder))
(go-recorder)
(o/stop)
(o/definst go-buffer [freq 440 attack 0.01 sustain 0.4 release 0.1 vol 1]
  [(* 4 vol (o/play-buf:ar 1 a-buffer))
   (* 4 vol (o/play-buf:ar 1 a-buffer))])
(go-buffer)
(o/buffer-info a-buffer)
(o/buffer-save a-buffer "123.wav")
(meta #'o/play-buf)
(meta #'o/record-buf)

(def m (o/metronome 128))

(defn get-ms-per-beat [m]
  (int (- (m 2) (m 1))))

(def ms-per-beat (get-ms-per-beat m))

(require '[overtone.at-at :as at])

(def my-pool (at/mk-pool))

(at/at (+ 1000 (o/now)) #(dirty-kick :amp 0.2) my-pool)

(defn base-drums [b]
  (at/at (m b) #(dirty-kick :amp 0.2) my-pool)
  (at/at (m (+ 0.5 b)) #(subby :amp 0.2) my-pool)
  (at/at (m (inc b)) #(wop :amp 0.2) my-pool)
  (at/at (m (+ 3 b)) #(click :amp 0.2) my-pool))
(base-drums (m))

(defn progressing [b]
  (doseq [t (range 0 2 0.25)]
    (at/at (m (+ t b)) #(dirty-kick :amp (* 0.5 (rand 1))) my-pool)))
(progressing (m))

(defn base-line [b]
  (doall
   (map
    (fn [note t]
      (at/at (m (+ t b)) #(sth/overpad note :amp (rand-nth [0 0.1 0.2])) my-pool))
    (shuffle (o/scale :a3 :minor))
    (range 0 8 (rand-nth [1 0.5 0.25])))))
(base-line (m))

(defn player [b]
  (let [next-beat (inc b)]
    (when (zero? (mod b 4))
      (base-drums b))
    (when (zero? (mod b 8))
      (progressing b))
    (when (zero? (mod b 16))
      (base-line b))
    (at/at (m next-beat) #(#'player next-beat) my-pool)))

(player (m))
(o/stop)
(at/scheduled-jobs my-pool)
(at/stop-and-reset-pool! my-pool :strategy :kill)
(at/stop-and-reset-pool! my-pool)

