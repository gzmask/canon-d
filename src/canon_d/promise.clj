(ns canon-d.promise
  (:require [overtone.core :as o]))

(def internal-server
  (o/boot-internal-server))

(o/demo (o/pan2 (o/sin-osc 440) (o/line -1 1 2)))

(o/defsynth dubstep [bpm 120 wobble 1 freq 440 snare-vol 1 kick-vol 1 v 1 out-bus 0]
 (let [trig (o/impulse:kr (/ bpm 120))
       swr (o/demand trig 0 (o/dseq [wobble] o/INF))
       sweep (o/lin-exp (o/lf-tri swr) -1 1 40 3000)
       wob (apply + (o/saw (* freq [0.99 1.01])))
       wob (o/lpf wob sweep)
       wob (* 0.8 (o/normalizer wob))
       wob (+ wob (o/bpf wob 1500 2))
       wob (+ wob (* 0.2 (o/g-verb wob 9 0.7 0.7)))

       kickenv (o/decay (o/t2a (o/demand (o/impulse:kr (/ bpm 30)) 0 (o/dseq [1 0 0 0 0 0 1 0 1 0 0 1 0 0 0 0] o/INF))) 0.7)
       kick (* (* kickenv 7) (o/sin-osc (+ 40 (* kickenv kickenv kickenv 200))))
       kick (o/clip2 kick 1)

       snare (* 3 (o/pink-noise) (apply + (* (o/decay (o/impulse (/ bpm 240) 0.5) [0.4 2]) [1 0.05])))
       snare (+ snare (o/bpf (* 4 snare) 2000))
       snare (o/clip2 snare 1)]

   (o/out out-bus (* v (o/clip2 (+ wob (* kick-vol kick) (* snare-vol snare)) 1)))))

(comment
  (def d (dubstep))
  (o/ctl d :wobble 2)
  (o/ctl d :freq (o/midi->hz (o/note :d4)))
  (o/ctl d :bpm 120)
  (o/stop)
  )

(o/odoc o/in)
(o/odoc o/sound-in)

(require '[overtone.inst.sampled-piano :as p])

(p/sampled-piano (o/note :d3))
(p/sampled-piano (o/note :e3))
(p/sampled-piano (o/note :f3))
(p/sampled-piano (o/note :g3))
(p/sampled-piano (o/note :a3))
(p/sampled-piano (o/note :a#3))
(p/sampled-piano (o/note :c4))
(p/sampled-piano (o/note :d4))


(map o/find-note-name (o/scale :d3 :minor))

(def m (o/metronome 120))
(- (m 2) (m 1))
(doseq [note (map o/find-note-name (concat (o/scale :d3 :minor)
                                           (rest (o/scale :d4 :minor))))]
  (p/sampled-piano (o/note note))
  (Thread/sleep 500))

(require '[overtone.at-at :as at])

(def beat-pool (at/mk-pool))

(at/at (m) #(p/sampled-piano (o/note :d3)) beat-pool)

(defmacro at [metronome play-fn]
  `(at/at ~metronome #(~play-fn) beat-pool))

(at (m (+ 2 (m))) (p/sampled-piano (o/note :d3)))
(defn intro1 [b]
  (at (m b) (p/sampled-piano (o/note :d4)))
  (at (m (+ 1 b)) (p/sampled-piano (o/note :a3)))
  (at (m (+ 2 b)) (p/sampled-piano (o/note :f4)))
  (at (m (+ 3 b)) (p/sampled-piano (o/note :a3))))
(intro1 (m))
(defn intro2 [b]
  (at (m (+ 0 b)) (p/sampled-piano (o/note :d3)))
  (at (m (+ 1 b)) (p/sampled-piano (o/note :a3)))
  (at (m (+ 2 b)) (p/sampled-piano (o/note :f3)))
  (at (m (+ 3 b)) (p/sampled-piano (o/note :a#3))))
(intro2 (m))
(defn intro3 [b]
  (at (m (+ 0 b)) (p/sampled-piano (o/note :g3)))
  (at (m (+ 1 b)) (p/sampled-piano (o/note :a#4)))
  (at (m (+ 2 b)) (p/sampled-piano (o/note :f4)))
  (at (m (+ 3 b)) (p/sampled-piano (o/note :a#3))))
(intro3 (m))

(require '[overtone.inst.synth :as sth])
(sth/overpad (o/note :d3))
(sth/overpad (o/note :g3))
(defn base-line [b]
  (doall
   (map (fn [n b]
          (at (m b) (sth/overpad (o/note n)))
          (at (m (+ 1 b)) (sth/overpad (o/note n) :amp 1.0))
          (at (m (+ 2 b)) (sth/overpad (o/note n)))
          (at (m (+ 3 b)) (sth/overpad (o/note n))))
        [:d3 :d3 :g3]
        [b (+ b 8) (+ b 16)])))
(base-line (m))

(require '[overtone.inst.drum :as d])

(d/kick)
(d/dub-kick)
(d/hat3)
(d/clap)
(d/noise-snare)
(d/open-hat)
(d/quick-kick)
(d/snare)
(d/tom)

(defn drums [b]
  (doall 
   (map
    #(if (zero? (mod % 4))
       (at (m (+ % b)) (d/quick-kick :amp 1.0))
       (at (m (+ % b)) (d/hat3 :amp 0.8)))
    (range 24))))

(drums (m))


(defn intro [b]
  (drums b)
  (base-line b)
  (intro1 b)
  (intro1 (+ 4 b))
  (intro2 (+ 8 b))
  (intro2 (+ 12 b))
  (intro3 (+ 16 b))
  (intro3 (+ 20 b))
  (at (m (+ 24 b)) (#'intro (+ 24 b))))
(intro (m))
(at/stop-and-reset-pool! beat-pool :strategy :kill)

(defn go-guitar [buffer-length-seconds]
  (let [in-buffer (o/buffer (* buffer-length-seconds
                               (o/server-sample-rate))
                            2)]
    (o/definst guitar-solo [vol 0.5]
      (let [guitar-in (o/sound-in [0 1])
            guitar-out (o/record-buf:ar guitar-in in-buffer)]
        (* 10 vol guitar-out)))
    (at (m (+ 8 (m))) (guitar-solo))
    in-buffer))
(def c-buffer (go-guitar 24))
(o/stop)
(o/free-sample c-buffer)

(defn play-guitar [out-buffer]
  (o/definst go-buffer [vol 1]
    (* 10 vol (o/play-buf:ar 2 out-buffer)))
  (go-buffer))
(play-guitar c-buffer)
