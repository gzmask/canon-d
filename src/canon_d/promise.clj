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
  (at (m b) (p/sampled-piano (o/note :a3) :level 0.5))
  (at (m (+ 1 b)) (p/sampled-piano (o/note :d3) :level 0.5))
  (at (m (+ 2 b)) (p/sampled-piano (o/note :f4) :level 0.5))
  (at (m (+ 3 b)) (p/sampled-piano (o/note :a3) :level 0.5)))
(intro1 (m))
(defn intro2 [b]
  (at (m (+ 0 b)) (p/sampled-piano (o/note :a3) :level 0.5))
  (at (m (+ 1 b)) (p/sampled-piano (o/note :d3) :level 0.5))
  (at (m (+ 2 b)) (p/sampled-piano (o/note :f4) :level 0.5))
  (at (m (+ 3 b)) (p/sampled-piano (o/note :a#3) :level 0.5)))
(intro2 (m))
(defn intro3 [b]
  (at (m (+ 0 b)) (p/sampled-piano (o/note :g3) :level 0.5))
  (at (m (+ 1 b)) (p/sampled-piano (o/note :a#3) :level 0.5))
  (at (m (+ 2 b)) (p/sampled-piano (o/note :f4) :level 0.5))
  (at (m (+ 3 b)) (p/sampled-piano (o/note :a#3) :level 0.5)))
(intro3 (m))
(defn intro [b]
  (intro1 b)
  (intro1 (+ 8 b))
  (intro2 (+ 16 b))
  (intro3 (+ 24 b))
  (at (m (+ 32 b)) (#'intro (+ 32 b))))
(intro (m))

(require '[overtone.inst.synth :as sth])
(sth/overpad (o/note :a3))
(sth/overpad (o/note :g3))
(defn base-line-one [b]
  (doall
   (map (fn [n b]
          (at (m b) (sth/overpad (o/note n)))
          (at (m (+ 1 b)) (sth/overpad (o/note n) :amp 1.1))
          (at (m (+ 2 b)) (sth/overpad (o/note n) :amp 1.1))
          (at (m (+ 3 b)) (sth/overpad (o/note n) :amp 1.1)))
        [:d3 :e3 :g3]
        [b (+ b 4) (+ b 8)])))
(defn base-line [b]
  (base-line-one b)
  (at (m (+ b 12)) (#'base-line (+ b 12))))
(base-line (m))

(require '[overtone.inst.drum :as d])

(d/hat3)
(d/clap)
(d/noise-snare)
(d/open-hat)
(d/quick-kick)
(d/dance-kick)
(d/snare)
(d/tom)

(defn drums-pat [b]
  (at (m (+ 0 b)) (d/hat3 :amp 1.0))
  (at (m (+ 1 b)) (d/snare :amp 1.0))
  (at (m (+ 2 b)) (d/snare :amp 1.0))
  (at (m (+ 3 b)) (d/quick-kick :amp 1.0))
  (at (m (+ 4 b)) (d/quick-kick :amp 0.8))
  (at (m (+ 5 b)) (d/clap :amp 0.8))
  (at (m (+ 5 b)) (d/snare :amp 0.8))
  (at (m (+ 7 b)) (d/quick-kick :amp 0.8)))

(defn drums [b]
  (drums-pat b)
  (at (m (+ b 8)) (#'drums (+ b 8))))
(drums (m))

(defn rec-guitar [reusable-long-buffer vol]
  (let [p (o/definst guitar-rec [vol 0.5]
            (let [guitar-in (o/sound-in [0 1])
                  guitar-out (o/record-buf:ar guitar-in reusable-long-buffer)]
              (* 10 vol guitar-out)))
        start-time (o/now)
        synth-id (p)]
    (fn stop-recording []
      (let [_ (o/kill synth-id)
            stop-time (o/now)
            buffer-length-ms (- stop-time start-time)
            buffer-length-seconds (/ buffer-length-ms 1000)
            out-buffer (o/buffer (* buffer-length-seconds
                                    (o/server-sample-rate)) 2)
            p (o/definst guitar-rec [vol 0.5]
                (let [guitar-in (o/play-buf:ar 2 reusable-long-buffer)
                      guitar-out (o/record-buf:ar guitar-in out-buffer)]
                  (* 10 vol guitar-out)))
            pid (p)]
        (at (+ (o/now) buffer-length-ms) (o/kill pid))
        out-buffer))))

(defn play-guitar [out-buffer]
  (o/definst go-buffer [vol 1]
    (* 10 vol (o/play-buf:ar 2 out-buffer)))
  (go-buffer))

(def buffer-pool (o/buffer (* 30 (o/server-sample-rate)) 2))
(def g15-73-15-37-stop-recod (rec-guitar buffer-pool 1.0))
(def g15-73-15-37-buffer (g15-73-15-37-stop-recod))
(play-guitar g15-73-15-37-buffer)
(def g77765-44543-77717-332-stop-rec (rec-guitar buffer-pool 1.0))
(def g77765-44543-77717-332-buffer (g77765-44543-77717-332-stop-rec))
(play-guitar g77765-44543-77717-332-buffer)
(def g3-21-123-4345-stop-rec (rec-guitar buffer-pool 1.0))
(def g3-21-123-4345-buffer (g3-21-123-4345-stop-rec))
(play-guitar g3-21-123-4345-buffer)

(o/stop)
(o/free-all-loaded-samples)
(at/stop-and-reset-pool! beat-pool :strategy :kill)
