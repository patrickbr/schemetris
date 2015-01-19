; Universität Tübingen
; Wilhelm Schickard Institut für Informatik

;
;    schemetris 3000
;
;    tetris in scheme
;
;    2007 patrick brosi


; verwirklichung eines klassischen tetrisklons in scheme unter verwendung von
; scheme, image.ss und world.ss. Aufbau einer listenmatrix zur darstellung eines
; spielfeldes, abfrage von atastaturereignissen und entwicklung eines
; spielalgorithmus

;

; records

(define-record-procedures world
  make-world world?
  (world-zeilen world-stein world-tick world-vars))

(define-record-procedures vars
  make-var var?
  (var-gameover? var-pause? var-punkte var-start?))

(define leervar
  (make-var #f #f 0 #f))

(define startvar
  (make-var #f #f 0 #t))
(define-record-procedures raster
  make-raster raster?
  (raster-voll? raster-moving? color))

(define-record-procedures stein
  make-stein stein?
  (stein-art stein-dreher first?))

(define nullstein
  (make-stein 1 1 #f))

; titelbild als listenmatrix
; zeile:
(define start-zeile (list (make-raster #f #f "white")
                          (make-raster #f #f "white")
                          (make-raster #f #f "white")
                          (make-raster #f #f "white")
                          (make-raster #f #f "white")
                          (make-raster #f #f "white")
                          (make-raster #f #f "white")
                          (make-raster #f #f "white")
                          (make-raster #f #f "white")
                          (make-raster #f #f "white")
                          (make-raster #f #f "white")))
; spalten

(define startliste
  (list start-zeile
        start-zeile
        start-zeile
        start-zeile
        start-zeile
        start-zeile
        start-zeile
        start-zeile
        start-zeile
        start-zeile
        start-zeile
        start-zeile
        start-zeile
        start-zeile
        start-zeile
        start-zeile
        start-zeile
        start-zeile
        start-zeile
        start-zeile
        start-zeile
        start-zeile
        start-zeile
        start-zeile ))

; strarbildschirmzeugs:

(define leer-world
  (make-world startliste nullstein 0 leervar) )

(define hintergrund (rectangle 220 440 "outline" "black"))

(define lollist
    (list
   (list (make-raster #t #t "red")(make-raster #f #f "white")(make-raster #f #f "white")(make-raster #f #f "white")(make-raster #t #t "red")(make-raster #f #f "white")
         (make-raster #f #f "white")(make-raster #f #f "white")(make-raster #f #f "white")(make-raster #f #f "white") (make-raster #f #f "white"))
   (list (make-raster #t #t "red")(make-raster #f #f "white")(make-raster #t #t "yellow")(make-raster #t #t "yellow")(make-raster #t #t "red")(make-raster #f #f "white")
         (make-raster #f #f "white")(make-raster #t #t "brown")(make-raster #f #f "white")(make-raster #f #f "white")(make-raster #f #f "white"))
   (list (make-raster #t #t "red")(make-raster #t #t "red")(make-raster #t #t "yellow")(make-raster #t #t "yellow")(make-raster #t #t "red")(make-raster #t #t "red")
         (make-raster #f #f "white")(make-raster #f #f "white")(make-raster #f #f "white")(make-raster #t #t "violet")(make-raster #t #t "violet"))
   (list (make-raster #t #t "green")(make-raster #t #t "green")(make-raster #t #t "green")(make-raster #t #t "blue")(make-raster #t #t "blue")(make-raster #f #f "white")(make-raster #f #f "white")(make-raster #t #t "brown")(make-raster #t #t "violet")
         (make-raster #f #f "white")(make-raster #f #f "white"))
   (list (make-raster #f #f "white")(make-raster #t #t "green")(make-raster #f #f "white")(make-raster #t #t "blue")(make-raster #f #f "white")(make-raster #t #t "blue")(make-raster #f #f "white")(make-raster #t #t "brown")(make-raster #f #f "white")(make-raster #t #t "violet")(make-raster #f #f "white")
         )
   (list (make-raster #f #f "white")(make-raster #t #t "green")(make-raster #f #f "white")(make-raster #t #t "blue")(make-raster #t #t "blue")(make-raster #f #f "white")(make-raster #f #f "white")(make-raster #t #t "brown")(make-raster #f #f "white")(make-raster #f #f "white")(make-raster #t #t "violet")
         )
   (list (make-raster #f #f "white")(make-raster #t #t "green")(make-raster #f #f "white")(make-raster #t #t "blue")(make-raster #f #f "white")(make-raster #t #t "blue")(make-raster #f #f "white")(make-raster #t #t "brown")(make-raster #t #t "violet")(make-raster #t #t "violet")(make-raster #f #f "white"))
   start-zeile
   start-zeile
   start-zeile
   start-zeile
   start-zeile
   start-zeile
   start-zeile
   start-zeile
   start-zeile
   start-zeile
   start-zeile
   start-zeile
   start-zeile
   start-zeile
   start-zeile
   start-zeile
   start-zeile
   start-zeile
   start-zeile ))

; erste welt, leer
(define start-world
  (make-world lollist nullstein 0 startvar))

; grafik: stein
(define steinklein
  (lambda (c)
    (overlay (rectangle 20 20 "solid" "black") (rectangle 16 16 "solid" c) 2 2)))

; information an stelle n einer zeile
(define get
  (lambda (n liste)
    (if (empty? liste) startliste
        (if (= n 1) (first liste)
            (get (- n 1) (rest liste))))))

; information "eintrag" an stelle n einer zeile schreiben
(define write
  (lambda (n liste eintrag)
    (if (empty? liste) empty
        (make-pair (if (= n 1) eintrag (first liste)) (write (- n 1) (rest liste) eintrag)))))

; selbiges innerhalb der spalten
(define raster
  (lambda (a b zeilen)

    (if (= a 1) (get b (first zeilen)) (raster (- a 1) b (rest zeilen)))))

; s.o.
(define rasterwrite
  (lambda (a b zeilen eintrag)
    (write a zeilen (write b (get a zeilen) eintrag))))

; hauptspielschleife
(define make-alles
  (lambda (w)
    (letrec ((loop (lambda (zeilen zeile spalte result)
                     (if (and (= zeile 1) (= spalte 12)) result
                         (if (= spalte 12) (loop zeilen (- zeile 1) 1 result)
                             (let ((rasterding (raster zeile spalte zeilen)))
                               (if (raster-voll? rasterding) (loop zeilen zeile (+ spalte 1)
                                                                   (if (> zeile 2)
                                                                       (overlay result
                                                                                (steinklein (color rasterding))
                                                                                (* (- spalte 1) 20) (* (- zeile 3) 20)) result))

                                   (loop zeilen zeile (+ spalte 1) result)))))))) (loop (world-zeilen w) 24 1 hintergrund ))))


; stein eine runde fallen lassen
(define rasterfall
  (lambda (a b zeilen)
    (rasterwrite a b (rasterwrite (+ a 1) b zeilen (make-raster #t #t (color (raster a b zeilen)))) (make-raster #f #f "white"))))

; stein eine pos nach links
(define rasterleft
  (lambda (a b zeilen)

    (rasterwrite a b (rasterwrite a (- b 1) zeilen (make-raster #t #t (color (raster a b zeilen)))) (make-raster #f #f "white")) ))

; stein eine pos nach rechts
(define rasterright
  (lambda (a b zeilen)

    (rasterwrite a b (rasterwrite a (+ b 1) zeilen (make-raster #t #t (color (raster a b zeilen)))) (make-raster #f #f "white")) ))

; stein bewegbar??
(define moveable?
  (lambda (w)
    (letrec ((loop (lambda (zeilen zeile spalte)
                     (if (and (= zeile 1) (= spalte 12)) #t
                         (if (= spalte 12) (loop zeilen (- zeile 1) 1)

                             (if (and
                                  (raster-moving? (raster zeile spalte zeilen))
                                  (or (= zeile 24)
                                      (let ((rasterding (raster (+ zeile 1) spalte zeilen)))
                                        (and
                                         (raster-voll? rasterding)
                                         (not (raster-moving? rasterding))))))
                                 #f
                                 (loop zeilen zeile (+ spalte 1))))))))

      (loop (world-zeilen w) 24 1))))

; stein nach links bewegbaR
(define leftable?
  (lambda (w)
    (letrec ((loop (lambda (zeilen zeile spalte)
                     (if (and (= zeile 1) (= spalte 12)) #t
                         (if (= spalte 12) (loop zeilen (- zeile 1) 1)

                             (if (and (raster-moving? (raster zeile spalte zeilen))
                                      (or (= spalte 1)
                                          (let ((rasterding  (raster zeile (- spalte 1) zeilen)))
                                            (and (raster-voll? rasterding)
                                                 (not (raster-moving? rasterding))))))
                                 #f
                                 (loop zeilen zeile (+ spalte 1))))))))

      (loop (world-zeilen w) 24 1))))

; nach rechts bewegbar?
(define rightable?
  (lambda (w)
    (letrec ((loop (lambda (zeilen zeile spalte)
                     (if (and (= zeile 1) (= spalte 12)) #t
                         (if (= spalte 12) (loop zeilen (- zeile 1) 1)

                             (if (and (raster-moving? (raster zeile spalte zeilen))
                                      (or (= spalte 11)
                                          (let ((rasterding (raster zeile (+ spalte 1) zeilen)))
                                            (and (raster-voll? rasterding)
                                                 (not (raster-moving? rasterding))))))
                                 #f
                                 (loop zeilen zeile (+ spalte 1))))))))

      (loop (world-zeilen w) 24 1))))


;zurücksetzen
(define reset-moveable
  (lambda (w)
    (letrec ((loop (lambda (zeilen zeile spalte result)
                     (if (and (= zeile 1) (> spalte 11)) (make-world result (world-stein w)(world-tick w)(world-vars w) )
                         (if (> spalte 11) (loop zeilen (- zeile 1) 1 result)
                             (let ((rasterdings  (raster zeile spalte zeilen)))
                               (if (raster-moving? rasterdings)
                                   (loop zeilen zeile (+ spalte 1)
                                         (rasterwrite zeile spalte result
                                                      (make-raster (raster-voll? rasterdings) #f (color rasterdings))))
                                   (loop zeilen zeile (+ spalte 1) result)))


                             )))))

      (loop (world-zeilen w) 24 1 (world-zeilen w)))))

; alle steine bewegen, spielschleife untergeordnet:
(define moveall!
  (lambda (w)
    (if (moveable? w)
        (letrec ((loop (lambda (zeilen zeile spalte result )
                         (if (and (= zeile 1) (> spalte 11)) (make-world result (world-stein w) (world-tick w) (world-vars w))
                             (if (> spalte 11) (loop zeilen (- zeile 1) 1 result )
                                 (if (raster-moving? (raster zeile spalte zeilen))
                                     (loop zeilen zeile (+ spalte 1) (rasterfall zeile spalte result ))
                                     (loop zeilen zeile (+ spalte 1) result )))))))
          (loop (world-zeilen w) 24 1 (world-zeilen w)))
        (deletefulllines (reset-moveable w ))
        )))

; volle linien suchen, löschen, punkte addiern
(define deletefulllines
  (lambda (w)
    (if (not (var-start? (world-vars w)))
        (zufallstein (make-world
                      (letrec ((loop (lambda (zeilen)
                                       (if (empty? zeilen) empty
                                           (let ((jo? (and (raster-voll? (get 1 (first zeilen)))
                                                           (raster-voll? (get 2 (first zeilen)))
                                                           (raster-voll? (get 3 (first zeilen)))
                                                           (raster-voll? (get 4 (first zeilen)))
                                                           (raster-voll? (get 5 (first zeilen)))
                                                           (raster-voll? (get 6 (first zeilen)))
                                                           (raster-voll? (get 7 (first zeilen)))
                                                           (raster-voll? (get 8 (first zeilen)))
                                                           (raster-voll? (get 9 (first zeilen)))
                                                           (raster-voll? (get 10 (first zeilen)))
                                                           (raster-voll? (get 11 (first zeilen))))))
                                             (append  (if jo? (list start-zeile) empty) (loop (rest zeilen)) (if jo? empty (list (first zeilen)))))
                                           ))))
                        (loop (reverse
                               ; neuerstein
                               (world-zeilen w)
                               ; ====
                               ))) nullstein 0  (make-var (var-gameover? (world-vars w)) (var-pause? (world-vars w))

                                                          ;punkte=======

                                                          (+ (var-punkte (world-vars w))
                                                             (* (countfulllines (world-zeilen w)) (countfulllines (world-zeilen w)) 10))
                                                          (var-start? (world-vars w)))))w)))

; volle linien zählen
(define countfulllines
  (lambda (zeilen)
    (if (empty? zeilen) 0
        (let ((jo? (and (raster-voll? (get 1 (first zeilen)))
                        (raster-voll? (get 2 (first zeilen)))
                        (raster-voll? (get 3 (first zeilen)))
                        (raster-voll? (get 4 (first zeilen)))
                        (raster-voll? (get 5 (first zeilen)))
                        (raster-voll? (get 6 (first zeilen)))
                        (raster-voll? (get 7 (first zeilen)))
                        (raster-voll? (get 8 (first zeilen)))
                        (raster-voll? (get 9 (first zeilen)))
                        (raster-voll? (get 10 (first zeilen)))
                        (raster-voll? (get 11 (first zeilen))))))
          (+ (if jo? 1 0) (countfulllines (rest zeilen)))))))


; stein nach links!
(define goleft!
  (lambda (w)
    (letrec ((loop (lambda (zeilen zeile spalte result)
                     (if (and (= zeile 1) (> spalte 11)) (make-world result (world-stein w) (world-tick w) (world-vars w))
                         (if (> spalte 11) (loop zeilen (- zeile 1) 1 result)
                             (if (raster-moving? (raster zeile spalte zeilen))
                                 (loop zeilen zeile (+ spalte 1) (rasterleft zeile spalte result))
                                 (loop zeilen zeile (+ spalte 1) result)))))))

      (loop (world-zeilen w) 24 1 (world-zeilen w)))))

; stein nach rechts
(define goright!
  (lambda (w)
    (letrec ((loop (lambda (zeilen zeile spalte result)
                     (if (and (= zeile 1) (= spalte 1)) (make-world result (world-stein w) (world-tick w) (world-vars w))
                         (if (= spalte 0) (loop zeilen (- zeile 1) 11 result)

                             (if (raster-moving? (raster zeile spalte zeilen))
                                 (loop zeilen zeile (- spalte 1) (rasterright zeile spalte result))
                                 (loop zeilen zeile (- spalte 1) result)))))))
      (loop (world-zeilen w) 24 11 (world-zeilen w)))))

(define next-tick
  (lambda (w)
    (moveall! w)))

;======================================= steine
;==============================================
(define make-viereck
  (lambda (w zeile spalte walt first)
    (let ((a zeile )
          (a1 spalte)
          (b  zeile )
          (b1 (+ spalte 1))
          (c (+ zeile 1))
          (c1 spalte)
          (d (+ zeile 1))
          (d1 (+ spalte 1)))

      (malstein a a1 b b1 c c1 d d1 w 3 1 walt "red" first))))

(define make-langes2
  (lambda (w zeile spalte walt first)
    (let ((a (- zeile 3))
          (a1 spalte)
          (b (- zeile 2))
          (b1 spalte)
          (c (- zeile 1))
          (c1 spalte)
          (d zeile)
          (d1 spalte))

      (malstein a a1 b b1 c c1 d d1 w 1 2 walt "orange" first))))

(define make-langes1
  (lambda (w zeile spalte walt first)
    (let ((a zeile)
          (a1 spalte)
          (b zeile)
          (b1 (+ spalte 1))
          (c zeile)
          (c1 (+ spalte 2))
          (d zeile)
          (d1 (+ spalte 3)))

      (malstein a a1 b b1 c c1 d d1 w 1 1 walt "orange" first))))

(define make-L11
  (lambda (w zeile spalte walt first)
    (let ((a zeile)
          (a1 spalte)
          (b (- zeile 1))
          (b1 spalte)
          (c zeile)
          (c1 (+ spalte 1))
          (d zeile)
          (d1 (+ spalte 2)))

      (malstein a a1 b b1 c c1 d d1 w 2 1 walt "violet" first))))

(define make-L12
  (lambda (w zeile spalte walt first)
    (let ((a (- zeile 2))
          (a1 (+ spalte 1))
          (b (- zeile 2))
          (b1 spalte)
          (c (- zeile 1))
          (c1 spalte)
          (d zeile)
          (d1 spalte ))

      (malstein a a1 b b1 c c1 d d1 w 2 2 walt "violet" first))))

(define make-L13
  (lambda (w zeile spalte walt first)
    (let ((a (+ zeile 1))
          (a1 (+ spalte 2))
          (b  zeile )
          (b1 (+ spalte 1))
          (c zeile )
          (c1 (+ spalte 2))
          (d zeile)
          (d1 spalte ))

      (malstein a a1 b b1 c c1 d d1 w 2 3 walt "violet" first))))

(define make-L14
  (lambda (w zeile spalte walt first)
    (let ((a (- zeile 2))
          (a1  spalte)
          (b  (- zeile 1) )
          (b1 spalte)
          (c zeile )
          (c1 (- spalte 1))
          (d zeile)
          (d1 spalte ))

      (malstein a a1 b b1 c c1 d d1 w 2 4 walt "violet" first))))

;==
(define make-L21
  (lambda (w zeile spalte walt first)
    (let ((a zeile)
          (a1 spalte)
          (b (+ zeile 1))
          (b1 spalte)
          (c zeile)
          (c1 (+ spalte 1))
          (d zeile)
          (d1 (+ spalte 2)))

      (malstein a a1 b b1 c c1 d d1 w 7 1 walt "blue" first))))

(define make-L22
  (lambda (w zeile spalte walt first)
    (let ((a (- zeile 2))
          (a1 (- spalte 1))
          (b (- zeile 2))
          (b1 spalte)
          (c (- zeile 1))
          (c1 spalte)
          (d zeile)
          (d1 spalte ))

      (malstein a a1 b b1 c c1 d d1 w 7 2 walt "blue" first))))

(define make-L23
  (lambda (w zeile spalte walt first)
    (let ((a (- zeile 1))
          (a1 (+ spalte 2))
          (b  zeile )
          (b1 (+ spalte 1))
          (c zeile )
          (c1 (+ spalte 2))
          (d zeile)
          (d1 spalte ))

      (malstein a a1 b b1 c c1 d d1 w 7 3 walt "blue" first))))

(define make-L24
  (lambda (w zeile spalte walt first)
    (let ((a (- zeile 2))
          (a1  spalte)
          (b  (- zeile 1) )
          (b1 spalte)
          (c zeile )
          (c1 (+ spalte 1))
          (d zeile)
          (d1 spalte ))

      (malstein a a1 b b1 c c1 d d1 w 7 4 walt "blue" first))))

(define make-T1
  (lambda (w zeile spalte walt first)
    (let ((a (- zeile 1))
          (a1  spalte)
          (b   zeile)
          (b1 (+ spalte 1))
          (c zeile )
          (c1 (- spalte 1))
          (d zeile)
          (d1 spalte ))

      (malstein a a1 b b1 c c1 d d1 w 4 1 walt "yellow" first))))

(define make-T2
  (lambda (w zeile spalte walt first)
    (let ((a (- zeile 1))
          (a1  spalte)
          (b   zeile)
          (b1  spalte)
          (c zeile )
          (c1 (+ spalte 1))
          (d (+ zeile 1))
          (d1 spalte ))

      (malstein a a1 b b1 c c1 d d1 w 4 2 walt "yellow" first))))

(define make-T3
  (lambda (w zeile spalte walt first)
    (let ((a (+ zeile 1))
          (a1  spalte)
          (b   zeile)
          (b1 (+ spalte 1))
          (c zeile )
          (c1 (- spalte 1))
          (d zeile)
          (d1 spalte ))

      (malstein a a1 b b1 c c1 d d1 w 4 3 walt "yellow" first))))

(define make-T4
  (lambda (w zeile spalte walt first)
    (let ((a (- zeile 1))
          (a1  spalte)
          (b   zeile)
          (b1  spalte)
          (c zeile )
          (c1 (- spalte 1))
          (d (+ zeile 1))
          (d1 spalte ))

      (malstein a a1 b b1 c c1 d d1 w 4 4 walt "yellow" first))))

(define make-Z11
  (lambda (w zeile spalte walt first)
    (let ((a  zeile )
          (a1  spalte)
          (b   zeile)
          (b1  (+ spalte 1))
          (c (- zeile 1) )
          (c1 (- spalte 1))
          (d (- zeile 1))
          (d1 spalte ))

      (malstein a a1 b b1 c c1 d d1 w 5 1 walt "green" first))))

(define make-Z12
  (lambda (w zeile spalte walt first)
    (let ((a  zeile )
          (a1  spalte)
          (b   (+ zeile 1))
          (b1  spalte )
          (c (+ zeile 1)  )
          (c1 (- spalte 1))
          (d  (+ zeile 2) )
          (d1  (- spalte 1) ))

      (malstein a a1 b b1 c c1 d d1 w 5 2 walt "green" first))))

(define make-Z21
  (lambda (w zeile spalte walt first)
    (let ((a  (- zeile 1) )
          (a1  spalte)
          (b   (- zeile 1))
          (b1  (+ spalte 1))
          (c zeile )
          (c1 (- spalte 1))
          (d zeile)
          (d1 spalte ))

      (malstein a a1 b b1 c c1 d d1 w 6 1 walt "brown" first))))

(define make-Z22
  (lambda (w zeile spalte walt first)
    (let ((a  zeile )
          (a1  (- spalte 1))
          (b   (+ zeile 1))
          (b1  (- spalte 1) )
          (c (+ zeile 1)  )
          (c1 spalte)
          (d  (+ zeile 2) )
          (d1  spalte ))

      (malstein a a1 b b1 c c1 d d1 w 6 2 walt "brown" first))))

; ende steine ======

(define belegt+?
  (lambda (w a b)

    (if (> b 10) #f
        (raster-voll? (raster a (+ b 1) (world-zeilen w))) )))
(define belegt?
  (lambda (w a b)

    (if (> b 10) #f
        (raster-voll? (raster a (+ b 0) (world-zeilen w))) )))

(define belegt-?
  (lambda (w a b)
    (if (< b 2) #f
        (raster-voll? (raster a (- b 1) (world-zeilen w))))))

(define gameoverworld
  (lambda (w)
    (make-world startliste nullstein -3 (make-var #t (var-pause? (world-vars w)) (var-punkte (world-vars w)) #f
                                                  ))))

; stein malen, überprüfen ob belebt, drehbar, etc etc
(define malstein
  (lambda (a a1 b b1 c c1 d d1 w e f walt color first)
    (if (or (> a 24) (> b 24) (> c 24) (> d 24))
        (if first (make-world startliste nullstein -3 (make-var #t (var-pause? (world-vars w))(var-punkte (world-vars w)))) walt)
        (if  (or (< a1 1) (< b1 1) (< c1 1) (< d1 1))
             (malstein a (+ a1 1) b (+ b1 1) c (+ c1 1) d (+ d1 1) w e f walt color first)
             (if (or (> a1 11) (> b1 11) (> c1 11) (> d1 11))
                 (malstein a (- a1 1) b (- b1 1) c (- c1 1) d (- d1 1) w e f walt color first)
                 (let ((rechtsbelegt? (or (belegt+? w a a1) (belegt+? w b b1) (belegt+? w c c1) (belegt+? w d d1)))
                       (linksbelegt? (or (belegt-? w a a1) (belegt-? w b b1) (belegt-? w c c1) (belegt-? w d d1)))
                       (allgemeinbelegt? (or (belegt? w a a1) (belegt? w b b1) (belegt? w c c1) (belegt? w d d1))))
                   (if (and first allgemeinbelegt?) (gameoverworld w)
                       (if allgemeinbelegt?
                           (if (or (and rechtsbelegt? linksbelegt?)
                                   (and linksbelegt?  (or (> a1 11) (> b1 11) (> c1 11) (> d1 11)))
                                   (and rechtsbelegt? (or (< a1 1) (< b1 1) (< c1 1) (< d1 1))))
                               (if first (gameoverworld w) walt)
                               (cond
                                 (rechtsbelegt?
                                  (if (not (or (< a1 1) (< b1 1) (< c1 1) (< d1 1)))
                                      (if first (gameoverworld w) walt)
                                      (malstein a (- a1 1) b (- b1 1) c (- c1 1) d (- d1 1) w e f walt color first)))
                                 (linksbelegt?
                                  (if (or (> a1 11) (> b1 11) (> c1 11) (> d1 11))
                                      (malstein a (+ a1 1) b (+ b1 1) c (+ c1 1) d (+ d1 1) w e f walt color first)
                                      (if first (gameoverworld w) walt)))
                                 (else (if first (gameoverworld w) walt))))
                           (make-world (rasterwrite a a1 (rasterwrite b b1 (rasterwrite  c c1  (rasterwrite  d d1 (world-zeilen w) (make-raster #t #t color)) (make-raster #t #t color)) (make-raster #t #t color)) (make-raster #t #t color)) (make-stein e f #f) (world-tick w) (world-vars w) )))))))))

; ==== steinumdrehen

(define-record-procedures kommunikation
  make-cum cum?
  (zeilen cum-1 cum-11 cum-2 cum-22 cum-3 cum-33 cum-4 cum-44))

(define deletemoveables
  (lambda (w)
    (letrec
        ((loop (lambda (zeilen zeile spalte n result cum)
                 (if (and (= zeile 1) (= spalte 1)) cum
                     (if (= spalte 0) (loop zeilen (- zeile 1) 11 n result cum)

                         (if (raster-moving? (raster zeile spalte zeilen))
                             (loop zeilen zeile (- spalte 1) (+ n 1)
                                   (rasterwrite zeile spalte result (make-raster #f#f "white"))
                                   (make-cum (rasterwrite zeile spalte result (make-raster #f#f "white")) (if (= n 1) zeile (cum-1 cum))
                                             (if (= n 1) spalte (cum-11 cum))
                                             (if (= n 2) zeile (cum-2 cum))
                                             (if (= n 2) spalte (cum-22 cum))
                                             (if (= n 3) zeile (cum-3 cum))
                                             (if (= n 3) spalte (cum-33 cum))
                                             (if (= n 4) zeile (cum-4 cum))
                                             (if (= n 4) spalte (cum-44 cum))))
                             (loop zeilen zeile (- spalte 1) n result cum)))))))
      (loop (world-zeilen w) 24 11 1 (world-zeilen w) (make-cum (world-zeilen w) 0 0 0 0 0 0 0 0 )))))

; stein drehen
(define drehstein
  (lambda (w walt cum)
    (cond ((= (stein-art (world-stein w)) 1)

           (cond ((= (stein-dreher (world-stein w)) 1)
                  (make-langes2 (make-world (zeilen cum) (world-stein w) (world-tick w) (world-vars w)) (cum-1 cum) (+ (cum-44 cum) 1) walt #f))
                 ((= (stein-dreher (world-stein w)) 2)
                  (make-langes1 (make-world (zeilen cum) (world-stein w) (world-tick w)(world-vars w)) (cum-1 cum) (- (cum-44 cum) 1) walt #f))))
          ((= (stein-art (world-stein w)) 2)
           (cond ((= (stein-dreher (world-stein w)) 1)
                  (make-L12 (make-world (zeilen cum) (world-stein w) (world-tick w)(world-vars w)) (cum-1 cum) (+ (cum-44 cum) 1) walt #f))
                 ((= (stein-dreher (world-stein w)) 2)
                  (make-L13 (make-world (zeilen cum) (world-stein w) (world-tick w)(world-vars w)) (cum-1 cum) (- (cum-44 cum) 1) walt #f))
                 ((= (stein-dreher (world-stein w)) 3)
                  (make-L14 (make-world (zeilen cum) (world-stein w) (world-tick w)(world-vars w)) (cum-1 cum) (+ (cum-44 cum) 1) walt #f))
                 ((= (stein-dreher (world-stein w)) 4)
                  (make-L11 (make-world (zeilen cum) (world-stein w) (world-tick w)(world-vars w)) (cum-1 cum)  ( - (cum-44 cum) 1) walt #f))))
          ((= (stein-art (world-stein w)) 7)
           (cond ((= (stein-dreher (world-stein w)) 1)
                  (make-L22 (make-world (zeilen cum) (world-stein w) (world-tick w)(world-vars w)) (cum-1 cum) (+ (cum-44 cum) 1) walt #f))
                 ((= (stein-dreher (world-stein w)) 2)
                  (make-L23 (make-world (zeilen cum) (world-stein w) (world-tick w)(world-vars w)) (cum-1 cum) (cum-44 cum) walt #f))
                 ((= (stein-dreher (world-stein w)) 3)
                  (make-L24 (make-world (zeilen cum) (world-stein w) (world-tick w)(world-vars w)) (cum-1 cum) (- (cum-44 cum) 1) walt #f))
                 ((= (stein-dreher (world-stein w)) 4)
                  (make-L21 (make-world (zeilen cum) (world-stein w) (world-tick w)(world-vars w)) (cum-1 cum)  ( - (cum-44 cum) 1) walt #f))))
          ((= (stein-art (world-stein w)) 4)
           (cond ((= (stein-dreher (world-stein w)) 1)
                  (make-T2 (make-world (zeilen cum) (world-stein w) (world-tick w)(world-vars w)) (cum-1 cum) (cum-44 cum) walt #f))
                 ((= (stein-dreher (world-stein w)) 2)
                  (make-T3 (make-world (zeilen cum) (world-stein w) (world-tick w)(world-vars w)) (cum-1 cum) (cum-44 cum) walt #f))
                 ((= (stein-dreher (world-stein w)) 3)
                  (make-T4 (make-world (zeilen cum) (world-stein w) (world-tick w)(world-vars w)) (cum-1 cum)  (+ (cum-44 cum) 1) walt #f))
                 ((= (stein-dreher (world-stein w)) 4)
                  (make-T1 (make-world (zeilen cum) (world-stein w) (world-tick w)(world-vars w)) (cum-1 cum)   (cum-44 cum) walt #f))
                 ))
          ((= (stein-art (world-stein w)) 5)
           (cond ((= (stein-dreher (world-stein w)) 1)
                  (make-z12 (make-world (zeilen cum) (world-stein w) (world-tick w)(world-vars w) ) (cum-1 cum) (+ (cum-44 cum) 1) walt #f))
                 ((= (stein-dreher (world-stein w)) 2)
                  (make-z11 (make-world (zeilen cum) (world-stein w) (world-tick w)(world-vars w)) (cum-1 cum) (cum-44 cum) walt #f))))
          ((= (stein-art (world-stein w)) 6)
           (cond ((= (stein-dreher (world-stein w)) 1)
                  (make-z22 (make-world (zeilen cum) (world-stein w) (world-tick w) (world-vars w)) (cum-1 cum) (+ (cum-44 cum) 1) walt #f))
                 ((= (stein-dreher (world-stein w)) 2)
                  (make-z21 (make-world (zeilen cum) (world-stein w) (world-tick w) (world-vars w)) (cum-1 cum) (cum-44 cum) walt #f))
                 ))
          ((= (stein-art (world-stein w)) 3) walt )
          (else w))))


; ======= ende steinrumdrehen


(define zufallstein
  (lambda (w)

    (if #f w  ; ===== autovar
        (let ((zufall (random 7)))
          (cond ((= zufall 0) (make-langes1 w 4 5 w #t))
                ((= zufall 2) (make-L11 w 4 5 w #t))
                ((= zufall 3) (make-t1 w 4 6 w #t))
                ((= zufall 4) (make-l21 w 3 5 w #t))
                ((= zufall 5) (make-viereck w 3 5 w #t))
                ((= zufall 6) (make-z11 w 4 6 w #t))
                ((= zufall 1) (make-z21 w 4 6 w #t)) )))))

; world zeugs

(big-bang 220 440 0.05 start-world)

(on-redraw(lambda (w)
            (if (var-gameover? (world-vars w))
                (overlay
                 (overlay
                  (overlay
                   (make-alles w)
                   (text "Game Over lol" 20 "black") 25 200)
                  (text (number->string (var-punkte (world-vars w))) 40 "red")
                  (/ (- 220 (image-width (text (number->string (var-punkte (world-vars w))) 40 "red"))) 2) 50)
                 (text "N für neues Spiel" 15 "black") 40 400)
                (make-alles w)) ))

(on-tick-event
 (lambda (w)
   (if (not (var-pause? (world-vars w)))
       (if (>= (world-tick w) (- 0.8 (/ (var-punkte (world-vars w)) 2000)))
           (if (var-start? (world-vars w))
               (next-tick
                (make-world (world-zeilen w)
                            (world-stein w)
                            0
                            (make-var #f (var-pause? (world-vars w)) (var-punkte (world-vars w)) #t )))
               (if (var-gameover? (world-vars w))
                   (gameoverworld w)
                   (next-tick
                    (make-world
                     (world-zeilen w)
                     (world-stein w)
                     0
                     (make-var #f (var-pause? (world-vars w)) (var-punkte (world-vars w)) #f)))))
           (make-world (world-zeilen w) (world-stein w) (+ (world-tick w) 0.1) (world-vars w)))w)))

(on-key-event
 (Lambda (w str)
         (if (var-gameover? (world-vars w))
             (if (or (string=? str "n") (string=? str "N")) (zufallstein leer-world) w)
             (if  (var-start? (world-vars w)) (zufallstein leer-world)
                  (if (or (string=? str "p")(string=? str "P"))
                      (if (var-pause? (world-vars w))
                          (make-world (world-zeilen w)
                                      (world-stein w)
                                      (world-tick w)
                                      (make-var (var-gameover? (world-vars w)) #f (var-punkte (world-vars w))#f))
                          (make-world (world-zeilen w) (world-stein w) (world-tick w) (make-var (var-gameover? (world-vars w)) #t (var-punkte (world-vars w))#f)))
                      (if (and (not (var-gameover? (world-vars w))) (not (var-pause? (world-vars w))))
                          (cond
                            ((or (string=? str "s")(string=? str "S")) (next-tick w))
                            ((or (string=? str "w") (string=? str "W")) (drehstein w w (deletemoveables w)))
                            ((or (string=? str "a") (string=? str "A")) (if (leftable? w) (goleft! w)w))
                            ((or (string=? str "d")(string=? str "D"))  (if (rightable? w) (goright! w)w))
                            (else w))
                          w
                          ))))))

; 2007 by patrick brosi
