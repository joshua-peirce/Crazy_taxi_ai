;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ai) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

(define BLOCK "X")
(define BLANK "_")

(define BLOCKER-IMG (rectangle 30 30 "solid" "black"))
(define AGENT-IMG   (rectangle 30 30 "solid" "red"))

(define MUTATION-RATE 150)
(define MUTATION-FACTOR 0.8)

(define FOO 5)
;; since racket insists that all functions take at least one argument,
;; for any functions that shouldn't take any arguments, I pass them FOO, therefore:
;; A FOO is any argument that doesn't matter whatsoever and is just there to make BSL happy
;; Interpretation: A useless argument

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; MAPS

;; a Cell is one of:
;; - BLANK
;; - BLOCK
;; Interpretation: One cell on the 8x3 track

;; a Row is a (list Cell Cell Cell)
;; Interpretation: A row of the track, that moves up as a unit

;; make-car-row : Nat [0-6] -> Row
;; Make one row given there are blocks in it, placing blocks according to seed
;; the binary representation of see is the positions of the blockers
;; no 7, since that's 0b111 and the track would be impassable
(define (make-car-row seed)
  (list
   (if (> seed 3) BLOCK BLANK)
   (if (> (modulo seed 4) 1) BLOCK BLANK)
   (if (> (modulo seed 2) 0) BLOCK BLANK)))

(check-expect (make-car-row 0) (list BLANK BLANK BLANK)) ;; 000 : _ _ _
(check-expect (make-car-row 1) (list BLANK BLANK BLOCK)) ;; 001 : _ _ X
(check-expect (make-car-row 2) (list BLANK BLOCK BLANK)) ;; 010 : _ X _
(check-expect (make-car-row 3) (list BLANK BLOCK BLOCK)) ;; 011 : _ X X
(check-expect (make-car-row 4) (list BLOCK BLANK BLANK)) ;; 100 : X _ _
(check-expect (make-car-row 5) (list BLOCK BLANK BLOCK)) ;; 101 : X _ X
(check-expect (make-car-row 6) (list BLOCK BLOCK BLANK)) ;; 110 : X X _

;; make-row : Boolean -> Row
;; makes one row of the map. If cars? add cars to this row
;; otherwise just add a blank row
(define (make-row cars?)
  (cond
    [cars? (make-car-row (random 7))]
    [else (list BLANK BLANK BLANK)]))

;; make-map : foo -> Map
;; Add all cars on starting track
(define (make-map foo)
  (list
   (make-row #f)
   (make-row #f) ;; First 3 rows start empty
   (make-row #f)
   (make-row #t)
   (make-row #f)
   (make-row #t)
   (make-row #f)
   (make-row #t)))
;; a Map is a cons(cons BLOCK (cons BLANK ...))
;; a Map is a 8x3 list of lists of either BLOCK's or BLANK's
;; Interpretation: the visible map that the agent operates on

(define EX-MAP-1
  (list
   (make-row #f)
   (make-row #f)
   (make-row #f)
   (list BLANK BLOCK BLANK)
   (make-row #f)
   (list BLOCK BLOCK BLANK)
   (make-row #f)))

(define EX-MAP-2
  (list
   (make-row #f)
   (make-row #f)
   (list BLANK BLOCK BLANK)
   (make-row #f)
   (list BLOCK BLOCK BLANK)
   (make-row #f)
   (list BLOCK BLANK BLOCK)))

(define EX-MAP-3
  (list
   (make-row #f)
   (list BLANK BLOCK BLANK)
   (make-row #f)
   (list BLOCK BLOCK BLANK)
   (make-row #f)
   (list BLOCK BLANK BLOCK)
   (make-row #f)))

(define EX-MAP-EXP (cons
 (cons "_" (cons "_" (cons "_" '())))
 (cons
  (cons "_" (cons "_" (cons "_" '())))
  (cons
   (cons "_" (cons "_" (cons "_" '())))
   (cons
    (cons "_" (cons "X" (cons "_" '())))
    (cons
     (cons "_" (cons "_" (cons "_" '())))
     (cons
      (cons "X" (cons "X" (cons "_" '())))
      (cons (cons "_" (cons "_" (cons "_" '()))) '()))))))))

;; advance-map : List -> List
;; Pop last row and add new row to front of map
(define (advance-map map cars?)
  (reverse
   (rest
    (reverse
     (append
      (rest map)
      (list (make-row cars?) null))))))

(check-expect (advance-map EX-MAP-2 #f) EX-MAP-3)
;; can't check-expect for a row w/ cars as there is randomness involved

;; draw-map : Map -> Image
;; Draw pic of map
(define (draw-map map)
  (draw-map/acc map (rectangle 90 240 "solid" "white") 0)) ;; the rectangle is the solid white bg

;; draw-map/acc : partial-map Image Number -> Image
;; acc : the image accumulates more blockers as it moves
(define (draw-map/acc partial-map img i)
  (cond
    [(empty? partial-map) img]
    [else (draw-map/acc
           (rest partial-map)
           (add-row (first partial-map) img i) (+ i 1))]))

;; add-row : Row Image Nat [0-7] -> Image
;; draw a row into the accumulated image of the map
(define (add-row row img rownum)
  (add-single-obj
   (add-single-obj
    (add-single-obj img (string=? (first row) BLOCK) 0 (* rownum -40))
    (string=? (first (rest row)) BLOCK) -30 (* rownum -40))
   (string=? (first (rest (rest row))) BLOCK) -60 (* rownum -40)))

(check-expect (add-single-obj (rectangle 90 240 "solid" "white") #t 0 -40)
              (overlay/xy BLOCKER-IMG 0 -40 (rectangle 90 240 "solid" "white")))
(check-expect (add-single-obj (rectangle 90 240 "solid" "white") #f 0 -80)
              (rectangle 90 240 "solid" "white"))

;; add-single-obj : Image Boolean Nat Nat -> Image
;; adds a blocker image if that Cell is blocked
(define (add-single-obj img blocked? x y)
  (if blocked? (overlay/xy BLOCKER-IMG x y img) img))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TRACKS

;; A Track is an integer in the range [-1, 1]
;; Interpretaion: A track that a car could be on and collide with obstacles

(define LEFT -1)
(define MIDDLE 0)
(define RIGHT 1)

;; move-left : Track -> Track
;; moves to track on left, if legal
(define (move-left track)
  (if (= track -1) track (- track 1)))

;; move-right : Track -> Track
;; moves to track on right, if legal
(define (move-right track)
  (if (= track 1) track (+ track 1)))

(check-expect (try-move 0  LEFT) -1)
(check-expect (try-move -1 LEFT) -1)
(check-expect (try-move 0 RIGHT)  1)
(check-expect (try-move 1 RIGHT)  1)

;; try-move : Track Action -> Track
;; produces new tracknum in new position if legal
(define (try-move track act)
  (cond [(= act LEFT) (move-left track)]
        [(= act RIGHT) (move-right track)]
        [else track]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; NEURAL NETWORKS


;; A WeightLayer is a (list Double Double ... Double)
;; Interpretation: the vector the input layer is dotted with to produce one node in the output layer
;; before being passed to the activation function (inverse tan)

(define EX-WEIGHTLAYER-1 (list 1.394 4.061 8.136 7.903 8.487 5.625 -6.734 6.669 -0.334 9.195 -2.149))
(define EX-WEIGHTLAYER-2 (list 1.161 8.306 -0.422 -8.906 -9.733 -1.86 3.366 8.605 8.50 1.167 -9.252))
(define EX-WEIGHTLAYER-3 (list -7.537 -4.19 9.15 9.689 -8.43 -4.937 5.905 -4.43 -0.162 -3.21 -5.058))
(define EX-WEIGHTLAYER-4 (list 1.161 8.306 -0.422 -8.906 -9.733 -1.867 3.36 8.605 8.502 1.167 -9.25))
(define EX-WEIGHTLAYER-5 (list -2.954 0.425 3.319 -7.27 9.232 0.862 -6.551 9.366 -3.729 5.526 6.421))

(define EX-WEIGHTLAYER-6 (list -1.847 8.409 6.63 -6.812 5.041 -9.054))
(define EX-WEIGHTLAYER-7 (list 5.479 -8.734 0.282 0.712 3.885 -3.201))
(define EX-WEIGHTLAYER-8 (list -1.973 -6.693 2.743 7.865 0.678 4.366))

;; weightlayer-template : WeightLayer -> ???
;; template function for WeightLayer
(define (weightlayer-template wl)
  (cond
    [(empty? wl) ...]
    [(cons? wl) (... (first wl) ... (WEIGHTLAYER-template (rest wl)))]))


;; a Weight is a (list WeightLayer WeightLayer ... WeightLayer)
;; Interpretation: the matrix the input layer is multiplied by to produce the output layer before
;; that is passed to the activation function (inverse tan)

(define EX-WEIGHT-1
  (list EX-WEIGHTLAYER-1 EX-WEIGHTLAYER-3 EX-WEIGHTLAYER-5 EX-WEIGHTLAYER-2 EX-WEIGHTLAYER-4))
(define EX-WEIGHT-2
  (list EX-WEIGHTLAYER-4 EX-WEIGHTLAYER-3 EX-WEIGHTLAYER-2 EX-WEIGHTLAYER-1 EX-WEIGHTLAYER-5))
(define EX-WEIGHT-3
  (list EX-WEIGHTLAYER-1 EX-WEIGHTLAYER-4 EX-WEIGHTLAYER-3 EX-WEIGHTLAYER-5 EX-WEIGHTLAYER-2))

(define EX-WEIGHT-4
  (list EX-WEIGHTLAYER-6 EX-WEIGHTLAYER-7 EX-WEIGHTLAYER-8))
(define EX-WEIGHT-5
  (list EX-WEIGHTLAYER-8 EX-WEIGHTLAYER-6 EX-WEIGHTLAYER-7))
(define EX-WEIGHT-6
  (list EX-WEIGHTLAYER-7 EX-WEIGHTLAYER-8 EX-WEIGHTLAYER-6))

;; weight-template : Weight -> ???
;; template function for Weight
(define (weight-template w)
  (cond
    [(empty? w) ...]
    [(cons? w) (... (first w) ... (weight-template (rest w)))]))


;; an InputLayer is a (list Double Double Double ... Double)
;; Interpretation: The input vector to the NN
;; Input formatted as:
;; (append (list 1 (agent-track)) (row 1) (row 2) (row 3))
;; where (row 1) is a list of ints 0-1, 0 if empty, 1 if blocked

(define EX-INPUT-LAYER-1 (list 1 -1 0 0 0 0 0 0 0 0 0))
(define EX-INPUT-LAYER-2 (list 1 1 0 1 1 0 0 0 1 1 0))
(define EX-INPUT-LAYER-3 (list 1 0 0 1 0 0 0 0 0 0 1))

;; input-layer-template : InputLayer -> ???
;; template function for InputLayer
(define (input-layer-template il)
  (cond
    [(empty? il) ...]
    [(cons? il) (... (first il) ... (input-layer-template (rest il)))]))


;; an ActivationVector is one of :
;; - (list Double Double ... Double)
;; - InputLayer
;; Interpretation: Vector returned after transforming and rotating
;; an InputLayer or another ActivationLayer
;; note: XActivationLayer is an ActivationLayer of length X

;; activation-layer-template : ActivationLayer -> ???
;; template function for ActivationLayer
(define (activation-layer-template il)
  (cond
    [(empty? il) ...]
    [(cons? il) (... (first il) ... (activation-layer-template (rest il)))]))


(define-struct nn [layer-1-weights layer-2-weights])
;; A Neural Network is a (make-nn layer-1-weights layer-2-weights)
;; Both layers are of the following form:
;; (list (list w11 w12 w13 ... w1n) (list w21 w22 w23 ... w2n) ... (list wm1 wm2 wm3 ... wmn))
;; where w12 refers to the weight of the second input neuron on the first output neuron
;; layer 1 must take 11 inputs, and give 5 outputs (w11 -> wB5)
;; layer 2 must take 6 inputs,  and give 3 outputs (w11 -> w63)

(define EX-NN-1 (make-nn EX-WEIGHT-1 EX-WEIGHT-4))
(define EX-NN-2 (make-nn EX-WEIGHT-2 EX-WEIGHT-5))
(define EX-NN-3 (make-nn EX-WEIGHT-3 EX-WEIGHT-6))

;; neural-network-template : Neural Network -> ???
;; template function for Neural Networks
(define (neural-network-template n)
  (... (weights-template (nn-layer-1-weights n)) ...
       (weights-template (nn-layer-2-weights n)) ...))


;; A MutationRate is an integer [0 - 1000]
;; Interpretation: the chance of mutation for EVERY weight out of 1000


;; A MutationFactor is a double
;; Interpretation: factor every mutation in a weight is scaled by.
;; A MutationFactor is generally between 0 and 2 but it could be bigger if you wish


;; new-net : Foo -> Neural Network
;; Make neural network from scratch (Foo does nothing)
(define (new-nn foo)
  (make-nn (random-weights 5 11) (random-weights 3 6)))

(check-expect (weight-shape EX-WEIGHT-1) "11x5")
(check-expect (weight-shape EX-WEIGHT-4) "6x3")

;; weight-shape : Weight -> String
;; return the shape of the weights, assuming non-jagged weights matrix
(define (weight-shape w)
  (string-append
   (number->string
    (length
     (first w)))
   "x"
   (number->string
    (length
     w))))

;; random-weights : Nat Nat -> Nested-List
;; generates a set of random weights of input size m and output size n
(define (random-weights m n)
  (random-weights-input/acc m n 0))

;; random-weights-input/acc : Nat Nat Nat Weight -> Weight
;; acc: accumulate the partial weight 
;; recursively accumulates a random set of weights
(define (random-weights-input/acc m n i)
  (if (= i m) null
      (cons
       (random-weights-output/acc n 0)
       (random-weights-input/acc m n (+ 1 i)))))

;; random-weights-output/acc Nat Nat WeightLayer -> WeightLayer
;; acc: accumulate the partial WeightLayer
;; accumulates random weight for one output node.
(define (random-weights-output/acc n i)
  (if
   (= i n)
   null
   (cons (- (/ (random 20000) 1000) 10) (random-weights-output/acc n (+ 1 i)))))

;; checking directly won't work because of the random call, but we can
;; ensure at least the shapes of the input and output matrices match
(check-expect
 (weight-shape (nn-layer-1-weights (child-net EX-NN-1 100 0.3)))
 (weight-shape (nn-layer-1-weights EX-NN-1)))

(check-expect
 (weight-shape (nn-layer-2-weights (child-net EX-NN-1 1000 10)))
 (weight-shape (nn-layer-2-weights EX-NN-1)))

;; child-net : Neural-Network MutationRate MutationFactor -> Neural-Network
;; make new nn after mutation
(define (child-net net mr mf)
  (make-nn (child-net-mutate-weights/acc (nn-layer-1-weights net) mr mf)
           (child-net-mutate-weights/acc (nn-layer-2-weights net) mr mf)))

(check-expect
 (length (child-net-mutate-weights/acc EX-WEIGHT-1 100 0.3))
 (length EX-WEIGHT-1))

(check-expect
 (length (child-net-mutate-weights/acc EX-WEIGHT-4 100 0.3))
 (length EX-WEIGHT-4))

;; child-net-mutate-weights/acc : Weight MutationRate MutationFactor -> Weight
;; recursively pass each row of input Weights to mutate-row along with mutation rate + factor
(define (child-net-mutate-weights/acc remaining mr mf)
  (if (empty? remaining) null
      (cons
       (child-net-mutate-layer/acc (first remaining) mr mf)
       (child-net-mutate-weights/acc (rest remaining) mr mf))))

(check-expect
 (length (child-net-mutate-layer/acc EX-WEIGHTLAYER-1 1000 10))
 (length EX-WEIGHTLAYER-1))

(check-expect
 (length (child-net-mutate-layer/acc EX-WEIGHTLAYER-4 1000 10))
 (length EX-WEIGHTLAYER-4))

;; child-net-mutate-row/acc : WeightLayer MutationRate MutationFactor -> Row
;; recursively pass each element of Weightlayer vector to mutate-weight-element
(define (child-net-mutate-layer/acc layer mr mf)
  (if (empty? layer) null
      (cons
       (mutate-weight-element (first layer) mr mf)
       (child-net-mutate-layer/acc
        (rest layer) mr mf))))

(check-expect (mutate-weight-element 5 1000 0) 5)
(check-expect (mutate-weight-element 5 0 100) 5)

;; mutate-weight-element : Double MutationRate MutationFactor -> Double
;; potentially mutates a single weight
(define (mutate-weight-element orig mr mf)
  (if (< (random 1000) mr)
      (+ orig (* 0.001 mf (- (random 2000) 1000)))
      orig))

(check-expect (get-action-from-nn-outputs
               (cons #i-0.9176592583529232
                     (cons #i-0.9103901442382379
                           (cons #i0.03855964061635822 '())))) 1)
(check-expect (get-action-from-nn-outputs
               (cons 0.6
                     (cons 0.3
                           (cons -0.9 '())))) -1)
               
;; get-action-from-nn-outputs : 3ActivationLayer -> Action
;; convert outputs of last layer of nn to an Action
(define (get-action-from-nn-outputs outputs)
  (if (> (first outputs) (first (rest outputs)))
      (if (> (first outputs) (first (rest (rest outputs))))
          -1 1)
      (if (> (first (rest outputs)) (first (rest (rest outputs))))
          0 1)))  

(check-expect
 (get-nn-inputs-from-map (make-agent EX-NN-1 -1 0 #t #t) EX-MAP-EXP)
 EX-INPUT-LAYER-1)

;; get-nn-inputs-from-map : Agent Map -> InputLayer
;; convert map into inputs of nn:
;; Biased (1), Agent's track,
;; First 3 rows (3x3) of the track.
(define (get-nn-inputs-from-map a map)
  (cons
   1
   (cons
    (agent-track a)
    (conv-map-to-inputs
     (append
      (first map)
      (first (rest map))
      (first (rest (rest map))))))))

(check-expect
 (conv-map-to-inputs
  (list "_" "_" "_" "X" "_" "X" "_" "_" "_"))
 (list 0 0 0 1 0 1 0 0 0))

(check-expect
 (conv-map-to-inputs
  (list "X" "_" "_" "_" "_" "_" "_" "_" "_"))
 (list 1 0 0 0 0 0 0 0 0))

;; conv-map-to-inputs : Map -> InputLayer
;; convert a map into the last 9 inputs of an input layer
(define (conv-map-to-inputs m)
  (if (empty? m) null
      (cons (if (string=? (first m) "X") 1 0) (conv-map-to-inputs (rest m)))))

;; Since these work, no need to test the subfunctions
(check-expect (compute-action (make-agent EX-NN-1 -1 0 #t #t) EX-MAP-EXP) 0)
(check-expect (compute-action (make-agent EX-NN-2  1 0 #t #t) EX-MAP-EXP) 1)

;; compute-action : Agent Map -> Action
;; Compute action
(define (compute-action a map)
  (get-action-from-nn-outputs
   (rest
    (apply-layer
     (apply-layer
      (get-nn-inputs-from-map a map)
      (nn-layer-1-weights (agent-nn a)))
     (nn-layer-2-weights (agent-nn a))))))

;; apply-layer : ActivationLayer Weight -> ActivationLayer
;; Apply one layer of a neural net, given an input (padded with a biased node) and a set of weights
(define (apply-layer input-activations weights)
  (cons 1 (comp-layer/acc input-activations weights null)))

;; comp-layer/acc : ActivationLayer Weight -> ActivationLayer
;; accumulates neuron activations
(define (comp-layer/acc input-activations remaining-weights outputs)
  (if (empty? remaining-weights) outputs
      (comp-layer/acc input-activations (rest remaining-weights)
                      (cons (comp-neuron input-activations (first remaining-weights)) outputs))))

;; comp-neuron : InputLayer WeightLayer -> double
;; 2/pi times arctan of dot product of weights and activations
(define (comp-neuron input-activations weights)
  (* 0.606619772367581 (atan (sum-neuron/acc input-activations weights))))

;; sum-neuron/acc InputLayer WeightLayer -> double
;; accumulator function for computing the dot product
(define (sum-neuron/acc remaining-activations remaining-weights)
  (if (empty? remaining-activations) 0
      (+ (* (first remaining-weights) (first remaining-activations))
         (sum-neuron/acc
          (rest remaining-activations)
          (rest remaining-weights)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; AGENTS

(define-struct agent [nn track score alive? showing?])
;; An Agent is a (make-agent nn track score alive? showing?)
;; nn is the Neural Network that controls the agent
;; track is the track the agent's car is on
;; score is the number of rows this agent has been alive for
;; alive? is if the agent is still alive for this generation
;; showing? is if this agent should be drawn on the image of the world

(define EX-AGENT-1  (make-agent EX-NN-1 1 10 #t #t))
(define EX-AGENT-2  (make-agent EX-NN-1 -1 10 #t #f))
(define EX-AGENT-3  (make-agent EX-NN-1 0 10 #f #t))
(define EX-AGENT-4  (make-agent EX-NN-1 0 10 #f #f))
(define EX-AGENT-5  (make-agent EX-NN-1 1 10 #t #f))
(define EX-AGENT-6  (make-agent EX-NN-1 -1 1000 #t #f))
(define EX-AGENT-7  (make-agent EX-NN-1  0 1000 #t #f))
(define EX-AGENT-8  (make-agent EX-NN-2 1 0 #t #t))
(define EX-AGENT-9  (make-agent EX-NN-1 0 0 #t #t))
(define EX-AGENT-10 (make-agent EX-NN-1 0 0 #t #f))
(define EX-AGENT-11 (make-agent EX-NN-3 1 1 #t #t))
(define EX-AGENT-12 (make-agent EX-NN-3 0 1 #t #t))
(define EX-AGENT-13 (make-agent EX-NN-1 1 10 #t #t))
(define EX-AGENT-14 (make-agent EX-NN-1 -1 10 #t #f))
(define EX-AGENT-15 (make-agent EX-NN-1 1 10.5 #t #t))
(define EX-AGENT-16 (make-agent EX-NN-1 -1 10.5 #t #f))

;; new-agent : Foo -> Agent
;; make a blank agent from defaults; defaults to a visible agent and middle track
(define (new-agent foo)
  (make-agent (new-nn foo) 0 0 #t #t))

(check-expect (decide-and-execute EX-AGENT-6 EX-MAP-EXP) EX-AGENT-6) ;; shouldn't move
(check-expect (decide-and-execute EX-AGENT-8 EX-MAP-EXP) EX-AGENT-8) ;; should move right -> not move
(check-expect (decide-and-execute EX-AGENT-11 EX-MAP-EXP) EX-AGENT-12) ;; should move letf

;; decide-and-execute: Agent Map -> Agent
;; pass map info to nn, decide where to move to, and move there (if legal)
(define (decide-and-execute a map)
  (make-agent-move
   a
   (compute-action a map)))

(check-expect (make-agent-dead EX-AGENT-1) EX-AGENT-3)
(check-expect (make-agent-dead EX-AGENT-2) EX-AGENT-4)

;; make-agent-dead : Agent -> Agent
;; makes a new agent with same properties except it's dead
(define (make-agent-dead a)
  (make-agent
   (agent-nn a)
   0 ;; Agents always start in the middle
   (agent-score a)
   #f ;; agent is not alive
   (agent-showing? a)))

(check-expect (make-agent-alive EX-AGENT-3) EX-AGENT-9)
(check-expect (make-agent-alive EX-AGENT-4) EX-AGENT-10)

;; make-agent-alive : Agent -> Agent
;; makes a new agent with same properties except it's alive
(define (make-agent-alive a)
  (make-agent
   (agent-nn a)
   0 ;; Agents always start in the middle
   0 ;; Agents start from 0 score when coming back to life
   #t ;; agent is not alive
   (agent-showing? a)))

(check-expect (make-agent-move EX-AGENT-6 RIGHT) EX-AGENT-7)
(check-expect (make-agent-move EX-AGENT-11 LEFT) EX-AGENT-12)

;; make-agent-move : Agent Action -> Agent
;; produce copy of agent after taking action act
(define (make-agent-move a act)
  (make-agent
   (agent-nn a)
   (try-move (agent-track a) act)
   (agent-score a)
   (agent-alive? a)
   (agent-showing? a)))

(check-expect (increment-score EX-AGENT-13) EX-AGENT-15)
(check-expect (increment-score EX-AGENT-14) EX-AGENT-16)

;; increment-score : Agent -> Agent
;; increases agent score by 0.5 (every two rows + 1)
(define (increment-score a)
  (make-agent
   (agent-nn a)
   (agent-track a)
   (+ 0.5 (agent-score a))
   (agent-alive? a)
   (agent-showing? a)))

;; child-agent : Agent Mutation-rate Mutation-factor -> Agent
(define (child-agent a mr mf)
  (make-agent
   (child-net (agent-nn a) mr mf)
   0 0 #t #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; WORLDS

(define-struct world [map
                      agents
                      alive-agents
                      dead-agents
                      rownum
                      total-agents
                      num-alive-agents
                      highscores])
;; a World is a (make-world Map [List-of Agents] [List-of Agents] [List-of Agents] Nat Nat Nat Nat)
;; Interpretation: representation of all data in the world.
;; - map is the map of the world
;; - agents is the list of agents in the population
;; - alive-agents is the list of agents who are alive in the population
;; - dead-agents is the list of agents who are dead in the population
;; - rownum is the number of rows elapsed
;; - total-agents is the total number of agents in the population
;; - num-alive-agents is the number of agents in this generation still alive
;; - highscores is a history of all the high scores for the simulation.

(define WORLD1 (make-world-with-agents (make-agents 1) 1))
(define WORLD2 (make-world-with-agents (list EX-AGENT-1 EX-AGENT-2 EX-AGENT-3) 3))
(define WORLD3 (make-world-with-agents (list EX-AGENT-1 EX-AGENT-3 EX-AGENT-4 EX-AGENT-5) 4))

;; template
(define (world-template w)
  (... (world-map w) ...
       (world-agents w) ...
       (world-alive-agents w) ...
       (world-dead-agents w) ...
       (world-rownum w) ...
       (world-total-agents w) ...
       (world-num-alive-agents w) ...
       (world highscores w) ...))

;; use (length l) instead
;; (define (count l)
  ;; (if (empty? l) 0 (+ 1 (count (rest l)))))

;; tick-world : World -> World
;; update entire world; todo above
(define (tick-world w)
  (if (= (world-num-alive-agents w) 0)  (next-gen w 300 1.5)
      (increment-world-score
       (check-and-handle-collisions
        (alive-agent-move
         (advance-world-map w))))))