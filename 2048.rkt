;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname a11) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)
(require 2htdp/universe)

; join-together : [ListOf X] [ListOf X] -> [ListOf X]
; (join-together ls1 ls2) returns a list consisting of the top-level
; elements of ls1 followed by the top-level elements of ls2
(define (join-together ls1 ls2)
  (local 
    [; help : [ListOf X] -> [ListOf X]
     ; (help ls1) drops the invariant argument ls2
     (define (help ls1)
       (cond
         [(empty? ls1) ls2]
         [else (cons (first ls1) 
                     (help (rest ls1)))]))]
    (help ls1)))

(check-expect (join-together '() '(a b c d e f)) '(a b c d e f))
(check-expect (join-together '(a)  '(b c d e f)) '(a b c d e f))
(check-expect (join-together '(a b)  '(c d e f)) '(a b c d e f))
(check-expect (join-together '(a b c ) '(d e f)) '(a b c d e f))
(check-expect (join-together '(a b c d)  '(e f)) '(a b c d e f))
(check-expect (join-together '(a b c d e)  '(f)) '(a b c d e f))
(check-expect (join-together '(a b c d e f) '()) '(a b c d e f))

; flatten/v1 : [ListOf [ListOf X]] -> [ListOf X]
; (flatten/v1 lls) returns the result of appending together all the
; sublists in lls
(define (flatten/v1 lls)
  (cond
    [(empty? lls) '()]
    [else (join-together (first lls)
                         (flatten/v1 (rest lls)))]))

; flatten : [ListOf [ListOf X]] -> [ListOf X]
; (flatten lls) returns the result of appending together all the
; sublists in lls
(define flatten flatten/v1)
(check-expect (flatten '((a) (b) (c) (d) (e) (f)))
              '(a b c d e f))
(check-expect (flatten '((a b) (c d) (e f) (g h) (i j)))
              '(a b c d e f g h i j))
(check-expect (flatten '(() () () () () () ())) '())
(check-expect (flatten '(() (a) (b c) (d e f) (g h i j)))
              '(a b c d e f g h i j))

; list-head : Nat [ListOf X] -> [ListOf X]
; (list-head n ls) returns the first n elements of ls,
; signaling an error if n is too large for ls
(define (list-head init-n init-ls)
  (local 
    [; help : Nat [ListOf X] -> [ListOf X]
     ; (help n ls) runs the recursion on n and ls, preserving
     ; the initial values for the error message
     (define (help n ls)
       (cond
         [(zero? n) '()]
         [(empty? ls) 
          (error 'list-head 
                 (format "~s is too large for ~s" 
                         init-n init-ls))]
         [else 
          (cons (first ls)
                (help (sub1 n) (rest ls)))]))]
    (help init-n init-ls)))
(check-expect (list-head 0 '(a b c)) '())
(check-expect (list-head 1 '(a b c)) '(a))
(check-expect (list-head 2 '(a b c)) '(a b))
(check-expect (list-head 3 '(a b c)) '(a b c))
(check-error (list-head 4 '(a b c)) 
             "list-head: 4 is too large for (a b c)")

; list-tail : Nat [ListOf X] -> [ListOf X]
; (list-tail n ls) returns the result applying rest to ls n times
(define (list-tail init-n init-ls)
  (local 
    [; help : Nat [ListOf X] -> [ListOf X]
     ; (help n ls) runs the recursion on n and ls, preserving
     ; the initial values for the error message
     (define (help n ls)
       (cond
         [(zero? n) ls]
         [(empty? ls) 
          (error 'list-tail 
                 (format "~s is too large for ~s"
                         init-n init-ls))]
         [else 
          (help (sub1 n) (rest ls))]))]
    (help init-n init-ls)))
(check-expect (list-tail 0 '(a b c)) '(a b c))
(check-expect (list-tail 1 '(a b c)) '(b c))
(check-expect (list-tail 2 '(a b c)) '(c))
(check-expect (list-tail 3 '(a b c)) '())
(check-error (list-tail 4 '(a b c)) 
             "list-tail: 4 is too large for (a b c)")

; A PosInt is an Int in [1..infinity)
; pop-up : PosInt [ListOf X] -> [ListOf [ListOf X]]
; (pop-up n ls) returns the result of grouping the elements of ls
; into sublists of size n, assuming n divides the length of ls
(define (pop-up n ls)
  (local
    [; help : [ListOf X] -> [ListOf [ListOf X]]
     ; help drops the invariant argument n
     (define (help ls)
       (cond
         [(empty? ls) '()]
         [else (cons (list-head n ls)
                     (help (list-tail n ls)))]))]
    (help ls)))
(check-expect (pop-up 1 '(a b c d e f g))
              '((a) (b) (c) (d) (e) (f) (g)))
(check-expect (pop-up 2 '(a b c d e f g h))
              '((a b) (c d) (e f) (g h)))
(check-expect (pop-up 3 '(a b c d e f g h i))
              '((a b c) (d e f) (g h i)))
(check-expect (pop-up 4 '(a b c d e f g h))
              '((a b c d) (e f g h)))
(check-expect (pop-up 5 '(a b c d e))
              '((a b c d e)))

; overwrite : [ListOf X] Nat X -> [ListOf X]
; (overwrite ls i x) returns the result of replacing the i-th
; element of ls with x, signaling an error if n is out of
; bounds for ls
(define (overwrite orig-ls orig-i x)
  (local
    [; help : [ListOf X] Nat -> [ListOf X]
     ; help drops the invariant argument x and preserves
     ; the initial values for the error message
     (define (help ls i)
       (cond
         [(empty? ls) (error 'overwrite
                             (format "~s is out of bounds for ~s"
                                     orig-i orig-ls))]
         [(zero? i) (cons x (rest ls))]
         [else 
          (cons (first ls)
                (help (rest ls) (sub1 i)))]))]
    (help orig-ls orig-i)))
(check-expect (overwrite '(a) 0 'b) '(b))
(check-expect (overwrite '(a b c) 0 '_) '(_ b c))
(check-expect (overwrite '(a b c) 1 '_) '(a _ c))
(check-expect (overwrite '(a b c) 2 '_) '(a b _))
(check-expect (overwrite '(x x x _ x x x x) 3 'x) (make-list 8 'x))
(check-error (overwrite '(a b c) 4 'd) 
             "overwrite: 4 is out of bounds for (a b c)")
(check-error (overwrite '(a b c) -3 'd) 
             "overwrite: -3 is out of bounds for (a b c)")

#|
  https://gabrielecirulli.github.io/2048/

  2048 Game
  - a 2D sliding tile puzzle game where tiles are labeled with 
    numbers of the form 2^i for i = 1, 2, ..., 11
  - Use the arrow keys to slide the tiles in the indicated direction
  - When two tiles with the same number, 2^i, touch, they merge into
    one tile numberd 2^(i+1)
  - "Non-greedy" movement. The tiles that were created by combining 
    other tiles should not be combined again during the same turn 
    (move). That is to say that moving the tile row of
    2 2 2 2 to the right should result in _ _ 4 4, not _ _ _ 8.
|#

;;;;;;;;;;;;;;;
;; The Model ;;
;;;;;;;;;;;;;;;
#|
   A PowerOfTwo is one of
   - 1
   - (* 2 PowerOfTwo)

   A TileValue is one of
   - '_
   - a PowerOfTwo in [2..2048]
 
   A Row is one of
   - (cons TileValue '())
   - (cons TileValue Row)

   A Board is one of
   - (cons Row '())
   - (cons Row Board)
|#

; We represent the blank with '_
(define BLANK '_)
(check-expect BLANK '_)

; blank? : X -> Bool
; (blank? x) returns #true iff x is the blank space
(define (blank? x)
  (equal? x BLANK))
(check-satisfied BLANK blank?)
(check-expect (blank? " ") #false)
(check-expect (blank? 'blank) #false)

; Three sample boards
(define b1 '((64 32) (16 16)))

(define b2
  (list
   (list 2 2 2 2)
   (list 4 '_ 4 '_)
   (list '_ 8 8 8)
   (list 16 '_ '_ 16)))

(define b3
  (list
   (list 16 64 8 256 4)
   (list 1024 1024 1024 32 128)
   (list 64 32 128 '_ '_)
   (list 4 4 32'_ '_)
   (list 2 '_ '_ 512 '_)))

; board-full? : Board -> Bool
; (board-full? board) returns #true iff board contains no
; blank tiles 
(define (board-full? board)
  (empty? (filter blank? (flatten board))))

; add-new-tile : Board -> Board
; (add-new-tile board) returns the board with one of the existing blank
; tiles replaced by either a 2 or a 4. The blank tile to replace is
; selected at random and with equal probability. A 2 replaces the blank
; tile 80% of the time. If there's no room on the board for a new tile,
; the board is returned unchanged.
(define (add-new-tile board)
  (if (board-full? board)
      board
      (local 
        [(define n (length (first board)))
         (define flat-board (flatten board))
         (define num-tiles (* n n))
         (define new-tile-val (if (zero? (random 5)) 4 2))
         ; try : Nat -> Nat
         ; (try i) returns i if the element at index i in flat-board
         ; is a blank, otherwise it tries a new random index
         (define (try i)
           (if (blank? (list-ref flat-board i))
               i
               (try (random num-tiles))))]
        (pop-up n (overwrite flat-board 
                             (try (random num-tiles))
                             new-tile-val)))))

; iterate : [X -> X] Nat X -> X
; (iterate f n x) returns the result of applying f n times, starting
; with x, and then cycling resulting values through f, 
; i.e., (f (f (... (f x))))
(define (iterate f n x)
  (local
    [; help : Nat X -> X
     ; (help n x) drops the invariant argument f
     (define (help n x)
       (cond
         [(zero? n) x]
         [else (help (sub1 n) (f x))]))]
    (help n x)))
(check-expect (iterate add1 0 "no problem") "no problem")
(check-expect (iterate string-length 1 "hello") 5)
(check-expect (iterate not 17 #true) #false)
(check-expect (iterate add1 5 2) (add1 (add1 (add1 (add1 (add1 2))))))
(check-expect (iterate rest 3 '(a b c d e f g))
              '(d e f g))
(check-expect (iterate sqr 4 2) (sqr (sqr (sqr (sqr 2)))))

; make-board : PosInt Nat -> Board
; (make-board n) returns a new nxn board with m initial
; non-blank tiles
(define (make-board n m)
  (iterate add-new-tile 
           m
           (make-list n (make-list n BLANK))))
(check-expect (length (flatten (make-board 10 40))) (* 10 10))
(check-expect (length (filter blank?
                              (flatten (make-board 10 40))))
              (- (* 10 10) 40))

; game-won? : Board -> Bool
; (game-won? board) returns #true iff board contains a 2048 tile
(define (game-won? board)
  (member? 2048 (flatten board)))

;;;;;;;;;;;;;;
;; The View ;;
;;;;;;;;;;;;;;
(define TILE-SIZE 120)
(define FONT-SIZE (quotient TILE-SIZE 2))
(define GRID-SPACING (quotient TILE-SIZE 20)) 
(define GRID-COLOR (make-color 186 172 160))

#|
  A RGB is an integer in the range [0..255]

  A Color is one of
  - Symbol
  - String
  - (make-color RGB RGB RGB)

  A Tile is a (make-tile TileValue Nat Color Color)
|#

(define-struct tile [val font fg bg])

(define the-tiles
  (list 
   ;;         Tile      Font      Foreground         Background
   ;;         Value     Size         Color              Color
   (make-tile BLANK   FONT-SIZE    'dimgray  (make-color 204 192 179))
   (make-tile    2    FONT-SIZE    'dimgray  (make-color 238 228 218))
   (make-tile    4    FONT-SIZE    'dimgray  (make-color 237 224 200))
   (make-tile    8    FONT-SIZE     'white   (make-color 242 177 121))
   (make-tile   16    FONT-SIZE     'white   (make-color 245 149  99))
   (make-tile   32    FONT-SIZE     'white   (make-color 246 124  95))
   (make-tile   64    FONT-SIZE     'white   (make-color 246  94  59))
   (make-tile  128 (- FONT-SIZE 4)  'white   (make-color 237 207 114))
   (make-tile  256 (- FONT-SIZE 4)  'white   (make-color 237 204  97))
   (make-tile  512 (- FONT-SIZE 4)  'white   (make-color 237 200  80))
   (make-tile 1024 (- FONT-SIZE 8)  'white   (make-color 237 197  63))
   (make-tile 2048 (- FONT-SIZE 8)  'white   (make-color 237 194  46))))

; tile->image : TileValue Nat Color Color -> Image
; (tile->image tile-val font-size foreground-color background-color) 
; returns the image corresponding to a tile with the given properties
(define (tile->image tile-val
                     font-size 
                     foreground-color
                     background-color)
  (local
    [(define back-image (overlay 
                         (square TILE-SIZE 'solid background-color)
                         (square (+ TILE-SIZE (* 2 GRID-SPACING))
                                 'solid GRID-COLOR)))]
    (if (blank? tile-val)
        back-image
        (overlay 
         (text (format "~s" tile-val) font-size foreground-color)
         back-image))))

; val->image : TileVal -> Image
; (val->image val) returns a square, properly colored, image labeled
; with val
(define (val->image val)
  (local
    [(define tile (val->tile val))]
    (tile->image (tile-val tile)
                 (tile-font tile)
                 (tile-fg tile)
                 (tile-bg tile))))

; val->tile : TileValue -> Tile
; (val->tile val) returns the tile associated with val in the-tiles.
; (This is a utility function used by val->image.)
(define (val->tile val)
  (local 
    [; lookup : [ListOf Tile] -> Tile
     ; (lookup tiles) searches tiles for the one matching val
     (define (lookup tiles)
       (cond
         [(empty? tiles)
          (error 'val->tile
                 (format "unknown tile value ~s" val))]
         [(equal? val (tile-val (first tiles))) (first tiles)]
         [else
          (lookup (rest tiles))]))]
    (lookup the-tiles)))
(check-satisfied (val->tile 2048) tile?)
(check-expect (val->tile BLANK) (first the-tiles))
(check-expect (val->tile 2) (second the-tiles))
(check-expect (val->tile 4) (third the-tiles))
(check-expect (val->tile 8) (fourth the-tiles))
(check-error (val->tile 3) "val->tile: unknown tile value 3")

; board->image : Board -> Image
; (board->image board) returns an image corresponding to the given 
; board
(define (board->image board)
  (local 
    [(define n (length (first board)))
     (define back-image
       (square (+ (* n TILE-SIZE) (* 2 (add1 n) GRID-SPACING))
               'solid GRID-COLOR))]
    (overlay
     (local
       [; build-row-image : [ListOf Image] -> Image
        ; (build-row-image tile-images) returns the result of joining  
        ; all the tile-images for a single row into one image
        (define (build-row-image tile-images)
          (foldr beside empty-image tile-images))
        ; build-board-image : [ListOf Image] -> Image
        ; (build-board-image row-images) returns the result of stacking
        ; all the images in row-images into a column
        (define (build-board-image row-images)
          (foldr above empty-image row-images))]
       (build-board-image 
        (map build-row-image (board-map val->image board))))  
     back-image)))

; board-map : [TileValue -> X] Board -> [ListOf [ListOf X]]
; (board-map fun board) returns the result of mapping fun over
; the board. (This is a utility function used by board->image.)
(define (board-map fun board)
  (local [(define n (length (first board)))]
    (pop-up n (map fun (flatten board)))))
(check-expect (slide-row-left '()) '())
(check-expect (slide-row-left '(_)) (list '_))
(check-expect (slide-row-left '(2)) (list 2))
(check-expect (slide-row-left '(_ _)) (list '_ '_))
(check-expect (slide-row-left '(2 _)) (list 2 '_))
(check-expect (slide-row-left '(_ 2)) (list 2 '_))
(check-expect (slide-row-left '(2 2)) (list 4 '_))
(check-expect (slide-row-left '(2 4)) (list 2 4))
(check-expect (slide-row-left '(4 4)) (list 8 '_))
(check-expect (slide-row-left '(_ _ _)) (list '_ '_ '_))
(check-expect (slide-row-left '(2 _ _)) (list 2 '_ '_))
(check-expect (slide-row-left '(_ 2 _)) (list 2 '_ '_))
(check-expect (slide-row-left '(_ _ 2)) (list 2 '_ '_))
(check-expect (slide-row-left '(2 2 _)) (list 4 '_ '_))
(check-expect (slide-row-left '(2 _ 2)) (list 4 '_ '_))
(check-expect (slide-row-left '(_ 2 2)) (list 4 '_ '_))
(check-expect (slide-row-left '(2 4 _)) (list 2 4 '_))
(check-expect (slide-row-left '(2 _ 4)) (list 2 4 '_))
(check-expect (slide-row-left '(_ 2 4)) (list 2 4 '_))
(check-expect (slide-row-left '(2 2 2)) (list 4 2 '_))
(check-expect (slide-row-left '(4 2 2)) (list 4 4 '_))
(check-expect (slide-row-left '(2 4 2)) (list 2 4 2))
(check-expect (slide-row-left '(2 2 4)) (list 4 4 '_))
(check-expect (slide-row-left '(2 4 8)) (list 2 4 8))
(check-expect (slide-row-left '(2 _ 4 4)) (list 2 8 '_ '_))
(check-expect (slide-row-left '(16 _ 64 64)) (list 16 128 '_ '_))

; slide-row-left : [ListOf TileValue] -> [ListOf TileValue]
; (slide-row-left ls) returns a list with all of the blank value shifted
; to the end of the list
(define (slide-row-left lotv)
  (local [; srl/help : [ListOf TileValue] [Maybe TileValue]
          ;            [ListOf TileValue] -> [ListOf TileValue]
          ; (srl/help row last-number blanks) returns a list with all of
          ; the blank value shifted to the end of the list
          (define (srl/help row last-number blanks)
            (cond
              [(empty? row)
               (if (false? last-number)
                   blanks
                   (cons last-number blanks))]
              [(and (number? (first row))
                    (false? last-number))
               (srl/help (rest row) (first row) blanks)]
              [(and (number? (first row))
                    (equal? (first row) last-number))
               (cons (+ (first row) last-number)
                     (srl/help (rest row) #false (cons '_ blanks)))]
              [else (if (blank? (first row))
                        (srl/help (rest row)
                                  last-number
                                  (cons '_ blanks))
                        (cons last-number (srl/help (rest row)
                                                    (first row)
                                                    blanks)))]))]
    (srl/help lotv #false '())))

; slide-row-right : [ListOf TileValue] -> [ListOf TileValue]
; (slide-row-right ls) returns a list with all of the blank value 
; shifted to the front of the list
(define (slide-row-right lotv)
  (reverse (slide-row-left (reverse lotv))))
(check-expect (slide-row-right '(_ _ 2 2 _ 2 4 _ _ 4))
              (list '_ '_ '_ '_ '_ '_ '_ 2 4 8))
(check-expect (slide-row-right '(2 2 4)) (list '_ 4 4))
(check-expect (slide-row-right '(_ _ _)) (list '_ '_ '_))
(check-expect (slide-row-right '(4 4 4 4)) (list '_ '_ 8 8))
(check-expect (slide-row-right '(4 _ 4 1024)) (list '_ '_ 8 1024))

; slide-left : Board -> Board
; (slide-left a-board) returns the board after sliding all rows to the
; left
(define (slide-left a-board)
  (map slide-row-left a-board))
(check-expect (slide-left b1) (list (list 64 32)
                                    (list 32 '_)))
(check-expect (slide-left b2) (list (list 4 4 '_ '_)
                                    (list 8 '_ '_ '_)
                                    (list 16 8 '_ '_)
                                    (list 32 '_ '_ '_)))
(check-expect (slide-left b3) (list (list 16 64 8 256 4)
                                    (list 2048 1024 32 128 '_)
                                    (list 64 32 128 '_ '_)
                                    (list 8 32 '_ '_ '_)
                                    (list 2 512 '_ '_ '_)))

; slide-right : Board -> Board
; (slide-right a-board) returns the board after sliding all rows to the
; right
(define (slide-right a-board)
  (map slide-row-right a-board))
(check-expect (slide-right b1) (list (list 64 32)
                                     (list '_ 32)))
(check-expect (slide-right b2) (list (list '_ '_ 4 4)
                                     (list '_ '_ '_ 8)
                                     (list '_ '_ 8 16)
                                     (list '_ '_ '_ 32)))
(check-expect (slide-right b3) (list (list 16 64 8 256 4)
                                     (list '_ 1024 2048 32 128)
                                     (list '_ '_ 64 32 128)
                                     (list '_ '_ '_ 8 32)
                                     (list '_ '_ '_ 2 512)))

; transpose : [ListOf [ListOf Any]] -> [ListOf [ListOf Any]]
; (transpose lls) returns the result of reflecting the elements along
; the main diagonal line of the list
; e.g. (transpose '((1 2) (3 4))) -> '((1 3) (2 4))
(define (transpose llos)
  (cond
    [(empty? (first llos)) '()]
    [else (cons (map first llos)
                (transpose (map rest llos)))]))
(check-expect (transpose '(())) '())
(check-expect (transpose '((a b c) (d e f))) (list (list 'a 'd)
                                                   (list 'b 'e)
                                                   (list 'c 'f)))
(check-expect (equal? (transpose (transpose b2)) b2) #true)

; slide-up : Board -> Board
; (slide-up a-board) returns the board after sliding all rows to the top
(define (slide-up a-board)
  (transpose (slide-left (transpose a-board))))
(check-expect (slide-up b1) b1)
(check-expect (slide-up b2) (list (list 2 2 2 2)
                                  (list 4 8 4 8)
                                  (list 16 '_ 8 16)
                                  (list '_ '_ '_ '_)))
(check-expect (slide-up b3) (list (list 16 64 8 256 4)
                                  (list 1024 1024 1024 32 128)
                                  (list 64 32 128 512 '_)
                                  (list 4 4 32 '_ '_)
                                  (list 2 '_ '_ '_ '_)))

; slide-down : Board -> Board
; (slide-down a-board) returns the board after sliding all rows to the
; bottom
(define (slide-down a-board)
  (transpose (slide-right (transpose a-board))))
(check-expect (slide-down b1) b1)
(check-expect (slide-down b2) (list (list '_ '_ '_ '_)
                                    (list 2 '_ 2 2)
                                    (list 4 2 4 8)
                                    (list 16 8 8 16)))
(check-expect (slide-down b3) (list (list 16 '_ '_ '_ '_)
                                    (list 1024 64 8 '_ '_)
                                    (list 64 1024 1024 256 '_)
                                    (list 4 32 128 32 4)
                                    (list 2 4 32 512 128)))

; game-lost? : Board -> Bool
; (game-lost? a-board) returns #true iff the board represents a losing
; configuration where there is no BLANK left
(define (game-lost? a-board)
  (and (equal? (slide-up a-board) a-board)
       (equal? (slide-down a-board) a-board)
       (equal? (slide-left a-board) a-board)
       (equal? (slide-right a-board) a-board)))
(check-expect (game-lost? b1) #false)
(check-expect (game-lost? b2) #false)
(check-expect (game-lost? b3) #false)
(check-expect (game-lost? (list (list 2 4)
                                (list 4 8))) #true)

; game-over? : Board -> Bool
; (game-over? a-board) returns #true iff the board represents a winning
; (there's 2048 on the board) or losing (there's no BLANK on the board)
; configuration
(define (game-over? a-board)
  (or (game-won? a-board)
      (game-lost? a-board)))
(check-expect (game-over? b1) #false)
(check-expect (game-over? b2) #false)
(check-expect (game-over? b3) #false)
(check-expect (game-over? (list (list 2048 2)
                                (list 2 '_))) #true)

#|
   A KeyStroke is one of
   - "up"
   - "down"
   - "right"
   - "left"
|#

; key-handler : Board KeyStroke -> Board
; (key-handler a-board key) returns a board with the new list of
; TileValue depending on the key pressed
(define (key-handler a-board key)
  (local [(define new-board
            (cond
              [(equal? key "up") (slide-up a-board)]
              [(equal? key "down") (slide-down a-board)]
              [(equal? key "left") (slide-left a-board)]
              [(equal? key "right") (slide-right a-board)]
              [else a-board]))]
    (if (equal? new-board a-board)
        a-board
        (add-new-tile new-board))))

; play : PosInt -> Image
; (play n) returns an interactive nxn board with two non-blank
; tiles that reacts to keyboard keys
(define (play n)
  (big-bang (make-board n 2)
            [to-draw board->image]
            [on-key key-handler]
            [stop-when game-over?]
            [name "2048"]))