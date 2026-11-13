#lang racket
(require 2htdp/image)
(require 2htdp/universe)
;Define constants
(define listOfRooms (list "Dorm" "Class1"))
(define empty-image (square 0.001 "solid" "White"))
(define blockSize 35)
;This is the data structure holding all the room data
;Key
;F:Floor
;W:Wall
;E:Empty
;D:Door
;B:Bed
(define initialRoomData
(list
    (list
        (list "W" "W" "W" "W" "W" "W" "W" "W" "D" "W")
        (list "W" "B" "F" "F" "F" "F" "F" "F" "F" "W")
        (list "F" "B" "F" "F" "F" "F" "F" "F" "F" "F")
        (list "F" "F" "F" "F" "F" "F" "F" "F" "F" "F")
        (list "F" "F" "F" "F" "F" "F" "F" "F" "F" "F")
        (list "F" "F" "F" "F" "F" "F" "F" "F" "F" "F")
        (list "F" "F" "F" "F" "F" "F" "F" "F" "F" "F")
    )
    (list
        (list "W" "W" "W" "W" "W" "W" "W" "W" "D" "W")
        (list "W" "F" "F" "F" "F" "F" "F" "F" "F" "W")
        (list "F" "F" "F" "F" "F" "F" "F" "F" "F" "F")
        (list "F" "F" "F" "F" "F" "F" "F" "F" "F" "F")
        (list "F" "F" "F" "F" "F" "F" "F" "F" "F" "F")
        (list "F" "F" "F" "F" "F" "F" "F" "F" "F" "F")
        (list "F" "F" "F" "F" "F" "F" "F" "F" "F" "F")
    )
 )
)
;room is a string detailing the room
;PE is a number representing physical energy
;ME is a number representing mental energy
;WP is a number representing willpower
;charX is a number representing character X position
;charY is a number representing character Y position
;hour is a number representing the hour
;day is a number representing the day
;roomData is a list of lists of lists of strings that represnts the room data for each possible room and x,y position
;moveList is a list of moves that the character will do
(define-struct WS (room PE ME WP charX charY hour day roomData moveList))

(define initial-WS (make-WS
                    "Dorm"
                    100
                    100
                    100
                    3
                    3
                    7
                    1
                    initialRoomData
                    empty
                   )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This chunk is for movement and collision logic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This function handles the key press
;worldstate,key->worldstate
(define (keyPress WS key)
  (make-WS
     (getNewRoom WS key)
     (WS-PE WS)
     (WS-ME WS)
     (WS-WP WS)
     (WS-charX WS)
     (WS-charY WS)
     (WS-hour WS)
     (WS-day WS)
     (WS-roomData WS)
     (getNewMoveList WS)
   )
)

;
;worldstate,key->string
(define (getNewRoom WS key) ())

;
;worldstate,key->list
(define (getNewXPosition WS key) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This chunk is for rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;This function takes the image name and it's form, and returns an image
;string, number -> image
;Key
;F:Floor
;W:Wall
;E:Empty
;D:Door
;B:Bed
(define (getImage imageName formNumber)
  (cond
    [(string=? imageName "F") (underlay (square blockSize "solid" "Brown") (square (- blockSize 2) "solid" "Goldenrod"))]
    [(string=? imageName "W") (underlay (square blockSize "solid" "Dim Grey") (square (- blockSize 2) "solid" "Dark Grey"))]
    [(string=? imageName "E") (square blockSize "solid" "White")]
    [(string=? imageName "D") (underlay (square blockSize "solid" "Dim Grey")
                                        (underlay
                                         (square (- blockSize 2) "solid" "Dark Grey")
                                         (underlay
                                          (rectangle (+ (/ (* 2 (- blockSize 2)) 3) 2) (- blockSize 2) "solid" "Dim Grey")
                                          (underlay/offset 
                                           (rectangle (/ (* 2 (- blockSize 2)) 3) (- blockSize 2) "solid" "Brown")
                                           (/ blockSize 10)
                                           0
                                           (underlay
                                            (circle (/ blockSize 10) "solid" "Black")
                                            (circle (- (/ blockSize 10) 0.5) "solid" "Dim Grey"))))))]
    [(string=? imageName "B")
        (cond [(= formNumber 0) (underlay (square blockSize "solid" "Dim Grey") (square (- blockSize 2) "solid" "Purple"))]
              [(= formNumber 1) (underlay (square blockSize "solid" "Dim Grey") (square (- blockSize 2) "solid" "Purple"))]
              [(= formNumber 2) (underlay/offset (square blockSize "solid" "Brown")
                                                 0 -0.575
                                          (underlay/align/offset "middle" "top"
                                                          (square (- blockSize 2) "solid" "Goldenrod")
                                                          0 -1.15
                                                          (above (rectangle (* 2 (/ blockSize 3)) (* 2 (/ blockSize 3)) "solid" "Red")
                                                                 (rectangle (* 3 (/ blockSize 4)) (/ blockSize 5) "solid" "brown"))))]
              [(= formNumber 3) (underlay (square blockSize "solid" "Dim Grey") (square (- blockSize 2) "solid" "Purple"))]
              [(= formNumber 4) (underlay/offset (square blockSize "solid" "Brown")
                                                 0 0.575
                                          (underlay/align/offset "middle" "bottom"
                                                          (square (- blockSize 2) "solid" "Goldenrod")
                                                          0 1.15
                                                          (above (rectangle (* 3 (/ blockSize 4)) (/ blockSize 5) "solid" "brown")
                                                                 (underlay/offset
                                                                  (rectangle (* 2 (/ blockSize 3)) (* 2 (/ blockSize 3)) "solid" "Red")
                                                                  0 (/ blockSize -20)
                                                                  (underlay (rectangle (/ blockSize 2) (/ blockSize 3) "solid" "Tan")
                                                                            (rectangle (/ (- blockSize 2) 2) (- (/ blockSize 3) 2) "solid" "White"))))))]
        )]
    [else "Error: broken getImage function"]
  )
)

;This takes the blocks surounding a block, and returns a number stating what form it should appear as
;5 strings-> number
(define (getForm block left up right down)
  (cond
    [(string=? block left) 1]
    [(string=? block up) 2]
    [(string=? block right) 3]
    [(string=? block down) 4]
    [else 0]
  )
)

;This function creates a row image
;List of strings,image,number,number,list of lists of strings->image
(define (buildRow rowData currentImage currentColumnIndex currentRowIndex currentRoomData)
  (cond
     [(empty? rowData) currentImage]
     [else (buildRow
        (rest rowData)
        (beside
          currentImage
          (getImage (first rowData) (getForm
                                     (first rowData)
                                     (if (not (empty? (rest rowData)))
                                              (first (rest rowData))
                                              "Error: broken buildRow or getForm function"
                                     )
                                     (if (> currentRowIndex 0)
                                         (list-ref (list-ref currentRoomData (- currentRowIndex 1)) currentColumnIndex)
                                         "Error: broken buildRow or getForm function"
                                      )
                                     (if (> currentColumnIndex 0)
                                         (list-ref (list-ref currentRoomData currentRowIndex) (- currentColumnIndex 1))
                                         "Error: broken buildRow or getForm function"
                                      )
                                     (if (< (+ currentRowIndex 1) (length currentRoomData))
                                         (list-ref (list-ref currentRoomData (+ currentRowIndex 1)) currentColumnIndex)
                                         "Error: broken buildRow or getForm function"
                                      )
                                    )
          )
        )
        (+ currentColumnIndex 1)
        currentRowIndex
        currentRoomData
     )]
  )
)

;Merge all the images for each grid location into one
;list of lists of strings, number -> image
(define (buildGrid currentRoomData currentImage currentRowIndex roomData)
  (cond
    [(empty? currentRoomData) currentImage]
    [else
     (buildGrid
        (rest currentRoomData)
        (above
          currentImage
          (buildRow (first currentRoomData) empty-image 0 currentRowIndex roomData)
       )
       (+ currentRowIndex 1)
       roomData
     )
    ]
  )
)

;This function renders everything
;worldstate->image
(define (render WS)
  (local [(define currentRoomData (list-ref (WS-roomData WS) (index-of listOfRooms (WS-room WS))))]
    (underlay/offset (buildGrid
              currentRoomData
              empty-image
              0
              currentRoomData
              )
              (* (- (- (WS-charX WS) 0.5) (/ (length (first currentRoomData)) 2)) blockSize) 
              (* (- (- (WS-charY WS) 0.5) (/ (length currentRoomData) 2)) blockSize)
              (underlay
               (rectangle (* 3 (/ blockSize 4)) (* 3 (/ blockSize 4)) "solid" "Purple")
               (rectangle (- (* 3 (/ blockSize 4)) 2) (- (* 3 (/ blockSize 4)) 2) "solid" "Medium Slate Blue"))
    )
  )
)

;This function runs the game
(big-bang initial-WS
  (to-draw render)
  (on-key keyHandler)
)