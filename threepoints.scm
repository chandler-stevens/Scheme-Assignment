; #lang racket

; Function to create a 2-D point
; Parameters: Numerical X-coordinate and Numerical Y-coordinate
(define (make-point x-cor y-cor)
  ; If the given X-coordinate is actually Numerical
  (if (number? x-cor)
      ; Then If the given Y-coordinate is actually Numerical
      (if (number? y-cor)
        ; Then create the 2-D point with the given coordinates
        (cons x-cor y-cor)
        ; Otherwise reject non-Numerical input for the Y-coordinate
        "The given Y-coordinate is not a number!"
      )
      ; Otherwise reject non-Numerical input for the X-coordinate
      "The given X-coordinate is not a number!"
  )
)

; Getter Accessor Function to get the X-coordinate of a 2-D point
; Parameter: 2-D point
(define (get-x point)
  ; Return the first attribute or X-coordinate of the given 2-D point
  (car point))

; Getter Accessor Function to get the Y-coordinate of a 2-D point
; Parameter: 2-D point
(define (get-y point)
  ; Return the second attribute or Y-coordinate of the given 2-D point
  (cdr point))

; Function to check if three 2-D points form a line or a triangle
; Parameters: Three 2-D points
(define (is-line point1 point2 point3)
  ; Create variables for each X and Y coordinate of each given 2-D point
  (let ( (X1 (get-x point1))  (Y1 (get-y point1))
         (X2 (get-x point2))  (Y2 (get-y point2))
         (X3 (get-x point3))  (Y3 (get-y point3))
       )
       ; If the three 2-D points form a line
       ;  Confirm that at least two 2-D points were actually given
       ;   meaning that there are at least two different X-coordinate values or
       (if (and (or (or (not (= X1 X2)
                        )
                        (not (= X2 X3)
                        )
                    )
                    ;   that there at least two different Y-coordinate values
                    (or (not (= Y1 Y2)
                        )
                        (not (= Y2 Y3)
                        )
                    )
                )
                ;  Account for the case of actually only two 2-D points given
                (or (or (and (and (= X1 X2) (= Y1 Y2)
                             )
                             (or (not (= X1 X3)
                                 )
                                 (not (= Y1 Y3)
                                 )
                             )
                        )
                        (and (and (= X1 X3) (= Y1 Y3)
                             )
                             (or (not (= X1 X2)
                                 )
                                 (not (= Y1 Y2)
                                 )
                             )
                        )
                        (and (and (= X2 X3) (= Y2 Y3)
                             )
                             (or (not (= X1 X3)
                                 )
                                 (not (= Y1 Y3)
                                 )
                             )
                        )
                    )
                  ;  Create variables for each slope between two 2-D points
                  (let (
                         (XChange1 (- X2 X1)) (XChange2 (- X3 X1))
                         (YChange1 (- Y2 Y1)) (YChange2 (- Y3 Y1))
                       )
                       ; Confirm that there is a uniform slope
                       (= (/ YChange1 XChange1) (/ YChange2 XChange2))
                  )
                )
           )
           ; Then Return True since the three 2-D points form a line
           #t
           ; Otherwise If the three 2-D points form a triangle
           ;  Create variables for each of the three sides of the triangle
           (if (let (
                     (Side1 (distance point1 point2))
                     (Side2 (distance point2 point3))
                     (Side3 (distance point3 point1))
                    )
                    ;  Confirm that three different 2-D points were given
                    (and (or (not (= X1 X2)
                             )
                             (not (= Y1 Y2)
                             )
                         )
                         (or (not (= X1 X3)
                             )
                             (not (= Y1 Y3)
                             )
                         )
                         (or (not (= X2 X3)
                             )
                             (not (= Y2 Y3)
                             )
                         )
                         ; Confirm that the sum of any two sides
                         ;  is greater than the length of the third side
                         (> (+ Side1 Side2) Side3)
                         (> (+ Side2 Side3) Side1)
                         (> (+ Side1 Side3) Side2)
                    )
               )
               ; Then Return False since the three 2-D points form a triangle
               #f
               ; Otherwise reject input that does not form a line or triangle
               "The three given points do not form a line or a triangle!"
           )
       )
   )
)

; Function to compute the distance between two 2-D points
; Parameters: Two 2-D points
(define (distance point1 point2)
  ; Create variables for each X and Y coordinate of each given 2-D point
  (let ( (X1 (get-x point1))  (Y1 (get-y point1))
         (X2 (get-x point2))  (Y2 (get-y point2))
       )
       ; Return the result of the Distance Formula
       (sqrt (+ (expt (- X2 X1) 2) (expt (- Y2 Y1) 2)))
  )
)

; Function to compute the perimeter of a triangle
; Parameters: Three 2-D points
(define (perimeter point1 point2 point3)
  ; If the three 2-D points actually form a line
  (if (is-line point1 point2 point3)
      ; Then reject input that forms a line instead of a triangle
      "The three given points do not form a triangle!"
      ; Otherwise Create variables for each of the three sides of the triangle
      (let (
            (Side1 (distance point1 point2))
            (Side2 (distance point2 point3))
            (Side3 (distance point3 point1))
           )
           ; Return the summation of the three sides of the triangle
           (+ Side1 Side2 Side3)
      )
  )
)

; Function to compute the area of a triangle
; Parameters: Three 2-D points
(define (area point1 point2 point3)
  ; If the three 2-D points actually form a line
  (if (is-line point1 point2 point3)
      ; Then reject input that forms a line instead of a triangle
      "The three given points do not form a triangle!"
      ; Otherwise Create variables for each of the three sides of the triangle
      (let (
            (Side1 (distance point1 point2))
            (Side2 (distance point2 point3))
            (Side3 (distance point3 point1))
           )
           ; Create a variable for the halved perimeter of the triangle
           (let (
                  (perimeterHalved (/ (+ Side1 Side2 Side3) 2))
                )
                ; Return the result of Heron's Formula
                (sqrt (* perimeterHalved
                         (- perimeterHalved Side1)
                         (- perimeterHalved Side2)
                         (- perimeterHalved Side3)
                      )
                )
           )
      )
  )
)

; Function to compute and print the three sides, perimeter, area, and
;  three interior angles in both radians and degrees of a triangle
; Paramters: Three 2-D points
(define (calculate-triangle point1 point2 point3)
  ; If the three 2-D points actually form a line
  (if (is-line point1 point2 point3)
      ; Then reject input that forms a line instead of a triangle
      "The three given points do not form a triangle!"
      ; Otherwise Create variables for each of the three sides,
      ;  perimeter, and area of the triangle
      (let (
            (Side1 (distance point1 point2))
            (Side2 (distance point2 point3))
            (Side3 (distance point3 point1))
            (Perimeter (perimeter point1 point2 point3))
            (Area (area point1 point2 point3))
           )
           ; Create variables for each of the
           ;  three interior angles in using the Law of Cosines
           (let (
                  (Angle1 (acos (/
                    (- (+ (expt Side2 2) (expt Side3 2)) (expt Side1 2))
                    (* 2 Side2 Side3)))
                  )
                  (Angle2 (acos (/
                    (- (+ (expt Side3 2) (expt Side1 2)) (expt Side2 2))
                    (* 2 Side1 Side3)))
                  )
                  (Angle3 (acos (/
                    (- (+ (expt Side1 2) (expt Side2 2)) (expt Side3 2))
                    (* 2 Side1 Side2)))
                  )
                )
                ; Create variables for each of the
                ;  three interior angles in degrees
                (let (
                        (Angle1Degrees (radians->degrees Angle1))
                        (Angle2Degrees (radians->degrees Angle2))
                        (Angle3Degrees (radians->degrees Angle3))
                     )
                     ; Print the three sides of the triangle
                     (display "Side 1 = ")
                     (displayln (string-trim
                       (real->decimal-string Side1 3) "0" #:repeat? #t))
                     (display "Side 2 = ")
                     (displayln (string-trim
                       (real->decimal-string Side2 3) "0" #:repeat? #t))
                     (display "Side 3 = ")
                     (displayln (string-trim
                       (real->decimal-string Side3 3) "0" #:repeat? #t))
                     ; Print the perimeter of the triangle
                     (display "Perimeter = ")
                     (displayln (string-trim
                       (real->decimal-string Perimeter 3) "0" #:repeat? #t))
                     ; Print the perimeter of the triangle
                     (display "Area = ")
                     (displayln (string-trim
                       (real->decimal-string Area 3) "0" #:repeat? #t))
                     ; Print the three interior angles in both
                     ;  radians and degrees of the triangle
                     (display "Angle 1 = ")
                     (display (string-trim
                       (real->decimal-string Angle1 5) "0" #:repeat? #t))
                     (display " ")
                     (displayln (string-trim
                       (real->decimal-string Angle1Degrees 5) "0" #:repeat? #t))
                     (display "Angle 2 = ")
                     (display (string-trim
                       (real->decimal-string Angle2 5) "0" #:repeat? #t))
                     (display " ")
                     (displayln (string-trim
                       (real->decimal-string Angle2Degrees 5) "0" #:repeat? #t))
                     (display "Angle 3 = ")
                     (display (string-trim
                       (real->decimal-string Angle3 5) "0" #:repeat? #t))
                     (display " ")
                     (displayln (string-trim
                       (real->decimal-string Angle3Degrees 5) "0" #:repeat? #t))
                )
           )
      )
  )
)

; Function to validate the format
;  of an International Standard Book Number (ISBN)
; Parameter: ISBN
;  Supports ISBN-13, ISBN-10, and even SBN
;  Supports integers and integer variables
;  Supports dashes within the ISBN
;  Supports the 'X' character for ISBN-10 and SBN
(define (check-isbn isbn)
  ; Modify the ISBN variable by removing any dashes and
  ;  by making any 'x' characters uppercase
  (set! isbn (string-replace (string-replace (~a isbn) "-" "") "x" "X"))
  ; If the ISBN has 13 digits
  (if (= (string-length isbn) 13)
    ; Then If the ISBN-13 is prefixed with "978" or "979"
    (if (and (string->number isbn)
             (or (string-prefix? isbn "978")
                 (string-prefix? isbn "979")
             )
        )
      ; Then If the ISBN-13 digits checksum to a multiple of 10
      (if (= (remainder (sum-isbn-13 isbn) 10) 0)
        ; Then Return True since the ISBN-13 has a valid format
        #t
        ; Otherwise Return False since the
        ;  ISBN-13 has an invalid format
        #f
      )
      ; Otherwise Return False since the
      ;  ISBN-13 has an invalid format
      #f
    )
    ; Otherwise If the ISBN has 10 digits
    (if (and (= (string-length isbn) 10)
             (string->number (string-replace isbn "X" ""))
        )
      ; Then If the ISBN-10 digits checksum to a multiple of 11
      (if (= (remainder (sum-isbn-10 isbn) 11) 0)
        ; Then Return True since the ISBN-10 has a valid format
        #t
        ; Otherwise Return False since the
        ;  ISBN-10 has an invalid format
        #f
      )
      ; Otherwise If the ISBN has 9 digits
      (if (and (= (string-length isbn) 9)
               (string->number (string-replace isbn "X" ""))
          )
        ; Then If the SBN digits checksum to a multiple of 11
        (if (= (remainder (sum-sbn isbn) 11) 0)
          ; Then Return True since the SBN has a valid format
          #t
          ; Otherwise Return False since the SBN has an invalid format
          #f
        )
        ; Otherwise Return False since the SBN has an invalid format
        #f
      )
    )
  )
)

; Supplementary Function to compute the checksum of an ISBN-13
; Parameters: ISBN-13 without dashes
(define (sum-isbn-13 isbn13)
  ; Create variables to initialize
  ;  the checksum to zero and the multiplier to one
  (let (
         (sum 0)
         (multiplier 1)
       )
       ; Create a counter-controlled loop with an initialized index at zero
       (let loop ((index 0))
         ; While the index is less than the length of the ISBN-13
         (when (< index 13)
           ; Increment the checksum by the digit times the multiplier
           (set! sum
             (+ (* (string->number
               (substring isbn13 index (+ index 1))) multiplier) sum)
           )
           ; If the index is an even number
           (if (even? index)
             ; Then alternate the multiplier to three
             (set! multiplier 3)
             ; Otherwise alternate the multiplier to one
             (set! multiplier 1)
           )
           ; Recursively call the loop with the index incremented by one
           (loop (+ index 1))
         )
       )
    ; Return the final computed checksum
    sum
  )
)

; Supplementary Function to compute the checksum of an ISBN-10
; Parameters: ISBN-10 without dashes and lowercase 'x' characters
(define (sum-isbn-10 isbn10)
  ; Create variable to initialize the checksum to zero
  (let ((sum 0))
       ; Create a counter-controlled loop with an initialized index at zero
       (let loop ((index 0))
         ; While the index is less than the length of the ISBN-10
         (when (< index 10)
           ; If the digit is an 'X' character
           (if (string=? (substring isbn10 index (+ index 1)) "X")
             ; Then increment the checksum by ten times the multiplier
             (set! sum
               (+ (* 10 (- 10 index)) sum)
             )
             ; Otherwise increment the checksum by
             ;  the digit times the multiplier
             (set! sum
               (+ (* (string->number
                 (substring isbn10 index (+ index 1))) (- 10 index)) sum)
             )
           )
           ; Recursively call the loop with the index incremented by one
           (loop (+ index 1))
         )
       )
    ; Return the final computed checksum
    sum
  )
)

; Supplementary Function to compute the checksum of an SBN
; Parameters: SBN without dashes and lowercase 'x' characters
(define (sum-sbn sbn)
  ; Create variable to initialize the checksum to zero
  (let ((sum 0))
       ; Create a counter-controlled loop with an initialized index at zero
       (let loop ((index 0))
         ; While the index is less than the length of the SBN
         (when (< index 9)
           ; If the digit is an 'X' character
           (if (string=? (substring sbn index (+ index 1)) "X")
             ; Then increment the checksum by ten times the multiplier
             (set! sum
               (+ (* 10 (- 9 index)) sum)
             )
             ; Otherwise increment the checksum by
             ;  the digit times the multiplier
             (set! sum
               (+ (* (string->number
                 (substring sbn index (+ index 1))) (- 9 index)) sum)
             )
           )
           ; Recursively call the loop with the index incremented by one
           (loop (+ index 1))
         )
       )
    ; Return the final computed checksum
    sum
  )
)
