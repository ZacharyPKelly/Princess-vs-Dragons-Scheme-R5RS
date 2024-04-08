(#%require (only racket/base random))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;This file contains methods, logic and code in order to run the Princess vs
;;Dragon program required for Assignment One in AUCSC 370. The user is
;;prompted to select a board size then navigate around the randomly placed
;;dragons in order to reach the edge of the board.
;;
;;Class: AUCSC 370
;;Name: Zachary Kelly
;;Student ID: 1236421
;;Date: October 28th, 2022


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;makePrincessBlank
;;
;;aList -> Assumed to a be a 2d list containing the game board.
;;
;;Wrapper for changePrincessToBlank, changePrincessToBlankHelper, and swapPrincessForBlank.
;;Takes a list and removes all instances of princess from the list.

(define (makePrincessBlank aList)
  (cond
    ((null? aList) aList)
    (#t (append (list (changePrincessToBlank (car aList))) (makePrincessBlank (cdr aList))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;changePrincessToBlank
;;
;;aList -> Assumed to be a 2d list containing the game board.
;;
;;Wrapper for changePrincessToBlankHelper and swapPrincessForBlank.
;;Takes a populated list and returns a list with no instances of princess.

(define (changePrincessToBlank aList)
  (changePrincessToBlankHelper aList '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;changePrincessToBlankHelper
;;
;;aList -> Assumed to be a 2d list containing the game board.
;;nameSoFar -> Assumed to be an empty single level list.
;;
;;Takes a populated list (assumed to be 2d) and an empty list and iterates through the first,
;;calling swapPrincessForBlank when it determines that an item in the list is princess to
;;swap princess for --------.

(define (changePrincessToBlankHelper aList nameSoFar)
  (cond
    ((null? aList) nameSoFar)
    ((list? (car aList)) (changePrincessToBlankHelper (car aList) nameSoFar))
    ((eq? 'princess (car aList)) (append nameSoFar (swapPrincessForBlank aList)))
    (#t (changePrincessToBlankHelper (cdr aList) (append nameSoFar (list (car aList)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;swapPrincessForBlank
;;
;;aList -> Assumed to be a single level list. Will be an array with 'princess' at index 0.
;;
;;Removes the first item of a list (will always be princess) and adds to the front of the
;;list a blank game tile --------.

(define (swapPrincessForBlank aList)
  (cons '-------- (cdr aList)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;swapBlankForPrincess
;;
;;originalList -> Assumed to be a 2d list containing the game board. Returned when game is
;;                either won or lost.
;;aList -> Assumed to be a 2d list containing the game board. Will be the 2d list that is
;;         'worked on' and modifiyed based on user input.
;;size -> The size given by the user as to the size of the game board as an integer.
;;row -> The current row postition of the princess as an integer. Will be used to iterate.
;;column -> The current col position of the princess as an integer. Will be used to iterate.
;;currentRow -> The current row postition of the princess as an integer.
;;currentCol -> The current col postition of the princess as an integer.
;;
;;Wrapper for swapBlankForPrincessHelper and blankToPrincess.
;;Takes the same populated list twice, the size given by the user for the board size, a row
;;and column to be iterated with and a row and column (representing the users current
;;postition) to return to startGameLoop.

(define (swapBlankForPrincess originalList aList size row column currentRow currentCol)
  (swapBlankForPrincessHelper originalList aList '() size row column currentRow currentCol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;swapBlankForPrincessHelper
;;
;;originalList -> Assumed to be a 2d list containing the game board. Returned when game is
;;                either won or lost.
;;aList -> Assumed to be a 2d list containing the game board. Will be the 2d list that is
;;         'worked on' and modifiyed based on user input.
;;listSoFar -> Assumed to be an empty single level list. Used to keep track of items car'd
;;             from aList while working on aList.
;;size -> The size given by the user as to the size of the game board as an integer.
;;row -> The current row postition of the princess as an integer. Will be used to iterate.
;;column -> The current col position of the princess as an integer. Will be used to iterate.
;;currentRow -> The current row postition of the princess as an integer.
;;currentCol -> The current col postition of the princess as an integer.
;;
;;Takes the same populated list twice, and empty list, the size given by the user for the
;;board size, a row and column to be iterated with and a row and column (representing the
;;users current postition) to return to startGameLoop. Will replace a blank tile with the
;;princess at the row and column given. Calls blankToPrincess to do the actual swapping.

(define (swapBlankForPrincessHelper originalList aList listSofar size row column currentRow currentCol)
  (cond
    ((null? aList) '())
    ((< currentRow 0) (playerWon (makePrincessBlank aList)))
    ((> currentRow (- size 1)) (playerWon (makePrincessBlank aList)))
    ((< currentCol 0) (playerWon (makePrincessBlank aList)))
    ((> currentCol (- size 1)) (playerWon (makePrincessBlank aList)))
    ((equal? '-dragon- (getIndexValue currentRow currentCol originalList)) (playerLost (swapDragonForYumyum aList currentRow currentCol)))
    ((eq? 0 row) (startGameLoop (append listSoFar (append (list (blankToPrincess (car aList) '() column)) (cdr alist))) currentRow currentCol size))
    (#t (swapBlankForPrincessHelper originalList (cdr aList) (append listSoFar (list (car aList))) size (- row 1) column currentRow currentCol))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;blankToPrincess
;;
;;aList -> Assumed to be a single level list representing a row of the game board.
;;listSoFar -> Assumed to be a single level list. Used to keep track of items car'd
;;             from aList while working on aList.
;;column -> The column postition that will have a blank tile changed to princess as an
;;          integer.
;;
;;Removes an item in a list (will always however be ------) and replaces it in the same
;;location with princess.

(define (blankToPrincess aList listSoFar column)
  (cond
    ((null? aList) listSoFar)
    ((eq? 0 column) (append listSoFar (cons 'PRINCESS (cdr aList))))
    (#t (blankToPrincess (cdr aList) (append listSoFar (list (car aList))) (- column 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;swapDragonForYumyum
;;
;;aList -> Assumed to be a 2d list containing the game board.
;;row -> The row postion of where dragon will be changed to -yumyum- as an integer.
;;column -> The column of position of where dragon will be changed to -yumyum- as an integer.
;;
;;Wrapper for swapDragonForYumyumHelper and dragonToYumyum.
;;Takes a populated list and the row and column that should be changed.

(define (swapDragonForYumyum aList row column)
  (swapDragonForYumyumHelper aList '() row column))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;swapDragonForYumyumHelper
;;
;;aList -> Assumed to be a 2d list containing the game board.
;;listSoFar -> Assumed to be a single level list. Used to keep track of items car'd
;;             from aList while working on aList.
;;row -> The row postion of where dragon will be changed to -yumyum- as an integer.
;;column -> The column of position of where dragon will be changed to -yumyum- as an integer.
;;
;;Takes a populated list, empty list, and the row and column where -dragon- should be swapped
;;with -yumyum-. Calls dragonToYumyum to do the actual swapping.
;;

(define (swapDragonForYumyumHelper aList listSoFar row column)
  (cond
    ((eq? 0 row) (append listSoFar (append (list (dragonToYumyum (car aList) '() column)) (cdr alist))))
    (#t (swapDragonForYumyumHelper (cdr aList) (append listSoFar (list (car aList))) (- row 1) column))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;dragonToYumyum
;;
;;aList -> Assumed to be a single level list containing the column where dragon will be
;;         changed to -yumyum-.
;;listSoFar -> Assumed to be a single level list. Used to keep track of items car'd
;;             from aList while working on aList.
;;column -> The column of position of where dragon will be changed to -yumyum- as an integer.
;;
;;Removes an item in a list (will always however be -dragon-) and replaces it in the same
;;location with -yumyum-.
;;

(define (dragonToYumyum aList listSoFar column)
  (cond
    ((null? aList) listSoFar)
    ((eq? 0 column) (append listSoFar (cons '-yumyum- (cdr aList))))
    (#t (dragonToYumyum (cdr aList) (append listSoFar (list (car aList))) (- column 1)))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;populateArray
;;
;;aList -> Assumed to be an empty 2d list.
;;size -> The size given by the user as to the size of the game board as an integer.
;;counter -> A counter to track how many items have been placed in aList as an integer.
;;           Assumed to be 0 when function is first called.
;;
;;Assumed to take a 2d array, car's off the current first list contained, and call populate
;;columns to add the correct game pieces, does this for all contained lists and cons it
;;back together.

(define (populateArray aList size counter)
  (cond
    ((null? aList) '())
    (#t (cons (populateColumns (car aList) size size (+ (* size counter) 1)) (populateArray (cdr aList) size (+ counter 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;populateColumns
;;
;;aList -> Assumed to be an empty single level list representing a row of the game board.
;;amount -> The size of the row, each index of the row representing the next column. Will
;;           be iterated with and taken as an integer.
;;size -> The size given by the user as to the size of the game board as an integer.
;;counter -> A counter to track how many items have been placed in aList as an integer.
;;
;;A helper for populateArray. Assumed to be given a single level array, counts down how many 
;;items should be placed and calls dragonOrEmpty to place the correct game board piece for
;;each game board tile.

(define (populateColumns aList amount size counter)
  (cond
    ((eq? amount 0) aList)
    (#t (populateColumns (cons (dragonOrEmpty (random 12) size counter) aList) (- amount 1) size (+ counter 1)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;dragonOrEmpty
;;
;;random -> A random integer. Assumed to be between 0 and 11 inclusive.
;;size -> The size given by the user as to the size of the game board as an integer.
;;counter -> A counter to track how many items have been placed in aList as an integer.
;;
;;Takes a random integer between 0 and 11 (inclusive) and places a dragon if said random
;;integer is 0, 1, or 2; Places an empty tile if not. If the current position is equal
;;to the postition the princess should be placed, the Princess is placed instead of an
;;empty tile or dragon.

(define (dragonOrEmpty random size counter)
  (cond
    ((eq? counter (princessPosition size)) 'PRINCESS)
    ((member random '(0 1 2)) '-DRAGON-)
    (#t '--------)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;princessPosition
;;
;;size -> The size given by the user as to the size of the game board as an integer.
;;
;;Calculates the center position of the board for where the princess should be placed
;;when starting a new game by using the size given by the user. Postition is calculated
;;as the middle of the tiles, not in a index fashion.
;;
;;e.g: (princessPostition 5) -> 25 total tiles so would return 13

(define (princessPosition size)
  (cond
    ((eq? 0 (modulo size 2)) (+ (* size (/ size 2)) (/ size 2)))
    (#t (/ (+ (* size size) 1) 2))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;makeEmptyArray
;;
;;size -> The size given by the user as to the size of the game board as an integer.
;;emptyArray -> Assumed to be a single level list.
;;
;;Makes a 2d array based on the size of the input.
;;Taken from the example posted on eClass.
;;
;;e.g: (makeEmptyArray 3) = (() () ())


(define (makeEmptyArray size emptyArray)
  ;(display "makeEmptyArray\n")
  (cond
    ((eq? size 0) emptyArray)
    (#t (cons emptyArray (makeEmptyArray (- size 1) emptyArray)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;printArray
;;
;;aList -> Assumed to be a 2d array containing the game board.
;;
;;Takes a 2d list and prints the contents in a manner resembling a game board, iterates
;;through the lists and calls printColumn to print the contents of each list.

(define (printArray aList)
  (cond
    ((null? aList) '())
    (#t (printColumn (car aList) (cdr aList)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;printColumn
;;
;;aList -> Assumed to be a single level list representing a row of the game board.
;;restOfList -> Assumed to be a 2d list containing the gameboard that has not been printed.
;;
;;Takes a single level list and prints the contents all on one line with a " " between.

(define (printColumn aList restOfList)
  (cond
    ((null? aList) (display "\n") (printArray restOfList))
    (#t (display (car aList)) (display " ") (printColumn (cdr aList) restOfList))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;startGameLoop
;;
;;aList -> Assumed to be a 2d list containing the game board.
;;row -> The current row postition of the princess as an integer.
;;column -> The current col position of the princess as an integer.
;;size -> The size given by the user as to the size of the game board as an integer.
;;
;;Takes a 2d list, the current row and column of the princess, and the size of the gameboard.
;;Prints the gameboard then calls checkDirection to see where the user wants to move to next.

(define (startGameLoop aList row column size)
  (display "--------------------------------------------------------------------------------------\n\n")
  (printArray aList)
  (newline)
  (checkDirection (getDirection) aList row column size))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;playAgain
;;
;;char -> Assumed to a 2d list containing lists with one character in each list.
;;
;;Takes a list of characters from the user (always gotten from getPlayAgainAnswer) and determines
;;if the car of the string is y, Y, n, N. If n or N then the program terminates with an exit
;;message, if y or Y calls main to start a new game, otherwise calls itself again to get
;;new input.

(define (playAgain char)
  (cond
    ((eq? char '()) (display "Sorry that input wasn't accepted, please try again\n\n") (playAgain (getPlayAgainAnswer)))
    ((eq? (car char) #\y) (main))
    ((eq? (car char) #\Y) (main))
    ((eq? (car char) #\n) (display "Thanks for playing"))
    ((eq? (car char) #\N) (display "Thanks for playing"))
    (#t (display "Sorry that input wasn't accepted, please try again\n\n") (playAgain (getPlayAgainAnswer)))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;getPlayAgainAnswer
;;
;;Ask's the user if they want to play another game and saves the input as a list of characters.
;;Will always be used in conjunction with playAgain to feed it input.
;;

(define (getPlayAgainAnswer)
  (display "Would you like to play again (y/n): ")
  (readLine))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;playerWon
;;
;;aList -> Assumed to be a 2d list containing the gameboard.
;;
;;Prints out the finished game board and a message congratulating the user on winning, calls
;;playAgain to see if the user wants to play another game.

(define (playerWon aList)
  (display "--------------------------------------------------------------------------------------\n\n")
  (printArray aList)
  (newline)
  (display "--------------------------------------------------------------------------------------\n\n")
  (display "------------------------------\n")
  (display "|                            |\n")
  (display "|                            |\n")
  (display "|     -----        -----     |\n")
  (display "|     |   |        |   |     |\n")
  (display "|     |   |        |   |     |\n")
  (display "|     -----        -----     |\n")
  (display "|                            |\n")
  (display "|   ---------------------    |\n")
  (display "|   \\                   /    |\n")
  (display "|    \\                 /     |\n")
  (display "|     -----------------      |\n")
  (display "|                            |\n")
  (display "------------------------------")
  (newline)
  (newline)
  (display "The PRINCESS has escaped!\n\n")
  (display "          YOU WIN!\n\n")
  (playAgain (getPlayAgainAnswer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;playerLost
;;
;;aList -> Assumed to be a 2d list containing the gameboard.
;;
;;Prints out the finished game board and a message chastising the user for losing such a 
;;simple game, calls playAgain to see if the user wants to play another game.

(define (playerLost aList)
  (display "--------------------------------------------------------------------------------------\n\n")
  (printArray aList)
  (newline)
  (display "--------------------------------------------------------------------------------------\n\n")
  (display "-----------------------------\n")
  (display "|                           |\n")
  (display "|                           |\n")
  (display "|     -----       -----     |\n")
  (display "|     |   |       |   |     |\n")
  (display "|     |   |       |   |     |\n")
  (display "|     -----       -----     |\n")
  (display "|                           |\n")
  (display "|     -----------------     |\n")
  (display "|     |               |     |\n")
  (display "|     |               |     |\n")
  (display "|     -----------------     |\n")
  (display "|                           |\n")
  (display "-----------------------------")
  (newline)
  (newline)
  (display "You somehow managed to run into a completely avoidable DRAGON!\n")
  (display "The DRAGON gobbles you up. YUM YUM!\n\n")
  (playAgain (getPlayAgainAnswer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;checkDirection
;;
;;char -> Assumed to a 2d list containing lists with one character in each list.
;;aList -> Assumed to be a 2d list containing the gameboard.
;;row -> The current row postition of the princess as an integer.
;;column -> The current col position of the princess as an integer.
;;size -> The size given by the user as to the size of the game board as an integer.
;;
;;Takes a list of characters from the user (always gotten from getDirection), a list representing
;;the game board, the current row and column of the princess and the size of the game board and
;;determines if the car of the list is an appropriate input to move the princess. If it is the
;;current row or column is adjusted appropriatly and sent to swapBlankForPrincess along with
;;the game board.

(define (checkDirection char aList row column size)
  (cond
    ((eq? char '()) (display "Sorry that input wasn't accepted, please try again\n\n") (checkDirection (getDirection) aList row column size))
    ((eq? (car char) #\l) (newline) (swapBlankForPrincess (makePrincessBlank aList) (makePrincessBlank aList) size row (- column 1) row (- column 1)))
    ((eq? (car char) #\L) (newline) (swapBlankForPrincess (makePrincessBlank aList) (makePrincessBlank aList) size row (- column 1) row (- column 1)))
    ((eq? (car char) #\r) (newline) (swapBlankForPrincess (makePrincessBlank aList) (makePrincessBlank aList) size row (+ column 1) row (+ column 1)))
    ((eq? (car char) #\R) (newline) (swapBlankForPrincess (makePrincessBlank aList) (makePrincessBlank aList) size row (+ column 1) row (+ column 1)))
    ((eq? (car char) #\d) (newline) (swapBlankForPrincess (makePrincessBlank aList) (makePrincessBlank aList) size (+ row 1) column (+ row 1) column))
    ((eq? (car char) #\D) (newline) (swapBlankForPrincess (makePrincessBlank aList) (makePrincessBlank aList) size (+ row 1) column (+ row 1) column))
    ((eq? (car char) #\u) (newline) (swapBlankForPrincess (makePrincessBlank aList) (makePrincessBlank aList) size (- row 1) column (- row 1) column))
    ((eq? (car char) #\U) (newline) (swapBlankForPrincess (makePrincessBlank aList) (makePrincessBlank aList) size (- row 1) column (- row 1) column))
    (#t (display "Sorry that input wasn't accepted, please try again\n\n") (checkDirection (getDirection) aList row column size))))

(define (getDirection)
  (display "Please enter the direction you would like to move: ")
  (readLine))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;main
;;
;;Called to start the game. Calls welcomeMessage.

(define (main)
  (welcomeMessage))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;welcomeMessage
;;
;;Prints a welcome message for the user explaining the rules of the game, calls getBoardSize
;;to get input from the user regarding the board size.

(define (welcomeMessage)
  (display "--------------------------------------------------------------------------------------\n\n")
  (display "You are the PRINCESS! You must escape the EVIL DRAGONS!\n\n")
  (display "Thankfully these DRAGONS are STUPID, they can't even move!\n\n")
  (display "Navigate around the DRAGONS to the edge of the board where the water is to WIN!\n\n")
  (display "You can move with the following commands:\n\n")
  (display "Enter 'L' for left\n")
  (display "Enter 'R' for right\n")
  (display "Enter 'U' for up\n")
  (display "Enter 'D' for down\n\n")
  (display "Capitalization doesn't matter!\n")
  (display "Any word or phrase that starts with those letters will move you in that direction!\n\n")
  (display "--------------------------------------------------------------------------------------\n\n")
  (getBoardSize(getInt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;getBoardSize
;;
;;size -> Assumed to a 2d list containing lists with one character in each list.
;;
;;Takes a list of characters from the user (gotten from getInt) and determines whether the
;;car of the list is equivelent to a numbers from 4 to 9. If it is, calls start game loop
;;with the integer representation of the character passed. If the size of the list is > 1
;;checkBoardSizeAboveNine is called. Otherwise calls itself again to get new input.

(define (getBoardSize size)
  (cond
    ((null? size) (display "Board size must be between 4 and 10 inclusive.\n") (newline) (getBoardSize (getInt)))
    ((> (length size) 1) (checkBoardSizeAboveNine size))
    ((equal? (car size) #\4) (newline) (startGameLoop (populateArray (makeEmptyArray 4 '()) 4 0) 2 2 4))
    ((equal? (car size) #\5) (newline) (startGameLoop (populateArray (makeEmptyArray 5 '()) 5 0) 2 2 5))
    ((equal? (car size) #\6) (newline) (startGameLoop (populateArray (makeEmptyArray 6 '()) 6 0) 3 3 6))
    ((equal? (car size) #\7) (newline) (startGameLoop (populateArray (makeEmptyArray 7 '()) 7 0) 3 3 7))
    ((equal? (car size) #\8) (newline) (startGameLoop (populateArray (makeEmptyArray 8 '()) 8 0) 4 4 8))
    ((equal? (car size) #\9) (newline) (startGameLoop (populateArray (makeEmptyArray 9 '()) 9 0) 4 4 9))
    (#t (display "Board size must be between 4 and 10 inclusive.\n") (newline) (getBoardSize (getInt)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;checkBoardSizeAboveNine
;;
;;size -> Assumed to a 2d list containing lists with one character in each list and more then
;;        one list contained within.
;;
;;Takes a list of characters. If the list is larger then 2 then calls itself to get new input,
;;if the car of the list is #\1 then calls checkBoardSizeAboveNineHelper, otherwise calls
;;getBoardSize to get new input.

(define (checkBoardSizeAboveNine size)
  (cond
    ((> (length size) 2) (display "Board size must be between 4 and 10 inclusive.\n") (newline) (getBoardSize (getInt))) ;at least three digits given so != 10
    ((equal? (car size) #\1) (checkBoardSizeAboveNineHelper size))
    (#t (display "Board size must be between 4 and 10 inclusive.\n") (newline) (getBoardSize (getInt)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;checkBoardSizeAboveNineHelper
;;
;;size -> Assumed to a 2d list containing lists with one character in each list and only
;;        two lists contained, with the first being '#\1'
;;
;;Checks the second item in a list that has a length of 2 to see if the character contained
;;there is #\0. If it is, calls startGameLoop with the integer representaition of the 
;;characters contained in the list, otherwise calls getBoardSize to get new input.

(define (checkBoardSizeAboveNineHelper size)
  (cond
    ((equal? (car (cdr size)) #\0) (newline) (startGameLoop (populateArray (makeEmptyArray 10 '()) 10 0) 5 5 10))
    (#t (display "Board size must be between 4 and 10 inclusive.\n") (newline) (getBoardSize (getInt)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;getInt
;;
;;Ask's the user what size board they want and saves the input as a list of characters.
;;Will always be used in conjunction with getBoardSize to feed it input.

(define (getInt)
  (display "Please enter the board size you wish to play on (4-10): ")
  (readLine))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; readLine() --> line (as String)
;;
;; Read one line from standard input, not including the newline
;; but eliminating it. This is wrapper for the recursive method
;; that does the work (readLoop).
;;
;;Taken from the spec sheet provided by Rosanna Heise for this assignment on eClass.
  
(define (readLine)
  (readLoop (read-char (current-input-port)) '())) ;do wait for one char

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; readLoop(currentCharacter line) --> line (as String)
;;
;; This recursive method reads a character at a time from the
;; current input port (assuming Scheme's "Interaction Window")
;; until it finds the newline (i.e. enter). It builds the characters
;; into a string which is returned at the end. Newline is not part
;; of the string, but is eliminated from the input
;;
;;Taken from the spec sheet provided by Rosanna Heise for this assignment on eClass.
  
(define (readLoop curChar line)
  (cond
    ((char=? #\newline curChar) line)
    (#t (readLoop (read-char (current-input-port))
                  (append line (list curChar))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;getIndexValue
;;
;;row -> The row postition of the index that will have its value returned. Taken as an integer
;;column -> The col position of the index that will have its value returned. Taken as an integer.
;;aList -> Asssumed to be a 2d list that contains the value that will be returned.
;;
;;Takes a populated 2d list, a row and a column and returns the value at said row and column.
;;Iterates through the lists contained in the given lists untill at the correct one, then
;;calls getColumnValue to get the specific item in said list.

(define (getIndexValue row column aList)
  (cond
    ((eq? row 0) (getColumnValue column (car aList)))
    (#t (getIndexValue (- row 1) column (cdr aList)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;getColumnValue
;;
;;column -> The col position of the index that will have its value returned. Taken as an integer.
;;aList -> Assumed to be a single level list representing a row of a 2d list.
;;
;;Takes a single level list and an index and returns the item found at the given index.

(define (getColumnValue column aList)
  (cond
    ((eq? column 0) (car aList))
    (#t (getColumnValue (- column 1) (cdr aList)))))

(main)
