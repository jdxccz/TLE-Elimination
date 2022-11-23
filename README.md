# TLE Elimination Project Proposal


In this project, we will build an arcade game named TLE Elimination. 

## Game Set Up: 
The program will initialize a game board with the size of M*N. At the start of the game, the program will fill in the board with multiple layers of pictures on each entry. The player will have an empty container with a size S.  

## Basic Rules: 
* When the setup is completed, the player can pick a picture from the board and put it into the empty container.
* The player can only select the picture on the top layer.
* After the player picks the picture from the board, he could not put it back on the board.
* After the picture is removed from the board, the picture below it will be exposed if there is one. 
* If there are more than three same pictures in the container, they will be eliminated, and the space of the container will be freed. (The order of the pictures added to the container does not matter. They don’t have to be added continuously to be eliminated.)
* The player should repeat the process until there are no pictures on the board.
* If the container is full sometime during the game and the board is not yet cleared, the player will lose the game.

## Requirements: 
* M*N must be a multiple of 3.
* Each kind of picture must appear exactly 3 * K times.
* The program will guarantee that it will initialize the game board with pictures in a way that there exists one solution (order of picture selection) to win the game.  (We will run simulations to check if the board is solvable after randomly generating a board.)

## Game Props:

We will design some bonus props to reduce the complexity of the game. So that the player will have several chances to continue the game even if the container is filled up. The player can only use each prop once.\\

* Shuffle: Once the player finds there are no ways to solve the board, he can choose to shuffle the board, and the program will collect all the pictures left on the board and randomly assign them to the game board again.
* Revoke: The player can choose to revoke his last move. The program will put the last picture he chose back onto the board.
* Move: The last three pictures added to the container will be moved back to the picture.

## Difficulty Levels: 
A higher level of the game will come with smaller container size (small S) and more layers of pictures. These parameters may also be customized by the player. 

## Build

cabal update
cabal install --only-dependencies
cabal build
cabal install

## Run
cabal run

## Contribution
Hansong Chen: Build Stack Logic, Build Win Logic, Caculate Scores, Improve Stack Display 
Xingkai Zheng: Build Muti-layer of Board, Build Revoke Logic, Build Shuffle Logic, Build Clear Stack Logic
