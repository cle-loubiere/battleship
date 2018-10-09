# Battleship game

Scala project made for learning functional programming.
The goal is to create a battleship game which can be played against an AI or against another player.

# Prerequisites

Scala version 2.12.7 : https://www.scala-lang.org/download/

Sbt version 1.2.4: https://www.scala-sbt.org/download.html

# How to run it

Use the command "sbt run" in the source directory

# The game

2 game modes are at your disposal :
  - Human vs Human
  - Human vs AI
 
 A third option "AI vs AI" test each AI, one against another a hundred time and create a file "ai_proof.csv" which contain the of each AI in each match.

This game is played like a normal battleship game : 10x10 grid, 5 ships respectively of size 2,3,3,4,5
    
For more information about the battleship game : https://en.wikipedia.org/wiki/Battleship_(game)
