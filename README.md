## Project: Go

### Rules

The following rules are either copied or adapted from "The Rules and Elements of Go" by James Davies.

- The board is empty at the onset of the game (unless players agree to place a handicap).
- Black makes the first move, after which White and Black alternate.
- A move consists of placing one stone of one's own color on an empty intersection on the board.
- A player may pass their turn at any time.
- A stone or solidly connected group of stones of one color is captured and removed from the board when all the intersections directly adjacent to it are occupied by the enemy. (Capture of the enemy takes precedence over self-capture.)
- No stone may be played so as to recreate a former board position.
- The end of the game is reached when neither player can place down a piece without losing it. The game end is
  typically determined by both players before all positions on the board are occupied.
- A player's area consists of all the points the player has either occupied or surrounded.
- The player with more area wins.
- To account for the advantage of the black player being able to go first, the white player gains 6 points at the end of the game. 

### Usage

To use the api: enter into bin/ and run 'dune build', followed by 'dune exec -- ./dream_routes.exe'
To use the frontend: enter into lib/go-frontend and run 'npm run build', followed by npm start.
Use node version 17 and above

Create a simple implementation of Go game in OCaml. This is the first part of a project. The game have two players - white pieces and black pieces. The starting state should be an empty board of 16 \* 16 that looks like the following:

```
     1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
 1 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 2 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 3 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 4 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 5 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 6 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 7 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 8 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 9 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
10 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
11 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
12 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
13 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
14 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
15 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
16 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
```

In each iteration, prompt the user to input a set of two numbers x y that represents the column and row of the location where they intend to place their move. A legal move can only be made in a position that is currently empty and not outside the bounds of the board. For example, an input of 2 2 will result in :

```
    1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
 1 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 2 [ ][B][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 3 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 4 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 5 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 6 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 7 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 8 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 9 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
10 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
11 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
12 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
13 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
14 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
15 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
16 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
```

Once one player has made its move, it is the other player's turn.
For example, an additional input of 2 3 will result in:

```
    1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
 1 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 2 [ ][B][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 3 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 5 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 6 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 7 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 8 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 9 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
10 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
11 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
12 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
13 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
14 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
15 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
16 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
```

This continues until the program is exited. If a player's pieces are surrounded on all sides by the other player's pieces, then they are taken off the board. For example, in the board:

```
    1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
 1 [ ][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 2 [W][B][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 3 [W][B][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 4 [ ][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 5 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 6 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 7 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 8 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 9 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
10 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
11 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
12 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
13 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
14 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
15 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
16 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
```

If the white player inputs 3 1, the result would be:

```
    1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
 1 [ ][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 2 [W][ ][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 3 [W][ ][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 4 [ ][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 5 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 6 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 7 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 8 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 9 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
10 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
11 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
12 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
13 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
14 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
15 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
16 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
```

Note the the pieces would be taken off if they are at the boundaries too.
For example:

```
    1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
 1 [W][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 2 [W][B][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 3 [W][B][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 4 [B][B][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 5 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 6 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 7 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 8 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 9 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
10 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
11 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
12 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
13 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
14 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
15 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
16 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
```

Following the white move if the black player inputs 1 3, the board
will look like this:

```
    1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
 1 [ ][ ][B][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 2 [ ][B][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 3 [ ][B][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 4 [B][B][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 5 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 6 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 7 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 8 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 9 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
10 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
11 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
12 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
13 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
14 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
15 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
16 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
```

Here are some additional cases to consider:

```
    1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
 1 [B][B][B][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 2 [B][B][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 3 [B][B][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 4 [B][B][B][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 5 [B][B][ ][B][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 6 [W][B][B][B][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 7 [ ][W][B][B][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 8 [ ][W][B][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 9 [ ][W][B][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
10 [ ][ ][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
11 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
12 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
13 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
14 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
15 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
16 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]

Player W, enter the row and column (e.g., '2 2') to place your piece: 5 3

    1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
 1 [ ][ ][ ][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 2 [ ][ ][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 3 [ ][ ][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 4 [ ][ ][ ][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 5 [ ][ ][W][ ][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 6 [W][ ][ ][ ][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 7 [ ][W][ ][ ][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 8 [ ][W][ ][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 9 [ ][W][ ][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
10 [ ][ ][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
11 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
12 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
13 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
14 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
15 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
16 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]

```

At the end of the game, calculate the score of both players by counting the area they captured(surrounded on all sides)

#### Example 1

Consider this board:

```
    1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
 1 [ ][ ][ ][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 2 [ ][ ][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 3 [ ][ ][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 4 [ ][ ][ ][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 5 [ ][ ][W][ ][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 6 [W][ ][ ][ ][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 7 [ ][W][ ][ ][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 8 [ ][W][ ][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 9 [ ][W][ ][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
10 [ ][ ][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
11 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
12 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
13 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
14 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
15 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
16 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
```

In the board above, white's score would be:

```
    1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16
 1 [1 ][2 ][3 ][4 ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
 2 [5 ][6 ][7 ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
 3 [8 ][9 ][10][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
 4 [11][12][13][14][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
 5 [15][16][17][18][19][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
 6 [20][21][22][23][24][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
 7 [  ][25][26][27][28][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
 8 [  ][29][30][31][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
 9 [  ][32][33][34][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
10 [  ][  ][35][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
11 [  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
12 [  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
13 [  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
14 [  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
15 [  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
16 [  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]

35 + 6 = 41 points
```

#### Example 2

```
    1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16
 1 [ ][ ][ ][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 2 [ ][ ][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 3 [ ][ ][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 4 [ ][ ][ ][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 5 [ ][ ][W][ ][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 6 [W][ ][ ][ ][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 7 [ ][W][ ][ ][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 8 [ ][W][ ][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
 9 [ ][W][ ][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
10 [ ][ ][W][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
11 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
12 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][B][B][ ][ ][ ]
13 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][B][ ][ ][B][ ][ ]
14 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][B][ ][B][ ][ ]
15 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][B][W][ ][ ]
16 [ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ][ ]
```

In the board above, white's score would be:

```
    1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16
 1 [1 ][2 ][3 ][4 ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
 2 [5 ][6 ][7 ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
 3 [8 ][9 ][10][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
 4 [11][12][13][14][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
 5 [15][16][17][18][19][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
 6 [20][21][22][23][24][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
 7 [  ][25][26][27][28][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
 8 [  ][29][30][31][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
 9 [  ][32][33][34][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
10 [  ][  ][35][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
11 [  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
12 [  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
13 [  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
14 [  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
15 [  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][36][  ][  ]
16 [  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]

36 + 6 = 42 points
```

Black's score would be:

```
    1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16
 1 [  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
 2 [  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
 3 [  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
 4 [  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
 5 [  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
 6 [  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
 7 [  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
 8 [  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
 9 [  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
10 [  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
11 [  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]
12 [  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][1 ][2 ][  ][  ][  ]
13 [  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][3 ][4 ][5 ][6 ][  ][  ]
14 [  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][7 ][8 ][9 ][  ][  ]
15 [  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][10][  ][  ][  ]
16 [  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ][  ]

10 points
```

so the winner is white
