# Guessing Game Solver
This project was created for a declarative programming subject and involved implementing a solver for a battle-ship like guessing game. The game is a 2-player guessing game where the 'hider' selects 3 locations on a 4x8 grid and the 'searcher' tries to guess all three locations (targets) in a single guess, in the lowest possible attempts. After each guess the 'searcher' receives feedback based on this guess, which can be used help with the next guess. The feedback provides the number of targets correctly identifed, the number of guesses exactly one space away from a target and the number of guesses two spaces away. Note that diagonals are included in these range.

## Approach Taken
The approach taken involved initially storing all possible 3 location combinations and for each subsequent move, reducing this search space based on the prior feedback. This required removing any possible targets that were 'inconsistent' with this feedback meaning any that produced different feedback when compared with the previous guess (indicating cannot be the correct one). The choice of next guess from these reduced possible targets was based on the lowest expected remaining targets out of each of these possible targets. To achieve this each possible target was selected and feedback was computed between this selected target and all other possibilities and these feedbacks were then grouped. Since feedback is indicative of remaining possible moves, the counts of these feedbacks could be used to find the average remaining possible moves for a single potential target (using the avgTargets function). 

To reduce the computation required for this, the optimal initial move was determined by finding the average remaining targets after the first move for each possible first guess. This involved running around 25 million playouts which was achieved, using a bash script and occupying all CPU cores. For the lowest few, the game was ran again on each target with number of guesses averaged, which eventually resulted in the best initial guess of "B1 H2 H4". The code used for this testing is within the `testing_init_guesses` directory and I have included example individual test results for reference (`results_B1 H2 H4_full.csv` and `results_B1 H2 H4_paring.csv`) where the title indicates the initial guess being tested. The best initial guess with the most optimised solver resulted in an average of 4.93 guesses over all 4960 test cases with the distribution shown in the provided histogram.

## Usage
The project can be interpreted using GHCi or compiled by running:
```
ghc -o Game Main.hs
```
And subsequently executed with an inputted test case using:
```
./Game <test-case>
```
For example `./Game "A1 C3 E2"`.
The testing code was implemented in `Main.hs` in a previous commit.