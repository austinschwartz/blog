---
title: Dance Dance Revolution DP Problem
excerpt: A shell script which converts a music collection to MP3.
tags: competitive-programming, dynamic-programming
math: true
---

<img src="/images/ddr-chart.jpg" alt="Example DDR Chart" style="width: 200px; float: left;margin-right: 15px;"/>

I wrote this contest problem recently for the ACM ICPC qualifier at Purdue, to see which students we were taking to the contest. However, I didn't finish writing the problem statement in time for the contest. I still wanted to put it out there somewhere, so I figured here might be a better place than any.

In the game Dance Dance Revolution, the player has a large game controller consisting of 4 directional buttons (up, down, left, right). The player uses their feet to step on these arrow buttons along with the beat of a song, matching up the steps with the song as the arrows on their screen move upwards. The picture to the left represents a "step chart" as they are called, and also shows the lines where these arrows are aligned. Lines can consist of single arrows (also called notes), no arrows, or multiple arrows. 

Collections of 2+ arrows are called jumps as its expected that the player jumps to hit them, although most step charts don't include 3 or 4 arrow jumps. There are an additional category of step charts that are meant for players using a keyboard as a controller, and these often have 3/4 arrow jumps, but we won't be using them.

###Problem Statement
For this problem, we're going to calculate the number of possible step charts of length `i`, given the constraints that no two consecutive lines can contain jumps, and we only use jumps of two arrows. Many DDR players don't like having to jump back to back so fast, so we'll try to avoid that.

If we think about this for awhile, we might realize that doing this recursively might be the easiest. We can build `charts(i)`, meaning the # of charts possible up to and including line i, by using the results of `charts(i - 1)`. So the state of our problem includes the line we're currently on, but because of the constraint that we can't have consecutive jumps, we also need to keep track of whether the previous result used jumps or not. So our state needs to include a line number, `i`, and also an indicator of whether we use jumps or not on this line, `j`. If `j = true` then we build line `i` only using jumps, and if `j = false` then we don't use any jumps.

Lets think of our base cases first. Whenever `i = 0`, we have 0 charts of that length. If we have `i = 1`, then it depends on whether we allow jumps. If we are counting jumps, so `j = true`, then we have 6 jump patterns available (`<v.. <.^. <..> .v^. .v.> ..^>`). If `j = false` then we have `4` different single arrow lines we can use (`<... .v.. ..^. ...>`), and `1` additional blank line, where we have no arrows. These are our three base cases.

Then for our actual recursion, if `j = true`, then we use jumps to represent this line, and multiply that `6` by `charts(i - 1, false)` as its the number of ways to represent the board as of `i-1` where line `i - 1` doesn't contain any jumps. 

Then for the other case where `j = false`, we don't use jumps so we multiply `5` times the sum of `charts(c - 1, false)` and `charts(c - 1, true)` since we don't care if the previous line has jumps or not.

The recursive relation itself looks like this:

$$
c(i, j) = 
\begin{cases}
    0,& \text{if } i = 0\\
    5,& \text{if } i = 1 \text{ and } j = \text{ false}\\
    6,& \text{if } i = 1 \text{ and } j = \text{ true}\\
    6 * c(i - 1, \text{false}),& \text{if } j = \text{true}\\
    5 * (c(i - 1, \text{false}) + c(i - 1, \text{true})),& \text{if } j = \text{false}
\end{cases}
$$

Actual code would look like this (assuming you have a table `dp` of the right size filled in with 0s):
```java
public static long charts(int i, int j) {
  if (i == 0) dp[i][j] = 0;
  if (i == 1 && j == 1) dp[i][j] = 6;
  if (i == 1 && j == 0) dp[i][j] = 5;

  if (dp[i][j] != -1) 
    return dp[i][j];

  if (j == 1)
    return dp[i][j] = 6 * charts(i - 1, 0);
  else
    return dp[i][j] = 5 * (charts(i - 1, 1) + charts(i - 1, 0));
}
```

The solution to the problem is then `charts(i, 0) + charts(i, 1)`. We can build this easily using recursion, and also add in some memoization to keep track of previous states, as otherwise we'd be recomputing the same values over and over. 

I also wrote another recursive function to generate all possible boards of length `n`, and display them.

First, here are the results for `n = 2`, then after that is the code (Also available on ideone at [http://ideone.com/BZekqf](http://ideone.com/BZekqf)).
```
85
....
....

....
<...

....
.^..

etc
```

```java
public static String boardListToString(List<List<Short>> boards) {
  StringJoiner ret = new StringJoiner("\n");
  for (List<Short> board : boards) {
    StringJoiner sj = new StringJoiner("\n");
    for (short line : board)
      sj.add(lineToString(line));
    ret.add(sj.toString() + "\n");
  }
  return ret.toString();
}

public static boolean conflicts(List<Short> board, short line) {
  if (board == null || board.size() == 0)
    return false;

  short lastRow = board.get(board.size() - 1);
  if (hasJumps(lastRow) && hasJumps(line))
    return true;

  return false;
}

public static boolean hasJumps(short line) {
  return Integer.bitCount(line) > 1;
}

public static List<List<Short>> genBoardsOfLength(int n) {
  List<List<Short>> ret = new ArrayList<>();
  if (n == 0) {
    return ret;
  } else if (n == 1) {
    for (short line: genRows()) {
      List<Short> board = new ArrayList<Short>();
      board.add(line);
      ret.add(board);
    }
    return ret;
  } else {
    List<List<Short>> prevBoards = genBoardsOfLength(n - 1);
    for (List<Short> board : prevBoards) {
      for (short line : genRows()) {
        if (conflicts(board, line))
          continue;
        List<Short> newBoard = new ArrayList<Short>(board);
        newBoard.add(line);
        ret.add(newBoard);
      }
    }
  }
  return ret;
}


public static String lineToString(short line) {
  StringBuilder sb = new StringBuilder();
  int count = Integer.bitCount(line);
  sb.append((line & 1) > 0 ? '<' : '.');
  sb.append((line & 2) > 0 ? '^' : '.');
  sb.append((line & 4) > 0 ? 'v' : '.');
  sb.append((line & 8) > 0 ? '>' : '.');
  return sb.toString();
}

public static List<Short> genRows() {
  List<Short> ret = new ArrayList<>();
  for (short i = 0; i < (short)Math.pow(2, 4); i++)
    if (Integer.bitCount(i) <= 2)
      ret.add(i);
  return ret;
}
```
