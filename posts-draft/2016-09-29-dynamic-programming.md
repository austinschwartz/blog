---
title: Dynamic Programming Primer
author: Austin
tags: competitive-programming, dynamic-programming
---

This blog post has been adapted from lecture notes I wrote while teaching Competitive Programming at Purdue University during the Fall 2016 semester. I tend to gravitate towards an example-heavy teaching style, as I feel thats the way I learn the best. 

I've layed out a few examples of problems that tend to build off of each other. However, dynamic programming is a huge topic, and thus theres likely a lot missing from this. Towards the end of this post I'll be including links to other resources where you'll be able to find more information. ...

Example 1: Climbing Stairs
-------------------------

You are climbing a staircase. It takes n steps to reach to the top. Each time you can either climb 1 or 2 steps. In how many distinct ways can you climb to the top?

Lets first think of what the first couple results would be. Given `n = 1`, we only have one way of climbing those steps, and thats by taking a single step. Given `n = 2`, we have two ways, one where we take 2 single steps, and one where we take a single step of size 2. Given `n = 3`, we have three ways: `1, 1, 1`, `1, 2`, and `2, 1`. Given `n = 4`, we can make the following series of steps: `1, 1, 1, 1`, `1, 1, 2`, `2, 1, 1`, `1, 2, 1`, or `2, 2` for a total of 5 ways.

At this point you should start seeing a pattern. .... We can model this recursively! The number of ways to climb `n` steps is equal to the number of ways to climb `n - 1` steps + the number of ways of climbing `n - 2` steps. So `f(n) = f(n - 1) + f(n - 2)`
<center>
$f(1) = 1$<br/>
$f(2) = 2$<br/>
$f(n) = f(n - 1) + f(n - 2)$
</center>

Simple recursion
``` java
int f(int n) {
  if (n <= 2)
    return n;
  return f(n - 1) + f(n - 2);
}
```

Top-down (start at n, end at 0)
``` java
int[] dp = new int[n + 1];
Arrays.fill(dp, -1);
return f(dp, n);

int f(int[] dp, int n) {
  if (dp[n] != -1)
    return dp[n];
  if (n <= 2)
    return dp[n] = n;
  return dp[n] = f(n - 1) + f(n - 2);
}
```

Bottom-up (start at 0, end at n)
``` java
int f(int n) {
  int[] dp = new int[n + 1];
  dp[0] = 0;
  dp[1] = 1;
  dp[2] = 2;
  if (n <= 2)
    return dp[n];

  for (int i = 3; i <= n; i++) {
    dp[i] = dp[i - 1] + dp[i - 2];
  }
  return dp[n];
}
```

Bottom-up, O(1) space, realizing we don't need a table
``` java
int f(int n) {
  if (n <= 2)
    return n;
  int a = 1;
  int b = 2;
  int result = 0;
  for(int i = 3; i <= n; i++) {
    result = a + b;
    a = b;
    b = result;
  }
  return result;
}
```

Example 2 - Number of paths in a matrix
-------------

Given `m` and `n`, representing the width and height of a matrix, count the number of paths from the top left cell to the bottom right with the constraint that you can only move down + right.

<center>
$f(m, 0) = 1$ <br/>
$f(0, n) = 1$ <br/>
$f(m, n) = f(m - 1, n) + f(m, n - 1)$
</center>


Just showing bottom up this time
``` java
int numberOfPaths(int m, int n)
{
    // Create a 2D table to store results of subproblems
    int count[m][n];
 
    // Count of paths to reach any cell in first column is 1
    for (int i = 0; i < m; i++)
        count[i][0] = 1;
 
    // Count of paths to reach any cell in first column is 1
    for (int j = 0; j < n; j++)
        count[0][j] = 1;

    // Calculate count of paths for other cells in bottom-up manner using
    // the recursive solution
    for (int i = 1; i < m; i++)
        for (int j = 1; j < n; j++)
            count[i][j] = count[i-1][j] + count[i][j-1];

    return count[m-1][n-1];
}
```

Follow-ups for this problem:
  *   How would you do this if you had certain cells blocked off?
  *   Do this with just two rows
  *   How would you do this if each cell had a cost, and you wanted to find the min cost path, traversing from the top left to bottom right and you can still only go down and right?

Example 3 - Maximum Sum Subarray
--------------------

Given an array A of non-zero integers, find the maximum sum between two indices `i` and `j`. 

I.e. Given A = `[−2, 1, −3, 4, −1, 2, 1, −5, 4]`, the largest contiguous subarray sum is `[4, −1, 2, 1]` with sum `6`.

Note that if all numbers are positive, we can just return the sum of the entire array.

The naive method is to run two loops. The outer loop picks the beginning element, the inner loop finds the maximum sum with first element picked by outer loop and compares this maximum with the overall maximum. Finally return the overall maximum. 
The time complexity of the Naive method is $O(n^2)$.

``` java
int f(int[] A) {
  int max = -1;
  for (int i = 0; i < n; i++) {
    int sum = 0;
    for (int j = i; j < n; j++) {
      sum += A[j];
    }
    max = Math.max(max, sum);
  }
  return max;
}
```

With some realization, we can divide this up into multiple subproblems!

Viewing this as DP, at each step we have 2 choices. We can either leverage the previously accumulated maximum sum or begin a new range. 

The DP variable `dp[i]` represents the maximum sum of a range of integers that ends with element `A[i]`. 

Thus, the final answer is the maximum over all `dp[i]` for `i` in `[0..n-1]`.

```java
int f(int[] A) {
  int n = A.length, max = A[0];
  int[] dp = new int[n + 1];
  dp[0] = A[0];
  for (int i = 1; i < n; i++) {
    if (dp[i - 1] > 0)
      dp[i] = dp[i - 1] + A[i];
    else
      dp[i] = A[i];
    if (dp[i] > max)
      max = dp[i];
  }
  return max;
}
```

This solution would be $O(n)$ time and $O(n)$ space.

With even more realization, we don’t even need the table!

```java
int f(int[] A) {
  int n = A.length, max = A[0];
  int sum = 0;
  for (int i = 0; i < n; i++) {
    sum += A[i];
    max = Math.max(max, sum);
    if (sum < 0) sum = 0;
  }
  return max;
}
```

The intuition here is that we just need to keep a running sum of the integers seen so far, then greedily reset to 0 if the sum dips below 0.

This solution is $O(n)$ time, $O(1)$ space

Example 3 - House Robber
-----------------

You are a professional robber planning to rob houses along a street. Each house has a certain amount of money stashed, the only constraint stopping you from robbing each of them is that adjacent houses have security system connected and it will automatically contact the police if two adjacent houses were broken into on the same night.

Ex: `[1, 3, 2, 6]`, we can take `1` and  `2` for a total of  `3`, `3 + 6 = 9`, or `1 + 6 = 7`, so the max we can take is `9` where we take `3` and `6`.

If we know the sum up to house `i - 1` and `i - 2`, we can easily calculate the sum for house `i`. Let’s call the sum of money we got up to house `i`, `f(i)`. 

  *   If we choose to rob house `i`, we have to exclude house `i - 1`. `f(i) = f(i - 2) + nums[i]`. 
  *   If we do not rob house `i`, `f(i) = f(i - 1)`. 

<center>
$f(0) = nums[0]$ <br/>
$f(1) = max(nums[0], nums[1])$ <br/>
$f(i) = max(f(i - 2) + nums[i], f(i - 1))$
</center>

```java
int f(int[] nums) {
  if(nums.length == 1)
    return nums[0];
  int n = nums.length;
  int[] sum = new int[n + 1];
  
  sum[0] = 0;
  sum[1] = nums[0];
  
  for (int i = 2; i <= n; i++)
    sum[i] = Math.max(sum[i - 1], sum[i - 2] + nums[i-1]);

  return sum[n];
}
```

Example 4 - House Robber 2
-------------------

Similar to the last problem, except now you have a tree $T$ with each node $i$ containing an amount of money $C_i$. Find the maximum sum that a robber can take where no two adjacent nodes (meaning they have an edge between them) are chosen.

In the previous problem, we represented a subproblem sum[i] being the maximum sum obtained from 0 to i. We can do something similar with our tree.

With this example, we can solve for subtrees, saying sum[i] is the max sum obtained for subtree rooted at i. The result would then be found at sum[1].

We solve this similarly to the previous. We have to make a decision about including node V in our subset or not. 
Include node V: we can't include any of its children (say v1, v2, ..., vn), but we can include any grand child of V. 
Don't include V: we can include any child of V.


<center>
$dp(V) = max(\sum_{i=1}^n{dp(v_i)}, C_v + (\sum_{i=1}^n{\text{sum of } dp(j) \text{ for all children j of } v_i}))$
</center>

We can simplify this by splitting this into two recurrence relations, where dp1(V) is the maximum sum where we take V, and dp2(V) is the maximum sum where we don’t take V. 

<center>
$dp1(V) = C_v + \sum_{i=1}^n{dp2(v_i)}$ </br>
$dp2(V) = \sum_{i=1}^n{max(dp1(v_i), dp2(v_i))}$ </br>
$sum(V) = max(dp1(V), dp2(V))$
</center>


Then sum(V) = max(dp1(V), dp2(V))

Note: you can find code for the solution here.


