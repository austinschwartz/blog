---
title: Competitive Programming for Interview Practice
excerpt: In my opinion, more students should consider Competitive Programming to help practice for interviews
tags: competitive-programming, interviews
math: true
---

$\frac{1}{\Bigl(\sqrt{\phi \sqrt{5}}-\phi\Bigr) e^{\frac25 \pi}} = 1+\frac{e^{-2\pi}} {1+\frac{e^{-4\pi}} {1+\frac{e^{-6\pi}} {1+\frac{e^{-8\pi}} {1+\cdots} } } }$

ok hi there $\left( \sum_{k=1}^n a_k b_k \right)^2 \leq \left( \sum_{k=1}^n a_k^2 \right) \left( \sum_{k=1}^n b_k^2 \right)$ lol

https://www.reddit.com/r/programming/comments/44kpzk/peter_norvig_being_good_at_programming/

> Being good at programming competitions correlates negatively with being good on the job

This kind of sentiment comes up frequently on (reddit, hacker news)


All of these claims from Google that say competition performance hurts or that GPA doesn't matter are missing one huge thing: selection bias.
Google only sees the performance of the employees that it hires, not the performance of the employees that it doesn't hire. Because of this, the data they analyze is statistically biased: all data is conditioned on being employed by Google. So when Google says things like "GPA is not correlated with job performance" what you should hear is "Given that you were hired by Google, GPA is not correlated with job performance."
In general, when you have some thresholding selection, it will cause artificial negative correlations to show up. Here's a very simple example that I hope illustrates the point: Imagine a world where high school students take only two classes, English and Math, and they receive one of two grades, A or B. Now imagine a college that admits students with at least one A (AB, BA, or AA) and that rejects everyone without an A (BB). Now imagine that there is absolutely zero correlation between Math and English - performance on one is totally independent of the other. However, when the college looks at their data, they will nonetheless see a stark anticorrelation between Math and English grades (because everyone who has a B in one subject always has an A in the other subject, simply because all the BBs are missing from their dataset).
When Google says that programming competitions are negatively correlated with performance and GPA is uncorrelated with performance, what that likely means is that Google's hiring overvalues programming competitions and fairly values GPA. 


Essentially, the way that this study was interpreted is highly flawed. Google put a lot of emphasis on hiring those with Competitive Programming experience, then checked among those employees, whether Competitive Programming was a predictor of their success at the Company. They determined that it was not, and this can be attributed to those people being exceptional in other ways.

Another problem with interpreting that study this way is that the feature they used was “did this person participate in a contest?” and not “did this person do well in contests?”

There are also problems with using machine learning for attribution like this. Suppose that people who did well in contests also did well in coding questions on interviews. Then much of the “contestants are good” signal would be captured by the “people who did well in interviews” feature in the model.

Peter Norvig has been citing this example for years. I don’t believe he has actually taken the time to understand why the results were the way they were.

