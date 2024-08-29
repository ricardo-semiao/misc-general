# GA Allocator for Preferences Optimization

Welcome! This is the repository for an attempt at building an optimization routine for a preference-based allocation problem, using generic algorithms.


## The Problem
The goal is to, based on preferences of $N$ individuals about joining one out of $K$ groups, generate the optimal allocation.

The problem is very flexible, where:

- Each group can have a different size;
- Groups can be left not perfectly filled or not filled at all;
- Individuals might not reveal their preferences about all groups.


## The Algorithm

This flexibility yields a very complex problem, so the approach was to use the inspiration of a parent-children generic algorithm (GA), but with custom functions built to accommodate this complexity.

Several algorithms were tried, but the here presented breaks the problem into two: at each iteration, cross/mutate the population (at the current group sizes), then cross/mutate the group sizes. The functions for each step are, respectively, at [repopulating.R](repopulating.R) and [resizing.R](resizing.R).

These iterations are done in the main loop described in [main.R](main.R), where there also is some dummy data for an example. In [utils.R](utils.R) there are some utility functions like a custom progress bar and a very interesting solution to find optimal childs using recursion.


## The Context

This project was an idea I had to add efficiency to [GVCode's](https://github.com/gvcode) selective process, where a similar allocation with the candidates was done by hand. Unfortunately, the algorithm didn't proved sufficiently easier than the by-hand process used at the time, such that the project could have had more improvements.
