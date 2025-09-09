# Drug Discovery with Genetic Algorithms


*Problem type:* Apply a genetic algorithm (GA) to solve an optimization problem 


## General Context 
Drug discovery is often a very laborious and expensive process, namely when trying to predict pharmacological characteristics of new compounds, such as toxicity or bioavailability. Starting from a dataset of known chemical compounds bioavailability, the goal is to generalize by finding a function that makes a prediction of the
bioavailability with the minimum error possible. To tackle the problem, a multiple linear regression model has been implemented, to find the coefficients (slopes) values of each molecular descriptor, that minimizes the error between the prediction and the compared target. As a fitness function, the root mean square (RMSE) has been used.


## Project Overview
1. Implement a combination of different genetic algorithms , including some selection methods and some mutation and crossover operators.
2. Implement a fitness function. 
3. Compare and report the convergence to the optimized solution using different genetic algorithms.
