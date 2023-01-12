from charles.charles import Population, Individual
from data.data import inputs,target
from copy import deepcopy
from charles.selection import fps, tournament, ranking
from charles.mutation import swap_mutation, inversion_mutation, insertion_mutation
from charles.crossover import single_point_co,cycle_co, pmx_co, arithmetic_co

import random as rand
import numpy as np
import math
from sklearn.metrics import mean_squared_error

# Defining the fitness function

def get_fitness(self):
    """A simple objective function to calculate RMSE

    Returns:
        int: the total RMSE
    """
    predict_values=[]      # Create an empty list
    for i in range(len(inputs)):
        # In each iteration, multiply from Beta1 until BetaN element wise with the first input row
        # and after that append Beta0

        temp = np.sum(np.multiply(self.representation[1:], inputs[i])) + self.representation[0]
        predict_values.append(temp)  # Append to the predicted values list
    # RMSE using SKlearn from the predicted values vs the actual (target), and round it to 2 decimal cases
    rmse = round(math.sqrt(mean_squared_error(target,predict_values)),2)

    return rmse


# Monkey patching
Individual.get_fitness = get_fitness

# statistical testing (5 runs)
#for i in range(5):

pop = Population(
    size=100,
    sol_size=(len(inputs[0])+1),
    valid_set = np.round(np.arange(0, 0.1, 0.001).tolist(), 3),
    replacement=True,
    optim="min",
)


pop.evolve(
    gens=100,
    select=tournament,
    crossover=single_point_co,
    mutate=swap_mutation,
    co_p=0.8,
    mu_p=0.2,
    elitism=True
)

    # Appending the results to txt files in order to plot them later

    #fitnesses=[]
    #[fitnesses.append(pop.fitness_list[i].fitness) for i in range(100)]

    #f = open(r"/Users/goncalogomes/Documents/NOVA_IMS/2º_Semester/Computational_Inteligence/charles backup/Results10.txt", "a")
    #f.write(f"\n")
    #for d in fitnesses:
        #f.write(f"{d}")
        #f.write(' ')
    #f.close()
