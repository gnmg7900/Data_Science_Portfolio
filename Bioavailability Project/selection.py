from random import uniform, sample
from operator import attrgetter
import numpy as np


def fps(population):
    """Fitness proportionate selection implementation.

    Args:
        population (Population): The population we want to select from.

    Returns:
        Individual: selected individual.
    """

    if population.optim == "min":
        # Sum total fitness
        total_fitness = sum([(1/i.fitness) for i in population])
        # Get a 'position' on the wheel
        spin = uniform(0, total_fitness)
        position = 0
        # Find individual in the position of the spin
        for individual in population:
            #using the inverse of the fitness as discussed in the report
            position += (1/individual.fitness)
            if position > spin:
                return individual

    elif population.optim == "max":
        # Our bioavailability problem is a minimization problem!
        # Sum total fitness
        total_fitness = sum([i.fitness for i in population])
        # Get a 'position' on the wheel
        spin = uniform(0, total_fitness)
        position = 0
        # Find individual in the position of the spin
        for individual in population:
            position += individual.fitness
            if position > spin:
                return individual

    else:
        raise Exception("No optimization specified.")


def tournament(population, size=50):
    """Tournament selection implementation.

    Args:
        population (Population): The population we want to select from.
        size (int): Size of the tournament.

    Returns:
        Individual: Best individual in the tournament.
    """

    # Select individuals based on tournament size
    tournament = sample(population.individuals, size)
    # Check if the problem is max or min
    if population.optim == 'min':
        return min(tournament, key=attrgetter("fitness"))

    elif population.optim == "max":
        # Our bioavailability problem is a minimization problem!
        return max(tournament, key=attrgetter("fitness"))
    else:
        raise Exception("No optimization specified.")

def ranking(population):
    """ Ranking selection Implementation

    Args:
        population (Population): The population we want to select from.
        size(int): size of the population

    Returns:
        Individual: selected individual.
    """
    if population.optim == "min":
        # sorting the fitnesses
        sorted_fitnesses = sorted(population, key=attrgetter("fitness"), reverse = True)
        # Total fitness for the ranking algorithm
        total_fitness = (len(population) + 1) * len(population) / 2

        ranking_scores = [i / total_fitness for i in range(0, len(population))]

        spin = uniform(0, sum(ranking_scores))
        position = 0

        # Find the individual in the position of the spin
        for index, rank in enumerate(ranking_scores):
            position += rank
            if position > spin:
                return sorted_fitnesses[index]

    elif population.optim == "max":
        # Our bioavailability problem is a minimization problem!
        # sorting the fitnesses
        sorted_fitnesses = sorted(population, key=attrgetter("fitness"), reverse=True)
        # Total fitness for the ranking algorithm
        total_fitness = (len(population) + 1) * len(population) / 2

        ranking_scores = [i / total_fitness for i in range(0, len(population))]

        spin = uniform(0, sum(ranking_scores))
        position = 0

        # Find the individual in the position of the spin
        for index, rank in enumerate(ranking_scores):
            position += rank
            if position > spin:
                return sorted_fitnesses[index]

    else:
        raise Exception("No optimization specified.")








