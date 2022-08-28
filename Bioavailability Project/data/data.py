import numpy as np

path = "https://raw.githubusercontent.com/gnmg7900/cifo_project/main/"

# Using numpy loadtxt to load the text files into a matrix format
inputs = np.loadtxt(path + "inputs.txt", dtype = float)
target = np.loadtxt(path + "target.txt", dtype = float)

print(inputs.shape), print(target.shape)