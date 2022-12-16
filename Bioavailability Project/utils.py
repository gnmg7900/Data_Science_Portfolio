from matplotlib import pyplot as plt
import numpy as np
import pandas as pd

results_path = "/Users/goncalogomes/Documents/NOVA_IMS/2º_Semester/Computational_Inteligence/cifo_project/Results/"

text1 = np.loadtxt(results_path + "Results1.txt", dtype= float)
df1 = pd.DataFrame(text1)
results1 = np.reshape(np.array(df1.mean()), 100)

text2 = np.loadtxt(results_path + "Results2.txt", dtype= float)
df2 = pd.DataFrame(text2)
results2 = np.array(df2.mean())

text3 = np.loadtxt(results_path + "Results3.txt", dtype= float)
df3 = pd.DataFrame(text3)
results3 = np.array(df3.mean())

text4 = np.loadtxt(results_path + "Results4.txt", dtype= float)
df4 = pd.DataFrame(text4)
results4 = np.array(df4.mean())

text5 = np.loadtxt(results_path + "Results5.txt", dtype= float)
df5 = pd.DataFrame(text5)
results5 = np.array(df5.mean())

text6 = np.loadtxt(results_path + "Results6.txt", dtype= float)
df6 = pd.DataFrame(text6)
results6 = np.array(df6.mean())

text7 = np.loadtxt(results_path + "Results7.txt", dtype= float)
df7 = pd.DataFrame(text7)
results7 = np.array(df7.mean())

text8 = np.loadtxt(results_path + "Results8.txt", dtype= float)
df8 = pd.DataFrame(text8)
results8 = np.array(df8.mean())

text9 = np.loadtxt(results_path + "Results9.txt", dtype= float)
df9 = pd.DataFrame(text9)
results9 = np.array(df9.mean())

text10 = np.loadtxt(results_path + "Results10.txt", dtype= float)
df10 = pd.DataFrame(text10)
results10 = np.array(df10.mean())

# Ploting the results over 100 generations

x = np.array(range(100))

# all models plot
plt.figure(figsize = (10, 6))
plt.plot(x, results1, label = "Fps/Single_Point/Swap")
plt.plot(x, results2, label = "Fps/Arithmetic/Swap")
plt.plot(x, results5, label = "Fps/Single_Point/Inversion")
plt.plot(x, results6, label = "Fps/Arithmetic/Inversion")
plt.plot(x, results3, label = "Tournament/Single_Point/Swap")
plt.plot(x, results4, label = "Tournament/Arithmetic/Swap")
plt.plot(x, results7, label = "Tournament/Single_Point/Inversion")
plt.plot(x, results8, label = "Tournament/Arithmetic/Inversion")
plt.plot(x, results9, label = "Ranking/Single_Point/Swap")
plt.plot(x, results10, label = "RankingArithmetic/Swap")
plt.xticks([1, 20, 40, 60, 80, 100])
plt.xlim(0)
plt.xlabel("Generations")
plt.ylabel("Fitness")
plt.legend(bbox_to_anchor = (1.05, 1), loc = "upper left")
plt.tight_layout(pad = 3)
plt.title("All models")
plt.show()

# Best ones Plot
plt.figure(figsize = (10, 6))
plt.plot(x, results5, label = "Fps/Single_Point/Inversion")
plt.plot(x, results3, label = "Tournament/Single_Point/Swap")
plt.plot(x, results9, label = "Ranking/Single_Point/Swap")
plt.xticks([1, 20, 40, 60, 80, 100])
plt.xlim(0)
plt.xlabel("Generations")
plt.ylabel("Fitness")
plt.legend(bbox_to_anchor = (1.05, 1), loc = "upper left")
plt.tight_layout(pad = 3)
plt.title("Best Models")
plt.show()


