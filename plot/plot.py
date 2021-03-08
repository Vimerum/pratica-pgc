from mpl_toolkits import mplot3d
from mpl_toolkits.mplot3d import axes3d
from fractions import Fraction
import math
import numpy as np
import matplotlib.pyplot as plt

SIZE = 5
PICK = 2
STEPS = np.linspace(-2, 2, SIZE)


def gaussian(x, y, sigma=1.0):
    lhs = 1.0 / (2*math.pi*pow(sigma, 2))
    rhs = math.exp(-(pow(x, 2) + pow(y, 2))/(2*pow(sigma, 2)))
    return lhs * rhs


print("Gerando X\t[..]", end='\r')
X = np.array([STEPS for _ in range(SIZE)])
print("Gerando X\t[OK]")
print("Gerando Y\t[..]", end='\r')
Y = np.array([[line for _ in range(SIZE)] for line in STEPS])
print("Gerando Y\t[OK]")
print("Gerando Z\t[..]", end='\r')
Z = np.array([[gaussian(x, y) for x in STEPS] for y in STEPS])
print("Gerando Z\t[OK]")
print(Z)

half = math.floor(SIZE / 2.0)
w = Z[half - PICK][half - PICK]
w = Z[0][0]
f = 1.0 / w
soma = 0
print(w)
print("1 / {0:d}".format(int(f)))
for x in range(half - PICK, half + PICK + 1, 1):
    for y in range(half - PICK, half + PICK + 1, 1):
        curr = int(Z[x][y] * f)
        soma += curr
        print("{0:2d} ".format(curr), end='')
    print()
print()
print("1 / {0:d}".format(int(soma)))

fig = plt.figure()
ax = plt.axes(projection='3d')
ax.plot_wireframe(X, Y, Z, color='black')
ax.set_xlabel("X")
ax.set_ylabel("Y")
ax.set_zlabel("G(x,y)")
ax.set_title("Distribuição Gaussiana")
plt.show()
