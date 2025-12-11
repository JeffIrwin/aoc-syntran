import numpy as np
from scipy.optimize import milp, LinearConstraint, Bounds

# Reference:
#
#     https://docs.scipy.org/doc/scipy/reference/generated/scipy.optimize.milp.html
#
# I have modified the example to do the first line of the test input here

# Objective function coefficients
#
# All button presses are weighted equally. We just want to minimize the total
# number of button presses
c = np.array([1, 1, 1, 1, 1, 1])
#c = np.array([-1, 4])

# Constraint matrix and bounds
#A = np.array([[-3, 1], [1, 2]])
A = np.array([
    [0, 0, 0, 1],
    [0, 1, 0, 1],
    [0, 0, 1, 0],
    [0, 0, 1, 1],
    [1, 0, 1, 0],
    [1, 1, 0, 0],
])
A = np.transpose(A)

#b_l = [-np.inf, -np.inf] # No lower bound on constraints
#b_u = [6, 4] # Upper bounds
b_l = [3, 5, 4, 7]
b_u = b_l

# Variable bounds
#bounds = Bounds([0, -3], [np.inf, np.inf]) # x[0] >= 0, x[1] >= -3
bounds = Bounds(
    [0, 0, 0, 0, 0, 0],
    [np.inf, np.inf, np.inf, np.inf, np.inf, np.inf]
)

# Integrality constraints (both variables are integers)
integrality = [1, 1, 1, 1, 1, 1]

# Linear constraints
constraints = LinearConstraint(A, b_l, b_u)

# Solve MILP
result = milp(c=c, integrality=integrality, bounds=bounds, constraints=constraints)

# Display results
print("Optimal Solution:", result.x)
print("Optimal Value:", result.fun)

