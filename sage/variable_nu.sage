# Import modules etc.
import os
import numpy as np
from scipy.stats import beta
import scipy.misc as misc
from scipy.misc import derivative
import matplotlib.pyplot as plt
from scipy.integrate import quad


###############################
# Assume nu^2 ~ Beta(a,b), and 
# enforce E[nu^2] = 1/4 (implies E[nu] ~ 1/2)
# The requires nu^2 ~ Beta(b/3, b)

# Define symbols
a, b = var('a b')

# Mean of the Beta distribution: a/(a + b)
mean_expr = a / (a + b)

# Solve mean_expr == 1/4
solve(mean_expr == 1/4, a)

# Define beta and x
Beta, x = var('Beta x')

# Solve the inequality
solve(sqrt(Beta) <= x, Beta)


###############################
# Define the Beta CDF function
def beta_cdf_x_squared(x, a, b):
    return beta.cdf(x**2, a, b)

# Define a function to compute the derivative of the Beta CDF with respect to x
def beta_cdf_derivative(x, a, b):
    return misc.derivative(lambda x: beta_cdf_x_squared(x, a, b), x, dx=1e-6)

# Example parameters for a and b
a = 2
b = 3
x = 0.5

# Compute the derivative at x
derivative_at_x = beta_cdf_derivative(x, a, b)
print(derivative_at_x)


###############################
# Plot nu^2 ~ Beta(b/3, b) for different values of b
# to see possible distributions.

# List of b values
bs = [0.5, 1, 1.5, 5, 50]

# Define x values for plotting
x_vals = np.linspace(0.0001, 0.9999, 10000)

# Create a plot
plt.figure(figsize=(8, 6))

# Loop over each value of b
for b_value in bs:
    # Define the CDF of Beta distribution with parameters a = b/3 and b
    a = b_value / 3
    
    # Define a lambda function to calculate the derivative of the CDF
    def cdf_derivative(x):
        return derivative(lambda x_val: beta.cdf(x_val^2, a, b_value), x, dx=1e-6)
    
    # Compute the derivative of the CDF for each x in x_vals
    y_vals = np.array([cdf_derivative(x) for x in x_vals])
    
    # Plot the derivative for the current b value
    plt.plot(x_vals, y_vals, label=f'b={b_value}', linewidth=2)

# Add legends and labels
plt.legend(title="b values")
plt.title("Derivative of CDF of Beta Distribution for Different b Values")
plt.xlabel("x")
plt.ylabel("Derivative of CDF")

# Adjust the plot range to ensure all lines are visible
plt.ylim([0, 10])


#######
# Export Lines as .csv files for plotting in R
# os.chdir('/path/to/wd')
#os.chdir('/Users/c-olito/Documents/Projects/FGM-BalancingSel-Selfers/sage/')
ax = plt.gca() # get axis handle

# Loop over b values and export data
for l in range(5):
    line = ax.lines[l]
    dat = line.get_xydata()
    filename = "nu_squared_Dist_b" + str(n(bs[l], digits=1)) + ".csv"
    np.savetxt(filename, dat, delimiter=",")
#######

# Display the plot
plt.show()


###############################
# Quick test: Calculate expectation of a function with a Beta-distributed variable
# Define the expectation expression (simplified version for demonstration)
def integrand(x, F, b):
    # Here we assume Nu^2 = Beta distributed with parameters b/3 and b
    return (1 - F**2) * x * (1 - x) / ((F + (1 - F) * x) * (1 - (1 - F) * x))

# Parameters
b = 3
F = 0.5

# Perform the integration to compute the expectation
result, _ = quad(integrand, 0, 1, args=(F, b))
print(result)




###############################
# Compute E[R_new] ~ F
# Plot the Expectation of R_new ~ F for different distributions of nu**2
# We integrate R_new over the distribution of nu**2 for each value of b 

# Define the integrand for the expectation
def R_new(nu_squared, F, b):
    # (1 - F^2) * nu_squared * (1 - nu_squared) / ((F + (1 - F) * nu_squared) * (1 - (1 - F) * nu_squared))
    return (1 - F**2) * nu_squared * (1 - nu_squared) / ((F + (1 - F) * nu_squared) * (1 - (1 - F) * nu_squared))

# Define the PDF of Beta distribution (nu^2 ~ Beta(b/3, b))
def beta_pdf(nu_squared, b):
    # Beta PDF with parameters b/3 and b
    return beta.pdf(nu_squared, b/3, b)

# List of b values
bs = [0.5, 1, 1.5, 5, 50]

# Define the F values range
F_vals = np.linspace(0, 1, 100)

# Create a plot
plt.figure(figsize=(8, 6))

# Iterate over each value of b
for b in bs:
    expectations = []  # Store the computed expectation values for each b
    for F in F_vals:
        # Perform the integration over nu_squared (0 to 1) for each F value and b
        expectation_value, _ = quad(lambda nu_squared: R_new(nu_squared, F, b) * beta_pdf(nu_squared, b), 0, 1)
        expectations.append(expectation_value)
    
    # Plot the expectation for each b with distinct line style
    plt.plot(F_vals, expectations, label=f'b={b}', linewidth=2)

# Add the line for Nu^2 = 1/4 (constant line)
nu_squared_1_4 = [(1 - F**2) * 1/4 * (1 - 1/4) / ((F + (1 - F) * 1/4) * (1 - (1 - F) * 1/4)) for F in F_vals]
plt.plot(F_vals, nu_squared_1_4, '--', label="Nu^2 = 1/4", color='black', linewidth=2)

# Add the line for uniform distribution (1 - F)
uniform_line = [1 - F for F in F_vals]
plt.plot(F_vals, uniform_line, label="uniform", color='gray', linewidth=2)

# Add legends and labels
plt.legend(title="b values")
plt.title("Expectation of $R_{new}$ for different distributions of $\\nu^2$")
plt.xlabel("F")
plt.ylabel("Expectation ($R_{new}$)")

# Adjust plot range to make sure all lines are visible
plt.ylim([-0.1, 1.1])

# Display the plot
plt.show()


# Export Lines
# os.chdir('/path/to/wd')
os.chdir('/Users/c-olito/Documents/Projects/FGM-BalancingSel-Selfers/sage/')
ax = plt.gca() # get axis handle

# Loop over b values and export data
for l in range(5):
    line = ax.lines[l]
    dat = line.get_xydata()
    filename = "R_new_b" + str(n(bs[l], digits=1)) + ".csv"
    np.savetxt(filename, dat, delimiter=",")













###############################
# Compute E[R_adapt] ~ F
# Plot the Expectation of R_adapt ~ F for different distributions of nu**2
# We integrate R_new over the distribution of nu**2 for each value of b 

# Define the integrand for the expectation
def R_adapt(nu_squared, F, b):
    # (1 - F^2) * nu * (1 - nu^2) / ((F + (1 - F) * nu) * (1 - (1 - F) * nu**2))
    return (1 - F**2) * sqrt(nu_squared) * (1 - nu_squared) / ((F + (1 - F) * sqrt(nu_squared)) * (1 - (1 - F) * nu_squared))

# Define the PDF of Beta distribution (nu ~ Beta(b/3, b))
def beta_pdf(nu_squared, b):
    # Beta PDF with parameters b/3 and b
    return beta.pdf(nu_squared, b/3, b)

# List of b values
bs = [0.5, 1, 1.5, 5, 50]

# Define the F values range
F_vals = np.linspace(0, 1, 100)

# Create a plot
plt.figure(figsize=(8, 6))

# Iterate over each value of b
for b in bs:
    expectations = []  # Store the computed expectation values for each b
    for F in F_vals:
        # Perform the integration over nu (0 to 1) for each F value and b
        expectation_value, _ = quad(lambda nu_squared: R_adapt(nu_squared, F, b) * beta_pdf(nu_squared, b), 0, 1)
        expectations.append(expectation_value)
    
    # Plot the expectation for each b with distinct line style
    plt.plot(F_vals, expectations, label=f'b={b}', linewidth=2)

# Add the line for Nu = 1/2 (constant line)
nu_1_2 = [(1 - F**2) * 1/2 * (1 - 1/2**2) / ((F + (1 - F) * 1/2) * (1 - (1 - F) * 1/2**2)) for F in F_vals]
plt.plot(F_vals, nu_1_2, '--', label="Nu = 1/2", color='black', linewidth=2)

# Add the line for uniform distribution (1 - F)
uniform_line = [1 - F for F in F_vals]
plt.plot(F_vals, uniform_line, label="uniform", color='gray', linewidth=2)

# Add legends and labels
plt.legend(title="b values")
plt.title("Expectation of $R_{adapt}$ for different distributions of $\\nu^2$")
plt.xlabel("F")
plt.ylabel("Expectation ($R_{adapt}$)")

# Adjust plot range to make sure all lines are visible
plt.ylim([-0.1, 1.5])

# Uncomment to Display the plot
plt.show()


# Export Lines
# os.chdir('/path/to/wd')
#os.chdir('/Users/c-olito/Documents/Projects/FGM-BalancingSel-Selfers/sage/')
ax = plt.gca() # get axis handle

# Loop over b values and export data
for l in range(5):
    line = ax.lines[l]
    dat = line.get_xydata()
    filename = "R_adapt_b" + str(n(bs[l], digits=1)) + ".csv"
    np.savetxt(filename, dat, delimiter=",")




