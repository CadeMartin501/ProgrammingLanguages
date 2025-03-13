import random
import math

def estimate_pi(num_points):
    """
    Estimates Pi using a Monte Carlo simulation.

    Args:
        num_points: The number of random points to generate.

    Returns:
        An estimated value of Pi.
    """

    points_inside_circle = 0
    for _ in range(num_points):
        # Generate random x and y coordinates between -1 and 1
        x = random.uniform(-1, 1)
        y = random.uniform(-1, 1)

        # Check if the point is within the unit circle (x^2 + y^2 <= 1)
        if math.sqrt(x**2 + y**2) <= 1:
            points_inside_circle += 1

    # Estimate Pi using the ratio of points inside the circle to total points
    pi_estimate = 4 * (points_inside_circle / num_points)
    return pi_estimate

# Example usage with 100,000 points
num_points = 20000000
pi_approx = estimate_pi(num_points)
print(f"Estimated value of Pi: {pi_approx}")