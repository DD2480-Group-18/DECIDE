import math


def dist(x: float, y: float) -> float:
    return math.sqrt(x ** 2 + y ** 2)


def get_angle(x1, y1, x2, y2) -> float:
    return math.acos((x1 * x2 + y1 * y2) / (dist(x1, y1) * dist(x2, y2)))


def get_area(x1, y1, x2, y2) -> float:
    return dist(x1, y1) * dist(x2, y2)


# Step 1: Find the slope using the formula:
# Step 2: Use the slope and one of the points to find the y-intercept b:
def line_two_points(x1: float, y1: float, x2: float, y2: float) -> [float]:
    m = (y2 - y1) / (x2 - x1)
    b = y2 - (m * x2)
    return [m, 1, b]


# Used the formula: https://brilliant.org/wiki/distance-between-point-and-line/
def dist_point_line(line: [float], x, y) -> float:
    return abs(line[0] * x + line[1] * y + line[2]) / math.sqrt(line[0] ** 2)


def dist_two_points(x1: float, y1: float, x2: float, y2: float):
    return math.sqrt((x1 - x2) ** 2 + (y1 - y2) ** 2)
