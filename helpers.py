import math


def dist(x: float, y: float) -> float:
    return math.sqrt(x^2 + y^2)

def get_angle(x1, y1, x2, y2) -> float:
    return math.acos((x1*x2 + y1*y2) / (dist(x1, y1) * dist(x2, y2)))

def get_area(x1, y1, x2, y2) -> float:
    return (dist(x1, y1) * dist(x2, y2))
