from typing import List

from globals import NUMPOINTS, PI, X, Y
from types import Parameters

from helpers import get_angle, get_area, line_two_points, dist_point_line, dist_two_points


def create_cmv(params: Parameters) -> List[bool]:
    return [condition0(params), condition1(params), condition2(params), condition3(params), condition4(params), condition5(params), condition6(params), condition7(params), condition8(params), condition9(params), condition10(params), condition11(params), condition12(params), condition13(params), condition14(params)]


def condition0(params):
    return 0 <= params.LENGTH1


def condition1(params):
    return 0 <= params.RADIUS1


def condition2(params):
    for i in range(0, NUMPOINTS - 3):
        # Create slice
        yslice = Y[i:i + 3]
        xslice = X[i:i + 3]
        # Create a, b vectors
        ax = xslice[0] - xslice[1]
        ay = yslice[0] - yslice[1]
        bx = xslice[2] - xslice[1]
        by = yslice[2] - yslice[1]
        angle = get_angle(ax, ay, bx, by)
        if angle < (PI - params.EPSILON) or angle > (PI + params.EPSILON):
            return True
    return False


def condition3(params: Parameters):
    for i in range(0, NUMPOINTS - 3):
        # Create slice
        yslice = Y[i:i + 3]
        xslice = X[i:i + 3]
        # Create a, b vectors
        ax = xslice[0] - xslice[1]
        ay = yslice[0] - yslice[1]
        bx = xslice[2] - xslice[1]
        by = yslice[2] - yslice[1]
        area = get_area(ax, ay, bx, by) / 2
        if 0 <= area <= params.AREA1:
            return True
    return False


def condition4(params: Parameters):
    in_quads = 0
    if params.Q_PTS >= params.QUADS:
        for i in range(NUMPOINTS):
            if X[i] >= 0 and Y[0] >= 0:
                in_quads += 1
            elif X[i] <= 0 and Y[0] >= 0:
                in_quads += 1
            elif X[i] <= 0 and Y[0] <= 0:
                in_quads += 1
            elif X[i] >= 0 and Y[0] <= 0:
                in_quads += 1
            else:
                in_quads = 0
            if in_quads > params.QUADS:
                return True
    return False


def condition5():
    for i in range(NUMPOINTS - 1):
        if X[i + 1] - X[i] < 0:
            return True
    return False


def condition6(params: Parameters):
    for i in range(NUMPOINTS - params.N_PTS):
        if X[i] == X[i + params.N_PTS - 1] and Y[i] == Y[i + params.N_PTS - 1]:
            for j in range(i + 1, i + params.N_PTS - 1):
                if dist_two_points(X[j], Y[j], X[i], Y[i]) > params.DIST:
                    return True
        else:
            line = line_two_points(X[i], Y[i], X[i + params.N_PTS - 1], Y[i + params.N_PTS - 1])
            for j in range(i + 1, i + params.N_PTS - 1):
                if dist_point_line(line, X[j], Y[j]) > params.DIST:
                    return True
    return False


def condition7(params: Parameters):
    return False


def condition8(params: Parameters):
    return False


def condition9(params: Parameters):
    return False


def condition10(params: Parameters):
    return False


def condition11(params: Parameters):
    return False


def condition12(params: Parameters):
    return False


def condition13(params: Parameters):
    return False


def condition14(params: Parameters):
    return False
