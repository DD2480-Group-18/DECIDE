from typing import List

from globals import NUMPOINTS, PI, X, Y
from types import Parameters

from helpers import get_angle, get_area, line_two_points, dist_point_line, dist_two_points


def create_cmv(params: Parameters) -> List[bool]:
    cmv = [False] * 15
    if condition0(params):
        cmv[0] = True
    if condition1(params):
        cmv[1] = True
    if condition2(params):
        cmv[2] = True

    if condition3(params):
        cmv[3] = True
    if condition4(params):
        cmv[4] = True
    if condition5():
        cmv[5] = True
    if condition6(params):
        cmv[6] = True
    if condition7(params):
        cmv[7] = True
    if condition8(params):
        cmv[8] = True
    if condition9(params):
        cmv[9] = True
    if condition10(params):
        cmv[10] = True
    if condition11(params):
        cmv[11] = True
    if condition12(params):
        cmv[12] = True
    if condition13(params):
        cmv[13] = True
    if condition14(params):
        cmv[14] = True

    return cmv


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
