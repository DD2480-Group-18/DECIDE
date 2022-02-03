from typing import List
from custom_types import PI, Parameters, X, Y, NUMPOINTS
from math import atan, sqrt
from helpers import *


def create_cmv(params: Parameters) -> List[bool]:
    return [condition0(X, Y, NUMPOINTS, params.LENGTH1),
            condition1(X, Y, NUMPOINTS, params.RADIUS1),
            condition2(X, Y, NUMPOINTS, params.EPSILON),
            condition3(X, Y, NUMPOINTS, params.AREA1),
            condition4(X, Y, NUMPOINTS, params.Q_PTS, params.QUADS),
            condition5(X, NUMPOINTS),
            condition6(X, Y, NUMPOINTS, params.N_PTS, params.DIST),
            condition7(X, Y, NUMPOINTS, params.K_PTS, params.LENGTH1),
            condition8(X, Y, NUMPOINTS, params.A_PTS, params.B_PTS, params.RADIUS1),
            condition9(X, Y, NUMPOINTS, params.C_PTS, params.D_PTS, params.EPSILON),
            condition10(X, Y, NUMPOINTS, params.E_PTS, params.F_PTS, params.AREA1),
            condition11(params),
            condition12(params), condition13(params), condition14(params)]


def condition0(X, Y, NUMPOINTS, LENGTH1):
    for i in range(NUMPOINTS - 1):
        if dist_two_points(X[i], Y[i], X[i + 1], Y[i + 1]) >= LENGTH1:
            return True
    return False


def condition1(X, Y, NUMPOINTS, RADIUS1):
    for i in range(NUMPOINTS - 2):
        if dist_two_points(X[i], Y[i], X[i + 1], Y[i + 1]) >= RADIUS1 \
                and dist_two_points(X[i], Y[i], X[i + 2], Y[i + 2]) >= RADIUS1 \
                and dist_two_points(X[i + 1], Y[i + 1], X[i + 2], Y[i + 2]) >= RADIUS1:
            return True
    return False


def condition2(X, Y, NUMPOINTS, EPSILON):
    if 0 > EPSILON or EPSILON >= PI:
        return False
    for i in range(0, NUMPOINTS - 2):
        # Create slice
        y_slice = Y[i:i + 3]
        x_slice = X[i:i + 3]

        # Create a, b vectors
        ax = x_slice[0] - x_slice[1]
        ay = y_slice[0] - y_slice[1]
        bx = x_slice[2] - x_slice[1]
        by = y_slice[2] - y_slice[1]
        angle = get_angle(ax, ay, bx, by)
        if angle < (PI - EPSILON) or angle > (PI + EPSILON):
            return True
    return False


def condition3(X, Y, NUMPOINTS, AREA1):
    for i in range(0, NUMPOINTS - 2):
        # Create slice
        y_slice = Y[i:i + 3]
        x_slice = X[i:i + 3]
        # Create a, b vectors
        ax = x_slice[0] - x_slice[1]
        ay = y_slice[0] - y_slice[1]
        bx = x_slice[2] - x_slice[1]
        by = y_slice[2] - y_slice[1]
        area = get_area(ax, ay, bx, by) / 2
        if 0 <= AREA1 < area:
            return True
    return False


def condition4(X, Y, NUMPOINTS, Q_PTS, QUADS):
    if Q_PTS < 2 or Q_PTS > NUMPOINTS or QUADS < 1 or QUADS > 3:
        return False

    for i in range(0, NUMPOINTS - Q_PTS + 1):
        consecutive = list(range(i, i + Q_PTS))
        q_1 = 0
        q_2 = 0
        q_3 = 0
        q_4 = 0
        for idx in consecutive:
            if X[idx] >= 0 and Y[idx] >= 0:
                q_1 = 1
            elif X[idx] <= 0 <= Y[idx]:
                q_2 = 1
            elif X[idx] <= 0 and Y[idx] <= 0:
                q_3 = 1
            elif X[idx] >= 0 >= Y[idx]:
                q_4 = 1
        if sum([q_1, q_2, q_3, q_4]) > QUADS:
            return True
    return False


def condition5(X, NUMPOINTS):
    for i in range(NUMPOINTS - 1):
        if X[i + 1] - X[i] < 0:
            return True
    return False


def condition6(X, Y, NUMPOINTS, N_PTS, DIST):
    for i in range(NUMPOINTS - N_PTS):
        if X[i] == X[i + N_PTS - 1] and Y[i] == Y[i + N_PTS - 1]:
            for j in range(i + 1, i + N_PTS - 1):
                if dist_two_points(X[j], Y[j], X[i], Y[i]) > DIST:
                    return True
        else:
            line = line_two_points(
                X[i], Y[i], X[i + N_PTS - 1], Y[i + N_PTS - 1])
            for j in range(i + 1, i + N_PTS - 1):
                if dist_point_line(line, X[j], Y[j]) > DIST:
                    return True
    return False


def condition7(X, Y, NUMPOINTS, K_PTS, LENGTH1):
    if NUMPOINTS < 3 or K_PTS > NUMPOINTS - 2:
        return False
    for i in range(0, NUMPOINTS - K_PTS - 1):
        j = i + 1 + K_PTS
        if dist_two_points(X[i], Y[i], X[j], Y[j]) > LENGTH1:
            return True
    return False


def condition8(X, Y, NUMPOINTS, A_PTS, B_PTS, RADIUS1):
    if NUMPOINTS < 5 or A_PTS + B_PTS > NUMPOINTS - 3 or A_PTS < 1 or B_PTS < 1:
        return False
    diameter1 = RADIUS1 * 2
    for i in range(0, NUMPOINTS - A_PTS - B_PTS - 2):
        # A = a-b, B = b-c, C = d-a

        j = i + 1 + A_PTS
        k = j + 1 + B_PTS
        a_x = X[i] - X[j]
        b_x = X[k] - X[j]
        c_x = X[i] - X[k]
        a_y = Y[i] - Y[j]
        b_y = Y[k] - Y[j]
        c_y = Y[i] - Y[k]

        a_dist = dist(a_x, a_y)
        b_dist = dist(b_x, b_y)
        c_dist = dist(c_x, c_y)

        a_angle = get_angle(a_x, a_y, b_x, b_y)
        b_angle = get_angle(b_x, b_y, c_x, c_y)
        c_angle = get_angle(c_x, c_y, a_x, a_y)

        if a_angle > PI / 2 or b_angle > PI / 2 or c_angle > PI / 2:
            if max(a_dist, b_dist, c_dist) < diameter1:
                return True
        else:
            s = (a_dist + b_dist + c_dist) / 2
            r = (a_dist * b_dist * c_dist) / \
                (4 * sqrt(s * (s - a_dist) * (s - b_dist) * (s - c_dist)))
            if r > RADIUS1:
                return True
    return False


def condition9(X, Y, NUMPOINTS, C_PTS, D_PTS, EPSILON):
    """
    Angle is in radians
    """
    if NUMPOINTS < 5 or C_PTS + D_PTS > NUMPOINTS - 3 or C_PTS < 1 or D_PTS < 1:
        return False
    for i in range(0, NUMPOINTS - C_PTS - D_PTS - 2):
        j = i + 1 + C_PTS
        k = j + 1 + D_PTS
        # vertex coincides with one of the other points => condition not met
        if [X[j], Y[j]] == [X[i], Y[i]] or [X[j], Y[j]] == [X[k], Y[k]]:
            return False

        opposite_dist = dist_two_points(X[i], Y[i], X[k], Y[k])
        adjacent_dist = dist_two_points(X[j], Y[j], X[k], Y[k])
        angle = atan(opposite_dist / adjacent_dist)
        if angle > (PI + EPSILON) or angle < (PI - EPSILON):
            return True
    return False


def condition10(X, Y, NUMPOINTS, E_PTS, F_PTS, AREA1):
    if NUMPOINTS < 5 or E_PTS + F_PTS > NUMPOINTS - 3 or E_PTS < 1 or F_PTS < 1:
        return False
    for i in range(0, NUMPOINTS - E_PTS - F_PTS - 2):
        j = i + 1 + E_PTS
        k = j + 1 + F_PTS
        a_x = X[i] - X[j]
        b_x = X[k] - X[j]
        a_y = Y[i] - Y[j]
        b_y = Y[k] - Y[j]
        if get_area(a_x, a_y, b_x, b_y) / 2 > AREA1:
            return True
    return False


def condition11(X, NUMPOINTS, G_PTS):
    if NUMPOINTS < 3 or NUMPOINTS - 2 < G_PTS or G_PTS < 1:
        return False
    for i in range(0, NUMPOINTS - G_PTS - 1):
        if X[i + G_PTS + 1] - X[i] < 0:
            return True
    return False


def condition12(X, Y, NUMPOINTS, K_PTS, LENGTH1, LENGTH2):
    if NUMPOINTS < 3 or LENGTH2 < 0:
        return False
    greater_than_l1 = False
    less_than_l2 = False
    for i in range(0, NUMPOINTS - K_PTS - 1):
        j = i + 1 + K_PTS
        a_x = X[j] - X[i]
        a_y = Y[j] - Y[i]
        d = dist(a_x, a_y)
        if d > LENGTH1:
            greater_than_l1 = True
        if d < LENGTH2:
            less_than_l2 = True

    return greater_than_l1 and less_than_l2


def condition13(X, Y, NUMPOINTS, A_PTS, B_PTS, RADIUS1, RADIUS2):
    if NUMPOINTS < 5 or A_PTS + B_PTS > NUMPOINTS - 3 or A_PTS < 1 or B_PTS < 1:
        return False
    diameter1 = RADIUS1 * 2
    diameter2 = RADIUS2 * 2
    greater_than_r1 = False
    less_than_r2 = False
    for i in range(0, NUMPOINTS - A_PTS - B_PTS - 2):
        j = i + 1 + A_PTS
        k = j + 1 + B_PTS

        # Triangle vectors
        a_x = X[i] - X[j]
        b_x = X[k] - X[j]
        c_x = X[i] - X[k]
        a_y = Y[i] - Y[j]
        b_y = Y[k] - Y[j]
        c_y = Y[i] - Y[k]

        # Get length of edges
        a_dist = dist(a_x, a_y)
        b_dist = dist(b_x, b_y)
        c_dist = dist(c_x, c_y)

        # Get angle of points
        # A = a-b, B = b-c, C = d-a
        a_angle = get_angle(a_x, a_y, b_x, b_y)
        b_angle = get_angle(b_x, b_y, c_x, c_y)
        c_angle = get_angle(c_x, c_y, a_x, a_y)

        # If triangle contains non-pointy angle, simply compare
        # the longest side with diameter
        if a_angle > PI / 2 or b_angle > PI / 2 or c_angle > PI / 2:
            if max(a_dist, b_dist, c_dist) > diameter1:
                greater_than_r1 = True
            if max(a_dist, b_dist, c_dist) < diameter2:
                less_than_r2 = True

        # Use length of sides to determine radius of circumference circle
        else:
            s = (a_dist + b_dist + c_dist) / 2
            r = (a_dist * b_dist * c_dist) / \
                (4 * sqrt(s * (s - a_dist) * (s - b_dist) * (s - c_dist)))
            if r > RADIUS1:
                greater_than_r1 = True
            if r > RADIUS2:
                less_than_r2 = True
    return greater_than_r1 and less_than_r2


def condition14(X, Y, NUMPOINTS, E_PTS, F_PTS, AREA1, AREA2):
    if NUMPOINTS < 5 or E_PTS + F_PTS > NUMPOINTS - 3 or AREA2 < 0:
        return False
    greater_than_a1 = False
    less_than_a2 = False
    for i in range(0, NUMPOINTS - E_PTS - F_PTS - 2):
        j = i + 1 + E_PTS
        k = j + 1 + F_PTS
        a_x = X[i] - X[j]
        b_x = X[k] - X[j]
        a_y = Y[i] - Y[j]
        b_y = Y[k] - Y[j]
        if get_area(a_x, a_y, b_x, b_y) / 2 > AREA1:
            greater_than_a1 = True
        if get_area(a_x, a_y, b_x, b_y) / 2 < AREA2:
            less_than_a2 = True
    return greater_than_a1 and less_than_a2
