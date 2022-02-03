from typing import List
import unittest

from globals import NUMPOINTS, PI, X, Y
from custom_types import Parameters

from math import atan
from helpers import get_angle, get_area, line_two_points, dist_point_line, dist_two_points, dist


def create_cmv(params: Parameters) -> List[bool]:
    return [condition0(params), condition1(params), condition2(params), condition3(params), condition4(params), condition5(params), condition6(params), condition7(params), condition8(params), condition9(params), condition10(params), condition11(params), condition12(params), condition13(params), condition14(params)]


def condition0(params: Parameters):
    return 0 <= params.LENGTH1


def condition1(params: Parameters):
    return 0 <= params.RADIUS1


def condition2(params: Parameters):
    if 0 > params.EPSILON or params.EPSILON >= PI:
        return False
    for i in range(0, NUMPOINTS - 2):
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
    for i in range(0, NUMPOINTS - 2):
        # Create slice
        yslice = Y[i:i + 3]
        xslice = X[i:i + 3]
        # Create a, b vectors
        ax = xslice[0] - xslice[1]
        ay = yslice[0] - yslice[1]
        bx = xslice[2] - xslice[1]
        by = yslice[2] - yslice[1]
        area = get_area(ax, ay, bx, by) / 2
        if 0 <= params.AREA1 < area:
            return True
    return False


def condition4(params: Parameters):
    if params.Q_PTS < 2 or params.Q_PTS > NUMPOINTS or params.QUADS < 1 or params.QUADS > 3:
        return False

    for i in range(0, NUMPOINTS - params.Q_PTS + 1):
        consecutives = list(range(i, i + params.Q_PTS))
        q_1 = 0
        q_2 = 0
        q_3 = 0
        q_4 = 0
        for idx in consecutives:
            if X[idx] >= 0 and Y[idx] >= 0:
                q_1 = 1
            elif X[idx] <= 0 and Y[idx] >= 0:
                q_2 = 1
            elif X[idx] <= 0 and Y[idx] <= 0:
                q_3 = 1
            elif X[idx] >= 0 and Y[idx] <= 0:
                q_4 = 1
        if sum([q_1, q_2, q_3, q_4]) > params.QUADS:
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
            line = line_two_points(
                X[i], Y[i], X[i + params.N_PTS - 1], Y[i + params.N_PTS - 1])
            for j in range(i + 1, i + params.N_PTS - 1):
                if dist_point_line(line, X[j], Y[j]) > params.DIST:
                    return True
    return False


def condition7(params: Parameters):
    if NUMPOINTS < 3 or params.K_PTS > NUMPOINTS - 2:
        return False
    for i in range(0, NUMPOINTS - params.K_PTS + 1):
        j = i + 1 + params.K_PTS
        if dist_two_points(X[i], Y[i], X[j], Y[j]) > params.LENGTH1:
            return True
    return False


def condition8(params: Parameters):
    if NUMPOINTS < 5 or params.A_PTS + params.B_PTS > NUMPOINTS - 3 or params.A_PTS < 1 or params.B_PTS < 1:
        return False
    DIAMETER1 = params.RADIUS1 * 2
    for i in range(0, NUMPOINTS - params.A_PTS - params.B_PTS + 2):
        # A = a-b, B = b-c, C = d-a

        j = i + 1 + params.A_PTS
        k = j + 1 + params.B_PTS
        a_x = X[i] - X[j]
        b_x = X[k] - X[j]
        c_x = X[i] - X[k]
        a_y = Y[i] - Y[j]
        b_y = Y[k] - Y[j]
        c_y = Y[i] - Y[k]

        a_dist = dist(a_x, a_y)
        b_dist = dist(b_x, b_y)
        c_dist = dist(c_x, c_y)

        A_angle = get_angle(a_x, a_y, b_x, b_y)
        B_angle = get_angle(b_x, b_y, c_x, c_y)
        C_angle = get_angle(c_x, c_y, a_x, a_y)

        if A_angle > PI/2 or B_angle > PI/2 or C_angle > PI/2:
            if max(a_dist, b_dist, c_dist) < DIAMETER1:
                return True
        else:
            s = (a_dist+b_dist+c_dist) / 2
            r = (a_dist*b_dist*c_dist) / \
                (4 * sqrt(s * (s - a_dist) * (s - b_dist) * (s - c_dist)))
            if (r > params.RADIUS1):
                return True
    return False


def condition9(params: Parameters):
    """
    Angle is in radians
    """
    if NUMPOINTS < 5 or params.C_PTS + params.D_PTS > NUMPOINTS - 3 or params.C_PTS < 1 or params.D_PTS < 1:
        return False
    for i in range(0, NUMPOINTS - params.C_PTS - params.D_PTS + 2):
        j = i + 1 + params.C_PTS
        k = j + 1 + params.D_PTS
        # vertex coincides with one of the other points => condition not met
        if [X[j], Y[j]] == [X[i], Y[i]] or [X[j], Y[j]] == [X[k], Y[k]]:
            return False

        opposite_dist = dist_two_points(X[i], Y[i], X[k], Y[k])
        adjacent_dist = dist_two_points(X[j], Y[j], X[k], Y[k])
        angle = atan(opposite_dist / adjacent_dist)
        if angle > (PI + params.EPSILON) or angle < (PI - params.EPSILON):
            return True
    return False


def condition10(params: Parameters):
    if NUMPOINTS < 5 or params.E_PTS + params.F_PTS > NUMPOINTS - 3 or params.E_PTS < 1 or params.F_PTS < 1:
        return False
    for i in range(0, NUMPOINTS - params.E_PTS - params.F_PTS + 2):
        j = i + 1 + params.E_PTS
        k = j + 1 + params.F_PTS
        a_x = X[i] - X[j]
        b_x = X[k] - X[j]
        a_y = Y[i] - Y[j]
        b_y = Y[k] - Y[j]
        if get_area(a_x, a_y, b_x, b_y) / 2 > params.AREA1:
            return True
    return False


def condition11(params: Parameters):
    if NUMPOINTS < 3 or NUMPOINTS - 2 < params.G_PTS or params.G_PTS < 1:
        return False
    for i in range(0, NUMPOINTS - params.G_PTS + 1):
        if X[i + params.G_PTS + 1] - X[i] < 0:
            return True
    return False


def condition12(params: Parameters):
    if NUMPOINTS < 3 or params.LENGTH2 < 0:
        return False
    greater_than_l1 = False
    less_than_l2 = False
    for i in range(0, NUMPOINTS - params.K_PTS + 1):
        j = i + 1 + params.K_PTS
        d = dist_two_points(X[i], Y[i], X[j], Y[j])
        if d > params.LENGTH1:
            greater_than_l1 = True
        if d < params.LENGTH2:
            less_than_l2 = True

    return greater_than_l1 and less_than_l2


def condition13(params: Parameters):
    if NUMPOINTS < 5 or params.A_PTS + params.B_PTS > NUMPOINTS - 3 or params.A_PTS < 1 or params.B_PTS < 1:
        return False
    DIAMETER1 = params.RADIUS1 * 2
    DIAMETER2 = params.RADIUS2 * 2
    greater_than_r1 = False
    less_than_r2 = False
    for i in range(0, NUMPOINTS - params.A_PTS - params.B_PTS + 2):
        j = i + 1 + params.A_PTS
        k = j + 1 + params.B_PTS

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
        A_angle = get_angle(a_x, a_y, b_x, b_y)
        B_angle = get_angle(b_x, b_y, c_x, c_y)
        C_angle = get_angle(c_x, c_y, a_x, a_y)

        # If triangle contains non-pointy angle, simply compare longest side with diameter
        if A_angle > PI/2 or B_angle > PI/2 or C_angle > PI/2:
            if max(a_dist, b_dist, c_dist) > DIAMETER1:
                greater_than_r1 = True
            if max(a_dist, b_dist, c_dist) < DIAMETER2:
                less_than_r2 = True

        # Use length of sides to determine radius of circumference circle
        else:
            s = (a_dist+b_dist+c_dist) / 2
            r = (a_dist*b_dist*c_dist) / \
                (4 * sqrt(s * (s - a_dist) * (s - b_dist) * (s - c_dist)))
            if (r > params.RADIUS1):
                greater_than_r1 = True
            if (r > params.RADIUS2):
                less_than_r2 = True
    return greater_than_r1 and less_than_r2


def condition14(params: Parameters):
    if NUMPOINTS < 5 or params.AREA2 < 0:
        return False
    greater_than_a1 = False
    less_than_a2 = False
    for i in range(0, NUMPOINTS - params.E_PTS - params.F_PTS + 2):
        j = i + 1 + params.E_PTS
        k = j + 1 + params.F_PTS
        a_x = X[i] - X[j]
        b_x = X[k] - X[j]
        a_y = Y[i] - Y[j]
        b_y = Y[k] - Y[j]
        if get_area(a_x, a_y, b_x, b_y) / 2 > params.AREA1:
            greater_than_a1 = True
        if get_area(a_x, a_y, b_x, b_y) / 2 < params.AREA2:
            less_than_a2 = True
    return greater_than_a1 and less_than_a2


class LICTest(unittest.TestCase):

    def test_0_positive(self):
        """
        Positive case to test if condition0 succeeds for the true case
        """
        params = Parameters(None, 1, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        self.assertTrue(condition0(params))

    def test_0_edge(self):
        """
        Edge case to test if condition0 succeeds for the edge case LENGTH1=0
        """
        params = Parameters(None, 0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        self.assertTrue(condition0(params))

    def test_0_negative(self):
        """
        Negative case to test if condition0 succeeds for the false case
        """
        params = Parameters(None, -1, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        self.assertFalse(condition0(params))

    def test_1_positive(self):
        """
        Positive case to test if condition1 succeeds for the true case
        """
        params = Parameters(None, 0, 1, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        self.assertTrue(condition1(params))

    def test_1_edge(self):
        """
        Edge case to test if condition1 succeeds for the edge case RADIUS1=0
        """
        params = Parameters(None, 0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        self.assertTrue(condition1(params))

    def test_1_negative(self):
        """
        Negative case to test if condition1 succeeds for the false case
        """
        params = Parameters(None, 0, -1, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        self.assertFalse(condition1(params))

    def test_2_positive(self):
        """
        Positive case to test if condition2 succeeds if the angle is within the specified bounds
        """
        global X
        global Y
        global NUMPOINTS

        NUMPOINTS = 3
        X = [0.2, 0.1, 0.3]
        Y = [0.5, 0.55, 0.6]
        params = Parameters(None, 0, 0, 0.25, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        self.assertTrue(condition2(params))

    def test_2_negative(self):
        """
        Negative case to test if condition2 returns false if the angle is outside of the specified bounds
        """
        global X
        global Y
        global NUMPOINTS

        NUMPOINTS = 4
        X = [0, 1, 2, 3]
        Y = [0, 0, 2, 2]
        params = Parameters(None, 0, 0, PI / 2, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        self.assertFalse(condition2(params))

    def test_3_positive(self):
        global X
        global Y
        global NUMPOINTS

        NUMPOINTS = 6
        X = [0, 1, 2, 3, 4, 20]
        Y = [0, 0, 2, 2, 5, 110]
        params = Parameters(None, 0, 0, 0, 150, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        self.assertTrue(condition3(params))

    def test_3_negative(self):
        global X
        global Y
        global NUMPOINTS

        NUMPOINTS = 6
        X = [100, 1, 2, 3, 4, 20]
        Y = [100, 5, 8, 2, 5, 110]
        params = Parameters(None, 0, 0, 0, 250, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        self.assertFalse(condition3(params))

    def test_4_positive(self):
        global X
        global Y
        global NUMPOINTS

        NUMPOINTS = 5
        X = [100, 10, -1, -3, 4]
        Y = [0, -10, 10, -29, 5]
        params = Parameters(None, 0, 0, 0, 0, 4, 3, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        self.assertTrue(condition4(params))

    def test_4_negative(self):
        global X
        global Y
        global NUMPOINTS

        NUMPOINTS = 5
        X = [100, 1, 2, 3, 4]
        Y = [100, 5, 8, 2, 5]
        params = Parameters(None, 0, 0, 0, 0, 3, 1, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        self.assertFalse(condition4(params))

    def test_5(self):
        assert condition5(self.params) == True

    def test_6(self):
        assert condition6(self.params) == True

    def test_7(self):
        assert condition7(self.params) == True

    def test_8(self):
        assert condition8(self.params) == True

    def test_9(self):
        assert condition9(self.params) == True

    def test_10(self):
        assert condition10(self.params) == True

    def test_11(self):
        assert condition11(self.params) == True

    def test_12(self):
        assert condition12(self.params) == True

    def test_13(self):
        assert condition13(self.params) == True

    def test_14(self):
        assert condition14(self.params) == True
