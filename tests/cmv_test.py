import unittest
from custom_types import Parameters, Y
from cmv import *


class CMVTest(unittest.TestCase):

    def test_0_positive(self):
        """
        Positive case to test if condition0 succeeds for the true case
        """
        LENGTH1 = 1
        self.assertTrue(condition0(LENGTH1))

    def test_0_edge(self):
        """
        Edge case to test if condition0 succeeds for the edge case LENGTH1=0
        """
        LENGTH1 = 0
        self.assertTrue(condition0(LENGTH1))

    def test_0_negative(self):
        """
        Negative case to test if condition0 succeeds for the false case
        """
        LENGTH1 = -1
        self.assertFalse(condition0(LENGTH1))

    def test_1_positive(self):
        """
        Positive case to test if condition1 succeeds for the true case
        """
        RADIUS1 = 1
        self.assertTrue(condition1(RADIUS1))

    def test_1_edge(self):
        """
        Edge case to test if condition1 succeeds for the edge case RADIUS1=0
        """
        RADIUS1 = 0
        self.assertTrue(condition1(RADIUS1))

    def test_1_negative(self):
        """
        Negative case to test if condition1 succeeds for the false case
        """
        RADIUS1 = -1
        self.assertFalse(condition1(RADIUS1))

    def test_2_positive(self):
        """
        Positive case to test if condition2 succeeds if the angle is within the specified bounds
        """

        X = [0.2, 0.1, 0.3]
        Y = [0.5, 0.55, 0.6]
        NUMPOINTS = 3
        EPSILON = 0.25
        self.assertTrue(condition2(X, Y, NUMPOINTS, EPSILON))

    def test_2_negative(self):
        """
        Negative case to test if condition2 returns false if the angle is outside of the specified bounds
        """
        X = [0, 1, 2, 3]
        Y = [0, 0, 2, 2]
        NUMPOINTS = 4
        EPSILON = PI / 2
        self.assertFalse(condition2(X, Y, NUMPOINTS, EPSILON))

    def test_3_positive(self):
        """
        Positive case to test if There exists at least one set of three consecutive data points that are the vertices
        of a triangle with area greater than AREA1.
        """
        X = [0, 1, 2, 3, 4, 20]
        Y = [0, 0, 2, 2, 5, 110]
        NUMPOINTS = 6
        AREA1 = 150
        self.assertTrue(condition3(X, Y, NUMPOINTS, AREA1))

    def test_3_negative(self):
        """
        Negative case to test if There does NOT exist at least one set of three consecutive data points that are the
        vertices of a triangle with area greater than AREA1.
        """
        X = [100, 1, 2, 3, 4, 20]
        Y = [100, 5, 8, 2, 5, 110]
        NUMPOINTS = 6
        AREA1 = 250
        self.assertFalse(condition3(X, Y, NUMPOINTS, AREA1))

    def test_4_positive(self):
        """
        Positive case to test if There exists at least one set of Q PTS consecutive data points that lie in more than
        QUADS quadrants.
        """
        X = [100, 10, -1, -3, 4]
        Y = [0, -10, 10, -29, 5]
        NUMPOINTS = 5
        Q_PTS = 0
        QUADS = 0
        self.assertFalse(condition4(X, Y, NUMPOINTS, Q_PTS, QUADS))

    def test_4_negative(self):
        """
        Negative case to test if There does NOT exist at least one set of Q PTS consecutive data points that lie in
        more than QUADS quadrants.
        """
        X = [100, 1, 2, 3, 4]
        Y = [100, 5, 8, 2, 5]
        NUMPOINTS = 5
        Q_PTS = 3
        QUADS = 1
        self.assertFalse(condition4(X, Y, NUMPOINTS, Q_PTS, QUADS))

    def test_5_positive(self):
        """
        Positive case to test if There exists at least one set of two consecutive data points, (X[i],Y[i]) and (X[j],
        Y[j]), such that X[j] - X[i] < 0. (where i = j-1)
        """
        X = [10, 9, 8, 7, 8]
        NUMPOINTS = 5
        self.assertTrue(condition5(X, NUMPOINTS))

    def test_5_negative(self):
        """
        Negative case to test if There does NOT exist at least one set of two consecutive data points, (X[i],
        Y[i]) and (X[j], Y[j]), such that X[j] - X[i] < 0. (where i = j-1)
        """
        X = [5, 6, 7, 8, 9]
        NUMPOINTS = 5
        self.assertFalse(condition5(X, NUMPOINTS))

    def test_6_positive(self):
        """
        Positive case to test if There exists at least one set of N PTS consecutive data points such that at least
        one of the points lies a distance greater than DIST from the line joining the first and last of these N PTS
        points.
        """
        X = [0, 10, 1, 2, 1]
        Y = [0, 10, 1, 2, 1]
        NUMPOINTS = 5
        N_PTS = 3
        DIST = 3
        self.assertTrue(condition6(X, Y, NUMPOINTS, N_PTS, DIST))

    def test_6_negative(self):
        """
        Negative case to test if There does NOT exist at least one set of N PTS consecutive data points such that at
        least one of the points lies a distance greater than DIST from the line joining the first and last of these N
        PTS points.
        """
        X = [1, 1, 2, 10, 10]
        Y = [3, 1, 2, 10, 10]
        NUMPOINTS = 5
        N_PTS = 10
        DIST = 4
        self.assertFalse(condition6(X, Y, NUMPOINTS, N_PTS, DIST))

    def test_7_positive(self):
        """
        Positive case to test if There exists at least one set of two data points separated by exactly K PTS
        consecutive intervening points that are a distance greater than the length, LENGTH1, apart.
        """
        X = [0, 1, 2, 3]
        Y = [0, 1, 2, 3]
        NUMPOINTS = 4
        K_PTS = 1
        LENGTH1 = 1
        self.assertTrue(condition7(X, Y, NUMPOINTS, K_PTS, LENGTH1))

    def test_7_negative(self):
        """
        Negative case to test if There does NOT exist at least one set of two data points separated by exactly K PTS
        consecutive intervening points that are a distance greater than the length, LENGTH1, apart.
        """
        X = [0, 1, 2, 3]
        Y = [0, 1, 2, 3]
        NUMPOINTS = 4
        K_PTS = 1
        LENGTH1 = 10
        self.assertFalse(condition7(X, Y, NUMPOINTS, K_PTS, LENGTH1))

    def test_7_not_fullfill(self):
        """
        Negative case to test The condition is not met when NUMPOINTS < 3.
        """
        X = [0, 1]
        Y = [0, 1]
        NUMPOINTS = 2
        K_PTS = 1
        LENGTH1 = 10
        self.assertFalse(condition7(X, Y, NUMPOINTS, K_PTS, LENGTH1))

    def test_8_positive(self):
        """
        Positive case to test if There exists at least one set of three data points separated by exactly A PTS and B
        PTS consecutive intervening points, respectively, that cannot be contained within or on a circle of radius
        RADIUS1.
        """
        X = [1, 1, 2, 10, 10]
        Y = [3, 1, 2, 10, 10]
        NUMPOINTS = 5
        A_PTS = 1
        B_PTS = 1
        RADIUS1 = 1
        self.assertTrue(condition8(X, Y, NUMPOINTS, A_PTS, B_PTS, RADIUS1))

    def test_8_negative(self):
        """
        Negative case to test if There does NOT exist at least one set of three data points separated by exactly A
        PTS and B PTS consecutive intervening points, respectively, that cannot be contained within or on a circle of
        radius RADIUS1.
        """
        X = [1, 1, 2, 10, 10]
        Y = [3, 1, 2, 10, 10]
        NUMPOINTS = 5
        A_PTS = 1
        B_PTS = 1
        RADIUS1 = 100
        self.assertFalse(condition8(X, Y, NUMPOINTS, A_PTS, B_PTS, RADIUS1))

    def test_8_not_fullfill(self):
        """
        Negative case to test The condition is not met when NUMPOINTS < 5.
        """
        X = [1, 1, 2, 10]
        Y = [3, 1, 2, 10]
        NUMPOINTS = 4
        A_PTS = 1
        B_PTS = 1
        RADIUS1 = 1
        self.assertFalse(condition8(X, Y, NUMPOINTS, A_PTS, B_PTS, RADIUS1))

    def test_9_positive(self):
        """
        Positive case to test if There exists at least one set of three data points separated by exactly C PTS and D
        PTS consecutive intervening points, respectively, that form an angle such that: angle < (PI􀀀EPSILON) or
        angle > (PI+EPSILON) The second point of the set of three points is always the vertex of the angle. If either
        the first point or the last point (or both) coincide with the vertex, the angle is undefined and the LIC is
        not satisfied by those three points.
        """
        X = [0, 1, 2, 10, 10, 100]
        Y = [0, 1, 2, 10, 10, 100]
        NUMPOINTS = 6
        C_PTS = 1
        D_PTS = 2
        EPSILON = 0.2
        self.assertTrue(condition9(X, Y, NUMPOINTS, C_PTS, D_PTS, EPSILON))

    def test_9_negative(self):
        """
        Negative case to test if There does NOT exist at least one set of three data points separated by exactly C
        PTS and D PTS consecutive intervening points, respectively, that form an angle such that: angle < (
        PI􀀀EPSILON) or angle > (PI+EPSILON) The second point of the set of three points is always the vertex of the
        angle. If either the first point or the last point (or both) coincide with the vertex, the angle is undefined
        and the LIC is not satisfied by those three points.
        """
        X = [1, 1, 2, 10, 10, 2]
        Y = [3, 1, 2, 10, 10, 2]
        NUMPOINTS = 6
        C_PTS = 1
        D_PTS = 2
        EPSILON = 5
        self.assertFalse(condition9(X, Y, NUMPOINTS, C_PTS, D_PTS, EPSILON))

    def test_9_not_fullfill(self):
        """
        Negative case to test When NUMPOINTS < 5, the condition is not met.
        """
        params = Parameters(0, 0, 1, 0, 0, 0, 0, 0, 0,
                            0, 0, 1, 2, 0, 0, 0, 0, 0, 0)
        X = [1, 1, 2, 10]
        Y = [3, 1, 2, 10]
        NUMPOINTS = 4
        C_PTS = 1
        D_PTS = 1
        EPSILON = 1
        self.assertFalse(condition9(X, Y, NUMPOINTS, C_PTS, D_PTS, EPSILON))

    def test_10_positive(self):
        """
        Positive test case to test that there exist 3 data points, separated by exactly
        E_PTS, F_PTS consecutive points where the area of the triangle formed by the points
        is larger than AREA1.
        """
        # Area should be 2
        X = [0, 1, 0, 1, 2]
        Y = [0, 0, 2, 2, 2]
        NUMPOINTS = 5
        E_PTS = 1
        F_PTS = 1
        AREA1 = 1
        self.assertTrue(condition10(X, Y, NUMPOINTS, E_PTS, F_PTS, AREA1))

    def test_10_negative(self):
        """
        Negative test case to test that condition10 fails if there does not exist 3 data points, separated
        by exactly E_PTS, F_PTS consecutive points where the area of the triangle formed by the points
        is larger than AREA1.
        """
        # Area should be 2
        X = [0, 1, 0, 1, 2]
        Y = [0, 0, 2, 2, 2]
        NUMPOINTS = 5
        E_PTS = 1
        F_PTS = 1
        AREA1 = 2.5
        self.assertFalse(condition10(X, Y, NUMPOINTS, E_PTS, F_PTS, AREA1))

    def test_11_positive(self):
        """
        Positive test case to test that there exists 2 data points, separated
        by G_PTS consecutive points where the first point is larger than the second point.
        """
        X = [3, 2, 0, 0, 2]
        NUMPOINTS = 5
        G_PTS = 2
        self.assertTrue(condition11(X, NUMPOINTS, G_PTS))

    def test_11_negative(self):
        """
        Negative test case to test that there does not exist 2 data points, separated
        by G_PTS consecutive points where the first point is larger than the second point.
        """
        X = [0, 2, 0, 3, 2]
        NUMPOINTS = 5
        G_PTS = 2
        self.assertFalse(condition11(X, NUMPOINTS, G_PTS))

    def test_12_positive(self):
        """
        Positive test case to test that
        -   There exists 2 points, separated by exactly K_PTS consecutive points that
            create a line which is **longer** than LENGTH1.
        -   There exists 2 points, separated by exactly K_PTS consecutive points that
            create a line which is **shorter** than LENGTH2.
        """
        X = [1, 2, 3, 10, 4]
        NUMPOINTS = 5
        K_PTS = 1
        LENGTH1 = 5
        LENGTH2 = 10
        self.assertTrue(condition12(X, Y, NUMPOINTS, K_PTS, LENGTH1, LENGTH2))

    def test_12_negative(self):
        """
        Negative test case to test that
        -   There is no 2 points, separated by exactly K_PTS consecutive points that
            create a line which is longer than LENGTH1.
        """
        X = [1, 2, 3, 4, 5]
        NUMPOINTS = 5
        K_PTS = 1
        LENGTH1 = 5
        LENGTH2 = 10
        self.assertFalse(condition12(X, Y, NUMPOINTS, K_PTS, LENGTH1, LENGTH2))

    def test_13_positive(self):
        """
        Positive test case to test that
        -   There exist 3 points, separated by exactly A_PTS, B_PTS consecutive points
            that create a triangle which do not fit in a circle of radius RADIUS1.
        -   There exist 3 points, separated by exactly A_PTS, B_PTS consecutive points
            that create a triangle which fit in a circle of radius RADIUS2.
        """
        X = [0, 2, 0, 3, 2]
        Y = [0, 1, 2, 1, 2]
        NUMPOINTS = 5
        A_PTS = 1
        B_PTS = 1
        RADIUS1 = 1
        RADIUS2 = 3
        self.assertTrue(condition13(X, Y, NUMPOINTS,
                        A_PTS, B_PTS, RADIUS1, RADIUS2))

    def test_13_negative(self):
        """
        Negative test case to test that
        -   There does not exist 3 points, separated by exactly A_PTS, B_PTS consecutive points
            that create a triangle that cannot fit in a circle of radius RADIUS1
        """
        X = [0, 2, 0, 3, 2]
        Y = [0, 1, 2, 1, 5]
        NUMPOINTS = 5
        A_PTS = 1
        B_PTS = 1
        RADIUS1 = 3
        RADIUS2 = 1
        self.assertFalse(condition13(X, Y, NUMPOINTS,
                                     A_PTS, B_PTS, RADIUS1, RADIUS2))

    def test_14_positive(self):
        """
        Positive test case to test that
        -   There exists 3 points, separated by exactly E_PTS, F_PTS consecutive points
            that create a triangle with an area larger than AREA1.
        -   There exists 3 points, separated by exactly E_PTS, F_PTS consecutive points
            that create a triangle with an area smaller than AREA2.
        """
        # Area should be 2
        X = [0, 1, 0, 1, 2]
        Y = [0, 0, 2, 2, 2]
        NUMPOINTS = 5
        E_PTS = 1
        F_PTS = 1
        AREA1 = 1
        AREA2 = 4

        self.assertTrue(condition14(X, Y, NUMPOINTS,
                                    E_PTS, F_PTS, AREA1, AREA2))

    def test_14_negative(self):
        """
        Negative test case to test that
        -   There does not exists 3 points, separated by exactly E_PTS, F_PTS consecutive points
            that create a triangle with an area larger than AREA1.
        """
        # Area should be 2, but AREA1 and AREA2 are flipped
        X = [0, 1, 0, 1, 2]
        Y = [0, 0, 2, 2, 2]
        NUMPOINTS = 5
        E_PTS = 1
        F_PTS = 1
        AREA1 = 4
        AREA2 = 1

        self.assertFalse(condition14(X, Y, NUMPOINTS,
                                     E_PTS, F_PTS, AREA1, AREA2))
