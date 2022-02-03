import unittest
from custom_types import Parameters
from cmv import *


class CMVTest(unittest.TestCase):

    def test_0_positive(self):
        """
        Positive case to test if condition0 succeeds for the true case
        """
        params = Parameters(0, 0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        self.assertTrue(condition0(params))

    def test_0_edge(self):
        """
        Edge case to test if condition0 succeeds for the edge case LENGTH1=0
        """
        params = Parameters(0, 0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        self.assertTrue(condition0(params))

    def test_0_negative(self):
        """
        Negative case to test if condition0 succeeds for the false case
        """
        params = Parameters(-1, 0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        self.assertFalse(condition0(params))

    def test_1_positive(self):
        """
        Positive case to test if condition1 succeeds for the true case
        """
        params = Parameters(0, 1, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        self.assertTrue(condition1(params))

    def test_1_edge(self):
        """
        Edge case to test if condition1 succeeds for the edge case RADIUS1=0
        """
        params = Parameters(0, 0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        self.assertTrue(condition1(params))

    def test_1_negative(self):
        """
        Negative case to test if condition1 succeeds for the false case
        """
        params = Parameters(0, -1, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

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
        params = Parameters(0, 0, 0.25, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

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
        params = Parameters(0, 0, PI / 2, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        self.assertFalse(condition2(params))

    def test_3_positive(self):
        global X
        global Y
        global NUMPOINTS

        NUMPOINTS = 6
        X = [0, 1, 2, 3, 4, 20]
        Y = [0, 0, 2, 2, 5, 110]
        params = Parameters(0, 0, 0, 150, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        self.assertTrue(condition3(params))

    def test_3_negative(self):
        global X
        global Y
        global NUMPOINTS

        NUMPOINTS = 6
        X = [100, 1, 2, 3, 4, 20]
        Y = [100, 5, 8, 2, 5, 110]
        params = Parameters(0, 0, 0, 250, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        self.assertFalse(condition3(params))

    def test_4_positive(self):
        global X
        global Y
        global NUMPOINTS

        NUMPOINTS = 5
        X = [100, 10, -1, -3, 4]
        Y = [0, -10, 10, -29, 5]
        params = Parameters(0, 0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        self.assertTrue(condition4(params))

    def test_4_negative(self):
        global X
        global Y
        global NUMPOINTS

        NUMPOINTS = 5
        X = [100, 1, 2, 3, 4]
        Y = [100, 5, 8, 2, 5]
        params = Parameters(0, 0, 0, 0, 3, 1, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        self.assertFalse(condition4(params))

    def test_5_positive(self):
        NUMPOINTS = 5
        X = [10, 9, 8, 7, 8]
        self.assertTrue(condition5(X, NUMPOINTS))

    def test_5_negative(self):
        NUMPOINTS = 5
        X = [5, 6, 7, 8, 9]
        self.assertFalse(condition5(X, NUMPOINTS))

    def test_6(self):
        assert condition6(self.params) == True

    def test_7(self):
        assert condition7(self.params) == True

    def test_8(self):
        assert condition8(self.params) == True

    def test_9(self):
        assert condition9(self.params) == True

    def test_10_positive(self):
        """
        Positive test case to test that there exist 3 data points, separated by exactly 
        E_PTS, F_PTS consecutive points where the area of the triangle formed by the points 
        is larger than AREA1.
        """
        # Area should be 2
        NUMPOINTS = 5
        X = [0, 1, 0, 1, 2]
        Y = [0, 0, 2, 2, 2]
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
        NUMPOINTS = 5
        X = [0, 1, 0, 1, 2]
        Y = [0, 0, 2, 2, 2]
        E_PTS = 1
        F_PTS = 1
        AREA1 = 2.5

        self.assertFalse(condition10(X, Y, NUMPOINTS, E_PTS, F_PTS, AREA1))

    def test_11_positive(self):
        """
        Positive test case to test that there exists 2 data points, separated
        by G_PTS consecutive points where the first point is larger than the second point. 
        """
        NUMPOINTS = 5
        X = [3, 2, 0, 0, 2]
        G_PTS = 2
        self.assertTrue(condition11(X, NUMPOINTS, G_PTS))

    def test_11_negative(self):
        """
        Negative test case to test that there does not exist 2 data points, separated
        by G_PTS consecutive points where the first point is larger than the second point. 
        """
        NUMPOINTS = 5
        X = [0, 2, 0, 3, 2]
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
        NUMPOINTS = 5
        X = [0, 1, 0, 1, 2]
        Y = [0, 0, 2, 2, 2]
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
        NUMPOINTS = 5
        X = [0, 1, 0, 1, 2]
        Y = [0, 0, 2, 2, 2]
        E_PTS = 1
        F_PTS = 1
        AREA1 = 4
        AREA2 = 1

        self.assertFalse(condition14(X, Y, NUMPOINTS,
                                     E_PTS, F_PTS, AREA1, AREA2))
