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
        NUMPOINTS = 4
        X = [0, 1, 2, 3]
        Y = [0, 0, 2, 2]
        params = Parameters(0, 0, PI / 2, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        self.assertFalse(condition2(X, Y, NUMPOINTS, params))

    def test_3_positive(self):
        NUMPOINTS = 6
        X = [0, 1, 2, 3, 4, 20]
        Y = [0, 0, 2, 2, 5, 110]
        params = Parameters(0, 0, 0, 150, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        self.assertTrue(condition2(X, Y, NUMPOINTS, params))

    def test_3_negative(self):
        NUMPOINTS = 6
        X = [100, 1, 2, 3, 4, 20]
        Y = [100, 5, 8, 2, 5, 110]
        params = Parameters(0, 0, 0, 250, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        self.assertFalse(condition3(X,Y, NUMPOINTS, params))

    def test_4_positive(self):
        NUMPOINTS = 5
        X = [100, 10, -1, -3, 4]
        Y = [0, -10, 10, -29, 5]
        params = Parameters(0, 0, 0, 0, 0, 0, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        self.assertFalse(condition4(X,Y, NUMPOINTS, params))

    def test_4_negative(self):

        NUMPOINTS = 5
        X = [100, 1, 2, 3, 4]
        Y = [100, 5, 8, 2, 5]
        params = Parameters(0, 0, 0, 0, 3, 1, 0, 0, 0,
                            0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        self.assertFalse(condition4(X,Y, NUMPOINTS, params))

    def test_5_positive(self):
        NUMPOINTS = 5
        X = [10, 9, 8, 7, 8]
        self.assertTrue(condition5(X, NUMPOINTS))

    def test_5_negative(self):
        NUMPOINTS = 5
        X = [5, 6, 7, 8, 9]
        self.assertFalse(condition5(X, NUMPOINTS))

    def test_6_positive(self):
        params = Parameters(0, 0, 0, 0, 0, 0, 3, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        numpoints = 5
        x = [0, 10, 1, 2, 1]
        y = [0, 10, 1, 2, 1]
        self.assertTrue(condition6(x, y, numpoints, params))

    def test_6_negative(self):
        params = Parameters(0, 0, 0, 0, 0, 0, 10, 4, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        x = [1, 1, 2, 10, 10]
        y = [3, 1, 2, 10, 10]
        self.assertFalse(condition6(x, y, 5, params))

    def test_7_positive(self):
        params = Parameters(1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        x = [0, 1, 2, 3]
        y = [0, 1, 2, 3]
        self.assertTrue(condition7(x, y, 4, params))

    def test_7_negative(self):
        params = Parameters(10, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        x = [0, 1, 2, 3]
        y = [0, 1, 2, 3]
        self.assertFalse(condition7(x, y, 4, params))

    def test_7_not_full_fill(self):
        params = Parameters(10, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
        x = [0, 1]
        y = [0, 1]
        self.assertFalse(condition7(x, y, 2, params))

    def test_8_positive(self):
        params = Parameters(0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0)
        x = [1, 1, 2, 10, 10]
        y = [3, 1, 2, 10, 10]
        self.assertTrue(condition8(x, y, 5, params))

    def test_8_negative(self):
        params = Parameters(0, 100, 0, 0, 0, 0, 0, 0, 0, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0)
        x = [1, 1, 2, 10, 10]
        y = [3, 1, 2, 10, 10]
        self.assertFalse(condition8(x, y, 5, params))

    def test_8_not_full_fill(self):
        params = Parameters(0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 2, 0, 0, 0, 0, 0, 0, 0, 0)
        x = [1, 1, 2, 10]
        y = [3, 1, 2, 10]
        self.assertFalse(condition8(x, y, 4, params))

    def test_9_positive(self):
        params = Parameters(0, 0, 0.2, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 0, 0, 0, 0, 0, 0)
        x = [0, 1, 2, 10, 10, 100]
        y = [0, 1, 2, 10, 10, 100]
        self.assertTrue(condition9(x, y, 6, params))

    def test_9_negative(self):
        params = Parameters(0, 0, 5, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 0, 0, 0, 0, 0, 0)
        x = [1, 1, 2, 10, 10, 2]
        y = [3, 1, 2, 10, 10, 2]
        self.assertFalse(condition9(x, y, 6, params))

    def test_9_not_full_fill(self):
        params = Parameters(0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 2, 0, 0, 0, 0, 0, 0)
        x = [1, 1, 2, 10, 10]
        y = [3, 1, 2, 10, 10]
        self.assertFalse(condition9(x, y, 5, params))

    def test_10(self):
        assert condition10(self.params) == True

    def test_11(self):
        assert condition11(self.params) == True

    def test_12(self):
        assert condition12(self.params) == True

    def test_13(self):
        X = [0, 0, 1]
        Y = [0, 1, 1]
        NUMPOINTS = 3
        params = Parameters(0, )
        self.assertTrue(condition13(X, Y, NUMPOINTS, params.A_PTS, params.B_PTS, params.RADIUS1, params.RADIUS2))

    def test_14_positive(self):
        NUMPOINTS = 8
        X = [10, 9, 8, 7, 8, 5, 6, 7]
        Y = [10, 9, 8, 7, 8, 5, 6, 7]
        E_PTS = 2
        F_PTS = 2
        AREA1 = 10
        AREA2 = 15

        self.assertTrue(condition14(X, Y, NUMPOINTS,
                                    E_PTS, F_PTS, AREA1, AREA2))
