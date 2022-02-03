import sys
from threading import Thread
from custom_types import MATRIX_DIMENSION
from cmv import *
import unittest

from fuv import create_fuv
from pum import createPUM


class AllTest(unittest.TestCase):

    def __init__(self, methodName: str = ...):
        super().__init__(methodName)
        self.params = Parameters(
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

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
        global NUMPOINTS
        global X

        NUMPOINTS = 5
        X = [10, 9, 8, 7, 8]
        self.assertTrue(condition5())

    def test_5_negative(self):
        global NUMPOINTS
        global X

        NUMPOINTS = 5
        X = [10, 9, 8, 7, 6]
        self.assertTrue(condition5())

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

    def test_pum_positive_intricate(self):
        """
        Test an intricate example case which should evaluate to the expected PUM.
        """
        cmv = [False, True, True, True, False, False, False,
               False, False, False, False, False, False, False, False]

        lcm = [
            ["ANDD", "ANDD", "ORR", "ANDD", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED",
             "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED"],

            ["ANDD", "ANDD", "ORR", "ORR", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED",
             "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED"],

            ["ORR", "ORR", "ANDD", "ANDD", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED",
             "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED"],

            ["ANDD", "ORR", "ANDD", "ANDD", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED",
             "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED"]
        ] + [["NOTUSED" for i in range(MATRIX_DIMENSION)]
             for j in range(11)]

        expected_pum = [
            [False, False, True, False, True, True, True, True,
             True, True, True, True, True, True, True],
            [False, True, True, True, True, True, True, True,
             True, True, True, True, True, True, True],
            [True, True, True, True, True, True, True, True,
             True, True, True, True, True, True, True],
            [False, True, True, True, True, True, True, True,
             True, True, True, True, True, True, True],
            [True, True, True, True, True, True, True, True,
             True, True, True, True, True, True, True],
            [True, True, True, True, True, True, True, True,
             True, True, True, True, True, True, True],
            [True, True, True, True, True, True, True, True,
             True, True, True, True, True, True, True],
            [True, True, True, True, True, True, True, True,
             True, True, True, True, True, True, True],
            [True, True, True, True, True, True, True, True,
             True, True, True, True, True, True, True],
            [True, True, True, True, True, True, True, True,
             True, True, True, True, True, True, True],
            [True, True, True, True, True, True, True, True,
             True, True, True, True, True, True, True],
            [True, True, True, True, True, True, True, True,
             True, True, True, True, True, True, True],
            [True, True, True, True, True, True, True, True,
             True, True, True, True, True, True, True],
            [True, True, True, True, True, True, True, True,
             True, True, True, True, True, True, True],
            [True, True, True, True, True, True, True, True,
             True, True, True, True, True, True, True]
        ]

        self.assertListEqual(createPUM(lcm, cmv), expected_pum)

    def test_fuv_all_true(self):
        """ Test that FUV is fully activated when PUV is fully activated
            and PUM is fully activated on every row.

            Should return an array with only True values.
        """
        puv = [True,
               True,
               True]
        pum = [[True, True, True],
               [True, True, True],
               [True, True, True]]
        fuv = create_fuv(puv, pum)
        assert all(fuv)

    def test_fuv_mixed(self):
        """ Test that indexes of FUV which either has a corresponding
            deactivated PUV or a corresponding fully activated PUM row.

            Should return an array with correct True, False values.
        """
        puv = [False,
               True,
               True]
        pum = [[False, False, True],
               [True, True, True],
               [True, False, True]]
        fuv = create_fuv(puv, pum)
        assert fuv == [True, True, False]
