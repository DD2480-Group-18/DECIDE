import unittest
from custom_types import MATRIX_DIMENSION
from pum import create_pum


class PUMTest(unittest.TestCase):
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

        self.assertListEqual(create_pum(lcm, cmv), expected_pum)
