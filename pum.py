import unittest
from typing import List
from custom_types import Parameters, BooleanOperator
from globals import MATRIX_DIMENSION


def createPUM(params: Parameters) -> List[List[bool]]:
    pum = [[None for i in range(MATRIX_DIMENSION)]
           for j in range(MATRIX_DIMENSION)]
    for i in range(0, MATRIX_DIMENSION):
        for j in range(0, MATRIX_DIMENSION):
            operator = params.LCM[i][j]
            if operator == BooleanOperator.AND.value:
                pum[i][j] = (params.CMV[i] and params.CMV[j])
            elif operator == BooleanOperator.OR.value:
                pum[i][j] = (params.CMV[i] or params.CMV[j])
            elif operator == BooleanOperator.NOT_USED.value:
                pum[i][j] = True
    return pum


class PUMTest(unittest.TestCase):

    def test_positive_intricate(self):
        """
        Test an intricate example case which should evaluate to the expected PUM.
        """

        cmv = [False, True, True, True, False, False, False,
               False, False, False, False, False, False, False, False]

        lcm = [
            ["ANDD", "ANDD", "ORR", "ANDD", "NOTUSED",  "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED",
                "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED"],

            ["ANDD", "ANDD", "ORR", "ORR", "NOTUSED",  "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED",
                "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED"],

            ["ORR", "ORR", "ANDD", "ANDD", "NOTUSED",  "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED",
                "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED"],

            ["ANDD", "ORR", "ANDD", "ANDD", "NOTUSED",  "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED",
                "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED"]
        ] + [["NOTUSED" for i in range(MATRIX_DIMENSION)]
             for j in range(11)]

        parameters = Parameters(cmv, lcm,  0, 0, 0, 0,
                                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

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

        self.assertListEqual(createPUM(parameters), expected_pum)
