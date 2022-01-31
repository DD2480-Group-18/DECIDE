import unittest
from typing import List
from custom_types import Parameters, BooleanOperator
from globals import MATRIX_DIMENSION


def createPUM(params: Parameters) -> List[List[bool]]:
    assert(len(params.LCM) == MATRIX_DIMENSION)
    assert(len(params.CMV) == MATRIX_DIMENSION)

    pum = [[None for i in range(MATRIX_DIMENSION)]
           for j in range(MATRIX_DIMENSION)]
    for i in range(0, MATRIX_DIMENSION):
        assert(len(params.LCM[i]) == MATRIX_DIMENSION)
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

    def test_faulty_LCM(self):
        """
        Test whether a faulty LCM (in dimensions) fails.
        """
        cmv = [False, True, True, False, False, False, False,
               True, False, False, True, False, False, False, False]

        # length MATRIX_DIMENSION - 1, should be MATRIX_DIMENSION
        lcm = [["NOTUSED" for i in range(MATRIX_DIMENSION)]
               for j in range(MATRIX_DIMENSION - 1)]

        parameters = Parameters(cmv, lcm,  0, 0, 0, 0,
                                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        did_raise_assertion_error = False
        try:
            createPUM(parameters)
        except AssertionError:
            did_raise_assertion_error = True

        self.assertTrue(did_raise_assertion_error)

    def test_faulty_CMV(self):
        """
        Test whether a faulty CMV (in dimensions) fails.
        """
        cmv = [False, True, True, False, False, False, False,
               True, False, False, True, False, False, False]

        lcm = [["NOTUSED" for i in range(MATRIX_DIMENSION)]
               for j in range(MATRIX_DIMENSION)]

        parameters = Parameters(cmv, lcm,  0, 0, 0, 0,
                                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)

        did_raise_assertion_error = False
        try:
            createPUM(parameters)
        except AssertionError:
            did_raise_assertion_error = True

        self.assertTrue(did_raise_assertion_error)

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
