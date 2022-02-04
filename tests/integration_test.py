import unittest
from fuv import create_fuv
from pum import create_pum
from launch import launch
from custom_types import MATRIX_DIMENSION


class IntegrationTest(unittest.TestCase):
    def test_should_launch(self):
        """
        Test that the program decides to launch when all conditions
        and matrices are in a should-launch state.
        """

        # Create CMV which should combine with LCM
        cmv = [True, True, True, True, False, False, False,
               False, False, False, False, False, False, False, False]

        # Create LCM which should combine with CMV
        lcm = [
            ["ANDD", "ORR", "ORR", "ANDD", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED",
             "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED"],

            ["ANDD", "ANDD", "ORR", "ORR", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED",
             "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED"],

            ["ORR", "ORR", "ANDD", "ANDD", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED",
             "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED"],

            ["ANDD", "ORR", "ANDD", "ANDD", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED",
             "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED"]
        ] + [["NOTUSED" for i in range(MATRIX_DIMENSION)] for j in range(11)]

        # create PUM from LCM and CMV
        pum = create_pum(lcm, cmv)

        # create PUV
        puv = [False, True, False, False, False, False, False,
               False, False, False, True, False, False, True, True]

        # create FUV from PUV and PUM
        fuv = create_fuv(puv, pum)

        did_launch = launch(fuv)

        self.assertTrue(did_launch)

    def test_no_launch(self):
        """ 
        Test that the program decides not to launch when all conditions
        and matrices are in a should-not-launch state.
        """

        # Create CMV which should combine with LCM
        cmv = [False, True, True, True, False, False, False,
               False, False, False, False, False, False, False, False]

        # Create LCM which should combine with CMV
        lcm = [
            ["ANDD", "ANDD", "ORR", "ANDD", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED",
             "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED"],

            ["ANDD", "ANDD", "ORR", "ORR", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED",
             "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED"],

            ["ORR", "ORR", "ANDD", "ANDD", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED",
             "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED"],

            ["ANDD", "ORR", "ANDD", "ANDD", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED",
             "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED", "NOTUSED"]
        ] + [["NOTUSED" for i in range(MATRIX_DIMENSION)] for j in range(11)]

        # create PUM from LCM and CMV
        pum = create_pum(lcm, cmv)

        # create PUV
        puv = [False, True, True, False, False, False, False,
               False, False, False, False, False, False, False, False]

        # create FUV from PUV and PUM
        fuv = create_fuv(puv, pum)

        did_launch = launch(fuv)

        self.assertFalse(did_launch)
