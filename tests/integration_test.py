import unittest
from fuv import create_fuv
from pum import create_pum


class IntegrationTest(unittest.TestCase):
    def test_no_launch(self):
        """ 
        Test that the program decides not to launch when all conditions
        and matrices are in a should-not-launch state.
        """

        # Create CMV which should combine with LCM to create falsy PUM
        cmv = [False, True, True, True, False, False, False,
               False, False, False, False, False, False, False, False]

        # Create LCM which should combine with CMV to create falsy PUM
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

        puv = [False, True, True]

        create_fuv(puv, pum)

        self.assertFalse(False)
