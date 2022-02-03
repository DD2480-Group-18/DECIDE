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
