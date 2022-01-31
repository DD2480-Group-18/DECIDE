from typing import List
import unittest


def create_fuv(puv: List[bool], pum: List[List[bool]]):
    return list(not puv[i] or all(pum[i]) for i in range(len(puv)))


class FUVTest(unittest.TestCase):
    def test_all_true(self):
        puv = [True,
               True,
               True]
        pum = [[True, True, True],
               [True, True, True],
               [True, True, True]]
        fuv = create_fuv(puv, pum)
        assert all(fuv)

    def test_all_false(self):
        puv = [False,
               False,
               False]
        pum = [[False, False, False],
               [False, False, False],
               [False, False, False]]
        fuv = create_fuv(puv, pum)
        assert all(fuv)

    def test_mixed(self):
        puv = [False,
               True,
               True]
        pum = [[False, False, True],
               [True, True, True],
               [True, False, True]]
        fuv = create_fuv(puv, pum)
        assert fuv == [True, True, False]
