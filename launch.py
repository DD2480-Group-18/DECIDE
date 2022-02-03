import unittest
from typing import List


def launch(fuv: List[bool]):
    return all(fuv)


class Test(unittest.TestCase):
    def test_launch_success(self):
        fuv = [True, True, True]
        isLaunched = launch(fuv)
        assert isLaunched == True

    def test_launch_failure(self):
        fuv = [False, True, True]
        isLaunched = launch(fuv)
        assert isLaunched == False
