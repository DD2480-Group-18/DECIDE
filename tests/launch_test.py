import unittest
from launch import launch


class Test(unittest.TestCase):
    def test_launch_success(self):
        fuv = [True, True, True]
        isLaunched = launch(fuv)
        self.assertTrue(isLaunched)

    def test_launch_failure(self):
        fuv = [False, True, True]
        isLaunched = launch(fuv)
        self.assertFalse(isLaunched)
