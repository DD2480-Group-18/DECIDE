from typing import List

from globals import NUMPOINTS, PI, X, Y
from types import Parameters

from helpers import get_angle


def createCVM(params: Parameters) -> List[bool]:
    CVM: List[bool] = []
    if condition0(params):
        CVM[0] = True
    if condition1(params):
        CVM[1] = True
    if condition2(params):
        CVM[2] = True

def condition0(params):
    return 0 <= params.LENGTH1

def condition1(params):
    return 0 <= params.RADIUS1

def condition2(params):
    for i in range(0, NUMPOINTS-3):
        # Create slice
        yslice = Y[i:i+3]
        xslice = X[i:i+3]
        # Create a, b vectors
        ax = xslice[0] - xslice[1]
        ay = yslice[0] - yslice[1]
        bx = xslice[2] - xslice[1]
        by = yslice[2] - yslice[1]
        angle = get_angle(ax, ay, bx, by)
        return angle < (PI - params.EPSILON) or angle > (PI + params.EPSILON)

def condition3(params: Parameters):
    for i in range(0, NUMPOINTS-3):
        # Create slice
        yslice = Y[i:i+3]
        xslice = X[i:i+3]
        # Create a, b vectors
        ax = xslice[0] - xslice[1]
        ay = yslice[0] - yslice[1]
        bx = xslice[2] - xslice[1]
        by = yslice[2] - yslice[1]
        area = get_area(ax, ay, bx, by)
