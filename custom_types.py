from enum import Enum
from typing import List, NamedTuple

PI: float = 3.14159265358979
MATRIX_DIMENSION: int = 15


class BooleanOperator(Enum):
    AND = "ANDD"
    OR = "ORR"
    NOT_USED = "NOTUSED"


class Parameters:
    def __init__(
        self, X, Y, NUMPOINTS, LENGTH1, RADIUS1, EPSILON, AREA1, Q_PTS, QUADS, DIST, N_PTS, K_PTS, A_PTS, B_PTS, C_PTS, D_PTS, E_PTS, F_PTS, G_PTS, LENGTH2, RADIUS2, AREA2
    ):
        self.X: List[float] = X
        self.Y: List[float] = Y
        self.NUMPOINTS: int = NUMPOINTS
        self.LENGTH1: float = LENGTH1
        self.RADIUS1: float = RADIUS1
        self.EPSILON: float = EPSILON
        self.AREA1: float = AREA1
        self.Q_PTS: int = Q_PTS
        self.QUADS: int = QUADS
        self.DIST: float = DIST
        self.N_PTS: int = N_PTS
        self.K_PTS: int = K_PTS
        self.A_PTS: int = A_PTS
        self.B_PTS: int = B_PTS
        self.C_PTS: int = C_PTS
        self.D_PTS: int = D_PTS
        self.E_PTS: int = E_PTS
        self.F_PTS: int = F_PTS
        self.G_PTS: int = G_PTS
        self.LENGTH2: float = LENGTH2
        self.RADIUS2: float = RADIUS2
        self.AREA2: float = AREA2
