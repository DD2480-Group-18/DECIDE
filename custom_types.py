from enum import Enum
from typing import List, NamedTuple


class BooleanOperator(Enum):
    AND = "ANDD"
    OR = "ORR"
    NOT_USED = "NOTUSED"


class Parameters(NamedTuple):
    CMV: List[bool]
    LCM: List[List[BooleanOperator]]
    LENGTH1: float
    RADIUS1: float
    EPISILON: float
    AREA1: float
    Q_PTS: int
    QUADS: int
    N_PTS: int
    K_PTS: int
    A_PTS: int
    B_PTS: int
    C_PTS: int
    D_PTS: int
    E_PTS: int
    F_PTS: int
    G_PTS: int
    LENGTH2: float
    RADIUS2: float
    AREA2: float