from custom_types import BooleanOperator, MATRIX_DIMENSION


def create_pum(lcm, cmv):
    pum = [[None for _ in range(MATRIX_DIMENSION)]
           for _ in range(MATRIX_DIMENSION)]
    for i in range(0, MATRIX_DIMENSION):
        for j in range(0, MATRIX_DIMENSION):
            operator = lcm[i][j]
            if operator == BooleanOperator.AND.value:
                pum[i][j] = (cmv[i] and cmv[j])
            elif operator == BooleanOperator.OR.value:
                pum[i][j] = (cmv[i] or cmv[j])
            else:
                pum[i][j] = True
    return pum
