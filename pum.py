from custom_types import BooleanOperator, MATRIX_DIMENSION


def createPUM(new_lcm, new_cmv):
    pum = [[None for _ in range(MATRIX_DIMENSION)]
           for _ in range(MATRIX_DIMENSION)]
    for i in range(0, MATRIX_DIMENSION):
        for j in range(0, MATRIX_DIMENSION):
            operator = new_lcm[i][j]
            if operator == BooleanOperator.AND.value:
                pum[i][j] = (new_cmv[i] and new_cmv[j])
            elif operator == BooleanOperator.OR.value:
                pum[i][j] = (new_cmv[i] or new_cmv[j])
            else:
                pum[i][j] = True
    return pum
