from types import Parameters, BooleanOperator

def createPUM(params: Parameters) -> List[List[bool]]:
    pum = [[]]
    for i in range(0, len(params.LCM)):
        for j in range(0, len(params.LCM[i])):
            operator = params.LCM[i][j]
            if operator == BooleanOperator.AND:
                params.PUM[i][j] = params.CMV[i] and params.CMV[j]
            elif operator == BooleanOperator.OR:
                params.PUM[i][j] = params.CMV[i] or params.CMV[j]
            elif operator == BooleanOperator.NOT_USED:
                params.PUM[i][j] = True
    return pum