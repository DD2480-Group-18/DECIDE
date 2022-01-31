from types import Parameters


def create_fuv(puv: List[bool], pum: List[List[bool]]):
  return list(not puv[i] or all(pum[i]) for i in range(len(puv)))