import numpy as np
import json

codes = ["bb", "color", "surface"]

for code in codes:
  chkmat = np.load(f"./{code}.npy")
  m = chkmat.shape[0] # num checks
  n = chkmat.shape[1] # num vars

  mat = [[bool(chkmat[i][j] != 0) for j in range(n)] for i in range(m)]
  with open(f"./{code}.json", "w") as f:
    json.dump(mat, f)
