import numpy as np
import json

chkmat = np.load("./chkmat.npy")
m = chkmat.shape[0] # num checks
n = chkmat.shape[1] # num vars

mat = [[bool(chkmat[i][j] != 0) for j in range(n)] for i in range(m)]
with open("./chkmat.json", "w") as f:
  json.dump(mat, f)

prior = np.load("./prior.npy")
prior_lst = [float(prior[i]) for i in range(n)]
with open("./prior.json", "w") as f:
  json.dump(prior_lst, f)

syndromes = np.load("./syndromes.npy")
batch_size = syndromes.shape[0]
syndrome_lst = [[bool(syndromes[i][j] != 0) for j in range(m)] for i in range(batch_size)]
with open("./syndromes.json", "w") as f:
  json.dump(syndrome_lst, f)

gammas = np.load("./gamma.npy")
gammas_lst = [float(gammas[i]) for i in range(n)]
with open("./gamma.json", "w") as f:
  json.dump(gammas_lst, f)

for suffix in ["bp", "dmembp"]:
  ehat = np.load(f"./ehat_{suffix}.npy")
  ehat_mat = [[bool(ehat[i][j] != 0) for j in range(n)] for i in range(batch_size)]
  with open(f"./ehat_{suffix}.json", "w") as f:
    json.dump(ehat_mat, f)
