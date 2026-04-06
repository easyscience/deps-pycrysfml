import copy
import json
from io import StringIO
import matplotlib.pyplot as plt
import os

from pycrysfml import cfml_utilities

################
# Help functions
################

def file_path(file_name:str):
  script_dir = os.path.dirname(__file__)
  return os.path.join(script_dir, file_name)

def read_json(file_name:str):
  with open(file_path(file_name), 'r') as file:
    return json.load(file)

def dat_to_ndarray(file_name:str, skip_begin:int=0, skip_end:int=0, usecols=(0,)):
  with open(file_name, 'r') as file:
    lines = file.readlines()  # reads into list
  del lines[:skip_begin]  # deletes requested number of first lines
  del lines[len(lines) - skip_end:]  # deletes requested number o
  joined = '\n'.join(lines)  # joins into single string# f last lines
  data = np.genfromtxt(StringIO(joined), usecols=usecols)  # converts string to ndarray
  return data

def read_fullprof(file_name:str, column:int=1):
  file_name = file_path(file_name)
  return dat_to_ndarray(file_name, skip_begin=6, skip_end=28, usecols=(column,))

def compute_pattern(study_dict:dict):
    _, y = cfml_utilities.powder_pattern_from_json(study_dict)  # returns x and y arrays
    y = y.astype(np.float64)
    return y

def normalize_pattern(y:np.ndarray):
  scale = 0.023
  return scale * y

######
# MAIN
######

# Type I - 'lbco'
# All 4 atoms: La, Ba, Co, O
lbco_dict = read_json('lbco.json')

# Type II - 'lbc'
# Remove one atom - O. Remaining atoms: La, Ba, Co
lbc_dict = copy.deepcopy(lbco_dict)
del lbc_dict['phases'][0]['lbco']['_atom_site'][3]

# Type III - 'o'
# Remove three atoms - La, Ba, Co. Remaining atoms: O
o_dict = copy.deepcopy(lbco_dict)
del o_dict['phases'][0]['lbco']['_atom_site'][2]
del o_dict['phases'][0]['lbco']['_atom_site'][1]
del o_dict['phases'][0]['lbco']['_atom_site'][0]

# compute patterns with PyCFML
y_lbco_pycrysfml = normalize_pattern(compute_pattern(lbco_dict))
y_lbc_pycrysfml = normalize_pattern(compute_pattern(lbc_dict))
y_o_pycrysfml = normalize_pattern(compute_pattern(o_dict))

# read patterns calculated with FullProf
x_obs = read_fullprof('lbco.prf', column=0)
y_obs = read_fullprof('lbco.prf', column=1)
y_full_fullprof = read_fullprof('full.prf', column=2)
y_lbco_fullprof = read_fullprof('lbco.prf', column=2)
y_lbc_fullprof = read_fullprof('lbc.prf', column=2)
y_o_fullprof = read_fullprof('o.prf', column=2)

# plot results
plt.title("Atoms: La, Ba, Co, O. (Refinement)")
plt.plot(x_obs, y_obs, 'b.', linewidth=2)
plt.plot(x_obs, y_full_fullprof, 'r-', linewidth=2)
plt.legend(["Y meas", "Y calc (FullProf)"])
plt.show()

plt.title("Atoms: La, Ba, Co, O. (Simulation)")
plt.plot(x_obs, y_lbco_fullprof, '-', linewidth=4)
plt.plot(x_obs, y_lbco_pycrysfml, '-', linewidth=2)
plt.legend(["Y meas", "Y calc (FullProf)", "Y calc (PyCrysFML)"])
plt.show()

plt.title("Atoms: La, Ba, Co. (Simulation)")
plt.plot(x_obs, y_lbc_fullprof, '-', linewidth=4)
plt.plot(x_obs, y_lbc_pycrysfml, '-', linewidth=2)
plt.legend(["Y meas", "Y calc (FullProf)", "Y calc (PyCrysFML)"])
plt.show()

plt.title("Atoms: O. (Simulation)")
plt.plot(x_obs, y_o_fullprof, '-', linewidth=4)
plt.plot(x_obs, y_o_pycrysfml, '-', linewidth=2)
plt.legend(["Y meas", "Y calc (FullProf)", "Y calc (PyCrysFML)"])
plt.show()