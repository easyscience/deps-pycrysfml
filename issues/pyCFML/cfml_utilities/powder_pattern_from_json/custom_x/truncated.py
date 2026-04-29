import copy
import json
import matplotlib.pyplot as plt
import numpy as np
import os
from importlib.metadata import version

from crysfml import cfml_py_utilities

################
# Help functions
################

def load_crysfml_dict(file_path:str):
    script_dir = os.path.dirname(__file__)
    file_path = os.path.join(script_dir, file_path)
    with open(file_path) as json_file:
        data = json.load(json_file)
    return data

def generated_x_array(crysfml_dict:dict):
    experiment = crysfml_dict['experiments'][0]['NPD']

    if '_pd_meas_2theta_scan' in experiment:
        return experiment['_pd_meas_2theta_scan']

    start = experiment['_pd_meas_2theta_range_min']
    stop = experiment['_pd_meas_2theta_range_max']
    step = experiment['_pd_meas_2theta_range_inc']
    x = np.arange(start=start, stop=stop+step, step=step)
    return x

def compute_pattern(crysfml_dict:dict):
    _, y = cfml_py_utilities.cw_powder_pattern_from_dict(crysfml_dict)  # returns x and y arrays
    y = np.asarray(y) #y.astype(np.float64)
    return y

######
# MAIN
######

print(version('crysfml'))

# x-data: min, max, inc
crysfml_dict_1 = load_crysfml_dict('pbso4_cw.json')
#crysfml_dict_1 = load_crysfml_dict('lbco_cw.json')
x_1 = generated_x_array(crysfml_dict_1)
y_1 = compute_pattern(crysfml_dict_1)

# x-data: list
crysfml_dict_2 = load_crysfml_dict('pbso4_cw_custom-x.json')
#crysfml_dict_2 = load_crysfml_dict('lbco_cw_custom-x.json')
x_2 = generated_x_array(crysfml_dict_2)
y_2 = compute_pattern(crysfml_dict_2)

# plot results
plt.plot(x_1, y_1, '-', linewidth=2)
plt.plot(x_2, y_2, '--', linewidth=2)
plt.title("crysfml 0.2.1")
plt.legend(["x-data: min, max, inc", "x-data: list"])
plt.show()
