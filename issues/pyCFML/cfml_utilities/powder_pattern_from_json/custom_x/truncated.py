import copy
import json
import matplotlib.pyplot as plt
import numpy as np
import os
from importlib.metadata import version

from pycrysfml import cfml_py_utilities

################
# Help functions
################

def load_pycrysfml_dict(file_path:str):
    script_dir = os.path.dirname(__file__)
    file_path = os.path.join(script_dir, file_path)
    with open(file_path) as json_file:
        data = json.load(json_file)
    return data

def generated_x_array(pycrysfml_dict:dict):
    experiment = pycrysfml_dict['experiments'][0]['NPD']

    if '_pd_meas_2theta_scan' in experiment:
        return experiment['_pd_meas_2theta_scan']

    start = experiment['_pd_meas_2theta_range_min']
    stop = experiment['_pd_meas_2theta_range_max']
    step = experiment['_pd_meas_2theta_range_inc']
    x = np.arange(start=start, stop=stop+step, step=step)
    return x

def compute_pattern(pycrysfml_dict:dict):
    _, y = cfml_py_utilities.cw_powder_pattern_from_dict(pycrysfml_dict)  # returns x and y arrays
    y = np.asarray(y) #y.astype(np.float64)
    return y

######
# MAIN
######

print(version('pycrysfml'))

# x-data: min, max, inc
pycrysfml_dict_1 = load_pycrysfml_dict('pbso4_cw.json')
#pycrysfml_dict_1 = load_pycrysfml_dict('lbco_cw.json')
x_1 = generated_x_array(pycrysfml_dict_1)
y_1 = compute_pattern(pycrysfml_dict_1)

# x-data: list
pycrysfml_dict_2 = load_pycrysfml_dict('pbso4_cw_custom-x.json')
#pycrysfml_dict_2 = load_pycrysfml_dict('lbco_cw_custom-x.json')
x_2 = generated_x_array(pycrysfml_dict_2)
y_2 = compute_pattern(pycrysfml_dict_2)

# plot results
plt.plot(x_1, y_1, '-', linewidth=2)
plt.plot(x_2, y_2, '--', linewidth=2)
plt.title("pycrysfml 0.2.1")
plt.legend(["x-data: min, max, inc", "x-data: list"])
plt.show()
