import copy
import matplotlib.pyplot as plt
import numpy as np

from crysfml import cfml_py_utilities

##################
# Global variables
##################

STUDY_DICT = {
  "phases": [
    {
      "SrTiO3": {
        "_space_group_name_H-M_alt": "P m -3 m",
        "_cell_length_a": 3.88,
        "_cell_length_b": 3.88,
        "_cell_length_c": 3.88,
        "_cell_angle_alpha": 90,
        "_cell_angle_beta": 90,
        "_cell_angle_gamma": 90,
        "_atom_site": [
          {
            "_label": "La",
            "_type_symbol": "La",
            "_fract_x": 0,
            "_fract_y": 0,
            "_fract_z": 0,
            "_occupancy": 0.5,
            "_adp_type": "Biso",
            "_B_iso_or_equiv": 0.5
          },
          {
            "_label": "Ba",
            "_type_symbol": "Ba",
            "_fract_x": 0,
            "_fract_y": 0,
            "_fract_z": 0,
            "_occupancy": 0.5,
            "_adp_type": "Biso",
            "_B_iso_or_equiv": 0.5
          },
          {
            "_label": "Co",
            "_type_symbol": "Co",
            "_fract_x": 0.5,
            "_fract_y": 0.5,
            "_fract_z": 0.5,
            "_occupancy": 1.0,
            "_adp_type": "Biso",
            "_B_iso_or_equiv": 0.5
          },
          {
            "_label": "O",
            "_type_symbol": "O",
            "_fract_x": 0,
            "_fract_y": 0.5,
            "_fract_z": 0.5,
            "_occupancy": 1,
            "_adp_type": "Biso",
            "_B_iso_or_equiv": 0.5
          }
        ]
      }
    }
  ],
  "experiments": [
    {
      "NPD": {
        "_diffrn_source": "nuclear reactor",
        "_diffrn_radiation_probe": "neutron",
        "_diffrn_radiation_wavelength": 1.494,
        "_pd_instr_resolution_u": 0.3,
        "_pd_instr_resolution_v": -0.5,
        "_pd_instr_resolution_w": 0.7,
        "_pd_instr_resolution_x": 0,
        "_pd_instr_resolution_y": 0,
        "_pd_instr_reflex_asymmetry_p1": 0,
        "_pd_instr_reflex_asymmetry_p2": 0,
        "_pd_instr_reflex_asymmetry_p3": 0,
        "_pd_instr_reflex_asymmetry_p4": 0,
        "_pd_meas_2theta_offset": 0,
        "_pd_meas_2theta_range_min": 80,
        "_pd_meas_2theta_range_max": 164.7,
        "_pd_meas_2theta_range_inc": 0.01
      }
    }
  ]
}

################
# Help functions
################

def generated_x_array(study_dict:dict):
    experiment = study_dict['experiments'][0]['NPD']
    start = experiment['_pd_meas_2theta_range_min']
    stop = experiment['_pd_meas_2theta_range_max']
    step = experiment['_pd_meas_2theta_range_inc']
    x = np.arange(start=start, stop=stop+step, step=step)
    return x

def compute_pattern(study_dict:dict):
    _, y = cfml_py_utilities.cw_powder_pattern_from_dict(study_dict)  # returns x and y arrays
    y = np.asarray(y) #y.astype(np.float64)
    return y

######
# MAIN
######

study_dict = copy.deepcopy(STUDY_DICT)

# set upper limit to 140deg
study_dict['experiments'][0]['NPD']['_pd_meas_2theta_range_max'] = 140.0
x_1 = generated_x_array(study_dict)
y_1 = compute_pattern(study_dict)

# set upper limit to 160deg
study_dict['experiments'][0]['NPD']['_pd_meas_2theta_range_max'] = 150.0
x_2 = generated_x_array(study_dict)
y_2 = compute_pattern(study_dict)

# set upper limit to 170deg
study_dict['experiments'][0]['NPD']['_pd_meas_2theta_range_max'] = 160.0
x_3 = generated_x_array(study_dict)
y_3 = compute_pattern(study_dict)

# plot results§
plt.plot(x_1, y_1, '-', linewidth=2)
plt.plot(x_2, y_2, '--', linewidth=2)
plt.plot(x_3, y_3, ':', linewidth=2)
plt.legend(["2theta_range_max = 140", "2theta_range_max = 160", "2theta_range_max = 180"])
plt.show()
