import argparse
import copy
import os
import matplotlib.pyplot as plt
import numpy as np

from crysfml import cfml_utilities


STUDY_DICT_PM3M = {
  "phases": [
    {
      "SrTiO3": {
        "_space_group_name_H-M_alt": "P m -3 m",
        "_cell_length_a": 3.9,
        "_cell_length_b": 3.9,
        "_cell_length_c": 3.9,
        "_cell_angle_alpha": 90,
        "_cell_angle_beta": 90,
        "_cell_angle_gamma": 90,
        "_atom_site": [
          {
            "_label": "Sr",
            "_type_symbol": "Sr",
            "_fract_x": 0.5,
            "_fract_y": 0.5,
            "_fract_z": 0.5,
            "_occupancy": 1,
            "_adp_type": "Biso",
            "_B_iso_or_equiv": 0.40
          },
          {
            "_label": "Ti",
            "_type_symbol": "Ti",
            "_fract_x": 0,
            "_fract_y": 0,
            "_fract_z": 0,
            "_occupancy": 1,
            "_adp_type": "Biso",
            "_B_iso_or_equiv": 0.50
          },
          {
            "_label": "O",
            "_type_symbol": "O",
            "_fract_x": 0.5,
            "_fract_y": 0,
            "_fract_z": 0,
            "_occupancy": 1,
            "_adp_type": "Biso",
            "_B_iso_or_equiv": 0.65
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
        "_diffrn_radiation_wavelength": 1.27,
        "_pd_instr_resolution_u": 0.02,
        "_pd_instr_resolution_v": -0.02,
        "_pd_instr_resolution_w": 0.12,
        "_pd_instr_resolution_x": 0.0015,
        "_pd_instr_resolution_y": 0,
        "_pd_instr_reflex_asymmetry_p1": 0,
        "_pd_instr_reflex_asymmetry_p2": 0,
        "_pd_instr_reflex_asymmetry_p3": 0,
        "_pd_instr_reflex_asymmetry_p4": 0,
        "_pd_meas_2theta_offset": 0,
        "_pd_meas_2theta_range_min": 1,
        "_pd_meas_2theta_range_max": 140,
        "_pd_meas_2theta_range_inc": 0.05
      }
    }
  ]
}

# Help functions

def parsed_args():
    parser = argparse.ArgumentParser(formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument("--calc",
                        default="none",
                        choices=['none', 'pm3m', 'pnma-then-pm3m'],
                        type=str.lower,
                        help="calculation order: only SG Pm-3m, or first SG Pnma and then SG Pm-3m")
    parser.add_argument("--plot",
                        action='store_true',
                        help="plot simulated patterns after previous runs")
    return parser.parse_args()

def generated_x_array(study_dict:dict):
    experiment = study_dict['experiments'][0]['NPD']
    start = experiment['_pd_meas_2theta_range_min']
    stop = experiment['_pd_meas_2theta_range_max']
    step = experiment['_pd_meas_2theta_range_inc']
    x = np.arange(start=start, stop=stop+step, step=step)
    return x

def compute_pattern(study_dict:dict):
    _, y = cfml_utilities.powder_pattern_from_json(study_dict)  # returns x and y arrays
    #_, y = crysfml08lib.f_powder_pattern_from_json(study_dict)  # returns x and y arrays
    y = y.astype(np.float64)
    return y

# Main

if __name__ == '__main__':
    ARGS = parsed_args()

    if ARGS.calc == 'pm3m':
        study_dict = copy.deepcopy(STUDY_DICT_PM3M)

        y_pm3m = compute_pattern(study_dict)

        file_name = 'y-pm3m_first-calc.txt'
        np.savetxt(file_name, y_pm3m, fmt='%10.2f')

        sg = study_dict['phases'][0]['SrTiO3']['_space_group_name_H-M_alt']
        print(f"First calculation for SG '{sg}' saved into '{file_name}'")

    if ARGS.calc == 'pnma-then-pm3m':
        study_dict = copy.deepcopy(STUDY_DICT_PM3M)

        study_dict['phases'][0]['SrTiO3']['_space_group_name_H-M_alt'] = 'P n m a'
        y_pnma = compute_pattern(study_dict)

        study_dict['phases'][0]['SrTiO3']['_space_group_name_H-M_alt'] = 'P m -3 m'
        y_pm3m = compute_pattern(study_dict)

        file_name = 'y-pm3m_second-calc.txt'
        np.savetxt(file_name, y_pm3m, fmt='%10.2f')

        sg = study_dict['phases'][0]['SrTiO3']['_space_group_name_H-M_alt']
        print(f"Second calculation for SG '{sg}' saved into '{file_name}'")

    if ARGS.plot:
        study_dict = copy.deepcopy(STUDY_DICT_PM3M)

        x = generated_x_array(study_dict)

        y_pm3m_1 = np.loadtxt('y-pm3m_first-calc.txt', unpack=True)
        y_pm3m_2 = np.loadtxt('y-pm3m_second-calc.txt', unpack=True)

        plt.plot(x, y_pm3m_1, '-', linewidth=3)
        plt.plot(x, y_pm3m_2, '-', linewidth=2)
        plt.plot(x, y_pm3m_1-y_pm3m_2, '-')
        #plt.xlim([31, 35])

        plt.legend(["Pm-3m, first calc", "Pm-3m, second calc", "diff"])

        plt.show()
