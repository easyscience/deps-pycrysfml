import inspect
import matplotlib.pyplot as plt
import numpy as np
from numpy.testing import assert_almost_equal
import os
import sys

sys.path.append(os.getcwd())  # to access tests/helpers.py
from tests.helpers import (chi_squared,
                           load_from_json,
                           path_to_input,
                           path_to_desired,
                           dat_to_ndarray,
                           sub_to_ndarray)

from pycrysfml import cfml_py_utilities


PLOT_CHARTS_IN_TESTS = bool(int(os.environ.get('PLOT_CHARTS_IN_TESTS', '0')))

# Help functions

def compute_cw_pattern(study_dict:dict):
    #_, y = crysfml08lib.f_powder_pattern_from_json(study_dict)  # returns x and y arrays
    x, y = cfml_py_utilities.cw_powder_pattern_from_dict(study_dict)  # returns x and y arrays
    return x, y

def compute_tof_pattern(study_dict:dict):
    x, y = cfml_py_utilities.tof_powder_pattern_from_dict(study_dict)  # returns x and y arrays
    return x, y

def plot_charts(desired_x:np.ndarray,
                desired_y:np.ndarray,
                actual_x:np.ndarray,
                actual_y:np.ndarray,
                chi2:float,
                skip_last:int = 0): # skip last points as FullProf calculation is not good there
    if not PLOT_CHARTS_IN_TESTS:
        return
    vertical_shift = 10
    if skip_last:
        plt.plot(desired_x[:-skip_last], desired_y[:-skip_last])
        plt.plot(actual_x[:-skip_last], actual_y[:-skip_last], linestyle='dotted')
        plt.plot(desired_x[:-skip_last], actual_y[:-skip_last] - desired_y[:-skip_last] - vertical_shift)
    else:
        plt.plot(desired_x, desired_y)
        plt.plot(actual_x, actual_y, linestyle='dotted')
        plt.plot(desired_x, actual_y - desired_y - vertical_shift)
    #plt.xlim(75000, 76000)
    plt.legend(["FullProf", "PyCrysFML"])
    plt.title(f'{inspect.stack()[1].function}, chi2={chi2:.1f}')
    plt.show()

# Tests

def _NOTWORKING_test__cw_powder_pattern_from_dict__Al2O3_uvwx_noassym(benchmark):
    # input
    project = load_from_json(path_to_input('al2o3_uvwx_no-assym.json'))
    # actual
    actual_x, actual_y = benchmark(compute_cw_pattern, project)
    actual_y = actual_y / actual_y.max() * 100  # normalize
    # desired
    desired_x, desired_y = sub_to_ndarray(path_to_desired('al2o3_uvwx_no-assym.sim'))
    desired_x = desired_x + project['experiments'][0]['NPD']['_pd_meas_2theta_offset']
    desired_y = desired_y / desired_y.max() * 100  # normalize
    # goodness of fit
    chi2 = chi_squared(desired_y, actual_y)
    # compare
    assert_almost_equal(chi2, 6.97, decimal=1, verbose=True)
    assert_almost_equal(actual_x, desired_x, decimal=3, verbose=True)
    assert_almost_equal(actual_y, desired_y, decimal=0, verbose=True)
    # plot
    plot_charts(desired_x, desired_y, actual_x, actual_y, chi2)

def test__cw_powder_pattern_from_dict__PbSO4(benchmark):
    # input
    project = load_from_json(path_to_input('pbso4_cw.json'))
    # actual
    actual_x, actual_y = benchmark(compute_cw_pattern, project)
    actual_y = actual_y / actual_y.max() * 100  # normalize
    # desired
    desired_x, desired_y = sub_to_ndarray(path_to_desired('pbso4_cw.sub'))
    desired_x = desired_x + project['experiments'][0]['NPD']['_pd_meas_2theta_offset']
    desired_y = desired_y / desired_y.max() * 100  # normalize
    # TO DO: skip last points
    skip_last = 100
    # goodness of fit
    chi2 = chi_squared(desired_y, actual_y, skip_last)
    # compare
    assert_almost_equal(chi2, 43.2, decimal=1, verbose=True)
    assert_almost_equal(actual_x, desired_x, decimal=3, verbose=True)
    assert_almost_equal(actual_y[:-skip_last], desired_y[:-skip_last], decimal=0, verbose=True)
    # plot
    plot_charts(desired_x, desired_y, actual_x, actual_y, chi2, skip_last)

def test__cw_powder_pattern_from_dict__PbSO4_custom_x(benchmark):
    # input
    project = load_from_json(path_to_input('pbso4_cw_custom-x.json'))
    # actual
    actual_x, actual_y = benchmark(compute_cw_pattern, project)
    actual_y = actual_y / actual_y.max() * 100  # normalize
    # desired
    desired_x, desired_y = sub_to_ndarray(path_to_desired('pbso4_cw.sub'))
    desired_x = desired_x + project['experiments'][0]['NPD']['_pd_meas_2theta_offset']
    desired_y = desired_y / desired_y.max() * 100  # normalize
    # TO DO: skip last points
    skip_last = 120
    # goodness of fit
    chi2 = chi_squared(desired_y, actual_y, skip_last)
    # compare
    assert_almost_equal(chi2, 43.2, decimal=1, verbose=True)
    assert_almost_equal(actual_x, desired_x, decimal=3, verbose=True)
    assert_almost_equal(actual_y[:-skip_last], desired_y[:-skip_last], decimal=0, verbose=True)
    # plot
    plot_charts(desired_x, desired_y, actual_x, actual_y, chi2, skip_last)

def test__tof_powder_pattern_from_dict__Al2O3(benchmark):
    # input
    project = load_from_json(path_to_input('al2o3_tof.json'))
    # actual
    actual_x, actual_y = benchmark(compute_tof_pattern, project)
    actual_y = actual_y / actual_y.max() * 100  # normalize
    # desired
    desired_x, desired_y = sub_to_ndarray(path_to_desired('al2o3_tof.sim'))
    desired_x = desired_x + project['experiments'][0]['NPD']['_pd_meas_tof_offset']
    desired_y = desired_y / desired_y.max() * 100  # normalize
    # goodness of fit
    chi2 = chi_squared(desired_y, actual_y)
    # compare
    assert_almost_equal(chi2, 12.1, decimal=1, verbose=True)
    assert_almost_equal(actual_x, desired_x, decimal=3, verbose=True)
    #assert_almost_equal(actual_y, desired_y, decimal=0, verbose=True)
    # plot
    plot_charts(desired_x, desired_y, actual_x, actual_y, chi2)

def test__tof_powder_pattern_from_dict__ncaf_custom_x(benchmark):
    # input
    project = load_from_json(path_to_input('ncaf_tof_custom-x.json'))
    # actual
    actual_x, actual_y = benchmark(compute_tof_pattern, project)
    actual_y = actual_y / actual_y.max() * 100  # normalize
    # desired
    desired_x, desired_y = dat_to_ndarray(path_to_desired('ncaf_tof.sub'), skip_begin=1, usecols=(0,1))
    desired_x = desired_x + project['experiments'][0]['NPD']['_pd_meas_tof_offset']
    #print(list(desired_x))
    desired_y = desired_y / desired_y.max() * 100  # normalize
    # goodness of fit
    chi2 = chi_squared(desired_y, actual_y)
    # compare
    assert_almost_equal(chi2, 0.08, decimal=2, verbose=True)
    assert_almost_equal(actual_x, desired_x, decimal=3, verbose=True)
    assert_almost_equal(actual_y, desired_y, decimal=0, verbose=True)
    # plot
    plot_charts(desired_x, desired_y, actual_x, actual_y, chi2)

def test__tof_powder_pattern_from_dict__Si(benchmark):
    # input
    project = load_from_json(path_to_input('si_tof.json'))
    # actual
    actual_x, actual_y = benchmark(compute_tof_pattern, project)
    actual_y = actual_y / actual_y.max() * 100  # normalize
    # desired
    desired_x, desired_y = sub_to_ndarray(path_to_desired('si_tof.sub'))
    desired_x = desired_x + project['experiments'][0]['NPD']['_pd_meas_tof_offset']
    desired_y = desired_y / desired_y.max() * 100  # normalize
    # goodness of fit
    chi2 = chi_squared(desired_y, actual_y)
    # compare
    assert_almost_equal(chi2, 0.49, decimal=2, verbose=True)
    assert_almost_equal(actual_x, desired_x, decimal=3, verbose=True)
    #assert_almost_equal(actual_y, desired_y, decimal=0, verbose=True)
    # plot
    plot_charts(desired_x, desired_y, actual_x, actual_y, chi2)

# Debug

if __name__ == '__main__':
    pass
