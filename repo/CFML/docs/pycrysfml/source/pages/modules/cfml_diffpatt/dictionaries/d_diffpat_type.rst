.. py:data:: diffpat_e_type

    A diffraction pattern.

    Keys and types:

    - **bgr** (ndarray, dim=(:), float32): background.
    - **filename** (str): file name.
    - **filepath** (str): file path.
    - **instr** (str): instrument name.
    - **kindrad** (str): type of radiation.
    - **monitor** (float): monitor counts.
    - **norm_mon** (float): monitor normalization.
    - **npts** (int): number of points.
    - **scal** (float): scale factor.
    - **scatvar** (str): scattering variable -2theta, TOF, Q, s, d-spacing-.
    - **sigma** (ndarray, dim=(:), float32): sigma values.
    - **sigvar** (bool): if True, sigma represents standard deviation, otherwise, it represents the variance.
    - **step** (float): step value.
    - **title** (str): title of the diffraction pattern.
    - **tsample** (float): sample temperature.
    - **tset** (float): wished temperature.
    - **wave** (float): wavelength.
    - **x** (ndarray, dim=(:), float32): x values.
    - **xmax** (float): maximum value of the scattering variable.
    - **xmin** (float): minimum value of the scattering variable.
    - **y** (ndarray, dim=(:), float32): y values.
    - **ycalc** (ndarray, dim=(:), float32): calculated intensity.
    - **ymax** (float): maximum value of the intensity.
    - **ymin** (float): minimum value of the intensity.