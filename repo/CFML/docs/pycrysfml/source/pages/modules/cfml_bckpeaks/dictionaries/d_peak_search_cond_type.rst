.. py:data:: peak_search_cond_type

    Parameters controlling the automatic determination of peaks and backgrounds (see :ref:`automatic_peak_background_search <automatic_peak_background_search>`)

    Keys and types:

    - **peak_threshold** (float): peak threshold.
    - **shoulder_threshold** (float): shoulder threshold.
    - **bkg_threshold** (float): background threshold.
    - **kindofpeaks** (int): 1: single peak | 2: doublet (Cu-ka) | 3: doublet (Mo-ka) | 4: doublet (Co-ka), (default = 1).
    - **iterations** (int): number of iterations.