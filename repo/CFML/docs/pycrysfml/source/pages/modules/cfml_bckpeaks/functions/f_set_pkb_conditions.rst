.. py:function:: set_pkb_conditions(**kwargs)

    Set the parameters that controls the algorithm of function :ref:`automatic_peak_background_search <_automatic_peak_background_search>`.

    :param kwargs: optional arguments.
    :param kwargs['pk_th']: peak threshold (default = 0.02).
    :type kwargs['pk_th']: float
    :param kwargs['sh_th']: shoulder threshold (default = 2.0).
    :type kwargs['sh_th']: float
    :param kwargs['bg_th']: background threshold (default = 0.05).
    :type kwargs['bg_th']: float
    :param kwargs['peak_kind']: 1: single peak | 2: doublet (Cu-ka) | 3: doublet (Mo-ka) | 4: doublet (Co-ka), (default = 1).
    :type kwargs['peak_kind']: int
    :param kwargs['iter']: Number of iterations (default = 3).
    :type kwargs['iter']: int
    :returns: None.

    .. code-block:: python

        >>> from pycrysfml import cfml_bckpeaks
        >>> cfml_bckpeaks.set_pkb_conditions(pk_th=1.0,iter=5)
        >>> pkb = cfml_bckpeaks.get_pkb_conditions()
        >>> for k in pkb:
        ...   print(f'{k:>14}: {pkb[k]}')
        ...
                fortran_type: peak_search_cond_type
              peak_threshold: 1.0
          shoulder_threshold: 2.0
               bkg_threshold: 0.05000000074505806
                 kindofpeaks: 1
                  iterations: 5

