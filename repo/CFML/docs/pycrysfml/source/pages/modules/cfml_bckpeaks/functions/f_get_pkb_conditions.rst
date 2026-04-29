.. py:function:: get_pkb_conditions()

    List the parameters that controls the algorithm of function :ref:`automatic_peak_background_search <automatic_peak_background_search>`.

    :returns: parameters controlling the automatic detection of peaks and background.
    :rtype: dict (:ref:`peak_search_cond_type <peak_search_cond_type>`)

    .. code-block:: python

        >>> from crysfml import cfml_bckpeaks
        >>> pkb = cfml_bckpeaks.get_pkb_conditions()
        >>> for k in pkb:
        ...   print(f'{k:>14}: {pkb[k]}')
        ...
                fortran_type: peak_search_cond_type
              peak_threshold: 0.019999999552965164
          shoulder_threshold: 2.0
               bkg_threshold: 0.05000000074505806
                 kindofpeaks: 1
                  iterations: 3

