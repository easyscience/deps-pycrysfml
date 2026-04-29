.. py:function:: powder_pattern_from_json(json)

    Computes a diffraction pattern.

    :param json: dictionary containing the structural and experimental parameters required for the pattern calculation. The easiest way to build the dictionary is to read the data from a *json* file. An example of such a *json* file can be found `here <../../../_static/SrTiO3.json>`_. The structural parameters can also be provided with a *cif* file. In this case, the *json* file should look like this `one <../../../_static/icsd_042622.json>`_.
    :type json: dict.
    :returns: a tuple containing the diffraction pattern:

        - first value: 2 theta axis
        - second value: intensity
    :rtype: tuple(ndarray, ndarray)

    .. code-block:: python

        >>> from crysfml import cfml_utilities
        >>> import json
        >>> # Build a dictionary from a json file
        >>> with open('SrTiO3.json') as f:
        ...   d = json.load(f)
        ...
        >>> # Compute powder pattern
        >>> x,y = cfml_utilities.powder_pattern_from_json(d)
        >>> x
        array([  1.     ,   1.05   ,   1.1    , ..., 139.90001, 139.95   ,
               140.     ], dtype=float32)
        >>> y
        array([ 0.      ,  0.      ,  0.      , ..., 20.433508, 19.255768,
        18.1968  ], dtype=float32)
