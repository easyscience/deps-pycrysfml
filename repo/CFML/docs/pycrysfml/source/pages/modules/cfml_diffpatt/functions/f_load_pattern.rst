.. py:function:: load_pattern(filename,pat,**kwargs)

    Load a diffraction pattern from a file.

    :param filename: full path of the file.
    :type filename: str
    :param kwargs: optional arguments.
    :param kwargs['mode']: format specifier: 'CIF' | 'DMC' |'D1A' | 'D1AOLD' | 'D1B' | 'D2B' | 'D2BOLD' | 'D20' | 'GSAS' | 'GSASTOF' | 'G41' | 'G42' | 'HRPT' | 'PANALYTICAL' | 'NLS' | 'SOCABIM' | 'TIMEVARIABLE' | 'XYSIGMA' | '3T2'.
    :type kwargs['mode']: str
    :param kwargs['sig']: if False, sigma of pattern will be the standard deviation. Otherwise, sigma of pattern will be the variance.
    :type kwargs['sig']: bool
    :param kwargs['header']: a header for the diffraction pattern
    :type kwargs['header']: str
    :returns: diffraction pattern.
    :rtype: dict (:ref:`diffpat_type <diffpat_type>`)

    .. code-block:: python

        >>> from pycrysfml import cfml_diffpatt
        >>> pat = cfml_diffpatt.load_pattern("pbso4.dat",mode="d1a")
        >>> for k in pat:
        ...   print(f'{k :>16}: {pat[k]}')
        ...
                   title:  PbSO4 D1A(ILL)(Rietveld Refinement Round Robin, R.J. Hill, JApC 25, 589 (1992)
                 kindrad:
                 scatvar:
                    xmin: 10.0
                    xmax: 155.4499969482422
                    ymin: 176.0
                    ymax: 2459.0
                    step: 0.0
                    npts: 2910
                  sigvar: True
                    wave: [0. 0. 0. 0. 0.]
                       x: [10.   10.05 10.1  ...  0.    0.    0.  ]
                       y: [220. 214. 219. ...   0.   0.   0.]
                   sigma: [220. 214. 219. ...   0.   0.   0.]
                   instr: D1A
                filename:
                filepath:
                    scal: 1.0
                 monitor: 10000.0
                norm_mon: 0.0
                col_time: 0.0
                 tsample: 298.0
                    tset: 0.0
                   ycalc: [0. 0. 0. ... 0. 0. 0.]
                     bgr: [0. 0. 0. ... 0. 0. 0.]
