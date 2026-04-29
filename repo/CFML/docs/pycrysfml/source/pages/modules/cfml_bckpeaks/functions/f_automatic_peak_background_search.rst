.. py:function:: automatic_peak_background_search(pat,x1,x2,mode)

    Determine automatically background and peak positions.

    :param pat: difrfaction pattern.
    :type pat: dict (:ref:`diffpat_type <diffpat_type>`)
    :param x1: minimum x value considered in the search.
    :type x1: float
    :param x2: maximum x value considered in the search.
    :type x2: float
    :param mode: 'peak' | 'background' | 'satellites'.
    :type mode: str
    :returns: pkb object.
    :rtype: dict (:ref:`pkb_type <pkb_type>`)

    .. code-block:: python

        >>> from crysfml import cfml_diffpatt
        >>> from crysfml import cflm_bckpeaks
        >>> pat = cfml_diffpatt.load_pattern('03-LFP_Discover.dat',mode='xysigma')
        >>> pkb = cfml_bckpeaks.automatic_peak_background_search(pat,10.0,80.0,'peaks')
        >>> for k in pkb:
        ...   print(f'{k:>14}: {pkb[k]}')
        ...
          fortran_type: pkb_type
            np: 40
             x: [17.232496 20.843233 22.771864 24.121595 25.64862  29.804972 32.303997
                 35.684982 36.62791  37.989883 39.41762  39.73446  39.900185 42.32651
                 44.49574  45.07983  49.32125  50.159317 50.38074  50.663807 52.613655
                 52.775547 55.087425 55.587704 56.736046 58.36594  61.60616  61.833393
                 61.970154 62.201885 64.125626 65.97682  66.46389  67.616005 69.544
                 70.26953  71.46118  72.40659  73.62047  74.47849 ]
             y: [ 3061.0933   9004.13     3159.8848   1509.5154  11844.467   11943.143
                  5292.503   16690.324    5522.8657   3406.3835   2583.3354   2731.1934
                  3272.1553   3246.5498    988.1683    942.6104   2250.4302    660.5695
                  2206.0413   1138.9563   5829.2695   2147.3718   2861.9934   3127.062
                  3548.5374   1917.7238    495.45114  3250.463    2342.05     2108.7014
                   668.74304   592.47046  1098.5673    901.63226   610.5516   1873.7783
                   539.43976  1528.5731    564.4623   1124.8981 ]
           bkg: [ 88.33722   95.28555   96.64737   97.34582  116.23704   98.893555
                 126.07866  182.0669   176.21368   90.71814  115.29556  114.05248
                 112.595955  91.82423   82.588554  81.76523  105.54546  102.172386
                 100.60827   98.71069  109.71594  111.741455 120.78247  136.22896
                 139.42548  107.78453   98.2847   100.828156 102.21943  104.198524
                 103.38235  107.31806  109.097916 141.09811   82.37343   81.92348
                 131.85231  152.9516    82.36922  106.29635 ]
