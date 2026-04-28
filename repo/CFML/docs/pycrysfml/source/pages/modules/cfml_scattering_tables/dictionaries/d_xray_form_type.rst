.. py:data:: xray_form_type

    X-ray scattering factors are computed as:

        f(s) = Sum_{i=1,4} (a(i) exp(-b(i)*s^2)) + c,

    where s is sin(theta) / lambda.

    Keys and types:

    - **symb** (str): chemical symbol.
    - **z** (int): atomic number.
    - **a** (ndarray, dim=(4), float32): coefficient.
    - **b** (ndarray, dim=(4), float32): coefficient.
    - **c** (float32): coefficient.