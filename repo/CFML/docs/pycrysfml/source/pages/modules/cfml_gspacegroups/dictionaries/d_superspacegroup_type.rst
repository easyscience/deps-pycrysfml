.. py:data:: superspacegroup_type

    A superspace group. Extension of :ref:`Shubnikov group <spg_type>`.

    Keys and types:

    - **ep** (ndarray, dim=(:,:,:), int32): modulation vector transform matrices (d,d,multip) in integer form to accelerate calculations.
    - **kv** (ndarray, dim=(3,nk), float32): k-vectors.
    - **kv_std** (ndarray, dim=(3,nk), float32): k-vectors standard deviations.
    - **m** (ndarray, dim=(:,:,:), int32): reciprocal operator matrices (3,3,multip) in integer form to accelerate calculations.
    - **nharm** (ndarray, dim=(nk), int32): number of harmonics along each k-vector (nk).
    - **nk** (integer): number of k-vectors.
    - **nq** (integer): number of effective set of q_coeff >= nk.
    - **q_coeff** (ndarray, dim=(nk,nq), int32): q-coefficients, coefficients of q-vectors in the k-basis.
    - **rot** (ndarray, dim=(:,:,:), int32): rotational operator matrices (3,3,multip) in integer form to accelerate calculations.
    - **sintlim** (ndarray, dim=(nk), float32): sintheta/lambda limits (nk).
    - **ssg_bravais** (str): symbol of the superspace Bravais class.
    - **ssg_nlabel** (str): label of the superspace  Bravais class (Stokes & Campbell database).
    - **ssg_symb** (str): symbol of the superspace group.
    - **t** (ndarray, dim=(:,:), int32): translation in external (physical) space (3,multip).
    - **ti** (ndarray, dim=(:,:), int32): translation in internal space (3,multip).

