import re

import numpy as np
import pytest
from numpy.testing import assert_allclose

from crysfml import cfml_py_utilities


CP_ATOL = 1e-5


def _pattern_block(index: int, xmin: float = 30.0, step: float = 0.01, xmax: float = 33.0) -> str:
    return f"""
PATTERN_NeutronCW_3t2_{index}  {index}
  Patt_Type   Neutrons    Powder     CW
  Zero_Sy  0.00  0.00000    0.0000
  Profile_function  TCH_pVoigt
  WDT   14.0
  ASYM    0.000000   0.000000
  LAMBDA  1.494   1.494      0.0
  UVWXY   0.0000   0.000   0.026173   0.00000   0.00
  Gen_patt {xmin:.8f}  {step:.8f} {xmax:.8f}
  Name_Patt  SrTiO3_simple_{index}.xys
END_PATTERN_NeutronCW_3t2_{index}
"""


def _phase_block(pattern_count: int) -> str:
    pattern_ids = "  ".join(str(i) for i in range(1, pattern_count + 1))
    scale_factors = "  ".join("0.0197" for _ in range(pattern_count))
    phase_patterns = "\n".join(
        f"""
  PH_Pattern {i}
     Iso_SIZE         950                  0.11
     Iso_STRAIN       0.2                  0.23
  END_PH_Pattern
"""
        for i in range(1, pattern_count + 1)
    )
    return f"""
PHASE_SrTiO3   1
  Cell    3.88        3.88      3.88     90.000000  90.000000 90.000000
  SPGR  P m -3 m
  Atom Ti    Ti       0.00000  0.00000  0.00000  0.23226   1.00000
  Atom Sr    Sr       0.50000  0.50000  0.50000  0.32967   1.00000
  Atom O      O       0.50000  0.00000  0.00000  0.40639   3.00000
  Contributes_to_patterns  {pattern_ids}
  Scale_Factors  {scale_factors}
{phase_patterns}
END_PHASE_SrTiO3
"""


def _cfl_lines(pattern_count: int = 1) -> list[str]:
    cfl = "Title  SrTiO3 simulation\n"
    cfl += "\n".join(_pattern_block(i) for i in range(1, pattern_count + 1))
    cfl += _phase_block(pattern_count)
    cfl += "\nCALCULATION_TYPE  simulation\n"
    return cfl.strip().splitlines()


def _regular_axis() -> np.ndarray:
    return np.linspace(30.0, 33.0, 301)


def _irregular_axis() -> np.ndarray:
    x = _regular_axis()
    return x + 0.002 * np.sin(np.linspace(0.0, 4.0 * np.pi, len(x)))


def test__patterns_simulation__backward_compatible_regular_axis():
    patterns = cfml_py_utilities.patterns_simulation(_cfl_lines())

    assert len(patterns) == 1
    assert patterns[0]["npts"] == len(_regular_axis())
    assert_allclose(patterns[0]["x"], _regular_axis(), atol=CP_ATOL, rtol=0)
    assert len(patterns[0]["y"]) == len(_regular_axis())


def test__patterns_simulation__uses_explicit_irregular_axis():
    x = _irregular_axis()

    patterns = cfml_py_utilities.patterns_simulation(_cfl_lines(), x)

    assert patterns[0]["npts"] == len(x)
    assert_allclose(patterns[0]["x"], x, atol=CP_ATOL, rtol=0)
    assert len(patterns[0]["y"]) == len(x)


def test__patterns_simulation__ordinates_follow_irregular_coordinates_near_peak():
    cfl_lines = _cfl_lines()
    regular = cfml_py_utilities.patterns_simulation(cfl_lines)[0]
    x = np.asarray(regular["x"], dtype=float)
    y = np.asarray(regular["y"], dtype=float)

    peak_index = int(np.argmax(y))
    gradients = np.diff(y)
    flank_start = max(0, peak_index - 80)
    flank = np.arange(flank_start, peak_index)
    assert len(flank) > 0
    flank_index = int(flank[np.argmax(gradients[flank])])

    shifted_x = x.copy()
    shifted_x[flank_index] += 0.4 * (x[flank_index + 1] - x[flank_index])

    shifted = cfml_py_utilities.patterns_simulation(cfl_lines, shifted_x)[0]
    shifted_y = np.asarray(shifted["y"], dtype=float)

    assert shifted_y[flank_index] > y[flank_index]
    assert not np.allclose(shifted_y, y)


@pytest.mark.parametrize(
    ("x", "message"),
    [
        (np.asarray([30.0]), "at least two points"),
        (np.asarray([30.0, 30.01, 30.01, 30.03]), "strictly increasing"),
        (_regular_axis()[:-1], "same number of points as GEN_PATT"),
    ],
)
def test__patterns_simulation__validates_explicit_axis(x, message):
    with pytest.raises(RuntimeError, match=re.escape(message)):
        cfml_py_utilities.patterns_simulation(_cfl_lines(), x)


def test__patterns_simulation__rejects_explicit_axis_for_multiple_patterns():
    with pytest.raises(RuntimeError, match="requires exactly one pattern"):
        cfml_py_utilities.patterns_simulation(_cfl_lines(pattern_count=2), _regular_axis())
