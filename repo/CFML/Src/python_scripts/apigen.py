"""
Python script for building CFML_Wraps.
Author: Nebil A. Katcho
February 2024

---------
Functions
---------
run() -> None
"""

import os
import reader
import wrapper_procs
import wrapper_types
import compilation
try:
    import colorama
    colorama.init()
    is_colorama = True
except:
    is_colorama = False

modules = {}
lucy = {} # Base class for every type

def run() -> None:

    if is_colorama:
        print(f"{' ' :>20}{colorama.Fore.GREEN}{'==========='}{colorama.Style.RESET_ALL}")
        print(f"{' ' :>20}{colorama.Back.GREEN}{'Build Wraps'}{colorama.Style.RESET_ALL}")
        print(f"{' ' :>20}{colorama.Fore.GREEN}{'==========='}{colorama.Style.RESET_ALL}")
    else:
        print(f"{' ' :>20}{'==========='}")
        print(f"{' ' :>20}{'Build Wraps'}")
        print(f"{' ' :>20}{'==========='}")
    if is_colorama:
        print(f"{colorama.Fore.GREEN}{'Reading modules'}{colorama.Style.RESET_ALL}")
    else:
        print(f"{'Reading modules'}")
    reader.read(modules,lucy)
    if is_colorama:
        print(f"{colorama.Fore.GREEN}{'Setting childs'}{colorama.Style.RESET_ALL}")
    else:
        print(f"{'Setting childs'}")
    if is_colorama:
        print(f"{colorama.Fore.GREEN}{'Start building wraps'}{colorama.Style.RESET_ALL}")
    else:
        print(f"{'Building wraps / unwraps of CrysFML08 types'}")
    wrapper_types.wrap(modules,lucy)
    wrapper_procs.wrap(modules,lucy)
    compilation.scripts(modules)

if __name__ == '__main__':

    run()
