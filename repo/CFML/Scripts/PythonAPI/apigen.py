"""
Python script for building CFML_Wraps.
Author: Nebil A. Katcho
February 2024

---------
Functions
---------
run()
"""
import argparse
import compilation
import os
import parser_utils
import reader
import sys
import wrapper_procs
import wrapper_types
try:
    import colorama
    colorama.init()
    is_colorama = True
except:
    is_colorama = False

modules = {}
lucy = {}    # Base class for every Fortran type of CrysFML08
classes = [] # CrysFML08 types that can be used as class

def str2bool(v):
    if isinstance(v, bool):
        return v
    if v.lower() in ('yes', 'true', 't', 'y', '1'):
        return True
    elif v.lower() in ('no', 'false', 'f', 'n', '0'):
        return False
    else:
        raise argparse.ArgumentTypeError('Boolean value expected.')

def run(build_dir : str,verbose : bool,scripts : bool,cmake : bool):
    """
    Run the generation of the python API for CrysFML08

    Parameters
    ----------
    build_dir : str
                directory where building the API
    verbose   : bool
                if True, print messages during the API generation
    scripts   : bool
                create compilation scripts
    cmake     : bool
                create file CMakeLists.txt
    """    
    parser_utils.VERBOSE = verbose
    reader.VERBOSE = verbose
    if verbose:
        if is_colorama:
            print(f"{' ' :>20}{colorama.Fore.GREEN}==========="\
                f"{colorama.Style.RESET_ALL}")
            print(f"{' ' :>20}{colorama.Back.GREEN}Build Wraps"\
                f"{colorama.Style.RESET_ALL}")
            print(f"{' ' :>20}{colorama.Fore.GREEN}==========="\
                f"{colorama.Style.RESET_ALL}")
        else:
            print(f"{' ' :>20}{'==========='}")
            print(f"{' ' :>20}{'Build Wraps'}")
            print(f"{' ' :>20}{'==========='}")
        if is_colorama:
            print(f"{colorama.Fore.GREEN}{'Reading modules'}"\
                f"{colorama.Style.RESET_ALL}")
        else:
            print(f"{'Reading modules'}")
    reader.read(modules,lucy,classes)
    if verbose:
        if is_colorama:
            print(f"{colorama.Fore.GREEN}{'Setting childs'}"\
                f"{colorama.Style.RESET_ALL}")
        else:
            print(f"{'Setting childs'}")
        if is_colorama:
            print(f"{colorama.Fore.GREEN}{'Start building wraps'}"\
                f"{colorama.Style.RESET_ALL}")
        else:
            print(f"{'Building wraps / unwraps of CrysFML08 types'}")
    wrapper_types.wrap(build_dir,modules,lucy,classes)
    wrapper_procs.wrap(build_dir,modules,lucy,classes)
    if scripts:
        compilation.scripts(build_dir,wrapper_types.wrap_files,modules)
    if cmake:
        compilation.cmake(build_dir,wrapper_types.wrap_files,modules)

if __name__ == '__main__':

    parser = argparse.ArgumentParser(description='Script for building '+
        'the Python API of CrysFML08')
    parser.add_argument('--build',type=str,help='build directory',default='')
    parser.add_argument('--verbose',type=str2bool,help='verbose',default=True)
    parser.add_argument('--scripts',type=str2bool,help='create compilation scripts',default=False)
    parser.add_argument('--cmake',type=str2bool,help='create CMakeLists.txt',default=False)
    args = parser.parse_args()
    if args.build:
        build_dir = os.path.abspath(args.build)
    else:
        build_dir = os.path.abspath('../../build')
    if not os.path.isdir(build_dir):
        os.mkdir(build_dir)
    run(build_dir,args.verbose,args.scripts,args.cmake)
