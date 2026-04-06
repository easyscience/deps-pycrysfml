import json
import inspect
import os
import platform
import numpy as np
import re
from io import StringIO

def set_crysfml_db():
    os.environ['CRYSFML_DB'] = os.path.join(os.getcwd(), 'repo', 'CFML', 'Src', 'Databases')

def path_to_progs(file_name:str=''):
    abspath = os.path.join(os.getcwd(), 'dist', 'CFML', 'progs', file_name)
    relpath = os.path.relpath(abspath)
    return relpath

def path_to_actual(file_name:str=''):
    caller = inspect.stack()[1].filename
    abspath = os.path.join(os.path.dirname(caller), 'actual', file_name)
    relpath = os.path.relpath(abspath)
    return relpath

def path_to_desired(file_name:str=''):
    caller = inspect.stack()[1].filename
    abspath = os.path.join(os.path.dirname(caller), 'desired', file_name)
    relpath = os.path.relpath(abspath)
    return relpath

def path_to_input(file_name:str=''):
    caller = inspect.stack()[1].filename
    abspath = os.path.join(os.path.dirname(caller), 'input', file_name)
    relpath = os.path.relpath(abspath)
    return relpath

def _run_exe_with_args(file_name:str,
                      args:str=''):
    """Runs the executable with optional arguments."""
    if platform.system() == 'Windows':
        file_name = f'{file_name}.exe'
    exe_path = os.path.join(os.getcwd(), file_name)
    cmd = f'{exe_path} {args}'
    os.system(cmd)

def run_exe_with_args(exe_path:str,
                      args:str=''):
    """Runs the executable with optional arguments."""
    exe_path = os.path.abspath(exe_path)
    if platform.system() == 'Windows':
        exe_path = f'{exe_path}.exe'
    args_list = args.split(' ')
    original_cwd = os.getcwd()
    new_cwd = os.path.dirname(args_list[0])
    args_list[0] = os.path.basename(args_list[0])
    args = ' '.join(args_list)
    cmd = f'{exe_path} {args}'
    os.chdir(new_cwd)
    os.system(cmd)
    os.chdir(original_cwd)

def dat_to_ndarray(file_name:str,
                   skip_begin:int=0,
                   skip_end:int=0,
                   usecols=(0)):
    """Parses the file to extract an array of data and converts it to a numpy array."""
    with open(file_name, 'r') as file:
        lines = file.readlines()  # reads into list
    del lines[:skip_begin]  # deletes requested number of first lines
    del lines[len(lines)-skip_end:]  # deletes requested number of last lines
    lines = [l.replace('(',' ').replace(')', ' ') for l in lines]  # replace brackets with spaces
    joined = '\n'.join(lines)  # joins into single string
    data = np.genfromtxt(StringIO(joined), usecols=usecols, unpack=True)  # converts string to ndarray
    return data

def sub_to_ndarray(file_name:str):
    """Parses the FullProf .SUB file to extract an array of data and converts it to a numpy array."""
    with open(file_name, 'r') as file:
        lines = file.readlines()  # reads into list
    # Extracts the first three numbers from the first line
    numbers = re.findall(r'\d+\.\d+|\d+', lines[0])[:3]
    min, inc, max = list(map(float, numbers))
    x_array = np.arange(start=min, stop=max+inc-(1e-5), step=inc)  # 1e-5 is to avoid extra point at the end
    # Extracts the data from the rest of the file
    skip_begin = 1
    del lines[:skip_begin]  # deletes requested number of first lines
    joined = ''.join(lines)  # joins into single string
    joined = joined.replace('\n', ' ')  # replaces newlines with spaces
    y_array = np.genfromtxt(StringIO(joined))  # converts string to ndarray
    return x_array, y_array

def load_from_json(file_name:str):
    """Loads a JSON file."""
    with open(file_name, 'r') as file:
        return json.load(file)

def chi_squared(calc:np.ndarray, meas:np.ndarray, skip_last:int=0):
    """Calculates the chi-squared value between two arrays."""
    if skip_last:
        calc = calc[:-skip_last]
        meas = meas[:-skip_last]
    return np.sum((meas - calc)**2)
