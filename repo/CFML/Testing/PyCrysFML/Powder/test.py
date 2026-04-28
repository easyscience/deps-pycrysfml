import json
import traceback
from pycrysfml import cfml_py_utilities

json_file='Al2O3_cw.json'

if __name__ == '__main__':

    print(f'Reading {json_file}')
    with open(json_file) as f:
        json_dict = json.load(f)
    try:
        print(f'Computing pattern...')
        x,y = cfml_py_utilities.cw_powder_pattern_from_dict(json_dict)
    except Exception as e:
        traceback.print_exc()
    print(f'Writting pattern...')
    with open('Al2O3_cw.dat','w') as f:
        for i in range(len(x)):
            f.write(f'{x[i] : <.4f} {y[i] : <.8f}\n')
