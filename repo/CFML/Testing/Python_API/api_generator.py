"""
Python script for generating an API of the CrysFML08 library.
Author: Nebil A. Katcho
November 2022

---------
Functions
---------
check_reading() -> None
fortran_types_to_dicts() -> None
get_cfml_modules_filenames() -> list
get_overloads(m_name : str,lines : list,n : int =0) -> int
get_procedures(m_name : str,lines : list,n : int =0) -> int
get_publics(m_name : str,lines : list,n : int =0) -> int)
get_types(m_name : str,lines : list,n : int =0) -> int
is_overload(m_name : str,p_name : str) -> str
move_to_install() -> None
move_to_source() -> None
read_cfml_module(file_name : str) -> None
read() -> None
run() -> None
wrap_procedures() -> None
"""

import colorama
import compilation
import cfml_objects
import glob
import os
import parser_utils
import wraper_utils

colorama.init()

modules = {}
is_read = False
CRYSFML08 = ''
CWD = os.getcwd()

def check_reading() -> None:

    if not is_read:
        print(f"{colorama.Fore.RED}{'Error: CrysFML08 library must be read before writing the API.'}{colorama.Style.RESET_ALL}")
        raise IOError

def fortran_types_to_dicts() -> None:

    check_reading()
    print('')
    move_to_install()
    print(f"{colorama.Fore.GREEN}{'Converting Fortran types in Python dictionaries'}{colorama.Style.RESET_ALL}")
    for m_name in modules.keys():
        print(f"{' ':>4}{colorama.Fore.GREEN}{'Module name: '}{colorama.Fore.CYAN}{m_name}{colorama.Style.RESET_ALL}")
        if len(modules[m_name].types.keys()) == 0:
            continue
        with open('API_'+m_name+'.py','w') as f:
            for t_name in modules[m_name].types.keys():
                print(f"{' ':>4}{colorama.Fore.GREEN}{'Converting '}{colorama.Fore.YELLOW}{'type' : <11}{colorama.Fore.CYAN}{t_name}{colorama.Style.RESET_ALL}")
                f.write(f"{'def create_'}{t_name}{'():'}\n")
                f.write('\n')
                t = modules[m_name].types[t_name]
                if t.parent:
                    f.write(f"{' ':>4}{'d = create_'}{t.parent}{'()'}\n")
                else:
                    f.write(f"{' ':>4}{'d = '}{'{}'}\n")
                f.write('\n')
                for c in t.components.keys():
                    left = "d['"+c+"']"
                    if t.components[c].value:
                        right = '= '+t.components[c].value
                    else:
                        right = '= None'
                    f.write(f"{' ':>4}{left:<35}{right}\n")
                for c in t.components.keys():
                    left = "d['ftype']['"+c+"']"
                    var_def = parser_utils.build_var_def(t.components[c])
                    right = "= '"+var_def+"'"
                    f.write(f"{' ':>4}{left:<35}{right}\n")
                f.write('\n')
                f.write(f"{' ':>4}{'return d'}\n")
                f.write('\n')

def get_argument_types() -> None:
    """
    Function for testing, it writes all the different primitive types used as arguments
    for crysfml subroutines and functions
    """
    d = {'integer':{'in':[],'inout':[],'out':[]},'real':{'in':[],'inout':[],'out':[]},'character':{'in':[],'inout':[],'out':[]},'logical':{'in':[],'inout':[],'out':[]}}
    info = {'integer':{'in':[],'inout':[],'out':[]},'real':{'in':[],'inout':[],'out':[]},'character':{'in':[],'inout':[],'out':[]},'logical':{'in':[],'inout':[],'out':[]}}
    for m_name in modules.keys():
        for p_name in modules[m_name].procedures.keys():
            for arg in modules[m_name].procedures[p_name].arguments.keys():
                ftype = modules[m_name].procedures[p_name].arguments[arg].ftype
                if parser_utils.is_primitive(ftype):
                    intent = parser_utils.get_intent(ftype)
                    if ftype.startswith('integer'):
                        if ftype not in d['integer'][intent]:
                            d['integer'][intent].append(ftype)
                            info['integer'][intent].append([p_name,m_name])
                    elif ftype.startswith('real'):
                        if ftype not in d['real'][intent]:
                            d['real'][intent].append(ftype)
                            info['real'][intent].append([p_name,m_name])
                    elif ftype.startswith('character'):
                        if ftype not in d['character'][intent]:
                            d['character'][intent].append(ftype)
                            info['character'][intent].append([p_name,m_name])
                    elif ftype.startswith('logical'):
                        if ftype not in d['logical'][intent]:
                            d['logical'][intent].append(ftype)
                            info['logical'][intent].append([p_name,m_name])
    os.chdir(CWD)
    with open('types.txt', 'w') as f:
        for k1 in d.keys():
            f.write(f"{k1.upper()}:\n")
            for k2 in d[k1].keys():
                f.write(f"{' ':>2}{k2.upper()}:\n")
                for t,i in zip(d[k1][k2],info[k1][k2]):
                    f.write(f"{' ':>4}{t:<60}{i[0]:<30}{i[1]:<30}\n")

def get_cfml_modules_filenames() -> list:

    cfml_modules_names = glob.glob('CFML*.f90')
    if len(cfml_modules_names) == 0:
        print(f"{colorama.Fore.RED}{'Error: No Fortran modules found. There is nothing to do. Bye bye.'}{colorama.Style.RESET_ALL}")
        raise IOError
    return cfml_modules_names

def get_overloads(m_name : str,lines : list,n : int =0) -> int:

    while n < len(lines):

        line = lines[n].lower()
        if line.startswith('contains'):
            return n-1
        if not line.strip().startswith('interface'):
            n += 1
            continue
        n,line = parser_utils.get_line(n,lines)
        i_name = parser_utils.get_interface_name(line)
        if i_name == '': # Starting the interface zone
            return n-1
        print(f"{' ':>4}{colorama.Fore.GREEN}{'Parsing '}{colorama.Fore.YELLOW}{'interface' : <11}{colorama.Fore.CYAN}{i_name}{colorama.Style.RESET_ALL}")
        modules[m_name].interface[i_name] = cfml_objects.Interface(i_name)
        n = parser_utils.get_overload_procedures(n+1,lines,modules[m_name].interface[i_name])
        n += 1
    return n

def get_procedures(m_name : str,lines : list,n : int =0) -> int:

    while n < len(lines):

        n,line = parser_utils.get_line(n,lines)
        if parser_utils.is_procedure('function',line):
            f_name = parser_utils.get_function_name(line)
            if f_name in modules[m_name].publics:
                print(f"{' ':>4}{colorama.Fore.GREEN}{'Parsing '}{colorama.Fore.YELLOW}{'function' : <11}{colorama.Fore.CYAN}{f_name}{colorama.Style.RESET_ALL}")
                modules[m_name].procedures[f_name] = cfml_objects.Function(f_name)
                parser_utils.get_arguments(line,modules[m_name].procedures[f_name])
                parser_utils.get_function_result(line,modules[m_name].procedures[f_name])
                ov = is_overload(m_name,f_name)
                if ov:
                    modules[m_name].procedures[f_name].is_overload = True
                    modules[m_name].procedures[f_name].overload = ov
                n = parser_utils.get_function_types(n+1,lines,modules[m_name].procedures[f_name])
            else:
                n += 1
        elif parser_utils.is_procedure('subroutine',line):
            s_name = parser_utils.get_subroutine_name(line)
            if s_name in modules[m_name].publics:
                print(f"{' ':>4}{colorama.Fore.GREEN}{'Parsing '}{colorama.Fore.YELLOW}{'subroutine' : <11}{colorama.Fore.CYAN}{s_name}{colorama.Style.RESET_ALL}")
                modules[m_name].procedures[s_name] = cfml_objects.Subroutine(s_name)
                parser_utils.get_arguments(line,modules[m_name].procedures[s_name])
                ov = is_overload(m_name,s_name)
                if ov:
                    modules[m_name].procedures[s_name].is_overload = True
                    modules[m_name].procedures[s_name].overload = ov
                n = parser_utils.get_subroutine_types(n+1,lines,modules[m_name].procedures[s_name])
            else:
                n += 1
        else:
            n += 1
    return n

def get_publics(m_name : str,lines : list,n : int =0) -> int:
    """
    Get the public methods of the module
    """
    while n < len(lines):
        line = lines[n].lower().strip()
        if line.startswith('type') or line.startswith('interface') or \
            line.startswith('contains') or line.startswith('end'):
            break
        if line.strip().startswith('public'):
            n,line = parser_utils.get_line(n,lines)
            line = line.lower().strip()
            i = line.find('::')
            for p in line[i+2:].split(','):
                modules[m_name].publics.append(p.strip())
        n += 1
    return n-1

def get_types(m_name : str,lines : list,n : int =0) -> int:

    while n < len(lines):
        line = lines[n].lower().strip()
        if line.startswith('interface') or line.startswith('contains'):
            return n-1
        if not line.startswith('type'):
            n += 1
            continue
        if line[4:].strip().startswith('('):
            n += 1
            continue
        if line.find('public') < 0:
            n += 1
            continue
        n,line = parser_utils.get_line(n,lines)
        t_name = parser_utils.get_type_name(line)
        p_name = parser_utils.get_type_parent(line)
        print(f"{' ':>4}{colorama.Fore.GREEN}{'Parsing '}{colorama.Fore.YELLOW}{'type' : <11}{colorama.Fore.CYAN}{t_name}{colorama.Style.RESET_ALL}")
        modules[m_name].types[t_name] = cfml_objects.FortranType(name=t_name,parent=p_name)
        n = parser_utils.get_type_components(n+1,lines,modules[m_name].types[t_name])
    return n

def is_overload(m_name : str,p_name : str) -> str:

    ov = ''
    for iface in modules[m_name].interface.keys():
        for p in modules[m_name].interface[iface].procedures:
            if p.strip() == p_name.strip():
                return iface
    return ov

def move_to_install(fortran=False) -> None:

    # Create install directory if it doesn't exist
    os.chdir(CRYSFML08)
    if not os.path.isdir('API'):
        os.makedirs('API')
    if not os.path.isdir('API/Src'):
        os.makedirs('API/Src')
    if not os.path.isdir('API/Src/Extensions'):
        os.makedirs('API/Src/Extensions')
    
    if not fortran:
        wdir = os.path.join(CRYSFML08,'API','Src')
    else:
        wdir = os.path.join(CRYSFML08,'API','Src','Extensions')
    print(f"{colorama.Fore.GREEN}{'Entering in API directory: '}{colorama.Fore.YELLOW}{wdir}{colorama.Style.RESET_ALL}")
    os.chdir(wdir)
    return None

def move_to_source() -> None:

    # Move to Crysfml08
    global CRYSFML08
    CRYSFML08 = os.getenv('CRYSFML08')
    if CRYSFML08 is None:
        print(f"{colorama.Fore.RED}{'Error: environment variable CRYSFML08 does not exist.'}{colorama.Style.RESET_ALL}")
        raise IOError
    if not os.path.isdir(CRYSFML08):
        print(f"{colorama.Fore.RED}{'Error: '}{colorama.Fore.YELLOW}{CRYSFML08}{colorama.Fore.RED}{' does not exist'}{colorama.Style.RESET_ALL}")
        raise IOError
    print(f"{colorama.Fore.GREEN}{'Entering in CrysFML08 directory: '}{colorama.Fore.YELLOW}{CRYSFML08}{colorama.Style.RESET_ALL}")
    os.chdir(CRYSFML08)

    # Move to Src\
    if not os.path.isdir('Src'):
        print(f"{colorama.Fore.RED}{'Error: Src directory not found in '}{CRYSFML08}{colorama.Style.RESET_ALL}")
        raise IOError
    print(f"{colorama.Fore.GREEN}{'Entering '}{colorama.Fore.YELLOW}{'Src'}{colorama.Fore.GREEN}{' directory'}{colorama.Style.RESET_ALL}")
    os.chdir('Src')
    return None

def read_cfml_module(file_name : str) -> None:

    print('')
    print(f"{colorama.Fore.GREEN}{'Reading file '}{colorama.Fore.CYAN}{file_name}{colorama.Style.RESET_ALL}")
    with open(file_name,'r') as f:
        lines = f.readlines()

    # Get module name
    try:
        m_name = parser_utils.get_module_name(lines)
    except Exception as e:
        print(f"{colorama.Fore.RED}{'Error: '}{e}{colorama.Style.RESET_ALL}")
        raise IOError
    modules[m_name] = cfml_objects.Module(name=m_name)
    print(f"{' ':>4}{colorama.Fore.GREEN}{'Module name: '}{colorama.Fore.CYAN}{m_name}{colorama.Style.RESET_ALL}")

    # Get public methods of the module
    n = get_publics(m_name,lines)

    # Get types
    n = get_types(m_name,lines,n)

    # Get overloads
    n = get_overloads(m_name,lines,n)

    # Get procedures
    n = get_procedures(m_name,lines,n)

def read() -> None:

    global is_read
    move_to_source()
    cfml_modules_fnames = get_cfml_modules_filenames()
    for file_name in cfml_modules_fnames:
        if file_name.lower().find('strings') > -1 or \
           file_name.lower().find('messages') > -1 or \
           file_name.lower().find('keycodes') > -1:
            continue
        read_cfml_module(file_name)
        #break
    is_read = True
    return None

def run() -> None:

    print(f"{' ' :>20}{colorama.Fore.GREEN}{'==========================='}{colorama.Style.RESET_ALL}")
    print(f"{' ' :>20}{colorama.Back.GREEN}{'API Generator for CRYSFML08'}{colorama.Style.RESET_ALL}")
    print(f"{' ' :>20}{colorama.Fore.GREEN}{'==========================='}{colorama.Style.RESET_ALL}")

    read()
    fortran_types_to_dicts()
    wrap_procedures()
    compilation.create_scripts(os.path.join(CRYSFML08,'API'))

    return None

def wrap_procedures() -> None:

    nprocs = 0
    procs = {}
    check_reading()
    move_to_install(fortran=True)
    print(f"{colorama.Fore.GREEN}{'Wrapping Fortran procedures'}{colorama.Style.RESET_ALL}")
    for m_name in modules.keys():
        nwraps = 0
        print(f"{' ':>4}{colorama.Fore.GREEN}{'Module name: '}{colorama.Fore.CYAN}{m_name}{colorama.Style.RESET_ALL}")
        if len(modules[m_name].procedures.keys()) == 0:
            continue
        for p_name in modules[m_name].procedures.keys():
            wrappea = True
            for arg in modules[m_name].procedures[p_name].arguments.keys():
                if not parser_utils.is_primitive(modules[m_name].procedures[p_name].arguments[arg].ftype) or \
                    parser_utils.is_optional(modules[m_name].procedures[p_name].arguments[arg].ftype) or \
                    modules[m_name].procedures[p_name].is_overload or \
                    modules[m_name].procedures[p_name].arguments[arg].ndim > 0:
                    wrappea = False
                    break
            if wrappea and type(modules[m_name].procedures[p_name]) == cfml_objects.Function:
                if not parser_utils.is_primitive(modules[m_name].procedures[p_name].xreturn.ftype) or \
                    parser_utils.is_optional(modules[m_name].procedures[p_name].xreturn.ftype) or \
                    modules[m_name].procedures[p_name].is_overload or \
                    modules[m_name].procedures[p_name].xreturn.ndim > 0:
                    wrappea = False
            if wrappea:
                if type(modules[m_name].procedures[p_name]) == cfml_objects.Function:
                    print(f"{' ':>4}{colorama.Fore.GREEN}{'Wrapping '}{colorama.Fore.YELLOW}{'function' :<12}{colorama.Fore.CYAN}{p_name}{colorama.Style.RESET_ALL}")
                else:
                    print(f"{' ':>4}{colorama.Fore.GREEN}{'Wrapping '}{colorama.Fore.YELLOW}{'subroutine':<12}{colorama.Fore.CYAN}{p_name}{colorama.Style.RESET_ALL}")
                if nwraps == 0:
                    wraper_utils.init_module(modules[m_name])
                    nwraps = 1
                wraper_utils.wrap_procedure(modules[m_name].procedures[p_name])
                if m_name in procs.keys():
                    procs[m_name].append(p_name)
                else:
                    procs[m_name] = [p_name]
                nprocs += 1
        if nwraps > 0:
            wraper_utils.end_module(modules[m_name])
    print(f"{colorama.Fore.GREEN}{'Writing interconversion module'}{colorama.Style.RESET_ALL}")
    wraper_utils.write_interconversion()
    print(f"{colorama.Fore.GREEN}{'Writing API_init'}{colorama.Style.RESET_ALL}")
    wraper_utils.write_api_init(procs,nprocs)

#if __name__ == '__main__':

run()
