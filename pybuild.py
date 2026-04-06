import argparse
import datetime
import os
import platform
import site
import sys
import sysconfig
import toml
#import tomllib
from colorama import Fore, Back, Style
from pathlib import Path
from pygit2 import Repository

global ARGS
global CONFIG

MSG_COLOR = r'\033[0;33m'  # orange
ERROR_COLOR = r'\033[0;31m'  # red
HEAD_COLOR = r'\033[1;34m'  # bold blue
COLOR_OFF = r'\033[0m'


def _github_actions():
    if 'GITHUB_ACTIONS' in os.environ:
        return True
    return False

def _github_branch():
    return Repository('.').head.shorthand

def _project_dir():
    return os.path.dirname(__file__)

def _project_path():
    return os.path.abspath(_project_dir())

def _config_path(name: str):
    path = os.path.join(_project_dir(), name)
    path = os.path.abspath(path)
    return path

def _scripts_path():
    dir = CONFIG['project']['dir']['scripts']
    path = os.path.abspath(dir)
    return path

def _main_script_path():
    name = 'main.sh'
    path = os.path.join(_scripts_path(), name)
    return path

def _echo_cmd():
    if _enable_backslash_escapes():
        return 'echo -e'
    return 'echo'

def _echo_msg(msg:str):
    return f'{_echo_cmd()} "{MSG_COLOR}:::::: {msg}{COLOR_OFF}"'

def _echo_progress_msg(current: int, total: int, msg: str):
    progress = _compiling_progress(current, total)
    msg = f"[{progress:>3}%] {msg}"
    return _echo_msg(msg)

def _echo_header(msg:str):
    msg = f'{HEAD_COLOR}:::::: {msg} ::::::{COLOR_OFF}'
    sep = ':' * (len(msg) - len(f'{HEAD_COLOR}') - len(f'{COLOR_OFF}'))
    lines = []
    lines.append(f'{_echo_cmd()} ""')
    lines.append(f'{_echo_cmd()} "{HEAD_COLOR}{sep}{COLOR_OFF}"')
    lines.append(f'{_echo_cmd()} "{HEAD_COLOR}{msg}{COLOR_OFF}"')
    lines.append(f'{_echo_cmd()} "{HEAD_COLOR}{sep}{COLOR_OFF}"')
    return lines

def _print_msg(msg:str):
    print(Fore.GREEN + f':::::: {msg}' + Style.RESET_ALL)

def _print_error_msg(msg:str):
    print(Fore.RED + f':::::: ERROR: {msg}' + Style.RESET_ALL)

def _processor():
    processor = platform.processor()
    processor = processor.split()[0]  # get the 1st word from string, such as 'Intel64 Family 6 Model 154 Stepping 3, GenuineIntel'
    return processor

def _platform():
    platform = 'macos'  # default
    if ARGS.platform:
        platform = ARGS.platform.lower()
    return platform

def _platform_tag():
    tag = sysconfig.get_platform()
    if _platform() == 'macos':  # replaces, e.g., 'macosx-14-arm64' with 'macosx-14_0-arm64'
        tag = tag.split('-')    # 'macosx-14-arm64' => ['macosx', '14', 'arm64']
        tag[1] = f'{tag[1]}_0'  # ['macosx', '14', 'arm64'] => ['macosx', '14_0', 'arm64']
        tag = '-'.join(tag)     # ['macosx', '14_0', 'arm64'] => 'macosx-14_0-arm64'
    tag = tag.replace('-', '_')
    tag = tag.replace('.', '_')
    return tag

def _macos_deployment_target():
    """Return the current macOS major version as 'X.0' (e.g. '26.0').

    This is used to set MACOSX_DEPLOYMENT_TARGET in generated scripts so that
    gfortran (which may have a stale built-in default, e.g. '16.0') and clang
    agree on the deployment target and the
      clang: warning: overriding deployment version from 'X.0' to 'Y.0'
    warning is silenced.
    """
    ver = platform.mac_ver()[0]  # e.g. '26.2'
    major = ver.split('.')[0]    # '26'
    return f'{major}.0'          # '26.0'

def _platform_tag_github_ci():  # sysconf always returns 'macosx_10_9_universal2' on GH macOS...
    tag = sysconfig.get_platform()
    if _platform() == 'macos':
        version = platform.mac_ver()[0]  # e.g., '14.5'
        version = version.split('.')  # '14.5' => ['14', '5']
        version[1] = '0'  # ['14', '5'] => ['14', '0']
        version = '.'.join(version[:2])  # ['14', '0'] => '14.0'; ['12', '0', '5'] => '12.0'
        machine = platform.mac_ver()[2]  # e.g., 'arm64'
        tag = f'macosx-{version}-{machine}'
    tag = tag.replace('-', '_')
    tag = tag.replace('.', '_')
    return tag

def _python_version_full():  # full version, e.g., '3.11.6'
    return platform.python_version()

def _python_version_short():  # short version, e.g., '311'
    return sysconfig.get_config_var('py_version_nodot')

def _python_tag():  # tag, e.g., 'py311'
    return f'py{_python_version_short()}'

def _python_site_packages():
    site_packages = site.getsitepackages()
    site_packages = site_packages[0]  # get the first location from the list
    return site_packages

def _python_lib():
    if _platform() == 'macos':
        #python_lib = '`python3-config --ldflags --embed`' # Get Python linker flags
        #python_lib = '`pkg-config --libs python3-embed`' # Get Python linker flags
        python_lib = '-Wl,-undefined,dynamic_lookup' # Resolve Python symbols at runtime
    elif _platform() == 'linux':
        python_lib = ''
    elif _platform() == 'windows':
        lib_file = f'python{_python_version_short()}.lib'
        python_lib = os.path.join(_python_site_packages(), 'libs', lib_file)
    else:
        raise Exception(f'Unsupported platform {_platform()}')
    return python_lib

def _ifport_lib():
    try:
        ifport_lib = CONFIG['build'][_platform()][_compiler_name()]
    except KeyError:
        ifport_lib = ''
    return ifport_lib

def _initial_python_wheel_tags():
    return {
        'python_tag': 'py3',
        'abi_tag': 'none',
        'platform_tag': 'any'
    }

def _new_python_wheel_tags():
    return {
        'python_tag': _python_tag(),
        'abi_tag': 'none',
        'platform_tag': _platform_tag_github_ci()
    }

def _initial_wheel_name():
    dist_package_name = PYPROJECT['project']['name']
    dist_package_version = PYPROJECT['project']['version']
    python_tag = _initial_python_wheel_tags()['python_tag']
    abi_tag = _initial_python_wheel_tags()['abi_tag']
    platform_tag = _initial_python_wheel_tags()['platform_tag']
    name = f'{dist_package_name}-{dist_package_version}-{python_tag}-{abi_tag}-{platform_tag}.whl'
    return name

def _new_wheel_name():
    dist_package_name = PYPROJECT['project']['name']
    dist_package_version = PYPROJECT['project']['version']
    python_tag = _new_python_wheel_tags()['python_tag']
    abi_tag = _new_python_wheel_tags()['abi_tag']
    platform_tag = _new_python_wheel_tags()['platform_tag']
    name = f'{dist_package_name}-{dist_package_version}-{python_tag}-{abi_tag}-{platform_tag}.whl'
    return name

def _fix_file_permissions(path: str):
    os.chmod(path, 0o777)

def _write_lines_to_file(lines: list, name: str):
    path = os.path.join(_scripts_path(), name)
    with open(path, 'w') as file:
        for line in lines:
            line = _apply_bash_syntax_if_needed(line)
            file.write(line + '\n')
    _fix_file_permissions(path)

def _total_src_file_count(modules: str):
    count = 0
    for module in CONFIG[modules]:
        if 'main-file' in module:
            count += 1
        if 'components-dir' in module and 'components-files' in module:
            for component_file in module['components-files']:
                count += 1
    return count

def _bash_syntax():
    bash_syntax = False  # if '--bash-syntax' is undefined
    if ARGS.bash_syntax:
        bash_syntax = ARGS.bash_syntax
    return bash_syntax

def _apply_bash_syntax_if_needed(line: str):
    if _bash_syntax():
        line = line.replace('\\', '/')  # change path separators
        line = line.replace('/033', r'\033')  # fix colors after previous step
    return line

def _enable_backslash_escapes():
    enable_backslash_escapes = False  # if '--enable-backslash-escapes' is undefined
    if ARGS.enable_backslash_escapes:
        enable_backslash_escapes = ARGS.enable_backslash_escapes
    return enable_backslash_escapes

def _print_wheel_dir():
    wheel_dir = CONFIG['pycfml']['dir']['dist-wheel']
    print(wheel_dir)

def _print_release_version():
    release_version = PYPROJECT['project']['version']
    release_version = f'v{release_version}'
    print(release_version)

def _print_release_title():
    release_version = PYPROJECT['project']['version']
    dt = datetime.datetime.now()
    build_date = f'{dt.day} {dt:%b} {dt.year}'  # e.g. 3 Jun 2021
    release_title = f'Version {release_version} ({build_date})'
    print(release_title)

def _compiler_name():
    compiler = 'gfortran'  # default
    if ARGS.compiler:
        compiler = ARGS.compiler.lower()
    return compiler

def _compiler_options():
    compiler = _compiler_name()
    mode = _compiling_mode()
    options = ''
    for build in CONFIG['build-configs']:
        if _platform() in build['platforms'] and compiler in build['compilers']:
            options = f"{build['modes']['base']}"
            if build['modes'][mode]:
                options += f" {build['modes'][mode]}"
            break
    return options

def _extra_compiler_options():
    compiler = _compiler_name()
    extra = ''
    for build in CONFIG['build-configs']:
        if _platform() in build['platforms'] and compiler in build['compilers']:
            if 'extra' in build['modes']:
                extra += f" {build['modes']['extra']}"
            break
    return extra

def _obj_ext():
    compiler = _compiler_name()
    ext = ''
    for build in CONFIG['build-configs']:
        if _platform() in build['platforms'] and compiler in build['compilers']:
            ext = build['obj-ext']
            break
    return ext

def _compiler_build_shared_template():
    compiler = _compiler_name()
    template = ''
    for build in CONFIG['build-configs']:
        if _platform() in build['platforms'] and compiler in build['compilers']:
            template = build['build-shared']
            break
    return template

def _compiler_build_exe_template():
    compiler = _compiler_name()
    template = ''
    for build in CONFIG['build-configs']:
        if _platform() in build['platforms'] and compiler in build['compilers']:
            template = build['build-exe']
            break
    return template

def _compiler_obj_ext():
    compiler = _compiler_name()
    obj_ext = ''
    for build in CONFIG['build-configs']:
        if _platform() in build['platforms'] and compiler in build['compilers']:
            obj_ext = build['obj-ext']
            break
    return obj_ext

def _compiling_mode():
    mode = 'debug'  # default
    if ARGS.mode:
        mode = ARGS.mode.lower()
    return mode

def _compiling_progress(current: int, total: int):
    progress = round(current / total * 100)
    return progress

def _compile_obj_script_line(src_path: str,
                             include_path: str=''):
    compiler = _compiler_name()
    options = _compiler_options()
    extra = _extra_compiler_options()
    template_cmd = CONFIG['template']['build-obj']
    if not include_path:
        template_cmd = template_cmd.replace(' -I {INCLUDE}', '')
    else:
        template_cmd = template_cmd.replace('{INCLUDE}', include_path)
    cmd = template_cmd
    cmd = cmd.replace('{COMPILER}', compiler)
    cmd = cmd.replace('{OPTIONS}', options)
    cmd = cmd.replace('{PATH}', src_path)
    cmd = cmd.replace('{EXTRA}', extra)
    return cmd

def _compile_pycfml_shared_obj_or_dynamic_lib_script_line():
    src_name = CONFIG['pycfml']['src-name']
    shared_lib_ext = CONFIG['build']['shared-lib-ext'][_platform()]
    cfml_lib_name = CONFIG['cfml']['static-lib-name']
    cfml_dist_dir = CONFIG['cfml']['dir']['dist']
    cfml_dist_path = os.path.join(_project_path(), cfml_dist_dir)
    cfml_lib_dist_dir = CONFIG['cfml']['dir']['dist-lib']
    cfml_lib_dist_path = os.path.join(_project_path(), cfml_lib_dist_dir)
    cmd = _compiler_build_shared_template()
    cmd = cmd.replace('{COMPILER}', _compiler_name())
    cmd = cmd.replace('{PATH}', src_name)
    cmd = cmd.replace('{OBJ_EXT}', _compiler_obj_ext())
    #cmd = cmd.replace('{PATH}.{OBJ_EXT}', f'*.{obj_ext}')  # CFML_Wraps.o Wraps_*.o crysfml08lib.o
    cmd = cmd.replace('{EXT}', shared_lib_ext)
    cmd = cmd.replace('{CFML_LIB_PATH}', cfml_lib_dist_path)
    cmd = cmd.replace('{CFML_LIB_NAME}', cfml_lib_name)
    cmd = cmd.replace('{IFPORT_LIB}', _ifport_lib())
    cmd = cmd.replace('{PYTHON_LIB}', _python_lib())
    return cmd

def _compile_objs_script_lines(modules: str,
                               src_path: str,
                               include_path: str=''):
    compiler = _compiler_name()
    options = _compiler_options()
    extra = _extra_compiler_options()
    src_ext = CONFIG['build']['src-ext']
    template_cmd = CONFIG['template']['build-obj']
    if not include_path:
        template_cmd = template_cmd.replace(' -I {INCLUDE}', '')
    else:
        template_cmd = template_cmd.replace('{INCLUDE}', include_path)
    total = _total_src_file_count(modules)
    current = 0
    lines = []
    for module in CONFIG[modules]:
        if 'main-file' in module:
            current += 1
            name = f'{module["main-file"]}.{src_ext}'
            msg = _echo_progress_msg(current, total, f'{name}')
            lines.append(msg)
            path = os.path.join(src_path, name)
            cmd = template_cmd
            cmd = cmd.replace('{COMPILER}', compiler)
            cmd = cmd.replace('{OPTIONS}', options)
            cmd = cmd.replace('{PATH}', path)
            cmd = cmd.replace('{EXTRA}', extra)
            lines.append(cmd)
        if 'components-dir' in module and 'components-files' in module:
            components_dir = module['components-dir']
            for component_file in module['components-files']:
                current += 1
                name = f'{component_file}.{src_ext}'
                path = os.path.join(components_dir, name)
                msg = _echo_progress_msg(current, total, f'{components_dir}/{name}')
                lines.append(msg)
                path = os.path.join(src_path, path)
                cmd = template_cmd
                cmd = cmd.replace('{COMPILER}', compiler)
                cmd = cmd.replace('{OPTIONS}', options)
                cmd = cmd.replace('{PATH}', path)
                cmd = cmd.replace('{EXTRA}', extra)
                cmd = f'{cmd}&'  # start this bash command in background for parallel compilation
                lines.append(cmd)
                if current % 11 == 0:  # do not parallelise for more than 10 compilations
                    lines.append('wait')  # wait for all parallel bash commands to finish
            lines.append('wait')  # wait for all parallel bash commands to finish
    return lines

def _compile_shared_objs_or_dynamic_libs_script_lines(modules: str):
    shared_lib_ext = CONFIG['build']['shared-lib-ext'][_platform()]
    cfml_lib_name = CONFIG['cfml']['static-lib-name']
    cfml_dist_dir = CONFIG['cfml']['dir']['dist']
    cfml_dist_path = os.path.join(_project_path(), cfml_dist_dir)
    cfml_lib_dist_dir = CONFIG['cfml']['dir']['dist-lib']
    cfml_lib_dist_path = os.path.join(_project_path(), cfml_lib_dist_dir)
    template_cmd = _compiler_build_shared_template()
    total = _total_src_file_count(modules)
    current = 0
    lines = []
    for module in CONFIG[modules]:
        if 'main-file' in module:
            current += 1
            name = f'{module["main-file"]}'
            msg = _echo_progress_msg(current, total, f'{name}.{_obj_ext()}')
            lines.append(msg)
            cmd = template_cmd
            cmd = cmd.replace('{COMPILER}', _compiler_name())
            cmd = cmd.replace('{PATH}', name)
            cmd = cmd.replace('{OBJ_EXT}', _compiler_obj_ext())
            cmd = cmd.replace('{EXT}', shared_lib_ext)
            cmd = cmd.replace('{CFML_LIB_PATH}', cfml_lib_dist_path)
            cmd = cmd.replace('{CFML_LIB_NAME}', cfml_lib_name)
            cmd = cmd.replace('{IFPORT_LIB}', _ifport_lib())
            cmd = cmd.replace('{PYTHON_LIB}', _python_lib())
            #lines.append(f"echo '>>>>> {cmd}'")
            lines.append(cmd)
    return lines

def _compile_executables_script_lines(modules: str,
                                      src_path: str,
                                      include_path: str,
                                      lib_dir: str,
                                      lib_name: str):
    src_ext = CONFIG['build']['src-ext']
    lib_ext = CONFIG['build']['static-lib-ext'][_platform()]
    template_cmd = _compiler_build_exe_template()
    compiler = _compiler_name()
    options = _compiler_options()
    extra = _extra_compiler_options()
    total = _total_src_file_count(modules)
    current = 0
    lines = []
    for test in CONFIG[modules]:
        current += 1
        dir = test["main-dir"]
        main_name = test["main-file"]
        source_name = f'{main_name}.{src_ext}'
        msg = _echo_progress_msg(current, total, f"{source_name}")
        lines.append(msg)
        source_path = os.path.join(src_path, dir, source_name)
        cmd = template_cmd
        cmd = cmd.replace('{COMPILER}', compiler)
        cmd = cmd.replace('{OPTIONS}', options)
        cmd = cmd.replace('{EXE_NAME}', main_name)
        cmd = cmd.replace('{SOURCE_PATH}', source_path)
        cmd = cmd.replace('{CFML_INCLUDE_PATH}', include_path)
        cmd = cmd.replace('{CFML_LIB_DIR}', lib_dir)
        cmd = cmd.replace('{CFML_LIB_NAME}', lib_name)
        cmd = cmd.replace('{LIB_EXT}', lib_ext)
        cmd = cmd.replace('{EXTRA}', extra)
        #lines.append(f"echo '>>>>> {cmd}'")
        lines.append(cmd)
    return lines

def _run_subprocess(cmd:str):
    result = None
    p = subprocess.run(cmd, shell=True, text=True, capture_output=True)
    result = p.stdout
    return result

def parsed_args():
    parser = argparse.ArgumentParser(formatter_class=argparse.ArgumentDefaultsHelpFormatter)
    parser.add_argument("--create-scripts",
                        action='store_true',
                        help="create scripts for building and testing")
    parser.add_argument("--platform",
                        default="macos",
                        choices=['macos', 'linux', 'windows'],
                        type=str.lower,
                        help="platform identifier")
    parser.add_argument("--compiler",
                        default="gfortran",
                        choices=['gfortran', 'nagfor', 'ifx', 'ifort'],
                        type=str.lower,
                        help="fortran compiler")
    parser.add_argument("--mode",
                        default="debug",
                        choices=['debug', 'release'],
                        type=str.lower,
                        help="compiling mode")
    parser.add_argument("--bash-syntax",
                        action='store_true',
                        help="force bash shell syntax")
    parser.add_argument("--enable-backslash-escapes",
                        action='store_true',
                        help="enable interpret backslash escapes (needed for GitHub CI)")
    parser.add_argument("--print-wheel-dir",
                        action='store_true',
                        help="print pycfml wheel directory name")
    parser.add_argument("--print-release-version",
                        action='store_true',
                        help="print pycfml package release version")
    parser.add_argument("--print-release-title",
                        action='store_true',
                        help="print pycfml package release title")
    return parser.parse_args()

def loaded_pyproject():
    path = os.path.join(_project_dir(), 'pyproject.toml')
    #with open(path, 'rb') as f:
        #pyproject = tomllib.load(f)
    pyproject = toml.load(path)
    return pyproject

def loaded_config(name: str):
    path = _config_path(name)
    #with open(path, 'rb') as f:
        #config = tomllib.load(f)
    config = toml.load(path)
    if _bash_syntax():
        for idx, build in enumerate(config['build-configs']):
            config['build-configs'][idx]['build-shared'] = build['build-shared'].replace('/', '-')
            config['build-configs'][idx]['build-exe'] = build['build-exe'].replace('/', '-')
            config['build-configs'][idx]['modes']['base'] = build['modes']['base'].replace('/', '-')
            config['build-configs'][idx]['modes']['debug'] = build['modes']['debug'].replace('/', '-')
            config['build-configs'][idx]['modes']['release'] = build['modes']['release'].replace('/', '-')
            if 'extra' in build['modes']:
                config['build-configs'][idx]['modes']['extra'] = build['modes']['extra'].replace('/', '-')
    return config

def clear_main_script():
    path = _main_script_path()
    if not os.path.isfile(path):
        file = Path(path)
        file.parent.mkdir(exist_ok=True, parents=True)
    with open(path, 'w') as file:
        pass

def append_to_main_script(obj: str | list):
    path = _main_script_path()
    if isinstance(obj, str):
        lines = [obj]
    elif isinstance(obj, list):
        lines = obj
    with open(path, 'a') as file:
        for line in lines:
            line = _apply_bash_syntax_if_needed(line)
            file.write(line + '\n')
    _fix_file_permissions(path)

def add_main_script_header(txt: str):
    header = _echo_header(txt)
    append_to_main_script(header)

def print_build_variables():
    lines = []
    if _platform() == 'macos':
        target = _macos_deployment_target()
        lines.append(f'export MACOSX_DEPLOYMENT_TARGET={target}')
        msg = _echo_msg(f"MACOSX_DEPLOYMENT_TARGET: {target}")
        lines.append(msg)
        lines.append('export SDKROOT="$(xcrun --show-sdk-path)"')
        msg = _echo_msg(f"SDKROOT: $(xcrun --show-sdk-path)")
        lines.append(msg)
    msg = _echo_msg(f"Platform: {_platform()}")
    lines.append(msg)
    msg = _echo_msg(f"Processor: {_processor()}")
    lines.append(msg)
    msg = _echo_msg(f"Compiling mode: {_compiling_mode()}")
    lines.append(msg)
    #msg = _echo_msg(f"Compiler options '{_compiler_options()}'")
    #lines.append(msg)
    msg = _echo_msg(f"Fortran compiler: {_compiler_name()}")
    lines.append(msg)
    msg = _echo_msg(f"Python version: {_python_version_full()}")
    lines.append(msg)
    msg = _echo_msg(f"Python tag: {_python_tag()}")
    lines.append(msg)
    msg = _echo_msg(f"Python site packages: {_python_site_packages()}")
    lines.append(msg)
    msg = _echo_msg(f"Python lib: {_python_lib()}")
    lines.append(msg)
    script_name = f'{sys._getframe().f_code.co_name}.sh'
    _write_lines_to_file(lines, script_name)
    append_to_main_script(lines)

def create_cfml_repo_dir():
    repo_dir = CONFIG['cfml']['dir']['repo']
    repo_path = os.path.join(_project_path(), repo_dir)
    lines = []
    msg = _echo_msg(f"Deleting build dir '{repo_dir}'")
    lines.append(msg)
    cmd = f'rm -rf {repo_path}'
    lines.append(cmd)
    msg = _echo_msg(f"Creating build dir '{repo_dir}'")
    lines.append(msg)
    cmd = f'mkdir -p {repo_path}'
    lines.append(cmd)
    script_name = f'{sys._getframe().f_code.co_name}.sh'
    _write_lines_to_file(lines, script_name)
    append_to_main_script(lines)

def download_cfml_repo():
    project_name = CONFIG['cfml']['log-name']
    url = CONFIG['cfml']['git']['url']
    branch = CONFIG['cfml']['git']['branch']
    out_dir = CONFIG['cfml']['dir']['repo']
    out_path = os.path.abspath(out_dir)
    lines = []
    msg = _echo_msg(f"Downloading {project_name} ('{branch}' branch) to '{out_dir}' from {url}")
    lines.append(msg)
    cmd = CONFIG['template']['clone-repo']
    cmd = cmd.replace('{BRANCH}', branch)
    cmd = cmd.replace('{URL}', url)
    cmd = cmd.replace('{OUT_PATH}', out_path)
    lines.append(cmd)
    script_name = f'{sys._getframe().f_code.co_name}.sh'
    _write_lines_to_file(lines, script_name)
    append_to_main_script(lines)

def create_cfml_build_dir():
    build_dir = CONFIG['cfml']['dir']['build-obj']
    build_path = os.path.join(_project_path(), build_dir)
    lines = []
    msg = _echo_msg(f"Deleting build dir '{build_dir}'")
    lines.append(msg)
    cmd = f'rm -rf {build_path}'
    lines.append(cmd)
    msg = _echo_msg(f"Creating build dir '{build_dir}'")
    lines.append(msg)
    cmd = f'mkdir -p {build_path}'
    lines.append(cmd)
    script_name = f'{sys._getframe().f_code.co_name}.sh'
    _write_lines_to_file(lines, script_name)
    append_to_main_script(lines)

def rename_global_deps_file():
    src_ext = CONFIG['build']['src-ext']
    src_dir = CONFIG['cfml']['dir']['repo-src']
    if _platform() == 'macos':
        platform_suffix = 'MacOS'
    elif _platform() == 'linux':
        platform_suffix = 'Linux'
    elif _platform() == 'windows':
        platform_suffix = 'Windows'
    compiler = _compiler_name()
    if compiler in ['gfortran', 'nagfor']:
        compiler_suffix = 'GFOR'
    elif compiler in ['ifort', 'ifx']:
        compiler_suffix = 'IFOR'
    from_name = f'CFML_GlobalDeps_{platform_suffix}_{compiler_suffix}.{src_ext}'
    from_relpath = os.path.join(src_dir, from_name)
    from_abspath = os.path.join(_project_path(), from_relpath)
    to_name = f'CFML_GlobalDeps.{src_ext}'
    to_relpath = os.path.join(src_dir, to_name)
    to_abspath = os.path.join(_project_path(), to_relpath)
    lines = []
    msg = _echo_msg(f"Copying '{from_relpath}' to '{to_relpath}'")
    lines.append(msg)
    cmd = f'cp {from_abspath} {to_abspath}'
    lines.append(cmd)
    script_name = f'{sys._getframe().f_code.co_name}.sh'
    _write_lines_to_file(lines, script_name)
    append_to_main_script(lines)

def build_cfml_modules_obj():
    project_name = CONFIG['cfml']['log-name']
    src_dir = CONFIG['cfml']['dir']['repo-src']
    src_path = os.path.join(_project_path(), src_dir)
    build_dir = CONFIG['cfml']['dir']['build-obj']
    build_path = os.path.join(_project_path(), build_dir)
    lines = []
    msg = _echo_msg(f"Entering build dir '{build_dir}'")
    lines.append(msg)
    cmd = f'cd {build_path}'
    lines.append(cmd)
    msg = _echo_msg(f"Building fortran objects for {project_name} modules")
    lines.append(msg)
    compile_lines = _compile_objs_script_lines('src-cfml-modules',
                                               src_path)
    lines.extend(compile_lines)
    msg = _echo_msg(f"Exiting build dir '{build_dir}'")
    lines.append(msg)
    cmd = f'cd {_project_path()}'
    lines.append(cmd)
    script_name = f'{sys._getframe().f_code.co_name}.sh'
    _write_lines_to_file(lines, script_name)
    append_to_main_script(lines)

def delete_renamed_global_deps_file():
    src_ext = CONFIG['build']['src-ext']
    src_dir = CONFIG['cfml']['dir']['repo-src']
    name = f'CFML_GlobalDeps.{src_ext}'
    relpath = os.path.join(src_dir, name)
    abspath = os.path.join(_project_path(), relpath)
    lines = []
    msg = _echo_msg(f"Deleting previously created '{relpath}'")
    lines.append(msg)
    cmd = f'rm {abspath}'
    lines.append(cmd)
    script_name = f'{sys._getframe().f_code.co_name}.sh'
    _write_lines_to_file(lines, script_name)
    append_to_main_script(lines)

def build_cfml_static_lib():
    lib_ext = CONFIG['build']['static-lib-ext'][_platform()]
    static_lib_prefix = CONFIG['build']['static-lib-prefix'][_platform()]
    lib_name = CONFIG['cfml']['static-lib-name']
    lib_name = f'{static_lib_prefix}{lib_name}'
    build_dir = CONFIG['cfml']['dir']['build-obj']
    build_path = os.path.join(_project_path(), build_dir)
    lines = []
    msg = _echo_msg(f"Entering build dir '{build_dir}'")
    lines.append(msg)
    cmd = f'cd {build_path}'
    lines.append(cmd)
    msg = _echo_msg(f"Creating fortran static library '{lib_name}.{lib_ext}'")
    lines.append(msg)
    cmd = CONFIG['template']['build-static'][_platform()]
    cmd = cmd.replace('{LIB}', lib_name)
    cmd = cmd.replace('{OBJ_EXT}', _obj_ext())
    lines.append(cmd)
    msg = _echo_msg(f"Exiting build dir '{build_dir}'")
    lines.append(msg)
    cmd = f'cd {_project_path()}'
    lines.append(cmd)
    script_name = f'{sys._getframe().f_code.co_name}.sh'
    _write_lines_to_file(lines, script_name)
    append_to_main_script(lines)

def create_cfml_dist_dir():
    dist_dir = CONFIG['cfml']['dir']['dist']
    lib_dir = CONFIG['cfml']['dir']['dist-lib']
    include_dir = CONFIG['cfml']['dir']['dist-include']
    progs_dir = CONFIG['cfml']['dir']['dist-progs']
    lines = []
    msg = _echo_msg(f"Deleting dist dir '{dist_dir}'")
    lines.append(msg)
    cmd = f'rm -rf {dist_dir}'
    lines.append(cmd)
    msg = _echo_msg(f"Creating dist dir '{dist_dir}'")
    lines.append(msg)
    cmd = f'mkdir -p {dist_dir}'
    lines.append(cmd)
    msg = _echo_msg(f"Creating dist dir '{lib_dir}'")
    lines.append(msg)
    cmd = f'mkdir -p {lib_dir}'
    lines.append(cmd)
    msg = _echo_msg(f"Creating dist dir '{include_dir}'")
    lines.append(msg)
    cmd = f'mkdir -p {include_dir}'
    lines.append(cmd)
    msg = _echo_msg(f"Creating dist dir '{progs_dir}'")
    lines.append(msg)
    cmd = f'mkdir -p {progs_dir}'
    lines.append(cmd)
    script_name = f'{sys._getframe().f_code.co_name}.sh'
    _write_lines_to_file(lines, script_name)
    append_to_main_script(lines)

def move_built_to_cfml_dist():
    lib_ext = CONFIG['build']['static-lib-ext'][_platform()]
    static_lib_prefix = CONFIG['build']['static-lib-prefix'][_platform()]
    lib_name = CONFIG['cfml']['static-lib-name']
    lib_name = f'{static_lib_prefix}{lib_name}'
    build_dir = CONFIG['cfml']['dir']['build-obj']
    build_path = os.path.join(_project_path(), build_dir)
    lib_dist_dir = CONFIG['cfml']['dir']['dist-lib']
    lib_dist_path = os.path.join(_project_path(), lib_dist_dir)
    include_dist_dir = CONFIG['cfml']['dir']['dist-include']
    include_dist_path = os.path.join(_project_path(), include_dist_dir)
    lines = []
    msg = _echo_msg(f"Moving built lib '{lib_name}.{lib_ext}' to dist dir '{lib_dist_dir}'")
    lines.append(msg)
    from_path = os.path.join(build_path, f'{lib_name}.{lib_ext}')
    cmd = f'mv {from_path} {lib_dist_path}'
    lines.append(cmd)
    msg = _echo_msg(f"Moving built modules '*.*mod' to dist dir '{include_dist_dir}'")
    lines.append(msg)
    from_path = os.path.join(build_path, '*.*mod')
    cmd = f'mv {from_path} {include_dist_path}'
    lines.append(cmd)
    script_name = f'{sys._getframe().f_code.co_name}.sh'
    _write_lines_to_file(lines, script_name)
    append_to_main_script(lines)

def build_cfml_test_programs():
    project_name = CONFIG['cfml']['log-name']
    src_dir = CONFIG['cfml']['dir']['repo-tests']
    src_path = os.path.join(_project_path(), src_dir)
    #build_dir = os.path.join('tests', 'functional_tests', 'cfml')
    build_dir = CONFIG['cfml']['dir']['dist-progs']
    build_path = os.path.join(_project_path(), build_dir)
    dist_dir = CONFIG['cfml']['dir']['dist']
    include_dir = CONFIG['cfml']['dir']['dist-include']
    include_path = os.path.join(_project_path(), include_dir)
    lib_dir = CONFIG['cfml']['dir']['dist-lib']
    lib_path = os.path.join(_project_path(), lib_dir)
    lib_name = CONFIG['cfml']['static-lib-name']
    lines = []
    msg = _echo_msg(f"Entering build dir '{build_dir}'")
    lines.append(msg)
    cmd = f'cd {build_path}'
    lines.append(cmd)
    msg = _echo_msg(f"Building test programs for {project_name}")
    lines.append(msg)
    compile_lines = _compile_executables_script_lines('src-cfml-tests',
                                                      src_path, include_path,
                                                      lib_path,
                                                      lib_name)
    lines.extend(compile_lines)
    msg = _echo_msg(f"Deleting *.mod/*.smod files in '{build_dir}'")
    lines.append(msg)
    cmd = f'rm -rf *.*mod'
    lines.append(cmd)
    msg = _echo_msg(f"Exiting build dir '{build_dir}'")
    lines.append(msg)
    cmd = f'cd {_project_path()}'
    lines.append(cmd)
    script_name = f'{sys._getframe().f_code.co_name}.sh'
    _write_lines_to_file(lines, script_name)
    append_to_main_script(lines)

def copy_cfml_test_programs_to_tests_dir():
    progs_relpath = CONFIG['cfml']['dir']['dist-progs']
    progs_abspath = os.path.join(_project_path(), progs_relpath)
    from_path = os.path.join(progs_abspath, '*')
    tests_relpath = os.path.join('tests', 'functional_tests', 'CFML')
    tests_abspath = os.path.join(_project_path(), tests_relpath)
    to_path = tests_abspath
    lines = []
    msg = _echo_msg(f"Copying all files from '{progs_relpath}' to '{tests_relpath}'")
    lines.append(msg)
    cmd = f'cp {from_path} {to_path}'
    lines.append(cmd)
    script_name = f'{sys._getframe().f_code.co_name}.sh'
    _write_lines_to_file(lines, script_name)
    append_to_main_script(lines)

def run_cfml_functional_tests_no_benchmarks():
    tests_relpath = os.path.join('tests', 'functional_tests', 'CFML')
    tests_abspath = os.path.join(_project_path(), tests_relpath)
    lines = []
    #msg = _echo_msg(f"Entering tests dir '{tests_relpath}'")
    #lines.append(msg)
    #cmd = f'cd {tests_relpath}'
    #lines.append(cmd)
    msg = _echo_msg(f"Running functional tests from '{tests_relpath}'")
    lines.append(msg)
    cmd = CONFIG['template']['run-tests']
    cmd = cmd.replace('{PATH}', tests_abspath)
    lines.append(cmd)
    #msg = _echo_msg(f"Exiting tests dir '{tests_relpath}'")
    #lines.append(msg)
    #cmd = f'cd {_project_path()}'
    #lines.append(cmd)
    script_name = f'{sys._getframe().f_code.co_name}.sh'
    _write_lines_to_file(lines, script_name)
    append_to_main_script(lines)

def run_cfml_functional_tests_with_benchmarks():
    project_name = CONFIG['cfml']['log-name']
    relpath = os.path.join('tests', 'functional_tests', 'CFML')
    abspath = os.path.join(_project_path(), relpath)
    lines = []
    msg = _echo_msg(f"Running functional tests with benchmarks from '{relpath}'")
    lines.append(msg)
    cmd = CONFIG['template']['run-benchmarks']['base']
    if _github_branch() == 'develop':
        cmd += ' ' + CONFIG['template']['run-benchmarks']['develop-branch']
    else:
        cmd += ' ' + CONFIG['template']['run-benchmarks']['non-develop-branch']
    cmd = cmd.replace('{PATH}', abspath)
    cmd = cmd.replace('{PROJECT}', project_name)
    if _github_actions():
        cmd = cmd.replace('{RUNNER}', 'github')
    else:
        cmd = cmd.replace('{RUNNER}', 'local')
    cmd = cmd.replace('{COMPILER}', _compiler_name())
    cmd = cmd.replace('{PROCESSOR}', _processor())
    lines.append(cmd)
    script_name = f'{sys._getframe().f_code.co_name}.sh'
    _write_lines_to_file(lines, script_name)
    append_to_main_script(lines)

def copy_powder_mod_to_pycfml_repo():
    CFML = CONFIG['cfml']['log-name']
    cfml_repo_dir = CONFIG['cfml']['dir']['repo']
    cfml_repo_path = os.path.join(_project_path(), cfml_repo_dir)
    pyCFML = CONFIG['pycfml']['log-name']
    pycfml_repo_dir = CONFIG['pycfml']['dir2']['repo']
    pycfml_src_dir = CONFIG['pycfml']['dir2']['repo-src']
    pycfml_src_path = os.path.join(_project_path(), pycfml_repo_dir, pycfml_src_dir)
    src_ext = CONFIG['build']['src-ext']
    lines = []
    from_relpath = os.path.join('Testing', 'Powder', 'Test_2', 'fortran', 'src', f'powder_mod.{src_ext}')
    from_abspath = os.path.join(cfml_repo_path, from_relpath)
    msg = _echo_msg(f"Copying '{from_relpath}' to '{pycfml_repo_dir}/{pycfml_src_dir}'")
    lines.append(msg)
    cmd = f'cp {from_abspath} {pycfml_src_path}'
    lines.append(cmd)
    from_relpath = os.path.join('Testing', 'Powder', 'Test_3', f'powder_mod_2.{src_ext}')
    from_abspath = os.path.join(cfml_repo_path, from_relpath)
    msg = _echo_msg(f"Copying '{from_relpath}' to '{pycfml_repo_dir}/{pycfml_src_dir}'")
    lines.append(msg)
    cmd = f'cp {from_abspath} {pycfml_src_path}'
    lines.append(cmd)
    script_name = f'{sys._getframe().f_code.co_name}.sh'
    _write_lines_to_file(lines, script_name)
    append_to_main_script(lines)

def create_pycfml_src_dir():
    src_dir = CONFIG['pycfml']['dir']['build-src']
    lines = []
    msg = _echo_msg(f"Deleting src dir '{src_dir}'")
    lines.append(msg)
    cmd = f'rm -rf {src_dir}'
    lines.append(cmd)
    msg = _echo_msg(f"Creating src dir '{src_dir}'")
    lines.append(msg)
    cmd = f'mkdir -p {src_dir}'
    lines.append(cmd)
    script_name = f'{sys._getframe().f_code.co_name}.sh'
    _write_lines_to_file(lines, script_name)
    append_to_main_script(lines)

def create_pycfml_build_dir():
    build_dir = CONFIG['pycfml']['dir']['build-obj']
    lines = []
    msg = _echo_msg(f"Deleting build dir '{build_dir}'")
    lines.append(msg)
    cmd = f'rm -rf {build_dir}'
    lines.append(cmd)
    msg = _echo_msg(f"Creating build dir '{build_dir}'")
    lines.append(msg)
    cmd = f'mkdir -p {build_dir}'
    lines.append(cmd)
    script_name = f'{sys._getframe().f_code.co_name}.sh'
    _write_lines_to_file(lines, script_name)
    append_to_main_script(lines)

def create_pycfml_src():
    project_name = CONFIG['pycfml']['log-name']
    apigen_relpath = CONFIG['cfml']['scripts']['pyapigen']
    apigen_parent_relpath = Path(apigen_relpath).parent.as_posix()
    apigen_file = Path(apigen_relpath).name
    build_relpath = CONFIG['pycfml']['dir']['build-src']
    build_abspath = os.path.join(_project_path(), build_relpath)
    lines = []
    msg = _echo_msg(f"Entering Python API gen dir '{apigen_parent_relpath}'")
    lines.append(msg)
    cmd = f'cd {apigen_parent_relpath}'
    lines.append(cmd)
    msg = _echo_msg(f"Creating {project_name} source code with '{apigen_file}'")
    lines.append(msg)
    cmd = CONFIG['template']['run-python']
    cmd = cmd.replace('{PATH}', apigen_file)
    cmd = cmd.replace('{OPTIONS}', f'--verbose False --scripts False --build {build_abspath}')
    lines.append(cmd)
    msg = _echo_msg(f"Exiting build dir '{apigen_parent_relpath}'")
    lines.append(msg)
    cmd = f'cd {_project_path()}'
    lines.append(cmd)
    script_name = f'{sys._getframe().f_code.co_name}.sh'
    _write_lines_to_file(lines, script_name)
    append_to_main_script(lines)

def build_pycfml_modules_obj():
    project_name = CONFIG['cfml']['log-name']
    src_relpath = CONFIG['pycfml']['dir']['build-src-fortran']
    src_abspath = os.path.join(_project_path(), src_relpath)
    build_relpath = CONFIG['pycfml']['dir']['build-obj']
    include_relpath = CONFIG['cfml']['dir']['dist-include']
    include_abspath = os.path.join(_project_path(), include_relpath)
    lines = []
    msg = _echo_msg(f"Entering build dir '{build_relpath}'")
    lines.append(msg)
    cmd = f'cd {build_relpath}'
    lines.append(cmd)
    msg = _echo_msg(f"Building fortran objects for {project_name} modules")
    lines.append(msg)
    compile_lines = _compile_objs_script_lines('src-cfml-wraps',
                                               src_abspath,
                                               include_abspath)
    lines.extend(compile_lines)
    msg = _echo_msg(f"Exiting build dir '{build_relpath}'")
    lines.append(msg)
    cmd = f'cd {_project_path()}'
    lines.append(cmd)
    script_name = f'{sys._getframe().f_code.co_name}.sh'
    _write_lines_to_file(lines, script_name)
    append_to_main_script(lines)

def build_pycfml_lib_obj():
    project_name = CONFIG['pycfml']['log-name']
    src_ext = CONFIG['build']['src-ext']
    pycfml_src_name = CONFIG['pycfml']['src-name']
    pycfml_src_file = f'{pycfml_src_name}.{src_ext}'
    src_relpath = CONFIG['pycfml']['dir']['build-src-fortran']
    src_abspath = os.path.join(_project_path(), src_relpath, pycfml_src_file)
    build_relpath = CONFIG['pycfml']['dir']['build-obj']
    include_relpath = CONFIG['cfml']['dir']['dist-include']
    include_abspath = os.path.join(_project_path(), include_relpath)
    lines = []
    msg = _echo_msg(f"Entering build dir '{build_relpath}'")
    lines.append(msg)
    cmd = f'cd {build_relpath}'
    lines.append(cmd)
    msg = _echo_msg(f"Building fortran object for {project_name} library")
    lines.append(msg)
    compile_line = _compile_obj_script_line(src_abspath, include_abspath)
    lines.append(compile_line)
    msg = _echo_msg(f"Exiting build dir '{build_relpath}'")
    lines.append(msg)
    cmd = f'cd {_project_path()}'
    lines.append(cmd)
    script_name = f'{sys._getframe().f_code.co_name}.sh'
    _write_lines_to_file(lines, script_name)
    append_to_main_script(lines)

def build_pycfml_shared_obj_or_dynamic_lib():
    build_relpath = CONFIG['pycfml']['dir']['build-obj']
    lib_name = CONFIG['pycfml']['src-name']  # NEED FIX: use CONFIG['pycfml']['dynamic-lib-name']
    lib_ext = CONFIG['build']['shared-lib-ext'][_platform()]
    lines = []
    msg = _echo_msg(f"Entering build dir '{build_relpath}'")
    lines.append(msg)
    cmd = f'cd {build_relpath}'
    lines.append(cmd)
    msg = _echo_msg(f"Building fortran shared obj or dynamic lib '{lib_name}.{lib_ext}'")
    lines.append(msg)
    compile_line = _compile_pycfml_shared_obj_or_dynamic_lib_script_line()
    lines.append(compile_line)
    msg = _echo_msg(f"Exiting build dir '{build_relpath}'")
    lines.append(msg)
    cmd = f'cd {_project_path()}'
    lines.append(cmd)
    script_name = f'{sys._getframe().f_code.co_name}.sh'
    _write_lines_to_file(lines, script_name)
    append_to_main_script(lines)

def create_pycfml_dist_dir():
    dist_dir = CONFIG['pycfml']['dir']['dist']
    lib_dist_dir = CONFIG['pycfml']['dir']['dist-lib']
    include_dist_dir = CONFIG['pycfml']['dir']['dist-include']
    package_dist_dir = CONFIG['pycfml']['dir']['dist-package'].replace('{PACKAGE_NAME}', PYPROJECT['project']['name'])
    wheel_dist_dir = CONFIG['pycfml']['dir']['dist-wheel']
    lines = []
    msg = _echo_msg(f"Deleting dist dir '{dist_dir}'")
    lines.append(msg)
    cmd = f'rm -rf {dist_dir}'
    lines.append(cmd)
    msg = _echo_msg(f"Creating dist dir '{dist_dir}'")
    lines.append(msg)
    cmd = f'mkdir -p {dist_dir}'
    lines.append(cmd)
    msg = _echo_msg(f"Creating dist dir '{lib_dist_dir}'")
    lines.append(msg)
    cmd = f'mkdir -p {lib_dist_dir}'
    lines.append(cmd)
    msg = _echo_msg(f"Creating dist dir '{include_dist_dir}'")
    lines.append(msg)
    cmd = f'mkdir -p {include_dist_dir}'
    lines.append(cmd)
    msg = _echo_msg(f"Creating dist dir '{package_dist_dir}'")
    lines.append(msg)
    cmd = f'mkdir -p {package_dist_dir}'
    lines.append(cmd)
    msg = _echo_msg(f"Creating dist dir '{wheel_dist_dir}'")
    lines.append(msg)
    cmd = f'mkdir -p {wheel_dist_dir}'
    lines.append(cmd)
    script_name = f'{sys._getframe().f_code.co_name}.sh'
    _write_lines_to_file(lines, script_name)
    append_to_main_script(lines)

def copy_built_to_pycfml_dist():
    project_name = CONFIG['pycfml']['log-name']
    shared_lib_ext = CONFIG['build']['shared-lib-ext'][_platform()]
    build_relpath = CONFIG['pycfml']['dir']['build-obj']
    build_abspath = os.path.join(_project_path(), build_relpath)
    package_relpath = CONFIG['pycfml']['dir']['dist-package'].replace('{PACKAGE_NAME}', PYPROJECT['project']['name'])
    package_abspath = os.path.join(_project_path(), package_relpath)
    lines = []
    msg = _echo_msg(f"Copying built {project_name} shared objects / dynamic libs to '{package_relpath}'")
    lines.append(msg)
    from_path = os.path.join(build_abspath, f'*.{shared_lib_ext}')
    to_path = package_abspath
    cmd = f'cp {from_path} {to_path}'
    #cmd = cmd + ' || true'  # allows to suppress the error message if no files are found
    lines.append(cmd)
    script_name = f'{sys._getframe().f_code.co_name}.sh'
    _write_lines_to_file(lines, script_name)
    append_to_main_script(lines)

def change_runpath_for_built_pycfml():
    # Tried to set rpath to $ORIGIN (with -Wl,-rpath,'$ORIGIN') during the build
    # shared objects step (CONFIG['build-shared']), but it didn't help :(
    # Ubuntu usage examples:
    # sudo find / -iname "libif*"
    # ls -l dist/pyCFML/pycrysfml
    # patchelf --print-rpath dist/pyCFML/pycrysfml/crysfml08lib.so
    # patchelf --set-rpath '$ORIGIN' dist/pyCFML/pycrysfml/crysfml08lib.so
    # patchelf --print-rpath dist/pyCFML/pycrysfml/crysfml08lib.so
    # patchelf --no-default-lib dist/pyCFML/pycrysfml/crysfml08lib.so
    # ldd dist/pyCFML/pycrysfml/crysfml08lib.so
    # ls -l /opt/hostedtoolcache/Python/3.11.8/x64/lib/python3.11/site-packages/pycrysfml08
    # ldd /opt/hostedtoolcache/Python/3.11.8/x64/lib/python3.11/site-packages/pycrysfml08/py_cfml_metrics.so
    # macOS usage example:
    # sudo find / -iname "libif*"
    # ls -l dist/pyCFML/pycrysfml
    # install_name_tool -rpath /opt/intel/oneapi/compiler/2023.2.0/mac/bin/intel64/../../compiler/lib @loader_path dist/pyCFML/pycrysfml/crysfml08lib.so
    # install_name_tool -delete_rpath /usr/local/Cellar/gcc/13.2.0/lib/gcc/current dist/pyCFML/pycrysfml/crysfml08lib.so
    # install_name_tool -change /usr/local/opt/gcc/lib/gcc/current/libgfortran.5.dylib @rpath/libgfortran.5.dylib dist/pyCFML/pycrysfml/crysfml08lib.so
    # otool -l dist/pyCFML/pycrysfml/crysfml08lib.so | grep RPATH -A2  # build.rpaths in in pybuild.toml
    # otool -L dist/pyCFML/pycrysfml/crysfml08lib.so                   # build.dependent-libs in pybuild.toml
    try:
        rpaths = CONFIG['build']['rpaths'][_platform()][_processor()][_compiler_name()]
    except KeyError:
        msg = _echo_msg(f"No change of runtime paths are needed for platform '{_platform()} ({_processor()})' and compiler '{_compiler_name()}'")
        lines = [msg]
        script_name = f'{sys._getframe().f_code.co_name}.sh'
        _write_lines_to_file(lines, script_name)
        append_to_main_script(lines)
        return
    project_name = CONFIG['pycfml']['log-name']
    shared_lib_ext = CONFIG['build']['shared-lib-ext'][_platform()]
    #dist_dir = CONFIG['pycfml']['dir2']['dist']
    #package_dir = CONFIG['pycfml']['dir2']['dist-package']
    #package_relpath = os.path.join(dist_dir, package_dir)
    #package_abspath = os.path.join(_project_path(), dist_dir, package_dir)
    package_relpath = CONFIG['pycfml']['dir']['dist-package'].replace('{PACKAGE_NAME}', PYPROJECT['project']['name'])
    package_abspath = os.path.join(_project_path(), package_relpath)
    #total = 1
    #current = 0
    lines = []
    if _platform() == 'linux':
        set_rpath_template_cmd = CONFIG['template']['rpath']['set'][_platform()]
        no_default_lib_template_cmd = CONFIG['template']['no-default-lib'][_platform()]
        msg = _echo_msg(f"Changing runpath(s) for built {project_name} shared object")
        lines.append(msg)
        name = CONFIG['pycfml']['src-name']
        path = os.path.join(package_abspath, name)
        for rpath in rpaths:
            old_rpath = rpath['old']
            new_rpath = rpath['new']
            msg = _echo_msg(f"Changing runpath for {name}.{shared_lib_ext} from '{old_rpath}' to '{new_rpath}'")
            lines.append(msg)
            cmd = set_rpath_template_cmd
            cmd = cmd.replace('{NEW}', new_rpath)
            cmd = cmd.replace('{PATH}', path)
            cmd = cmd.replace('{EXT}', shared_lib_ext)
            lines.append(cmd)
            cmd = no_default_lib_template_cmd
            cmd = cmd.replace('{PATH}', path)
            cmd = cmd.replace('{EXT}', shared_lib_ext)
            lines.append(cmd)
    elif _platform() == 'macos':
        try:
            dependent_libs = CONFIG['build']['dependent-libs'][_platform()][_processor()][_compiler_name()]
            change_lib_template_cmd = CONFIG['template']['dependent-lib']['change'][_platform()]
        except KeyError:
            dependent_libs = []
        delete_rpath_template_cmd = CONFIG['template']['rpath']['delete'][_platform()]
        change_rpath_template_cmd = CONFIG['template']['rpath']['change'][_platform()]
        msg = _echo_msg(f"Changing runpath(s) for built {project_name} shared objects")
        lines.append(msg)
        name = CONFIG['pycfml']['src-name']
        path = os.path.join(package_abspath, name)
        for rpath in rpaths:
            old_rpath = rpath['old']
            new_rpath = rpath['new']
            msg = _echo_msg(f"Changing runpath for {name}.{shared_lib_ext} from '{old_rpath}' to '{new_rpath}'")
            lines.append(msg)
            if rpath['new'] == '':  # delete this rpath
                cmd = delete_rpath_template_cmd
                cmd = cmd.replace('{OLD}', old_rpath)
                cmd = cmd.replace('{PATH}', path)
                cmd = cmd.replace('{EXT}', shared_lib_ext)
            else:  # change this rpath
                cmd = change_rpath_template_cmd
                cmd = cmd.replace('{OLD}', old_rpath)
                cmd = cmd.replace('{NEW}', new_rpath)
                cmd = cmd.replace('{PATH}', path)
                cmd = cmd.replace('{EXT}', shared_lib_ext)
            #cmd = cmd + ' || true'  # allows to suppress the error message if no files are found
            lines.append(cmd)
        for lib in dependent_libs:
            old_lib = lib['old']
            new_lib = lib['new']
            msg = _echo_msg(f"Changing the dependent shared library install name for {name}.{shared_lib_ext} from '{old_lib}' to '{new_lib}'")
            lines.append(msg)
            cmd = change_lib_template_cmd
            cmd = cmd.replace('{OLD}', old_lib)
            cmd = cmd.replace('{NEW}', new_lib)
            cmd = cmd.replace('{PATH}', path)
            cmd = cmd.replace('{EXT}', shared_lib_ext)
            #cmd = cmd + ' || true'  # allows to suppress the error message if no files are found
            lines.append(cmd)
    else:
        msg = _echo_msg(f"Changing runpath is not needed for platform '{_platform()}'")
        lines.append(msg)
    script_name = f'{sys._getframe().f_code.co_name}.sh'
    _write_lines_to_file(lines, script_name)
    append_to_main_script(lines)

def copy_extra_libs_to_pycfml_dist():
    try:
        extra_libs = CONFIG['build']['extra-libs'][_platform()][_processor()][_compiler_name()]
    except KeyError:
        msg = _echo_msg(f"No extra libraries are needed for platform '{_platform()}' and compiler '{_compiler_name()}'")
        lines = [msg]
        script_name = f'{sys._getframe().f_code.co_name}.sh'
        _write_lines_to_file(lines, script_name)
        append_to_main_script(lines)
        return
    package_relpath = CONFIG['pycfml']['dir']['dist-package'].replace('{PACKAGE_NAME}', PYPROJECT['project']['name'])
    package_abspath = os.path.join(_project_path(), package_relpath)
    lines = []
    for lib_path in extra_libs:
        display_lib_path = lib_path.replace('"', '')
        msg = _echo_msg(f"Copying '{display_lib_path}' to dist dir '{package_relpath}'")
        lines.append(msg)
        cmd = f'cp {lib_path} {package_abspath}'
        lines.append(cmd)
    script_name = f'{sys._getframe().f_code.co_name}.sh'
    _write_lines_to_file(lines, script_name)
    append_to_main_script(lines)

def copy_py_api_files_to_pycfml_dist():
    project_name = CONFIG['pycfml']['log-name']
    py_api_relpath = CONFIG['pycfml']['dir']['build-src-python']
    py_api_abspath = os.path.join(_project_path(), py_api_relpath)
    from_path = os.path.join(py_api_abspath, '*.py')
    package_relpath = CONFIG['pycfml']['dir']['dist-package'].replace('{PACKAGE_NAME}', PYPROJECT['project']['name'])
    package_abspath = os.path.join(_project_path(), package_relpath)
    to_path = package_abspath
    lines = []
    msg = _echo_msg(f"Copying {project_name} python api files from '{py_api_relpath}' to dist dir '{package_relpath}'")
    lines.append(msg)
    cmd = f'cp {from_path} {to_path}'
    lines.append(cmd)
    script_name = f'{sys._getframe().f_code.co_name}.sh'
    _write_lines_to_file(lines, script_name)
    append_to_main_script(lines)

def copy_init_file_to_pycfml_dist():
    project_name = CONFIG['pycfml']['log-name']
    from_path = os.path.join(_project_path(), 'src', '__init__.py')
    package_relpath = CONFIG['pycfml']['dir']['dist-package'].replace('{PACKAGE_NAME}', PYPROJECT['project']['name'])
    package_abspath = os.path.join(_project_path(), package_relpath)
    to_path = package_abspath
    lines = []
    msg = _echo_msg(f"Copying {project_name} 'src/__init__.py' to dist dir '{package_relpath}'")
    lines.append(msg)
    cmd = f'cp {from_path} {to_path}'
    lines.append(cmd)
    script_name = f'{sys._getframe().f_code.co_name}.sh'
    _write_lines_to_file(lines, script_name)
    append_to_main_script(lines)

def copy_cfml_databases_to_pycfml_dist():
    cfml_db_relpath = CONFIG['cfml']['dir']['repo-database']
    cfml_db_abspath = os.path.join(_project_path(), cfml_db_relpath)
    from_path = cfml_db_abspath
    package_relpath = CONFIG['pycfml']['dir']['dist-package'].replace('{PACKAGE_NAME}', PYPROJECT['project']['name'])
    pycfml_db_relpath = os.path.join(package_relpath, 'Databases')
    pycfml_db_abspath = os.path.join(_project_path(), pycfml_db_relpath)
    to_path = pycfml_db_abspath
    lines = []
    msg = _echo_msg(f"Creating dir '{pycfml_db_relpath}'")
    lines.append(msg)
    cmd = f'mkdir -p {pycfml_db_relpath}'
    lines.append(cmd)
    msg = _echo_msg(f"Copying '{cfml_db_relpath}' database to dist dir '{pycfml_db_relpath}'")
    lines.append(msg)
    cmd = f'cp {from_path} {to_path}'
    lines.append(cmd)
    script_name = f'{sys._getframe().f_code.co_name}.sh'
    _write_lines_to_file(lines, script_name)
    append_to_main_script(lines)

def validate_pyproject_toml():
    lines = []
    msg = _echo_msg(f"Validating pyproject.toml")
    lines.append(msg)
    cmd = 'validate-pyproject pyproject.toml'
    lines.append(cmd)
    script_name = f'{sys._getframe().f_code.co_name}.sh'
    _write_lines_to_file(lines, script_name)
    append_to_main_script(lines)

def create_pycfml_python_wheel():
    project_name = CONFIG['pycfml']['log-name']
    wheel_dir = CONFIG['pycfml']['dir']['dist-wheel']
    wheel_path = os.path.join(_project_path(), wheel_dir)
    lines = []
    msg = _echo_msg(f"Creating {project_name} python wheel in '{wheel_dir}'")
    lines.append(msg)
    cmd = CONFIG['template']['build-wheel']
    cmd = cmd.replace('{PATH}', wheel_path)
    lines.append(cmd)
    script_name = f'{sys._getframe().f_code.co_name}.sh'
    _write_lines_to_file(lines, script_name)
    append_to_main_script(lines)

def rename_pycfml_python_wheel():
    project_name = CONFIG['pycfml']['log-name']
    wheel_dir = CONFIG['pycfml']['dir']['dist-wheel']
    wheel_relpath = os.path.join(wheel_dir, _initial_wheel_name())
    wheel_abspath = os.path.join(_project_path(), wheel_relpath)
    lines = []
    msg = _echo_msg(f"Renaming {project_name} python wheel from '{_initial_wheel_name()}' to '{_new_wheel_name()}' in '{wheel_dir}'")
    lines.append(msg)
    cmd = CONFIG['template']['rename-wheel']
    cmd = cmd.replace('{PYTHON_TAG}', _python_tag())  # https://packaging.python.org/en/latest/specifications/platform-compatibility-tags/
    cmd = cmd.replace('{PLATFORM_TAG}', _platform_tag_github_ci())
    cmd = cmd.replace('{PATH}', wheel_abspath)
    lines.append(cmd)
    script_name = f'{sys._getframe().f_code.co_name}.sh'
    _write_lines_to_file(lines, script_name)
    append_to_main_script(lines)

def detect_abi3_violations():
    wheel_dir = CONFIG['pycfml']['dir']['dist-wheel']
    wheel_path = os.path.join(wheel_dir, _new_wheel_name())
    lines = []
    msg = _echo_msg(f"Scanning Python extensions in python wheel '{_new_wheel_name()}' for abi3 violations")
    lines.append(msg)
    cmd = f'abi3audit {wheel_path}'
    lines.append(cmd)
    script_name = f'{sys._getframe().f_code.co_name}.sh'
    _write_lines_to_file(lines, script_name)
    append_to_main_script(lines)

def check_wheel_contents():
    wheel_dir = CONFIG['pycfml']['dir']['dist-wheel']
    wheel_path = os.path.join(wheel_dir, _new_wheel_name())
    lines = []
    msg = _echo_msg(f"Checking content of Python wheel '{_new_wheel_name()}'")
    lines.append(msg)
    cmd = f'check-wheel-contents {wheel_path}'
    lines.append(cmd)
    script_name = f'{sys._getframe().f_code.co_name}.sh'
    _write_lines_to_file(lines, script_name)
    append_to_main_script(lines)

def install_pycfml_from_wheel():
    project_name = CONFIG['pycfml']['log-name']
    package_name = PYPROJECT['project']['name']
    wheel_dir = CONFIG['pycfml']['dir']['dist-wheel']
    wheel_path = os.path.join(_project_path(), wheel_dir)
    lines = []
    msg = _echo_msg(f"Installing {project_name} python wheel from '{wheel_dir}'")
    lines.append(msg)
    cmd = CONFIG['template']['install-wheel']
    cmd = cmd.replace('{PACKAGE}', package_name)
    cmd = cmd.replace('{PATH}', wheel_path)
    lines.append(cmd)
    script_name = f'{sys._getframe().f_code.co_name}.sh'
    _write_lines_to_file(lines, script_name)
    append_to_main_script(lines)

def run_pycfml_unit_tests():
    relpath = os.path.join('tests', 'unit_tests', 'pyCFML')
    abspath = os.path.join(_project_path(), relpath)
    lines = []
    msg = _echo_msg(f"Running tests from '{relpath}'")
    lines.append(msg)
    cmd = CONFIG['template']['run-tests']
    cmd = cmd.replace('{PATH}', abspath)
    lines.append(cmd)
    script_name = f'{sys._getframe().f_code.co_name}.sh'
    _write_lines_to_file(lines, script_name)
    append_to_main_script(lines)

def run_powder_mod_tests():
    relpath = os.path.join('tests', 'functional_tests', 'pyCFML')
    abspath = os.path.join(_project_path(), relpath)
    lines = []
    msg = _echo_msg(f"Running powder_mod tests from '{relpath}'")
    lines.append(msg)
    cmd = CONFIG['template']['run-tests']
    cmd = cmd.replace('{PATH}', abspath)
    lines.append(cmd)
    script_name = f'{sys._getframe().f_code.co_name}.sh'
    _write_lines_to_file(lines, script_name)
    append_to_main_script(lines)

def run_powder_mod_main():
    relpath = os.path.join('tests', 'functional_tests', 'pycfml', 'test__powder_mod.py')
    abspath = os.path.join(_project_path(), relpath)
    lines = []
    msg = _echo_msg(f"Running powder_mod main from '{relpath}'")
    lines.append(msg)
    cmd = CONFIG['template']['run-python']
    cmd = cmd.replace('{PATH}', abspath)
    cmd = cmd.replace('{OPTIONS}', '')
    lines.append(cmd)
    script_name = f'{sys._getframe().f_code.co_name}.sh'
    _write_lines_to_file(lines, script_name)
    append_to_main_script(lines)

def run_pycfml_functional_tests_no_benchmarks():
    relpath = os.path.join('tests', 'functional_tests', 'pyCFML')
    abspath = os.path.join(_project_path(), relpath)
    lines = []
    msg = _echo_msg(f"Running tests from '{relpath}'")
    lines.append(msg)
    cmd = CONFIG['template']['run-tests']
    cmd = cmd.replace('{PATH}', abspath)
    lines.append(cmd)
    script_name = f'{sys._getframe().f_code.co_name}.sh'
    _write_lines_to_file(lines, script_name)
    append_to_main_script(lines)

def run_pycfml_functional_tests_with_benchmarks_save():
    project_name = CONFIG['pycfml']['log-name']
    relpath = os.path.join('tests', 'functional_tests', 'pyCFML')
    abspath = os.path.join(_project_path(), relpath)
    lines = []
    msg = _echo_msg(f"Running functional tests with benchmarks from '{relpath}' and saving results")
    lines.append(msg)
    cmd = CONFIG['template']['run-benchmarks']['base'] + ' ' + CONFIG['template']['run-benchmarks']['save']
    cmd = cmd.replace('{PATH}', abspath)
    cmd = cmd.replace('{PROJECT}', project_name)
    if _github_actions():
        cmd = cmd.replace('{RUNNER}', 'github')
    else:
        cmd = cmd.replace('{RUNNER}', 'local')
    cmd = cmd.replace('{COMPILER}', _compiler_name())
    cmd = cmd.replace('{PROCESSOR}', _processor())
    lines.append(cmd)
    script_name = f'{sys._getframe().f_code.co_name}.sh'
    _write_lines_to_file(lines, script_name)
    #append_to_main_script(lines)

def run_pycfml_functional_tests_with_benchmarks_compare():
    project_name = CONFIG['pycfml']['log-name']
    relpath = os.path.join('tests', 'functional_tests', 'pyCFML')
    abspath = os.path.join(_project_path(), relpath)
    lines = []
    msg = _echo_msg(f"Running functional tests with benchmarks from '{relpath}' and comparing with previously saved results")
    lines.append(msg)
    cmd = CONFIG['template']['run-benchmarks']['base'] + ' ' + CONFIG['template']['run-benchmarks']['compare']
    cmd = cmd.replace('{PATH}', abspath)
    cmd = cmd.replace('{PROJECT}', project_name)
    if _github_actions():
        cmd = cmd.replace('{RUNNER}', 'github')
    else:
        cmd = cmd.replace('{RUNNER}', 'local')
    cmd = cmd.replace('{COMPILER}', _compiler_name())
    cmd = cmd.replace('{PROCESSOR}', _processor())
    lines.append(cmd)
    script_name = f'{sys._getframe().f_code.co_name}.sh'
    _write_lines_to_file(lines, script_name)
    append_to_main_script(lines)


if __name__ == '__main__':
    ARGS = parsed_args()
    PYPROJECT = loaded_pyproject()
    CONFIG = loaded_config('pybuild.toml')

    if ARGS.print_wheel_dir:  # NEED FIX. Maybe save extras to toml as in EDA?
        _print_wheel_dir()
        exit(0)

    if ARGS.print_release_version:  # NEED FIX. Maybe save extras to toml as in EDA?
        _print_release_version()
        exit(0)

    if ARGS.print_release_title:  # NEED FIX. Maybe save extras to toml as in EDA?
        _print_release_title()
        exit(0)

    if not ARGS.create_scripts:  # NEED FIX. Need proper check if create scripts or print flags are given
        _print_error_msg('Incorrect set of command line arguments')
        exit(1)

    CFML = CONFIG['cfml']['log-name']
    pyCFML = CONFIG['pycfml']['log-name']

    #############
    # Preparation
    #############

    clear_main_script()

    add_main_script_header(f"Print some build-specific variables")
    print_build_variables()

    add_main_script_header(f"Create scripts, {CFML} and {pyCFML} directories")
    create_cfml_repo_dir()
    create_cfml_build_dir()
    create_cfml_dist_dir()
    create_pycfml_src_dir()
    create_pycfml_build_dir()
    create_pycfml_dist_dir()

    ############
    # Build CFML
    ############

    add_main_script_header(f"Download {CFML} repository")
    download_cfml_repo()

    add_main_script_header(f"Build {CFML} modules")
    rename_global_deps_file()
    build_cfml_modules_obj()
    delete_renamed_global_deps_file()

    add_main_script_header(f"Build {CFML} static library")
    build_cfml_static_lib()

    add_main_script_header(f"Make {CFML} distribution")
    move_built_to_cfml_dist()

    add_main_script_header(f"Create and run {CFML} test programs")
    build_cfml_test_programs()
    #copy_cfml_test_programs_to_tests_dir()
    ####run_cfml_functional_tests_no_benchmarks()
    #run_cfml_functional_tests_with_benchmarks()

    ##############
    # Build pyCFML
    ##############

    add_main_script_header(f"Create {pyCFML} source code")
    create_pycfml_src()

    add_main_script_header(f"Build {pyCFML} modules")
    build_pycfml_modules_obj()

    add_main_script_header(f"Build {pyCFML} shared obj / dynamic library")
    build_pycfml_lib_obj()
    build_pycfml_shared_obj_or_dynamic_lib()

    add_main_script_header(f"Make {pyCFML} distribution")
    copy_built_to_pycfml_dist()
    change_runpath_for_built_pycfml()
    copy_extra_libs_to_pycfml_dist()
    copy_py_api_files_to_pycfml_dist()
    copy_init_file_to_pycfml_dist()
    copy_cfml_databases_to_pycfml_dist()

    add_main_script_header(f"Create Python package wheel of {pyCFML}")
    validate_pyproject_toml()
    create_pycfml_python_wheel()
    rename_pycfml_python_wheel()
    detect_abi3_violations()
    check_wheel_contents()

    add_main_script_header(f"Install {pyCFML} from Python package wheel")
    install_pycfml_from_wheel()

    add_main_script_header(f"Run {pyCFML} tests")
    run_pycfml_unit_tests()
    run_pycfml_functional_tests_no_benchmarks()
    run_pycfml_functional_tests_with_benchmarks_save()
    run_pycfml_functional_tests_with_benchmarks_compare()

    _print_msg(f'All scripts were successfully created in {_scripts_path()}')
