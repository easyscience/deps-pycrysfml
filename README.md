## About

This is a repository for creating a python library **pycrysfml** based on the new automatically generated Python API for the crystallographic library CrysFML2008 (Fortran 2008) from https://code.ill.fr/scientific-software/CrysFML2008.

## CI status

### Workflow for testing the build process: `debug-build-and-test`

#### Python version: 3.11

| Platform / Compiler       | gfortran | ifx                | ifort             | nagfor       |
| ------------------------- | -------- |--------------------|-------------------|--------------|
| Windows 10                | ✅       | ❌ [Issue #23](https://github.com/EasyScience/PyCrysFML/issues/23) | ✅                 | ⚙️ Testing   |
| Ubuntu 22.04              | ✅       | ✅                  | ✅                 | ⚙️ Testing   |
| macOS 12 (Intel)          | ✅       | Unsupported        | ❌ [Issue #22](https://github.com/EasyScience/PyCrysFML/issues/22) | ⚙️ Testing   |
| macOS 14 (Apple Silicon)  | ✅       | Unsupported        | Unsupported       | ⚙️ Testing   |

### Workflow for building a python release package: `release-build-and-test`

#### Fortran compiler: gfortran

| Platform / Python         | 3.10 | 3.11 | 3.12 |
| ------------------------- | ---- | ---- | ---- |
| Windows 10                | ✅   | ✅   | ✅   |
| Ubuntu 22.04              | ✅   | ✅   | ✅   |
| macOS 12 (Intel)          | ✅   | ✅   | ✅   |
| macOS 14 (Apple Silicon)  | ✅   | ✅   | ✅   |

## Installing

### Standard installation

The pycrysfml python package is currently in beta testing, with pre-releases being published on our own PyPi server. To test it, please install it using the PIP package manager with additional option as shown below.

* Create and activate a python environment (_optional_)

  ***macOS and Linux***

  ```
  python3.11 -m venv .venv
  source .venv/bin/activate
  ```

  ***Windows***

  ```
  python3.11 -m venv .venv
  .venv\Scripts\activate
  ```

* Upgrade the package installer for Python (_optional_)

  ```
  pip install --upgrade pip
  ```

* Install pycrysfml using a link to our own online repository

  ```
  pip install pycrysfml --extra-index-url https://easyscience.github.io/pypi/
  ```

### Installation from source

If the standard installation doesn't work for you, try building pycrysfml locally and installing it from the generated Python wheel.

* Create and activate a python environment (_optional_)

  ```
  python3 -m venv .venv
  . .venv/bin/activate          # macOS and Linux
  . .venv/Scripts/activate      # Windows with Unix like shells
  .\.venv\Scripts\activate.bat  # Windows with CMD
  .\.venv\Scripts\activate.ps1  # Windows with Power shell
  ```
  
* Exit and remove the virtual environment (_if no longer needed_)

  ```
  deactivate
  rm -rf .venv       # macOS and Linux
  rmdir /s /q .venv  # Windows
  ```

* Upgrade the package installer for Python (_optional_)

  ```
  pip install --upgrade pip
  ```

* Install Python dependences, including extras (ci and test)

  ```
  pip install '.[ci,test]'
  ```

* Print possible options for creating job scripts (_optional_)

  ```
  python pybuild.py --help
  ```

* Create job scripts with default options

  ```
  python pybuild.py --create-scripts
  ```

* Alternative: Create job scripts some options specified 

  ```
  python pybuild.py --create-scripts --platform macos --compiler gfortran --mode debug
  ```

* Print some build-specific variables (_optional_)

  ```
  scripts/print_build_variables.sh
  ```

* Create CFML and pyCFML directories

  ```
  scripts/create_cfml_repo_dir.sh
  scripts/create_cfml_build_dir.sh
  scripts/create_cfml_dist_dir.sh
  scripts/create_pycfml_src_dir.sh
  scripts/create_pycfml_build_dir.sh
  scripts/create_pycfml_dist_dir.sh
  ```

* Download CFML repository

  ```
  scripts/download_cfml_repo.sh
  ```

* Build CFML modules

  ```
  scripts/rename_global_deps_file.sh
  scripts/build_cfml_modules_obj.sh
  scripts/delete_renamed_global_deps_file.sh
  ```

* Build CFML static library

  ```
  scripts/build_cfml_static_lib.sh
  ```

* Make CFML distribution

  ```
  scripts/move_built_to_cfml_dist.sh
  ```

* Build und run CFML functional test programs

  ```
  scripts/build_cfml_test_programs.sh
  scripts/copy_cfml_test_programs_to_tests_dir.sh
  scripts/run_cfml_functional_tests_no_benchmarks.sh
  ```

* Create pyCFML source code

  ```
  scripts/create_pycfml_src.sh
  ```

* Build pyCFML modules

  ```
  scripts/build_pycfml_modules_obj.sh
  ```

* Build pyCFML shared obj / dynamic library

  ```
  scripts/build_pycfml_lib_obj.sh
  scripts/build_pycfml_shared_obj_or_dynamic_lib.sh
  ```

* Make pyCFML distribution

  ```
  scripts/copy_built_to_pycfml_dist.sh
  scripts/change_runpath_for_built_pycfml.sh
  scripts/copy_extra_libs_to_pycfml_dist.sh
  scripts/copy_py_api_files_to_pycfml_dist.sh
  scripts/copy_init_file_to_pycfml_dist.sh
  scripts/copy_cfml_databases_to_pycfml_dist.sh
  ```

* Create Python package wheel of pyCFML

  ```
  scripts/validate_pyproject_toml.sh
  scripts/create_pycfml_python_wheel.sh
  scripts/rename_pycfml_python_wheel.sh
  ```

* Install pyCFML from Python package wheel (with dev extras)

  ```
  scripts/install_pycfml_from_wheel.sh
  ```

* Run pyCFML unit tests

  ```
  scripts/run_pycfml_unit_tests.sh
  ```

* Run pyCFML functional tests

  ```
  scripts/run_pycfml_functional_tests_no_benchmarks.sh
  ```

## How to use

### Compute the powder diffraction pattern

Here is the simplest example of using pycrysfml to calculate a powder diffraction pattern from a Python dictionary, which contains a description of the crystallographic phase as well as parameters related to the instrument and experiment.

Install the matplotlib package used in the example below:

```
pip install matplotlib
```

Run this example [srtio3-pattern-simulation.py](examples/pyCFML/cfml_utilities/powder_pattern_from_json/srtio3-pattern-simulation.py) to see the simulated powder diffraction pattern:

```
python examples/pyCFML/cfml_utilities/powder_pattern_from_json/srtio3-pattern-simulation.py
```

More features will be added as the project develops.
