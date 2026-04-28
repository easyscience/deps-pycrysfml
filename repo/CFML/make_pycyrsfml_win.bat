@echo off

set CWD=%CD%
set BUILD_DIR=..\..\..\..\build\pycrysfml\ifort_release
set CRYSFML08_INSTALL=C:\Users\katcho\libs\crysfml08\ifort_release
set PYTHON_INSTALL=C:\Users\katcho\AppData\Local\Programs\Python\Python314
set OPT=/c /fpp /nologo /Qdiag-disable:10448 /libs:dll
set LIBS=^
   "C:\Program Files (x86)\Intel\oneAPI\compiler\2024.0\bin\libmmd.dll"^
   "C:\Program Files (x86)\Intel\oneAPI\compiler\2024.0\bin\libifcoremd.dll"^
   "C:\Program Files (x86)\Intel\oneAPI\compiler\2024.0\bin\libiomp5md.dll"^
   "C:\Program Files (x86)\Intel\oneAPI\compiler\2024.0\bin\svml_dispmd.dll"

rem Generate Fortran code
cd Scripts\PythonAPI
python apigen.py --build=%BUILD_DIR%

cd %BUILD_DIR%
rem CFML_Wraps
echo Compiling CFML_Wraps.f90
ifort %OPT% Fortran\CFML_Wraps.f90 /I%CRYSFML08_INSTALL%\include
rem Wraps_Atoms.f90
echo Compiling Wraps_Atoms.f90
ifort %OPT% Fortran\CFML_Wraps\Wraps_Atoms.f90 /I%CRYSFML08_INSTALL%\include
rem Wraps_BckPeaks.f90
echo Compiling Wraps_BckPeaks.f90
ifort %OPT% Fortran\CFML_Wraps\Wraps_BckPeaks.f90 /I%CRYSFML08_INSTALL%\include
rem Wraps_Bonds_Tables.f90
echo Compiling Wraps_Bonds_Tables.f90
ifort %OPT% Fortran\CFML_Wraps\Wraps_Bonds_Tables.f90 /I%CRYSFML08_INSTALL%\include
rem Wraps_BVS_Tables.f90
echo Compiling Wraps_BVS_Tables.f90
ifort %OPT% Fortran\CFML_Wraps\Wraps_BVS_Tables.f90 /I%CRYSFML08_INSTALL%\include
rem Wraps_DiffPatt.f90
echo Compiling Wraps_DiffPatt.f90
ifort %OPT% Fortran\CFML_Wraps\Wraps_DiffPatt.f90 /I%CRYSFML08_INSTALL%\include
rem Wraps_EnBVS.f90
echo Compiling Wraps_EnBVS.f90
ifort %OPT% Fortran\CFML_Wraps\Wraps_EnBVS.f90 /I%CRYSFML08_INSTALL%\include
rem Wraps_ExtinCorr.f90
echo Compiling Wraps_ExtinCorr.f90
ifort %OPT% Fortran\CFML_Wraps\Wraps_ExtinCorr.f90 /I%CRYSFML08_INSTALL%\include
rem Wraps_Geom.f90
echo Compiling Wraps_Geom.f90
ifort %OPT% Fortran\CFML_Wraps\Wraps_Geom.f90 /I%CRYSFML08_INSTALL%\include
rem Wraps_gSpaceGroups.f90
echo Compiling Wraps_gSpaceGroups.f90
ifort %OPT% Fortran\CFML_Wraps\Wraps_gSpaceGroups.f90 /I%CRYSFML08_INSTALL%\include
rem Wraps_ILL_Instrm_Data.f90
echo Compiling Wraps_ILL_Instrm_Data.f90
ifort %OPT% Fortran\CFML_Wraps\Wraps_ILL_Instrm_Data.f90 /I%CRYSFML08_INSTALL%\include
rem Wraps_IOForm.f90
echo Compiling Wraps_IOForm.f90
ifort %OPT% Fortran\CFML_Wraps\Wraps_IOForm.f90 /I%CRYSFML08_INSTALL%\include
rem Wraps_kvec_Symmetry.f90
echo Compiling Wraps_kvec_Symmetry.f90
ifort %OPT% Fortran\CFML_Wraps\Wraps_kvec_Symmetry.f90 /I%CRYSFML08_INSTALL%\include
rem Wraps_Metrics.f90
echo Compiling Wraps_Metrics.f90
ifort %OPT% Fortran\CFML_Wraps\Wraps_Metrics.f90 /I%CRYSFML08_INSTALL%\include
rem Wraps_Molecules.f90
echo Compiling Wraps_Molecules.f90
ifort %OPT% Fortran\CFML_Wraps\Wraps_Molecules.f90 /I%CRYSFML08_INSTALL%\include
rem Wraps_Profiles.f90
echo Compiling Wraps_Profiles.f90
ifort %OPT% Fortran\CFML_Wraps\Wraps_Profiles.f90 /I%CRYSFML08_INSTALL%\include
rem Wraps_Propagation_Vectors.f90
echo Compiling Wraps_Propagation_Vectors.f90
ifort %OPT% Fortran\CFML_Wraps\Wraps_Propagation_Vectors.f90 /I%CRYSFML08_INSTALL%\include
rem Wraps_Py_Utilities.f90
echo Compiling Wraps_Py_Utilities.f90
ifort %OPT% Fortran\CFML_Wraps\Wraps_Py_Utilities.f90 /I%CRYSFML08_INSTALL%\include
rem Wraps_Rational.f90
echo Compiling Wraps_Rational.f90
ifort %OPT% Fortran\CFML_Wraps\Wraps_Rational.f90 /I%CRYSFML08_INSTALL%\include
rem Wraps_Reflections.f90
echo Compiling Wraps_Reflections.f90
ifort %OPT% Fortran\CFML_Wraps\Wraps_Reflections.f90 /I%CRYSFML08_INSTALL%\include
rem Wraps_Scattering_Tables.f90
echo Compiling Wraps_Scattering_Tables.f90
ifort %OPT% Fortran\CFML_Wraps\Wraps_Scattering_Tables.f90 /I%CRYSFML08_INSTALL%\include
rem Wraps_Simulated_Annealing.f90
echo Compiling Wraps_Simulated_Annealing.f90
ifort %OPT% Fortran\CFML_Wraps\Wraps_Simulated_Annealing.f90 /I%CRYSFML08_INSTALL%\include
rem Wraps_Strings.f90
echo Compiling Wraps_Strings.f90
ifort %OPT% Fortran\CFML_Wraps\Wraps_Strings.f90 /I%CRYSFML08_INSTALL%\include
rem Wraps_Structure_Factors.f90
echo Compiling Wraps_Structure_Factors.f90
ifort %OPT% Fortran\CFML_Wraps\Wraps_Structure_Factors.f90 /I%CRYSFML08_INSTALL%\include
rem Wraps_SXTAL_Geom.f90
echo Compiling Wraps_SXTAL_Geom.f90
ifort %OPT% Fortran\CFML_Wraps\Wraps_SXTAL_Geom.f90 /I%CRYSFML08_INSTALL%\include
rem Wraps_Symmetry_Tables.f90
echo Compiling Wraps_Symmetry_Tables.f90
ifort %OPT% Fortran\CFML_Wraps\Wraps_Symmetry_Tables.f90 /I%CRYSFML08_INSTALL%\include
rem Wraps_Utilities.f90
echo Compiling Wraps_Utilities.f90
ifort %OPT% Fortran\CFML_Wraps\Wraps_Utilities.f90 /I%CRYSFML08_INSTALL%\include

rem crysfml08lib
echo Building crysfml08lib.pyd
ifort %OPT% Fortran\crysfml08lib.f90 /I%CRYSFML08_INSTALL%\include
link *.obj /out:crysfml08lib.dll /libpath:%CRYSFML08_INSTALL%\lib /dll /libpath:%PYTHON_INSTALL%\libs CrysFML08.lib python314.lib
rem /FORCE:MULTIPLE

del *.obj *.mod *.smod *.exp *.lib

rem # Build the python package
rem # PyCrysFML directory
if not exist pycrysfml (
    mkdir pycrysfml
)
move crysfml08lib.dll pycrysfml\crysfml08lib.pyd
copy Python\* pycrysfml\
for %%F in (%LIBS%) do (
    echo Copiando %%F a %DEST%
    copy %%F pycrysfml\
)

rem # File setup.py
(
echo from setuptools import setup, find_packages
echo import os
echo setup^(
echo ^    name='pycrysfml',
echo ^    version='1.0.0',
echo ^    description='Python wrapper of the CrysFML08 library',
echo ^    author='Nebil A. Katcho, Juan Rodriguez Carvajal',
echo ^    packages=find_packages^(^),
echo ^    include_package_data=True,
echo ^    install_requires=[
echo ^        'numpy',
echo ^    ],
echo ^)
) > setup.py

rem # File MANIFEST.in
(
echo include pycrysfml/*.pyd
echo include pycrysfml/*.dll
) > MANIFEST.in

rem # Generate the wheel
python setup.py bdist_wheel
cd dist
pip install pycrysfml-1.0.0-py3-none-any.whl --force-reinstall --no-deps
cd %CWD%