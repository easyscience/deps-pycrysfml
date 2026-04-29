@echo off

set CWD=%CD%
set INTEL=""
set LIBS=^
   %INTEL%\libmmd.dll^
   %INTEL%\libifcoremd.dll^
   %INTEL%\libiomp5md.dll^
   %INTEL%\svml_dispmd.dll

set CRYSFML08_INSTALL=""
cd %CRYSFML08_INSTALL%
mkdir build
cd build
mkdir crysfml
for %%F in (%LIBS%) do (
  echo Copiando %%F 
  copy %%F crysfml\
)
copy ..\crysfml\* crysfml\

rem # File setup.py
(
echo from setuptools import setup, find_packages
echo import os
echo setup^(
echo ^    name='crysfml',
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
echo include crysfml/*.pyd
echo include crysfml/*.dll
) > MANIFEST.in

rem # Generate the wheel
python setup.py bdist_wheel
cd dist
pip install crysfml-1.0.0-py3-none-any.whl --force-reinstall --no-deps
cd %CWD%