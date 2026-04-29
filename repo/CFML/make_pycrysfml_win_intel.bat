@echo off
set CWD=%CD%
set INTEL="C:\Program Files (x86)\Intel\oneAPI\compiler\2024.0"
set LIBS=^
   %INTEL%\bin\libmmd.dll^
   %INTEL%\bin\libifcoremd.dll^
   %INTEL%\bin\libiomp5md.dll^
   %INTEL%\bin\svml_dispmd.dll
set CRYSFML08_INSTALL=%CRYSFML08%\ifort64\
cd %CRYSFML08_INSTALL%
mkdir build
copy crysfml build
cd build
mkdir crysfml
copy ..\crysfml\* crysfml\
for %%F in (%LIBS%) do (
    echo Copiando %%F 
    copy %%F crysfml\
)

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