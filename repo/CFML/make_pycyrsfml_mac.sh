BUILD_DIR=../../build/api_gfortran_release
CRYSFML08_DIR=/Users/nebil/git/CrysFML2008/gfortran_release_nebil
PYTHON_DIR="/opt/homebrew/opt/python@3.12/Frameworks/Python.framework/Versions/3.12/lib/"
OPT="-c -cpp -std=f2008 -ffree-line-length-none -fPIC -fdec-math -fall-intrinsics -frecursive"
LIB_GFORTRAN=/opt/homebrew/opt/gcc/lib/gcc/current/libgfortran.5.dylib 
LIB_QUADMATH=/opt/homebrew/opt/gcc/lib/gcc/current/libquadmath.0.dylib
LIB_PYTHON=$PYTHON_DIR/libpython3.12.dylib

# Generate Fortran code
cd Scripts/PythonAPI/
python apigen.py --build=$BUILD_DIR

# Compile Fortran code
cd $BUILD_DIR
# CFML_Wraps
echo Compiling CFML_Wraps.f90
gfortran $OPT Fortran/CFML_Wraps.f90 -I$CRYSFML08_DIR/include
# Wraps_Atoms.f90
echo Compiling Wraps_Atoms.f90
gfortran $OPT Fortran/CFML_Wraps/Wraps_Atoms.f90 -I$CRYSFML08_DIR/include
# Wraps_Bonds_Tables.f90
echo Compiling Wraps_Bonds_Tables.f90
gfortran $OPT Fortran/CFML_Wraps/Wraps_Bonds_Tables.f90 -I$CRYSFML08_DIR/include
# Wraps_Strings.f90
echo Compiling Wraps_Strings.f90
gfortran $OPT Fortran/CFML_Wraps/Wraps_Strings.f90 -I$CRYSFML08_DIR/include
# Wraps_BckPeaks.f90
echo Compiling Wraps_BckPeaks.f90
gfortran $OPT Fortran/CFML_Wraps/Wraps_BckPeaks.f90 -I$CRYSFML08_DIR/include
# Wraps_Scattering_Tables.f90
echo Compiling Wraps_Scattering_Tables.f90
gfortran $OPT Fortran/CFML_Wraps/Wraps_Scattering_Tables.f90 -I$CRYSFML08_DIR/include
# Wraps_Structure_Factors.f90
echo Compiling Wraps_Structure_Factors.f90
gfortran $OPT Fortran/CFML_Wraps/Wraps_Structure_Factors.f90 -I$CRYSFML08_DIR/include
# Wraps_Geom.f90
echo Compiling Wraps_Geom.f90
gfortran $OPT Fortran/CFML_Wraps/Wraps_Geom.f90 -I$CRYSFML08_DIR/include
# Wraps_Metrics.f90
echo Compiling Wraps_Metrics.f90
gfortran $OPT Fortran/CFML_Wraps/Wraps_Metrics.f90 -I$CRYSFML08_DIR/include
# Wraps_Py_Utilities.f90
echo Compiling Wraps_Py_Utilities.f90
gfortran $OPT Fortran/CFML_Wraps/Wraps_Py_Utilities.f90 -I$CRYSFML08_DIR/include
# Wraps_ExtinCorr.f90
echo Compiling Wraps_ExtinCorr.f90
gfortran $OPT Fortran/CFML_Wraps/Wraps_ExtinCorr.f90 -I$CRYSFML08_DIR/include
# Wraps_Symmetry_Tables.f90
echo Compiling Wraps_Symmetry_Tables.f90
gfortran $OPT Fortran/CFML_Wraps/Wraps_Symmetry_Tables.f90 -I$CRYSFML08_DIR/include
# Wraps_DiffPatt.f90
echo Compiling Wraps_DiffPatt.f90
gfortran $OPT Fortran/CFML_Wraps/Wraps_DiffPatt.f90 -I$CRYSFML08_DIR/include
# Wraps_Utilities.f90
echo Compiling Wraps_Utilities.f90
gfortran $OPT Fortran/CFML_Wraps/Wraps_Utilities.f90 -I$CRYSFML08_DIR/include
# Wraps_IOForm.f90
echo Compiling Wraps_IOForm.f90
gfortran $OPT Fortran/CFML_Wraps/Wraps_IOForm.f90 -I$CRYSFML08_DIR/include
# Wraps_Molecules.f90
echo Compiling Wraps_Molecules.f90
gfortran $OPT Fortran/CFML_Wraps/Wraps_Molecules.f90 -I$CRYSFML08_DIR/include
# Wraps_Propagation_Vectors.f90
echo Compiling Wraps_Propagation_Vectors.f90
gfortran $OPT Fortran/CFML_Wraps/Wraps_Propagation_Vectors.f90 -I$CRYSFML08_DIR/include
# Wraps_ILL_Instrm_Data.f90
echo Compiling Wraps_ILL_Instrm_Data.f90
gfortran $OPT Fortran/CFML_Wraps/Wraps_ILL_Instrm_Data.f90 -I$CRYSFML08_DIR/include
# Wraps_Reflections.f90
echo Compiling Wraps_Reflections.f90
gfortran $OPT Fortran/CFML_Wraps/Wraps_Reflections.f90 -I$CRYSFML08_DIR/include
# Wraps_BVS_Tables.f90
echo Compiling Wraps_BVS_Tables.f90
gfortran $OPT Fortran/CFML_Wraps/Wraps_BVS_Tables.f90 -I$CRYSFML08_DIR/include
# Wraps_EnBVS.f90
echo Compiling Wraps_EnBVS.f90
gfortran $OPT Fortran/CFML_Wraps/Wraps_EnBVS.f90 -I$CRYSFML08_DIR/include
# Wraps_SXTAL_Geom.f90
echo Compiling Wraps_SXTAL_Geom.f90
gfortran $OPT Fortran/CFML_Wraps/Wraps_SXTAL_Geom.f90 -I$CRYSFML08_DIR/include
# Wraps_Simulated_Annealing.f90
echo Compiling Wraps_Simulated_Annealing.f90
gfortran $OPT Fortran/CFML_Wraps/Wraps_Simulated_Annealing.f90 -I$CRYSFML08_DIR/include
# Wraps_Profiles.f90
echo Compiling Wraps_Profiles.f90
gfortran $OPT Fortran/CFML_Wraps/Wraps_Profiles.f90 -I$CRYSFML08_DIR/include
# Wraps_Rational.f90
echo Compiling Wraps_Rational.f90
gfortran $OPT Fortran/CFML_Wraps/Wraps_Rational.f90 -I$CRYSFML08_DIR/include
# Wraps_gSpaceGroups.f90
echo Compiling Wraps_gSpaceGroups.f90
gfortran $OPT Fortran/CFML_Wraps/Wraps_gSpaceGroups.f90 -I$CRYSFML08_DIR/include
# Wraps_kvec_Symmetry.f90
echo Compiling Wraps_kvec_Symmetry.f90
gfortran $OPT Fortran/CFML_Wraps/Wraps_kvec_Symmetry.f90 -I$CRYSFML08_DIR/include
# crysfml08lib
echo Building crysfml08lib.so
gfortran $OPT Fortran/crysfml08lib.f90 -I$CRYSFML08_DIR/include
gfortran -fPIC -shared -o crysfml08lib.so *.o -L $CRYSFML08_DIR/lib -L $PYTHON_DIR -l CrysFML08 -l python3.12
##mv crysfml08lib.so $INSTALLATION_DIR/
rm *.o *.mod *.smod

# Build the python package
# pyCFML directory
if [ ! -d crysfml ]; then
    mkdir crysfml
fi
mv crysfml08lib.so crysfml/
cp Python/* crysfml/
cp $LIB_GFORTRAN crysfml/
cp $LIB_QUADMATH crysfml/
cp $LIB_PYTHON   crysfml/
# File setup.py
echo "from setuptools import setup, find_packages" > setup.py
echo "import os" >> setup.py
echo "setup(" >> setup.py
echo "        name='crysfml'," >> setup.py
echo "        version='1.0.0'," >> setup.py
echo "        description='Python wrapper of the CrysFML08 library'," >> setup.py
echo "        author='Nebil A. Katcho, Juan Rodriguez Carvajal'," >> setup.py
echo "        packages=find_packages()," >> setup.py
echo "        include_package_data=True," >> setup.py
echo "        install_requires=[" >> setup.py
echo "                'numpy'," >> setup.py
echo "        ]," >> setup.py
echo ")" >> setup.py
# File MANIFEST.in
echo "include crysfml/*dylib" > MANIFEST.in
echo "include crysfml/*so" >> MANIFEST.in
# Generate the wheel
python setup.py bdist_wheel
# Install
cd dist
pip install crysfml-1.0.0-py3-none-any.whl --force-reinstall --no-deps