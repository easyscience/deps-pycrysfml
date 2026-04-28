GFORTRAN="" # usually /opt/homebrew/opt/gcc/lib/gcc/current
LIBS="$GFORTRAN/libgfortran.5.dylib
      $GFORTRAN/libquadmath.0.dylib"
CRYSFML08_INSTALL=""
cd $CRYSFML08_INSTALL
mkdir build
cp -r pycrysfml build
cd build
cp $LIBS pycrysfml/
# File setup.py
echo "from setuptools import setup, find_packages" > setup.py
echo "import os" >> setup.py
echo "setup(" >> setup.py
echo "        name='pycrysfml'," >> setup.py
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
echo "include pycrysfml/*dylib" > MANIFEST.in
echo "include pycrysfml/*so" >> MANIFEST.in
# Generate the wheel
python setup.py bdist_wheel
cd dist
pip install pycrysfml-1.0.0-py3-none-any.whl --force-reinstall --no-deps
cd ../../
rm -rf build