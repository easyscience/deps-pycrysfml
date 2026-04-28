compiler=$1
echo "Building CrysFML with ${compiler##*/}"

rm -Rf build_${compiler##*/}
mkdir build_${compiler##*/}
cd build_${compiler##*/}

#cmake -D ARCH32=OFF -D CMAKE_BUILD_TYPE=Debug -D CMAKE_Fortran_COMPILER=${compiler} -D USE_HDF=ON -D HDF5_INCLUDE_PATH=${HDF5_INCLUDE_PATH} -D HDF5_LIBRARY_PATH=${HDF5_LIBRARY_PATH} ..
cmake -D ARCH32=OFF -D CMAKE_BUILD_TYPE=Debug -D CMAKE_Fortran_COMPILER=${compiler} -D USE_HDF=OFF ..
cmake --build .

ctest
status=$?
if [ $status -ne 0 ]; then
	echo "Failure/Error during ctest"
	exit $status
fi
