SRC=../../
SRC_NXS=$CRYSFML08/HDF5
INCLUDE="-I$CRYSFML08_INSTALL/include -I$HDF5_INSTALL/mod/static"
OPT="-c -O3 -heap-arrays -Warn -nologo"

echo Compiling Nexus_Mod.f90...
ifort $SRC_NXS/Nexus_Mod.f90           $OPT $INCLUDE
echo Compiling racer_cfml08_mod.f90...
ifort $SRC/racer_cfml08_mod.f90        $OPT $INCLUDE
echo Compiling racer_cfml08_files.f90
ifort $SRC/racer_cfml08_files.f90      $OPT $INCLUDE
echo Compiling racer_cfml08.f90...
ifort $SRC/racer_cfml08.f90            $OPT $INCLUDE

echo Linking...
ifort *.o -o racernxs -static -L $HDF5_INSTALL/lib -L $CRYSFML08_INSTALL/lib -L $ZLIB_DIR -l CrysFML08 -l hdf5_fortran -l hdf5_f90cstub -l hdf5  -l z 

if [ ! -d ../../bin ]; then
    mkdir ../../bin
fi
mv racernxs ../../bin
rm *.o *.mod