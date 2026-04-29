# This script replaces the sequences Module Pure and Module Elemental
# by Pure Module and Elemental Module in every fortran file of the 
# project. It must be run from a unix terminal.

files=`ls *.f90`
dirs=`ls -d */ | grep CFML`

# Edit files in Src08/ 
for file in $files
do
    echo "Processing $file"
    sed -i 's/Module Pure/Pure Module/' $file
    sed -i 's/Module Elemental/Elemental Module/' $file
    sed -i 's/Module Recursive/Recursive Module/' $file
done

# Edit files in submodules
for dir in $dirs
do
    cd $dir
    files=`ls *.f90` 
    for file in $files
    do
        echo "Processing $dir$file"
        sed -i 's/Module Pure/Pure Module/' $file
        sed -i 's/Module Elemental/Elemental Module/' $file
        sed -i 's/Module Recursive/Recursive Module/' $file
    done
    cd ..
done
