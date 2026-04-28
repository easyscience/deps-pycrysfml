ifort  -c Prep_Input.f90         -I$CRYSFML/ifort64/LibC
ifort  -c Cost_MagFunctions.f90  -I$CRYSFML/ifort64/LibC
ifort  -c MagOptim.f90           -I$CRYSFML/ifort64/LibC
ifort   *.o -o MagOptim -L$CRYSFML/ifort64/LibC -lcrysfml 
rm *.o *.mod
