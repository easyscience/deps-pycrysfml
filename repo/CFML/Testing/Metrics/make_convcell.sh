#!/bin/sh
# Script to compile the Program: Get_Conv_Cell
#
if [ a$1 = a ]
then
cat << !
make_convcell.sh : Make the Get_Conv_Cell Program for Absoft/G95/GFortran/Intel/Lahey Compilers (Linux)
Syntax           : make_convcell.sh af95:g95:ifort:lf95:gfortran
!
exit
fi
#
# Compiler Name
#
COMP=$1
if [ $COMP != "lf95" ]
then
   if [ $COMP != "g95" ]
   then
     if [ $COMP != "gfortran" ]
     then
       if [ $COMP != "ifort" ]
       then
         if [ $COMP != "af95" ]
         then
            echo "Compiler Name was wrong!!!"
            exit
         fi
       fi
     fi
   fi
fi
DEBUG=nodeb
if [ $# -eq 2 ]
then
   DEBUG=$2
fi
#
# Compilation Options
#
if [ $DEBUG = "deb" ]
then
   case $COMP
   in
      'af95')
          OPT1="-c -g -O0"
            ;;
      'g95')
          OPT1="-c -g"
            ;;
      'gfortran')
          OPT1="-c -g"
            ;;
      'ifort')
          OPT1="-c -g"
            ;;
      'lf95')
          OPT1="-c -g"
             ;;
   esac
else
   case $COMP
   in
      'af95')
          OPT1="-c -O2 -N11"
            ;;
      'g95')
          OPT1="-c -O"
            ;;
      'gfortran')
          OPT1="-c -O"
            ;;
      'ifort')
          OPT1="-c -O -w -vec-report0"
            ;;
      'lf95')
          OPT1="-c -O --nchk --tpp"
             ;;
   esac
fi
#
# External Libraries Options
#
case $COMP
in
   'af95')
      INC="-I../../Absoft/LibC"
      LIB="-L../../Absoft/LibC"
      LIBSTATIC="-lcrysfml"
      LIBDYNAMIC="-lcrysfml"
     ;;
   'g95')
      INC="-I../../G95/LibC"
      LIB="-L../../G95/LibC"
      LIBSTATIC="-lcrysfml"
      LIBDYNAMIC="-lcrysfml"
      ;;
   'gfortran')
      INC="-I../../GFortran/LibC"
      LIB="-L../../GFortran/LibC"
      LIBSTATIC="-lcrysfml"
      LIBDYNAMIC="-lcrysfml"
     ;;
   'ifort')
      INC="-I../../ifort/LibC"
      LIB="-L../../ifort/LibC"
      LIBDYNAMIC="-lcrysfml"
      LIBSTATIC="-lcrysfml"
     ;;
   'lf95')
      INC="--mod .:../../Lahey/LibC"
      LIB="-L../../Lahey/LibC"
      LIBDYNAMIC="-lcrysfml"
      LIBSTATIC="-lcrysfml -lpthread"
     ;;
esac
#
# Compilation Process
#
$COMP $OPT1 Get_Conv_Cell.f90    $INC
case $COMP
in
  'af95')
     $COMP *.o  -o Get_Conv_Cell -static $LIB $LIBSTATIC
     ;;
  'g95')
     $COMP *.o  -o Get_Conv_Cell -static $LIB $LIBSTATIC
     ;;
  'gfortran')
     $COMP *.o  -o Get_Conv_Cell  $LIB $LIBSTATIC
     ;;
  'ifort')
     $COMP *.o -o Get_Conv_Cell -static-intel $LIB $LIBSTATIC
     ;;
  'lf95')
     $COMP *.o --out Get_Conv_Cell --staticlink $LIB $LIBDYNAMIC
     ;;
esac
rm -rf *.o *.mod
