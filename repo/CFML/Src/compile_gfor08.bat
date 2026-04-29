@echo off
rem Setlocal EnableDelayedExpansion
rem ---------------------------------------
rem ---- CrysFML for GFortran Compiler ----
rem ---- JGP                      2019 ----
rem ---------------------------------------
rem
rem ---- INIT ----
   (set _DEBUG=N)
   (set _VER=8.1)
rem
rem GFortran for 32bits
   (set OPTC=-m32)
rem
rem GFortran for 64bits
rem   (set OPTC=-m64)
rem
rem ----
rem ---- Arguments ----
rem ----
:LOOP
    if [%1]==[debug] (set _DEBUG=Y)
    if [%1]==[winter] (set _WINTER=Y)
    if [%1]==[32] (set OPTC=-m32)
    if [%1]==[64] (set OPTC=-m64)
    shift
    if not [%1]==[] goto LOOP
rem
rem ---- Options  (All warnings i gfortran -Wall)
rem
   if [%_DEBUG%]==[Y] (
      if [%OPTC%]==[-m32] (set DIRECTORY=gfortran_debug) else (set DIRECTORY=gfortran64_debug)
      (set OPT0=-O0 -std=f2008 -Wunused-variable -fdec-math -fbacktrace  -ffree-line-length-0 -fall-intrinsics)
      (set OPT1=-O0 -std=f2008 -Wunused-variable -fdec-math -fbacktrace  -ffree-line-length-0 -fall-intrinsics)
   ) else (
      if [%OPTC%]==[-m32] (set DIRECTORY=gfortran) else (set DIRECTORY=gfortran64)
      (set OPT0=-O0 -std=f2008 -ffree-line-length-0 -fdec-math -fall-intrinsics)
      (set OPT1=-O3 -std=f2008 -ffree-line-length-0 -fdec-math -fall-intrinsics)
   )
   (set OPT3=)
   if [%_WINTER%]==[Y] (
      if [%OPTC%]==[-m32] (set LIBFOR=lib.gnu32/%_VER%) else (set LIBFOR=lib.gnu64/%_VER%)
      (set OPT3=-I%WINTER%\%LIBFOR%)
   )
rem
   cd %CRYSFML%\Src08
   if not exist .\mod mkdir .\mod
rem
   echo.
   echo **-------------------------------------**
   echo **---- CrysFML: Start Compilation  ----**
   echo **-------------------------------------**
   echo Compiler Options
   echo OPT0:%OPT0%
   echo OPT1:%OPT1%
   echo OPT2:%OPT2%
   echo OPT3:%OPT3%
   echo.
rem
   echo .... Global Dependencies
   gfortran -c %OPTC% -J.\mod CFML_GlobalDeps_Windows_GFOR.f90         %OPT1%
rem
   echo .... Input / Output Messages
   if [%_WINTER%]==[Y] (
     gfortran -c %OPTC% -J.\mod CFML_Messages_Win.f90                  %OPT1% %OPT3%
   ) else (
     gfortran -c %OPTC% -J.\mod CFML_Messages.f90                      %OPT1%
   )
rem
rem   Submodules CFML_Mess
      cd .\CFML_Messages
      if [%_WINTER%]==[Y] (
        gfortran -c %OPTC%  -J..\mod Win_Err_Message.f90               %OPT1% %OPT3%
        gfortran -c %OPTC%  -J..\mod Win_Info_Message.f90              %OPT1% %OPT3%
        gfortran -c %OPTC%  -J..\mod Win_Question_Message.f90          %OPT1% %OPT3%
        gfortran -c %OPTC%  -J..\mod Win_Stop_Message.f90              %OPT1% %OPT3%
        gfortran -c %OPTC%  -J..\mod Win_Warning_Message.f90           %OPT1% %OPT3%
        gfortran -c %OPTC%  -J..\mod Win_Write_ScrollMsg.f90           %OPT1% %OPT3%

      ) else (
        gfortran -c %OPTC%  -J..\mod Con_Err_Message.f90               %OPT1%
        gfortran -c %OPTC%  -J..\mod Con_Info_Message.f90              %OPT1%
        gfortran -c %OPTC%  -J..\mod Con_Print_Message.f90             %OPT1%
        gfortran -c %OPTC%  -J..\mod Con_Wait_Message.f90              %OPT1%
        gfortran -c %OPTC%  -J..\mod Con_Write_ScrollMsg.f90           %OPT1%
      )
      move /y *.o .. > nul
      cd ..
rem
   echo .... Mathematics
   gfortran -c %OPTC%  -J.\mod CFML_Maths.f90                        %OPT1%
   gfortran -c %OPTC%  -J.\mod CFML_FFT.f90                          %OPT1%
   gfortran -c %OPTC%  -J.\mod CFML_Random.f90                       %OPT1%
   gfortran -c %OPTC%  -J.\mod CFML_Trigonometry.f90                 %OPT1%
rem
rem   Submodules CFML_Maths
      cd .\CFML_Maths
      gfortran -c %OPTC%  -J..\mod Co_Prime.f90                      %OPT1%
      gfortran -c %OPTC%  -J..\mod Co_Linear.f90                     %OPT1%
      gfortran -c %OPTC%  -J..\mod Cross_Product.f90                 %OPT1%
      gfortran -c %OPTC%  -J..\mod Debye.f90                         %OPT1%
      gfortran -c %OPTC%  -J..\mod Determinant.f90                   %OPT1%
      gfortran -c %OPTC%  -J..\mod Diagonalize_SH.f90                %OPT1%
      gfortran -c %OPTC%  -J..\mod Diagonalize_Gen.f90               %OPT1%
      gfortran -c %OPTC%  -J..\mod Equal_Matrix.f90                  %OPT1%
      gfortran -c %OPTC%  -J..\mod Equal_Vector.f90                  %OPT1%
      gfortran -c %OPTC%  -J..\mod Erfc_Der.f90                      %OPT1%
      gfortran -c %OPTC%  -J..\mod Factorial.f90                     %OPT1%
      gfortran -c %OPTC%  -J..\mod Inverse_Matrix.f90                %OPT1%
      gfortran -c %OPTC%  -J..\mod In_Limits.f90                     %OPT1%
      gfortran -c %OPTC%  -J..\mod Is_Diagonal_Matrix.f90            %OPT1%
      gfortran -c %OPTC%  -J..\mod Is_Null_Vector.f90                %OPT1%
      gfortran -c %OPTC%  -J..\mod Linear_Dependent.f90              %OPT1%
      gfortran -c %OPTC%  -J..\mod Locate.f90                        %OPT1%
      gfortran -c %OPTC%  -J..\mod Lower_Triangular.f90              %OPT1%
      gfortran -c %OPTC%  -J..\mod ManipVec.f90                      %OPT1%
      gfortran -c %OPTC%  -J..\mod Mat_Cross.f90                     %OPT1%
      gfortran -c %OPTC%  -J..\mod Modulo_Lat.f90                    %OPT1%
      gfortran -c %OPTC%  -J..\mod Negligible.f90                    %OPT1%
      gfortran -c %OPTC%  -J..\mod Norm.f90                          %OPT1%
      gfortran -c %OPTC%  -J..\mod Outerprod.f90                     %OPT1%
      gfortran -c %OPTC%  -J..\mod Pgcd.f90                          %OPT1%
      gfortran -c %OPTC%  -J..\mod Points_In_Line2D.f90              %OPT1%
      gfortran -c %OPTC%  -J..\mod Poly_Legendre.f90                 %OPT1%
      gfortran -c %OPTC%  -J..\mod Rank.f90                          %OPT1%
      gfortran -c %OPTC%  -J..\mod Resolv_System.f90                 %OPT1%
      gfortran -c %OPTC%  -J..\mod Rotation_Axes.f90                 %OPT1%
      gfortran -c %OPTC%  -J..\mod RowEchelon.f90                    %OPT1%
      gfortran -c %OPTC%  -J..\mod Scalar.f90                        %OPT1%
      gfortran -c %OPTC%  -J..\mod SistCoord_Changes.f90             %OPT1%
      gfortran -c %OPTC%  -J..\mod Sort.f90                          %OPT1%
      gfortran -c %OPTC%  -J..\mod Spher_Harm.f90                    %OPT1%
      gfortran -c %OPTC%  -J..\mod Swap.f90                          %OPT1%
      gfortran -c %OPTC%  -J..\mod Tensor_Product.f90                %OPT1%
      gfortran -c %OPTC%  -J..\mod Trace.f90                         %OPT1%
      gfortran -c %OPTC%  -J..\mod Upper_Triangular.f90              %OPT1%
      gfortran -c %OPTC%  -J..\mod Vec_Length.f90                    %OPT1%
      gfortran -c %OPTC%  -J..\mod Zbelong.f90                       %OPT1%
      move /y *.o .. > nul
      cd ..
rem
rem   Submodules CFML_FFT
      cd .\CFML_FFT
      gfortran -c %OPTC%  -J..\mod FFT_Convol.f90                    %OPT1%
      gfortran -c %OPTC%  -J..\mod FFT_Gen.f90                       %OPT1%
      move /y *.o .. > nul
      cd ..
rem   Submodules CFML_Random
      cd .\CFML_Random
      gfortran -c %OPTC%  -J..\mod Random_Beta.f90                   %OPT1%
      gfortran -c %OPTC%  -J..\mod Random_Binomial.f90               %OPT1%
      gfortran -c %OPTC%  -J..\mod Random_Cauchy.f90                 %OPT1%
      gfortran -c %OPTC%  -J..\mod Random_Gamma.f90                  %OPT1%
      gfortran -c %OPTC%  -J..\mod Random_InvGauss.f90               %OPT1%
      gfortran -c %OPTC%  -J..\mod Random_Normal.f90                 %OPT1%
      gfortran -c %OPTC%  -J..\mod Random_Poisson.f90                %OPT1%
      gfortran -c %OPTC%  -J..\mod Random_T.f90                      %OPT1%
      gfortran -c %OPTC%  -J..\mod Random_VonMises.f90               %OPT1%
      move /y *.o .. > nul
      cd ..

rem
   echo .... Strings
   gfortran -c %OPTC%  -J.\mod  CFML_Strings.f90                 %OPT1%
rem
rem   Submodules CFML_Strings
      cd .\CFML_Strings

      gfortran -c %OPTC%  -J..\mod StringTools.f90                   %OPT1%
      gfortran -c %OPTC%  -J..\mod StringNum.f90                     %OPT1%
      gfortran -c %OPTC%  -J..\mod StringReadKey.f90                 %OPT1%
      gfortran -c %OPTC%  -J..\mod StringFullp.f90                   %OPT1%
      move /y *.o .. > nul
      cd ..
rem
   echo .... Rational arithmetics
   gfortran -c %OPTC%  -J.\mod CFML_Rational.f90                %OPT1%
rem
rem   Submodules CFML_Rational
      cd .\CFML_Rational
      gfortran -c %OPTC% -J..\mod Assignment.f90                     %OPT1%
      gfortran -c %OPTC% -J..\mod Constructor.f90                    %OPT1%
      gfortran -c %OPTC% -J..\mod Equal_rational.f90                 %OPT1%
      gfortran -c %OPTC% -J..\mod Generic.f90                        %OPT1%
      gfortran -c %OPTC% -J..\mod Is_Integer.f90                     %OPT1%
      gfortran -c %OPTC% -J..\mod Operator_add.f90                   %OPT1%
      gfortran -c %OPTC% -J..\mod Operator_eq.f90                    %OPT1%
      gfortran -c %OPTC% -J..\mod Operator_divisor.f90               %OPT1%
      gfortran -c %OPTC% -J..\mod Operator_ge.f90                    %OPT1%
      gfortran -c %OPTC% -J..\mod Operator_gt.f90                    %OPT1%
      gfortran -c %OPTC% -J..\mod Operator_le.f90                    %OPT1%
      gfortran -c %OPTC% -J..\mod Operator_lt.f90                    %OPT1%
      gfortran -c %OPTC% -J..\mod Operator_minus.f90                 %OPT1%
      gfortran -c %OPTC% -J..\mod Operator_multiply.f90              %OPT1%
      gfortran -c %OPTC% -J..\mod Operator_neq.f90                   %OPT1%
      gfortran -c %OPTC% -J..\mod Overloads.f90                      %OPT1%
      gfortran -c %OPTC% -J..\mod Rat_RowEchelon.f90                 %OPT1%
      move /y *.o .. > nul
      cd ..
rem
   echo .... Metrics
   gfortran -c %OPTC%  -J.\mod CFML_Metrics.f90                %OPT1%
rem
rem   Submodules CFML_Rational
      cd .\CFML_Metrics
      gfortran -c %OPTC% -J..\mod GenMetrics.f90                     %OPT1%
      gfortran -c %OPTC% -J..\mod IORoutines.f90                     %OPT1%
      gfortran -c %OPTC% -J..\mod NiggliCell.f90                     %OPT1%
      gfortran -c %OPTC% -J..\mod ThConver.f90                       %OPT1%
      move /y *.o .. > nul
      cd ..
rem
   echo .... Defining Tables
   gfortran -c %OPTC% -J.\mod CFML_Scattering_Tables.f90             %OPT1%
   gfortran -c %OPTC% -J.\mod CFML_Bonds_Tables.f90                  %OPT1%
   gfortran -c %OPTC% -J.\mod CFML_Symmetry_Tables.f90               %OPT0%
   gfortran -c %OPTC% -J.\mod CFML_BVS_Tables.f90                    %OPT1%
   gfortran -c %OPTC% -J.\mod CFML_Magnetic_Database.f90             %OPT1%
   gfortran -c %OPTC% -J.\mod CFML_SuperSpace_Database.f90           %OPT1%
rem
rem   Submodules CFML_Tables
      cd .\CFML_Tables
      gfortran -c %OPTC% -J..\mod Del_ScatterT.f90                   %OPT1%
      gfortran -c %OPTC% -J..\mod Get_ScatterT.f90                   %OPT1%
      gfortran -c %OPTC% -J..\mod Set_ScatterT.f90                   %OPT0%
rem
      gfortran -c %OPTC% -J..\mod Del_BondsT.f90                     %OPT1%
      gfortran -c %OPTC% -J..\mod Get_BondsT.f90                     %OPT1%
      gfortran -c %OPTC% -J..\mod Set_BondsT.f90                     %OPT0%
rem
      gfortran -c %OPTC% -J..\mod Del_SpgT.f90                       %OPT1%
      gfortran -c %OPTC% -J..\mod Get_SpgT.f90                       %OPT1%
      gfortran -c %OPTC% -J..\mod Get_SpgSymbols.f90                 %OPT0%
      gfortran -c %OPTC% -J..\mod Set_SpgT.f90                       %OPT0%
rem
      gfortran -c %OPTC% -J..\mod Del_BVST.f90                       %OPT1%
      gfortran -c %OPTC% -J..\mod Set_BVST.f90                       %OPT0%
rem
      gfortran -c %OPTC% -J..\mod Allocating_MagneticDBase.f90       %OPT0%
      gfortran -c %OPTC% -J..\mod Read_MagneticDBase.f90             %OPT1%
rem
      gfortran -c %OPTC% -J..\mod Allocating_SuperSpaceDBase.f90     %OPT0%
      gfortran -c %OPTC% -J..\mod Read_SSG_DBase.f90                 %OPT1%
rem
      move /y *.o .. > nul
      cd ..
rem
   echo .... Symmetry / SpaceGroups
   gfortran -c %OPTC%  -J.\mod CFML_gSpaceGroups.f90                 %OPT1%
rem
rem   Submodules CFML_SpaceG
      cd .\CFML_gSpaceGroups
      gfortran -c %OPTC% -J..\mod Init_Procedures.f90                %OPT1%
      gfortran -c %OPTC% -J..\mod Is_InversionCentre.f90             %OPT1%
      gfortran -c %OPTC% -J..\mod Is_LattCentring.f90                %OPT1%
      gfortran -c %OPTC% -J..\mod Rational_IsLattVec.f90             %OPT1%
      gfortran -c %OPTC% -J..\mod Rational_RedTraslation.f90         %OPT1%
      gfortran -c %OPTC% -J..\mod Operator_Equal.f90                 %OPT1%
      gfortran -c %OPTC% -J..\mod Operator_Mult.f90                  %OPT1%
      gfortran -c %OPTC% -J..\mod Sort_Operator.f90                  %OPT1%
      gfortran -c %OPTC% -J..\mod Write_SpaceG.f90                   %OPT1%
      gfortran -c %OPTC% -J..\mod Allocate_Opers.f90                 %OPT1%
      gfortran -c %OPTC% -J..\mod Allocate_SpaceG.f90                %OPT1%
      gfortran -c %OPTC% -J..\mod Reorder_Oper.f90                   %OPT1%
      gfortran -c %OPTC% -J..\mod Get_Symb_Mat.f90                   %OPT1%
      gfortran -c %OPTC% -J..\mod Get_Symb_Oper.f90                  %OPT1%
      gfortran -c %OPTC% -J..\mod Get_Mat_Symb.f90                   %OPT1%
      gfortran -c %OPTC% -J..\mod Get_Oper_Symb.f90                  %OPT1%
      gfortran -c %OPTC% -J..\mod Get_Dimension.f90                  %OPT1%
      gfortran -c %OPTC% -J..\mod Get_Mult_OPTable.f90               %OPT1%
      gfortran -c %OPTC% -J..\mod Get_GenerStr.f90                   %OPT1%
      gfortran -c %OPTC% -J..\mod CheckGener.f90                     %OPT1%
      gfortran -c %OPTC% -J..\mod Get_Ops_Gener.f90                  %OPT1%
      gfortran -c %OPTC% -J..\mod Spg_Const_Str.f90                  %OPT1%
      gfortran -c %OPTC% -J..\mod Spg_Const_VGen.f90                 %OPT1%
      gfortran -c %OPTC% -J..\mod Get_Cosets.f90                     %OPT1%
      gfortran -c %OPTC% -J..\mod Get_SubGrp.f90                     %OPT1%
      gfortran -c %OPTC% -J..\mod Smallest_IntegralVec.f90           %OPT1%
      gfortran -c %OPTC% -J..\mod Get_LattType.f90                   %OPT1%
      gfortran -c %OPTC% -J..\mod Get_OriginShift.f90                %OPT1%
      gfortran -c %OPTC% -J..\mod Get_HM_Standard.f90                %OPT1%
      gfortran -c %OPTC% -J..\mod Get_Rotations.f90                  %OPT1%
      gfortran -c %OPTC% -J..\mod Get_PseudoStdBase.f90              %OPT1%
      gfortran -c %OPTC% -J..\mod Get_X_Matrix.f90                   %OPT1%
      gfortran -c %OPTC% -J..\mod Get_Generators.f90                 %OPT1%
      gfortran -c %OPTC% -J..\mod Match_Shubnikov_Grp.f90            %OPT1%
      gfortran -c %OPTC% -J..\mod Identify_Groups.f90                %OPT1%
      gfortran -c %OPTC% -J..\mod Get_LauePg.f90                     %OPT1%
      gfortran -c %OPTC% -J..\mod Get_Gener_Hall.f90                 %OPT1%
      gfortran -c %OPTC% -J..\mod Inverse_OP.f90                     %OPT1%
      gfortran -c %OPTC% -J..\mod Get_CrystalSys.f90                 %OPT1%
      gfortran -c %OPTC% -J..\mod Match_Spg3D.f90                    %OPT1%
      gfortran -c %OPTC% -J..\mod Set_SpaceG.f90                     %OPT1%
      gfortran -c %OPTC% -J..\mod OnePrimeOp.f90                     %OPT1%
      gfortran -c %OPTC% -J..\mod Is_Antilattice.f90                 %OPT1%
      gfortran -c %OPTC% -J..\mod ApplySO.f90                        %OPT1%
      gfortran -c %OPTC% -J..\mod Get_Orb_Stabilizer_Constr.f90      %OPT1%
      gfortran -c %OPTC% -J..\mod Symm_Symbols.f90                   %OPT1%
      move /y *.o .. > nul
      cd ..
rem
   echo .... Profiles
   gfortran -c %OPTC%  -J.\mod CFML_Profiles.f90               %OPT1%
rem
rem   Submodules CFML_Profiles
      cd .\CFML_Profiles
      gfortran -c %OPTC% -J..\mod Init_ProfVal.f90                   %OPT0%
      gfortran -c %OPTC% -J..\mod Profile_Hester.f90                 %OPT1%
      gfortran -c %OPTC% -J..\mod Profile_Gaussian.f90               %OPT1%
      gfortran -c %OPTC% -J..\mod Profile_Lorentzian.f90             %OPT1%
      gfortran -c %OPTC% -J..\mod Profile_PseudoVoigt.f90            %OPT1%
      gfortran -c %OPTC% -J..\mod TOF_Carpenter.f90                  %OPT1%
      gfortran -c %OPTC% -J..\mod TOF_Jorgensen.f90                  %OPT1%
      gfortran -c %OPTC% -J..\mod TOF_Jorg_Vondreele.f90             %OPT1%
      gfortran -c %OPTC% -J..\mod Profile_BacktoBack.f90             %OPT1%
      gfortran -c %OPTC% -J..\mod Profile_Exponential.f90            %OPT1%
      gfortran -c %OPTC% -J..\mod Profile_Hat.f90                    %OPT1%
      gfortran -c %OPTC% -J..\mod Profile_IkedaCarpenter.f90         %OPT1%
      gfortran -c %OPTC% -J..\mod Profile_TCHpVoigt.f90              %OPT1%
      move /y *.o .. > nul
      cd ..
rem
   echo .... Diffraction Patterns
   gfortran -c %OPTC%  -J.\mod CFML_DiffPatt.f90                %OPT1%
rem
rem   Submodules CFML_DiffPatt
      cd .\CFML_DiffPatt
      gfortran -c %OPTC% -J..\mod FWHM_Peak.f90                     %OPT1%
      gfortran -c %OPTC% -J..\mod NoisyPoints.f90                   %OPT1%
      gfortran -c %OPTC% -J..\mod BackgPatt.f90                     %OPT1%
      gfortran -c %OPTC% -J..\mod ReadPatt_CIF.f90                  %OPT1%
      gfortran -c %OPTC% -J..\mod ReadPatt_FREE.f90                 %OPT1%
      gfortran -c %OPTC% -J..\mod ReadPatt_XYSIG.f90                %OPT1%
      gfortran -c %OPTC% -J..\mod ReadPatt_TimeVar.f90              %OPT1%
      gfortran -c %OPTC% -J..\mod ReadPatt_GSAS.f90                 %OPT1%
      gfortran -c %OPTC% -J..\mod ReadPatt_ILL.f90                  %OPT1%
      gfortran -c %OPTC% -J..\mod ReadPatt_LLB.f90                  %OPT1%
      gfortran -c %OPTC% -J..\mod ReadPatt_ISIS.f90                 %OPT1%
      gfortran -c %OPTC% -J..\mod ReadPatt_NLS.f90                  %OPT1%
      gfortran -c %OPTC% -J..\mod ReadPatt_PSI.f90                  %OPT1%
      gfortran -c %OPTC% -J..\mod ReadPatt_PAN.f90                  %OPT1%
      gfortran -c %OPTC% -J..\mod ReadPatt_Socabim.f90              %OPT1%
      gfortran -c %OPTC% -J..\mod Add_Patterns.f90                  %OPT1%
      gfortran -c %OPTC% -J..\mod ReadPatterns.f90                  %OPT1%
      gfortran -c %OPTC% -J..\mod WritePatterns.f90                 %OPT1%
      move /y *.o .. > nul
      cd ..
rem
   echo .... Extintion corrections
   gfortran -c %OPTC%  -J.\mod CFML_ExtinCorr.f90               %OPT1%
rem
rem   Submodules CFML_ExtinCorr
      cd .\CFML_ExtinCorr
      gfortran -c %OPTC% -J..\mod BeckerCoppens.f90                  %OPT1%
      gfortran -c %OPTC% -J..\mod FlippingRatios.f90                 %OPT1%
      gfortran -c %OPTC% -J..\mod ShelxCorr.f90                      %OPT1%
      move /y *.o .. > nul
      cd ..
rem
   echo .... EoS Calculations
   gfortran -c %OPTC%  -J.\mod CFML_EoS.f90                     %OPT1%
rem
rem   Submodules CFML_EoS
      cd .\CFML_EoS
      gfortran -c %OPTC%  -J..\mod AllocateEos.f90                   %OPT1%
      gfortran -c %OPTC%  -J..\mod AlphaCalc.f90                     %OPT1%
      gfortran -c %OPTC%  -J..\mod Checks.f90                        %OPT1%
      gfortran -c %OPTC%  -J..\mod Conlev.f90                        %OPT1%
      gfortran -c %OPTC%  -J..\mod DerivPartial.f90                  %OPT1%
      gfortran -c %OPTC%  -J..\mod dKdTCalc.f90                      %OPT1%
      gfortran -c %OPTC%  -J..\mod EoSCalc.f90                       %OPT1%
      gfortran -c %OPTC%  -J..\mod FfCalc.f90                        %OPT1%
      gfortran -c %OPTC%  -J..\mod Get_Bulk.f90                      %OPT1%
      gfortran -c %OPTC%  -J..\mod Get_Pressure.f90                  %OPT1%
      gfortran -c %OPTC%  -J..\mod Get_Properties.f90                %OPT1%
      gfortran -c %OPTC%  -J..\mod Get_Tait.f90                      %OPT1%
      gfortran -c %OPTC%  -J..\mod Get_Temperature.f90               %OPT1%
      gfortran -c %OPTC%  -J..\mod Get_Transition.f90                %OPT1%
      gfortran -c %OPTC%  -J..\mod Get_Volume.f90                    %OPT1%
      gfortran -c %OPTC%  -J..\mod Gruneisen.f90                     %OPT1%
      gfortran -c %OPTC%  -J..\mod Init_EoS.f90                      %OPT1%
      gfortran -c %OPTC%  -J..\mod K_Cal.f90                         %OPT1%
      gfortran -c %OPTC%  -J..\mod NormPressure.f90                  %OPT1%
      gfortran -c %OPTC%  -J..\mod Pthermal.f90                      %OPT1%
      gfortran -c %OPTC%  -J..\mod PVT_Table.f90                     %OPT1%
      gfortran -c %OPTC%  -J..\mod Read_EoS.f90                      %OPT1%
      gfortran -c %OPTC%  -J..\mod Set_EoS.f90                       %OPT1%
      gfortran -c %OPTC%  -J..\mod Strain.f90                        %OPT1%
      gfortran -c %OPTC%  -J..\mod Write_EoS.f90                     %OPT1%
      move /y *.o .. > nul
      cd ..
rem
   echo .... Atoms procedures
   gfortran -c %OPTC%  -J.\mod CFML_Atoms.f90                     %OPT1%
rem
rem   Submodules CFML_Atoms
      cd .\CFML_Atoms
      gfortran -c %OPTC% -J..\mod Allocating_Atoms.f90               %OPT1%
      gfortran -c %OPTC% -J..\mod RW_Bin_Atmlist.f90                 %OPT1%
      gfortran -c %OPTC% -J..\mod Write_AtmList.f90                  %OPT1%
      gfortran -c %OPTC% -J..\mod ExtendList.f90                     %OPT1%
      move /y *.o .. > nul
      cd ..
rem
   echo .... Reflections procedures
   gfortran -c %OPTC%  -J.\mod CFML_Reflections.f90              %OPT1%
rem
rem   Submodules CFML_Reflections
      cd .\CFML_Reflections
      gfortran -c %OPTC% -J..\mod H_Absent.f90                       %OPT1%
      gfortran -c %OPTC% -J..\mod H_Equal.f90                        %OPT1%
      gfortran -c %OPTC% -J..\mod H_Equiv.f90                        %OPT1%
      gfortran -c %OPTC% -J..\mod H_Mult.f90                         %OPT1%
      gfortran -c %OPTC% -J..\mod H_S.f90                            %OPT1%
      gfortran -c %OPTC% -J..\mod MaxNum_Refl.f90                    %OPT1%
      gfortran -c %OPTC% -J..\mod UnitaryVec.f90                     %OPT1%
      gfortran -c %OPTC% -J..\mod AsymUnit_Refl.f90                  %OPT1%
      gfortran -c %OPTC% -J..\mod Generate_Refl.f90                  %OPT1%
      gfortran -c %OPTC% -J..\mod Refl_Conditions.f90                %OPT1%
      gfortran -c %OPTC% -J..\mod Init_RefList.f90                   %OPT1%
      gfortran -c %OPTC% -J..\mod H_EquivList.f90                    %OPT1%
      gfortran -c %OPTC% -J..\mod Write_RefList.f90                  %OPT1%
      move /y *.o .. > nul
      cd ..
rem
      echo .... Propagation vectors procedures
      gfortran -c %OPTC%  -J.\mod CFML_Propagk.f90                    %OPT1%
rem
      echo .... I/O Formats procedures
      gfortran -c %OPTC%  -J.\mod CFML_IOForm.f90                    %OPT1%
rem
rem   Submodules CFML_IOForm
      cd .\CFML_IOForm
      gfortran -c %OPTC% -J..\mod Format_GEN.f90                     %OPT1%
      gfortran -c %OPTC% -J..\mod Format_SHX.f90                     %OPT1%
      gfortran -c %OPTC% -J..\mod Format_CIF.f90                     %OPT1%
      gfortran -c %OPTC% -J..\mod Format_CFL.f90                     %OPT1%
      gfortran -c %OPTC% -J..\mod Format_MCIF.f90                    %OPT1%
      move /y *.o .. > nul
      cd ..
      goto END

:END
rem
   echo.
   echo Creating CrysFML Library
rem
   if [%_WINTER%]==[Y] (
     ar cr libwcrysfml.a *.o
   ) else (
     ar cr libcrysfml.a *.o
   )
rem
   echo Creating GFORT directory
rem
   if not exist ..\%DIRECTORY% mkdir ..\%DIRECTORY%
   if [%_WINTER%]==[Y] (
     if exist ..\%DIRECTORY%\LibW08\ rmdir ..\%DIRECTORY%\LibW08\ /S /Q
     mkdir ..\%DIRECTORY%\LibW08\
     copy .\mod\*.mod ..\%DIRECTORY%\LibW08\. > nul
     copy .\mod\*.smod ..\%DIRECTORY%\LibW08\. > nul
     move *.a ..\%DIRECTORY%\LibW08\. > nul
   ) else (
     if exist ..\%DIRECTORY%\LibC08\ rmdir ..\%DIRECTORY%\LibC08\ /S /Q
     mkdir ..\%DIRECTORY%\LibC08\
     copy .\mod\*.mod ..\%DIRECTORY%\LibC08\.> nul
     copy .\mod\*.smod ..\%DIRECTORY%\LibC08\. > nul
     move *.a ..\%DIRECTORY%\LibC08\. > nul
   )
   del *.o  *.lst *.bak > nul
rem
   echo.
   echo **---- End Compilation for CrysFML ----**
   echo.
rem




