@echo off
rem Setlocal EnableDelayedExpansion
rem ------------------------------------
rem ---- CrysFML for Intel Compiler ----
rem ---- JGP                   2019 ----
rem ------------------------------------
rem
rem ---- INIT ----
   (set _DEBUG=N)
   (set _WINTER=N)
   if [%TARGET_ARCH%]==[] (set TARGET_ARCH=ia32)
rem
rem ----
rem ---- Arguments ----
rem ----
:LOOP
    if [%1]==[debug]  (set _DEBUG=Y)
    if [%1]==[winter] (set _WINTER=Y)
    shift
    if not [%1]==[] goto LOOP
rem
rem ---- Options
rem
   if [%_DEBUG%]==[Y] (
      if [%TARGET_ARCH%]==[ia32] (set DIRECTORY=ifort_debug) else (set DIRECTORY=ifort64_debug)
      (set OPT0=/Od)
      (set OPT1=/debug:full /check /check:noarg_temp_created /traceback /nologo /CB)
   ) else (
      if [%TARGET_ARCH%]==[ia32] (set DIRECTORY=ifort) else (set DIRECTORY=ifort64)
      (set OPT0=/Od)
      (set OPT1=/O2)
   )
rem
rem   (set OPT2=/fpp /Qopt-report:0)
   (set OPT2=/Qopt-report:0)
   (set OPT3=)
   if [%_WINTER%]==[Y] (
      if [%TARGET_ARCH%]==[ia32] (
         (set OPT3=/I%WINTER%\lib.if8)
      ) else (
         (set OPT3=/I%WINTER%\lib.i64)
      )
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
   ifort /c CFML_GlobalDeps_Windows_IFOR.f90         /nologo %OPT1% %OPT2% /module:.\mod
rem
   echo .... Input / Output Messages
   if [%_WINTER%]==[Y] (
     ifort /c CFML_Messages_Win.f90                  /nologo %OPT1% %OPT2% %OPT3% /module:.\mod
   ) else (
     ifort /c CFML_Messages.f90                      /nologo %OPT1% %OPT2% /module:.\mod
   )
rem
rem   Submodules CFML_Mess
      cd .\CFML_Messages
      if [%_WINTER%]==[Y] (
        ifort /c Win_Err_Message.f90                 /nologo %OPT1% %OPT2% %OPT3% /module:..\mod
        ifort /c Win_Info_Message.f90                /nologo %OPT1% %OPT2% %OPT3% /module:..\mod
        ifort /c Win_Question_Message.f90            /nologo %OPT1% %OPT2% %OPT3% /module:..\mod
        ifort /c Win_Stop_Message.f90                /nologo %OPT1% %OPT2% %OPT3% /module:..\mod
        ifort /c Win_Warning_Message.f90             /nologo %OPT1% %OPT2% %OPT3% /module:..\mod
        ifort /c Win_Write_ScrollMsg.f90             /nologo %OPT1% %OPT2% %OPT3% /module:..\mod

      ) else (
        ifort /c Con_Err_Message.f90                 /nologo %OPT1% %OPT2%  /module:..\mod
        ifort /c Con_Info_Message.f90                /nologo %OPT1% %OPT2%  /module:..\mod
        ifort /c Con_Print_Message.f90               /nologo %OPT1% %OPT2%  /module:..\mod
        ifort /c Con_Wait_Message.f90                /nologo %OPT1% %OPT2%  /module:..\mod
        ifort /c Con_Write_ScrollMsg.f90             /nologo %OPT1% %OPT2%  /module:..\mod
      )
      move /y *.obj .. > nul
      cd ..
rem
rem
   echo .... Mathematics
   ifort /c CFML_Maths.f90                           /nologo %OPT1% %OPT2% /module:.\mod
   ifort /c CFML_FFT.f90                             /nologo %OPT1% %OPT2% /module:.\mod
   ifort /c CFML_Random.f90                          /nologo %OPT1% %OPT2% /module:.\mod
rem  The module below is not needed in ifort, but it is maintained here for compatibility with gfortran
   ifort /c CFML_Trigonometry.f90                    /nologo %OPT1% %OPT2% /module:.\mod
rem
rem   Submodules CFML_Maths
      cd .\CFML_Maths
      ifort /c Co_Prime.f90                          /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Co_Linear.f90                         /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Cross_Product.f90                     /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Debye.f90                             /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Determinant.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Diagonalize_SH.f90                    /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Diagonalize_GEN.f90                   /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Equal_Matrix.f90                      /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Equal_Vector.f90                      /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Erfc_Der.f90                          /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Factorial.f90                         /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Inverse_Matrix.f90                    /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c In_Limits.f90                         /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Is_Diagonal_Matrix.f90                /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Is_Null_Vector.f90                    /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Linear_Dependent.f90                  /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Locate.f90                            /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Lower_Triangular.f90                  /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c ManipVec.f90                          /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Mat_Cross.f90                         /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Modulo_Lat.f90                        /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Negligible.f90                        /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Norm.f90                              /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Outerprod.f90                         /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Pgcd.f90                              /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Points_In_Line2D.f90                  /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Poly_Legendre.f90                     /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Rank.f90                              /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Resolv_System.f90                     /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Rotation_Axes.f90                     /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c RowEchelon.f90                        /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Scalar.f90                            /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c SistCoord_Changes.f90                 /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Sort.f90                              /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Spher_Harm.f90                        /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Swap.f90                              /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Tensor_Product.f90                    /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Trace.f90                             /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Upper_Triangular.f90                  /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Vec_Length.f90                        /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Zbelong.f90                           /nologo %OPT1% %OPT2%  /module:..\mod
      move /y *.obj .. > nul
      cd ..
rem
rem   Submodules CFML_FFT
      cd .\CFML_FFT
      ifort /c FFT_Convol.f90                        /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c FFT_Gen.f90                           /nologo %OPT1% %OPT2%  /module:..\mod
      move /y *.obj .. > nul
      cd ..
rem
rem   Submodules CFML_Random
      cd .\CFML_Random
      ifort /c Random_Beta.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Random_Binomial.f90                   /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Random_Cauchy.f90                     /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Random_Gamma.f90                      /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Random_InvGauss.f90                   /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Random_Normal.f90                     /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Random_Poisson.f90                    /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Random_T.f90                          /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Random_VonMises.f90                   /nologo %OPT1% %OPT2%  /module:..\mod
      move /y *.obj .. > nul
      cd ..
rem
   echo .... Strings
   ifort /c CFML_Strings.f90                         /nologo %OPT1% %OPT2% /module:.\mod
rem
rem   Submodules CFML_Strings
      cd .\CFML_Strings
      ifort /c StringFullp.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c StringNum.f90                         /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c StringReadKey.f90                     /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c StringTools.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      move /y *.obj .. > nul
      cd ..
rem
   echo .... Rational arithmetics
   ifort /c CFML_Rational.f90                        /nologo %OPT1% %OPT2% /module:.\mod
rem
rem   Submodules CFML_Rational
      cd .\CFML_Rational
      ifort /c Assignment.f90                        /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Constructor.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Equal_rational.f90                    /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Generic.f90                           /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Is_Integer.f90                        /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Operator_add.f90                      /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Operator_eq.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Operator_divisor.f90                  /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Operator_ge.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Operator_gt.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Operator_le.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Operator_lt.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Operator_minus.f90                    /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Operator_multiply.f90                 /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Operator_neq.f90                      /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Overloads.f90                         /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Rat_RowEchelon.f90                    /nologo %OPT1% %OPT2%  /module:..\mod
      move /y *.obj .. > nul
      cd ..
rem
   echo .... Metrics
   ifort /c CFML_Metrics.f90                         /nologo %OPT1% %OPT2% /module:.\mod
rem
rem   Submodules CFML_Metrics
      cd .\CFML_Metrics
      ifort /c GenMetrics.f90                        /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c IORoutines.f90                        /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c NiggliCell.f90                        /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c ThConver.f90                          /nologo %OPT1% %OPT2%  /module:..\mod
      move /y *.obj .. > nul
      cd ..
rem
   echo .... Defining Tables
   ifort /c CFML_Scattering_Tables.f90               /nologo %OPT1% %OPT2% /module:.\mod
   ifort /c CFML_Bonds_Tables.f90                    /nologo %OPT1% %OPT2% /module:.\mod
   ifort /c CFML_Symmetry_Tables.f90                 /nologo %OPT0% %OPT2% /module:.\mod
   ifort /c CFML_BVS_Tables.f90                      /nologo %OPT0% %OPT2% /module:.\mod
   ifort /c CFML_Magnetic_Database.f90               /nologo %OPT0% %OPT2% /module:.\mod
   ifort /c CFML_SuperSpace_Database.f90             /nologo %OPT0% %OPT2% /module:.\mod
rem
rem   Submodules CFML_Tables
      cd .\CFML_Tables
      ifort /c Del_ScatterT.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Get_ScatterT.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Set_ScatterT.f90                       /nologo %OPT0% %OPT2%  /module:..\mod
rem
      ifort /c Del_BondsT.f90                         /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Get_BondsT.f90                         /nologo %OPT1% %OPT2%  /module:..\mod
   echo .... Compiling Set_BondsT
      ifort /c Set_BondsT.f90                         /nologo %OPT0% %OPT2%  /module:..\mod
   echo .... End Compiling Set_BondsT
rem
      ifort /c Del_SpgT.f90                           /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Get_SpgT.f90                           /nologo %OPT1% %OPT2%  /module:..\mod
   echo .... Compiling Get_SpgSymbols
      ifort /c Get_SpgSymbols.f90                     /nologo %OPT0% %OPT2%  /module:..\mod
   echo .... End Compiling Get_SpgSymbols
   echo .... Compiling Set_SpgT
      ifort /c Set_SpgT.f90                           /nologo %OPT0% %OPT2%  /module:..\mod
   echo .... End Compiling Set_SpgT
rem
      ifort /c Del_BVST.f90                           /nologo %OPT1% %OPT2%  /module:..\mod
   echo .... Compiling Set_BVST
      ifort /c Set_BVST.f90                           /nologo %OPT0% %OPT2%  /module:..\mod
   echo .... End Compiling Set_BVST
rem
      ifort /c Allocating_MagneticDBase.f90           /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Read_MagneticDBase.f90                 /nologo %OPT1% %OPT2%  /module:..\mod
rem
      ifort /c Allocating_SuperSpaceDBase.f90         /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Read_SSG_DBase.f90                     /nologo %OPT1% %OPT2%  /module:..\mod
rem
      move /y *.obj .. > nul
      cd ..
rem
   echo .... Symmetry / SpaceGroups
   ifort /c CFML_gSpaceGroups.f90                           /nologo %OPT1% %OPT2% /module:.\mod
rem
rem   Submodules CFML_gSpaceGroups
      cd .\CFML_gSpaceGroups
      ifort /c Init_Procedures.f90                    /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Is_InversionCentre.f90                 /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Is_LattCentring.f90                    /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Rational_RedTraslation.f90             /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Operator_Equal.f90                     /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Operator_Mult.f90                      /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Sort_Operator.f90                      /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Write_SpaceG.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Allocate_Opers.f90                     /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Allocate_SpaceG.f90                    /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Reorder_Oper.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Get_Symb_Mat.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Get_Symb_Oper.f90                      /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Get_Mat_Symb.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Get_Oper_Symb.f90                      /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Get_Dimension.f90                      /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Get_Mult_OPTable.f90                   /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Get_GenerStr.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c CheckGener.f90                         /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Get_Ops_Gener.f90                      /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Spg_Const_Str.f90                      /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Spg_Const_VGen.f90                     /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Get_Cosets.f90                         /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Get_SubGrp.f90                         /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Smallest_IntegralVec.f90               /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Get_LattType.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Get_OriginShift.f90                    /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Get_HM_Standard.f90                    /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Get_Rotations.f90                      /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Get_PseudoStdBase.f90                  /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Get_X_Matrix.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Get_Generators.f90                     /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Match_Shubnikov_Grp.f90                /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Identify_Groups.f90                    /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Get_LauePG.f90                         /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Get_Gener_Hall.f90                     /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Inverse_OP.f90                         /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Get_CrystalSys.f90                     /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Match_Spg3D.f90                        /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Set_SpaceG.f90                         /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c OnePrimeOp.f90                         /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Is_Antilattice.f90                     /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c ApplySO.f90                            /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Get_Orb_Stabilizer_Constr.f90          /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Symm_Symbols.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      move /y *.obj .. > nul
      cd ..
 rem
    echo .... Profiles
    ifort /c CFML_Profiles.f90                        /nologo %OPT1% %OPT2% /module:.\mod
rem
rem   Submodules CFML_Profiles
      cd .\CFML_Profiles
      ifort /c Init_ProfVal.f90                       /nologo %OPT0% %OPT2%  /module:..\mod
      ifort /c Profile_Hester.f90                     /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Profile_Gaussian.f90                   /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Profile_Lorentzian.f90                 /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Profile_PseudoVoigt.f90                /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c TOF_Carpenter.f90                      /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c TOF_Jorgensen.f90                      /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c TOF_Jorg_Vondreele.f90                 /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Profile_BacktoBack.f90                 /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Profile_Exponential.f90                /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Profile_Hat.f90                        /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Profile_IkedaCarpenter.f90             /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Profile_TCHpVoigt.f90                  /nologo %OPT1% %OPT2%  /module:..\mod
      move /y *.obj .. > nul
      cd ..
rem
   echo .... Diffraction Patterns
   ifort /c CFML_DiffPatt.f90                         /nologo %OPT1% %OPT2% /module:.\mod
rem
rem   Submodules CFML_DiffPatt
      cd .\CFML_DiffPatt
      ifort /c FWHM_Peak.f90                          /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c NoisyPoints.f90                        /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c BackgPatt.f90                          /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c ReadPatt_CIF.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c ReadPatt_FREE.f90                      /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c ReadPatt_XYSIG.f90                     /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c ReadPatt_TimeVar.f90                   /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c ReadPatt_GSAS.f90                      /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c ReadPatt_ILL.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c ReadPatt_LLB.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c ReadPatt_ISIS.f90                      /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c ReadPatt_NLS.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c ReadPatt_PSI.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c ReadPatt_PAN.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c ReadPatt_Socabim.f90                   /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Add_Patterns.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c ReadPatterns.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c WritePatterns.f90                      /nologo %OPT1% %OPT2%  /module:..\mod
      move /y *.obj .. > nul
      cd ..
rem
   echo .... Extintion corrections
   ifort /c CFML_ExtinCorr.f90                        /nologo %OPT1% %OPT2% /module:.\mod
rem
rem   Submodules CFML_ExtinCorr
      cd .\CFML_ExtinCorr
      ifort /c BeckerCoppens.f90                      /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c FlippingRatios.f90                     /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c ShelxCorr.f90                          /nologo %OPT1% %OPT2%  /module:..\mod
      move /y *.obj .. > nul
      cd ..
rem
   echo .... EoS Calculations
   ifort /c CFML_EoS.f90                              /nologo %OPT1% %OPT2% /module:.\mod
rem
rem   Submodules CFML_EoS
      cd .\CFML_EoS
      ifort /c AllocateEos.f90                        /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c AlphaCalc.f90                          /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Checks.f90                             /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Conlev.f90                             /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c DerivPartial.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c dKdTCalc.f90                           /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c EoSCalc.f90                            /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c FfCalc.f90                             /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Get_Bulk.f90                           /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Get_Pressure.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Get_Properties.f90                     /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Get_Tait.f90                           /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Get_Temperature.f90                    /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Get_Transition.f90                     /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Get_Volume.f90                         /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Gruneisen.f90                          /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Init_EoS.f90                           /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c K_Cal.f90                              /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c NormPressure.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Pthermal.f90                           /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c PVT_Table.f90                          /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Read_EoS.f90                           /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Set_EoS.f90                            /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Strain.f90                             /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Write_EoS.f90                          /nologo %OPT1% %OPT2%  /module:..\mod
      move /y *.obj .. > nul
      cd ..
rem
   echo .... Atoms procedures
   ifort /c CFML_Atoms.f90                            /nologo %OPT1% %OPT2% /module:.\mod
rem
rem   Submodules CFML_Atoms
      cd .\CFML_Atoms
      ifort /c Allocating_Atoms.f90                   /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c RW_Bin_Atmlist.f90                     /nologo %OPT1% %OPT2%  /module:..\mod
	  ifort /c Write_AtmList.f90                      /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c ExtendList.f90                         /nologo %OPT1% %OPT2%  /module:..\mod
      move /y *.obj .. > nul
      cd ..
rem
   echo .... Reflections procedures
   ifort /c CFML_Reflections.f90                      /nologo %OPT1% %OPT2% /module:.\mod
rem
rem   Submodules CFML_Reflections
      cd .\CFML_Reflections
      ifort /c H_Absent.f90                           /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c H_Equal.f90                            /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c H_Equiv.f90                            /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c H_Mult.f90                             /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c H_S.f90                                /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c MaxNum_Refl.f90                        /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c UnitaryVec.f90                         /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c AsymUnit_Refl.f90                      /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Generate_Refl.f90                      /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Refl_Conditions.f90                    /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Init_RefList.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c H_EquivList.f90                        /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Write_RefList.f90                      /nologo %OPT1% %OPT2%  /module:..\mod
      move /y *.obj .. > nul
      cd ..
rem
      echo .... Propagation vectors procedures
      ifort /c CFML_Propagk.f90                       /nologo %OPT1% %OPT2% /module:.\mod
rem
      echo .... I/O Formats procedures
      ifort /c CFML_IOForm.f90                        /nologo %OPT1% %OPT2% /module:.\mod
rem
rem    Submodules CFML_IOForm
      cd .\CFML_IOForm
      ifort /c Format_GEN.f90                         /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Format_SHX.f90                         /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Format_CIF.f90                         /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Format_CFL.f90                         /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Format_MCIF.f90                        /nologo %OPT1% %OPT2%  /module:..\mod
      move /y *.obj .. > nul
      cd ..
rem
      echo .... Maps/Percolation procedures
      ifort /c CFML_Maps.f90                          /nologo %OPT1% %OPT2% /module:.\mod
rem
rem    Submodules CFML_Maps
      cd .\CFML_Maps
      ifort /c MarchingCubes.f90                      /nologo %OPT1% %OPT2%  /module:..\mod
      ifort /c Maps.f90                               /nologo %OPT1% %OPT2%  /module:..\mod
      move /y *.obj .. > nul
      cd ..
      goto END
rem
rem rem
rem    echo .... Mathematical(II), Optimization, Tables, Patterns
rem rem
rem    ifort /c CFML_LSQ_TypeDef.f90                      /nologo %OPT1% %OPT2%
rem    ifort /c CFML_optimization.f90                     /nologo %OPT1% %OPT2%
rem    ifort /c CFML_optimization_lsq.f90                 /nologo %OPT1% %OPT2%
rem rem
rem    echo .... Bonds, Crystal Metrics, Symmetry, ILL_Instr
rem rem
rem    ifort /c CFML_ILL_Instrm_data.f90                  /nologo %OPT1% %OPT2%
rem rem
rem    echo .... Formats, Geometry, Molecules
rem rem
rem    ifort /c CFML_geom_calc.f90                        /nologo %OPT1% %OPT2%
rem    ifort /c CFML_molecules.f90                        /nologo %OPT1% %OPT2%
rem rem
rem    echo .... Extinction, Structure Factors, SXTAL geometry, Propag Vectors
rem rem
rem    ifort /c CFML_sfac.f90                            /nologo %OPT1% %OPT2%
rem    ifort /c CFML_sxtal_Geom.f90                      /nologo %OPT1% %OPT2%
rem    ifort /c CFML_propagk.f90                         /nologo %OPT1% %OPT2%
rem rem
rem    echo .... Maps, BVS, Energy Configurations
rem rem
rem    ifort /c CFML_Export_Vtk.f90                      /nologo %OPT1% %OPT2%
rem    ifort /c CFML_conf_calc.f90                       /nologo %OPT1% %OPT2%
rem rem
rem    echo .... Magnetic Symmetry, Simulated Annealing, Keywords Parser
rem rem
rem    ifort /c CFML_Magnetic_Groups.f90                 /nologo %OPT1% %OPT2%
rem    ifort /c CFML_magsymm.f90                         /nologo %OPT1% %OPT2%
rem    ifort /c CFML_optimization_san.f90                /nologo %OPT1% %OPT2% %OPT3%
rem    ifort /c CFML_refcodes.f90                        /nologo %OPT1% %OPT2%
rem rem
rem    echo .... Magnetic Structure Factors, Polarimetry
rem rem
rem    ifort /c CFML_msfac.f90                           /nologo %OPT1% %OPT2%
rem    ifort /c CFML_polar.f90                           /nologo %OPT1% %OPT2%
rem
:END
rem
   echo.
   echo Creating CrysFML Library
rem
   if [%_WINTER%]==[Y] (
     lib /out:wcrysfml.lib *.obj
   ) else (
     lib /out:crysfml.lib *.obj
   )
rem
   echo Creating IFORT directory
rem
   if not exist ..\%DIRECTORY% mkdir ..\%DIRECTORY%
   if [%_WINTER%]==[Y] (
     if exist ..\%DIRECTORY%\LibW08\ rmdir ..\%DIRECTORY%\LibW08\ /S /Q
     mkdir ..\%DIRECTORY%\LibW08\
     copy .\mod\*.mod ..\%DIRECTORY%\LibW08\. > nul
     copy .\mod\*.smod ..\%DIRECTORY%\LibW08\. > nul
     move *.lib ..\%DIRECTORY%\LibW08\. > nul
   ) else (
     if exist ..\%DIRECTORY%\LibC08\ rmdir ..\%DIRECTORY%\LibC08\ /S /Q
     mkdir ..\%DIRECTORY%\LibC08\
     copy .\mod\*.mod ..\%DIRECTORY%\LibC08\. > nul
     copy .\mod\*.smod ..\%DIRECTORY%\LibC08\. > nul
     move *.lib ..\%DIRECTORY%\LibC08\. > nul
   )
  del *.obj  *.lst *.bak *.pdb > nul
rem
:FIN
   echo.
   echo **---- End Compilation for CrysFML ----**
   echo.
rem
