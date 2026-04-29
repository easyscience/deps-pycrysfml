@echo off
rem  
rem ----------------------------------------
rem ---- CrysFML for Intel ifx Compiler ----
rem ---- JGP-JRC                   2022 ----
rem ----------------------------------------
rem
rem ---- INIT ----
   (set _DEBUG=N)
   (set _WINTER=N)
   if [%TARGET_ARCH%]==[] (set TARGET_ARCH=intel64)
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
      (set DIRECTORY=ifx_debug)
      (set OPT0=/Od)
      (set OPT1=/debug:full /check /check:noarg_temp_created /traceback /nologo /CB /Warn)
   ) else (
      (set DIRECTORY=ifx_release)
      (set OPT0=/Od)
      (set OPT1=/O2)
   )
rem
   (set OPT2=/Qopt-report:0)
   (set OPT3=)
   if [%_WINTER%]==[Y] (
     (set OPT3=/I%WINTER%\lib.i64)
   )
rem Create "mod" subdirectory withib Src
   cd %CRYSFML%\Src
   if not exist .\mod mkdir .\mod
rem
   echo.
   echo **-------------------------------------------------------------**
   echo **---- CrysFML: Start Compilation with oneAPI ifx compiler ----**
   echo **-------------------------------------------------------------**
   echo Options for ifx Compiler  
   echo OPT0:%OPT0%
   echo OPT1:%OPT1%
   echo OPT2:%OPT2%
   echo OPT3:%OPT3%
   echo.
rem
   echo .... Global Dependencies
   ifx /c CFML_GlobalDeps_Windows_IFOR.f90         /nologo %OPT1% %OPT2% /module:.\mod
rem
   echo .... Input / Output Messages
   if [%_WINTER%]==[Y] (
     ifx /c CFML_Messages_Win.f90                  /nologo %OPT1% %OPT2% %OPT3% /module:.\mod
   ) else (
     ifx /c CFML_Messages.f90                      /nologo %OPT1% %OPT2% /module:.\mod
   )
rem
rem   Submodules CFML_Mess
      cd .\CFML_Messages
      if [%_WINTER%]==[Y] (
        ifx /c Win_Err_Message.f90                 /nologo %OPT1% %OPT2% %OPT3% /module:..\mod
        ifx /c Win_Info_Message.f90                /nologo %OPT1% %OPT2% %OPT3% /module:..\mod
        ifx /c Win_Question_Message.f90            /nologo %OPT1% %OPT2% %OPT3% /module:..\mod
        ifx /c Win_Stop_Message.f90                /nologo %OPT1% %OPT2% %OPT3% /module:..\mod
        ifx /c Win_Warning_Message.f90             /nologo %OPT1% %OPT2% %OPT3% /module:..\mod
        ifx /c Win_Write_ScrollMsg.f90             /nologo %OPT1% %OPT2% %OPT3% /module:..\mod

      ) else (
        ifx /c Con_Err_Message.f90                 /nologo %OPT1% %OPT2%  /module:..\mod
        ifx /c Con_Info_Message.f90                /nologo %OPT1% %OPT2%  /module:..\mod
        ifx /c Con_Print_Message.f90               /nologo %OPT1% %OPT2%  /module:..\mod
        ifx /c Con_Wait_Message.f90                /nologo %OPT1% %OPT2%  /module:..\mod
        ifx /c Con_Write_ScrollMsg.f90             /nologo %OPT1% %OPT2%  /module:..\mod
      )
      move /y *.obj .. > nul
      cd ..
rem
rem
   echo .... Mathematics
   ifx /c CFML_Maths.f90                           /nologo %OPT1% %OPT2% /module:.\mod
   ifx /c CFML_FFT.f90                             /nologo %OPT1% %OPT2% /module:.\mod
   ifx /c CFML_Random.f90                          /nologo %OPT1% %OPT2% /module:.\mod
rem
rem   Submodules CFML_Maths
rem
      cd .\CFML_Maths
      ifx /c Math_Co_Prime.f90                          /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_Co_Linear.f90                         /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_Cross_Product.f90                     /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_Debye.f90                             /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_Determinant.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_Diagonalize_SH.f90                    /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_Diagonalize_GEN.f90                   /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_Equal_Matrix.f90                      /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_Equal_Vector.f90                      /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_Erfc_Der.f90                          /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_Factorial.f90                         /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_Inverse_Matrix.f90                    /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_In_Limits.f90                         /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_Is_Diagonal_Matrix.f90                /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_Is_Null_Vector.f90                    /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_Linear_Dependent.f90                  /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_Locate.f90                            /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_Lower_Triangular.f90                  /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_Mat_Cross.f90                         /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_Modulo_Lat.f90                        /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_Negligible.f90                        /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_Norm.f90                              /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_Outerprod.f90                         /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_Pgcd.f90                              /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_Points_In_Line2D.f90                  /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_Poly_Legendre.f90                     /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_PolynomialFit.f90                     /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_Polyhedron_Volume.f90                 /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_Rank.f90                              /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_Resolv_System.f90                     /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_Rotation_Axes.f90                     /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_RowEchelon.f90                        /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_Scalar.f90                            /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_SistCoord_Changes.f90                 /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_Smoothing_Interpol.f90                /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_Sort.f90                              /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_Spher_Harm.f90                        /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_Swap.f90                              /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_Tensor_Product.f90                    /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_Trace.f90                             /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_Upper_Triangular.f90                  /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Math_Zbelong.f90                           /nologo %OPT1% %OPT2%  /module:..\mod
      move /y *.obj .. > nul
      cd ..
rem
rem   Submodules CFML_FFT
      cd .\CFML_FFT
      ifx /c FFT_Convol.f90                        /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c FFT_Gen.f90                           /nologo %OPT1% %OPT2%  /module:..\mod
      move /y *.obj .. > nul
      cd ..
rem
rem   Submodules CFML_Random
      cd .\CFML_Random
      ifx /c Random_Beta_Sm.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Random_Binomial_Sm.f90                   /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Random_Cauchy_Sm.f90                     /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Random_Gamma_Sm.f90                      /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Random_InvGauss_Sm.f90                   /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Random_Normal_Sm.f90                     /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Random_Poisson_Sm.f90                    /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Random_T_Sm.f90                          /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Random_VonMises_Sm.f90                   /nologo %OPT1% %OPT2%  /module:..\mod
      move /y *.obj .. > nul
      cd ..
rem
   echo .... Strings
   ifx /c CFML_Strings.f90                         /nologo %OPT1% %OPT2% /module:.\mod
rem
rem   Submodules CFML_Strings
      cd .\CFML_Strings
      ifx /c StringFullp.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c StringNum.f90                         /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c StringReadKey.f90                     /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c StringTools.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      move /y *.obj .. > nul
      cd ..
rem
   echo .... Optimization 
   ifx /c CFML_Optimization.f90                 /nologo %OPT1% %OPT2%  /module:.\mod
rem   Submodules CFML_Optimization
      cd .\CFML_Optimization
      ifx /c OPT_Cg_Quasi_Newton.f90             /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c OPT_Global_Csendes.f90              /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c OPT_Local_Optim.f90                 /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c OPT_Simplex.f90                     /nologo %OPT1% %OPT2%  /module:..\mod
      move /y *.obj .. > nul
      cd ..
rem
   echo .... Optimization LSQ 
   ifx /c CFML_Optimization_LSQ.f90              /nologo %OPT1% %OPT2%  /module:.\mod
rem   Submodules CFML_Optimization_LSQ
      cd .\CFML_Optimization_LSQ
      ifx /c OPT_LSQ_LevebergMarquardt_AnalyDer.f90 /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c OPT_LSQ_LevebergMarquardt_NumDer.f90   /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c OPT_LSQ_Marquardt_Fit.f90              /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c OPT_LSQ_Output.f90                     /nologo %OPT1% %OPT2%  /module:..\mod
      move /y *.obj .. > nul
      cd ..
rem
   echo .... Simulated Annealing 
   ifx /c CFML_Simulated_Annealing.f90            /nologo %OPT1% %OPT2% /module:.\mod
rem   Submodules CFML_Simulated_Annealing
      cd .\CFML_Optimization_SAnn
      ifx /c SAnn_General.f90                     /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c SAnn_inout.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c SAnn_LocalOpt.f90                    /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c SAnn_MultiConf.f90                   /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c SAnn_SetnCheck.f90                   /nologo %OPT1% %OPT2%  /module:..\mod
      move /y *.obj .. > nul
      cd ..

rem
   echo .... Rational arithmetics
   ifx /c CFML_Rational.f90                        /nologo %OPT1% %OPT2% /module:.\mod
rem
rem   Submodules CFML_Rational
      cd .\CFML_Rational
      ifx /c Rat_Assignment.f90                        /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Rat_Constructor.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Rat_Equal_rational.f90                    /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Rat_Generic.f90                           /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Rat_Is_Integer.f90                        /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Rat_Operator_add.f90                      /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Rat_Operator_eq.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Rat_Operator_divisor.f90                  /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Rat_Operator_ge.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Rat_Operator_gt.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Rat_Operator_le.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Rat_Operator_lt.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Rat_Operator_minus.f90                    /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Rat_Operator_multiply.f90                 /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Rat_Operator_neq.f90                      /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Rat_Overloads.f90                         /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Rat_RowEchelon.f90                        /nologo %OPT1% %OPT2%  /module:..\mod
      move /y *.obj .. > nul
      cd ..
rem
   echo .... Metrics
   ifx /c CFML_Metrics.f90                         /nologo %OPT1% %OPT2% /module:.\mod
rem
rem   Submodules CFML_Metrics
      cd .\CFML_Metrics
      ifx /c Metrics_Gen.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Metrics_IO.f90                        /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Metrics_NiggliCell.f90                /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Metrics_ThConver.f90                  /nologo %OPT1% %OPT2%  /module:..\mod
      move /y *.obj .. > nul
      cd ..
rem
   echo .... Defining Tables
   ifx /c CFML_Scattering_Tables.f90               /nologo %OPT1% %OPT2% /module:.\mod
   ifx /c CFML_Bonds_Tables.f90                    /nologo %OPT1% %OPT2% /module:.\mod
   ifx /c CFML_BVS_Tables.f90                      /nologo %OPT0% %OPT2% /module:.\mod
   ifx /c CFML_Magnetic_Database.f90               /nologo %OPT0% %OPT2% /module:.\mod
   ifx /c CFML_SuperSpace_Database.f90             /nologo %OPT0% %OPT2% /module:.\mod
   ifx /c CFML_Symmetry_Tables.f90                 /nologo %OPT0% %OPT2% /module:.\mod
rem
rem   Submodules CFML_Tables
      cd .\CFML_Tables
      ifx /c Tab_Del_ScatterT.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Tab_Get_ScatterT.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Tab_Set_ScatterT.f90                       /nologo %OPT0% %OPT2%  /module:..\mod
rem
      ifx /c Tab_Del_BondsT.f90                         /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Tab_Get_BondsT.f90                         /nologo %OPT1% %OPT2%  /module:..\mod
   echo .... Compiling Tab_Set_BondsT
      ifx /c Tab_Set_BondsT.f90                         /nologo %OPT0% %OPT2%  /module:..\mod
   echo .... End Compiling Tab_Set_BondsT
rem
      ifx /c Tab_Del_SpgT.f90                           /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Tab_Get_SpgT.f90                           /nologo %OPT1% %OPT2%  /module:..\mod
   echo .... Compiling Tab_Get_SpgSymbols
      ifx /c Tab_Get_SpgSymbols.f90                     /nologo %OPT0% %OPT2%  /module:..\mod
   echo .... End Compiling Tab_Get_SpgSymbols
   echo .... Tab_Compiling Set_SpgT
      ifx /c Tab_Set_SpgT.f90                           /nologo %OPT0% %OPT2%  /module:..\mod
   echo .... End Compiling Tab_Set_SpgT
rem
      ifx /c Tab_Del_BVST.f90                           /nologo %OPT1% %OPT2%  /module:..\mod
   echo .... Compiling Tab_Set_BVST
      ifx /c Tab_Set_BVST.f90                           /nologo %OPT0% %OPT2%  /module:..\mod
   echo .... End Compiling Tab_Set_BVST
rem
      ifx /c Tab_Allocating_MagneticDBase.f90           /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Tab_Read_MagneticDBase.f90                 /nologo %OPT1% %OPT2%  /module:..\mod
rem
      ifx /c Tab_Allocating_SuperSpaceDBase.f90         /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Tab_Read_SSG_DBase.f90                     /nologo %OPT1% %OPT2%  /module:..\mod
rem
      move /y *.obj .. > nul
      cd ..
rem
   echo .... Symmetry / SpaceGroups
   ifx /c CFML_gSpaceGroups.f90                           /nologo %OPT1% %OPT2% /module:.\mod
rem
rem   Submodules CFML_gSpaceGroups
      cd .\CFML_gSpaceGroups
      ifx /c gS_Init_Procedures.f90                    /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Is_InversionCentre.f90                 /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Is_LattCentring.f90                    /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Rational_RedTraslation.f90             /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Operator_Equal.f90                     /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Operator_Mult.f90                      /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Sort_Operator.f90                      /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Write_SpaceG.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Allocate_Opers.f90                     /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Allocate_SpaceG.f90                    /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Reorder_Oper.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Get_Symb_Mat.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Get_Symb_Oper.f90                      /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Get_Mat_Symb.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Get_Oper_Symb.f90                      /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Get_Dimension.f90                      /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Get_Mult_OPTable.f90                   /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Get_GenerStr.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_CheckGener.f90                         /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Get_Ops_Gener.f90                      /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Spg_Const_Str.f90                      /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Spg_Const_VGen.f90                     /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Get_Cosets.f90                         /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Get_SubGrp.f90                         /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Smallest_IntegralVec.f90               /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Get_LattType.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Get_OriginShift.f90                    /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Get_HM_Standard.f90                    /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Get_Rotations.f90                      /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Get_PseudoStdBase.f90                  /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Get_X_Matrix.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Get_Generators.f90                     /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Match_Shubnikov_Grp.f90                /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Identify_Groups.f90                    /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Get_LauePG.f90                         /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Get_Hall_Gener.f90                     /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Inverse_OP.f90                         /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Get_CrystalSys.f90                     /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Match_Spg3D.f90                        /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Set_SpaceG.f90                         /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_OnePrimeOp.f90                         /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Is_Antilattice.f90                     /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_ApplySO.f90                            /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Get_Orb_Stabilizer_Constr.f90          /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c gS_Symm_Symbols.f90                       /nologo %OPT1% %OPT2%  /module:..\mod
      move /y *.obj .. > nul
      cd ..
 rem
    echo .... Profiles
    ifx /c CFML_Profiles.f90                        /nologo %OPT1% %OPT2% /module:.\mod
rem
rem   Submodules CFML_Profiles
      cd .\CFML_Profiles
      ifx /c Profile_BacktoBack.f90             /nologo %OPT0% %OPT2%  /module:..\mod
      ifx /c Profile_Exponential.f90            /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Profile_Finger.f90                 /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Profile_Gaussian.f90               /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Profile_Hat.f90                    /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Profile_IkedaCarpenter.f90         /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Profile_Init_ProfVal.f90           /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Profile_Lorentzian.f90             /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Profile_PseudoVoigt.f90            /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Profile_TCHpVoigt.f90              /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Profile_TOF_Carpenter.f90          /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Profile_TOF_Jorgensen.f90          /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Profile_TOF_Jorg_Vondreele.f90     /nologo %OPT1% %OPT2%  /module:..\mod
      move /y *.obj .. > nul
      cd ..
rem
   echo .... Diffraction Patterns
   ifx /c CFML_DiffPatt.f90                         /nologo %OPT1% %OPT2% /module:.\mod
rem
rem   Submodules CFML_DiffPatt
      cd .\CFML_DiffPatt
      ifx /c DiffP_Add_Patterns.f90             /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c DiffP_BackgPatt.f90                /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c DiffP_FWHM_Peak.f90                /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c DiffP_NoisyPoints.f90              /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c DiffP_ReadPatterns.f90             /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c DiffP_ReadPatt_CIF.f90             /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c DiffP_ReadPatt_FREE.f90            /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c DiffP_ReadPatt_GSAS.f90            /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c DiffP_ReadPatt_ILL.f90             /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c DiffP_ReadPatt_ISIS.f90            /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c DiffP_ReadPatt_LLB.f90             /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c DiffP_ReadPatt_NLS.f90             /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c DiffP_ReadPatt_PAN.f90             /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c DiffP_ReadPatt_PSI.f90             /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c DiffP_ReadPatt_Socabim.f90         /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c DiffP_ReadPatt_TimeVar.f90         /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c DiffP_ReadPatt_XYSIG.f90           /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c DiffP_WritePatterns.f90            /nologo %OPT1% %OPT2%  /module:..\mod
      move /y *.obj .. > nul
      cd ..
rem
   echo .... Background-Peak search
   ifx /c CFML_BckPeaks.f90                     /nologo %OPT1% %OPT2% /module:.\mod
rem
   echo .... Legacy ILL_Instrument procedures (CFML_ILL_Instrm_Data)
     ifx /c CFML_ILL_Instrm_Data.f90            /nologo %OPT1% %OPT2%  /module:.\mod   
rem
   echo .... Extintion corrections
   ifx /c CFML_ExtinCorr.f90                    /nologo %OPT1% %OPT2% /module:.\mod
rem
rem   Submodules CFML_ExtinCorr
      cd .\CFML_ExtinCorr
      ifx /c Ext_BeckerCoppens.f90              /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Ext_FlippingRatios.f90             /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Ext_ShelxCorr.f90                  /nologo %OPT1% %OPT2%  /module:..\mod
      move /y *.obj .. > nul
      cd ..
rem
   echo .... EoS Calculations
   ifx /c CFML_EoS.f90                          /nologo %OPT1% %OPT2% /module:.\mod
rem
rem   Submodules CFML_EoS
      cd .\CFML_EoS
      ifx /c Eos_Init.f90                 /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Eos_Allocate.f90             /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Eos_AlphaCalc.f90            /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c EoS_Calc.f90                 /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c EoS_CellPar.f90              /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Eos_Checks.f90               /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Eos_Conlev.f90               /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c EoS_CopyEDat.f90             /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Eos_DerivPartial.f90         /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Eos_dKdTCalc.f90             /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Eos_FfCalc.f90               /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c EoS_Get_Angle.f90            /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c EoS_Get_APL.f90              /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Eos_Get_Bulk.f90             /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c EoS_Get_HeatCap.f90          /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Eos_Get_Pressure.f90         /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Eos_Get_Properties.f90       /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Eos_Get_Tait.f90             /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Eos_Get_Temperature.f90      /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c EoS_Get_Tensor.f90           /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Eos_Get_Transition.f90       /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Eos_Get_Volume.f90           /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Eos_Gruneisen.f90            /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Eos_K_Cal.f90                /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c EoS_LinEoS_Allowed.f90       /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c EoS_ModDir.f90               /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Eos_NormPressure.f90         /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c EoS_PrincipalEoS.f90         /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Eos_Pthermal.f90             /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Eos_PVT_Table.f90            /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Eos_Read.f90                 /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Eos_Set.f90                  /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Eos_Strain.f90               /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c EoS_Transform_ESD.f90        /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Eos_Write.f90                /nologo %OPT1% %OPT2%  /module:..\mod        
      move /y *.obj .. > nul
      cd ..
rem
   echo .... Atoms procedures
   ifx /c CFML_Atoms.f90                  /nologo %OPT1% %OPT2% /module:.\mod
rem
rem   Submodules CFML_Atoms
      cd .\CFML_Atoms
      ifx /c Atm_Allocating_Atoms.f90     /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Atm_ExtendList.f90           /nologo %OPT1% %OPT2%  /module:..\mod
	  ifx /c Atm_RW_Bin_AtmList.f90       /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Atm_SymmetryConstraints.f90  /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Atm_Write_AtmList.f90        /nologo %OPT1% %OPT2%  /module:..\mod
      move /y *.obj .. > nul
      cd ..
rem
   echo .... Export to VTK procedures
     ifx /c CFML_Export_VTK.f90           /nologo %OPT1% %OPT2%  /module:.\mod
   echo .... Reflections procedures
     ifx /c CFML_Reflections.f90          /nologo %OPT1% %OPT2% /module:.\mod
rem
rem   Submodules CFML_Reflections
      cd .\CFML_Reflections
      ifx /c Refl_AsymUnit.f90          /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Refl_Conditions.f90        /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Refl_Generate.f90          /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Refl_H_Absent.f90          /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Refl_H_Equal.f90           /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Refl_H_Equiv.f90           /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Refl_H_EquivList.f90       /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Refl_H_Mult.f90            /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Refl_H_S.f90               /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Refl_Init_RefList.f90      /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Refl_MaxNum.f90            /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Refl_UnitaryVec.f90        /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Refl_Write_List.f90        /nologo %OPT1% %OPT2%  /module:..\mod
      move /y *.obj .. > nul
      cd ..
rem
      echo .... Propagation vectors procedures
      ifx /c CFML_Propagation_Vectors.f90  /nologo %OPT1% %OPT2% /module:.\mod
rem
      echo .... I/O Formats procedures
      ifx /c CFML_IOForm.f90               /nologo %OPT1% %OPT2% /module:.\mod
rem
rem    Submodules CFML_IOForm
      cd .\CFML_IOForm
      ifx /c Format_GEN.f90                /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Format_SHX.f90                /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Format_CIF.f90                /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Format_CFL.f90                /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Format_MCIF.f90               /nologo %OPT1% %OPT2%  /module:..\mod
      move /y *.obj .. > nul
      cd ..
rem
      echo .... Geometry calculations CFML_Geom
rem
      ifx /c CFML_Geom.f90              /nologo %OPT1% %OPT2%  /module:.\mod
      cd .\CFML_Geom
      ifx /c Geom_Allocations.f90       /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Geom_Angles.f90            /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Geom_Coordination.f90      /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Geom_Distances.f90         /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Geom_Matrices.f90          /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Geom_Orbits.f90            /nologo %OPT1% %OPT2%  /module:..\mod
      move /y *.obj .. > nul
      cd ..

rem
      echo .... SXTAL Geometry calculations (CFML_Geometry_SXTAL)
      ifx /c CFML_SXTAL_Geom.f90    /nologo %OPT1% %OPT2%  /module:.\mod
rem   Submodules of CFML_Geometry_SXTAL      
      cd .\CFML_SXTAL_Geom
      ifx /c SXTAL_Angles.f90           /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c SXTAL_FlatCone.f90         /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c SXTAL_IO.f90               /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c SXTAL_Matx_Zvect.f90       /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c SXTAL_PSD.f90              /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c SXTAL_UB.f90               /nologo %OPT1% %OPT2%  /module:..\mod
      move /y *.obj .. > nul
      cd ..
rem
      echo .... Maps/Percolation procedures
      ifx /c CFML_Maps.f90               /nologo %OPT1% %OPT2% /module:.\mod
rem
rem    Submodules CFML_Maps
      cd .\CFML_Maps
      ifx /c Maps_MarchingCubes.f90      /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Maps_Mapping.f90            /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c Maps_Percolation.f90        /nologo %OPT1% %OPT2%  /module:..\mod
      move /y *.obj .. > nul
      cd ..
      
rem
      echo .... Energy/Bond-Valence/BVEL calculations
      ifx /c CFML_EnBVS.f90               /nologo %OPT1% %OPT2% /module:.\mod
rem
rem    Submodules CFML_EnBVS
      cd .\CFML_EnBVS
      ifx /c EnBVS_CostF.f90    /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c EnBVS_Energy.f90   /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c EnBVS_Maps.f90     /nologo %OPT1% %OPT2%  /module:..\mod
      ifx /c EnBVS_SetTab.f90   /nologo %OPT1% %OPT2%  /module:..\mod
      move /y *.obj .. > nul
      cd ..
      
      goto END
rem
rem rem
rem    echo .... Mathematical(II), Optimization, Tables, Patterns
rem rem
rem    ifx /c CFML_LSQ_TypeDef.f90                      /nologo %OPT1% %OPT2%
rem    ifx /c CFML_optimization.f90                     /nologo %OPT1% %OPT2%
rem    ifx /c CFML_optimization_lsq.f90                 /nologo %OPT1% %OPT2%
rem rem
rem    echo .... Bonds, Crystal Metrics, Symmetry, ILL_Instr
rem rem
rem    ifx /c CFML_ILL_Instrm_data.f90                  /nologo %OPT1% %OPT2%
rem rem
rem    echo .... Formats, Geometry, Molecules
rem rem
rem    ifx /c CFML_geom_calc.f90                        /nologo %OPT1% %OPT2%
rem    ifx /c CFML_molecules.f90                        /nologo %OPT1% %OPT2%
rem rem
rem    echo .... Extinction, Structure Factors, SXTAL geometry, Propag Vectors
rem rem
rem    ifx /c CFML_sfac.f90                            /nologo %OPT1% %OPT2%
rem    ifx /c CFML_sxtal_Geom.f90                      /nologo %OPT1% %OPT2%
rem rem
rem    echo .... Maps, BVS, Energy Configurations
rem rem
rem    ifx /c CFML_Export_Vtk.f90                      /nologo %OPT1% %OPT2%
rem rem
rem    echo .... Magnetic Symmetry, Simulated Annealing, Keywords Parser
rem rem
rem    ifx /c CFML_Magnetic_Groups.f90                 /nologo %OPT1% %OPT2%
rem    ifx /c CFML_magsymm.f90                         /nologo %OPT1% %OPT2%
rem    ifx /c CFML_optimization_san.f90                /nologo %OPT1% %OPT2% %OPT3%
rem    ifx /c CFML_refcodes.f90                        /nologo %OPT1% %OPT2%
rem rem
rem    echo .... Magnetic Structure Factors, Polarimetry
rem rem
rem    ifx /c CFML_msfac.f90                           /nologo %OPT1% %OPT2%
rem    ifx /c CFML_polar.f90                           /nologo %OPT1% %OPT2%
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
   
rem
   if [%_WINTER%]==[Y] (
     echo Copying WCrysFML.lib and *.*mod files in ..\%DIRECTORY%\LibW\
     copy .\mod\*.mod ..\%DIRECTORY%\LibW\. > nul
     copy .\mod\*.smod ..\%DIRECTORY%\LibW\. > nul
     move *.lib ..\%DIRECTORY%\LibW\. > nul
   ) else (
     echo Copying CrysFML.lib and *.*mod files in ..\%DIRECTORY%\LibC\
     copy .\mod\*.mod ..\%DIRECTORY%\LibC\. > nul
     copy .\mod\*.smod ..\%DIRECTORY%\LibC\. > nul
     move *.lib ..\%DIRECTORY%\LibC\. > nul
   )
  del *.obj  *.lst *.bak *.pdb > nul
rem
:FIN
   echo.
   echo **---- End Compilation for CrysFML ----**
   echo.
rem
