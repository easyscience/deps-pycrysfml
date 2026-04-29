!!-------------------------------------------------------
!!---- Crystallographic Fortran Modules Library (CrysFML)
!!-------------------------------------------------------
!!---- The CrysFML project is distributed under LGPL. In agreement with the
!!---- Intergovernmental Convention of the ILL, this software cannot be used
!!---- in military applications.
!!----
!!---- Copyright (C) 1999-2022  Institut Laue-Langevin (ILL), Grenoble, FRANCE
!!----                          Universidad de La Laguna (ULL), Tenerife, SPAIN
!!----                          Laboratoire Leon Brillouin(LLB), Saclay, FRANCE
!!----
!!---- Authors: Juan Rodriguez-Carvajal (ILL)
!!----          Javier Gonzalez-Platas  (ULL)
!!----          Nebil Ayape Katcho      (ILL)
!!----
!!---- Contributors: Laurent Chapon     (ILL)
!!----               Marc Janoschek     (Los Alamos National Laboratory, USA)
!!----               Oksana Zaharko     (Paul Scherrer Institute, Switzerland)
!!----               Tierry Roisnel     (CDIFX,Rennes France)
!!----               Eric Pellegrini    (ILL)
!!----               Ross Angel         (University of Pavia)
!!----
!!---- This library is free software; you can redistribute it and/or
!!---- modify it under the terms of the GNU Lesser General Public
!!---- License as published by the Free Software Foundation; either
!!---- version 3.0 of the License, or (at your option) any later version.
!!----
!!---- This library is distributed in the hope that it will be useful,
!!---- but WITHOUT ANY WARRANTY; without even the implied warranty of
!!---- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!!---- Lesser General Public License for more details.
!!----
!!---- You should have received a copy of the GNU Lesser General Public
!!---- License along with this library; if not, see <http://www.gnu.org/licenses/>.
!!----
!!----
!!----
!!---- MODULE: CFML_Optimization_LSQ
!!----   INFO: Module implementing several algorithms for non-linear least-squares.
!!----         At present only two versions of the Levenberg-Marquardt method are implemented.
!!--..
!!--..         General Information Concerning The Least-Squares Procedures
!!--..
!!--..         There are two high level procedures contained in CFML_Optimization_LSQ based
!!--..         in the Levenberg-Marquardt method. The first procedure, called "Marquardt_Fit",
!!--..         is a simple implementation of the method and the second one is a Fortran 90
!!--..         version of the MINPACK Fortran 77 LMxxx subroutines, accessible through the
!!--..         general name "Levenberg_Marquardt_Fit". The second one is, in principle, more
!!--..         robust for general LSQ problems.
!!--..
!!--..
!!--..         Marquardt_Fit (overloaded subroutine)
!!--..         =====================================
!!--..         This procedure needs analytical derivatives to be provided by the user. This
!!--..         procedure minimizes the following cost function:
!!--..
!!--..         C =  Sum(i) { w(i) ( y(x(i)) - yc(x(i),a))^2 }  =
!!--..           =  Sum(i) {  [( y(x(i)) - yc(x(i),a))/sig(i)]^2 }
!!--..
!!--..         Where x(i),y(i),w(i) are observational values (normally w(i)=1/variance(y(i)))
!!--..         and yc is a model to the data depending on the vector a containing the
!!--..         parameters of the model. The user should provide a subroutine for calculating
!!--..         yc(x(i),a) and invoke the procedure passing as the first argument the name
!!--..         of the subroutine.
!!--..
!!--..         Two interfaces are available for calling this procedure:
!!--..
!!--..         First interface:
!!--..         ----------------
!!--..         Subroutine Marquardt_Fit(Model_Functn, X, Y, W, Yc, Nobs, c, vs, Ipr, Chi2, scroll_lines)
!!--..            real(kind=cp), dimension(:),   intent(in)             :: x      !Vector of x-values
!!--..            real(kind=cp), dimension(:),   intent(in)             :: y      !Vector of observed y-values
!!--..            real(kind=cp), dimension(:),   intent(in out)         :: w      !Vector of weights-values (1/variance)
!!--..            real(kind=cp), dimension(:),   intent(   out)         :: yc     !Vector of calculated y-values
!!--..            integer                    ,   intent(in)             :: nobs   !Number of effective components of x,y,w,yc
!!--..            type(LSQ_conditions_type),     intent(in out)         :: c      !Conditions for the algorithm
!!--..            type(LSQ_State_Vector_type),   intent(in out)         :: vs     !State vector for the model calculation
!!--..            integer                    ,   intent(in)             :: Ipr    !Logical unit for writing
!!--..            real(kind=cp),                 intent(out)            :: chi2   !Reduced Chi-2
!!--..            character(len=*),dimension(:), intent(out), optional  :: scroll_lines  !If present, part of the output is stored
!!--..                                                                            !in this text for treatment in the calling program
!!--..
!!--..            Model_functn                                                    !Name of the subroutine calculating yc(i) for point x(i)
!!--..            Interface                                      !Interface for the Model_Functn subroutine
!!--..              Subroutine Model_Functn(iv,Xv,ycalc,a,der)
!!--..                   use CFML_GlobalDeps, only: cp
!!--..                   integer,                             intent(in) :: iv     !Number of the component: "i" in x(i)
!!--..                   real(kind=cp),                       intent(in) :: xv     !Value of x(i)
!!--..                   real(kind=cp),                       intent(out):: ycalc  !Value of yc at point x(i) => ycalc=yc(i)
!!--..                   real(kind=cp),dimension(:),          intent(in) :: a      !Vector of free parameters
!!--..                   real(kind=cp),dimension(:),optional, intent(out):: der    !Derivatives of the function w.r.t. free parameters
!!--..              End Subroutine Model_Functn                                    !at the given point
!!--..           End Interface
!!--..
!!--..
!!--..           The model function subroutine calculates the value of the function "ycalc"
!!--..           at the provided single point "Xv" (component i of vector X).It contains
!!--..           as an argument the vector "a" of "free parameters", whereas the total number
!!--..           of parameters in the model is stored in the structure "Vs" of type
!!--..           LSQ_State_Vector_type (see the definition of the type for details). It is
!!--..           responsibility of the user to write the Model_Functn subroutine with access
!!--..           to Vs (via a module variable) and do the necessary connection between the
!!--..           "a" and "Vs%pv" vectors, as well as the calculation of the derivatives of
!!--..           "ycalc" w.r.t "a" stored in the optional array "der"
!!--..
!!--..         Second interface:
!!--..         -----------------
!!--..         Subroutine Marquardt_Fit(Model_Functn, d, c, vs, Ipr, Chi2, scroll_lines)
!!--..            Type(LSQ_Data_Type),        intent(in out) :: d      !Data type for LSQ
!!--..            type(LSQ_conditions_type),  intent(in out) :: c      !Conditions for the algorithm
!!--..            type(LSQ_State_Vector_type),intent(in out) :: vs     !State vector for the model calculation
!!--..            integer                    ,intent(in)     :: Ipr    !Logical unit for writing
!!--..            real(kind=cp),              intent(out)    :: chi2   !Reduced Chi-2
!!--..            character(len=*),dimension(:), intent(out), optional  :: scroll_lines  !If present, part of the output is stored
!!--..                                                                                   !in this text for treatment in the calling program
!!--..
!!--..           Model_functn                                            !Name of the subroutine calculating yc(i) for point x(i)
!!--..           Interface                                               !Interface for the Model_Functn subroutine
!!--..               Subroutine Model_Functn(iv,Xv,ycalc,Vsa,calder)
!!--..                    use CFML_GlobalDeps, only: cp
!!--..                    integer,                    intent(in)     :: iv     !Number of the component: "i" in x(i)
!!--..                    real(kind=cp),              intent(in)     :: xv     !Value of x(i)
!!--..                    real(kind=cp),              intent(out)    :: ycalc  !Value of yc at point x(i) => ycalc=yc(i)
!!--..                    Type(LSQ_State_Vector_type),intent(in out) :: Vsa    !Parameters, codes, and derivatives
!!--..                    logical,optional,           intent(in)     :: calder !If present, derivatives, stored in Vsa%dpv, are calculated
!!--..               End Subroutine Model_Functn
!!--..           End Interface
!!--..
!!--..           This second interface is simpler than the first one, it encapsulates the
!!--..           handling of the connection between free and total number of parameters.
!!--..           The structure "d", of type LSQ_Data_Type, contains the information that
!!--..           was explicitly used in the first interface (X, Y, W, Yc, Nobs) in five
!!--..           arguments.
!!--..           The model function uses directly as an argument the object "Vsa", of type
!!--..           LSQ_State_Vector_type in terms of which can be explicitly written the
!!--..           calculation of the model function "ycalc" at the given point "iv,Xv". The
!!--..           calculation of the derivatives (stored in Vsa%dpv vector) are needed only
!!--..           if the argument "calder" is present (irrespective of its value .true. or
!!--..           .false.).
!!--..
!!--..
!!--..         Levenberg_Marquardt_Fit (overloaded subroutine), Based on MINPACK
!!--..         =================================================================
!!--..         This procedure needs either analytical (provided by the user) or numerical
!!--..         derivatives (automatically calculated by the procedure from the provided
!!--..         model function subroutine).The procedure minimizes a cost function of the
!!--..         form:
!!--..
!!--..             C =  Sum(i) { fvec(i)^2 }  =   Sum(i) {  [( y(i) - yc(i,a))/sig(i)]^2 }
!!--..
!!--..         The user-provided subroutine should have as arguments the vector of free
!!--..         parameters x(1:n) and the vector or residuals fvec(m).
!!--..
!!--..         Interface No_Fderivatives
!!--..           Subroutine Model_Functn(m, n, x, fvec, iflag)             !Model Function subroutine
!!--..             Use CFML_GlobalDeps, Only: cp
!!--..             Integer,                       Intent(In)    :: m, n    !Number of observations and free parameters
!!--..             Real (Kind=cp),Dimension(:),   Intent(In)    :: x       !Array with the values of free parameters: x(1:n)
!!--..             Real (Kind=cp),Dimension(:),   Intent(In Out):: fvec    !Array of residuals fvec=(y-yc)/sig : fvec(1:m)
!!--..             Integer,                       Intent(In Out):: iflag   !If iflag=1 calculate only fvec without changing fjac
!!--..           End Subroutine Model_Functn                               !If iflag=2 calculate only fjac keeping fvec fixed
!!--..         End Interface No_Fderivatives
!!--..
!!--..
!!--..         If analytical derivatives can be calculated the subroutine should have also
!!--..         as an additional argument the Jacobian rectangular matrix fjac(m,n)
!!--..
!!--..         Interface  FDerivatives
!!--..           Subroutine Model_Functn(m, n, x, fvec, fjac, iflag)       !Model Function subroutine
!!--..             Use CFML_GlobalDeps, Only: cp
!!--..             Integer,                       Intent(In)    :: m, n    !Number of observations and free parameters
!!--..             Real (Kind=cp),Dimension(:),   Intent(In)    :: x       !Array with the values of free parameters: x(1:n)
!!--..             Real (Kind=cp),Dimension(:),   Intent(In Out):: fvec    !Array of residuals fvec=(y-yc)/sig : fvec(1:m)
!!--..             Real (Kind=cp),Dimension(:,:), Intent(Out)   :: fjac    !Jacobian Dfvec/Dx(i,j) = [ dfvec(i)/dx(j) ] : fjac(1:m,1:n)
!!--..             Integer,                       Intent(In Out):: iflag   !If iflag=1 calculate only fvec without changing fjac
!!--..           End Subroutine Model_Functn                               !If iflag=2 calculate only fjac keeping fvec fixed
!!--..         End Interface FDerivatives
!!--..
!!--..
!!--..         The five available interfaces are related to these two options:
!!--..
!!--..         First/second Interface(s) (no derivatives required)
!!--..         ---------------------------------------------------
!!--..
!!--..         Subroutine Levenberg_Marquardt_Fit(Model_Functn, m, c, Vs, chi2, infout,residuals)
!!--..            Integer,                     Intent(In)      :: m        !Number of observations
!!--..            type(LSQ_conditions_type),   Intent(In Out)  :: c        !Conditions of refinement
!!--..            type(LSQ_State_Vector_type), Intent(In Out)  :: Vs       !State vector
!!--..            Real (Kind=cp),              Intent(out)     :: chi2     !final Chi2
!!--..            character(len=*),            Intent(out)     :: infout   !Information about the refinement (min length 256)
!!--..            Real (Kind=cp), dimension(:),optional, intent(out) :: residuals
!!--..         End Subroutine
!!--..
!!--..
!!--..         The second interface corresponds to the original version in MINPACK
!!--..         (called lmdif1)
!!--..
!!--..         Subroutine Levenberg_Marquardt_Fit(Model_Functn, m, n, x, fvec, tol, info, iwa)
!!--..            Integer,                     Intent(In)      :: m
!!--..            Integer,                     Intent(In)      :: n
!!--..            Real (Kind=cp),Dimension(:), Intent(In Out)  :: x
!!--..            Real (Kind=cp),Dimension(:), Intent(Out)     :: fvec
!!--..            Real (Kind=cp),              Intent(In)      :: tol
!!--..            Integer,                     Intent(Out)     :: info
!!--..            Integer,Dimension(:),        Intent(Out)     :: iwa
!!--..         End Subroutine
!!--..
!!--..         The interface for Model_Functn is identical to the interface No_Fderivatives
!!--..
!!--..
!!--..         Third/fourth/fifth Interface(s) (analytical derivatives required)
!!--..         -----------------------------------------------------------
!!--..
!!--..         Subroutine Levenberg_Marquardt_Fit(Model_Functn, m, c, Vs, chi2, calder, infout,residuals)
!!--..            Integer,                     Intent(In)      :: m        !Number of observations
!!--..            type(LSQ_conditions_type),   Intent(In Out)  :: c        !Conditions of refinement
!!--..            type(LSQ_State_Vector_type), Intent(In Out)  :: Vs       !State vector
!!--..            Real (Kind=cp),              Intent(out)     :: chi2     !final Chi2
!!--..            logical,                     Intent(in)      :: calder   !logical (should be .true.) used only for purposes
!!--..                                                                     !of making unambiguous the generic procedure
!!--..            character(len=*),            Intent(out)     :: infout   !Information about the refinement (min length 256)
!!--..
!!--..         This third interface is effectively called LM_DER
!!--..
!!--..         The fourth interface corresponds to the original version in MINPACK
!!--..         (called lmder1)
!!--..
!!--..        Subroutine lmder1(Model_Functn, m, n, x, fvec, fjac, tol, info, ipvt)
!!--..          Integer,                        Intent(In)      :: m
!!--..          Integer,                        Intent(In)      :: n
!!--..          Real (Kind=cp),Dimension(:),    Intent(In Out)  :: x
!!--..          Real (Kind=cp),Dimension(:),    Intent(Out)     :: fvec
!!--..          Real (Kind=cp),Dimension(:,:),  Intent(In Out)  :: fjac
!!--..          Real (Kind=cp),                 Intent(In)      :: tol
!!--..          Integer,                        Intent(Out)     :: info
!!--..          Integer,Dimension(:),           Intent(In Out)  :: ipvt
!!--..
!!--..          The interface for Model_Functn is identical to the previous interface
!!--..          Fderivatives
!!--..
!!--..         The fifth interface corresponds to a mixed version that uses the
!!--..         conditions of refinement as the third interface, but the state vector
!!--..         is directly an 1D array. This cal use directly ve state vector as
!!--..         defined in CFML_Refcodes
!!--..         This fifth interface is effectively called LM_DERV
!!--..
!!--..         Subroutine Levenberg_Marquardt_Fit(Model_Functn, m, c, Vect, chi2, calder, infout,residuals)
!!--..            Integer,                     Intent(In)      :: m        !Number of observations
!!--..            type(LSQ_conditions_type),   Intent(In Out)  :: c        !Conditions of refinement
!!--..            real, dimension(:),          Intent(In Out)  :: Vect     !State vector like in CFML_Refcodes Vec_RefPar
!!--..            Real (Kind=cp),              Intent(out)     :: chi2     !final Chi2
!!--..            logical,                     Intent(in)      :: calder   !logical (should be .true.) used only for purposes
!!--..                                                                     !of making unambiguous the generic procedure
!!--..            character(len=*),            Intent(out)     :: infout   !Information about the refinement (min length 256)
!!--..
!!--..          The interface for Model_Functn is identical to the previous interfaces
!!--..          with derivatives
!!--..
!!--..
!!---- HISTORY
!!----    Update: 04/03/2011, 16/06/2020
!!----
!!---- DEPENDENCIES
!!--++    Use CFML_GlobalDeps,    only : Cp, Dp, Err_CFML, clear_error
!!--++    Use CFML_Maths,         only : Inverse_Matrix, Upper_Triangular, SVDcmp
!!----
!!---- VARIABLES
!!--++    CH                      [Private]
!!--++    CORREL                  [Private]
!!--++    CURV_MAT                [Private]
!!----    INFO_LSQ_MESS
!!--++    NAMFREE                 [Private]
!!--++    PN                      [Private]
!!----
!!---- PROCEDURES
!!----    Functions:
!!----       FCHISQ
!!----
!!----    Subroutines:
!!--++       BOX_CONSTRAINTS           [Private]
!!--++       CURFIT_v1                 [Private]
!!--++       CURFIT_v2                 [Private]
!!--++       FDJAC2                    [Private]
!!----       LEVENBERG_MARQUARDT_FIT   [Overloaded]   {LM_DER, LM_DERV, LM_DIF, LMDER1, LMDIF1}
!!--++       LM_DER                    [Private]
!!--++       LM_DERV                   [Private]
!!--++       LMDER                     [Private]
!!--++       LM_DIF                    [Private]
!!--++       LMDIF                     [Private]
!!--++       LMPAR                     [Private]
!!----       MARQUARDT_FIT             [Overloaded]  {MARQUARDT_FIT_v1, MARQUARDT_FIT_v2}
!!--++       MARQUARDT_FIT_v1          [Private]
!!--++       MARQUARDT_FIT_v2          [Private]
!!--++       OUTPUT_CYC                [Private]
!!----       INFO_LSQ_LM               [Overloaded] {INFO_LSQ_LM_V, INFO_LSQ_LM_VS}
!!--++       INFO_LSQ_LM_V             [Private]
!!--++       INFO_LSQ_LM_VS            [Private]
!!----       INFO_LSQ_OUTPUT
!!--++       QRFAC                     [Private]
!!--++       QRSOLV                    [Private]
!!----
!!----
!!
 Module CFML_Optimization_LSQ
    !---- Use Files ----!
    Use CFML_GlobalDeps,   only: Cp, Dp, Err_CFML, Clear_Error, line_term
    Use CFML_Maths,        only: Inverse_Matrix, Upper_Triangular,SVDcmp


    !---- Variables ----!
    implicit none

    private

    !---- List of public functions ----!
    public  :: Correl_from_LSQ_LM_VS,fchisq
    !---- List of public overloaded procedures: functions ----!

    !---- List of public subroutines ----!
    public  :: Marquardt_Fit, Levenberg_Marquardt_Fit, Info_LSQ_Output, Info_LSQ_LM, Modify_Codes_State_Vector

    !---- List of public overloaded procedures: subroutines ----!

    !---- List of private functions ----!

    !---- List of private subroutines ----! Nebil 18/07/2025: make these subroutines public because gfortran does not compile otherwise
    public :: box_constraints, lmpar, qrfac, qrsolv, lmder1, lmdif1

    !---- Definitions ----!

    !!----
    !!---- MAX_FREE_PAR
    !!----    integer, parameter, public  :: Max_Free_Par
    !!----
    !!----    Maximum number of free parameters (3000)
    !!----    (it may be changed at will!)
    !!----
    !!---- Update: 01/07/2015
    !!
    integer, parameter, public :: Max_Free_Par=3000   !Maximum number of free parameters

    !!----
    !!---- TYPE :: LSQ_CONDITIONS_TYPE
    !!--..
    !!----  Type, public :: LSQ_Conditions_type
    !!----     logical          :: constr          ! if true box constraint of percent% are applied to parameters
    !!----     logical          :: reached         ! if true convergence was reached in the algorithm
    !!----     integer          :: corrmax         ! value of correlation in % to output
    !!----     integer          :: nfev            ! number of function evaluations (output component, useful for assessing LM algorithm)
    !!----     integer          :: njev            ! number of Jacobian evaluations                 "
    !!----     integer          :: icyc            ! number of cycles of refinement or maximum number of function evaluations in LM
    !!----                                         ! In LM procedures the default value is icyc = maxfev = 100(npvar+1)
    !!----     integer          :: npvar           ! number of effective free parameters of the model
    !!----     integer          :: iw              ! indicator for weighting scheme (if iw=1 => w=1/yc)
    !!----     integer          :: nprint          ! indicator for printing during iterations, if nprint > 0 printing each nprint iterations
    !!----     real(kind=cp)    :: tol             ! tolerance value for applying stopping criterion in LM algorithm
    !!----     real(kind=cp)    :: percent         ! %value of maximum variation of a parameter w.r.t.
    !!----                                         ! the intial value before fixing it
    !!----  End Type LSQ_Conditions_type
    !!----
    !!----  Derived type encapsulating all necessary conditions for running the LSQ algorithm
    !!----
    !!---- Update: 01/07/2015
    !!
    Type, public :: LSQ_Conditions_Type
       logical          :: constr=.false.  ! if true box constraint of percent% are applied to parameters
       logical          :: reached=.false. ! if true convergence was reached in the algorithm
       logical          :: failed=.false.  ! the algorithm failed because singular matrix
       integer          :: corrmax=50      ! value of correlation in % to output
       integer          :: nfev=0          ! number of function evaluations (output component, useful for assessing LM algorithm)
       integer          :: njev=0          ! number of Jacobian evaluations                 "
       integer          :: icyc=0          ! number of cycles of refinement or maximum number of function evaluations in LM
                                           ! In LM procedures the default value is icyc = maxfev = 100(npvar+1)
       integer          :: npvar=0         ! number of effective free parameters of the model
       integer          :: iw=0            ! indicator for weighting scheme (if iw=1 => w=1/yc)
       integer          :: nprint=0        ! indicator for printing during iterations, if nprint > 0 printing each nprint iterations
       real(kind=cp)    :: tol=0.0         ! tolerance value for applying stopping criterion in LM algorithm
       real(kind=cp)    :: percent=0.0     ! %value of maximum variation of a parameter w.r.t.
                                           ! the intial value before fixing it
    End Type LSQ_Conditions_Type

    !!----
    !!---- TYPE :: LSQ_DATA_TYPE
    !!--..
    !!----
    !!----  Type, public :: LSQ_Data_Type
    !!----     integer                                    :: nobs  !total number of observations
    !!----     integer                                    :: iw    !Indicator for type of values contained in component sig
    !!----     real(kind=cp),    dimension(:),allocatable :: x     !Vector containing a relevant quantity for each observation (x-coordinate ...)
    !!----     real(kind=cp),    dimension(:),allocatable :: y     !Vector containing the observed values
    !!----     real(kind=cp),    dimension(:),allocatable :: sw    !Vector containing the standard deviation of observations if iw=0
    !!----                                                         !or the weight factors for least squares refinement if iw=1
    !!----     real(kind=cp),    dimension(:),allocatable :: yc    !Vector containing the calculated values
    !!----  End Type LSQ_Data_type
    !!----
    !!----  Derived type encapsulating the observed and calculated data as well as the
    !!----  weighting factors, a variable related with each observed value and the
    !!----  total number of observations. It is responsibility of the calling program to
    !!----  allocate the components before calling the Marquardt_fit procedure.
    !!----
    !!---- Update: August - 2009
    !!
    Type, public :: LSQ_Data_Type
       integer                                    :: nobs  !total number of observations
       integer                                    :: iw    !Indicator for type of values contained in component sw
       real(kind=cp),    dimension(:),allocatable :: x     !Vector containing a relevant quantity for each observation (x-coordinate ...)
       real(kind=cp),    dimension(:),allocatable :: y     !Vector containing the observed values
       real(kind=cp),    dimension(:),allocatable :: sw    !Vector containing the standard deviation of observations if iw=0
                                                           !or the weight factors for least squares refinement if iw=1
       real(kind=cp),    dimension(:),allocatable :: yc    !Vector containing the calculated values
    End Type LSQ_Data_type

    !!----
    !!---- TYPE :: LSQ_STATE_VECTOR_TYPE
    !!--..
    !!----
    !!----  Type, public :: LSQ_State_Vector_Type
    !!----     integer                                    :: np         !total number of model parameters <= Max_Free_Par
    !!----     logical                                    :: code_comp  !If .true. the codes are interpreted as number in the LSQ list
    !!----     integer(kind=2)                            :: code_max   !Maximum code number (used in case of code_comp=.true.)
    !!----     real(kind=cp),     dimension(Max_Free_Par) :: mul        !Vector of multipliers (used in case of code_comp=.true.
    !!----     real(kind=cp),     dimension(Max_Free_Par) :: pv         !Vector of parameters
    !!----     real(kind=cp),     dimension(Max_Free_Par) :: spv        !Vector of standard deviations
    !!----     real(kind=cp),     dimension(Max_Free_Par) :: dpv        !Vector of derivatives at a particular point
    !!----     integer(kind=4),   dimension(Max_Free_Par) :: code       !pointer for selecting variable parameters
    !!----     character(len=40), dimension(Max_Free_Par) :: nampar     !Names of parameters
    !!----  End Type LSQ_State_Vector_Type
    !!----
    !!----  Derived type encapsulating the vector state defining a set of parameter
    !!----  for calculating the model function and running the LSQ algorithm.
    !!----  Now, with the introduction of code_comp and mul, the codes may be also interpreted
    !!----  as the ordinal number in the LSQ list of independent parameters. Depending on the
    !!----  the way the user program attributes codes and constraints a call to the subroutine
    !!----  Modify_Codes_State_Vector (see below)
    !!----
    !!---- Update: January- 2014
    !!
    Type, public :: LSQ_State_Vector_Type
       integer                                    :: np         !total number of model parameters <= Max_Free_Par
       logical                                    :: code_comp  !If .true. the codes are interpreted as number in the LSQ list
       integer(kind=2)                            :: code_max   !Maximum code number (used in case of code_comp=.true.)
       real(kind=cp),     dimension(Max_Free_Par) :: mul        !Vector of multipliers (used in case of code_comp=.true.)
       real(kind=cp),     dimension(Max_Free_Par) :: pv         !Vector of parameters
       real(kind=cp),     dimension(Max_Free_Par) :: spv        !Vector of standard deviations
       real(kind=cp),     dimension(Max_Free_Par) :: dpv        !Vector of derivatives at a particular point
       integer(kind=4),   dimension(Max_Free_Par) :: code       !pointer for selecting variable parameters
       character(len=40), dimension(Max_Free_Par) :: nampar     !Names of parameters
    End Type LSQ_State_Vector_type

    !!--++
    !!--++ CH
    !!--++    real(kind=cp),     dimension(Max_Free_Par), private   :: ch
    !!--++
    !!--++    (PRIVATE)
    !!--++    Vector holding the change in the values of parameters (ch = pn - pv)
    !!--++
    !!--++ Update: February - 2005
    !!
    real(kind=cp), dimension(Max_Free_Par), private   :: ch         ! ch = pn - pv

    !!--++
    !!--++ CORREL
    !!--++    real(kind=cp), dimension(Max_Free_Par,Max_Free_Par), public  :: correl
    !!--++
    !!--++    Variance/covariance/correlation matrix
    !!--++
    !!--++ Update: February - 2005
    !!
    real(kind=cp), dimension(Max_Free_Par,Max_Free_Par), private  :: correl     !Variance/covariance/correlation matrix


    !!--++
    !!--++ CURV_MAT
    !!--++    real(kind=cp), dimension(Max_Free_Par,Max_Free_Par), public  :: curv_mat
    !!--++
    !!--++    Curvature matrix
    !!--++
    !!--++ Update: February - 2005
    !!
    real(kind=cp), dimension(Max_Free_Par,Max_Free_Par), private  :: curv_mat   !Curvature matrix

    !!----
    !!---- INFO_LSQ_MESS
    !!----    Character(len=150), public  :: Info_Lsq_Mess
    !!----
    !!----    Character variable containing the information message associated to the
    !!----    exit parameter "info" of the Levenberg_Marquardt_Fit procedure.
    !!----
    !!---- Update: August - 2005
    !!
    Character(len=150), public  :: Info_Lsq_Mess


    !!--++
    !!--++ NAMFREE
    !!--++    character(len=40), dimension(Max_Free_Par), private   :: namfree
    !!--++
    !!--++  (PRIVATE)
    !!--++  Names of refined parameters
    !!--++
    !!--++ Update: January - 2014
    !!
    character(len=40), dimension(Max_Free_Par), private,save   :: namfree    !Names of refined parameters

    !!--++
    !!--++ PN
    !!--++    real(kind=cp), dimension(Max_Free_Par), public   :: pn
    !!--++
    !!--++    Vector with new values of parameters
    !!--++
    !!--++ Update: February - 2005
    !!
    real(kind=cp), dimension(Max_Free_Par), private   :: pn   !Vector with new values of parameters

    !---- Interfaces ----!

    Interface Marquardt_fit
      Module Procedure Marquardt_Fit_v1
      Module Procedure Marquardt_Fit_v2
    End Interface

    Interface Levenberg_Marquardt_Fit
      Module Procedure LM_Dif
      Module Procedure lmdif1
      Module Procedure LM_DifV
      Module Procedure LM_Der
      Module Procedure LM_DerV
      Module Procedure lmder1
    End Interface Levenberg_Marquardt_Fit

    Interface Info_LSQ_LM
      Module Procedure Info_LSQ_LM_VS
      Module Procedure Info_LSQ_LM_V
    End Interface Info_LSQ_LM

  Interface

    Module Function Correl_from_LSQ_LM_VS(c) Result(corr_matrix)
       !---- Arguments ----!
       type(LSQ_conditions_type),  intent(in)    :: c
       real(kind=cp), dimension(c%npvar,c%npvar) :: corr_matrix
    End Function Correl_from_LSQ_LM_VS

    Module Subroutine Info_LSQ_LM_V(Chi2,Lun,c,v,vstd,vnam)
       !---- Arguments ----!
       real(kind=cp),                 intent(in) :: chi2
       integer,                       intent(in) :: lun
       type(LSQ_conditions_type),     intent(in) :: c
       real(kind=cp),   dimension(:), intent(in) :: v,vstd
       character(len=*),dimension(:), intent(in) :: vnam
    End Subroutine Info_LSQ_LM_V

    Module Subroutine Info_LSQ_LM_VS(Chi2,Lun,c,vs)
       !---- Arguments ----!
       real(kind=cp),              intent(in)     :: chi2
       integer,                    intent(in)     :: lun
       type(LSQ_conditions_type),  intent(in)     :: c
       type(LSQ_State_Vector_type),intent(in)     :: vs
    End Subroutine Info_LSQ_LM_VS

    Module Subroutine Info_LSQ_Output(Chi2,FL,Nobs,X,Y,Yc,W,Lun,c,vs,algor,out_obscal,text_info)
       !---- Arguments ----!
       real(kind=cp),              intent(in)     :: chi2
       real(kind=cp),              intent(in)     :: FL
       integer,                    intent(in)     :: nobs
       real(kind=cp),dimension(:), intent(in)     :: x
       real(kind=cp),dimension(:), intent(in)     :: y
       real(kind=cp),dimension(:), intent(in)     :: yc
       real(kind=cp),dimension(:), intent(in)     :: w
       integer,                    intent(in)     :: lun
       type(LSQ_conditions_type),  intent(in)     :: c
       type(LSQ_State_Vector_type),intent(in)     :: vs
       character(len=*),           intent(in)     :: algor
       character(len=*), optional, intent(in)     :: out_obscal
       character(len=*), optional, intent(in)     :: text_info
    End Subroutine Info_LSQ_Output

    Module Subroutine LM_Der(Model_Functn, m, c, Vs, chi2, calder, infout,residuals)
       !---- Arguments ----!
       Integer,                               Intent(In)      :: m
       type(LSQ_conditions_type),             Intent(In Out)  :: c
       type(LSQ_State_Vector_type),           Intent(In Out)  :: Vs
       Real (Kind=cp),                        Intent(out)     :: chi2
       logical,                               Intent(in)      :: calder
       character(len=*),                      Intent(out)     :: infout
       real (Kind=cp), dimension(:),optional, Intent(out)     :: residuals

       Interface
         Subroutine Model_Functn(m, n, x, fvec, fjac, iflag)
            Use CFML_GlobalDeps, Only: cp
            Integer,                       Intent(In)    :: m, n
            Real (Kind=cp),Dimension(:),   Intent(In)    :: x
            Real (Kind=cp),Dimension(:),   Intent(In Out):: fvec
            Real (Kind=cp),Dimension(:,:), Intent(Out)   :: fjac
            Integer,                       Intent(In Out):: iflag
         End Subroutine Model_Functn
       End Interface
    End Subroutine LM_Der

    Module Subroutine LM_DerV(Model_Functn, m, c, V, Vstd, chi2, calder, infout,residuals)
       !---- Arguments ----!
       Integer,                               Intent(In)      :: m
       type(LSQ_conditions_type),             Intent(In Out)  :: c
       Real (Kind=cp), dimension(:),          Intent(In Out)  :: V,Vstd
       Real (Kind=cp),                        Intent(out)     :: chi2
       logical,                               Intent(in)      :: calder
       character(len=*),                      Intent(out)     :: infout
       real (Kind=cp), dimension(:),optional, Intent(out)     :: residuals

       Interface
         Subroutine Model_Functn(m, n, x, fvec, fjac, iflag)
            Use CFML_GlobalDeps, Only: cp
            Integer,                       Intent(In)    :: m, n
            Real (Kind=cp),Dimension(:),   Intent(In)    :: x
            Real (Kind=cp),Dimension(:),   Intent(In Out):: fvec
            Real (Kind=cp),Dimension(:,:), Intent(Out)   :: fjac
            Integer,                       Intent(In Out):: iflag
         End Subroutine Model_Functn
       End Interface
    End Subroutine LM_DerV

    Module Subroutine LM_Dif(Model_Functn, m, c, Vs, chi2, infout,residuals,idebug)
       !---- Arguments ----!
       Integer,                     Intent(In)      :: m        !Number of observations
       type(LSQ_conditions_type),   Intent(In Out)  :: c        !Conditions of refinement
       type(LSQ_State_Vector_type), Intent(In Out)  :: Vs       !State vector
       Real (Kind=cp),              Intent(out)     :: chi2     !final Chi2
       character(len=*),            Intent(out)     :: infout   !Information about the refinement  (min length 256)
       Real (Kind=cp), dimension(:),optional, intent(out) :: residuals
       integer,                     optional, intent(in)  :: idebug !logical unit for writing results

       Interface
         Subroutine Model_Functn(m, n, x, fvec, iflag)             !Model Function subroutine
            Use CFML_GlobalDeps, Only: cp
            Integer,                       Intent(In)    :: m, n    !Number of observations and free parameters
            Real (Kind=cp),Dimension(:),   Intent(In)    :: x       !Array with the values of free parameters: x(1:n)
            Real (Kind=cp),Dimension(:),   Intent(In Out):: fvec    !Array of residuals fvec=(y-yc)/sig : fvec(1:m)
            Integer,                       Intent(In Out):: iflag   !If iflag=1 calculate only fvec without changing fjac
         End Subroutine Model_Functn                                !If iflag=2 calculate only fjac keeping fvec fixed
       End Interface
    End Subroutine LM_Dif

    Module Subroutine LM_DifV(Model_Functn, m, c, V, Vstd, chi2, infout,residuals)
       !---- Arguments ----!
       Integer,                               Intent(In)      :: m
       type(LSQ_conditions_type),             Intent(In Out)  :: c
       Real (Kind=cp), dimension(:),          Intent(In Out)  :: V,Vstd
       Real (Kind=cp),                        Intent(out)     :: chi2
       character(len=*),                      Intent(out)     :: infout
       real (Kind=cp), dimension(:),optional, Intent(out)     :: residuals

       Interface
         Subroutine Model_Functn(m, n, x, fvec, iflag)
            Use CFML_GlobalDeps, Only: cp
            Integer,                     Intent(In)      :: m, n
            Real (Kind=cp),Dimension(:), Intent(In)      :: x
            Real (Kind=cp),Dimension(:), Intent(In Out)  :: fvec
            Integer,                     Intent(In Out)  :: iflag
         End Subroutine Model_Functn
       End Interface
    End Subroutine LM_DifV

    Module Subroutine lmder1(Model_Functn, m, n, x, fvec, fjac, tol, nprint, info, ipvt)
       !---- Arguments ----!
       Integer,                        Intent(In)      :: m
       Integer,                        Intent(In)      :: n
       Real (Kind=cp),Dimension(:),    Intent(In Out)  :: x
       Real (Kind=cp),Dimension(:),    Intent(Out)     :: fvec
       Real (Kind=cp),Dimension(:,:),  Intent(In Out)  :: fjac
       Real (Kind=cp),                 Intent(In)      :: tol
       Integer,                        Intent(in)      :: nprint
       Integer,                        Intent(Out)     :: info
       Integer,Dimension(:),           Intent(In Out)  :: ipvt

       Interface
         Subroutine Model_Functn(m, n, x, fvec, fjac, iflag)
            Use CFML_GlobalDeps, Only: cp
            Integer,                       Intent(In)    :: m, n
            Real (Kind=cp),Dimension(:),   Intent(In)    :: x
            Real (Kind=cp),Dimension(:),   Intent(In Out):: fvec
            Real (Kind=cp),Dimension(:,:), Intent(Out)   :: fjac
            Integer,                       Intent(In Out):: iflag
         End Subroutine Model_Functn
       End Interface
    End Subroutine lmder1

    Module Subroutine lmdif1(Model_Functn, m, n, x, fvec, tol, nprint, info, iwa)
       !---- Argument ----!
       Integer,                     Intent(In)      :: m
       Integer,                     Intent(In)      :: n
       Real (Kind=cp),Dimension(:), Intent(In Out)  :: x
       Real (Kind=cp),Dimension(:), Intent(Out)     :: fvec
       Real (Kind=cp),              Intent(In)      :: tol
       Integer,                     Intent(In)      :: nprint
       Integer,                     Intent(Out)     :: info
       Integer,Dimension(:),        Intent(Out)     :: iwa

       Interface
         Subroutine Model_Functn(m, n, x, fvec, iflag)
            Use CFML_GlobalDeps, Only: cp
            Integer,                     Intent(In)      :: m, n
            Real (Kind=cp),Dimension(:), Intent(In)      :: x
            Real (Kind=cp),Dimension(:), Intent(In Out)  :: fvec
            Integer,                     Intent(In Out)  :: iflag
         End Subroutine Model_Functn
       End Interface
    End Subroutine lmdif1

    Module Subroutine Marquardt_Fit_v1(Model_Functn,X,Y,W,Yc,Nobs,c,vs,Ipr,Chi2,scroll_lines,text_info)
       !---- Arguments ----!
       real(kind=cp),   dimension(:), intent(in)             :: x,y
       real(kind=cp),   dimension(:), intent(in out)         :: w
       real(kind=cp),   dimension(:), intent(   out)         :: yc
       integer,                       intent(in)             :: nobs,Ipr
       type(LSQ_conditions_type),     intent(in out)         :: c
       type(LSQ_State_Vector_type),   intent(in out)         :: vs
       real(kind=cp),                 intent(   out)         :: chi2
       character(len=*),dimension(:), intent(out), optional  :: scroll_lines
       character(len=*),              intent(in),  optional  :: text_info

       Interface
        Subroutine Model_Functn(iv,Xv,ycalc,aa,der)
           use CFML_GlobalDeps, only: cp
           integer,                             intent(in) :: iv
           real(kind=cp),                       intent(in) :: xv
           real(kind=cp),dimension(:),          intent(in) :: aa
           real(kind=cp),                       intent(out):: ycalc
           real(kind=cp),dimension(:),optional, intent(out):: der
        End Subroutine Model_Functn
       End Interface
    End Subroutine Marquardt_Fit_v1

    Module Subroutine Marquardt_Fit_v2(Model_Functn,d,c,vs,Ipr,Chi2,scroll_lines,text_info)
       !---- Arguments ----!
       Type(LSQ_Data_Type),           intent(in out)         :: d
       type(LSQ_conditions_type),     intent(in out)         :: c
       type(LSQ_State_Vector_type),   intent(in out)         :: vs
       integer,                       intent(in)             :: Ipr
       real(kind=cp),                 intent(   out)         :: chi2
       character(len=*),dimension(:), intent(out), optional  :: scroll_lines
       character(len=*),              intent(in),  optional  :: text_info

       Interface
        Subroutine Model_Functn(iv,xv,ycalc,Vsa,calder)
           use CFML_GlobalDeps, only: cp
           import :: LSQ_State_Vector_type
           integer,                     intent(in)     :: iv
           real(kind=cp),               intent(in)     :: xv
           real(kind=cp),               intent(out)    :: ycalc
           Type(LSQ_State_Vector_type), intent(in out) :: Vsa
           logical,optional,            intent(in)     :: calder
        End Subroutine Model_Functn
       End Interface
    End Subroutine Marquardt_Fit_v2

    Module Subroutine Output_Cyc(Ic,Lun,Chi2,vs)
       !---- Arguments ----!
       integer,                    intent(in) :: ic  !cycle number
       integer,                    intent(in) :: lun !logical number of the output file
       real(kind=cp),              intent(in) :: chi2
       type(LSQ_State_Vector_type),intent(in) :: vs
    End Subroutine Output_Cyc

  End interface

 Contains

    !---- Functions ----!

    !!----
    !!---- Function Fchisq(Nfr,Nobs,Y,W,Yc) Result(Chisq)
    !!----    integer, intent(in)                    :: Nfr
    !!----    integer, intent(in)                    :: Nobs
    !!----    real(kind=cp), intent(in),dimension(:) :: y
    !!----    real(kind=cp), intent(in),dimension(:) :: w
    !!----    real(kind=cp), intent(in),dimension(:) :: yc
    !!----
    !!----    Evaluate reduced chi2
    !!----
    !!---- Update: February - 2005
    !!
    Function Fchisq(Nfr,Nobs,Y,W,Yc) Result(Chisq)
       !---- Arguments ----!
       integer,                    intent(in) :: nfr
       integer,                    intent(in) :: nobs
       real(kind=cp),dimension(:), intent(in) :: y
       real(kind=cp),dimension(:), intent(in) :: w
       real(kind=cp),dimension(:), intent(in) :: yc
       real(kind=cp)                          :: chisq

       !---- Local variables ----!
       integer :: i

       chisq=0.0
       do i=1,nobs
          chisq=chisq+w(i)*(y(i)-yc(i))**2
       end do
       chisq=chisq/real(nfr)

    End Function Fchisq

    !---- Subroutines ----!

    !!--++
    !!--++ Subroutine Box_Constraints(A,Sa,Fixed,c,vs)
    !!--++    real(kind=cp), dimension (Max_Free_Par), intent(in out) :: a
    !!--++    real(kind=cp), dimension (Max_Free_Par), intent(in out) :: sa
    !!--++    logical                        ,         intent(   out) :: fixed
    !!--++    Type(LSQ_Conditions_type),               intent(in)     :: c     !conditions for refinement
    !!--++    Type(LSQ_State_Vector_type),             intent(in)     :: vs    !State vector
    !!--++
    !!--++    (PRIVATE)
    !!--++    This subroutine avoid a peak-position parameter undergoing a change
    !!--++    greater than percent% w.r.t. the initial value. This is probably enough
    !!--++    to avoid lack of convergence when a peak makes an excursion outside
    !!--++    the current angular range. The parameter is fixed for the next cycle
    !!--++    The intensity parameters are also constrained to be strictly positive
    !!--++
    !!--++ Update: February - 2005
    !!
    Subroutine Box_Constraints(A,Sa,Fixed,c,vs)
       !---- Arguments ----!
       real(kind=cp), dimension (Max_Free_Par), intent(in out) :: a
       real(kind=cp), dimension (Max_Free_Par), intent(in out) :: sa
       logical                        ,         intent(   out) :: fixed
       Type(LSQ_Conditions_type),               intent(in)     :: c     !conditions for refinement
       Type(LSQ_State_Vector_type),             intent(in)     :: vs    !State vector

       !---- Local variables ----!
       integer       :: i,ncount
       real(kind=cp) :: per
       logical       :: ifi

       fixed=.false.
       ncount=0
       per=c%percent*0.01
       do i=1,vs%np
          ifi=.false.
          if (vs%code(i) /= 0) then
             ncount=ncount+1
             if (abs(vs%pv(i)) > 0.0000001) then
                if (abs(a(ncount)-vs%pv(i))/vs%pv(i) > per )  ifi=.true.
                if (ifi) then
                   fixed=.true.
                   a(ncount) = vs%pv(i)
                   namfree(ncount) = trim(namfree(ncount))//"-*"
                   sa(ncount) = 0.0
                   pn(i) = vs%pv(i)
                   ch(i) = 0.0
                end if
             end if
          end if
       end do

    End Subroutine Box_Constraints

    !!--++
    !!--++   Subroutine lmpar(n, r, ipvt, diag, qtb, delta, par, x, sdiag)
    !!--++      Integer,                        Intent(In)      :: n
    !!--++      Real (Kind=cp), dimension(:,:), Intent(In Out)  :: r
    !!--++      Integer,        dimension(:),   Intent(In)      :: ipvt
    !!--++      Real (Kind=cp), dimension(:),   Intent(In)      :: diag
    !!--++      Real (Kind=cp), dimension(:),   Intent(In)      :: qtb
    !!--++      Real (Kind=cp),                 Intent(In)      :: delta
    !!--++      Real (Kind=cp),                 Intent(Out)     :: par
    !!--++      Real (Kind=cp), dimension(:),   Intent(Out)     :: x
    !!--++      Real (Kind=cp), dimension(:),   Intent(Out)     :: sdiag
    !!--++
    !!--..  Original documentation:
    !!--..
    !!--..  Given an m by n matrix a, an n by n nonsingular diagonal matrix d,
    !!--..  an m-vector b, and a positive number delta, the problem is to determine a
    !!--..  value for the parameter par such that if x solves the system
    !!--..
    !!--..        a*x = b ,     sqrt(par)*d*x = 0 ,
    !!--..
    !!--..  in the least squares sense, and dxnorm is the euclidean
    !!--..  norm of d*x, then either par is zero and
    !!--..
    !!--..        (dxnorm-delta) <= 0.1*delta ,
    !!--..
    !!--..  or par is positive and
    !!--..
    !!--..        abs(dxnorm-delta) <= 0.1*delta .
    !!--..
    !!--..  This subroutine completes the solution of the problem if it is provided
    !!--..  with the necessary information from the r factorization, with column
    !!--..  qpivoting, of a.  That is, if a*p = q*r, where p is a permutation matrix,
    !!--..  q has orthogonal columns, and r is an upper triangular matrix with diagonal
    !!--..  elements of nonincreasing magnitude, then lmpar expects the full upper
    !!--..  triangle of r, the permutation matrix p, and the first n components of
    !!--..  (q transpose)*b.
    !!--..  On output lmpar also provides an upper triangular matrix s such that
    !!--..
    !!--..         t   t                   t
    !!--..        p *(a *a + par*d*d)*p = s *s .
    !!--..
    !!--..  s is employed within lmpar and may be of separate interest.
    !!--..
    !!--..  Only a few iterations are generally needed for convergence of the algorithm.
    !!--..  If, however, the limit of 10 iterations is reached, then the output par
    !!--..  will contain the best value obtained so far.
    !!--..
    !!--..  the subroutine statement is
    !!--..
    !!--..    subroutine lmpar(n,r,ldr,ipvt,diag,qtb,delta,par,x,sdiag, wa1,wa2)
    !!--..
    !!--..  where
    !!--..
    !!--..    n is a positive integer input variable set to the order of r.
    !!--..
    !!--..    r is an n by n array. on input the full upper triangle
    !!--..      must contain the full upper triangle of the matrix r.
    !!--..      On output the full upper triangle is unaltered, and the
    !!--..      strict lower triangle contains the strict upper triangle
    !!--..      (transposed) of the upper triangular matrix s.
    !!--..
    !!--..    ldr is a positive integer input variable not less than n
    !!--..      which specifies the leading dimension of the array r.
    !!--..
    !!--..    ipvt is an integer input array of length n which defines the
    !!--..      permutation matrix p such that a*p = q*r. column j of p
    !!--..      is column ipvt(j) of the identity matrix.
    !!--..
    !!--..    diag is an input array of length n which must contain the
    !!--..      diagonal elements of the matrix d.
    !!--..
    !!--..    qtb is an input array of length n which must contain the first
    !!--..      n elements of the vector (q transpose)*b.
    !!--..
    !!--..    delta is a positive input variable which specifies an upper
    !!--..      bound on the euclidean norm of d*x.
    !!--..
    !!--..    par is a nonnegative variable. on input par contains an
    !!--..      initial estimate of the levenberg-marquardt parameter.
    !!--..      on output par contains the final estimate.
    !!--..
    !!--..    x is an output array of length n which contains the least
    !!--..      squares solution of the system a*x = b, sqrt(par)*d*x = 0,
    !!--..      for the output par.
    !!--..
    !!--..    sdiag is an output array of length n which contains the
    !!--..      diagonal elements of the upper triangular matrix s.
    !!--..
    !!--..    wa1...2 are work arrays of length n.
    !!--..
    !!--..  Argonne National Laboratory. minpack project. march 1980.
    !!--..  Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More
    !!--++
    !!--++
    !!--++ Update: February - 2009
    !!
    Subroutine lmpar(n, r, ipvt, diag, qtb, delta, par, x, sdiag)
       !---- Argument ----!
       Integer,                        Intent(In)      :: n
       Real (Kind=cp), dimension(:,:), Intent(In Out)  :: r
       Integer,        dimension(:),   Intent(In)      :: ipvt
       Real (Kind=cp), dimension(:),   Intent(In)      :: diag
       Real (Kind=cp), dimension(:),   Intent(In)      :: qtb
       Real (Kind=cp),                 Intent(In)      :: delta
       Real (Kind=cp),                 Intent(Out)     :: par
       Real (Kind=cp), dimension(:),   Intent(Out)     :: x
       Real (Kind=cp), dimension(:),   Intent(Out)     :: sdiag

       !--- Local Variables ---!
       Integer                      :: iter, j, jm1, jp1, k, l, nsing
       Real (Kind=cp)               :: dxnorm, dwarf, fp, gnorm, parc, parl, paru, suma, temp
       Real (Kind=cp), dimension(n) :: wa1, wa2
       Real (Kind=cp), Parameter    :: p1 = 0.1_CP, p001 = 0.001_CP, zero = 0.0_CP
       logical                      :: lmerror

       !     dwarf is the smallest positive magnitude.
       dwarf = Tiny(zero)
       lmerror=.false.

       !     compute and store in x the gauss-newton direction. if the
       !     jacobian is rank-deficient, obtain a least squares solution.
       nsing = n
       Do j = 1, n
          wa1(j) = qtb(j)
          If (r(j,j) == zero .AND. nsing == n) nsing = j - 1
          If (nsing < n) wa1(j) = zero
       End Do

       Do k = 1, nsing
          j = nsing - k + 1
          wa1(j) = wa1(j)/r(j,j)
          temp = wa1(j)
          jm1 = j - 1
          wa1(1:jm1) = wa1(1:jm1) - r(1:jm1,j)*temp
       End Do

       Do j = 1, n
          l = ipvt(j)
          x(l) = wa1(j)
       End Do

       !     initialize the iteration counter.
       !     evaluate the function at the origin, and test
       !     for acceptance of the gauss-newton direction.
       iter = 0
       wa2(1:n) = diag(1:n)*x(1:n)
       dxnorm = norm2(wa2(1:n))
       fp = dxnorm - delta
       If (fp <= p1*delta) lmerror=.true.
       If (.not. lmerror) then

          !     if the jacobian is not rank deficient, the newton
          !     step provides a lower bound, parl, for the zero of
          !     the function.  Otherwise set this bound to zero.
          parl = zero
          If (nsing >= n) Then
             Do j = 1, n
                l = ipvt(j)
                wa1(j) = diag(l)*(wa2(l)/dxnorm)
             End Do
             Do j = 1, n
                suma = Dot_Product( r(1:j-1,j), wa1(1:j-1) )
                wa1(j) = (wa1(j) - suma)/r(j,j)
             End Do
             temp = norm2(wa1(1:n))
             parl = ((fp/delta)/temp)/temp
          End If

          !     calculate an upper bound, paru, for the zero of the function.
          Do j = 1, n
             suma = Dot_Product( r(1:j,j), qtb(1:j) )
             l = ipvt(j)
             wa1(j) = suma/diag(l)
          End Do
          gnorm = norm2(wa1(1:n))
          paru = gnorm/delta
          If (paru == zero) paru = dwarf/Min(delta,p1)

          !     if the input par lies outside of the interval (parl,paru),
          !     set par to the closer endpoint.
          par = Max(par,parl)
          par = Min(par,paru)
          If (par == zero) par = gnorm/dxnorm

          !     beginning of an iteration.
          Do_Iter: Do
             iter = iter + 1

             !        evaluate the function at the current value of par.
             If (par == zero) par = Max(dwarf, p001*paru)
             temp = Sqrt(par)
             wa1(1:n) = temp*diag(1:n)
             Call qrsolv(n, r, ipvt, wa1, qtb, x, sdiag)
             wa2(1:n) = diag(1:n)*x(1:n)
             dxnorm = norm2(wa2(1:n))
             temp = fp
             fp = dxnorm - delta

             !        if the function is small enough, accept the current value
             !        of par. also test for the exceptional cases where parl
             !        is zero or the number of iterations has reached 10.
             If (Abs(fp) <= p1*delta .OR. parl == zero .AND. fp <= temp  &
                 .AND. temp < zero .OR. iter == 10) Exit Do_Iter

             !        compute the newton correction.
             Do j = 1, n
                l = ipvt(j)
                wa1(j) = diag(l)*(wa2(l)/dxnorm)
             End Do
             Do j = 1, n
                wa1(j) = wa1(j)/sdiag(j)
                temp = wa1(j)
                jp1 = j + 1
                wa1(jp1:n) = wa1(jp1:n) - r(jp1:n,j)*temp
             End Do
             temp = norm2(wa1(1:n))
             parc = ((fp/delta)/temp)/temp

             !        depending on the sign of the function, update parl or paru.
             If (fp > zero) parl = Max(parl,par)
             If (fp < zero) paru = Min(paru,par)

             !        compute an improved estimate for par.
             par = Max(parl, par+parc)

             !        end of an iteration.
          End Do Do_iter
          !     termination.
       End If ! .not. lmerror
       If (iter == 0) par = zero
    End Subroutine lmpar

    !!--++
    !!--++  Subroutine qrfac(m, n, a, pivot, ipvt, rdiag, acnorm)
    !!--++     Integer,                       Intent(In)      :: m
    !!--++     Integer,                       Intent(In)      :: n
    !!--++     Real (Kind=cp),Dimension(:,:), Intent(In Out)  :: a
    !!--++     Logical,                       Intent(In)      :: pivot
    !!--++     Integer,Dimension(:),          Intent(Out)     :: ipvt
    !!--++     Real (Kind=cp),Dimension(:),   Intent(Out)     :: rdiag
    !!--++     Real (Kind=cp),Dimension(:),   Intent(Out)     :: acnorm
    !!--++
    !!--..  Original documentation:
    !!--..
    !!--..  This subroutine uses Householder transformations with column pivoting
    !!--..  (optional) to compute a qr factorization of the m by n matrix a. That is,
    !!--..  qrfac determines an orthogonal matrix q, a permutation matrix p, and an
    !!--..  upper trapezoidal matrix r with diagonal elements of nonincreasing magnitude,
    !!--..  such that a*p = q*r.  The householder transformation for column k,
    !!--..  k = 1,2,...,min(m,n), is of the form
    !!--..
    !!--..                        t
    !!--..        i - (1/u(k))*u*u
    !!--..
    !!--..  where u has zeros in the first k-1 positions.  The form of this transformation
    !!--..  and the method of pivoting first appeared in the corresponding linpack i
    !!--..  subroutine.
    !!--..
    !!--..  The subroutine statement is
    !!--..
    !!--..      subroutine qrfac(m, n, a, lda, pivot, ipvt, lipvt, rdiag, acnorm, wa)
    !!--..
    !!--..  where
    !!--..
    !!--..    m     is a positive integer input variable set to the number of rows of a.
    !!--..
    !!--..    n     is a positive integer input variable set to the number of columns of a.
    !!--..
    !!--..    a     is an m by n array.  On input a contains the matrix for which the qr
    !!--..          factorization is to be computed.  On output the strict upper trapezoidal
    !!--..          part of a contains the strict upper trapezoidal part of r, and the lower
    !!--..          trapezoidal part of a contains a factored form of q (the non-trivial
    !!--..          elements of the u vectors described above).
    !!--..
    !!--..    lda   is a positive integer input variable not less than m which specifies
    !!--..          the leading dimension of the array a.
    !!--..
    !!--..    pivot is a logical input variable.  If pivot is set true, then column pivoting
    !!--..          is enforced.  If pivot is set false, then no column pivoting is done.
    !!--..
    !!--..    ipvt  is an integer output array of length lipvt.  ipvt defines the permutationi
    !!--..          matrix p such that a*p = q*r. Column j of p is column ipvt(j) of the i
    !!--..          identity matrix. If pivot is false, ipvt is not referenced.
    !!--..
    !!--..    lipvt is a positive integer input variable.  If pivot is false, then lipvt may
    !!--..          be as small as 1.  If pivot is true, then lipvt must be at least n.
    !!--..
    !!--..    rdiag is an output array of length n which contains the diagonal elements of
    !!--..          r.
    !!--..
    !!--..   acnorm is an output array of length n which contains the norms of the
    !!--..          corresponding columns of the input matrix a. If this information is not
    !!--..          needed, then acnorm can coincide with rdiag.
    !!--..
    !!--..    wa    is a work array of length n.  If pivot is false, then wa can coincide i
    !!--..          with rdiag.
    !!--..
    !!--..  Argonne National Laboratory. minpack project. march 1980.
    !!--..  Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More
    !!--++
    !!--++
    !!--++ Update: August - 2009
    !!
    Subroutine qrfac(m, n, a, pivot, ipvt, rdiag, acnorm)
       !---- Arguments ----!
       Integer,                       Intent(In)      :: m
       Integer,                       Intent(In)      :: n
       Real (Kind=cp),Dimension(:,:), Intent(In Out)  :: a
       Logical,                       Intent(In)      :: pivot
       Integer,Dimension(:),          Intent(Out)     :: ipvt
       Real (Kind=cp),Dimension(:),   Intent(Out)     :: rdiag
       Real (Kind=cp),Dimension(:),   Intent(Out)     :: acnorm

       !--- Local Variables ---!
       Integer                      :: i, j, jp1, k, kmax, minmn
       Real (Kind=cp)               :: ajnorm, epsmch, suma, temp
       Real (Kind=cp), Dimension(n) :: wa
       Real (Kind=cp), Parameter    :: one = 1.0_CP, p05 = 0.05_CP, zero = 0.0_CP

       !     epsmch is the machine precision.
       epsmch = Epsilon(zero)

       !     compute the initial column norms and initialize several arrays.
       Do j = 1, n
          acnorm(j) = norm2(a(1:m,j))
          rdiag(j) = acnorm(j)
          wa(j) = rdiag(j)
          If (pivot) ipvt(j) = j
       End Do

       !     Reduce A to R with Householder transformations.
       minmn = Min(m,n)
       Do j = 1, minmn
          If (pivot) Then

             !Bring the column of largest norm into the pivot position.
             kmax = j
             Do k = j, n
                If (rdiag(k) > rdiag(kmax)) kmax = k
             End Do

             If (kmax /= j) Then
                Do i = 1, m
                   temp = a(i,j)
                   a(i,j) = a(i,kmax)
                   a(i,kmax) = temp
                End Do
                rdiag(kmax) = rdiag(j)
                wa(kmax) = wa(j)
                k = ipvt(j)
                ipvt(j) = ipvt(kmax)
                ipvt(kmax) = k
             End If
          End If

          !     Compute the Householder transformation to reduce the
          !     j-th column of a to a multiple of the j-th unit vector.
          ajnorm = norm2(a(j:m-j+1,j))
          If (ajnorm == zero) Cycle
          If (a(j,j) < zero) ajnorm = -ajnorm
          a(j:m,j) = a(j:m,j)/ajnorm
          a(j,j) = a(j,j) + one

          !     Apply the transformation to the remaining columns and update the norms.
          jp1 = j + 1
          Do k = jp1, n
             suma = Dot_Product( a(j:m,j), a(j:m,k) )
             temp = suma/a(j,j)
             a(j:m,k) = a(j:m,k) - temp*a(j:m,j)
             If (.NOT. pivot .OR. rdiag(k) == zero) Cycle
             temp = a(j,k)/rdiag(k)
             rdiag(k) = rdiag(k)*Sqrt(Max(zero, one-temp**2))
             If (p05*(rdiag(k)/wa(k))**2 > epsmch) Cycle
             rdiag(k) = norm2(a(jp1:m-j,k))
             wa(k) = rdiag(k)
          End Do
          rdiag(j) = -ajnorm
       End Do

    End Subroutine qrfac

    !!--++
    !!--++ Subroutine qrsolv(n, r, ipvt, diag, qtb, x, sdiag)
    !!--++    Integer,                        Intent(In)      :: n
    !!--++    Real (Kind=cp), dimension(:,:), Intent(In Out)  :: r
    !!--++    Integer, dimension(:),          Intent(In)      :: ipvt
    !!--++    Real (Kind=cp), dimension(:),   Intent(In)      :: diag
    !!--++    Real (Kind=cp), dimension(:),   Intent(In)      :: qtb
    !!--++    Real (Kind=cp), dimension(:),   Intent(Out)     :: x
    !!--++    Real (Kind=cp), dimension(:),   Intent(Out)     :: sdiag
    !!--++
    !!--..    Original documentation:
    !!--..
    !!--..    Given an m by n matrix A, an n by n diagonal matrix D, and an m-vector b,
    !!--..    the problem is to determine an x which solves the system
    !!--..
    !!--..        A*x = b ,     D*x = 0 ,
    !!--..
    !!--..    in the least squares sense.
    !!--..
    !!--..    This subroutine completes the solution of the problem if it is provided
    !!--..    with the necessary information from the QR factorization, with column
    !!--..    pivoting, of A.  That is, if A*P = Q*R, where P is a permutation matrix,
    !!--..    Q has orthogonal columns, and R is an upper triangular matrix with diagonal
    !!--..    elements of nonincreasing magnitude, then qrsolv expects the full upper
    !!--..    triangle of R, the permutation matrix P, and the first n components of
    !!--..    (Q transpose)*b.  The system A*x = b, D*x = 0, is then equivalent to
    !!--..
    !!--..               t       t
    !!--..        R*z = q *b ,  p *d*p*z = 0 ,
    !!--..
    !!--..    where x = p*z. if this system does not have full rank,
    !!--..    then a least squares solution is obtained.  On output qrsolv
    !!--..    also provides an upper triangular matrix s such that
    !!--..
    !!--..         t   t               t
    !!--..        p *(a *a + d*d)*p = s *s .
    !!--..
    !!--..    s is computed within qrsolv and may be of separate interest.
    !!--..
    !!--..    The subroutine statement is
    !!--..
    !!--..        subroutine qrsolv(n, r, ldr, ipvt, diag, qtb, x, sdiag, wa)
    !!--..
    !!--..    where
    !!--..
    !!--..    n     is a positive integer input variable set to the order of r.
    !!--..
    !!--..    r     is an n by n array.  On input the full upper triangle must contain
    !!--..          the full upper triangle of the matrix r.
    !!--..          On output the full upper triangle is unaltered, and the strict lower
    !!--..          triangle contains the strict upper triangle (transposed) of the
    !!--..          upper triangular matrix s.
    !!--..
    !!--..    ldr   is a positive integer input variable not less than n which specifies
    !!--..          the leading dimension of the array r.
    !!--..
    !!--..    ipvt   is an integer input array of length n which defines the permutation
    !!--..           matrix p such that a*p = q*r.  Column j of p is column ipvt(j) of
    !!--..           the identity matrix.
    !!--..
    !!--..    diag   is an input array of length n which must contain the diagonal elements
    !!--..           of the matrix d.
    !!--..
    !!--..    qtb    is an input array of length n which must contain the first n elements
    !!--..           of the vector (q transpose)*b.
    !!--..
    !!--..    x      is an output array of length n which contains the least squares
    !!--..           solution of the system a*x = b, d*x = 0.
    !!--..
    !!--..    sdiag  is an output array of length n which contains the diagonal elements
    !!--..           of the upper triangular matrix s.
    !!--..
    !!--..    wa     is a work array of length n.
    !!--..
    !!--..  Argonne National Laboratory. minpack project. march 1980.
    !!--..  Burton S. Garbow, Kenneth E. Hillstrom, Jorge J. More
    !!--++
    !!--++ Update: August - 2009
    !!
    Subroutine qrsolv(n, r, ipvt, diag, qtb, x, sdiag)
       !---- Arguments ----!
       Integer,                        Intent(In)      :: n
       Real (Kind=cp), dimension(:,:), Intent(In Out)  :: r
       Integer, dimension(:),          Intent(In)      :: ipvt
       Real (Kind=cp), dimension(:),   Intent(In)      :: diag
       Real (Kind=cp), dimension(:),   Intent(In)      :: qtb
       Real (Kind=cp), dimension(:),   Intent(Out)     :: x
       Real (Kind=cp), dimension(:),   Intent(Out)     :: sdiag

       !--- Local Variables ---!
       Integer                      :: i, j, k, kp1, l, nsing
       Real (Kind=cp)               :: cosi, cotan, qtbpj, sini, suma, tani, temp
       Real (Kind=cp), dimension(n) ::  wa
       Real (Kind=cp), Parameter :: p5 = 0.5_CP, p25 = 0.25_CP, zero = 0.0_CP

       !     Copy r and (q transpose)*b to preserve input and initialize s.
       !     In particular, save the diagonal elements of r in x.

       Do j = 1, n
          r(j:n,j) = r(j,j:n)
          x(j) = r(j,j)
          wa(j) = qtb(j)
       End Do

       !     Eliminate the diagonal matrix d using a givens rotation.
       Do j = 1, n

          !        Prepare the row of d to be eliminated, locating the
          !        diagonal element using p from the qr factorization.
          l = ipvt(j)
          If (diag(l) == zero) Cycle
          sdiag(j:n) = zero
          sdiag(j) = diag(l)

          !     The transformations to eliminate the row of d modify only a single
          !     element of (q transpose)*b beyond the first n, which is initially zero.
          qtbpj = zero
          Do k = j, n

             !        Determine a givens rotation which eliminates the
             !        appropriate element in the current row of d.
             If (sdiag(k) == zero) Cycle
             If (Abs(r(k,k)) < Abs(sdiag(k))) Then
                cotan = r(k,k)/sdiag(k)
                sini = p5/Sqrt(p25 + p25*cotan**2)
                cosi = sini*cotan
             Else
                tani = sdiag(k)/r(k,k)
                cosi = p5/Sqrt(p25 + p25*tani**2)
                sini = cosi*tani
             End If

             !        Compute the modified diagonal element of r and
             !        the modified element of ((q transpose)*b,0).
             r(k,k) = cosi*r(k,k) + sini*sdiag(k)
             temp = cosi*wa(k) + sini*qtbpj
             qtbpj = -sini*wa(k) + cosi*qtbpj
             wa(k) = temp

             !        Accumulate the tranformation in the row of s.
             kp1 = k + 1
             Do i = kp1, n
                temp = cosi*r(i,k) + sini*sdiag(i)
                sdiag(i) = -sini*r(i,k) + cosi*sdiag(i)
                r(i,k) = temp
             End Do
          End Do

          !     Store the diagonal element of s and restore
          !     the corresponding diagonal element of r.
          sdiag(j) = r(j,j)
          r(j,j) = x(j)
       End Do

       !     Solve the triangular system for z.  If the system is singular,
       !     then obtain a least squares solution.
       nsing = n
       Do j = 1, n
          If (sdiag(j) == zero .AND. nsing == n) nsing = j - 1
          If (nsing < n) wa(j) = zero
       End Do

       Do k = 1, nsing
          j = nsing - k + 1
          suma = Dot_Product( r(j+1:nsing,j), wa(j+1:nsing) )
          wa(j) = (wa(j) - suma)/sdiag(j)
       End Do

       !     Permute the components of z back to components of x.
       Do j = 1, n
          l = ipvt(j)
          x(l) = wa(j)
       End Do

    End Subroutine qrsolv

    !!----
    !!---- Subroutine Modify_Codes_State_Vector(Lcodes,Multip,Lcode_max)
    !!----   integer,       dimension(:),intent (in out) :: Lcodes
    !!----   real(kind=cp), dimension(:),intent (in out) :: Multip
    !!----   integer,                    intent (in out) :: Lcode_max
    !!----
    !!----  This subroutine should be called with the arguments corresponding to
    !!----  the components  Lcodes=LSQ_State_Vector%code, Multip=LSQ_State_Vector%mul
    !!----  and Lcode_max=maxval(LSQ_State_Vector%code) before using the LSQ methods
    !!----  in which and object of LSQ_State_Vector_type exist and LSQ_State_Vector%code_comp=.true.
    !!----  This allows the use of constraints once the codes have been attributed.
    !!----
    Subroutine Modify_Codes_State_Vector(Lcodes,Multip,Lcode_max)
       !---- Arguments ----!
       integer,       dimension(:),intent (in out) :: Lcodes
       real(kind=cp), dimension(:),intent (in out) :: Multip
       integer,                    intent (in out) :: Lcode_max

       !--- Local variables ---!
       integer                       ::  k, L , j, n_given, Lcm, n_att,ndisp, nn, ni, ndispm,maxs
       integer, dimension(Lcode_max) :: dispo
       real(kind=cp), parameter      :: e=0.001

       !> Check correlated parameters and already used codes
       ndisp=0
       n_given=0
       dispo(:)=0
       Lcm=size(Lcodes)
       !> First Pass
       Do L=1, Lcode_max
          ni=0
          do k=1, Lcm
             if (L == Lcodes(k)) ni=ni+1
          end do
          if (ni == 0) then
             ndisp=ndisp+1   !number of disponible codes
             dispo(ndisp)=L  !disponible code number
          else
             n_given=n_given+1  !number of given codes
          end if
       end do !=1,Lcode_max

       if (ndisp == 0) return  !all codes have been attributed

       !> Attributing numbers to parameters (not already attributed) with multipliers
       !> different from zero. First the attribution is taken from the vector dispo() and
       !> continued, after finishing the disponible codes, from Lcode_max+1, ...
       !> If after attributing the codes ndisp /=0, then a displacement of all parameters
       !> is done and the maximum number of parameters to be refined is diminished by
       !> ndisp
       ndispm=ndisp !number of disponible codes before attributing code numbers
       ni=0
       nn=Lcode_max
       n_att=0
       do j=1,Lcm
          if (abs(Multip(j)) > e .and. Lcodes(j) == 0 ) then
             if (abs(Multip(j)) > 1.001) then
                Multip(j)=sign(1.0_cp,Multip(j))*(abs(Multip(j))-1.0)
             end if
             if (ndisp==0) then
                nn=nn+1
                Lcodes(j)=nn
                n_att=n_att+1
             else
                ni=ni+1
                Lcodes(j)=dispo(ni)
                n_att=n_att+1
                ndisp=ndisp-1
             end if
          end if
       end do

       if (ndisp == 0) then
          if (nn > Lcode_max) Lcode_max=nn
          return  !all parameters have been attributed
       end if

       maxs=n_given+n_att    !Number of refined parameters (given + attributed)

       !> Third Pass ndisp /=0 => Displacement of codes needed to avoid holes in the matrix.
       n_att=ndispm-ndisp+1
       do L=Lcode_max, maxs+1,-1
          nn=0
          do j =1,Lcm
             if (L == Lcodes(j)) then
                Lcodes(j)=dispo(n_att)
                nn=nn+1
             end if
          end do
          if (nn == 0) cycle
          n_att=n_att+1
          if(n_att > ndispm) exit
       end do !i=Lcode_max,maxs+1,-1
       Lcode_max=maxs

    End Subroutine Modify_Codes_State_Vector

 End Module CFML_Optimization_LSQ
