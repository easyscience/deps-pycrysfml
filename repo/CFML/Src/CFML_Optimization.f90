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
!!---- MODULE: CFML_Optimization_General
!!----   INFO: Module implementing several algorithms for global and local
!!----         optimization.
!!----
!!---- HISTORY
!!----    Update: 04/03/2011
!!----
!!----    April - 2004 Created by JRC
!!---- DEPENDENCIES
!!--++    use CFML_GlobalDeps,       only: cp
!!--++    use CFML_String_Utilities, only: l_case
!!----
!!---- VARIABLES
!!--++    METHOD                               [Private]
!!----    OPT_CONDITIONS_TYPE
!!----
!!---- PROCEDURES
!!----    Functions:
!!----
!!----    Subroutines:
!!----       CG_QUASI_NEWTON
!!----       CSENDES_GLOBAL
!!--++       FUN                               [Private]
!!----       INIT_OPT_CONDITIONS
!!--++       LOCAL_DFP
!!----       LOCAL_MIN_DFP
!!----       LOCAL_MIN_RAND
!!----       LOCAL_OPTIMIZE
!!----       NELDER_MEAD_SIMPLEX
!!--++       PRINT_TRI_MATRIX                  [Private]
!!--++       PUT_IN_BOX                        [Private]
!!----       SET_OPT_CONDITIONS
!!----       WRITE_OPTIMIZATION_CONDITIONS
!!----
!!
 Module CFML_Optimization
    !---- Use Files ----!
    use CFML_GlobalDeps, only: cp, dp, Err_CFML, Clear_Error
    use CFML_Strings,    only: l_case

    implicit none

    private

    !---- List of public functions ----!

    !---- List of public overloaded procedures: functions ----!

    !---- List of public subroutines ----!
    public :: Csendes_Global,Cg_Quasi_Newton, Init_Opt_Conditions, Nelder_Mead_Simplex, &
              Local_Min_DFP, Local_Min_Rand, Set_Opt_Conditions, Local_Optimize, &
              Write_Optimization_Conditions

    !---- List of public overloaded procedures: subroutines ----!

    !---- List of private functions ----!

    !---- List of private subroutines ----!
    private :: Fun, Local_DFP, Put_In_Box

    !---- Variable Definitions ----!

    !!--++
    !!--++ METHOD
    !!--++    Character(len=*),parameter, private, dimension(7) :: method
    !!--++
    !!--++    Character Variable to store the different methods available in the module.
    !!--++
    !!--++ Update: August - 2007
    !!--++

    Character(len=*),parameter, private, dimension(7) :: method = [  "conjugate_gradient", &
                                                                     "bfgs_quasi_newton ", &
                                                                     "simplex           ", &
                                                                     "dfp_no-derivatives", &
                                                                     "global_csendes    ", &
                                                                     "local_random      ", &
                                                                     "unirandi          "]


    !!----
    !!---- TYPE :: OPT_CONDITIONS_TYPE
    !!--<<
    !!---- Type, public :: Opt_Conditions_Type
    !!----    character(len=20) :: method ! "Conjugate_Gradient","BFGS_Quasi_Newton",
    !!----                                  "Simplex","DFP_no-derivatives","Global_Csendes",
    !!----                                  "Local_Random","UniRandi"
    !!----    integer           :: nmeth  ! (in)  nmeth=0 => conjugate gradient
    !!----                                !       nmeth=1 => the BFGS method is used
    !!----    integer           :: npar   ! (in)  Number of free parameters
    !!----    integer           :: mxfun  ! (in)  Maximum number function calls
    !!----    integer           :: iout   ! (in)  Printing parameter,
    !!----                                !       if iout= 0 no printing for Quasi_Newton & Conjugate Gradient
    !!----                                !       if iout= 0 partial printing for Simplex (<0 no printing)
    !!----                                !       if iout>0 printing each iout iterations/evaluations.
    !!----    integer           :: loops  ! (in)  Useful for SIMPLEX method: = 0
    !!----                                !
    !!----    integer           :: iquad  ! (in)  For SIMPLEX, if iquad/= 0 fitting to a quadratic
    !!----                                !
    !!----    integer           :: nflag  ! (out) Flag value states which condition caused the exit of
    !!----                                !       the optimization subroutine
    !!----                                !       If NFLAG=0, the algorithm has converged.
    !!----                                !       If NFLAG=1, the maximum number of function
    !!----                                !          evaluations have been used.
    !!----                                !       If NFLAG=2, the linear search has failed to
    !!----                                !          improve the function value. This is the
    !!----                                !          usual exit if either the function or the
    !!----                                !          gradient is incorrectly coded.
    !!----                                !       If NFLAG=3, The search vector was not
    !!----                                !          a descent direction. This can only be caused
    !!----                                !          by roundoff,and may suggest that the
    !!----                                !          convergence criterion is too strict.
    !!----    integer           :: ifun   ! (out) Total number of function and gradient evaluations
    !!----    integer           :: iter   ! (out) Total number of search directions used in the algorithm
    !!----    real(kind=cp)     :: eps    ! (in)  Convergence occurs when the norm of the gradient
    !!----                                !       is less than or equal to EPS times the maximum
    !!----                                !       of one and the norm of the vector X
    !!----                                !       Initialized to 1.0E-6
    !!----    real(kind=cp)     :: acc    ! (in)  ACC is a user supplied estimate of machine
    !!----                                !       accuracy. A linear search is unsuccessfully
    !!----                                !       terminated when the norm of the step size
    !!----                                !       becomes smaller than ACC. In practice,
    !!----                                !       ACC=10.0E-20 has proved satisfactory.
    !!----                                !       This is the default value.
    !!----                                !       For Simplex method the meaning is different
    !!----                                !       (see below) and this should be changed to 1.0e-6
    !!---- End Type Opt_Conditions_Type
    !!----
    !!----    This TYPE has been introduced to simplify the call to optimization
    !!----    procedures. It contains the optimization parameters useful for different
    !!----    algorithms.
    !!----    All integer components are initialized to zero and the real components
    !!----    are initilized as indicated below.
    !!----    A variable of this type should be defined by the user and all their
    !!----    input parameters (in) must be provided before calling the procedures.
    !!----    On output from the procedure the (out) items are provided for checking.
    !!-->>
    !!---- Update: February - 2005
    !!
    Type, public :: Opt_Conditions_Type
       character(len=20) :: method
       integer           :: nmeth
       integer           :: npar
       integer           :: mxfun
       integer           :: loops
       integer           :: iquad
       integer           :: iout
       integer           :: nflag
       integer           :: ifun
       integer           :: iter
       real(kind=cp)     :: eps
       real(kind=cp)     :: acc
    End Type Opt_Conditions_Type

  Interface

    Module Subroutine Cg_Quasi_Newton(Model_Functn, N, X, F, G, C, Ipr)
       !---- Arguments ----!
       integer,                    intent(in)       :: n
       real(kind=cp),dimension(n), intent(in out)   :: x
       real(kind=cp),              intent(   out)   :: f
       real(kind=cp),dimension(n), intent(   out)   :: g
       type(Opt_conditions_Type),  intent(in out)   :: C
       integer, optional,          intent(in)       :: Ipr

       Interface
          Subroutine Model_Functn(n,x,f,g)
             use CFML_GlobalDeps,  only: cp
             integer,                             intent(in) :: n
             real(kind=cp),dimension(:),          intent(in) :: x
             real(kind=cp),                       intent(out):: f
             real(kind=cp),dimension(:),optional, intent(out):: g
          End Subroutine Model_Functn
       End Interface
    End Subroutine Cg_Quasi_Newton

    Module Subroutine Csendes_Global(Model_Functn, Mini, Maxi, Nparm, Nsampl, Nsel, Nsig, X0, Nc, F0, Ipr,mode)
       !---- Arguments ----!
       real(kind=cp), dimension(:),  intent(in)      :: mini    !Nparm
       real(kind=cp), dimension(:),  intent(in)      :: maxi    !Nparm
       integer,                      intent(in)      :: nparm
       integer,                      intent(in out)  :: Nsampl  !100*nparm
       integer,                      intent(in out)  :: nsel    ! 2times number of expected local minima
       integer,                      intent(in)      :: nsig
       real(kind=cp), dimension(:,:),intent(in out)  :: x0      ! nparm x Nc
       integer,                      intent(out)     :: nc
       real(kind=cp), dimension(:),  intent(in out)  :: f0      !Nc
       integer,                      intent(in)      :: ipr
       integer, optional,            intent(in)      :: mode

       Interface
          Subroutine Model_Functn(n,x,f,g)
             use CFML_GlobalDeps,  only: cp
             integer,                             intent(in) :: n
             real(kind=cp),dimension(:),          intent(in) :: x
             real(kind=cp),                       intent(out):: f
             real(kind=cp),dimension(:),optional, intent(out):: g
          End Subroutine Model_Functn
       End Interface
    End Subroutine Csendes_Global

    Module Subroutine Local_DFP(N, Eps, Maxfn, X, F, Nfev, Mmin, Mmax, Model_Functn)
       !---- Arguments ----!
       integer,                     intent(in)      :: n
       real(kind=cp),               intent(in)      :: eps
       integer,                     intent(in)      :: maxfn
       real(kind=cp), dimension(:), intent(in out)  :: x
       real(kind=cp),               intent(out)     :: f
       integer,                     intent(out)     :: nfev
       real(kind=cp), dimension(:), intent(in)      :: mmin
       real(kind=cp), dimension(:), intent(in)      :: mmax

       Interface
          Subroutine Model_Functn(n,x,f,g)
             use CFML_GlobalDeps,  only: cp
             integer,                             intent(in) :: n
             real(kind=cp),dimension(:),          intent(in) :: x
             real(kind=cp),                       intent(out):: f
             real(kind=cp),dimension(:),optional, intent(out):: g
          End Subroutine Model_Functn
       End Interface

    End Subroutine Local_DFP

    Module Subroutine Local_Min_DFP(Model_Functn, N, X, F, C, mini, maxi, Ipr)
       !---- Arguments ----!
       integer,                     intent(in)      :: n
       real(kind=cp), dimension(:), intent(in out)  :: x
       real(kind=cp),               intent(out)     :: f
       type(Opt_conditions_Type),   intent(in out)  :: C
       real(kind=cp), dimension(:), intent(in)      :: mini
       real(kind=cp), dimension(:), intent(in)      :: maxi
       integer, optional,           intent(in)      :: Ipr

       Interface
          Subroutine Model_Functn(n,x,f,g)
             use CFML_GlobalDeps,  only: cp
             integer,                             intent(in) :: n
             real(kind=cp),dimension(:),          intent(in) :: x
             real(kind=cp),                       intent(out):: f
             real(kind=cp),dimension(:),optional, intent(out):: g
          End Subroutine Model_Functn
       End Interface
    End Subroutine Local_Min_DFP

    Module Subroutine Local_Min_Rand(Model_Functn,N,X,F,C,Mini,Maxi)
       !---- Arguments ----!
       integer,                               intent(in)      :: n
       real(kind=cp), dimension(:),           intent(in out)  :: x
       real(kind=cp),                         intent(out)     :: f
       type(Opt_conditions_Type),             intent(in out)  :: C
       real(kind=cp), dimension(:), optional, intent(in)      :: mini
       real(kind=cp), dimension(:), optional, intent(in)      :: maxi

       Interface
          Subroutine Model_Functn(n,x,f,g)
             use CFML_GlobalDeps,  only: cp
             integer,                             intent(in) :: n
             real(kind=cp),dimension(:),          intent(in) :: x
             real(kind=cp),                       intent(out):: f
             real(kind=cp),dimension(:),optional, intent(out):: g
          End Subroutine Model_Functn
       End Interface
    End Subroutine Local_Min_Rand

    Module Subroutine Nelder_Mead_Simplex(Model_Functn, Nop, P, Step, Var, Func, C, Ipr)
       !---- Arguments ----!
       integer,                      intent(in)      :: nop
       real(kind=cp), dimension(:),  intent(in out)  :: p, step
       real(kind=cp), dimension(:),  intent(out)     :: var
       real(kind=cp),                intent(out)     :: func
       type(opt_conditions_Type),    intent(in out)  :: c
       integer, optional,            intent(in)      :: Ipr

       Interface
          Subroutine Model_Functn(n,x,f,g)
             use CFML_GlobalDeps,  only: cp
             integer,                             intent(in) :: n
             real(kind=cp),dimension(:),          intent(in) :: x
             real(kind=cp),                       intent(out):: f
             real(kind=cp),dimension(:),optional, intent(out):: g
          End Subroutine Model_Functn
       End Interface
    End Subroutine Nelder_Mead_Simplex

  End Interface

  contains

    !!--++
    !!--++ Subroutine Fun(R, F, Nparm, M, Mini, Maxi, Model_Functn)
    !!--++
    !!--++    (Private)
    !!--++    Subroutine used by Csendes, Local_DFP, ...
    !!--++
    !!--++ Update: February - 2005
    !!
    Subroutine Fun(R, F, Nparm, Mini, Maxi, Model_Functn)
       !---- Arguments ----!
       real(kind=cp),dimension(:),  intent(in)      :: r
       real(kind=cp),               intent(in out)  :: f
       integer,                     intent(in)      :: nparm
       real(kind=cp),dimension(:),  intent(in)      :: mini
       real(kind=cp),dimension(:),  intent(in)      :: maxi

       Interface
          Subroutine Model_Functn(n,x,f,g)
             use CFML_GlobalDeps,  only: cp
             integer,                             intent(in) :: n
             real(kind=cp),dimension(:),          intent(in) :: x
             real(kind=cp),                       intent(out):: f
             real(kind=cp),dimension(:),optional, intent(out):: g
          End Subroutine Model_Functn
       End Interface

       !---- Local Variables ----!
       real(kind=cp), dimension(nparm)    :: x

       x(1:nparm) = maxi(1:nparm) * r(1:nparm) + mini(1:nparm)
       call Model_Functn(nparm,x, f)

    End Subroutine Fun

    !!----
    !!---- Subroutine Local_Optimize(Model_Functn,X,F,C,G,mini,maxi,v,Ipr)
    !!----    real(kind=cp),dimension(:),           intent(in out) :: x  ! The vector containing the current estimate to
    !!----                                                               ! the minimizer.
    !!----                                                               ! In -> Must contain an initial estimate supplied by the user.
    !!----                                                               ! Out-> X will hold the best estimate to the minimizer obtained
    !!----    real(kind=cp),                        intent(   out) :: f  ! Out-> F will contain the lowest value of the object function obtained.
    !!----    type(Opt_conditions),                 intent(in out) :: C  ! Conditions for the algorithm. C is of type(Opt_conditions)
    !!----    real(kind=cp),dimension(:), optional, intent(in out) :: g  ! Out-> G =(g(1),...g(n)) will contain the elements of the gradient of
    !!----                                                               !       F evaluated at the point contained in X=(x(1),...x(N))
    !!----                                                               ! For "simplex" it contains the step values
    !!----    real(kind=cp),dimension(:), optional, intent(in out) ::mini! Interval where the minimum is expected to be
    !!----                                                           maxi!
    !!----    real(kind=cp),dimension(:), optional, intent(in out) :: v  ! For "simplex" it contains the sigma of parameters
    !!----    integer, optional,                    intent(in)     :: ipr! Logical unit for printing if the parameter C%IOUT /= 0.
    !!----
    !!--<<    Interface
    !!----       Subroutine Model_Functn(n,x,f,g)
    !!----          use CFML_GlobalDeps, only:cp
    !!----          integer,                    intent(in)     :: n
    !!----          real(kind=cp),dimension(:), intent(in)     :: x
    !!----          real(kind=cp),              intent(out)    :: f
    !!----          real(kind=cp),dimension(:), intent(out)    :: g
    !!----       End Subroutine Model_Functn
    !!-->>    End Interface
    !!----
    !!----    Wraper for selection an optimization method of the function Model_Funct
    !!----    The list of free parameters are provided in the vector X (in out)
    !!----    The value of the function F, and eventually the gradient G, are output
    !!----    variables. The optimization conditions in the variable C should be
    !!----    provided for selecting the optimization algorithm
    !!----
    !!---- Update: February - 2005
    !!

    Subroutine Local_Optimize(Model_Functn,X,F,C,G,mini,maxi,v,Ipr)
       real(kind=cp),dimension(:),           intent(in out) :: x
       real(kind=cp),                        intent(   out) :: f
       type(Opt_conditions_type),            intent(in out) :: C
       real(kind=cp),dimension(:), optional, intent(in out) :: g
       real(kind=cp),dimension(:), optional, intent(in out) :: mini,maxi
       real(kind=cp),dimension(:), optional, intent(   out) :: v
       integer, optional,                    intent(in)     :: ipr

       Interface
          Subroutine Model_Functn(n,x,f,g)
             use CFML_GlobalDeps,  only: cp
             integer,                             intent(in) :: n
             real(kind=cp),dimension(:),          intent(in) :: x
             real(kind=cp),                       intent(out):: f
             real(kind=cp),dimension(:),optional, intent(out):: g
          End Subroutine Model_Functn
       End Interface

       !---- Local Variables ----!
       integer                          :: npar, idev
       character(len=20)                :: cmeth
       real(kind=cp),dimension(C%npar)  :: gg, var, mmin, mmax

       cmeth=l_case(adjustl(C%method))
       npar=c%npar
       if(present(Ipr)) then
         idev=ipr
       else
         idev=6
       end if

       if(present(mini) .and. present(maxi)) then
         mmin=mini(1:npar)
         mmax=maxi(1:npar)
       else
         mmin= x(1:npar)-0.50*x(1:npar)
         mmax= x(1:npar)+0.50*x(1:npar)
       end if

       if(present(g)) then
         gg=g(1:npar)
       else
         gg=0.01
       end if

       Select Case(trim(cmeth))

         Case("conjugate_gradient")
           c%nmeth=0
           Call CG_quasi_newton(Model_Functn,npar,x,f,gg,c,idev)
           if(present(g)) g(1:npar)=gg

         Case("bfgs_quasi_newton")
           c%nmeth=1
           Call CG_quasi_newton(Model_Functn,npar,x,f,gg,c,idev)
           if(present(g)) g(1:npar)=gg

         Case("simplex")
           Call Nelder_Mead_Simplex(Model_Functn,npar, x, gg, var, f, c,  idev)
           if(present(g)) g(1:npar)=gg

         Case("dfp_no-derivatives")
           Call Local_Min_DFP(Model_Functn,npar, x, f, c, mmin,mmax)

         Case("local_random","unirandi")
           CAll Local_Min_Rand(Model_Functn,npar,x,f,C,mmin,mmax)

         Case Default
           Call Nelder_Mead_Simplex(Model_Functn,npar, x, gg, var, f, c,  idev)
           if(present(g)) g(1:npar)=gg
           if(present(v)) v(1:npar)=var

       End Select
    End Subroutine Local_Optimize

    !!--++
    !!--++  Subroutine Put_In_box(n,mini,maxi,x)
    !!--++    integer,            intent(in)     :: n
    !!--++    real, dimension(:), intent(in)     :: mini,maxi
    !!--++    real, dimension(:), intent(in out) :: x
    !!--++
    !!--++  Procedure to apply Box constraints to a vector of arbitrary dimension
    !!--++  If any of the componet of the vector is outside the box, the component
    !!--++  is fixed to the closest box limit
    !!--++
    !!--++ Update: August - 2007
    !!
    Subroutine Put_In_box(n,mini,maxi,x)
      integer,                     intent(in)     :: n
      real(kind=cp), dimension(:), intent(in)     :: mini,maxi
      real(kind=cp), dimension(:), intent(in out) :: x

      integer                            :: i
      do i=1,n
        if(x(i) < mini(i)) x(i)=mini(i)
        if(x(i) > maxi(i)) x(i)=maxi(i)
      end do

    End Subroutine Put_In_box

    !!----
    !!---- Subroutine Init_Opt_Conditions(Opt)
    !!----    type(Opt_Conditions_Type), intent(out) :: Opt
    !!----
    !!----    Initialize the variable Opt
    !!----
    !!---- Update: February - 2005
    !!
    Subroutine Init_Opt_Conditions(Opt)
       !---- Arguments ----!
       type(Opt_Conditions_Type), intent(in out) :: Opt
      !                       nmeth mxfun loops iquad iout nflag(out) ifun(out) iter(out)   eps       acc
       Opt=Opt_Conditions_Type("Simplex",0, 0,   1000, 2000,  1,    0,     0,        0,         0,  1.0e-6_cp,1.0e-20_cp)

    End Subroutine Init_Opt_Conditions

    !!----
    !!---- Subroutine Set_Opt_Conditions(n,file_lines,Opt)
    !!----    integer,                        intent( in)  :: n
    !!----    character(len=*), dimension(:), intent( in)  :: file_lines
    !!----    type(Opt_Conditions_Type),      intent(out)  :: Opt
    !!----
    !!----
    !!----    Get the optimization conditions from a list of text lines obtained
    !!----    from the input file
    !!----
    !!---- Update: February - 2005
    !!
    Subroutine Set_Opt_Conditions(n,file_lines,Opt)
       !---- Arguments ----!
       integer,                        intent( in)  :: n
       character(len=*), dimension(:), intent( in)  :: file_lines
       type(Opt_Conditions_Type),      intent(out)  :: Opt
       !
       !--- Local variables ----!
       integer :: i, j, k, ier, ipos
       real    :: rval
       character(len=len(file_lines)) :: linefil
       logical                        :: mth
       Character(len=*),parameter, dimension(8) :: keyword = (/ "method","params","maxfun", &
                               "iloops", "quadra","output","ep_sil","accept"/)


      ! nmeth mxfun loops iquad iout nflag(out) ifun(out) iter(out) eps  acc

        Opt=Opt_Conditions_Type("Simplex",0,0,1000, 2000, 1, 0, 0, 0, 0,  1.0e-6_cp,1.0e-20_cp)

        call Clear_Error()
        mth=.false.

        do i=1,n
           if (file_lines(i)(1:10) == "OPTIM_COND") then
              opt%method=adjustl(file_lines(i)(11:))
              linefil=l_case(opt%method)

              do j=1,7
                 if (index(linefil,trim(method(j))) /= 0) then
                    mth=.true.
                    exit
                 end if
              end do
              if (.not. mth) then
                 Err_CFML%Ierr=1
                 Err_CFML%Msg="ERROR, no optimization method has been provided!"
                 return
              end if

              do j=i+1,n
                 do k=1,8
                    linefil=l_case(file_lines(j))
                    ipos=index(linefil,keyword(k))
                    if (ipos /= 0) then
                       read(unit=file_lines(j)(ipos+7:), fmt=*, iostat=ier) rval
                       if (ier /= 0) then
                          Err_CFML%Ierr=1
                          write(unit=Err_CFML%Msg,fmt="(a)") "ERROR reading optimization condition: "//keyword(k)
                          return
                       end if

                       Select Case (k)
                          Case(1)
                             Opt%nmeth=nint(rval)
                          Case(2)
                             Opt%npar=nint(rval)
                          Case(3)
                             Opt%mxfun=nint(rval)
                          Case(4)
                             Opt%loops=nint(rval)
                          Case(5)
                             Opt%iquad=nint(rval)
                          Case(6)
                             Opt%iout=nint(rval)
                          Case(7)
                             Opt%eps=rval
                          Case(8)
                             Opt%acc=rval
                       End Select
                    end if
                 end do
              end do
              exit
           end if
        end do

     End Subroutine Set_Opt_Conditions

    !!----
    !!---- Subroutine Write_Optimization_Conditions(ipr,c)
    !!----    integer,                  intent(in)  :: ipr !Logical unit for writing
    !!----    type(Opt_Conditions_type),intent(in)  :: c   !Opt Conditions
    !!----
    !!---- Subroutine for Writing in unit=ipr the Opt_Conditions_type
    !!---- variable "c"
    !!----
    !!---- Update: August - 2007
    !!

    Subroutine Write_Optimization_Conditions(ipr,c)
       !---- Arguments ----!
       integer,                  intent(in)  :: ipr
       type(Opt_Conditions_type),intent(in)  :: c

       !--- Local Variables ---!
       character(len=20) :: line

       line=l_case(c%method)
       write(unit=ipr,fmt="(/,a)") "               ============================="
       write(unit=ipr,fmt="(a)")   "               =  OPTIMIZATION CONDITIONS  ="
       write(unit=ipr,fmt="(a,/)") "               ============================="

       write(unit=ipr,fmt="(a)")        " =>               Optimization method name: "//trim(c%method)
       write(unit=ipr,fmt="(a,i4  )")   " =>                       Value of c%nmeth: ",c%nmeth
       write(unit=ipr,fmt="(a,i4  )")   " =>              Number of free parameters: ",c%npar
       write(unit=ipr,fmt="(a,i8)")     " => Maximum number of function evaluations: ",c%mxfun
       write(unit=ipr,fmt="(a,g12.4)")  " =>            Convergenge criterion (eps): ",c%eps
       write(unit=ipr,fmt="(a,g12.4)")  " =>           Machine accuracy (empirical): ",c%acc

       if (trim(line) == "simplex") then
          write(unit=ipr,fmt="(a,i4)")   " =>         Fitting to a quadratic surface: ",c%iquad
          write(unit=ipr,fmt="(a,g12.4)")" =>    Criterion for expanding the simplex: ",c%acc
          write(unit=ipr,fmt="(a,i4,a) ")" =>            Stopping rule applied after: ",c%loops," function evaluations"
          if (c%iout < 0) then
             write(unit=ipr,fmt="(a,i4)")   " =>  No printing during the optimization procedure "
          else if (c%iout == 0) then
             write(unit=ipr,fmt="(a,i4)")   " =>  Partial printing during the optimization procedure "
          else
             write(unit=ipr,fmt="(a,i4)")   " =>  Printing during the optimization procedure "
          end if
       else
          write(unit=ipr,fmt="(a,g12.4)")  " =>  Convergence (gradient<eps*max(1,norm(x)), eps: ",c%eps
          write(unit=ipr,fmt="(a,g12.4)")  " =>           Machine accuracy (empirical): ",c%acc
          if (c%iout == 0) then
             write(unit=ipr,fmt="(a,i4)")   " =>  No printing during the optimization procedure "
          else
             write(unit=ipr,fmt="(a,i4)")   " =>  Printing during the optimization procedure "
          end if
       end if

    End Subroutine Write_Optimization_Conditions

 End Module CFML_Optimization
