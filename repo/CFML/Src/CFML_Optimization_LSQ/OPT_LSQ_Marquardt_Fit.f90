 Submodule (CFML_Optimization_LSQ) OPT_LSQ_Marquard_Fit
   implicit none
 contains

    !!--++
    !!--++  Subroutine Curfit_v1(Model_Functn, X, Y, W, Nobs, c, A, Sa, Fl, Yc, Chir, Ifail,nt)
    !!--++     real(kind=cp),    dimension(:),      intent(in)      :: x     !vector with abcisae
    !!--++     real(kind=cp),    dimension(:),      intent(in)      :: y     !Observed values
    !!--++     real(kind=cp),    dimension(:),      intent(in out)  :: w     !weight of observations
    !!--++     integer,                             intent(in)      :: nobs  !number of observations
    !!--++     Type(LSQ_Conditions_type),           intent(in)      :: c     !conditions for refinement
    !!--++     real(kind=cp),dimension(:),          intent(in out)  :: a     !vector of parameter
    !!--++     real(kind=cp),dimension(:),          intent(in out)  :: sa    !estimated standard deviations
    !!--++     real(kind=cp),                       intent(in out)  :: fl    !Marquardt LAMBDA value
    !!--++     real(kind=cp),dimension(:),          intent(out)     :: yc    !Calculated
    !!--++     real(kind=cp),                       intent(out)     :: chir  !Chi-square
    !!--++     integer,                             intent(out)     :: ifail !If /= 0 the algorithm failed
    !!--++     integer,                             intent(out)     :: nt    ! Number of trials
    !!--++
    !!--++     Interface
    !!--++      Subroutine Model_Functn(iv,Xv,ycalc,aa,der)
    !!--++         use CFML_GlobalDeps, only: cp
    !!--++         integer,                             intent(in) :: iv
    !!--++         real(kind=cp),                       intent(in) :: xv
    !!--++         real(kind=cp),dimension(:),          intent(in) :: aa
    !!--++         real(kind=cp),                       intent(out):: ycalc
    !!--++         real(kind=cp),dimension(:),optional, intent(out):: der
    !!--++      End Subroutine Model_Functn
    !!--++     End Interface
    !!--++
    !!--++ Update: February - 2003
    !!
    Subroutine Curfit_v1(Model_Functn, X, Y, W, Nobs, c, A, Sa, Fl, Yc, Chir, Ifail,nt)
       !---- Arguments ----!
       real(kind=cp),    dimension(:),      intent(in)      :: x     !vector with abcisae
       real(kind=cp),    dimension(:),      intent(in)      :: y     !Observed values
       real(kind=cp),    dimension(:),      intent(in out)  :: w     !weight of observations
       integer,                             intent(in)      :: nobs  !number of observations
       Type(LSQ_Conditions_type),           intent(in out)  :: c     !conditions for refinement
       real(kind=cp),dimension(:),          intent(in out)  :: a     !vector of parameter
       real(kind=cp),dimension(:),          intent(in out)  :: sa    !estimated standard deviations
       real(kind=cp),                       intent(in out)  :: fl    !Marquardt LAMBDA value
       real(kind=cp),dimension(:),          intent(out)     :: yc    !Calculated
       real(kind=cp),                       intent(out)     :: chir
       integer,                             intent(out)     :: ifail
       integer,                             intent(out)     :: nt

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

       !---- Local variables ----!
       logical                          :: change_par
       integer                          :: ntrials
       integer                          :: nfr,j,i,k,ntr
       real(kind=cp)                    :: chisq1
       real(kind=cp), dimension(c%npvar):: beta, b, der


       ntrials=40
       call clear_error()
       !---- Optimization with MARQUARDT ALGORITHM ----!
       nfr=nobs-c%npvar
       nt=0
       ifail=0
       change_par=.false.
       if (nobs > 2000) ntrials= 20

       !---- Evaluate ALFA and BETA matrices       ----!
       !---- (BETA=DP*ALFA)--- DP=BETA*ALFA(-1)    ----!
       beta(:) =0.0
       curv_mat(:,:)=0.0


       do i=1,nobs
          call model_functn(i,x(i),yc(i),a,der)
          if (c%iw == 1 .and. yc(i) /= 0.0) w(i)=1.0/yc(i)
          do j=1,c%npvar
             beta(j)=beta(j)+w(i)*(y(i)-yc(i))*der(j)
             do k=1,j
                curv_mat(j,k)=curv_mat(j,k)+w(i)*der(j)*der(k)
             end do
          end do
       end do

       do i=1,c%npvar
          if (curv_mat(i,i) < 1.0e-30) then
             Err_CFML%Ierr = 1
             write(unit=Err_CFML%Msg,fmt="(a,i5,a,a)")  &
                  " => Singular matrix!!, problem with parameter no.:",i," -> ",trim(namfree(i))
             c%failed=.true.
             ifail=2
             return
          end if
       end do

       do j=1,c%npvar
          do k=1,j
             curv_mat(k,j)=curv_mat(j,k)
          end do
       end do

       !---- Evaluate CHI2 at starting point ----!
       chisq1=fchisq(nfr,nobs,y,w,yc)  !chi2 before inverting and applying shifts

       !---- Invert modified curvature matrix to find new parameters
       do ntr=1,ntrials
          do j=1,c%npvar
             do k=1,c%npvar
                correl(j,k)=curv_mat(j,k)/sqrt(curv_mat(j,j)*curv_mat(k,k))
             end do
             correl(j,j)=1.0+fl
          end do

          correl(1:c%npvar,1:c%npvar)=Inverse_Matrix(correl(1:c%npvar,1:c%npvar))

          if (Err_CFML%Ierr /= 0) then
             do i=1,c%npvar
                if (correl(i,i) < 1.0e-20) then
                   j=i !iperm(i)
                   exit
                end if
             end do
             write(unit=Err_CFML%Msg,fmt="(a,i5,a,a)")  &
                  " => Singular matrix!!, problem with parameter no.:",j," -> ",trim(namfree(j))
             c%failed=.true.
             ifail=2
             return
          end if

          if (fl < 1.0e-20)  then
             chir=chisq1
             exit
          end if

          do j=1,c%npvar
             b(j)=a(j)
             do k=1,c%npvar
                b(j)=b(j)+beta(k)*correl(j,k)/sqrt(curv_mat(j,j)*curv_mat(k,k))
             end do
          end do


          do i=1,nobs
             call model_functn(i,x(i),yc(i),b)
          end do

          chir=fchisq(nfr,nobs,y,w,yc)

          if (ntr == ntrials .or. fl > 1.0e+20 ) then    !no improvement
             ifail=1
             change_par=.false.
             exit
          end if

          !---- If Chi2 increase, increase fl and try again ----!
          if (chisq1-chir < 0.0) then  !Chi2 increases
             fl=10.0*fl                !Increase fl
             cycle
          end if
          change_par=.true.
          exit
       end do  ! ntrials
       nt=ntr
       !---- Evaluate parameters and uncertainties ----!
       do j=1,c%npvar
          sa(j)=sqrt(abs(correl(j,j)/curv_mat(j,j)))
       end do
       if (change_par) then
          do j=1,c%npvar
             a(j)=b(j)
          end do
       end if
       fl=fl/10.0

    End Subroutine Curfit_v1

    !!--++
    !!--++  Subroutine Curfit_v2(Model_Functn, d, c, vs, Fl, Chir, Ifail,nt)
    !!--++     Type(LSQ_Data_type)                  intent(in out)  :: d     !Data
    !!--++     Type(LSQ_Conditions_type),           intent(in)      :: c     !conditions for refinement
    !!--++     Type(LSQ_State_Vector_type),         intent(in out)  :: vs    !State Vector with model parameters
    !!--++     real(kind=cp),                       intent(in out)  :: fl    !Marquardt LAMBDA value
    !!--++     real(kind=cp),dimension(:),          intent(out)     :: yc    !Calculated
    !!--++     real(kind=cp),                       intent(out)     :: chir
    !!--++     integer,                             intent(out)     :: ifail
    !!--++     integer,                             intent(out)     :: nt
    !!--++
    !!--++     Interface
    !!--++      Subroutine Model_Functn(iv,xv,ycalc,Vsa,der)
    !!--++         use CFML_GlobalDeps, only: cp
    !!--++         import :: LSQ_State_Vector_type
    !!--++         integer,                             intent(in) :: iv
    !!--++         real(kind=cp),                       intent(in) :: xv
    !!--++         real(kind=cp),                       intent(out):: ycalc
    !!--++         Type(LSQ_State_Vector_type),         intent(in) :: Vsa
    !!--++         real(kind=cp),dimension(:),optional, intent(out):: der
    !!--++      End Subroutine Model_Functn
    !!--++     End Interface
    !!--++
    !!--++ Update: February - 2003
    !!
    Subroutine Curfit_v2(Model_Functn, d, c, vs, Fl, Chir, Ifail,nt)
       !---- Arguments ----!
       Type(LSQ_Data_Type),                 intent(in out)  :: d
       Type(LSQ_Conditions_type),           intent(in out)  :: c     !conditions for refinement
       Type(LSQ_State_Vector_type),         intent(in out)  :: vs
       real(kind=cp),                       intent(in out)  :: fl    !Marquardt LAMBDA value
       real(kind=cp),                       intent(out)     :: chir
       integer,                             intent(out)     :: ifail,nt

       Interface
        Subroutine Model_Functn(iv,xv,ycalc,Vsa,calder)
           use CFML_GlobalDeps, only: cp
           import :: LSQ_State_Vector_type
           integer,                    intent(in)     :: iv
           real(kind=cp),              intent(in)     :: xv
           real(kind=cp),              intent(out)    :: ycalc
           Type(LSQ_State_Vector_type),intent(in out) :: Vsa
           Logical, optional,          intent(in)     :: calder
        End Subroutine Model_Functn
       End Interface

       !---- Local variables ----!
       logical                          :: change_par
       integer                          :: ntrials,ncount
       integer                          :: nfr,j,i,k,ntr
       real(kind=cp)                    :: chisq1
       real(kind=cp), dimension(c%npvar):: beta, der
       real(kind=cp), dimension(c%npvar):: b     !vector of parameter
       real(kind=cp), dimension(c%npvar):: sb    !estimated standard deviations
       Type(LSQ_State_Vector_type)      :: lvs   !local state vector

       call clear_error()
       ntrials=40
       nt=0
       lvs=vs         !Save current vector state for calling the function

       !---- Optimization with MARQUARDT ALGORITHM ----!
       nfr=d%nobs-c%npvar
       ifail=0
       change_par=.false.
       if (d%nobs > 2000) ntrials= 20

       !---- Evaluate ALFA and BETA matrices       ----!
       !---- (BETA=DP*ALFA)--- DP=BETA*ALFA(-1)    ----!
       beta(:) =0.0
       curv_mat(:,:)=0.0


       do i=1,d%nobs
          call model_functn(i,d%x(i),d%yc(i),lvs,.true.)
          ncount=0
          do j=1,lvs%np
             if (lvs%code(j) == 0) cycle
             ncount=ncount+1
             der(ncount)=lvs%dpv(j)  !Extracting derivatives of free parameters
          end do
          if (c%iw == 1 .and. d%yc(i) /= 0.0) d%sw(i)=1.0/d%yc(i)
          do j=1,c%npvar
             beta(j)=beta(j)+d%sw(i)*(d%y(i)-d%yc(i))*der(j)
             do k=1,j
                curv_mat(j,k)=curv_mat(j,k)+d%sw(i)*der(j)*der(k)
             end do
          end do
       end do

       do i=1,c%npvar
          if (curv_mat(i,i) < 1.0e-30) then
             Err_CFML%Ierr = 1
             write(unit=Err_CFML%Msg,fmt="(a,i5,a,a)")  &
                  " => Singular matrix!!, problem with parameter no.:",i," -> ",trim(namfree(i))
             ifail=2
             c%failed=.true.
             return
          end if
       end do

       do j=1,c%npvar
          do k=1,j
             curv_mat(k,j)=curv_mat(j,k)
          end do
       end do

       !---- Evaluate CHI2 at starting point ----!
       chisq1=fchisq(nfr,d%nobs,d%y,d%sw,d%yc)  !chi2 before inverting and applying shifts

       !---- Invert modified curvature matrix to find new parameters
       do ntr=1,ntrials   !Apply the Marquardt procedure without re-calculating derivatives in the call to the function
          nt=nt+1
          ncount=0
          do j=1,lvs%np
             if (lvs%code(j) == 0) cycle
             ncount=ncount+1
             b(ncount)=lvs%pv(j)     !Select and store in b the varying parameters
          end do
          do j=1,c%npvar
             do k=1,c%npvar
                correl(j,k)=curv_mat(j,k)/sqrt(curv_mat(j,j)*curv_mat(k,k))
             end do
             correl(j,j)=1.0+fl
          end do

          correl(1:c%npvar,1:c%npvar)=Inverse_Matrix(correl(1:c%npvar,1:c%npvar))

          if (Err_CFML%Ierr /= 0) then
             do i=1,c%npvar
                if (correl(i,i) < 1.0e-20) then
                   j=i !iperm(i)
                   exit
                end if
             end do
             write(unit=Err_CFML%Msg,fmt="(a,i5,a,a)")  &
                  " => Singular matrix!!, problem with parameter no.:",j," -> ",trim(namfree(j))
             c%failed=.true.
             ifail=2
             return
          end if

          if (fl < 1.0e-20)  then
             chir=chisq1
             exit
          end if
          !Update parameters
          do j=1,c%npvar
             do k=1,c%npvar
                b(j)=b(j)+beta(k)*correl(j,k)/sqrt(curv_mat(j,j)*curv_mat(k,k))
             end do
          end do
          ncount=0
          do i=1,lvs%np
             if (lvs%code(i) == 0) cycle
             ncount=ncount+1
             lvs%pv(i)=b(ncount)   !Provisional change in local vector state
          end do
          do i=1,d%nobs    !Calculation of the function for all points with the new parameters
             call model_functn(i,d%x(i),d%yc(i),lvs)
          end do

          chir=fchisq(nfr,d%nobs,d%y,d%sw,d%yc)

          if (ntr == ntrials .or. fl > 1.0e+20 ) then    !no improvement
             ifail=1
             change_par=.false.
             exit
          end if

          !---- If Chi2 increase, increase fl and try again ----!
          if (chisq1-chir < 0.0) then  !Chi2 increases
             fl=10.0*fl                !Increase fl
             lvs=vs                    !Restore the value of the local state vector to the input value
             cycle
          end if
          change_par=.true.
          exit
       end do  ! ntrials

       !---- Evaluate parameters and uncertainties ----!
       do j=1,c%npvar
          sb(j)=sqrt(abs(correl(j,j)/curv_mat(j,j)))
       end do
       fl=fl/10.0
       ncount=0
       do i=1,vs%np            !Complete update with standard deviations and evaluate the change
          if (vs%code(i) == 0) then
             vs%spv(i)=0.0
             pn(i)=lvs%pv(i)   !Change in Vs done in the calling routine
             ch(i)=0.0
          else
             ncount=ncount+1
             pn(i)=b(ncount)      !Change in Vs done in the calling routine
             ch(i)=pn(i)-vs%pv(i) !change w.r.t input parameters
             if(change_par) vs%spv(i)=sb(ncount)
          end if
       end do
    End Subroutine Curfit_v2

    !!----
    !!---- Module Subroutine Marquardt_Fit(Model_Functn, X, Y, W, Yc, Nobs, c, vs, Ipr, Chi2, scroll_lines,text_info)
    !!----    real(kind=cp), dimension(:),             intent(in)     :: x             !Vector of x-values
    !!----    real(kind=cp), dimension(:),             intent(in)     :: y             !Vector of observed y-values
    !!----    real(kind=cp), dimension(:),             intent(in out) :: w             !Vector of weights-values (1/variance)
    !!----    real(kind=cp), dimension(:),             intent(   out) :: yc            !Vector of calculated y-values
    !!----    integer                    ,             intent(in)     :: nobs          !Number of effective components of x,y,w,yc
    !!----    type(LSQ_conditions_type),               intent(in out) :: c             !Conditions for the algorithm
    !!----    type(LSQ_State_Vector_type),             intent(in out) :: vs            !State vector for the model calculation
    !!----    integer                    ,             intent(in)     :: Ipr           !Logical unit for writing
    !!----    real(kind=cp),                           intent(out)    :: chi2          !Reduced Chi-2
    !!----    character(len=*),dimension(:), optional, intent(out)    :: scroll_lines  !If present, part of the output is stored
    !!----                                                                             !in this text for treatment in the calling program
    !!----    character(len=*),              optional, intent(in)     :: text_info     !If present, a text is passed to Info_LSQ_Output
    !!----
    !!----    Model_functn                                                             !Name of the subroutine calculating yc(i) for point x(i)
    !!----    Interface                                                                !Interface for the Model_Functn subroutine
    !!----       Subroutine Model_Functn(iv,Xv,ycalc,aa,der)
    !!----          use CFML_GlobalDeps, only: cp
    !!----          integer,                             intent(in) :: iv     !Number of the component: "i" in x(i)
    !!----          real(kind=cp),                       intent(in) :: xv     !Value of x(i)
    !!----          real(kind=cp),                       intent(out):: ycalc  !Value of yc at point x(i) => ycalc=yc(i)
    !!----          real(kind=cp),dimension(:),          intent(in) :: aa     !Vector of parameters
    !!----          real(kind=cp),dimension(:),optional, intent(out):: der    !Derivatives of the function w.r.t. free parameters
    !!----       End Subroutine Model_Functn
    !!----    End Interface
    !!----
    !!----     or
    !!----
    !!---- Subroutine Marquardt_Fit(Model_Functn, d, c, vs, Ipr, Chi2, scroll_lines)
    !!----    Type(LSQ_Data_Type),                     intent(in out) :: d             !Vector of x-values
    !!----    type(LSQ_conditions_type),               intent(in out) :: c             !Conditions for the algorithm
    !!----    type(LSQ_State_Vector_type),             intent(in out) :: vs            !State vector for the model calculation
    !!----    integer                    ,             intent(in)     :: Ipr           !Logical unit for writing
    !!----    real(kind=cp),                           intent(out)    :: chi2          !Reduced Chi-2
    !!----    character(len=*),dimension(:), optional, intent(out)    :: scroll_lines  !If present, part of the output is stored
    !!----                                                                             !in this text for treatment in the calling program
    !!----
    !!----    Model_functn                                                             !Name of the subroutine calculating yc(i) for point x(i)
    !!----    Interface                                                                !Interface for the Model_Functn subroutine
    !!----       Subroutine Model_Functn(iv,Xv,ycalc,Vsa,calder)
    !!----          use CFML_GlobalDeps, only: cp
    !!----          integer,                    intent(in)     :: iv     !Number of the component: "i" in x(i)
    !!----          real(kind=cp),              intent(in)     :: xv     !Value of x(i)
    !!----          real(kind=cp),              intent(out)    :: ycalc  !Value of yc at point x(i) => ycalc=yc(i)
    !!----          Type(LSQ_State_Vector_type),intent(in out) :: Vsa    !Parameters, codes, and derivatives
    !!----          logical,optional,           intent(in)     :: calder !If present, derivatives, stored in Vsa%dpv, are calculated
    !!----       End Subroutine Model_Functn
    !!----    End Interface
    !!----
    !!----
    !!---- Update: January 2010
    !!

    !!--++
    !!--++ Module Subroutine Marquardt_Fit_v1(Model_Functn, X, Y, W, Yc, Nobs, c, vs, Ipr, Chi2, scroll_lines)
    !!--++    real(kind=cp), dimension(:),intent(in)     :: x      !Vector of x-values
    !!--++    real(kind=cp), dimension(:),intent(in)     :: y      !Vector of observed y-values
    !!--++    real(kind=cp), dimension(:),intent(in out) :: w      !Vector of weights-values (1/variance)
    !!--++    real(kind=cp), dimension(:),intent(   out) :: yc     !Vector of calculated y-values
    !!--++    integer                    ,intent(in)     :: nobs   !Number of effective components of x,y,w,yc
    !!--++    type(LSQ_conditions_type),  intent(in out) :: c      !Conditions for the algorithm
    !!--++    type(LSQ_State_Vector_type),intent(in out) :: vs     !State vector for the model calculation
    !!--++    integer                    ,intent(in)     :: Ipr    !Logical unit for writing
    !!--++    real(kind=cp),              intent(out)    :: chi2   !Reduced Chi-2
    !!--++    character(len=*),dimension(:), intent(out), optional  :: scroll_lines  !If present, part of the output is stored
    !!--++                                                                           !in this text for treatment in the calling program
    !!--++    character(len=*),dimension(:), optional, intent(out)    :: scroll_lines  !If present, part of the output is stored
    !!--++
    !!--++    Model_functn                                            !Name of the subroutine calculating yc(i) for point x(i)
    !!--++    Interface                                               !Interface for the Model_Functn subroutine
    !!--++       Subroutine Model_Functn(iv,Xv,ycalc,aa,der)
    !!--++            use CFML_GlobalDeps, only: cp
    !!--++            integer,                             intent(in) :: iv     !Number of the component: "i" in x(i)
    !!--++            real(kind=cp),                       intent(in) :: xv     !Value of x(i)
    !!--++            real(kind=cp),                       intent(out):: ycalc  !Value of yc at point x(i) => ycalc=yc(i)
    !!--++            real(kind=cp),dimension(:),          intent(in) :: aa     !Vector of parameters
    !!--++            real(kind=cp),dimension(:),optional, intent(out):: der    !Derivatives of the function w.r.t. free parameters
    !!--++       End Subroutine Model_Functn
    !!--++    End Interface
    !!--++
    !!--++
    !!--++    Subroutine for applying the Levenberg-Marquardt method for Least-Squares.
    !!--++    The user must provide a model function according to the interface above.
    !!--++    The model function should use at least some of the public variables of the
    !!--++    present module in order to set the derivatives with respect to the model
    !!--++    parameters. Examples of using this module are given in the program
    !!--++    templates CW_fit and TOF_fit. This subroutine ignores vs%code_comp!
    !!--++
    !!--..    INFORMATION
    !!--..        Author: Juan Rodriguez-Carvajal (based in text descriptions of the literature)
    !!--..                Translated, extracted and modified from the Fortran 77 code: XRFIT (JRC 1986)
    !!--..        Module implementing the Marquardt method for non-linear least-squares.
    !!--..        For using this module, the user must provide:
    !!--..            1: Number of cycles (c%icyc), type of weighting scheme (c%iw), number of
    !!--..               model parameters (vs%np), constraint conditions (c%constr, c%percent)
    !!--..            2: A character(len=15) name for all possible parameters of the
    !!--..               model stored in the array vs%nampar(:).
    !!--..            3: A set of initial value for all parameters stored in vs%pv(:)
    !!--..            4: A set of flags values to refine or fix the parameters vs%code(:)
    !!--..            5: The model function (subroutine model_functn) must be provided by the user
    !!--..               according to the interface described below. The actual name of the model function
    !!--..               is arbitrary and it is passed to the only public procedure "marquardt_fit" as a
    !!--..               dummy argument.
    !!--..
    !!--..       The values of all possible refinable parameters are stored in the array vs%pv(:).
    !!--..       The derivatives must be calculated within model_functn, by using the array vs%der(:)
    !!--..       The actual refined parameters a(:) are selected from the larger vs%pv(:) array by
    !!--..       the integer array of flags: vs%code(:). A value vs%code(j) /= 0 means that the j-th
    !!--..       parameter is to be varied. A value vs%code(k) = 0 means that the k-th parameter is
    !!--..       kept fixed through the refinement cycles.
    !!----
    !!---- Update: March - 2005
    !!
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

       !---- local variables ----!
       logical                                :: fixed, write_cyc
       character(len=180)                     :: line
       integer                                :: ifail,nt
       integer                                :: i,j,ncount,li,ntex
       real(kind=cp)                          :: fl,chi1
       real(kind=cp), dimension(Max_Free_Par) :: a,sa

       fixed = .false.
       write_cyc=.true.
       if (c%icyc > 50) write_cyc=.false.

       !---- Beginning the iterations ----!
       fl=0.001
       chi1=1.0e+30
       c%reached =.false.
       write(unit=ipr,fmt="(/,a)")   " -------------------------------------------------"
       write(unit=ipr,fmt="(a,i3,a)")" => Marquardt Least Squares Fitting for ",c%icyc," cycles"
       write(unit=ipr,fmt="(a,/)")   " -------------------------------------------------"

       write(unit=ipr,fmt="(a,i6)")        " => Number of observations   : ",nobs
       ncount=0
       do i=1,vs%np
          if (vs%code(i) == 0) cycle
          ncount=ncount+1
          j=vs%code(i)
          namfree(j)=vs%nampar(i)
       end do
       c%npvar=ncount
       write(unit=ipr,fmt="(a,i6)")        " => Number of free parameters: ",ncount
       write(unit=ipr,fmt="(2(a,f12.4),a)")" => Range of variable X      : (",x(1),",",x(nobs),")"

       ntex=0
       if(present(scroll_lines)) scroll_lines=" "  !Clear the stored text

       do li=1,c%icyc                      !Loop over icyc cycles
          ncount=0
          do i=1,vs%np
             if (vs%code(i) == 0) cycle
             ncount=ncount+1
             a(ncount)=vs%pv(i)
          end do

          call curfit_v1(Model_Functn,x,y,w,nobs,c,a,sa,fl,yc,chi2,ifail,nt)

          if (ifail /= 0) then
             line= " "
             write(unit=line,fmt="(a,i3,a)") " => IFAIL /= 0 : ",ifail, " "//trim(Err_CFML%Msg)
             if(present(scroll_lines)) then
                ntex=ntex+1
                scroll_lines(ntex)=line
             else
                 if(ifail /= 1) write(unit=*,fmt="(a)") trim(line)
             end if
             if (ABS(chi2-chi1)*100/Chi2 <= 0.001 .and. ifail==1) then
                c%reached=.true.
                line= " "
                write(unit=line,fmt="(a)") " => Convergence reached! : Chi2(old)-Chi2(new) < 0.00001 * Chi2(old)"
                if(present(scroll_lines)) then
                    ntex=ntex+1
                    scroll_lines(ntex)=line
                else
                    write(unit=*,fmt="(a)") trim(line)
                end if
                write(unit=Ipr,fmt="(/,a,/)") " => Convergence reached! : Chi2(old)-Chi2(new) < 0.00001 * Chi2(old)"
                exit
             end if

             if(present(scroll_lines)) then
                 ntex=ntex+1
                 scroll_lines(ntex)=" => Marquardt convergence not reached (F-lambda increased too much, see output file)."
             else
                 write(unit=*,fmt="(a)") " => Marquardt convergence not reached (F-lambda increased too much, see output file)."
             end if

             write(unit=ipr,fmt="(/,a)")" => Marquardt convergence not reached (F-lambda increased too much)."
             write(unit=ipr,fmt="(a)" ) "    If Chi2 is reasonably low, you may have reached the minimum"
             write(unit=ipr,fmt="(a)" ) "    within the precision of the machine.                       "
             write(unit=ipr,fmt="(a)" ) "    Otherwise, look at the output file to search for the reason "
             write(unit=ipr,fmt="(a,/)")"    of anomalous behaviour and try to start from new parameters"

             write(unit=ipr,fmt="(/,a,/)")" =>  Diagonal elements of LSQ-matrix before inversion"
             do i=1,c%npvar
                write(unit=ipr,fmt="(a30,i3,a,i3,a,f16.6)") "    "//trim(namfree(i))//"  (",i,",",i,")", curv_mat(i,i)
             end do

             exit
          end if  !ifail/=0

          if (abs(chi2-chi1)*100.0/chi2 >= 0.0001 ) then
             c%reached=.false.
             chi1=chi2
             ncount=0
             do i=1,vs%np
                if (vs%code(i) == 0) then
                   vs%spv(i)=0.0
                   pn(i)=vs%pv(i)
                   ch(i)=0.0
                else
                   ncount=ncount+1
                   pn(i)=a(ncount)
                   ch(i)=pn(i)-vs%pv(i)
                   vs%spv(i)=sa(ncount)
                end if
             end do

             if (c%constr)  then
                call box_constraints(a,sa,fixed,c,vs)
                if (fixed) then
                   write(unit=ipr,fmt="(/,a,/,a,i2,a)") " => Some parameters have been restored to their initial value ", &
                                                        "    because of local divergence (change > ",nint(c%percent),"%)"
                   write(unit=ipr,fmt="(a,/)")          "    Look at the above list for parameters having a '*'"
                end if  !fixed
             end if

             line= " "
             write(unit=line,fmt="(a,i5,2(a,f14.6),a,i3)")" => Cycle No.:",li,"  Chi2 =",chi2, "  Marquardt-Lambda: ",fl, &
                                                          "  Ntrials: ",nt
             if(present(scroll_lines)) then
                 ntex=ntex+1
                 scroll_lines(ntex)=line
             else
                 write(unit=*,fmt="(a)") trim(line)
             end if

             if (write_cyc) call output_cyc(li,Ipr,chi2,vs)
             vs%pv(1:vs%np)=pn(1:vs%np)
             cycle
          else
             line= " "
             write(unit=line,fmt="(a)") " => Convergence reached! : Chi2(old)-Chi2(new) < 0.000001 * Chi2(old)"
             if(present(scroll_lines)) then
                 ntex=ntex+1
                 scroll_lines(ntex)=line
             else
                 write(unit=*,fmt="(a)") trim(line)
             end if
             write(unit=Ipr,fmt="(/,a,/)") " => Convergence reached! : Chi2(old)-Chi2(new) < 0.000001 * Chi2(old)"
             c%reached=.true.
             exit
          end if
       end do  !li=1,c%icyc

       if (c%reached) then
          if (fl <= 10.0) then
             fl=0.0
          else
             fl=0.5e-20
          end if
          call curfit_v1(Model_Functn,x,y,w,nobs,c,a,sa,fl,yc,chi2,ifail,nt)
          ncount=0
          do i=1,vs%np
             if (vs%code(i) == 0) then
                vs%spv(i)=0.0
                pn(i)=vs%pv(i)
                ch(i)=0.0
             else
                ncount=ncount+1
                namfree(ncount)=vs%nampar(i)
                pn(i)=a(ncount)
                ch(i)=pn(i)-vs%pv(i)
                vs%spv(i)=sa(ncount)
                vs%pv(i)=a(ncount)
             end if
          end do
       end if

       if(present(text_info)) then
          call Info_LSQ_Output(chi2,FL,nobs,x,y,yc,w,Ipr,c,vs,"CURFIT",text_info)
       else
          call Info_LSQ_Output(chi2,FL,nobs,x,y,yc,w,Ipr,c,vs,"CURFIT")
       end if

    End Subroutine Marquardt_Fit_v1

    !!--++
    !!--++ Module Subroutine Marquardt_Fit_v2(Model_Functn, d, c, vs, Ipr, Chi2, scroll_lines)
    !!--++    Type(LSQ_Data_Type),                     intent(in out) :: d             !Vector of x-values
    !!--++    type(LSQ_conditions_type),               intent(in out) :: c             !Conditions for the algorithm
    !!--++    type(LSQ_State_Vector_type),             intent(in out) :: vs            !State vector for the model calculation
    !!--++    integer                    ,             intent(in)     :: Ipr           !Logical unit for writing
    !!--++    real(kind=cp),                           intent(out)    :: chi2          !Reduced Chi-2
    !!--++    character(len=*),dimension(:), optional, intent(out)    :: scroll_lines  !If present, part of the output is stored
    !!--++                                                                             !in this text for treatment in the calling program
    !!--++
    !!--++    Model_functn                                                             !Name of the subroutine calculating yc(i) for point x(i)
    !!--++    Interface                                                                !Interface for the Model_Functn subroutine
    !!--++       Subroutine Model_Functn(iv,Xv,ycalc,Vsa,calder)
    !!--++          use CFML_GlobalDeps, only: cp
    !!--++          integer,                    intent(in)     :: iv     !Number of the component: "i" in x(i)
    !!--++          real(kind=cp),              intent(in)     :: xv     !Value of x(i)
    !!--++          real(kind=cp),              intent(out)    :: ycalc  !Value of yc at point x(i) => ycalc=yc(i)
    !!--++          Type(LSQ_State_Vector_type),intent(in out) :: Vsa    !Parameters, codes, and derivatives
    !!--++          logical,optional,           intent(in)     :: calder !If present, derivatives, stored in Vsa%dpv, are calculated
    !!--++       End Subroutine Model_Functn
    !!--++    End Interface
    !!--++
    !!--++
    !!--++    Subroutine for applying the Levenberg-Marquardt method for Least-Squares.
    !!--++    The user must provide a model function according to the interface above.
    !!--++    The model function should use at least some of the public variables of the
    !!--++    present module in order to set the derivatives with respect to the model
    !!--++    parameters. Examples of using this module are given in the program
    !!--++    templates CW_fit and TOF_fit.
    !!--++
    !!--..    INFORMATION
    !!--..        Author: Juan Rodriguez-Carvajal (based in text descriptions of the literature)
    !!--..                Translated, extracted and modified from the Fortran 77 code: XRFIT (JRC 1986)
    !!--..        Module implementing the Marquardt method for non-linear least-squares.
    !!--..        For using this module, the user must provide:
    !!--..            1: Number of cycles (c%icyc), type of weighting scheme (c%iw), number of
    !!--..               model parameters (vs%np), constraint conditions (c%constr, c%percent)
    !!--..            2: A character(len=15) name for all possible parameters of the
    !!--..               model stored in the array vs%nampar(:).
    !!--..            3: A set of initial value for all parameters stored in vs%pv(:)
    !!--..            4: A set of flags values to refine or fix the parameters vs%code(:)
    !!--..            5: The model function (subroutine model_functn) must be provided by the user
    !!--..               according to the interface described below. The actual name of the model function
    !!--..               is arbitrary and it is passed to the only public procedure "marquardt_fit" as a
    !!--..               dummy argument.
    !!--..
    !!--..       The values of all possible refinable parameters are stored in the array vs%pv(:).
    !!--..       The derivatives must be calculated within model_functn, by using the array vs%dpv(:)
    !!--..       The actual refined parameters are selected from the larger vs%pv(:) array by
    !!--..       the integer array of flags: vs%code(:). A value vs%code(j)=1 means that the j-th
    !!--..       parameter is to be varied. A value vs%code(k)=0 means that the k-th parameter is
    !!--..
    !!--..       Subroutine Model_Functn(iv,Xv,ycalc,Vsa,calder)
    !!--..       It is recommended that Model_Functn be stored in a Module. The integer iv (counter
    !!--..       of the loop for all observations for which the subroutine is invoked) is passed
    !!--..       because in many cases part of the calculations and derivatives may be calculated only
    !!--..       for iv=1 and stored in local variables that should have the SAVE attribute or be private
    !!--..       module variables accessible by host association. The only output values of the subroutine
    !!--..       are ycalc and Vsa%dpv(:) that vary for each "iv" point.
    !!--..       In this version of the algorithm the derivatives with respect to the free parameters
    !!--..       in Model_Functn should be calculated before exiting by using the following loop:
    !!--..
    !!--..       vs%dpv=0.0          !Should be always be initialized for each point
    !!--..       if(present(calder)) then
    !!--..         Do i=1,vs%np
    !!--..           if(vs%code(i) == 0) cycle
    !!--..           vs%dpv(i) = derivative_wrt(i) !symbolic form for calculating the derivative w.r.t. parameter "i"
    !!--..         end do                          !at the current point xv (observation "iv")
    !!--..       end if
    !!--++
    !!--++ Update: August - 2009
    !!
    Module Subroutine Marquardt_Fit_v2(Model_Functn,d,c,vs,Ipr,Chi2,scroll_lines,text_info)
       !---- Arguments ----!
       Type(LSQ_Data_Type),           intent(in out)         :: d
       type(LSQ_conditions_type),     intent(in out)         :: c
       type(LSQ_State_Vector_type),   intent(in out)         :: vs
       integer,                       intent(in)             :: Ipr
       real(kind=cp),                 intent(   out)         :: chi2
       character(len=*),dimension(:), intent(out), optional  :: scroll_lines
       character(len=*),              intent(in),  optional  :: text_info     !If present, a text is passed to Info_LSQ_Output

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

       !---- local variables ----!
       logical                                :: fixed, write_cyc
       character(len=180)                     :: line
       integer                                :: ifail
       integer                                :: i,ncount,li,ntex,nt
       real(kind=cp)                          :: fl,chi1
       real(kind=cp), dimension(Max_Free_Par) :: a,sa

       fixed = .false.
       write_cyc=.true.
       if (c%icyc > 50) write_cyc=.false.

       !---- Beginning the iterations ----!
       fl=0.001
       chi1=1.0e+30
       c%reached =.false.
       write(unit=ipr,fmt="(/,a)")   " -------------------------------------------------"
       write(unit=ipr,fmt="(a,i3,a)")" => Marquardt Least Squares Fitting for ",c%icyc," cycles"
       write(unit=ipr,fmt="(a,/)")   " -------------------------------------------------"

       write(unit=ipr,fmt="(a,i6)")        " => Number of observations   : ",d%nobs
       ncount=0
       do i=1,vs%np
          if (vs%code(i) == 0) cycle
          ncount=ncount+1
          namfree(ncount)=vs%nampar(i)
       end do
       c%npvar=ncount
       write(unit=ipr,fmt="(a,i6)")        " => Number of free parameters: ",ncount
       write(unit=ipr,fmt="(2(a,f12.4),a)")" => Range of variable X      : (",d%x(1),",",d%x(d%nobs),")"

       ntex=0
       if(present(scroll_lines)) scroll_lines=" "  !Clear the stored text
       do li=1,c%icyc                      !Loop over icyc cycles

          call curfit_v2(Model_Functn,d,c,vs,fl,chi2,ifail,nt)

          if (ifail /= 0) then
             line= " "
             write(unit=line,fmt="(a,i3,a)") " => IFAIL /= 0 : ",ifail, " "//trim(Err_CFML%Msg)
             if(present(scroll_lines)) then
                ntex=ntex+1
                scroll_lines(ntex)=line
              else
                 if(ifail /= 1) write(unit=*,fmt="(a)") trim(line)
             end if
             if (ABS(chi2-chi1)*100/Chi2 <= 0.001 .and. ifail==1) then
                c%reached=.true.
                line= " "
                write(unit=line,fmt="(a)") " => Convergence reached! : Chi2(old)-Chi2(new) < 0.00001 * Chi2(old)"
                if(present(scroll_lines)) then
                    ntex=ntex+1
                    scroll_lines(ntex)=line
                else
                    write(unit=*,fmt="(a)") trim(line)
                end if
                write(unit=Ipr,fmt="(/,a,/)") " => Convergence reached! : Chi2(old)-Chi2(new) < 0.00001 * Chi2(old)"
                exit
             end if

             do i=1,d%nobs            !Calculation of the function for all points with the previous parameters
                call model_functn(i,d%x(i),d%yc(i),vs)
             end do
             chi2=fchisq(d%nobs-c%npvar,d%nobs,d%y,d%sw,d%yc)
             line= " "
             write(unit=line,fmt="(a,i5,2(a,f14.6),a,i3)") " => Marquardt convergence not reached! Cycle No.:",&
                                            li,"  Last Chi2 =",chi2, "  Marquardt-Lambda: ",fl, "  Ntrials: ",nt
             if(present(scroll_lines)) then
                ntex=ntex+1
                scroll_lines(ntex)=line
              else
                write(unit=*,fmt="(a)") trim(line)
             end if

             write(unit=ipr,fmt="(/,a)") trim(line)
             write(unit=ipr,fmt="(a)" ) "    If Chi2 is reasonably low, you may have reached the minimum"
             write(unit=ipr,fmt="(a)" ) "    within the precision of the machine.                       "
             write(unit=ipr,fmt="(a)" ) "    Otherwise, look at the output file to search for the reason "
             write(unit=ipr,fmt="(a,/)")"    of anomalous behaviour and try to start from new parameters"

             write(unit=ipr,fmt="(/,a,/)")" =>  Diagonal elements of LSQ-matrix before inversion"
             do i=1,c%npvar
                write(unit=ipr,fmt="(a30,i3,a,i3,a,f16.6)") "    "//trim(namfree(i))//"  (",i,",",i,")", curv_mat(i,i)
             end do

             exit
          end if  !ifail/=0

          if (abs(chi2-chi1)*100.0/chi2 >= 0.0001 ) then
             c%reached=.false.
             chi1=chi2
              if (c%constr)  then
                !Extract the vectors a and sa from pn
                ncount=0
                do i=1,vs%np
                   if (vs%code(i) == 0) cycle
                   ncount=ncount+1
                   a(ncount)=pn(i)
                   sa(ncount)=vs%spv(i)
                end do
                call box_constraints(a,sa,fixed,c,vs)
                if (fixed) then
                   write(unit=ipr,fmt="(/,a,/,a,i2,a)") " => Some parameters have been restored to their initial value ", &
                                                        "    because of local divergence (change > ",nint(c%percent),"%)"
                   write(unit=ipr,fmt="(a,/)")          "    Look at the above list for parameters having a '*'"
                end if  !fixed
             end if

             line= " "
             write(unit=line,fmt="(a,i5,2(a,f14.6),a,i3)") &
                " => Cycle No.:",li,"  Chi2 =",chi2, "  Marquardt-Lambda: ",fl, "  Ntrials: ",nt
             if(present(scroll_lines)) then
                 ntex=ntex+1
                 scroll_lines(ntex)=line
             else
                 write(unit=*,fmt="(a)") trim(line)
             end if

             if (write_cyc) call output_cyc(li,Ipr,chi2,vs)
             vs%pv(1:vs%np)=pn(1:vs%np)  !Change is done here
             cycle
          else
             line= " "
             write(unit=line,fmt="(a)") " => Convergence reached! : Chi2(old)-Chi2(new) < 0.000001 * Chi2(old)"
             if(present(scroll_lines)) then
                 ntex=ntex+1
                 scroll_lines(ntex)=line
             else
                 write(unit=*,fmt="(a)") trim(line)
             end if
             write(unit=Ipr,fmt="(/,a,/)") " => Convergence reached! : Chi2(old)-Chi2(new) < 0.000001 * Chi2(old)"
             c%reached=.true.
             exit
          end if
       end do  !li=1,c%icyc

       if (c%reached) then
          if (fl <= 10.0) then
             fl=0.0
          else
             fl=0.5e-20
          end if
          call curfit_v2(Model_Functn,d,c,vs,fl,chi2,ifail,nt)
       end if

       if(present(text_info)) then
          call Info_LSQ_Output(chi2,FL,d%nobs,d%x,d%y,d%yc,d%sw,Ipr,c,vs,"CURFIT",text_info)
       else
          call Info_LSQ_Output(chi2,FL,d%nobs,d%x,d%y,d%yc,d%sw,Ipr,c,vs,"CURFIT")
       end if

    End Subroutine Marquardt_Fit_v2

 End Submodule OPT_LSQ_Marquard_Fit
