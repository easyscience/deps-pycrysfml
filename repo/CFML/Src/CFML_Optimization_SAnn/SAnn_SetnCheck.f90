 Submodule (CFML_Simulated_Annealing) SAnn_SetnCheck
  implicit none
   contains

    !!--++
    !!--++ Module Subroutine Boundary_Cond(n,x,low,high,bound)
    !!--++    integer,                     intent(in)     :: n
    !!--++    real(kind=cp), dimension(:), intent(in out) :: x
    !!--++    real(kind=cp), dimension(:), intent(in)     :: low,high
    !!--++    integer, dimension(:),       intent(in)     :: bound
    !!--++
    !!--++    (PRIVATE)
    !!--++    Apply boundary conditions to vector x according to
    !!--++    information supplied inn,low,high & bound
    !!--++
    !!--++ Update: September - 2007
    !!
    Module Subroutine Boundary_Cond(n,x,low,high,bound)
       !---- Arguments ----!
       integer,                     intent(in)     :: n
       real(kind=cp), dimension(:), intent(in out) :: x
       real(kind=cp), dimension(:), intent(in)     :: low,high
       integer, dimension(:),       intent(in)     :: bound

       !---- Local variables -----!
       integer :: i
       real    :: plage

       do i=1,n
          plage=high(i)-low(i)
          if (bound(i) == 0) then   !boundary conditions
             if (x(i) < low(i) ) x(i) = low(i)
             if (x(i) > high(i)) x(i) = high(i)
          else
             if (x(i) < low(i) ) x(i) = x(i)+plage
             if (x(i) > high(i)) x(i) = x(i)-plage
          end if
       end do

    End Subroutine Boundary_Cond

    !!--++
    !!--++ Module Subroutine Check(c,vs)
    !!--++    type(SimAnn_Conditions_type), intent(in out) :: c
    !!--++    type(State_Vector_Type),      intent(in out) :: vs
    !!--++
    !!--++    (PRIVATE)
    !!--++    Chek if all important variables of the SA algorithm has
    !!--++    been supplied. Fix some values
    !!--++
    !!--++ Update: March - 2005
    !!
    Module Subroutine Check(c,vs)
       !---- Arguments ----!
       type(SimAnn_Conditions_type), intent(in out) :: c
       type(State_Vector_Type),      intent(in out) :: vs

       !---- Local variables ----!
       integer :: i
       character(len=3) :: num

       call clear_error()
       if (vs%npar == 0) then
          Err_CFML%Ierr=1
          Err_CFML%Msg=" Zero parameters (vs%npar = 0) for the vector state: Check VARY instructions!"
          return
       end if
       if (c%nm_cycl <= 1) then
          Err_CFML%Ierr=1
          Err_CFML%Msg=" Too small value for the number of MCcycles/Temp : PROVIDE a number > 10*Npar for variable: NM_CYCL"
          return
       end if

       !---- Default values ----!
       if (c%num_temps < 1)   c%num_temps=1
       if (c%num_therm < 0)   c%num_therm=0
       if (c%initconfig < 0)  c%initconfig=0
       if (c%nalgor < 0)      c%nalgor=0
       if (c%anneal <= 0.001) c%anneal=0.9
       if (c%t_ini <= 0.0 )   c%t_ini=5.0
       if (c%accept <= 0.0 )  c%accept=0.01
       if (len_trim(c%Cost_function_name) == 0) then
          c%Cost_function_name="Unnamed Cost_function"
       end if

       do i=1,vs%npar
          if (len_trim(vs%nampar(i)) == 0) then
             write(unit=num,fmt="(i3)") i
             num=adjustl(num)
             vs%nampar(i)="Par_"//num
          end if
          if (abs(vs%low(i)-vs%high(i)) < 1.0e-12) vs%code(i)=0
       end do

    End Subroutine Check

    !!--++
    !!--++ Module Subroutine Checkm(c,vs)
    !!--++    type(SimAnn_Conditions_type), intent(in out) :: c
    !!--++    type(State_Vector_Type),      intent(in out) :: vs
    !!--++
    !!--++    (PRIVATE)
    !!--++    Check if all important variables of the SA algorithm have
    !!--++    been supplied. Fix some values
    !!--++
    !!--++ Update: March - 2005
    !!
    Module Subroutine Checkm(c,vs)
       !---- Arguments ----!
       type(SimAnn_Conditions_type), intent(in out) :: c
       type(MultiState_Vector_Type), intent(in out) :: vs

       !---- Local variables ----!
       integer          :: i
       character(len=3) :: num

       call clear_error()
       if (vs%npar == 0) then
          Err_CFML%Ierr=1
          Err_CFML%Msg=" Zero parameters (vs%npar = 0) for the vector state: Check VARY instructions "
          return
       end if
       if (c%nm_cycl <= 1) then
          Err_CFML%Ierr=1
          Err_CFML%Msg=" Too small value for the number of MCcycles/Temp : PROVIDE a number > 10*Npar for variable: NM_CYCL"
          return
       end if

       !---- Default values ----!
       if (c%num_temps < 1)   c%num_temps=1
       if (c%num_therm < 0)   c%num_therm=0
       if (c%initconfig < 0)  c%initconfig=0
       if (c%nalgor < 0)      c%nalgor=0
       if (c%anneal <= 0.001) c%anneal=0.9
       if (c%t_ini <= 0.0 )   c%t_ini=5.0
       if (c%accept <= 0.0 )  c%accept=0.01
       if (len_trim(c%Cost_function_name) == 0) then
          c%Cost_function_name="Unnamed Cost_function"
       end if

       do i=1,vs%npar
          if (len_trim(vs%nampar(i)) == 0) then
             write(unit=num,fmt="(i3)") i
             num=adjustl(num)
             vs%nampar(i)="Par_"//num
          end if
          if (abs(vs%low(i)-vs%high(i)) < 1.0e-12) vs%code(i)=0
       end do

    End Subroutine Checkm

    !!--++
    !!--++ Module Subroutine Init_Ran(seed)
    !!--++    integer, dimension(:), intent (in) :: seed
    !!--++
    !!--++    (PRIVATE)
    !!--..    Calling sequence: Call RANDOM_SEED(size=k,put=seed(1:k),get=old(1:k))
    !!--..    where:
    !!--..          size: Integer, intent(out) - default integer size used by the processor to hold the seed
    !!--..          put : Integer, dimension(:), intent(in)- used by the processor to set the seed value
    !!--..          get : Integer, dimension(:), intent(out)- set by the processor to current seed value
    !!--..          size(put) and size(get) >= size
    !!--++
    !!--++ Update: February - 2003
    !!
    Module Subroutine Init_Ran(Seed)
       !---- Argument ----!
       integer, dimension(:), intent (in) :: seed

       if (seed(1) == 0) then
          call random_seed() !seed selected by the system clock
       else
          call random_seed(put=seed) !seed selected by the user
       end if

    End Subroutine Init_Ran

    !!----
    !!---- Module Subroutine Set_SimAnn_Cond(file_list,c)
    !!----    type (file_type),            intent( in)  :: file_list
    !!----    type(SimAnn_Conditions_type),intent(out)  :: c
    !!----
    !!----    Subroutine for reading and set up the SimAnn_Conditions_type
    !!----    variable "c"
    !!----
    !!---- Update: April - 2005
    !!
    Module Subroutine Set_SimAnn_Cond_FT(file_list,c)
       !---- Arguments ----!
       type(file_type),             intent( in)  :: file_list
       type(SimAnn_Conditions_type),intent(out)  :: c

       !--- Local Variables ---!
       integer           :: i,j,k,ier
       logical           :: notset,tempar,algor,noinst
       character(len=80) :: line

       notset=.true.
       tempar=.false.
       algor =.false.

       c%t_ini=5.0      ! Initial temperature
       c%anneal=0.9     ! Kirpactrick factor for Annealing
       c%accept=0.01    ! Minimum percentage of accepted configurations
       c%threshold=0.0  ! Maximun value of an acceptable cost function
       c%initconfig=0   ! Flag determining if the first configuration is random or read
       c%nalgor=0    ! Flag determining if the Corana algorithm is selected (0) or not (/=0)
       c%nm_cycl=0   ! Number of Cycles per temp  in SA searchs
       c%num_temps=1 ! Maximum number of temperatures in SA
       c%num_therm=0 ! Number of thermalization cycles in SA
       c%num_conf=1  ! Number of paralell configurations in SA
       c%Cost_function_name=" Unnamed Cost Function"
       c%seed=0      ! If different from zero, holds the seed for random number generator

       do_read: do i=1,file_list%nlines
         if(file_list%line(i)%Str(1:7) == "SIM_ANN") then

           do j=i+1,file_list%nlines
              line=u_case(file_list%line(j)%Str)

              Select Case (line(1:7))

                 Case("COSTNAM")
                    k=index(line,"!")
                    if( k == 0) then
                      k=len_trim(file_list%line(j)%Str)
                    else
                      k=k-1
                    end if
                    c%Cost_function_name=adjustl(file_list%line(j)%Str(8:k))

                 Case("THRESHO")
                    read(unit=line(10:),fmt=*,iostat=ier) c%threshold
                    if(ier /= 0) c%threshold=25.0

                 Case("TEMPARM")
                    read(unit=line(8:),fmt=*,iostat=ier) c%T_ini,c%anneal,c%num_temps
                    if(ier /= 0) exit do_read
                    tempar=.true.

                 Case("ALGOR_T")
                    read(unit=line(8:),fmt=*,iostat=ier) c%nalgor,c%num_conf,c%nm_cycl, c%num_therm, c%accept
                    if(ier /= 0) exit do_read
                    algor=.true.

                 Case("SEEDVAL")
                    read(unit=line(8:),fmt=*,iostat=ier) c%seed
                    if(ier /= 0) c%seed=0

                 Case("INITCON")
                    line = adjustl(line(8:))
                    if(line(1:3) /= "RAN") c%initconfig = 1

              End Select
              if(tempar .and. algor) notset=.false.
           end do
           exit do_read
         end if
         noinst=.true.
       end do do_read
       if (notset) then
          Err_CFML%Ierr=1
          if (noinst) then
             Err_CFML%Msg=" => No Simulated Annealing conditions in input file "
          else if(.not. tempar) then
             Err_CFML%Msg=&
             " => Unable to set Simulated Annealing conditions (Error in line: TemParM T_ini anneal num_temps num_therm)"
          else if(.not. algor) then
             Err_CFML%Msg= &
             " => Unable to set Simulated Annealing conditions (Error in line: Algor_T  nalgor  num_conf  nm_cycl   num_therm)"
          else
             Err_CFML%Msg=" Unable to set Simulated Annealing conditions (Error in CFL file) "
          end if
       end if

    End Subroutine Set_SimAnn_Cond_FT

    Module Subroutine Set_SimAnn_Cond_FLT(file_list,c)
       !---- Arguments ----!
       type(file_list_type),        intent( in)  :: file_list
       type(SimAnn_Conditions_type),intent(out)  :: c

       !--- Local Variables ---!
       integer           :: i,j,k,ier
       logical           :: notset,tempar,algor,noinst
       character(len=80) :: line

       notset=.true.
       tempar=.false.
       algor =.false.

       c%t_ini=5.0      ! Initial temperature
       c%anneal=0.9     ! Kirpactrick factor for Annealing
       c%accept=0.01    ! Minimum percentage of accepted configurations
       c%threshold=0.0  ! Maximun value of an acceptable cost function
       c%initconfig=0   ! Flag determining if the first configuration is random or read
       c%nalgor=0    ! Flag determining if the Corana algorithm is selected (0) or not (/=0)
       c%nm_cycl=0   ! Number of Cycles per temp  in SA searchs
       c%num_temps=1 ! Maximum number of temperatures in SA
       c%num_therm=0 ! Number of thermalization cycles in SA
       c%num_conf=1  ! Number of paralell configurations in SA
       c%Cost_function_name=" Unnamed Cost Function"
       c%seed=0      ! If different from zero, holds the seed for random number generator

       do_read: do i=1,file_list%nlines
         if(file_list%line(i)(1:7) == "SIM_ANN") then

           do j=i+1,file_list%nlines
              line=u_case(file_list%line(j))

              Select Case (line(1:7))

                 Case("COSTNAM")
                    k=index(line,"!")
                    if( k == 0) then
                      k=len_trim(file_list%line(j))
                    else
                      k=k-1
                    end if
                    c%Cost_function_name=adjustl(file_list%line(j)(8:k))

                 Case("THRESHO")
                    read(unit=line(10:),fmt=*,iostat=ier) c%threshold
                    if(ier /= 0) c%threshold=25.0

                 Case("TEMPARM")
                    read(unit=line(8:),fmt=*,iostat=ier) c%T_ini,c%anneal,c%num_temps
                    if(ier /= 0) exit do_read
                    tempar=.true.

                 Case("ALGOR_T")
                    read(unit=line(8:),fmt=*,iostat=ier) c%nalgor,c%num_conf,c%nm_cycl, c%num_therm, c%accept
                    if(ier /= 0) exit do_read
                    algor=.true.

                 Case("SEEDVAL")
                    read(unit=line(8:),fmt=*,iostat=ier) c%seed
                    if(ier /= 0) c%seed=0

                 Case("INITCON")
                    line = adjustl(line(8:))
                    if(line(1:3) /= "RAN") c%initconfig = 1

              End Select
              if(tempar .and. algor) notset=.false.
           end do
           exit do_read
         end if
         noinst=.true.
       end do do_read

       if (notset) then
          Err_CFML%Ierr=1
          if (noinst) then
             Err_CFML%Msg=" => No Simulated Annealing conditions in input file "
          else if(.not. tempar) then
             Err_CFML%Msg=&
             " => Unable to set Simulated Annealing conditions (Error in line: TemParM T_ini anneal num_temps num_therm)"
          else if(.not. algor) then
             Err_CFML%Msg= &
             " => Unable to set Simulated Annealing conditions (Error in line: Algor_T  nalgor  num_conf  nm_cycl   num_therm)"
          else
             Err_CFML%Msg=" Unable to set Simulated Annealing conditions (Error in CFL file) "
          end if
       end if

    End Subroutine Set_SimAnn_Cond_FLT

    !!----
    !!---- Module Subroutine Set_SimAnn_MStateV(n,nsol,Con,Bounds,VNam,Vec,vs,cod)
    !!----    integer,                      intent(in) :: n,nsol !number of parameters & configurations
    !!----    integer,         dimension(:),intent(in) :: con    !Boundary conditions
    !!----    real(kind=cp),          dimension(:,:),intent(in) :: Bounds ! (1,:)-> Low, (2,:) -> High, (3,:) -> Step
    !!----    character(len=*),dimension(:),intent(in) :: VNam   !Names of parameters
    !!----    real(kind=cp),            dimension(:),intent(in) :: Vec    !Initial value of parameters
    !!----    type(MultiState_Vector_Type), intent(out):: vs     !Initial State vector
    !!----    integer,optional,dimension(:),intent(in) :: cod    !If present, cod(i)=0 fix the "i" parameter
    !!----
    !!----
    !!----    Subroutine for setting up the State_Vector_type
    !!----    variable "vs"
    !!----
    !!---- Update: April - 2005
    !!
    Module Subroutine Set_SimAnn_MStateV(n,nsol,Con,Bounds,VNam,Vec,vs,cod)
       !---- Arguments ----!
       integer,                                     intent(in) :: n,nsol
       integer,                      dimension(:),  intent(in) :: con
       real(kind=cp),                dimension(:,:),intent(in) :: Bounds
       character(len=*),             dimension(:),  intent(in) :: VNam
       real(kind=cp),                dimension(:),  intent(in) :: Vec
       type(MultiState_Vector_Type),                intent(out):: vs
       integer, optional,            dimension(:),  intent(in) :: cod

       !---- Local Variables ----!
       integer :: j

       if (n > np_SAN) then
          Err_CFML%Ierr=1
          write(unit=Err_CFML%Msg,fmt="(a,i4,a)") " => Too many parameters! Simulated Anneling module limited to ",&
               np_SAN," parameters"
          return
       end if
       vs%nconf= 0
       vs%npar = 0
       vs%code(:) = 1
       vs%bound(:) = 0
       vs%state(:,:) = 0.0
       vs%low(:) = 0.0
       vs%high(:) = 0.0
       vs%stp(:,:) = 0.0
       vs%config(:) = 0.0
       vs%sigma(:) = 0.0
       vs%cost(:) =0.0
       vs%Nampar(:) = " "
       vs%best_cost=1.0e8

       !----  Copying arguments in state vector
       vs%npar        = n
       vs%nconf       = nsol
       vs%code(1:n)   = 1
       vs%bound(1:n)  = Con(1:n)
       vs%low(1:n)    = Bounds(1,1:n)
       vs%high(1:n)   = Bounds(2,1:n)
       do j=1,nsol
          vs%stp(1:n,j)   = Bounds(3,1:n)
          vs%state(1:n,j) = vec(1:n)
       end do
       vs%config(1:n)   = vec(1:n)
       vs%Nampar(1:n)   = vNam(1:n)
       vs%Nampar(1:n)   = adjustr(vs%Nampar(1:n))
       if (present(cod)) then
          vs%code(1:n)  = Cod(1:n)
       end if

    End Subroutine Set_SimAnn_MStateV

    !!----
    !!---- Module Subroutine Set_SimAnn_StateV(n,Con,Bounds,VNam,Vec,vs,cod)
    !!----    integer,                      intent(in) :: n      !number of parameters
    !!----    integer,         dimension(:),intent(in) :: con    !Boundary conditions
    !!----    real(kind=cp), dimension(:,:),intent(in) :: Bounds ! (1,:)-> Low, (2,:) -> High, (3,:) -> Step
    !!----    character(len=*),dimension(:),intent(in) :: VNam   !Names of parameters
    !!----    real(kind=cp),   dimension(:),intent(in) :: Vec    !Initial value of parameters
    !!----    type(State_Vector_Type),      intent(out):: vs     !Initial State vector
    !!----    integer,optional,dimension(:),intent(in) :: cod    !If present, cod(i)=0 fix the "i" parameter
    !!----
    !!----    Subroutine for setting up the State_Vector_type
    !!----    variable "vs"
    !!----
    !!---- Update: April - 2005
    !!
    Module Subroutine Set_SimAnn_StateV(n,Con,Bounds,VNam,Vec,vs,cod)
       !---- Arguments ----!
       integer,                                intent(in) :: n
       integer,                 dimension(:),  intent(in) :: con
       real(kind=cp),           dimension(:,:),intent(in) :: Bounds
       character(len=*),        dimension(:),  intent(in) :: VNam
       real(kind=cp),           dimension(:),  intent(in) :: Vec
       type(State_Vector_Type),                intent(out):: vs
       integer, optional,       dimension(:),  intent(in) :: cod

       if (n > np_SAN) then
          Err_CFML%Ierr=1
          write(unit=Err_CFML%Msg,fmt="(a,i4,a)") " => Too many parameters! Simulated Anneling module limited to ",&
               np_SAN," parameters"
          return
       end if

       vs%npar = 0
       vs%code(:) = 1
       vs%bound(:) = 0
       vs%state(:) = 0.0
       vs%low(:) = 0.0
       vs%high(:) = 0.0
       vs%stp(:) = 0.0
       vs%config(:) = 0.0
       vs%cost = 0.0
       vs%Nampar(:) = " "

       ! Copying arguments in state vector
       vs%npar         = n
       vs%code(1:n)    = 1
       vs%bound(1:n)   = Con(1:n)
       vs%state(1:n)   = vec(1:n)
       vs%low(1:n)     = Bounds(1,1:n)
       vs%high(1:n)    = Bounds(2,1:n)
       vs%stp(1:n)     = Bounds(3,1:n)
       vs%config(1:n)  = vec(1:n)
       vs%Nampar(1:n)  = vNam(1:n)
       vs%Nampar(1:n)  = adjustr(vs%Nampar(1:n))
       if (present(cod)) then
          vs%code(1:n) = Cod(1:n)
       end if

    End Subroutine Set_SimAnn_StateV

 End Submodule SAnn_SetnCheck
