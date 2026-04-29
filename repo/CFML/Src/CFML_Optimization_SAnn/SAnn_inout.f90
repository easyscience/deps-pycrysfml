 Submodule (CFML_Simulated_Annealing) SAnn_inout
  implicit none
   contains
    !!----
    !!---- Module Subroutine Write_SimAnn_Cond(ipr,c)
    !!----    integer,                     intent(in)  :: ipr !Logical unit for writing
    !!----    type(SimAnn_Conditions_type),intent(in)  :: c   !SAN Conditions
    !!----
    !!----    Subroutine for Writing in unit=ipr the SimAnn_Conditions_type
    !!----    variable "c"
    !!----
    !!---- Update: April - 2005
    !!
    Module Subroutine Write_SimAnn_Cond(ipr,c)
       !---- Arguments ----!
       integer,                     intent(in)  :: ipr
       type(SimAnn_Conditions_type),intent(in)  :: c

       !--- Local Variables ---!


       write(unit=ipr,fmt="(/,a)") "     ===================================="
       write(unit=ipr,fmt="(a)")   "     =  SIMULATED ANNEALING CONDITIONS  ="
       write(unit=ipr,fmt="(a,/)") "     ===================================="

       write(unit=ipr,fmt="(a)")        " =>               Cost Function name: "//trim(c%Cost_function_name)
       write(unit=ipr,fmt="(a,f8.3)")   " =>              Initial Temperature: ",c%T_ini
       write(unit=ipr,fmt="(a,f8.3)")   " =>                 Annealing Factor: ",c%anneal
       write(unit=ipr,fmt="(a,i4  )")   " =>   Maximum number of Temperatures: ",c%num_temps
       if(c%nalgor == 0) then
          write(unit=ipr,fmt="(a)")     " => Corana's variable step algorithm  "
       else if(c%nalgor == 1) then
          write(unit=ipr,fmt="(a)")     " => Corana's variable step algorithm  "
       else
          write(unit=ipr,fmt="(a)")     " => Const. step conventional algorithm "
       end if
       write(unit=ipr,fmt="(a,i4  )")   " =>  Number of Montecarlo cycles/Temp: ",c%nm_cycl
       write(unit=ipr,fmt="(a,i4  )")   " =>   Number of thermalization cycles: ",c%num_therm
       if(c%num_conf > 1) then
          write(unit=ipr,fmt="(a,i8)")     " => Number of paralell configurations: ",c%num_conf
          write(unit=ipr,fmt="(a,f8.3)")     " =>   Cost threshold for good configs: ",c%threshold
       else
          write(unit=ipr,fmt="(a)")     " => Conventional SAnn single configuration "
       end if
       if(c%seed == 0) then
          write(unit=ipr,fmt="(a)")     " => Random Seed selected from system clock "
       else
          write(unit=ipr,fmt="(a,i8)")  " =>     Initial Seed selected by user: ",c%seed
       end if
       if(c%initconfig == 0) then
          write(unit=ipr,fmt="(a)")     " =>    Initial random configuration "
       else
          write(unit=ipr,fmt="(a)")     " =>    Initial configuration as given  "
       end if
       return

    End Subroutine Write_SimAnn_Cond

    !!----
    !!---- Module Subroutine Write_SimAnn_MStateV(ipr,vs,text,cost)
    !!----    integer,                     intent(in) :: ipr   !Logical unit for writing
    !!----    type(MultiState_Vector_Type),intent(in) :: vs    !State vector
    !!----    character(len=*),            intent(in) :: text
    !!----    integer, optional,           intent(in) :: cost
    !!----
    !!----    Subroutine for Writing in unit=ipr the SimAnn_Conditions_type
    !!----    variable "c"
    !!----
    !!---- Update: April - 2005
    !!
    Module Subroutine Write_SimAnn_MStateV(ipr,vs,text,cost)
       !---- Arguments ----!
       integer,                     intent(in) :: ipr
       type(MultiState_Vector_Type),intent(in) :: vs
       character(len=*),            intent(in) :: text
       integer, optional,           intent(in) :: cost

       !--- Local Variables ---!
       integer :: i,j
       character(len=32) :: forma1,forma2
       character(len=15) :: namep

       forma1="(a,   (a,i2))"
       write(unit=forma1(4:6),fmt="(i3)") vs%nconf
       forma2="(i6,3x,a12,2f12.5,2i6,   f12.5))"
       write(unit=forma2(23:25),fmt="(i3)") 2*vs%nconf+1

       write(unit=ipr,fmt="(/,a)") "     ========================================================="
       write(unit=ipr,fmt="(  a)") "     => SIMULATED ANNEALING MULTI-STATE VECTOR: "//trim(text)
       write(unit=ipr,fmt="(a,/)") "     ========================================================="

       write(unit=ipr,fmt=forma1) "   Num           Name     Low_lim    High_Lim  BCond Code    BestConf",&
                                  ("      Value&Step Conf#",j, j=1,vs%nconf)
       do i=1,vs%npar
          namep=adjustl(vs%nampar(i))
          write(unit=ipr,fmt=forma2) i,namep,vs%low(i),vs%high(i),vs%bound(i),vs%code(i),vs%config(i),&
                                   (vs%state(i,j),vs%stp(i,j),j=1,vs%nconf)
       end do
       if (present(cost)) then
          forma1="(   (a,i2,a,f10.4))"
          write(unit=forma1(2:4),fmt="(i3)") vs%nconf
          write(unit=ipr,fmt="(/,a,/)") "  ==> Cost Function values of the different configurations"
          write(unit=ipr,fmt=forma1)   ("      Cost#",j,":",vs%Cost(j),j=1,vs%nconf)
       end if

    End Subroutine Write_SimAnn_MStateV

    !!----
    !!---- Module Subroutine Write_SimAnn_StateV(ipr,vs,text)
    !!----    integer,                intent(in)  :: ipr   !Logical unit for writing
    !!----    type(State_Vector_Type),intent(in)  :: vs    !State vector
    !!----    character(len=*),       intent(in)  :: text
    !!----
    !!----    Subroutine for Writing in unit=ipr the State_Vector_Type
    !!----    variable "vs"
    !!----
    !!---- Update: April - 2005
    !!
    Module Subroutine Write_SimAnn_StateV(ipr,vs,text)
       !---- Arguments ----!
       integer,                intent(in)  :: ipr
       type(State_Vector_Type),intent(in)  :: vs
       character(len=*),       intent(in)  :: text

       !--- Local Variables ---!
       integer           :: i
       character(len=30) :: forma
       character(len=15) :: namep

       forma="(i6,3x,a8,3f12.5,2(i7,f12.5))"

       write(unit=ipr,fmt="(/,a)") "     ================================================="
       write(unit=ipr,fmt="(  a)") "     => SIMULATED ANNEALING STATE VECTOR: "//trim(text)
       write(unit=ipr,fmt="(a,/)") "     ================================================="
       write(unit=ipr,fmt="(a)") "   Num    Name       Value     Low_lim    High_Lim    BCond      Step    Code   BestConf"
       do i=1,vs%npar
          namep=adjustl(vs%nampar(i))
          write(unit=ipr,fmt=forma)  i,namep,vs%state(i),vs%low(i),vs%high(i),vs%bound(i),vs%stp(i),vs%code(i),vs%config(i)
       end do

    End Subroutine Write_SimAnn_StateV

 End Submodule SAnn_inout