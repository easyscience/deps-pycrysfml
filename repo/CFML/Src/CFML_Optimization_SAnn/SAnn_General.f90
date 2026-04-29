 Submodule (CFML_Simulated_Annealing) SAnn_General
  implicit none
   contains

    !!----
    !!---- Module Subroutine SimAnneal_Gen(Model_Funct,c,vs,Ipr,fileSav,fst)
    !!----    type(SimAnn_Conditions_type),intent(in out)  :: c
    !!----    type(State_Vector_Type),     intent(in out)  :: vs
    !!----    integer,                     intent(in)      :: Ipr
    !!----    character(len=*), optional,  intent(in)      :: filesav
    !!----    character(len=*), optional,  intent(in)      :: fst
    !!----    Interface
    !!----       Subroutine Model_Funct(v,cost)
    !!----          real,dimension(:),    intent( in):: v
    !!----          real,                 intent(out):: cost
    !!----       End Subroutine Model_Funct
    !!----       Subroutine Write_FST(fst_file,v,cost)
    !!----          character(len=*),     intent(in):: fst_file
    !!----          real,dimension(:),    intent(in):: v
    !!----          real,                 intent(in):: cost
    !!----       End Subroutine Write_FST
    !!----    End Interface
    !!----
    !!---- Update: March - 2005
    !!
    Module Subroutine SimAnneal_Gen(Model_Funct,c,vs,Ipr,fileSav,fst)
       !---- Arguments ----!
       type(SimAnn_Conditions_type),intent(in out)  :: c
       type(State_Vector_Type),     intent(in out)  :: vs
       integer,                     intent(in)      :: Ipr
       character(len=*), optional,  intent(in)      :: filesav
       character(len=*), optional,  intent(in)      :: fst

       Interface
          Subroutine Model_Funct(v,cost)
             Use CFML_GlobalDeps, only: Cp
             real(kind=cp),dimension(:),    intent( in):: v
             real(kind=cp),                 intent(out):: cost
          End Subroutine Model_Funct

          Subroutine Write_FST(fst_file,v,cost)
             Use CFML_GlobalDeps, only: Cp
             character(len=*),            intent(in):: fst_file
             real(kind=cp),dimension(:),  intent(in):: v
             real(kind=cp),               intent(in):: cost
          End Subroutine Write_FST
       End Interface

       !--- Local Variables ---!
       character(len=256)                 :: messag, strings
       integer, parameter                 :: i_conf=99
       integer, dimension(1)              :: seed
       integer                            :: i, j, neval, ncf, jk, naj, ntp, naver, jj !, last
       real (kind=cp)                     :: temp, cost, cost1, cost2, costop, ep, ener, cpp, dsen, dsen2, &
                                             energ, paj, prob, rav, rati, plage, stepav,random !, shift
       integer,          dimension(np_SAN) :: nacp       !number of accepted moves for parameter i
       real (kind=cp),   dimension(np_SAN) :: stateo     !Vector State characterizing the old configuration
       real (kind=cp),   dimension(np_SAN) :: cv         !constant vector used in the Corana algorithm
       real (kind=cp),   dimension(np_SAN) :: raver      !Vector State characterizing the average configuration
       real (kind=cp),   dimension(np_SAN) :: sigp       !Standard deviations of the average configuration

       call check(c,vs)
       if (Err_CFML%Ierr /= 0) then
          call mess(Err_CFML%Msg)
          return
       end if
       seed(1)=c%seed
       call init_ran(seed)
       jj=min(vs%npar,26)

       if (present(filesav)) then
          open(unit=i_conf,file=trim(filesav)//".anl",status="replace",action="write")
          write(unit=i_conf,fmt="(a,a)")"! Simulated Anneling Run with cost function: ", trim(c%Cost_function_name)
          write(unit=i_conf,fmt="(a,26(a,a))") &
                   "! NT    Temp  <Cost-val>   %Accpt     <Step>     Cp   ",("  ",vs%nampar(i),i=1,jj)
          call flush(i_conf)
          close(unit=i_conf)
       end if

       if (c%nalgor == 0) then
          vs%stp(1:vs%npar)=vs%high(1:vs%npar)-vs%low(1:vs%npar)
       end if
       cv(:)=2.0

       !---- Get the initial configuration in config(1:msz) ----!
       if (c%initconfig == 1) then
          do i=1,vs%npar
             vs%state(i) = vs%config(i)
          end do
       else
          do i=1,vs%npar
             if (vs%code(i)==1) then
                call random_number(random)
                vs%state(i) = vs%low(i) + random*(vs%high(i)-vs%low(i))
            else
                vs%state(i)=vs%config(i)
             end if
          end do
       end if
       stateo(:)=vs%state(:)

       !---- Determine the initial value of the cost function -> cost1 ----!
       call Model_Funct(stateo,Cost1)

       costop=cost1
       vs%config(:)=stateo(:)

       messag=" ---- Simulated Annealing to minimize General Cost Functions ----"
       write(unit=ipr,fmt="(/,a,/,a,/)") messag, "     Cost-function name:  "//trim(c%Cost_function_name)
       call mess(" ")
       call mess(messag)
       call mess(" ")

       strings=" "
       write(unit=strings,fmt="(a,g16.6)") " => Initial configuration cost: ",cost1
       call mess(strings)
       write(unit=ipr,fmt="(a)") trim(strings)

       strings=" "
       write(unit=strings,fmt="(a)") " => Initial configuration state vector: "
       call mess(strings)
       write(unit=ipr,fmt="(a)") trim(strings)

       do i=1,vs%npar
          write(unit=strings,fmt="(i6,a,F16.5)")  i,vs%nampar(i), vs%state(i)
          write(unit=ipr,fmt="(a)") trim(strings)
          call mess(strings)
       end do

       !---- Loop over temperatures ----!
       naver=c%nm_cycl-c%num_therm
       rav= real(naver*vs%npar)
       temp=c%t_ini/c%anneal
       neval=0
       nacp=0
       do_temp: do ntp=1,c%num_temps     ! Global DO for changing temperature
          naj=0
          cost=0.0
          energ=0.0
          sigp(:)=0.0
          raver(:)=0.0
          dsen=0.0
          dsen2=0.0

          temp=c%anneal*Temp  ! Current temperature

          do ncf=1,c%nm_cycl
            ! last=vs%npar

             cyc_par: do i=1,vs%npar        !loop on the components of the vector state
                if (vs%code(i) == 0) cycle cyc_par
                !---- Set new configuration and store the previous one in config(1:msz) ----!
                call random_number(random)
                vs%state(i)=stateo(i)+vs%stp(i)*(2.0*random-1.0)
                plage=vs%high(i)-vs%low(i)
                if (vs%bound(i) == 0) then   !boundary conditions
                   if (vs%state(i) < vs%low(i) .or. vs%state(i) > vs%high(i))  &  !Fixed boundaries
                      call random_number(random)
                   vs%state(i)=vs%low(i)+ random*plage
                else
                   if (vs%state(i) < vs%low(i) ) vs%state(i)=vs%state(i)+plage  !Periodic boundary conditions
                   if (vs%state(i) > vs%high(i) ) vs%state(i)=vs%state(i)-plage
                end if
                ! shift=vs%state(i)-stateo(i)

                !---- Calculate the cost function ----!
                call Model_Funct(vs%state,Cost2)
                neval=neval+1
                ener= cost2-cost1

                !---- Metropolis test ----!
                if (ener > 0.0 ) then
                   ep=ener/temp
                   if (ep > 88.7228) then
                      prob=0.0
                   else
                      prob=exp(-ep)
                   end if
                   call random_number(random)
                   if (prob <= random) then
                      !---- Restore the old configuration ----!
                      vs%state(i)=stateo(i)
                      cycle cyc_par   !cycle and try another configuration
                   end if
                end if

                !---- Accepted configuration ----!
                cost1=cost2               !update the cost function
                stateo(i)=vs%state(i)     !update configuration
                if (cost1 < costop) then  !the best current configuration is found
                   costop=cost1
                   vs%config(:)=stateo(:)
                   vs%cost=costop
                   if(present(fst)) then
                      call Write_FST(fst,vs%config(:),costop)
                   end if
                end if

                if (ncf <= c%Num_therm) cycle cyc_par
                nacp(i)=nacp(i)+1
                naj=naj+1   !number of accepted jumps
                energ=energ+abs(ener)
                cost=cost+cost2
                dsen=dsen+ener
                dsen2=dsen2+ener*ener

             end do cyc_par  !end loop over components of state vector

             do j=1,vs%npar
                sigp(j)=sigp(j)+stateo(j)*stateo(j)
                raver(j)=raver(j)+stateo(j)
             end do

          end do    !End loop over Montecarlo moves at fixed temperature

          !---- Statistic  and average values for previous temperature ----!
          if (naj == 0) then
             naj=1
             cost=cost1
             energ=abs(ener)
             dsen=ener
             dsen2=ener*ener
          end if
          paj=100.0*real(naj)/rav

          energ=energ/naj
          cost=cost/naj
          dsen=dsen/naj
          dsen2=dsen2/naj
          cpp=(dsen2-dsen*dsen)/(temp*temp)
          do j=1,vs%npar
             raver(j)=raver(j)/naver
             sigp(j)=sqrt(abs(sigp(j)/naver-raver(j)*raver(j)))
          end do
          stepav=sum(vs%stp(:))/real(vs%npar)


          !---- Writing partial results ----!
          strings=" "
          write(unit=strings,fmt="(a,i4,2(a,f8.3),a,f10.5,a,g14.7)")  &
               " => NT:",ntp," Temp:",temp," (%Acc):",paj,"  <Step>:",stepav,"  <"//trim(c%Cost_function_name)//">:",cost
          call mess(strings)
          write(unit=ipr,fmt="(/,a)") trim(strings)
          strings=" "
          write(unit=strings,fmt="(a,g14.7,a,g14.7,a,i10 )")  &
               "    Cost-val:",energ, "     Cp:", cpp, "     Num-Cost-Evaluations: ",neval
          write(unit=ipr,fmt="(a)") trim(strings)

          write(unit=ipr,fmt="(a)") " => Average value of parameters, sigmas and steps:"
          do i=1,vs%npar
            write(unit=strings,fmt="(i6,a,3F16.5)")  i,vs%nampar(i), raver(i),sigp(i),vs%stp(i)
            write(unit=ipr,fmt="(a)") trim(strings)
          end do


          jk= min(vs%npar,26)
          if (present(filesav)) then
             open(unit=i_conf,file=trim(filesav)//".anl",status="old",action="write", position="append")
             write(unit=i_conf,fmt="(i4,31f10.4)")  ntp,temp,cost,paj,stepav,cpp,(raver(jj),jj=1,jk)
             call flush(i_conf)
             close(unit=i_conf)
          end if
          if(cost < c%threshold) then
             write(unit=strings,fmt="(a,f8.2,a,f8.2)") &
             " => Local Optimization of the best configuration because <Cost> = ",cost," is less than threshold ", c%threshold
             call mess(strings)
             write(unit=ipr,fmt="(a)") trim(strings)
             call Local_Optim(Model_Funct,vs%npar,vs%config,costop,vs%low,vs%high,vs%bound)
             write(unit=strings,fmt="(a,f8.2)") " => Final configuration locally optimized. Final cost: ",costop
             call mess(strings)
             write(unit=ipr,fmt="(a)") trim(strings)
             vs%state(:)=vs%config(:)
             vs%cost=costop
             Cost1=costop
             cost2=costop
             if (present(fst)) then
                call Write_FST(fst,vs%config(:),costop)
             end if
             exit do_temp
          end if

          if (paj-c%accept <=0 ) exit   !Convergence criterium

          !---- Adjust STP so that approximately half of all evaluations are accepted ----!
          !---- (Corana's algorithm)

          if (c%nalgor == 0 .or. c%nalgor == 1) then
             do i=1,vs%npar
                jj=0
                 if (vs%stp(i) > abs(0.01*c%accept*raver(i))) exit
                jj=1
             end do
             if (jj == 1) exit
             do i = 1, vs%npar
                plage=vs%high(i)-vs%low(i)
                rati = real(nacp(i)) /real(naver)
                if (rati > 0.6) then
                   vs%stp(i) = vs%stp(i)*(1.0 + cv(i)*(rati - 0.6)/0.4)
                else if (rati < 0.4) then
                   vs%stp(i) = vs%stp(i)/(1.0 + cv(i)*((0.4 - rati)/0.4))
                end if
                if (vs%stp(i) > plage) then
                   vs%stp(i) = plage
                end if
             end do
          end if
          nacp(:) = 0

       end do  do_temp !ntp=1,c%num_temps

       !---- Re-calculate the cost function for the best configuration ----!
       call Model_Funct(vs%config,Cost2)

       messag="  "
       call mess(messag)
       messag=" => Best configuration found by Simulated Annealing (Sigma of the last Montecarlo Cycles):"
       call mess(messag)
       write(unit=ipr,fmt="(/,a,a,/)") "     ",trim(messag)
       strings=" "
       write(unit=strings,fmt="(a,f16.4,a)")" -> Best Solution Cost (before eventual local optimization)=",cost2," :: "
       call mess(strings)
       write(unit=ipr,fmt="(/,a)") trim(strings)
       messag=" -> Configuration parameters :"
       call mess(messag)
       write(unit=ipr,fmt="(a,/)") trim(messag)
       do i=1,vs%npar
         write(unit=strings,fmt="(i6,a,2F16.5)")  i,"  "//vs%nampar(i), vs%config(i),sigp(i)
         write(unit=ipr,fmt="(a)") trim(strings)
         call mess(strings)
       end do

    End Subroutine SimAnneal_Gen

 End Submodule SAnn_General