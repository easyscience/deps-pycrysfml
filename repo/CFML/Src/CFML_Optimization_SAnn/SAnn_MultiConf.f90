 Submodule (CFML_Simulated_Annealing) SAnn_MultiConf
  implicit none
   contains
    !!----
    !!---- Module Subroutine SAnn_Opt_MultiConf(Model_Funct,c,vs,Ipr,fileSav,fst)
    !!----    type(SimAnn_Conditions_type),  intent(in out)  :: c
    !!----    type(MultiState_Vector_Type),  intent(in out)  :: vs
    !!----    integer,                       intent(in)      :: Ipr
    !!----    character(len=*), optional,    intent(in)      :: filesav
    !!----    character(len=*), optional,    intent(in)      :: fst
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
    !!----    Multiconfigurational Simmulated Annealing with local optimization
    !!----    when a configuration with cost lower than a threshold value is given
    !!----    or when one of the Markov chains stalls
    !!----
    !!---- Update: March - 2005
    !!
    Module Subroutine SAnn_Opt_MultiConf(Model_Funct,c,vs,Ipr,fileSav,fst)
       !---- Arguments ----!
       type(SimAnn_Conditions_type),  intent(in out)  :: c
       type(MultiState_Vector_Type),  intent(in out)  :: vs
       integer,                       intent(in)      :: Ipr
       character(len=*), optional,    intent(in)      :: filesav
       character(len=*), optional,    intent(in)      :: fst

       Interface
          Subroutine Model_Funct(v,cost)
             Use CFML_GlobalDeps, only: Cp
             real(kind=cp),dimension(:),    intent( in):: v
             real(kind=cp),                 intent(out):: cost
          End Subroutine Model_Funct

          Subroutine Write_FST(fst_file,v,cost)
             Use CFML_GlobalDeps, only: Cp
             character(len=*),              intent(in):: fst_file
             real(kind=cp),dimension(:),    intent(in):: v
             real(kind=cp),                 intent(in):: cost
          End Subroutine Write_FST
       End Interface

       !--- Local Variables ---!
       character (len=256)                         :: messag, strings
       integer                                     :: i, j, k, neval, ncf, ntp, naver, jj, survive, jopt, &
                                                      minut,itick1,itick2,count_rate  !, last
       real(kind=cp)                               :: temp, ep, ener, costop, half_init_avstp, sumdel,sumsig, &
                                                      prob, rav, rati, plage, stepav, random, tini,tf,minuts,sec !, shift
       integer, parameter                          :: i_conf=99
       integer, dimension(1)                       :: seed
       logical, dimension(np_CONF)                 :: dead
       integer, dimension(np_CONF)                 :: naj
       real(kind=cp),    dimension(np_CONF)        :: cost, cost1, cost2, paj, costav
       real(kind=cp),    dimension(np_SAN,np_CONF) :: stateo     !Vector State characterizing the old configuration
       real(kind=cp),    dimension(np_SAN)         :: cv         !constant vector used in the Corana algorithm
       integer,          dimension(np_SAN,np_CONF) :: nacp       !number of accepted moves for parameter i
       real(kind=cp),    dimension(np_SAN,np_CONF) :: raver      !Vector State characterizing the average configuration
       real(kind=cp),    dimension(np_SAN,np_CONF) :: sigp       !Standard deviations of the average configuration

       call checkm(c,vs)
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

       cv(1:vs%npar)=vs%high(1:vs%npar)-vs%low(1:vs%npar)
       half_init_avstp=0.5*sum(cv(1:vs%npar)/real(vs%npar))

       if (c%nalgor == 0) then
         do j=1,vs%nconf
          vs%stp(1:vs%npar,j)=cv(1:vs%npar)
         end do
       else
         half_init_avstp=4.0*half_init_avstp
       end if

       cv(:)=2.0
       stateo=0.0
       !---- Get the initial configuration in config(1:msz) ----!
       dead(:)=.false.
       if (c%initconfig == 1) then
          do j=1,vs%nconf
            do i=1,vs%npar
               vs%state(i,j) = vs%config(i)
            end do
          end do
       else
          do j=1,vs%nconf
            do i=1,vs%npar
               if (vs%code(i)==1) then
                  call random_number(random)
                  vs%state(i,j) = vs%low(i) + random*(vs%high(i)-vs%low(i))
               else
                  vs%state(i,j)=vs%config(i)
               end if
               stateo(i,j)=vs%state(i,j)
            end do
          end do
       end if

       !---- Determine the initial values of the cost function -> cost1 ----!
       do j=1,vs%nconf
         call Model_Funct(stateo(:,j),Cost1(j))
       end do
         j=minloc(cost1(1:vs%nconf),dim=1)
         vs%config(:)=stateo(:,j)  !Best configuration for the moment
         costop=cost1(j)
         jopt=j

       messag=" ---- MultiConfiguration Simulated Annealing to minimize General Cost Functions ----"
       write(unit=ipr,fmt="(/,a,/,a,/)") messag, "     Cost-function name:  "//trim(c%Cost_function_name)
       call mess(" ")
       call mess(messag)
       call mess(" ")

       do j=1,vs%nconf
         strings=" "
         write(unit=strings,fmt="(a,i2,a,g12.5)") " => Initial configuration cost(",j,"): ",cost1(j)
         call mess(strings)
         write(unit=ipr,fmt="(a)") trim(strings)
       end do
       strings=" "
       write(unit=strings,fmt="(a)") " => Initial Best configuration state vector: "
       call mess(strings)
       write(unit=ipr,fmt="(a)") trim(strings)

       strings=" "
       do i=1,vs%npar
         write(unit=strings,fmt="(i6,a,f16.5)")  i,vs%nampar(i), vs%config(i)
         write(unit=ipr,fmt="(a)") trim(strings)
         call mess(strings)
       end do

       !---- Loop over temperatures ----!
       naver=c%nm_cycl-c%num_therm
       rav= real(naver*vs%npar)
       temp=c%t_ini/c%anneal
       neval=0
       nacp=0
       costav(:)=0.0
       !call cpu_time(tini)
       call system_clock(count_rate=count_rate)
       call system_clock(itick1)
       tini=itick1/real(count_rate)
       do ntp=1,c%num_temps     ! Global DO for changing temperature
            naj(:)   =0
           cost(:)   =0.0
           sigp(:,:) =0.0
          raver(:,:) =0.0

          temp=c%anneal*Temp  ! Current temperature

          strings=" "
          !call cpu_time(tf)   ! minutos=(tf-tini)/60  min=int(minutos)  minutos-min=
          call system_clock(itick2)
          tf=itick2/real(count_rate)
          sec=tf-tini
          minuts=sec/60.0
          minut=int(minuts)
          sec=(minuts-real(minut))*60.0
          write(unit=strings,fmt="(a,f9.5,a,i5,a,i8,a,i6,a,f8.3,a)") "  => New Temp:",temp,"  NT: ",ntp, &
               "       Number of function evaluations:",neval,"         Cumulated CPU-time: ",minut," minutes -> ",sec," seconds"
          call mess(strings)
          write(unit=ipr,fmt="(/,a)") trim(strings)

          do j=1,vs%nconf   ! loop over different configuration state vectors

             if(dead(j)) cycle  !skip if configuration is dead

             do ncf=1,c%nm_cycl
                ! last=vs%npar

                cyc_par: do i=1,vs%npar        !loop on the components of the vector state
                   if (vs%code(i) == 0) cycle cyc_par
                   !---- Set new configuration and store the previous one in config(1:msz) ----!
                   call random_number(random)
                   vs%state(i,j)=stateo(i,j)+vs%stp(i,j)*(2.0*random-1.0)
                   plage=vs%high(i)-vs%low(i)
                   if (vs%bound(i) == 0) then   !boundary conditions
                      if (vs%state(i,j) < vs%low(i) .or. vs%state(i,j) > vs%high(i))  &  !Fixed boundaries
                         call random_number(random)
                      vs%state(i,j)=vs%low(i)+ random*plage
                   else
                      if (vs%state(i,j) < vs%low(i) )  vs%state(i,j)=vs%state(i,j)+plage  !Periodic boundary conditions
                      if (vs%state(i,j) > vs%high(i) ) vs%state(i,j)=vs%state(i,j)-plage
                   end if
                 !  shift=vs%state(i,j)-stateo(i,j)

                   !---- Calculate the cost function ----!
                   call Model_Funct(vs%state(:,j),Cost2(j))
                   neval=neval+1
                   ener= cost2(j)-cost1(j)

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
                         vs%state(i,j)=stateo(i,j)
                         cycle cyc_par   !cycle and try another configuration
                      end if
                   end if

                   !---- Accepted configuration ----!
                   cost1(j)=cost2(j)               !update the cost function
                   stateo(i,j)=vs%state(i,j)       !update configuration
                   if (cost1(j) < costop) then     !the best current configuration is found
                      costop=cost1(j)
                      vs%config(:)=stateo(:,j)
                      jopt=j
                      if(present(fst)) then
                        call Write_FST(fst,vs%config(:),costop)
                      end if
                   end if

                   if (ncf <= c%Num_therm) cycle cyc_par
                   nacp(i,j)=nacp(i,j)+1
                   naj(j)=naj(j)+1   !number of accepted jumps for state vector j
                   cost(j)=cost(j)+cost2(j)

                end do cyc_par  !end loop over components of state vector


                do k=1,vs%npar
                   sigp(k,j)= sigp(k,j)+stateo(k,j)*stateo(k,j)
                  raver(k,j)=raver(k,j)+stateo(k,j)
                end do

             end do    !End loop over Montecarlo moves at fixed temperature

          end do   !end loop over configurations

          vs%cost(1:vs%nconf)=cost1(1:vs%nconf)
          if(modulo(ntp,4) == 0) then
            costav=costav/3.0
          end if

          !---- Statistic  and average values for previous temperature ----!
          do j=1,vs%nconf
             if(dead(j)) cycle
             if (naj(j) == 0) then
                naj(j)=1
                cost(j)=cost1(j)
             end if
             paj(j)=100.0*real(naj(j))/rav

             cost(j)=cost(j)/naj(j)   !Average cost

             do i=1,vs%npar
                raver(i,j)=raver(i,j)/naver
                sigp(i,j)=sqrt(abs(sigp(i,j)/naver-raver(i,j)*raver(i,j)))
             end do
             stepav=sum(vs%stp(:,j))/real(vs%npar)
             if(j == jopt) vs%sigma(:)=sigp(:,j)
             !---- Writing partial results ----!
             strings=" "
             write(unit=strings,fmt="(a,i4,a,f8.3,a,i2,a,f8.3,2(a,g14.7))")  &
             "     Conf:",j,"  (%Acc):",paj(j),"  <Step(",j,")>:",stepav, &
             "  <"//trim(c%Cost_function_name)//">:",cost(j),"  -> Current Cost:", cost1(j)
             call mess(strings)
             write(unit=ipr,fmt="(a)") trim(strings)
             write(unit=ipr,fmt="(a,i10 )")  "     Num-Cost-Evaluations: ",neval

            !Apply test of convergence and suppress bad configurations

            if (paj(j) <= c%accept ) then
                 dead(j)=.true. !Convergence criterium
                 write(unit=strings,fmt="(a,i3,a)") " => Configuration #",j," converged => dead in the algorithm!"
                 call mess(strings)
                 write(unit=ipr,fmt="(a)") trim(strings)
            end if

            if(abs(costav(j)-cost(j)) < 0.20 .and. modulo(ntp,4) == 0 .and. stepav < half_init_avstp ) then
                 dead(j)=.true. !Convergence criterium
                 call Local_Optim(Model_Funct,vs%npar,raver(:,j),cost(j),vs%low,vs%high,vs%bound)
                 vs%state(:,j)=raver(:,j)
                 vs%cost(j)=cost(j)
                 cost1(j)=cost(j)
                 write(unit=strings,fmt="(a,i3,a,f8.2)")" => Configuration #",j, &
                 " do not change anymore => dead in the algorithm after local optimization! Final cost: ", cost(j)
                 call mess(strings)
                 write(unit=ipr,fmt="(a)") trim(strings)
                 if (cost(j) < costop) then     !the best current configuration is found
                    write(unit=strings,fmt="(a,f8.2)") " => It becomes the best configuration for the moment!"
                    call mess(strings)
                    write(unit=ipr,fmt="(a)") trim(strings)
                    costop=cost(j)
                    vs%config(:)=raver(:,j)
                    vs%sigma(:)=sigp(:,j)
                    jopt=j
                    if(present(fst)) then
                      call Write_FST(fst,vs%config(:),costop)
                    end if
                 end if
            end if

            do k=j+1,vs%nconf
              if(dead(k)) cycle
            !  sumdel = sum(abs(vs%state(:,j)-vs%state(:,k)))
              sumdel = sum(abs(raver(:,j)- raver(:,k)))
              sumsig= min(0.2, 0.5*sum(sigp(:,j)+sigp(:,k)))
              if( sumdel < sumsig )  then
                 dead(k) = .true.
                 write(unit=strings,fmt="(2(a,i3))") " => Configuration #",k," dead, because it is equal to Configuration #",j
                 call mess(strings)
                 write(unit=ipr,fmt="(a)") trim(strings)
              end if
            end do

            !Perform a local minimization if the cost function is lower than c%threshold
            !Use as starting point the average configuration
            if((cost(j) < c%threshold .and. .not. dead(j)) .or. ntp == c%num_temps) then
               write(unit=strings,fmt="(a,i3,a,f8.2)") " => Local Optimization of Configuration #",j,"   Average Cost: ",cost(j)
               call mess(strings)
               write(unit=ipr,fmt="(a)") trim(strings)
               if(cost1(j) < cost(j)) then
                  cost(j)=cost1(j)
                  raver(:,j) = stateo(:,j)
               end if
               call Local_Optim(Model_Funct,vs%npar,raver(:,j),cost(j),vs%low,vs%high,vs%bound)
               dead(j)=.true.
               write(unit=strings,fmt="(a,i3,a,f8.2)") " => Configuration #",j,&
               " dead, because it has been locally optimized (cost < Threshold). Final cost: ",cost(j)
               call mess(strings)
               write(unit=ipr,fmt="(a)") trim(strings)
               vs%state(:,j)=raver(:,j)
               vs%cost(j)=cost(j)
               cost1(j)=cost(j)
               if (cost(j) < costop) then     !the best current configuration is found
                  call mess(" => It becomes the best configuration for the moment!")
                  write(unit=ipr,fmt="(a)") " => It becomes the best configuration for the moment!"
                  costop=cost(j)
                  vs%config(:)=raver(:,j)
                  vs%sigma(:)=sigp(:,j)
                  jopt=j
                  if (present(fst)) then
                    call Write_FST(fst,vs%config(:),costop)
                  end if
               end if
            end if

          end do !j=1,vs%nconf

          if (present(filesav)) then
             open(unit=i_conf,file=trim(filesav)//".anl",status="old",action="write", position="append")
             write(unit=i_conf,fmt="(i4,31f10.4)")  ntp,temp,costop,vs%config(1:vs%npar)
             call flush(i_conf)
             close(unit=i_conf)
          end if


          survive=0
          do j=1,vs%nconf
           if(dead(j)) cycle
            survive=survive+1
          end do
          if(survive == 0) then
            strings = " => Convergence reached, look the list of configurations"
            call mess(strings)
            write(unit=ipr,fmt="(a)") trim(strings)
            exit
          end if

          !---- Adjust STP so that approximately half of all evaluations are accepted ----!
          !---- (Corana's algorithm)

          if (c%nalgor == 0 .or. c%nalgor == 1) then
             do j=1,vs%nconf
                if(dead(j)) cycle
                do i=1,vs%npar
                   jj=0
                   if (vs%stp(i,j) > abs(0.01*c%accept*raver(i,j))) exit
              !     if (vs%stp(i,j) > c%accept ) exit
                   jj=1
                end do
                if (jj == 1) exit
                do i = 1, vs%npar
                   plage=vs%high(i)-vs%low(i)
                   rati = real(nacp(i,j)) /real(naver)
                   if (rati > 0.6) then
                      vs%stp(i,j) = vs%stp(i,j)*(1.0 + cv(i)*(rati - 0.6)/0.4)
                   else if (rati < 0.4) then
                      vs%stp(i,j) = vs%stp(i,j)/(1.0 + cv(i)*((0.4 - rati)/0.4))
                   end if
                   if (vs%stp(i,j) > plage) then
                      vs%stp(i,j) = plage
                   end if
                end do
             end do !j=1,vs%nconf
          end if
          nacp(:,:) = 0
          if(modulo(ntp,4) == 0) then
             costav(:)=0.0
          else
             costav(:)=costav(:)+ cost(:)
          end if

       end do   !ntp=1,c%num_temps

       !---- Re-calculate the cost function for the best configuration ----!
       call Model_Funct(vs%config,Costop)

       messag=" "
       call mess(messag)
       messag=" => Best configuration found by Simulated Annealing (Sigma of the last Montecarlo Cycles):"
       call mess(messag)
       write(unit=ipr,fmt="(/,a,a,/)") "     ",trim(messag)
       strings=" "
       write(unit=strings,fmt="(a,f16.4,a,i2)")" -> Best Solution Cost (before eventual local optimization) = ",costop,&
                                               " :: from configuration: ",jopt
       vs%best_cost=costop
       call mess(strings)
       write(unit=ipr,fmt="(/,a)") trim(strings)
       messag=" -> Best Configuration Parameters :"
       call mess(messag)
       write(unit=ipr,fmt="(a,/)") trim(messag)
       do i=1,vs%npar
         write(unit=strings,fmt="(i6,a,2F16.5)")  i,"  "//vs%nampar(i), vs%config(i),vs%sigma(i)
         write(unit=ipr,fmt="(a)") trim(strings)
         call mess(strings)
       end do

    End Subroutine SAnn_Opt_MultiConf

    !!----
    !!---- Module Subroutine SimAnneal_MultiConf(Model_Funct,Nsol,c,vs,Ipr,fileSav)
    !!----    integer,                       intent(in out)  :: Nsol
    !!----    type(SimAnn_Conditions_type),  intent(in out)  :: c
    !!----    type(MultiState_Vector_Type),  intent(in out)  :: vs
    !!----    integer,                       intent(in)      :: Ipr
    !!----    character(len=*), optional,    intent(in)      :: filesav
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
    Module Subroutine SimAnneal_MultiConf(Model_Funct,c,vs,Ipr,fileSav,fst)
       !---- Arguments ----!
       type(SimAnn_Conditions_type),  intent(in out)  :: c
       type(MultiState_Vector_Type),  intent(in out)  :: vs
       integer,                       intent(in)      :: Ipr
       character(len=*), optional,    intent(in)      :: filesav
       character(len=*), optional,    intent(in)      :: fst

       Interface
          Subroutine Model_Funct(v,cost)
             Use CFML_GlobalDeps, only: Cp
             real(kind=cp),dimension(:), intent( in):: v
             real(kind=cp),              intent(out):: cost
          End Subroutine Model_Funct

          Subroutine Write_FST(fst_file,v,cost)
             Use CFML_GlobalDeps,       only: Cp
             character(len=*),           intent(in):: fst_file
             real(kind=cp),dimension(:), intent(in):: v
             real(kind=cp),              intent(in):: cost
          End Subroutine Write_FST
       End Interface

       !--- Local Variables ---!
       character (len=256)                         :: messag, strings
       integer                                     :: i, j, k, neval, ncf, ntp, naver, jj, survive, jopt, &
                                                      minut,itick1,itick2,count_rate  !, last
       real(kind=cp)                               :: temp, ep, ener, costop, half_init_avstp, sumdel,sumsig, &
                                                      prob, rav, rati, plage, stepav, random,tini,tf,minuts,sec !, shift
       integer, parameter                          :: i_conf=99
       integer,          dimension(1)              :: seed
       logical,          dimension(np_CONF)        :: dead
       integer,          dimension(np_CONF)        :: naj
       real(kind=cp),    dimension(np_CONF)        :: cost, cost1, cost2, paj, costav
       real(kind=cp),    dimension(np_SAN,np_CONF) :: stateo     !Vector State characterizing the old configuration
       real(kind=cp),    dimension(np_SAN)         :: cv         !constant vector used in the Corana algorithm
       integer,          dimension(np_SAN,np_CONF) :: nacp       !number of accepted moves for parameter i
       real(kind=cp),    dimension(np_SAN,np_CONF) :: raver      !Vector State characterizing the average configuration
       real(kind=cp),    dimension(np_SAN,np_CONF) :: sigp       !Standard deviations of the average configuration

       call checkm(c,vs)
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

       cv(1:vs%npar)=vs%high(1:vs%npar)-vs%low(1:vs%npar)
       half_init_avstp=0.5*sum(cv(1:vs%npar)/real(vs%npar))

       if (c%nalgor == 0) then
         do j=1,vs%nconf
          vs%stp(1:vs%npar,j)=cv(1:vs%npar)
         end do
       else
         half_init_avstp=4.0*half_init_avstp
       end if

       cv(:)=2.0
       stateo=0.0
       !---- Get the initial configuration in config(1:msz) ----!
       dead(:)=.false.
       if (c%initconfig == 1) then
          do j=1,vs%nconf
            do i=1,vs%npar
               vs%state(i,j) = vs%config(i)
            end do
          end do
       else
          do j=1,vs%nconf
            do i=1,vs%npar
               if (vs%code(i)==1) then
                  call random_number(random)
                  vs%state(i,j) = vs%low(i) + random*(vs%high(i)-vs%low(i))
               else
                  vs%state(i,j)=vs%config(i)
               end if
               stateo(i,j)=vs%state(i,j)
            end do
          end do
       end if

       !---- Determine the initial values of the cost function -> cost1 ----!
       do j=1,vs%nconf
         call Model_Funct(stateo(:,j),Cost1(j))
       end do
         j=minloc(cost1(1:vs%nconf),dim=1)
         vs%config(:)=stateo(:,j)  !Best configuration for the moment
         costop=cost1(j)
         jopt=j

       messag=" ---- MultiConfiguration Simulated Annealing to minimize General Cost Functions ----"
       write(unit=ipr,fmt="(/,a,/,a,/)") messag, "     Cost-function name:  "//trim(c%Cost_function_name)
       call mess(" ")
       call mess(messag)
       call mess(" ")

       do j=1,vs%nconf
         strings=" "
         write(unit=strings,fmt="(a,i2,a,g12.5)") " => Initial configuration cost(",j,"): ",cost1(j)
         call mess(strings)
         write(unit=ipr,fmt="(a)") trim(strings)
       end do
       strings=" "
       write(unit=strings,fmt="(a)") " => Initial Best configuration state vector: "
       call mess(strings)
       write(unit=ipr,fmt="(a)") trim(strings)

       strings=" "
       do i=1,vs%npar
         write(unit=strings,fmt="(i6,a,f16.5)")  i,vs%nampar(i), vs%config(i)
         write(unit=ipr,fmt="(a)") trim(strings)
         call mess(strings)
       end do

       !---- Loop over temperatures ----!
       naver=c%nm_cycl-c%num_therm
       rav= real(naver*vs%npar)
       temp=c%t_ini/c%anneal
       neval=0
       nacp=0
       costav(:)=0.0
       !call cpu_time(tini)
       call system_clock(count_rate=count_rate)
       call system_clock(itick1)
       tini=itick1/real(count_rate)
       do ntp=1,c%num_temps     ! Global DO for changing temperature
            naj(:)   =0
           cost(:)   =0.0
           sigp(:,:) =0.0
          raver(:,:) =0.0

          temp=c%anneal*Temp  ! Current temperature

          strings=" "
          !call cpu_time(tf)   ! minutos=(tf-tini)/60  min=int(minutos)  minutos-min=
          call system_clock(itick2)
          tf=itick2/real(count_rate)
          sec=tf-tini
          minuts=sec/60.0
          minut=int(minuts)
          sec=(minuts-real(minut))*60.0
          write(unit=strings,fmt="(a,f9.5,a,i5,a,i8,a,i6,a,f8.3,a)") "  => New Temp:",temp,"  NT: ",ntp, &
               "       Number of function evaluations:",neval,"         Cumulated CPU-time: ",minut," minutes -> ",sec," seconds"
          call mess(strings)
          write(unit=ipr,fmt="(/,a)") trim(strings)

          do j=1,vs%nconf   ! loop over different configuration state vectors

             if(dead(j)) cycle  !skip is configuration is dead

             do ncf=1,c%nm_cycl
                ! last=vs%npar

                cyc_par: do i=1,vs%npar        !loop on the components of the vector state
                   if (vs%code(i) == 0) cycle cyc_par
                   !---- Set new configuration and store the previous one in config(1:msz) ----!
                   call random_number(random)
                   vs%state(i,j)=stateo(i,j)+vs%stp(i,j)*(2.0*random-1.0)
                   plage=vs%high(i)-vs%low(i)
                   if (vs%bound(i) == 0) then   !boundary conditions
                      if (vs%state(i,j) < vs%low(i) .or. vs%state(i,j) > vs%high(i))  &  !Fixed boundaries
                         call random_number(random)
                      vs%state(i,j)=vs%low(i)+ random*plage
                   else
                      if (vs%state(i,j) < vs%low(i) )  vs%state(i,j)=vs%state(i,j)+plage  !Periodic boundary conditions
                      if (vs%state(i,j) > vs%high(i) ) vs%state(i,j)=vs%state(i,j)-plage
                   end if
                 !  shift=vs%state(i,j)-stateo(i,j)

                   !---- Calculate the cost function ----!
                   call Model_Funct(vs%state(:,j),Cost2(j))
                   neval=neval+1
                   ener= cost2(j)-cost1(j)

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
                         vs%state(i,j)=stateo(i,j)
                         cycle cyc_par   !cycle and try another configuration
                      end if
                   end if

                   !---- Accepted configuration ----!
                   cost1(j)=cost2(j)               !update the cost function
                   stateo(i,j)=vs%state(i,j)       !update configuration
                   if (cost1(j) < costop) then     !the best current configuration is found
                      costop=cost1(j)
                      vs%config(:)=stateo(:,j)
                      jopt=j
                      if(present(fst)) then
                        call Write_FST(fst,vs%config(:),costop)
                      end if
                   end if

                   if (ncf <= c%Num_therm) cycle cyc_par
                   nacp(i,j)=nacp(i,j)+1
                   naj(j)=naj(j)+1   !number of accepted jumps for state vector j
                   cost(j)=cost(j)+cost2(j)

                end do cyc_par  !end loop over components of state vector


                do k=1,vs%npar
                   sigp(k,j)= sigp(k,j)+stateo(k,j)*stateo(k,j)
                  raver(k,j)=raver(k,j)+stateo(k,j)
                end do

             end do    !End loop over Montecarlo moves at fixed temperature

          end do   !end loop over configurations

          vs%cost(1:vs%nconf)=cost1(1:vs%nconf)
          if(modulo(ntp,4) == 0) then
            costav=costav/3.0
          end if

          !---- Statistic  and average values for previous temperature ----!
          do j=1,vs%nconf
             if(dead(j)) cycle
             if (naj(j) == 0) then
                naj(j)=1
                cost(j)=cost1(j)
             end if
             paj(j)=100.0*real(naj(j))/rav

             cost(j)=cost(j)/naj(j)

             do i=1,vs%npar
                raver(i,j)=raver(i,j)/naver
                sigp(i,j)=sqrt(abs(sigp(i,j)/naver-raver(i,j)*raver(i,j)))
             end do
             stepav=sum(vs%stp(:,j))/real(vs%npar)

             !---- Writing partial results ----!
             strings=" "
             write(unit=strings,fmt="(a,i4,a,f8.3,a,i2,a,f8.3,2(a,g14.7))")  &
             "     Conf:",j,"  (%Acc):",paj(j),"  <Step(",j,")>:",stepav, &
             "  <"//trim(c%Cost_function_name)//">:",cost(j),"  -> Current Cost:", cost1(j)
             call mess(strings)
             write(unit=ipr,fmt="(a)") trim(strings)
             write(unit=ipr,fmt="(a,i10 )")  "     Num-Cost-Evaluations: ",neval

            !Apply test of convergence and suppress bad configurations

            if (paj(j) <= c%accept ) then
                 dead(j)=.true. !Convergence criterium
                 write(unit=strings,fmt="(a,i3,a)") " => Configuration #",j," converged => dead in the algorithm!"
                 call mess(strings)
                 write(unit=ipr,fmt="(a)") trim(strings)
            end if

            if(abs(costav(j)-cost(j)) < 0.20 .and. modulo(ntp,4) == 0 .and. stepav < half_init_avstp ) then
                 dead(j)=.true. !Convergence criterium
                 write(unit=strings,fmt="(a,i3,a)") " => Configuration #",j," do not change anymore => dead in the algorithm!"
                 call mess(strings)
                 write(unit=ipr,fmt="(a)") trim(strings)
            end if

            do k=j+1,vs%nconf
              if(dead(k)) cycle
            !  sumdel = sum(abs(vs%state(:,j)-vs%state(:,k)))
              sumdel = sum(abs(raver(:,j)- raver(:,k)))
              sumsig= min(0.2, 0.5*sum(sigp(:,j)+sigp(:,k)))
              if( sumdel < sumsig )  then
                 dead(k) = .true.
                 write(unit=strings,fmt="(2(a,i3))") " => Configuration #",k," dead, because it is equal to Configuration #",j
                 call mess(strings)
                 write(unit=ipr,fmt="(a)") trim(strings)
              end if
            end do

          end do !j=1,vs%nconf

          if (present(filesav)) then
             open(unit=i_conf,file=trim(filesav)//".anl",status="old",action="write", position="append")
             write(unit=i_conf,fmt="(i4,31f10.4)")  ntp,temp,costop,vs%config(1:vs%npar)
             call flush(i_conf)
             close(unit=i_conf)
          end if


          survive=0
          do j=1,vs%nconf
           if(dead(j)) cycle
            survive=survive+1
          end do
          if(survive == 0) then
            strings = " => Convergence reached, look the list of configurations"
            call mess(strings)
            write(unit=ipr,fmt="(a)") trim(strings)
            exit
          end if

          !---- Adjust STP so that approximately half of all evaluations are accepted ----!
          !---- (Corana's algorithm)

          if (c%nalgor == 0 .or. c%nalgor == 1) then
             do j=1,vs%nconf
                if(dead(j)) cycle
                do i=1,vs%npar
                   jj=0
                   if (vs%stp(i,j) > abs(0.01*c%accept*raver(i,j))) exit
              !     if (vs%stp(i,j) > c%accept ) exit
                   jj=1
                end do
                if (jj == 1) exit
                do i = 1, vs%npar
                   plage=vs%high(i)-vs%low(i)
                   rati = real(nacp(i,j)) /real(naver)
                   if (rati > 0.6) then
                      vs%stp(i,j) = vs%stp(i,j)*(1.0 + cv(i)*(rati - 0.6)/0.4)
                   else if (rati < 0.4) then
                      vs%stp(i,j) = vs%stp(i,j)/(1.0 + cv(i)*((0.4 - rati)/0.4))
                   end if
                   if (vs%stp(i,j) > plage) then
                      vs%stp(i,j) = plage
                   end if
                end do
             end do !j=1,vs%nconf
          end if
          nacp(:,:) = 0
          if(modulo(ntp,4) == 0) then
             costav(:)=0.0
          else
             costav(:)=costav(:)+ cost(:)
          end if

       end do   !ntp=1,c%num_temps

       !---- Re-calculate the cost function for the best configuration ----!
       call Model_Funct(vs%config,Costop)
       vs%best_cost=costop

       messag=" "
       call mess(messag)
       messag=" => Best configuration found by Simulated Annealing (Sigma of the last Montecarlo Cycles):"
       call mess(messag)
       write(unit=ipr,fmt="(/,a,a,/)") "     ",trim(messag)
       strings=" "
       write(unit=strings,fmt="(a,f16.4,a)")" -> Best Solution Cost (before eventual local optimization)= ",costop," :: "
       call mess(strings)
       write(unit=ipr,fmt="(/,a)") trim(strings)
       messag=" -> Configuration parameters :"
       call mess(messag)
       write(unit=ipr,fmt="(a,/)") trim(messag)
       do i=1,vs%npar
         vs%sigma(i)=sigp(i,jopt)
         write(unit=strings,fmt="(i6,a,2F16.5)")  i,"  "//vs%nampar(i), vs%config(i),sigp(i,jopt)
         write(unit=ipr,fmt="(a)") trim(strings)
         call mess(strings)
       end do

    End Subroutine SimAnneal_MultiConf

 End Submodule SAnn_MultiConf