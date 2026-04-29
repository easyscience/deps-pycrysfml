Module GLS_glopsann
   use CFML_GlobalDeps,                only: cp
   use CFML_Metrics,                   only: Cell_G_Type
   use CFML_Strings,                   only: u_case, file_list_type
   use CFML_Simulated_Annealing,       only: State_Vector_Type
   implicit none

  contains

    Subroutine expand_vary_fix(within,Cell,fcfl,fvarfix)
      real(kind=cp),           intent(in)  :: within
      type(Cell_G_Type),       intent(in)  :: Cell
      type(file_list_type),    intent(in)  :: fcfl
      type(file_list_type),    intent(out) :: fvarfix
      !---- local variables ----!
      integer, parameter :: nfix=40
      integer :: i,j,jj,k
      character(len=256)           :: line
      character(len=6)             :: label, scatt
      real(kind=cp), dimension(3)  :: pos,low,high,step


      fvarfix%nlines =4*fcfl%nlines
      allocate(fvarfix%line(fvarfix%nlines))
      j=0
      do i=1,fcfl%nlines
        j=j+1
        fvarfix%line(j)=fcfl%line(i)
      end do
      do k=1,3
        step(k)= 0.5_cp*within/cell%cell(k)
      end do

      !Calculate the limits for the new generated VARY lines
      do i=1,fcfl%nlines
        line=adjustl(fcfl%line(i))
        if(u_case(line(1:4)) == "ATOM") then
          !Eliminate standard deviations
          do jj=1,len_trim(line)
             if(line(jj:jj) == "(") then
               do k=jj+1,jj+5
                   if(line(k:k) == ")") then
                      line(jj:k)= " "
                   end if
               end do
             end if
          end do
          read(unit=line(5:),fmt=*) label,scatt,pos
          do k=1,3
            low(k)=pos(k)-step(k)
            high(k)=pos(k)+step(k)
          end do
          j=j+1
          write(unit=fvarfix%line(j), fmt="(a,2f10.4,2i4)")  "VARY "//" x_"//trim(label), low(1),high(1),0,0
          j=j+1
          write(unit=fvarfix%line(j), fmt="(a,2f10.4,2i4)")  "VARY "//" y_"//trim(label), low(2),high(2),0,0
          j=j+1
          write(unit=fvarfix%line(j), fmt="(a,2f10.4,2i4)")  "VARY "//" z_"//trim(label), low(3),high(3),0,0
        end if
      end do

      do i=1,fcfl%nlines
        line=adjustl(fcfl%line(i))
        if(u_case(line(1:3)) == "FIX" .or. u_case(line(1:5)) == "EQUAL") then
          j=j+1
          fvarfix%line(j)=trim(line)
        end if
      end do
      fvarfix%nlines=j

    End Subroutine expand_vary_fix

    Subroutine Gen_random_initial_state(vs)
      type(State_Vector_Type), intent(in out) :: vs
      integer       :: i
      real(kind=cp) :: random

      do i=1,vs%npar
        call random_number(random)
        vs%config(i) = vs%low(i) + random*(vs%high(i)-vs%low(i))
        !write(*,"(a,2f9.5,a)") " Random number & value: ",random, vs%config(i),"   "//trim(vs%nampar(i))
      end do
    End Subroutine Gen_random_initial_state
End Module GLS_glopsann


!!---- GLOpSAnn : Global optimization by Simulated Annealing of crystal structures
!!---- This program was an example of the use of CrysFML library. Currently it is
!!---- distributed as an executable within the FullProf Suite since February 2015.
!!----  Author: Juan Rodriguez-Carvajal
!!----  Created in October 2008, updated in Febraury 2015
!!----
Program Global_Optimization_Xtal_structures
   !---- Use Modules ----!
   use CFML_Rational,                  only: assignment (=)
   use CFML_GlobalDeps,                only: Err_CFML
   use CFML_gSpaceGroups,              only: SPG_Type, Write_SpaceGroup_info
   use CFML_strings,                   only: u_case,file_list_type, File_To_FileList
   use CFML_Atoms,                     only: AtList_Type, Write_Atom_List !, Copy_Atom_List
   use CFML_Metrics,                   only: Cell_G_Type, Write_Crystal_Cell
   use CFML_Reflections,               only: RefList_Type, SRefl_Type

   use CFML_IOForm,                    only: Read_Xtal_Structure, Write_CFL_File, Write_Cif_Template
   use CFML_EnBVS,                     only: calc_BVS
   use CFML_Structure_Factors,         only: Structure_Factors, Write_Structure_Factors, &
                                             Init_Structure_Factors
   use CFML_Keywords_Code_Parser,      only: NP_Max,NP_Refi,V_BCon,V_Bounds,V_Name,V_Vec,V_Vec_std,&
                                             Allocate_VParam,Init_RefCodes, Read_RefCodes_File, &
                                             Write_Info_RefCodes, &
                                             allocate_restparam, Write_restraints_ObsCalc,VState_to_AtomsPar
   use CFML_Simulated_Annealing,       only: SimAnn_Conditions_type, state_Vector_Type, Multistate_Vector_Type, &
                                             Simanneal_Gen,Set_SimAnn_Cond, &
                                             Set_SimAnn_StateV,Write_SimAnn_Cond, Write_SimAnn_StateV, &
                                             Write_SimAnn_MStateV, SimAnneal_MultiConf,Set_SimAnn_MStateV, &
                                             Sann_Opt_MultiConf, Local_Optim
   use GLS_observ,                     only: Read_observations,Observation_Type,Observation_List_Type, &
                                             Write_ObsCalc_SFactors,err_observ,err_mess_observ, SumGobs,&
                                             wavel_int, Write_FoFc_Powder, Nselr, Read_Profile_Type,    &
                                             Weight_Sim,iwgt
   use GLS_cost_functions,             only: Cell,A,A_Clone,Ac,SpG,hkl,Oh,Icost,wcost,Err_cost,Err_Mess_cost, &
                                             General_Cost_function, readn_set_costfunctpars, write_costfunctpars, &
                                             Write_FinalCost,wavel,diff_mode,anti_bump, Write_PRF, Mat, trr, irad
   use GLS_glopsann

   implicit none

   type(file_list_type)                   :: fich_cfl,fich_rest,fich_varfix
   type(SimAnn_Conditions_type),save      :: c
   type(state_Vector_Type)                :: vs
   real(kind=cp),dimension(:),allocatable :: v_av,v_sig
   type (Multistate_Vector_Type)          :: mvs
   type(state_Vector_Type),dimension(:),allocatable :: vsp

   character(len=256)                 :: filcod     !Name of the input file
   character(len=256)                 :: filhkl     !Name of the hkl-file
   character(len=256)                 :: filprof    !Name of the profile-file
   character(len=256)                 :: filrest    !Name of restraints file
   character(len=256)                 :: line       !Text line
   character(len=256)                 :: fst_cmd    !Commands for FP_Studio
   character(len=256)                 :: cmdline    !Command line
   character(len=80)                  :: title      !
   character(len=140),dimension(200)  :: info_lines=" "     ! Information lines to be output in solution files
   integer                            :: lun=1,ifst=2, ier,i,j,k, n, i_cfl,ninfo=0,i_best, nlong
   integer, dimension(:),allocatable  :: i_bvs
   real(kind=cp)                      :: start,fin, mindspc, maxsintl, within, costop, costmax, thr, tims,timm
   integer                            :: narg, num_p, d, itick1,count_rate, itick2
   Logical                            :: esta, arggiven=.false., ref_within=.false.,&
                                         fst_out=.false., local_opt=.false., rest_file=.false., local_ref=.false., wait_end=.true.

    !---- Arguments on the command line ----!
    !narg=iargc()
    narg=COMMAND_ARGUMENT_COUNT()

    if(narg > 0) then
      call Get_Command(Command=cmdline,Length=nlong)
      call GET_COMMAND_ARGUMENT(1,filcod)
      arggiven=.true.
      i=index(filcod,".cfl")
      j=index(Cmdline,"-f_stop")
      if(i /= 0) filcod=filcod(1:i-1)
      if(j /= 0) wait_end=.false.
    end if
    call system_clock(count_rate=count_rate)

    write (unit=*,fmt="(/,/,7(a,/))")                                                  &
     "                                       G L O p S A n n"                      , &
     "                   ------ Global Optimization by Simulated Annealing ------" , &
     "                           ------  of Crystal Structures  ------"            , &
     "                             ---- Version 1.2 April-2023 ----"                               , &
     "    ****************************************************************************************"  , &
     "    * Optimizes X-tal structures against combined cost functions described in a *.CFL file *"  , &
     "    ****************************************************************************************"  , &
     "             ( JRC/NAK - ILL - Created in December-2008, last update in April-2023 )"
   write (unit=*,fmt=*) " "

   if(.not. arggiven) then
     write(unit=*,fmt="(a)",advance='no') " => Code of the file xx.cfl (give xx): "
     read(unit=*,fmt="(a)") filcod
     if(len_trim(filcod) == 0) stop
     i=index(filcod,".cfl")
     if(i /= 0) filcod=filcod(1:i-1)
   end if

   open(unit=lun,file=trim(filcod)//".out", status="replace",action="write")
    write(unit=lun,fmt="(/,/,6(a,/))")                                               &
     "                                       G L O p S A n n"                      , &
     "                   ------ Global Optimization by Simulated Annealing ------" , &
     "                           ------  of Crystal Structures  ------"            , &
     "                             ---- Version 1.2 April-2023 ----"                                 , &
     "    ****************************************************************************************"  , &
     "    * Optimizes X-tal structures against combined cost functions described in a *.CFL file *"  , &
     "    ****************************************************************************************"  , &
     "             ( JRC/NAK - ILL - Created in December-2008, last update in April-2023 )"

   inquire(file=trim(filcod)//".cfl",exist=esta)
   if( .not. esta) then
     write(unit=*,fmt="(a)") " File: "//trim(filcod)//".cif (or .cfl) does'nt exist!"
     call Close_GLOpSAnn()
   end if

    write(unit=*,fmt="(a)")   " => Input  control file: "//trim(filcod)//".cfl"
    write(unit=*,fmt="(a)")   " => Results output file: "//trim(filcod)//".out"
    write(unit=lun,fmt="(a)") " => Input  control file: "//trim(filcod)//".cfl"
    write(unit=lun,fmt="(a)") " => Results output file: "//trim(filcod)//".out"

   call Read_Xtal_Structure(trim(filcod)//".cfl", Cell, Spg, A,"Atm_Ref_type", FileList=fich_cfl)
   If(Err_CFML%Ierr /= 0) then
     write(unit=*,fmt="(a)") trim(Err_CFML%Msg)
   else
     call Write_Crystal_Cell(Cell,lun)
     call Write_SpaceGroup_Info(SpG,lun)
     call Write_Atom_List(A,Iunit=lun)

     !Convert to integer/real the operators of the group for calculating distances
     if(allocated(Mat)) deallocate(Mat)
     if(allocated(trr)) deallocate(trr)
     allocate(Mat(3,3,SpG%Multip), trr(3,SpG%Multip))
     d=SpG%d
     do i=1,SpG%Multip
       Mat(:,:,i)= SpG%Op(i)%Mat(1:3,1:3)
       trr(:,i)  = SpG%Op(i)%Mat(1:3,d)
     End do

     np_max= A%natoms*11  ! x,y,z,biso,occ,betas
     call allocate_vparam(np_max)

    !Read what cost functions will be minimized
     Call Readn_Set_CostFunctPars(fich_cfl)
     if(Err_cost) then
       write(unit=*,fmt="(a)") trim(Err_Mess_cost)
       call Close_GLOpSAnn()
     end if
     Call Write_CostFunctPars(lun)

     !Look for information lines in the CFL (normally at the end of the instructions)
     !and especial instructions like REF_WINTHIN
     do i=1,fich_cfl%nlines
       line=adjustl(u_case(fich_cfl%line(i)))
       if(line(1:10) == "REF_WITHIN") then
         read(unit=line(12:),fmt=*,iostat=ier) within
         if(ier == 0) then
           ref_within=.true.
         end if
       end if
       if(line(1:10) == "INFO_LINES") then
         ninfo=1
         info_lines(ninfo)= adjustl(fich_cfl%line(i))
         do
           ninfo=ninfo+1
           if(ninfo > 200) exit
           j=i+ninfo-1
           if(j > fich_cfl%nlines) then
             ninfo=ninfo-1
             exit
           end if
           line=adjustl(fich_cfl%line(j))
           info_lines(ninfo)=line
           !Put also here ref_within in case other programs will use the information lines
           if(u_case(line(1:10)) == "REF_WITHIN") then
             read(unit=line(12:),fmt=*,iostat=ier) within
             if(ier == 0) then
               ref_within=.true.
             end if
           end if
           if(u_case(line(1:14)) == "END_INFO_LINES") exit
         end do
       end if
     end do

     if(ref_within) write(unit=lun,fmt="(/,a,f8.4,a/)") " => A Refinement within ",within, &
                                         " angstroms will be performed by expanding VARY directives"

     !Allocate objects for restraints
     if(icost(2) == 1 .or. icost(3) == 1 .or. icost(4) == 1) then
       !Look first for restraints file
          do i=1,fich_cfl%nlines
            line=adjustl(u_case(fich_cfl%line(i)))
            if(line(1:10) == "RESTR_FILE") then
              j=index(line,"!")
              if(j /= 0) line=trim(line(1:j-1))
              filrest= adjustl(line(12:))
              inquire(file=trim(filrest),exist=esta)
              fich_rest= File_To_FileList(filrest)
              If(Err_CFML%Ierr /= 0) then
                 write(unit=*,fmt="(a)") trim(Err_CFML%Msg)
                 call Close_GLOpSAnn()
              else
                 rest_file=.true.
              end if
              exit
            end if
          end do
        if(rest_file) then
           call allocate_restparam(fich_rest)
        else
           call allocate_restparam(fich_cfl)
        end if
     end if
     if(icost(5) == 1) then !Look for restrictions in the calculation of Bond-Valence
        allocate(i_bvs(A%natoms))
        i_bvs=0
        do i=1,fich_cfl%nlines
          line=adjustl(u_case(fich_cfl%line(i)))
          if(line(1:9) == "BVS_RESTR") then
            j=index(line,"!")
            if(j /= 0) line=trim(line(1:j-1))
             read(unit=line(10:),fmt=*,iostat=ier) i_bvs(1:A%natoms)
             if(ier /= 0) exit
             A%Atom(:)%VarF(5) = real(i_bvs)
             exit
          end if
        end do
     end if

     !Determine the refinement codes from vary/fix instructions
     call Init_RefCodes(A)
     if(ref_within) then
        call expand_vary_fix(within,Cell,fich_cfl,fich_varfix)
        write(unit=lun, fmt="(/,a)") "  GENERATED VARY INSTRUCTIONS FROM REF_WITHIN OPTION"
        write(unit=lun, fmt="(a,/)") "  =================================================="
        do i=1,fich_varfix%nlines
          if(index(fich_varfix%line(i),"VARY") == 0) cycle
          write(unit=lun, fmt="(a)") "      "//trim(fich_varfix%line(i))
        end do
        call Read_RefCodes_File(fich_varfix,1,fich_varfix%nlines,A,Spg)
     else
        call Read_RefCodes_File(fich_cfl,1,fich_cfl%nlines,A,Spg)
     end if
     if(Err_CFML%Ierr /= 0) then
       write(unit=*,fmt="(a)") trim(Err_CFML%Msg)
     end if
     if(rest_file) then
        call Read_RefCodes_File(fich_rest,1,fich_rest%nlines,A,Spg)
        if(Err_CFML%Ierr /= 0) then
          write(unit=*,fmt="(a)") trim(Err_CFML%Msg)
        end if
     end if

     if(anti_bump%nrel > 0)then
          write(unit=lun, fmt="(/,a,i5)") " =>   Anti-Bump relations: ",anti_bump%nrel
          write(unit=lun, fmt="(a)")      " => Repulsion of the form: (dmin/d)**power "
          write(unit=lun, fmt="(a)") " "
          write(unit=lun, fmt="(a)") "   N.A-Bump   Minimal-Distance   Power    Species1  Species2"
          write(unit=lun, fmt="(a)") " ==========================================================="
          do i=1,anti_bump%nrel
            write(unit=lun, fmt="(i7,tr8,f8.4,i11,tr9,a,tr9,a)")  i, anti_bump%damin(i), anti_bump%power(i),&
                                                                     anti_bump%sp1(i),anti_bump%sp2(i)
          end do
     end if

     call Write_Info_RefCodes(A,Spg,lun)

     !------------------------------  Testing refinement codes
     !  STOP
     !------------------------------

     !--- Look for FP_Studio commands
     fst_cmd=" "
     do i=1,fich_cfl%nlines
       line=adjustl(u_case(fich_cfl%line(i)))
       if(line(1:7) == "FST_CMD") then
         j=index(line,"!")
         if(j /= 0) then
           fst_cmd= adjustl(fich_cfl%line(i)(9:j-1))
         else
           fst_cmd= adjustl(fich_cfl%line(i)(9:))
         end if
           fst_out=.true.
         exit
       end if
     end do

     !--- Look for Local_Opt commands
     do i=1,fich_cfl%nlines
       line=adjustl(u_case(fich_cfl%line(i)))
       if(line(1:9) == "LOCAL_OPT") then
         Local_Opt=.true.
         exit
       end if
       if(line(1:9) == "LOCAL_REF") then
         read(unit=line(10:),fmt=*,iostat=ier) num_p   !number of starting points for local optimization
         if(ier /= 0) num_p=1
         allocate(vsp(num_p))
         Local_Ref=.true.
         exit
       end if
     end do


     if(Icost(1) == 1 .or. Icost(7) == 1 .or. Icost(10) == 1 .or. Icost(11) == 1) then
          ! Reading observed structure factors squared and construct hkl if Icost(1) =1
          ! First detect the name of the hkl file
          esta=.false.
          do i=1,fich_cfl%nlines
            line=adjustl(u_case(fich_cfl%line(i)))
            if(line(1:7) == "HKL-OBS") then
              line=fich_cfl%line(i)
              j=index(line,"!")
              if(j /= 0) line=trim(line(1:j-1))
              filhkl= adjustl(line(9:))
              inquire(file=trim(filhkl),exist=esta)
              exit
            end if
          end do

          if(.not. esta) then
            write(unit=*,fmt="(a)") " => No hkl-file (or wrong name!) has been provided! "
            Icost(1)=0; wcost(1)=0.0
            Icost(7)=0; wcost(7)=0.0
            Icost(10:11)=0; wcost(10:11)=0.0
          end if

          mindspc=0.0       !Read minimun d-spacing to be considered in the list
          Nselr=0
          do i=1,fich_cfl%nlines
            line=u_case(fich_cfl%line(i))
            if(line(1:12) == "MIN-DSPACING") then
              read(unit=line(14:),fmt=*,iostat=ier) mindspc
              if(ier /= 0) mindspc=0.0
              exit
            end if
          end do

          if(Icost(7) == 1) then  !for clusters
             !call Read_observations_clusters(trim(filhkl),Cell,SpG,.true.,Oh,hkl)
             call read_observations(trim(filhkl),Cell,SpG,.true.,Oh,hkl)
          else  !Structure factors, single crystals or profile
             call read_observations(trim(filhkl),Cell,SpG,.true.,hkl)
          end if
          if(err_observ) then
             write(unit=*,fmt="(a)") " => Error reading observations"
             write(unit=*,fmt="(a)") trim(err_mess_observ)
            stop
          end if

          if(Icost(7) == 1 .or. Icost(10) == 1 .or. Icost(11) == 1) then
            wavel=wavel_int
          end if

        ! Change the number of reflections to be considered according to the
        ! resolution (mindspc) read above. Change also SumGobs
         if( mindspc > 0.001) then
            n=0
            maxsintl=0.5/mindspc
            maxsintl=maxsintl*1.01

            do i=1,hkl%Nref
              n=n+1
              if(hkl%Ref(i)%s > maxsintl) then
                hkl%Nref=n
                Nselr=n
                exit
              end if
            end do

            if(Icost(7) == 1) then
              do i=1,Oh%Nobs
                  j=Oh%Ob(i)%ncont
                  k=Oh%Ob(i)%p(j)
                  if(k > n) then
                    Oh%Nobs=i-1
                    exit
                  end if
              end do
            end if

            SumGobs=0.0
            if(Icost(7) == 1) then
              do i=1,Oh%nobs
                SumGobs=SumGobs+Oh%Ob(i)%Gobs
              end do
            else
              Select Type(R => hkl%Ref)
                Type is (SRefl_Type)
                   do i=1,n
                     SumGobs=SumGobs+abs(R(i)%Fo(irad))
                   end do
              End Select
            end if

            write(unit= * ,fmt="(/,a,i5,/,a,f8.4)") " => Number of reflections to consider: ",n,&
                                                    "    Resolution: ",mindspc
            write(unit=lun,fmt="(/,a,i5,/,a,f8.4)") " => Number of reflections to consider: ",n,&
                                                    "    Resolution: ",mindspc
            if(Icost(7) == 1) then
              write(unit= * ,fmt="(a,i5)") " => Number of Observations to consider: ",Oh%nobs
              write(unit=lun,fmt="(a,i5)") " => Number of Observations to consider: ",Oh%nobs
            end if
         end if

         if(Icost(10) == 1 .or. Icost(11) == 1) then !Reading the profile
              esta=.false.
              do i=1,fich_cfl%nlines
                line=adjustl(u_case(fich_cfl%line(i)))
                if(line(1:11) == "PROFILE-OBS") then
                  line=fich_cfl%line(i)
                  j=index(line,"!")
                  if(j /= 0) line=trim(line(1:j-1))
                  filprof= adjustl(line(12:))
                  inquire(file=trim(filprof),exist=esta)
                  exit
                end if
                if(line(1:6) == "WEIGHT") then
                  Weight_Sim=.true.
                  read(unit=line(7:),fmt=*,iostat=ier) iwgt
                  if(ier /= 0) iwgt=0
                end if
              end do
              if(.not. esta) then
                write(unit=*,fmt="(a)") " => No profile-file (or wrong name!) has been provided! "
                Icost(10:11)=0; wcost(10:11)=0.0
              end if
              if(Icost(10) == 1 .or. Icost(11) == 1) then
                 call Read_Profile_Type(filprof)
              end if
              if(err_observ) then
                 write(unit=*,fmt="(a)") " => Error reading powder profile file"
                 write(unit=*,fmt="(a)")  trim(err_mess_observ)
                 call Close_GLOpSAnn()
              end if

         end if

        !Set up Structure factor tables and initial values
         call Init_Structure_Factors(hkl,A,Spg,mode=diff_mode,lambda=wavel,lun=lun)
         call Structure_Factors(hkl,A,SpG,mode=diff_mode,lambda=wavel)
          if(Err_CFML%Ierr /= 0) then
             write(unit=*,fmt="(a)") " => Error in Structure factor calculations"
             write(unit=*,fmt="(a)") trim(Err_CFML%Msg)
             call Close_GLOpSAnn()
          end if
     End if !Icost(1) == 1 .or. Icost(7) == 1 .or. Icost(10) == 1 .or. Icost(11) == 1

     call cpu_time(start)
     call system_clock(itick1)
     n=NP_Refi
     if(local_ref) then
       allocate(v_av(NP_Refi),v_sig(NP_Refi))
       v_av=0.0_cp; v_sig=0.0_cp
       write(unit=lun,fmt="(/,a)")      "============================================================================"
       write(unit=lun,fmt="(a,i5,a)")   "=  Local Refinement using UNIRANDI for ",num_p," initial random configurations ="
       write(unit=lun,fmt="(a,/)")      "============================================================================"

       call random_seed()
       !do i=1,n
       !   write(*,"(2f8.4,a,f9.5)") V_Bounds(1:2,i),"   "//V_Name(i), V_Vec(i)
       !end do
       do i=1,num_p
          call Set_SimAnn_StateV(NP_Refi,V_BCon,V_Bounds,V_Name,V_Vec,vsp(i))
          call  Gen_random_initial_state(vsp(i))
          !call Write_SimAnn_StateV(lun,vsp(i),"STARTING STATE")
          !Call directly to local optimization
          vsp(i)%state=vsp(i)%config
          !call General_Cost_function(vsp(i)%config, vsp(i)%cost)
          !write(*,"(a,5f14.5,a,f14.5)") " Config: ", vsp(i)%config(1:5), " Cost: ", vsp(i)%cost
          call Local_Optim(General_Cost_function,n,vsp(i)%config,vsp(i)%cost,vsp(i)%low,vsp(i)%high,vsp(i)%bound)
          write(unit=*,fmt="(a,i6,a,f14.4)") " Cost value resulting from refinement of configuration #",i,": ",vsp(i)%cost
          !do j=1,n
          !  write(*,"(2(a,f10.5),a)") "  Delta: ",vsp(i)%state(j)-vsp(i)%config(j),"  Value: ",vsp(i)%config(j),"   "//trim(vsp(i)%nampar(j))
          !end do
          write(unit=lun,fmt="(a,i6,a,f14.4)") " Cost value resulting from refinement of configuration #",i,": ",vsp(i)%cost
       end do
       costop=9.9e30; costmax=0.0_cp
       do i=1,num_p
         if(vsp(i)%cost < costop) then
           costop = vsp(i)%cost
           i_best=i
         end if
         if(vsp(i)%cost > costmax)  costmax = vsp(i)%cost
       end do
       thr=0.25*(costmax-costop)+costop
       k=0
       do i=1,num_p
         if(vsp(i)%cost <= thr) then
           k=k+1
           v_av=v_av+vsp(i)%config
         end if
       end do
       v_av=v_av/real(max(1,k))
       do i=1,num_p
         if(vsp(i)%cost <= thr) then
          v_sig=v_sig+(vsp(i)%config-v_av)**2
         end if
       end do
       v_sig=sqrt(v_sig)/real(max(1,k))

       write(*,*) "  i_best: ",i_best
       write(unit=*,fmt="(a,f12.2)") "  Configuration found by Local_Refinement,  cost=",costop
        V_vec(1:n)=vsp(i_best)%config(1:n)
        V_vec_std(1:n)=v_sig(1:n)
        call VState_to_AtomsPar(A,mode="V") !This is the refined configuration

        !Write a CFL file

        write(unit=line,fmt="(a,f12.2)") "  Configuration found by Local_Refinement,  cost=",costop
        open(newunit=i_cfl,file=trim(filcod)//"_ref.cfl",status="replace",action="write")
        if(ninfo > 0) then
           call Write_CFL_File(i_cfl,Cell,SpG,A,line,info_lines)
        else
           call Write_CFL_File(i_cfl,Cell,SpG,A_Clone,line,info_lines)
        end if
        call flush(i_cfl)
        close(unit=i_cfl)
        write(unit=*,fmt="(a)") "  The file "//trim(filcod)//"_ref.cfl"//"  has been written!"

     else

        !Set up the simulated annealing conditions by reading the SimAnn_Conditions_type: c
         call Set_SimAnn_Cond(fich_cfl,c)
          if(err_CFML%Ierr /=0) then
             write(unit=*,fmt="(a)") " => Error setting Simulated Annealing conditions"
             write(unit=*,fmt="(a)") trim(err_CFML%Msg)
             call Close_GLOpSAnn()
          end if
         call Write_SimAnn_Cond(lun,c)

         if(c%num_conf == 1) then
            !Set up the Simulated Annealing State Vector
             call Set_SimAnn_StateV(NP_Refi,V_BCon,V_Bounds,V_Name,V_Vec,vs)
              if(err_CFML%Ierr /=0) then
                 write(unit=*,fmt="(a)") " => Error setting Simulated Annealing State Vector"
                 write(unit=*,fmt="(a)") trim(err_CFML%Msg)
                 call Close_GLOpSAnn()
              end if
             call Write_SimAnn_StateV(lun,vs,"INITIAL STATE")
             if(fst_out) then
               call Simanneal_Gen(General_Cost_function,c,vs,lun,fst=trim(filcod)//".fst  "//trim(fst_cmd))
             else
               call Simanneal_Gen(General_Cost_function,c,vs,lun)
             end if
             call Write_SimAnn_StateV(lun,vs,"FINAL STATE")
             V_vec(1:n)=vs%config(1:n)
             V_vec_std(1:n)=0.0 !vs%stp(1:n)
             call VState_to_AtomsPar(A,mode="V") !This is the best configuration

             !Write a CFL file
             !call Copy_Atom_List(A, A_clone)
             A_clone=A

             write(unit=line,fmt="(a,f12.2)") "  Best Configuration found by Simanneal_Gen,  cost=",vs%cost
             open(newunit=i_cfl,file=trim(filcod)//"_sol.cfl",status="replace",action="write")
             if(ninfo > 0) then
               call Write_CFL_File(i_cfl,Cell,SpG,A_Clone,line,info_lines)
             else
               call Write_CFL_File(i_cfl,Cell,SpG,A_Clone,line)
             end if
             call flush(i_cfl)
             close(unit=i_cfl)

         else   ! Multi-configuration Simulated Annealing

             call Set_SimAnn_MStateV(NP_Refi,c%num_conf,V_BCon,V_Bounds,V_Name,V_Vec,mvs)
              if(err_CFML%Ierr /= 0) then
                 write(unit=*,fmt="(a)") " => Error setting Simulated Annealing State Vector"
                 write(unit=*,fmt="(a)") trim(err_CFML%Msg)
                 call Close_GLOpSAnn()
              end if
             call Write_SimAnn_MStateV(lun,mvs,"INITIAL STATE")

             if (Local_Opt) then
               if(fst_out) then
                  call SAnn_Opt_MultiConf(General_Cost_function,c,mvs,lun,fst=trim(filcod)//".fst  "//trim(fst_cmd))
               else
                  call SAnn_Opt_MultiConf(General_Cost_function,c,mvs,lun)
               end if
             else
               if(fst_out) then
                  call SimAnneal_MultiConf(General_Cost_function,c,mvs,lun,fst=trim(filcod)//".fst  "//trim(fst_cmd))
               else
                  call SimAnneal_MultiConf(General_Cost_function,c,mvs,lun)
               end if
             end if

             call Write_SimAnn_MStateV(lun,mvs,"FINAL STATE",1)

             !Writing the Best configuration in a separated CFL file
             n=mvs%npar
             V_vec(1:n)=mvs%config(1:n)
             V_vec_std(1:n)=mvs%sigma(1:n)
             call VState_to_AtomsPar(A,mode="V") !This is the best configuration

             line=" "
             write(unit=line,fmt="(a)") trim(filcod)//"_best.cfl"
             open(newunit=i_cfl,file=trim(line),status="replace",action="write")
             write(unit=line,fmt="(a,f12.2)") "  Best Configuration found by SimAnneal_MultiConf,  cost=",mvs%best_cost
             if(ninfo > 0) then
               call Write_CFL_File(i_cfl,Cell,SpG,A,line,info_lines)
             else
               call Write_CFL_File(i_cfl,Cell,SpG,A,line)
             end if
             call flush(i_cfl)
             close(unit=i_cfl)


             !Write CFL files of each solution
             !call Copy_Atom_List(A, A_clone)
             A_clone=A
             do i=1,c%num_conf
               line=" "
               write(unit=line,fmt="(a,i2.2,a)") trim(filcod)//"_sol",i,".cfl"
               V_vec(1:n)=mvs%state(1:n,i)
               call VState_to_AtomsPar(A_clone,mode="V")

               open(newunit=i_cfl,file=trim(line),status="replace",action="write")
               write(unit=line,fmt="(a,i2,a,f12.2)") "  Configuration #",i," found by SimAnneal_MultiConf,  cost=",mvs%cost(i)
               if(ninfo > 0) then
                 call Write_CFL_File(i_cfl,Cell,SpG,A_Clone,line,info_lines)
               else
                 call Write_CFL_File(i_cfl,Cell,SpG,A_Clone,line)
               end if
               close(unit=i_cfl)
             end do
         end if
     end if

     call Write_FinalCost(lun)
     !call Write_Atom_List(A,lun=lun)
     call Write_CFL_File(lun,Cell,SpG,A,line)
     !call Write_FST_A()

     if(Icost(1) == 1) call Write_ObsCalc_SFactors(lun,hkl,mode=diff_mode)
     if(Icost(7) == 1) call Write_FoFc_Powder(lun,Oh,hkl,mode=diff_mode)
     if(icost(2) == 1 .or. icost(3) == 1 .or. icost(4) == 1) then
        call Write_restraints_ObsCalc(A,lun)
     end if


     if(Icost(10) == 1 .or. Icost(11) == 1 ) call Write_Prf(filcod)

     !Write a CIF file and a VESTA file
     title=" CIF file generated by GLOpSAnn "
     !Write_CIF_Template(filename, Cell, SpG, Atmlist, Type_data, Code)
     call Write_Cif_Template(trim(filcod)//"_gen.cif",Cell,SpG,A,2,title)
     call Write_Vesta_File()

     if(fst_out) Call modify_fst()

      if(icost(5) == 1 .or. icost(6) == 1) then
        ! write(*,*) " => Calling Bond_Str ...."
        ! if(local_ref) then
        !    inquire(file=trim(filcod)//"_ref.cfl",exist=esta)
        !    if(esta) then
        !      call execute_command_line("Bond_Str "//trim(filcod)//"_ref.cfl",exitstat=ier)
        !      if(ier /= 0) write(*,*) "  Error calling Bond_Str"
        !    end if
        ! else if(c%num_conf == 1) then
        !    call execute_command_line("Bond_Str "//trim(filcod)//"_sol.cfl")
        ! else
        !    call execute_command_line("Bond_Str "//trim(filcod)//"_best.cfl")
        ! end if
       call Calc_BVS(Ac,lun)
      end if

     write(unit=*,fmt="(a)") " Normal End of: PROGRAM FOR OPTIMIZING X-TAL STRUCTURES "
     write(unit=*,fmt="(a)") " Results in File: "//trim(filcod)//".out"
     call cpu_time(fin)
     call system_clock(itick2)
     write(unit=*,fmt="(a,f10.2,a)")  "  Single Thread CPU-Time: ", fin-start," seconds"
     write(unit=*,fmt="(a,f10.2,a)")  "  Single Thread CPU-Time: ", (fin-start)/60.0," minutes"
     tims=(itick2-itick1)/real(count_rate)
     timm=tims/60.0
     write(unit=*,fmt="(a,f10.2,a)")  "      Elapsed CLOCK-Time: ",tims ," seconds"
     write(unit=*,fmt="(a,f10.2,a)")  "      Elapsed CLOCK-Time: ",timm," minutes"

     write(unit=lun,fmt="(/,a,f10.2,a)")  "  Single Thread CPU-Time: ", fin-start," seconds"
     write(unit=lun,fmt="(  a,f10.2,a)")  "  Single Thread CPU-Time: ", (fin-start)/60," Minutes"
     write(unit=lun,fmt="(  a,f10.2,a)")  "  Single Thread CPU-Time: ", (fin-start)/60/60," Hours"
     write(unit=lun,fmt="(a,f10.2,a)")    "      Elapsed CLOCK-Time: ",tims ," seconds"
     write(unit=lun,fmt="(a,f10.2,a)")    "      Elapsed CLOCK-Time: ",timm," minutes"
   end if

   close(unit=lun)
   call Close_GLOpSAnn()

   Contains

    Subroutine Close_GLOpSAnn()
      character(len=1) :: keyw
      if (wait_end) then
        write(unit=*,fmt="(/,a)") " => Press <Enter> to finish ..."
        read(unit=*,fmt="(a)") keyw
        stop
      else
        stop
      end if
    End Subroutine Close_GLOpSAnn

    Subroutine Write_fst_A()
       open(unit=ifst, file=trim(filcod)//"_A.fst", status="replace",action="write",position="rewind",iostat=ier)
        if(ier == 0) then
          write(unit=ifst,fmt="(a)") "TITLE  FST-file generated with Write_FST"
          write(unit=ifst,fmt="(a)") "SPACEG "//trim(Spg%SPG_Symb)
          write(unit=ifst,fmt="(a,3f12.5,3f8.3)") "CELL ",cell%cell, cell%ang
          write(unit=ifst,fmt="(a)") "BOX   -0.20  1.20   -0.20  1.20    -0.20  1.20 "
          do i=1,A%natoms
             write(unit=ifst,fmt="(a,a,3f12.5,tr4,a)")"Atom "//A%atom(i)%lab, A%atom(i)%ChemSymb, A%atom(i)%x, A%atom(i)%AtmInfo
          end do
          write(unit=ifst,fmt="(a)") "!"
          do i=1,ninfo
            write(unit=ifst,fmt="(a)") trim(info_lines(i))
          end do
          call flush(ifst)
          close(unit=ifst)
        end if
    End Subroutine Write_fst_A

    Subroutine modify_fst()
      logical :: esta
      if(ninfo > 0) then
        inquire(file=trim(filcod)//".fst",exist=esta)
        if(esta) then
           open(unit=ifst,file=trim(filcod)//".fst",status="old",action="write",position="append")
           do i=1,ninfo
             write(unit=ifst,fmt="(a)") trim(info_lines(i))
           end do
           close(unit=ifst)
        end if
      end if
    end Subroutine modify_fst

    Subroutine Write_Vesta_File()

       integer                                     :: lun, i, j, n,np, pol,k,cent,n_fst
       character(len=2)                            :: elem
       character(len=80)                           :: box_cmd,aux
       character(len=2), dimension(:), allocatable :: poly
       character(len=80),dimension(:), allocatable :: cmd_bond
       character(len=30),dimension(30)             :: cmds

       open(newunit=lun,file=trim(filcod)//".vesta",action="write",status="replace")
       write(unit=lun,fmt="(a)") "#VESTA_FORMAT_VERSION 3.1.9"
       write(unit=lun,fmt="(a)") " "
       write(unit=lun,fmt="(a)") " "
       write(unit=lun,fmt="(a)") "CRYSTAL"
       write(unit=lun,fmt="(a)") "TITLE"
       write(unit=lun,fmt="(a)") "  "//trim(title(5:))
       write(unit=lun,fmt="(a)") " "
       write(unit=lun,fmt="(a)") "IMPORT_STRUCTURE"
       write(unit=lun,fmt="(a)") trim(filcod)//"_gen.cif"
       write(unit=lun,fmt="(a)") " "
       write(unit=lun,fmt="(a)") " "
       write(unit=lun,fmt="(a)") "BOUND"
      ! Extract the information contained in string fst_cmd
      ! The items in string are separated by ";"
      n_fst=0
      do
        i=index(fst_cmd,";")
        if(i /= 0) then
          n_fst=n_fst+1
          cmds(n_fst)=fst_cmd(1:i-1)
          fst_cmd=fst_cmd(i+1:)
        else
          n_fst=n_fst+1
          cmds(n_fst)=fst_cmd
          exit
        end if
      end do

       box_cmd=" "
       do i=1,n_fst
         aux=u_case(cmds(i))
         j=index(aux,"BOX")
         if(j /= 0) then
           box_cmd=cmds(i)(j+3:)
           exit
         end if
       end do
       if(len_trim(box_cmd) == 0) then
          write(unit=lun,fmt="(a)") "    -0.1      1.1      -0.1      1.1      -0.1     1.1 "
       else
          write(unit=lun,fmt="(a)") trim(box_cmd)
       end if
       write(unit=lun,fmt="(a)")"  0   0   0   0  0"

       write(unit=lun,fmt="(a)") "SBOND"

       n=0; np=0
       pol=0
       allocate(cmd_bond(A%natoms), poly(A%natoms))
       cmd_bond=" "
       poly=" "
       do i=1,n_fst
           aux=u_case(cmds(i))
           j=index(aux,"CONN")
           if(j /= 0) then
             n=n+1
             cmd_bond(n)=adjustl(cmds(i)(j+4:))
             cycle
           end if
           j=index(line,"POLY")
           if(j /= 0)  then
              pol=1
              np=np+1
              poly(np)=adjustl(cmds(i)(j+4:))
           end if
       end do
       do i=1,n
         j=index(cmd_bond(i)," ")
         elem=cmd_bond(i)(1:j-1)
         cent=0
         do k=1,np
           if(poly(k) == elem) then
              cent=1
              exit
           end if
         end do
         write(unit=lun,fmt="(i3,a,5i3)") i,"  "//trim(cmd_bond(i)),0,1,cent,0,1
       end do
       write(unit=lun,fmt="(a)")    "  0 0 0 0"
       write(unit=lun,fmt="(a)")    " "
       write(unit=lun,fmt="(a)")    "STYLE"
       write(unit=lun,fmt="(a)")    "MODEL   2  1  0"
       write(unit=lun,fmt="(a)")    "SURFS   0  1  1"
       write(unit=lun,fmt="(a)")    "SECTS  96  0"
       write(unit=lun,fmt="(a,i1)") "POLYS  ",pol
       call flush(lun)
       close(unit=lun)
    End Subroutine Write_Vesta_File

End Program Global_Optimization_Xtal_structures

    !!----       Subroutine Write_FST(fst_file,v,cost)
    !!----          character(len=*),           intent(in):: fst_file
    !!----          real(kind=cp),dimension(:), intent(in):: v
    !!----          real(kind=cp),              intent(in):: cost
    !!----       End Subroutine Write_FST
Subroutine Write_FST(fst_file,v,cost)
   !---- Arguments ----!
   Use CFML_GlobalDeps,            only: cp
   Use CFML_Keywords_Code_Parser,  only: VState_to_AtomsPar
   use GLS_cost_functions,         only: Cell,A,SpG

   character(len=*),               intent(in) :: fst_file
   real(kind=cp),dimension(:),     intent(in) :: v
   real(kind=cp),                  intent(in) :: cost

   !----- Local variables -----!
   integer :: lun,i,nc, ier
   character(len=132)                 :: file_fst,fst_cmd
   character(len=30), dimension(10)   :: cmds

   if(v(1) == 1.234567 .and. cost == v(2)) write(*,"(a)") " => This is nothing! To avoid warnings in compilation"
   i=index(fst_file,".fst")
   file_fst=fst_file(1:i+3)
   fst_cmd=adjustl(fst_file(i+4:))
   nc=0
   do
     i=index(fst_cmd,";")
     if(i /= 0) then
       nc=nc+1
       cmds(nc)=fst_cmd(1:i-1)
       fst_cmd=fst_cmd(i+1:)
     else
       nc=nc+1
       cmds(nc)=fst_cmd
       exit
     end if
   end do

   !Update the atom parameters
   call VState_to_AtomsPar(A,mode="V")

   open(newunit=lun, file=trim(file_fst), status="replace",action="write",position="rewind",iostat=ier)
   if (ier == 0) then
      write(unit=lun,fmt="(a)") "TITLE  FST-file generated with Write_FST"
      write(unit=lun,fmt="(a)") "SPACEG "//trim(Spg%SPG_Symb)
      write(unit=lun,fmt="(a,3f12.5,3f8.3)") "CELL ",cell%cell, cell%ang
      write(unit=lun,fmt="(a)") "BOX   -0.20  1.20   -0.20  1.20    -0.20  1.20 "
      do i=1,A%natoms
         write(unit=lun,fmt="(a,a,3f12.5,tr4,a)")"Atom "//A%atom(i)%lab, A%atom(i)%ChemSymb, A%atom(i)%x, A%atom(i)%AtmInfo
      end do
      do i=1,nc
         write(unit=lun,fmt="(a)") trim(cmds(i))
      end do
      write(unit=lun,fmt="(a)") "!"
      call flush(lun)
      !flush(lun)
      close(unit=lun)
   end if

End Subroutine Write_FST


