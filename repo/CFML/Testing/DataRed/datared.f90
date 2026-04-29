!! --- New version of the program DataRed based in CrysFML08
!! ---

  Program DataRed

    Use CFML_GlobalDeps
    Use CFML_Maths,   only: Set_Eps_Math
    Use CFML_gSpaceGroups
    Use CFML_Metrics
    Use CFML_Reflections
    Use CFML_strings, only: File_Type, Reading_File
    Use Twin_Mod
    Use DataRed_Mod
    Use DataRed_rnw_Reflections_Mod
    Use DataRed_treat_Reflections_Mod
    Use CFML_Propagation_Vectors
    implicit none

    integer                      :: narg,len_cmdline,idot,lun,i
    character(len=:),allocatable :: cmdline,filered
    logical :: esta !, debug=.true.

    type(Reflection_List)          :: Ref
    type(File_Type)                :: cfl_file
    type(Twin_Type)                :: Tw
    class(SPG_Type),   allocatable :: SpG, twSpG
    class(Cell_G_Type),allocatable :: cell
    Type(Conditions_Type)          :: cond
    Type(kvect_info_Type)          :: kinf
    Type(Group_k_Type),dimension(:),allocatable :: Gk


    narg=COMMAND_ARGUMENT_COUNT()
    cmdline="                          "
    len_cmdline=0
    if(narg > 0) then
        call GET_COMMAND_ARGUMENT(1,cmdline)
        len_cmdline=len_trim(cmdline)
    else
        write(unit=*,fmt="(a)") " => The program DataRed should be invoked as: DataRed xxx.red or xxx.cfl ! "
        call CloseProgram()
    end if
    !Attempt to read the input *.red file
    call Header_Output()
    if(len_cmdline /= 0) then
        idot=index(cmdline,".")
        if(idot == 0) then
           filered=cmdline//".red"
        else
           filered=cmdline
        end if
        inquire(file=filered,exist=esta)

        if(.not. esta) then
          write(unit=*,fmt="(a)") " => File: "//filered//" not found! "
          call CloseProgram()

        else

          cfl_file=Reading_File(filered)
          if(Err_CFML%Ierr /= 0) then
            write(unit=*,fmt="(a)")  Err_CFML%Msg
            call CloseProgram()
          end if

          Call Read_DataRed_File(cfl_file,cond,kinf,tw,SpG,Cell)

          if(Err_CFML%Ierr /= 0) then
            write(unit=*,fmt="(a)")  Err_CFML%Msg
            call CloseProgram()
          else if (.not. allocated(SpG)) then
            write(unit=*,fmt="(a)")  " => No space group is allocated! Check your input file!"
            call CloseProgram()
          else
            if(cond%eps_given) then
              call Set_Eps_Math(cond%epsg)
            else
              call Set_Eps_Math(0.0005_cp)
            end if
          end if

          if(cond%prop .and. SpG%mag_type == 1) then !Setting up the propagation vectors groups
            allocate(Gk(kinf%nk))
            do i=1,kinf%nk
              call K_Star(kinf%kv(:,i),SpG,Gk(i),.true.)
            end do
          end if

          if(len_trim(cond%filhkl) /= 0) then  !Read Reflections and contruct Reflection_list object Ref
            if(cond%prop .and. SpG%mag_type == 1) then
              call Read_Reflections_File(cond%filhkl,cond,kinf,Cell,Ref,Gk)
            else
              call Read_Reflections_File(cond%filhkl,cond,kinf,Cell,Ref)
            end if
            if(Err_CFML%Ierr /= 0) then
              write(unit=*,fmt="(a)")  Err_CFML%Msg
              call CloseProgram()
            end if
          end if

          if(len_trim(cond%fileout) /= 0) then
             open(newunit=lun,file=trim(cond%fileout)//".out",status="replace",action="write")
             !Call Write_Conditions(cfl_file,cond,Cell,SpG,kinf,tw,cond%forma)
             Call Header_Output(lun)
             if(cond%prop .and. SpG%mag_type == 1) then
               Call Write_Conditions(cfl_file,cond,Cell,SpG,kinf,tw,cond%forma,lun,Gk)
             else
               Call Write_Conditions(cfl_file,cond,Cell,SpG,kinf,tw,cond%forma,lun)
             end if

             if(tw%ISpG)  then
               allocate(SpG_Type :: twSpG)
               call Set_SpaceGroup(tw%twin_SpG,twSpG)
               write(unit=lun,fmt="(/,a)") "     SPACE GROUP FOR DOMAINS OF A TWINNED CRYSTAL: USED FOR SYSTEMATIC ABSENCES"
               write(unit=lun,fmt="( a)") "     =========================================================================="
               call Write_SpaceGroup_Info(twSpG,lun)
             end if
             if(cond%print_all) Call Write_Reflections(Ref,cond,kinf,lun)
             if(cond%prop .and. SpG%mag_type == 1 .and. kinf%nk <= 1 .and. cond%hkl_type /= 11 .and. cond%hkl_type /= 12) then
               Call Treat_Reflections(Ref,cond,cell,SpG,Gk(1),tw,lun)
             else
               if(kinf%nk == 0) then
                 Call Treat_Reflections("SHUB",Ref,cond,cell,SpG,kinf,tw,lun) !,debug)
               else
                 Call Treat_Reflections("SUPER",Ref,cond,cell,SpG,kinf,tw,lun) !,debug)
               end if
             end if
          end if
        end if
    else
        write(unit=*,fmt="(a)") " => An input (xxx.red or xxx.cfl) datared file is needed !"
        call CloseProgram()
    End if
    write(unit=*,fmt="(a)")     " => Program finished O.K.!, look in output files!"
    write(unit=*,fmt="(a,a)")   "           Output  file: ", trim(cond%fileout)//".out"
    write(unit=*,fmt="(a,a)")   "      Reflections  file: ", trim(cond%fileout)//".int"
    if(cond%split_mag) &
    write(unit=*,fmt="(a,a)")   "      Reflections  file: ", trim(cond%fileout)//"_mag.int"
    if(cond%to_og) &
    write(unit=*,fmt="(a,a)")   "      Reflections  file: ", trim(cond%fileout)//"_mag_og.int"
    write(unit=*,fmt="(a,a)")   "           Reject  file: ", trim(cond%fileout)//".rej"
    write(unit=*,fmt="(a)")   " "
    call CloseProgram()
    Contains
      Subroutine CloseProgram()
         character(len=1) :: ans
         write(unit=*,fmt="(a)")   " "
         write(unit=*,fmt="(a)")   " => Press <cr> to finish ...."
         read(unit=*,fmt="(a)") ans
         stop
      End Subroutine CloseProgram

  End Program DataRed
