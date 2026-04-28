  Program cfl_to_cif
    use CFML_GlobalDeps
    Use CFML_Metrics
    Use CFML_Rational
    Use CFML_Maths,        only: modulo_lat,Set_Eps_Math, determ3D, Inverse_Matrix
    Use CFML_gSpaceGroups, only: Set_SpaceGroup, Write_SpaceGroup_Info, SpG_Type, &
                                 get_stabilizer, Get_multip_pos, Get_Mat_From_Symb_Int, &
                                 get_orbit, Get_SubGroups_full, Identify_Group, &
                                 Change_Setting_SpaceG, Get_SubGroups_gen
    Use CFML_Strings,      only: l_case, number_lines, pack_string, u_case, File_Type
    Use CFML_Atoms,        only: AtList_Type, Allocate_Atom_list, Write_Atom_List, &
                                 Extend_Atom_List,Atom_Equiv_List_Type, Atm_Cell_Type
    Use CFML_Geom,         only: point_list_type, get_transf_list, allocate_point_list, &
                                 deallocate_point_list,set_orbits_inlist,Set_New_AsymUnit
    Use CFML_IOForm

    Implicit None

    type(File_Type)                  :: file_dat
    class(Cell_G_Type),allocatable   :: Cell, Cell_n
    class(SpG_Type),allocatable      :: SpaceGroup,SpaceGroup_n
    type(AtList_Type)                :: A, A_n   !List of atoms in the asymmetric unit
    type(Atom_Equiv_List_Type)       :: Ate  !List of all atoms in the cell

    character(len=80)        :: line , cmdline, trans_symb
    character(len=256)       :: filcod,outfil
    integer, parameter       :: lun1=1,lun2=6,lun=2
    integer                  :: i, j, ln, len_cmdline, &
                                lenf, lr
    integer                  :: n_ini,n_end
    real                     :: End_time, start_time, det
    real, dimension(3,3)     :: trans      !matrix transforming the cells
    real, dimension(3)       :: orig       !origin of the transformed cell in old cell
    logical                  :: trans_given, trn_std, esta
    integer                  :: narg
    real, parameter,dimension(3,3) :: identity=reshape ([1,0,0,0,1,0,0,0,1],[3,3])
    type(rational), dimension(4,4) :: rMat

    narg=COMMAND_ARGUMENT_COUNT()
    len_cmdline=0
    if(narg > 0) then
            call GET_COMMAND_ARGUMENT(1,cmdline)
            len_cmdline=len_trim(cmdline)
            outfil=" "
            if(narg > 1) then
              call GET_COMMAND_ARGUMENT(1,outfil)
              i=index(outfil,".")
              if(i /= 0) outfil=outfil(1:i-1)
            end if
    end if

    call Set_Eps_Math(0.001)   !Needed for atom position comparisons and integer comparisons

    write(unit=*,fmt="(/,/,6(a,/))")                                                 &
    "                  ------ PROGRAM CFL to CIF ------",                            &
    "                   ---- Version 0.0 May-2020 ----",                             &
    "                      (ILL JRC, NAK - May 2020 )"

    Do
      if(len_cmdline /=0) then
        lenf=index(cmdline,".")-1
        if(lenf <= 0) then
          filcod=cmdline
        else
          filcod=cmdline(1:lenf)
        end if
        if(len_trim(outfil) == 0) outfil=filcod
        ln=len_trim(filcod)
        lr=len_trim(outfil)
      else
        write(unit=*,fmt="(a)") " => A CFL file should be given as an argument invoking the program "
        call CloseProgram()
      end if

      inquire(file=trim(filcod)//".cfl",exist=esta)
      if( .not. esta) then
        write(unit=*,fmt="(a)") " File: "//trim(filcod)//".cfl doesn't exist!"
        call CloseProgram()
      end if
      !call Readn_Set_Xtal_Structure(trim(filcod)//".cfl",Cell,SpaceGroup,A,"Atm",Mode="CFL",file_list=file_dat)
      call Read_Xtal_Structure(trim(filcod)//".cfl",Cell,SpaceGroup,A)
      call Write_Atom_List(A)
      If(err_CFML%Ierr == 1) then
        write(unit=*,fmt="(a)") trim(err_CFML%Msg)
        call CloseProgram()
      else
        exit
      end if
    End do

    open(unit=lun,file=outfil(1:lr)//".out",status="replace",action="write")

      write(unit=lun,fmt="(/,6(a,/))")                                               &
    "                  ------ PROGRAM CFL to CIF ------",                            &
    "                   ---- Version 0.0 May-2020 ----",                             &
    "                      (ILL JRC, NAK - May 2020 )"
   !  Second reading and start calculations
    !  -------------------------------------
    CALL CPU_TIME(start_time)

    ! Print the whole information on cell, space group and atoms

    call Write_SpaceGroup_Info(SpaceGroup,lun)
    call Write_Crystal_Cell(Cell,lun)
    call Write_Atom_List(A,lun)
    call Extend_Atom_List(SpaceGroup,cell,A,Ate,lun)

    n_ini=1
    n_end=file_dat%nlines
    trans_given=.false.
    trn_std = .false.
    call clear_error()
    do i=4,n_end
      line=l_case(adjustl(file_dat%line(i)%Str))
      if(line(1:1) == "!") cycle
      j=index(line," ")
      if(line(1:5) == "trans") then
        line=adjustl(line(j:))
        trans_symb=line
        call Get_Mat_From_Symb_Int(trans_symb,rMat)
        trans=rMat(1:3,1:3)
        orig=rMat(1:3,4)
        if(ERR_CFML%Ierr /= 0) then
          write(unit=*,fmt="(a)") " => Error: "//trim(ERR_CFML%Msg)
        else
          trans_given=.true.
        end if
        cycle
      end if
    end do
    if(trans_given) then
       write(unit=lun,fmt="(/,a,/,a,/)")" => Change of Space group setting according to the transformation:", &
                           "    --------------------------------------------------------------"
       write(unit=lun,fmt="(a)") "                Matrix M (A'= M A)              Origin"
       write(unit=*,fmt="(/,a,/,a,/)")" => Change of Space group setting according to the transformation:", &
                           "    --------------------------------------------------------------"
          write(unit=*,fmt="(a)") "                  Matrix M (A'= M A)              Origin"
       do i=1,3
          write(unit=lun,fmt="(a,(3f8.4,a,f10.5))") "            ",trans(i,1:3), "          ", orig(i)
          write(unit=*,  fmt="(a,(3f8.4,a,f10.5))") "            ",trans(i,1:3), "          ", orig(i)
       end do
       det=determ3D(trans)
       det=abs(det)
       write(unit=lun,fmt="(/,a,/)")" => Maximal subgroup of the original space group according to the cell transformation:"
       SpaceGroup_n=SpaceGroup
       if(trim(trans_symb)/="a,b,c;0,0,0") then
         call Change_Setting_SpaceG(trans_symb, SpaceGroup_n)
       end if
       call Change_Setting_Cell(Cell,trans,Cell_n)
       call Write_SpaceGroup_Info(SpaceGroup_n,lun)
       call Write_Crystal_Cell(Cell_n,lun)
       write(unit=*,fmt="(a)")" => Creating The new Asymmetric unit"
       ! It is needed to increase the asymmetric unit because some atoms may be lost
       ! after decreasind the number of symmetry operators in the new subgroup.
       ! call change_setting_atoms(Cell_n,A,trans,orig,A_n)  <= not adequate in this context
       call Set_new_AsymUnit(SpaceGroup_n,Ate,trans,orig,A_n,"IT")!,debug="D")
       if(err_CFML%Ierr /= 0) then
         write(unit=*,fmt="(a)") trim(err_CFML%Msg)
       end if
       ! Write the atoms the in new asymmetric unit
       call Write_Atom_List(A_n,lun)
     else
       Cell_n=Cell
       SpaceGroup_n=SpaceGroup
       A_n=A
     end if
    call Write_Cif_Template(trim(filcod)//".cif", Cell_n, SpaceGroup_n, A_n, 2, "CIF file for: "//trim(filcod))

    CALL CPU_TIME(end_time)
    write(unit=*,fmt="(/a,f9.3,a)")  " => Program finished normally, CPU-time: ",end_time-start_time, " seconds"
    write(unit=lun,fmt="(/a,f9.3,a)")  " => Program finished normally, CPU-time: ",end_time-start_time, " seconds"
    write(unit=*,fmt="(a,a)") " => Results in file: ",  trim(outfil)//".out"
    write(unit=*,fmt="(a,a)") "           and file: ",  trim(outfil)//".cif"

    contains
      Subroutine CloseProgram()
         character(len=1) :: ans
         write(unit=*,fmt="(a)")   " "
         write(unit=*,fmt="(a)")   " => Press <cr> to finish ...."
         read(unit=*,fmt="(a)") ans
         stop
      End Subroutine CloseProgram

  End Program cfl_to_cif
