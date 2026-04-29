  Program Similar_Obtain_Subgroups
  !!-----------------------------------------------------------------------------------------------
  !!--- Derived from the Program SIMILAR
  !!--- Purpose: Generate files with geometric transformations of atom positions.
  !!---          Change of Space Group Settings
  !!---          Obtention of subgroups and transformation to standard setting
  !!--- (C) Created by JRC at Laboratorire Leon Brillouin, January 2002. Based in an old
  !!---     Fortran 77 program written in 1990 at ILL. It was re-written based in CrysFML
  !!---     Updated in January 2014 (JRC). Update October 2018 (JRC)
  !!--- Authors: Juan Rodriguez-Carvajal (Institut Laue-Langevin)
  !!---          Nebil A. Katcho (Institut Laue-Langevin) (Module: CFML_SpG_Standard_Representation)
  !!------------------------------------------------------------------------------------------------
    use CFML_GlobalDeps
    Use CFML_Metrics
    Use CFML_Rational
    Use CFML_Maths,        only: modulo_lat,Set_Eps_Math, determ3D, Inverse_Matrix
    Use CFML_gSpaceGroups, only: Set_SpaceGroup, Write_SpaceGroup_Info, SpG_Type, &
                                 get_stabilizer, Get_multip_pos, Get_Mat_From_Symb_Int, &
                                 get_orbit, Get_SubGroups_full, Identify_Group, &
                                 Change_Setting_SpaceG, Get_SubGroups_gen, Point_Orbit
    Use CFML_Strings,      only: l_case, number_lines, pack_string, u_case, File_Type
    Use CFML_Atoms,        only: AtList_Type, Allocate_Atom_list, Write_Atom_List, &
                                 Extend_Atom_List,Atom_Equiv_List_Type, Atm_Cell_Type
    Use CFML_Geom,         only: point_list_type, get_transf_list, allocate_point_list, &
                                 deallocate_point_list,set_orbits_inlist,Set_New_AsymUnit
    Use CFML_IOForm

    Implicit None

    type(File_Type)                  :: file_dat
    class(Cell_G_Type),allocatable   :: Cell, Cell_n, Cell_std
    class(SpG_Type),allocatable      :: SpaceGroup,SpaceGroup_n
    type(SpG_Type), dimension (1024) :: SubGroup
    type(AtList_Type)                :: A, A_n      !List of atoms in the asymmetric unit
    type(AtList_Type)                :: Asub
    type(Atom_Equiv_List_Type)       :: Ate,Ate_n  !List of all atoms in the cell
    type(Point_list_type)            :: pl, pl_n

    character(len=1)         :: ans
    character(len=20)        :: nam
    character(len=80)        :: line , cmdline, trans_symb
    character(len=40),dimension(1024)  :: symb_setting
    character(len=256)       :: filcod,outfil,texto
    character(len=:),allocatable :: aux_symm
    integer, parameter       :: lun1=1,lun2=6,lun=2
    integer                  :: i, j, numops, ier, ln, nauas,  len_cmdline, &
                                lenf, lr, nsg, nat, i1,i2
    integer                  :: l,ng, indx, k, norbi, n, indice,i_cfl,mult,na
    integer                  :: n_ini,n_end
    real                     :: seconds, End_time, start_time, det,occ
    integer, dimension(3,3)  :: Mat        !Auxiliary matrix
    integer, dimension(1024) :: ind_group = 0
    real, dimension(3,3)     :: trans      !matrix transforming the cells
    real, dimension(3,3,1024):: invMt,Mstd !Transformation of coordinates to standard setting
    real, dimension(3,1024)  :: tor        !Origin for transforming to standard setting
    real, dimension(3)       :: orig       !origin of the transformed cell in old cell
    real, dimension(3)       :: xp         !auxiliary 3D-vector
    logical                  :: trans_given, trn_std, index_given, fix_given, full_given, esta, none_given
    type(Point_Orbit)        :: orb
    character(len=1)         :: fix_lat
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
    "                    ------ PROGRAM SIMILAR ------",                             &
    "                   ---- Version 5.0 May-2020 ----",                             &
    "  **************************************************************************",  &
    "  * Cell and coordinate transformations, Space Group Settings, coordinate  *",  &
    "  **************************************************************************",  &
    "                      (ILL JRC, NAK - May 2020 )"

    DO
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
        write(unit=*,fmt="(a)",advance="no") " => Code of the file xx.cfl/cif (give xx): "
        read(unit=*,fmt="(a)") filcod
        ln=len_trim(filcod)
        write(unit=*,fmt="(3a)",advance="no") " => Code of the output files (.coo,.sym) ( <cr>= ",filcod(1:ln),") :"
        read(unit=*,fmt="(a)") outfil
        lr=len_trim(outfil)
        IF(lr == 0) THEN
          outfil=filcod
          lr=len_trim(outfil)
        END IF
      end if

      !inquire(file=trim(filcod)//".cif",exist=esta)
      !if(esta) then
      !  call Readn_Set_Xtal_Structure(trim(filcod)//".cif",Cell,SpaceGroup,A,Mode="CIF")
      !else
        inquire(file=trim(filcod)//".cfl",exist=esta)
        if( .not. esta) then
          write(unit=*,fmt="(a)") " File: "//trim(filcod)//".cif (or .cfl) doesn't exist!"
          stop
        end if
        !call Readn_Set_Xtal_Structure(trim(filcod)//".cfl",Cell,SpaceGroup,A,"Atm",Mode="CFL",file_list=file_dat)
        call Read_Xtal_Structure(trim(filcod)//".cfl",Cell,SpaceGroup,A,Ftype=file_dat)
      !end if
      If(err_CFML%Ierr == 1) then
        write(unit=*,fmt="(a)") trim(err_CFML%Msg)
        call CloseProgram()
      else
        exit
      end if
    End do

    open(unit=lun,file=outfil(1:lr)//".coo",status="replace",action="write")

      write(unit=lun,fmt="(/,6(a,/))")                                               &
    "                    ------ PROGRAM SIMILAR ------",                             &
    "                   ---- Version 5.0 May-2020 ----",                             &
    "  **************************************************************************",  &
    "  * Cell and coordinate transformations, Space Group Settings, coordinate  *",  &
    "  **************************************************************************",  &
    "                      (ILL JRC, NAK - May 2020 )"
    !  Second reading and start calculations
    !  -------------------------------------
    CALL CPU_TIME(start_time)

    ! Print the whole information on cell, space group and atoms

    call Write_SpaceGroup_Info(SpaceGroup,lun)
    call Write_Crystal_Cell(Cell,lun)
    call Write_Atom_List(A,Iunit=lun)
    call Extend_Atom_List(SpaceGroup,cell,A,Ate,lun)
    n_ini=1
    n_end=file_dat%nlines
    write(*,*) " => Number of lines in file: ",n_end
    trans_given=.false.
    trn_std = .false.
    index_given=.false.
    indice=48
    fix_given=.false.
    full_given=.false.
    none_given=.false.
    call clear_error()
    do i=4,n_end
      write(*,"(a)") " -> "//file_dat%line(i)%str
      line=l_case(adjustl(file_dat%line(i)%str))
      if(line(1:1) == "!") cycle
      if(line(1:4) == "none") none_given=.true.
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
      if(line(1:6) == "trn_to") then !Transform coordinates to standard setting
        trn_std = .true.
        cycle
      end if
      if(index(line,"index") /= 0) then
        read(unit=file_dat%line(i)%str(6:),fmt=*,iostat=ier) indice
        if(ier == 0) index_given=.true.
        cycle
      end if
      if(index(line,"fix_lat") /= 0) then
        fix_lat=adjustl(file_dat%line(i)%str(8:))
        if(index("PABCIRF",fix_lat) /= 0) fix_given=.true.
        cycle
      end if
      if(index(line,"full") /= 0) full_given=.true.
    end do
    write(*,"(a)") " => Entering main Loop"
    do
      if(.not. trans_given) then
        write(unit=*,fmt="(/,a)") " => Give the transformation Matrix in the form: -b,a+b,2c;0,1/4,0 => "
        read(unit=*,fmt="(a)") trans_symb
        if(len_trim(trans_symb) == 0) then
          trans_symb="a,b,c;0,0,0"
        end if
        call Get_Mat_From_Symb_Int(trans_symb,rMat)
        trans=rMat(1:3,1:3)
        orig=rMat(1:3,4)
      end if
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

       if(trim(trans_symb)=="a,b,c;0,0,0") then
         write(unit=lun,fmt="(a)") " => The new space group and cell are identical to the provided group and cell"
         Cell_n=Cell
       else
         call Write_SpaceGroup_Info(SpaceGroup_n,lun)
         call Change_Setting_Cell(Cell,trans,Cell_n)
         write(unit=lun,fmt="(/,a,/,a,/)")" => Change of Unit cell according to the transformation:", &
                                          "    ----------------------------------------------------"
         write(unit=lun,fmt="(a)") "                Matrix M (A'= M A)               Origin"

         do i=1,3
            write(unit=lun,fmt="(a,(3f8.4,a,f10.5))") "            ",trans(i,1:3), "          ", orig(i)
         end do
         call Write_Crystal_Cell(Cell_n,lun)
       end if


      ! It is needed to increase the asymmetric unit because some atoms may be lost
      ! after decreasind the number of symmetry operators in the new subgroup.
      ! call change_setting_atoms(Cell_n,A,trans,orig,A_n)  <= not adequate in this context

      write(unit=*,fmt="(a)")" => Creating The new Asymmetric unit"
      call Set_new_AsymUnit(SpaceGroup_n,Ate,trans,orig,A_n,"IT")!,debug="D")
      if(err_CFML%Ierr /= 0) then
        write(unit=*,fmt="(a)") trim(err_CFML%Msg)
      end if
      ! Write the atoms the in new asymmetric unit
      call Write_Atom_List(A_n,Iunit=lun)

      if(full_given) then
        write(unit=lun,fmt="(/,a,/,a,/)")" => List of all atoms in the new cell", &
                                         "    ---------------------------------"
        call Extend_Atom_List(SpaceGroup_n,cell_n,A_n,Ate_n,lun)
      end if

      if(none_given) exit !no calculation of subgroups

      call allocate_point_list(SpaceGroup%multip,pl,ier)  !Allocate the point list for original cell
      if(ier /= 0) then
         write(unit=*,fmt="(a)")  " => Error allocating  ", SpaceGroup%multip," points for PL"
      end if
      if(det >= 1.0) then
        nat=SpaceGroup%multip*det+1
      else
        nat=SpaceGroup%multip+1
      end if
      call Allocate_Point_List(nat,pl_n,ier)   !allocate the new point list in the new cell
      if(ier /= 0) then
         write(unit=*,fmt="(a)")  " => Error allocating  ", nat," points for PL_n"
      end if


     !Determine all translationengleiche subgroups of the new space group
      CALL CPU_TIME(seconds)
      write(unit=*,fmt="(a)")  " => Start calculation of subgroups: "
          !call Get_SubGroups_full(SpaceGroup_n, SubGroup, nsg,printd=.true.)
          call Get_SubGroups_gen(SpaceGroup_n, SubGroup, nsg, printd=.true.)
          if (Err_CFML%Ierr /= 0) then
             write(*,'(/,4x,"=> Error in the generation of the subgroups: ",a)') trim(Err_CFML%Msg)
          end if
          do L=1,nsg
            call Identify_Group(SubGroup(L))
            if (Err_CFML%Ierr /= 0) then
               write(*,'(/,4x,"=> Error in the identification of the group: ",a)') trim(Err_CFML%Msg)
            end if
          end do
      CALL CPU_TIME(end_time)
      write(unit=*,fmt="(a,i3,a,f9.3,a)") " => A total of ", nsg, " Subgroups have been calculated !, CPU-time: ",end_time-seconds, " seconds"

      write(unit=lun,fmt="(/,/,a,a,/)") " => LIST of Subgroups for: ",SpaceGroup_n%BNS_Symb
      do i=1,nsg
        indx=SpaceGroup_n%Multip/SubGroup(i)%multip
        ind_group(i) =indx
        if(index_given .and. indx > indice) cycle
        ng=SubGroup(i)%numops
        if(len_trim(SubGroup(i)%mat2std_shu) == 0  ) then
           write(unit=SubGroup(i)%mat2std_shu,fmt="(i4)") i
           symb_setting(i)=pack_string(SubGroup(i)%mat2std_shu)
           !do n=1,SubGroup(i)%Multip
           !    write(*,"(12x,i4,a,a1,i2)") n, " -> "//trim(SubGroup(i)%Symb_Op(n)),",",1
           !end do
        else
           symb_setting(i)=SubGroup(i)%mat2std_shu
        end if
        write(unit=lun,fmt="(4a,i6,30a)") " => ", SubGroup(i)%BNS_Symb,"  To standard: "//symb_setting(i),&
          "   Index: [",indx,"]   First Numops Operators: ->  { ", ( trim(SubGroup(i)%Symb_Op(l))//" ; ",l=1,ng-1),&
          trim(SubGroup(i)%Symb_Op(ng))," }    ", trim(SubGroup(i)%centre)
        write(unit=lun,fmt="(4a,i6,30a)") "    Generators:", trim(SubGroup(i)%generators_list)
        write(unit=*,fmt="(4a,i6,30a)")   " => ", SubGroup(i)%BNS_Symb, "  To standard: "//symb_setting(i),&
          "   Index: [",indx,"]   First Numops Operators: ->  { ", ( trim(SubGroup(i)%Symb_Op(l))//" ; ",l=1,ng-1),&
          trim(SubGroup(i)%Symb_Op(ng))," }    ", trim(SubGroup(i)%centre)
        write(unit=*,fmt="(4a,i6,30a)")   "    Generators:", trim(SubGroup(i)%generators_list)
      end do

      write(unit=lun,fmt="(/,a)") "    ---------------------------------------------------------  "
      write(unit=lun,fmt="(a  )") "    Decomposition of orbits in Translationengleiche Subgroups  "
      write(unit=lun,fmt="(a,/)") "    ---------------------------------------------------------  "
      !
      !  Writing the notation of the orbits with respect to the original group
      !
       write(unit=lun,fmt="(a,a)") "    Below, the atoms of the asymmetric unit of the original space group : ", &
                  trim(SpaceGroup%BNS_Symb)//"   "//trim(SpaceGroup%hall)
       write(unit=lun,fmt="(a)")   "    Are labelled as follows : "

       do n=1,A%natoms  !loop over atoms in the asymmetric unit of the original group

         Call get_orbit(A%atom(n)%x,SpaceGroup,orb)
         pl%np= orb%mult
         pl%x = orb%pos
         do j=1,pl%np
           write(unit=pl%nam(j),fmt="(a,i3,a)") trim(A%atom(n)%lab)//"(",j,")"
           pl%nam(j)=pack_string(pl%nam(j))
         end do
         write(unit=lun,fmt="(a,48a)") "     Atom: "//trim(A%atom(n)%lab)//" -> ", ( trim(pl%nam(j))//"   ",j=1,pl%np)
       end do

      write(unit=lun,fmt="(/,a)")   "    The representative atoms of the different sub-orbits are obtained by appending"
      write(unit=lun,fmt="(  a)")   "    an underscore '_' followed by a figure numbering the different orbits  "
      write(unit=lun,fmt="(  a)")   "    Example: an atom like Fe1(3)_4 corresponds to the orbit 4 that is obtained from the "
      write(unit=lun,fmt="(  a)")   "             sublattice (3) of the atom Fe1 in the asymmetric unit of the original group "
      write(unit=lun,fmt="(  a)")   "    When 'full' is provided and additional underscore followed by the number of the new   "
      write(unit=lun,fmt="(  a)")   "    sublattice is added   "
      write(unit=lun,fmt="(/,a)")   "    The output of the new asymmetric units in the generated CFL files is simplified:"
      write(unit=lun,fmt="(  a)")   "    The original label is conserved and only '_n', with n=1,n_suborbits is appended"


      if(index_given) then
         write(unit=lun,fmt="(/,a,i2,a,/)") "    Only sub-groups of index below or equal to [",indice,"] are output"
         write(unit=*,fmt="(/,a,i2,a,/)")   "    Only sub-groups of index below or equal to [",indice,"] are output"
      end if

      write(unit=*,fmt="(a)")  " => Start Decomposition of orbits for subgroup: "

      Mat=identity
      nauas=SpaceGroup_n%Multip*A%natoms
      do i=1,nsg
        indx=ind_group(i)
        if(index_given .and. indx > indice ) cycle
        write(*,"(a,i4,a,a,i4,a)") " => SubGroup #",i,"   "//SubGroup(i)%BNS_Symb," of index [",indx,"]"
        write(unit=lun,fmt="(/,a)") "    --------------------------------------------------------------------------------------"
        write(unit=lun,fmt="(3a,i4,a)")  " => Decomposition of orbits for subgroup: ", &
                                          SubGroup(i)%BNS_Symb, "  Index [",indx,"]"
        write(unit=lun,fmt="(a,/)") "    --------------------------------------------------------------------------------------"
        if(trn_std .and. len_trim(SubGroup(i)%BNS_Symb) == 0) write(unit=lun,fmt="(a)")" => All atom coordinates have been changed to those of the subgroup in the standard setting"

        !write(unit=texto,fmt="(a,i4)") trim(SubGroup(i)%BNS_Symb)//"_"//trim(symb_setting(i))//"_ind",indx
        write(unit=texto,fmt="(a,i4)") trim(filcod)//"_"//trim(SubGroup(i)%BNS_Symb)//"_ind",indx

        texto=Pack_String(texto)
        do j=1,len_trim(texto)
          if(texto(j:j) == "'" .or. texto(j:j) == '"') texto(j:j)="p"
          if(texto(j:j) == "/" ) texto(j:j)="_"
        end do


        open(newunit=i_cfl,file=trim(texto)//".cfl",status="replace",action="write")
        write(unit=i_cfl,fmt="(a)") "TITLE   CFL-file generated by SIMILAR: "//trim(texto)
        write(unit=i_cfl,fmt="(a)") "! The symmetry operators and the atom positions come from the transformation"
        write(unit=i_cfl,fmt="(a)") "! "//trim(SpaceGroup_n%setting)//"  Applied to space group: "//trim(SpaceGroup%Spg_Symb)
        write(unit=i_cfl,fmt="(a)") "! The maximal subgroup with this setting is: "//trim(SpaceGroup_n%Spg_Symb)
        write(unit=i_cfl,fmt="(a)") "! This gives rise to a series of tranlationengleiche subgroups, the current subgroup is: "
        write(unit=i_cfl,fmt="(a,i4,a)") "! "//SubGroup(i)%BNS_Symb//"  "//trim(symb_setting(i))//"  Index [",indx,"]"

        if(trn_std .and. len_trim(SubGroup(i)%BNS_Symb) == 0) then !Transform the unit cell
          write(unit=i_cfl,fmt="(a)") "! Cell changed to standard setting"
          write(unit=i_cfl,fmt="(a)") "!            a           b           c       alpha    beta   gamma"
          call Change_Setting_Cell(Cell_n,Mstd(:,:,i),Cell_std)
          write(unit=i_cfl,fmt="(a,3f12.5,3f8.3)") "Cell  ",cell_std%cell,cell_std%ang
        else
          write(unit=i_cfl,fmt="(a)") "!            a           b           c       alpha    beta   gamma"
          write(unit=i_cfl,fmt="(a,3f12.5,3f8.3)") "Cell  ",cell_n%cell,cell_n%ang
        end if
        write(unit=i_cfl,fmt="(a)") "!"
        !if(index(SubGroup(i)%Spg_Symb,"Unknown") /= 0) then
        if(.not. trn_std .and. trim(symb_setting(i)) /= "a,b,c;0,0,0" .or. index(SubGroup(i)%Spg_Symb,"Unknown") /= 0) then
           do j=2,SubGroup(i)%NumOps
             k=index(SubGroup(i)%Symb_Op(j),",1",back=.true.)
             aux_symm=SubGroup(i)%Symb_Op(j)(1:k-1)
             write(unit=i_cfl,fmt="(a)") "GENR  "//aux_symm
           end do
           NumOps=SubGroup(i)%NumOps
           if(SubGroup(i)%centred /= 1) then
             write(unit=i_cfl,fmt="(a)") "GENR  "//trim(SubGroup(i)%Symb_Op(NumOps+1))
             NumOps=NumOps*2
           end if
           if(SubGroup(i)%Num_Lat > 0) then
             k=NumOps+1
             do j=1,SubGroup(i)%Num_Lat
               n=index(SubGroup(i)%Symb_Op(k),",1",back=.true.)
               aux_symm=SubGroup(i)%Symb_Op(k)(1:n-1)
               write(unit=i_cfl,fmt="(a)") "GENR  "//aux_symm
               k=k+Numops
             end do
           end if
        else
           write(unit=i_cfl,fmt="(a,3f12.5,3f8.3)") "SpGR  "//trim(SubGroup(i)%Spg_Symb)
        end if
        write(unit=i_cfl,fmt="(a)") "!"
        write(unit=i_cfl,fmt="(a)") "!    Label        Sfac/Chem        x         y         z       Biso       occ     2*Spin  Q"

        !Construct for each atom in the asymmetric unit of the initial space group the whole list
        !of equivalent points
        na=0
        write(*,"(a,i6,a)") " => Allocating Atom List Asub for ",nauas," atoms"
        call Allocate_Atom_List(nauas,Asub,"atm_type",0)
        call clear_Error()
        do n=1,A%natoms  !loop over atoms in the asymmetric unit of the original group
          Call get_orbit(A%atom(n)%x,SpaceGroup,orb)
          pl%np= orb%mult
          pl%x = orb%pos
          do j=1,pl%np
            write(unit=pl%nam(j),fmt="(a,i5,a)") trim(A%atom(n)%lab)//"(",j,")"
            pl%nam(j)=pack_string(pl%nam(j))
          end do

          call get_transf_list(trans,orig,pl,pl_n)  !Transform the list in SpaceGroup to the list in SPGn
          if(Err_CFML%Ierr == 0) then
             !CALL CPU_TIME(seconds)
             call set_orbits_inlist(SubGroup(i),pl_n)     !Determines the decomposition of orbits w.r.t. SubGroup(i)
             norbi=maxval(pl_n%p(1:pl_n%np))
             !CALL CPU_TIME(end_time)
             !write(unit=*,fmt="(a,f9.3,a)") " =>set_orbits_inlist calculated !, CPU-time: ",end_time-seconds, " seconds"
             if(full_given) then
               write(unit=lun,fmt="(a,i3,a)")     " => Atom number: ",n," of label: "//trim(A%atom(n)%lab)
               write(unit=lun,fmt="(a,2(i3,a))")  "    List of equivalent atoms in the subgroup: ",pl_n%np,&
                                                  " atoms in " ,norbi, " orbits"
              !!--..       A'  = M  A           X'  = inv(Mt) X
              !!--..   If a change of origin is performed the positions are changed
              !!--..   Ot=(o1,o2,o3) origin of the new basis A' w.r.t. old basis A
              !!--..
              !!--..       X' = inv(Mt) (X-O)
              !!--..                                                  " atoms in " ,norbi, " orbits"
               k=0
               do j=1,pl_n%np
                 l=index(pl_n%nam(j),"_")
                 nam=pl_n%nam(j)(1:l)
                 write(unit=nam(l+1:),fmt="(i5,a,i5)") pl_n%p(j),"_",j
                 nam=pack_string(nam)
                 xp=pl_n%x(:,j)
                 if(trn_std .and. len_trim(SubGroup(i)%BNS_Symb) == 0 ) xp=modulo_lat(matmul(invMt(:,:,i),pl_n%x(:,j)-tor(:,i)))
                 write(unit=lun,fmt="(a,3f9.4,i6)")  "    "//nam, xp, pl_n%p(j)
                 if(pl_n%p(j) > k ) then
                   k=k+1
                   if(k <= norbi ) then
                     i1=index(nam,"("); i2=index(nam,")")
                     if(i1 /= 0 .and. i2 /=0 ) then
                        nam(i1:i2) =" "
                     end if
                     nam=pack_string(nam)
                     Call get_orbit(pl_n%x(:,j),SubGroup(i),orb)
                     mult=orb%mult
                     occ=real(mult)/real(SubGroup(i)%Multip)
                     na=na+1
                     !write(*,"(3(a,i4))") "Group:",i," Atom#:",na, "  Max#atoms:",nauas
                     !Asub%Atom(na)=A%atom(n) !it does not compile with gfortran
                     Asub%Atom(na)%ChemSymb=A%atom(n)%ChemSymb
                     Asub%Atom(na)%SfacSymb=A%atom(n)%SfacSymb
                     Asub%Atom(na)%Z=A%atom(n)%Z
                     Asub%Atom(na)%Mult=Mult
                     Asub%Atom(na)%Charge=A%atom(n)%Charge
                     Asub%Atom(na)%U_iso=A%atom(n)%U_iso
                     Asub%Atom(na)%U=A%atom(n)%U
                     Asub%Atom(na)%Mom=A%atom(n)%Mom
                     Asub%Atom(na)%Moment=A%atom(n)%Moment
                     Asub%Atom(na)%Ind_ff=A%atom(n)%Ind_ff
                     Asub%Atom(na)%AtmInfo=A%atom(n)%AtmInfo
                     Asub%Atom(na)%wyck=A%atom(n)%wyck
                     Asub%Atom(na)%VarF=A%atom(n)%VarF

                     Asub%Atom(na)%Lab=nam
                     Asub%Atom(na)%x=xp
                     Asub%Atom(na)%occ=occ
                     Asub%active(na)=.true.
                     write(unit=i_cfl,fmt="(a,5f10.5,f8.3,i4)")  "Atom "//nam//A%atom(n)%SfacSymb, xp,&
                                                              A%atom(n)%U_iso,occ,A%atom(n)%Mom,A%atom(n)%Charge
                   end if
                 end if
               end do

             else

               k=0
               do j=1,pl_n%np
                 if(pl_n%p(j) > k ) then
                   l=index(pl_n%nam(j),"_")
                   nam=pl_n%nam(j)(1:l)
                   write(unit=nam(l+1:),fmt="(i5)") pl_n%p(j)
                   nam=pack_string(nam)
                   xp=pl_n%x(:,j)
                   if(trn_std .and. index(SubGroup(i)%Spg_Symb,"Unknown") == 0) xp=modulo_lat(matmul(invMt(:,:,i),pl_n%x(:,j)-tor(:,i)))
                   write(unit=lun,fmt="(a,3f9.4,i6)")  "    "//nam, xp, pl_n%p(j)
                   i1=index(nam,"("); i2=index(nam,")")
                   if(i1 /= 0 .and. i2 /=0 ) then
                      nam(i1:i2) =" "
                   end if
                   nam=pack_string(nam)
                   Call get_orbit(pl_n%x(:,j),SubGroup(i),orb)
                   mult=orb%mult
                   occ=real(mult)/real(SubGroup(i)%Multip)
                   na=na+1
                   !Asub%Atom(na)=A%atom(n) !it does not compile with gfortran
                   !(1) Error: Nonallocatable variable must not be polymorphic in intrinsic
                   !assignment at (1) - check that there is a matching specific subroutine for '=' operator
                   Asub%Atom(na)%ChemSymb=A%atom(n)%ChemSymb
                   Asub%Atom(na)%SfacSymb=A%atom(n)%SfacSymb
                   Asub%Atom(na)%Z=A%atom(n)%Z
                   Asub%Atom(na)%Mult=Mult
                   Asub%Atom(na)%Charge=A%atom(n)%Charge
                   Asub%Atom(na)%U_iso=A%atom(n)%U_iso
                   Asub%Atom(na)%U=A%atom(n)%U
                   Asub%Atom(na)%Mom=A%atom(n)%Mom
                   Asub%Atom(na)%Moment=A%atom(n)%Moment
                   Asub%Atom(na)%Ind_ff=A%atom(n)%Ind_ff
                   Asub%Atom(na)%AtmInfo=A%atom(n)%AtmInfo
                   Asub%Atom(na)%wyck=A%atom(n)%wyck
                   Asub%Atom(na)%VarF=A%atom(n)%VarF

                   Asub%Atom(na)%Lab=nam
                   Asub%Atom(na)%x=xp
                   Asub%Atom(na)%occ=occ
                   Asub%active(na)=.true.
                   write(unit=i_cfl,fmt="(a,5f10.5,f8.3,i4)") "Atom "//nam//A%atom(n)%SfacSymb, xp,&
                                                              A%atom(n)%U_iso,occ,A%atom(n)%Mom,A%atom(n)%Charge
                   k=k+1
                   if(k > norbi) exit
                 end if
               end do
             end if
          end if

        end do !n atoms
        if((index_given .and. ind_group(i) >  indice) .or. na == 0) cycle
        Asub%natoms=na
        !call Write_Atom_List(Asub)
        if(.not. trn_std) then
          write(unit=i_cfl,fmt="(a)") "!  For transforming to standard setting use:"
          write(unit=i_cfl,fmt="(a)") "!transf  "//trim(symb_setting(i))
          end if
        close(unit=i_cfl)
        if(trn_std) then
          call Write_Cif_Template(trim(texto)//".cif", Cell_std, SubGroup(i), Asub, 2, "CIF file for: "//trim(texto))
        else
          call Write_Cif_Template(trim(texto)//".cif", Cell_n, SubGroup(i), Asub, 2, "CIF file for: "//trim(texto))
        end if

      end do !Subgroups

      if(.not. trans_given) then
        write(unit=*,fmt="(a)",advance="no") " => Another setting change?: "
        read(unit=*,fmt="(a)") ans
        if(ans == "y" .or. ans == "Y") then
          trans_given=.false.
          cycle
        end if
      end if
      exit
    end do

    CALL CPU_TIME(end_time)
    write(unit=*,fmt="(/a,f9.3,a)")  " => Program finished normally, CPU-time: ",end_time-start_time, " seconds"
    write(unit=lun,fmt="(/a,f9.3,a)")  " => Program finished normally, CPU-time: ",end_time-start_time, " seconds"
    write(unit=*,fmt="(a,a)") " => Results in file: ",  trim(outfil)//".coo"

    contains
      Subroutine CloseProgram()
         character(len=1) :: ans
         write(unit=*,fmt="(a)")   " "
         write(unit=*,fmt="(a)")   " => Press <cr> to finish ...."
         read(unit=*,fmt="(a)") ans
         stop
      End Subroutine CloseProgram

  End Program Similar_Obtain_Subgroups
