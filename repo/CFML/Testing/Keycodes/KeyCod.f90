!!----
!!---- KeyCodes Program (For Testing purposes)
!!----
!!----
Program KeyCodes
   !---- Use Modules ----!
   use CFML_GlobalDeps,   only: CP, Err_CFML, clear_error, set_error, CFML_DEBUG,Set_CFML_DEBUG
   Use CFML_Strings,      only: File_Type, U_Case, Cut_String, Get_Words, &
                                Get_Num, Reading_File, l_case, Get_Separator_Pos
   use CFML_gSpaceGroups, only: SpG_Type, Write_SpaceGroup_Info
   use CFML_Metrics,      only: Cell_Type, Cell_G_Type, Cell_LS_Type, Cell_GLS_Type, Write_Crystal_Cell
   use CFML_Atoms,        only: AtList_Type, Atm_type, Atm_Std_Type, Atm_Ref_Type, &
                                ModAtm_Std_Type, ModAtm_Ref_type, Write_Atom_List, &
                                Index_AtLab_on_AtList, Change_AtomList_Type
   use CFML_DiffPatt,      only: Excl_reg_type, Bck_type
   use CFML_KeyCodes
   use CFML_Molecules
   use CFML_IOform
   !---- Variables ----!
   implicit none

   !> Maximum numbers
   integer, parameter :: NB_MAX=100       ! Maximum number of blocks

   integer, parameter :: MAX_EXREG=100    ! Maximum number of excluded regions
   integer, parameter :: MAX_BCKGD= 500   ! Maximum number of backgrounds

   integer, parameter :: NMAX_PHAS =10
   integer, parameter :: NMAX_PATT =10
   integer, parameter :: NMAX_MOLEX=10
   integer, parameter :: NMAX_ATLIS=10

   !> --------------------------
   !> ---- Type definitions ----
   !> --------------------------

   !!----
   !!---- PHASE_TYPE
   !!----
   !!----
   !!
   !Type :: Phase_Type
   !   character(len=:),      allocatable :: phas_name
   !   logical, dimension(:), allocatable :: patt_contrib ! Contribution to patterns
   !   class(Cell_G_Type),    allocatable :: Cell         ! Cell object
   !   class(SpG_Type),       allocatable :: SpG          ! Space Group object
   !   type(Atlist_Type)                  :: Atm          ! Atom List object
   !End Type Phase_Type


   !!----
   !!---- MOLPHASE_TYPE
   !!----
   !!----
   !!
   Type, extends(Phase_Type) :: MolPhase_Type
      integer                                              :: N_Mol=0          ! Number of Molecules
      type(Molecule_type),     allocatable, dimension(  :) :: Molec            ! Molecules
   End type MolPhase_Type


   integer :: NB_Comm  ! Number of Command Block (for the moment only 1)
   integer :: NB_Patt  ! Number of Pattern blocks
   integer :: NB_Phas  ! Number of Phase blocks
   integer :: NC_Patt  ! Number of Pattern blocks into Command Block
   integer :: NC_Phas  ! Number of Phase blocks into command block
   integer :: NB_Mol   ! Number of Molecules blocks

   type(File_type)                         :: Ffile        ! File and lines information

   type(BlockInfo_Type)                    :: Bl_Comm      ! Command block
   type(BlockInfo_Type), dimension(NB_MAX) :: Bl_Patt      ! Pattern blocks
   type(BlockInfo_Type), dimension(NB_MAX) :: Bl_Phas      ! Phase blocks
   type(BlockInfo_Type), dimension(NB_MAX) :: Bl_CommPatt  ! Patterns zone into Command block
   type(BlockInfo_Type), dimension(NB_MAX) :: Bl_CommPhas  ! Phases zone into Command block
   type(BlockInfo_Type), dimension(NB_MAX) :: Bl_Mol       ! Molex zones

   type(GenParList_Type)                       :: VGen     ! General vector constains refinement codes

   type(Phase_Type),      dimension(:), allocatable :: Ph  ! Phases
   type(MolPhase_Type),   dimension(:), allocatable :: MPh ! MolPhase
   Type(Excl_reg_type)    :: Exclr
   Type(Bck_type)         :: Bck

   !---- Variables ----!
   integer :: i, j, k, n_ini, n_end, narg
   integer :: n, nt, nc, lun
   integer, dimension(NB_MAX) :: ph_molcrys=0

   character(len=256) :: filcod
   character(len=2)   :: ans
   character(len=60)  :: Str1, Str2

   real(kind=cp) :: T_ini,T_fin, T_Time

   logical :: arg_given=.false.
   logical :: existe=.false.

   !> Init
   T_Time=0.0
   call Set_CFML_DEBUG(.true.)

   !> Arguments on the command line
   narg=Command_Argument_Count()
   if (narg > 0) then
      call Get_Command_Argument(1, filcod)
      i=index(filcod,'.cfl')
      if (i /= 0) filcod=filcod(1:i-1)
      arg_given=.true.
   end if

   write(unit=*,fmt="(/,/,8(a,/))")                                                           &
              "                      =============================="                        , &
              "                      ====== PROGRAM KEYCODES ======"                        , &
              "                      =============================="                        , &
              "    ***********************************************************************" , &
              "    *   Checking Key Codes for Refinement Procedures using CrysFML2008    *" , &
              "    *                            Reading only .cfl                        *" , &
              "    ***********************************************************************" , &
              "                     (version: May 2023 JGP+NAK+JRC)"
   write(unit=*,fmt=*) " "

   if (.not. arg_given) then
      write(unit=*,fmt="(a)") " => Code of the file xxx.cfl (give xxx): "
      read(unit=*,fmt="(a)") filcod
      if (len_trim(filcod) == 0) then
         write(unit=*,fmt="(a)",advance="no") " => Please, press <cr> to finish the program"
         read(unit=*,fmt="(a)") ans
         stop
      end if

      i=index(filcod,".cfl")
      if (i /= 0) filcod=filcod(1:i-1)
   end if

   inquire(file=trim(filcod)//'.cfl',exist=existe)
   if (.not. existe) then
      write(unit=*,fmt="(a)") " File: "//trim(filcod)//".cfl doesn't exist!"
      stop
   end if

   !> Initialize the clock
   call cpu_time(T_Ini)

   !> Read file CFL
   ffile=reading_file(trim(filcod)//'.cfl')
   if (Err_CFML%Ierr /=0) then
      write(unit=*,fmt="(a)") trim(Err_CFML%Msg)
      write(unit=*,fmt="(/,a)") " => PROGRAM KEYCODES finished in error!"
      stop
   end if

   !> Create a Log File
   if (CFML_DEBUG) then
      open(newunit=lun,file=trim(filcod)//".log", status="replace",action="write")
      write(unit=lun,fmt="(/,/,8(a,/))")                                                        &
           "                      =============================="                        , &
           "                      ====== PROGRAM KEYCODES ======"                        , &
           "                      =============================="                        , &
           "    ***********************************************************************" , &
           "    *   Checking Key Codes for Refinement Procedures using CrysFML2008    *" , &
           "    *                            Reading only .cfl                        *" , &
           "    ***********************************************************************" , &
           "                     (version: June 2023 JGP+NAK+JRC)"
   end if

   !> Determine the differents Blocks existing into de CFL file
   call Get_Blocks_Filetype(ffile, NB_Comm, Bl_Comm, NB_Patt, Bl_Patt, NB_Phas, Bl_Phas, &
                                   NC_Patt, Bl_CommPatt, NC_Phas, Bl_CommPhas )

   if (Err_CFML%IErr /= 0) then
      write(unit=*,fmt="(a)") trim(Err_CFML%Msg)
      write(unit=*,fmt="(/,a)") " => PROGRAM KEYCODES finished in error!"
      stop
   end if

   !> Allocating Vectors
   call Allocate_GPList(500,VGen)   ! Up to 1000 parameters for refinement

   if (allocated(Ph)) deallocate(Ph)
   if (allocated(MPh)) deallocate(MPh)
   nt=max(1,NB_Phas)

   allocate(ph(nt), Mph(nt))

   if (allocated(Vec_Instr)) deallocate(Vec_Instr)
   allocate (Vec_Instr(3*ffile%nlines))

   !if (NB_Patt > 0) then
   !  !if (allocated(Vec_ExReg)) deallocate(Vec_ExReg)
   !  !allocate (Vec_ExReg(MAX_EXREG))
   !
   !   if (allocated(Vec_Backgd)) deallocate(Vec_Backgd)
   !   allocate (Vec_Backgd(MAX_BCKGD))
   !end if

   call Allocate_Restraints_List(Rest_Dis, 50)
   call Allocate_Restraints_List(Rest_Ang, 50)
   call Allocate_Restraints_List(Rest_Tor, 50)

   !> ----------------------
   !> ---- PATTERN ZONE ----
   !> ----------------------
   if (NB_Patt ==0) then
      NB_Patt=1

      Bl_Patt(1)%StrName='PatternDefault'
      Bl_Patt(1)%BlName='PATTERN'
      Bl_Patt(1)%IBl=2
      if (NB_Comm > 0) then
         Bl_Patt(1)%Nl=[1,Bl_Comm%Nl(1)-1]
      else
         Bl_Patt(1)%Nl=[1, ffile%nlines]
      end if
   end if

   do i=1,NB_Patt
      call Read_Block_ExcludeReg(ffile, Bl_Patt(i)%Nl(1), Bl_Patt(i)%Nl(2), Exclr)
      call Read_Block_Backgd(ffile, Bl_Patt(i)%Nl(1), Bl_Patt(i)%Nl(2), Bck)
      call Read_Block_Instructions(ffile,Bl_Patt(i)%Nl(1), Bl_Patt(i)%Nl(2))
   end do

    do i=1,NB_Patt
       call Write_InfoBlock_ExcludedRegions(Exclr,lun)
       call Write_InfoBlock_Backgd(Bck, lun)
    end do
    call write_info_Instructions(1,NP_Instr)

   !> --------------------
   !> ---- PHASE ZONE ----
   !> --------------------
   if (NB_Phas == 0) then
      NB_Phas=1

      Bl_Phas(1)%StrName='PhaseDefault'
      Bl_Patt(1)%BlName='PHASE'
      Bl_Patt(1)%IBl=1
      if (NB_Comm > 0) then
         Bl_Phas(1)%Nl=[1,Bl_Comm%Nl(1)-1]
      else
         Bl_Phas(1)%Nl=[1, ffile%nlines]
      end if
   end if

   !> Molex definitions?
   call Get_SubBlock_MolPhases(ffile, 1, ffile%nlines, NB_Mol, Bl_Mol)
   if (Err_CFML%IErr == 1) then
      write(unit=*,fmt="(a)")  ' => Error reading Molex zone: '//trim(err_CFML%Msg)
      stop
   end if

   do i=1, NB_Phas
   ! Read directives within a phase block

      do j=1,NB_Mol
         if (Bl_Mol(j)%Nl(1) >= Bl_Phas(i)%Nl(1) .and. &
             Bl_Mol(j)%Nl(2) <= Bl_Phas(i)%Nl(2) ) then

             ! N. molecules in the current Phase
             ph_molcrys(i)=ph_molcrys(i)+1

         end if
      end do
   end do

   print*,' ---- Blocks Zone ----'
   print*,' Number of Patterns: ', NB_Patt
   print*,'   Number of Phases: ', NB_Phas
   print*,' '

   do i=1,NB_Phas
      if (ph_molcrys(i) == 0) then
         call Read_XTal_Structure(trim(filcod)//'.cfl',ph(i)%Cell, ph(i)%Spg, ph(i)%Atm_list, IPhase=i)
         if (Err_CFML%IErr == 1) then
            write(unit=*,fmt="(a,i3)")  ' => Error reading phase #',i
            write(unit=*,fmt='(a)') ' => '//trim(err_CFML%Msg)
            stop
         end if
         ph(i)%Atm_list%Iph=i   ! Actualizar
         ph(i)%name=BL_Phas(i)%StrName
   !Type, public :: Phase_Type
   !   type(AtList_Type)                  :: atm_list      ! Atom list
   !   class(Cell_G_Type),    allocatable :: cell          ! Crystal cell
   !   character(len=:),      allocatable :: name          ! Phase name
   !   integer, dimension(:), allocatable :: patterns      ! Patterns to which this phase contributes
   !   real,    dimension(:), allocatable :: scale_factors ! Scale factors for patterns
   !   class(SpG_Type),       allocatable :: spg           ! Space group
   !end type

         call Read_Phase_PattContr(ffile, Bl_Phas(i)%Nl(1), Bl_Phas(i)%Nl(2),Ph(i))
         write(unit=lun,fmt='(/,a)')  '  ==========================='
         write(unit=lun,fmt='(a,i4)') '  Information about Phase',i
         write(unit=lun,fmt='(a,/)')  '  ==========================='
         write(unit=lun,fmt='(a)') " Phase Name: "//Ph(i)%name
         write(unit=lun,fmt='(a,100i3)')   " Contribution   to Patterns: ",Ph(i)%patterns(1:Ph(i)%Ncontr)
         write(unit=lun,fmt='(a,30f14.6)') " Scale Factors for Patterns: ",Ph(i)%scale_factors(1:Ph(i)%Ncontr)
         call Write_SpaceGroup_Info(Ph(i)%SpG,lun)
         call Write_Atom_List(Ph(i)%atm_list,i, Iunit=lun)
         write(unit=lun,fmt='(a)')' '

      else

         if (allocated(MPh(i)%Molec)) deallocate(MPh(i)%Molec)
         allocate(MPh(i)%Molec(ph_molcrys(i)))

         call read_cfl_MolPhase(ffile, Bl_Phas(i)%Nl(1), Bl_Phas(i)%Nl(2),MPh(i))

         Mph(i)%Atm_list%Iph=i

         write(unit=lun,fmt='(/,a)')  '  ====================================='
         write(unit=lun,fmt='(a,i4)') '  Information about Molecular Phase',i
         write(unit=lun,fmt='(a,/)')  '  ====================================='
         call Write_SpaceGroup_Info(MPh(i)%SpG,lun)
         do j=1, ph_molcrys(i)
            call WriteInfo_Molecule(Mph(i)%Molec(j),Lun)
         end do
         call Write_Atom_List(MPh(i)%atm_list,i, Iunit=lun)

         write(unit=lun,fmt='(a)')' '
      end if
   end do

   nt = Np_Instr
   do i=1,NB_Phas
      call Read_Block_Instructions(ffile,Bl_Phas(i)%Nl(1), Bl_Phas(i)%Nl(2),.true.)
   end do
   call write_info_Instructions(nt+1,NP_Instr)

   !> ----------------------
   !> ---- COMMAND ZONE ----
   !> ----------------------
   if (NB_Comm > 0) then
      print*,' ---- Commands Zone ----'
      print*,' Number of Patterns Block in Coomand Zone: ', nc_patt
      print*,'   Number of Phases Block in Command Zone: ', nc_phas
      print*,' '

      !> Check the Atom type in the list
      !> Conversion
      do i=1, NB_Phas
         select type (A => Ph(i)%Atm_list%atom)
            type is (Atm_Type)
               call Change_AtomList_Type(Ph(i)%atm_list, 'Atm_Ref_Type', 0)

            type is (Atm_Std_Type)
               call Change_AtomList_Type(Ph(i)%atm_list, 'Atm_Ref_Type', 0)

            type is (Atm_Ref_Type)
               ! Change no necessary

            type is (ModAtm_Std_Type)
               call Change_AtomList_Type(Ph(i)%atm_list, 'ModAtm_Ref_Type', 0)

            type is (ModAtm_Ref_Type)
               ! Change no necessary
         end select
      end do

      !> -------------------------
      !> Read RefCodes of Patterns
      !> -------------------------
      if (NC_Patt ==0) then
         call Read_RefCodes_PATT(ffile, Bl_Comm%Nl(1)+1, Bl_Comm%Nl(2)-1, 1, VGen)

      else
         do i=1,NC_Patt
            Str1=u_case(Bl_CommPatt(i)%StrName)
            k=0
            do j=1,NB_Patt
               Str2=u_case(Bl_Patt(j)%StrName)
               if (trim(str1) /= trim(str2)) cycle
               k=j
               exit
            end do

            call Read_RefCodes_PATT(ffile, Bl_CommPatt(i)%Nl(1)+1, &
                                           Bl_CommPatt(i)%Nl(2)-1, k, VGen)
         end do
      end if

      !> -----------------------
      !> Read RefCodes of Phases
      !> -----------------------
      if (NC_Phas ==0) then
         if (ph_molcrys(1) ==0) then
            !> Refinable parameters
            call Read_RefCodes_PHAS(ffile, Bl_Comm%Nl(1)+1, Bl_Comm%Nl(2)-1, 1, &
                                    Ph(1)%Spg, Ph(1)%Cell, Ph(1)%atm_list, VGen)

            !> Restraints parameters
            call Read_Restraints_PHAS(ffile, Bl_Comm%Nl(1)+1, Bl_Comm%Nl(2)-1, 1, &
                                      Ph(1)%atm_list, Rest_Dis, Rest_Ang, Rest_Tor )
         else
            !> Refinable parameters
            call Read_RefCodes_PHAS(ffile, Bl_Comm%Nl(1)+1, Bl_Comm%Nl(2)-1, 1, &
                                    MPh(1)%Spg, MPh(1)%Cell, MPh(1)%atm_list, VGen)

            call Read_RefCodes_MOL(ffile, Bl_Comm%Nl(1)+1, Bl_Comm%Nl(2)-1, 1, &
                                   MPh(1)%N_Mol, MPh(1)%Molec, VGen)

            !> Restraints parameters
         end if


      else
         do i=1, NC_Phas
            Str1=u_case(Bl_CommPhas(i)%StrName)
            k=0
            do j=1,NB_Phas
               Str2=u_case(Bl_Phas(j)%StrName)
               if (trim(str1) /= trim(str2)) cycle
               k=j
               exit
            end do

            if (ph_molcrys(k) == 0) then

               !> Refinable parameters
               call Read_RefCodes_PHAS(ffile, Bl_CommPhas(i)%Nl(1)+1,    &
                                              Bl_CommPhas(i)%Nl(2)-1, k, &
                                              Ph(k)%Spg, Ph(k)%Cell, Ph(k)%atm_list, VGen)

               !> Restraints parameters
               call Read_Restraints_PHAS(ffile, Bl_CommPhas(i)%Nl(1)+1,               &
                                                Bl_CommPhas(i)%Nl(2)-1, k, Ph(k)%atm_list, &
                                                Rest_Dis, Rest_Ang, Rest_Tor )
            else

               !> Refinable parameters
               call Read_RefCodes_PHAS(ffile, Bl_CommPhas(i)%Nl(1)+1,    &
                                              Bl_CommPhas(i)%Nl(2)-1, k, &
                                              MPh(k)%Spg, MPh(k)%Cell, MPh(k)%atm_list, VGen)

               call Read_RefCodes_MOL(ffile, Bl_CommPhas(i)%Nl(1)+1, &
                                      Bl_CommPhas(i)%Nl(2)-1, k, &
                                      MPh(k)%N_Mol, MPh(k)%Molec, VGen)

               !> Restraints parameters
               call Read_Restraints_PHAS(ffile, Bl_CommPhas(i)%Nl(1)+1,               &
                                                Bl_CommPhas(i)%Nl(2)-1, k, MPh(k)%atm_list, &
                                                Rest_Dis, Rest_Ang, Rest_Tor )
            end if
         end do
      end if

      call WriteInfo_GPList(VGen)
      do i=1, NC_Phas
         if (ph_molcrys(i) == 0) then
            call WriteInfo_Restraints(Rest_Dis, Rest_Ang, Rest_Tor, i, Ph(i)%atm_list)
         else
            call WriteInfo_Restraints(Rest_Dis, Rest_Ang, Rest_Tor, i, MPh(i)%atm_list)
         end if
      end do

   end if ! NB_Comm

   !> ---- End Program ----
   call cpu_time(T_fin)
   T_fin=T_fin-T_ini
   T_time=T_time + T_fin

   T_ini=T_fin/60.0
   T_fin=int(T_ini)
   T_ini=(T_ini-T_fin)*60.0

   write(unit=*,fmt='(a,i3,a,f8.4,a)')    " => CPU-time: ",nint(T_fin)," minutes",T_ini," seconds"
   T_ini=T_time/60.0
   T_fin=int(T_ini)
   T_ini=(T_ini-T_fin)*60.0
   write(unit=*,fmt='(a,i3,a,f8.4,a)')       " => TOTAL CPU-time: ",nint(T_fin)," minutes",T_ini," seconds"

   if (CFML_DEBUG) then
      write(unit=lun,fmt='(/,a)')            " => Normal End of: PROGRAM KEYCODES "
      write(unit=lun,fmt='(a,i3,a,f8.4,a)')  " => CPU-time: ",nint(T_fin)," minutes",T_ini," seconds"
      close(unit=lun)
   end if


   Contains

   Subroutine Read_Phase_PattContr(ffile,N_ini,N_end,Phase)
      Type(file_type),         intent(in)       :: ffile
      integer,                 intent(in)       :: n_ini
      integer,                 intent(in)       :: n_end
      Type(Phase_Type),        intent(in out)   :: Phase

      !---- Local Variables ----!
      integer                       :: i, j, iv
      character(len=:),allocatable  :: line
      logical                       :: contr_pat, scale_pat
      integer,        dimension(15) :: ivet
      real(kind=cp),  dimension(15) :: vet


      !> Init
      call clear_error()

      Phase%Ncontr=0; contr_pat=.false.; scale_pat=.false.
      vet=0.0_cp; ivet=0
      if(allocated(Phase%patterns)) deallocate(Phase%patterns)
      if(allocated(Phase%scale_factors)) deallocate(Phase%scale_factors)

      do i=N_ini,N_end
         line=l_case(adjustl(ffile%line(i)%str))
         if (len_trim(line) == 0) cycle
         if (line(1:1) =="!") cycle
         j=index(line,'!')
         if (j > 0) line=line(:j-1)
         j=index(line,'#')
         if (j > 0) line=line(:j-1)

         j=index(line,'patterns')
         if(j /= 0) then
         	 line=line(j+8:)
         	 call Get_Num(line, vet, ivet, iv)
         	 if(iv == 0) exit
         	 allocate(Phase%patterns(iv))
         	 Phase%patterns=vet(1:iv)
         	 Phase%Ncontr=iv
         	 contr_pat=.true.
         	 cycle
         end if
         j=index(line,"scale_factors")
         if(j /= 0) then
         	 line=line(j+13:)
         	 call Get_Num(line, vet, ivet, iv)
         	 if(iv == 0) exit
         	 allocate(Phase%scale_factors(iv))
         	 Phase%scale_factors=vet(1:iv)
         	 scale_pat=.true.
         	 cycle
         end if
      end do
      if (.not. scale_pat .and. contr_pat) then
      	 allocate(Phase%scale_factors(Phase%Ncontr))
         Phase%scale_factors=1.0
      end if
   End Subroutine Read_Phase_PattContr

   Subroutine Write_Info_Instructions(N_ini,N_end, Iunit)
    !---- Arguments ----!
      integer,           intent(in) :: N_ini
      integer,           intent(in) :: N_end
      integer, optional, intent(in) :: Iunit

      !---- Local Variables ----!
      integer :: i,j, lun
      integer :: n1, n2

      !> Init
      lun=6
      if (present(iunit)) lun=iunit

      !> Check
      n1=n_ini
      if (n1 < 0 .or. n1 > NP_Instr) n1=1

      n2=min(n_end, NP_Instr)
      if (n2 <= 0) return

      write(unit=lun,fmt='(a)')' '
      do i=n1, n2
         write(unit=lun, fmt='(a)') '       Directive: '//trim(Vec_Instr(i)%Str)
         write(unit=lun, fmt='(a, i2)') ' Num. Parameters: ', Vec_Instr(i)%Npar
         do j=1, Vec_Instr(i)%Npar
            write(unit=lun,fmt='(i8,3x,f12.4,3x,t35,a)') Vec_Instr(i)%IV(j), Vec_Instr(i)%RV(j), trim(Vec_Instr(i)%CV(j))
         end do
         write(unit=lun,fmt='(a)')' '
         write(unit=lun,fmt='(a)')' '
      end do

   End Subroutine Write_Info_Instructions

   !!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!!
   !!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!!
   !!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!!
   !!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>!!
   Subroutine Read_CFL_MolPhase(cfl, n_ini, n_end, M)
      !---- Arguments ----!
      type(File_Type),       intent(in)     :: cfl
      integer,               intent(in)     :: n_ini
      integer,               intent(in)     :: n_end
      type(MolPhase_Type),   intent(in out) :: M

      !---- Local varibles ----!
      character(len=80) :: line
      integer           :: i, natm, nmol, k, n
      type(BlockInfo_Type), dimension(10) :: Bl
      type(Molecule_type)                 :: Mol

      !> Init
      call clear_error()

      !> Cell parameters
      call read_cfl_cell(cfl, M%Cell, i_ini=n_ini,i_end=n_end, cmd=.true.)
      if (Err_CFML%IErr == 1) return

      !> Space group
      call read_CFL_SpG(cfl,M%SpG, i_ini=n_ini, i_end=n_end)
      if (Err_CFML%IErr==1) return

      !> Molecules
      call Get_SubBlock_MolPhases(cfl, n_ini, n_end, NMol, Bl)
      do i=1, nmol
         k=Bl(i)%iex(1)
         if (k ==0) k=i
         call Read_CFL_Molecule(cfl, Bl(i)%Nl(1), Bl(i)%Nl(2), M%Molec(k))
      end do
      M%N_Mol=nmol

      !> Free atoms
      call Read_CFL_Atoms(cfl, M%atm_list, 'Atm_Ref_Type', 0, n_ini, n_end)

   End Subroutine Read_CFL_MolPhase


End Program KeyCodes

