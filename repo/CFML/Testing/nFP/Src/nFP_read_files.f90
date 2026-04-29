Module nFP_read_files

   use nFP_globals
   !---- Use Modules ----!
   use CFML_GlobalDeps,   only: CP, Err_CFML, clear_error, set_error,CFML_DEBUG, set_CFML_DEBUG
   Use CFML_Strings,      only: File_Type, U_Case, Cut_String, Get_Words, &
                                Get_Num, Reading_File, l_case, Get_Separator_Pos
   use CFML_gSpaceGroups, only: SpG_Type, Write_SpaceGroup_Info
   use CFML_Metrics,      only: Cell_Type, Cell_G_Type, Cell_LS_Type, Cell_GLS_Type, Write_Crystal_Cell
   use CFML_Atoms,        only: AtList_Type, Atm_type, Atm_Std_Type, Atm_Ref_Type, &
                                ModAtm_Std_Type, ModAtm_Ref_type, Write_Atom_List, &
                                Index_AtLab_on_AtList, Change_AtomList_Type
   use CFML_DiffPatt,     only: Calc_BackGround_Chebychev, DiffPat_E_Type, Calc_BackGround_pVoigt, &
                                Calc_BackGround_split_pVoigt
   use CFML_Rational
   use CFML_IOForm
   use CFML_KeyCodes
   use CFML_Molecules
   use CFML_Powder

   implicit none

   Public

   Public :: nFP_read_CIFS, nFP_read_CFL

   !> Maximum numbers
   integer, parameter :: NB_MAX=100       ! Maximum number of blocks
   integer, parameter :: MAX_EXREG=100    ! Maximum number of excluded regions
   integer, parameter :: MAX_BCKGD= 500   ! Maximum number of backgrounds
   integer, parameter :: NMAX_PHAS =20
   integer, parameter :: NMAX_PATT =10
   integer, parameter :: NMAX_ATLIS=20
   integer, parameter :: NMAX_MOLEX=10


   integer :: NB_Comm  ! Number of Command Block (for the moment only 1)
   integer :: NB_Patt  ! Number of Pattern blocks
   integer :: NB_Phas  ! Number of Phase blocks
   integer :: NC_Patt  ! Number of Pattern blocks into Command Block
   integer :: NC_Phas  ! Number of Phase blocks into command block
   integer :: NB_Mol   ! Number of Molecules blocks

   integer,          dimension(NMAX_MOLEX) :: ph_molcrys   ! Number of molecules
   type(File_type)                         :: Ffile        ! File and lines information
   type(BlockInfo_Type)                    :: Bl_Comm      ! Command block
   type(BlockInfo_Type), dimension(NB_MAX) :: Bl_Patt      ! Pattern blocks
   type(BlockInfo_Type), dimension(NB_MAX) :: Bl_Phas      ! Phase blocks
   type(BlockInfo_Type), dimension(NB_MAX) :: Bl_CommPatt  ! Patterns zone into Command block
   type(BlockInfo_Type), dimension(NB_MAX) :: Bl_CommPhas  ! Phases zone into Command block
   type(BlockInfo_Type), dimension(NB_MAX) :: Bl_Mol       ! Molex zones

  contains

  !> Subroutine nFP_read_CIFS(cif_files, N_cifs)
  !>   character(len=*), dimension(N_cifs), intent(in) :: cif_files
  !>
  !> This subroutine reads a list of cifs files and convert them
  !> to Ph objects that will be dimensioned to Ph(N_cifs). The
  !> names of the CIF files may be provided in a CFL file
  !>
  Subroutine nFP_read_CIFS(buff_cif)
    character(len=*), intent(in) :: buff_cif !Buffer containing cif files
    if(len_trim(buff_cif) == 0) return
  End Subroutine nFP_read_CIFS

  Subroutine nFP_read_CFL(cfl_file,lun)
    character(len=*), intent(in) :: cfl_file
    integer,          intent(in) :: lun
    !--- Local Variables
    integer                       :: i, j, k, n
    integer                       :: nt, nc, np
    character(len=60)             :: Str1, Str2
    class(irf_type), allocatable  :: CWTF_irf
    real(kind=cp)                 :: xmin,xmax

    call clear_error()

    !> Read file CFL
    ffile=reading_file(cfl_file)
    if (Err_CFML%Ierr /=0) then
       write(unit=*,fmt="(a)") trim(Err_CFML%Msg)
       write(unit=*,fmt="(/,a)") " => Subroutine nFP_read_CFL/reading_file finished in error!"
       stop
    end if

    !> Create a Log File
    if (CFML_DEBUG) then
       write(unit=lun,fmt="(/,/,8(a,/))")                                                        &
            "                      =============================="                        , &
            "                   ====== Subroutine nFP_read_CFL ====="                     , &
            "                      =============================="                        , &
            "    ***********************************************************************" , &
            "    *   Checking Key Codes for Refinement Procedures using CrysFML2008    *" , &
            "    *                            Reading only .cfl                        *" , &
            "    ***********************************************************************" , &
            "                     (version: May 2025 JGP+NAK+JRC)"
    end if

    !> Determine the differents Blocks existing into de CFL file
    call Get_Blocks_Filetype(ffile, NB_Comm, Bl_Comm, NB_Patt, Bl_Patt, NB_Phas, Bl_Phas, &
                                    NC_Patt, Bl_CommPatt, NC_Phas, Bl_CommPhas )

    if (Err_CFML%IErr /= 0) then
       write(unit=*,fmt="(a)") trim(Err_CFML%Msg)
       write(unit=*,fmt="(/,a)") " => Subroutine nFP_read_CFL/Get_Blocks_Filetype finished in error!"
       return
    end if
    if (CFML_DEBUG) then
      write(lun,"(a, i3)") " NB_Comm:", NB_Comm
      write(lun,"(a, i3)") " NB_Patt:", NB_Patt
      write(lun,"(a, i3)") " NB_Phas:", NB_Phas
    end if
    !> Allocating Vectors
    call Allocate_GPList(500,VGen)   ! Up to 1000 parameters for refinement

    if (allocated(Ph))  deallocate(Ph)
    N_phases=max(1,NB_Phas)

    allocate(Ph(N_phases))

    if (allocated(Vec_Instr)) deallocate(Vec_Instr)
    allocate (Vec_Instr(3*ffile%nlines))

    call Allocate_Restraints_List(Rest_Dis, 50)
    call Allocate_Restraints_List(Rest_Ang, 50)
    call Allocate_Restraints_List(Rest_Tor, 50)

    !> ----------------------
    !> ---- PATTERN ZONE ----
    !> ----------------------
    if (NB_Patt == 0) then
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

    allocate(Pat(NB_Patt)); allocate(Bck(NB_Patt)); allocate(Excl(NB_Patt))
    allocate(gen_pat(NB_Patt))
    N_CW_patt=0; N_TOF_patt=0
    gen_pat=.false.

    do i=1,NB_Patt
       call Read_Block_ExcludeReg(ffile, Bl_Patt(i)%Nl(1), Bl_Patt(i)%Nl(2), Excl(i))
       if (Err_CFML%IErr == 1) then
          write(unit=*,fmt="(a,i3)")  ' => Error reading Excluded Regions: '//trim(err_CFML%Msg)//" => Pattern",i
          stop
       end if
       !call Read_Block_Backgd(ffile, Bl_Patt(i)%Nl(1), Bl_Patt(i)%Nl(2), i)
       call Read_Block_Backgd(ffile, Bl_Patt(i)%Nl(1), Bl_Patt(i)%Nl(2), Bck(i),i)
       if (Err_CFML%IErr == 1) then
          write(unit=*,fmt="(a,i3)")  ' => Error reading Background: '//trim(err_CFML%Msg)//" => Pattern",i
          stop
       end if
       call Read_Block_Instructions(ffile,Bl_Patt(i)%Nl(1), Bl_Patt(i)%Nl(2))
       if (Err_CFML%IErr == 1) then
          write(unit=*,fmt="(a,i3)")  ' => Error reading Instructions: '//trim(err_CFML%Msg)//" => Pattern",i
          stop
       end if
       !Reading and constructing Pat
       call Read_CFL_Pattern(ffile, Pat(i), Bl_Patt(i)%Nl(1), Bl_Patt(i)%Nl(2),gen_pat(i))
       if (Err_CFML%IErr == 1 .or. Err_CFML%flag) then
          write(unit=*,fmt="(a)")  ' => Error reading/loading Pattern: '//trim(err_CFML%Msg)//" => Pattern",i
          stop
       end if
       Pat(i)%irf=0
       if(Pat(i)%mode == "CW") N_CW_patt=N_CW_patt+1
       if(Pat(i)%mode == "TF") N_TOF_patt=N_TOF_patt+1
    end do

    allocate(CW_irf(N_CW_patt)); allocate(tof_irf(N_TOF_patt))
    nt=0; nc=0
    do i=1,NB_Patt
       ! Reading IRF file of the pattern

       if(Pat(i)%mode == "CW") then
          nc=nc+1
          call Read_Patt_IRF(ffile,Bl_Patt(i)%Nl(1), Bl_Patt(i)%Nl(2),"CW",CWTF_irf,i)
          if (Err_CFML%IErr == 1) then
             write(unit=*,fmt="(a,i3)")  ' => Error reading IRF file: '//trim(err_CFML%Msg)//" => Pattern",i
             stop
          end if
          Select Type(CWTF_irf)
             Type is (IRF_CW_type)
               cw_irf(nc)=CWTF_irf
               Pat(i)%irf=nc  !Number of the CW IRF file to be used with pattern i
          End Select
       end if

       if(Pat(i)%mode == "TF") then
          nt=nt+1
          call Read_Patt_IRF(ffile,Bl_Patt(i)%Nl(1), Bl_Patt(i)%Nl(2),"TF",CWTF_irf,i)
          if (Err_CFML%IErr == 1) then
             write(unit=*,fmt="(a,i3)")  ' => Error reading IRF file: '//trim(err_CFML%Msg)//" => Pattern",i
             stop
          end if
          Select Type(CWTF_irf)
             Type is (IRF_TOF_type)
               tof_irf(nt)=CWTF_irf
               Pat(i)%irf=nt !Number of the Tof IRF file to be used with pattern i
          End Select
       end if

    end do

    N_patterns=NB_Patt
    !Allocation of reliability indices
    allocate(calc_typ(N_phases,N_patterns))
    allocate( R_Bragg(N_phases,N_patterns))
    allocate(  R_Fact(N_phases,N_patterns))
    allocate(   R_Mag(N_phases,N_patterns))

    allocate(Chi_sqr(N_patterns))
    allocate( R_patt(N_patterns))
    allocate(R_wpatt(N_patterns))
    allocate(  R_exp(N_patterns))

     do i=1,NB_Patt
        if(Pat(i)%sample /= "P") cycle
        !write(*,"(a,i3)") " Writing IRF file for patter #",i
        if(Pat(i)%irf > 0) then

          if(Pat(i)%mode == "CW") then
            do j=1,N_CW_patt
               if(cw_irf(j)%Num_Patt == i) then
                  call Write_IRF(cw_irf(j),lun)
                  Pat(i)%Pdat%wave(1:2)=cw_irf(j)%lambda
                  Pat(i)%Pdat%wave(3)  =cw_irf(j)%ratio
                  exit
               end if
            end do
          end if

          if(Pat(i)%mode == "TF") then
            do j=1,N_TOF_patt
               if(tof_irf(j)%Num_Patt == i) then
                  call Write_IRF(tof_irf(j),lun)
                  Pat(i)%Pdat%wave(1)=tof_irf(j)%dtt1_i
                  Pat(i)%Pdat%wave(2)=tof_irf(j)%dtt2_i
                  Pat(i)%Pdat%wave(3)=tof_irf(j)%dtt_1overD_i
                  exit
               end if
            end do
          end if
        end if  ! Pat(i)%irf

        call Write_CFL_Pattern(Lun,Pat(i),i)

        if(Excl(i)%num_excl > 0) then
           call Write_InfoBlock_ExcludedRegions(Excl(i),lun,i)
        end if
        call Write_InfoBlock_Backgd(Bck(i),lun,i)
        ! Calculation of the background
        !write(*,*) " => Calculation of smooth background ..."
        Select Case(u_case(Bck(i)%Funct_typ))
          Case("CHEBYCHEV")
            Select Type(pt => Pat(i)%Pdat)
             class is (DiffPat_E_Type)
                xmin=pt%xmin !max(Excl(i)%Exc(1)%maxb,pt%xmin)
                xmax=pt%xmax
                call Calc_BackGround_Chebychev(pt, Bck(i)%Num_FPar, Bck(i)%Fpar, Xmin, Xmax)
            End Select
        End Select
        !write(*,*) " => Calculation of peaked background ..."
        Select Case(u_case(Bck(i)%Peak_typ))
          Case("PSEUDO-VOIGT")
            Select Type(pt => Pat(i)%Pdat)
             class is (DiffPat_E_Type)
                call Calc_BackGround_pVoigt(pt, Bck(i)%Num_peaks, Bck(i)%pPar)
            End Select
          Case("SPLIT-PSEUDO-VOIGT")
            Select Type(pt => Pat(i)%Pdat)
             class is (DiffPat_E_Type)
                call Calc_BackGround_split_pVoigt(pt, Bck(i)%Num_peaks, Bck(i)%pPar)
            End Select
        End Select
     end do

     !call write_info_Instructions(1,NP_Instr)

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

    if(allocated(prph)) deallocate(prph)
    allocate(prph(NB_Phas))

    do i=1, NB_Phas
      ! Look for molecules in the different phases
      !> Molex definitions?
      call clear_error()
      write(*,"(a,2i4)") " Calling Get_SubBlock_MolPhases with: ",Bl_Phas(i)%Nl
      call Get_SubBlock_MolPhases(ffile, Bl_Phas(i)%Nl(1), Bl_Phas(i)%Nl(2), NB_Mol, Bl_Mol)
      if (Err_CFML%IErr == 1) then
         write(unit=*,fmt="(a)")  ' => Error reading Molex zone: '//trim(err_CFML%Msg)
         stop
      end if
      ph_molcrys(i)=NB_Mol
    end do

    print*,'  ---- Blocks Zone ----'
    print*,'  Number of Patterns: ', NB_Patt
    print*,'    Number of Phases: ', NB_Phas
    print*,' Number of molecules: ', NB_Mol

    if(allocated(rot_mats)) deallocate(rot_mats) !Rotational parts of conventional
    allocate(rot_mats(NB_Phas))                  !symmetry operators

    do i=1,NB_Phas
       call Read_XTal_Structure(cfl_file,ph(i)%Cell, ph(i)%Spg, ph(i)%Atm_list, IPhase=i)
       if (Err_CFML%IErr == 1) then
          write(unit=*,fmt="(a,i3)")  ' => Error reading phase #',i
          write(unit=*,fmt='(a)') ' => '//trim(err_CFML%Msg)
          stop
       end if
       ph(i)%Atm_list%Iph=i   ! Actualizar
       ph(i)%name=BL_Phas(i)%StrName

       !Storing rotational matrices
       rot_mats(i)%numops=ph(i)%Spg%Numops
       if(allocated(rot_mats(i)%rot)) deallocate(rot_mats(i)%rot)
       allocate(rot_mats(i)%rot(3,3,ph(i)%Spg%Numops))
       do n=1,ph(i)%Spg%Numops
         rot_mats(i)%rot(:,:,n)=ph(i)%Spg%Op(n)%Mat(1:3,1:3)
       end do

       if(ph_molcrys(i) > 0) then
          ph(i)%nmol=ph_molcrys(i)
          call Read_CFL_Molecules(ffile,ph_molcrys(i),Bl_Phas(i)%Nl(1), Bl_Phas(i)%Nl(2),Ph(i))
          if (Err_CFML%IErr == 1) then
             write(unit=*,fmt="(a,i3)")  ' => Error reading molecules of phase #',i
             write(unit=*,fmt='(a)') ' => '//trim(err_CFML%Msg)
             stop
          end if
          write(unit=lun,fmt='(a)')' '
       end if

       call Read_Phase_PattContr(ffile, Bl_Phas(i)%Nl(1), Bl_Phas(i)%Nl(2),Ph(i))
       nc=Ph(i)%Ncontr
       if(allocated(prph(i)%prc)) deallocate(prph(i)%prc) !Profiles of each phase and each pattern
       allocate(prph(i)%prc(nc))  !For Phase i, Ph(i)%Ncontr profiles are stored

       do j=1,Ph(i)%Ncontr
         k=Ph(i)%patterns(j)
         if(Pat(k)%sample == "P") then
           np=Pat(k)%Pdat%npts
           if(allocated(prph(i)%prc(j)%yp)) deallocate(prph(i)%prc(j)%yp)
           allocate(prph(i)%prc(j)%yp(np))
           prph(i)%prc(j)%n_pts=np
         end if
       end do
       write(unit=lun,fmt='(/,a)')  '  ==========================='
       write(unit=lun,fmt='(a,i4)') '  Information about Phase',i
       write(unit=lun,fmt='(a,/)')  '  ==========================='
       write(unit=lun,fmt='(a)') " Phase Name: "//Ph(i)%name
       write(unit=lun,fmt='(a,100i3)')   " Contribution   to Patterns: ",Ph(i)%patterns(1:nc)
       write(unit=lun,fmt='(a,30a3)')    "  Sample state for Patterns: ",Ph(i)%pat_sample(1:nc)
       write(unit=lun,fmt='(a,30a3)')    "           Mode of Patterns: ",Ph(i)%pat_mode(1:nc)
       write(unit=lun,fmt='(a,30g14.6)') " Scale Factors for Patterns: ",Ph(i)%scale_factors(1:nc)

       !call Write_SpaceGroup_Info(Ph(i)%SpG,lun)
       !call Write_Atom_List(Ph(i)%atm_list,i, Iunit=lun)


       if(Ph(i)%Nmol > 0) then
         write(unit=lun,fmt='(/,a)')  '  ====================================='
         write(unit=lun,fmt='(a,i4)') '  Information about Molecules in Phase ',i
         write(unit=lun,fmt='(a,/)')  '  ====================================='
         do j=1, Ph(i)%Nmol
            call WriteInfo_Molecule(Ph(i)%Mol(j),Lun)
         end do
       end if
       write(unit=lun,fmt='(a)')' '
       !Read now powder and single crystal attributes
       Call Read_Phase_PowSxt_Attributes(ffile,Bl_Phas(i)%Nl(1), Bl_Phas(i)%Nl(2),Ph(i)%pat_mode(1:nc),Ph(i)%pat_sample(1:nc),Ph(i))

       call Write_CFL_Phase(Lun,Ph(i),i)

    end do

    nt = Np_Instr
    do i=1,NB_Phas
       call Read_Block_Instructions(ffile,Bl_Phas(i)%Nl(1), Bl_Phas(i)%Nl(2),.true.)
    end do
   !call write_info_Instructions(nt+1,NP_Instr)

    !> ----------------------
    !> ---- COMMAND ZONE ----
    !> ----------------------
    if (NB_Comm > 0) then
       print*,' ---- Commands Zone ----'
       print*,' Number of Patterns Block in Command Zone: ', nc_patt
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
       if (NC_Patt == 0) then
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
       if (NC_Phas == 0) then
             !> Refinable parameters
             call Read_RefCodes_PHAS(ffile, Bl_Comm%Nl(1)+1, Bl_Comm%Nl(2)-1, 1, &
                                     Ph(1)%Spg, Ph(1)%Cell, Ph(1)%atm_list, VGen)

             !> Restraints parameters
             call Read_Restraints_PHAS(ffile, Bl_Comm%Nl(1)+1, Bl_Comm%Nl(2)-1, 1, &
                                       Ph(1)%atm_list, Rest_Dis, Rest_Ang, Rest_Tor )
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

                !> Refinable parameters
                call Read_RefCodes_PHAS(ffile, Bl_CommPhas(i)%Nl(1)+1,    &
                                               Bl_CommPhas(i)%Nl(2)-1, k, &
                                               Ph(k)%Spg, Ph(k)%Cell, Ph(k)%atm_list, VGen)

                !> Restraints parameters
                call Read_Restraints_PHAS(ffile, Bl_CommPhas(i)%Nl(1)+1,               &
                                                 Bl_CommPhas(i)%Nl(2)-1, k, Ph(k)%atm_list, &
                                                 Rest_Dis, Rest_Ang, Rest_Tor )
          end do
       end if

       call WriteInfo_GPList(VGen,lun)
       do i=1, NC_Phas
          call WriteInfo_Restraints(Rest_Dis, Rest_Ang, Rest_Tor, i, Ph(i)%atm_list)
       end do

    end if ! NB_Comm
  End Subroutine nFP_read_CFL

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
     if(allocated(Phase%pat_mode)) deallocate(Phase%pat_mode)
     if(allocated(Phase%pat_sample)) deallocate(Phase%pat_sample)
     if(allocated(Phase%scale_factors)) deallocate(Phase%scale_factors)

     do i=N_ini,N_end
        if(contr_pat .and. scale_pat) exit
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
        	 if(iv == 0) then
        	   call Set_Error(1,"At least 1 pattern should be specified after PATTERNS keyword")
        	   return
        	 end if
        	 allocate(Phase%patterns(iv),Phase%pat_mode(iv),Phase%pat_sample(iv),Phase%pow(iv),Phase%sxt(iv))
        	 Phase%patterns=vet(1:iv)
        	 Phase%pat_mode=" "
        	 Phase%pat_sample=" "
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

  Subroutine Read_Phase_PowSxt_Attributes(ffile,n_ini,n_end,modes,samples,Phase)
     Type(file_type),                intent(in) :: ffile
     integer,                        intent(in) :: n_ini
     integer,                        intent(in) :: n_end
     character(len=*), dimension(:), intent(in) :: modes   !Array containing the Mode component of all the patterns
     character(len=*), dimension(:), intent(in) :: samples !Array containing the Sample component of all the patterns
     Type(Phase_Type),           intent(in out) :: Phase
     !---- Local Variables ----!
     integer                          :: i, j, k, L, iv, n_pat, ier, ndir, ini
     character(len=:),allocatable     :: line, keyv,num_line,char_line
     !logical                          :: contr_pat, scale_pat
     integer,           dimension(15) :: ivet
     real(kind=cp),     dimension(15) :: vet
     !real(kind=cp),     dimension(3)  :: axis
     real(kind=cp)                    :: mod2ax
     character(len=30), dimension(15) :: dire

     !Complete Phase type. It is assumed that the Pat array has been read before
     Phase%n_pow=0; Phase%n_sxt=0
     do j=1,Phase%Ncontr
       k=Phase%patterns(j)
       if(samples(k) == "P") Phase%n_pow=Phase%n_pow+1
       if(samples(k) == "X") Phase%n_sxt=Phase%n_sxt+1
       Phase%pat_mode(j)=modes(k)
       Phase%pat_sample(j)=samples(k)
     end do
     if(Phase%n_pow > 0) then
       allocate(Phase%Pow(Phase%n_pow))
     end if
     if(Phase%n_sxt > 0) then
       allocate(Phase%Sxt(Phase%n_sxt))
     end if
     !Read now the attributes for each pattern
     do i=N_ini,N_end
        line=l_case(adjustl(ffile%line(i)%str))
        if (len_trim(line) == 0) cycle
        if (line(1:1) =="!") cycle
        j=index(line,'!')
        if (j > 0) line=line(:j-1)
        j=index(line,'#')
        if (j > 0) line=line(:j-1)
        ! Isotropic size and strain
        j=0
        if(line(1:10) == "ph_pattern") then
          read(line(11:),*,iostat=ier) n_pat
          if(ier /= 0) then
        	   call Set_Error(1,"The number of the pattern should be specified after PH_PATTERN keyword")
        	   return
          end if
          if(samples(n_pat) == "P") then
             Phase%Pow(n_pat)%powder=.true.
             do
                j=j+1
                if(i+j > n_end) exit
                char_line=adjustl(ffile%line(i+j)%str)
                keyv=trim(l_case(char_line))
                if(len_trim(keyv) == 0) cycle
                if(keyv(1:1) == "!") cycle
                L=min(index(keyv," "),len_trim(keyv)+1)
                Select Case(keyv(1:L-1))
                  Case("calc_type")
                    calc_typ(Phase%iph,n_pat)=adjustl(char_line(L:))
                  Case("iso_size")
                      num_line=keyv(L:)
        	            call Get_Num(num_line, vet, ivet, iv)
        	            if(iv == 0) then
        	              write(unit=*,fmt="(a)")  " WARNING: At least 1 number (normally two!) should be specified after the ISO_SIZE keyword"
        	              write(unit=*,fmt="(a)")  " ISO_SIZE keyword ignored"
        	            else if(iv == 1) then
        	               Phase%Pow(n_pat)%iso_size=vet(1)
        	               Phase%Pow(n_pat)%Gauss_iso_size_frac=0.0
        	            else
        	               Phase%Pow(n_pat)%iso_size=vet(1)
        	               Phase%Pow(n_pat)%Gauss_iso_size_frac=vet(2)
        	            end if

                  Case("iso_strain")
                      num_line=keyv(L:)
        	            call Get_Num(num_line, vet, ivet, iv)
        	            if(iv == 0) then
        	              write(unit=*,fmt="(a)")  " WARNING: At least 1 number (normally two!) should be specified after the ISO_STRAIN keyword"
        	              write(unit=*,fmt="(a)")  " ISO_STRAIN keyword ignored"
        	            else if(iv == 1) then
        	               Phase%Pow(n_pat)%iso_strain=vet(1)
        	               Phase%Pow(n_pat)%Lorentz_iso_strain_frac=0.0
        	            else
        	               Phase%Pow(n_pat)%iso_strain=vet(1)
        	               Phase%Pow(n_pat)%Lorentz_iso_strain_frac=vet(2)
        	            end if

                  Case("pref_or")
                      num_line=keyv(L:)
        	            call Get_Num(num_line, vet, ivet, iv)
        	            if(iv == 0) then
        	              call Set_Error(1,"At least 1 number (number of preferred orientation axes) should be specified after the PREF_OR keyword")
        	              return
        	            else
        	              Phase%Pow(n_pat)%n_pref=ivet(1)
        	            end if
        	            do k=1, Phase%Pow(n_pat)%n_pref
                         j=j+1
                         if(i+j > n_end) exit
                         num_line=adjustl(ffile%line(i+j)%str)
        	               if(len_trim(num_line) == 0) cycle
        	               if(num_line(1:1) == "!") cycle
        	               call Get_Num(num_line, vet, ivet, iv)
        	               if(iv < 4) then
        	                 call Set_Error(1,"Preferred orientation items should be 5: p1 p2 p3  value  fraction")
        	                 return
        	               end if
        	               if(iv < 5 .and. Phase%Pow(n_pat)%n_pref == 1) then
        	                 Phase%Pow(n_pat)%axes_pref(1:3,k)=vet(1:3)
        	                 Phase%Pow(n_pat)%pref(1,k)=vet(4)
        	                 Phase%Pow(n_pat)%pref(2,k)=1.0
        	               else if(iv == 5) then
        	                 Phase%Pow(n_pat)%axes_pref(1:3,k)=vet(1:3)
        	                 Phase%Pow(n_pat)%pref(1:2,k)=vet(4:5)
        	               end if
        	               !axis=vet(1:3)
        	               !write(*,"(a,3F12.5)") " Axis:", axis
        	               !Calculate the fourth component of axes_pref
        	               !write(*,"(3F12.5)") (Phase%cell%GR(:,j),j=1,3)
        	               mod2ax=dot_product(vet(1:3),matmul(Phase%cell%GR,vet(1:3)))
        	               Phase%Pow(n_pat)%axes_pref(4,k)=mod2ax
        	            end do

                  Case("extinction")
                      num_line=keyv(L:)
        	            call Get_Num(num_line, vet, ivet, iv)
        	            if(iv == 0) then
        	              write(unit=*,fmt="(a)")  " WARNING: 1 real number should be specified after the EXTINCTION keyword"
        	              write(unit=*,fmt="(a)")  " EXTINCTION keyword ignored"
        	            else if(iv == 1) then
        	               Phase%Pow(n_pat)%extinct=vet(1)
        	            end if

                  Case("absorption")
                      num_line=keyv(L:)
        	            call Get_Num(num_line, vet, ivet, iv)
        	            if(iv == 0) then
        	              write(unit=*,fmt="(a)")  " WARNING: at least 1 real number should be specified after the ABSORPTION keyword"
        	              write(unit=*,fmt="(a)")  " ABSORPTION keyword ignored"
        	            else if(iv == 1) then
        	               Phase%Pow(n_pat)%abs_corr(1)=vet(1)
        	            else if(iv == 2) then
        	               Phase%Pow(n_pat)%abs_corr(1:2)=vet(1:2)
        	            end if

                   Case("aniso_size")
                      call cut_string(keyv)    !  Model  "Laue"  Nani_size   or  platelet axis_size aniso_size(1) frac_gauss
                      call get_words(keyv,dire,ndir)
                      Select Case (ndir)
                         Case(0)
        	                 write(unit=*,fmt="(a)")  " WARNING: at least the 'size model' should be specified after the ANISO_SIZE keyword"
        	                 write(unit=*,fmt="(a)")  " ANISO_SIZE keyword ignored"
        	                 cycle
                         Case(1)
                           Phase%Pow(n_pat)%aniso_size_model=dire(1)
                         Case(2:)
                           if(dire(2)(1:1) == '"') then
                              Phase%Pow(n_pat)%Laue_Class=dire(2)(2:len_trim(dire(2))-1)
                              if(ndir > 2) then
                                read(dire(3),*) Phase%Pow(n_pat)%Nani_size
                              end if
                           else if (trim(dire(1)) == "platelet" .or. trim(dire(1)) == "needle") then
                              call cut_string(keyv) ! axis  aniso_size(1)
                              call Get_Num(keyv, vet, ivet, iv)
                              if(iv < 4) then
                                 call set_error(1,"For Platelet/Needle model at least four numbers should be provided: axis(3) value")
                                 return
                              else if(iv == 4) then
                                Phase%Pow(n_pat)%axis_size(1:3)=vet(1:3)
                                Phase%Pow(n_pat)%aniso_size(1)=vet(4)
                                Phase%Pow(n_pat)%Nani_size=1
                              else if(iv > 4) then
                                Phase%Pow(n_pat)%axis_size(1:3)=vet(1:3)
                                Phase%Pow(n_pat)%aniso_size(1:2)=vet(4:5)
                                Phase%Pow(n_pat)%Nani_size=2
                              end if
                           else ! The second directive is a number equal to the number of parameters
                              read(dire(2),*) Phase%Pow(n_pat)%Nani_size
                           end if
                      End Select

                      if(Phase%Pow(n_pat)%Nani_size > 2) then
                         ini=1
                         do
                            j=j+1
                            if(i+j > n_end) exit
                            num_line=adjustl(ffile%line(i+j)%str)
        	                  if(num_line(1:1) == "!" .or. len_trim(num_line) == 0) cycle
        	                  call Get_Num(num_line, vet, ivet, iv)
        	                  if(iv == 0) then
        	                    call Set_Error(1,"Error reading the ANISO_SIZE parameters")
        	                    return
        	                  else
        	                    Phase%Pow(n_pat)%aniso_size(ini:ini+iv-1)=vet(1:iv)
        	                    ini=ini+iv
        	                    if(ini > Phase%Pow(n_pat)%Nani_size) exit
        	                  end if
       	                 end do
                      end if

                   Case("aniso_strain")
                      call cut_string(keyv)    !  Model  "Laue"  Nani_strain   or  uniaxial axis_size aniso_strain(1) frac_lorentz
                      call get_words(keyv,dire,ndir)
                      Select Case (ndir)
                         Case(0)
        	                 write(unit=*,fmt="(a)")  " WARNING: at least the 'strain model' should be specified after the ANISO_STRAIN keyword"
        	                 write(unit=*,fmt="(a)")  " ANISO_STRAIN keyword ignored"
        	                 cycle
                         Case(1)
                           Phase%Pow(n_pat)%aniso_strain_model=dire(1)
                         Case(2:)
                           if(dire(2)(1:1) == '"') then
                              Phase%Pow(n_pat)%Laue_Class=dire(2)(2:len_trim(dire(2))-1)
                              if(ndir > 2) then
                                read(dire(3),*) Phase%Pow(n_pat)%Nani_strain
                              end if
                           else if (trim(dire(1)) == "uniaxial") then
                              call cut_string(keyv) ! axis  aniso_size(1)
                              call Get_Num(keyv, vet, ivet, iv)
                              if(iv < 4) then
                                 call set_error(1,"For uniaxial model at least four numbers should be provided: axis(3) value")
                                 return
                              else if(iv == 4) then
                                Phase%Pow(n_pat)%axis_strain(1:3)=vet(1:3)
                                Phase%Pow(n_pat)%aniso_strain(1)=vet(4)
                                Phase%Pow(n_pat)%Nani_strain=1
                              else if(iv > 4) then
                                Phase%Pow(n_pat)%axis_strain(1:3)=vet(1:3)
                                Phase%Pow(n_pat)%aniso_strain(1:2)=vet(4:5)
                                Phase%Pow(n_pat)%Nani_strain=2
                              end if
                           else ! The second directive is a number equal to the number of parameters
                              read(dire(2),*) Phase%Pow(n_pat)%Nani_size
                           end if
                      End Select

                      if(Phase%Pow(n_pat)%Nani_strain > 2) then
                         ini=1
                         do
                            j=j+1
                            if(i+j > n_end) exit
                            num_line=adjustl(ffile%line(i+j)%str)
        	                  if(num_line(1:1) == "!" .or. len_trim(num_line) == 0) cycle
        	                  call Get_Num(num_line, vet, ivet, iv)
        	                  if(iv == 0) then
        	                    call Set_Error(1,"Error reading the ANISO_STRAIN parameters")
        	                    return
        	                  else
        	                    Phase%Pow(n_pat)%aniso_strain(ini:ini+iv-1)=vet(1:iv)
        	                    ini=ini+iv
        	                    if(ini > Phase%Pow(n_pat)%Nani_strain) exit
        	                  end if
       	                 end do
                      end if

                  Case("end_ph_pattern")
                    exit
                End Select
             end do

          else if(samples(n_pat) == "X") then
             !To be implemented

          end if
        end if
     end do

  End Subroutine Read_Phase_PowSxt_Attributes




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
        write(unit=lun, fmt='(a)')     '       Directive: '//trim(Vec_Instr(i)%Str)
        write(unit=lun, fmt='(a, i2)') ' Num. Parameters: ', Vec_Instr(i)%Npar
        do j=1, Vec_Instr(i)%Npar
           write(unit=lun,fmt='(i8,3x,f12.4,3x,t35,a)') Vec_Instr(i)%IV(j), Vec_Instr(i)%RV(j), trim(Vec_Instr(i)%CV(j))
        end do
        write(unit=lun,fmt='(a)')' '
        write(unit=lun,fmt='(a)')' '
     end do

  End Subroutine Write_Info_Instructions

  Subroutine Read_CFL_Molecules(cfl, Nmol, n_ini, n_end, M)
     !---- Arguments ----!
     type(File_Type),       intent(in)     :: cfl
     integer,               intent(in)     :: Nmol
     integer,               intent(in)     :: n_ini
     integer,               intent(in)     :: n_end
     class(Phase_Type),     intent(in out) :: M

     !---- Local varibles ----!
     character(len=80)          :: line
     integer                    :: i, j, k
     integer, dimension(2,Nmol) :: mol_pos

     !> Init
     call clear_error()

     !First look for the positions of molecules in the curren phase
     k=0
     do i=n_ini,n_end
       line=adjustl(cfl%line(i)%str)
       if(u_case(line(1:4)) == "MOLE") then
          k=k+1
          mol_pos(1,k)=i
       end if
     end do
     if(k /= Nmol) then
        call set_error(1,"The number of read molecules is not correct!")
        return
     end if
     do j=1,k
        do i=mol_pos(1,j),n_end
          line=adjustl(cfl%line(i)%str)
          if(u_case(line(1:8)) == "END_MOLE") then
             mol_pos(2,j)=i
             exit
          end if
        end do
     end do
     if(CFML_Debug) then
        write(*,"(a)") " Number of molecules: ",nmol
        do i=1,nmol
           write(*,"(a,i2,a,2i4)") " Molecule #: ",i, "  between lines: ",mol_pos(:,i)
        end do
     end if
     if(allocated(M%Mol)) deallocate (M%Mol)
     allocate(M%Mol(Nmol))
     !> Molecules
     do i=1, nmol
        call Read_CFL_Molecule(cfl, mol_pos(1,i), mol_pos(2,i), M%Mol(i))
     end do
     M%Nmol=nmol

  End Subroutine Read_CFL_Molecules


End Module nFP_read_files