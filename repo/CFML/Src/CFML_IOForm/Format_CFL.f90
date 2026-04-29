!!----
!!----
!!----
SubModule (CFML_IOForm) Format_CFL
   !---- Variables ----!
   implicit none

 Contains
   !!----
   !!---- GET_CFL_BLOCK_INFO
   !!----    Returns the blocks of type "name" in a CFL file. The block must
   !!----    start with keyword "NAME" and finishes with "END_NAME". Keyword
   !!----    can be in upper or lowercase.
   !!----
   !!---- 18/12/2024
   !!
   Module Function Get_CFL_Block_Info(cfl, name) Result(blok)
      !---- Arguments ----!
      type(File_Type),                    intent(in)  :: cfl   ! CFL content
      character(len=*),                   intent(in)  :: name  ! Block name -PHASE, PATTERN...-
      type(BlockInfo_Type), dimension(:), allocatable :: blok  ! Block information

      !---- Local variables ----!
      integer :: i,j
      integer :: iblock,nblock,nblock_
      character(len=:), allocatable :: iline,block_name

      ! Determine the number of blocks
      nblock = Get_CFL_NBlock(cfl,name)
      block_name = l_case(adjustl(trim(name)))
      if (err_cfml%ierr /= 0) return
      if (nblock == 0) then
         err_cfml%ierr=1
         err_cfml%msg="Get_CFL_NBlock@CFML_IOForm: No block "//block_name//" found"
         return
      end if

      allocate(blok(nblock))

      iblock = 0
      nblock_ = 0
      do i = 1 , cfl%nlines
         iline = l_case(adjustl(trim(cfl%line(i)%str)))
         j = index(iline,block_name)
         if (j == 1) then
            iblock = iblock + 1
            blok(iblock)%nl(1) = i
         end if
         j = index(iline,"end_"//block_name)
         if (j == 1) then
            blok(iblock)%nl(2) = i
            nblock_ = nblock_ + 1
         end if
         if (nblock == nblock_) exit
      end do

   End Function Get_CFL_Block_Info

   !!----
   !!---- GET_CFL_NBLOCK
   !!----    Returns the number of blocks of type "name" in a CFL file.
   !!----    A block starts with keyword "NAME" and finishes with "END_NAME".
   !!----    Keyword can be in upper or lowercase.
   !!----
   !!---- 18/12/2024
   !!
   Module Function Get_CFL_NBlock(cfl, name) Result(nblock)
      !---- Arguments ----!
      type(File_Type),  intent(in)  :: cfl    ! CFL content
      character(len=*), intent(in)  :: name   ! Block name -PHASE, PATTERN...-
      integer                       :: nblock ! Number of blocks

      !---- Local variables ----!
      integer :: i,j
      logical :: inside_block
      character(len=:), allocatable :: iline,block_name

      nblock = 0
      block_name = l_case(adjustl(trim(name)))
      inside_block = .false.
      do i = 1 , cfl%nlines
         iline = l_case(adjustl(trim(cfl%line(i)%str)))
         j = index(iline,block_name)
         if (j == 1) then
            if (inside_block) then
               err_cfml%ierr=1
               err_cfml%msg="Get_CFL_NBlock@CFML_IOForm: Block "//block_name//" not properly closed"
               return
            else
               inside_block = .true.
            end if
         else
            j = index(iline,"end_"//block_name)
            if (j == 1) then
               if (inside_block) then
                  nblock = nblock + 1
                  inside_block = .false.
               else
                  err_cfml%ierr=1
                  err_cfml%msg="Get_CFL_NBlock@CFML_IOForm: Block "//block_name//" not properly opened"
                  return
               end if
            end if
         end if
      end do

   End Function Get_CFL_NBlock

   !!----
   !!---- READ_CFL_ATOM
   !!----    Subroutine to read atoms parameters
   !!----
   !!----         ATOM   Label  ChemSymb   x y z B Occ Us or Moment
   !!----
   !!----     For charge obtained from Label: Label[+/-][number]
   !!----
   !!---- 07/05/2020
   !!
   Module Subroutine Read_CFL_Atoms(cfl, AtmList, Type_Atm, d, i_ini, i_end)
      !---- Arguments ----!
      type(File_Type),      intent(in)     :: cfl     ! Containing information
      Type(AtList_Type),    intent(out)    :: AtmList
      character(len=*),     intent(in)     :: Type_Atm
      integer,              intent(in)     :: d
      integer, optional,    intent(in)     :: i_ini, i_end

      !---- Local variables -----!
      character(len=80) :: mom_comp
      character(len=80) :: direc
      integer           :: i, j, na, npos, n_oc, n_mc,n_dc,n_uc
      integer           :: j_ini, j_end

      !> Init
      call clear_error()
      if (cfl%nlines <=0) then
         err_CFML%Ierr=1
         err_CFML%Msg="Read_CFL_Atoms@CFML_IOForm: 0 lines "
         return
      end if

      j_ini=1; j_end=cfl%nlines
      if (present(i_ini)) j_ini=i_ini
      if (present(i_end)) j_end=i_end

      if (AtmList%natoms > 0) call Allocate_Atom_List(0, Atmlist, Type_Atm, d)

      !> Calculate number of Atoms
      na=0
      mom_comp=" "
      do i=j_ini,j_end
          line=adjustl(cfl%line(i)%str)
          if (len_trim(line) == 0) cycle
          if (line(1:1) == "!" .or. line(1:1) == "#") cycle
          if (line(1:1) == ' ') cycle

          if (index(u_case(line),"ATM_MOM_COMP") /= 0) then
             j=index(line,"!")
             if ( j /= 0) then
                mom_comp=adjustl(line(13:j-1))
             else
                mom_comp=adjustl(line(13:))
             end if
          end if
          if(u_case(line(1:4)) == "ATOM") na=na+1
      end do
      if (na == 0) return             ! No atoms in the lines

      !> Allocate List
      call Allocate_Atom_List(na, Atmlist, Type_Atm, d)
      if (len_trim(mom_comp) > 2) Atmlist%mcomp=mom_comp

      na=0
      do i=j_ini,j_end
         line=adjustl(cfl%line(i)%str)
         if (len_trim(line) == 0) cycle
         if (line(1:1) == "!" .or. line(1:1) == "#") cycle
         if (line(1:1) ==' ') cycle

         !> Truncate line from symbols: # and !
         npos=index(line,'!')
         if (npos > 0) line=line(:npos-1)
         ! Eliminate part of the line for reading files from FP_Studio
         npos=index(u_case(line),"COLOR")
         if ( npos > 0) line=line(1:npos-1)
         npos=index(u_case(line),"NODISP")
         if ( npos > 0) line=line(1:npos-1)
         npos=index(u_case(line),"SCALE")
         if ( npos > 0) line=line(1:npos-1)

         !> Eliminate Tabs
         do
            npos=index(line,TAB)
            if (npos == 0) exit
            line(npos:npos)=' '
         end do

         !> ATOM Directive
         if(len_trim(line) < 4) then
           cycle
         else
           direc=adjustl(u_case(line(1:4)))
         end if
         if (trim(direc) /= "ATOM") cycle

         na=na+1
         call read_atom(line, Atmlist%atom(na))  ! Utype is read now in the line
         Atmlist%atom(na)%ThType="iso"
         if (len_trim(Atmlist%atom(na)%SfacSymb) == 0) Atmlist%atom(na)%SfacSymb=Atmlist%atom(na)%chemSymb

         !Debugging
         !if(CFML_DEBUG) then
         !  associate (Atm => Atmlist%atom)
         !    write(*,"(a)") " => "//trim(line)
         !    write(*,"(a,i4,5f10.5,a)")  trim(Atm(na)%Lab)//" "//trim(Atm(na)%SfacSymb), &
         !       Atm(na)%mult,  Atm(na)%X, Atm(na)%U_iso, Atm(na)%Occ,"   "//Atm(na)%UType
         !  end associate
         !end if
         !> Trial to read anisotropic thermal and
         !> magnetic moment parameters
         j=i
         do
            j=j+1
            if ( j < j_end ) then
               line=adjustl(cfl%line(j)%str)

               if (len_trim(line) == 0) cycle
               if (line(1:1) == "!" .or. line(1:1) == "#") cycle
               if (u_case(line(1:4)) == "ATOM") exit

               npos=index(line," ")
               if (npos <= 1) cycle

               select case (u_case(line(1:npos-1)))
                  case ("MOMENT")
                     call read_Moment(line,Atmlist%atom(na))

                  case ("U_IJ")
                     call read_UTherms(line,Atmlist%atom(na))
                     Atmlist%atom(na)%UType= "U"
                     Atmlist%atom(na)%ThType= "ANI"

                  case ("B_IJ")
                     call read_UTherms(line,Atmlist%atom(na))
                     Atmlist%atom(na)%UType="B"
                     Atmlist%atom(na)%ThType="ANI"

                  case ("BETA")
                     call read_UTherms(line,Atmlist%atom(na))
                     Atmlist%atom(na)%UType= "BETA"
                     Atmlist%atom(na)%ThType="ANI"
               end select
               if (Err_CFML%Ierr /= 0) return

            else
               exit
            end if
         end do

         select type(at => Atmlist%atom)
            class is(ModAtm_Std_Type)
               n_oc=0; n_mc=0; n_dc=0; n_uc=0
               j=i
               do
                  j=j+1
                  if ( j < j_end ) then
                     line=adjustl(cfl%line(j)%str)

                     if (len_trim(line) == 0) cycle
                     if (line(1:1) == "!" .or. line(1:1) == "#") cycle
                     if (line(1:1) == ' ') cycle
                     if (u_case(line(1:4)) == "ATOM") exit

                     npos=index(line," ")
                     if (npos <= 1) then
                        err_CFML%Ierr=1
                        err_CFML%Msg="Read_CFL_Atoms: Error in line "//trim(line)
                        return
                     end if

                     select case (u_case(line(1:npos-1)))
                        case ("O_CS")
                           n_oc=n_oc+1
                           call read_modulation_amplitudes(line,At(na),"O_CS",n_oc)

                        case ("M_CS")
                           n_mc=n_mc+1
                           call read_modulation_amplitudes(line,At(na),"M_CS",n_mc)

                        case ("D_CS")
                           n_dc=n_dc+1
                           call read_modulation_amplitudes(line,At(na),"D_CS",n_dc)

                        case ("U_CS")
                           n_uc=n_uc+1
                           call read_modulation_amplitudes(line,At(na),"U_CS",n_uc)
                     end select
                     if (Err_CFML%Ierr /= 0) return
                  else
                     exit
                  end if
               end do
               At(na)%n_oc=n_oc
               At(na)%n_mc=n_mc
               At(na)%n_dc=n_dc
               At(na)%n_uc=n_uc
         end select
      end do
      AtmList%natoms=na

   End Subroutine Read_CFL_Atoms

   !!----
   !!---- READ_CFL_CELL
   !!----
   !!----    Obtaining Cell Parameter from CFL Format
   !!----
   !!---- 07/05/2020
   !!
   Module Subroutine Read_CFL_Cell(cfl, Cell, CFrame, i_ini, i_end, cmd)
      !---- Arguments ----!
      type(File_Type),                intent(in)     :: cfl     ! Containing information
      class(Cell_G_Type),allocatable, intent(out)    :: Cell    ! Cell object
      character(len=*),     optional, intent(in)     :: CFrame
      integer,              optional, intent(in)     :: i_ini, i_end     ! Lines to explore
      logical,              optional, intent(in)     :: cmd
      !---- Local variables -----!
      integer                              :: i, iv, n_ini, n_end
      integer                              :: j_ini,j_end
      real(kind=cp), dimension (6)         :: vcell, std
      character(len=132), dimension(1)     :: linec

      !> Init
      call clear_error()
      if (cfl%nlines <=0) then
         err_CFML%Ierr=1
         err_CFML%Msg="Read_CFL_Cell@CFML_IOForm: 0 lines "
         return
      end if

      j_ini=1; j_end=cfl%nlines
      if (present(i_ini)) j_ini=i_ini
      if (present(i_end)) j_end=i_end

      !> Search: CELL
      do i=j_ini,j_end
         linec(1)=adjustl(u_case(cfl%line(i)%str))
         if (linec(1)(1:4) == "CELL") exit
         linec(1)=" "
      end do

      if (len_trim(linec(1)) == 0) then
         err_CFML%Ierr=1
         err_CFML%Msg="Read_CFL_Cell@CFML_IOForm: Instruction 'CELL' not provided "
         return
      end if

      !> Eliminate Tabs
      do
         iv=index(linec(1),TAB)
         if (iv == 0) exit
         linec(1)(iv:iv)=' '
      end do

      n_ini=1; n_end=1
      vcell=0.0
      std=0.0
      call Read_Key_ValueSTD(linec, n_ini, n_end,"CELL", vcell, std, iv)
      if (iv /= 6) then
         err_CFML%Ierr=1
         err_CFML%Msg="Read_CFL_Cell@CFML_IOForm: Problems reading cell parameters!"
         return
      end if
      if(present(cmd)) then
        allocate(Cell_GLS_Type :: Cell)
      else
        allocate(Cell_G_Type :: Cell)
      end if
      if (present(CFrame)) then
         call Set_Crystal_Cell(vcell(1:3),vcell(4:6), Cell, CarType=CFrame, Vscell=std(1:3), Vsang=std(4:6))
      else
         call Set_Crystal_Cell(vcell(1:3),vcell(4:6), Cell, Vscell=std(1:3), Vsang=std(4:6))
      end if

   End Subroutine Read_CFL_Cell

   !!----
   !!---- READ_CFL_KVECTORS
   !!----
   !!---- Read K-vectors information
   !!----
   !!---- 07/05/2020
   !!
   Module Subroutine Read_CFL_KVectors(cfl, Kvec, i_ini, i_end)
      !---- Arguments ----!
      type(File_Type),         intent(in)     :: cfl
      type(kvect_info_Type),   intent(out)    :: Kvec
      integer,       optional, intent(in)     :: i_ini, i_end

      !---- Local Variables ----!
      integer                      :: i,j,ier,nk,nq,iv
      integer                      :: j_ini, j_end
      character(len=:),allocatable :: uline

      !> Init
      call clear_error()
      if (cfl%nlines <=0) then
         err_CFML%Ierr=1
         err_CFML%Msg="Read_CFL_Kvectors: 0 lines "
         return
      end if

      j_ini=1; j_end=cfl%nlines
      if (present(i_ini)) j_ini=i_ini
      if (present(i_end)) j_end=i_end

      nk=0; nq=0
      do i=j_ini,j_end
         line=adjustl(cfl%line(i)%str)
         if (len_trim(line) == 0) cycle
         if (line(1:1) == "!" .or. line(1:1) == "#") cycle
         if (line(1:1) == " ") cycle

         !> Eliminate Tabs
         do
            iv=index(line,TAB)
            if (iv == 0) exit
            line(iv:iv)=' '
         end do

         j=index(line,"!")
         if (j /= 0) line=line(:j-1)

         j=index(line," ")
         if ( j == 0) then
            uline=u_case(line)
         else
            uline=u_case(line(:j-1))
         end if
         line=adjustl(line(j+1:))

         select case(trim(uline))
            case("NQVECT","NKVECT","NKVEC","NQVEC")
               read(unit=line,fmt=*,iostat=ier) Kvec%nk, Kvec%nq
               if (ier /= 0) then
                  Err_CFML%Ierr=1
                  Err_CFML%Msg="Error reading the number of k-vectors and/or number of Q-coefficients"
                  return
               end if
               allocate(Kvec%kv(3,Kvec%nk),Kvec%q_coeff(Kvec%nk,Kvec%nq))
               allocate(Kvec%nharm(Kvec%nk),Kvec%sintlim(Kvec%nk))
               Kvec%kv=0.0_cp; Kvec%q_coeff=1; Kvec%nharm=1; Kvec%sintlim=1.0

            case("QVECT","KVECT","KVEC","QVEC")
               if (Kvec%nk > 0) then
                  nk=nk+1
                  read(unit=line,fmt=*,iostat=ier) Kvec%kv(:,nk)
                  if (ier /= 0) then
                     Err_CFML%Ierr=1
                     write(unit=Err_CFML%Msg,fmt="(a,i2)") "Error reading the k-vector #",nk
                     return
                  end if
               end if

           case("NHARM")
              if (Kvec%nk > 0) then
                 read(unit=line,fmt=*,iostat=ier) Kvec%nharm(1:Kvec%nk)
                 if (ier /= 0) then
                    Err_CFML%Ierr=1
                    Err_CFML%Msg = "Error reading the nk harmonics !"
                    return
                 end if
              end if

           case("SINTL")
              if (Kvec%nk > 0) then
                 read(unit=line,fmt=*,iostat=ier) Kvec%sintlim(1:Kvec%nk)
                 if (ier /= 0) then
                    Err_CFML%Ierr=1
                    Err_CFML%Msg = "Error reading the maximum sinTheta/Lambda for harmonics!"
                    return
                 end if
              end if

           case("Q_COEFF")
              nq=nq+1
              read(unit=line,fmt=*,iostat=ier) Kvec%q_coeff(1:Kvec%nk,nq)
              if (ier /= 0) then
                 Err_CFML%Ierr=1
                 write(unit=Err_CFML%Msg,fmt="(a,i2)") "Error reading the Q-coefficent # ",nq
                 return
              end if
         end select
      end do

      if (Kvec%nk /= nk) then
         Err_CFML%Ierr=1
         write(unit=Err_CFML%Msg,fmt="(2(a,i2))") "The number of k-vectors,",Kvec%nk, ", does not correspond with the prescribed number: ",nk
         return
      end if

      if (Kvec%nq /= nq) then
         Err_CFML%Ierr=1
         write(unit=Err_CFML%Msg,fmt="(2(a,i2))") "The number of expected Q-coefficients,",Kvec%nq, ", does not correspond with number of read Q-coefficients ",nq
         return
      end if

   End Subroutine Read_CFL_KVectors

   !!----
   !!---- READ_CFL_PATTERN
   !!----
   !!---- Read pattern information in a CFL file
   !!----
   !!---- 21/12/2024
   !!
   Module Subroutine Read_CFL_Pattern(cfl, pat, i_ini, i_end, genp)
      !---- Argumens ----!
      Type(File_Type),            intent(in)  :: cfl
      type(Pattern_Type),         intent(out) :: pat
      integer,          optional, intent(in)  :: i_ini
      integer,          optional, intent(in)  :: i_end
      logical,          optional, intent(out) :: genp

      !---- Local variables ----!
      integer :: i,iv,j,j_ini,j_end,k,npts,ier
      real(kind=cp) :: xmin,step,xmax
      character(len=:), allocatable :: key,key_, mode

      call clear_error()
      if (cfl%nlines <= 0) then
         err_CFML%Ierr=1
         err_CFML%Msg="Read_CFL_Pattern@CFML_IOForm: 0 lines "
         return
      end if

      j_ini=1
      j_end=cfl%nlines
      if (present(i_ini)) j_ini=i_ini
      if (present(i_end)) j_end=i_end
      if (present(genp))  genp=.false.
      pat%filename="      "
      mode="       "
      do i = j_ini , j_end
         line = adjustl(cfl%line(i)%str)
         if (len_trim(line) == 0) cycle
         if (line(1:1) == "!" .or. line(1:1) == "#") cycle

         ! Eliminate Tabs
         do
            iv=index(line,TAB)
            if (iv == 0) exit
            line(iv:iv)=' '
         end do

         ! Remove comments at the end of the line
         j = index(line,"!")
         if (j /= 0) line = line(:j-1)

         ! Get the keyword
         j = index(line," ")
         key_ = line(:j-1)
         key = u_case(key_)
         k = index(key,'PATTERN_')
         if (k == 1) then
            pat%name = key_(9:)
            exit
         end if
      end do

      allocate(DiffPat_E_Type :: pat%pdat)

      call read_cfl_pat_type(cfl,pat,i_ini=j_ini,i_end=j_end)
      if (err_cfml%ierr /= 0) return
      if (allocated(pat%cond)) deallocate(pat%cond)
      if (pat%sample == 'P') then
         if (pat%radiation == 'N' .and. pat%mode == 'TF') then
            allocate(PowPatt_TOF_Conditions_Type :: pat%cond)
            pat%pdat%ScatVar="T.O.F (uSecs)"
         else
            allocate(PowPatt_CW_Conditions_Type :: pat%cond)
            pat%pdat%ScatVar="2Theta (degrees)"
         end if
         if (pat%radiation == 'N') then
            pat%cond%job = 1
         else
            pat%cond%job = 0
         end if
         call read_cfl_pat_conditions(cfl,pat%cond,i_ini=j_ini,i_end=j_end)
      end if
      !Look for the file where the pattern is contained
      !or, if it is a simulated pattern, for the instruction "gen_patt xmin step xmax"
      do i = j_ini , j_end
         line = adjustl(cfl%line(i)%str)
         if (len_trim(line) == 0) cycle
         if (line(1:1) == "!" .or. line(1:1) == "#") cycle
         ! Remove comments at the end of the line
         j = index(line,"!")
         if (j /= 0) line = line(:j-1)
         if(u_case(line(1:9)) == "PATT_FILE") then
            pat%filename=adjustl(trim(line(11:)))
         end if
         if(u_case(line(1:11)) == "FORMAT_FILE") then
           mode=adjustl(line(13:))
         end if
         if(u_case(line(1:8)) == "GEN_PATT") then
           read(unit=line(9:),fmt=*,iostat=ier) xmin,step,xmax
           if(ier /= 0) then
            xmin=0.0; step=0.05; xmax=120.0
           end if
           npts=(xmax-xmin)/step+1.2
           pat%pdat%xmin=xmin
           pat%pdat%xmax=xmax
           pat%pdat%step=step
           pat%pdat%npts=npts
           if(present(genp)) genp=.true.
           call allocate_pattern(pat%pdat)

           Select Type (c => pat%cond)
             class is (PowPatt_CW_Conditions_Type)
              if(c%tthmin < 0.00001) c%tthmin=xmin
              if(c%step  < 0.00001) c%step=step
              if(c%tthmax < 0.00001) c%tthmax=xmax
              Pat%Pdat%wave(1:4)=[c%lambda,c%ratio,c%zero] ! Wave1, Wave2, ratio, zero
             class is (PowPatt_TOF_Conditions_Type)
              if(c%tof_min < 0.00001) c%tof_min=xmin
              if(c%step  < 0.00001)   c%step=step
              if(c%tof_max < 0.00001) c%tof_max=xmax
              Pat%Pdat%wave(1:4)=[c%dtt1,c%dtt2,c%Dtt_1overD,c%zero] ! Dtt1, Dtt2, Dtt_1overD, zero...
           End Select
           do k=1,npts
             pat%pdat%x(k)=xmin+(k-1)*step !The rest of the pattern will be constructed elsewhere
           end do
         end if
      end do
      if(len_trim(pat%filename) > 4 .and. len_trim(mode) > 2) then
         pat%pdat=Load_Pattern(pat%Filename, Mode)
         if(.not. allocated(pat%pdat%x) .or. .not. allocated(pat%pdat%y)) then
           err_CFML%Ierr=1; err_CFML%Flag=.true.
           err_CFML%Msg="Error loading the pattern from "//trim(pat%Filename)//" @Read_CFL_Pattern"
           return
         end if
      end if

   End Subroutine Read_CFL_Pattern

   !!----
   !!---- READ_CFL_PAT_CONDITIONS
   !!----
   !!---- Read diffraction pattern conditions and put them in cond
   !!----
   !!---- 15/01/2025
   !!
   Module Subroutine Read_CFL_Pat_Conditions(cfl, cond, i_ini, i_end)
      !---- Arguments ----!
      type(File_Type),                  intent(in)     :: cfl   ! Containing information
      class(DiffPatt_Conditions_Type),  intent(inout)  :: cond  ! Conditions
      integer,              optional,   intent(in)     :: i_ini ! First lines to explore
      integer,              optional,   intent(in)     :: i_end ! Last line to explore

      !---- Local variables ----!
      integer :: i,ierr,iv,iw,j,k,j_ini,j_end
      character(len=:),  allocatable   :: key,key_
      character(len=20), dimension(10) :: dire,sub_dire


      !> Init
      ierr = 0
      call clear_error()
      if (cfl%nlines <= 0) then
         err_CFML%Ierr=1
         err_CFML%Msg="Read_CFL_Pat_Conditions@CFML_IOForm: 0 lines "
         return
      end if

      j_ini=1
      j_end=cfl%nlines
      if (present(i_ini)) j_ini=i_ini
      if (present(i_end)) j_end=i_end
      Select Type (cond)
          type is (PowPatt_CW_Conditions_Type)
            cond%wdt=5.0; cond%asym1=0.0; cond%asym2=0.0; cond%is_asym = .false.
            cond%lambda=0.0; cond%ratio=0.0; cond%profile="       "
            cond%tthmin=0.0; cond%tthmax=0.0; cond%step=0.0
            cond%u=0.0; cond%v=0.0; cond%w=0.0; cond%x=0.0; cond%y=0.0
            cond%zero=0.0; cond%sycos=0.0; cond%sysin=0.0
          type is (PowPatt_TOF_Conditions_Type)
            cond%profile="         "; cond%wdt=5.0
            cond%zero=0.0;    cond%dtt1=0.0;    cond%dtt2=0.0; cond%dtt_1overD=0.0
            cond%alpha0=0.0;  cond%alpha1=0.0;  cond%alphaQ=0.0
            cond%beta0=0.0;   cond%beta1=0.0;   cond%betaQ=0.0
            cond%sigma2=0.0;  cond%sigma1=0.0;  cond%sigma0=0.0; cond%sigmaQ=0.0
            cond%gamma2=0.0;  cond%gamma1=0.0;  cond%gamma0=0.0
            cond%tof_min=0.0; cond%tof_max=0.0; cond%step=0.0
            cond%zero=0.0;    cond%dtt1=0.0;    cond%dtt2=0.0; cond%dtt_1overd=0.0
      End Select
      do i = j_ini , j_end
         line = adjustl(cfl%line(i)%str)
         if (len_trim(line) == 0) cycle
         if (line(1:1) == "!" .or. line(1:1) == "#") cycle

         ! Eliminate Tabs
         do
            iv=index(line,TAB)
            if (iv == 0) exit
            line(iv:iv)=' '
         end do

         ! Remove comments at the end of the line
         j = index(line,"!")
         if (j /= 0) line = line(:j-1)

         ! Get the keyword
         j = index(line," ")
         key_ = line(:j-1)
         key = u_case(key_)
         line = adjustl(line(j+1:))

         select type (cond)

          type is (PowPatt_CW_Conditions_Type)
            select case(trim(key))
               case("WDT")
                  read(unit=line,fmt=*,iostat=ierr) cond%wdt
                  if (ierr /= 0) then
                     err_CFML%Ierr=1
                     err_CFML%Msg="Error reading PowPatt_CW_Conditions_Type: WDT"
                     return
                  end if
               case("ASYM")
                  read(unit=line,fmt=*,iostat=ierr) cond%asym1,cond%asym2
                  if (ierr == 0) cond%is_asym = .true.
                  if (ierr /= 0) then
                     err_CFML%Ierr=1
                     err_CFML%Msg="Error reading PowPatt_CW_Conditions_Type: ASYM1 ASYM2"
                     return
                  end if
               case("LAMBDA")
                  call get_words(line,dire,iv)
                  Select Case(iv)
                    Case(1,2)
                      read(unit=line,fmt=*,iostat=ierr) cond%lambda(1)
                      if(ierr == 0) then
                        cond%lambda(2)=cond%lambda(1)
                        cond%ratio=0.0
                      end if
                    Case(3)
                      read(unit=line,fmt=*,iostat=ierr) cond%lambda,cond%ratio
                  End Select
                  if (ierr /= 0) then
                     err_CFML%Ierr=1
                     err_CFML%Msg="Error reading PowPatt_CW_Conditions_Type: lambda1, lambda2, ratio"
                     return
                  end if
                  cond%twowaves=.false.
                  if(abs(cond%lambda(1)-cond%lambda(2)) > 0.0001_cp .and. cond%ratio > 0.01_cp)  cond%twowaves=.true.
               case("PROFILE_FUNCTION")
                  read(unit=line,fmt=*,iostat=ierr) cond%profile
                  if (ierr /= 0) then
                     err_CFML%Ierr=1
                     err_CFML%Msg="Error reading PowPatt_CW_Conditions_Type: PROFILE_FUNCTION"
                     return
                  end if
               case("THETA_RANGE")
                  read(unit=line,fmt=*,iostat=ierr) cond%tthmin,cond%tthmax,cond%step
                  if (ierr /= 0) then
                     err_CFML%Ierr=1
                     err_CFML%Msg="Error reading PowPatt_CW_Conditions_Type: THETA_RANGE"
                     return
                  end if
               case("UVWXY")
                  read(unit=line,fmt=*,iostat=ierr) cond%u,cond%v,cond%w,cond%x,cond%y
                  if (ierr /= 0) then
                     err_CFML%Ierr=1
                     err_CFML%Msg="Error reading PowPatt_CW_Conditions_Type: UVWXY"
                     return
                  end if
               case("ZERO_SY")
                  read(unit=line,fmt=*,iostat=ierr) cond%zero,cond%sycos,cond%sysin
                  if (ierr /= 0) then
                     err_CFML%Ierr=1
                     err_CFML%Msg="Error reading PowPatt_CW_Conditions_Type: Zero, SyCos, SySin"
                     return
                  end if
               case("ZERO")
                  !Check if other keywords are provided in the same line
                  call get_words(line,dire,iv,";")
                  if(iv > 1) then
                     do k= 1, iv
                       call get_words(dire(k),sub_dire,iw)
                       if(iw == 2) then
                         Select Case(trim(u_case(sub_dire(1))))
                           case("ZERO")
                             read(unit=sub_dire(2),fmt=*,iostat=ierr) cond%zero
                           case("SYCOS")
                             read(unit=sub_dire(2),fmt=*,iostat=ierr) cond%sycos
                           case("SYSIN")
                             read(unit=sub_dire(2),fmt=*,iostat=ierr) cond%sysin
                         End Select
                       End if
                     end do
                  else
                     read(unit=line,fmt=*,iostat=ierr) cond%zero
                  end if
                  if (ierr /= 0) then
                     err_CFML%Ierr=1
                     err_CFML%Msg="Error reading PowPatt_CW_Conditions_Type: ZERO"
                     return
                  end if
               case("SYCOS")
                  read(unit=line,fmt=*,iostat=ierr) cond%sycos
                  if (ierr /= 0) then
                     err_CFML%Ierr=1
                     err_CFML%Msg="Error reading PowPatt_CW_Conditions_Type: SyCos"
                     return
                  end if
               case("SYSIN")
                  read(unit=line,fmt=*,iostat=ierr) cond%sysin
                  if (ierr /= 0) then
                     err_CFML%Ierr=1
                     err_CFML%Msg="Error reading PowPatt_CW_Conditions_Type: SySin"
                     return
                  end if
            end select

          type is (PowPatt_TOF_Conditions_Type)
            select case(trim(key))
               case("PROFILE_FUNCTION")
                  read(unit=line,fmt=*,iostat=ierr) cond%profile
                  if (ierr /= 0) then
                     err_CFML%Ierr=1
                     err_CFML%Msg="Error reading PowPatt_TOF_Conditions_Type: PROFILE_FUNCTION"
                     return
                  end if
               case("WDT")
                  read(unit=line,fmt=*,iostat=ierr) cond%wdt
                  if (ierr /= 0) then
                     err_CFML%Ierr=1
                     err_CFML%Msg="Error reading PowPatt_TOF_Conditions_Type: WDT"
                     return
                  end if
               case("D2TOF")
                  read(unit=line,fmt=*,iostat=ierr) cond%zero,cond%dtt1,cond%dtt2,cond%dtt_1overD
                  if (ierr /= 0) then
                     err_CFML%Ierr=1
                     err_CFML%Msg="Error reading PowPatt_TOF_Conditions_Type: Zero, Dtt1, Dtt2, Dtt_1overD"
                     return
                  end if
               case("ALPHA")
                  read(unit=line,fmt=*,iostat=ierr) cond%alpha0,cond%alpha1,cond%alphaQ
                  if (ierr /= 0) then
                     err_CFML%Ierr=1
                     err_CFML%Msg="Error reading PowPatt_TOF_Conditions_Type: Alph0, Alpha1, AlphaQ"
                     return
                  end if
               case("BETA")
                  read(unit=line,fmt=*,iostat=ierr) cond%beta0,cond%beta1,cond%betaQ
                  if (ierr /= 0) then
                     err_CFML%Ierr=1
                     err_CFML%Msg="Error reading PowPatt_TOF_Conditions_Type: Beta0, Beta1, BetaQ"
                     return
                  end if
               case("SIGMA")
                  read(unit=line,fmt=*,iostat=ierr) cond%sigma2,cond%sigma1,cond%sigma0,cond%sigmaQ
                  if (ierr /= 0) then
                     err_CFML%Ierr=1
                     err_CFML%Msg="Error reading PowPatt_TOF_Conditions_Type: Sigma2, Sigma1, Sigma0, SigmaQ"
                     return
                  end if
               case("GAMMA")
                  read(unit=line,fmt=*,iostat=ierr) cond%gamma2,cond%gamma1,cond%gamma0
                  if (ierr /= 0) then
                     err_CFML%Ierr=1
                     err_CFML%Msg="Error reading PowPatt_TOF_Conditions_Type: Gamma2, Gamma1, Gamma0"
                     return
                  end if
               case("TOF_RANGE")
                  read(unit=line,fmt=*,iostat=ierr) cond%tof_min,cond%tof_max,cond%step
                  if (ierr /= 0) then
                     err_CFML%Ierr=1
                     err_CFML%Msg="Error reading PowPatt_TOF_Conditions_Type: TOF_Range"
                     return
                  end if
               case("ZERO")
                  !Check if other keywords are provided in the same line
                  call get_words(line,dire,iv,";")
                  if(iv > 1) then
                     do k= 1, iv
                       call get_words(dire(k),sub_dire,iw)
                       if(iw == 2) then
                         Select Case(trim(u_case(sub_dire(1))))
                           case("ZERO")
                             read(unit=sub_dire(2),fmt=*,iostat=ierr) cond%zero
                           case("DTT1")
                             read(unit=sub_dire(2),fmt=*,iostat=ierr) cond%dtt1
                           case("DTT2")
                             read(unit=sub_dire(2),fmt=*,iostat=ierr) cond%dtt2
                           case("DTT_1OVERD")
                             read(unit=sub_dire(2),fmt=*,iostat=ierr) cond%dtt_1overD
                         End Select
                       End if
                     end do
                  else
                     read(unit=line,fmt=*,iostat=ierr) cond%zero
                  end if
                  if (ierr /= 0) then
                     err_CFML%Ierr=1
                     err_CFML%Msg="Error reading PowPatt_TOF_Conditions_Type: ZERO"
                     return
                  end if
               case("DTT1")
                  read(unit=line,fmt=*,iostat=ierr) cond%dtt1
                  if (ierr /= 0) then
                     err_CFML%Ierr=1
                     err_CFML%Msg="Error reading PowPatt_TOF_Conditions_Type: Dtt1"
                     return
                  end if
               case("DTT2")
                  read(unit=line,fmt=*,iostat=ierr) cond%dtt2
                  if (ierr /= 0) then
                     err_CFML%Ierr=1
                     err_CFML%Msg="Error reading PowPatt_TOF_Conditions_Type: Dtt2"
                     return
                  end if
               case("DTT_1OVERD")
                  read(unit=line,fmt=*,iostat=ierr) cond%dtt_1overd
                  if (ierr /= 0) then
                     err_CFML%Ierr=1
                     err_CFML%Msg="Error reading PowPatt_TOF_Conditions_Type: Dtt_1OverD"
                     return
                  end if
            end select

         end select
      end do

      if(CFML_DEBUG) then
        select type (cond)
          type is(PowPatt_CW_Conditions_Type)
            write(*,"(a,f12.4)")  "WDT", cond%wdt
            write(*,"(a,2f12.4)") "ASYM", cond%asym1,cond%asym2
            write(*,"(a,3f12.4)") "LAMBDA", cond%lambda,cond%ratio
            write(*,"(a)") "PROFILE_FUNCTION "//trim(cond%profile)
            write(*,"(a,3f12.4)") "THETA_RANGE ",cond%tthmin,cond%tthmax,cond%step
            write(*,"(a,5f12.4)") "UVWXY",cond%u,cond%v,cond%w,cond%x,cond%y
            write(*,"(a,3f12.4)") "ZERO_SY",cond%zero,cond%sycos,cond%sysin
        end select
      end if

   End Subroutine Read_CFL_Pat_Conditions

   !!----
   !!---- READ_CFL_PAT_TYPE
   !!----
   !!---- Read keyword "PATT_TYPE" and put its content in pt
   !!----
   !!---- 15/01/2025
   !!
   Module Subroutine Read_CFL_Pat_Type(cfl, pt, i_ini, i_end)
      !---- Arguments ----!
      type(File_Type),                intent(in)     :: cfl   ! Containing information
      type(Pattern_Type),             intent(in out) :: pt    ! Pattern
      integer,              optional, intent(in)     :: i_ini ! First lines to explore
      integer,              optional, intent(in)     :: i_end ! Last line to explore

      !---- Local variables ----!
      integer :: i,iv,j,j_ini,j_end,k,nw
      character(len=:), allocatable :: key,key_
      character(len=30), dimension(20) :: w

      !> Init
      call clear_error()
      if (cfl%nlines <=0) then
         err_CFML%Ierr=1
         err_CFML%Msg="Read_CFL_Pat_Type@CFML_IOForm: 0 lines "
         return
      end if

      j_ini=1
      j_end=cfl%nlines
      if (present(i_ini)) j_ini=i_ini
      if (present(i_end)) j_end=i_end

      do i = j_ini , j_end
         line = adjustl(cfl%line(i)%str)
         if (len_trim(line) == 0) cycle
         if (line(1:1) == "!" .or. line(1:1) == "#") cycle

         ! Eliminate Tabs
         do
            iv=index(line,TAB)
            if (iv == 0) exit
            line(iv:iv)=' '
         end do

         ! Remove comments at the end of the line
         j = index(line,"!")
         if (j /= 0) line = line(:j-1)

         ! Get the keyword
         j = index(line," ")
         key_ = line(:j-1)
         key = u_case(key_)
         k = index(key,'PATT_TYPE')
         if (k == 1) then
            call get_words(line,w,nw)
            if (nw < 3) then
               err_CFML%Ierr=1
               err_CFML%Msg="Read_CFL_Pat_Type@CFML_IOForm: Radiation and type must be specified in PATT_TYPE"
               return
            end if
            pt%patt_type = adjustl(trim(line(10:)))
            w(2) = u_case(w(2))
            if (trim(w(2)) == 'X-RAY' .or. trim(w(2)) == 'X-RAYS'.or. trim(w(2)) == 'X_RAYS') then
               pt%radiation = 'X'
               pt%pdat%kindrad = 'X-rays'
            else if (trim(w(2)) == 'NEUTRONS') then
               pt%radiation = 'N'
               pt%pdat%kindrad = 'Neutrons'
            else
               err_CFML%Ierr=1
               err_CFML%Msg="Read_CFL_Pat_Type@CFML_IOForm: Unknown radiation: "//w(2)
               return
            end if
            w(3) = u_case(w(3))
            if (trim(w(3)) == 'POWDER') then
               pt%sample = 'P'
            else if (trim(w(3)) == 'SINGLE-CRYSTAL' .or. trim(w(3)) == 'INTEGRATED_INTENSITIES') then
               pt%sample = 'X'
            else
               err_CFML%Ierr=1
               err_CFML%Msg="Read_CFL_Pat_Type@CFML_IOForm: Unknown sample/data type: "//w(3)
               return
            end if
            if (pt%radiation == 'N') then
               if (nw < 4) then
                  pt%mode = 'CW'
               else
                  w(4) = u_case(w(4))
                  if (w(4) == 'CW') then
                     pt%mode = 'CW'
                  else if (w(4) == 'TOF') then
                     pt%mode = 'TF'
                  else
                     err_CFML%Ierr=1
                     err_CFML%Msg="Read_CFL_Pat_Type@CFML_IOForm: Unknown mode: "//w(4)
                     return
                  end if
               end if
            else
               ! Energy is not implemented yet
               pt%mode = 'CW'
            end if
         end if
      end do

   End Subroutine Read_CFL_Pat_Type

   !!----
   !!---- READ_CFL_PHASE
   !!----
   !!---- Read phase information in a CFL file
   !!----
   !!---- 21/12/2024
   !!
   Module Subroutine Read_CFL_Phase(cfl, ph, atom_type, d, i_ini, i_end)
      !---- Arguments ----!
      Type(File_Type),                     intent(in)  :: cfl
      Type(Phase_Type),                    intent(out) :: ph
      character(len=*),                    intent(in)  :: atom_type
      integer,                             intent(in)  :: d
      integer,          optional,          intent(in)  :: i_ini
      integer,          optional,          intent(in)  :: i_end

      !---- Local variables ----!
      integer :: i,iv,j,j_ini,j_end,k,nw,ier
      character(len=:), allocatable :: key,key_
      character(len=2), dimension(20) :: w

      call clear_error()
      if (cfl%nlines <= 0) then
         err_CFML%Ierr=1
         err_CFML%Msg="Read_CFL_Phase@CFML_IOForm: 0 lines "
         return
      end if

      ph%mag=.false.
      j_ini=1
      j_end=cfl%nlines
      if (present(i_ini)) j_ini=i_ini
      if (present(i_end)) j_end=i_end

      call read_cfl_cell(cfl,ph%cell,i_ini=j_ini,i_end=j_end)
      if (err_cfml%ierr /= 0) return
      call read_cfl_atoms(cfl,ph%atm_list,atom_type,d,i_ini=j_ini,i_end=j_end)
      if (err_cfml%ierr /= 0) return
      call read_cfl_spg(cfl,ph%spg,i_ini=j_ini,i_end=j_end)
      if (err_cfml%ierr /= 0) return

      do i = j_ini , j_end
         line = adjustl(cfl%line(i)%str)
         if (len_trim(line) == 0) cycle
         if (line(1:1) == "!" .or. line(1:1) == "#") cycle

         ! Eliminate Tabs
         do
            iv=index(line,TAB)
            if (iv == 0) exit
            line(iv:iv)=' '
         end do

         ! Remove comments at the end of the line
         j = index(line,"!")
         if (j /= 0) line = line(:j-1)

         ! Get the keyword
         j = index(line," ")
         key_ = line(:j-1)
         key = u_case(key_)
         k = index(key,'PHASE_')
         if (k == 1) then
            ph%name = key_(7:)
            read(line(j:),*,iostat=ier) ph%iph  !Reading the number of the phase
            if(ier /= 0) then
               err_cfml%ierr = -1
               err_cfml%msg = "Read_CFL_Phase@CFML_IOForm: the number of the phase should be provied after the keyword as in: 'PHASE_namePhase iph'"
               return
            end if
            cycle
         end if

         line = adjustl(line(j+1:))

         select case(trim(key))
            case("PATTERNS")
               call get_words(line,w,nw)
               if (err_cfml%ierr /= 0) return
               if (allocated(ph%patterns)) deallocate(ph%patterns)
               allocate(ph%patterns(nw))
               do j = 1 , nw
                  read(unit=w(j),fmt=*) ph%patterns(j)
               end do
            case ("PHASE_")
               ph%name = key_(7:)
            case ("MAGNETIC")
               ph%mag = .true.
               if(index(u_case(line),"MAG_ONLY") /= 0) ph%mag_only = .true.
            case("SCALE_FACTORS")
               call get_words(line,w,nw)
               if (err_cfml%ierr /= 0) return
               if (allocated(ph%scale_factors)) deallocate(ph%scale_factors)
               allocate(ph%scale_factors(nw))
               do j = 1 , nw
                  read(unit=w(j),fmt=*) ph%scale_factors(j)
               end do
         end select
      end do

      if (allocated(ph%patterns)) then
         if (allocated(ph%scale_factors)) then
            if (size(ph%patterns) /= size(ph%scale_factors)) then
               err_cfml%ierr = -1
               err_cfml%msg = "Read_CFL_Phase@CFML_IOForm: patterns and scale factors must have the same dimensions"
               return
            end if
         else
            allocate(ph%scale_factors(size(ph%patterns)))
            ph%scale_factors(:) = 1.0
         end if
      end if

   End Subroutine Read_CFL_Phase

   !!----
   !!---- READ_CFL_SPG
   !!----
   !!---- Read Space group information ina CFL file
   !!----
   !!---- 07/05/2020
   !!
   Module Subroutine Read_CFL_SpG(cfl, SpG, xyz_type, i_ini, i_end, database_path)
      !---- Arguments ----!
      Type(File_Type),                 intent(in)     :: cfl
      class(SpG_Type), allocatable,    intent(out)    :: SpG
      character(len=*), optional,      intent(in)     :: xyz_type
      integer,          optional,      intent(in)     :: i_ini, i_end
      character(len=*), optional,      intent(in )    :: database_path

      !--- Local Variables ---!
      integer                           :: i,j,ngen,nk,nq,iv,ier,d,dr,Mult
      integer                           :: j_ini, j_end
      character(len=:),     allocatable :: uline,setting,strcode
      character(len=80), dimension(192) :: gen
      logical                           :: change_setting, trn_to

      !> Init
      call clear_error()

      if (cfl%nlines <= 0) then
         err_CFML%Ierr=1
         err_CFML%Msg="Read_CFL_Spg: 0 lines "
         return
      end if

      j_ini=1; j_end=cfl%nlines
      if (present(i_ini)) j_ini=i_ini
      if (present(i_end)) j_end=i_end

      !> Look for the appropriate keywords to construct the space group:
      !> Crystallographic, Shubnikov, or superspace
      ngen=0
      setting=" "
      change_setting=.false.
      trn_to = .false.

      strcode="xyz"
      if (present(xyz_type)) strcode=trim(xyz_type)

      do i=j_ini,j_end
         line=adjustl(cfl%line(i)%str)
         if (len_trim(line) == 0) cycle
         if (line(1:1) == "!" .or. line(1:1) == "#") cycle
         if (line(1:1) == " ") cycle

         !> Eliminate Tabs
         do
            iv=index(line,TAB)
            if (iv == 0) exit
            line(iv:iv)=' '
         end do

         j=index(line,"!")
         if (j /= 0) line=line(:j-1)

         j=index(line,"::")
         if (j /= 0) then
            iv=index(line,"trn_to")
            if(iv /= 0) then
               trn_to = .true.
               setting=trim(adjustl(line(j+2:iv-1)))
            else
               setting=trim(adjustl(line(j+2:)))
            end if
            if (len_trim(setting) /= 0) change_setting=.true.
            line=line(:j-1)
         end if

         j=index(line," ")
         uline=u_case(line(:j-1))

         line=adjustl(line(j+1:))
         !if(CFML_DEBUG) then
         !  write(*,"(a)") " Spg directive: "//trim(uline)
         !  write(*,"(a)") "         Value: "//trim(line)
         !end if
         select case(trim(uline))
            case("HALL","SPGR","SPACEG")
               allocate(SpG_Type :: Spg)
               if (present(database_path)) then
                  call Set_SpaceGroup(line, SpG, database_path=database_path)
               else
                  call Set_SpaceGroup(line, SpG)
               end if
               exit

            case("SHUB")
               allocate(SpG_Type :: Spg)
               if(change_setting) then
                  if(trn_to) then
                     if (present(database_path)) then
                        call Set_SpaceGroup(line,"SHUBN",SpG,setting=setting,trn_to=trn_to,database_path=database_path)
                     else
                        call Set_SpaceGroup(line,"SHUBN",SpG,setting=setting,trn_to=trn_to)
                     end if
                  else
                     if (present(database_path)) then
                        call Set_SpaceGroup(line,"SHUBN",SpG,setting=setting,database_path=database_path)
                     else
                        call Set_SpaceGroup(line,"SHUBN",SpG,setting=setting)
                     end if
                  end if
               else
                  if (present(database_path)) then
                     call Set_SpaceGroup(line,"SHUBN",SpG,database_path=database_path)
                  else
                     call Set_SpaceGroup(line,"SHUBN",SpG)
                  end if
               end if
               exit

            case("SSG","SUPER","SSPG")
               allocate(SuperSpaceGroup_Type :: Spg)
               if (present(database_path)) then
                  call Set_SpaceGroup(line,"SUPER",SpG, strcode, database_path=database_path)
               else
                  call Set_SpaceGroup(line,"SUPER",SpG, strcode)
               end if
               d=SpG%d
               exit

            case("GENLIST","GENERATORS","LIST")
               j=index(line,";")
               if(j == 0) then
                 d=Get_Dimension_SymmOp(trim(line))
               else
                 d=Get_Dimension_SymmOp(trim(line(1:j-1)))
               end if
               if(d > 4) then
                 allocate(SuperSpaceGroup_Type :: Spg)
               else
                 allocate(SpG_Type :: Spg)
               end if
               if (present(database_path)) then
                  call Set_SpaceGroup(line,SpG,database_path=database_path)
               else
                  call Set_SpaceGroup(line,SpG)
               end if
               exit

            case("GEN","GENR","SYMM")
               ngen=ngen+1
               gen(ngen)=line

         end select
      end do

      if (ngen > 0) then
        d=Get_Dimension_SymmOp(trim(gen(1)))
        if(d > 4) then
          allocate(SuperSpaceGroup_Type :: Spg)
        else
          allocate(SpG_Type :: Spg)
        end if
        if (present(database_path)) then
         call Set_SpaceGroup("  ",SpG,ngen,gen,database_path=database_path)
        else
         call Set_SpaceGroup("  ",SpG,ngen,gen)
        end if
      end if

      if (Err_CFML%Ierr == 1) return

      if (change_setting) then
         if (strcode == "xyz")  then
            call Change_Setting_SpaceG(setting, SpG)
         else
            call Change_Setting_SpaceG(setting, SpG, strcode)
         end if
      end if
      if (Err_CFML%Ierr == 1) return

      !> Now read q-vectors and other items if the class of SpG is SuperSpaceGroup_Type
      Select Type (SpG)
         Class is (SuperSpaceGroup_Type)
            dr=d-1
            nk=dr-3
            Mult=SpG%Multip
            if (allocated(SpG%kv))      deallocate (SpG%kv)
            if (allocated(SpG%nharm))   deallocate (SpG%nharm)
            if (allocated(SpG%sintlim)) deallocate (SpG%sintlim)
            if (allocated(SpG%q_coeff)) deallocate (SpG%q_coeff)
            if (allocated(SpG%Rot)) deallocate (SpG%Rot)
            if (allocated(SpG%t)) deallocate (SpG%t)
            if (allocated(SpG%tI)) deallocate (SpG%tI)
            if (allocated(SpG%M)) deallocate (SpG%M)
            if (allocated(SpG%Ep)) deallocate (SpG%Ep)


            allocate(SpG%Rot(3,3,Mult),SpG%t(3,Mult),SpG%tI(nk,Mult),SpG%M(nk,3,Mult),SpG%Ep(nk,nk,Mult))

            do i=1,Mult
                SpG%Rot(:,:,i)=SpG%Op(i)%Mat(1:3,1:3)
                SpG%t    (:,i)=SpG%Op(i)%Mat(1:3,d)
                SpG%tI   (:,i)=SpG%Op(i)%Mat(4:dr,d)
                SpG%M  (:,:,i)=SpG%Op(i)%Mat(4:dr,1:3)
                SpG%Ep (:,:,i)=SpG%Op(i)%Mat(4:dr,4:dr)
            end do

            nk=0; nq=0
            do i=j_ini,j_end
               line=adjustl(cfl%line(i)%str)
               if (len_trim(line) == 0) cycle
               if (line(1:1) == "!" .or. line(1:1) == "#") cycle

               j=index(line,"!")
               if (j /= 0) line=line(:j-1)

               j=index(line," ")
               uline=u_case(line(:j-1))

               line=adjustl(line(j+1:))
               select case(trim(uline))
                  case("NQVECT","NKVECT")
                     read(unit=line,fmt=*,iostat=ier) Spg%nk, Spg%nq
                     if (ier /= 0) then
                        Err_CFML%Ierr=1
                        Err_CFML%Msg="Error reading the number of k-vectors and/or number of Q-coefficients"
                        return
                     end if
                     allocate(Spg%kv(3,Spg%nk),Spg%q_coeff(Spg%nk,Spg%nq))
                     allocate(Spg%nharm(Spg%nk),Spg%sintlim(Spg%nk))
                     SpG%kv=0.0_cp; SpG%q_coeff=1; Spg%nharm=1; Spg%sintlim=1.0

                  case("QVECT","KVECT")
                     if (Spg%nk > 0) then
                        nk=nk+1
                        read(unit=line,fmt=*,iostat=ier) Spg%kv(:,nk)
                        if (ier /= 0) then
                           Err_CFML%Ierr=1
                           write(unit=Err_CFML%Msg,fmt="(a,i2)") "Error reading the k-vector #",nk
                           return
                        end if
                     end if

                  case("NHARM")
                     if (Spg%nk > 0) then
                        read(unit=line,fmt=*,iostat=ier) Spg%nharm(1:Spg%nk)
                        if (ier /= 0) then
                           Err_CFML%Ierr=1
                           Err_CFML%Msg = "Error reading the nk harmonics !"
                           return
                        end if
                     end if

                  case("SINTL")
                     if (Spg%nk > 0) then
                        read(unit=line,fmt=*,iostat=ier) Spg%sintlim(1:Spg%nk)
                        if (ier /= 0) then
                           Err_CFML%Ierr=1
                           Err_CFML%Msg = "Error reading the maximum sinTheta/Lambda for harmonics!"
                           return
                        end if
                     end if

                  case("Q_COEFF")
                     nq=nq+1
                     read(unit=line,fmt=*,iostat=ier) Spg%q_coeff(1:Spg%nk,nq)
                     if (ier /= 0) then
                        Err_CFML%Ierr=1
                        write(unit=Err_CFML%Msg,fmt="(a,i2)") "Error reading the Q-coefficent # ",nq
                        return
                     end if

               end select
            end do

            if (Spg%nk /= (Spg%D-4)) then
               Err_CFML%Ierr=1
               write(unit=Err_CFML%Msg,fmt="(2(a,i2))") "The number of k-vectors,",Spg%nk, ", does not correspond with the additional dimensions of the group ",Spg%D-4
               return
            end if

            if (Spg%nq /= nq) then
               Err_CFML%Ierr=1
               write(unit=Err_CFML%Msg,fmt="(2(a,i2))") "The number of expected Q-coefficients,",Spg%nq, ", does not correspond with number of read Q-coefficients ",nq
               return
            end if

      End Select

   End Subroutine Read_CFL_SpG

   !!----
   !!---- WRITE_CFL_ATOMS
   !!----
   !!----    Write the atoms in the asymmetric unit for a CFL file
   !!----
   !!---- 08/05/2020
   !!
   Module Subroutine Write_CFL_Atoms(AtmList, Lun, Cell)
      !---- Arguments ----!
      Type(AtList_Type),            intent(in) :: AtmList
      integer,            optional, intent(in) :: Lun
      class(Cell_G_Type), optional, intent(in) :: Cell

      !---- Local Variables ----!
      character(len=36)              :: forma
      character(len=30),dimension(6) :: text
      real(kind=cp), dimension(6)    :: u,bet,sb
      integer                        :: i, j, iunit !, maxl, leng

      !> Unit
      iunit=6
      if (present(lun)) iunit=lun

      if (AtmList%natoms == 0) then
         write (unit=iunit,fmt="(a)") " There are no atoms defined!"
         return
      end if

      !> Determine the maximum length of the atom labels
      !maxl=0
      !do i=1,AtmList%natoms
      !   leng=len_trim(atmList%atom(i)%lab)
      !   if (leng > maxl) maxl=leng
      !end do
      !maxl=max(maxl,4)+1

      !> define format forma
      forma="(a,a8,tr2,a4,tr3,5a14,2f9.2,tr3,a)"
      !select case(maxl)
      !   case(:9)
      !      write(unit=forma(5:5),fmt="(i1)") maxl
      !   case(10:)
      !      write(unit=forma(5:6),fmt="(i2)") maxl
      !end select

      write (unit=iunit,fmt="(a)")  &
      "!    Atom      Type     x/a           y/b           z/c           Biso          Occ             Spin    Charge    Info"

      select type (at => AtmList%atom)
         class is (Atm_Std_Type)
            do i=1,AtmList%natoms

               do j=1,3
                  text(j)=string_NumStd(at(i)%x(j), at(i)%x_std(j))
               end do
               text(4)=string_NumStd(at(i)%U_iso, at(i)%U_iso_std)
               text(5)=string_NumStd(at(i)%Occ, at(i)%Occ_std)

               if(at(i)%magnetic) then
                   write (unit=iunit,fmt=forma) "Atom ",at(i)%lab,at(i)%SfacSymb, &
                     (text(j),j=1,5), at(i)%mom, real(at(i)%charge), " # "//trim(at(i)%AtmInfo)
                   write (unit=iunit,fmt="(a,3f14.5)") "Moment          ",at(i)%moment
               else
                   write (unit=iunit,fmt=forma) "Atom ",at(i)%lab,at(i)%ChemSymb, &
                     (text(j),j=1,5), at(i)%mom, real(at(i)%charge), " # "//trim(at(i)%AtmInfo)
               end if

               select case (l_case(at(i)%ThType))
                  case ('ani')
                     select case (l_case(at(i)%UType))
                        case ('beta')
                           u=at(i)%u(1:6)
                           if (present(cell)) then
                              bet=Get_U_from_Betas(u,cell)
                              sb=Get_U_from_Betas(at(i)%u_std,cell)
                           else
                              bet=u
                               sb=at(i)%u_std(1:6)
                           end if
                           do j=1,6
                              text(j)=string_NumStd(bet(j), sb(j))
                           end do
                           if (present(cell)) then
                              write(unit=iunit,fmt="(a,6a14)") "!U_ij  ", text
                           else
                              write (unit=iunit,fmt="(a,tr1,6a14)") "Beta  ", text
                           end if

                        case ('u_ij')
                           u=at(i)%u(1:6)
                           if (present(cell)) then
                              bet=Get_Betas_from_U(u,cell)
                              sb=Get_Betas_from_U(at(i)%u_std,cell)
                           else
                              bet=u
                               sb=at(i)%u_std(1:6)
                           end if
                           do j=1,6
                              text(j)=string_NumStd(bet(j), sb(j))
                           end do
                           if (present(cell)) then
                              write(unit=iunit,fmt="(a,6a14)") "!Beta  ", text
                           else
                              write (unit=iunit,fmt="(a,tr1,6a14)") "U_ij  ", text
                           end if

                     end select
               end select
            end do
      end select

   End Subroutine Write_CFL_Atoms

   !!----
   !!---- Write_CFL_File
   !!----
   !!----    Write a CFL file
   !!----
   !!---- 08/05/2020
   !!
   Module Subroutine Write_CFL_File(Lun,Cell, SpG, Atm, Title,info_lines)
      !---- Arguments ----!
      integer,                               intent(in)    :: lun
      class(Cell_G_Type),                    intent(in)    :: Cell
      class(SpG_Type),                       intent(in)    :: SpG
      Type(AtList_Type), optional,           intent(in)    :: Atm
      character(len=*),  optional,           intent(in)    :: Title
      character(len=*),dimension(:),optional,intent(in)    :: info_lines
      !----- Local variables -----!
      integer                         :: i,j
      real(kind=cp), dimension(6)     :: a,sa
      character(len=30), dimension(6) :: text

      !> Title
      if (present(title)) write(unit=lun,fmt="(a)") "TITLE "//trim(title)

      write(unit=lun,fmt='(a)')" "
      write(unit=lun,fmt='(a)') "!  Automatically generated CFL file (Write_CFL_file)"
      write(unit=lun,fmt='(a)')" "

      !> Cell
      a(1:3)=Cell%Cell
      a(4:6)=Cell%ang
      sa(1:3)=Cell%scell
      sa(4:6)=Cell%sang
      do j=1,6
         text(j)=string_NumStd(a(j), sa(j))
      end do
      write(unit=lun,fmt="(a)") "!         a               b               c            alpha           beta            gamma"
      write(unit=lun,fmt="(a,6a16)") "Cell ",text
      write(unit=lun,fmt='(a)')" "

      !> Space group
      if(SpG%magnetic) then
        write(unit=lun,fmt="(a)")"! Magnetic Space Group, BNS number: "//SpG%BNS_num
        write(unit=lun,fmt="(a)")"!                       BNS symbol: "//SpG%BNS_symb
        if(len_trim(SpG%UNI) /= 0) write(unit=lun,fmt="(a)")"!                       UNI symbol: "//SpG%UNI
        if(SpG%standard_setting) then
          write(unit=lun,fmt="(a)") "SHUB "//SpG%BNS_symb
        else
          write(unit=lun,fmt="(a)") "SHUB "//SpG%BNS_symb//" :: "//trim(SpG%Matfrom)
          do i=1,SpG%Multip
             write(unit=lun,fmt="(a)") "SYMM "//trim(Spg%Symb_Op(i))
          end do
        end if
        write(unit=lun,fmt="(a)")"  "
      else
        write(unit=lun,fmt="(a,i3)")"!     Space Group # ",SpG%NumSpg
        write(unit=lun,fmt="(a,a)") "Spgr  ",SpG%spg_symb
        write(unit=lun,fmt='(a)')" "
      end if

      !> Atoms
      if (present(Atm)) then
         call Write_CFL_Atoms(Atm,Lun,cell)
         write(unit=lun,fmt='(a)')" "
      end if
      if(present(info_lines)) then
        j=0
        write(unit=lun,fmt="(a)") "!"
        do
         j=j+1
         write(unit=lun,fmt="(a)") trim(info_lines(j))
         if(u_case(info_lines(j)(1:14)) == "END_INFO_LINES" .or. j > 100) exit
        end do
      end if

   End Subroutine Write_CFL_File

   !!----
   !!---- Write_CFL_Pattern
   !!----
   !!----    Write a CFL pattern
   !!----
   !!---- 15/01/2025
   !!
   Module Subroutine Write_CFL_Pattern(Lun,Pat,iPat)
      !---- Arguments ----!
      integer,            intent(in) :: lun
      type(Pattern_Type), intent(in) :: Pat
      integer, optional,  intent(in) :: iPat

      !----- Local variables -----!
      integer                       :: i_pat,i
      real(kind=cp)                 :: sig
      character(len=:), allocatable :: profile

      i_pat = 1
      if (present(iPat)) i_pat = iPat

      if (allocated(pat%name)) then
         write(lun,'(/,a,i3)') 'PATTERN_'//trim(pat%name)//"   This is the pattern numbered: ",i_pat
      else
         write(lun,'(/,a)') 'PATTERN_'
      end if
      write(lun,'(2x,2a)') 'Patt_Type ',pat%patt_type
      write(lun,"(2x,a)")        " =>    Pattern Title: "//trim(pat%pDat%Title)
      write(lun,"(2x,a)")        " =>   Radiation Type: "//trim(pat%pDat%KindRad)
      write(lun,"(2x,a)")        " =>  Scatt. variable: "//trim(pat%pDat%ScatVar)
      write(lun,"(2x,a,2f12.4)") " =>       Xmin, Xmax: ",pat%pDat%xmin,pat%pDat%xmax
      write(lun,"(2x,a,2f12.4)") " =>       Ymin, Ymax: ",pat%pDat%ymin,pat%pDat%ymax
      write(lun,"(2x,a,i12)")    " => Number of points: ",pat%pDat%npts
      write(lun,"(2x,a,5f12.4)") " => W1, W2, Dtt1, ..: ",pat%pDat%Wave

      select type (c => pat%cond)
         class is (PowPatt_CW_Conditions_Type)
            write(lun,'(2x,2a)') 'Profile_function ',c%profile
            profile = u_case(c%profile)
            if (profile == 'TCH_PVOIGT') then
               write(lun,'(2x,a, f12.6)')  ' => Bragg contribution (WDT*FWHM), WDT: ',c%wdt
               write(lun,'(2x,a,3f12.6)')  ' =>                 Zero, SyCos, SySin: ',c%zero, c%SyCos, c%SySin
               write(lun,'(2x,a,2f12.6)')  ' =>               Asymmetry (S_L, D_L): ',c%asym1, c%asym2
               write(lun,'(2x,a,3f12.6)')  ' =>            Lambda1, Lambda2, ratio: ',c%lambda, c%ratio
               write(lun,'(2x,a,3f12.6)')  ' =>      2Theta_Range (min, max, step): ',c%tthmin,c%tthmax,c%step
               write(lun,'(2x,a,3f12.6)')  ' => Gaussian   FWHM-TCH_pVoigt (U,V,W): ',c%u,c%v,c%w
               write(lun,'(2x,a,3f12.6)')  ' => Lorentzian FWHM-TCH_pVoigt   (X,Y): ',c%x,c%y
            end if
         class is (PowPatt_TOF_Conditions_Type)
            write(lun,'(2x,2a)') 'Profile_function ',c%profile
            profile = u_case(c%profile)
            if (profile == 'CONV_B2B_EXP*PVOIGT') then
               write(lun,'(2x,a, f12.6)')  ' => Bragg contribution (WDT*FWHM), WDT: ',c%wdt
               write(lun,'(2x,a,f12.4,a)') ' =>                         Bank_Angle: ',c%bank_angle," degrees"
               write(lun,'(2x,a,4f12.6)')  ' =>       Zero, Dtt1, Dtt2, Dtt_1overD: ',c%Zero, c%Dtt1, c%Dtt2, c%Dtt_1overD
               write(lun,'(2x,a,3f12.3)')   ' =>         TOF_Range (min, max, step): ',c%tof_min,c%tof_max,c%step
               write(lun,'(2x,a,3f12.6)')  ' =>             Alpha0, Alpha1, AlphaQ: ',c%Alpha0, c%Alpha1, c%AlphaQ
               write(lun,'(2x,a,3f12.6)')  ' =>              Beta0,  Beta1,  BetaQ: ',c%Beta0, c%Beta1, c%BetaQ
               write(lun,'(2x,a,4f12.6)')  ' =>     Sigma0, Sigma1, Sigma2, SigmaQ: ',c%Sigma0, c%Sigma1, c%Sigma2,c%SigmaQ
               write(lun,'(2x,a,3f12.6)')  ' =>           Gamma0,  Gamma1,  Gamma2: ',c%Gamma0, c%Gamma1, c%Gamma2
            end if
      end select

      write(lun,"(/2x,a)")       " =>  First 10 points and last point (Scatt_Var, Intensity, Sigma) "
      do i=1,10
         sig=0.0
         if(allocated(pat%pDat%sigma)) sig=sqrt(pat%pDat%sigma(i))
         write(lun,"(3f12.4)") pat%pDat%x(i),pat%pDat%y(i),sig
      end do
      write(lun,"(a)") "    . . . . . . . "
      sig=0.0
      if(allocated(pat%pDat%sigma)) sig=pat%pDat%sigma(pat%pDat%npts)
      sig=sqrt(sig)
      write(lun,"(3f12.4)") pat%pDat%x(pat%pDat%npts),pat%pDat%y(pat%pDat%npts),sig
      write(lun,"(a)") "    "
      if (allocated(pat%name)) then
         write(lun,'(a)') 'END_PATTERN_'//pat%name
      else
         write(lun,'(a)') 'END_PATTERN_'
      end if

   End Subroutine Write_CFL_Pattern

   !!----
   !!---- Write_CFL_Phase
   !!----
   !!----    Write a CFL phase
   !!----
   !!---- 22/12/2024
   !!
   Module Subroutine Write_CFL_Phase(Lun,Ph,iPh)
      !---- Arguments ----!
      integer,           intent(in) :: lun
      type(Phase_Type),  intent(in) :: Ph
      integer, optional, intent(in) :: iPh

      !----- Local variables -----!
      integer :: i,j,k,i_ph

      i_ph = ph%iph
      if (present(iPh)) i_ph = iPh
      if (allocated(ph%name)) then
         write(lun,'(a,1x,a)') 'PHASE name:',ph%name
         write(lun,'(a,1x,i3)') 'PHASE number:',i_ph
      else
         write(lun,'(a,i3)') 'PHASE: ',i_ph
      end if

      call write_crystal_cell(ph%cell,lun)
      call write_spacegroup_info(ph%spg,lun)
      call write_atom_list(ph%atm_list,i_ph,lun)

      if (allocated(ph%patterns)) then
         write(lun,'(/,4x,a)',advance='no') 'PATTERNS:'
         do i = 1 , size(ph%patterns)
            write(lun,'(2x,i14)',advance='no') ph%patterns(i)
         end do
         write(lun,*)
         write(lun,'(/,4x,a)',advance='no') 'SCALE_FACTORS:'
         do i = 1 , size(ph%patterns)
            write(lun,'(2x,f14.6)',advance='no')  ph%scale_factors(i)
         end do
         write(lun,*)
      end if

      if (allocated(ph%pow)) then   !Powder attributes have been allocated and read
         write(lun,'(/,4x,a)',advance='no') ' Powder Attributes:'
         do i = 1 , size(ph%patterns)
            if(ph%pow(i)%powder) then
               do j=1,ph%Ncontr
                  if(ph%patterns(j) == i) then
                     write(lun,"(a)")    "  ========="
                     write(lun,"(a,i3)") "  Pattern # ",i
                     write(lun,"(a)")    "  ========="

                     if(ph%pat_mode(j) == "TF") write(lun,"(a,f14.6)") "           Primay Extinction: ",ph%pow(j)%Extinct
                     write(lun,"(a,f14.6)")      " Isotropic Size & GaussFract: ",ph%pow(j)%iso_size,   ph%pow(j)%Gauss_iso_size_frac
                     write(lun,"(a,2f14.6)")     " Isotropic Strain & LorFract: ",ph%pow(j)%iso_strain, ph%pow(j)%Lorentz_iso_strain_frac
                     if(len_trim(ph%pow(j)%aniso_size_model) /= 0) then
                         write(lun,"(a,i3)")      "  Anisotropic size model # ",ph%pow(j)%aniso_size_model
                         write(lun,"(a,15f12.6)") "  Anisotropic size parameters: ",ph%pow(j)%aniso_size(1:ph%pow(j)%Nani_size)
                     end if
                     if(len_trim(ph%pow(j)%aniso_strain_model) /= 0) then
                         write(lun,"(a,i3)")      "  Anisotropic strain model # ",ph%pow(j)%aniso_strain_model
                         write(lun,"(a,15f12.6)") "  Anisotropic strain parameters: ",ph%pow(j)%aniso_strain(1:ph%pow(j)%Nani_strain)
                     end if
                     if(ph%pow(j)%n_pref > 0) then
                        do k=1,ph%pow(j)%n_pref
                             write(lun,"(2(a,3f12.6))") "  Preferred Orientation  axis: ",ph%pow(j)%axes_pref(:,k) , "  Pref.Or. Value & Fraction:", ph%pow(j)%pref(:,k)
                        end do
                     end if
                     if(ph%pat_mode(j) == "TF") write(lun,"(a,2f14.6)") "        Absorption correction: ",ph%pow(j)%abs_corr
                  end if !ph%patterns(j) == i
               end do ! j=1,ph%Ncontr
            end if ! ph%pow(i)%powder
         end do
      end if
   End Subroutine Write_CFL_Phase

   !!--++
   !!--++ READ_XTAL_CFL
   !!--++
   !!--++ Read Crystal Information in a CFL File
   !!--++
   !!--++ 10/05/2020
   !!
   Module Subroutine Read_XTal_CFL(cfl, Cell, SpG, AtmList, Atm_Typ, Nphase, CFrame, Job_Info,database_path)
      !---- Arguments ----!
      type(File_Type),               intent(in)  :: cfl
      class(Cell_G_Type),allocatable,intent(out) :: Cell
      class(SpG_Type),   allocatable,intent(out) :: SpG
      Type(AtList_Type),             intent(out) :: Atmlist
      character(len=*),    optional, intent(in)  :: Atm_Typ
      Integer,             optional, intent(in)  :: Nphase   ! Select the Phase to read
      character(len=*),    optional, intent(in)  :: CFrame
      Type(Job_Info_type), optional, intent(out) :: Job_Info
      character(len=*),    optional,   intent(in)  :: database_path

      !---- Local variables ----!
      logical                          :: set_moment, set_ModAtm_std
      integer, dimension(MAX_PHASES)   :: ip
      integer                          :: i, j,nt_phases, iph, n_ini, n_end
      integer                          :: k

      real(kind=cp),dimension(:),allocatable:: xvet

      type(kvect_info_Type)            :: Kvec
      logical                          :: commands

      !> Init
      call clear_error()
      if (cfl%nlines <=0) then
         err_CFML%Ierr=1
         err_CFML%Msg="Read_XTal_CFL: No lines in the file!"
         return
      end if

      commands=.false.

      !> Calculating number of Phases
      nt_phases=0; ip=cfl%nlines; ip(1)=1
      do i=1,cfl%nlines
         line=u_case(adjustl(cfl%line(i)%str))
         if (len_trim(line) <= 0) cycle
         if (line(1:1) =='!') cycle
         if (line(1:1) ==' ') cycle

         if(index(line,"COMMANDS") /= 0 .or. index(line,"VARY") /= 0 .or. index(line,"FIX") /= 0) commands=.true.
         if (line(1:6) == 'PHASE_') then
            nt_phases=nt_phases+1
            ip(nt_phases)=i
         end if
      end do
      if (nt_phases == 0) nt_phases=1

      !> Read the Phase information
      iph=1
      if (present(nphase)) then
         iph=min(nphase, nt_phases)
         iph=max(1,iph)
      end if
      n_ini=ip(iph)
      n_end=ip(iph+1)  !-1 This was an error that eliminates a line when Nphase is not present

      do i=n_ini,n_end
        line=u_case(adjustl(cfl%line(i)%str))
        if(line(1:9) == "END_PHASE") then
          n_end=i
          exit
        end if
      end do

      !if(CFML_DEBUG) then
      !  write(*,"(a,i3)") " Reading phase: ", iph
      !  write(*,"(a,i3)") "    Start line: ", n_ini
      !  write(*,"(a,i3)") "      End line: ", n_end
      !end if

      if (present(Job_Info)) then
         call Get_Job_Info(cfl,Job_info, n_ini, n_end)
      end if

      !> Reading Cell Parameters
      if (present(CFrame)) then
         if(commands) then
            call read_cfl_cell(cfl, Cell, CFrame,n_ini,n_end,commands)
         else
            call read_cfl_cell(cfl, Cell, CFrame,n_ini,n_end)
         end if
      else
         if(commands) then
            call read_cfl_cell(cfl, Cell, i_ini=n_ini,i_end=n_end,cmd=commands)
         else
            call read_cfl_cell(cfl, Cell, i_ini=n_ini,i_end=n_end)
         end if
      end if
      if (Err_CFML%IErr == 1) return

      !> Reading Space groups
      !if(CFML_DEBUG) then
      !  write(*,"(a)") " Reading Space Group in lines: "
      !  do i=n_ini,n_end
      !    write(*,"(a,i4,a)") "    Line: ",i,"  "//trim(cfl%line(i)%str)
      !  end do
      !end if

      if (present(database_path)) then
         call read_CFL_SpG(cfl,SpG, i_ini=n_ini, i_end=n_end, database_path=database_path)
      else
         call read_CFL_SpG(cfl,SpG, i_ini=n_ini, i_end=n_end)
      end if
      if (Err_CFML%IErr == 1 .or. .not. allocated(SpG)) return

      !> Read Atoms information
      set_moment=.false.
      set_ModAtm_std=.false.
      !if(CFML_DEBUG) then
      !  write(*,"(a)") " Reading Atoms in lines: "
      !  do i=n_ini,n_end
      !    write(*,"(a,i4,a)") "    Line: ",i,"  "//trim(cfl%line(i)%str)
      !  end do
      !end if
      do i=n_ini,n_end
         line=adjustl(cfl%line(i)%str)

         if (len_trim(line) <=0) cycle
         if (line(1:1) == '!') cycle
         if (line(1:1) == ' ') cycle

         if (u_case(line(1:4)) /= 'ATOM') cycle

         do j=i+1,n_end
            line=adjustl(cfl%line(j)%str)
            if (len_trim(line) <= 0) cycle
            if (line(1:1) == '!') cycle

            if (l_case(line(1:4)) == 'atom') exit
            if (l_case(line(1:6)) == 'moment') set_moment=.true.
            if (l_case(line(2:4)) == '_cs')    set_ModAtm_std=.true.
         end do
      end do

      if(SpG%d == 4) set_ModAtm_std=.false.

      if(present(Atm_Typ)) then
        call read_cfl_Atoms(cfl,AtmList,Atm_Typ,0,n_ini,n_end)
      else
         if ((.not. set_moment) .and. (.not. set_ModAtm_std)) then
            !> Type of Atoms: Atm_std or Atm_Ref
            If(commands) Then
               call read_cfl_Atoms(cfl,AtmList,'Atm_Ref_type',0,n_ini,n_end)
            Else
               call read_cfl_Atoms(cfl,AtmList,'Atm_std_type',0,n_ini,n_end)
            End If

         else if (set_moment .and. (.not. set_ModAtm_std)) then
            !> Type of Atoms: Atm_std
            If(commands) Then
               call read_cfl_Atoms(cfl,AtmList,'Atm_Ref_type',0,n_ini,n_end)
            Else
               call read_cfl_Atoms(cfl,AtmList,'Atm_std_type',0,n_ini,n_end)
            End If

         else if (set_moment .and. set_ModAtm_std) then
            !> Type of Atoms: ModAtm_std
            call read_cfl_kvectors(cfl,kvec,n_ini,n_end)
            if (err_CFML%Ierr ==1) return
            If(commands) Then
               call read_cfl_Atoms(cfl,AtmList,'ModAtm_Ref_type',Kvec%nk,n_ini,n_end)
            Else
               call read_cfl_Atoms(cfl,AtmList,'ModAtm_std_type',Kvec%nk,n_ini,n_end)
            End If

         else
            !> Type of atoms not defined
            err_CFML%Ierr=1
            err_CFML%Msg="Read_XTal_CFL: Impossible to define the type of Atoms. Please, check it!"
            return
         end if
      end if
      if (allocated(xvet)) deallocate(xvet)
      Select Type (SpG)
         type is (SuperSpaceGroup_Type)
             allocate(xvet(SpG%D-1))
             do i=1,Atmlist%natoms
                xvet(1:3)=Atmlist%atom(i)%x
                do k=1,Spg%nk
                   xvet(3+k)=dot_product(xvet(1:3),SpG%kv(:,k))
                end do
                Atmlist%atom(i)%Mult=Get_Multip_Pos(xvet,SpG)
                if (Atmlist%atom(i)%occ < EPSV) Atmlist%atom(i)%occ=real(Atmlist%atom(i)%Mult)/real(SpG%Multip)
                select type (at => Atmlist%atom(i))
                   class is (ModAtm_Std_Type)
                      At%Xs=xvet
                end select
             end do

         class Default
             allocate(xvet(3))
             do i=1,Atmlist%natoms
                xvet=Atmlist%atom(i)%x
                Atmlist%atom(i)%Mult=Get_Multip_Pos(xvet,SpG)
                if (Atmlist%atom(i)%occ < EPSV) Atmlist%atom(i)%occ=real(Atmlist%atom(i)%Mult)/real(SpG%Multip)
             end do
      End Select

      !> Convert Us to Betas and Uiso to Biso
      do i=1,AtmList%natoms
         select case (AtmList%atom(i)%thtype)
            case ("iso")

               if(l_case(Atmlist%atom(i)%utype) == "u_ij") then  !Only multiply by 8 pi^2 when Utype is explicitly provided
                 Atmlist%atom(i)%u_iso= Atmlist%atom(i)%u_iso*78.95683521
               end if

            case ("ani")

               Atmlist%atom(i)%u_iso= Atmlist%atom(i)%u(1)*78.95683521 !by default

               select type (cell)
                  class is (Cell_G_Type)
                     Atmlist%atom(i)%u_iso=U_Equiv(cell,Atmlist%atom(i)%u(1:6))  ! Uequi
                     Atmlist%atom(i)%u_iso= Atmlist%atom(i)%u_iso*78.95683521

                     select case (l_case(Atmlist%atom(i)%Utype))
                        case ("u_ij")
                           Atmlist%atom(i)%u(1:6) =  Get_Betas_from_U(Atmlist%atom(i)%u(1:6),Cell)

                        case ("b_ij")
                           Atmlist%atom(i)%u(1:6) = Get_Betas_from_B(Atmlist%atom(i)%u(1:6),Cell)
                     end select
               end select
               Atmlist%atom(i)%Utype="beta"

            case default

               !Atmlist%atom(i)%u_iso = Atmlist%atom(i)%u_iso*78.95683521
               Atmlist%atom(i)%thtype = "iso"
               Atmlist%atom(i)%Utype="b_ij"

         end select
      end do

   End Subroutine Read_XTal_CFL

   !!----
   !!---- GET_JOB_INFO
   !!----
   !!----    Constructor of the object Job_info.
   !!----
   !!---- 10/05/2020
   !!
   Module Subroutine Get_Job_Info(cfl,Job_info, i_ini,i_end)
      !---- Arguments ----!
      type(File_Type),      intent(in)  :: cfl              ! Containing information
      type(job_info_type),  intent(out) :: Job_info         ! Object to be constructed
      integer,              intent(in)  :: i_ini, i_end     ! Lines to explore

      !---- Local Variables ----!
      integer                           :: i,nphas, ncmd,n_pat,ier, j
      integer, dimension(i_end-i_ini+1) :: ip,ic,ipt
      real(kind=sp)                     :: a1,a2,a3,a4,a5
      character(len=120)                :: fmtfields, fmtformat

      !> Init
      if (cfl%nlines <=0) then
         err_CFML%Ierr=1
         err_CFML%Msg="Get_Job_Info: 0 lines "
         return
      end if

      !> Initialize FindFMT
      call Init_FindFMT(i_ini)

      nphas=0
      ncmd=0
      n_pat=0
      ip=i_end
      ic=0
      ipt=0

      Job_info%title=" General Job: CrysFML"
      Job_info%Num_Patterns=1

      do i=i_ini,i_end
         line=u_case(adjustl(cfl%line(i)%str))

         if (line(1:5) == "TITLE") Job_info%title=line(7:)

         if (line(1:5) == "NPATT") then
            read(unit=line(7:), fmt=*,iostat=ier) Job_info%Num_Patterns
            if (ier /= 0) Job_info%Num_Patterns=1
         end if

         if (line(1:6) == "PHASE_") then
            nphas=nphas+1
            ip(nphas)=i
         end if

         if (line(1:4) == "CMDL") then
            ncmd=ncmd+1
            ic(ncmd)=i
         end if

         if (line(1:5) == "PATT_") then
            n_pat=n_pat+1
            ipt(n_pat)=i
         end if
      end do

      if (nphas == 0) then
         nphas=1
         ip(nphas)=0
      end if
      if (n_pat == 0) then
         n_pat=1
         ipt(n_pat) = 0
      end if

      if (Job_info%Num_Patterns /= n_pat) Job_info%Num_Patterns = n_pat
      Job_info%Num_Phases=nphas
      Job_info%Num_Cmd=ncmd

      if (allocated(Job_Info%Patt_typ))     deallocate(Job_Info%Patt_typ)
      if (allocated(Job_Info%Phas_nam))     deallocate(Job_Info%Phas_nam)
      if (allocated(Job_Info%range_stl))    deallocate(Job_Info%range_stl)
      if (allocated(Job_Info%range_q))      deallocate(Job_Info%range_q)
      if (allocated(Job_Info%range_d))      deallocate(Job_Info%range_d)
      if (allocated(Job_Info%range_2theta)) deallocate(Job_Info%range_2theta)
      if (allocated(Job_Info%range_energy)) deallocate(Job_Info%range_energy)
      if (allocated(Job_Info%range_tof))    deallocate(Job_Info%range_tof)
      if (allocated(Job_Info%lambda))       deallocate(Job_Info%lambda)
      if (allocated(Job_Info%ratio))        deallocate(Job_Info%ratio)
      if (allocated(Job_Info%dtt1))         deallocate(Job_Info%dtt1)
      if (allocated(Job_Info%dtt2))         deallocate(Job_Info%dtt2)

      allocate(Job_Info%Patt_typ(n_pat))
      allocate(Job_Info%Phas_nam(nphas))
      allocate(Job_Info%range_stl(n_pat))
      allocate(Job_Info%range_q(n_pat))
      allocate(Job_Info%range_d(n_pat))
      allocate(Job_Info%range_2theta(n_pat))
      allocate(Job_Info%range_energy(n_pat))
      allocate(Job_Info%range_tof(n_pat))
      allocate(Job_Info%lambda(n_pat))
      allocate(Job_Info%ratio(n_pat))
      allocate(Job_Info%dtt1(n_pat))
      allocate(Job_Info%dtt2(n_pat))

      !> Initialize all variables
      Job_Info%Patt_typ    =" "
      Job_Info%Phas_nam    =" "
      Job_Info%range_stl%mina=0.0
      Job_Info%range_stl%maxb=0.0
      Job_Info%range_q%mina=0.0
      Job_Info%range_q%maxb=0.0
      Job_Info%range_d%mina=0.0
      Job_Info%range_d%maxb=0.0
      Job_Info%range_2theta%mina=0.0
      Job_Info%range_2theta%maxb=0.0
      Job_Info%range_Energy%mina=0.0
      Job_Info%range_Energy%maxb=0.0
      Job_Info%range_tof%mina=0.0
      Job_Info%range_tof%maxb=0.0
      Job_Info%Lambda%mina=0.0
      Job_Info%Lambda%maxb=0.0
      Job_Info%ratio = 0.0
      Job_Info%dtt1 = 0.0
      Job_Info%dtt2 = 0.0

      if (ncmd > 0) then
         if (allocated(Job_Info%cmd)) deallocate(Job_Info%cmd)
         allocate(Job_Info%cmd(ncmd))
         Job_Info%cmd=" "
      end if

      !> Fill the different fields of Job_Info
      !> Start with patterns
      fmtfields = "9fffff"

      !> First asks if there is a PATT_ card, if not a standard is taken
      if (ipt(1) /= 0) then
         do n_pat=1, Job_info%Num_Patterns
            i=ipt(n_pat)

            line=u_case(adjustl(cfl%line(i)%str))
            line=line(8:)
            call findfmt(0,line,fmtfields,fmtformat)
            read(unit=line,fmt=fmtformat) Job_Info%Patt_typ(n_pat), a1,a2,a3,a4,a5
            if (Err_CFML%Ierr /= 0) return

            line=u_case(Job_Info%Patt_typ(n_pat))

            select case(line(1:9))
               case("XRAY_2THE","NEUT_2THE","XRAY_SXTA","NEUT_SXTA")
                  if ( a1 <= 0.000001) a1=1.5405
                  if ( a2 <= 0.000001) then
                     a2=a1
                     a3=0.0
                  end if
                  if (a5 <= a4) a5=120.0
                  Job_Info%Lambda(n_pat)%mina=a1
                  Job_Info%Lambda(n_pat)%maxb=a2
                  Job_Info%ratio(n_pat)=a3
                  Job_Info%range_2theta(n_pat)%mina=a4
                  Job_Info%range_2theta(n_pat)%maxb=a5
                  a4=sind(0.5*a4)/a1
                  a5=sind(0.5*a5)/a2
                  Job_Info%range_stl(n_pat)%mina=a4
                  Job_Info%range_stl(n_pat)%maxb=a5
                  Job_Info%range_q(n_pat)%mina=a4*4.0*pi
                  Job_Info%range_q(n_pat)%maxb=a5*4.0*pi
                  Job_Info%range_d(n_pat)%mina=0.5/a5
                  Job_Info%range_d(n_pat)%maxb=0.5/a4

               case("NEUT_TOF ")
                  if (a1 <= 0.000001) a1=1000.0
                  if (a4 <= a3) a4=2.0*abs(a3)
                  Job_Info%dtt1(n_pat)=a1
                  Job_Info%dtt2(n_pat)=a2
                  Job_Info%range_tof(n_pat)%mina=a3
                  Job_Info%range_tof(n_pat)%maxb=a4
                  Job_Info%range_d(n_pat)%mina=0.5*(-1.0+sqrt(1.0+4.0*a2*a3/a1/a1))
                  Job_Info%range_d(n_pat)%maxb=0.5*(-1.0+sqrt(1.0+4.0*a2*a4/a1/a1))
                  Job_Info%range_stl(n_pat)%mina=0.5/Job_Info%range_d(n_pat)%maxb
                  Job_Info%range_stl(n_pat)%maxb=0.5/Job_Info%range_d(n_pat)%mina
                  Job_Info%range_q(n_pat)%mina=Job_Info%range_stl(n_pat)%mina*4.0*pi
                  Job_Info%range_q(n_pat)%maxb=Job_Info%range_stl(n_pat)%maxb*4.0*pi

               case("XRAY_ENER")
                  if (a1 <= 0.000001) a1=12.4 !(=hc(keV.Angstr.)
                  Job_Info%dtt1(n_pat)=a1
                  Job_Info%dtt2(n_pat)=0.0
                  Job_Info%range_energy(n_pat)%mina=a3
                  Job_Info%range_energy(n_pat)%maxb=a4
                  if (a3 <= 0.00001) a3=0.01
                  if (a4 <= 0.00001) a4=2.00
                  Job_Info%range_d(n_pat)%mina=a1/a4
                  Job_Info%range_d(n_pat)%maxb=a1/a3
                  Job_Info%range_stl(n_pat)%mina=0.5/Job_Info%range_d(n_pat)%maxb
                  Job_Info%range_stl(n_pat)%maxb=0.5/Job_Info%range_d(n_pat)%mina
                  Job_Info%range_q(n_pat)%mina=Job_Info%range_stl(n_pat)%mina*4.0*pi
                  Job_Info%range_q(n_pat)%maxb=Job_Info%range_stl(n_pat)%maxb*4.0*pi

            end select
         end do

      else
         n_pat=1
         a1=1.5405
         a2=a1
         a3=0.0
         a4=0.0
         a5=120.0
         Job_Info%Patt_typ(n_pat)="XRAY_2THE"
         Job_Info%Lambda(n_pat)%mina=a1
         Job_Info%Lambda(n_pat)%maxb=a2
         Job_Info%ratio(n_pat)=a3
         Job_Info%range_2theta(n_pat)%mina=a4
         Job_Info%range_2theta(n_pat)%maxb=a5
         a4=sind(0.5*a4)/a1
         a5=sind(0.5*a5)/a2
         Job_Info%range_stl(n_pat)%mina=a4
         Job_Info%range_stl(n_pat)%maxb=a5
         Job_Info%range_q(n_pat)%mina=a4*4.0*pi
         Job_Info%range_q(n_pat)%maxb=a5*4.0*pi
         Job_Info%range_d(n_pat)%mina=0.5/a5
         Job_Info%range_d(n_pat)%maxb=0.5/a4
      end if

      !> Phase names
      if (ip(1) /= 0) then
         do i=1,nphas
            j=ip(i)
            line=adjustl(cfl%line(j)%str)
            Job_Info%Phas_nam(i)=line(8:)
         end do
      else
         Job_Info%Phas_nam(1)= Job_info%title
      end if

      !> Command Lines, stored but not analysed here
      do i=1,ncmd
         j=ic(i)
         line=adjustl(cfl%line(j)%str)
         Job_Info%cmd(i)=line(8:)
      end do

   End Subroutine Get_Job_Info

   !!----
   !!---- SUBROUTINE READINFO_MOLECULE
   !!----
   !!----    Subroutine to read a molecule from a CFL ffile.
   !!----    The format is:
   !!----
   !!----        MOLEX_MoleculeName Coordinates_Type
   !!----        ....
   !!----        END_MOLEX_MoleculeName
   !!----
   !!----    where:
   !!----        Coordinates_Type    C: Cartesian coordinates
   !!----                            F: Fractional coordinates
   !!----                            S: Spherical coordinates
   !!----                            Z: Z-Matrix coordinates
   !!----
   !!----    Into the MOLEX Block:
   !!----       First line:Molecule_Centre(3), Molecule_Orient(3), Rotational_Angle Type(1), Thermal_Factor Type(1)
   !!----
   !!----       where:
   !!----        Molecule_Centre     Coordinate of Center of Molecule
   !!----        Molecule_Orient     Angles orientation
   !!----        Rotational Angle    E: Conventional Euler angles (alpha, beta, gamma)
   !!----                            P: Polar Euler angles (Phi, theta, Chi) (default)
   !!----        Thermal Factor    ISO: No collective motion
   !!----                          TLS: Traslational + Librational + Correlation
   !!----                           TL: Traslational + Librational
   !!----                            T: Traslational
   !!----
   !!----        According to Thermal Factors, next lines will be read
   !!----                          [T]: 6 Thermal Factors (Line1)
   !!----
   !!----                         [TL]: 6 Thermal Factors (Line1)
   !!----                               6 Thermal Factors (Line3)
   !!----
   !!----                        [TLS]: 6 Thermal Factors (Line1)
   !!----                               6 Thermal Factors (Line3)
   !!----                               9 Thermal Factors (Line5)
   !!----
   !!----    Internal Coordinates for Atoms (N_Atoms Lines)
   !!----        Atom_Name(6)  Atom_Specie(4)  Coordinates(3)  [N1  N2  N3]  Biso  Occ
   !!----
   !!---- Update: June - 2023
   !!
   Module Subroutine Read_CFL_Molecule(cfl, N_ini, N_end, Mol)
      !---- Arguments ----!
      Type(file_type),      intent(in)   :: cfl
      integer, optional,    intent(in)   :: N_Ini
      integer, optional,    intent(in)   :: N_End
      type (Molecule_type), intent(out)  :: Mol

      !---- Local variables -----!
      character(len=40)               :: Molname
      character(len=6)                :: Atname
      character(len=4)                :: AtSymb,var
      character(len=1)                :: ct
      integer                         :: i,k,n, ic,iv, npos,na
      real(kind=cp),dimension(3,3)    :: Eu

      logical :: err_flag

      !> Init
      call clear_error()

      !> First line: MOLEX_MoleculeName Coordinates_Type  Number
      i=n_ini
      line=adjustl(cfl%line(i)%str)

      k=index(line,'_')
      if (k == 0) then
         call set_error(1, 'Bad format for MOLEX block definition!')
         return
      end if
      line=line(k+1:)

      call get_words(line, dire, ic)
      if (ic < 2) then
         call set_error(1, 'Bad format for MOLEX block definition!')
         return
      end if

      !> Name of the Molecule
      Molname=trim(dire(1))

      !> Format of Coordinates
      ct='-'
      ct=adjustl(dire(2))
      ct=u_case(ct)
      select case (ct)
         case ('F','C','S','Z')
         case default
            call set_error(1," The type of the coordinates is unknown: "//ct )
            return
      end select

      !> Number of atoms
      na=0
      do i=n_ini+1,n_end-1
         line=adjustl(cfl%line(i)%str)
         if (len_trim(line) == 0) cycle
         if (line(1:1) == "!") cycle
         if (line(1:1) == "#") cycle
         na=na+1
      end do
      na=na-1    ! Delete the line for Center definition
      if (na <= 0) then
         call set_error(1, "The number of atoms in the Molecule is zero!" )
         return
      end if

      !> Initialize the Molecule_Type
      call Init_Molecule(Mol, na)

      Mol%Name_Mol=trim(Molname)
      Mol%coor_type=ct

      !> Centre / Orientation
      do i=n_ini+1, n_end-1
         line=adjustl(cfl%line(i)%str)
         if (len_trim(line) == 0) cycle
         if (line(1:1) =="!") cycle
         npos=index(line,'!')
         if (npos > 0) line=line(:npos-1)

         call get_words(line, dire, ic)
         if (ic /= 8) then
            call set_error(1, "Molecule_Centre(3R), Molecule_Orient(3R), Rotational_Angle Type(1C), Thermal_Factor Type(1C)" )
            return
         end if

         !> Centre of Molecule
         line=trim(dire(1))//'  '//trim(dire(2))//'  '//trim(dire(3))
         call get_num(line,vet,ivet,iv)
         if (iv /=3) then
            call set_error(1, "Wrong number of parameters describing the Centre position of the molecule!")
            return
         end if
         Mol%xcentre=vet(1:3)

         !> Orientation
         line=trim(dire(4))//'  '//trim(dire(5))//'  '//trim(dire(6))
         call get_num(line,vet,ivet,iv)
         if (iv /= 3) then
            call set_error(1, "Wrong number of parameters describing the Orientation of the molecule!")
            return
         end if
         Mol%orient=vet(1:3)

         !> Rotation type
         ct=adjustl(u_case(trim(dire(7))))
         select case (ct)
            case ('E','P')  ! Euler, Polar
            case default
               call set_error(1, "Wrong description for angle rotation type: "//ct)
               return
         end select
         Mol%rot_type=ct

         !> Thermal type
         var=adjustl(u_case(trim(dire(8))))
         select case (trim(var))
            case ('ISO')
            case ('TLS')
               Na=Na-3
            case ('TL')
               Na=Na-2
            case ('T')
               Na=Na-1
            case default
               call set_error(1, "Wrong description for Thermal type: "//trim(var))
               return
         end select
         Mol%therm_type=trim(var)
         Mol%natoms=Na

         exit
      end do
      Eu=Set_Euler_Matrix(Mol%rot_type, Mol%orient(1), Mol%orient(2), Mol%orient(3))
      Mol%Euler=Eu
      Mol%is_EulerMat=.true.

      !> Read the internal coordinates of the atoms in the Mol
      !> Read the Z-matrix/Cartesian/spherical/Fractional coordinates of the Mol
      k=0
      n=i+1

      do i=n, n_end-1
         line=adjustl(cfl%line(i)%str)
         if (len_trim(line) == 0) cycle
         if (line(1:1) =="!") cycle

         npos=index(line,'!')
         if (npos > 0) line=line(:npos-1)

         !> Atom Name
         call Cut_string(line, ic, Atname)

         !> Chemical symbol
         call Cut_string(line, ic, AtSymb)

         !> Numbers
         call get_num(line,vet,ivet,iv)

         select case (Mol%coor_type)
            case ('F','C','S')
               k=k+1
               if (iv >= 3 .and. iv <=5) then
                  select case (iv)
                     case (3)
                        Mol%I_Coor(:,k)  =vet(1:3)
                        Mol%U_iso(k)     =0.5_cp
                        Mol%Occ(k)       =1.0_cp
                     case (4)
                        Mol%I_Coor(:,k)  =vet(1:3)
                        Mol%U_iso(k)     =vet(4)
                        Mol%Occ(k)       =1.0_cp
                     case (5)
                        Mol%I_Coor(:,k)  =vet(1:3)
                        Mol%U_iso(k)     =vet(4)
                        Mol%Occ(k)       =vet(5)
                  end select
               else
                  call set_error(1," Wrong number of parameters for Atoms information into Molecule! " //trim(Atname))
                  return
               end if

            case ('Z')
               if (iv >= 6 .and. iv <=8) then
                  k=k+1
                  select case (iv)
                     case (6)
                        Mol%I_Coor(:,k)  =vet(1:3)
                        Mol%U_iso(k)     =0.5_cp
                        Mol%Occ(k)       =1.0_cp
                        Mol%conn(1:3,k)  =ivet(4:6)
                     case (7)
                        Mol%I_Coor(:,k)  =vet(1:3)
                        Mol%U_iso(k)     =vet(7)
                        Mol%Occ(k)       =1.0_cp
                        Mol%conn(1:3,k)  =ivet(4:6)
                     case (8)
                        Mol%I_Coor(:,k)  =vet(1:3)
                        Mol%U_iso(k)     =vet(7)
                        Mol%Occ(k)       =vet(8)
                        Mol%conn(1:3,k)  =ivet(4:6)
                  end select
               else
                  call set_error(1," Wrong number of parameters for Atoms information into Molecule! " //trim(Atname))
                  return
               end if
         end select

         mol%Atname(k)=trim(atname)
         mol%AtSymb(k)=trim(atsymb)

         if (k == na) exit
      end do
      if (k /= na) then
         call set_error(1," The number of Atoms readen in the file is different from asked! ")
         return
      end if

      !> TLS Info
      n=i+1
      if (var(1:1) == 'T') then
         do i=n, n_end-1
            line=adjustl(cfl%line(i)%str)
            if (len_trim(line) == 0) cycle
            if (line(1:1) =="!") cycle
            if (line(1:1) ==" ") cycle

            npos=index(line,'!')
            if (npos > 0) line=line(:npos-1)

            call get_num(line,vet,ivet,iv)
            if (iv /= 6) then
               call set_error(1, "Wrong number of parameters for Thermal values for Molecule")
               return
            end if
            Mol%T_TLS=vet(1:6)
            exit
         end do
      end if

      if (var(2:2) == 'L') then
         n=i+1
         do i=n, n_end-1
            line=adjustl(cfl%line(i)%str)
            if (len_trim(line) == 0) cycle
            if (line(1:1) =="!") cycle
            if (line(1:1) ==" ") cycle

            npos=index(line,'!')
            if (npos > 0) line=line(:npos-1)

            call get_num(line,vet,ivet,iv)
            if (iv /= 6) then
               call set_error(1, "Wrong number of parameters for Thermal values for Molecule")
               return
            end if
            Mol%L_TLS=vet(1:6)
            exit
         end do
      end if

      if (var(3:3) == 'S') then
         n=i+1
         do i=n, n_end-1
            line=adjustl(cfl%line(i)%str)
            if (len_trim(line) == 0) cycle
            if (line(1:1) =="!") cycle
            if (line(1:1) ==" ") cycle

            npos=index(line,'!')
            if (npos > 0) line=line(:npos-1)

            call get_num(line,vet,ivet,iv)
            if (iv /= 9) then
               call set_error(1, "Wrong number of parameters for Thermal values for Molecule")
               return
            end if
            Mol%S_TLS(1,:)=vet(1:3)
            Mol%S_TLS(2,:)=vet(4:6)
            Mol%S_TLS(3,:)=vet(7:9)
            exit
         end do
      end if

      !> Check connectivity if ZMatrix coordinates
      Mol%is_connect=.false.
      err_flag=.false.
      if (Mol%coor_type == "Z") then
         do i=2,Na
            if (i==2 .and. all(Mol%conn(:,2) ==0) ) Mol%conn(1,i)=1
            if (any(Mol%conn(:,i) >= i)) then
               err_flag=.true.
               exit
            end if
            if (i == 3 .and. (Mol%conn(1,i) == 0 .or. Mol%conn(2,i) == 0)) then
               err_flag=.true.
               exit
            end if
            if (i > 3) then
               if (any(Mol%conn(:,i) == 0)) then
                  err_flag=.true.
                  exit
               end if
            end if
         end do
         if (err_flag) then
            call set_error(1,"The Z-matrix connectivity is wrong!" )
            return
         end if
      end if

   End Subroutine Read_CFL_Molecule

End SubModule Format_CFL