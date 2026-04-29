!!
Submodule (CFML_KeyCodes) KeyCod_Phas
   implicit none

   Contains
   !!----
   !!---- SUBROUTINE READ_REFCODES_PHAS
   !!----
   !!---- Read all Keywords relative to a phase parameters
   !!----
   !!---- Update: 12/05/2022
   !!
   Module Subroutine Read_RefCodes_PHAS(ffile, n_ini, n_end, IPhas, Spg, Cell, Atm, G)
      !---- Arguments ----!
      Type(file_type),         intent(in)     :: ffile
      integer,                 intent(in)     :: n_ini  ! Start line
      integer,                 intent(in)     :: n_end  ! End line
      integer,                 intent(in)     :: IPhas  ! Number of the phase block definition
      class(SpG_Type),         intent(in)     :: SpG
      class(Cell_G_Type),      intent(in out) :: Cell
      type(Atlist_Type),       intent(in out) :: Atm
      type(GenParList_Type),   intent(in out) :: G

      !---- Local Variables ----!
      integer                 :: i, k

      !> Init
      call clear_error()

      do i=n_ini,n_end
         !> load information on line variable
         line=adjustl(ffile%line(i)%str)
         if (len_trim(line) == 0) cycle
         if (line(1:1) =="!") cycle
         if (line(1:1) ==" ") cycle

         k=index(line,"!")
         if( k /= 0) line=line(:k-1)

         !> Directives
         select case (u_case(line(1:4)))
            case ("FIX ", "FIXE")   ! FIX
               call ReadCode_FIX_PHAS(line, IPhas, Spg, Cell, Atm, G)
               if (err_CFML%Flag) return

            case ("VARY")    ! VARY
               call ReadCode_VARY_PHAS(line, IPhas, Spg, Cell, Atm, G)
               if (err_CFML%Flag) return

            case ("EQUA") ! Equal (Constraints)
               call ReadCode_EQUAL_PHAS(line, IPhas, Spg, Cell, Atm, G)
               if (err_CFML%Flag) return
         end select
      end do

   End Subroutine Read_RefCodes_PHAS

   !!----
   !!---- SUBROUTINE READCODE_FIX_PHAS
   !!----
   !!---- Update: May - 2022
   !!
   Module Subroutine ReadCode_FIX_PHAS(String, IPhas, Spg, Cell, Atm, G)
      !---- Arguments ----!
      character(len=*),      intent(in)     :: String
      integer,               intent(in)     :: IPhas
      class(SpG_Type),       intent(in)     :: SpG
      class(Cell_G_Type),    intent(in out) :: Cell
      type(Atlist_Type),     intent(in out) :: Atm
      type(GenParList_Type), intent(in out) :: G

      !---- Local Variables ----!
      character(len=3)                  :: car
      character(len=15)                 :: lab
      integer                           :: i, j, jj, k, ikey, iph
      integer                           :: n, n1, n2, n3, n_dir
      integer                           :: qc

      !> Init
      call clear_error()

      !> copy
      line=trim(adjustl(string))

      car=u_case(line(1:3))
      if (car /= 'FIX') then
         call set_error(1,'Wrong Directive for FIX instruction: '//trim(line))
         return
      end if

      !> Cut FIX word
      call cut_string(line,n)

      call get_words(line, dire, n_dir)

      i=1
      do while(i <= n_dir)
         !> Is dire(i) a general directive? (n1 > 0))
         n1 = index_Key_Phas(dire(i))

         !> Is there another general directive in the same line?
         !> If yes, j is the word position in the line
         j=0
         do jj=i+1, n_dir
            if (index_Key_Phas(dire(jj)) > 0 ) then
               j=jj
               exit
            end if
         end do

         !> word(i) is a directive: cell, xyz,...
         if (n1 > 0) then

            if (j ==0) then
               if (i == n_dir) then
                  !> last instruction
                  k=iphas

                  select case (n1)
                     case (1:7)
                        call Set_RefCodes_PHAS('FIX', n1, k, 0,' ', G)
                        call GPList_to_Cell(G, k, Cell)
                        call Set_KeyConstr_Cell(Spg%CrystalSys, Cell)
                        call GPList_from_Cell(Cell, k, G)

                     case (8:33)
                        do n=1,Atm%natoms
                           call Set_RefCodes_PHAS('FIX', n1, k, 0,trim(Atm%atom(n)%lab), G)
                        end do

                        call GPList_to_AtmList(G, k, Atm)
                        call Set_KeyConstr_Atm(Atm, Spg)
                        call GPList_from_AtmList(Atm, k, G)
                        call Update_GPList_Code(G)

                     case (34:48)
                     case (52:54)

                     case (NKEY_PHAS)

                     case default
                        call Set_RefCodes_PHAS('FIX', n1, k, 0,' ', G)
                  end select


               else
                  do n=i+1, n_dir
                     call Get_InfoKey_StrPhas(dire(n), iKey, IPh, Lab)

                     k=iphas
                     if (iph > 0) k=iPh

                     !> ikey debe ser 0

                     select case (n1)
                        case (1:7)
                           call Set_RefCodes_PHAS('FIX', n1, k, 0,' ', G)
                           call GPList_to_Cell(G, k, Cell)
                           call Set_KeyConstr_Cell(Spg%CrystalSys, Cell)
                           call GPList_from_Cell(Cell, k, G)

                        case (8:33)
                           n2 = Index_AtLab_on_AtList(trim(lab),k, Atm)

                           if (n2 > 0) then
                              !> Atom label
                              call Set_RefCodes_PHAS('FIX', n1, k, 0,trim(lab), G)

                           else
                              !> Chemical species
                              do n3=1, Atm%natoms
                                 if (trim(Atm%atom(n3)%ChemSymb) /= trim(lab)) cycle
                                 call Set_RefCodes_PHAS('FIX', n1, k, 0,trim(Atm%atom(n3)%Lab), G)
                              end do
                           end if

                           call GPList_to_AtmList(G, k, Atm)
                           call Set_KeyConstr_Atm(Atm, Spg)
                           call GPList_from_AtmList(Atm, k, G)
                           if (n < n_dir) call Update_GPList_Code(G)

                        case (34:48)
                        case (52:54)

                        case (NKEY_PHAS)
                           n2 = Index_AtLab_on_AtList(trim(lab),k, Atm)

                           if (n2 > 0) then
                              !> Atom label
                              call Set_RefCodes_PHAS('FIX', 13, k, 0,trim(lab), G) !UISO
                              call Set_RefCodes_PHAS('FIX', 11, k, 0,trim(lab), G) !XYZ

                           else
                              !> Chemical species
                              do n3=1, Atm%natoms
                                 if (trim(Atm%atom(n3)%ChemSymb) /= trim(lab)) cycle
                                 call Set_RefCodes_PHAS('FIX', 13, k, 0,trim(Atm%atom(n3)%Lab), G)
                                 call Set_RefCodes_PHAS('FIX', 11, k, 0,trim(Atm%atom(n3)%Lab), G)
                              end do
                           end if

                           call GPList_to_AtmList(G, k, Atm)
                           call Set_KeyConstr_Atm(Atm, Spg)
                           call GPList_from_AtmList(Atm, k, G)
                           if (n < n_dir) call Update_GPList_Code(G)

                        case default
                           call Set_RefCodes_PHAS('FIX', n1, k, 0,' ', G)
                     end select
                  end do

                  i=n_dir
               end if

            else

               if (i+1 == n_dir) then
                  k=iphas

                  select case (n1)
                     case (1:7)
                        call Set_RefCodes_PHAS('FIX', n1, k, 0,' ', G)
                        call GPList_to_Cell(G, k, Cell)
                        call Set_KeyConstr_Cell(Spg%CrystalSys, Cell)
                        call GPList_from_Cell(Cell, k, G)

                     case (8:33)
                        do n=1,Atm%natoms
                           call Set_RefCodes_PHAS('FIX', n1, k, 0,trim(Atm%atom(n)%lab), G)
                        end do

                        call GPList_to_AtmList(G, k, Atm)
                        call Set_KeyConstr_Atm(Atm, Spg)
                        call GPList_from_AtmList(Atm, k, G)
                        call Update_GPList_Code(G)

                     case (34:48)
                     case (52:54)

                     case (NKEY_PHAS)

                     case default
                        call Set_RefCodes_PHAS('FIX', n1, k, 0,' ', G)
                  end select

               else
                  do n=i+1, j-1
                     call Get_InfoKey_StrPhas(dire(n), iKey, IPh, Lab)

                     k=iphas
                     if (iph > 0) k=iPh

                     !> ikey debe ser 0

                     select case (n1)
                        case (1:7)
                           call Set_RefCodes_PHAS('FIX', n1, k, 0,' ', G)
                           call GPList_to_Cell(G, k, Cell)
                           call Set_KeyConstr_Cell(Spg%CrystalSys, Cell)
                           call GPList_from_Cell(Cell, k, G)

                        case (8:33)
                           n2 = Index_AtLab_on_AtList(trim(lab),k, Atm)
                           if (n2 > 0) then
                              !> Atom label
                              call Set_RefCodes_PHAS('FIX', n1, k, 0,trim(lab), G)

                           else
                              !> Chemical species
                              do n3=1, Atm%natoms
                                 if (trim(Atm%atom(n3)%ChemSymb) /= trim(lab)) cycle
                                 call Set_RefCodes_PHAS('FIX', n1, k, 0,trim(Atm%atom(n3)%Lab), G)
                              end do
                           end if

                           call GPList_to_AtmList(G, k, Atm)
                           call Set_KeyConstr_Atm(Atm, Spg)
                           call GPList_from_AtmList(Atm, k, G)
                           if (n < j-1) call Update_GPList_Code(G)

                        case (34:48)
                        case (52:54)

                        case (NKEY_PHAS)

                        case default
                           call Set_RefCodes_PHAS('FIX', n1, k, 0,' ', G)
                     end select
                  end do
               end if

               i=j-1
            end if

         else
            !> word(i) is local instruction: x_, xyz_,....
            call Get_InfoKey_StrPhas(dire(i), iKey, IPh, Lab, qc)

            k=iphas
            if (iph > 0) k=iPh

            select case (ikey)
               case (1:7)
                  call Set_RefCodes_PHAS('FIX', ikey, k, 0,' ', G)
                  call GPList_to_Cell(G, k, Cell)
                  call Set_KeyConstr_Cell(Spg%CrystalSys, Cell)
                  call GPList_from_Cell(Cell, k, G)

               case (8:33)
                  n2 = Index_AtLab_on_AtList(trim(lab),k, Atm)
                  if (n2 > 0) then
                     !> Atom label
                     call Set_RefCodes_PHAS('FIX', ikey, k, 0,trim(lab), G)

                  else
                     !> Chemical species
                     do n3=1, Atm%natoms
                        if (trim(Atm%atom(n3)%ChemSymb) /= trim(lab)) cycle
                        call Set_RefCodes_PHAS('FIX', ikey, k, 0,trim(Atm%atom(n3)%Lab), G)
                     end do
                  end if

                  call GPList_to_AtmList(G, k, Atm)
                  call Set_KeyConstr_Atm(Atm, Spg)
                  call GPList_from_AtmList(Atm, k, G)

               case (34:48)
               case (52:54)

               case (181:218)
                  n2 = Index_AtLab_on_AtList(trim(lab),k, Atm)
                  call Set_RefCodes_PHAS('FIX', ikey, k, qc, trim(lab), G)

               case (NKEY_PHAS)

               case default
                  call Set_RefCodes_PHAS('FIX', ikey, k, 0,' ', G)
            end select


         end if

         !> Updating Codes
         call Update_GPList_Code(G)

         !> Debugging
         !call WriteInfo_GPList(G)

         i=i+1
      end do

   End Subroutine ReadCode_FIX_PHAS

   !!----
   !!---- SUBROUTINE READCODE_VARY_PHAS
   !!----
   !!---- Update: May - 2022
   !!
   Module Subroutine ReadCode_VARY_PHAS(String, IPhas, Spg, Cell, Atm, G)
      !---- Arguments ----!
      character(len=*),      intent(in)     :: String
      integer,               intent(in)     :: IPhas
      class(SpG_Type),       intent(in)     :: SpG
      class(Cell_G_Type),    intent(in out) :: Cell
      type(Atlist_Type),     intent(in out) :: Atm
      type(GenParList_Type), intent(in out) :: G

      !---- Local Variables ----!
      character(len=3)                  :: car
      character(len=15)                 :: lab
      integer                           :: i, j, jj, k, ikey, iph
      integer                           :: n, n1, n2, n3, n_dir
      integer                           :: qc

      !> Init
      call clear_error()

      !> copy
      line=trim(adjustl(string))

      car=u_case(line(1:3))
      if (car /= 'VAR') then
         call set_error(1,'Wrong Directive for VARY instruction: '//trim(line))
         return
      end if

      !> Cut VARY word
      call cut_string(line,n)

      !> Get words
      call get_words(line, dire, n_dir)

      i=1
      do while(i <= n_dir)
         !> Is dire(i) a general directive? n1 > 0
         n1 = index_Key_Phas(dire(i))

         !> Is there other general directive?
         !> If yes j > 0 (word position in the line)
         j=0
         do jj=i+1, n_dir
            if (index_Key_Phas(dire(jj)) > 0 ) then
               j=jj
               exit
            end if
         end do

         !> dire(i) is a general directive: cell, xyz,...
         if (n1 > 0) then

            if (j ==0) then
               if (i == n_dir) then
                  !> only one directive: vary cell
                  k=iphas

                  select case (n1)
                     case (1:7) ! Cell parameters
                        call Set_RefCodes_PHAS('VARY', n1, k, 0,' ', G)

                        call GPList_to_Cell(G, k, Cell)
                        call Set_KeyConstr_Cell(Spg%CrystalSys, Cell)
                        call GPList_from_Cell(Cell, k, G)

                     case (8:26) ! Normal atoms
                        do n=1,Atm%natoms
                           call Set_RefCodes_PHAS('VARY', n1, k, 0,trim(Atm%atom(n)%lab), G)
                        end do

                        call GPList_to_AtmList(G, k, Atm)
                        call Set_KeyConstr_Atm(Atm, Spg)
                        call GPList_from_AtmList(Atm, k, G)

                        call Update_GPList_Code(G)

                     case (27:68) ! MOL

                     case (NKEY_PHAS) ! ALL

                     case default
                        call Set_RefCodes_PHAS('VARY', n1, k, 0,' ', G)
                  end select


               else
                  !> only one generic directive with options: vary xyz O1 O2 ...
                  do n=i+1, n_dir
                     call Get_InfoKey_StrPhas(dire(n), iKey, IPh, Lab) ! Ikey should be 0

                     k=iphas
                     if (iph > 0) k=iPh

                     select case (n1)
                        case (1:7) ! Cell parameters
                           !> Don't have sense

                           !call Set_RefCodes_PHAS('VARY', n1, k, ' ', G)
                           !call GPList_to_Cell(G, k, Cell)
                           !call Set_KeyConstr_Cell(Spg%CrystalSys, Cell)
                           !call GPList_from_Cell(Cell, k, G)

                        case (8:26) ! Normal atoms
                           n2 = Index_AtLab_on_AtList(trim(lab),k, Atm)

                           if (n2 > 0) then
                              !> Atom labels
                              call Set_RefCodes_PHAS('VARY', n1, k, 0,trim(lab), G)

                           else
                              !> Chemical species
                              do n3=1, Atm%natoms
                                 if (trim(Atm%atom(n3)%ChemSymb) /= trim(lab)) cycle
                                 call Set_RefCodes_PHAS('VARY', n1, k, 0,trim(Atm%atom(n3)%Lab), G)
                              end do
                           end if

                           call GPList_to_AtmList(G, k, Atm)
                           call Set_KeyConstr_Atm(Atm, Spg)
                           call GPList_from_AtmList(Atm, k, G)
                           if (n < n_dir) call Update_GPList_Code(G)

                        case (27:68) ! MOL

                        case (NKEY_PHAS) ! ALL
                           n2 = Index_AtLab_on_AtList(trim(lab),k, Atm)

                           if (n2 > 0) then
                              !> Atom labels
                              call Set_RefCodes_PHAS('VARY', 11, k, 0,trim(lab), G) !XYZ
                              if (Atm%atom(n2)%Utype == 'U_IJ') then
                                 call Set_RefCodes_PHAS('VARY', 13, k, 0,trim(lab), G) !UISO
                              else
                                 call Set_RefCodes_PHAS('VARY', 20, k, 0,trim(lab), G) !BISO
                              end if

                           else
                              !> Chemical species
                              do n3=1, Atm%natoms
                                 if (trim(Atm%atom(n3)%ChemSymb) /= trim(lab)) cycle
                                 call Set_RefCodes_PHAS('VARY', 11, k, 0,trim(Atm%atom(n3)%Lab), G)
                                 if (Atm%atom(n3)%Utype == 'U_IJ') then
                                    call Set_RefCodes_PHAS('VARY', 13, k, 0,trim(Atm%atom(n3)%Lab), G)
                                 else
                                    call Set_RefCodes_PHAS('VARY', 20, k, 0,trim(Atm%atom(n3)%Lab), G)
                                 end if
                              end do
                           end if

                           call GPList_to_AtmList(G, k, Atm)
                           call Set_KeyConstr_Atm(Atm, Spg)
                           call GPList_from_AtmList(Atm, k, G)
                           if (n < n_dir) call Update_GPList_Code(G)

                        case default
                           call Set_RefCodes_PHAS('VARY', n1, k, 0,' ', G)
                     end select
                  end do

                  i=n_dir
               end if

            else
               if (i+1 == n_dir) then
                  !> two directives: vary uiso xyz
                  k=iphas

                  select case (n1)
                     case (1:7) ! Cell parameters
                        call Set_RefCodes_PHAS('VARY', n1, k, 0,' ', G)

                        call GPList_to_Cell(G, k, Cell)
                        call Set_KeyConstr_Cell(Spg%CrystalSys, Cell)
                        call GPList_from_Cell(Cell, k, G)

                     case (8:26) ! Normal atoms
                        do n=1,Atm%natoms
                           call Set_RefCodes_PHAS('VARY', n1, k, 0,trim(Atm%atom(n)%lab), G)
                        end do

                        call GPList_to_AtmList(G, k, Atm)
                        call Set_KeyConstr_Atm(Atm, Spg)
                        call GPList_from_AtmList(Atm, k, G)
                        call Update_GPList_Code(G)

                     case (27:68) ! MOL

                     case (NKEY_PHAS) ! ALL

                     case default
                        call Set_RefCodes_PHAS('VARY', n1, k, 0,' ', G)
                  end select

               else
                  !> a directive with options... vary xyz o1 o2 uiso o3
                  do n=i+1, j-1
                     call Get_InfoKey_StrPhas(dire(n), iKey, IPh, Lab)  ! ikey should be 0

                     k=iphas
                     if (iph > 0) k=iPh

                     select case (n1)
                        case (1:7) ! Cell parameters
                           call Set_RefCodes_PHAS('VARY', n1, k, 0,' ', G)

                           call GPList_to_Cell(G, k, Cell)
                           call Set_KeyConstr_Cell(Spg%CrystalSys, Cell)
                           call GPList_from_Cell(Cell, k, G)

                        case (8:26) ! Normal atoms
                           n2 = Index_AtLab_on_AtList(trim(lab),k, Atm)

                           if (n2 > 0) then
                              !> Atom labels
                              call Set_RefCodes_PHAS('VARY', n1, k, 0,trim(lab), G)

                           else
                              !> Chemical species
                              do n3=1, Atm%natoms
                                 if (trim(Atm%atom(n3)%ChemSymb) /= trim(lab)) cycle
                                 call Set_RefCodes_PHAS('VARY', n1, k, 0,trim(Atm%atom(n3)%Lab), G)
                              end do
                           end if

                           call GPList_to_AtmList(G, k, Atm)
                           call Set_KeyConstr_Atm(Atm, Spg)
                           call GPList_from_AtmList(Atm, k, G)
                           if (n < j-1) call Update_GPList_Code(G)

                        case (27:68) ! MOL

                        case (NKEY_PHAS) ! ALL

                        case default
                           call Set_RefCodes_PHAS('VARY', n1, k, 0,' ', G)
                     end select
                  end do
               end if

               i=j-1
            end if

         else
            !> dire(i) is local instruction: x_O1, xyz_O1, ....
            call Get_InfoKey_StrPhas(dire(i), iKey, IPh, Lab, Qc)

            k=iphas
            if (iph > 0) k=iPh

            select case (ikey)
               case (1:7) ! Cell parameters
                  call Set_RefCodes_PHAS('VARY', ikey, k, 0,' ', G)

                  call GPList_to_Cell(G, k, Cell)
                  call Set_KeyConstr_Cell(Spg%CrystalSys, Cell)
                  call GPList_from_Cell(Cell, k, G)

               case (8:26) ! Normal atoms
                  n2 = Index_AtLab_on_AtList(trim(lab),k, Atm)
                  if (n2 > 0) then
                     !> Atom labels
                     call Set_RefCodes_PHAS('VARY', ikey, k, 0,trim(lab), G)

                  else
                     !> Chemical species
                     do n3=1, Atm%natoms
                        if (trim(Atm%atom(n3)%ChemSymb) /= trim(lab)) cycle
                        call Set_RefCodes_PHAS('VARY', ikey, k, 0,trim(Atm%atom(n3)%Lab), G)
                     end do
                  end if

                  call GPList_to_AtmList(G, k, Atm)
                  call Set_KeyConstr_Atm(Atm, Spg)
                  call GPList_from_AtmList(Atm, k, G)

               case (27:68) ! MOL

               case (181:218)
                  n2 = Index_AtLab_on_AtList(trim(lab),k, Atm)
                  call Set_RefCodes_PHAS('VARY', ikey, k, qc,trim(lab), G)

               case (NKEY_PHAS) ! ALL

               case default
                  call Set_RefCodes_PHAS('VARY', ikey, k, 0,' ', G)
            end select

         end if

         !> Updating Codes
         call Update_GPList_Code(G)

         !> Debugging
         !call WriteInfo_GPList(G)

         i=i+1
      end do

   End Subroutine ReadCode_VARY_PHAS



   !!--++
   !!--++ SUBROUTINE SETL_REFCODES_PHAS
   !!--++
   !!--++
   !!--++ Update: May - 2022
   !!
   Module Subroutine Set_RefCodes_PHAS(Keyword, Npar,  IPhas, Qc, Lab, G)
      !---- Arguments ----!
      character(len=*),            intent(in)     :: Keyword     ! VARY/FIX/....
      integer,                     intent(in)     :: NPar        ! Specific parameter A,B,C,...
      integer,                     intent(in)     :: IPhas
      integer,                     intent(in)     :: Qc
      character(len=*),            intent(in)     :: Lab
      type(GenParList_Type),       intent(in out) :: G

      !---- Local variables ----!
      !integer          :: i
      character(len=4) :: cdire, car, cqc
      character(len=20):: str

      !> Init
      call clear_error()

      write(car,fmt='(i3)') iphas
      car=adjustl(car)

      !> keyword
      cdire=u_case(Keyword)
      select case (trim(cdire))
         !> FIX
         case ("FIX")
            select case (Npar)
               case (0)
                  call set_error(1," Error in the Refinement Code for Patterns!")
                  return

               case (7) ! CELL
                  call Fix_GPList_Par(G,'A_PHAS'//trim(car))
                  call Fix_GPList_Par(G,'B_PHAS'//trim(car))
                  call Fix_GPList_Par(G,'C_PHAS'//trim(car))
                  call Fix_GPList_Par(G,'ALPHA_PHAS'//trim(car))
                  call Fix_GPList_Par(G,'BETA_PHAS'//trim(car))
                  call Fix_GPList_Par(G,'GAMMA_PHAS'//trim(car))

               case (11) ! XYZ
                  call Fix_GPList_Par(G,'X_'//trim(lab)//'_PHAS'//trim(car))
                  call Fix_GPList_Par(G,'Y_'//trim(lab)//'_PHAS'//trim(car))
                  call Fix_GPList_Par(G,'Z_'//trim(lab)//'_PHAS'//trim(car))

               case (14) ! U
                  call Fix_GPList_Par(G,'U11_'//trim(lab)//'_PHAS'//trim(car))
                  call Fix_GPList_Par(G,'U22_'//trim(lab)//'_PHAS'//trim(car))
                  call Fix_GPList_Par(G,'U33_'//trim(lab)//'_PHAS'//trim(car))
                  call Fix_GPList_Par(G,'U12_'//trim(lab)//'_PHAS'//trim(car))
                  call Fix_GPList_Par(G,'U13_'//trim(lab)//'_PHAS'//trim(car))
                  call Fix_GPList_Par(G,'U23_'//trim(lab)//'_PHAS'//trim(car))

               case (34:48) ! MOL

               case (181:218)
                  if (Qc > 0) then
                     write(cqc,fmt='(i3)') qc
                     cqc=adjustl(cqc)
                     str=trim(KEY_PHAS(Npar))//'.'//trim(cqc)//'_'//trim(lab)//'_PHAS'//trim(car)
                  else
                     str=trim(KEY_PHAS(Npar))//'_'//trim(lab)//'_PHAS'//trim(car)
                  end if
                  call Fix_GPList_Par(G,trim(str))

               case (NKEY_PHAS) ! ALL

               case default
                  if (len_trim(lab) > 0) then
                     str=trim(KEY_PHAS(Npar))//'_'//trim(lab)//'_PHAS'//trim(car)
                  else
                     str=trim(KEY_PHAS(Npar))//'_PHAS'//trim(car)
                  end if

                  call Fix_GPList_Par(G,trim(str))

            end select ! Npar

         !> VARY
         case ("VARY")
            select case (Npar)
               case (0)
                  call set_error(1," Error in the Refinement Code for Patterns!")
                  return

               case (7)
                  call Vary_GPList_Par(G,'A_PHAS'//trim(car))
                  call Vary_GPList_Par(G,'B_PHAS'//trim(car))
                  call Vary_GPList_Par(G,'C_PHAS'//trim(car))
                  call Vary_GPList_Par(G,'ALPHA_PHAS'//trim(car))
                  call Vary_GPList_Par(G,'BETA_PHAS'//trim(car))
                  call Vary_GPList_Par(G,'GAMMA_PHAS'//trim(car))

               case (11)
                  call Vary_GPList_Par(G,'X_'//trim(lab)//'_PHAS'//trim(car))
                  call Vary_GPList_Par(G,'Y_'//trim(lab)//'_PHAS'//trim(car))
                  call Vary_GPList_Par(G,'Z_'//trim(lab)//'_PHAS'//trim(car))

               case (14)
                  call Vary_GPList_Par(G,'U11_'//trim(lab)//'_PHAS'//trim(car))
                  call Vary_GPList_Par(G,'U22_'//trim(lab)//'_PHAS'//trim(car))
                  call Vary_GPList_Par(G,'U33_'//trim(lab)//'_PHAS'//trim(car))
                  call Vary_GPList_Par(G,'U12_'//trim(lab)//'_PHAS'//trim(car))
                  call Vary_GPList_Par(G,'U13_'//trim(lab)//'_PHAS'//trim(car))
                  call Vary_GPList_Par(G,'U23_'//trim(lab)//'_PHAS'//trim(car))

               case (34:48)

               case (181:218)
                  if (Qc > 0) then
                     write(cqc,fmt='(i3)') qc
                     cqc=adjustl(cqc)
                     str=trim(KEY_PHAS(Npar))//'.'//trim(cqc)//'_'//trim(lab)//'_PHAS'//trim(car)
                  else
                     str=trim(KEY_PHAS(Npar))//'_'//trim(lab)//'_PHAS'//trim(car)
                  end if
                  call Vary_GPList_Par(G,trim(str))

               case (NKEY_PHAS) ! ALL

               case default
                  if (len_trim(lab) > 0) then
                     str=trim(KEY_PHAS(Npar))//'_'//trim(lab)//'_PHAS'//trim(car)
                  else
                     str=trim(KEY_PHAS(Npar))//'_PHAS'//trim(car)
                  end if
                  call Vary_GPList_Par(G,trim(str))

            end select ! Npar

      end select ! Directives

   End Subroutine Set_RefCodes_PHAS

   !!----
   !!---- Subroutine Set_KeyConstr_Cell
   !!----
   !!----
   !!---- June - 2023
   !!
   Module Subroutine Set_KeyConstr_Cell(CrystSys, Cell)
      !---- Arguments ----!
      character(len=*),    intent(in)     :: CrystSys
      class(Cell_G_Type),  intent(in out) :: Cell

      !---- Local Variables ----!
      integer               :: i, j, k, n, ic_min, ic_max
      integer, dimension(6) :: ind
      real(kind=cp)         :: dif1, dif2, dif3

      Select Type(Cell)
       Type is(Cell_GLS_Type)
         Select case (trim(CrystSys))
            case ('Triclinic')
               ! Nothing to do

            case ('Monoclinic')
               dif1=abs(cell%ang(1)-90.0_cp)
               dif2=abs(cell%ang(2)-90.0_cp)
               dif3=abs(cell%ang(3)-90.0_cp)

               !Axis a
               if (dif1 > 0.005) then
                  cell%lang(2:3)=0
                  cell%ang(2:3)=90.0_cp

               !Axis b
               else if (dif2 > 0.005) then
                  cell%lang(1)=0
                  cell%lang(3)=0
                  cell%ang(1)=90.0_cp
                  cell%ang(3)=90.0_cp

               !> Axis c
               else if (dif3 > 0.005) then
                  cell%lang(1:2)=0
                  cell%ang(1:2)=90.0_cp
               end if

            case ('Orthorhombic')
               cell%lang=0
               cell%ang=90.0_cp

            case ('Tetragonal')
               !> b=a
               if (cell%lcell(1) /= cell%lcell(2)) cell%lcell(2)=cell%lcell(1)
               cell%cell(2)=cell%cell(1)

               cell%lang=0
               cell%ang=90.0_cp

            case ('Trigonal')
               !> b=a; c=a
               if (cell%lcell(1) /= cell%lcell(2)) cell%lcell(2)=cell%lcell(1)
               if (cell%lcell(1) /= cell%lcell(3)) cell%lcell(3)=cell%lcell(1)
               cell%cell(2:3)=cell%cell(1)

               !> beta=gamma=alpha
               if (cell%lang(1) /= cell%lang(2)) cell%lang(2)=cell%lang(1)
               if (cell%lang(1) /= cell%lang(3)) cell%lang(3)=cell%lang(1)
               cell%ang(2:3)=cell%ang(1)


            case ('Hexagonal', 'Rhombohedral')
               !> b=a
               if (cell%lcell(1) /= cell%lcell(2)) cell%lcell(2)=cell%lcell(1)
               cell%cell(2)=cell%cell(1)

               !> angles
               cell%lang(1:2)=0
               cell%lang(3)=cell%lcell(1)    ! Esto es correcto para algo general?

               cell%ang=[90.0_cp, 90.0_cp, 120.0_cp]


            case ('Cubic')
               !> c=b=a
               if (cell%lcell(1) /= cell%lcell(2)) cell%lcell(2)=cell%lcell(1)
               if (cell%lcell(1) /= cell%lcell(3)) cell%lcell(3)=cell%lcell(1)
               cell%cell(2:3)=cell%cell(1)

               !> angles
               cell%lang=0
               cell%ang=90.0_cp
         End Select

         if (all(cell%lcell == 0) .and. all(cell%lang == 0)) return

         !> Reindexation
         ind(1:3)=cell%lcell
         ind(4:6)=cell%lang
         ic_min=minval(ind, mask=ind > 0)
         ic_max=maxval(ind)

         n=0
         do i=ic_min, ic_max
            k=count(ind ==i)
            if (k > 0) n=n+1
            do j=1,3
               if (cell%lcell(j) == i) cell%lcell(j)=n
            end do
            do j=1,3
               if (cell%lang(j) == i) cell%lang(j)=n
            end do
         end do

      End Select
   End Subroutine Set_KeyConstr_Cell

   !!----
   !!---- Subroutine ReadCode_EQUAL_PHAS
   !!----
   !!---- June 2023
   !!
   Module Subroutine ReadCode_EQUAL_PHAS(String, IPhas, Spg, Cell, Atm, G)
      !---- Arguments ----!
      character(len=*),      intent(in)     :: String
      integer,               intent(in)     :: IPhas
      class(SpG_Type),       intent(in)     :: SpG
      class(Cell_G_Type),    intent(in out) :: Cell
      type(Atlist_Type),     intent(in out) :: Atm
      type(GenParList_Type), intent(in out) :: G

      !---- Local Variables ----!
      character(len=3) :: car
      integer :: k, nlong

      !> Init
      call clear_error()

      !> copy
      line=trim(adjustl(string))

      car=u_case(line(1:3))
      if (car /= 'EQU') then
         call set_error(1,'Wrong Directive for EQUAL instruction: '//trim(line))
         return
      end if

      !> Cut EQUAL word
      call cut_string(line,nlong)

      call SetCode_EQUAL_PHAS(trim(line), IPhas, G)

      !> Load information on Atmlist
      call GPList_to_AtmList(G, k, Atm)

      !> Updating Codes
      call Update_GPList_Code(G)

   End Subroutine ReadCode_EQUAL_PHAS

   !!----
   !!---- Subroutine Set_KeyConstr_Atm
   !!----
   !!---- Determine constrains on Atom parameters
   !!----
   !!---- June - 2023
   !!
   Module Subroutine Set_KeyConstr_Atm(Atm, Spg)
      !---- Arguments ----!
      type(Atlist_Type), intent(in out) :: Atm
      class(SpG_Type),   intent(in)     :: SpG

      !---- Local Variables ----!
      integer               :: i,j,nc
      integer, dimension(6) :: lc

      nc=Atm%natoms*10
      lc=0

      do i=1, Atm%natoms
         associate(A=> Atm%atom(i))
            select type (A)
               type is (Atm_Ref_Type)
                  if (any(A%l_x > 0)) then
                     lc(1:3)=A%l_x
                     call Get_AtomPos_CTR(A%X, Spg, nc, A%l_x, A%m_x)
                     do j=1,3
                        if (lc(j) ==0) A%l_x(j)=0
                     end do
                  end if

                  if (any(A%l_u > 0)) then
                     lc=A%l_u
                     call Get_AtomBet_CTR(A%x,A%u,Spg, nc, A%l_u, A%m_u)
                     do j=1,6
                        if (lc(j) ==0) A%l_u(j)=0
                     end do
                  end if

               type is (ModAtm_Ref_Type)
                  if (any(A%l_x > 0)) then
                     lc(1:3)=A%l_x
                     call Get_AtomPos_CTR(A%X, Spg, nc, A%l_x, A%m_x)
                     do j=1,3
                        if (lc(j) ==0) A%l_x(j)=0
                     end do
                  end if

                  if (any(A%l_u > 0)) then
                     lc=A%l_u
                     call Get_AtomBet_CTR(A%x,A%u,Spg, nc, A%L_u, A%m_u)
                     do j=1,6
                        if (lc(j) ==0) A%l_u(j)=0
                     end do
                  end if

            end select ! A
         end associate
      end do

   End Subroutine Set_KeyConstr_Atm

   !!----
   !!---- Subroutine SetCode_EQUAL_PHAS
   !!----
   !!----
   !!---- June 2023
   !!
   Module Subroutine SetCode_EQUAL_PHAS(string, IPhas, G)
      !---- Arguments ----!
      character(len=*),      intent(in)     :: String
      integer,               intent(in)     :: IPhas
      type(GenParList_Type), intent(in out) :: G

      !---- Local Variables ----!
      integer :: i, j, n, k, kc, kc2, kc3, nc, iv
      integer :: n1, n2, n3
      integer :: ind1, ind2, ind3, ik, ik2, ik3
      character(len=1) :: cn
      character(len=3) :: car
      character(len=2) :: spc, spc3
      character(len=20):: lab, lab2, lab3
      character(len=60):: str1, str2, str3

      logical :: chemlabel = .false.
      logical :: atmlabel = .false.

      !> Init
      call clear_error()
      if (len_trim(string) <=0) return

      !> Active Phase
      k=1
      if (Iphas > 0) k=IPhas
      write(unit=car, fmt='(i3)') k
      car=adjustl(car)

      line=trim(string)
      call get_words(line, dire, nc)
      if (nc < 2) then
         call set_error(1, "Incorrect EQUAL definition: "//trim(line))
         return
      end if

      !> Parent (First word): general / particular
      n = index(dire(1),'_')

      select case (n)
         case (0)
            !> -----------------
            !> General Directive
            !> -----------------
            ind1=Index_key_phas(trim(u_case(dire(1))) )
            if (ind1 ==0) then
               call set_error(1, &
                   "Error in EQUAL Instructions. Waiting for a PHASE directive: "//trim(dire(1)) )
               return
            end if

            !> -----------
            !> Parent info
            !> -----------
            call Get_InfoKey_StrPhas(trim(dire(2)), ind2, ik2, lab2)

            !> Phase
            if (ik2 ==0) ik2=k

            !> chemical species
            spc = Get_Chem_Symb(lab2)
            if (trim(spc) == trim(lab2) ) chemlabel=.true.

            if (.not. chemlabel) then
               if (ind2 ==0 .and. len_trim(lab2) > 0) atmlabel=.true.
            end if

            !> ----------
            !> Offspring
            !> ----------
            if (chemlabel) then
               write(unit=car, fmt='(i3)') ik2
               car=adjustl(car)

               !> Found a  parent
               spc = Get_Chem_Symb(lab2)
               str1='_'//trim(spc)
               str2=trim(u_case(dire(1)))//trim(str1)
               str3='_PHAS'//trim(car)

               ind2=0
               do j=1,G%NPar
                  n = index(trim(G%Par(j)%Nam), trim(str3)) ! Select the correct phase
                  if (n == 0) cycle

                  n = index(trim(G%Par(j)%Nam), trim(str2)) ! Select same directive + chemical symbol
                  if (n == 0) cycle

                  ind2=j
                  exit
               end do
               if (ind2 == 0) then
                  call set_error(1, 'Cannot determine a possible Parent for EQUAL instruction!')
                  return
               end if

               kc2=G%Par(ind2)%L

               !> Offspring
               do j=1, G%NPar
                  if (j == ind2) cycle
                  call Get_InfoKey_StrPhas(trim(G%Par(j)%Nam), ind3, ik3, lab3)

                  if (ik3 /= ik2) cycle                ! Same phase
                  if (ind3 /= ind1) cycle              ! Same parameter
                  spc3 = Get_Chem_Symb(lab3)
                  if (trim(spc) /= trim(spc3)) cycle   ! Same chemical specie

                  !> Assign the values
                  kc3=G%Par(j)%L

                  G%Par(j)%L=kc2

                  !> Checks others parameters with the same Code
                  do n=1,G%NPar
                     if (n==ind2 .or. n == j) cycle

                     if (G%Par(n)%L == kc3) then
                        G%Par(n)%L =kc2
                     end if
                  end do
               end do

            else if (atmlabel) then
               write(unit=car, fmt='(i3)') ik2
               car=adjustl(car)

               ind2= index_GPList(trim(u_case(dire(1)))//'_'//trim(lab2)//'_PHAS'//trim(car), G)
               if (ind2 ==0) then
                  call set_error(1, ' Not found the parent information on EQUAL directive!')
                  return
               end if

               kc2=G%Par(ind2)%L

               j=3
               do while (j <= nc)
                  call Get_InfoKey_StrPhas(trim(dire(j)), ind3, ik3, lab3)

                  !> Phase
                  if (ik3 ==0) ik3=k
                  write(unit=car, fmt='(i3)') ik3
                  car=adjustl(car)

                  ind3= index_GPList(trim(u_case(dire(1)))//'_'//trim(lab3)//'_PHAS'//trim(car), G)
                  if (ind3 ==0) then
                     call set_error(1, ' Not found a correct offprings information on EQUAL directive!')
                     return
                  end if

                  kc3=G%Par(ind3)%L

                  !> Check if the next is a number
                  call get_num(trim(dire(j+1)), vet, ivet, iv)

                  !> Assign the values
                  kc3=G%Par(ind3)%L

                  G%Par(ind3)%L=kc2
                  if (iv ==1) G%Par(ind3)%M=vet(1)

                  !> Checks orthers parameters with the same Code
                  do n=1,G%NPar
                     if (n==ind2 .or. n == ind3) cycle

                     if (G%Par(n)%L == kc3) then
                        G%Par(n)%L =kc2
                        if (iv ==1) G%Par(n)%M=vet(1)
                     end if
                  end do

                  j=j+1
                  if (iv ==1) j=j+1
               end do

            else
               call set_error(1, " Error in EQUAL directive format! ")
               return
            end if

         case (1:)! Local/ atom
            !> -----------
            !> Parent info
            !> -----------
            call Get_InfoKey_StrPhas(trim(dire(1)), ind1, ik, lab)

            if (ind1 == 0 .or. len_trim(lab) == 0) then
               call set_error(1,"Wrong format for EQUAL directive: "//trim(string))
               return
            end if

            !> Phase
            if (ik ==0) ik=k
            write(unit=car, fmt='(i3)') ik
            car=adjustl(car)

            ind1 = index_GPList(u_case(trim(dire(1)))//'_PHAS'//trim(car), G)
            if (ind1 ==0) then
               call set_error(1, 'Not found the Parent into the Refinement vector!')
               return
            end if

            !> ----------
            !> Offspring
            !> ----------
            i=2
            do while (i <= nc)
               call Get_InfoKey_StrPhas(trim(dire(i)), ind2, ik2, lab2)

               if (ind2 == 0 .or. len_trim(lab2) == 0) then
                  call set_error(1,"Wrong format for EQUAL directive: "//trim(dire(i)))
                  return
               end if

               !> Phase
               if (ik2 ==0) ik2=k
               write(unit=car, fmt='(i3)') ik2
               car=adjustl(car)

               ind2 = index_GPList(u_case(trim(dire(i)))//'_PHAS'//trim(car), G)
               if (ind2 ==0) then
                  call set_error(1, 'Not found the Parent into the Refinement vector!')
                  return
               end if

               call get_num(trim(dire(i+1)), vet, ivet, iv)

               kc=G%Par(ind2)%L

               !> Assign the values
               G%Par(ind2)%L    =G%Par(ind1)%L
               if (iv ==1) G%Par(ind2)%M=vet(1)

               !> Checks orthers parameters with the same Code
               do n=1,G%NPar
                  if (n==ind1 .or. n == ind2) cycle

                  if (G%Par(n)%L == kc) then
                      G%Par(n)%L =G%Par(ind1)%L
                      if (iv ==1) G%Par(n)%M=vet(1)
                  end if
               end do

               i=i+1
               if (iv ==1) i=i+1
            end do

      end select

      !> Updating Codes
      call Update_GPList_Code(G)

   End Subroutine SetCode_EQUAL_PHAS

End SubModule KeyCod_Phas