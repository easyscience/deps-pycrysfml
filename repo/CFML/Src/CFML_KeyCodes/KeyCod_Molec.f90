!!
Submodule (CFML_KeyCodes) KeyCod_Molec
   implicit none

   Contains
   !!----
   !!---- SUBROUTINE READ_REFCODES_MOL
   !!----
   !!----
   !!---- Update: 14/06/2023
   !!
   Module Subroutine Read_RefCodes_MOL(ffile, n_ini, n_end, IPhas, N_Mol, Mol, G)
      !---- Arguments ----!
      Type(file_type),                   intent(in)     :: ffile
      integer,                           intent(in)     :: n_ini
      integer,                           intent(in)     :: n_end
      integer,                           intent(in)     :: IPhas
      integer,                           intent(in)     :: N_Mol
      type(Molecule_type), dimension(:), intent(in out) :: Mol
      type(GenParList_Type),             intent(in out) :: G

      !---- Local Variables ----!
      integer                 :: i, k

      !> Init
      call clear_error()

      do i=n_ini,n_end
         !> load information on line variable
         line=adjustl(ffile%line(i)%str)
         if (len_trim(line) <=0) cycle
         if (line(1:1) ==" ") cycle
         if (line(1:1) =="!") cycle

         k=index(line,"!")
         if( k /= 0) line=line(:k-1)

         !> Directives
         select case (u_case(line(1:4)))
            case ("FIX ", "FIXE")   ! FIX
               call ReadCode_FIX_MOL(line, IPhas, N_Mol, Mol, G)
               if (err_CFML%Flag) return

            case ("VARY")    ! VARY
               call ReadCode_VARY_MOL(line, IPhas, N_Mol, Mol, G)
               if (err_CFML%Flag) return

            case ("EQUA") ! Equal (Constraints)
               !call ReadCode_EQUAL_MOL()  ! Por hacer
               if (err_CFML%Flag) return
         end select
      end do

   End Subroutine Read_RefCodes_MOL

   !!----
   !!---- SUBROUTINE READCODE_FIX_MOL
   !!----
   !!---- Update: May - 2022
   !!
   Module Subroutine ReadCode_FIX_MOL(String, IPhas, N_Mol, Mol, G)
      !---- Arguments ----!
      character(len=*),                  intent(in)     :: String
      integer,                           intent(in)     :: IPhas
      integer,                           intent(in)     :: N_Mol
      type(Molecule_type), dimension(:), intent(in out) :: Mol
      type(GenParList_Type),             intent(in out) :: G

      !---- Local Variables ----!
      character(len=3) :: car
      integer :: i, j, jj, im, n, k, n_dir, nlong
      integer :: n1, n2, iKey, IPh, jMol

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
      call cut_string(line,nlong)

      !> Get words
      call get_words(line, dire, n_dir)

      i=1
      do while(i <= n_dir)
         !> Is dire(i) a general directive? Vary TL Mol1
         n1 = index_Key_Phas(dire(i))

         !> The next word is a directive?
         j=0
         do jj=i+1, n_dir
            if (index_Key_Phas(dire(jj)) > 0 ) then
               j=jj
               exit
            end if
         end do

         !> dire(i) is a directive: centre, orient, tls, ...
         if (n1 > 0) then

            if (j ==0) then
               if (i == n_dir) then
                  !> Only one directive: vary centre
                  k=iphas
                  im=n_mol

                  select case (n1)
                     case (13, 20)
                        !> All atoms
                        do n2=1,Mol(im)%natoms
                           call Set_RefCodes_MOL('FIX', n1, k, im, Mol(im)%Atname(n2), G)
                        end do

                     case (27:34)
                        call Set_RefCodes_MOL('FIX', n1, k, im, G=G)

                     case (35:40)
                        !> all atoms
                        do n2=1,Mol(im)%natoms
                           call Set_RefCodes_MOL('FIX', n1, k, im, Mol(im)%Atname(n2), G)
                        end do

                     case (41:68)
                        call Set_RefCodes_MOL('FIX', n1, k, im, G=G)

                  end select

                  call GPList_to_Molec(G, k, im, Mol(im))
                  call GPList_from_Molec(Mol(im), im, k, G)

               else
                  !> One directive with options: vary centre mol1 mol2
                  do n=i+1, n_dir
                     call Get_InfoKey_StrMol(dire(n), iKey, IPh, jMol) ! ikey should be 0

                     k=iphas
                     if (iph > 0) k=iPh

                     im=n_mol
                     if (jmol > 0) im=jmol

                     select case (n1)
                        case (13, 20) ! Uiso, Biso
                           do n2=1,Mol(im)%natoms
                              call Set_RefCodes_MOL('FIX', n1, k, im, Mol(im)%Atname(n2), G)
                           end do

                        case (27:34)
                           call Set_RefCodes_MOL('FIX', n1, k, im, G=G)

                        case (35:40)
                           do n2=1,Mol(im)%natoms
                              call Set_RefCodes_MOL('FIX', n1, k, im, Mol(im)%Atname(n2), G)
                           end do

                        case (41:68)
                           call Set_RefCodes_MOL('FIX', n1, k, im, G=G)

                     end select

                     call GPList_to_Molec(G, k, im, Mol(im))
                     call GPList_from_Molec(Mol(im), im, k, G)
                  end do

                  i=n_dir
               end if

            else
               if (i+1 == n_dir) then
                  k=iphas
                  im=n_mol

                  select case (n1)
                     case (13, 20)
                        do n2=1,Mol(im)%natoms
                           call Set_RefCodes_MOL('FIX', n1, k, im, Mol(im)%Atname(n2), G)
                        end do

                     case (27:34)
                        call Set_RefCodes_MOL('FIX', n1, k, im, G=G)

                     case (35:40)
                        do n2=1,Mol(im)%natoms
                           call Set_RefCodes_MOL('FIX', n1, k, im, Mol(im)%Atname(n2), G)
                        end do

                     case (41:68)
                        call Set_RefCodes_MOL('FIX', n1, k, im, G=G)

                  end select

                  call GPList_to_Molec(G, k, im, Mol(im))
                  call GPList_from_Molec(Mol(im), im, k, G)

               else
                  do n=i+1, j-1
                     call Get_InfoKey_StrMol(dire(n), iKey, IPh, jMol) ! ikey should be 0

                     k=iphas
                     if (iph > 0) k=iPh

                     im=n_mol
                     if (jmol > 0) im=jmol

                     select case (n1)
                        case (13, 20)
                           do n2=1,Mol(im)%natoms
                              call Set_RefCodes_MOL('FIX', n1, k, im, Mol(im)%Atname(n2), G)
                           end do

                        case (27:34)
                           call Set_RefCodes_MOL('FIX', n1, k, im, G=G)

                        case (35:40)
                           do n2=1,Mol(im)%natoms
                              call Set_RefCodes_MOL('FIX', n1, k, im, Mol(im)%Atname(n2), G)
                           end do

                        case (41:68)
                           call Set_RefCodes_MOL('FIX', n1, k, im, G=G)

                     end select

                     call GPList_to_Molec(G, k, im, Mol(im))
                     call GPList_from_Molec(Mol(im), im, k, G)
                  end do
               end if

               i=j-1
            end if

         else
            !> dire(i) is local instruction: xc_, ...
            call Get_InfoKey_StrMol(dire(i), iKey, IPh, jMol)

            k=iphas
            if (iph > 0) k=iPh

            im=n_mol
            if (jmol > 0) im=jmol

            select case (ikey)
               case (13, 20)
                  do j=i+1, n_dir
                     !> Check for atom label
                     n2=Index_Atlab_on_Molecule(dire(j), Mol(im))
                     if (n2 > 0) then
                        call Set_RefCodes_MOL('FIX', ikey, k, im, &
                                                  Mol(im)%Atname(n2), G)
                     end if
                  end do

               case (27:34)
                  call Set_RefCodes_MOL('FIX', ikey, k, im, G=G)

               case (35:40)
                  do j=i+1, n_dir
                     !> Check for atom label
                     n2=Index_Atlab_on_Molecule(dire(j),Mol(im))
                     if (n2 > 0) then
                        call Set_RefCodes_MOL('FIX', ikey, k, im, &
                                                  Mol(im)%Atname(n2), G)
                     end if
                  end do

               case (41:68)
                  call Set_RefCodes_MOL('FIX', ikey, k, im, G=G)

            end select

            call GPList_to_Molec(G, k, im, Mol(im))
            call GPList_from_Molec(Mol(im), im, k, G)

         end if

         !> Updating Codes
         call Update_GPList_Code(G)

         !> Debugging
         !call WriteInfo_GPList(G)

         i=i+1
      end do

   End Subroutine ReadCode_FIX_MOL

   !!----
   !!---- SUBROUTINE READCODE_VARY_MOL
   !!----
   !!---- Update: May - 2022
   !!
   Module Subroutine ReadCode_VARY_MOL(String, Iphas, N_Mol, Mol, G)
      !---- Arguments ----!
      character(len=*),                  intent(in)     :: String
      integer,                           intent(in)     :: IPhas
      integer,                           intent(in)     :: N_Mol
      type(Molecule_type), dimension(:), intent(in out) :: Mol
      type(GenParList_Type),             intent(in out) :: G

      !---- Local Variables ----!
      character(len=3) :: car
      integer :: i, j, jj, im, n, k, n_dir, nlong
      integer :: n1, n2, n3, iKey, IPh, jMol


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
      call cut_string(line,nlong)

      !> Get words
      call get_words(line, dire, n_dir)

      i=1
      do while(i <= n_dir)
         !> Is dire(i) a general directive?
         n1 = index_Key_Phas(dire(i))

         !> next directive
         j=0
         do jj=i+1, n_dir
            if (index_Key_Phas(dire(jj)) > 0 ) then
               j=jj
               exit
            end if
         end do

         !> word(i) is a directive: centre, orient, tls, ...
         if (n1 > 0) then

            if (j ==0) then
               if (i == n_dir) then
                  !> last instruction
                  k=iphas
                  im=n_mol

                  select case (n1)
                     case (13, 20)
                        !> All atoms
                        do n2=1,Mol(im)%natoms
                           call Set_RefCodes_MOL('VARY', n1, k, im, Mol(im)%Atname(n2), G)
                        end do

                     case (27:34)
                        call Set_RefCodes_MOL('VARY', n1, k, im, G=G)

                     case (35:40)
                        !> All atoms
                        do n2=1,Mol(im)%natoms
                           call Set_RefCodes_MOL('VARY', n1, k, im, Mol(im)%Atname(n2), G)
                        end do

                     case (41:68)
                        call Set_RefCodes_MOL('VARY', n1, k, im, G=G)

                  end select

                  call GPList_to_Molec(G, k, im, Mol(im))
                  call GPList_from_Molec(Mol(im), im, k, G)

               else
                  do n=i+1, n_dir
                     call Get_InfoKey_StrMol(dire(n), iKey, IPh, jMol) ! ikey should be 0

                     k=iphas
                     if (iph > 0) k=iPh

                     im=n_mol
                     if (jmol > 0) im=jmol

                     select case (n1)
                        case (13, 20)
                           !> All atoms
                           do n2=1,Mol(im)%natoms
                              call Set_RefCodes_MOL('VARY', n1, k, im, Mol(im)%Atname(n2), G)
                           end do

                        case (27:34)
                           call Set_RefCodes_MOL('VARY', n1, k, im, G=G)

                        case (35:40)
                           !> All atoms
                           do n2=1,Mol(im)%natoms
                              call Set_RefCodes_MOL('VARY', n1, k, im, Mol(im)%Atname(n2), G)
                           end do

                        case (41:68)
                           call Set_RefCodes_MOL('VARY', n1, k, im, G=G)

                     end select

                     call GPList_to_Molec(G, k, im, Mol(im))
                     call GPList_from_Molec(Mol(im), im, k, G)

                  end do

                  i=n_dir
               end if

            else
               if (i+1 == n_dir) then
                  k=iphas
                  im=n_mol

                  select case (n1)
                     case (13, 20)
                        !> All atoms
                        do n2=1,Mol(im)%natoms
                           call Set_RefCodes_MOL('VARY', n1, k, im, Mol(im)%Atname(n2), G)
                        end do

                     case (27:34)
                        call Set_RefCodes_MOL('VARY', n1, k, im, G=G)

                     case (35:40)
                        !> All atoms
                        do n2=1,Mol(im)%natoms
                           call Set_RefCodes_MOL('VARY', n1, k, im, Mol(im)%Atname(n2), G)
                        end do

                     case (41:68)
                        call Set_RefCodes_MOL('VARY', n1, k, im, G=G)

                  end select

                  call GPList_to_Molec(G, k, im, Mol(im))
                  call GPList_from_Molec(Mol(im), im, k, G)

               else
                  do n=i+1, j-1
                     call Get_InfoKey_StrMol(dire(n), iKey, IPh, jMol) ! ikey should be 0

                     k=iphas
                     if (iph > 0) k=iPh

                     im=n_mol
                     if (jmol > 0) im=jmol

                     select case (n1)
                        case (13, 20)
                           !> All atoms
                           do n2=1,Mol(im)%natoms
                              call Set_RefCodes_MOL('VARY', n1, k, im, Mol(im)%Atname(n2), G)
                           end do

                        case (27:34)
                           call Set_RefCodes_MOL('VARY', n1, k, im, G=G)

                        case (35:40)
                           !> All atoms
                           do n2=1,Mol(im)%natoms
                              call Set_RefCodes_MOL('VARY', n1, k, im, Mol(im)%Atname(n2), G)
                           end do

                        case (41:68)
                           call Set_RefCodes_MOL('VARY', n1, k, im, G=G)

                     end select

                     call GPList_to_Molec(G, k, im, Mol(im))
                     call GPList_from_Molec(Mol(im), im, k, G)

                  end do
               end if

               i=j-1
            end if

         else
            !> word(i) is local instruction: xc_, ...
            call Get_InfoKey_StrMol(dire(i), iKey, IPh, jMol)

            k=iphas
            if (iph > 0) k=iPh

            im=n_mol
            if (jmol > 0) im=jmol

            select case (ikey)
               case (13, 20) ! UISO, BISO
                  if (i == n_dir) then
                     !> All aoms
                     do j=1, Mol(im)%natoms
                        call Set_RefCodes_MOL('VARY', ikey, k, im, &
                                              Mol(im)%Atname(j), G=G)
                     end do

                  else
                     do j=i+1, n_dir
                        !> Check for atom label
                        n2=Index_Atlab_on_Molecule(dire(j),Mol(im))
                        if (n2 > 0) then
                           call Set_RefCodes_MOL('VARY', ikey, k, im, &
                                                  Mol(im)%Atname(n2), G)
                        else
                           n3=0
                           !> Check for chemical symbol
                           do n=1, Mol(im)%natoms
                              if (trim(dire(j)) == trim(Mol(im)%AtSymb(n))) then
                                 n3=n
                                 exit
                              end if
                           end do
                           if (n3 > 0) then
                              do n=1, Mol(im)%natoms
                                 if (trim(Mol(im)%AtSymb(n)) == &
                                     trim(Mol(im)%AtSymb(n3))) then

                                    call Set_RefCodes_MOL('VARY', ikey, k, im, &
                                                  Mol(im)%Atname(n), G)

                                 end if
                              end do
                           end if
                        end if
                     end do
                  end if

                  call GPList_to_Molec(G, k, im, Mol(im))
                  call GPList_from_Molec(Mol(im), im, k, G)

                  call Update_GPList_Code(G)

                  exit

               case (27:34)
                  call Set_RefCodes_MOL('VARY', ikey, k, im, G=G)

               case (35:40)
                  do j=i+1, n_dir
                     !> Check for atom label
                     n2=Index_Atlab_on_Molecule(dire(j),Mol(im))
                     if (n2 > 0) then
                        call Set_RefCodes_MOL('VARY', ikey, k, im, &
                                                  Mol(im)%Atname(n2), G)
                     end if
                  end do

               case (41:68)
                  call Set_RefCodes_MOL('VARY', ikey, k, im, G=G)

            end select

            call GPList_to_Molec(G, k, im, Mol(im))
            call GPList_from_Molec(Mol(im), im, k, G)

         end if

         !> Updating Codes
         call Update_GPList_Code(G)

         !> Debugging
         !call WriteInfo_GPList(G)

         i=i+1
      end do

   End Subroutine ReadCode_VARY_MOL



   !!--++
   !!--++ SUBROUTINE SETL_REFCODES_MOL
   !!--++
   !!--++
   !!--++ Update: May - 2022
   !!
   Module Subroutine Set_RefCodes_MOL(Keyword, Npar, IPhas, IMol, Lab, G)
      !---- Arguments ----!
      character(len=*),           intent(in)     :: Keyword     ! VARY/FIX/....
      integer,                    intent(in)     :: NPar        ! Specific parameter A,B,C,...
      integer,                    intent(in)     :: IPhas
      integer,                    intent(in)     :: IMol
      character(len=*), optional, intent(in)     :: Lab
      type(GenParList_Type),      intent(in out) :: G

      !---- Local variables ----!
      character(len=4) :: cdire, c_ph, c_mol
      character(len=10):: labc

      !> Init
      call clear_error()

      write(c_ph,fmt='(i3)') iphas
      write(c_mol,fmt='(i3)') imol
      c_ph=adjustl(c_ph)
      c_mol=adjustl(c_mol)
      line=trim(c_mol)//'_PHAS'//trim(c_ph)

      Labc=''
      if (present(lab)) Labc=trim(lab)

      !> keyword
      cdire=u_case(Keyword)
      select case (trim(cdire))
         !> FIX
         case ("FIX")
            select case (Npar)
               case (0)
                  call set_error(1," Error in the Refinement Code for Molecules!")
                  return

               case (13)
                  call Fix_GPList_Par(G,'UISO_'//trim(Labc)//'_MOL'//trim(line))

               case (20)
                  call Fix_GPList_Par(G,'BISO_'//trim(Labc)//'_MOL'//trim(line))

               case (27) ! XC
                  call Fix_GPList_Par(G,'XC_MOL'//trim(line))

               case (28) ! YC
                  call Fix_GPList_Par(G,'YC_MOL'//trim(line))

               case (29) ! ZC
                  call Fix_GPList_Par(G,'ZC_MOL'//trim(line))

               case (30) ! CENTRE
                  call Fix_GPList_Par(G,'XC_MOL'//trim(line))
                  call Fix_GPList_Par(G,'YC_MOL'//trim(line))
                  call Fix_GPList_Par(G,'ZC_MOL'//trim(line))

               case (31) ! THE
                  call Fix_GPList_Par(G,'THE_MOL'//trim(line))

               case (32) ! PHI
                  call Fix_GPList_Par(G,'PHI_MOL'//trim(line))

               case (33) ! CHI
                  call Fix_GPList_Par(G,'CHI_MOL'//trim(line))

               case (34) ! ORIENT
                  call Fix_GPList_Par(G,'THE_MOL'//trim(line))
                  call Fix_GPList_Par(G,'PHI_MOL'//trim(line))
                  call Fix_GPList_Par(G,'CHI_MOL'//trim(line))

               case (35) ! DIST
                  call Fix_GPList_Par(G,'DIST_'//trim(Labc)//'_MOL'//trim(line))

               case (36) ! BANG
                  call Fix_GPList_Par(G,'BANG_'//trim(Labc)//'_MOL'//trim(line))

               case (37) ! TORS
                  call Fix_GPList_Par(G,'TORS_'//trim(Labc)//'_MOL'//trim(line))

               case (38) ! RHO
                  call Fix_GPList_Par(G,'RHO_'//trim(Labc)//'_MOL'//trim(line))

               case (39) ! TH
                  call Fix_GPList_Par(G,'TH_'//trim(Labc)//'_MOL'//trim(line))

               case (40) ! PH
                  call Fix_GPList_Par(G,'PH_'//trim(Labc)//'_MOL'//trim(line))

               case (41)
                  call Fix_GPList_Par(G,'T11_MOL'//trim(line))

               case (42)
                  call Fix_GPList_Par(G,'T22_MOL'//trim(line))

               case (43)
                  call Fix_GPList_Par(G,'T33_MOL'//trim(line))

               case (44)
                  call Fix_GPList_Par(G,'T12_MOL'//trim(line))

               case (45)
                  call Fix_GPList_Par(G,'T13_MOL'//trim(line))

               case (46)
                  call Fix_GPList_Par(G,'T23_MOL'//trim(line))

               case (47)
                  call Fix_GPList_Par(G,'T11_MOL'//trim(line))
                  call Fix_GPList_Par(G,'T22_MOL'//trim(line))
                  call Fix_GPList_Par(G,'T33_MOL'//trim(line))
                  call Fix_GPList_Par(G,'T12_MOL'//trim(line))
                  call Fix_GPList_Par(G,'T13_MOL'//trim(line))
                  call Fix_GPList_Par(G,'T23_MOL'//trim(line))

               case (48)
                  call Fix_GPList_Par(G,'L11_MOL'//trim(line))

               case (49)
                  call Fix_GPList_Par(G,'L22_MOL'//trim(line))

               case (50)
                  call Fix_GPList_Par(G,'L33_MOL'//trim(line))

               case (51)
                  call Fix_GPList_Par(G,'L12_MOL'//trim(line))

               case (52)
                  call Fix_GPList_Par(G,'L13_MOL'//trim(line))

               case (53)
                  call Fix_GPList_Par(G,'L23_MOL'//trim(line))

               case (54)
                  call Fix_GPList_Par(G,'L11_MOL'//trim(line))
                  call Fix_GPList_Par(G,'L22_MOL'//trim(line))
                  call Fix_GPList_Par(G,'L33_MOL'//trim(line))
                  call Fix_GPList_Par(G,'L12_MOL'//trim(line))
                  call Fix_GPList_Par(G,'L13_MOL'//trim(line))
                  call Fix_GPList_Par(G,'L23_MOL'//trim(line))

               case (55)
                  call Fix_GPList_Par(G,'S11_MOL'//trim(line))

               case (56)
                  call Fix_GPList_Par(G,'S12_MOL'//trim(line))

               case (57)
                  call Fix_GPList_Par(G,'S13_MOL'//trim(line))

               case (58)
                  call Fix_GPList_Par(G,'S21_MOL'//trim(line))

               case (59)
                  call Fix_GPList_Par(G,'S22_MOL'//trim(line))

               case (60)
                  call Fix_GPList_Par(G,'S23_MOL'//trim(line))

               case (61)
                  call Fix_GPList_Par(G,'S31_MOL'//trim(line))

               case (62)
                  call Fix_GPList_Par(G,'S32_MOL'//trim(line))

               case (63)
                  call Fix_GPList_Par(G,'S33_MOL'//trim(line))

               case (64)
                  call Fix_GPList_Par(G,'S11_MOL'//trim(line))
                  call Fix_GPList_Par(G,'S12_MOL'//trim(line))
                  call Fix_GPList_Par(G,'S13_MOL'//trim(line))
                  call Fix_GPList_Par(G,'S21_MOL'//trim(line))
                  call Fix_GPList_Par(G,'S22_MOL'//trim(line))
                  call Fix_GPList_Par(G,'S23_MOL'//trim(line))
                  call Fix_GPList_Par(G,'S31_MOL'//trim(line))
                  call Fix_GPList_Par(G,'S32_MOL'//trim(line))
                  call Fix_GPList_Par(G,'S33_MOL'//trim(line))

               case (65) ! TL
                  call Fix_GPList_Par(G,'T11_MOL'//trim(line))
                  call Fix_GPList_Par(G,'T22_MOL'//trim(line))
                  call Fix_GPList_Par(G,'T33_MOL'//trim(line))
                  call Fix_GPList_Par(G,'T12_MOL'//trim(line))
                  call Fix_GPList_Par(G,'T13_MOL'//trim(line))
                  call Fix_GPList_Par(G,'T23_MOL'//trim(line))

                  call Fix_GPList_Par(G,'L11_MOL'//trim(line))
                  call Fix_GPList_Par(G,'L22_MOL'//trim(line))
                  call Fix_GPList_Par(G,'L33_MOL'//trim(line))
                  call Fix_GPList_Par(G,'L12_MOL'//trim(line))
                  call Fix_GPList_Par(G,'L13_MOL'//trim(line))
                  call Fix_GPList_Par(G,'L23_MOL'//trim(line))

               case (66) ! LS
                  call Fix_GPList_Par(G,'L11_MOL'//trim(line))
                  call Fix_GPList_Par(G,'L22_MOL'//trim(line))
                  call Fix_GPList_Par(G,'L33_MOL'//trim(line))
                  call Fix_GPList_Par(G,'L12_MOL'//trim(line))
                  call Fix_GPList_Par(G,'L13_MOL'//trim(line))
                  call Fix_GPList_Par(G,'L23_MOL'//trim(line))

                  call Fix_GPList_Par(G,'S11_MOL'//trim(line))
                  call Fix_GPList_Par(G,'S12_MOL'//trim(line))
                  call Fix_GPList_Par(G,'S13_MOL'//trim(line))
                  call Fix_GPList_Par(G,'S21_MOL'//trim(line))
                  call Fix_GPList_Par(G,'S22_MOL'//trim(line))
                  call Fix_GPList_Par(G,'S23_MOL'//trim(line))
                  call Fix_GPList_Par(G,'S31_MOL'//trim(line))
                  call Fix_GPList_Par(G,'S32_MOL'//trim(line))
                  call Fix_GPList_Par(G,'S33_MOL'//trim(line))

               case (67) ! TS
                  call Fix_GPList_Par(G,'T11_MOL'//trim(line))
                  call Fix_GPList_Par(G,'T22_MOL'//trim(line))
                  call Fix_GPList_Par(G,'T33_MOL'//trim(line))
                  call Fix_GPList_Par(G,'T12_MOL'//trim(line))
                  call Fix_GPList_Par(G,'T13_MOL'//trim(line))
                  call Fix_GPList_Par(G,'T23_MOL'//trim(line))

                  call Fix_GPList_Par(G,'S11_MOL'//trim(line))
                  call Fix_GPList_Par(G,'S12_MOL'//trim(line))
                  call Fix_GPList_Par(G,'S13_MOL'//trim(line))
                  call Fix_GPList_Par(G,'S21_MOL'//trim(line))
                  call Fix_GPList_Par(G,'S22_MOL'//trim(line))
                  call Fix_GPList_Par(G,'S23_MOL'//trim(line))
                  call Fix_GPList_Par(G,'S31_MOL'//trim(line))
                  call Fix_GPList_Par(G,'S32_MOL'//trim(line))
                  call Fix_GPList_Par(G,'S33_MOL'//trim(line))

               case (68) ! TLS
                  call Fix_GPList_Par(G,'T11_MOL'//trim(line))
                  call Fix_GPList_Par(G,'T22_MOL'//trim(line))
                  call Fix_GPList_Par(G,'T33_MOL'//trim(line))
                  call Fix_GPList_Par(G,'T12_MOL'//trim(line))
                  call Fix_GPList_Par(G,'T13_MOL'//trim(line))
                  call Fix_GPList_Par(G,'T23_MOL'//trim(line))

                  call Fix_GPList_Par(G,'L11_MOL'//trim(line))
                  call Fix_GPList_Par(G,'L22_MOL'//trim(line))
                  call Fix_GPList_Par(G,'L33_MOL'//trim(line))
                  call Fix_GPList_Par(G,'L12_MOL'//trim(line))
                  call Fix_GPList_Par(G,'L13_MOL'//trim(line))
                  call Fix_GPList_Par(G,'L23_MOL'//trim(line))

                  call Fix_GPList_Par(G,'S11_MOL'//trim(line))
                  call Fix_GPList_Par(G,'S12_MOL'//trim(line))
                  call Fix_GPList_Par(G,'S13_MOL'//trim(line))
                  call Fix_GPList_Par(G,'S21_MOL'//trim(line))
                  call Fix_GPList_Par(G,'S22_MOL'//trim(line))
                  call Fix_GPList_Par(G,'S23_MOL'//trim(line))
                  call Fix_GPList_Par(G,'S31_MOL'//trim(line))
                  call Fix_GPList_Par(G,'S32_MOL'//trim(line))
                  call Fix_GPList_Par(G,'S33_MOL'//trim(line))

            end select ! Npar

         !> VARY
         case ("VARY")
            select case (Npar)
               case (0)
                  call set_error(1," Error in the Refinement Code for Molecules!")
                  return

               case (13)
                  call Vary_GPList_Par(G,'UISO_'//trim(Labc)//'_MOL'//trim(line))

               case (20)
                  call Vary_GPList_Par(G,'UISO_'//trim(Labc)//'_MOL'//trim(line))

               case (27) ! XC
                  call Vary_GPList_Par(G,'XC_MOL'//trim(line))

               case (28) ! YC
                  call Vary_GPList_Par(G,'YC_MOL'//trim(line))

               case (29) ! ZC
                  call Vary_GPList_Par(G,'ZC_MOL'//trim(line))

               case (30) ! CENT
                  call Vary_GPList_Par(G,'XC_MOL'//trim(line))
                  call Vary_GPList_Par(G,'YC_MOL'//trim(line))
                  call Vary_GPList_Par(G,'ZC_MOL'//trim(line))

               case (31) ! THE
                  call Vary_GPList_Par(G,'THE_MOL'//trim(line))

               case (32) ! PHI
                  call Vary_GPList_Par(G,'PHI_MOL'//trim(line))

               case (33) ! CHI
                  call Vary_GPList_Par(G,'CHI_MOL'//trim(line))

               case (34) ! ORIENT
                  call Vary_GPList_Par(G,'THE_MOL'//trim(line))
                  call Vary_GPList_Par(G,'PHI_MOL'//trim(line))
                  call Vary_GPList_Par(G,'CHI_MOL'//trim(line))

               case (35) ! DIST
                  call Vary_GPList_Par(G,'DIST_'//trim(Labc)//'_MOL'//trim(line))

               case (36) ! BANG
                  call Vary_GPList_Par(G,'BANG_'//trim(Labc)//'_MOL'//trim(line))

               case (37) ! TORS
                  call Vary_GPList_Par(G,'TORS_'//trim(Labc)//'_MOL'//trim(line))

               case (38) ! RHO
                  call Vary_GPList_Par(G,'RHO_'//trim(Labc)//'_MOL'//trim(line))

               case (39) ! TH
                  call Vary_GPList_Par(G,'TH_'//trim(Labc)//'_MOL'//trim(line))

               case (40) ! PH
                  call Vary_GPList_Par(G,'PH_'//trim(Labc)//'_MOL'//trim(line))

               case (41)
                  call Vary_GPList_Par(G,'T11_MOL'//trim(line))

               case (42)
                  call Vary_GPList_Par(G,'T22_MOL'//trim(line))

               case (43)
                  call Vary_GPList_Par(G,'T33_MOL'//trim(line))

               case (44)
                  call Vary_GPList_Par(G,'T12_MOL'//trim(line))

               case (45)
                  call Vary_GPList_Par(G,'T13_MOL'//trim(line))

               case (46)
                  call Vary_GPList_Par(G,'T23_MOL'//trim(line))

               case (47)
                  call Vary_GPList_Par(G,'T11_MOL'//trim(line))
                  call Vary_GPList_Par(G,'T22_MOL'//trim(line))
                  call Vary_GPList_Par(G,'T33_MOL'//trim(line))
                  call Vary_GPList_Par(G,'T12_MOL'//trim(line))
                  call Vary_GPList_Par(G,'T13_MOL'//trim(line))
                  call Vary_GPList_Par(G,'T23_MOL'//trim(line))

               case (48)
                  call Vary_GPList_Par(G,'L11_MOL'//trim(line))

               case (49)
                  call Vary_GPList_Par(G,'L22_MOL'//trim(line))

               case (50)
                  call Vary_GPList_Par(G,'L33_MOL'//trim(line))

               case (51)
                  call Vary_GPList_Par(G,'L12_MOL'//trim(line))

               case (52)
                  call Vary_GPList_Par(G,'L13_MOL'//trim(line))

               case (53)
                  call Vary_GPList_Par(G,'L23_MOL'//trim(line))

               case (54)
                  call Vary_GPList_Par(G,'L11_MOL'//trim(line))
                  call Vary_GPList_Par(G,'L22_MOL'//trim(line))
                  call Vary_GPList_Par(G,'L33_MOL'//trim(line))
                  call Vary_GPList_Par(G,'L12_MOL'//trim(line))
                  call Vary_GPList_Par(G,'L13_MOL'//trim(line))
                  call Vary_GPList_Par(G,'L23_MOL'//trim(line))

               case (55)
                  call Vary_GPList_Par(G,'S11_MOL'//trim(line))

               case (56)
                  call Vary_GPList_Par(G,'S12_MOL'//trim(line))

               case (57)
                  call Vary_GPList_Par(G,'S13_MOL'//trim(line))

               case (58)
                  call Vary_GPList_Par(G,'S21_MOL'//trim(line))

               case (59)
                  call Vary_GPList_Par(G,'S22_MOL'//trim(line))

               case (60)
                  call Vary_GPList_Par(G,'S23_MOL'//trim(line))

               case (61)
                  call Vary_GPList_Par(G,'S31_MOL'//trim(line))

               case (62)
                  call Vary_GPList_Par(G,'S32_MOL'//trim(line))

               case (63)
                  call Vary_GPList_Par(G,'S33_MOL'//trim(line))

               case (64)
                  call Vary_GPList_Par(G,'S11_MOL'//trim(line))
                  call Vary_GPList_Par(G,'S12_MOL'//trim(line))
                  call Vary_GPList_Par(G,'S13_MOL'//trim(line))
                  call Vary_GPList_Par(G,'S21_MOL'//trim(line))
                  call Vary_GPList_Par(G,'S22_MOL'//trim(line))
                  call Vary_GPList_Par(G,'S23_MOL'//trim(line))
                  call Vary_GPList_Par(G,'S31_MOL'//trim(line))
                  call Vary_GPList_Par(G,'S32_MOL'//trim(line))
                  call Vary_GPList_Par(G,'S33_MOL'//trim(line))

               case (65) ! TL
                  call Vary_GPList_Par(G,'T11_MOL'//trim(line))
                  call Vary_GPList_Par(G,'T22_MOL'//trim(line))
                  call Vary_GPList_Par(G,'T33_MOL'//trim(line))
                  call Vary_GPList_Par(G,'T12_MOL'//trim(line))
                  call Vary_GPList_Par(G,'T13_MOL'//trim(line))
                  call Vary_GPList_Par(G,'T23_MOL'//trim(line))

                  call Vary_GPList_Par(G,'L11_MOL'//trim(line))
                  call Vary_GPList_Par(G,'L22_MOL'//trim(line))
                  call Vary_GPList_Par(G,'L33_MOL'//trim(line))
                  call Vary_GPList_Par(G,'L12_MOL'//trim(line))
                  call Vary_GPList_Par(G,'L13_MOL'//trim(line))
                  call Vary_GPList_Par(G,'L23_MOL'//trim(line))

               case (66) ! LS
                  call Vary_GPList_Par(G,'L11_MOL'//trim(line))
                  call Vary_GPList_Par(G,'L22_MOL'//trim(line))
                  call Vary_GPList_Par(G,'L33_MOL'//trim(line))
                  call Vary_GPList_Par(G,'L12_MOL'//trim(line))
                  call Vary_GPList_Par(G,'L13_MOL'//trim(line))
                  call Vary_GPList_Par(G,'L23_MOL'//trim(line))

                  call Vary_GPList_Par(G,'S11_MOL'//trim(line))
                  call Vary_GPList_Par(G,'S12_MOL'//trim(line))
                  call Vary_GPList_Par(G,'S13_MOL'//trim(line))
                  call Vary_GPList_Par(G,'S21_MOL'//trim(line))
                  call Vary_GPList_Par(G,'S22_MOL'//trim(line))
                  call Vary_GPList_Par(G,'S23_MOL'//trim(line))
                  call Vary_GPList_Par(G,'S31_MOL'//trim(line))
                  call Vary_GPList_Par(G,'S32_MOL'//trim(line))
                  call Vary_GPList_Par(G,'S33_MOL'//trim(line))

               case (67) ! TS
                  call Vary_GPList_Par(G,'T11_MOL'//trim(line))
                  call Vary_GPList_Par(G,'T22_MOL'//trim(line))
                  call Vary_GPList_Par(G,'T33_MOL'//trim(line))
                  call Vary_GPList_Par(G,'T12_MOL'//trim(line))
                  call Vary_GPList_Par(G,'T13_MOL'//trim(line))
                  call Vary_GPList_Par(G,'T23_MOL'//trim(line))

                  call Vary_GPList_Par(G,'S11_MOL'//trim(line))
                  call Vary_GPList_Par(G,'S12_MOL'//trim(line))
                  call Vary_GPList_Par(G,'S13_MOL'//trim(line))
                  call Vary_GPList_Par(G,'S21_MOL'//trim(line))
                  call Vary_GPList_Par(G,'S22_MOL'//trim(line))
                  call Vary_GPList_Par(G,'S23_MOL'//trim(line))
                  call Vary_GPList_Par(G,'S31_MOL'//trim(line))
                  call Vary_GPList_Par(G,'S32_MOL'//trim(line))
                  call Vary_GPList_Par(G,'S33_MOL'//trim(line))

               case (68) ! TLS
                  call Vary_GPList_Par(G,'T11_MOL'//trim(line))
                  call Vary_GPList_Par(G,'T22_MOL'//trim(line))
                  call Vary_GPList_Par(G,'T33_MOL'//trim(line))
                  call Vary_GPList_Par(G,'T12_MOL'//trim(line))
                  call Vary_GPList_Par(G,'T13_MOL'//trim(line))
                  call Vary_GPList_Par(G,'T23_MOL'//trim(line))

                  call Vary_GPList_Par(G,'L11_MOL'//trim(line))
                  call Vary_GPList_Par(G,'L22_MOL'//trim(line))
                  call Vary_GPList_Par(G,'L33_MOL'//trim(line))
                  call Vary_GPList_Par(G,'L12_MOL'//trim(line))
                  call Vary_GPList_Par(G,'L13_MOL'//trim(line))
                  call Vary_GPList_Par(G,'L23_MOL'//trim(line))

                  call Vary_GPList_Par(G,'S11_MOL'//trim(line))
                  call Vary_GPList_Par(G,'S12_MOL'//trim(line))
                  call Vary_GPList_Par(G,'S13_MOL'//trim(line))
                  call Vary_GPList_Par(G,'S21_MOL'//trim(line))
                  call Vary_GPList_Par(G,'S22_MOL'//trim(line))
                  call Vary_GPList_Par(G,'S23_MOL'//trim(line))
                  call Vary_GPList_Par(G,'S31_MOL'//trim(line))
                  call Vary_GPList_Par(G,'S32_MOL'//trim(line))
                  call Vary_GPList_Par(G,'S33_MOL'//trim(line))

            end select ! Npar

      end select ! Directives

   End Subroutine Set_RefCodes_MOL

End Submodule KeyCod_Molec