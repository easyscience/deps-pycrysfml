Submodule (CFML_Keywords_Code_Parser) KWC_FillCodes_MolX
   !---- Variables ----!
   implicit none

 Contains
    !!--++
    !!--++ Module Subroutine Fill_RefCodes_Molcrys(Keyv,Dire,Na,Nb,Xl,Xu,Xs,Ic,Molcrys,NMol)
    !!--++    integer,                      intent(in)     :: Keyv
    !!--++    character(len=*),             intent(in)     :: Dire
    !!--++    integer,                      intent(in)     :: Na
    !!--++    integer,                      intent(in)     :: Nb
    !!--++    real(kind=cp),                intent(in)     :: Xl
    !!--++    real(kind=cp),                intent(in)     :: Xu
    !!--++    real(kind=cp),                intent(in)     :: Xs
    !!--++    integer,                      intent(in)     :: Ic
    !!--++    type(MolCrystal_Type), intent(in out) :: MolCrys
    !!--++    integer,                      intent(in)     :: NMol
    !!--++
    !!--++ Overloaded
    !!--++ Write on Vectors the Information for Free Atoms
    !!--++
    !!--++ Update: March - 2005
    !!
    Module Subroutine Fill_RefCodes_Molcrys(Keyv,Dire,Na,Nb,Xl,Xu,Xs,Ic,Molcrys,Nmol)
       !---- Arguments ----!
       integer,                      intent(in)     :: Keyv
       character(len=*),             intent(in)     :: Dire
       integer,                      intent(in)     :: Na
       integer,                      intent(in)     :: Nb
       real(kind=cp),                intent(in)     :: Xl
       real(kind=cp),                intent(in)     :: Xu
       real(kind=cp),                intent(in)     :: Xs
       integer,                      intent(in)     :: Ic
       type(MolCrystal_Type),        intent(in out) :: MolCrys
       integer,                      intent(in)     :: NMol

       !---- Local variables ----!
       character(len=2) :: car
       integer          :: i, j, nc, naa

       call clear_error()
       if (Na <= 0) then
          Err_CFML%Flag=.true.
          Err_CFML%Msg="The number of atom no defined"
          return
       end if

       Select Type (atm => molcrys%atm)
        type is(atm_ref_type)
           select case (dire)
              !---- FIX Directive ----!
              case ("fix")

                 select case (Keyv)
                    case (0)
                       !---- Nb must be different zero ----!
                       select case (nb)
                          case (0)
                             Err_CFML%Flag=.true.
                             Err_CFML%Msg="Option Not defined"
                             return

                          case ( 1:3)
                             !---- X_, Y_, Z_ ----!
                             if (na <= molcrys%n_free) then
                                if ( atm(na)%l_x(nb) /=0) then
                                   nc= atm(na)%l_x(nb)
                                   call Delete_RefCodes(nc,MolCrys)
                                end if
                             else
                                naa=na-molcrys%n_free
                                do i=1,molcrys%n_mol
                                   if (naa > molcrys%mol(i)%natoms) then
                                      naa=naa-molcrys%mol(i)%natoms
                                      cycle
                                   end if
                                   if (molcrys%mol(i)%lI_coor(nb,naa) /=0) then
                                      nc=molcrys%mol(i)%lI_coor(nb,naa)
                                      call Delete_RefCodes(nc,MolCrys)
                                   end if
                                end do
                             end if

                          case ( 4) ! Keyv=0, nb=4 Biso for atom na
                             !---- Biso_ ----!
                             if (na <= molcrys%n_free) then
                                if ( atm(na)%l_u_iso /=0) then
                                   nc= atm(na)%l_u_iso
                                   call Delete_RefCodes(nc,MolCrys)
                                end if
                             else
                                naa=na-molcrys%n_free
                                do i=1,molcrys%n_mol
                                   if (naa > molcrys%mol(i)%natoms) then
                                      naa=naa-molcrys%mol(i)%natoms
                                      cycle
                                   end if
                                   if (molcrys%mol(i)%lu_iso(naa) /=0) then
                                      nc=molcrys%mol(i)%lu_iso(naa)
                                      call Delete_RefCodes(nc,MolCrys)
                                   end if
                                end do
                             end if

                          case ( 5)
                             !---- Occ_ ----!
                             if (na <= molcrys%n_free) then
                                if ( atm(na)%l_occ /=0) then
                                   nc= atm(na)%l_occ
                                   call Delete_RefCodes(nc,MolCrys)
                                end if
                             else
                                naa=na-molcrys%n_free
                                do i=1,molcrys%n_mol
                                   if (naa > molcrys%mol(i)%natoms) then
                                      naa=naa-molcrys%mol(i)%natoms
                                      cycle
                                   end if
                                   if (molcrys%mol(i)%locc(naa) /=0) then
                                      nc=molcrys%mol(i)%locc(naa)
                                      call Delete_RefCodes(nc,MolCrys)
                                   end if
                                end do
                             end if

                          case ( 6:11)
                             !---- B11_, ..., B23_ ----!
                             if (na <= molcrys%n_free) then
                                if ( atm(na)%l_u(nb-5) /=0) then
                                   nc= atm(na)%l_u(nb-5)
                                   call Delete_RefCodes(nc,MolCrys)
                                end if
                             else
                                Err_CFML%Flag=.true.
                                Err_CFML%Msg="Option Not defined"
                                return
                             end if

                          case (12)
                             !---- Banis_ ----!
                             if (na <= molcrys%n_free) then
                                do j=1,6
                                   if ( atm(na)%l_u(j) /=0) then
                                      nc= atm(na)%l_u(j)
                                      call Delete_RefCodes(nc,MolCrys)
                                   end if
                                end do
                             else
                                Err_CFML%Flag=.true.
                                Err_CFML%Msg="Option Not defined"
                                return
                             end if

                          case (13:15)
                             !---- Xc_, Yc_, Zc_ ----!
                             select case (nmol)
                                case (-1)
                                   Err_CFML%Flag=.true.
                                   Err_CFML%Msg="Option Not defined"
                                   return

                                case (0)
                                   do i=1,molcrys%n_mol
                                      if (molcrys%mol(i)%lxcentre(nb-12) /=0) then
                                         nc=molcrys%mol(i)%lxcentre(nb-12)
                                         call Delete_RefCodes(nc,MolCrys)
                                      end if
                                   end do

                                case (1:)
                                   if (molcrys%mol(nmol)%lxcentre(nb-12) /=0) then
                                      nc=molcrys%mol(nmol)%lxcentre(nb-12)
                                      call Delete_RefCodes(nc,MolCrys)
                                   end if
                             end select

                          case (16:18)
                             !---- Theta_, Phi_, Chi_ ----!
                             select case (nmol)
                                case (-1)
                                   Err_CFML%Flag=.true.
                                   Err_CFML%Msg="Option Not defined"
                                   return

                                case (0)
                                   do i=1,molcrys%n_mol
                                      if (molcrys%mol(i)%lOrient(nb-15) /=0) then
                                         nc=molcrys%mol(i)%lOrient(nb-15)
                                         call Delete_RefCodes(nc,MolCrys)
                                      end if
                                   end do

                                case (1:)
                                   if (molcrys%mol(nmol)%lOrient(nb-15) /=0) then
                                      nc=molcrys%mol(nmol)%lOrient(nb-15)
                                      call Delete_RefCodes(nc,MolCrys)
                                   end if
                             end select

                          case (19:21)
                             !!! Not yet implemented !!!

                       end select ! nb

                    case (1)
                       !---- XYZ ----!
                       if (na <= molcrys%n_free) then
                          do j=1,3
                             if ( atm(na)%l_x(j) /=0) then
                                nc= atm(na)%l_x(j)
                                call Delete_RefCodes(nc,MolCrys)
                             end if
                          end do
                       else
                          naa=na-molcrys%n_free
                          do i=1,molcrys%n_mol
                             if (naa > molcrys%mol(i)%natoms) then
                                naa=naa-molcrys%mol(i)%natoms
                                cycle
                             end if
                             do j=1,3
                                if (molcrys%mol(i)%lI_coor(j,naa) /=0) then
                                   nc=molcrys%mol(i)%lI_coor(j,naa)
                                   call Delete_RefCodes(nc,MolCrys)
                                end if
                             end do
                          end do
                       end if

                    case (2)
                       !---- OCC ----!
                       if (na <= molcrys%n_free) then
                          if ( atm(na)%l_occ /=0) then
                             nc= atm(na)%l_occ
                             call Delete_RefCodes(nc,MolCrys)
                          end if
                       else
                          naa=na-molcrys%n_free
                          do i=1,molcrys%n_mol
                             if (naa > molcrys%mol(i)%natoms) then
                                naa=naa-molcrys%mol(i)%natoms
                                cycle
                             end if
                             if (molcrys%mol(i)%locc(naa) /=0) then
                                nc=molcrys%mol(i)%locc(naa)
                                call Delete_RefCodes(nc,MolCrys)
                             end if
                          end do
                       end if

                    case (3)
                       !---- BIS ----!
                       if (na <= molcrys%n_free) then
                          if ( atm(na)%l_u_iso /=0) then
                             nc= atm(na)%l_u_iso
                             call Delete_RefCodes(nc,MolCrys)
                          end if
                       else
                          naa=na-molcrys%n_free
                          do i=1,molcrys%n_mol
                             if (naa > molcrys%mol(i)%natoms) then
                                naa=naa-molcrys%mol(i)%natoms
                                cycle
                             end if
                             if (molcrys%mol(i)%lu_iso(naa) /=0) then
                                nc=molcrys%mol(i)%lu_iso(naa)
                                call Delete_RefCodes(nc,MolCrys)
                             end if
                          end do
                       end if

                    case (4)
                       !---- BAN ----!
                       if (na <= molcrys%n_free) then
                          do j=1,6
                             if ( atm(na)%l_u(j) /=0) then
                                nc= atm(na)%l_u(j)
                                call Delete_RefCodes(nc,MolCrys)
                             end if
                          end do
                       else
                          Err_CFML%Flag=.true.
                          Err_CFML%Msg="Option Not defined"
                          return
                       end if

                    case (5)
                       !---- ALL ----!
                       if (na <= molcrys%n_free) then
                          do j=1,3
                             if ( atm(na)%l_x(j) /=0) then
                                nc= atm(na)%l_x(j)
                                call Delete_RefCodes(nc,MolCrys)
                             end if
                          end do
                       else
                          naa=na-molcrys%n_free
                          do i=1,molcrys%n_mol
                             if (naa > molcrys%mol(i)%natoms) then
                                naa=naa-molcrys%mol(i)%natoms
                                cycle
                             end if
                             do j=1,3
                                if (molcrys%mol(i)%lI_coor(j,naa) /=0) then
                                   nc=molcrys%mol(i)%lI_coor(j,naa)
                                   call Delete_RefCodes(nc,MolCrys)
                                end if
                             end do
                          end do
                       end if

                       if (na <= molcrys%n_free) then
                          if ( atm(na)%l_occ /=0) then
                             nc= atm(na)%l_occ
                             call Delete_RefCodes(nc,MolCrys)
                          end if
                       else
                          naa=na-molcrys%n_free
                          do i=1,molcrys%n_mol
                             if (naa > molcrys%mol(i)%natoms) then
                                naa=naa-molcrys%mol(i)%natoms
                                cycle
                             end if
                             if (molcrys%mol(i)%locc(naa) /=0) then
                                nc=molcrys%mol(i)%locc(naa)
                                call Delete_RefCodes(nc,MolCrys)
                             end if
                          end do
                       end if

                       if (na <= molcrys%n_free) then
                          if ( atm(na)%l_u_iso /=0) then
                             nc= atm(na)%l_u_iso
                             call Delete_RefCodes(nc,MolCrys)
                          end if
                       else
                          naa=na-molcrys%n_free
                          do i=1,molcrys%n_mol
                             if (naa > molcrys%mol(i)%natoms) then
                                naa=naa-molcrys%mol(i)%natoms
                                cycle
                             end if
                             if (molcrys%mol(i)%lu_iso(naa) /=0) then
                                nc=molcrys%mol(i)%lu_iso (naa)
                                call Delete_RefCodes(nc,MolCrys)
                             end if
                          end do
                       end if

                       if (na <= molcrys%n_free) then
                          do j=1,6
                             if ( atm(na)%l_u(j) /=0) then
                                nc= atm(na)%l_u(j)
                                call Delete_RefCodes(nc,MolCrys)
                             end if
                          end do
                       end if

                       select case (nmol)
                          case (-1)
                             Err_CFML%Flag=.true.
                             Err_CFML%Msg="Option Not defined"
                             return

                          case (0)
                             do i=1,molcrys%n_mol
                                do j=1,3
                                   if (molcrys%mol(i)%lxcentre(j) /=0) then
                                      nc=molcrys%mol(i)%lxcentre(j)
                                      call Delete_RefCodes(nc,molcrys)
                                   end if
                                end do

                                do j=1,3
                                   if (molcrys%mol(i)%lOrient(j) /=0) then
                                      nc=molcrys%mol(i)%lOrient(j)
                                      call Delete_RefCodes(nc,molcrys)
                                   end if
                                end do
                             end do

                          case (1:)
                             do j=1,3
                                if (molcrys%mol(nmol)%lxcentre(j) /=0) then
                                   nc=molcrys%mol(nmol)%lxcentre(j)
                                   call Delete_RefCodes(nc,molcrys)
                                end if
                             end do

                             do j=1,3
                                if (molcrys%mol(nmol)%lOrient(j) /=0) then
                                   nc=molcrys%mol(nmol)%lOrient(j)
                                   call Delete_RefCodes(nc,molcrys)
                                end if
                             end do
                       end select

                       !!! Not yet Implemented !!!

                    case (6)
                       !---- CEN ----!
                       select case (nmol)
                          case (-1)
                             Err_CFML%Flag=.true.
                             Err_CFML%Msg="Option Not defined"
                             return

                          case (0)
                             do i=1,molcrys%n_mol
                                do j=1,3
                                   if (molcrys%mol(i)%lxcentre(j) /=0) then
                                      nc=molcrys%mol(i)%lxcentre(j)
                                      call Delete_RefCodes(nc,molcrys)
                                   end if
                                end do
                             end do

                          case (1:)
                             do j=1,3
                                if (molcrys%mol(nmol)%lxcentre(j) /=0) then
                                   nc=molcrys%mol(nmol)%lxcentre(j)
                                   call Delete_RefCodes(nc,molcrys)
                                end if
                             end do
                       end select

                    case (7)
                       !---- ORI ----!
                       select case (nmol)
                          case (-1)
                             Err_CFML%Flag=.true.
                             Err_CFML%Msg="Option Not defined"
                             return

                          case (0)
                             do i=1,molcrys%n_mol
                                do j=1,3
                                   if (molcrys%mol(i)%lOrient(j) /=0) then
                                      nc=molcrys%mol(i)%lOrient(j)
                                      call Delete_RefCodes(nc,molcrys)
                                   end if
                                end do
                             end do

                          case (1:)
                             do j=1,3
                                if (molcrys%mol(nmol)%lOrient(j) /=0) then
                                  nc=molcrys%mol(nmol)%lOrient(j)
                                   call Delete_RefCodes(nc,molcrys)
                                end if
                             end do
                       end select

                    case (8)
                       !---- THE ----!
                       !!! Not yet Implemented !!!!
                 end select

              !---- VARY Directive ----!
              case ("var")

                 select case (Keyv)
                    case (0)
                       !---- Nb must be different zero ----!
                       select case (nb)
                          case (0)
                             Err_CFML%Flag=.true.
                             Err_CFML%Msg="Option Not defined"
                             return

                          case ( 1:3)
                             !---- X_, Y_, Z_ ----!
                             if (na <= molcrys%n_free) then
                                if ( atm(na)%l_x(nb) ==0) then
                                    atm(na)%m_x(nb)=1.0
                                   call get_atompos_ctr( atm(na)%x,   &
                                                        molcrys%Spg,np_refi, &
                                                         atm(na)%l_x,  &
                                                         atm(na)%m_x)
                                   if ( atm(na)%l_x(nb) == np_refi) then
                                      V_Vec(np_refi)= atm(na)%x(nb)
                                      V_Name(np_refi)=trim(code_nam(nb))//trim( atm(na)%lab)
                                      V_Bounds(1,np_refi)=xl
                                      V_Bounds(2,np_refi)=xu
                                      V_Bounds(3,np_refi)=xs
                                      V_BCon(np_refi)=ic
                                      V_list(np_refi)=na
                                   else
                                      np_refi=np_refi-1
                                   end if
                                end if
                             else
                                naa=na-molcrys%n_free
                                do i=1,molcrys%n_mol
                                   if (naa > molcrys%mol(i)%natoms) then
                                      naa=naa-molcrys%mol(i)%natoms
                                      cycle
                                   end if

                                   if (molcrys%mol(i)%lI_coor(nb,naa) ==0) then
                                      molcrys%mol(i)%mI_coor(nb,naa)=1.0
                                      call get_atompos_ctr(molcrys%mol(i)%I_Coor(:,naa),  &
                                                           molcrys%Spg, np_refi,          &
                                                           molcrys%mol(i)%lI_coor(:,naa), &
                                                           molcrys%mol(i)%mI_coor(:,naa))
                                      if (molcrys%mol(i)%lI_coor(nb,naa) == np_refi) then
                                         V_Vec(np_refi)=molcrys%mol(i)%I_Coor(nb,naa)
                                         V_Name(np_refi)=trim(code_nam(nb))//trim(molcrys%mol(i)%AtName(naa))
                                         V_Bounds(1,np_refi)=xl
                                         V_Bounds(2,np_refi)=xu
                                         V_Bounds(3,np_refi)=xs
                                         V_BCon(np_refi)=ic
                                         V_list(np_refi)=na
                                      else
                                         np_refi=np_refi-1
                                      end if
                                   end if
                                end do
                             end if

                          case ( 4)
                             !---- Biso_ ----!
                             if (na <= molcrys%n_free) then
                                if ( atm(na)%l_u_iso ==0) then
                                   np_refi=np_refi+1
                                   V_Vec(np_refi)= atm(na)%u_iso
                                   V_Name(np_refi)=trim(code_nam(nb))//trim( atm(na)%lab)
                                    atm(na)%m_u_iso=1.0
                                    atm(na)%l_u_iso=np_refi
                                   V_Bounds(1,np_refi)=xl
                                   V_Bounds(2,np_refi)=xu
                                   V_Bounds(3,np_refi)=xs
                                   V_BCon(np_refi)=ic
                                   V_list(np_refi)=na
                                end if
                             else
                                naa=na-molcrys%n_free
                                do i=1,molcrys%n_mol
                                   if (naa > molcrys%mol(i)%natoms) then
                                      naa=naa-molcrys%mol(i)%natoms
                                      cycle
                                   end if
                                   if (molcrys%mol(i)%lu_iso (naa) ==0) then
                                      np_refi=np_refi+1
                                      V_Vec(np_refi)=molcrys%mol(i)%u_iso(naa)
                                      V_Name(np_refi)=trim(code_nam(nb))//trim(molcrys%mol(i)%AtName(naa))
                                      molcrys%mol(i)%mu_iso (naa)=1.0
                                      molcrys%mol(i)%lu_iso (naa)=np_refi
                                      V_Bounds(1,np_refi)=xl
                                      V_Bounds(2,np_refi)=xu
                                      V_Bounds(3,np_refi)=xs
                                      V_BCon(np_refi)=ic
                                      V_list(np_refi)=na
                                   end if
                                end do
                             end if

                          case ( 5)
                             !---- Occ_ ----!
                             if (na <= molcrys%n_free) then
                                if ( atm(na)%l_occ ==0) then
                                   np_refi=np_refi+1
                                   V_Vec(np_refi)= atm(na)%occ
                                   V_Name(np_refi)=trim(code_nam(nb))//trim( atm(na)%lab)
                                    atm(na)%m_occ=1.0
                                    atm(na)%l_occ=np_refi
                                   V_Bounds(1,np_refi)=xl
                                   V_Bounds(2,np_refi)=xu
                                   V_Bounds(3,np_refi)=xs
                                   V_BCon(np_refi)=ic
                                   V_list(np_refi)=na
                                end if
                             else
                                naa=na-molcrys%n_free
                                do i=1,molcrys%n_mol
                                   if (naa > molcrys%mol(i)%natoms) then
                                      naa=naa-molcrys%mol(i)%natoms
                                      cycle
                                   end if
                                   if (molcrys%mol(i)%locc (naa) ==0) then
                                      np_refi=np_refi+1
                                      V_Vec(np_refi)=molcrys%mol(i)%occ(naa)
                                      V_Name(np_refi)=trim(code_nam(nb))//trim(molcrys%mol(i)%AtName(naa))
                                      molcrys%mol(i)%mocc (naa)=1.0
                                      molcrys%mol(i)%locc (naa)=np_refi
                                      V_Bounds(1,np_refi)=xl
                                      V_Bounds(2,np_refi)=xu
                                      V_Bounds(3,np_refi)=xs
                                      V_BCon(np_refi)=ic
                                      V_list(np_refi)=na
                                   end if
                                end do
                             end if

                          case ( 6:11)
                             !---- B11_, ..., B23_ ----!
                             if (na <= molcrys%n_free) then
                                if ( atm(na)%l_u(nb-5) ==0) then
                                    atm(na)%m_u(nb-5)=1.0
                                   call get_atombet_ctr( atm(na)%x, atm(na)%u,molcrys%Spg, &
                                                       np_refi, atm(na)%l_u, atm(na)%m_u)
                                   if ( atm(na)%l_u(nb-5) == np_refi) then
                                      V_Vec(np_refi)= atm(na)%u(nb-5)
                                      V_Name(np_refi)=trim(code_nam(nb))//trim( atm(na)%lab)
                                      V_Bounds(1,np_refi)=xl
                                      V_Bounds(2,np_refi)=xu
                                      V_Bounds(3,np_refi)=xs
                                      V_BCon(np_refi)=ic
                                      V_list(np_refi)=na
                                   else
                                      np_refi=np_refi-1
                                   end if
                                end if
                             else
                                Err_CFML%Flag=.true.
                                Err_CFML%Msg="Option Not defined"
                                return
                             end if

                          case (12)
                             !---- Banis_ ----!
                             if (na <= molcrys%n_free) then
                                do j=1,6
                                   if ( atm(na)%l_u(j) ==0) then
                                       atm(na)%m_u(j)=1.0
                                      call get_atombet_ctr( atm(na)%x, atm(na)%u,molcrys%Spg, &
                                                           np_refi, atm(na)%l_u, atm(na)%m_u)
                                      if ( atm(na)%l_u(j) == np_refi) then
                                         V_Vec(np_refi)= atm(na)%u(j)
                                         V_Name(np_refi)=trim(code_nam(5+j))//trim( atm(na)%lab)
                                         V_Bounds(1,np_refi)=xl
                                         V_Bounds(2,np_refi)=xu
                                         V_Bounds(3,np_refi)=xs
                                         V_BCon(np_refi)=ic
                                         V_list(np_refi)=na
                                      else
                                         np_refi=np_refi-1
                                      end if
                                   end if
                                end do
                             else
                                Err_CFML%Flag=.true.
                                Err_CFML%Msg="Option Not defined"
                                return
                             end if

                          case (13:15)
                             !---- Xc_, Yc_, Zc_ ----!
                             select case (nmol)
                                case (-1)
                                   Err_CFML%Flag=.true.
                                   Err_CFML%Msg="Option Not defined"
                                   return

                                case (0)
                                   do i=1,molcrys%n_mol
                                      write(unit=car,fmt="(i2)") i
                                      car=adjustl(car)
                                      if (molcrys%mol(i)%lxcentre(nb-12) ==0) then
                                         np_refi=np_refi+1
                                         V_Vec(np_refi)=molcrys%mol(i)%xcentre(nb-12)
                                         V_Name(np_refi)=trim(code_nam(nb))//"Mol"//trim(car)
                                         molcrys%mol(i)%mxcentre(nb-12)=1.0
                                         molcrys%mol(i)%lxcentre(nb-12)=np_refi
                                         V_Bounds(1,np_refi)=xl
                                         V_Bounds(2,np_refi)=xu
                                         V_Bounds(3,np_refi)=xs
                                         V_BCon(np_refi)=ic
                                         V_list(np_refi)=-i
                                      end if
                                   end do

                                case (1:)
                                   write(unit=car,fmt="(i2)") nmol
                                   car=adjustl(car)
                                   if (molcrys%mol(nmol)%lxcentre(nb-12) ==0) then
                                      np_refi=np_refi+1
                                      V_Vec(np_refi)=molcrys%mol(nmol)%xcentre(nb-12)
                                      V_Name(np_refi)=trim(code_nam(nb))//"entre_Mol"//trim(car)
                                      molcrys%mol(nmol)%mxcentre(nb-12)=1.0
                                      molcrys%mol(nmol)%lxcentre(nb-12)=np_refi
                                      V_Bounds(1,np_refi)=xl
                                      V_Bounds(2,np_refi)=xu
                                      V_Bounds(3,np_refi)=xs
                                      V_BCon(np_refi)=ic
                                      V_list(np_refi)=-nmol
                                   end if
                             end select

                          case (16:18)
                             !---- Theta_, Phi_, Chi_ ----!
                             select case (nmol)
                                case (-1)
                                   Err_CFML%Flag=.true.
                                   Err_CFML%Msg="Option Not defined"
                                   return

                                case (0)
                                   do i=1,molcrys%n_mol
                                      write(unit=car,fmt="(i2)") i
                                      car=adjustl(car)
                                      if (molcrys%mol(i)%lOrient(nb-15) ==0) then
                                         np_refi=np_refi+1
                                         V_Vec(np_refi)=molcrys%mol(i)%Orient(nb-15)
                                         V_Name(np_refi)=trim(code_nam(nb))//"Orient_Mol"//trim(car)
                                         molcrys%mol(i)%mOrient(nb-15)=1.0
                                         molcrys%mol(i)%lOrient(nb-15)=np_refi
                                         V_Bounds(1,np_refi)=xl
                                         V_Bounds(2,np_refi)=xu
                                         V_Bounds(3,np_refi)=xs
                                         V_BCon(np_refi)=ic
                                         V_list(np_refi)=-i
                                      end if
                                   end do

                                case (1:)
                                   write(unit=car,fmt="(i2)") nmol
                                   car=adjustl(car)
                                   if (molcrys%mol(nmol)%lOrient(nb-15) ==0) then
                                      np_refi=np_refi+1
                                      V_Vec(np_refi)=molcrys%mol(nmol)%Orient(nb-15)
                                      V_Name(np_refi)=trim(code_nam(nb))//"Orient_Mol"//trim(car)
                                      molcrys%mol(nmol)%mOrient(nb-15)=1.0
                                      molcrys%mol(nmol)%lOrient(nb-15)=np_refi
                                      V_Bounds(1,np_refi)=xl
                                      V_Bounds(2,np_refi)=xu
                                      V_Bounds(3,np_refi)=xs
                                      V_BCon(np_refi)=ic
                                      V_list(np_refi)=-nmol
                                   end if
                             end select

                          case (19:21)
                             !!! Not Yet Implemented !!!

                       end select ! nb

                    case (1)
                       !---- XYZ ----!
                       if (na <= molcrys%n_free) then
                          do j=1,3
                             if ( atm(na)%l_x(j) ==0) then
                                 atm(na)%m_x(j)=1.0
                                call get_atompos_ctr( atm(na)%x,   &
                                                     molcrys%Spg,np_refi, &
                                                      atm(na)%l_x,  &
                                                      atm(na)%m_x)
                                if ( atm(na)%l_x(j) == np_refi) then
                                   V_Vec(np_refi)= atm(na)%x(j)
                                   V_Name(np_refi)=trim(code_nam(j))//trim( atm(na)%lab)
                                   V_Bounds(1,np_refi)=xl
                                   V_Bounds(2,np_refi)=xu
                                   V_Bounds(3,np_refi)=xs
                                   V_BCon(np_refi)=ic
                                   V_list(np_refi)=na
                                else
                                   np_refi=np_refi-1
                                end if
                             end if
                          end do
                       else
                          naa=na-molcrys%n_free
                          do i=1,molcrys%n_mol
                             if (naa > molcrys%mol(i)%natoms) then
                                 naa=naa-molcrys%mol(i)%natoms
                                 cycle
                             end if
                             do j=1,3
                                if (molcrys%mol(i)%lI_coor(j,naa) ==0) then
                                   molcrys%mol(i)%mI_coor(nb,naa)=1.0
                                   call get_atompos_ctr(molcrys%mol(i)%I_Coor(:,naa), &
                                                        molcrys%Spg,np_refi,   &
                                                        molcrys%mol(i)%lI_coor(:,naa), &
                                                        molcrys%mol(i)%mI_coor(:,naa))
                                   if (molcrys%mol(i)%lI_coor(j,naa) == np_refi) then
                                      V_Vec(np_refi)=molcrys%mol(i)%I_Coor(nb,naa)
                                      V_Name(np_refi)=trim(code_nam(j))//trim(molcrys%mol(i)%AtName(naa))
                                      V_Bounds(1,np_refi)=xl
                                      V_Bounds(2,np_refi)=xu
                                      V_Bounds(3,np_refi)=xs
                                      V_BCon(np_refi)=ic
                                      V_list(np_refi)=na
                                   else
                                      np_refi=np_refi-1
                                   end if
                                end if
                             end do
                          end do
                       end if

                    case (2)
                       !---- OCC ----!
                       if (na <= molcrys%n_free) then
                          if ( atm(na)%l_occ ==0) then
                             np_refi=np_refi+1
                             V_Vec(np_refi)= atm(na)%occ
                             V_Name(np_refi)=trim(code_nam(5))//trim( atm(na)%lab)
                              atm(na)%m_occ=1.0
                              atm(na)%l_occ=np_refi
                             V_Bounds(1,np_refi)=xl
                             V_Bounds(2,np_refi)=xu
                             V_Bounds(3,np_refi)=xs
                             V_BCon(np_refi)=ic
                             V_list(np_refi)=na
                          end if
                       else
                          naa=na-molcrys%n_free
                          do i=1,molcrys%n_mol
                             if (naa > molcrys%mol(i)%natoms) then
                                naa=naa-molcrys%mol(i)%natoms
                                cycle
                             end if

                             if (molcrys%mol(i)%locc (naa) ==0) then
                                np_refi=np_refi+1
                                V_Vec(np_refi)=molcrys%mol(i)%Occ(naa)
                                V_Name(np_refi)=trim(code_nam(5))//trim(molcrys%mol(i)%AtName(naa))
                                molcrys%mol(i)%mocc (naa)=1.0
                                molcrys%mol(i)%locc (naa)=np_refi
                                V_Bounds(1,np_refi)=xl
                                V_Bounds(2,np_refi)=xu
                                V_Bounds(3,np_refi)=xs
                                V_BCon(np_refi)=ic
                                V_list(np_refi)=na
                             end if
                          end do
                       end if

                    case (3)
                       !---- BIS ----!
                       if (na <= molcrys%n_free) then
                          if ( atm(na)%l_u_iso ==0) then
                             np_refi=np_refi+1
                             V_Vec(np_refi)= atm(na)%u_iso
                             V_Name(np_refi)=trim(code_nam(4))//trim( atm(na)%lab)
                              atm(na)%m_u_iso=1.0
                              atm(na)%l_u_iso=np_refi
                             V_Bounds(1,np_refi)=xl
                             V_Bounds(2,np_refi)=xu
                             V_Bounds(3,np_refi)=xs
                             V_BCon(np_refi)=ic
                             V_list(np_refi)=na
                          end if
                       else
                          naa=na-molcrys%n_free
                          do i=1,molcrys%n_mol
                             if (naa > molcrys%mol(i)%natoms) then
                                naa=naa-molcrys%mol(i)%natoms
                                cycle
                             end if
                             if (molcrys%mol(i)%lu_iso (naa) ==0) then
                                np_refi=np_refi+1
                                V_Vec(np_refi)=molcrys%mol(i)%u_iso(naa)
                                V_Name(np_refi)=trim(code_nam(4))//trim(molcrys%mol(i)%AtName(naa))
                                molcrys%mol(i)%mu_iso (naa)=1.0
                                molcrys%mol(i)%lu_iso (naa)=np_refi
                                V_Bounds(1,np_refi)=xl
                                V_Bounds(2,np_refi)=xu
                                V_Bounds(3,np_refi)=xs
                                V_BCon(np_refi)=ic
                                V_list(np_refi)=na
                             end if
                          end do
                       end if

                    case (4)
                       !---- BAN ----!
                       if (na <= molcrys%n_free) then
                          do j=1,6
                             if ( atm(na)%l_u(j) ==0) then
                                 atm(na)%m_u(j)=1.0
                                call get_atombet_ctr( atm(na)%x, atm(na)%u,molcrys%Spg, &
                                                     np_refi, atm(na)%l_u, atm(na)%m_u)
                                if ( atm(na)%l_u(j) == np_refi) then
                                   V_Vec(np_refi)= atm(na)%u(j)
                                   V_Name(np_refi)=trim(code_nam(5+j))//trim( atm(na)%lab)
                                   V_Bounds(1,np_refi)=xl
                                   V_Bounds(2,np_refi)=xu
                                   V_Bounds(3,np_refi)=xs
                                   V_BCon(np_refi)=ic
                                   V_list(np_refi)=na
                                else
                                   np_refi=np_refi-1
                                end if
                             end if
                          end do
                       else
                          Err_CFML%Flag=.true.
                          Err_CFML%Msg="Option Not defined"
                          return
                       end if

                    case (5)
                       !---- ALL ----!
                       if (na <= molcrys%n_free) then
                          do j=1,3
                             if ( atm(na)%l_x(j) ==0) then
                                 atm(na)%m_x(j)=1.0
                                call get_atompos_ctr( atm(na)%x,   &
                                                     molcrys%Spg,np_refi, &
                                                      atm(na)%l_x,  &
                                                      atm(na)%m_x)
                                if ( atm(na)%l_x(j) == np_refi) then
                                   V_Vec(np_refi)= atm(na)%x(j)
                                   V_Name(np_refi)=trim(code_nam(j))//trim( atm(na)%lab)
                                   V_Bounds(1,np_refi)=xl
                                   V_Bounds(2,np_refi)=xu
                                   V_Bounds(3,np_refi)=xs
                                   V_BCon(np_refi)=ic
                                   V_list(np_refi)=na
                                else
                                   np_refi=np_refi-1
                                end if
                             end if
                          end do
                       else
                          naa=na-molcrys%n_free
                          do i=1,molcrys%n_mol
                             if (naa > molcrys%mol(i)%natoms) then
                                 naa=naa-molcrys%mol(i)%natoms
                                 cycle
                             end if

                             do j=1,3
                                if (molcrys%mol(i)%lI_coor(j,naa) ==0) then
                                   molcrys%mol(i)%mI_coor(nb,naa)=1.0
                                   call get_atompos_ctr(molcrys%mol(i)%I_Coor(:,naa), &
                                                        molcrys%Spg,np_refi,   &
                                                        molcrys%mol(i)%lI_coor(:,naa), &
                                                        molcrys%mol(i)%mI_coor(:,naa))
                                   if (molcrys%mol(i)%lI_coor(j,naa) == np_refi) then
                                      V_Vec(np_refi)=molcrys%mol(i)%I_Coor(nb,naa)
                                      V_Name(np_refi)=trim(code_nam(j))//trim(molcrys%mol(i)%AtName(naa))
                                      V_Bounds(1,np_refi)=xl
                                      V_Bounds(2,np_refi)=xu
                                      V_Bounds(3,np_refi)=xs
                                      V_BCon(np_refi)=ic
                                      V_list(np_refi)=na
                                   else
                                      np_refi=np_refi-1
                                   end if
                                end if
                             end do
                          end do
                       end if

                       if (na <= molcrys%n_free) then
                          if ( atm(na)%l_occ ==0) then
                             np_refi=np_refi+1
                             V_Vec(np_refi)= atm(na)%occ
                             V_Name(np_refi)=trim(code_nam(5))//trim( atm(na)%lab)
                              atm(na)%m_occ=1.0
                              atm(na)%l_occ=np_refi
                             V_Bounds(1,np_refi)=xl
                             V_Bounds(2,np_refi)=xu
                             V_Bounds(3,np_refi)=xs
                             V_BCon(np_refi)=ic
                             V_list(np_refi)=na
                          end if
                       else
                          naa=na-molcrys%n_free
                          do i=1,molcrys%n_mol
                             if (naa > molcrys%mol(i)%natoms) then
                                naa=naa-molcrys%mol(i)%natoms
                                cycle
                             end if
                             if (molcrys%mol(i)%locc (naa) ==0) then
                                np_refi=np_refi+1
                                V_Vec(np_refi)=molcrys%mol(i)%Occ(naa)
                                V_Name(np_refi)=trim(code_nam(5))//trim(molcrys%mol(i)%AtName(naa))
                                molcrys%mol(i)%mocc (naa)=1.0
                                molcrys%mol(i)%locc (naa)=np_refi
                                V_Bounds(1,np_refi)=xl
                                V_Bounds(2,np_refi)=xu
                                V_Bounds(3,np_refi)=xs
                                V_BCon(np_refi)=ic
                                V_list(np_refi)=na
                             end if
                          end do
                       end if

                       if (na <= molcrys%n_free) then
                          if ( atm(na)%l_u_iso ==0) then
                             np_refi=np_refi+1
                             V_Vec(np_refi)= atm(na)%u_iso
                             V_Name(np_refi)=trim(code_nam(4))//trim( atm(na)%lab)
                              atm(na)%m_u_iso=1.0
                              atm(na)%l_u_iso=np_refi
                             V_Bounds(1,np_refi)=xl
                             V_Bounds(2,np_refi)=xu
                             V_Bounds(3,np_refi)=xs
                             V_BCon(np_refi)=ic
                             V_list(np_refi)=na
                          end if
                       else
                          naa=na-molcrys%n_free
                          do i=1,molcrys%n_mol
                             if (naa > molcrys%mol(i)%natoms) then
                                naa=naa-molcrys%mol(i)%natoms
                                cycle
                             end if

                             if (molcrys%mol(i)%lu_iso(naa) ==0) then
                                np_refi=np_refi+1
                                V_Vec(np_refi)=molcrys%mol(i)%u_iso(naa)
                                V_Name(np_refi)=trim(code_nam(4))//trim(molcrys%mol(i)%AtName(naa))
                                molcrys%mol(i)%mu_iso (naa)=1.0
                                molcrys%mol(i)%lu_iso (naa)=np_refi
                                V_Bounds(1,np_refi)=xl
                                V_Bounds(2,np_refi)=xu
                                V_Bounds(3,np_refi)=xs
                                V_BCon(np_refi)=ic
                                V_list(np_refi)=na
                             end if
                          end do
                       end if

                       if (na <= molcrys%n_free) then
                          do j=1,6
                             if ( atm(na)%l_u(j) ==0) then
                                 atm(na)%m_u(j)=1.0
                                call get_atombet_ctr( atm(na)%x, atm(na)%u,molcrys%Spg, &
                                                     np_refi, atm(na)%l_u, atm(na)%m_u)
                                if ( atm(na)%l_u(j) == np_refi) then
                                   V_Vec(np_refi)= atm(na)%u(j)
                                   V_Name(np_refi)=trim(code_nam(5+j))//trim( atm(na)%lab)
                                   V_Bounds(1,np_refi)=xl
                                   V_Bounds(2,np_refi)=xu
                                   V_Bounds(3,np_refi)=xs
                                   V_BCon(np_refi)=ic
                                   V_list(np_refi)=na
                                else
                                   np_refi=np_refi-1
                                end if
                             end if
                          end do
                       end if

                       select case (nmol)
                          case (-1)
                             Err_CFML%Flag=.true.
                             Err_CFML%Msg="Option Not defined"
                             return

                          case (0)
                             do i=1,molcrys%n_mol
                                write(unit=car,fmt="(i2)") i
                                car=adjustl(car)
                                do j=1,3
                                   if (molcrys%mol(i)%lxcentre(j) ==0) then
                                      np_refi=np_refi+1
                                      V_Vec(np_refi)=molcrys%mol(i)%xcentre(j)
                                      V_Name(np_refi)=trim(code_nam(12+j))//"entre_Mol"//trim(car)
                                      molcrys%mol(i)%mxcentre(j)=1.0
                                      molcrys%mol(i)%lxcentre(j)=np_refi
                                      V_Bounds(1,np_refi)=xl
                                      V_Bounds(2,np_refi)=xu
                                      V_Bounds(3,np_refi)=xs
                                      V_BCon(np_refi)=ic
                                      V_list(np_refi)=-i
                                   end if
                                end do
                             end do

                          case (1:)
                             write(unit=car,fmt="(i2)") nmol
                             car=adjustl(car)
                             do j=1,3
                                if (molcrys%mol(nmol)%lxcentre(j) ==0) then
                                   np_refi=np_refi+1
                                   V_Vec(np_refi)=molcrys%mol(nmol)%xcentre(j)
                                   V_Name(np_refi)=trim(code_nam(12+j))//"entre_Mol"//trim(car)
                                   molcrys%mol(nmol)%mxcentre(j)=1.0
                                   molcrys%mol(nmol)%lxcentre(j)=np_refi
                                   V_Bounds(1,np_refi)=xl
                                   V_Bounds(2,np_refi)=xu
                                   V_Bounds(3,np_refi)=xs
                                   V_BCon(np_refi)=ic
                                   V_list(np_refi)=-nmol
                                end if
                             end do
                       end select

                       select case (nmol)
                          case (-1)
                             Err_CFML%Flag=.true.
                             Err_CFML%Msg="Option Not defined"
                             return

                          case (0)
                             do i=1,molcrys%n_mol
                                write(unit=car,fmt="(i2)") i
                                car=adjustl(car)
                                do j=1,3
                                   if (molcrys%mol(i)%lOrient(j) ==0) then
                                      np_refi=np_refi+1
                                      V_Vec(np_refi)=molcrys%mol(i)%Orient(j)
                                      V_Name(np_refi)=trim(code_nam(15+j))//"Orient_Mol"//trim(car)
                                      molcrys%mol(i)%mOrient(j)=1.0
                                      molcrys%mol(i)%lOrient(j)=np_refi
                                      V_Bounds(1,np_refi)=xl
                                      V_Bounds(2,np_refi)=xu
                                      V_Bounds(3,np_refi)=xs
                                      V_BCon(np_refi)=ic
                                      V_list(np_refi)=-i
                                   end if
                                end do
                             end do

                          case (1:)
                             write(unit=car,fmt="(i2)") nmol
                             car=adjustl(car)
                             do j=1,3
                                if (molcrys%mol(nmol)%lOrient(j) ==0) then
                                   np_refi=np_refi+1
                                   V_Vec(np_refi)=molcrys%mol(nmol)%Orient(j)
                                   V_Name(np_refi)=trim(code_nam(15+j))//"Orient_Mol"//trim(car)
                                   molcrys%mol(nmol)%mOrient(j)=1.0
                                   molcrys%mol(nmol)%lOrient(j)=np_refi
                                   V_Bounds(1,np_refi)=xl
                                   V_Bounds(2,np_refi)=xu
                                   V_Bounds(3,np_refi)=xs
                                   V_BCon(np_refi)=ic
                                   V_list(np_refi)=-nmol
                                end if
                             end do
                       end select

                    case (6)
                       !---- CEN ----!
                       select case (nmol)
                          case (-1)
                             Err_CFML%Flag=.true.
                             Err_CFML%Msg="Option Not defined"
                             return

                          case (0)
                             do i=1,molcrys%n_mol
                                write(unit=car,fmt="(i2)") i
                                car=adjustl(car)
                                do j=1,3
                                   if (molcrys%mol(i)%lxcentre(j) ==0) then
                                      np_refi=np_refi+1
                                      V_Vec(np_refi)=molcrys%mol(i)%xcentre(j)
                                      V_Name(np_refi)=trim(code_nam(12+j))//"entre_Mol"//trim(car)
                                      molcrys%mol(i)%mxcentre(j)=1.0
                                      molcrys%mol(i)%lxcentre(j)=np_refi
                                      V_Bounds(1,np_refi)=xl
                                      V_Bounds(2,np_refi)=xu
                                      V_Bounds(3,np_refi)=xs
                                      V_BCon(np_refi)=ic
                                      V_list(np_refi)=-i
                                   end if
                                end do
                             end do

                          case (1:)
                             write(unit=car,fmt="(i2)") nmol
                             car=adjustl(car)
                             do j=1,3
                                if (molcrys%mol(nmol)%lxcentre(j) ==0) then
                                   np_refi=np_refi+1
                                   V_Vec(np_refi)=molcrys%mol(nmol)%xcentre(j)
                                   V_Name(np_refi)=trim(code_nam(12+j))//"entre_Mol"//trim(car)
                                   molcrys%mol(nmol)%mxcentre(j)=1.0
                                   molcrys%mol(nmol)%lxcentre(j)=np_refi
                                   V_Bounds(1,np_refi)=xl
                                   V_Bounds(2,np_refi)=xu
                                   V_Bounds(3,np_refi)=xs
                                   V_BCon(np_refi)=ic
                                   V_list(np_refi)=-nmol
                                end if
                             end do
                       end select

                    case (7)
                       !---- ORI  ----!
                       select case (nmol)
                          case (-1)
                             Err_CFML%Flag=.true.
                             Err_CFML%Msg="Option Not defined"
                             return

                          case (0)
                             do i=1,molcrys%n_mol
                                write(unit=car,fmt="(i2)") i
                                car=adjustl(car)
                                do j=1,3
                                   if (molcrys%mol(i)%lOrient(j) ==0) then
                                      np_refi=np_refi+1
                                      V_Vec(np_refi)=molcrys%mol(i)%Orient(j)
                                      V_Name(np_refi)=trim(code_nam(15+j))//"Orient_Mol"//trim(car)
                                      molcrys%mol(i)%mOrient(j)=1.0
                                      molcrys%mol(i)%lOrient(j)=np_refi
                                      V_Bounds(1,np_refi)=xl
                                      V_Bounds(2,np_refi)=xu
                                      V_Bounds(3,np_refi)=xs
                                      V_BCon(np_refi)=ic
                                      V_list(np_refi)=-i
                                   end if
                                end do
                             end do

                          case (1:)
                             write(unit=car,fmt="(i2)") nmol
                             car=adjustl(car)
                             do j=1,3
                                if (molcrys%mol(nmol)%lOrient(j) ==0) then
                                   np_refi=np_refi+1
                                   V_Vec(np_refi)=molcrys%mol(nmol)%Orient(j)
                                   V_Name(np_refi)=trim(code_nam(15+j))//"Orient_Mol"//trim(car)
                                   molcrys%mol(nmol)%mOrient(j)=1.0
                                   molcrys%mol(nmol)%lOrient(j)=np_refi
                                   V_Bounds(1,np_refi)=xl
                                   V_Bounds(2,np_refi)=xu
                                   V_Bounds(3,np_refi)=xs
                                   V_BCon(np_refi)=ic
                                   V_list(np_refi)=-nmol
                                end if
                             end do
                       end select

                    case (8)
                       !!! Not yet implemented !!!

                 end select
           end select
       end select
    End Subroutine Fill_RefCodes_Molcrys

    !!--++
    !!--++ Module Subroutine Fill_RefCodes_Molec(Keyv,Dire,Na,Nb,Xl,Xu,Xs,Ic,Molec,Spg)
    !!--++    integer,                      intent(in)     :: Keyv
    !!--++    character(len=*),             intent(in)     :: Dire
    !!--++    integer,                      intent(in)     :: Na
    !!--++    integer,                      intent(in)     :: Nb
    !!--++    real(kind=cp),                intent(in)     :: Xl
    !!--++    real(kind=cp),                intent(in)     :: Xu
    !!--++    real(kind=cp),                intent(in)     :: Xs
    !!--++    integer,                      intent(in)     :: Ic
    !!--++    type(molecule_type),          intent(in out) :: Molec
    !!--++    type(SPG_Type),       intent(in)     :: Spg
    !!--++
    !!--++ Overloaded
    !!--++ Write on Vectors the Information for Molecule_Type
    !!--++
    !!--++ Update: March - 2005
    !!
    Module Subroutine Fill_RefCodes_Molec(Keyv,Dire,Na,Nb,Xl,Xu,Xs,Ic,Molec,Spg)
       !---- Arguments ----!
       integer,                      intent(in)     :: Keyv
       character(len=*),             intent(in)     :: Dire
       integer,                      intent(in)     :: Na
       integer,                      intent(in)     :: Nb
       real(kind=cp),                intent(in)     :: Xl
       real(kind=cp),                intent(in)     :: Xu
       real(kind=cp),                intent(in)     :: Xs
       integer,                      intent(in)     :: Ic
       type(molecule_type),          intent(in out) :: Molec
       type(SPG_Type),               intent(in)     :: Spg

       !---- Local variables ----!
       integer :: j, nc

       call clear_error()
       if (Na <= 0) then
          Err_CFML%Flag=.true.
          Err_CFML%Msg="The number of atom no defined"
          return
       end if

       select case (dire)
          !---- FIX Directive ----!
          case ("fix")

             select case (keyv)
                case (0)

                   !---- nb must be different zero ----!
                   select case (nb)
                      case (0)
                         Err_CFML%Flag=.true.
                         Err_CFML%Msg="Option not defined"
                         return

                      case ( 1:3)
                         !---- X_, Y_, Z_----!
                         if (molec%lI_coor(nb,na) /=0) then
                            nc=molec%lI_coor(nb,na)
                            call Delete_RefCodes(nc,molec)
                         end if

                      case ( 4)
                         !---- Biso_ ----!
                         if (molec%lu_iso(na) /=0) then
                            nc=molec%lu_iso(na)
                            call Delete_RefCodes(nc,molec)
                         end if

                      case ( 5)
                         !---- Occ_ ----!
                         if (molec%locc(na) /=0) then
                            nc=molec%locc(na)
                            call Delete_RefCodes(nc,molec)
                         end if

                      case ( 6:12)
                         !---- B11_, ..., B23_ ----!
                         Err_CFML%Flag=.true.
                         Err_CFML%Msg="Option not defined"
                         return

                      case (13:15)
                         !---- Xc_, Yc_, Zc_ ----!
                         if (molec%lxcentre(nb-12) /=0) then
                            nc=molec%lxcentre(nb-12)
                            call Delete_RefCodes(nc,molec)
                         end if

                      case (16:18)
                         !---- Theta_, Phi_, Chi_ ----!
                         if (molec%lOrient(nb-15) /=0) then
                            nc=molec%lOrient(nb-15)
                            call Delete_RefCodes(nc,molec)
                         end if

                      case (19:21)
                         !!! Not yet implement !!!

                   end select ! nb

                case (1)
                   !---- XYZ ----!
                   do j=1,3
                      if (molec%lI_coor(j,na) /=0) then
                         nc=molec%lI_coor(j,na)
                         call Delete_RefCodes(nc,molec)
                      end if
                   end do

                case (2)
                   !---- OCC ----!
                   if (molec%locc(na) /=0) then
                      nc=molec%locc(na)
                      call Delete_RefCodes(nc,molec)
                   end if

                case (3)
                   !---- BIS ----!
                   if (molec%lu_iso(na) /=0) then
                      nc=molec%lu_iso(na)
                      call Delete_RefCodes(nc,molec)
                   end if

                case (4)
                   !---- BAN ----!
                   Err_CFML%Flag=.true.
                   Err_CFML%Msg="Option not defined"
                   return

                case (5)
                  !---- ALL ----!
                  do j=1,3
                     if (molec%lI_coor(j,na) /=0) then
                        nc=molec%lI_coor(j,na)
                        call Delete_RefCodes(nc,molec)
                     end if
                  end do
                  if (molec%locc(na) /=0) then
                     nc=molec%locc(na)
                     call Delete_RefCodes(nc,molec)
                  end if
                  if (molec%lu_iso(na) /=0) then
                     nc=molec%lu_iso(na)
                     call Delete_RefCodes(nc,molec)
                  end if
                  do j=1,3
                     if (molec%lxcentre(j) /=0) then
                        nc=molec%lxcentre(j)
                        call Delete_RefCodes(nc,molec)
                     end if
                  end do
                  do j=1,3
                     if (molec%lOrient(j) /=0) then
                        nc=molec%lOrient(j)
                        call Delete_RefCodes(nc,molec)
                     end if
                  end do
                  !!! Falta THermal TLS !!!

               case (6)
                  !---- CEN ----!
                  do j=1,3
                     if (molec%lxcentre(j) /=0) then
                        nc=molec%lxcentre(j)
                        call Delete_RefCodes(nc,molec)
                     end if
                  end do

               case (7)
                  !---- ORI ----!
                  do j=1,3
                     if (molec%lOrient(j) /=0) then
                        nc=molec%lOrient(j)
                        call Delete_RefCodes(nc,molec)
                     end if
                  end do

               case (8)
                  !---- THE ----!
                  !!! Not Yet Implemented !!!
             end select

          !---- VARY Directive ----!
          case ("var")

             select case (keyv)
                case (0)

                   !---- Nb must be different zero ----!
                   select case (nb)
                      case (0)
                         Err_CFML%Flag=.true.
                         Err_CFML%Msg="Option not defined"
                         return

                      case ( 1:3)
                         !--- X_, Y_, Z_ ----!
                         if (molec%lI_coor(nb,na) ==0) then
                            molec%mI_coor(nb,na)=1.0
                            call get_atompos_ctr(molec%I_Coor(:,na),  &
                                                 Spg, np_refi,  &
                                                 molec%lI_coor(:,na), &
                                                 molec%mI_coor(:,na))
                            if (molec%lI_coor(nb,na) == np_refi) then
                               V_Vec(np_refi)=molec%I_Coor(nb,na)
                               V_Name(np_refi)=trim(code_nam(nb))//trim(molec%AtName(na))
                               V_Bounds(1,np_refi)=xl
                               V_Bounds(2,np_refi)=xu
                               V_Bounds(3,np_refi)=xs
                               V_BCon(np_refi)=ic
                               V_list(np_refi)=na
                            else
                               np_refi=np_refi-1
                            end if
                         end if

                      case ( 4)
                         !---- Biso_ ----!
                         if (molec%lu_iso(na) ==0) then
                            np_refi=np_refi+1
                            V_Vec(np_refi)=molec%u_iso(na)
                            V_Name(np_refi)=trim(code_nam(nb))//trim(molec%AtName(na))
                            molec%mu_iso(na)=1.0
                            molec%lu_iso(na)=np_refi
                            V_Bounds(1,np_refi)=xl
                            V_Bounds(2,np_refi)=xu
                            V_Bounds(3,np_refi)=xs
                            V_BCon(np_refi)=ic
                            V_list(np_refi)=na
                         end if

                      case ( 5)
                         !---- Occ_ ----!
                         if (molec%locc(na) ==0) then
                            np_refi=np_refi+1
                            V_Vec(np_refi)=molec%occ(na)
                            V_Name(np_refi)=trim(code_nam(nb))//trim(molec%AtName(na))
                            molec%mocc(na)=1.0
                            molec%locc(na)=np_refi
                            V_Bounds(1,np_refi)=xl
                            V_Bounds(2,np_refi)=xu
                            V_Bounds(3,np_refi)=xs
                            V_BCon(np_refi)=ic
                            V_list(np_refi)=na
                         end if

                      case ( 6:12)
                         !---- B11_, ..., B23_ ----!
                         Err_CFML%Flag=.true.
                         Err_CFML%Msg="Option not defined"
                         return

                      case (13:15)
                         !---- Xc_, Yc_, Zc_ ----!
                         if (molec%lxcentre(nb-12) ==0) then
                            np_refi=np_refi+1
                            V_Vec(np_refi)=molec%xcentre(nb-12)
                            V_Name(np_refi)=trim(code_nam(nb))//"Mol"
                            molec%mxcentre(nb-12)=1.0
                            molec%lxcentre(nb-12)=np_refi
                            V_Bounds(1,np_refi)=xl
                            V_Bounds(2,np_refi)=xu
                            V_Bounds(3,np_refi)=xs
                            V_BCon(np_refi)=ic
                            V_list(np_refi)=0
                         end if

                      case (16:18)
                         !---- Theta_, Phi_, Chi_ ----!
                         if (molec%lOrient(nb-15) ==0) then
                            np_refi=np_refi+1
                            V_Vec(np_refi)=molec%orient(nb-15)
                            V_Name(np_refi)=trim(code_nam(nb))//"Mol"
                            molec%mOrient(nb-15)=1.0
                            molec%lOrient(nb-15)=np_refi
                            V_Bounds(1,np_refi)=xl
                            V_Bounds(2,np_refi)=xu
                            V_Bounds(3,np_refi)=xs
                            V_BCon(np_refi)=0
                            V_list(np_refi)=0
                         end if

                      case (19:21)
                         !!! Not yet implement !!!

                   end select ! nb

                case (1)
                   !---- XYZ ----!
                   do j=1,3
                      if (molec%lI_coor(j,na) ==0) then
                         molec%mI_coor(j,na)=1.0
                         call get_atompos_ctr(molec%I_Coor(:,na),  &
                                              Spg, np_refi,  &
                                              molec%lI_coor(:,na), &
                                              molec%mI_coor(:,na))
                         if (molec%lI_coor(j,na) == np_refi) then
                            V_Vec(np_refi)=molec%I_Coor(j,na)
                            V_Name(np_refi)=trim(code_nam(j))//trim(molec%AtName(na))
                            V_Bounds(1,np_refi)=xl
                            V_Bounds(2,np_refi)=xu
                            V_Bounds(3,np_refi)=xs
                            V_BCon(np_refi)=ic
                            V_list(np_refi)=na
                         else
                            np_refi=np_refi-1
                         end if
                      end if
                   end do

                case (2)
                   !---- OCC ----!
                   if (molec%locc(na) ==0) then
                      np_refi=np_refi+1
                      V_Vec(np_refi)=molec%occ(na)
                      V_Name(np_refi)=trim(code_nam(5))//trim(molec%AtName(na))
                      molec%mocc(na)=1.0
                      molec%locc(na)=np_refi
                      V_Bounds(1,np_refi)=xl
                      V_Bounds(2,np_refi)=xu
                      V_Bounds(3,np_refi)=xs
                      V_BCon(np_refi)=ic
                      V_list(np_refi)=na
                   end if

                case (3)
                   !---- BIS ----!
                   if (molec%lu_iso(na) ==0) then
                      np_refi=np_refi+1
                      V_Vec(np_refi)=molec%u_iso(na)
                      V_Name(np_refi)=trim(code_nam(4))//trim(molec%AtName(na))
                      molec%mu_iso(na)=1.0
                      molec%lu_iso(na)=np_refi
                      V_Bounds(1,np_refi)=xl
                      V_Bounds(2,np_refi)=xu
                      V_Bounds(3,np_refi)=xs
                      V_BCon(np_refi)=ic
                      V_list(np_refi)=na
                   end if

                case (4)
                   !---- BAN ----!
                   Err_CFML%Flag=.true.
                   Err_CFML%Msg="Option not defined"
                   return

                case (5)
                   !---- ALL ----!
                   do j=1,3
                      if (molec%lI_coor(j,na) ==0) then
                         molec%mI_coor(j,na)=1.0
                         call get_atompos_ctr(molec%I_Coor(:,na),  &
                                              Spg, np_refi,  &
                                              molec%lI_coor(:,na), &
                                              molec%mI_coor(:,na))
                         if (molec%lI_coor(j,na) == np_refi) then
                            V_Vec(np_refi)=molec%I_Coor(j,na)
                            V_Name(np_refi)=trim(code_nam(j))//trim(molec%AtName(na))
                            V_Bounds(1,np_refi)=xl
                            V_Bounds(2,np_refi)=xu
                            V_Bounds(3,np_refi)=xs
                            V_BCon(np_refi)=ic
                            V_list(np_refi)=na
                         else
                            np_refi=np_refi-1
                         end if
                      end if
                   end do
                   if (molec%locc(na) ==0) then
                      np_refi=np_refi+1
                      V_Vec(np_refi)=molec%occ(na)
                      V_Name(np_refi)=trim(code_nam(5))//trim(molec%AtName(na))
                      molec%mocc(na)=1.0
                      molec%locc(na)=np_refi
                      V_Bounds(1,np_refi)=xl
                      V_Bounds(2,np_refi)=xu
                      V_Bounds(3,np_refi)=xs
                      V_BCon(np_refi)=ic
                      V_list(np_refi)=na
                   end if
                   if (molec%lu_iso(na) ==0) then
                      np_refi=np_refi+1
                      V_Vec(np_refi)=molec%u_iso(na)
                      V_Name(np_refi)=trim(code_nam(4))//trim(molec%AtName(na))
                      molec%mu_iso(na)=1.0
                      molec%lu_iso(na)=np_refi
                      V_Bounds(1,np_refi)=xl
                      V_Bounds(2,np_refi)=xu
                      V_Bounds(3,np_refi)=xs
                      V_BCon(np_refi)=ic
                      V_list(np_refi)=na
                   end if
                   do j=1,3
                      if (molec%lxcentre(j) ==0) then
                         np_refi=np_refi+1
                         V_Vec(np_refi)=molec%xcentre(j)
                         V_name(np_refi)=trim(code_nam(12+j))//"entre_Mol"
                         molec%mxcentre(j)=1.0
                         molec%lxcentre(j)=np_refi
                         V_Bounds(1,np_refi)=xl
                         V_Bounds(2,np_refi)=xu
                         V_Bounds(3,np_refi)=xs
                         V_BCon(np_refi)=ic
                         V_list(np_refi)=0
                      end if
                   end do
                   do j=1,3
                      if (molec%lOrient(j) ==0) then
                         np_refi=np_refi+1
                         V_Vec(np_refi)=molec%orient(j)
                         V_name(np_refi)=trim(code_nam(15+j))//"Orient_Mol"
                         molec%mOrient(j)=1.0
                         molec%lOrient(j)=np_refi
                         V_Bounds(1,np_refi)=xl
                         V_Bounds(2,np_refi)=xu
                         V_Bounds(3,np_refi)=xs
                         V_BCon(np_refi)=0
                         V_list(np_refi)=0
                      end if
                   end do

                   !!! Falta THE !!!

                case (6)
                   !---- CEN ----!
                   do j=1,3
                      if (molec%lxcentre(j) ==0) then
                         np_refi=np_refi+1
                         V_Vec(np_refi)=molec%xcentre(j)
                         V_name(np_refi)=trim(code_nam(12+j))//"Mol"
                         molec%mxcentre(j)=1.0
                         molec%lxcentre(j)=np_refi
                         V_Bounds(1,np_refi)=xl
                         V_Bounds(2,np_refi)=xu
                         V_Bounds(3,np_refi)=xs
                         V_BCon(np_refi)=ic
                         V_list(np_refi)=0
                      end if
                   end do

                case (7)
                   !---- ORI ----!
                   do j=1,3
                      if (molec%lOrient(j) ==0) then
                         np_refi=np_refi+1
                         V_Vec(np_refi)=molec%orient(j)
                         V_name(np_refi)=trim(code_nam(15+j))//"Mol"
                         molec%mOrient(j)=1.0
                         molec%lOrient(j)=np_refi
                         V_Bounds(1,np_refi)=xl
                         V_Bounds(2,np_refi)=xu
                         V_Bounds(3,np_refi)=xs
                         V_BCon(np_refi)=ic
                         V_list(np_refi)=0
                      end if
                   end do

                case (8)
                   !---- THE ----!

                   !!! Not yet implemented !!!
             end select
       end select
    End Subroutine Fill_RefCodes_Molec

End Submodule KWC_FillCodes_MolX
