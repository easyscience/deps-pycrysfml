Submodule (CFML_Keywords_Code_Parser) KWC_FillCodes_FAtm
   !---- Variables ----!
   implicit none

 Contains
    !!--++
    !!--++ Module Subroutine Fill_RefCodes(Keyv,Dire,Na,Nb,Xl,Xu,Xs,Ic,FAtom/MolCrys/Molec,Spg)
    !!--++    integer,                       intent(in)     :: Keyv
    !!--++    character(len=*),              intent(in)     :: Dire
    !!--++    integer,                       intent(in)     :: Na
    !!--++    integer,                       intent(in)     :: Nb
    !!--++    real(kind=cp),                 intent(in)     :: Xl
    !!--++    real(kind=cp),                 intent(in)     :: Xu
    !!--++    real(kind=cp),                 intent(in)     :: Xs
    !!--++    integer,                       intent(in)     :: Ic
    !!--++    type(AtList_Type),             intent(in out) :: FAtom
    !!--++    or
    !!--++    type(MolCrystal_Type),  intent(in out) :: MolCrys
    !!--++    or
    !!--++    type(molecule_type),           intent(in out) :: Molec
    !!--++    type(SPG_Type),        intent(in)     :: Spg
    !!--++
    !!--++    (Private)
    !!--++    Write on Vectors the Information for Free Atoms
    !!--++
    !!--++ Update: March - 2005
    !!

    !!--++
    !!--++ Module Subroutine Fill_RefCodes_FAtom(Keyv,Dire,Na,Nb,Xl,Xu,Xs,Ic,FAtom,Spg)
    !!--++    integer,                       intent(in)     :: Keyv  !0=> nb as below,
    !!--++    character(len=*),              intent(in)     :: Dire !Var of Fix
    !!--++    integer,                       intent(in)     :: Na   !Number of the atom in the asymmetric unit
    !!--++    integer,                       intent(in)     :: Nb   !Type of individual parameter x=1,y=2,z=3,biso=4, etc
    !!--++    real(kind=cp),                 intent(in)     :: Xl   !Lower bound of parameter
    !!--++    real(kind=cp),                 intent(in)     :: Xu   !Upper bound of parameter
    !!--++    real(kind=cp),                 intent(in)     :: Xs   !Step of parameter
    !!--++    integer,                       intent(in)     :: Ic   !Boundary condition (0:fixed or 1:periodic)
    !!--++    type(AtList_Type),             intent(in out) :: FAtom
    !!--++    type(SPG_Type),                intent(in)     :: Spg
    !!--++
    !!--++ Overloaded
    !!--++ Write on Vectors the Information for Free Atoms
    !!--++  Keyv=0 -> Provide information on individual parameter for atom na (nb should be given)
    !!--++  Keyv=1  XYZ -> fix or vary all coordinates of atom na
    !!--++  Keyv=2  OCC -> fix or vary occupation factor of atom na
    !!--++  Keyv=3  BIS -> fix or vary isotropic temperature factor of atom na
    !!--++  Keyv=4  BAN -> fix or vary anisotropic temperature factors of atom na
    !!--++  Keyv=5  ALL -> fix or vary all parameters of atom na
    !!--++
    !!--++  nb=1:3   X_ Y_ Z_
    !!--++  nb=4     Biso_
    !!--++  nb=5     Occ_
    !!--++  nb=6:11  B11_  B22_  B33_  B12_  B13_  B23_
    !!--++  nb=12    Bns_ (all Bij ...)
    !!--++
    !!--++ Update: March - 2005
    !!
    Module Subroutine Fill_RefCodes_FAtom(Keyv,Dire,Na,Nb,Xl,Xu,Xs,Ic,FAtom,Spg)
       !---- Arguments ----!
       integer,                       intent(in)     :: Keyv
       character(len=*),              intent(in)     :: Dire
       integer,                       intent(in)     :: Na
       integer,                       intent(in)     :: Nb
       real(kind=cp),                 intent(in)     :: Xl
       real(kind=cp),                 intent(in)     :: Xu
       real(kind=cp),                 intent(in)     :: Xs
       integer,                       intent(in)     :: Ic
       type(AtList_Type),             intent(in out) :: FAtom
       type(SPG_Type),                intent(in)     :: Spg

       !---- Local variables ----!
       integer           :: j,nc,np_ini

       call clear_error()
       if (Na <= 0) then
          Err_CFML%Flag=.true.
          Err_CFML%Msg="The number of atom no defined"
          return
       end if
       Select Type (atom => FAtom%atom)
         Type is (atm_ref_type)
         select case (dire)
            !---- FIX Directive ----!
            case ("fix")

               select case (Keyv)
                  case (0) !Keyv=0 -> Provide information on individual parameter for atom na (nb should be given)
                     !---- nb must be different zero ----!
                     select case (nb)
                        case (0)
                           Err_CFML%Flag=.true.
                           Err_CFML%Msg="Option not defined"
                           return

                        case ( 1:3) ! Keyv=0, nb=1 to 3. Fix particular coordinate x(nb) for atom na
                           !---- X_, Y_, Z_ ----!
                           if (atom(na)%l_x(nb) /=0) then
                              nc= atom(na)%l_x(nb)
                              call Delete_RefCodes(nc,FAtom)
                           end if

                        case ( 4:6) ! Keyv=0, nb=4 to 6. Fix particular component moment(nb-3) for atom na
                           !---- Mx_, My_, Mz_ ----!
                           if (atom(na)%l_moment(nb-3) /=0) then
                              nc= atom(na)%l_moment(nb-3)
                              call Delete_RefCodes(nc,FAtom)
                           end if

                        case ( 7) ! Keyv=0, nb=7. Fix Biso for atom na
                           !---- Biso_ ----!
                           if ( atom(na)%l_u_iso /=0) then
                              nc= atom(na)%l_u_iso
                              call Delete_RefCodes(nc,fatom)
                           end if

                        case ( 8) ! Keyv=0, nb=5. Fix Occ for atom na
                           !---- Occ_ ----!
                           if ( atom(na)%l_occ /=0) then
                              nc= atom(na)%l_occ
                              call Delete_RefCodes(nc,fatom)
                           end if

                        case ( 9:14) ! Keyv=0, nb=6 to 11. Fix particular Baniso(nb) for atom na
                           !---- B11_,...,B23_ ----!
                           if ( atom(na)%l_u(nb-8) /=0) then
                              nc= atom(na)%l_u(nb-8)
                              call Delete_RefCodes(nc,fatom)
                           end if

                        case (15) ! Keyv=0, nb=12. Fix all B for atom na
                           !---- Banis_ or Bns_ ----!
                           do j=1,6
                              if ( atom(na)%l_u(j) /=0) then
                                 nc= atom(na)%l_u(j)
                                 call Delete_RefCodes(nc,fatom)
                              end if
                           end do

                        case (16:)
                           Err_CFML%Flag=.true.
                           Err_CFML%Msg="Option not defined for this type of variable "
                           return
                     end select ! nb

                  case (1) ! Keyv=1  XYZ -> Fix all coordinates of atom na
                     !---- XYZ ----!
                     do j=1,3
                        if ( atom(na)%l_x(j) /=0) then
                           nc= atom(na)%l_x(j)
                           call Delete_RefCodes(nc,fatom)
                        end if
                     end do

                  case (2) !  Keyv=2  OCC -> Fix occupation factor of atom na
                     !---- OCC ----!
                     if ( atom(na)%l_occ /=0) then
                        nc= atom(na)%l_occ
                        call Delete_RefCodes(nc,fatom)
                     end if

                  case (3) !  Keyv=3  BIS -> Fix isotropic temperature factor of atom na
                     !---- BIS ----!
                     if ( atom(na)%l_u_iso /=0) then
                        nc= atom(na)%l_u_iso
                        call Delete_RefCodes(nc,fatom)
                     end if

                  case (4) !  Keyv=4  BAN -> Fix anisotropic temperature factors of atom na
                    !---- BAN ----!
                    do j=1,6
                       if ( atom(na)%l_u(j) /=0) then
                          nc= atom(na)%l_u(j)
                          call Delete_RefCodes(nc,fatom)
                       end if
                    end do

                  case (5) !  Keyv=5  ALL -> Fix all parameters of atom na
                     !---- ALL ----!
                     do j=1,3
                        if ( atom(na)%l_x(j) /=0) then
                           nc= atom(na)%l_x(j)
                           call Delete_RefCodes(nc,fatom)
                        end if
                     end do
                     if ( atom(na)%l_occ /=0) then
                        nc= atom(na)%l_occ
                        call Delete_RefCodes(nc,fatom)
                     end if
                     if ( atom(na)%l_u_iso /=0) then
                        nc= atom(na)%l_u_iso
                        call Delete_RefCodes(nc,fatom)
                     end if
                     do j=1,6
                        if ( atom(na)%l_u(j) /=0) then
                           nc= atom(na)%l_u(j)
                           call Delete_RefCodes(nc,fatom)
                        end if
                     end do

                  case (6:)
                     Err_CFML%Flag=.true.
                     Err_CFML%Msg="Incompatible information for this type of variable "
                     return
               end select

            !---- VARY Directive ----!
            case ("var")

               select case (Keyv)
                  case (0)

                     !---- nb must be different zero ----!
                     select case (nb)
                        case (0)
                           Err_CFML%Flag=.true.
                           Err_CFML%Msg="Option not defined"
                           return

                        case ( 1:3) ! Keyv=0, nb=1 to 3. Vary particular coordinate x(nb) for atom na
                           !---- X_, Y_, Z_ ----!
                           if ( atom(na)%l_x(nb) ==0) then
                              atom(na)%m_x(nb)=1.0
                              call get_atompos_ctr( atom(na)%x, Spg, np_refi,   &
                                                    atom(na)%l_x,  atom(na)%m_x)
                              if ( atom(na)%l_x(nb) == np_refi) then
                                 V_Vec(np_refi)= atom(na)%x(nb)
                                 V_Name(np_refi)=trim(code_nam(nb))//trim( atom(na)%lab)
                                 V_Bounds(1,np_refi)=xl
                                 V_Bounds(2,np_refi)=xu
                                 V_Bounds(3,np_refi)=xs
                                 V_BCon(np_refi)=ic
                                 V_list(np_refi)=na
                              else
                                 np_refi=np_refi-1
                              end if
                           end if

                         case ( 4:6) ! Keyv=0, nb=4 to 6. Vary particular component moment(nb-3) for atom na
                            !---- Mx_, My_, Mz_ ----!
                            if ( atom(na)%l_moment(nb-3) == 0) then
                               atom(na)%m_moment(nb-3)=1.0
                               call get_moment_ctr_Wigner( atom(na)%x, atom(na)%moment, Spg, np_refi,   &
                                                     atom(na)%l_moment,atom(na)%m_moment)
                               if ( atom(na)%l_moment(nb-3) == np_refi) then
                                  V_Vec(np_refi)= atom(na)%moment(nb-3)
                                  V_Name(np_refi)=trim(code_nam(nb))//trim( atom(na)%lab)
                                  V_Bounds(1,np_refi)=xl
                                  V_Bounds(2,np_refi)=xu
                                  V_Bounds(3,np_refi)=xs
                                  V_BCon(np_refi)=ic
                                  V_list(np_refi)=na
                               else
                                  np_refi=np_refi-1
                               end if
                            end if

                        case ( 7) ! Keyv=0, nb=4. Vary Biso for atom na
                           !---- Biso_ ----!
                           if ( atom(na)%l_u_iso ==0) then
                              np_refi=np_refi+1
                              V_Vec(np_refi)= atom(na)%u_iso
                              V_Name(np_refi)=trim(code_nam(nb))//trim( atom(na)%lab)
                               atom(na)%m_u_iso=1.0
                               atom(na)%l_u_iso=np_refi
                              V_Bounds(1,np_refi)=xl
                              V_Bounds(2,np_refi)=xu
                              V_Bounds(3,np_refi)=xs
                              V_BCon(np_refi)=ic
                              V_list(np_refi)=na
                           end if

                        case ( 8)  ! Keyv=0, nb=5. Vary Occ for atom na
                           !---- Occ_ ----!
                           if ( atom(na)%l_occ ==0) then
                              np_refi=np_refi+1
                              V_Vec(np_refi)= atom(na)%occ
                              V_Name(np_refi)=trim(code_nam(nb))//trim( atom(na)%lab)
                               atom(na)%m_occ=1.0
                               atom(na)%l_occ=np_refi
                              V_Bounds(1,np_refi)=xl
                              V_Bounds(2,np_refi)=xu
                              V_Bounds(3,np_refi)=xs
                              V_BCon(np_refi)=ic
                              V_list(np_refi)=na
                           end if

                        case ( 9:14) ! Keyv=0, nb=6 to 11. Vary particular Baniso(nb) for atom na
                           !---- B11_,...,B23_ ----!
                           if ( atom(na)%l_u(nb-5) ==0) then
                               atom(na)%m_u(nb-5)=1.0
                              call get_atombet_ctr( atom(na)%x, atom(na)%u,Spg, &
                                                   np_refi, atom(na)%l_u, atom(na)%m_u)
                              if ( atom(na)%l_u(nb-5) == np_refi) then
                                 V_Vec(np_refi)= atom(na)%u(nb-5)
                                 V_Name(np_refi)=trim(code_nam(nb))//trim( atom(na)%lab)
                                 V_Bounds(1,np_refi)=xl
                                 V_Bounds(2,np_refi)=xu
                                 V_Bounds(3,np_refi)=xs
                                 V_BCon(np_refi)=ic
                                 V_list(np_refi)=na
                              else
                                 np_refi=np_refi-1
                              end if
                           end if

                        case (15)  ! Keyv=0, nb=12. Vary all Baniso for atom na
                           !---- Banis_ ----!
                           np_ini=np_refi
                            atom(na)%m_u(:)=1.0
                           call get_atombet_ctr( atom(na)%x, atom(na)%u,Spg, &
                                                np_refi, atom(na)%l_u, atom(na)%m_u)
                           !write(*,"(a,2i4,3x,6i4)")"BANIS: np_ini,np_refi,LU",np_ini,np_refi, atom(na)%l_u
                           np_refi=np_ini
                           do j=1,6
                              if ( atom(na)%l_u(j) > np_refi) then
                                 np_refi=np_refi+1
                                  atom(na)%l_u(j)=np_refi
                                  V_Vec(np_refi)= atom(na)%u(j)
                                  V_Name(np_refi)=trim(code_nam(5+j))//trim( atom(na)%lab)
                                  V_Bounds(1,np_refi)=xl
                                  V_Bounds(2,np_refi)=xu
                                  V_Bounds(3,np_refi)=xs
                                  V_BCon(np_refi)=ic
                                  V_list(np_refi)=na
                              end if
                           end do

                        case (16:)
                           Err_CFML%Flag=.true.
                           Err_CFML%Msg="Option Not defined by this type of variables"
                           return

                     end select ! nb

                  case (1)  ! Keyv=1  XYZ -> Vary all coordinates of atom na
                     !---- XYZ ----!
                     do j=1,3
                        if ( atom(na)%l_x(j) == 0) then
                            atom(na)%m_x(j)=1.0
                            call get_atompos_ctr( atom(na)%x, Spg, np_refi,   &
                                                  atom(na)%l_x, atom(na)%m_x)!, Ipr=6)
                           if ( atom(na)%l_x(j) == np_refi) then
                              V_Vec(np_refi)= atom(na)%x(j)
                              V_Name(np_refi)=trim(code_nam(j))//trim( atom(na)%lab)
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

                  case (2) !  Keyv=2  OCC -> Vary occupation factor of atom na
                     !---- OCC ----!
                     if ( atom(na)%l_occ ==0) then
                        np_refi=np_refi+1
                        V_Vec(np_refi)= atom(na)%occ
                        V_Name(np_refi)=trim(code_nam(5))//trim( atom(na)%lab)
                         atom(na)%m_occ=1.0
                         atom(na)%l_occ=np_refi
                        V_Bounds(1,np_refi)=xl
                        V_Bounds(2,np_refi)=xu
                        V_Bounds(3,np_refi)=xs
                        V_BCon(np_refi)=ic
                        V_list(np_refi)=na
                     end if

                  case (3) !  Keyv=3  BIS -> Vary isotropic temperature factor of atom na
                     !---- BIS ----!
                     if ( atom(na)%l_u_iso ==0) then
                        np_refi=np_refi+1
                        V_Vec(np_refi)= atom(na)%u_iso
                        V_Name(np_refi)=trim(code_nam(4))//trim( atom(na)%lab)
                         atom(na)%m_u_iso=1.0
                         atom(na)%l_u_iso=np_refi
                        V_Bounds(1,np_refi)=xl
                        V_Bounds(2,np_refi)=xu
                        V_Bounds(3,np_refi)=xs
                        V_BCon(np_refi)=ic
                        V_list(np_refi)=na
                     end if

                  case (4) !  Keyv=4  BAN -> Vary all anisotropic temperature factors of atom na
                     !---- BAN ----!
                     np_ini=np_refi
                      atom(na)%m_u(:)=1.0
                     call get_atombet_ctr( atom(na)%x, atom(na)%u,Spg, &
                                          np_refi, atom(na)%l_u, atom(na)%m_u)
                     np_refi=np_ini
                     do j=1,6
                        if ( atom(na)%l_u(j) > np_refi) then
                           np_refi=np_refi+1
                            atom(na)%l_u(j)=np_refi
                              V_Vec(np_refi)= atom(na)%u(j)
                              V_Name(np_refi)=trim(code_nam(5+j))//trim( atom(na)%lab)
                              V_Bounds(1,np_refi)=xl
                              V_Bounds(2,np_refi)=xu
                              V_Bounds(3,np_refi)=xs
                              V_BCon(np_refi)=ic
                              V_list(np_refi)=na
                        end if
                     end do

                  case (5) !  Keyv=5  ALL -> Vary all parameters of atom na
                     !---- ALL ----!
                     do j=1,3
                        if ( atom(na)%l_x(j) ==0) then
                            atom(na)%m_x(j)=1.0
                           call get_atompos_ctr( atom(na)%x, Spg, np_refi,   &
                                                 atom(na)%l_x,  atom(na)%m_x)
                           if ( atom(na)%l_x(j) == np_refi) then
                              V_Vec(np_refi)= atom(na)%x(j)
                              V_Name(np_refi)=trim(code_nam(j))//trim( atom(na)%lab)
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
                     if ( atom(na)%l_occ ==0) then
                        np_refi=np_refi+1
                        V_Vec(np_refi)= atom(na)%occ
                        V_Name(np_refi)=trim(code_nam(5))//trim( atom(na)%lab)
                         atom(na)%m_occ=1.0
                         atom(na)%l_occ=np_refi
                        V_Bounds(1,np_refi)=xl
                        V_Bounds(2,np_refi)=xu
                        V_Bounds(3,np_refi)=xs
                        V_BCon(np_refi)=ic
                        V_list(np_refi)=na
                     end if
                     if ( atom(na)%l_u_iso ==0) then
                        np_refi=np_refi+1
                        V_Vec(np_refi)= atom(na)%u_iso
                        V_Name(np_refi)=trim(code_nam(4))//trim( atom(na)%lab)
                         atom(na)%m_u_iso=1.0
                         atom(na)%l_u_iso=np_refi
                        V_Bounds(1,np_refi)=xl
                        V_Bounds(2,np_refi)=xu
                        V_Bounds(3,np_refi)=xs
                        V_BCon(np_refi)=ic
                        V_list(np_refi)=na
                     end if
                     np_ini=np_refi
                      atom(na)%m_u(:)=1.0
                     call get_atombet_ctr( atom(na)%x, atom(na)%u,Spg, &
                                          np_refi, atom(na)%l_u, atom(na)%m_u)
                     np_refi=np_ini
                     do j=1,6
                        if ( atom(na)%l_u(j) > np_refi) then
                           np_refi=np_refi+1
                            atom(na)%l_u(j)=np_refi
                              V_Vec(np_refi)= atom(na)%u(j)
                              V_Name(np_refi)=trim(code_nam(5+j))//trim( atom(na)%lab)
                              V_Bounds(1,np_refi)=xl
                              V_Bounds(2,np_refi)=xu
                              V_Bounds(3,np_refi)=xs
                              V_BCon(np_refi)=ic
                              V_list(np_refi)=na
                        end if
                     end do


                  case(6:)
                     Err_CFML%Flag=.true.
                     Err_CFML%Msg="Option Not defined by this type of variables"
                     return
               end select
            end select
        end select
    End Subroutine Fill_RefCodes_FAtom

    !!--++
    !!--++ Module Subroutine Fill_RefCodes_FmAtom(Keyv,Dire,Na,Nb,Xl,Xu,Xs,Ic,ik,FmAtom)
    !!--++    integer,                       intent(in)     :: Keyv
    !!--++    character(len=*),              intent(in)     :: Dire
    !!--++    integer,                       intent(in)     :: Na
    !!--++    integer,                       intent(in)     :: Nb
    !!--++    real(kind=cp),                 intent(in)     :: Xl
    !!--++    real(kind=cp),                 intent(in)     :: Xu
    !!--++    real(kind=cp),                 intent(in)     :: Xs
    !!--++    integer,                       intent(in)     :: Ic
    !!--++    integer,                       intent(in)     :: ik
    !!--++    type(mAtom_List_Type),         intent(in out) :: FmAtom
    !!--++
    !!--++ Write on Vectors the Information for Free Magnetic Atoms
    !!--++ magnetic clone of Fill_RefCodes_FAtom
    !!--++ Created: December - 2011
    !!--++ Updated: February - 2012
    !!--++
    Module Subroutine Fill_RefCodes_FmAtom(Keyv,Dire,Na,Nb,Xl,Xu,Xs,Ic,ik,FmAtom)
       !---- Arguments ----!
       integer,                       intent(in)     :: Keyv
       character(len=*),              intent(in)     :: Dire
       integer,                       intent(in)     :: Na
       integer,                       intent(in)     :: Nb
       real(kind=cp),                 intent(in)     :: Xl
       real(kind=cp),                 intent(in)     :: Xu
       real(kind=cp),                 intent(in)     :: Xs
       integer,                       intent(in)     :: Ic
       integer,                       intent(in)     :: ik
       type(mAtom_List_Type),         intent(in out) :: FmAtom

       !---- Local variables ----!
       integer           :: j,nc

       call clear_error()
       if (Na <= 0) then
          Err_CFML%Flag=.true.
          Err_CFML%Msg="The number of atom no defined"
          return
       end if

       !---- Get im, number of the magnetic matrices/irrep set
       !ik=FmAtom%atom(na)%nvk !This is wrong, ik should be provided in the argument of the subroutine
                               !in order to select the corresponding Fourier coefficient
       select case (dire)
          !---- FIX Directive ----!
          case ("fix")

             select case (Keyv)
                case (0)
                   !---- nb must be different zero ----!
                   select case (nb)
                      case (0)
                         Err_CFML%Flag=.true.
                         Err_CFML%Msg="Option not defined"
                         return

                      case ( 1:3) ! Keyv=0, nb=1 to 3, particular coordinate x(nb) for atom na
                         !---- Rx_, Ry_, Rz_ ----!
                         if (FmAtom%atom(na)%lSkR(nb,ik) /=0) then
                            nc=FmAtom%atom(na)%lSkR(nb,ik)
                            call Delete_RefCodes(nc,FmAtom)
                         end if

                      case ( 4:6)
                         !---- Ix_, Iy_, Iz_ ----!
                         if (FmAtom%atom(na)%lSkI(nb-3,ik) /=0) then
                            nc=FmAtom%atom(na)%lSkI(nb-3,ik)
                            call Delete_RefCodes(nc,FmAtom)
                         end if

                      case ( 7:9)
                         !---- Rm_, Rphi_, Rth_ ----!
                         if (FmAtom%atom(na)%lSkR(nb-6,ik) /=0) then
                            nc=FmAtom%atom(na)%lSkR(nb-6,ik)
                            call Delete_RefCodes(nc,FmAtom)
                         end if

                      case (10:12)
                         !---- Im_, Iphi_, Ith_ ----!
                         if (FmAtom%atom(na)%lSkI(nb-9,ik) /=0) then
                            nc=FmAtom%atom(na)%lSkI(nb-9,ik)
                            call Delete_RefCodes(nc,FmAtom)
                         end if

                      case (13)
                         !---- MagPh_ ----!
                            if (FmAtom%atom(na)%lmphas(ik) /=0) then
                               nc=FmAtom%atom(na)%lmphas(ik)
                               call Delete_RefCodes(nc,FmAtom)
                            end if

                      case (14:25)
                         !---- C1_,..C12_ ----!
                            if (FmAtom%atom(na)%lbas(nb-13,ik) /=0) then
                               nc=FmAtom%atom(na)%lbas(nb-13,ik)
                               call Delete_RefCodes(nc,FmAtom)
                            end if

                      case (26:)
                         Err_CFML%Flag=.true.
                         Err_CFML%Msg="Option not defined for this type of variable "
                         return
                   end select ! nb

                case (1)
                   !---- Rxyz ----!
                   do j=1,3
                      if (FmAtom%atom(na)%lSkR(j,ik) /=0) then
                         nc=FmAtom%atom(na)%lSkR(j,ik)
                         call Delete_RefCodes(nc,FmAtom)
                      end if
                   end do

                case (2)
                   !---- Ixyz ----!
                   do j=1,3
                      if (FmAtom%atom(na)%lSkI(j,ik) /=0) then
                         nc=FmAtom%atom(na)%lSkI(j,ik)
                         call Delete_RefCodes(nc,FmAtom)
                      end if
                   end do

                case (3)
                   !---- Mxyz ----!
                   do j=1,3
                      if (FmAtom%atom(na)%lSkR(j,ik) /=0) then
                         nc=FmAtom%atom(na)%lSkR(j,ik)
                         call Delete_RefCodes(nc,FmAtom)
                      end if
                   end do
                   do j=1,3
                      if (FmAtom%atom(na)%lSkI(j,ik) /=0) then
                         nc=FmAtom%atom(na)%lSkI(j,ik)
                         call Delete_RefCodes(nc,FmAtom)
                      end if
                   end do

                case (4:)
                   Err_CFML%Flag=.true.
                   Err_CFML%Msg="Incompatible information for this type of variable "
                   return
             end select

          !---- VARY Directive ----!
          case ("var")

              select case (Keyv)
                case (0)

                   !---- nb must be different zero ----!
                   select case (nb)
                      case (0)
                         Err_CFML%Flag=.true.
                         Err_CFML%Msg="Option not defined"
                         return

                      case ( 1:3)
                         !---- Rx_, Ry_, Rz_ ----!
                         if (FmAtom%atom(na)%lSkR(nb,ik) ==0) then

                            np_refi=np_refi+1
                            FmAtom%atom(na)%lSkR(nb,ik)=np_refi
                            FmAtom%atom(na)%mSkR(nb,ik)=1.0

                            if (FmAtom%atom(na)%lSkR(nb,ik) == np_refi) then
                               V_Vec(np_refi)=FmAtom%atom(na)%SkR(nb,ik)
                               V_Name(np_refi)=trim(mcode_nam(nb))//trim(FmAtom%atom(na)%lab)
                               V_Bounds(1,np_refi)=xl
                               V_Bounds(2,np_refi)=xu
                               V_Bounds(3,np_refi)=xs
                               V_BCon(np_refi)=ic
                               V_list(np_refi)=na
                            else
                               np_refi=np_refi-1
                            end if
                         end if

                      case ( 4:6)
                         !---- Ix_, Iy_, Iz_ ----!
                         if (FmAtom%atom(na)%lSkI(nb-3,ik) ==0) then

                            np_refi=np_refi+1
                            FmAtom%atom(na)%lSkI(nb-3,ik)=np_refi
                            FmAtom%atom(na)%mSkI(nb-3,ik)=1.0

                            if (FmAtom%atom(na)%lSkI(nb-3,ik) == np_refi) then
                               V_Vec(np_refi)=FmAtom%atom(na)%SkI(nb-3,ik)
                               V_Name(np_refi)=trim(mcode_nam(nb))//trim(FmAtom%atom(na)%lab)
                               V_Bounds(1,np_refi)=xl
                               V_Bounds(2,np_refi)=xu
                               V_Bounds(3,np_refi)=xs
                               V_BCon(np_refi)=ic
                               V_list(np_refi)=na
                             else
                               np_refi=np_refi-1
                            end if
                         end if

                      case ( 7:9)
                         !---- Rm_, Rphi_, Rth_ ----!
                         if (FmAtom%atom(na)%lSkR(nb-6,ik) ==0) then

                            np_refi=np_refi+1
                            FmAtom%atom(na)%lSkR(nb-6,ik)=np_refi
                            FmAtom%atom(na)%mSkR(nb-6,ik)=1.0

                            if (FmAtom%atom(na)%lSkR(nb-6,ik) == np_refi) then
                               V_Vec(np_refi)=FmAtom%atom(na)%Spher_SkR(nb-6,ik)
                               V_Name(np_refi)=trim(mcode_nam(nb))//trim(FmAtom%atom(na)%lab)
                               V_Bounds(1,np_refi)=xl
                               V_Bounds(2,np_refi)=xu
                               V_Bounds(3,np_refi)=xs
                               V_BCon(np_refi)=ic
                               V_list(np_refi)=na
                            else
                               np_refi=np_refi-1
                            end if
                         end if

                      case (10:12)
                         !---- Im_, Iphi_, Ith_ ----!
                         if (FmAtom%atom(na)%lSkI(nb-9,ik) ==0) then

                            np_refi=np_refi+1
                            FmAtom%atom(na)%lSkI(nb-9,ik)=np_refi
                            FmAtom%atom(na)%mSkI(nb-9,ik)=1.0

                            if (FmAtom%atom(na)%lSkI(nb-9,ik) == np_refi) then
                               V_Vec(np_refi)=FmAtom%atom(na)%Spher_SkI(nb-9,ik)
                               V_Name(np_refi)=trim(mcode_nam(nb))//trim(FmAtom%atom(na)%lab)
                               V_Bounds(1,np_refi)=xl
                               V_Bounds(2,np_refi)=xu
                               V_Bounds(3,np_refi)=xs
                               V_BCon(np_refi)=ic
                               V_list(np_refi)=na
                             else
                               np_refi=np_refi-1
                            end if
                         end if

                      case (13)
                         !---- MagPh_ ----!
                         if (FmAtom%atom(na)%lmphas(ik) ==0) then

                            np_refi=np_refi+1
                            FmAtom%atom(na)%mmphas(ik)=1.0
                            FmAtom%atom(na)%lmphas(ik)=np_refi

                            V_Vec(np_refi)=FmAtom%atom(na)%mphas(ik)
                            V_Name(np_refi)=trim(mcode_nam(nb))//trim(FmAtom%atom(na)%lab)
                            V_Bounds(1,np_refi)=xl
                            V_Bounds(2,np_refi)=xu
                            V_Bounds(3,np_refi)=xs
                            V_BCon(np_refi)=ic
                            V_list(np_refi)=na
                         end if

                      case (14:25)
                         !---- C1_,..C12_ ----!
                         if (FmAtom%atom(na)%lbas(nb-13,ik) ==0) then

                            np_refi=np_refi+1
                            FmAtom%atom(na)%lbas(nb-13,ik)=np_refi
                            FmAtom%atom(na)%mbas(nb-13,ik)=1.0

                            if (FmAtom%atom(na)%lbas(nb-13,ik) == np_refi) then
                               V_Vec(np_refi)=FmAtom%atom(na)%cbas(nb-13,ik)
                               V_Name(np_refi)=trim(mcode_nam(nb))//trim(FmAtom%atom(na)%lab)
                               V_Bounds(1,np_refi)=xl
                               V_Bounds(2,np_refi)=xu
                               V_Bounds(3,np_refi)=xs
                               V_BCon(np_refi)=ic
                               V_list(np_refi)=na
                             else
                               np_refi=np_refi-1
                            end if
                         end if

                      case (26:)
                         Err_CFML%Flag=.true.
                         Err_CFML%Msg="Option Not defined by this type of variables"
                         return

                   end select ! nb

                case (1)
                   !---- Rxyz ----!
                   do j=1,3
                      if (FmAtom%atom(na)%lSkR(j,ik) ==0) then

                            np_refi=np_refi+1
                            FmAtom%atom(na)%lSkR(j,ik)=np_refi
                            FmAtom%atom(na)%mSkR(j,ik)=1.0

                         if (FmAtom%atom(na)%lSkR(j,ik) == np_refi) then
                            V_Vec(np_refi)=FmAtom%atom(na)%SkR(j,ik)
                            V_Name(np_refi)=trim(mcode_nam(j))//trim(FmAtom%atom(na)%lab)
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
                   !---- Ixyz ----!
                   do j=1,3
                      if (FmAtom%atom(na)%lSkI(j,ik) ==0) then

                            np_refi=np_refi+1
                            FmAtom%atom(na)%lSkI(j,ik)=np_refi
                            FmAtom%atom(na)%mSkI(j,ik)=1.0

                         if (FmAtom%atom(na)%lSkI(j,ik) == np_refi) then
                            V_Vec(np_refi)=FmAtom%atom(na)%SkI(j,ik)
                            V_Name(np_refi)=trim(mcode_nam(j+3))//trim(FmAtom%atom(na)%lab)
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

                case (3)
                    !---- Mxyz ----!
                   do j=1,3
                      if (FmAtom%atom(na)%lSkR(j,ik) ==0) then

                            np_refi=np_refi+1
                            FmAtom%atom(na)%lSkR(j,ik)=np_refi
                            FmAtom%atom(na)%mSkR(j,ik)=1.0

                         if (FmAtom%atom(na)%lSkR(j,ik) == np_refi) then
                            V_Vec(np_refi)=FmAtom%atom(na)%SkR(j,ik)
                            V_Name(np_refi)=trim(mcode_nam(j))//trim(FmAtom%atom(na)%lab)
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

                   do j=1,3
                      if (FmAtom%atom(na)%lSkI(j,ik) ==0) then

                            np_refi=np_refi+1
                            FmAtom%atom(na)%lSkI(j,ik)=np_refi
                            FmAtom%atom(na)%mSkI(j,ik)=1.0

                         if (FmAtom%atom(na)%lSkI(j,ik) == np_refi) then
                            V_Vec(np_refi)=FmAtom%atom(na)%SkI(j,ik)
                            V_Name(np_refi)=trim(mcode_nam(j+3))//trim(FmAtom%atom(na)%lab)
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

                case (4:)
                   Err_CFML%Flag=.true.
                   Err_CFML%Msg="Option Not defined by this type of variables"
                   return
             end select
       end select
    End Subroutine Fill_RefCodes_FmAtom

End Submodule KWC_FillCodes_FAtm
