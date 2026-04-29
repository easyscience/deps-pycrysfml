Submodule (CFML_Keywords_Code_Parser) KWC_GetConCodes
   !---- Variables ----!
   implicit none

 Contains
    !!--++
    !!--++ Module Subroutine Get_ConCodes_Line(Line,FAtom/FmAtom/MolCrys/Molec/MagDom)
    !!--++    character(len=*),             intent(in)     :: Line
    !!--++    integer,                      intent(in)     :: Nat
    !!--++    type(AtList_Type),            intent(in out) :: FAtom
    !!--++    or
    !!--++    type(mAtom_List_Type),        intent(in out) :: FmAtom
    !!--++    or
    !!--++    type(MolCrystal_Type), intent(in out) :: MolCrys
    !!--++    or
    !!--++    type(molecule_type),          intent(in out) :: Molec
    !!--++    or
    !!--++    type(Magnetic_Domain_type),   intent(in out) :: Mag_Dom
    !!--++
    !!--++    (Private)
    !!--++    Get the Constraints relations
    !!--++
    !!--++ Update: March - 2005
    !!

    !!--++
    !!--++ Module Subroutine Get_ConCodes_Line_FAtom(Line,FAtom)
    !!--++    character(len=*),         intent(in)     :: Line
    !!--++    integer,                  intent(in)     :: Nat
    !!--++    type(AtList_Type),        intent(in out) :: FAtom
    !!--++
    !!--++ Overloaded
    !!--++ Get the Constraints relations
    !!--++
    !!--++ Update: March - 2005
    !!
    Module Subroutine Get_ConCodes_Line_FAtom(Line,FAtom)
       !---- Arguments ----!
       character(len=*),     intent(in)     :: Line
       type(AtList_Type),    intent(in out) :: FAtom

       !---- Local variables ----!
       character(len=20), dimension(30) :: label
       integer                          :: j,ic,n,na,nb,nc,nd,npos
       integer                          :: nl,nl2,iv
       integer, dimension(1)            :: ivet
       real(kind=cp)                    :: fac_0,fac_1
       real(kind=cp),dimension(1)       :: vet

       call clear_error()

       nl=0
       call Get_Words(line,label,ic)
       if (ic < 2) then
          Err_CFML%Flag=.true.
          Err_CFML%Msg="EQUAL keyword needs two labels: "//trim(line)
          return
       end if

       !---- Set Futher Information----!
       !---- Na is the number of atom on List
       !---- Nb is the Keyv (X,Y,Z,Occ,...)
       !---- Fac0 is the multiplier
       !---- Nl is the number of refinement parameter

       npos=index(label(1),"_")
       if (npos == 0) then
          Err_CFML%Flag=.true.
          Err_CFML%Msg="The name "//trim(label(1))//" does not fit any known code-name of CrysFML "
          return
       end if

       na=0
       do j=1,FAtom%Natoms
          if (u_case(FAtom%atom(j)%lab) == u_case(label(1)(npos+1:npos+6))) then
             na=j
             exit
          end if
       end do
       if (na == 0) then
          Err_CFML%Flag=.true.
          Err_CFML%Msg=" Atom label not found for "//trim(line)
          return
       end if

       nb=0
       do j=1,ncode
          if (u_case(label(1)(1:npos))==u_case(trim(code_nam(j)))) then
             nb=j
             exit
          end if
       end do
       if (nb == 0) then
          Err_CFML%Flag=.true.
          Err_CFML%Msg="Code-name not found for parameter name: "//trim(label(1))
          return
       end if
       atm_Type: Select Type (atom => FAtom%atom)
         Type is (atm_ref_type)
          select case (nb)
             case ( 1:3)
                fac_0= atom(na)%m_x(nb)
                   nl= atom(na)%l_x(nb)
             case ( 4:6)
                fac_0= atom(na)%m_moment(nb-3)
                   nl= atom(na)%l_moment(nb-3)
             case ( 7)
                fac_0= atom(na)%m_u_iso
                   nl= atom(na)%l_u_iso
             case ( 8)
                fac_0= atom(na)%m_occ
                   nl= atom(na)%l_occ
             case ( 9:14)
                fac_0= atom(na)%m_u(nb-8)
                   nl= atom(na)%l_u(nb-8)
             case (15)
                fac_0= atom(na)%m_u(1)
             case (16:)
                Err_CFML%Flag=.true.
                Err_CFML%Msg="Incompatible Code-name for parameter name: "//trim(label(1))
                return
          end select ! nb

          if (nb < ncode) then
             if (nl == 0) then
                Err_CFML%Flag=.true.
                Err_CFML%Msg="No refinable parameter was selected for "//trim(label(1))
                return
             end if
          end if

          !---- Set the rest elements in Constraints ----!
          n=1
          do
             n=n+1
             if (n > ic) exit

             npos=index(label(n),"_")
             if (npos ==0) then
                Err_CFML%Flag=.true.
                Err_CFML%Msg="No CrysFML code-name was found for "//trim(label(n))
                return
             end if

             nc=0
             do j=1,FAtom%Natoms
                if (u_case( atom(j)%lab) == u_case(label(n)(npos+1:npos+6))) then
                   nc=j
                   exit
                end if
             end do
             if (nc == 0) then
                Err_CFML%Flag=.true.
                Err_CFML%Msg=" Atom label not found for "//trim(label(n))
                return
             end if

             nd=0
             do j=1,ncode
                if (u_case(label(n)(1:npos))==u_case(trim(code_nam(j)))) then
                   nd=j
                   exit
                end if
             end do
             if (nd == 0) then
                Err_CFML%Flag=.true.
                Err_CFML%Msg="Code-name not found for "//trim(label(n))
                return
             end if

             !---- Is there a new multiplier?
             n=n+1
             call Get_Num(label(n),vet,ivet,iv)
             if (iv == 1) then
                fac_1=vet(1)
             else
                fac_1=fac_0
                n=n-1
             end if

             select case (nd)
                case ( 1:3)
                   nl2= atom(nc)%l_x(nd)
                   call Delete_refCodes(nl2,FAtom)
                   atom(nc)%m_x(nd)=fac_1
                   atom(nc)%l_x(nd)=nl
                case ( 4:6)
                   nl2= atom(nc)%l_moment(nd-3)
                   call Delete_refCodes(nl2,FAtom)
                   atom(nc)%m_moment(nd-3)=fac_1
                   atom(nc)%l_moment(nd-3)=nl
                case ( 7)
                   nl2= atom(nc)%l_u_iso
                   call Delete_refCodes(nl2,FAtom)
                   atom(nc)%m_u_iso=fac_1
                   atom(nc)%l_u_iso=nl
                case ( 8)
                   nl2= atom(nc)%l_occ
                   call Delete_refCodes(nl2,FAtom)
                   atom(nc)%m_occ=fac_1
                   atom(nc)%l_occ=nl
                case ( 9:14)
                   nl2= atom(nc)%l_u(nd-8)
                   call Delete_refCodes(nl2,FAtom)
                   atom(nc)%m_u(nd-8)=fac_1
                   atom(nc)%l_u(nd-8)=nl
                case (15)
                   do j=1,6
                      nl2= atom(nc)%l_u(j)
                      call Delete_refCodes(nl2,FAtom)
                      atom(nc)%m_u(j)=fac_1
                      atom(nc)%l_u(j)= atom(na)%l_u(j)
                   end do
                   np_cons=np_cons+5
                case (16:)
                   Err_CFML%Flag=.true.
                   Err_CFML%Msg="Incompatible Code-name for parameter name: "//trim(label(1))
                   return
             end select ! nb

             np_cons=np_cons+1

          end do
       End Select atm_Type

    End Subroutine Get_ConCodes_Line_FAtom

    !!--++
    !!--++ Module Subroutine Get_ConCodes_Line_FmAtom(Line,ik,FmAtom)
    !!--++    character(len=*),         intent(in)     :: Line
    !!--++    integer,                  intent(in)     :: ik
    !!--++    type(mAtom_List_Type),    intent(in out) :: FmAtom
    !!--++
    !!--++ Get the Magnetic Constraints Relations: Presently only 'equal'
    !!--++ mag clone of Get_ConCodes_Line_FAtom.
    !!--++ Created: December - 2011, updated March 2020.
    !!--++
    !!
    Module Subroutine Get_ConCodes_Line_FmAtom(Line,ik,FmAtom)
       !---- Arguments ----!
       character(len=*),     intent(in)     :: Line
       integer,              intent(in)     :: ik
       type(mAtom_List_Type),intent(in out) :: FmAtom

       !---- Local variables ----!
       character(len=len(Line))         :: Linem
       character(len=20), dimension(30) :: label
       integer                          :: j,ic,n,na,nb,nc,nd,npos
       integer                          :: nl,nl2,iv
       integer, dimension(1)            :: ivet
       real(kind=cp)                    :: fac_0,fac_1
       real(kind=cp),dimension(1)       :: vet

       call clear_error()
       Linem=line
       nl=0

       call Get_Words(line,label,ic)
       if (ic < 2) then
          Err_CFML%Flag=.true.
          Err_CFML%Msg="EQUAL keyword needs two labels: "//trim(line)
          return
       end if

       !---- Set Futher Information----!
       !---- Na is the number of atom on List
       !---- Nb is the number of keys (Rx,Ry,Rz,Ix,Iy,Iz,MagPh...)
       !---- Fac0 is the multiplier
       !---- Nl is the number of refinement parameter

       npos=index(label(1),"_")
       if (npos ==0) then
          Err_CFML%Flag=.true.
          Err_CFML%Msg="The name "//trim(label(1))//" does not fit any known code-name of CrysFML "
          return
       end if

       na=0
       do j=1,FmAtom%Natoms
          if (u_case(FmAtom%atom(j)%lab) == u_case(label(1)(npos+1:npos+6))) then
             na=j
             exit
          end if
       end do
       if (na == 0) then
          Err_CFML%Flag=.true.
          Err_CFML%Msg=" Atom label not found for "//trim(line)
          return
       end if

       nb=0
       do j=1,mNcode
          if (u_case(label(1)(1:npos))==u_case(trim(mcode_nam(j)))) then
             nb=j
             exit
          end if
       end do
       if (nb == 0) then
          Err_CFML%Flag=.true.
          Err_CFML%Msg="Code-name not found for parameter name: "//trim(label(1))
          return
       end if


       select case (nb)
          case ( 1:3)
             fac_0=FmAtom%atom(na)%mSkR(nb,ik)
                nl=FmAtom%atom(na)%lSkR(nb,ik)
          case ( 4:6)
             fac_0=FmAtom%atom(na)%mSkI(nb-3,ik)
                nl=FmAtom%atom(na)%lSkI(nb-3,ik)
          case ( 7:9)
             fac_0=FmAtom%atom(na)%mSkR(nb-6,ik)
                nl=FmAtom%atom(na)%lSkR(nb-6,ik)
          case (10:12)
             fac_0=FmAtom%atom(na)%mSkI(nb-9,ik)
                nl=FmAtom%atom(na)%lSkI(nb-9,ik)
          case (13)
             fac_0=FmAtom%atom(na)%mmphas(ik)
                nl=FmAtom%atom(na)%lmphas(ik)
          case (14:25)
             fac_0=FmAtom%atom(na)%mbas(nb-13,ik)
                nl=FmAtom%atom(na)%lbas(nb-13,ik)
         case (26:)
             Err_CFML%Flag=.true.
             Err_CFML%Msg="Incompatible Code-name for parameter name: "//trim(label(1))
             return
       end select ! nb

       if (nb < mNcode) then
          if (nl == 0) then
             Err_CFML%Flag=.true.
             Err_CFML%Msg="No refinable parameter was selected for "//trim(label(1))
             return
          end if
       end if

       !---- Set the rest elements in Constraints ----!
       n=1
       do
          n=n+1
          if (n > ic) exit

          npos=index(label(n),"_")
          if (npos ==0) then
             Err_CFML%Flag=.true.
             Err_CFML%Msg="No CrysFML code-name was found for "//trim(label(n))
             return
          end if

          nc=0
          do j=1,FmAtom%Natoms
             if (u_case(FmAtom%atom(j)%lab) == u_case(label(n)(npos+1:npos+6))) then
                nc=j
                exit
             end if
          end do
          if (nc == 0) then
             Err_CFML%Flag=.true.
             Err_CFML%Msg=" Atom label not found for "//trim(label(n))
             return
          end if

          nd=0
          do j=1,ncode
             if (u_case(label(n)(1:npos))==u_case(trim(mcode_nam(j)))) then
                nd=j
                exit
             end if
          end do
          if (nd == 0) then
             Err_CFML%Flag=.true.
             Err_CFML%Msg="Code-name not found for "//trim(label(n))
             return
          end if

          !---- Is there a new multiplier?
          n=n+1
          call Get_Num(label(n),vet,ivet,iv)
          if (iv == 1) then
             fac_1=vet(1)
          else
             fac_1=fac_0
             n=n-1
          end if

          select case (nd)
             case ( 1:3)
                nl2=FmAtom%atom(nc)%lSkR(nd,ik)
                call Delete_refCodes(nl2,FmAtom)
                FmAtom%atom(nc)%mSkR(nd,ik)=fac_1
                FmAtom%atom(nc)%lSkR(nd,ik)=nl
             case ( 4:6)
                nl2=FmAtom%atom(nc)%lSkI(nd-3,ik)
                call Delete_refCodes(nl2,FmAtom)
                FmAtom%atom(nc)%mSkI(nd-3,ik)=fac_1
                FmAtom%atom(nc)%lSkI(nd-3,ik)=nl
             case ( 7:9)
                nl2=FmAtom%atom(nc)%lSkR(nd-6,ik)
                call Delete_refCodes(nl2,FmAtom)
                FmAtom%atom(nc)%mSkR(nd-6,ik)=fac_1
                FmAtom%atom(nc)%lSkR(nd-6,ik)=nl
             case (10:12)
                nl2=FmAtom%atom(nc)%lSkI(nd-9,ik)
                call Delete_refCodes(nl2,FmAtom)
                FmAtom%atom(nc)%mSkI(nd-9,ik)=fac_1
                FmAtom%atom(nc)%lSkI(nd-9,ik)=nl
             case (13)
                nl2=FmAtom%atom(nc)%lmphas(ik)
                call Delete_refCodes(nl2,FmAtom)
                FmAtom%atom(nc)%mmphas(ik)=fac_1
                FmAtom%atom(nc)%lmphas(ik)=nl
             case (14:25)
                nl2=FmAtom%atom(nc)%lbas(nd-13,ik)
                call Delete_refCodes(nl2,FmAtom)
                FmAtom%atom(nc)%mbas(nd-13,ik)=fac_1
                FmAtom%atom(nc)%lbas(nd-13,ik)=nl
             case (26:)
                Err_CFML%Flag=.true.
                Err_CFML%Msg="Incompatible Code-name for parameter name: "//trim(label(1))
                return
          end select ! nb

          np_cons=np_cons+1

       end do
    End Subroutine Get_ConCodes_Line_FmAtom

    !!--++
    !!--++ Module Subroutine Get_ConCodes_Line_Molcrys(Line,Molcrys)
    !!--++    character(len=*),             intent(in)     :: Line
    !!--++    type(MolCrystal_Type), intent(in out) :: MolCrys
    !!--++
    !!--++ Overloaded
    !!--++ Get the Constraints relations
    !!--++
    !!--++ Update: March - 2005
    !!
    Module Subroutine Get_ConCodes_Line_Molcrys(Line,Molcrys)
       !---- Arguments ----!
       character(len=*),             intent(in)     :: Line
       type(MolCrystal_Type), intent(in out) :: MolCrys

       !---- Loval variables ----!
       character(len=5)                 :: car
       character(len=20), dimension(30) :: label
       integer                          :: i,j,ic,n,na,naa,nb,nc,ncc,nd
       integer                          :: npos, nposm, nmol1,nmol2
       integer                          :: nl,nl2,iv
       integer, dimension(1)            :: ivet
       real(kind=cp)                    :: fac_0,fac_1
       real(kind=cp),dimension(1)       :: vet

       call clear_error()

       call Get_Words(line,label,ic)
       if (ic < 2) then
          Err_CFML%Flag=.true.
          Err_CFML%Msg="EQUAL keyword needs two labels: "//trim(line)
          return
       end if

       !---- Set Father Information ----!
       !---- Na is the number of atom on List
       !---- Nb is the Keyv (X,Y,Z,Occ,...)
       !---- Fac0 is the multiplier
       !---- Nl is the number of refinement parameter
       npos=index(label(1),"_")
       if (npos ==0) then
          Err_CFML%Flag=.true.
          Err_CFML%Msg="The name "//trim(label(1))//" does not fit any known code-name of CrysFML "
          return
       end if
       nposm=index(u_case(label(1)),"MOL")
       if (nposm /= 0) then
          car=adjustl(label(1)(nposm+3:))
          if (car(1:2) == "  ") then
             nmol1=0
          else
             read(unit=car,fmt="(i2)") nmol1
          end if
       else
          nmol1=-1
       end if

       na=0
       do j=1,molcrys%n_free
          if (u_case(molcrys%atm(j)%lab) == u_case(label(1)(npos+1:npos+6))) then
             na=j
             exit
          end if
       end do
       if (na == 0) then
          do i=1,molcrys%n_mol
             do j=1,molcrys%mol(i)%natoms
                if (u_case(molcrys%mol(i)%Atname(j)) == u_case(label(1)(npos+1:npos+6))) then
                   if (j > 1) then
                      na=molcrys%n_free+sum(molcrys%mol(1:j-1)%natoms)+j
                   else
                      na=molcrys%n_free+j
                   end if
                   exit
                end if
             end do
          end do
       end if

       nb=0
       do j=1,ncode
          if (u_case(label(1)(1:npos))==u_case(trim(code_nam(j)))) then
             nb=j
             exit
          end if
       end do
       if (nb == 0) then
          Err_CFML%Flag=.true.
          Err_CFML%Msg="Code-name not found for parameter name: "//trim(label(1))
          return
       end if

       !---- Checking ----!
       if (nb <= 12 .and. na==0) then
          Err_CFML%Flag=.true.
          Err_CFML%Msg="Incompatible option for parameter name: "//trim(label(1))
          return
       end if

       nl=0
       Select Type( atm => molcrys%atm)
        Type is (atm_ref_type)
           select case (nb)
              case ( 1:3)
                 !---- X_, Y_, Z_ ----!
                 if (na <= molcrys%n_free) then
                    fac_0=atm(na)%m_x(nb)
                       nl=atm(na)%l_x(nb)
                 else
                    naa=na-molcrys%n_free
                    do i=1,molcrys%n_mol
                       if (naa > molcrys%mol(i)%natoms) then
                          naa=naa-molcrys%mol(i)%natoms
                          cycle
                       end if
                       fac_0=molcrys%mol(i)%mI_coor(nb,naa)
                          nl=molcrys%mol(i)%lI_coor(nb,naa)
                    end do
                 end if

              case ( 4)
                 !---- Biso_ ----!
                 if (na <= molcrys%n_free) then
                    fac_0=atm(na)%m_u_iso
                       nl=atm(na)%l_u_iso
                 else
                    naa=na-molcrys%n_free
                    do i=1,molcrys%n_mol
                       if (naa > molcrys%mol(i)%natoms) then
                          naa=naa-molcrys%mol(i)%natoms
                          cycle
                       end if
                       fac_0=molcrys%mol(i)%mu_iso(naa)
                          nl=molcrys%mol(i)%lu_iso(naa)
                    end do
                 end if

              case ( 5)
                 !---- Occ_ ----!
                 if (na <= molcrys%n_free) then
                    fac_0=atm(na) %m_occ
                       nl=atm(na) %l_occ
                 else
                    naa=na-molcrys%n_free
                    do i=1,molcrys%n_mol
                       if (naa > molcrys%mol(i)%natoms) then
                          naa=naa-molcrys%mol(i)%natoms
                          cycle
                       end if
                       fac_0=molcrys%mol(i)%mocc(naa)
                          nl=molcrys%mol(i)%locc(naa)
                    end do
                 end if

              case ( 6:11)
                 !---- B11_, ..., B23_ ----!
                 if (na <= molcrys%n_free) then
                    fac_0=atm(na) %m_u(nb-5)
                       nl=atm(na) %l_u(nb-5)
                 else
                    Err_CFML%Flag=.true.
                    Err_CFML%Msg="Invalid Option"
                    return
                 end if

              case (12)
                 !---- Banis_ ----!
                 if (na <= molcrys%n_free) then
                    fac_0=atm(na) %m_u(1)
                 else
                    Err_CFML%Flag=.true.
                    Err_CFML%Msg="Invalid Option"
                    return
                 end if

              case (13:15)
                 !---- Xc_, Yc_, Zc_ ----!
                 select case (nmol1)
                    case (-1)
                       Err_CFML%Flag=.true.
                       Err_CFML%Msg="Invalid Option"
                       return

                    case (0)
                       fac_0=molcrys%mol(1)%mxcentre(nb-12)
                       nl=molcrys%mol(1)%lxcentre(nb-12)

                    case (1:)
                       fac_0=molcrys%mol(nmol1)%mxcentre(nb-12)
                       nl=molcrys%mol(nmol1)%lxcentre(nb-12)
                 end select

              case (16:18)
                 !---- Theta_ , Phi_, Chi_ ----!
                 select case (nmol1)
                    case (-1)
                       Err_CFML%Flag=.true.
                       Err_CFML%Msg="Invalid Option"
                       return

                    case (0)
                       fac_0=molcrys%mol(1)%mOrient(nb-15)
                       nl=molcrys%mol(1)%lOrient(nb-15)

                    case (1)
                       fac_0=molcrys%mol(nmol1)%mOrient(nb-15)
                       nl=molcrys%mol(nmol1)%lOrient(nb-15)
                 end select

              case (19:21)
                 !!! Not yet Implemented !!!
           end select ! nb
       end select !type

       if (nb < ncode) then
          if (nl == 0) then
             Err_CFML%Flag=.true.
             Err_CFML%Msg="No refinable parameter was selected for "//trim(label(1))
             return
          end if
       end if

       !---- Set the rest elements in Constraints ----!
       n=1
       do
          n=n+1
          if (n > ic) exit

          npos=index(label(n),"_")
          if (npos ==0) then
             Err_CFML%Flag=.true.
             Err_CFML%Msg="No CrysFML code-name was found for "//trim(label(n))
             return
          end if
          nposm=index(u_case(label(n)),"MOL")
          if (nposm /= 0) then
             car=adjustl(label(n)(nposm+3:))
             if (car(1:2) == "  ") then
                nmol2=0
             else
                read(unit=car,fmt="(i2)") nmol2
             end if
          else
             nmol2=-1
          end if

          nc=0
          do j=1,molcrys%n_free
             if (u_case(molcrys%atm(j)%lab) == u_case(label(n)(npos+1:npos+6))) then
                nc=j
                exit
             end if
          end do
          if (nc == 0) then
             do i=1,molcrys%n_mol
                do j=1,molcrys%mol(i)%natoms
                   if (u_case(molcrys%mol(i)%Atname(j)) == u_case(label(n)(npos+1:npos+6))) then
                      if (j > 1) then
                         nc=molcrys%n_free+sum(molcrys%mol(1:j-1)%natoms)+j
                      else
                         nc=molcrys%n_free+j
                      end if
                      exit
                   end if
                end do
             end do
          end if

          nd=0
          do j=1,ncode
             if (u_case(label(n)(1:npos))==u_case(trim(code_nam(j)))) then
                nd=j
                exit
             end if
          end do
          if (nd == 0) then
             Err_CFML%Flag=.true.
             Err_CFML%Msg="Code-name not found for "//trim(label(n))
             return
          end if

          !---- Checking ----!
          if (nd <= 12 .and. nc==0) then
             Err_CFML%Flag=.true.
             Err_CFML%Msg="Incompatible option for parameter name: "//trim(label(n))
             return
          end if

          !---- Is there a new multiplier? ----!
          n=n+1
          call Get_Num(label(n),vet,ivet,iv)
          if (iv == 1) then
             fac_1=vet(1)
          else
             fac_1=fac_0
             n=n-1
          end if
        Select Type (Atm => molcrys%atm)
          type is (atm_ref_type)
          select case (nd)
             case ( 1:3)
                !---- X_, Y_, Z_ ----!
                if (nc <= molcrys%n_free) then
                   nl2=atm(nc)%l_x(nd)
                   call Delete_RefCodes(nl2,molcrys)
                   atm(nc)%m_x(nd)=fac_1
                   atm(nc)%l_x(nd)=nl
                else
                   ncc=nc-molcrys%n_free
                   do i=1,molcrys%n_mol
                      if (ncc > molcrys%mol(i)%natoms) then
                         ncc=ncc-molcrys%mol(i)%natoms
                         cycle
                      end if
                      nl2=molcrys%mol(i)%lI_coor(nd,ncc)
                      call Delete_RefCodes(nl2,molcrys)
                      molcrys%mol(i)%mI_coor(nd,ncc)=fac_1
                      molcrys%mol(i)%lI_coor(nd,ncc)=nl
                   end do
                end if

             case ( 4)
                !---- Biso_ ----!
                if (nc <= molcrys%n_free) then
                   nl2=atm(nc)%l_u_iso
                   call Delete_RefCodes(nl2,molcrys)
                   atm(nc)%m_u_iso=fac_1
                   atm(nc)%l_u_iso=nl
                else
                   ncc=nc-molcrys%n_free
                   do i=1,molcrys%n_mol
                      if (ncc > molcrys%mol(i)%natoms) then
                         ncc=ncc-molcrys%mol(i)%natoms
                         cycle
                      end if
                      nl2=molcrys%mol(i)%lu_iso (ncc)
                      call Delete_RefCodes(nl2,molcrys)
                      molcrys%mol(i)%mu_iso (ncc)=fac_1
                      molcrys%mol(i)%lu_iso (ncc)=nl
                   end do
                end if

             case ( 5)
                !---- Occ_ ----!
                if (nc <= molcrys%n_free) then
                   nl2=atm(nc)%l_occ
                   call Delete_RefCodes(nl2,molcrys)
                   atm(nc)%m_occ=fac_1
                   atm(nc)%l_occ=nl
                else
                   ncc=nc-molcrys%n_free
                   do i=1,molcrys%n_mol
                      if (ncc > molcrys%mol(i)%natoms) then
                         ncc=ncc-molcrys%mol(i)%natoms
                         cycle
                      end if
                      nl2=molcrys%mol(i)%locc(ncc)
                      call Delete_RefCodes(nl2,molcrys)
                      molcrys%mol(i)%mocc(ncc)=fac_1
                      molcrys%mol(i)%locc(ncc)=nl
                   end do
                end if

             case ( 6:11)
                !---- B11_, ...., B23_ ----!
                if (nc <= molcrys%n_free) then
                   nl2=atm(nc)%l_u(nd-5)
                   call Delete_RefCodes(nl2,molcrys)
                   atm(nc)%m_u(nd-5)=fac_1
                   atm(nc)%l_u(nd-5)=nl
                else
                   Err_CFML%Flag=.true.
                   Err_CFML%Msg="Invalid Option"
                   return
                end if

             case (12)
                !---- Banis_ ----!
                if (nc <= molcrys%n_free) then
                   do j=1,6
                      nl2=atm(nc)%l_u(j)
                      call Delete_RefCodes(nl2,molcrys)
                      atm(nc)%m_u(j)=fac_1
                      atm(nc)%l_u(j)=atm(na) %l_u(j)
                   end do
                   np_cons=np_cons+5
                else
                   Err_CFML%Flag=.true.
                   Err_CFML%Msg="Invalid Option"
                   return
                end if

             case (13:15)
                select case (nmol2)
                   case (-1)
                      Err_CFML%Flag=.true.
                      Err_CFML%Msg="Invalid Option"
                      return

                   case (0)
                      do i=1,molcrys%n_mol
                         nl2=molcrys%mol(i)%lxcentre(nc-12)
                         call Delete_RefCodes(nl2,molcrys)
                         molcrys%mol(i)%mxcentre(nc-12)=fac_1
                         molcrys%mol(i)%lxcentre(nc-12)=nl
                      end do
                      np_cons=np_cons+(i-1)

                   case (1:)
                      nl2=molcrys%mol(nmol2)%lxcentre(nc-12)
                      call Delete_RefCodes(nl2,molcrys)
                      molcrys%mol(nmol2)%mxcentre(nc-12)=fac_1
                      molcrys%mol(nmol2)%lxcentre(nc-12)=nl
                end select

             case (16:18)
                select case (nmol2)
                   case (-1)
                      Err_CFML%Flag=.true.
                      Err_CFML%Msg="Invalid Option"
                      return

                   case (0)
                      do i=1,molcrys%n_mol
                         nl2=molcrys%mol(i)%lorient(nc-15)
                         call Delete_RefCodes(nl2,molcrys)
                         molcrys%mol(i)%morient(nc-15)=fac_1
                         molcrys%mol(i)%lorient(nc-15)=nl
                      end do
                      np_cons=np_cons+(i-1)

                   case (1:)
                      nl2=molcrys%mol(nmol2)%lorient(nc-15)
                      call Delete_RefCodes(nl2,molcrys)
                      molcrys%mol(nmol2)%morient(nc-15)=fac_1
                      molcrys%mol(nmol2)%lorient(nc-15)=nl
                end select

             case (19:21)
                !!! Not yet implemented !!!

          end select ! nb
         end select !type
         np_cons=np_cons+1
       end do
    End Subroutine Get_ConCodes_Line_Molcrys

    !!--++
    !!--++ Module Subroutine Get_ConCodes_Line_Molec(Line,Molec)
    !!--++    character(len=*),    intent(in)     :: Line
    !!--++    type(molecule_type), intent(in out) :: Molec
    !!--++
    !!--++ Overloaded
    !!--++ Get the Constraints relations
    !!--++
    !!--++ Update: March - 2005
    !!
    Module Subroutine Get_ConCodes_Line_Molec(Line,Molec)
       !---- Arguments ----!
       character(len=*),    intent(in)     :: Line
       type(molecule_type), intent(in out) :: Molec

       !---- Loval variables ----!
       character(len=20), dimension(30) :: label
       integer                          :: j,ic,na,nb,nc,nd,npos!,i,naa,ncc
       integer                          :: n,nl,nl2,iv
       integer, dimension(1)            :: ivet
       real(kind=cp)                    :: fac_0,fac_1
       real(kind=cp),dimension(1)       :: vet

       call clear_error()

       call Get_Words(line,label,ic)
       if (ic < 2) then
          Err_CFML%Flag=.true.
          Err_CFML%Msg="EQUAL keyword needs two labels: "//trim(line)
          return
       end if

       !---- Set Father Information ----!
       !---- Na is the number of atom on List
       !---- Nb is the Keyv (X,Y,Z,Occ,...)
       !---- Fac0 is the multiplier
       !---- Nl is the number of refinement parameter
       npos=index(label(1),"_")
       if (npos ==0) then
          Err_CFML%Flag=.true.
          Err_CFML%Msg="The name "//trim(label(1))//" does not fit any known code-name of CrysFML "
          return
       end if

       na=0
       do j=1,molec%natoms
          if (u_case(molec%Atname(j)) == u_case(label(1)(npos+1:npos+6))) then
             na=j
             exit
          end if
       end do

       nb=0
       do j=1,ncode
          if (u_case(label(1)(1:npos))==u_case(trim(code_nam(j)))) then
             nb=j
             exit
          end if
       end do
       if (nb == 0) then
          Err_CFML%Flag=.true.
          Err_CFML%Msg="Code-name not found for parameter name: "//trim(label(1))
          return
       end if

       !---- Checking ----!
       if (nb < 6 .and. na == 0) then
          Err_CFML%Flag=.true.
          Err_CFML%Msg="Incompatible relation: "//trim(label(1))
          return
       end if

       nl=0
       select case (nb)
          case ( 1:3)
             !---- X_, Y_, Z_ ----!
             fac_0=molec%mI_coor(nb,na)
                nl=molec%lI_coor(nb,na)
          case ( 4)
             !---- Biso_ ----!
             fac_0=molec%mu_iso(na)
                nl=molec%lu_iso(na)
          case ( 5)
             !---- Occ_ ----!
             fac_0=molec%mocc(na)
                nl=molec%locc(na)
          case ( 6:12)
             !---- Anisotropic Parameters ----!
             Err_CFML%Flag=.true.
             Err_CFML%Msg="Incompatible Code-name for parameter name: "//trim(label(1))
             return
          case (13:15)
             !---- Xc_, Yc_, Zc_ ----!
             fac_0=molec%mxcentre(nb-12)
                nl=molec%lxcentre(nb-12)
          case (16:18)
             !---- Theta_, Phi_, Chi_ ----!
             fac_0=molec%mxcentre(nb-15)
                nl=molec%lxcentre(nb-15)
          case (19:21)
             !!! Not Yet Implemented !!!
       end select ! nb

       if (nb < ncode) then
          if (nl == 0) then
             Err_CFML%Flag=.true.
             Err_CFML%Msg="No refinable parameter was selected for "//trim(label(1))
             return
          end if
       end if

       !---- Set Others ----!
       n=1
       do
          n=n+1
          if (n > ic) exit

          npos=index(label(n),"_")
          if (npos ==0) then
             Err_CFML%Flag=.true.
             Err_CFML%Msg="No CrysFML code-name was found for "//trim(label(n))
             return
          end if

          nc=0
          do j=1,molec%natoms
             if (u_case(molec%Atname(j)) == u_case(label(n)(npos+1:npos+6))) then
                nc=j
                exit
             end if
          end do

          nd=0
          do j=1,ncode
             if (u_case(label(n)(1:npos))==u_case(trim(code_nam(j)))) then
                nd=j
                exit
             end if
          end do
          if (nd == 0) then
             Err_CFML%Flag=.true.
             Err_CFML%Msg="Code-name not found for "//trim(label(n))
             return
          end if

          n=n+1
          call Get_Num(label(n),vet,ivet,iv)
          if (iv == 1) then
             fac_1=vet(1)
          else
             fac_1=fac_0
             n=n-1
          end if

          !---- Checking ----!
          if (nd < 6 .and. nc == 0) then
             Err_CFML%Flag=.true.
             Err_CFML%Msg="Incompatible relation: "//trim(label(n))
             return
          end if

          select case (nd)
             case ( 1:3)
                !---- X_, Y_, Z_ ----!
                nl2=molec%lI_coor(nd,nc)
                call Delete_RefCodes(nl2,molec)
                molec%mI_coor(nd,nc)=fac_1
                molec%lI_coor(nd,nc)=nl
             case ( 4)
                !---- Biso_ ----!
                nl2=molec%lu_iso(nc)
                call Delete_RefCodes(nl2,molec)
                molec%mu_iso(nc)=fac_1
                molec%lu_iso(nc)=nl
             case ( 5)
                !---- Occ_ ----!
                nl2=molec%locc(nc)
                call Delete_RefCodes(nl2,molec)
                molec%mocc(nc)=fac_1
                molec%locc(nc)=nl
             case ( 6:12)
                Err_CFML%Flag=.true.
                Err_CFML%Msg="Incompatible Code-name for "//trim(label(n))
                return
             case (13:15)
                !---- Xc_, Yc_, Zc_ ----!
                nl2=molec%lxcentre(nc-12)
                call Delete_RefCodes(nl2,molec)
                molec%mxcentre(nc-12)=fac_1
                molec%lxcentre(nc-12)=nl
             case (16:18)
                !---- Theta_, Phi_, Chi_ ----!
                nl2=molec%lorient(nc-15)
                call Delete_RefCodes(nl2,molec)
                molec%morient(nc-15)=fac_1
                molec%lorient(nc-15)=nl
             case (19:21)
                !!! not yet implemented !!!
          end select ! nb
          np_cons=np_cons+1
       end do
    End Subroutine Get_ConCodes_Line_Molec

    !!--++
    !!--++ Module Subroutine Get_ConCodes_Line_Magdom(Line,Mag_dom)
    !!--++    character(len=*),         intent(in)          :: Line
    !!--++    type(Magnetic_Domain_type),    intent(in out) :: Mag_dom
    !!--++
    !!--++ Get the Magnetic Constraints Relations: Presently only 'equal'
    !!--++ related to magnetic domains
    !!--++ Created: February - 2012
    !!
    Module Subroutine Get_ConCodes_Line_Magdom(Line,Mag_dom)
       !---- Arguments ----!
       character(len=*),     intent(in)          :: Line
       type(Magnetic_Domain_type),intent(in out) :: Mag_dom

       !---- Local variables ----!
       character(len=20), dimension(30) :: label
       integer                          :: ic,n,na,nb,ina,inb,npos,ich
       integer                          :: nl,nl2,iv
       integer, dimension(1)            :: ivet
       real(kind=cp)                    :: fac_0,fac_1
       real(kind=cp),dimension(1)       :: vet

       call clear_error()

       !---- Check is chirality is present ----!
       if (Mag_Dom%chir) then
        ich=2
       else
        ich=1
       end if
       nl=0
       call Get_Words(line,label,ic)
       if (ic < 2) then
          Err_CFML%Flag=.true.
          Err_CFML%Msg="EQUAL keyword needs two labels: "//trim(line)
          return
       end if

       !---- Set Futher Information----!
       !---- Na is the number of S-domains
       !---- Nb is the number of chiral domains
       !---- Fac0 is the multiplier
       !---- Nl is the number of refinement parameter

       npos=index(label(1),"d") ! identifying magDom word
       if (npos ==0) then
          Err_CFML%Flag=.true.
          Err_CFML%Msg="The name "//trim(label(1))//" does not fit any known code-name of CrysFML "
          return
       end if
          do na=1,Mag_Dom%nd
           do nb=1,ich ! npos+8 as magdom0N should have length magD + om0N
             if (u_case(Mag_Dom%Lab(nb,na)) == u_case(label(1)(1:npos+8))) then
             fac_0=Mag_Dom%Mpop(nb,na)
                nl=Mag_Dom%Lpop(nb,na)
                exit
             end if

           end do
          end do

       !---- Set the rest elements in Contsraints ----!
       n=1
       do
          n=n+1
          if (n > ic) exit

          npos=index(label(n),"d")! identifying magDom word
          if (npos ==0) then
             Err_CFML%Flag=.true.
             Err_CFML%Msg="No CrysFML code-name was found for "//trim(label(n))
             return
          end if

          do na=1,Mag_Dom%nd
           do nb=1,ich ! npos+8 as magdom0N should have length magD + om0N
             if (u_case(trim(Mag_Dom%Lab(nb,na))) == u_case(label(n)(1:npos+8))) then
             ina=na
             inb=nb
                exit
             end if
           end do
          end do

          !---- Is there a new multiplier?
          n=n+1
          call Get_Num(label(n),vet,ivet,iv)
          if (iv == 1) then
             fac_1=vet(1)
          else
             fac_1=fac_0
             n=n-1
          end if

            nl2=Mag_Dom%Lpop(inb,ina)
            call Delete_refCodes(nl2,Mag_dom)
            Mag_Dom%Mpop(inb,ina)=fac_1
            Mag_Dom%Lpop(inb,ina)=nl

          np_cons=np_cons+1

       end do
    End Subroutine Get_ConCodes_Line_Magdom

End Submodule KWC_GetConCodes
