Submodule (CFML_Keywords_Code_Parser) KWC_VStateToAtomPar
   !---- Variables ----!
   implicit none

 Contains

    !!----
    !!---- Module Subroutine VState_to_AtomsPar(FAtom/MolCrys/Molec/Magdom,Mode)
    !!----    type(AtList_Type),            intent(in out) :: FAtom
    !!----    or
    !!----    type(MolCrystal_Type),        intent(in out) :: MolCrys
    !!----    or
    !!----    type(molecule_type),          intent(in out) :: Molec
    !!----    character(len=*), optional,   intent(in)     :: Mode
    !!----
    !!----    Update the values to the variable FAtom/MolCrys/Molec from Vector
    !!----
    !!---- Update: November 22 - 2013
    !!

    !!--++
    !!--++ Module Subroutine VState_to_AtomsPar_FAtom(FAtom,Mode,MultG)
    !!--++    type(AtList_Type),          intent(in out) :: FAtom
    !!--++    character(len=*), optional, intent(in)     :: Mode
    !!--++    integer,          optional, intent(in)     :: MultG
    !!--++
    !!--++    (Overloaded)
    !!--++
    !!--++ Update: November 22 - 2013.
    !!--++ Modified to include standard deviations, November 3, 2013 (JRC)
    !!
    Module Subroutine VState_to_AtomsPar_FAtom(FAtom,Mode,MultG)
       !---- Arguments ----!
       type(AtList_Type),          intent(in out) :: FAtom
       character(len=*), optional, intent(in)     :: Mode
       integer,          optional, intent(in)     :: MultG

       !---- Local Variables ----!
       integer          :: i,j,l
       real(kind=cp)    :: fac1,fac2
       character(len=1) :: car

       call clear_error()

       car="s"
       if (present(mode)) car=adjustl(mode)

       Select Type(atom => FAtom%atom)
        type is (atm_ref_type)
          if(present(MultG)) then
            fac1=atom(1)%Occ*real(MultG)/real(atom(1)%mult)
            fac1=1.0/fac1
          end if
          do i=1,FAtom%natoms
             !---- XYZ ----!
             do j=1,3
                l=atom(i)%l_x(j)
                if (l == 0) cycle
                if (l > np_refi) then
                   Err_CFML%Flag=.true.
                   Err_CFML%Msg="The number of Refinable parameters is wrong"
                   return
                end if
                select case (car)
                   case ("v","V") ! Passing Value
                      atom(i)%x(j)=v_vec(l)*atom(i)%m_x(j)

                   case ("s","S") ! Passing Shift
                      atom(i)%x(j)=atom(i)%x(j)+v_shift(l)*atom(i)%m_x(j)
                end select
                atom(i)%x_std(j)=v_vec_std(l)*atom(i)%m_x(j)
             end do

             !---- MxMyMz ----!
             do j=1,3
                l=atom(i)%l_moment(j)
                if (l == 0) cycle
                if (l > np_refi) then
                   Err_CFML%Flag=.true.
                   Err_CFML%Msg="The number of Refinable parameters is wrong"
                   return
                end if
                select case (car)
                   case ("v","V") ! Passing Value
                      atom(i)%moment(j)=v_vec(l)*atom(i)%m_moment(j)

                   case ("s","S") ! Passing Shift
                      atom(i)%moment(j)=atom(i)%moment(j)+v_shift(l)*atom(i)%m_moment(j)
                end select
                atom(i)%moment_std(j)=v_vec_std(l)*atom(i)%m_moment(j)
             end do

             !---- BISO ----!
             l=atom(i)%l_u_iso
             if (l > 0) then
                 if (l > np_refi) then
                     Err_CFML%Flag=.true.
                     Err_CFML%Msg="The number of Refinable parameters is wrong"
                     return
                 end if
                 select case (car)
                    case ("v","V") ! Passing Value
                       atom(i)%u_iso=v_vec(l)*atom(i)%m_u_iso

                    case ("s","S") ! Passing Shift
                       atom(i)%u_iso=atom(i)%u_iso+v_shift(l)*atom(i)%m_u_iso
                 end select
                 atom(i)%u_iso_std=v_vec_std(l)*atom(i)%m_u_iso
             end if


             !---- OCC ----!
             l=atom(i)%l_occ
             if (l > 0) then
                 if (l > np_refi) then
                    Err_CFML%Flag=.true.
                    Err_CFML%Msg="The number of Refinable parameters is wrong"
                    return
                 end if
                 select case (car)
                    case ("v","V") ! Passing Value
                       atom(i)%occ=v_vec(l)*atom(i)%m_occ
                    case ("s","S") ! Passing Shift
                       atom(i)%occ=atom(i)%occ+v_shift(l)*atom(i)%m_occ

                 end select
                 atom(i)%occ_std=v_vec_std(l)*atom(i)%m_occ

                 if(present(MultG)) then  !Update occupancy from occupation factors
                   fac2=fac1*real(MultG)/real(atom(i)%mult)
                   atom(i)%VarF(1)=atom(i)%occ*fac2
                   atom(i)%VarF(2)=atom(i)%occ_std*fac2
                 end if
             end if

             !---- BANIS ----!
             do j=1,6
                l=atom(i)%l_u(j)
                if (l == 0) cycle
                if (l > np_refi) then
                   Err_CFML%Flag=.true.
                   Err_CFML%Msg="The number of Refinable parameters is wrong"
                   return
                end if
                select case (car)
                   case ("v","V") ! Passing Value
                      atom(i)%u(j)=v_vec(l)*atom(i)%m_u(j)

                   case ("s","S") ! Passing Shift
                      atom(i)%u(j)=atom(i)%u(j)+v_shift(l)*atom(i)%m_u(j)
                end select
                atom(i)%u_std(j)=v_vec_std(l)*atom(i)%m_u(j)
             end do

          end do
      End Select !type

    End Subroutine VState_to_AtomsPar_FAtom
    !!--++
    !!--++ Module Subroutine VState_to_AtomsPar_FmAtom(FmAtom,MGp,Mode,Mag_dom)
    !!--++    type(mAtom_List_Type),      intent(in out)           :: FmAtom
    !!--++    type(MagSymm_k_Type),       intent(in)               :: MGp
    !!--++    character(len=*),           optional, intent(in)     :: Mode
    !!--++    type(Magnetic_Domain_type), optional, intent(in out) :: Mag_dom
    !!--++
    !!--++ magnetic clone of VState_to_AtomsPar_FAtom
    !!--++ Created: December - 2011
    !!--++ Updated: February - 2012, November 3, 2013 (standard deviations,JRC)
    !!
    Module Subroutine VState_to_AtomsPar_FmAtom(FmAtom,MGp,Mode,Mag_dom)
       !---- Arguments ----!
       type(mAtom_List_Type),                intent(in out) :: FmAtom
       type(MagSymm_k_Type),                 intent(in)     :: MGp
       character(len=*),           optional, intent(in)     :: Mode
       type(Magnetic_Domain_type), optional, intent(in out) :: Mag_dom
       !---- Local Variables ----!
       integer          :: i,j,l,ik,ich
       character(len=1) :: car

       call clear_error()
       car="s"
       if (present(mode)) car=adjustl(mode)

       do i=1,FmAtom%natoms
          ik=FmAtom%atom(i)%nvk

          !---- Rxyz ----!
          do j=1,3
             l=FmAtom%atom(i)%lSkR(j,ik)
             if (l == 0) cycle
             if (l > np_refi) then
                Err_CFML%Flag=.true.
                Err_CFML%Msg="The number of Refinable parameters is wrong"
                return
             end if

             select case (car)

                case ("v","V") ! Passing Value
                 if(MGp%Sk_type == "Spherical_Frame") then
                   FmAtom%atom(i)%Spher_SkR(j,ik)=v_vec(l)*FmAtom%atom(i)%mSkR(j,ik)
                   FmAtom%atom(i)%Spher_SkR_std(j,ik)=v_vec_std(l)*FmAtom%atom(i)%mSkR(j,ik)
                 else
                   FmAtom%atom(i)%SkR(j,ik)=v_vec(l)*FmAtom%atom(i)%mSkR(j,ik)
                   FmAtom%atom(i)%SkR_std(j,ik)=v_vec_std(l)*FmAtom%atom(i)%mSkR(j,ik)
                 end if

                case ("s","S") ! Passing Shift
                 if(MGp%Sk_type == "Spherical_Frame") then
                   FmAtom%atom(i)%Spher_SkR(j,ik)=FmAtom%atom(i)%Spher_SkR(j,ik)+v_shift(l)*FmAtom%atom(i)%mSkR(j,ik)
                   FmAtom%atom(i)%Spher_SkR_std(j,ik)=v_vec_std(l)*FmAtom%atom(i)%mSkR(j,ik)
                 else
                   FmAtom%atom(i)%SkR(j,ik)=FmAtom%atom(i)%SkR(j,ik)+v_shift(l)*FmAtom%atom(i)%mSkR(j,ik)
                   FmAtom%atom(i)%SkR_std(j,ik)=v_vec_std(l)*FmAtom%atom(i)%mSkR(j,ik)
                 end if

             end select
          end do

          !---- Ixyz ----!
          do j=1,3
             l=FmAtom%atom(i)%lSkI(j,ik)
             if (l == 0) cycle
             if (l > np_refi) then
                Err_CFML%Flag=.true.
                Err_CFML%Msg="The number of Refinable parameters is wrong"
                return
             end if
             select case (car)
                case ("v","V") ! Passing Value
                 if(MGp%Sk_type == "Spherical_Frame") then
                   FmAtom%atom(i)%Spher_SkI(j,ik)=v_vec(l)*FmAtom%atom(i)%mSkI(j,ik)
                   FmAtom%atom(i)%Spher_SkI_std(j,ik)=v_vec_std(l)*FmAtom%atom(i)%mSkI(j,ik)
                 else
                   FmAtom%atom(i)%SkI(j,ik)=v_vec(l)*FmAtom%atom(i)%mSkI(j,ik)
                   FmAtom%atom(i)%SkI_std(j,ik)=v_vec_std(l)*FmAtom%atom(i)%mSkI(j,ik)
                 end if

                case ("s","S") ! Passing Shift
                 if(MGp%Sk_type == "Spherical_Frame") then
                   FmAtom%atom(i)%Spher_SkI(j,ik)=FmAtom%atom(i)%Spher_SkI(j,ik)+v_shift(l)*FmAtom%atom(i)%mSkI(j,ik)
                   FmAtom%atom(i)%Spher_SkI_std(j,ik)=v_vec_std(l)*FmAtom%atom(i)%mSkI(j,ik)
                 else
                   FmAtom%atom(i)%SkI(j,ik)=FmAtom%atom(i)%SkI(j,ik)+v_shift(l)*FmAtom%atom(i)%mSkI(j,ik)
                   FmAtom%atom(i)%SkI_std(j,ik)=v_vec_std(l)*FmAtom%atom(i)%mSkI(j,ik)
                 end if

             end select
          end do

          !---- MagPh ----!
          l=FmAtom%atom(i)%lmphas(ik)
          if (l /= 0) then
            if (l > np_refi) then
               Err_CFML%Flag=.true.
               Err_CFML%Msg="The number of Refinable parameters is wrong"
               return
            end if
            select case (car)
               case ("v","V") ! Passing Value
                  FmAtom%atom(i)%mphas(ik)=v_vec(l)*FmAtom%atom(i)%mmphas(ik)

               case ("s","S") ! Passing Shift
                  FmAtom%atom(i)%mphas(ik)=FmAtom%atom(i)%mphas(ik)+v_shift(l)*FmAtom%atom(i)%mmphas(ik)
            end select
            FmAtom%atom(i)%mphas_std(ik)=v_vec_std(l)*FmAtom%atom(i)%mmphas(ik)
          end if
          !---- C1-12 ----!
          do j=1,12
             l=FmAtom%atom(i)%lbas(j,ik)
             if (l == 0) cycle
             if (l > np_refi) then
                Err_CFML%Flag=.true.
                Err_CFML%Msg="The number of Refinable parameters is wrong"
                return
             end if
             select case (car)
                case ("v","V") ! Passing Value
                   FmAtom%atom(i)%cbas(j,ik)=v_vec(l)*FmAtom%atom(i)%mbas(j,ik)

                case ("s","S") ! Passing Shift
                   FmAtom%atom(i)%cbas(j,ik)=FmAtom%atom(i)%cbas(j,ik)+v_shift(l)*FmAtom%atom(i)%mbas(j,ik)
             end select
             FmAtom%atom(i)%cbas_std(j,ik)=v_vec_std(l)*FmAtom%atom(i)%mbas(j,ik)
          end do
       end do !on atoms

       !---- Check is chirality is present ----!
       if(present(Mag_Dom)) then
         if (Mag_Dom%chir) then
          ich=2
         else
          ich=1
         end if

         do i=1,Mag_Dom%nd
           do j=1,ich
               l=Mag_Dom%Lpop(j,i)
               if (l == 0) cycle
               if (l > np_refi) then
                  Err_CFML%Flag=.true.
                  Err_CFML%Msg="The number of Refinable parameters is wrong"
                  return
               end if
               select case (car)
                  case ("v","V") ! Passing Value
                     Mag_Dom%pop(j,i)=v_vec(l)*Mag_Dom%Mpop(j,i)

                  case ("s","S") ! Passing Shift
                     Mag_Dom%pop(j,i)=Mag_Dom%pop(j,i)+v_shift(l)*Mag_Dom%Mpop(j,i)
               end select
               Mag_Dom%pop_std(j,i)=v_vec_std(l)*Mag_Dom%Mpop(j,i)
           end do
         end do
       end if
    End Subroutine VState_to_AtomsPar_FmAtom

    !!--++
    !!--++ Module Subroutine VState_to_AtomsPar_Molcrys(Molcrys,Mode)
    !!--++    type(MolCrystal_Type), intent(in out) :: MolCrys
    !!--++    character(len=*), optional,   intent(in)     :: Mode
    !!--++
    !!--++    (Overloaded)
    !!--++
    !!--++ Update: November 22 - 2013
    !!
    Module Subroutine VState_to_AtomsPar_Molcrys(Molcrys,Mode)
       !---- Arguments ----!
       type(MolCrystal_Type), intent(in out) :: MolCrys
       character(len=*), optional,    intent(in)     :: Mode

       !---- Local variables ----!
       integer          :: i,j,jj,k,l
       character(len=1) :: car

       call clear_error()

       car="s"
       if (present(mode)) car=adjustl(mode)
       Select Type(Atm => molcrys%atm)
         Type is(atm_ref_type)
           do i=1,molcrys%n_free
              !---- XYZ ----!
              do j=1,3
                 l=Atm(i)%l_x(j)
                 if (l == 0) cycle
                 if (l > np_refi) then
                    Err_CFML%Flag=.true.
                    Err_CFML%Msg="The number of Refinable parameters is wrong"
                    return
                 end if
                 select case (car)
                    case ("v","V") ! Passing Value
                       Atm(i)%x(j)=v_vec(l)*Atm(i)%m_x(j)

                    case ("s","S") ! Passing Shift
                       Atm(i)%x(j)=Atm(i)%x(j)+v_shift(l)*Atm(i)%m_x(j)
                 end select
              end do

              !---- BISO ----!
              l=Atm(i)%l_u_iso
              if (l > 0) then
                 if (l > np_refi) then
                    Err_CFML%Flag=.true.
                    Err_CFML%Msg="The number of Refinable parameters is wrong"
                    return
                 end if
                 select case (car)
                    case ("v","V") ! Passing Value
                       Atm(i)%u_iso=v_vec(l)*Atm(i)%m_u_iso

                    case ("s","S") ! Passing Shift
                       Atm(i)%u_iso=Atm(i)%u_iso+v_shift(l)*Atm(i)%m_u_iso
                 end select
              end if

              !---- OCC ----!
              l=Atm(i)%l_occ
              if (l > 0) then
                 if (l > np_refi) then
                    Err_CFML%Flag=.true.
                    Err_CFML%Msg="The number of Refinable parameters is wrong"
                    return
                 end if
                 select case (car)
                    case ("v","V") ! Passing Value
                       Atm(i)%occ=v_vec(l)*Atm(i)%m_occ

                    case ("s","S") ! Passing Shift
                       Atm(i)%occ=Atm(i)%occ+v_shift(l)*Atm(i)%m_occ
                 end select
              end if

              !---- BANIS ----!
              do j=1,6
                 l=Atm(i)%l_u(i)
                 if (l == 0) cycle
                 if (l > np_refi) then
                    Err_CFML%Flag=.true.
                    Err_CFML%Msg="The number of Refinable parameters is wrong"
                    return
                 end if
                 select case (car)
                    case ("v","V") ! Passing Value
                       Atm(i)%u(j)=v_vec(l)*Atm(i)%m_u(j)

                    case ("s","S") ! Passing Shift
                       Atm(i)%u(j)=Atm(i)%u(j)+v_shift(l)*Atm(i)%m_u(j)
                 end select
              end do
           end do
       End Select !type

       do k=1,molcrys%n_mol
          do i=1,molcrys%mol(k)%natoms
             !---- Coordinates ----!
             do j=1,3
                l=molcrys%mol(k)%lI_Coor(j,i)
                if (l == 0) cycle
                if (l > np_refi) then
                   Err_CFML%Flag=.true.
                   Err_CFML%Msg="The number of Refinable parameters is wrong"
                   return
                end if
                select case (car)
                   case ("v","V") ! Passing Value
                      molcrys%mol(k)%I_Coor(j,i)=v_vec(l)*molcrys%mol(k)%mI_Coor(j,i)

                   case ("s","S") ! Passing Shift
                      molcrys%mol(k)%I_Coor(j,i)=molcrys%mol(k)%I_Coor(j,i)+v_shift(l)*molcrys%mol(k)%mI_Coor(j,i)
                end select
             end do

             !---- Biso ----!
             l=molcrys%mol(k)%lu_iso(i)
             if (l > 0) then
                if (l > np_refi) then
                   Err_CFML%Flag=.true.
                   Err_CFML%Msg="The number of Refinable parameters is wrong"
                   return
                end if
                select case (car)
                   case ("v","V") ! Passing Value
                      molcrys%mol(k)%u_iso(i)=v_vec(l)*molcrys%mol(k)%mu_iso(i)

                   case ("s","S") ! Passing Shift
                      molcrys%mol(k)%u_iso(i)=molcrys%mol(k)%u_iso(i)+v_shift(l)*molcrys%mol(k)%mu_iso(i)
                end select
             end if

             !---- Occ ----!
             l=molcrys%mol(k)%locc(i)
             if (l > 0) then
                if (l > np_refi) then
                   Err_CFML%Flag=.true.
                   Err_CFML%Msg="The number of Refinable parameters is wrong"
                   return
                end if
                select case (car)
                   case ("v","V") ! Passing Value
                      molcrys%mol(k)%occ(i)=v_vec(l)*molcrys%mol(k)%mocc(i)

                   case ("s","S") ! Passing Shift
                      molcrys%mol(k)%occ(i)=molcrys%mol(k)%occ(i)+v_shift(l)*molcrys%mol(k)%mocc(i)
                end select
             end if

             !---- Centre ----!
             do j=1,3
                l=molcrys%mol(i)%lxcentre(j)
                if (l == 0) cycle
                if (l > np_refi) then
                   Err_CFML%Flag=.true.
                   Err_CFML%Msg="The number of Refinable parameters is wrong"
                   return
                end if
                select case (car)
                   case ("v","V") ! Passing Value
                      molcrys%mol(i)%xcentre(j)=v_vec(l)*molcrys%mol(i)%mxcentre(j)

                   case ("s","S") ! Passing Shift
                      molcrys%mol(i)%xcentre(j)=molcrys%mol(i)%xcentre(j)+v_shift(l)*molcrys%mol(i)%mxcentre(j)
                end select
             end do

             !---- Orient ----!
             do j=1,3
                l=molcrys%mol(i)%lorient(j)
                if (l == 0) cycle
                if (l > np_refi) then
                   Err_CFML%Flag=.true.
                   Err_CFML%Msg="The number of Refinable parameters is wrong"
                   return
                end if
                select case (car)
                   case ("v","V") ! Passing Value
                      molcrys%mol(i)%orient(j)=v_vec(l)*molcrys%mol(i)%morient(j)

                   case ("s","S") ! Passing Shift
                      molcrys%mol(i)%orient(j)=molcrys%mol(i)%orient(j)+v_shift(l)*molcrys%mol(i)%morient(j)
                end select
             end do

             !---- T_TLS ----!
             do j=1,6
                l=molcrys%mol(i)%lT_TLS(j)
                if (l == 0) cycle
                if (l > np_refi) then
                   Err_CFML%Flag=.true.
                   Err_CFML%Msg="The number of Refinable parameters is wrong"
                   return
                end if
                select case (car)
                   case ("v","V") ! Passing Value
                      molcrys%mol(i)%T_TLS(j)=v_vec(l)*molcrys%mol(i)%mT_TLS(j)

                   case ("s","S") ! Passing Shift
                      molcrys%mol(i)%T_TLS(j)=molcrys%mol(i)%T_TLS(j)+v_shift(l)*molcrys%mol(i)%mT_TLS(j)
                end select
             end do

             !---- L_TLS ----!
             do j=1,6
                l=molcrys%mol(i)%lL_TLS(j)
                if (l == 0) cycle
                if (l > np_refi) then
                   Err_CFML%Flag=.true.
                   Err_CFML%Msg="The number of Refinable parameters is wrong"
                   return
                end if
                select case (car)
                   case ("v","V") ! Passing Value
                      molcrys%mol(i)%L_TLS(j)=v_vec(l)*molcrys%mol(i)%mL_TLS(j)

                   case ("s","S") ! Passing Shift
                      molcrys%mol(i)%L_TLS(j)=molcrys%mol(i)%L_TLS(j)+v_shift(l)*molcrys%mol(i)%mL_TLS(j)
                end select
             end do

             !---- S_TLS ----!
             do j=1,3
                do jj=1,3
                   l=molcrys%mol(i)%lS_TLS(j,jj)
                   if (l == 0) cycle
                   if (l > np_refi) then
                      Err_CFML%Flag=.true.
                      Err_CFML%Msg="The number of Refinable parameters is wrong"
                      return
                   end if
                   select case (car)
                      case ("v","V") ! Passing Value
                         molcrys%mol(i)%S_TLS(j,jj)=v_vec(l)*molcrys%mol(i)%mS_TLS(j,jj)

                      case ("s","S") ! Passing Shift
                         molcrys%mol(i)%S_TLS(j,jj)=molcrys%mol(i)%S_TLS(j,jj)+v_shift(l)*molcrys%mol(i)%mS_TLS(j,jj)
                   end select
                end do
             end do
          end do
       end do
    End Subroutine VState_to_AtomsPar_Molcrys

    !!--++
    !!--++ Module Subroutine VState_to_AtomsPar_Molec(Molec,Mode)
    !!--++    type(molecule_type),          intent(in out) :: Molec
    !!--++    character(len=*), optional,   intent(in)     :: Mode
    !!--++
    !!--++    (Overloaded)
    !!--++
    !!--++ Update: November 22 - 2013
    !!
    Module Subroutine VState_to_AtomsPar_Molec(Molec,Mode)
       !---- Arguments ----!
       type(molecule_type),          intent(in out) :: Molec
       character(len=*), optional,   intent(in)     :: Mode

       !---- Local variables ----!
       integer          :: i,j,l
       character(len=1) :: car

       call clear_error()

       car="s"
       if (present(mode)) car=adjustl(mode)

       do i=1,molec%natoms
          !---- Coordinates ----!
          do j=1,3
             l=molec%lI_Coor(j,i)
             if (l == 0) cycle
             if (l > np_refi) then
                Err_CFML%Flag=.true.
                Err_CFML%Msg="The number of Refinable parameters is wrong"
                return
             end if
             select case (car)
                case ("v","V") ! Passing Value
                   molec%I_Coor(j,i)=v_vec(l)*molec%mI_Coor(j,i)

                case ("s","S") ! Passing Shift
                   molec%I_Coor(j,i)=molec%I_Coor(j,i)+v_shift(l)*molec%mI_Coor(j,i)
             end select
          end do

          !---- Biso ----!
          l=molec%lu_iso(i)
          if (l > 0) then
             if (l > np_refi) then
                Err_CFML%Flag=.true.
                Err_CFML%Msg="The number of Refinable parameters is wrong"
                return
             end if
             select case (car)
                 case ("v","V") ! Passing Value
                    molec%u_iso(i)=v_vec(l)*molec%mu_iso(i)

                 case ("s","S") ! Passing Shift
                    molec%u_iso(i)=molec%u_iso(i)+v_shift(l)*molec%mu_iso(i)
             end select
          end if

          !---- Occ ----!
          l=molec%locc(i)
          if (l > 0) then
             if (l > np_refi) then
                Err_CFML%Flag=.true.
                Err_CFML%Msg="The number of Refinable parameters is wrong"
                return
             end if
             select case (car)
                 case ("v","V") ! Passing Value
                    molec%occ(i)=v_vec(l)*molec%mocc(i)

                 case ("s","S") ! Passing Shift
                    molec%occ(i)=molec%occ(i)+v_shift(l)*molec%mocc(i)
             end select
          end if
       end do

       !---- Centre ----!
       do j=1,3
          l=molec%lxcentre(j)
          if (l == 0) cycle
          if (l > np_refi) then
             Err_CFML%Flag=.true.
             Err_CFML%Msg="The number of Refinable parameters is wrong"
             return
          end if
          select case (car)
              case ("v","V") ! Passing Value
                 molec%xcentre(j)=v_vec(l)*molec%mxcentre(j)

              case ("s","S") ! Passing Shift
                 molec%xcentre(j)=molec%xcentre(j)+v_shift(l)*molec%mxcentre(j)
          end select
       end do

       !---- Orient ----!
       do j=1,3
          l=molec%lorient(j)
          if (l == 0) cycle
          if (l > np_refi) then
             Err_CFML%Flag=.true.
             Err_CFML%Msg="The number of Refinable parameters is wrong"
             return
          end if
          select case (car)
              case ("v","V") ! Passing Value
                 molec%orient(j)=v_vec(l)*molec%morient(j)

              case ("s","S") ! Passing Shift
                 molec%orient(j)=molec%orient(j)+v_shift(l)*molec%morient(j)
          end select
       end do

       !---- T_TLS ----!
       do j=1,6
          l=molec%lT_TLS(j)
          if (l == 0) cycle
          if (l > np_refi) then
             Err_CFML%Flag=.true.
             Err_CFML%Msg="The number of Refinable parameters is wrong"
             return
          end if
          select case (car)
              case ("v","V") ! Passing Value
                 molec%T_TLS(j)=v_vec(l)*molec%mT_TLS(j)

              case ("s","S") ! Passing Shift
                 molec%T_TLS(j)=molec%T_TLS(j)+v_shift(l)*molec%mT_TLS(j)
          end select
       end do

       !---- L_TLS ----!
       do j=1,6
          l=molec%lL_TLS(j)
          if (l == 0) cycle
          if (l > np_refi) then
             Err_CFML%Flag=.true.
             Err_CFML%Msg="The number of Refinable parameters is wrong"
             return
          end if
          select case (car)
              case ("v","V") ! Passing Value
                 molec%L_TLS(j)=v_vec(l)*molec%mL_TLS(j)

              case ("s","S") ! Passing Shift
                 molec%L_TLS(j)=molec%L_TLS(j)+v_shift(l)*molec%mL_TLS(j)
          end select
       end do

       !---- S_TLS ----!
       do i=1,3
          do j=1,3
             l=molec%lS_TLS(i,j)
             if (l == 0) cycle
             if (l > np_refi) then
                Err_CFML%Flag=.true.
                Err_CFML%Msg="The number of Refinable parameters is wrong"
                return
             end if
             select case (car)
                 case ("v","V") ! Passing Value
                    molec%S_TLS(i,j)=v_vec(l)*molec%mS_TLS(i,j)

                 case ("s","S") ! Passing Shift
                    molec%S_TLS(i,j)=molec%S_TLS(i,j)+v_shift(l)*molec%mS_TLS(i,j)
             end select
          end do
       end do
    End Subroutine VState_to_AtomsPar_Molec

    !!----
    !!---- Module Subroutine VState_to_ModelPar(Model,Mode)
    !!----    type(Nonatomic_Parameter_List_Type), intent(in out) :: model
    !!----    character(len=*), optional,          intent(in)     :: Mode
    !!----
    !!----    (Overloaded)
    !!----
    !!---- Update: November 2 - 2013
    !!
    Module Subroutine VState_to_ModelPar(Model,Mode)
       !---- Arguments ----!
       type(Nonatomic_Parameter_List_Type), intent(in out) :: model
       character(len=*), optional,          intent(in)     :: Mode

       !---- Local variables ----!
       integer          :: i,l
       character(len=1) :: car

       call clear_error()

       car="s"
       if (present(mode)) car=adjustl(mode)

       do i=1,model%npar
          l=model%par(i)%Lcode
          !write(*,"(a,i5,a,i5)") " Parameter: "//trim(model%par(i)%nam),i,"  Code-number:",l
          if (l == 0) cycle
          if (l > np_refi) then
             Err_CFML%Flag=.true.
             Err_CFML%Msg="The number of Refinable parameters is wrong"
             return
          end if
          select case (car)
             case ("v","V") ! Passing Value
                model%par(i)%value=v_vec(l)*model%par(i)%multip

             case ("s","S") ! Passing Shift
                model%par(i)%value=model%par(i)%value+v_shift(l)*model%par(i)%multip
          end select
          model%par(i)%sigma=v_vec_std(l)*model%par(i)%multip
          !write(*,"(a,2f14.5)") " New value and sigma: "//trim(model%par(i)%nam),model%par(i)%value,model%par(i)%sigma
       end do
    End Subroutine VState_to_ModelPar

End Submodule KWC_VStateToAtomPar
