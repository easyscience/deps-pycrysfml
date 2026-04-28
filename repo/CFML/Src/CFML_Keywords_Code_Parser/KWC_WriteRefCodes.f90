Submodule (CFML_Keywords_Code_Parser) KWC_WriteRefCodes
   !---- Variables ----!
   implicit none

 Contains

    !!----
    !!---- Module Subroutine Write_Info_RefCodes(FAtom/FmAtom/MolCrys/Molec/MagStr, Iunit)
    !!----    type(AtList_Type),                       intent(in) :: FAtom
    !!----    or
    !!----    type(mAtom_List_Type),                   intent(in) :: FmAtom
    !!----    or
    !!----    type(MolCrystal_Type),                   intent(in) :: Molcrys
    !!----    or
    !!----    type(molecule_type),                     intent(in) :: Molec
    !!----    or
    !!--++    type(mAtom_List_Type),                   intent(in) :: FmAtom
    !!--++    and type(Magnetic_Domain_type),optional, intent(in) :: Mag_dom
    !!----    integer, optional,                       intent(in) :: Iunit
    !!----
    !!----    Write the Information about Refinement Codes
    !!----
    !!---- Update: March - 2005
    !!---- Update: February - 2012
    !!

    !!--++
    !!--++ Module Subroutine Write_Info_RefCodes_FAtom(FAtom, Spg, Iunit)
    !!--++    type(AtList_Type),     intent(in) :: FAtom
    !!--++    type(SPG_Type),        intent(in) :: Spg
    !!--++    integer, optional,     intent(in) :: Iunit
    !!--++
    !!--++    Overloaded
    !!--++    Write the Information about Refinement Codes
    !!--++
    !!--++ Update: March - 2005
    !!
    Module Subroutine Write_Info_RefCodes_FAtom(FAtom, Spg, Iunit)
       !---- Arguments ----!
       type(AtList_Type),      intent(in) :: FAtom
       type(SPG_Type),         intent(in) :: Spg
       integer, optional,      intent(in) :: Iunit

       !---- Local variables ----!
       character(len=20)              :: car
       character(len=60)              :: fmt1,fmt2,fmt3,fmt4,fmt5
       Character(len=25),dimension(3) :: symcar
       integer                        :: i,j,k,n,na,np,lun,p1,p2,p3,p4
       real(kind=cp)                  :: mu
       real(kind=cp), dimension(3)    :: tr,taux
       real(kind=cp), dimension(3,3)  :: Rot

       !---- Format Zone ----!
       fmt1="(t5,a,t16,i3,t27,a,t33,4(tr6,f8.4),tr8,i2,tr6,f8.3,i9)"
       fmt2="(t10,i3,t21,a,t31,a,t39,f8.4)"

       lun=6
       if (present(iunit)) lun=iunit
      Select Type( Atom => FAtom%atom)
       type is (atm_ref_type)
          if (NP_Refi > 0) then
             write(unit=lun, fmt="(a)") " "
             write(unit=lun, fmt="(a,i5)") " => Refinable Parameters: ",np_refi
             write(unit=lun, fmt="(a)") " "
             write(unit=lun, fmt="(a,a)")"    Name      N.Param   Code-Name       Value        L.Bound       ",&
                                         "U.Bound         Step    Condition   Multiplier    N.Atom"
             write(unit=lun, fmt="(a,a)")" ==================================================================",&
                                         "========================================================="


             do i=1,FAtom%natoms
                do j=1,3
                   if (Atom(i)%l_x(j) > 0) then
                      na=Atom(i)%l_x(j)
                      mu=Atom(i)%m_x(j)
                      car=trim(code_nam(j))//trim(Atom(i)%lab)
                      write(unit=lun,fmt=fmt1)  &
                           trim(car),na,trim(V_Name(na)),V_Vec(na),V_Bounds(:,na),V_BCon(na),mu,V_List(na)
                   end if
                end do

                if (Atom(i)%l_occ > 0) then
                   na=Atom(i)%l_occ
                   mu=Atom(i)%m_occ
                   car=trim(code_nam(5))//trim(Atom(i)%lab)
                   write(unit=lun,fmt=fmt1)  &
                        trim(car),na,trim(V_Name(na)),V_Vec(na),V_Bounds(:,na),V_BCon(na),mu,V_List(na)
                end if

                if (Atom(i)%l_u_iso > 0) then
                   na=Atom(i)%l_u_iso
                   mu=Atom(i)%m_u_iso
                   car=trim(code_nam(4))//trim(Atom(i)%lab)
                   write(unit=lun,fmt=fmt1)  &
                        trim(car),na,trim(V_Name(na)),V_Vec(na),V_Bounds(:,na),V_BCon(na),mu,V_List(na)
                end if

                do j=1,6
                   if (Atom(i)%l_u(j) > 0) then
                      na=Atom(i)%l_u(j)
                      mu=Atom(i)%m_u(j)
                      car=trim(code_nam(5+j))//trim(Atom(i)%lab)
                      write(unit=lun,fmt=fmt1)  &
                           trim(car),na,trim(V_Name(na)),V_Vec(na),V_Bounds(:,na),V_BCon(na),mu,V_List(na)
                   end if
                end do
             end do
          end if

          if (NP_Cons > 0) then
             write(unit=lun, fmt="(a)") " "
             write(unit=lun, fmt="(a,i5)") " => Constraints relations: ",np_cons
             write(unit=lun, fmt="(a)") " "
             write(unit=lun, fmt="(a)") "       N.Constr     Name      Father    Factor"
             write(unit=lun, fmt="(a)") "    ============================================="

             np=0
             do i=1,NP_Refi
                n=0
                do j=1,FAtom%natoms
                   n=n+count(Atom(j)%l_x ==i)
                   n=n+count(Atom(j)%l_u==i)
                   if (Atom(j)%l_occ==i) n=n+1
                   if (Atom(j)%l_u_iso==i) n=n+1
                end do
                if ( n > 1) then
                   do j=1,FAtom%natoms
                      do k=1,3
                         if (Atom(j)%l_x(k) == i) then
                           car=trim(code_nam(i))//trim(Atom(j)%lab)
                           if (trim(car)==trim(V_Name(i))) cycle
                            np=np+1
                            write(unit=lun,fmt=fmt2)  np, trim(car), &
                                 trim(V_Name(i)),Atom(j)%m_x(k)
                         end if
                      end do

                      if (Atom(j)%l_u_iso == i) then
                         car=trim(code_nam(4))//trim(Atom(j)%lab)
                         if (trim(car)==trim(V_Name(i))) cycle
                         np=np+1
                         write(unit=lun,fmt=fmt2)  np, trim(car), &
                              trim(V_Name(i)),Atom(j)%m_u_iso
                      end if

                      if (Atom(j)%l_occ == i) then
                         car=trim(code_nam(5))//trim(Atom(j)%lab)
                         if (trim(car)==trim(V_Name(i))) cycle
                         np=np+1
                         write(unit=lun,fmt=fmt2)  np, trim(car), &
                              trim(V_Name(i)),Atom(j)%m_occ
                      end if

                      do k=1,6
                         if (Atom(j)%l_u(k) == i) then
                            car=trim(code_nam(5+k))//trim(Atom(j)%lab)
                            if (trim(car)==trim(V_Name(i))) cycle
                            np=np+1
                            write(unit=lun,fmt=fmt2)  np, trim(car), &
                                 trim(V_Name(i)),Atom(j)%m_u(k)
                         end if
                      end do
                   end do
                end if

             end do
          end if

          if (NP_Rest_Dis > 0) then
             write(unit=lun, fmt="(a)") " "
             write(unit=lun, fmt="(a,i5)") " => Distance Restraints relations: ",np_rest_dis
             write(unit=lun, fmt="(a)") " "
             write(unit=lun, fmt="(a)") "   N.Rest  Distance    Sigma    Atom1     Atom2: Symmetry_Op"
             write(unit=lun, fmt="(a)") " ==============================================================="
             fmt3="(i7,tr3,f8.4,tr4,f6.3,t34,a,t43,a)"
             do i=1,np_rest_dis
                p1=dis_rest(i)%p(1)
                p2=dis_rest(i)%p(2)
                n=0
                tr=0.0
                symcar(1)=" "
                if (len_trim(dis_rest(i)%stcode) > 0) then
                   call Read_SymTrans_Code(dis_rest(i)%stcode,n,tr)
                   taux=Spg%OP(n)%Mat(1:3,4)
                   Rot=Spg%OP(n)%Mat(1:3,1:3)
                   tr=tr+taux
                   call get_symSymb(Rot,Tr,symcar(1))
                end if
                symcar(1)=": "//symcar(1)
                write(unit=lun, fmt=fmt3) i,dis_rest(i)%dobs,dis_rest(i)%sigma,trim(Atom(p1)%Lab), &
                     trim(Atom(p2)%Lab)//trim(symcar(1))
             end do
          end if

          if (NP_Rest_Ang > 0) then
             write(unit=lun, fmt="(a)") " "
             write(unit=lun, fmt="(a,i5)") " => Angle Restraints relations: ",np_rest_ang
             write(unit=lun, fmt="(a)") " "
             write(unit=lun, fmt="(a)") &
             "   N.Rest   Atom1        Atom2: Symmetry_Op      Atom3: Symmetry_Op              Angle   Sigma"
             write(unit=lun, fmt="(a)") &
             " =============================================================================================="
             fmt4="(i7,tr5,a,t22,a,t50,a,t75,f12.4,tr3,f7.4)"
             do i=1,np_rest_ang
                p1=ang_rest(i)%p(1)
                p2=ang_rest(i)%p(2)
                p3=ang_rest(i)%p(3)
                do j=1,2
                   n=0
                   tr=0.0
                   symcar(j)=" "
                   if (len_trim(Ang_rest(i)%stcode(j)) > 0) then
                      call Read_SymTrans_Code(Ang_rest(i)%stcode(j),n,tr)
                      taux=Spg%OP(n)%Mat(1:3,4)
                      Rot=Spg%OP(n)%Mat(1:3,1:3)
                      tr=tr+taux
                      call get_symSymb(Rot,Tr,symcar(j))
                   end if
                   symcar(j)=": "//symcar(j)
                end do
                write(unit=lun, fmt=fmt4) i,Atom(p1)%Lab,trim(Atom(p2)%Lab)//trim(symcar(1)), &
                     trim(Atom(p3)%Lab)//trim(symcar(2)),ang_rest(i)%aobs,ang_rest(i)%sigma
             end do
          end if

          if (NP_Rest_Tor > 0) then
             write(unit=lun, fmt="(a)") " "
             write(unit=lun, fmt="(a,i5)") " => Torsion Angle Restraints relations: ",np_rest_tor
             write(unit=lun, fmt="(a)") " "
             write(unit=lun, fmt="(a)") &
             "   N.Rest   Atom1        Atom2: Symmetry_Op      Atom3: Symmetry_Op      Atom4: Symmetry_Op              Angle   Sigma"
             write(unit=lun, fmt="(a)") &
             " ======================================================================================================================"
             fmt5="(i7,tr5,a,t26,a,t34,a,t52,a,t75,f12.4,tr3,f7.4)"
             do i=1,np_rest_tor
                p1=tor_rest(i)%p(1)
                p2=tor_rest(i)%p(2)
                p3=tor_rest(i)%p(3)
                p4=tor_rest(i)%p(4)
                do j=1,3
                   n=0
                   tr=0.0
                   symcar(j)=" "
                   if (len_trim(Tor_rest(i)%stcode(j)) > 0) then
                      call Read_SymTrans_Code(Tor_rest(i)%stcode(j),n,tr)
                      taux=Spg%OP(n)%Mat(1:3,4)
                      Rot=Spg%OP(n)%Mat(1:3,1:3)
                      tr=tr+taux
                      call get_symSymb(Rot,Tr,symcar(j))
                   end if
                   symcar(j)=": "//symcar(j)
                end do
                write(unit=lun, fmt=fmt5) i,trim(Atom(p1)%Lab),trim(Atom(p2)%Lab)//trim(symcar(1)), &
                     trim(Atom(p3)%Lab)//trim(symcar(2)),trim(Atom(p4)%Lab)//trim(symcar(3)),       &
                     tor_rest(i)%tobs,tor_rest(i)%sigma
             end do
          end if
      End Select !Type
    End Subroutine Write_Info_RefCodes_FAtom

    !!--++
    !!--++ Module Subroutine Write_Info_RefCodes_Molcrys(MolCrys, iunit)
    !!--++    type(MolCrystal_Type), intent(in) :: molcrys
    !!--++    integer, optional,     intent(in) :: iunit
    !!--++
    !!--++ Overloaded
    !!--++ Write the Information about Refinement Codes
    !!--++
    !!--++ Update: March - 2005
    !!
    Module Subroutine Write_Info_RefCodes_Molcrys(MolCrys,iunit)
       !---- Arguments ----!
       type(MolCrystal_Type), intent(in) :: molcrys
       integer, optional,     intent(in) :: iunit

       !---- Local variables ----!
       character(len=20) :: car
       character(len=60) :: fmt1,fmt2
       integer           :: i,j,k,kk,n,na,np, lun
       real              :: mu

       fmt1="(t5,a,t16,i3,t27,a,t33,4(tr6,f8.4),tr8,i2,tr6,f8.3,i9)"
       fmt2="(t10,i3,t21,a,t31,a,t39,f8.4)"

       lun=6
       if (present(iunit)) lun=iunit

       if (NP_Refi > 0) then
          write(unit=lun, fmt="(a)") " "
          write(unit=lun, fmt="(a,i5)") " =>  Refinable Parameters: ",np_refi
          write(unit=lun, fmt="(a)") " "
          write(unit=lun, fmt="(a,a)")"    Name      N.Param   Code-Name       Value        L.Bound       ",&
                                      "U.Bound         Step    Condition   Multiplier    N.Atom"
          write(unit=lun, fmt="(a,a)")" ==================================================================",&
                                      "========================================================="

          Select Type (Atm => molcrys%atm)
            Type is (atm_ref_type)
               do i=1,Molcrys%N_Free
                  do j=1,3
                     if (Atm(i)%l_x(j) /=0) then
                        na=Atm(i)%l_x(j)
                        mu=Atm(i)%m_x(j)
                        car=trim(code_nam(j))//trim(Atm(i)%lab)
                        write(unit=lun,fmt=fmt1)  &
                             trim(car),na,trim(V_Name(na)),V_Vec(na),V_Bounds(:,na),V_BCon(na),mu,V_List(na)
                     end if
                  end do

                  if (Atm(i)%l_occ /=0) then
                     na=Atm(i)%l_occ
                     mu=Atm(i)%m_occ
                     car=trim(code_nam(5))//trim(Atm(i)%lab)
                     write(unit=lun,fmt=fmt1)  &
                          trim(car),na,trim(V_Name(na)),V_Vec(na),V_Bounds(:,na),V_BCon(na),mu,V_List(na)
                  end if

                  if (Atm(i)%l_u_iso /=0) then
                     na=Atm(i)%l_u_iso
                     mu=Atm(i)%m_u_iso
                     car=trim(code_nam(4))//trim(Atm(i)%lab)
                     write(unit=lun,fmt=fmt1)  &
                          trim(car),na,trim(V_Name(na)),V_Vec(na),V_Bounds(:,na),V_BCon(na),mu,V_List(na)
                  end if

                  do j=1,6
                     if (Atm(i)%l_u(j) /=0) then
                        na=Atm(i)%l_u(j)
                        mu=Atm(i)%m_u(j)
                        car=trim(code_nam(5+j))//trim(Atm(i)%lab)
                        write(unit=lun,fmt=fmt1)  &
                             trim(car),na,trim(V_Name(na)),V_Vec(na),V_Bounds(:,na),V_BCon(na),mu,V_List(na)
                     end if
                  end do
               end do
          End Select !Type

          do k=1,Molcrys%N_Mol

             do i=1,Molcrys%Mol(k)%natoms
                do j=1,3
                   if (Molcrys%Mol(k)%lI_coor(j,i) /=0) then
                      na=Molcrys%Mol(k)%lI_coor(j,i)
                      mu=Molcrys%Mol(k)%mI_coor(j,i)
                      car=trim(code_nam(j))//trim(molcrys%mol(k)%AtName(i))
                      write(unit=lun,fmt=fmt1)  &
                           trim(car),na,trim(V_Name(na)),V_Vec(na),V_Bounds(:,na),V_BCon(na),mu,V_List(na)
                   end if
                end do

                if (Molcrys%Mol(k)%locc(i) /=0) then
                   na=Molcrys%Mol(k)%locc(i)
                   mu=Molcrys%Mol(k)%mocc(i)
                   car=trim(code_nam(5))//trim(molcrys%mol(k)%AtName(i))
                   write(unit=lun,fmt=fmt1)  &
                        trim(car),na,trim(V_Name(na)),V_Vec(na),V_Bounds(:,na),V_BCon(na),mu,V_List(na)
                end if

                if (Molcrys%Mol(k)%lu_iso(i) /=0) then
                   na=Molcrys%Mol(k)%lu_iso(i)
                   mu=Molcrys%Mol(k)%mu_iso(i)
                   car=trim(code_nam(4))//trim(molcrys%mol(k)%AtName(i))
                   write(unit=lun,fmt=fmt1)  &
                        trim(car),na,trim(V_Name(na)),V_Vec(na),V_Bounds(:,na),V_BCon(na),mu,V_List(na)
                end if
             end do

             write(unit=lun, fmt="(a)") " "

             do j=1,3
                if (Molcrys%Mol(k)%lxcentre(j) /=0) then
                   na=Molcrys%Mol(k)%lxcentre(j)
                   mu=Molcrys%Mol(k)%mxcentre(j)
                   car=trim(code_nam(12+j))//"entre"
                   write(unit=lun,fmt=fmt1)  &
                        trim(car),na,trim(V_Name(na)),V_Vec(na),V_Bounds(:,na),V_BCon(na),mu,V_List(na)
                end if
             end do

             do j=1,3
                if (Molcrys%Mol(k)%lOrient(j) /=0) then
                   na=Molcrys%Mol(k)%lOrient(j)
                   mu=Molcrys%Mol(k)%mOrient(j)
                   car=trim(code_nam(15+j))//"Orient"
                   write(unit=lun,fmt=fmt1)  &
                        trim(car),na,trim(V_Name(na)),V_Vec(na),V_Bounds(:,na),V_BCon(na),mu,V_List(na)
                end if
             end do

             do j=1,6
                if (Molcrys%Mol(k)%lT_TLS(j) /=0) then
                   na=Molcrys%Mol(k)%lT_TLS(j)
                   mu=Molcrys%Mol(k)%mT_TLS(j)
                   car=trim(code_nam(19))
                   write(unit=lun,fmt=fmt1)  &
                        trim(car),na,trim(V_Name(na)),V_Vec(na),V_Bounds(:,na),V_BCon(na),mu,V_List(na)
                end if
             end do

             do j=1,6
                if (Molcrys%Mol(k)%lL_TLS(j) /=0) then
                   na=Molcrys%Mol(k)%lL_TLS(j)
                   mu=Molcrys%Mol(k)%mL_TLS(j)
                   car=trim(code_nam(10))
                   write(unit=lun,fmt=fmt1)  &
                        trim(car),na,trim(V_Name(na)),V_Vec(na),V_Bounds(:,na),V_BCon(na),mu,V_List(na)
                end if
             end do

             do i=1,3
                do j=1,3
                   if (Molcrys%Mol(k)%lS_TLS(i,j) /=0) then
                      na=Molcrys%Mol(k)%lS_TLS(i,j)
                      mu=Molcrys%Mol(k)%mS_TLS(i,j)
                      car=trim(code_nam(21))
                      write(unit=lun,fmt=fmt1)  &
                           trim(car),na,trim(V_Name(na)),V_Vec(na),V_Bounds(:,na),V_BCon(na),mu,V_List(na)
                   end if
                end do
             end do
          end do
       end if

       if (NP_Cons > 0) then
          write(unit=lun, fmt="(a)") " "
          write(unit=lun, fmt="(a,i5)") " => Constraints relations: ",np_cons
          write(unit=lun, fmt="(a)") " "
          write(unit=lun, fmt="(a)") "       N.Constr     Name      Father    Factor"
          write(unit=lun, fmt="(a)") "    ============================================="

          np=0

          Select Type( Atm => molcrys%Atm)
            Type is (atm_ref_type)
              do i=1,NP_Refi
                 n=0
                 do j=1,Molcrys%N_Free
                    n=n+count(Atm(j)%l_x ==i)
                    n=n+count(Atm(j)%l_u==i)
                    if (Atm(j)%l_occ==i) n=n+1
                    if (Atm(j)%l_u_iso==i) n=n+1
                 end do
                 if ( n > 1) then
                    do j=1,Molcrys%N_Free
                       do k=1,3
                          if (Atm(j)%l_x(k) == i) then
                             car=trim(code_nam(k))//trim(Atm(j)%lab)
                             if (trim(car)==trim(V_name(i))) cycle
                             np=np+1
                             write(unit=lun,fmt=fmt2)  &
                                  np, trim(car),trim(V_Name(i)),Atm(j)%m_x(k)
                          end if
                       end do

                       if (Atm(j)%l_u_iso == i) then
                          car=trim(code_nam(4))//trim(Atm(j)%lab)
                          if (trim(car)==trim(V_name(i))) cycle
                          np=np+1
                          write(unit=lun,fmt=fmt2)  &
                               np, trim(car), trim(V_Name(i)),Atm(j)%m_u_iso
                       end if

                       if (Atm(j)%l_occ == i) then
                          car=trim(code_nam(5))//trim(Atm(j)%lab)
                          if (trim(car)==trim(V_name(i))) cycle
                          np=np+1
                          write(unit=lun,fmt=fmt2)  &
                               np, trim(car), trim(V_Name(i)),Atm(j)%m_occ
                       end if

                       do k=1,6
                          if (Atm(j)%l_u(k) == i) then
                             car=trim(code_nam(5+k))//trim(Atm(j)%lab)
                             if (trim(car)==trim(V_name(i))) cycle
                             np=np+1
                             write(unit=lun,fmt=fmt2)  &
                                  np, trim(car), trim(V_Name(i)),Atm(j)%m_u(k)
                          end if
                       end do
                    end do
                 end if
              end do
          End Select !Type
          do i=1,NP_Refi
             do k=1,Molcrys%N_Mol
                n=count(molcrys%mol(k)%lxcentre  == i)
                n=n+count(molcrys%mol(k)%lorient == i)
                n=n+count(molcrys%mol(k)%lT_TLS  == i)
                n=n+count(molcrys%mol(k)%lL_TLS  == i)
                n=n+count(molcrys%mol(k)%lS_TLS  == i)
                if (n > 1) then
                   do j=1,3
                      if (molcrys%mol(k)%lxcentre(j) == i) then
                         car=trim(code_nam(12+j))//"entre"
                         if (trim(car)==trim(V_name(i))) cycle
                         np=np+1
                         write(unit=lun,fmt=fmt2)  &
                              np, trim(car), trim(V_Name(i)),molcrys%mol(k)%mxcentre(j)
                      end if
                   end do

                   do j=1,3
                      if (molcrys%mol(k)%lOrient(j) == i) then
                         car=trim(code_nam(15+j))//"Orient"
                         if (trim(car)==trim(V_name(i))) cycle
                         np=np+1
                         write(unit=lun,fmt=fmt2)  &
                              np, trim(car), trim(V_Name(i)),molcrys%mol(k)%morient(j)
                      end if
                   end do

                   do j=1,6
                      if (molcrys%mol(k)%lT_TLS(j) == i) then
                         car=trim(code_nam(19))
                         if (trim(car)==trim(V_name(i))) cycle
                         np=np+1
                         write(unit=lun,fmt=fmt2)  &
                              np,trim(car), trim(V_Name(i)),molcrys%mol(k)%mT_TLS(j)
                      end if
                   end do

                   do j=1,6
                      if (molcrys%mol(k)%lL_TLS(j) == i) then
                         car=trim(code_nam(20))
                         if (trim(car)==trim(V_name(i))) cycle
                         np=np+1
                         write(unit=lun,fmt=fmt2)  &
                              np, trim(car), trim(V_Name(i)),molcrys%mol(k)%mL_TLS(j)
                      end if
                   end do

                   do j=1,3
                      do kk=1,3
                         if (molcrys%mol(k)%lS_TLS(j,kk) == i) then
                            car=trim(code_nam(21))
                            if (trim(car)==trim(V_name(i))) cycle
                            np=np+1
                            write(unit=lun,fmt=fmt2)  &
                                 np,trim(car),trim(V_Name(i)),molcrys%mol(k)%mS_TLS(j,kk)
                         end if
                      end do
                   end do

                end if
             end do
          end do
       end if
    End Subroutine Write_Info_RefCodes_Molcrys
    !!--++
    !!--++ Module Subroutine Write_Info_RefCodes_Molec(Molec, iunit)
    !!--++    type(molecule_type), intent(in) :: molec
    !!--++    integer, optional,   intent(in) :: iunit
    !!--++
    !!--++ Overloaded
    !!--++ Write the Information about Refinement Codes
    !!--++
    !!--++ Update: March - 2005
    !!
    Module Subroutine Write_Info_RefCodes_Molec(Molec,iunit)
       !---- Arguments ----!
       type(molecule_type), intent(in) :: molec
       integer, optional,   intent(in) :: iunit

       !---- Local variables ----!
       character(len=60) :: fmt1,fmt2
       character(len=20) :: car
       integer           :: i,j,k,n,na,np,lun
       real              :: mu

       fmt1="(t5,a,t16,i3,t27,a,t33,4(tr6,f8.4),tr8,i2,tr6,f8.3,i9)"
       fmt2="(t10,i3,t21,a,t31,a,t39,f8.4)"

       lun=6
       if (present(iunit)) lun=iunit

       if (NP_Refi > 0) then
          write(unit=lun, fmt="(a)") " "
          write(unit=lun, fmt="(a,i5)") " => Refinable Parameters: ",np_refi
          write(unit=lun, fmt="(a)") " "
          write(unit=lun, fmt="(a)") " "
          write(unit=lun, fmt="(a,a)")"    Name      N.Param   Code-Name       Value        L.Bound       ",&
                                      "U.Bound         Step    Condition   Multiplier    N.Atom"
          write(unit=lun, fmt="(a,a)")" ==================================================================",&
                                      "========================================================="

          do i=1,Molec%natoms
             do j=1,3
                if (molec%lI_coor(j,i) /=0) then
                   na=molec%lI_coor(j,i)
                   mu=molec%mI_coor(j,i)
                   car=trim(code_nam(j))//trim(molec%AtName(i))
                   write(unit=lun,fmt=fmt1)  &
                        trim(car),na,trim(V_Name(na)),V_Vec(na),V_Bounds(:,na),V_BCon(na),mu,V_List(na)
                end if
             end do

             if (molec%locc(i) /=0) then
                na=molec%locc(i)
                mu=molec%mocc(i)
                car=trim(code_nam(4))//trim(molec%AtName(i))
                write(unit=lun,fmt=fmt1)  &
                     trim(car),na,trim(V_Name(na)),V_Vec(na),V_Bounds(:,na),V_BCon(na),mu,V_List(na)
             end if

             if (molec%lu_iso(i) /=0) then
                na=molec%lu_iso(i)
                mu=molec%mu_iso(i)
                car=trim(code_nam(5))//trim(molec%AtName(i))
                write(unit=lun,fmt=fmt1)  &
                     trim(car),na,trim(V_Name(na)),V_Vec(na),V_Bounds(:,na),V_BCon(na),mu,V_List(na)
             end if
          end do

          write(unit=lun, fmt="(a)") " "

          do j=1,3
             if (molec%lxcentre(j) /=0) then
                na=molec%lxcentre(j)
                mu=molec%mxcentre(j)
                car=trim(code_nam(12+j))//"entre"
                write(unit=lun,fmt=fmt1)  &
                     trim(car),na,trim(V_Name(na)),V_Vec(na),V_Bounds(:,na),V_BCon(na),mu,V_List(na)
             end if
          end do

          do j=1,3
             if (molec%lOrient(j) /=0) then
                na=molec%lOrient(j)
                mu=molec%mOrient(j)
                car=trim(code_nam(15+j))//"Orient"
                write(unit=lun,fmt=fmt1)  &
                     trim(car),na,trim(V_Name(na)),V_Vec(na),V_Bounds(:,na),V_BCon(na),mu,V_List(na)
             end if
          end do

          do j=1,6
             if (molec%lT_TLS(j) /=0) then
                na=molec%lT_TLS(j)
                mu=molec%mT_TLS(i)
                car=trim(code_nam(19))
                write(unit=lun,fmt=fmt1)  &
                     trim(car),na,trim(V_Name(na)),V_Vec(na),V_Bounds(:,na),V_BCon(na),mu,V_List(na)
             end if
          end do

          do j=1,6
             if (molec%lL_TLS(j) /=0) then
                na=molec%lL_TLS(j)
                mu=molec%mL_TLS(i)
                car=trim(code_nam(20))
                write(unit=lun,fmt=fmt1)  &
                     trim(car),na,trim(V_Name(na)),V_Vec(na),V_Bounds(:,na),V_BCon(na),mu,V_List(na)
             end if
          end do

          do i=1,3
             do j=1,3
                if (molec%lS_TLS(i,j) /=0) then
                   na=molec%lS_TLS(i,j)
                   mu=molec%mS_TLS(i,j)
                   car=trim(code_nam(21))
                   write(unit=lun,fmt=fmt1)  &
                        trim(car),na,trim(V_Name(na)),V_Vec(na),V_Bounds(:,na),V_BCon(na),mu,V_List(na)
                end if
             end do
          end do
       end if

       if (NP_Cons > 0) then
          write(unit=lun, fmt="(a)") " "
          write(unit=lun, fmt="(a,i5)") " => Constraints relations: ",np_cons
          write(unit=lun, fmt="(a)") " "
          write(unit=lun, fmt="(a)") "       N.Constr     Name      Father    Factor"
          write(unit=lun, fmt="(a)") "    ============================================="

          np=0
          do i=1,NP_Refi
             n=0
             do j=1,Molec%natoms
                n=n+count(molec%lI_coor(:,j) ==i)
                if (molec%locc(j)==i) n=n+1
                if (molec%lu_iso(j)==i) n=n+1
             end do
             if ( n > 1) then
                do j=1,Molec%natoms
                   do k=1,3
                      if (molec%lI_coor(k,j) == i) then
                         car=trim(code_nam(k))//trim(molec%AtName(j))
                         if (trim(car)==trim(V_Name(i))) cycle
                         np=np+1
                         write(unit=lun,fmt=fmt2)  np, trim(car),trim(V_Name(i)),molec%mI_coor(k,j)
                      end if
                   end do

                   if (molec%lu_iso(j) == i) then
                      car=trim(code_nam(5))//trim(molec%AtName(j))
                      if (trim(car)==trim(V_Name(i))) cycle
                      np=np+1
                      write(unit=lun,fmt=fmt2) np, trim(car), trim(V_Name(i)),molec%mu_iso(j)
                   end if

                   if (molec%locc(j) == i) then
                      car=trim(code_nam(4))//trim(molec%AtName(j))
                      if (trim(car)==trim(V_Name(i))) cycle
                      np=np+1
                      write(unit=lun,fmt=fmt2)  np, trim(car),trim(V_Name(i)),molec%mocc(j)
                   end if
                end do
             end if
          end do

          do i=1,NP_Refi
             n=count(molec%lxcentre  == i)
             n=n+count(molec%lorient == i)
             n=n+count(molec%lT_TLS  == i)
             n=n+count(molec%lL_TLS  == i)
             n=n+count(molec%lS_TLS  == i)
             if (n > 1) then
                do j=1,3
                   if (molec%lxcentre(j) == i) then
                      car=trim(code_nam(12+j))//"entre"
                      if (trim(car)==trim(V_Name(i))) cycle
                      np=np+1
                      write(unit=lun,fmt=fmt2)  np, trim(car),trim(V_Name(i)),molec%mxcentre(j)
                   end if
                end do

                do j=1,3
                   if (molec%lOrient(j) == i) then
                      car=trim(code_nam(15+j))//"Orient"
                      if (trim(car)==trim(V_Name(i))) cycle
                      np=np+1
                      write(unit=lun,fmt=fmt2)  np,trim(car),trim(V_Name(i)),molec%morient(j)
                   end if
                end do

                do j=1,6
                   if (molec%lT_TLS(j) == i) then
                      car=trim(code_nam(19))
                      if (trim(car)==trim(V_Name(i))) cycle
                      np=np+1
                      write(unit=lun,fmt=fmt2) np,trim(car), trim(V_Name(i)),molec%mT_TLS(j)
                  end if
                end do

                do j=1,6
                   if (molec%lL_TLS(j) == i) then
                      car=trim(code_nam(20))
                      if (trim(car)==trim(V_Name(i))) cycle
                      np=np+1
                      write(unit=lun,fmt=fmt2) np,trim(car), trim(V_Name(i)),molec%mL_TLS(j)
                   end if
                end do

                do j=1,3
                   do k=1,3
                      if (molec%lS_TLS(j,k) == i) then
                         car=trim(code_nam(21))
                         if (trim(car)==trim(V_Name(i))) cycle
                         np=np+1
                         write(unit=lun,fmt=fmt2) np,trim(car), trim(V_Name(i)),molec%mS_TLS(j,k)
                      end if
                   end do
                end do
             end if
          end do

       end if
    End Subroutine Write_Info_RefCodes_Molec

    !!--++
    !!--++ Module Subroutine Write_Info_RefCodes_MagStr(FmAtom, Mag_dom, MGp, Iunit)
    !!--++    type(mAtom_List_Type),               intent(in) :: FmAtom
    !!--++    type(Magnetic_Domain_type),optional, intent(in) :: Mag_dom
    !!--++    type(MagSymm_k_Type),   intent(in)    :: MGp
    !!--++    integer, optional,      intent(in)    :: Iunit
    !!--++
    !!--++ Write the Information about the Magnetic Refinement Codes
    !!--++    magnetic domains
    !!--++ Created: February - 2012
    !!
    Module Subroutine Write_Info_RefCodes_MagStr(FmAtom, Mag_dom, MGp, Iunit)
       !---- Arguments ----!
       type(mAtom_List_Type),               intent(in) :: FmAtom
       type(Magnetic_Domain_type),optional, intent(in) :: Mag_dom
       type(MagSymm_k_Type),      intent(in) :: MGp
       integer, optional,         intent(in) :: Iunit

       !---- Local variables ----!
       character(len=20)              :: car
       character(len=60)              :: fmt1,fmt2
       integer                        :: i,j,na,lun,ik,ich
       real(kind=cp)                  :: mu

       !---- Format Zone ----!
       fmt1="(t5,a,t16,i3,t27,a,t33,4(tr6,f8.4),tr8,i2,tr6,f8.3,i9)"
       fmt2="(t10,i3,t21,a,t31,a,t39,f8.4)"
       lun=6
       if (present(iunit)) lun=iunit

       if (NP_Refi > 0) then
          write(unit=lun, fmt="(a)") " "
          write(unit=lun, fmt="(a,i5)") " => Refinable Parameters: ",np_refi
          write(unit=lun, fmt="(a)") " "
          write(unit=lun, fmt="(a,a)")"    Name      N.Param   Code-Name       Value        L.Bound       ",&
                                      "U.Bound         Step    Condition   Multiplier    N.Atom"
          write(unit=lun, fmt="(a,a)")" ==================================================================",&
                                      "========================================================="

          do i=1,FmAtom%natoms
             !---- Get im, number of the magnetic matrices/irrep set
             ik=FmAtom%atom(i)%nvk

             !----Real components
             do j=1,3
                if (FmAtom%atom(i)%lSkR(j,ik) /= 0) then
                   na=FmAtom%atom(i)%lSkR(j,ik)
                   mu=FmAtom%atom(i)%mSkR(j,ik)
                 if(MGp%Sk_type == "Spherical_Frame") then
                   car=trim(mcode_nam(j+6))//trim(FmAtom%atom(i)%lab)
                 else
                   car=trim(mcode_nam(j  ))//trim(FmAtom%atom(i)%lab)
                 end if
                   write(unit=lun,fmt=fmt1)  &
                        trim(car),na,trim(V_Name(na)),V_Vec(na),V_Bounds(:,na),V_BCon(na),mu,V_List(na)

                end if
             end do

             !----Imaginary components
             do j=1,3
                if (FmAtom%atom(i)%lSkI(j,ik) /= 0) then
                   na=FmAtom%atom(i)%lSkI(j,ik)
                   mu=FmAtom%atom(i)%mSkI(j,ik)
                 if(MGp%Sk_type == "Spherical_Frame") then
                   car=trim(mcode_nam(j+9))//trim(FmAtom%atom(i)%lab)
                 else
                   car=trim(mcode_nam(j+3))//trim(FmAtom%atom(i)%lab)
                 end if
                   write(unit=lun,fmt=fmt1)  &
                        trim(car),na,trim(V_Name(na)),V_Vec(na),V_Bounds(:,na),V_BCon(na),mu,V_List(na)
                end if
             end do

             if (FmAtom%atom(i)%lmphas(ik) /=0) then
                na=FmAtom%atom(i)%lmphas(ik)
                mu=FmAtom%atom(i)%mmphas(ik)
                car=trim(mcode_nam(13))//trim(FmAtom%atom(i)%lab)
                write(unit=lun,fmt=fmt1)  &
                     trim(car),na,trim(V_Name(na)),V_Vec(na),V_Bounds(:,na),V_BCon(na),mu,V_List(na)
             end if
          end do

          !----C1-12 coefficients
          do i=1,FmAtom%natoms
             do j=1,12
                if (FmAtom%atom(i)%lbas(j,ik) /= 0) then
                   na=FmAtom%atom(i)%lbas(j,ik)
                   mu=FmAtom%atom(i)%mbas(j,ik)
                   car=trim(mcode_nam(j+13))//trim(FmAtom%atom(i)%lab)
                   write(unit=lun,fmt=fmt1)  &
                        trim(car),na,trim(V_Name(na)),V_Vec(na),V_Bounds(:,na),V_BCon(na),mu,V_List(na)
                end if
             end do
          end do

          !---- Check is chirality is present ----!
          if(present(mag_Dom)) then
            if (Mag_Dom%chir) then
             ich=2
            else
             ich=1
            end if

            do i=1,Mag_Dom%nd
               do j=1,ich
                  if (Mag_Dom%Lpop(j,i) /= 0) then
                     na=Mag_Dom%Lpop(j,i)
                     mu=Mag_Dom%Mpop(j,i)
                     car=trim(Mag_Dom%Lab(j,i))
                     write(unit=lun,fmt=fmt1)  &
                          trim(car),na,trim(V_Name(na)),V_Vec(na),V_Bounds(:,na),V_BCon(na),mu,V_List(na)
                  end if
               end do
            end do
          end if
       end if
    End Subroutine Write_Info_RefCodes_MagStr

    !!----
    !!---- Module Subroutine Write_Info_RefGCodes(model, Iunit)
    !!----    type(Nonatomic_Parameter_List_Type), intent(in) :: model
    !!----    integer, optional,                   intent(in) :: Iunit
    !!----
    !!----    Write the Information about Refinement Codes of non-atomic parameters
    !!----
    !!---- Update: November 2 - 2013
    !!
    Module Subroutine Write_Info_RefGCodes(model, Iunit)
       !---- Arguments ----!
       type(Nonatomic_Parameter_List_Type), intent(in) :: model
       integer, optional,                   intent(in) :: Iunit

       !---- Local variables ----!
       character(len=20)              :: car
       character(len=60)              :: fmt1,fmt2 !,fmt3,fmt4,fmt5
       !Character(len=25),dimension(3) :: symcar
       integer                        :: i,na,lun !,n,j,k,np,p1,p2,p3,p4
       real(kind=cp)                  :: mu
       !real(kind=cp),dimension(3)     :: tr

       !---- Format Zone ----!
       fmt1="(t5,a,t16,i3,t27,a,t33,4(tr6,f8.4),tr8,i2,tr6,f8.3,i9)"
       fmt2="(t10,i3,t21,a,t31,a,t39,f8.4)"

       lun=6
       if (present(iunit)) lun=iunit

       if (NP_Refi > 0) then
          write(unit=lun, fmt="(a)") " "
          write(unit=lun, fmt="(a,i5)") " => Non-atomic refinable Parameters: ",model%npar
          write(unit=lun, fmt="(a)") " "
          write(unit=lun, fmt="(a,a)")"    Name      N.Param   Code-Name       Value        L.Bound       ",&
                                      "U.Bound         Step    Condition   Multiplier   Order in Model"
          write(unit=lun, fmt="(a,a)")" ==================================================================",&
                                      "==========================================================="
          do i=1,model%npar
             if (model%par(i)%lcode > 0) then
                na=model%par(i)%lcode
                mu=model%par(i)%multip
                car=trim(model%par(i)%nam)
                write(unit=lun,fmt=fmt1)  &
                     trim(car),na,trim(V_Name(na)),V_Vec(na),V_Bounds(:,na),V_BCon(na),mu,V_List(na)
                !write(unit=*,fmt="(2i6,a)") i,na, "    "//trim(car)
             !else
                !write(unit=*,fmt="(2i6,a)") i,model%par(i)%lcode, "    "//trim(model%par(i)%nam)
             end if
          end do
       end if
    End Subroutine Write_Info_RefGCodes

    !!----
    !!---- Module Subroutine Write_Info_RefParams(iunit)
    !!----    integer, optional,  intent(in) :: iunit
    !!----
    !!----    Write the Information about Refinement parameters in file associated with
    !!----    logical unit "iunit". If no argument is passed the standard output (iunit=6)
    !!----    is used.
    !!----
    !!---- Update: August - 2007
    !!
    Module Subroutine Write_Info_RefParams(iunit)
       !---- Arguments ----!
       integer, optional,   intent(in) :: iunit

       !---- Local variables ----!
       integer           :: i,lun

       lun=6
       if (present(iunit)) lun=iunit

       if (NP_Refi > 0) then
          write(unit=lun, fmt="(a)") " "
          write(unit=lun, fmt="(a,i5)") " => Number of Refinable Parameters: ",np_refi
          write(unit=lun, fmt="(a)") " "
          do i=1,nP_refi
            write(unit=lun,fmt="(i6,tr5,a20,4f14.5, i4, i6)") i,V_Name(i),V_Vec(i),V_Bounds(:,i),V_BCon(i),V_List(i)
          end do
       end if
    End Subroutine Write_Info_RefParams

    !!----
    !!---- Module Subroutine Write_Restraints_ObsCalc(A,iunit)
    !!----    type(AtList_Type),   intent(in) :: A
    !!----    integer, optional,   intent(in) :: iunit
    !!----
    !!----    Write the current values of the "observed" and calculated
    !!----    restraints, as well as the corresponding cost value.
    !!----
    !!---- Update: April - 2005
    !!
    Module Subroutine Write_Restraints_ObsCalc(A,iunit)
       !---- Arguments ----!
       type(AtList_Type),   intent(in) :: A
       integer, optional,   intent(in) :: iunit

       !---- Local variables ----!
       character(len=14) :: car1,car2,car3,car4
       integer           :: i,i1,i2,i3,i4,lun
       real              :: disto,distc,ango,angc,sigm, cost,w, delta

       lun=6
       if (present(iunit)) lun=iunit

       if( NP_Rest_Dis > 0) then
          Write(unit=lun,fmt="(/,a)") " ============================================================"
          Write(unit=lun,fmt="(a)")   "   Distance Restraints: Atoms, Dobs, Dcalc, Sigma, delt/Sigma"
          Write(unit=lun,fmt="(a,/)") " ============================================================"
          Write(unit=lun,fmt="(a)") " Rest#    Atom1         Atom2              Dobs        Dcalc       Sigma   (Do-Dc)/Sigma"
          cost=0.0
          do i=1,NP_Rest_Dis
             i1=Dis_rest(i)%p(1)
             i2=Dis_rest(i)%p(2)
             car1=trim(A%Atom(i1)%lab)
             car2=trim(A%Atom(i2)%lab)//dis_rest(i)%stcode
             disto=Dis_rest(i)%dobs
             distc=Dis_rest(i)%dcalc
             delta=disto-distc
             sigm=Dis_rest(i)%sigma
             w= 1.0/(sigm*sigm)
             cost= cost+delta*delta*w
             Write(unit=lun,fmt="(i6,tr4,2a,4f12.5)") i,car1,car2,disto,distc,sigm,delta/sigm
          end do

          Write(unit=lun,fmt="(/,a,f12.5)") "   Distance Restraints Cost = Sum{[(dobs-dcalc)/Sigma]^2} = ",cost
       end if


       if( NP_Rest_Ang > 0) then
          Write(unit=lun,fmt="(/,a)") " ============================================================="
          Write(unit=lun,fmt="(a)")   "   Angle Restraints: Atoms, Angobs, Angcalc, Sigma, delt/Sigma"
          Write(unit=lun,fmt="(a,/)") " ============================================================="
          Write(unit=lun,fmt="(a)") &
          " Rest#    Atom1         Atom2          Atom3            Ang_obs    Ang_calc      Sigma   (Ao-Ac)/Sigma"

          cost=0.0
          do i=1,NP_Rest_Ang
             i1=Ang_rest(i)%p(1)
             i2=Ang_rest(i)%p(2)
             i3=Ang_rest(i)%p(3)
             car1=trim(A%Atom(i1)%lab)
             car2=trim(A%Atom(i2)%lab)//ang_rest(i)%stcode(1)
             car3=trim(A%Atom(i3)%lab)//ang_rest(i)%stcode(2)
             ango=Ang_rest(i)%Aobs
             angc=Ang_rest(i)%Acalc
             delta=ango-angc
             sigm=Ang_rest(i)%sigma
             w= 1.0/(sigm*sigm)
             cost= cost+delta*delta*w
             Write(unit=lun,fmt="(i6,tr4,3a,4f12.5)") i,car1,car2,car3,ango,angc,sigm,delta/sigm
          end do

          Write(unit=lun,fmt="(/,a,f12.5)") "   Angle Restraints Cost = Sum{[(Ang_obs-Ang_calc)/Sigma]^2} = ",cost
       End If



       if( NP_Rest_tor > 0) then
          Write(unit=lun,fmt="(/,a)") " ====================================================================="
          Write(unit=lun,fmt="(a)")   "   Torsion Angle Restraints: Atoms, Angobs, Angcalc, Sigma, delt/Sigma"
          Write(unit=lun,fmt="(a,/)") " ====================================================================="
          Write(unit=lun,fmt="(a)") " Rest#    Atom1         Atom2          Atom3          Atom4            "//&
               "Ang_obs    Ang_calc      Sigma   (Ao-Ac)/Sigma"

          cost=0.0
          do i=1,NP_Rest_tor
             i1=Tor_rest(i)%p(1)
             i2=Tor_rest(i)%p(2)
             i3=Tor_rest(i)%p(3)
             i4=Tor_rest(i)%p(4)
             car1=trim(A%Atom(i1)%lab)
             car2=trim(A%Atom(i2)%lab)//tor_rest(i)%stcode(1)
             car3=trim(A%Atom(i3)%lab)//tor_rest(i)%stcode(2)
             car4=trim(A%Atom(i4)%lab)//tor_rest(i)%stcode(3)
             ango=Ang_rest(i)%Aobs
             angc=Ang_rest(i)%Acalc
             delta=ango-angc
             sigm=Ang_rest(i)%sigma
             w= 1.0/(sigm*sigm)
             cost= cost+delta*delta*w
             Write(unit=lun,fmt="(i6,tr4,4a,4f12.5)") i,car1,car2,car3,car4,ango,angc,sigm,delta/sigm
          end do

          Write(unit=lun,fmt="(/,a,f12.5)") "   Torsion Angle Restraints Cost = Sum{[(Ang_obs-Ang_calc)/Sigma]^2} = ",cost
       End If
    End Subroutine Write_Restraints_ObsCalc

End Submodule KWC_WriteRefCodes
