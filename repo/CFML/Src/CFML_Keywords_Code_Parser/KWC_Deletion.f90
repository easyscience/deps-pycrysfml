Submodule (CFML_Keywords_Code_Parser) KWC_Deletion
   !---- Variables ----!
   implicit none

 Contains

    !!--++
    !!--++ Module Subroutine Delete_RefCodes(N, FAtom/FmAtom/MolCrys/Molec/Mag_Dom)
    !!--++    integer,                      intent(in)     :: N
    !!--++    type(AtList_Type),            intent(in out) :: FAtom
    !!--++    or
    !!--++    type(mAtom_List_Type),        intent(in out) :: FmAtom
    !!--++    or
    !!--++    type(MolCrystal_Type), intent(in out) :: MolCrys
    !!--++    or
    !!--++    type(molecule_type),          intent(in out) :: Molec
    !!--++    or
    !!--++    type(Magnetic_Domain_type),intent(in out) :: Mag_Dom
    !!--++
    !!--++    (Private)
    !!--++    Delete the number of Refinable Parameter (N) on the list
    !!--++
    !!--++ Update: March - 2005
    !!

    !!--++
    !!--++ Module Subroutine Delete_RefCodes_FAtom(N, FAtom)
    !!--++    integer,              intent(in)     :: N
    !!--++    type(AtList_Type),    intent(in out) :: FAtom
    !!--++
    !!--++ Overloaded
    !!--++ Delete the number of Refinable Parameter (N) on the list
    !!--++
    !!--++ Update: March - 2005
    !!
    Module Subroutine Delete_RefCodes_FAtom(N, FAtom)
       !---- Arguments ----!
       integer,              intent(in)     :: N
       type(AtList_Type),    intent(in out) :: FAtom

       !---- Local Variables ----!
       logical :: deleted
       integer :: i,j

       deleted=.false.

       select type (Atm => FAtom%atom)

       type is (atm_ref_type)

          !---- Eliminate N Parameter ----!
          do i=1,FAtom%natoms
             do j=1,3
                if (Atm(i)%l_x(j) == N) then
                    Atm(i)%l_x(j)=0
                    Atm(i)%m_x(j)=0.0
                    deleted=.true.
                end if
             end do

             do j=1,3
                if (Atm(i)%l_moment(j) == N) then
                    Atm(i)%l_moment(j) = 0
                    Atm(i)%m_moment(j) = 0.0
                    deleted = .true.
                end if
             end do

             if (Atm(i)%l_u_iso == N) then
                 Atm(i)%l_u_iso=0
                 Atm(i)%m_u_iso=0.0
                 deleted=.true.
             end if

             if (Atm(i)%l_occ == N) then
                 Atm(i)%l_occ=0
                 Atm(i)%m_occ=0.0
                 deleted=.true.
             end if

             do j=1,6
                if (Atm(i)%l_u(j) == N) then
                    Atm(i)%l_u(j)=0
                    Atm(i)%m_u(j)=0.0
                    deleted=.true.
                end if
             end do
          end do

          !---- Updating Variables ----!
          do i=1,FAtom%natoms
             do j=1,3
                if (Atm(i)%l_x(j) > N) then
                   Atm(i)%l_x(j)=Atm(i)%l_x(j)-1
                end if
             end do

             do j=1,3
                if (Atm(i)%l_moment(j) > N) then
                   Atm(i)%l_moment(j)=Atm(i)%l_moment(j)-1
                end if
             end do

             if (Atm(i)%l_u_iso > N) then
                Atm(i)%l_u_iso=Atm(i)%l_u_iso-1
             end if

             if (Atm(i)%l_occ > N) then
                Atm(i)%l_occ=Atm(i)%l_occ-1
             end if

             do j=1,6
                if (Atm(i)%l_u(j) > N) then
                   Atm(i)%l_u(j)=Atm(i)%l_u(j)-1
                end if
             end do
          end do
       End Select
       !---- Updating V_Vectors ----!
       if (deleted) call Delete_element_in_Varrays(N)
    End Subroutine Delete_RefCodes_FAtom

    !!--++
    !!--++ Subroutine Delete_RefCodes_FmAtom(N, FmAtom)
    !!--++    integer,              intent(in)     :: N
    !!--++    type(mAtom_List_Type),intent(in out) :: FmAtom
    !!--++
    !!--++ Delete the number of Refinable Parameters (N) on the list
    !!--++ magnetic clone of Delete_RefCodes_FAtom
    !!--++ Created: December - 2011
    !!
    Module Subroutine Delete_RefCodes_FmAtom(N, FmAtom)
       !---- Arguments ----!
       integer,               intent(in)     :: N
       type(mAtom_List_Type), intent(in out) :: FmAtom

       !---- Local Variables ----!
       logical :: deleted
       integer :: i,j,k,ik

       deleted=.false.

       !---- Eliminate N Parameter ----!
       do i=1,FmAtom%natoms
          ik=FmAtom%atom(i)%nvk
          do j=1,3
             do k=1,ik
                if (FmAtom%atom(i)%lSkR(j,k) == N) then
                    FmAtom%atom(i)%mSkR(j,k)=0.0
                    FmAtom%atom(i)%lskr(j,k)=0
                    deleted=.true.
                end if
             end do
          end do

          do j=1,3
             do k=1,ik
                if (FmAtom%atom(i)%lSkI(j,k) == N) then
                    FmAtom%atom(i)%mSkI(j,k)=0.0
                    FmAtom%atom(i)%lski(j,k)=0
                    deleted=.true.
                end if
             end do
          end do

          do k=1,ik
             if (FmAtom%atom(i)%lmphas(k) == N) then
                 FmAtom%atom(i)%mmphas(k)=0.0
                 FmAtom%atom(i)%lmphas(k)=0
                 deleted=.true.
             end if
          end do

          do j=1,12
             do k=1,ik
                if (FmAtom%atom(i)%lbas(j,k) == N) then
                    FmAtom%atom(i)%mbas(j,k)=0.0
                    FmAtom%atom(i)%lbas(j,k)=0
                    deleted=.true.
                end if
           end do
          end do
       end do

       !---- Updating Variables ----!
       do i=1,FmAtom%natoms
          do j=1,3
             do k=1,ik
                if (FmAtom%atom(i)%lSkR(j,k) > N) then
                    FmAtom%atom(i)%lSkR(j,k)=FmAtom%atom(i)%lSkR(j,k)-1
                end if
             end do
          end do

          do j=1,3
             do k=1,ik
                if (FmAtom%atom(i)%lSkI(j,k) > N) then
                    FmAtom%atom(i)%lSkI(j,k)=FmAtom%atom(i)%lSkI(j,k)-1
                end if
             end do
          end do

          do k=1,ik
             if (FmAtom%atom(i)%lmphas(k) > N) then
                 FmAtom%atom(i)%lmphas(k)=FmAtom%atom(i)%lmphas(k)-1
             end if
          end do

          do j=1,12
             do k=1,ik
                if (FmAtom%atom(i)%lbas(j,k) > N) then
                    FmAtom%atom(i)%lbas(j,k)=FmAtom%atom(i)%lbas(j,k)-1
                end if
             end do
          end do
       end do

       !---- Updating V_Vectors ----!
       if (deleted) call Delete_element_in_Varrays(N)
    End Subroutine Delete_RefCodes_FmAtom

    !!--++
    !!--++ Module Subroutine Delete_RefCodes_MolCrys(N,MolCrys)
    !!--++    integer,                      intent(in)     :: N
    !!--++    type(MolCrystal_Type), intent(in out) :: MolCrys
    !!--++
    !!--++ Overloaded
    !!--++ Delete the number of Refinable Parameter (N) on the list
    !!--++
    !!--++ Update: March - 2005
    !!
    Module Subroutine Delete_RefCodes_MolCrys(N,Molcrys)
       !---- Arguments ----!
       integer,               intent(in)     :: N
       type(MolCrystal_Type), intent(in out) :: MolCrys

       !---- Local Variables ----!
       logical :: deleted
       integer :: i,j,k

       deleted=.false.

       if (MolCrys%N_Free > 0 ) then
        Select Type (Atm => MolCrys%Atm)
          Type is (atm_ref_type)
             do i=1,MolCrys%N_Free
                do j=1,3
                   if (Atm(i)%l_x(j) == N) then
                       Atm(i)%l_x(j)=0
                       Atm(i)%m_x(j)=0.0
                       deleted=.true.
                   end if
                end do

                if (Atm(i)%l_u_iso == N) then
                    Atm(i)%l_u_iso=0
                    Atm(i)%m_u_iso=0.0
                    deleted=.true.
                end if

                if (Atm(i)%l_occ == N) then
                    Atm(i)%l_occ=0
                    Atm(i)%m_occ=0.0
                    deleted=.true.
                end if

                do j=1,6
                   if (Atm(i)%l_u(j) == N) then
                       Atm(i)%l_u(j)=0
                       Atm(i)%m_u(j)=0.0
                       deleted=.true.
                   end if
                end do
             end do

             do i=1,MolCrys%N_Free
                do j=1,3
                   if (Atm(i)%l_x(j) > N) then
                       Atm(i)%l_x(j) = Atm(i)%l_x(j)-1
                   end if
                end do

                if (Atm(i)%l_u_iso > N) then
                    Atm(i)%l_u_iso=Atm(i)%l_u_iso-1
                end if

                if (Atm(i)%l_occ > N) then
                    Atm(i)%l_occ= Atm(i)%l_occ-1
                end if

                do j=1,6
                   if (Atm(i)%l_u(j) > N) then
                       Atm(i)%l_u(j)= Atm(i)%l_u(j)-1
                   end if
                end do
             end do
         End Select
       end if

       if (MolCrys%N_Mol > 0 ) then

          do k=1,MolCrys%N_Mol
             do j=1,3
                if (Molcrys%Mol(k)%lxcentre(j) == N) then
                    Molcrys%Mol(k)%lxcentre(j)=0
                    Molcrys%Mol(k)%mxcentre(j)=0.0
                    deleted=.true.
                end if
             end do

             do j=1,3
                if (Molcrys%Mol(k)%lOrient(j) == N) then
                    Molcrys%Mol(k)%lOrient(j)=0
                    Molcrys%Mol(k)%mOrient(j)=0.0
                    deleted=.true.
                end if
             end do

             do j=1,6
                if (Molcrys%Mol(k)%lT_TLS(j) == N) then
                    Molcrys%Mol(k)%lT_TLS(j)=0
                    Molcrys%Mol(k)%mT_TLS(j)=0.0
                    deleted=.true.
                end if
             end do

             do j=1,6
                if (Molcrys%Mol(k)%lL_TLS(j) == N) then
                    Molcrys%Mol(k)%lL_TLS(j)=0
                    Molcrys%Mol(k)%mL_TLS(j)=0.0
                    deleted=.true.
                end if
             end do

             do i=1,3
                do j=1,3
                   if (Molcrys%Mol(k)%lS_TLS(i,j) == N) then
                       Molcrys%Mol(k)%lS_TLS(i,j)=0
                       Molcrys%Mol(k)%mS_TLS(i,j)=0.0
                       deleted=.true.
                   end if
                end do
             end do

             !---- Updating ----!
             do j=1,3
                if (Molcrys%Mol(k)%lxcentre(j) > N) then
                    Molcrys%Mol(k)%lxcentre(j)=Molcrys%Mol(k)%lxcentre(j)-1
                end if
             end do

             do j=1,3
                if (Molcrys%Mol(k)%lOrient(j) > N) then
                    Molcrys%Mol(k)%lOrient(j)=Molcrys%Mol(k)%lOrient(j)-1
                end if
             end do

             do j=1,6
                if (Molcrys%Mol(k)%lT_TLS(j) > N) then
                    Molcrys%Mol(k)%lT_TLS(j)=Molcrys%Mol(k)%lT_TLS(j)-1
                end if
             end do

             do j=1,6
                if (Molcrys%Mol(k)%lL_TLS(j) > N) then
                    Molcrys%Mol(k)%lL_TLS(j)=Molcrys%Mol(k)%lL_TLS(j)-1
                end if
             end do

             do i=1,3
                do j=1,3
                   if (Molcrys%Mol(k)%lS_TLS(i,j) > N) then
                       Molcrys%Mol(k)%lS_TLS(i,j)=Molcrys%Mol(k)%lS_TLS(i,j)-1
                   end if
                end do
             end do

             if (Molcrys%Mol(k)%natoms <=0) cycle

             do i=1,Molcrys%Mol(k)%natoms
                do j=1,3
                   if (MolCrys%Mol(k)%lI_coor(j,i) == N) then
                       MolCrys%Mol(k)%lI_coor(j,i)=0
                       MolCrys%Mol(k)%mI_coor(j,i)=0.0
                       deleted=.true.
                   end if
                end do

                if (MolCrys%Mol(k)%lu_iso(i) == N) then
                    MolCrys%Mol(k)%lu_iso(i)=0
                    MolCrys%Mol(k)%mu_iso(i)=0.0
                    deleted=.true.
                end if

                if (MolCrys%Mol(k)%locc(i) == N) then
                    MolCrys%Mol(k)%locc(i)=0
                    MolCrys%Mol(k)%mocc(i)=0.0
                    deleted=.true.
                end if
             end do

             do i=1,Molcrys%Mol(k)%natoms
                do j=1,3
                   if (MolCrys%Mol(k)%lI_coor(j,i) > N) then
                       MolCrys%Mol(k)%lI_coor(j,i)=MolCrys%Mol(k)%lI_coor(j,i)-1
                   end if
                end do

                if (MolCrys%Mol(k)%lu_iso(i) > N) then
                    MolCrys%Mol(k)%lu_iso(i)=MolCrys%Mol(k)%lu_iso(i)-1
                end if

                if (MolCrys%Mol(k)%locc(i) > N) then
                    MolCrys%Mol(k)%locc(i)=MolCrys%Mol(k)%locc(i)-1
                end if
             end do

          end do
       end if

       !---- Updating V_Vectors ----!
       if (deleted) call Delete_element_in_Varrays(N)
    End Subroutine Delete_RefCodes_MolCrys

    !!--++
    !!--++ Module Subroutine Delete_RefCodes_Molec(N,Molec)
    !!--++    integer,             intent(in)     :: N
    !!--++    type(molecule_type), intent(in out) :: Molec
    !!--++
    !!--++ Overloaded
    !!--++ Delete the number of Refinable Parameter (N) on the list
    !!--++
    !!--++ Update: March - 2005
    !!
    Module Subroutine Delete_RefCodes_Molec(N,Molec)
       !---- Arguments ----!
       integer,             intent(in)     :: N
       type(molecule_type), intent(in out) :: Molec

       !---- Local Variables ----!
       logical :: deleted
       integer :: i,j

       deleted=.false.

       do j=1,3
          if (Molec%lxcentre(j) == N) then
             Molec%lxcentre(j)=0
             Molec%mxcentre(j)=0.0
             deleted=.true.
          end if
       end do

       do j=1,3
          if (Molec%lOrient(j) == N) then
             Molec%lOrient(j)=0
             Molec%mOrient(j)=0.0
             deleted=.true.
          end if
       end do

       do j=1,6
          if (Molec%lT_TLS(j) == N) then
             Molec%lT_TLS(j)=0
             Molec%mT_TLS(j)=0.0
             deleted=.true.
          end if
       end do

       do j=1,6
          if (Molec%lL_TLS(j) == N) then
             Molec%lL_TLS(j)=0
             Molec%mL_TLS(j)=0.0
             deleted=.true.
          end if
       end do

       do i=1,3
          do j=1,3
             if (Molec%lS_TLS(i,j) == N) then
                Molec%lS_TLS(i,j)=0
                Molec%mS_TLS(i,j)=0.0
                deleted=.true.
             end if
          end do
       end do

       !---- Updating ----!
       do j=1,3
          if (Molec%lxcentre(j) > N) then
             Molec%lxcentre(j)=Molec%lxcentre(j)-1
          end if
       end do

       do j=1,3
          if (Molec%lOrient(j) > N) then
             Molec%lOrient(j)=Molec%lOrient(j)-1
          end if
       end do

       do j=1,6
          if (Molec%lT_TLS(j) > N) then
             Molec%lT_TLS(j)=Molec%lT_TLS(j)-1
          end if
       end do

       do j=1,6
          if (Molec%lL_TLS(j) > N) then
             Molec%lL_TLS(j)=Molec%lL_TLS(j)-1
          end if
       end do

       do i=1,3
          do j=1,3
             if (Molec%lS_TLS(i,j) > N) then
                Molec%lS_TLS(i,j)=Molec%lS_TLS(i,j)-1
             end if
          end do
       end do

       if (molec%natoms <=0) return

       do i=1,Molec%Natoms
          do j=1,3
             if (Molec%lI_coor(j,i) == N) then
                Molec%lI_coor(j,i)=0
                Molec%mI_coor(j,i)=0.0
                deleted=.true.
             end if
          end do

          if (Molec%lu_iso(i) == N) then
             Molec%lu_iso(i)=0
             Molec%mu_iso(i)=0.0
             deleted=.true.
          end if

          if (Molec%locc(i) == N) then
             Molec%locc(i)=0
             Molec%mocc(i)=0.0
             deleted=.true.
          end if
       end do

       !---- Updating ----!
       do i=1,Molec%Natoms
          do j=1,3
             if (Molec%lI_coor(j,i) > N) then
                Molec%lI_coor(j,i)=Molec%lI_coor(j,i)-1
             end if
          end do

          if (Molec%lu_iso(i) > N) then
             Molec%lu_iso(i)=Molec%lu_iso(i)-1
          end if

          if (Molec%locc(i) > N) then
             Molec%locc(i)=Molec%locc(i)-1
          end if
       end do

       !---- Updating V_Vectors ----!
       if (deleted) call Delete_element_in_Varrays(N)
    End Subroutine Delete_RefCodes_Molec

    !!--++
    !!--++ Module Subroutine Delete_RefCodes_Magdom(N, Mag_Dom)
    !!--++    integer,              intent(in)          :: N
    !!--++    type(Magnetic_Domain_type),intent(in out) :: Mag_Dom
    !!--++
    !!--++ Delete the number of Refinable Parameters (N) on the list
    !!--++ related to magnetic domains
    !!--++ Created: February - 2012
    !!
    Module Subroutine Delete_RefCodes_Magdom(N, Mag_Dom)
       !---- Arguments ----!
       integer,                    intent(in)     :: N
       type(Magnetic_Domain_type), intent(in out) :: Mag_Dom

       !---- Local Variables ----!
       logical :: deleted
       integer :: i,j,ich

       deleted=.false.

       !---- Check is chirality is present ----!
       if (Mag_Dom%chir) then
        ich=2
       else
        ich=1
       end if

       !---- Eliminate N Parameter ----!
        do i=1,Mag_Dom%nd
         do j=1,ich
          if (Mag_Dom%Lpop(j,i) == N) then
              Mag_Dom%Mpop(j,i)=0.0
              Mag_Dom%Lpop(j,i)=0
              deleted=.true.
          end if
         end do
        end do

       !---- Updating Variables ----!
        do i=1,Mag_Dom%nd
         do j=1,ich
          if (Mag_Dom%Lpop(j,i) > N) then
              Mag_Dom%Lpop(j,i)=Mag_Dom%Lpop(j,i)-1
          end if
         end do
        end do

       !---- Updating V_Vectors ----!
       if (deleted) call Delete_element_in_Varrays(N)
    End Subroutine Delete_RefCodes_Magdom

    !!----
    !!---- Module Subroutine Delete_RefGCodes(N, model)
    !!----   integer,                             intent(in)     :: N
    !!----   type(Nonatomic_Parameter_List_Type), intent(in out) :: model
    !!----
    !!---- Delete the number of Refinable Parameter (N) on the list of
    !!---- non atomic parameters
    !!----
    !!---- Update: November 2 - 2013
    !!
    Module Subroutine Delete_RefGCodes(N, model)
       !---- Arguments ----!
       integer,                             intent(in)     :: N
       type(Nonatomic_Parameter_List_Type), intent(in out) :: model

       !---- Local Variables ----!
       logical :: deleted
       integer :: i

       deleted=.false.

       !---- Eliminate N Parameter ----!
       do i=1,model%npar
          if (model%par(i)%Lcode == N) then
              model%par(i)%Lcode=0
              model%par(i)%multip=0.0
              deleted=.true.
          end if
       end do

       !---- Updating Variables ----!
       do i=1,model%npar
          if (model%par(i)%Lcode > N) then
              model%par(i)%Lcode=model%par(i)%Lcode-1
          end if
       end do

       !---- Updating V_Vectors ----!
       if (deleted) call Delete_element_in_Varrays(N)
    End Subroutine Delete_RefGCodes

    Module Subroutine Delete_element_in_Varrays(N)
       integer, intent(in) :: N
       integer             :: i
       do i=N+1,Np_refi
          V_Vec(i-1)=V_Vec(i)
          V_Name(i-1)=V_Name(i)
          V_Bounds(:,i-1)=V_Bounds(:,i)
          V_BCon(i-1)=V_BCon(i)
          V_List(i-1)=V_List(i)
       end do
       V_Vec(np_refi)=0.0
       V_Name(np_refi)=" "
       V_Bounds(:,np_refi)=0.0
       V_BCon(np_refi)=0
       V_List(np_refi)=0
       np_refi=np_refi-1
    End Subroutine Delete_element_in_Varrays

End Submodule KWC_Deletion
