
 Submodule (CFML_Geom) Geom_Coordination
    implicit none
  contains

    !!----
    !!---- Module Subroutine Set_TDist_Coordination(Max_coor,Dmax, Cell, Spg, A)
    !!----    integer,            intent(in)   :: max_coor !  Maximum expected coordination
    !!----    real(kind=cp),      intent(in)   :: dmax     !  In -> Max. Distance to calculate
    !!----    real(kind=cp),      intent(in)   :: dangl    !  In -> Max. distance for angle calculations
    !!----    type (Cell_G_Type), intent(in)   :: Cell     !  In -> Object of Crytal_Cell_Type
    !!----    Class(SpG_Type),    intent(in)   :: SpG      !  In -> Object of SpG_Type
    !!----    type (AtList_Type), intent(in)   :: A        !  In -> Object of AtList_Type
    !!----
    !!----    Subroutine to calculate distances, below the prescribed distance "dmax"
    !!----    Sets up the coordination type: Coord_Info for each atom in the asymmetric unit
    !!----    Needs as input the objects Cell (of type Crystal_cell), SpG (of type Space_Group)
    !!----    and A (of type atom_list, that should be allocated in the calling program).
    !!----    The input argument Max_Coor is obtained, before calling the present procedure,
    !!----    by a call to Allocate_Coordination_Type with arguments:(A%natoms,Spg%Multip,Dmax,max_coor)
    !!----    Further calls to this routine do not need a previous call to Allocate_Coordination_Type.
    !!----
    !!---- Update: February - 2005
    !!
    Module Subroutine Set_TDist_Coordination(max_coor,Dmax, Cell, Spg, A)
       !---- Arguments ----!
       integer,                  intent(in)   :: max_coor
       real(kind=cp),            intent(in)   :: dmax
       type (cell_G_Type),       intent(in)   :: Cell
       Class(SpG_Type),          intent(in)   :: SpG
       type (AtList_Type),       intent(in)   :: A

       !---- Local Variables ----!
       integer                                 :: i,j,k,lk,i1,i2,i3,nn,L,ico
       integer,       dimension(3)             :: ic1,ic2
       real(kind=cp), dimension(3)             :: xx,x1,xo,Tn,xr, QD
       real(kind=cp)                           :: T,dd
       real(kind=cp), dimension(3,max_coor)    :: uu

       !real(kind=cp), dimension(3,SpG%Multip)  :: tr  !Replaced by a call to init_opMatTr
       !integer,       dimension(3,3,SpG%Multip):: Mat
       !
       !d=SpG%d
       !do i=1,SpG%Multip
       !  Mat(:,:,i) =SpG%Op(i)%Mat(1:3,1:3)
       !   tr(:,i)  = SpG%Op(i)%Mat(1:3,d)
       !End do

       if(.not. init_symOP) call init_opMatTr(SpG)

       qd(:)=1.0/cell%rcell(:)
       ic2(:)= int(dmax/cell%cell(:))+1
       ic1(:)=-ic2(:)
       do i=1,a%natoms
          xo(:)=a%atom(i)%x(:)

          ico=0
          do k=1,a%natoms
             lk=1
             uu(:,lk)=xo(:)
             do j=1,Spg%Multip
                !xx=Apply_OP(Spg%Op(j),a%atom(k)%x)
                xx=Matmul(opMat(:,:,j),a%atom(k)%x)+opTr(:,j)
                do i1=ic1(1),ic2(1)
                   do i2=ic1(2),ic2(2)
                      do_i3:do i3=ic1(3),ic2(3)
                            Tn(1)=real(i1); Tn(2)=real(i2); Tn(3)=real(i3)
                            x1(:)=xx(:)+tn(:)
                            do l=1,3
                               t=abs(x1(l)-xo(l))*qd(l)
                               if (t > dmax) cycle  do_i3
                            end do
                            do nn=1,lk
                               if (sum(abs(uu(:,nn)-x1(:)))  <= epsi) cycle  do_i3
                            end do
                            xr = matmul(cell%cr_orth_cel,x1-xo)
                            dd=sqrt(dot_product(xr,xr))
                            if (dd > dmax .or. dd < 0.001) cycle
                            ico=ico+1
                           ! Control not performed ... it is supposed that max_coor is large enough
                           !if (Coord_Info%Coord_Num(i) > Coord_Info%Max_Coor) then
                           !   Err_CFML%Ierr=1
                           !   Err_CFML%Msg=" => Too many distances around an atom"
                           !   return
                           !end if
                            lk=lk+1
                            uu(:,lk)=x1(:)
                            Coord_Info%Dist(ico,i)=dd
                            Coord_Info%N_Cooatm(ico,i)=k
                            Coord_Info%N_sym(ico,i)=j

                            ! Added by JGP
                            Coord_Info%Tr_Coo(:,ico,i)=tn
                      end do do_i3 !i3
                   end do !i2
                end do !i1
             end do !j
          end do !k
          Coord_Info%Coord_Num(i)=ico
       end do !i
    End Subroutine Set_TDist_Coordination

    !!----
    !!---- Subroutine Set_TDist_Partial_Coordination(List,Max_coor,Dmax, Cell, Spg, A)
    !!----    integer,             intent(in)   :: List     !  Modified atom
    !!----    integer,             intent(in)   :: max_coor !  Maximum expected coordination
    !!----    real(kind=cp),       intent(in)   :: dmax     !  In -> Max. Distance to calculate
    !!----    real(kind=cp),       intent(in)   :: dangl    !  In -> Max. distance for angle calculations
    !!----    type (Cell_G_Type),  intent(in)   :: Cell     !  In -> Object of Crytal_Cell_Type
    !!----    Class(SpG_Type),     intent(in)   :: SpG      !  In -> Object of SpG_Type
    !!----    type (AtList_Type),  intent(in)   :: A        !  In -> Object of AtList_Type
    !!----
    !!----    Modify the coordination type: Coord_Info for the atoms affected by the change of atom "List"
    !!----    Needs as input the objects Cell (of type Crystal_cell), SpG (of type Space_Group)
    !!----    and A (or type atom_list, that should be allocated in the calling program).
    !!----    This routine is a modification of Set_TDist_Coordination to avoid superfluous calculations
    !!----    in global optimization methods. It assumes that Set_TDist_Coordination has previously been
    !!----    called and the object "Coord_Info" has already been set.
    !!----
    !!---- Update: May - 2009
    !!
    Module Subroutine Set_TDist_Partial_Coordination(List,max_coor,Dmax, Cell, Spg, A)
       !---- Arguments ----!
       integer,                  intent(in)   :: List
       integer,                  intent(in)   :: max_coor
       real(kind=cp),            intent(in)   :: dmax
       type (cell_G_Type),       intent(in)   :: Cell
       Class(SpG_Type),          intent(in)   :: SpG
       type (AtList_Type),       intent(in)   :: A

       !---- Local Variables ----!
       integer                                 :: i,j,k,lk,i1,i2,i3,nn,L,ic,ico
       integer,       dimension(3)             :: ic1,ic2
       integer,       dimension(A%natoms)      :: po,pn
       real(kind=cp), dimension(3)             :: xx,x1,xo,Tn,xr, QD
       real(kind=cp)                           :: T,dd
       real(kind=cp), dimension(3,max_coor)    :: uu
       !real(kind=cp), dimension(3,SpG%Multip)  :: tr
       !integer,       dimension(3,3,SpG%Multip):: Mat
       !
       !d=SpG%d
       !do i=1,SpG%Multip
       !  Mat(:,:,i) =SpG%Op(i)%Mat(1:3,1:3)
       !   tr(:,i)  = SpG%Op(i)%Mat(1:3,d)
       !End do
       if(.not. init_symOP) call init_opMatTr(SpG)

       po=0; pn=0
       po(List)=1 !This atom has a modified coordination sphere
       ic=Coord_Info%Coord_Num(List)
       do i=1,ic
         po(Coord_Info%N_Cooatm(i,List))=1  !This atom has a modified coordination sphere
       end do

       qd(:)=1.0/cell%rcell(:)
       ic2(:)= int(dmax/cell%cell(:))+1
       ic1(:)=-ic2(:)
       !Determine the new coordination sphere of the changed atom
       i=List
       xo(:)=a%atom(i)%x(:)

       ico=0
       do k=1,a%natoms
          lk=1
          uu(:,lk)=xo(:)
          do j=1,Spg%Multip
             !xx=Apply_OP(Spg%Op(j),a%atom(k)%x)
             xx=Matmul(opMat(:,:,j),a%atom(k)%x)+opTr(:,j)
             do i1=ic1(1),ic2(1)
                do i2=ic1(2),ic2(2)
                   do_i3:do i3=ic1(3),ic2(3)
                         Tn(:)=real([i1,i2,i3])
                         x1(:)=xx(:)+tn(:)
                         do l=1,3
                            t=abs(x1(l)-xo(l))*qd(l)
                            if (t > dmax) cycle  do_i3
                         end do
                         do nn=1,lk
                            if (sum(abs(uu(:,nn)-x1(:)))  <= epsi) cycle  do_i3
                         end do
                         xr = matmul(cell%cr_orth_cel,x1-xo)
                         dd=sqrt(dot_product(xr,xr))
                         if (dd > dmax .or. dd < 0.001) cycle
                         ico=ico+1
                         lk=lk+1
                         uu(:,lk)=x1(:)
                         Coord_Info%Dist(ico,i)=dd
                         Coord_Info%N_Cooatm(ico,i)=k
                         Coord_Info%N_sym(ico,i)=j
                   end do do_i3 !i3
                end do !i2
             end do !i1
          end do !j
       end do !k
       Coord_Info%Coord_Num(i)=ico
       pn(list)=0
       po(list)=0

       ic=Coord_Info%Coord_Num(List)    !New coordination number of atom List
       do i=1,ic
         pn(Coord_Info%N_Cooatm(i,List))=1  !This atom has now a newly modified coordination sphere
       end do
       !Look now the changed coordinaion spheres
       do i=1,a%natoms
         if(pn(i) == 0 .and. po(i) == 0) cycle
         !if(po(i) == 1 .and. pn(i) == 1) then !the atom remains in the coordination sphere, only recalculation of distance is needed
         !  ic=Coord_Info%Coord_Num(i)
         !  do k=1,ic
         !   if(List == Coord_Info%N_Cooatm(k,i)) then
         !   end if
         !  end do
         !end if
         !DO ALL WAITING FOR A MORE EFFICIENT ALGORITHM
         xo(:)=a%atom(i)%x(:)

         ico=0
         do k=1,a%natoms
            lk=1
            uu(:,lk)=xo(:)
            do j=1,Spg%Multip
               !xx=Apply_OP(Spg%Op(j),a%atom(k)%x)
               xx=Matmul(opMat(:,:,j),a%atom(k)%x)+opTr(:,j)
               do i1=ic1(1),ic2(1)
                  do i2=ic1(2),ic2(2)
                     do_inter:do i3=ic1(3),ic2(3)
                           Tn(:)=real([i1,i2,i3])
                           x1(:)=xx(:)+tn(:)
                           do l=1,3
                              t=abs(x1(l)-xo(l))*qd(l)
                              if (t > dmax) cycle  do_inter
                           end do
                           do nn=1,lk
                              if (sum(abs(uu(:,nn)-x1(:)))  <= epsi) cycle  do_inter
                           end do
                           xr = matmul(cell%cr_orth_cel,x1-xo)
                           dd=sqrt(dot_product(xr,xr))
                           if (dd > dmax .or. dd < 0.001) cycle
                           ico=ico+1
                           lk=lk+1
                           uu(:,lk)=x1(:)
                           Coord_Info%Dist(ico,i)=dd
                           Coord_Info%N_Cooatm(ico,i)=k
                           Coord_Info%N_sym(ico,i)=j
                     end do do_inter !i3
                  end do !i2
               end do !i1
            end do !j
         end do !k
         Coord_Info%Coord_Num(i)=ico
       end do !i
    End Subroutine Set_TDist_Partial_Coordination

 End Submodule Geom_Coordination
