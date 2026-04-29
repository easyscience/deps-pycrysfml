!!----
!!----
!!----
!!
SubModule (CFML_Atoms) Atm_ExtendList
  implicit none
   Contains

   !!----
   !!---- EXTEND_LIST
   !!----
   !!----    Subroutine to generate atoms in the primitive (conven=.false.) or the conventional
   !!----    unit cell (conven=.true.), Excluding atoms with A%atom(:)%active=.false.
   !!----
   !!---- Update: February - 2005
   !!
   Module Subroutine Extend_List(A, B, Spg, Type_Atm,Conven,lun)
      !---- Arguments ----!
      type(atlist_type),    intent(in)     :: A         ! Atom list (asymmetric unit)
      type(atlist_type),    intent(in out) :: B         ! Atom list into the unit cell
      class(SpG_Type),      intent(in)     :: SpG       ! SpaceGroup
      character(len=*),     intent(in)     :: Type_Atm  ! !Atomic type: Atm, Atm_Std, ModAtm_Std, Atm_Ref, ModAtm_Ref
      logical, optional,    intent(in)     :: Conven    ! If present and .true. using the whole conventional unit cell
      integer, optional,    intent(in)     :: lun       ! Outputs the content of a primitive/conventional cell
      !---- Local Variables ----!
      character(len=4)                         :: fmm
      integer                                  :: i,k,j,l,nt,npeq,n,d
      real(kind=cp)                            :: qc
      real(kind=cp),dimension(3)               :: xo,xx
      real(kind=cp),dimension(3,Spg%multip)    :: u
      logical                                  :: ccell,iprin
      type(atlist_type)                        :: c_atm
      real(kind=cp), dimension(3,SpG%Multip)   :: tr
      integer,       dimension(3,3,SpG%Multip) :: Mat


      !> Init
      call clear_error()
      ccell=.false.; iprin=.false.
      if (present(conven)) ccell=conven
      if (present(lun)) iprin=.true.
      !> Check arguments
      if (.not. extends_type_of(B,A) .or. (.not. same_type_as(B,A)) ) then
         err_CFML%IErr=1
         err_CFML%Msg="Extend_List@CFML_Atoms: Incompatible arguments!"
         return
      end if

      d=SpG%d
      do i=1,SpG%Multip
        Mat(:,:,i)= SpG%Op(i)%Mat(1:3,1:3)
         tr(:,i)  = SpG%Op(i)%Mat(1:3,d)
      End do

      npeq=SpG%numops
      if (SpG%centred == 2) npeq=npeq*2
      if (ccell) npeq=SpG%multip
      call allocate_atom_list(npeq*A%natoms,c_atm,Type_Atm,0)

      if (iprin)  then
         if (ccell) then
            write(unit=lun,fmt="(/,a)") "     LIST OF ATOMS INSIDE THE CONVENTIONAL UNIT CELL "
            write(unit=lun,fmt="(a,/)") "     =============================================== "
         else
            write(unit=lun,fmt="(/,a)") "     LIST OF ATOMS CONTAINED IN A PRIMITIVE CELL "
            write(unit=lun,fmt="(a,/)") "     =========================================== "
         end if
      end if

      n=0
      c_atm%Iph = A%Iph
      do k=1,A%natoms
         if (.not. A%atom(k)%active) cycle
         l=1       ! Number of representant atom in asymmetric unit
         n=n+1     ! Number of atom in the list

         c_atm%Active(n) =A%Active(k)

         xo= modulo_lat(A%atom(k)%x)
         u(:,l)= xo
         !c_atm%Atom(n)=A%atom(k)        !First atom of the orbit (unsupported by gfortran!!!!)
         !Workaround for gfortran
         c_atm%Atom(n)%lab     = trim(A%atom(k)%lab)//"_1"
         c_atm%Atom(n)%ChemSymb= A%atom(k)%ChemSymb
         c_atm%Atom(n)%SfacSymb= A%atom(k)%SfacSymb
         c_atm%Atom(n)%Z       = A%atom(k)%Z
         c_atm%Atom(n)%Mult    = 1 !real(l)/SpG%multip
         c_atm%Atom(n)%Charge  = A%atom(k)%Charge
         c_atm%Atom(n)%x       = xo
         c_atm%Atom(n)%U_iso   = A%atom(k)%U_iso
         c_atm%Atom(n)%Occ     = A%atom(k)%Occ
         c_atm%Atom(n)%UType   = A%atom(k)%UType
         c_atm%Atom(n)%ThType  = A%atom(k)%ThType
         c_atm%Atom(n)%U       = A%atom(k)%U
         c_atm%Atom(n)%Magnetic= A%atom(k)%Magnetic
         c_atm%Atom(n)%Mom     = A%atom(k)%Mom
         c_atm%Atom(n)%Moment  = A%atom(k)%Moment
         c_atm%Atom(n)%Ind_ff  = A%atom(k)%Ind_ff
         c_atm%Atom(n)%AtmInfo = A%atom(k)%AtmInfo
         c_atm%Atom(n)%wyck    = A%atom(k)%wyck
         c_atm%Atom(n)%VarF    = A%atom(k)%VarF
         c_atm%Atom(n)%active  = A%atom(k)%active
         if (iprin) then
            qc=c_atm%Atom(n)%Charge
            write(unit=lun,fmt="(/,a,a)") " => Equivalent positions of atom: ",trim(A%atom(k)%lab)
            write(unit=lun,fmt="(2a,3f10.5,2(a,f6.3))")"       ",   &
             c_atm%Atom(n)%lab, xo(:), "   M = ", c_atm%Atom(n)%Mom ," Q = ", qc
         end if
         loop:do j=2,npeq
            !xx=Apply_OP(SpG%Op(j),xo)
            xx=Matmul(Mat(:,:,j),xo) + tr(:,j)
            xx=modulo_lat(xx)
            do nt=1,l
               if (equal_vector(u(:,nt),xx,3)) then
                  c_atm%atom(n-(l-nt))%occ = c_atm%atom(n-(l-nt))%occ + A%atom(k)%occ
                  cycle loop
               end if
            end do

            l=l+1
            u(:,l)=xx(:)
            n=n+1

            c_atm%active(n)=A%Active(k)

            select case (l)
               case(:9)
                  write(unit=fmm,fmt="(i1)") l
               case(10:99)
                  write(unit=fmm,fmt="(i2)") l
               case(100:999)
                  write(unit=fmm,fmt="(i3)") l
            end select


            !c_atm%Atom(n)     = A%atom(k)     ! Valid for Intel and not for GFortran
            !Workaround for gfortran
            c_atm%Atom(n)%lab     = trim(A%atom(k)%lab)//"_"//adjustl(fmm)
            c_atm%Atom(n)%ChemSymb= A%atom(k)%ChemSymb
            c_atm%Atom(n)%SfacSymb= A%atom(k)%SfacSymb
            c_atm%Atom(n)%Z       = A%atom(k)%Z
            c_atm%Atom(n)%Mult    = 1 !real(l)/SpG%multip
            c_atm%Atom(n)%Charge  = A%atom(k)%Charge
            c_atm%Atom(n)%x       = xx
            c_atm%Atom(n)%U_iso   = A%atom(k)%U_iso
            c_atm%Atom(n)%Occ     = A%atom(k)%Occ
            c_atm%Atom(n)%UType   = A%atom(k)%UType
            c_atm%Atom(n)%ThType  = A%atom(k)%ThType
            c_atm%Atom(n)%U       = A%atom(k)%U
            c_atm%Atom(n)%Magnetic= A%atom(k)%Magnetic
            c_atm%Atom(n)%Mom     = A%atom(k)%Mom
            c_atm%Atom(n)%Moment  = A%atom(k)%Moment
            c_atm%Atom(n)%Ind_ff  = A%atom(k)%Ind_ff
            c_atm%Atom(n)%AtmInfo = A%atom(k)%AtmInfo
            c_atm%Atom(n)%wyck    = A%atom(k)%wyck
            c_atm%Atom(n)%VarF    = A%atom(k)%VarF
            c_atm%Atom(n)%active  = A%atom(k)%active
            if (iprin) then
               qc=c_atm%Atom(n)%Charge
               write(unit=lun,fmt="(2a,3f10.5,2(a,f6.3))")"       ",   &
               c_atm%Atom(n)%lab, xx(:), "   M = ", c_atm%Atom(n)%Mom ," Q = ", qc
            end if

            !select type(atm => A%atom(k))
            !   class is (Atm_Std_Type)
            !      c_atm%Atom(n)%u_iso_std=atm%u_iso_std
            !      c_atm%Atom(n)%x_std=Atm%x_std
            !      c_atm%Atom(n)%occ_std=Atm%occ_std
            !end select

            !select type(atm => A%atom(k))
            !   type is (ModAtm_Std_Type)
            !      c_atm%Atom(n)%wyck   = atm%wyck
            !      c_atm%Atom(n)%n_mc   = atm%n_mc
            !      c_atm%Atom(n)%n_dc   = atm%n_dc
            !      !c_atm%Atom(n)%mcs    = A%atom(k)%mcs
            !      !c_atm%Atom(n)%mcs_std= A%atom(k)%mcs_std
            !      !c_atm%Atom(n)%dcs    = A%atom(k)%dcs
            !      !c_atm%Atom(n)%dcs_std= A%atom(k)%dcs_std
            !end select

         end do loop
      end do

      if (n == 0) then
         err_CFML%IErr=1
         Err_CFML%flag=.true.
         err_CFML%Msg="Extend_List@CFML_Atoms: Number of extended atoms is zero!"
         call allocate_atom_list(0,c_atm,Type_Atm,0)
         return
      end if

      call allocate_atom_list(n,B,Type_Atm,0)
      B%Active=c_atm%active(1:n)
      B%Iph=c_atm%iph
      B%Atom=c_atm%atom(1:n)

      !> DEallocate
      call allocate_atom_list(0,c_atm,Type_Atm,0)

   End Subroutine Extend_List

   !!----
   !!---- Subroutine AtList_To_Atm_Cell(A,Ac)
   !!----    type(Atlist_type),  intent(in)        :: A    !  In -> instance of Atlist_type
   !!----    type(Atm_Cell_Type),intent(in out)    :: Ac   !  In -> instance of Atm_Cell_Type
   !!----                                                    Out -> Initialize some Atm_Cell_Type components
   !!----
   !!----    Subroutine to construct an Atom Cell object "Ac" from an atom_list
   !!----    object "A". It is supposed that both objects have been previouly
   !!----    allocated using the appropriate procedures. It is supposed that the atoms
   !!----    in the input A list constitute all the atoms in the conventional/primitive cell.
   !!----
   !!---- Update: February - 2005, updated May-2022
   !!
   Module Subroutine AtList_To_Atm_Cell(A,Ac)
      !---- Arguments ----!
      type(Atlist_type),   intent(in)        :: A
      type(Atm_Cell_Type), intent(in out)    :: Ac

      !---- Local Variables ----!
      integer :: i

      Ac%nat=A%natoms
      do i=1,Ac%nat
         Ac%Lab(i)         = A%atom(i)%lab
         Ac%xyz(:,i)       = A%Atom(i)%x
         Ac%var_free(:,i)  = A%Atom(i)%varf
         Ac%moment(i)      = A%Atom(i)%mom
      end do
   End Subroutine AtList_To_Atm_Cell

   !!----
   !!---- SET_ATOM_EQUIV_LIST
   !!----
   !!---- Subroutine constructing the list of all atoms in the unit cell.
   !!---- The atoms are in a structure of type "Atom_Equiv_List_Type" containing
   !!---- just the fractional coordinates of all the atoms in the cell.
   !!---- This a simplified version of the Extend_List Subroutine useful for geometric
   !!---- calculations, using the type Atom_Equiv_List_Type, without the burden of
   !!---- all components of Aton_Type
   !!----
   !!---- Updated: May 2020
   !!
   Module Subroutine Set_Atom_Equiv_List(SpG,cell,A,Ate,lun)
      !---- Arguments ----!
      class(SpG_Type),            intent(in) :: SpG
      type(Cell_G_Type),          intent(in) :: Cell
      type(Atlist_Type),          intent(in) :: A
      type(Atom_Equiv_List_Type), intent(out):: Ate
      integer, optional,          intent(in) :: lun

      !---- local variables ----!
      real(kind=cp),  dimension(3)            :: xx,xo,v,xc
      real(kind=cp),  dimension(3,SpG%Multip) :: u
      character(len=20),dimension(SpG%Multip) :: label
      integer                                 :: i,k,j,L,nt,d
      character (len=6)                       :: fmm
      character (len=20)                      :: nam
      real(kind=cp), parameter                :: epsi = 0.002
      real(kind=cp), dimension(3,SpG%Multip)  :: tr
      integer,       dimension(3,3,SpG%Multip):: Mat

      d=SpG%d
      do i=1,SpG%Multip
        Mat(:,:,i)= SpG%Op(i)%Mat(1:3,1:3)
         tr(:,i)  = SpG%Op(i)%Mat(1:3,d)
      End do
      if (.not. allocated (Ate%atm)) allocate(Ate%atm(A%natoms))
      ate%nauas=A%natoms
      if (present(lun))  then
         write(unit=lun,fmt="(/,a)") "     LIST OF ATOMS INSIDE THE CONVENTIONAL UNIT CELL "
         write(unit=lun,fmt="(a,/)") "     =============================================== "
      end if
      do k=1,A%natoms

         ate%atm(k)%ChemSymb = A%atom(k)%ChemSymb
         xo(:) =Modulo_Lat(A%atom(k)%x(:))
         L=1
         u(:,L)=xo(:)
         xc =matmul(cell%Cr_Orth_cel,xo)
         if (present(lun))then
            write(unit=lun,fmt="(/,a,a)") " => Equivalent positions of atom: ",A%atom(k)%lab
            write(unit=lun,fmt="(a)")  &
            "                                    x         y         z          Xc        Yc        Zc"
         end if
         fmm="(a,i1)"
         write(unit=label(L),fmt=fmm) trim(A%Atom(k)%lab)//"_",L
         nam=label(L)
         if (present(lun)) write(unit=lun,fmt="(3a,3f10.5,a,3f10.5)") "       ",nam,"  ", xo,"  ", xc

         do_eq:DO j=2,SpG%multip
            !xx=Apply_OP(SpG%Op(j),xo)
            xx=Matmul(Mat(:,:,j),xo) + tr(:,j)
            xx=modulo_lat(xx)
            do nt=1,L
               v=u(:,nt)-xx(:)
               if (sum(abs((v))) < epsi ) cycle do_eq
            end do
            L=L+1
            u(:,L)=xx(:)
            if ( L > 9 .and. L < 100)  fmm="(a,i2)"
            if ( L >= 100 )  fmm="(a,i3)"
            write(unit=label(L),fmt=fmm) trim(A%Atom(k)%lab)//"_",L
            nam=Label(L)
            xc=matmul(cell%Cr_Orth_cel,xx)
            if (present(lun)) write(unit=lun,fmt="(3a,3f10.5,a,3f10.5)") "       ",nam,"  ", xx,"  ", xc
         end do do_eq

         if (allocated(Ate%Atm(k)%Lab)) deallocate(Ate%Atm(k)%Lab)
         allocate(Ate%Atm(k)%lab(L))
         if (allocated(Ate%Atm(k)%x)) deallocate(Ate%Atm(k)%x)
         allocate(Ate%Atm(k)%x(3,L))

         Ate%Atm(k)%mult=L
         do j=1,Ate%Atm(k)%mult
            Ate%Atm(k)%lab(j)=Label(j)
            Ate%Atm(k)%x(:,j)=u(:,j)
         end do
      end do
      if (present(lun))  write(unit=lun,fmt="(/)")
   End Subroutine Set_Atom_Equiv_List

End SubModule Atm_ExtendList