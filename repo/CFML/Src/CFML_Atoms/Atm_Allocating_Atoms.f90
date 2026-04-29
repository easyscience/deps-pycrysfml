!!----
!!----
!!----
SubModule (CFML_Atoms) Atm_Allocating_Atoms
   implicit none
   Contains

   !!----
   !!---- INIT_ATOM_TYPE
   !!----    Initialize Atom_Type
   !!----
   !!---- 12/06/2019
   !!
   Module Subroutine Init_Atom_Type(Atm, d)
      !---- Arguments ----!
      class(Atm_Type), intent(in out)   :: Atm
      integer,         intent(in)       :: d    ! Number of k-vectors

      Atm%Lab      =" "
      Atm%ChemSymb =" "
      Atm%Z        =0
      Atm%Mult     =0
      Atm%Charge   =0
      Atm%X        =0.0_cp
      Atm%Occ      =0.0_cp

      !> Thermal model
      Atm%UType    ="B"
      Atm%ThType   ="iso"
      Atm%U_iso    = 0.0_cp
      Atm%U        = 0.0_cp

      !> Magnetic
      Atm%Magnetic =.false.
      Atm%Moment   = 0.0_cp

      !> SFac
      Atm%SfacSymb =" "
      Atm%Ind_ff   = 0

      !> Info
      Atm%AtmInfo  = " "
      Atm%wyck     = " "

      !> Free variables
      Atm%VarF     = 0.0
      Atm%active   = .true.

      select type (Atm)
         type is (atm_std_type)
            Atm%X_Std     = 0.0_cp
            Atm%Occ_Std   = 0.0_cp
            Atm%U_iso_Std = 0.0_cp
            Atm%U_Std     = 0.0_cp
            Atm%Moment_Std= 0.0_cp

         type is (ModAtm_std_type)
            Atm%X_Std     = 0.0_cp
            Atm%Occ_Std   = 0.0_cp
            Atm%U_iso_Std = 0.0_cp
            Atm%U_Std     = 0.0_cp
            Atm%Moment_Std= 0.0_cp

            Atm%n_oc      = 0
            Atm%n_bc      = 0
            Atm%n_mc      = 0
            Atm%n_dc      = 0
            Atm%n_uc      = 0
            Atm%poc_q     = 0
            Atm%pbc_q     = 0
            Atm%pmc_q     = 0
            Atm%pdc_q     = 0
            Atm%puc_q     = 0
            Atm%Ocs       = 0.0_cp
            Atm%Ocs_std   = 0.0_cp
            Atm%Bcs       = 0.0_cp
            Atm%Bcs_std   = 0.0_cp
            Atm%Mcs       = 0.0_cp
            Atm%Mcs_std   = 0.0_cp
            Atm%Dcs       = 0.0_cp
            Atm%Dcs_std   = 0.0_cp
            Atm%Ucs       = 0.0_cp
            Atm%Ucs_std   = 0.0_cp
            if(allocated (Atm%Xs)) deallocate(Atm%Xs)
            if(allocated (Atm%Us)) deallocate(Atm%Us)
            if(allocated (Atm%Moms)) deallocate(Atm%Moms)
            allocate(Atm%Xs(3+d),Atm%Us(3+d,3+d),Atm%Moms(3+d))
            Atm%Xs=0.0; Atm%Us=0.0; Atm%Moms=0.0

         type is (atm_ref_type)
            Atm%X_Std     = 0.0_cp
            Atm%Occ_Std   = 0.0_cp
            Atm%U_iso_Std = 0.0_cp
            Atm%U_Std     = 0.0_cp
            Atm%Moment_Std= 0.0_cp

            Atm%L_X       =0
            Atm%L_Occ     =0
            Atm%L_U_iso   =0
            Atm%L_U       =0
            Atm%L_moment  =0
            Atm%M_X       =0.0_cp
            Atm%M_Occ     =0.0_cp
            Atm%M_U_iso   =0.0_cp
            Atm%M_U       =0.0_cp
            Atm%M_moment  =0

         type is (ModAtm_ref_type)
            Atm%X_Std     = 0.0_cp
            Atm%Occ_Std   = 0.0_cp
            Atm%U_iso_Std = 0.0_cp
            Atm%U_Std     = 0.0_cp
            Atm%Moment_Std= 0.0_cp

            Atm%n_oc      = 0
            Atm%n_bc      = 0
            Atm%n_mc      = 0
            Atm%n_dc      = 0
            Atm%n_uc      = 0
            Atm%poc_q     = 0
            Atm%pbc_q     = 0
            Atm%pmc_q     = 0
            Atm%pdc_q     = 0
            Atm%puc_q     = 0
            Atm%Ocs       = 0.0_cp
            Atm%Ocs_std   = 0.0_cp
            Atm%Bcs       = 0.0_cp
            Atm%Bcs_std   = 0.0_cp
            Atm%Mcs       = 0.0_cp
            Atm%Mcs_std   = 0.0_cp
            Atm%Dcs       = 0.0_cp
            Atm%Dcs_std   = 0.0_cp
            Atm%Ucs       = 0.0_cp
            Atm%Ucs_std   = 0.0_cp

            Atm%L_X       =0
            Atm%L_Occ     =0
            Atm%L_U_iso   =0
            Atm%L_U       =0
            Atm%M_X       =0.0_cp
            Atm%M_Occ     =0.0_cp
            Atm%M_U_iso   =0.0_cp
            Atm%M_U       =0.0_cp

            Atm%L_Ocs       = 0.0_cp
            Atm%L_Bcs       = 0.0_cp
            Atm%L_Mcs       = 0.0_cp
            Atm%L_Dcs       = 0.0_cp
            Atm%L_Ucs       = 0.0_cp
            Atm%M_Ocs       = 0.0_cp
            Atm%M_Bcs       = 0.0_cp
            Atm%M_Mcs       = 0.0_cp
            Atm%M_Dcs       = 0.0_cp
            Atm%M_Ucs       = 0.0_cp
            if(allocated (Atm%Xs)) deallocate(Atm%Xs)
            if(allocated (Atm%Us)) deallocate(Atm%Us)
            if(allocated (Atm%Moms)) deallocate(Atm%Moms)
            allocate(Atm%Xs(3+d),Atm%Us(3+d,3+d),Atm%Moms(3+d))
            Atm%Xs=0.0; Atm%Us=0.0; Atm%Moms=0.0
      end select

   End Subroutine Init_Atom_Type

   !!----
   !!---- ALLOCATE_ATOM_LIST
   !!----    Allocation of objet A of type atom_list.
   !!----    This procedure subroutine should be called before using an object of type atm_list
   !!----
   !!---- 12/06/2019
   !!
   Module Subroutine Allocate_Atom_List(N, A,Type_Atm,d,iph)
      !---- Arguments ----!
      integer,             intent(in)       :: n        ! Atoms in the List
      type(Atlist_type),   intent(in out)   :: A        ! Objet to be allocated
      character(len=*),    intent(in)       :: Type_Atm
      integer,             intent(in)       :: d        ! Number of k-vectors
      integer, optional,   intent(in)       :: iph      ! Number of the phase

      !---- Local Variables ----!
      integer                            :: i,ier

      !> Init
      if (n <= 0) then
         A%natoms=0
         !> Deallocating atom list
         if (allocated(A%Active)) deallocate(A%Active)
         if (allocated(A%Atom)) deallocate(A%Atom)
         return
      end if

      !> Allocating variables
      Select Case(trim(l_case(Type_Atm)))
         case("atm_type")
            allocate(atm_type :: A%atom(n),stat=ier)

         case("atm_std_type")
            allocate(atm_std_type :: A%atom(n),stat=ier)

         case("modatm_std_type")
            allocate(modatm_std_type :: A%atom(n),stat=ier)

         case("atm_ref_type")
            allocate(atm_ref_type :: A%atom(n),stat=ier)

         case("modatm_ref_type")
            allocate(modatm_ref_type :: A%atom(n),stat=ier)

         case default
            call set_error(1," The argument on Type_Atm is unknown for Allocate_Atom_List procedure")
            A%natoms=0
            if (allocated(A%Active)) deallocate(A%Active)
            if (allocated(A%Atom)) deallocate(A%Atom)
            return
      end select

      allocate (A%active(n), stat=ier)
      if (ier /= 0) then
         Err_CFML%Ierr=1
         Err_CFML%flag=.true.
         write(unit=Err_CFML%Msg,fmt="(a,i6,a)") "Error allocating atom List for N =",N," atoms"
         return
      end if

      A%active=.true.
      A%IPh=1
      if(present(iph)) A%IPh=iph
      A%mcomp="crystal"

      do i=1,n
         call Init_Atom_Type(A%Atom(i),d)
      end do

      A%natoms=n
   End Subroutine Allocate_Atom_list

   !!----
   !!---- ALLOCATE_ATOMS_CELL
   !!----
   !!----    Allocation of objet "Ac" of type Atm_Cell. "Ac" contains
   !!----    components with ALLOCATABLE attribute with dimension depending
   !!----    on the input arguments "Nasu", "Mul" and "Dmax". The variables used for calculating the
   !!----    de dimensions are:
   !!--<<
   !!----          natcel=nasu*mul       and      id=idp=nint(0.74048*(dmax/r_atom)**3)
   !!-->>
   !!----    This subroutine should be called before using the subroutines of this module with
   !!----    dummy arguments of type Atm_Cell. For deallocating just put Nasu=0
   !!----
   !!---- Update: February - 2005
   !!
   Module Subroutine Allocate_Atoms_Cell(Nasu,Mul,Dmax,Ac)
      !---- Arguments ----!
      integer,              intent(in)     :: nasu   ! Number of atoms in asymmetric unit
      integer,              intent(in)     :: mul    ! General multiplicity of the Space Group
      real(kind=cp),        intent(in)     :: dmax   ! Maximun distance to be calculated
      type (Atm_cell_type), intent(in out) :: Ac     ! Object of type atoms_cell_type

      !---- local variables ----!
      integer :: natcel,id

      if(nasu <= 0) then !Deallocate the component of the object Ac
         if (allocated(Ac%Lab         ))   deallocate (Ac%Lab       )
         if (allocated(Ac%xyz         ))   deallocate (Ac%xyz        )
         if (allocated(Ac%charge      ))   deallocate (Ac%charge     )
         if (allocated(Ac%moment      ))   deallocate (Ac%moment     )
         if (allocated(Ac%var_free    ))   deallocate (Ac%var_free   )
         if (allocated(Ac%neighb      ))   deallocate (Ac%neighb     )
         if (allocated(Ac%neighb_atom ))   deallocate (Ac%neighb_atom)
         if (allocated(Ac%distance    ))   deallocate (Ac%distance   )
         if (allocated(Ac%trans       ))   deallocate (Ac%trans      )
         if (allocated(Ac%ddist       ))   deallocate (Ac%ddist      )
         if (allocated(Ac%ddlab       ))   deallocate (Ac%ddlab      )
         return
      end if

      natcel=nasu*mul
      id=nint(0.74048*(dmax/r_atom)**3)
      id=max(id,natcel)

      if (.not. allocated(Ac%Lab         ))   allocate (Ac%Lab          (natcel))
      if (.not. allocated(Ac%xyz         ))   allocate (Ac%xyz         (3,natcel))
      if (.not. allocated(Ac%charge      ))   allocate (Ac%charge        (natcel))
      if (.not. allocated(Ac%moment      ))   allocate (Ac%moment        (natcel))
      if (.not. allocated(Ac%var_free    ))   allocate (Ac%var_free    (5,natcel))
      if (.not. allocated(Ac%neighb      ))   allocate (Ac%neighb        (natcel))
      if (.not. allocated(Ac%neighb_atom ))   allocate (Ac%neighb_atom(id,natcel))
      if (.not. allocated(Ac%distance    ))   allocate (Ac%distance   (id,natcel))
      if (.not. allocated(Ac%trans       ))   allocate (Ac%trans    (3,id,natcel))
      if (.not. allocated(Ac%ddist       ))   allocate (Ac%ddist      (natcel*id))
      if (.not. allocated(Ac%ddlab       ))   allocate (Ac%ddlab      (natcel*id))

      Ac%nat         = natcel
      Ac%Lab         = " "
      Ac%xyz         = 0.0
      Ac%charge      = 0.0
      Ac%moment      = 0.0
      Ac%var_free    = 0.0
      Ac%neighb      = 0
      Ac%neighb_atom = 0
      Ac%distance    = 0.0
      Ac%trans       = 0
      Ac%ddist       = 0.0
      Ac%ddlab       = " "
   End Subroutine Allocate_Atoms_Cell

    !!----
    !!---- Module Subroutine Init_mAtom_Type(A)
    !!----    type (mAtom_Type),  intent(in out) :: A   ! In / Out -> mAtom type
    !!----
    !!----    Initialize mAtom_Type
    !!----
    !!---- Updated: November 3 - 2013
    !!
    Module Subroutine Init_mAtom_Type(A)
       !---- Arguments ----!
       type (mAtom_Type), intent(in out)   :: A

       A%Lab      =" "
       A%ChemSymb =" "
       A%SfacSymb =" "
       A%Wyck     ="."
       A%Active   =.true.
       A%Z =0; A%Mult=1
       A%X=0.0; A%X_Std=0.0; A%MX=0.0; A%LX=0
       A%Occ=0.0; A%Occ_Std=0.0; A%MOcc=0.0; A%LOcc=0
       A%Biso=0.0; A%Biso_std=0.0; A%MBiso=0.0; A%LBiso=0
       A%Utype    ="none"
       A%ThType   ="isotr"
       A%U=0.0; A%U_std=0.0; A%Ueq=0.0; A%MU=0.0; A%LU=0
       A%Charge=0.0; A%Moment=0.0
       A%Ind=0
       A%NVar=0; A%VarF=0.0
       A%AtmInfo  =" "
       !Magnetic parameters
       A%nvk =0
       A%imat=0
       A%SkR=0.0; A%SkR_std=0.0; A%Spher_SkR=0.0; A%Spher_SkR_std=0.0; A%mSkR=0.0; A%lSkR=0
       A%SkI=0.0; A%SkI_std=0.0; A%Spher_SkI=0.0; A%Spher_SkI_std=0.0; A%mSkI=0.0; A%lSkI=0
       A%mphas=0.0; A%mphas_std=0.0; A%mmphas=0.0; A%lmphas=0
       A%cbas=0.0; A%cbas_std=0.0; A%mbas=0.0; A%lbas=0
       A%chitype="none"
       A%chi=0.0; A%chi_std=0.0; A%mchi=0.0; A%lchi=0; A%Chieq=0.0

    End Subroutine Init_mAtom_Type

    !!----
    !!---- Module Subroutine Allocate_Matom_List(N,A)
    !!----    integer,                              intent(in)     :: n    !  In -> Number of elements of A
    !!----    type (mAtom_list_type),               intent(in out) :: A    !  In -> Objet to be allocated
    !!----    real(kind=cp), optional,              intent(in)     :: MField
    !!----    real(kind=cp), optional,dimension(3), intent(in)     :: dirF
    !!----
    !!----    Allocation of objet A of type mAtom_list. This subroutine
    !!----    should be called before using an object of type mAtom_list.
    !!----
    !!---- Updated: April - 2005, June -2014
    !!
    Module Subroutine Allocate_mAtom_List(N,A,MField,dirF)
       !---- Arguments ----!
       integer,                              intent(in)     :: n  !# atoms in asymmetric magnetic unit
       type (mAtom_list_type),               intent(in out) :: A  !Objet to be allocated
       real(kind=cp), optional,              intent(in)     :: MField
       real(kind=cp), optional,dimension(3), intent(in)     :: dirF

       !---- Local Variables ----!
       integer :: i

       A%natoms = n
       A%suscept=.false.
       A%MagField=0.0
       A%dir_MField=[0.0,0.0,1.0]
       if (allocated(A%Atom)) deallocate(A%Atom)
       allocate (A%Atom(n))
       if(present(MField)) then
          A%suscept=.true.
          A%MagField=MField
       end if
       if(present(dirF))   A%dir_MField=dirF

       do i=1,n
          call init_mAtom_type(A%Atom(i))
       end do

    End Subroutine Allocate_mAtom_List

    !!----
    !!---- Module Subroutine Deallocate_mAtom_list(A)
    !!----    type (mAtom_list_type), intent(in out)   :: A  ! In/ Out -> Objet to be deallocated
    !!----
    !!----    De-allocation of objet A of type atom_list. This subroutine
    !!----    should be invoked after using an object of type mAtom_list
    !!----    that is no more needed.
    !!----
    !!---- Update: April - 2005
    !!
    Module Subroutine Deallocate_mAtom_list(A)
       !---- Arguments ----!
       type (mAtom_list_type), intent(in out)   :: A  !Objet to be deallocated

       if (allocated(A%Atom)) deallocate (A%Atom)

    End Subroutine Deallocate_mAtom_list

End SubModule Atm_Allocating_Atoms