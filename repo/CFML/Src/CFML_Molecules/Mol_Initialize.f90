Submodule (CFML_Molecules) Mol_Initialize

   implicit none

 Contains
   !!----
   !!---- Subroutine Init_Molecule(Molecule,Natm)
   !!----
   !!----    Initialize the Variable Molecule
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine Init_Molecule(Mol, Natm)
      !---- Argument ----!
      type(Molecule_Type), intent(in out) :: Mol
      integer, optional,   intent(in)     :: Natm

      mol%name_mol   =" "
      mol%natoms     =0

      mol%in_xtal    = .false.
      mol%is_eulerMat= .false.
      mol%is_connect = .false.
      mol%rot_type   =" "
      mol%coor_type  =" "
      mol%therm_type =" "

      mol%xcentre    =0.0_cp
      mol%mxcentre   =0.0_cp
      mol%lxcentre   =0

      mol%orient     =0.0_cp
      mol%mOrient    =0.0_cp
      mol%lorient    =0

      mol%t_tls      =0.0_cp
      mol%mT_TLS     =0.0_cp
      mol%lt_tls     =0

      mol%l_tls      =0.0_cp
      mol%mL_TLS     =0.0_cp
      mol%ll_tls     =0

      mol%s_tls      =0.0_cp
      mol%mS_TLS     =0.0_cp
      mol%ls_tls     =0

      mol%euler      =0.0_cp

      if (allocated(mol%AtName))  deallocate(mol%AtName)
      if (allocated(mol%AtSymb))  deallocate(mol%AtSymb)
      if (allocated(mol%AtZ))     deallocate(mol%AtZ)
      if (allocated(mol%Ptr))     deallocate(mol%Ptr)
      if (allocated(mol%I_Coor))  deallocate(mol%I_Coor)
      if (allocated(mol%mI_Coor)) deallocate(mol%mI_Coor)
      if (allocated(mol%lI_Coor)) deallocate(mol%lI_Coor)
      if (allocated(mol%U_iso))    deallocate(mol%U_iso)
      if (allocated(mol%mU_iso))   deallocate(mol%mU_iso)
      if (allocated(mol%lU_iso))   deallocate(mol%lU_iso)
      if (allocated(mol%Occ))     deallocate(mol%Occ)
      if (allocated(mol%mocc))    deallocate(mol%mocc)
      if (allocated(mol%lOcc))    deallocate(mol%lOcc)
      if (allocated(mol%Nb))      deallocate(mol%Nb)
      if (allocated(mol%INb))     deallocate(mol%INb)
      if (allocated(mol%Tb))      deallocate(mol%Tb)
      if (allocated(mol%Conn))    deallocate(mol%Conn)

      if (present(natm)) then
         if (natm > 0) then
            mol%natoms=natm

            allocate(mol%AtName(natm))
            allocate(mol%AtSymb(natm))
            allocate(mol%AtZ(natm))
            allocate(mol%Ptr(2,natm))
            allocate(mol%I_Coor(3,natm))
            allocate(mol%mI_Coor(3,natm))
            allocate(mol%lI_Coor(3,natm))
            allocate(mol%U_iso(natm))
            allocate(mol%mU_iso(natm))
            allocate(mol%lU_iso(natm))
            allocate(mol%Occ(natm))
            allocate(mol%mocc(natm))
            allocate(mol%lOcc(natm))
            allocate(mol%Nb(natm))
            allocate(mol%INb(10,natm))
            allocate(mol%Tb(10,natm))
            allocate(mol%Conn(3,natm))

            mol%AtName  =" "
            mol%AtSymb  =" "
            mol%AtZ     =0
            mol%Ptr     =0
            mol%I_Coor  =0.0_cp
            mol%mI_Coor =0.0_cp
            mol%lI_Coor =0
            mol%U_iso   =0.0_cp
            mol%mU_iso  =0.0_cp
            mol%lU_iso  =0
            mol%Occ     =0.0_cp
            mol%mocc    =0.0_cp
            mol%lOcc    =0
            mol%Nb      =0
            mol%INb     =0
            mol%Tb      =0
            mol%Conn    =0

         end if
      end if

   End Subroutine Init_Molecule

   !!----
   !!---- Subroutine Init_MolCrystal
   !!----
   !!----    Initialization for Molecular Crystal object
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine Init_MolCrystal(MolX, NMol, NAtm, AtmType)
      !---- Argument ----!
      type(MolCrystal_Type),      intent(out) :: MolX
      integer,          optional, intent(in)  :: Nmol       !Molucule object
      integer,          optional, intent(in)  :: NAtm       !Free atoms
      character(len=*), optional, intent(in)  :: Atmtype

      !---- Local Variables ----!
      integer :: i
      type(Atm_Ref_Type)    :: at1
      type(ModAtm_Ref_Type) :: at2

      molx%N_Free    = 0
      molx%N_Mol     = 0
      molx%N_Species = 0
      molx%Npat      = 0

      if (allocated(molx%mol))  deallocate(molx%mol)
      if (allocated(molx%atm))  deallocate(molx%atm)

      if (present(Nmol) .and. nmol > 0) then
         molx%N_Mol = nmol
         allocate(molx%mol(nmol))
         do i=1,nmol
            call init_molecule(molx%mol(i))
         end do
      end if

      if (present(Natm) .and. natm > 0) then
         molx%N_Free = natm
         if (present(Atmtype) ) then
            if (u_case(trim(AtmType)) == 'MAG') then
               allocate(molx%atm(natm), source=at2)
            else
               allocate(molx%atm(natm), source=at1)
            end if
         else
            allocate(molx%atm(natm), source=at1)
         end if

         do i=1,natm
            call init_atom_type(molx%atm(i),0)
         end do
      end if

   End Subroutine Init_MolCrystal

End Submodule Mol_Initialize