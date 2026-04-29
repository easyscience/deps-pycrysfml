Submodule (CFML_Molecules) Mol_to_AtList

   implicit none

 Contains
   !!----
   !!---- SUBROUTINE MOLEC_TO_ATLIST
   !!----
   !!---- Subroutine to pass all information from Molecule_Type
   !!---- to AtList_Type.
   !!---- Coor_type determine the type of cordinates parameter
   !!---- in output. ("F", "S", "C", "Z")
   !!---- In general Cell if necessary to obtain on Output fractional
   !!---- coordinates or special case for ZMatrix.
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine Molec_to_AtList(Mol, Type_Atm, AtList, Coor_Type, Cell)
      !---- Arguments ----!
      type (Molecule_Type),         intent(in)   :: Mol
      character(len=*),             intent(in)   :: Type_Atm
      type (AtList_Type),           intent(out)  :: AtList
      character(len=*),   optional, intent(in)   :: Coor_type
      type (Cell_G_type), optional, intent(in)   :: Cell

      !---- Local Variables ----!
      character(len=1)      :: car
      integer               :: i,nat,d
      type (Molecule_Type)  :: IMol

      !> Init
      call clear_error()
      AtList%natoms=0

      !> Number of Atoms
      Nat=mol%natoms
      if (Nat <= 0) then
         call set_error(1, "Number of atoms in the Molecule was zero!")
         return
      end if

      car="F"
      if (present(coor_type)) car=adjustl(coor_type)

      !> Internal copy
      call init_molecule(IMol,nat)
      IMol=mol

      !> Conversion
      select case (car)
         case ("C")
            select case (mol%coor_type)
               case ("C")
                  ! No changes

               case ("F")
                  if (present(cell)) then
                     call Fractional_to_Cartesian(IMol,cell)
                     if (err_CFML%Ierr /= 0) then
                        call init_molecule(IMol)
                        return
                     end if
                  else
                     call set_error(1, "You need Cell object to move Fractional to Cartesian coordinates! " )
                     call init_molecule(IMol)
                     return
                  end if

               case ("S")
                  call Spherical_to_Cartesian(IMol)
                  if (err_CFML%Ierr /= 0) then
                     call init_molecule(IMol)
                     return
                  end if

               case ("Z")
                  call ZMatrix_to_Cartesian(IMol)
                  if (err_CFML%Ierr /= 0) then
                     call init_molecule(IMol)
                     return
                  end if
            end select

         case ("F")
            select case (mol%coor_type)
               case ("C")
                  if ( present(cell)) then
                     call Cartesian_to_Fractional(IMol,cell)
                     if (err_CFML%Ierr /= 0) then
                        call init_molecule(IMol)
                        return
                     end if
                  else
                     call set_error(1, "You need Cell object to move Cartesian to Fractional coordinates! " )
                     call init_molecule(IMol)
                     return
                  end if

               case ("F")
                  ! No changes

               case ("S")
                  if (present(cell)) then
                     call Spherical_to_Fractional(IMol,cell)
                     if (err_CFML%Ierr /= 0) then
                        call init_molecule(IMol)
                        return
                     end if
                  else
                     call set_error(1,"You need Cell object to move Spherical to Fractional coordinates! ")
                     call init_molecule(IMol)
                     return
                  end if

               case ("Z")
                  if (present(cell)) then
                     call ZMatrix_to_Fractional(IMol,cell)
                     if (err_CFML%Ierr /= 0) then
                        call init_molecule(IMol)
                        return
                     end if
                  else
                     call set_error(1,"You need Cell object to move Zmatrix to Fractional coordinates! ")
                     call init_molecule(IMol)
                     return
                  end if
            end select

         case ("S")
            select case (mol%coor_type)
               case ("C")
                  call Cartesian_to_Spherical(IMol)
                  if (err_CFML%Ierr /= 0) then
                     call init_molecule(IMol)
                     return
                  end if

               case ("F")
                  if (present(cell)) then
                     call Fractional_to_Spherical(IMol,cell)
                     if (err_CFML%Ierr /= 0) then
                        call init_molecule(IMol)
                        return
                     end if
                  else
                     call set_error(1,"You need Cell object to move Fractional to Spherical coordinates! ")
                     call init_molecule(IMol)
                     return
                  end if

               case ("S")
                  ! No changes

               case ("Z")
                  call ZMatrix_to_Spherical(IMol)
                  if (err_CFML%Ierr /= 0) then
                     call init_molecule(IMol)
                     return
                  end if
            end select

         case ("Z")
            select case (mol%coor_type)
               case ("C")
                  if (present(cell)) then
                     call Cartesian_to_ZMatrix(IMol,cell=cell)
                  else
                     call Cartesian_to_ZMatrix(IMol)
                  end if
                  if (err_CFML%Ierr /= 0) then
                     call init_molecule(IMol)
                     return
                  end if

               case ("F")
                  if (present(cell)) then
                     call Fractional_to_ZMatrix(IMol,cell)
                     if (err_CFML%Ierr /= 0) then
                        call init_molecule(IMol)
                        return
                     end if
                  else
                     call set_error(1,"You need Cell object to move Fractional to ZMatrix coordinates! ")
                     call init_molecule(IMol)
                     return
                  end if

               case ("S")
                  if (present(cell)) then
                     call Spherical_to_ZMatrix(IMol,cell=cell)
                  else
                     call Spherical_to_ZMatrix(IMol)
                  end if
                  if (err_CFML%Ierr /= 0) then
                     call init_molecule(IMol)
                     return
                  end if

               case ("Z")
                  ! No changes
            end select

      end select

      !>Allocating Atom_List_Type
      d=0   ! k vectors
      call Allocate_Atom_List(Nat, AtList, Type_atm, d)

      !> Passing Information

      !> Default (Atm_Type)
      AtList%Atom(1:Nat)%Lab      =IMol%AtName(1:Nat)
      AtList%Atom(1:Nat)%SfacSymb =IMol%AtSymb(1:Nat)
      AtList%Atom(1:Nat)%Z        =IMol%AtZ(1:Nat)
      AtList%Atom(1:Nat)%Mult     =1
      AtList%Atom(1:Nat)%Charge   =0.0_cp
      AtList%Atom(1:Nat)%U_iso    =IMol%U_iso(1:Nat)
      AtList%Atom(1:Nat)%Occ      =IMol%Occ(1:Nat)
      AtList%Atom(1:Nat)%Utype    =" "
      AtList%Atom(1:Nat)%ThType   ="iso"
      AtList%Atom(1:Nat)%Magnetic =.false.
      AtList%Atom(1:Nat)%Mom      =0.0_cp
      AtList%Atom(1:Nat)%AtmInfo  =" "
      AtList%Atom(1:Nat)%wyck     =" "
      AtList%Atom(1:Nat)%Active   =.true.

      do i=1,Nat
         AtList%Atom(i)%ChemSymb = Get_Chem_Symb(AtList%Atom(i)%SfacSymb)
         AtList%Atom(i)%X=IMol%I_Coor(:,i)
         AtList%Atom(i)%U     =0.0_cp
         AtList%Atom(i)%Moment=0.0_cp
         AtList%Atom(i)%Ind_ff=0
         AtList%Atom(i)%VarF  =0.0_cp
      end do

      !> Class Atm_Std_Type
      select type(A => AtList%Atom)
         class is (Atm_Std_Type)
            A(1:Nat)%Occ_Std   =0.0_cp
            A(1:Nat)%U_iso_std =0.0_cp
            do i=1,Nat
               A(i)%X_Std=0.0_cp
               A(i)%U_Std=0.0_cp
               A(i)%Moment_Std=0.0_cp
            end do
      end select

      !> Class Atm_Ref_Type
      select type(A => AtList%Atom)
         type is (Atm_Ref_Type)
            A(1:Nat)%M_Occ     =IMol%mOcc(1:Nat)
            A(1:Nat)%L_Occ     =IMol%lOcc(1:Nat)
            A(1:Nat)%M_U_iso  =IMol%mU_iso(1:Nat)
            A(1:Nat)%L_U_iso  =IMol%lU_iso(1:Nat)
            do i=1,Nat
               A(i)%m_X=IMol%mI_Coor(:,i)
               A(i)%l_X=IMol%lI_Coor(:,i)
               A(i)%m_moment=0.0_cp
               A(i)%l_moment=0
               A(i)%m_U=0.0_cp
               A(i)%l_U=0
            end do

         class is (ModAtm_Std_Type)
      end select

      !> Type ModAtm_Ref_Type
      select type (A => AtList%Atom)
         type is (ModAtm_Ref_Type)
            A(1:Nat)%M_Occ     =IMol%mOcc(1:Nat)
            A(1:Nat)%L_Occ     =IMol%lOcc(1:Nat)
            A(1:Nat)%M_U_iso  =IMol%mU_iso(1:Nat)
            A(1:Nat)%L_U_iso  =IMol%lU_iso(1:Nat)
            do i=1,Nat
               A(i)%m_X=IMol%mI_Coor(:,i)
               A(i)%l_X=IMol%lI_Coor(:,i)
               A(i)%m_U=0.0_cp
               A(i)%l_U=0
            end do

      end select

   End Subroutine Molec_to_AtList

   !!----
   !!---- Subroutine MolCrystal_to_AtList
   !!----
   !!---- Subroutine to pass all information from MolCrystal_Type
   !!---- to AtList_Type
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine MolCrystal_to_AtList(Molcrys, AtList)
      !---- Arguments ----!
      type (MolCrystal_Type), intent(in)  :: Molcrys
      type (AtList_Type),     intent(out) :: AtList

      !---- Local variables ----!
      integer               :: i, j, n, d
      integer               :: Nat, NaF, NMol
      type (AtList_Type)    :: A
      character(len=:), allocatable :: Atm_typ

      !> Init
      call clear_error()

      !> Number of Atoms
      Nat=0
      NaF=molcrys%n_free
      NMol=molcrys%n_mol
      if (NMol > 0) then
         Nat=NaF+sum(molcrys%mol(1:NMol)%natoms)
      else
         Nat=NaF
      end if
      if (Nat <= 0) then
         call set_error(1, " No atoms were defined in the MolCrystal object!")
         return
      end if


      !>Allocating Atom_List_Type
      d=0   ! k vectors
      select type (Atm => MolCrys%Atm)
         type is (Atm_Std_Type)
            call Allocate_Atom_List(Nat, AtList, 'Atm_Std_Type', d)
            Atm_typ='Atm_Std_Type'
         type is (Atm_Ref_Type)
            call Allocate_Atom_List(Nat, AtList, 'Atm_Ref_Type', d)
            Atm_typ='Atm_Ref_Type'
         type is (ModAtm_Std_Type)
            call Allocate_Atom_List(Nat, AtList, 'ModAtm_Std_Type', d)
            Atm_typ='ModAtm_Std_Type'
         type is (ModAtm_Ref_Type)
            call Allocate_Atom_List(Nat, AtList, 'ModAtm_Ref_Type', d)
            Atm_typ='ModAtm_Ref_Type'
      end select

      !> Fill information from Molecules Part
      n=0
      do i=1,NMol
         if (molcrys%mol(i)%natoms <= 0) cycle

         call Molec_to_AtList(molcrys%mol(i),Atm_typ,A,"F", molcrys%cell)
         if (err_CFML%Ierr /= 0) return

         if (A%natoms <= 0) cycle
         !AtList%Atom(n+1:n+A%natoms)=A%Atom(1:A%natoms)
         !this assignment is not compiled by gfortran -> individual components
         !should be used, otherwise the following construct does the work!

         select type (Atm => AtList%Atom)
            type is (Atm_Std_Type)
               Select Type (a_atom => A%Atom)
                  Type is(Atm_Std_Type)
                  Atm(n+1:n+A%natoms)=a_atom(1:A%natoms)
               End Select
            type is (Atm_Ref_Type)
               Select Type (a_atom => A%Atom)
                  Type is(Atm_Ref_Type)
                  Atm(n+1:n+A%natoms)=a_atom(1:A%natoms)
               End Select
            type is (ModAtm_Std_Type)
               Select Type (a_atom => A%Atom)
                  Type is(ModAtm_Std_Type)
                  Atm(n+1:n+A%natoms)=a_atom(1:A%natoms)
               End Select
            type is (ModAtm_Ref_Type)
               Select Type (a_atom => A%Atom)
                  Type is(ModAtm_Ref_Type)
                  Atm(n+1:n+A%natoms)=a_atom(1:A%natoms)
               End Select
         end select
         n=n+A%natoms
         call allocate_atom_list(0,A,' ',0)
      end do

      !> Fill information from Free atoms Part
      if (NaF > 0) then
         select type (Atm => AtList%Atom)
            type is (Atm_Std_Type)
               Select Type (a_atom => molcrys%atm)
                  Type is(Atm_Std_Type)
                  Atm(n+1:n+NaF)=a_atom(1:NaF)
               End Select
            type is (Atm_Ref_Type)
               Select Type (a_atom => molcrys%atm)
                  Type is(Atm_Ref_Type)
                  Atm(n+1:n+NaF)=a_atom(1:A%natoms)
               End Select
            type is (ModAtm_Std_Type)
               Select Type (a_atom => molcrys%atm)
                  Type is(ModAtm_Std_Type)
                  Atm(n+1:n+NaF)=a_atom(1:A%natoms)
               End Select
            type is (ModAtm_Ref_Type)
               Select Type (a_atom => molcrys%atm)
                  Type is(ModAtm_Ref_Type)
                  Atm(n+1:n+NaF)=a_atom(1:A%natoms)
               End Select
         End Select
      end if

   End Subroutine MolCrystal_to_AtList

End Submodule Mol_to_AtList