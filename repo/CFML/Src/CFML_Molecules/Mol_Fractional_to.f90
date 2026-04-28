Submodule (CFML_Molecules) Mol_Fractional_To

   implicit none

 Contains
   !!----
   !!---- Subroutine Fractional_to_Cartesian
   !!----
   !!----    Subroutine to transform the fractional coordinates to cartesian internal
   !!----    coordinates of a molecule.
   !!----
   !!----    If Newmolecule is present the subroutine creates a new molecule
   !!----    (copy of the old one) with cartesian coordinates, preserving
   !!----    the input molecule in fractional. Otherwise the input molecule is
   !!----    changed on output.
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine Fractional_to_Cartesian(Mol, Cell, NMol)
      !---- Arguments ----!
      type (Molecule_type),           intent(in out) :: Mol
      type (Cell_G_Type),             intent(in    ) :: Cell
      type (Molecule_type), optional, intent(   out) :: NMol

      !---- Local variables -----!
      integer                       :: i, na
      real(kind=cp)                 :: phi,theta,chi
      real(kind=cp), dimension(3)   :: ci,xi
      real(kind=cp), dimension(3,3) :: Eu
      type (Molecule_type)          :: IMol

      !> Controls
      if (Mol%coor_type /= "F") then
         call set_error(1, "Fractional_to_Cartesian: the input Mol is not in fractional coordinates")
         return
      end if

      na= Mol%natoms
      if (na <= 0) then
         call set_error(1, "Fractional_to_Cartesian: No atoms are defined")
         return
      end if

      call Init_molecule(IMol,na)
      IMol=Mol

      if (Mol%in_xtal) then
         if (IMol%is_EulerMat) then
            Eu=IMol%Euler
         else
            phi=IMol%orient(1)
            theta=IMol%orient(2)
            chi=IMol%orient(3)
            Eu = Set_Euler_matrix(IMol%rot_type,phi,theta,chi)
            IMol%Euler=Eu
            IMol%is_EulerMat=.true.
         end if

         !> IMol contains fractional coordinates
         do i=1,IMol%natoms
            xi=IMol%I_coor(:,i) - IMol%xcentre   !Fractional coordinates after removing translation
            ci=matmul(cell%Cr_Orth_cel,xi)       !Cartesian components in the Crystal Frame
            IMol%I_coor(1:3,i) = matmul(ci,Eu)   !Final Cartesian internal coordinates (use passive matrix!)
         end do

      else
         do i=1,IMol%natoms
            IMol%I_coor(:,i)=matmul(cell%cr_orth_cel,IMol%I_coor(:,i))
         end do
         call Fix_Orient_Cartesian(IMol)  ! Select the internal frame as needed for Z-matrices
      end if
      IMol%coor_type = "C"

      if (present(NMol)) then
         call Init_molecule(NMol,IMol%natoms)
         if (NMol%natoms <=0) then
            call set_error(1, "Fractional to Cartesian: The optional variable was not dimensioned!")
            return
         end if
         NMol=IMol
      else
         Mol=IMol
      end if

   End Subroutine Fractional_to_Cartesian

   !!----
   !!---- SUBROUTINE FRACTIONAL_TO_SPHERICAL
   !!----
   !!----    Subroutine to transform the internal coordinates of a
   !!----    molecule from Fractional coordinates to  Spherical coordinaters.
   !!----
   !!----    If a third argument is present the subroutine creates a new
   !!----    molecule (copy of the old one) with Spherical coordinates,
   !!----    preserving the input molecule in Fractional Coordinates. Otherwise
   !!----    the input molecule is changed on output.
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine Fractional_to_Spherical(Mol, Cell, NMol)
      !---- Arguments ----!
      type (Molecule_type),           intent(in out) :: Mol
      type (Cell_G_Type),             intent(in)     :: Cell
      type (Molecule_type), optional, intent(   out) :: NMol

      !---- Local Variables ----!
      integer                     :: na
      type (Molecule_type)        :: IMol

      !---- Controls ----!
      if (Mol%coor_type /= "F") then
         call set_error(1, "Fractional_to_Spherical: the input Mol is not in Fractional coordinates")
         return
      end if

      na= Mol%natoms
      if (na <= 0) then
         call set_error(1, "Fractional_to_Spherical: No atoms are defined")
         return
      end if

      !> Step 1
      call init_molecule(IMol,na)
      IMol= Mol
      call Fractional_to_Cartesian(IMol,Cell)
      if (err_CFML%IErr /= 0) then
         Err_CFML%Msg="Fractional_to_Spherical: Intermediate procedure fail (I)!"
         return
      end if

      !> Step 2
      call Cartesian_to_Spherical(IMol)
      if (err_CFML%IErr /= 0) then
         Err_CFML%Msg="Fractional_to_Spherical: Intermediate procedure fail (II)!"
         return
      end if

      !> Step 3
      if (present(NMol)) then
         call Init_molecule(NMol,IMol%natoms)
         if (NMol%natoms <=0) then
            call set_error(1, "Fractional to Spherical: The optional variable was not dimensioned!")
            return
         end if
         NMol=IMol
      else
         Mol=IMol
      end if

   End Subroutine Fractional_to_Spherical

   !!----
   !!---- Subroutine Fractional_to_Zmatrix
   !!----
   !!----    Subroutine to transform the internal coordinates of a
   !!----    molecule from Fractional coordinates to  Zmatrix coordinaters.
   !!----
   !!----    If a second argument is present the subroutine creates a new
   !!----    molecule (copy of the old one) with Zmatrix coordinates,
   !!----    preserving the input molecule in Fractional Coordinates. Otherwise
   !!----    the input molecule is changed on output.
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine Fractional_to_Zmatrix(Mol,Cell,NMol)
      !---- Arguments ----!
      type (Molecule_type),           intent(in out) :: Mol
      type (Cell_G_Type),             intent(in)     :: Cell
      type (Molecule_type), optional, intent(   out) :: NMol

      !---- Local Variables ----!
      integer                     :: na
      type (Molecule_type)        :: IMol

      !---- Controls ----!
      if (Mol%coor_type /= "F") then
         call set_error(1, "Fractional_to_Zmatrix: the input Mol is not in Fractional coordinates")
         return
      end if

      na= Mol%natoms
      if (na <= 0) then
         call set_error(1, "Fractional_to_Spherical: No atoms are defined")
         return
      end if

      !> Step 1
      call Init_Molecule(IMol,na)
      IMol=Mol
      call Fractional_to_Cartesian(IMol,Cell)
      if (err_CFML%Ierr /= 0) then
         Err_CFML%Msg="Fractional_to_Zmatrix: Intermediate procedure fail (I)!"
         return
      end if

      !Z Step 2
      call Cartesian_to_Zmatrix(IMol, Cell=Cell)  !The cell is needed to eventually take into account
      if (err_CFML%IErr /= 0) then                !a different Cartesian frame on the input Mol
         Err_CFML%Msg="Fractional_to_Zmatrix: Intermediate procedure fail (II)!"
         return
      end if

      !> Step 3
      if (present(NMol)) then
         call Init_molecule(NMol,na)
         if (NMol%natoms <=0) then
            call set_error(1, "Fractional to ZMatrix: The optional variable was not dimensioned!")
            return
         end if
         NMol=IMol
      else
         Mol=IMol
      end if

   End Subroutine Fractional_to_Zmatrix

End Submodule Mol_Fractional_to