Submodule (CFML_Molecules) Mol_Spherical_To

   implicit none

 Contains
   !!----
   !!---- Subroutine Spherical_to_Cartesian
   !!----
   !!----    Subroutine to transform the internal coordinates of a molecule from Spherical
   !!----    coordinates to  cartesian coordinaters.
   !!----
   !!----    If a second argument is present the subroutine creates a new molecule
   !!----    (copy of the old one) with spherical coordinates, preserving
   !!----    the input molecule in Cartesian Coordinates. Otherwise the input
   !!----    molecule is changed on output.
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine Spherical_to_Cartesian(Mol,NMol)
      !---- Arguments ----!
      type (Molecule_type),           intent(in out) :: Mol
      type (Molecule_type), optional, intent(   out) :: NMol

      !---- Local variables -----!
      integer                     :: i,na
      real(kind=cp)               :: r, theta, phi

      type (Molecule_type)        :: IMol

      !---- Controls ----!
      if (Mol%coor_type /= "S") then
         call set_error(1, "Spherical_to_Cartesian: the input Mol is not in Spherical coordinates")
         return
      end if

      na= Mol%natoms
      if (na <= 0) then
         call set_error(1, "Spherical_to_Cartesian: No atoms are defined")
         return
      end if

      call init_molecule(IMol,na)
      IMol=Mol

      !---- Start calculations for each atom of the Mol ----!
      do i=1,na
         r     = Mol%I_coor(1,i)
         theta = Mol%I_coor(2,i)
         phi   = Mol%I_coor(3,i)
         IMol%I_coor(1,i) = r*sind(theta)*cosd(phi)
         IMol%I_coor(2,i) = r*sind(theta)*sind(phi)
         IMol%I_coor(3,i) = r*cosd(theta)
      end do
      IMol%coor_type="C"

      if (present(NMol)) then
         call Init_molecule(NMol,IMol%natoms)
         if (NMol%natoms <=0) then
            call set_error(1, "Spherical to Cartesian: The optional variable was not dimensioned!")
            return
         end if
         NMol=IMol
      else
         Mol=IMol
      end if

   End Subroutine Spherical_to_Cartesian

   !!----
   !!---- Subroutine Spherical_to_Fractional
   !!----
   !!----    Subroutine to transform the internal coordinates of a
   !!----    molecule from Spherical coordinates to  Fractional coordinaters.
   !!----
   !!----    If a second argument is present the subroutine creates a new
   !!----    molecule (copy of the old one) with Fractional coordinates,
   !!----    preserving the input molecule in Spherical Coordinates. Otherwise
   !!----    the input molecule is changed on output.
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine Spherical_to_Fractional(Mol, Cell, NMol)
      !---- Arguments ----!
      type (Molecule_type),           intent(in out) :: Mol
      type (Cell_G_Type),             intent(in)     :: Cell
      type (Molecule_type), optional, intent(   out) :: NMol

      !---- Local Variables ----!
      integer                     :: na
      type (Molecule_type)        :: IMol

      !---- Controls ----!
      if (Mol%coor_type /= "S") then
         call set_error(1, "Spherical_to_Fractional: the input Mol is not in Spherical coordinates")
         return
      end if

      na= Mol%natoms
      if (na <= 0) then
         call set_error(1, "Spherical_to_Fractional: No atoms are defined")
         return
      end if

      !> Step 1
      call init_molecule(IMol,na)
      IMol=Mol
      call Spherical_to_Cartesian(IMol)
      if (err_CFML%Ierr /= 0) then
         Err_CFML%Msg="Spherical_to_Fractional: Intermediate procedure fail (I)!"
         return
      end if

      !> Step 2
      call Cartesian_to_Fractional(IMol,Cell)
      if (err_CFML%Ierr /= 0) then
         Err_CFML%Msg="Spherical_to_Fractional: Intermediate procedure fail (II)!"
         return
      end if

      !> Step 3
      if (present(NMol)) then
         call Init_molecule(NMol,na)
         if (NMol%natoms <=0) then
            call set_error(1, "Spherical to Fractional: The optional variable was not dimensioned!")
            return
         end if
         NMol=IMol
      else
         Mol=IMol
      end if

   End Subroutine Spherical_to_Fractional

   !!----
   !!---- Subroutine Spherical_to_Zmatrix
   !!----
   !!----    Subroutine to transform the internal coordinates of a
   !!----    molecule from Spherical coordinates to  Zmatrix coordinaters.
   !!----
   !!----    If a second argument is present the subroutine creates a new
   !!----    molecule (copy of the old one) with Zmatrix coordinates,
   !!----    preserving the input molecule in Spherical Coordinates. Otherwise
   !!----    the input molecule is changed on output.
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine Spherical_to_Zmatrix(Mol, Cell, NMol)
      !---- Arguments ----!
      type (Molecule_type),           intent(in out) :: Mol
      Type (Cell_G_Type),   optional, intent(in)     :: Cell
      type (Molecule_type), optional, intent(   out) :: NMol

      !---- Local Variables ----!
      integer                     :: na
      type (Molecule_type)        :: IMol

      !> Controls
      if (Mol%coor_type /= "S") then
         call set_error(1, "Spherical_to_ZMatrix: the input Mol is not in Spherical coordinates")
         return
      end if

      na= Mol%natoms
      if (na <= 0) then
         call set_error(1, "Spherical_to_ZMatrix: No atoms are defined")
         return
      end if

      !> Step 1
      call init_molecule(IMol,na)
      IMol= Mol
      call Spherical_to_Cartesian(IMol)
      if (err_CFML%IErr /=0) then
         Err_CFML%Msg="Spherical_to_Zmatrix: Intermediate procedure fail (I)!"
         return
      end if

      !> Step 2
      if (present(Cell)) then
         call Cartesian_to_Zmatrix(IMol,Cell=Cell)
      else
         call Cartesian_to_Zmatrix(IMol)
      end if
      if (err_CFML%IErr /= 0) then
         Err_CFML%Msg="Spherical_to_Zmatrix: Intermediate procedure fail (II)!"
         return
     end if

      !> Step 3
      if (present(NMol)) then
         call Init_molecule(NMol,na)
         if (NMol%natoms <=0) then
            call set_error(1, "Spherical to ZMatrix: The optional variable was not dimensioned!")
            return
         end if
         NMol=IMol
      else
         Mol=IMol
      end if

   End Subroutine Spherical_to_Zmatrix

End Submodule Mol_Spherical_To