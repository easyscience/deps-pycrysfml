Submodule (CFML_Molecules) Mol_ZMatrix_To

   implicit none

 Contains
   !!----
   !!---- SUBROUTINE ZMATRIX_TO_CARTESIAN
   !!----
   !!----    Subroutine to transform the internal coordinates of a molecule from
   !!----    Z-matrix to cartesian coordinates.
   !!----
   !!----    If a second argument is present the subroutine creates a new molecule
   !!----    (copy of the old one) with cartesian coordinates, preserving
   !!----    the input molecule. Otherwise the input molecule is changed on output.
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine Zmatrix_to_Cartesian(Mol, NMol)
      !---- Arguments ----!
      type (Molecule_type),           intent(in out) :: Mol
      type (Molecule_type), optional, intent(   out) :: NMol

      !---- Local variables -----!
      integer                     :: i,na,j,k,n
      real(kind=cp)               :: dist, ang
      real(kind=cp), dimension(3) :: ci,ri,rj,rk,rn

      type (Molecule_type)        :: IMol

      !---- Controls ----!
      if (Mol%coor_type /= "Z") then
         call set_error(1, "Zmatrix_to_Cartesian: the input Mol is not a Z-matrix")
         return
      end if

      na= Mol%natoms
      if (na <= 0) then
         call set_error(1, "Zmatrix_to_Cartesian: Not atoms are defined")
         return
      end if

      call init_molecule(IMol,na)
      IMol=Mol


      !> First atom is always at origin (Z-matrix)
      IMol%I_coor(:,1) = 0.0_cp
      IMol%conn(:,1) = 0

      !> Second atom is always along "x"
      IMol%I_coor(2:3,2) = 0.0
      IMol%conn(2:3,2) = 0
      IMol%conn(1,2)   = 1

      !> Third atom is always in the "xy" plane       !A(i) d_ij  ang_ijk   dang_ijkl  j k l
      if (IMol%conn(1,3) == 1) then
         IMol%conn(2,3) = 2
         IMol%conn(3,3) = 0
         dist= IMol%I_coor(1,3)
         ang = IMol%I_coor(2,3)
         IMol%I_coor(1,3) = dist * cosd(ang)
         IMol%I_coor(2,3) = dist * sind(ang)
         IMol%I_coor(3,3) = 0.0_cp
      else
         IMol%conn(1,3) = 2
         IMol%conn(2,3) = 1
         IMol%conn(3,3) = 0
         dist= IMol%I_coor(1,3)
         ang = IMol%I_coor(2,3)
         IMol%I_coor(1,3) = dist * cosd(180.0_cp-ang) +  IMol%I_coor(1,2)
         IMol%I_coor(2,3) = dist * sind(180.0_cp-ang)
         IMol%I_coor(3,3) = 0.0_cp
      end if

      do i=4,na
         ci(:) = IMol%I_coor(:,i)
         j     = IMol%conn(1,i)         !The connectivity is needed for the Z-matrix description
         k     = IMol%conn(2,i)         !If the connectivity is given it is possible to transform to
         n     = IMol%conn(3,i)         !Z-matrix if cartesian/spherical coordinates are given.
         if (j == 0 .or. k == 0 .or. n == 0) cycle
         rj(:) = IMol%I_coor(:,j)
         rk(:) = IMol%I_coor(:,k)
         rn(:) = IMol%I_coor(:,n)
         ri =  get_cartesian_from_Z(ci,rj,rk,rn)
         IMol%I_coor(:,i) = ri
      end do
      IMol%coor_type="C"

      if (present(NMol)) then
         call Init_molecule(NMol,na)
         if (NMol%natoms <=0) then
            call set_error(1, "ZMatrix to Cartesian: The optional variable was not dimensioned!")
            return
         end if
         NMol=IMol
      else
         Mol=IMol
      end if

   End Subroutine Zmatrix_to_Cartesian

   !!----
   !!---- SUBROUTINE ZMATRIX_TO_FRACTIONAL
   !!----
   !!----    Subroutine to transform the internal coordinates of a molecule from
   !!----    Z-matrix to fractional coordinates.
   !!----
   !!----    If a third argument is present the subroutine creates a new molecule
   !!----    (copy of the old one) with fractional coordinates, preserving
   !!----    the input molecule in Z-matrix. Otherwise the input molecule is changed on output.
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine Zmatrix_to_Fractional(Mol, Cell, NMol)
      !---- Arguments ----!
      type (Molecule_type),           intent(in out) :: Mol
      type (Cell_G_Type),             intent(in    ) :: Cell
      type (Molecule_type), optional, intent(   out) :: NMol

      !---- Local variables -----!
      integer                       :: na
      type (Molecule_type)          :: IMol

      !> Controls
      if (Mol%coor_type /= "Z") then
         call set_error(1, "Zmatrix_to_Fractional: the input Mol is not in Zmatrix coordinates")
         return
      end if

      na=Mol%natoms
      if (na <= 0) then
         call set_error(1, "Zmatrix_to_Fractional: No atoms found")
         return
      end if

      call init_molecule(IMol,na)
      IMol= Mol
      call Zmatrix_to_Cartesian(IMol)
      call Cartesian_to_Fractional(IMol,cell)

      if (present(NMol)) then
         call Init_molecule(NMol,IMol%natoms)
         if (NMol%natoms <=0) then
            call set_error(1, "ZMatrix_to_Fractional: The optional variable was not dimensioned!")
            return
         end if
         NMol=IMol
      else
         Mol=IMol
      end if

   End Subroutine Zmatrix_to_Fractional

   !!----
   !!---- Subroutine Zmatrix_to_Spherical
   !!----
   !!----    Subroutine to transform the internal coordinates of a
   !!----    molecule from Zmatrix coordinates to  Spherical coordinaters.
   !!----
   !!----    If a second argument is present the subroutine creates a new
   !!----    molecule (copy of the old one) with Spherical coordinates,
   !!----    preserving the input molecule in Zmatrix Coordinates. Otherwise
   !!----    the input molecule is changed on output.
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine Zmatrix_to_Spherical(Mol, NMol)
      !---- Arguments ----!
      type (Molecule_type),           intent(in out) :: Mol
      type (Molecule_type), optional, intent(   out) :: NMol

      !---- Local Variables ----!
      integer                     :: na
      type (Molecule_type)        :: IMol

      !> Controls
      if (Mol%coor_type /= "Z") then
         call set_error (1, "Zmatrix_to_Spherical: the input Mol is not in Zmatrix coordinates")
         return
      end if

      na=Mol%natoms
      if (na <= 0) then
         call set_error(1, "Zmatrix_to_Fractional: No atoms found")
         return
      end if

      !> Step 1
      call init_Molecule(IMol,na)
      IMol=Mol
      call Zmatrix_to_Cartesian(IMol)
      if (err_CFML%IErr /= 0) then
         Err_CFML%Msg="Zmatrix_to_Spherical: Intermediate procedure fail (I)!"
         return
      end if

      !> Step 2
      call Cartesian_to_Spherical(IMol)
      if (err_CFML%IErr /= 0) then
         Err_CFML%Msg="Zmatrix_to_Spherical: Intermediate procedure fail (II)!"
         return
      end if

      !> Step 3
      if (present(NMol)) then
         call Init_molecule(NMol,na)
         if (NMol%natoms <=0) then
            call set_error(1, "ZMatrix to Spherical: The optional variable was not dimensioned!")
            return
         end if
         NMol=IMol
      else
         Mol=IMol
      end if

   End Subroutine Zmatrix_to_Spherical

   !!--++
   !!--++ Subroutine Get_Cartesian_from_Z(ci,ri,rj,rk,rn)
   !!--++    real, dimension(3), intent ( in) :: ci,rj,rj,rn
   !!--++    real, dimension(3), intent (out) :: ri
   !!--++
   !!--++    Subroutine to calculate the cartesian coordinates of an atom (i)
   !!--++    when its distance (dij=ci(1)) to another atom (j), the angle (aijk=ci(2))
   !!--++    spanned with another atom (k) centred at (j), the torsion angle
   !!--++    (bijkn=ci(3)) with a fourth atom (n) and the coordinates of
   !!--++    the three atoms (jkn), rj,rk,rn are all given.
   !!--++
   !!--<<    The algorithm used to determine the Cartesian coordinates of atom (i) is the
   !!--++    following:
   !!--++       - Select a local Cartesian frame with (j) at origin, x-axis along (jk),
   !!--++         z-axis perpendicular to the plane (jkn), y-axis right-handled frame
   !!--++            e1 = rjk/djk, e2 = e3 x e1,  e3= rjk x rkn / djk/dkn
   !!--++       - The above system determine a matrix M = (e1,e2,e3), with components ei in columns
   !!--++         that serves to transform interatomic vector components back to the original system.
   !!--++       - In the above system the coordinates of atom (i) is given by
   !!--++            ri = rj + M ui
   !!--++
   !!--++         where
   !!--++            ui = d ( cos(aijk), cos(bijkn) sin(aijk), sqrt(1 - cos(aijk)^2 -(cos(bijkn) sin(aijk))^2))
   !!-->>
   !!--++
   !!--++ Update: February - 2005
   !!
   Module Function Get_Cartesian_from_Z(ci,rj,rk,rn) Result(ri)
      !---- Arguments ----!
      real(kind=cp), dimension(3), intent ( in) :: ci,rj,rk,rn
      real(kind=cp), dimension(3)               :: ri

      !--- Local variables ---!
      real(kind=cp)                 :: ca,cb,sa
      real(kind=cp), dimension(3)   :: r,e1,e2,e3
      real(kind=cp), dimension(3,3) :: M

      ca = cosd(ci(2))                  ! cos(aijk)
      sa = sqrt(abs(1.0_cp - ca*ca))    ! sin(aijk)
      cb = cosd(ci(3))                  ! cos(bijkn)
      r(1) = ci(1) * ca                 ! Coordinates in the local system
      r(2) = ci(1)*cb*sa
      r(3) = ci(1)*sqrt(abs(1.0_cp - ca*ca - sa*sa*cb*cb )) *sign(1.0_cp,ci(3))

      e1  = rk - rj
      e1  = e1/sqrt(dot_product(e1,e1))
      e3  = cross_product( rk - rj, rn - rk)
      e3  = e3/sqrt(dot_product(e3,e3))
      e2  = cross_product( e3, e1)
      M(:,1) = e1
      M(:,2) = e2
      M(:,3) = e3

      ri = rj + matmul(M,r)

   End Function Get_Cartesian_from_Z

End Submodule Mol_ZMatrix_To