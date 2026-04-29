Submodule (CFML_Molecules) Mol_Cartesian_To

   implicit none

 Contains
   !!----
   !!---- SUBROUTINE CARTESIAN_TO_FRACTIONAL
   !!----
   !!----    Subroutine to transform the internal coordinates of a
   !!----    molecule from cartesian coordinates to  fractional coordinates.
   !!--..
   !!----    If a third argument is present the subroutine creates a new
   !!----    molecule (copy of the old one) with fractional coordinates,
   !!----    preserving the input molecule in Cartesian Coordinates. Otherwise
   !!----    the input molecule is changed on output.
   !!----
   !!--..       Xc= Euler.Xic  (Cartesian in the crystal frame)
   !!--..       xf= Orth_Cr_cel Xc (fractional before translating to the centre)
   !!--..       Xf = Orth_Cr_cel (Euler.Xic) + Xo (final fractional coordinates)
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine Cartesian_to_Fractional(Mol, Cell, NMol)
      !---- Arguments ----!
      type (Molecule_type),  intent(in out)           :: Mol
      type (Cell_G_Type),    intent(in)               :: Cell
      type (Molecule_type),  optional, intent(   out) :: NMol

      !---- Local Variables ----!
      integer                       :: i,na
      real(kind=cp)                 :: phi,theta,chi
      real(kind=cp), dimension(3)   :: ci,xi
      real(kind=cp), dimension(3,3) :: Eu
      type (Molecule_type)          :: Imol

      !> Controls
      call clear_error()
      if (mol%coor_type /= "C") then
         call Set_Error(1, "Cartesian_to_Fractional: the input molecule is not in Cartesian coordinates")
         return
      end if

      na=mol%natoms
      if (na <= 0) then
         call Set_Error(1, "Cartesian_to_Fractional: No atoms are defined on molecule variable")
         return
      end if

      if (.not. mol%in_xtal) then
         call Set_Error(1, "Cartesian_to_Fractional: The input molecule haven't crystal information" )
         return
      end if

      !> Step 1
      call init_molecule(Imol,na)
      Imol=mol

      !>  Frame after a rotation defined by the matrix M(theta,phi,Chi)
      phi   = Imol%orient(1)
      theta = Imol%orient(2)
      chi   = Imol%orient(3)
      if (Imol%is_EulerMat) then
         Eu=Imol%Euler
      else
         Eu = Set_Euler_matrix(Imol%rot_type,phi,theta,chi)
         Imol%Euler=Eu
         Imol%is_EulerMat=.true.
      end if

      do i=1,na
         ci=matmul(Eu,Imol%I_coor(:,i))           !Cartesian components in the Crystal Frame
         xi=matmul(cell%Orth_Cr_cel,ci)           !Fractional coordinates before translation
         Imol%I_coor(:,i) = Imol%xcentre + xi     !Final fractional coordinates
      end do
      Imol%coor_type = "F"

      !> Step 3
      if (present(Nmol)) then
         call Init_molecule(Nmol,na)
         if (Nmol%natoms <=0) then
            call set_error (1, "Cartesian_to_Fractional: The optional variable was not dimensioned!")
            return
         end if
         Nmol=Imol
      else
         Mol=Imol
      end if

   End Subroutine Cartesian_to_Fractional

   !!----
   !!---- Subroutine Cartesian_to_Spherical
   !!----
   !!----    Subroutine to transform the internal coordinates of a
   !!----    molecule from cartesian coordinates to  spherical coordinaters.
   !!----
   !!----    If a second argument is present the subroutine creates a new
   !!----    molecule (copy of the old one) with spherical coordinates,
   !!----    preserving the input molecule in Cartesian Coordinates. Otherwise
   !!----    the input molecule is changed on output.
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine Cartesian_to_Spherical(Mol, NMol)
      !---- Arguments ----!
      type (Molecule_type), intent(in out)           :: Mol
      type (Molecule_type), intent(   out), optional :: NMol

      !---- Local variables -----!
      integer                     :: i,na
      type (Molecule_type)        :: Imol

      !> Controls
      if (mol%coor_type /= "C") then
         call set_error(1, "Cartesian_to_Spherical: the input molecule is not in Cartesian coordinates")
         return
      end if

      na= mol%natoms
      if (na <= 0) then
         call set_error(1, "Cartesian_to_Spherical: No atoms are defined")
         return
      end if

      !> Start calculations for each atom of the molecule
      call init_molecule(Imol,na)
      Imol=mol

      do i=1,na
         Imol%I_Coor(:,i)=  Get_Spher_from_Cart(mol%I_Coor(:,i),"D")
      end do
      Imol%coor_type="S"

      if (present(Nmol)) then
         call Init_molecule(NMol,na)
         if (NMol%natoms <=0) then
            call set_error(1,"Cartesian_to_Spherical: The optional variable was not dimensioned!")
            return
         end if
         NMol=Imol
      else
         mol=Imol
      end if

   End Subroutine Cartesian_to_Spherical

   !!----
   !!---- Subroutine Cartesian_to_Zmatrix
   !!----
   !!----    Subroutine to transform the internal coordinates of a molecule
   !!----    from cartesian coordinates to  Z-matrix.
   !!----
   !!----    If a second argument is present the subroutine creates a new
   !!----    molecule (copy of the old one) with Z-matrix, preserving
   !!----    the input molecule in Cartesian Coordinates. Otherwise the input
   !!----    molecule is changed on output.
   !!----
   !!----    The input cartesian coordinates may be defined with respect to another
   !!----    internal frame. The final internal frame is that defined for Z-matrices:
   !!----    the x-axis is from the first to the second atom and the x-y plane is formed
   !!----    by the three first atoms. The Euler matrix and the molecular centre in the
   !!----    crystallographic system is changed in consequence.
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine Cartesian_to_Zmatrix(Mol, NMol, Cell, Dmin, Dmax)
      !---- Arguments ----!
      type (Molecule_type),           intent(in out) :: Mol
      type (Molecule_type), optional, intent(   out) :: NMol
      Type (Cell_G_Type),   optional, intent(in)     :: Cell
      real(kind=cp),        optional, intent(in)     :: Dmin
      real(kind=cp),        optional, intent(in)     :: Dmax

      !---- Local variables -----!
      integer                       :: i,na,j,k,n,mode
      real(kind=cp)                 :: dist, ang, phi, theta, chi
      real(kind=cp), dimension(3)   :: ci,ri,rj,rk,rn,u1,u2,u3
      real(kind=cp), dimension(3,3) :: Mat, Eu
      type (Molecule_type)          :: Imol

      !> Controls
      if (mol%coor_type /= "C") then
         call set_error(1, "Cartesian_to_Zmatrix: the input molecule is not in Cartesian coordinates")
         return
      end if

      na= mol%natoms
      if (na <= 0) then
         call set_error(1, "Cartesian_to_Zmatrix: Not atoms are defined" )
         return
      end if

      if (na < 3) then
         call set_error(1, "Cartesian_to_Zmatrix: You need at least three atoms")
         return
      end if

      !> Call Connectivity if necessary
      if (.not. mol%is_connect) then
         mode=0
         if (present(dmin)) mode=1
         if (present(dmax)) mode=mode + 2
         select case (mode)
            case (0)
               call create_connectivity_cartesian(mol)
            case (1)
               call create_connectivity_cartesian(mol,dmin=dmin)
            case (2)
               call create_connectivity_cartesian(mol,dmax=dmax)
            case (3)
               call create_connectivity_cartesian(mol,dmin=dmin,dmax=dmax)
         end select
         if (err_CFML%Ierr /= 0) then
            call set_error(1, "Cartesian_to_Zmatrix: the connectivity is wrong")
            return
         end if
         mol%is_connect=.true.
      end if

      !> Start calculations for each atom of the mol
      call init_molecule(Imol,na)
      Imol=mol

      !> First atom is always at origin (Z-matrix)
      Imol%I_Coor(:,1) = 0.0_cp
      Imol%conn(:,1)   = 0

      !> Second atom is always along "x"
      ri=mol%I_coor(:,2)-mol%I_coor(:,1)
      dist=sqrt(dot_product(ri,ri))
      Imol%I_Coor(1,2)   = dist
      Imol%I_Coor(2:3,2) = 0.0_cp
      Imol%conn(2:3,2)   = 0
      Imol%conn(1,2)     = 1

      !> Third atom is always in the "xy" plane
      !> A(i) d_ij  ang_ijk   dang_ijkl  j k l
      if (Imol%conn(1,3) == 1) then
         Imol%conn(2,3) = 2
         Imol%conn(3,3) = 0
         ri=mol%I_coor(:,3)-mol%I_coor(:,1)
         rj=mol%I_coor(:,2)-mol%I_coor(:,1)
         dist= sqrt(dot_product(ri,ri))
         ang = acosd(dot_product(ri,rj)/dist/sqrt(dot_product(rj,rj)))
         Imol%I_coor(1,3) = dist
         Imol%I_coor(2,3) = ang
         Imol%I_coor(3,3) = 0.0_cp

      else
         Imol%conn(1,3) = 2
         Imol%conn(2,3) = 1
         Imol%conn(3,3) = 0
         ri=mol%I_coor(:,3)-mol%I_coor(:,2)
         rj=mol%I_coor(:,1)-mol%I_coor(:,2)
         dist= sqrt(dot_product(ri,ri))
         ang = acosd(dot_product(ri,rj)/dist/sqrt(dot_product(rj,rj)))
         Imol%I_coor(1,3) = dist
         Imol%I_coor(2,3) = ang
         Imol%I_coor(3,3) = 0.0_cp
      end if

      if (mol%in_xtal) then    !Modify the Euler matrix, orientation angles and centre
         if (mol%is_EulerMat) then
            Eu=mol%Euler
         else
            phi=mol%orient(1)
            theta=mol%orient(2)
            chi=mol%orient(3)
            Eu = Set_Euler_matrix(mol%rot_type,phi,theta,chi)
         end if
         Imol%Euler=Eu
         Imol%is_EulerMat=.true.

         ri=mol%I_coor(:,1)
         rj=mol%I_coor(:,2)
         rk=mol%I_coor(:,3)
         u1=rj-ri
         u1=u1/sqrt(dot_product(u1,u1))
         u3=cross_product(u1,rk-ri)
         u3=u3/sqrt(dot_product(u3,u3))
         u2=cross_product(u3,u1)
         Mat(:,1)=u1
         Mat(:,2)=u2  !Active matrix needed to get the new Euler matrix
         Mat(:,3)=u3

         Imol%Euler=matmul(Eu,Mat)  !New Euler Matrix
         call Get_PhiTheChi(Imol%Euler,Phi,Theta,Chi,"D")
         Imol%orient(1)=  phi
         Imol%orient(2)=theta
         Imol%orient(3)=  chi

         !> New centre (?) Needs the Cell argument
         if (present(Cell)) then
            rj=Matmul(Mat,ri)
            Imol%xcentre=matmul(Cell%Orth_Cr_cel,rj)+mol%xcentre

         else
            if (dot_product(ri,ri) > EPS) then
               call set_error (1, "Cartesian_to_Zmatrix: First atom not at the origin => a cell has to be provided ")
               return
            end if
         end if
      end if

      do i=4,na                 !The result of this calculation is independent of the type of
         ri = mol%I_coor(:,i)   !cartesian coordinates => it is not needed to transforn the input Cartesian!
         j  = mol%conn(1,i)     !The connectivity is needed for the Z-matrix description
         k  = mol%conn(2,i)     !If the connectivity is given it is possible to transform to
         n  = mol%conn(3,i)     !Z-matrix if cartesian/spherical coordinates are given.
         if ( j == 0 .or. k == 0 .or. n == 0) then
            call set_error(1,"Cartesian_to_Zmatrix: the connectivity is wrong for atom: "//mol%Atname(i))
            return
         end if
         rj = mol%I_coor(:,j)
         rk = mol%I_coor(:,k)
         rn = mol%I_coor(:,n)
         ci= get_Z_from_cartesian(ri,rj,rk,rn)
         Imol%I_coor(:,i) = ci
      end do
      Imol%coor_type="Z"

      if (present(NMol)) then
         call Init_molecule(NMol,na)
         if (NMol%natoms <=0) then
            call set_error (1, "Cartesian_to_Zmatrix: The optional variable was not dimensioned!")
            return
         end if
         NMol=Imol
      else
         mol=Imol
      end if

   End Subroutine Cartesian_to_Zmatrix

   !!--++
   !!--++ SUBROUTINE GET_Z_FROM_CARTESIAN
   !!--++
   !!--++     Subroutine to calculate the distance of an atom (i)
   !!--++     (dij=ci(1)) to another atom (j), the angle (aijk=ci(2))
   !!--++     spanned with another atom (k) centred at (j) and  the torsion angle
   !!--++     (bijkn=ci(3)) with a fourth atom (n) when the cartesian coordinates are given
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Function Get_Z_from_Cartesian(ri,rj,rk,rn) Result(ci)
      real(kind=cp), dimension(3), intent ( in) :: ri,rj,rk,rn
      real(kind=cp), dimension(3)               :: ci

      !--- Local variables ---!
      real(kind=cp)                 :: dji,djk
      real(kind=cp), dimension(3)   :: rji,rjk

      rji = ri-rj
      ci(1) = sqrt(dot_product(rji,rji))
      rjk = rk-rj
      dji = ci(1)
      djk = sqrt(dot_product(rjk,rjk))
      ci(2) = acosd( dot_product(rji,rjk)/dji/djk)
      ci(3) = angle_dihedral(ri,rj,rk,rn)
      if (abs(ci(3)+180.00) <= 0.001) ci(3)=180.0

   End Function Get_Z_from_Cartesian

   !!--++
   !!--++ SUBROUTINE CREATE_CONNECTIVITY_CARTESIAN
   !!--++    Subroutine that create the connectivity for the molecule.
   !!--++    The coordinates must be in Cartesian system.
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine Create_Connectivity_Cartesian(Mol, Dmin, Dmax)
      !---- Arguments ----!
      type (Molecule_type),          intent(in out):: Mol
      real(kind=cp), optional,       intent(in)    :: Dmin
      real(kind=cp), optional,       intent(in)    :: Dmax

      !---- Local variables ----!
      logical                                         :: re_order
      integer                                         :: i,j,k,l,m,nc1,nc2,nc3
      integer, dimension(mol%natoms,mol%natoms)       :: T_Conn
      integer, dimension(3,mol%natoms)                :: T_N
      integer, dimension(mol%natoms)                  :: T_Ind
      real(kind=cp), dimension(mol%natoms,mol%natoms) :: T_Dist
      real(kind=cp)                                   :: d_min, d_max
      real(kind=cp)                                   :: dist
      type (Molecule_type)                            :: Nmol


      !> Init
      d_min=0.6
      d_max=3.0
      T_Conn=0
      T_N   =0
      T_Ind =0
      T_Dist=0.0
      if (present(dmin)) d_min=dmin
      if (present(dmax)) d_max=dmax

      !> Controls
      if (Mol%coor_type /= "C") then
         call set_error( 1, "Connectivity: the input Mol is not in Cartesian coordinates")
         return
      end if

      !> Creating Tables
      do i=1,Mol%natoms
         do j=i+1,Mol%natoms
            dist=distance(Mol%I_coor(:,i),Mol%I_coor(:,j))
            if (dist < d_min .or. dist > d_max) cycle
            if (adjustl(Mol%Atsymb(i)) == "H   " .and. &
                adjustl(Mol%Atsymb(j)) == "H   ") cycle
            T_Conn(i,j)=i
            T_Conn(j,i)=i
            T_Dist(i,j)=dist
            T_Dist(j,i)=dist
         end do
      end do

      !> Test for reorder atoms
      re_order=.false.

      do i=2,Mol%natoms
         j=count(T_conn(i,1:i-1) > 0)
         if (j==0) re_order=.true.
      end do

      if (re_order) then
         m=1
         T_ind(m)=1
         do i=1,Mol%natoms
            do j=1,Mol%natoms
               if (T_Conn(i,j) <= 0) cycle
               l=0
               do k=1,m
                    if (j == T_ind(k)) then
                       l=1
                       exit
                    end if
               end do
               if (l > 0) cycle
               m=m+1
               T_ind(m)=j
            end do
         end do

         call init_molecule(NMol,Mol%natoms)
         NMol=Mol
         do i=2,NMol%natoms
            j=T_ind(i)
            NMol%AtName(i)=   Mol%AtName(j)
            NMol%AtSymb(i)=   Mol%AtSymb(j)
            NMol%AtZ(i)=      Mol%AtZ(j)
            NMol%Ptr(:,i)=    Mol%Ptr(:,j)
            NMol%I_Coor(:,i)= Mol%I_Coor(:,j)
            NMol%mI_Coor(:,i)=Mol%mI_Coor(:,j)
            NMol%lI_Coor(:,i)=Mol%lI_Coor(:,j)
            NMol%U_iso(i)=    Mol%U_iso(j)
            NMol%mU_iso(i)=   Mol%mU_iso(j)
            NMol%lU_iso(i)=   Mol%lU_iso(j)
            NMol%occ(i)=      Mol%occ(j)
            NMol%mocc(i)=     Mol%mocc(j)
            NMol%locc(i)=     Mol%locc(j)
            NMol%nb(i)=       Mol%nb(j)
            NMol%Inb(:,i)=    Mol%Inb(:,j)
            NMol%Tb(:,i)=     Mol%Tb(:,j)
            NMol%Conn(:,i)=   Mol%Conn(:,j)
         end do
         Mol=NMol
         call init_molecule(NMol,0)

         T_Conn=0
         T_Dist=0.0
         do i=1,Mol%natoms
            do j=i+1,Mol%natoms
               dist=distance(Mol%I_coor(:,i),Mol%I_coor(:,j))
               if (dist < d_min .or. dist > d_max) cycle
               if (adjustl(Mol%Atsymb(i)) == "H   " .and. &
                   adjustl(Mol%Atsymb(j)) == "H   ") cycle
               T_Conn(i,j)=i
               T_Conn(j,i)=i
               T_Dist(i,j)=dist
               T_Dist(j,i)=dist
            end do
         end do
      end if

      !> Connectivity Info
      do i=2, Mol%natoms

         !> Distances: Fill N1
         j=minloc(T_Dist(i,1:i-1),dim=1,mask=(T_Dist(i,1:i-1) > 0.0))
         T_N(1,i)=j

         if (j == 0) then
            call set_error(1, "Connectivity: Some Index are zeros" )
            return
         end if

         !> Angles: Fill N2
         if (i > 2) then
            nc1=count((T_Conn(j,1:i-1) > 0 .and. T_Conn(j,1:i-1) /=j),dim=1)
            nc2=count((T_Conn(i,1:i-1) > 0 .and. T_Conn(i,1:i-1) /=j),dim=1)
            k=0
            if (nc1 > 0) then
               do
                  k=minloc(T_Dist(j,1:i-1),dim=1, mask=(T_Dist(j,1:i-1) > 0.0))
                  if (k == j) then
                     T_Dist(j,k)=-T_Dist(j,k)
                     cycle
                  else
                     exit
                  end if
               end do

            elseif (nc2 > 0) then
               do
                  k=minloc(T_Dist(i,1:i-1),dim=1, mask=(T_Dist(i,1:i-1) > 0.0))
                  if (k == j) then
                     T_Dist(i,k)=-T_Dist(i,k)
                     cycle
                  else
                     exit
                  end if
               end do
            end if
            if (k == 0) then
               ! Elegir uno cualquiera
               do l=1,i-1
                  if (l == j) cycle
                  k=l
                  exit
               end do
            end if
            T_N(2,i)=k
         end if
         T_Dist=abs(T_Dist)

         !> Torsion
         if (i > 3) then
            nc1=count((T_Conn(k,1:i-1) > 0 .and. T_Conn(k,1:i-1) /=j .and. T_Conn(k,1:i-1) /=k),dim=1)
            nc2=count((T_Conn(j,1:i-1) > 0 .and. T_Conn(j,1:i-1) /=j .and. T_Conn(j,1:i-1) /=k),dim=1)
            nc3=count((T_Conn(i,1:i-1) > 0 .and. T_Conn(i,1:i-1) /=j .and. T_Conn(i,1:i-1) /=k),dim=1)

            l=0
            if (nc1 > 0) then
               do
                  l=minloc(T_Dist(k,1:i-1),dim=1, mask=(T_Dist(k,1:i-1) > 0.0))
                  if (l == j .or. l == k) then
                     T_Dist(k,l)=-T_Dist(k,l)
                     cycle
                  else
                     exit
                  end if
               end do

            elseif (nc2 > 0) then
               do
                  l=minloc(T_Dist(j,1:i-1),dim=1, mask=(T_Dist(j,1:i-1) > 0.0))
                  if (l == j .or. l == k) then
                     T_Dist(j,l)=-T_Dist(j,l)
                     cycle
                  else
                     exit
                  end if
               end do

            elseif (nc3 > 0) then
               do
                  l=minloc(T_Dist(i,1:i-1),dim=1, mask=(T_Dist(i,1:i-1) > 0.0))
                  if (l == j .or. l == k) then
                     T_Dist(i,l)=-T_Dist(i,l)
                     cycle
                  else
                     exit
                  end if
               end do
            end if
            if (l==0) then
               ! Elegir uno cualquiera
               do m=1,i-1
                  if (m == j .or. m == k) cycle
                  l=m
                  exit
               end do
            end if
            T_N(3,i)=l
         end if
         T_Dist=abs(T_Dist)

      end do

      !> Final Part
      do i=1, Mol%natoms
         Mol%Conn(:,i)=T_N(:,i)
         select case (i)
            case (2)
               if (T_N(1,i) == 0) then
                  call set_error(1, "Connectivity: Some Index are zeros")
               end if
            case (3)
               if (any(T_N(1:2,i) == 0)) then
                  call set_error(1, "Connectivity: Some Index are zeros")
               end if
            case (4:)
               if (any(T_N(:,i) == 0)) then
                  call set_error(1, "Connectivity: Some Index are zeros")
               end if
         end select
      end do

   End Subroutine Create_Connectivity_Cartesian

End Submodule Mol_Cartesian_To