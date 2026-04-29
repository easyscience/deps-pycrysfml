Submodule (CFML_Molecules) Mol_Orientation

   implicit none

 Contains
   !!----
   !!---- Subroutine Set_Euler_Matrix
   !!----
   !!----    Subroutine to obtain the Euler active matrix to transform a point
   !!----    to another point. For instance the internal coordinates of a molecule
   !!----    can be transformed to absolute positions using columns vectors.
   !!----
   !!----    If the Cartesian coordinates of an atom in the molecular frame is the
   !!----    column vector  Xm, the cartesian coordinates in the crystal frame X
   !!----    are obtained from:  X = Eu Xm
   !!----
   !!----    The internal coordinates of a point are obtained from Xm = EuT X.
   !!----    The character variable "rt" indicates the type of Euler angles provided.
   !!----    If rt="E", the angles PHI,THETA,CHI correspond to the conventional
   !!----    Euler angles ALPHA, BETA, GAMMA. Otherwise, they correspond to the
   !!----    2nd setting, allowing to interpret PHI and THETA as the polar angles of
   !!----    the molecular frame Zm-axis, and CHI a rotation around Zm.
   !!----
   !!----   Update: February - 2005
   !!
   Module Function Set_Euler_Matrix(Rt, Phi, Theta, Chi) Result(Eu)
      !---- Arguments ----!
      character(len=*),              intent ( in) :: Rt
      real(kind=cp),                 intent ( in) :: Phi,Theta,Chi
      real(kind=cp), dimension(3,3)               :: Eu

      !---- Local Variables ----!
      character(len=1) :: car
      real(kind=cp)    :: PH,TH,CH

      car=u_case(adjustl(rt))

      TH=THETA
      if (car == "E") then
         PH=PHI+90.0_cp
         CH=CHI-90.0_cp
      else
         PH=PHI
         CH=CHI
      end if
      Eu(1,1) =  cosd(PH)* cosd(TH)* cosd(CH) - sind(PH)* sind(CH)
      Eu(1,2) = -cosd(PH)* cosd(TH)* sind(CH) - sind(PH)* cosd(CH)
      Eu(1,3) =  cosd(PH)* sind(TH)
      Eu(2,1) =  sind(PH)* cosd(TH)* cosd(CH) + cosd(PH)* sind(CH)
      Eu(2,2) = -sind(PH)* cosd(TH)* sind(CH) + cosd(PH)* cosd(CH)
      Eu(2,3) =  sind(PH)* sind(TH)
      Eu(3,1) = -cosd(CH)* sind(TH)
      Eu(3,2) =  sind(CH)* sind(TH)
      Eu(3,3) =            cosd(TH)

   End Function Set_Euler_Matrix

   !!----
   !!---- SUBROUTINE FIX_ORIENT_CARTESIAN
   !!----
   !!----    Subroutine to transform the Cartesian coordinates of the molecule choosing
   !!----    which atom is the origin, which define the X axis and which defines the XY Plane
   !!----    If the second argument is present the subroutine creates a new molecule
   !!----    preserving the input molecule in Cartesian. Otherwise the input molecule is
   !!----    changed on output.
   !!----
   !!----    If Natom_0 is absent, then the first atom on the molecule will be the origin.
   !!----    If Natom_X is absent, then the second atom on the molecule will define the X axis.
   !!----    If Natom_XY is absent, then the third atom on the molecule will define the XY Plane.
   !!----
   !!----    The optional output matrix Mat is the active rotation matrix passing from the old
   !!----    Cartesian frame to the new one. The transpose matrix has served to transform the
   !!----    original Cartesian coordinates.
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine Fix_Orient_Cartesian(Mol, NMol, NAtom_O, NAtom_X, NAtom_XY,Mat)
      !---- Arguments ----!
      type (Molecule_type),                    intent(in out) :: Mol
      type (Molecule_type),          optional, intent(   out) :: NMol
      integer,                       optional, intent(in)     :: NAtom_O
      integer,                       optional, intent(in)     :: NAtom_X
      integer,                       optional, intent(in)     :: NAtom_XY
      real(kind=cp), dimension(3,3), optional, intent(out)    :: Mat

      !---- Local variables ----!
      integer                       :: n_or, n_x, n_xy
      integer                       :: i
      real(kind=cp),dimension(3)    :: u1,u2,u3
      real(kind=cp),dimension(3,3)  :: R
      type (Molecule_type)          :: IMol

      n_or=1
      n_x =2
      n_xy=3
      if (present(natom_O))  n_or=natom_o
      if (present(natom_x))  n_x =natom_x
      if (present(natom_xy)) n_xy=natom_xy

      if (Mol%natoms > 0) call Init_Molecule(IMol,Mol%natoms)
      call Set_MolReference(Mol, IMol, n_or, n_x, n_xy)
      if (err_CFML%IErr /=0) return

      !> Traslation the Origin
      do i=2,IMol%natoms
         IMol%I_coor(:,i)=IMol%I_coor(:,i)-IMol%I_coor(:,1)
      end do
      IMol%I_coor(:,1)=0.0

      u1=IMol%I_coor(:,2)
      u1=u1/sqrt(dot_product(u1,u1))
      u2=IMol%I_coor(:,3)
      u3=cross_product(u1,u2)
      u3=u3/sqrt(dot_product(u3,u3))
      u2=cross_product(u3,u1)
      R(1,:)=u1
      R(2,:)=u2  !Passive matrix needed to get the new coordinates
      R(3,:)=u3  !The active matrix can be output in the optional argument
      if (present(Mat)) Mat=transpose(R)

      do i=2,IMol%natoms
         IMol%I_coor(:,i)=matmul(R,IMol%I_coor(:,i))
      end do

      if (present(NMol)) then
         if (IMol%natoms <=0) then
            call set_error (1, "Fix_Orient_Cartesian: The optional variable was not dimensioned!")
            return
         end if
         NMol=IMol
      else
         Mol=IMol
      end if

   End Subroutine Fix_Orient_Cartesian

   !!----
   !!---- SUBROUTINE SET_MOLREFERENCE
   !!----
   !!----    Subroutine to order the molecule choosing which atom is the origin,
   !!----    which define the X axis and which defines the XY Plane.
   !!----
   !!----    If the second argument is present the subroutine creates a new molecule
   !!----    preserving the input molecule in Cartesian. Otherwise the input molecule is
   !!----    changed on output.
   !!----
   !!----    If Natom_0 is absent, then the first atom on the molecule will be the origin.
   !!----    If Natom_X is absent, then the second atom on the molecule will define the X axis.
   !!----    If Natom_XY is absent, then the third atom on the molecule will define the XY Plane.
   !!----
   !!---- Update: April - 2022
   !!
   Module Subroutine Set_MolReference(Mol, NMol, NAtom_O, NAtom_X, NAtom_XY)
      !---- Arguments ----!
      type (Molecule_type),           intent(in out) :: Mol
      type (Molecule_type), optional, intent(   out) :: NMol
      integer,              optional, intent(in)     :: NAtom_O
      integer,              optional, intent(in)     :: NAtom_X
      integer,              optional, intent(in)     :: NAtom_XY

      !---- Local variables ----!
      integer                   :: n_or, n_x, n_xy
      integer                   :: i
      type (Molecule_type)      :: IMol,SetMol

      !> Initialize
      call clear_error()

      n_or=1
      n_x =2
      n_xy=3
      if (present(natom_O))  n_or=natom_o
      if (present(natom_x))  n_x =natom_x
      if (present(natom_xy)) n_xy=natom_xy

      if (n_x == n_or) then
         call set_error(1, "The atom defining origin and X axis is the same")
         return
      end if

      if (n_xy == n_or .or. n_xy ==n_x) then
         call set_error(1, "The atom defining the Plane XY is equal to the origin or that define the X axis")
         return
      end if

      if (Mol%natoms > 0) call Init_Molecule(IMol,Mol%natoms)
      IMol=Mol

      !> Sorting the Atom List
      call init_molecule(SetMol,1)

      !> Fix Origin
      if (n_or /= 1) then
         SetMol%AtName(1)    =IMol%AtName(n_or)
         SetMol%AtSymb(1)    =IMol%AtSymb(n_or)
         SetMol%AtZ(1)       =IMol%AtZ(n_or)
         SetMol%Ptr(:,1)     =IMol%Ptr(:,n_or)
         SetMol%I_Coor(:,1)  =IMol%I_Coor(:,n_or)
         SetMol%mI_Coor(:,1) =IMol%mI_Coor(:,n_or)
         SetMol%lI_Coor(:,1) =IMol%lI_Coor(:,n_or)
         SetMol%U_iso(1)     =IMol%U_iso(n_or)
         SetMol%mU_iso(1)    =IMol%mU_iso(n_or)
         SetMol%lU_iso(1)    =IMol%lU_iso(n_or)
         SetMol%Occ(1)       =IMol%Occ(n_or)
         SetMol%mocc(1)      =IMol%mocc(n_or)
         SetMol%lOcc(1)      =IMol%lOcc(n_or)
         SetMol%Nb(1)        =IMol%Nb(n_or)
         SetMol%INb(:,1)     =IMol%INb(:,n_or)
         SetMol%Tb(:,1)      =IMol%Tb(:,n_or)
         SetMol%Conn(:,1)    =IMol%Conn(:,n_or)

         IMol%AtName(2:n_or)    =IMol%AtName(1:n_or-1)
         IMol%AtSymb(2:n_or)    =IMol%AtSymb(1:n_or-1)
         IMol%AtZ(2:n_or)       =IMol%AtZ(1:n_or-1)
         IMol%Ptr(:,2:n_or)     =IMol%Ptr(:,1:n_or-1)
         IMol%I_Coor(:,2:n_or)  =IMol%I_Coor(:,1:n_or-1)
         IMol%mI_Coor(:,2:n_or) =IMol%mI_Coor(:,1:n_or-1)
         IMol%lI_Coor(:,2:n_or) =IMol%lI_Coor(:,1:n_or-1)
         IMol%U_iso(2:n_or)     =IMol%U_iso(1:n_or-1)
         IMol%mU_iso(2:n_or)    =IMol%mU_iso(1:n_or-1)
         IMol%lU_iso(2:n_or)    =IMol%lU_iso(1:n_or-1)
         IMol%Occ(2:n_or)       =IMol%Occ(1:n_or-1)
         IMol%mocc(2:n_or)      =IMol%mocc(1:n_or-1)
         IMol%lOcc(2:n_or)      =IMol%lOcc(1:n_or-1)
         IMol%Nb(2:n_or)        =IMol%Nb(1:n_or-1)
         IMol%INb(:,2:n_or)     =IMol%INb(:,1:n_or-1)
         IMol%Tb(:,2:n_or)      =IMol%Tb(:,1:n_or-1)
         IMol%Conn(:,2:n_or)    =IMol%Conn(:,1:n_or-1)

         IMol%AtName(1)    =SetMol%AtName(1)
         IMol%AtSymb(1)    =SetMol%AtSymb(1)
         IMol%AtZ(1)       =SetMol%AtZ(1)
         IMol%Ptr(:,1)     =SetMol%Ptr(:,1)
         IMol%I_Coor(:,1)  =SetMol%I_Coor(:,1)
         IMol%mI_Coor(:,1) =SetMol%mI_Coor(:,1)
         IMol%lI_Coor(:,1) =SetMol%lI_Coor(:,1)
         IMol%U_iso(1)     =SetMol%U_iso(1)
         IMol%mU_iso(1)    =SetMol%mU_iso(1)
         IMol%lU_iso(1)    =SetMol%lU_iso(1)
         IMol%Occ(1)       =SetMol%Occ(1)
         IMol%mocc(1)      =SetMol%mocc(1)
         IMol%lOcc(1)      =SetMol%lOcc(1)
         IMol%Nb(1)        =SetMol%Nb(1)
         IMol%INb(:,1)     =SetMol%INb(:,1)
         IMol%Tb(:,1)      =SetMol%Tb(:,1)
         IMol%Conn(:,1)    =SetMol%Conn(:,1)

         if (IMol%is_connect) then
            do i=1,n_or
               if (IMol%conn(1,i) == n_or) then
                  IMol%conn(1,i)=1
               else if (IMol%conn(1,i) < n_or) then
                  IMol%conn(1,i)=IMol%conn(1,i)+1
               end if

               if (IMol%conn(2,i) == n_or) then
                  IMol%conn(2,i)=1
               else if (IMol%conn(2,i) < n_or) then
                  IMol%conn(2,i)=IMol%conn(2,i)+1
               end if

               if (IMol%conn(3,i) == n_or) then
                  IMol%conn(3,i)=1
               else if (IMol%conn(3,i) < n_or) then
                  IMol%conn(3,i)=IMol%conn(3,i)+1
               end if
            end do
         end if

         if (n_x < n_or) then
            n_x=n_x+1
         end if
         if (n_xy < n_or) then
            n_xy=n_xy+1
         end if
      end if

      !> Fix X Axis
      if (n_x /= 2) then

         SetMol%AtName(1)    =IMol%AtName(n_x)
         SetMol%AtSymb(1)    =IMol%AtSymb(n_x)
         SetMol%AtZ(1)       =IMol%AtZ(n_x)
         SetMol%Ptr(:,1)     =IMol%Ptr(:,n_x)
         SetMol%I_Coor(:,1)  =IMol%I_Coor(:,n_x)
         SetMol%mI_Coor(:,1) =IMol%mI_Coor(:,n_x)
         SetMol%lI_Coor(:,1) =IMol%lI_Coor(:,n_x)
         SetMol%U_iso(1)     =IMol%U_iso(n_x)
         SetMol%mU_iso(1)    =IMol%mU_iso(n_x)
         SetMol%lU_iso(1)    =IMol%lU_iso(n_x)
         SetMol%Occ(1)       =IMol%Occ(n_x)
         SetMol%mocc(1)      =IMol%mocc(n_x)
         SetMol%lOcc(1)      =IMol%lOcc(n_x)
         SetMol%Nb(1)        =IMol%Nb(n_x)
         SetMol%INb(:,1)     =IMol%INb(:,n_x)
         SetMol%Tb(:,1)      =IMol%Tb(:,n_x)
         SetMol%Conn(:,1)    =IMol%Conn(:,n_x)

         IMol%AtName(3:n_x)    =IMol%AtName(2:n_x-1)
         IMol%AtSymb(3:n_x)    =IMol%AtSymb(2:n_x-1)
         IMol%AtZ(3:n_x)       =IMol%AtZ(2:n_x-1)
         IMol%Ptr(:,3:n_x)     =IMol%Ptr(:,2:n_x-1)
         IMol%I_Coor(:,3:n_x)  =IMol%I_Coor(:,2:n_x-1)
         IMol%mI_Coor(:,3:n_x) =IMol%mI_Coor(:,2:n_x-1)
         IMol%lI_Coor(:,3:n_x) =IMol%lI_Coor(:,2:n_x-1)
         IMol%U_iso(3:n_x)     =IMol%U_iso(2:n_x-1)
         IMol%mU_iso(3:n_x)    =IMol%mU_iso(2:n_x-1)
         IMol%lU_iso(3:n_x)    =IMol%lU_iso(2:n_x-1)
         IMol%Occ(3:n_x)       =IMol%Occ(2:n_x-1)
         IMol%mocc(3:n_x)      =IMol%mocc(2:n_x-1)
         IMol%lOcc(3:n_x)      =IMol%lOcc(2:n_x-1)
         IMol%Nb(3:n_x)        =IMol%Nb(2:n_x-1)
         IMol%INb(:,3:n_x)     =IMol%INb(:,2:n_x-1)
         IMol%Tb(:,3:n_x)      =IMol%Tb(:,2:n_x-1)
         IMol%Conn(:,3:n_x)    =IMol%Conn(:,2:n_x-1)

         IMol%AtName(2)    =SetMol%AtName(1)
         IMol%AtSymb(2)    =SetMol%AtSymb(1)
         IMol%AtZ(2)       =SetMol%AtZ(1)
         IMol%Ptr(:,2)     =SetMol%Ptr(:,1)
         IMol%I_Coor(:,2)  =SetMol%I_Coor(:,1)
         IMol%mI_Coor(:,2) =SetMol%mI_Coor(:,1)
         IMol%lI_Coor(:,2) =SetMol%lI_Coor(:,1)
         IMol%U_iso(2)     =SetMol%U_iso(1)
         IMol%mU_iso(2)    =SetMol%mU_iso(1)
         IMol%lU_iso(2)    =SetMol%lU_iso(1)
         IMol%Occ(2)       =SetMol%Occ(1)
         IMol%mocc(2)      =SetMol%mocc(1)
         IMol%lOcc(2)      =SetMol%lOcc(1)
         IMol%Nb(2)        =SetMol%Nb(1)
         IMol%INb(:,2)     =SetMol%INb(:,1)
         IMol%Tb(:,2)      =SetMol%Tb(:,1)
         IMol%Conn(:,2)    =SetMol%Conn(:,1)

         if (IMol%is_connect) then
            do i=1,n_x
               if (IMol%conn(1,i) == n_x) then
                  IMol%conn(1,i)=2
               else if (IMol%conn(1,i) < n_x .and. IMol%conn(1,i) > 1) then
                  IMol%conn(1,i)=IMol%conn(1,i)+1
               end if

               if (IMol%conn(2,i) == n_x) then
                  IMol%conn(2,i)=2
               else if (IMol%conn(2,i) < n_x .and. IMol%conn(2,i) > 1) then
                  IMol%conn(2,i)=IMol%conn(2,i)+1
               end if

               if (IMol%conn(3,i) == n_x) then
                  IMol%conn(3,i)=2
               else if (IMol%conn(3,i) < n_x .and. IMol%conn(3,i) > 1) then
                  IMol%conn(3,i)=IMol%conn(3,i)+1
               end if
            end do
         end if
         if (n_xy < n_x) then
            n_xy=n_xy+1
         end if
      end if

      !> Fix XY Plane
      if (n_xy /= 3) then

         SetMol%AtName(1)    =IMol%AtName(n_xy)
         SetMol%AtSymb(1)    =IMol%AtSymb(n_xy)
         SetMol%AtZ(1)       =IMol%AtZ(n_xy)
         SetMol%Ptr(:,1)     =IMol%Ptr(:,n_xy)
         SetMol%I_Coor(:,1)  =IMol%I_Coor(:,n_xy)
         SetMol%mI_Coor(:,1) =IMol%mI_Coor(:,n_xy)
         SetMol%lI_Coor(:,1) =IMol%lI_Coor(:,n_xy)
         SetMol%U_iso(1)     =IMol%U_iso(n_xy)
         SetMol%mU_iso(1)    =IMol%mU_iso(n_xy)
         SetMol%lU_iso(1)    =IMol%lU_iso(n_xy)
         SetMol%Occ(1)       =IMol%Occ(n_xy)
         SetMol%mocc(1)      =IMol%mocc(n_xy)
         SetMol%lOcc(1)      =IMol%lOcc(n_xy)
         SetMol%Nb(1)        =IMol%Nb(n_xy)
         SetMol%INb(:,1)     =IMol%INb(:,n_xy)
         SetMol%Tb(:,1)      =IMol%Tb(:,n_xy)
         SetMol%Conn(:,1)    =IMol%Conn(:,n_xy)

         IMol%AtName(4:n_xy)    =IMol%AtName(3:n_xy-1)
         IMol%AtSymb(4:n_xy)    =IMol%AtSymb(3:n_xy-1)
         IMol%AtZ(4:n_xy)       =IMol%AtZ(3:n_xy-1)
         IMol%Ptr(:,4:n_xy)     =IMol%Ptr(:,3:n_xy-1)
         IMol%I_Coor(:,4:n_xy)  =IMol%I_Coor(:,3:n_xy-1)
         IMol%mI_Coor(:,4:n_xy) =IMol%mI_Coor(:,3:n_xy-1)
         IMol%lI_Coor(:,4:n_xy) =IMol%lI_Coor(:,3:n_xy-1)
         IMol%U_iso(4:n_xy)     =IMol%U_iso(3:n_xy-1)
         IMol%mU_iso(4:n_xy)    =IMol%mU_iso(3:n_xy-1)
         IMol%lU_iso(4:n_xy)    =IMol%lU_iso(3:n_xy-1)
         IMol%Occ(4:n_xy)       =IMol%Occ(3:n_xy-1)
         IMol%mocc(4:n_xy)      =IMol%mocc(3:n_xy-1)
         IMol%lOcc(4:n_xy)      =IMol%lOcc(3:n_xy-1)
         IMol%Nb(4:n_xy)        =IMol%Nb(3:n_xy-1)
         IMol%INb(:,4:n_xy)     =IMol%INb(:,3:n_xy-1)
         IMol%Tb(:,4:n_xy)      =IMol%Tb(:,3:n_xy-1)
         IMol%Conn(:,4:n_xy)    =IMol%Conn(:,3:n_xy-1)

         IMol%AtName(3)    =SetMol%AtName(1)
         IMol%AtSymb(3)    =SetMol%AtSymb(1)
         IMol%AtZ(3)       =SetMol%AtZ(1)
         IMol%Ptr(:,3)     =SetMol%Ptr(:,1)
         IMol%I_Coor(:,3)  =SetMol%I_Coor(:,1)
         IMol%mI_Coor(:,3) =SetMol%mI_Coor(:,1)
         IMol%lI_Coor(:,3) =SetMol%lI_Coor(:,1)
         IMol%U_iso(3)     =SetMol%U_iso(1)
         IMol%mU_iso(3)    =SetMol%mU_iso(1)
         IMol%lU_iso(3)    =SetMol%lU_iso(1)
         IMol%Occ(3)       =SetMol%Occ(1)
         IMol%mocc(3)      =SetMol%mocc(1)
         IMol%lOcc(3)      =SetMol%lOcc(1)
         IMol%Nb(3)        =SetMol%Nb(1)
         IMol%INb(:,3)     =SetMol%INb(:,1)
         IMol%Tb(:,3)      =SetMol%Tb(:,1)
         IMol%Conn(:,3)    =SetMol%Conn(:,1)

         if (IMol%is_connect) then
            do i=1,n_xy
               if (IMol%conn(1,i) == n_xy) then
                  IMol%conn(1,i)=3
               else if (IMol%conn(1,i) < n_xy .and. IMol%conn(1,i) > 2) then
                  IMol%conn(1,i)=IMol%conn(1,i)+1
               end if

               if (IMol%conn(2,i) == n_xy) then
                  IMol%conn(2,i)=3
               else if (IMol%conn(2,i) < n_xy .and. IMol%conn(2,i) > 2) then
                  IMol%conn(2,i)=IMol%conn(2,i)+1
               end if

               if (IMol%conn(3,i) == n_xy) then
                  IMol%conn(3,i)=3
               else if (IMol%conn(3,i) < n_xy .and. IMol%conn(3,i) > 2) then
                  IMol%conn(3,i)=IMol%conn(3,i)+1
               end if
            end do
         end if
      end if

      if (present(NMol) ) then
         call Init_molecule(NMol,IMol%natoms)
         NMol=IMol
      else
         Mol=IMol
      end if

   End Subroutine Set_MolReference

End Submodule Mol_Orientation