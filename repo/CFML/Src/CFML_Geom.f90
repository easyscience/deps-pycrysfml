!!-------------------------------------------------------
!!---- Crystallographic Fortran Modules Library (CrysFML)
!!-------------------------------------------------------
!!---- The CrysFML project is distributed under LGPL. In agreement with the
!!---- Intergovernmental Convention of the ILL, this software cannot be used
!!---- in military applications.
!!----
!!---- Copyright (C) 1999-2022  Institut Laue-Langevin (ILL), Grenoble, FRANCE
!!----                          Universidad de La Laguna (ULL), Tenerife, SPAIN
!!----                          Laboratoire Leon Brillouin(LLB), Saclay, FRANCE
!!----
!!---- Authors: Juan Rodriguez-Carvajal (ILL)
!!----          Javier Gonzalez-Platas  (ULL)
!!----          Nebil Ayape Katcho      (ILL)
!!----
!!---- Contributors: Laurent Chapon     (ILL)
!!----               Marc Janoschek     (Los Alamos National Laboratory, USA)
!!----               Oksana Zaharko     (Paul Scherrer Institute, Switzerland)
!!----               Tierry Roisnel     (CDIFX,Rennes France)
!!----               Eric Pellegrini    (ILL)
!!----               Ross Angel         (University of Pavia)
!!----
!!---- This library is free software; you can redistribute it and/or
!!---- modify it under the terms of the GNU Lesser General Public
!!---- License as published by the Free Software Foundation; either
!!---- version 3.0 of the License, or (at your option) any later version.
!!----
!!---- This library is distributed in the hope that it will be useful,
!!---- but WITHOUT ANY WARRANTY; without even the implied warranty of
!!---- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!!---- Lesser General Public License for more details.
!!----
!!---- You should have received a copy of the GNU Lesser General Public
!!---- License along with this library; if not, see <http://www.gnu.org/licenses/>.
!!----
!!----
!!---- MODULE: CFML_Geometry_Calc
!!----   INFO: Routines for Geometry Calculations
!!----
!!---- HISTORY
!!----    Update: 06/03/2011
!!----
!!----
!!
 Module CFML_Geom

    !---- Use Modules ----!
    use CFML_GlobalDeps
    use CFML_Rational
    use CFML_Maths,         only: Modulo_Lat, Cross_Product, Inverse_Matrix, Determ3D
    use CFML_Strings,       only: Frac_Trans_1Dig, L_Case,U_Case,pack_string,String_NumStd
    use CFML_Metrics,       only: Cell_G_Type, Get_Deriv_Orth_Cell,Rot_Gibbs_Matrix
    use CFML_Atoms,         only: AtList_Type,Atm_Cell_Type,Equiv_Atm, Wrt_Lab, Atom_Equiv_List_Type, &
                                  Allocate_Atom_List, Atm_Std_Type
    use CFML_gSpaceGroups,  only: Spg_Type, Apply_OP, Get_Multip_Pos, Is_Lattice_Vec, &
                                  searchop, Write_SymTrans_Code, Get_Orbit, Point_Orbit

    implicit none

    private

    !---- List of public functions ----!

    !---- List of public overloaded procedures: functions ----!
    public :: Angle_Dihedral, Angle_Mod, Angle_Uv, Distance, Matrix_PhiTheChi, Matrix_Rx, &
              Matrix_Ry, Matrix_Rz

    !---- List of public subroutines ----!
    public :: Allocate_Coordination_Type, Allocate_Point_List, Calc_Dist_Angle, Calc_Dist_Angle_Sigma, &
              Deallocate_Coordination_Type, Deallocate_Point_List, Distance_and_Sigma, Get_Euler_From_Fract, &
              Get_PhiTheChi, P1_Dist, Print_Distances, Set_Orbits_InList, Set_TDist_Coordination, &
              Get_Transf_List, Set_TDist_Partial_Coordination, Get_Anglen_Axis_From_RotMat, Get_Matrix_moving_v_to_u, &
              Get_OmegaChiPhi, Set_Rotation_Matrix, Set_New_AsymUnit,Angle_and_Sigma, Torsion_and_Sigma, &
              Clear_init_symOp

    public :: init_opMatTr !These procedures should be private but Gfortran fails in constructing programs using
                           !private procedures accessed by public procedures in submodules.

    !---- List of public overloaded procedures: subroutines ----!

    !---- List of private functions ----!
    private :: Angle_Dihedral_Uvw,  Angle_Dihedral_Ijkn, Angle_Uvi, Angle_Uvr, Angle_Modn, Angle_Modv, &
               Distance_fr, Distance_fr_dp, Distance_sc

    !---- Definitions ----!

    !!----
    !!---- TYPE :: COORDINATION_TYPE
    !!--..
    !!---- 31/05/2020
    !!
    Type, public :: Coordination_Type
       integer                                      :: Natoms    ! number of atoms
       integer                                      :: Max_Coor  ! Maximum number of connected atoms to a given one
       integer,       dimension(:),     allocatable :: Coord_Num ! Counter of distances connected to the current atom
       integer,       dimension(:,:),   allocatable :: N_Cooatm  ! Pointer to the ordinal number in the list of the attached
                                                                 ! atom to the atom given by the first index
       integer,       dimension(:,:),   allocatable :: N_Sym     ! Number of symmetry operator to apply to N_Cooatm
       real(kind=cp), dimension(:,:),   allocatable :: Dist      ! List of distances related to an atom
       real(kind=cp), dimension(:,:),   allocatable :: S_Dist    ! List of Sigma(distances)
       real(kind=cp), dimension(:,:,:), allocatable :: Tr_coo
    End type Coordination_Type

    !!----
    !!---- TYPE :: POINT_LIST_TYPE
    !!--..
    !!---- 31/05/2020
    !!
    Type, public :: point_list_type
       integer                                       :: np   !number of points in list
       character(len=20), dimension(:),  allocatable :: nam  !name/label associated to each point
       integer,           dimension(:),  allocatable :: p    !integer pointer for various purposes
       real(kind=cp),     dimension(:,:),allocatable :: x    !fractional coordinates of points
    End type point_list_type

    !---- Parameters ----!
    real(kind=cp), parameter, private :: epsi=0.001  ! Epsilon for roughly comparing distances

    !---- Variables ----!
    type(Coordination_Type),                    public  :: coord_info    ! Coordination Information
    real(kind=cp), dimension(:,:), allocatable, private :: optr   ! Associated translations of symmetry operators
    integer, dimension(:,:,:),     allocatable, private :: opMat  ! Matrices of the symmetry operators
    logical, private :: init_symOp=.false.

    !---- Overload Zone ----!
    Interface  Angle_Dihedral
       Module Procedure Angle_Dihedral_Ijkn
       Module Procedure Angle_Dihedral_Uvw
    End Interface

    Interface  Angle_Uv
       Module Procedure Angle_UvI
       Module Procedure Angle_UvR
    End Interface

    Interface  Angle_Mod
       Module Procedure Angle_ModN
       Module Procedure Angle_ModV
    End Interface

    Interface  Distance
       Module Procedure Distance_FR_DP
       Module Procedure Distance_FR
       Module Procedure Distance_SC
    End Interface

    !------------------------!
    !---- Interface Zone ----!
    !------------------------!
    Interface

      Pure Module Function Angle_Dihedral_Ijkn(ri,rj,rk,rn) result(angle)
         !> Calculates the dihedral angle corresponding to the four points (ri,rj,rk,rn)
         !> given in cartesian components. The definition used for the dihedral angle
         !> is the following:
         !>
         !>    Phi(i,j,k,n) = acos { (rij x rjk) (rjk x rkn) / |rij x rjk| / |rjk x rkn| }
         !>
         !> with this definition the sign of Phi is positive if the vector product
         !> (rij x rjk) x (rjk x rkn) is in the same direction as rjk, and negative if
         !> the direction is opposite.
         real(kind=cp), dimension(3), intent( in) :: ri,rj,rk,rn  !> Position vector
         real(kind=cp)                            :: angle        !> Dihedral angle
      End Function Angle_Dihedral_Ijkn

      Pure Module Function Angle_Dihedral_Uvw(u,v,w) result(angle)
         !> Calculates the dihedral angle between planes u-v and v-w
         !> Vectors u,v,w are given in cartesian components.
         real(kind=cp), dimension(3), intent( in) :: u,v,w  !> Vector
         real(kind=cp)                            :: angle  !> Dihedral angle
      End Function Angle_Dihedral_Uvw

      Pure Module Function Angle_ModN(Angle) Result(AngMod)
         !> Transforms angle in radians between -pi and +pi
         real(kind=cp), intent(in) :: Angle   !> Angle (radians)
         real(kind=cp)             :: AngMod  !> Angle in the interval (-pi,pi]
      End Function Angle_ModN

      Pure Module Function Angle_ModV(V_Angle) Result(VAngMod)
         !> Transforms angle in radians between -pi and +pi
         real(kind=cp), dimension(:),intent(in) :: V_Angle !> Set of angles (radians)
         real(kind=cp), dimension(size(V_Angle)):: VAngMod !> Set of angles in the interval (-pi,pi]
      End Function Angle_ModV

      Pure Module Function Angle_UvI(Ui,Vi,G) Result(Angle)
         !---- Argument ----!
         integer, dimension(:),   intent( in)                 :: ui
         integer, dimension(:),   intent( in)                 :: vi
         real(kind=cp), dimension(:,:), intent( in), optional :: g   !metric tensor
         real(kind=cp)                                        :: angle
      End Function Angle_uvi

      Pure Module Function Angle_UvR(u,v,g) result(angle)
         !---- Argument ----!
         real(kind=cp), dimension(:),   intent( in)           :: u
         real(kind=cp), dimension(:),   intent( in)           :: v
         real(kind=cp), dimension(:,:), intent( in), optional :: g   !metric tensor
         real(kind=cp)                                        :: angle
      End Function Angle_uvr

      Pure Module Function Distance_Fr(X0,X1,Celda) Result(Dis)
         !---- Arguments ----!
         real(kind=cp), dimension(3), intent(in) :: x0,x1
         type (Cell_G_Type),          intent(in) :: Celda
         real(kind=cp)                           :: dis
      End Function Distance_Fr

      Pure Module Function Distance_Fr_dp(X0,X1,Celda) Result(Dis)
         !---- Arguments ----!
         real(kind=dp), dimension(3), intent(in) :: x0,x1
         type (Cell_G_Type),          intent(in) :: Celda
         real(kind=dp)                           :: dis
      End Function Distance_Fr_dp

      Pure Module Function Distance_SC(X0,X1,Code) Result(Dis)
         !> Calculate distance between two points in Cartesian or Spherical
         !> If Code =="C" or Blank or not present then the coordinates are Cartesian.
         !> If Code =="S" then the coordinates are spherical (R, Theta, Phi).
         real(kind=cp), dimension(3), intent(in) :: x0,x1 !> Point coordinate
         character(len=*), optional,  intent(in) :: Code  !> 'C': cartesian, 'S': spherical
         real(kind=cp)                           :: dis   !> Distance
      End Function Distance_SC

      Pure Module Function Matrix_Phithechi(Phi,Theta,Chi,Code) Result(Mt)
         !> Calculate the active rotation matrix corresponding to the composition
         !> of a positive rotation around z of angle Chi, followed by a positive rotation
         !> of angle Theta around the y-axis and a subsequent positive rotation of angle Phi
         !> around z. "Positive" means counter-clockwise.
         !> The matrix is M = Rz(Phi) . Ry(Theta) . Rz(Chi)
         !> The colums represent the components of the unitary vectors {u,v,w} that
         !> may be considered as an alternative orthonormal frame to the canonical {i,j,k}.
         !> Applying the matrix M to a point in {i,j,k} gives another point in {i,j,k} obtained
         !> by the successive application of the three rotations given above. The transpose
         !> (inverse) of the M-matrix, when applied to a point in {i,j,k}, gives the coordinates
         !> of the same point referred to the frame {u,v,w}. This transpose matrix corresponds
         !> to a passive (change or Cartesian frame) rotation leaving the points in the same
         !> position with respect to the  {i,j,k} frame.
         !> The matrix M when applied to a column vector containing the coordinates of a point
         !> with respect to the {u,v,w} frame provides the coordinates of the same point with
         !> respect to the {i,j,k} frame.
         !> If Code =="R" or Blank or not present then the input angles are given in radians.
         !> If Code =="D" then the input angles are given in degrees (Phi, Theta, Chi).
         real(kind=cp),                intent(in) :: Phi    !> Phi angle
         real(kind=cp),                intent(in) :: Theta  !> Theta angle
         real(kind=cp),                intent(in) :: Chi    !> Chi angle
         character(len=*), optional,   intent(in) :: Code   !> 'R': radians, 'D': degrees
         real(kind=cp), dimension(3,3)            :: Mt     !> Rotation matrix
      End Function Matrix_Phithechi

      Pure Module Function Matrix_Rx(Ang,Code) Result(Mt)
         !> Calculate the active rotation matrix corresponding to the positive rotation
         !> of an angle Phi around the x-axis. The transpose matrix corresponds to a
         !> passive rotation that changes the orthogonal system to {u,v,w} leaving the point
         !> at the same position w.r.t. the canonical {i,j,k} frame.
         !> If Code =="R" or Blank or not present then the input angle is given in radians.
         !> If Code =="D" then the input angle is given in degrees.
         real(kind=cp),               intent(in) :: Ang   !> Rotation angle
         character(len=*), optional,  intent(in) :: Code  !> 'R': radians, 'D': degrees
         real(kind=cp), dimension(3,3)           :: Mt    !> Rotation matrix
      End Function Matrix_Rx

      Pure Module Function Matrix_Ry(Ang,Code) Result(Mt)
         !> Calculate the active rotation matrix corresponding to the positive rotation
         !> of an angle Phi around the y-axis. The transpose matrix corresponds to a
         !> passive rotation that changes the orthogonal system to {u,v,w} leaving the point
         !> at the same position w.r.t. the canonical {i,j,k} frame.
         !> If Code =="R" or Blank or not present then the input angle is given in radians.
         !> If Code =="D" then the input angle is given in degrees.
         real(kind=cp),               intent(in) :: Ang   !> Rotation angle
         character(len=*), optional,  intent(in) :: Code  !> 'R': radians, 'D': degrees
         real(kind=cp), dimension(3,3)           :: Mt    !> Rotation matrix
      End Function Matrix_Ry

      Pure Module Function Matrix_Rz(Ang,Code) Result(Mt)
         !> Calculate the active rotation matrix corresponding to the positive rotation
         !> of an angle Phi around the z-axis. The transpose matrix corresponds to a
         !> passive rotation that changes the orthogonal system to {u,v,w} leaving the point
         !> at the same position w.r.t. the canonical {i,j,k} frame.
         !> If Code =="R" or Blank or not present then the input angle is given in radians.
         !> If Code =="D" then the input angle is given in degrees.
         real(kind=cp),               intent(in) :: Ang   !> Rotation angle
         character(len=*), optional,  intent(in) :: Code  !> 'R': radians, 'D': degrees
         real(kind=cp), dimension(3,3)           :: Mt    !> Rotation matrix
      End Function Matrix_Rz

      Pure Module Function Set_Rotation_Matrix(ang) Result(Rot)
         !> Calculate the rotation matrix Rot corresponding to the application
         !> (active rotations) of the following succesive rotations:
         !>
         !> Rot = Rx(ang(3)) . Ry(ang(2)) . Rz(ang(1))
         real(kind=cp), dimension(3),   intent( in) :: ang  !> Angles in degrees
         real(kind=cp), dimension(3,3)              :: Rot  !> Rotation matrix
      End Function Set_Rotation_Matrix

    !---------------------!
    !---- Subroutines ----!
    !---------------------!

      Module Subroutine Allocate_Coordination_Type(nasu,numops,dmax,Max_Coor)
         !> Allocation of coord_info public variable. This function must be
         !> called before using this module.
         integer,       intent(in) :: nasu      !> Number of atoms in asymetric unit
         integer,       intent(in) :: numops    !> Number of symmetry operations excluding lattice centering
         real(kind=cp), intent(in) :: dmax      !> Maximum bond distance allowed
         integer,      intent(out) :: Max_Coor  !> Maximum coordination number allowed
      End Subroutine Allocate_Coordination_Type

      Module Subroutine Allocate_Point_List(n,Pl,Ier)
         !---- Arguments ----!
         integer,               intent(in)     :: n
         type(point_list_type), intent(in out) :: pl
         integer,               intent(out)    :: ier
      End subroutine Allocate_Point_List

      Module Subroutine Angle_and_Sigma(Cellp,DerM,x1,x0,x2,s1,s0,s2,ang,s)
         !---- Arguments ----!
         Type(Cell_G_Type),               intent(in)  :: Cellp         ! Cell object
         real(kind=cp), dimension(3,3,6), intent(in)  :: DerM          ! Matrix of derivatives of Cellp%Cr_Orth_cel
         real(kind=cp), dimension(3),     intent(in)  :: x0,x1,x2      ! Three points in fractional coordinates and sigmas, X0 is central
         real(kind=cp), dimension(3),     intent(in)  :: s0,s1,s2      ! Sigmas of the three points
         real(kind=cp),                   intent(out) :: ang,s         ! Angle and sigma
      End Subroutine Angle_and_Sigma

      Module Subroutine Calc_Dist_Angle(Dmax, Dangl, Cell, Spg, A, Lun)
         !---- Arguments ----!
         real(kind=cp),         intent(in)   :: Dmax, Dangl
         type (Cell_G_Type),    intent(in)   :: Cell
         Class(SpG_Type),       intent(in)   :: SpG
         type (AtList_Type),    intent(in)   :: A
         integer, optional,     intent(in)   :: lun
      End Subroutine Calc_Dist_Angle

      Module Subroutine Calc_Dist_Angle_Sigma(Dmax, Dangl, Cell, Spg, A, Lun, Lun_cons, Lun_cif,filrest,rdmax,ramin)
         !---- Arguments ----!
         real(kind=cp),             intent(in)   :: dmax, dangl
         type (Cell_G_Type),        intent(in)   :: Cell
         Class(SpG_Type),           intent(in)   :: SpG
         type (AtList_Type),        intent(in)   :: A
         integer, optional,         intent(in)   :: lun
         integer, optional,         intent(in)   :: lun_cons
         integer, optional,         intent(in)   :: lun_cif
         character(len=*), optional,intent(in)   :: filrest
         real(kind=cp),    optional,intent(in)   :: rdmax, ramin
      End Subroutine Calc_Dist_Angle_Sigma

      Module Subroutine Deallocate_Coordination_Type()
         !> Deallocate coord_info public variable.
      End Subroutine Deallocate_Coordination_Type

      Module Subroutine Deallocate_Point_List(Pl)
         !---- Arguments ----!
         type(point_list_type), intent(in out) :: pl
      End Subroutine Deallocate_Point_List

      Module Subroutine Distance_and_Sigma(Cellp,DerM,x0,x1,s0,s1,dis,s)
         !---- Arguments ----!
         Type(Cell_G_Type),               intent(in)  :: Cellp         ! Cell object
         real(kind=cp), dimension(3,3,6), intent(in)  :: DerM          ! Matrix of derivatives of Cellp%Cr_Orth_cel
         real(kind=cp), dimension(3),     intent(in)  :: x0,x1,s0,s1   ! Two points in fractional coordinates and sigmas
         real(kind=cp),                   intent(out) :: dis,s         ! Distance and sigma
      End Subroutine Distance_and_Sigma

      Module Subroutine Get_Anglen_Axis_From_RotMat(R,axis,angle)
         !> Compute the axis and angle of rotation corresponding to
         !> an input orthogonal matrix. A Cartesian frame is assumed.
         Real(kind=cp), dimension(3,3), intent(in) :: R     !> Input orthogonal matrix
         Real(kind=cp), dimension(3),   intent(out):: axis  !> Non normalized rotation axis
         Real(kind=cp),                 intent(out):: angle !> Angle of rotation
      End Subroutine Get_Anglen_Axis_From_RotMat

      Module Subroutine Get_Euler_From_Fract(X1,X2,X3,Mt,Phi,Theta,Chi,Eum,Code)
         !---- Arguments ----!
         real(kind=cp),           dimension(3),   intent (in) :: x1,x2,x3
         real(kind=cp),           dimension(3,3), intent (in) :: Mt
         real(kind=cp),                           intent(out) :: theta,phi,chi
         real(kind=cp), optional, dimension(3,3), intent(out) :: EuM
         character(len=*), optional,              intent (in) :: Code
      End Subroutine Get_Euler_From_Fract

      Module Subroutine Get_Matrix_moving_v_to_u(v,u,R,w,ang)
        real(kind=cp), dimension(3),           intent(in)  :: v,u
        real(kind=cp), dimension(3,3),         intent(out) :: R
        real(kind=cp), optional,               intent(out) :: ang
        real(kind=cp), optional,dimension(3),  intent(out) :: w
      End Subroutine Get_Matrix_moving_v_to_u

      Module Subroutine Get_OmegaChiPhi(Mt,Omega,Chi,Phi,Code)
         !> Calculate the Euler Angles corresponding to an orthogonal matrix.
         !> The definition of the Euler angles in this case correspond to the
         !> rotation matrix of Busing and Levy for diffractometry obtained from
         !> the composition of a rotation around z of angle Phi, followed by a
         !> rotation of angle Chi around the y-axis and a subsequent rotation
         !> of angle Omega around z.
         !> The matrix is supposed to be of the form:
         !> M = Rz(Omega) x Ry(Chi) x Rz(Phi)
         !> If Code =="R" or not present then the output angles are provided
         !> in radians. If Code =="D" then the output angles are provided in
         !> degrees. A checking of the input matrix is given before calculating
         !> the angles. The user must check the logical variable "ERR_RotMat"
         !> after calling this subroutine. If ERR_RotMat=.true. it means that the
         !> input matrix is not orthogonal. The obtained rotations should be
         !> interpreted as changes of reference systems, the angles correspond
         !>  to the motor settings to put a reciprocal vector in Cartesian
         !> the L-system (all angles equal to zero) in the position given
         !> coordinates w.r.t. by the active rotation matrix Mt:  z4= Mt z1.
         real(kind=cp), dimension(3,3),intent(in)  :: Mt    !> Orthogonal matrix
         real(kind=cp),                intent(out) :: Omega !> Omega angle
         real(kind=cp),                intent(out) :: Chi   !> Chi angle
         real(kind=cp),                intent(out) :: Phi   !> Phi angle
         character(len=*), optional,   intent(in)  :: Code  !> Code for specifying angle units
      End Subroutine Get_OmegaChiPhi

      Module Subroutine Get_PhiTheChi(Mt,Phi,Theta,Chi,Code)
         !> Calculate the Euler Angles corresponding to an orthogonal matrix
         !> The definition of the Euler angles in this case correspond to the
         !> active rotation matrix obtained from the composition of a rotation
         !> around z of angle Chi, followed by a rotation of angle Theta
         !> around the y-axis and a subsequent rotation of angle Phi around z.
         !> The matrix is supposed to be of the form:
         !> M = Rz(Phi) x Ry(Theta) x Rz(Chi)
         !> If Code =="R" or not present then the output angles are provided
         !> in radians. If Code =="D" then the output angles are provided
         !> in degrees. A checking of the input matrix is given before
         !> calculating the angles. The user must check the logical variable
         !> "Err_CFML%" after calling this subroutine. If Err_CFML%Ierr=1 it means
         !> that the input matrix is not orthogonal.
         real(kind=cp), dimension(3,3),intent(in)  :: Mt    !> Orthogonal matrix
         real(kind=cp),                intent(out) :: Phi   !> Omega angle
         real(kind=cp),                intent(out) :: Theta !> Chi angle
         real(kind=cp),                intent(out) :: Chi   !> Phi angle
         character(len=*), optional,   intent(in)  :: Code  !> Code for specifying angle units
      End Subroutine Get_PhiTheChi

      Module Subroutine Get_Transf_List(trans,ox,pl,npl)
         !---- Arguments ----!
         real(kind=cp),         dimension(3,3), intent(in)     :: trans
         real(kind=cp),         dimension(3  ), intent(in)     :: ox
         type(point_list_type),                 intent(in)     :: pl
         type(point_list_type),                 intent(in out) :: npl
      End Subroutine Get_Transf_List

      Module Subroutine P1_Dist(Dmax, Cell, Spg, Ac, Lun)
         !---- Arguments ----!
         real(kind=cp),         intent(in)       :: dmax
         type (Cell_G_Type),    intent(in)       :: Cell
         Class(SpG_Type),       intent(in)       :: SpG
         type (Atm_Cell_Type),  intent(in out)   :: Ac
         integer, optional,     intent(in)       :: lun
      End Subroutine P1_Dist

      Module Subroutine Print_Distances(Lun, Dmax, Cell, Spg, A)
         !-- Arguments --!
         integer,               intent(in)   :: lun
         real(kind=cp),         intent(in)   :: dmax
         type (Cell_G_Type),    intent(in)   :: Cell
         Class(SpG_Type),       intent(in)   :: SpG
         type (AtList_Type),    intent(in)   :: A
      End Subroutine Print_Distances

      Module Subroutine Set_New_AsymUnit(SpGn,Ate,Mat,orig,A_n,matkind,debug)
         Class(SpG_Type) ,              intent(in ) :: SpGn
         type (Atom_Equiv_List_Type),   intent(in ) :: Ate !In old group
         real(kind=cp), dimension (3,3),intent(in ) :: Mat
         real(kind=cp), dimension (  3),intent(in ) :: orig
         type (AtList_Type),            intent(out) :: A_n
         character (len=*), optional,   intent(in ) :: matkind
         character (len=*), optional,   intent(in ) :: debug
      End Subroutine Set_New_AsymUnit


      Module Subroutine Set_Orbits_Inlist(Spg,Pl)
         !---- Arguments ----!
         Class(SpG_Type),        intent(in)     :: SpG
         type(point_list_type),  intent(in out) :: pl
      End Subroutine Set_Orbits_Inlist

      Module Subroutine Set_TDist_Coordination(max_coor,Dmax, Cell, Spg, A)
         !---- Arguments ----!
         integer,                  intent(in)   :: max_coor
         real(kind=cp),            intent(in)   :: dmax
         type (cell_G_Type),       intent(in)   :: Cell
         Class(SpG_Type),          intent(in)   :: SpG
         type (AtList_Type),       intent(in)   :: A
      End Subroutine Set_TDist_Coordination

      Module Subroutine Set_TDist_Partial_Coordination(List,max_coor,Dmax, Cell, Spg, A)
         !---- Arguments ----!
         integer,              intent(in)   :: List
         integer,              intent(in)   :: max_coor
         real(kind=cp),        intent(in)   :: dmax
         type (Cell_G_Type),   intent(in)   :: Cell
         Class(SpG_Type),      intent(in)   :: SpG
         type (AtList_Type),   intent(in)   :: A
      End Subroutine Set_TDist_Partial_Coordination

      Module Subroutine Torsion_and_Sigma(Cellp, x1,x2,x3,x4,sx1,sx2,sx3,sx4,tor,s)
         !---- Arguments ----!
         Type(Cell_G_Type),         intent(in)  :: Cellp         ! Cell object
         real(kind=cp), dimension(3),     intent(in)  :: x1,x2,x3,x4       ! Three points in fractional coordinates and sigmas, X0 is central
         real(kind=cp), dimension(3),     intent(in)  :: sx1,sx2,sx3,sx4   ! Sigmas of the three points
         real(kind=cp),                   intent(out) :: tor,s             ! Angle and sigma
      End Subroutine Torsion_and_Sigma

    End interface

    contains

      Subroutine init_opMatTr(SpG)
        class(SPG_Type), intent(in) :: SpG
        integer :: i
        if(.not. init_symOp) then
           ! Private symetry operators to accelerate calculations
           if(allocated(opMat)) deallocate(opMat)
           if(allocated(opTr))  deallocate(opTr)
           allocate(opMat(3,3,SpG%Multip),opTr(3,SpG%Multip))
           do i=1,SpG%Multip
             opMat(:,:,i)= SpG%Op(i)%Mat(1:3,1:3)
              opTr(:,i)  = SpG%Op(i)%Mat(1:3,4)
           End do
           init_symOp=.true.
        end if
      End Subroutine init_opMatTr

      Subroutine Clear_init_symOp()
         !> Set init_symOp public variable to false
        init_symOp=.false.
      End Subroutine Clear_init_symOp

 End Module CFML_Geom
