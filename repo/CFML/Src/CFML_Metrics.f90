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
!!---- MODULE: CFML_Crystal_Metrics
!!----   INFO: Module to define crystallographic types and to provide
!!----         automatic crystallographic operations.
!!----
!!----
!!--.. INFORMATION
!!--..
!!--..    List Of Matrix Relationships For Crystallographic Applications
!!--..
!!--..    Small "t" is for transpose, inv(F) is the inverse of matrix F
!!--..
!!--..    Basis vectors as symbolic matrices
!!--..       At = (a,b,c)  At'=(a',b',c') ;  At* = (a*,b*,c*)  At*'=(a*',b*',c*')
!!--..
!!--..    Direct and reciprocal metric tensors: G, G*=inv(G)
!!--..    X  column vector in     direct space, referred to basis A
!!--..    X* column vector in reciprocal space, referred to basis A*
!!--..
!!--..       A'  = M  A           X'  = inv(Mt) X
!!--..       A*  = G* A           X*  =   G     X
!!--..       A*' = inv(Mt) A*     X*' =   M     X*
!!--..
!!--..       G' = M G Mt          G*' = inv(Mt) G* inv(M)
!!--..
!!--..   Symmetry operator defined in bases: A, A', A*, A*'
!!--..       C = (R,T), C'= (R',T'), C*= (R*,T*), C*'= (R*',T*')
!!--..
!!--..       R'  = inv(Mt) R Mt  ; T' = inv(Mt) T
!!--..       R*' =  M  R* inv(M) ; T*' = M T*
!!--..       R*  = G R G*  = inv(Rt)
!!--..
!!--..   If a change of origin is performed the positions are changed
!!--..   Ot=(o1,o2,o3) origin of the new basis A' w.r.t. old basis A
!!--..
!!--..       X' = inv(Mt) (X-O)
!!--..
!!--..   Changing just the origin   Xn  = C  X  = R  X  + T
!!--..                              Xn' = C' X' = R' X' + T'
!!--..          R=R'                X'  = X -O
!!--..                              Xn' = Xn-O
!!--..                  Xn-O = R' (X-O) + T' = R X + T - O
!!--..                   R X - R O + T' = R X + T - O
!!--..                               T' = T - (O - R O) = T - (E-R)O
!!--..
!!--..   Changing the basis (A,o) -> (A',o')
!!--..                  Xn  = C  X  = R  X  + T
!!--..                  Xn' = C' X' = R' X' + T'
!!--..                  X'= inv(Mt) (X-O), Xn' = inv(Mt) (Xn-O)
!!--..
!!--..            inv(Mt) (Xn-O) = R' inv(Mt) (X-O) + T'
!!--..            inv(Mt) (R  X  + T -O) = R' inv(Mt) (X-O) + T'
!!--..            inv(Mt) R X + inv(Mt)(T-O) = R' inv(Mt) X - R' inv(Mt) O + T'
!!--..            inv(Mt) R = R' inv(Mt)  => R' = inv(Mt) R Mt
!!--..            inv(Mt) (T-O)  = - R' inv(Mt) O + T'
!!--..            T' = R' inv(Mt) O + inv(Mt) (T-O)
!!--..            T' = inv(Mt) R Mt inv(Mt) O + inv(Mt) (T-O)
!!--..            T' = inv(Mt) R  O + inv(Mt) (T-O)
!!--..            T' = inv(Mt) R  O + inv(Mt) T - inv(Mt) O
!!--..            T' = inv(Mt)( R  O + T -  O) = inv(Mt) (T -(E-R)O)
!!--..
!!--..
!!--..                       R' = inv(Mt) R Mt
!!--..
!!--..                       T' = inv(Mt) (T -(E-R)O)
!!--..
!!--..
!!--..   A symmetry operator does not change the modulus of vectors and
!!--..   the angles between vectors (dot product is invariant):
!!--..
!!--..      X' = R X ,  Y' = R Y  =>  Xt' = Xt Rt,  Yt' = Yt Rt
!!--..
!!--..      Xt' G Y' = Xt Rt G R Y = Xt G Y  =>  G = Rt G R
!!--..
!!--..
!!--..   Second rank tensor Q and Q* defined in bases A and A*.
!!--..
!!--..      Q' = M Q Mt      Q* = G* Q G*     Q*'= inv(Mt) Q* inv(M)
!!--..
!!--..   A symmetry operator R is equivalent to a transformation
!!--..   M = inv(Rt) acting on basis vectors => G' = inv(Rt) G inv(R) = G
!!--..   The anisotropic temperature factors Beta is defined in reciprocal
!!--..   space: is a tensor like Q*, the transformation of beta under
!!--..   a symmetry operator is then :
!!--..
!!--..           Beta' = Inv(Mt) Beta inv(M) = R Beta Rt
!!--..
!!----
!!----
!!
 Module CFML_Metrics

    !---- Use files ----!
    Use CFML_GlobalDeps,  only: CP, EPS, PI, TO_RAD, Err_CFML, CFML_debug
    Use CFML_Maths,       only: Inverse_Matrix, Determ_V, Determ, Cross_Product, &
                                Co_Linear, Sort, Co_Prime, Swap
    Use CFML_Strings,     only: U_Case, Get_Transf

    implicit none

    private

    !---- Public Functions ----!
    public :: Cart_U_Vector, Cart_Vector,  &
              Get_B_from_Betas, Get_B_from_U, Get_Betas_from_B, Get_Betas_from_Biso, &
              Get_Betas_from_U, Get_U_from_B, Get_U_from_Betas, &
              Get_Basis_From_UVW, Get_Deriv_Orth_Cell, Get_Transfm_Matrix , &
              Get_TwoFold_Axes, Rot_Gibbs_Matrix, &
              SigmaV_From_Cell, Set_Cell, Strain_from_Cell, &
              U_Equiv, Volume_from_Cell

    !---- Public Subroutine ----!
    public :: Calc_Paxes_Angles, Change_Setting_Cell, &
              Fix_Tensor, Find_Lowindex_Dir,          &
              Get_Conventional_Cell, Get_Cryst_Family, Get_Primitive_Cell, &
              Init_Strain_Tensor,   &
              Niggli_Cell, &
              Read_Bin_Crystal_Cell, Set_Crystal_Cell, &
              Write_Crystal_Cell, Write_Bin_Crystal_Cell


    !---- Definitions ----!

    !!----
    !!---- CELL_TYPE
    !!--..
    Type, public :: Cell_Type
       real(kind=cp),dimension(3)   :: cell  =0.0_cp      ! Cell parameters
       real(kind=cp),dimension(3)   :: scell =0.0_cp      ! Standard deviation for cell paramaters
       real(kind=cp),dimension(3)   :: ang   =0.0_cp      !
       real(kind=cp),dimension(3)   :: sang  =0.0_cp      !
       real(kind=cp)                :: vol   =0.0_cp      ! Volume and sig(V)
       real(kind=cp)                :: svol  =0.0_cp      !
    End Type Cell_Type

    !!----
    !!---- CELL_G_TYPE
    !!--..
    Type, public, extends(Cell_Type):: Cell_G_Type
       real(kind=cp),dimension(3)   :: rcell      =0.0_cp  ! Reciprocal Cell parameters
       real(kind=cp),dimension(3)   :: rang       =0.0_cp  !
       real(kind=cp)                :: rvol       =0.0_cp
       real(kind=cp),dimension(3,3) :: GD         =0.0_cp  ! Direct Metric Tensor
       real(kind=cp),dimension(3,3) :: GR         =0.0_cp  ! Reciprocal Metric Tensor
       real(kind=cp),dimension(3,3) :: Cr_Orth_cel=0.0_cp  ! Fractional to Cartesian
       real(kind=cp),dimension(3,3) :: Orth_Cr_cel=0.0_cp  ! Cartesian to Fractional
       real(kind=cp),dimension(3,3) :: BL_M       =0.0_cp  ! Busing-Levy B-matrix
       real(kind=cp),dimension(3,3) :: Inv_BL_M   =0.0_cp  ! Inverse of Busing-Levy B-matrix
       character(len=2)             :: CartType   ="CA"    ! if "CA" x// a
    End Type Cell_G_Type

    !!----
    !!---- CELL_LS_TYPE
    !!--..
    Type, public, extends(Cell_Type) :: Cell_LS_Type
       integer, dimension(3) :: lcell=0   ! code for Refinements
       integer, dimension(3) :: lang =0   ! code for Refinements
    End Type Cell_LS_Type

    !!----
    !!---- CELL_GLS_TYPE
    !!--..
    Type, public, extends(Cell_G_Type) :: Cell_GLS_Type
       integer, dimension(3) :: lcell=0   ! code for Refinements
       integer, dimension(3) :: lang =0   ! code for Refinements
    End Type Cell_GLS_Type

    !!----
    !!---- TWOFOLD_AXES_TYPE
    !!--..
    !!
    Type, public :: Twofold_Axes_Type
       integer                        :: ntwo    =0                  ! Number of two-fold axes
       real(kind=cp)                  :: tol     =3.0_cp             ! Angular tolerance (ca 3 degrees)
       real(kind=cp) ,dimension(3,12) :: caxes   =0.0_cp             ! Cartesian components of two-fold axes
       integer,dimension(3,12)        :: dtwofold=0                  ! Direct indices of two-fold axes
       integer,dimension(3,12)        :: rtwofold=0                  ! Reciprocal indices of two-fold axes
       integer,dimension(12)          :: dot     =0                  ! Scalar product of reciprocal and direct indices
       real(kind=cp), dimension(12)   :: cross   =0.0_cp             ! Angle between direct and reciprocal axes ( < tol)
       real(kind=cp), dimension(12)   :: maxes   =0.0_cp             ! Modulus of the zone axes (two-fold axes) vectors
       real(kind=cp), dimension(3)    :: a       =0.0_cp             ! Cartesian components of direct cell parameters
       real(kind=cp), dimension(3)    :: b       =0.0_cp
       real(kind=cp), dimension(3)    :: c       =0.0_cp
    End Type Twofold_Axes_Type

    !!----
    !!---- ZONE_AXIS_TYPE
    !!--..
    !!
    Type, public :: Zone_Axis_Type
      Integer               :: nlayer =0   ! number of the reciprocal layer considered normally nlayer=0
      Integer, dimension(3) :: uvw    =0   ! Indices of the zone axis
      Integer, dimension(3) :: rx     =0   ! Indices (reciprocal vector) of the basis vector 1
      Integer, dimension(3) :: ry     =0   ! Indices (reciprocal vector) of the basis vector 2
    End Type Zone_Axis_Type

    !!----
    !!----  TYPE :: STRAIN_TENSOR_TYPE
    !!--..
    Type, public :: Strain_Tensor_Type
       integer                       :: Iref=  0         ! Cell number in dat file used as reference
       integer                       :: Icell= 0         ! Cell number in dat file used as final cell
       integer                       :: Istype=0         ! Strain type
       type(cell_g_type)             :: cell0            ! cfml data structure for the reference cell
       type(cell_g_type)             :: cell1            ! cfml data structure for the final cell
       character(len=2)              :: carType          ! Cartesian axial choice: first character specifies real axis
                                                         ! parallel to Cart, second recip axis with a or a* always
                                                         ! close to X, b or b* always close to Y
       character(len=40)             :: System=" "       ! Crystal System  (transferred from dat structures)

       real(kind=cp),dimension(0:1,1:2,1:2) :: pt        ! The p & t of cell 0 and 1: pt(0,1,1) is P1, pt(0,2,1) is T1 etc,
                                                         ! last value=2 is esd

       !> values normally calculated from two cells
       real(kind=cp), dimension(3,3)   :: e=       0.0   ! Strains
       real(kind=cp), dimension(3,3)   :: esd=     0.0   ! Strain esd values
       real(kind=cp), dimension(3)     :: eval=    0.0   ! Eigen values in ascending order
       real(kind=cp), dimension(3)     :: evalesd= 0.0   ! Eigen values esds
       real(kind=cp), dimension(3,3)   :: evec=    0.0   ! Eigenvector components in same order: evec(1:3,i) holds the
                                                         ! ith vector components wrt Cartesian axes.
       real(kind=cp), dimension(3,3,2) :: cart_ang=0.0   ! Angles of eigenvectors to Cartesian axes cart_ang(1:3,i,1)
                                                         ! has the angles for the i'th eigenvector, cart_ang(1:3,i,2)
                                                         ! the esd
       real(kind=cp), dimension(3,3,4) :: cell_ang=0.0   ! Angles of eigenvectors to cell axes of reference cell
                                                         ! cell_ang(1:3,i,1) has the angles for the i'th eigenvector,
                                                         ! cell_ang(1:3,i,2) the esd, last index 3,4 angles to recip cell
       real(kind=cp), dimension(3,2,4) :: dir_close      ! Closest low index hkl and angle to evec i in dir_close(i,1,1:4), same for UVW in dir_close(i,2,1:4)

       !> Property values calculated directly from strain values and PT: Stored because makes output easier!
       real(kind=cp), dimension(3,3) :: ep=      0.0     ! Strains
       real(kind=cp), dimension(3,3) :: esdp=    0.0     ! Strain esd values
       real(kind=cp), dimension(3)   :: evalp=   0.0     ! Eigen values in ascending order
       real(kind=cp), dimension(3)   :: evalpesd=0.0     ! Eigen values esds
       character(len=60)             :: property=''      ! property for strain, eg thermal expansion, compressibility

    End Type Strain_Tensor_Type


    !> Parameters
    real(kind=cp), parameter                 :: TPI2=2.0*PI*PI
    real(kind=cp), dimension(3,3), parameter :: IDENTITY= &
                   reshape ([1.0,0.0,0.0, 0.0,1.0,0.0, 0.0,0.0,1.0],[3,3])


    !---- Overloaded ----!
    Interface Change_Setting_Cell
       Module Procedure Change_Setting_Cell_Mat
       Module Procedure Change_Setting_Cell_Symb
    End Interface Change_Setting_Cell

    Interface  Niggli_Cell                   ! The first(s) argument(s) is(are)
      Module Procedure Niggli_Cell_ABC       ! List of cell parameters passed as a 6D vector
      Module Procedure Niggli_Cell_Mat       ! Niggli matrix passed as a 2x3 matrix (ultimately applying the algorithm)
      Module Procedure Niggli_Cell_Params    ! List of cell parameters a,b,c,alpha,beta,gamma
      Module Procedure Niggli_Cell_Type      ! The object Cell is passed as argument
      Module Procedure Niggli_Cell_Vect      ! Input three vectors in Cartesian components
    End Interface  Niggli_Cell

    Interface
       Pure Module Function Cart_U_Vector(Mode,V,Cell) Result(Vc)
          !---- Arguments ----!
          character(len=*),            intent(in) :: Mode   ! Options, D, R, BL, BLD
          real(kind=cp), dimension(3), intent(in) :: v      ! Vector
          class(Cell_G_Type),          intent(in) :: Cell   ! Cell object
          real(kind=cp), dimension(3)             :: vc
       End Function Cart_U_Vector

       Pure Module Function Cart_Vector(Mode,V,Cell) Result(Vc)
          !---- Arguments ----!
          character(len=*),            intent(in) :: mode      !  D: Direct, R: Reciprocal, BL or BLD
          real(kind=cp), dimension(3), intent(in) :: v         !  Vector
          class(Cell_G_Type),          intent(in) :: Cell      !  Cell object
          real(kind=cp), dimension(3)             :: vc
       End Function Cart_Vector

       Module Subroutine Change_Setting_Cell_Mat(Cell,Mat,Celln,Matkind)
          !---- Arguments ----!
          class(Cell_G_Type),            intent( in)     :: Cell
          real(kind=cp), dimension (3,3),intent( in)     :: Mat
          class(Cell_G_Type),            intent(out)     :: Celln
          character(len=*),  optional,   intent (in)     :: Matkind
       End Subroutine Change_Setting_Cell_Mat

       Module Subroutine Change_Setting_Cell_Symb(Cell,sett,Celln)
          !---- Arguments ----!
          class(Cell_G_Type),            intent( in)     :: Cell
          character(len=*),              intent (in)     :: sett
          class(Cell_G_Type),            intent(out)     :: Celln
       End Subroutine Change_Setting_Cell_Symb

       Pure Module Function Get_B_from_Betas(Beta,Cell) Result(B)
          !---- Arguments ----!
          real(kind=cp),dimension(6), intent(in)  :: Beta
          class(Cell_G_Type),         intent(in)  :: Cell
          real(kind=cp),dimension(6)              :: B
       End Function Get_B_from_Betas

       Pure Module Function Get_B_from_U(U) Result(B)
          !---- Arguments ----!
          real(kind=cp),dimension(6), intent(in)  :: U
          real(kind=cp),dimension(6)              :: B
       End Function Get_B_from_U

       Pure Module Function Get_Betas_from_B(B,Cell) Result(Beta)
          !---- Arguments ----!
          real(kind=cp),dimension(6), intent(in)  :: B
          class(Cell_G_Type),         intent(in)  :: Cell
          real(kind=cp),dimension(6)              :: Beta
       End Function Get_Betas_from_B

       Pure Module Function Get_Betas_from_Biso(Biso,Cell) Result(Betas)
          !--- Argument ----!
          real(kind=cp),           intent(in)  :: Biso
          class(Cell_G_Type),      intent(in)  :: Cell
          real(kind=cp), dimension(6)          :: Betas
       End Function Get_Betas_from_Biso

       Pure Module Function Get_Betas_from_U(U,Cell) Result(Beta)
          !---- Arguments ----!
          real(kind=cp),dimension(6),intent(in)  :: U
          class(Cell_G_Type),        intent(in)  :: Cell
          real(kind=cp),dimension(6)             :: Beta
       End Function Get_Betas_from_U

       Pure Module Function Get_U_from_B(B) Result(U)
          !---- Arguments ----!
          real(kind=cp),dimension(6),  intent(in)  :: B
          real(kind=cp),dimension(6)               :: U
       End Function Get_U_from_B

       Pure Module Function Get_U_from_Betas(Beta,Cell) Result(U)
          !---- Arguments ----!
          real(kind=cp),dimension(6),intent(in)  :: Beta
          class(Cell_G_Type),        intent(in)  :: Cell
          real(kind=cp),dimension(6)             :: U
       End Function Get_U_from_Betas

       Module Function Get_Basis_From_UVW(dmin,u,cell,mode) Result(ZoneB)
          !--- Arguments ---!
          real(kind=cp),              intent(in) :: dmin      ! Minimum d-spacing (smax=1/dmin)
          integer, dimension(3),      intent(in) :: u         ! Zone axis indices
          class(Cell_G_Type),         intent(in) :: cell      ! Cell object
          character(len=*), optional, intent(in) :: mode
          type (Zone_Axis_Type)                  :: ZoneB     ! !Object containing u and basis vector in the plane
       End Function Get_Basis_From_UVW

       Module Function Set_Cell(vcell,vang,carttype,vscell,vsang) result(cell)
          !> Set crystal cell from cell parameters
         real(kind=cp), dimension(3),           intent(in) :: vcell    !> Cell parameters a,b,c
         real(kind=cp), dimension(3),           intent(in) :: vang     !> Cell parameters alpha,beta,gamma
         character (len=*),           optional, intent(in) :: carttype !> Cartesian coordinate system: 'CA' | 'BC' | 'BA' | 'CB'
         real(kind=cp), dimension(3), optional, intent(in) :: vscell   !> Standard deviations of vcell
         real(kind=cp), dimension(3), optional, intent(in) :: vsang    !> Standard deviations of vang
         type(Cell_G_Type)                                 :: cell     !> Crystal cell
       End Function Set_Cell

       Module Function Strain_from_Cell(Itype,T0,T1) Result(strain)
          !---- Arguments ----!
          integer,                       intent(in) :: itype  ! Strain type
          real(kind=cp), dimension(3,3), intent(in) :: T0     ! CR_Orth_Cel for chosen axial system for the starting state
          real(kind=cp), dimension(3,3), intent(in) :: T1     ! CR_Orth_Cel for chosen axial system for the final state
          real(kind=cp), dimension(3,3)             :: Strain ! calculated cell strain
       End Function Strain_from_Cell

       Module Subroutine Get_Conventional_Cell(Twofold,Cell,Tr,Message,told,lattice)
          !---- Arguments ----!
          Type(Twofold_Axes_Type),    intent(in)  :: Twofold
          class(Cell_G_Type),         intent(out) :: Cell
          integer, dimension(3,3),    intent(out) :: tr
          character(len=*),           intent(out) :: message
          real(kind=cp), optional,    intent(in)  :: told
          character(len=2), optional, intent(out) :: lattice
       End Subroutine Get_Conventional_Cell

       Module Subroutine Get_Cryst_Family(Cell, Family, Symbol, System)
          !---- Arguments ----!
          class(Cell_G_Type),     intent(in ) :: Cell
          character(len=*),       intent(out) :: Family
          character(len=*),       intent(out) :: Symbol
          character(len=*),       intent(out) :: System
       End Subroutine Get_Cryst_Family

       Module Function Get_Cryst_Orthog_Matrix(Cell, Ang, CarType) Result(Mat)
          !---- Arguments ----!
          real(kind=cp), dimension(3  ), intent (in ) :: cell,ang   ! Cell Parameters
          character(len=*), optional,    intent (in ) :: CarType    ! Type of Cartesian axes
          real(kind=cp), dimension(3,3)               :: Mat        ! Convsersion matrix
       End Function Get_Cryst_Orthog_Matrix

       Module Function Get_Deriv_Orth_Cell(Cell,Cartype) Result(De_Orthcell)
          !---- Arguments ----!
          class(Cell_G_Type),              intent(in ) :: cell
          character(len=*), optional,      intent(in ) :: CarType
          real(kind=cp), dimension(3,3,6)              :: De_Orthcell
       End Function Get_Deriv_Orth_Cell

       Pure Module Function Get_Metrics(cell,ang) Result(G)
          !---- Arguments ----!
          real(kind=cp), dimension(3)  , intent(in ) :: cell  ! Cell Parameters
          real(kind=cp), dimension(3)  , intent(in ) :: ang
          real(kind=cp), dimension(3,3)              :: G     ! Metric Tensor
       End Function Get_Metrics

       Module Subroutine Get_Primitive_Cell(Lat_Type,C_Cell,P_Cell,Transfm)
          !---- Arguments ----!
          character(len=*),              intent(in)  :: lat_type    ! Lattice type
          class(Cell_G_Type),            intent(in)  :: c_cell      ! Input Cell Object
          class(Cell_G_Type),            intent(out) :: p_cell      ! Output Cell Object
          real(kind=cp), dimension(3,3), intent(out) :: transfm     ! Transformation Matrix between Cell objects
       End Subroutine Get_Primitive_Cell

       Module Function Get_TwoFold_Axes(Cell,Tol) Result(Twofold)
          !---- Arguments ----!
          class(Cell_G_Type),      intent (in) :: Cell     ! Cell object
          real(kind=cp),           intent (in) :: Tol      ! angular tolerance in degrees
          Type(twofold_axes_type)              :: Twofold
       End Function Get_TwoFold_Axes

       Module Function Get_Transfm_Matrix(cella,cellb,tol) Result(Trm)
          !---- Arguments ----!
          class(Cell_G_Type),              intent(in) :: cella  ! Cell object
          class(Cell_G_Type),              intent(in) :: cellb  ! Cell object
          real(kind=cp), optional,         intent(in) :: tol    ! Tolerance
          real(kind=cp), dimension(3,3)               :: trm    ! Transformation matrix
       End Function Get_Transfm_Matrix

       Module Subroutine Niggli_Cell_ABC(VCell,Niggli_Point,Cell,Trans)
          !---- Arguments ----!
          real(kind=cp),dimension(6),              intent(in out) :: VCell
          real(kind=cp),dimension(5),    optional, intent(   out) :: Niggli_Point
          class(Cell_G_Type),            optional, intent(   out) :: Cell
          real(kind=cp), dimension(3,3), optional, intent(   out) :: Trans
       End Subroutine Niggli_Cell_ABC

       Module Subroutine Niggli_Cell_Mat(N_Mat,Niggli_Point,Cell,Trans)    !Scalar algorithm
          !---- Arguments ----!
          real(kind=cp),dimension(2,3),              intent(in out) :: N_mat
          real(kind=cp),dimension(5),      optional, intent(out)    :: Niggli_Point
          class(Cell_G_Type),              optional, intent(out)    :: cell
          real(kind=cp), dimension(3,3),   optional, intent(out)    :: trans
       End Subroutine Niggli_Cell_Mat

       Module Subroutine Niggli_Cell_Params(A,B,C,Alpha,Beta,Gamma,Niggli_Point,Cell,Trans)
          !---- Arguments ----!
          real(kind=cp),                           intent(in out)  :: a,b,c,alpha,beta,gamma
          real(kind=cp), dimension(5),   optional, intent(   out)  :: Niggli_Point
          class(Cell_G_Type),            optional, intent(   out)  :: Cell
          real(kind=cp), dimension(3,3), optional, intent(   out)  :: Trans
       End Subroutine Niggli_Cell_Params

       Module Subroutine Niggli_Cell_Type(Cell,Niggli_Point,Celln,Trans)
          !---- Arguments ----!
          class(Cell_G_Type),                      intent(in out ) :: cell
          real(kind=cp),dimension(5),    optional, intent(   out)  :: Niggli_Point
          class(Cell_G_Type),            optional, intent(   out)  :: celln
          real(kind=cp), dimension(3,3), optional, intent(   out)  :: trans
       End Subroutine Niggli_Cell_Type

       Module Subroutine Niggli_Cell_Vect(Vec1,Vec2,Vec3,Niggli_Point,Cell,Trans)
          !---- Arguments ----!
          real(kind=cp),dimension(3),                intent(in)     :: Vec1, Vec2, Vec3
          real(kind=cp),dimension(5),      optional, intent(out)    :: Niggli_Point
          class(Cell_G_Type),              optional, intent(out)    :: cell
          real(kind=cp), dimension(3,3),   optional, intent(out)    :: trans
       End Subroutine Niggli_Cell_Vect

       Module Subroutine Read_Bin_Crystal_Cell(Cell,Iunit)
          !---- Arguments ----!
          class(Cell_Type),  intent(out) :: Cell       ! Cell object
          integer,           intent(in)  :: Iunit
       End Subroutine Read_Bin_Crystal_Cell

       Pure Module Subroutine Reciprocal_Cell(cell,ang,rcell,rang,rVol)
          !---- Arguments ----!
          real(kind=cp), dimension(3), intent(in ) :: cell,ang
          real(kind=cp), dimension(3), intent(out) :: rcell,rang
          real(kind=cp),               intent(out) :: rvol
       End Subroutine Reciprocal_Cell

       Pure Module Function Rot_Gibbs_Matrix(V,Phi,Cell) Result(Mat)
          !---- Argument ----!
          real(kind=cp), dimension(3),      intent(in) :: V     ! Direction vector
          real(kind=cp),                    intent(in) :: phi   ! Angle in Degrees of rotation around V
          class(Cell_G_Type), optional,     intent(in) :: cell  ! Cell object
          real(kind=cp), dimension(3,3)                :: Mat   ! Gibbs Matrix corresponding to the rotation around V of an angle Phi
       End Function Rot_Gibbs_Matrix

       Module Subroutine Set_Crystal_Cell(VCell,VAng,Cell,Cartype,Vscell,Vsang)
          ! Arguments
          real(kind=cp), dimension(3),         intent(in)  :: vcell   ! Cell parameters a,b,c
          real(kind=cp), dimension(3),         intent(in)  :: vang    ! Cell parameters alpha,beta,gamma
          class(Cell_G_Type),               intent(inout)  :: cell    ! Crystal cell
          character (len=*),          optional,intent(in ) :: carType ! Cartesian coordinate system: 'CA' | 'BC' | 'BA' | 'CB'
          real(kind=cp), dimension(3),optional,intent(in ) :: vscell  ! Standard deviations of vcell
          real(kind=cp), dimension(3),optional,intent(in ) :: vsang   ! Standard deviations of vang
       End Subroutine Set_Crystal_Cell

       Pure Module Function SigmaV_From_Cell(Cell) Result(sigma)
          !---- Arguments ----!
          class(Cell_G_Type), intent(in) :: Cell      ! Cell Parameters
          real(kind=cp)                  :: sigma     ! Sigma
       End Function SigmaV_From_Cell

       Pure Module Function U_Equiv(Cell, Th_U) Result(Uequi)
          !---- Arguments ----!
          class(Cell_G_Type),          intent(in)  :: Cell    ! Cell object
          real(kind=cp), dimension(6), intent(in)  :: Th_U    ! U thermal parameters
          real(kind=cp)                            :: Uequi   ! Uequiv
       End Function U_Equiv

       Pure Module Function Volume_from_Cell(cell,ang) Result(Vol)
          !---- Arguments ----!
          real(kind=cp), dimension(3), intent(in) :: cell
          real(kind=cp), dimension(3), intent(in) :: ang
          real(kind=cp)                           :: vol
       End Function Volume_from_Cell

       Module Subroutine Write_Bin_Crystal_Cell(Cell,Iunit)
          !---- Arguments ----!
          class(Cell_Type),  intent(in) :: Cell       ! Cell object
          Integer,           intent(in) :: Iunit
       End Subroutine Write_Bin_Crystal_Cell

       Module Subroutine Write_Crystal_Cell(Cell, Iunit)
          !---- Arguments ----!
          class(Cell_Type),    intent(in) :: Cell         ! Cell object
          integer, optional,   intent(in) :: Iunit
       End Subroutine Write_Crystal_Cell

       Module Subroutine Fix_Tensor(A,Sys_In)
          !---- Arguments ----!
          real(kind=cp), dimension(3,3), intent(in out) :: A
          character(len=*),              intent(in)     :: Sys_in
       End Subroutine Fix_Tensor

       Module Subroutine Init_Strain_Tensor(T)
          !---- Argument ----!
          type(Strain_Tensor_Type), intent(in out) :: T
       End Subroutine Init_Strain_Tensor

       Module Subroutine Find_Lowindex_Dir(Cvec,Cell,Space,Irange,Ind,Ang)
          !---- Arguments ----!
          real(kind=cp), dimension(3), intent(in)  :: Cvec
          type(cell_G_type),           intent(in)  :: Cell
          character(len=*),            intent(in)  :: Space
          integer,                     intent(in)  :: Irange
          real(kind=cp),dimension(3),  intent(out) :: Ind
          real(kind=cp),               intent(out) :: Ang
       End Subroutine Find_Lowindex_Dir

       Module Subroutine Calc_Paxes_Angles(x,c,index_range)
          !---- Arguments ----!
          type(Strain_Tensor_Type), intent(inout) :: X
          type(cell_G_type),        intent(in)    :: c
          integer,                  intent(in)    :: index_range
       End Subroutine Calc_Paxes_Angles

    End Interface


 End Module CFML_Metrics
