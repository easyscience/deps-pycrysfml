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
!!---- MODULE: Molecules
!!----
!!--..    Explanations about Eulerian angles, active and passive rotations
!!--..    Ref. Texture Analysis in Material Science, H.J. Bunge.
!!--..    Ed Butterworths London 1970?
!!--..
!!--..
!!--..    First variant:
!!--..    -------------
!!--..    Eulerian angles g={phi1,PHI,phi2}, positive rotations: anti-clockwise
!!--..
!!--..     1: Rotation around the common Z,Zm axis of an angle phi1
!!--..     2: Rotation around the new Xm axis of an angle PHI
!!--..     3: Rotation around the new Zm-axis of an angle phi2
!!--..
!!--..          g = gZm(phi2) . gXm(PHI) . gZm(Zm)
!!--..
!!--..
!!--..                   (  cosphi2  sinphi2    0  )                     (   1      0       0      )
!!--..
!!--..       gZm(phi2) =(  -sinphi2  cosphi2    0   )        gXm(PHI)  =(    0    cosPHI  sinPHI    )
!!--..
!!--..                   (    0         0       1  )                     (   0   -sinPHI  cosPHI   )
!!--..
!!--..
!!--..
!!--..                   (  cosphi1  sinphi1    0  )
!!--..
!!--..       gZm(phi1) =(  -sinphi1  cosphi1    0   )
!!--..
!!--..                   (    0         0       1  )
!!--..
!!--..
!!--..
!!--..    Second variant:
!!--..    ---------------
!!--..     Eulerian angles g={PSI,THETA,PHI}, positive rotations: anti-clockwise
!!--..
!!--..     1: Rotation around the common Z,Zm axis of an angle PSI   (PHI)
!!--..     2: Rotation around the new Ym axis of an angle THETA      (THETA) <--- FullProf
!!--..     3: Rotation around the new Zm-axis of an angle PHI        (CHI)
!!--..
!!--..       phi1=PSI+pi/2   PHI = THETA   phi2=PHI - pi/2
!!--..
!!--..
!!--..
!!--..    Rotation Axis and Rotation Angle
!!--..    --------------------------------
!!--..
!!--..    The rotation axis is given by the unit vector u represented by its polar
!!--..    coordinates (theta,phi) and the rotation angle (omega) around u, so that
!!--..    one can write the rotation g={u,omega}={theta,phi,omega}
!!--..
!!--..    Passive rotations: one looks for the coordinates of a unique point respect
!!--..                       to two rotated frames
!!--..
!!--..                             ( cosphi  sinphi)
!!--..         -----\--------  M = (               )     is the matrix relating the basis (e)=M(i)
!!--..         |\    \             (-sinphi  cosphi)
!!--..         | \    \
!!--..         |  \    r        The point r  has coordinates (x,y) and coordinates (x',y')
!!--..         |   \ /          w.r.t. the rotated axes the relation is:
!!--..         |    \
!!--..         | phi \          (x')   ( cosphi  sinphi)  (x)     x'= x cosphi + y sinphi
!!--..                          (  ) = (               )  ( )  => y'=-x sinphi + y cosphi
!!--..                          (y')   (-sinphi  cosphi)  (y)
!!--..
!!--..     Active  rotations: one looks for the new coordinates of a point respect
!!--..                       to the same frame when a rotation is applied
!!--..
!!--..
!!--..                             ( cosphi -sinphi)
!!--..         --------------  R = (               )
!!--..         |\                  ( sinphi  cosphi)
!!--..         | \
!!--..         |  \   r'           x'= x cosphi - y sinphi
!!--..         | r \               y'= x sinphi + y cosphi
!!--..         |    \
!!--..         | phi \
!!--..
!!--..
!!--..      The representative matrices are one the inverse of the other. R=Minv=Mt
!!--..
!!--..      In molecular crystals one looks for determining the position of each atom of
!!--..      the molecule in the crystallographic frame when one knows the internal coordinates
!!--..      of the atoms, the position of the origin of the internal frame in the crystallographic
!!--..      frame and the orientation (Euler or Euler-like angles) of the internal frame with
!!--..      respect to the crystallographic frame.
!!--..
!!--..      The problem is to define a simple set of orientational angles
!!--..
!!--..      We shall adopt the conventional definition of Euler angles but we will call then
!!--..      a=phi1, b=PHI, c=phi2. The above matrices correspond to passive rotations, so that
!!--..      when applied to a fixed point their product will give the coordinates of this point
!!--..      with respect to the rotated system. In our case will give the position of an external
!!--..      point (cartesian crystal  frame, CCF) w.r.t the cartesian molecular frame (CMF).
!!--..      Taking the transpose of the final matrix one obtains an active rotation matrix that
!!--..      applied to a point moves it to a new point referred to the CCF.
!!--..
!!--..
!!--..
!!--..               ( cosa cosc - sina sinc cosb     sina cosc + cosa sinc cosb    sinc sinb )
!!--..
!!--..    g(a,b,c) =( -cosa sinc - sina cosc cosb    -sina sinc + cosa cosc cosb    cosc sinb  )
!!--..
!!--..               (         sina  sinb                   -cosa sinb                 cosb   )
!!--..
!!--..
!!--..               ( cosa cosc - sina sinc cosb    -cosa sinc - sina cosc cosb    sina sinb )
!!--..
!!--..   gt(a,b,c) =(  sina cosc + cosa sinc cosb    -sina sinc + cosa cosc cosb   -cosa sinb  )
!!--..
!!--..               (      sinc sinb                      cosc sinb                   cosb   )
!!--..
!!--..
!!--..
!!--..     The matrix g applied to a point with coordinates given w.r.t. CCF, provides the coordinates
!!--..     w.r.t. CMF. If we take a point in the CMF and we apply the matrix gt we obtain the coordinates
!!--..     of this point w.r.t. CCF.
!!--..
!!--..     Orientational angles used in FullProf
!!--..     -------------------------------------
!!--..
!!--..     The molecular frame (CMF) is supposed to coincide at the begining with the Cartesian
!!--..     crystallographic frame (CCF). To position a molecule in an arbitrary position the
!!--..     total movement is decomposed in the following way:
!!--..
!!--..     1) Perform a rotation of angle CHI around the Z,Zm-axis : the rotation matrix relating
!!--..        the two unitary bases (Em and E in form of columns) is the following:
!!--..
!!--..                       (cosCHI    sinCHI   0 )            (e1)           (i)
!!--..                                                          (  )           ( )
!!--..             Rz(CHI) =(-sinCHI    cosCHI   0  )        Em=(e2) = Rz(CHI) (j) = Rz(CHI) E
!!--..                                                          (  )           ( )
!!--..                       (  0         0      1 )            (e3)           (k)
!!--..
!!--..        An active rotation is obtained transposing Rz(CHI)t = Az(CHI). This matrix is
!!--..        applied to a point in CCF and provides the new coordinates in the CCF after
!!--..        the rotation of angle CHI around Z.
!!--..
!!--..
!!--..     2) Perform a rotation of angle THE around the Y-axis : the rotation matrix relating
!!--..        the two unitary bases (Em and E in form of columns) is now the following:
!!--..
!!--..                       (cosTHE   0   -sinTHE )            (e1)           (i)
!!--..                                                          (  )           ( )
!!--..             Ry(THE) =(   0      1      0     )        Em=(e2) = Ry(THE) (j) = Ry(THE) E
!!--..                                                          (  )           ( )
!!--..                       (sinTHE   0    cosTHE )            (e3)           (k)
!!--..
!!--..        An active rotation is obtained transposing Ry(THE)t = Ay(THE). This matrix is
!!--..        applied to a point in CCF and provides the new coordinates in the CCF after
!!--..        the rotation of angle THE around Y.
!!--..
!!--..     3) Perform a rotation of angle PHI around the Z-axis : the rotation matrix relating
!!--..        the two unitary bases (Em and E in form of columns) is the following:
!!--..
!!--..                       (cosPHI    sinPHI   0 )            (e1)           (i)
!!--..                                                          (  )           ( )
!!--..             Rz(PHI) =(-sinPHI    cosPHI   0  )        Em=(e2) = Rz(PHI) (j) = Rz(PHI) E
!!--..                                                          (  )           ( )
!!--..                       (  0         0      1 )            (e3)           (k)
!!--..
!!--..        An active rotation is obtained transposing Rz(PHI)t = Az(PHI). This matrix is
!!--..        applied to a point in CCF and provides the new coordinates in the CCF after
!!--..        the rotation of angle PHI around Z.
!!--..
!!--..    With this rotational angles the interpretation of the angles (THE,PHI) correspond to
!!--..    the spherical angles of the CMF Zm-axis with respect to the CCF. The total active
!!--..    matrix to be applied to atoms of the molecule in the initial position (when the two
!!--..    frames coincide) to get the final coordinates is the following:
!!--..
!!--..
!!--..                 M = Az(PHI) . Ay(THE) . Az(CHI) =  XA(PHI,THE)  . XAp(CHI)
!!--..
!!--..
!!--..     In the initial state the Cartesian coordinates of atoms (x), in columns, are
!!--..     the same in both frames, the positions after the total rotation are given by:
!!--..
!!--..                         (x)-final =   M (x)
!!--..
!!--..     To obtain the internal coordinates of a point in the CCF one must apply the
!!--..     following formula:
!!--..
!!--..                   X-internal  =  Mt  X = XAp(CHI)t . XA(PHI,THE)t  X
!!--..
!!--..     the final expressions of the different matrices are the following:
!!--..
!!--..
!!--..                    (cosPHI cosTHE      -sinPHI      cosPHI sinTHE )
!!--..
!!--..     XA(PHI,THE) = ( sinPHI cosTHE       cosPHI      sinPHI sinTHE  )
!!--..
!!--..                    (  -sinTHE             0             cosTHE    )
!!--..
!!--..
!!--..                   (cosCHI   -sinCHI   0 )
!!--..
!!--..        XAp(CHI) =( sinCHI    cosCHI   0  )
!!--..
!!--..                   (  0         0      1 )
!!--..
!!--..
!!--..               ( cosa cosc - sina sinc cosb     sina cosc + cosa sinc cosb    sinc sinb )
!!--..
!!--..    g(a,b,c) =( -cosa sinc - sina cosc cosb    -sina sinc + cosa cosc cosb    cosc sinb  )
!!--..
!!--..               (         sina  sinb                   -cosa sinb                 cosb   )
!!--..
!!--..
!!--..               ( cosa cosc - sina sinc cosb    -cosa sinc - sina cosc cosb    sina sinb )
!!--..
!!--..   gt(a,b,c) =(  sina cosc + cosa sinc cosb    -sina sinc + cosa cosc cosb   -cosa sinb  )
!!--..
!!--..               (      sinc sinb                      cosc sinb                   cosb   )
!!--..
!!--..
!!--..
!!--..
!!--..
!!--..
!!--..  M(PHI,THE,CHI) =
!!--..
!!--..     (cosPHI cosTHE cosCHI - sinPHI sinCHI   -cosPHI cosTHE sinCHI - sinPHI cosCHI    cosPHI sinTHE)
!!--..
!!--..   =( sinPHI cosTHE cosCHI + cosPHI sinCHI   -sinPHI cosTHE sinCHI + cosPHI cosCHI    sinPHI sinTHE )
!!--..
!!--..     (       -sinTHE cosCHI                           sinTHE sinCHI                      cosTHE    )
!!--..
!!--..
!!--..
!!--..   Comparing the matrix M(THE,PHI,CHI) with the matrix gt(a,b,c)=gt(alpha,beta,gamma)=gt(phi1,PHI,phi2)
!!--..
!!--..   One can see that both matrices are identical if we take:
!!--..
!!--..        alpha=phi1=PHI+pi/2     beta=PHI=THETA   gamma=phi2=CHI-pi/2
!!--..
!!--..         (phi1=PSI+pi/2   PHI = THETA   phi2=PHI - pi/2)
!!--..
!!--..
!!--..      The angles used in FullProf correspond to the second variant of Euler angles making the
!!--..      sustitution:
!!--..
!!--..        (PSI,THETA,PHI)  --->   (PHI, THETA, CHI)
!!--..
!!--..            2nd variant   -->      FullProf
!!--..
!!--..      This is clear from the following. If we take passive rotations as for deriving the matrix
!!--..  corresponding to the Euler angles the matrix Mt should be the result
!!--..
!!--..      Mt = (  Az(PHI) . Ay(THE) . Az(CHI) )t = Rz(CHI) . Ry(THE) . Rz(PHI)
!!--..
!!--..  Then the interpretation of the rotations are strictly the same as given in the description
!!--..  of the second variant of Euler angles.
!!--..
!!--..
!!----
!!
 Module CFML_Molecules
    !---- Use Modules ----!
    use CFML_GlobalDeps,    only: CP, EPS, TO_RAD, err_cfml, clear_error, set_error
    use CFML_gSpacegroups,  only: SpG_type, Write_SpaceGroup_Info
    Use CFML_Atoms,         only: AtList_Type, Init_Atom_Type, Allocate_Atom_List, &
                                  Atm_Type, Atm_Std_Type, Atm_Ref_Type, ModAtm_Std_Type, ModAtm_Ref_Type
    Use CFML_Metrics,       only: Cell_G_Type,  Write_Crystal_Cell
    Use CFML_Strings,       only: L_Case, U_Case, File_Type, Get_Num, Cut_String, Get_words
    Use CFML_Maths,         only: Cross_Product, Get_Spher_from_Cart
    Use CFML_Geom,          only: Angle_Dihedral, Distance, Get_PhiTheChi

    Use CFML_Scattering_Tables, only: NUM_CHEM_INFO, Set_Chem_Info, Remove_Chem_Info, Get_Chem_Symb, Chem_Info

    implicit none

    private

    !---- List of public functions ----!
    public :: Set_Euler_Matrix


    !---- List of public subroutines ----!
    public :: Cartesian_to_Fractional, Cartesian_to_Spherical, Cartesian_to_ZMatrix, &
              Empiric_Formula, &
              Fractional_to_Cartesian, Fractional_to_Spherical, Fractional_to_Zmatrix, Fix_Orient_Cartesian, &
              Init_Molecule, Init_MolCrystal, Index_AtLab_on_Molecule, &
              Molec_to_AtList, MolCrystal_to_AtList, &
              ReadInfo_Free_Atoms, ReadInfo_Molecule, &
              Spherical_to_Cartesian, Spherical_to_Fractional, Spherical_to_Zmatrix, Set_MolReference, &
              WriteInfo_Molecule, WriteInfo_Molecular_Crystal, WriteInfo_Free_Atoms, &
              Zmatrix_to_Cartesian, Zmatrix_to_Fractional, Zmatrix_to_Spherical


    !!----
    !!----  TYPE :: MOLECULE_TYPE
    !!--..
    !!----  Update: April - 2022
    !!
    Type, public :: Molecule_type
       character(len=80)                               :: Name_mol=' '         !Global name for the molecule
       integer                                         :: natoms=0             !Number of atoms
       logical                                         :: in_xtal=.false.      !True if global coordinates xcentre, orient are defined
       logical                                         :: is_EulerMat=.false.  !True if the Euler Matrix has been set
       logical                                         :: is_connect=.false.   !True if the connectivity is given and correct
       character(len=1)                                :: rot_type='E'         !Type of rotational angles
                                                                               !"E": Conventional Euler angles (alpha,beta,gamma)
                                                                               !"P": Second variant of Euler angles (default)
                                                                               !     Polar:(theta,phi,chi)
       character(len=1)                                :: coor_type='F'        !Type of internal coordinates
                                                                               !"Z": Z-matrix
                                                                               !"C": Cartesian
                                                                               !"S": Spherical
                                                                               !"F": Fractional coordinates (only if in_xtal = .true.)
       character(len=3)                                :: therm_type='ISO'     !Type of thermal factor
                                                                               !"ISO": No collective motion
                                                                               !"T  ": Translational
                                                                               !"TL ": Translational + Librational
                                                                               !"TLS": Translational + Librational + Correlation
       real(kind=cp), dimension(3)                     :: xcentre=0.0_cp       !Fractional coordinates of the centre
       real(kind=cp), dimension(3)                     :: mxcentre=0.0_cp      !Refinement codes (or multipliers) of Fractional coordinates of the centre
       integer,       dimension(3)                     :: lxcentre=0           !Numbers of LSQ parameters for Fractional coordinates of the centre
       real(kind=cp), dimension(3)                     :: Orient=0.0_cp        !Orientation angles (Euler angles or variant ...)
       real(kind=cp), dimension(3)                     :: mOrient=0.0_cp       !Refinement codes (or multipliers) of Orientation angles (Euler angles or variant ...)
       integer,       dimension(3)                     :: lOrient=0            !Numbers of LSQ parameters for Orientation angles (Euler angles or variant ...)
       real(kind=cp), dimension(6)                     :: T_TLS=0.0_cp         !Translational Thermal factor tensor
       real(kind=cp), dimension(6)                     :: mT_TLS=0.0_cp        !Refinement codes (or multipliers) of Translational Thermal factor tensor
       integer,       dimension(6)                     :: lT_TLS=0             !Numbers of LSQ parameters for Translational Thermal factor tensor
       real(kind=cp), dimension(6)                     :: L_TLS=0.0_cp         !Librational Thermal factor tensor
       real(kind=cp), dimension(6)                     :: mL_TLS=0.0_cp        !Refinement codes (or multipliers) of Librational Thermal factor tensor
       integer,       dimension(6)                     :: lL_TLS=0             !Numbers of LSQ parameters for Librational Thermal factor tensor
       real(kind=cp), dimension(3,3)                   :: S_TLS=0.0_cp         !TL-correlation Thermal factor
       real(kind=cp), dimension(3,3)                   :: mS_TLS=0.0_cp        !Refinement codes (or multipliers) of TL-correlation Thermal factor
       integer,       dimension(3,3)                   :: lS_TLS=0             !Numbers of LSQ parameters for TL-correlation Thermal factor
       real(kind=cp), dimension(3,3)                   :: Euler=0.0_cp         !Euler matrix
       character(len=20), allocatable, dimension(  :)  :: AtName               !Atom Name
       character(len=4),  allocatable, dimension(  :)  :: AtSymb               !Atom species
       integer,           allocatable, dimension(  :)  :: AtZ                  !Atomic Number
       integer,           allocatable, dimension(:,:)  :: Ptr                  !Pointer to scat.factors (first index -> pattern)
       real(kind=cp),     allocatable, dimension(:,:)  :: I_coor               !Internal coordinates (d,ang,dang)
       real(kind=cp),     allocatable, dimension(:,:)  :: mI_Coor              !Refinement codes (or multipliers) of internal coordinates
       integer,           allocatable, dimension(:,:)  :: lI_coor              !Numbers of LSQ parameters for internal coordinates
       real(kind=cp),     allocatable, dimension(  :)  :: U_iso                !Isotropic temperature factor
       real(kind=cp),     allocatable, dimension(  :)  :: mU_iso               !Refinement codes (or multipliers) of Isotropic temperature factor
       integer,           allocatable, dimension(  :)  :: lU_iso               !Numbers of LSQ parameters for Isotropic temperature factor
       real(kind=cp),     allocatable, dimension(  :)  :: occ                  !Occupation factor
       real(kind=cp),     allocatable, dimension(  :)  :: mocc                 !Refinement codes (or multipliers) of Occupation factor
       integer,           allocatable, dimension(  :)  :: locc                 !Numbers of LSQ parameters for Occupation factor
       integer,           allocatable, dimension(  :)  :: Nb                   !Number of neighbours
       integer,           allocatable, dimension(:,:)  :: INb                  !Index of neighbours
       integer,           allocatable, dimension(:,:)  :: Tb                   !Type of Bonds
       integer,           allocatable, dimension(:,:)  :: Conn                 !Conectivity (N1,N2,N3)
    End Type Molecule_type

    !!----
    !!----  TYPE :: MOLECULAR_CRYSTAL_TYPE
    !!--..
    !!----  Update: April - 2022
    !!
    Type, public :: MolCrystal_Type
       integer                                              :: N_Free=0         !Number of free atoms
       integer                                              :: N_Mol=0          !Number of Molecules
       integer                                              :: N_Species=0      !Number of species
       integer                                              :: NPat=0
       type(Cell_G_type)                                    :: Cell
       type(SpG_type)                                       :: SpG
       class(Atm_Std_Type),     allocatable, dimension(  :) :: Atm              ! Free atoms
       type(Molecule_type ),    allocatable, dimension(  :) :: Mol              ! Molecule
    End type MolCrystal_Type

    !---- Overloading Section ----!
    Interface Empiric_Formula
       Module Procedure Empiric_Formula_AtList
       Module Procedure Empiric_Formula_Molec
       Module Procedure Empiric_Formula_Molcrys
    End Interface

    !---- Interface Zone ----!

    Interface
       Module Function Index_AtLab_on_Molecule(Lab, Mol) Result(Ind)
          !---- Arguments ----!
          character(len=*),    intent(in) :: Lab
          type(molecule_type), intent(in) :: Mol
          integer                         :: Ind
       End Function Index_AtLab_on_Molecule

       Module Function Set_Euler_Matrix(Rt, Phi, Theta, Chi) Result(Eu)
          !> Computes the Euler active matrix to transform a point
          !> to another point. For instance the internal coordinates of a molecule
          !> can be transformed to absolute positions using columns vectors.
          !>
          !> If the Cartesian coordinates of an atom in the molecular frame is the
          !> column vector  Xm, the cartesian coordinates in the crystal frame X
          !> are obtained from:  X = Eu Xm
          !>
          !> The internal coordinates of a point are obtained from Xm = EuT X.
          !> The character variable "rt" indicates the type of Euler angles provided.
          !> If rt="E", the angles PHI,THETA,CHI correspond to the conventional
          !> Euler angles ALPHA, BETA, GAMMA. Otherwise, they correspond to the
          !> 2nd setting, allowing to interpret PHI and THETA as the polar angles of
          !> the molecular frame Zm-axis, and CHI a rotation around Zm.
          character(len=*),              intent ( in) :: Rt            !> See above
          real(kind=cp),                 intent ( in) :: Phi,Theta,Chi !> Euler angles
          real(kind=cp), dimension(3,3)               :: Eu            !> Euler matrix
       End Function Set_Euler_Matrix

       Module Function Get_Z_from_Cartesian(ri,rj,rk,rn) Result(ci)
          !---- Arguments ----!
          real(kind=cp), dimension(3), intent ( in) :: ri,rj,rk,rn
          real(kind=cp), dimension(3)               :: ci
       End Function Get_Z_from_Cartesian

       Module Function Get_Cartesian_from_Z(ci,rj,rk,rn) Result(ri)
          !---- Arguments ----!
          real(kind=cp), dimension(3), intent ( in) :: ci,rj,rk,rn
          real(kind=cp), dimension(3)               :: ri
       End Function Get_Cartesian_from_Z

       Module Subroutine Cartesian_to_Fractional(Mol, Cell, NMol)
          !---- Arguments ----!
          type (Molecule_type),  intent(in out)           :: Mol
          type (Cell_G_Type),    intent(in)               :: Cell
          type (Molecule_type),  optional, intent(   out) :: NMol
       End Subroutine Cartesian_to_Fractional

       Module Subroutine Cartesian_to_Spherical(Mol, NMol)
          !---- Arguments ----!
          type (Molecule_type), intent(in out)           :: Mol
          type (Molecule_type), intent(   out), optional :: NMol
       End Subroutine Cartesian_to_Spherical

       Module Subroutine Init_Molecule(Mol, Natm)
          !---- Argument ----!
          type(Molecule_Type), intent(in out) :: Mol
          integer, optional,   intent(in)  :: Natm
       End Subroutine Init_Molecule

       Module Subroutine Cartesian_to_Zmatrix(Mol, NMol, Cell, Dmin, Dmax)
          !---- Arguments ----!
          type (Molecule_type),           intent(in out) :: Mol
          type (Molecule_type), optional, intent(   out) :: NMol
          Type (Cell_G_Type),   optional, intent(in)     :: Cell
          real(kind=cp),        optional, intent(in)     :: Dmin
          real(kind=cp),        optional, intent(in)     :: Dmax
       End Subroutine Cartesian_to_Zmatrix

       Module Subroutine Create_Connectivity_Cartesian(Mol, Dmin, Dmax)
          !---- Arguments ----!
          type (Molecule_type),          intent(in out):: Mol
          real(kind=cp), optional,       intent(in)    :: Dmin
          real(kind=cp), optional,       intent(in)    :: Dmax
       End Subroutine Create_Connectivity_Cartesian

       Module Subroutine Fix_Orient_Cartesian(Mol, NMol, NAtom_O, NAtom_X, NAtom_XY,Mat)
          !---- Arguments ----!
          type (Molecule_type),                    intent(in out) :: Mol
          type (Molecule_type),          optional, intent(   out) :: NMol
          integer,                       optional, intent(in)     :: NAtom_O
          integer,                       optional, intent(in)     :: NAtom_X
          integer,                       optional, intent(in)     :: NAtom_XY
          real(kind=cp), dimension(3,3), optional, intent(out)    :: Mat
       End Subroutine Fix_Orient_Cartesian

       Module Subroutine Fractional_to_Cartesian(Mol, Cell, NMol)
          !---- Arguments ----!
          type (Molecule_type),           intent(in out) :: Mol
          type (Cell_G_Type),             intent(in    ) :: Cell
          type (Molecule_type), optional, intent(   out) :: NMol
       End Subroutine Fractional_to_Cartesian

       Module Subroutine Fractional_to_Spherical(Mol, Cell, NMol)
          !---- Arguments ----!
          type (Molecule_type),           intent(in out) :: Mol
          type (Cell_G_Type),             intent(in)     :: Cell
          type (Molecule_type), optional, intent(   out) :: NMol
       End Subroutine Fractional_to_Spherical

       Module Subroutine Fractional_to_Zmatrix(Mol,Cell,NMol)
          !---- Arguments ----!
          type (Molecule_type),           intent(in out) :: Mol
          type (Cell_G_Type),             intent(in)     :: Cell
          type (Molecule_type), optional, intent(   out) :: NMol
       End Subroutine Fractional_to_Zmatrix

       Module Subroutine Spherical_to_Cartesian(Mol, NMol)
          !---- Arguments ----!
          type (Molecule_type),           intent(in out) :: Mol
          type (Molecule_type), optional, intent(   out) :: NMol
       End Subroutine Spherical_to_Cartesian

       Module Subroutine Spherical_to_Fractional(Mol, Cell, NMol)
          !---- Arguments ----!
          type (Molecule_type),           intent(in out) :: Mol
          type (Cell_G_Type),             intent(in)     :: Cell
          type (Molecule_type), optional, intent(   out) :: NMol
       End Subroutine Spherical_to_Fractional

       Module Subroutine Spherical_to_Zmatrix(Mol, Cell, NMol)
          !---- Arguments ----!
          type (Molecule_type),           intent(in out) :: Mol
          Type (Cell_G_Type),   optional, intent(in)     :: Cell
          type (Molecule_type), optional, intent(   out) :: NMol
       End Subroutine Spherical_to_Zmatrix

       Module Subroutine Zmatrix_to_Cartesian(Mol, NMol)
          !---- Arguments ----!
          type (Molecule_type),           intent(in out) :: Mol
          type (Molecule_type), optional, intent(   out) :: NMol
       End Subroutine Zmatrix_to_Cartesian

       Module Subroutine Zmatrix_to_Fractional(Mol, Cell, NMol)
          !---- Arguments ----!
          type (Molecule_type),           intent(in out) :: Mol
          type (Cell_G_Type),             intent(in    ) :: Cell
          type (Molecule_type), optional, intent(   out) :: NMol
       End Subroutine Zmatrix_to_Fractional

       Module Subroutine Zmatrix_to_Spherical(Mol, NMol)
          !---- Arguments ----!
          type (Molecule_type),           intent(in out) :: Mol
          type (Molecule_type), optional, intent(   out) :: NMol
       End Subroutine Zmatrix_to_Spherical

       Module Subroutine WriteInfo_Free_Atoms(AtmF, N, Lun)
          !---- Arguments ----!
          class(Atm_type), dimension(:), intent(in) :: AtmF
          integer,                        intent(in) :: N
          integer, optional,              intent(in) :: Lun
       End Subroutine WriteInfo_Free_Atoms

       Module Subroutine WriteInfo_Molecule(Mol, Lun)
          !---- Arguments ----!
          type (Molecule_type), intent(in):: Mol
          integer,optional,     intent(in):: Lun
       End Subroutine WriteInfo_Molecule

       Module Subroutine WriteInfo_Molecular_Crystal(MolCrys, Lun)
          !---- Arguments ----!
          type(MolCrystal_Type), intent(in) :: MolCrys
          integer, optional,     intent(in) :: Lun
       End Subroutine WriteInfo_Molecular_Crystal

       Module Subroutine ReadInfo_Free_Atoms(FileType, AtmF, N, Nl_ini, Nl_end)
          !---- Arguments ----!
          type(File_type),               intent(in)      :: FileType
          class(Atm_Type), dimension(:), intent(in out)  :: AtmF
          integer,                       intent(out)     :: N
          integer, optional,             intent(in)      :: Nl_ini
          integer, optional,             intent(in)      :: Nl_end
       End Subroutine ReadInfo_Free_Atoms

       Module Subroutine ReadInfo_Molecule(FileType, Mol, Nl_ini, Nl_end)
         !---- Arguments ----!
         type(File_type),        intent(in)   :: Filetype
         type (Molecule_type),   intent(out)  :: Mol
         integer, optional,      intent(in)   :: Nl_Ini
         integer, optional,      intent(in)   :: Nl_End
       End Subroutine ReadInfo_Molecule

       Module Subroutine Empiric_Formula_AtList(Atm, Formula, Form_Weight)
          !---- Arguments ----!
          type(AtList_Type),       intent(in)  :: Atm
          character(len=*),        intent(out) :: Formula
          real(kind=cp), optional, intent(out) :: Form_Weight
       End Subroutine Empiric_Formula_AtList

       Module Subroutine Empiric_Formula_Molec(Mol, Formula, Form_Weight)
          !---- Arguments ----!
          type(molecule_type),      intent(in)  :: Mol
          character(len=*),         intent(out) :: Formula
          real(kind=cp), optional,  intent(out) :: Form_Weight
       End Subroutine Empiric_Formula_Molec

       Module Subroutine Empiric_Formula_Molcrys(Molcrys, Formula, Form_Weight)
          !---- Arguments ----!
          type(MolCrystal_type),   intent(in)  :: Molcrys
          character(len=*),        intent(out) :: Formula
          real(kind=cp), optional, intent(out) :: Form_Weight
       End Subroutine Empiric_Formula_Molcrys

       Module Subroutine Init_MolCrystal(MolX, NMol, NAtm, AtmType)
          !---- Argument ----!
          type(MolCrystal_Type),      intent(out) :: MolX
          integer,          optional, intent(in)  :: Nmol       !Molecule object
          integer,          optional, intent(in)  :: NAtm       !Free atoms
          character(len=*), optional, intent(in)  :: Atmtype
       End Subroutine Init_MolCrystal

       Module Subroutine Set_MolReference(Mol, NMol, NAtom_O, NAtom_X, NAtom_XY)
          !---- Arguments ----!
          type (Molecule_type),           intent(in out) :: Mol
          type (Molecule_type), optional, intent(   out) :: NMol
          integer,              optional, intent(in)     :: NAtom_O
          integer,              optional, intent(in)     :: NAtom_X
          integer,              optional, intent(in)     :: NAtom_XY
       End Subroutine Set_MolReference

       Module Subroutine Molec_to_AtList(Mol, Type_Atm, AtList, Coor_Type, Cell)
          !---- Arguments ----!
          type (Molecule_Type),         intent(in)   :: Mol
          character(len=*),             intent(in)   :: Type_Atm
          type (AtList_Type),           intent(out)  :: AtList
          character(len=*),   optional, intent(in)   :: Coor_type
          type (Cell_G_type), optional, intent(in)   :: Cell
       End Subroutine Molec_to_AtList

       Module Subroutine MolCrystal_to_AtList(Molcrys, AtList)
          !---- Arguments ----!
          type (MolCrystal_Type), intent(in)  :: Molcrys
          type (AtList_Type),     intent(out) :: AtList
       End Subroutine MolCrystal_to_AtList

    End Interface


 Contains




 End Module CFML_Molecules
