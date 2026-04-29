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
!!---- MODULE: CFML_Geometry_SXTAL
!!----   INFO: Module for making geometrical calculations in single crystal instruments.
!!----         All subroutines gathered in this module have been taken from packages
!!----         at ILL maintained informally by different people. When the original authors
!!----         have made some comments in the header of the routines these have been kept.
!!----
!!---- HISTORY
!!----    Update: 06/03/2011
!!----
!!--..
!!--..    The original routines have been transformed to Fortran 95 and the names
!!--..    have been changed to indicate more clearly the purpose.
!!--..
!!--..    Code converted using MODIF90 by Juan Rodriguez-Carvajal
!!--..    from manually modified files produced by a pre-treatment with the
!!--..    program To_F90, by Alan Miller.
!!--..    Date: 2005-03-06  Time: 22:07:48
!!--..
!!--..    The persons that, to my knowledge, have contributed to maintain these procedures
!!--..    in some way are: R.F.D Stansfield, A. Barthelemy, Garry McIntyre, John Allibon,
!!--..    S.A. Mason, M.Thomas and Alain Filhol
!!--..
!!--..    Juan Rodriguez-Carvajal
!!--..
!!--..    The module is under test, I'm entirely responsible for all the errors and bugs.
!!--..
!!--..
!!--..    Some words about notation and conventions
!!--..
!!--..    A reflection in r.l.u. units is represented by a real vector h
!!--..    The same vector referred to the Cartesian laboratory system when all
!!--..    motor angles are set to zero is noted as z1, so that [z1]= [UB] [h]
!!--..    where [] denotes a column matrices for 3-D vectors.
!!--..
!!--..    The rotational matrices [PHI], [CHI], [OMG], [PSI] are calculated
!!--..    according to conventions in Busing & Levy
!!--..    After applying a [PHI] matrix to vector [z1] we obtain [z2] ... and so on
!!--..     [z2] = [PHI] [z1]
!!--..     [z3] = [CHI] [z2]
!!--..     [z4] = [OMG] [z3]
!!--..
!!--..
!!--..
!!--..    The default instrument cartesian frame is that defined by
!!--..    W.R.Busing & H.A.Levy (Acta Cryst. 22,457-464 (1967)
!!--..    The reciprocal Cartesian Frame used by Busing-Levy has "x" along a*
!!--..    "y" in the (a*,b*) plane and "z" perpendicular to that plane. The
!!--..    matrix passing from (h,k,l) in r.l.u. to the cartesian frame is
!!--..           | a*   b*.cos(gamm*)   c*.cos(beta*)            |
!!--..    [B] =  | 0    b*.sin(gamm*)  -c*.sin(beta*).cos(alpha) |
!!--..           ! 0          0          1/c                      |
!!--..
!!--..        [hc] = [B] [h]
!!--..
!!--..    Let U be the orthogonal matrix relating the phi-axis system
!!--..    attached to the phi-shaft of the instrument with the Cartesian crystal system
!!--..    then     [h-phi]= [U] [hc]      [h-phi] = [U] [B] [h] = [UB] [h]  --> z1
!!--..
!!--..    Let us define three more cartesian systems attached to the Chi,Omega and
!!--..    Theta axes, coincident with phi-axis when all instrument angles are set
!!--..    to zero :
!!--..       [h-chi] = [PHI] [h-phi]     --> z2
!!--..       [h-omg] = [CHI] [h-chi]     --> z3
!!--..       [h-tet] = [OMG] [h-omg]     --> z4
!!--..
!!--..             |  cos(phi)      sin(phi)      0  |             |zL-axis
!!--..     [PHI] = | -sin(phi)      cos(phi)      0  |             |
!!--..             |     0            0           1  |             |
!!--..                                           incoming beam ->__|_________yL-axis
!!--..             |  cos(chi)       0     sin(chi)  |            /
!!--..     [CHI] = |     0           1       0       |           /
!!--..             | -sin(chi)       0     cos(chi)  |          /xL-axis
!!--..
!!--..             |  cos(omg)      sin(omg)      0  |
!!--..     [OMG] = | -sin(omg)      cos(omg)      0  |
!!--..             |     0            0           1  |
!!--..
!!--..    Let us define 2 more Cartesian frames: The laboratory fixed system
!!--..    and the 2Theta-axis system attached to the 2theta shaft. All coincide
!!--..    for all angles equal to zero.
!!--..     [h-Lab] = [THE]  [h-tet] = [N] [h-omg]
!!--..     [h-tte] = [THE]t [h-tet] = [M] [h-omg]
!!--..    Where:
!!--..             |  cos(the)      sin(the)      0  |
!!--..     [THE] = | -sin(the)      cos(the)      0  |
!!--..             |     0            0           1  |
!!--..
!!--..                         |  cos(nu)      sin(nu)      0  |
!!--..     [N] = [THE] [OMG] = | -sin(nu)      cos(nu)      0  |  with nu=omg+tet
!!--..                         |     0            0         1  |
!!--..
!!--..                          |  cos(mu)      sin(mu)      0  |
!!--..     [M] = [THE]t [OMG] = | -sin(mu)      cos(mu)      0  |  with mu=omg-tet
!!--..                          |     0            0         1  |
!!--..    All angles, except Chi, are left-handed rotations about their respective axes.
!!--..
!!--..    In this module the origin of the Omega angle is different from that of BL.
!!--..    The Omega angle is always evaluated from the YL laboratory axis instead of
!!--..    taking the origin from the YTheta axis. This means that the bisecting geometry
!!--..    condition is Omega=Theta instead of Omega=0. This makes a change in some BL
!!--..    equations to which we have to add the value of Theta.
!!--..
!!--..    Basic Diffractometer equations:
!!--..          q = |h| = sqrt(dot_product(hc,hc)), with [hc]=[B][h]
!!--..    Bragg equation: sin(theta) = Lambda.q/2
!!--..    The reflection [h] is in the diffraction position if
!!--..         [h-tet] = [OMG] [CHI] [PHI] [U] [B] [h]
!!--..    has the form [h-tet]t = (q, 0, 0)
!!--..
!!--..    The orientation matrix U can be obtained from two non-colinear reflections h1 & h2,
!!--..    provided the cell parameters are known
!!--..
!!--..    Two kind of Busing-Levy (BL) frames are possible:
!!--..    For both frames the origin is at sample position and the y-axis
!!--..    positive sense is along the secondary beam (from monochromator to sample)
!!--..    a)  z upward
!!--..    b)  z downward
!!--..    The x-axis makes a right-handed frame with the other axes. Positive 2theta
!!--..    angles are from y-axis towards x-axis
!!--..    A change of geometry is always possible by providing the components of the
!!--..    axes {e1,e2,e3} with respect to the standard {i,j,k} BL-frame
!!--..
!!--..    igeom=1: Bisecting (PSI=0)
!!--..    igeom=2: Bisecting - HiCHI
!!--..    igeom=3: Normal beam (Chi=-90)
!!--..    igeom=4: Parallel    (Chi=90, PSI=90)
!!--..
!!----
!!---- DEPENDENCIES
!!----
!!----
!!---- VARIABLES
!!--..    Types
!!----    PSD_VAL_TYPE
!!----    SXD_VAL_TYPE
!!--++    PSD_SET            [Private]
!!----    SXD
!!----    TWIN_TYPE
!!----
!!---- PROCEDURES
!!----    Functions:
!!----
!!----    Subroutines:
!!----       ANGS_4C_BISECTING
!!----       CALANG
!!----       CALC_OM_CHI_PHI
!!----       CALC_PSI
!!----       CELL_FR_UB
!!----       CHI_MAT
!!----       PSD_CONVERT
!!----       D19PSD
!!----       DSPACE
!!----       EQUATORIAL_CHI_PHI
!!----       FIXDNU
!!----       FLAT_CONE_VERTDET
!!----       GANU_FROM_XZ
!!----       GENB
!!----       GENUB
!!----       GET_ANGS_NB
!!----       GET_DSPACING_THETA
!!----       GET_GAOMNU_FRCHIPHI
!!----       GET_UB_FROM_HKL_HKL_OMEGA
!!----       GET_UB_FROM_UVW_HKL_OMEGA
!!----       GET_WAVEGANU_FRZ4
!!----       GET_Z1_D9ANGLS
!!----       GET_Z1_FROM_PIXEL
!!----       NORMAL
!!----       NORMAL_BEAM_ANGLES
!!----       PHI_MAT
!!----       PSI_MAT
!!----       READ_TWINLAW
!!----       REFVEC
!!----       S4CNB
!!----       SET_PSD
!!----       SNB4C
!!----       SXDPSD
!!----       TRIPLE
!!----       WRITE_TWINLAW
!!----       Z1FRFC
!!----       Z1FRMD
!!----       Z1FRNB
!!----       Z1FRZ2
!!----       Z1FRZ3
!!----       Z1FRZ4
!!----       Z2FRZ1
!!----       Z3FRZ1
!!----       Z4FRGN
!!----       Z4FRZ1
!!----
!!
 Module CFML_SXTAL_Geom
    !---- Use Modules ----!
    Use CFML_GlobalDeps,        Only: Cp,Dp,To_Deg,To_Rad, clear_error, Err_CFML,ops_sep, CFML_debug
    use CFML_gSpacegroups,      Only: Spg_Type,Set_Spacegroup
    Use CFML_Maths,             Only: Cross_Product,set_eps_math, invert => Inverse_Matrix,co_linear,in_limits,sort
    Use CFML_Metrics,           Only: Cell_G_Type, Zone_Axis_type, Get_basis_from_uvw, &
                                      Rot_Gibbs_Matrix,Set_Crystal_Cell
    Use CFML_Geom,              Only: Get_OmegaChiPhi, Get_Matrix_moving_v_to_u,&
                                      Get_Anglen_Axis_From_Rotmat, Set_Rotation_Matrix
    Use CFML_ILL_Instrm_data,   Only: Current_Orient, Current_Instrm, SXTAL_Numor_type, &
                                      Diffractometer_Type
    use CFML_Reflections,       Only: Reflist_Type,Hkl_Gen_Sxtal
    Use CFML_Strings,           Only: file_type,l_case

    !---- Variables ----!
    Implicit None

    Private

    !---- List of public functions ----!
    Public :: Chkin180

    !---- List of public subroutines ----!
    Public :: Angs_4C_bisecting, Equatorial_Chi_Phi, Get_dspacing_theta,                          &
              Get_GaOmNu_frChiPhi, Chi_mat, Phi_mat, Psi_mat, Get_Angs_NB,                        &
              Calc_Om_Chi_Phi, Calc_Psi, d19psd, dspace, fixdnu, Normal_Beam_Angles,              &
              s4cnb, snb4c, Flat_Cone_vertDet, ganu_from_xz, Get_WaveGaNu_frZ4, normal, refvec,   &
              sxdpsd, triple, ubfrqcel, z3frz1, z2frz1, z1frfc, z1frnb, z1frmd, z1frz4, z1frz3,   &
              z1frz2, z4frgn, z4frz1, calang, genb, genub, cell_fr_UB, set_psd, get_z1_from_pixel,&
              Get_z1_D9angls, psd_convert, Get_UB_from_uvw_hkl_omega, Get_UB_from_hkl_hkl_omega,  &
              Get_FlatCone_Angles_D10, Read_Twinlaw, Write_Twinlaw, psd_convert_old,invert_ub_dp


    !---- Definitions ----!

    !!----
    !!---- TYPE :: PSD_VAL_TYPE
    !!--..
    !!---- Type, public :: Psd_Val_Type       !Set of caracteristic parameters of the PSD
    !!----    Character(len=12) :: name_inst  ! Name of the instrument
    !!----    real(kind=cp)     :: xoff       !
    !!----    real(kind=cp)     :: zoff       !
    !!----    real(kind=cp)     :: radius     !
    !!----    real(kind=cp)     :: yoff       !
    !!----    real(kind=cp)     :: cgap       !
    !!----    real(kind=cp)     :: agap       !
    !!----    Integer           :: ncat       !
    !!----    Integer           :: nano       !
    !!----    Integer           :: ipsd       !
    !!---- End Type Psd_Val_Type
    !!----
    !!---- Update: July - 2010
    !!
    Type, Public :: Psd_Val_Type
       Character(len=12) :: name_inst ! Name of the instrument
       real(kind=cp)     :: xoff
       real(kind=cp)     :: zoff
       real(kind=cp)     :: radius
       real(kind=cp)     :: yoff
       real(kind=cp)     :: cgap
       real(kind=cp)     :: agap
       Integer           :: ncat
       Integer           :: nano
       Integer           :: ipsd
    End Type Psd_Val_Type

    !!----
    !!---- PSD
    !!----    Type(Psd_Val_Type), Public :: psd
    !!----
    !!----
    !!---- Update: January - 2009
    !!
    Type(Psd_Val_Type), Public :: psd

    !!----
    !!---- TYPE :: Sxd_Val_Type
    !!--..
    !!---- Type, public :: Sxd_Val_Type        !Set of caracteristic parameters of the diffractometr SXD (old?) at ISIS
    !!----    real(kind=cp)      :: distms         !Not tested! (used only in the subroutine sxdpsd)
    !!----    real(kind=cp)      :: distsd         !
    !!----    real(kind=cp)      :: dimx           !
    !!----    real(kind=cp)      :: dimz           !
    !!----    real(kind=cp)      :: xoff           !
    !!----    real(kind=cp)      :: yoff           !
    !!----    real(kind=cp)      :: zoff           !
    !!----    real(kind=cp)      :: toff           !
    !!----    real(kind=cp)      :: velcon         !
    !!----    Integer            :: nxcel          !
    !!----    Integer            :: nzcel          !
    !!---- End Type Sxd_Val_Type
    !!----
    !!---- Update: April - 2008
    !!
    Type, Public  :: Sxd_Val_Type
       real(kind=cp)      :: distms
       real(kind=cp)      :: distsd
       real(kind=cp)      :: dimx
       real(kind=cp)      :: dimz
       real(kind=cp)      :: xoff
       real(kind=cp)      :: yoff
       real(kind=cp)      :: zoff
       real(kind=cp)      :: toff
       real(kind=cp)      :: velcon
       Integer            :: nxcel
       Integer            :: nzcel
    End Type Sxd_Val_Type

    !!--++
    !!--++ PSD_SET
    !!--++    logical, private :: psd_set
    !!--++
    !!--++    ...No comments!!!
    !!--++
    !!--++ Update: April - 2008
    !!
    logical, private :: psd_set=.false.

    !!----
    !!---- SXD
    !!----    Type(Sxd_Val_Type), Public :: sxd
    !!----
    !!----    ...No comments!!!
    !!----
    !!---- Update: April - 2008
    !!
    Type(Sxd_Val_Type), Public :: sxd

    !!----   Type, public :: twin_type
    !!----     character(len=80)                :: Twin_name      !
    !!----     integer                          :: ityp           ! type of twin
    !!----     integer                          :: n_twins        ! number of variants
    !!----     real(kind=cp), dimension(3,3,48) :: Twin_Mat       !
    !!----     real(kind=cp), dimension(3,3,48) :: Twin_Matinv    !
    !!----     real(kind=cp), dimension(3,48)   :: Twin_axis      !
    !!----     real(kind=cp), dimension(48)     :: Twin_ang       !
    !!----   End Type twin_type
    Type, public :: twin_type
      character(len=80)                :: Twin_name
      integer                          :: ityp
      integer                          :: n_twins
      real(kind=cp), dimension(3,3,48) :: Twin_Mat
      real(kind=cp), dimension(3,3,48) :: Twin_Matinv
      real(kind=cp), dimension(3,48)   :: Twin_axis
      real(kind=cp), dimension(48)     :: Twin_ang
    End Type twin_type

    interface Get_z1_from_pixel
      module procedure Get_z1_from_pixel_num
      module procedure Get_z1_from_pixel_ang
    end interface

    Interface


      !Submodule: SXTAL_Matx_Zvect

      Module Function Chi_mat(chi) Result(dum)
         !---- Arguments ----!
         real(kind=cp), Intent(In)      :: chi
         real(kind=cp), Dimension(3,3)  :: dum
      End Function Chi_mat

      Module Function Phi_mat(phi) Result(dum)
         !---- Arguments ----!
         real(kind=cp), Intent(In)    :: phi
         real(kind=cp), Dimension(3,3):: dum
      End Function Phi_mat

      Module Function Psi_mat(psi) Result(dum)
         !---- Arguments ----!
         real(kind=cp), Intent(In)     :: psi
         real(kind=cp), Dimension(3,3) :: dum
      End Function Psi_mat

      Module Function Get_z1_D9angls(wave,ttheta,om,ch,ph) Result(z1) !Warning, this function should be verified
         !---- Arguments ----!                                        !It looks like as for a four circle instrument
         real(kind=cp), intent (in)  :: wave,ttheta,om,ch,ph
         real(kind=cp), dimension(3) :: z1
      End Function Get_z1_D9angls

      Module Function z1frfc(wave,tth,om,ch,ph) Result(z1)
         !---- Argument ----!
         real(kind=cp), Intent(In)   :: wave,tth,om,ch,ph
         real(kind=cp), Dimension(3) :: z1
      End Function z1frfc

      Module Function  z1frmd(wave,ch,ph,ga,om,nu) Result(z1)
         !---- Argument ----!
         real(kind=cp), Intent(In)    :: wave,ch,ph,ga,om,nu
         real(kind=cp), Dimension(3)  :: z1
      End Function z1frmd

      Module Function z1frnb(wave,ga,om,nu) Result(z1)
         !---- Arguments ----!
         real(kind=cp), Intent(In)     :: wave,ga,om,nu
         real(kind=cp),  Dimension(3)  :: z1
      End Function z1frnb

      Module Function z1frz2(z2,ph) Result(z1)
         !---- Arguments ----!
         real(kind=cp), Intent(In ), Dimension(3)  :: z2
         real(kind=cp), Intent(In )                :: ph
         real(kind=cp),              Dimension(3)  :: z1
      End Function z1frz2

      Module Function z1frz3(z3,ch,ph) Result(z1)
         !---- Arguments ----!
         real(kind=cp), Intent(In), Dimension(3)   :: z3
         real(kind=cp), Intent(In)                 :: ch,ph
         real(kind=cp),              Dimension(3)  :: z1
      End Function z1frz3

      Module Function z1frz4(z4,om,ch,ph) Result(z1)
         !---- Arguments ----!
         real(kind=cp), Intent(In), Dimension(3)   :: z4
         real(kind=cp), Intent(In)                 :: om,ch,ph
         real(kind=cp),             Dimension(3)   :: z1
      End Function z1frz4

      Module Function z2frz1(z1,ph) Result(z2)
         !---- Arguments ----!
         real(kind=cp), Intent(In),Dimension(3)    :: z1
         real(kind=cp), Intent(In)                 :: ph
         real(kind=cp),            Dimension(3)    :: z2
      End Function z2frz1

      Module Function z3frz1(z1,ch,ph)  Result(z3)
         !---- Arguments ----!
         real(kind=cp), Intent(In), Dimension(3)    :: z1
         real(kind=cp), Intent(In)                  :: ch,ph
         real(kind=cp),             Dimension(3)    :: z3
      End Function z3frz1

      Module Function z4frgn(wave,ga,nu) Result(z4)
         !---- Arguments ----!
         real(kind=cp), Intent(In)    :: wave
         real(kind=cp), Intent(In)    :: ga,nu
         real(kind=cp), Dimension(3)  :: z4
      End Function z4frgn

      Module Function z4frz1(z1,om,ch,ph) Result(z4)
         !---- Arguments ----!
         real(kind=cp), Intent(In), Dimension(3)    :: z1
         real(kind=cp), Intent(In)                  :: om,ch,ph
         real(kind=cp),             Dimension(3)    :: z4
      End Function z4frz1

      module function invert_UB_dp(A) result(Ainv)
         !---- Arguments ----!
         real(kind=dp) :: A(3,3)
         real(kind=dp) :: Ainv(3,3)
      end function invert_UB_dp

      ! End Submodule: SXTAL_Matx_Zvect

      ! SubModule (CFML_Geometry_SXTAL) SXTAL_FlatCone

      Module Subroutine Calc_Om_Chi_Phi(vhkl,vlab1,psi,ub,om,ch,ph) !Flat cone
         !---- Arguments ----!
         real(kind=cp), Intent(In),Dimension(3)     :: vhkl,vlab1
         real(kind=cp), Intent(In)                  :: psi
         real(kind=cp), Intent(In),Dimension(3,3)   :: ub
         real(kind=cp), Intent(In Out)              :: om,ch,ph
      End Subroutine Calc_Om_Chi_Phi

      Module Subroutine Flat_Cone_vertDet(wave,z1,ub,vrho,rho,ch,ph,ga,om,nu)
         !---- Arguments ----!
         real(kind=cp), Intent(In)                      :: wave
         real(kind=cp), Intent(In),     Dimension(3)    :: z1
         real(kind=cp), Intent(In),   Dimension(3,3)    :: ub
         real(kind=cp), Intent(In Out), Dimension(3)    :: vrho
         real(kind=cp), Intent(Out)                     :: rho,ch,ph,ga,om,nu
      End Subroutine Flat_Cone_vertDet

      Module Subroutine Get_FlatCone_Angles_D10(z,mu_fc,psi1,psi2,n_ang,limits,psi,omega,chi,phi,inlim,Rot_base)
        real(kind=cp), dimension(3),             intent(in)  :: z
        real(kind=cp),                           intent(in)  :: mu_fc,psi1,psi2
        integer,                                 intent(in)  :: n_ang
        real(kind=cp), dimension(:,:),           intent(in)  :: limits
        real(kind=cp), dimension(:),             intent(out) :: psi,omega,chi,phi
        logical,       dimension(:),             intent(out) :: inlim
        real(kind=cp), dimension(3,3), optional, intent(out) :: Rot_base
      End Subroutine Get_FlatCone_Angles_D10

      ! End SubModule (CFML_Geometry_SXTAL) SXTAL_FlatCone

      ! SubModule (CFML_Geometry_SXTAL) SXTAL_UB

      Module Subroutine cell_fr_UB(ub,ipr,dcel,rcel)
         !---- Arguments ----!
         real(kind=cp),Dimension(3,3),         Intent(In)  :: ub
         Integer, optional,                    Intent(In)  :: ipr
         real(kind=cp),Dimension(6), optional, Intent(out) :: dcel,rcel
      End Subroutine cell_fr_UB

      Module Function genb(c) Result (b)
         !---- Arguments ----!
         Type(Cell_G_Type), Intent(In)  :: c
         real(kind=cp), Dimension(3,3)  :: b
      End Function genb

      Module Function GenUB(b,h1,h2,h1o,h2o) Result (ub)
         !---- Arguments ----!
         real(kind=cp), Dimension(3,3), Intent(In)  :: B        !Busing-Levy B-matrix
         real(kind=cp), Dimension(3),   Intent(In)  :: h1,h2    !Miller indices
         real(kind=cp), Dimension(3),   Intent(In)  :: h1o,h2o  !Components in Lab system
         real(kind=cp), Dimension(3,3)              :: UB       !Busing-Levy UB-matrix
      End Function GenUB

      Module Function Get_UB_from_hkl_hkl_omega(wave,Cell,h1,h2,omega) Result(UB)
        real(kind=cp),                 intent(in)    :: wave
        type (Cell_G_Type),            intent(in)    :: Cell
        real(kind=cp), dimension(3),   intent(in)    :: h1
        real(kind=cp), dimension(3),   intent(in)    :: h2
        real(kind=cp),                 intent(in)    :: omega
        real(kind=cp), dimension(3,3)                :: UB
      End Function Get_UB_from_hkl_hkl_omega

      Module Function Get_UB_from_uvw_hkl_omega(wave,Cell,Zone_Axis,h1,omega) Result(UB)
        real(kind=cp),                 intent(in)    :: wave
        type (Cell_G_Type),            intent(in)    :: Cell
        Type (Zone_Axis_type),         intent(in out):: Zone_Axis
        real(kind=cp), dimension(3),   intent(in)    :: h1
        real(kind=cp),                 intent(in)    :: omega
        real(kind=cp), dimension(3,3)                :: UB
      End Function Get_UB_from_uvw_hkl_omega

      Module Subroutine normal(v)
         !---- Argument ----!
         real(kind=cp), Intent(In Out), Dimension(3)   :: v
      End Subroutine normal

      Module Subroutine refvec(vhkl,ub,vs,vz)
         !---- Arguments ----!
         real(kind=cp), Intent(In),  Dimension(3)    :: vhkl
         real(kind=cp), Intent(In),  Dimension(3,3)  :: ub
         real(kind=cp), Intent(Out), Dimension(3)    :: vs,vz
      End Subroutine refvec

      Module Subroutine triple(v1,v2,tv)
         !---- Arguments ----!
         real(kind=cp),    Intent(In Out), Dimension(3)  :: v1,v2  !they come back normalized and V2 perp. to V1
         real(kind=cp),    Intent(Out),    Dimension(3,3):: tv
      End Subroutine triple

      Module Subroutine ubfrqcel(spg_id,q_user,cell_user,ub,rfac,nq_max_user,npairs_max_user,nubs_max_user,angle_min_user,rtol_user,rfac_max_user,output_file)
         !---- Arguments ----!
         character(len=*),                       intent(in)  :: spg_id          ! space group (number, symbol...)
         real,    dimension(:,:),                intent(in)  :: q_user          ! scattering vectors (3,nq)
         real,    dimension(6),                  intent(in)  :: cell_user       ! cell parameters
         real,    dimension(:,:,:), allocatable, intent(out) :: ub              ! returned ubs
         real,    dimension(:),     allocatable, intent(out) :: rfac            ! rfactors for returned ubs
         integer, optional,                      intent(in)  :: nq_max_user     ! maximum number of q-vectors
         integer, optional,                      intent(in)  :: npairs_max_user ! maximum number of pairs to be tested
         integer, optional,                      intent(in)  :: nubs_max_user   ! maximum number of ubs returned
         real,    optional,                      intent(in)  :: angle_min_user  ! minimum angle between pairs of reflections
         real,    optional,                      intent(in)  :: rtol_user       ! tolerance in reciprocal space
         real,    optional,                      intent(in)  :: rfac_max_user   ! maximum allowed value for R-factor
         character(len=*), optional,             intent(in)  :: output_file     ! full path of the output file
      End Subroutine ubfrqcel
      ! End SubModule (CFML_Geometry_SXTAL) SXTAL_UB

      ! SubModule (CFML_Geometry_SXTAL) SXTAL_IO

      Module Subroutine Read_Twinlaw(Twin,read_ok,lun,fich_cfl)
        Type(twin_type),   intent (out)          :: Twin
        Logical,           intent (out)          :: read_ok
        integer,           intent (in), optional :: lun !logical unit of the file to be read
        Type (file_type),  intent (in), optional :: fich_cfl
      End Subroutine Read_Twinlaw

      Module Subroutine Write_Twinlaw(Twin,lun,cell)
        Type(twin_type),                   intent(in out) :: Twin
        integer,                           intent(in)     :: lun !logical unit of the file to be written
        type(Cell_G_Type),       optional, intent(in)     :: cell
      End Subroutine Write_Twinlaw


      ! End SubModule (CFML_Geometry_SXTAL) SXTAL_IO

      ! SubModule (CFML_Geometry_SXTAL) SXTAL_Angles

      Elemental Module Function chkin180(angle) result(angle_in)
        real(kind=cp), intent(in) :: angle
        real(kind=cp)             :: angle_in
      End Function chkin180

      Module Subroutine Angs_4c_bisecting(wave,z1,tth,om,ch,ph)
         !---- Arguments ----!
         real(kind=cp), Intent(In)                :: wave
         real(kind=cp), Intent(In), Dimension(3)  :: z1
         real(kind=cp), Intent(Out)               :: tth,om,ch,ph
      End Subroutine Angs_4c_bisecting

      Module Subroutine calang(h,tteta,om,ch,ph,wav,ubm,geom)
         !---- Arguments ----!
         real(kind=cp),Dimension(3),             Intent(In) :: h
         real(kind=cp),                          Intent(Out):: tteta,om,ch,ph
         real(kind=cp),                optional, intent(in) :: wav
         real(kind=cp), dimension(3,3),optional, intent(in) :: ubm
         integer,                      optional, intent(in) :: geom
      End Subroutine calang

      Module Function Calc_Psi(vhkl,vlab1,om,ch,ph,ub) Result(psi)
         !---- Arguments ----!
         real(kind=cp), Intent(In),Dimension(3)   :: vhkl,vlab1
         real(kind=cp), Intent(In)                :: om,ch,ph
         real(kind=cp), Intent(In),Dimension(3,3) :: ub
         real(kind=cp)                            :: psi
      End Function Calc_Psi

      Module Subroutine dspace(wave,vhkl,cell,ds,th)
         !---- Arguments ----!
         real(kind=cp), Intent(In)               :: wave
         real(kind=cp), Intent(In),Dimension(3)  :: vhkl
         real(kind=cp), Intent(In),Dimension(6)  :: cell
         real(kind=cp), Intent(Out)              :: ds,th
      End Subroutine dspace

      Module Subroutine Equatorial_Chi_Phi(z1,ch,ph)
         !---- Arguments ----!
         real(kind=cp), Intent(In), Dimension(3)  :: z1
         real(kind=cp), Intent(Out)               :: ch,ph
      End Subroutine Equatorial_Chi_Phi

      Module Subroutine fixdnu(wave,z1,nu,ch,ph,ga,om)
         !---- Arguments ----!
         real(kind=cp), Intent(In)               :: wave
         real(kind=cp), Intent(In), Dimension(3) :: z1
         real(kind=cp), Intent(In)               :: nu
         real(kind=cp), Intent(Out)              :: ch,ph,ga,om
      End Subroutine fixdnu

      Module Subroutine Get_Angs_NB(wave,z1,ga,om,nu)
         !---- Arguments ----!
         real(kind=cp), Intent(In)                      :: wave
         real(kind=cp), Intent(In),Dimension(3)         :: z1
         real(kind=cp), Intent(Out)                     :: ga,om,nu
      End Subroutine Get_Angs_NB

      Module Subroutine Get_dspacing_theta(wave,z1,ds,th)
         !---- Arguments ----!
         real(kind=cp), Intent(In)                 :: wave
         real(kind=cp), Intent(In), Dimension(3)   :: z1
         real(kind=cp), Intent(Out)                :: ds,th
      End Subroutine Get_dspacing_theta

      Module Subroutine Get_GaOmNu_frChiPhi(wave,z1,ch,ph,ga,om,nu)
         !---- Arguments ----!
         real(kind=cp), Intent(In)                :: wave
         real(kind=cp), Intent(In), Dimension(3)  :: z1
         real(kind=cp), Intent(In)                :: ch,ph
         real(kind=cp), Intent(Out)               :: ga,om,nu
      End Subroutine Get_GaOmNu_frChiPhi

      Module Subroutine Get_WaveGaNu_frZ4(z4,wave,ga,nu)
         !---- Arguments ----!
         real(kind=cp), Intent(In), Dimension(3)  :: z4
         real(kind=cp), Intent(Out)               :: wave,ga,nu
      End Subroutine Get_WaveGaNu_frZ4

      Module Subroutine Normal_Beam_Angles(wav,ub,h,sig,anbcal,ier,zer,nusign)
         !---- Arguments ----!
         real(kind=cp),                 Intent(In)          :: wav
         real(kind=cp), dimension(3,3), Intent(In)          :: ub
         real(kind=cp), dimension(3),   Intent(In)          :: h
         Integer,                       Intent(In Out)      :: sig
         real(kind=cp), dimension(:),   Intent(Out)         :: anbcal
         Integer,                       Intent(Out)         :: ier
         real(kind=cp), dimension(3),   Intent(In),optional :: zer
         integer,                       Intent(In),optional :: nusign
      End Subroutine Normal_Beam_Angles

      Module Function s4cnb(angl_4C) Result(angl_NB)
         !---- Arguments ----!
         real(kind=cp), dimension(4), Intent(In) :: angl_4C
         real(kind=cp), dimension(3)             :: angl_NB
      End Function s4cnb

      Module Function snb4c(angl_NB) Result(angl_4C)
         !---- Arguments ----!
         real(kind=cp), dimension(3), Intent(In )  :: angl_NB
         real(kind=cp), dimension(4)               :: angl_4C
      End Function snb4c

      Module Subroutine ganu_from_xz(px,pz,ga_D,nu_D,ipsd,npix,pisi,dist_samp_detector,det_offsets,origin,ga_P,nu_P,f_virtual)
         !---- Arguments ----!
         real,                  intent(in)  :: px                 ! x coordinate, in pixels
         real,                  intent(in)  :: pz                 ! z coordinate, in pixels
         real,                  intent(in)  :: ga_D               ! gamma angle of the center of the detector, in degrees
         real,                  intent(in)  :: nu_D               ! nu    angle of the center of the detector, in degrees
         integer,               intent(in)  :: ipsd               ! detector type
         integer, dimension(2), intent(in)  :: npix               ! number of horizontal and vertical pixels
         real,    dimension(2), intent(in)  :: pisi               ! horizontal and vertical pixel sizes
         real,                  intent(in)  :: dist_samp_detector ! sample detector distance
         real,    dimension(3), intent(in)  :: det_offsets        ! x, y and z detector offsets
         integer,               intent(in)  :: origin             ! origin for numbering pixels
         real,                  intent(out) :: ga_P               ! Gamma value of the reflection
         real,                  intent(out) :: nu_P               ! Nu value of the reflection
         integer, optional,     intent(in)  :: f_virtual          ! stretching factor for virtual detectors
      End Subroutine ganu_from_xz

      Module Subroutine psd_convert(diffractometer,f_virtual,conversion_type,ga_D,nu_D,px,pz,x_D,z_D,ga_P,nu_P,Shifts,origin)
         !---- Arguments ----!
        type(diffractometer_type), intent(in out) :: diffractometer
        integer,                   intent(in)     :: f_virtual
        integer,                   intent(in)     :: conversion_type
        real(kind=cp),             intent(in)     :: ga_D
        real(kind=cp),             intent(in)     :: nu_D
        real(kind=cp),             intent(in out) :: px ! pixel x
        real(kind=cp),             intent(in out) :: pz ! pixel z
        real(kind=cp),             intent(in out) :: x_D
        real(kind=cp),             intent(in out) :: z_D
        real(kind=cp),             intent(in out) :: ga_P
        real(kind=cp),             intent(in out) :: nu_P
        logical, optional,         intent(in)     :: Shifts
        integer, optional,         intent(in)     :: origin
      End Subroutine psd_convert

      Module Subroutine psd_convert_old(mpsd,gamm,gamp,nup,xobs,zobs,cath,anod)
         !---- Arguments ----!
         Integer,          Intent(In)      :: mpsd
         real(kind=cp),    Intent(In)      :: gamm
         real(kind=cp),    Intent(In Out)  :: gamp
         real(kind=cp),    Intent(In Out)  :: nup
         real(kind=cp),    Intent(Out)     :: xobs
         real(kind=cp),    Intent(Out)     :: zobs
         real(kind=cp),    Intent(in Out)  :: cath
         real(kind=cp),    Intent(in Out)  :: anod
      End Subroutine psd_convert_old

      Module Subroutine d19psd(mpsd,ga,nu,cath,anod)
         !---- Arguments ----!
         Integer, Intent(In)                :: mpsd
         real(kind=cp), Intent(In Out)      :: ga
         real(kind=cp), Intent(In Out)      :: nu
         real(kind=cp), Intent(In Out)      :: cath
         real(kind=cp), Intent(In Out)      :: anod
      End Subroutine d19psd

      Module Function Get_z1_from_pixel_num(npx,npz,ifr,snum) Result(z1)
         !---- Arguments ----!
         integer,                intent(in)  :: npx,npz,ifr
         type(SXTAL_Numor_type), intent(in)  :: snum
         real(kind=cp), dimension(3)         :: z1
      End Function Get_z1_from_pixel_num

      Module Function Get_z1_from_pixel_ang(npx,npz,wave,gamm,omem,chim,phim) Result(z1)
         !---- Arguments ----!
         integer,         intent(in)  :: npx,npz
         real(kind=cp),   intent(in)  :: wave,gamm,omem,chim,phim
         real(kind=cp), dimension(3)  :: z1
      End Function Get_z1_from_pixel_ang

      Module Subroutine Set_PSD(dist,cg,ag,nh,nv,ip)
         real(kind=cp), optional, intent(in) :: dist,cg,ag
         integer,       optional, intent(in) :: nh,nv,ip
      End Subroutine Set_PSD

      Module Subroutine sxdpsd(mpsd,gamm,wave,nup,gamp,xobs,zobs, xcel,tim,zcel)
         !---- Arguments ----!
         Integer, Intent(In)                   :: mpsd
         real(kind=cp),    Intent(In)          :: gamm
         real(kind=cp),    Intent(In Out)      :: wave,nup,gamp
         real(kind=cp),    Intent(Out)         :: xobs,zobs, xcel,tim,zcel
      End Subroutine sxdpsd

      ! End SubModule (CFML_Geometry_SXTAL) SXTAL_PSD

    End Interface

 End Module CFML_SXTAL_Geom
