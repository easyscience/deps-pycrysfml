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
!!---- This particular module has been developed by:
!!---- Juan Rodriguez-Carvajal & Marc Janoschek & Oksana Zaharko (OZ)
!!----
!!---- MODULE: CFML_Polarimetry
!!----
!!----   INFO: Subroutines and Functions to calculate the polarisation tensor
!!----   as it will be measured. It uses matrices defined in CFML_Crystal_Metrics in
!!----   order to calculate the polar tensor with respect to the coordinate
!!----   frame defined in the Blume equations (Phys. Rev. Vol. 130 p.1670-1676,
!!----   1963, see also the definitions below in magn_Inter_Vec_PF). As input
!!----   the nuclear structure factor, the magnetic interaction vector with
!!----   respect to the crystal frame and the matrices defined in CFML_Crystal_Metrics
!!----   for the crystal frame are needed.
!!----
!!---- HISTORY
!!----    Updates:June     - 2012: Corrections, Blume Equations in Crystallographic Frame (JRC)
!!----            January  - 2012: General revision of variables introduced by OZ (JRC)
!!----            November - 2011: New subroutines and calculations with domains (OZ)
!!----            April    - 2008:
!!----            December - 2006: Added function Write_Polar_line for more convenient
!!----                             output of matrices of many reflections in one file
!!----            April    - 2005: Created by MJ and revised by JRC
!!----
!!---- DEPENDENCIES
!!--++    Use CFML_GlobalDeps,              only: cp, tpi
!!--++    Use CFML_Metrics,                 only: Set_Crystal_Cell, Cell_G_Type, Cart_Vector
!!--++    Use CFML_Maths,                   only: Cross_Product
!!--++    use CFML_kvec_Structure_Factors,  only: MagHD_Type
!!--++    use CFML_kvec_Symmetry,           only: Magnetic_domain_type
!!----
!!---- VARIABLES
!!----    Polar_Info_Type
!!----    Polar_Matrix_Type
!!----    Polar_Calc_Type
!!----    Polar_Calc_List_Type
!!----    Polar_Calcmulti_List_Type
!!----    Polar_Obs_Type
!!----    Polar_Obs_List_Type
!!----    Polar_Obsmulti_List_Type
!!----    Polar_Calc_Svs_Type
!!----    Polar_Calc_Svs_List_Type
!!----    Polar_Calcmulti_Svs_List_Type

!!----
!!---- PROCEDURES
!!----    Functions:
!!--++       Im_Nm_Y                   [Private]
!!--++       Im_Nm_Z                   [Private]
!!--++       Mag_Y                     [Private]
!!--++       Mag_Z                     [Private]
!!--++       Magn_Inter_Vec_Pf         [Private]
!!--++       Mm                        [Private]
!!--++       Nuc_Contr                 [Private]
!!--++       Real_Nm_Y                 [Private]
!!--++       Real_Nm_Z                 [Private]
!!--++       Tchiral                   [Private]
!!----
!!----    Subroutines:
!!----       Calc_Polar_Dom  (OZ adapted from SET_POLAR_INFO by including domain information, Feb 2009)
!!----       Set_Polar_Info  (Useful only for theoretical monodomains)
!!----       Write_Polar_Info
!!----       Write_Polar_Line
!!----       Calc_Polar_Dom_Efficiency
!!----       Calc_Polar_CrSec
!!----       Calc_Polar
!!----       Get_Pol_Tensor_Pc
!!
 Module CFML_kvec_Polarimetry
    !---- Used External Modules ----!
    Use CFML_GlobalDeps,                 only: cp, tpi, err_CFML, CFML_debug
    Use CFML_Metrics,                    only: Set_Crystal_Cell, Cell_G_Type, Cart_Vector
    Use CFML_Maths,                      only: Cross_Product,Tensor_Product,Mat_Cross,Inverse_Matrix
    use CFML_kvec_Structure_Factors,     only: MagHD_Type
    use CFML_kvec_Symmetry,              only: Magnetic_domain_type
    use CFML_SXTAL_Geom,                 only: Phi_mat,Chi_mat, Psi_mat,Get_Angs_NB

    !---- Variables ----!
    implicit none

    private

    !---- List of public overloaded operators ----!

    !---- List of public functions ----!

    !---- List of public subroutines ----!
    public  :: Calc_Polar_Dom, Set_Polar_Info, Write_Polar_Info, Write_Polar_line, &
               Calc_Polar_Dom_Efficiency, Calc_Polar_CrSec, Calc_Polar, Get_Pol_Tensor_Pc

    !---- List of private Operators ----!

    !---- List of private functions ----!
    private :: Magn_Inter_Vec_PF, Nuc_Contr, Mag_Y, Mag_Z, Real_Nm_Y, Real_Nm_Z, &
               Im_Nm_Y, Im_Nm_Z, Tchiral, Mm

    !---- List of private subroutines ----!


    !---- Definitions ----!

    !!----
    !!---- TYPE :: Polar_calc_type
    !!--..
    !!---- Type, public :: Polar_calc_type
    !!----     real(kind=cp), dimension (3)        :: H     ! Scattering vector in hkl
    !!----     real(kind=cp), dimension (3)        :: SPV   ! Second vector in Scattering plane apart of scattering vector to define plane
    !!----     Type(Cell_G_Type)                   :: Cell  ! Unit Cell of Crystal
    !!----     real(kind=cp)                       :: P     ! magnitude of initial polarisation vector
    !!----     complex(kind=cp), dimension (3,2,24):: MiV   ! magnetic interaction vector
    !!----     complex(kind=cp)                    :: NSF   ! nuclear structure factor
    !!----     real(kind=cp)                       :: NC    ! nuclear scattering contribution
    !!----     real(kind=cp), dimension (2,24)     :: MY    ! magnetic contribution along y
    !!----     real(kind=cp), dimension (2,24)     :: MZ    ! magnetic contribution along z
    !!----     real(kind=cp), dimension (2,24)     :: RY    ! real part of nuclear magnetic interference term along y
    !!----     real(kind=cp), dimension (2,24)     :: RZ    ! real part of nuclear magnetic interference term along z
    !!----     real(kind=cp), dimension (2,24)     :: IY    ! imaginary part of nuclear magnetic interference term along y
    !!----     real(kind=cp), dimension (2,24)     :: IZ    ! imaginary part of nuclear magnetic interference term along y
    !!----     real(kind=cp), dimension (2,24)     :: TC    ! chiral contribution
    !!----     real(kind=cp), dimension (2,24)     :: MM    ! magnetic-magnetic interference term
    !!----     real(kind=cp), dimension (3,2,24)   :: CS    ! the three different elastic cross-sections depending on the direction of the initial polar vector
    !!----     real(kind=cp), dimension (3,3)      :: Pij   ! the polarisation tensor
    !!---- End Type Polar_calc_type
    !!----
    !!---- Update: February 2009 (OZ)
    !!

    !!
    Type, public :: Polar_calc_type
        real(kind=cp), dimension (3)        :: H=0.0
        real(kind=cp), dimension (3)        :: SPV=0.0
        type(Cell_G_Type)                   :: Cell
        real(kind=cp)                       :: P=0.0
        complex(kind=cp), dimension (3,2,24):: MiV=0.0
        complex(kind=cp)                    :: NSF=0.0
        real(kind=cp)                       :: NC=0.0
        real(kind=cp), dimension (2,24)     :: MY=0.0
        real(kind=cp), dimension (2,24)     :: MZ=0.0
        real(kind=cp), dimension (2,24)     :: RY=0.0
        real(kind=cp), dimension (2,24)     :: RZ=0.0
        real(kind=cp), dimension (2,24)     :: IY=0.0
        real(kind=cp), dimension (2,24)     :: IZ=0.0
        real(kind=cp), dimension (2,24)     :: TC=0.0
        real(kind=cp), dimension (2,24)     :: MM=0.0
        real(kind=cp), dimension (3,2,24)   :: CS=0.0
        real(kind=cp), dimension (3,3)      :: Pij=0.0
    End Type Polar_calc_type

    !!----
    !!---- TYPE :: Polar_Calc_List_type
    !!--..
    !!----     integer                                         :: NRef  ! Number of Reflections
    !!----     type(Polar_calc_type),allocatable, dimension(:) :: Polari ! Observed Polarisation tensor for the Reflection List
    !!---- End Type Polar_Calc_List_type
    !!----
    !!---- Update: Februar 2009 OZ
    !!
    Type, public :: Polar_Calc_List_type
       integer                                         :: NRef  ! Number of Reflections
       type(Polar_calc_type),allocatable, dimension(:) :: Polari ! Observed Polarisation tensor for the Reflection List
    End Type Polar_Calc_List_type

    !!
    !!---- TYPE :: Polar_CalcMulti_List_type
    !!
    !!---- Created: February 2012 OZ
    !!
    Type, public :: Polar_CalcMulti_List_type
       integer                                              :: Nset  ! Number of Datasets
       type(Polar_Calc_List_type),allocatable, dimension(:) :: Polarilist ! Calculated Polarisation tensors for NRef
    End Type Polar_CalcMulti_List_type

    !!----
    !!---- TYPE :: POLAR_INFO_TYPE
    !!--..
    !!---- Type, public :: Polar_Info_type
    !!----     real(kind=cp), dimension (3)    :: H     ! Scattering vector in hkl
    !!----     real(kind=cp), dimension (3)    :: SPV   ! Second vector in Scattering plane apart of scattering vector to define plane
    !!----     type(Cell_G_Type)               :: Cell  ! Unit Cell of Crystal
    !!----     real(kind=cp)                   :: P     ! magnitude of initial polarisation vector
    !!----     complex(kind=cp), dimension (3) :: MiV   ! magnetic interaction vector
    !!----     complex(kind=cp)                :: NSF   ! nuclear structure factor
    !!----     real(kind=cp)                   :: NC    ! nuclear scattering contribution
    !!----     real(kind=cp)                   :: MY    ! magnetic contribution along y
    !!----     real(kind=cp)                   :: MZ    ! magnetic contribution along z
    !!----     real(kind=cp)                   :: RY    ! real part of nuclear magnetic interference term along y
    !!----     real(kind=cp)                   :: RZ    ! real part of nuclear magnetic interference term along z
    !!----     real(kind=cp)                   :: IY    ! imaginary part of nuclear magnetic interference term along y
    !!----     real(kind=cp)                   :: IZ    ! imaginary part of nuclear magnetic interference term along y
    !!----     real(kind=cp)                   :: TC    ! chiral contribution
    !!----     real(kind=cp)                   :: MM    ! magnetic-magnetic interference term
    !!----     real(kind=cp), dimension (3)    :: CS    ! the three different elastic cross-sections depending on the direction of the initial polar vector
    !!----     real(kind=cp), dimension (3,3)  :: Pij   ! the polarisation tensor
    !!---- End Type Polar_Info_type
    !!----
    !!---- Update: April 2008
    !!
    Type, public :: Polar_Info_type
       real(kind=cp), dimension (3)     :: H
       real(kind=cp), dimension (3)     :: SPV
       type(Cell_G_Type)                :: Cell
       real(kind=cp)                    :: P
       complex(kind=cp), dimension (3)  :: MiV
       complex(kind=cp)                 :: NSF
       real(kind=cp)                    :: NC
       real(kind=cp)                    :: MY
       real(kind=cp)                    :: MZ
       real(kind=cp)                    :: RY
       real(kind=cp)                    :: RZ
       real(kind=cp)                    :: IY
       real(kind=cp)                    :: IZ
       real(kind=cp)                    :: TC
       real(kind=cp)                    :: MM
       real(kind=cp), dimension (3)     :: CS
       real(kind=cp), dimension (3,3)   :: PIJ
    End Type Polar_Info_type

    !!----
    !!---- TYPE :: Polar_obs_type
    !!--..
    !!----     real(kind=cp), dimension(3)  :: H      ! H +/- k
    !!----     real(kind=cp), dimension(3)  :: SPV
    !!----     real(kind=cp)                :: Pin
    !!----     real(kind=cp), dimension(3,3):: oPij   ! the observed polarisation tensor
    !!----     real(kind=cp), dimension(3,3):: soPij  ! the Sigma of polarisation tensor
    !!----     real(kind=cp), dimension(3,3):: woPij  ! the weight 1/Sigma**2
    !!---- End Type Polar_obs_type
    !!----
    !!---- Updated: February 2012 OZ
    !!
    Type, public :: Polar_obs_type
       Real(Kind=Cp), Dimension(3)  :: H
       Real(Kind=Cp), Dimension(3)  :: SPV
       Real(Kind=Cp)                :: P
       Real(Kind=Cp), Dimension(3,3):: oPij
       Real(Kind=Cp), Dimension(3,3):: soPij
       Real(Kind=Cp), Dimension(3,3):: woPij
    End Type Polar_obs_type

    !!----
    !!---- TYPE :: Polar_Obs_List_type
    !!--..
    !!----     integer                                         :: NRef   ! Number of Reflections
    !!----     type(Polar_obs_type),allocatable, dimension(:)  :: Polaro ! Observed Polarisation tensor for the Reflection List
    !!---- End Type Polar_Obs_List_type
    !!----
    !!---- Update: Februar 2009 OZ
    !!
    Type, public :: Polar_Obs_List_type
       integer                                         :: NRef
       type(Polar_obs_type),allocatable, dimension(:)  :: Polaro
    End Type Polar_Obs_List_type

    !!
    !!---- TYPE :: Polar_ObsMulti_List_type
    !!
    !!---- Created: November 2011 OZ
    !!
    Type, public :: Polar_ObsMulti_List_type
       integer                                              :: Nset       ! Number of Datasets
       type(Polar_Obs_List_type),allocatable, dimension(:)  :: Polarolist ! Observed Polarisation tensor for the Reflection List
    End Type Polar_ObsMulti_List_type

    !!
    !!---- TYPE :: Polar_Calc_sVs_type
    !!
    !!---- Created: November 2011 OZ
    !!
    Type, public :: Polar_Calc_sVs_type
        real(kind=cp), dimension (3)  :: H     ! Scattering vector in hkl
        real(kind=cp), dimension (3)  :: SPV   ! Second vector in Scattering plane
        Type(Cell_G_Type)             :: Cell  ! Unit Cell of Crystal
        real(kind=cp)                 :: P     ! Polarisation
        real(kind=cp), dimension (3,3):: Pij   ! Calculated Polarisation tensor
    End Type Polar_Calc_sVs_type

    !!----
    !!---- TYPE :: Polar_Calc_sVs_List_type
    !!
    !!---- Created: November 2011 OZ
    !!
    Type, public :: Polar_Calc_sVs_List_type
       integer                                             :: NRef      ! Number of Reflections
       type(Polar_Calc_sVs_type),allocatable, dimension(:) :: PolarisVs ! Calculated Polarisation tensor for the Reflection List
    End Type Polar_Calc_sVs_List_type

    !!
    !!---- TYPE :: Polar_CalcMulti_sVs_List_type
    !!
    !!---- Created: November 2011 OZ
    !!
    Type, public :: Polar_CalcMulti_sVs_List_type
       integer                                                  :: Nset          ! Number of Datasets
       type(Polar_Calc_sVs_List_type),allocatable, dimension(:) :: PolarisVslist ! Calculated Polarisation tensors for NRef
    End Type Polar_CalcMulti_sVs_List_type

 Interface

    !-------------------!
    !---- Functions ----!
    !-------------------!

    Module Function Im_Nm_Y(Nsf, MiV_Pf,B_Q) Result(I_Nm_Y)
       !---- Argument ----!
       complex(kind=cp),               intent( in)  :: NSF
       complex(kind=cp), dimension(3), intent( in)  :: MiV_PF
       character(len=*),optional,      intent(in)   :: B_Q
       real(kind=cp)                                :: I_Nm_Y
    End Function  Im_Nm_Y

    Module Function Im_Nm_Z(Nsf, MiV_Pf,B_Q) Result(I_Nm_Z)
       !---- Argument ----!
       complex(kind=cp),              intent(in) :: NSF
       complex(kind=cp), dimension(3),intent(in) :: MiV_PF
       character(len=*),optional,     intent(in) :: B_Q
       real(kind=cp)                             :: I_NM_Z
    End Function  Im_Nm_Z

    Module Function Mag_Y(MiV_Pf) Result(My)
       !---- Argument ----!
       complex(kind=cp), dimension(3), intent( in)  :: MiV_PF
       real(kind=cp)                                :: MY
    End Function  Mag_Y

    Module Function Mag_Z(MiV_Pf) Result(Mz)
       !---- Argument ----!
       complex(kind=cp), dimension(3), intent( in) :: MiV_PF
       real(kind=cp)                               :: MZ
    End Function  Mag_Z

    Module Function Magn_Inter_Vec_Pf(MiVC,H,Spv, Cell) Result(MiV_Pf)
       !---- Argument ----!
       complex(kind=cp), dimension(3), intent(in) :: MiVC   !Must be provided in Crystallographic
       real(kind=cp),    dimension(3), intent(in) :: H      !Cartesian frame
       real(kind=cp),    dimension(3), intent(in) :: SPV
       Type (Cell_G_Type),             intent(in) :: Cell
       complex(kind=cp), dimension(3)             :: MiV_PF
    End Function  Magn_Inter_Vec_Pf

    Module Function Mm(MiV_PF) Result(Mmc)
       !---- Argument ----!
       complex(kind=cp), dimension(3), intent( in) :: MiV_PF
       real(kind=cp)                               :: MMC
    End Function  Mm

    Module Function Nuc_Contr(Nsf) Result(Nsc)
       !---- Argument ----!
       complex(kind=cp), intent( in)   :: NSF
       real(kind=cp)                   :: NSC
    End Function  Nuc_Contr

    Module Function Real_Nm_Y(Nsf, MiV_Pf) Result(R_Nm_Y)
       !---- Argument ----!
       complex(kind=cp),               intent(in)  :: NSF
       complex(kind=cp), dimension(3), intent(in)  :: MiV_PF
       real(kind=cp)                               :: R_Nm_Y
    End Function  Real_Nm_Y

    Module Function Real_Nm_Z(Nsf, MiV_Pf) Result(R_Nm_Z)
       !---- Argument ----!
       complex(kind=cp),               intent( in) :: NSF
       complex(kind=cp), dimension(3), intent( in) :: MiV_PF
       real(kind=cp)                               :: R_Nm_Z
    End Function  Real_Nm_Z

    Module Function Tchiral(MiV_Pf,B_Q) Result(Tc)
       !---- Argument ----!
       complex(kind=cp), dimension(3), intent(in) :: MiV_PF
       character(len=*),optional,      intent(in) :: B_Q
       real(kind=cp)                              :: TC
    End Function  Tchiral

    !---------------------!
    !---- Subroutines ----!
    !---------------------!

    Module Subroutine Calc_Polar(frame,wave,Cell,UB, Pin, NSF, Mag_dom, Mh, Pf,ok,mess,B_Q)
       !---- Arguments ----!
       character(len=*),             intent(in)    :: frame
       real(kind=cp),                intent(in)    :: wave
       type (Cell_G_Type),           intent(in)    :: Cell
       Real(kind=cp), dimension(3,3),intent(in)    :: UB
       Real(kind=cp), dimension(3),  intent(in)    :: Pin
       complex(kind=cp),             intent(in)    :: NSF
       type(Magnetic_Domain_type),   intent(in)    :: Mag_Dom
       type(MagHD_Type),             intent(in out):: Mh
       Real(kind=cp), dimension(3),  intent(   out):: Pf
       logical,                      intent(   out):: ok
       Character(len=*),             intent(   out):: mess
       Character(len=*), optional,   intent(in)    :: B_Q
    End Subroutine Calc_Polar

    Module Subroutine Get_Pol_Tensor_Pc(frame,wave,Cell,UB,Pin, NSF, Mag_dom, Mh, Pol_tens, Pc,ok,mess,B_Q)
       character(len=*),             intent(in)    :: frame
       real(kind=cp),                intent(in)    :: wave
       type (Cell_G_Type),           intent(in)    :: Cell
       Real(kind=cp), dimension(3,3),intent(in)    :: UB
       Real(kind=cp), dimension(3),  intent(in)    :: Pin
       complex(kind=cp),             intent(in)    :: NSF
       type(Magnetic_Domain_type),   intent(in)    :: Mag_Dom
       type(MagHD_Type),             intent(in)    :: Mh
       real(kind=cp), dimension(3,3),intent(out)   :: Pol_tens
       real(kind=cp), dimension(3),  intent(out)   :: Pc
       logical,                      intent(   out):: ok
       Character(len=*),             intent(   out):: mess
       Character(len=*), optional,   intent(in)    :: B_Q  !Original Blume equations are used Q=Q_BM
    End Subroutine Get_Pol_Tensor_Pc

    Module Subroutine Calc_Polar_Dom(Cell, H, SPV, Pin, NSF, Mag_dom, Mh, Polari,ok,mess,B_Q)
       ! ---- Arguments ---- !
       type (Cell_G_Type),          intent(in)       :: Cell
       real(kind=cp), dimension (3),intent(in)       :: H
       real(kind=cp), dimension(3), intent(in)       :: SPV
       real(kind=cp),               intent(in)       :: Pin
       complex(kind=cp),            intent(in)       :: NSF
       type(Magnetic_Domain_type),  intent(in)       :: Mag_Dom
       type(MagHD_Type),            intent(in out)   :: Mh
       type(Polar_calc_type),       intent(out)      :: Polari
       logical,                     intent(out)      :: ok
       character(len=*),            intent(out)      :: mess
       character(len=*), optional,  intent(in)       :: B_Q
    End Subroutine Calc_Polar_Dom

    Module Subroutine Set_Polar_Info(Cell, H, Spv, Pin, Nsf, MiV, Polari,B_Q)
       !---- Arguments ----!
       Type (Cell_G_Type),             intent(in)  :: Cell
       real(kind=cp), DIMENSION (3),   intent(in)  :: H
       real(kind=cp), dimension(3),    intent(in)  :: SPV
       real(kind=cp),                  intent(in)  :: Pin
       complex(kind=cp),               intent(in)  :: NSF
       complex(kind=cp), dimension(3), intent(in)  :: MiV
       Type (Polar_Info_type),         intent(out) :: Polari
       character(len=*), optional,     intent(in)  :: B_Q
    End Subroutine Set_Polar_Info

    Module Subroutine Write_Polar_Info(Polari, Mag_Dom, Lun, info)
       !---- Arguments ----!
       type(Polar_calc_type),     intent(in)  :: Polari
       type(Magnetic_Domain_type),intent(in)  :: Mag_Dom
       integer,         optional, intent(in)  :: Lun
       character(len=*),optional, intent(in)  :: info
    End Subroutine Write_Polar_Info

    Module Subroutine Write_Polar_Line(Polari, Lun)
       !---- Arguments ----!
       Type (Polar_calc_type), intent( in)     :: Polari !
       integer, optional,      intent(in)      :: Lun
    End Subroutine Write_Polar_line

    Module Subroutine Calc_Polar_Dom_Efficiency(Cell,H,SPV,Pin,NSF,Mag_dom,Mh,Polari)
      Type (Cell_G_Type),          intent(in)    :: Cell
      Real(kind=cp), dimension (3),intent(in)    :: H
      Real(kind=cp), dimension(3), intent(in)    :: SPV
      Real(kind=cp),               intent(in)    :: Pin
      complex(kind=cp),            intent(in)    :: NSF
      Type(Magnetic_Domain_type),  intent(in)    :: Mag_Dom
      Type(MagHD_Type),            intent(in out):: Mh
      Type(Polar_calc_type),       intent(out)   :: Polari
    End Subroutine Calc_Polar_Dom_Efficiency

    Module Subroutine Calc_Polar_CrSec(Cell,H,SPV,Pin,NSF,Mag_dom,Mh,Ipp,Ipm,Imp,Imm)
      Type (Cell_G_Type),           intent(in)     :: Cell
      Real(Kind=Cp), dimension (3), intent(in)     :: H
      Real(kind=cp), dimension(3),  intent(in)     :: SPV
      Real(kind=cp),                intent(in)     :: Pin
      complex(kind=cp),             intent(in)     :: NSF
      Type(Magnetic_Domain_type),   intent(in)     :: Mag_Dom
      Type(MagHD_Type),             intent(in out) :: Mh
      Real(kind=cp), dimension(3,3),intent(out)    :: Ipp,Ipm,Imp,Imm
    End Subroutine Calc_Polar_CrSec

  End interface

 End Module CFML_kvec_Polarimetry
