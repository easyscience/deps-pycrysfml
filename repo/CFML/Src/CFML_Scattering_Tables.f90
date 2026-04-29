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
!!---- MODULE: CFML_Scattering_Chemical_Tables
!!----
!!----   INFO: Tabulated information about atomic chemical and scattering data. A set of fortran
!!----         TYPEs and variables are defined. Tables are declared as allocatable arrays of
!!----         types and they are charged only if the setting (initialising) procedures are called.
!!----         It is convenient in a particular program using this moduled to call the "removing"
!!----         procedures (making a deallocation) to liberate memory after the required information
!!----         is found and stored in user-defined variables.
!!----
!!----
!!
 Module CFML_Scattering_Tables
    !---- Use Modules ----!
    Use CFML_GlobalDeps, only: CP, CFML_debug
    Use CFML_Strings,    only: U_Case, L_Case

    implicit none

    private

    !---- List of public subroutines ----!
    public :: Get_Anomalous_ScFac, Get_Atomic_Mass, Get_Atomic_Vol, Get_Chem_Info, Get_Chem_Symb
    public :: Get_Covalent_radius, Get_Ionic_radius, Get_Fermi_Length, Get_Abs_Xs, Get_Inc_Xs
    public :: Get_Magnetic_Form, Get_Magnetic_J2, Get_Magnetic_J4, Get_Magnetic_J6
    public :: Get_Xray_Form, Get_Xray_Wavelengths, Get_Z_Symb
    public :: Remove_Chem_Info, Remove_Delta_Fp_Fpp, Remove_Magnetic_Form, Remove_Xray_Form
    public :: Set_Chem_Info, Set_Delta_Fp_Fpp, Set_Magnetic_Form, Set_Xray_Form

    !---- Definitions ----!

    !---- Parameters ----!
    integer, parameter, public :: NUM_CHEM_INFO = 108  ! Number of total Chem_info Data
    integer, parameter, public :: NUM_DELTA_FP  = 98   ! Number of total Delta (Fp,Fpp) Data
    integer, parameter, public :: NUM_MAG_FORM  = 119  ! Number of total Magnetic_Form Data
    integer, parameter, public :: NUM_MAG_J2    = 97   ! Number of <j2> Magnetic_Form Data
    integer, parameter, public :: NUM_MAG_J4    = 97   ! Number of <j4> Magnetic_Form Data
    integer, parameter, public :: NUM_MAG_J6    = 39   ! Number of <j6> Magnetic_Form Data
    integer, parameter, public :: NUM_XRAY_FORM = 214  ! Number of total Xray_Form Data

    !---- Type definitions ----!

    !!----
    !!---- TYPE, PUBLIC :: ANOMALOUS_SC_TYPE
    !!--..
    !!---- Update: February - 2005
    !!
    Type, public :: Anomalous_Sc_Type
       character(len= 2)            :: Symb=" "       ! Symbol of the Chemical species
       real(kind=cp), dimension(5)  :: Fp  =0.0_cp    ! Delta Fp
       real(kind=cp), dimension(5)  :: Fpp =0.0_cp    ! Delta Fpp
    End Type Anomalous_Sc_Type

    !!----
    !!---- TYPE, PUBLIC :: CHEM_INFO_TYPE
    !!--..
    !!---- Update: February - 2005
    !!
    Type, public :: Chem_Info_Type
       character(len= 2)          :: Symb  =" "         ! Symbol of the Element
       character(len=12)          :: Name  =" "         ! Name of the Element
       integer                    :: Z     =0           ! Atomic Number
       real(kind=cp)              :: AtWe  =0.0_cp      ! Atomic weight
       real(kind=cp)              :: RCov  =0.0_cp      ! Covalent Radius
       real(kind=cp)              :: RWaals=0.0_cp      ! van der Waals Radius
       real(kind=cp)              :: VAtm  =0.0_cp      ! Atomic volumen
       integer, dimension(5)      :: Oxid  =0           ! Oxidation State
       real(kind=cp), dimension(5):: Rion  =0.0_cp      ! Ionic Radius (depending of the oxidation)
       real(kind=cp)              :: SctF  =0.0_cp      ! Fermi length [10**(-12) cm]
       real(kind=cp)              :: SedInc=0.0_cp      ! Incoherent Scattering Neutron cross-section (barns -> [10**(-24) cm**2] )
       real(kind=cp)              :: Sea   =0.0_cp      ! Neutron Absorption cross-section ( barns, for v= 2200m/s, l(A)=3.95/v (km/s) )
    End Type Chem_Info_Type

    !!----
    !!---- TYPE :: MAGNETIC_FORM_TYPE
    !!--..
    !!---- Update: February - 2005
    !!
    Type, public :: Magnetic_Form_Type
       character(len=4)            :: Symb =" "           ! Symbol of the Chemical species
       real(kind=cp), dimension(7) :: SctM =0.0_cp        ! Scattering Factors
    End Type Magnetic_Form_Type

    !!----
    !!---- TYPE :: XRAY_FORM_TYPE
    !!--..
    !!---- Update: February - 2005
    !!
    Type, public :: Xray_Form_Type
       character(len=4)           :: Symb=" "         ! Symbol of the Chemical species
       integer                    :: Z   =0           ! Atomic Number
       real(kind=cp), dimension(4):: a   =0.0_cp      ! Coefficients for calculating the X-ray scattering factors
       real(kind=cp), dimension(4):: b   =0.0_cp      ! f(s) = Sum_{i=1,4} { a(i) exp(-b(i)*s^2) } + c
       real(kind=cp)              :: c   =0.0_cp      ! s=sinTheta/Lambda
    End Type Xray_Form_Type

    !!----
    !!---- TYPE :: XRAY_WAVELENGTH_TYPE
    !!--..
    !!---- Update: February - 2005
    !!
    Type, public :: Xray_Wavelength_Type
       character(len=2)            :: Symb  =" "        ! Symbol of the Chemical species
       real(kind=cp), dimension(2) :: Kalfa =0.0_cp     ! K-Serie for X-ray
       real(kind=cp)               :: Kbeta =0.0_cp     ! K-Serie for X-ray
    End Type Xray_Wavelength_Type


    !---- Variables ----!
    Type(Anomalous_Sc_Type),  allocatable, dimension(:), public :: Anomalous_ScFac ! Table of Delta-Fp and Delta-Fpp for 5 common radiations
                                                                                   ! Order (Cr, Fe, Cu, Mo, Ag)
    Type(Chem_Info_Type),     allocatable, dimension(:), public :: Chem_Info       ! Tabulated chemical data according to the items specified in the definition of Chem_Info_Type.
    Type(Magnetic_Form_Type), allocatable, dimension(:), public :: Magnetic_Form   ! Tabulated magnetic form factor data
    Type(Magnetic_Form_Type), allocatable, dimension(:), public :: Magnetic_j2     ! Tabulated magnetic form factor J2
    Type(Magnetic_Form_Type), allocatable, dimension(:), public :: Magnetic_j4     ! Tabulated magnetic form factor J4
    Type(Magnetic_Form_Type), allocatable, dimension(:), public :: Magnetic_j6     ! Tabulated magnetic form factor J6
    Type(Xray_Form_Type),     allocatable, dimension(:), public :: Xray_Form       ! Tabulated Xray scattering factor coefficients

    Type(Xray_Wavelength_Type), dimension(7), public :: Xray_Wavelengths =(/                          &  ! Tabulated K-Series for Xray
                                Xray_Wavelength_type("CR",[2.28988,2.29428],2.08480), &
                                Xray_Wavelength_type("FE",[1.93631,1.94043],1.75650), &
                                Xray_Wavelength_type("CU",[1.54059,1.54431],1.39220), &
                                Xray_Wavelength_type("MO",[0.70932,0.71360],0.63225), &
                                Xray_Wavelength_type("AG",[0.55942,0.56380],0.49708), &
                                Xray_Wavelength_type("CO",[1.78919,1.79321],1.62083), &
                                Xray_Wavelength_type("NI",[1.65805,1.66199],1.50017)  /)

    Interface
       Module Function Get_Abs_Xs(Symb) Result(u)
         !> Returns the absorption cross-section (barns, energy = 0.025 eV)
         !> of a chemical element. If an error ocurred, it returns 0.
          character(len=*), intent(in) :: Symb  !> Atomic symbol
          real(kind=cp)                :: u     !> absorption cross-section
       End Function Get_Abs_Xs

       Module Function Get_Anomalous_ScFac(r) Result(info)
         !> Returns the row r from the anomalous_scfac table
         integer,             intent(in) :: r      !> Row
         type(anomalous_sc_type)         :: info   !> anomalous scattering factors
       End Function Get_Anomalous_ScFac

       Module Function Get_Atomic_Mass(Symb) Result(mass)
         !> Returns the atomic mass (amu) of a chemical element.
         !> If an error ocurred, it returns 0.
          character(len=*), intent (in) :: Symb     !> Atomic symbol
          real(kind=cp)                 :: Mass     !> Atomic mass
       End Function Get_Atomic_Mass

       Module Function Get_Atomic_Vol(Symb) Result(vol)
          !> Returns the atomic volume (cm3/mol) of a chemical element.
          !> If an error ocurred, it returns 0.
          character(len=*), intent (in) :: Symb  !> Atomic symbol
          real(kind=cp)                 :: Vol   !> Atomic volume
       End Function Get_Atomic_Vol

       Module Function Get_Chem_Info(r) Result(info)
          !> Returns the row r of the chemical info table.
          integer,             intent(in) :: r    !> Row
          type(chem_info_type)            :: info !> Chemical info
       End Function Get_Chem_Info

       Module Function Get_Chem_Symb(Label) Result(Symb)
          !> Returns the chemical symbol of an element from a label.
          character(len=*),  intent(in) :: Label    !> Label
          character(len=2)              :: Symb     !> Atomic symbol
       End Function Get_Chem_Symb

       Module Function Get_Covalent_Radius(Symb) Result(rad)
          !> Returns the covalent radius (angstroms) of a chemical element.
          !> If an error ocurred, it returns 1.4.
          character(len=*), intent (in) :: Symb !> Atomic symbol
          real(kind=cp)                 :: rad  !> Atomic radius
       End Function Get_Covalent_Radius

       Module Function Get_Fermi_Length(Symb) Result(b)
          !> Returns the Fermi length (10^-12 cm)  of a chemical element.
          !> If an error ocurred, it returns 0.
          character(len=*), intent(in) :: Symb !> Atomic symbol
          real(kind=cp)                :: b    !> Fermi length
       End Function Get_Fermi_Length

       Module Function Get_Inc_Xs(Symb) Result(u)
          !> Returns the incoherent scattering neutron cross section (barns) of a chemical element.
          !> If an error ocurred, it returns 0.
          character(len=*), intent(in) :: Symb !> Atomic symbol
          real(kind=cp)                :: u    !> Incoherent scattering neutron cross-section
       End Function Get_Inc_Xs

       Module Function Get_Ionic_Radius(Symb,Valence) Result(rad)
          !> Returns the ionic radius (angstroms) of an ion.
          !> If an error ocurred, it returns 0.
          character(len=*), intent (in) :: Symb    !> Atomic symbol
          integer,          intent (in) :: valence !> Valence
          real(kind=cp)                 :: rad     !> Ionic radius
       End Function Get_Ionic_Radius

       Module Function Get_Magnetic_Form(r) Result(info)
         !> Returns the row r from the magnetic_form table
         integer,             intent(in) :: r      !> Row
         type(magnetic_form_type)        :: info   !> Magnetic form factors
       End Function Get_Magnetic_Form

       Module Function Get_Magnetic_J2(r) Result(info)
         !> Returns the row r from the magnetic_j2 table
         integer,             intent(in) :: r      !> Row
         type(magnetic_form_type)        :: info   !> Magnetic form factors J2
       End Function Get_Magnetic_J2

       Module Function Get_Magnetic_J4(r) Result(info)
         !> Returns the row r from the magnetic_j2 table
         integer,             intent(in) :: r      !> Row
         type(magnetic_form_type)        :: info   !> Magnetic form factors J4
       End Function Get_Magnetic_J4

       Module Function Get_Magnetic_J6(r) Result(info)
         !> Returns the row r from the magnetic_j2 table
         integer,             intent(in) :: r      !> Row
         type(magnetic_form_type)        :: info   !> Magnetic form factors J6
       End Function Get_Magnetic_J6

       Module Function Get_Z_Symb(Symb) Result(Z)
          !> Returns the atomic number of a chemical element.
          !> If an error ocurred, it returns 0.
          character(len=*),  intent(in):: Symb     !> Atomic label
          integer                      :: Z        !> Atomic number
       End Function Get_Z_Symb

       Module Function Get_Xray_Form(r) Result(info)
          !> Returns the row r from the xray_form table
          integer,             intent(in) :: r      !> Row
          type(xray_form_type)            :: info   !> Xray form factors
       End Function Get_Xray_Form

       Module Function Get_Xray_Wavelengths(r) Result(info)
         !> Returns the row r from the xray_form table
         integer,             intent(in) :: r      !> Row
         type(xray_wavelength_type)      :: info   !> Xray form factors
      End Function Get_Xray_Wavelengths

       Module Subroutine Remove_Chem_Info()
           !> Deallocate Chem_Info table
       End Subroutine Remove_Chem_Info

       Module Subroutine Remove_Delta_Fp_Fpp()
           !> Deallocate Anomalous_ScFac table
       End Subroutine Remove_Delta_Fp_Fpp

       Module Subroutine Remove_Magnetic_Form()
           !> Deallocate Magnetic_Form table
       End Subroutine Remove_Magnetic_Form

       Module Subroutine Remove_Xray_Form()
           !> Deallocate Xray_Form table
       End Subroutine Remove_Xray_Form

       Module Subroutine Set_Chem_Info()
           !> Allocates and loads the table  chem_info(num_chem_info):
           !>
           !>   1: Symbol of the Element
           !>   2: Name of the Element
           !>   3: Atomic Number
           !>   4: Atomic weight
           !>   5: Covalent Radius
           !>   6: Van der Waals Radius
           !>   7: Atomic volumen
           !>   8: Oxidation State (5 states)
           !>   9: Ionic Radius (depending of the oxidation)
           !>  10: Fermi lenght [10**(-12) cm]
           !>  11: Incoherent Scattering Neutron cross-section (barns -> [10**(-24) cm**2] )
           !>  12: Neutron Absorption cross-section ( barns, for v= 2200m/s, l(A)=3.95/v (km/s) )
       End Subroutine Set_Chem_Info

       Module Subroutine Set_Delta_Fp_Fpp()
           !> Wavelenghts:     Cr        Fe        Cu         Mo         Ag
           !>      Lambda   2.28962   1.93597   1.54051    0.70926    0.556363
           !>
           !> Set values for Delta-fp & Delta-fpp for the above wavelengths
       End Subroutine Set_Delta_Fp_Fpp

       Module Subroutine Set_Magnetic_Form()
           !> Set magnetic form factors <j0> P.J. Brown, ILL prep. SP.88BR5016
       End Subroutine Set_Magnetic_Form

       Module Subroutine Set_Xray_Form()
           !> Set Xray_Form Array:
           !>
           !>      1: Symbol of the Element
           !>      2: Name of the Element
           !>      3: a(4)
           !>      4: b(4)
           !>      5: c
           !>  Coefficients for calculating the X-ray scattering factors
           !>      f(s) = Sum_{i=1,4} { a(i) exp(-b(i)*s^2) } + c
           !>  where s=sinTheta/Lambda
       End Subroutine Set_Xray_Form
    End Interface

   Contains

 End Module CFML_Scattering_Tables

