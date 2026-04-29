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
!!----                          Universita di Pavia, Pavia, ITALY
!!----
!!---- Authors: Juan Rodriguez-Carvajal (ILL) (CrysFML)
!!----          Javier Gonzalez-Platas  (ULL) (CrysFML)
!!----          Ross John Angel         (Pavia)   (EoS)
!!----
!!---- Contributors: Laurent Chapon     (ILL)
!!----               Marc Janoschek     (Los Alamos National Laboratory, USA)
!!----               Oksana Zaharko     (Paul Scherrer Institute, Switzerland)
!!----               Tierry Roisnel     (CDIFX,Rennes France)
!!----               Eric Pellegrini    (ILL)
!!----               Nebil Ayape Katcho (ILL)
!!----               Ross John Angel    (IGG-CNR, Italy)
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
!!---- MODULE: CFML_EoS
!!----   INFO:
!!----
!!---- HISTORY
!!----    Update: 16/03/2017
!!----
!!----    ...   2013: First version of the Module
!!----    23/02/2013: Incorporates bug fix to get_volume (JGP)
!!----    ...   2014: Validation for non-thermal EOS and modifications (RJA)
!!----    ...   2015: Addition of code to handle Landau-type phase transitions (RJA)
!!----    ...   2015: Modifications for 2003 compatibility and Eos development (RJA,  JGP)
!!----    ...   2016: Modifications for curved phase boundaries and fitting moduli (RJA)
!!----    ...   2016: Modifications to add PVT table to EoS structure as alternative to EoS parameters (RJA)
!!----    ...   2017: Split write_eoscal to improve error handling, new thermalP eos (RJA)
!!----    ...   2018: New routines: physical_check, get_kp, added pscale,vscale,lscale to data_list_type (RJA)
!!----    ...   2019: Bug fixes, addition of new thermal-pressure EoS types (RJA)
!!----    ...   2020: Jan: E%LinearDir to label linear EoS (RJA)
!!----    ...   2020: March: Addition of extra oscillators for thermal pressure, addition of scales block to eos%params (RJA)
!!----    ...   2020: Sept: Moved enquiry routines for groups into this module (RJA)
!!----    ...   2020: Sept: Moved calculation and management routines for eos of cells into this module (RJA)
!!----    ...   2021: Introduction of Kumar PV EoS, Einstein oscillator, multi oscillator and q-compromise thermal-pressure EoS (RJA)
!!----    ...   2021: Thermal pressure from Cv table in EoS file (RJA)
!!----    ...   2021: Improvements to handle EoS from PTVtables (RJA)
!!
Module CFML_EoS
   !---- Use Modules ----!
   Use CFML_GlobalDeps,only: CP, PI, TO_RAD, err_cfml, clear_error, set_error
   Use CFML_Maths,     only: Debye, First_Derivative, Second_Derivative, Spline_interpol, Diagonalize_SH, &
                             Orient_Eigenvectors, Locate
   Use CFML_Metrics,   only: Cell_G_Type, Strain_Tensor_Type, Get_Cryst_Family, Set_Crystal_Cell, Cell_Type, &
                             Volume_from_Cell,SigmaV_From_Cell, Fix_Tensor, Calc_Paxes_Angles
   Use CFML_Strings,   only: u_case, string_real, string_numstd, number_lines, get_words, get_numstd, &
                             get_separator_pos, get_num, reading_lines, read_key_str

   !---- Definitions ----!
   implicit none

   private

   !---- Public procedures ----!
   public :: Alpha_Cal,       &
             Deriv_Partial_P, Deriv_Partial_P_Analytic, Deriv_Partial_P_Numeric, Deriv_Partial_P_Scales, Dkdt_Cal, &
             EoS_Cal, EoS_Cal_Esd, Eos_Cal_Text, &
             Get_Alpha_Cell, Get_Angle_Deriv, Get_Cp, Get_Cv, Get_DebyeT, Get_GPT, Get_Grun_PT, Get_Grun_Th, &
             Get_Grun_V, Get_K, Get_Kp, Get_Mod_Axis, Get_Mod_Cell, Get_Modp_Axis, Get_Modp_Cell, Get_Params_Cell, &
             Get_Pressure, Get_Pressure_Esd, Get_Pressure_X, Get_Property_X, &
             Get_Props_General, Get_Props_Third, Get_Temperature, Get_Temperature_P0, Get_Transition_Pressure, &
             Get_Transition_Strain, Get_Transition_Temperature, Get_Volume, Get_Volume_Axis, Get_Volume_Cell, &
             Get_Volume_S, &
             Isotropic_Cell,  &
             K_Cal, Kp_Cal, Kpp_Cal, &
             Linear_EoS_Allowed, &
             Pressure_F, Principal_Eos, PscaleMGD, PThermal, &
             Set_XdataTypes, Strain, Strain_EOS, &
             Thermal_Pressure_Eos, Transition_phase, &
             VscaleMGD

   public :: Allocate_EoS_Data_List, Allocate_EoS_List, &
             Calc_Conlev, Check_Scales, Copy_EoS_Data_List, &
             Deallocate_EoS_Data_List, Deallocate_EoS_List, Def_Crystal_System, &
             Eos_Cell_Loaded_Check, EosParams_Check, &
             FfCal_Dat, FfCal_Dat_Esd, FfCal_EoS, &
             Get_Tensor_EoS, &
             Init_EoS_Angles, Init_Eos_Cell_Type, Init_EoS_Cross, Init_EoS_Data_Type, Init_EoS_GroupScales, &
             Init_EoS_Osc, Init_Eos_Shear, Init_Eos_Thermal, Init_EoS_Transition, Init_EoS_Type, &
             Physical_Check, &
             Read_EoS_DataFile, Read_EoS_File, Read_Multiple_EoS_File, &
             Set_Cell_Types, Set_EoS_Implied_Values, Set_Eos_Names, Set_Eos_Use, &
             Write_Data_Conlev, Write_EoS_DataFile, Write_EoS_File, Write_Eoscal, Write_Eoscal_Header, &
             Write_Info_Conlev, Write_Info_EoS, Write_Info_Eos_Cell_Type



   !--------------------!
   !---- PARAMETERS ----!
   !--------------------!
   integer, public, parameter :: NCOL_DATA_MAX=22   ! Defines the maximum number of columns allowed in input data files

   integer, public, parameter :: N_EOSPAR=59        ! Specify the maximum number of Eos parameters allowed in Eos_type data type
   integer, public, parameter :: N_ANGPOLY=3        ! Dimension of polynomial for angles

   integer, public, parameter :: N_PRESS_MODELS=7   ! Number of possible pressure models
   integer, public, parameter :: N_THERM_MODELS=9   ! Number of possible Thermal models
   integer, public, parameter :: N_TRANS_MODELS=3   ! Number of possible Transition models
   integer, public, parameter :: N_SHEAR_MODELS=1   ! Number of possible Shear models
   integer, public, parameter :: N_CROSS_MODELS=2   ! Number of possible Cross-term models
   integer, public, parameter :: N_OSC_MODELS=2     ! Number of possible extra oscillator model types.
   integer, public, parameter :: N_ANGLE_MODELS=1   ! Number of possible angle polynomial models
   integer, public, parameter :: N_DATA_TYPES=2     ! Number of possible data types in addition to V (Kt,Ks etc)


   character(len=*), public, parameter, dimension(-1:N_PRESS_MODELS) :: PMODEL_NAMES=[     &      ! Name of the Pressure Models
                                                                        'PTV Table      ', &
                                                                        'None           ', &
                                                                        'Murnaghan      ', &
                                                                        'Birch-Murnaghan', &
                                                                        'Vinet          ', &
                                                                        'Natural Strain ', &
                                                                        'Tait           ', &
                                                                        'APL            ', &
                                                                        'Kumar          ']

   character(len=*), public, parameter, dimension(-1:N_THERM_MODELS) :: TMODEL_NAMES=[         &  ! Name of the Thermal Models
                                                                        'PTV Table          ', &
                                                                        'None               ', &
                                                                        'Berman 1988        ', &
                                                                        'Fei 1995           ', &
                                                                        'Modified HP1998    ', &
                                                                        'Kroll              ', &
                                                                        'Salje low-T        ', &
                                                                        'HP Thermal Pressure', &
                                                                        'Mie-Gruneisen-Debye', &
                                                                        'Einstein Oscillator', &
                                                                        'ThermalP Cv Table  ']

   character(len=*), public, parameter, dimension(-1:N_TRANS_MODELS) :: TRANMODEL_NAMES=[  &      ! Name of Transition models
                                                                        'PTV Table      ', &
                                                                        'None           ', &
                                                                        'Landau P only  ', &
                                                                        'Landau T only  ', &
                                                                        'Landau PVT     ']

   character(len=*), public, parameter, dimension(0:N_SHEAR_MODELS) :: SHEARMODEL_NAMES=[  &      ! Name of Shear models
                                                                       'None           ',  &
                                                                       'Polynomial     ']

   character(len=*), public, parameter, dimension(0:N_CROSS_MODELS) :: CROSSMODEL_NAMES=[  &      ! Name of Cross-term models
                                                                       'None           ',  &
                                                                       'Linear dK/dT   ',  &
                                                                       'Hellfrich-Conn ']

   character(len=*), public, parameter, dimension(0:N_OSC_MODELS)   :: OSCMODEL_NAMES=[    &      ! Name of extra oscillator types
                                                                       'None           ',  &
                                                                       'Debye          ',  &
                                                                       'Einstein       ']

   character(len=*), public, parameter, dimension(0:N_DATA_TYPES) :: DATATYPE_NAMES=[       &     ! Name of Data types
                                                                     'Cell parameters    ', &
                                                                     'Isothermal moduli  ', &
                                                                     'Adiabatic moduli   ']

   character(len=*), public, parameter, dimension(4:21) :: DATA_NAMES=[         &    !Names of data variables in ic_dat in EoS_Data_List_Type
                                                                       'T    ', &
                                                                       'SIGT ', &
                                                                       'P    ', &
                                                                       'SIGP ', &
                                                                       'V    ', &
                                                                       'SIGV ', &
                                                                       'A    ', &
                                                                       'SIGA ', &
                                                                       'B    ', &
                                                                       'SIGB ', &
                                                                       'C    ', &
                                                                       'SIGC ', &
                                                                       'ALPHA', &
                                                                       'SIGAL', &
                                                                       'BETA ', &
                                                                       'SIGBE', &
                                                                       'GAMMA', &
                                                                       'SIGGA']

   character(len=*), public, parameter, dimension(1:7) :: CELLLABEL =  ['a    ',  &
                                                                         'b    ', &
                                                                         'c    ', &
                                                                         'alpha', &
                                                                         'beta ', &
                                                                         'gamma', &
                                                                         'Vol  ']   !labels for unit-cell parameters

   character(len=*), public, parameter, dimension(0:6) :: AXISLABEL =  ['V   ',  &
                                                                         'a   ', &
                                                                         'b   ', &
                                                                         'c   ', &
                                                                         'd100', &
                                                                         'd010', &
                                                                         'd001']  !labels for axis eos

   real(kind=cp), public, parameter               :: AFERMIGAS    = 2337.0                                 ! Fermi Gas constant in GPa/A^5
   real(kind=cp), public, parameter, dimension(6) :: DELCHI       =[ 2.30, 4.61, 6.17, 9.21,11.80,18.40] ! Delta Chi2 values
   real(kind=cp), public, parameter, dimension(6) :: DELCHI_LEVELS=[68.30,90.00,95.40,99.00,99.73,99.99] ! Confidence Levels

   !---------------!
   !---- TYPES ----!
   !---------------!

   !!----
   !!----  TYPE :: PVT_Table
   !!--..
   !!---- Update: 23/09/2016
   !!
   Type, public :: PVT_Table
      integer                                      :: np  =0        ! number of pressure lines
      integer                                      :: nt  =0        ! number of temperature columns
      real(kind=cp)                                :: pmin=0.0      ! smallest pressure
      real(kind=cp)                                :: pmax=1.0E6    ! biggest pressure
      real(kind=cp)                                :: tmin=0.0      ! smallest temperature
      real(kind=cp)                                :: tmax=1.0E6    ! biggest temperature
      real(kind=cp), allocatable, dimension(:,:,:) :: ptv           ! The table, last index is 1=p, 2=t, 3=v
   End Type PVT_Table

   !!----
   !!----  TYPE :: EOS_TYPE
   !!--..
   !!---- Update: 23/09/2016
   !!
   Type, public :: EoS_Type
      character(len=80)                         :: Title=" "             ! Descriptive title of EoS, set by user
      character(len=20)                         :: System=" "            ! Crystal system name
      character(len=15)                         :: Model=" "             ! Murnaghan, Birch-Murnaghan, Vinet, Natural-Strain
      character(len=20)                         :: TModel=" "            ! Name for thermal model
      character(len=15)                         :: TranModel=" "         ! Name for phase transition model
      character(len=15)                         :: SModel=" "            ! Name for shear model
      character(len=15)                         :: CModel=" "            ! Name for cross-terms model
      character(len=25),dimension(2)            :: OscModel=" "          ! Names for oscillator models
      character(len=5), dimension(N_EOSPAR)     :: ParName=" "           ! Names of the Eos variables...init
      character(len=50), dimension(N_EOSPAR)    :: Comment=" "           ! Description of the Eos variables inclduing units...init
      character(len=15)                         :: Pscale_name=" "       ! Description of the Pressure scale. Only used for output
      character(len=15)                         :: Vscale_name=" "       ! Description of the Volume scale. Only used for output
      character(len=120), dimension(20)         :: doc=" "               ! Documentation for eos: set by user
      character(len=120)                        :: savedate=" "          ! Documentation on last save
      character(len=32)                         :: LinearDir=" "         ! Label for linear direction; default blank
      integer                                   :: IModel=0              ! Index for Model
      integer                                   :: IOrder=0              ! Order for the Model
      logical                                   :: Linear=.false.        ! Flag for Linear EoS not volume
      integer                                   :: ITherm=0              ! Index for thermal expansion model, =0 for none
      integer                                   :: ITran=0               ! Index for phase transition model, =0 for none
      integer                                   :: IShear=0              ! Index for shear model, =0 for none
      integer                                   :: ICross=0              ! Index for P-T cross-terms model, =0 for none or Pth
      integer                                   :: IAngle=0              ! Index for angle polynomial and not eos
      integer,dimension(2)                      :: IOsc=0                ! Index for extra oscillator models
      integer, dimension(N_EOSPAR)              :: Iuse=0                ! Flags for parameters allowed for a given EoS =0 (not), =1 (refineable), =2 (settable) =3 implied, not settable
      real(kind=cp)                             :: PRef=0.0              ! Pressure of Reference
      real(kind=cp)                             :: TRef=298.0            ! Temperature of Reference
      real(kind=cp)                             :: Stoich=0.0            ! Stocihometry factor for multiple-phase calculations
      real(kind=cp)                             :: Density0=0.0          ! Density at reference conditions
      logical                                   :: TRef_fixed=.false.    ! If true, then Tref cannot be changed by user
      logical                                   :: Pthermaleos=.false.   ! Indicates a Pthermal model if .true.
      logical                                   :: Osc_allowed=.false.   ! Indicates if extra oscillators can be used with current thermal model
      logical, dimension(2:4)                   :: allowed_orders        ! Indicates which orders (.true.) are allowed for the PV Eos
      real(kind=cp), dimension(N_EOSPAR)        :: Params=0.0            ! EoS Parameters
      real(kind=cp), dimension(N_EOSPAR)        :: Esd=0.0               ! Sigma EoS Parameters
      real(kind=cp)                             :: X=0.0                 ! Spare intensive variable, after P,T
      real(kind=cp)                             :: WChi2=0.0             ! weighted chi-squared for the refined parameters...set by ls
      real(kind=cp)                             :: DelPMax=0.0           ! Maximum misfit in P between obs and calc P...set by ls
      integer, dimension(4)                     :: IWt=0                 ! Choice for weighting in LS: order is P,T,V,X, 1 = use sigmas for weights
      integer, dimension(N_EOSPAR)              :: IRef=0                ! Refinement switches
      real(kind=cp),dimension(N_EOSPAR)         :: Factor=1.0            ! Scale factor to multiply variables for output (and divide on input)
      real(kind=cp)                             :: AlphaFactor=1.0E5_cp  ! Scale factor to multiply values of alpha (not parameters) for output
      real(kind=cp),dimension(N_EOSPAR)         :: Lastshift=0.0         ! Shift applied in last LS cycle to parameters
      real(kind=cp),dimension(N_EOSPAR,N_EOSPAR):: VCV=0.0               ! Var-Covar matrix from refinement
      real(kind=cp),dimension(3,0:3,N_ANGPOLY)  :: angpoly=0.0           ! Polynomial coefficients for unit-cell angles
      Type(PVT_Table)                           :: Table                 ! A pvt table, used instead of eos parameters when imodel=-1
      real(kind=cp),allocatable, dimension(:,:) :: cv_table              ! A table for externally-provided Cv values
      logical                                   :: cv_external=.false.   ! True if cv_table contains valid data
   End Type EoS_Type

   !!----
   !!---- TYPE :: EOS_LIST_TYPE
   !!--..
   !!---- Update: 17/07/2015
   !!
   Type, public :: EoS_List_Type
      integer                                   :: N=0        ! Number of EoS List
      character(len=30)                         :: System=" " ! Crystal system name, including setting info (e.g. b-unique for mono)
      type(EoS_Type), allocatable, dimension(:) :: EoS        ! EoS Parameters
   End Type EoS_List_Type

   !!----
   !!---- TYPE :: EOS_CELL_TYPE
   !!--..
   !!---- Update: 14/02/2020.
   !!
   Type, public :: EoS_Cell_Type
      integer                                     :: N=0              ! Max index of used EoS  in List - depends on crystal system:
      character(len=30)                           :: system=" "       ! Crystal system name, including setting info (e.g. b-unique for mono)
      type(EoS_Type),dimension(0:6)               :: EoS              ! EoS Parameters for V,a,b,c,d100,d010,d001
      character(len=1)                            :: unique_label=" " ! A,B, or C to indicate unqiue axis. Only used in monoclinic
      integer                                     :: unique=0         ! integer to indicate unique axis
      logical,dimension(3)                        :: obtuse=.false.   ! .true. if cell angle is obtuse. Only used in monoclinic and triclinic
      type(Eos_type)                              :: eosc             ! The common factors to all EoS in an EoS
      type(Eos_type)                              :: eosang           ! The unit cell angle information, if stored as polynomials
      integer,dimension(0:6)                      :: loaded = 0       ! 0 when absent, 1 when eos present, 2 set by symmetry, 3 when possible to calc, 4 monoclinic d_unique (set by set_cell_types)
      character(len=1),dimension(0:6,3)           :: cout = " "       ! output array for reporting PV, VT and PVT types of EoS
      character(len=30)                           :: inputlist= " "   ! List of allowed eos that can be selected, given the cell symmetry. Useful for i/o prompts
   End Type EoS_Cell_Type

   !!----
   !!---- TYPE :: AXIS_TYPE
   !!--..
   !!---- Update: 03/02/2021
   !!
   Type, public :: Axis_type
      real(kind=cp), dimension(3)  :: v=0.0        ! UVW or hkl
      character(len=1)             :: atype=' '    ! axis type, H=hkl, U=UVW
      integer                      :: Ieos=0       ! >0 if a primary axis. 0 = volume,  -1 error, if -2 axis vector in array axis
   End Type Axis_type

   !!----
   !!----  TYPE :: EOS_DATA_TYPE
   !!--..
   !!---- Update: 17/07/2015
   !!
   Type, public :: EoS_Data_Type
      integer                     :: IUse=0    ! 0=No active, 1= active
      integer , dimension(5)      :: IGrp=0    ! Group
      integer                     :: xtype=0   ! Indicates type of data in V,cell, etc xtype=0 default, xtype=1 isothermal moduli etc
      real(kind=cp)               :: T=298.0   ! Temperature
      real(kind=cp)               :: P=0.0     ! Pressure
      real(kind=cp)               :: V=0.0     ! Volume (xtype=0) or volume derivative as indicated by xtype
      real(kind=cp), dimension(3) :: cell=0.0  ! a,b,c parameters or derivatives as indicated by xtype
      real(kind=cp), dimension(3) :: ang=0.0   ! alpha, beta, gamma parameters
      real(kind=cp)               :: SigT=0.0  ! Sigma Temperature
      real(kind=cp)               :: SigP=0.0  ! Sigma Pressure
      real(kind=cp)               :: SigV=0.0  ! Sigma Volume
      real(kind=cp), dimension (3):: sigC=0.0  ! Sigma for a, b and c parameters
      real(kind=cp), dimension (3):: sigA=0.0  ! Sigma for alpha, beta and gamma angles
   End Type EoS_Data_Type

   !!----
   !!---- TYPE :: EOS_DATA_LIST_TYPE
   !!--..
   !!---- Update: January - 2013
   !!
   Type, public :: EoS_Data_List_Type
      character(len=80)                              :: Title=" "           ! Title of dataset (normally from input datafile)
      character(len=40)                              :: System=" "          ! Crystal System  (normally set by Def_Crystal_System)
      integer                                        :: N=0                 ! Number of EoS Data List
      integer, dimension(NCOL_DATA_MAX)              :: IC_Dat=0            ! Which values are input
      character(len=15)                              :: Pscale_name=" "     ! Description of the Pressure scale of data (e.g. GPa)
      character(len=15)                              :: Vscale_name=" "     ! Description of the units of volume data (e.g. A3/cell)
      character(len=15)                              :: Lscale_name=" "     ! Description of the units of linear data  (e.g. A)
      type(EoS_Data_Type), allocatable, dimension(:) :: EoSD                ! Data values
   End Type EoS_Data_List_Type


   !--------------------!
   !---- INTERFACES ----!
   !--------------------!

   !--------------------!
   !---- Overloaded ----!
   !--------------------!
   Interface  Linear_EoS_Allowed
       Module Procedure Linear_EoS_Allowed_I
       Module Procedure Linear_EoS_Allowed_EoS
   End Interface

   Interface
      Module Function Alpha_Cal(P, T, Eos, DeltaT) Result(Alpha)
         !---- Arguments ----!
         real(kind=cp),            intent(in) :: P
         real(kind=cp),            intent(in) :: T
         type(Eos_Type),           intent(in) :: EoS
         real(kind=cp),  optional, intent(in) :: DeltaT
         real(kind=cp)                        :: Alpha
      End Function Alpha_Cal

      Module Function Deriv_Partial_P(V, T, EoS, Xtype, Calc) Result(TD)
         !---- Arguments ----!
         real(kind=cp),                      intent(in)  :: V
         real(kind=cp),                      intent(in)  :: T
         type(Eos_Type),                     intent(in)  :: EoS
         integer,          optional,         intent(in)  :: Xtype
         character(len=*), optional,         intent(in)  :: Calc
         real(kind=cp), dimension(N_EOSPAR)              :: Td
      End Function Deriv_Partial_P

      Module Function Deriv_Partial_P_Analytic(V, T, Eos) Result(TD)
         !---- Arguments ----!
         real(kind=cp),                      intent(in)  :: V
         real(kind=cp),                      intent(in)  :: T
         type(Eos_Type),                     intent(in)  :: Eos
         real(kind=cp), dimension(N_EOSPAR)              :: Td
      End Function Deriv_Partial_P_Analytic

      Module Function Deriv_Partial_P_Numeric(X1, X2, Eos, Xtype, Calc) Result(TD)
         !---- Arguments ----!
         real(kind=cp),                      intent(in) :: X1,X2
         type(Eos_Type),                     intent(in) :: Eos
         integer,         optional,          intent(in) :: Xtype
         character(len=*),optional,          intent(in) :: Calc
         real(kind=cp), dimension(n_eospar)             :: Td
      End Function Deriv_Partial_P_Numeric

      Module Function Deriv_Partial_P_Scales(V, T, EoS, Xtype, Igp) Result(TD)
         !---- Arguments ----!
         real(kind=cp),                      intent(in) :: V,T
         type(Eos_Type),                     intent(in) :: Eos
         integer,                            intent(in) :: Xtype
         integer,                            intent(in) :: Igp
         real(kind=cp)                                  :: Td
      End Function Deriv_Partial_P_Scales

      Module Function dKdT_Cal(Pin, Tin, EoS, DeltaT) Result(dKdT)
         !---- Arguments ----!
         real(kind=cp),            intent(in) :: Pin
         real(kind=cp),            intent(in) :: Tin
         type(Eos_Type),           intent(in) :: EoS
         real(kind=cp),  optional, intent(in) :: DeltaT
         real(kind=cp)                        :: dKdT
      End Function dKdT_Cal

      Module Function EoS_Cal(P, T, EoS) Result(Parvals)
         !---- Arguments ----!
         real(kind=cp),                intent(in)  :: P
         real(kind=cp),                intent(in)  :: T
         type(Eos_Type),               intent(in)  :: EoS
         real(kind=cp), dimension(6)               :: Parvals
      End Function EoS_Cal

      Module Function EoS_Cal_Esd(P, T, EoS) Result(Esd)
         !---- Arguments ----!
         real(kind=cp),                intent(in)  :: P
         real(kind=cp),                intent(in)  :: T
         type(Eos_Type),               intent(in)  :: EoS
         real(kind=cp),  dimension(6)              :: Esd
      End Function EoS_Cal_Esd

      Module Function EoS_Cal_Text(P, T, Tscale_In, EoS) Result(Text)
         !---- Arguments ----!
         real(kind=cp),      intent(in)  :: P
         real(kind=cp),      intent(in)  :: T
         character(len=*),   intent(in)  :: Tscale_in
         type(EoS_Type),     intent(in)  :: EoS
         character(len=:), allocatable   :: Text
      End Function EoS_Cal_Text

      Module Function Eoscal_Text_Direction(P, T, Tscale_In, Cell_eos, Axis) Result(Text)
         !---- Arguments ----!
         real(kind=cp),      intent(in)  :: P
         real(kind=cp),      intent(in)  :: T
         character(len=*),   intent(in)  :: Tscale_in
         type(axis_type),    intent(in)  :: Axis
         type(EoS_Cell_Type),intent(in)  :: Cell_EoS
         character(len=:), allocatable   :: Text
      End Function Eoscal_Text_Direction

      Module Function EPthermal_Factor(EoS) Result(scale)
         !---- Arguments ----!
         type(Eos_Type), intent(in) :: EoS  ! Eos Parameters
         real(kind=cp)              :: scale
      End Function EPthermal_Factor

      Module Function EthDebye(T, Theta, Natom) Result(Eth)
         !---- Arguments ----!
         real(kind=cp),  intent(in) :: T
         real(kind=cp),  intent(in) :: Theta
         real(kind=cp),  intent(in) :: Natom
         real(kind=cp)              :: Eth
      End Function EthDebye

      Module Function EthEinstein(T, Theta, Natom) Result(Eth)
         !---- Arguments ----!
         real(kind=cp),  intent(in) :: T
         real(kind=cp),  intent(in) :: Theta
         real(kind=cp),  intent(in) :: Natom
         real(kind=cp)              :: Eth
      End Function EthEinstein

      Module Function Get_Alpha_Axis(P, T, Cell_EoS, Ieos) Result(Alpha)
         !---- Arguments ----!
         real(kind=cp),      intent(in)  :: P, T
         type(eos_cell_type),intent(in)  :: Cell_EoS
         integer,            intent(in)  :: Ieos
         real(kind=cp)                   :: Alpha
      End Function Get_Alpha_Axis

      Module Function Get_Alpha_Cell(P, T, Cell_EoS, Axis) Result(Alpha)
         !---- Arguments ----!
         real(kind=cp),      intent(in)  :: p,T
         type(eos_cell_type),intent(in)  :: cell_eos
         type(axis_type),    intent(in)  :: axis
         real(kind=cp)                   :: alpha
      End Function Get_Alpha_Cell

      Module Function Get_Alpha_General(P, T, Cell_Eos, Axis) Result(Alpha)
         !---- Arguments ----!
         real(kind=cp),      intent(in)  :: p,T
         type(eos_cell_type),intent(in)  :: cell_eos
         type(axis_type),    intent(in)  :: axis
         real(kind=cp)                   :: alpha
      End Function Get_Alpha_General

      Module Function Get_Alpha_Third(P, T, Cell_Eos, Ieos) Result(Alpha)
         !---- Arguments ----!
         real(kind=cp),      intent(in) :: p,T
         type(eos_cell_type),intent(in) :: cell_eos
         integer,            intent(in) :: ieos
         real(kind=cp)                  :: alpha
      End Function Get_Alpha_Third

      Module Function Get_Angle_Deriv(P, T, Cell_Eos, Ia, RealAng, Dx) Result(Da)
         !---- Arguments ----!
         real(kind=cp),      intent(in)  :: p,T
         type(eos_cell_type),intent(in)  :: cell_eos
         integer,            intent(in)  :: ia
         logical,            intent(in)  :: realang
         character(len=1),   intent(in)  :: dx
         real(kind=cp)                   :: da
      End Function Get_Angle_Deriv

      Module Function Get_Angle_Eos_Deriv(Pin, Tin, Cell_EoS, Ia, RealAng, Xl) Result(D)
         !---- Arguments ----!
         real(kind=cp),      intent(in)  :: pin,tin
         type(eos_cell_type),intent(in)  :: cell_eos
         integer,            intent(in)  :: ia
         logical,            intent(in)  :: realang
         character(len=1),   intent(in)  :: xl
         real(kind=cp)                   :: d
      End Function Get_Angle_Eos_Deriv

      Module Function Get_Angle_Poly(P, T, EoS, Ia) Result(Ang)
         !---- Arguments ----!
         real(kind=cp), intent(in) :: p,t
         type(eos_type),intent(in) :: eos
         integer,       intent(in) :: ia            ! The angle number
         real(kind=cp)             :: ang
      End Function Get_Angle_Poly

      Module Function Get_Angle_Poly_Deriv(P, T, EoS, Ia, X) Result(D)
         !---- Arguments ----!
         real(kind=cp),   intent(in) :: p,t
         type(eos_type),  intent(in) :: eos
         integer,         intent(in) :: ia
         character(len=1),intent(in) :: x
         real(kind=cp)               :: d
      End Function Get_Angle_Poly_Deriv

      Module Function Get_Angle_VolFactor(P, T, E) Result(vf)
         !---- Arguments ----!
         real(kind=cp),      intent(in) :: p,T
         type(eos_cell_type),intent(in) :: e
         real(kind=cp)                  :: vf
      End Function Get_Angle_VolFactor

      Module Function Get_Angle_VolFactor_Deriv(P, T, E, X) Result(D)
         !---- Arguments ----!
         real(kind=cp),      intent(in) :: p,T
         type(eos_cell_type),intent(in) :: e
         character(len=1),   intent(in) :: x
         real(kind=cp)                  :: d
      End Function Get_Angle_VolFactor_Deriv

      Module Function Get_Angle_Volfactor_Deriv2(P, T, E, X) Result(D)
         !---- Arguments ----!
         real(kind=cp),      intent(in) :: p,T
         type(eos_cell_type),intent(in) :: e
         character(len=*),   intent(in) :: x
         real(kind=cp)                  :: d
      End Function Get_Angle_Volfactor_Deriv2

      Module Function Get_APL(VV0, V0, K0, Kp, Kpp, Z, Iorder) Result(A)
         !---- Arguments ----!
         real(kind=cp),               intent(in)   :: VV0, V0, K0, Kp, Kpp, Z
         integer,                     intent(in)   :: iorder
         real(kind=cp), dimension(3,3)             :: a
      End Function Get_APL

      Module Function Get_Cp(P, T, EoS) Result(C)
         !---- Arguments ----!
         real(kind=cp),  intent(in) :: P,T
         type(Eos_Type), intent(in) :: EoS
         real(kind=cp)              :: C
      End Function Get_Cp

      Module Function Get_Cv(P, T, Eos, j) result(Cv)
         !---- Arguments ----!
         real(kind=cp),     intent(in) :: P,T
         type(Eos_Type),    intent(in) :: EoS
         integer, optional, intent(in) :: j
         real(kind=cp)                 :: Cv
      End Function Get_Cv

      Module Function Get_DebyeT(V, Eos, i) result(DebyeT)
         !---- Arguments ----!
         real(kind=cp),    intent(in) :: V
         type(Eos_Type),   intent(in) :: EoS
         integer,optional, intent(in) :: i
         real(kind=cp)                :: DebyeT
      End Function Get_DebyeT

      Module Function Get_DmDt_Axis(P, T, Cell_eos, Ieos) Result(dMdT)
         !---- Arguments ----!
         real(kind=cp),      intent(in)  :: P,T
         type(eos_cell_type),intent(in)  :: cell_eos
         integer,            intent(in)  :: ieos
         real(kind=cp)                   :: dMdT
      End Function Get_DmDt_Axis

      Module Function Get_DmDt_Cell(P, T, Cell_eos, Axis) Result(dMdT)
         !---- Arguments ----!
         real(kind=cp),      intent(in)  :: p,T
         type(eos_cell_type),intent(in)  :: cell_eos
         type(axis_type),    intent(in)  :: axis
         real(kind=cp)                   :: dMdT
      End Function Get_DmDt_Cell

      Module Function Get_DmDt_General(P, T, Cell_eos, Axis) Result(dMdT)
         !---- Arguments ----!
         real(kind=cp),      intent(in)  :: p,T
         type(eos_cell_type),intent(in)  :: cell_eos
         type(axis_type),    intent(in)  :: axis
         real(kind=cp)                   :: dMdT
      End Function Get_DmDt_General

      Module Function Get_DmDt_Third(P, T, Cell_eos, Ieos) Result(modp)
         !---- Arguments ----!
         real(kind=cp),      intent(in) :: p,T
         type(eos_cell_type),intent(in) :: cell_eos
         integer,            intent(in) :: ieos
         real(kind=cp)                  :: modp
      End Function Get_DmDt_Third

      Module Function Get_GPT(P,T,EoS) Result(gpt)
         !---- Arguments ----!
         real(kind=cp),  intent(in) :: P
         real(kind=cp),  intent(in) :: T
         type(Eos_Type), intent(in) :: Eos
         real(kind=cp)              :: Gpt
      End Function Get_GPT

      Module Function Get_Grun_PT(P,T,EoS, I) Result(G)
         !---- Arguments ----!
         real(kind=cp),     intent(in) :: P
         real(kind=cp),     intent(in) :: T
         type(Eos_Type),    intent(in) :: EoS
         integer, optional, intent(in) :: I
         real(kind=cp)                 :: G
      End Function Get_Grun_PT

      Module Function Get_Grun_Th(P, T, Eos)  Result(G)
         !---- Arguments ----!
         real(kind=cp),  intent(in) :: P,T
         type(Eos_Type), intent(in) :: EoS
         real(kind=cp)              :: g
      End Function Get_Grun_Th

      Module Function Get_Grun_V(V, Eos, i) Result(Grun)
         !---- Arguments ----!
         real(kind=cp),     intent(in) :: V
         type(Eos_Type),    intent(in) :: EoS
         integer, optional, intent(in) :: i
         real(kind=cp)                 :: Grun
      End Function Get_Grun_V

      Module Function Get_K(P,T,EoS) Result(K)
         !---- Arguments ----!
         real(kind=cp),intent(in)        :: p
         real(kind=cp),intent(in)        :: t
         type(Eos_Type),intent(in)       :: EoS
         real(kind=cp)                   :: K
      End Function Get_K

      Module Function Get_Kp(P,T,EoS) Result(Kp)
         !---- Arguments ----!
         real(kind=cp),intent(in)        :: p
         real(kind=cp),intent(in)        :: t
         type(Eos_Type),intent(in)       :: EoS
         real(kind=cp)                   :: kp
      End Function Get_Kp

      Module Function Get_K0_T(T,EoS) Result(k0)
         !---- Arguments ----!
         real(kind=cp),  intent(in) :: T
         type(Eos_Type), intent(in) :: EoS
         real(kind=cp)              :: k0
      End Function Get_K0_T

      Module Function Get_Kp0_T(T, Eos) Result(kp0)
         !---- Arguments ----!
         real(kind=cp),  intent(in) :: T
         type(Eos_Type), intent(in) :: EoS
         real(kind=cp)              :: kp0
      End Function Get_Kp0_T

      Module Function Get_Kpp0_T(T, Eos) Result(kpp0)
         !---- Arguments ----!
         real(kind=cp),  intent(in) :: T
         type(Eos_Type), intent(in) :: EoS
         real(kind=cp)              :: kpp0
      End Function Get_Kpp0_T

      Module Function Get_Mod_Axis(P, T, Cell_eos, Ieos) result(Modu)
         !---- Arguments ----!
         real(kind=cp),      intent(in)  :: p,T
         type(eos_cell_type),intent(in)  :: cell_eos
         integer,            intent(in)  :: ieos
         real(kind=cp)                   :: modu
      End Function Get_Mod_Axis

      Module Function Get_Mod_Cell(P, T, Cell_eos, Axis) Result(Modu)
         !---- Arguments ----!
         real(kind=cp),      intent(in)  :: p,T
         type(eos_cell_type),intent(in)  :: cell_eos
         type(axis_type),    intent(in)  :: axis
         real(kind=cp)                   :: modu
      End Function Get_Mod_Cell

      Module Function Get_Mod_General(P, T, Cell_eos, Axis) Result(Modu)
         !---- Arguments ----!
         real(kind=cp),      intent(in)  :: p,T
         type(eos_cell_type),intent(in)  :: cell_eos
         type(axis_type),    intent(in)  :: axis
         real(kind=cp)                   :: modu
      End Function Get_Mod_General

      Module Function Get_Mod_Third(P, T, Cell_eos, Ieos) Result(Modu)
         !---- Arguments ----!
         real(kind=cp),       intent(in) :: p,T
         type(eos_cell_type), intent(in) :: cell_eos
         integer,             intent(in) :: ieos
         real(kind=cp)                   :: modu
      End Function Get_Mod_Third

      Module Function Get_Modp_Axis(P, T, Cell_eos, Ieos) Result(Modp)
         !---- Arguments ----!
         real(kind=cp),      intent(in)  :: p,T
         type(eos_cell_type),intent(in)  :: cell_eos
         integer,            intent(in)  :: ieos
         real(kind=cp)                   :: modp
      End Function Get_Modp_Axis

      Module Function Get_Modp_Cell(P, T, Cell_eos, Axis) Result(Modp)
         !---- Arguments ----!
         real(kind=cp),      intent(in)  :: p,T
         type(eos_cell_type),intent(in)  :: cell_eos
         type(axis_type),    intent(in)  :: axis
         real(kind=cp)                   :: modp
      End Function Get_Modp_Cell

      Module Function Get_Modp_General(P, T, Cell_eos, Axis) Result(Mp)
         !---- Arguments ----!
         real(kind=cp),      intent(in)  :: p,T
         type(eos_cell_type),intent(in)  :: cell_eos
         type(axis_type),    intent(in)  :: axis
         real(kind=cp)                   :: mp
      End Function Get_Modp_General

      Module Function Get_Modp_Third(P, T, Cell_Eos, Ieos) Result(Modp)
         !---- Arguments ----!
         real(kind=cp),      intent(in) :: p,T
         type(eos_cell_type),intent(in) :: cell_eos
         integer,            intent(in) :: ieos
         real(kind=cp)                  :: modp
      End Function Get_Modp_Third

      Module Function Get_Params_Cell(P, T, Cell_Eos, Cartype) Result(Cell)
         !---- Arguments ----!
         real(kind=cp),              intent(in) :: p,t
         type(eos_cell_type),        intent(in) :: cell_eos
         character(len=2), optional, intent(in) :: cartype
         type(cell_g_type)                      :: cell
      End Function Get_Params_Cell

      Module Function Get_Press_Axis(A, T, Cell_eos, Ieos)  Result(P)
         !---- Arguments ----!
         real(kind=cp),      intent(in) :: a,T
         type(eos_cell_type),intent(in) :: cell_eos
         integer,            intent(in) :: ieos
         real(kind=cp)                  :: p
      End Function Get_Press_Axis

      Module Function Get_Press_Cell(A, T, Cell_eos, Axis) Result(P)
         !---- Arguments ----!
         real(kind=cp),      intent(in) :: a,t
         type(eos_cell_type),intent(in) :: cell_eos
         type(axis_type),    intent(in) :: axis
         real(kind=cp)                  :: p
      End Function Get_Press_Cell

      Module Function Get_Press_General(A, T, Cell_Eos, Axis) Result(P)
         !---- Arguments ----!
         real(kind=cp),      intent(in) :: a,t
         type(eos_cell_type),intent(in) :: cell_eos
         type(axis_type),    intent(in) :: axis
         real(kind=cp)                  :: p
      End Function Get_Press_General

      Module Function Get_Press_Third(A, T, Cell_Eos, Ieos, Pguess) Result(P)
         !---- Arguments ----!
         real(kind=cp),         intent(in) :: a,T
         type(eos_cell_type),   intent(in) :: cell_eos
         integer,               intent(in) :: ieos
         real(kind=cp),optional,intent(in) :: pguess
         real(kind=cp)                     :: p
      End Function Get_Press_Third

      Module Function Get_Pressure(V, T, Eos) Result(P)
         !---- Arguments ----!
         real(kind=cp),  intent(in) :: V
         real(kind=cp),  intent(in) :: T
         type(Eos_Type), intent(in) :: EoS
         real(kind=cp)              :: p
      End Function Get_Pressure

      Module Function Get_Pressure_Esd(V, T, Eos) Result(Esd)
         !---- Arguments ----!
         real(kind=cp),  intent(in) :: V
         real(kind=cp),  intent(in) :: T
         type(Eos_Type), intent(in) :: EoS
         real(kind=cp)              :: esd
      End Function Get_Pressure_Esd

      Module Function Get_Pressure_K(K, T, Eos, Itype, Pest) Result(P)
         !---- Arguments ----!
         real(kind=cp),           intent(in) :: K
         real(kind=cp),           intent(in) :: T
         type(Eos_Type),          intent(in) :: EoS
         integer,                 intent(in) :: itype
         real(kind=cp), optional, intent(in) :: Pest
         real(kind=cp)                       :: p
      End Function Get_Pressure_K

      Module Function Get_Pressure_X(X, T, Eos, Xtype, Pest) Result(P)
         !---- Arguments ----!
         real(kind=cp),           intent(in) :: x
         real(kind=cp),           intent(in) :: T
         type(Eos_Type),          intent(in) :: EoS
         integer,                 intent(in) :: Xtype
         real(kind=cp), optional, intent(in) :: Pest
         real(kind=cp)                       :: p
      End Function Get_Pressure_X

      Module Function Get_Property_X(P, T, Eos, Xtype) Result(Val)
         !---- Arguments ----!
         real(kind=cp),     intent(in) :: P
         real(kind=cp),     intent(in) :: T
         type(Eos_Type),    intent(in) :: EoS
         integer, optional, intent(in) :: xtype
         real(kind=cp)                 :: Val
      End Function Get_Property_X

      Module Function Get_Props_General(P, T, Cell_Eos, Axis) Result(Parvals)
         !---- Arguments ----!
         real(kind=cp),      intent(in)  :: p,T
         type(eos_cell_type),intent(in)  :: cell_eos
         type(axis_type),    intent(in)  :: axis
         real(kind=cp), dimension(6)     :: parvals
      End Function Get_Props_General

      Module Function Get_Props_Third(P, T, Cell_Eos, Ieos) Result(Parvals)
         !---- Arguments ----!
         real(kind=cp),             intent(in)  :: p,T
         type(eos_cell_type),       intent(in)  :: cell_eos
         integer,                   intent(in)  :: ieos
         real(kind=cp), dimension(6)            :: parvals
      End Function Get_Props_Third

      Module Function Get_Props_PTVTable(P, T, V, Eos, Res) Result(Val)
         !---- Arguments ----!
         real(kind=cp),    intent(in) :: P
         real(kind=cp),    intent(in) :: T
         real(kind=cp),    intent(in) :: V
         type(Eos_Type),   intent(in) :: EoS
         character(len=*), intent(in) :: Res
         real(kind=cp)                :: Val
      End Function Get_Props_PTVTable

      Module Function Get_Tait(T, Eos) Result(Vec)
         !---- Arguments ----!
         real(kind=cp),             intent(in)  :: T
         type(Eos_Type),            intent(in)  :: Eos
         real(kind=cp), dimension(3)            :: Vec
      End Function Get_Tait

      Module Function Get_Temperature(P, V, Eos) Result(T)
         !---- Arguments ----!
         real(kind=cp),  intent(in) :: P
         real(kind=cp),  intent(in) :: V
         type(Eos_Type), intent(in) :: EoS
         real(kind=cp)              :: t
      End Function Get_Temperature

      Module Function Get_Temperature_P0(V, EoS, Tmin, Tmax) Result(Tk)
         !---- Arguments ----!
         real(kind=cp),           intent(in) :: V
         type(Eos_Type),          intent(in) :: EoS
         real(kind=cp), optional, intent(in) :: Tmin
         real(kind=cp), optional, intent(in) :: Tmax
         real(kind=cp)                       :: tk
      End Function Get_Temperature_P0

      Module Function Get_Transition_Pressure(T, Eos) Result(Ptr)
         !---- Arguments ----!
         real(kind=cp),  intent(in) :: T
         type(Eos_Type), intent(in) :: EoS
         real(kind=cp)              :: Ptr
      End Function Get_Transition_Pressure

      Module Function Get_Transition_Strain(P, T, Eos) Result(VS)
         !---- Arguments ----!
         real(kind=cp),  intent(in) :: P
         real(kind=cp),  intent(in) :: T
         type(Eos_Type), intent(in) :: EoS
         real(kind=cp)              :: Vs
      End Function Get_Transition_Strain

      Module Function Get_Transition_Temperature(P, Eos) Result(Tr)
         !---- Arguments ----!
         real(kind=cp),  intent(in) :: P
         type(Eos_Type), intent(in) :: EoS
         real(kind=cp)              :: Tr
      End Function Get_Transition_Temperature

      Module Function Get_V0_Axis(Cell_eos, Ieos) result(L)
         !---- Arguments ----!
         type(eos_cell_type), intent(in)  :: cell_eos
         integer,             intent(in)  :: ieos
         real(kind=cp)                    :: L
      End Function Get_V0_Axis

      Module Function Get_V0_Cell(Cell_eos, Axis) result(L)
         !---- Arguments ----!
         type(eos_cell_type), intent(in)  :: cell_eos
         type(axis_type),     intent(in)  :: axis
         real(kind=cp)                    :: L
      End Function Get_V0_Cell

      Module Function Get_V0_T(T, EoS) Result(V)
         !---- Arguments ----!
         real(kind=cp),  intent(in) :: T
         type(Eos_Type), intent(in) :: EoS
         real(kind=cp)              :: V
      End Function Get_V0_T

      Module Function Get_Volume(P, T, Eos) Result(V)
         !---- Arguments ----!
         real(kind=cp),  intent(in) :: P
         real(kind=cp),  intent(in) :: T
         type(Eos_Type), intent(in) :: EoS
         real(kind=cp)              :: V
      End Function Get_Volume

      Module Function Get_Volume_Axis(P, T, Cell_Eos, Ieos) Result(L)
         !---- Arguments ----!
         real(kind=cp),      intent(in)  :: p,T
         type(eos_cell_type),intent(in)  :: cell_eos
         integer,            intent(in)  :: ieos
         real(kind=cp)                   :: L
      End Function Get_Volume_Axis

      Module Function Get_Volume_Cell(P, T, Cell_Eos, Axis) Result(L)
         !---- Arguments ----!
         real(kind=cp),      intent(in)  :: p,T
         type(eos_cell_type),intent(in)  :: cell_eos
         type(axis_type),    intent(in)  :: axis
         real(kind=cp)                   :: L
      End Function Get_Volume_Cell

      Module Function Get_Volume_General(P, T, Cell_Eos, Axis) Result(L)
         !---- Arguments ----!
         real(kind=cp),      intent(in)  :: p,T
         type(eos_cell_type),intent(in)  :: cell_eos
         type(axis_type),    intent(in)  :: axis
         real(kind=cp)                   :: L
      End Function Get_Volume_General

      Module Function Get_Volume_K(K, T, EoS) Result(V)
         !---- Arguments ----!
         real(kind=cp),  intent(in) :: K
         real(kind=cp),  intent(in) :: T
         type(Eos_Type), intent(in) :: EoS
         real(kind=cp)              :: V
      End Function Get_Volume_K

      Module Function Get_Volume_S(S, T, Eos) Result(V)
         !---- Arguments ----!
         real(kind=cp),  intent(in) :: S
         real(kind=cp),  intent(in) :: T
         type(Eos_Type), intent(in) :: EoS
         real(kind=cp)              :: V
      End Function Get_Volume_S

      Module Function Get_Volume_Third(P, T, Cell_Eos, Ieos) Result(L)
         !---- Arguments ----!
         real(kind=cp),      intent(in) :: p,T
         type(eos_cell_type),intent(in) :: cell_eos
         integer,            intent(in) :: ieos
         real(kind=cp)                  :: l
      End Function Get_Volume_Third

      Module Function Isotropic_Cell(cell_eos) result(isotropic)
         !---- Arguments ----!
         type(eos_cell_type),intent(in)      :: cell_eos
         logical                             :: isotropic
      End Function Isotropic_Cell

      Module Function K_Cal(V, T, Eos, P) Result(Kc)
         !---- Arguments ----!
         real(kind=cp),            intent(in) :: V
         real(kind=cp),            intent(in) :: T
         type(Eos_Type),           intent(in) :: EoS
         real(kind=cp),  optional, intent(in) :: P
         real(kind=cp)                        :: kc
      End Function K_Cal

      Module Function Kp_Cal(V, T, EoS, P) Result (Kpc)
         !---- Arguments ----!
         real(kind=cp),           intent(in) :: V
         real(kind=cp),           intent(in) :: T
         type(Eos_Type),          intent(in) :: EoS
         real(kind=cp), optional, intent(in) :: P
         real(kind=cp)                       :: kpc
      End Function Kp_Cal

      Module Function Kpp_Cal(V, T, EoS) Result (Kppc)
         !---- Arguments ----!
         real(kind=cp),  intent(in) :: V
         real(kind=cp),  intent(in) :: T
         type(Eos_Type), intent(in) :: EoS
         real(kind=cp)              :: kppc
      End Function Kpp_Cal

      Module Function Linear_EoS_Allowed_Eos(Eos) Result(Allowed)
         !---- Arguments ----!
         type(Eos_Type), intent(in) :: EoS
         logical                    :: Allowed
      End Function Linear_EoS_Allowed_Eos

      Module Function Linear_EoS_Allowed_I(Imodel, Itherm) Result(Allowed)
         !---- Arguments ----!
         integer, optional, intent(in) :: imodel
         integer, optional, intent(in) :: itherm
         logical                       :: allowed
      End Function Linear_EoS_Allowed_I

      Module Function Murn_Interpolate_PTV_Table(I, J, P, Eos) Result(V)
         !---- Arguments ----!
         integer,        intent(in) :: I
         integer,        intent(in) :: J
         real(kind=cp),  intent(in) :: P
         type(Eos_Type), intent(in) :: EoS
         real(kind=cp)              :: V
      End Function Murn_Interpolate_PTV_Table

      Module Function Murn_PTV_Table(I, J, Eos) Result(Eosm)
         !---- Arguments ----!
         integer,        intent(in)  :: i
         integer,        intent(in)  :: j
         type(Eos_Type), intent(in)  :: EoS
         type(Eos_Type)              :: EoSm
      End Function Murn_PTV_Table

      Module Function NormPressure_Eos(S, T, Eos) Result(F)
         !---- Arguments ----!
         real(kind=cp),  intent(in) :: S
         real(kind=cp),  intent(in) :: T
         type(Eos_Type), intent(in) :: EoS
         real(kind=cp)              :: F
      End Function NormPressure_Eos

      Module Function NormPressure_P(S, P, Imodel) Result(F)
         !---- Arguments ----!
         real(kind=cp),  intent(in) :: S
         real(kind=cp),  intent(in) :: P
         integer      ,  intent(in) :: Imodel
         real(kind=cp)              :: F
      End Function NormPressure_P

      Module Function Pressure_F(F,S,Eos) Result(P)
         !---- Arguments ----!
         real(kind=cp),  intent(in) :: F
         real(kind=cp),  intent(in) :: S
         type(Eos_Type), intent(in) :: EoS
         real(kind=cp)              :: P
      End Function Pressure_F

      Module Function Principal_EoS(Cell_Eos, I) Result(Ieos)
         !---- Arguments ----!
         type(eos_cell_type),intent(in) :: cell_eos !eos for cells
         integer,            intent(in) :: i        !proposed axis number
         integer                        :: ieos
      End Function Principal_EoS

      Module Function PscaleMGD(EoS) Result(MGD)
         !---- Arguments ----!
         type(Eos_Type), intent(in)  :: EoS
         logical                     :: MGD
      End Function PscaleMGD

      Module Function PThermal(V, T, Eos, J) Result(Pth)
         !---- Arguments ----!
         real(kind=cp),   intent(in) :: V
         real(kind=cp),   intent(in) :: T
         type(Eos_Type),  intent(in) :: EoS
         integer,optional,intent(in) :: j
         real(kind=cp)               :: Pth
      End Function PThermal

      Module Function Set_XdataTypes(Gdat, Used) Result(XdataTypes)
         !---- Arguments ----!
         type (EoS_Data_List_Type), intent(in) :: Gdat
         logical,                   intent(in) :: Used
         integer, dimension(0:N_DATA_TYPES)    :: XdataTypes
      End Function Set_XdataTypes

      Module Function Strain(VV0, Eos) Result(S)
         !---- Arguments ----!
         real(kind=cp),  intent(in) :: VV0
         type(Eos_Type), intent(in) :: EoS
         real(kind=cp)              :: S
      End Function Strain

      Module Function Strain_EoS(V, T,Eos) Result(S)
         !---- Arguments ----!
         real(kind=cp),  intent(in) :: V
         real(kind=cp),  intent(in) :: T
         type(Eos_Type), intent(in) :: EoS
         real(kind=cp)              :: S
      End Function Strain_EoS

      Module Function Thermal_Pressure_Eos(I) Result(Pth)
         !---- Arguments ----!
         integer, intent(in) :: I
         logical             :: Pth
      End Function Thermal_Pressure_Eos

      Module Function Transform_Esd(P, T, Eos) Result(Esd)
         !---- Arguments ----!
         real(kind=cp),   intent(in)              :: P
         real(kind=cp),   intent(in)              :: T
         type (EoS_Type), intent(in)              :: EoS
         real(kind=cp),dimension(N_EOSPAR)        :: Esd
      End Function Transform_Esd

      Module Function Transition_Phase(P, T, Eos) Result(Ip)
         !---- Arguments ----!
         real(kind=cp),  intent(in) :: P
         real(kind=cp),  intent(in) :: T
         type(Eos_Type), intent(in) :: EoS
         logical                    :: Ip
      End Function Transition_Phase

      Module Function VscaleMGD(EoS) Result(MGD)
         !---- Arguments ----!
         type(Eos_Type),intent(in)  :: EoS
         logical                    :: MGD
      End Function VscaleMGD

      Module Subroutine Allocate_EoS_Data_List(N, E)
         !---- Arguments ----!
         integer,                    intent(in)       :: N
         type (eos_data_list_type),  intent(in out)   :: E
      End Subroutine Allocate_EoS_Data_List

      Module Subroutine Allocate_EoS_List(N, E)
         !---- Arguments ----!
         integer,               intent(in)       :: N
         type (eos_list_type),  intent(in out)   :: E
      End Subroutine Allocate_EoS_List

      Module Subroutine Calc_Conlev(Eos,ix,iy,isig,xyy,n)
         !---- Arguments ----!
         type(Eos_Type),              intent(in)  :: Eos
         integer,                     intent(in)  :: ix
         integer,                     intent(in)  :: iy
         integer,                     intent(in)  :: isig
         real(kind=cp),dimension(:,:),intent(out) :: xyy
         integer,                     intent(out) :: n
      End Subroutine Calc_Conlev

      Module Subroutine Check_Axis(Cell, Axis, OK)
         !---- Arguments ----!
         type(eos_cell_type), intent(in out) :: Cell
         type(axis_type),     intent(in)     :: Axis
         logical                             :: Ok
      End Subroutine Check_Axis

      Module Subroutine Check_Scales(EoS, Dat)
         !---- Arguments ----!
         type(Eos_Type),                       intent(in)  :: EoS
         type (eos_data_list_type), optional,  intent(in)  :: Dat
      End  Subroutine Check_Scales

      Module Subroutine Copy_Eos_Data_List(Dat1, Dat2)
         !---- Arguments ----!
         type (eos_data_list_type), intent(in)      :: Dat1
         type (eos_data_list_type), intent(in out)  :: Dat2
      End Subroutine Copy_Eos_Data_List

      Module Subroutine Deallocate_EoS_Data_List(E)
         !---- Arguments ----!
         type (eos_data_list_type), intent(in out)   :: E
      End Subroutine Deallocate_EoS_Data_List

      Module Subroutine Deallocate_EoS_List(E)
         !---- Arguments ----!
         type (eos_list_type), intent(in out)   :: E
      End Subroutine Deallocate_EoS_List

      Module Subroutine Eos_Cell_Loaded_Check(Cell_Eos)
         !---- Arguments ----!
         type(eos_cell_type), intent(in) :: cell_eos
      End Subroutine Eos_Cell_Loaded_Check

      Module Subroutine EoSParams_Check(EoS)
         !---- Argument ----!
         type (EoS_Type), intent(in out) :: EoS
      End Subroutine EoSParams_Check

      Module Subroutine FfCal_Dat(V,V0,P,EoS,F,S)
         !---- Arguments ----!
         real(kind=cp),           intent(in)  :: V
         real(kind=cp),           intent(in)  :: V0
         real(kind=cp),           intent(in)  :: P
         type(Eos_Type),          intent(in)  :: EoS
         real(kind=cp), optional, intent(out) :: F
         real(kind=cp), optional, intent(out) :: S
      End Subroutine FfCal_Dat

      Module Subroutine FfCal_Dat_Esd(V,SigV,V0,SigV0,P,SigP,Eos,F,SigF,S,SigS)
         !---- Arguments ----!
         real(kind=cp),  intent(in)  :: V
         real(kind=cp),  intent(in)  :: SigV
         real(kind=cp),  intent(in)  :: V0
         real(kind=cp),  intent(in)  :: SigV0
         real(kind=cp),  intent(in)  :: P
         real(kind=cp),  intent(in)  :: SigP
         type(Eos_Type), intent(in)  :: EoS
         real(kind=cp),  intent(out) :: F
         real(kind=cp),  intent(out) :: SigF
         real(kind=cp),  intent(out) :: S
         real(kind=cp),  intent(out) :: SigS
      End Subroutine FfCal_Dat_Esd

      Module Subroutine FfCal_EoS(P,T,EoS,F,S)
         !---- Arguments ----!
         real(kind=cp),           intent(in)     :: P
         real(kind=cp),           intent(in)     :: T
         type(Eos_Type),          intent(in)     :: EoS
         real(kind=cp),           intent(out)    :: F
         real(kind=cp),           intent(out)    :: S
      End Subroutine FfCal_EoS

      Module Subroutine Get_Tensor_Eos(P, T, Cell_Eos, Dx, X)
         !---- Arguments ----!
         real(kind=cp),           intent(in)     :: P
         real(kind=cp),           intent(in)     :: T
         type(eos_cell_type),     intent(in)     :: Cell_eos
         character(len=1),        intent(in)     :: Dx
         type(Strain_Tensor_Type),intent(in out) :: X
      End Subroutine Get_Tensor_Eos

      Module Subroutine Init_EoS_Angles(EoS)
         !---- Arguments ----!
         type (EoS_Type), intent(in out) :: EoS
      End Subroutine Init_EoS_Angles

      Module Subroutine Init_Eos_Cell_Type(Cell_Eos)
         !---- Arguments ----!
         type(eos_cell_type),intent(in out) :: Cell_EoS
      End Subroutine Init_Eos_Cell_Type

      Module Subroutine Init_EoS_Cross(EoS)
         !---- Arguments ----!
         type (EoS_Type), intent(in out) :: EoS
      End Subroutine Init_EoS_Cross

      Module Subroutine Init_EoS_Data_Type(E)
         !---- Arguments ----!
         type (EoS_Data_Type), intent(in out)   :: E
      End Subroutine Init_EoS_Data_Type

      Module Subroutine Init_EoS_GroupScales(EoS)
         !---- Arguments ----!
         type (EoS_Type), intent(in out) :: EoS
      End Subroutine Init_EoS_GroupScales

      Module Subroutine Init_EoS_Osc(EoS, i)
         !---- Arguments ----!
         type (EoS_Type), intent(in out) :: EoS
         integer,         intent(in)     :: i
      End Subroutine Init_EoS_Osc

      Module Subroutine Init_EoS_Shear(EoS)
         !---- Arguments ----!
         type (EoS_Type), intent(in out) :: EoS
      End Subroutine Init_EoS_Shear

      Module Subroutine Init_EoS_Thermal(EoS)
         !---- Arguments ----!
         type (EoS_Type), intent(in out) :: EoS
      End Subroutine Init_EoS_Thermal

      Module Subroutine Init_EoS_Transition(EoS)
         !---- Arguments ----!
         type (EoS_Type), intent(in out) :: EoS
      End Subroutine Init_EoS_Transition

      Module Subroutine Init_EoS_Type(EoS, CLin, IThermal, ITransition, IShear, ICross)
         !---- Arguments ----!
         type (EoS_Type),            intent(out)    :: EoS
         character(len=*), optional, intent(in)     :: CLin
         integer,          optional, intent(in)     :: IThermal
         integer,          optional, intent(in)     :: ITransition
         integer,          optional, intent(in)     :: IShear
         integer,          optional, intent(in)     :: ICross
      End Subroutine Init_EoS_Type

      Module Subroutine Physical_Check(Eos, Pin, Tin, Vin)
         !---- Arguments ----!
         type(Eos_Type),        intent(in) :: Eos
         real(kind=cp),optional,intent(in) :: Pin
         real(kind=cp),optional,intent(in) :: Tin
         real(kind=cp),optional,intent(in) :: Vin
      End Subroutine Physical_Check

      Module Subroutine PVEoS_Check(EoS, Pin, Vin, Tin, Vpresent)
         !---- Arguments ----!
         type(Eos_Type),          intent(in) :: Eos
         real(kind=cp), optional, intent(in) :: pin
         real(kind=cp), optional, intent(in) :: vin
         real(kind=cp), optional, intent(in) :: tin
         logical,                 intent(in) :: vpresent
      End Subroutine PVEoS_Check

      Module Subroutine Read_EoS_DataFile(FName, Dat)
         !---- Arguments ----!
         character(len=*),          intent(in)  :: FName
         type (eos_data_list_type), intent(out) :: Dat
      End Subroutine Read_EoS_DataFile

      Module Subroutine Read_Eos_File(Fname, Eos)
         !---- Arguments ----!
         character(len=*),intent(in)  :: fname
         type (EoS_Type), intent(out) :: Eos
      End Subroutine Read_Eos_File

      Module Subroutine Read_Eos_In(Flines,Eos)
         !---- Arguments ----!
         character(len=*),dimension(:),intent(in)   :: Flines
         type(Eos_type),               intent(out)  :: EoS
      End Subroutine Read_Eos_In

      Module Subroutine Read_Multiple_Eos_File(Fname,Eoslist)
         !---- Arguments ----!
         character(len=*),     intent(in)  :: Fname
         type (EoS_List_Type), intent(out) :: Eoslist
      End Subroutine Read_Multiple_Eos_File

      Module Subroutine Set_Cell_Types(Cell_Eos)
         !---- Arguments ----!
          type (eos_cell_type), intent(in out) :: cell_eos
      End Subroutine Set_Cell_Types

      Module Subroutine Set_Cross_Names(EoS)
         !---- Arguments ----!
         type (EoS_Type), intent(in out) :: EoS
      End Subroutine Set_Cross_Names

      Module Subroutine Set_EoS_Factors(EoS)
         !---- Arguments ----!
         type (EoS_Type), intent(in out) :: EoS
      End Subroutine Set_EoS_Factors

      Module Subroutine Set_EoS_Implied_Values(EoS)
         !---- Arguments ----!
         type (EoS_Type), intent(in out) :: EoS
      End Subroutine Set_EoS_Implied_Values

      Module Subroutine Set_Eos_Names(EoS)
         !---- Arguments ----!
         type (EoS_Type), intent(in out) :: EoS
      End Subroutine Set_Eos_Names

      Module Subroutine Set_EoS_Use(EoS)
         !---- Arguments ----!
         type (EoS_Type), intent(in out) :: EoS
      End Subroutine Set_EoS_Use

      Module Subroutine Set_Osc_Names(EoS)
         !---- Arguments ----!
         type (EoS_Type), intent(in out) :: EoS
      End Subroutine Set_Osc_Names

      Module Subroutine Set_Shear_Names(EoS)
         !---- Arguments ----!
         type (EoS_Type), intent(in out) :: EoS
      End Subroutine Set_Shear_Names

      Module Subroutine Set_Thermal_Names(EoS)
         !---- Arguments ----!
         type (EoS_Type), intent(in out) :: EoS
      End Subroutine Set_Thermal_Names

      Module Subroutine Set_Transition_Names(EoS)
         !---- Arguments ----!
         type (EoS_Type), intent(in out) :: EoS
      End Subroutine Set_Transition_Names

      Module Subroutine Write_Data_Conlev(Xyy, N, Iout)
         !---- Arguments ----!
         real(kind=cp),dimension(:,:),intent(in)   :: xyy
         integer,                     intent(in)   :: n
         integer, optional,           intent(in)   :: iout
      End Subroutine Write_Data_Conlev

      Module Subroutine Write_EoS_DataFile(Dat, Lun)
         !---- Arguments ----!
         type (eos_data_list_type), intent(in) :: Dat
         integer,                   intent(in) :: Lun
      End Subroutine Write_EoS_DataFile

      Module Subroutine Write_Eos_File(Eos, Lun)
         !---- Arguments ----!
         type (EoS_Type), intent(in)   :: EoS
         integer, intent(in)           :: Lun
      End Subroutine Write_Eos_File

      Module Subroutine Write_Eoscal(Pmin, Pmax, Pstep, Tmin, Tmax, Tstep, Tscale_In, Eos, Lun, &
                                     Nprint, Eoscal_ERR, Cell_Eos, Axis)
         !---- Arguments ----!
         real(kind=cp),                 intent(in)     ::  pmin, pmax, pstep
         real(kind=cp),                 intent(in)     ::  tmin,tmax,tstep
         character(len=*),              intent(in)     ::  tscale_in
         type(EoS_Type),                intent(in)     ::  eos
         integer,                       intent(in)     :: lun
         integer,                       intent(out)    :: nprint
         logical,                       intent(out)    :: eoscal_err
         type(EoS_Cell_Type), optional, intent(in out) :: cell_eos
         type(axis_type),     optional, intent(in out) :: axis
      End Subroutine Write_Eoscal

      Module Subroutine Write_Eoscal_Cell_Header(Axis, Lun, Tscale_In)
         !---- Arguments ----!
         type(axis_type),  intent(in) :: Axis
         integer,          intent(in) :: Lun
         character(len=*), intent(in) :: Tscale_in
      End Subroutine Write_Eoscal_Cell_Header

      Module Subroutine Write_Eoscal_Header(Eos,Lun,Tscale_In,Headout)
         !---- Arguments ----!
         type(EoS_Type),            intent(in)  :: EoS
         integer,                   intent(in)  :: Lun
         character(len=*),          intent(in)  :: Tscale_in
         character(len=*),optional, intent(out) :: Headout
      End Subroutine Write_Eoscal_Header

      Module Subroutine Write_Info_Angle_Poly(EoS, Iang, Iout)
         !---- Arguments ----!
         type(eos_type),    intent(in) :: EoS
         integer, optional, intent(in) :: Iang
         integer, optional, intent(in) :: Iout
      End Subroutine Write_Info_Angle_Poly

      Module Subroutine Write_Info_Conlev(Eos,ix,iy,isig,iout)
         !---- Arguments ----!
         type(Eos_Type),    intent(in) :: Eos
         integer,           intent(in) :: ix
         integer,           intent(in) :: iy
         integer,           intent(in) :: isig
         integer, optional, intent(in) :: iout
      End Subroutine Write_Info_Conlev

      Module Subroutine Write_Info_Eos(EoS, Iout)
         !---- Arguments ----!
         type(Eos_Type),    intent(in) :: EoS
         integer, optional, intent(in) :: Iout
      End Subroutine Write_Info_Eos

      Module Subroutine Write_Info_Eos_Cell_Type(Cell_Eos, Iout)
         !---- Arguments ----!
         type(eos_cell_type), intent(in out) :: cell_eos
         integer, optional,   intent(in)     :: iout
      End Subroutine Write_Info_Eos_Cell_Type

      Module Subroutine Write_Info_Eos_Cross(Eos,Iout)
         !---- Arguments ----!
         type(Eos_Type),    intent(in) :: Eos
         integer, optional, intent(in) :: iout
      End Subroutine Write_Info_Eos_Cross

      Module Subroutine Write_Info_Eos_GroupScales(EoS, Iout)
         !---- Arguments ----!
         type(Eos_Type),    intent(in) :: Eos
         integer, optional, intent(in) :: iout
      End Subroutine Write_Info_Eos_GroupScales

      Module Subroutine Write_Info_Eos_Oscillator(EoS, Iout)
         !---- Arguments ----!
         type(Eos_Type),    intent(in) :: Eos
         integer, optional, intent(in) :: iout
      End Subroutine Write_Info_Eos_Oscillator

      Module Subroutine Write_Info_Eos_Shear(Eos,Iout)
         !---- Arguments ----!
         type(Eos_Type),    intent(in) :: Eos
         integer, optional, intent(in) :: iout
      End Subroutine Write_Info_Eos_Shear

      Module Subroutine Write_Info_Eos_Thermal(EoS, Iout)
         !---- Arguments ----!
         type(Eos_Type),    intent(in) :: EoS
         integer, optional, intent(in) :: Iout
      End Subroutine Write_Info_Eos_Thermal

      Module Subroutine Write_Info_Eos_Transition(EoS, Iout)
         !---- Arguments ----!
         type(Eos_Type),    intent(in) :: EoS
         integer, optional, intent(in) :: Iout
      End Subroutine Write_Info_Eos_Transition

      Module Function EthTable(T, EoS) Result(Eth)
         !---- Arguments ----!
         real(kind=cp),  intent(in) :: T    ! Temperature
         type(Eos_Type), intent(in) :: EoS  ! Eos Parameters
          real(kind=cp)              :: Eth
      End Function EthTable

   End Interface

Contains

   !!--++
   !!--++ FUNCTION EOS_TO_VEC
   !!--++
   !!--++ Copy parameters from EoS type to a vector
   !!--++
   !!--++ Date: 28/02/2013
   !!
   Function EoS_to_Vec(Eos) Result(Vec)
      !---- Arguments ----!
      type(EoS_Type),              intent(in)  :: Eos
      real(kind=cp), dimension(N_EOSPAR)       :: Vec

      !> Init
      vec=0.0_cp

      !> Copy everything 1 to 1 as default
      vec=eos%params

      !> adjust any linearr values as necessary
      if (eos%linear) then
         vec(1)=eos%params(1)**3.0_cp
         vec(2:4)=eos%params(2:4)/3.0_cp

         select case(eos%icross)
            case (1)
               vec(8)=eos%params(8)/3.0_cp

            case (2)
               vec(8:9)=eos%params(8:9)
         end select

         select case(eos%itherm)                 ! thermal expansion terms
            case(1:3)
               vec(10:12)=eos%params(10:12)*3.0_cp

            case(4,5,6,8)
               vec(10)=eos%params(10)*3.0_cp
               vec(11)=eos%params(11)
         end select

         !> phase transition: no changes required for linear
      end if
   End Function EoS_to_Vec

   !!----
   !!---- SUBROUTINE DEF_CRYSTAL_SYSTEM
   !!----
   !!----   Either sets cell parameters to conform to specifed system Or, if no system specified,
   !!----   tries to determine system if all cell parameters provided
   !!----
   !!---- Update: 30/10/2020
   !!
   Subroutine Def_Crystal_System(Dat)
      !---- Arguments ----!
      type (eos_data_list_type),  intent(in out)   :: dat  ! data structure

      !---- Local Variables ----!
      character(len=40)       :: Family, SystemC, system
      character(len=1)        :: Symbol,U
      integer                 :: i,ndat
      type(Cell_G_Type)       :: ncell

      !> Check
      ndat=dat%n
      if (ndat <= 0) return

      !> local copies from data construct
      system=dat%system

      if (len_trim(system) > 0) then
         system=adjustl(system)
         select case (u_case(system(1:4)))
            case ('MONO')
               i=index(system,'-un')
               U='B'        !default is b-unique
               if (i > 0)U=U_case(system(i-1:i-1))
               select case(U)
                  case('A')
                     !> beta = gamma = 90
                     dat%eosd(1:ndat)%ang(2)=90.0
                     dat%eosd(1:ndat)%ang(3)=90.0
                     dat%eosd(1:ndat)%siga(2)=0.0
                     dat%eosd(1:ndat)%siga(3)=0.0
                  case('B')
                     !> alpha = gamma = 90
                     dat%eosd(1:ndat)%ang(1)=90.0
                     dat%eosd(1:ndat)%ang(3)=90.0
                     dat%eosd(1:ndat)%siga(1)=0.0
                     dat%eosd(1:ndat)%siga(3)=0.0
                  case('C')
                     !> alpha = beta = 90
                     dat%eosd(1:ndat)%ang(1)=90.0
                     dat%eosd(1:ndat)%ang(2)=90.0
                     dat%eosd(1:ndat)%siga(1)=0.0
                     dat%eosd(1:ndat)%siga(2)=0.0
               end select

            case ('ORTH')
               !> Angles =90
               dat%eosd(1:ndat)%ang(1)=90.0
               dat%eosd(1:ndat)%ang(2)=90.0
               dat%eosd(1:ndat)%ang(3)=90.0
               dat%eosd(1:ndat)%siga(1)=0.0
               dat%eosd(1:ndat)%siga(2)=0.0
               dat%eosd(1:ndat)%siga(3)=0.0

            case ('TETR')
               !> Angles =90
               dat%eosd(1:ndat)%ang(1)=90.0
               dat%eosd(1:ndat)%ang(2)=90.0
               dat%eosd(1:ndat)%ang(3)=90.0
               dat%eosd(1:ndat)%siga(1)=0.0
               dat%eosd(1:ndat)%siga(2)=0.0
               dat%eosd(1:ndat)%siga(3)=0.0

               !> b=a
               dat%eosd(1:ndat)%cell(2)=dat%eosd(1:ndat)%cell(1)
               dat%eosd(1:ndat)%sigc(2)=dat%eosd(1:ndat)%sigc(1)

            case ('TRIG','HEXA')
               !> Angles alpha=beta=90, gamma=120
               dat%eosd(1:ndat)%ang(1)= 90.0
               dat%eosd(1:ndat)%ang(2)= 90.0
               dat%eosd(1:ndat)%ang(3)  =120.0
               dat%eosd(1:ndat)%siga(1)=0.0
               dat%eosd(1:ndat)%siga(2)=0.0
               dat%eosd(1:ndat)%siga(3)=0.0

               !> b=a
               dat%eosd(1:ndat)%cell(2)=dat%eosd(1:ndat)%cell(1)
               dat%eosd(1:ndat)%sigc(2)=dat%eosd(1:ndat)%sigc(1)

            case ('RHOM')
               !> alpha = beta = gamma
               dat%eosd(1:ndat)%ang(2)=dat%eosd(1:ndat)%ang(1)
               dat%eosd(1:ndat)%ang(3)=dat%eosd(1:ndat)%ang(1)
               dat%eosd(1:ndat)%siga(2)=dat%eosd(1:ndat)%siga(1)
               dat%eosd(1:ndat)%siga(3)=dat%eosd(1:ndat)%siga(1)

               !> a = b = c
               dat%eosd(1:ndat)%cell(2)=dat%eosd(1:ndat)%cell(1)
               dat%eosd(1:ndat)%cell(3)=dat%eosd(1:ndat)%cell(1)
               dat%eosd(1:ndat)%sigc(2)=dat%eosd(1:ndat)%sigc(1)
               dat%eosd(1:ndat)%sigc(3)=dat%eosd(1:ndat)%sigc(1)

            case ('CUBI')
               !> Angles =90
               dat%eosd(1:ndat)%ang(1)=90.0
               dat%eosd(1:ndat)%ang(2)=90.0
               dat%eosd(1:ndat)%ang(3)=90.0
               dat%eosd(1:ndat)%siga(1)=0.0
               dat%eosd(1:ndat)%siga(2)=0.0
               dat%eosd(1:ndat)%siga(3)=0.0

               !> a=b=c: Modified RJA 14 Jan to handle case if V is supplied, but 'a' is not
               do i=1,ndat
                  if (dat%eosd(i)%cell(1) < tiny(0.0) ) then
                     dat%eosd(i)%cell(1)=dat%eosd(i)%v**(1.0_cp/3.0_cp)
                     dat%eosd(i)%sigc(1)=dat%eosd(i)%sigv/3.0_cp/(dat%eosd(i)%cell(1)**2.0_cp)
                  end if
                  dat%eosd(i)%cell(2)=dat%eosd(i)%cell(1)
                  dat%eosd(i)%cell(3)=dat%eosd(i)%cell(1)
                  dat%eosd(i)%sigc(2)=dat%eosd(i)%sigc(1)
                  dat%eosd(i)%sigc(3)=dat%eosd(i)%sigc(1)
               end do
         end select

      else
         !> checking cell parameters
         system=' '
         if (any(dat%eosd(1)%cell <= 0.1)) return
         if (any(dat%eosd(1)%ang <= 0.5)) return

         !> Obtaining system from cell parameters
         ncell%cell=dat%eosd(1)%cell
         ncell%ang =dat%eosd(1)%ang
         call Get_Cryst_Family(ncell,Family,Symbol,System)
         system=adjustl(system)
         if (len_trim(system) <= 0) then
            call set_error(1, "Cell values for first data in the data file are inconsistent")
            return
         end if

         !> Now check if all data the same. If they are not, set system=' ' because there may be a transition
         do i=2,ndat
            ncell%cell=dat%eosd(i)%cell
            ncell%ang =dat%eosd(i)%ang
            call Get_Cryst_Family(ncell,Family,Symbol,SystemC)
            systemC=adjustl(systemc)

            if (systemC /= System) then
               dat%system=' '
               return
            end if

         end do
      end if

   End Subroutine Def_Crystal_System

   !!--++
   !!--++ SUBROUTINE VEC_TO_EOS
   !!--++
   !!--++ Pass values fron a vector to respective EoS parameter
   !!--++
   !!--++ Date: 28/02/2013
   !!
   Subroutine Vec_to_EoS(Vec, Eos)
      !---- Arguments ----!
      real(kind=cp), dimension(:), intent(in)     :: Vec
      type(EoS_Type),              intent(in out) :: Eos

      !> Copy vec to eosparams as 1 to 1 as default
      EoS%params=vec

      if (EoS%linear) then
         EoS%params(1)=vec(1)**(1.0_cp/3.0_cp)

         EoS%params(2:4)=vec(2:4)*3.0_cp          ! recheck cross terms******


         select case(EoS%itherm)                 ! thermal expansion terms
            case (1,2,3)
               EoS%params(10:12)=vec(10:12)/3.0_cp

            case (4,5,6)
               EoS%params(10)=vec(10)/3.0_cp
         end select

         select case(EoS%icross)
            case (1)
               EoS%params(8)=vec(8)*3.0_cp

            case (2)
               EoS%params(8:9)=vec(8:9)
         end select

      end if

   End Subroutine Vec_to_EoS

   !!--++
   !!--++ SET_VOLUME_FROM_CELL
   !!--++   Sets V and esd(V) from cell parameter data for all data items in dat
   !!--++   If V is present in first data item, no esd is calculated
   !!--++
   !!--++ 17/07/2015
   !!
   Subroutine Set_Volume_from_Cell(Dat)
      !---- Arguments ----!
      type (eos_data_list_type),  intent(in out)  :: dat  ! data structure

      !---- Local Variables ----!
      integer           :: i
      type(Cell_G_type) :: cell

      !> Check
      if (dat%eosd(1)%v > 0.0) return
      if (any(dat%eosd(1)%cell < 0.5)) return

      !> Calculations
      do i=1,dat%n
        cell%cell=dat%eosd(i)%cell
        cell%scell=dat%eosd(i)%sigc
        cell%ang=dat%eosd(i)%ang
        cell%sang=dat%eosd(i)%siga
        dat%eosd(i)%v=Volume_from_Cell(cell%cell,cell%ang)
        dat%eosd(i)%sigv=SigmaV_From_Cell(Cell)
        ! call Volume_Sigma_from_Cell(dat%eosd(i)%cell,dat%eosd(i)%ang,dat%eosd(i)%sigc, &
        !                             dat%eosd(i)%siga,dat%eosd(i)%v,dat%eosd(i)%sigv)
      end do
   End Subroutine Set_Volume_from_Cell

End Module CFML_EoS
