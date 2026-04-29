!!-------------------------------------------------------
!!---- Crystallographic Fortran Modules Library (CrysFML08)
!!-------------------------------------------------------
!!---- The CrysFML project is distributed under LGPL. In agreement with the
!!---- Intergovernmental Convention of the ILL, this software cannot be used
!!---- in military applications. This module is similar to the CrysFML (F95)
!!---- module of the same name. It has been adapted to be used with the new
!!---- version of CrysFML and split in submodules (March 2023, JRC)
!!----
!!---- Copyright (C) 1999-2023  Institut Laue-Langevin (ILL), Grenoble, FRANCE
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
!!---- MODULE: CFML_Keywords_Code_Parser
!!----   INFO: Refinable Codes for Parameters
!!----   The procedures gathered in this module have as purpose the control of refinable
!!----   parameters in optimization techniques (least squares, simulated annealing, etc).
!!----   Many derived types in CrysFML have as component the code of a particular parameter
!!----   indicating that it may be refined in an optimization procedure with an associated
!!----   multiplier. In this module we associate a name to the particular parameter and we
!!----   add the parameter value, the code, the multiplier, the name, the range, step and
!!----   boundary conditions to general arrays of this module called V_vec, V_vec_std, V_name,
!!----   V_list, V_bounds and V_shift. The code of each derived type points to a particular
!!----   element of the V-arrays. The last refined parameter corresponds to the global variable
!!----   NP_refi that is updated each time a VARY or GVARY directive is processed.
!!----   There are procedures for deleting refined parameters by processing FIX or GFIX
!!----   instruction in the input CFL file.
!!----   For non-atomic parameters there are no specific derived types in CrysFML. In this
!!----   module we provide a simple derived type called "NonAtomic_Parameter_Type" containing
!!----   two real values, an integer code, a multiplier and a name. We provide also a derived
!!----   type "NonAtomic_Parameter_List_Type" containing an allocatable array of objects with
!!----   "NonAtomic_Parameter_Type", "par" and an integer "npar" with the effective number
!!----   of allocated elements. It is responsibility of the user of this module to allocate
!!----   an fill up an object of derived type "NonAtomic_Parameter_List_Type", before use of
!!----   the procedures of the present module. A call to Allocate_VParam with the maximum number
!!----   of expected refinable parameters is needed before the interpretation of VARY/GVARY (or
!!----   FIX/GFIX) instructions if the input CFL file(s)
!!----
!!----
!!---- HISTORY
!!----    Update: 07/03/2011
!!----    Modifications: 12/2011
!!----    Modifications: 02/2012
!!----    Modifications: 10/2013 (Introduction of non-atomic parameters, correction of bugs,
!!----                            and merging of some procedures, JRC)
!!----    Modifications: 03/2033 (Making the whole module compatible with CrysFML08, JRC)
!!----
!!----
!!---- DEPENDENCIES
!!----
!!----
!!---- VARIABLES
!!--..    Types
!!----    ANGLE_RESTRAINT_TYPE
!!----    DISTANCE_RESTRAINT_TYPE
!!----    NONATOMIC_PARAMETER_TYPE
!!----    NONATOMIC_PARAMETER_LIST_TYPE
!!----    TORSION_RESTRAINT_TYPE
!!--..
!!----    ANG_REST
!!----    CODE_NAM
!!----    mCODE_NAM                    !NEW 12/11
!!----    DIS_REST
!!--++    NCODE                        [Private]
!!--++    NKEY                         [Private]
!!--++    mNCODE                       [Private] !NEW 12/11
!!--++    mNKEY                        [Private] !NEW 12/11
!!----    KEY_CODE
!!----    KEY_mCODE                    !NEW 12/11
!!----    NP_CONS
!!----    NP_MAX
!!----    NP_REFI
!!----    NP_REST_ANG
!!----    NP_REST_DIS
!!----    NP_REST_TOR
!!----    TORSION_REST
!!----    V_BCON
!!----    V_BOUNDS
!!----    V_LIST
!!----    V_NAME
!!----    V_VEC
!!----    V_VEC_STD                    !NEW 11/13 (JRC)
!!----    V_SHIFT
!!----
!!---- PUBLIC PROCEDURES
!!----    Functions:
!!----
!!----    Subroutines:
!!----       ALLOCATE_RESTPARAM
!!----       ALLOCATE_VPARAM
!!----       DELETE_ELEMENT_IN_VARRAYS   [Private]    !NEW 11/13
!!--++       DELETE_REFCODES             [Private]
!!--++       DELETE_REFCODES_FATOM       [Overloaded]
!!--++       DELETE_REFCODES_FmATOM      [Overloaded] !NEW 12/11
!!--++       DELETE_REFCODES_MOLCRYS     [Overloaded]
!!--++       DELETE_REFCODES_MOLEC       [Overloaded]
!!--++       DELETE_REFCODES_MAGDOM                   !NEW 02/12
!!--++
!!--++       FILL_REFCODES_FATOM         [Overloaded]
!!--++       FILL_REFCODES_FmATOM        [Overloaded] !NEW 12/11
!!--++       FILL_REFCODES_MOLCRYS       [Overloaded]
!!--++       FILL_REFCODES_MOLEC         [Overloaded]
!!--++       FILL_REFCODES_MAGDOM                     !NEW 02/12
!!--++       FILL_REFGCODES              [Private]    !NEW 11/13
!!--++       GET_ATOMBET_CTR             [Private]
!!--++       GET_ATOMPOS_CTR             [Private]
!!--++       GET_CONCODES_LINE           [Private]
!!--++       GET_CONCODES_LINE_FATOM     [Overloaded]
!!--++       GET_CONCODES_LINE_FmATOM    [Overloaded] !NEW 12/11
!!--++       GET_CONCODES_LINE_MOLCRYS   [Overloaded]
!!--++       GET_CONCODES_LINE_MOLEC     [Overloaded]
!!--++       GET_CONCODES_LINE_MAGDOM                 !NEW 02/12
!!--++       GET_REFCODES_LINE           [Private]
!!--++       GET_REFCODES_LINE_FATOM     [Overloaded]
!!--++       GET_REFCODES_LINE_FmATOM    [Overloaded] !NEW 12/11
!!--++       GET_REFCODES_LINE_MOLCRYS   [Overloaded]
!!--++       GET_REFCODES_LINE_MOLEC     [Overloaded]
!!--++       GET_REFCODES_LINE_MAGDOM                 !NEW 02/12
!!--++       GET_REFGCODES_LINE          [Private]    !NEW 11/13
!!----       GET_RESTANG_LINE
!!----       GET_RESTDIS_LINE
!!----       GET_RESTTOR_LINE
!!----       INIT_Err_CFML%Flag!!----       INIT_REFCODES                            !NEW 11/13 (merged with optional arguments, JRC)
!!----       READ_REFCODES_FILE
!!--++       READ_REFCODES_FILE_FATOM    [Overloaded]
!!--++       READ_REFCODES_FILE_FmATOM   [Overloaded] !NEW 12/11 !replaced 02/12
!!--++       READ_REFCODES_FILE_MagStr                !NEW 02/12
!!--++       READ_REFCODES_FILE_MOLCRYS  [Overloaded]
!!--++       READ_REFCODES_FILE_MOLEC    [Overloaded]
!!----       READ_REFGCODES_FILE                      !NEW 11/13 (JRC, non atomic parameters)
!!--++       SPLIT_OPERATIONS            [Private]
!!--++       SPLIT_mOPERATIONS           [Private]    !NEW 12/11
!!----       VSTATE_TO_ATOMSPAR
!!--++       VSTATE_TO_ATOMSPAR_FATOM    [Overloaded]
!!--++       VSTATE_TO_ATOMSPAR_FmATOM   [Overloaded] !NEW 12/11 !modified 02/12 to include MagDom
!!--++       VSTATE_TO_ATOMSPAR_MOLCRYS  [Overloaded]
!!--++       VSTATE_TO_ATOMSPAR_MOLEC    [Overloaded]
!!----       VSTATE_TO_MODELPAR                       !NEW 11/13
!!----       WRITE_INFO_REFCODES
!!--++       WRITE_INFO_REFCODES_FATOM   [Overloaded]
!!--++       WRITE_INFO_REFCODES_FmATOM  [Overloaded] !NEW 12/11 !replaced 02/12
!!--++       WRITE_INFO_REFCODES_Magstr               !NEW 02/12
!!--++       WRITE_INFO_REFCODES_MOLCRYS [Overloaded]
!!--++       WRITE_INFO_REFCODES_MOLEC   [Overloaded]
!!----       WRITE_INFO_REFGCODES                     !NEW 11/13
!!----       WRITE_INFO_REFPARAMS
!!----       WRITE_RESTRAINTS_OBSCALC
!!----
!!
 Module CFML_Keywords_Code_Parser
    !---- Modules ----!
    Use CFML_Rational
    Use CFML_GlobalDeps,    only: cp, clear_error, Err_CFML
    Use CFML_Maths,         only: Sort
    Use CFML_Strings,       only: Cut_string, U_Case, L_Case, Get_words, Get_Num, File_List_Type
    Use CFML_gSpaceGroups,  only: Spg_Type, Symm_Oper_Type, Get_Stabilizer, Get_Symb_from_OP, &
                                  Get_OP_from_Symb, Symmetry_symbol,Read_Symtrans_Code,       &
                                  Get_AtomPos_CTR,Get_AtomBet_CTR, Get_Moment_CTR, Get_TFourier_CTR, &
                                  Get_Moment_CTR_Wigner
    Use CFML_Atoms,         only: AtList_Type, mAtom_list_Type, Atm_Std_Type,Atm_Ref_Type, ModAtm_Ref_Type
    Use CFML_Molecules,     only: Molecule_Type, MolCrystal_Type
    Use CFML_kvec_Symmetry, only: Get_SymSymb, Read_Xsym, MagSymm_k_Type, Magnetic_Domain_type

    !---- Variables ----!
    implicit none

    private

    !---- List of public functions ----!

    !---- List of public subroutines ----!
    public :: Allocate_VParam, Init_RefCodes, Read_RefCodes_File, VState_to_AtomsPar,  &
              Write_Info_RefCodes, Get_RestAng_Line, Get_RestDis_Line, Get_RestTor_Line, &
              Allocate_RestParam, Write_Restraints_ObsCalc, &
              Write_Info_RefParams, Read_RefGCodes_File, Write_Info_RefGCodes, &
              VState_to_ModelPar

    !---- List of private functions ----!

    !---- List of private subroutines ----!
    private :: Delete_RefCodes,   &
               Delete_RefCodes_FAtom, Delete_RefCodes_FmAtom, Delete_RefCodes_Molcrys,          &
               Delete_RefCodes_Molec, Delete_RefCodes_Magdom, &
               Fill_RefCodes,Fill_RefGCodes,     &
               Fill_RefCodes_FAtom, Fill_RefCodes_FmAtom, Fill_RefCodes_Molcrys,                &
               Fill_RefCodes_Molec, Fill_RefCodes_Magdom,  &
               Get_ConCodes_Line, &
               Get_ConCodes_Line_FAtom, Get_ConCodes_Line_FmAtom, Get_ConCodes_Line_Molcrys,    &
               Get_ConCodes_Line_Molec, Get_ConCodes_Line_Magdom, &
               Get_RefCodes_Line,Get_RefGCodes_Line, &
               Get_RefCodes_Line_FAtom, Get_RefCodes_Line_FmAtom, Get_RefCodes_Line_Molcrys,    &
               Get_RefCodes_Line_Molec, Get_RefCodes_Line_Magdom, &
               Read_RefCodes_File_FAtom, Read_RefCodes_File_MagStr, Read_RefCodes_File_Molcrys, &
               Read_RefCodes_File_Molec, &
               Split_Operations, Split_mOperations, &
               VState_to_AtomsPar_FAtom, VState_to_AtomsPar_FmAtom, VState_to_AtomsPar_Molcrys, &
               VState_to_AtomsPar_Molec, &
               Write_Info_RefCodes_FAtom,Write_Info_RefCodes_MagStr,Write_Info_RefCodes_Molcrys,&
               Write_Info_RefCodes_Molec

    !---- Definitions ----!

    !!----
    !!---- TYPE :: ANGLE_RESTRAINT_TYPE
    !!--..
    !!---- Type, public :: Angle_Restraint_Type
    !!----    real(kind=cp)                 :: AObs
    !!----    real(kind=cp)                 :: ACalc
    !!----    real(kind=cp)                 :: Sigma
    !!----    integer,dimension(3)          :: P
    !!----    character(len=8),dimension(2) :: STCode
    !!---- End Type Angle_Restraint_Type
    !!----
    !!---- Update: April - 2005
    !!
    Type, public :: Angle_Restraint_Type
      real(kind=cp)                 :: AObs
      real(kind=cp)                 :: ACalc
      real(kind=cp)                 :: Sigma
      integer,dimension(3)          :: P
      character(len=8),dimension(2) :: STCode
    End Type Angle_Restraint_Type

    !!----
    !!---- TYPE :: DISTANCE_RESTRAINT_TYPE
    !!--..
    !!---- Type, public :: Distance_Restraint_Type
    !!----    real(kind=cp)        :: DObs
    !!----    real(kind=cp)        :: DCalc
    !!----    real(kind=cp)        :: Sigma
    !!----    integer,dimension(2) :: P
    !!----    character(len=8)     :: STCode    ! _N.ABC
    !!---- End Type Distance_Restraint_Type
    !!----
    !!---- Update: April - 2005
    !!
    Type, public :: Distance_Restraint_Type
      real(kind=cp)        :: DObs
      real(kind=cp)        :: DCalc
      real(kind=cp)        :: Sigma
      integer,dimension(2) :: P
      character(len=8)     :: STCode
    End Type Distance_Restraint_Type

    !!----
    !!---- TYPE :: NONATOMIC_PARAMETER_TYPE
    !!--..
    !!---- Type, public :: Nonatomic_Parameter_Type
    !!----    real(kind=cp)        :: Value
    !!----    real(kind=cp)        :: Sigma
    !!----    integer              :: Lcode
    !!----    real(kind=cp)        :: multip
    !!----    character(len=20)    :: Nam
    !!---- End Type Nonatomic_Parameter_Type
    !!----
    !!---- Update: November 1 - 2013
    !!
    Type, public :: Nonatomic_Parameter_Type
       real(kind=cp)        :: Value
       real(kind=cp)        :: Sigma
       integer              :: Lcode
       real(kind=cp)        :: multip
       character(len=20)    :: Nam
    End Type Nonatomic_Parameter_Type

    !!----
    !!---- TYPE :: NONATOMIC_PARAMETER_LIST_TYPE
    !!--..
    !!---- Type, public :: Nonatomic_Parameter_List_Type
    !!----    Integer                                                       :: npar
    !!----    Type(Nonatomic_Parameter_Type),dimension(:),allocatable :: par
    !!---- End Type Nonatomic_Parameter_List_Type
    !!----
    !!---- The user of this derived type must allocate an fill a type of this kind for his(her) own
    !!---- his(her) own problem. An object of this type is needed to fill the V_vec and
    !!---- accompanying arrays.
    !!----
    !!---- Update: November 1 - 2013
    !!
    Type, public :: Nonatomic_Parameter_List_Type
       Integer                                                 :: npar
       Type(Nonatomic_Parameter_Type),dimension(:),allocatable :: par
    End Type Nonatomic_Parameter_List_Type

    !!----
    !!---- TYPE :: TORSION_RESTRAINT_TYPE
    !!--..
    !!---- Type, public :: Torsion_Restraint_Type
    !!----    real(kind=cp)                 :: TObs
    !!----    real(kind=cp)                 :: TCalc
    !!----    real(kind=cp)                 :: Sigma
    !!----    integer,dimension(4)          :: P
    !!----    character(len=8),dimension(3) :: STCode
    !!---- End Type Torsion_Restraint_Type
    !!----
    !!---- Update: April - 2005
    !!
    Type, public :: Torsion_Restraint_Type
      real(kind=cp)                 :: TObs
      real(kind=cp)                 :: TCalc
      real(kind=cp)                 :: Sigma
      integer,dimension(4)          :: P
      character(len=8),dimension(3) :: STCode
    End Type Torsion_Restraint_Type

    !!----
    !!---- ANG_REST
    !!----    type(Angle_Restraint_Type), public, dimension(:), allocatable :: Ang_rest
    !!----
    !!---- Relations for Angle Restraints
    !!----
    !!---- Update: March - 2005
    !!
    type(Angle_Restraint_Type), public, dimension(:), allocatable :: Ang_Rest

    !!--++
    !!--++ NCODE
    !!--++    integer, private :: NCode
    !!--++
    !!--++    Number of Code variables
    !!--++
    !!--++ Update: March - 2005
    !!
    integer, private, parameter :: NCode=52

    !!----
    !!---- CODE_NAM
    !!----    character(len=*), dimension(NCode), public, parameter :: Code_Nam
    !!----
    !!----    Variable for treatement Codes
    !!--..    21 debe ser igual a NCode
    !!----
    !!---- Update: March - 2005
    !!
    character(len=*), dimension(NCode), public, parameter :: &
                   Code_Nam=[  "X_     ","Y_     ","Z_     ","Mx_    ","My_    ","Mz_    ",  &
                               "B_     ","Occ_   ","B11_   ","B22_   ","B33_   ","B12_   ",  &
                               "B13_   ","B23_   ","Bns_   ","Xc_    ","Yc_    ","Zc_    ",  &
                               "Theta_ ","Phi_   ","Chi_   ","Th_L_  ","Th_T_  ","Th_S_  ",  &
                               "Ocos_  ","Osin_  ","Bcos_  ","Bsin_  ",                      &
                               "Mxcos_ ","Mycos_ ","Mzcos_ ","Mxsin_ ","Mysin_ ","Mzsin_ ",  &
                               "Dxcos_ ","Dycos_ ","Dzcos_ ","Dxsin_ ","Dysin_ ","Dzsin_ ",  &
                               "U11cos_","U22cos_","U33cos_","U12cos_","U13cos_","U23cos_",  &
                               "U11sin_","U22sin_","U33sin_","U12sin_","U13sin_","U23sin_"]
    !!--++
    !!--++ NCODE
    !!--++    integer, private :: NGCode
    !!--++
    !!--++    Number of Codes for non atomic variables
    !!--++
    !!--++ Update: November - 2013
    !!
    integer, private, parameter :: NGCode=87

    !!----
    !!---- GCODE_NAM
    !!----    character(len=*), dimension(NGCode), public, parameter :: GCode_Nam
    !!----
    !!----    Variable for treatement of GCodes
    !!--..
    !!----
    !!---- Update: November - 2013, February 2014
    !!
    character(len=*), dimension(NGCode), public, parameter :: &
                      GCode_Nam =["Scalef ","Cell-a ","Cell-b ", &
                                  "Cell-c ","C-alpha","C-beta ", &
                                  "C-gamma","Cell   ","Up     ","Vp     ", &
                                  "Wp     ","Xp     ","Yp     ","Xfract ", &
                                  "Size   ","Gsize  ","Strain ", &
                                  "LStrain","Qbroad ","Qdamp  ", &
                                  "delta1 ","delta2 ","Lratio ","Sharpf ", &
                                  "Sratio ","Zero   ","Diff1  ", &
                                  "Diff2  ","eta    ","sig2   ", &
                                  "sig1   ","sig0   ","gamm2  ", &
                                  "gamm1  ","gamm0  ","alph0  ", &
                                  "alph1  ","beta0  ","beta1  ", &
                                  "kappa  ","Bover  ","Abs1   ", &
                                  "Abs2   ","Extinct","BCExt1 ", &
                                  "BCExt2 ","Ext11  ","Ext22  ", &
                                  "Ext33  ","Ext12  ","Ext13  ", &
                                  "Ext23  ","S400   ","S040   ", &
                                  "S004   ","S220   ","S202   ", &
                                  "S022   ","S211   ","S121   ", &
                                  "S112   ","S310   ","S301   ", &
                                  "S130   ","S103   ","S013   ", &
                                  "S031   ","Sizh2  ","Sizk2  ", &
                                  "Sizl2  ","Siz2hk ","Siz2hl ", &
                                  "Siz2kl ","kx     ","ky     ", &
                                  "kz     ","bkg    ","bkg_   ", &
                                  "kx_    ","ky_    ","kz_    ", &
                                  "Sc_    ","sycos  ","sysin  ", &
                                  "Dom_   ","sycos_ ","sysin_ "]
    integer, private, parameter :: mNCode=25

    !!----
    !!---- mCODE_NAM
    !!----    character(len=*), dimension(mNCode), public, parameter :: mCode_Nam
    !!----
    !!----    Variable for treatement Codes
    !!----    mag clone of CODE_NAM.
    !!----    The item "K_", has been added to take into account the number of the propagation vector
    !!----    to which the other parameters refer. Giving "K_3" together with another keyword means that
    !!----    the keyword refers to the propagation vector number 3.
    !!---- Created: December - 2011
    !!
    character(len=*),dimension(mNCode), public, parameter :: mCode_Nam=(/"Rx_   ","Ry_   ","Rz_   ",&
                                                                         "Ix_   ","Iy_   ","Iz_   ",&
                                                                         "Rm_   ","Rphi_ ","Rth_  ",&
                                                                         "Im_   ","Iphi_ ","Ith_  ",&
                                                                         "MagPh_",                  &
                                                                         "C1_   ","C2_   ","C3_   ",&
                                                                         "C4_   ","C5_   ","C6_   ",&
                                                                         "C7_   ","C8_   ","C9_   ",&
                                                                         "C10_  ","C11_  ","C12_  "/)

    !!----
    !!---- DIS_REST
    !!----    type(Distance_Restraint_Type), public, dimension(:), allocatable :: Dis_Rest
    !!----
    !!---- Relations for Angle Restraints
    !!----
    !!---- Update: March - 2005
    !!
    type(Distance_Restraint_Type), public, dimension(:), allocatable :: Dis_Rest


    !!--++
    !!--++ NKEY
    !!--++    integer, public :: NKey
    !!--++
    !!--++    Number of Keys variables
    !!--++
    !!--++ Update: March - 2005
    !!
    integer, private, parameter :: NKey=17

    !!----
    !!---- KEY_CODE
    !!----    character(len=*), dimension(NKey), public, parameter :: key_Code
    !!----
    !!----     Keyv codes defined in the module
    !!----
    !!---- Update: March - 2005
    !!
    character(len=*), dimension(Nkey), public, parameter :: Key_Code=["XYZ ","OCC ","BIS ","BAN ", &
                                                                      "ALL ","CEN ","ORI ","THE ", &
                                                                      "Mxyz","Mcos","Msin","Dcos", &
                                                                      "Dsin","Bcos","Bsin","Ucos","Usin"]
    integer, private, parameter :: mNKey=4

    !!----
    !!---- KEY_mCODE
    !!----    character(len=*), dimension(mNKey), public, parameter :: key_mCode
    !!----
    !!----    Keyv codes defined in the module
    !!----    mag clone of KEY_CODE
    !!---- Created: December - 2011
    !!
    character(len=*), dimension(mNkey), public, parameter :: Key_mCode=["Rxyz","Ixyz","Mxyz","Magd"]

    !!----
    !!---- NP_CONS
    !!----    integer, public :: NP_Cons
    !!----
    !!----    Number of Constraints relations
    !!----
    !!---- Update: March - 2005
    !!
    integer, public :: NP_Cons

    !!----
    !!---- NP_MAX
    !!----    integer, public :: NP_Max
    !!----
    !!----    Number of Maximum Parameters to Refine
    !!----
    !!---- Update: March - 2005
    !!
    integer, public :: NP_Max

    !!----
    !!---- NP_REFI
    !!----    integer, public :: NP_Refi
    !!----
    !!----    Number of Refinable Parameters
    !!----
    !!---- Update: March - 2005
    !!
    integer, public :: NP_Refi

    !!----
    !!---- NP_REST_ANG
    !!----    integer, save, public :: NP_Rest_Ang=0
    !!----
    !!----    Number of Angle Restraints relations
    !!----
    !!---- Update: March-2005, May-2015
    !!
    integer, save, public :: NP_Rest_Ang=0

    !!----
    !!---- NP_REST_DIS
    !!----    integer, save, public :: NP_Rest_Dis=0
    !!----
    !!----    Number of Distance Restraints relations
    !!----
    !!---- Update: March-2005, May-2015
    !!
    integer, save, public :: NP_Rest_Dis=0

    !!----
    !!---- NP_REST_TOR
    !!----    iinteger, save, public :: NP_Rest_Tor=0
    !!----
    !!----    Number of Torsion Restraints relations
    !!----
    !!---- Update: March - 2005, May-2015
    !!
    integer, save, public :: NP_Rest_Tor=0

    !!----
    !!---- TOR_REST
    !!----    type(Torsion_Restraint_Type), public, dimension(:), allocatable :: Tor_Rest
    !!----
    !!---- Relations for Torsion Angle Restraints
    !!----
    !!---- Update: March - 2005
    !!
    type(Torsion_Restraint_Type), public, dimension(:), allocatable :: Tor_Rest

    !!----
    !!---- V_BCON
    !!----    integer, public, dimension(:), allocatable :: V_BCon
    !!----
    !!----    Vector of Boundary Conditions
    !!----
    !!---- Update: March - 2005
    !!
    integer, public, dimension(:), allocatable :: V_BCon

    !!----
    !!---- V_BOUNDS
    !!----    real(kind=cp), public, dimension(:,:), allocatable :: V_Bounds
    !!----
    !!----    Vector of Lower, Upper limits and Step for Parameters
    !!----
    !!---- Update: March - 2005
    !!
    real(kind=cp), public, dimension(:,:),  allocatable :: V_Bounds

    !!----
    !!---- V_LIST
    !!----    integer, public, dimension(:), allocatable :: V_List
    !!----
    !!----    Vector of indices pointing to the parameter number
    !!----
    !!---- Update: March - 2005
    !!
    integer, public, dimension(:),  allocatable :: V_List

    !!----
    !!---- V_NAME
    !!----    character(len=20), public, dimension(:), allocatable :: V_Name
    !!----
    !!----    Vector of names for all refinable parameters (all parameters of the model)
    !!----
    !!---- Update: March - 2005
    !!
    character(len=20), public, dimension(:), allocatable :: V_Name

    !!----
    !!---- V_VEC
    !!----    real(kind=cp), public, dimension(:), allocatable :: V_Vec
    !!----
    !!----    Vector of refined parameters
    !!----
    !!---- Update: March - 2005
    !!

    real(kind=cp), public, dimension(:),    allocatable :: V_Vec
    !!----
    !!---- V_SAVE
    !!----    real(kind=cp), public, dimension(:), allocatable :: V_Save
    !!----
    !!----    Vector of Parameters saving previous values of parameters.
    !!----    This vector is handled by the calling program. It is only automatically
    !!----    allocated in this module by a call to Allocate_VParam.
    !!----
    !!---- Update: April - 2014
    !!

    real(kind=cp), public, dimension(:),    allocatable :: V_Save
    !!----
    !!---- V_VEC_std
    !!----    real(kind=cp), public, dimension(:), allocatable :: V_Vec_std
    !!----
    !!----    Standard deviations of the parameters
    !!----
    !!---- Update: November - 2013 (JRC)
    !!
    real(kind=cp), public, dimension(:),    allocatable :: V_Vec_std

    !!----
    !!---- V_SHIFT
    !!----    real(kind=cp), public, dimension(:), allocatable :: V_Shift
    !!----
    !!----    Vector of holding the shift of parameters
    !!----
    !!---- Update: March - 2005
    !!
    real(kind=cp), public, dimension(:),    allocatable :: V_Shift

    !---- Interfaces - Overloaded ----!
    Interface Allocate_RestParam
       Module Procedure Allocate_RestParam_Single
       Module Procedure Allocate_RestParam_Mult
    End Interface

    Interface Delete_RefCodes
       Module Procedure Delete_RefCodes_FAtom
       Module Procedure Delete_RefCodes_FmAtom
       Module Procedure Delete_RefCodes_Molcrys
       Module Procedure Delete_RefCodes_Molec
       Module Procedure Delete_RefCodes_Magdom
    End Interface

    Interface Fill_RefCodes
       Module Procedure Fill_RefCodes_FAtom
       Module Procedure Fill_RefCodes_FmAtom
       Module Procedure Fill_RefCodes_Molcrys
       Module Procedure Fill_RefCodes_Molec
       Module Procedure Fill_RefCodes_Magdom
    End Interface

    Interface Get_ConCodes_Line
       Module Procedure Get_ConCodes_Line_FAtom
       Module Procedure Get_ConCodes_Line_FmAtom
       Module Procedure Get_ConCodes_Line_Molcrys
       Module Procedure Get_ConCodes_Line_Molec
       Module Procedure Get_ConCodes_Line_Magdom
    End Interface

    Interface Get_RefCodes_Line
       Module Procedure Get_RefCodes_Line_FAtom
       Module Procedure Get_RefCodes_Line_FmAtom
       Module Procedure Get_RefCodes_Line_Molcrys
       Module Procedure Get_RefCodes_Line_Molec
       Module Procedure Get_RefCodes_Line_Magdom
    End Interface

    Interface Read_RefCodes_File
       Module Procedure Read_RefCodes_File_FAtom
       Module Procedure Read_RefCodes_File_MagStr
       Module Procedure Read_RefCodes_File_Molcrys
       Module Procedure Read_RefCodes_File_Molec
    End Interface

    Interface VState_to_AtomsPar
       Module Procedure VState_to_AtomsPar_FAtom
       Module Procedure VState_to_AtomsPar_FmAtom
       Module Procedure VState_to_AtomsPar_Molcrys
       Module Procedure VState_to_AtomsPar_Molec
    End Interface

    Interface Write_Info_RefCodes
       Module Procedure Write_Info_RefCodes_FAtom
       Module Procedure Write_Info_RefCodes_MagStr
       Module Procedure Write_Info_RefCodes_Molcrys
       Module Procedure Write_Info_RefCodes_Molec
    End Interface


    Interface

       Module Subroutine Allocate_RestParam_Single(file_dat)
          !---- Arguments ----!
          Type(file_list_type),     intent( in) :: file_dat
       End Subroutine Allocate_RestParam_Single

       Module Subroutine Allocate_RestParam_Mult(file_dat,ndis_rest,nang_rest,ntor_rest)
          !---- Arguments ----!
          Type(file_list_type), dimension(:), intent( in) :: file_dat
          integer,              dimension(:), intent(out) :: ndis_rest
          integer,              dimension(:), intent(out) :: nang_rest
          integer,              dimension(:), intent(out) :: ntor_rest
       End Subroutine Allocate_RestParam_Mult

       Module Subroutine Allocate_VParam(N)
          !---- Arguments ----!
          integer, intent(in) :: N
       End Subroutine Allocate_VParam

       Module Subroutine Delete_RefCodes_FAtom(N, FAtom)
          !---- Arguments ----!
          integer,              intent(in)     :: N
          type(AtList_Type),    intent(in out) :: FAtom
       End Subroutine Delete_RefCodes_FAtom

       Module Subroutine Delete_RefCodes_FmAtom(N, FmAtom)
          !---- Arguments ----!
          integer,               intent(in)     :: N
          type(mAtom_List_Type), intent(in out) :: FmAtom
       End Subroutine Delete_RefCodes_FmAtom

       Module Subroutine Delete_RefCodes_MolCrys(N,Molcrys)
          !---- Arguments ----!
          integer,               intent(in)     :: N
          type(MolCrystal_Type), intent(in out) :: MolCrys
       End Subroutine Delete_RefCodes_MolCrys

       Module Subroutine Delete_RefCodes_Molec(N,Molec)
          !---- Arguments ----!
          integer,             intent(in)     :: N
          type(molecule_type), intent(in out) :: Molec
       End Subroutine Delete_RefCodes_Molec

       Module Subroutine Delete_RefCodes_Magdom(N, Mag_Dom)
          !---- Arguments ----!
          integer,                    intent(in)     :: N
          type(Magnetic_Domain_type), intent(in out) :: Mag_Dom
       End Subroutine Delete_RefCodes_Magdom

       Module Subroutine Delete_RefGCodes(N, model)
          !---- Arguments ----!
          integer,                             intent(in)     :: N
          type(Nonatomic_Parameter_List_Type), intent(in out) :: model
       End Subroutine Delete_RefGCodes

       Module Subroutine Delete_element_in_Varrays(N)
          integer, intent(in) :: N
       End Subroutine Delete_element_in_Varrays

       Module Subroutine Fill_RefCodes_FAtom(Keyv,Dire,Na,Nb,Xl,Xu,Xs,Ic,FAtom,Spg)
          !---- Arguments ----!
          integer,                       intent(in)     :: Keyv
          character(len=*),              intent(in)     :: Dire
          integer,                       intent(in)     :: Na
          integer,                       intent(in)     :: Nb
          real(kind=cp),                 intent(in)     :: Xl
          real(kind=cp),                 intent(in)     :: Xu
          real(kind=cp),                 intent(in)     :: Xs
          integer,                       intent(in)     :: Ic
          type(AtList_Type),             intent(in out) :: FAtom
          type(SPG_Type),                intent(in)     :: Spg
       End Subroutine Fill_RefCodes_FAtom

       Module Subroutine Fill_RefCodes_FmAtom(Keyv,Dire,Na,Nb,Xl,Xu,Xs,Ic,ik,FmAtom)
          !---- Arguments ----!
          integer,                       intent(in)     :: Keyv
          character(len=*),              intent(in)     :: Dire
          integer,                       intent(in)     :: Na
          integer,                       intent(in)     :: Nb
          real(kind=cp),                 intent(in)     :: Xl
          real(kind=cp),                 intent(in)     :: Xu
          real(kind=cp),                 intent(in)     :: Xs
          integer,                       intent(in)     :: Ic
          integer,                       intent(in)     :: ik
          type(mAtom_List_Type),         intent(in out) :: FmAtom
       End Subroutine Fill_RefCodes_FmAtom

       Module Subroutine Fill_RefCodes_Molcrys(Keyv,Dire,Na,Nb,Xl,Xu,Xs,Ic,Molcrys,Nmol)
          !---- Arguments ----!
          integer,                      intent(in)     :: Keyv
          character(len=*),             intent(in)     :: Dire
          integer,                      intent(in)     :: Na
          integer,                      intent(in)     :: Nb
          real(kind=cp),                intent(in)     :: Xl
          real(kind=cp),                intent(in)     :: Xu
          real(kind=cp),                intent(in)     :: Xs
          integer,                      intent(in)     :: Ic
          type(MolCrystal_Type),        intent(in out) :: MolCrys
          integer,                      intent(in)     :: NMol
       End Subroutine Fill_RefCodes_Molcrys

       Module Subroutine Fill_RefCodes_Molec(Keyv,Dire,Na,Nb,Xl,Xu,Xs,Ic,Molec,Spg)
          !---- Arguments ----!
          integer,                      intent(in)     :: Keyv
          character(len=*),             intent(in)     :: Dire
          integer,                      intent(in)     :: Na
          integer,                      intent(in)     :: Nb
          real(kind=cp),                intent(in)     :: Xl
          real(kind=cp),                intent(in)     :: Xu
          real(kind=cp),                intent(in)     :: Xs
          integer,                      intent(in)     :: Ic
          type(molecule_type),          intent(in out) :: Molec
          type(SPG_Type),               intent(in)     :: Spg
       End Subroutine Fill_RefCodes_Molec

       Module Subroutine Fill_RefCodes_Magdom(Keyv,Dire,Na,Nb,Xl,Xu,Xs,Ic,Mag_dom)
          !---- Arguments ----!
          integer,                       intent(in)     :: Keyv
          character(len=*),              intent(in)     :: Dire
          integer,                       intent(in)     :: Na
          integer,                       intent(in)     :: Nb
          real(kind=cp),                 intent(in)     :: Xl
          real(kind=cp),                 intent(in)     :: Xu
          real(kind=cp),                 intent(in)     :: Xs
          integer,                       intent(in)     :: Ic
          type(Magnetic_Domain_type),    intent(in out) :: Mag_dom
       End Subroutine Fill_RefCodes_Magdom

       Module Subroutine Fill_RefGCodes(Keyv,Dire,Namp,Xl,Xu,Xs,Ic,model,sys,Iphas)
          integer,                             intent(in)     :: Keyv !0 => nb as below,
          character(len=*),                    intent(in)     :: Dire !GVar of GFix
          character(len=*),                    intent(in)     :: Namp !Name of the parameter to be refined or fixed
          real(kind=cp),                       intent(in)     :: Xl   !Lower bound of parameter
          real(kind=cp),                       intent(in)     :: Xu   !Upper bound of parameter
          real(kind=cp),                       intent(in)     :: Xs   !Step of parameter
          integer,                             intent(in)     :: Ic   !Boundary condition (0:fixed or 1:periodic)
          type(NonAtomic_Parameter_List_Type), intent(in out) :: model
          character(len=*), optional,          intent(in)     :: sys
          integer,          optional,          intent(in)     :: Iphas
       End Subroutine Fill_RefGCodes

       Module Subroutine Get_ConCodes_Line_FAtom(Line,FAtom)
          !---- Arguments ----!
          character(len=*),     intent(in)     :: Line
          type(AtList_Type),    intent(in out) :: FAtom
       End Subroutine Get_ConCodes_Line_FAtom


       Module Subroutine Get_ConCodes_Line_FmAtom(Line,ik,FmAtom)
          !---- Arguments ----!
          character(len=*),     intent(in)     :: Line
          integer,              intent(in)     :: ik
          type(mAtom_List_Type),intent(in out) :: FmAtom
       End Subroutine Get_ConCodes_Line_FmAtom

       Module Subroutine Get_ConCodes_Line_Molcrys(Line,Molcrys)
          !---- Arguments ----!
          character(len=*),             intent(in)     :: Line
          type(MolCrystal_Type), intent(in out) :: MolCrys
       End Subroutine Get_ConCodes_Line_Molcrys

       Module Subroutine Get_ConCodes_Line_Molec(Line,Molec)
          !---- Arguments ----!
          character(len=*),    intent(in)     :: Line
          type(molecule_type), intent(in out) :: Molec
       End Subroutine Get_ConCodes_Line_Molec

       Module Subroutine Get_ConCodes_Line_Magdom(Line,Mag_dom)
          !---- Arguments ----!
          character(len=*),     intent(in)          :: Line
          type(Magnetic_Domain_type),intent(in out) :: Mag_dom
       End Subroutine Get_ConCodes_Line_Magdom

       Module Subroutine Get_RefCodes_Line_FAtom(Keyv,Dire,Line,FAtom,Spg)
          !---- Arguments ----!
          integer,                 intent(in)     :: Keyv
          character(len=*),        intent(in)     :: Dire
          character(len=*),        intent(in)     :: Line
          type(AtList_Type),       intent(in out) :: FAtom
          type(SPG_Type),  intent(in)     :: Spg
       End Subroutine Get_RefCodes_Line_FAtom

       Module Subroutine Get_RefCodes_Line_FmAtom(Keyv,Dire,Line,ik,FmAtom)
          !---- Arguments ----!
          integer,                 intent(in)     :: Keyv
          character(len=*),        intent(in)     :: Dire
          character(len=*),        intent(in)     :: Line
          integer,                 intent(in)     :: ik
          type(mAtom_List_Type),   intent(in out) :: FmAtom
       End Subroutine Get_RefCodes_Line_FmAtom

       Module Subroutine Get_RefCodes_Line_Molcrys(Keyv,Dire,Line,Molcrys,NMol)
          !---- Arguments ----!
          integer,                      intent(in)     :: Keyv
          character(len=*),             intent(in)     :: Dire
          character(len=*),             intent(in)     :: Line
          type(MolCrystal_Type),        intent(in out) :: MolCrys
          integer,                      intent(in)     :: NMol
       End Subroutine Get_RefCodes_Line_Molcrys

       Module Subroutine Get_RefCodes_Line_Molec(Keyv,Dire,Line,Molec,Spg)
          !---- Arguments ----!
          integer,                      intent(in)     :: Keyv
          character(len=*),             intent(in)     :: Dire
          character(len=*),             intent(in)     :: Line
          type(molecule_type),          intent(in out) :: Molec
          type(SPG_Type),               intent(in)     :: Spg
       End Subroutine Get_RefCodes_Line_Molec

       Module Subroutine Get_RefCodes_Line_Magdom(Keyv,Dire,Line,Mag_dom)
          !---- Arguments ----!
          integer,                   intent(in)     :: Keyv
          character(len=*),          intent(in)     :: Dire
          character(len=*),          intent(in)     :: Line
          type(Magnetic_Domain_type),intent(in out) :: Mag_dom
       End Subroutine Get_RefCodes_Line_Magdom

       Module Subroutine Get_RefGCodes_Line(Keyv,Dire,Line,namp,model,sys,Iphas)
          integer,                             intent(in)     :: Keyv
          character(len=*),                    intent(in)     :: Dire
          character(len=*),                    intent(in)     :: Line
          character(len=*),                    intent(in)     :: namp
          type(Nonatomic_Parameter_List_Type), intent(in out) :: model
          character(len=*), optional,          intent(in)     :: sys
          integer,          optional,          intent( in)    :: Iphas
       End Subroutine Get_RefGCodes_Line

       Module Subroutine Init_RefCodes(FAtom,FmAtom,Mag_dom,MolCrys,Molec,Model)
          !---- Arguments ----!
          type(AtList_Type),                  optional,intent(in out) :: FAtom   ! Free Atom Object
          type(mAtom_List_Type),              optional,intent(in out) :: FmAtom  ! Magnetic Atom Object
          type(Magnetic_Domain_type),         optional,intent(in out) :: Mag_dom ! Magnetic domain object
          type(MolCrystal_Type),              optional,intent(in out) :: MolCrys ! Molecular Crystal Object
          type(Molecule_Type),                optional,intent(in out) :: Molec   ! Molecule Object
          type(Nonatomic_Parameter_List_Type),optional,intent(in out) :: Model   !Non atomic parameter object
       End Subroutine Init_RefCodes

       Module Subroutine Get_RestAng_Line(Line, FAtom)
          !---- Arguments ----!
          character(len=*),        intent(in) :: Line
          type(AtList_Type),       intent(in) :: FAtom
       End Subroutine Get_RestAng_Line

       Module Subroutine Get_RestDis_Line(Line, FAtom)
          !---- Arguments ----!
          character(len=*),        intent(in) :: Line
          type(AtList_Type),       intent(in) :: FAtom
       End Subroutine Get_RestDis_Line

       Module Subroutine Get_RestTor_Line(Line, FAtom)
          !---- Arguments ----!
          character(len=*),        intent(in) :: Line
          type(AtList_Type),       intent(in) :: FAtom
       End Subroutine Get_RestTor_Line

       Module Subroutine Read_RefCodes_File_FAtom(file_dat,n_ini,n_end,FAtom,Spg)
          !---- Arguments ----!
          Type(file_list_type),     intent( in)    :: file_dat
          integer,                  intent( in)    :: n_ini
          integer,                  intent( in)    :: n_end
          type(AtList_Type),        intent(in out) :: fatom
          type(SPG_Type),           intent(in)     :: Spg
       End Subroutine Read_RefCodes_File_FAtom

       Module Subroutine Read_RefCodes_File_MagStr(file_dat,n_ini,n_end,FmAtom,Mag_dom)
          !---- Arguments ----!
          Type(file_list_type),              intent( in)    :: file_dat
          integer,                           intent( in)    :: n_ini
          integer,                           intent( in)    :: n_end
          type(mAtom_List_Type),             intent(in out) :: FmAtom
          type(Magnetic_Domain_type),optional,intent(in out):: Mag_dom
       End Subroutine Read_RefCodes_File_MagStr

       Module Subroutine Read_RefCodes_File_Molcrys(file_dat,n_ini,n_end,molcrys)
          !---- Arguments ----!
          Type(file_list_type),  intent( in)    :: file_dat
          integer,               intent( in)    :: n_ini
          integer,               intent( in)    :: n_end
          type(MolCrystal_Type), intent(in out) :: molcrys
       End Subroutine Read_RefCodes_File_Molcrys

       Module Subroutine Read_RefCodes_File_Molec(file_dat,n_ini,n_end,molec,spg)
          !---- Arguments ----!
          Type(file_list_type),   intent( in)    :: file_dat
          integer,                intent( in)    :: n_ini
          integer,                intent( in)    :: n_end
          type(molecule_type),    intent(in out) :: molec
          type(SPG_Type),         intent(in)     :: Spg
       End Subroutine Read_RefCodes_File_Molec

       Module Subroutine Read_RefGCodes_File(file_dat,n_ini,n_end,model,sys,Iphas)
          Type(file_list_type),                intent( in)    :: file_dat
          integer,                             intent( in)    :: n_ini
          integer,                             intent( in)    :: n_end
          type(Nonatomic_Parameter_List_Type), intent(in out) :: model
          character(len=*), optional,          intent( in)    :: sys
          integer, optional,                   intent( in)    :: Iphas
       End Subroutine Read_RefGCodes_File

       Module Subroutine Split_Operations(Line, Ni, S_Lines)
          !---- Arguments ----!
          character(len=*),              intent( in) :: line
          integer,                       intent(out) :: ni
          character(len=*),dimension(:), intent(out) :: s_lines
       End Subroutine Split_Operations

       Module Subroutine Split_GOperations(Line, Ni, S_Lines)
          !---- Arguments ----!
          character(len=*),              intent( in) :: line
          integer,                       intent(out) :: ni
          character(len=*),dimension(:), intent(out) :: s_lines
       End Subroutine Split_GOperations

       Module Subroutine Split_mOperations(Line, Ni, S_Lines)
          !---- Arguments ----!
          character(len=*),              intent( in) :: line
          integer,                       intent(out) :: ni
          character(len=*),dimension(:), intent(out) :: s_lines
       End Subroutine Split_mOperations

       Module Subroutine VState_to_AtomsPar_FAtom(FAtom,Mode,MultG)
          !---- Arguments ----!
          type(AtList_Type),          intent(in out) :: FAtom
          character(len=*), optional, intent(in)     :: Mode
          integer,          optional, intent(in)     :: MultG
       End Subroutine VState_to_AtomsPar_FAtom

       Module Subroutine VState_to_AtomsPar_FmAtom(FmAtom,MGp,Mode,Mag_dom)
          !---- Arguments ----!
          type(mAtom_List_Type),                intent(in out) :: FmAtom
          type(MagSymm_k_Type),                 intent(in)     :: MGp
          character(len=*),           optional, intent(in)     :: Mode
          type(Magnetic_Domain_type), optional, intent(in out) :: Mag_dom
       End Subroutine VState_to_AtomsPar_FmAtom

       Module Subroutine VState_to_AtomsPar_Molcrys(Molcrys,Mode)
          !---- Arguments ----!
          type(MolCrystal_Type), intent(in out) :: MolCrys
          character(len=*), optional,    intent(in)     :: Mode
       End Subroutine VState_to_AtomsPar_Molcrys

       Module Subroutine VState_to_AtomsPar_Molec(Molec,Mode)
          !---- Arguments ----!
          type(molecule_type),          intent(in out) :: Molec
          character(len=*), optional,   intent(in)     :: Mode
       End Subroutine VState_to_AtomsPar_Molec

       Module Subroutine VState_to_ModelPar(Model,Mode)
          !---- Arguments ----!
          type(Nonatomic_Parameter_List_Type), intent(in out) :: model
          character(len=*), optional,          intent(in)     :: Mode
       End Subroutine VState_to_ModelPar

       Module Subroutine Write_Info_RefCodes_FAtom(FAtom, Spg, Iunit)
          !---- Arguments ----!
          type(AtList_Type),      intent(in) :: FAtom
          type(SPG_Type),         intent(in) :: Spg
          integer, optional,      intent(in) :: Iunit
       End Subroutine Write_Info_RefCodes_FAtom

       Module Subroutine Write_Info_RefCodes_Molcrys(MolCrys,iunit)
          !---- Arguments ----!
          type(MolCrystal_Type), intent(in) :: molcrys
          integer, optional,     intent(in) :: iunit
       End Subroutine Write_Info_RefCodes_Molcrys

       Module Subroutine Write_Info_RefCodes_Molec(Molec,iunit)
          !---- Arguments ----!
          type(molecule_type), intent(in) :: molec
          integer, optional,   intent(in) :: iunit
       End Subroutine Write_Info_RefCodes_Molec

       Module Subroutine Write_Info_RefCodes_MagStr(FmAtom, Mag_dom, MGp, Iunit)
          !---- Arguments ----!
          type(mAtom_List_Type),               intent(in) :: FmAtom
          type(Magnetic_Domain_type),optional, intent(in) :: Mag_dom
          type(MagSymm_k_Type),      intent(in) :: MGp
          integer, optional,         intent(in) :: Iunit
       End Subroutine Write_Info_RefCodes_MagStr

       Module Subroutine Write_Info_RefGCodes(model, Iunit)
          !---- Arguments ----!
          type(Nonatomic_Parameter_List_Type), intent(in) :: model
          integer, optional,                   intent(in) :: Iunit
       End Subroutine Write_Info_RefGCodes

       Module Subroutine Write_Info_RefParams(iunit)
          !---- Arguments ----!
          integer, optional,   intent(in) :: iunit
       End Subroutine Write_Info_RefParams

       Module Subroutine Write_Restraints_ObsCalc(A,iunit)
          !---- Arguments ----!
          type(AtList_Type),   intent(in) :: A
          integer, optional,   intent(in) :: iunit
       End Subroutine Write_Restraints_ObsCalc

    End Interface

 End Module CFML_Keywords_Code_Parser
