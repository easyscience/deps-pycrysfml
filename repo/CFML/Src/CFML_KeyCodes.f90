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
!!---- MODULE: CFML_KeyCodes
!!----   INFO: Routines for Codes refinement
!!----
!!---- HISTORY
!!----    Update: 11/04/2022
!!----
!!----
!!
Module CFML_KeyCodes
   !---- Use Modules ----!
   Use CFML_GlobalDeps,   only: CP, Clear_error, Set_Error, Err_CFML, CFML_debug
   Use CFML_Atoms,        only: MAX_MOD, AtList_Type, Atm_Type, Atm_Std_Type, ModAtm_Std_Type, &
                                Atm_Ref_Type, ModAtm_Ref_Type, Index_AtLab_on_AtList,          &
                                Change_AtomList_Type
   Use CFML_Strings,      only: File_Type, Get_Num, Cut_String, Get_Words, U_Case, Get_Separator_Pos
   Use CFML_gSpaceGroups, only: Spg_Type, Symm_Oper_Type, Get_Stabilizer, Get_Symb_from_OP,         &
                                Get_OP_from_Symb, Symmetry_symbol, Get_AtomBet_CTR, Get_AtomPos_CTR,&
                                Get_Moment_CTR, Get_TFourier_CTR
   use CFML_Metrics,      only: Cell_Type, Cell_G_Type, Cell_LS_Type, Cell_GLS_Type

   use CFML_Molecules,    only: Molecule_type, index_atLab_on_Molecule

   Use CFML_Scattering_Tables, only: Get_Chem_Symb
   Use CFML_Rational

   Implicit none

   private

   !---- List of public Functions ----!
   public :: Index_GPList

   !---- List of public Subroutines ----!
   public :: Allocate_Restraints_List, Allocate_GPList,   &
             Del_RefCode_ATM, Del_RefCode_GPList, &
             Fill_RefCodes_Atm,  &
             Get_AFIX_Line, Get_DFIX_Line, Get_TFIX_Line, GPList_to_Cell, GPList_to_Molec, &
             GPList_to_AtmList, GPList_from_AtmList, GPList_from_Molec, Get_InfoKey_StrPhas, &
             Get_InfoKey_StrPatt, Get_InfoKey_StrMol, Get_InfoKey_StrPhasPatt, &
             GPList_from_Cell, &
             ReadCode_FIX_ATM, ReadCode_VARY_ATM, Read_RefCodes_ATM, Read_RefCodes_PATT, &
             Read_RefCodes_PHAS, Read_RefCodes_MOL, ReadCode_EQUAL_PHAS, &
             ReadCode_EQUAL_PATT, Read_Restraints_PHAS, &
             Split_GenRefCod_ATM, Split_LocRefCod_ATM, Set_KeyConstr_Cell, &
             Update_GPList_Code, &
             WriteInfo_Restraints, WriteInfo_Constraints, WriteInfo_GPList


   !---- Definitions ----!

   !!----
   !!---- TYPE :: RESTRAINT_TYPE
   !!--..
   !!--.. For distance, set the dimension (Iph, P, Code): 2
   !!--..        angle, set the dimension (Iph, P, Code): 3
   !!--..      torsion, set the dimension (Iph, P, Code): 4
   !!--..
   !!--..
   !!---- April - 2022
   Type, public :: Restraint_Type
      real(kind=cp)                  :: Obs = 0.0_cp
      real(kind=cp)                  :: Cal = 0.0_cp
      real(kind=cp)                  :: Sig = 0.0_cp
      integer,          dimension(4) :: IPh = 0  ! Phase identification
      integer,          dimension(4) :: P   = 0  ! Pointer vector
      character(len=15),dimension(3) :: Code=' ' ! Code
   End Type Restraint_Type

   !!----
   !!----
   !!----
   !!----
   !!----
   !!
   Type, public :: RestList_Type
      integer                                         :: ND_MAX=0 ! Number of Dimension of Par when it was created
      integer                                         :: NRel=0      ! Current number of Parameters
      type(Restraint_Type), allocatable, dimension(:) :: RT
   End Type RestList_Type

   !!----
   !!---- TYPE :: RELATION_TYPE
   !!----
   !!----
   !!----
   !!---- Update: April - 2022
   Type, public :: GenPar_Type
      integer, dimension(2)       :: Ip=0           !(1): Pattern, (2): Phase
      character(len=60)           :: Nam=" "
      character(len=60)           :: Ext=" "
      integer                     :: L=0            ! Code number
      real(kind=cp)               :: M=0.0_cp       ! Multiplier
      real(kind=cp)               :: Val=0.0_cp     ! Values
      real(kind=cp)               :: Sig=0.0_cp     ! Sig values
      real(kind=cp), dimension(2) :: Vlim=0.0_cp    ! Lower/Upper limits
      logical                     :: Bcond=.false.  ! Boundary condition
   End Type GenPar_Type

   !!----
   !!---- TYPE :: GenParList_Type
   !!----
   !!---- The user of this derived type must allocate and fill a type of this kind for
   !!---- his(her) own problem. An object of this type is needed to fill the Vec_RefPar and
   !!---- accompanying arrays.
   !!----
   !!---- Update: April - 2022
   Type, public :: GenParList_Type
      integer                                      :: ND_MAX=0 ! Number of Dimension of Par when it was created
      integer                                      :: NPar=0   ! Current number of Parameters
      type(GenPar_Type), allocatable, dimension(:) :: Par
   End Type GenParList_Type



   !---- Parameters ----!
   integer, public, parameter :: NKEY_PATT=71      ! Number of Keywords for Patterns
   integer, public, parameter :: NKEY_PHAS=219     ! Number of Keywords for Phases

   character(len=*), dimension(NKEY_PATT), public, parameter :: KEY_PATT=[       &
                     "U     ", "V     ", "W     ", "UVW   ",                     &
                     "BKG1  ", "BKG2  ", "BKG3  ", "BKG4  ", "BKG5  ",           &
                     "BKG6  ", "BKG7  ", "BKG8  ", "BKG9  ", "BKG10 ",           &
                     "BKG11 ", "BKG12 ", "BKG   ",                               &
                     "XX1   ", "XX2   ", "XX3   ", "XX    ",                     &
                     "ZERO  ", "SYCOS ", "SYSIN ",                               &
                     "LAMBDA", "SD    ", "SL    ", "SDSL  ",                     &
                     "DTT1  ", "DTT2  ",                                         &
                     "SIG0  ", "SIG1  ", "SIG2  ", "SIG   ",                     &
                     "GAM0  ", "GAM1  ", "GAM2  ", "GAM   ",                     &
                     "FW0   ", "FW1   ", "FW2   ", "FW3   ", "FW4   ",           &
                     "FW5   ", "FW6   ", "FW7   ", "FW8   ", "FW9   ", "FWHM  ", &
                     "FWG0  ", "FWG1  ", "FWG2  ", "FWG3  ", "FWG4  ",           &
                     "FWG5  ", "FWG6  ", "FWG7  ", "FWG8  ", "FWG9  ", "FWHM-G", &
                     "FWL0  ", "FWL1  ", "FWL2  ", "FWL3  ", "FWL4  ",           &
                     "FWL5  ", "FWL6  ", "FWL7  ", "FWL8  ", "FWL9  ", "FWHM-L"]


   character(len=*), dimension(NKEY_PHAS), public, parameter :: KEY_PHAS=[       &
                     "A       ", "B       ", "C       ",                         &
                     "ALPHA   ", "BETA    ", "GAMMA   ", "CELL    ",             &
                     "X       ", "Y       ", "Z       ", "XYZ     ", "OCC     ", &
                     "UISO    ", "U11     ", "U22     ", "U33     ", "U12     ", &
                     "U13     ", "U23     ",                                     &
                     "BISO    ", "B11     ", "B22     ", "B33     ", "B12     ", &
                     "B13     ", "B23     ",                                     &
                     "XC      ", "YC      ", "ZC      ", "CENTRE  ",             &
                     "THE     ", "PHI     ", "CHI     ", "ORIENT  ",             &
                     "DIST    ", "BANG    ", "TORS    ",                         & ! For Z-Matrix
                     "RHO     ", "TH      ", "PH      ",                         & ! For Spherical
                     "T11     ", "T22     ", "T33     ", "T12     ", "T13     ", &
                     "T23     ", "T       ",                                     &
                     "L11     ", "L22     ", "L33     ", "L12     ", "L13     ", &
                     "L23     ", "L       ",                                     &
                     "S11     ", "S12     ", "S13     ",                         &
                     "S21     ", "S22     ", "S23     ",                         &
                     "S31     ", "S32     ", "S33     ", "S       ",             &
                     "TL      ", "LS      ", "TS      ", "TLS     ",             &
                     "SCALE   ", "SC1     ", "SC2     ", "SC3     ", "SC4     ", &
                     "SC5     ", "SC6     ",                                     &
                     "EXTINC  ", "EXTI1   ", "EXTI2   ", "EXTI3   ",             &
                     "EXTI4   ", "EXTI5   ", "EXTI6   ",                         &
                     "ISOSTR  ", "ISOSIZ  ",                                     & !84
                     "ANISTR11", "ANISTR22", "ANISTR33", "ANISTR44", "ANISTR55", &
                     "ANISTR66", "ANISTR12", "ANISTR13", "ANISTR14", "ANISTR15", &
                     "ANISTR16", "ANISTR23", "ANISTR24", "ANISTR25", "ANISTR26", &
                     "ANISTR34", "ANISTR35", "ANISTR36", "ANISTR45", "ANISTR46", &
                     "ANISTR56", "ANISTR  ",                                     & !106
                     "ANISIZ11", "ANISIZ22", "ANISIZ33", "ANISIZ44", "ANISIZ55", &
                     "ANISIZ66", "ANISIZ12", "ANISIZ13", "ANISIZ14", "ANISIZ15", &
                     "ANISIZ16", "ANISIZ23", "ANISIZ24", "ANISIZ25", "ANISIZ26", &
                     "ANISIZ34", "ANISIZ35", "ANISIZ36", "ANISIZ45", "ANISIZ46", &
                     "ANISIZ56", "ANISIZ  ",                                     &
                     "STRGFRAC", "STRLFRAC", "SIZGFRAC", "SIZLFRAC",             & !132
                     "KX      ", "KY      ", "KZ      ",                         &
                     "KX1     ", "KY1     ", "KZ1     ",                         &
                     "KX2     ", "KY2     ", "KZ2     ",                         &
                     "KX3     ", "KY3     ", "KZ3     ",                         &
                     "KX4     ", "KY4     ", "KZ4     ",                         &
                     "KX5     ", "KY5     ", "KZ5     ",                         &
                     "KX6     ", "KY6     ", "KZ6     ",                         &
                     "KX7     ", "KY7     ", "KZ7     ",                         &
                     "KX8     ", "KY8     ", "KZ8     ",                         &
                     "RX      ", "RY      ", "RZ      ",                         &
                     "IX      ", "IY      ", "IZ      ",                         &
                     "RM      ", "RPHI    ", "RTHE    ",                         & !168
                     "IM      ", "IPHI    ", "ITHE    ", "MAGPH   ",             &
                     "MX      ", "MY      ", "MZ      ", "MXYZ    ",             &
                     "MOM     ", "MAZI    ", "MPOL    ", "MSPHER  ",             &
                     "DCOSX   ", "DSINX   ", "DCOSY   ", "DSINY   ",             & !184
                     "DCOSZ   ", "DSINZ   ",                                     &
                     "MCOSX   ", "MSINX   ", "MCOSY   ", "MSINY   ",             &
                     "MCOSZ   ", "MSINZ   ",                                     & !192
                     "BCOS11  ", "BCOS22  ", "BCOS33  ", "BCOS12  ", "BCOS13  ", &
                     "BCOS23  ", "BSIN11  ", "BSIN22  ", "BSIN33  ", "BSIN12  ", &
                     "BSIN13  ", "BSIN23  ",                                     &!204
                     "UCOS11  ", "UCOS22  ", "UCOS33  ", "UCOS12  ", "UCOS13  ", &
                     "UCOS23  ", "USIN11  ", "USIN22  ", "USIN33  ", "USIN12  ", &
                     "USIN13  ", "USIN23  ",                                     & !216
                     "OCCOS   ", "OCSIN   ",                                     &
                     "ALL     "]


   !-------------------!
   !---- Variables ----!
   !-------------------!

   !---- Private ----!
   character(len=256), private                :: line
   character(len=60),  private, dimension(40) :: dire=" "
   integer,            private, dimension(10) :: ivet=0
   real(kind=cp),      private, dimension(10) :: vet=0.0_cp

   !---- Public ----!
   integer, public :: NP_Constr  =0  ! Number of Constraints relations

   integer, public :: NP_Ref_Max =0  ! Number of Maximum refinable Parameters
   integer, public :: NP_Ref     =0  ! Number of Refinable parameters

   integer, public :: NMax_FWHM_Param =-1    ! Number of Maximum parameters for FWHM definitions
   integer, public :: NMax_Bckgd_Params=-1   ! Number of Maximum parameters for Background coefficients

   type(RestList_Type), public :: Rest_Ang  ! Relations for Angle restraints
   type(RestList_Type), public :: Rest_Dis  ! Relations for Distance restraints
   type(RestList_Type), public :: Rest_Tor  ! Relations for Torsion restraints


   !---- Overload Zone ----!

   !---- Interface Zone ----!
   Interface

      Module Function Index_GPList(CodeNam, G) Result(Ind)
         !---- Arguments ----!
         character(Len=*),      intent(in) :: CodeNam
         type(GenParList_Type), intent(in) :: G
         integer                           :: Ind
      End Function Index_GPList

      Module Function Index_KEY_PHAS(String, N_ini, N_end) Result(Ind)
         !---- Arguments ----!
         character(Len=*), intent(in) :: String
         integer, optional, intent(in):: N_ini
         integer, optional, intent(in):: N_end
         integer                      :: Ind
      End Function Index_KEY_PHAS

      Module Function Index_KEY_PATT(String, N_ini, N_end) Result(Ind)
         !---- Arguments ----!
         character(Len=*), intent(in) :: String
         integer, optional, intent(in):: N_ini
         integer, optional, intent(in):: N_end
         integer                      :: Ind
      End Function Index_KEY_PATT

      Module Subroutine Allocate_Restraints_List(R, NDim)
         !---- Arguments ----!
         Type(RestList_Type), intent(in out) :: R
         integer,             intent(in)     :: NDim
      End Subroutine Allocate_Restraints_List

      Module Subroutine Allocate_GPList(NDMax, G)
         !---- Arguments ----!
         integer,               intent(in)     :: NDMax
         type(GenParList_Type), intent(in out) :: G
      End Subroutine Allocate_GPList

      Module Subroutine Del_RefCode_Atm(AtList, NPar)
         !---- Arguments ----!
         type(AtList_Type), intent(in out) :: AtList
         integer,           intent(in)     :: NPar
      End Subroutine Del_RefCode_Atm

      Module Subroutine Del_RefCode_GPList(G, NCode)
         !---- Arguments ----!
         type(GenParList_Type), intent(in out) :: G
         integer,               intent(in)     :: NCode
      End Subroutine Del_RefCode_GPList

      Module Subroutine Fill_RefCodes_Atm(Keyword, Npar, Bounds, Ic, Natm, Spg, AtList)
         !---- Arguments ----!
         character(len=*),              intent(in)     :: Keyword
         integer,                       intent(in)     :: NPar
         real(kind=cp), dimension(3),   intent(in)     :: Bounds
         integer,                       intent(in)     :: Ic
         integer,                       intent(in)     :: Natm
         type(Spg_Type),                intent(in)     :: Spg
         type(AtList_Type),             intent(in out) :: AtList
      End Subroutine Fill_RefCodes_Atm

      Module Subroutine Fix_OCC_Atm(Atlist, NAtm)
         !---- Arguments ----!
         type(AtList_Type), intent(in out) :: AtList
         integer,           intent(in)     :: NAtm
      End Subroutine Fix_OCC_Atm

      Module Subroutine Fix_U_Atm(Atlist, NAtm, Ind)
         !---- Arguments ----!
         type(AtList_Type), intent(in out) :: AtList
         integer,           intent(in)     :: NAtm
         integer,           intent(in)     :: Ind
      End Subroutine Fix_U_Atm

      Module Subroutine Fix_XYZ_Atm(Atlist, NAtm, Ind)
         !---- Arguments ----!
         type(AtList_Type), intent(in out) :: AtList
         integer,           intent(in)     :: NAtm
         integer,           intent(in)     :: Ind
      End Subroutine Fix_XYZ_Atm

      Module Subroutine Get_AFIX_Line(String, IPhase, AtList, R)
         !---- Arguments ----!
         character(len=*),    intent(in)     :: String
         integer,             intent(in)     :: IPhase
         type(AtList_Type),   intent(in)     :: AtList
         Type(RestList_Type), intent(in out) :: R
      End Subroutine Get_AFIX_Line

      Module Subroutine Get_DFIX_Line(String, IPhase, AtList, R)
         !---- Arguments ----!
         character(len=*),    intent(in)     :: String
         integer,             intent(in)     :: IPhase
         type(AtList_Type),   intent(in)     :: AtList
         Type(RestList_Type), intent(in out) :: R
      End Subroutine Get_DFIX_Line

      Module Subroutine Get_TFIX_Line(String, IPhase, AtList, R)
         !---- Arguments ----!
         character(len=*),    intent(in)     :: String
         integer,             intent(in)     :: IPhase
         type(AtList_Type),   intent(in)     :: AtList
         Type(RestList_Type), intent(in out) :: R
      End Subroutine Get_TFIX_Line

      Module Subroutine Split_GenRefCod_ATM(String, Nc, Ikeys, IPhas, Keys)
         !---- Arguments ----!
         character(len=*),                         intent(in)  :: String
         integer,                                  intent(out) :: Nc
         integer, dimension(:),                    intent(out) :: IKeys
         integer, dimension(:),                    intent(out) :: IPhas
         character(len=*), dimension(:), optional, intent(out) :: Keys
      End Subroutine Split_GenRefCod_ATM

      Module Subroutine Split_LocRefCod_ATM(String, Nc, Keys, Ikeys, IPhas, AtLab)
         !---- Arguments ----!
         character(len=*),               intent(in)  :: String
         integer,                        intent(out) :: Nc
         character(len=*), dimension(:), intent(out) :: Keys
         integer,          dimension(:), intent(out) :: Ikeys
         integer,          dimension(:), intent(out) :: IPhas
         character(len=*), dimension(:), intent(out) :: AtLab
      End Subroutine Split_LocRefCod_ATM

      Module Subroutine Vary_OCC_Atm(A, NCode)
         !---- Arguments ----!
         class(Atm_Type),           intent(in out) :: A
         integer,                   intent(in out) :: NCode
      End Subroutine Vary_OCC_Atm

      Module Subroutine Vary_U_Atm(A, NCode, Ind)
         !---- Arguments ----!
         class(Atm_Type), intent(in out) :: A
         integer,         intent(in out) :: NCode
         integer,         intent(in)     :: Ind
      End Subroutine Vary_U_Atm

      Module Subroutine Vary_XYZ_Atm(A, NCode, Ind)
         !---- Arguments ----!
         class(Atm_Type), intent(in out) :: A
         integer,         intent(in out) :: NCode
         integer,         intent(in)     :: Ind
      End Subroutine Vary_XYZ_Atm

      Module Subroutine WriteInfo_GPList(G, Iunit)
         !---- Arguments ----!
         type(GenParList_Type), intent(in) :: G
         integer, optional,     intent(in) :: Iunit
      End Subroutine WriteInfo_GPList

      Module Subroutine WriteInfo_Restraints(RDis, RAng, RTor, IPhase, AtList, Calc,Iunit)
         !---- Arguments ----!
         type(RestList_Type), intent(in) :: RDis
         type(RestList_Type), intent(in) :: RAng
         type(RestList_Type), intent(in) :: RTor
         integer,             intent(in) :: IPhase
         type(AtList_Type),   intent(in) :: AtList
         logical, optional,   intent(in) :: Calc
         integer, optional,   intent(in) :: iunit
      End Subroutine WriteInfo_Restraints

      Module Subroutine WriteInfo_Constraints(AtList, Iunit)
         !---- Arguments ----!
         type(AtList_Type), intent(in) :: AtList
         integer, optional, intent(in) :: Iunit
      End Subroutine WriteInfo_Constraints

      Module Subroutine Read_RefCodes_ATM(ffile, n_ini, n_end, Spg, Atlist)
         !---- Arguments ----!
         Type(file_type),    intent(in)     :: ffile
         integer,            intent(in)     :: n_ini
         integer,            intent(in)     :: n_end
         class (SpG_type),   intent(in)     :: Spg
         type(AtList_Type),  intent(in out) :: AtList
      End Subroutine Read_RefCodes_ATM

      Module Subroutine ReadCode_EQUAL_PHAS(String, IPhas, Spg, Cell, Atm, G)
         !---- Arguments ----!
         character(len=*),      intent(in)     :: String
         integer,               intent(in)     :: IPhas
         class(SpG_Type),       intent(in)     :: SpG
         class(Cell_G_Type),    intent(in out) :: Cell
         type(Atlist_Type),     intent(in out) :: Atm
         type(GenParList_Type), intent(in out) :: G
      End Subroutine ReadCode_EQUAL_PHAS

      Module Subroutine ReadCode_EQUAL_PATT(String, IPatt, G)
         !---- Arguments ----!
         character(len=*),      intent(in)     :: String
         integer,               intent(in)     :: IPatt
         type(GenParList_Type), intent(in out) :: G
      End Subroutine ReadCode_EQUAL_PATT

      Module Subroutine ReadCode_FIX_ATM(String, AtList, Spg)
         !---- Arguments ----!
         character(len=*),   intent(in)     :: String
         type(AtList_Type),  intent(in out) :: AtList
         class (SpG_type),   intent(in)     :: Spg
      End Subroutine ReadCode_FIX_ATM

      Module Subroutine ReadCode_VARY_ATM(String, AtList, Spg)
         !---- Arguments ----!
         character(len=*),   intent(in)     :: String
         type(AtList_Type),  intent(in out) :: AtList
         class (SpG_type),   intent(in)     :: Spg
      End Subroutine ReadCode_VARY_ATM

      Module Subroutine Read_RefCodes_PATT(ffile, n_ini, n_end, IPatt, G)
         !---- Arguments ----!
         Type(file_type),         intent(in)     :: ffile
         integer,                 intent(in)     :: n_ini
         integer,                 intent(in)     :: n_end
         integer,                 intent(in)     :: IPatt
         type(GenParList_Type),   intent(in out) :: G
      End Subroutine Read_RefCodes_PATT

      Module Subroutine Split_RefCod_Patt(String, Nc, Ikeys, IPatt, Keys)
         !---- Arguments ----!
         character(len=*),               intent(in)  :: String
         integer,                        intent(out) :: Nc
         integer, dimension(:),          intent(out) :: IKeys
         integer, dimension(:),          intent(out) :: IPatt
         character(len=*), dimension(:), intent(out) :: Keys
      End Subroutine Split_RefCod_Patt

      Module Subroutine ReadCode_FIX_PATT(String, IPatt, G)
         !---- Arguments ----!
         character(len=*),        intent(in)     :: String
         integer,                 intent(in)     :: IPatt
         type(GenParList_Type),   intent(in out) :: G
      End Subroutine ReadCode_FIX_PATT

      Module Subroutine ReadCode_VARY_PATT(String, IPatt, G)
         !---- Arguments ----!
         character(len=*),        intent(in)   :: String
         integer,                 intent(in)   :: IPatt
         type(GenParList_Type), intent(in out) :: G
      End Subroutine ReadCode_VARY_PATT

      Module Subroutine FIX_GPList_Par(G, CodeNam)
         !---- Arguments ----!
         type(GenParList_Type), intent(in out) :: G
         character(len=*),      intent(in)     :: CodeNam
      End Subroutine FIX_GPList_Par

      Module Subroutine VARY_GPList_Par(G, CodeNam, Value, Sig, Mult, Vlim, Bc)
         !---- Arguments ----!
         type(GenParList_Type),                 intent(in out) :: G
         character(len=*),                      intent(in)     :: CodeNam
         real(kind=cp),               optional, intent(in)     :: Value
         real(kind=cp),               optional, intent(in)     :: Sig
         real(kind=cp),               optional, intent(in)     :: Mult
         real(kind=cp), dimension(2), optional, intent(in)     :: Vlim
         logical,                     optional, intent(in)     :: Bc
      End Subroutine VARY_GPList_Par

      Module Subroutine Set_RefCodes_PATT(Keyword, Npar,  IPatt, G)
         !---- Arguments ----!
         character(len=*),              intent(in)     :: Keyword
         integer,                       intent(in)     :: NPar
         integer,                       intent(in)     :: IPatt
         type(GenParList_Type),         intent(in out) :: G
      End Subroutine Set_RefCodes_PATT

      Module Subroutine Read_RefCodes_PHAS(ffile, n_ini, n_end, IPhas, SpG, Cell, Atm, G)
         !---- Arguments ----!
         Type(file_type),         intent(in)     :: ffile
         integer,                 intent(in)     :: n_ini
         integer,                 intent(in)     :: n_end
         integer,                 intent(in)     :: Iphas
         class(SpG_Type),         intent(in)     :: SpG
         class(Cell_G_Type),      intent(in out) :: Cell
         type(Atlist_Type),       intent(in out) :: Atm
         type(GenParList_Type),   intent(in out) :: G
      End Subroutine Read_RefCodes_PHAS

      Module Subroutine Read_Restraints_PHAS(ffile, N_ini, N_end, IPhas, Atlist, RDis, RAng, RTor)
         !---- Arguments ----!
         Type(file_type),     intent( in)    :: ffile
         integer,             intent(in)     :: N_ini
         integer,             intent(in)     :: N_end
         integer,             intent(in)     :: IPhas
         type(AtList_Type),   intent(in)     :: AtList
         Type(RestList_Type), intent(in out) :: RDis
         Type(RestList_Type), intent(in out) :: RAng
         Type(RestList_Type), intent(in out) :: RTor
      End Subroutine Read_Restraints_PHAS

      Module Subroutine GPList_to_Cell(G, Ip, Cell)
         !---- Arguments ----!
         type(GenParList_Type), intent(in)    :: G
         integer,               intent(in)    :: Ip
         class(Cell_Type),      intent(in out):: Cell
      End Subroutine GPList_to_Cell

      Module Subroutine GPList_from_Cell(Cell, IPh, G)
         !---- Arguments ----!
         class(Cell_Type),      intent(in)     :: Cell
         integer,               intent(in)     :: IPh
         type(GenParList_Type), intent(in out) :: G
      End Subroutine GPList_from_Cell

      Module Subroutine GPList_from_Molec(Mol, Im, Iph, G)
         !---- Arguments ----!
         type(Molecule_type),   intent(in)     :: Mol
         integer,               intent(in)     :: Im
         integer,               intent(in)     :: Iph
         type(GenParList_Type), intent(in out) :: G
      End Subroutine GPList_from_Molec

      Module Subroutine ReadCode_FIX_PHAS(String, IPhas, Spg, Cell, Atm, G)
         !---- Arguments ----!
         character(len=*),      intent(in)     :: String
         integer,               intent(in)     :: IPhas
         class(SpG_Type),       intent(in)     :: SpG
         class(Cell_G_Type),    intent(in out) :: Cell
         type(Atlist_Type),     intent(in out) :: Atm
         type(GenParList_Type), intent(in out) :: G
      End Subroutine ReadCode_FIX_PHAS

      Module Subroutine ReadCode_VARY_PHAS(String, IPhas, Spg, Cell, Atm, G)
         !---- Arguments ----!
         character(len=*),      intent(in)     :: String
         integer,               intent(in)     :: IPhas
         class(SpG_Type),       intent(in)     :: SpG
         class(Cell_G_Type),    intent(in out) :: Cell
         type(Atlist_Type),     intent(in out) :: Atm
         type(GenParList_Type), intent(in out) :: G
      End Subroutine ReadCode_VARY_PHAS

      Module Subroutine Set_RefCodes_PHAS(Keyword, Npar,  IPhas, Qc, Lab, G)
         !---- Arguments ----!
         character(len=*),              intent(in)     :: Keyword
         integer,                       intent(in)     :: NPar
         integer,                       intent(in)     :: IPhas
         integer,                       intent(in)     :: Qc
         character(len=*),              intent(in)     :: Lab
         type(GenParList_Type),         intent(in out) :: G
      End Subroutine Set_RefCodes_PHAS

      Module Subroutine Read_RefCodes_MOL(ffile, n_ini, n_end, IPhas, N_Mol, Mol, G)
         !---- Arguments ----!
         Type(file_type),                   intent(in)     :: ffile
         integer,                           intent(in)     :: n_ini
         integer,                           intent(in)     :: n_end
         integer,                           intent(in)     :: IPhas
         integer,                           intent(in)     :: N_Mol
         type(Molecule_type), dimension(:), intent(in out) :: Mol
         type(GenParList_Type),             intent(in out) :: G
      End Subroutine Read_RefCodes_MOL

      Module Subroutine ReadCode_FIX_MOL(String, IPhas, N_Mol, Mol, G)
         !---- Arguments ----!
         character(len=*),                  intent(in)     :: String
         integer,                           intent(in)     :: IPhas
         integer,                           intent(in)     :: N_Mol
         type(Molecule_type), dimension(:), intent(in out) :: Mol
         type(GenParList_Type),             intent(in out) :: G
      End Subroutine ReadCode_FIX_MOL

      Module Subroutine ReadCode_VARY_MOL(String, IPhas, N_Mol, Mol, G)
         !---- Arguments ----!
         character(len=*),                  intent(in)     :: String
         integer,                           intent(in)     :: IPhas
         integer,                           intent(in)     :: N_Mol
         type(Molecule_type), dimension(:), intent(in out) :: Mol
         type(GenParList_Type),             intent(in out) :: G
      End Subroutine ReadCode_VARY_MOL

      Module Subroutine Set_RefCodes_MOL(Keyword, Npar, IPhas, IMol, Lab, G)
         !---- Arguments ----!
         character(len=*),           intent(in)     :: Keyword
         integer,                    intent(in)     :: NPar
         integer,                    intent(in)     :: IPhas
         integer,                    intent(in)     :: IMol
         character(len=*), optional, intent(in)     :: Lab
         type(GenParList_Type),      intent(in out) :: G
      End Subroutine Set_RefCodes_MOL

      Module Subroutine GPList_to_Molec(G, Iph, Im, Mol)
         !---- Arguments ----!
         type(GenParList_Type), intent(in)     :: G
         integer,               intent(in)     :: Iph
         integer,               intent(in)     :: Im
         type(Molecule_type),   intent(in out) :: Mol
      End Subroutine GPList_to_Molec

      Module Subroutine GPList_from_AtmList(AtList, IPh, G)
         !---- Arguments ----!
         type(Atlist_Type),     intent(in)     :: AtList
         integer,               intent(in)     :: IPh
         type(GenParList_Type), intent(in out) :: G
      End Subroutine GPList_from_AtmList

      Module Subroutine GPList_to_AtmList(G, IPh, Atlist)
         !---- Arguments ----!
         type(GenParList_Type), intent(in)     :: G
         integer,               intent(in)     :: IPh   ! Phase
         type(Atlist_Type),     intent(in out) :: AtList
      End Subroutine GPList_to_AtmList

      Module Subroutine Set_KeyConstr_Cell(CrystSys, Cell)
         !---- Arguments ----!
         character(len=*),   intent(in)     :: CrystSys
         class(Cell_G_Type), intent(in out) :: Cell
      End Subroutine Set_KeyConstr_Cell

      Module Subroutine Update_GPList_Code(G)
         !---- Argument ----!
         type(GenParList_Type), intent(in out) :: G
      End Subroutine Update_GPList_Code

      Module Subroutine Set_KeyConstr_Atm(Atm, Spg)
         !---- Arguments ----!
         type(Atlist_Type), intent(in out) :: Atm
         class(SpG_Type),   intent(in)     :: SpG
      End Subroutine Set_KeyConstr_Atm

      Module Subroutine Get_InfoKey_StrMol(Str, iKey, IPh, IMol)
         !---- Arguments ----!
         character(len=*), intent(in) :: Str
         integer,          intent(out):: iKey
         integer,          intent(out):: IPh
         integer,          intent(out):: IMol
      End Subroutine Get_InfoKey_StrMol

      Module Subroutine Get_InfoKey_StrPhas(Str, iKey, IPh, Lab, QCoeff)
         !---- Arguments ----!
         character(len=*),  intent(in) :: Str
         integer,           intent(out):: iKey
         integer,           intent(out):: IPh
         character(len=*),  intent(out):: Lab
         integer, optional, intent(out):: QCoeff
      End Subroutine Get_InfoKey_StrPhas

      Module Subroutine Get_InfoKey_StrPhasPatt(Str, iKey, IPh, IPat)
         !---- Arguments ----!
         character(len=*), intent(in) :: Str
         integer,          intent(out):: iKey
         integer,          intent(out):: IPh
         integer,          intent(out):: IPat
      End Subroutine Get_InfoKey_StrPhasPatt

      Module Subroutine Get_InfoKey_StrPatt(Str, iKey, IPat)
         !---- Arguments ----!
         character(len=*), intent(in) :: Str
         integer,          intent(out):: iKey
         integer,          intent(out):: IPat
      End Subroutine Get_InfoKey_StrPatt

      Module Subroutine SetCode_EQUAL_PHAS(string, IPhas, G)
         !---- Arguments ----!
         character(len=*),      intent(in)     :: String
         integer,               intent(in)     :: IPhas
         type(GenParList_Type), intent(in out) :: G
      End Subroutine SetCode_EQUAL_PHAS

   End Interface

End Module CFML_KeyCodes
