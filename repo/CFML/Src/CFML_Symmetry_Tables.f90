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
!!---- MODULE: CFML_Symmetry_Tables
!!----   INFO: Tabulated information on Crystallographic Symmetry
!!----
!!---- HISTORY
!!----    Update: 04/03/2011
!!----
!!
 Module CFML_Symmetry_Tables
    !---- Use modules ----!
    Use CFML_GlobalDeps, only: CP, CFML_Debug
    Use CFML_Strings,    only: u_case, pack_string, string_count

    !---- Variables ----!
    implicit none

    private

    !---- List of public subroutines ----!
    public :: get_compact_hm, Get_HM_Compact_HM, get_it_generators, &
              get_shubnikov_info,get_spacegroup_Symbols,get_spgr_info
    public :: set_IT_gen,set_shubnikov_info,set_spgr_info, set_system_equiv, set_wyckoff_info
    public :: remove_shubnikov_info, remove_spgr_info, remove_system_equiv, remove_wyckoff_info

    !---- Types Definitions ----!

    !!----
    !!---- TYPE :: SHUB_SPGR_INFO_TYPE
    !!----
    !!
    !Type, public :: Shub_Spgr_Info_Type
    !   character(len=8)  :: ID_BNS =" "   !  ID Number of BNS
    !   character(len=15) :: BNS    =" "   !  BNS symbol
    !   character(len=12) :: ID_OG  =" "   !  ID number of OG
    !   character(len=15) :: OG     =" "   !  OG symbol
    !   character(len=25) :: STD    =" "   !  Proposed standard symbol
    !   character(len=25) :: MHall  =" "   !  Magnetic Hall symbol
    !End Type Shub_Spgr_Info_Type

    Type, public :: Shub_Spgr_Info_Type
       character(len=7)   :: ID_BNS =" "      !  ID Number of BNS
       character(len=12)  :: BNS    =" "      !  BNS symbol
       character(len=11)  :: ID_OG  =" "      !  ID number of OG
       character(len=14)  :: OG     =" "      !  OG symbol
       character(len=27)  :: STD    =" "      !  Proposed standard symbol (UNI symbol)
       character(len=20)  :: MHall  =" "      !  Magnetic Hall symbol
       character(len=104) :: generators =" "  !  String with generators of the UNI symbol
    End Type Shub_Spgr_Info_Type

    !!----
    !!---- TYPE :: SPGR_INFO_TYPE
    !!--..
    !!
    Type, public :: Spgr_Info_Type
       integer                 :: N         =0      ! Number of the Spacegroup
       character (len=12)      :: HM        =" "    ! Hermann-Mauguin
       character (len=16)      :: Hall      =" "    ! Hall
       integer                 :: Laue      =0      ! Laue Group
       integer                 :: Pg        =0      ! Point group
       integer, dimension(6)   :: Asu       =0      ! Asymmetric unit * 24
       character (len= 5)      :: Inf_Extra =" "    ! Extra information
    End Type Spgr_Info_Type

    !!----
    !!---- TYPE :: TABLE_EQUIV_TYPE
    !!--..
    !!
    Type, public :: Table_Equiv_Type
       character(len= 6)      :: SC=" "             ! Schoenflies
       character(len=17)      :: ML=" "             ! Miller & Love
       character(len=18)      :: KO=" "             ! Kovalev
       character(len=32)      :: BC=" "             ! Bradley & Cracknell
       character(len=18)      :: ZA=" "             ! Zak
    End Type Table_Equiv_Type

    !!----
    !!---- TYPE :: WYCK_INFO_TYPE
    !!--..
    !!
    Type, public :: Wyck_Info_Type
       character (len=12)               :: HM      =" "
       integer                          :: Norbit  =0
       character (len=15),dimension(26) :: Corbit  =" "
    End Type Wyck_Info_Type

    !---- Private Variables ----!
    character(len=120), private, dimension(230) :: it_spg_gen=" "          ! Vector containing Generators of the Space group

    !---- Public variables ----!
    Type(Shub_Spgr_Info_Type), allocatable, dimension(:), public :: Shubnikov_Info ! General Info about Shubnikov Magnetic grousp
    Type(Spgr_Info_Type),      allocatable, dimension(:), public :: Spgr_Info      ! General Info about Space Groups
    Type(Table_Equiv_Type),    allocatable, dimension(:), public :: System_Equiv
    Type(Wyck_Info_Type),      allocatable, dimension(:), public :: Wyckoff_Info

    logical, public :: Shubnikov_Info_loaded=.false., Spgr_Info_loaded=.false., &
                       System_Equiv_loaded=.false., Wyckoff_Info_loaded=.false.

    !---- Parameters ----!
    integer,       parameter, public  :: IND_GRP_CUBIC  = 554    ! Cubic parameter index
    integer,       parameter, public  :: IND_GRP_HEXAG  = 527    ! Index parameter for hexagonal Groups
    integer,       parameter, public  :: IND_GRP_MONOC  =  15    ! Index parameter for Monoclinic Groups
    integer,       parameter, public  :: IND_GRP_ORTHO  = 163    ! Index parameter for Orthorhombic Groups
    integer,       parameter, public  :: IND_GRP_TETRA  = 410    ! Index parameter for Tetragonal Groups
    integer,       parameter, public  :: IND_GRP_TRIGO  = 495    ! Index parameter for Trigonal Groups
    integer,       parameter, public  :: NUM_SPGR_INFO  = 612    ! Total dimension of SPGR_INFO
    integer,       parameter, public  :: NUM_SHUBNIKOV  = 1651   ! Total dimension of SHUBNIKOV_INFO


    !> Lattice Traslations
    real(kind=cp), dimension(3,2), parameter, public :: LTR_A =reshape ( (/0.0,0.0,0.0, 0.0,0.5,0.5/), (/3,2/) )
    real(kind=cp), dimension(3,2), parameter, public :: LTR_B =reshape ( (/0.0,0.0,0.0, 0.5,0.0,0.5/), (/3,2/) )
    real(kind=cp), dimension(3,2), parameter, public :: LTR_C =reshape ( (/0.0,0.0,0.0, 0.5,0.5,0.0/), (/3,2/) )
    real(kind=cp), dimension(3,2), parameter, public :: LTR_I =reshape ( (/0.0,0.0,0.0, 0.5,0.5,0.5/), (/3,2/) )
    real(kind=cp), dimension(3,4), parameter, public :: &
                   LTR_F =reshape( (/0.0,0.0,0.0, 0.0,0.5,0.5, 0.5,0.0,0.5, 0.5,0.5,0.0 /),(/3,4/) )
    real(kind=cp), dimension(3,3), parameter, public :: &
                   LTR_R =reshape( (/0.0,0.0,0.0, 2.0/3.0,1.0/3.0,1.0/3.0,  1.0/3.0,2.0/3.0,2.0/3.0/),(/3,3/) )


    !> Bradley & Cracknell Notation
    character(len=*), dimension(24), parameter, public  :: BC_D6h =[            &
       "  E  "," C+_3"," C-_3"," C_2 "," C-_6"," C+_6","C'_23","C'_21","C'_22", &
       "C`_23","C`_21","C`_22","  I  "," S-_6"," S+_6"," s_h "," S+_3"," S-_3", &
       " s_v3"," s_v1"," s_v2"," s_d3"," s_d1"," s_d2"]

    character(len=*), dimension(48), parameter, public :: BC_Oh =[              &
       "  E  "," C_2z"," C_2y"," C_2x","C+_31","C+_34","C+_33","C+_32","C-_31", &
       "C-_33","C-_32","C-_34"," C_2a"," C_2b","C-_4z","C+_4z","C-_4x"," C_2d", &
       " C_2f","C+_4x","C+_4y"," C_2c","C-_4y"," C_2e","  I  "," s_z "," s_y ", &
       " s_x ","S-_61","S-_64","S-_63","S-_62","S+_61","S+_63","S+_62","S+_64", &
       " s_da"," s_db","S+_4z","S-_4z","S+_4x"," s_dd"," s_df","S-_4x","S-_4y", &
       " s_dc","S+_4y"," s_de"]

    !> Magnetic Array
    character(len=*), dimension(72), parameter, public :: DEPMAT = [        &
       "( Dx, Dy, Dz)      ","(-Dx,-Dy, Dz)      ","(-Dx, Dy,-Dz)      ",   &
       "( Dx,-Dy,-Dz)      ","( Dz, Dx, Dy)      ","( Dz,-Dx,-Dy)      ",   &
       "(-Dz,-Dx, Dy)      ","(-Dz, Dx,-Dy)      ","( Dy, Dz, Dx)      ",   &
       "(-Dy, Dz,-Dx)      ","( Dy,-Dz,-Dx)      ","(-Dy,-Dz, Dx)      ",   &
       "( Dy, Dx,-Dz)      ","(-Dy,-Dx,-Dz)      ","( Dy,-Dx, Dz)      ",   &
       "(-Dy, Dx, Dz)      ","( Dx, Dz,-Dy)      ","(-Dx, Dz, Dy)      ",   &
       "(-Dx,-Dz,-Dy)      ","( Dx,-Dz, Dy)      ","( Dz, Dy,-Dx)      ",   &
       "( Dz,-Dy, Dx)      ","(-Dz, Dy, Dx)      ","(-Dz,-Dy,-Dx)      ",   &
       "(-Dx,-Dy,-Dz)      ","( Dx, Dy,-Dz)      ","( Dx,-Dy, Dz)      ",   &
       "(-Dx, Dy, Dz)      ","(-Dz,-Dx,-Dy)      ","(-Dz, Dx, Dy)      ",   &
       "( Dz, Dx,-Dy)      ","( Dz,-Dx, Dy)      ","(-Dy,-Dz,-Dx)      ",   &
       "( Dy,-Dz, Dx)      ","(-Dy, Dz, Dx)      ","( Dy, Dz,-Dx)      ",   &
       "(-Dy,-Dx, Dz)      ","( Dy, Dx, Dz)      ","(-Dy, Dx,-Dz)      ",   &
       "( Dy,-Dx,-Dz)      ","(-Dx,-Dz, Dy)      ","( Dx,-Dz,-Dy)      ",   &
       "( Dx, Dz, Dy)      ","(-Dx, Dz,-Dy)      ","(-Dz,-Dy, Dx)      ",   &
       "(-Dz, Dy,-Dx)      ","( Dz,-Dy,-Dx)      ","( Dz, Dy, Dx)      ",   &
       "( Dx   ,    Dy, Dz)","(   -Dy, Dx-Dy, Dz)","(-Dx+Dy,-Dx   , Dz)",   &
       "(-Dx   ,   -Dy, Dz)","(    Dy,-Dx+Dy, Dz)","( Dx-Dy, Dx   , Dz)",   &
       "(    Dy, Dx   ,-Dz)","( Dx-Dy,   -Dy,-Dz)","(-Dx   ,-Dx+Dy,-Dz)",   &
       "(   -Dy,-Dx   ,-Dz)","(-Dx+Dy,    Dy,-Dz)","( Dx   , Dx-Dy,-Dz)",   &
       "(-Dx   ,   -Dy,-Dz)","(    Dy,-Dx+Dy,-Dz)","( Dx-Dy, Dx   ,-Dz)",   &
       "( Dx   ,    Dy,-Dz)","(   -Dy, Dx-Dy,-Dz)","(-Dx+Dy,-Dx   ,-Dz)",   &
       "(   -Dy,-Dx   , Dz)","(-Dx+Dy,    Dy, Dz)","( Dx   , Dx-Dy, Dz)",   &
       "(    Dy, Dx   , Dz)","( Dx-Dy,   -Dy, Dz)","(-Dx   ,-Dx+Dy, Dz)"]

    character(len=*), dimension(72), parameter, public :: MAGMAT = [        &
       "( Mx, My, Mz)      ","(-Mx,-My, Mz)      ","(-Mx, My,-Mz)      ",   &
       "( Mx,-My,-Mz)      ","( Mz, Mx, My)      ","( Mz,-Mx,-My)      ",   &
       "(-Mz,-Mx, My)      ","(-Mz, Mx,-My)      ","( My, Mz, Mx)      ",   &
       "(-My, Mz,-Mx)      ","( My,-Mz,-Mx)      ","(-My,-Mz, Mx)      ",   &
       "( My, Mx,-Mz)      ","(-My,-Mx,-Mz)      ","( My,-Mx, Mz)      ",   &
       "(-My, Mx, Mz)      ","( Mx, Mz,-My)      ","(-Mx, Mz, My)      ",   &
       "(-Mx,-Mz,-My)      ","( Mx,-Mz, My)      ","( Mz, My,-Mx)      ",   &
       "( Mz,-My, Mx)      ","(-Mz, My, Mx)      ","(-Mz,-My,-Mx)      ",   &
       "(-Mx,-My,-Mz)      ","( Mx, My,-Mz)      ","( Mx,-My, Mz)      ",   &
       "(-Mx, My, Mz)      ","(-Mz,-Mx,-My)      ","(-Mz, Mx, My)      ",   &
       "( Mz, Mx,-My)      ","( Mz,-Mx, My)      ","(-My,-Mz,-Mx)      ",   &
       "( My,-Mz, Mx)      ","(-My, Mz, Mx)      ","( My, Mz,-Mx)      ",   &
       "(-My,-Mx, Mz)      ","( My, Mx, Mz)      ","(-My, Mx,-Mz)      ",   &
       "( My,-Mx,-Mz)      ","(-Mx,-Mz, My)      ","( Mx,-Mz,-My)      ",   &
       "( Mx, Mz, My)      ","(-Mx, Mz,-My)      ","(-Mz,-My, Mx)      ",   &
       "(-Mz, My,-Mx)      ","( Mz,-My,-Mx)      ","( Mz, My, Mx)      ",   &
       "( Mx   ,    My, Mz)","(   -My, Mx-My, Mz)","(-Mx+My,-Mx   , Mz)",   &
       "(-Mx   ,   -My, Mz)","(    My,-Mx+My, Mz)","( Mx-My, Mx   , Mz)",   &
       "(    My, Mx   ,-Mz)","( Mx-My,   -My,-Mz)","(-Mx   ,-Mx+My,-Mz)",   &
       "(   -My,-Mx   ,-Mz)","(-Mx+My,    My,-Mz)","( Mx   , Mx-My,-Mz)",   &
       "(-Mx   ,   -My,-Mz)","(    My,-Mx+My,-Mz)","( Mx-My, Mx   ,-Mz)",   &
       "( Mx   ,    My,-Mz)","(   -My, Mx-My,-Mz)","(-Mx+My,-Mx   ,-Mz)",   &
       "(   -My,-Mx   , Mz)","(-Mx+My,    My, Mz)","( Mx   , Mx-My, Mz)",   &
       "(    My, Mx   , Mz)","( Mx-My,   -My, Mz)","(-Mx   ,-Mx+My, Mz)"]


    !> International Symbols For Point Group Elements Of 6/mmm (D6h)
    character(len=*), dimension(24), parameter, public :: IntSymD6h =[       &
       "  1           "," 3+ ( 0, 0, z)"," 3- ( 0, 0, z)","  2 ( 0, 0, z)",  &
       " 6- ( 0, 0, z)"," 6+ ( 0, 0, z)","  2 ( x, x, 0)","  2 ( x, 0, 0)",  &
       "  2 ( 0, y, 0)","  2 ( x,-x, 0)","  2 ( x,2x, 0)","  2 (2x, x, 0)",  &
       " -1           ","-3+ ( 0, 0, z)","-3- ( 0, 0, z)","  m ( x, y, 0)",  &
       "-6- ( 0, 0, z)","-6+ ( 0, 0, z)","  m ( x,-x, z)","  m ( x,2x, z)",  &
       "  m (2x, x, z)","  m ( x, x, z)","  m ( x, 0, z)","  m ( 0, y, z)"]

    !> International Symbols For Point Group Elements Of M3M (Oh)
    character(len=*), dimension(48), parameter, public :: IntSymOh = [       &
       "  1           ","  2 ( 0, 0, z)","  2 ( 0, y, 0)","  2 ( x, 0, 0)",  &
       " 3+ ( x, x, x)"," 3+ (-x, x,-x)"," 3+ ( x,-x,-x)"," 3+ (-x,-x, x)",  &
       " 3- ( x, x, x)"," 3- ( x,-x,-x)"," 3- (-x,-x, x)"," 3- (-x, x,-x)",  &
       "  2 ( x, x, 0)","  2 ( x,-x, 0)"," 4- ( 0, 0, z)"," 4+ ( 0, 0, z)",  &
       " 4- ( x, 0, 0)","  2 ( 0, y, y)","  2 ( 0, y,-y)"," 4+ ( x, 0, 0)",  &
       " 4+ ( 0, y, 0)","  2 ( x, 0, x)"," 4- ( 0, y, 0)","  2 (-x, 0, x)",  &
       " -1           ","  m ( x, y, 0)","  m ( x, 0, z)","  m ( 0, y, z)",  &
       "-3+ ( x, x, x)","-3+ (-x, x,-x)","-3+ ( x,-x,-x)","-3+ (-x,-x, x)",  &
       "-3- ( x, x, x)","-3- ( x,-x,-x)","-3- (-x,-x, x)","-3- (-x, x,-x)",  &
       "  m ( x,-x, z)","  m ( x, x, z)","-4- ( 0, 0, z)","-4+ ( 0, 0, z)",  &
       "-4- ( x, 0, 0)","  m ( x, y,-y)","  m ( x, y, y)","-4+ ( x, 0, 0)",  &
       "-4+ ( 0, y, 0)","  m (-x, y, x)","-4- ( 0, y, 0)","  m ( x, y, x)"]

    !> Kovalev Notation
    character(len=*), dimension(24), parameter, public :: KOV_D6H=[        &
       " h1"," h3"," h5"," h4"," h6"," h2","h11"," h9"," h7"," h8","h12",  &
       "h10","h13","h15","h17","h16","h18","h14","h23",                    &
       "h21","h19","h20","h24","h22"]

    character(len=*), dimension(48), parameter, public :: KOV_OH=[                &
       " h1"," h4"," h3"," h2"," h9","h10","h12","h11"," h5"," h7"," h6"," h8",   &
       "h16","h13","h15","h14","h20","h18","h17","h19","h24","h23",               &
       "h22","h21","h25","h28","h27","h26","h33","h34","h36","h35",               &
       "h29","h31","h30","h32","h40","h37","h39","h38","h44","h42",               &
       "h41","h43","h48","h47","h46","h45"]

    !> Lattice Traslations
    character(len=*), dimension(8) , parameter, public  :: LATT =[     &
       "  P: { 000 }                                       ",          &
       "  A: { 000;  0  1/2 1/2 }+                         ",          &
       "  B: { 000; 1/2  0  1/2 }+                         ",          &
       "  C: { 000; 1/2 1/2  0  }+                         ",          &
       "  I: { 000; 1/2 1/2 1/2 }+                         ",          &
       "  R: { 000; 2/3 1/3 1/3; 1/3 2/3 2/3   }+          ",          &
       "  F: { 000;  0  1/2 1/2; 1/2  0  1/2; 1/2 1/2  0 }+",          &
       "  Z: { 000;  Unconventional Z-centering vectors  }+"]

    !> Laue symbols
    character(len=*), dimension(16), parameter, public :: LAUE_CLASS=[  &
       "-1   ","2/m  ","mmm  ","4/m  ","4/mmm","-3 R ","-3m R","-3   ", &
       "-3m1 ","-31m ","6/m  ","6/mmm","m-3  ","m-3m ","m3   ","m3m  "]

    integer, dimension(1651), parameter, public :: Litvin2IT = [                             &  !Litvin to International pointer
        1,    2,    3,    4,    5,    6,    7,    8,    9,   10,   11,   12,   23,   15,   16,   17,   18,   14,   24,   19,  &
       20,   21,   22,   13,   25,   26,   27,   28,   29,   42,   32,   33,   34,   35,   31,   36,   48,   44,   38,   39,  &
       40,   41,   30,   45,   46,   47,   43,   37,   49,   50,   51,   52,   53,   54,   55,   72,   59,   60,   61,   62,  &
       63,   64,   57,   74,   66,   67,   68,   69,   70,   71,   56,   77,   78,   79,   80,   81,   82,   83,   58,   75,  &
       97,   86,   87,   88,   89,   90,   91,   85,   65,   76,   98,   92,   93,   94,   95,   96,   73,   84,   99,  100,  &
      101,  102,  134,  148,  106,  107,  108,  109,  110,  105,  138,  126,  154,  113,  114,  115,  116,  112,  117,  128,  &
      137,  149,  119,  120,  121,  118,  127,  153,  122,  123,  124,  125,  136,  111,  144,  129,  130,  131,  132,  133,  &
      103,  143,  140,  141,  142,  104,  145,  146,  147,  135,  150,  151,  152,  139,  155,  156,  157,  158,  159,  160,  &
      241,  271,  328,  168,  169,  170,  171,  172,  173,  174,  164,  275,  287,  254,  345,  178,  179,  180,  181,  165,  &
      182,  262,  289,  335,  185,  186,  187,  188,  189,  166,  190,  191,  296,  284,  245,  343,  198,  199,  200,  201,  &
      202,  177,  203,  194,  306,  288,  255,  336,  205,  206,  207,  208,  209,  210,  184,  196,  276,  308,  263,  346,  &
      212,  213,  214,  215,  216,  176,  217,  195,  298,  274,  256,  329,  219,  220,  221,  222,  223,  193,  246,  305,  &
      337,  226,  227,  228,  229,  230,  218,  204,  224,  297,  307,  257,  344,  231,  232,  233,  234,  211,  225,  299,  &
      264,  330,  236,  237,  238,  239,  240,  161,  313,  249,  250,  251,  252,  253,  243,  175,  315,  258,  259,  260,  &
      261,  244,  183,  316,  265,  266,  267,  268,  269,  270,  162,  314,  278,  279,  280,  281,  282,  283,  167,  317,  &
      291,  292,  293,  294,  295,  273,  192,  318,  300,  301,  302,  303,  304,  286,  197,  319,  309,  310,  311,  312,  &
      163,  320,  321,  322,  323,  235,  324,  325,  326,  327,  242,  272,  331,  332,  333,  334,  248,  290,  338,  339,  &
      340,  341,  342,  247,  277,  285,  347,  348,  349,  350,  351,  352,  553,  626,  358,  359,  360,  361,  362,  386,  &
      576,  629,  364,  365,  366,  367,  368,  369,  370,  371,  356,  585,  571,  637,  377,  378,  379,  380,  381,  382,  &
      383,  375,  384,  601,  561,  642,  387,  388,  389,  390,  391,  392,  393,  394,  395,  355,  396,  397,  520,  557,  &
      590,  657,  406,  407,  408,  409,  410,  411,  412,  413,  414,  427,  385,  440,  575,  527,  604,  658,  415,  416,  &
      417,  418,  419,  420,  421,  422,  423,  401,  424,  374,  572,  560,  542,  659,  428,  429,  430,  431,  432,  433,  &
      434,  435,  436,  373,  437,  404,  538,  589,  602,  649,  441,  442,  443,  444,  445,  446,  447,  402,  448,  537,  &
      559,  640,  451,  452,  453,  454,  455,  456,  457,  439,  487,  541,  573,  639,  458,  459,  460,  461,  462,  463,  &
      464,  465,  466,  467,  403,  399,  591,  540,  521,  638,  471,  472,  473,  474,  475,  476,  477,  426,  450,  525,  &
      574,  628,  478,  479,  480,  481,  482,  483,  484,  400,  485,  522,  558,  627,  488,  489,  490,  491,  492,  493,  &
      494,  495,  496,  438,  470,  425,  544,  603,  526,  641,  497,  498,  499,  500,  501,  469,  543,  648,  502,  503,  &
      504,  505,  506,  507,  508,  509,  510,  486,  449,  468,  523,  524,  539,  660,  511,  512,  513,  514,  515,  516,  &
      517,  518,  519,  556,  398,  611,  528,  529,  530,  531,  532,  533,  534,  535,  536,  587,  405,  614,  545,  546,  &
      547,  548,  549,  550,  551,  552,  353,  610,  564,  565,  566,  567,  568,  569,  570,  555,  372,  613,  577,  578,  &
      579,  580,  581,  582,  583,  584,  357,  612,  594,  595,  596,  597,  598,  599,  600,  588,  376,  615,  605,  606,  &
      607,  608,  609,  354,  616,  617,  618,  619,  620,  363,  621,  622,  623,  624,  625,  554,  630,  631,  632,  633,  &
      634,  635,  636,  563,  586,  643,  644,  645,  646,  647,  593,  650,  651,  652,  653,  654,  655,  656,  592,  562,  &
      661,  662,  663,  664,  665,  686,  668,  669,  670,  675,  671,  691,  672,  673,  674,  667,  676,  687,  679,  680,  &
      681,  678,  682,  692,  683,  684,  685,  666,  688,  689,  690,  677,  693,  694,  695,  696,  697,  702,  699,  700,  &
      701,  698,  703,  704,  705,  706,  707,  708,  709,  738,  713,  714,  715,  716,  717,  711,  718,  739,  720,  721,  &
      722,  723,  724,  725,  712,  740,  727,  728,  729,  730,  731,  726,  719,  741,  733,  734,  735,  736,  737,  710,  &
      742,  743,  744,  745,  746,  732,  747,  748,  749,  750,  751,  752,  753,  810,  757,  758,  759,  760,  761,  762,  &
      756,  812,  764,  765,  766,  767,  768,  781,  769,  819,  771,  772,  773,  774,  775,  791,  770,  821,  776,  777,  &
      778,  779,  780,  755,  782,  811,  786,  787,  788,  789,  790,  763,  785,  813,  793,  794,  795,  796,  797,  784,  &
      798,  820,  800,  801,  802,  803,  804,  792,  799,  822,  805,  806,  807,  808,  809,  754,  814,  815,  816,  817,  &
      818,  783,  823,  824,  825,  826,  827,  828,  829,  888,  836,  837,  838,  839,  840,  841,  834,  897,  845,  846,  &
      847,  848,  849,  831,  876,  898,  852,  853,  854,  855,  856,  842,  877,  889,  859,  860,  861,  862,  863,  833,  &
      864,  900,  866,  867,  868,  869,  870,  844,  865,  891,  871,  872,  873,  874,  875,  832,  850,  890,  878,  879,  &
      880,  881,  882,  843,  851,  899,  883,  884,  885,  886,  887,  830,  892,  893,  894,  895,  896,  835,  901,  902,  &
      903,  904,  905,  857,  906,  907,  908,  909,  910,  858,  911,  912,  913,  914,  915,  916,  947,  990,  922,  923,  &
      924,  925,  926,  919,  956,  992,  929,  930,  931,  932,  933,  934,  950,  991,  936,  937,  938,  939,  940,  935,  &
      957,  993,  941,  942,  943,  944,  945,  946,  917,  976,  951,  952,  953,  954,  955,  949,  927,  983,  958,  959,  &
      960,  961,  962,  963,  920,  984,  965,  966,  967,  968,  969,  964,  928,  977,  971,  972,  973,  974,  975,  918,  &
      978,  979,  980,  981,  982,  921,  985,  986,  987,  988,  989,  948,  994,  995,  996,  997,  998,  970,  999, 1000,  &
     1001, 1002, 1003, 1004, 1005, 1006, 1007, 1008, 1009, 1188, 1018, 1019, 1020, 1021, 1022, 1023, 1024, 1025, 1026, 1013,  &
     1027, 1205, 1031, 1032, 1033, 1034, 1035, 1036, 1037, 1038, 1039, 1040, 1016, 1212, 1044, 1045, 1046, 1047, 1048, 1049,  &
     1050, 1051, 1052, 1043, 1030, 1195, 1053, 1054, 1055, 1056, 1057, 1058, 1059, 1060, 1061, 1062, 1015, 1210, 1066, 1067,  &
     1068, 1069, 1070, 1071, 1072, 1073, 1074, 1065, 1029, 1193, 1075, 1076, 1077, 1078, 1079, 1080, 1081, 1082, 1083, 1084,  &
     1014, 1189, 1088, 1089, 1090, 1091, 1092, 1093, 1094, 1095, 1096, 1087, 1028, 1206, 1097, 1098, 1099, 1100, 1101, 1102,  &
     1103, 1104, 1105, 1012, 1119, 1191, 1110, 1111, 1112, 1113, 1114, 1115, 1116, 1117, 1118, 1011, 1106, 1208, 1123, 1124,  &
     1125, 1126, 1127, 1128, 1129, 1130, 1131, 1042, 1122, 1209, 1132, 1133, 1134, 1135, 1136, 1137, 1138, 1139, 1140, 1041,  &
     1109, 1192, 1143, 1144, 1145, 1146, 1147, 1148, 1149, 1150, 1151, 1064, 1121, 1207, 1152, 1153, 1154, 1155, 1156, 1157,  &
     1158, 1159, 1160, 1063, 1108, 1190, 1161, 1162, 1163, 1164, 1165, 1166, 1167, 1168, 1169, 1086, 1120, 1194, 1170, 1171,  &
     1172, 1173, 1174, 1175, 1176, 1177, 1178, 1085, 1107, 1211, 1179, 1180, 1181, 1182, 1183, 1184, 1185, 1186, 1187, 1010,  &
     1196, 1197, 1198, 1199, 1200, 1201, 1202, 1203, 1204, 1017, 1213, 1214, 1215, 1216, 1217, 1218, 1219, 1220, 1221, 1141,  &
     1222, 1223, 1224, 1225, 1226, 1227, 1228, 1229, 1230, 1142, 1231, 1232, 1233, 1234, 1235, 1239, 1237, 1238, 1236, 1240,  &
     1241, 1242, 1243, 1244, 1245, 1246, 1247, 1248, 1249, 1250, 1251, 1252, 1253, 1254, 1255, 1256, 1257, 1258, 1259, 1260,  &
     1261, 1270, 1263, 1264, 1265, 1274, 1267, 1268, 1269, 1262, 1271, 1272, 1273, 1266, 1275, 1276, 1277, 1278, 1279, 1280,  &
     1281, 1282, 1284, 1285, 1286, 1287, 1289, 1290, 1291, 1283, 1292, 1293, 1294, 1288, 1295, 1296, 1297, 1298, 1300, 1301,  &
     1302, 1299, 1303, 1304, 1305, 1306, 1307, 1308, 1310, 1311, 1312, 1313, 1314, 1309, 1315, 1316, 1317, 1318, 1319, 1320,  &
     1322, 1323, 1324, 1325, 1326, 1321, 1327, 1328, 1329, 1330, 1331, 1332, 1334, 1335, 1336, 1337, 1338, 1333, 1339, 1340,  &
     1341, 1342, 1344, 1345, 1346, 1353, 1347, 1348, 1349, 1359, 1350, 1351, 1352, 1358, 1355, 1356, 1357, 1354, 1360, 1361,  &
     1362, 1343, 1363, 1364, 1365, 1366, 1367, 1368, 1369, 1370, 1371, 1372, 1374, 1375, 1376, 1377, 1378, 1373, 1379, 1380,  &
     1381, 1382, 1383, 1384, 1386, 1387, 1388, 1389, 1390, 1401, 1391, 1392, 1393, 1394, 1395, 1409, 1396, 1397, 1398, 1399,  &
     1400, 1408, 1403, 1404, 1405, 1406, 1407, 1402, 1410, 1411, 1412, 1413, 1414, 1385, 1415, 1416, 1417, 1418, 1419, 1420,  &
     1424, 1425, 1426, 1427, 1428, 1423, 1429, 1430, 1431, 1432, 1433, 1421, 1434, 1435, 1436, 1437, 1438, 1422, 1439, 1440,  &
     1441, 1442, 1443, 1444, 1446, 1447, 1448, 1449, 1450, 1445, 1451, 1452, 1453, 1454, 1455, 1456, 1458, 1459, 1460, 1461,  &
     1462, 1457, 1463, 1464, 1465, 1466, 1467, 1468, 1469, 1470, 1471, 1472, 1476, 1477, 1478, 1479, 1480, 1481, 1482, 1483,  &
     1484, 1475, 1485, 1486, 1487, 1488, 1489, 1490, 1491, 1492, 1493, 1473, 1494, 1495, 1496, 1497, 1498, 1499, 1500, 1501,  &
     1502, 1474, 1503, 1504, 1510, 1506, 1507, 1505, 1508, 1509, 1511, 1512, 1515, 1513, 1514, 1516, 1517, 1518, 1533, 1520,  &
     1521, 1522, 1534, 1524, 1525, 1526, 1519, 1527, 1528, 1529, 1523, 1530, 1531, 1532, 1535, 1536, 1537, 1541, 1538, 1539,  &
     1540, 1542, 1543, 1544, 1559, 1546, 1547, 1548, 1560, 1550, 1551, 1552, 1545, 1553, 1554, 1555, 1549, 1556, 1557, 1558,  &
     1561, 1562, 1563, 1570, 1564, 1565, 1566, 1571, 1567, 1568, 1569, 1572, 1573, 1574, 1583, 1577, 1578, 1579, 1575, 1580,  &
     1581, 1582, 1585, 1586, 1587, 1584, 1588, 1589, 1590, 1576, 1591, 1592, 1593, 1594, 1595, 1596, 1597, 1598, 1643, 1601,  &
     1602, 1603, 1604, 1605, 1646, 1606, 1607, 1608, 1609, 1610, 1645, 1611, 1612, 1613, 1614, 1615, 1644, 1618, 1619, 1620,  &
     1621, 1622, 1599, 1623, 1624, 1625, 1626, 1627, 1600, 1628, 1629, 1630, 1631, 1632, 1616, 1633, 1634, 1635, 1636, 1637,  &
     1617, 1638, 1639, 1640, 1641, 1642, 1647, 1648, 1649, 1650, 1651 ]
    !> Litvin points Operators
    character(len=*), dimension(48), parameter, public :: LITVIN_POINT_OP_LABEL=[               &
       "1       ","2x      ","2y      ","2z      ","3xyz-1  ","3xy-z   ","3-xyz   ","3x-yz   ", &
       "3xyz    ","3x-yz-1 ","3xy-z-1 ","3-xyz-1 ","2-xy    ","4z      ","4z-1    ","2xy     ", &
       "2-yz    ","2yz     ","4x      ","4x-1    ","2-xz    ","4y-1    ","2xz     ","4y      ", &
       "-1      ","mx      ","my      ","mz      ","-3xyz-1 ","-3xy-z  ","-3-xyz  ","-3x-yz  ", &
       "-3xyz   ","-3x-yz-1","-3xy-z-1","-3-xyz-1","m-xy    ","-4z     ","-4z-1   ","mxy     ", &
       "m-yz    ","myz     ","-4x     ","-4x-1   ","m-xz    ","-4y-1   ","mxz     ","-4y     "]

    character(len=*), dimension(48), parameter, public :: LITVIN_POINT_OP=[  &
       "x,y,z   ", "x,-y,-z ", "-x,y,-z ", "-x,-y,z ", "y,z,x   ",           &
       "y,-z,-x ", "-y,z,-x ", "-y,-z,x ", "z,x,y   ", "z,-x,-y ",           &
       "-z,x,-y ", "-z,-x,y ", "-y,-x,-z", "-y,x,z  ", "y,-x,z  ",           &
       "y,x,-z  ", "-x,-z,-y", "-x,z,y  ", "x,-z,y  ", "x,z,-y  ",           &
       "-z,-y,-x", "-z,y,x  ", "z,-y,x  ", "z,y,-x  ", "-x,-y,-z",           &
       "-x,y,z  ", "x,-y,z  ", "x,y,-z  ", "-y,-z,-x", "-y,z,x  ",           &
       "y,-z,x  ", "y,z,-x  ", "-z,-x,-y", "-z,x,y  ", "z,-x,y  ",           &
       "z,x,-y  ", "y,x,z   ", "y,-x,-z ", "-y,x,-z ", "-y,-x,z ",           &
       "x,z,y   ", "x,-z,-y ", "-x,z,-y ", "-x,-z,y ", "z,y,x   ",           &
       "z,-y,-x ", "-z,y,-x ", "-z,-y,x "]

    character(len=*), dimension(24), parameter, public :: LITVIN_POINT_OP_HEX_LABEL=[  &
       "1    ","6z   ","3z   ","2z   ","3z-1 ","6z-1 ","2x   ","21   ",                &
       "2xy  ","22   ","2y   ","23   ","-1   ","-6z  ","-3z  ","mz   ",                &
       "-3z-1","-6z-1","mx   ","m1   ","mxy  ","m2   ","my   ","m3   "]

    character(len=*), dimension(24), parameter, public :: LITVIN_POINT_OP_HEX=[       &
       "x,y,z     ","x-y,x,z   ","-y,x-y,z  ","-x,-y,z   ","-x+y,-x,z ","y,-x+y,z  ", &
       "x-y,-y,-z ","x,x-y,-z  ","y,x,-z    ","-x+y,y,-z ","-x,-x+y,-z","-y,-x,-z  ", &
       "-x,-y,-z  ","-x+y,-x,-z","y,-x+y,-z ","x,y,-z    ","x-y,x,-z  ","-y,x-y,-z ", &
       "-x+y,y,z  ","-x,-x+y,z ","-y,-x,z   ","x-y,-y,z  ","x,x-y,z   ","y,x,z     "]

    !> Miller & Love Notation
    character(len=*), dimension(24), parameter, public :: ML_D6H=[                &
       " 1"," 3"," 5"," 4"," 6"," 2"," 9"," 7","11","12","10"," 8","13","15","17",&
       "16","18","14","21","19","23","24","22","20"]

    character(len=*), dimension(48), parameter, public :: ML_OH=[                 &
       " 1"," 4"," 3"," 2"," 9","10","12","11"," 5"," 7"," 6"," 8","16","13","15",&
       "14","20","18","17","19","24","23","22","21","25","28","27","26","33","34",&
       "36","35","29","31","30","32","40","37","39","38","44","42","41","43","48",&
       "47","46","45"]

    !> Matrix Types For Rotational Operators In Conventional Basis: 1->24 Oh, 25->36 D6h
    Integer,  dimension(36,3,3), parameter, public :: MOD6 = reshape ( [     &
       1,-1,-1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,-1,                   &
      -1, 1, 0, 0, 0, 0, 1, 0,-1,-1, 0, 1, 0, 1,-1, 0,-1, 1,                   &
       0, 0, 0, 0, 1,-1,-1, 1, 0, 0, 0, 0, 1,-1,-1, 1, 0, 0,                   &
       0, 0, 0, 0, 0, 0, 0, 1,-1, 0,-1, 1, 1, 0,-1,-1, 0, 1,                   &
       0, 0, 0, 0, 0, 0, 0, 0, 1,-1,-1, 1, 0, 0, 0, 0, 0, 0,                   &
       0, 0,-1, 1, 1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,                   &
       0, 0, 0, 0, 0, 0, 0, 0, 1,-1, 1,-1, 1,-1, 1,-1, 0, 0,                   &
       0, 0, 0, 0, 0, 0, 0,-1, 1, 0, 1,-1, 1,-1, 0,-1, 1, 0,                   &
       1,-1, 1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,                   &
       0, 0, 1,-1, 1,-1, 1,-1, 0,-1, 1, 0, 0,-1, 1, 0, 1,-1,                   &
       0, 0, 0, 0, 1,-1, 1,-1, 0, 0, 0, 0, 0, 0, 0, 0,-1, 1,                   &
      -1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,                   &
       0, 0, 0, 0, 1, 1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,                   &
       0, 0, 1, 1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,                   &
       0, 0, 0, 0, 0, 0, 0, 0, 1, 1,-1,-1, 0, 0, 0, 0, 1, 1,                   &
      -1,-1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,                   &
       1, 1,-1,-1, 0, 0, 0, 0, 0, 0, 0, 0,-1,-1, 1, 1, 0, 0,                   &
       0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1,-1,-1,-1,-1,-1,-1], [36,3,3] )

    !> Point Group Symbols
    character(len=*), dimension(42), parameter, public :: POINT_GROUP=[   &
       "1    ","-1   ","2    ","m    ","2/m  ","222  ","mm2  ","m2m  ",   &
       "2mm  ","mmm  ","4    ","-4   ","4/m  ","422  ","4mm  ","-42m ",   &
       "-4m2 ","4/mmm","3    ","-3   ","32   ","3m   ","-3m  ","312  ",   &
       "31m  ","-31m ","6    ","-6   ","6/m  ","622  ","6mm  ","-62m ",   &
       "-6m2 ","6/mmm","23   ","m-3  ","432  ","-43m ","m-3m ","m3   ",   &
       "m3m  ","-3m1 "]

    !> System type
    character(len=*), dimension(7) , parameter, public:: SYS_CRY =[    &
       "Triclinic   ","Monoclinic  ","Orthorhombic","Tetragonal  ",    &
       "Trigonal    ","Hexagonal   ","Cubic       "]

    !> X_D6H
    character(len=*), dimension(24), parameter, public   :: X_D6H = [        &
       "( x  ,   y, z)","(  -y, x-y, z)","(-x+y,-x  , z)","(-x  ,  -y, z)",  &
       "(   y,-x+y, z)","( x-y, x  , z)","(   y, x  ,-z)","( x-y,  -y,-z)",  &
       "(-x  ,-x+y,-z)","(  -y,-x  ,-z)","(-x+y,   y,-z)","( x  , x-y,-z)",  &
       "(-x  ,  -y,-z)","(   y,-x+y,-z)","( x-y, x  ,-z)","( x  ,   y,-z)",  &
       "(  -y, x-y,-z)","(-x+y,-x  ,-z)","(  -y,-x  , z)","(-x+y,   y, z)",  &
       "( x  , x-y, z)","(   y, x  , z)","( x-y,  -y, z)","(-x  ,-x+y, z)"]

    character(len=*), dimension(48), parameter, public  :: X_OH = [                   &
       "( x, y, z)","(-x,-y, z)","(-x, y,-z)","( x,-y,-z)","( z, x, y)","( z,-x,-y)", &
       "(-z,-x, y)","(-z, x,-y)","( y, z, x)","(-y, z,-x)","( y,-z,-x)","(-y,-z, x)", &
       "( y, x,-z)","(-y,-x,-z)","( y,-x, z)","(-y, x, z)","( x, z,-y)","(-x, z, y)", &
       "(-x,-z,-y)","( x,-z, y)","( z, y,-x)","( z,-y, x)","(-z, y, x)","(-z,-y,-x)", &
       "(-x,-y,-z)","( x, y,-z)","( x,-y, z)","(-x, y, z)","(-z,-x,-y)","(-z, x, y)", &
       "( z, x,-y)","( z,-x, y)","(-y,-z,-x)","( y,-z, x)","(-y, z, x)","( y, z,-x)", &
       "(-y,-x, z)","( y, x, z)","(-y, x,-z)","( y,-x,-z)","(-x,-z, y)","( x,-z,-y)", &
       "( x, z, y)","(-x, z,-y)","(-z,-y, x)","(-z, y,-x)","( z,-y,-x)","( z, y, x)"]

    !> Zak Notation
    character(len=*), dimension(24), parameter, public :: ZAK_D6H =[            &
       "   E   "," C(z)_3","C(2z)_3","  C_2  ","C(5z)_6"," C(z)_6","  U(xy)",   &
       "  U(x) ","  U(y) ","  U(3) ","  U(2) ","  U(1) ","   I   ","S(5z)_6",   &
       " S(z)_6","  s(z) "," S(z)_3","S(2z)_3"," s(xy) ","  s(x) ","  s(y) ",   &
       "  s(3) ","  s(2) ","  s(1) "]

    character(len=*), dimension(48), parameter, public :: ZAK_OH =[             &
       "     E     ","    U(z)   ","    U(y)   ","    U(x)   ","  C(xyz)_3 ",   &
       " C(-xy-z)_3"," C(x-y-z)_3"," C(-x-yz)_3"," C(2xyz)_3 ","C(2x-y-z)_3",   &
       " C(2x-yz)_3","C(-2xy-z)_3","    U(xy)  ","   U(-xy)  ","   C(3z)_4 ",   &
       "   C(z)_4  ","   C(3x)_4 ","    U(yz)  ","   U(y-z)  ","   C(x)_4  ",   &
       "   C(y)_4  ","    U(xz)  ","   C(3y)_4 ","   U(x-z)  ","      I    ",   &
       "    s(z)   ","    s(y)   ","    s(x)   "," S(5xyz)_6 ","S(-5xy-z)_6",   &
       "S(5x-y-z)_6","S(-5x-yz)_6","  S(xyz)_6 "," S(x-y-z)_6"," S(-x-yz)_6",   &
       " S(-xy-z)_6","    s(xy)  ","   s(-xy)  ","   S(z)_4  ","  S(3z)_4  ",   &
       "   S(x)_4  ","    s(yz)  ","   s(y-z)  ","  S(3x)_4  ","  S(3y)_4  ",   &
       "    s(xz)  ","   S(y)_4  ","   s(x-z)  "]


    Interface
       Module Function Get_Compact_HM(HM) Result(C_HM)
          !> Get a compact Hermann-Maugin symbol from a
          !> Hermann-Maugin symbol.
          character(len=*), intent(in) :: HM    !> Hermann-Maugin symbol
          character(len=:), allocatable :: C_HM !> Compact Hermann-Maugin symbol
       End Function Get_Compact_HM

       Module Function Get_HM_Compact_HM(C_HM) Result(HM)
          !> Get a Hermann-Maugin symbol from a compact
          !> Hermann-Maugin symbol.
          character(len=*), intent(in) :: C_HM !> Compact Hermann-Maugin symbol
          character(len=:), allocatable :: HM  !> Hermann-Maugin symbol
       End Function Get_HM_Compact_HM

       Module Function Get_IT_Generators(Spg) Result(StrGen)
          !> Returns a string with the list of the generators (as given
          !> in the IT Crystallography) corresponding to the space group
          !> of symbol "spg".
          !> Generators are given in the Jone's Faithful notation and
          !> the separator is the symbol ";".
          !
          !> An example: R-3 c
          !> gener = " x+1/3,y+2/3,z+2/3; -y,x-y,z; -y,-x,z+1/2"
          !> The variable is the string contained between the quotes.
          character(len=*), intent(in)  :: spg    !> space group symbol: Hermann-Maugin | Hall | number
          character(len=:), allocatable :: Strgen !> list of generators
       End Function Get_IT_Generators

       Module Function Get_Shubnikov_Info(r) Result(info)
         !> Returns the row r from the shubnikov_info table
         integer,             intent(in) :: r      !> Row
         type(Shub_Spgr_Info_Type)       :: info   !> Tabulated data of a Shubnikov group
       End Function Get_Shubnikov_Info

       Module Subroutine Get_SpaceGroup_Symbols(Str, HM, Hall, IT, C_HM)
          !> From a valid space group name returns the rest of possible names
          character(len=*),           intent(in)  :: Str  !> space group name
          character(len=*), optional, intent(out) :: HM   !> Hermann-Maugin symbol
          character(len=*), optional, intent(out) :: Hall !> Hall symbol
          integer,          optional, intent(out) :: IT   !> Number of the space group according to IT
          character(len=*), optional, Intent(out) :: C_HM !> Compact Hermann-Maugin symbol
       End Subroutine Get_SpaceGroup_Symbols

       Module Function Get_Spgr_Info(r) Result(info)
         !> Returns the row r from the spgr_info table
         integer,             intent(in) :: r      ! Row
         type(Spgr_Info_Type)            :: info   ! Tabulated data of a crystallographic space group
       End Function Get_Spgr_Info

       Module Subroutine Set_IT_Gen()
          !---- Arguments ----!
       End Subroutine Set_IT_Gen

       Module Subroutine Set_Shubnikov_Info()
          !> Set table shubnikov_info:
          !>     ID Number of BNS
          !>     BNS symbol
          !>     ID number of OG
          !>     OG symbol
          !>     Proposed standard symbol (UNI symbol)
          !>     Magnetic Hall symbol
          !>     String with generators of the UNI symbol
       End Subroutine Set_Shubnikov_Info

       Module Subroutine Set_Spgr_Info()
          !> Set table spgr_info:
          !>     Number of the Space Group
          !>     Hermann-Mauguin Symbol
          !>     Hall symbol
          !>     Laue Group
          !>     Point Group
          !>     Asymmetric unit in direct space.
          !>     Miscellaneous Information depending on crystal system:
          !>         Monoclinic         b           c           a
          !>                         abc  c-ba   abc  ba-c   abc -acb
          !>                         ---------   ---------   --------
          !>         cell choice 1    b1   -b1    c1   -c1    a1  -a1
          !>         cell choice 2    b2   -b2    c2   -c2    a2  -a2
          !>         cell choice 3    b3   -b3    c3   -c3    a3  -a3
          !>         Orthorhombic     ba-c   change of basis abc -> ba-c
          !>                          1      origin choice 1
          !>                          2ba-c  origin choice 2, change basis
          !>                                 abc -> ba-c
          !>         Tetragonal       1      origin choice 1
          !>         Cubic            2      origin choice 2
          !>         Trigonal         H      hexagonal axes
          !>                          R      rhombohedral axes
       End Subroutine Set_Spgr_Info

       Module Subroutine Set_System_Equiv()
          !> Set table system_equiv:
          !>     Schoenflies
          !>     Miller & Love
          !>     Kovalev
          !>     Bradley & Cracknell
          !>     Zak
          !>
          !> Conversion Table    IT - ML - Kov - BC - Zak
          !>
          !> TABLE 6 of "Isotropy Subgroups of the 230 Crystallographic Space Groups", by
          !> Harold T Stokes and Dorian M Hatch, World Scientific, Singapore (1988).
          !>
          !> The transformation operators that take space group elements in the
          !> International setting (International Tables of Crystallography, Hahn 1983)
          !> to space-groups elements in the Miller and Love ( ML, 1967), Kovalev
          !> (Kov,1986) Bradley anb Cracknell (BC, 1972) and Zak (Zak, 1969) settings.
          !>
          !> In the international setting the basis vectors are always those of the
          !> conventional unit cell. In the Trigonal system the primitive basis
          !> vectors are in an obverse relationship given by (2/3 1/3 1/3),
          !> (-1/3 1/3 1/3) and (-1/3, -2/3 1/3).
          !> In ML the same basis vectors are chosen except that for trigonal/rhombohedral
          !> system the reverse setting is adopted, so the primitive basis vectors
          !> are: t1=(1/3 -1/3 1/3), t2=(1/3, 2/3 1/3) and t3=(2/3 1/3 1/3)
          !> In Kovalev the a,b,c axes of the coordinate system are along the
          !> conventional basis vectors of the lattice, however in the trigonal
          !> system an hexagonal system is chosen so that the primitive basis vectors
          !> are a1=(-1 -1 1/3), a2=(1 0 1/3) and a3=(0 1 1/3).
          !> In the setting of BC the axes a,b,c of the coordinate system are chosen
          !> to be the primitive basis vectors t1,t2,t3 as defined in their book.
          !> The setting of Zak the basis vectors are as in the international setting,
          !> but for trigonal/rhombohedral system the primitive basis vectors w.r.t. the selected
          !> hexagonal coordinate system are given by: (1/3 2/3 1) (1/3 -1/3 1)
          !> (-2/3 -1/3 1)
          !>
          !> Symmetry and transformation operators of Space Groups can be given as
          !> 4 x 4 Seitz matrices or as a character string called Jones Faithful
          !> representation. This last representation is that used in this file.
          !>
          !> To transform a symmetry operator "gI" in the international setting into
          !> a symmetry element "g" in one of the other settings, we simply perform
          !> the following operation: g = gT gI gT(-1), where gT is the transformation
          !> given tabulated below.
       End Subroutine Set_System_Equiv

       Module Subroutine Set_Wyckoff_Info()
          !> Set table wyckoff_info:
          !>     Hermann-Maugin symbol
          !>     Number of Wyckoff positions
          !>     Coordinates of Wyckoff positions
       End Subroutine Set_Wyckoff_Info

       Module Subroutine Remove_Shubnikov_Info()
          !> Deallocate table shubnikov_info
       End Subroutine Remove_Shubnikov_Info

       Module Subroutine Remove_Spgr_Info()
          !> Deallocate table spgr_info
       End Subroutine Remove_Spgr_Info

       Module Subroutine Remove_System_Equiv()
          !> Deallocate table system_equiv
       End Subroutine Remove_System_Equiv

       Module Subroutine Remove_Wyckoff_Info()
          !> Deallocate table wyckoff_info
       End Subroutine Remove_Wyckoff_Info

    End Interface

 End Module CFML_Symmetry_Tables
