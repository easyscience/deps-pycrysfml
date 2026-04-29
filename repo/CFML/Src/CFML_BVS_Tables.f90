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
!!---- MODULE: CFML_BVS
!!----   INFO: Subroutines related to calculations of energy or
!!----         configuration properties depending on the crystal structure: BVS, Energy,....
!!----
!!----
!!
 Module CFML_BVS_Tables
    !---- Use Files ----!
    Use CFML_GlobalDeps, only: CP, DP

    !---- Variables ----!
    implicit none

    private

    !---- List of public subroutines ----!

    public :: Remove_Bvs_Table, Remove_BVEL_Table, Remove_Atomic_Properties_Table, Remove_sBVS_Table,&
              Set_Atomic_Properties_Table, Set_Bvel_Table, Set_Bvs_Table, Set_Sbvs_Table, Set_Pauling_Electronegativity, &
              Set_Common_Oxidation_States_Table, Set_Oxidation_States_Table

    !---- Parameters ----!
    integer, parameter, public :: AP_SPECIES_N  =183  ! Number of atomic properties species in:  Ap_Table
    integer, parameter, public :: BVEL_ANIONS_N =1    ! Number of anions known in BVEL Table
    integer, parameter, public :: BVEL_SPECIES_N=132  ! Maximum Number of species in BVEL_Table
    integer, parameter, public :: BVS_ANIONS_N  =14   ! Number of anions known in BVS Table in O"Keefe, Breese, Brown
    integer, parameter, public :: BVS_SPECIES_N =247  ! Maximum Number of species in BVS_Table
    integer, parameter, public :: SBVS_SPECIES_N=168  ! Maximum Number of species in SBVS_Table

    character(len=*), parameter, public, dimension(BVEL_ANIONS_N) :: BVEL_ANIONS = ["O-2 "]          ! Anions known from Stefan Adams and R. Prasada Rao
    character(len=*), parameter, public, dimension(BVS_ANIONS_N)  :: BVS_ANIONS  = ["O-2 ","F-1 ",&  ! Anions known from O'Keefe, Bresse, Brown
                                                                                    "CL-1","BR-1",&
                                                                                    "I-1 ","S-2 ",&
                                                                                    "SE-2","TE-2",&
                                                                                    "N-3 ","P-3 ",&
                                                                                    "AS-3","H-1 ",&
                                                                                    "O-1 ","SE-1"]

    character(len=*), parameter, public, dimension(0:35) :: REF_BVS = [                      &
         "Unknown                                                                         ", &  !0
         "Brown and Altermatt, (1985), Acta Cryst. B41, 244-247 (empirical)               ", &  !1
         "Brese and O'Keeffe, (1991), Acta Cryst. B47, 192-197 (extrapolated)             ", &  !2
         "Adams, 2001, Acta Cryst. B57, 278-287 (includes second neighbours)              ", &  !3
         "Hu et al. (1995) Inorg. Chim. Acta, 232, 161-165.                               ", &  !4
         "I.D.Brown Private communication                                                 ", &  !5
         "Brown et al. (1984) Inorg. Chem. 23, 4506-4508                                  ", &  !6
         "Palenik (1997) Inorg. Chem. 36 4888-4890                                        ", &  !7
         "Kanowitz and Palenik (1998) Inorg. Chem. 37 2086-2088                           ", &  !8
         "Wood and Palenik (1998) Inorg. Chem. 37 4149-4151                               ", &  !9
         "Liu and Thorp (1993) Inorg. Chem. 32 4102-4105                                  ", &  !10
         "Palenik (1997) Inorg. Chem. 36 3394-3397                                        ", &  !11
         "Shields, Raithby, Allen and Motherwell (1999) Acta Cryst.B56, 455-465           ", &  !12
         "Chen, Zhou and Hu (2002) Chinese Sci. Bul. 47, 978-980.                         ", &  !13
         "Kihlbourg (1963) Ark. Kemi 21 471; Schroeder 1975 Acta Cryst. B31, 2294         ", &  !14
         "Allmann (1975) Monatshefte Chem. 106, 779                                       ", &  !15
         "Zachariesen (1978) J.Less Common Metals 62, 1                                   ", &  !16
         "Krivovichev and Brown (2001) Z. Krist. 216, 245                                 ", &  !17
         "Burns, Ewing and Hawthorne (1997) Can. Miner. 35,1551-1570                      ", &  !18
         "Garcia-Rodriguez, et al. (2000) Acta Cryst. B56, 565-569                        ", &  !19
         "Mahapatra et al. (1996) J. Amer.Chem. Soc. 118, 11555                           ", &  !20
         "Wood and Palenik (1999) Inorg. Chem. 38, 1031-1034                              ", &  !21
         "Wood and Palenik (1999) Inorg. Chem. 38, 3926-3930                              ", &  !22
         "Wood, Abboud, Palenik and Palenik (2000) Inorg. Chem. 39, 2065-2068             ", &  !23
         "Tytko, Mehnike and Kurad (1999) Structure and Bonding 93, 1-66                  ", &  !24
         "Gundemann, et al.(1999) J. Phys. Chem. A 103, 4752-4754                         ", &  !25
         "Zocchi (2000) Solid State Sci. 2 383-387                                        ", &  !26
         "Jensen, Palenik and Tiekiak (2001) Polyhedron 20, 2137                          ", &  !27
         "Roulhac and Palenik (2002) Inorg. Chem. 42, 118-121                             ", &  !28
         "Holsa et al.(2002) J.Solid State Chem 165, 48-55                                ", &  !29
         "Trzesowska, Kruszynski & Bartezak (2004) Acta Cryst. B60, 174-178               ", &  !30
         "Locock & Burns (2004) Z.Krist. 219, 267-271                                     ", &  !31
         "J.Rodriguez-Carvajal, Private communication                                     ", &  !32
         "S. Adams and R. Prasada Rao, (2011) Phys. Status Solidi A 208, No. 8, 1746-1753 ", &  !33
         "S. Adams (2013),  Structure and Bonding (eds. Brown & Poeppelmeier) 158, 91-128 ", &  !34
         "Adams S, Moretsky O and Canadell E (2004) Solid State Ionics 168, 281-290       "]    !35

    real(kind=cp), parameter, public, dimension(BVEL_ANIONS_N) :: BVEL_ANIONS_RION = [1.40]             ! Radii Ionic for Anions in BVEL
    real(kind=cp), parameter, public, dimension(BVS_ANIONS_N)  :: BVS_ANIONS_RION  = [1.40,1.19,1.67, & ! Ionic Radii for Anions
                                                                                      1.95,2.16,1.84, &
                                                                                      1.98,2.21,1.71, &
                                                                                      2.12,2.22,2.08, &
                                                                                      1.35,1.80]

    !---- Types Definitions ----!

    !!----
    !!---- TYPE :: Atomic_Properties_Type
    !!--..
    !!
    Type, public :: Atomic_Properties_Type
        integer          :: Z    =0      ! Atomic number
        character(len=4) :: Symb =" "    ! Element with charge
        integer          :: oxs  =0      ! Nominal oxidation state
        integer          :: dox  =0      ! Default oxidation state
        real(kind=cp)    :: Mass =0.0_cp ! Atomic mass in atomic units
        integer          :: n    =0      ! Principal Quantum number (period)
        integer          :: g    =0      ! Group in the periodic table
        integer          :: b    =0      ! Block (s:0, p:1, d:2, f:3)
        real(kind=cp)    :: Rc   =0.0_cp ! Covalent radius
        real(kind=cp)    :: sigma=0.0_cp ! Softness
    End Type Atomic_Properties_Type

    !!----
    !!---- TYPE :: BVEL_PAR_TYPE
    !!--..
    !!
    Type, public :: Bvel_Par_Type
       character(len=5)                       :: symb    =" "   !Symbol of the cation
       real(kind=cp),dimension(BVEL_ANIONS_N) :: Avcoor  =0.0   !Average cation coordination number
       real(kind=cp),dimension(BVEL_ANIONS_N) :: Rzero   =0.0   !Modified Bond-Valence parameter R0
       real(kind=cp),dimension(BVEL_ANIONS_N) :: Rcutoff =0.0   !Cutoff distance in Angstroms
       real(kind=cp),dimension(BVEL_ANIONS_N) :: Dzero   =0.0   !First Morse potential parameter (eV)
       real(kind=cp),dimension(BVEL_ANIONS_N) :: Rmin    =0.0   !Second Morse potential parameter (Angstroms)
       real(kind=cp),dimension(BVEL_ANIONS_N) :: alpha   =0.0   !Third Morse potential parameter (1/b) (Angstroms^-1)
       integer      ,dimension(BVEL_ANIONS_N) :: refnum  =0     !Pointer to reference paper
    End Type Bvel_Par_Type

    !!----
    !!---- TYPE :: BVS_PAR_TYPE
    !!--..
    !!
    Type, public :: Bvs_Par_Type
       character(len=4)                      :: Symb   =" "    ! Chemical symbol
       real(kind=cp),dimension(BVS_ANIONS_N) :: d0     =0.0    ! D0 Parameter
       real(kind=cp),dimension(BVS_ANIONS_N) :: b_par  =0.0    ! B Parameter
       integer      ,dimension(BVS_ANIONS_N) :: refnum =0      ! Integer pointing to the reference paper
    End Type Bvs_Par_Type

    !!----
    !!---- TYPE :: sBVS_PAR_TYPE
    !!--..
    !!
    Type, public :: sBvs_Par_Type
       character(len=4)                      :: Symb   =" "    ! Chemical symbol
       real(kind=cp),dimension(BVS_ANIONS_N) :: d0     =0.0    ! D0 Parameter
       real(kind=cp),dimension(BVS_ANIONS_N) :: b_par  =0.0    ! B Parameter
       real(kind=cp),dimension(bvs_anions_n) :: cn     =0.0    ! Preferred Coordination
       real(kind=cp),dimension(bvs_anions_n) :: ctoff  =0.0    ! Cutoff distance
       integer      ,dimension(BVS_ANIONS_N) :: refnum =0      ! Integer pointing to the reference paper
    End Type sBvs_Par_Type

    !---- Variables ----!

    real(kind=cp), public, allocatable, dimension(:,:) :: Table_Alpha    ! Matrix N_Species x N_Species of Alpha (equivalent to 1/b in BVS) parameters for BVEL
    real(kind=cp), public, allocatable, dimension(:,:) :: Table_Avcoor   ! Matrix N_Species x N_Species of Average coordination parameters for BVEL
    real(kind=cp), public, allocatable, dimension(:,:) :: Table_b        ! Matrix N_Species x N_Species of B parameters for BVS
    real(kind=cp), public, allocatable, dimension(:,:) :: Table_d0       ! Matrix N_Species x N_Species of D0 for BVS
    real(kind=cp), public, allocatable, dimension(:,:) :: Table_DZero    ! Matrix N_Species x N_Species of DZero for BVS
    real(kind=cp), public, allocatable, dimension(:,:) :: Table_RCutOff  ! Matrix N_Species x N_Species of RCutOFFD for BVS
    real(kind=cp), public, allocatable, dimension(:,:) :: Table_RMin     ! Matrix N_Species x N_Species of RMin for BVS
    real(kind=cp), public, allocatable, dimension(:,:) :: Table_RZero    ! Matrix N_Species x N_Species of RZero for BVS
    real(kind=dp), public, allocatable, dimension(:)   :: PaulingX       ! Table of electronegativities

    integer, public, allocatable, dimension(:,:) :: Table_ref              ! Matrix N_Species x N_Species with references for BVS parameters
    integer, public, allocatable, dimension(:,:) :: Common_OxStates_Table  ! Tables of Common Oxidation States
    integer, public, allocatable, dimension(:,:) :: OxStates_Table         ! Tables of Oxidation States

    Type(Atomic_Properties_Type), public, allocatable, dimension(:) :: Ap_Table
    Type(Bvel_Par_Type),          public, allocatable, dimension(:) :: BVEL_Table
    Type(Bvs_Par_Type),           public, allocatable, dimension(:) :: BVS_Table
    Type(sBvs_Par_Type),          public, allocatable, dimension(:) :: sBVS_Table ! SBVS Parameters for calculations (only alkali chalcogenides are available)

    Interface
       Module Subroutine Set_Atomic_Properties_Table()
         !> Set Ap_Table.
         !> The Ap_Table contains the parameters for single atomic properties, including softness,
         !> for determining D0 and Rmin parameters of the Morse potential for non-tabulated
         !> values. The values are taken from supplementary material of Stefan Adams' paper.
         !> "Practical considerations in determining bond-valence parameters"
         !> Structure and Bonding (2014) 158, 91-128 / DOI 10.1007/430_2013_96.
       End Subroutine Set_Atomic_Properties_Table

       Module Subroutine Set_BVEL_Table()
         !> Set BVEL_Table.
         !> BVEL_Table contains the parameters for BVEL from Stefan Adams and R. Prasada Rao.
         !> "High power lithium ion battery materials by computational design"
         !>  Phys. Status Solidi A 208, No. 8, 1746, 1753 (2011) / DOI 10.1002/pssa.201001116
         !> Only one anion is available (O-2) for the moment.
       End Subroutine Set_BVEL_Table

       Module Subroutine Set_BVS_Table()
         !> Set BVS_Table.
         !> BVS_Table contains the parameters for BVS from O'Keefe, Bresse, Brown.
       End Subroutine Set_BVS_Table

       Module Subroutine Set_Common_Oxidation_States_Table()
         !> Set Common_OxStates_Table
         !> Common_OxStates_Table contains the usual oxidation states for
         !> every element of the periodic table.
       End Subroutine Set_Common_Oxidation_States_Table

       Module Subroutine Set_Oxidation_States_Table()
         !> Set OxStates_Table.
         !> OxStates_Table contains the possible oxidation states for every
         !> element of the periodic table.
       End Subroutine Set_Oxidation_States_Table

       Module Subroutine Set_Pauling_Electronegativity()
         !> Set PaulingX table.
         !> PaulingX contains the Pauling electronegativity for every element
         !> of the periodic table.
       End Subroutine Set_Pauling_Electronegativity

       Module Subroutine Set_SBVS_Table()
         !> Set sBVS_Table.
         !> sBVS_Table contains the parameters for soft-BVS from Stefan Adams.
         !> "Practical Considerations in Determining Bond-Valence Parameters"
         !> Structure and Bonding (2014) 158: 91-128
         !> DOI: 10.1007/430_2013_96.
       End Subroutine Set_SBVS_Table

       Module Subroutine Remove_Atomic_Properties_Table()
         !> Deallocate Ap_Table
       End Subroutine Remove_Atomic_Properties_Table

       Module Subroutine Remove_BVEL_Table()
         !> Deallocate BVEL_Table
       End Subroutine Remove_BVEL_Table

       Module Subroutine Remove_BVS_Table()
         !> Deallocate BVS_Table
       End Subroutine Remove_BVS_Table

       Module Subroutine Remove_sBVS_Table()
         !> Deallocate sBVS_Table
       End Subroutine Remove_sBVS_Table

    End Interface

 End Module CFML_BVS_Tables
