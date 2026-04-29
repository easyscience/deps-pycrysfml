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
!!---- MODULE:CFML_Bond_Tables
!!----   INFO: This module constains a simple subroutine providing the list
!!----         of the usual bonds between atoms. There are three possible
!!----         mode for each bonds.
!!----         Type
!!----           1      Simple Bond
!!----           2      Double Bond or Shorter
!!----           3      Triple Bond or Shortest
!!----
!!
 Module CFML_Bonds_Tables
    !---- Use Modules ----!
    Use CFML_GlobalDeps,        only: CP, CFML_debug
    Use CFML_Scattering_Tables, only: Get_Chem_Symb, Get_Z_Symb

    !---- Variables ----!
    implicit none

    private

    !---- List of public subroutines ----!
    public  :: Get_Bonds_Table, Remove_Bonds_Table, Set_Bonds_Table


    !---- Definitions ----!
    logical :: Set_BT_Variable = .false.                                      ! Define if the Variable Bond_Length_Table was loaded or not

    real(kind=cp), public, allocatable, dimension(:,:,:) :: Bond_Length_Table ! Bonds length between type of atoms. Order by Z


    !---- Overlapp ----!
    Interface  Get_Bonds_Table
       Module Procedure Get_Bonds_Table_Symbol
       Module Procedure Get_Bonds_Table_Z
    End Interface

    Interface
       Module Function Get_Bonds_Table_Symbol(Symb1,Symb2) Result(Bonds)
          !> Get typical bond distances for single, double and triple
          !> bonds in organic compounds.
          character(len=*),           intent(in)  :: Symb1 !> Atomic symbol
          character(len=*),           intent(in)  :: Symb2 !> Atomic symbol
          real(kind=cp), dimension(3)             :: Bonds !> Bond distances
       End Function Get_Bonds_Table_Symbol

       Module Function Get_Bonds_Table_Z(Z1,Z2) Result(Bonds)
          !> Get typical bond distances for single, double and triple
          !> bonds in organic compounds.
          integer,                    intent(in)  :: Z1    !> Atomic number
          integer,                    intent(in)  :: Z2    !> Atomic number
          real(kind=cp),dimension(3)              :: Bonds !> Bond distances
       End Function Get_Bonds_Table_Z

       Module Subroutine Remove_Bonds_Table()
          !> Remove from memory the bond_length_table (see set_bonds_table).
       End Subroutine Remove_Bonds_Table

       Module Subroutine Set_Bonds_Table()
          !> Set bond_length_table, the table containing typical bond
          !> distances for single, double and triple bonds in organic
          !> compunds.
       End Subroutine Set_Bonds_Table

    End Interface

 Contains

End Module CFML_Bonds_Tables
