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
!!---- This library is free software; you can redistribute it and/or  modify
!!---- it  under  the  terms  of  the  GNU  Lesser General Public License as
!!---- published by the Free Software Foundation; either version  3.0 of the
!!---- License, or (at your option) any later version.
!!----
!!---- This library is distributed in the hope  that it will  be useful, but
!!---- WITHOUT   ANY   WARRANTY;   without   even  the implied  warranty  of
!!---- MERCHANTABILITY  or  FITNESS  FOR  A PARTICULAR PURPOSE.  See the GNU
!!---- Lesser General Public License for more details.
!!----
!!---- You  should  have  received  a  copy of the GNU Lesser General Public
!!---- License along with this library; if not, see
!!---- <http://www.gnu.org/licenses/>.
!!----
!!----
!!---- MODULE: CFML_MESSAGES
!!----   INFO:Input / Output General Messages. It is convenient to use these
!!----        intermediate  procedures  instead  of  Fortran  Write(*,*)  or
!!----        Print*, because it is much more simple to modify a program for
!!----        making a GUI.
!!----        Usually GUI tools and libraries need special  calls  to dialog
!!----        boxes for screen  messages. These  calls  may  be  implemented
!!----        within  this  module  using  the   same  name  procedures. The
!!----        subroutines ERROR_MESSAGE and  INFO_MESSAGE are  just wrappers
!!----        for the actual calls.
!!--..
!!--..        NON-GRAPHICS ZONE
!!--..
!!----
!!
Module CFML_Messages
   !---- Use Modules ----!
   Use CFML_GlobalDeps

   !---- Definitions ----!
   implicit none

   !---- List of public subroutines ----!
   public :: Error_Message, Info_Message, Print_Message, Wait_Message, Write_Scroll_Text

   Interface
      Module Subroutine Error_Message(Mess, Iunit, Routine, Fatal)
         !---- Arguments ----!
         Character(Len=*), Intent(In)           :: Mess
         Integer, Intent(In), Optional :: Iunit
         Character(Len=*), Intent(In), Optional :: Routine
         Logical, Intent(In), Optional :: Fatal
      End Subroutine Error_Message

      Module Subroutine Info_Message(Mess, iunit)
         !---- Arguments ----!
         character(len=*), intent(in)           :: Mess
         integer, intent(in), optional :: iunit
      End Subroutine Info_Message

      Module Subroutine Print_Message(Mess)
         !---- Arguments ----!
         character(len=*), intent(in) ::  Mess
      End Subroutine Print_Message

      Module Subroutine Wait_Message(Mess)
         !---- Argument ----!
         character(len=*), optional, intent(in) :: Mess
      End Subroutine Wait_Message

      Module Subroutine Write_Scroll_Text(Mess)
         !---- Argument ----!
         character(len=*), intent(in) :: Mess
      End Subroutine Write_Scroll_Text

   End Interface

End Module CFML_Messages
