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
!!---- MODULE: CFML_MESSAGES_WIN
!!----   INFO:Input / Output General Messages for the use of WINTERACTER
!!----
!!
 Module CFML_Messages_Win
    !---- Use Modules ----!
    use Winteracter, only: YesNo,OKOnly,CommonYes, CommonOK, Modeless, ViewOnly,         &
                           WordWrap, NoMenu, NoToolbar, SystemFixed, EditTextLength,     &
                           ScreenHeight, StopIcon,InformationIcon, ExclamationIcon,      &
                           QuestionIcon, WMessageBox, WindowCloseChild, WindowOpenChild, &
                           WEditFile, WEditPutTextPart, WindowSelect, WInfoEditor,       &
                           CommandLine,WInfoScreen,CourierNew,win_message

    use CFML_GlobalDeps, only: OPS

    !---- Definitions ----!
    implicit none

    private

    !---- List of public subroutines ----!
    public :: Close_Scroll_Window, Error_Message, Info_Message, Question_Message, Warning_Message, &
              Stop_Message, Write_Scroll_Text

    !---- Definitions ----!

    !!----
    !!---- WIN_CONSOLE
    !!----    integer, private :: icwindow
    !!----
    !!----    Code number for Scroll Window
    !!----
    !!---- Update: March - 2005
    !!
    integer, public :: win_console= -1

    !!--++
    !!--++ WSCROLL
    !!--++    logical, private :: wscroll
    !!--++
    !!--++    Logical variable to indicate if the Scroll Window is
    !!--++    active or not.
    !!--++
    !!--++ Update: March - 2005
    !!
    logical, private :: wscroll = .false.

    Interface
       Module Subroutine Close_Scroll_Window()
       End Subroutine Close_Scroll_Window

       Module Subroutine Error_Message(Mess, Iunit, Routine, Fatal)
          !---- Arguments ----!
          character(len=*),            intent(in) :: Mess         ! Error information
          integer, optional,           intent(in) :: iunit        ! Write information on Iunit unit
          Character(Len =*), Optional, Intent(In) :: Routine      ! Added for consistency with the CFML_IO_Mess.f90 version.
          Logical,           Optional, Intent(In) :: Fatal        ! Added for consistency with the CFML_IO_Mess.f90 version.
       End Subroutine Error_Message

       Module Subroutine Info_Message(Mess, iunit)
          !---- Arguments ----!
          character(len=*), intent(in)           :: Mess        ! Info information
          integer,          intent(in), optional :: iunit       ! Write information on Iunit unit
       End Subroutine Info_Message

       Module Subroutine Question_Message(Mess, Title)
          !---- Argument ----!
          character (len=*),           intent(in) :: Mess        ! Message
          character (len=*), optional, intent(in) :: Title       ! Title in the Pop-up
       End Subroutine Question_Message

       Module Subroutine Stop_Message(Mess, Title)
          !---- Argument ----!
          character (len=*),           intent(in) :: Mess      ! Message
          character (len=*), optional, intent(in) :: Title    ! Title in the Pop-up
       End Subroutine Stop_Message

       Module Subroutine Warning_Message(Mess, Iunit)
          !---- Arguments ----!
          character(len=*), intent(in) :: Mess             ! Message
          integer, optional,intent(in) :: iunit           ! Write information on Iunit unit
       End Subroutine Warning_Message

       Module Subroutine Write_Scroll_Text(Mess,ICmd)
          !---- Argument ----!
          character(len=*), intent(in) :: Mess      ! Message to write
          integer, optional,intent(in) :: ICmd      ! Define the type of the Editor opened
       End Subroutine Write_Scroll_Text

    End Interface

 End Module CFML_Messages_Win
