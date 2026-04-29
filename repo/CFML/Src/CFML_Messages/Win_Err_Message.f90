!!----
!!----  CRYSFML + WINTERACTER
!!----
!!----
!!----
SubModule (CFML_Messages) Win_Err_Message
  Implicit none
   Contains

   !!----
   !!---- ERROR_MESSAGE
   !!----
   !!---- Print an error message on the screen and in "Iunit" if present
   !!----
   !!---- 03/05/2019
   !!
   Module Subroutine Error_Message(Mess, Iunit, Routine, Fatal)
      !---- Arguments ----!
      character(len=*),            intent(in) :: Mess         ! Error information
      integer, optional,           intent(in) :: iunit        ! Write information on Iunit unit
      Character(Len =*), Optional, Intent(In) :: Routine      ! Added for consistency with the CFML_IO_Mess.f90 version.
      Logical,           Optional, Intent(In) :: Fatal        ! Added for consistency with the CFML_IO_Mess.f90 version.

      !> Init
      call WMessageBox(OKOnly, ExclamationIcon, CommonOK, trim(Mess),"CryFML: Error Message")

      if (present(iunit)) then
         write(unit=iunit,fmt="(tr1,a)") "****"
         write(unit=iunit,fmt="(tr1,a)") "**** ERROR: "//trim(Mess)
         write(unit=iunit,fmt="(tr1,a)") "****"
         write(unit=iunit,fmt="(tr1,a)") " "

         If (Present(Routine)) Then
            Write(Unit = iunit, Fmt = "(tr1,a)") "**** PROCEDURE: "//trim(Routine)
         End If

         If (Present(Fatal)) Then
            If (Fatal) Then
               write(unit=iunit,fmt="(tr1,a)") " "
               Write(Unit = iunit, Fmt = "(/tr1,a)") "**** The Program Will Stop Here."
               Stop
            End If
         End If
      end if

   End Subroutine Error_Message

End SubModule Win_Err_Message