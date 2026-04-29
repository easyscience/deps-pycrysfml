!!----
SubModule (CFML_Messages) Con_Info_Message
  Implicit none
   Contains

   !!----
   !!---- Info_Message
   !!----
   !!----    Print an message on the screen or in "Iunit" if present
   !!----
   !!---- 23/04/2019
   !!
   Module Subroutine Info_Message(Mess, iunit)
      !---- Arguments ----!
      character(len=*), intent(in)           :: Mess   !  In -> Info information
      integer,          intent(in), optional :: iunit  !  In -> Write information on Iunit unit

      !---- Local Variables ----!
      integer :: lun

      lun=6
      if (present(iunit)) lun=iunit
      write(unit=lun,fmt="(a)") " => "//Mess

   End Subroutine Info_Message

End SubModule Con_Info_Message