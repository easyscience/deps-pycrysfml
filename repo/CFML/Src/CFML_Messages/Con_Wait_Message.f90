!!----
SubModule (CFML_Messages) Con_Wait_Message
  Implicit none
   Contains

   !!----
   !!---- WAIT_MESSAGE
   !!----
   !!----    Similar to Pause for Console version
   !!----
   !!---- 23/04/2019
   !!
   Module Subroutine Wait_Message(Mess)
      !---- Argument ----!
      character(len=*), optional, intent(in) :: Mess

      !---- Local variable ----!
      character(len=1) :: car

      write(unit=*,fmt="(a)") " "
      if (present(mess)) write(unit=*,fmt="(a)", advance="no") mess
      read(unit=*,fmt="(a)") car

   End Subroutine Wait_Message

End SubModule Con_Wait_Message