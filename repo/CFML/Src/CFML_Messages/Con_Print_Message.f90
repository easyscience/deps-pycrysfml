!!----
SubModule (CFML_Messages) Con_Print_Message
  Implicit none
   Contains

   !!----
   !!---- Print_Message
   !!----
   !!----    Print an message on the screen
   !!----
   !!---- 23/04/2019
   !!
   Module Subroutine Print_Message(Mess)
      !---- Arguments ----!
      character(len=*),intent(in) ::  Mess

      !---- Local Variables ----!
      integer :: lon

      lon=len_trim(mess)
      if (lon == 0) then
         write(unit=*,fmt="(a)") "  "
      else
         if (mess(1:1) == "=" .or. mess(2:2) == "=") then
            write(unit=*,fmt="(a)") mess(1:lon)
         else
            write(unit=*,fmt="(a,a)")" =>", mess(1:lon)
         end if
      end if

   End Subroutine Print_Message

End SubModule Con_Print_Message