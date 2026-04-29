!!----
SubModule (CFML_Messages) Win_Info_Message
  Implicit none
   Contains

   !!----
   !!---- INFO_MESSAGE
   !!----
   !!----    Print an message on the screen or in "Iunit" if present
   !!----
   !!---- 03/05/2019
   !!
   Module Subroutine Info_Message(Mess, iunit)
      !---- Arguments ----!
      character(len=*), intent(in)           :: Mess        ! Info information
      integer,          intent(in), optional :: iunit       ! Write information on Iunit unit

      call WMessageBox(OKOnly, InformationIcon, CommonOK, trim(Mess),"CrysFML: Information Message")

      if (present(iunit) ) then
         write(unit=iunit,fmt="(tr1,a)") " "
         write(unit=iunit,fmt="(tr1,a)") " "//trim(Mess)
         write(unit=iunit,fmt="(tr1,a)") " "
      end if

   End Subroutine Info_Message

End SubModule Win_Info_Message