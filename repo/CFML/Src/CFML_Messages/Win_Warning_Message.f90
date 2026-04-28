!!----
SubModule (CFML_Messages) Win_Warning_Message
  Implicit none
   Contains

   !!----
   !!---- WARNING_MESSAGE
   !!----
   !!----    Print an message on the screen or in "Iunit" if present
   !!----
   !!---- 03/05/2019
   !!
   Module Subroutine Warning_Message(Mess, Iunit)
      !---- Arguments ----!
      character(len=*), intent(in) :: Mess             ! Message
      integer, optional,intent(in) :: iunit           ! Write information on Iunit unit

      call WMessageBox(OKOnly,ExclamationIcon,CommonOK, trim(Mess),"CrysFML: Warning Message")

      if (present(iunit) ) then
         write(unit=iunit,fmt="(tr1,a)") "****"
         write(unit=iunit,fmt="(tr1,a)") "**** WARNING: "//trim(Mess)
         write(unit=iunit,fmt="(tr1,a)") "****"
      end if

   End Subroutine Warning_Message

End SubModule Win_Warning_Message