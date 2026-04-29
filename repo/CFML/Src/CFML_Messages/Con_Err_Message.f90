!!----
SubModule (CFML_Messages) Con_Err_Message
  Implicit none
   Contains

   !!----
   !!---- ERROR_MESSAGE
   !!----
   !!----    Print an error message on the screen or in "Iunit" if present
   !!----    If "routine" is present the subroutine where the occured will be also displayed.
   !!----    If "fatal" is present and .True. the program will stop after the printing.
   !!----
   !!---- 23/04/2019
   !!
   Module Subroutine Error_Message(Mess, Iunit, Routine, Fatal)
      !---- Arguments ----!
      Character(Len=*), Intent(In)           :: Mess      !  In -> Error information
      Integer,          Intent(In), Optional :: Iunit     !  In -> Write information on Iunit unit
      Character(Len=*), Intent(In), Optional :: Routine   !  In -> The subroutine where the error occured
      Logical,          Intent(In), Optional :: Fatal     !  In -> Should the program stop here ?

      !---- Local Variables ----!
      Integer          :: Lun, Lenm
      character(len=1) :: ent

      Lun = 6
      If (Present(Iunit)) Lun = Iunit

      If (Present(Routine)) Then
          Write(Unit = Lun, Fmt = "(A)") " => Error in: "//trim(routine)
      End If

      Lenm = Len_Trim(Mess)
      Write(Unit = Lun, Fmt = "(A)") " => Message Error: "//Mess(1:Lenm)

      If (Present(Fatal)) Then
          If (Fatal) Then
              Write(Unit = Lun, Fmt = "(A)") " => Fatal error: the program stops here."
              Write(Unit= *, Fmt="(/,a)") " => Press <enter> to finish "
              Read (Unit= *, Fmt="(a)") ent
              Stop
          End If
      End If

   End Subroutine Error_Message

End SubModule Con_Err_Message