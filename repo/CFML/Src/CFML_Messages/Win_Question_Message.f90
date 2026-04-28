!!----
SubModule (CFML_Messages) Win_Question_Message
  Implicit none
   Contains
   !!----
   !!---- QUESTION_MESSAGE
   !!----
   !!----    Print an question on the screen
   !!----
   !!---- 03/05/2019
   !!
   Module Subroutine Question_Message(Mess, Title)
      !---- Argument ----!
      character (len=*),           intent(in) :: Mess        ! Message
      character (len=*), optional, intent(in) :: Title       ! Title in the Pop-up

      !---- Variable ----!
      character(:), allocatable :: ch_title

      ch_title=" "
      if (present(title)) ch_title=trim(title)

      call WMessageBox(YesNo,QuestionIcon,CommonYes, trim(Mess),trim(ch_title))

   End Subroutine Question_Message



End SubModule Win_Question_Message