!!----
SubModule (CFML_Messages) Wmess_ScrollMsg
  Implicit none
   Contains
   !!----
   !!---- CLOSE_SCROLL_WINDOW
   !!----
   !!----    Close the Scroll Window
   !!----
   !!---- 03/05/2019
   !!
   Module Subroutine Close_Scroll_Window()

      if (wscroll) call WindowCloseChild(win_console)
      win_console= -1
      wscroll=.false.

   End Subroutine Close_Scroll_Window

   !!----
   !!---- WRITE_SCROLL_TEXT
   !!----
   !!----    Print the string in a the window
   !!----
   !!---- 03/05/2019
   !!
   Module Subroutine Write_Scroll_Text(Mess,ICmd)
      !---- Argument ----!
      character(len=*), intent(in) :: Mess      ! Message to write
      integer, optional,intent(in) :: ICmd      ! Define the type of the Editor opened

      !---- Local variables ----!
      integer :: iendbuf, ic

      ic=0
      if (present(ICmd)) ic=ICmd

      !> Open the Scroll Window if necessary
      if (.not. wscroll) then
         call WindowOpenChild(win_console,height=nint(WInfoScreen(ScreenHeight)*0.5), &
                              title='CrysFML: Scroll_Window')
         select case (ic)
            case (0)
               call WEditFile(" ",IFlags=ViewOnly+NoMenu+NoToolbar+CommandLine,             &
                                  IFont=CourierNew)
            case (1)
               call WEditFile(" ",IFlags=ViewOnly+NoMenu+NoToolbar,                         &
                                  IFont=CourierNew)
         end select
         wscroll=.true.
      end if

      call WindowSelect(win_console)
      iendbuf=WInfoEditor(win_console,EditTextLength)+1
      call WEditPutTextPart(trim(Mess)//newline,iendbuf)
      call WindowSelect(0)

   End Subroutine Write_Scroll_Text

End SubModule Wmess_ScrollMsg