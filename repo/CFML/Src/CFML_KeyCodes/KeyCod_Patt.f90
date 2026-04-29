!!
Submodule (CFML_KeyCodes) KeyCod_Patt
   implicit none

   Contains

   !!----
   !!---- SUBROUTINE READ_REFCODES_PATT
   !!----
   !!----
   !!---- Update: 12/05/2022
   !!
   Module Subroutine Read_RefCodes_PATT(ffile, n_ini, n_end, IPatt, G)
      !---- Arguments ----!
      Type(file_type),       intent(in)     :: ffile
      integer,               intent(in)     :: n_ini
      integer,               intent(in)     :: n_end
      integer,               intent(in)     :: IPatt
      type(GenParList_Type), intent(in out) :: G

      !---- Local Variables ----!
      integer           :: i,k

      !> Init
      call clear_error()

      do i=n_ini,n_end
         !> load information on line variable
         line=adjustl(ffile%line(i)%str)
         if (line(1:1) ==" ") cycle
         if (line(1:1) =="!") cycle
         if (len_trim(line) == 0) cycle

         k=index(line,"!")
         if( k /= 0) line=line(:k-1)

         !> Directives
         select case (u_case(line(1:4)))
            case ("FIX ", "FIXE")   ! FIX
               call ReadCode_FIX_PATT(line, IPatt, G)
               if (err_CFML%Flag) return

            case ("VARY")    ! VARY
               call ReadCode_VARY_PATT(line, IPatt, G)
               if (err_CFML%Flag) return

            case ("EQUA") ! Equal (Constraints)
               call ReadCode_EQUAL_PATT(line, IPatt, G)
               if (err_CFML%Flag) return
         end select
      end do

   End Subroutine Read_RefCodes_PATT

   !!--++
   !!--++ SUBROUTINE SPLIT_REFCOD_PATT
   !!--++
   !!--++ Update: May - 2022
   !!
   Module Subroutine Split_RefCod_Patt(String, Nc, Ikeys, IPatt, Keys)
      !---- Arguments ----!
      character(len=*),               intent(in)  :: String   ! String line
      integer,                        intent(out) :: Nc       ! Number of directives
      integer, dimension(:),          intent(out) :: IKeys    ! Index respect to KEY_PATT
      integer, dimension(:),          intent(out) :: IPatt    ! Index for Pattern
      character(len=*), dimension(:), intent(out) :: Keys     ! Directive

      !---- Local Variables ----!
      integer          :: i,j,n,iv,npos
      !character(len=2) :: car

      !> Init
      Nc=0; Ikeys=0; IPatt=0; Keys=" "
      if (len_trim(string) == 0) return

      call get_words(string, dire, n)

      loop1: do i=1,n
         !> Patterns
         npos=index(u_case(dire(i)),'_PATT')
         if (npos > 0) then
            call get_num(dire(i)(npos+5:),vet,ivet,iv)
            if (iv /= 1) then
               call set_error(1,'Bad format to include the Pattern!')
               return
            end if
            IPatt(i)=ivet(1)    ! Positive values for Patterns references
            dire(i)=dire(i)(:npos-1)
         end if

         do j=1, NKEY_PATT
            if (trim(KEY_PATT(j)) == trim(u_case(dire(i)))) then
               nc=nc+1
               ikeys(nc)=j
               keys(nc)=trim(dire(i))
               cycle loop1
            end if
         end do ! Key_Patt

      end do loop1 ! General

   End Subroutine Split_RefCod_Patt

   !!----
   !!---- SUBROUTINE READCODE_FIX_PATT
   !!----
   !!---- Update: May - 2022
   !!
   Module Subroutine ReadCode_FIX_PATT(String, IPatt, G)
      !---- Arguments ----!
      character(len=*),        intent(in)   :: String
      integer,                 intent(in)   :: IPatt
      type(GenParList_Type), intent(in out) :: G

      !---- Local Variables ----!
      integer, parameter :: NMAX = 20

      character(len=40),dimension(NMAX) :: dir_gen
      character(len=3)                  :: car

      integer, dimension(NMAX)          :: Ind_dir, IPat_dir
      integer                           :: npos, nlong, n_dir,  nc
      integer                           :: j,k,iv

      !> Init
      call clear_error()

      !> copy
      line=trim(adjustl(string))

      car=u_case(line(1:3))
      if (car /= 'FIX') then
         call set_error(1,'Wrong Directive for FIX instruction: '//trim(line))
         return
      end if

      !> Cut FIX word
      call cut_string(line,nlong)

      !> general directives
      call split_refcod_Patt(line, n_dir, ind_dir, Ipat_dir, dir_gen)

      if (n_dir > 0) then
         call get_words(line, dire, nc)
         do j=1,n_dir
            iv=IPatt
            if (Ipat_dir(j) /=0) iv=ipat_dir(j)

            if (n_dir == nc) then
               call Set_RefCodes_Patt('FIX', ind_dir(j), iv, G)
            else
               do k=n_dir+1,nc
                  npos=index(u_case(dire(k)),'PAT')
                  if (npos > 0) then
                     call get_num(dire(k)(npos+3:),vet,ivet,iv)
                     if (iv == 1) call Set_RefCodes_Patt('FIX', ind_dir(j), ivet(1), G)
                  end if
               end do !k
            end if

            do k=n_dir+1,nc
               call Set_RefCodes_Patt('FIX', ind_dir(j), iv, G)
            end do ! Objects
         end do ! n_dir
      end if

   End Subroutine ReadCode_FIX_PATT

   !!----
   !!---- SUBROUTINE READCODE_VARY_PATT
   !!----
   !!---- Update: May - 2022
   !!
   Module Subroutine ReadCode_VARY_PATT(String, IPatt, G)
      !---- Arguments ----!
      character(len=*),      intent(in)     :: String
      integer,               intent(in)     :: IPatt
      type(GenParList_Type), intent(in out) :: G

      !---- Local Variables ----!
      integer, parameter :: NMAX = 20 ! Maximum number of words in a line

      character(len=40),dimension(NMAX) :: dir_gen
      character(len=3)                  :: car

      integer, dimension(NMAX)          :: Ind_dir, IPat_dir
      integer                           :: npos, nlong, n_dir,  nc
      integer                           :: j,k,iv


      !> Init
      call clear_error()

      !> copy
      line=trim(adjustl(string))

      car=u_case(line(1:3))
      if (car /= 'VAR') then
         call set_error(1,'Wrong Directive for VARY instruction: '//trim(line))
         return
      end if

      !> Delete VARY word
      call cut_string(line,nlong)

      !> general directives
      call split_refcod_Patt(line, n_dir, ind_dir, Ipat_dir, dir_gen)
      if (n_dir ==0) return

      call get_words(line, dire, nc)
      do j=1,n_dir
         iv=ipatt
         if (ipat_dir(j) /=0) iv=ipat_dir(j)

         if (n_dir == nc) then
            if (ind_dir(j) ==0) then
               write(car,fmt='(i3)') iv
               car=adjustl(car)
               call Vary_GPList_Par(G,trim(dir_gen(1))//'_PATT'//trim(car))
            else
               call Set_RefCodes_Patt('VARY', ind_dir(j), iv, G)
            end if
         else
            do k=n_dir+1,nc
               npos=index(u_case(dire(k)),'PATT')

               if (npos > 0) then
                  call get_num(dire(k)(npos+4:),vet,ivet,iv)
                  if (iv == 1) then
                     if (ind_dir(j) ==0) then
                        write(car,fmt='(i3)') ivet(1)
                        car=adjustl(car)
                        call Vary_GPList_Par(G,trim(dir_gen(1))//'_PATT'//trim(car))
                     else
                        call Set_RefCodes_Patt('VARY', ind_dir(j), ivet(1), G)
                     end if
                  end if
               end if

            end do !k
         end if

      end do ! ndir

      !> Updating Codes
      call Update_GPList_Code(G)

   End Subroutine ReadCode_VARY_PATT

   !!--++
   !!--++ SUBROUTINE SETL_REFCODES_PATT
   !!--++
   !!--++
   !!--++ Update: May - 2022
   !!
   Module Subroutine Set_RefCodes_PATT(Keyword, Npar,  IPatt, G)
      !---- Arguments ----!
      character(len=*),            intent(in)     :: Keyword     ! VARY/FIX/....
      integer,                     intent(in)     :: NPar        ! Specific parameter U,V,W,...
      integer,                     intent(in)     :: IPatt       ! Number of Pattern
      type(GenParList_Type),       intent(in out) :: G

      !---- Local variables ----!
      integer          :: i
      character(len=4) :: cdire, car, car_n
      character(len=20):: str

      !> Init
      call clear_error()

      !> keyword
      cdire=u_case(Keyword)

      write(car,fmt='(i3)') ipatt
      car=adjustl(car)

      select case (trim(cdire))
         !> FIX
         case ("FIX")
            select case (Npar)
               case (0)
                  call set_error(1," Error in the Refinement Code for Patterns!")
                  return

               case (4) ! UVW
                  call Fix_GPList_Par(G,'U_PATT'//trim(car))
                  call Fix_GPList_Par(G,'V_PATT'//trim(car))
                  call Fix_GPList_Par(G,'W_PATT'//trim(car))

               case (17) ! BKG (All)
                  do i=1,NMax_Bckgd_Params
                     write(car_n,fmt='(i3)') i
                     car_n=adjustl(car_n)
                     call Fix_GPList_Par(G,'BKG'//trim(car_n)//'_PATT'//trim(car))
                  end do

               case (21) ! Not used yet!

               case (28) ! SDSL
                  call Fix_GPList_Par(G,'SD_PATT'//trim(car))
                  call Fix_GPList_Par(G,'SL_PATT'//trim(car))

               case (34) ! SIG (All)
                  do i=0,2
                     write(car_n,fmt='(i3)') i
                     car_n=adjustl(car_n)
                     call Fix_GPList_Par(G,'SIG'//trim(car_n)//'_PATT'//trim(car))
                  end do

               case (38) ! GAM (All)
                  do i=0,2
                     write(car_n,fmt='(i3)') i
                     car_n=adjustl(car_n)
                     call Fix_GPList_Par(G,'GAM'//trim(car_n)//'_PATT'//trim(car))
                  end do

               case (49) ! FWHM Generic
                  do i=0, NMax_FWHM_Param
                     write(car_n,fmt='(i3)') i
                     car_n=adjustl(car_n)
                     call Fix_GPList_Par(G,'FW'//trim(car_n)//'_PATT'//trim(car))
                  end do

               case (60) ! FWHM-G (All)
                  do i=0, NMax_FWHM_Param
                     write(car_n,fmt='(i3)') i
                     car_n=adjustl(car_n)
                     call Fix_GPList_Par(G,'FWG'//trim(car_n)//'_PATT'//trim(car))
                  end do

               case (71) ! FWHM-L (All)
                  do i=0, NMax_FWHM_Param
                     write(car_n,fmt='(i3)') i
                     car_n=adjustl(car_n)
                     call Fix_GPList_Par(G,'FWL'//trim(car_n)//'_PATT'//trim(car))
                  end do

               case default
                  str=trim(KEY_PATT(Npar))//'_PATT'//trim(car)
                  call Fix_GPList_Par(G,trim(str))

            end select ! Npar

         !> VARY
         case ("VARY")
            select case (Npar)
               case (0)
                  call set_error(1," Error in the Refinement Code for Patterns!")
                  return

               case (4) ! UVW
                  call Vary_GPList_Par(G,'U_PATT'//trim(car))
                  call Vary_GPList_Par(G,'V_PATT'//trim(car))
                  call Vary_GPList_Par(G,'W_PATT'//trim(car))

               case (17) ! BKGD (All))
                  do i=1,NMax_Bckgd_Params
                     write(car_n,fmt='(i3)') i
                     car_n=adjustl(car_n)
                     call Vary_GPList_Par(G,'BKG'//trim(car_n)//'_PATT'//trim(car))
                  end do

               case (21)  ! SC (All))

               case (28) ! SDSL
                  call Vary_GPList_Par(G,'SD_PATT'//trim(car))
                  call Vary_GPList_Par(G,'SL_PATT'//trim(car))

               case (34) ! SIG
                  do i=0,2
                     write(car_n,fmt='(i3)') i
                     car_n=adjustl(car_n)
                     call Vary_GPList_Par(G,'SIG'//trim(car_n)//'_PATT'//trim(car))
                  end do

               case (38) ! GAM
                  do i=0,2
                     write(car_n,fmt='(i3)') i
                     car_n=adjustl(car_n)
                     call Vary_GPList_Par(G,'GAM'//trim(car_n)//'_PATT'//trim(car))
                  end do

               case (49)
                  do i=0,NMax_FWHM_Param
                     write(car_n,fmt='(i3)') i
                     car_n=adjustl(car_n)
                     call Vary_GPList_Par(G,'FW'//trim(car_n)//'_PATT'//trim(car))
                  end do

               case (60)
                  do i=0,NMax_FWHM_Param
                     write(car_n,fmt='(i3)') i
                     car_n=adjustl(car_n)
                     call Vary_GPList_Par(G,'FWG'//trim(car_n)//'_PATT'//trim(car))
                  end do

               case (71)
                  do i=0,NMax_FWHM_Param
                     write(car_n,fmt='(i3)') i
                     car_n=adjustl(car_n)
                     call Vary_GPList_Par(G,'FWL'//trim(car_n)//'_PATT'//trim(car))
                  end do

               case default
                  str=trim(Key_PATT(NPar))//'_PATT'//trim(car)
                  call Vary_GPList_Par(G,trim(str))

            end select ! Npar

      end select ! Directives

   End Subroutine Set_RefCodes_Patt

   !!----
   !!---- Subroutine ReadCode_EQUAL_PATT
   !!----
   !!---- June 2023
   !!
   Module Subroutine ReadCode_EQUAL_PATT(String, IPatt, G)
      !---- Arguments ----!
      character(len=*),      intent(in)     :: String
      integer,               intent(in)     :: IPatt
      type(GenParList_Type), intent(in out) :: G

      !---- Local Variables ----!
      character(len=3) :: car
      character(len=40):: str

      integer :: i, j, i1, i2, iv, nlong, kc

      !> Init
      call clear_error()

      !> copy
      line=trim(adjustl(string))

      car=u_case(line(1:3))
      if (car /= 'EQU') then
         call set_error(1,'Wrong Directive for EQUAL instruction: '//trim(line))
         return
      end if

      !> Cut EQUAL word
      call cut_string(line,nlong)

      !> Get Words
      call get_words(line, dire, iv)

      if (iv <= 1) then
         call set_error(1,'Wrong Directive for EQUAL instruction: '//trim(line))
         return
      end if

      !> Current Pattern
      if (IPatt ==0) then
         car='1'
      else
         write(unit=car, fmt='(i3)') IPatt
         car=adjustl(car)
      end if

      !> Parent
      str='_PATT'//trim(car)
      i=index(dire(1),trim(str))
      if (i > 0) str=' '

      i1=0
      do i=1,G%NPar
         if (trim(u_case(G%Par(i)%Nam)) /= trim(u_case(dire(1)))//trim(str)) cycle
         i1=i
         exit
      end do
      if (i1 ==0) then
         call set_error(1,'Not found the Parent reference in the Refinement vector!')
         return
      end if

      !> Offspring
      do i=2,iv

         str='_PATT'
         j=index(dire(i),trim(str))
         if (j > 0) then
            str=' '
         else
            str=trim(str)//trim(car)
         end if

         i2=0
         do j=1,G%NPar
            if (trim(u_case(G%Par(j)%Nam)) /= trim(u_case(dire(i)))//trim(str)) cycle
            i2=j
            exit
         end do
         if (i2 ==0) then
            call set_error(1,'Not found the Parent reference in the Refinement vector!')
            return
         end if

         kc=G%Par(i2)%L

         !> Assign the values
         G%Par(i2)%L    =G%Par(i1)%L
         G%Par(i2)%M    =G%Par(i1)%M

         !> Checks orthers parameters with the same Code
         do j=1,G%NPar
            if (j==i1 .or. j == i2) cycle

            if (G%Par(j)%L == kc) then
               G%Par(j)%L    =G%Par(i1)%L
               G%Par(j)%M    =G%Par(i1)%M
            end if
         end do
      end do

   End Subroutine ReadCode_EQUAL_PATT

End Submodule KeyCod_Patt