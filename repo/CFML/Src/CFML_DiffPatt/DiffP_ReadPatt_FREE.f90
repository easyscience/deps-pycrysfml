!!----
SubModule (CFML_DiffPatt) DiffP_ReadPatt_FREE

 implicit none

   Contains

   !!--++
   !!--++ READ_PATTERN_FREE
   !!--++
   !!--++    Read a pattern for Free
   !!--++
   !!--++ 30/04/2019
   !!
   Module Subroutine Read_Pattern_Free(Filename,Pat,ext)
      !---- Arguments ----!
      character(len=*),           intent(in)  :: Filename      ! Path+Filename
      class(DiffPat_Type),        intent(out) :: Pat
      character(len=*), optional, intent(in)  :: ext

      !---- Local Variables ----!
      integer                                      :: i,no,ier,inum,nc,iv,i_dat
      integer, dimension(3)                        :: ivet
      character(len=180)                           :: aline
      character(len=30), dimension(10)             :: dire
      real(kind=cp), dimension(3)                  :: vet
      real(kind=cp)                                :: step
      logical                                      :: title_given,ext_given, rigaku,info


       !> Init
      call clear_error()

      !> File exists?
      inquire(file=trim(filename),exist=info)
      if ( .not. info) then
         Err_CFML%IErr=1
         Err_CFML%Msg="Read_Pattern_FREE@DIFFPATT: The file "//trim(filename)//" doesn't exist"
         return
      end if

      !> Open File
      open(newunit=i_dat,file=trim(filename),status="old",action="read",position="rewind",iostat=ier)
      if (ier /= 0 ) then
         Err_CFML%IErr=1
         Err_CFML%Msg="Read_Pattern_FREE@DIFFPATT: Problems with opening the file: "//trim(filename)
         return
      end if

      title_given=.false.
      ext_given=.false.
      rigaku=.false.

      no=0
      pat%ScatVar="2theta"

      if (present(ext)) ext_given=.true.
      if (ext_given) then
         if (trim(ext) == ".MDI") rigaku=.true.
      end if

      do
         read(unit=i_dat,fmt="(a)",iostat=ier) aline
         if (ier /= 0) then
            Err_CFML%IErr=1
            Err_CFML%Msg="Read_Pattern_FREE@DIFFPATT: End of file was detected!"
            close(unit=i_dat)
            return
         end if

         aline=adjustl(aline)

         !> Comment lines using ! or #
         if (aline(1:1) == "!" .or. aline(1:1) == "#") then
            i=index(aline,"Legend_X")
            if (i /= 0) then
               !pat%xax_text=adjustl(aline(i+9:))
            end if

            i=index(aline,"Legend_Y")
            if (i /= 0) then
               !pat%yax_text=adjustl(aline(i+9:))
            end if

            i=index(aline,"Scattering variable:")
            if (i /= 0) then
               pat%ScatVar=adjustl(aline(i+20:))
            end if

            i=index(aline,"TSAMP")
            if (i /= 0) then
               !read(unit=aline(i+5:),fmt=*,iostat=ier) pat%tsamp
               !if (ier /= 0) pat%tsamp = 0.0
            end if

            i=index(aline,"TITLE")
            if (i /= 0) then
               Pat%title=trim(aline(i+6:))
               title_given=.true.
            end if

            i=index(aline,"Title:")
            if (i /= 0) then
               Pat%title=trim(aline(i+7:))
               title_given=.true.
            end if

            cycle
         end if

         !> BANK Information
         if (aline(1:4) == "BANK") then
            read(unit=aline(5:41),fmt=*) inum, pat%npts
            read(unit=aline(47:90),fmt=*) pat%xmin,step
            pat%xmax=pat%xmin+(pat%npts-1)*step
            exit
         end if

         if (rigaku) then
            if (.not. title_given) then
               title_given=.true.
               Pat%title=trim(adjustl(aline))
               cycle

            else
               call get_words(aline,dire,nc)
               call get_num(trim(dire(1))//' '//trim(dire(2))//' '//trim(dire(6)),vet,ivet,iv)
               pat%xmin=vet(1)
               step=vet(2)
               pat%xmax=vet(3)
               pat%npts = nint((pat%xmax-pat%xmin)/step+1.0)
               exit
            end if
         end if

         !> Reading Xmin, Step, Xmax, Title (optional)
         call get_words(aline,dire,nc)
         if (nc > 2) then
            call get_num(trim(dire(1))//' '//trim(dire(2))//' '//trim(dire(3)),vet,ivet,iv)
            if (iv == 3) then
               pat%xmin=vet(1)
               step=vet(2)
               pat%xmax=vet(3)

               if (step <= epsilon(1.0_cp)) then
                  Err_CFML%IErr=1
                  Err_CFML%Msg="Read_Pattern_FREE@DIFFPATT: Step is close to zero!"
                  close(unit=-i_dat)
                  return
               end if

               pat%npts = nint((pat%xmax-pat%xmin)/step+1.0)

               !> Title?
               i=index(aline,trim(dire(3)))
               nc=len_trim(dire(3))

               if (len_trim(aline(i+nc+1:)) > 0 .and. .not. title_given) then
                  Pat%title=trim(aline(i+nc+1:))
                  title_given=.true.
               end if

               exit  ! Salida del Bucle
            end if

            !> TSAMP
            !i=index(aline,"TSAMP")
            !if (i /= 0) then
            !   read(unit=aline(i+5:),fmt=*,iostat=ier) pat%tsamp
            !   if (ier /= 0) pat%tsamp = 0.0
            !end if
         end if

         !> Probably Coment line or Title
         if (.not. title_given) then
            Pat%title=trim(aline)
            title_given=.true.
         end if

         no=no+1
         if (no > 7)then
            Err_CFML%IErr=1
            Err_CFML%Msg="Read_Pattern_FREE@DIFFPATT: Number of Comment lines was exceeded ( > 7) !"
            close(unit=i_dat)
            return
         else
            cycle
         end if
      end do

      !> Aditional checks
      if (pat%npts <= 10 .or. pat%xmax <  pat%xmin  .or. step > pat%xmax) then
         Err_CFML%IErr=1
         Err_CFML%Msg="Read_Pattern_FREE@DIFFPATT: Problems reading 2Theta_ini, Step, 2Theta_end !"
         close(unit=i_dat)
         return
      end if

      ! Allocating memory
      call Allocate_Pattern(pat)

      ! Reading intensities values
      read(unit=i_dat,fmt=*,iostat=ier)(pat%y(i),i=1,pat%npts)
      if (ier /= 0) then
         Err_CFML%IErr=1
         Err_CFML%Msg="Read_Pattern_FREE@DIFFPATT: Number of intensities values is wrong!!"
         close(unit=i_dat)
         return
      end if

      do i=1,pat%npts
         pat%sigma(i) = sqrt(pat%y(i))
         pat%x(i)= pat%xmin+(i-1)*step
      end do
      pat%sigvar=.true.
      pat%ymin=minval(pat%y(1:pat%npts))
      pat%ymax=maxval(pat%y(1:pat%npts))

      if (pat%scatvar == "2theta" .and. pat%xmax > 180.0 ) pat%Scatvar="TOF"

      close(unit=i_dat)
   End Subroutine Read_Pattern_Free

End SubModule DiffP_ReadPatt_FREE