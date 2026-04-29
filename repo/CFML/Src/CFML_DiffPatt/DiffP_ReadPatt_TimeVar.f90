!!----
SubModule (CFML_DiffPatt) DiffP_ReadPatt_TimeVar

 implicit none

   Contains

   !!--++
   !!--++ READ_PATTERN_TIME_VARIABLE
   !!--++
   !!--++    Read a pattern for Time Variable
   !!--++
   !!--++ 30/04/2019
   !!
   Module Subroutine Read_Pattern_TimeVar(Filename, Pat)
      !---- Arguments ----!
      character(len=*),    intent(in)  :: Filename      ! Path+Filename
      class(DiffPat_Type), intent(out) :: Pat

      !---- Local Variables ----!
      character(len=180)                           :: txt1
      character(len=132)                           :: txt2
      character(len=132)                           :: txt3
      real(kind=cp), dimension(:), allocatable     :: bk
      real(kind=cp)                                :: cnorma,step
      integer                                      :: i,ier,i_dat
      logical                                      :: info

       !> Init
      call clear_error()

      !> File exists?
      inquire(file=trim(filename),exist=info)
      if ( .not. info) then
         Err_CFML%IErr=1
         Err_CFML%Msg="Read_Pattern_TimeVar@DIFFPATT: The file "//trim(filename)//" doesn't exist"
         return
      end if

      !> Open File
      open(newunit=i_dat,file=trim(filename),status="old",action="read",position="rewind",iostat=ier)
      if (ier /= 0 ) then
         Err_CFML%IErr=1
         Err_CFML%Msg="Read_Pattern_TimeVar@DIFFPATT: Problems opening the file: "//trim(filename)
         return
      end if

      !> Title
      read(unit=i_dat,fmt="(a)",iostat=ier) txt1   !1
      pat%title=trim(txt1)
      if (ier /= 0) then
         Err_CFML%IErr=1
         Err_CFML%Msg="Read_Pattern_TimeVar@DIFFPATT: Problem in Title!"
         close(unit=i_dat)
         return
      end if

      read(unit=i_dat,fmt="(a)",iostat=ier) txt2   !2
      if (ier /= 0) then
         Err_CFML%IErr=1
         Err_CFML%Msg="Read_Pattern_TimeVar@DIFFPATT: Problem txt2!"
         close(unit=i_dat)
         return
      end if

      read(unit=i_dat,fmt="(a)",iostat=ier) txt3   !3
      if (ier /= 0) then
         Err_CFML%IErr=1
         Err_CFML%Msg="Read_Pattern_TimeVar@DIFFPATT: Problem txt3!"
         close(unit=i_dat)
         return
      end if

      read(unit=i_dat,fmt="(a)",iostat=ier) txt3   !4
      if (ier /= 0)then
         Err_CFML%IErr=1
         Err_CFML%Msg="Read_Pattern_TimeVar@DIFFPATT: Problem txt4!"
         close(unit=i_dat)
         return
      end if

      read(unit=i_dat,fmt=*,iostat=ier)pat%xmin, step, pat%xmax
      if (ier /= 0)then
         Err_CFML%IErr=1
         Err_CFML%Msg="Read_Pattern_TimeVar@DIFFPATT: Error in  xmin, step, xmax!"
         close(unit=i_dat)
         return
      end if

      pat%npts = (pat%xmax-pat%xmin)/step+1.5_cp
      if (pat%npts <=0) then
         Err_CFML%IErr=1
         Err_CFML%Msg="Read_Pattern_TimeVar@DIFFPATT: Number of points was zero!"
         close(unit=i_dat)
         return
      end if

      !> Allocating
      call Allocate_Pattern(pat)

      if(allocated(bk) ) deallocate(bk)
      allocate(bk(pat%npts))

      read(unit=i_dat,fmt="(5(F6.0,F10.0))",iostat=ier)(bk(i),pat%y(i),i=1,pat%npts)
      if (ier /= 0) then
         Err_CFML%IErr=1
         Err_CFML%Msg="Read_Pattern_TimeVar@DIFFPATT: Problems reading the background points!"
         close(unit=i_dat)
         return
      end if

      !> Normalize data to constant time
      cnorma=0.0
      DO i=1,pat%npts
         IF (bk(i) < epsilon(1.0_cp)) then
            Err_CFML%IErr=1
            Err_CFML%Msg="Read_Pattern_TimeVar@DIFFPATT: Problems reading the background points!"
            close(unit=i_dat)
            return
         end if
         cnorma=cnorma+bk(i)
         pat%x(i)= pat%xmin+(i-1)*step
      end do
      cnorma=cnorma/real(pat%npts)
      do i=1,pat%npts
         pat%sigma(i)=sqrt(pat%y(i))*cnorma/bk(i)
         pat%y(i)=pat%y(i)*cnorma/bk(i)
         bk(i)=0.0_cp
      end do
      pat%ymin=minval(pat%y(1:pat%npts))
      pat%ymax=maxval(pat%y(1:pat%npts))

      close(unit=i_dat)
   End subroutine Read_Pattern_TimeVar

End SubModule DiffP_ReadPatt_TimeVar