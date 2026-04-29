SubModule (CFML_DiffPatt) DiffP_ReadPatt_PSI

 implicit none

 Contains
    !!--++
    !!--++ READ_PATTERN_DMC
    !!--++
    !!--++    Read a pattern for DMC
    !!--++
    !!--++ 01/05/2019
    !!
    Module Subroutine Read_Pattern_DMC(Filename,Pat)
       !---- Arguments ----!
       character (len=*),   intent(in)  :: Filename      ! Path+Filename
       class(DiffPat_type), intent(out) :: pat

       !---- Local Variables ----!
       character(len=180)                           :: txt1
       integer                                      :: ier, i, i_dat
       logical                                      :: info
       real(kind=cp)                                :: step

       !> Init
       call clear_error()

       !> File exists?
       inquire(file=trim(filename),exist=info)
       if ( .not. info) then
          Err_CFML%IErr=1
          Err_CFML%Msg="Read_Pattern_DMC@DIFFPATT: The file "//trim(filename)//" doesn't exist"
          return
       end if

       !> Open File
       open(newunit=i_dat,file=trim(filename),status="old",action="read",position="rewind",iostat=ier)
       if (ier /= 0 ) then
          Err_CFML%IErr=1
          Err_CFML%Msg="Read_Pattern_DMC@DIFFPATT:  Problems opening the file: "//trim(filename)
          return
       end if

       read(unit=i_dat,fmt="(a)",iostat=ier) txt1
       if (ier /= 0) then
          Err_CFML%IErr=1
          Err_CFML%Msg="Read_Pattern_DMC@DIFFPATT: Check your intensity file!"
          close(unit=i_dat)
          return
       end if
       pat%title=txt1

       read(unit=i_dat,fmt="(A)",iostat=ier)txt1
       if (ier /= 0) then
          Err_CFML%IErr=1
          Err_CFML%Msg="Read_Pattern_DMC@DIFFPATT: Check your intensity file!"
          close(unit=i_dat)
          return
       end if

       read(unit=i_dat,fmt=*,iostat=ier) pat%xmin,step,pat%xmax
       if (ier /= 0) then
          Err_CFML%IErr=1
          Err_CFML%Msg="Read_Pattern_DMC@DIFFPATT: 2theta_min, step, 2theta_max!"
          close(unit=i_dat)
          return
       end if
       pat%npts = (pat%xmax - pat%xmin)/step + 1.005
       if (pat%npts < 20)then
          Err_CFML%IErr=1
          Err_CFML%Msg="Read_Pattern_DMC@DIFFPATT: Number of points too low!"
          close(unit=i_dat)
          return
       end if

       !> Allocating
       call Allocate_Pattern(pat)

       read(unit=i_dat,fmt="(10f8.0)",iostat=ier)(pat%y(i),i=1,pat%npts)
       if (ier > 0) then
          Err_CFML%IErr=1
          Err_CFML%Msg="Read_Pattern_DMC@DIFFPATT: Check your intensity file!"
          close(unit=i_dat)
          return
       end if

       read(unit=i_dat,fmt="(10f8.0)",iostat=ier)(pat%sigma(i),i=1,10)
       if (ier /= 0) then
          !> Sigmas are not provided, assume sigma=sqrt(Y)
          pat%sigma(1:pat%npts) =sqrt(pat%y(1:pat%npts))

       else
          backspace (unit=i_dat)
          read(unit=i_dat,fmt="(10f8.0)",iostat=ier)(pat%sigma(i),i=1,pat%npts)
          if (ier /= 0) then
             Err_CFML%IErr=1
             Err_CFML%Msg="Read_Pattern_DMC@DIFFPATT: Problems reading sigmas!"
             close(unit=i_dat)
             return
          end if
       end if

       do i=1,pat%npts
          pat%sigma(i) = pat%sigma(i)*pat%sigma(i)
          pat%x(i)= pat%xmin+(i-1)*step
       end do

       pat%ymin=minval(pat%y(1:pat%npts))
       pat%ymax=maxval(pat%y(1:pat%npts))

       close(unit=i_dat)
    End Subroutine Read_Pattern_DMC

End SubModule DiffP_ReadPatt_PSI