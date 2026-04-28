SubModule (CFML_DiffPatt) DiffP_ReadPatt_LLB

 implicit none

 Contains
    !!--++
    !!--++ READ_PATTERN_G41
    !!--++
    !!--++    Read a pattern for G41
    !!--++
    !!--++ 01/05/2019
    !!
    Module Subroutine Read_Pattern_G41(Filename,Pat)
       !---- Arguments ----!
       character(len=*),        intent(in)  :: Filename      ! Path+Filename
       class(DiffPat_E_Type),   intent(out) :: Pat

       !---- Local Variables ----!
       character(len=180)                           :: txt1, txt2, txt3
       integer                                      :: i, ier, ivari,i_dat
       real(kind=cp)                                :: cnorm,step
       real(kind=cp)                                :: rmon1, rmon2
       logical                                      :: info

       !> Init
       call clear_error()

       !> File exists?
       inquire(file=trim(filename),exist=info)
       if ( .not. info) then
          Err_CFML%IErr=1
          Err_CFML%Msg="Read_Pattern_G41@DIFFPATT: The file "//trim(filename)//" doesn't exist"
          return
       end if

       !> Open File
       open(newunit=i_dat,file=trim(filename),status="old",action="read",position="rewind",iostat=ier)
       if (ier /= 0 ) then
          Err_CFML%IErr=1
          Err_CFML%Msg="Read_Pattern_G41@DIFFPATT: Problems opening the file: "//trim(filename)
          return
       end if

       read(unit=i_dat,fmt="(A)",iostat=ier) txt1                  !1
       pat%title=txt1
       if (ier /= 0 ) then
          Err_CFML%IErr=1
          Err_CFML%Msg="Read_Pattern_G41@DIFFPATT: check your Intensity file!"
          close(unit=i_dat)
          return
       end if

       read(unit=i_dat,fmt="(A)",iostat=ier) txt2                  !2
       if (ier /= 0 ) then
          Err_CFML%IErr=1
          Err_CFML%Msg="Read_Pattern_G41@DIFFPATT: check your Intensity file!"
          close(unit=i_dat)
          return
       end if

       read(unit=i_dat,fmt="(A)",iostat=ier) txt3                  !3
       if (ier /= 0 ) then
          Err_CFML%IErr=1
          Err_CFML%Msg="Read_Pattern_G41@DIFFPATT: check your Intensity file!"
          close(unit=i_dat)
          return
       end if

       do
         read(unit=i_dat,fmt="(A)",iostat=ier) txt3
         txt3=adjustl(txt3)
         if(txt3(1:1) /= "!") exit
       end do
       read(unit=txt3, fmt=*, iostat=ier)  pat%npts,pat%tsample,pat%tset,ivari,rmon1,rmon2
       if (ier /= 0 )then
          Err_CFML%IErr=1
          Err_CFML%Msg="Read_Pattern_G41@DIFFPATT: check your Intensity file!"
          close(unit=i_dat)
          return
       end if

       pat%monitor=rmon1
       if (pat%npts <= 0) then
          Err_CFML%IErr=1
          Err_CFML%Msg="Read_Pattern_G41@DIFFPATT: Number of points negative or zero!"
          close(unit=i_dat)
          return
       end if

       !> Allocating
       call Allocate_Pattern(pat)


       read(unit=i_dat,fmt=*,iostat=ier)pat%xmin,step,pat%xmax              !5
       if (ier /= 0 ) then
          Err_CFML%IErr=1
          Err_CFML%Msg="Read_Pattern_G41@DIFFPATT: Xmin, step, XMax!"
          close(unit=i_dat)
          return
       end if

       read(unit=i_dat,fmt=*,iostat=ier)(pat%y(i),i=1, pat%npts)
       if (ier /= 0 )then
          Err_CFML%IErr=1
          Err_CFML%Msg="Read_Pattern_G41@DIFFPATT: Problems reading the intensities!"
          close(unit=i_dat)
          return
       end if

       if (ivari /= 0) then          !IVARI
          read(unit=i_dat,fmt=*,iostat=ier)(pat%sigma(i),i=1, pat%npts)
          if (ier /= 0 ) then
             Err_CFML%IErr=1
             Err_CFML%Msg="Read_Pattern_G41@DIFFPATT: Problems reading the Sigmas!"
             close(unit=i_dat)
             return
          end if

          cnorm=0.0_cp
          do i=1,pat%npts
             pat%sigma(i)=pat%sigma(i)*pat%sigma(i)
             if (pat%sigma(i) < 0.000001) pat%sigma(i)=1.0_cp
             pat%x(i)= pat%xmin+(i-1)*step
             cnorm=cnorm+pat%sigma(i)/pat%y(i)
          end do
          cnorm=cnorm/real(pat%npts)
       else                         !ivari
          if (rmon1 > 1.0_cp .and. rmon2 > 1.0_cp) then
             cnorm=rmon1/rmon2
          else
             cnorm=1.0_cp
          end if
          do i=1,pat%npts
             pat%sigma(i)=pat%y(i)*cnorm
             pat%x(i)= pat%xmin+(i-1)*step
          end do
       end if                        !IVARI

       pat%ymin=minval(pat%y(1:pat%npts))
       pat%ymax=maxval(pat%y(1:pat%npts))

       !> Close unit
       close(unit=i_dat)
    End Subroutine Read_Pattern_G41

End SubModule DiffP_ReadPatt_LLB