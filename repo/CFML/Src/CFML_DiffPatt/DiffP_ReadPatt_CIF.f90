!!----
!!----
!!----
!!---- 30/04/19
!!
SubModule (CFML_DiffPatt) DiffP_ReadPatt_CIF

 implicit none

   Contains

   !!--++
   !!--++ READ_PATTERN_CIF
   !!--++
   !!--++    (PRIVATE)
   !!--++    Read a pattern from a CIF file
   !!--++
   !!--++ 30/04/2019
   !!
   Module Subroutine Read_Pattern_CIF(Filename,Pat)
      !---- Arguments ----!
      character(len=*),    intent(in)  :: Filename      ! Path+Filename
      class(DiffPat_Type), intent(out) :: Pat

      !---- Local Variables ----!
      character(len=132), dimension(:),allocatable  :: file_lines
      character(len=1)                             :: aux
      integer                                      :: i, n, nlines, ic, ier,i_dat
      real(kind=cp), dimension(6)                  :: values, std
      real(kind=cp)                                :: chi2
      integer,       dimension(6)                  :: pos
      integer                                      :: line_block_id, line_probe, line_loop, &
                                                      line_point_id, line_npoints, line_start_data
      logical                                      :: info

      !> Init
      call clear_error()

      !> File exists?
      inquire(file=trim(filename),exist=info)
      if ( .not. info) then
         Err_CFML%IErr=1
         Err_CFML%Msg="Read_Pattern_CIF@DIFFPATT: The file "//trim(filename)//" doesn't exist"
         return
      end if

      !> Open File
      open(newunit=i_dat,file=trim(filename),status="old",action="read",position="rewind",iostat=ier)
      if (ier /= 0 ) then
         Err_CFML%IErr=1
         Err_CFML%Msg="Read_Pattern_CIF@DIFFPATT: Problems opening the file: "//trim(filename)
         return
      end if

      !> Init
      nlines=0
      do
        read(unit=i_dat,fmt="(a)",iostat=ier) aux
        if (ier /= 0) exit
        nlines=nlines+1
      end do
      if (nlines < 10) then
         Err_CFML%IErr=1
         Err_CFML%Msg="Read_Pattern_CIF@DIFFPATT: Number of lines too small to hold a diffraction pattern!"
         close(unit=i_dat)
         return
      end if

      allocate(file_lines(nlines))
      line_block_id=0; line_probe=0; line_loop=0; line_point_id=0;
      line_npoints=0; line_start_data=0

      rewind(unit=i_dat)
      do i=1,nlines
         read(unit=i_dat,fmt="(a)",iostat=ier) file_lines(i)

         if (index(file_lines(i),"_pd_block_id") /= 0) line_block_id=i
         if (index(file_lines(i),"_diffrn_radiation_probe") /= 0) line_probe=i
         if (index(file_lines(i),"loop_") /= 0 .and. line_loop == 0) line_loop=i
         if (index(file_lines(i),"_pd_proc_number_of_points") /= 0 ) line_npoints=i
      end do
      close(unit=i_dat)

      if (line_npoints == 0) then
         Err_CFML%IErr=1
         Err_CFML%Msg="Read_Pattern_CIF@DIFFPATT: No line with the number of points in the pattern!"
         return
      end if

      read(unit=file_lines(line_npoints)(27:),fmt=*,iostat=ier) pat%npts
      if (ier /= 0) then
         Err_CFML%IErr=1
         Err_CFML%Msg="Read_Pattern_CIF@DIFFPATT: Error reading the number of points in the pattern!"
         return
      end if

      !> Allocating
      call Allocate_Pattern(pat)

      if (line_block_id > 0) pat%title=file_lines(line_block_id)(13:)
      if (line_probe > 0)    pat%kindRad = adjustl(file_lines(line_probe)(24:))

      n=0
      pos=0
      do i=line_loop+1, line_loop+10
         n=n+1
         if (index(file_lines(i),"_pd_proc_point_id") /= 0) then
            pos(1)=n
            cycle
         end if
         if (index(file_lines(i),"_pd_proc_2theta_corrected") /= 0) then
            pat%scatvar =  "2theta"
            !pat%xax_text =  "2theta(degrees)"
            pos(2)=n
            cycle
         end if
         if (index(file_lines(i),"_pd_proc_d_spacing") /= 0) then
            pat%scatvar =  "d-spacing"
            !pat%xax_text =  "d-spacing(Angstroms)"
            pat%kindRad=  "Neutrons (TOF)"
            pos(2)=n
            cycle
         end if
         if (index(file_lines(i),"_pd_proc_intensity_total") /= 0) then
            pos(3)=n
            cycle
         end if
         if (index(file_lines(i),"_pd_calc_intensity_total") /= 0) then
            pos(4)=n
            cycle
         end if
         if (index(file_lines(i),"_pd_proc_intensity_bkg_calc") /= 0) then
            pos(5)=n
            cycle
         end if
         if (len_trim(file_lines(i)) == 0) then
            line_start_data=i+1
            exit
         end if
      end do

      if (pos(2) == 0 .or. pos(3) == 0) then  !Error experimental data are lacking
         Err_CFML%IErr=1
         Err_CFML%Msg="Read_Pattern_CIF@DIFFPATT: Experimental data are lacking!"
         return
      end if

      n=0
      do i=line_start_data, nlines
         n=n+1
         if (n > pat%npts) exit
         call Get_NumStd(file_lines(i), values, std, ic)
         if (ic < 2) exit

         pat%x(n) =  values(pos(2))
         pat%y(n) =  values(pos(3))
         pat%sigma(n) = std(pos(3))
      end do

      pat%xmin=pat%x(1)
      pat%xmax=pat%x(pat%npts)
      pat%ymin=minval(pat%y(1:pat%npts))
      pat%ymax=maxval(pat%y(1:pat%npts))
      pat%Sigvar=.true.

      select type (Pat)
         class is (DiffPat_E_Type)
            n=0
            chi2=0.0_cp
            do i=line_start_data, nlines
               n=n+1
               if (n > pat%npts) exit
               call Get_NumStd(file_lines(i), values, std, ic)
               if (ic < 2) exit

               if (pos(4) /= 0) pat%ycalc(n) = values(pos(4))
               if (pos(5) /= 0) pat%bgr(n) = values(pos(5))

               if (pat%sigma(n) > 0.001) then
                  chi2=chi2+((pat%y(n)-pat%ycalc(n))/pat%sigma(n))**2
               end if
            end do
            chi2=chi2/real(pat%npts)

            i=len_trim(pat%title)
            write(unit=pat%title(i+2:),fmt="(a,g12.4)") "  Chi2(free) = ",chi2
      end select
   End Subroutine Read_Pattern_CIF

End SubModule DiffP_ReadPatt_CIF