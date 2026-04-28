SubModule (CFML_DiffPatt) DiffP_ReadPatt_PAN

 implicit none

 Contains

    !!--++
    !!--++ READ_PATTERN_PANALYTICAL_CSV
    !!--++
    !!--++    Read a pattern for Panalitical Format CSV
    !!--++
    !!--++ 01/05/2019
    !!
    Module Subroutine Read_Pattern_Panalytical_CSV(Filename,Pat)
       !---- Arguments ----!
       character (len=*),   intent(in)  :: Filename      ! Path+Filename
       class(DiffPat_type), intent(out) :: pat

       !---- Local Variables ----!
       logical                  :: info
       character (len=180)      :: line
       integer                  :: i, i_dat, j, long, ier
       real(kind=cp)            :: alpha1, alpha2, ratio_I


       !> Init
       call clear_error()

       !> File exists?
       inquire(file=trim(filename),exist=info)
       if ( .not. info) then
          Err_CFML%IErr=1
          Err_CFML%Msg="Read_Pattern_Panalytical_CSV@DIFFPATT: The file "//trim(filename)//" doesn't exist"
          return
       end if

       !> Open File
       open(newunit=i_dat,file=trim(filename),status="old",action="read",position="rewind",iostat=ier)
       if (ier /= 0 ) then
          Err_CFML%IErr=1
          Err_CFML%Msg="Read_Pattern_Panalytical_CSV@DIFFPATT: Problems opening the file: "//trim(filename)
          return
       end if

       !> Main Loop
       do
          read(unit=i_dat,fmt="(a)",IOSTAT=ier) line
          if (ier /= 0) then
             Err_CFML%IErr=1
             Err_CFML%Msg="Read_Pattern_Panalytical_CSV@DIFFPATT: Problems reading a profile CSV-DATA file!"
             close (unit=i_dat)
             return
          end if
          long=LEN_TRIM(line)

          if (line(1:7) == "Title 1") then
             pat%title=line(8:)

          else if(line(1:19) =="K-Alpha1 wavelength") then
             read(unit=line(21:long),fmt=*, IOSTAT=ier) alpha1
             pat%wave(1) = alpha1

          else if(line(1:19) =="K-Alpha2 wavelength") then
             read(unit=line(21:long),fmt=*, IOSTAT=ier) alpha2
             pat%wave(2) = alpha2

          else if(line(1:23) =="Ratio K-Alpha2/K-Alpha1") then
             read(unit=line(25:long),fmt=*, IOSTAT=ier) ratio_I
             pat%wave(3) = ratio_I

          else if(line(1:16) =="Data angle range") then
             read(unit=line(18:long),fmt=*)  pat%xmin  , pat%xmax

          !else if(line(1:14) =="Scan step size") then
          !   read(unit=line(16:long),fmt=*, IOSTAT=ier) pat%step

          else if(line(1:13) =="No. of points") then
             read(unit=line(15:long),fmt=*, IOSTAT=ier) pat%npts

          else if(line(1:13) =="[Scan points]") then
             read(unit=i_dat,fmt="(a)",IOSTAT=ier) line     ! lecture de la ligne Angle,Intensity
             if (ier/=0) return
             exit
          end if
       end do

       if (pat%npts <= 0) then
          Err_CFML%IErr=1
          Err_CFML%Msg=" Read_Pattern_Panalytical_CSV@DIFFPATT: Number of points negative or zero!"
          close(unit=i_dat)
          return
       end if

       !> Allocating variables for Pattern object
       call Allocate_Pattern(pat)

       i=0
       do
          i=i+1
          if (i > pat%npts) then
             i=i-1
             exit
          end if
          read(unit=i_dat,fmt="(a)",IOSTAT=ier) line
          j=index(line,",")
          if (j == 0) then
             read(unit=line,fmt=*,IOSTAT=ier) pat%x(i), pat%y(i)
          else
             read(unit=line(1:j-1),fmt=*,IOSTAT=ier) pat%x(i)
             read(unit=line(j+1:),fmt=*,IOSTAT=ier) pat%y(i)
          end if
          if (ier /=0) exit
          pat%sigma(i) = abs(pat%y(i))
       end do

       pat%npts = i
       pat%ymin=minval(pat%y(1:i))
       pat%ymax=maxval(pat%y(1:i))

       !> Close unit
       close(unit=i_dat)
    End Subroutine Read_Pattern_Panalytical_CSV

    !!--++
    !!--++ READ_PATTERN_PANALYTICAL_JCP
    !!--++
    !!--++    Read a pattern for Panalitical Format JCP
    !!--++
    !!--++ 01/05/2019
    !!
    Module Subroutine Read_Pattern_Panalytical_JCP(Filename, Pat)
       !---- Arguments ----!
       character(len=*),    intent(in)  :: Filename      ! Path+Filename
       class(DiffPat_type), intent(out) :: pat

       !---- Local Variables ----!
       character (len=132)                          :: line
       integer                                      :: i, j, long , k, ier, i_dat
       real(kind=cp)                                :: alpha1, alpha2, ratio_I, step
       logical :: info

       !> Init
       call clear_error()

       !> File exists?
       inquire(file=trim(filename),exist=info)
       if ( .not. info) then
          Err_CFML%IErr=1
          Err_CFML%Msg="Read_Pattern_Panalytical_JCP@DIFFPATT: The file "//trim(filename)//" doesn't exist"
          return
       end if

       !> Open File
       open(newunit=i_dat,file=trim(filename),status="old",action="read",position="rewind",iostat=ier)
       if (ier /= 0 ) then
          Err_CFML%IErr=1
          Err_CFML%Msg="Read_Pattern_Panalytical_JCP@DIFFPATT: Problems opening the file: "//trim(filename)
          return
       end if

       k=0
       do
          k=k+1
          read(unit=i_dat,fmt="(a)",IOSTAT=ier) line
          if (ier /= 0) then
             Err_CFML%IErr=1
             Err_CFML%Msg="Read_Pattern_Panalytical_JCP@DIFFPATT: Problems reading a profile JCP file!"
             close(unit=i_dat)
             return
          end if
          if ( k == 1) pat%title=trim(line)
          long=LEN_TRIM(line)

          if (line(1:7) == "## END=") exit

          if (line(1:21) =="##$WAVELENGTH ALPHA1=") then
             read(unit=line(22:long),fmt=*, IOSTAT=ier) alpha1
             if (ier /= 0) then
                Err_CFML%IErr=1
                Err_CFML%Msg="Read_Pattern_Panalytical_JCP@DIFFPATT: Problems reading Alpha1!"
                close(unit=i_dat)
                return
             end if
             pat%wave(1)= alpha1

          else if(line(1:21) =="##$WAVELENGTH ALPHA2=") then
             read(unit=line(22:long),fmt=*, IOSTAT=ier) alpha2
             if (ier /= 0) then
                Err_CFML%IErr=1
                Err_CFML%Msg="Read_Pattern_Panalytical_JCP@DIFFPATT: Problems reading Alpha2!"
                close(unit=i_dat)
                return
             end if
             pat%wave(2)= alpha2

          else if(line(1:33) =="##$INTENSITY RATIO ALPHA2/ALPHA1=") then
             read(unit=line(34:long),fmt=*, IOSTAT=ier) ratio_I
             if (ier /= 0) then
                Err_CFML%IErr=1
                Err_CFML%Msg="Read_Pattern_Panalytical_JCP@DIFFPATT: Problems reading Ratio!"
                close(unit=i_dat)
                return
             end if
             pat%wave(3)= ratio_I

          else if(line(1:10) =="## FIRSTX=") then
             read(unit=line(11:long),fmt=*, IOSTAT=ier) pat%xmin
             if (ier /= 0) then
                Err_CFML%IErr=1
                Err_CFML%Msg="Read_Pattern_Panalytical_JCP@DIFFPATT: Problems reading Xmin!"
                close(unit=i_dat)
                return
             end if

          else if(line(1:10) =="## LASTX=") then
             read(unit=line(11:long),fmt=*, IOSTAT=ier) pat%xmax
             if (ier /= 0) then
                Err_CFML%IErr=1
                Err_CFML%Msg="Read_Pattern_Panalytical_JCP@DIFFPATT: Problems reading Xmax!"
                close(unit=i_dat)
                return
             end if

          else if(line(1:10) =="## DELTAX=") then
             read(unit=line(11:long),fmt=*, IOSTAT=ier) step
             if (ier /= 0) then
                Err_CFML%IErr=1
                Err_CFML%Msg="Read_Pattern_Panalytical_JCP@DIFFPATT: Problems reading Step!"
                close(unit=i_dat)
                return
             end if

          else if(line(1:11) =="## NPOINTS=") then
             read(unit=line(12:long),fmt=*, IOSTAT=ier) pat%npts
             if (ier /= 0 .or. pat%npts <=0) then
                Err_CFML%IErr=1
                Err_CFML%Msg="Read_Pattern_Panalytical_JCP@DIFFPATT: Problems reading Numer of points!"
                close(unit=i_dat)
                return
             end if

          else if(line(1:20) =="## XYDATA= X++<Y..Y>") then

             call Allocate_Pattern(pat)

             i=1
             do
                read(unit=i_dat,fmt="(f9.3,tr1,5f11.3)",iostat=ier) pat%x(i),(pat%y(i+j),j=0,4)
                if (ier /= 0) then
                   Err_CFML%IErr=1
                   Err_CFML%Msg="Read_Pattern_Panalytical_JCP@DIFFPATT: Problems reading Intensities!"
                   close(unit=i_dat)
                   return
                end if

                do j=1,4
                   pat%x(i+j) = pat%x(i) + real(j)*step
                end do
                if (i+5 > pat%npts ) exit
                i=i+5
             end do
             pat%npts = i

          end if
       end do ! File

       pat%sigma(1:pat%npts)=abs(pat%y(1:pat%npts))
       pat%ymin=minval(pat%y(1:pat%npts))
       pat%ymax=maxval(pat%y(1:pat%npts))

       !> Close the unit
       close(unit=i_dat)
    End Subroutine Read_Pattern_Panalytical_JCP

    !!--++
    !!--++ READ_PATTERN_PANALYTICAL_UDF
    !!--++
    !!--++    Read a pattern for Panalitical Format UDF
    !!--++
    !!--++ 01/05/2019
    !!
    Module Subroutine Read_Pattern_Panalytical_UDF(Filename, Pat)
       !---- Arguments ----!
       character(len=*),    intent(in)  :: Filename      ! Path+Filename
       class(DiffPat_type), intent(out) :: pat

       !---- Local Variables ----!
       character (len=132)                            :: line, newline
       integer                                        :: i, j, long, ier, n, nb_lignes, np, i_dat
       real(kind=cp)                                  :: alpha1, alpha2, ratio, step
       logical                                        :: info,title_given

       !> Init
       call clear_error()

       !> File exists?
       inquire(file=trim(filename),exist=info)
       if ( .not. info) then
          Err_CFML%IErr=1
          Err_CFML%Msg="Read_Pattern_Panalytical_UDF@DIFFPATT: The file "//trim(filename)//" doesn't exist"
          return
       end if

       !> Open File
       open(newunit=i_dat,file=trim(filename),status="old",action="read",position="rewind",iostat=ier)
       if (ier /= 0 ) then
          Err_CFML%IErr=1
          Err_CFML%Msg="Read_Pattern_Panalytical_UDF@DIFFPATT: Problems opening the file: "//trim(filename)
          return
       end if

       !>
       title_given = .false.

       do
          read(unit=i_dat,fmt="(a)",IOSTAT=ier) line
          if (ier /= 0) then
             Err_CFML%IErr=1
             Err_CFML%Msg="Read_Pattern_Panalytical_UDF@DIFFPATT: Problems reading a profile UDF-DATA file!"
             close (unit=i_dat)
             return
          end if
          if(.not. title_given) then
            pat%title=trim(line)
            title_given=.true.
          end if
          long=LEN_TRIM(line)

          if (line(1:12) =="LabdaAlpha1,") then
             read(unit=line(23:long-2),fmt=*, IOSTAT=ier) alpha1
             pat%wave(1)=  alpha1

          else if(line(1:12) =="LabdaAlpha2,") then
             read(unit=line(23:long-2),fmt=*, IOSTAT=ier) alpha2
             pat%wave(2)=  alpha2

          else if(line(1:13) =="RatioAlpha21,") then
             read(unit=line(14:long-2),fmt=*, IOSTAT=ier) ratio
             pat%wave(3)= ratio

          else if(line(1:15) =="DataAngleRange,") then
             write(unit=newline,fmt="(a)")  line(16:long-2)
             i = INDEX(NewLine,",")
             long=LEN_TRIM(NewLine)
             read(unit=NewLine(1:i-1),fmt=*, IOSTAT=ier)   pat%xmin
             read(unit=NewLine(i+1:long),fmt=*,IOSTAT=ier) pat%xmax

          else if(line(1:13) =="ScanStepSize,") then
             read(unit=line(14:long-2),fmt=*, IOSTAT=ier) step
             pat%npts=(pat%xmax-pat%xmin)/step+1.2
             if (pat%npts <= 0) then
                Err_CFML%IErr=1
                Err_CFML%Msg="Read_Pattern_Panalytical_UDF@DIFFPATT: Problems reading a profile UDF-DATA file!"
                close (unit=i_dat)
                return
             end if

          else if(line(1:7) =="RawScan") then

             call Allocate_Pattern(pat)

             nb_lignes = int(pat%npts/8)
             n = 0
             do j=1, nb_lignes
                read(unit=i_dat,fmt= "(7(f8.0,tr1),F8.0)", IOSTAT=ier) (pat%y(i+n),i=1,7), pat%y(n+8)
                if (ier /= 0) then
                   Err_CFML%IErr=1
                   Err_CFML%Msg="Read_Pattern_Panalytical_UDF@DIFFPATT: Problems reading a profile UDF-DATA file!"
                   close(unit=i_dat)
                   return
                end if
                n = n + 8
             end do
             np = pat%npts - n

             if (np /= 0) then
                read(unit=i_dat, fmt = "(7(f8.0,tr1),F8.0)") (pat%y(i), i=n+1, pat%npts)
             end if
             exit

          end if
       end do ! file

       do i=1,pat%npts
          pat%x(i)=pat%xmin+real(i-1)*step
          pat%sigma(i) = abs(pat%y(i))
       end do
       pat%ymin=minval(pat%y(1:pat%npts))
       pat%ymax=maxval(pat%y(1:pat%npts))

       !> close unit
       close(unit=i_dat)
    End Subroutine Read_Pattern_Panalytical_UDF

    !!--++
    !!--++ READ_PATTERN_PANALYTICAL_XRDML
    !!--++
    !!--++    Read a pattern for Panalitical Format XRDML
    !!--++
    !!--++ 01/05/2019
    !!
    Module Subroutine Read_Pattern_Panalytical_XRDML(Filename,Pat)
       !---- Arguments ----!
       character(len=*),    intent(in)  :: Filename      ! Path+Filename
       class(DiffPat_type), intent(out) :: pat

       !---- Local Variables ----!
       integer                                            :: i, i1, i2, nl, ier, np,nr,i_dat
       real                                               :: step
       integer,                allocatable, dimension(:)  :: counts
       character (len=256000), allocatable, dimension(:)  :: XRDML_line, XRDML_intensities_line
       logical                                            :: info

       !> Init
       call clear_error()

       !> File exists?
       inquire(file=trim(filename),exist=info)
       if ( .not. info) then
          Err_CFML%IErr=1
          Err_CFML%Msg="Read_Pattern_Panalytical_XRDML@DIFFPATT: The file "//trim(filename)//" doesn't exist"
          return
       end if

       !> Open File
       open(newunit=i_dat,file=trim(filename),status="old",action="read",position="rewind",iostat=ier)
       if (ier /= 0 ) then
          Err_CFML%IErr=1
          Err_CFML%Msg="Read_Pattern_Panalytical_XRDML@DIFFPATT: Problems opening the file: "//trim(filename)
          return
       end if

       if (allocated(XRDML_line))             deallocate(XRDML_line)
       if (allocated(XRDML_intensities_line)) deallocate(XRDML_intensities_line)

       allocate(XRDML_line(1))


       !> Count the number of scans stored in the file. All of them will be summed
       !> to get the final diffraction pattern
       np=0
       do
          read(unit=i_dat, fmt="(a)", iostat=ier) XRDML_line(1)
          if (ier /= 0) exit
          if (index(XRDML_line(1),"</xrdMeasurement>") /= 0) exit
          i1= index(XRDML_line(1), "<intensities unit=""counts"">")
          if (i1 /= 0) np=np+1
       end do
       rewind(unit=i_dat)

       allocate(XRDML_intensities_line(np))

       !> Wavelengths (by JGP)
       do
          !> Kalpha1
          read(unit=i_dat, fmt="(a)", iostat=ier) XRDML_line(1)
          if (ier /= 0) then
             Err_CFML%IErr=1
             ERR_CFML%Msg="Read_Pattern_Panalytical_XRDML@DIFFPATT:  Problems reading Halpha1!"
             close(unit=i_dat)
             return
          end if
          i1= index(XRDML_line(1), '<kAlpha1 unit="Angstrom">')
          if (i1==0) cycle
          i2= index(XRDML_line(1), "</kAlpha1>")
          read(unit=XRDML_line(1)(i1+25:i2-1), fmt=*) pat%wave(1)

          !> Kalpha2
          read(unit=i_dat, fmt="(a)", iostat=ier) XRDML_line(1)
          i1= index(XRDML_line(1), '<kAlpha2 unit="Angstrom">')
          if (i1==0) cycle
          i2= index(XRDML_line(1), "</kAlpha2>")
          read(unit=XRDML_line(1)(i1+25:i2-1), fmt=*) pat%wave(2)

          !>Kbeta
          read(unit=i_dat, fmt="(a)", iostat=ier) XRDML_line(1)
          i1= index(XRDML_line(1), '<kBeta unit="Angstrom">')
          if (i1==0) cycle
          i2= index(XRDML_line(1), "</kBeta>")
          read(unit=XRDML_line(1)(i1+25:i2-1), fmt=*) pat%wave(4)

          !>Kratio
          read(unit=i_dat, fmt="(a)", iostat=ier) XRDML_line(1)
          i1= index(XRDML_line(1), '<ratioKAlpha2KAlpha1>')
          if (i1==0) cycle
          i2= index(XRDML_line(1), "</ratioKAlpha2KAlpha1>")
          read(unit=XRDML_line(1)(i1+21:i2-1), fmt=*) pat%wave(3)

          exit
       end do

       !> recherche de "<positions axis="2Theta" unit="deg">"
       do
          read(unit=i_dat, fmt="(a)", iostat=ier) XRDML_line(1)
          if (ier /= 0) then
             Err_CFML%IErr=1
             Err_CFML%Msg="Read_Pattern_Panalytical_XRDML@DIFFPATT: Problems reading 2theta!"
             close(unit=i_dat)
             return
          end if
          i1= index(XRDML_line(1), "<positions axis=""2Theta"" unit=""deg"">")
          if (i1/=0) exit
       end do

       !> recherche de 2theta_min
       do
          read(unit=i_dat, fmt="(a)", iostat=ier) XRDML_line(1)
          if (ier /= 0) then
             Err_CFML%IErr=1
             Err_CFML%Msg="Read_Pattern_Panalytical_XRDML@DIFFPATT: Problems reading 2theta!"
             close(unit=i_dat)
             return
          end if
          i1= index(XRDML_line(1), "<startPosition>")
          if (i1==0) cycle
          i2= index(XRDML_line(1), "</startPosition>")
          read(unit=XRDML_line(1)(i1+15:i2-1), fmt=*) pat%xmin
          exit
       end do

       !> recherche de 2theta_max
       do
          read(unit=i_dat, fmt="(a)", iostat=ier) XRDML_line(1)
          if (ier /= 0) then
             Err_CFML%IErr=1
             Err_CFML%Msg="Read_Pattern_Panalytical_XRDML@DIFFPATT: Problems reading 2theta!"
             close(unit=i_dat)
             return
          end if
          i1= index(XRDML_line(1), "<endPosition>")
          if (i1==0) cycle
          i2= index(XRDML_line(1), "</endPosition>")
          read(unit=XRDML_line(1)(i1+13:i2-1), fmt=*) pat%xmax
          exit
       end do

       nr=0
       do
          read(unit=i_dat, fmt="(a)", iostat=ier) XRDML_line(1)
          if (ier /= 0) then
             Err_CFML%IErr=1
             Err_CFML%Msg="Read_Pattern_Panalytical_XRDML@DIFFPATT: Problems reading Intensities!"
             close(unit=i_dat)
             return
          end if
          i1= index(XRDML_line(1), "<intensities unit=""counts"">")
          if (i1==0) cycle
          i2= index(XRDML_line(1), "</intensities>")
          nr=nr+1
          XRDML_intensities_line(nr) = XRDML_line(1)(i1+27:i2-1)
          if(nr == np) exit
       end do

       XRDML_intensities_line(1:np)=adjustl(XRDML_intensities_line(1:np))
       nl=LEN_TRIM(XRDML_intensities_line(1))
       i1=1
       do i=2,nl
          if (XRDML_intensities_line(1)(i:i) /= " ") then
             if (XRDML_intensities_line(1)(i-1:i-1) == " ") then
                i1=i1+1
             end if
          end if
       end do
       pat%npts=i1

       if (pat%npts <= 0) then
          Err_CFML%IErr=1
          Err_CFML%Msg="Read_Pattern_Panalytical_XRDML@DIFFPATT: Number of points!"
          close(unit=i_dat)
          return
       end if

       call Allocate_Pattern(pat)

       if (allocated(counts)) deallocate(counts)
       allocate(counts(pat%npts))

       do i=1,np
          read(unit=XRDML_intensities_line(i), fmt=*, iostat=ier) counts(1:pat%npts)
          if (ier /= 0) then
             Err_CFML%IErr=1
             Err_CFML%Msg="Read_Pattern_Panalytical_XRDML@DIFFPATT: Problems reading a profile XRDML-DATA file!"
             close(unit=i_dat)
             return
          end if
          pat%y(1:pat%npts)=pat%y(1:pat%npts)+counts(1:pat%npts)
       end do

       step = (pat%xmax - pat%xmin) / real(pat%npts-1)
       do i=1,pat%npts
          pat%x(i)=pat%xmin+real(i-1)*step
          pat%sigma(i) = abs(pat%y(i))
       end do
       pat%ymin=minval(pat%y(1:pat%npts))
       pat%ymax=maxval(pat%y(1:pat%npts))

       if (allocated(XRDML_line))             deallocate(XRDML_line)
       if (allocated(XRDML_intensities_line)) deallocate(XRDML_intensities_line)
       if (allocated(counts))                 deallocate(counts)

       !> Close
       close(unit=i_dat)
    End Subroutine Read_Pattern_Panalytical_XRDML

End Submodule DiffP_ReadPatt_PAN