SubModule (CFML_DiffPatt) DiffP_ReadPatt_Socabim

 implicit none

  Contains
    !!--++
    !!--++ READ_PATTERN_SOCABIM
    !!--++
    !!--++    Read a pattern for Socabim
    !!--++
    !!--++ 01/05/2019
    !!
    Module Subroutine Read_Pattern_Socabim(Filename,Pat)
       !---- Arguments ----!
       character(len=*),    intent(in)  :: Filename      ! Path+Filename
       class(DiffPat_Type), intent(out) :: Pat

       !---- Local Variables ----!
       logical                                      :: string_counts, string_CPS, string_2thetacounts, string_2thetacps ,free_format
       character(len=132)                           :: line
       character(len=30),dimension(30)              :: dire
       character(len=1)                             :: separateur
       integer                                      :: i, j, i1, long, nb_sep, nb_col, n, ier,i_dat
       real(kind=cp)                                :: step_time,step
       logical                                      :: info


       !> Init
       call clear_error()

       !> File exists?
       inquire(file=trim(filename),exist=info)
       if ( .not. info) then
          Err_CFML%IErr=1
          Err_CFML%Msg="Read_Pattern_Socabim@DIFFPATT: The file "//trim(filename)//" doesn't exist"
          return
       end if

       !> Open File
       open(newunit=i_dat,file=trim(filename),status="old",action="read",position="rewind",iostat=ier)
       if (ier /= 0 ) then
          Err_CFML%IErr=1
          Err_CFML%Msg="Read_Pattern_Socabim@DIFFPATT: Problems opening the file: "//trim(filename)
          return
       end if

       !> Init
       string_COUNTS       = .false.
       string_CPS          = .false.
       string_2THETACOUNTS = .false.
       string_2THETACPS    = .false.
       free_format         = .false.

       nb_sep = 0    ! nombre de separateurs
       nb_col = 0    ! nombre de colonnes

       !> recherche du type de donnees et de divers parametres (step, 2theta_min ...)
       DO
           read(unit=i_dat,fmt="(a)",IOSTAT=ier) line

           if (ier /= 0) then
              Err_CFML%IErr=1
              Err_CFML%Msg="Read_Pattern_Socabim@DIFFPATT: Problems on Socabim UXD Intensity file!"
              close(unit=i_dat)
              return
           end if

           if (len_trim(line) == 0) cycle
           IF (line(1:7) == "_COUNTS") THEN
              string_COUNTS    = .true.
              if (index(line,"=")/=0) then
                 read(unit=i_dat,fmt="(a)",iostat=ier) line
                 if (ier /= 0) then
                    Err_CFML%IErr=1
                    Err_CFML%Msg="Read_Pattern_Socabim@DIFFPATT: Problems on Socabim UXD Intensity file!"
                    close(unit=i_dat)
                    return
                 end if
              end if
              EXIT

           ELSE IF (line(1:4)  =="_CPS") THEN
              string_CPS = .true.
              if (index(line,"=")/=0) then
                 read(unit=i_dat,fmt="(a)",iostat=ier) line
                 if (ier /= 0) then
                    Err_CFML%IErr=1
                    Err_CFML%Msg="Read_Pattern_Socabim@DIFFPATT: Problems on Socabim UXD Intensity file!"
                    close(unit=i_dat)
                    return
                 end if
              end if
              exit

           ELSE IF (line(1:13) =="_2THETACOUNTS") then
              string_2THETACOUNTS = .true.
              if (index(line,"=") /= 0) then
                 read(unit=i_dat,fmt="(a)",iostat=ier) line
                 if (ier /= 0) then
                    Err_CFML%IErr=1
                    Err_CFML%Msg="Read_Pattern_Socabim@DIFFPATT: Problems on Socabim UXD Intensity file!"
                    close(unit=i_dat)
                    return
                 end if
              end if
              exit

           ELSE IF (line(1:10) == "_2THETACPS") THEN
              string_2THETACPS = .true.
              if (index(line,"=") /= 0) then
                 read(unit=i_dat,fmt="(a)",iostat=ier) line
                 if (ier /= 0) then
                    Err_CFML%IErr=1
                    Err_CFML%Msg="Read_Pattern_Socabim@DIFFPATT: Problems on Socabim UXD Intensity file!"
                    close(unit=i_dat)
                    return
                 end if
              end if
              EXIT

           ELSE IF (line(1:7) == "_2THETA" .or. line(1:6) == '_START') THEN
              i = INDEX(line,"=")
              j = LEN_TRIM(line)
              if (LEN_TRIM(line(i+1:j)) /=0) then
                 read(unit=line(i+1:j),fmt=*)  pat%xmin
              end if

           ELSE IF (line(1:9) == "_STEPSIZE") THEN
              i=INDEX(line,"=")
              j = LEN_TRIM(line)
              if (LEN_TRIM(line(i+1:j)) /=0) then
                 read(unit=line(i+1:j),fmt=*)  step
              end if

           ELSE IF (line(1:9) == "_STEPTIME") then
              i=INDEX(line,"=")
              j = LEN_TRIM(line)
              if (LEN_TRIM(line(i+1:j)) /=0) then
                 read(unit=line(i+1:j),fmt=*)  step_time
              end if

           ELSE IF (line(1:10) == "_STEPCOUNT") THEN
              i=INDEX(line,"=")
              j = LEN_TRIM(line)
              if (LEN_TRIM(line(i+1:j)) /=0) then
                 read(unit=line(i+1:j),fmt=*)  pat%npts
              end if

           ELSE IF (line(1:4) == "_WL1") THEN
              i=INDEX(line,"=")
              j = LEN_TRIM(line)
              if (LEN_TRIM(line(i+1:j)) /=0) then
                 read(unit=line(i+1:j),fmt=*)  pat%wave(1)
              end if

           ELSE IF (line(1:4) == "_WL2") THEN
              i=INDEX(line,"=")
              j = LEN_TRIM(line)
              if (LEN_TRIM(line(i+1:j)) /=0) then
                 read(unit=line(i+1:j),fmt=*)  pat%wave(2)
              end if

           ELSE IF (line(1:8) == "_WLRATIO") THEN
              i=INDEX(line,"=")
              j = LEN_TRIM(line)
              if (LEN_TRIM(line(i+1:j)) /=0) then
                 read(unit=line(i+1:j),fmt=*)  pat%wave(3)
              end if

           END IF
        END DO

        if (pat%npts <= 0) then
           !> _STEPCOUNT not given ... estimate the number of points for allocating the diffraction
           !> pattern by supposing the maximum angle equal to 160 degrees
           if (step > 0.000001) then
              pat%npts= nint((160.0-pat%xmin)/step + 1.0_cp)
           else
              Err_CFML%IErr=1
              Err_CFML%Msg="Read_Pattern_Socabim@DIFFPATT: Number of points is zero!"
              close(unit=i_dat)
              return
           end if
        end if

        do
           if (len_trim(line) /= 0) exit
           read(unit=i_dat, fmt="(a)", iostat=ier) line
           if (ier /= 0) then
              Err_CFML%IErr=1
              Err_CFML%Msg="Read_Pattern_Socabim@DIFFPATT: Problems on Socabim UXD Intensity file!"
              close(unit=i_dat)
              return
           end if
        end do

        if (string_2THETACOUNTS .or. string_COUNTS .or. string_2THETACPS) then   ! TR : 29.03.12
           if (index(line, '=') /=0) then
              do
                 read(unit=i_dat, fmt="(a)", iostat=ier) line
                 if (ier /= 0) then
                    Err_CFML%IErr=1
                    Err_CFML%Msg="Read_Pattern_Socabim@DIFFPATT: Problems on Socabim UXD Intensity file!"
                    close(unit=i_dat)
                    return
                 end if
                 if (len_trim(line) == 0) cycle
                 exit
              end do
           end if
        end if

        call Allocate_Pattern(pat)

        !> lecture de la premiere ligne de donnees pour determiner le
        !> format: format libre, type de separateur
        do
           read(unit=i_dat,fmt= "(a)", IOSTAT=ier) line
           if (ier /= 0) then
              Err_CFML%IErr=1
              Err_CFML%Msg="Read_Pattern_Socabim@DIFFPATT: Problems on Socabim UXD Intensity file!"
              close(unit=i_dat)
              return
           end if

           if(len_trim(line) /=0) exit
        end do

        i1 = INDEX(line, CHAR(9))      ! "TAB" ?
        if (i1 /= 0) then
           separateur=CHAR(9)

        else
           i1 = INDEX(line, ";")         ! ";" ?
           if (i1 /= 0) then
              separateur=";"
           else
              i1 = INDEX(line,",")         ! ","
              if (i1 /= 0) separateur = ","
           end if
        end if

        if (i1 == 0) then   ! format libre  (separateur= caractere blanc)
           call get_words(line,dire,nb_col)
           if (nb_col ==0) then
              Err_CFML%IErr=1
              Err_CFML%Msg="Read_Pattern_Socabim@DIFFPATT: Problems on Socabim UXD Intensity file!"
              close(unit=i_dat)
              return
           end if
           free_format = .true.

        else
           !> determination du nombre de tabulations
           do
              nb_sep = nb_sep + 1
              long=LEN_TRIM(line)
              line = adjustl(line(i1+1:long))
              i1=INDEX(line,separateur)
              if (i1 == 0) exit
           end do
           nb_col = nb_sep + 1
        end if

        if (string_2THETACOUNTS  .or. string_2THETACPS) nb_col = nb_col -1
        BACKSPACE(unit=i_dat)   ! on remonte d"une ligne

        !> lecture des donnees
        j = 0       ! indice de la ligne
        n = 0       ! indice du comptage

        do
           j = j+1
           read(unit=i_dat,fmt= "(a)", IOSTAT=ier) line
           if (ier /= 0) exit
           if (len_trim(line) == 0) cycle    ! TR : 29.03.12
           IF (free_format) then
              call get_words(line,dire,nb_col)
              if (nb_col==0) then
                 Err_CFML%IErr=1
                 Err_CFML%Msg="Read_Pattern_Socabim@DIFFPATT: Problems on Socabim UXD Intensity file!"
                 close(unit=i_dat)
                 return
              end if
              if (string_2THETACOUNTS  .or. string_2THETACPS)then
                 nb_col = nb_col - 1           !  << corrected 14.03.02
                 read(unit=line,fmt=*,IOSTAT=ier) pat%x(n+1), (pat%Y(n+i),i=1,nb_col)
                 if (ier /= 0) then
                    n=n-1
                    exit
                 end if
              else
                 read(unit=line,fmt=*, IOSTAT=ier) (pat%Y(n + i),i=1,nb_col)
                 if (ier /= 0) then
                    n=n-1
                    exit
                 end if
              end if
              n = n + nb_col

           else
              if (string_2THETACOUNTS  .or. string_2THETACPS) then
                 i1=INDEX(line,separateur)
                 long=LEN_TRIM(line)
                 read(unit=line(1:i1-1),fmt=*, IOSTAT=ier)pat%x(1+nb_col*(j-1))
                 if (ier /= 0)  exit
                 line = line(i1+1:long)
              end if

              !> lecture des comptages d'une ligne
              if (nb_sep > 1) then
                 do i =1, nb_sep
                    n=n+1
                    i1=INDEX(line, separateur)
                    long=LEN_TRIM(line)
                    if (i1==0) then
                       n=n-1
                       exit
                    end if
                    read(unit=line(1:i1-1), fmt=*, IOSTAT=ier) pat%Y(n)

                    if (ier/=0) then
                       n=n-1
                       exit
                    end if
                    j=j+1
                    line=adjustl(line(i1+1:long))
                 end do
              end if

              !> lecture du dernier point de la ligne
              n =n + 1
              read(unit=line, fmt=*, IOSTAT=ier) pat%Y(n)
              if (ier /= 0) exit
           end if
        end do

        pat%npts = n
        pat%xmax = pat%xmin + step*(pat%npts-1)

        !>creation des abcisses
        !> modif. des comptages si necessaire et calculs sigmas_comptages

        if (string_COUNTS .or. string_2THETACOUNTS ) then
           pat%sigma(1:n ) = sqrt(pat%Y(1:n))
        else  ! data in CPS
           pat%sigma(1:n ) = sqrt(abs(pat%Y(1:n))/step_time)
        end if

        do i=1,pat%npts
           pat%x(i)= pat%xmin+(i-1)*step
        end do
        pat%ymin=minval(pat%y(1:pat%npts))
        pat%ymax=maxval(pat%y(1:pat%npts))

        close(unit=i_dat)
     End subroutine Read_Pattern_Socabim

End SubModule DiffP_ReadPatt_Socabim