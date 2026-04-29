  Module Input_output_data_mod
    use CW_diffraction_PV, only: rla,ngl,n_ba,bac_pos, jobtyp, inter, vs, c,  &
                                 icont, rla1,rla2, filedat, NPEAKX,title, eta1,eta2, &
                                 fwhm1,fwhm2, filecode, use_asymm,use_hps
    use CFML_GlobalDeps, only: Err_CFML
    use CFML_Optimization_LSQ
    use CFML_DiffPatt

    implicit none
    private
    !Public procedures
    public  :: INPUT_data, get_texte, output_plot
    private :: Backa
    ! Global variables
    real,    public :: thmin,step,thmax,ain,afin
    integer, public :: imeth

    contains

    Function Backa(tth) result(back)
     real, intent(in)    :: tth
     Real                :: Back
     integer             :: ib1,ib2,i1,i2, ib
     real                :: tang

     !  Calculation of the background
     i1=1
     i2=n_ba         ! nb de points de bruit de fond
     ib1=ngl+1       ! nb de parametres globaux
     ib2=ib1+1       ! nb de parametres globaux + 1

     do ib=1,n_ba-1
       if(tth >= bac_pos(ib) .and. tth <= bac_pos(ib+1)) then
         i1=ib
         i2=ib+1
         ib1=ngl+i1
         ib2=ib1+1
         exit
       end if
     end do
     tang=(tth-bac_pos(i1))/(bac_pos(i2)-bac_pos(i1))
     Back=vs%pv(ib1)+(vs%pv(ib2)-vs%pv(ib1))*tang
    End Function Backa

    Subroutine Get_Texte(lun,texte)
      integer,intent(in)               :: lun
      character(len=*), intent(in out) :: texte
      do
        read(unit=lun,fmt="(a)") texte
        texte=adjustl(texte)
        if(texte(1:1) == "!") cycle
        exit
      end do
    End Subroutine Get_Texte

    Subroutine Output_Plot(nobs,xx,fobs,fcalc,iform,chi2,rew)
      integer,                     intent(in) :: nobs, iform
      real,dimension(:),           intent(in) :: xx,fobs,fcalc
      real,                        intent(in) :: chi2
      character (len=*), optional, intent(in) :: rew
      ! Local variables
      integer                          :: i,j,k,l,inum,ki,kj,ico,ifinal
      real                             :: shb,shd,dif,yma,ymi,twtet,twtetb
      real                             :: iposr

 !    REWRITING THE INPUT FILE
      if(present(rew)) then
         Open(Unit=8,File=trim(filecode)//".pik",status="replace",action="write")
      else
         Open(Unit=8,File=trim(filecode)//".new",status="replace",action="write")
      end if
      ico=0
      if(c%constr) ico=1
      write(unit=8,fmt="(a)") trim(title)
      write(unit=8,fmt="(a)")       &
      "! Ang_init   Ang_fin  Nbac Npeak  Ncyc   Inte  Inst  Jobt  Cont Weight Corr Constr   %  Method"

      write(unit=8,fmt="(2f10.4,13i6)")  &
              ain,afin,n_ba,npeakx,c%icyc,inter,iform,jobtyp,icont,c%iw,c%corrmax,ico,nint(c%percent),imeth
      write(unit=8,fmt="(2F12.6,a)")rla1,rla2, "  <=  Lambda1 & Lambda2"
      write(unit=8,fmt="(a)") "!  Global Profile Parameters:"
      write(unit=8,fmt="(f14.6,tr4,i2,tr5,a)")    vs%pv(1),vs%code(1), " <= Kalph2/Kalph1 ratio  &  Flag "
      write(unit=8,fmt="(f14.6,tr4,i2,tr5,a)")    vs%pv(2),vs%code(2), " <= Asymmetry-1(S_L)     &    '  "
      write(unit=8,fmt="(f14.6,tr4,i2,tr5,a)")    vs%pv(3),vs%code(3), " <= Asymmetry-2(D_L)     &    '  "
      write(unit=8,fmt="(f14.6,tr4,i2,tr5,a)")    vs%pv(4),vs%code(4), " <= Parameter  U         &    '  "
      write(unit=8,fmt="(f14.6,tr4,i2,tr5,a)")    vs%pv(5),vs%code(5), " <= Parameter  V         &    '  "
      write(unit=8,fmt="(f14.6,tr4,i2,tr5,a)")    vs%pv(6),vs%code(6), " <= Parameter  W         &    '  "
      write(unit=8,fmt="(f14.6,tr4,i2,tr5,a)")    vs%pv(7),vs%code(7), " <= Parameter  Z         &    '  "
      write(unit=8,fmt="(f14.6,tr4,i2,tr5,a)")    vs%pv(8),vs%code(8), " <= Parameter Eta0       &    '  "
      write(unit=8,fmt="(f14.6,tr4,i2,tr5,a)")    vs%pv(9),vs%code(9), " <= Parameter  X         &    '  "
      write(unit=8,fmt="(a)") "!  Background Parameters:"
      write(unit=8,fmt="(a)") "!    2Theta/TOF/Energy   Background    Flag"
      do j=1,n_ba   !write background paramers
       write(unit=8,fmt="(2f14.4,i6)") bac_pos(j), vs%pv(j+ngl), vs%code(j+ngl)
      end do
      l=ngl+n_ba+1
      write(unit=8,fmt="(a)") "!  Reflection Parameters:"
      write(unit=8,fmt="(a)") "!       2Theta     Intensity    Shift-FWHM     Shift-Eta    Flags "
      Do i=1,npeakx    !write peak parameters
        write(unit=8,fmt="(f14.6,f14.3,2f14.6,4i3)") vs%pv(l),vs%pv(l+1),vs%pv(l+2),vs%pv(l+3),(vs%code(l+k),k=0,3)
        l=l+4
      End Do
      write(unit=8,fmt="(a,f14.6)") "!  Chi2 = ",chi2
      close(unit=8)

      yma= -1.E9   !
      ymi=  1.E9   !
      Do i=1,nobs
        if(fobs(i) > yma ) yma =fobs(i)
        if(fobs(i) < ymi ) ymi =fobs(i)
      End Do

       Open(Unit=22,File=trim(filecode)//".xrf",status="replace",action="write")

        shb   = 0.0                         ! idem wpl_pfit.f90
        shd   = ymi - 0.2*(yma-ymi)
        iposr = ymi - 0.1*(yma-ymi)

        write(unit=22,fmt="(2A)") " ",TRIM(title)
        write(unit=22,fmt="(2a)")      " => Data file name: ",TRIM(filedat)
        write(unit=22,fmt="(a,I4)")    " => Instrm        : ", iform
        write(unit=22,fmt="(a,2f9.5)") " => Lambda(1&2)   : ", rla1,rla2
        write(unit=22,fmt="(a,I9)")    " => Numb.of.points: ", nobs
        write(unit=22,fmt="(a,i9)")    " => Numb.of.peaks : ", npeakx
        write(unit=22,fmt="(a)")   &
        "    2Theta     Yobs     Ycalc  Yobs-Ycal    Backg    Bragg     Posr  Intensity   FWHM    Eta"
        IF(npeakx > nobs) THEN
          ifinal=nobs
        ELSE
          ifinal=npeakx
        END IF
        j=ngl+n_ba+1
        Do  i=1,ifinal
          twtet=xx(i)
          dif=fobs(i)-fcalc(i)+shd
          twtetb=vs%pv(j)
          write(unit=22,fmt="(f10.4,4f10.2,f10.4,f10.2,tr1,f10.2,2f8.4)")  &
               twtet,fobs(i),fcalc(i), dif, backa(twtet)-shb,  &
               twtetb,iposr, vs%pv(j+1), fwhm1(i), eta1(i)
          j=j+4
        End Do

        If(npeakx+1 < nobs) THEN
          Do  i=npeakx+1,nobs
            twtet=xx(i)
            dif=fobs(i)-fcalc(i)+shd       ! shd < 0.
            write(unit=22,fmt="(f10.4,4f10.2)") twtet,fobs(i),fcalc(i),dif,  &
                backa(twtet)-shb
          End Do
        End If
      close (UNIT=22)
    End Subroutine Output_Plot

    Subroutine Input_Data(filename,iform,dif)
        character(len=*),   intent(in) :: filename
        class(DiffPat_Type),intent(out):: dif
        integer,            intent(in) :: iform
        character (len=10) :: modem
        Select Case(iform)
          case(0)
            modem="DEFAULT"
          case(1)
            modem="D1AOLD"
          case(2)
            modem="D1BOLD"
          case(3)
            modem="D1B"
          case(4)
            modem="NLS"
          case(5)
            modem="G41"
          case(6)
            modem="D2B"
          case(7)
            modem="3T2"
          case(8)
            modem="DMC"
          case(9,10)
            modem="XYSIGMA"
        End Select
        Call Read_Pattern(filename,dif,modem)
        if(Err_CFML%Ierr /= 0) then
         write(unit=*,fmt="(a)") trim(Err_CFML%Msg)
         stop
        end if
      End Subroutine Input_Data

  End Module Input_output_data_mod
  !----------------------------------------------------------------------------------
  !------------------------------------------------------------------
  !     PROGRAM TO FIT POWDER PATTERNS
  !     VERSION 2.1  J.RODRIGUEZ-CARVAJAL (APRIL-1988)
  !     THE PROGRAM FITS SELECTED PROFILE ZONES
  !     MARQUARDT FITTING PROCEDURE WITH ANALYTICAL DERIVATIVES
  !------------------------------------------------------------------
  !------------------------------------------------------------------
  Program Xrfit
    use CFML_GlobalDeps, only: cp, Err_CFML
    use CFML_Optimization_LSQ
    use CW_diffraction_PV
    use Input_output_data_mod
    use CFML_Profiles, only : Init_Prof_Val
    use CFML_DiffPatt, only : DiffPat_E_Type

    Implicit None

    Integer               :: ll,ier,no
    Character (Len=512)   :: texte
    Logical               :: esta, numeric
    Integer               :: i,j,k,npeak,npts,L,ico

    character(len=120), allocatable, dimension(:)   :: scroll_lines
    Real                           :: chi2, timi,timf
    real,dimension(:), allocatable :: ww
    Integer                        :: narg,lr,ln
    Type(DiffPat_E_Type) :: df  !Diffraction pattern

    !---- Arguments on the command line ----!
    lr=0
    ln=0
    narg=COMMAND_ARGUMENT_COUNT()
    if(narg > 0) then
            call GET_COMMAND_ARGUMENT(1,filecode)
            i=index(filecode,".pik")
            if(i /= 0) filecode=filecode(1:i-1)
            ln=len_trim(filecode)
            filedat=trim(filecode)//".dat"
            lr=ln+4
    end if
    if(narg > 1) then
            call GET_COMMAND_ARGUMENT(2,filedat)
            i=index(filedat,".")
            if(i == 0) filedat=trim(filedat)//".dat"
            lr=len_trim(filedat)
    end if
    numeric=.false.
    if(narg > 2) numeric=.true.
    !   Header and openning of files

    write(unit=*,fmt="(a)")" "
    write(unit=*,fmt="(a)")"            ------------------------------------ "
    write(unit=*,fmt="(a)")"                  --- PROGRAM: XRFIT ---"
    write(unit=*,fmt="(a)")"            (Author: J. Rodriguez-Carvajal, ILL)"
    write(unit=*,fmt="(a)")"                  (Original version 1986) "
    write(unit=*,fmt="(a)")"                (ILL version 2.1 April 1988) "
    write(unit=*,fmt="(a)")"                (LLB Updated(F90) March 1998) "
    write(unit=*,fmt="(a)")"                   (Updated July 2010) "
    write(unit=*,fmt="(a)")"            ------------------------------------ "
    write(unit=*,fmt="(a)") " "

    if( lr == 0) then
       write(unit=*,fmt="(a)") " => Give the code of the files (XX for XX.pik, <cr>=stop): "
       read(unit=*,fmt="(a)") filecode
       IF(len_trim(filecode) == 0) STOP
       write(unit=*,fmt="(a)") " => Give the code of data file (xx for xx.dat) "
       write(unit=*,fmt="(a,a,a)") "                 (<cr> =",trim(filecode),"): "
       read(unit=*,fmt="(a)") filedat
    end if
    Open(Unit=1,File=Trim(Filecode)//".pik",STATUS="old",iostat=ier,action="read",position="rewind")
    if( ier /= 0 ) then
      write(unit=*,fmt="(a,a)") " => Error openning ",trim(filecode)//".pik"
      Stop
    end if
    If(len_trim(filedat) == 0) filedat=trim(filecode)//".dat"
    Open(Unit=7, File=trim(filecode)//".out",status="replace",action="write")
    !-------------------Start Program
    c%percent=50.0
    write(unit=7,fmt="(a)")"            ------------------------------------ "
    write(unit=7,fmt="(a)")"                  --- PROGRAM: XRFIT ---"
    write(unit=7,fmt="(a)")"            (Author: J. Rodriguez-Carvajal, ILL)"
    write(unit=7,fmt="(a)")"                  (Original version 1986) "
    write(unit=7,fmt="(a)")"                (ILL version 2.1 April 1988) "
    write(unit=7,fmt="(a)")"                (LLB Updated(F90) March 1998) "
    write(unit=7,fmt="(a)")"                   (Updated July 2010) "
    write(unit=7,fmt="(a)")"            ------------------------------------ "

    !      Read the input control file and write in output files

    Do ! repeat until icont = 0 !

      call get_texte(1,texte)
      read(unit=texte,fmt="(a)",iostat=ier)title
      if(ier /= 0) then
         write(unit=*,fmt="(a)") " => End of pik input file ! "
         stop
      end if
      call get_texte(1,texte)
      read(unit=texte,fmt=*,IOSTAT=ier) ain,afin,n_ba,npeakx,c%icyc,inter,itype,jobtyp,icont,c%iw, c%corrmax, ico, c%percent, imeth
      if(c%corrmax < 1.0) c%corrmax=50.0
      if (ico/=0) c%constr = .true.
      npeak=npeakx
      call get_texte(1,texte)
      read(unit=texte,fmt=*) rla1,rla2
      rla=rla2/rla1
      if(n_ba > nbac) then
        write(unit=*,fmt="(a)") "  ! Warning: too many background parameters !"
        stop
      end if

      do j=1,ngl         !read global parameters
        call get_texte(1,texte)
        read(unit=texte,fmt=*) vs%pv(j), vs%code(j)
      end do
      do j=1,n_ba        !read background parameters
         call get_texte(1,texte)
         read(unit=texte,fmt=*) bac_pos(j), vs%pv(j+ngl), vs%code(j+ngl)
      end do
      if(ain > bac_pos(1) )     ain=bac_pos(1)
      if(afin < bac_pos(n_ba) ) afin=bac_pos(n_ba)

      j=ngl+n_ba+1
      Do i=1,npeak       !read peak parameters
       call get_texte(1,texte)
       read(unit=texte,fmt=*) vs%pv(j),vs%pv(j+1),vs%pv(j+2),vs%pv(j+3),(vs%code(j+k),k=0,3)
       j=j+4
      End Do

      if(inter == 1) close(unit=1)

      write(unit=7,fmt="(/,a)")        " => 2Theta range, peaks and cycles read "
      write(unit=7,fmt="(a,tr1,f12.6)")" => 2Theta(init)    :",ain
      write(unit=7,fmt="(a,tr1,f12.6)")" => 2Theta(fin )    :",afin
      write(unit=7,fmt="(a,tr1,i5  )")" => Number of peaks :",npeak
      write(unit=7,fmt="(a,tr1,i5  )")" => Number of background points :",n_ba
      write(unit=7,fmt="(a,tr1,i5  )")" => Number of cycles:",c%icyc
      rla=rla2/rla1
      vs%np = ngl+ n_ba + 4 * npeak

      IF(jobtyp == 2 .or. jobtyp==4) THEN
       vs%code(1) = 0    ! ratio Ka1/Ka2
       vs%pv(1)   = 0.0
      END IF

      if(vs%pv(2)+vs%pv(3) >= 0.00001) use_asymm=.true.
      if(vs%pv(3) <= 0.00001) then
        use_hps=.true.
        vs%pv(3)=0.0
      end if

      !save input data
      j = 0
      do i=1,vs%np
       if (vs%code(i) /=0) j = j + 1
      end do
      c%npvar  = j    ! number of refined parameters

      write(unit=7,fmt="(/,a)")          " => Global parameters                 Flag"
      write(unit=7,fmt="(a,f14.6,i3)")   " => Kalph2/Kalph1 ratio :", vs%pv(1),vs%code(1)
      write(unit=7,fmt="(a,f14.6,i3)")   " => Asymmetry-1(S_L)    :", vs%pv(2),vs%code(2)
      write(unit=7,fmt="(a,f14.6,i3)")   " => Asymmetry-2(D_L)    :", vs%pv(3),vs%code(3)
      write(unit=7,fmt="(/,a)")          "      Profile Parameters for Pseudo-Voigt"
      write(unit=7,fmt="(a)")            "        pV(x) = ETA* L(x) + (1-ETA)* G(x)"
      write(unit=7,fmt="(a)")            "    FWHM = SQRT((U tanT + V) tanT + W) + Z/cosT"
      write(unit=7,fmt="(a,/)")          "    ETA  = Eta0 + X * 2T "
      write(unit=7,fmt="(a,f14.6,i3)")   " => Parameter  U        :", vs%pv(4),vs%code(4)
      write(unit=7,fmt="(a,f14.6,i3)")   " => Parameter  V        :", vs%pv(5),vs%code(5)
      write(unit=7,fmt="(a,f14.6,i3)")   " => Parameter  W        :", vs%pv(6),vs%code(6)
      write(unit=7,fmt="(a,f14.6,i3)")   " => Parameter  Z        :", vs%pv(7),vs%code(7)
      write(unit=7,fmt="(a,f14.6,i3)")   " => Parameter Eta0      :", vs%pv(8),vs%code(8)
      write(unit=7,fmt="(a,f14.6,i3)")   " => Parameter  X        :", vs%pv(9),vs%code(9)
      write(unit=7,fmt="(/,a)")          " => Background parameters"
      write(unit=7,fmt="(a)")            "      Scatt. Variable     Background  Flag"
      do j=1,n_ba
        write(unit=7,fmt="(2f18.4,i4)")   bac_pos(j), vs%pv(j+ngl),vs%code(j+ngl)
      end do

      write(unit=7,fmt="(/,a,/)")                                                    &
           "      Position      Intensity      D-Gamma         D-Eta            Flags"
      j=ngl+n_ba+1

      DO i=1,npeak
        write(unit=7,fmt="(f14.6,f14.2,2F14.6,tr5,4i3)") vs%pv(j),vs%pv(j+1),vs%pv(j+2),vs%pv(j+3),   &
                                    vs%code(j),vs%code(j+1),vs%code(j+2),vs%code(j+3)
        j=j+4
      END DO
      call set_nampar(n_ba,npeak,vs)
      write(unit=7,fmt= "(/,a,i4,/)") " => Total number of refined parameters: ", c%npvar

      !   READ INTENSITY DATA

        Call Input_Data(filedat,itype,df)

      no=0
       Do i=1,df%npts
         If(df%x(i) >= ain .AND. df%x(i) <= afin) no=no+1
       End Do

       if (ALLOCATED(d%x ))   deallocate (d%x)
       if (ALLOCATED(d%sw))   deallocate (d%sw)
       if (ALLOCATED(d%y ))   deallocate (d%y)
       if (ALLOCATED(d%yc))   deallocate (d%yc)
       if (ALLOCATED(ww))     deallocate (ww)   !Needed

       allocate ( d%x(no),d%sw(no),d%y(no),d%yc(no),ww(no) )
      j=0
      d%iw=0
      do i=1,df%npts
        if(df%x(i) >= ain .and. df%x(i) <= afin) then
          j=j+1
          if(j > no) exit
          d%y(j)=df%y(i)
          d%sw(j)=sqrt(df%sigma(i)) !Using the LM method one should store the standard deviation d%iw=0
          ww(j)=1.0/df%sigma(i)
          d%x(j)=df%x(i)
          !write(unit=*,fmt="(a,i5,3f14.3)") " i, x ,y ,w: ", j,d%x(j),d%y(j),d%sw(j)
        end if
      end do
      d%nobs=no
      c%tol=epsilon(0.0_cp)
      bac_pos(1) = d%x(1)
      bac_pos(n_ba) = d%x(no)
      write(unit=*,fmt="(a,i6    )") " => Number of points: ",no
      write(unit=*,fmt="(a,2f14.4)") " => Treating data in range: ",ain,afin
      write(unit=*,fmt="(a,2f14.4)") " => Background in range: ",bac_pos(1),bac_pos(n_ba)
      call Init_Prof_Val()

      if(jobtyp > 2) then
         do i=1,d%nobs
           call Sum_PV_Peaks(i,d%x(i),d%yc(i),Vs)
         end do
         chi2=Fchisq(d%nobs-c%npvar,d%Nobs,d%Y,ww,d%Yc)
         write(unit=*,fmt= "(a,f10.5)") " => Simulation work, calculated Chi2: ",chi2
      else
         write(unit=*,fmt= "(a,i4)") " => Total number of refined parameters: ", c%npvar
         write(unit=*,fmt= "(a)") " => Refinement using the Levenberg-Marquardt method "
         call cpu_time(timi)
         if(numeric) then
            call Levenberg_Marquardt_Fit(powder_patt_nder, d%nobs, c, Vs, chi2, texte)
         else
            call Levenberg_Marquardt_Fit(powder_patt_der, d%nobs, c, Vs, chi2, .true., texte)
         end if
         call cpu_time(timf)
          write(unit=*,fmt="(a)") " => "//texte(1:41)
          write(unit=*,fmt="(a)") " => "//texte(41:133)
          write(unit=*,fmt="(a)") " => "//trim(texte(133:))
          write(unit=*,fmt="(a,f15.5)") " => Final   Chi2:", chi2
          !write(unit=*,fmt=*) " => Function and Jacobian evaluations: ",c%nfev,c%njev
          write(unit=*,fmt="(a,f10.3,a)") " => CPU-time:", timf-timi," seconds"
      end if
      if(Err_CFML%Ierr /= 0) stop
      !call Info_LSQ_Output(Chi2,0.0,d%nobs,d%x,d%y,d%yc,d%sw,7,c,vs)
      call Info_LSQ_Output(Chi2,0.0,d%nobs,d%x,d%y,d%yc,ww,7,c,vs)
      if(inter == 1) then
        call Output_Plot(d%nobs,d%x,d%y,d%yc,itype,chi2,"rew")
        exit
      else
        call Output_Plot(d%nobs,d%x,d%y,d%yc,itype,chi2)
      end if
      if(icont == 0) exit
    End Do  !icont

 End Program Xrfit