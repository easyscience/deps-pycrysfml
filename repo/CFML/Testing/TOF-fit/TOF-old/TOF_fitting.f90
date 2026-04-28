   Module Input_output_data_mod
      use TOF_diffraction
      use CFML_Optimization_LSQ
      use CFML_DiffPatt

      implicit none
      private
      !Public procedures
      public  :: INPUT_data, get_texte, output_plot
      private :: Backa
      ! Global variables
      real,    public :: thmin,step,thmax,ain,afin
      integer, public :: icont,imeth, NPEAKX

      contains

      Function Backa(tth) result(back)
       real, intent(in)    :: tth
       Real                :: Back
       integer             :: ib1,ib2,i1,i2, ib
       real                :: tang

       !  Calculation of the background
       i1=1
       i2=n_ba         ! nb de points de bruit de fond
       ib1=nglob_tof+1 ! nb de parametres globaux
       ib2=ib1+1       ! nb de parametres globaux + 1

        do ib=1,n_ba-1
          if(tth >= BackGroundPoint(ib)%x .and. tth <= BackGroundPoint(ib+1)%x) then
            i1=ib
            i2=ib+1
            ib1=nglob_tof+i1
            ib2=ib1+1
            exit
          end if
        end do
        tang=(tth-BackGroundPoint(i1)%x)/(-BackGroundPoint(i2)%x--BackGroundPoint(i1)%x)
        Back=vs%pv(ib1)+(vs%pv(ib2)-vs%pv(ib1))*tang
        return
      End Function Backa


      Subroutine Get_Texte(lun,texte,ok)
        integer,intent(in)               :: lun
        character(len=*), intent(in out) :: texte
        logical,          intent(out)    :: ok
        integer :: ier
        ok=.false.
        do
          read(unit=lun,fmt="(a)",iostat=ier) texte
          if( ier /= 0) return
          texte=adjustl(texte)
          if(texte(1:1) == "!") cycle
          exit
        end do
        ok=.true.
        return
      End Subroutine Get_Texte

!--------------------------------------------------------------------
     Subroutine output_plot(nob,xx,fobs,sig,fcalc,chi2)
      integer, intent(in)              :: nob
      real,    intent(in),dimension(:) :: xx,fobs,fcalc,sig
      real,    intent(in)              :: chi2
      ! Local variables
      integer                          :: i,j,k,l,ico,ifinal
      real                             :: shb,shd,dif,yma,ymi,tof,tofbragg,alpha,beta,gamm,sigma,&
                                          hg,hl,dsp,dsp2,dsp4,fwhm,eta
      real                             :: iposr

 !    Rewriting the input file
      open(Unit=8,file=trim(filecode)//".new",status="replace",action="write")
      ico=0
      if(c%constr) ico=1
      write(unit=8,fmt="(a)")   trim(title)
      write(unit=8,fmt="(a)")       &
      "!    TOF_init       TOF_fin      Nbac Npeak  Ncyc  Inst  Jobt  Cont Weight Corr Constr Percent "

      write(unit=8,fmt="(2f15.4,10i6)")  &
              ain,afin,n_ba,npeakx,c%icyc,itype,jobtyp,icont,c%iw,c%corrmax,ico,nint(c%percent)

      write(unit=8,fmt="(f14.6,a)") d2tof , "  <=  d to T.O.F. coefficient"
      write(unit=8,fmt="(a)") "!  Global Profile Parameters:"
      write(unit=8,fmt="(f14.6,4x,i2,5x,a)")    vs%pv(1),vs%code(1),   " <= Global-alpha0  &  Flag "
      write(unit=8,fmt="(f14.6,4x,i2,5x,a)")    vs%pv(2),vs%code(2),   " <= Global-alpha1  &       "
      write(unit=8,fmt="(f14.6,4x,i2,5x,a)")    vs%pv(3),vs%code(3),   " <= Global-alpha2  &       "
      write(unit=8,fmt="(f14.6,4x,i2,5x,a)")    vs%pv(4),vs%code(4),   " <= Global-alpha3  &       "
      write(unit=8,fmt="(f14.6,4x,i2,5x,a)")    vs%pv(5),vs%code(5),   " <= Global-beta0   &       "
      write(unit=8,fmt="(f14.6,4x,i2,5x,a)")    vs%pv(6),vs%code(6),   " <= Global-beta1   &       "
      write(unit=8,fmt="(f14.6,4x,i2,5x,a)")    vs%pv(7),vs%code(7),   " <= Global-beta2   &       "
      write(unit=8,fmt="(f14.6,4x,i2,5x,a)")    vs%pv(8),vs%code(8),   " <= Global-beta3   &       "
      write(unit=8,fmt="(f14.6,4x,i2,5x,a)")    vs%pv(9),vs%code(9),   " <= Global-Sig-2   &       "
      write(unit=8,fmt="(f14.6,4x,i2,5x,a)")    vs%pv(10),vs%code(10), " <= Global-Sig-1   &       "
      write(unit=8,fmt="(f14.6,4x,i2,5x,a)")    vs%pv(11),vs%code(11), " <= Global-Sig-0   &       "
      write(unit=8,fmt="(f14.6,4x,i2,5x,a)")    vs%pv(12),vs%code(12), " <= Global-Sig-Q   &       "
      write(unit=8,fmt="(f14.6,4x,i2,5x,a)")    vs%pv(13),vs%code(13), " <= Global-eta0    &       "
      write(unit=8,fmt="(f14.6,4x,i2,5x,a)")    vs%pv(14),vs%code(14), " <= Global-eta1    &       "
      write(unit=8,fmt="(f14.6,4x,i2,5x,a)")    vs%pv(15),vs%code(15), " <= Global-eta2    &       "
      write(unit=8,fmt="(a)") "!  Background Parameters:"
      write(unit=8,fmt="(a)") "!       TOF        Background   Flag "
      do j=1,n_ba   !write background paramers
       write(unit=8,fmt="(2f14.4,i6)") BackGroundPoint(j)%x, vs%pv(j+nglob_tof), vs%code(j+nglob_tof)
      end do
      l=nglob_tof+n_ba+1
      write(unit=8,fmt="(a)") "!  Reflection Parameters:"
      write(unit=8,fmt="(a)") "!    TOF-Bragg     Intensity   Shift-sigma   Shift-alpha    Shift-beta     Shift-eta     Flags"
      do i=1,npeakx    !write peak parameters
        write(unit=8,fmt="(3f14.4,3f14.6,2x,6i2,i8)") &
             vs%pv(l),vs%pv(l+1),vs%pv(l+2),vs%pv(l+3),vs%pv(l+4),vs%pv(l+5),(vs%code(l+k),k=0,5),i
        l=l+nshp_tof
      end do
      write(unit=8,fmt="(a,g14.6)") "!  Chi2 = ",chi2
      close(unit=8)

      yma= -1.E9   !
      ymi=  1.E9   !
      do i=1,nob
        if(fobs(i) > yma ) yma =fobs(i)
        if(fobs(i) < ymi ) ymi =fobs(i)
      end do

       !open(unit=22,file=trim(filecode)//".xys",status="replace")
        shb   = 0.0                         ! idem wpl_pfit.f90
        shd   = ymi - 0.2*(yma-ymi)
        iposr = ymi - 0.1*(yma-ymi)
       open(unit=22,file=trim(filecode)//".xrf",status="replace",action="write")
        write(unit=22,fmt="(2A)") " ",TRIM(title)
        write(unit=22,fmt="(2a)")      " => Data file name: ",TRIM(filedat)
        write(unit=22,fmt="(a,I4)")    " => Instrm        : ", itype
        write(unit=22,fmt="(a,2f14.5)") " => Dtt1(usec)    : ", d2tof, 0.0
        write(unit=22,fmt="(a,I9)")    " => Numb.of.points: ", nob
        write(unit=22,fmt="(a,i9)")    " => Numb.of.peaks : ", npeakx
        write(unit=22,fmt="(a)")   &
        "  Time-of-flight      Yobs          Ycalc       Yobs-Ycal        Backg     Bragg"//   &
        "              Posr     Intensity        Sigma           Alpha        Beta           Eta"
        IF(npeakx > nob) THEN
          ifinal=nob
        ELSE
          ifinal=npeakx
        END IF
        j=nglob_tof+n_ba+1
        Do  i=1,ifinal
          tof=xx(i)
          dif=fobs(i)-fcalc(i)+shd
          tofbragg=vs%pv(j)
          write(unit=22,fmt="(f14.4,4f14.2,2f14.4,tr1,2f14.4,4f14.6)")  &
               tof,fobs(i),fcalc(i), dif, backa(tof)-shb,  &
               tofbragg,iposr, vs%pv(j+1),vs%pv(j+2),vs%pv(j+3),vs%pv(j+4),vs%pv(j+5)
          j=j+nshp_tof
        End Do

        If(npeakx+1 < nob) THEN
          Do  i=npeakx+1,nob
            tof=xx(i)
            dif=fobs(i)-fcalc(i)+shd       ! shd < 0.
            write(unit=22,fmt="(f14.4,4f14.2)") tof,fobs(i),fcalc(i),dif,  &
                backa(tof)-shb
          End Do
        End If
      close (unit=22)
      open(unit=22,file=trim(filecode)//".irf",status="replace",action="write")

      write(unit=22,fmt="(a)") "!  d-spacing       Sigma^2         Gamma         Alpha          Beta        Shift"
      write(unit=22,fmt="(a,i6)") "LIST_SIG_GAM_ALF_BET_SHIFT",npeakx
      l=nglob_tof+n_ba+1
      do i=1,npeakx    !write peak parameters
        dsp=vs%pv(l)/d2tof
        dsp2=dsp*dsp
        dsp4=dsp2*dsp2
        alpha=vs%pv(1) + vs%pv(2)/dsp  + vs%pv(3)/dsp2  + vs%pv(4)/sqrt(dsp) + vs%pv(l+3)
        beta= vs%pv(5) + vs%pv(6)/dsp  + vs%pv(7)/dsp2  + vs%pv(8)/dsp4      + vs%pv(l+4)
        sigma=vs%pv(9)*dsp4 + vs%pv(10)*dsp2 + vs%pv(11) + vs%pv(12)/dsp2    + vs%pv(l+2)
        eta=vs%pv(13) + vs%pv(14)*dsp + vs%pv(15)*dsp2 +  vs%pv(l+5)
        !Calculate gamma from sigma and eta using TCH relations
        FWHM= sqrt_8ln2*sqrt(abs(sigma))
        call get_HG_HL(fwhm,eta,hg,gamm)
        sigma=(hg/sqrt_8ln2)**2
        write(unit=22,fmt="(6f16.7,i8)") dsp, sigma, gamm, alpha, beta, 0.0,i
        l=l+nshp_tof
      end do
      close (unit=22)
      return
     End subroutine output_plot

     Subroutine Input_Data(filename,iform,dif)
        character(len=*),    intent(in) :: filename
        Type(DiffPat_E_Type),intent(out):: dif
        integer,             intent(in) :: iform
        character (len=10) :: modem
        integer :: i
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
          case(12)
            modem="GSASTOF"
        End Select
        Call Read_Pattern(filename,dif,modem)
        if(Err_CFML%Ierr /= 0) then
          write(unit=*,fmt="(a)") trim(Err_CFML%Msg)
          if(Err_CFML%nl /= 0) then
            do i=1, Err_CFML%nl
              write(unit=*,fmt="(a)") trim(Err_CFML%txt(i))
            end do
          end if
         stop
        end if
     End Subroutine Input_Data

   End Module Input_output_data_mod
!----------------------------------------------------------------------------------
!------------------------------------------------------------------
!     Program to fit TOF powder patterns
!     Version 3.0  J.Rodriguez-Carvajal (June-2015)
!     The program fits selected profile zones
!     Marquardt fitting procedure with analytical derivatives
!------------------------------------------------------------------
!------------------------------------------------------------------
   Program TOF_fit
      use CFML_GlobalDeps, only: cp, Err_CFML
      use CFML_Optimization_LSQ
      use TOF_diffraction
      use Input_output_data_mod
      use CFML_DiffPatt,  only : DiffPat_E_Type

      Implicit None

      Integer               :: ll,ier,no,ifail
      Character (Len=256)   :: texte
      Character (Len=4)     :: ext
      Logical               :: esta, numeric,ok
      Integer               :: i,j,k,npts,L,ico

     ! character(len=120), ALLOCATABLE, DIMENSION(:)   :: scroll_lines
      Real                           :: timi,timf
      real,dimension(:), allocatable :: ww
      Integer                        :: narg,lr,ln
      Type(DiffPat_E_Type)           :: df  !Diffraction pattern

      !---- Arguments on the command line ----!
      lr=0
      ln=0
      narg=COMMAND_ARGUMENT_COUNT()
      if(narg > 0) then
              call GET_COMMAND_ARGUMENT(1,filecode)
              i=index(filecode,".pik")
              if(i /= 0) then
                 filecode=filecode(1:i-1)
                 ext=".pik"
              else
                 j=index(filecode,".new")
                 if(j /= 0) then
                    filecode=filecode(1:j-1)
                    ext=".new"
                 else
                    ext=".pik"
                 end if
              end if
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
      WRITE(unit=*,fmt='(a)')'            ------------------------------------ '
      WRITE(unit=*,fmt='(a)')'                  --- PROGRAM: TOF-FIT ---'
      WRITE(unit=*,fmt='(a)')'            (Author: J. Rodriguez-Carvajal, ILL)'
      WRITE(unit=*,fmt='(a)')'                 (version 4.0 June - 2015) '
      WRITE(unit=*,fmt='(a)')'            ------------------------------------ '
      write(unit=*,fmt="(a)") " "

      if( lr == 0) then
         write(unit=*,fmt="(a)") " => Give the name of the input file (e.g. XX.pik or XX.new, <cr>=stop): "
         read(unit=*,fmt="(a)") filecode
         IF(len_trim(filecode) == 0) STOP
         i=index(filecode,".pik")
         if(i /= 0) then
            filecode=filecode(1:i-1)
            ext=".pik"
         else
            j=index(filecode,".new")
            if(j /= 0) then
               filecode=filecode(1:j-1)
               ext=".new"
            else
               ext=".pik"
            end if
         end if
         write(unit=*,fmt="(a)")  " => Give the name of the data file (e.g. xx.dat) "
         write(unit=*,fmt="(2a)") "             (<cr> =",trim(filecode)//".dat): "
         read(unit=*,fmt="(a)") texte
         if(len_trim(texte) == 0) then
            filedat=trim(filecode)//".dat"
         else
            filedat=texte
         end if
      end if
      Open(Unit=1,File=Trim(Filecode)//ext,STATUS="old",iostat=ier,action="read",position="rewind")
      if( ier /= 0 ) then
        write(unit=*,fmt="(a,a)") " => Error openning ",trim(filecode)//ext
        STOP
      end if
      IF(len_trim(filedat) == 0) filedat=trim(filecode)//".dat"
      !-------------------Start Program
      c%percent=50.0
      !      Read the input control file and write in output files

   Do   ! repeat until icont = 0 !

     call get_texte(1,texte,ok)
     if(ok) then
       read(unit=texte,fmt="(a)",iostat=ier)title
       if(ier /= 0) then
          write(unit=*,fmt="(a)") " => End of pik input file at reading text: title! "
          stop
       end if
     else
          write(unit=*,fmt="(a)") " => Error in pik input file at reading TITLE ! "
          stop
     end if
     write(*,"(a)") " => Line: "//trim(texte)
     call get_texte(1,texte,ok)
     if(ok) then
         read(unit=texte,fmt=*,iostat=ier) ain,afin,n_ba,npeakx,c%icyc,itype,jobtyp,icont,c%iw, c%corrmax, ico, c%percent
         if(ier /= 0) then
           write(unit=*,fmt="(a)") " => Error at reading text: tof_ini, tof_fin, n_ba, etc ! "
           stop
         end if
     else
         write(unit=*,fmt="(a)") " => Error in pik input file at reading: tof_ini, tof_fin, n_ba, etc ! "
         stop
     end if
     write(*,"(a)") " => Line: "//trim(texte)
     if(c%corrmax < 1.0) c%corrmax=50.0
     if (ico/=0) c%constr = .true.
     npeaks=npeakx
     call get_texte(1,texte,ok)
     if(ok) then
         read(unit=texte,fmt=*,iostat=ier) d2tof
         if(ier /= 0) then
           write(unit=*,fmt="(a)") " => Error at reading text: d2tof ! "
           stop
         end if
     else
         write(unit=*,fmt="(a)") " => Error in pik input file at reading: d2tof ! "
         stop
     end if
     write(*,"(a)") " => Line: "//trim(texte)
     if(n_ba > nbac) then
       write(unit=*,fmt="(a)") "  ! Warning: too many background parameters !"
       stop
     end if

     do j=1,nglob_tof         !read global parameters
       call get_texte(1,texte,ok)
       if(ok) then
           read(unit=texte,fmt=*,iostat=ier) vs%pv(j), vs%code(j)
           if(ier /= 0) then
             write(unit=*,fmt="(a,i2)") " => Error at reading text: global parameter #",j
             stop
           end if
       else
         write(unit=*,fmt="(a,i2)") " => Error in pik input file at reading global parameter #",j
         stop
       end if
       write(*,"(a)") " => Line: "//trim(texte)
     end do
     do j=1,n_ba        !read background parameters
        call get_texte(1,texte,ok)
        if(ok) then
           read(unit=texte,fmt=*,iostat=ier) BackGroundPoint(j)%x, vs%pv(j+nglob_tof), vs%code(j+nglob_tof)
           if(ier /= 0) then
             write(unit=*,fmt="(a,i3)") " => Error at reading text: background point #",j
             stop
           end if
           BackGroundPoint(j)%y = vs%pv(j+nglob_tof)
        else
           write(unit=*,fmt="(a,i3)") " => Error in pik input file at reading background point #",j
           stop
        end if
        write(*,"(a,i3)") " => Line: "//trim(texte)//"   Back#",j
     end do
     if(ain > BackGroundPoint(1)%x )     ain=BackGroundPoint(1)%x
     if(afin < BackGroundPoint(n_ba)%x ) afin=BackGroundPoint(n_ba)%x

     j=nglob_tof+n_ba+1
     npeaks_rf=0
     DO i=1,npeaks       !read peak parameters
      call get_texte(1,texte,ok)
      if(ok) then
           read(unit=texte,fmt=*,iostat=ier) vs%pv(j),vs%pv(j+1),vs%pv(j+2),vs%pv(j+3),vs%pv(j+4),vs%pv(j+5),(vs%code(j+k),k=0,5)
           if(ier /= 0) then
             write(unit=*,fmt="(a,i3)") " => Error at reading text: peak #",i
             stop
           end if
      else
           write(unit=*,fmt="(a,i3)") " => Error in pik input file at reading peak #",i
           stop
      end if
      write(*,"(a,i3)") " => Line: "//trim(texte)//"   Peak#",i

      if(sum(vs%code(j:j+5)) > 0) npeaks_rf=npeaks_rf+1
      j=j+nshp_tof
     END DO

     call set_nampar_tof(n_ba,npeaks)

      !   Read intensity data

      Call Input_Data(filedat,itype,df)
      call cpu_time(timi)
      Call Tof_Profile_Fitting(Filecode, ain,afin,df, Ifail)
      call cpu_time(timf)
      call Info_LSQ_Output(Chi2,0.0,d%nobs,d%x,d%y,d%yc,d%sw,7,c,vs)
      close(unit=1)
      call Output_Plot(d%nobs,d%x,d%y,d%sw,d%yc,chi2)
      if(icont == 0) exit
   End Do  !icont
   stop
   End Program TOF_fit
!------------------------------------------------------------------------
