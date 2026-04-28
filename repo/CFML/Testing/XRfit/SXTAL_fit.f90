  Module Input_output_data_mod
    use CW_diffraction_PV, only: rla,ngl,n_ba,bac_pos, jobtyp, inter, vs, c,  &
                                 icont, rla1,rla2, filedat, NPEAKX,title, eta1,eta2, &
                                 fwhm1,fwhm2, filecode, use_asymm,use_hps
    use CFML_GlobalDeps,   only: Err_CFML,cp
    use CFML_Optimization_LSQ
    use CFML_Strings,      only : pack_string,u_case
    use CFML_BckPeaks,     only : peak_search_cond_type, pkb_cond
    use CFML_gSpaceGroups, only : Spg_Type, Set_SpaceGroup
    !Type,public  :: peak_search_cond_type
    !   real(kind=cp)     :: peak_threshold     = 0.02
    !   real(kind=cp)     :: shoulder_threshold = 2.00
    !   real(kind=cp)     :: bkg_threshold      = 0.05
    !   integer           :: kindOfpeaks        = 1  !single peaks: 1, doublets: 2(Cu-Ka), 3 (Mo-Ka), 4 (Co-Ka)
    !   integer           :: Iterations         = 3
    !End type peak_search_cond_type
    !
    !
    !Type(peak_search_cond_type),save, public :: pkb_cond


    implicit none
    private
    !Public procedures
    public  :: get_texte, output_plot, read_CFL
    private :: Backa
    ! Global variables
    real(kind=cp),                 public :: thmin,step,thmax,ain,afin, norm_mon
    character(len=5),              public :: scan_type="omega"
    real(kind=cp), dimension(6),   public :: cell= [5.0,5.0,5.0,90.0,90.0,90.0]
    integer,                       public :: nkvec=0,jsc=1
    real(kind=cp), dimension(3,24),public :: kvec=0.0
    logical,                       public :: cell_given=.false., kvec_given=.false.,sigma_given=.false., &
                                             spg_given=.false., pkb_given=.false.
    real(kind=cp), dimension(3),   public :: h_ini, h_fin
    character(len=2), dimension(3),public :: scan_along=["a*","b*","c*"]
    type(Spg_Type),                public :: SpG
    real(kind=cp), dimension(Max_Free_par),public :: lower, upper
    integer,                       public :: c_print
    character(len=20),             public :: spg_symb


    contains

    Function Backa(tth) result(back)
       real(kind=cp), intent(in)    :: tth
       Real(kind=cp)                :: Back
       integer                      :: ib1,ib2,i1,i2, ib
       real(kind=cp)                :: tang

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

    Subroutine Output_Plot(nobs,xx,fobs,fcalc,nref,chi2,refl)
      integer,                     intent(in) :: nobs, nref
      real,dimension(:),           intent(in) :: xx,fobs,fcalc
      real,                        intent(in) :: chi2
      character (len=*),           intent(in) :: refl
      ! Local variables
      integer                          :: i,j,k,l,inum,ki,kj,ico,ifinal,i_plot=22
      real                             :: shb,shd,dif,yma,ymi,twtet,twtetb,gam
      real                             :: iposr
      character (len=90)  :: refplot


      yma= -1.E9   !
      ymi=  1.E9   !
      Do i=1,nobs
        if(fobs(i) > yma ) yma =fobs(i)
        if(fobs(i) < ymi ) ymi =fobs(i)
      End Do
      if(scan_type=="omega") then
         i=index(refl,"Gamma:")
         read(refl(i+6:),*) gam
         write(refplot,"(i6.6,a,i3.3)") nref,"_",nint(gam)
      else
         write(refplot,"(a,i6.6)") "Scan_",nref
      end if
       Open(Unit=i_plot,File=trim(refplot)//".xrf",status="replace",action="write")

        shb   = 0.0                         ! idem wpl_pfit.f90
        shd   = ymi - 0.2*(yma-ymi)
        iposr = ymi - 0.1*(yma-ymi)

        !write(unit=i_plot,fmt="(2A)") " ",TRIM(title)
        Select Case(scan_type)
          Case("omega")
             write(unit=i_plot,fmt="(a)") trim(refl)
             write(unit=i_plot,fmt="(2a)")      " => Data file name: ",TRIM(filedat)
             write(unit=i_plot,fmt="(a)")       " => Scattering Variable:  Omega"
             write(unit=i_plot,fmt="(a,2f9.5)") " => Lambda(1&2)   : ", rla1,rla2
             write(unit=i_plot,fmt="(a,I9)")    " => Numb.of.points: ", nobs
             write(unit=i_plot,fmt="(a,i9)")    " => Numb.of.peaks : ", npeakx
             write(unit=i_plot,fmt="(a)")   &
             "    Omega     Yobs     Ycalc  Yobs-Ycal    Backg    Bragg     Posr  Intensity   FWHM    Eta"
          Case("qscan")
             write(unit=i_plot,fmt="(a)") trim(refl)
             write(unit=i_plot,fmt="(2a)")      " => Data file name: ",TRIM(filedat)
             write(unit=i_plot,fmt="(a)")       " => Scattering Variable:  r.l.u."
             write(unit=i_plot,fmt="(a,2f9.5)") " => Lambda(1&2)   : ", rla1,rla2
             write(unit=i_plot,fmt="(a,I9)")    " => Numb.of.points: ", nobs
             write(unit=i_plot,fmt="(a,i9)")    " => Numb.of.peaks : ", npeakx
             write(unit=i_plot,fmt="(a)")   &
             " Scan Along "//scan_along(jsc)//"  Yobs     Ycalc  Yobs-Ycal    Backg   PeakPos     Posr  Intensity   FWHM    Eta"

        End Select

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
          write(unit=i_plot,fmt="(f10.4,4f10.2,f10.4,f10.2,tr1,f10.2,2f8.4)")  &
               twtet,fobs(i),fcalc(i), dif, backa(twtet)-shb,  &
               twtetb,iposr, vs%pv(j+1), fwhm1(i), eta1(i)
          j=j+4
        End Do

        If(npeakx+1 < nobs) THEN
          Do  i=npeakx+1,nobs
            twtet=xx(i)
            dif=fobs(i)-fcalc(i)+shd       ! shd < 0.
            write(unit=i_plot,fmt="(f10.4,4f10.2)") twtet,fobs(i),fcalc(i),dif,  &
                backa(twtet)-shb
          End Do
        End If
      close (UNIT=i_plot)
    End Subroutine Output_Plot

    Subroutine Read_CFL(cfl_file)
      character(Len=*), intent(in) :: cfl_file
      integer :: i,j, i_cfl, ier
      character(len=20)  :: keyw
      character(Len=180) :: texte
      open(newunit=i_cfl,file=cfl_file,status="old", action="read", position="rewind",iostat=ier)
      if(ier /= 0) then
        write(unit=*,fmt="(a)") " => Error opening the file: "//trim(cfl_file)
        stop
      end if
      filecode=" "
      nkvec=0
      spg_symb=" "
      pkb_cond%peak_threshold =0.05
      pkb_cond%shoulder_threshold =3.0
      pkb_cond%bkg_threshold = 0.1
      pkb_cond%kindOfpeaks = 1
      pkb_cond%Iterations = 4
      c_print=0
      do
         read(unit=i_cfl,fmt="(a)",iostat=ier) texte
         if(ier /= 0) exit
         texte=adjustl(texte)
         i=index(texte," ")
         keyw=u_case(texte(1:i-1))

         !write(*,*) L,trim(texte)
         Select Case(trim(keyw))

           Case("NORM_MONITOR")
              read(texte(13:),*,iostat=ier)  norm_mon
              if(ier /= 0) norm_mon=100000.0

           Case("CELL")
              read(texte(5:),*,iostat=ier)  cell
              if(ier == 0) cell_given=.true.

           Case("SPGR")
              Spg_symb = adjustl(texte(5:))

           Case("CPRINT")
              read(texte(7:),*,iostat=ier)  c_print
              if(ier /= 0) c_print=0

           Case("KVEC")
              nkvec=nkvec+1
              read(texte(5:),*,iostat=ier)  kvec(:,nkvec)
              if(ier == 0) kvec_given=.true.

           Case("PKB_PSBKI")
              read(texte(10:),*,iostat=ier) pkb_cond%peak_threshold,pkb_cond%shoulder_threshold, &
                                            pkb_cond%bkg_threshold,pkb_cond%kindOfpeaks, &
                                            pkb_cond%Iterations
              if(ier /= 0) then
                   pkb_cond%peak_threshold =0.05
                   pkb_cond%shoulder_threshold =3.0
                   pkb_cond%bkg_threshold = 0.1
                   pkb_cond%kindOfpeaks = 1
                   pkb_cond%Iterations = 4
              end if
              pkb_given=.true.

           Case("FILESCAN")
              filecode= adjustl(texte(9:))
              i=index(filecode,".ref")
              j=index(filecode,".qsc")
              if(i /= 0) then
                 filecode=filecode(1:i-1)
                 scan_type="omega"
                 filedat=trim(filecode)//".ref"
              end if
              if(j /= 0) then
                filecode=filecode(1:j-1)
                 scan_type="qscan"
                 filedat=trim(filecode)//".qsc"
              end if

         End Select

      end do
      close(unit=i_cfl)

      write(*,*) " Number of propagation vectors:",nkvec
      do i=1,nkvec
        write(*,*) i," kvec: ",kvec(:,i)
      end do
      write(*,*) " Scan Type: "//scan_type

      if(len_trim(spg_symb) > 2) then
         spg_given=.true.
         call Set_SpaceGroup(spg_symb,SpG)
         if(Err_CFML%Ierr /= 0) spg_given=.false.
      end if

    End Subroutine Read_CFL

  End Module Input_output_data_mod

  Module LSQ_management
    use CFML_GlobalDeps,   only : cp, Err_CFML
    use CFML_BckPeaks
    use Input_output_data_mod
    use CFML_DiffPatt,     only : DiffPat_E_type, Allocate_Pattern, FWHM_peak
    use CW_diffraction_PV, only : ngl,n_ba,bac_pos, bac_int, NPEAKX, vs, c, d, Sum_PV_Peaks
    use CFML_Reflections,  only : h_absent
    Use CFML_Maths,        only : zbelong,epss,Set_Eps_Math,Locate

    implicit none
    public

    contains

     Function Backg(x,n,b_pos,b_int) result(bgr)
         real(kind=cp),              intent(in) :: x
         integer,                    intent(in) :: n
         real(kind=cp),dimension(n), intent(in) :: b_pos,b_int
         real(kind=cp)                          :: bgr

         integer       :: i1,i2,ib
         real(kind=cp) :: tang
         do ib=1,n-1
           if(x >= b_pos(ib) .and. x <= b_pos(ib+1)) then
             i1=ib
             i2=ib+1
            exit
           end if
         end do
         if(abs(b_pos(i2)-b_pos(i1)) < 0.01) then
            tang=0.0
         else
            tang=(x-b_pos(i1))/(b_pos(i2)-b_pos(i1))
         end if
         bgr=b_int(i1)+(b_int(i2)-b_int(i1))*tang
     End Function Backg


     !!---- Subroutine set_initial_conditions_qscan(x1,x2,Pat)
     !!----   real(kind=cp),        intent(in)     :: x1,x2 ! [x1,x2] region to be analysed
     !!----   type(DiffPat_E_type), intent(in out) :: Pat
     !!----
     !!---- This subroutine established a set of initial conditions adapted to Q-scans.
     !!---- The conditions for searching peaks and background determination are set by
     !!---- updating the global variable "pkb_cond" of type peak_search_cond_type in CFML_BackPeaks.
     !!---- The change of refinement conditions are done dynamically by the subroutine manage_LSQ.
     !!----
     !!----
     Subroutine set_initial_conditions_qscan(x1,x2,Pat)
       real(kind=cp),        intent(in)     :: x1,x2 ! [x1,x2] region to be analysed
       type(DiffPat_E_type), intent(in out) :: Pat
       !
       integer              :: i,j,k,L,n, jmax
       type(pkb_type)       :: pkb_pk, pkb_bck
       logical              :: exclude, detected,weak
       real(kind=cp)        :: pos, averb, max_int, resid, ch, fwh, bgr
       integer,dimension(3) :: ihkl
       logical, allocatable, dimension(:) ::remove

       call Allocate_Pattern(Pat,d%nobs)     !Accessible via global variable d of type LSQ_Data_Type
       Pat%x=d%x; Pat%y=d%y; pat%sigma= d%sw !Copy to use automatic background and peak finding

       lower= 0.00001; upper=1.0e+30

       vs%pv(:)=lower; vs%code(:)=0
       vs%pv(6)= 0.000 !Assignment of global FWHM^2 in r.l.u. W
       vs%code(6)= 0
       upper(6)=0.5

       call Automatic_PkB_Search(Pat,x1,x2,"background",pkb_bck)
       if(Err_CFML%flag) then
         write(*,"(a)")  " Fatal error: "//trim(Err_CFML%Msg)
         return
       else
         allocate(remove(pkb_bck%np))
         remove=.false.
         do i=1,pkb_bck%np-1
           do j=i+1,pkb_bck%np
             if(pkb_bck%x(j)-pkb_bck%x(i) < 0.05) then
                 remove(j) = .true.
             else
                 exit
             end if
           end do
         end do
       end if

       pkb_cond%Iterations = 3
       call Automatic_PkB_Search(Pat,x1,x2," ",pkb_pk) !Looking for peaks
       if(Err_CFML%flag) then
         write(*,"(a)")  " Fatal error: "//trim(Err_CFML%Msg)
         return
       end if

       n_ba = 1; averb=0.0
       bac_pos(n_ba)=x1+0.02
       bac_int(n_ba)=pkb_bck%y(1)
       j=ngl+1  !number of global parameters
       vs%pv(j)=pkb_bck%y(1)
       if(abs(pkb_bck%y(1)-bac_pos(n_ba)) < 0.02) remove(1)=.true.

       do_bck: do i=1,pkb_bck%np
          !do k=1,pkb_pk%np
          !  if (abs(real(nint(pkb_pk%x(k)))-pkb_bck%x(i)) < 0.05 .or. remove(i)) cycle do_bck
          !end do
         if (abs(real(nint(pkb_bck%x(i)))-pkb_bck%x(i)) < 0.05 .or. remove(i)) cycle do_bck
         j=j+1
         n_ba = n_ba + 1
         bac_pos(n_ba)=pkb_bck%x(i)
         bac_int(n_ba)=pkb_bck%y(i)
         vs%pv(j)=pkb_bck%y(i) !Assignment of background parameters
         vs%code(j)= 0
         averb=averb+vs%pv(j)
       end do do_bck
       n_ba = n_ba + 1
       bac_pos(n_ba)=x2-0.02
       j=j+1
       vs%pv(j)=pkb_bck%y(pkb_bck%np)
       bac_int(n_ba)=vs%pv(j)
       averb=(averb+vs%pv(j))/n_ba


       !Check now if predicted peak positions are close or not to the detected peak positions
       !If predicted peak positions are not detected, add predicted peaks and fix their position and FWHM.

       !Number of fundamental and satellite peaks
       j=ngl+n_ba+1
       NPEAKX=0

       do i=1,d%nobs
          pos=d%x(i)
          if (abs(real(nint(pos))-pos) < 0.0002) then
             exclude=.false.
             if(spg_given) then
               ihkl=nint(h_ini)
               ihkl(jsc)=nint(pos)
               exclude=h_absent(ihkl,SpG)
             end if
             if(.not. exclude) then
                detected=.false.; weak=.false.
                !Check first if there is a predicted peak that has not been found by peak-search
                do k=1, pkb_pk%np
                   if(abs(pkb_pk%x(k)-pos) < 0.05) then
                    detected=.true.
                    exit
                   end if
                end do
                npeakx=npeakx+1
                bgr=Backg(pos,n_ba,bac_pos(1:n_ba),bac_int(1:n_ba))
                vs%pv(j)  = pos                          !Position
                vs%pv(j+1)= 0.1*(d%y(i)-bgr)             !Intensity, a factor 10 lower than peak intensity
                if(d%y(i) < 0.05 * bgr .or. vs%pv(j+1) <= 1.02 * lower(j+1)) then
                  vs%pv(j+1) = 0.105*bgr
                  weak=.true.
                end if
                !Try to estimate Fwhm
                fwh=FWHM_Peak(Pat, pos, d%y(i), bgr, 0.25)
                vs%pv(j+2)=fwh
                if(fwh > 0.0) then
                    vs%pv(j+2)=fwh
                else
                    vs%pv(j+2)= 0.04              !Local Fwhm
                end if
                vs%pv(j+3)= 0.01                  !Eta
                if(detected .and. .not. weak) then
                  vs%code(j:j+2)=1
                else
                  vs%code(j+1)=1 !Only the intensity is refined
                end if
                lower(j)=pos-0.25
                upper(j)=pos+0.25
                upper(j+1)=1.0e+20
                lower(j+2)=0.0
                upper(j+2)=0.30
                lower(j+3)=0.0
                upper(j+3)=1.0
                j=j+4
             end if

             if(nkvec > 0) then
               do L=1,nkvec
                  pos=d%x(i)-kvec(jsc,L)
                  k=Locate(d%x,pos,d%nobs)
                  if(pos > x1 + 0.02) then
                     detected=.false.; weak=.false.
                     !Check first if there is a predicted peak that has not been found by peak-search
                     do n=1, pkb_pk%np
                        if(abs(pkb_pk%x(n)-pos) < 0.05) then
                         detected=.true.
                         exit
                        end if
                     end do
                     npeakx=npeakx+1
                     vs%pv(j)  = pos    !Position
                     vs%pv(j+1)= d%y(i)-averb           !Intensity
                     if(d%y(i) < 0.05 * averb .or. vs%pv(j+1) <= 1.02 * lower(j+1)) then
                       vs%pv(j+1) = 0.05*averb
                       weak=.true.
                     end if
                     vs%pv(j+2)= 0.03                  !Fwhm
                     vs%pv(j+3)= 0.05                  !Eta
                     if(detected .and. .not. weak) then
                       vs%code(j:j+2)=1
                     else
                       vs%code(j+1)=1 !Only the intensity is refined
                     end if
                     lower(j)=pos-0.25
                     upper(j)=pos+0.25
                     upper(j+1)=1.0e+20
                     lower(j+2)=0.0
                     upper(j+2)=0.35
                     lower(j+3)=0.0
                     upper(j+3)=1.0
                     j=j+4
                  end if
                  pos=d%x(i)+kvec(jsc,L)
                  k=Locate(d%x,pos,d%nobs)
                  if(pos < x2-0.02) then
                     detected=.false.
                     !Check first if there is a predicted peak that has not been found by peak-search
                     do n=1, pkb_pk%np
                        if(abs(pkb_pk%x(n)-pos) < 0.05) then
                         detected=.true.; weak=.false.
                         exit
                        end if
                     end do
                     npeakx=npeakx+1
                     vs%pv(j)  = pos             !Position
                     !vs%pv(j+1)= max(maxval(d%y(max(k-4,1):min(k+4,d%nobs)))-averb,averb*0.08)             !Intensity
                     if(d%y(i) < 0.05 * averb .or. vs%pv(j+1) <= 1.02 * lower(j+1)) then
                       vs%pv(j+1) = 0.05*averb
                       weak=.true.
                     end if
                     vs%pv(j+2)= 0.03                   !Fwhm
                     vs%pv(j+3)= 0.05                   !Eta
                     if(detected .and. .not. weak) then
                       vs%code(j:j+2)=1
                     else
                       vs%code(j+1)=1 !Only the intensity is refined
                     end if
                     lower(j)=pos-0.25
                     upper(j)=pos+0.25
                     upper(j+1)=1.0e+20
                     lower(j+2)=0.0
                     upper(j+2)=0.35
                     lower(j+3)=0.0
                     upper(j+3)=1.0
                     j=j+4
                  end if
               end do
             end if
          end if
       end do
       !Checking that the most intense peak hold the global FWHM
       !j=ngl+n_ba+1
       !max_int=0.0; jmax=ngl+n_ba+2
       !do i=1, npeakx
       !  if(vs%pv(j+1) > max_int) then
       !     jmax=j+1
       !     max_int=vs%pv(jmax)
       !  end if
       !  j=j+4
       !end do
       !vs%code(jmax)=0
       !Checking that the initial parameters are within the lower and upper limits
       do i=1, vs%np
         if(vs%pv(i) < lower(i)) then
            !write(*,"(a,i3,g14.4,a,g14.4)") " Parameter ",i,vs%pv(i),"( "//trim(vs%nampar(i))//" ) lower than ",lower(i)
            vs%pv(i)=lower(i)+0.0001
         else if(vs%pv(i) > upper(i)) then
            !write(*,"(a,i3,g14.4,a,g14.4)") " Parameter ",i,vs%pv(i),"( "//trim(vs%nampar(i))//" ) higher than ",upper(i)
            vs%pv(i)=upper(i)-0.0001
         end if
       end do

       ! Provisory code for testing the initial conditions
       !write(*,"(a)") " Testing the calculation with the initial conditions"
       !ch=0.0
       !do i=1,d%nobs
       !  call Sum_PV_Peaks(i,d%x(i),d%yc(i),vs)
       !  !write(*,"(i6,3f14.4)") i,d%x(i),d%y(i),d%yc(i)
       !  resid= (d%y(i)-d%yc(i))/d%sw(i)
       !  ch=ch+resid*resid
       !end do
       !ch=ch/real(d%nobs-vs%np)
       !write(*,"(a,g14.5)") " Initial Chi2: ",ch
       !if(ch > 1000.0 ) then  !Write the initial conditions
       !   j=ngl+n_ba+1
       !   do i=1, npeakx
       !     write(*,"(a,4f12.4)") " Pos, Int, FWHM, eta: ",vs%pv(j),vs%pv(j+1),vs%pv(j+2),vs%pv(j+3)
       !     write(*,"(a,4i12)")   "               Codes: ",vs%code(j),vs%code(j+1),vs%code(j+2),vs%code(j+3)
       !     j=j+4
       !   end do
       !end if
     End Subroutine set_initial_conditions_qscan

     Subroutine set_initial_conditions_omega_scan(x1,x2,gamm,omega)
       real(kind=cp), intent(in) :: x1,x2,gamm,omega
       integer :: j, no

       no=size(d%x)
       npeakx=1
       n_ba=2
       bac_pos(1)    = x1
       bac_pos(n_ba) = x2
       if(gamm > 93.0) then
         vs%pv(1)=0.8
         vs%code(1)=1
       end if
       vs%pv(6)= 0.25*(d%x(no)-d%x(1)) !Global Fwhm
       vs%code(6)=1
       vs%code(ngl+1:ngl+2)=1          !Background
       vs%pv(ngl+1) = d%y(1)
       vs%pv(ngl+2) = d%y(no)

       j=ngl+n_ba+1
       vs%pv(j)  = omega                 !Position
       vs%pv(j+1)= d%y(no/2)             !Intensity
       vs%pv(j+2)= 0.0                   !Fwhm
       vs%pv(j+3)= 0.0                   !Eta
       vs%code(j:j+1)=1
       if(npeakx > 1) then
         j=j+4
         vs%pv(j)  = omega+0.3             !Position
         vs%pv(j+1)= d%y(no/2)             !Intensity
         vs%pv(j+2)= 0.0                   !Fwhm
         vs%pv(j+3)= 0.0                   !Eta
         vs%code(j:j+1)=1
       end if

     End Subroutine set_initial_conditions_omega_scan

     Subroutine manage_LSQ()


     End Subroutine manage_LSQ

  End Module LSQ_management

  !----------------------------------------------------------------------------------
  !------------------------------------------------------------------
  !     Program to fit single crystal scans
  !     Version 0.1  J.Rodriguez-Carvajal (October-2021)
  !     The Program Fits Selected Profile Zones
  !     Marquardt Fitting Procedure with Analytical Derivatives
  !------------------------------------------------------------------
  !------------------------------------------------------------------
  Program SXTAL_fit
    use CFML_GlobalDeps, only: cp, Err_CFML
    use CFML_Optimization_LSQ
    Use CFML_Maths,        only : zbelong,epss,Set_Eps_Math,Locate
    use CFML_Strings,      only : pack_string
    use CFML_DiffPatt,     only : DiffPat_E_type
    use CW_diffraction_PV
    use LSQ_management
    use Input_output_data_mod
    use CFML_Profiles, only : Init_Prof_Val
    use ODR_wrapper

    Implicit None

    type(DiffPat_E_type)      :: Pat
    Integer                   :: ll,ier,no,np,i,j,k,npeak,npts,L,narg, numor
    Integer                   :: lun=1, i_out=7, i_hkl=8, i_odr=99, i_sc=10
    integer, dimension (2500) :: num_pix
    Logical                   :: esta, redo, exclude
    Real(kind=cp)             :: timi,timf,sumpix,aver,normp, norma, pos, averb,fwh,fg,sfwh
    real(kind=cp)             :: gamm, omega, nu, chi, phi, monitor, inten, sigma,aux1,aux2,aux3
    real(kind=cp),dimension(3):: hkl

    Character (Len=512)                     :: texte,line,saved_line
    Character (Len=130)                     :: refl,testchi2
    Character (Len=80), dimension(2500)     :: profile
    real(kind=cp),dimension(:), allocatable :: ww

    !---- Arguments on the command line ----!
    narg=COMMAND_ARGUMENT_COUNT()
    if(narg > 0) then
          call GET_COMMAND_ARGUMENT(1,texte)
          call Read_CFL(texte)
    else
          write(unit=*,fmt="(a,a)") " => The program should be invoked with an argument like MyCFL_File.cfl"
          stop
    end if
    !   Header and openning of files

    write(unit=*,fmt="(a)")" "
    write(unit=*,fmt="(a)")"            ------------------------------------ "
    write(unit=*,fmt="(a)")"                --- PROGRAM: SXTAL_FIT ---"
    write(unit=*,fmt="(a)")"            (Author: J. Rodriguez-Carvajal, ILL)"
    write(unit=*,fmt="(a)")"                    (Created April 2021) "
    write(unit=*,fmt="(a)")"                   (Version October 2021) "
    write(unit=*,fmt="(a)")"            ------------------------------------ "
    write(unit=*,fmt="(a)") " "

    write(*,*) " => Opening the file: "//trim(filedat)
    write(*,*) " => Code of the file: "//trim(filecode)
    Open(Unit=lun,File=Trim(Filedat),STATUS="old",iostat=ier,action="read",position="rewind")
    if( ier /= 0 ) then
      write(unit=*,fmt="(a)") " => Error openning "//trim(filedat)
      Stop
    end if
    Open(Unit=i_out, File=trim(filecode)//".out",status="replace",action="write")
    Open(Unit=i_hkl, File=trim(filecode)//".hkl",status="replace",action="write")
    Open(Unit=i_odr, File=trim(filecode)//".odr",status="replace",action="write")
    if(c_print == 0) then
        write(unit=i_odr,fmt="(a)")   " The output to this file has been blocked by the user: CPRINT=0"
        write(unit=i_odr,fmt="(/,a)") " Recommended values for CPRINT to be provided in the CFL file"
        write(unit=i_odr,fmt="(a)")   "   CPRINT  1001    <- Short Initial and Final summary  "
        write(unit=i_odr,fmt="(a)")   "   CPRINT  0001    <- Short Final summary  "
        write(unit=i_odr,fmt="(a)")   "   CPRINT  2002    <- Long Initial and Final summary"
        write(unit=i_odr,fmt="(a)")   "   CPRINT  0004    <- Short Final summary in *.odr file and the screen"
        write(unit=i_odr,fmt="(a)")   "   CPRINT  1111    <- Short Initial, Intermediate and Final summary"
        write(unit=i_odr,fmt="(a)")   " See the Guide to ODRPACK95 for more options"
        close(unit=i_odr)
    end if
    !-------------------Start Program
    c%percent=50.0
    write(unit=i_out,fmt="(a)")"            ------------------------------------ "
    write(unit=i_out,fmt="(a)")"                --- PROGRAM: SXTAL_FIT ---"
    write(unit=i_out,fmt="(a)")"            (Author: J. Rodriguez-Carvajal, ILL)"
    write(unit=i_out,fmt="(a)")"                    (Created April 2021) "
    write(unit=i_out,fmt="(a)")"                   (Version October 2021) "
    write(unit=i_out,fmt="(a)")"            ------------------------------------ "

    !      Read the input control file and write in output files
    call Set_Eps_Math(0.005)
    np=0; L=0
    title="SXTAL integration for reflections in file: "//Trim(Filedat)
    call cpu_time(timi)
    !Writing the read data of the CFL file:
    write(unit=i_out,fmt="(a)") " => Title: "//title
    write(unit=i_out,fmt="(a)") " => Scan Type: "//scan_type
    Write(unit=i_out,fmt="(a,f12.1)") " => Normalization monitor: ",norm_mon
    if(cell_given) then
      Write(unit=i_out,fmt="(a,6f10.4)") " => Unit cell parameters: ",cell
    end if
    if(spg_given) then
      Write(unit=i_out,fmt="(a)") " => Space Group: "//Spg_symb
    end if
    if(kvec_given) then
      Write(unit=i_out,fmt="(a)") " => Propagation vector(s): "
      do i=1,nkvec
        write(unit=i_out,fmt="(i4,a,3f8.4,a)") i," kvec: [",kvec(:,i)," ]"
      end do
    end if
    if(pkb_given) then
      Write(unit=i_out,fmt="(a)") " => Provided conditions for background/peak search: "
    else
      Write(unit=i_out,fmt="(a)") " => Default conditions for background/peak search: "
    end if
    Write(unit=i_out,fmt="(a,f6.3)") "     peak_threshold", pkb_cond%peak_threshold
    Write(unit=i_out,fmt="(a,f6.3)") " shoulder_threshold", pkb_cond%shoulder_threshold
    Write(unit=i_out,fmt="(a,f6.3)") "      bkg_threshold", pkb_cond%bkg_threshold
    Write(unit=i_out,fmt="(a,i6)")   "        kindOfpeaks", pkb_cond%kindOfpeaks
    Write(unit=i_out,fmt="(a,i6)")   "         Iterations", pkb_cond%Iterations


    Do ! repeat for all reflections/qscans in the file !

      read(unit=lun,fmt="(a)",iostat=ier) texte
      if(ier /= 0) exit
      if(trim(texte) == "REFLECTION" .or. texte(1:5) == "QSCAN") then
         if(scan_type == "omega") then
            read(unit=lun,fmt="(a)",iostat=ier) texte
            if(ier /= 0) exit
            read(unit=texte,fmt=*,iostat=ier) hkl,gamm,omega,nu,chi,phi,monitor
            if(ier /= 0) then
               write(unit=*,fmt="(a)") " => Problem reading hkl, gamma, omega, nu, chi, phi, monitor ! "
               exit
            else
              np=np+1
              write(unit=i_out,fmt="(/,a,i6,a)") " => Reflection: #",np,"    hkl gamma, omega, nu, chi, phi, monitor: "//trim(texte)
            end if
         else if(scan_type == "qscan") then
            read(unit=texte(6:),fmt=*) h_ini,h_fin,numor
            hkl=abs(h_fin-h_ini)
            do i=1,3
              if(hkl(i) > 0.001) then
                jsc=i
                exit
              end if
            end do
            np=np+1
            write(unit=i_out,fmt="(/,a,i6,a)") " =>     Qscan:#",np," along the "//scan_along(jsc)
            write(unit=i_out,fmt="(a,3f10.4)") "    hkl- init:",h_ini
            write(unit=i_out,fmt="(a,3f10.4)") "    hkl-final:",h_fin
            write(unit=i_out,fmt="(a,i7)")     "        Numor:",Numor
         else
            write(unit=*,fmt="(a)") " => Problem reading, unknown scan type ! "
            exit
         end if
         no=0
         do
            read(lun,"(a)",iostat=ier) line
            if(ier /= 0)  exit
            if(trim(line) == "REFLECTION" .or. line(1:5) == "QSCAN") then
              no=no-1
              backspace(unit=lun)
              exit
            end if
            Select Case(scan_type)
              case("omega")
                read(line,*) aux1,aux2,L  !Eliminate points with zero pixels
                if(L == 0 ) cycle
              case("qscan")
                read(line,*,iostat=ier) aux1,aux2,aux3
                if(ier /= 0) then
                  read(line,*,iostat=ier) aux1,aux2
                  sigma_given=.false.
                else
                  sigma_given=.true.
                end if
                if(abs(aux2) < 1.0) cycle !eliminate zero-intensity points
            End Select
             no=no+1
             profile(no)=line
         end do
         line=" "
         if(no < 10) cycle
         if (Allocated(d%x ))   deallocate (d%x)
         if (Allocated(d%sw))   deallocate (d%sw)
         if (Allocated(d%y ))   deallocate (d%y)
         if (Allocated(d%yc))   deallocate (d%yc)
         if (Allocated(ww))     deallocate (ww)
         allocate ( d%x(no),d%sw(no),d%y(no),d%yc(no),ww(no) )
         d%nobs=no
         ww=0.0
         sumpix=0.0
         Select Case(scan_type)
           case("omega")
             do i=1,d%nobs
                read(unit=profile(i),fmt=*) d%x(i),d%y(i),num_pix(i)
                d%sw(i)=sqrt(d%y(i))
                sumpix=sumpix+num_pix(i)
             end do
             aver=sumpix/no
           case("qscan")
             if(sigma_given) then
                num_pix=1
                do i=1,d%nobs
                   read(unit=profile(i),fmt=*) d%x(i),d%y(i),d%sw(i) !,num_pix(i)
                   !sumpix=sumpix+num_pix(i)
                   sumpix=sumpix+1
                end do
             else
                do i=1,d%nobs
                   read(unit=profile(i),fmt=*) d%x(i),d%y(i)
                   d%sw(i)=sqrt(d%y(i))
                   sumpix=sumpix+1
                end do
             end if
         End Select
         aver=sumpix/no
         step=0.0
         do i=1,d%nobs
           if(i > 1) step=step + abs(d%x(i)-d%x(i-1))
           normp=aver / num_pix(i)
           d%y(i) =  d%y(i) * normp
           d%sw(i)= d%sw(i) * normp
           if(abs(d%sw(i)) > 0.001) ww(i)=1.0/(d%sw(i)*d%sw(i))
         end do
         step=step/no

         ! Initial values
         if(scan_type == "omega") then
            norma=abs(sind(gamm) * cosd(nu) * (d%nobs-1)*step) * norm_mon/monitor
            ain=d%x(1)
            afin=d%x(no)
         else
            norma=1.0
            ain=d%x(1)
            afin=d%x(no)
         end if

         !Setting Background points
         !Determining the number of peaks for Qscans
         vs%pv(1:ngl)=0.0
         vs%code(1:ngl)=0

         Select Case (scan_type)
           Case("omega")
             call set_initial_conditions_omega_scan(d%x(12),d%x(no-12),gamm,omega)
             write(unit=i_out,fmt="(/,a)")        " => Omega range, peaks and cycles read "
             write(unit=i_out,fmt="(a,tr1,f12.6)")" => Omega(init)    :",ain
             write(unit=i_out,fmt="(a,tr1,f12.6)")" => Omega(fin )    :",afin
             rla2=1.5
             rla1=1.54

           Case("qscan")  !Determine the expected number of peaks as a function of kvec and the range in Q
             rla2=1.5
             rla1=1.5
             call set_initial_conditions_qscan(d%x(1),d%x(no),Pat)
             if(Err_CFML%flag) then
                !Output of the scan giving rise to problems
                write(unit=line,fmt="(a,i6.6,a)") "Scan_",np,".xys"
                open(unit=i_sc, file=trim(line),status="replace",action="write")
                write(unit=i_sc,fmt="(a)") "Problems in refinement: "//trim(texte)
                write(unit=i_sc,fmt="(a)") "! "
                write(unit=i_sc,fmt="(a)") "! "
                write(unit=i_sc,fmt="(a)") "! "
                write(unit=i_sc,fmt="(a)") "! "
                write(unit=i_sc,fmt="(a)") "! "
                do i=1,d%nobs
                    write(unit=i_sc,fmt="(3f14.4)") d%x(i),d%y(i),d%sw(i)
                end do
                close(unit=i_sc)
                cycle
             end if
         End Select

         c%icyc=250
         c%iw=0
         c%corrmax=50.0
         c%constr = .true.
         c%percent=50.0
         npeak=npeakx

         write(unit=i_out,fmt="(a,tr1,i5  )")" => Number of peaks :",npeak
         write(unit=i_out,fmt="(a,tr1,i5  )")" => Number of background points :",n_ba
         write(unit=i_out,fmt="(a,tr1,i5  )")" => Number of cycles:",c%icyc

         vs%np = ngl+ n_ba + 4 * npeak
         use_asymm=.false.
         use_hps=.false.

         !save input data
         j = 0
         do i=1,vs%np
          if (vs%code(i) /=0) j = j + 1
         end do
         c%npvar  = j    ! number of refined parameters

         write(unit=i_out,fmt="(/,a)")          " => Global parameters                 Flag"
         write(unit=i_out,fmt="(a,f14.6,i3)")   " => Kalph2/Kalph1 ratio :", vs%pv(1),vs%code(1)
         !write(unit=i_out,fmt="(a,f14.6,i3)")   " => Asymmetry-1(S_L)    :", vs%pv(2),vs%code(2)
         write(unit=i_out,fmt="(a,f14.6,i3)")   " => Asymmetry-2(D_L)    :", vs%pv(3),vs%code(3)
         write(unit=i_out,fmt="(/,a)")          "      Profile Parameters for Pseudo-Voigt"
         write(unit=i_out,fmt="(a)")            "        pV(x) = ETA* L(x) + (1-ETA)* G(x)"
         write(unit=i_out,fmt="(a)")            "    FWHM = SQRT((U tanT + V) tanT + W) + Z/cosT"
         !write(unit=i_out,fmt="(a,/)")          "    ETA  = Eta0 + X * 2T "
         !write(unit=i_out,fmt="(a,f14.6,i3)")   " => Parameter  U        :", vs%pv(4),vs%code(4)
         !write(unit=i_out,fmt="(a,f14.6,i3)")   " => Parameter  V        :", vs%pv(5),vs%code(5)
         write(unit=i_out,fmt="(a,f14.6,i3)")   " => Parameter  W        :", vs%pv(6),vs%code(6)
         !write(unit=i_out,fmt="(a,f14.6,i3)")   " => Parameter  Z        :", vs%pv(7),vs%code(7)
         !write(unit=i_out,fmt="(a,f14.6,i3)")   " => Parameter Eta0      :", vs%pv(8),vs%code(8)
         !write(unit=i_out,fmt="(a,f14.6,i3)")   " => Parameter  X        :", vs%pv(9),vs%code(9)
         write(unit=i_out,fmt="(/,a)")          " => Background parameters"
         write(unit=i_out,fmt="(a)")            "      Scatt. Variable     Background  Flag"
         do j=1,n_ba
           write(unit=i_out,fmt="(2f18.4,i4)")   bac_pos(j), vs%pv(j+ngl),vs%code(j+ngl)
         end do

         write(unit=i_out,fmt="(/,a,/)")                                                    &
              "      Position      Intensity      FWHM          D-Eta            Flags"
         j=ngl+n_ba+1

         DO i=1,npeak
           write(unit=i_out,fmt="(f14.6,f14.2,2F14.6,tr5,4i3)") vs%pv(j),vs%pv(j+1),vs%pv(j+2),vs%pv(j+3),   &
                                       vs%code(j),vs%code(j+1),vs%code(j+2),vs%code(j+3)
           j=j+4
         END DO
         call set_nampar(n_ba,npeak,vs)
         write(unit=i_out,fmt= "(/,a,i4,/)") " => Total number of refined parameters: ", c%npvar
         d%iw=0
         c%tol=epsilon(0.0_cp)
         !write(unit=*,fmt="(a,i6    )") " => Number of points: ",no
         !write(unit=*,fmt="(a,2f14.4)") " => Treating data in range: ",ain,afin
         !write(unit=*,fmt="(a,2f14.4)") " => Background in range: ",bac_pos(1),bac_pos(n_ba)

         ll=0
         do
           call Init_Prof_Val()
           !call Levenberg_Marquardt_Fit(powder_patt_der, d%nobs, c, Vs, chi2, .true., texte)
           !call ODR(powder_patt_odr, n=d%nobs, m=1, np=vs%np, nq=1, beta=vs%pv, Y=reshape(d%y,[d%nobs,1]), X=reshape(d%x,[d%nobs,1]), &
           !     ifixb=vs%code, we = reshape(ww,[d%nobs,1,1]), Lower=lower, Upper=upper, iprint=2122, lunerr=i_odr, lunrpt=i_odr, &
           !     Maxit=c%icyc)
           call ODR_LSQ(powder_patt_odr,d,vs,c,lower,upper,c_print,lun=i_odr )
           ll=ll+1
           j=ngl+n_ba+1
           redo=.false.
           do i=ngl+n_ba+1,vs%np
              if( vs%pv(j+1) <= 1.02*lower(j+1))  then
                vs%code(j:j+3) = 0
                vs%pv(j+1) = 0.0001
                redo=.true.
              end if
              j=j+4
           end do
           if(redo .and. ll < 10) cycle
           exit
         end do
         !call Marquardt_Fit(Sum_PV_Peaks,d,c,vs,i_out,chi2)
         !write(unit=*,fmt="(a)") " => "//texte(1:41)
         !write(unit=*,fmt="(a)") " => "//texte(41:133)
         !write(unit=*,fmt="(a)") " => "//trim(texte(133:))
         write(unit=testchi2,fmt="(a,i5,a,g15.5)") "   Scan: ",np," => Chi2:", chi2
         !write(unit=*,fmt=*) " => Function and Jacobian evaluations: ",c%nfev,c%njev
         !if(Err_CFML%Ierr /= 0) stop
         !call Info_LSQ_Output(Chi2,0.0,d%nobs,d%x,d%y,d%yc,ww,i_out,c,vs,"Analytical Levenberg-Marquardt")
         call Info_LSQ_Output(Chi2,0.0,d%nobs,d%x,d%y,d%yc,ww,i_out,c,vs,"ODR-LSQ")
         !call Info_LSQ_Output(Chi2,0.0,d%nobs,d%x,d%y,d%yc,ww,i_out,c,vs,"CURFIT")
         exclude=.false.
         j=ngl+n_ba+1
         Select Case(scan_type)
           Case("omega")
              if(npeak > 1) then
                inten=(vs%pv(j+1) + vs%pv(j+5))*norma
                sigma=sqrt(vs%spv(j+1)**2+vs%spv(j+5)**2)*norma*sqrt(chi2)
              else
                inten=vs%pv(j+1)*norma
                if(inten < 0.0) inten=0.1
                sigma=vs%spv(j+1)*norma*sqrt(chi2)
              end if
              write(unit=*,fmt="(a,3i4, 2f14.4)")    trim(testchi2)//"  hkl, Int, Sig: ",nint(hkl),Inten,sigma
              write(unit=i_hkl,fmt="(3i4,2f14.4,a)") nint(hkl),Inten,sigma,  "          # "//trim(testchi2)
              if(Inten > 300) then
                write(line,"(2(f12.2,a))") Inten,"(",sigma,")"
                line=pack_string(line)
                write(refl,"(a,3i4,a,f6.2)") "Reflection(",nint(hkl),")  Int:"//trim(line)//",  Gamma:",gamm
                call Output_Plot(d%nobs,d%x,d%y,d%yc,np,chi2,refl)
              end if
           Case("qscan")
              Inten=0.0; sigma=0
              do i=1,npeak
                hkl=h_ini
                hkl(jsc)=vs%pv(j)
                !fg=sqrt(vs%pv(6))
                !fwh=fg+vs%pv(j+2)
                fwh=vs%pv(j+2)
                sfwh=vs%spv(j+2)
                !sfwh=sqrt(0.25*vs%spv(6)*vs%spv(6)/vs%pv(6)+vs%spv(j+2)*vs%spv(j+2))
                write(unit=line,fmt="(a,3f9.4,6f12.4)") trim(testchi2)//"  hkl, Int, Sig, FWHM, Sig: ",&
                   hkl,vs%pv(j+1),vs%spv(j+1),fwh,sfwh
                write(unit=*,fmt="(a)")  trim(line)
                if(index(line,"NaN") == 0 .and. chi2 < 80.0) then
                  if(nkvec /= 0) then
                    write(unit=i_hkl,fmt="(I6.6,3f8.4,2f14.4,4f8.2,a)") Numor, hkl,vs%pv(j+1),vs%spv(j+1), 0.0,0.0,0.0,0.0,"          # "//trim(testchi2)
                  else
                    write(unit=i_hkl,fmt="(I6.6,3i4,2f14.4,4f8.2,a)") Numor,nint(hkl),vs%pv(j+1),vs%spv(j+1), 0.0,0.0,0.0,0.0,  "          # "//trim(testchi2)
                  end if
                  exclude=.false.
                else
                  !Output of the scan giving rise to problems
                  write(unit=line,fmt="(a,i6.6,a)") "NAN_",np,".xys"
                  open(unit=i_sc, file=trim(line),status="replace",action="write")
                  write(unit=i_sc,fmt="(a)") "Problems in refinement: "//trim(texte)
                  write(unit=i_sc,fmt="(a)") "! "
                  write(unit=i_sc,fmt="(a)") "! "
                  write(unit=i_sc,fmt="(a)") "! "
                  write(unit=i_sc,fmt="(a)") "! "
                  write(unit=i_sc,fmt="(a)") "! "
                  do k=1,d%nobs
                      write(unit=i_sc,fmt="(3f14.4)") d%x(k),d%y(k),d%sw(k)
                  end do
                  close(unit=i_sc)
                  exclude=.true.
                  exit
                end if
                Inten=Inten+vs%pv(j+1)
                sigma=sigma+vs%pv(j+1)**2
                j=j+4
              end do
              if(.not. exclude) then
                sigma=sqrt(sigma)
                write(line,"(2(f12.2,a))") Inten,"(",sigma,")"
                line=pack_string(line)
                write(refl,"(a,3f6.2,a,3f6.2,a)") "Scan(",h_ini,"->", h_fin,")  Total Intensity: "//trim(line)
                call Output_Plot(d%nobs,d%x,d%y,d%yc,np,chi2,refl)
              end if
         End Select


      end if

    End Do  !icont
    call cpu_time(timf)
    write(unit=*,fmt="(a,f10.3,a)") " => CPU-time:", timf-timi," seconds"

 End Program SXTAL_fit