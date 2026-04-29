  Module Input_output_data_mod
    use CFML_GlobalDeps,   only: Err_CFML,cp
    use rL_kvInt_Mod
    use CFML_Optimization_LSQ
    use CFML_Strings,      only : pack_string,u_case,number_lines
    use CFML_BckPeaks,     only : peak_search_cond_type, pkb_cond
    use CFML_gSpaceGroups, only : Spg_Type, Set_SpaceGroup


    implicit none
    private
    !Public procedures
    public  :: get_texte, output_plot, read_CFL
    ! Global variables
    real(kind=cp),                 public :: fwh0_f=0.03,fwh1_f=0.00,fwh2_f=0.00, eta0_f=0.0,eta1_f=0.0, &
                                             fwh0_s=0.04,fwh1_s=0.00,fwh2_s=0.00, eta0_s=0.0,eta1_s=0.0, w_f=1.2, w_s=1.5, &
                                             zero0,zero1,zero2, highchi=25.0

    real(kind=cp), dimension(6),   public :: cell
    logical,                       public :: cell_given=.false., kvec_given=.false.,sigma_given=.false., zero_given=.false., &
                                             spg_given=.false., scan_range=.false.,fwgf_given=.false.,fwgs_given=.false.,plot_ini=.false., &
                                             apply_cut=.false., rewrite=.false.
    real(kind=cp), dimension(3),   public :: h_ini, h_fin
    character(len=2), dimension(3),public :: scan_along=["a*","b*","c*"]
    type(Spg_Type),                public :: SpG
    real(kind=cp), dimension(Max_Free_par),public :: lower, upper
    integer,                       public :: c_print
    character(len=20),             public :: spg_symb,algor="CURFIT",mode="MULTI"
    integer,                       public :: scan_ini=0, scan_fin=0


    contains


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

    Subroutine Output_Plot(nobs,xx,fobs,fcalc,nref,chi2,refl,ini)
      integer,                     intent(in) :: nobs, nref
      real,dimension(:),           intent(in) :: xx,fobs,fcalc
      real,                        intent(in) :: chi2
      character (len=*),           intent(in) :: refl
      logical, optional,           intent(in) :: ini
      ! Local variables
      integer                          :: i,j,ifinal,i_plot=22
      real                             :: shb,shd,dif,yma,ymi,pos,posb, bgr
      real                             :: iposr
      character (len=90)  :: refplot


      yma= -1.E9   !
      ymi=  1.E9   !
      Do i=1,nobs
        if(fobs(i) > yma ) yma =fobs(i)
        if(fobs(i) < ymi ) ymi =fobs(i)
      End Do
      if(present(ini)) then
         write(refplot,"(a,i6.6)") "ini_Scan_",nref
      else
         write(refplot,"(a,i6.6)") "Scan_",nref
      end if
      Open(Unit=i_plot,File=trim(refplot)//".xrf",status="replace",action="write")

      shb   = 0.0                         ! idem wpl_pfit.f90
      shd   = ymi - 0.2*(yma-ymi)
      iposr = ymi - 0.1*(yma-ymi)

      !write(unit=i_plot,fmt="(2A)") " ",TRIM(title)
      write(unit=i_plot,fmt="(a,f8.2)") trim(refl)//"  Chi2=",chi2
      write(unit=i_plot,fmt="(2a)")      " => Data file name: ",TRIM(filedat)
      write(unit=i_plot,fmt="(a)")       " => Scattering Variable:  r.l.u."
      write(unit=i_plot,fmt="(a,2f9.5)") " => Lambda(1&2)   : ", 1.0,1.0
      write(unit=i_plot,fmt="(a,I9)")    " => Numb.of.points: ", nobs
      write(unit=i_plot,fmt="(a,i9)")    " => Numb.of.peaks : ", npeakx
      write(unit=i_plot,fmt="(a)")   &
      " Scan Along "//scan_along(jsc)//"  Yobs     Ycalc  Yobs-Ycal    Backg   PeakPos     Posr  Intensity   FWHM    Eta"

      IF(npeakx > nobs) THEN
        ifinal=nobs
      ELSE
        ifinal=npeakx
      END IF
      j=ngl+n_ba+1
      Do  i=1,ifinal
        pos=xx(i)
        dif=fobs(i)-fcalc(i)+shd
        posb=corr_pp(i)
        call Back_Chebychev(pos,x_ini,x_fin,n_ba,vs%pv(ngl+1:ngl+n_ba),bgr)
        if(satellite(i)) then
           write(unit=i_plot,fmt="(f10.4,4f10.2,f10.4,f10.2,tr1,f10.2,2f10.4)")  &
             xx(i),fobs(i),fcalc(i), dif, bgr-shb, posb,iposr, vs%pv(j+1), fwhms(i), etas(i)
        else
           write(unit=i_plot,fmt="(f10.4,4f10.2,f10.4,f10.2,tr1,f10.2,2f10.4)")  &
             xx(i),fobs(i),fcalc(i), dif, bgr-shb, posb,iposr, vs%pv(j+1), fwhmf(i), etaf(i)
        end if
        j=j+4
      End Do

      If(npeakx+1 < nobs) THEN
        Do  i=npeakx+1,nobs
          pos=xx(i)
          dif=fobs(i)-fcalc(i)+shd       ! shd < 0.
          call Back_Chebychev(pos,x_ini,x_fin,n_ba,vs%pv(ngl+1:ngl+n_ba),bgr)
          write(unit=i_plot,fmt="(f10.4,4f10.2)") pos,fobs(i),fcalc(i),dif,bgr-shb
        End Do
      End If
      close (unit=i_plot)
    End Subroutine Output_Plot

    Subroutine Read_CFL(cfl_file)
      character(Len=*), intent(in) :: cfl_file
      !-----
      integer                          :: i,j,k, i_cfl, ier, nlines,istart,istartpar
      character(len=20)                :: keyw,forma
      character(Len=180)               :: texte
      character(Len=180),dimension(:),allocatable :: line
      open(newunit=i_cfl,file=cfl_file,status="old", action="read", position="rewind",iostat=ier)
      if(ier /= 0) then
        write(unit=*,fmt="(a)") " => Error opening the file: "//trim(cfl_file)
        stop
      end if
      nlines = Number_Lines(cfl_file)
      allocate(line(nlines))
      filecode=" "
      spg_symb=" "
      c_print=0
      nlines=0
      nkvec=0
      do
         read(unit=i_cfl,fmt="(a)",iostat=ier) texte
         if(ier == 0) then
            nlines=nlines+1
            texte=adjustl(texte)
            line(nlines) = texte
            i=index(texte," ")
            keyw=u_case(texte(1:i-1))
            if(trim(keyw) == "KVEC") nkvec=nkvec+1
            if(trim(keyw) == "NUMPEAKS")  then
                mode="SINGLE"
                read(texte(9:),*) npeakx
                istart=nlines
            end if
            if(trim(keyw) == "NUMPAR")  then
                mode="SINGLE"
                read(texte(9:),*) vs%np
                istartpar=nlines
            end if
         else
            exit
         end if
      end do
      close(unit=i_cfl)
      if(mode == "MULTI") then
        istart=nlines
      end if
      if(allocated(kvec)) deallocate(kvec)
      allocate(kvec(3,nkvec), nharm(nkvec))
      highchi=25.0
      nkvec=0

      do j=1,istart
         texte=line(j)
         i=index(texte," ")
         keyw=u_case(texte(1:i-1))

         Select Case(trim(keyw))

           Case("ALGOR")
              algor=adjustl(u_case(texte(6:)))

           Case("PLOTINI")
              plot_ini=.true.

           Case("REWRITE")
              rewrite=.true.

           Case("CUT")
              apply_cut=.true.

           Case("SCANS")
              read(texte(6:),*,iostat=ier)  scan_ini, scan_fin
              if(ier /= 0) then
                scan_ini=0
                scan_fin=0
                scan_range=.false.
              else
                scan_range=.true.
              end if

           Case("THRESHOLD")
              read(texte(10:),*,iostat=ier)  threshold
              if(ier /= 0) threshold=3.0

           Case("HIGHCHI")
              read(texte(8:),*,iostat=ier)  highchi
              if(ier /= 0) highchi=25.0

           Case("CELL")
              read(texte(5:),*,iostat=ier)  cell
              if(ier == 0) cell_given=.true.

           Case("POLY")
              read(texte(5:),*,iostat=ier)  n_ba
              if(ier /= 0) n_ba=5

           Case("SPGR")
              Spg_symb = adjustl(texte(5:))

           Case("CPRINT")
              read(texte(7:),*,iostat=ier)  c_print
              if(ier /= 0) c_print=0

           Case("KVEC")
              nkvec=nkvec+1
              read(texte(5:),*,iostat=ier)  kvec(:,nkvec), nharm(nkvec)
              if(ier == 0) kvec_given=.true.

           Case("ZERO")
              read(texte(8:),*,iostat=ier)  zero0, zero1, zero2
              if(ier /= 0) then
                  zero0=0.0
                  zero1=0.0
                  zero2=0.0
              end if
              zero_given=.true.

           Case("FWHETAF")
              read(texte(8:),*,iostat=ier)  fwh0_f,fwh1_f,fwh2_f, eta0_f, eta1_f, w_f
              if(ier /= 0) then
                  fwh0_f=0.03
                  fwh1_f=0.03
                  fwh2_f=0.03
                  eta0_f=0.00
                  eta1_f=0.00
                    w_f=1.2
              end if
              fwgf_given=.true.

           Case("FWHETAS")
              read(texte(8:),*,iostat=ier)  fwh0_s,fwh1_s,fwh2_s, eta0_s, eta1_s, w_s
              if(ier /= 0) then
                  fwh0_s=0.03
                  fwh1_s=0.03
                  fwh2_s=0.03
                  eta0_s=0.00
                  eta1_s=0.00
                    w_s=1.5
              end if
              fwgs_given=.true.

           Case("FILESCAN")
              filecode= adjustl(texte(9:))
              i=index(filecode,".qsc")
              if(i /= 0) then
                filecode=filecode(1:i-1)
                filedat=trim(filecode)//".qsc"
              end if

         End Select

      end do

      if(len_trim(spg_symb) > 2) then
         spg_given=.true.
         call Set_SpaceGroup(spg_symb,SpG)
         if(Err_CFML%Ierr /= 0) spg_given=.false.
      end if

      if(mode == "SINGLE") then
        if(allocated(peak_pos)) deallocate(peak_pos)
        if(allocated(hkl)) deallocate(hkl)
        if(allocated(satellite)) deallocate(satellite)
        allocate(satellite(npeakx))
        allocate(peak_pos(npeakx))
        call Allocate_Globals(npeakx)
        Select Case(nkvec)
          Case(0)
              allocate(hkl(3,npeakx))
              forma="(i6,tr2,3i4,7f14.6)"
          Case(1)
              allocate(hkl(4,npeakx))
              forma="(i6,tr2,4i4,7f14.6)"
          Case(2)
              allocate(hkl(5,npeakx))
              forma="(i6,tr2,5i4,7f14.6)"
          Case(3)
              allocate(hkl(6,npeakx))
              forma="(i6,tr2,6i4,7f14.6)"
        End Select
        j=ngl+n_ba+1
        do i=1,npeakx
          read(line(istart+i),*) k, hkl(:,i), peak_pos(i), Intens(i),vs%spv(j+1), fwhm(i), sfwhm(i), eta(i), seta(i),satellite(i)
          j=j+4
        end do
        do i=1,vs%np
          read(line(istartpar+i),*) k, vs%nampar(i),vs%pv(i),vs%spv(i),vs%code(i)
        end do
        !Setting upper and lower limits
        corr_pp=peak_pos
        upper(1:vs%np) =  9.9e+35
        lower(1:vs%np) = -9.9e+35
        upper(1)=  0.03  !-zero
        lower(1)= -0.03
        upper(4)= 0.200
        lower(4)= 0.002
        upper(7)= 0.400
        lower(7)= 0.002
        upper(10)=  1.0   !Etaf-zero
        lower(10)=  0.0
        upper(12)=  1.0   !Etas-zero
        lower(12)=  0.0
        j=ngl+n_ba+1
        do i=1,npeakx
          lower(j)=-0.1
          upper(j)= 0.1
          lower(j+1)= 0.001
          lower(j+2)=-0.4
          upper(j+2)= 0.4
          lower(j+3)=-1.00
          upper(j+3)= 1.0
          j=j+4
        end do
      end if

    End Subroutine Read_CFL

  End Module Input_output_data_mod

  Module LSQ_management
    use CFML_GlobalDeps,   only : cp, Err_CFML
    use CFML_BckPeaks
    use Input_output_data_mod
    use CFML_Optimization_LSQ, only: LSQ_State_Vector_type
    use rL_kvInt_Mod,      only : ngl,n_ba, NPEAKX, vs, c, d, Sum_PV_Peaks, nkvec,kvec, jsc, peak_pos,Intens, &
                                  fwhms,fwhmf,etas,etaf, satellite, corr_pp,x_ini,x_fin,sigma_back, weak_dyn, &
                                  set_weakness, threshold
    use CFML_Reflections,  only : h_absent
    Use CFML_Maths,        only : zbelong,epss,Set_Eps_Math,Locate

    implicit none
    public
    type(LSQ_State_Vector_type), save :: saved_vs
    real(kind=cp),               save :: saved_chi

    contains

     Subroutine set_initial_conditions(nscan)
       integer, intent(in)  :: nscan
       !--- Local Variables ---!
       integer              :: i,j,k,n
       real(kind=cp)        :: pos, averb, max_int, resid, ch, min_int
       character(len=132)   :: refl

       lower= -1.0e+36; upper=1.0e+36

       vs%pv(:)=0.0; vs%code(:)=0
       if(zero_given) then
          vs%pv(1)= zero0
          vs%pv(2)= zero1
          vs%pv(3)= zero2
       end if
       vs%code(1)= 1
       vs%code(2)= 0

       if(fwgf_given) then
          vs%pv(4) = fwh0_f  !Assignment of global g0f_HPv in r.l.u.
          vs%pv(5) = fwh1_f  !Assignment of global g1f_HPv in r.l.u./pos
          vs%pv(6) = fwh2_f  !Assignment of global g2f_HPv in r.l.u./pos^2
          vs%pv(10)= eta0_f  !Assignment of global g0f_EtaPV
          vs%pv(11)= eta1_f  !Assignment of global g1f_EtaPV
       else
          vs%pv(4) = 0.03  !Assignment of global g0_HPv in r.l.u.
          vs%pv(5) = 0.00  !Assignment of global g1_HPv in r.l.u.
          vs%pv(6) = 0.00  !Assignment of global g2_HPv in r.l.u.
          vs%pv(10)= 0.00  !Assignment of global g0f_EtaPV
          vs%pv(11)= 0.00  !Assignment of global g1f_EtaPV
       end if

       if(fwgs_given) then
          vs%pv(7) = fwh0_s  !Assignment of global g0f_HPv in r.l.u.
          vs%pv(8) = fwh1_s  !Assignment of global g1f_HPv in r.l.u./pos
          vs%pv(9) = fwh2_s  !Assignment of global g2f_HPv in r.l.u./pos^2
          vs%pv(12)= eta0_s  !Assignment of global g0f_EtaPV
          vs%pv(13)= eta1_s  !Assignment of global g1f_EtaPV
       else
          vs%pv(7) = 0.03  !Assignment of global g0_HPv in r.l.u.
          vs%pv(8) = 0.00  !Assignment of global g1_HPv in r.l.u.
          vs%pv(9) = 0.00  !Assignment of global g2_HPv in r.l.u.
          vs%pv(12)= 0.00  !Assignment of global g0f_EtaPV
          vs%pv(13)= 0.00  !Assignment of global g1f_EtaPV
       end if
       vs%code(4)  = 1
       if(nkvec > 0) vs%code(7)  = 1
       vs%code(10) = 1
       if(nkvec > 0) vs%code(12) = 1

       upper(1)=  0.03  !-zero
       lower(1)= -0.03
       upper(4)= 0.200
       lower(4)= 0.002
       upper(7)= 0.400
       lower(7)= 0.002
       upper(10)=  1.0   !Etaf-zero
       lower(10)=  0.0
       upper(12)=  1.0   !Etas-zero
       lower(12)=  0.0

       !Propagation vectors
       j=13
       !write(*,*) "Assignment of propagation vector parameters", jsc
       do i=1,nkvec
         do k=1,3
            j=j+1
            if(k == jsc) then
               vs%pv(j)=kvec(jsc,i)
               !vs%code(j)=1
               upper(j)=(1+0.09)*vs%pv(j)
               lower(j)=(1-0.09)*vs%pv(j)
            end if
         end do
       end do

       !Estimation of the constant term of background
       max_int=maxval(d%y); min_int=minval(d%y)
       max_int=min_int+0.05*(max_int-min_int)
       n=0
       averb=0.0
       do i=1,d%nobs
         if(d%y(i) < max_int) then
            n=n+1
            averb=averb+d%y(i)
         end if
       end do
       averb=averb/real(n)

       j=ngl+1  ! ngl: number of global parameters
       vs%pv(j)=averb
       vs%code(j)=1
       do j=2,n_ba
         vs%pv(j+ngl)=0.1/real(j)
         vs%code(j+ngl)=1
       end do

       j=ngl+n_ba+1
       do i=1,NPEAKX
         pos = peak_pos(i) !They have been calculated in a previous call to Gen_Peaks
         k=Locate(d%x,pos,d%nobs)
         vs%pv(j+1)= max(0.1*(d%y(k)-averb),1.5)           !Intensity, a factor 10 lower than peak intensity
         vs%code(j+1)=1 !All intensities are always refined in the first pass
         lower(j) =-0.2
         upper(j) = 0.2
         lower(j+1)= 0.001
         lower(j+2) =-0.2
         upper(j+2) = 0.2
         lower(j+3) =-1.0
         upper(j+3) = 1.0
         j=j+4
       end do

       do i=1, vs%np
         if(vs%pv(i) < lower(i)) then
            vs%pv(i)=lower(i)+0.0001
         else if(vs%pv(i) > upper(i)) then
            vs%pv(i)=upper(i)-0.0001
         end if
       end do

     End Subroutine set_initial_conditions

     Subroutine Test_Initial_Parameters(nscan,weight,ch)
        integer,      intent(in) :: nscan
        logical,      intent(in) :: weight
        real(kind=cp),intent(out):: ch
        real(kind=cp) :: resid
        integer       :: i
        write(*,"(a)") " => Calculation of the Chi2 for initial parameters:"
        ch=0.0
        do i=1,d%nobs
          call Sum_PV_Peaks(i,d%x(i),d%yc(i),vs)
          if(weight) then
            resid= d%sw(i)*(d%y(i)-d%yc(i))**2  !here d%sw is weight
            ch=ch+resid
          else
            resid= (d%y(i)-d%yc(i))/d%sw(i) !here d%sw is sigma
            ch=ch+resid*resid
          end if
        end do
        ch=ch/real(d%nobs-vs%np)
        write(*,"(a,g14.5,a,i4)") " => Initial Chi2: ",ch, " for scan #",nscan
     End Subroutine Test_Initial_Parameters


     Subroutine manage_LSQ(L,chi,redo,bad)
        integer,              intent(in out) :: L
        real(kind=cp),        intent(in)     :: chi
        logical,              intent(in out) :: redo
        integer,dimension(:), intent(in)     :: bad
        !
        integer :: i,j
        logical :: isbad
        !Check weakness of reflections
        call set_weakness()
        i=sum(bad)
        if(i /= 0) then
           isbad=.true.
        else
           isbad=.false.
        end if
        redo=.false.
        !Applying constraints in eta and FWHM for all cases

        if(vs%pv(4) < 0) vs%pv(4)=-vs%pv(4)
        if(vs%pv(7) < 0) vs%pv(4)=-vs%pv(7)

        if(vs%pv(10) < 0.0)then
          vs%pv(10)=0.0  !Eta parameters
          vs%code(10) = 0
        end if
        if(vs%pv(10) >  1.0) then
          vs%pv(10)= 1.0
          vs%code(10) = 0
        end if
        if(vs%pv(12) < 0.0) then
          vs%pv(12)= 0.0
          vs%code(12) = 0
        end if
        if(vs%pv(12) >  1.0) then
          vs%pv(12)= 1.0
          vs%code(12) = 0
        end if
        j=ngl+n_ba+1
        do i=1,npeakx  !Constraining simultaneously the intensity and fwhm to be positive
          if(satellite(i)) then
            if(vs%pv(j+1) < 0.0) then
               if(fwhms(i) < 0.0) then
                 vs%pv(j+1) = -vs%pv(j+1)
                 !vs%pv(j+2) = 0.0
                 !vs%code(j+2) = 0
               else
                 vs%pv(j+1) = 0.01
                 vs%pv(j+2) = 0.0
                 vs%code(j:j+3) = 0
               end if
            end if
          else
            if(vs%pv(j+1) < 0.0) then
               if(fwhmf(i) < 0.0) then
                 vs%pv(j+1) = -vs%pv(j+1)
                 !vs%pv(j+2) = 0.0
                 !vs%code(j+2) = 0
               else
                 vs%pv(j+1) = 0.01
                 vs%pv(j+2) = 0.0
                 vs%code(j:j+3) = 0
               end if
            end if
         end if
          j=j+4
        end do
        if( L == 1) then
           if(isbad) return  !This means the program crashed at the first trial,
                             !so no action is needed ... accept the wrong choice because redo is false
           saved_vs=vs       !Value of parameters after the first trial
           saved_chi=chi
           vs%code(1)=0   !Fix zeroes and try to refine local shifts
           vs%code(2)=0   ! Zero1    Linear variation of the zero-shift
           vs%code(4)=1   ! g0f_Hpv  global variation of FWHM
           !vs%code(5)=1  ! g1f_Hpv
           if(nkvec> 0) vs%code(7)=1   ! g0s_Hpv
           !vs%code(8)=1  ! g1s_Hpv  Linear Variation of FWHM

           j=ngl+n_ba+1
           do i=1,npeakx        !Refine shift positions and put negative intensities positive
              vs%code(j+1) =1   !All intensities are now refined
              if(vs%pv(j+1) < 0.0) then
                 vs%pv(j+1) = -vs%pv(j+1)
              end if
              if(weak_dyn(i)) then
                vs%code(j)  =0  !sh-pos
              else
                vs%code(j)=1    !sh-pos
                vs%pv(j)=0.001 !Put a small number to avoid instabilities
              end if
              j=j+4
           end do
           !Apply box constraints
           redo=.true.
           !j=13
           !write(*,*) "Assignment of propagation vector parameters", jsc
           !do i=1,nkvec
           !  do k=1,3
           !     j=j+1
           !     if(k == jsc) then
           !        vs%code(j)=1
           !        redo=.true.
           !     end if
           !  end do
           !end do
        end if

        if(L == 2) then
            !Fix global variation of FWHMs during local refinement of sh-fwhm
           vs%code(4)=0   ! g0f_Hpv  global variation of FWHM
           if(nkvec > 0) vs%code(7)=0   ! g0s_Hpv
           if(isbad .or. chi > saved_chi) then
             j=ngl+n_ba+1
             do i=1,npeakx
                if(abs(vs%pv(j)-saved_vs%pv(j)) > 0.1  )  then
                   vs%pv(j)=saved_vs%pv(j)
                   vs%code(j)=0
                end if
                if(bad(i) == 1) then
                  vs%code(j)=0
                  if(vs%pv(j+1) < 0.1) then
                     vs%pv(j+1)=0.01
                     vs%code(j+1)=0
                  end if
                else
                  if(weak_dyn(i)) then
                    vs%code(j+2)=0  !sh-fwhm
                    vs%code(j)=0
                  else
                    vs%code(j+2)=1  !sh-fwhm  !refining the local fwhm
                    vs%code(j)  =0
                  end if
                end if
                j=j+4
             end do
           else
             j=ngl+n_ba+1
             do i=1,npeakx
                if(abs(vs%pv(j)-saved_vs%pv(j)) > 0.1  )  then
                   vs%pv(j)=saved_vs%pv(j)
                   vs%code(j)=0
                end if
                vs%code(j)  =0  !fixing sh-pos
                if(weak_dyn(i)) then
                  vs%code(j+2)=0  !sh-fwhm
                  vs%code(j)=0
                else
                  vs%code(j)=1    !sh-pos  !refining Sh-pos
                  vs%code(j+2)=1  !sh-fwhm  !refining the local fwhm
                end if
                j=j+4
             end do
             vs%code(1)=1  !retake the zero-shift
             vs%code(4:9)=0 !Fixing global FWHM
             saved_vs=vs
             saved_chi=chi
           end if
           redo=.true.
        end if

        if(L == 3) then
           if(isbad) then
             j=ngl+n_ba+1
             do i=1,npeakx
                if(bad(i) == 1) then
                  vs%pv(j)=saved_vs%pv(j)     !Value of parameters after the first trial
                  vs%pv(j+2)=saved_vs%pv(j+2)
                  vs%pv(j+3)=saved_vs%pv(j+3)
                  vs%code(j) = 0
                  vs%code(j+2:j+3)=saved_vs%code(j+2:j+3)
                end if
                j=j+4
             end do

           else

             saved_vs=vs
             saved_chi=chi
             j=ngl+n_ba+1
             do i=1,npeakx
                if(satellite(i)) then
                   if(fwhms(i) < 0.0) then
                     vs%pv(j+2) = 0.0
                     vs%code(j+2) = 0
                   end if
                else
                   if(fwhmf(i) < 0.0) then
                     vs%pv(j+2) = 0.0
                     vs%code(j+2) = 0
                   end if
                end if
                j=j+4
             end do
           end if
           redo=.true.
        end if

        if(L == 4) then
           if(isbad) then
             j=ngl+n_ba+1
             do i=1,npeakx
                if(bad(i) == 1) then
                  vs%pv(j)=saved_vs%pv(j)     !Value of parameters after the first trial
                  vs%pv(j+2)=saved_vs%pv(j+2)
                  vs%pv(j+3)=saved_vs%pv(j+3)
                  vs%code(j:j+3)=saved_vs%code(j:j+3)
                  if( vs%pv(j+1) <= 0.1) then  !Negative intensites fixed to 0.01
                     vs%pv(j+1) = 0.01 !This put a positive number of the final intensity conserving the
                     vs%code(j:j+3) = 0
                  end if
                end if
                j=j+4
             end do
             redo=.true.
           else
             j=ngl+n_ba+1
             do i=1,npeakx
                if( vs%pv(j+1) <= 0.1) then  !Negative intensites fixed to 0.01
                   vs%pv(j+1) = 0.01 !This put a positive number of the final intensity conserving the
                   vs%code(j:j+3)=0
                   if(vs%spv(j+1) <= 0.01 ) then   !sigma obtained by LSQ
                     vs%spv(j+1)= Sigma_Back(corr_pp(i),x_ini,x_fin,n_ba,vs%spv(ngl+1:ngl+n_ba)) / threshold
                   end if
                end if
                j=j+4
             end do
             redo=.false.
           end if
        end if
        if(L == 5) then
          redo=.false.
        end if
        if(algor == "LEVENBERG-MARQUARDT" .or. algor == "CURFIT") then
           j = 0
           do i=1,vs%np
            if (vs%code(i) /= 0) j = j + 1
           end do
           c%npvar = j    ! number of refined parameters
        end if
     End Subroutine manage_LSQ

     Subroutine Write_Initial_Conditions(L)
        integer, intent(in) :: L
        integer :: i
        write(*,"(a,i3)") " Initial parameters for trial #",L
        do i=1,vs%np
           write(*,"(i4,a,f14.4,i3)") i, "  "//vs%nampar(i),vs%pv(i),vs%code(i)
        end do
     End Subroutine Write_Initial_Conditions

  End Module LSQ_management

  !-----------------------------------------------------------------------------------
  !-----------------------------------------------------------------------------------
  !     Program to fit single crystal scans
  !     Version 0.1  J.Rodriguez-Carvajal (October-2021)
  !     The Program Fits Selected Profile Zones of reciprocal space
  !     Orthogonal Distance Regression Fitting Procedure with Analytical Derivatives
  !-----------------------------------------------------------------------------------
  !-----------------------------------------------------------------------------------
  Program rl_Int1D
    use CFML_GlobalDeps, only: cp, Err_CFML
    use CFML_Optimization_LSQ
    Use CFML_Maths,        only : zbelong,Set_Eps_Math,Locate
    use CFML_Strings,      only : pack_string, Number_Lines
    use rL_kvInt_Mod
    use LSQ_management
    use Input_output_data_mod
    use ODR_wrapper

    Implicit None

    Integer                    :: ier,no,np,i,j,k,L,narg, numor, ichi,hchi, i_nuc
    Integer                    :: lun=1, i_out=7, i_hkl=8, i_odr=99, i_sc=10
    Logical                    :: redo, exclude, weig, cut
    Real(kind=cp)              :: timi,timf,aver, pos, fwh,sfwh,et,set
    real(kind=cp)              :: inten, sigma,aux1,aux2,aux3,chir
    real(kind=cp),dimension(3) :: rhkl
    real(kind=cp),dimension(10):: ch

    Character (Len=512)                     :: texte,line
    Character (Len=130)                     :: refl,testchi2,highchi_file, cfl_code
    integer,      dimension(:), allocatable :: bad
    integer,      dimension(50)             :: high_chi
    real(kind=cp),dimension(50)             :: vhigh_chi
    real(kind=cp),dimension(:), allocatable :: ww
    Character (Len=80),  dimension(:), allocatable :: profile
    Character (Len=256), dimension(:), allocatable :: lines

    !---- Arguments on the command line ----!
    narg=COMMAND_ARGUMENT_COUNT()
    if(narg > 0) then
          call GET_COMMAND_ARGUMENT(1,texte)
          call Read_CFL(texte)
          i=index(texte,".",back=.true.)
          if(i /= 0) then
            cfl_code=texte(1:i-1)
          else
            cfl_code="output"
          end if
    else
          write(unit=*,fmt="(a,a)") " => The program should be invoked with an argument like MyCFL_File.cfl"
          stop
    end if
    !   Header and openning of files

    write(unit=*,fmt="(a)")" "
    write(unit=*,fmt="(a)")"            ------------------------------------ "
    write(unit=*,fmt="(a)")"                --- PROGRAM: rL_Int1D ---"
    write(unit=*,fmt="(a)")"            (Author: J. Rodriguez-Carvajal, ILL)"
    write(unit=*,fmt="(a)")"                   (Version November 2021) "
    write(unit=*,fmt="(a)")"            ------------------------------------ "
    write(unit=*,fmt="(a)") " "

    write(*,*) " => Opening the file: "//trim(filedat)
    write(*,*) " => Code of the file: "//trim(filecode)

    Open(Unit=lun,File=Trim(Filedat),STATUS="old",iostat=ier,action="read",position="rewind")
    if( ier /= 0 ) then
      write(unit=*,fmt="(a)") " => Error openning "//trim(filedat)
      Stop
    end if
    no=Number_Lines(Filedat)
    allocate(profile(no))
    Open(Unit=i_out, File=trim(filecode)//".out",status="replace",action="write")
    if(mode== "MULTI") then
        Open(Unit=i_hkl, File=trim(filecode)//".hkl",status="replace",action="write")
        Open(newUnit=i_nuc, File=trim(filecode)//"_nuc.hkl",status="replace",action="write")
    else
        Open(Unit=i_hkl, File=trim(cfl_code)//".hkl",status="replace",action="write")
    end if
    if(algor=="ODR") then
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
    end if
    !-------------------Start Program
    c%percent=50.0
    write(unit=i_out,fmt="(a)")"            ------------------------------------ "
    write(unit=i_out,fmt="(a)")"                  --- PROGRAM: rL_Int1D ---"
    write(unit=i_out,fmt="(a)")"            (Author: J. Rodriguez-Carvajal, ILL)"
    write(unit=i_out,fmt="(a)")"                   (Version November 2021) "
    write(unit=i_out,fmt="(a)")"            ------------------------------------ "

    !      Read the input control file and write in output files
    call Set_Eps_Math(0.005)
    np=0; L=0
    title="SXTAL integration for reflections in file: "//Trim(Filedat)
    call cpu_time(timi)
    !Writing the read data of the CFL file:
    write(unit=i_out,fmt="(a)") " => Title: "//title
    if(cell_given) then
      Write(unit=i_out,fmt="(a,6f10.4)") " => Unit cell parameters: ",cell
    end if
    if(spg_given) then
      Write(unit=i_out,fmt="(a)") " => Space Group: "//Spg_symb
    end if
    if(kvec_given) then
      Write(unit=i_out,fmt="(a)") " => Propagation vector(s): "
      do i=1,nkvec
        write(unit=i_out,fmt="(i4,a,3f8.4,a,i3)") i," kvec: [",kvec(:,i)," ]  Max-harmonic: ", nharm(i)
      end do
    end if
    ichi=0

    Do ! repeat for all qscans in the file !

      read(unit=lun,fmt="(a)",iostat=ier) texte
      if(ier /= 0) exit

      if(texte(1:5) == "QSCAN") then
         np=np+1
         if(scan_range) then
            if(.not. (np >= scan_ini .and. np <= scan_fin) ) cycle
         end if
         read(unit=texte(6:),fmt=*) h_ini,h_fin,numor
         rhkl=abs(h_fin-h_ini)
         do i=1,3
           if(rhkl(i) > 0.001) then
             jsc=i
             exit
           end if
         end do
         write(unit=i_out,fmt="(/,a,i6,a)") " =>     Qscan:#",np," along the "//scan_along(jsc)
         write(unit=i_out,fmt="(a,3f10.4,a)") "    hkl- init:",h_ini," (it may be cut by the program)"
         write(unit=i_out,fmt="(a,3f10.4,a)") "    hkl-final:",h_fin," (it may be cut by the program)"
         write(unit=i_out,fmt="(a,i7)")     "        Numor:",Numor
         no=0
         profile=" "
         do
            read(lun,"(a)",iostat=ier) line
            if(ier /= 0)  exit
            if(line(1:5) == "QSCAN") then
              no=no-1
              backspace(unit=lun)
              exit
            end if
            read(line,*,iostat=ier) aux1,aux2,aux3
            if(ier /= 0) then
              read(line,*,iostat=ier) aux1,aux2
              sigma_given=.false.
            else
              sigma_given=.true.
            end if
            if(abs(aux2) < 1.0) cycle !eliminate zero-intensity points
            no=no+1
            profile(no)=line(1:80)
         end do
         line=" "
         if(no < 10) cycle
         if (Allocated(d%x ))   deallocate (d%x)
         if (Allocated(d%sw))   deallocate (d%sw)
         if (Allocated(d%y ))   deallocate (d%y)
         allocate ( d%x(no),d%sw(no),d%y(no) )
         d%nobs=no
         ww=0.0
         if(sigma_given) then
            do i=1,d%nobs
               read(unit=profile(i),fmt=*) d%x(i),d%y(i),d%sw(i)
            end do
         else
            do i=1,d%nobs
               read(unit=profile(i),fmt=*) d%x(i),d%y(i)
               d%sw(i)=sqrt(d%y(i))
            end do
         end if

         if(apply_cut) then
           call Cut_bad_borders(no) !Cut bad borders and re-allocate the scan with the proper number of points
           d%nobs=no                !no is modified in Cut_bad_borders
           h_ini(jsc) = d%x(1)
           h_fin(jsc) = d%x(d%nobs)
           write(unit=i_out,fmt="(a,3f10.4)") "    Final hkl- init:",h_ini
           write(unit=i_out,fmt="(a,3f10.4)") "    Final hkl-final:",h_fin
         end if

         if (Allocated(d%yc))   deallocate (d%yc)
         if (Allocated(ww))     deallocate (ww)
         allocate ( d%yc(no),ww(no) )
         do i=1,d%nobs
           if(abs(d%sw(i)) > 0.001) ww(i)=1.0/(d%sw(i)*d%sw(i))
         end do

         x_ini=d%x(1)
         x_fin=d%x(d%nobs)

         if(mode /= "SINGLE") then
            !Setting initial conditions
            vs%pv(1:ngl)=0.0
            vs%code(1:ngl)=0

            !Generate peak positions (peak_pos) and indices for the current scan
            call Gen_Peaks(h_ini,h_fin,jsc,nkvec,kvec,nharm,npeakx,hkl,peak_pos,satellite)
            vs%np = ngl+ n_ba + 4 * npeakx
            call set_nampar(n_ba,npeakx,vs)
            !Allocate globals
            call Allocate_Globals(npeakx)
            !Set initial conditions for the scan "np"
            call set_initial_conditions(np)
         end if

         c%icyc=250
         c%iw=0
         c%corrmax=50.0
         c%constr = .false.
         c%percent=50.0

         write(unit=i_out,fmt="(a,tr1,i5  )")" => Number of peaks :",npeakx
         write(unit=i_out,fmt="(a,tr1,i5  )")" => Number of background parameters :",n_ba
         write(unit=i_out,fmt="(a,tr1,i5  )")" => Number of cycles:",c%icyc
         if(npeakx == 0) then
           write(unit=i_out,fmt="(a)")" => ZERO Number of peaks, skipping this scan ...."
           cycle
         end if

         !save input data
         j = 0
         do i=1,vs%np
          if (vs%code(i) /= 0) j = j + 1
         end do
         c%npvar = j    ! number of refined parameters

         write(unit=i_out,fmt="(/,a)")          "      Profile Parameters for Pseudo-Voigt"
         write(unit=i_out,fmt="(a)")            "        pV(x) = ETA* L(x) + (1-ETA)* G(x)"
         write(unit=i_out,fmt="(a)")            "         FWHM_F = g0f_Hpv + g1f_Hpv * pos + g2f_Hpv * pos^2"
         write(unit=i_out,fmt="(a,/)")          "              ETA_F  = g0f_EtaPV + g1f_EtaPV * pos"
         write(unit=i_out,fmt="(a)")            "         FWHM_S = g0s_Hpv + g1s_Hpv * pos + g2s_Hpv * pos^2"
         write(unit=i_out,fmt="(a,/)")          "              ETA_S  = g0s_EtaPV + g1s_EtaPV * pos"
         write(unit=i_out,fmt="(/,a)")          " => Global parameters            Flag"
         write(unit=i_out,fmt="(a,f14.6,i3)")   " =>   Zero0      :", vs%pv(1),vs%code(1)
         write(unit=i_out,fmt="(a,f14.6,i3)")   " =>   Zero1      :", vs%pv(2),vs%code(2)
         write(unit=i_out,fmt="(a,f14.6,i3)")   " =>   Zero2      :", vs%pv(3),vs%code(3)
         write(unit=i_out,fmt="(a,f14.6,i3)")   " =>   g0f_Hpv    :", vs%pv(4),vs%code(4)
         write(unit=i_out,fmt="(a,f14.6,i3)")   " =>   g1f_Hpv    :", vs%pv(5),vs%code(5)
         write(unit=i_out,fmt="(a,f14.6,i3)")   " =>   g2f_Hpv    :", vs%pv(6),vs%code(6)
         write(unit=i_out,fmt="(a,f14.6,i3)")   " =>   g0s_Hpv    :", vs%pv(7),vs%code(7)
         write(unit=i_out,fmt="(a,f14.6,i3)")   " =>   g1s_Hpv    :", vs%pv(8),vs%code(8)
         write(unit=i_out,fmt="(a,f14.6,i3)")   " =>   g2s_Hpv    :", vs%pv(9),vs%code(9)
         write(unit=i_out,fmt="(a,f14.6,i3)")   " =>   g0f_EtaPV  :", vs%pv(10),vs%code(10)
         write(unit=i_out,fmt="(a,f14.6,i3)")   " =>   g1f_EtaPV  :", vs%pv(11),vs%code(11)
         write(unit=i_out,fmt="(a,f14.6,i3)")   " =>   g0s_EtaPV  :", vs%pv(12),vs%code(12)
         write(unit=i_out,fmt="(a,f14.6,i3)")   " =>   g1s_EtaPV  :", vs%pv(13),vs%code(13)
         write(unit=i_out,fmt="(a,f14.6,i3)")   " =>   k1_x       :", vs%pv(14),vs%code(14)
         write(unit=i_out,fmt="(a,f14.6,i3)")   " =>   k1_y       :", vs%pv(15),vs%code(15)
         write(unit=i_out,fmt="(a,f14.6,i3)")   " =>   k1_z       :", vs%pv(16),vs%code(16)
         write(unit=i_out,fmt="(a,f14.6,i3)")   " =>   k2_x       :", vs%pv(17),vs%code(17)
         write(unit=i_out,fmt="(a,f14.6,i3)")   " =>   k2_y       :", vs%pv(18),vs%code(18)
         write(unit=i_out,fmt="(a,f14.6,i3)")   " =>   k2_z       :", vs%pv(19),vs%code(19)
         write(unit=i_out,fmt="(a,f14.6,i3)")   " =>   k3_x       :", vs%pv(20),vs%code(20)
         write(unit=i_out,fmt="(a,f14.6,i3)")   " =>   k3_y       :", vs%pv(21),vs%code(21)
         write(unit=i_out,fmt="(a,f14.6,i3)")   " =>   k3_z       :", vs%pv(22),vs%code(22)

         write(unit=i_out,fmt="(/,a)")          " => Background parameters"
         write(unit=i_out,fmt="(a,i3,a)")          "      Chebychev polynomial of ",n_ba," coefficients"
         do j=1,n_ba
           write(unit=i_out,fmt="(i3,f18.4,i4)")   j, vs%pv(j+ngl),vs%code(j+ngl)
         end do

         write(unit=i_out,fmt="(/,a,/)")                                                    &
              " Peak-Position    Intensity    Sh-Position      Sh-FWHM         Sh-Eta            Flags"
         j=ngl+n_ba+1

         DO i=1,npeakx
           write(unit=i_out,fmt="(2f14.6,f14.2,2F14.6,tr5,4i3,L3)") corr_pp(i),vs%pv(j+1), vs%pv(j),vs%pv(j+2),vs%pv(j+3),   &
                                       vs%code(j+1), vs%code(j),vs%code(j+2),vs%code(j+3), satellite(i)
           j=j+4
         END DO
         write(unit=i_out,fmt= "(/,a,i4,/)") " => Total number of refined parameters: ", c%npvar
         c%tol=epsilon(0.0_cp)
         !write(unit=*,fmt="(a,i6    )") " => Number of points: ",no
         !write(unit=*,fmt="(a,2f14.4)") " => Treating data in range: ",d%x(1),d%x(1)

         chiold=1.0e35  !Initialize chi-old for each scan (used only in ODR)
         !For marquardt fit d%sw should be the weights
         weig=.false.
         if(algor == "CURFIT" .or. algor == "MARQUARDT") then
            d%sw=ww
            weig=.true.
         end if
         if(index(algor,"L") /= 0) algor="LEVENBERG-MARQUARDT"
         profile=" "; texte=" "
         write(texte,"(a,i3) ") " Scan #",np
         if(allocated(bad)) deallocate(bad)
         allocate(bad(npeakx))
         bad=0
         if(allocated(lines)) deallocate(lines)
         allocate(lines(npeakx))
         lines= " "

         L=1
         ! Provisory code for testing the initial conditions
         call Test_Initial_Parameters(np,weig,ch(L))
         if(plot_ini) then
           write(refl,"(a,3f6.2,a,3f6.2,a)") "Scan(",h_ini,"->", h_fin,") "
           call Output_Plot(d%nobs,d%x,d%y,d%yc,np,ch(L),refl,.true.)
         end if

         do
           write(*,*) " Trial #",L
           if(L > 1 .and. mode=="MULTI") then
             call Test_Initial_Parameters(np,weig,ch(L))
             if(ch(L) > ch(L-1)) then !Do not attempt to refine because the initial Chi2 is worse than the previous one
               Vs=Saved_Vs
               exit
             end if
           end if

           Select Case(trim(algor))
              Case("CURFIT","MARQUARDT")
                     call marquardt_fit(Sum_PV_Peaks,d,c,Vs,i_out,chir,profile,text_info=trim(texte))
              Case("ODR")
                    call ODR_LSQ(profile_patt_odr,d,Vs,c,lower,upper,c_print,lun=i_odr )
                    chir=chi2
              Case("LEVENBERG-MARQUARDT")
                   call Levenberg_Marquardt_Fit(profile_patt_LVM, d%nobs, c, Vs, chir, .true., texte)
           End Select

           call Update_Peaks()
           call Set_Lines()
           if(mode == "MULTI") then
             call manage_LSQ(L,chir,redo,bad)
             L=L+1 !Increment the trial number
             if(redo .and. L < 10) cycle
           end if
           exit
         end do

         if(chir > highchi .and. mode == "MULTI") then
            if(ichi <= 50) then
                ichi=ichi+1
                high_chi(ichi) = np
                vhigh_chi(ichi)= chir
            end if
            call write_single_CFL_scan(chir)
         else if(mode == "SINGLE" .and. rewrite) then
            call write_single_CFL_scan(chir)
         end if


         if(algor == "ODR") then
            call Info_ODR_VS(Chi2,i_out,c,vs,d)
         else if (algor == "LEVENBERG-MARQUARDT") then
            call Info_LSQ_Output(Chi2,0.0,d%nobs,d%x,d%y,d%yc,ww,i_out,c,vs,algor,text_info=trim(texte))
         else
            !Nothing to do because "CURFIT" calls directly to Info_LSQ_Output
         end if
         exclude=.false.
         Inten=0.0; sigma=0
         j=ngl+n_ba+1
         do i=1,npeakx
           rhkl=h_ini
           rhkl(jsc)=peak_pos(i)
           write(unit=*,fmt="(a)")  trim(lines(i))
           if(index(lines(i),"NaN") == 0 .and. chir < highchi) then
             if(nkvec /= 0) then
               write(unit=i_hkl,fmt="(I6.6,3f8.4,2f14.4,4f8.2,a)") Numor, rhkl,vs%pv(j+1),vs%spv(j+1), 0.0,0.0,0.0,0.0,"          # "//trim(testchi2)
               if(.not. satellite(i)) write(unit=i_nuc,fmt="(I6.6,3i4,2f14.4,4f8.2)") Numor, nint(rhkl),vs%pv(j+1),vs%spv(j+1), 0.0,0.0,0.0,0.0
             else
               write(unit=i_hkl,fmt="(I6.6,3i4,2f14.4,4f8.2,a)") Numor,nint(rhkl),vs%pv(j+1),vs%spv(j+1), 0.0,0.0,0.0,0.0,"          # "//trim(testchi2)
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
           call Output_Plot(d%nobs,d%x,d%y,d%yc,np,chir,refl)
         end if
         if(mode == "SINGLE") exit
      end if

    End Do

    if(ichi > 0 .and. mode == "MULTI") then
        write(*,"(/,a,f8.4)") " => The following scans have a Chi^2 higher than ",highchi
        do i=1,ichi
          write(*,"(a,i6,f12.4)")   "    Scan :",high_chi(i), vhigh_chi(i)
        end do
        write(*,"(a,f8.4,a,i4)") " => Number of scans with Chi^2 higher than ",highchi, " :", ichi
    end if

    call cpu_time(timf)
    write(unit=*,fmt="(a,f10.3,a)") " => CPU-time:", timf-timi," seconds"

  contains

    Subroutine Set_Lines()
      !Writing the line to be shown in the screen and check if **** or NaN appeared in order to

      bad=0
      j=ngl+n_ba+1
      lines=" "
      write(unit=testchi2,fmt="(a,i5,a,g16.4)") "   Scan: ",np," => Chi2:",chir !chi2
      do i=1,npeakx
        rhkl=h_ini
        pos=peak_pos(i)
        rhkl(jsc)=pos
        write(unit=line,fmt="(a,3f9.4,6f12.4,2L3)") trim(testchi2)//"  hkl, Int, Sig, FWHM, Sig: ",&
           rhkl,vs%pv(j+1),vs%spv(j+1),fwhm(i),sfwhm(i),eta(i),seta(i),satellite(i), weak_dyn(i)
        if(index(line,"NaN") /= 0 .or. index(line,"******") /= 0 ) bad(i) = 1
        lines(i) = line
        j=j+4
      end do
    End Subroutine Set_Lines

    Subroutine Cut_Bad_Borders(no)
      integer, intent(in out) :: no
      integer :: i_ini, i_fin, n
      real(kind=cp) :: max_int,min_int, b_level
      real(kind=cp), dimension(:), allocatable :: x,y,sy

      !Try to use a scan starting and finishing with low number of counts
       max_int=maxval(d%y); min_int=minval(d%y)
       b_level=min_int+0.025*(max_int-min_int)
       i_ini=0; i_fin=0
       do i=1,nint(0.25*d%nobs)  !First point with intensity close to max_int
         if(d%y(i) < b_level) then
           i_ini=i
           exit
         end if
       end do
       if(i_ini == 0) i_ini = 1
       do i=d%nobs,nint(0.75*d%nobs),-1  !Last point with intensity close to max_int
         if(d%y(i) < b_level) then
           i_fin=i
           exit
         end if
       end do
       if(i_fin == 0) i_fin = d%nobs
       no=i_fin-i_ini+1
       if(allocated(x)) deallocate(x)
       if(allocated(y)) deallocate(y)
       if(allocated(sy)) deallocate(sy)
       allocate(x(no),y(no),sy(no))
       n=0
       do i=i_ini,i_fin
         n=n+1
         x(n)=d%x(i); y(n)=d%y(i); sy(n)=d%sw(i)
       end do

       if(allocated(d%x)) deallocate(d%x)
       if(allocated(d%y)) deallocate(d%y)
       if(allocated(d%sw)) deallocate(d%sw)
       allocate(d%x(no),d%y(no),d%sw(no))
       d%x=x; d%y=y; d%sw=sy
    End Subroutine Cut_Bad_Borders

    Subroutine write_single_CFL_scan(chi)
      real(kind=cp), intent(in) :: chi
      character(len=25) :: forma
      write(highchi_file,"(a,i6.6,a)") "Scan_",np,".cfl"
      open(newunit=hchi,file=trim(highchi_file),status="replace",action="write")
      write(hchi,"(a,i4,a,6f8.2,i12)") "CFL File correspondig to scan # ",np," QSCAN",h_ini,h_fin,Numor
      write(hchi,"(a,f14.3)") "Current Chi2:", chi
      write(hchi,"(a)") "ALGOR "//trim(algor)
      write(hchi,"(a)") "FILESCAN "//trim(filedat)
      write(hchi,"(a,2i4)") "SCANS ",np,np
      if(plot_ini) write(hchi,"(a)") "PLOTINI"
      if(rewrite)  write(hchi,"(a)") "REWRITE"
      if(apply_cut) write(hchi,"(a)") "CUT"
      write(hchi,"(a,f6.2)") "THRESHOLD ", threshold
      if(cell_given) write(hchi,"(a,6f12.4)") "CELL ",cell
      if(len_trim(spg_symb) > 2) write(hchi,"(a)") "SPGR "//trim(spg_symb)
      write(hchi,"(a,i3)") "POLY ",n_ba
      if(nkvec > 0) then
        do i=1,nkvec
           write(hchi,"(a,3f12.7,i3)") "KVEC ",kvec(:,i), nharm(i)
        end do
      end if
      if(trim(algor) == "ODR") write(hchi,"(a,i4.4)") "CPRINT ",c_print
      Select Case(nkvec)
        Case(0)
            forma="(i6,tr2,3i4,7f14.6,tr7,L)"
            write(hchi,"(/,a)") "!    #     h   k   l     Position     Intensity     Sig(Int.)        FWHM       Sig(Fwhm)         Eta        Sig(Eta)   Satellite"
        Case(1)
            forma="(i6,tr2,4i4,7f14.6,tr7,L)"
            write(hchi,"(/,a)") "!    #     h   k   l   m     Position     Intensity     Sig(Int.)        FWHM       Sig(Fwhm)         Eta        Sig(Eta)   Satellite"
        Case(2)
            forma="(i6,tr2,5i4,7f14.6,tr7,L)"
            write(hchi,"(/,a)") "!    #     h   k   l   m   n     Position     Intensity     Sig(Int.)        FWHM       Sig(Fwhm)         Eta        Sig(Eta)   Satellite"
        Case(3)
            forma="(i6,tr2,6i4,7f14.6,tr7,L)"
            write(hchi,"(/,a)") "!    #     h   k   l   m   n   p     Position     Intensity     Sig(Int.)        FWHM       Sig(Fwhm)         Eta        Sig(Eta)   Satellite"
      End Select
      write(hchi,"(a,i4)") "NUMPEAKS ",npeakx
      j=ngl+n_ba+1
      do i=1,npeakx
        write(hchi,forma) i, hkl(:,i), peak_pos(i), Intens(i),vs%spv(j+1), fwhm(i), sfwhm(i), eta(i), seta(i),satellite(i)
        j=j+4
      end do
      write(hchi,"(/,a,i4)") "NUMPAR ",vs%np
      do i=1,vs%np
        write(hchi,"(i6,a,2f14.6,i6)") i, "   "//vs%nampar(i),vs%pv(i),vs%spv(i),vs%code(i)
      end do
      close(unit=hchi)
    End Subroutine write_single_CFL_scan

  End Program rl_Int1D