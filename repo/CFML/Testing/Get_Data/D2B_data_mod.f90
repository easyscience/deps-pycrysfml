!-------------------------------------------------------------
! Int3D08
! -------------------------------------------------------------
! This file is part of Int3d08
!
! The Int3D project is distributed under LGPL. In agreement with the
! Intergovernmental Convention of the ILL, this software cannot be used
! in military applications.
!
! Copyright (C) 2020-2022  Institut Laue-Langevin (ILL), Grenoble, FRANCE
!
! Authors: Nebil A. Katcho (ILL)
!          Juan Rodriguez-Carvajal (ILL)
!
!
!
! This library is free software; you can redistribute it and/or
! modify it under the terms of the GNU Lesser General Public
! License as published by the Free Software Foundation; either
! version 3.0 of the License, or (at your option) any later version.
!
! This library is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
! Lesser General Public License for more details.
!
! You should have received a copy of the GNU Lesser General Public
! License along with this library; if not, see <http://www.gnu.org/licenses/>.
!
! -------------------------------------------------------------

module D2B_data_mod

    use CFML_GlobalDeps,      only: To_Rad,To_Deg,Err_CFML,clear_error,CFML_DEBUG,cp
    use CFML_ILL_Instrm_Data, only: Current_Instrm,Diffractometer_Type, Calibration_Detector_Type, &
                                    Write_Current_Instrm_data
    use CFML_SXTAL_Geom,      only: diffractometer => psd, psd_convert
    use CFML_DiffPatt,        only: DiffPat_E_Type,Allocate_Pattern,write_pattern,Add_patterns,DiffPat_Type
    use CFML_Maths,           only: locate, Linear_Interpol
    use D2B_read_mod,         only: cfl_D2B_type
    use HDF5
    use nexus_mod

    implicit none

    private

    ! List of public subroutines
    public :: construct_virtual_detector, get_powder_pattern, set_alphas_shifts_D2B, set_state, &
              VerticalStraight_Integration

    !Private module variables
    integer, parameter :: NSAMPLES_MAX = 1000
    real,    parameter :: EPSIL = 0.01
    real               :: ga_range_real
    real               :: diffpat_tsamp, diffpat_tset
    integer, dimension(:,:),   allocatable :: nsamples
    real,    dimension(:,:,:), allocatable :: counts_virtual,sigma_counts_virtual
    real,    dimension(:,:),   allocatable :: calibration
    !logical :: pattern_allocated = .false.
    character(len=:), allocatable :: diffpat_Title, diffpat_info, diffpat_norm, diffpat_cal

    !Public global variables
    type(Calibration_Detector_Type),        public :: Cal
    real,    dimension(:,:),   allocatable, public :: Data2D, sData2D
    real,                                   public :: nu_D_virtual = 0.0
    real,                                   public :: ga_D_virtual
    type(diffractometer_type),              public :: virtual_instrm

    contains

    Subroutine Construct_Virtual_Detector(nex,nscans,nsigma,num_vc,norm_monitor,printt,ApplyShifts,Cal_file)
       type(nexus_type), dimension(:),  intent(in) :: nex
       integer,                         intent(in) :: nscans
       real,                            intent(in) :: nsigma
       integer,                         intent(in) :: num_vc
       real,                            intent(in) :: norm_monitor
       logical,                         intent(in) :: printt
       logical,         optional,       intent(in) :: ApplyShifts
       character(len=*),optional,       intent(in) :: Cal_file

       integer :: i
       real    :: cnorm,ga_range_real,dga,dga_i,ga_D_min,ga_D_min_i,ga_D_max,ga_D_max_i,ga_1,ga_N
       logical :: Apply

       call clear_error()
       Apply=.false.
       if(present(ApplyShifts)) Apply=ApplyShifts
       write(*,'(a)') ' => Computing virtual detector common for all scans'
       ! Determine the virtual detector common for all scans
       ! dga_i -> scan step of the scan i
       ! ga_D_min_i -> minimum value of the gamma detector value for frames in the scan
       ! ga_D_max_i -> maximum value of the gamma detector value for frames in the scan
       ga_range_real = to_deg * current_instrm%cgap * (current_instrm%np_horiz - 1) / current_instrm%dist_samp_detector
       dga      = abs(nex(1)%angles(4,nex(1)%nf)-nex(1)%angles(4,1)) / (nex(1)%nf - 1)
       ga_D_min = min(nex(1)%angles(4,nex(1)%nf),nex(1)%angles(4,1))
       ga_D_max = max(nex(1)%angles(4,nex(1)%nf),nex(1)%angles(4,1))

        cnorm=0.0
        do i=1,nscans
          cnorm=cnorm+nex(i)%monitor(1)
        end do
        cnorm=cnorm/nscans

        !cnorm=sum(nex(:)%monitor(1))/nscans

       do i = 2 , nscans
          dga_i      = abs(nex(i)%angles(4,nex(i)%nf)-nex(i)%angles(4,1)) / (nex(i)%nf - 1)
          ga_D_min_i = min(nex(i)%angles(4,nex(i)%nf),nex(i)%angles(4,1))
          ga_D_max_i = max(nex(i)%angles(4,nex(i)%nf),nex(i)%angles(4,1))
          if (abs(dga-dga_i) > EPSIL) then
              err_CFML%Flag=.true.
              err_CFML%Msg = 'Scans with different scan steps cannot be combined'
              return
          end if
          if (ga_D_min_i < ga_D_min) ga_D_min = ga_D_min_i
          if (ga_D_max_i > ga_D_max) ga_D_max = ga_D_max_i
       end do

       ! ga_1 -> gamma value of the first pixel of the virtual detector
       ! ga_N -> gamma value of the last  pixel of the virtual detector
       ga_1 = ga_D_min - 0.5 * ga_range_real
       ga_N = ga_D_max + 0.5 * ga_range_real

       !Construction of "virtual_instrm" of type diffractometer_type
       call set_virtual_detector(ga_1,ga_N,dga,printt)

       ! Start looping over scans to fill the virtual detector
       do i = 1 , nscans
          if (abs(current_instrm%wave-nex(i)%wave) > EPSIL) then
              write(*,'(8x,a)') 'Warning! Wavelengths from instrument and nexus differ! Instrument wavelength will be used'
              write(*,'(8x,a,f6.4)') 'Wavelength (instrument): ',current_instrm%wave
              write(*,'(8x,a,f6.4)') 'Wavelength (nexus): ',nex(i)%wave
          end if
          if (trim(current_instrm%data_ordering) /= nex(i)%data_ordering) then
              write(*,'(8x,a)') 'Warning! Data ordering from instrument and nexus differ! Instrument data ordering will be used'
              write(*,'(8x,a)') 'Data ordering (instrument): '//trim(current_instrm%data_ordering)
              write(*,'(8x,a)') 'Data ordering (nexus)     : '//trim(nex(i)%data_ordering)
          end if
          call fill_virtual_detector(nex(i),cnorm,Apply)
       end do

       write(*,'(a)') ' => Averaging counts for virtual pixels'

       call average_virtual_counts(nsigma,num_vc) !-> Construct Data2D

       !Setting information about the diffraction pattern to be output

       diffpat_Title=trim(nex(1)%experiment_id)
       diffpat_info=" Numors: "//nex(1)%filcod//" - "//nex(nscans)%filcod//"  "//trim(nex(1)%user)//"  "//trim(nex(1)%local_contact)//"  "//trim(nex(nscans)%end_time)
       diffpat_norm="                  "
       write(diffpat_norm,"(f12.2)") norm_monitor
       if(present(Cal_file)) then
         diffpat_cal="Calibration file: "//trim(Cal_file)
       else
         if(Apply) then
            diffpat_cal="Calibration file: unspecified, but full callibration is applied"
            if(current_instrm%alpha_correct) diffpat_cal="Calibration data obtained from the *.geom file"
         else
            diffpat_cal="Calibration shift of detectors not applied"
         end if
       end if
       diffpat_tsamp = sum(nex(:)%temperature)/real(nscans)
       diffpat_tset  = nex(1)%setp_temperature
    End Subroutine Construct_Virtual_Detector


    Subroutine allocate_virtual_arrays()

        if (allocated(Data2D)) deallocate(Data2D)
        if (allocated(sData2D)) deallocate(sData2D)
        if (allocated(counts_virtual)) deallocate(counts_virtual)
        if (allocated(sigma_counts_virtual)) deallocate(sigma_counts_virtual)
        if (allocated(nsamples)) deallocate(nsamples)
        ! Assign memory to arrays
        allocate(        Data2D(virtual_instrm%np_vert,virtual_instrm%np_horiz))
        allocate(       sData2D(virtual_instrm%np_vert,virtual_instrm%np_horiz))
        allocate(counts_virtual(virtual_instrm%np_vert,virtual_instrm%np_horiz,NSAMPLES_MAX))
        allocate(sigma_counts_virtual(virtual_instrm%np_vert,virtual_instrm%np_horiz,NSAMPLES_MAX))
        allocate(      nsamples(virtual_instrm%np_vert,virtual_instrm%np_horiz))
        Data2D(:,:) = 0.0; sData2D(:,:)=0.0
        counts_virtual(:,:,:) = 0.0
        sigma_counts_virtual(:,:,:) = 0.0
        nsamples(:,:) = 0

    End Subroutine allocate_virtual_arrays

    Subroutine average_virtual_counts(nsigma, num_vc)
        ! Arguments
        real,    intent(in) :: nsigma
        integer, intent(in) :: num_vc
        ! Local variables
        integer :: i,j,k,n
        real    :: ave,sigma,dsigma

        do j = 1 , size(nsamples,2)
            do i = 1 , size(nsamples,1)
                if (nsamples(i,j) == 0) then
                     Data2D(i,j)=-1.0
                    cycle
                end if
                if (nsamples(i,j) == 1) then
                    Data2D(i,j) = counts_virtual(i,j,1)
                    cycle
                end if
                ! Compute the average
                ave = 0.0
                do k = 1 , nsamples(i,j)
                    ave = ave + counts_virtual(i,j,k)
                end do
                ave = ave / nsamples(i,j)
                ! Compute standard deviation
                sigma = 0.0
                do k = 1 , nsamples(i,j)
                    sigma = sigma + (counts_virtual(i,j,k) - ave)**2
                end do
                sigma = sqrt(sigma / nsamples(i,j))
                ! Compute the final value
                n = 0
                dsigma = nsigma * sigma
                Data2D(i,j) = 0.0
                do k = 1 , nsamples(i,j)
                    if (abs(counts_virtual(i,j,k) - ave) < dsigma) then
                        n = n + 1
                        Data2D(i,j)  =  Data2D(i,j) + counts_virtual(i,j,k)
                        sData2D(i,j) = sData2D(i,j) + sigma_counts_virtual(i,j,k)
                    end if
                end do
                if (n > 0) then
                    Data2D(i,j) =  num_vc *  Data2D(i,j)  / real(n)
                   sData2D(i,j) =  num_vc * sData2D(i,j)  / real(n)
                end if
            end do
        end do

    End Subroutine average_virtual_counts

    Subroutine fill_virtual_detector(nexus,cnorm,apply)

        ! Arguments
        type(nexus_type), intent(in) :: nexus
        real,             intent(in) :: cnorm
        logical,          intent(in) :: Apply  !Apply detector shifts if true
        ! Local variables
        integer :: i,j,k,ii,jj
        real :: ga_D,nu_D,px,pz,x_D,z_D,ga_P,nu_P,fac,alpha,s_alpha, Ints, s_Ints

        do k = 1 , nexus%nf
            ga_D = nexus%angles(4,k)
            nu_D = nexus%angles(7,k)
            fac=cnorm/nexus%monitor(k) !This should be close to the average count monitor except if one of the scans is not completed

            do j = 1 , nexus%nx
                do i = 1 , nexus%nz
                    if(.not. Cal%Active(i,j)) cycle
                    px = j - 1 !Should be corrected for horizontal positions of detectors
                    pz = i - 1
                    if(apply) then
                       call psd_convert(current_instrm,1,0,ga_D,nu_D,px,pz,x_D,z_D,ga_P,nu_P,.true.)  !Pixels(px,pz) -> Angles(ga_P,nu_P)
                    else
                       call psd_convert(current_instrm,1,0,ga_D,nu_D,px,pz,x_D,z_D,ga_P,nu_P)      !Pixels(px,pz) -> Angles(ga_P,nu_P)
                    end if
                    call psd_convert(virtual_instrm,1,1,ga_D_virtual,nu_D,px,pz,x_D,z_D,ga_P,nu_P) !Angles(ga_P,nu_P) -> Pixels(px,pz)
                    if (Err_CFML%ierr == 0) then
                        ii = nint(pz) + 1
                        jj = nint(px) + 1
                        if (ii > 0 .and. ii <= virtual_instrm%np_vert .and. jj > 0 .and. jj <= virtual_instrm%np_horiz) then
                            nsamples(ii,jj) = nsamples(ii,jj) + 1
                            if (nsamples(ii,jj) <= NSAMPLES_MAX) then

                               alpha=current_instrm%alphas(i,j); s_alpha=current_instrm%sigma_alphas(i,j)**2  !s_ are here variances
                               Ints=nexus%counts(i,j,k); s_Ints=nexus%counts(i,j,k)

                               counts_virtual(ii,jj,nsamples(ii,jj)) = counts_virtual(ii,jj,nsamples(ii,jj)) + fac *Ints *  alpha

                                !Here, for the moment sigma is variance
                               sigma_counts_virtual(ii,jj,nsamples(ii,jj)) = sigma_counts_virtual(ii,jj,nsamples(ii,jj)) + &
                                                                              Ints * fac * (alpha*alpha + Ints * s_alpha)
                            else
                                write(*,'(8x,a)') 'Warning! Nsamples reached its maximum allowed value!'
                                nsamples(ii,jj) = NSAMPLES_MAX
                            end if
                        end if
                    end if
                end do
            end do
        end do

    End Subroutine fill_virtual_detector

    Subroutine set_virtual_detector(ga_1,ga_N,dga,printt)

        ! Arguments
        real,    intent(in) :: ga_1  ! gamma of the first pixel
        real,    intent(in) :: ga_N  ! gamma of the last pixel
        real,    intent(in) :: dga   ! gamma step
        logical, intent(in) :: printt

        virtual_instrm%ipsd     = current_instrm%ipsd
        virtual_instrm%np_horiz = nint((ga_N-ga_1) / dga) + 1
        virtual_instrm%cgap     = current_instrm%dist_samp_detector * dga * to_rad
        virtual_instrm%np_vert  = current_instrm%np_vert
        virtual_instrm%agap     = current_instrm%agap
        virtual_instrm%det_offsets(:)     = current_instrm%det_offsets(:)
        virtual_instrm%dist_samp_detector = current_instrm%dist_samp_detector
        virtual_instrm%data_ordering      = current_instrm%data_ordering
        ga_D_virtual = 0.5 * (ga_1 + ga_N)
        call allocate_virtual_arrays()
        if(printt) then
          write(*,'(/,8x,a)')         'VIRTUAL DETECTOR FEATURES: '
          write(*,'(8x,a,1x,f8.3)')   'Gamma step: ', dga
          write(*,'(8x,a,1x,f8.3)')   'Gamma range of the real    detector: ', ga_range_real
          write(*,'(8x,a,1x,f8.3)')   'Gamma range of the virtual detector: ', ga_N - ga_1
          write(*,'(8x,a,1x,i6)')     'Number of horizontal pixels of the real    detector: ', current_instrm%np_horiz
          write(*,'(8x,a,1x,i6)')     'Number of horizontal pixels of the virtual detector: ', virtual_instrm%np_horiz
          write(*,'(8x,a,1x,f8.3)')   'Size of horizontal pixel of the real    detector (mm): ', current_instrm%cgap
          write(*,'(8x,a,1x,f8.3)')   'Size of horizontal pixel of the virtual detector (mm): ', virtual_instrm%cgap
          write(*,'(8x,a,1x,f8.3)')   'Gamma value for the first pixel of the virtual detector: ', ga_1
          write(*,'(8x,a,1x,f8.3,/)') 'Gamma value for the last  pixel of the virtual detector: ', ga_N
        end if
    End Subroutine set_virtual_detector

    Subroutine get_powder_pattern(nz_int,scale_fac,np_horiz,virtual_cgap,ga_D,pat,counts_int,nz2)
        ! Arguments
        integer,                        intent(in)     :: nz_int
        real,                           intent(in)     :: scale_fac
        integer,                        intent(in)     :: np_horiz
        real,                           intent(in)     :: virtual_cgap
        real,                           intent(in)     :: ga_D
        type(DiffPat_E_Type),           intent(in out) :: pat
        integer,optional,dimension(:,:),intent(in)     :: counts_int
        integer,optional,               intent(in)     :: nz2

        ! Local variables
        integer                         :: i,k,k1,k2,nc,ith
        real                            :: span_angle,tth,fac
        real                            :: cosgam,gmin, stepg
        real                            :: h, hini
        real, dimension(:), allocatable :: cnu, gam

        call clear_Error()
        current_instrm%np_horiz = np_horiz  !This makes partially "current_instrm" equal
        current_instrm%cgap = virtual_cgap  !to the constructed virtual instrument
        span_angle = (((current_instrm%np_horiz-1) * current_instrm%cgap)/current_instrm%dist_samp_detector) * to_deg

        ! Compute possible cos(nu) and gamma values
        allocate(cnu(current_instrm%np_vert),gam(current_instrm%np_horiz))
        h = current_instrm%np_vert * current_instrm%agap
        hini = -0.5*(h - current_instrm%agap)
        do i=1,current_instrm%np_vert
            h = hini + (i-1) * current_instrm%agap
            cnu(i) = cos(atan2(h,current_instrm%dist_samp_detector))
        end do

        gmin = ga_D - 0.5 * span_angle; stepg = to_deg * current_instrm%cgap/current_instrm%dist_samp_detector

        do i=1,current_instrm%np_horiz
            gam(i) = gmin + (i-1) * stepg
        end do

        ! Allocating a 1D powder diffraction pattern
        call Allocate_Pattern(pat,np_horiz)

        ! Set diffraction pattern attributes
        pat%kindrad = 'neutron'
        pat%scatvar = '2theta'
        pat%npts    = current_instrm%np_horiz - 1
        pat%xmin    = ga_d - span_angle * 0.5
        pat%xmax    = ga_d + span_angle * 0.5
        pat%step    = span_angle / (current_instrm%np_horiz - 1)
        pat%wave(1) = current_instrm%wave
        pat%title   = trim(diffpat_title)//" -> "//trim(diffpat_cal)
        pat%FilePath= diffpat_info
        pat%TSet    = diffpat_tset
        pat%TSample = diffpat_tsamp
        read(unit=diffpat_norm,fmt=*) pat%monitor
        ! Initialize pattern
        do i = 1 , current_instrm%np_horiz
            pat%x(i) = pat%xmin + (i-1) * pat%step - 0.625 !half angular distance between two tubes
        end do

        pat%y     = 0.0
        pat%sigma = 0.0
        pat%nd    = 0

        ! Compute integration limits
        if(present(nz2)) then
           k1=max(nz_int,1)
           k2=min(nz2,current_instrm%np_vert)
        else
           nc = max(1,nz_int/2)
           nc = min(current_instrm%np_vert/2,nc)
           k1 = current_instrm%np_vert/2 - nc   !+ 1
           k2 = current_instrm%np_vert/2 + nc
        end if
        write(unit=*,fmt="(12x,a,2i4)") " 2Theta-Arc Integration between cells: ",k1,k2

        if(present(counts_int)) then
            data2D=counts_int
        end if

        ! Map Data2D -> Pat
        do i = 1 , np_horiz
            cosgam=cosd(gam(i))
            do k = k1, k2
                if(data2D(k,i) <= 0.0) cycle
                tth = acosd(cnu(k)*cosgam)*sign(1.0,gam(i))
                ith = nint((tth-pat%xmin) / pat%step) + 1
                if (ith > 0 .and. ith <= Pat%npts) then
                    pat%y(ith)  = pat%y(ith)  +  Data2D(k,i)
                    pat%sigma(i)= pat%sigma(i)+ sData2D(k,i)
                    pat%nd(ith) = pat%nd(ith) + 1
                end if
            end do
        end do

        ! Average
        do i = 1 , pat%npts
            fac = scale_fac / max(1,pat%nd(i))
            pat%y(i)     = fac*pat%y(i)
            pat%sigma(i) = sqrt(fac*pat%sigma(i))
        end do

    End Subroutine get_powder_pattern

    Subroutine set_alphas_shifts_D2B(Cal)
      type(Calibration_Detector_Type), intent(in) :: Cal
      ! Local variables
      integer :: i,lun
      real, dimension(size(Cal%PosX)) :: angles

      if(allocated(current_instrm%alphas)) deallocate(current_instrm%alphas)
      if(allocated(current_instrm%sigma_alphas)) deallocate(current_instrm%sigma_alphas)
      if(allocated(current_instrm%shifts)) deallocate(current_instrm%shifts)
      allocate(current_instrm%alphas(current_instrm%np_vert,current_instrm%np_horiz))
      allocate(current_instrm%sigma_alphas(current_instrm%np_vert,current_instrm%np_horiz))
      allocate(current_instrm%shifts(current_instrm%np_horiz))

      if(Cal%True_Eff) then
        current_instrm%alphas=1.0/Cal%Effic
      else
        current_instrm%alphas=Cal%Effic
      end if
      current_instrm%sigma_alphas = Cal%sEffic
      where(.not. Cal%Active) current_instrm%alphas=-1.0
      !Calculate the shifts of the detector pixels in mm
      do i=1,size(Cal%PosX)
        angles(i) = to_rad*(-158.750+(i-1)*1.25 - Cal%PosX(i)) ! A negative value means a shift towards lower gamma
        current_instrm%shifts(i)=angles(i)*current_instrm%dist_samp_detector
      end do
      current_instrm%alpha_correct=.true.
      current_instrm%sigma_alpha_correct=.true.
      current_instrm%shift_correct=.true.
      !Testing
      open(newunit=lun,file="wtest_D2B.geom",status="replace",action="write",position="rewind")
      call Write_Current_Instrm_data(lun,"fil")
      close(unit=lun)
    End Subroutine set_alphas_shifts_D2B

    Subroutine set_state(cutoff)

        ! Arguments
        real, intent(in) :: cutoff

        ! Local variables
        integer:: i,j
        real :: einv_min,einv_max

        einv_min = 1.0 / (1.0 + cutoff)
        einv_max = 1.0 / (1.0 - cutoff)
        !Calibration is accessed via the current module global variable: type(Calibration_Detector_Type),public :: Cal
        do j = 1 , current_instrm%np_horiz
            do i = 1 , current_instrm%np_vert
                if (Cal%Effic(i,j) < einv_min .or. Cal%Effic(i,j) > einv_max) Cal%Active(i,j) = .false.
            end do
        end do

    End Subroutine set_state

    Subroutine VerticalStraight_Integration(nexus,cfl,cal,pat)
        ! Arguments
        type(nexus_type),dimension(:),  intent(in)  :: nexus
        type(cfl_D2B_type),             intent(in)  :: cfl
        type(Calibration_Detector_Type),intent(in)  :: Cal
        type(DiffPat_E_Type),           intent(out) :: pat

        ! Local variables
        integer, parameter :: nt=128 !Number of tubes
        integer                              :: i,j,k,nmax,nf,n
        logical, dimension(nt)               :: Active
        real,   dimension (:,:), allocatable :: IntTubes
        real,   dimension (:,:), allocatable :: TwoThetaTubes
        integer,dimension (:),   allocatable :: np
        real                                 :: step,varI,varE,sqI,sqE
        character(len=132)                   :: straux,fname
        type(DiffPat_E_Type),dimension(nt)   :: patterns

        call clear_Error()
        nmax= cfl%nscans * maxval(nexus(:)%nf)
        step=0.05
        allocate(IntTubes(nmax,nt),TwoThetaTubes(nmax,nt),np(nmax))
        IntTubes=0.0; TwoThetaTubes=0.0
        !Integration of the central part just suming the counts along each tube
        write(*,"(/,a,2i4,a,i3,/)") " => Integrating individual tubes between vertical pixels: ",cfl%kc1,cfl%kc2, &
                                  " -> width: ",cfl%kc2-cfl%kc1+1
        write(*,"(a,f10.4)") " => Used scale factor: ",cfl%scale_fac
        do j=1,nt
           np(j)=0
           do i=1, cfl%nscans
             do nf=1,nexus(i)%nf
                np(j)=np(j)+1
                TwoThetaTubes(np(j),j)=nexus(i)%angles(8,nf)+cal%PosX(j)
                IntTubes(np(j),j)=0.0
                do k=cfl%kc1,cfl%kc2  !Vertical integration of each individual detector
                  IntTubes(np(j),j)=IntTubes(np(j),j)+nexus(i)%counts(k,j,nf)
                end do
             end do
           end do
           call Allocate_Pattern(Patterns(j),np(j))
           write(straux,"(2(a,i4),a,2i4)") " Tube #",j,"    Number of points: ",np(j), " -> Integration between vertical pixels: ",cfl%kc1,cfl%kc2
           Patterns(j)%kindrad = 'Neutrons'
           Patterns(j)%scatvar = '2theta'
           Patterns(j)%Title=trim(straux)
           Patterns(j)%x(1:np(j))=TwoThetaTubes(1:np(j),j)
           Patterns(j)%npts=np(j)
           Patterns(j)%y(1:np(j))=cfl%scale_fac*IntTubes(1:np(j),j)*cal%Effic(1,j)

           do k=1,np(j)
             varI=IntTubes(k,j) ; varE=cal%sEffic(1,j)*cal%sEffic(1,j)
             sqI=IntTubes(k,j)*IntTubes(k,j) ; sqE=cal%sEffic(1,j)*cal%sEffic(1,j)
             Patterns(j)%sigma(k)= cfl%scale_fac*sqrt(sqE * varI + sqI * varE)
           end do

           n=Patterns(j)%npts
           Patterns(j)%xmin=Patterns(j)%x(1)
           Patterns(j)%ymin=minval(Patterns(j)%y)
           Patterns(j)%xmax=Patterns(j)%x(n)
           Patterns(j)%ymax=maxval(Patterns(j)%y)
           if(cfl%tubes_output) then
              if(cfl%Apply_Shifts) then
                 write(fname,"(3(a,i3.3),a)") "NZ_",cfl%kc1,"_",cfl%kc2,"_Shift_tube_",j,".xys"
              else
                 write(fname,"(3(a,i3.3),a)") "NZ_",cfl%kc1,"_",cfl%kc2,"_tube_",j,".xys"
              end if
              write(*,'(a)') ' => Writing xys file '//trim(fname)
              call write_pattern(trim(fname),Patterns(j),'xys')
           end if

        end do !j=1,nt tubes

        if(.not. cfl%tubes_output) then
           write(*,'(/,a)') ' => Output of individual tubes suppressed! '
           write(*,'(a)')   '    It is supposed that the shifts of detectors are '
           write(*,'(a)')   '    already refined and we output only the final integrated pattern. '
        else
           if(cfl%Apply_shifts) then
                write(*,'(/,a)') ' => The individual tube patterns are output with corrected positions '
           else
                write(*,'(/,a)') ' => The individual tube patterns are output with ideal positions '
                write(*,'(a)')   '    they are prepared to be used by FullProf in sequential mode, '
                write(*,'(a)')   '    for refining the Zero-shifts of each tube '
           end if
        end if

        if(cfl%combine) then  !Output the total integrated pattern if COMBINE is provided
           Active=.true.
           if(cfl%excl_dets) then
             do j=1,size(cfl%det_excluded)
                n=cfl%det_excluded(j)
                Active(n) = .false.
             end do
           end if
           call Add_Patterns(Patterns, nt, Active, Pat, step_int=step)
           if(err_CFML%Ierr /= 0) then
               write(*,"(a)") "  ERROR! "//trim(err_CFML%Msg)
           end if
        end if

    End Subroutine VerticalStraight_Integration

end module D2B_data_mod