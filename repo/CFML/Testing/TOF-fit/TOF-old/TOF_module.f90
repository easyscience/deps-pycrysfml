!!----
!!---- Module  TOF_Diffraction
!!----
!!
Module  TOF_Diffraction
   !--- Use Modules ----!
   Use CFML_GlobalDeps, only: Err_CFML
   USE CFML_Optimization_LSQ
   Use CFML_Profiles
   use CFML_DiffPatt
   use CFML_strings

   !---- Global Variables ----!
   implicit none

   type(LSQ_State_Vector_type), public     :: vs  ! State vector containing pv, code, vs%nampar,etc..
   type(LSQ_Conditions_type ),  public     :: c   ! Conditions of the algorithm
   Type(LSQ_Data_Type),         public     :: d   !Data to be refined (set in the main program)
   Type, Public :: IRF_Type
      integer                :: ich_jobt
      integer                :: ich_prof
      integer                :: ich_geom
      integer                :: ich_thrg
      integer                :: ich_uvw
      integer                :: ich_wave
      integer                :: ich_cthm
      integer                :: ich_rkk
      integer                :: ich_asym
      integer                :: ich_tofrg
      integer                :: ich_sigma
      integer                :: ich_d2tot
      integer                :: ich_alfbe
      integer                :: ich_alfbt
      integer                :: ich_twoth
      integer                :: ich_d2tof
      integer                :: ich_gamma
      character(len=150)     :: title
      integer                :: ireso
      integer                :: jobt
      integer                :: prof
      integer                :: geom
      integer                :: npoints
      integer                :: n_uvw
      real                   :: geomval
      real                   :: cthm
      real                   :: rkk
      real                   :: twoth
      real, dimension(2)     :: asym
      real, dimension(2)     :: d2tof
      real, dimension(3)     :: shape
      real, dimension(3)     :: thrg
      real, dimension(3)     :: wave
      real, dimension(3)     :: tofrg
      real, dimension(3)     :: sigma
      real, dimension(3)     :: gamma
      real, dimension(4)     :: d2tot
      real, dimension(4)     :: alfbe
      real, dimension(4)     :: alfbt
      real, dimension(2,6)   :: uvw
      real, dimension(3,60)  :: hghl
   End Type IRF_Type

   real, parameter                         :: sqrt_8ln2=2.3548200450309493820231386529194
   Type(IRF_Type)                          :: IRF_Info


   integer,parameter                       :: nBackGroundPoints_Max=300
   integer                                 :: nBackGroundPoints
   Type, Public :: BackGround_Type
        real                               :: x
        real                               :: y
   End Type BackGround_Type
   Type(BackGround_Type),dimension(nBackGroundPoints_Max) :: BackGroundPoint
   integer :: npeaks,npeaks_rf
   real    :: d2tof
   character(len=256),public ::  filedat,title,filecode



   integer, parameter     :: nglob_tof=15       ! Maximum number of global parameters for TOF
   integer, parameter     :: nshp_tof=6          ! Maximum number of peak-shape parameters per peak
   integer, parameter     :: nbac=120           ! Maximum number of background parameters
   integer                :: n_ba               ! Number of points to define the background
   integer                :: jobtyp             ! 0 Refinement, 1: Simulation
   integer                :: jstart,itype

   real, parameter     :: rad=57.29577951
   real                :: Chi2

   integer, dimension(nBackGroundPoints_Max),    private    :: bac_code           ! Backgroud refinement codes
   integer, parameter, private :: npeaks_max=300
   real, dimension(npeaks_max), private,save    :: Intens
   real, dimension(npeaks_max), private,save    :: sigma0
   real, dimension(npeaks_max), private,save    :: alpha0
   real, dimension(npeaks_max), private,save    :: beta0
   real, dimension(npeaks_max), private,save    :: eta0
   real, dimension(npeaks_max), private,save    :: der_sig2,der_sig1,der_sig0,der_sigQ
   real, dimension(npeaks_max), private,save    :: der_alf0,der_alf1,der_alf2,der_alf3
   real, dimension(npeaks_max), private,save    :: der_bet0,der_bet1,der_bet2,der_bet3
   real, dimension(npeaks_max), private,save    :: der_eta0,der_eta1,der_eta2

 Contains

    !!----
    !!---- Subroutine Open_Irf_File(filename,Ierr)
    !!----    character(len=*), intent(in) :: filename
    !!----    integer,          intent(out):: Ierr
    !!----
    !!---- Filename is the name of the irf file without extention
    !!----
    !!---- Update: April - 2009
    !!
    Subroutine Open_Irf_File(Filename,Ierr)
       !---- Arguments ----!
       character(len=*), intent(in) :: filename
       integer,          intent(out):: Ierr

       !---- Local variables ----!
       character(len=150)    :: line
       character(len=5)      :: keyw
       character(len=4)      :: job

       integer, parameter    :: iunit=65
       integer, dimension(10):: ivet
       integer               :: ierror, iv, npos, np
       real,dimension(10)    :: vet


       ! Init Variables
       IRF_Info=irf_type(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0, &
                         ' ',0,0,0,0,0,0,0.0,0.0,0.0,0.0,0.0,&
                         0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,&
                         0.0,0.0,0.0)
       ierr=0

       ! Open File
       open(unit=iunit, file=trim(filename)//'.irf',status='old',iostat=ierror)
       if (ierror /= 0) then
          write(unit=*,fmt="(a)") ' => Error when openning the IRF File: '//trim(filename)//'.irf'
          ierr=1
          return
       end if

       ! Title/ Ireso
       read(unit=iunit,fmt='(a)',iostat=ierror) line
       if (ierror /= 0) then
          write(unit=*,fmt="(a)") ' => Error reading IRF File: '//trim(filename)//'.irf'
          ierr=1
          return
       end if
       irf_info%title=adjustl(line)

       irf_info%ireso=0
       npos=index(line,'ireso')
       if (npos > 0) then
          line=line(npos:)
          call cut_string(line)
          call get_num(line,vet,ivet,iv)
          if (iv ==1) irf_info%ireso=ivet(1)
       end if

       ! Reading File
       np=0
       do
          read(unit=iunit,fmt='(a)',iostat=ierror) line
          if (ierror /= 0) exit

          line=adjustl(line)
          npos=len_trim(line)

          if (npos == 0) cycle
          if (line(1:1) =='!' .or. line(1:1)=='#') cycle

          keyw=line(1:5)
          npos=index(keyw,' ')
          if (npos > 0) keyw=keyw(1:npos-1)
          keyw= U_Case(keyw)

          select case (keyw)
             case ('JOBT ')
                !call INextString(line(5:),job)
                read(line(5:),*,iostat=ierror) job
                job= U_Case(job)
                if (job(1:3) == 'XR ' ) irf_info%jobt = 1
                if (job(1:3) == 'XRC' ) irf_info%jobt = 2
                if (job(1:4) == 'NEUT') irf_info%jobt = 3
                if (job(1:4) == 'NEUC') irf_info%jobt = 4
                if (job(1:4) == 'TOF ') irf_info%jobt = 5
                if (job(1:4) == 'TOFC') irf_info%jobt = 6
                if (irf_info%jobt /= 0) irf_info%ich_jobt=1

             case ('PROF ')
                read(line(5:),*,iostat=ierror) irf_info%prof,irf_info%shape
                if (ierror == 0) irf_info%ich_prof=1

             case ('THRG ')
                read(line(5:),*,iostat=ierror) irf_info%thrg
                if (ierror == 0) irf_info%ich_thrg=1

             case ('WAVE ')
                read(line(5:),*,iostat=ierror) irf_info%wave
                if (ierror == 0) irf_info%ich_wave=1

             case ('GEOM ')
                read(line(5:),*,iostat=ierror) job
                job=adjustl(job)
                if (ierror == 0) then
                   job = U_Case(job)
                   if (job(1:4) == 'BRAG') irf_info%geom = 1
                   if (job(1:4) == 'DEBY') irf_info%geom = 2
                   if (job(1:4) == 'SYNC') irf_info%geom = 3
                   if (job(1:3) == 'PSD' ) irf_info%geom = 4
                   if (job(1:4) == 'TRMB') irf_info%geom = 5
                   if (job(1:4) == 'TRMF') irf_info%geom = 6

                   select case (irf_info%geom)
                      case (3)
                         read(line(11:),*,iostat=ierror) irf_info%geomval
                      case (4)
                         read(line(10:),*,iostat=ierror) irf_info%geomval
                      case (5)
                         read(line(11:),*,iostat=ierror) irf_info%geomval
                   end select
                   irf_info%ich_geom=1
                end if

             case ('CTHM ')
                read(line(5:),*,iostat=ierror) irf_info%cthm
                if (ierror == 0) irf_info%ich_cthm=1

             case ('RKK  ')
                read(line(4:),*,iostat=ierror) irf_info%rkk
                if (ierror == 0) irf_info%ich_rkk=1

             case ('ASYM ')
                read(line(5:),*,iostat=ierror) irf_info%asym
                if (ierror == 0) irf_info%ich_asym=1

             case ('D2TOF')
                if (irf_info%ireso ==6) then
                   read(line(6:),*,iostat=ierror) irf_info%d2tof
                else
                   read(line(6:),*,iostat=ierror) irf_info%d2tof(1)
                end if
                if (ierror == 0) irf_info%ich_d2tof=1

             case ('TWOTH')
                read(line(6:),*,iostat=ierror) irf_info%twoth
                if (ierror == 0) irf_info%ich_twoth=1

             case ('TOFRG')
                read(line(6:),*,iostat=ierror) irf_info%thrg
                if (ierror == 0) irf_info%ich_tofrg=1

             case ('D2TOT')
                read(line(6:),*,iostat=ierror) irf_info%d2tot
                if (ierror == 0) irf_info%ich_d2tot=1

             case ('SIGMA')
                read(line(6:),*,iostat=ierror) irf_info%sigma
                if (ierror == 0) irf_info%ich_sigma=1

             case ('GAMMA')
                read(line(6:),*,iostat=ierror) irf_info%gamma
                if (ierror == 0) irf_info%ich_gamma=1

             case ('ALFBE')
                read(line(6:),*,iostat=ierror) irf_info%alfbe
                if (ierror == 0) irf_info%ich_alfbe=1

             case ('ALFBT')
                read(line(6:),*,iostat=ierror) irf_info%alfbt
                if (ierror == 0) irf_info%ich_alfbt=1

             case default
                call get_num(line,vet,ivet,iv)
                select case (iv)
                   case (1) ! IRESO=4
                      irf_info%npoints=ivet(1)

                   case (3) !   de IRESO=4
                      np=np+1
                      irf_info%hghl(1:3,np)=vet(1:3)

                   case (6)
                      irf_info%n_uvw=irf_info%n_uvw+1
                      irf_info%uvw(irf_info%n_uvw,:)=vet(1:6)
                      irf_info%ich_uvw=1
                end select

          end select ! keyw
       end do

       close(unit=iunit)

       return
    End Subroutine Open_Irf_File

   !!----
   !!---- Subroutine Tof_Profile_Fitting(Filecode, Pat, Ifail)
   !!----
   !!----
   !!---- Update: April - 2009
   !!
   Subroutine Tof_Profile_Fitting(Filecode, fit_xmin,fit_xmax,Pat, Ifail)
      !---- Arguments ----!
      character(len=*),      intent(in)     :: Filecode
      real,                  intent(in)     :: fit_xmin,fit_xmax
      Type(DiffPat_E_Type),  intent(in out) :: Pat
      integer,               intent(out)    :: ifail

      !---- Local Variables ----!
      integer                            :: i,j, no, ierror,ncount
      real, allocatable, dimension (:)   :: wf,A
      character(len=1) :: ans

      real :: ymax_gr, ymin_gr, yrange, intervalo

      ! Init
      chi2 = 0.
      c%reached = .false.
      C%corrmax = 50.0
      ifail = 0

      BackGroundPoint(1)%x=fit_xmin
      BackGroundPoint(n_ba)%x=fit_xmax

      i=index(filecode,".",back=.true.)-1
      if(i <= 0) i=len_trim(filecode)
      ! General output file
      OPEN(UNIT=7, FILE=filecode(1:i)//'_pf.out',STATUS='replace')
      WRITE(unit=7,fmt='(a)')' '
      WRITE(unit=7,fmt='(a)')'            ------------------------------------ '
      WRITE(unit=7,fmt='(a)')'                  --- PROGRAM: TOF-FIT ---'
      WRITE(unit=7,fmt='(a)')'            (Author: J. Rodriguez-Carvajal, ILL)'
      WRITE(unit=7,fmt='(a)')'                 (version 4.0 April - 2009) '
      WRITE(unit=7,fmt='(a)')'            ------------------------------------ '

      WRITE(7,'(/a,a)') ' => TITLE: ' ,Pat%title

      write(unit=7,fmt="(/,a)")    " => TOF range, peaks and cycles read "
      write(unit=7,fmt="(a,f14.3)")" => TOF(init)       : ",fit_xmin
      write(unit=7,fmt="(a,f14.3)")" => TOF(fin )       : ",fit_xmax
      write(unit=7,fmt="(a,i5  )") " => Number of peaks : ",npeaks
      write(unit=7,fmt="(a,i5  )") " => Number of background points : ",n_ba
      write(unit=7,fmt="(a,i5  )") " => Number of cycles: ",c%icyc

      ! Number of parameters
      vs%np = nglob_tof+ n_ba + 6 * npeaks

      ! save input data
      j = 0
      do i=1,vs%np
       if (vs%code(i) /=0) j = j + 1
      end do
      c%npvar  = j    ! nombre de parametres affines
      if(allocated(A)) deallocate(A)
      allocate(A(j))
      ncount=0
      do i=1,vs%np
         if (vs%code(i) == 0) cycle
         ncount=ncount+1
         a(ncount)=vs%pv(i)
      end do


      write(unit=7,fmt="(/a)")         " => Global parameters          Flag"
      write(unit=7,fmt="(a,f14.6,i3)") " => Global-alpha0",   vs%pv( 1),vs%code( 1)
      write(unit=7,fmt="(a,f14.6,i3)") " => Global-alpha1",   vs%pv( 2),vs%code( 2)
      write(unit=7,fmt="(a,f14.6,i3)") " => Global-alpha2",   vs%pv( 3),vs%code( 3)
      write(unit=7,fmt="(a,f14.6,i3)") " => Global-alpha3",   vs%pv( 4),vs%code( 4)
      write(unit=7,fmt="(a,f14.6,i3)") " => Global-beta0 ",   vs%pv( 5),vs%code( 5)
      write(unit=7,fmt="(a,f14.6,i3)") " => Global-beta1 ",   vs%pv( 6),vs%code( 6)
      write(unit=7,fmt="(a,f14.6,i3)") " => Global-beta2 ",   vs%pv( 7),vs%code( 7)
      write(unit=7,fmt="(a,f14.6,i3)") " => Global-beta3 ",   vs%pv( 8),vs%code( 8)
      write(unit=7,fmt="(a,f14.6,i3)") " => Global-Sig-2 ",   vs%pv( 9),vs%code( 9)
      write(unit=7,fmt="(a,f14.6,i3)") " => Global-Sig-1 ",   vs%pv(10),vs%code(10)
      write(unit=7,fmt="(a,f14.6,i3)") " => Global-Sig-0 ",   vs%pv(11),vs%code(11)
      write(unit=7,fmt="(a,f14.6,i3)") " => Global-Sig-Q ",   vs%pv(12),vs%code(12)
      write(unit=7,fmt="(a,f14.6,i3)") " => Global-eta0  ",   vs%pv(13),vs%code(13)
      write(unit=7,fmt="(a,f14.6,i3)") " => Global-eta1  ",   vs%pv(14),vs%code(14)
      write(unit=7,fmt="(a,f14.6,i3)") " => Global-eta2  ",   vs%pv(15),vs%code(15)
      write(unit=7,fmt="(/a)")           " => Background parameters"
      write(unit=7,fmt="(a)")            "      Scatt. Variable     Background  Flag"

      do j=1,n_ba
        write(unit=7,fmt="(2f18.4,i4)")   BackGroundPoint(j)%x, vs%pv(j+nglob_tof),vs%code(j+nglob_tof)
      end do

      write(unit=7,fmt="(/A/)")                                                    &
           "    Position       Intensity    Shift-Sigma   Shift-alpha    Shift-beta    Shift-Eta       Flags"
      j=nglob_tof+n_ba+1
      do i=1,npeaks
         write(unit=7,fmt="(f14.6,f14.2,4F14.6,5x,6i2)")                          &
               vs%pv(j),vs%pv(j+1),vs%pv(j+2),vs%pv(j+3),vs%pv(j+4),vs%pv(j+5),   &
               vs%code(j),vs%code(j+1),vs%code(j+2),vs%code(j+3),vs%code(j+4),vs%code(j+5)
         j=j+nshp_tof
      end do

      write(unit=7,fmt= "(/a,i4/)") " => Total number of refined parameters: ", c%npvar

      ! How many points are in the fit zone?
      no = 0
      do i = 1, Pat%npts
         if (Pat%x(i) > fit_xmin .and. Pat%x(i) < fit_xmax) no = no + 1
      end do

       if (allocated(d%x ))   deallocate (d%x)
       if (allocated(d%sw))   deallocate (d%sw)
       if (allocated(d%y ))   deallocate (d%y)
       if (allocated(d%yc))   deallocate (d%yc)
       if (allocated(wf))     deallocate (wf)   !Needed

       allocate ( d%x(no),d%sw(no),d%y(no),d%yc(no),wf(no), stat=ierror )
       if (ierror /= 0) then
         write(unit=*,fmt="(a)")' => Error allocating Space for TOF Fitting Procedure!'
         ifail=1
         return
       end if
      j=0
      d%iw=0
      do i=1,Pat%npts
        if(Pat%x(i) >= fit_xmin .and. Pat%x(i) <= fit_xmax) then
          j=j+1
          if(j > no) exit
          d%y(j)=Pat%y(i)
          d%sw(j)=sqrt(Pat%sigma(i)) !Using the LM method one should store the standard deviation d%iw=0
          wf(j)=1.0/Pat%sigma(i)
          d%x(j)=Pat%x(i)
          !write(unit=*,fmt="(a,i5,3f14.3)") " i, x ,y ,w: ", j,d%x(j),d%y(j),d%sw(j)
        end if
      end do
      d%nobs=no
      no=j
      if(jobtyp == 0) then
         do
            call marquardt_fit(Sum_Jorgensen_VonDreele_peaks,d%x,d%y,wf,d%yc,d%nobs,c,vs,7,chi2)
            if(c%failed) exit
            if (.not. c%reached) then
               write(unit=*,fmt="(a)",advance='no')' => Convergence not reached!. Do you want continue?'
               read(unit=*,fmt="(a)") ans
               if( ans == 'y' .or. ans == 'Y') cycle
            end if
            exit
         end do
      else
         do i=1,d%nobs
           call Sum_Jorgensen_VonDreele_peaks(i,d%x(i),d%yc(i),A)
         end do
         chi2=Fchisq(d%nobs-c%npvar,d%Nobs,d%Y,wf,d%Yc)
         write(unit=*,fmt= "(a,g14.5)") " => Simulation work, calculated Chi2: ",chi2

      end if

      ! Refill the calculated vales
      j=0
      do i=1,Pat%npts
         if (Pat%x(i) > fit_xmin .and. Pat%x(i) < fit_xmax) THEN
            j=j+1
            Pat%ycalc(i)=d%yc(j)
         else
            Pat%ycalc(i)=Pat%y(i)
         end if
      end do

      return
   End Subroutine Tof_Profile_Fitting

   !!----
   !!---- Subroutine Set_Nampar_TOF(N_Ba,Npeak)
   !!----
   !!----
   !!---- Update: April - 2009
   !!
   Subroutine Set_Nampar_TOF(N_Ba, Npeak)
      !---- Arguments ----!
      integer, intent (in):: n_ba
      integer, intent (in):: npeak

      !---- Local Variables ----!
      integer :: i,j

      vs%code_comp=.false.
      vs%nampar(:)= "               "

      vs%nampar( 1)="Global-alpha0"
      vs%nampar( 2)="Global-alpha1"
      vs%nampar( 3)="Global-alpha2"
      vs%nampar( 4)="Global-alpha3"
      vs%nampar( 5)="Global-beta0 "
      vs%nampar( 6)="Global-beta1 "
      vs%nampar( 7)="Global-beta2 "
      vs%nampar( 8)="Global-beta3 "
      vs%nampar( 9)="Global-Sig-2 "
      vs%nampar(10)="Global-Sig-1 "
      vs%nampar(11)="Global-Sig-0 "
      vs%nampar(12)="Global-Sig-Q "
      vs%nampar(13)="Global-eta0  "
      vs%nampar(14)="Global-eta1  "
      vs%nampar(15)="Global-eta2  "

      ! Background
      if (n_ba <= 9) then
         do j=1,n_ba
            write(unit=vs%nampar(nglob_tof+j),fmt="(a,i1)")   "background_",j
         end do
      else
         do j=1,9
            write(unit=vs%nampar(nglob_tof+j),fmt="(a,i1)")   "background_",j
         end do
         do j=10,min(n_ba,99)
            write(unit=vs%nampar(nglob_tof+j),fmt="(a,i2)")   "background_",j
         end do
         if(n_ba >= 100) then
            do j=100,n_ba
               write(unit=vs%nampar(nglob_tof+j),fmt="(a,i3)")   "background_",j
            end do
         end if
      end if

      j=nglob_tof+n_ba+1
      if (npeak <= 9) then
         do i=1,npeak
            write(unit=vs%nampar(j),  fmt="(a,i1)")   "TOF___Pos_",i
            write(unit=vs%nampar(j+1),fmt="(a,i1)")   "Intensity_",i
            write(unit=vs%nampar(j+2),fmt="(a,i1)")   "Shf_Sigma_",i
            write(unit=vs%nampar(j+3),fmt="(a,i1)")   "Shf_Alpha_",i
            write(unit=vs%nampar(j+4),fmt="(a,i1)")   "Shf__Beta_",i
            write(unit=vs%nampar(j+5),fmt="(a,i1)")   "Shf___Eta_",i
            j=j+nshp_tof
         end do
      else
         do i=1,9
            write(unit=vs%nampar(j),  fmt="(a,i1)")   "TOF___Pos_",i
            write(unit=vs%nampar(j+1),fmt="(a,i1)")   "Intensity_",i
            write(unit=vs%nampar(j+2),fmt="(a,i1)")   "Shf_Sigma_",i
            write(unit=vs%nampar(j+3),fmt="(a,i1)")   "Shf_Alpha_",i
            write(unit=vs%nampar(j+4),fmt="(a,i1)")   "Shf__Beta_",i
            write(unit=vs%nampar(j+5),fmt="(a,i1)")   "Shf___Eta_",i
            j=j+nshp_tof
         end do

         do i=10,min(99,npeak)
            write(unit=vs%nampar(j),  fmt="(a,i2)")   "TOF___Pos_",i
            write(unit=vs%nampar(j+1),fmt="(a,i2)")   "Intensity_",i
            write(unit=vs%nampar(j+2),fmt="(a,i2)")   "Shf_Sigma_",i
            write(unit=vs%nampar(j+3),fmt="(a,i2)")   "Shf_Alpha_",i
            write(unit=vs%nampar(j+4),fmt="(a,i2)")   "Shf__Beta_",i
            write(unit=vs%nampar(j+5),fmt="(a,i2)")   "Shf___Eta_",i
            j=j+nshp_tof
         end do
         if(npeak >= 100) then
           do i=100,npeak
              write(unit=vs%nampar(j),  fmt="(a,i3)")   "TOF___Pos_",i
              write(unit=vs%nampar(j+1),fmt="(a,i3)")   "Intensity_",i
              write(unit=vs%nampar(j+2),fmt="(a,i3)")   "Shf_Sigma_",i
              write(unit=vs%nampar(j+3),fmt="(a,i3)")   "Shf_Alpha_",i
              write(unit=vs%nampar(j+4),fmt="(a,i3)")   "Shf__Beta_",i
              write(unit=vs%nampar(j+5),fmt="(a,i3)")   "Shf___Eta_",i
              j=j+nshp_tof
           end do
         end if
      end if

      return
   End Subroutine Set_Nampar_TOF

   !!----
   !!---- Subroutine Sum_Jorgensen_Vondreele_Peaks(I,Tof,Ycalc,A,Der)
   !!----
   !!----
   !!---- Update: April - 2009
   !!
   Subroutine Sum_Jorgensen_Vondreele_Peaks(I,Tof,Ycalc,A,Der)
      !---- Arguments ----!
      integer,                    intent(in)   :: i
      real,                       intent(in)   :: tof
      real,                       intent(out)  :: ycalc
      real,dimension(:),          intent(in)   :: a
      real,dimension(:),optional, intent(out)  :: der

      !---Local variables ---!
      real, dimension(Max_Free_Par) :: p
      real, dimension(Max_Free_Par) :: dval
      integer                       :: ncount,j,npea,l,ib,ib1,ib2,i1,i2
      real                          :: profil, tang, bac_ctr, alfa, beta, dt, omega, gamma, eta,&
                                       w,dsp, dsp2, dsp4
      type(deriv_TOF_type)          :: deriv

      ! FITS TO Jorgensen functions: convolution of gaussian and back-to-back exponentials
      ! P are parameters. YCALC is the value returned to the main program
      ! IF DER is present the function calculates the analytical derivatives
      ! P(1) to P(14) are global parameters: Global-alpha0,1,2,3, Global-beta0,1,2,3 Global-Sig-2,
      ! Global-Sig-1, Global-Sig-0, global-Sig-Q, global-Eta0,1,2   (nglob_tof=15)
      ! P(15=nglob_tof+1) to P(nglob_tof+n_ba) are background parameters
      ! Jstart= nglob_tof+ n_ba +1
      ! P(jstart),P(jstar+1),P(jstar+2),P(jstar+3),P(jstar+4)... : PeakPosition, Intensity,
      !  Shft-sigma, Shft-alpha, Shft-beta, Shft-eta, PeakPosition, ....
      ! Background is calculated by linear interpolation of low and high background
      ! To avoid repetitive calculations, the values that cannot vary are stored
      ! in intermediate arrays in the first call
      !

      IF (i == 1) then
         lorcomp=.false.
         if (any(abs(vs%pv(13:) ) > 0.00001)) lorcomp=.true.
         p(:)=0.0
         ncount=0
         DO j=1,vs%np
            IF (vs%code(j) == 0) then
               p(j)=vs%pv(j)
            else
               ncount=ncount+1
               p(j)=a(ncount)
            end if
         END DO

         npea=0
         jstart=nglob_tof+n_ba+1
         DO j=jstart,vs%np,nshp_tof
            npea=npea+1
            Intens(npea)=p(j+1)
            if (abs(p(j+5)) > 0.00001 .or. vs%code(j+5) == 1 ) lorcomp=.true.
            dsp=p(j)/d2tof
            dsp2=dsp*dsp
            dsp4=dsp2*dsp2
            alpha0(npea) = p(1)+ p(2)/dsp  + p(3)/dsp2 +  p(4)/sqrt(dsp)   !Alpha0 calculated for all peaks
            beta0(npea)  = p(5)+ p(6)/dsp  + p(7)/dsp2 +  p(8)/dsp4        !Beta0 calculated for all peaks
            sigma0(npea) = p(9)*dsp4 + p(10)*dsp2 + p(11) + p(12)/dsp2     !Sigma0 calculated for all peaks
            eta0(npea)   = p(13) + p(14)*dsp  + p(15)*dsp2                 !Eta0 calculated for all peaks

            ! Derivatives
            if (present(der)) then
               der_sig2(npea)=0.0
               der_sig1(npea)=0.0
               der_sig0(npea)=0.0
               der_sigQ(npea)=0.0
               if (sigma0(npea) > 0.0001) then
                  der_sig2(npea)=dsp2*dsp2     !9
                  der_sig1(npea)=dsp2          !10
                  der_sig0(npea)=1.0           !11
                  der_sigQ(npea)=1.0/dsp2      !12
               end if
               der_alf0(npea)=0.0
               der_alf1(npea)=0.0
               der_alf2(npea)=0.0
               der_alf3(npea)=0.0
               if (alpha0(npea) > 0.0001) then
                  der_alf0(npea)=1.0
                  der_alf1(npea)=1.0/dsp
                  der_alf2(npea)=1.0/dsp2
                  der_alf3(npea)=1.0/sqrt(dsp)
               end if
               der_bet0(npea)=0.0
               der_bet1(npea)=0.0
               der_bet2(npea)=0.0
               der_bet3(npea)=0.0
               if (beta0(npea) > 0.0001) then
                  der_bet0(npea)=1.0
                  der_bet1(npea)=1.0/dsp
                  der_bet2(npea)=1.0/dsp2
                  der_bet3(npea)=1.0/dsp4
               end if
               der_eta0(npea)=0.0
               der_eta1(npea)=0.0
               der_eta2(npea)=0.0
               if (eta0(npea) > 0.0001) then
                  der_eta0(npea)=1.0
                  der_eta1(npea)=dsp2
                  der_eta2(npea)=dsp2
               end if
            end if
         END DO
      end if  !i=1

      ! Calculation of the function
      profil=0.0
      dval(1:vs%np)=0.0

      ! Calculation of the background
      i1=1
      i2=n_ba
      ib1=nglob_tof+1
      ib2=ib1+1
      do ib=1,n_ba-1
         if (tof >= BackGroundPoint(ib)%x .and. tof <= BackGroundPoint(ib+1)%x) then
            i1=ib
            i2=ib+1
            ib1=nglob_tof+i1
            ib2=ib1+1
            exit
         end if
      end do
      tang=(tof-BackGroundPoint(i1)%x )/(BackGroundPoint(i2)%x-BackGroundPoint(i1)%x)
      bac_ctr=p(ib1)+(p(ib2)-p(ib1))*tang     !background calculated

      l=0
      DO j=jstart,vs%np,nshp_tof
         l=l+1
         dt=tof-p(j)
         gamma= sqrt_8ln2*sqrt(abs(sigma0(l)+p(j+2)))
         alfa = alpha0(l)+p(j+3)
         beta = beta0(l)+p(j+4)
         if ( dt <= 0.0) then
             w=1.38629436112/alfa
         else
             w=1.38629436112/beta
         end if
         if (abs(dt)> 15.0*(gamma+w)) cycle
         eta  = eta0(l)+p(j+5)
         ! Calculation of the derivatives
         IF (present(der)) then
            call tof_Jorgensen_VonDreele(dt,alfa,beta,gamma,eta,omega,deriv)

            dval(1)  = dval(1)  + Intens(l)*deriv%alfa*der_alf0(l)      !DOmega/Dalfa  * Dalfa/Dalf0
            dval(2)  = dval(2)  + Intens(l)*deriv%alfa*der_alf1(l)      !DOmega/Dalfa  * Dalfa/Dalf1
            dval(3)  = dval(3)  + Intens(l)*deriv%alfa*der_alf2(l)      !DOmega/Dalfa  * Dalfa/Dalf2
            dval(4)  = dval(4)  + Intens(l)*deriv%alfa*der_alf3(l)      !DOmega/Dalfa  * Dalfa/Dalf3

            dval(5)  = dval(5)  + Intens(l)*deriv%beta*der_bet0(l)      !DOmega/Dbeta * Dbeta/Dbet0
            dval(6)  = dval(6)  + Intens(l)*deriv%beta*der_bet1(l)      !DOmega/Dbeta * Dbeta/Dbet1
            dval(7)  = dval(7)  + Intens(l)*deriv%beta*der_bet2(l)      !DOmega/Dbeta * Dbeta/Dbet2
            dval(8)  = dval(8)  + Intens(l)*deriv%beta*der_bet3(l)      !DOmega/Dbeta * Dbeta/Dbet3

            dval(9)   = dval(9)   + Intens(l)*deriv%sigma*der_sig2(l)   !DOmega/Dsigma * Dsigma/DSig-2
            dval(10)  = dval(10)  + Intens(l)*deriv%sigma*der_sig1(l)   !DOmega/Dsigma * Dsigma/DSig-1
            dval(11)  = dval(11)  + Intens(l)*deriv%sigma*der_sig0(l)   !DOmega/Dsigma * Dsigma/DSig-0
            dval(12)  = dval(12)  + Intens(l)*deriv%sigma*der_sigQ(l)   !DOmega/Dsigma * Dsigma/DSig-Q

            dval(13)  = dval(13)  + Intens(l)*deriv%eta*der_eta0(l)    !DOmega/Deta * Deta/Deta0
            dval(14)  = dval(14)  + Intens(l)*deriv%eta*der_eta1(l)    !DOmega/Deta * Deta/Deta1
            dval(15)  = dval(15)  + Intens(l)*deriv%eta*der_eta2(l)    !DOmega/Deta * Deta/Deta2

            dval(j)  = dval(j)  - Intens(l)*deriv%dt               !DOmega/Ddel * Ddel/Dp(j) <- Bragg Pos.
            dval(j+1)= dval(j+1)+ omega                            !Dprofil/Dri  <- Integr. Intensity
            dval(j+2)= dval(j+2)+ Intens(l)*deriv%sigma            !DOmega/DSh-sigma
            dval(j+3)= dval(j+3)+ Intens(l)*deriv%alfa             !DOmega/DSh-alfa
            dval(j+4)= dval(j+4)+ Intens(l)*deriv%beta             !DOmega/DSh-beta
            dval(j+5)= dval(j+5)+ Intens(l)*deriv%eta              !DOmega/DSh-eta
         else
            call tof_Jorgensen_VonDreele(dt,alfa,beta,gamma,eta,omega)
         end if
         !write(*,"(a)") trim(vs%nampar(j))//" "//trim(vs%nampar(j+1))//" "//       &
         !trim(vs%nampar(j+2))//" "//trim(vs%nampar(j+3))//" "//trim(vs%nampar(j+4)) &
         !//" "//trim(vs%nampar(j+5))
         !write(*,"(2i4,8f12.3)") j,l, p(j),tof, Intens(l), alfa,beta,gamma,eta,omega
         !write(*,"(2i4,7f12.3)") l,j, p(j:j+5) ,omega

         profil = profil + Intens(l) *  omega
      END DO

      ycalc = bac_ctr + profil    ! ycalc = Background + Bragg-contribution

      ! Assign derivatives to array DER
      IF (present(der)) then
         dval(ib1)=1.0-tang
         dval(ib2)=tang
         ncount=0
         DO j=1,vs%np
            IF (vs%code(j) == 0)CYCLE
            ncount=ncount+1
            der(ncount)=dval(j)
         END DO
      end if

      return
   End Subroutine Sum_Jorgensen_Vondreele_Peaks

End Module TOF_Diffraction
