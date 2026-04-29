SubModule (CFML_SXTAL_Geom) SXTAL_PSD

   implicit none

   Contains

   !!----
   !!---- GANU_FROM_XZ
   !!---- Calculates gamma and nu values from x,z pixel coordinates. This subroutine has been introduced
   !!---- to facilitate some calculations done by Int3D, to allow using psd_convert without having defined
   !!---- a diffractometer object
   !!----
   !!---- 29/03/2023
   !!----
   Module Subroutine ganu_from_xz(px,pz,ga_D,nu_D,ipsd,npix,pisi,dist_samp_detector,det_offsets,origin,ga_P,nu_P,f_virtual)
      !---- Arguments ----!
      real,                  intent(in)  :: px                 ! x coordinate, in pixels
      real,                  intent(in)  :: pz                 ! z coordinate, in pixels
      real,                  intent(in)  :: ga_D               ! gamma angle of the center of the detector, in degrees
      real,                  intent(in)  :: nu_D               ! nu    angle of the center of the detector, in degrees
      integer,               intent(in)  :: ipsd               ! detector type
      integer, dimension(2), intent(in)  :: npix               ! number of horizontal and vertical pixels
      real,    dimension(2), intent(in)  :: pisi               ! horizontal and vertical pixel sizes
      real,                  intent(in)  :: dist_samp_detector ! sample detector distance
      real,    dimension(3), intent(in)  :: det_offsets        ! x, y and z detector offsets
      integer,               intent(in)  :: origin             ! origin for numbering pixels
      real,                  intent(out) :: ga_P               ! Gamma value of the reflection
      real,                  intent(out) :: nu_P               ! Nu value of the reflection
      integer, optional,     intent(in)  :: f_virtual          ! stretching factor for virtual detectors

      !---- Local variables ----!
      integer :: f
      real    :: x_D,z_D,px_,pz_
      type(diffractometer_type) :: diffractometer

      diffractometer%ipsd = ipsd
      diffractometer%np_horiz = npix(1)
      diffractometer%np_vert = npix(2)
      diffractometer%cgap = pisi(1)
      diffractometer%agap = pisi(2)
      diffractometer%dist_samp_detector = dist_samp_detector
      diffractometer%det_offsets(:) = det_offsets(:)
      if (present(f_virtual)) then
         f = f_virtual
      else
         f = 1
      end if
      px_ = px ! px must be copied into px_ because it has intent = inout in psd_convert. Same for pz.
      pz_ = pz
      call psd_convert(diffractometer,f,0,ga_D,nu_D,px_,pz_,x_D,z_D,ga_P,nu_P,origin=origin)

   end subroutine ganu_from_xz

   !!---- PSD_CONVERT
   !!---- Calculates pixel coordinates from angles and viceversa for 2D detectors.
   !!----
   !!---- Coordinates given by the users are assumed to be Busing-Levy.
   !!---- Pixel numbering starts by zero
   !!---- The detector is assumed to be viewed from the crystal
   !!----
   !!---- r_D contains the pixel coordinates in the detector
   !!---- reference system
   !!----
   !!---- r_L contains the pixel coordinates in the laboratory
   !!---- reference system (detector with gamma = 0
   !!---- and nu = 0)
   !!----
   !!---- conversion_type = 0: pixels to angles
   !!---- conversion_type = 1: angles to pixels
   !!----
   !!---- origin for pixel numbering = 0 : top    left
   !!---- origin for pixel numbering = 1 : top    right
   !!---- origin for pixel numbering = 2 : bottom right
   !!---- origin for pixel numbering = 3 : bottom left
   !!----
   !!---- 29/03/2023
   !!----
   Module Subroutine psd_convert(diffractometer,f_virtual,conversion_type,ga_D,nu_D,px,pz,x_D,z_D,ga_P,nu_P,shifts,origin)
      !---- Arguments ----!
      type(diffractometer_type), intent(in out) :: diffractometer
      integer,                   intent(in)     :: f_virtual
      integer,                   intent(in)     :: conversion_type
      real(kind=cp),             intent(in)     :: ga_D
      real(kind=cp),             intent(in)     :: nu_D
      real(kind=cp),             intent(in out) :: px ! pixel x
      real(kind=cp),             intent(in out) :: pz ! pixel z
      real(kind=cp),             intent(in out) :: x_D
      real(kind=cp),             intent(in out) :: z_D
      real(kind=cp),             intent(in out) :: ga_P
      real(kind=cp),             intent(in out) :: nu_P
      logical, optional,         intent(in)     :: shifts
      integer, optional,         intent(in)     :: origin

      !---- Local variables ----!
      integer :: orig
      real(kind=cp) :: px_mid,pz_mid,radius,y_D,x_L,y_L,z_L,px_,pz_,deltaX

      call clear_error()
      diffractometer%np_horiz = diffractometer%np_horiz * f_virtual
      diffractometer%cgap = diffractometer%cgap / f_virtual
      px_mid = (diffractometer%np_horiz-1)  / 2.0
      pz_mid = (diffractometer%np_vert-1)   / 2.0
      !px_ = px
      !pz_ = pz
      if(present(shifts)) then
         deltaX = diffractometer%shifts(nint(px)) !indices for pixels in the calling program start with 0
      else
         deltaX = 0.0
      end if
      if(present(origin)) then
         orig = origin
      else
         orig = 3
      end if

      if (conversion_type == 0) then ! pixels to angles

         ! Detector reference system: origin at the center of the detector
         ! Use origin = 3 for calculations
         px_ = px
         pz_ = pz
         if (orig == 0 .or. orig == 1) pz_ = diffractometer%np_vert - pz_ - 1
         if (orig == 1 .or. orig == 2) px_ = diffractometer%np_horiz - px_ - 1
         x_D = (px_ - px_mid) * diffractometer%cgap
         y_D = 0.0
         z_D = (pz_ - pz_mid) * diffractometer%agap
         ! Cartesian coordinates in the laboratory system
         select case(diffractometer%ipsd)
               case(2) ! Flat detector
                  x_L = x_D + diffractometer%det_offsets(1)
                  y_L = y_D + diffractometer%det_offsets(2) + diffractometer%dist_samp_detector
                  z_L = z_D + diffractometer%det_offsets(3)
               case(3) ! Horizontal banana
                  x_L = diffractometer%dist_samp_detector * sin(x_D/diffractometer%dist_samp_detector) + &
                        diffractometer%det_offsets(1) - deltaX
                  y_L = diffractometer%dist_samp_detector * cos(x_D/diffractometer%dist_samp_detector) + &
                        diffractometer%det_offsets(2)
                  z_L = z_D + diffractometer%det_offsets(3)
               case default ! Unknown detector
                  Err_CFML%ierr = -1
                  Err_CFML%Msg = "Unknown detector"
                  return
         end select

         ga_P = ga_D + atan2d(x_L,y_L)
         nu_P = nu_D + atan2d(z_L,sqrt(x_L**2 + y_L**2))

      else ! angles to pixels

         ga_P = ga_P - ga_D
         nu_P = nu_P - nu_D
         radius = diffractometer%dist_samp_detector + diffractometer%det_offsets(2)

         select case(diffractometer%ipsd)
               case(2) ! Flat detector
                  x_D = radius * tand(ga_P) - diffractometer%det_offsets(1)
                  z_D = radius * tand(nu_P) / cosd(ga_P) - diffractometer%det_offsets(3)
               case(3) ! Horizontal banana
                  x_D = radius * ga_P * to_rad - diffractometer%det_offsets(1) + deltaX
                  z_D = radius * tand(nu_P) - diffractometer%det_offsets(3)
               case default ! Unknown detector
                  Err_CFML%ierr = -1
                  Err_CFML%Msg = "Unknown detector"
                  return
         end select

         ! Restore original values of ga_P and nu_P
         ga_P = ga_P + ga_D
         nu_P = nu_P + nu_D

         ! Compute pixels from detector coordinates
         px = px_mid + x_D / diffractometer%cgap
         pz = pz_mid + z_D / diffractometer%agap
         if (orig == 0 .or. orig == 1) pz = diffractometer%np_vert - pz - 1
         if (orig == 1 .or. orig == 2) px = diffractometer%np_horiz - px - 1

      end if

      ! Restore original values
      diffractometer%np_horiz = diffractometer%np_horiz / f_virtual
      diffractometer%cgap = diffractometer%cgap * f_virtual

   End Subroutine psd_convert
    !!----
    !!---- Module Subroutine psd_convert_old(mpsd,gamm,gamp,nup,xobs,zobs,cath,anod)
    !!----    Integer, Intent(In)            :: mpsd
    !!----    real(kind=cp), Intent(In)      :: gamm
    !!----    real(kind=cp), Intent(In Out)  :: gamp
    !!----    real(kind=cp), Intent(In Out)  :: nup
    !!----    real(kind=cp), Intent(Out)     :: xobs
    !!----    real(kind=cp), Intent(Out)     :: zobs
    !!----    real(kind=cp), Intent(in Out)  :: cath
    !!----    real(kind=cp), Intent(in Out)  :: anod
    !!----
    !!----    Original name: d19amd, now generalized to a series of PSDs
    !!----    Subroutine for getting Gamma and Nu of a reflections spot (GamP,NuP), given the
    !!----    gamma angle of the detector (GamM) and the pixel values (cath,anod). This is
    !!----    calculated when mpsd > 0, otherwise the inverse calculation is done. In both
    !!----    cases the detector coordinates (xobs,zobs) in mm are also calculated.
    !!----    The caracteristics of the detector are accessed via de global variable PSD of
    !!----    Type(Psd_Val_Type), that should be set by the calling program.
    !!----
    !!--<<    Original Comment:
    !!----    Specifically for D19A bannana detector, 4 x 64 degrees - 16 x 512
    !!----    cells and vertically curved.             R.F.D. STANSFIELD SEP-83
    !!----
    !!----    Modified for general case GamM .NE. GamP                     Feb-84
    !!----
    !!----    MPSD +VE - Find GamP, NuP given GamM, Cath and Anod
    !!----    MPSD -VE - Find Cath, Anod given GamM, GamP and NuP
    !!-->>
    !!----
    !!----    Extended for D19 cylindrical banana (from MJ Turner, peak find ...)
    !!----
    !!---- Update: July 2010
    !!
    Module Subroutine psd_convert_old(mpsd,gamm,gamp,nup,xobs,zobs,cath,anod)
       !---- Arguments ----!
       Integer, Intent(In)               :: mpsd
       real(kind=cp),    Intent(In)      :: gamm
       real(kind=cp),    Intent(In Out)  :: gamp
       real(kind=cp),    Intent(In Out)  :: nup
       real(kind=cp),    Intent(Out)     :: xobs
       real(kind=cp),    Intent(Out)     :: zobs
       real(kind=cp),    Intent(in Out)  :: cath
       real(kind=cp),    Intent(in Out)  :: anod

       !--- Local Variables ---!
       real(kind=cp) :: cmid, amid, delga, td, tn, e, f, g, cd, a, b, z, d

       cmid=real(psd%ncat-1)/2.0
       amid=real(psd%nano-1)/2.0
       call clear_error()

       If (mpsd < 0) Then   !Find Cath, Anod given GamM, GamP and NuP
          delga=gamp - gamm
          td=tand(delga)
          tn=tand(nup)

          Select Case(psd%ipsd)

            Case(1)                               ! Vertically curved banana detector
             e=Sqrt(1.0 + td*td)
             f=psd%yoff*tn*e - psd%zoff
             g=psd%radius*Sqrt(1.0 + tn*tn + tn*tn*td*td)
             zobs=psd%radius*( Atan(tn*e) + Asin(f/g) )
             xobs=td*( psd%radius*Cos(zobs/psd%radius) + psd%yoff ) - psd%xoff
             anod=amid - zobs/psd%agap                          ! D19A
             cath=cmid - xobs/psd%cgap                          ! D19A

            Case(2)                                             ! Flat detector
             cd=cosd(delga)                                     ! D9, D10, Db21
             xobs=(psd%radius+psd%yoff)*td    - psd%xoff        ! D16, etc.
             zobs=(psd%radius+psd%yoff)*tn/cd - psd%zoff
             anod=amid + zobs/psd%agap
             cath=cmid - xobs/psd%cgap

            Case(3)                         ! Now checked!
             xobs=(psd%radius + psd%yoff)*delga*to_rad - psd%xoff    ! D19-like Horizontal banana
             zobs=tn*(psd%radius + psd%yoff) - psd%zoff
             anod=amid - zobs/psd%agap
             cath=cmid - xobs/psd%cgap

          End Select

          If (anod > 0.0 .and. anod < real(psd%nano-1) .AND.  &
              cath > 0.0 .and. cath < real(psd%ncat-1)) then
              Err_CFML%ierr=-1
          End if
       Else   ! Find GamP, NuP given GamM, Cath and Anod

          Select Case(psd%ipsd)

            Case(1)                                  ! Vertically curved detector
             xobs=(cmid-cath)*psd%cgap               ! D19A
             zobs=(amid-anod)*psd%agap               ! D19A
             a=xobs                            + psd%xoff
             b=psd%radius*Cos(zobs/psd%radius) + psd%yoff
             z=psd%radius*Sin(zobs/psd%radius) + psd%zoff
             d=Sqrt(a*a + b*b)
             gamp=gamm + atan2d(a,b)
             nup=atan2d(z,d)

            Case(2)                            ! Flat detector
             xobs=  (cmid-cath)*psd%cgap       ! D9, D10, Db21
             zobs= -(amid-anod)*psd%agap       ! D16, etc.
             a=xobs       + psd%xoff
             b=psd%radius + psd%yoff
             z=zobs       + psd%zoff
             d=Sqrt(a*a + b*b)
             gamp=gamm + atan2d(a,b)
             nup=atan2d(z,d)

            Case(3)

              xobs=(cmid-cath)*psd%cgap     ! D19-like Horizontal banana (like in retreat)
              zobs=(amid-anod)*psd%agap     ! Need to check yoff at large xobs
              a= psd%radius*sin(xobs/psd%radius) + psd%xoff
              b= psd%radius*cos(xobs/psd%radius) + psd%yoff
              z=    zobs   + psd%zoff
              gamp=gamm + Atan2d(a,b)
              d=sqrt(a*a + b*b)
              nup=Atan2d(z,d)

          End Select
       End If
    End Subroutine psd_convert_old

    !!----
    !!---- Module Subroutine d19psd(mpsd,ga,nu,cath,anod)
    !!----    Integer, Intent(In Out)            :: mpsd
    !!----    real(kind=cp), Intent(In Out)      :: ga
    !!----    real(kind=cp), Intent(In Out)      :: nu
    !!----    real(kind=cp), Intent(In Out)      :: cath
    !!----    real(kind=cp), Intent(In Out)      :: anod
    !!----
    !!--<<    Original Comment:
    !!----    Specifically for D19A bannana detector, 4 X 64 degrees - 16 X 512
    !!----    cells and vertically curved.                RFD STANSFIELD SEP-83
    !!----
    !!----    MPSD +VE - Calculate delta GAMMA and NU from the cathode, anode
    !!----               co-ordinates
    !!----    MPSD -VE - Calculate the anode co-ordinate from NU
    !!----
    !!----    Some of the variables making reference to the characteristics of the
    !!-->>    detector are provisionally stored in a public type(Psd_Val_Type):: PSD
    !!----
    !!---- Update: April 2008, June 2020
    !!
    Module Subroutine d19psd(mpsd,ga,nu,cath,anod)
       !---- Arguments ----!
       Integer, Intent(In)                :: mpsd
       real(kind=cp), Intent(In Out)      :: ga
       real(kind=cp), Intent(In Out)      :: nu
       real(kind=cp), Intent(In Out)      :: cath
       real(kind=cp), Intent(In Out)      :: anod

       !--- Local Variables ---!
       real(kind=cp) :: nurad,cmid,amid
       real(kind=cp) :: xo,yo,a,b,z,d

       cmid=real(psd%ncat-1)/2.0
       amid=real(psd%nano-1)/2.0
       call clear_error()

       If (mpsd >= 0) Then
          xo=(cmid-cath)*psd%cgap
          yo=(amid-anod)*psd%agap
          a=xo                            + psd%xoff
          b=psd%radius*Cos(yo/psd%radius) + psd%yoff
          z=psd%radius*Sin(yo/psd%radius) + psd%zoff
          d=Sqrt(a*a + b*b)
          ga=ga + atan2d(a,b)
          nu=atan2d(z,d)
       Else
          xo=-psd%xoff
          cath=cmid-xo/psd%cgap

          If (cath > 0.0 .AND. cath < real(psd%ncat-1)) Then
             nurad=nu*To_Rad
             yo=psd%radius*(nurad - Asin((psd%zoff*cosd(nu)-psd%yoff*sind(nu))/psd%radius))
             anod=amid-yo/psd%agap
             If ( .Not. (anod > 0.0 .AND. anod < real(psd%nano-1)) ) Then
                Err_CFML%ierr=-1
                xo=0.0
                yo=0.0
             End If
          Else
             Err_CFML%ierr=-1
             xo=0.0
             yo=0.0
          End If
       End If

    End Subroutine d19psd

    !!----
    !!----  Module Function Get_z1_from_pixel_num(npx,npz,ifr,snum) Result(z1)
    !!----    integer,                intent(in)  :: npx,npz,ifr (pixel and frame)
    !!----    type(SXTAL_Numor_type), intent(in)  :: snum
    !!----    real(kind=cp), dimension(3)         :: z1
    !!----
    !!---- Update: May 2011, February 2019 (JRC), June 2020
    !!
    Module Function Get_z1_from_pixel_num(npx,npz,ifr,snum) Result(z1)
       !---- Arguments ----!
       integer,                intent(in)  :: npx,npz,ifr
       type(SXTAL_Numor_type), intent(in)  :: snum
       real(kind=cp), dimension(3)         :: z1

       !---- Local Variables ----!
       integer        :: mpsd
       real(kind=cp)  :: gamm,gamp,nup,xobs,zobs,cath,anod, wave,chim,phim,omem

       mpsd  = 1        !Find Gamma_Pixel and Nu_Pixel given GamM, Cath and Anod in PSD_Convert
       phim  = snum%angles(1)  !Angles corresponding to hmin,kmin,lmin
       chim  = snum%angles(2)
       omem  = snum%angles(3)
       gamm  = snum%angles(4)
       if(snum%scantype == 'omega') then
          omem  = snum%tmc_ang(4,ifr)
       else if(snum%scantype == 'phi') then
          phim  = snum%tmc_ang(4,ifr)
       else if(snum%scantype == 'q-scan') then  ! Angles: gamma, omega, Chi,phi, psi?
          gamm  = snum%tmc_ang(4,ifr)
          omem  = snum%tmc_ang(5,ifr)
          chim  = snum%tmc_ang(6,ifr)
          phim  = snum%tmc_ang(7,ifr)
       end if

       anod  = npz
       cath  = npx
       wave  = Current_Orient%wave

       ! Find GAMP and NUP for this pixel
       Call Psd_Convert_old(mpsd,gamm,gamp,nup,xobs,zobs,cath,anod)

       ! Find the scattering vector in cartesian coordinates for this pixel
       ! from GAMP, NUP, CHIM, OMEGM and PHIM
       z1= z1frmd(wave,chim,phim,gamp,omem,nup)
    End Function Get_z1_from_pixel_num

    !!----
    !!---- Module Function Get_z1_from_pixel_ang(npx,npz,wave,gamm,omem,chim,phim) Result(z1)
    !!----    integer,         intent(in)  :: npx,npz (pixels )
    !!----    real(kind=cp),   intent(in)  :: wave,gamm,omem,chim,phim
    !!----    real(kind=cp), dimension(3)  :: z1
    !!----
    !!---- Update: Created February 2019 (JRC), June 2020
    !!
    Module Function Get_z1_from_pixel_ang(npx,npz,wave,gamm,omem,chim,phim) Result(z1)
       !---- Arguments ----!
       integer,         intent(in)  :: npx,npz
       real(kind=cp),   intent(in)  :: wave,gamm,omem,chim,phim
       real(kind=cp), dimension(3)  :: z1

       !---- Local Variables ----!
       integer        :: mpsd
       real(kind=cp)  :: gamp,nup,xobs,zobs,cath,anod

       mpsd  = 1        !Find Gamma_Pixel and Nu_Pixel given GamM, Cath and Anod in PSD_Convert
       anod  = npz
       cath  = npx

       ! Find GAMP and NUP for this pixel
       Call Psd_Convert_old(mpsd,gamm,gamp,nup,xobs,zobs,cath,anod)

       ! Find the scattering vector in cartesian coordinates for this pixel
       ! from GAMP, NUP, CHIM, OMEGM and PHIM
       z1 = z1frmd(wave,chim,phim,gamp,omem,nup)
    End Function Get_z1_from_pixel_ang

    !!---
    !!--- Module Subroutine Set_PSD()
    !!---
    !!---- Update: July 2008
    !!
    Module Subroutine Set_PSD(dist,cg,ag,nh,nv,ip)
       real(kind=cp), optional, intent(in) :: dist,cg,ag
       integer,       optional, intent(in) :: nh,nv,ip

       if(present(dist) .and. present(cg) .and. present(ag) .and. present(nh) &
                        .and. present(nv) .and. present(ip)) then
          psd%xoff   = 0.0_cp; psd%yoff=0.0_cp; psd%zoff=0.0_cp
          psd%radius = dist
          psd%cgap   = cg
          psd%agap   = ag
          psd%ncat   = nh
          psd%nano   = nv
          psd%ipsd   = ip
       else
          psd%xoff   = Current_Instrm%det_offsets(1)
          psd%yoff   = Current_Instrm%det_offsets(2)
          psd%zoff   = Current_Instrm%det_offsets(3)
          psd%radius = Current_Instrm%dist_samp_detector
          psd%cgap   = Current_Instrm%cgap
          psd%agap   = Current_Instrm%agap
          psd%ncat   = Current_Instrm%np_horiz
          psd%nano   = Current_Instrm%np_vert
          psd%ipsd   = Current_Instrm%ipsd
       end if
       psd_set    = .true.

    End Subroutine Set_PSD


    !!----
    !!---- Module Subroutine sxdpsd(mpsd,gamm,wave,nup,gamp,xobs,zobs, xcel,tim,zcel)
    !!----    Integer, Intent(In)          :: mpsd
    !!----    real(kind=cp),    Intent(In)          :: gamm
    !!----    real(kind=cp),    Intent(In Out)      :: wave,nup,gamp
    !!----    real(kind=cp),    Intent(Out)         :: xobs,zobs, xcel,tim,zcel
    !!----
    !!--<<    Original comments:
    !!----
    !!----
    !!----    SPECIFICALLY FOR SXD ON SNS AT RAL.       R.F.D STANSFIELD DEC-84
    !!----
    !!----    The coordinate system adopted, whether the origin is at the
    !!----    moderator, at the sample (called the fixed laboratory system),
    !!----    or at the surface of the PSD when positioned at 0 degrees;
    !!----    is Y parallel to the beam, X in the horizontal plane on the
    !!----    diffraction side, and Z vertical. Hence if neutrons are diffracted
    !!----    to the left, Z is vertically down.
    !!----    The PSD is driven to an angle GamM. When GamM=0, the direct
    !!----    beam strikes a perfectly aligned PSD at its centre C.
    !!----    Call this point in space A. A is where we define NuP=0, GamP=0.
    !!----    The coordinates of A with respect to the sample are (0, Distsd, 0);
    !!----    and with respect to the moderator are (0, Distms+Distsd, 0).
    !!----    For a mis-aligned detector, the coordinates of A with respect to
    !!----    C are the translational offsets (Xoff, Yoff, Zoff) in mm.
    !!----    With the PSD at a general position GamM, the point where the
    !!----    direct beam struck it is rotated to O, where now NuP=0, GamP=GamM.
    !!----    For convenience, define a new cartesian system by rotating the
    !!----    axes of the fixed laboratory system about the vertical, such that
    !!----    X is now along the line joining the sample and O on the PSD.
    !!----    In this system, the coordinates of a Bragg peak P with respect to
    !!----    C are (Xobs, 0, Zobs) in mm, or (Xcel, 0, Zcel) in pixels.
    !!----    Hence the coordinates of P with respect to O, in this system, are:
    !!----     (x)     (Xobs   + Xoff)
    !!----     (y)  =  (Distsd + Yoff)  and: Tan(GamP - GamM) = x/y
    !!----     (z)     (Zobs   + Zoff)       Tan(NuP)         = z/SQRT(x*x + y*y)
    !!----    The PSD front surface measures Dimx by Dimy mm, and is divided
    !!----    into Nxcel by Nzcel pixels.
    !!----
    !!----                 -------------------------       At GamM=0 the direct
    !!----                 I          PSD          I       beam hits the detector
    !!----                 I     front surface     I       at A (NuP=0, GamP=0)
    !!----    Gamma________I_______________.O______I_________________________.A
    !!----           Zoff  I               !       I                         !
    !!----        X________I___________.C  !       I                         !
    !!----                 I           !   !       I                         !
    !!----           Zobs  I           !   !       I                         !
    !!----         ________I____.P     !   !       I                         !
    !!----                 I    !      !   !       I                         !
    !!----                 -----!------!---!--------                         !
    !!----                      ! Xobs !Xoff                                 !
    !!----                      !      !   !                                 !
    !!----                             Z  Nu                              GamM=0
    !!----
    !!----    Time is the time coordinate (bin) relative to an elapsed time
    !!----    Toff after the emission of a pulse at the moderator.
    !!----    The effect of moderator thickness on Time is NOT included yet.
    !!----    Distot is the total distance travelled from the moderator to a
    !!----    particular pixel on the PSD surface, in a total time Timtot.
    !!----    Wave = Velcon * Timtot / Distot, where Velcon is the velocity of a
    !!----    1 Angstrom neutron in Km/s or mm/mms.
    !!----
    !!----    MPSD +VE -> Find Wave, NuP, GamP; given GamM, Xcel, Zcel and Time
    !!----    MPSD -VE -> Find Xcel, Zcel, Time; given Wave, NuP, GamP and GamM
    !!----    (Routine not tested, probably obsolete for present SXD!!!)
    !!-->>
    !!----
    !!---- Update: April 2008
    !!
    Module Subroutine sxdpsd(mpsd,gamm,wave,nup,gamp,xobs,zobs, xcel,tim,zcel)
       !---- Arguments ----!
       Integer, Intent(In)                   :: mpsd
       real(kind=cp),    Intent(In)          :: gamm
       real(kind=cp),    Intent(In Out)      :: wave,nup,gamp
       real(kind=cp),    Intent(Out)         :: xobs,zobs,xcel,tim,zcel

       !--- Local variables ---!
       real(kind=cp) :: xmax,zmax, a, b, c, d, e, timtot, distot, delga, td, f, tn

       call clear_error()
       xmax=real(sxd%nxcel/2)
       zmax=real(sxd%nzcel/2)
       If (mpsd < 0) Then
          !---- Xcel and Zcel are the coords. in pixels of P from C, on the PSD
          !---- NuP, GamP are the angular coordinates of P from the sample
          delga=gamp - gamm
          If (Abs(delga) >= 90.0) then
             Err_CFML%ierr=-1
             return
          End if
          b=sxd%distsd+sxd%yoff
          td=tand(delga)
          a=b*td
          xobs=a - sxd%xoff
          xcel=xobs*real(sxd%nxcel)/sxd%dimx
          f=Sqrt(1.0 + td*td)
          tn=tand(nup)
          c=b*f*tn
          zobs=c -sxd%zoff
          zcel=zobs*real(sxd%nzcel)/sxd%dimz
          If (Abs(xcel) > xmax .or. Abs(zcel) > zmax) then
             Err_CFML%ierr=-1
             return
          End If
          e=Sqrt(a*a + b*b + c*c)
          distot=sxd%distms+e
          timtot=wave*distot/sxd%velcon
          tim=timtot-sxd%toff
       Else
          If (Abs(xcel) > xmax.or.abs(zcel) > zmax) Then
             Err_CFML%ierr=-1
             return
          End If

          !---- Xobs and Zobs are the coords. in mm of P from C on the PSD surface
          !---- (A,B,C) are the coordinates of P from the sample in the lab system

          xobs=xcel*sxd%dimx/real(sxd%nxcel)
          zobs=zcel*sxd%dimz/real(sxd%nzcel)
          a=xobs  +sxd%xoff
          b=sxd%distsd+sxd%yoff
          c=zobs  +sxd%zoff
          d=Sqrt(a*a + b*b)
          e=Sqrt(c*c + d*d)
          gamp=gamm + atan2d(a,b)
          nup=atan2d(c,d)
          timtot=tim+sxd%toff
          distot=sxd%distms+e
          wave=sxd%velcon*timtot/distot
       End If
    End Subroutine sxdpsd

 End SubModule SXTAL_PSD
