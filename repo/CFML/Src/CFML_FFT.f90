!!-------------------------------------------------------
!!---- Crystallographic Fortran Modules Library (CrysFML)
!!-------------------------------------------------------
!!---- The CrysFML project is distributed under LGPL. In agreement with the
!!---- Intergovernmental Convention of the ILL, this software cannot be used
!!---- in military applications.
!!----
!!---- Copyright (C) 1999-2022  Institut Laue-Langevin (ILL), Grenoble, FRANCE
!!----                          Universidad de La Laguna (ULL), Tenerife, SPAIN
!!----                          Laboratoire Leon Brillouin(LLB), Saclay, FRANCE
!!----
!!---- Authors: Juan Rodriguez-Carvajal (ILL)
!!----          Javier Gonzalez-Platas  (ULL)
!!----          Nebil Ayape Katcho      (ILL)
!!----
!!---- Contributors: Laurent Chapon     (ILL)
!!----               Marc Janoschek     (Los Alamos National Laboratory, USA)
!!----               Oksana Zaharko     (Paul Scherrer Institute, Switzerland)
!!----               Tierry Roisnel     (CDIFX,Rennes France)
!!----               Eric Pellegrini    (ILL)
!!----               Ross Angel         (University of Pavia)
!!----
!!---- This library is free software; you can redistribute it and/or
!!---- modify it under the terms of the GNU Lesser General Public
!!---- License as published by the Free Software Foundation; either
!!---- version 3.0 of the License, or (at your option) any later version.
!!----
!!---- This library is distributed in the hope that it will be useful,
!!---- but WITHOUT ANY WARRANTY; without even the implied warranty of
!!---- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!!---- Lesser General Public License for more details.
!!----
!!---- You should have received a copy of the GNU Lesser General Public
!!---- License along with this library; if not, see <http://www.gnu.org/licenses/>.
!!----
!!----
!!---- MODULE: CFML_FFT
!!----   INFO: FFT Calculations Routines
!!----
!!--..
!!--..    Multivariate Fast Fourier Transform
!!--..    Fortran 90 (ELF90) Implementation of Singleton's mixed-radix
!!--..    algorithm, RC Singleton, Stanford Research Institute, Sept. 1968.
!!--..    Adapted from fftn.c, translated from Fortran 66 to C by Mark Olesen
!!--..    and John Beale.
!!--..    Fourier transforms can be computed either in place, using assumed
!!--..    size arguments, or by generic function, using assumed shape arguments.
!!--..
!!--..    FFT(array, dim, inv)                 generic transform function
!!--..    COMPLEX(fftkind), DIMENSION(:,...,:), INTENT(IN)           :: array
!!--..    INTEGER,          DIMENSION(:),       INTENT(IN),  OPTIONAL:: dim
!!--..    LOGICAL,                              INTENT(IN),  OPTIONAL:: inv
!!--..
!!--..    Formal Parameters:
!!--..
!!--..    array    The complex array to be transformed. array can be of arbitrary
!!--..             rank (i.e. up to seven).
!!--..
!!--..    shape    With subroutine fftn, the shape of the array to be transformed
!!--..             has to be passed separately, since fftradix - the internal trans-
!!--..             formation routine - will treat array always as one dimensional.
!!--..             The product of elements in shape must be the number of
!!--..             elements in array.
!!--..             Although passing array with assumed shape would have been nicer,
!!--..             I prefered assumed size in order to prevent the compiler from
!!--..             using a copy-in-copy-out mechanism. That would generally be
!!--..             necessary with fftn passing array to fftradix and with fftn
!!--..             being prepared for accepting non consecutive array sections.
!!--..             Using assumed size, it's up to the user to pass an array argu-
!!--..             ment, that can be addressed as continous one dimensional array
!!--..             without copying. Otherwise, transformation will not really be
!!--..             performed in place.
!!--..             On the other hand, since the rank of array and the size of
!!--..             shape needn't match, fftn is appropriate for handling more than
!!--..             seven dimensions.
!!--..             As far as function fft is concerned all this doesn't matter,
!!--..             because the argument will be copied anyway. Thus no extra
!!--..             shape argument is needed for fft.
!!--..
!!--..    Optional Parameters:
!!--..
!!--..    dim      One dimensional integer array, containing the dimensions to be
!!--..             transformed. Default is [1,...,N] with N being the rank of
!!--..             array, i.e. complete transform. dim can restrict transformation
!!--..             to a subset of available dimensions. Its size must not exceed the
!!--..             rank of array or the size of shape respectivly.
!!--..
!!--..    inv      If .true., inverse transformation will be performed. Default is
!!--..             .false., i.e. forward transformation.
!!--..
!!--..    stat     If present, a system dependent nonzero status value will be
!!--..             returned in stat, if allocation of temporary storage failed.
!!--..             For functions, the integer variable status is used.
!!--..
!!--..    Scaling:
!!--..             Transformation results will always be scaled by the square
!!--..             root of the product of sizes of each dimension in dim.
!!--..             (See examples below)
!!--..
!!--..    Examples:
!!--..             Let A be a L*M*N three dimensional complex array. Then
!!--..             result = fft(A) will produce a three dimensional transform,
!!--..             scaled by sqrt(L*M*N), while call fftn(A, SHAPE(A)) will do
!!--..             the same in place.
!!--..
!!--..             result = fft(A, dim=[1,3]) will transform with respect to
!!--..             the first and the third dimension, scaled by sqrt(L*N).
!!--..
!!--..             result = fft(fft(A), inv=.true.) should (approximately)
!!--..             reproduce A. With B having the same shape as A
!!--..
!!--..             result = fft(fft(A) * CONJG(fft(B)), inv=.true.) will
!!--..             correlate A and B.
!!--..
!!--..     Remarks:
!!--..
!!--..             Following changes have been introduced with respect to fftn.c:
!!--..             - complex arguments and results are of type complex, rather
!!--..               than real an imaginary part separately.
!!--..             - increment parameter (magnitude of isign) has been dropped,
!!--..               inc is always one, direction of transform is given by inv.
!!--..             - maxf and maxp have been dropped. The amount of temporary
!!--..               storage needed is determined by the fftradix routine.
!!--..               Both fftn and fft can handle any size of array. (Maybe they
!!--..               take a lot of time and memory, but they will do it)
!!--..
!!--..             Redesigning fftradix in a way, that it handles assumed shape
!!--..             arrays would have been desirable. However, I found it rather
!!--..             hard to do this in an efficient way. Problems were:
!!--..             - to prevent stride multiplications when indexing arrays. At
!!--..               least our compiler was not clever enough to discover that
!!--..               in fact additions would do the job as well. On the other
!!--..               hand, I haven't been clever enough to find an implementation
!!--..               using array operations.
!!--..             - fftradix is rather large and different versions would be
!!--..               necessaray for each possible rank of array.
!!--..             Consequently, in place transformation still needs the
!!--..             argument stored in a consecutive bunch of memory and can't be
!!--..             performed on array sections like A(100:199:-3, 50:1020).
!!--..             Calling fftn with such sections will most probably imply
!!--..             copy-in-copy-out. However, the function fft works with
!!--..             everything it gets and should be convenient to use.
!!--..
!!--..             To enable this module to be used with ELF90 it appears to be
!!--..             necessary to allocate a 1-D work array into which the
!!--..             multi-dimensional array is copied, and then to copy the
!!--..             results back from the 1-D array to the multi-dimensional
!!--..             array ft.
!!--..
!!--..             Unfortunately, ELF90 will not allow a function to return more
!!--..             than one output variable.   The variable `stat' has been
!!--..             dropped from the function arguments. Users should examine the
!!--..             value of the variable `StatusF' instead. This is a PUBLIC
!!--..             variable declared in this module.
!!--..
!!--..             Michael Steffens, 09.12.96,
!!--..             <Michael.Steffens@mbox.muk.uni-hannover.de>
!!--..
!!--..             ELF90-compatible version by Alan Miller, 29 April 1997
!!--..             Alan.Miller @ mel.dms.csiro.au
!!--..
!!----
!!
Module CFML_FFT
   !---- Use Modules ----!
   use CFML_GlobalDeps, only : CP, DP, ERR_CFML

   !---- Local Variables ----!
   implicit none

   private

   !---- List of public functions ----!
   public :: Convol, Convol_Peaks, direct_convol, &
             FFT, F_FFT

   !---- List of public subroutines ----!
   public :: SFFT, HFFT

   !---- Definitions ----!

   !--------------------!
   !---- PARAMETERS ----!
   !--------------------!
   integer, parameter:: FFTKIND = Kind(0.0_cp)                             ! Default Precicion for FFT Variables.
                                                                           ! Adjust Here For Other Precisions

   real(kind=FFTKIND), parameter:: Cos72 = 0.30901699437494742_Fftkind  ! COS72
   real(kind=FFTKIND), parameter:: Pi    = 3.14159265358979323_Fftkind  ! Pi
   real(kind=FFTKIND), parameter:: Sin60 = 0.86602540378443865_Fftkind  ! SIN60
   real(kind=FFTKIND), parameter:: Sin72 = 0.95105651629515357_Fftkind  ! SIN72


   !-------------------!
   !---- VARIABLES ----!
   !-------------------!
   !integer, save :: StatusF                  ! Information on FFT Routines


   !!----
   !!---- TYPE, public :: Points_Interval_Type
   !!----
   Type, public :: Points_Interval_Type
      integer       :: Np                      ! Number of Point
      real(kind=cp) :: Low                     ! Lower range value
      real(kind=cp) :: High                    ! Upper range value
   End Type Points_Interval_Type

   !---- Overlapp ----!
   Interface Fft
      Module Procedure Fft1D
      Module Procedure Fft2D
      Module Procedure Fft3D
      Module Procedure Fft4D
      Module Procedure Fft5D
      Module Procedure Fft6D
      Module Procedure Fft7D
   End Interface

   !---- General Procedures ----!
   Interface
      Pure Module Function F_FFT(Array, Mode ) Result(fft)
         !---- Arguments ----!
         complex(kind=cp), dimension(:),      intent(in) :: Array  !  In -> real array containing real parts of transform
         character(len=*),          optional, intent(in) :: Mode   !  In -> type="INV" : backward transform. Absent or whatever -> forward transform
         complex(kind=cp), dimension(size(Array))        :: fft
      End Function F_FFT

      Module Function Fft1D(Array, Dim, Inv) Result(Ft)
         !--- formal parameters
         complex(fftkind), dimension(:), intent(in)             :: array     ! In -> Complex array
         integer,          dimension(:), intent(in),  optional  :: dim       ! In -> array containing the dimensions to be transformed
         logical,                        intent(in),  optional  :: inv       ! In -> If .true., inverse transformation will be performed.
         complex(fftkind), dimension(size(array, 1))            :: ft
      End Function Fft1D

      Module Function Fft2D(Array, Dim, Inv) Result(Ft)
         !--- formal parameters
         complex(fftkind), dimension(:,:), intent(in)                :: array         ! In -> Complex array
         integer,          dimension(:),   intent(in),  optional     :: dim           ! In -> array containing the dimensions to be transformed
         logical,                          intent(in),  optional     :: inv
         complex(fftkind), dimension(size(array, 1), size(array, 2)) :: ft
      End Function Fft2D

      Module Function Fft3D(Array, Dim, Inv) Result(Ft)
         !--- formal parameters
         complex(fftkind), dimension(:,:,:), intent(in)               :: array        ! In -> Complex array
         integer,          dimension(:),     intent(in),  optional    :: dim          ! In -> array containing the dimensions to be transformed
         logical,                            intent(in),  optional    :: inv
         complex(fftkind), &
            dimension(size(array, 1), size(array, 2), size(array, 3)) :: ft
      End Function Fft3D

      Module Function Fft4D(Array, Dim, Inv) Result(Ft)
         !--- formal parameters
         complex(fftkind), dimension(:,:,:,:), intent(in)                   :: array         ! In -> Complex array
         integer,          dimension(:),       intent(in),  optional        :: dim           ! In -> array containing the dimensions to be transformed
         logical,                              intent(in),  optional        :: inv
         complex(fftkind), dimension( &
            size(array, 1), size(array, 2), size(array, 3), size(array, 4)) :: ft
      End Function Fft4D

      Module Function Fft5D(Array, Dim, Inv) Result(Ft)
         !--- formal parameters
         complex(fftkind), dimension(:,:,:,:,:), intent(in)                   :: array           ! In -> Complex array
         integer,          dimension(:),         intent(in),  optional        :: dim             ! In -> array containing the dimensions to be transformed
         logical,                                intent(in),  optional        :: inv
         complex(fftkind), dimension( &
            size(array, 1), size(array, 2), size(array, 3), size(array, 4), &
            size(array, 5))                                                   :: ft
      End Function Fft5D

      Module Function Fft6D(Array, Dim, Inv) Result(Ft)
         !--- formal parameters
         complex(fftkind), dimension(:,:,:,:,:,:), intent(in)                 :: array         ! In -> Complex array
         integer,          dimension(:),           intent(in),  optional      :: dim           ! In -> array containing the dimensions to be transformed
         logical,                                  intent(in),  optional      :: inv
         complex(fftkind), dimension( &
            size(array, 1), size(array, 2), size(array, 3), size(array, 4), &
            size(array, 5), size(array, 6))                                   :: ft
      End Function Fft6D

      Module Function Fft7D(Array, Dim, Inv) Result(Ft)
         !--- formal parameters
         complex(fftkind), dimension(:,:,:,:,:,:,:), intent(in)               :: array      ! In -> Complex array
         integer,          dimension(:),             intent(in),  optional    :: dim        ! In -> array containing the dimensions to be transformed
         logical,                                    intent(in),  optional    :: inv
         complex(fftkind), dimension( &
            size(array, 1), size(array, 2), size(array, 3), size(array, 4), &
            size(array, 5), size(array, 6), size(array, 7))                   :: ft
      End Function Fft7D

      Module Subroutine Fftn(Array, Shape, Dim, Inv, Stat)
         !--- formal parameters
         complex(fftkind), dimension(:), intent(in out)       :: array
         integer,          dimension(:), intent(in)           :: shape
         integer,          dimension(:), intent(in),  optional:: dim
         logical,                        intent(in),  optional:: inv
         integer,                        intent(out), optional:: stat
      End Subroutine Fftn

      Pure Module Subroutine Hfft(Array,Ifset,Iferr)
         !---- Arguments ----!
         complex(kind=cp), dimension(0:,0:,0:), intent(in out) :: Array       ! In -> Contains the complex 3D array to be transformed
         integer,                               intent(in    ) :: IfSet       ! In -> 1,2 Inverse Fourier Transform; -1,-2 Fourier Transform
         integer,                               intent(   out) :: IFerr       ! Out -> Flags to error. 0 No error
      End Subroutine Hfft

      Pure Module Subroutine Sfft( Array, Mode,Iferr )
         !---- Arguments ----!
         complex(kind=cp), dimension(:),      intent(in out) :: Array   ! In -> real array containing real parts of transform
         character(len=*), optional,          intent(in)     :: Mode    ! In -> type="INV" : backward transform; Absent or whatever : forward transform
         integer,          optional,          intent(   out) :: Iferr   ! Out -> Flags to error. 0 No error
      End Subroutine Sfft

   End Interface

 Contains
    !!--++
    !!--++ Subroutine Fftradix
    !!--++
    !!--++    (PRIVATE)
    !!--++
    !!--++ Update: February - 2005
    !!
    Subroutine Fftradix(Array, Ntotal, Npass, Nspan, Inv, Stat)
       !--- formal parameters
       integer,                        intent(in)           :: ntotal, npass, nspan
       complex(fftkind), dimension(:), intent(in out)       :: array
       logical,                        intent(in)           :: inv
       integer,                        intent(out), optional:: stat

       !--- local arrays
       integer,          dimension(bit_size(0))     :: factor
       complex(fftkind), dimension(:), allocatable  :: ctmp
       real(fftkind),    dimension(:), allocatable  :: sine, cosine
       integer,          dimension(:), allocatable  :: perm

       !--- local scalars
       integer         :: ii, kspan, ispan
       integer         :: j, jc, jf, jj, k, k1, k2, k3, k4, kk, kt, nn, ns, nt
       integer         :: maxfactor, nfactor, nperm
       real(fftkind)   :: s60, c72, s72, pi2
       real(fftkind)   :: radf
       real(fftkind)   :: c1, c2, c3, cd, ak
       real(fftkind)   :: s1, s2, s3, sd
       complex(fftkind):: cc, cj, ck, cjp, cjm, ckp, ckm

       if (npass <= 1) return
       c72 = cos72
       if (inv) then
          s72 = sin72
          s60 = sin60
          pi2 = pi
       else
          s72 = -sin72
          s60 = -sin60
          pi2 = -pi
       end if
       nt = ntotal
       ns = nspan
       kspan = ns
       nn = nt - 1
       jc = ns / npass
       radf = pi2 * jc
       pi2 = pi2 * 2.0_fftkind !-- use 2 pi from here on
       call factorize()
       maxfactor = maxval(factor (:nfactor))
       if (nfactor - ishft(kt, 1) > 0) then
          nperm = max(nfactor + 1, product(factor(kt+1: nfactor-kt)) - 1)
       else
          nperm = nfactor + 1
       end if
       if (present(stat)) then
          allocate(ctmp(maxfactor), sine(maxfactor), cosine(maxfactor), stat=stat)
          if (stat /= 0) return
          call transform()
          deallocate(sine, cosine, stat=stat)
          if (stat /= 0) return
          allocate(perm(nperm), stat=stat)
          if (stat /= 0) return
          call permute()
          deallocate(perm, ctmp, stat=stat)
          if (stat /= 0) return
       else
          allocate(ctmp(maxfactor), sine(maxfactor), cosine(maxfactor))
          call transform()
          deallocate(sine, cosine)
          allocate(perm(nperm))
          call permute()
          deallocate(perm, ctmp)
       end if

    contains

       !!--++
       !!--++ subroutine factorize
       !!--++
       !!--++    (private)
       !!--++
       !!--++ update: february - 2005
       !!
       Subroutine factorize()

          nfactor = 0
          k = npass
          do while (mod(k, 16) == 0)
             nfactor = nfactor + 1
             factor (nfactor) = 4
             k = k / 16
          end do

          j = 3
          jj = 9
          do
             do while (mod(k, jj) == 0)
                nfactor=nfactor + 1
                factor (nfactor) = j
                k = k / jj
             end do
             j = j + 2
             jj = j * j
             if (jj > k) exit
          end do

          if (k <= 4) then
             kt = nfactor
             factor (nfactor + 1) = k
             if (k /= 1) nfactor = nfactor + 1
          else
             if (k - ishft(k / 4, 2) == 0) then
                nfactor = nfactor + 1
                factor (nfactor) = 2
                k = k / 4
             end if
             kt = nfactor
             j = 2
             do
                if (mod(k, j) == 0) then
                   nfactor = nfactor + 1
                   factor (nfactor) = j
                   k = k / j
                end if
                j = ishft((j + 1)/2, 1) + 1
                if (j > k) exit
             end do
          end if

          if (kt > 0) then
             j = kt
             do
                nfactor = nfactor + 1
                factor (nfactor) = factor (j)
                j = j - 1
                if (j==0) exit
             end do
          end if

       End Subroutine factorize

       !!--++
       !!--++ subroutine transform
       !!--++
       !!--++    compute fourier transform
       !!--++    (private)
       !!--++
       !!--++ update: february - 2005
       !!
       Subroutine transform()

          ii = 0
          jf = 0
          do
             sd = radf / kspan
             cd = sin(sd)
             cd = 2.0_fftkind * cd * cd
             sd = sin(sd + sd)
             kk = 1
             ii = ii + 1
             select case (factor (ii))
                case (2)
                   !-- transform for factor of 2 (including rotation factor)
                   kspan = kspan / 2
                   k1 = kspan + 2
                   do
                      do
                         k2 = kk + kspan
                         ck = array(k2)
                         array(k2) = array(kk) - ck
                         array(kk) = array(kk) + ck
                         kk = k2 + kspan
                         if (kk > nn) exit
                      end do
                      kk = kk - nn
                      if (kk > jc) exit
                   end do

                   if (kk > kspan) return
                   do
                      c1 = 1.0_fftkind - cd
                      s1 = sd
                      do
                         do
                            do
                               k2 = kk + kspan
                               ck = array(kk) - array(k2)
                               array(kk) = array(kk) + array(k2)
                               array(k2) = ck * cmplx(c1, s1, kind=fftkind)
                               kk = k2 + kspan
                               if (kk >= nt) exit
                            end do
                            k2 = kk - nt
                            c1 = -c1
                            kk = k1 - k2
                            if (kk <= k2) exit
                         end do
                         ak = c1 - (cd * c1 + sd * s1)
                         s1 = sd * c1 - cd * s1 + s1
                         c1 = 2.0_fftkind - (ak * ak + s1 * s1)
                         s1 = s1 * c1
                         c1 = c1 * ak
                         kk = kk + jc
                         if (kk >= k2) exit
                      end do

                      k1 = k1 + 1 + 1
                      kk = (k1 - kspan) / 2 + jc
                      if (kk > jc + jc) exit
                   end do

                case (4) !-- transform for factor of 4
                   ispan = kspan
                   kspan = kspan / 4
                   do
                      c1 = 1.0_fftkind
                      s1 = 0.0_fftkind
                      do
                         do
                            k1 = kk + kspan
                            k2 = k1 + kspan
                            k3 = k2 + kspan
                            ckp = array(kk) + array(k2)
                            ckm = array(kk) - array(k2)
                            cjp = array(k1) + array(k3)
                            cjm = array(k1) - array(k3)
                            array(kk) = ckp + cjp
                            cjp = ckp - cjp
                            if (inv) then
                               ckp = ckm + cmplx(-aimag(cjm), real(cjm), kind=fftkind)
                               ckm = ckm + cmplx(aimag(cjm), -real(cjm), kind=fftkind)
                            else
                               ckp = ckm + cmplx(aimag(cjm), -real(cjm), kind=fftkind)
                               ckm = ckm + cmplx(-aimag(cjm), real(cjm), kind=fftkind)
                            end if
                            !-- avoid useless multiplies
                            if (s1 == 0.0_fftkind) then
                               array(k1) = ckp
                               array(k2) = cjp
                               array(k3) = ckm
                            else
                               array(k1) = ckp * cmplx(c1, s1, kind=fftkind)
                               array(k2) = cjp * cmplx(c2, s2, kind=fftkind)
                               array(k3) = ckm * cmplx(c3, s3, kind=fftkind)
                            end if
                            kk = k3 + kspan
                            if (kk > nt) exit
                         end do
                         c2 = c1 - (cd * c1 + sd * s1)
                         s1 = sd * c1 - cd * s1 + s1
                         c1 = 2.0_fftkind - (c2 * c2 + s1 * s1)
                         s1 = s1 * c1
                         c1 = c1 * c2
                         !-- values of c2, c3, s2, s3 that will get used next time
                         c2 = c1 * c1 - s1 * s1
                         s2 = 2.0_fftkind * c1 * s1
                         c3 = c2 * c1 - s2 * s1
                         s3 = c2 * s1 + s2 * c1
                         kk = kk - nt + jc
                         if (kk > kspan) exit
                      end do
                      kk = kk - kspan + 1
                      if (kk > jc) exit
                   end do
                   if (kspan == jc) return

                case default
                   !-- transform for odd factors
                   k = factor (ii)
                   ispan = kspan
                   kspan = kspan / k
                   select case (k)
                      case (3) !-- transform for factor of 3 (optional code)
                         do
                            do
                               k1 = kk + kspan
                               k2 = k1 + kspan
                               ck = array(kk)
                               cj = array(k1) + array(k2)
                               array(kk) = ck + cj
                               ck = ck - 0.5_fftkind * cj
                               cj = (array(k1) - array(k2)) * s60
                               array(k1) = ck + cmplx(-aimag(cj), real(cj), kind=fftkind)
                               array(k2) = ck + cmplx(aimag(cj), -real(cj), kind=fftkind)
                               kk = k2 + kspan
                               if (kk >= nn) exit
                            end do
                            kk = kk - nn
                            if (kk > kspan) exit
                         end do

                      case (5) !-- transform for factor of 5 (optional code)
                         c2 = c72 * c72 - s72 * s72
                         s2 = 2.0_fftkind * c72 * s72
                         do
                            do
                               k1 = kk + kspan
                               k2 = k1 + kspan
                               k3 = k2 + kspan
                               k4 = k3 + kspan
                               ckp = array(k1) + array(k4)
                               ckm = array(k1) - array(k4)
                               cjp = array(k2) + array(k3)
                               cjm = array(k2) - array(k3)
                               cc = array(kk)
                               array(kk) = cc + ckp + cjp
                               ck = ckp * c72 + cjp * c2 + cc
                               cj = ckm * s72 + cjm * s2
                               array(k1) = ck + cmplx(-aimag(cj), real(cj), kind=fftkind)
                               array(k4) = ck + cmplx(aimag(cj), -real(cj), kind=fftkind)
                               ck = ckp * c2 + cjp * c72 + cc
                               cj = ckm * s2 - cjm * s72
                               array(k2) = ck + cmplx(-aimag(cj), real(cj), kind=fftkind)
                               array(k3) = ck + cmplx(aimag(cj), -real(cj), kind=fftkind)
                               kk = k4 + kspan
                               if (kk >= nn) exit
                            end do
                            kk = kk - nn
                            if (kk > kspan) exit
                         end do

                      case default
                         if (k /= jf) then
                            jf = k
                            s1 = pi2 / k
                            c1 = cos(s1)
                            s1 = sin(s1)
                            cosine (jf) = 1.0_fftkind
                            sine (jf) = 0.0_fftkind
                            j = 1
                            do
                               cosine (j) = cosine (k) * c1 + sine (k) * s1
                               sine (j) = cosine (k) * s1 - sine (k) * c1
                               k = k-1
                               cosine (k) = cosine (j)
                               sine (k) = -sine (j)
                               j = j + 1
                               if (j >= k) exit
                            end do
                         end if

                         do
                            do
                               k1 = kk
                               k2 = kk + ispan
                               cc = array(kk)
                               ck = cc
                               j = 1
                               k1 = k1 + kspan
                               do
                                  k2 = k2 - kspan
                                  j = j + 1
                                  ctmp(j) = array(k1) + array(k2)
                                  ck = ck + ctmp(j)
                                  j = j + 1
                                  ctmp(j) = array(k1) - array(k2)
                                  k1 = k1 + kspan
                                  if (k1 >= k2) exit
                               end do
                               array(kk) = ck
                               k1 = kk
                               k2 = kk + ispan
                               j = 1
                               do
                                  k1 = k1 + kspan
                                  k2 = k2 - kspan
                                  jj = j
                                  ck = cc
                                  cj = (0.0_fftkind, 0.0_fftkind)
                                  k = 1
                                  do
                                     k = k + 1
                                     ck = ck + ctmp(k) * cosine (jj)
                                     k = k + 1
                                     cj = cj + ctmp(k) * sine (jj)
                                     jj = jj + j
                                     if (jj > jf) jj = jj - jf
                                     if (k >= jf) exit
                                  end do
                                  k = jf - j
                                  array(k1) = ck + cmplx(-aimag(cj), real(cj), kind=fftkind)
                                  array(k2) = ck + cmplx(aimag(cj), -real(cj), kind=fftkind)
                                  j = j + 1
                                  if (j >= k) exit
                               end do
                               kk = kk + ispan
                               if (kk > nn) exit
                            end do
                            kk = kk - nn
                            if (kk > kspan) exit
                         end do
                   end select !k

                   !--  multiply by rotation factor (except for factors of 2 and 4)
                   if (ii == nfactor) return
                   kk = jc + 1
                   do
                      c2 = 1.0_fftkind - cd
                      s1 = sd
                      do
                         c1 = c2
                         s2 = s1
                         kk = kk + kspan
                         do
                            do
                               array(kk) = cmplx(c2, s2, kind=fftkind) * array(kk)
                               kk = kk + ispan
                               if (kk > nt) exit
                            end do
                            ak = s1 * s2
                            s2 = s1 * c2 + c1 * s2
                            c2 = c1 * c2 - ak
                            kk = kk - nt + kspan
                            if (kk > ispan) exit
                         end do
                         c2 = c1 - (cd * c1 + sd * s1)
                         s1 = s1 + sd * c1 - cd * s1
                         c1 = 2.0_fftkind - (c2 * c2 + s1 * s1)
                         s1 = s1 * c1
                         c2 = c2 * c1
                         kk = kk - ispan + jc
                         if (kk > kspan) exit
                      end do
                      kk = kk - kspan + jc + 1
                      if (kk > jc + jc) exit
                   end do
             end select ! factor
          end do

       End Subroutine transform

       !!--++
       !!--++ subroutine permute
       !!--++
       !!--++    permute the results to normal order---done in two stages
       !!--++    permutation for square factors of n
       !!--++
       !!--++    (private)
       !!--++
       !!--++ update: february - 2005
       !!
       Subroutine permute()

          perm (1) = ns
          if (kt > 0) then
             k = kt + kt + 1
             if (nfactor < k) k = k - 1
             j = 1
             perm (k + 1) = jc
             do
                perm (j + 1) = perm (j) / factor (j)
                perm (k) = perm (k + 1) * factor (j)
                j = j + 1
                k = k - 1
                if (j >= k) exit
             end do
             k3 = perm (k + 1)
             kspan = perm (2)
             kk = jc + 1
             k2 = kspan + 1
             j = 1
             if (npass /= ntotal) then
                permute_multi: do
                   do
                      do
                         k = kk + jc
                         do
                            !-- swap array(kk) <> array(k2)
                            ck = array(kk)
                            array(kk) = array(k2)
                            array(k2) = ck
                            kk = kk + 1
                            k2 = k2 + 1
                            if (kk >= k) exit
                         end do
                         kk = kk + ns - jc
                         k2 = k2 + ns - jc
                         if (kk >= nt) exit
                      end do
                      kk = kk - nt + jc
                      k2 = k2 - nt + kspan
                      if (k2 >= ns) exit
                   end do

                   do
                      do
                         k2 = k2 - perm (j)
                         j = j + 1
                         k2 = perm (j + 1) + k2
                         if (k2 <= perm (j)) exit
                      end do
                      j = 1

                      do
                         if (kk < k2) cycle permute_multi
                         kk = kk + jc
                         k2 = k2 + kspan
                         if (k2 >= ns) exit
                      end do
                      if (kk >= ns) exit
                   end do

                   exit
                end do permute_multi
             else
                permute_single: do
                   do
                      !-- swap array(kk) <> array(k2)
                      ck = array(kk)
                      array(kk) = array(k2)
                      array(k2) = ck
                      kk = kk + 1
                      k2 = k2 + kspan
                      if (k2 >= ns) exit
                   end do

                   do
                      do
                         k2 = k2 - perm (j)
                         j = j + 1
                         k2 = perm (j + 1) + k2
                         if (k2 <= perm (j)) exit
                      end do
                      j = 1
                      do
                         if (kk < k2) cycle permute_single
                         kk = kk + 1
                         k2 = k2 + kspan
                         if (k2 >= ns) exit
                      end do
                      if (kk >= ns) exit
                   end do
                   exit
                end do permute_single
             end if
             jc = k3
          end if

          if (ishft(kt, 1) + 1 >= nfactor) return
          ispan = perm (kt + 1)

          !-- permutation for square-free factors of n
          j = nfactor - kt
          factor (j + 1) = 1
          do
             factor(j) = factor(j) * factor(j+1)
             j = j - 1
             if (j == kt) exit
          end do
          kt = kt + 1
          nn = factor(kt) - 1
          j = 0
          jj = 0
          do
             k = kt + 1
             k2 = factor(kt)
             kk = factor(k)
             j = j + 1
             if (j > nn) exit !-- exit infinite loop
             jj = jj + kk
             do while (jj >= k2)
                jj = jj - k2
                k2 = kk
                k = k + 1
                kk = factor(k)
                jj = jj + kk
             end do
             perm (j) = jj
          end do

          !--  determine the permutation cycles of length greater than 1
          j = 0
          do
             do
                j = j + 1
                kk = perm(j)
                if (kk >= 0) exit
             end do
             if (kk /= j) then
                do
                   k = kk
                   kk = perm (k)
                   perm (k) = -kk
                   if (kk == j) exit
                end do
                k3 = kk
             else
                perm (j) = -j
                if (j == nn) exit !-- exit infinite loop
             end if
          end do

          !--  reorder a and b, following the permutation cycles
          do
             j = k3 + 1
             nt = nt - ispan
             ii = nt - 1 + 1
             if (nt < 0) exit !-- exit infinite loop
             do
                do
                   j = j-1
                   if (perm(j) >= 0) exit
                end do
                jj = jc

                do
                   kspan = jj
                   if (jj > maxfactor) kspan = maxfactor
                   jj = jj - kspan
                   k = perm(j)
                   kk = jc * k + ii + jj
                   k1 = kk + kspan
                   k2 = 0
                   do
                      k2 = k2 + 1
                      ctmp(k2) = array(k1)
                      k1 = k1 - 1
                      if (k1 == kk) exit
                   end do
                   do
                      k1 = kk + kspan
                      k2 = k1 - jc * (k + perm(k))
                      k = -perm(k)
                      do
                         array(k1) = array(k2)
                         k1 = k1 - 1
                         k2 = k2 - 1
                         if (k1 == kk) exit
                      end do
                      kk = k2
                      if (k == j) exit
                   end do
                   k1 = kk + kspan
                   k2 = 0
                   do
                      k2 = k2 + 1
                      array(k1) = ctmp(k2)
                      k1 = k1 - 1
                      if (k1 == kk) exit
                   end do
                   if (jj == 0) exit
                end do
                if (j == 1) exit
             end do
          end do

       End Subroutine permute

    End Subroutine fftradix

    !!----
    !!---- convol
    !!----
    !!--<<   convolution of the user-provided centred (x=0) peak functions f and g.
    !!----
    !!----   the characteristic parameters of the functions f and g are provided in vectors pf and pg.
    !!----   the intent-in points_interval_type variable "interval" gives the number of points and the
    !!----   limits of the interval
    !!----         number of points:  interval%np
    !!----     range of calculation: [ interval%low, interval%high ]
    !!----                    step : (interval%high-interval%low)/(interval%np-1)
    !!----   the convolution function is normalized to unit area .
    !!----
    !!----   example of use:
    !!----      h = convol(pseudo_voigt,p_pv, hat, p_hat, my_interval)
    !!----
    !!----   generates my_interval%np values  h(i), i=1,my_interval%np corresponding
    !!-->>   to the convolution of a pseudo-voigt function with a hat function
    !!----
    !!---- 14/04/2019
    !!
    pure function convol(f, pf, g, pg, interval) result(conv)
       !---- arguments ----!
       real(kind=cp),dimension(:),          intent(in) :: pf
       real(kind=cp),dimension(:),          intent(in) :: pg
       type(points_interval_type),          intent(in) :: interval
       real(kind=cp), dimension(interval%np)           :: conv

       interface f_function
          pure function f(x,parf)  result (vf)
             use cfml_globaldeps, only: cp
             real(kind=cp),              intent(in) :: x
             real(kind=cp), dimension(:),intent(in) :: parf
             real(kind=cp)                          :: vf
          end function f
       end interface f_function

       interface g_function
          pure function g(x,parg)  result (vg)
             use cfml_globaldeps, only: cp
             real(kind=cp),              intent(in) :: x
             real(kind=cp), dimension(:),intent(in) :: parg
             real(kind=cp)                          :: vg
          end function g
       end interface g_function

       !---- local variables ----!
       integer                                  :: i, n, nd2
       real(kind=cp)                            :: step,xvar, value, area
       complex(kind=cp), dimension(interval%np) :: fx,gx,convo

       n=interval%np-1
       nd2=interval%np/2
       step = (interval%high - interval%low)/real(n,kind=cp)
       conv = 0.0_cp

       do i=1,nd2
          xvar=interval%low+real(i-1)*step
          value =  f(xvar,pf)
          fx(nd2+i) =  cmplx(value)
          value =  g(xvar,pg)
          gx(i) =  cmplx(value)
       end do

       do i=nd2+1,interval%np
          xvar=interval%low+real(i-1)*step
          value =  f(xvar,pf)
          fx(i-nd2) =  cmplx(value)
          value =  g(xvar,pg)
          gx(i) =  cmplx(value)
       end do

       call sfft(fx)
       call sfft(gx)

       convo = fx * gx

       call sfft(convo,"inv")

       !> correct for a displacement of 1 step
       !> to recalculate the array for the same input points
       !> one has to interpolate between succesive values

       conv(1) = real(convo(1),kind=cp)
       do i=2,interval%np
          conv(i) = 0.5_cp * ( real(convo(i-1),kind=cp) + real(convo(i),kind=cp) )
       end do

       area=sum(conv)*step
       conv=conv/area

    end function convol

    !!----
    !!---- convol_peaks
    !!----
    !!----   convolution of the user-provided centred (x=0) peak functions f and g.
    !!----
    !!----   the characteristic parameters of the functions f and g are provided in vectors pf and pg.
    !!----   the first component should be the value of the parameter related to the fwhm.
    !!----   the parameter wd controls the range of the calculation in terms
    !!----   of the fwhm of peaks. the definition interval [a,b] of the peaks
    !!----   is calculated as: a=-b, with b=wd*fwhm=wd*pf(1).
    !!----   the number of points to calculate the convolution is "np".
    !!----   internally, the actual number of points "n". corresponding to
    !!----   and increased value of np up to the nearest higher power of 2.
    !!----   the convolution function is normalized to unit area .
    !!----   the internal step is:  step=(b-a)/(n-1)
    !!----   example of use:
    !!----      h = convol_peaks (pseudo_voigt,p_pv, hat, p_hat, 15.0, 150)
    !!----   generates 150 values  h(i), i=1,150 corresponding to the convolution
    !!----   of a pseudo-voigt function with a hat function
    !!----
    !!---- 14/04/2019
    !!
    pure function convol_peaks(f,pf,g,pg,wd,np) result(conv)
       !---- arguments ----!
       real(kind=cp),dimension(:),          intent(in) :: pf !parameters of the function f (starting with fwhm)
       real(kind=cp),dimension(:),          intent(in) :: pg !parameters of the function g (starting with fwhm)
       real(kind=cp),                       intent(in) :: wd !number of times a fwhm of the f-function to calculate range
       integer,                             intent(in) :: np !number of points (it is increased internally up to the closest power of 2)
       real(kind=cp), dimension(np)                    :: conv

       interface f_function
          pure function f(x,parf)  result (vf)
             use cfml_globaldeps, only: cp
             real(kind=cp),              intent(in) :: x
             real(kind=cp), dimension(:),intent(in) :: parf
             real(kind=cp)                          :: vf
          end function f
       end interface f_function

       interface g_function
          pure function g(x,parg) result (vg)
             use cfml_globaldeps, only: cp
             real(kind=cp),              intent(in) :: x
             real(kind=cp), dimension(:),intent(in) :: parg
             real(kind=cp)                          :: vg
          end function g
       end interface g_function

       !---- local variables ----!
       integer                                     :: i,j, n, nd2, m
       real(kind=cp)                               :: step, xvar, value, area, a,b, nstep
       logical                                     :: is_powerof2
       complex(kind=cp), dimension(:), allocatable :: fx,gx,convo
       real(kind=cp), dimension(:), allocatable    :: xv

       ! m will be the effective number of points used in fft
       do i=1,20
          m=2**i
          if (m >= np) exit
       end do
       is_powerof2=.false.
       if( m == np) is_powerof2=.true.

       n=m-1
       nd2=m/2
       b=wd*pf(1)
       a=-b
       step = (b-a)/real(n,kind=cp)
       conv = 0.0_cp

       if(allocated(fx)) deallocate(fx)
       allocate(fx(m))

       if(allocated(gx)) deallocate(gx)
       allocate(gx(m))

       if(allocated(convo)) deallocate(convo)
       allocate(convo(m))

       if(allocated(xv)) deallocate(xv)
       allocate(xv(m))

       do i=1,nd2
          xvar  = a+real(i-1,kind=cp)*step
          xv(i) =  xvar
          value =  f(xvar,pf)
          fx(nd2+i) =  cmplx(value)
          value =  g(xvar,pg)
          gx(i) =  cmplx(value)
       end do

       do i=nd2+1,m
          xvar  = a+real(i-1,kind=cp)*step
          xv(i) = xvar
          value =  f(xvar,pf)
          fx(i-nd2) =  cmplx(value)
          value =  g(xvar,pg)
          gx(i) =  cmplx(value)
       end do

       call sfft(fx)     !forward fft
       call sfft(gx)

       convo = fx * gx

       call sfft(convo,"inv")  !backward fft

       !correct for a displacement of 1 step
       !to recalculate the array for the same input points
       !one has to interpolate between succesive values

       fx(1) = convo(1)
       do i=2,m
          fx(i) = 0.5 * ( convo(i-1) + convo(i) )
       end do
       area=sum(real(fx,kind=cp))*step
       fx=fx/area

       ! calculate an interpolated array for the number of points supplied by the user
       if (.not. is_powerof2) then
          nstep = (b-a)/real(np-1,kind=cp) !new step
          n=1
          conv(np) = real(fx(m),kind=cp)

          do i=1,np-1
             xvar=a+real(i-1,kind=cp)*nstep
             do j=n,m-1
                if (xv(j) >= xvar) then
                   n=j
                   exit
                end if
             end do
             conv(i) = real(fx(n),kind=cp)+(xvar-xv(n))*(real(fx(n+1),kind=cp)- real(fx(n),kind=cp))/step
          end do

       else
          conv=real(fx,kind=cp)
       end if

    end function convol_peaks

    !!---- DIRECT_CONVOL
    !!---- Direct convolution: the signal(f) and kernel(g) should be centred in the middle of the arrays
    !!---- The working array "y" allows a straightforward calculation of the convolution without shifting of the result.
    !!---- The final "convol" result represents the central part (n-points) of "y"
    Pure Function direct_convol(f, g) result(convol)
        real(kind=cp), dimension(:),  intent(in) :: f, g !f is the signal array, g is the impulse array
        real(kind=cp), dimension(:), allocatable :: convol, y
        integer :: n , m, ini
        integer :: i,j

        n = size(f)
        m = size(g)

        allocate(y(n+m-1),convol(n))
        y=0.0
        do i=1,n
          do j=1,m
            y(i+j-1)=y(i+j-1)+f(i)*g(j)
          end do
        end do
        ini=n/2
        convol(1:n)=y(ini+1:ini+n)

    End Function direct_convol

 end module cfml_fft
