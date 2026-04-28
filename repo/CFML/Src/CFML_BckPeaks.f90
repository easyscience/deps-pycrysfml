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
!!---- MODULE: CFML_BCKPEAKS
!!----   INFO: Automatic peak and background search in a one dimensional array
!!----         supposed to contain a powder diffraction pattern or a spectrum
!!----         with positive more or less sharp peaks.
!!----
!!---- HISTORY
!!----    Update: January - 2006  (Created by JRC)
!!----            Based in a code developped by Thierry Roisnel for WinPLOTR.
!!----            Incorporated within CrysFML by JRC in October 2021
!!----
!!---- DEPENDENCIES
!!----
!!----    CFML_GLOBALDEPS, CFML_MATHS, CFML_DIFFPATT
!!----
!!----
!!---- This module has a single public subroutine "Automatic_PkB_Search" used for calculating peak positions
!!---- and background points. There is a public type: "pkb_type", containing peak positions and intensities of
!!---- peaks and background. If mode="background" the polygonal background is returned in the components
!!---- x,y of the pkb_type output argument. If mode="satellites" it is supposed that Pat is a differencett
!!---- pattern and only the possitive oscillactions are used. If mode is whatever other keyword, then the
!!---- peak positions, intensity and background at the peak positions are returned in the pkb_type argument
!!----
!!
 Module CFML_BckPeaks

    !---- Used Modules ----!
    use CFML_GlobalDeps, only: cp, Err_CFML, Set_Error
    use CFML_Maths,      only: Smoothing_Vec,spline_interpol, first_derivative, second_derivative
    use CFML_DiffPatt,   only: DiffPat_E_Type

    !---- Variables ----!
    implicit none

    private

    !---- List of public subroutines ----!
    public     :: automatic_peak_background_search,automatic_pkb_search,get_pkb_conditions,set_pkb_conditions

    !---- Public Type Definitions ----!
    Type, public :: pkb_type
       integer   :: np
       real(kind=cp),dimension(:),allocatable  :: x
       real(kind=cp),dimension(:),allocatable  :: y
       real(kind=cp),dimension(:),allocatable  :: bkg
    End type  pkb_type

    Type,public  :: peak_search_cond_type
       real(kind=cp)     :: peak_threshold     = 0.02
       real(kind=cp)     :: shoulder_threshold = 2.00
       real(kind=cp)     :: bkg_threshold      = 0.05
       integer           :: kindOfpeaks        = 1  !single peaks: 1, doublets: 2(Cu-Ka), 3 (Mo-Ka), 4 (Co-Ka)
       integer           :: Iterations         = 3
    End type peak_search_cond_type

    !---- Public Variables ----!
    Type(peak_search_cond_type),save, public :: pkb_cond

    !---- List of private subroutines ----!
    private    :: look_for_peaks, look_for_background, background_peak, verif_intensity, &
                  search_doublets, sort_by_increasing_order

    !---- Private Variables ----!
    character(len=80),                      private  :: aux
    real(kind=cp),dimension(:),allocatable, private  :: peak_position, peak_intensity, peak_background, &
                                                        background_X, background_Y

 Contains

    function automatic_peak_background_search(pat,x1,x2,mode) result(pkb)
      !> Returns peak positions, intensities and background points in a region [x1,x2]
      !> of the diffraction / spectrum pattern.
      type(DiffPat_E_Type),intent (in)    :: pat  !> diffraction pattern
      real(kind=cp),       intent (in)    :: x1   !> minimum value of x
      real(kind=cp),       intent (in)    :: x2   !> maximum value of x
      character(len=*),    intent (in)    :: mode !> 'peak' | 'background' | 'satellites'
      type(pkb_type)                      :: pkb  !>

      call automatic_pkb_search(pat,x1,x2,mode,pkb)

    end function automatic_peak_background_search

    function get_pkb_conditions() result(pkbc)

      type(peak_search_cond_type) :: pkbc

      pkbc%peak_threshold     = pkb_cond%peak_threshold
      pkbc%shoulder_threshold = pkb_cond%shoulder_threshold
      pkbc%bkg_threshold      = pkb_cond%bkg_threshold
      pkbc%kindofpeaks        = pkb_cond%kindofpeaks
      pkbc%iterations         = pkb_cond%iterations

    end function get_pkb_conditions

    subroutine set_pkb_conditions(pk_th,sh_th,bg_th,peak_kind,iter)
      !> Set parameters used in the automatic detection of peaks and background
      real(kind=cp), optional :: pk_th      !> Peak threshold
      real(kind=cp), optional :: sh_th      !> Shoulder threshold
      real(kind=cp), optional :: bg_th      !> Background threshold
      integer,       optional :: peak_kind  !> 1: single peak | 2: doublet (Cu-Ka) | 3: doublet (Mo-Ka) | 4: doublet (Co-Ka)
      integer,       optional :: iter       !> Number of iterations

      if (present(pk_th))     pkb_cond%peak_threshold = pk_th
      if (present(sh_th))     pkb_cond%shoulder_threshold = sh_th
      if (present(bg_th))     pkb_cond%bkg_threshold = bg_th
      if (present(peak_kind)) pkb_cond%kindofpeaks = peak_kind
      if (present(iter))      pkb_cond%iterations = iter

    end subroutine set_pkb_conditions

     !!---- Subroutine Automatic_PkB_Search(Pat,x1,x2,mode,pkb,print_PkB)
     !!----   !--- Arguments ---!
     !!----   type(DiffPat_E_Type),intent (in)    :: Pat
     !!----   real(kind=cp),       intent (in)    :: x1,x2  !Region for searching peaks or background
     !!----   character(len=*),    intent (in)    :: mode   !"background","satellites", any other word => peaks
     !!----   type(pkb_type),      intent (in out):: pkb    !pkb%x,pkb%y : pkb%np positions and intensities
     !!----   logical, optional,   intent (in)    :: print_PkB !if present writes the files "peaks.aps" or "background.xy"
     !!----
     !!----   This subroutine provide peak positions, intensities and background points in a region [x1,x2]
     !!----   of the diffraction/spectrum pattern Pat
     !!----
     Subroutine Automatic_PkB_Search(Pat,x1,x2,mode,pkb,print_PkB)
       !--- Arguments ---!
       type(DiffPat_E_Type),intent (in)    :: Pat
       real(kind=cp),       intent (in)    :: x1,x2
       character(len=*),    intent (in)    :: mode
       type(pkb_type),      intent (in out):: pkb
       logical, optional,   intent (in)    :: print_PkB

       !--- Local variables ---!
       integer                                  :: npts_eff, n, i_bac, i_pic, ier
       real(kind=cp),dimension(size(pat%x))     :: Xdom, Ydom
       real(kind=cp),dimension(2*size(pat%x)+1) :: X_inter, Y_inter, Y1, Y2
       real(kind=cp)                            :: X0, Y0, step
       integer                                  :: nb_pic, nb_background
       integer                                  :: i,j_start,j_end

       if(allocated(peak_position))   deallocate(peak_position)
       if(allocated(peak_intensity))  deallocate(peak_intensity)
       if(allocated(peak_background)) deallocate(peak_background)
       if(allocated(background_x))    deallocate(background_x)
       if(allocated(background_y))    deallocate(background_y)

       ! Determination of the number of points in the screen: Allocation of Xdom, Ydom

       call Set_Error(0," ")
       n = 0
       j_start = 1
       j_end   = pat%npts
       Xdom=0.0; Ydom=0.0

       do i=1, pat%npts        !it is supposed that the data are ordered by increasing x
         if (pat%X(i) < x1) cycle
         j_start=i
         exit
       end do

       do i=j_start, pat%npts
         if (pat%X(i) > x2) exit
         n = n + 1
         j_end = i
       end do

       Xdom(1:n) = pat%X(j_start:j_end)
       if(mode == "satellites") then
         Ydom(1:n) = pat%Y(j_start:j_end)-pat%Ycalc(j_start:j_end)+pat%bgr(j_start:j_end)
         where(Ydom < 0.0) Ydom=0.0
       else
         Ydom(1:n) = pat%Y(j_start:j_end)
       end if
       Y2=0.0
       Y1=0.0

      !1. Calculation of the second derivative for the n points
          Y2= second_derivative(Xdom, Ydom, n)

      !2. Calculation of data with step = step /2 by interpolation
          step = (Xdom(n) - Xdom(1)) / (n -1)
          step = step / 2
          npts_eff = 2*n -1

          do i=1, npts_eff - 1
            X0 = Xdom(1) + step * (i-1)
            Y0=  Spline_Interpol(X0,Xdom,Ydom,Y2,n)
            X_inter(i) = X0
            Y_inter(i) = Y0
          end do
          X_inter(npts_eff) = Xdom(n)
          Y_inter(npts_eff) = Ydom(n)

      !3. Calculation of the second derivative for the npts_eff points
          Y2=0.0 !initialize the second derivative vector
          Y2=second_derivative(X_inter, Y_inter, npts_eff)

      !4. Calculation of the first derivative for each point
          npts_eff = npts_eff - 1
          !call first_derivative(X_inter, Y_inter, npts_eff, Y2, Y1)
          Y1 = first_derivative(X_inter, Y_inter, npts_eff)

      !5. Smoothing of the derivative curve
          Y1=Smoothing_Vec(Y1, npts_eff, pkb_cond%iterations)

      !6. look for background points
          nb_background = int(SQRT(REAL(n)))

          allocate (background_X(nb_background), background_Y(nb_background))

          call look_for_background(Xdom,Ydom,n,Y1,npts_eff, nb_background)

          if (nb_background < 2) then
            call Set_Error(1," The number of determined background points is too low, peak search impossible!")
            return
          end if

          if(mode == "background") then
             if(present(print_PkB)) then
                open (newunit=i_bac, file='background.xy', iostat=ier, status="replace",action="write")
                if (ier /=0) then
                  call Set_Error(ier," Problem opening the 'background.xy' file")
                  return
                else
                   write(i_bac, '(a)', iostat=ier) '! Data file: '//trim(pat%filename)
                   write(i_bac, '(a)', iostat=ier) '! Format:    '//trim(pat%instr)
                   if (ier/=0) then
                      call Set_Error(ier,"Problem writing in the 'background.xy' file: please check file properties !")
                      return
                   end if
                   WRITE(i_bac, '(a)') '! position      background_value'
                   do i=1, nb_background
                      write(i_bac,'(3f15.5)')  background_x(i), background_y(i)
                   end do
                end if
                close(unit=i_bac)
             end if
             ! Copy the background points in the output argument and return
             if(allocated(pkb%x)) deallocate(pkb%x)
             allocate(pkb%x(nb_background))
             if(allocated(pkb%y)) deallocate(pkb%y)
             allocate(pkb%y(nb_background))
             pkb%np=nb_background
             pkb%x(1:nb_background)=background_x(1:nb_background)
             pkb%y(1:nb_background)=background_y(1:nb_background)
             return
          end if

      !7. Continuing the procedure if searching peaks within the npts_eff points (interpolated
      !   with step = initial_step/2 )

           Y2=Smoothing_Vec(Y2, npts_eff, pkb_cond%iterations)

           allocate (peak_position(n), peak_intensity(n))
           call look_for_peaks(X_inter, Y_inter, Y1, Y2, npts_eff, nb_pic, nb_background)

           ! Determination of the background associated to each found peak (reflection)

           if (nb_pic /= 0) then

              if (allocated(peak_background)) deallocate (peak_background)
              allocate (peak_background(nb_pic))
              call background_peak(nb_pic, nb_background)
              call verif_intensity(X_inter, Y_inter,npts_eff, nb_pic)

              if (pkb_cond%kindofpeaks /= 1)  call search_doublets(nb_pic)

              if(nb_pic /=0) then
                if(present(print_PkB)) then
                   open (newunit=i_pic, file='peak.aps', iostat=ier, status="replace")
                   if (ier /=0) then
                     call Set_Error(ier,"Problem opening the 'peak.aps' file: please check the file properties.")
                     return
                   else
                     write(i_pic, '(a)', iostat=ier) '! Data file: '//trim(pat%filename)
                     write(i_pic, '(a)', iostat=ier) '! Format:    '//trim(pat%instr)
                     if (ier/=0) then
                       call Set_Error(ier,"Problem writing in the 'peak.xy' file. Please check the file properties !")
                       return
                     end if
                     write(i_pic, '(a)') '! peak_position   peak_intensity   background'
                     do i=1, nb_pic
                       write(i_pic,'(3f15.5)')  peak_position(i), peak_intensity(i), peak_background(i)
                     end do
                   end if
                   close(unit=i_pic)
                end if
               ! Copy the points in the output argument and return
                if(allocated(pkb%x)) deallocate(pkb%x)
                allocate(pkb%x(nb_pic))
                if(allocated(pkb%y)) deallocate(pkb%y)
                allocate(pkb%y(nb_pic))
                if(allocated(pkb%bkg)) deallocate(pkb%bkg)
                allocate(pkb%bkg(nb_pic))
                pkb%np=nb_pic
                pkb%x(1:nb_pic)=peak_position(1:nb_pic)
                pkb%y(1:nb_pic)=peak_intensity(1:nb_pic)
                pkb%bkg(1:nb_pic)=peak_background(1:nb_pic)

              end if
           else
              call Set_Error(ier,"No peak detected !")
           end if  ! end of the condition nb_pic /=0

     End Subroutine Automatic_pkb_search


     Subroutine look_for_peaks(X, Y, Y1, Y2, n, nb_pic, nb_background)
       integer,                     intent(in)  :: n             ! Number of points
       integer,                     intent(in)  :: nb_background ! Number of the background points
       real(kind=cp), dimension(:), intent(in)  :: X, Y          ! Arrays X, Y
       real(kind=cp), dimension(:), intent(in)  :: Y1            ! smoothed first derivative
       real(kind=cp), dimension(:), intent(in)  :: Y2            ! smoothed second derivative
       integer,                     intent(out) :: nb_pic        ! Number of peaks
       !---- Local variables ----!
       real(kind=cp)   :: a, b
       real(kind=cp)   :: Y_high, Y_low, delta_Y, delta_Y_max
       real(kind=cp)   :: y_bf_min, y_bf_max
       integer         :: i, j, k, dom
       real(kind=cp)   :: delta_bf, delta_bf_max
       integer         :: nb_pic_1  ! nombre de pics determines a partir de la derivee
       integer         :: nb_pic_2  ! nombre d'epaulements a droite
       integer         :: nb_pic_3  ! nombre d'epaulements a gauche
       integer         :: num


       nb_pic   = 0
       nb_pic_1 = 0
       nb_pic_2 = 0
       nb_pic_3 = 0

       ! Determination of the maximal amplitude deltaY_max of the derivative signal when it changes sign.
       delta_Y_max = -1.0e+30
       do i = 2,n -2
         if (Y1(i) > 0.0 .and. Y1(i+1) < 0.0) then  ! signal derive nul
         ! Search of the maxim signal before the i-point
          Y_high = Y1(i)
          j = 1
          do while (Y1(i-j) > Y_high .and. (i-j) > 1)
           Y_high = Y1(i-j)
           j = j + 1
          end do

         ! Search of the minimum signal after the i-point
           Y_low = Y1(i+1)
           j = 2
           do WHILE (Y1(i+j) < Y_low .and. (i+j) < n)
            Y_low = Y1(i+j)
            j = j + 1
           end do

         ! Signal amplitude around the null derivative
           delta_Y = Y_high - Y_low
           if (delta_Y > delta_Y_max) delta_Y_max = delta_Y
         end if
       end do


       ! Comparison of the amplitude delta_Y of the derivative signal (when it changes sign)
       ! with the maximale amplitude delta_Y_max
       do i=2, n-2

         if (Y1(i) > 0.0 .and. Y1(i+1) < 0.0) then  ! zero derivative signal

           ! Search of the maximum signal before the i-point
           Y_high = Y1(i)
           j = 1
           do WHILE (Y1(i-j) > Y_high .and. (i-j) > 1)
             Y_high = Y1(i-j)
             j = j + 1
           end do

           ! Search of the minimum signal after the i-point
           Y_low = Y1(i+1)
           j = 2
           do WHILE (Y1(i+j) < Y_low .and. (i+j) < n)
             Y_low = Y1(i+j)
             j = j + 1
           end do

           ! Signal amplitude around the zero derivative
           delta_Y = Y_high - Y_low
           if (delta_Y > pkb_cond%peak_threshold * delta_Y_max) then
             nb_pic_1 = nb_pic_1  + 1
             num = nb_pic + nb_pic_1
             ! Position of the peak: zero derivative
             a = (Y1(i+1) - Y1(i)) / (X(i+1) - X(i))
             b = Y1(i) - a * X(i)
             peak_position(num) = -b/a
             ! Intensity of the peak: linear interpolation
             a = (Y(i+1) - Y(i)) / (X(i+1) - X(i))
             b = Y(i) - a * X(i)
             peak_intensity(num) = a * peak_position(num) + b
           end if

         end if

       end do

       nb_pic = nb_pic_1

       ! Search for shoulders: ==> Y > Bkg + derivative close to zero
       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
       !! 27.01.99 : The heigh of the well should be compared with the maximum fluctuations of the
       !             background and not with the fluctuations around the closest background point
       ! Determination of the maximal fluctuations of background
        delta_bf = -1.0E+30
        delta_bf_max = -1.0E+30
        do dom=1, nb_background
          ! Search of the index of the closest point to the background point
          do i=4, n-3
            if (X(i) > background_x(dom)) exit
          end do
          if (i == n-2) i= n-3     ! Case where the condition in the previous loop is not satisfyied
          ! Calculation of the fluctuations around this point
          y_bf_min = Y1(i-2)
          y_bf_max = Y1(i-2)
          do k = -3,3
            if (Y1(i+k) < y_bf_min ) y_bf_min = Y1(i+k)
            if (Y1(i+k) > y_bf_max ) y_bf_max = Y1(i+k)
          end do
          delta_bf = y_bf_max - y_bf_min
          if (delta_bf > delta_bf_max) delta_bf_max = delta_bf
        end do

       ! 1. Shoulders on the right
       !          => Search for a maximum in a negative first derivative
       !          => Search for the high of the barrier after the maximum
       !          => Position of the peak: minimum in the second derivative

        do i=4, n-3
           if (Y1(i) < 0.0) then                                                          ! condition 1
              if (Y1(i-3) < Y1(i-2) .and. Y1(i-2) < Y1(i-1) .and. Y1(i-1) < Y1(i) )then     ! condition 2
                 if (Y1(i) > Y1(i+1) .and. Y1(i+1) > Y1(i+2) .and. Y1(i+2) > Y1(i+3)) then    ! condition 3
                    ! search of points in the descent of Y1 from the maximum
                    j=i
                    k=0
                    do
                      if (Y1(j+1) > Y1(j)) exit
                      j=j+1                         ! we continue on the descent
                      k=k+1                         ! number of points in the descent
                      if(j>n-3) exit
                    end do
                    delta_Y = abs(Y1(j) - Y1(i))   ! haight of the descent

                    if(delta_Y > pkb_cond%shoulder_threshold*delta_bf_max) then  ! wall > maximum fluctuations of the background
                      nb_pic_2 = nb_pic_2  + 1
                      num = nb_pic + nb_pic_2

                      ! Search minimum in the second derivative
                      j=i
                      k=0
                      do
                        if (Y2(j+1) > Y2(j)) exit
                        j=j+1
                        k=k+1
                        if (j>n-3) exit
                      end do
                      peak_position(num)  = X(j)
                      peak_intensity(num) = Y(j)
                    end if

                 end if       ! end condition 3
              end if       ! end condition 2
           end if         ! end condition 1
        end do

         nb_pic= nb_pic + nb_pic_2

         ! 2. Shoulders on the left
         !          => Search for a minimum in a positive first derivative
         !          => Search for the high of the barrier before the maximum
         !          => Position of the peak: minimum in the second derivative
        do i=4, n-3
           if (Y1(i) > 0.) then                                                           ! condition 1
              if (Y1(i-3) > Y1(i-2) .and. Y1(i-2) > Y1(i-1) .and. Y1(i-1) > Y1(i) )then     ! condition 2
                 if (Y1(i) < Y1(i+1) .and. Y1(i+1) < Y1(i+2) .and. Y1(i+2) < Y1(i+3)) then    ! condition 3

                    ! 02.09.03:  search of points in the rise of Y1 from the minimum
                    j=i
                    k=0
                    do
                       if (Y1(j-1) < Y1(j)) exit
                       j=j-1                         ! continuing the rise
                       k=k+1                         ! number of points in the rise
                       if (j< 4) exit
                    end do
                    delta_Y = abs(Y1(j) - Y1(i))   ! height of the rise

                    if(delta_Y > pkb_cond%shoulder_threshold*delta_bf_max) then ! height of well > fluctuations of background
                       nb_pic_3 = nb_pic_3  + 1
                       num = nb_pic + nb_pic_3
                       ! search of the minimum in the second derivative
                       j=i
                       k=0
                       do
                         if (Y2(j-1) > Y2(j)) exit
                         j=j-1
                         k=k+1
                         if (j<4) exit
                       end do
                       peak_position(num)  = X(j)
                       peak_intensity(num) = Y(j)
                    end if

                 end if  ! end condition 3
              end if   ! end condition 2
           end if    ! end condition 1
        end do

         nb_pic = nb_pic + nb_pic_3

         ! Ordering the positions of the peaks by increasing values
         call  sort_by_increasing_order(peak_position, peak_intensity, nb_pic)

     End Subroutine look_for_peaks


     !!---- Subroutine look_for_background(X,Y,N,Y1,n_eff,nb_background)
     !!----   real(kind=cp), dimension(:),intent(in)    :: X, Y          ! arrays X, Y
     !!----   integer,                    intent(in)    :: N             ! number of points in the data
     !!----   real(kind=cp), dimension(:),intent(in)    :: Y1            ! derivative curve
     !!----   integer,                    intent(in)    :: N_eff         ! number of points in the derivative curve
     !!----   integer,                    intent(in out):: nb_background ! number of background points
     Subroutine look_for_background(X,Y,N,Y1,n_eff,nb_background)
       real(kind=cp), dimension(:),intent(in)    :: X, Y
       integer,                    intent(in)    :: N
       real(kind=cp), dimension(:),intent(in)    :: Y1
       integer,                    intent(in)    :: N_eff
       integer,                    intent(in out):: nb_background
       !---- Local variables ----!
       integer       :: i, j, k, dom , num, num_bf
       real(kind=cp) :: y_min, x_dom
       real(kind=cp) :: y_bf_min, y_bf_max, delta_bf
       real(kind=cp) :: delta_Y1, delta_Y1_max

       ! Calculation of the maximum fluctuation around a point
       delta_Y1_max = -1.0E+30
       do i=2, n_eff-1
         delta_Y1 = abs(y1(i-1) -y1(i+1))
         if (delta_Y1 > delta_Y1_max) delta_Y1_max = delta_Y1
       end do

       !  Division of the diffraction/spectrum pattern in several sections
       num_bf = 0
       j = 2
       num = 2

       do dom= 1, nb_background
         y_min = 1.0E+30
         x_dom = X(1) + (X(n)-X(1)) * real(dom)/real(nb_background)
         do i=j, N-1
           ! Look for points with weak intensity within each section
           if (X(i) < x_dom) then
             if (Y(i) < Y_min) then
               Y_min = Y(i)
               num = i
             end if
           else
             j = i
             exit
           end if
         end do     ! fin de la boucle sur les points

         ! The derivative must be close to zero for the point to be considered
         ! as a background point. Calculation of fluctuations around this point
         y_bf_min =  1.0E+30
         y_bf_max = -1.0E+30

         do k=-1,1
           if (Y1((2*num-1)+k) < y_bf_min ) y_bf_min = Y1((2*num-1)+k)    !      N points in Y
           if (Y1((2*num-1)+k) > y_bf_max ) y_bf_max = Y1((2*num-1)+k)    ! and 2N points in Y1
         end do
         delta_bf = y_bf_max - y_bf_min

         if (delta_bf < pkb_cond%bkg_threshold * delta_Y1_max) then
           num_bf = num_bf +1
           background_x(num_bf) = X(num)
           background_y(num_bf) = (y(num)+y(num-1)+y(num+1))/3
         end if

       end do

       nb_background = num_bf

       if (nb_background < 2) then
           Err_CFML%flag = .true.
           Err_CFML%Msg  = " ! Not enough data points to correctly determine the background points ! "
       end if
     End Subroutine  look_for_background

     !!---- Subroutine background_peak(nb_pic, nb_background)
     !!----   integer,  intent(in)  :: nb_pic
     !!----   integer,  intent(in)  :: nb_background
     !!----
     !!----  Determination of the background at the peak positions
     !!----  from the list of background points
     !!----
     Subroutine background_peak(nb_pic, nb_background)
       integer,  intent(in)  :: nb_pic
       integer,  intent(in)  :: nb_background
       !--- Local variables ---!
       integer               :: i
       real(kind=cp)         :: x0, y0
       real(kind=cp), dimension(nb_background) :: y2   ! second derivative of background_y

       y2= second_derivative(background_x, background_y, nb_background)
       do i=1, nb_pic
          x0 = peak_position(i)
          if (x0 > background_x(1)) then
            y0=Spline_Interpol(X0,background_x, background_y,Y2,nb_background)
            peak_background(i) = y0
          else        ! new line: bad interpolation if x0 < background_x(1)
            peak_background(i) = -1.0
          end if
       end do
     End Subroutine background_peak

     !!----  Subroutine verif_intensity(X, Y, n, nb_pic)
     !!----    real(kind=cp),dimension(:),intent (in)    :: X, Y
     !!----    integer,                   intent (in)    :: n
     !!----    integer,                   intent (in out):: nb_pic
     !!----
     !!----  Elimination of peaks with intensity below  (1 + peak_threshold)*background
     !!----  and recalculation of the effective number of peaks
     !!----
     Subroutine verif_intensity(X, Y, n, nb_pic)
       real(kind=cp),dimension(:),intent (in)    :: X, Y
       integer,                   intent (in)    :: n
       integer,                   intent (in out):: nb_pic
       !--- Local variables ---!
       logical,       dimension(nb_pic)  :: pic_ok
       real(kind=cp), dimension(nb_pic)  :: x_pic, y_pic, bf_pic
       integer                           :: i, i_eff, nb_eff_pic, num
       integer                           :: pic

       do pic=1, nb_pic
         pic_ok(pic) = .false.
         if (peak_background(pic) < 0.0) then
           cycle
         end if

         ! The points at both sides should be higher than the background
         do i=4, n-2
           if (X(i) > peak_position(pic) ) exit
         end do
         num = i-1
         if (Y(num-2) > (1 + pkb_cond%peak_threshold)*peak_background(pic)) then
           if (Y(num-1) > (1 + pkb_cond%peak_threshold)*peak_background(pic)) then
             if (Y(num)   > (1 + pkb_cond%peak_threshold)*peak_background(pic)) then
               if (Y(num+1) > (1 + pkb_cond%peak_threshold)*peak_background(pic)) then
                 if (Y(num+2) > (1 + pkb_cond%peak_threshold)*peak_background(pic)) then
                   pic_ok(pic) = .true.
                 end if
               end if
             end if
           end if
         end if
       end do

       ! Determination of the effective number of peaks
       nb_eff_pic = 0
       do pic = 1, nb_pic
         if (pic_ok(pic)) then
           nb_eff_pic = nb_eff_pic + 1
         end if
       end do

       ! Ordering reflections
       i_eff = 0
       do pic=1, nb_pic
         if (pic_ok(pic)) then
           i_eff = i_eff + 1
           x_pic(i_eff)  = peak_position(pic)
           y_pic(i_eff)  = peak_intensity(pic)
           bf_pic(i_eff) = peak_background(pic)
         end if
       end do

       do pic=1, nb_eff_pic
         peak_position(pic)   = x_pic(pic)
         peak_intensity(pic)  = y_pic(pic)
         peak_background(pic) = bf_pic(pic)
       end do
       nb_pic = nb_eff_pic

     End Subroutine verif_intensity

     !!---- Subroutine search_doublets(nb_pic)
     !!---- integer, intent(in out)      :: nb_pic
     !!----
     !!---- Search for Ka1/Ka2 doublets in the reflection list and
     !!---- elimination of Ka2 reflections
     !!----
     Subroutine search_doublets(nb_pic)
       integer, intent(in out)      :: nb_pic
       !---- Local variables ----!
       integer                      :: i, i_eff, nb_eff, pic
       real(kind=cp)                :: sin_theta1, sin_theta2, sin_ratio
       real(kind=cp)                :: int_ratio, lambda_ratio
       logical,      dimension(nb_pic)  :: pic_ok
       real(kind=cp),dimension(nb_pic)  :: x_pic, y_pic, bf_pic


       ! Cu Ka1=1.540598  Ka2= 1.54439  ==> Ka2/Ka1 = 1.00247 +- ?%
       ! Mo Ka1=0.71354   Ka2= 0.70926  ==> Ka2/Ka1 = 1.00603 +- ?%
       ! Co Ka1=1.79278   Ka2= 1.78892  ==> Ka1/Ka2 = 1.00216 +- ?%
       !                                    Ia2/Ia1 = 0.5 +- ?%
       ! For a doublet, we should have:
       !   sinTheta2 / sinTheta1 = Ka2 / Ka1


       if (pkb_cond%kindofpeaks==2) then
         lambda_ratio = 1.0025
       elseif (pkb_cond%kindofpeaks==3) then
         lambda_ratio = 1.0060
       elseif (pkb_cond%kindofpeaks==4) then
         lambda_ratio = 1.0022
       end if

       pic_ok(1) = .true.
       do i=2, nb_pic
         pic_ok(i) = .true.
         ! Comparison of reflection n with reflection n-1
         sin_theta1 =  SIND(peak_position(i-1)*0.5)
         sin_theta2 =  SIND(peak_position(i)*0.5)
         sin_ratio  = sin_theta2 / sin_theta1
         if (abs(sin_ratio - lambda_ratio) < 0.005 ) then  ! doublet
           ! Verification of the intensity: it must verify Ia2/Ia1 approx. 0.5
           int_ratio = (peak_intensity(i) - peak_background(i)) / (peak_intensity(i-1) - peak_background(i-1))
           if (abs(int_ratio - 0.5) < 0.2 ) then
              pic_ok(i) = .false.         ! The peak is a  Ka2 reflexion: it should be eliminated
           end if
         end if
       end do

       ! Determination of the effectif number of reflexions
        nb_eff = 0
        do i=1, nb_pic
          if (pic_ok(i)) then
            nb_eff = nb_eff + 1
          end if
        end do

        ! Put reflections in order
         i_eff = 0
         do pic=1, nb_pic
           if (pic_ok(pic)) then
             i_eff = i_eff + 1
             x_pic(i_eff)  = peak_position(pic)
             y_pic(i_eff)  = peak_intensity(pic)
             bf_pic(i_eff) = peak_background(pic)
           end if
         end do

         do pic=1, nb_eff
           peak_position(pic)   = x_pic(pic)
           peak_intensity(pic)  = y_pic(pic)
           peak_background(pic) = bf_pic(pic)
         end do
         nb_pic = nb_eff
     End Subroutine search_doublets

     !!---- Subroutine sort_by_increasing_order(x,y,n,z)
     !!----   real(kind=cp),  dimension(:),intent(in out) :: x, y
     !!----   integer,                     intent(in)     :: n
     !!----   real(kind=cp),  dimension(:), optional, intent(in out) :: z
     !!----
     !!----  Local sort of arrays X,Y by increasing values of X
     !!----
     Subroutine sort_by_increasing_order(x,y,n,z)
       real(kind=cp),  dimension(:),intent(in out) :: x, y
       integer,                     intent(in)     :: n
       real(kind=cp),  dimension(:), optional, intent(in out) :: z
       !--- Local variables ---!
       integer   :: i, j
       real(kind=cp)     :: temp

       do i=1, n -1
         do j=i+1, n
           if(x(i) > x(j)) then
             temp = x(i)
             x(i) = x(j)
             x(j) = temp
             temp = y(i)
             y(i) = y(j)
             y(j) = temp
             if(present(z)) then
               temp = z(i)
               z(i) = z(j)
               z(j) = temp
              end if
           end if
         end do
       end do
     End Subroutine sort_by_increasing_order

  End Module CFML_BckPeaks
