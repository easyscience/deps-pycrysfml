Submodule (CFML_DiffPatt) DiffP_BackgPatt

 implicit none

 real(kind=cp), parameter :: eps=0.0001_cp

 Contains

    Pure Module Function is_in_excluded_region(x,Excl) result(isin)
      real(kind=cp),       intent(in) :: x
      type(Excl_reg_type), intent(in) :: Excl
      logical                         :: isin
      !--- Local variables ---!
      integer :: i
      isin=.false.
      do i=1,Excl%num_excl
        if(x >= Excl%Exc(i)%mina .and. x <= Excl%Exc(i)%maxb) then
          isin=.true.
          exit
        end if
      end do
    End Function is_in_excluded_region

    Module Function Calc_Chi2(Pat,Excl,nfree) result(Chi2)
       class(DiffPat_E_Type), intent(in) :: Pat     !> Diffraction pattern
       type(Excl_reg_type),   intent(in) :: Excl    !> Excluded region
       integer,               intent(in) :: nfree
       real(kind=cp)                     :: Chi2
       !--- Local variables ---!
       integer :: i,n
       real(kind=cp) :: res

       n=0; chi2=0.0_cp
       do i=1,Pat%npts
         if(is_in_excluded_region(Pat%x(i),Excl) .or. Pat%sigma(i) < eps) cycle
         n=n+1
         res=(Pat%y(i)-Pat%ycalc(i))/Pat%sigma(i)
         chi2=chi2+res*res
       end do
       chi2=chi2/(n-nfree)
    End Function Calc_Chi2

    Module Subroutine Calc_Rfactors(Pat,Excl,nfree,Chi2,Rexp,Rwpatt,Rpatt,Gof,sigvar)
       class(DiffPat_E_Type), intent(in)  :: Pat     !> Diffraction pattern
       type(Excl_reg_type),   intent(in)  :: Excl    !> Excluded region
       integer,               intent(in)  :: nfree   !> Number of degrees of freedom
       real(kind=cp),         intent(out) :: Chi2    !> Chi-square
       real(kind=cp),         intent(out) :: Rexp    !> Expected weigthed R-factor
       real(kind=cp),         intent(out) :: Rwpatt  !> Weigthed R-factor
       real(kind=cp),         intent(out) :: Rpatt   !> R-factor
       real(kind=cp),         intent(out) :: Gof     !> "Goodness-of-fit"
       logical,               intent(in)  :: sigvar  !> If .false. sigma component is the variance
       !--- Local variables ---!
       integer :: i,n
       real(kind=cp) :: res, sumy,sumwy
       real(kind=cp), dimension(Pat%npts) :: w

       n=0; chi2=0.0_cp; Rexp=0.0_cp; Rwpatt=0.0_cp; Rpatt=0.0_cp
       sumy=0.0_cp; sumwy=0.0_cp
       if(sigvar) then
         w=1.0_cp/(Pat%sigma(1:Pat%npts))**2
       else
         w=1.0_cp/Pat%sigma(1:Pat%npts)
       end if
       do i=1,Pat%npts
         if(is_in_excluded_region(Pat%x(i),Excl) .or. Pat%sigma(i) < eps) cycle
         n=n+1
         res=abs(Pat%y(i)-Pat%ycalc(i))
         Rpatt = Rpatt + res
         sumy = sumy + Pat%y(i)
         !w = 1.0_cp/(Pat%sigma(i)*Pat%sigma(i))
         sumwy = sumwy + w(i)*Pat%y(i)**2
         chi2 = chi2 + w(i)*res*res
       end do
       Rwpatt=100.0_cp*sqrt(chi2/sumwy)
       chi2=chi2/real(n-nfree)
       Rpatt=100.0*Rpatt/sumy
       Rexp=100.0*sqrt(real(n-nfree)/sumwy)
       Gof=Rwpatt/Rexp
    End Subroutine Calc_Rfactors

    !>
    !> CALC_BACKGROUND
    !>
    !>    Calculate a Background using an iterative process according
    !>    to Bruckner, S. (2000). J. Appl. Cryst., 33, 977-979.
    !>
    !>    Np is the extension of Np points at Left and Right. Normally it could be
    !>    around 10-40 points.
    !>
    !> 30/04/2019
    !>
    Module Subroutine Calc_BackGround(Pat, Ncyc, Np, Xmin, Xmax)
       !---- Arguments ----!
       class(DiffPat_E_Type),     intent(in out) :: Pat        ! Pattern object
       integer,                   intent(in)     :: NCyc       ! Number of Cycles to apply
       integer,                   intent(in)     :: Np         ! Number of extension points at L and R.
       real(kind=cp), optional,   intent(in)     :: Xmin       ! Min, max values in X
       real(kind=cp), optional,   intent(in)     :: Xmax

       !---- Variables ----!
       integer                                 :: n,n_ini,n_fin
       integer                                 :: i,j,k,ind1,ind2,nt
       real(kind=cp)                           :: x_ini,x_fin, yc_min, yc_max, yc_ave
       real(kind=cp),dimension(:), allocatable :: yc, yb

       !> Init
       pat%bgr=0.0_cp

       !> Check
       call clear_error()
       if (pat%npts <= 1) then
          err_CFML%Ierr=1
          err_CFML%Msg="Calc_BackGround@DIFFPAT: There's no points in the Pattern used for Background calculation!"
          return
       end if

       !> Number of points into the range
       x_ini=pat%xmin
       x_fin=pat%xmax
       if (present(xmin)) x_ini=xmin
       if (present(xmax)) x_fin=xmax

       nt=0
       do i=1,pat%npts
          if (pat%x(i) <= x_ini) cycle
          if (pat%x(i) > x_fin) cycle
          nt=nt+1
       end do
       if (nt < 1) then
          err_CFML%Ierr=1
          err_CFML%Msg="Calc_BackGround@DIFFPAT: There's no background points into the range"
          return
       end if

       !> Locating index that define the range to study
       ind1=0
       if (abs(x_ini-pat%xmin) <= eps) then
          ind1=1
       else
          ind1=locate(pat%x,x_ini)
          ind1=max(ind1,1)
          ind1=min(ind1,pat%npts)
       end if

       ind2=0
       if (abs(x_fin-pat%xmax) <= eps) then
          ind2=pat%npts
       else
          ind2=locate(pat%x,x_fin)
          ind2=min(ind2,pat%npts)
          ind2=max(ind2,1)
       end if

       if (ind1 == ind2) then
          err_CFML%IErr=1
          err_CFML%Msg="Calc_BackGround@DIFFPAT: Same value for Xmin and Xmax! "
          return
       end if
       if (ind1 > ind2) then
          i=ind1
          ind1=ind2
          ind2=i
       end if

       !> Allocating arrays
       if (ind2-ind1+1 > nt) nt=ind2-ind1+1
       if (allocated(yc)) deallocate(yc)
       if (allocated(yb)) deallocate(yb)
       allocate(yc(nt+2*np))
       allocate(yb(nt+2*np))

       yc=0.0_cp

       !> Load initial values
       n_ini=np+1
       n_fin=np+nt
       yc(1:np)=pat%y(ind1)
       yc(n_ini:n_fin)=pat%y(ind1:ind2)
       yc(n_fin+1:n_fin+np)=pat%y(ind2)

       yc_min=minval(pat%y(ind1:ind2))
       yc_ave=sum(pat%y(ind1:ind2))/real(nt)
       yc_max=yc_ave+2.0_cp*(yc_ave-yc_min)
       where(yc > yc_max) yc=yc_max

       !> Main cycles
       do n=1,Ncyc
          yb=0.0_cp
          do k=n_ini,n_fin ! Points Observed
             do i=-np,np
                if (i == 0) cycle
                j=k+i
                yb(k)=yb(k)+yc(j)
             end do
             yb(k)=yb(k)/real(2*np)
          end do
          do k=n_ini,n_fin
             j=k-np+ind1-1
             if (yb(k) > pat%y(j)) yb(k)=pat%y(j)
          end do
          yb(1:np)=yb(n_ini)
          yb(n_fin+1:n_fin+np)=yb(n_fin)
          yc=yb
       end do

       !> Save the result
       pat%bgr(ind1:ind2)=yc(n_ini:n_fin)

    End Subroutine Calc_BackGround

    Module Subroutine Calc_BackGround_Chebychev(Pat, Nd, Par_bck, Xmin, Xmax,der)
       class(DiffPat_E_Type),                 intent(in out) :: Pat     !> Diffraction pattern
       integer,                               intent(in)     :: Nd      !> Number of polynomial coefficients: Nd=Order+1
       real(kind=cp), dimension(:),           intent(in)     :: Par_bck !> Parameter vector of Chebychev coefficients
       real(kind=cp), optional,               intent(in)     :: Xmin    !> Minimum value of x
       real(kind=cp), optional,               intent(in)     :: Xmax    !> Maximum value of x
       real(kind=cp), optional, dimension(:), intent(out)    :: der     !> Derivatives w.r.t. coefficients
       !--- Local variables ---!
       integer :: i,j
       real(kind=cp) :: derv,thx,rj, xp,xm,ac


       if (.not. allocated(Pat%bgr)) then   !if it is already allocated just add the previous
         allocate(Pat%bgr(Pat%npts))        !backgrount to that calculated here
         Pat%bgr=0.0_cp ! nullify the background
       end if
       Pat%al_bgr=.true.
       xp=0.5_cp*(xmax+xmin) ! may be Pat%xmin, Pat%xmax
       xm=0.5_cp*(xmax-xmin)
       if(present(der)) then
        der=0.0_cp
        der(1)=1.0_cp
       end if
       do i=1,Pat%npts
         Pat%bgr(i)=Par_bck(1)
         thx=(Pat%x(i)-xp)/xm
         if(thx < -1.0) thx=-1.0
         if(thx >  1.0) thx= 1.0
         ac=acos(thx)
         do j=1,nd-1
           rj=real(j)
           derv=cos(rj*ac)
           Pat%bgr(i)=Pat%bgr(i)+Par_bck(j+1)*derv
           if(present(der)) der(j+1)= der(j+1)+derv
         end do
       end do
    End Subroutine Calc_BackGround_Chebychev

    Module Subroutine Calc_BackGround_split_pVoigt(Pat, Np, Par, deriv)
       class(DiffPat_E_Type),                     intent(in out) :: Pat     !> Diffraction pattern
       integer,                                   intent(in)     :: Np      !> Number of peaks
       real(kind=cp), dimension(:,:),             intent(in)     :: Par     !> Parameters (1:6,Np)
       real(kind=cp), optional, dimension(:,:,:), intent(out)    :: deriv   !> Derivatives w.r.t. parameters for each point (1:6,Np,npts)
       !--- Local variables ---!
       integer :: i,j
       real(kind=cp) :: x,pv
       real(kind=cp), dimension(Pat%npts) :: bk,xbk
       real(kind=cp), dimension(5)        :: der


       if (.not. allocated(Pat%bgr)) then   !if it is already allocated just add the previous
         allocate(Pat%bgr(Pat%npts))        !backgrount to that calculated here
         Pat%bgr=0.0_cp ! nullify the background
       end if
       Pat%al_bgr=.true.
       if(present(deriv)) then
          deriv=0.0
          der=0.0_cp
          do i=1,Pat%npts
            xbk(i) = Pat%x(i)
             bk(i) = 0.0_cp
            do j=1,np
              x=xbk(i)-Par(1,j)
              if(abs(x) <= 20.0_cp*(Par(3,j)+Par(4,j))) then
                 call Split_Pseudovoigt_Der(X,Par(3:6,j),Pv,Der)
              else
                 pv=0.0_cp
                 Der=0.0_cp
              end if
              bk(i)=bk(i)+Par(2,j)*Pv
              Deriv(1,j,i)=Der(1)
              Deriv(2,j,i)=Pv
              Deriv(3:6,j,i)=der(2:5)
            end do
            Pat%bgr(i)=Pat%bgr(i)+bk(i)
          end do
       else
          do i=1,Pat%npts
            xbk(i) = Pat%x(i)
             bk(i) = 0.0_cp
            do j=1,np
              x=xbk(i)-Par(1,j)
              if(abs(x) <= 20.0_cp*(Par(3,j)+Par(4,j))) then
                 call Split_Pseudovoigt_Der(X,Par(3:6,j),Pv)
              else
                 pv=0.0_cp
              end if
              bk(i)=bk(i)+Par(2,j)*Pv
            end do
            Pat%bgr(i)=Pat%bgr(i)+bk(i)
          end do
       end if

    End Subroutine Calc_BackGround_split_pVoigt

    Module Subroutine Calc_BackGround_pVoigt(Pat, Np, Par, deriv)
       class(DiffPat_E_Type),                     intent(in out) :: Pat     !> Diffraction pattern
       integer,                                   intent(in)     :: Np      !> Number of peaks
       real(kind=cp), dimension(:,:),             intent(in)     :: Par     !> Parameters (1:4,Np)
       real(kind=cp), optional, dimension(:,:,:), intent(out)    :: deriv   !> Derivatives w.r.t. parameters (1:4,Np,npts)
       !--- Local variables ---!
       integer :: i,j
       real(kind=cp) :: x,pv
       real(kind=cp), dimension(Pat%npts) :: bk,xbk
       real(kind=cp), dimension(3)        :: der


       if (.not. allocated(Pat%bgr)) then   !if it is already allocated just add the previous
         allocate(Pat%bgr(Pat%npts))        !backgrount to that calculated here
         Pat%bgr=0.0_cp ! nullify the background
       end if
       Pat%al_bgr=.true.
       if(present(deriv)) then
          deriv=0.0
          der=0.0_cp
          do i=1,Pat%npts
            xbk(i) = Pat%x(i)
             bk(i) = 0.0_cp
            do j=1,np
              x=xbk(i)-Par(1,j)
              if(abs(x) <= 30.0_cp*Par(3,j) ) then
                 call PseudoVoigt_Der(X,Par(3:4,j),Pv,Der)
              else
                 pv=0.0_cp
                 Der=0.0_cp
              end if
              bk(i)=bk(i)+Par(2,j)*Pv
              Deriv(1,j,i)=Der(1)
              Deriv(2,j,i)=Pv
              Deriv(3:4,j,i)=der(2:3)
            end do
            Pat%bgr(i)=Pat%bgr(i)+bk(i)
          end do
       else
          do i=1,Pat%npts
            xbk(i) = Pat%x(i)
             bk(i) = 0.0_cp
            do j=1,np
              x=xbk(i)-Par(1,j)
              if(abs(x) <= 30.0_cp*Par(3,j)) then
                 call PseudoVoigt_Der(X,Par(3:4,j),Pv)
              else
                 pv=0.0_cp
              end if
              bk(i)=bk(i)+Par(2,j)*Pv
            end do
            Pat%bgr(i)=Pat%bgr(i)+bk(i)
          end do
       end if

    End Subroutine Calc_BackGround_pVoigt
    !!----
    !!---- READ_BACKGOUND_FILE
    !!----
    !!----    Read background pattern from an external file.
    !!----    Mode:
    !!----         Poly | Inter
    !!----
    !!----
    !!---- 30/04/2019
    !!
    Module Subroutine Read_Background_File(Bck_File, Bck_Mode, Pat)
       !---- Arguments ----!
       character(len=*),         intent(in   )    :: bck_file      ! Path+Filename of Background file
       character(len=*),         intent(in   )    :: bck_mode
       class(DiffPat_E_Type),    intent(inout)    :: Pat

       !---- local variables ----!
       logical                                       :: esta
       character(len=80)                             :: line
       character(len=3)                              :: car
       integer                                       :: bck_points
       integer                                       :: i,j,i_bck
       integer                                       :: ier
       real(kind=cp), dimension (:), allocatable     :: bck_v
       real(kind=cp), dimension (:), allocatable     :: bck_p

       !> Init
       call clear_error()

       inquire(file=trim(bck_file), exist =esta)
       if (.not. esta) then
          Err_CFML%IErr=1
          Err_CFML%Msg="Read_Background_File@DIFFPATT: The file "//trim(bck_file)//" doesn't exist"
          return
       end if

       !> Open Backgriund file
       open(newunit=i_bck,file=trim(bck_file),status="old",action="read",position="rewind",iostat=ier)
       if (ier /= 0) then
          Err_CFML%Ierr=1
          Err_CFML%Msg="Read_Background_File@DIFFPATT: Problems opening the file: "//trim(bck_file)
          return
       end if

       !> Number of background points
       i=0
       do
          read(unit=i_bck,fmt="(a)",iostat=ier) line
          if (ier /= 0) exit

          if (len_trim(line) == 0)  cycle
          if (index(line,"!") /= 0) cycle
          if (index(line,"#") /= 0) cycle
          i=i+1
       end do
       bck_points=i

       if (bck_points <=0) then
          Err_CFML%IErr=1
          Err_CFML%Msg="Read_Background_File@DIFFPATT: Impossible to read any background point in the file: "//trim(bck_file)
          close (unit=i_bck)
          return
       end if

       !> Allocating variables
       if (allocated(bck_v)) deallocate(bck_v)
       allocate(bck_v(bck_points+1))
       bck_v=0.0_cp

       if (allocated(bck_p)) deallocate(bck_p)
       allocate(bck_p(bck_points+1))
       bck_p=0.0_cp

       rewind(unit=i_bck)
       j=0
       do
          read(unit=i_bck,fmt="(a)",iostat=ier) line
          if (ier /= 0) exit

          if (len_trim(line) == 0 .or. line(1:1) == "!" .or. line(1:1)=="#") cycle

          j=j+1
          read(unit=line, fmt=*, iostat=ier)  bck_p(j), bck_v(j)
          if (ier /= 0) then
             bck_points=j-1
             close(unit=i_bck)

             Err_CFML%Ierr=1
             ERR_CFML%Msg="Read_Background_File@DIFFPATT: Problems during loading the background points!"
             exit
          end if
       end do
       close (unit=i_bck)

       bck_points=j
       if (bck_points <=0) then
          Err_CFML%IErr=1
          Err_CFML%Msg="Read_Background_File@DIFFPATT: Zero background points in the file: "//trim(bck_file)
          return
       end if

       car=u_case(adjustl(bck_mode))
       select case (car)
          case ("POL") ! Polynomial
             call set_background_poly (Pat, 50.0_cp, bck_p, bck_points )

          case ("INT") ! Interpolation
             call  set_background_inter (Pat, bck_v, bck_p, bck_points )

          case default
             Err_CFML%IErr=1
             ERR_CFML%Msg="Read_Background_File@DIFFPATT: Define the mode: Polynomial or Interpolation"
             return
       end select
    End Subroutine Read_Background_File

    !!--++
    !!--++ SET_BACKGROUND_POLY
    !!--++
    !!--++    (PRIVATE)
    !!--++    Define a n-polynomial with constant value at bkpos Background
    !!--++
    !!--++ 30/04/2019
    !!
    Module Subroutine Set_Background_Poly(Pat, Bkpos, Bckx, N)
       !---- Arguments ----!
       class(DiffPat_E_Type),         intent(in out) :: Pat
       real (kind=cp),                intent(in    ) :: bkpos
       real (kind=cp), dimension(:),  intent(in    ) :: bckx
       integer,                       intent(in    ) :: n

       !---- Local Variables ----!
       integer :: i,j

       if (allocated(pat%bgr)) deallocate(pat%bgr)
       allocate(pat%bgr(pat%npts))
       pat%bgr=0.0_cp
       pat%al_bgr=.true.

       do i=1, pat%npts
          do j=1,n
             pat%bgr(i)= pat%bgr(i) + bckx(j)*((pat%x(i)/bkpos-1.0)**(j-1))
          end do
       end do
    End Subroutine Set_Background_Poly

    !!--++
    !!--++ SET_BACKGROUND_INTER
    !!--++
    !!--++    (PRIVATE)
    !!--++    Define a Background
    !!--++
    !!--++ 30/04/2019
    !!
    Module Subroutine Set_Background_Inter(Pat, Bcky, Bckx, N)
       !---- Arguments ----!
       class(DiffPat_E_Type),         intent(in out) :: Pat
       real (kind=cp), dimension(:),  intent(in out) :: bcky
       real (kind=cp), dimension(:),  intent(in out) :: bckx
       integer,                       intent(in    ) :: n

       !---- Local variables ----!
       integer        :: nbx, nbac1 , i , j  , nxx
       real(kind=cp)  :: difl, difr , thx , delt, slope, bstep,p,step

       nbx=1
       nbac1=n

       difl=bckx(1)-pat%xmin
       difr=bckx(n)-pat%xmax

       if (difl >= 0) then
          if (pat%ct_step) then
             step=pat%x(2)-pat%x(1)
             nbx=difl/step + 1.5
          else
             nbx=locate(pat%x,bckx(1))
             if (nbx <= 0) nbx=1
          end if
          do i=1,nbx
             pat%bgr(i)=bcky(1)
          end do
       end if

       if (difr <= 0) then
          nbac1=n+1
          bckx(nbac1)=pat%xmax
          bcky(nbac1)=bcky(n)
       end if

       nxx=2
       do_i: do i=nbx,pat%npts
          thx=pat%x(i)
          do j=nxx,nbac1
             delt=bckx(j)-thx
             if (delt > 0.0) then
                p=bckx(j)-bckx(j-1)
                if (abs(p) > eps) then
                   slope=(bcky(j)-bcky(j-1))/p
                else
                   slope=0.0
                end if
                bstep=(thx-bckx(j-1))*slope
                pat%bgr(i)=bcky(j-1)+bstep
                nxx=j-1
                cycle do_i
             end if
          end do
       end do  do_i
    End Subroutine Set_Background_Inter

End Submodule DiffP_BackgPatt
