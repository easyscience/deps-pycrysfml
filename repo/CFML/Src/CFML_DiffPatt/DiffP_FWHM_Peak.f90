!!----
SubModule (CFML_DiffPatt) DiffP_FWHM_Peak

 implicit none

   Contains
   !!----
   !!---- FWHM_PEAK
   !!----
   !!---- Function that calculate the FHWM of a peak situated on (xi,yi). Then
   !!---- the routine search the Ym value in the range (xi-rlim, xi+rlim) to
   !!---- obtain the FWHM.
   !!----
   !!---- The function return a negative values if an error is ocurred during calculation
   !!----
   !!---- 30/04/2019
   !!
   Module Function FWHM_Peak(Pat, Xi, Yi, Ybi, RLim) Result(v)
      !---- Arguments ----!
      class(DiffPat_Type),       intent(in) :: Pat      ! Pattern object
      real(kind=cp),             intent(in) :: Xi       ! (Xi,Yi) for point i
      real(kind=cp),             intent(in) :: Yi       !
      real(kind=cp),             intent(in) :: Ybi      ! Background at point i
      real(kind=cp),optional,    intent(in) :: RLim     ! Limit range in X units to search the point
      real(kind=cp)                         :: V

      !---- Local variables ----!
      integer        :: j, i1, j1,n,nlim
      real(kind=cp)  :: xml, xmr, ym, x1, x2, y1, y2
      real(kind=cp)  :: difx


      !> Init value
      v=-1.0

      !> Y value for FHWM
      ym=0.5*(yi-ybi) + ybi

      !> Limit to search
      difx=pat%x(2)-pat%x(1)
      if (present(rlim)) then
         nlim=nint(rlim/difx)
      else
         nlim=nint(0.5/difx)     ! 0.5
      end if

      !> Locating the index that X(i1) <= Xi < X(i1+1)
      i1=0
      i1=locate(Pat%x,xi)
      if (i1 <=0 .or. i1 > Pat%npts) return  ! Error in the search

      !> Searching on Left side: Y(j1) <= ym < Y(j1+1)
      n=max(1,i1-nlim)
      j1=0
      do j=i1,n,-1
         if (pat%y(j) < ym) then
            j1=j
            exit
         end if
      end do
      if (j1 <= 0) j1=i1-1

      x1=Pat%x(j1)
      y1=Pat%y(j1)
      x2=Pat%x(j1+1)
      y2=Pat%y(j1+1)
      xml= x1 + ((ym-y1)/(y2-y1) )*(x2-x1)

      !> Searching on Right side: Y(j1) <= yn < Y(j1+1)
      n=min(i1+nlim,pat%npts)
      j1=0
      do j=i1,n
         if (pat%y(j) < ym) then
            j1=j
            exit
         end if
      end do
      if (j1 ==0) j1=i1

      x1=Pat%x(j1-1)
      y1=Pat%y(j1-1)
      x2=Pat%x(j1)
      y2=Pat%y(j1)
      xmr= x1 + ((ym-y1)/(y2-y1) )*(x2-x1)

      v=xmr-xml
   End Function FWHM_Peak

End SubModule DiffP_FWHM_Peak