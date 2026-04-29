!!----
!!----
!!----
SubModule (CFML_EoS) EoS_Conlev
   implicit none
   Contains

   !!----
   !!---- CALC_CONLEV
   !!----    Performs the calculations of one confidence ellipse
   !!----    for a pair of EoS parameters refined by least-squares
   !!----
   !!---- 18/07/2016
   !!---- Revision: OK
   !!
   Module Subroutine Calc_Conlev(Eos, ix, iy, isig, xyy, n)
      !---- Arguments ----!
      type(Eos_Type),              intent(in)  :: Eos          ! EoS with refined parameters
      integer,                     intent(in)  :: ix,iy        ! input pointers to two variables in the variance-covariance matrix
      integer,                     intent(in)  :: isig         ! requested confidence level, values 1-6
      real(kind=cp),dimension(:,:),intent(out) :: xyy          ! output points for plotting; array must by of dimension >100
      integer,                     intent(out) :: n            ! number of data output

      !---- Local Variables ----!
      integer                    :: ilast,nlimit
      real(kind=cp)              :: c11,c22,c12,det
      real(kind=cp)              :: cinv11,cinv22,cinv12,a,b,c,root
      real(kind=cp)              :: xe,xs,xinc,x,y1,y2

      !> init
      nlimit=size(xyy,dim=2)

      n=0
      xyy=0.0_cp

      !> Check
      if (isig < 1 .or. isig > 6) then
         call set_error(1,"Confidence level is out of range")
         return
      end if

      !> Copy over vcv values
      c11=eos%vcv(ix,ix)
      c22=eos%vcv(iy,iy)
      c12=eos%vcv(ix,iy)

      !> Invert matrix
      det=c11*c22-c12*c12
      if (abs(det) <= tiny(0.0_cp)) then
         call set_error(1, "Determinant value is zero in the Confidence ellipses calculation")
         return
      end if

      cinv22=c11/det
      cinv11=c22/det
      cinv12=-1.0_cp*c12/det

      xe=sqrt(cinv22*delchi(isig)/(cinv22*cinv11-cinv12**2.0_cp))    !x at the end
      xs=-1.0_cp*xe
      xinc=sqrt(c11)/10.0_cp

      !> at x=xs equal roots, calculate explicity
      x=xs
      y1= -1.0_cp*cinv12*xs/cinv22  ! gives equal roots
      y2=y1

      ilast=0
      n=n+1
      xyy(1,n)=x+eos%params(ix)
      xyy(2,n)=y1+eos%params(iy)
      xyy(3,n)=y2+eos%params(iy)

      !> loop over x starts here
      do
         if (abs(x-xe) < xinc .or. abs(x-xs) < xinc) then
            x=x+0.2*xinc     ! increment x
         else                ! small step close to end
            x=x+xinc
         end if

         if (x > xe) then               ! last of this delchi
            ilast=1                     ! calc for x=xe which
            y1= -1.0*cinv12*xe/cinv22   ! gives equal roots
            y2=y1
            x=xe
         else
            ! solve for y
            a=cinv22
            b=2.0*cinv12*x
            c=cinv11*x*x-delchi(isig)
            root=b*b-4.0*a*c
            if (root < 0.0) cycle                  ! error: try next x
            y1= (sqrt(root)-b)/2.0/a
            y2= -1.0*(sqrt(root)+b)/2.0/a
         end if

         n=n+1
         xyy(1,n)=x+eos%params(ix)
         xyy(2,n)=y1+eos%params(iy)
         xyy(3,n)=y2+eos%params(iy)
         if (ilast /= 0) exit

         if (n == nlimit) then
            call set_error(1,"Number of points arrived to the limit for Confidence ellipses")
            exit
         end if
      end do

   End Subroutine Calc_Conlev

   !!----
   !!---- WRITE_DATA_CONLEV
   !!----    Writes out confidence ellipse data to a file
   !!----
   !!---- 05/12/2015
   !!---- Revision: OK
   !!
   Module Subroutine Write_Data_Conlev(Xyy, N, Iout)
      !---- Arguments ----!
      real(kind=cp),dimension(:,:),intent(in)   :: xyy  ! output points for plotting
      integer,                     intent(in)   :: n    ! Number of points
      integer, optional,           intent(in)   :: iout ! Iunit output

      !---- Local Variables ----!
      integer :: lun,i

      !> Check
      if (n < 1) return

      !> Unit to print the data
      lun=6
      if (present(iout)) lun=iout

      do i=1,n
         write(lun,'(3x,a,3x,a,3x,a)') trim(string_real(xyy(1,i),10)), &
                                       trim(string_real(xyy(2,i),10)), &
                                       trim(string_real(xyy(3,i),10))
      end do

   End Subroutine Write_Data_Conlev

   !!----
   !!---- WRITE_INFO_CONLEV
   !!----    Writes out header info specific to a confidence ellipse
   !!----
   !!---- 05/12/2015
   !!---- Revision: OK
   !!
   Module Subroutine Write_Info_Conlev(Eos,ix,iy,isig,iout)
      !---- Arguments ----!
      type(Eos_Type),    intent(in) :: Eos
      integer,           intent(in) :: ix,iy,isig  ! parameter numbers for the plot, and sigma level
      integer, optional, intent(in) :: iout

      !---- Local Variables ----!
      integer :: lun

      !> Unit to print the information
      lun=6
      if (present(iout)) lun=iout

      !> Info
      write(lun,'(//a,f6.2,a)') "Coordinates for confidence ellipse at confidence level of ",delchi_levels(isig),"%"

      write(lun,'(3x,"X axis as ",a," with variance = ",a)') trim(eos%parname(ix)), &
                                                             trim(string_real(eos%vcv(ix,ix),10))
      write(lun,'(3x,"Y axis as ",a," with variance = ",a)') trim(eos%parname(iy)), &
                                                             trim(string_real(eos%vcv(iy,iy),10))
      write(lun,'(3x,"The covariance of the two variables = ",a)') trim(string_real(eos%vcv(ix,iy),10))

      write(lun,'(//,a,//)') &
                "Data points for plotting: first column is X with two columns with the two Y values at that X"


   End Subroutine Write_Info_Conlev

End SubModule EoS_Conlev
