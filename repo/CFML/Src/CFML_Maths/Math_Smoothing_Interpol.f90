!!----
!!---- SUBMODULE CFML_Math_General
!!----
!!----
!!
Submodule (CFML_Maths) Maths_Smoothing_Interpol
 implicit none
 Contains

    !!----
    !!---- SECOND_DERIVATIVE
    !!----    Calculate the second derivate of N Points
    !!----
    !!---- 04/04/2019
    !!
    Pure Module Function Second_Derivative(x,y,n) Result(d2y)
       !---- Arguments ----!
       real(kind=cp), dimension(:), intent(in)  :: x     ! Vector xi
       real(kind=cp), dimension(:), intent(in)  :: y     ! Vector Yi=F(xi)
       integer ,                    intent(in)  :: n     ! Dimension
       real(kind=cp), dimension(n)              :: d2y   ! Second derivate

       !---- Local Variables ----!
       integer                     :: i, k
       real(kind=cp), dimension(n) :: u
       real(kind=cp)               :: yp1, ypn, sig, p, qn, un

       yp1=(y(2) - y(1))   / (x(2) - x(1))     ! derivative at point 1
       ypn=(y(n) - y(n-1)) / (x(n) - x(n-1))   ! derivative at point n

       d2y(1)=-0.5
       u(1)=(3.0/(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)

       do i=2,n-1
          sig=(x(i)-x(i-1))/(x(i+1)-x(i-1))
          p=sig*d2y(i-1)+2.0
          d2y(i)=(sig-1.0)/p
          u(i)=(6.0*((y(i+1)-y(i))/(x(i+1)-x(i))-(y(i)-y(i-1))/(x(i)-x(i-1)))  &
               /(x(i+1)-x(i-1))-sig*u(i-1))/p
       end do

       qn=0.5
       un=(3.0/(x(n)-x(n-1)))*(ypn-(y(n)-y(n-1))/(x(n)-x(n-1)))
       d2y(n)=(un-qn*u(n-1))/(qn*d2y(n-1)+1.0)
       do k=n-1,1,-1
          d2y(k)=d2y(k)*d2y(k+1)+u(k)
       end do

    End Function Second_Derivative

    !!----
    !!---- SMOOTHING_VEC
    !!----    Procedure to smooth the vector values
    !!----
    !!---- 04/04/2019
    !!
    Pure Module Function Smoothing_Vec(Y, N, Niter) Result(Ys)
       !---- Arguments ----!
       real(kind=cp),dimension(:),            intent(in) :: Y         !  In Out-> Array to be smoothed
       integer,                               intent(in) :: n         !  In -> Number of points
       integer,                               intent(in) :: niter     !  In -> Number of iterations
       real(kind=cp),dimension(n)                        :: Ys        !  Out-> Array smoothed

       !---- Local Variables ----!
       integer                     :: n1, n2
       integer                     :: i, iter
       real(kind=cp), dimension (n):: datYs


       n1 = 4
       n2 = n-3

       do iter = 1 ,niter
          datYs(n1-1)=((Y(n1-2)+Y(n1))*10.0+(Y(n1-3)+Y(n1+1))*5.0+Y(n1+2))/31.0
          datYs(n1-2)=((Y(n1-3)+Y(n1-1))*10.0+Y(n1)*5.0+Y(n1+1))/26.0
          datYs(n1-3)=(Y(n1-2)*10.0+Y(n1-1)*5.0+Y(n1))/16.0

          do i=n1,n2
             datYs(i)=(Y(i-3)+Y(i+3)+5.0*(Y(i-2)+Y(i+2))+10.0*(Y(i-1)+Y(i+1)))/ 32.0
          end do

          datYs(n2+1)=((Y(n2+2)+Y(n2))*10.0+(Y(n2+3)+Y(n2-1))*5.0+Y(n2-2))/31.0
          datYs(n2+2)=((Y(n2+3)+Y(n2+1))*10.0+Y(n2)*5.0+Y(n2-1))/26.0
          datYs(n2+3)=(Y(n2+2)*10.0+Y(n2+1)*5.0+Y(n2))/16.0
       end do

       Ys=datYs

    End Function Smoothing_Vec

    !!----
    !!---- SPLINT
    !!----    Spline Interpolation
    !!----
    !!---- 04/04/2019
    !!
    Pure Module Function Spline_Interpol(xi,x,y,d2y,n) Result(yi)
       !---- Arguments ----!
       real(kind=cp),               intent(in)  :: xi   ! X value for evaluation
       real(kind=cp), dimension(:), intent(in)  :: x    ! Vector Xi points
       real(kind=cp), dimension(:), intent(in)  :: y    ! Vector Yi points
       real(kind=cp), dimension(:), intent(in)  :: d2y  ! Vector Second derivate of Yi points
       integer ,                    intent(in)  :: n    ! Dimension of vectors
       real(kind=cp)                            :: yi

       !---- Local Variables ----!
       integer          :: klo, khi, k
       real(kind=cp)    :: h, a, b

       !> Init
       yi=0.0_cp

       klo=1
       khi=n
       do
          if (khi-klo > 1) then
             k=(khi+klo)/2
             if (x(k) > xi) then
                khi=k
             else
                klo=k
             end if
             cycle
          else
             exit
          end if
       end do

       h=x(khi)-x(klo)
       a=(x(khi)-xi)/h
       b=(xi-x(klo))/h
       yi=a*y(klo)+b*y(khi)+((a**3-a)*d2y(klo)+(b**3-b)* d2y(khi))*(h**2)/6.0

    End Function Spline_Interpol

    !!----
    !!---- LINEAR_INTERPOL
    !!----    Simple Linear Interpolation
    !!----
    !!---- 04/04/2019
    !!
    Pure Module Function Linear_Interpol(xi,x,y) Result(yi)
       !---- Arguments ----!
       real(kind=cp),              intent(in)   :: xi ! X point to evaluate
       real(kind=cp), dimension(:),intent(in)   :: x  ! Vector containing Xi points
       real(kind=cp), dimension(:),intent(in)   :: y  ! Vector Yi=F(xi)
       real(kind=cp)                            :: yi ! Output

       !--- Local variables ---!
       integer       :: i,np
       real(kind=cp) :: slope

       np=size(x)
       i=locate(x,xi,np)
       slope=(y(i+1)-y(i))/(x(i+1)-x(i))
       yi=(xi-x(i))*slope+y(i)

    End Function Linear_Interpol

    !!----
    !!---- FIRST_DERIVATIVE
    !!----    Calculate the First derivate values of the N points
    !!----
    !!---- 04/04/2019
    !!
    Pure Module Function First_Derivative(x,y,n) Result(d1y)
       !---- Arguments ----!
       real(kind=cp), dimension(:), intent(in)  :: x    ! Vector containing Xi
       real(kind=cp), dimension(:), intent(in)  :: y    ! Vector containing Yi
       integer ,                    intent(in)  :: n    ! Dimension
       real(kind=cp), dimension(n)              :: d1y  ! Vector containing the first derivative

       !---- Local Variables ----!
       integer                      :: i
       real(kind=cp)                :: step, x0, y0, y1, y2
       real(kind=cp), dimension(n)  :: d2y

       !> Init
       d1y=0.0_cp

       !> Calling the calculation of the second derivative
       d2y=second_derivative(x,y,n)

       !> Calculation
       do i=1,n
         if (i /= n) then
           step = x(i+1)-x(i)
         end if
         x0 = x(i) - step/2.0_cp
         y0 = spline_interpol(x0,x,y, d2y, n)

         y1 = y0
         x0 = x(i) + step/2.0_cp
         y0 = spline_interpol(x0,x,y, d2y, n)
         y2 = y0
         d1y(i) = (y2 - y1) / step
       end do

    End Function First_Derivative

    !!----
    !!---- SPLINE
    !!----    Second derivative for points using Spline
    !!----
    !!---- 04/04/2019
    !!
    Pure Module Function Spline_D2Y(x,y,n,yp1,ypn) Result(Ys)
       !---- Arguments ----!
       real(kind=cp), dimension(:), intent(in)  :: x               !  In -> Array X
       real(kind=cp), dimension(:), intent(in)  :: y               !  In -> Array Yi=F(Xi)
       integer ,                    intent(in)  :: n               !  In -> Dimension of X, Y
       real(kind=cp),               intent(in)  :: yp1             !  In -> Derivate of Point 1
       real(kind=cp),               intent(in)  :: ypn             !  In -> Derivate of Point N
       real(kind=cp), dimension(n)              :: ys              ! Out -> array containing second derivatives

       !---- Local Variables ----!
       integer                     :: i, k
       real(kind=cp), dimension(n) :: u
       real(kind=cp)               :: sig, p, qn, un

       !> Init
       Ys=0.0_cp

       if (yp1 > huge(1.0_cp)) then
          ys(1)=0.0
          u(1)=0.0
       else
          ys(1)=-0.5
          u(1)=(3.0/(x(2)-x(1)))*((y(2)-y(1))/(x(2)-x(1))-yp1)
       end if

       do i=2,n-1
          sig=(x(i)-x(i-1))/(x(i+1)-x(i-1))
          p=sig*ys(i-1)+2.0
          ys(i)=(sig-1.0)/p
          u(i)=(6.0*((y(i+1)-y(i))/(x(i+1)-x(i))-(y(i)-y(i-1))/(x(i)-x(i-1)))  &
               /(x(i+1)-x(i-1))-sig*u(i-1))/p
       end do
       if (ypn > huge(1.0_cp)) then
          qn=0.0
          un=0.0
       else
          qn=0.5
          un=(3.0/(x(n)-x(n-1)))*(ypn-(y(n)-y(n-1))/(x(n)-x(n-1)))
       end if
       ys(n)=(un-qn*u(n-1))/(qn*ys(n-1)+1.0)
       do k=n-1,1,-1
          ys(k)=ys(k)*ys(k+1)+u(k)
       end do

    End Function Spline_D2Y

End Submodule Maths_Smoothing_Interpol
