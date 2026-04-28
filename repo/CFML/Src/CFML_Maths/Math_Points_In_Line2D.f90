!!----
!!---- SUBMODULE CFML_Math_General
!!----
!!----
!!
Submodule (CFML_Maths) Maths_Points_In_Line2D
 implicit none
 Contains
    !!----
    !!---- POINTS_IN_LINE2D
    !!----    The routine calculate N points belonging to the line defined
    !!----    by X1 and Xn with equal distance between them. XP contains
    !!----    X1,X2,.....,XN points.
    !!----
    !!---- 04/04/2019
    !!
    Pure Module Subroutine Points_In_Line2D(X1, XN, N, XP)
       !---- Arguments ----!
       real(kind=cp), dimension(2),   intent(in)  :: X1   ! Point1 in 2D
       real(kind=cp), dimension(2),   intent(in)  :: XN   ! PointN in 2D
       integer,                       intent(in)  :: N    ! Number of Total points
       real(kind=cp), dimension(:,:), intent(out) :: XP   ! List of points

       !---- Local Variables ----!
       integer          :: i
       real(kind=cp)    :: ml,bl,dl,t
       real(kind=cp)    :: a,b,c,d
       real(kind=cp)    :: xa,xb

       xp=0.0_cp
       if (n <= 1) return

       !> Calculating the distance between two points to
       !> eliminate rare considerations as the same point
       dl=sqrt( (xn(1)-x1(1))**2 + (xn(2)-x1(2))**2 )
       if (dl <= 0.0001) return

       !>- When N=2 is trivial case
       if (n == 2) then
          xp(:,1)=x1
          xp(:,2)=xn
          return
       end if

       !---- Case 1: Y=cte ----!
       !Xn(2) and X1(2) are equal, then we have a line  with Y=cte
       if (abs(xn(2)-x1(2)) <= 0.0001) then
          dl=abs(xn(1)-x1(1))
          d=dl/real(n-1)
          xp(:,1)=x1
          if (xn(1) > x1(1)) then
             do i=2,n-1
                xp(1,i)=xp(1,i-1)+d
                xp(2,i)=xp(2,1)
             end do
          else
             do i=2,n-1
                xp(1,i)=xp(1,i-1)-d
                xp(2,i)=xp(2,1)
             end do
          end if
          xp(:,n)=xn

          return
       end if

       !---- Case 2: X=cte ----!
       !Xn(1) - X1(1) are equal, then we have a line with X=cte
       if (abs(xn(1)-x1(1)) <= 0.0001) then
          dl=abs(xn(2)-x1(2))
          d=dl/real(n-1)
          xp(:,1)=x1
          if (xn(2) > x1(2)) then
             do i=2,n-1
                xp(1,i)=xp(1,1)
                xp(2,i)=xp(2,i-1)+d
             end do
          else
             do i=2,n-1
                xp(1,i)=xp(1,1)
                xp(2,i)=xp(2,i-1)-d
             end do
          end if
          xp(:,n)=xn

          return
       end if

       !---- Case 3: General case ----!
       ml=(x1(2)-xn(2))/(x1(1)-xn(1))
       bl=x1(2) - (ml * x1(1))

       !---- Distance between X1 and XN ----!
       dl=sqrt( (xn(1)-x1(1))**2 + (xn(2)-x1(2))**2 )

       !---- Creating the list ----!
       a=ml**2 + 1.0
       b=2.0 *( ml*(bl-x1(2)) -x1(1) )

       xp(:,1)=x1
       do i=2,n-1
          t=(dl**2)*((real(i-1)/real(n-1))**2)
          c=(x1(2)-bl)**2 + x1(1)**2 - t

          xa=(-b + sqrt(b**2 - 4.0*a*c))/(2.0*a)
          xb=(-b - sqrt(b**2 - 4.0*a*c))/(2.0*a)
          if (x1(1) <= xa .and. xa <= xn(1)) then
             xp(1,i)=xa
             xp(2,i)=ml*xa+bl
          else
             xp(1,i)=xb
             xp(2,i)=ml*xb+bl
          end if
       end do
       xp(:,n)=xn

    End Subroutine Points_In_Line2D

End Submodule Maths_Points_In_Line2D
