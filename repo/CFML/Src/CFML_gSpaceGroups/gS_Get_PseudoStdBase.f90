!!----
!!----
!!----
!!----
SubModule (CFML_gSpaceGroups) gS_Get_PseudoStdBase
   implicit none
   Contains
   !!----
   !!---- GET_PSEUDO_STANDARD_BASE
   !!----
   !!---- Given the rotation matrix of the principal axis W, the shortest
   !!---- lattice vector along the axis, bz, and the four shortest lattice
   !!---- vectors perpendicular to that axis, perpAxis, computes the basis
   !!---- bx,by,bz which gives the smallest unit cell
   !!
   !!---- 24/04/2019
   !!
   Module Subroutine Get_Pseudo_Standard_Base(W,perpAxis,bz,bx,by)
       !---- Arguments ----!
       type(rational), dimension(3,3), intent(in)  :: W
       type(rational), dimension(3,4), intent(in)  :: perpAxis
       type(rational), dimension(3),   intent(in)  :: bz
       type(rational), dimension(3),   intent(out) :: bx
       type(rational), dimension(3),   intent(out) :: by

       !---- Local variables ----!
       integer                           :: i,j,n,imin,order
       type(rational)                    :: detW,minDet
       type(rational), dimension(3,3)    :: A,B
       type(rational), dimension(:),   allocatable :: det
       integer,        dimension(:,:), allocatable :: test_
       type(rational), dimension(:,:), allocatable :: byAux

       !> Init
       call Clear_Error()
       detW= rational_determ(W)
       if (detW%Numerator == detW%Denominator) then
          A = W
       else if (detW%Numerator == -detW%Denominator) then
          A = -W
       else
          Err_CFML%Ierr = 1
          Err_CFML%Msg  = "Get_Pseudo_Standard_Base@SPACEG: Determinant is not +-1"
          return
       end if

       B(:,3)   = bz(:)
       order=abs(Get_Rotation_Order(A))
       select case (order)
          case (2)
             Allocate(det(6),test_(6,2))
             n = 0
             do i = 1 , 4
                do j = i + 1 , 4
                   n = n + 1
                   test_(n,1) = i
                   test_(n,2) = j
                   B(:,1)     = perpAxis(:,i)
                   B(:,2)     = perpAxis(:,j)
                   det(n)     = rational_determ(B)
                end do
             end do

             !> Select the combination with the smallest determinant
             imin   = 1
             minDet = abs(det(1))
             do i = 2 , 6
                if (abs(det(i)) < minDet) then
                   imin   = i
                   minDet = abs(det(i))
                end if
             end do

             bx(:) = perpAxis(:,test_(imin,1))
             by(:) = perpAxis(:,test_(imin,2))

          case(3,4,6)
             allocate(det(4),test_(4,1),byAux(3,4))
             do i = 1 , 4
                byAux(:,i) = matmul(A,perpAxis(:,i))
                B(:,1)     = perpAxis(:,i)
                B(:,2)     = byAux(:,i)
                det(i)     = rational_determ(B)
             end do

             imin = 1
             minDet = abs(det(1))
             do i = 2 , 4
                if (abs(det(i)) < minDet) then
                   imin = i
                   minDet = abs(det(i))
                end if
             end do

             bx(:) = perpAxis(:,imin)
             by(:) = byAux(:,imin)
       end select

   End Subroutine Get_Pseudo_Standard_Base

End SubModule gS_Get_PseudoStdBase