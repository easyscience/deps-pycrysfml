SubModule (CFML_gSpaceGroups) gS_Get_OriginShift
   implicit none
   Contains
   !!----
   !!----  GET_ORIGIN_SHIFT
   !!----
   !!---- Tries to make G = G_ by an origin shift. If a solution is found,
   !!---- it returns shift = .true. P is used to express G and G_ in a
   !!---- primitive basis
   !!----
   !!---- 22/04/2019
   !!
   Module Subroutine Get_Origin_Shift(ng, G, G_, P_, origShift, shift)
      !---- Arguments ----!
      integer,                             intent(in)  :: ng
      type(symm_oper_type), dimension(ng), intent(in)  :: G
      type(symm_oper_type), dimension(ng), intent(in)  :: G_
      type(rational), dimension(3,3),      intent(in)  :: P_
      type(rational), dimension(3),        intent(out) :: origShift
      logical,                             intent(out) :: shift

      !---- Local Variables ----!
      integer                           :: i,j,k,l,r,nr
      type(rational), dimension(3,3)    :: identity
      type(rational), dimension(4,4)    :: P,Pinv
      type(rational), dimension(4,4,ng) :: Gx,Gt
      type(rational), dimension(:,:), allocatable :: U,D,T,V
      type(rational), dimension(:,:), allocatable :: b,x

      !> Init
      call Rational_Identity_Matrix(P)
      call Rational_Identity_Matrix(identity)
      shift      = .true.

      P(1:3,1:3) = P_
      do i = 1 , ng
         Gx(:,:,i)  = G(i)%Mat
         Gt(:,:,i)  = G_(i)%Mat
      end do
      Pinv=Rational_Inverse_Matrix(P)

      !> Transform generators to a primitive setting
      do i = 1 , ng
         Gt(:,:,i) = matmul(Gt(:,:,i),P)
         Gt(:,:,i) = matmul(Pinv,Gt(:,:,i))
         Gx(:,:,i) = matmul(Gx(:,:,i),P)
         Gx(:,:,i) = matmul(Pinv,Gx(:,:,i))
      end do

      !> Build the equation to compute the origin shift
      !>      U x cp = b (mod Z)
      if (.not. allocated(U)) then
         nr = 3 * ng
         allocate(U(nr,3))
         allocate(b(nr,1))
         allocate(x(3,1))
         allocate(D(nr,3))
         allocate(T(nr,nr))
         allocate(V(3,3))
      end if
      r = 0
      do j = 1 , ng
         do k = 1 , 3
            r = r + 1
            b(r,1) = Gt(k,4,j) - Gx(k,4,j)
            do l = 1 , 3
               U(r,l) = Gt(k,l,j) - identity(k,l)
            end do
         end do
      end do
      call Rational_SmithNormalForm(U,D,T,V)
      b = matmul(T,b)
      do j = 1 , 3
         if (D(j,j) == (0//1)) then
            if (mod(b(j,1)%Numerator,b(j,1)%Denominator) /= 0) then
               shift = .false.
               return
            else
               x(j,1) = (0//1)
            end if
         else
            x(j,1) = b(j,1) / D(j,j)
         end if
      end do
      do j = 4 , nr
         if (mod(b(j,1)%Numerator,b(j,1)%Denominator) /= 0) then
            shift = .false.
            return
         end if
      end do
      if (shift) then
         x = matmul(V,x)
         x = matmul(P(1:3,1:3),x)
         origShift = x(:,1)
      end if

   End Subroutine Get_Origin_Shift

End Submodule gS_Get_OriginShift