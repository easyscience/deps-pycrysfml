SubModule (CFML_gSpaceGroups) gS_Get_Rotations
   implicit none
   Contains

   !!----
   !!---- SET_RIGHT_HANDEDNESS
   !!----
   !!---- 22/04/2019
   !!
   Module Subroutine Set_Right_Handedness(A)
      !---- Arguments ----!
      type(rational), dimension(3,3), intent(inout) :: A

      !---- Local variables ----!
      type(rational)               :: det
      type(rational), dimension(3) :: row

      det = rational_determ(A)

      if (det < (0//1)) then
          row(:) = A(:,1)
          A(:,1) = A(:,2)
          A(:,2) = row(:)
      end if
   End Subroutine Set_Right_Handedness

   !!----
   !!---- POSITIVE_SENSEROT
   !!----
   !!---- Evaluates if axis corresponds with the positive sense of rotation
   !!---- of the rotation matrix W.
   !!----
   !!---- 22/04/2019
   !!
   Module Function Positive_SenseRot(W, Axis) Result(Positive)
      !---- Arguments ----!
      type(rational), dimension(3,3), intent(in)  :: W
      type(rational), dimension(3),   intent(in)  :: axis
      logical                                     :: positive

      !---- Local variables ----!
      type(rational)                 :: det
      type(rational), dimension(3,3) :: U

      det = rational_determ(W)
      if (det < (0//1)) then
          U = -W
      else
          U = W
      end if

      if (axis(2) == (0//1) .and. axis(3) == (0//1) .and. (axis(1) * U(3,2)) > (0//1)) then
          positive = .true.

      else if ( (U(2,1) * axis(3) - U(3,1) * axis(2)) > (0//1) ) then
          positive = .true.

      else
          positive = .false.
      end if
   End Function Positive_SenseRot

   !!----
   !!---- GET_ROTATION_AXIS
   !!----
   !!---- It computes the shortest lattice vector along the rotation
   !!---- axis of the symmetry operation W
   !!----
   !!---- 22/04/2019
   !!
   Module Function Get_Rotation_Axis(W) Result(axis)
      !---- Arguments ----!
      type(rational), dimension(3,3), intent(in)  :: W       !rotation matrix
      type(rational), dimension(3)                :: axis    !shortest vector along the rotation axisP

      !---- Local variables ----!
      integer                        :: i,j,rnk,nzeros_aux
      integer,        dimension(3)   :: nzeros
      logical                        :: ordered
      type(rational)                 :: det
      type(rational), dimension(3,3) :: A,U
      type(rational), dimension(3)   :: row

      !> Init
      axis=0//1

      det = rational_determ(W)
      if (det%Numerator == det%Denominator) then
         A = W
      else if (det%Numerator == -det%Denominator) then
         A = -W
      else
         Err_CFML%Ierr = 1
         Err_CFML%Msg = "Get_Rotation_Axis@SPACEG:  Determinant is not +-1"
         return
      end if

      do i = 1 , 3
         A(i,i) = A(i,i) - (1//1)
      end do

      U = A
      call Rational_RowEchelonForm(U)
      rnk=Rational_Rank(U)
      if ( rnk /= 2 ) then
         Err_CFML%Ierr = 1
         Err_CFML%Msg = "Get_Rotation_Axis@SPACEG: Rank of matrix U is not two"
         return
      end if

      !> If there is a row with all zero entries,
      !> we put it in the last row
      nzeros = 0
      do i = 1 , 3
         do j = 1 , 3
            if ( U(i,j) == (0//1) ) then
               nzeros(i) = nzeros(i) + 1
            else
               exit
            end if
         end do
      end do

      do
         i = 1
         ordered = .true.
         do i = 1 , 2
            if (nzeros(i+1) < nzeros(i)) then
               nzeros_aux  = nzeros(i)
               row(:)      = U(i,:)
               U(i,:)      = U(i+1,:)
               U(i+1,:)    = row(:)
               nzeros(i)   = nzeros(i+1)
               nzeros(i+1) = nzeros_aux
               ordered      = .false.
            end if
         end do
         if ( ordered ) exit
      end do

      !> Compute the axis direction
      if ( U(3,3) /= (0//1) ) then
         axis(3) = (0//1)
      else if ( U(2,3) /= (0//1) .and. U(2,2) == (0//1) ) then
         axis(3) = (0//1)
      else
         axis(3) = (1//1)  ! Free choice
      end if

      if ( U(2,2) /= (0//1) ) then
         axis(2) = - U(2,3) * axis(3) / U(2,2)
         if (U(1,1) == (0//1)) then
            axis(1) = (1//1) ! Free choice
         else
            axis(1) = - ( U(1,2) * axis(2) + U(1,3) * axis(3) ) / U(1,1)
         end if
      else ! axis(3) must be zero because row(2) cannot be zero
         if ( U(1,2) == (0//1) ) then
            axis(2) = (1//1) ! Free choice
            axis(1) = - ( U(1,2) * axis(2) + U(1,3) * axis(3) ) / U(1,1)
         else
            axis(1) = (1//1) ! Free choice
            axis(2) = - ( U(1,1) * axis(1) + U(1,3) * axis(3) ) / U(1,2)
         end if
      end if

      call Smallest_Integral_Vector(axis)

      !> Choose the eigenvector axis with axis(3) > 0. If axis(3) = 0, choose axis with
      !> axis(2) > 0. If axis(2) = 0, choose axis with axis(1) > 0
      do i = 3, 1, -1
         if (axis(i) /= (0//1)) then
            if (axis(i) < (0//1)) axis = -axis
            exit
         end if
      end do
   End Function Get_Rotation_Axis

   !!----
   !!---- GET_ROTATIONS
   !!----
   !!---- It returns the number of symmetry operations in array symOp
   !!---- with rotational part of order n. The corresponding index
   !!---- and value of the determinant are returned in idd.
   !!----
   !!---- 22/04/2019
   !!
   Module Subroutine Get_Rotations(nSymOP, symOP, n, nso, idd)
      !---- Arguments ----!
      integer,                                 intent(in)  :: nSymOP
      type(Symm_Oper_Type), dimension(nSymOP), intent(in)  :: symOP
      integer,                                 intent(in)  :: n
      integer,                                 intent(out) :: nso
      integer, dimension(nSymOP,2),            intent(out) :: idd

      !---- Local variables ----!
      integer                           :: i,d,t
      type(rational)                    :: det,tr
      integer,           dimension(6)   :: traces
      character(len=20), dimension(6)   :: axisName

      !> Init
      call Clear_Error()

      axisName(1) = "onefold"
      axisName(2) = "twofold"
      axisName(3) = "threefold"
      axisName(4) = "fourfold"
      axisName(6) = "sixfold"

      traces(1) =  3
      traces(2) = -1
      traces(3) =  0
      traces(4) =  1
      traces(6) =  2

      nso = 0
      idd=0
      do i = 1 , nSymOP
         det = rational_determ(symOp(i)%Mat(1:3,1:3))
         if (mod(det%Numerator,det%Denominator) /= 0) then
            Err_CFML%Ierr = 1
            Err_CFML%Msg = "Get_Rotations@SPACEG: Determinant is not an integer."
            return
         end if
         d = det%Numerator / det%Denominator
         tr  = rational_trace(symOp(i)%Mat(1:3,1:3))
         if (mod(tr%Numerator,tr%Denominator) /= 0) then
            Err_CFML%Ierr = 1
            Err_CFML%Msg = "Get_Rotations@SPACEG: Trace is not an integer."
            return
         end if
         t = tr%Numerator / tr%Denominator
         if ( d == 1 .and. t == traces(n) .or. d == -1 .and. t == -traces(n)) then
            nso = nso + 1
            idd(nso,1) = i
            idd(nso,2) = d
         end if
      end do
   End Subroutine Get_Rotations

   !!----
   !!---- GET_ROTATION_ORDER
   !!----    Determine the orden of rotation (valid for all bases).
   !!----    Return a zero if any error occurs.
   !!----
   !!---- 13/05/2019
   !!
   Module Function Get_Rotation_Order(W) Result(N)
      !---- Arguments ----!
      type(rational), dimension(3,3), intent(in)  :: W
      integer                                     :: N

      !---- Local Variables ----!
      integer :: det, itr

      !> Init
      n=0

      call clear_error()
      det=rational_determ(W)
      if (err_cfml%ierr /=0) return
      itr=rational_trace(W)

      select case (itr)
         case (-3)
            if (det == -1) n=-1  !Inversion centre

         case (-2)
            if (det == -1) n=-6  ! Six-fold axis + inversion

         case (-1)
            if (det == -1) n=-4  ! Four-fold axis + inversion
            if (det ==  1) n= 2  ! Binary axis

         case ( 0)
            if (det == -1) n=-3  ! Three-fold axis + inversion
            if (det ==  1) n= 3  ! Three-fold axis

         case ( 1)
            if (det == -1) n=-2  ! Mirror plane
            if (det ==  1) n= 4  ! Four-fold axis

         case ( 2)
            if (det ==  1) n= 6  ! Six-fold axis

         case ( 3)
            if (det ==  1) n= 1  ! Identity
      end select
   End Function Get_Rotation_Order

End Submodule gS_Get_Rotations