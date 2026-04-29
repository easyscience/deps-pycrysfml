!!----
!!----
!!----
!!----
!!
SubModule (CFML_Rational) RAT_generic
 implicit none
 Contains
   !!----
   !!---- RATIONAL_CO_LINEAR
   !!----
   !!---- 08/04/2019
   !!
   Pure Module Function Rational_Co_linear(R,S,N) Result(OK)
      !---- Argument ----!
      type(rational), dimension(:), intent(in) :: R
      type(rational), dimension(:), intent(in) :: S
      integer, optional,            intent(in) :: n
      logical                                  :: OK

      !---- Local variables ----!
      integer        :: i,ia,ib,n1,n2
      type(rational) :: c

      !> Init
      OK=.true.
      if (present(n)) then
         n1=n; n2=n
      else
         n1=size(R)
         n2=size(S)
      end if
      if (n1 /= n2) return

      do i=1,n1
         if (abs(R(i)%numerator) > 0) then
            ia=i
            exit
         end if
      end do
      do i=1,n1
         if (abs(S(i)%numerator) > 0) then
            ib=i
            exit
         end if
      end do
      if (ia /= ib) then
         OK=.false.
         return
      end if

      c=R(ia) / S(ib)
      do i=1, n1
         if ((R(i) - c * S(i) /= (0//1))) then
            OK=.false.
            return
         end if
      end do

   End Function Rational_Co_linear

   !!----
   !!---- RATIONAL_IS_NULLVECTOR
   !!----
   !!---- 08/04/2019
   !!
   Pure Module Function Rational_Is_NullVector(V) Result(nulo)
      !---- Arguments ----!
      type(rational), dimension(:), intent(in) :: v
      logical                                  :: nulo

      !---- Local variables ----!
      integer :: i, n

      !> Init
      nulo = .true.
      n=size(V)

      do i=1, n
         if (v(i) /= (0//1)) then
            nulo = .false.
            return
         end if
      end do

   End Function Rational_Is_NullVector

   !!----
   !!---- RATIONAL_IS_DIAGONALMATRIX
   !!----
   !!---- 08/04/2019
   !!
   Pure Module Function Rational_Is_DiagonalMatrix(M) Result(Diagonal)
      !---- Arguments ----!
      type(rational), dimension(:,:), intent(in) :: M
      logical                                    :: Diagonal

      !---- Local variables ----!
      integer :: i,j,n1,n2

      !> Init
      diagonal=.true.
      n1=size(M,1)
      n2=size(M,2)
      do i=1, n1
         do j=1, n2
            if (i /= j .and. M(i,j) /= (0//1)) then
               diagonal = .false.
               return
            end if
         end do
      end do

   End Function Rational_Is_DiagonalMatrix

   !!----
   !!---- RATIONAL_MODULO_LAT
   !!----
   !!---- 08/04/2019
   !!
   Elemental Module Function Rational_Modulo_Lat(R) Result(S)
      !---- Arguments ----!
      type(rational), intent(in) :: r
      type(rational)             :: s

      !> Init
      S=R

      do
         if (s < 0_LI // 1_LI) then
            s=s+1_LI
         else
            exit
         end if
      end do

      do
         if (s >= 1_LI // 1_LI) then
            s=s-1_LI
         else
            exit
         end if
      end do

   End Function Rational_Modulo_Lat

   !!----
   !!---- RATIONAL_RANK
   !!----
   !!---- 08/04/2019
   !!
   Pure Module Function Rational_Rank(M) Result(k)
      !---- Arguments ----!
      type(rational), dimension(:,:), intent(in)  :: M
      integer                                     :: k

      !---- Local variables ----!
      integer :: i,n1,n2,nNull
      type(rational), dimension(:),   allocatable :: nullVector
      type(rational), dimension(:,:), allocatable :: U

      !> Init
      n1=size(M,1); n2=size(M,2)

      allocate(U(n1,n2))
      allocate(nullVector(n2))

      nNull = 0
      U=M
      nullVector = 0 // 1

      call Rational_RowEchelonForm(U)
      do i=1, n1
         if (rational_equal(U(i,:),nullVector)) nNull = nNull + 1
      end do

      k=n1 - nNull
   End Function Rational_Rank

   !!----
   !!---- RATIONAL_TRACE
   !!----
   !!---- 08/04/2019
   !!
   Module Function Rational_Trace(M) Result(R)
      !---- Arguments ----!
      type(rational), dimension(:,:), intent(in) :: M
      type(rational)                             :: R

      !---- Local variables ----!
      integer :: n1,n2,i

      !> Init
      r=0
      n1=size(M,dim=1); n2=size(M,dim=2)
      if (n1 /= n2) then
         Err_CFML%Ierr=1
         Err_CFML%Msg="Error in Rational_Trace: matrix is not a square matrix!"
         return
      end if

      do i=1, n1
         r=r + M(i,i)
      end do

   End Function Rational_Trace

   !!----
   !!---- RATIONAL_DETERM
   !!----
   !!---- This function for calculating the determinant is not very efficient but
   !!---- largely enough for our needs. Will be probably replaced by another one
   !!---- using LU decomposition.
   !!----
   !!---- 08/04/2019
   !!
   Pure Recursive Module Function Rational_Determ(A) Result(Det)
      !---- Arguments ----!
      type(rational), dimension(:,:), intent(in) :: a
      type(rational)                             :: det

      !---- Local variables ----!
      type(rational), dimension(size(a,1)-1, size(a,1)-1) :: b
      type(rational)                                      :: sgn
      integer                                             :: i, n

      !> Init
      n=size(a,1)
      if (n == 1) then
         det = a(1,1)
      else
         det = 0_LI//1_LI
         sgn = 1_LI/1_LI
         do i=1,n
            b(:, :(i-1)) = a(2:, :i-1)
            b(:, i:) = a(2:, i+1:)
            det = det + sgn * a(1, i) * rational_determ(b)
            sgn = sgn * (-1_LI/1_LI)
         end do
      end if

   End Function Rational_Determ

   !!----
   !!---- RATIONAL_INVERSE_MATRX
   !!----
   !!----  This calculates the inverse of a matrix converting it previously to
   !!----  a double precision matrix. The final invMat is an approximation according
   !!----  to the value of the 'maximum_denominator' value
   !!----
   !!---- 08/04/2019
   !!
   Module Function Rational_Inverse_Matrix(M) Result(B)
      !---- Local Variables ----!
      type(rational), dimension(:,:),    intent(in)  :: M
      type(rational), dimension(size(M,1),size(M,2)) :: B

      !---- Local variables ----!
      real(kind=cp), dimension(size(M,1),size(M,2)) :: A, invA
      integer :: n1,n2

      !> Init
      B=0_LI//1_LI
      n1=size(M,1); n2=size(M,2)
      if (n1 /= n2) then
         Err_CFML%Ierr=1
         Err_CFML%msg="Error: the provided matrix is not square!"
         return
      end if

      A=M
      invA=Inverse_Matrix(A)
      if (Err_CFML%ierr /=0) return

      B=invA ! This gives an approximation according to the 'maximum_denominator' value

   End Function Rational_Inverse_Matrix

End SubModule RAT_generic
