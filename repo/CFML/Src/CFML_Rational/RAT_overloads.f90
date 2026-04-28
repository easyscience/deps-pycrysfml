!!----
!!----
!!----
!!
Submodule (CFML_Rational) RAT_Overloads
 implicit none
 Contains
    !!----
    !!---- RATIONAL_ABS
    !!----
    !!---- 08/04/2019
    !!
    Elemental Module Function Rational_Abs(R) Result(Res)
       !---- Arguments ----!
       type(rational), intent (in) :: r
       type(rational)              :: res

       res = sign (r%numerator, r%denominator) // r%denominator

    End Function Rational_Abs

    !!----
    !!---- RATIONAL_INT
    !!----
    !!---- 08/04/2019
    !!
    Elemental Module Function Rational_Int(R) Result(Res)
       !---- Arguments ----!
       type(rational), intent (in) :: r
       integer(kind=LI)            :: res

       res = r%numerator / r%denominator

    End Function Rational_Int

    !!----
    !!---- RATIONAL_NINT
    !!----
    !!---- 08/04/2019
    !!
    Elemental Module Function Rational_Nint(R) Result(Res)
       !---- Arguments ----!
       type(rational),  intent (in)  :: r
       integer(kind=LI)              :: res

       res = nint(real(r%numerator,kind=cp)/real(r%denominator,kind=cp),kind=LI)

    End Function Rational_Nint

    !!----
    !!---- RATIONAL_MODULO
    !!----
    !!---- 08/04/2019
    !!
    Elemental Module Function Rational_Modulo(R) Result(Res)
       !---- Arguments ----!
       type(rational), intent (in) :: r
       integer                     :: res

       res = modulo (r%numerator, r%denominator)

    End Function Rational_Modulo

    !!----
    !!---- RATIONAL_INTEGER_MODULO
    !!----
    !!---- 08/04/2019
    !!
    Elemental Module Function Rational_Integer_Modulo(R,I) Result(Res)
       !---- Arguments ----!
       type(rational),  intent (in) :: r
       integer(kind=LI),intent(in)  :: i
       type(rational)               :: res

       !---- Local Variables ----!
       real(kind=cp) :: val

       val = modulo (real(r%numerator,kind=cp) / real(r%denominator,kind=cp),real(i,kind=cp))
       res = val

    End Function Rational_Integer_Modulo

    !!----
    !!---- RATIONAL_MOD
    !!----
    !!---- 08/04/2019
    !!
    Elemental Module Function Rational_Mod(R) Result(Res)
       !---- Arguments ----!
       type(rational), intent (in) :: r
       integer                     :: res

       res = mod(r%numerator, r%denominator)

    End Function Rational_Mod

    !!----
    !!---- FUNCTION RATIONAL_INTEGER_MOD
    !!----
    !!----
    !!
    Elemental Module Function Rational_Integer_Mod(R,I) Result(Res)
       !---- Arguments ----!
      type(rational),   intent (in) :: r
      integer(kind=LI), intent (in) :: i
      type(rational)                :: res

      !---- Local Variables ----!
      real(kind=cp) :: val

      val = mod(real(r%numerator,kind=cp) / real(r%denominator,kind=cp),real(i,kind=cp))
      res = val

    End Function Rational_Integer_Mod

    !!----
    !!---- RATIONAL_DOT_PRODUCT
    !!----
    !!---- 08/04/2019
    !!
    Pure Module Function Rational_Dot_Product(R1,R2) Result(Res)
       !---- Arguments ----!
       type(rational), dimension(:), intent (in) :: r1
       type(rational), dimension(:), intent (in) :: r2
       type(rational)                            :: res

       !---- Local Variables ----!
       integer :: n1,n2,nm,i

       n1=size(r1); n2=size(r2)
       res=0_LI//1_LI
       nm=min(n1,n2)
       do i=1,nm
          res=rational_simplify(res + r1(i)*r2(i))
       end do

    End Function Rational_Dot_Product

    !!----
    !!---- RATIONAL_MAXVAL_VECTOR
    !!----
    !!---- 08/04/2019
    !!
    Pure Module Function Rational_Maxval_Vector(R) Result(Res)
       !---- Arguments ----!
       type(rational), dimension(:), intent (in) :: r
       type(rational)                            :: res

       !---- Local Variables ----!
       integer :: i,n

       n=size(r)
       res=-huge(1_LI)//1_LI
       do i=1,n
          if (r(i) > res) res=r(i)
       end do

    End Function Rational_Maxval_Vector

    !!----
    !!---- RATIONAL_MAXVAL_MATRIX
    !!----
    !!---- 08/04/2019
    !!
    Pure Module Function Rational_Maxval_Matrix(R) Result(Res)
       !---- Arguments ----!
       type(rational), dimension(:,:), intent(in) :: r
       type(rational)                              :: Res

       !---- Local Variables ----!
       integer :: i,j,n1,n2

       n1=size(r,dim=1);  n2=size(r,dim=2)
       res=-huge(1_LI)//1_LI
       do j=1,n2
          do i=1,n1
             if (r(i,j) > res) res=r(i,j)
          end do
       end do

    End Function Rational_Maxval_Matrix

    !!----
    !!---- RATIONAL_MINVAL_VECTOR
    !!----
    !!---- 08/04/2019
    !!
    Pure Module Function Rational_Minval_Vector(R) Result(Res)
       !---- Arguments ----!
       type(rational), dimension(:), intent (in) :: r
       type(rational)                            :: res

       !---- Local Variables ----!
       integer :: i,n

       n=size(r)
       res=huge(1_LI)//1_LI
       do i=1,n
          if (r(i) < res) res=r(i)
       end do

    End Function Rational_Minval_Vector

    !!----
    !!---- RATIONAL_MINVAL_MATRIX
    !!----
    !!---- 08/04/2019
    !!
    Pure Module Function Rational_Minval_Matrix(R) Result(Res)
       !---- Arguments ----!
       type(rational), dimension(:,:), intent (in) :: r
       type(rational)                              :: res

       !---- Local Variables ----!
       integer :: i,j,n1,n2

       n1=size(r,dim=1);  n2=size(r,dim=2)
       res=huge(1_LI)//1_LI
       do j=1,n2
          do i=1,n1
             if (r(i,j) < res) res=r(i,j)
          end do
       end do

    End Function Rational_Minval_Matrix

    !!----
    !!---- RATIONAL_MATMUL_MATVEC
    !!----
    !!---- 08/04/2019
    !!
    Pure Module Function Rational_Matmul_Matvec(Mat,Vec) Result(Vec_Out)
       !---- Arguments ----!
       type(rational), dimension(:,:), intent (in) :: mat
       type(rational), dimension(:),   intent (in) :: vec
       type(rational), dimension(size(vec))        :: vec_out

       !---- Local Variables ----!
       integer :: n1,n2,n3,nm,i

       n1=size(mat,dim=1); n2=size(mat,dim=2); n3=size(vec)
       nm=min(n2,n3)
       vec_out=0_LI//1_LI
       do i=1,nm
          vec_out(i) = rational_simplify(dot_product(mat(i,1:nm),vec(1:nm)))
       end do

    End Function Rational_Matmul_Matvec

    !!----
    !!---- RATIONAL_MATMUL_MATMAT
    !!----
    !!---- 08/04/2019
    !!
    Pure Module Function Rational_Matmul_Matmat(Mat1,Mat2) Result(Mat_Out)
       !---- Arguments ----!
       type(rational), dimension(:,:), intent (in)                 :: mat1
       type(rational), dimension(:,:), intent (in)                 :: mat2
       type(rational),dimension(size(mat1,dim=1),size(mat2,dim=2)) :: mat_out

       !---- Local Variables ----!
       integer :: n1,n2,n3,n4,nm,i,j

       n1=size(mat1,dim=1); n2=size(mat1,dim=2); n3=size(mat2,dim=1); n4=size(mat2,dim=2)
       forall ( i=1:n1, j=1:n4 ) mat_out(i,j) = 0_LI/1_LI

       if (n2 == n3) then
          do j=1,n4
             do i=1,n1
                mat_out(i,j) = rational_simplify (dot_product(mat1(i,:),mat2(:,j)))
             end do
          end do
       else
          nm=min(n2,n3)
          do j=1,n4
             do i=1,n1
                mat_out(i,j) = rational_simplify (dot_product(mat1(i,1:nm),mat2(1:nm,j)))
             end do
          end do
       end if

    End Function Rational_Matmul_Matmat

    !!----
    !!---- RATIONAL_MAXLOC_MATRIX
    !!----
    !!----
    !!
    Pure Module Function Rational_Maxloc_Matrix(Mat) Result(Pos_Max)
       !---- Arguments ----!
       type(rational),  dimension(:,:), intent(in) :: Mat
       integer, dimension(2)                       :: pos_max

       !---- Local variables ----!
       integer        :: nu1,nl1,nu2,nl2,i,j
       type(rational) :: res

       nu1=ubound(Mat,dim=1); nu2=ubound(Mat,dim=2)
       nl1=lbound(Mat,dim=1); nl2=lbound(Mat,dim=2)

       res=-huge(1_LI)//1_LI
       do j=nl2,nu2
          do i=nl1,nu1
             if (mat(i,j) > res) then
                res=mat(i,j)
                pos_max=[i,j]
             end if
          end do
       end do

    End Function Rational_Maxloc_Matrix

    !!----
    !!---- RATIONAL_MAXLOC_VECTOR
    !!----
    !!---- 08/04/2019
    !!
    Pure Module Function Rational_Maxloc_Vector(Vec) Result(Pos_Max)
       !---- Arguments ----!
       type(rational), dimension(:), intent(in) :: vec
       integer                                  :: pos_max

       !---- Local variables ----!
       integer        :: nu,nl,i
       type(rational) :: res

       nu=ubound(vec,dim=1)
       nl=lbound(vec,dim=1)
       res=-huge(1_LI)//1_LI
       do i=nl,nu
          if (vec(i) > res) then
             res=vec(i)
             pos_max=i
          end if
       end do

    End Function Rational_Maxloc_Vector

    !!----
    !!---- FUNCTION RATIONAL_MINLOC_MATRIX
    !!----
    !!---- 08/04/2019
    !!
    Pure Module Function Rational_Minloc_Matrix(Mat) Result(Pos_Min)
       !---- Arguments ----!
       type(rational),  dimension(:,:), intent(in) :: Mat
       integer, dimension(2)                       :: pos_min

       !---- Local variables ----!
       integer:: nu1,nl1,nu2,nl2,i,j
       type(rational) :: res

       nu1=ubound(Mat,dim=1); nu2=ubound(Mat,dim=2)
       nl1=lbound(Mat,dim=1); nl2=lbound(Mat,dim=2)

       res=huge(1_LI)//1_LI
       do j=nl2,nu2
          do i=nl1,nu1
             if (mat(i,j) < res) then
                res=mat(i,j)
                pos_min=[i,j]
             end if
          end do
       end do

    End Function Rational_Minloc_Matrix


    !!----
    !!---- RATIONAL_MINLOC_VECTOR
    !!----
    !!---- 08/04/2019
    !!
    Pure Module Function Rational_Minloc_Vector(Vec) Result(Pos_Min)
       !---- Arguments ----!
       type(rational), dimension(:), intent(in) :: vec
       integer                                  :: pos_min

       !---- Local variables ----!
       integer        :: nu,nl,i
       type(rational) :: res

       nu=ubound(vec,dim=1)
       nl=lbound(vec,dim=1)
       res=huge(1_LI)//1_LI
       do i=nl,nu
          if (vec(i) < res) then
             res=vec(i)
             pos_min=i
          end if
       end do

    End Function Rational_Minloc_Vector

    !!----
    !!---- RATIONAL_REAL
    !!----
    !!---- 08/04/2019
    !!
    Elemental Module Function Rational_Real(R) Result (Res)
       !---- Arguments ----!
       type (rational),  intent(in) :: r
       real(kind=cp)                :: res

       res=real(r%numerator,kind=cp) / real(r%denominator,kind=cp)

    End Function Rational_Real

    !!----
    !!---- FUNCTION RATIONAL_SUM_VECTOR
    !!----
    !!---- 08/04/2019
    !!
    Pure Module Function Rational_Sum_Vector(Vec) Result(Suma)
       !---- Arguments ----!
       type(rational), dimension(:), intent(in) :: vec
       type(rational)                           :: suma

       !---- Local variables ----!
       integer :: i,n

       n=size(vec)
       suma=0_LI/1_LI
       do i=1,n
          suma=suma+vec(i)
       end do

    End Function Rational_Sum_Vector

End Submodule RAT_Overloads