!!-------------------------------------------------------
!!---- Crystallographic Fortran Modules Library (CrysFML)
!!-------------------------------------------------------
!!---- The CrysFML project is distributed under LGPL. In agreement with the
!!---- Intergovernmental Convention of the ILL, this software cannot be used
!!---- in military applications.
!!----
!!---- Copyright (C) 1999-2022  Institut Laue-Langevin (ILL), Grenoble, FRANCE
!!----                          Universidad de La Laguna (ULL), Tenerife, SPAIN
!!----                          Laboratoire Leon Brillouin(LLB), Saclay, FRANCE
!!----
!!---- Authors: Juan Rodriguez-Carvajal (ILL)
!!----          Javier Gonzalez-Platas  (ULL)
!!----          Nebil Ayape Katcho      (ILL)
!!----
!!---- Contributors: Laurent Chapon     (ILL)
!!----               Marc Janoschek     (Los Alamos National Laboratory, USA)
!!----               Oksana Zaharko     (Paul Scherrer Institute, Switzerland)
!!----               Tierry Roisnel     (CDIFX,Rennes France)
!!----               Eric Pellegrini    (ILL)
!!----               Ross Angel         (University of Pavia)
!!----
!!---- This library is free software; you can redistribute it and/or
!!---- modify it under the terms of the GNU Lesser General Public
!!---- License as published by the Free Software Foundation; either
!!---- version 3.0 of the License, or (at your option) any later version.
!!----
!!---- This library is distributed in the hope that it will be useful,
!!---- but WITHOUT ANY WARRANTY; without even the implied warranty of
!!---- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!!---- Lesser General Public License for more details.
!!----
!!---- You should have received a copy of the GNU Lesser General Public
!!---- License along with this library; if not, see <http://www.gnu.org/licenses/>.
!!----
!!---- MODULE: CFML_Rational_Arithmetic
!!----   INFO: Extension of the module: "module_rational" from
!!----         https://rosettacode.org/wiki/Arithmetic/Rational#Fortran
!!----         It is intented for making algebra with not too big numerators
!!----         and denominators. The implementation of error control against
!!----         oveflows is not yet done. To avoid partially this the module
!!----         uses integer of 8 bytes (integer kind li=8). Subroutines
!!----         for inverting matrices are provided: a special subroutine working
!!----         strictly with rational numbers 'Matinv_rational' and the subroutine
!!----         'rational_inv_matrix' that uses LU decomposition through double
!!----         precision and reconvesion to an approximate rational matrix.
!!----         The control of the maximum denominator is done through a global
!!----         variable called 'maximum_denominator'.
!!----         For small rational number everything seems to work, specially for
!!----         matrices representing symmetry operators in superspace and
!!----         transformation of them by not too complex change of basis.
!!----
!!---- HISTORY
!!----    Created: 01/02/2017
!!----    Last updated: 03/11/2018 (JRC)
!!----
!!---- DEPENDENCIES
!!----
!!----    CFML_GlobalDeps, CFML_String_Utilities, CFML_Math_general
!!----
!!----
!!
Module CFML_Rational

    Use CFML_GlobalDeps, only : CP, LI, Err_CFML,Clear_Error, CFML_debug
    Use CFML_Maths,      only : Inverse_Matrix
    Use CFML_Strings,    only : Pack_String

    implicit none
    private

    !> Public operators and assignment
    public :: assignment (=)     ! Elemental Assignement rational<=>rational,real<=>rational, integer<=>rational of different types (sp,dp,int,int_ik)
    public :: operator (//)      ! Elemental Definition of rationals using numerator and denominator integers, e.g. 23_ik//34_ik
    public :: operator (+)       ! Elemental Addition of rationals and rational plus integers
    public :: operator (-)       ! Elemental subtraction of rationals and rational plus integers
    public :: operator (*)       ! Elemental Multiplication of rationals or rationals by integers and viceversa
    public :: operator (/)       ! Elemental division of rationals or rationals by integers and viceversa
    public :: operator (<)       ! Pure 'less than' operator comparing rational<=>rational rational<=>integer
    public :: operator (<=)      ! Pure 'less or equal than' operator comparing rational<=>rational rational<=>integer
    public :: operator (>)       ! Pure 'greater than' operator comparing rational<=>rational rational<=>integer
    public :: operator (>=)      ! Pure 'greater or equal than' operator comparing rational<=>rational rational<=>integer
    public :: operator (==)      ! Pure 'logical equal to' operator comparing rational<=>rational rational<=>integer
    public :: operator (/=)      ! Pure 'logical no-equal to' operator comparing rational<=>rational rational<=>integer

    !> Public functions
    public :: Rational_Simplify,         &   !Simplification of fractions, normally called in the majority of operations
              Rational_Determ,           &   !Calculation Procedures using a recursive subroutine (valid only for small rationals)
              Rational_String,           &   !Transform a rational type to a string like xxx/yyy
              Rational_Is_Integer,       &   !Logical function applied to rationals: scalar, vector and matrix
              Rational_Equal,            &   !Logical function telling if two matrices/vector are equal
              Rational_Recip,            &   !Calculates the reciprocal of a rational  a/b -> b/a
              Rational_Co_Linear,        &   !Logical function telling if two vectors are colinear
              Rational_Trace,            &   !Returns the trace of a square matrix
              Rational_Inverse_Matrix,   &   !Calculates the inverse of a rational matrix
              Rational_Modulo_Lat,       &   !Reduces a translation vector to that with values in the interval [0_ik, 1_ik)
              Rational_Rank,             &   !Computes the rank of a rational matrix
              Rational_Is_DiagonalMatrix,&   !Logical function telling if the matrix is diagonal
              Rational_Is_NullVector         !Logical function telling if the vector is the null vector

    !> Public subroutines
    public :: Rational_Identity_Matrix,  &   !Returns an identity matrix
              Rational_RowEchelonForm,   &   !Put a matrix in a rowechelonform
              Rational_SmithNormalForm

    !> Public overloaded intrinsic functions (transpose is not needed)
    public :: abs, int, nint, modulo, mod, dot_product, maxval, minval, &
              maxloc,minloc, matmul, sum, real


    integer(kind=8),   public, parameter :: MAXIMUM_DENOMINATOR=999_LI

    !> Types definitions
    Type, public :: rational
       integer(kind=8) :: numerator
       integer(kind=8) :: denominator
    End type rational

    !> Assignment
    interface assignment (=)
      module procedure assign_rational_int
      module procedure assign_rational_int_LI
      module procedure assign_int_rational
      module procedure assign_int_LI_rational
      module procedure assign_rational_real_CP
      module procedure assign_real_rational_CP
    end interface

    !> Constructor of a rational from two integers
    interface operator (//)
      module procedure make_rational       !input integers of 8bytes   4_li//5_li
      module procedure make_rational_int   !input integers of 4bytes   3//4
    end interface

    interface operator (+)
      module procedure rational_add
      module procedure rational_integer_add
      module procedure integer_rational_add
    end interface

    interface operator (-)
      module procedure rational_minus
      module procedure rational_subtract
      module procedure integer_rational_subtract
      module procedure rational_integer_subtract
    end interface

    interface operator (*)
      module procedure rational_multiply
      module procedure integer_rational_multiply
      module procedure rational_integer_multiply
    end interface

    interface operator (/)
      module procedure rational_divide
      module procedure rational_integer_divide
      module procedure integer_rational_divide
    end interface

    interface operator (<)
      module procedure rational_lt
      module procedure rational_integer_lt
      module procedure integer_rational_lt
    end interface

    interface operator (<=)
      module procedure rational_le
      module procedure rational_integer_le
      module procedure integer_rational_le
    end interface

    interface operator (>)
      module procedure rational_gt
      module procedure rational_integer_gt
      module procedure integer_rational_gt
    end interface

    interface operator (>=)
      module procedure rational_ge
      module procedure rational_integer_ge
      module procedure integer_rational_ge
    end interface

    interface operator (==)
      module procedure rational_eq
      module procedure rational_integer_eq
      module procedure integer_rational_eq
    end interface

    interface operator (/=)
      module procedure rational_ne
      module procedure rational_integer_ne
      module procedure integer_rational_ne
    end interface

    !> Intrinsics Overloads
    interface abs
      module procedure rational_abs
    end interface

    interface int
      module procedure rational_int
    end interface

    interface nint
      module procedure rational_nint
    end interface

    interface modulo
      module procedure rational_modulo
      module procedure rational_integer_modulo
    end interface

    interface mod
      module procedure rational_mod
      module procedure rational_integer_mod
    end interface

    interface dot_product
      module procedure rational_dot_product
    end interface

    interface maxval
      module procedure rational_maxval_vector
      module procedure rational_maxval_matrix
    end interface

    interface minval
      module procedure rational_minval_vector
      module procedure rational_minval_matrix
    end interface

    interface matmul
      module procedure rational_matmul_matvec
      module procedure rational_matmul_matmat
    end interface

    interface maxloc
      module procedure rational_maxloc_vector
      module procedure rational_maxloc_matrix
    end interface

    interface minloc
      module procedure rational_minloc_vector
      module procedure rational_minloc_matrix
    end interface

    interface real
      module procedure rational_real
    end interface

    interface sum
      module procedure rational_sum_vector
    end interface

    !> General overloads
    interface Rational_Equal
      module procedure Rational_Equal_Matrix
      module procedure Rational_Equal_Vector
    end interface

    interface Rational_Is_Integer
      module procedure Is_Integer_rational_scalar
      module procedure Is_Integer_rational_vector
      module procedure Is_Integer_rational_matrix
    end interface

    interface Rational_RowEchelonForm
      module procedure Rational_RowEchelonForm_M
      module procedure Rational_RowEchelonForm_MT
    end interface

    Interface
       !> Constructor //
       Elemental Module Function Make_Rational(Numerator, Denominator) Result(Res)
          !---- Arguments ----!
          integer(kind=8), intent (in) :: numerator
          integer(kind=8), intent (in) :: denominator
          type(rational)                :: res
       End Function Make_Rational

       Elemental Module Function Make_Rational_Int(Numerator, Denominator) Result(Res)
          !---- Arguments ----!
          integer, intent (in) :: numerator
          integer, intent (in) :: denominator
          type(rational)       :: res
       End Function Make_Rational_Int

       !> Assignment
       Elemental Module Subroutine Assign_Int_LI_Rational(I, Res)
          !---- Arguments ----!
          type(rational),  intent (in)   :: res  !, volatile
          integer(kind=8),intent (out)  :: i
       End Subroutine Assign_Int_LI_Rational

       Elemental Module Subroutine Assign_Int_Rational(I, Res)
          !---- Arguments ----!
          type(rational), intent (in)   :: res  !, volatile
          integer,        intent (out)  :: i
       End Subroutine Assign_Int_Rational

       Elemental Module Subroutine Assign_Rational_Int(Res, I)
          !---- Arguments ----!
          type(rational),  intent (out) :: res  ! volatile
          integer,         intent (in)  :: i
       End Subroutine Assign_Rational_Int

       Elemental Module Subroutine Assign_Rational_Int_LI(Res, I)
          !---- Arguments ----!
          type(rational),   intent (out) :: res  ! volatile
          integer(kind=8), intent (in)  :: i
       End Subroutine Assign_Rational_Int_LI

       Elemental Module Subroutine Assign_Rational_Real_CP(Res, Xr)
          !---- Arguments ----!
          type(rational), intent(out) :: res  ! volatile
          real(kind=cp),  intent (in) :: xr
       End Subroutine Assign_Rational_Real_CP

       Elemental Module Subroutine Assign_Real_Rational_CP(X, Res)
          !---- Arguments ----!
          type(rational), intent(in)   :: res
          real(kind=cp),  intent (out) :: x
       End Subroutine Assign_Real_Rational_CP

       !> Operator +
       Elemental Module Function Integer_Rational_Add(I, S) Result(Res)
          !---- Arguments ----!
          integer(kind=8),intent (in) :: i
          type(rational),  intent (in) :: s
          type(rational)               :: res
       End Function Integer_Rational_Add

       Elemental Module Function Rational_Add(R, S) Result(Res)
          !---- Arguments ----!
          type(rational), intent (in) :: r
          type(rational), intent (in) :: s
          type(rational)              :: res
       End Function Rational_Add

       Elemental Module Function Rational_Integer_Add(S, I) Result(Res)
          !---- Arguments ----!
          type(rational),  intent (in) :: s
          integer(kind=8),intent (in) :: i
          type(rational)               :: res
       End Function Rational_Integer_Add

       !> Operator -
       Elemental Module Function Integer_Rational_Subtract(I, S) Result(Res)
          !---- Arguments ----!
          integer(kind=8),intent (in) :: i
          type(rational),  intent (in) :: s
          type(rational)               :: res
       End Function Integer_Rational_Subtract

       Elemental Module Function Rational_Integer_Subtract(S,I) Result(Res)
          !---- Arguments ----!
          type(rational),  intent (in) :: s
          integer(kind=8),intent (in) :: i
          type(rational)               :: res
       End Function Rational_Integer_Subtract

       Elemental Module Function Rational_Minus(R) Result(Res)
          !---- Arguments ----!
          type(rational), intent (in) :: r
          type(rational)              :: res
       End Function Rational_Minus

       Elemental Module Function Rational_Subtract(R, S) Result(Res)
          !---- Arguments ----!
          type(rational), intent (in) :: r
          type(rational), intent (in) :: s
          type(rational)              :: res
       End Function Rational_Subtract

       !> Operator *
       Elemental Module Function Integer_Rational_Multiply(I, S) Result(Res)
          !---- Arguments ----!
          integer(kind=8),intent (in) :: i
          type(rational),  intent (in) :: s
          type(rational)               :: res
       End Function Integer_Rational_Multiply

       Elemental Module Function Rational_Integer_Multiply(S,I) Result(Res)
          !---- Arguments ----!
          integer(kind=8),intent (in) :: i
          type(rational),  intent (in) :: s
          type(rational)               :: res
       End Function Rational_Integer_Multiply

       Elemental Module Function Rational_Multiply(R, S) Result(Res)
          !---- Arguments ----!
          type(rational), intent (in) :: r
          type(rational), intent (in) :: s
          type(rational)              :: res
       End Function Rational_Multiply

       !> Operator divisor
       Elemental Module Function Integer_Rational_Divide(I, R) Result(Res)
          !---- Arguments ----!
          integer(kind=8), intent(in) :: I
          type (rational),  intent(in) :: r
          type (rational)              :: res
       End Function Integer_Rational_Divide

       Elemental Module Function Rational_Divide(R, S) Result(Res)
          !---- Arguments ----!
          type(rational), intent (in) :: r
          type(rational), intent (in) :: s
          type(rational)              :: res
       End Function Rational_Divide

       Elemental Module Function Rational_Integer_Divide(R, I) Result(Res)
          !---- Arguments ----!
          type(rational),  intent (in) :: r
          integer(kind=8),intent (in) :: i
          type(rational)               :: res
       End Function Rational_Integer_Divide

       !> Operator <
       Elemental Module Function Integer_Rational_LT(I,R) Result (Res)
          !---- Arguments ----!
          integer(kind=8),intent (in) :: i
          type(rational),  intent (in) :: r
          logical                      :: res
       End Function Integer_Rational_LT

       Elemental Module Function Rational_Integer_LT(R, I) Result(Res)
          !---- Arguments ----!
          type(rational),  intent (in) :: r
          integer(kind=8),intent (in) :: i
          logical                      :: res
       End Function Rational_Integer_LT

       Elemental Module Function Rational_LT(R, S) Result(Res)
          !---- Arguments ----!
          type(rational), intent (in) :: r
          type(rational), intent (in) :: s
          logical                     :: res
       End Function Rational_LT

       !> Operator <=
       Elemental Module Function Rational_Integer_LE(R, I) Result(Res)
          !---- Arguments ----!
          type(rational),  intent (in) :: r
          integer(kind=8),intent (in) :: i
          logical                      :: res
       End Function Rational_Integer_LE

       Elemental Module Function Rational_LE(R, S) Result(Res)
          !---- Argument ----!
          type(rational), intent (in) :: r
          type(rational), intent (in) :: s
          logical                     :: res
       End Function Rational_LE

       Pure Module Function Integer_Rational_LE(I, R) Result(Res)
          !---- Arguments ----!
          integer(kind=8),intent (in) :: i
          type(rational),  intent (in) :: r
          logical                      :: res
       End Function Integer_Rational_LE

       !> Operator >
       Elemental Module Function Integer_Rational_GT(I, R) Result(Res)
          !---- Arguments ----!
          integer(kind=8),intent (in) :: i
          type(rational),  intent (in) :: r
          logical                      :: res
       End Function Integer_Rational_GT

       Elemental Module Function Rational_GT(R, S) Result(Res)
          !---- Arguments ----!
          type(rational), intent (in) :: r
          type(rational), intent (in) :: s
          logical                     :: res
       End Function Rational_GT

       Elemental Module Function Rational_Integer_GT(R, I) Result(Res)
          !---- Arguments ----!
          type(rational),   intent (in) :: r
          integer(kind=8), intent (in) :: i
          logical                       :: res
       End Function Rational_Integer_GT

       !> Operator >=
       Elemental Module Function Integer_Rational_GE(I, R) Result(Res)
          !---- Arguments ----!
          integer(kind=8),intent (in) :: i
          type(rational),  intent (in) :: r
          logical                      :: res
       End Function Integer_Rational_GE

       Elemental Module Function Rational_GE(R, S) Result(Res)
          !---- Arguments ----!
          type(rational), intent (in) :: r
          type(rational), intent (in) :: s
          logical                     :: res
       End Function Rational_GE

       Elemental Module Function Rational_Integer_GE(R, I) Result(Res)
          !---- Arguments ----!
          type(rational),  intent (in) :: r
          integer(kind=8),intent (in) :: i
          logical                      :: res
       End Function Rational_Integer_GE

       !> Operator ==
       Elemental Module Function Integer_Rational_EQ(I, R) Result(Res)
          !---- Arguments ----!
          integer(kind=8),intent (in) :: i
          type(rational),  intent (in) :: r
          logical                      :: res
       End Function Integer_Rational_EQ

       Elemental Module Function Rational_EQ(R, S) Result(Res)
          !---- Arguments ----!
          type(rational), intent (in) :: r
          type(rational), intent (in) :: s
          logical                     :: res
       End Function Rational_EQ

       Elemental Module Function Rational_Integer_EQ(R, I) Result(Res)
          !---- Arguments ----!
          type(rational),  intent (in) :: r
          integer(kind=8),intent (in) :: i
          logical                      :: res
       End Function Rational_Integer_EQ

       !> Operator /=
       Elemental Module Function Integer_Rational_NE(I, R) Result(Res)
          !---- Arguments ----!
          integer(kind=8),intent (in) :: i
          type(rational),  intent (in) :: r
          logical                      :: res
       End Function Integer_Rational_NE

       Elemental Module Function Rational_Integer_NE(R, I) Result(Res)
          !---- Arguments ----!
          type(rational),  intent (in) :: r
          integer(kind=8),intent (in) :: i
          logical                      :: res
       End Function Rational_Integer_NE

       Elemental Module Function Rational_NE(R, S) Result(Res)
          !---- Arguments ----!
          type(rational), intent (in) :: r
          type(rational), intent (in) :: s
          logical                     :: res
       End Function Rational_NE

       !> Intrinsic Overloads
       Elemental Module Function Rational_Abs(R) Result(Res)
          !---- Arguments ----!
          type(rational), intent (in) :: r
          type(rational)              :: res
       End Function Rational_Abs

       Elemental Module Function Rational_Int(R) Result(Res)
          !---- Arguments ----!
          type(rational), intent (in) :: r
          integer(kind=8)            :: res
       End Function Rational_Int

       Elemental Module Function Rational_Integer_Mod(R,I) Result(Res)
          !---- Arguments ----!
          type(rational),   intent (in) :: r
          integer(kind=8), intent (in) :: i
          type(rational)                :: res
       End Function Rational_Integer_Mod

       Elemental Module Function Rational_Integer_Modulo(R,I) Result(Res)
          !---- Arguments ----!
          type(rational),  intent (in) :: r
          integer(kind=8),intent(in)  :: i
          type(rational)               :: res
       End Function Rational_Integer_Modulo

       Elemental Module Function Rational_Mod(R) Result(Res)
          !---- Arguments ----!
          type(rational), intent (in) :: r
          integer                     :: res
       End Function Rational_Mod

       Elemental Module Function Rational_Modulo(R) Result(Res)
          !---- Arguments ----!
          type(rational), intent (in) :: r
          integer                     :: res
       End Function Rational_Modulo

       Elemental Module Function Rational_Nint(R) Result(Res)
          !---- Arguments ----!
          type(rational),  intent (in)  :: r
          integer(kind=8)              :: res
       End Function Rational_Nint

       Elemental Module Function Rational_Real(R) Result (Res)
          !---- Arguments ----!
          type (rational),  intent(in) :: r
          real(kind=cp)                :: res
       End Function Rational_Real

       Pure Module Function Rational_Dot_Product(R1,R2) Result(Res)
          !---- Arguments ----!
          type(rational), dimension(:), intent (in) :: r1
          type(rational), dimension(:), intent (in) :: r2
          type(rational)                            :: res
       End Function Rational_Dot_Product

       Pure Module Function Rational_Matmul_Matmat(Mat1,Mat2) Result(Mat_Out)
          !---- Arguments ----!
          type(rational), dimension(:,:), intent (in)                 :: mat1
          type(rational), dimension(:,:), intent (in)                 :: mat2
          type(rational),dimension(size(mat1,dim=1),size(mat2,dim=2)) :: mat_out
       End Function Rational_Matmul_Matmat

       Pure Module Function Rational_Matmul_Matvec(Mat,Vec) Result(Vec_Out)
          !---- Arguments ----!
          type(rational), dimension(:,:), intent (in) :: mat
          type(rational), dimension(:),   intent (in) :: vec
          type(rational), dimension(size(vec))        :: vec_out
       End Function Rational_Matmul_Matvec

       Pure Module Function Rational_Maxloc_Matrix(Mat) Result(Pos_Max)
          !---- Arguments ----!
          type(rational),  dimension(:,:), intent(in) :: Mat
          integer, dimension(2)                       :: pos_max
       End Function Rational_Maxloc_Matrix

       Pure Module Function Rational_Maxloc_Vector(Vec) Result(Pos_Max)
          !---- Arguments ----!
          type(rational), dimension(:), intent(in) :: vec
          integer                                  :: pos_max
       End Function Rational_Maxloc_Vector

       Pure Module Function Rational_Maxval_Matrix(R) Result(Res)
          !---- Arguments ----!
          type(rational), dimension(:,:), intent(in) :: r
          type(rational)                              :: Res
       End Function Rational_Maxval_Matrix

       Pure Module Function Rational_Maxval_Vector(R) Result(Res)
          !---- Arguments ----!
          type(rational), dimension(:), intent (in) :: r
          type(rational)                            :: res
       End Function Rational_Maxval_Vector

       Pure Module Function Rational_Minloc_Matrix(Mat) Result(Pos_Min)
          !---- Arguments ----!
          type(rational),  dimension(:,:), intent(in) :: Mat
          integer, dimension(2)                       :: pos_min
       End Function Rational_Minloc_Matrix

       Pure Module Function Rational_Minloc_Vector(Vec) Result(Pos_Min)
          !---- Arguments ----!
          type(rational), dimension(:), intent(in) :: vec
          integer                                  :: pos_min
       End Function Rational_Minloc_Vector

       Pure Module Function Rational_Minval_Matrix(R) Result(Res)
          !---- Arguments ----!
          type(rational), dimension(:,:), intent (in) :: r
          type(rational)                              :: res
       End Function Rational_Minval_Matrix

       Pure Module Function Rational_Minval_Vector(R) Result(Res)
          !---- Arguments ----!
          type(rational), dimension(:), intent (in) :: r
          type(rational)                            :: res
       End Function Rational_Minval_Vector

       Pure Module Function Rational_Sum_Vector(Vec) Result(Suma)
          !---- Arguments ----!
          type(rational), dimension(:), intent(in) :: vec
          type(rational)                           :: suma
       End Function Rational_Sum_Vector

       !> Equal_Rational
       Pure Module Function Rational_Equal_Matrix(Mat1,Mat2) Result(Equal)
          !---- Arguments ----!
          type(rational), dimension(:,:), intent(in) :: Mat1,Mat2
          logical                                    :: equal
       End Function Rational_Equal_Matrix

       Pure Module Function Rational_Equal_Vector(Vec1,Vec2) Result(Equal)
          !---- Arguments ----!
          type(rational), dimension(:), intent(in) :: vec1, vec2
          logical                                  :: equal
       End Function Rational_Equal_Vector

       !> Is_Integer_Rational
       Elemental Module Function Is_Integer_Rational_Scalar(R) Result(OK)
          !---- Arguments ----!
          type(rational), intent(in) :: r
          logical                    :: OK
       End Function Is_Integer_Rational_Scalar

       Pure Module Function Is_Integer_Rational_Matrix(Mat) Result(OK)
          !---- Arguments ----!
          type(rational), dimension(:,:), intent(in) :: Mat
          logical                                    :: OK
       End Function Is_Integer_Rational_Matrix

       Pure Module Function Is_Integer_Rational_Vector(Vec) Result(OK)
          !---- Arguments ----!
          type(rational), dimension(:), intent(in) :: vec
          logical                                  :: ok
       End Function Is_Integer_Rational_Vector

       !> Rational_RowEchelonForm
       Pure Module Subroutine Rational_RowEchelonForm_M(M)
          !---- Arguments ----!
          type(rational), dimension(:,:), intent(inout) :: M
       End Subroutine Rational_RowEchelonForm_M

       Pure Module Subroutine Rational_RowEchelonForm_MT(M,T)
          !---- Arguments ----!
          type(rational), dimension(:,:), intent(inout) :: M
          type(rational), dimension(:,:), intent(inout) :: T
       End Subroutine Rational_RowEchelonForm_MT

       Module Subroutine Rational_SmithNormalForm(M,D,P,Q)
          !---- Arguments ----!
          type(rational), dimension(:,:), intent(in)  :: M     ! M(NR,NC)
          type(rational), dimension(:,:), intent(out) :: D     ! D(NR,NC)
          type(rational), dimension(:,:), intent(out) :: P     ! P(NR,NR)
          type(rational), dimension(:,:), intent(out) :: Q     ! Q(NC,NC)
       End Subroutine Rational_SmithNormalForm

       !> Generic procedures
       Elemental Module Function Rational_Modulo_Lat(R) Result(S)
          !---- Arguments ----!
          type(rational), intent(in) :: r
          type(rational)             :: s
       End Function Rational_Modulo_Lat

       Module Function Rational_Inverse_Matrix(M) Result(B)
          !---- Local Variables ----!
          type(rational), dimension(:,:),    intent(in)  :: M
          type(rational), dimension(size(M,1),size(M,2)) :: B
       End Function Rational_Inverse_Matrix

       Module Function Rational_Trace(M) Result(R)
          !---- Arguments ----!
          type(rational), dimension(:,:), intent(in) :: M
          type(rational)                             :: R
       End Function Rational_Trace

       Pure Module Function Rational_Is_DiagonalMatrix(M) Result(Diagonal)
          !---- Arguments ----!
          type(rational), dimension(:,:), intent(in) :: M
          logical                                    :: Diagonal
       End Function Rational_Is_DiagonalMatrix

       Pure Module Function Rational_Is_NullVector(V) Result(nulo)
          !---- Arguments ----!
          type(rational), dimension(:), intent(in) :: v
          logical                                  :: nulo
       End Function Rational_Is_NullVector

       Pure Module Function Rational_Co_Linear(R,S,N) Result(OK)
          !---- Argument ----!
          type(rational), dimension(:), intent(in) :: R
          type(rational), dimension(:), intent(in) :: S
          integer, optional,            intent(in) :: n
          logical                                  :: OK
       End Function Rational_Co_linear

       Pure Module Function Rational_Rank(M) Result(k)
          !---- Arguments ----!
          type(rational), dimension(:,:), intent(in)  :: M
          integer                                     :: k
       End Function Rational_Rank

       Pure Recursive Module Function Rational_Determ(A) Result(Det)
          !---- Arguments ----!
          type(rational), dimension(:,:), intent(in) :: a
          type(rational)                             :: det
       End Function Rational_Determ

    End Interface

 contains
   !!----
   !!---- RATIONAL_IDENTITY_MATRIX
   !!----
   !!---- 08/04/2019 17:06:26
   !!
   Pure Subroutine Rational_Identity_Matrix(R)
       !---- Arguments ----!
       type(rational), dimension(:,:), intent(in out) :: R

       !---- Local variables ----!
       integer :: j, n1, n2

       !> Init
       R= 0_LI//1_LI

       n1=size(R,1)
       n2=size(R,2)
       if (n1 /= n2) return

       do j =1, n1
          R(j,j)= 1_LI//1_LI
       end do

    End Subroutine Rational_Identity_Matrix

    !!----
    !!---- R_GCD
    !!----
    !!---- 08/04/2019
    !!
    Pure Recursive Function R_Gcd(I, J) Result(Res)
       !---- Arguments ----!
       integer(kind=8), intent (in) :: i
       integer(kind=8), intent (in) :: j
       integer(kind=8)              :: res

       if (j == 0) then
          res=i
       else
          res=r_gcd(j, modulo(i, j))
       end if

    End Function R_Gcd

    !!----
    !!---- RATIONAL_SIMPLIFY
    !!----
    !!---- 08/04/2019
    !!
    Elemental Function Rational_Simplify(r) Result(Res)
       !---- Arguments ----!
       type(rational), intent (in) :: r
       type(rational)              :: res

       !---- Local Variables ----!
       integer(kind=8) :: g

       g=r_gcd(r%numerator, r%denominator)
       if (g /= 0) then
          res=(r%numerator / g) // (r%denominator / g)
       else
         res= r
       end if
    End Function Rational_Simplify

    !!----
    !!---- RATIONAL_RECIP
    !!----
    !!---- 08/04/2019
    !!
    Elemental Function Rational_Recip(R) result (S)
       !---- Arguments ----!
       type(rational), intent(in) :: r
       type(rational)             :: s

       s= 0_LI
       if (r%numerator /= 0_LI) s= r%denominator // r%numerator

    End Function Rational_Recip

    !!----
    !!---- RATIONAL_STRING
    !!----
    !!---- 08/04/2019
    !!
    Elemental Function Rational_String(R) Result (Str)
       !---- Arguments ----!
       type(rational),   intent(in)   :: r
       character(len=80)              :: Str

       !---- Local Variables ----!
       character(len=100) :: line
       type(rational)     :: sr

       if (r%denominator /= 0_LI) then
          sr=rational_simplify(r)
       else
          sr=r
       end if
       if (sr%denominator == 1_LI) then
          write(unit=line,fmt="(i40)") sr%numerator
       else
          write(unit=line,fmt="(i40,a,i40)") sr%numerator,"/",sr%denominator
       end if
       str=adjustl(Pack_String(line))

    End Function Rational_String

End Module CFML_Rational