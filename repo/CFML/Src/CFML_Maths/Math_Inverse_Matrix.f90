!!----
!!---- SUBMODULE CFML_Math_General
!!----
!!----
!!
Submodule (CFML_Maths) Maths_Inverse_Matrix
 implicit none
 Contains
    !!----
    !!---- INVERSE_MATRIX_C
    !!----
    !!---- 01/04/2019
    !!
    Module Function Inverse_Matrix_C(A) Result(Ainv)
       !---- Arguments ----!
       complex(kind=cp), dimension(:,:),     intent(in) :: A
       complex(kind=cp), dimension(size(a,1),size(a,1)) :: Ainv

       !---- Local Variables ----!
       integer :: n,n1,n2

       !> Init
       Ainv=(0.0_cp,0.0_cp)

       !> Control
       n1=size(a,1)
       n2=size(a,2)
       if (n1 <=1 .or. n2 <=1) then
          err_CFML%Ierr=1
          err_CFML%Msg="Inverse_Matrix@CFML_Maths: The dimension of the complex arrary have to be > 1!"
          return
       end if

       if (n1 /= n2) then
          err_CFML%Ierr=1
          err_CFML%Msg="Inverse_Matrix@CFML_Maths: The complex arrary have to be square NxN!"
          return
       end if

       n=n1
       select case (n)
          case (2)
             Ainv=MatInv2_C(A)
          case (3)
             Ainv=MatInv3_C(A)
          case (4)
             Ainv=MatInv4_C(A)
          case default
             Ainv=MatInvN_C(A,n)
       end select

    End Function Inverse_Matrix_C

    !!----
    !!---- INVERSE_Matrix_I
    !!----
    !!---- 01/04/2019
    !!
    Module Function Inverse_Matrix_I(A) Result(Ainv)
       !---- Arguments ----!
       integer, dimension(:,:), intent(in) :: A
       real(kind=cp), dimension(size(a,1),size(a,1)) :: Ainv

       !---- Local Variables ----!
       integer :: n, n1,n2
       real(kind=cp), dimension(size(a,1),size(a,1)) :: aa

       !> Init
       Ainv=0.0_cp

       !> Control
       n1=size(a,1)
       n2=size(a,2)
       if (n1 <=1 .or. n2 <=1) then
          err_CFML%Ierr=1
          err_CFML%Msg="Inverse_Matrix@CFML_Maths: The dimension of the integer arrary have to be > 1!"
          return
       end if

       if (n1 /= n2) then
          err_CFML%Ierr=1
          err_CFML%Msg="Inverse_Matrix@CFML_Maths: The integer arrary have to be square NxN!"
          return
       end if

       !> Copy
       aa=real(a,kind=cp)

       n=n1
       select case (n)
          case (2)
             Ainv=MatInv2_R(aa)
          case (3)
             Ainv=MatInv3_R(aa)
          case (4)
             Ainv=MatInv4_R(aa)
          case default
             !Ainv=MatInvN_R(aa,n)
             call Invert_Matrix_R(aa,ainv)
       end select

    End Function Inverse_Matrix_I

    !!----
    !!---- INVERSE_Matrix_R
    !!----
    !!---- 01/04/2019
    !!
    Module Function Inverse_Matrix_R(A) Result(Ainv)
       !---- Arguments ----!
       real(kind=cp), dimension(:,:),      intent(in) :: A
       real(kind=cp), dimension(size(a,1),size(a,1))  :: Ainv

       !---- Local Variables ----!
       integer :: n, n1, n2

       !> Init
       Ainv=0.0_cp

       !> Control
       n1=size(a,1)
       n2=size(a,2)
       if (n1 <=1 .or. n2 <=1) then
          err_CFML%Ierr=1
          err_CFML%Msg="Inverse_Matrix@CFML_Maths: The dimension of the integer arrary have to be > 1!"
          return
       end if

       if (n1 /= n2) then
          err_CFML%Ierr=1
          err_CFML%Msg="Inverse_Matrix@CFML_Maths: The integer arrary have to be square NxN!"
          return
       end if

       n=n1
       select case (n)
          case (2)
             Ainv=MatInv2_R(A)
          case (3)
             Ainv=MatInv3_R(A)
          case (4)
             Ainv=MatInv4_R(A)
          case default
             !Ainv=MatInvN_R(A,n)
             call Invert_Matrix_R(A,Ainv)
       end select

    End Function Inverse_Matrix_R

    !!----
    !!---- MATINVN_C
    !!----
    !!---- 01/04/2019
    !!
    Module Function MatInvN_C(A,n) Result(Ainv)
       !---- Arguments ----!
       complex(kind=cp), dimension(:,:), intent(in) :: A
       integer,                          intent(in) :: N
       complex(kind=cp), dimension(n,n)             :: Ainv

       !---- Local Variables ----!
       complex(kind=cp), dimension(n,n) :: aa
       complex(kind=cp), dimension(n)   :: cs
       complex(kind=cp)                 :: pv, tt, det
       real(kind=cp)                    :: pav
       integer,          dimension(n)   :: pc,pl
       integer                          :: i,j,k,ik,jk

       !> Init
       Ainv=cmplx(0.0,0.0)

       det=cmplx(1.0,0.0)
       pc=0; pl=0
       cs=cmplx(0.0,0.0)

       aa=a
       do k=1,n
          !> Searching greatest pivot
          pv=aa(k,k)
          ik=k
          jk=k
          pav=abs(pv)

          do i=k,n
             do j=k,n
                if (abs(aa(i,j)) > pav) then
                   pv=aa(i,j)
                   pav=abs(pv)
                   ik=i
                   jk=j
                end if
             end do
          end do

          !> Search terminated, the pivot is in location I=IK, J=JK.
          !> Memorizing pivot location:
          pc(k)=jk
          pl(k)=ik

          !> Determinant DET is actualised
          !>If DET=0, ERROR MESSAGE and STOP
          if (ik /= k) det=-det
          if (jk /= k) det=-det
          det=det*pv
          if (abs(det) < epsilon(1.0_cp)) then
             !> Error
             Err_CFML%Ierr=1
             Err_CFML%Msg="MatInv@CFML_Maths: Singular matrix!"
             return
          end if

          !> positionning pivot in k,k
          if (ik /= k) then
             do i=1,n
                !> exchange lines ik and k
                tt=aa(ik,i)
                aa(ik,i)=aa(k,i)
                aa(k,i)=tt
             end do
          end if

          !> Pivot is at correct line
          if (jk /= k) then
             do i=1,n
                !> exchange columns jk and k of matrix aa
                tt=aa(i,jk)
                aa(i,jk)=aa(i,k)
                aa(i,k)=tt
             end do
          end if

          !> Pivot is at correct column and located in K,K
          !> Store column K in vector CS then set column K to zero
          do i=1,n
             cs(i)=aa(i,k)
             aa(i,k)=cmplx(0.0,0.0)
          end do

          cs(k)=cmplx(0.0,0.0)
          aa(k,k)=cmplx(1.0,0.0)

          !> Modify line K :
          if (abs(pv) < epsilon(1.0_cp)) then
             !> Error
             Err_CFML%Ierr=1
             Err_CFML%Msg="MatInv@CFML_Maths: Pivot to small to work fine!"
             return
          end if

          do i=1,n
             aa(k,i)=aa(k,i)/pv
          end do

          !>Modify other lines of matrix AA:
          do j=1,n
             if (j == k) continue
             do i=1,n
                !> modify line j of matrix aa
                aa(j,i)=aa(j,i)-cs(j)*aa(k,i)
             end do
          end do

          !> End of K loop
       end do

       !> The matrix AA is inverted - Rearrange AA
       !> Exchange lines
       do i=n,1,-1
          ik=pc(i)
          if (ik == i) continue

          !> exchange lines i and pc(i) of aa:
          do j=1,n
             tt=aa(i,j)
             aa(i,j)=aa(ik,j)
             aa(ik,j)=tt
          end do
       end do

       !> exchange columns
       do j=n,1,-1
          jk=pl(j)
          if (jk == j) continue

          !> exchange columns j and pl(j) of aa :
          do i=1,n
             tt=aa(i,j)
             aa(i,j)=aa(i,jk)
             aa(i,jk)=tt
          end do
       end do

       Ainv=aa

   End Function MatInvN_C

    !!----
    !!---- Inverse_Matrix_R
    !!----    invert a real matrix using LU decomposition.
    !!----
    !!---- 01/04/2019
    !!
    Module Subroutine Invert_Matrix_R(a,b,perm)
       !---- Arguments ----!
       real(kind=cp), dimension(:,:),  intent(in ) :: a         ! Input Array
       real(kind=cp), dimension(:,:),  intent(out) :: b         ! Output array
       integer, dimension(:),optional, intent(out) :: perm

       !---- Local variables ----!
       integer                                       :: i,n
       integer,       dimension(size(a,1))           :: indx
       logical                                       :: singular
       real(kind=cp)                                 :: d, det
       real(kind=cp), dimension(size(a,1),size(a,1)) :: lu


       n=size(a,1)
       lu=a(1:n,1:n)

       call LU_Decomp(lu,d,singular,indx)
       if (present(perm)) perm(1:n)=indx(1:n)

       if (singular) then
          err_CFML%Ierr=1
          err_CFML%Msg="Invert_Matrix@CFML_Maths: Singular Matrix using LU_Decomp procedure"
          b=lu
          return
       else
          det=0.0
          do i=1,n
             d=d*sign(1.0_cp,lu(i,i))
             det=det + log(abs(lu(i,i)))
          end do
          det=d*exp(det)
          if (abs(det) <= tiny(1.0_cp)) then
             err_CFML%Ierr=1
             err_CFML%Msg="Invert_Matrix@CFML_Maths: Singular Matrix using LU_Decomp procedure"
             b=lu
             return
          end if
       end if

       b=0.0
       do i=1,n
          b(i,i)=1.0
          call LU_backsub(lu,indx,b(:,i))
       end do

    End Subroutine Invert_Matrix_R

    !!----
    !!---- LU_BACKSUB
    !!--<<
    !!----    Adapted from Numerical Recipes.
    !!----    Solves the set of N linear equations A  X = B. Here the N x N matrix A is input,
    !!----    not as the original matrix A, but rather as its LU decomposition, determined
    !!----    by the routine LU_DECOMP. INDX is input as the permutation vector of length N
    !!----    returned by LU_DECOMP. B is input as the right-hand-side vector B,
    !!----    also of length N, and returns with the solution vector X.
    !!----    A and INDX are not modified by this routine and can be left in place for successive calls
    !!----    with different right-hand sides B. This routine takes into account the possibility that B will
    !!----    begin with many zero elements, so it is efficient for use in matrix inversion.
    !!-->>
    !!----
    !!---- 01/04/2019
    !!
    Module Subroutine LU_Backsub(a,indx,b)
       !---- Arguments ----!
       real(kind=cp), dimension(:,:), intent(in)     :: a
       integer,       dimension(:),   intent(in)     :: indx
       real(kind=cp), dimension(:),   intent(in out) :: b

       !---- Local Variables ----!
       integer       :: i,ii,ll,n
       real(kind=cp) :: summ

       n=size(a,1)
       ii=0              !When ii is set to a positive value, it will become the index
       do i=1,n          !of the first nonvanishing element of b. We now do
          ll=indx(i)     !the forward substitution. The only new wrinkle is to
          summ=b(ll)     !unscramble the permutation as we go.
          b(ll)=b(i)
          if (ii /= 0) then
             summ=summ-dot_product(a(i,ii:i-1),b(ii:i-1))
          else if(summ /= 0.0) then   !A nonzero element was encountered, so from now on
             ii=i                       !we will have to do the dot product above.
          end if
          b(i)=summ
       end do

       do i=n,1,-1       !Now we do the backsubstitution
          b(i) = (b(i)-dot_product(a(i,i+1:n),b(i+1:n)))/a(i,i)
       end do

    End Subroutine LU_Backsub

    !!----
    !!---- LU_DECOMP
    !!--<<
    !!----    Subroutine to make the LU decomposition of an input matrix A.
    !!----    The input matrix is destroyed and replaced by a matrix containing
    !!----    in its upper triangular part (plus diagonal) the matrix U. The
    !!----    lower triangular part contains the nontrivial part (Lii=1) of matrix L.
    !!----    The output is rowwise permutation of the initial matrix. The vector INDX
    !!----    recording the row permutation. D is output as +/-1 depending on whether
    !!----    the number of row interchanges was even or odd, respectively.
    !!-->>
    !!----
    !!---- 01/04/2019
    !!
    Module Subroutine LU_Decomp(a,d,singular,indx)
       !---- Arguments ----!
       real(kind=cp), dimension(:,:),    intent(in out) :: a
       real(kind=cp),                    intent(out)    :: d
       logical,                          intent(out)    :: singular
       integer,  dimension(:), optional, intent(out)    :: indx

       !---- Local variables ----!
       real(kind=cp), dimension(size(a,1)):: vv  !vv stores the implicit scaling of each row.
       real(kind=cp), parameter           :: vtiny = epsilon(1.0_cp) !A small number.
       integer                            :: j,jj,imax,n

       !> Init
       singular=.false.
       n=size(a,1)
       if (present(indx)) then
          do j=1,n
             indx(j)=j
          end do
       end if

       d=1.0                      !No row interchanges yet.
       vv=maxval(abs(a),dim=2)    !Loop over rows to get the implicit scaling information.
       if (any(abs(vv) <= vtiny)) then   !There is a row of zeros.
          singular=.true.
          return
       end if
       vv=1.0_cp/vv     !Save the scaling.

       do j=1,n
          jj=maxloc(vv(j:n)*abs(a(j:n,j)),dim=1)
          imax=(j-1)+jj                               !Find the pivot row.
          if (j /= imax) then                         !Do we need to interchange rows?
             call swap(a(imax,:),a(j,:))              !Yes, do so...
             d=-d                                     !...and change the parity of d.
             vv(imax)=vv(j)                           !Also interchange the scale factor.
          end if
          if (present(indx)) indx(j)=imax
          if (abs(a(j,j)) <= vtiny) then !If the pivot element is zero the matrix is singular.
             a(j,j)=vtiny                !(at least to the precision of the algorithm)
             singular=.true.             !For some applications on singular matrices,
          end if                         !This is actually the present case
          a(j+1:n,j)=a(j+1:n,j)/a(j,j)                                    !Divide by the pivot element.
          a(j+1:n,j+1:n)=a(j+1:n,j+1:n)-outerprod(a(j+1:n,j),a(j,j+1:n))  !Reduce remaining submatrix.
       end do

    End Subroutine LU_Decomp

    !!----
    !!---- LU_DESCOMPOSITION
    !!----
    !!---- 19/04/2019
    !!
    Pure Module Subroutine LU_Descomposition(a,p)
       !---- Arguments ----!
       real(kind=cp), intent(in out) :: a(:,:)
       integer,       intent(   out) :: p(:)

       !---- Local Variables ----!
       integer :: n,i,j,k,kmax

       n=size(a,1)
       p=[ ( i, i=1,n ) ]
       do k = 1,n-1
          kmax = maxloc(abs(a(p(k:),k)),1) + k-1
          if (kmax /= k ) p([k, kmax]) = p([kmax, k])
          a(p(k+1:),k) = a(p(k+1:),k) / a(p(k),k)
          forall (j=k+1:n) a(p(k+1:),j) = a(p(k+1:),j) - a(p(k+1:),k) * a(p(k),j)
       end do

    End Subroutine LU_Descomposition

    !!----
    !!---- MATINV2_C
    !!----    Performs a direct calculation of the inverse of a 2×2 matrix
    !!----
    !!---- 02/04/2019
    !!
    Module Function MatInv2_C(A) Result(B)
       !---- arguments ----!
       complex(kind=cp), dimension(2,2), intent(in) :: A  !! Matrix
       complex(kind=cp), dimension(2,2)             :: B   !! Inverse matrix

       !---- Local Variables ----!
       complex(kind=cp)             :: det,detinv

       !> Init
       B=cmplx(0.0_cp,0.0_cp)

       !> Deter
       det=determ(A,2)
       if (abs(det) <= epss) then
          Err_CFML%Ierr=1
          Err_CFML%MSG="MATINV2_C@MATHS: Determinant is zero, singular matrix!"
       end if

       !> Calculate the inverse determinant of the matrix
       detinv = 1/(A(1,1)*A(2,2) - A(1,2)*A(2,1))

       !> Calculate the inverse of the matrix
       B(1,1) = +detinv * A(2,2)
       B(2,1) = -detinv * A(2,1)
       B(1,2) = -detinv * A(1,2)
       B(2,2) = +detinv * A(1,1)

    End Function MatInv2_C

    !!----
    !!---- MATINV2_R
    !!----    Performs a direct calculation of the inverse of a 2×2 matrix
    !!----
    !!---- 02/04/2019
    !!
    Module Function MatInv2_R(A) Result(B)
       !---- arguments ----!
       real(kind=cp), dimension(2,2), intent(in) :: A   !! Matrix
       real(kind=cp), dimension(2,2)             :: B   !! Inverse matrix

       !---- Local Variables ----!
       real(kind=cp) :: det, detinv

       !> Init
       B=0.0_cp

       !> Deter
       det=determ(A,2)
       if (abs(det) <= epss) then
          Err_CFML%Ierr=1
          Err_CFML%MSG="MATINV2_R@MATHS: Determinant is zero, singular matrix!"
       end if

       !> Calculate the inverse determinant of the matrix
       detinv = 1/(A(1,1)*A(2,2) - A(1,2)*A(2,1))

       !> Calculate the inverse of the matrix
       B(1,1) = +detinv * A(2,2)
       B(2,1) = -detinv * A(2,1)
       B(1,2) = -detinv * A(1,2)
       B(2,2) = +detinv * A(1,1)

    End Function MatInv2_R

    !!----
    !!---- MATINV3_C
    !!----    Performs a direct calculation of the inverse of a 3×3 matrix
    !!----
    !!---- 02/04/2019
    !!
    Module Function MatInv3_C(A) Result(B)
       !---- Arguments ----!
       complex(kind=cp), dimension(3,3),intent(in) :: A   !! Matrix
       complex(Kind=cp), dimension(3,3)            :: B   !! Inverse matrix

       !---- Local variables ----!
       complex(Kind=cp) :: det, detinv

       !> Deter
       det=determ(A,3)
       if (abs(det) <= epss) then
          Err_CFML%Ierr=1
          Err_CFML%MSG="MATINV3_C@MATHS: Determinant is zero, singular matrix!"
       end if

       !> Calculate the inverse determinant of the matrix
       detinv = 1/(A(1,1)*A(2,2)*A(3,3) - A(1,1)*A(2,3)*A(3,2)&
                 - A(1,2)*A(2,1)*A(3,3) + A(1,2)*A(2,3)*A(3,1)&
                 + A(1,3)*A(2,1)*A(3,2) - A(1,3)*A(2,2)*A(3,1))

       !> Calculate the inverse of the matrix
       B(1,1) = +detinv * (A(2,2)*A(3,3) - A(2,3)*A(3,2))
       B(2,1) = -detinv * (A(2,1)*A(3,3) - A(2,3)*A(3,1))
       B(3,1) = +detinv * (A(2,1)*A(3,2) - A(2,2)*A(3,1))
       B(1,2) = -detinv * (A(1,2)*A(3,3) - A(1,3)*A(3,2))
       B(2,2) = +detinv * (A(1,1)*A(3,3) - A(1,3)*A(3,1))
       B(3,2) = -detinv * (A(1,1)*A(3,2) - A(1,2)*A(3,1))
       B(1,3) = +detinv * (A(1,2)*A(2,3) - A(1,3)*A(2,2))
       B(2,3) = -detinv * (A(1,1)*A(2,3) - A(1,3)*A(2,1))
       B(3,3) = +detinv * (A(1,1)*A(2,2) - A(1,2)*A(2,1))

    End Function MatInv3_C

    !!----
    !!---- MATINV3_R
    !!----    Performs a direct calculation of the inverse of a 3×3 matrix
    !!----
    !!---- 02/04/2019
    !!
    Module Function MatInv3_R(A) Result(B)
       !---- Arguments ----!
       real(kind=cp), dimension(3,3), intent(in) :: A   !! Matrix
       real(Kind=cp), dimension(3,3)             :: B   !! Inverse matrix

       !---- Local variables ----!
       real(Kind=cp) :: det,detinv

       !> Deter
       det=determ(A,3)
       if (abs(det) <= epss) then
          Err_CFML%Ierr=1
          Err_CFML%MSG="MATINV3_R@MATHS: Determinant is zero, singular matrix!"
       end if

       !> Calculate the inverse determinant of the matrix
       detinv = 1/(A(1,1)*A(2,2)*A(3,3) - A(1,1)*A(2,3)*A(3,2)&
                 - A(1,2)*A(2,1)*A(3,3) + A(1,2)*A(2,3)*A(3,1)&
                 + A(1,3)*A(2,1)*A(3,2) - A(1,3)*A(2,2)*A(3,1))

       !> Calculate the inverse of the matrix
       B(1,1) = +detinv * (A(2,2)*A(3,3) - A(2,3)*A(3,2))
       B(2,1) = -detinv * (A(2,1)*A(3,3) - A(2,3)*A(3,1))
       B(3,1) = +detinv * (A(2,1)*A(3,2) - A(2,2)*A(3,1))
       B(1,2) = -detinv * (A(1,2)*A(3,3) - A(1,3)*A(3,2))
       B(2,2) = +detinv * (A(1,1)*A(3,3) - A(1,3)*A(3,1))
       B(3,2) = -detinv * (A(1,1)*A(3,2) - A(1,2)*A(3,1))
       B(1,3) = +detinv * (A(1,2)*A(2,3) - A(1,3)*A(2,2))
       B(2,3) = -detinv * (A(1,1)*A(2,3) - A(1,3)*A(2,1))
       B(3,3) = +detinv * (A(1,1)*A(2,2) - A(1,2)*A(2,1))

    End Function MatInv3_R

    !!----
    !!---- MATINV4_C
    !!----    Performs a direct calculation of the inverse of a 4×4 matrix
    !!----
    !!---- 02/04/2019
    !!
    Module Function MatInv4_C(A) result(B)
       !---- Arguments ----!
       complex(kind=cp), dimension(4,4), intent(in) :: A   !! Matrix
       complex(kind=cp), dimension(4,4)             :: B   !! Inverse matrix

       !---- Local Variable ----!
       complex(kind=cp)             :: det,detinv

       !> Deter
       det=determ(A,4)
       if (abs(det) <= epss) then
          Err_CFML%Ierr=1
          Err_CFML%MSG="MATINV4_C@MATHS: Determinant is zero, singular matrix!"
       end if

       !> Calculate the inverse determinant of the matrix
       detinv = &
                1/(A(1,1)*(A(2,2)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+ &
                   A(2,3)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+ &
                   A(2,4)*(A(3,2)*A(4,3)-A(3,3)*A(4,2)))&
                 - A(1,2)*(A(2,1)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+ &
                   A(2,3)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+ &
                   A(2,4)*(A(3,1)*A(4,3)-A(3,3)*A(4,1)))&
                 + A(1,3)*(A(2,1)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+ &
                   A(2,2)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+ &
                   A(2,4)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))&
                 - A(1,4)*(A(2,1)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))+ &
                   A(2,2)*(A(3,3)*A(4,1)-A(3,1)*A(4,3))+ &
                   A(2,3)*(A(3,1)*A(4,2)-A(3,2)*A(4,1))))

       !> Calculate the inverse of the matrix
       B(1,1) = detinv*(A(2,2)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+ &
                A(2,3)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(2,4)*(A(3,2)*A(4,3)-A(3,3)*A(4,2)))
       B(2,1) = detinv*(A(2,1)*(A(3,4)*A(4,3)-A(3,3)*A(4,4))+ &
                A(2,3)*(A(3,1)*A(4,4)-A(3,4)*A(4,1))+A(2,4)*(A(3,3)*A(4,1)-A(3,1)*A(4,3)))
       B(3,1) = detinv*(A(2,1)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+ &
                A(2,2)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))
       B(4,1) = detinv*(A(2,1)*(A(3,3)*A(4,2)-A(3,2)*A(4,3))+ &
                A(2,2)*(A(3,1)*A(4,3)-A(3,3)*A(4,1))+A(2,3)*(A(3,2)*A(4,1)-A(3,1)*A(4,2)))
       B(1,2) = detinv*(A(1,2)*(A(3,4)*A(4,3)-A(3,3)*A(4,4))+ &
                A(1,3)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(1,4)*(A(3,3)*A(4,2)-A(3,2)*A(4,3)))
       B(2,2) = detinv*(A(1,1)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+ &
                A(1,3)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(1,4)*(A(3,1)*A(4,3)-A(3,3)*A(4,1)))
       B(3,2) = detinv*(A(1,1)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+ &
                A(1,2)*(A(3,1)*A(4,4)-A(3,4)*A(4,1))+A(1,4)*(A(3,2)*A(4,1)-A(3,1)*A(4,2)))
       B(4,2) = detinv*(A(1,1)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))+ &
                A(1,2)*(A(3,3)*A(4,1)-A(3,1)*A(4,3))+A(1,3)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))
       B(1,3) = detinv*(A(1,2)*(A(2,3)*A(4,4)-A(2,4)*A(4,3))+ &
                A(1,3)*(A(2,4)*A(4,2)-A(2,2)*A(4,4))+A(1,4)*(A(2,2)*A(4,3)-A(2,3)*A(4,2)))
       B(2,3) = detinv*(A(1,1)*(A(2,4)*A(4,3)-A(2,3)*A(4,4))+ &
                A(1,3)*(A(2,1)*A(4,4)-A(2,4)*A(4,1))+A(1,4)*(A(2,3)*A(4,1)-A(2,1)*A(4,3)))
       B(3,3) = detinv*(A(1,1)*(A(2,2)*A(4,4)-A(2,4)*A(4,2))+ &
                A(1,2)*(A(2,4)*A(4,1)-A(2,1)*A(4,4))+A(1,4)*(A(2,1)*A(4,2)-A(2,2)*A(4,1)))
       B(4,3) = detinv*(A(1,1)*(A(2,3)*A(4,2)-A(2,2)*A(4,3))+ &
                A(1,2)*(A(2,1)*A(4,3)-A(2,3)*A(4,1))+A(1,3)*(A(2,2)*A(4,1)-A(2,1)*A(4,2)))
       B(1,4) = detinv*(A(1,2)*(A(2,4)*A(3,3)-A(2,3)*A(3,4))+ &
                A(1,3)*(A(2,2)*A(3,4)-A(2,4)*A(3,2))+A(1,4)*(A(2,3)*A(3,2)-A(2,2)*A(3,3)))
       B(2,4) = detinv*(A(1,1)*(A(2,3)*A(3,4)-A(2,4)*A(3,3))+ &
                A(1,3)*(A(2,4)*A(3,1)-A(2,1)*A(3,4))+A(1,4)*(A(2,1)*A(3,3)-A(2,3)*A(3,1)))
       B(3,4) = detinv*(A(1,1)*(A(2,4)*A(3,2)-A(2,2)*A(3,4))+ &
                A(1,2)*(A(2,1)*A(3,4)-A(2,4)*A(3,1))+A(1,4)*(A(2,2)*A(3,1)-A(2,1)*A(3,2)))
       B(4,4) = detinv*(A(1,1)*(A(2,2)*A(3,3)-A(2,3)*A(3,2))+ &
                A(1,2)*(A(2,3)*A(3,1)-A(2,1)*A(3,3))+A(1,3)*(A(2,1)*A(3,2)-A(2,2)*A(3,1)))

    End Function MatInv4_C

    !!----
    !!---- MATINV4_R
    !!----    Performs a direct calculation of the inverse of a 4×4 matrix
    !!----
    !!---- 02/04/2019
    !!
    Module Function MatInv4_R(A) result(B)
       !---- Arguments ----!
       real(kind=cp), intent(in) :: A(4,4)   !! Matrix
       real(kind=cp)             :: B(4,4)   !! Inverse matrix

       !---- Local Variable ----!
       real(kind=cp)             :: det, detinv

       !> Deter
       det=determ(A,4)
       if (abs(det) <= epss) then
          Err_CFML%Ierr=1
          Err_CFML%MSG="MATINV4_R@MATHS: Determinant is zero, singular matrix!"
       end if

       !> Calculate the inverse determinant of the matrix
       detinv = &
                1/(A(1,1)*(A(2,2)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+ &
                   A(2,3)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+ &
                   A(2,4)*(A(3,2)*A(4,3)-A(3,3)*A(4,2)))&
                 - A(1,2)*(A(2,1)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+ &
                   A(2,3)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+ &
                   A(2,4)*(A(3,1)*A(4,3)-A(3,3)*A(4,1)))&
                 + A(1,3)*(A(2,1)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+ &
                   A(2,2)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+ &
                   A(2,4)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))&
                 - A(1,4)*(A(2,1)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))+ &
                   A(2,2)*(A(3,3)*A(4,1)-A(3,1)*A(4,3))+ &
                   A(2,3)*(A(3,1)*A(4,2)-A(3,2)*A(4,1))))

       !> Calculate the inverse of the matrix
       B(1,1) = detinv*(A(2,2)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+ &
                A(2,3)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+A(2,4)*(A(3,2)*A(4,3)-A(3,3)*A(4,2)))
       B(2,1) = detinv*(A(2,1)*(A(3,4)*A(4,3)-A(3,3)*A(4,4))+ &
                A(2,3)*(A(3,1)*A(4,4)-A(3,4)*A(4,1))+A(2,4)*(A(3,3)*A(4,1)-A(3,1)*A(4,3)))
       B(3,1) = detinv*(A(2,1)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+ &
                A(2,2)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(2,4)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))
       B(4,1) = detinv*(A(2,1)*(A(3,3)*A(4,2)-A(3,2)*A(4,3))+ &
                A(2,2)*(A(3,1)*A(4,3)-A(3,3)*A(4,1))+A(2,3)*(A(3,2)*A(4,1)-A(3,1)*A(4,2)))
       B(1,2) = detinv*(A(1,2)*(A(3,4)*A(4,3)-A(3,3)*A(4,4))+ &
                A(1,3)*(A(3,2)*A(4,4)-A(3,4)*A(4,2))+A(1,4)*(A(3,3)*A(4,2)-A(3,2)*A(4,3)))
       B(2,2) = detinv*(A(1,1)*(A(3,3)*A(4,4)-A(3,4)*A(4,3))+ &
                A(1,3)*(A(3,4)*A(4,1)-A(3,1)*A(4,4))+A(1,4)*(A(3,1)*A(4,3)-A(3,3)*A(4,1)))
       B(3,2) = detinv*(A(1,1)*(A(3,4)*A(4,2)-A(3,2)*A(4,4))+ &
                A(1,2)*(A(3,1)*A(4,4)-A(3,4)*A(4,1))+A(1,4)*(A(3,2)*A(4,1)-A(3,1)*A(4,2)))
       B(4,2) = detinv*(A(1,1)*(A(3,2)*A(4,3)-A(3,3)*A(4,2))+ &
                A(1,2)*(A(3,3)*A(4,1)-A(3,1)*A(4,3))+A(1,3)*(A(3,1)*A(4,2)-A(3,2)*A(4,1)))
       B(1,3) = detinv*(A(1,2)*(A(2,3)*A(4,4)-A(2,4)*A(4,3))+ &
                A(1,3)*(A(2,4)*A(4,2)-A(2,2)*A(4,4))+A(1,4)*(A(2,2)*A(4,3)-A(2,3)*A(4,2)))
       B(2,3) = detinv*(A(1,1)*(A(2,4)*A(4,3)-A(2,3)*A(4,4))+ &
                A(1,3)*(A(2,1)*A(4,4)-A(2,4)*A(4,1))+A(1,4)*(A(2,3)*A(4,1)-A(2,1)*A(4,3)))
       B(3,3) = detinv*(A(1,1)*(A(2,2)*A(4,4)-A(2,4)*A(4,2))+ &
                A(1,2)*(A(2,4)*A(4,1)-A(2,1)*A(4,4))+A(1,4)*(A(2,1)*A(4,2)-A(2,2)*A(4,1)))
       B(4,3) = detinv*(A(1,1)*(A(2,3)*A(4,2)-A(2,2)*A(4,3))+ &
                A(1,2)*(A(2,1)*A(4,3)-A(2,3)*A(4,1))+A(1,3)*(A(2,2)*A(4,1)-A(2,1)*A(4,2)))
       B(1,4) = detinv*(A(1,2)*(A(2,4)*A(3,3)-A(2,3)*A(3,4))+ &
                A(1,3)*(A(2,2)*A(3,4)-A(2,4)*A(3,2))+A(1,4)*(A(2,3)*A(3,2)-A(2,2)*A(3,3)))
       B(2,4) = detinv*(A(1,1)*(A(2,3)*A(3,4)-A(2,4)*A(3,3))+ &
                A(1,3)*(A(2,4)*A(3,1)-A(2,1)*A(3,4))+A(1,4)*(A(2,1)*A(3,3)-A(2,3)*A(3,1)))
       B(3,4) = detinv*(A(1,1)*(A(2,4)*A(3,2)-A(2,2)*A(3,4))+ &
                A(1,2)*(A(2,1)*A(3,4)-A(2,4)*A(3,1))+A(1,4)*(A(2,2)*A(3,1)-A(2,1)*A(3,2)))
       B(4,4) = detinv*(A(1,1)*(A(2,2)*A(3,3)-A(2,3)*A(3,2))+ &
                A(1,2)*(A(2,3)*A(3,1)-A(2,1)*A(3,3))+A(1,3)*(A(2,1)*A(3,2)-A(2,2)*A(3,1)))

    End Function MatInv4_R

End Submodule Maths_Inverse_Matrix
