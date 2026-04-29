!!----
!!---- SUBMODULE CFML_Math_General
!!----
!!----
!!
Submodule (CFML_Maths) Maths_Diagonalize_SH
 implicit none
 Contains

    !!--++ SUBROUTINE DIAGONALIZE_HERM
    !!--++
    !!--++    (OVERLOADED)
    !!--++    Diagonalize Hermitian matrices.
    !!--++    The eigen_values E_val are sorted in descending order if 'norder' is not present.
    !!--++    The columns of E_vect are the corresponding eigenvectors.
    !!--++
    !!--++ Update: February - 2005, January -2018 (JRC)
    !!
    Module Subroutine Diagonalize_Herm(a,n,e_val,e_vect,norder)
       !---- Arguments ----!
       complex(kind=cp),           dimension(:,:), intent( in)  :: A
       integer,                                    intent( in)  :: n
       real(kind=cp),              dimension(:),   intent(out)  :: E_val   ! Eigenvalues
       complex(kind=cp), optional, dimension(:,:), intent(out)  :: E_vect  ! Eigenvectors
       logical, optional,                          intent(in)   :: norder  ! If present no ordering

       !---- Local variables ----!
       real(kind=cp),        dimension(2*n,2*n)   :: aux
       real(kind=cp),        dimension(2*n)       :: e,d
       integer :: nn

       !> Init
       e_val=0.0_cp

       if (n > size(A,1) .or. n > size(A,2)) then
          err_cfml%ierr=1
          err_cfml%msg="MATHS@DIAGONALIZE_HERM: Error in dimension of input matrix: A(m,m) with m < n"
          return
       end if

       nn=2*n
       aux(  1:n ,  1:n ) =  real(a(1:n ,1:n))   !      (  U   V )
       aux(n+1:nn,n+1:nn) =  real(a(1:n ,1:n))   !   M=(          ),   A = U + i V
       aux(n+1:nn,  1:n ) = aimag(a(1:n ,1:n))   !      ( -V   U )
       aux(  1:n ,n+1:nn) =-aimag(a(1:n ,1:n))   !

       if (present(E_vect)) then
          call Diagonalize_PR_Tred2(aux,nn,d,e)
          call Diagonalize_PR_Tqli2(d,e,nn,aux)
          if(.not. present(norder)) call Diagonalize_EigenvSort(d,aux,nn,1)
          e_vect(1:n,1:n)=cmplx(aux(1:n,1:nn:2),aux(n+1:nn,1:nn:2))
       else
          call Diagonalize_PR_Tred1(aux,nn,d,e)
          call Diagonalize_PR_Tqli1(d,e,nn)
          if(.not. present(norder)) call Diagonalize_EigenvSort(d,aux,nn,0)
       end if
       e_val(1:n)=d(1:nn:2)

    End Subroutine Diagonalize_Herm

    !!----
    !!---- DIAGONALIZE_SYMM
    !!----
    !!----    (OVERLOADED)
    !!----    Diagonalize symmetric matrices
    !!----    The eigen_values E_val are sorted in descending order if 'norder' is not present.
    !!----    The columns of E_vect are the corresponding eigenvectors.
    !!----
    !!---- 03/04/2019
    !!
    Module Subroutine Diagonalize_Symm(A,n,E_Val,E_vect,norder)
       !---- Arguments ----!
       real(kind=cp),           dimension(:,:), intent( in)  :: A
       integer,                                 intent( in)  :: n
       real(kind=cp),           dimension(:),   intent(out)  :: E_val    ! Eigenvalues
       real(kind=cp), optional, dimension(:,:), intent(out)  :: E_vect   ! Eigenvectors
       logical,       optional,                 intent(in)   :: norder   ! If present no ordering

       !---- Local variables ----!
       real(kind=cp),        dimension(n,n)   :: aux
       real(kind=cp),        dimension(n)     :: e

       !> Init
       e_val=0.0_cp

       if (n > size(A,1) .or. n > size(A,2)) then
          err_cfml%Ierr=1
          err_cfml%msg="MATHS@DIAGONALIZE_SYMM: Error in dimension of input matrix: A(m,m) with m < n"
          return
       end if

       aux=a(1:n,1:n)
       if (present(E_vect)) then
          call Diagonalize_PR_Tred2(aux,n,E_val,e)
          call Diagonalize_PR_Tqli2(E_val,e,n,aux)
          if(.not. present(norder)) call Diagonalize_EigenvSort(E_val,aux,n,1)
          e_vect(1:n,1:n)=aux
       else
          call Diagonalize_PR_Tred1(aux,n,E_val,e)
          call Diagonalize_PR_Tqli1(E_val,e,n)
          if(.not. present(norder)) call Diagonalize_EigenvSort(E_val,aux,n,0)
       end if

    End Subroutine Diagonalize_Symm

    !!----
    !!---- DIAGONALIZE_PR_Tqli1
    !!----
    !!----    (PRIVATE)
    !!----    QL-algorithm with implicit shifts, to determine the eigenvalues
    !!----    and eigenvectors of a real tridiagonal symmetric matrix, or of
    !!----    a real symmetric matrix previously reduced by tred. D is a vector
    !!----    with the diagonal elements of the tridiagonal matrix. on output
    !!----    it returns the eigenvalues. the vector e inputs the subdiagonal
    !!----    elements of the tridiagonal matrix, with E(1) arbitrary. on
    !!----    output e is destroyed.
    !!----    In TLQ1 only the eigenvalues are calculated
    !!----
    !!---- 03/04/2019
    !!
    Module Subroutine Diagonalize_PR_Tqli1(d,e,n)
       !---- Arguments ----!
       real(kind=cp), dimension(:), intent(in out):: d, e ! d(np),e(np)
       integer,                     intent(in )   :: n

       !---- Local variables ----!
       integer, parameter :: NMAX_ITER=200
       integer      :: i, iter, l, m, mv
       real(kind=cp):: b, c, dd, f, g, p, r, s, comp

       !> init
       do i=2,n
          e(i-1)=e(i)
       end do
       e(n)=0.0_cp

       do l=1,n
          iter=0
          do_g : do
             mv=n
             do m=l,n-1
                dd=abs(d(m))+abs(d(m+1))
                comp= abs(e(m))+dd
                if (abs(comp-dd) <= epsilon(1.0_cp)) then
                   mv=m
                   exit
                end if
             end do
             m=mv

             if (m /= l) then
                if (iter == NMAX_ITER) then
                   err_cfml%ierr=1
                   err_cfml%msg="MATHS@DIAGONALIZE_PR_TQLI1: Too many iterations"
                   exit
                end if

                iter=iter+1
                g=(d(l+1)-d(l))/(2.0*e(l))
                r=sqrt(g*g+1.0)
                g=d(m)-d(l)+e(l)/(g+sign(r,g))
                s=1.0
                c=1.0
                p=0.0
                do i=m-1,l,-1
                   f=s*e(i)
                   b=c*e(i)
                   r=sqrt(f*f+g*g)
                   e(i+1)=r
                   if (abs(r)  <= epss) then
                      d(i+1)=d(i+1)-p
                      e(m)=0.0
                      cycle do_g
                   end if
                   s=f/r
                   c=g/r
                   g=d(i+1)-p
                   r=(d(i)-g)*s+2.0*c*b
                   p=s*r
                   d(i+1)=g+p
                   g=c*r-b
                end do
                d(l)=d(l)-p
                e(l)=g
                e(m)=0.0
                cycle do_g
             end if
             exit
          end do do_g
       end do

    End Subroutine Diagonalize_PR_Tqli1

    !!----
    !!---- DIAGONALIZE_PR_TqlI2
    !!----
    !!----    (PRIVATE)
    !!----    QL-algorithm with implicit shifts, to determine the eigenvalues
    !!----    and eigenvectors of a real tridiagonal symmetric matrix, or of
    !!----    a real symmetric matrix previously reduced by tred. D is a vector
    !!----    with the diagonal elements of the tridiagonal matrix. on output
    !!----    it returns the eigenvalues. the vector e inputs the subdiagonal
    !!----    elements of the tridiagonal matrix, with E(1) arbitrary. on
    !!----    output e is destroyed.
    !!----    The eigenvectors of the tridiagonal matrix are calculated in TLQ2
    !!----    by providing the matrix Z  as the identity matrix on input. if the
    !!----    eigenvectors of the matrix reduced by tred are required, then Z
    !!----    is input as the matrix output of tred. in either cased, the k-th
    !!----    column of Z returns the mormalized eigenvector corresponding to
    !!----    D(k).
    !!----
    !!---- 03/04/2019
    !!
    Module Subroutine Diagonalize_PR_Tqli2(d,e,n,z)
       !---- Arguments ----!
       real(kind=cp), dimension(:),   intent(in out) :: d, e ! d(np),e(np)
       integer,                       intent(in )    :: n
       real(kind=cp), dimension(:,:), intent(in out) :: z    ! z(np,np)

       !---- Local Variables ----!
       integer, parameter :: NMAX_ITER=200
       integer            :: i, iter, k, l, m, mv
       real(kind=cp)      :: b, c, dd, f, g, p, r, s, comp

       !> init
       do i=2,n
          e(i-1)=e(i)
       end do
       e(n)=0.0_cp

       do l=1,n
          iter=0
          do_g: do
             mv=n
             do m=l,n-1
                dd=abs(d(m))+abs(d(m+1))
                comp= abs(e(m))+dd
                if (abs(comp-dd) <= epsilon(1.0_cp)) then
                   mv=m
                   exit
                end if
             end do
             m=mv
             if (m /= l) then
                if (iter == NMAX_ITER) then
                   err_cfml%ierr=1
                   err_cfml%msg="MATHS@DIAGONALIZE_PR_TQLI2: Too many iterations"
                   exit
                end if

                iter=iter+1
                g=(d(l+1)-d(l))/(2.0*e(l))
                r=sqrt(g*g+1.0)
                g=d(m)-d(l)+e(l)/(g+sign(r,g))
                s=1.0
                c=1.0
                p=0.0
                do i=m-1,l,-1
                   f=s*e(i)
                   b=c*e(i)
                   r=sqrt(f*f+g*g)
                   e(i+1)=r
                   if (abs(r) <= epsilon(1.0_cp)) then
                      d(i+1)=d(i+1)-p
                      e(m)=0.0
                      cycle do_g
                   end if
                   s=f/r
                   c=g/r
                   g=d(i+1)-p
                   r=(d(i)-g)*s+2.0*c*b
                   p=s*r
                   d(i+1)=g+p
                   g=c*r-b

                   !> omit lines from here ...
                   do k=1,n
                      f=z(k,i+1)
                      z(k,i+1)=s*z(k,i)+c*f
                      z(k,i)=c*z(k,i)-s*f
                   end do

                   !> to here when finding only eigenvalues.
                end do
                d(l)=d(l)-p
                e(l)=g
                e(m)=0.0
                cycle do_g
             end if
             exit
          end do do_g
       end do

    End Subroutine Diagonalize_PR_Tqli2

    !!----
    !!---- DIAGONALIZE_PR_TRED1
    !!----
    !!----    (PRIVATE)
    !!----    Subroutine for preparing the matrix to find only eigenvalues
    !!----    Householder reduction of a real symetric nxn matrix A.
    !!----    On output A is replaced by the orthogonal matrix Q effecting
    !!----    the transformation. D returns the diagonal elements of the tri-
    !!----    diagonal matrix and E the off-diagonal elements with E(1)=0.
    !!----    In tred1 several lines have been deleted and A contains no
    !!----    useful information on output.
    !!----
    !!---- 03/04/2019
    !!
    Pure Module Subroutine Diagonalize_PR_Tred1(a,n,d,e)
       !---- Arguments ----!
       real(kind=cp), dimension(:,:), intent(in out) :: a    ! a(np,np)
       integer,                       intent(in)     :: n
       real(kind=cp), dimension(:),   intent(in out) :: d, e ! d(np),e(np)

       !---- Local Variables ----!
       integer          :: i, j, k, l
       real(kind=cp)    :: f, g, h, hh, scala

       do i=n,2,-1
          l=i-1
          h=0.0
          scala=0.0
          if (l > 1)then
             do k=1,l
                scala=scala+abs(a(i,k))
             end do
             if (abs(scala) <= epss) then
                e(i)=a(i,l)
             else
                do k=1,l
                   a(i,k)=a(i,k)/scala
                   h=h+a(i,k)**2
                end do
                f=a(i,l)
                g=-sign(sqrt(h),f)
                e(i)=scala*g
                h=h-f*g
                a(i,l)=f-g
                f=0.0
                do j=1,l
                   g=0.0
                   do k=1,j
                      g=g+a(j,k)*a(i,k)
                   end do
                   do k=j+1,l
                      g=g+a(k,j)*a(i,k)
                   end do
                   e(j)=g/h
                   f=f+e(j)*a(i,j)
                end do
                hh=f/(h+h)
                do j=1,l
                   f=a(i,j)
                   g=e(j)-hh*f
                   e(j)=g
                   do k=1,j
                      a(j,k)=a(j,k)-f*e(k)-g*a(i,k)
                   end do
                end do
             end if
          else
             e(i)=a(i,l)
          end if
          d(i)=h
       end do

       e(1)=0.0
       do i=1,n
          d(i)=a(i,i)
       end do

    End Subroutine Diagonalize_PR_Tred1

    !!----
    !!---- DIAGONALIZE_PR_TRED2
    !!----
    !!----    (PRIVATE)
    !!----    Subroutine for preparing the matrix to find the complete set
    !!----    of eigenvectors.
    !!----    Householder reduction of a real symetric nxn matrix A.
    !!----    On output A is replaced by the orthogonal matrix Q effecting
    !!----    the transformation. D returns the diagonal elements of the tri-
    !!----    diagonal matrix and E the off-diagonal elements with E(1)=0.
    !!----
    !!---- 03/04/2019
    !!
    Pure Module Subroutine Diagonalize_PR_Tred2(a,n,d,e)
       !---- Arguments ----!
       real(kind=cp), dimension(:,:), intent(in out) :: a    ! a(np,np)
       integer,                       intent(in)     :: n
       real(kind=cp), dimension(:),   intent(in out) :: d, e ! d(np),e(np)

       !---- Local variables ----!
       integer          :: i, j, k, l
       real(kind=cp)    :: f, g, h, hh, scala

       do i=n,2,-1
          l=i-1
          h=0.0
          scala=0.0
          if (l > 1)then
             do k=1,l
                scala=scala+abs(a(i,k))
             end do
             if (abs(scala) <= epss) then
                e(i)=a(i,l)
             else
                do k=1,l
                   a(i,k)=a(i,k)/scala
                   h=h+a(i,k)**2
                end do
                f=a(i,l)
                g=-sign(sqrt(h),f)
                e(i)=scala*g
                h=h-f*g
                a(i,l)=f-g
                f=0.0
                do j=1,l
                   !---- omit following line if finding only eigenvalues
                   a(j,i)=a(i,j)/h
                   g=0.0
                   do k=1,j
                      g=g+a(j,k)*a(i,k)
                   end do
                   do k=j+1,l
                      g=g+a(k,j)*a(i,k)
                   end do
                   e(j)=g/h
                   f=f+e(j)*a(i,j)
                end do
               hh=f/(h+h)
                do j=1,l
                   f=a(i,j)
                   g=e(j)-hh*f
                   e(j)=g
                   do k=1,j
                      a(j,k)=a(j,k)-f*e(k)-g*a(i,k)
                   end do
                end do
             end if
          else
             e(i)=a(i,l)
          end if
          d(i)=h
       end do

       !> omit following line if finding only eigenvalues.
       d(1)=0.0
       e(1)=0.0
       do i=1,n
          !> delete lines from here ...
          l=i-1
          if (abs(d(i)) > epsilon(1.0_cp)) then
             do j=1,l
                g=0.0
                do k=1,l
                   g=g+a(i,k)*a(k,j)
                end do
                do k=1,l
                   a(k,j)=a(k,j)-g*a(k,i)
                end do
             end do
          end if

          !> ... to here when finding only eigenvalues.
          d(i)=a(i,i)

          !> also delete lines from here ...
          a(i,i)=1.0
          do j=1,l
             a(i,j)=0.0
             a(j,i)=0.0
          end do
          !> ... to here when finding only eigenvalues.
       end do

    End Subroutine Diagonalize_PR_Tred2

    !!----
    !!---- DIAGONALIZE_EIGENVSORT
    !!----
    !!----    (PRIVATE)
    !!----    Subroutine for sorting eigenvalues in d(n) and eigenvectors
    !!----    in columns of v(n,n). Sorts d(n) in descending order and
    !!----    rearranges v(n,n) correspondingly. The method is the straight
    !!----    insertion. If io=0 order  only the eigenvalues are treated.
    !!----    Adapted from Numerical Recipes. Valid for hermitian matrices
    !!----
    !!---- 03/04/2019
    !!
    Pure Module Subroutine Diagonalize_EigenvSort(d,v,n,io)
       !---- Arguments ----!
       real(kind=cp), dimension(:),   intent(in out) :: d
       real(kind=cp), dimension(:,:), intent(in out) :: v
       integer,                       intent(in)     :: n
       integer,                       intent(in)     :: io

       !---- Local Variables ----!
       integer          :: i,j,k
       real(kind=cp)    :: p

       do i=1,n-1
          k=i
          p=d(i)
          do j=i+1,n
             if (d(j) >= p) then
                k=j
                p=d(j)
             end if
          end do
          if (k /= i) then
             d(k)=d(i)
             d(i)=p
             if (io == 1) then
                do j=1,n
                   p=v(j,i)
                   v(j,i)=v(j,k)
                   v(j,k)=p
                end do
             end if
          end if
       end do

    End Subroutine Diagonalize_EigenvSort

    !!----
    !!---- Pure Module Subroutine Orient_Eigenvectors(eval,evec)
    !!----
    !!----    real(kind=cp),dimension(3),intent(inout)     :: eval
    !!----    real(kind=cp),dimension(3,3),intent(inout)     :: evec
    !!----
    !!----  Re-orders eigenvectors and their eigenvalues so that #1 is close to +X of Cartesian, etc
    !!----  Written 2/2019 RJA
    !!----  Changed sort algorithm  4/2024 RJA because previous version was not correct for all cases.

    Pure Module Subroutine Orient_Eigenvectors(eval,evec)
      !---- Arguments ----!
      real(kind=cp),dimension(3),  intent(in out) :: eval
      real(kind=cp),dimension(3,3),intent(in out) :: evec

      !---- Local Variables ----!
      integer                        :: j
      integer,dimension(2)           :: ibig

      real(kind=cp),dimension(3)     :: val
      real(kind=cp),dimension(3,3)   :: vec,vecs

      !>working copies
      val=eval
      vec=evec
      vecs=evec

      !> do sort by copy from vec back to evec
      do j=1,3
          ibig=maxloc(abs(vec))       !Find largest abs(cosine) = smallest angle
          !>copy evec and eval from store to correct location in evec
          evec(1:3,ibig(2))=vecs(1:3,ibig(1))
          eval(ibig(2))=val(ibig(1))
          !>clear used values in vec to 0. which corresponds to 90deg
          vec(ibig(1),1:3)=0.0_cp
          vec(1:3,ibig(2))=0.0_cp
      end do

    End Subroutine Orient_Eigenvectors

End Submodule Maths_Diagonalize_SH
