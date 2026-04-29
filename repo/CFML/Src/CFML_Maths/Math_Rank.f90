!!----
!!---- SUBMODULE CFML_Math_General
!!----
!!----
!!
Submodule (CFML_Maths) Maths_Rank
 implicit none
 Contains

    !!----
    !!---- mRANK
    !!----    Computes the rank (in algebraic sense) of the rectangular matrix A.
    !!----
    !!---- 03/04/2019
    !!
    Module Function mRank(a,tol) Result(r)
       !---- Arguments ----!
       real(kind=cp), dimension(:,:),intent( in)      :: a     ! Input array
       real(kind=cp),                intent( in)      :: tol   ! Tolerance
       integer                                        :: r

       !---- Arguments ----!
       real(kind=cp), dimension(size(a,1),size(a,2))  :: u
       real(kind=cp), dimension(size(a,2))            :: w
       real(kind=cp), dimension(size(a,2),size(a,2))  :: v
       integer                                        :: i

       !> Init
       r=0

       u=a
       call svdcmp(u,w,v)
       if (err_cfml%ierr /=0) return

       r=0
       do i=1,size(a,2)
          if (w(i) > tol) r=r+1
       end do

    End Function mRank

    !!----
    !!---- SVDCMP
    !!----
    !!----  Given an m x n matrix A, this subroutine computes its singular value decomposition,
    !!----  A = U W Vt . The matrix U replaces A on output. The diagonal matrix of
    !!----  singular values W is output as the n-dimensional vector w. The nxn matrix V
    !!----  (not the transpose Vt )is output as v .
    !!----  Adapted from Numerical Recipes. Valid for arbitrary real matrices
    !!--++
    !!--++ 03/04/2019
    !!
    Module Subroutine Svdcmp(a,w,v)
       !---- Arguments ----!
       real(kind=cp),dimension(:,:),intent(in out) ::a   ! A(m,n)
       real(kind=cp),dimension(:),  intent(   out) ::w   ! W(n)
       real(kind=cp),dimension(:,:),intent(   out) ::v   ! V(n,n)

       !---- Local variables ----!
       integer, parameter                  :: num_its=500
       integer                             :: i,its,j,k,l,m,n,nm
       real(kind=cp)                       :: anorm,c,f,g,h,s,scal,x,y,z
       real(kind=cp),dimension(size(a,1))  :: tempm
       real(kind=cp),dimension(size(a,2))  :: rv1,tempn

       !> Init
       W=0.0_cp
       V=0.0_cp

       m=size(a,1)
       n=size(a,2)
       if ( .not. (size(v,1) == n .and. size(v,2) == n .and. size(w) == n)) then
          ERR_CFML%ierr=1
          ERR_CFML%Msg= "MATHS@SVDCMP: Physical dimensions of arguments are not compatible "
          return
       end if
       g=0.0_cp
       scal=0.0_cp

       do i=1,n
          l=i+1
          rv1(i)=scal*g
          g=0.0_cp
          scal=0.0_cp
          if (i <=m)then
             scal=sum(abs(a(i:m,i)))
             if ( abs(scal) > tiny(1.0_cp) ) then
                a(i:m,i)=a(i:m,i)/scal
                s=dot_product(a(i:m,i),a(i:m,i))
                f=a(i,i)
                g=-sign(sqrt(s),f)
                h=f*g-s
                a(i,i)=f-g
                tempn(l:n)=matmul(a(i:m,i),a(i:m,l:n))/h
                a(i:m,l:n)=a(i:m,l:n)+outerprod(a(i:m,i),tempn(l:n))
                a(i:m,i)=scal*a(i:m,i)
             end if
          end if
          w(i)=scal*g
          g=0.0_cp
          scal=0.0_cp
          if ((i <=m).and.(i /=n))then
             scal=sum(abs(a(i,l:n)))
             if ( abs(scal) > tiny(1.0_cp) ) then
                a(i,l:n)=a(i,l:n)/scal
                s=dot_product(a(i,l:n),a(i,l:n))
                f=a(i,l)
                g=-sign(sqrt(s),f)
                h=f*g-s
                a(i,l)=f-g
                rv1(l:n)=a(i,l:n)/h
                tempm(l:m)=matmul(a(l:m,l:n),a(i,l:n))
                a(l:m,l:n)=a(l:m,l:n)+outerprod(tempm(l:m),rv1(l:n))
                a(i,l:n)=scal*a(i,l:n)
             end if
          end if
       end do
       anorm=maxval(abs(w)+abs(rv1))
       do i=n,1,-1
          if (i <n) then
             if ( abs(g) > tiny(1.0_cp) ) then
                v(l:n,i)=(a(i,l:n)/a(i,l))/g
                tempn(l:n)=matmul(a(i,l:n),v(l:n,l:n))
                v(l:n,l:n)=v(l:n,l:n)+outerprod(v(l:n,i),tempn(l:n))
             end if
             v(i,l:n)=0.0_cp
             v(l:n,i)=0.0_cp
          end if
          v(i,i)=1.0_cp
          g=rv1(i)
          l=i
       end do
       do i=min(m,n),1,-1
          l=i+1
          g=w(i)
          a(i,l:n)=0.0_cp
          if ( abs(g) > tiny(1.0_cp) ) then
             g=1.0_cp/g
             tempn(l:n)=(matmul(a(l:m,i),a(l:m,l:n))/a(i,i))*g
             a(i:m,l:n)=a(i:m,l:n)+outerprod(a(i:m,i),tempn(l:n))
             a(i:m,i)=a(i:m,i)*g
          else
             a(i:m,i)=0.0_cp
          end if
          a(i,i)=a(i,i)+1.0_cp
       end do
       do k=n,1,-1
          do its=1,num_its
             do l=k,1,-1
                nm=l-1
                if ((abs(rv1(l))+anorm)==anorm) exit
                if ((abs(w(nm))+anorm)==anorm) then
                   c=0.0_cp
                   s=1.0_cp
                   do i=l,k
                      f=s*rv1(i)
                      rv1(i)=c*rv1(i)
                      if ((abs(f)+anorm)==anorm)exit
                      g=w(i)
                      h=hypot(f,g)
                      w(i)=h
                      h=1.0_cp/h
                      c=(g*h)
                      s=-(f*h)
                      tempm(1:m)=a(1:m,nm)
                      a(1:m,nm)=a(1:m,nm)*c+a(1:m,i)*s
                      a(1:m,i)=-tempm(1:m)*s+a(1:m,i)*c
                   end do
                   exit
                end if
             end do
             z=w(k)
             if (l == k)then
                if (z < 0.0_cp)then
                   w(k)=-z
                   v(1:n,k)=-v(1:n,k)
                end if
                exit
             end if
             if (its == num_its) then
                ERR_CFML%ierr=1
                ERR_CFML%Msg = "MATHS@SVDCMP: convergence not reached in SVD! "
                return
             end if
             x=w(l)
             nm=k-1
             y=w(nm)
             g=rv1(nm)
             h=rv1(k)
             f=((y-z)*(y+z)+(g-h)*(g+h))/(2.0_cp*h*y)
             g=hypot(f,1.0_cp)
             f=((x-z)*(x+z)+h*((y/(f+sign(g,f)))-h))/x
             c=1.0_cp
             s=1.0_cp
             do j=l,nm
                i=j+1
                g=rv1(i)
                y=w(i)
                h=s*g
                g=c*g
                z=hypot(f,h)
                rv1(j)=z
                c=f/z
                s=h/z
                f=(x*c)+(g*s)
                g=-(x*s)+(g*c)
                h=y*s
                y=y*c
                tempn(1:n)=v(1:n,j)
                v(1:n,j)=v(1:n,j)*c+v(1:n,i)*s
                v(1:n,i)=-tempn(1:n)*s+v(1:n,i)*c
                z=hypot(f,h)
                w(j)=z
                if ( abs(z) > tiny(1.0_cp) ) then
                   z=1.0_cp/z
                   c=f*z
                   s=h*z
                end if
                f=(c*g)+(s*y)
                x=-(s*g)+(c*y)
                tempm(1:m)=a(1:m,j)
                a(1:m,j)=a(1:m,j)*c+a(1:m,i)*s
                a(1:m,i)=-tempm(1:m)*s+a(1:m,i)*c
             end do
             rv1(l)=0.0_cp
             rv1(k)=f
             w(k)=x
          end do
       end do

    End Subroutine Svdcmp

End Submodule Maths_Rank
