!!----
!!----
!!----
SubModule (CFML_FFT) FFT_Convol
  implicit none
  Contains

    !!----
    !!---- FUNCTION F_FFT
    !!----
    !!----    This Function is a slight modification of a complex split radix FFT routine presented
    !!----    by C.S. Burrus. There is no control of the error consisting in giving a dimension that
    !!----    is not a power of two. It is the responsibility of the user to provide a complex
    !!----    array of dimension equal to a power of 2.
    !!----    The function is similar to subroutine SFFT and it is useful only when
    !!----    one is interested in conserving the original array.
    !!----    Example of use:
    !!----
    !!----    FX = F_fft(X)
    !!----     Y = F_fft(FY,"INV")
    !!----
    !!---- 14/04/2019
    !!
    Pure Module Function F_FFT(Array, Mode ) Result(fft)
       !---- Arguments ----!
       complex(kind=cp), dimension(:),      intent(in) :: Array  !  In -> real array containing real parts of transform
       character(len=*),          optional, intent(in) :: Mode   !  In -> type="INV" : backward transform. Absent or whatever -> forward transform
       complex(kind=cp), dimension(size(Array))        :: fft

       !---- Local variables ----!
       integer                               :: i, j, k, n, m, n1, n2, n4, is, id, i0, i1, i2, i3
       real(kind=cp)                         :: r1, r2, s1, s2, s3, xt
       real(kind=cp)                         :: e, a, a3, cc1, ss1, cc3, ss3
       real(kind=cp), parameter              :: twopi=6.2831853071795864769
       real(kind=cp), dimension(size(Array)) :: x
       real(kind=cp), dimension(size(Array)) :: y

       n=size(Array)
       m=0
       do i=1,20
          if (2**i /= n) cycle
          m=i
          exit
       end do

       do i=1,n
          x(i)=real(Array(i),kind=cp)
          y(i)=aimag(Array(i))
       end do
       if(present(Mode)) then
        if (Mode == "INV") y=-y
       end if

       n2 = 2 * n
       do k = 1, m-1
          n2 = n2 / 2
          n4 = n2 / 4
          e = twopi / n2
          a = 0.0
          do j = 1, n4
             a3 = 3 * a
             cc1 = cos( a )
             ss1 = sin( a )
             cc3 = cos( a3 )
             ss3 = sin( a3 )
             a = j * e
             is = j
             id = 2 * n2
             do
                do i0 = is, n-1, id
                   i1 = i0 + n4
                   i2 = i1 + n4
                   i3 = i2 + n4
                   r1 = x(i0) - x(i2)
                   x(i0) = x(i0) + x(i2)
                   r2 = x(i1) - x(i3)
                   x(i1) = x(i1) + x(i3)
                   s1 = y(i0) - y(i2)
                   y(i0) = y(i0) + y(i2)
                   s2 = y(i1) - y(i3)
                   y(i1) = y(i1) + y(i3)
                   s3 = r1 - s2
                   r1 = r1 + s2
                   s2 = r2 - s1
                   r2 = r2 + s1
                   x(i2) = r1 * cc1 - s2 * ss1
                   y(i2) = - s2 * cc1 - r1 * ss1
                   x(i3) = s3 * cc3 + r2 * ss3
                   y(i3) = r2 * cc3 - s3 * ss3
                end do
                is = 2 * id - n2 + j
                id = 4 * id
                if (is >= n) exit
             end do
          end do
       end do

       !- LAST STAGE, LENGTH-2 BUTTERFLY
       is = 1
       id = 4
       do
          do i0 = is, n, id
             i1 = i0 + 1
             r1 = x(i0)
             x(i0) = r1 + x(i1)
             x(i1) = r1 - x(i1)
             r1 = y(i0)
             y(i0) = r1 + y(i1)
             y(i1) = r1 - y(i1)
          end do
          is = 2 * id - 1
          id = 4 * id
          if (is >= n) exit
       end do

       ! BIT REVERSE COUNTER
       j = 1
       n1 = n - 1
       do i = 1, n1
          if (i < j) then
             xt = x(j)
             x(j) = x(i)
             x(i) = xt
             xt = y(j)
             y(j) = y(i)
             y(i) = xt
          end if
          k = n / 2

          do
             if (k >=j) exit
             j = j - k
             k = k / 2
          end do
          j = j + k
       end do

       if(present(Mode)) then
        if (Mode == "INV") then
          x=x/n
          y=-y/n
        end if
       end if

       do i=1,n
          fft(i)=cmplx(x(i),y(i))
       end do

    End Function F_FFT

    !!----
    !!---- HFFT
    !!----
    !!----    Performs discrete complex fourier transforms on a complex three dimensional array.
    !!----    This subroutine is to be used for complex, 3-dimensional arrays in which each
    !!----    dimension is a power of 2. The maximum m(i) must not be less than 3 or greater than 20,
    !!----    i = 1,2,3
    !!----
    !!--..    Translation:
    !!--..               Unknown author
    !!--..               (http://risc2.numis.nwu.edu/fft/transforms/harm.f)
    !!--..               Translation to F by Javier Gonzalez-Platas
    !!--<<
    !!----  Method
    !!----
    !!----     For IFSET = -1, or -2, the fourier transform of complex
    !!----     array a is obtained.
    !!----
    !!----                  N1-1   N2-1   N3-1                L1   L2   L3
    !!----     X(J1,J2,J3)=SUM    SUM    SUM    A(K1,K2,K3)*W1  *W2  *W3
    !!----                  K1=0   K2=0   K3=0
    !!----
    !!----     where wi is the n(i) root of unity and L1=K1*J1,L2=K2*J2, L3=K3*J3
    !!----
    !!----
    !!----     For IFSET = +1, or +2, the inverse fourier transform a of
    !!----     complex array x is obtained.
    !!----
    !!----     A(K1,K2,K3)=
    !!----
    !!----               1      N1-1   N2-1   N3-1                -L1  -L2  -L3
    !!----           -------- *SUM    SUM    SUM    X(J1,J2,J3)*W1  *W2  *W3
    !!----           N1*N2*N3   J1=0   J2=0   J3=0
    !!-->>
    !!--..
    !!--..  Reference
    !!--..
    !!--..     See J.W. COOLEY and J.W. TUKEY, "an algorithm for the
    !!--..     machine calculation of complex fourier series",
    !!--..     Mathematics of Computations, Vol. 19 (apr. 1965), p. 297.
    !!--..
    !!--..
    !!---- 14/04/2019
    !!
    Pure Module Subroutine Hfft(Array,Ifset,Iferr)
       !---- Arguments ----!
       complex(kind=cp), dimension(0:,0:,0:), intent(in out) :: Array       ! In -> Contains the complex 3D array to be transformed
       integer,                               intent(in    ) :: IfSet       ! In -> 1,2 Inverse Fourier Transform; -1,-2 Fourier Transform
       integer,                               intent(   out) :: IFerr       ! Out -> Flags to error. 0 No error

       !---- Local variables ----!
       integer          :: n1,n2,n3,m1,m2,m3,k1,k2,k3,nx,fn,nnn
       integer          :: i,j,k,l,ii,jj,i2,i3, jj1,jj2,jj3,ip1,ip2,ip3, jp1,jp2,jp3
       integer          :: mm,mt,mtt,nt,ntv2,jstep,jstep2,idif,jdif,jlast,mtlexp,lm1exp
       Integer          :: kbit,mev,klast,jjdif,ilast,lfirst
       integer          :: jc,jc1,jd,id,il,il1,mi,kl
       integer          :: ntsq,n3vnt,n2vnt,n1vnt
       integer          :: ipp1,ipp2,ipp3,jpp1,jpp2,jpp3,jjd1,jjd2,jjd3,igo1,igo2,igo3
       integer          :: m1mt,m2mt,m3mt,minn1,minn2,minn3,ntvn1,ntvn2,ntvn3
       integer, dimension(3)              :: m,ngr,np
       integer, dimension(:), allocatable :: inv

       real(kind=cp)                               :: t,r,theta,root2,awr,awi
       real(kind=cp)                               :: ak0_0,ak0_1,ak1_0,ak1_1,ak2_0,ak2_1,ak3_0,ak3_1
       real(kind=cp), dimension(2*size(Array))     :: a
       real(kind=cp), dimension(2)                 :: w, w2, w3
       real(kind=cp), dimension(:), allocatable    :: s

       !---- Init ----!
       iferr = 0
       ngr(1)=size(Array,1)
       ngr(2)=size(Array,2)
       ngr(3)=size(Array,3)

       m=0
       do i=3,20
          j=2**i
          if (ngr(1) == j) m(1)=i
          if (ngr(2) == j) m(2)=i
          if (ngr(3) == j) m(3)=i
          if (all(m /= 0) ) exit
       end do
       if (any(m ==0)) then
          iferr=1
          return
       end if
       mm=maxval(ngr)/4

       allocate(inv(mm))
       allocate(s(mm))

       do i=0,ngr(1)-1
          do j=0,ngr(2)-1
             do k=0,ngr(3)-1
                ii=2*(k*ngr(1)*ngr(2) + j*ngr(1) + i) + 1
                a(ii)=real(Array(i,j,k),kind=cp)
                a(ii+1)=aimag(Array(i,j,k))
             end do
          end do
       end do

       !----
       !---- The following are parameters which may be overwritten.
       !---- The original coding in fact assumed that these variables
       !---- would be retained
       mt    =maxval(m)-2
       mt    =max(2,mt)
       if (mt >= 20) then
          iferr=1
          return
       end if

       nt=2**mt

       if (abs(ifset) <= 1) then
          !---- Computing the sin and inv tables. ----!
          ntv2=nt/2

          !---- SET UP SIN TABLE ----!
          Theta=asin(1.0)/2.0_cp

          !---- JSTEP=2**(MT-L+1) FOR L=1 ----!
          jstep=nt

          !---- JDIF=2**(MT-L) FOR L=1 ----!
          jdif=ntv2
          s(jdif)=sin(theta)
          do l=2,mt
             theta=theta/2.0_cp
             jstep2=jstep
             jstep=jdif
             jdif=jstep/2
             s(jdif)=sin(theta)
             jc1=nt-jdif
             s(jc1)=cos(theta)
             jlast=nt-jstep2
             if (jlast < jstep) cycle
             do j=jstep,jlast,jstep
                jc=nt-j
                jd=j+jdif
                s(jd)=s(j)*s(jc1)+s(jdif)*s(jc)
             end do
          end do

          !---- SET UP INV(J) TABLE ----!
          mtlexp=ntv2

          !---- MTLEXP=2**(MT-L). FOR L=1
          lm1exp=1

          !---- LM1EXP=2**(L-1). FOR L=1
          inv(1)=0
          do l=1,mt
             inv(lm1exp+1) = mtlexp
             do j=2,lm1exp
                jj=j+lm1exp
                inv(jj)=inv(j)+mtlexp
             end do
             mtlexp=mtlexp/2
             lm1exp=lm1exp*2
          end do

          if (ifset == 0) return
       end if

       mtt=maxval(m)-2
       root2=sqrt(2.0_cp)
       if (mtt > mt) then
          iferr=1
          return
       end if

       m1=m(1)
       m2=m(2)
       m3=m(3)
       n1=2**m1
       n2=2**m2
       n3=2**m3

       if (ifset < 0) then
          nx= n1*n2*n3
          fn = nx
          !---- may be faster than * ----!
          nnn=nx*2
          do i = 1,nnn,2
             a(i) = a(i)/fn
             a(i+1) = -a(i+1)/fn
          end do
       end if

       np(1)= n1*2
       np(2)= np(1)*n2
       np(3)= np(2)*n3

       do id=1,3
          il = np(3)-np(id)
          il1 = il+1
          mi = m(id)
          if (mi <=0) cycle
          idif=np(id)
          kbit=np(id)
          mev = 2*(mi/2)

          if (mi > mev) then
             !---- m is odd. do l=1 case
             kbit=kbit/2
             kl=kbit-2
             do i=1,il1,idif
                klast=kl+i
                do k=i,klast,2
                   k1=k+kbit
                   ak0_0=a(k)
                   ak0_1=a(k+1)
                   ak1_0=a(k1)
                   ak1_1=a(k1+1)
                   a(k)=ak0_0+ak1_0
                   a(k+1)=ak0_1+ak1_1
                   a(k1)=ak0_0-ak1_0
                   a(k1+1)=ak0_1-ak1_1
                end do
             end do

             if (mi > 1) then
                lfirst=3
                jlast=1
             else
                cycle
             end if
          else
             !---- m is even ----!
             lfirst = 2
             jlast=0
          end if

          do l=lfirst,mi,2
             jjdif=kbit
             kbit=kbit/4
             kl=kbit-2

             do i=1,il1,idif
                klast=i+kl
                do k=i,klast,2
                   k1=k+kbit
                   k2=k1+kbit
                   k3=k2+kbit
                   ak0_0=a(k)
                   ak0_1=a(k+1)
                   ak1_0=a(k1)
                   ak1_1=a(k1+1)
                   ak2_0=a(k2)
                   ak2_1=a(k2+1)
                   ak3_0=a(k3)
                   ak3_1=a(k3+1)

                   t=ak2_0
                   ak2_0=ak0_0-t
                   ak0_0=ak0_0+t
                   t=ak2_1
                   ak2_1=ak0_1-t
                   ak0_1=ak0_1+t

                   t=ak3_0
                   ak3_0=ak1_0-t
                   ak1_0=ak1_0+t
                   t=ak3_1
                   ak3_1=ak1_1-t
                   ak1_1=ak1_1+t
                   a(k)=ak0_0+ak1_0
                   a(k+1)=ak0_1+ak1_1
                   a(k1)=ak0_0-ak1_0
                   a(k1+1)=ak0_1-ak1_1
                   a(k2)=ak2_0-ak3_1
                   a(k2+1)=ak2_1+ak3_0
                   a(k3)=ak2_0+ak3_1
                   a(k3+1)=ak2_1-ak3_0
                end do
             end do

             if (jlast > 0) then
                jj=jjdif   +1
                ilast= il +jj
                do i = jj,ilast,idif
                   klast = kl+i
                   do k=i,klast,2
                      k1 = k+kbit
                      k2 = k1+kbit
                      k3 = k2+kbit
                      ak0_0=a(k)
                      ak0_1=a(k+1)
                      ak1_0=a(k1)
                      ak1_1=a(k1+1)
                      ak2_0=a(k2)
                      ak2_1=a(k2+1)
                      ak3_0=a(k3)
                      ak3_1=a(k3+1)
                      r =-ak2_1
                      t = ak2_0
                      ak2_0 = ak0_0-r
                      ak0_0 = ak0_0+r
                      ak2_1=ak0_1-t
                      ak0_1=ak0_1+t

                      awr=ak1_0-ak1_1
                      awi = ak1_1+ak1_0
                      r=-ak3_0-ak3_1
                      t=ak3_0-ak3_1
                      ak3_0=(awr-r)/root2
                      ak3_1=(awi-t)/root2
                      ak1_0=(awr+r)/root2
                      ak1_1=(awi+t)/root2
                      a(k)=ak0_0+ak1_0
                      a(k+1)=ak0_1+ak1_1
                      a(k1)=ak0_0-ak1_0
                      a(k1+1)=ak0_1-ak1_1
                      a(k2)=ak2_0-ak3_1
                      a(k2+1)=ak2_1+ak3_0
                      a(k3)=ak2_0+ak3_1
                      a(k3+1)=ak2_1-ak3_0
                   end do
                end do

                if (jlast-1 > 0) then
                   jj= jj + jjdif
                   do j=2,jlast
                      i=inv(j+1)
                      w(1)=s(nt-i)
                      w(2)=s(i)
                      k=i+i
                      k1=nt-k

                      select case (k1)
                         case (:-1)
                            !---- 2*i is in second quadrant ----!
                            k2 = k1+nt
                            k1=-k1
                            w2(1)=-s(k1)
                            w2(2)=s(k2)
                         case (0)
                            w2(1)=0.0
                            w2(2)=1.0
                         case (1:)
                            !---- 2*i is in first quadrant ----!
                            w2(1)=s(k1)
                            w2(2)=s(k)
                      end select

                      k1=i+k
                      k2=nt-k1

                      select case (k2)
                         case (:-1)
                            k=k2+nt
                            select case (k)
                               case (:-1)
                                  !---- 3*i in third quadrant ----!
                                  k1=nt+k
                                  k = -k
                                  w3(1)=-s(k1)
                                  w3(2)=-s(k)
                               case (0)
                                  w3(1)=-1.0_cp
                                  w3(2)=0.0_cp
                               case (1:)
                                  !---- i3 in second quadrant ----!
                                  k2=-k2
                                  w3(1)=-s(k2)
                                  w3(2)=s(k)
                            end select
                         case (0)
                            w3(1)=0.0_cp
                            w3(2)=1.0_cp
                         case (1:)
                            !---- i3 in first quadrant ----!
                            w3(1)=s(k2)
                            w3(2)=s(k1)
                      end select

                      ilast=il+jj
                      do i=jj,ilast,idif
                         klast=kl+i
                         do k=i,klast,2
                            k1=k+kbit
                            k2=k1+kbit
                            ak0_0=a(k)
                            ak0_1=a(k+1)
                            ak1_0=a(k1)
                            ak1_1=a(k1+1)
                            ak2_0=a(k2)
                            ak2_1=a(k2+1)
                            ak3_0=a(k2+kbit)
                            ak3_1=a(k2+kbit+1)
                            r=ak2_0*w2(1)-ak2_1*w2(2)
                            t=ak2_0*w2(2)+ak2_1*w2(1)
                            ak2_0=ak0_0-r
                            ak0_0=ak0_0+r
                            ak2_1=ak0_1-t
                            ak0_1=ak0_1+t

                            r=ak3_0*w3(1)-ak3_1*w3(2)
                            t=ak3_0*w3(2)+ak3_1*w3(1)
                            awr=ak1_0*w(1)-ak1_1*w(2)
                            awi=ak1_0*w(2)+ak1_1*w(1)
                            ak3_0=awr-r
                            ak3_1=awi-t
                            ak1_0=awr+r
                            ak1_1=awi+t
                            a(k)=ak0_0+ak1_0
                            a(k+1)=ak0_1+ak1_1
                            a(k1)=ak0_0-ak1_0
                            a(k1+1)=ak0_1-ak1_1
                            a(k2)=ak2_0-ak3_1
                            a(k2+1)=ak2_1+ak3_0
                            a(k2+kbit)=ak2_0+ak3_1
                            a(k2+kbit+1)=ak2_1-ak3_0
                         end do
                      end do
                      jj=jjdif+jj
                   end do ! j
                end if
             end if
             jlast=4*jlast+3

          end do ! end l
       end do ! id

       !---- We now have the complex fourier sums but their addresses are
       !---- bit-reversed. The following routine puts them in order.
       ntsq=nt*nt
       m3mt=m3-mt

       if (m3mt >=0) then
          !---- m3 gr. or eq. mt ----!
          igo3=1
          n3vnt=n3/nt
          minn3=nt
       else
           !---- m3 less than mt ----!
          igo3=2
          n3vnt=1
          ntvn3=nt/n3
          minn3=n3
       end if
       jjd3 = ntsq/n3
       m2mt=m2-mt

       if (m2mt >=0) then
          !---- m2 gr. or eq. mt ----!
          igo2=1
          n2vnt=n2/nt
          minn2=nt
       else
          !---- m2 less than mt ----!
          igo2 = 2
          n2vnt=1
          ntvn2=nt/n2
          minn2=n2
       end if
       jjd2=ntsq/n2
       m1mt=m1-mt

       if (m1mt >= 0) then
          !---- m1 gr. or eq. mt ----!
          igo1=1
          n1vnt=n1/nt
          minn1=nt
       else
          !---- m1 less than mt ----!
          igo1=2
          n1vnt=1
          ntvn1=nt/n1
          minn1=n1
       end if

       jjd1=ntsq/n1
       jj3=1
       j=1

       do jpp3=1,n3vnt
          ipp3=inv(jj3)
          do jp3=1,minn3
             select case (igo3)
                case (1)
                   ip3=inv(jp3)*n3vnt
                case (2)
                   ip3=inv(jp3)/ntvn3
             end select
             i3=(ipp3+ip3)*n2
             jj2=1
             do jpp2=1,n2vnt
                ipp2=inv(jj2)+i3
                do jp2=1,minn2
                   select case (igo2)
                      case (1)
                         ip2=inv(jp2)*n2vnt
                      case (2)
                         ip2=inv(jp2)/ntvn2
                   end select
                   i2=(ipp2+ip2)*n1
                   jj1=1
                   do jpp1=1,n1vnt
                      ipp1=inv(jj1)+i2
                      do jp1=1,minn1
                         select case (igo1)
                            case (1)
                               ip1=inv(jp1)*n1vnt
                            case (2)
                               ip1=inv(jp1)/ntvn1
                         end select
                         i=2*(ipp1+ip1)+1
                         if (j < i) then
                            t=a(i)
                            a(i)=a(j)
                            a(j)=t
                            t=a(i+1)
                            a(i+1)=a(j+1)
                            a(j+1)=t
                         end if
                         j=j+2
                      end do
                      jj1=jj1+jjd1
                   end do !jpp1
                end do ! jp2
                jj2=jj2+jjd2
             end do ! jpp2
          end do ! jp3
          jj3 = jj3+jjd3
       end do ! jpp3

       if (ifset < 0) then
          nnn=nx*2
          do i=2,nnn,2
             a(i)=-a(i)
          end do
       end if

       do i=0,ngr(1)-1
          do j=0,ngr(2)-1
             do k=0,ngr(3)-1
                ii=2*(k*ngr(1)*ngr(2) + j*ngr(1) + i) + 1
                Array(i,j,k)=cmplx(a(ii),a(ii+1))
             end do
          end do
       end do

    End Subroutine HFFT

    !!----
    !!---- SFFT
    !!----
    !!----    This routine is a slight modification of a complex split radix FFT routine presented by C.S. Burrus.
    !!--..    The original program header is shown below.
    !!--<<
    !!----    The forward transform computes
    !!----        X(k) = sum_{j=0}^{N-1} x(j)*exp(-2ijk*pi/N)
    !!----
    !!----    The backward transform computes
    !!----        x(j) = (1/N) * sum_{k=0}^{N-1} X(k)*exp(2ijk*pi/N)
    !!-->>
    !!--.. Authors:
    !!--..
    !!--..      Steve Kifowit, 9 July 1997
    !!--..      Traslation to Fortran90 by Javier Gonzalez-Platas
    !!--..
    !!--.. Refrences:
    !!--..
    !!--..  A Duhamel-Hollman Split-Radix DIF FFT
    !!--..  Electronics Letters, January 5, 1984
    !!----
    !!---- Update: 11/07/2015
    !!
    Pure Module Subroutine Sfft( Array, Mode,Iferr )
       !---- Arguments ----!
       complex(kind=cp), dimension(:),      intent(in out) :: Array   ! In -> real array containing real parts of transform
       character(len=*), optional,          intent(in)     :: Mode    ! In -> type="INV" : backward transform; Absent or whatever : forward transform
       integer,          optional,          intent(   out) :: Iferr   ! Out -> Flags to error. 0 No error

       !---- Local variables ----!
       integer                            :: i, j, k, n, m, n1, n2, n4, is, id, i0, i1, i2, i3
       real(kind=cp)                      :: r1, r2, s1, s2, s3, xt
       real(kind=cp)                      :: e, a, a3, cc1, ss1, cc3, ss3
       real(kind=cp), parameter           :: twopi=6.2831853071795864769
       real(kind=cp), dimension(size(Array)) :: x
       real(kind=cp), dimension(size(Array)) :: y

       n=size(Array)
       m=0
       do i=1,20
          if (2**i /= n) cycle
          m=i
          exit
       end do

       if(present(iferr)) then
         iferr=0
         if (n == 1 .or. m ==0) then
            iferr=1
            return
         end if
       end if

       do i=1,n
          x(i)=real(Array(i),kind=cp)
          y(i)=aimag(Array(i))
       end do

       if(present(Mode)) then
         if (Mode == "INV") y=-y
       end if

       n2 = 2 * n
       do k = 1, m-1
          n2 = n2 / 2
          n4 = n2 / 4
          e = twopi / n2
          a = 0.0
          do j = 1, n4
             a3 = 3 * a
             cc1 = cos( a )
             ss1 = sin( a )
             cc3 = cos( a3 )
             ss3 = sin( a3 )
             a = j * e
             is = j
             id = 2 * n2
             do
                do i0 = is, n-1, id
                   i1 = i0 + n4
                   i2 = i1 + n4
                   i3 = i2 + n4
                   r1 = x(i0) - x(i2)
                   x(i0) = x(i0) + x(i2)
                   r2 = x(i1) - x(i3)
                   x(i1) = x(i1) + x(i3)
                   s1 = y(i0) - y(i2)
                   y(i0) = y(i0) + y(i2)
                   s2 = y(i1) - y(i3)
                   y(i1) = y(i1) + y(i3)
                   s3 = r1 - s2
                   r1 = r1 + s2
                   s2 = r2 - s1
                   r2 = r2 + s1
                   x(i2) = r1 * cc1 - s2 * ss1
                   y(i2) = - s2 * cc1 - r1 * ss1
                   x(i3) = s3 * cc3 + r2 * ss3
                   y(i3) = r2 * cc3 - s3 * ss3
                end do
                is = 2 * id - n2 + j
                id = 4 * id
                if (is >= n) exit
             end do
          end do
       end do

       ! LAST STAGE, LENGTH-2 BUTTERFLY
       is = 1
       id = 4
       do
          do i0 = is, n, id
             i1 = i0 + 1
             r1 = x(i0)
             x(i0) = r1 + x(i1)
             x(i1) = r1 - x(i1)
             r1 = y(i0)
             y(i0) = r1 + y(i1)
             y(i1) = r1 - y(i1)
          end do
          is = 2 * id - 1
          id = 4 * id
          if (is >= n) exit
       end do

       ! BIT REVERSE COUNTER
       j = 1
       n1 = n - 1
       do i = 1, n1
          if (i < j) then
             xt = x(j)
             x(j) = x(i)
             x(i) = xt
             xt = y(j)
             y(j) = y(i)
             y(i) = xt
          end if
          k = n / 2

          do
             if (k >=j) exit
             j = j - k
             k = k / 2
          end do
          j = j + k
       end do

       if(present(Mode)) then
         if (Mode == "INV") then
          x=x/n
          y=-y/n
         end if
       end if

       do i=1,n
          Array(i)=cmplx(x(i),y(i))
       end do

    End Subroutine SFFT

 End SubModule FFT_Convol
