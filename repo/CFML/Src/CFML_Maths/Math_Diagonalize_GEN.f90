
 Submodule (CFML_Maths) Maths_Diagonalize_GEN
 implicit none
   contains

    !!----
    !!----  Module Subroutine Diagonalize_RGen( n,a,wr,wi,matz,z,ierr)
    !!----  integer,                         intent(in)    :: n
    !!----  real(kind = dp), dimension(n,n), intent(in out):: a
    !!----  real(kind = dp), dimension(n),   intent(out)   :: wi, wr
    !!----  logical,                         intent(in)    :: matz
    !!----  real(kind = dp), dimension(n,n), intent(out)   :: z
    !!----  integer,                         intent(out)   :: ierr
    !!----
    !!---- Renamed RG_ORT to Diagonalize_Gen, computes eigenvalues and eigenvectors
    !!---- of a real general matrix.
    !!----
    !!----  Discussion:
    !!----
    !!----    RG_ORT calls EISPACK routines to find the eigenvalues and eigenvectors
    !!----    of a real general matrix, using orthogonal transformations. All the
    !!----    subroutines of EISPACK needed by this Module Subroutine are included in
    !!----    this submodule but they are kept private.
    !!----
    !!----  Modified:
    !!----
    !!----    11 February 2018, 29 November 2019 (JRC), 3 February 2020 (JRC)
    !!----
    !!----  Author:
    !!----
    !!----    John Burkardt
    !!----
    !!----  Arguments:
    !!----
    !!----    Input, integer ::N, the order of the matrix.
    !!----
    !!----    Input/output, real(kind=dp) A(N,N), the real general matrix.  On
    !!----    output, A has been overwritten.
    !!----
    !!----    Input, logical MATZ, is false if only eigenvalues are desired,
    !!----    and true if both eigenvalues and eigenvectors are desired.
    !!----
    !!----    Output, real(kind=dp) WR(N), WI(N), the real and imaginary parts,
    !!----    respectively, of the eigenvalues.  Complex conjugate pairs of eigenvalues
    !!----    appear consecutively with the eigenvalue having the positive imaginary
    !!----    part first.
    !!----
    !!----    Output, real(kind=dp) Z(N,N), contains the real and imaginary parts of
    !!----    the eigenvectors if MATZ is true.  If the J-th eigenvalue is real, the
    !!----    J-th column of Z contains its eigenvector.  If the J-th eigenvalue is
    !!----    complex with positive imaginary part, the J-th and (J+1)-th columns of
    !!----    Z contain the real and imaginary parts of its eigenvector.  The
    !!----    conjugate of this vector is the eigenvector for the conjugate eigenvalue.
    !!----
    !!----    Output, integer ::IERR, an error completion code described in
    !!----    the documentation for HQR and HQR2.  The normal completion code is zero.
    !!----
    Module Subroutine Diagonalize_RGen(n,a,wr,wi,matz,z)
      integer,                         intent(in)    :: n
      real(kind = dp), dimension(n,n), intent(in out):: a
      real(kind = dp), dimension(n),   intent(out)   :: wi, wr
      logical,                         intent(in)    :: matz
      real(kind = dp), dimension(n,n), intent(out)   :: z

      real(kind = dp), dimension(n) :: fv1,ort
      integer:: is1,is2,ierr


      call balanc( n, a, is1, is2, fv1 )
      call orthes( n, is1, is2, a, ort )
      if ( .not. matz ) then
        call hqr( n, is1, is2, a, wr, wi, ierr )
        if ( ierr /= 0 ) then
          ERR_CFML%ierr=ierr
          ERR_CFML%Msg=" Diagonalize_RGen: Error return from HQR"
          return
        end if
      else
        call ortran( n, is1, is2, a, ort, z )
        call hqr2( n, is1, is2, a, wr, wi, z, ierr )
        if ( ierr /= 0 ) then
          ERR_CFML%ierr=ierr
          ERR_CFML%Msg=" Diagonalize_RGen: Error return from HQR2"
          return
        end if
        call balbak ( n, is1, is2, fv1, n, z )
      end if

    End Subroutine Diagonalize_RGen

    !!---- Subroutine balanc ( n, a, low, igh, scal )
    !!----  integer,                      intent(in)     :: n
    !!----  real(kind=dp), dimension(n,n),intent(in out) :: a
    !!----  integer,                      intent(out)    :: low, igh
    !!----  real(kind=dp), dimension(n),  intent(out)    :: scal
    !!----
    !!----  BALANC balances a real matrix before eigenvalue calculations.
    !!----
    !!----  Discussion:
    !!----
    !!----    BALANC balances a real matrix and isolates eigenvalues.
    !!----
    !!----    Suppose that the principal submatrix in rows LOW through IGH
    !!----    has been balanced, that P(J) denotes the index interchanged
    !!----    with J during the permutation step, and that the elements
    !!----    of the diagonal matrix used are denoted by D(I,J).  Then
    !!----
    !!----      scal(J) = P(J),    J = 1,...,LOW-1,
    !!----               = D(J,J),  J = LOW,...,IGH,
    !!----               = P(J)     J = IGH+1,...,N.
    !!----
    !!----    The order in which the interchanges are made is N to IGH+1,
    !!----    then 1 to LOW-1.
    !!----
    !!----    Note that 1 is returned for LOW if IGH is zero formally.
    !!----
    !!----  Modified:
    !!----
    !!----    13 February 2018, 30 November 2019
    !!----
    !!----  Arguments:
    !!----
    !!----    Input, integer ::N, the order of the matrix.
    !!----
    !!----    Input/output, real(kind=dp) A(N,N), the N by N matrix.  On output,
    !!----    the matrix has been balanced.
    !!----
    !!----    Output, integer ::LOW, IGH, indicate that A(I,J) is equal to
    !!----    zero if
    !!----    (1) I is greater than J and
    !!----    (2) J=1,...,LOW-1 or I=IGH+1,...,N.
    !!----
    !!----    Output, real(kind=dp) scal(N), contains information determining the
    !!----    permutations and scaling factors used.
    !!----
    Subroutine balanc ( n, a, low, igh, scal )
      integer,                      intent(in)     :: n
      real(kind=dp), dimension(n,n),intent(in out) :: a
      integer,                      intent(out)    :: low, igh
      real(kind=dp), dimension(n),  intent(out)    :: scal
      !
      real(kind=dp) :: b2,c,f,g,r,s,t,radixx
      logical :: done, noconv, swap
      integer :: i,j,k,l,m

      radixx = 16.0_dp
      b2 = radixx * radixx
      j = 0
      m = 0
      k = 1
      l = n
    !
    !  Search for rows isolating an eigenvalue and push them down.
    !
      done = .false.

      do while ( .not. done )

        do j = l, 1, -1

          swap = .true.

          do i = 1, l
            if ( i /= j ) then
              if ( a(j,i) /= 0.0_dp ) then
                swap = .false.
                exit
              end if
            end if
          end do

          if ( swap ) then

            m = l
            scal(m) = j

            if ( j /= m ) then

              do i = 1, l
                t      = a(i,j)
                a(i,j) = a(i,m)
                a(i,m) = t
              end do

              do i = k, n
                t      = a(j,i)
                a(j,i) = a(m,i)
                a(m,i) = t
              end do

            end if

            if ( l == 1 ) then
              low = k
              igh = l
              return
            end if

            l = l - 1
            if ( l < 1 ) then
              done = .true.
            end if
            exit

          else if ( j == 1 ) then
            done = .true.
            exit
          end if

        end do

      end do
      !
      !  Search for columns isolating an eigenvalue and push them left.
      !
      done = .false.

      do while ( .not. done )

        do j = k, l

          swap = .true.

          do i = k, l
            if ( i /= j ) then
              if ( a(i,j) /= 0.0_dp ) then
                swap = .false.
                exit
              end if
            end if
          end do

          if ( swap ) then

            m = k
            scal(m) = j

            if ( j /= m ) then

              do i = 1, l
                t      = a(i,j)
                a(i,j) = a(i,m)
                a(i,m) = t
              end do

              do i = k, n
                t      = a(j,i)
                a(j,i) = a(m,i)
                a(m,i) = t
              end do

            end if

            k = k + 1
            if ( l < k ) then
              done = .true.
            end if
            exit

          else

            if ( j == l ) then
              done = .true.
              exit
            end if

          end if

        end do

      end do
      !
      !  Balance the submatrix in rows K to L.
      !
      scal(k:l) = 1.0_dp
      !
      !  Iterative loop for norm reduction.
      !
      noconv = .true.

      do while ( noconv )

        noconv = .false.

        do i = k, l

          c = 0.0_dp
          r = 0.0_dp

          do j = k, l
            if ( j /= i ) then
              c = c + abs ( a(j,i) )
              r = r + abs ( a(i,j) )
            end if
          end do
          !
          !  Guard against zero C or R due to underflow.
          !
          if ( c /= 0.0_dp .and. r /= 0.0_dp ) then

            g = r / radixx
            f = 1.0_dp
            s = c + r

            do while ( c < g )
              f = f * radixx
              c = c * b2
            end do

            g = r * radixx

            do while ( g <= c )
              f = f / radixx
              c = c / b2
            end do
            !
            !  Balance.
            !
            if ( ( c + r ) / f < 0.95_dp * s ) then

              g = 1.0_dp / f
              scal(i) = scal(i) * f
              noconv = .true.

              a(i,k:n) = a(i,k:n) * g
              a(1:l,i) = a(1:l,i) * f

            end if

          end if

        end do

      end do

      low = k
      igh = l

    End Subroutine balanc

    !!----    Subroutine orthes ( n, low, igh, a, ort )
    !!----
    !!----  ORTHES transforms a real general matrix to upper Hessenberg form.
    !!----
    !!----  Discussion:
    !!----
    !!----    ORTHES is given a real general matrix, and reduces a submatrix
    !!----    situated in rows and columns LOW through IGH to upper Hessenberg form by
    !!----    orthogonal similarity transformations.
    !!----
    !!----  Modified:
    !!----
    !!----    04 March 2018
    !!----
    !!----  Arguments:
    !!----
    !!----    Input, integer ::N, the order of the matrix.
    !!----
    !!----    Input, integer ::LOW, IGH, are determined by the balancing
    !!----    routine BALANC.  If BALANC has not been used, set LOW = 1 and IGH = N.
    !!----
    !!----    Input/output, real(kind=dp) A(N,N).  On input, the matrix.  On output,
    !!----    the Hessenberg matrix.  Information about the orthogonal transformations
    !!----    used in the reduction is stored in the remaining triangle under the
    !!----    Hessenberg matrix.
    !!----
    !!----    Output, real(kind=dp) ORT(IGH), contains further information about the
    !!----    transformations.
    !!----
    Subroutine orthes ( n, low, igh, a, ort )
      integer,                        intent(in)     :: n, low, igh
      real(kind=dp), dimension(n,n),  intent(in out) :: a
      real(kind=dp), dimension(igh),  intent(out)    :: ort

      real(kind=dp) :: f,g,h,scal
      integer :: i,j,m

      do m = low + 1, igh - 1

        h = 0.0_dp
        ort(m) = 0.0_dp
        scal = 0.0_dp
        !
        !  scal the column.
        !
        do i = m, igh
          scal = scal + abs ( a(i,m-1) )
        end do

        if ( scal /= 0.0_dp ) then

          do i = igh, m, -1
            ort(i) = a(i,m-1) / scal
            h = h + ort(i) * ort(i)
          end do

          g = - sign ( sqrt ( h ), ort(m) )
          h = h - ort(m) * g
          ort(m) = ort(m) - g
          !
          !  Form (I-(U*Ut)/h) * A.
          !
          do j = m, n

            f = 0.0_dp
            do i = igh, m, -1
              f = f + ort(i) * a(i,j)
            end do
            f = f / h

            do i = m, igh
              a(i,j) = a(i,j) - f * ort(i)
            end do

          end do
          !
          !  Form (I-(u*ut)/h) * A * (I-(u*ut)/h).
          !
          do i = 1, igh

            f = 0.0_dp
            do j = igh, m, -1
              f = f + ort(j) * a(i,j)
            end do

            a(i,m:igh) = a(i,m:igh) - f * ort(m:igh) / h

          end do

          ort(m) = scal * ort(m)
          a(m,m-1) = scal * g

        end if

      end do

    End Subroutine orthes

    !!----  Subroutine hqr ( n, low, igh, h, wr, wi, ierr )
    !!----
    !!----  HQR computes all eigenvalues of a real upper Hessenberg matrix.
    !!----
    !!----  Discussion:
    !!----
    !!----    HQR finds the eigenvalues of a real
    !!----    upper Hessenberg matrix by the QR method.
    !!----
    !!----  Modified:
    !!----
    !!----    31 January 2018
    !!----
    !!----  Reference:
    !!----
    !!----    Martin, Peters, James Wilkinson,
    !!----    HQR,
    !!----    Numerische Mathematik,
    !!----    Volume 14, pages 219-231, 1970.
    !!----
    !!----
    !!----  Arguments:
    !!----
    !!----    Input, integer ::N, the order of the matrix.
    !!----
    !!----    Input, integer ::LOW, IGH, two integers determined by
    !!----    BALANC.  If BALANC is not used, set LOW=1, IGH=N.
    !!----
    !!----    Input/output, real(kind=dp) H(N,N), the N by N upper Hessenberg matrix.
    !!----    Information about the transformations used in the reduction to
    !!----    Hessenberg form by ELMHES or ORTHES, if performed, is stored
    !!----    in the remaining triangle under the Hessenberg matrix.
    !!----    On output, the information in H has been destroyed.
    !!----
    !!----    Output, real(kind=dp) WR(N), WI(N), the real and imaginary parts of the
    !!----    eigenvalues.  The eigenvalues are unordered, except that complex
    !!----    conjugate pairs of values appear consecutively, with the eigenvalue
    !!----    having positive imaginary part listed first.  If an error exit
    !!----    occurred, then the eigenvalues should be correct for indices
    !!----    IERR+1 through N.
    !!----
    !!----    Output, integer ::IERR, error flag.
    !!----    0, no error.
    !!----    J, the limit of 30*N iterations was reached while searching for
    !!----      the J-th eigenvalue.
    !!----
    Subroutine hqr ( n, low, igh, h, wr, wi, ierr )
      integer,                      intent(in)     :: n, low, igh
      real(kind=dp), dimension(n,n),intent(in out) :: h
      real(kind=dp), dimension(n),  intent(out)    :: wr, wi
      integer,                      intent(out)    :: ierr

      integer :: en,enm2,i,itn,its,j,k,l,m,na
      logical :: notlas
      real(kind=dp) :: norm,p,q,r,s,t,tst1,tst2,w,x,y,zz

      ierr = 0
      norm = 0.0_dp
      k = 1
      !
      !  Store roots isolated by BALANC and compute matrix norm.
      !
      do i = 1, n

        do j = k, n
          norm = norm + abs ( h(i,j) )
        end do

        k = i
        if ( i < low .or. igh < i ) then
          wr(i) = h(i,i)
          wi(i) = 0.0_dp
        end if

      end do

      en = igh
      t = 0.0_dp
      itn = 30 * n
      !
      !  Search for next eigenvalues.
      !
      if ( igh < low ) return

      its = 0
      na = igh - 1
      enm2 = igh - 2
      !
      !  Look for a single small sub-diagonal element.
      !
      do

        do l = en, low, -1
          if ( l == low ) exit
          s = abs ( h(l-1,l-1) ) + abs ( h(l,l) )
          if ( s == 0.0_dp ) then
            s = norm
          end if
          tst1 = s
          tst2 = tst1 + abs ( h(l,l-1) )
          if ( tst2 == tst1 ) exit
        end do
        !
        !  Form shift.
        !
        x = h(en,en)
        !
        !  One root found.
        !
        if ( l == en ) then
          wr(en) = x + t
          wi(en) = 0.0_dp
          en = na
          if ( en < low ) return
          its = 0
          na = en - 1
          enm2 = na - 1
          cycle
        end if

        y = h(na,na)
        w = h(en,na) * h(na,en)
        !
        !  Two roots found.
        !
        if ( l == na ) then

          p = ( y - x ) / 2.0_dp
          q = p * p + w
          zz = sqrt ( abs ( q ) )
          x = x + t
          !
          !  Real root, or complex pair.
          !
          if ( 0.0_dp <= q ) then

            zz = p + sign ( zz, p )
            wr(na) = x + zz
            if ( zz == 0.0_dp ) then
              wr(en) = wr(na)
            else
              wr(en) = x - w / zz
            end if
            wi(na) = 0.0_dp
            wi(en) = 0.0_dp

          else

            wr(na) = x + p
            wr(en) = x + p
            wi(na) = zz
            wi(en) = - zz

          end if

          en = enm2

          if ( en < low ) then
            return
          end if

          its = 0
          na = en - 1
          enm2 = na - 1
          cycle

        end if

        if ( itn == 0 ) then
          ierr = en
          return
        end if
        !
        !  Form an exceptional shift.
        !
        if ( its == 10 .or. its == 20 ) then

          t = t + x

          do i = low, en
            h(i,i) = h(i,i) - x
          end do

          s = abs ( h(en,na) ) + abs ( h(na,enm2) )
          x = 0.75_dp * s
          y = x
          w = - 0.4375_dp * s * s

        end if

        its = its + 1
        itn = itn - 1
        !
        !  Look for two consecutive small sub-diagonal elements.
        !
        do m = enm2, l, -1

          zz = h(m,m)
          r = x - zz
          s = y - zz
          p = ( r * s - w ) / h(m+1,m) + h(m,m+1)
          q = h(m+1,m+1) - zz - r - s
          r = h(m+2,m+1)
          s = abs ( p ) + abs ( q ) + abs ( r )
          p = p / s
          q = q / s
          r = r / s

          if ( m == l ) exit

          tst1 = abs ( p ) * ( abs ( h(m-1,m-1) ) + abs ( zz ) &
            + abs ( h(m+1,m+1) ) )
          tst2 = tst1 + abs ( h(m,m-1) ) * ( abs ( q ) + abs ( r ) )

          if ( tst2 == tst1 ) exit

        end do

        do i = m + 2, en
          h(i,i-2) = 0.0_dp
          if ( i /= m + 2 ) then
            h(i,i-3) = 0.0_dp
          end if
        end do
        !
        !  Double QR step involving rows l to EN and columns M to EN.
        !
        do k = m, na

          notlas = k /= na

          if ( k /= m ) then

            p = h(k,k-1)
            q = h(k+1,k-1)

            if ( notlas ) then
              r = h(k+2,k-1)
            else
              r = 0.0_dp
            end if

            x = abs ( p ) + abs ( q ) + abs ( r )

            if ( x == 0.0_dp ) cycle

            p = p / x
            q = q / x
            r = r / x

          end if

          s = sign ( sqrt ( p * p + q * q + r * r ), p )

          if ( k /= m ) then
            h(k,k-1) = - s * x
          else if ( l /= m ) then
            h(k,k-1) = - h(k,k-1)
          end if

          p = p + s
          x = p / s
          y = q / s
          zz = r / s
          q = q / p
          r = r / p

          if ( .not. notlas ) then
            !
            !  Row modification.
            !
            do j = k, n
              p = h(k,j) + q * h(k+1,j)
              h(k,j) = h(k,j) - p * x
              h(k+1,j) = h(k+1,j) - p * y
            end do

            j = min ( en, k + 3 )
            !
            !  Column modification.
            !
            do i = 1, j
              p = x * h(i,k) + y * h(i,k+1)
              h(i,k) = h(i,k) - p
              h(i,k+1) = h(i,k+1) - p * q
            end do

          else
            !
            !  Row modification.
            !
            do j = k, n
              p = h(k,j) + q * h(k+1,j) + r * h(k+2,j)
              h(k,j) = h(k,j) - p * x
              h(k+1,j) = h(k+1,j) - p * y
              h(k+2,j) = h(k+2,j) - p * zz
            end do

            j = min ( en, k + 3 )
            !
            !  Column modification.
            !
            do i = 1, j
              p = x * h(i,k) + y * h(i,k+1) + zz * h(i,k+2)
              h(i,k) = h(i,k) - p
              h(i,k+1) = h(i,k+1) - p * q
              h(i,k+2) = h(i,k+2) - p * r
            end do

          end if

        end do

      end do

    End Subroutine hqr

    !!----  Subroutine hqr2 ( n, low, igh, h, wr, wi, z, ierr )
    !!----
    !!----  HQR2 computes eigenvalues and eigenvectors of a real upper Hessenberg matrix.
    !!----
    !!----  Discussion:
    !!----
    !!----    HQR2 finds the eigenvalues and eigenvectors of a real upper Hessenberg
    !!----    matrix by the qr method.
    !!----
    !!----    The eigenvectors of a real general matrix can also be found
    !!----    if ELMHES and ELTRAN or ORTHES and ORTRAN have
    !!----    been used to reduce this general matrix to Hessenberg form
    !!----    and to accumulate the similarity transformations.
    !!----
    !!----   THE STATEMENT BELOW CAME FROM THE ORIGINAL Eispack.f90 code
    !!----    Thanks to David Chichka, 02 May 2019, for pointing out that a previous
    !!----    version of this F90 translation of hqr2 gave erroneous results when some
    !!----    eigenvalues were complex.  I gave up my ideal of a complete rewrite of
    !!----    this function, and did a much lighter conversion of the F77 code
    !!----    for a second try.
    !!----   IN THIS VERSION ALL GOTO's AND NUMERICAL LABELS HAVE BEEN ELIMINATED
    !!----
    !!----  Modified:
    !!----
    !!----    02 May 2019
    !!----
    !!----  Arguments:
    !!----
    !!----    Input, integer ::N, the order of the matrix.
    !!----
    !!----    Input, integer ::LOW, IGH, determined by the balancing
    !!----    routine BALANC.  If BALANC has not been used, set LOW = 1, IGH = N.
    !!----
    !!----    Input/output, real(kind=dp) H(N,N), the N by N upper Hessenberg matrix.
    !!----    On output, the information in H has been destroyed.
    !!----
    !!----    Output, real(kind=dp) WR(N), WI(N), the real and imaginary parts of the
    !!----    eigenvalues.  The eigenvalues are unordered, except that complex
    !!----    conjugate pairs of values appear consecutively, with the eigenvalue
    !!----    having positive imaginary part listed first.  If an error exit
    !!----    occurred, then the eigenvalues should be correct for indices
    !!----    IERR+1 through N.
    !!----
    !!----    Input/output, real(kind=dp) Z(N,N).  On input, the transformation
    !!----    matrix produced by ELTRAN after the reduction by ELMHES, or by ORTRAN after
    !!----    the reduction by ORTHES, if performed.  If the eigenvectors of the
    !!----    Hessenberg matrix are desired, Z must contain the identity matrix.  On
    !!----    output, Z contains the real and imaginary parts of the eigenvectors.
    !!----    If the I-th eigenvalue is real, the I-th column of Z contains its
    !!----    eigenvector.  If the I-th eigenvalue is complex with positive imaginary
    !!----    part, the I-th and (I+1)-th columns of Z contain the real and imaginary
    !!----    parts of its eigenvector.  The eigenvectors are unnormalized.  If an
    !!----    error exit is made, none of the eigenvectors has been found.
    !!----
    !!----    Output, integer ::IERR, error flag.
    !!----    0, for normal return,
    !!----    J, if the limit of 30*N iterations is exhausted while the J-th
    !!----      eigenvalue is being sought.
    !!----

    Subroutine hqr2 ( n, low, igh, h, wr, wi, z, ierr )
      integer,                      intent(in)     :: n, low, igh
      real(kind=dp), dimension(n,n),intent(in out) :: h
      real(kind=dp), dimension(n),  intent(out)    :: wr, wi
      real(kind=dp), dimension(n,n),intent(in out) :: z
      integer,                      intent(out)    :: ierr

      integer :: i,j,k,l,m,en,ii,jj,ll,mm,na,nn
      integer :: itn,its,mp2,enm2
      real(kind=dp) :: p,q,r,s,t,w,x,y,ra,sa,vi,vr,zz,norm,tst1,tst2
      logical :: notlas,two_found !This last logical has been introduced to simplify the code (JRC)

       ierr = 0
       norm = 0.0_dp
       k = 1
       two_found=.false.
       !
       !  store roots isolated by balanc and compute matrix norm
       !
       do i = 1, n

          do j = k, n
            norm = norm + abs(h(i,j))
          end do

          k = i
          if ( i >= low .and. i <= igh ) cycle
          wr(i) = h(i,i)
          wi(i) = 0.0_dp
       end do

       en = igh
       t = 0.0_dp
       itn = 30*n
       !
       !  search for next eigenvalues
       !
       do

          if (en < low) exit
          its = 0
          na = en - 1
          enm2 = na - 1
         !
         !  look for single small sub-diagonal element
         !  for l=en step -1 until low do --
         !
         do

            do ll = low, en
               l = en + low - ll
               if (l == low) exit
               s = abs(h(l-1,l-1)) + abs(h(l,l))
               if (s == 0.0_dp) s = norm
               tst1 = s
               tst2 = tst1 + abs(h(l,l-1))
               if (tst2 == tst1) exit
            end do
            !
            !  form shift
            !
            x = h(en,en)
            if (l == en) then
              two_found=.false.
              exit
            end if
            y = h(na,na)
            w = h(en,na) * h(na,en)
            if (l == na) then
              two_found=.true.
              exit
            end if
            if (itn == 0)  then
              !
              !  set error -- all eigenvalues have not converged after 30*n iterations
              !
              ierr = en
              return
            end if
            if (.not. (its /= 10 .and. its /= 20)) then
              !
              !  form exceptional shift
              !
               t = t + x

               do i = low, en
                 h(i,i) = h(i,i) - x
               end do

               s = abs(h(en,na)) + abs(h(na,enm2))
               x = 0.75_dp * s
               y = x
               w = -0.4375_dp * s * s
            end if
            its = its + 1
            itn = itn - 1
            !
            !  look for two consecutive small sub-diagonal elements.
            !  for m=en-2 step -1 until l do --
            !
            do mm = l, enm2
               m = enm2 + l - mm
               zz = h(m,m)
               r = x - zz
               s = y - zz
               p = (r * s - w) / h(m+1,m) + h(m,m+1)
               q = h(m+1,m+1) - zz - r - s
               r = h(m+2,m+1)
               s = abs(p) + abs(q) + abs(r)
               p = p / s
               q = q / s
               r = r / s
               if (m == l) exit
               tst1 = abs(p)*(abs(h(m-1,m-1)) + abs(zz) + abs(h(m+1,m+1)))
               tst2 = tst1 + abs(h(m,m-1))*(abs(q) + abs(r))
               if (tst2 == tst1) exit
            end do

            mp2 = m + 2

            do i = mp2, en
               h(i,i-2) = 0.0_dp
               if (i == mp2) cycle
               h(i,i-3) = 0.0_dp
            end do
            !
            !  double qr step involving rows l to en and columns m to en.
            !
            do k = m, na

               notlas = k /= na
               if (k /= m) then
                 p = h(k,k-1)
                 q = h(k+1,k-1)
                 r = 0.0_dp
                 if (notlas) r = h(k+2,k-1)
                 x = abs(p) + abs(q) + abs(r)
                 if (x == 0.0_dp) cycle
                 p = p / x
                 q = q / x
                 r = r / x
               end if

               s = sign(sqrt(p*p+q*q+r*r),p)
               if (k == m) then
                 if (l /= m) h(k,k-1) = -h(k,k-1)
               else
                 h(k,k-1) = -s * x
               end if
               p = p + s
               x = p / s
               y = q / s
               zz = r / s
               q = q / p
               r = r / p
               if (notlas) then
                !
                !  row modification
                !
                 do j = k, n
                    p = h(k,j) + q * h(k+1,j) + r * h(k+2,j)
                    h(k,j) = h(k,j) - p * x
                    h(k+1,j) = h(k+1,j) - p * y
                    h(k+2,j) = h(k+2,j) - p * zz
                 end do

                 j = min(en,k+3)
                 !
                 !  column modification
                 !
                 do i = 1, j
                    p = x * h(i,k) + y * h(i,k+1) + zz * h(i,k+2)
                    h(i,k) = h(i,k) - p
                    h(i,k+1) = h(i,k+1) - p * q
                    h(i,k+2) = h(i,k+2) - p * r
                 end do
                 !
                 !  accumulate transformations
                 !
                 do i = low, igh
                    p = x * z(i,k) + y * z(i,k+1) + zz * z(i,k+2)
                    z(i,k) = z(i,k) - p
                    z(i,k+1) = z(i,k+1) - p * q
                    z(i,k+2) = z(i,k+2) - p * r
                 end do

               else
                 !
                 !  row modification.
                 !
                 do j = k, n
                    p = h(k,j) + q * h(k+1,j)
                    h(k,j) = h(k,j) - p * x
                    h(k+1,j) = h(k+1,j) - p * y
                 end do

                 j = min(en,k+3)
                 !
                 !  column modification
                 !
                 do i = 1, j
                    p = x * h(i,k) + y * h(i,k+1)
                    h(i,k) = h(i,k) - p
                    h(i,k+1) = h(i,k+1) - p * q
                 end do
                 !
                 !  accumulate transformations
                 !
                 do i = low, igh
                    p = x * z(i,k) + y * z(i,k+1)
                    z(i,k) = z(i,k) - p
                    z(i,k+1) = z(i,k+1) - p * q
                 end do
              end if

            end do

         end do

         if(.not. two_found) then
            !
            !  one root found
            !
            h(en,en) = x + t
            wr(en) = h(en,en)
            wi(en) = 0.0_dp
            en = na
            cycle
         else
            !
            !  two roots found
            !
            p = (y - x) / 2.0_dp
            q = p * p + w
            zz = sqrt(abs(q))
            h(en,en) = x + t
            x = h(en,en)
            h(na,na) = y + t
         end if

         if (q >= 0.0_dp) then
             !
             !  real pair
             !
             zz = p + sign(zz,p)
             wr(na) = x + zz
             wr(en) = wr(na)
             if (zz /= 0.0_dp) wr(en) = x - w / zz
             wi(na) = 0.0_dp
             wi(en) = 0.0_dp
             x = h(en,na)
             s = abs(x) + abs(zz)
             p = x / s
             q = zz / s
             r = sqrt(p*p+q*q)
             p = p / r
             q = q / r
             !
             !  row modification
             !
             do j = na, n
                zz = h(na,j)
                h(na,j) = q * zz + p * h(en,j)
                h(en,j) = q * h(en,j) - p * zz
             end do
             !
             !  column modification
             !
             do i = 1, en
                zz = h(i,na)
                h(i,na) = q * zz + p * h(i,en)
                h(i,en) = q * h(i,en) - p * zz
             end do
             !
             !  accumulate transformations
             !
             do i = low, igh
                zz = z(i,na)
                z(i,na) = q * zz + p * z(i,en)
                z(i,en) = q * z(i,en) - p * zz
             end do
         else
             !
             !  complex pair
             !
             wr(na) = x + p
             wr(en) = x + p
             wi(na) = zz
             wi(en) = -zz
         end if

         en = enm2
       end do
       !
       !  all roots found.  backsubstitute to find vectors of upper triangular form
       !
       if (norm == 0.0_dp) return
       !
       !  for en=n step -1 until 1 do
       !
          do nn = 1, n  !do 800

             en = n + 1 - nn
             p = wr(en)
             q = wi(en)
             na = en - 1
             if (q > 0.0_dp) cycle
             if (q < 0.0_dp) then
                !
                !  complex vector
                !
                m = na
                !
                !  last vector component chosen imaginary so eigenvector matrix is triangular
                !
                if (abs(h(en,na)) > abs(h(na,en))) then
                  h(na,na) = q / h(en,na)
                  h(na,en) = -(h(en,en) - p) / h(en,na)
                else
                  call cdiv(0.0_dp,-h(na,en),h(na,na)-p,q,h(na,na),h(na,en))
                end if

                h(en,na) = 0.0_dp
                h(en,en) = 1.0_dp
                enm2 = na - 1
                if (enm2 == 0) cycle
                !
                !  for i=en-2 step -1 until 1 do
                !
                do ii = 1, enm2

                   i = na - ii
                   w = h(i,i) - p
                   ra = 0.0_dp
                   sa = 0.0_dp

                   do j = m, en
                      ra = ra + h(i,j) * h(j,na)
                      sa = sa + h(i,j) * h(j,en)
                   end do

                   if (wi(i) < 0.0_dp) then
                      zz = w
                      r = ra
                      s = sa
                      cycle
                   end if

                   m = i
                   if (wi(i) == 0.0_dp) then
                     call cdiv(-ra,-sa,w,q,h(i,na),h(i,en))
                   else
                     !
                     !  solve complex equations
                     !
                     x = h(i,i+1)
                     y = h(i+1,i)
                     vr = (wr(i) - p) * (wr(i) - p) + wi(i) * wi(i) - q * q
                     vi = (wr(i) - p) * 2.0_dp * q
                     if (.not. (vr /= 0.0_dp .or. vi /= 0.0_dp)) then
                        tst1 = norm * (abs(w) + abs(q) + abs(x) + abs(y) + abs(zz))
                        vr = tst1
                     else
                        do
                          vr = 0.01d0 * vr
                          tst2 = tst1 + vr
                          if (tst2 > tst1) cycle
                          exit
                        end do
                     end if

                     call cdiv(x*r-zz*ra+q*sa,x*s-zz*sa-q*ra,vr,vi,h(i,na),h(i,en))
                     if (abs(x) > abs(zz) + abs(q)) then
                       h(i+1,na) = (-ra - w * h(i,na) + q * h(i,en)) / x
                       h(i+1,en) = (-sa - w * h(i,en) - q * h(i,na)) / x
                     else
                       call cdiv(-r-y*h(i,na),-s-y*h(i,en),zz,q,h(i+1,na),h(i+1,en))
                     end if
                   end if
                   !
                   !  overflow control
                   !
                   t = max(abs(h(i,na)), abs(h(i,en)))
                   if (t == 0.0_dp) cycle
                   tst1 = t
                   tst2 = tst1 + 1.0_dp/tst1
                   if (tst2 > tst1) cycle
                   do j = i, en
                      h(j,na) = h(j,na)/t
                      h(j,en) = h(j,en)/t
                   end do

                end do
                !
                !  end complex vector
                !
             else if (q == 0.0_dp) then
               !
               !  real vector
               !
               m = en
               h(en,en) = 1.0_dp
               if (na == 0) cycle
               !
               !  for i=en-1 step -1 until 1 do
               !
               do ii = 1, na

                  i = en - ii
                  w = h(i,i) - p
                  r = 0.0_dp

                  do j = m, en
                    r = r + h(i,j) * h(j,en)
                  end do

                  if (wi(i) < 0.0_dp) then
                     zz = w
                     s = r
                     cycle
                  end if

                  m = i
                  if (wi(i) == 0.0_dp) then
                    t = w
                    if (t == 0.0_dp) then
                       tst1 = norm
                       t = tst1
                       do
                         t = 0.01d0 * t
                         tst2 = norm + t
                         if (tst2 > tst1) cycle
                         exit
                       end do
                    end if
                    h(i,en) = -r / t
                  else
                    !
                    !  solve real equations
                    !
                     x = h(i,i+1)
                     y = h(i+1,i)
                     q = (wr(i) - p) * (wr(i) - p) + wi(i) * wi(i)
                     t = (x * s - zz * r) / q
                     h(i,en) = t
                     if (abs(x) > abs(zz)) then
                     h(i+1,en) = (-r - w * t) / x
                     else
                       h(i+1,en) = (-s - y * t) / zz
                     end if
                  end if
                  !
                  !  overflow control
                  !
                  t = abs(h(i,en))
                  if (t == 0.0_dp) cycle
                  tst1 = t
                  tst2 = tst1 + 1.0_dp/tst1
                  if (tst2 > tst1) cycle
                  do j = i, en
                     h(j,en) = h(j,en)/t
                  end do
               end do
               !
               !  end real vector
               !
             end if
          end do
          !
          !  end back substitution.
          !
          !  vectors of isolated roots
          !
          do i = 1, n
             if (i >= low .and. i <= igh) cycle

             do j = i, n
               z(i,j) = h(i,j)
             end do

          end do
          !
          !  multiply by transformation matrix to give vectors of original full matrix.
          !  for j=n step -1 until low do.
          !
          do jj = low, n
             j = n + low - jj
             m = min(j,igh)

             do i = low, igh
                zz = 0.0_dp
                do k = low, m
                  zz = zz + z(i,k) * h(k,j)
                end do
                z(i,j) = zz
            end do
          end do

    End Subroutine hqr2

    !!----  Subroutine Ortran(n,low,igh,a,ort,z)
    !!----   integer,                          intent(in)     :: n,low,igh
    !!----   real(kind = dp),dimension(n,igh), intent(in)     :: a
    !!----   real(kind = dp),dimension(igh),   intent(in out) :: ort
    !!----   real(kind = dp),dimension(n,n),   intent(out)    :: z
    !!----
    !!----  Subroutine adapted from EisPack, originally called RG_ORT
    !!----  ORTRAN accumulates similarity transformations generated by ORTHES.
    !!----
    !!----  Discussion:
    !!----
    !!----    ORTRAN accumulates the orthogonal similarity transformations used in
    !!----    the reduction of a real general matrix to upper Hessenberg form by ORTHES.
    !!----
    !!----  Modified:
    !!----
    !!----    01 February 2018, 29 November 2019 (JRC)
    !!----
    !!----  Arguments:
    !!----
    !!----    Input, integer ::N, the order of the matrix.
    !!----
    !!----    Input, integer ::LOW, IGH, are determined by the balancing
    !!----    routine BALANC.  If BALANC has not been used, set LOW = 1, IGH = N.
    !!----
    !!----    Input, real(kind=dp) A(N,IGH), contains information about the
    !!----    orthogonal transformations used in the reduction by ORTHES in its strict
    !!----    lower triangle.
    !!----
    !!----    Input/output, real(kind=dp) ORT(IGH), contains further information
    !!----    about the transformations used in the reduction by ORTHES.  On output, ORT
    !!----    has been further altered.
    !!----
    !!----    Output, real(kind=dp) Z(N,N), contains the transformation matrix
    !!----    produced in the reduction by ORTHES.
    !!----
    Subroutine Ortran(n,low,igh,a,ort,z)
      integer,                          intent(in)     :: n,low,igh
      real(kind = dp),dimension(n,igh), intent(in)     :: a
      real(kind = dp),dimension(igh),   intent(in out) :: ort
      real(kind = dp),dimension(n,n),   intent(out)    :: z

      real(kind = dp):: g
      integer :: j,mp
      !
      !  Initialize Z to the identity matrix.
      !
      call r8mat_identity ( n, z )
      if ( igh - low < 2 ) then
        return
      end if
      do mp = igh - 1, low + 1, -1
        if ( a(mp,mp-1) /= 0.0_dp ) then
          ort(mp+1:igh) = a(mp+1:igh,mp-1)
          do j = mp, igh
            g = dot_product ( ort(mp:igh), z(mp:igh,j) )
            g = ( g / ort(mp) ) / a(mp,mp-1)
            z(mp:igh,j) = z(mp:igh,j) + g * ort(mp:igh)
          end do
        end if
      end do

    End Subroutine Ortran

    !!----     Subroutine r8mat_identity ( n, a )
    !!----
    !!----  R8MAT_IDENTITY stores the identity matrix in an R8MAT.
    !!----
    !!----  Discussion:
    !!----
    !!----    An R8MAT is an MxN array of R8's, stored by (I,J) -> [I+J*M].
    !!----
    !!----  Modified:
    !!----
    !!----    24 March 2000
    !!----
    !!----  Author:
    !!----
    !!----    John Burkardt
    !!----
    !!----  Arguments:
    !!----
    !!----    Input, integer ::N, the order of A.
    !!----
    !!----    Output, real(kind=dp) A(N,N), the N by N identity matrix.
    !!----
    Subroutine r8mat_identity ( n, a )
      integer, intent(in) :: n

      real(kind=dp), dimension(n,n), intent(out) :: a
      integer ::i

      a(1:n,1:n) = 0.0_dp
      do i = 1, n
        a(i,i) = 1.0_dp
      end do

    End Subroutine r8mat_identity

    !!---- subroutine balbak ( n, low, igh, scal, m, z )
    !!---- BALBAK determines eigenvectors by undoing the BALANC transformation.
    !!----
    !!----  Discussion:
    !!----
    !!----    BALBAK forms the eigenvectors of a real general matrix by
    !!----    back transforming those of the corresponding balanced matrix
    !!----    determined by BALANC.
    !!----
    !!----  Modified:
    !!----
    !!----    04 March 2018
    !!----
    !!----  Arguments:
    !!----
    !!----    Input, integer ::N, the order of the matrix.
    !!----
    !!----    Input, integer ::LOW, IGH, column indices determined by BALANC.
    !!----
    !!----    Input, real(kind=dp) scal(N), contains information determining
    !!----    the permutations and scaling factors used by BALANC.
    !!----
    !!----    Input, integer ::M, the number of columns of Z to be
    !!----    back-transformed.
    !!----
    !!----    Input/output, real(kind=dp) Z(N,M), contains the real and imaginary
    !!----    parts of the eigenvectors, which, on return, have been back-transformed.
    !!----
    Subroutine balbak ( n, low, igh, scal, m, z )

      integer,                       intent(in) :: n, low, igh,m
      real(kind=dp), dimension(n),   intent(in) :: scal
      real(kind=dp), dimension(n,m), intent(out):: z

      integer       :: i,j,k,ii
      real(kind=dp) :: t

      if ( m <= 0 ) return

      if ( igh /= low ) then
        do i = low, igh
          z(i,1:m) = z(i,1:m) * scal(i)
        end do
      end if

      do ii = 1, n
        i = ii
        if ( i < low .or. igh < i ) then
          if ( i < low ) then
            i = low - ii
          end if
          k = int ( scal(i) )
          if ( k /= i ) then
            do j = 1, m
              t      = z(i,j)
              z(i,j) = z(k,j)
              z(k,j) = t
            end do
          end if
        end if
      end do

    End Subroutine balbak

    !!----  Subroutine cdiv ( ar, ai, br, bi, cr, ci )
    !!----
    !!----  CDIV emulates complex division, using real arithmetic.
    !!----
    !!----  Discussion:
    !!----
    !!----    CDIV performs complex division:
    !!----
    !!----      (CR,CI) = (AR,AI) / (BR,BI)
    !!----
    !!----  Modified:
    !!----
    !!----    18 October 2009
    !!----
    !!----  Arguments:
    !!----
    !!----    Input, real(kind=dp) AR, AI, the real and imaginary parts of
    !!----    the numerator.
    !!----
    !!----    Input, real(kind=dp) BR, BI, the real and imaginary parts of
    !!----    the denominator.
    !!----
    !!----    Output, real(kind=dp) CR, CI, the real and imaginary parts of
    !!----    the result.
    !!----
    Subroutine cdiv ( ar, ai, br, bi, cr, ci )
      real(kind=dp), intent(in)  :: ar, ai, br, bi
      real(kind=dp), intent(out) :: cr, ci

      real(kind=dp) :: ais,ars,bis,brs,s

      s = abs ( br ) + abs ( bi )

      ars = ar / s
      ais = ai / s
      brs = br / s
      bis = bi / s

      s = brs * brs + bis * bis
      cr = ( ars * brs + ais * bis ) / s
      ci = ( ais * brs - ars * bis ) / s

    End Subroutine cdiv

End SubModule Maths_Diagonalize_GEN

