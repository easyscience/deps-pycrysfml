!!----
!!----
!!----
SubModule (CFML_Maps) Maps_Mapping
   implicit none
   Contains

   !!----
   !!---- VPOINT_IN_LINE
   !!----    Function that interpolate the value
   !!----
   !!----      0-----r-----1
   !!----
   !!---- 31/05/2020
   !!
   Pure Module Function VPoint_in_Line(R, X0, X1) Result (X)
      !---- Arguments ----!
      real(kind=cp), intent(in) :: R        ! R is distance between the ends points
      real(kind=cp), intent(in) :: X0       ! Value of the Point 0
      real(kind=cp), intent(in) :: X1       ! Value of the Point 1
      real(kind=cp)             :: X        ! Interpolated value

      x = ( 1.0_cp - r ) * x0 + r * x1

   End Function VPoint_in_Line

   !!----
   !!---- VPOINT_IN_SQUARE
   !!----    Function that interpolate the value on square
   !!----
   !!----
   !!----      01------------11
   !!----       |      .      |
   !!----       |      .      |
   !!----       |.....rs......|
   !!----       |      .      |
   !!----       |      .      |
   !!----      00------------10
   !!----
   !!----    Formula:
   !!----
   !!----      Written in terms of R and S, the map has the form:
   !!----
   !!----        X(R,S) =
   !!----                 1     * ( + x00 )
   !!----               + r     * ( - x00 + x10 )
   !!----               + s     * ( - x00       + x01 )
   !!----               + r * s * ( + x00 - x10 - x01 + x11 )
   !!----
   !!----      Written in terms of the coefficients, the map has the form:
   !!----
   !!----        X(R,S) = x00 * ( 1 - r - s + r * s )
   !!----               + x01 * (         s - r * s )
   !!----               + x10 * (     r     - r * s )
   !!----               + x11 * (             r * s )
   !!----
   !!----               = x00 * ( 1 - r ) * ( 1 - s )
   !!----               + x01 * ( 1 - r ) *       s
   !!----               + x10 *       r   * ( 1 - s )
   !!----               + x11 *       r           s
   !!----
   !!----      The nonlinear term ( r * s ) has an important role:
   !!----
   !!----        If ( x01 + x10 - x00 - x11 ) is zero, then the input data lies in
   !!----        a plane, and the mapping is affine.  All the interpolated data
   !!----        will lie on the plane defined by the four corner values.  In
   !!----        particular, on any line through the square, data values at
   !!----        intermediate points will lie between the values at the endpoints.
   !!----
   !!----        If ( x01 + x10 - x00 - x11 ) is not zero, then the input data does
   !!----        not lie in a plane, and the interpolation map is nonlinear.  On
   !!----        any line through the square, data values at intermediate points
   !!----        may lie above or below the data values at the endpoints.  The
   !!----        size of the coefficient of r * s will determine how severe this
   !!-->>        effect is.
   !!----
   !!--..    Reference:
   !!--..
   !!--..      William Gordon,
   !!--..      Blending-Function Methods of Bivariate and Multivariate Interpolation
   !!--..        and Approximation,
   !!--..      SIAM Journal on Numerical Analysis,
   !!--..      Volume 8, Number 1, March 1971, pages 158-177.
   !!--..
   !!--..      William Gordon and Charles Hall,
   !!--..      Transfinite Element Methods: Blending-Function Interpolation over
   !!--..        Arbitrary Curved Element Domains,
   !!--..      Numerische Mathematik,
   !!--..      Volume 21, Number 1, 1973, pages 109-129.
   !!--..
   !!--..      William Gordon and Charles Hall,
   !!--..      Construction of Curvilinear Coordinate Systems and Application to
   !!--..        Mesh Generation,
   !!--..      International Journal of Numerical Methods in Engineering,
   !!--..      Volume 7, 1973, pages 461-477.
   !!--..
   !!--..      Joe Thompson, Bharat Soni, Nigel Weatherill,
   !!--..      Handbook of Grid Generation,
   !!--..      CRC Press, 1999.
   !!--..
   !!----
   !!---- 31/05/2020
   !!
   Pure Module Function VPoint_in_Square(R, S, X00, X01, X10, X11) Result(x)
      !---- Arguments ----!
      real(kind=cp), intent(in) :: r
      real(kind=cp), intent(in) :: s
      real(kind=cp), intent(in) :: x00
      real(kind=cp), intent(in) :: x01
      real(kind=cp), intent(in) :: x10
      real(kind=cp), intent(in) :: x11
      real(kind=cp)             :: x

      x =             + x00 &
          + r *     ( - x00 + x10 ) &
          + s *     ( - x00       + x01 ) &
          + r * s * ( + x00 - x10 - x01 + x11 )

   End Function VPoint_in_Square

   !!----
   !!---- VPOINT_IN_CUBE
   !!----    Function that interpolate the value into a cube
   !!----
   !!----
   !!----     011--------------111
   !!----       |               |
   !!----       |               |
   !!----       |               |
   !!----       |               |
   !!----       |               |
   !!----     001--------------101
   !!----
   !!----
   !!----       *---------------*
   !!----       |               |
   !!----       |               |
   !!----       |      rst      |
   !!----       |               |
   !!----       |               |
   !!----       *---------------*
   !!----
   !!----
   !!----     010--------------110
   !!----       |               |
   !!----       |               |
   !!----       |               |
   !!----       |               |
   !!----       |               |
   !!----     000--------------100
   !!----
   !!----
   !!----   Formula:
   !!----
   !!----     Written as a polynomial in R, S and T, the interpolation map has the
   !!----     form:
   !!----
   !!----       X(R,S,T) =
   !!----         1         * ( + x000 )
   !!----       + r         * ( - x000 + x100 )
   !!----       +     s     * ( - x000        + x010 )
   !!----       +         t * ( - x000               + x001 )
   !!----       + r * s     * ( + x000 - x100 - x010                       + x110 )
   !!----       + r     * t * ( + x000 - x100        - x001        + x101 )
   !!----       +     s * t * ( + x000        - x010 - x001 + x011 )
   !!----       + r * s * t * ( - x000 + x100 + x010 + x001 - x011 - x101 - x110 + x111 )
   !!-->>
   !!--..   Reference:
   !!--..
   !!--..     William Gordon,
   !!--..     Blending-Function Methods of Bivariate and Multivariate Interpolation
   !!--..       and Approximation,
   !!--..     SIAM Journal on Numerical Analysis,
   !!--..     Volume 8, Number 1, March 1971, pages 158-177.
   !!--..
   !!--..     William Gordon and Charles Hall,
   !!--..     Transfinite Element Methods: Blending-Function Interpolation over
   !!--..       Arbitrary Curved Element Domains,
   !!--..     Numerische Mathematik,
   !!--..     Volume 21, Number 1, 1973, pages 109-129.
   !!--..
   !!--..     William Gordon and Charles Hall,
   !!--..     Construction of Curvilinear Coordinate Systems and Application to
   !!--..       Mesh Generation,
   !!--..     International Journal of Numerical Methods in Engineering,
   !!--..     Volume 7, 1973, pages 461-477.
   !!--..
   !!--..     Joe Thompson, Bharat Soni, Nigel Weatherill,
   !!--..     Handbook of Grid Generation,
   !!--..     CRC Press, 1999.
   !!----
   !!---- 31/05/2020
   !!
   Pure Module Function VPoint_in_Cube(R,S,T,X000,X001,X010,X011,X100,X101,X110,X111) Result(X)
      !---- Arguments ----!
      real(kind=cp), intent(in) :: R
      real(kind=cp), intent(in) :: S
      real(kind=cp), intent(in) :: T
      real(kind=cp), intent(in) :: X000
      real(kind=cp), intent(in) :: X001
      real(kind=cp), intent(in) :: X010
      real(kind=cp), intent(in) :: X011
      real(kind=cp), intent(in) :: X100
      real(kind=cp), intent(in) :: X101
      real(kind=cp), intent(in) :: X110
      real(kind=cp), intent(in) :: X111
      real(kind=cp)             :: X

      x = &
              1.0E+00     * ( + x000 ) &
              + r         * ( - x000 + x100 ) &
              +     s     * ( - x000        + x010 ) &
              +         t * ( - x000               + x001 ) &
              + r * s     * ( + x000 - x100 - x010                      + x110 ) &
              + r     * t * ( + x000 - x100        - x001        + x101 ) &
              +     s * t * ( + x000        - x010 - x001 + x011 ) &
              + r * s * t * ( - x000 + x100 + x010 + x001 - x011 - x101 - x110 + x111 )

   End Function VPoint_in_Cube

   !!--++
   !!--++ XY_SECT
   !!--++
   !!--++  (Private)
   !!--++  Calculates the intersection of lines
   !!--++  Used internally by Calculate_Contour2D
   !!--++
   !!--++ 31/05/2020
   !!
   Pure Module Function xy_sect(p1,p2,h,xy) result(sect)
      !---- Arguments ----!
      integer,                       intent(in) :: p1,p2
      real(kind=cp), dimension(0:4), intent(in) :: h,xy
      real(kind=cp)                             :: sect

      sect= (h(p2)*xy(p1)-h(p1)*xy(p2))/(h(p2)-h(p1))
   End Function xy_sect

   !!--++
   !!--++ PEAK_POSITION
   !!--++    (Private)
   !!--++    Return the position of the peak
   !!--++
   !!--++ 31/05/2020
   !!
   Pure Module Function Peak_Position(nr3d,i,j,k) Result(Pto)
      !---- Arguments ----!
      integer, dimension(:,:,:),intent(in) :: nr3d  ! Density map scaled as integer values
      integer,                  intent(in) :: i
      integer,                  intent(in) :: j     ! (i,j,k) is the central point
      integer,                  intent(in) :: k
      real(kind=cp), dimension(3)          :: pto

      !---- Local variables ----!
      integer       :: ntx,nty,ntz
      integer       :: i1,i2,i3,j1,j2,j3,k1,k2,k3
      real(kind=cp) :: a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19
      real(kind=cp) :: b1,b2,b3,b4,b5,b6,b7,b8,b9,b10,b11,b12,b13,b14,b15,b16,b17,b18,b19
      real(kind=cp) :: b,c,d,h,kk,l,e,f,g,det,deltax,deltay,deltaz,x,y,z,dx,dy,dz

      !---- Calculation of the peak position ----!
      ntx=size(nr3d,1)
      nty=size(nr3d,2)
      ntz=size(nr3d,3)
      dx=1.0_cp/real(ntx)
      dy=1.0_cp/real(nty)
      dz=1.0_cp/real(ntz)
      pto=0.0_cp

      i2=i
      i1=i-1
      i3=i+1
      if (i1 <= 0)  i1=ntx+i1
      if (i3 > ntx) i3=i3-ntx

      j2=j
      j1=j-1
      j3=j+1
      if (j1 <= 0) j1=nty+j1
      if (j3 > nty) j3=j3-nty

      k2=k
      k1=k-1
      k3=k+1
      if (k1 <= 0) k1=ntz+k1
      if (k3 > ntz) k3=k3-ntz

       a1=real( MAX( nr3d(i2,j2,k2),1 ) )
       a2=real( MAX( nr3d(i1,j2,k2),1 ) )
       a3=real( MAX( nr3d(i3,j2,k2),1 ) )
       a4=real( MAX( nr3d(i3,j1,k2),1 ) )
       a5=real( MAX( nr3d(i2,j1,k2),1 ) )
       a6=real( MAX( nr3d(i1,j1,k2),1 ) )
       a7=real( MAX( nr3d(i1,j3,k2),1 ) )
       a8=real( MAX( nr3d(i2,j3,k2),1 ) )
       a9=real( MAX( nr3d(i3,j3,k2),1 ) )
      a10=real( MAX( nr3d(i2,j2,k1),1 ) )
      a11=real( MAX( nr3d(i1,j2,k1),1 ) )
      a12=real( MAX( nr3d(i3,j2,k1),1 ) )
      a13=real( MAX( nr3d(i2,j1,k1),1 ) )
      a14=real( MAX( nr3d(i2,j3,k1),1 ) )
      a15=real( MAX( nr3d(i2,j2,k3),1 ) )
      a16=real( MAX( nr3d(i1,j2,k3),1 ) )
      a17=real( MAX( nr3d(i3,j2,k3),1 ) )
      a18=real( MAX( nr3d(i2,j1,k3),1 ) )
      a19=real( MAX( nr3d(i2,j3,k3),1 ) )

       b1=LOG(a1)
       b2=LOG(a2)
       b3=LOG(a3)
       b4=LOG(a4)
       b5=LOG(a5)
       b6=LOG(a6)
       b7=LOG(a7)
       b8=LOG(a8)
       b9=LOG(a9)
      b10=LOG(a10)
      b11=LOG(a11)
      b12=LOG(a12)
      b13=LOG(a13)
      b14=LOG(a14)
      b15=LOG(a15)
      b16=LOG(a16)
      b17=LOG(a17)
      b18=LOG(a18)
      b19=LOG(a19)

      b=(b3+b4+b9+b12+b17-b2-b7-b6-b11-b16)/10.0
      c=(b7+b8+b9+b14+b19-b4-b5-b6-b13-b18)/10.0
      d=(b15+b16+b17+b18+b19-b10-b11-b13-b12-b14)/10.0
      h=(b13+b19-b18-b14)/4.0
      kk=(b11+b17-b16-b12)/4.0
      l=(b6+b9-b4-b7)/4.0
      e=(-10*b1+b2+b3+5*(b4+b6+b7+b9+b11+b12+b16+b17) &
                                  -6*(b5+b8+b10+b15)-2*(b13+b14+b18+b19))/21.0
      f=(-10*b1+b5+b8-6*(b2+b3+b10+b15)+5*(b4+b6+b7+b9+b13+b14+b18+b19) &
                                                    -2*(b11+b12+b16+b17))/21.0
      g=(-10*b1+b10+b15-6*(b2+b3+b5+b8)+5*(b11+b12+b13+b14+b16+b17+b18+b19) &
                                                        -2*(b4+b6+b7+b9))/21.0

      det=e*f*g+2*h*kk*l-kk*kk*f-h*h*e-l*l*g

      deltax=(-b*g*f-h*l*d-h*c*kk+kk*f*d+h*h*b+c*l*g)/det
      if (abs(deltax)-1.0 <= 0.0) then
         deltay=(-e*c*g-b*h*kk-l*d*kk+kk*kk*c+d*h*e+b*l*g)/det
         if (abs(deltay)-1.0 <= 0.0) then
            deltaz=(-e*f*d-l*c*kk-b*l*h+b*f*kk+l*l*d+h*c*e)/det
            if (abs(deltaz)-1.0 <= 0.0) then
               deltax=deltax*dx
               deltay=deltay*dy
               deltaz=deltaz*dz
            else
               deltax=0.0_cp
               deltay=0.0_cp
               deltaz=0.0_cp
            end if
         else
            deltax=0.0_cp
            deltay=0.0_cp
            deltaz=0.0_cp
         end if
      else
         deltax=0.0_cp
         deltay=0.0_cp
         deltaz=0.0_cp
      end if

      x=(i-1)*dx
      y=(j-1)*dy
      z=(k-1)*dz

      x=x+deltax
      y=y+deltay
      z=z+deltaz

      x=mod(x+10.0_cp,1.0_cp)
      y=mod(y+10.0_cp,1.0_cp)
      z=mod(z+10.0_cp,1.0_cp)

      if (abs(x) <= 0.001_cp) x=0.0_cp
      if (abs(y) <= 0.001_cp) y=0.0_cp
      if (abs(z) <= 0.001_cp) z=0.0_cp
      if (abs(1.0-x) <= 0.001_cp ) x=0.0_cp
      if (abs(1.0-y) <= 0.001_cp ) y=0.0_cp
      if (abs(1.0-z) <= 0.001_cp ) z=0.0_cp

      pto(1)=x
      pto(2)=y
      pto(3)=z

   End Function Peak_Position

   !!----
   !!---- STATISTIC_MAP
   !!----
   !!----    Some statistic parameters of the map
   !!----
   !!---- 31/05/2020
   !!
   Module Subroutine Statistic_Map(Rho,MaxV,MinV,AveV,SigV)
      !---- Arguments ----!
      real(kind=cp), dimension(:,:,:), intent(in) :: Rho         ! Density map
      real(kind=cp), optional,         intent(out):: MaxV        ! Maximum value of Rho
      real(kind=cp), optional,         intent(out):: MinV        ! Minimum value of Rho
      real(kind=cp), optional,         intent(out):: AveV        ! Average value of Rho
      real(kind=cp), optional,         intent(out):: SigV        ! Sigma value of Rho

      !---- Local Variables ----!
      integer :: i,j,k
      integer :: nu,nv,nw
      real(kind=cp) :: v_min,v_max,v_ave,v_sig

      !> Init
      call clear_error()
      if (present(MaxV)) MaxV=0.0_cp
      if (present(MinV)) MinV=0.0_cp
      if (present(AveV)) AveV=0.0_cp
      if (present(SigV)) SigV=0.0_cp

      nu=size(rho,1)
      nv=size(rho,2)
      nw=size(rho,3)
      if (nu*nv*nw == 0) then
         err_CFML%Ierr=1
         err_CFML%Msg="Statistic_Map procedure: Density maps contains a 0 dimension!"
         return
      end if

      V_max  =maxval(rho)
      V_min  =minval(rho)
      V_Ave  = 0.0_cp
      V_Sig  = 0.0_cp

      if (present(MaxV)) MaxV=V_Max
      if (present(MinV)) MinV=V_Min

      if (present(AveV) .or. present(SigV)) then
         do i=1,nu
            do j=1,nv
               do k=1,nw
                  V_ave=V_ave + rho(i,j,k)
                  V_sig= V_sig + rho(i,j,k)*rho(i,j,k)
               end do
            end do
         end do
         V_ave= V_ave/real(nu*nv*nw)
         V_sig= V_sig/real(nu*nv*nw) - V_ave*V_ave
         if (V_sig > tiny(1.0_cp)) then
            V_sig=sqrt(V_sig)
         else
            V_sig=0.0_cp
         end if

         if (present(AveV)) AveV=V_ave
         if (present(SigV)) SigV=V_sig
      end if

   End Subroutine Statistic_Map

   !!----
   !!---- LOAD_SECTION
   !!----    This routine only works with fractional coordinates
   !!----
   !!---- 31/05/2020
   !!
   Pure Module Function Load_Section(Rho,ngrid,imap,section,limits,ngrid2) Result(dmap)
      !---- Arguments ----!
      real(kind=cp), dimension(:,:,:), intent(in)  :: rho
      integer,       dimension(3),     intent(in)  :: ngrid
      integer,                         intent(in)  :: imap
      integer,                         intent(in)  :: section
      real(kind=cp), dimension(2,2),   intent(in)  :: limits
      integer,       dimension(2),     intent(in)  :: ngrid2    ! Dimesions for DMap
      real(kind=cp), dimension(ngrid2(1),ngrid2(2)):: dmap

      !---- Local Variables ----!
      integer :: ndimx, ndimy
      integer :: i,j,ii1,ii2,jj1,jj2,kk

      real(kind=cp) :: uinc,vinc
      real(kind=cp) :: uu,vv,x1,y1
      real(kind=cp) :: dx,dy,f1,f2,f3,z1,z2,z3,z4
      real(kind=cp) :: xx1,xx2,yy1,yy2

      !> Init
      dmap = 0.0_cp

      select case (imap)
         case (1) ! X-Y
            ndimx=ngrid(1)+1
            ndimy=ngrid(2)+1
            dx=1.0_cp/real(ngrid(1))
            dy=1.0_cp/real(ngrid(2))

            kk=mod(section,ngrid(3)+1)
            if (kk == 0) kk=1

         case (2) ! Y-Z
            ndimx=ngrid(2)+1
            ndimy=ngrid(3)+1
            dx=1.0_cp/real(ngrid(2))
            dy=1.0_cp/real(ngrid(3))

            kk=mod(section,ngrid(1)+1)
            if (kk == 0) kk=1

         case (3) ! Z-X
            ndimx=ngrid(3)+1
            ndimy=ngrid(1)+1
            dx=1.0_cp/real(ngrid(3))
            dy=1.0_cp/real(ngrid(1))

            kk=mod(section,ngrid(2)+1)
            if (kk == 0) kk=1
      end select

      uinc=(limits(1,2) - limits(1,1))/real(ngrid2(1)-1)
      vinc=(limits(2,2) - limits(2,1))/real(ngrid2(2)-1)

      do i=1, ngrid2(1)
         uu=limits(1,1)+uinc*(i-1)

         do j=1,ngrid2(2)
            vv=limits(2,1)+vinc*(j-1)

            x1=mod(uu+10.0_cp,1.0_cp)
            y1=mod(vv+10.0_cp,1.0_cp)

            !> Entre que nodos X
            do ii1=1,ndimx-1
               xx1=(ii1-1)*dx
               xx2=xx1+dx
               if (abs(xx1-x1) <= 0.0001) then
                  exit
               else if (x1 > xx1 .and. x1 <= xx2) then
                    exit
               end if
            end do
            ii2=ii1+1
            if (ii2 == ndimx) ii2=1

            !> Entre que nodos Y
            do jj1=1,ndimy-1
               yy1=(jj1-1)*dy
               yy2=yy1+dy
               if (abs(yy1-y1) <= 0.0001) then
                  exit
               else if (y1 > yy1 .and. y1 <= yy2) then
                  exit
               end if
            end do
            jj2=jj1+1
            if (jj2 == ndimy) jj2=1

            select case (imap)
               case (1)
                  z1=rho(ii1,jj1,kk)
                  z2=rho(ii2,jj1,kk)
                  z3=rho(ii1,jj2,kk)
                  z4=rho(ii2,jj2,kk)
               case (2)
                  z1=rho(kk,ii1,jj1)
                  z2=rho(kk,ii2,jj1)
                  z3=rho(kk,ii1,jj2)
                  z4=rho(kk,ii2,jj2)
               case (3)
                  z1=rho(jj1,kk,ii1)
                  z2=rho(jj1,kk,ii2)
                  z3=rho(jj2,kk,ii1)
                  z4=rho(jj2,kk,ii2)
            end select

            f1=( (x1-xx1)/(xx2-xx1) )*(z2-z1) + z1
            f2=( (y1-yy1)/(yy2-yy1) )*(z3-z1)
            f3=( (x1-xx1)/(xx2-xx1) )*( (y1-yy1)/(yy2-yy1) )*(z1-z2-z3+z4)

            dmap(i,j)=dmap(i,j) + f1+f2+f3

         end do
      end do

   End Function Load_Section

   !!----
   !!---- LOAD_EXTENTEDMAP
   !!----
   !!----    Rhonew has one dimension more in each dimension that Rho
   !!----    This routine is useful for 2D representation.
   !!----        Rho(nx,ny,nz) -> Rhonew(nx+1,ny+1,nz+1)
   !!----
   !!---- 31/05/2020
   !!
   Pure Module Function Load_ExtendedMap(Rho,Ngrid,Limits) Result(Rhonew)
      !---- Arguments ----!
      real(kind=cp), dimension(:,:,:), intent(in)               :: rho
      integer,       dimension(3),     intent(in)               :: ngrid
      real(kind=cp), dimension(2,3),   intent(in)               :: limits
      real(kind=cp), dimension(ngrid(1)+1,ngrid(2)+1,ngrid(3)+1):: rhonew

      !---- Local Variables ----!
      integer                     :: nx,ny,nz,nx1,ny1,nz1
      integer                     :: i,j,k
      integer                     :: ii1,ii2,jj1,jj2,kk1,kk2
      real(kind=cp)               :: dx, dy, dz
      real(kind=cp)               :: xval,yval,zval
      real(kind=cp)               :: v1,v2,v3,v4,v5,v6,v7,v8,r,s,t,valor
      real(kind=cp)               :: x1,y1,z1,xx1,xx2,yy1,yy2,zz1,zz2
      real(kind=cp), dimension(3) :: rinc

      !> Init
      Rhonew=0.0_cp

      nx=ngrid(1)
      ny=ngrid(2)
      nz=ngrid(3)
      nx1=nx+1
      ny1=ny+1
      nz1=nz+1

      dx=1.0_cp/real(nx)
      dy=1.0_cp/real(ny)
      dz=1.0_cp/real(nz)

      rinc(1)=abs(limits(2,1)-limits(1,1))*dx
      rinc(2)=abs(limits(2,2)-limits(1,2))*dy
      rinc(3)=abs(limits(2,3)-limits(1,3))*dz

      !> Loading RhoNew from Rho
      if (abs(limits(1,1)-0.0_cp) <= EPS .and. abs(limits(2,1)-1.0_cp) <= EPS .and. &
          abs(limits(1,2)-0.0_cp) <= EPS .and. abs(limits(2,2)-1.0_cp) <= EPS .and. &
          abs(limits(1,3)-0.0_cp) <= EPS .and. abs(limits(2,3)-1.0_cp) <= EPS ) then

          do i=1,nx1
              do j=1,ny1
                 do k=1,nz1

                    ii1=mod(i,nx1)
                    if (ii1 == 0) ii1=1
                    jj1=mod(j,ny1)
                    if (jj1 == 0) jj1=1
                    kk1=mod(k,nz1)
                    if (kk1 == 0) kk1=1

                    rhonew(i,j,k)=rho(ii1,jj1,kk1)

                 end do
              end do
           end do

      else

         do i=1,nx1
            do j=1,ny1
               do k=1,nz1
                  !> Pto equivalente en (0,1)
                  xval=limits(1,1) + (i-1)*rinc(1)
                  yval=limits(1,2) + (j-1)*rinc(2)
                  zval=limits(1,3) + (k-1)*rinc(3)

                  if (abs(xval) <= eps) then
                     xval=0.0_cp
                  elseif (abs(xval-1.0) <= eps) then
                     xval=1.0_cp
                  else
                     if (xval < 0.0) xval=xval+1.0_cp
                     if (xval > 1.0) xval=xval-1.0_cp
                  end if

                  if (abs(yval) <= eps) then
                     yval=0.0_cp
                  elseif (abs(yval-1.0) <= eps) then
                     yval=1.0_cp
                  else
                     if (yval < 0.0) yval=yval+1.0_cp
                     if (yval > 1.0) yval=yval-1.0_cp
                  end if

                  if (abs(zval) <= eps) then
                     zval=0.0_cp
                  elseif (abs(zval-1.0) <= eps) then
                     zval=1.0_cp
                  else
                     if (zval < 0.0) zval=zval+1.0_cp
                     if (zval > 1.0) zval=zval-1.0_cp
                  end if

                  !> Entre que planos de X esta el Pto
                  do ii1=1,nx
                     ii2=ii1+1
                     xx1=(ii1-1)*dx
                     xx2=ii1*dx
                     if (abs(xx1-xval) <= eps) then
                        exit
                     elseif (xval > xx1 .and. xval <= xx2) then
                        exit
                     end if
                  end do

                  !> Entre que planos de Y esta el Pto
                  do jj1=1,ny
                     jj2=jj1+1
                     yy1=(jj1-1)*dy
                     yy2=jj1*dy
                     if (abs(yy1-yval) <= eps) then
                        exit
                     else if (yval > yy1 .and. yval <= yy2) then
                        exit
                     end if
                  end do

                  !> Entre que planos de Z esta el Pto
                  do kk1=1,nz
                     kk2=kk1+1
                     zz1=(kk1-1)*dz
                     zz2=kk1*dz
                     if (abs(zz1-zval) <= eps) then
                        exit
                     else if (zval > zz1 .and. zval <= zz2) then
                        exit
                     end if
                  end do

                  ii1=mod(ii1,nx+1)
                  if (ii1 == 0) ii1=1
                  ii2=mod(ii2,nx+1)
                  if (ii2 == 0) ii2=1

                  jj1=mod(jj1,ny+1)
                  if (jj1 == 0) jj1=1
                  jj2=mod(jj2,ny+1)
                  if (jj2 == 0) jj2=1

                  kk1=mod(kk1,nz+1)
                  if (kk1 == 0) kk1=1
                  kk2=mod(kk2,nz+1)
                  if (kk2 == 0) kk2=1

                  v1=rho(ii1,jj1,kk1)
                  v2=rho(ii2,jj1,kk1)
                  v3=rho(ii2,jj2,kk1)
                  v4=rho(ii1,jj2,kk1)
                  v5=rho(ii1,jj1,kk2)
                  v6=rho(ii2,jj1,kk2)
                  v7=rho(ii2,jj2,kk2)
                  v8=rho(ii1,jj2,kk2)

                  !> Defino puntos del cubo
                  x1=(ii1-1)*dx
                  y1=(jj1-1)*dy
                  z1=(kk1-1)*dz

                  r=(xval-x1)/dx
                  s=(yval-y1)/dy
                  t=(zval-z1)/dz

                  if (abs(r) <= eps) r=0.0
                  if (abs(s) <= eps) s=0.0
                  if (abs(t) <= eps) t=0.0

                  !> Interpolacion tridimensional a 8 puntos
                  valor=(1.0-r)*(1.0-s)*(1.0-t)* v1 + &
                             r *(1.0-s)*(1.0-t)* v2 + &
                             r *     s *(1.0-t)* v3 + &
                        (1.0-r)*     s *(1.0-t)* v4 + &
                        (1.0-r)*(1.0-s)*     t * v5 + &
                             r *(1.0-s)*     t * v6 + &
                             r *     s *     t * v7 + &
                        (1.0-r)*     s *     t * v8

                  rhonew(i,j,k)=valor

               end do
            end do
         end do

      end if

   End Function Load_ExtendedMap

   !!----
   !!---- CALCULATE_CONTOUR2D
   !!----     Subroutine for Contour 2D
   !!----
   !!---- 31/05/2020
   !!
   Module Subroutine Calculate_Contour2D(d,ilb,iub,jlb,jub,x,y,z,nlv,npt,xyz)
      !---- Arguments ----!
      integer,                                    intent(in)     :: ilb,iub           ! Lower and upper limits on the first dimension
      integer,                                    intent(in)     :: jlb,jub           ! Lower and upper limits for the second dimension
      real(kind=cp), dimension (ilb:iub,jlb:jub), intent(in)     :: d                 ! Section 2D
      real(kind=cp), dimension (ilb:iub),         intent(in)     :: x                 ! Limits values on X
      real(kind=cp), dimension (jlb:jub),         intent(in)     :: y                 ! Limits values on Y
      real(kind=cp), dimension (:),               intent(in)     :: z                 ! Level values
      integer,                                    intent(in)     :: nlv               ! Number of levels
      integer,                                    intent(in out) :: npt               ! Number of Points
      real(kind=cp), dimension (:,:),             intent(out)    :: xyz               ! XY Points

      !---- Local variables ----!
      integer                             :: j,i,k,m,m1,m2,m3
      integer                             :: cases
      integer, dimension (0:4)            :: sh
      integer, dimension (1:4)            :: im=[0,1,1,0], jm=[0,0,1,1]
      integer, dimension (-1:1,-1:1,-1:1) :: castab
      real(kind=cp), dimension (0:4)      :: h, xh, yh
      real(kind=cp)                       :: dmin,dmax,x1,y1,x2,y2


      !> Init
      castab= reshape ( [0,0,9,0,1,5,7,4,8,0,3,6,2,3,2,6,3,0,8,4,7,5,1,0,9,0,0],[3,3,3] )

      !> Scan the arrays, top down, left to right within rows
      do j=jub-1,jlb,-1
         do i=ilb,iub-1
            dmin=min(d(i,j),d(i,j+1),d(i+1,j),d(i+1,j+1))
            dmax=max(d(i,j),d(i,j+1),d(i+1,j),d(i+1,j+1))

            if (dmax >= z(1) .and. dmin <= z(nlv)) then
               do k=1,nlv
                  if (z(k) >= dmin .and. z(k) <= dmax) then
                     do m=4,0,-1
                        if (m > 0) then
                           h(m)=d(i+im(m),j+jm(m)) -z(k)
                           xh(m)=x(i+im(m))
                           yh(m)=y(j+jm(m))
                        else
                           h(0)=0.25*(h(1)+h(2)+h(3)+h(4))
                           xh(0)=0.5*(x(i)+x(i+1))
                           yh(0)=0.5*(y(j)+y(j+1))
                        end if
                        if ( h(m) > 0.0) then
                           sh(m)=+1
                        else if ( h(m) < 0.0) then
                           sh(m)=-1
                        else
                           sh(m)=0
                        end if
                     end do

                     !> Scan each triangle in the box
                     do m=1,4
                        m1=m
                        m2=0
                        if (m /= 4) then
                           m3=m+1
                        else
                           m3=1
                        end if
                        cases=castab(sh(m1),sh(m2),sh(m3))
                        if (cases /= 0) then
                           select case (cases)
                              case (1)
                                 ! Case 1 - Line between vertices 1 and 2
                                 x1=xh(m1)
                                 y1=yh(m1)
                                 x2=xh(m2)
                                 y2=yh(m2)

                              case (2)
                                 ! Case 2 - Line between vertices 2 and 3
                                 x1=xh(m2)
                                 y1=yh(m2)
                                 x2=xh(m3)
                                 y2=yh(m3)

                              case (3)
                                 ! Case 3 - Line between vertices 3 and 1
                                 x1=xh(m3)
                                 y1=yh(m3)
                                 x2=xh(m1)
                                 y2=yh(m1)

                              case (4)
                                 ! Case 4 - Line between vertices 1 and side 2-3
                                 x1=xh(m1)
                                 y1=yh(m1)
                                 x2= xy_sect(m2,m3,h,xh) !xsect(m2,m3)
                                 y2= xy_sect(m2,m3,h,yh) !ysect(m2,m3)

                              case (5)
                                 ! Case 5 - Line between vertices 2 and side 3-1
                                 x1=xh(m2)
                                 y1=yh(m2)
                                 x2= xy_sect(m3,m1,h,xh) !xsect(m3,m1)
                                 y2= xy_sect(m3,m1,h,yh) !ysect(m3,m1)

                              case (6)
                                 ! Case 6 - Line between vertices 3 and side 1-2
                                 x1=xh(m3)
                                 y1=yh(m3)
                                 x2= xy_sect(m1,m2,h,xh) !xsect(m1,m2)
                                 y2= xy_sect(m1,m2,h,yh) !ysect(m1,m2)

                              case (7)
                                 ! Case 7 - Line between sides 1-2 and  2-3
                                 x1= xy_sect(m1,m2,h,xh) !xsect(m1,m2)
                                 y1= xy_sect(m1,m2,h,yh) !ysect(m1,m2)
                                 x2= xy_sect(m2,m3,h,xh) !xsect(m2,m3)
                                 y2= xy_sect(m2,m3,h,yh) !ysect(m2,m3)

                              case (8)
                                 ! Case 8 - Line between sides 2-3 and  3-1
                                 x1= xy_sect(m2,m3,h,xh) !xsect(m2,m3)
                                 y1= xy_sect(m2,m3,h,yh) !ysect(m2,m3)
                                 x2= xy_sect(m3,m1,h,xh) !xsect(m3,m1)
                                 y2= xy_sect(m3,m1,h,yh) !ysect(m3,m1)

                              case (9)
                                 ! Case 9 - Line between sides 3-1 and  1-2
                                 x1= xy_sect(m3,m1,h,xh) !xsect(m3,m1)
                                 y1= xy_sect(m3,m1,h,yh) !ysect(m3,m1)
                                 x2= xy_sect(m1,m2,h,xh) !xsect(m1,m2)
                                 y2= xy_sect(m1,m2,h,yh) !ysect(m1,m2)
                           end select

                           if (npt+2 <= MAX_POINTS) then
                              npt=npt+1
                              xyz(1,npt)=x1
                              xyz(2,npt)=y1
                              xyz(3,npt)=real(k)

                              npt=npt+1
                              xyz(1,npt)=x2
                              xyz(2,npt)=y2
                              xyz(3,npt)=real(k)
                           end if

                        end if
                     end do
                  end if
               end do
            end if
         end do
      end do

   End Subroutine Calculate_Contour2D

   !!--++
   !!--++ PEAK_LIST
   !!--++
   !!--++    (Private)
   !!--++    Add a new peak position on the list if there is no close peak (< 0.25).
   !!--++
   !!--++ 31/05/2020
   !!
   Module Subroutine Peak_List(Pto,Grp,Cell,Npks,Pks)
      !---- Arguments ----!
      real(kind=cp), dimension(4),   intent(in)     :: Pto    ! New position to add on the List
      class(SpG_Type),               intent(in)     :: Grp    ! Space Group
      class(Cell_G_Type),            intent(in)     :: Cell   ! Cell
      integer,                       intent(in out) :: NPks   ! Number of peaks on the list
      real(kind=cp), dimension(:,:), intent(in out) :: Pks    ! List of Peaks

      !---- Local variables ----!
      integer                      :: i,j
      real(kind=cp), dimension (3) :: pto1

      if (npks == 0) then
         !> First Peak
         npks=1
         pks(:,1)=pto
      else
         !> Searching if the peak in in the list ----!
         do j=1,npks
            do i=1,grp%multip
               pto1=Apply_OP(grp%op(i),pks(1:3,j))
               pto1=mod(pto1+10.0_cp,1.0_cp)
               if (distance(pto,pto1,cell) <= 0.25) return
            end do
         end do
         npks=npks+1
         pks(:,npks)=pto
      end if

   End Subroutine Peak_List

   !!----
   !!---- SEARCH_PEAKS
   !!----    General procedure to search peaks on Rho
   !!----
   !!---- 31/05/2020
   !!
   Module Subroutine Search_Peaks(Rho,Grp,Cell,NPFound,Pks,Abs_Code)
      !---- Arguments ----!
      real(kind=cp), dimension(:,:,:),    intent(in)      :: Rho         ! Density
      class(SpG_Type),                    intent(in)      :: Grp         ! Space Group
      class(cell_G_type),                 intent(in)      :: Cell        ! Cell
      integer,                            intent(in out)  :: NPFound     ! Number of peaks to found
      real(kind=cp), dimension(:,:),      intent(out)     :: Pks         ! Peak List  dimension(4, NPfound)
      logical, optional,                  intent(in)      :: Abs_Code    ! logical to use absolute value on Rho

      !---- Local Variables ----!
      logical                                    :: mode_abs
      integer                                    :: nscan,npks
      integer                                    :: nu,nv,nw
      integer                                    :: ii_ini,jj_ini,kk_ini,ii_fin,jj_fin,kk_fin
      integer                                    :: ia,ja,ka
      integer                                    :: i,j,k,ji,ji11,ji21,i11,i21
      integer                                    :: izt1t,izt2t,izt3t

      real(kind=cp), parameter                   :: FAC_MAX=1.0e4
      real(kind=cp)                              :: denabsmax,fac_scale
      real(kind=cp), dimension(4)                :: pto
      integer, dimension (size(rho,1),size(rho,2),size(rho,3)):: nr3d     ! automatic array

      !> Init
      call clear_error()
      npks=0
      pks =0.0_cp
      mode_abs=.false.
      if (present(abs_code)) mode_abs=abs_code

      !> Memory for NR3D
      nr3d=0
      nu=size(rho,1)
      nv=size(rho,2)
      nw=size(rho,3)

      !> Loading Rho on Nr3D
      denabsmax=max(maxval(rho),abs(minval(rho)))
      fac_scale=FAC_MAX/denabsmax
      nr3d=nint(rho*fac_scale)
      if (mode_abs) nr3d=abs(nr3d)

      !> Searching Zone
      ii_ini=1
      jj_ini=1
      kk_ini=1
      ii_fin=nv
      jj_fin=nu
      kk_fin=nw

      !> Searching Procedure
      search:do nscan=nint(fac_max),0,-10
         do ka=kk_ini,kk_fin
            if (ka <= 0) then
               k=nw+ka
            elseif (ka > nw) then
               k=ka-nw
            else
               k=ka
            end if
            do ia=ii_ini,ii_fin
               if (ia <= 0) then
                  i=nv+ia
               elseif (ia > nv) then
                  i=ia-nv
               else
                  i=ia
               end if
               do ja=jj_ini,jj_fin
                  if (ja <= 0) then
                     j=nu+ja
                  elseif(ja > nu) then
                     j=ja-nu
                  else
                     j=ja
                  end if

                  if (nr3d(j,i,k)-nscan <= 0) cycle
                  ji=j
                  ji11=j-1
                  ji21=j+1
                  i11=i-1
                  i21=i+1
                  izt2t=k
                  izt1t=k-1
                  izt3t=k+1

                  if (ji11  <= 0) ji11=nu+ji11
                  if (i11   <= 0) i11=nv+i11
                  if (izt1t <= 0) izt1t=nw+izt1t

                  if (ji21  > nu) ji21=ji21-nu
                  if (i21   > nv) i21=i21-nv
                  if (izt3t > nw) izt3t=izt3t-nw

                  if (nr3d(ji,i,izt2t)-nr3d(ji11,  i,izt2t) <= 0) cycle   !Punto 122 >
                  if (nr3d(ji,i,izt2t)-nr3d(ji21,  i,izt2t) <  0) cycle   !      322 >=
                  if (nr3d(ji,i,izt2t)-nr3d(ji21,i11,izt2t) <= 0) cycle   !      312 >
                  if (nr3d(ji,i,izt2t)-nr3d(ji  ,i11,izt2t) <= 0) cycle   !      212 >
                  if (nr3d(ji,i,izt2t)-nr3d(ji11,i11,izt2t) <= 0) cycle   !      112 >
                  if (nr3d(ji,i,izt2t)-nr3d(ji11,i21,izt2t) <  0) cycle   !      132 >=
                  if (nr3d(ji,i,izt2t)-nr3d(ji  ,i21,izt2t) <  0) cycle   !      232 >=
                  if (nr3d(ji,i,izt2t)-nr3d(ji21,i21,izt2t) <  0) cycle   !      332 >=
                  if (nr3d(ji,i,izt2t)-nr3d(ji  ,  i,izt1t) <= 0) cycle   !      221 >
                  if (nr3d(ji,i,izt2t)-nr3d(ji11,  i,izt1t) <= 0) cycle   !      121 >
                  if (nr3d(ji,i,izt2t)-nr3d(ji21,  i,izt1t) <= 0) cycle   !      321 >
                  if (nr3d(ji,i,izt2t)-nr3d(ji  ,i11,izt1t) <= 0) cycle   !      211 >
                  if (nr3d(ji,i,izt2t)-nr3d(ji  ,i21,izt1t) <= 0) cycle   !      231 >
                  if (nr3d(ji,i,izt2t)-nr3d(ji  ,  i,izt3t) <  0) cycle   !      223 >=
                  if (nr3d(ji,i,izt2t)-nr3d(ji11,  i,izt3t) <  0) cycle   !      123 >=
                  if (nr3d(ji,i,izt2t)-nr3d(ji21,  i,izt3t) <  0) cycle   !      323 >=
                  if (nr3d(ji,i,izt2t)-nr3d(ji  ,i11,izt3t) <  0) cycle   !      213 >=
                  if (nr3d(ji,i,izt2t)-nr3d(ji  ,i21,izt3t) <  0) cycle   !      233 >=

                  !> Position of the peak
                  pto(1:3)=peak_position(nr3d,j,i,k)
                  pto(4)=rho(j,i,k)

                  !> Good peak?
                  call peak_list(pto,Grp,Cell,npks,pks)
                  if (npks == npfound) exit search
               end do
            end do
         end do
      end do search! nscan

      npfound=npks

   End Subroutine Search_Peaks

End SubModule Maps_Mapping