!!----
!!----
!!----
SubModule (CFML_Maps) Maps_Marching_Cubes
   implicit none
   Contains
   !!----
   !!---- INDEX_CUBE
   !!----    Return the index for Marching cubes algorithm
   !!----
   !!---- 31/05/2020
   !!
   Pure Module Function Index_Cube(iv, mc) Result(Ind)
      !---- Arguments ----!
      integer, dimension(8), intent(in) :: iv        ! In -> Vertices state On/Off
      logical,               intent(in) :: mc        ! Mc = .true. Only triangles (128-255 Code) if not (0-127)
      integer                           :: Ind

      !---- local variables ----!
      integer :: i

      i = iv(1) + iv(2)*2 + iv(3)*4 + iv(4)*8 + iv(5)*16 + &
          iv(6)*32 + iv(7)*64 + iv(8)*128

      ind=i
      if (mc) then
         if (ind <= 127) ind=255-i
      else
         if (ind >= 128) ind=255-i
      end if

   End Function Index_Cube

   !!----
   !!---- VERTICES_CUBE
   !!----
   !!----    Return the state of the 8 vertices of the cube in Marching
   !!----    cubes algorithm
   !!----
   !!---- Update: February - 2005
   !!
   Pure Module Function Vertices_Cube(Index_Cube) Result(Iv)
      !---- Arguments ----!
      integer, intent(in)   :: index_cube  !  In -> Index cube
      integer, dimension(8) :: iv          ! Out -> Vertices state

      !---- Local Variables ----!
      integer :: k

      k=index_cube
      iv=0

      iv(8)=k/128
      k=k-iv(8)*128

      iv(7)=k/64
      k=k-iv(7)*64

      iv(6)=k/32
      k=k-iv(6)*32

      iv(5)=k/16
      k=k-iv(5)*16

      iv(4)=k/8
      k=k-iv(4)*8

      iv(3)=k/4
      k=k-iv(3)*4

      iv(2)=k/2
      k=k-iv(2)*2

      iv(1)=k

   End Function Vertices_Cube

   !!--++
   !!--++ VERTICE_POINT_FIX
   !!--++
   !!--++     (OVERLOADED)
   !!--++     Return the relative position point from (i,j,k) of V1
   !!--++     Given a binary dataset.
   !!--++
   !!--++ 31/05/2020
   !!
   Pure Module Function Vertice_Point_Fix(Code_Edge) Result(V)
      !---- Arguments ----!
      integer,          intent(in) :: code_edge
      real(kind=cp), dimension(3)  :: v

      !---- Local Variables ----!

      v=0.0_cp
      select case (code_edge)
         case ( 1)
            v(1)=0.5_cp

         case ( 2)
            v(1)=1.0_cp
            v(2)=0.5_cp

         case ( 3)
            v(1)=0.5_cp
            v(2)=1.0_cp

         case ( 4)
            v(2)=0.5_cp

         case ( 5)
            v(1)=0.5_cp
            v(3)=1.0_cp

         case ( 6)
            v   =1.0_cp
            v(2)=0.5_cp

         case ( 7)
            v   =1.0_cp
            v(1)=0.5_cp

         case ( 8)
            v(2)=0.5_cp
            v(3)=1.0_cp

         case ( 9)
            v(3)=0.5_cp

         case (10)
            v(1)=1.0_cp
            v(3)=0.5_cp

         case (11)
            v(2)=1.0_cp
            v(3)=0.5_cp

         case (12)
            v   =1.0_cp
            v(3)=0.5_cp

      end select

   End Function Vertice_Point_Fix

   !!--++
   !!--++ VERTICE_POINT_CAL
   !!--++
   !!--++     (OVERLOADED)
   !!--++     Return the relative position point from (i,j,k) of V1
   !!--++     Given a binary dataset, linear interpolation is not needed to
   !!--++     extract isosurfaces, When a cell edge in a binary dataset has
   !!--++     both on and off corners, the midpoint of the edge is the
   !!--++     intersection being looked for.
   !!--++
   !!--++ 31/05/2020
   !!
   Pure Module Function Vertice_Point_Cal(Code_Edge,D0,D1,D2,D3,D4,D5,D6,D7,D8) Result(V)
      !---- Arguments ----!
      integer,          intent(in) :: code_edge
      real(kind=cp),    intent(in) :: d0,d1,d2,d3,d4,d5,d6,d7,d8
      real(kind=cp), dimension(3)  :: v

      !---- Local Variables ----!
      real(kind=cp) :: dmin,dmax,dd

      v=0.0_cp
      select case (code_edge)
         case ( 1)
            v(1)=0.5
            dmin=min(d2,d1)
            dmax=max(d2,d1)
            dd=dmax-dmin
            if (abs(dd) > eps ) v(1)=(d0-dmin)/dd

         case ( 2)
            v(1)=1.0
            v(2)=0.5
            dmin=min(d3,d2)
            dmax=max(d3,d2)
            dd=dmax-dmin
            if (abs(dd) > eps ) v(2)=(d0-dmin)/dd

         case ( 3)
            v(1)=0.5
            v(2)=1.0
            dmin=min(d4,d3)
            dmax=max(d4,d3)
            dd=dmax-dmin
            if (abs(dd) > eps ) v(1)=(d0-dmin)/dd

         case ( 4)
            v(2)=0.5
            dmin=min(d4,d1)
            dmax=max(d4,d1)
            dd=dmax-dmin
            if (abs(dd) > eps ) v(2)=(d0-dmin)/dd

         case ( 5)
            v(1)=0.5
            v(3)=1.0
            dmin=min(d6,d5)
            dmax=max(d6,d5)
            dd=dmax-dmin
            if (abs(dd) > eps ) v(1)=(d0-dmin)/dd

         case ( 6)
            v(1)=1.0
            v(2)=0.5
            v(3)=1.0
            dmin=min(d7,d6)
            dmax=max(d7,d6)
            dd=dmax-dmin
            if (abs(dd) > eps ) v(2)=(d0-dmin)/dd

         case ( 7)
            v(1)=0.5
            v(2)=1.0
            v(3)=1.0
            dmin=min(d8,d7)
            dmax=max(d8,d7)
            dd=dmax-dmin
            if (abs(dd) > eps ) v(1)=(d0-dmin)/dd

         case ( 8)
            v(2)=0.5
            v(3)=1.0
            dmin=min(d8,d5)
            dmax=max(d8,d5)
            dd=dmax-dmin
            if (abs(dd) > eps ) v(2)=(d0-dmin)/dd

         case ( 9)
            v(3)=0.5
            dmin=min(d5,d1)
            dmax=max(d5,d1)
            dd=dmax-dmin
            if (abs(dd) > eps ) v(3)=(d0-dmin)/dd

         case (10)
            v(1)=1.0
            v(3)=0.5
            dmin=min(d6,d2)
            dmax=max(d6,d2)
            dd=dmax-dmin
            if (abs(dd) > eps ) v(3)=(d0-dmin)/dd

         case (11)
            v(2)=1.0
            v(3)=0.5
            dmin=min(d8,d4)
            dmax=max(d8,d4)
            dd=dmax-dmin
            if (abs(dd) > eps ) v(3)=(d0-dmin)/dd

         case (12)
            v(1)=1.0
            v(2)=1.0
            v(3)=0.5
            dmin=min(d7,d3)
            dmax=max(d7,d3)
            dd=dmax-dmin
            if (abs(dd) > eps ) v(3)=(d0-dmin)/dd

      end select

   End Function Vertice_Point_Cal

   !!----
   !!---- SET_CUBE_INFO
   !!----
   !!----    Set values for Cube_Info.
   !!----    From 0 to 127 the code is defined according the next table.
   !!----    From 128 to 255 all is defined using triangles.
   !!----
   !!--<<   Code  Figure               Process
   !!----    =====================================================================
   !!----      1   Triangle             Pto1 -> Pto2 -> Pto3 -> Pto1
   !!----
   !!----      2   Trapezoide           Pto1 -> Pto2 -> Pto3 -> Pto4 -> Pto1
   !!----
   !!----      3   Triangle             Pto1 -> Pto2 -> Pto3 -> Pto1
   !!----          Trapezoide           Pto4 -> Pto5 -> Pto6 -> Pto7 -> Pto4
   !!----
   !!----      4   Triangle             Pto1 -> Pto2 -> Pto3 -> Pto1
   !!----          Triangle             Pto4 -> Pto5 -> Pto6 -> Pto4
   !!----          Trapezoide           Pto7 -> Pto8 -> Pto9 -> Pto10 -> Pto7
   !!----
   !!----      5   Triangle + Line      Pto1 -> Pto2 -> Pto3 -> Pto1 :  Pto1 -> Pto4
   !!----
   !!----      6   Triangle             Pto1 -> Pto2 -> Pto3 -> Pto1
   !!----          Triangle + Line      Pto4 -> Pto5 -> Pto6 -> Pto4 :  Pto4 -> Pto7
   !!-->>
   !!---- 31/05/2020
   !!
   Module Subroutine Set_Cube_Info()
      !---- This is a polyline configuration ----!
      cube_info(  0) = cube_info_type (0,0,(/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/))
      cube_info(  1) = cube_info_type (1,1,(/ 1, 4, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0/))
      cube_info(  2) = cube_info_type (1,1,(/ 1, 2,10, 0, 0, 0, 0, 0, 0, 0, 0, 0/))
      cube_info(  3) = cube_info_type (1,2,(/ 2, 4, 9,10, 0, 0, 0, 0, 0, 0, 0, 0/))
      cube_info(  4) = cube_info_type (1,1,(/ 2, 3,12, 0, 0, 0, 0, 0, 0, 0, 0, 0/))
      cube_info(  5) = cube_info_type (2,1,(/ 1, 4, 9, 2, 3,12, 0, 0, 0, 0, 0, 0/))
      cube_info(  6) = cube_info_type (1,2,(/ 1, 3,12,10, 0, 0, 0, 0, 0, 0, 0, 0/))
      cube_info(  7) = cube_info_type (1,3,(/ 9,10,12, 3, 4, 9,12, 0, 0, 0, 0, 0/))
      cube_info(  8) = cube_info_type (1,1,(/ 3, 4,11, 0, 0, 0, 0, 0, 0, 0, 0, 0/))
      cube_info(  9) = cube_info_type (1,2,(/ 1, 3,11, 9, 0, 0, 0, 0, 0, 0, 0, 0/))
      cube_info( 10) = cube_info_type (2,1,(/ 1, 2,10, 3, 4,11, 0, 0, 0, 0, 0, 0/))
      cube_info( 11) = cube_info_type (1,3,(/ 9,10,11, 2, 3,11,10, 0, 0, 0, 0, 0/))
      cube_info( 12) = cube_info_type (1,2,(/ 2, 4,11,12, 0, 0, 0, 0, 0, 0, 0, 0/))
      cube_info( 13) = cube_info_type (1,3,(/ 9,11,12, 1, 2,12, 9, 0, 0, 0, 0, 0/))
      cube_info( 14) = cube_info_type (1,3,(/10,11,12, 1, 4,11,10, 0, 0, 0, 0, 0/))
      cube_info( 15) = cube_info_type (1,2,(/ 9,10,12,11, 0, 0, 0, 0, 0, 0, 0, 0/))
      cube_info( 16) = cube_info_type (1,1,(/ 5, 8, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0/))
      cube_info( 17) = cube_info_type (1,2,(/ 1, 4, 8, 5, 0, 0, 0, 0, 0, 0, 0, 0/))
      cube_info( 18) = cube_info_type (2,1,(/ 1, 2,10, 5, 8, 9, 0, 0, 0, 0, 0, 0/))
      cube_info( 19) = cube_info_type (1,3,(/ 2, 4, 8, 2, 8, 5,10, 0, 0, 0, 0, 0/))
      cube_info( 20) = cube_info_type (2,1,(/ 2, 3,12, 5, 8, 9, 0, 0, 0, 0, 0, 0/))
      cube_info( 21) = cube_info_type (1,3,(/ 2, 3,12, 1, 4, 8, 5, 0, 0, 0, 0, 0/))
      cube_info( 22) = cube_info_type (1,3,(/ 5, 8, 9, 1, 3,12,10, 0, 0, 0, 0, 0/))
      cube_info( 23) = cube_info_type (3,1,(/ 3, 4,12, 4, 5, 8, 5,10,12, 0, 0, 0/))
      cube_info( 24) = cube_info_type (2,1,(/ 3, 4,11, 5, 8, 9, 0, 0, 0, 0, 0, 0/))
      cube_info( 25) = cube_info_type (1,3,(/ 1, 3, 5, 3,11, 8, 5, 8, 0, 0, 0, 0/))
      cube_info( 26) = cube_info_type (3,1,(/ 1, 2,10, 3, 4,11, 5, 8, 9, 0, 0, 0/))
      cube_info( 27) = cube_info_type (2,5,(/ 2, 5,10, 3, 8,11, 3, 5, 0, 0, 0, 0/))
      cube_info( 28) = cube_info_type (1,3,(/ 5, 8, 9, 2, 4,11,12, 0, 0, 0, 0, 0/))
      cube_info( 29) = cube_info_type (3,1,(/ 1, 2, 5, 5, 8,11, 2,11,12, 0, 0, 0/))
      cube_info( 30) = cube_info_type (1,4,(/ 5, 8, 9,10,11,12, 1,10,11, 4, 0, 0/))
      cube_info( 31) = cube_info_type (1,6,(/ 5,10, 8,11,12,10, 8, 0, 0, 0, 0, 0/))
      cube_info( 32) = cube_info_type (1,1,(/ 5, 6,10, 0, 0, 0, 0, 0, 0, 0, 0, 0/))
      cube_info( 33) = cube_info_type (2,1,(/ 1, 4, 9, 5, 6,10, 0, 0, 0, 0, 0, 0/))
      cube_info( 34) = cube_info_type (1,2,(/ 1, 2, 6, 5, 0, 0, 0, 0, 0, 0, 0, 0/))
      cube_info( 35) = cube_info_type (1,3,(/ 2, 4, 6, 5, 6, 4, 9, 0, 0, 0, 0, 0/))
      cube_info( 36) = cube_info_type (2,1,(/ 2, 3,12, 5, 6,10, 0, 0, 0, 0, 0, 0/))
      cube_info( 37) = cube_info_type (3,1,(/ 1, 4, 9, 2, 3,12, 5, 6,10, 0, 0, 0/))
      cube_info( 38) = cube_info_type (1,3,(/ 1, 3, 5, 3, 5, 6,12, 0, 0, 0, 0, 0/))
      cube_info( 39) = cube_info_type (2,5,(/ 4, 5, 9, 3, 6, 3,12, 5, 0, 0, 0, 0/))
      cube_info( 40) = cube_info_type (2,1,(/ 3, 4,11, 5, 6,10, 0, 0, 0, 0, 0, 0/))
      cube_info( 41) = cube_info_type (1,3,(/ 5, 6,10, 1, 3,11, 9, 0, 0, 0, 0, 0/))
      cube_info( 42) = cube_info_type (1,3,(/ 3, 4,11, 1, 2, 6, 5, 0, 0, 0, 0, 0/))
      cube_info( 43) = cube_info_type (3,1,(/ 2, 3, 6, 5, 6, 9, 3, 9,11, 0, 0, 0/))
      cube_info( 44) = cube_info_type (1,3,(/ 5, 6,10, 2, 4,11,12, 0, 0, 0, 0, 0/))
      cube_info( 45) = cube_info_type (1,4,(/ 1, 2,10, 9,11,12, 5, 6,12, 9, 0, 0/))
      cube_info( 46) = cube_info_type (3,1,(/ 1, 4, 5, 4,11,12, 5, 6,12, 0, 0, 0/))
      cube_info( 47) = cube_info_type (1,3,(/ 9,11,12, 5, 6,12, 9, 0, 0, 0, 0, 0/))
      cube_info( 48) = cube_info_type (1,2,(/ 6, 8, 9,10, 0, 0, 0, 0, 0, 0, 0, 0/))
      cube_info( 49) = cube_info_type (1,3,(/ 4, 6, 8, 1, 4, 6,10, 0, 0, 0, 0, 0/))
      cube_info( 50) = cube_info_type (1,3,(/ 2, 6, 8, 1, 2, 8, 9, 0, 0, 0, 0, 0/))
      cube_info( 51) = cube_info_type (1,2,(/ 2, 4, 8, 6, 0, 0, 0, 0, 0, 0, 0, 0/))
      cube_info( 52) = cube_info_type (1,3,(/ 2, 3,12, 6, 8, 9,10, 0, 0, 0, 0, 0/))
      cube_info( 53) = cube_info_type (1,4,(/ 2, 3,12, 4, 6, 8, 1, 4, 6,10, 0, 0/))
      cube_info( 54) = cube_info_type (3,1,(/ 1, 3, 9, 6, 8, 9, 3, 6,12, 0, 0, 0/))
      cube_info( 55) = cube_info_type (1,6,(/ 4, 8, 6,12, 4, 3, 6, 0, 0, 0, 0, 0/))
      cube_info( 56) = cube_info_type (1,3,(/ 3, 4,11, 6, 8, 9,10, 0, 0, 0, 0, 0/))
      cube_info( 57) = cube_info_type (3,1,(/ 1, 3,10, 6, 8,10, 3, 8,11, 0, 0, 0/))
      cube_info( 58) = cube_info_type (1,4,(/ 3, 4,11, 2, 6, 8, 1, 2, 8, 9, 0, 0/))
      cube_info( 59) = cube_info_type (1,6,(/ 2, 6, 8,11, 3, 2, 8, 0, 0, 0, 0, 0/))
      cube_info( 60) = cube_info_type (2,2,(/ 2, 4,11,12, 6, 8, 9,10, 0, 0, 0, 0/))
      cube_info( 61) = cube_info_type (1,3,(/ 1, 2,10, 6, 8,11,12, 0, 0, 0, 0, 0/))
      cube_info( 62) = cube_info_type (1,3,(/ 1, 4, 9, 6, 8,11,12, 0, 0, 0, 0, 0/))
      cube_info( 63) = cube_info_type (1,2,(/ 6, 8,11,12, 0, 0, 0, 0, 0, 0, 0, 0/))
      cube_info( 64) = cube_info_type (1,1,(/ 6, 7,12, 0, 0, 0, 0, 0, 0, 0, 0, 0/))
      cube_info( 65) = cube_info_type (2,1,(/ 1, 4, 9, 6, 7,12, 0, 0, 0, 0, 0, 0/))
      cube_info( 66) = cube_info_type (2,1,(/ 1, 2,10, 6, 7,12, 0, 0, 0, 0, 0, 0/))
      cube_info( 67) = cube_info_type (1,3,(/ 6, 7,12, 2, 4, 9,10, 0, 0, 0, 0, 0/))
      cube_info( 68) = cube_info_type (1,2,(/ 2, 3, 7, 6, 0, 0, 0, 0, 0, 0, 0, 0/))
      cube_info( 69) = cube_info_type (1,3,(/ 1, 4, 9, 2, 3, 7, 6, 0, 0, 0, 0, 0/))
      cube_info( 70) = cube_info_type (1,3,(/ 1, 3, 7, 1, 7, 6,10, 0, 0, 0, 0, 0/))
      cube_info( 71) = cube_info_type (3,1,(/ 3, 4, 7, 4, 9,10, 6, 7,10, 0, 0, 0/))
      cube_info( 72) = cube_info_type (2,1,(/ 3, 4,11, 6, 7,12, 0, 0, 0, 0, 0, 0/))
      cube_info( 73) = cube_info_type (1,3,(/ 6, 7,12, 1, 3,11, 9, 0, 0, 0, 0, 0/))
      cube_info( 74) = cube_info_type (3,1,(/ 1, 2,10, 3, 4,11, 6, 7,12, 0, 0, 0/))
      cube_info( 75) = cube_info_type (1,4,(/ 6, 7,12, 9,10,11, 2, 3,11,10, 0, 0/))
      cube_info( 76) = cube_info_type (1,3,(/ 2, 4, 6, 4, 6, 7,11, 0, 0, 0, 0, 0/))
      cube_info( 77) = cube_info_type (3,1,(/ 1, 2, 6, 1, 9,11, 6, 7,11, 0, 0, 0/))
      cube_info( 78) = cube_info_type (2,5,(/ 4, 7,11, 1, 6,10, 1, 7, 0, 0, 0, 0/))
      cube_info( 79) = cube_info_type (1,3,(/ 9,10,11, 6, 7,11,10, 0, 0, 0, 0, 0/))
      cube_info( 80) = cube_info_type (2,1,(/ 5, 8, 9, 6, 7,12, 0, 0, 0, 0, 0, 0/))
      cube_info( 81) = cube_info_type (1,3,(/ 6, 7,12, 1, 4, 8, 5, 7, 0, 0, 0, 0/))
      cube_info( 82) = cube_info_type (3,1,(/ 1, 2,10, 5, 8, 9, 6, 7,12, 0, 0, 0/))
      cube_info( 83) = cube_info_type (1,4,(/ 6, 7,12, 2, 4, 8, 2, 8, 5,10, 0, 0/))
      cube_info( 84) = cube_info_type (1,3,(/ 5, 8, 9, 2, 3, 7, 6, 0, 0, 0, 0, 0/))
      cube_info( 85) = cube_info_type (2,2,(/ 1, 4, 8, 5, 2, 3, 7, 6, 0, 0, 0, 0/))
      cube_info( 86) = cube_info_type (1,4,(/ 5, 8, 9, 1, 3, 7, 1, 7, 6,10, 0, 0/))
      cube_info( 87) = cube_info_type (1,3,(/ 5, 6,10, 3, 4, 8, 7, 0, 0, 0, 0, 0/))
      cube_info( 88) = cube_info_type (3,1,(/ 3, 4,11, 5, 8, 9, 6, 7,12, 0, 0, 0/))
      cube_info( 89) = cube_info_type (1,4,(/ 6, 7,12, 1, 3, 5, 3, 5, 8,11, 0, 0/))
      cube_info( 90) = cube_info_type (4,1,(/ 1, 4, 9, 2, 3,12, 5, 6,10, 7, 8,11/))
      cube_info( 91) = cube_info_type (3,1,(/ 2, 3,12, 5, 6,10, 7, 8,11, 0, 0, 0/))
      cube_info( 92) = cube_info_type (1,4,(/ 5, 8, 9, 2, 4, 6, 4, 6, 7,11, 0, 0/))
      cube_info( 93) = cube_info_type (1,3,(/ 7, 8,11, 1, 2, 6, 5, 0, 0, 0, 0, 0/))
      cube_info( 94) = cube_info_type (3,1,(/ 1, 4, 9, 5, 6,10, 7, 8,11, 0, 0, 0/))
      cube_info( 95) = cube_info_type (2,1,(/ 5, 6,10, 7, 8,11, 0, 0, 0, 0, 0, 0/))
      cube_info( 96) = cube_info_type (1,2,(/ 5, 7,12,10, 0, 0, 0, 0, 0, 0, 0, 0/))
      cube_info( 97) = cube_info_type (1,3,(/ 1, 4, 9, 5, 7,12,10, 0, 0, 0, 0, 0/))
      cube_info( 98) = cube_info_type (1,3,(/ 1, 5, 7, 1, 2,12, 7, 0, 0, 0, 0, 0/))
      cube_info( 99) = cube_info_type (3,1,(/ 2, 4, 9, 5, 7, 9, 2, 7,12, 0, 0, 0/))
      cube_info(100) = cube_info_type (1,3,(/ 3, 5, 7, 2, 3, 5,10, 0, 0, 0, 0, 0/))
      cube_info(101) = cube_info_type (1,4,(/ 1, 4, 9, 3, 5, 7, 2, 3, 5,10, 0, 0/))
      cube_info(102) = cube_info_type (1,2,(/ 1, 3, 7, 5, 0, 0, 0, 0, 0, 0, 0, 0/))
      cube_info(103) = cube_info_type (1,3,(/ 3, 5, 7, 3, 4, 9, 5, 0, 0, 0, 0, 0/))
      cube_info(104) = cube_info_type (1,3,(/ 3, 4,11, 5, 7,12,10, 0, 0, 0, 0, 0/))
      cube_info(105) = cube_info_type (2,2,(/ 1, 3,11, 9, 5, 7,12,10, 0, 0, 0, 0/))
      cube_info(106) = cube_info_type (1,4,(/ 3, 4,11, 1, 5, 7, 1, 2,12, 7, 0, 0/))
      cube_info(107) = cube_info_type (1,3,(/ 2, 3,12, 5, 7,11, 9, 0, 0, 0, 0, 0/))
      cube_info(108) = cube_info_type (3,1,(/ 2, 4,10, 5, 7,10, 4, 7,11, 0, 0, 0/))
      cube_info(109) = cube_info_type (1,3,(/ 1, 2,10, 5, 7,11, 9, 0, 0, 0, 0, 0/))
      cube_info(110) = cube_info_type (1,3,(/ 1, 5, 7, 1, 4,11, 7, 0, 0, 0, 0, 0/))
      cube_info(111) = cube_info_type (1,2,(/ 5, 7,11, 9, 0, 0, 0, 0, 0, 0, 0, 0/))
      cube_info(112) = cube_info_type (1,3,(/ 9,10,12, 7, 8, 9,12, 0, 0, 0, 0, 0/))
      cube_info(113) = cube_info_type (3,1,(/ 1,10,12, 1, 4, 8, 7, 8,12, 0, 0, 0/))
      cube_info(114) = cube_info_type (2,5,(/ 2, 7,12, 1, 8, 9, 1, 7, 0, 0, 0, 0/))
      cube_info(115) = cube_info_type (1,3,(/ 2, 4, 8, 2, 8, 7,12, 0, 0, 0, 0, 0/))
      cube_info(116) = cube_info_type (3,1,(/ 2, 3, 7, 2, 9,10, 7, 8, 9, 0, 0, 0/))
      cube_info(117) = cube_info_type (1,3,(/ 1, 2,10, 3, 4, 8, 7, 0, 0, 0, 0, 0/))
      cube_info(118) = cube_info_type (1,3,(/ 1, 3, 7, 1, 7, 8, 9, 0, 0, 0, 0, 0/))
      cube_info(119) = cube_info_type (1,2,(/ 3, 4, 8, 7, 0, 0, 0, 0, 0, 0, 0, 0/))
      cube_info(120) = cube_info_type (1,4,(/ 3, 4,11, 9,10,12, 7, 8, 9,12, 0, 0/))
      cube_info(121) = cube_info_type (1,3,(/ 7, 8,11, 1, 3,12,10, 0, 0, 0, 0, 0/))
      cube_info(122) = cube_info_type (3,1,(/ 1, 4, 9, 2, 3,12, 7, 8,11, 0, 0, 0/))
      cube_info(123) = cube_info_type (2,1,(/ 2, 3,12, 7, 8,11, 0, 0, 0, 0, 0, 0/))
      cube_info(124) = cube_info_type (1,3,(/ 7, 8,11, 2, 4, 9,10, 0, 0, 0, 0, 0/))
      cube_info(125) = cube_info_type (2,1,(/ 1, 2,10, 7, 8,11, 0, 0, 0, 0, 0, 0/))
      cube_info(126) = cube_info_type (2,1,(/ 1, 4, 9, 7, 8,11, 0, 0, 0, 0, 0, 0/))
      cube_info(127) = cube_info_type (1,1,(/ 7, 8,11, 0, 0, 0, 0, 0, 0, 0, 0, 0/))

      !---- This is a triangle configuration ----!
      cube_info(128) = cube_info_type (1,1,(/ 7, 8,11, 0, 0, 0, 0, 0, 0, 0, 0, 0/))
      cube_info(129) = cube_info_type (2,1,(/ 1, 4, 9, 7, 8,11, 0, 0, 0, 0, 0, 0/))
      cube_info(130) = cube_info_type (2,1,(/ 1, 2,10, 7, 8,11, 0, 0, 0, 0, 0, 0/))
      cube_info(131) = cube_info_type (3,1,(/ 2, 4,10, 4, 9,10, 7, 8,11, 0, 0, 0/))
      cube_info(132) = cube_info_type (2,1,(/ 2, 3,12, 7, 8,11, 0, 0, 0, 0, 0, 0/))
      cube_info(133) = cube_info_type (3,1,(/ 1, 4, 9, 2, 3,12, 7, 8,11, 0, 0, 0/))
      cube_info(134) = cube_info_type (3,1,(/ 1, 3,10, 3,10,12, 7, 8,11, 0, 0, 0/))
      cube_info(135) = cube_info_type (4,1,(/ 3, 4,11, 7, 8, 9, 7, 9,12, 9,10,12/))
      cube_info(136) = cube_info_type (2,1,(/ 3, 4, 7, 4, 7, 8, 0, 0, 0, 0, 0, 0/))
      cube_info(137) = cube_info_type (3,1,(/ 1, 3, 7, 1, 7, 9, 7, 8, 9, 0, 0, 0/))
      cube_info(138) = cube_info_type (3,1,(/ 1, 2,10, 3, 4, 7, 4, 7, 8, 0, 0, 0/))
      cube_info(139) = cube_info_type (3,1,(/ 2, 3, 7, 2, 9,10, 7, 8, 9, 0, 0, 0/))
      cube_info(140) = cube_info_type (3,1,(/ 2, 4, 8, 2, 7, 8, 2, 7,12, 0, 0, 0/))
      cube_info(141) = cube_info_type (4,1,(/ 1, 2, 7, 2, 7, 8, 1, 8, 9, 2, 7,12/))
      cube_info(142) = cube_info_type (3,1,(/ 1,10,12, 1, 4, 8, 7, 8,12, 0, 0, 0/))
      cube_info(143) = cube_info_type (3,1,(/ 7, 8, 9, 7, 9,12, 9,10,12, 0, 0, 0/))
      cube_info(144) = cube_info_type (2,1,(/ 5, 7, 9, 7, 9,11, 0, 0, 0, 0, 0, 0/))
      cube_info(145) = cube_info_type (3,1,(/ 1, 4,11, 1, 5, 7, 1, 7,11, 0, 0, 0/))
      cube_info(146) = cube_info_type (3,1,(/ 1, 2,10, 5, 7, 9, 7, 9,11, 0, 0, 0/))
      cube_info(147) = cube_info_type (3,1,(/ 2, 4,10, 5, 7,10, 4, 7,11, 0, 0, 0/))
      cube_info(148) = cube_info_type (3,1,(/ 2, 3,12, 5, 7, 9, 7, 9,11, 0, 0, 0/))
      cube_info(149) = cube_info_type (4,1,(/ 1, 2,12, 1, 5, 7, 1, 7,12, 3, 4,11/))
      cube_info(150) = cube_info_type (4,1,(/ 1, 3, 9, 3, 9,11, 5, 7,10, 7,10,12/))
      cube_info(151) = cube_info_type (3,1,(/ 3, 4,11, 5, 7,10, 7,10,12, 0, 0, 0/))
      cube_info(152) = cube_info_type (3,1,(/ 3, 4, 5, 3, 5, 7, 4, 5, 9, 0, 0, 0/))
      cube_info(153) = cube_info_type (2,1,(/ 1, 3, 5, 3, 5, 7, 0, 0, 0, 0, 0, 0/))
      cube_info(154) = cube_info_type (4,1,(/ 1, 4, 9, 2, 3,10, 3, 5, 7, 3, 5,10/))
      cube_info(155) = cube_info_type (3,1,(/ 2, 3,10, 3, 5, 7, 3, 5,10, 0, 0, 0/))
      cube_info(156) = cube_info_type (3,1,(/ 2, 4, 9, 5, 7, 9, 2, 7,12, 0, 0, 0/))
      cube_info(157) = cube_info_type (3,1,(/ 1, 2, 7, 1, 5, 7, 2, 7,12, 0, 0, 0/))
      cube_info(158) = cube_info_type (3,1,(/ 1, 4, 9, 5, 7,10, 7,10,12, 0, 0, 0/))
      cube_info(159) = cube_info_type (2,1,(/ 5, 7,10, 7,10,12, 0, 0, 0, 0, 0, 0/))
      cube_info(160) = cube_info_type (2,1,(/ 5, 6,10, 7, 8,11, 0, 0, 0, 0, 0, 0/))
      cube_info(161) = cube_info_type (3,1,(/ 1, 4, 9, 5, 6,10, 7, 8,11, 0, 0, 0/))
      cube_info(162) = cube_info_type (3,1,(/ 1, 2, 5, 2, 5, 6, 7, 8,11, 0, 0, 0/))
      cube_info(163) = cube_info_type (4,1,(/ 2, 4, 6, 4, 6,11, 5, 8, 9, 6, 7,11/))
      cube_info(164) = cube_info_type (3,1,(/ 2, 3,12, 5, 6,10, 7, 8,11, 0, 0, 0/))
      cube_info(165) = cube_info_type (4,1,(/ 1, 4, 9, 2, 3,12, 5, 6,10, 7, 8,11/))
      cube_info(166) = cube_info_type (4,1,(/ 1, 3, 5, 3, 8, 5, 3, 8,11, 6, 7,12/))
      cube_info(167) = cube_info_type (3,1,(/ 3, 4,11, 5, 8, 9, 6, 7,12, 0, 0, 0/))
      cube_info(168) = cube_info_type (3,1,(/ 3, 4, 7, 4, 7, 8, 5, 6,10, 0, 0, 0/))
      cube_info(169) = cube_info_type (4,1,(/ 1, 3, 7, 1, 6,10, 1, 6, 7, 5, 8, 9/))
      cube_info(170) = cube_info_type (4,1,(/ 1, 4, 5, 4, 5, 8, 2, 3, 6, 3, 6, 7/))
      cube_info(171) = cube_info_type (3,1,(/ 2, 3, 6, 3, 6, 7, 5, 8, 9, 0, 0, 0/))
      cube_info(172) = cube_info_type (4,1,(/ 2, 4, 8, 2, 8,10, 5, 8,10, 6, 7,12/))
      cube_info(173) = cube_info_type (3,1,(/ 1, 2,10, 5, 8, 9, 6, 7,12, 0, 0, 0/))
      cube_info(174) = cube_info_type (3,1,(/ 1, 4, 5, 4, 5, 8, 6, 7,12, 0, 0, 0/))
      cube_info(175) = cube_info_type (2,1,(/ 5, 8, 9, 6, 7,12, 0, 0, 0, 0, 0, 0/))
      cube_info(176) = cube_info_type (3,1,(/ 6, 7,11, 6,10,11, 9,10,11, 0, 0, 0/))
      cube_info(177) = cube_info_type (4,1,(/ 1, 4, 6, 4, 6, 7, 1, 6,10, 4, 7,11/))
      cube_info(178) = cube_info_type (3,1,(/ 1, 2, 6, 1, 9,11, 6, 7,11, 0, 0, 0/))
      cube_info(179) = cube_info_type (3,1,(/ 2, 4, 6, 4, 6,11, 6, 7,11, 0, 0, 0/))
      cube_info(180) = cube_info_type (4,1,(/ 2, 3,10, 3,10,11, 6, 7,12, 9,10,11/))
      cube_info(181) = cube_info_type (3,1,(/ 1, 2,10, 3, 4,11, 6, 7,12, 0, 0, 0/))
      cube_info(182) = cube_info_type (3,1,(/ 1, 3, 9, 3, 9,11, 6, 7,12, 0, 0, 0/))
      cube_info(183) = cube_info_type (2,1,(/ 3, 4,11, 6, 7,12, 0, 0, 0, 0, 0, 0/))
      cube_info(184) = cube_info_type (3,1,(/ 3, 4, 7, 4, 9,10, 6, 7,10, 0, 0, 0/))
      cube_info(185) = cube_info_type (3,1,(/ 1, 3, 7, 1, 6, 7, 1, 6,10, 0, 0, 0/))
      cube_info(186) = cube_info_type (3,1,(/ 1, 4, 9, 2, 3, 6, 3, 6, 7, 0, 0, 0/))
      cube_info(187) = cube_info_type (2,1,(/ 2, 3, 6, 3, 6, 7, 0, 0, 0, 0, 0, 0/))
      cube_info(188) = cube_info_type (3,1,(/ 2, 4,10, 4, 9,10, 6, 7,12, 0, 0, 0/))
      cube_info(189) = cube_info_type (2,1,(/ 1, 2,10, 6, 7,12, 0, 0, 0, 0, 0, 0/))
      cube_info(190) = cube_info_type (2,1,(/ 1, 4, 9, 6, 7,12, 0, 0, 0, 0, 0, 0/))
      cube_info(191) = cube_info_type (1,1,(/ 6, 7,12, 0, 0, 0, 0, 0, 0, 0, 0, 0/))
      cube_info(192) = cube_info_type (2,1,(/ 6, 8,11, 6,11,12, 0, 0, 0, 0, 0, 0/))
      cube_info(193) = cube_info_type (3,1,(/ 1, 4, 9, 6, 8,12, 8,11,12, 0, 0, 0/))
      cube_info(194) = cube_info_type (3,1,(/ 1, 2,10, 6, 8,12, 8,11,12, 0, 0, 0/))
      cube_info(195) = cube_info_type (4,1,(/ 2, 4,12, 4,11,12, 6, 9,10, 6, 8, 9/))
      cube_info(196) = cube_info_type (3,1,(/ 2, 3,11, 2, 6, 8, 2, 8,11, 0, 0, 0/))
      cube_info(197) = cube_info_type (4,1,(/ 1, 2, 9, 2, 6, 8, 2, 8, 9, 3, 4,11/))
      cube_info(198) = cube_info_type (3,1,(/ 1, 3,10, 6, 8,10, 3, 8,11, 0, 0, 0/))
      cube_info(199) = cube_info_type (3,1,(/ 3, 4,11, 6, 8,10, 8, 9,10, 0, 0, 0/))
      cube_info(200) = cube_info_type (3,1,(/ 3, 4,12, 4, 6, 8, 4, 6,12, 0, 0, 0/))
      cube_info(201) = cube_info_type (3,1,(/ 1, 3, 9, 6, 8, 9, 3, 6,12, 0, 0, 0/))
      cube_info(202) = cube_info_type (4,1,(/ 1, 4, 6, 1, 6,10, 2, 3,12, 4, 6, 8/))
      cube_info(203) = cube_info_type (3,1,(/ 2, 3,12, 6, 8,10, 8, 9,10, 0, 0, 0/))
      cube_info(204) = cube_info_type (2,1,(/ 2, 4, 6, 4, 6, 8, 0, 0, 0, 0, 0, 0/))
      cube_info(205) = cube_info_type (3,1,(/ 1, 2, 9, 2, 6, 8, 2, 8, 9, 0, 0, 0/))
      cube_info(206) = cube_info_type (3,1,(/ 1, 4, 6, 1, 6,10, 4, 6, 8, 0, 0, 0/))
      cube_info(207) = cube_info_type (2,1,(/ 6, 8, 9, 6, 9,10, 0, 0, 0, 0, 0, 0/))
      cube_info(208) = cube_info_type (3,1,(/ 5, 6,12, 5, 9,12, 9,11,12, 0, 0, 0/))
      cube_info(209) = cube_info_type (3,1,(/ 1, 4, 5, 4,11,12, 5, 6,12, 0, 0, 0/))
      cube_info(210) = cube_info_type (4,1,(/ 1, 2,10, 5, 9,12, 5, 6,12, 9,11,12/))
      cube_info(211) = cube_info_type (3,1,(/ 2, 4,12, 4,11,12, 5, 6,10, 0, 0, 0/))
      cube_info(212) = cube_info_type (3,1,(/ 2, 3, 6, 5, 6, 9, 3, 9,11, 0, 0, 0/))
      cube_info(213) = cube_info_type (3,1,(/ 1, 2, 5, 2, 5, 6, 3, 4,11, 0, 0, 0/))
      cube_info(214) = cube_info_type (3,1,(/ 1, 3, 9, 3, 9,11, 5, 6,10, 0, 0, 0/))
      cube_info(215) = cube_info_type (2,1,(/ 3, 4,11, 5, 6,10, 0, 0, 0, 0, 0, 0/))
      cube_info(216) = cube_info_type (4,1,(/ 3, 4, 5, 4, 5, 6, 3, 6,12, 4, 5, 9/))
      cube_info(217) = cube_info_type (3,1,(/ 1, 3, 5, 3, 5,12, 5, 6,12, 0, 0, 0/))
      cube_info(218) = cube_info_type (3,1,(/ 1, 4, 9, 2, 3,12, 5, 6,10, 0, 0, 0/))
      cube_info(219) = cube_info_type (2,1,(/ 2, 3,12, 5, 6,10, 0, 0, 0, 0, 0, 0/))
      cube_info(220) = cube_info_type (3,1,(/ 2, 4, 6, 4, 5, 6, 4, 5, 9, 0, 0, 0/))
      cube_info(221) = cube_info_type (2,1,(/ 1, 2, 5, 2, 5, 6, 0, 0, 0, 0, 0, 0/))
      cube_info(222) = cube_info_type (2,1,(/ 1, 4, 9, 5, 6,10, 0, 0, 0, 0, 0, 0/))
      cube_info(223) = cube_info_type (1,1,(/ 5, 6,10, 0, 0, 0, 0, 0, 0, 0, 0, 0/))
      cube_info(224) = cube_info_type (3,1,(/ 5, 8,10, 8,10,11,10,11,12, 0, 0, 0/))
      cube_info(225) = cube_info_type (4,1,(/ 5, 8, 9,10,11,12, 1,10, 4, 4,10,11/))
      cube_info(226) = cube_info_type (3,1,(/ 1, 2, 5, 5, 8,11, 2,11,12, 0, 0, 0/))
      cube_info(227) = cube_info_type (3,1,(/ 2, 4,12, 4,11,12, 5, 8, 9, 0, 0, 0/))
      cube_info(228) = cube_info_type (4,1,(/ 2, 3, 5, 2, 5, 8, 2, 5,10, 3, 8,11/))
      cube_info(229) = cube_info_type (3,1,(/ 1, 2,10, 3, 4,11, 5, 8, 9, 0, 0, 0/))
      cube_info(230) = cube_info_type (3,1,(/ 1, 3, 5, 3, 5, 8, 3, 8,11, 0, 0, 0/))
      cube_info(231) = cube_info_type (2,1,(/ 3, 4,11, 5, 8, 9, 0, 0, 0, 0, 0, 0/))
      cube_info(232) = cube_info_type (3,1,(/ 3, 4,12, 4, 5, 8, 5,10,12, 0, 0, 0/))
      cube_info(233) = cube_info_type (3,1,(/ 1, 3,10, 3,10,12, 5, 8, 9, 0, 0, 0/))
      cube_info(234) = cube_info_type (3,1,(/ 1, 4, 5, 4, 5, 8, 2, 3,12, 0, 0, 0/))
      cube_info(235) = cube_info_type (2,1,(/ 2, 3,12, 5, 8, 9, 0, 0, 0, 0, 0, 0/))
      cube_info(236) = cube_info_type (3,1,(/ 2, 4, 8, 2, 8,10, 5, 8,10, 0, 0, 0/))
      cube_info(237) = cube_info_type (2,1,(/ 1, 2,10, 5, 8, 9, 0, 0, 0, 0, 0, 0/))
      cube_info(238) = cube_info_type (2,1,(/ 1, 4, 5, 4, 5, 8, 0, 0, 0, 0, 0, 0/))
      cube_info(239) = cube_info_type (1,1,(/ 5, 8, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0/))
      cube_info(240) = cube_info_type (2,1,(/ 9,10,11,10,11,12, 0, 0, 0, 0, 0, 0/))
      cube_info(241) = cube_info_type (3,1,(/ 1, 4,11, 1,10,11,10,11,12, 0, 0, 0/))
      cube_info(242) = cube_info_type (3,1,(/ 1, 2, 9, 2, 9,12, 9,11,12, 0, 0, 0/))
      cube_info(243) = cube_info_type (2,1,(/ 2, 4,11, 2,11,12, 0, 0, 0, 0, 0, 0/))
      cube_info(244) = cube_info_type (3,1,(/ 2, 3,10, 3,10,11, 9,10,11, 0, 0, 0/))
      cube_info(245) = cube_info_type (2,1,(/ 1, 2,10, 3, 4,11, 0, 0, 0, 0, 0, 0/))
      cube_info(246) = cube_info_type (2,1,(/ 1, 3, 9, 3, 9,11, 0, 0, 0, 0, 0, 0/))
      cube_info(247) = cube_info_type (1,1,(/ 3, 4,11, 0, 0, 0, 0, 0, 0, 0, 0, 0/))
      cube_info(248) = cube_info_type (3,1,(/ 3, 4,12, 4, 9,12, 9,10,12, 0, 0, 0/))
      cube_info(249) = cube_info_type (2,1,(/ 1, 3,10, 3,10,12, 0, 0, 0, 0, 0, 0/))
      cube_info(250) = cube_info_type (2,1,(/ 1, 4, 9, 2, 3,12, 0, 0, 0, 0, 0, 0/))
      cube_info(251) = cube_info_type (1,1,(/ 2, 3,12, 0, 0, 0, 0, 0, 0, 0, 0, 0/))
      cube_info(252) = cube_info_type (2,1,(/ 2, 4, 9, 2, 9,10, 0, 0, 0, 0, 0, 0/))
      cube_info(253) = cube_info_type (1,1,(/ 1, 2,10, 0, 0, 0, 0, 0, 0, 0, 0, 0/))
      cube_info(254) = cube_info_type (1,1,(/ 1, 4, 9, 0, 0, 0, 0, 0, 0, 0, 0, 0/))
      cube_info(255) = cube_info_type (0,0,(/ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/))

   End Subroutine Set_Cube_Info

   !!----
   !!---- CALCULATE_MESH
   !!----    Calculate the 3D Contour
   !!----
   !!---- 31/05/2020
   !!
   Module Subroutine Calculate_Mesh(Rho,Ngrid,Nlevel,Levels,MC_Method,NPoints,Xyz,Limits,Step)
      !---- Arguments ----!
      real(kind=cp),    dimension(:,:,:),           intent(in)  :: Rho
      integer,          dimension(3),               intent(in)  :: ngrid
      integer,                                      intent(in)  :: nlevel
      real(kind=cp),    dimension(:),               intent(in)  :: Levels       ! dimension(nlevel)
      character(len=*),                             intent(in)  :: MC_Method
      integer,          dimension(:),               intent(out) :: NPoints      ! dimension(nlevel)
      real(kind=cp),    dimension(:,:),             intent(out) :: Xyz
      real(kind=cp),    dimension(2,3),   optional, intent(in)  :: Limits
      integer,          dimension(3),     optional, intent(in)  :: Step

      !---- Local Variables ----!
      character(len=2)               :: mc_char
      integer                        :: i,ii,j,k,n,lv,ntr
      integer                        :: ind,nelem,ncase
      integer                        :: nx_ini,nx_fin,ny_ini,ny_fin,nz_ini,nz_fin
      integer                        :: i1,j1,k1,i2,j2,k2,i3,j3,k3,i4,j4,k4
      integer                        :: i5,j5,k5,i6,j6,k6,i7,j7,k7,i8,j8,k8
      integer, dimension(8)          :: iv
      integer, dimension(10)         :: icode
      integer, dimension(3)          :: istep
      real(kind=cp)                  :: dx,dy,dz,dxs,dys,dzs
      real(kind=cp)                  :: den1,den2,den3,den4,den5,den6,den7,den8
      real(kind=cp)                  :: xmax,xmin,ymax,ymin,zmin,zmax
      real(kind=cp),dimension(3,10)  :: xp

      logical                        :: mc

      !> Init variables
      Npoints=0
      Xyz=0.0_cp

      call set_cube_info()

      dx=1.0_cp/real(ngrid(1))
      dy=1.0_cp/real(ngrid(2))
      dz=1.0_cp/real(ngrid(3))
      dxs=dx
      dys=dy
      dzs=dz
      istep=1

      if (present(limits)) then
         xmin=minval(limits(:,1))
         xmax=maxval(limits(:,1))
         ymin=minval(limits(:,2))
         ymax=maxval(limits(:,2))
         zmin=minval(limits(:,3))
         zmax=maxval(limits(:,3))
      else
         xmin=0.0_cp
         xmax=1.0_cp
         ymin=0.0_cp
         ymax=1.0_cp
         zmin=0.0_cp
         zmax=1.0_cp
      end if

      nx_ini=nint(xmin/dx)-1
      ny_ini=nint(ymin/dy)-1
      nz_ini=nint(zmin/dz)-1
      nx_fin=nint(xmax/dx)+1
      ny_fin=nint(ymax/dy)+1
      nz_fin=nint(zmax/dz)+1

      if (present(step)) then
         istep=step
         dx=istep(1)*dx
         dy=istep(2)*dy
         dz=istep(3)*dz
      end if
      mc_char=adjustl(l_case(mc_method))
      mc=.false.
      if (mc_char=="tr") mc=.true.

      ntr=0
      do lv=1,nlevel
         do i=nx_ini,nx_fin,istep(1)
            do j=ny_ini,ny_fin,istep(2)
               do k=nz_ini,nz_fin,istep(3)

                  i1=i
                  j1=j
                  k1=k
                  i1=mod(i1+4*ngrid(1),ngrid(1))
                  if (i1 == 0) i1=1
                  j1=mod(j1+4*ngrid(2),ngrid(2))
                  if (j1 == 0) j1=1
                  k1=mod(k1+4*ngrid(3),ngrid(3))
                  if (k1 == 0) k1=1

                  i2=i+istep(1)
                  j2=j
                  k2=k
                  i2=mod(i2+4*ngrid(1),ngrid(1))
                  if (i2 == 0) i2=1
                  j2=mod(j2+4*ngrid(2),ngrid(2))
                  if (j2 == 0) j2=1
                  k2=mod(k2+4*ngrid(3),ngrid(3))
                  if (k2 == 0) k2=1

                  i3=i+istep(1)
                  j3=j+istep(2)
                  k3=k
                  i3=mod(i3+4*ngrid(1),ngrid(1))
                  if (i3 == 0) i3=1
                  j3=mod(j3+4*ngrid(2),ngrid(2))
                  if (j3 == 0) j3=1
                  k3=mod(k3+4*ngrid(3),ngrid(3))
                  if (k3 == 0) k3=1

                  i4=i
                  j4=j+istep(2)
                  k4=k
                  i4=mod(i4+4*ngrid(1),ngrid(1))
                  if (i4 == 0) i4=1
                  j4=mod(j4+4*ngrid(2),ngrid(2))
                  if (j4 == 0) j4=1
                  k4=mod(k4+4*ngrid(3),ngrid(3))
                  if (k4 == 0) k4=1

                  i5=i
                  j5=j
                  k5=k+istep(3)
                  i5=mod(i5+4*ngrid(1),ngrid(1))
                  if (i5 == 0) i5=1
                  j5=mod(j5+4*ngrid(2),ngrid(2))
                  if (j5 == 0) j5=1
                  k5=mod(k5+4*ngrid(3),ngrid(3))
                  if (k5 == 0) k5=1

                  i6=i+istep(1)
                  j6=j
                  k6=k+istep(3)
                  i6=mod(i6+4*ngrid(1),ngrid(1))
                  if (i6 == 0) i6=1
                  j6=mod(j6+4*ngrid(2),ngrid(2))
                  if (j6 == 0) j6=1
                  k6=mod(k6+4*ngrid(3),ngrid(3))
                  if (k6 == 0) k6=1

                  i7=i+istep(1)
                  j7=j+istep(2)
                  k7=k+istep(3)
                  i7=mod(i7+4*ngrid(1),ngrid(1))
                  if (i7 == 0) i7=1
                  j7=mod(j7+4*ngrid(2),ngrid(2))
                  if (j7 == 0) j7=1
                  k7=mod(k7+4*ngrid(3),ngrid(3))
                  if (k7 == 0) k7=1

                  i8=i
                  j8=j+istep(2)
                  k8=k+istep(3)
                  i8=mod(i8+4*ngrid(1),ngrid(1))
                  if (i8 == 0) i8=1
                  j8=mod(j8+4*ngrid(2),ngrid(2))
                  if (j8 == 0) j8=1
                  k8=mod(k8+4*ngrid(3),ngrid(3))
                  if (k8 == 0) k8=1

                  den1=Rho(i1,j1,k1)
                  den2=Rho(i2,j2,k2)
                  den3=Rho(i3,j3,k3)
                  den4=Rho(i4,j4,k4)
                  den5=Rho(i5,j5,k5)
                  den6=Rho(i6,j6,k6)
                  den7=Rho(i7,j7,k7)
                  den8=Rho(i8,j8,k8)

                  iv=0
                  if (den1 >= levels(lv)) iv(1)=1
                  if (den2 >= levels(lv)) iv(2)=1
                  if (den3 >= levels(lv)) iv(3)=1
                  if (den4 >= levels(lv)) iv(4)=1
                  if (den5 >= levels(lv)) iv(5)=1
                  if (den6 >= levels(lv)) iv(6)=1
                  if (den7 >= levels(lv)) iv(7)=1
                  if (den8 >= levels(lv)) iv(8)=1

                  ind=index_cube(iv,mc)

                  nelem=cube_info(ind)%nelem
                  if (nelem == 0) cycle
                  ncase=cube_info(ind)%code
                  icode=0
                  xp=0.0
                  select case (ncase)
                     case (1) ! Triangle
                        do n=1,nelem
                           if (ntr+3 > MAX_POINTS) then
                              err_CFML%Ierr=1
                              err_CFML%Msg="The number of points (ntr+3) is higher than Max_Points: 500000!"
                              exit
                           end if
                           do ii=1,3
                              icode(ii)=cube_info(ind)%edges(3*(n-1)+ii)
                              xp(:,ii)=vertice_point(icode(ii))

                              npoints(lv)=npoints(lv)+1
                              ntr=ntr+1
                              xyz(1,ntr)=(i-1)*dxs + xp(1,ii)*dx
                              xyz(2,ntr)=(j-1)*dys + xp(2,ii)*dy
                              xyz(3,ntr)=(k-1)*dzs + xp(3,ii)*dz
                              xyz(4,ntr)=real(ncase)
                           end do
                        end do

                     case (2) ! Trapezoide
                        do n=1,nelem
                           if (ntr+4 > Max_Points) then
                              err_CFML%Ierr=1
                              err_CFML%Msg="The number of points (ntr+4) is higher than Max_Points: 500000!"
                              exit
                           end if
                           do ii=1,4
                              icode(ii)=cube_info(ind)%edges(4*(n-1)+ii)
                              xp(:,ii)=vertice_point(icode(ii))

                              npoints(lv)=npoints(lv)+1
                              ntr=ntr+1
                              xyz(1,ntr)=(i-1)*dxs + xp(1,ii)*dx
                              xyz(2,ntr)=(j-1)*dys + xp(2,ii)*dy
                              xyz(3,ntr)=(k-1)*dzs + xp(3,ii)*dz
                              xyz(4,ntr)=real(ncase)
                           end do
                        end do

                     case (3) ! Triangle + Trapezoide
                        do n=1,nelem
                           if (ntr+7 > Max_Points) then
                              err_CFML%Ierr=1
                              err_CFML%Msg="The number of points (ntr+7) is higher than Max_Points: 500000!"
                              exit
                           end if
                           do ii=1,7
                              icode(ii)=cube_info(ind)%edges(7*(n-1)+ii)
                              xp(:,ii)=vertice_point(icode(ii))

                              npoints(lv)=npoints(lv)+1
                              ntr=ntr+1
                              xyz(1,ntr)=(i-1)*dxs + xp(1,ii)*dx
                              xyz(2,ntr)=(j-1)*dys + xp(2,ii)*dy
                              xyz(3,ntr)=(k-1)*dzs + xp(3,ii)*dz
                              xyz(4,ntr)=real(ncase)
                           end do
                        end do

                     case (4) ! Triangle + Triangle + Trapezoide
                        do n=1,nelem
                           if (ntr+10 > Max_Points) then
                              err_CFML%Ierr=1
                              err_CFML%Msg="The number of points (ntr+10) is higher than Max_Points: 500000!"
                              exit
                           end if
                           do ii=1,10
                              icode(ii)=cube_info(ind)%edges(10*(n-1)+ii)
                              xp(:,ii)=vertice_point(icode(ii))

                              npoints(lv)=npoints(lv)+1
                              ntr=ntr+1
                              xyz(1,ntr)=(i-1)*dxs + xp(1,ii)*dx
                              xyz(2,ntr)=(j-1)*dys + xp(2,ii)*dy
                              xyz(3,ntr)=(k-1)*dzs + xp(3,ii)*dz
                              xyz(4,ntr)=real(ncase)
                           end do
                        end do

                     case (5) ! Triangle + Line
                        do n=1,nelem
                           if (ntr+4 > Max_Points) then
                              err_CFML%Ierr=1
                              err_CFML%Msg="The number of points (ntr+4) is higher than Max_Points: 500000!"
                              exit
                           end if
                           do ii=1,4
                              icode(ii)=cube_info(ind)%edges(4*(n-1)+ii)
                              xp(:,ii)=vertice_point(icode(ii))

                              npoints(lv)=npoints(lv)+1
                              ntr=ntr+1
                              xyz(1,ntr)=(i-1)*dxs + xp(1,ii)*dx
                              xyz(2,ntr)=(j-1)*dys + xp(2,ii)*dy
                              xyz(3,ntr)=(k-1)*dzs + xp(3,ii)*dz
                              xyz(4,ntr)=real(ncase)
                           end do
                        end do

                     case (6) ! Triangle + Triangle + Line
                        do n=1,nelem
                           if (ntr+7 > Max_Points) then
                              err_CFML%Ierr=1
                              err_CFML%Msg="The number of points (ntr+7) is higher than Max_Points: 500000!"
                              exit
                           end if
                           do ii=1,7
                              icode(ii)=cube_info(ind)%edges(7*(n-1)+ii)
                              xp(:,ii)=vertice_point(icode(ii))

                              npoints(lv)=npoints(lv)+1
                              ntr=ntr+1
                              xyz(1,ntr)=(i-1)*dxs + xp(1,ii)*dx
                              xyz(2,ntr)=(j-1)*dys + xp(2,ii)*dy
                              xyz(3,ntr)=(k-1)*dzs + xp(3,ii)*dz
                              xyz(4,ntr)=real(ncase)
                           end do
                        end do

                  end select

               end do
            end do
         end do

      end do ! lv

   End Subroutine Calculate_Mesh

End SubModule Maps_Marching_Cubes