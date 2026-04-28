!!----
!!----
!!----
SubModule (CFML_Reflections) Refl_Conditions
   implicit none
   Contains

   !!--++
   !!--++ SEARCH_EXTINTIONS_IUNIT
   !!--++    (Overloaded)
   !!--++    Write information about the Reflections Extintion for SpaceGroup
   !!--++
   !!--++ 24/06/2019
   !!
   Module Subroutine Search_Extinctions_Iunit(SpG, Iunit)
      !---- Arguments ----!
      class(SpG_Type),   intent(in) :: SpG
      integer, optional, intent(in) :: Iunit

      if (.not. hkl_ref_cond_ini) then
         call Init_Refl_Conditions()
         hkl_ref_cond_ini=.true.
      end if

      if (present(Iunit)) then
         call write_integral_conditions(SpG, iunit)
         call write_glide_planes_conditions(SpG,iunit)
         call write_screw_axis_conditions(SpG, iunit)
      else
         call write_integral_conditions(SpG)
         call write_glide_planes_conditions(SpG)
         call write_screw_axis_conditions(SpG)
      end if

   End Subroutine Search_Extinctions_Iunit

   !!--++
   !!--++ SEARCH_EXTINCTIONS_FILE
   !!--++    (Overloaded)
   !!--++    Write information about the Reflections Extintion for SpaceGroup
   !!--++    in filevar variable
   !!--++
   !!--++ 24/06/2019
   !!
   Module Subroutine Search_Extinctions_File(SpG, nlines, filevar)
      !---- Arguments ----!
      class(SpG_Type),                intent(in)   :: SpG
      integer,                        intent(out)  :: nlines
      character(len=*), dimension(:), intent(out)  :: filevar

      !---- Local Variables ----!
      integer            :: iunit,ierr
      character(len=132) :: line

      !> Init
      nlines=0
      filevar=' '

      !> Load Information
      if (.not. hkl_ref_cond_ini) then
         call Init_Refl_Conditions()
         hkl_ref_cond_ini=.true.
      end if

      open(newunit=iunit,file='search_extin_xyx.tmp')

      call write_integral_conditions(SpG,iunit)
      call write_glide_planes_conditions(SpG,iunit)
      call write_screw_axis_conditions(SpG,iunit)

      rewind(unit=iunit)
      do
         read(unit=iunit,fmt='(a)', iostat=ierr) line
         if (ierr /=0) exit
         nlines=nlines+1
         filevar(nlines)=trim(line)
      end do
      close(unit=iunit, status='delete')

   End Subroutine Search_Extinctions_File

   !!--++
   !!--++ INIT_REFL_CONDITIONS()
   !!--++    Initialize the Reflection conditions information array
   !!--++
   !!--++ 24/06/2019
   !!
   Module Subroutine Init_Refl_Conditions()

      Hkl_Ref_Conditions(1:20)(1:80)   = (/  &
            "(h k l)    h+k=2n : xy0 centred face (C)                                        " , &
            "(h k l)    k+l=2n : 0yz centred face (A)                                        " , &
            "(h k l)    h+l=2n : x0z centred face (B)                                        " , &
            "(h k l)  h+k+l=2n : body centred (I)                                            " , &
            "(h k l)  h,k,l same parity: all-face centred (F)                                " , &
            "(h k l) -h+k+l=3n : rhombohedrally centred (R)                                  " , &
            "(  0  k  l)     k=2n : (100) glide plane with b/2 translation (b)               " , &
            "(  0  k  l)     l=2n : (100) glide plane with c/2 translation (c)               " , &
            "(  0  k  l)   k+l=2n : (100) glide plane with b/2 + c/2 translations (n)        " , &
            "(  0  k  l)   k+l=4n : (100) glide plane with b/4 +- c/4 translations (d)       " , &
            "(  h  0  l)     h=2n : (010) glide plane with a/2 translation (a)               " , &
            "(  h  0  l)     l=2n : (010) glide plane with c/2 translation (c)               " , &
            "(  h  0  l)   l+h=2n : (010) glide plane with c/2 + a/2 translations (n)        " , &
            "(  h  0  l)   l+h=4n : (010) glide plane with c/4 +- a/4 translations (d)       " , &
            "(  h  k  0)     h=2n : (001) glide plane with a/2 translation (a)               " , &
            "(  h  k  0)     k=2n : (001) glide plane with b/2 translation (b)               " , &
            "(  h  k  0)   h+k=2n : (001) glide plane with a/2 + b/2 translations (n)        " , &
            "(  h  k  0)   h+k=4n : (001) glide plane with a/4 +- b/4 translations (d)       " , &
            "(  h  -h   0 l) l=2n : (11-20) glide plane with c/2 translation (c)             " , &
            "(  0   k  -k l) l=2n : (-2110) glide plane with c/2 translation (c)             " /)

      Hkl_Ref_Conditions(21:39)(1:80)   = (/  &
            "( -h   0   h l) l=2n : (1-210) glide plane with c/2 translation (c)             " , &
            "(  h   h -2h l) l=2n : (1-100) glide plane with c/2 translation (c)             " , &
            "(-2h   h   h l) l=2n : (01-10) glide plane with c/2 translation (c)             " , &
            "(  h -2h   h l) l=2n : (-1010) glide plane with c/2 translation (c)             " , &
            "(  h  h  l)     l=2n : (1-10) glide plane with c/2 translation (c,n)            " , &
            "(  h  k  k)     h=2n : (01-1) glide plane with a/2 translation (a,n)            " , &
            "(  h  k  h)     k=2n : (-101) glide plane with b/2 translation (b,n)            " , &
            "(  h  h  l)     l=2n : (1-10) glide plane with c/2 translation (c,n)            " , &
            "(  h  h  l)  2h+l=4n : (1-10) glide plane with a/4 +- b/4 +- c/4 translation (d)" , &
            "(  h -h  l)     l=2n : (110)  glide plane with c/2 translation (c,n)            " , &
            "(  h -h  l)  2h+l=4n : (110)  glide plane with a/4 +- b/4 +- c/4 translation (d)" , &
            "(  h  k  k)     h=2n : (01-1) glide plane with a/2 translation (a,n)            " , &
            "(  h  k  k)  2k+h=4n : (01-1) glide plane with +-a/4 + b/4 +- c/4 translation(d)" , &
            "(  h  k -k)     h=2n : (011)  glide plane with a/2 translation (a,n)            " , &
            "(  h  k -k)  2k+h=4n : (011)  glide plane with +-a/4 + b/4 +- c/4 translation(d)" , &
            "(  h  k  h)     k=2n : (-101) glide plane with b/2 translation (b,n)            " , &
            "(  h  k  h)  2h+k=4n : (-101) glide plane with +-a/4 +- b/4 + c/4 translation(d)" , &
            "( -h  k  h)     k=2n : (101)  glide plane with b/2 translation (b,n)            " , &
            "( -h  k  h)  2h+k=4n : (101)  glide plane with +-a/4 +- b/4 + c/4 translation(d)" /)

        Hkl_Ref_Conditions(40:58)(1:80)   = (/  &
            "(h 0 0)      h=2n : screw axis // [100] with  a/2 translation (21)              " , & ! monoclinic, ortho., tetra and cubic
            "(h 0 0)      h=2n : screw axis // [100] with 2a/4 translation (42)              " , & ! cubic
            "(h 0 0)      h=4n : screw axis // [100] with  a/4 translation (41)              " , & ! cubic
            "(h 0 0)      h=4n : screw axis // [100] with 3a/4 translation (43)              " , & ! cubic
            "(0 k 0)      k=2n : screw axis // [010] with  b/2 translation (21)              " , & ! monoclinic, ortho., tetra and cubic
            "(0 k 0)      k=2n : screw axis // [010] with 2b/4 translation (42)              " , & ! cubic
            "(0 k 0)      k=4n : screw axis // [010] with  b/4 translation (41)              " , & ! cubic
            "(0 k 0)      k=4n : screw axis // [010] with 3b/4 translation (43)              " , & ! cubic
            "(0 0 l)      l=2n : screw axis // [00l] with  c/2 translation (21)              " , & ! monoclinic, ortho., tetra and cubic
            "(0 0 l)      l=2n : screw axis // [00l] with 2c/4 translation (42)              " , & ! tetragonal and cubic
            "(0 0 l)      l=4n : screw axis // [00l] with  c/4 translation (41)              " , & ! tetragonal and cubic
            "(0 0 l)      l=4n : screw axis // [00l] with 3c/4 translation (43)              " , & ! tetragonal and cubic
            "(0 0 0 l)    l=2n : screw axis // [00l] axis with 3c/6 translation (63)         " , &
            "(0 0 0 l)    l=3n : screw axis // [00l] axis with  c/3 translation (31)         " , &
            "(0 0 0 l)    l=3n : screw axis // [00l] axis with 2c/3 translation (32)         " , &
            "(0 0 0 l)    l=3n : screw axis // [00l] axis with 2c/6 translation (62)         " , &
            "(0 0 0 l)    l=3n : screw axis // [00l] axis with 4c/6 translation (64)         " , &
            "(0 0 0 l)    l=6n : screw axis // [00l] axis with  c/6 translation (61)         " , &
            "(0 0 0 l)    l=6n : screw axis // [00l] axis with 5c/6 translation (65)         " /)

   End Subroutine Init_Refl_Conditions

   !!--++
   !!--++ WRITE_GLIDE_PLANES_CONDITIONS
   !!--++    Reflections Conditions according with I.T. Table 2.2.13.2
   !!--++    space.
   !!--++
   !!--++ 24/06/2019
   !!
   Module Subroutine Write_Glide_Planes_Conditions(SpG,Iunit)
      !---- Arguments ----!
      class(SpG_Type),    intent(in) :: SpG
      integer, optional,  intent(in) :: Iunit

      !---- Local variables ----!
      integer, dimension(3) :: hh
      integer               :: h, k, l, m
      integer               :: n, n_ext
      integer               :: num_exti
      logical               :: zonal_condition

      zonal_condition   = .false.

      if (present(iunit) ) then
         write(unit=iunit,fmt=*) " "
         write(unit=iunit,fmt=*) " >>> Zonal reflections conditions for glide planes:"
         write(unit=iunit,fmt=*) "---------------------------------------------------"
         write(unit=iunit,fmt=*) " "
      end if

      !GLIDE PLANES and screw axes: table 2.13.2
      !-------------
      !
      !        0 k l:    k=2n    b/2             monoclinic, orthorhombic, tetragonal and cubic
      !        0 k l:    l=2n    c/2             monoclinic, orthorhombic, tetragonal and cubic
      !        0 k l:  k+l=2n    b/2 +  c/2      monoclinic, orthorhombic, tetragonal and cubic
      !        0 k l:  k+l=4n    b/4 +- c/4      orthorhombic and cubic
      !
      !
      !        h 0 l:    h=2n    a/2             monoclinic, orthorhombic, tetragonal and cubic
      !        h 0 l:    l=2n    c/2             monoclinic, orthorhombic, tetragonal and cubic
      !        h 0 l:  l+h=2n    c/2 +  a/2      monoclinic, orthorhombic, tetragonal and cubic
      !        h 0 l:  l+h=4n    c/4 +- a/4      orthorhombic and cubic
      !
      !        h k 0:    h=2n    a/2             monoclinic, orthorhombic, tetragonal and cubic
      !        h k 0:    k=2n    b/2             monoclinic, orthorhombic, tetragonal and cubic
      !        h k 0:  h+k=2n    a/2 +  b/2      monoclinic, orthorhombic, tetragonal and cubic
      !        h k 0:  h+k=4n    a/4 +- b/4      monoclinic, orthorhombic, tetragonal and cubic

      if (SpG%CrystalSys(1:10) == "Monoclinic"   .or. SpG%CrystalSys(1:10) == "Tetragonal" .or.     &
          SpG%CrystalSys(1:12) == "Orthorhombic" .or. SpG%CrystalSys(1:5)  == "Cubic") then

         !> glide plane b/2:
         ! Hkl_Ref_Conditions(7)  =   "(0 k l)      k=2n : 0yz glide plane with b/2 translation"
         num_exti = 7
         n = 0       ! nombre de reflections pouvant obeir a la regle d"extinction
         n_ext = 0   ! nombre de reflecions obeissant a la regle
         do k=-6, 6
            do l=-6, 6
               hh(1)=0
               hh(2)=k
               hh(3)=l
               m =  k
               if (m /= int(m/2)*2) then
                  n=n+1
                  if (h_absent(hh,SpG)) n_ext=n_ext+1
               end if
            end do   ! l loop
         end do    ! k loop
         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                Hkl_Ref_Conditions(num_exti)
            zonal_condition = .true.
         end if

         !> glide plane c/2:
         ! Hkl_Ref_Conditions(8)  =   "(0 k l)      l=2n : 0yz glide plane with c/2 translation"
         num_exti = 8
         n = 0       ! nombre de reflections pouvant obeir a la regle d"extinction
         n_ext = 0   ! nombre de reflecions obeissant a la regle
         do k=-6, 6
            do l=-6, 6
               hh(1)=0
               hh(2)=k
               hh(3)=l
               m =  l
               if (m /= int(m/2)*2) then
                  n=n+1
                  if (h_absent(hh,SpG)) n_ext=n_ext+1
               end if
            end do   ! l loop
         end do    ! k loop
         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                Hkl_Ref_Conditions(num_exti)
            zonal_condition = .true.
         end if

         !> glide plane b/2 + c/2:
         ! Hkl_Ref_Conditions(9)  =   "(0 k l)    k+l=2n : 0yz glide plane with b/2 + c/2 translation"
         num_exti = 9
         n = 0       ! nombre de reflections pouvant obeir a la regle d"extinction
         n_ext = 0   ! nombre de reflecions obeissant a la regle
         do k=-6, 6
            do l=-6, 6
               hh(1)=0
               hh(2)=k
               hh(3)=l
               m =  k+l
               if (m /= int(m/2)*2) then
                  n=n+1
                  if (h_absent(hh,SpG)) n_ext=n_ext+1
               end if
            end do   ! l loop
         end do    ! k loop
         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                Hkl_Ref_Conditions(num_exti)
            zonal_condition = .true.
         end if
      end if    ! fin de la condition "if monoclinic, tetragonal, ortho, cubic


      if (SpG%CrystalSys(1:12) == "Orthorhombic" .or. SpG%CrystalSys(1:5)  == "Cubic") then
         !> glide plane b/4 + c/4:
         ! Hkl_Ref_Conditions(10)  =   "(0 k l)    k+l=4n : 0yz glide plane with b/4 +- c/4 translation"
         num_exti = 10
         n = 0       ! nombre de reflections pouvant obeir a la regle d"extinction
         n_ext = 0   ! nombre de reflecions obeissant a la regle
         do k=-6, 6, 1
            do l=-6, 6, 1
               hh(1)=0
               hh(2)=k
               hh(3)=l
               m =  k+l
               if (m /= int(m/4)*4) then
                  n=n+1
                  if (h_absent(hh,SpG)) n_ext=n_ext+1
               end if
            end do   ! l loop
         end do    ! k loop
         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                Hkl_Ref_Conditions(num_exti)
            zonal_condition = .true.
         end if
      end if ! fin de la condition "if ortho, cubic

      if (SpG%CrystalSys(1:10) == "Monoclinic"   .or. SpG%CrystalSys(1:10) == "Tetragonal" .or.     &
         SpG%CrystalSys(1:12) == "Orthorhombic" .or. SpG%CrystalSys(1:5)  == "Cubic") then

         !>- glide plane a/2:
         !  Hkl_Ref_Conditions(11)  =   "(h 0 l)      h=2n : x0z glide plane with a/2 translation"
         num_exti = 11
         n = 0       ! nombre de reflections pouvant obeir a la regle d"extinction
         n_ext = 0   ! nombre de reflecions obeissant a la regle
         do h=-6, 6
            do l=-6, 6
               hh(1)=h
               hh(2)=0
               hh(3)=l
               m =  h
               if (m /= int(m/2)*2) then
                  n=n+1
                  if (h_absent(hh,SpG)) n_ext=n_ext+1
               end if
            end do   ! l loop
         end do     ! h loop
         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                Hkl_Ref_Conditions(num_exti)
            zonal_condition = .true.
         end if

         !> glide plane c/2:
         ! Hkl_Ref_Conditions(12) =   "(h 0 l)      l=2n : x0z glide plane with c/2 translation"
         num_exti = 12
         n = 0       ! nombre de reflections pouvant obeir a la regle d"extinction
         n_ext = 0   ! nombre de reflecions obeissant a la regle
         do h=-6, 6
            do l=-6, 6
               hh(1)=h
               hh(2)=0
               hh(3)=l
               m =  l
               if (m /= int(m/2)*2) then
                  n=n+1
                  if (h_absent(hh,SpG)) n_ext=n_ext+1
               end if
            end do   ! l loop
         end do     ! h loop
         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                Hkl_Ref_Conditions(num_exti)
            zonal_condition = .true.
         end if

         !> glide plane c/2 + a/2:
         ! Hkl_Ref_Conditions(13) =   "(h 0 l)    l+h=2n : x0z glide plane with a/2 + c/2 translations"
         num_exti = 13
         n = 0       ! nombre de reflections pouvant obeir a la regle d"extinction
         n_ext = 0   ! nombre de reflecions obeissant a la regle
         do h=-6, 6
            do l=-6, 6
               hh(1)=h
               hh(2)=0
               hh(3)=l
               m =  h+l
               if (m /= int(m/2)*2) then
                  n=n+1
                  if (h_absent(hh,SpG)) n_ext=n_ext+1
               end if
            end do   ! l loop
         end do     ! h loop
         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                Hkl_Ref_Conditions(num_exti)
            zonal_condition = .true.
         end if
      end if  ! fin de la condition "if monoclinic, tetragonal, ortho, cubic

      if (SpG%CrystalSys(1:12) == "Orthorhombic" .or. SpG%CrystalSys(1:5)  == "Cubic") then

         !> glide plane c/4 + a/4:
         ! Hkl_Ref_Conditions(14) =   "(h 0 l)    l+h=4n : x0z glide plane with a/4 +- c/4 translations"
         num_exti = 14
         n = 0       ! nombre de reflections pouvant obeir a la regle d"extinction
         n_ext = 0   ! nombre de reflecions obeissant a la regle
         do h=-6, 6
            do l=-6, 6
               hh(1)=h
               hh(2)=0
               hh(3)=l
               m =  h+l
               if (m /= int(m/4)*4) then
                  n=n+1
                  if (h_absent(hh,SpG)) n_ext=n_ext+1
               end if
            end do   ! l loop
         end do     ! h loop
         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                Hkl_Ref_Conditions(num_exti)
            zonal_condition = .true.
         end if
      end if ! fin de la condition "if ortho, cubic

      if (SpG%CrystalSys(1:10) == "Monoclinic"   .or. SpG%CrystalSys(1:10) == "Tetragonal" .or.     &
         SpG%CrystalSys(1:12) == "Orthorhombic" .or. SpG%CrystalSys(1:5)  == "Cubic") then

         !> glide plane a/2:
         ! Hkl_Ref_Conditions(15) =   "(h k 0)      h=2n : xy0 glide plane with a/2 translation"
         num_exti = 15
         n = 0       ! nombre de reflections pouvant obeir a la regle d"extinction
         n_ext = 0   ! nombre de reflecions obeissant a la regle
         do h=-6, 6
            do k=-6, 6
               hh(1)=h
               hh(2)=k
               hh(3)=0
               m =  h
               if (m /= int(m/2)*2) then
                  n=n+1
                  if (h_absent(hh,SpG)) n_ext=n_ext+1
               end if
            end do    ! k loop
         end do     ! h loop
         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                Hkl_Ref_Conditions(num_exti)
            zonal_condition = .true.
         end if

         !> glide plane b/2:
         !Hkl_Ref_Conditions(16) =   "(h k 0)      k=2n : xy0 glide plane with b/2 translation"
         num_exti = 16
         n = 0       ! nombre de reflections pouvant obeir a la regle d"extinction
         n_ext = 0   ! nombre de reflecions obeissant a la regle
         do h=-6, 6
            do k=-6, 6
               hh(1)=h
               hh(2)=k
               hh(3)=0
               m =  k
               if (m /= int(m/2)*2) then
                  n=n+1
                 if (h_absent(hh,SpG)) n_ext=n_ext+1
               end if
            end do    ! k loop
         end do     ! h loop
         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                Hkl_Ref_Conditions(num_exti)
            zonal_condition = .true.
         end if

         !> glide plane a/2 + b/2:
         ! Hkl_Ref_Conditions(17) =   "(h k 0)    h+k=2n : xy0 glide plane with a/2 + b/2 translations"
         num_exti = 17
         n = 0       ! nombre de reflections pouvant obeir a la regle d"extinction
         n_ext = 0   ! nombre de reflecions obeissant a la regle
         do h=-6, 6
            do k=-6, 6
               hh(1)=h
               hh(2)=k
               hh(3)=0
               m =  h+k
               if (m /= int(m/2)*2) then
                  n=n+1
                  if (h_absent(hh,SpG)) n_ext=n_ext+1
               end if
            end do    ! k loop
         end do     ! h loop
         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                Hkl_Ref_Conditions(num_exti)
            zonal_condition = .true.
         end if
      end if  ! fin de la condition "if monoclinic, tetragonal, ortho, cubic

      if (SpG%CrystalSys(1:12) == "Orthorhombic" .or. SpG%CrystalSys(1:5)  == "Cubic") then
         !> glide plane a/4 + b/4:
         ! Hkl_Ref_Conditions(18) =   "(h k 0)    h+k=4n : xy0 glide plane with a/4 +- b/4 translations"
         num_exti = 18
         n = 0       ! nombre de reflections pouvant obeir a la regle d"extinction
         n_ext = 0   ! nombre de reflecions obeissant a la regle
         do h=-6, 6, 1
            do k=-6, 6, 1
               hh(1)=h
               hh(2)=k
               hh(3)=0
               m =  h+k
               if (m /= int(m/4)*4) then
                  n=n+1
                  if (h_absent(hh,SpG)) n_ext=n_ext+1
               end if
            end do    ! k loop
         end do     ! h loop
         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                Hkl_Ref_Conditions(num_exti)
            zonal_condition = .true.
         end if
      end if  ! fin de la condition "if ortho, cubic

      if (SpG%SPG_Lat == "h") then
         !> glide plane with c/2 translation: hexagonal
         !  Hkl_Ref_Conditions(19) =   "(  h  -h   0 l) l=2n : (11-20) glide plane with c/2 translation (c)"
         num_exti = 19
         n = 0
         n_ext = 0
         do h=-6, +6, 1
            do l=-6, +6, 1
               hh(1)=h
               hh(2)=-h
               hh(3)=l
               m=l
               if (m /=int(m/2)*2) then
                  n=n+1
                  if (h_absent(hh, SpG)) n_ext=n_ext+1
               end if
            end do  ! l loop
         end do   ! h loop
         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                Hkl_Ref_Conditions(num_exti)
            zonal_condition = .true.
         end if

         !> glide plane with c/2 translation: hexagonal
         !  Hkl_Ref_Conditions(20) =   "(  0   k  -k l) l=2n : (-2110) glide plane with c/2 translation (c)"
         num_exti = 20
         n = 0
         n_ext = 0
         do k=-6, +6, 1
            do l=-6, +6, 1
               hh(1)=0
               hh(2)=k
               hh(3)=l
               m=l
               if (m /=int(m/2)*2) then
                  n=n+1
                  if (h_absent(hh, SpG)) n_ext=n_ext+1
               end if
            end do  ! l loop
         end do   ! h loop
         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                Hkl_Ref_Conditions(num_exti)
            zonal_condition = .true.
         end if

         !> glide plane with c/2 translation: hexagonal
         !Hkl_Ref_Conditions(21) =   "( -h   0   h l) l=2n : (1-210) glide plane with c/2 translation (c)"
         num_exti = 21
         n = 0
         n_ext = 0
         do h=-6, 6
            do l=-6, 6
               hh(1)=-h
               hh(2)=0
               hh(3)=l
               m=l
               if (m /=int(m/2)*2) then
                  n=n+1
                  if (h_absent(hh, SpG)) n_ext=n_ext+1
               end if
            end do  ! l loop
         end do   ! h loop
         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                Hkl_Ref_Conditions(num_exti)
            zonal_condition = .true.
         end if

         !> glide plane with c/2 translation: hexagonal
         ! Hkl_Ref_Conditions(22) =   "(  h   h -2h l) l=2n : (1-100) glide plane with c/2 translation (c)"
         num_exti = 22
         n = 0
         n_ext = 0
         do h=-6, +6, 1
            do l=-6, +6, 1
               hh(1)=h
               hh(2)=h
               hh(3)=l
               m=l
               if (m /=int(m/2)*2) then
                  n=n+1
                  if (h_absent(hh, SpG)) n_ext=n_ext+1
               end if
            end do  ! l loop
         end do   ! h loop
         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                Hkl_Ref_Conditions(num_exti)
            zonal_condition = .true.
         end if

         !> glide plane with c/2 translation: hexagonal
         !  Hkl_Ref_Conditions(23) =   "(-2h   h   h l) l=2n : (01-10) glide plane with c/2 translation (c)"
         num_exti = 23
         n = 0
         n_ext = 0
         do h=-6, +6, 1
            do l=-6, +6, 1
               hh(1)=-2*h
               hh(2)=h
               hh(3)=l
               m=l
               if (m /=int(m/2)*2) then
                  n=n+1
                  if (h_absent(hh, SpG)) n_ext=n_ext+1
               end if
            end do  ! l loop
         end do   ! h loop
         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                Hkl_Ref_Conditions(num_exti)
            zonal_condition = .true.
         end if

         !> glide plane with c/2 translation: hexagonal
         !  Hkl_Ref_Conditions(24) =   "(  h -2h   h l) l=2n : (-1010) glide plane with c/2 translation (c)"
         num_exti = 24
         n = 0
         n_ext = 0
         do h=-6, +6, 1
            do l=-6, +6, 1
               hh(1)=h
               hh(2)=-2*h
               hh(3)=l
               m=l
               if (m /=int(m/2)*2) then
                  n=n+1
                  if (h_absent(hh, SpG)) n_ext=n_ext+1
               end if
            end do  ! l loop
         end do   ! h loop
         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                Hkl_Ref_Conditions(num_exti)
            zonal_condition = .true.
         end if
      end if ! fin de la condition if hexagonal

      !25: glide plane with c/2 translation: rhomboedral
      !  Hkl_Ref_Conditions(25) =  "(  h  h  l) l=2n : (1-10) glide plane with c/2 translation (c,n)"
      num_exti = 25
      n = 0
      n_ext = 0
      do h=-6, +6, 1
         do l=-6, +6, 1
            hh(1)=h
            hh(2)=h
            hh(3)=l
            m=l
            if (m /=int(m/2)*2) then
               n=n+1
               if (h_absent(hh, SpG)) n_ext=n_ext+1
            end if
         end do  ! l loop
      end do   ! h loop
      if (n==n_ext) then
         if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                             Hkl_Ref_Conditions(num_exti)
         zonal_condition = .true.
      end if

      !> glide plane with c/2 translation: rhomboedral
      !  Hkl_Ref_Conditions(26) =  "(  h  k  k) h=2n : (01-1) glide plane with a/2 translation (a,n)"
      num_exti = 26
      n = 0
      n_ext = 0
      do h=-6, +6, 1
         do k=-6, +6, 1
            hh(1)=h
            hh(2)=k
            hh(3)=k
            m=h
            if (m /=int(m/2)*2) then
               n=n+1
               if (h_absent(hh, SpG)) n_ext=n_ext+1
            end if
         end do  ! l loop
      end do   ! h loop
      if (n==n_ext) then
         if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                             Hkl_Ref_Conditions(num_exti)
         zonal_condition = .true.
      end if

      !27: glide plane with c/2 translation: rhomboedral
      !  Hkl_Ref_Conditions(27) =  "(  h  k  h) k=2n : (-101) glide plane with b/2 translation (b,n)"
      num_exti = 27
      n = 0
      n_ext = 0
      do h=-6, +6, 1
         do k=-6, +6, 1
            hh(1)=h
            hh(2)=k
            hh(3)=h
            m=k
            if (m /=int(m/2)*2) then
               n=n+1
               if (h_absent(hh, SpG)) n_ext=n_ext+1
            end if
         end do  ! l loop
      end do   ! h loop
      if (n==n_ext) then
         if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                             Hkl_Ref_Conditions(num_exti)
         zonal_condition = .true.
      end if

      if (SpG%CrystalSys(1:10) == "Tetragonal" .or. SpG%CrystalSys(1:5) == "Cubic") then
         !> glide plane with c/2 translation: tetragonal + cubic
         !  Hkl_Ref_Conditions(28) =  "(  h  h  l)    l=2n : (1-10) glide plane with c/2 translation (c,n)"
         num_exti = 28
         n = 0
         n_ext = 0
         do h=-6, +6, 1
            do l=-6, +6, 1
               hh(1)=h
               hh(2)=h
               hh(3)=l
               m=l
               if (m /=int(m/2)*2) then
                  n=n+1
                  if (h_absent(hh, SpG)) n_ext=n_ext+1
               end if
            end do  ! l loop
         end do   ! h loop
         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                Hkl_Ref_Conditions(num_exti)
            zonal_condition = .true.
         end if

         !> glide plane with a/4 +- b/4 +- c/4 translation: tetragonal + cubic
         !  Hkl_Ref_Conditions(29) =  "(  h  h  l) 2h+l=4n : (1-10) glide plane with a/4 +- b/4 +- c/4 translation (d)"
         num_exti = 29
         n = 0
         n_ext = 0
         do h=-6, +6, 1
            do l=-6, +6, 1
               hh(1)=h
               hh(2)=h
               hh(3)=l
               m=2*h+l
               if (m /=int(m/4)*4) then
                  n=n+1
                  if (h_absent(hh, SpG)) n_ext=n_ext+1
               end if
            end do  ! l loop
         end do   ! h loop
         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                Hkl_Ref_Conditions(num_exti)
            zonal_condition = .true.
         end if

         !30: glide plane with c/2 translation: tetragonal + cubic
         !  Hkl_Ref_Conditions(30) =  "(  h -h  l)    l=2n : (110)  glide plane with c/2 translation (c,n)"
         num_exti = 30
         n = 0
         n_ext = 0
         do h=-6, +6, 1
            do l=-6, +6, 1
               hh(1)=h
               hh(2)=-h
               hh(3)=l
               m=l
               if (m /=int(m/2)*2) then
                  n=n+1
                  if (h_absent(hh, SpG)) n_ext=n_ext+1
               end if
            end do  ! l loop
         end do   ! h loop
         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                Hkl_Ref_Conditions(num_exti)
            zonal_condition = .true.
         end if

         ! 31: glide plane with a/4 +- b/4 +- c/4 translation: tetragonal + cubic
         !  Hkl_Ref_Conditions(31) = "(  h -h  l) 2h+l=4n : (110)  glide plane with a/4 +- b/4 +- c/4 translation (d)"
         num_exti = 31
         n = 0
         n_ext = 0
         do h=-6, +6, 1
            do l=-6, +6, 1
               hh(1)=h
               hh(2)=-h
               hh(3)=l
               m=2*h+l
               if (m /=int(m/4)*4) then
                  n=n+1
                  if (h_absent(hh, SpG)) n_ext=n_ext+1
               end if
            end do  ! l loop
         end do   ! h loop
         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                Hkl_Ref_Conditions(num_exti)
            zonal_condition = .true.
         end if
      end if   ! fin de la condition "if tetragonal .or. cubic

      if (SpG%CrystalSys(1:5) == "Cubic") then
         !> glide plane with a/2 translation: tetragonal + cubic
         !  Hkl_Ref_Conditions(32) = "(  h  k  k)    h=2n : (01-1) glide plane with a/2 translation (a,n)"
         num_exti = 32
         n = 0
         n_ext = 0
         do h=-6, +6, 1
            do k=-6, +6, 1
               hh(1)=h
               hh(2)=k
               hh(3)=k
               m=h
               if (m /=int(m/2)*2) then
                  n=n+1
                  if (h_absent(hh, SpG)) n_ext=n_ext+1
               end if
            end do  ! l loop
         end do   ! h loop
         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                Hkl_Ref_Conditions(num_exti)
            zonal_condition = .true.
         end if

         !> glide plane with +-a/4 +- b/4 +- c/4 translation: tetragonal + cubic
         !  Hkl_Ref_Conditions(33) = "(  h  k  k) 2k+h=4n : (01-1) glide plane with +-a/4 + b/4 +- c/4 translation (d)"
         num_exti = 33
         n = 0
         n_ext = 0
         do h=-6, +6, 1
            do k=-6, +6, 1
               hh(1)=h
               hh(2)=k
               hh(3)=k
               m=2*k+h
               if (m /=int(m/4)*4) then
                  n=n+1
                  if (h_absent(hh, SpG)) n_ext=n_ext+1
               end if
            end do  ! l loop
         end do   ! h loop
         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                Hkl_Ref_Conditions(num_exti)
            zonal_condition = .true.
         end if

         !34: glide plane with a/2 translation: tetragonal + cubic
         !  Hkl_Ref_Conditions(34) =  "(  h  k -k)    h=2n : (011)  glide plane with a/2 translation (a,n)"
         num_exti = 34
         n = 0
         n_ext = 0
         do h=-6, +6, 1
            do k=-6, +6, 1
               hh(1)=h
               hh(2)=k
               hh(3)=-k
               m=h
               if (m /=int(m/2)*2) then
                  n=n+1
                  if (h_absent(hh, SpG)) n_ext=n_ext+1
               end if
            end do  ! l loop
         end do   ! h loop
         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                Hkl_Ref_Conditions(num_exti)
            zonal_condition = .true.
         end if

         !> 35: glide plane with a/4 +- b/4 +- c/4 translation: tetragonal + cubic
         !  Hkl_Ref_Conditions(351) = "(  h  k -k) 2k+h=4n : (011)  glide plane with +-a/4 + b/4 +- c/4 translation (d)"
         num_exti = 35
         n = 0
         n_ext = 0
         do h=-6, +6, 1
            do k=-6, +6, 1
               hh(1)=h
               hh(2)=k
               hh(3)=-k
               m=2*k+h
               if (m /=int(m/4)*4) then
                  n=n+1
                  if (h_absent(hh, SpG)) n_ext=n_ext+1
               end if
            end do  ! l loop
         end do   ! h loop
         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                Hkl_Ref_Conditions(num_exti)
            zonal_condition = .true.
         end if

         !36: glide plane with b/2 translation: tetragonal + cubic
         !  Hkl_Ref_Conditions(36) = "(  h  k  h)    k=2n : (-101) glide plane with b/2 translation (b,n)"
         num_exti = 36
         n = 0
         n_ext = 0
         do h=-6, +6, 1
            do k=-6, +6, 1
               hh(1)=h
               hh(2)=k
               hh(3)=h
               m=k
               if (m /=int(m/2)*2) then
                  n=n+1
                  if (h_absent(hh, SpG)) n_ext=n_ext+1
               end if
            end do  ! l loop
         end do   ! h loop
         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                Hkl_Ref_Conditions(num_exti)
            zonal_condition = .true.
         end if

         !37: glide plane with +-a/4 +- b/4 +- c/4 translation: tetragonal + cubic
         !  Hkl_Ref_Conditions(33) = "(  h  k  h) 2h+k=4n : (-101) glide plane with +-a/4 + b/4 +- c/4 translation (d)"
         num_exti = 37
         n = 0
         n_ext = 0
         do h=-6, +6, 1
            do k=-6, +6, 1
               hh(1)=h
               hh(2)=k
               hh(3)=h
               m=2*h+k
               if (m /=int(m/4)*4) then
                  n=n+1
                  if (h_absent(hh, SpG)) n_ext=n_ext+1
               end if
            end do  ! l loop
         end do   ! h loop
         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                Hkl_Ref_Conditions(num_exti)
            zonal_condition = .true.
         end if

         !38: glide plane with b/2 translation: tetragonal + cubic
         !  Hkl_Ref_Conditions(38) = "( -h  k  h)    k=2n : (101)  glide plane with b/2 translation (b,n)"
         num_exti = 38
         n = 0
         n_ext = 0
         do h=-6, +6, 1
            do k=-6, +6, 1
               hh(1)=-h
               hh(2)=k
               hh(3)=h
               m=k
               if (m /=int(m/2)*2) then
                  n=n+1
                  if (h_absent(hh, SpG)) n_ext=n_ext+1
               end if
            end do  ! l loop
         end do   ! h loop
         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                Hkl_Ref_Conditions(num_exti)
            zonal_condition = .true.
         end if

         !> 39: glide plane with a/4 +- b/4 +- c/4 translation: tetragonal + cubic
         !  Hkl_Ref_Conditions(39) = "( -h  k  h) 2h+k=4n : (101)  glide plane with +-a/4 + b/4 +- c/4 translation (d)"
         num_exti = 39
         n = 0
         n_ext = 0
         do h=-6, +6, 1
            do k=-6, +6, 1
               hh(1)=-h
               hh(2)=k
               hh(3)=h
               m=2*h+k
               if (m /=int(m/4)*4) then
                  n=n+1
                  if (h_absent(hh, SpG)) n_ext=n_ext+1
               end if
            end do  ! l loop
         end do   ! h loop
         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                Hkl_Ref_Conditions(num_exti)
            zonal_condition = .true.
         end if
      end if  ! fin de la condition "if cubic

      if (.not. zonal_condition)   then
         if (present(iunit)) write(unit=iunit,fmt=*) "     =====>>> no zonal reflection condition"
      end if

   End Subroutine Write_Glide_Planes_Conditions

   !!--++
   !!--++ INTEGRAL_CONDITIONS
   !!--++    Integral Conditions according with I.T. Table 2.2.13.1
   !!--++    space.
   !!--++
   !!--++ 24/06/2019
   !!
   Module Subroutine Write_Integral_Conditions(SpG,iunit)
      !---- Arguments ----!
      class(SpG_Type),    intent(in)  :: SpG
      integer, optional,  intent(in)  :: iunit

      !---- local variables ----!
      integer, dimension(3) :: hh
      integer               :: h, k,l, m
      integer               :: n, n_ext
      integer               :: num_exti
      logical               :: integral_condition

      integral_condition   = .false.

      ! 1.       h+k   = 2n                   C-face centred                      C
      ! 2.       k+l   = 2n                   A-face centred                      A
      ! 3.       h+l   = 2n                   B-face centred                      B
      ! 4.       h+k+l = 2n                   Body centred                        I
      !
      ! 5.       h+k   = 2n
      !      and k+l   = 2n
      !      and h+l   = 2n                   All-face centred                    F
      !     or h,k,l all odd
      !     or h,k,l all even
      !
      ! 6.      -h+k+l = 3n                   Rhombohedrally centred,             R
      !                                     obverse setting

      if (present(iunit)) then
         write(unit=iunit,fmt=*) " "
         write(unit=iunit,fmt=*) " >>> Integral reflections conditions for centred lattices:"
         write(unit=iunit,fmt=*) "----------------------------------------------------------"
         write(unit=iunit,fmt=*) " "
      end if

      !> C-face centred
      !  Hkl_Ref_Conditions(1) =   "(h k l)  h+k=2n           : xy0 centered base"
      num_exti = 1
      n = 0       ! nombre de reflections pouvant obeir a la regle d"extinction
      n_ext = 0   ! nombre de reflecions obeissant a la regle
      do h=-6, 6
         do k=-6, 6
            do l=-6, 6
               hh(1)=h
               hh(2)=k
               hh(3)=l
               m =  h+k
               if (m /= int(m/2)*2) then
                  n=n+1
                  if (h_absent(hh,SpG)) n_ext=n_ext+1
               end if
            end do   ! l loop
         end do    ! k loop
      end do     ! h loop
      if (n==n_ext) then
         if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                             Hkl_Ref_Conditions(num_exti)
         integral_condition = .true.
      end if

      !> A-face centred
      !   Hkl_Ref_Conditions(2) =   "(h k l)  k+l=2n           : 0yz centered base"
      num_exti = 2
      n = 0       ! nombre de reflections pouvant obeir a la regle d"extinction
      n_ext = 0   ! nombre de reflecions obeissant a la regle
      do h=-6, 6
         do k=-6, 6
            do l=-6, 6
               hh(1)=h
               hh(2)=k
               hh(3)=l
               m =  k+l
               if (m /= int(m/2)*2) then
                  n=n+1
                  if (h_absent(hh,SpG)) n_ext=n_ext+1
               end if
            end do   ! l loop
         end do    ! k loop
      end do     ! h loop
      if (n==n_ext) then
         if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                             Hkl_Ref_Conditions(num_exti)
         integral_condition = .true.
      end if

      !> B-face centred
      !  Hkl_Ref_Conditions(3) =   "(h k l)  h+l=2n           : x0z centered base"
      num_exti = 3
      n = 0       ! nombre de reflections pouvant obeir a la regle d"extinction
      n_ext = 0   ! nombre de reflecions obeissant a la regle
      do h=-6, 6
         do k=-6, 6
            do l=-6, 6
               hh(1)=h
               hh(2)=k
               hh(3)=l
               m =  h+l
               if (m /= int(m/2)*2) then
                  n=n+1
                  if (h_absent(hh,SpG)) n_ext=n_ext+1
               end if
            end do   ! l loop
         end do    ! k loop
      end do     ! h loop
      if (n==n_ext) then
         if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                             Hkl_Ref_Conditions(num_exti)
         integral_condition = .true.
      end if

      !> Body centred (I) ----!
      !  Hkl_Ref_Conditions(4) =   "(h k l)  h+k+l=2n         : body centred"
      num_exti = 4
      n = 0       ! nombre de reflections pouvant obeir a la regle d"extinction
      n_ext = 0   ! nombre de reflecions obeissant a la regle
      do h=-6, 6
         do k=-6, 6
            do l=-6, 6
               hh(1)=h
               hh(2)=k
               hh(3)=l
               m =  h+k+l
               if (m /= int(m/2)*2) then
                  n=n+1
                  if (h_absent(hh,SpG)) n_ext=n_ext+1
               end if
            end do   ! l loop
         end do    ! k loop
      end do     ! h loop
      if (n==n_ext) then
         if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                             Hkl_Ref_Conditions(num_exti)
         integral_condition = .true.
      end if

      !> all-face centred (F) ----!
      ! Hkl_Ref_Conditions(5) =   "(h k l)  h,k,l same parity: all-face centred"
      num_exti = 5
      n = 0       ! nombre de reflections pouvant obeir a la regle d"extinction
      n_ext = 0   ! nombre de reflecions obeissant a la regle
      do h=-6, 6
         do k=-6, 6
            do l=-6, 6
               hh(1)=h
               hh(2)=k
               hh(3)=l
               if (h /= int(h/2)*2 .and.  k /= int(k/2)*2 .and. l == int(l/2)*2 ) then
                  n=n+1
                  if (h_absent(hh,SpG)) n_ext=n_ext+1

               else if(h /= int(h/2)*2 .and.  k == int(k/2)*2 .and. l /= int(l/2)*2 ) then
                  n=n+1
                  if (h_absent(hh,SpG)) n_ext=n_ext+1

               else if(h == int(h/2)*2 .and.  k /= int(k/2)*2 .and. l /= int(l/2)*2 ) then
                  n=n+1
                  if (h_absent(hh,SpG)) n_ext=n_ext+1

               else if(h == int(h/2)*2 .and.  k == int(k/2)*2 .and. l /= int(l/2)*2 ) then
                  n=n+1
                  if (h_absent(hh,SpG)) n_ext=n_ext+1

               else if(h == int(h/2)*2 .and.  k /= int(k/2)*2 .and. l == int(l/2)*2 ) then
                  n=n+1
                  if (h_absent(hh,SpG)) n_ext=n_ext+1

               else if(h /= int(h/2)*2 .and.  k == int(k/2)*2 .and. l == int(l/2)*2 ) then
                  n=n+1
                  if (h_absent(hh,SpG)) n_ext=n_ext+1
               end if
            end do   ! l loop
         end do    ! k loop
      end do     ! h loop
      if (n==n_ext) then
         if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                             Hkl_Ref_Conditions(num_exti)
         integral_condition = .true.
      end if

      !>R network
      !  Hkl_Ref_Conditions(6) =   "(h k l) -h+k+l=3n         : Rhombohedrally centred (R)"
      num_exti = 6
      n = 0       ! nombre de reflections pouvant obeir a la regle d"extinction
      n_ext = 0   ! nombre de reflecions obeissant a la regle
      do h=-6, 6
         do k=-6, 6
            do l=-6, 6
               hh(1)=h
               hh(2)=k
               hh(3)=l
               m =  -h+k+l
               if (m /= int(m/3)*3) then
                  n=n+1
                  if (h_absent(hh,SpG)) n_ext=n_ext+1
               end if
            end do   ! l loop
         end do    ! k loop
      end do     ! h loop
      if (n==n_ext) then
         if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                             Hkl_Ref_Conditions(num_exti)
         integral_condition = .true.
      end if

      if (.not. integral_condition)   then
         if (present(iunit)) write(unit=iunit,fmt=*) "     =====>>> no general reflection condition"
      end if

   End Subroutine Write_Integral_Conditions

   !!--++
   !!--++ WRITE_SCREW_AXIS_CONDITIONS
   !!--++    Reflections conditions for Screw axes Table 2.2.13.2
   !!--++
   !!--++ 24/06/2019
   !!
   Module Subroutine Write_Screw_Axis_Conditions(SpG ,Iunit)
      !---- Arguments ----!
      class(SpG_Type),    intent(in) :: SpG
      integer, optional,  intent(in) :: Iunit

      !---- Local variables ----!
      integer, dimension(3) :: hh
      integer               :: h, k,l
      integer               :: n, n_ext
      integer               :: num_exti
      logical               :: serial_condition

      serial_condition   = .false.

      if (present(iunit)) then
         write(unit=iunit,fmt=*) " "
         write(unit=iunit,fmt=*) " >>> Serial reflections conditions for screw axes:"
         write(unit=iunit,fmt=*) "---------------------------------------------------"
         write(unit=iunit,fmt=*) " "
      end if

      !SCREW AXES:      33 extinctions

      if (SpG%CrystalSys(1:10) == "Monoclinic" .or. SpG%CrystalSys(1:12) == "Orthorhombic" .or.   &
         SpG%CrystalSys(1:10) == "Tetragonal" .or. SpG%CrystalSys(1:5)  == "Cubic" ) then

         ! Hkl_Ref_Conditions(40) =   "(h 0 0)      h=2n : screw axis // [100] with  a/2 translation (21)"   ! monoclinic, ortho., tetra or cubic
         num_exti = 40
         n = 0       ! nombre de reflections pouvant obeir a la regle d"extinction
         n_ext = 0   ! nombre de reflecions obeissant a la regle
         do h=-6, 6
            hh(1)=h
            hh(2)=0
            hh(3)=0
            if (h /= int(h/2)*2) then
               n=n+1
               if (h_absent(hh,SpG)) n_ext=n_ext+1
            end if
         end do     ! h loop
         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                Hkl_Ref_Conditions(num_exti)
            serial_condition = .true.
         end if
      end if ! fin de la condition "if monoclinic, ortho, tetragonal, cubic

      if (SpG%CrystalSys(1:5) == "Cubic") then
         ! 41
         ! Hkl_Ref_Conditions(41) =   "(h 0 0)      h=2n : screw axis // [100] with  2a/4 translation (42)"   !  cubic
         num_exti = 41
         n = 0       ! nombre de reflections pouvant obeir a la regle d"extinction
         n_ext = 0   ! nombre de reflecions obeissant a la regle
         do h=-6, 6, 1
            hh(1)=h
            hh(2)=0
            hh(3)=0
            if (h /= int(h/2)*2) then
               n=n+1
               if (h_absent(hh,SpG)) n_ext=n_ext+1
            end if
         end do     ! h loop
         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                Hkl_Ref_Conditions(num_exti)
            serial_condition = .true.
         end if

         ! Hkl_Ref_Conditions(42) =   "(h 0 0)      h=4n : screw axis // [100] with  a/4 translation (41)"   ! cubic
         ! Hkl_Ref_Conditions(43) =   "(h 0 0)      h=4n : screw axis // [100] with 3a/4 translation (43)"   ! cubic
         num_exti = 42
         n = 0       ! nombre de reflections pouvant obeir a la regle d"extinction
         n_ext = 0   ! nombre de reflecions obeissant a la regle
         do h=-6, 6, 1
            hh(1)=h
            hh(2)=0
            hh(3)=0
            if (h /= int(h/4)*4) then
               n=n+1
               if (h_absent(hh,SpG)) n_ext=n_ext+1
            end if
         end do     ! h loop
         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                Hkl_Ref_Conditions(num_exti)
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti+1,": ", &
                                Hkl_Ref_Conditions(num_exti+1)
            serial_condition = .true.
         end if
      end if ! fin de la condition "if cubic

      if (SpG%CrystalSys(1:10) == "Monoclinic" .or. SpG%CrystalSys(1:12) == "Orthorhombic" .or.   &
         SpG%CrystalSys(1:10) == "Tetragonal" .or. SpG%CrystalSys(1:5)  == "Cubic" ) then
         ! Hkl_Ref_Conditions(44) =   "(0 k 0)      k=2n : screw axis // [010] with  b/2 translation (21)"   ! monoclinic, ortho., tetra and cubic
         num_exti = 44
         n = 0       ! nombre de reflections pouvant obeir a la regle d"extinction
         n_ext = 0   ! nombre de reflecions obeissant a la regle
         do k=-6, 6, 1
            hh(1)=0
            hh(2)=k
            hh(3)=0
            if (k /= int(k/2)*2) then
               n=n+1
               if (h_absent(hh,SpG)) n_ext=n_ext+1
            end if
         end do     ! h loop
         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                Hkl_Ref_Conditions(num_exti)
            serial_condition = .true.
         end if
      end if   ! fin de la condition "if mono, ortho, tetra, cubic

      if (SpG%CrystalSys(1:5) == "Cubic") then
         ! Hkl_Ref_Conditions(45) =   "(0 k 0)      k=2n : screw axis // [010] with  2b/4 translation (42)"   ! cubic
         num_exti = 45
         n = 0       ! nombre de reflections pouvant obeir a la regle d"extinction
         n_ext = 0   ! nombre de reflecions obeissant a la regle
         do k=-6, 6, 1
            hh(1)=0
            hh(2)=k
            hh(3)=0
            if (k /= int(k/2)*2) then
               n=n+1
               if (h_absent(hh,SpG)) n_ext=n_ext+1
            end if
         end do     ! h loop
         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                Hkl_Ref_Conditions(num_exti)
            serial_condition = .true.
         end if

         ! Hkl_Ref_Conditions(46) =   "(0 k 0)      k=4n : screw axis // [010] with  b/4 translation (41)"   ! cubic
         ! Hkl_Ref_Conditions(47) =   "(0 k 0)      k=4n : screw axis // [010] with 3b/4 translation (43)"   ! cubic
         num_exti = 46
         n = 0       ! nombre de reflections pouvant obeir a la regle d"extinction
         n_ext = 0   ! nombre de reflecions obeissant a la regle
         do k=-6, 6, 1
            hh(1)=0
            hh(2)=k
            hh(3)=0
            if (k /= int(k/4)*4) then
               n=n+1
               if (h_absent(hh,SpG)) n_ext=n_ext+1
            end if
         end do     ! h loop
         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                Hkl_Ref_Conditions(num_exti)
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti+1,": ", &
                                Hkl_Ref_Conditions(num_exti+1)
            serial_condition = .true.
         end if
      end if ! fin de la condition "if cubic

      if (SpG%CrystalSys(1:10) == "Monoclinic" .or. SpG%CrystalSys(1:12) == "Orthorhombic" .or.   &
         SpG%CrystalSys(1:5)  == "Cubic" ) then
         ! Hkl_Ref_Conditions(48) =   "(0 0 l)      l=2n : screw axis // [00l] with  c/2 translation (21)"   ! monoclinic, ortho. and cubic
         num_exti = 48
         n = 0       ! nombre de reflections pouvant obeir a la regle d"extinction
         n_ext = 0   ! nombre de reflecions obeissant a la regle
         do l=-6, 6, 1
            hh(1)=0
            hh(2)=0
            hh(3)=l
            if (l /= int(l/2)*2) then
               n=n+1
               if (h_absent(hh,SpG)) n_ext=n_ext+1
            end if
         end do     ! h loop
         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                Hkl_Ref_Conditions(num_exti)
            serial_condition = .true.
         end if
      end if  ! fin de la condition mono, ortho, tetra, cubic

      if (SpG%CrystalSys(1:5) == "Cubic" .or. SpG%CrystalSys(1:10) == "Tetragonal") then
         ! 49
         ! Hkl_Ref_Conditions(49) =   "(0 0 l)      l=2n : screw axis // [00l] with  c/2 translation (21)"   ! monoclinic, ortho. and cubic
         num_exti = 49
         n = 0       ! nombre de reflections pouvant obeir a la regle d"extinction
         n_ext = 0   ! nombre de reflecions obeissant a la regle
         do l=-6, 6, 1
            hh(1)=0
            hh(2)=0
            hh(3)=l
            if (l /= int(l/2)*2) then
               n=n+1
               if (h_absent(hh,SpG)) n_ext=n_ext+1
            end if
         end do     ! h loop
         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                Hkl_Ref_Conditions(num_exti)
            serial_condition = .true.
         end if

         ! 50 51
         ! Hkl_Ref_Conditions(50) =  "(0 0 l)      l=4n : screw axis // [00l] with  c/4 translation (41)"   ! tetragonal and cubic
         ! Hkl_Ref_Conditions(51) =  "(0 0 l)      l=4n : screw axis // [00l] with 3c/4 translation (43)"   ! tetragonal and cubic
         num_exti = 50
         n = 0       ! nombre de reflections pouvant obeir a la regle d"extinction
         n_ext = 0   ! nombre de reflecions obeissant a la regle
         do l=-6, 6, 1
            hh(1)=0
            hh(2)=0
            hh(3)=l
            if (l /= int(l/4)*4) then
               n=n+1
               if (h_absent(hh,SpG)) n_ext=n_ext+1
            end if
         end do     ! h loop
         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                Hkl_Ref_Conditions(num_exti)
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti+1,": ", &
                                Hkl_Ref_Conditions(num_exti+1)
            serial_condition = .true.
         end if
      end if ! fin de la condition "if cubic

      if (SpG%SPG_Lat(1:1) == "h") then

         !52:
         ! Hkl_Ref_Conditions(52) =   "(0 0 0 l)    l=2n : screw axis // [00l] axis with 3c/6 translation (63)"
         num_exti = 52
         n = 0       ! nombre de reflections pouvant obeir a la regle d"extinction
         n_ext = 0   ! nombre de reflecions obeissant a la regle
         do l=-6, 6, 1
            hh(1)=0
            hh(2)=0
            hh(3)=l
            if (l /= int(l/2)*2) then
               n=n+1
               if (h_absent(hh,SpG)) n_ext=n_ext+1
            end if
         end do     ! h loop
         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ",&
                                Hkl_Ref_Conditions(num_exti)
            serial_condition = .true.
         end if

         !53 54 55 56
         ! Hkl_Ref_Conditions(53) =   "(0 0 0 l)    l=3n : screw axis // [00l] axis with  c/3 translation (31)"
         ! Hkl_Ref_Conditions(54) =   "(0 0 0 l)    l=3n : screw axis // [00l] axis with 2c/3 translation (32)"
         ! Hkl_Ref_Conditions(55) =   "(0 0 0 l)    l=3n : screw axis // [00l] axis with 2c/6 translation (62)"
         ! Hkl_Ref_Conditions(56) =   "(0 0 0 l)    l=3n : screw axis // [00l] axis with 4c/6 translation (64)"
         num_exti = 53
         n = 0       ! nombre de reflections pouvant obeir a la regle d"extinction
         n_ext = 0   ! nombre de reflecions obeissant a la regle
         do l=-6, 6, 1
            hh(1)=0
            hh(2)=0
            hh(3)=l
            if (l /= int(l/3)*3) then
               n=n+1
               if (h_absent(hh,SpG)) n_ext=n_ext+1
            end if
         end do     ! h loop

         if (n==n_ext) then
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                Hkl_Ref_Conditions(num_exti)
            if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti+1,": ", &
                                Hkl_Ref_Conditions(num_exti+1)
            if (SpG%laue(1:3) == "6/m") then
               if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti+2,": ", &
                                   Hkl_Ref_Conditions(num_exti+2)
               if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti+3,": ", &
                                   Hkl_Ref_Conditions(num_exti+3)
            end if ! fin de la condition "6/m
            serial_condition   = .true.
         end if

         if (SpG%laue(1:3) == "6/m") then
            !57 58:
            ! Hkl_Ref_Conditions(57) =   "(0 0 0 l)    l=6n : screw axis // [00l] axis with  c/6 translation (61)"
            ! Hkl_Ref_Conditions(58) =   "(0 0 0 l)    l=6n : screw axis // [00l] axis with 5c/6 translation (65)"
            num_exti = 57
            n = 0       ! nombre de reflections pouvant obeir a la regle d"extinction
            n_ext = 0   ! nombre de reflecions obeissant a la regle
            do l=-6, 6, 1
               hh(1)=0
               hh(2)=0
               hh(3)=l
               if (l /= int(l/6)*6) then
                  n=n+1
                  if (h_absent(hh,SpG)) n_ext=n_ext+1
               end if
            end do     ! h loop
            if (n==n_ext) then
               if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti,": ", &
                                   Hkl_Ref_Conditions(num_exti)
               if (present(iunit)) write(unit=iunit,fmt="(tr5,a,i2,2a)")  "#",num_exti+1,": ", &
                                   Hkl_Ref_Conditions(num_exti+1)
               serial_condition   = .true.
            end if
         end if ! fin de la condition "6/m
      end if  ! fin de la condition "if hexagonal

      if (.not. serial_condition)   then
         if (present(iunit)) write(unit=iunit,fmt=*) "     =====>>> no serial reflection condition"
      end if

   End Subroutine Write_Screw_Axis_Conditions

End SubModule Refl_Conditions