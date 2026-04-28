!!----
SubModule (CFML_DiffPatt) DiffP_NoisyPoints

 implicit none

   Contains

   !!----
   !!---- DEL_NOISYPOINTS
   !!----
   !!---- Delete noisy points in a Pattern.
   !!----
   !!---- If FileInfo is .true. then a file is created containing
   !!---- information about the elimination of noisy points
   !!----
   !!---- 30/04/2019
   !!
   Module Subroutine Del_NoisyPoints(Pat, NoisyP, FileInfo)
       !---- Arguments ----!
       class(DiffPat_Type),  intent(in out) :: Pat        ! Pattern object
       integer,              intent(out)    :: NoisyP     ! Noisy points
       logical, optional,    intent(in)     :: FileInfo   ! .true. For create an information file

       !---- Local Variables ----!
       logical                                  :: info
       integer                                  :: i,j,nomo1,nomo2,lun
       real(kind=cp), dimension(5)              :: cc
       real(kind=cp)                            :: suma,sc,dif1,dif2
       real(kind=cp)                            :: ci2,ci1,c,cd1,cd2,cn
       real(kind=cp), dimension(:), allocatable :: yc

       !> Init
       call clear_error()
       NoisyP=0

       info=.false.
       if (present(FileInfo)) info=FileInfo


       !> Check Pattern
       if (pat%npts < 1) then
          err_CFML%Ierr=1
          err_CFML%Msg="Del_NoisyPoints@DIFFPATT: There aren't points in the Pattern!"
          return
       end if

       if (info) then
          open(newunit=lun, file='Noisy_Points_Information.txt')

          write(unit=lun,fmt='(a/)')  " => Analysis of Noisy points of Pattern "//trim(Pat%title)
          write(unit=lun,fmt='(/a/)') " => A Noisy point means the following:"
          write(unit=lun,fmt='(a/)')  "        NoMono .and. Iosci = .true. "
          write(unit=lun,fmt='(a/)')  " where:"
          write(unit=lun,fmt='(a)')   "     ci2 : counts at          left-left position"
          write(unit=lun,fmt='(a)')   "     ci1 : counts at               left position"
          write(unit=lun,fmt='(a)')   "     cc  : counts at            current position"
          write(unit=lun,fmt='(a)')   "     cd1 : counts at              right position"
          write(unit=lun,fmt='(a)')   "     cd2 : counts at              right position"
          write(unit=lun,fmt='(a)')   "     sc  : 8.0*sqrt((ci1+ci2+cd1+cd2)/4.0)"
          write(unit=lun,fmt='(a)')   "     dif1: cc -2.0*ci1+ci2"
          write(unit=lun,fmt='(a)')   "     dif2: cc -2.0*cd1+cd2"
          write(unit=lun,fmt='(a)')   "    Iosci: .not.(dif1 < sc .or. dif2 < sc)"
          write(unit=lun,fmt='(a)')   "   NoMono: Non monotonic ci2,ci1,cc,cd1,cd2"
          write(unit=lun,fmt='(a/)')  "  cc(new): 0.5*(ci1+cd1)"
       end if

       !> Copy Y values
       if (allocated(yc)) deallocate(yc)
       allocate(yc(Pat%npts))
       yc=Pat%y

       cyc_1: do j=3,Pat%npts-2
          suma=0.0_cp
          do i=1,5
             cc(i)=yc(i+j-3)
             if (cc(i) <= 0.0_cp) cycle cyc_1
             if (i /= 3) suma=suma+cc(i)
          end do
          nomo1=0
          nomo2=0
          do i=2,5
             if (cc(i) > cc(i-1)) nomo1=nomo1+1
             if (cc(i) < cc(i-1)) nomo2=nomo2+1
          end do
          if (nomo1 == 4 .or. nomo2 == 4) cycle cyc_1

          sc=4.0_cp*sqrt(suma)
          dif1=cc(3)-2.0_cp*cc(1)+cc(2)
          dif2=cc(3)-2.0_cp*cc(4)+cc(5)
          if (.not. (dif1 <= sc .or. dif2 <= sc) ) then
             if (info) then
                ci2=cc(1)
                ci1=cc(2)
                c=cc(3)
                cd1=cc(4)
                cd2=cc(5)
                cn=0.5*(ci1+cd1)
                write(unit=lun,fmt='(a,2i6,2(a,i6),a,a,2i6)') " Counts-left: ",nint(ci2),nint(ci1), &
                                                              " Counts: ",nint(c)," (",nint(cn),")",  &
                                                              " Counts-right: ",nint(cd1),nint(cd2)
             end if
             noisyp=noisyp+1
             yc(j)=0.5*(yc(j-1)+yc(j+1))
          end if
       end do cyc_1

       if (info) then
          select case (noisyP)
             case (0)
                write(unit=lun,fmt='(/a)')  " => No noisy points were found for this Pattern!"
             case (1)
                write(unit=lun,fmt='(/a)')  " => Only one noisy point was found for this Pattern!"
             case (2:)
                write(unit=lun,fmt='(/a,i3,a)')  " => A ",noisyP," noisy points were found for this Pattern!"
          end select
          close(unit=lun)
       end if

       Pat%y=yc

   End Subroutine Del_NoisyPoints

End SubModule DiffP_NoisyPoints