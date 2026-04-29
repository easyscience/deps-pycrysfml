!!----
!!----
!!----
SubModule(CFML_Symmetry_Tables) TAB_Get_SpgSymbols
   Implicit none
   Contains
   !!----
   !!---- GET_COMPACT_HM_SYMBOL
   !!----
   !!---- 07/05/2019
   !!
   Module Function Get_Compact_HM(HM) Result(C_HM)
      !---- Arguments ----!
      character(len=*), intent(in) :: HM
      character(len=:), allocatable :: C_HM

      !---- Local Variables ----!
      character(len=40) :: l_chm
      integer           :: i,j,k

      l_chm=trim(hm)

      k=string_count(l_chm,'21')
      if (k > 0) then
         do j=1,k
            i=index(l_chm,'21')
            if (i > 0) l_chm=l_chm(:i)//'_'//l_chm(i+1:)
         end do
      end if

      k=string_count(l_chm,'31')
      if (k > 0) then
         do j=1,k
            i=index(l_chm,'31')
            if (i > 0) l_chm=l_chm(:i)//'_'//l_chm(i+1:)
         end do
      end if

      k=string_count(l_chm,'32')
      if (k > 0) then
         do j=1,k
            i=index(l_chm,'32')
            if (i > 0) l_chm=l_chm(:i)//'_'//l_chm(i+1:)
         end do
      end if

      k=string_count(l_chm,'41')
      if (k > 0) then
         do j=1,k
            i=index(l_chm,'41')
            if (i > 0) l_chm=l_chm(:i)//'_'//l_chm(i+1:)
         end do
      end if

      k=string_count(l_chm,'42')
      if (k > 0) then
         do j=1,k
            i=index(l_chm,'42')
            if (i > 0) l_chm=l_chm(:i)//'_'//l_chm(i+1:)
         end do
      end if

      k=string_count(l_chm,'43')
      if (k > 0) then
         do j=1,k
            i=index(l_chm,'43')
            if (i > 0) l_chm=l_chm(:i)//'_'//l_chm(i+1:)
         end do
      end if

      k=string_count(l_chm,'61')
      if (k > 0) then
         do j=1,k
            i=index(l_chm,'61')
            if (i > 0) l_chm=l_chm(:i)//'_'//l_chm(i+1:)
         end do
      end if

      k=string_count(l_chm,'62')
      if (k > 0) then
         do j=1,k
            i=index(l_chm,'62')
            if (i > 0) l_chm=l_chm(:i)//'_'//l_chm(i+1:)
         end do
      end if

      k=string_count(l_chm,'63')
      if (k > 0) then
         do j=1,k
            i=index(l_chm,'63')
            if (i > 0) l_chm=l_chm(:i)//'_'//l_chm(i+1:)
         end do
      end if

      k=string_count(l_chm,'64')
      if (k > 0) then
         do j=1,k
            i=index(l_chm,'64')
            if (i > 0) l_chm=l_chm(:i)//'_'//l_chm(i+1:)
         end do
      end if

      k=string_count(l_chm,'65')
      if (k > 0) then
         do j=1,k
            i=index(l_chm,'65')
            if (i > 0) l_chm=l_chm(:i)//'_'//l_chm(i+1:)
         end do
      end if

      c_hm=pack_string(l_chm)
   End Function Get_Compact_HM

   !!----
   !!---- GET_HM_FROM_COMPACT_HM
   !!----
   !!---- 07/05/2019
   !!
   Module Function Get_HM_Compact_HM(C_HM) Result(HM)
      !---- Arguments ----!
      character(len=*), intent(in) :: C_HM
      character(len=:), allocatable :: HM

      !---- Local Variables ----!
      character(len=1) :: car0
      character(len=20):: l_chm
      integer          :: i,j,n

      !> Init
      HM='Not defined'
      if (len_trim(c_hm) <=0) return

      l_chm=u_case(adjustl(c_hm))

      !> Cell type
      car0=l_chm(1:1)
      l_chm=l_chm(2:)

      !> Step 1:
      n=len_trim(l_chm)
      do i=n-1, 1, -1
         select case (l_chm(i:i))
            case ('A','B','C','M','N','D','E')
               l_chm=l_chm(:i)//' '//l_chm(i+1:)

            case ('1','2','3','4','6')
               if (l_chm(i+1:i+1) /= '_' .and. l_chm(i+1:i+1) /= '/') then
                  l_chm=l_chm(:i)//' '//l_chm(i+1:)
               end if
         end select
      end do

      !> Step 2: improper rotations
      do
         i=index(l_chm,'_')
         if (i == 0) exit

         select case (i)
            case (2)
               n=len_trim(l_chm)
               if (n > 3) then
                  l_chm=l_chm(1:1)//l_chm(3:3)//' '//adjustl(l_chm(4:))
               else
                  l_chm=l_chm(1:1)//l_chm(3:3)
               end if

            case default
               n=len_trim(l_chm)
               j=i+2
               if (j > n) then
                  l_chm=l_chm(:i-1)//l_chm(i+1:i+1)
               else
                  l_chm=l_chm(:i-1)//l_chm(i+1:i+1)//' '//adjustl(l_chm(j:))
               end if
         end select
      end do

      !> Step 3: search ' /'
      do
         i=index(l_chm,' /')
         if (i == 0) exit

         l_chm=l_chm(:i-1)//l_chm(i+1:)
      end do

      hm=car0//' '//trim(l_chm)
   End Function Get_HM_Compact_HM

   !!----
   !!---- GET_SPACEG_SYMBOLS
   !!----    From a valid space group name return the rest of possible names
   !!----    Using SPGR_INFO.
   !!----
   !!---- 07/05/2019
   !!
   Module Subroutine Get_SpaceGroup_Symbols(Str, HM, Hall, IT, C_HM)
      !---- Arguments ----!
      character(len=*),           intent(in)  :: Str   ! Input string with any format for SpaceGroup name
      character(len=*), optional, intent(out) :: HM    ! Hermman-Mauguin
      character(len=*), optional, intent(out) :: Hall  ! Hall
      integer,          optional, intent(out) :: IT    ! Number of SpaceGroup acording to IT
      character(len=*), optional, Intent(out) :: C_HM

      !---- Local Variables ----!
      character(len=:), allocatable :: l_HM, l_Hall, l_chm
      integer                       :: l_it
      integer                       :: i,ii,n,ier
      logical                       :: llchm, found

      !> Init
      l_it=0
      l_hm=" "; l_hall=" "; l_chm=" "
      llchm=.false.
      found=.false.

      !> Load Symmetry information
      call Set_Spgr_Info()

      !> Is Str a number?
      read(unit=str,fmt=*,iostat=ier) n
      if (ier == 0) then
         select case (n)
            case (1:230)
               l_it=n

            case default
               l_it=0
         end select

         if (l_it > 0) then
            do i=1, NUM_SPGR_INFO
               if (spgr_info(i)%n == l_it) then

                  !> Selecting standard
                  ii=i
                  select case (i)
                     case (IND_GRP_MONOC:IND_GRP_ORTHO-1)  ! Monoclinic
                        ii=i+1

                     case (IND_GRP_ORTHO:IND_GRP_TRIGO-1)
                        if (index(spgr_info(i)%hm,':') > 0) ii=i+1

                     case (IND_GRP_CUBIC:)
                        if (index(spgr_info(i)%hm,':') > 0) ii=i+1
                  end select

                  l_hm=spgr_info(ii)%hm
                  l_hall=spgr_info(ii)%hall
                  found=.true.
                  exit
               end if
            end do
         end if
      end if

      !> Is str a HM?
      if (l_it <= 0 .and. .not. found) then
         l_hm=trim(u_case(Str))
         do i=1, NUM_SPGR_INFO
            if (trim(spgr_info(i)%hm) == trim(l_hm)) then
               l_it=spgr_info(i)%n
               l_hall=spgr_info(i)%hall
               found=.true.
               exit
            end if
         end do
      end if

      !> Is str a Hall!
      if (l_it <= 0 .and. .not. found) then
         l_hall=trim((Str))
         do i=1, NUM_SPGR_INFO
            if (trim(spgr_info(i)%hall) == trim(l_hall)) then
               l_it=spgr_info(i)%n
               l_hm=spgr_info(i)%hm
               found=.true.
               exit
            end if
         end do
      end if

      !> Is str a compact HM
      if (l_it <= 0 .and. .not. found) then
         llchm=.false.
         l_hm=get_hm_compact_HM(u_case(str))
         do i=1, NUM_SPGR_INFO
            if (trim(spgr_info(i)%hm) == trim(l_hm)) then
               l_it=spgr_info(i)%n
               l_hall=spgr_info(i)%hall
               llchm=.true.
               exit
            end if
         end do
         if (llchm) l_chm=trim(u_case(str))
      end if

      !> Compact HM
      if (l_it > 0 .and. (.not. llchm)) then
         l_chm=get_compact_hm(l_hm)
      end if

      !> Final
      if (present(hm))   hm=l_hm
      if (present(hall)) hall=l_hall
      if (present(it))   it=l_it
      if (present(c_hm)) c_hm=l_chm
   End Subroutine Get_SpaceGroup_Symbols

End SubModule TAB_Get_SpgSymbols