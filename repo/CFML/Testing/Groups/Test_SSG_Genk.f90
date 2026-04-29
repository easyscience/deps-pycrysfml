  Module getting_ssg
    use CFML_GlobalDeps
    use CFML_gSpaceGroups
    use CFML_Rational
    use CFML_Propagation_Vectors
    use CFML_Maths,             only : Equal_Matrix
    use CFML_Symmetry_Tables,   only : Get_HM_Compact_HM
    use CFML_Strings,           only : String_Fraction_2Dig,l_case,get_words

    implicit none

    character(len=2), dimension(11) :: screw=(/"21","31","32","41","42","43","61","62","63","64","65"/)
    character(len=1), dimension(11) :: rm_screw=(/"2","3","3","4","4","4","6","6","6","6","6"/)

    contains

    Subroutine Set_Intersection_Groups(SpGs,SpG)
      Type (SpG_Type),dimension(:), intent(in)   :: SpGs
      class(SpG_Type), allocatable, intent(out)  :: SpG
      !--- Local Variables ---!
      integer :: i,j,k,ng,ipos,n
      integer,dimension(1) :: iip
      character(len=80),dimension(192) :: gen
      logical,dimension(size(SpGs(:))) :: estak

      !write(*,"(/a/)") " => Entering subroutine Set_Intersection_Groups "

      n=size(SpGs(:))
      if(n == 1) then
         SpG=SpGs(1)
         return
      end if
      iip=minloc(SpGs(:)%multip)
      ipos=iip(1)
      ng=1
      gen(1)="x,y,z,1"
      estak=.false.
      estak(ipos)=.true.

      !write(*,"(3(a,i3))") " => Number of space groups: ",n, "  Position: ",ipos,"  Multiplicity:",SpGs(ipos)%multip

      do_ext:do i=2,SpGs(ipos)%multip
        ng=ng+1
        gen(ng)=SpGs(ipos)%Symb_Op(i)
        do j=1,n
           if(j == ipos) cycle
           estak(j)=.false.
           do k=2,SpGs(j)%multip
             !write(*,"(2i4,a,a)") k,ng, "   "//trim(gen(ng))//"   "//trim(SpGs(j)%Symb_Op(k))
             if(trim(SpGs(j)%Symb_Op(k)) == trim(gen(ng))) then
               estak(j)=.true.
               exit
             end if
           end do
        end do
        !write(*,*) estak
        k=count(estak(1:n))
        if(k /= n) then
          !write(*,"(a,i3)") "  Operator: "//trim(gen(ng))//"  Discarded  ng=",ng-1
          ng=ng-1
          cycle
        end if
        !Passing here means that the operator is common to all space groups
        if(ng > 192) exit
      end do do_ext
      call Set_SpaceGroup(" ",SpG,ng,gen)

    End Subroutine Set_Intersection_groups

    ! This function returns the maximum order
    ! (defined as the exponent to become the Identity)
    ! of a group of rotational operators,optionally the
    ! order of each element is also provided

    Subroutine get_Order(SpG, maxorder,g_order)
      class(SpG_type),                           intent(in)    :: SpG
      integer,                                   intent(out)   :: maxorder
      integer,dimension(:),optional,allocatable, intent(out)   :: g_order

      integer, dimension(3,3)  :: ID, CM, TM
      integer                  :: cnt, i, j

      if(present(g_order)) then
        allocate(g_order(SpG%multip))
        g_order(1)=1
        g_order(2:)=0
      end if
      TM=SpG%Op(1)%Mat(1:3,1:3) ! Current Matrix
      CM=SpG%Op(1)%Mat(1:3,1:3) ! Initiallized to the Identity
      ID=SpG%Op(1)%Mat(1:3,1:3) ! Identity matrix is always the first of the SG

      maxorder=0

      do_ext:do i=2, SpG%Multip
         CM=SpG%op(i)%Mat(1:3,1:3)  ! Current Matrix
         TM=SpG%op(1)%Mat(1:3,1:3)  ! Test Matrix (initiallized to the Id)
         cnt=0
         do j=1,6 !order is never greater than 6
             cnt=cnt+1
             TM=matmul(TM,CM)
             if(Equal_Matrix(TM,ID,3)) then
                if(present(g_order) ) g_order(i)=cnt
                if(cnt > maxorder) maxorder=cnt
                cycle do_ext
             end if
         end do
      end do do_ext

    End Subroutine get_Order

    Subroutine Get_Set_of_Generators(SpG,gen,point_op,ngen,List_Symb)
      class(SpG_Type),                intent (in) :: SpG
      Character(len=*), dimension(:), intent(out) :: gen
      integer,          dimension(:), intent(out) :: point_op
      integer,                        intent(out) :: ngen
      character(len=*), dimension(:), intent(out) :: List_symb
      !--- Local variables ---!
      integer                                   :: i,j,n,m
      integer,          dimension(3,3)          :: s
      real(kind=cp),    dimension(3)            :: t
      character(len=6), dimension(8)            :: sgen,msgen
      character(len=20)                         :: spg_symb
      character(len=40),dimension(SpG%Multip)   :: op_symb
      logical,          dimension(SpG%Multip)   :: done

      point_op=0; done=.false.
      if(len_trim(SpG%SPG_Symb) == 0) then
        spg_symb= Get_HM_Compact_HM(SpG%BNS_symb)
      else
        spg_symb=adjustl(SpG%SPG_Symb)
      end if
      spg_symb(2:)=l_case(spg_symb(2:))
      !write(*,"(a)") " => Symbol of the space group without lattice: "//trim(spg_symb)
      i=index(spg_symb,":")
      if( i /= 0) spg_symb=spg_symb(1:i-1) !Remove ":"
      i=index(spg_symb,"/")
      if( i /= 0) spg_symb(i:i)=" "  !Remove "/"
      spg_symb=adjustl(spg_symb(2:)) !Remove lattice symbol

      call get_words(spg_symb,sgen,n)

      m=0
      do i=1,n
        if(sgen(i)(1:1) == "1") cycle
        m=m+1
        msgen(m)=sgen(i)
      end do
      do i=1,m
        sgen(i)=msgen(i)
      end do

      ngen = m
      List_Symb(1:m)=sgen(1:m)
      !Remove the second number for screw axes and replace it by positive rotation
      do j=1,ngen
        do i=1,11
          if(trim(sgen(j)) == screw(i)) then
            sgen(j)= rm_screw(i)//"+"
            exit
          end if
        end do
      end do


      do j=1,ngen   !Look always for positive roto-inversions
          if(sgen(j)(1:1) == "-") then
            sgen(j)= trim(sgen(j))//"+"
            exit
          end if
      end do

      do j=1,ngen   !Look always for positive roto-inversions
          if(index(sgen(j),"+") /= 0) cycle
          if(sgen(j)(1:1) == "3" .or. sgen(j)(1:1) == "4".or. sgen(j)(1:1) == "6") then
            sgen(j)= trim(sgen(j))//"+"
          end if
      end do

      write(*,"(10a)") " => Final Symbol to be analysed: ",(trim(sgen(j))//" ",j=1,ngen)

      op_symb(1)="1"
      do i=2,SpG%Multip
         s=SpG%Op(i)%Mat(1:3,1:3)
         t=SpG%Op(i)%Mat(1:3,4)
         op_symb(i) = Symmetry_Symbol(s,t)
      end do
      done=.false.

      Select Case(SpG%CrystalSys)

        Case("Triclinic")

          if(SpG%centred == 1) then
            ngen=1
            List_Symb(1)="1"
            point_op(1)=1
            gen(1)="x,y,z,1"
          else
            ngen=1
            List_Symb(1)="-1"
            point_op(1)=2
            gen(1)="-x,-y,-z,1"
          end if

        Case("Monoclinic","Orthorhombic")

          do j=1,ngen
             do i=2,SpG%Multip
               if(done(i)) cycle
               if(index(op_symb(i),trim(sgen(j))) /= 0 ) then
                 Select Case(j)
                   Case(1)
                      if(trim(sgen(j)) == "2" .and. index(op_symb(i),"x,") /= 0) then
                        point_op(j)=i
                        done(i)=.true.
                        List_Symb(j)=trim(List_Symb(j))//"_x"
                      else if(trim(sgen(j)) /= "2" .and. index(op_symb(i),",y,x") /= 0) then
                        point_op(j)=i
                        done(i)=.true.
                        List_Symb(j)=trim(List_Symb(j))//"_x"
                      end if
                   Case(2)
                      if(trim(sgen(j)) == "2" .and. index(op_symb(i),",y,") /= 0) then
                        point_op(j)=i
                        done(i)=.true.
                        List_Symb(j)=trim(List_Symb(j))//"_y"
                      else if(trim(sgen(j)) /= "2" .and. index(op_symb(i),"x,") /= 0 .and. index(op_symb(i),",z") /= 0) then
                        point_op(j)=i
                        done(i)=.true.
                        List_Symb(j)=trim(List_Symb(j))//"_y"
                      end if
                   Case(3)
                      if(trim(sgen(j)) == "2" .and. index(op_symb(i),",z") /= 0) then
                        point_op(j)=i
                        done(i)=.true.
                        List_Symb(j)=trim(List_Symb(j))//"_z"
                      else if(trim(sgen(j)) /= "2" .and. index(op_symb(i),"x,y,") /= 0 ) then
                        point_op(j)=i
                        done(i)=.true.
                        List_Symb(j)=trim(List_Symb(j))//"_z"
                      end if
                 End Select
                 exit
               end if
             end do
          end do

          if(any(point_op(1:ngen) == 0)) then !Look for unidentified operators (glide planes in monoclinic or orthorhombic systems)
              do j=1,ngen
                 i=point_op(j)
                 if(i == 0) then
                   do i=2,SpG%Multip
                     if(done(i)) cycle
                     if(index(op_symb(i),"a") /= 0) then
                          point_op(j)=i
                          done(i)=.true.
                          List_Symb(j)=trim(List_Symb(j))//"-a"
                          exit
                     else if(index(op_symb(i),"b") /= 0) then
                          point_op(j)=i
                          done(i)=.true.
                          List_Symb(j)=trim(List_Symb(j))//"-b"
                          exit
                     else if(index(op_symb(i),"c") /= 0) then
                          point_op(j)=i
                          done(i)=.true.
                          List_Symb(j)=trim(List_Symb(j))//"-c"
                          exit
                     else if(index(op_symb(i),"d") /= 0) then
                          point_op(j)=i
                          done(i)=.true.
                          List_Symb(j)=trim(List_Symb(j))//"-d"
                          exit
                     else if(index(op_symb(i),"g") /= 0) then
                          point_op(j)=i
                          done(i)=.true.
                          List_Symb(j)=trim(List_Symb(j))//"-g"
                          exit
                     end if
                   end do
                 end if
              end do
          end if

        Case("Trigonal")
          do i=2,SpG%Multip
            if(index(op_symb(i),trim(sgen(1))) /= 0 ) then
              point_op(1)=i
              done(i)=.true.
              !Determination of the direction of ternary axis
              if(index(op_symb(i),",z") /= 0 ) then
              else if(index(op_symb(i),"x,x,x") /= 0 ) then
              end if
              exit
            end if
          end do
        Case("Tetragonal")
        Case("Hexagonal")
        Case("Cubic")
      End Select
    End Subroutine Get_Set_of_Generators

    !!----
    !!---- Subroutine Get_Generators_From_SpGSymbol(SpG,gen,point_op,ngen,List_Symb)
    !!----    class(SpG_Type),                intent (in) :: SpG
    !!----    Character(len=*), dimension(:), intent(out) :: gen
    !!----    integer,          dimension(:), intent(out) :: point_op
    !!----    integer,                        intent(out) :: ngen
    !!----    character(len=*), dimension(:), intent(out) :: List_Symb
    !!----
    !!----    This subroutine provides the generators of the space group that
    !!----    are explicitly written in the Hermann-Mauguin symbol of the space group.
    !!----    The generators of the lattice are ignored. The generators gen(i),are written
    !!----    in the Jones faithful representation. There are ngen generators and the
    !!----    integer vector "point_op" contains the index of the corresponding operator in
    !!----    the list of the total SpG%Multip operators. This subroutine works only for
    !!----    one of the NUM_SPGR_INFO  = 612 settings of the 230 space groups.
    !!----
    !!----   Update: February - 2017
    !!----
    Subroutine Get_Generators_From_SpGSymbol(SpG,gen,point_op,ngen,List_Symb)
      class(SpG_Type),                intent (in) :: SpG
      Character(len=*), dimension(:), intent(out) :: gen
      integer,          dimension(:), intent(out) :: point_op
      integer,                        intent(out) :: ngen
      character(len=*), dimension(:), intent(out) :: List_symb
      !--- Local variables ---!
      integer                                   :: i,j,n,m,dir,dirm
      integer,          dimension(3)            :: dm
      integer,          dimension(3,3)          :: s
      real(kind=cp),    dimension(3)            :: t
      character(len=6), dimension(8)            :: sgen,msgen
      character(len=20)                         :: spg_symb
      character(len=60),dimension(SpG%Multip)   :: op_symb
      logical,          dimension(SpG%Multip)   :: done
      logical                                   :: slash
      character(len=*),parameter,dimension(3)   :: dire=["_x","_y","_z"]

      point_op=0; done=.false.; slash=.false.
      if(len_trim(SpG%SPG_Symb) == 0) then
        spg_symb= Get_HM_Compact_HM(SpG%BNS_symb)
      else
        spg_symb=adjustl(SpG%SPG_Symb)
      end if
      spg_symb(2:)=l_case(spg_symb(2:))
      !write(*,"(a)") " => Symbol of the space group without lattice: "//trim(spg_symb)
      i=index(spg_symb,":")
      if( i /= 0) spg_symb=spg_symb(1:i-1) !Remove ":"
      i=index(spg_symb,"/")
      if( i /= 0) then
        spg_symb(i:i)=" "  !Remove "/"
        slash=.true.
      end if
      spg_symb=adjustl(spg_symb(2:)) !Remove lattice symbol

      call get_words(spg_symb,sgen,n)

      m=0; dm=1; dirm=2
      do i=1,n
        if(sgen(i)(1:1) == "1") then
          if(SpG%crystalsys=="Monoclinic") dm(i)=0
          cycle
        end if
        m=m+1
        msgen(m)=sgen(i)
      end do
      if(SpG%crystalsys=="Monoclinic") then
        if(sum(dm) == 3) then
          dirm=2
        else
          do i=1,n
            if(dm(i) == 1) then
              dirm=i
              exit
            end if
          end do
        end if
      end if

      do i=1,m
        sgen(i)=msgen(i)
      end do
      ngen = m
      List_Symb(1:m)=sgen(1:m)
      !Remove the second number for screw axes and replace it by positive rotation
      do j=1,ngen
        do i=1,11
          if(trim(sgen(j)) == screw(i)) then
            sgen(j)= rm_screw(i)
            if(i > 1) sgen(j)=trim(sgen(j))//"+"
            exit
          end if
        end do
      end do


      do j=1,ngen   !Look always for positive roto-inversions
          if(sgen(j)(1:1) == "-" .and. sgen(j)(2:2) /= "1") then
            sgen(j)= trim(sgen(j))//"+"
            exit
          end if
      end do

      do j=1,ngen   !Look always for positive roto-inversions
          if(index(sgen(j),"+") /= 0) cycle
          if(sgen(j)(1:1) == "3" .or. sgen(j)(1:1) == "4".or. sgen(j)(1:1) == "6") then
            sgen(j)= trim(sgen(j))//"+"
          end if
      end do

      !write(*,"(10a)") " => Final Symbol to be analysed: ",(trim(sgen(j))//" ",j=1,ngen)

      op_symb(1)="1"
      do i=2,SpG%Multip
         s=SpG%Op(i)%Mat(1:3,1:3)
         t=SpG%Op(i)%Mat(1:3,4)
         op_symb(i) = Symmetry_Symbol(s,t)
      end do
      done=.false.

      Select Case(SpG%crystalsys)

         Case("Triclinic")
           do j=1,ngen
             do i=2,SpG%Multip
               if(done(i)) cycle
               if(index(op_symb(i),trim(sgen(j))) /= 0 ) then
                 point_op(j)=i
                 done(i)=.true.
                 exit
               end if
             end do
           end do

         Case("Monoclinic")

           do j=1,ngen
             do i=2,SpG%Multip
               if(done(i)) cycle
               if(index(op_symb(i),trim(sgen(j))) /= 0 ) then
                 point_op(j)=i
                 done(i)=.true.
                 List_Symb(j) = trim(List_Symb(j))//dire(dirm)
                 exit
               end if
             end do
           end do

         Case("Orthorhombic")

           do j=1,ngen
             do i=2,SpG%Multip
               if(done(i)) cycle
               if(index(op_symb(i),trim(sgen(j))) /= 0 ) then
                 if(j == 1) then
                   if(op_symb(i)(1:1) == "2") then
                      if(index(op_symb(i),"x,") == 0) cycle
                   else
                      if(index(op_symb(i),"y,z") == 0) cycle
                   end if
                 end if
                 if(j == 2) then
                   if(op_symb(i)(1:1) == "2") then
                      if(index(op_symb(i),",y,") == 0) cycle
                   else
                      if(index(op_symb(i),"x,") == 0 .or. index(op_symb(i),",z") == 0) cycle
                   end if
                 end if
                 if(j == 3) then
                   if(op_symb(i)(1:1) == "2") then
                      if(index(op_symb(i),",z") == 0) cycle
                   else
                      if(index(op_symb(i),"x,y") == 0 ) cycle
                   end if
                 end if
                 point_op(j)=i
                 done(i)=.true.
                 List_Symb(j) = trim(List_Symb(j))//dire(j)
                 exit
               end if
             end do
           end do

         Case("Tetragonal","Hexagonal")  !Asume the fourfold/sixfold axis is along z

           !Assume the direction of the principal rotation axis is along c
           dir=3
           !Check special case
           if(SpG%SPG_Symb(1:1) == "C") then !Tetragonal Group 117 in C-centred lattice
             ngen= 3
             point_op(1)=5; List_Symb(1) = trim(List_Symb(1))//dire(dir); gen(1)=SpG%Symb_Op(5)
             point_op(2)=7; List_Symb(2) = trim(List_Symb(2))//"_xxz";    gen(2)=SpG%Symb_Op(7)
             point_op(3)=2; List_Symb(3) = trim(List_Symb(3))//"_x";      gen(3)=SpG%Symb_Op(2)
             return
           end if
           if(SpG%crystalsys=="Tetragonal") then
              if(index(sgen(1),"4+") /= 0) then
                do i=2,SpG%Multip
                  if(index(op_symb(i),trim(sgen(1))) /= 0 ) then
                     point_op(1)=i
                     done(i)=.true.
                     List_Symb(1) = trim(List_Symb(1))//dire(dir)
                     exit
                  end if
                end do
              end if
           else
              if(index(sgen(1),"6+") /= 0) then
                do i=2,SpG%Multip
                  if(index(op_symb(i),trim(sgen(1))) /= 0 ) then
                     point_op(1)=i
                     done(i)=.true.
                     List_Symb(1) = trim(List_Symb(1))//dire(dir)
                     exit
                  end if
                end do
              end if
           end if

           if(ngen > 1) then
             !Now analyse the second generator that may be perpendicular to the 4-fold axis or along a if it is 4_z
             if(sgen(2)(1:1) == "2")  then

               do i=2,SpG%Multip
                 if(done(i)) cycle
                 if(op_symb(i)(1:1) == "2" ) then
                   if(msgen(2) == "21") then
                     if(index(op_symb(i),"(") /= 0 .and. (index(op_symb(i),"x,0") /= 0 .or. index(op_symb(i),"x,1") /= 0)) then
                       point_op(2)=i
                       done(i)=.true.
                       List_Symb(2) = trim(List_Symb(2))//dire(1)
                       exit
                     end if
                   else if(index(op_symb(i),"x,0") /= 0) then
                     point_op(2)=i
                     done(i)=.true.
                     List_Symb(2) = trim(List_Symb(2))//dire(1)
                     exit
                   end if
                 end if
               end do

             else  !a plane

               if(slash) then

                 do i=2,SpG%Multip
                   if(done(i)) cycle
                   if(index(op_symb(i),trim(sgen(2))) /= 0 ) then
                      if(index(op_symb(i),"x,y,") /= 0) then
                        point_op(2)=i
                        done(i)=.true.
                        List_Symb(2) = trim(List_Symb(2))//dire(3)
                        exit
                      end if
                   end if
                 end do

               else !directions ab,bc or ac

                 do i=2,SpG%Multip
                   if(done(i)) cycle
                   if(index(op_symb(i),trim(sgen(2))) /= 0 ) then
                      if(index(op_symb(i),"x,") /= 0 .and. index(op_symb(i),",z") /= 0) then
                        point_op(2)=i
                        done(i)=.true.
                        List_Symb(2) = trim(List_Symb(2))//"_xz"
                        exit
                      end if
                      if(index(op_symb(i),"y,z") /= 0) then
                        point_op(2)=i
                        done(i)=.true.
                        List_Symb(2) = trim(List_Symb(2))//"_yz"
                        exit
                      end if
                   end if
                 end do

               end if
             end if
           end if

           if(ngen > 2) then !Now analyse the third generator

             if(slash) then  !directions along a,b if 4_z  !it should be a plane
                  do i=2,SpG%Multip
                    if(done(i)) cycle
                    if(index(op_symb(i),trim(sgen(3))) /= 0 ) then
                      if(index(op_symb(i),"y,z") /= 0 ) then
                         point_op(3)=i
                         done(i)=.true.
                         List_Symb(3) = trim(List_Symb(3))//"_yz"
                         exit
                      end if
                      if(index(op_symb(i),"x,") /= 0 .and. index(op_symb(i),",z") /= 0 ) then
                         point_op(3)=i
                         done(i)=.true.
                         List_Symb(3) = trim(List_Symb(3))//"_xz"
                         exit
                       end if
                    end if
                  end do
             else            !directions along x,x,0 if 4_z

               if(sgen(3)(1:1) == "2")  then
                  do i=2,SpG%Multip
                    if(done(i)) cycle
                    if(op_symb(i)(1:1) == "2"  .and. index(op_symb(i),"x,x") /= 0) then
                      point_op(3)=i
                      done(i)=.true.
                      List_Symb(3) = trim(List_Symb(3))//"_xx"
                      exit
                    end if
                  end do
               else
                  do i=2,SpG%Multip
                    if(done(i)) cycle
                    if(index(op_symb(i),trim(sgen(3))) /= 0 ) then
                       if(index(op_symb(i),"x,x") /= 0 ) then
                         point_op(3)=i
                         done(i)=.true.
                         List_Symb(3) = trim(List_Symb(3))//"_xxz"
                         exit
                       end if
                       if(index(op_symb(i),"x,-x") /= 0 ) then
                         point_op(3)=i
                         done(i)=.true.
                         List_Symb(3) = trim(List_Symb(3))//"_x-xz"
                         exit
                       end if
                    end if
                  end do
               end if

             end if
           end if

           if(ngen > 3) then !Now analyse the fourth generator !directions along x,x,0 if 4_z (only planes)
              do i=2,SpG%Multip
                if(done(i)) cycle
                if(index(op_symb(i),trim(sgen(4))) /= 0 ) then
                   if(index(op_symb(i),"x,x") /= 0 ) then
                     point_op(4)=i
                     done(i)=.true.
                     List_Symb(4) = trim(List_Symb(4))//"_xxz"
                     exit
                   end if
                   if(index(op_symb(i),"x,-x") /= 0 ) then
                     point_op(4)=i
                     done(i)=.true.
                     List_Symb(4) = trim(List_Symb(4))//"_x-xz"
                     exit
                   end if
                end if
              end do
           end if


         Case("Trigonal")
           !The direction of the principal 3-fold rotation axis is assumed to be along c-axis
           dir=3
           if(index(sgen(1),"3+") /= 0) then
             do i=2,SpG%Multip
               if(index(op_symb(i),trim(sgen(1))) /= 0 ) then
                  point_op(1)=i
                  done(i)=.true.
                  List_Symb(1) = trim(List_Symb(1))//dire(dir)
                  exit
               end if
             end do
           end if

           if(ngen > 1) then
             !Now analyse the second generator that may be perpendicular to the 3-fold axis or along a if it is 3_z
             if(sgen(2)(1:1) == "2")  then

               do i=2,SpG%Multip
                 if(done(i)) cycle
                 if(op_symb(i)(1:1) == "2" ) then
                   if(msgen(2) == "21") then
                     if(index(op_symb(i),"(") /= 0 .and. (index(op_symb(i),"x,0") /= 0 .or. index(op_symb(i),"x,1") /= 0)) then
                       point_op(2)=i
                       done(i)=.true.
                       List_Symb(2) = trim(List_Symb(2))//dire(1)
                       exit
                     end if
                   else if(index(op_symb(i),"x,0") /= 0) then
                     point_op(2)=i
                     done(i)=.true.
                     List_Symb(2) = trim(List_Symb(2))//dire(1)
                     exit
                   end if
                 end if
               end do

             else  !a plane

                 do i=2,SpG%Multip
                   if(done(i)) cycle
                   if(index(op_symb(i),trim(sgen(2))) /= 0 ) then
                      if(index(op_symb(i),"x,") /= 0 .and. index(op_symb(i),",z") /= 0) then
                        point_op(2)=i
                        done(i)=.true.
                        List_Symb(2) = trim(List_Symb(2))//"_xz"
                        exit
                      end if
                      if(index(op_symb(i),"y,z") /= 0) then
                        point_op(2)=i
                        done(i)=.true.
                        List_Symb(2) = trim(List_Symb(2))//"_yz"
                        exit
                      end if
                   end if
                 end do

             end if
           end if

         Case("Cubic")

           do j=1,ngen
             do i=2,SpG%Multip
               if(done(i)) cycle
               if(index(op_symb(i),trim(sgen(j))) /= 0 ) then
                 if(j == 1) then
                   if(sgen(j)(1:1) /= "2" .and. sgen(j)(1:1) /= "4" .and. sgen(j)(1:2) /= "-4")  then
                     if(index(op_symb(i),"x,y") == 0) cycle
                   end if
                 end if
                 if(j == 3) then
                   if(index(op_symb(i),"x,x") == 0 .and. index(op_symb(i),"x,-x") == 0)  cycle
                 end if
                 point_op(j)=i
                 done(i)=.true.
                 exit
               end if
             end do
           end do

           if(op_symb(point_op(1)) == "2" .or. index(op_symb(point_op(1))(1:3),"4") /= 0 ) then
              List_Symb(1) = trim(List_Symb(1))//"_z"
           else
              List_Symb(1) = trim(List_Symb(1))//"_xy"
           end if

           List_Symb(2) = trim(List_Symb(2))//"_xxx"

           if(op_symb(point_op(3))(1:1) == "2") then
              List_Symb(3) = trim(List_Symb(3))//"_xx"
           else
              if(index(op_symb(point_op(3)),"x,-x") /= 0) then
                List_Symb(3) = trim(List_Symb(3))//"_x-xz"
              else
                List_Symb(3) = trim(List_Symb(3))//"_xxz"
              end if
           end if

      End Select

      n=0
      do j=1,ngen
        i=point_op(j)
        if(i == 0) then
           gen(j) = "not found"
           cycle
        end if
        gen(j)=SpG%Symb_Op(i)
      end do

    End Subroutine Get_Generators_From_SpGSymbol

    Subroutine Get_Symbol_SSG_from_Operators()
      !Translations along extra coordinates allowed for the different order of operators
      !  t =  0  1/2  1/3  -1/3   1/4  -1/4   1/6   -1/6
      !Symb   0   s    t   -t      q    -q     h     -h
    End Subroutine Get_Symbol_SSG_from_Operators

  End Module getting_ssg

    Program Test_ssg_Genk
      Use CFML_GlobalDeps
      Use CFML_gSpaceGroups
      Use CFML_gSpaceGroups
      Use CFML_Propagation_Vectors, only: K_Star, Write_Group_K, Set_Gk, Group_k_Type
      Use CFML_SuperSpace_Database
      Use CFML_IOForm, only : Read_CFL_SpG,Read_CFL_Cell
      use CFML_Rational
      use getting_ssg

      implicit none

      integer                         :: nkv,i,j,maxorder,ngen !,n,j,k,m,multip
      character(len=1)                :: ans
      character(len=50)               :: Str_tmp !,forma
      real(kind=cp), dimension(3,12)  :: kv
      type(Group_k_Type),dimension(12):: Gk
      class(SpG_Type), allocatable    :: SpG,SpG_aux,intSpG !,st_intSpG
      Type(SpG_Type),  dimension(24)  :: Grpk
      logical                         :: ext=.true.
      type(rational),dimension(4,4)   :: Pmat
      real(kind=cp), dimension(3,3)   :: rot
      integer,           dimension(:),allocatable :: order,point_op
      character(len=40), dimension(:),allocatable :: gen
      character(len=10), dimension(6) :: list_symb_gk

      do
        write(*,"(a)",advance="no") " => Enter the number (or the symbol) of a space group: "
        read(*,"(a)") Str_tmp
        if(len_trim(Str_tmp) == 0) exit
        write(*,"(a)",advance="no") " => Enter the number of propagation vectors (1 integer): "
        read(*,*) nkv
        do i=1,nkv
           write(*,"(a,i2,a)",advance="no") " => Enter the propagation vector # ",i,": "
           read(*,*) kv(:,i)
        end do
        call Set_SpaceGroup(Str_tmp,SpG)
        if(Err_CFML%Ierr /= 0) then
          write(unit=*,fmt="(a)") " => "//trim(Err_CFML%Msg)
          cycle
        end if
        call Write_SpaceGroup_Info(SpG)

        do i=1,nkv
          call K_Star(kv(:,i),SpG,Gk(i),ext)
          call Set_Gk(Gk(i),SpG_aux,ext)
          Grpk(i)=SpG_aux
          write(*,"(//a,3f10.5/)") " => GROUP OF THE PROPAGATION VECTOR: ",kv(:,i)
          call Write_Group_K(Gk(i))
          write(*,"(//a/)") " => FULL PROPAGATION VECTOR SPACE GROUP INCLUDING -k (Extended Little Group): "
          call Write_SpaceGroup_Info(Grpk(i))
        end do
        call Set_Intersection_Groups(Grpk(1:nkv),intSpG)
        write(*,"(//a)")  "  INTERSECTION SPACE GROUP FOR GETTING POSSIBLE SUPERSPACE GROUPS"
        call Write_SpaceGroup_Info(intSpG)

        write(*,"(a)",advance="no") " => Transform to standard: "
        read(*,"(a)") ans
        if(ans == "y" .or. ans=="Y") then
          call Change_Setting_SpaceG(intSpG%mat2std,intSpG)
          write(*,"(//a)")  "  INTERSECTION SPACE GROUP IN STANDARD SETTING"
          call Write_SpaceGroup_Info(intSpG)
          call Get_Mat_From_Symb_Int(intSpG%mat2std, Pmat)
          rot=Pmat(1:3,1:3)
          write(*,"(/a)")  " => Propagation vectors in the standard setting: "
          do i=1,nkv
            kv(:,i)=matmul(kv(:,i),rot)
            write(*,"(i5,a,3f10.5,a)") i,"   [",kv(:,i)," ]"
          end do
        end if
        call get_Order(intSpG, maxorder, order)
        if(allocated(point_op)) deallocate(point_op)
        allocate(point_op(intSpG%multip))
        if(allocated(gen)) deallocate(gen)
        allocate(gen(intSpG%Multip))
        call Get_Generators_From_SpGSymbol(intSpG,gen,point_op,ngen,list_symb_gk)

        write(*,"(//a,i2)")  "  VISIBLE GENERATORS OF SPACE GROUP: ",ngen
        do i=1,ngen
          write(*,"(i4,a,t90,i3)") i,"  "//list_symb_gk(i)//"  "//gen(i)//"  "//trim(intSpG%Symb_Op(point_op(i))),point_op(i)
        end do
        write(*,"(//a,i2)")  "  MAXIMUM ORDER OF INTERSECTION SPACE GROUP: ", maxorder

        do i=1,ngen
          j=point_op(i)
          write(*,"(i4,a,t50,i2)") i,"  "//list_symb_gk(i)//"  "//trim(intSpG%Symb_Op(j)),order(j)
        end do
        !Determine now the possible superspace groups supposing that the propagation vectors are purely magnetic
        !That is magnetic superspace groups of type 4. Just adding the operation 1'(0 0 0 1/2) and adding in the
        !addiditonal dimensions a phase of different types depending on the order of operators
        !Take up to three generators of the Extended Little Group
        !Determine the order

        !Now determine how the propagation vectors transform by the generators

      end do

    End Program Test_ssg_Genk