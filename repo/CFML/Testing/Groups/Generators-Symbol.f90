  Module getting_ssg_generators
    use CFML_GlobalDeps
    use CFML_gSpaceGroups
    use CFML_Rational
    use CFML_Symmetry_Tables,   only : Get_HM_Compact_HM
    use CFML_Strings,           only : String_Fraction_2Dig,l_case,get_words

    implicit none

    character(len=2), dimension(11) :: screw=["21","31","32","41","42","43","61","62","63","64","65"]
    character(len=1), dimension(11) :: rm_screw=["2","3","3","4","4","4","6","6","6","6","6"]

    contains


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

      write(*,"(10a)") " => Final Symbol to be analysed: ",(trim(sgen(j))//" ",j=1,ngen)

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
                     if(index(op_symb(i),"(") /=0 .and. (index(op_symb(i),"x,0") /= 0 .or. index(op_symb(i),"x,1") /= 0)) then
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

           write(*,"(a,4i4)") " Pointers to operators: ", (point_op(i),i=1,ngen)
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
      !ngen=n
    End Subroutine Get_Generators_From_SpGSymbol

    Subroutine Get_Generators_From_SpGSymbol_inter(SpG,gen,point_op,ngen,List_Symb)
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

      write(*,"(10a)") " => Final Symbol to be analysed: ",(trim(sgen(j))//" ",j=1,ngen)

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
                 point_op(j)=i
                 done(i)=.true.
                 List_Symb(j) = trim(List_Symb(j))//dire(j)
                 exit
               end if
             end do
           end do

         Case("Tetragonal")

           !Check the direction of the principal 4-fold rotation axis if it exist
           dir=0
           if(index(sgen(1),"4+") /= 0) then
             do i=2,SpG%Multip
               if(index(op_symb(i),trim(sgen(1))) /= 0 ) then
                  point_op(1)=i
                  done(i)=.true.
                  if(index(op_symb(i),"x") /= 0) then
                    dir=1
                  else if(index(op_symb(i),"y") /= 0) then
                    dir=2
                  else if(index(op_symb(i),"z") /= 0) then
                    dir=3
                  end if
                  List_Symb(1) = trim(List_Symb(1))//dire(dir)
                  exit
               end if
             end do
           end if
           if(ngen > 1) then
             !Now analyse the second generator that may be perpendicular to the 4-fold axis or along ab if it is 4_z

             if(sgen(2)(1:1) == "2")  then

               do i=2,SpG%Multip
                 if(done(i)) cycle
                 if(op_symb(i)(1:1) == "2" ) then
                   if(index(op_symb(i),"x") /= 0 .and. dir == 1) cycle
                   if(index(op_symb(i),"y") /= 0 .and. dir == 2) cycle
                   if(index(op_symb(i),"z") /= 0 .and. dir == 3) cycle
                   point_op(2)=i
                   done(i)=.true.
                   j=index(op_symb(i),")")
                   if( j == 0) j=index(op_symb(i)," ")
                   List_Symb(2) = trim(List_Symb(2))//"_"//adjustl(op_symb(i)(j+1:))
                   exit
                 end if
               end do

             else if (index(sgen(2),"m") /= 0 .or. index(sgen(2),"a") /= 0 .or. index(sgen(2),"d") /= 0 .or. index(sgen(2),"n") /= 0)  then

               if(slash) then
                 do i=2,SpG%Multip
                   if(done(i)) cycle
                   if(index(op_symb(i),trim(sgen(2))) /= 0 ) then

                     Select Case(dir)
                       Case(1)
                         if(index(op_symb(i),",y,z") /= 0) then
                           point_op(2)=i
                           done(i)=.true.
                           List_Symb(2) = trim(List_Symb(2))//dire(1)
                           exit
                         end if
                       Case(2)
                         if(index(op_symb(i),"x,") /= 0 .and.  index(op_symb(i),",z") /= 0) then
                           point_op(2)=i
                           done(i)=.true.
                           List_Symb(2) = trim(List_Symb(2))//dire(2)
                           exit
                         end if
                       Case(3)
                         if(index(op_symb(i),"x,y,") /= 0) then
                           point_op(2)=i
                           done(i)=.true.
                           List_Symb(2) = trim(List_Symb(2))//dire(3)
                           exit
                         end if
                     End Select
                   end if
                 end do

               else !directions ab,bc or ac

                 do i=2,SpG%Multip
                   if(done(i)) cycle
                   if(index(op_symb(i),trim(sgen(2))) /= 0 ) then
                     Select Case(dir)
                       Case(1)
                         if(index(op_symb(i),"y,z") /= 0) then
                           point_op(2)=i
                           done(i)=.true.
                           List_Symb(2) = trim(List_Symb(2))//dire(2)
                           exit
                         else if(index(op_symb(i),",z") /= 0) then
                           point_op(2)=i
                           done(i)=.true.
                           List_Symb(2) = trim(List_Symb(2))//dire(3)
                           exit
                         end if
                       Case(2)
                         if(index(op_symb(i),"x,") /= 0 .and. index(op_symb(i),",z") /= 0) then
                           point_op(2)=i
                           done(i)=.true.
                           List_Symb(2) = trim(List_Symb(2))//dire(1)
                           exit
                         else if(index(op_symb(i),",y,z") /= 0) then
                           point_op(2)=i
                           done(i)=.true.
                           List_Symb(2) = trim(List_Symb(2))//dire(3)
                           exit
                         end if
                       Case(3)
                         if(index(op_symb(i),"y,z") /= 0 ) then
                           point_op(2)=i
                           done(i)=.true.
                           List_Symb(2) = trim(List_Symb(2))//dire(1)
                           exit
                         else if(index(op_symb(i),"x,") /= 0 .and. index(op_symb(i),",z") /= 0) then
                           point_op(2)=i
                           done(i)=.true.
                           List_Symb(2) = trim(List_Symb(2))//dire(2)
                           exit
                         end if
                     End Select
                   end if
                 end do
               end if
             end if
           end if

           if(ngen > 2) then !Now analyse the third generator
             if(slash) then  !directions along a,b if 4_z
               if(sgen(3)(1:1) == "2")  then
                  do i=2,SpG%Multip
                    if(done(i)) cycle
                    if(op_symb(i)(1:1) == "2" ) then
                      if(index(op_symb(i),"x") /= 0 .and. dir == 1) cycle
                      if(index(op_symb(i),"y") /= 0 .and. dir == 2) cycle
                      if(index(op_symb(i),"z") /= 0 .and. dir == 3) cycle
                      point_op(3)=i
                      done(i)=.true.
                      j=index(op_symb(i),")")
                      if( j == 0) j=index(op_symb(i)," ")
                      List_Symb(3) = trim(List_Symb(3))//"_"//adjustl(op_symb(i)(j+1:))
                      exit
                    end if
                  end do

               else if(index(sgen(3),"m") /= 0 .or. index(sgen(3),"a") /= 0 .or. index(sgen(3),"d") /= 0 .or. index(sgen(3),"n") /= 0)  then

                  do i=2,SpG%Multip
                    if(done(i)) cycle
                    if(index(op_symb(i),trim(sgen(3))) /= 0 ) then
                      Select Case(dir)
                        Case(1)
                          if(index(op_symb(i),"x,") /= 0) then
                            point_op(3)=i
                            done(i)=.true.
                            exit
                          end if
                        Case(2)
                          if(index(op_symb(i),",y,") /= 0 ) then
                            point_op(3)=i
                            done(i)=.true.
                            exit
                          end if
                        Case(3)
                          if(index(op_symb(i),",z") /= 0 ) then
                            point_op(2)=i
                            done(i)=.true.
                            exit
                          end if
                      End Select
                    end if
                  end do

               end if

             else            !directions along x,x,0 if 4_z

               if(sgen(3)(1:1) == "2")  then
                  do i=2,SpG%Multip
                    if(done(i)) cycle
                    if(op_symb(i)(1:1) == "2" ) then
                      if(index(op_symb(i),"x,") /= 0  .and. dir == 1) cycle
                      if(index(op_symb(i),",y,") /= 0 .and. dir == 2) cycle
                      if(index(op_symb(i),",z") /= 0  .and. dir == 3) cycle
                      point_op(3)=i
                      done(i)=.true.
                      j=index(op_symb(i),")")
                      if( j == 0) j=index(op_symb(i)," ")
                      List_Symb(3) = trim(List_Symb(3))//"_"//adjustl(op_symb(i)(j+1:))
                      exit
                    end if
                  end do

               else if(index(sgen(3),"m") /= 0 .or. index(sgen(3),"a") /= 0 .or. index(sgen(3),"d") /= 0 .or. index(sgen(3),"n") /= 0)  then

                  do i=2,SpG%Multip
                    if(done(i)) cycle
                    if(index(op_symb(i),trim(sgen(3))) /= 0 ) then
                      Select Case(dir)
                        Case(1)
                          if(index(op_symb(i),"x,y,y") /= 0) then
                            point_op(3)=i
                            done(i)=.true.
                            exit
                          end if
                        Case(2)
                          if(index(op_symb(i),",x,") /= 0 ) then
                            point_op(3)=i
                            done(i)=.true.
                            exit
                          end if
                        Case(3)
                          if(index(op_symb(i),"x,-x,z") /= 0 ) then
                            point_op(2)=i
                            done(i)=.true.
                            exit
                          end if
                      End Select
                    end if
                  end do

               end if

             end if
           end if
           if(ngen > 3) then !Now analyse the fourth generator !directions along x,x,0 if 4_z

           end if

         Case("Trigonal")
           !Check the direction of the principal 3-fold rotation axis if it exist
           dir=0
           if(index(sgen(1),"3")/=0 .or. index(sgen(1),"4")/=0 .or. index(sgen(1),"6")/=0) then
             j=point_op(1)
             if(index(op_symb(j),"x") /= 0) then
               dir=1
             else if(index(op_symb(j),"y") /= 0) then
               dir=2
             else if(index(op_symb(j),"z") /= 0) then
               dir=3
             end if
           end if
         Case("Hexagonal")
         Case("Cubic")

      End Select

      if(any(point_op(1:ngen) == 0)) then !Look for unidentified operators (glide planes in monoclinic or orthorhombic systems)

        if(SpG%crystalsys == "Monoclinic" .or. SpG%crystalsys == "Orthorhombic") then
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
                 end if
               end do
             end if
          end do
        end if

      end if

      if(any(point_op(1:ngen) == 0)) then !Look for unidentified operators (glide planes at faces)
        do j=1,ngen
           i=point_op(j)
           if(i == 0) then
             do i=2,SpG%Multip
               if(done(i)) cycle
               if(index(op_symb(i),"g") /= 0) then  !assume that it is a glide plane "g" perpendicular to c
                  if(index(op_symb(i),"x,y,0") /= 0 .and. dir == 3) then  !assume that it is a glide plane "g" perpendicular to c
                    point_op(j)=i
                    done(i)=.true.
                    List_Symb(j)=trim(List_Symb(j))//"-gc0"
                    exit
                  else if(index(op_symb(i),"0,y,z") /= 0 .and. dir == 1) then !assume that it is a glide plane "g" perpendicular to a or containing y and z
                    point_op(j)=i
                    done(i)=.true.
                    List_Symb(j)=trim(List_Symb(j))//"-ga0"
                    exit
                  else if(index(op_symb(i),"x,0,z") /= 0  .and. dir == 2) then !assume that it is a glide plane "g" perpendicular to b or containing x and z
                    point_op(j)=i
                    done(i)=.true.
                    List_Symb(j)=trim(List_Symb(j))//"-gb0"
                    exit
                  end if
               end if
             end do
           end if
        end do
      end if

      if(any(point_op(1:ngen) == 0)) then !Look for unidentified operators (glide planes out of faces)
        do j=1,ngen
           i=point_op(j)
           if(i==0) then
             do i=2,SpG%Multip
               if(done(i)) cycle
               if(index(op_symb(i),"g") /= 0) then  !assume that it is a glide plane "g" perpendicular to c
                  if(index(op_symb(i),"x,y,") /= 0 ) then  !assume that it is a glide plane "g" perpendicular to c
                    point_op(j)=i
                    done(i)=.true.
                    List_Symb(j)=trim(List_Symb(j))//"-gc"
                    exit
                  else if(index(op_symb(i),",y,z") /= 0 ) then !assume that it is a glide plane "g" perpendicular to a or containing y and z
                    point_op(j)=i
                    done(i)=.true.
                    List_Symb(j)=trim(List_Symb(j))//"-ga"
                    exit
                  else if(index(op_symb(i),"x,") /= 0 .and. index(op_symb(i),",z") /= 0) then !assume that it is a glide plane "g" perpendicular to b or containing x and z
                    point_op(j)=i
                    done(i)=.true.
                    List_Symb(j)=trim(List_Symb(j))//"-gb"
                    exit
                  end if
               end if
             end do
           end if
        end do
      end if

      n=0
      do j=1,ngen
        i=point_op(j)
        if(i == 0) cycle
        n=n+1
        gen(n)=SpG%Symb_Op(i)
      end do
      ngen=n
    End Subroutine Get_Generators_From_SpGSymbol_inter

    Subroutine Get_Generators_From_SpGSymbol_old(SpG,gen,point_op,ngen,List_Symb)
      class(SpG_Type),                intent (in) :: SpG
      Character(len=*), dimension(:), intent(out) :: gen
      integer,          dimension(:), intent(out) :: point_op
      integer,                        intent(out) :: ngen
      character(len=*), dimension(:), intent(out) :: List_symb
      !--- Local variables ---!
      integer                                   :: i,j,n,m,dir
      integer,          dimension(3,3)          :: s
      real(kind=cp),    dimension(3)            :: t
      character(len=6), dimension(8)            :: sgen,msgen
      character(len=20)                         :: spg_symb
      character(len=60),dimension(SpG%Multip)   :: op_symb
      logical,          dimension(SpG%Multip)   :: done
      character(len=2), dimension(11)           :: screw=["21","31","32","41","42","43","61","62","63","64","65"]
      character(len=1), dimension(11)           :: rm_screw=["2","3","3","4","4","4","6","6","6","6","6"]

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

      do j=1,ngen
        !write(*,"(a)")  "  => Looking for generator   "//trim(sgen(j))
        do i=2,SpG%Multip
          if(done(i)) cycle
          !write(*,"(i4,a)") i,"     "//trim(op_symb(i))
          if(index(op_symb(i),trim(sgen(j))) /= 0 ) then
            point_op(j)=i
            done(i)=.true.
            exit
          end if
        end do
      end do

      if(any(point_op(1:ngen) == 0)) then !Look for unidentified operators (glide planes in monoclinic or orthorhombic systems)
        if(SpG%crystalsys == "Monoclinic" .or. SpG%crystalsys == "Orthorhombic") then
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
                 end if
               end do
             end if
          end do
        end if
      end if
      !Check the direction of the principal rotation axis if it exist
      dir=0
      if(index(sgen(1),"3")/=0 .or. index(sgen(1),"4")/=0 .or. index(sgen(1),"6")/=0) then
        j=point_op(1)
        if(index(op_symb(j),"x") /= 0) then
          dir=1
        else if(index(op_symb(j),"y") /= 0) then
          dir=2
        else if(index(op_symb(j),"z") /= 0) then
          dir=3
        end if
      end if

      if(any(point_op(1:ngen) == 0)) then !Look for unidentified operators (glide planes at faces)
        do j=1,ngen
           i=point_op(j)
           if(i == 0) then
             do i=2,SpG%Multip
               if(done(i)) cycle
               if(index(op_symb(i),"g") /= 0) then  !assume that it is a glide plane "g" perpendicular to c
                  if(index(op_symb(i),"x,y,0") /= 0 .and. dir == 3) then  !assume that it is a glide plane "g" perpendicular to c
                    point_op(j)=i
                    done(i)=.true.
                    List_Symb(j)=trim(List_Symb(j))//"-gc0"
                    exit
                  else if(index(op_symb(i),"0,y,z") /= 0 .and. dir == 1) then !assume that it is a glide plane "g" perpendicular to a or containing y and z
                    point_op(j)=i
                    done(i)=.true.
                    List_Symb(j)=trim(List_Symb(j))//"-ga0"
                    exit
                  else if(index(op_symb(i),"x,0,z") /= 0  .and. dir == 2) then !assume that it is a glide plane "g" perpendicular to b or containing x and z
                    point_op(j)=i
                    done(i)=.true.
                    List_Symb(j)=trim(List_Symb(j))//"-gb0"
                    exit
                  end if
               end if
             end do
           end if
        end do
      end if

      if(any(point_op(1:ngen) == 0)) then !Look for unidentified operators (glide planes out of faces)
        do j=1,ngen
           i=point_op(j)
           if(i==0) then
             do i=2,SpG%Multip
               if(done(i)) cycle
               if(index(op_symb(i),"g") /= 0) then  !assume that it is a glide plane "g" perpendicular to c
                  if(index(op_symb(i),"x,y,") /= 0 ) then  !assume that it is a glide plane "g" perpendicular to c
                    point_op(j)=i
                    done(i)=.true.
                    List_Symb(j)=trim(List_Symb(j))//"-gc"
                    exit
                  else if(index(op_symb(i),",y,z") /= 0 ) then !assume that it is a glide plane "g" perpendicular to a or containing y and z
                    point_op(j)=i
                    done(i)=.true.
                    List_Symb(j)=trim(List_Symb(j))//"-ga"
                    exit
                  else if(index(op_symb(i),"x,") /= 0 .and. index(op_symb(i),",z") /= 0) then !assume that it is a glide plane "g" perpendicular to b or containing x and z
                    point_op(j)=i
                    done(i)=.true.
                    List_Symb(j)=trim(List_Symb(j))//"-gb"
                    exit
                  end if
               end if
             end do
           end if
        end do
      end if

      n=0
      do j=1,ngen
        i=point_op(j)
        if(i == 0) cycle
        n=n+1
        gen(n)=SpG%Symb_Op(i)
      end do
      ngen=n
    End Subroutine Get_Generators_From_SpGSymbol_old

    Subroutine Get_Set_of_Generators(SpG,gen,point_op,ngen,List_Symb)
      class(SpG_Type),                intent (in) :: SpG
      Character(len=*), dimension(:), intent(out) :: gen
      integer,          dimension(:), intent(out) :: point_op
      integer,                        intent(out) :: ngen
      character(len=*), dimension(:), intent(out) :: List_symb
      !--- Local variables ---!
      integer                                   :: i,j,n,m !,e_numops
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
                     !Select Case(j)
                     !  Case(1)
                     !     if(trim(sgen(j)) == "2" .and. index(op_symb(i),"x,") /= 0) then
                     !       point_op(j)=i
                     !       done(i)=.true.
                     !       List_Symb(j)=trim(List_Symb(j))//"_x"
                     !     else if(trim(sgen(j)) /= "2" .and. index(op_symb(i),",y,x") /= 0) then
                     !       point_op(j)=i
                     !       done(i)=.true.
                     !       List_Symb(j)=trim(List_Symb(j))//"_x"
                     !     end if
                     !  Case(2)
                     !     if(trim(sgen(j)) == "2" .and. index(op_symb(i),",y,") /= 0) then
                     !       point_op(j)=i
                     !       done(i)=.true.
                     !       List_Symb(j)=trim(List_Symb(j))//"_y"
                     !     else if(trim(sgen(j)) /= "2" .and. index(op_symb(i),"x,") /= 0 .and. index(op_symb(i),",z") /= 0) then
                     !       point_op(j)=i
                     !       done(i)=.true.
                     !       List_Symb(j)=trim(List_Symb(j))//"_y"
                     !     end if
                     !  Case(3)
                     !     if(trim(sgen(j)) == "2" .and. index(op_symb(i),",z") /= 0) then
                     !       point_op(j)=i
                     !       done(i)=.true.
                     !       List_Symb(j)=trim(List_Symb(j))//"_z"
                     !     else if(trim(sgen(j)) /= "2" .and. index(op_symb(i),"x,y,") /= 0 ) then
                     !       point_op(j)=i
                     !       done(i)=.true.
                     !       List_Symb(j)=trim(List_Symb(j))//"_z"
                     !     end if
                     !End Select
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
          !do i=2,SpG%Multip
          !  if(index(op_symb(i),trim(sgen(1))) /= 0 ) then
          !    point_op(1)=i
          !    done(i)=.true.
          !    !Determination of the direction of ternary axis
          !    if(index(op_symb(i),",z") /= 0 ) then
          !    else if(index(op_symb(i),"x,x,x") /= 0 ) then
          !    end if
          !    exit
          !  end if
          !end do
        Case("Tetragonal")
        Case("Hexagonal")
        Case("Cubic")
      End Select
    End Subroutine Get_Set_of_Generators

  End Module getting_ssg_generators

    Program Get_Generators_SPG_Symbol
      Use CFML_GlobalDeps
      Use CFML_gSpaceGroups
      use CFML_Rational
      use getting_ssg_generators
      use CFML_Symmetry_Tables
      implicit none
      integer                                    :: i,n,k,iout,ngen,ierror
      character(len=40)                          :: Str_tmp
      class(SpG_Type),   allocatable             :: SpG
      integer,           dimension(:),allocatable:: point_op
      character(len=40), dimension(:),allocatable:: gen,sym_symb
      character(len=10), dimension(6)            :: list_symb_or
      integer,           dimension(3,3)          :: s
      real(kind=cp),     dimension(3)            :: t

      call Set_Spgr_Info()
      open(newunit=iout,file="Generators.txt",status="replace",action="write")
      open(newunit=ierror,file="Generators_wrong.txt",status="replace",action="write")
      ! Up to 409: triclinic, monoclinic, orthorhombic
      ! 410:494 : Tetragonal
      ! 495:526 : Trigonal
      ! 527:553 : Hexagonal
      ! 554:612 : Cubic
      allocate(SPG_Type :: SpG)
      call Init_SpaceGroup(SpG)
      do n=1,612  !NUM_SPGR_INFO
        Str_tmp=spgr_info(n)%HM
        call Set_SpaceGroup(Str_tmp,SpG)
        if(Err_CFML%Ierr /= 0) then
          write(unit=*,fmt="(a)") " => "//trim(Err_CFML%Msg)
          cycle
        end if
        if(allocated(point_op)) deallocate(point_op)
        allocate(point_op(SpG%Multip))
        if(allocated(gen)) deallocate(gen)
        allocate(gen(SpG%Multip))
        if(allocated(sym_symb)) deallocate(sym_symb)
        allocate(sym_symb(SpG%Multip))
        do k=1,SpG%Multip
           s=SpG%Op(k)%Mat(1:3,1:3)
           t=SpG%Op(k)%Mat(1:3,4)
           sym_symb(k) = Symmetry_Symbol(s,t)
        end do
        call Get_Generators_From_SpGSymbol(SpG,gen,point_op,ngen,list_symb_or)
        !call Get_Set_of_Generators(SpG,gen,point_op,ngen,list_symb_or)
        write(*,"(//a,t70,i6)")  "  VISIBLE GENERATORS OF SPACE GROUP: "//trim(Str_tmp),SpG%numspg
        write(iout,"(//a,t70,i6)")  "  VISIBLE GENERATORS OF SPACE GROUP: "//trim(Str_tmp),SpG%numspg

        if(any(point_op(1:ngen) == 0)) then
           write(ierror,"(/,a,i8)") "  Error for Space Group: "//trim(Str_tmp),SpG%numspg
           do i=1,ngen
             if(point_op(i) /= 0) then
               write(ierror,"(i4,a,t50,i3)") i,"  "//list_symb_or(i)//"  "//gen(i),point_op(i)
             else
               write(ierror,"(i4,a,t50,i3)") i,"  => Error: "//list_symb_or(i)//" not found ",point_op(i)
             end if
           end do
        end if

        do i=1,ngen
          if(point_op(i) /= 0) then
            k=point_op(i)
            write(*,"(i4,a,t50,i3,a)") i,"  "//list_symb_or(i)//"  "//gen(i),point_op(i),"  "//trim(sym_symb(k))
            write(iout,"(i4,a,t50,i3,a)") i,"  "//list_symb_or(i)//"  "//gen(i),point_op(i),"  "//trim(sym_symb(k))
          else
            write(*,"(i4,a,t50,i3)") i,"  => Error: "//list_symb_or(i)//" not found ",point_op(i)
          end if
        end do
           write(iout,"(/,a)") " Symmetry operators and symmetry symbols:"
        do i=2,Spg%Multip
           write(iout,"(i4,a,t50,a)") i,"  "//trim(Spg%Symb_Op(i)) ,trim(sym_symb(i))
        end do
      end do
    End Program Get_Generators_SPG_Symbol