!!----
!!----
!!----
SubModule (CFML_gSpaceGroups) gS_Get_SubGrp
   implicit none
   Contains
   !!----
   !!---- GET_SUBGROUPS
   !!----
   !!----   This subroutine provides all subgroups of a generic space group
   !!----   It is supposed that the operators have been already ordered, so
   !!----   that if the group is centrosymmetric the position of the corresponding
   !!----   operator is Numops+1, moreover if it is centred the symmetry operators
   !!----   are orderd as:
   !!----   [1...Numops] {-1|t}*[1...Numops] {1|t1}*[1...Numops] {-1|t}*[1 ... Numops]}
   !!----   {1|t2}*[1...Numops] {-1|t}*[1 ... Numops]} ....
   !!----   where t1,t2, ... are the centring translations
   !!----   This ordering facilitates the calculation of the major part of subgroups
   !!----   (excluding those for which less centring translations are considered)
   !!----   If indexg is present only the subgroups of index=indexg are output
   !!----   If point is present, this logical array of dimension (multip,n_subgroups)
   !!----   has its value .true. for the operators of the input SpG that belong to the
   !!----   considered subgroup.
   !!----   The difference if this subroutine with respect to the Get_SubGroups subroutine
   !!----   is that previously to calculating the subgroups, a list of generators of the
   !!----   initial group is created in a better way than Get_SubGroups.
   !!----   (Still testing for determining which one is more complete)
   !!----
   !!----   Updated: 17/04/2020
   !!
   Module Subroutine Get_SubGroups_gen(SpG, SubG, nsg, point,printd)
      !---- Arguments ----!
      type(Spg_Type),                    intent( in) :: SpG
      type(Spg_Type),dimension(:),       intent(out) :: SubG
      integer,                           intent(out) :: nsg
      logical, dimension(:,:),  optional,intent(out) :: point
      logical,                  optional,intent( in) :: printd

      !--- Local variables ---!
      integer  :: i,L,j,k,d, nc, mp,ngen,nla,n,nop,ng,i1,i2,nalloc !,idx
      character (len=80),  dimension(:),allocatable :: gen,gen_min
      character (len=80),  dimension(Spg%Num_Lat)   :: gen_lat
      character (len=2048),dimension(:),allocatable :: list_gen
      character (len=80)                            :: gen_cent, gen_aux

      type(Symm_Oper_Type)                          :: Op_cent, Op_aux
      type(Symm_Oper_Type), dimension(Spg%Num_Lat)  :: Op_lat

      !> Trivial P1: x,y,z
      if (Spg%Multip == 1) then
         SubG(1)=SpG
         nsg=1
         return
      else if (Spg%Multip == 2) then !> Trivial P-1: x,y,z; -x,-y,-z
         Op_aux=Get_Op_from_Symb(SpG%Symb_Op(2))
         if (Is_OP_Inversion_Centre(Op_aux)) then
           SubG(1)=SpG
           nsg=1
           return
         end if
      end if

      !> Test if generators are available
      !> construct a procedure for selecting the minimal set of generators
      if (len_trim(SpG%generators_list) ==  0) then
         Err_CFML%Ierr = 1
         Err_CFML%Msg  = "Get_Subgroups@SPACEG: A list of generators of the parent group is needed!"
         return
      end if

      allocate(gen(SpG%multip))
      d=SpG%d
      ng=0; nc=0
      nop=SpG%numops !number of symmetry operators excluding lattice centrings & centre of symmetry
      if (SpG%centred /= 1) then
         nop=nop*2        ! number of symmetry operators excluding lattice centrings
         nc=SpG%Numops+1  ! Position of the centre of symmetry if it exist
         gen_cent=SpG%Symb_Op(nc)
         call Allocate_Op(SpG%d,Op_cent)
         Op_cent=SpG%Op(nc)  !Operator corresponding to the centre of symmetry
      end if

      nla=0
      if (SpG%num_lat > 0) then
         do i=1,SpG%num_lat
            ng=ng+1
            gen_lat(ng)= SpG%Symb_Op(1+nop*i)
            Op_lat(ng)= SpG%Op(1+nop*i)  !Operators corresponding to the lattice centring vectors
         end do
         nla=ng
      end if

      !> Formation of the list of possible generators extracted from list of generators of
      !> the input Group
      call Get_Generators(SpG%generators_list,d,gen_min, ngen)

      !write(*,"(a)")  "  Input generator list: "//trim(SpG%generators_list)
      !write(*,"(a,i5)")  "  Number of generators: ",ngen
      !do i=1,ngen
      !  write(*,"(i5,a)") i, "  "//trim(gen_min(i))
      !end do

      !>Purge the list of operators eliminating centre of symmetry and Lattice Translations
      n=0
      ng=ngen
      if (allocated(gen)) deallocate(gen)
      if (SpG%centred /= 1) ng=ngen*2
      allocate(gen(ng))
      gen=" "
      do i=1,ngen
         gen_aux=gen_min(i)
         Op_aux=Get_Op_from_Symb(gen_aux)
         if (Is_OP_Inversion_Centre(Op_aux)) cycle
         if (Is_OP_Lattice_Centring(Op_aux)) cycle
         n=n+1
         gen(n) = gen_aux
         if (SpG%centred /= 1) then
            n=n+1
            Op_aux=Op_aux*Op_cent
            gen(n)=Get_Symb_from_Op(Op_aux)
         end if
      end do
      ngen=n
      !mp=max(1,ngen-2)*max(1,ngen-1)*ngen*max(1,ngen-1)*ngen*ngen*max(1,SpG%num_lat)**2
      nalloc=min((ngen**4)*max(1,SpG%num_lat)**2, 180000)+2
      if (allocated(list_gen)) deallocate(list_gen)
      allocate(list_gen(nalloc))

      L=0
      ! Start with the simplest lists: 1 generator
      if (ngen >= 1) then
         do i=1,ngen
            L=L+1
            list_gen(L)=trim(gen(i))
         end do
      end if
      ! Continue with the lists : 2 generators
      if (ngen >= 2) then
         do i=1,ngen-1
            do j=i+1,ngen
               if(trim(gen(i)) == trim(gen(j))) cycle
               L=L+1
               list_gen(L)=trim(gen(i))//";"//trim(gen(j))
            end do
         end do
      end if
      ! Continue with the lists : 3 generators (it is enough for general crystallographic groups)
      if(ngen >= 3) then
         do_3g: do i=1,ngen-2
            do j=i+1,ngen-1
               do k=j+1,ngen
                  if(trim(gen(i)) == trim(gen(j)) .or. trim(gen(i)) == trim(gen(k)) .or. trim(gen(k)) == trim(gen(j))) cycle
                  L=L+1
                  if(L > nalloc) then
                    L=nalloc
                    write(*,"(a)") "Warning: Maximum number of generator lists reached at 3-generators loop"
                    exit do_3g
                  end if
                  list_gen(L)=trim(gen(i))//";"//trim(gen(j))//";"//trim(gen(k))
               end do
            end do
         end do do_3g
      end if

      mp=L

      !do i=1,L
      !  write(*,"(i5,a)") i, "  "//list_gen(i)
      !end do
      !do i=1,SpG%num_lat
      !  write(*,"(i5,a)") i, "  Latt: "//gen_lat(i)
      !end do

      if (SpG%num_lat > 0)  then
         do_jl: do j=1,SpG%num_lat
            do i=1,mp
               if(index(list_gen(i),trim(gen_lat(j))) /= 0) cycle
               L=L+1
               if(L > nalloc) then
                 L=nalloc
                 write(*,"(a)") "Warning: Maximum number of generator lists reached at first Lattice-generators loop"
                 exit do_jl
               end if
               list_gen(L)=trim(list_gen(i))//";"//trim(gen_lat(j))
            end do
         end do  do_jl
      end if
      i1=mp+1
      i2=i1+mp
      if (SpG%num_lat > 1)  then
         do_jla: do j=1,SpG%num_lat
            do i=i1,i2   !1,mp
               if(index(list_gen(i),trim(gen_lat(j))) /= 0) cycle
               L=L+1
               if(L > nalloc) then
                 L=nalloc
                 write(*,"(a)") "Warning: Maximum number of generator lists reached at second Lattice-generators loop"
                 exit do_jla
               end if
               list_gen(L)=trim(list_gen(i))//";"//trim(gen_lat(j))
            end do
         end do do_jla
      end if

      nsg=L

      if(present(printd)) then
        write(unit=*,fmt="(a,i7)") " Number of allocated lists: ",nalloc
        write(unit=*,fmt="(a,i7)") " Number of generator lists: ",nsg
        open(unit=55,file="generator_list_subgroup.txt",status="replace",action="write")
        do i=1,nsg
          write(unit=55,fmt="(a,i6,a)") " Generator list #",i,"  "//trim(List_gen(i))
        end do
        close(unit=55)
      end if

      !> Now generate the subgroups
      n=1
      if(present(printd)) write(*,"(a,2(i4,a))") " => Generating subgroup #",n," with list of generators: ",1,"  "//trim(List_gen(1))
      call Group_Constructor(List_gen(1),SubG(1))
      do_L:do L=2,nsg
         do i=1,n-1
           if(Is_in_Group(list_gen(L),SubG(i))) cycle do_L !Prevent generation of repeated groups
         end do
         n=n+1
         if(present(printd)) write(*,"(a,2(i4,a))") " => Generating subgroup #",n," with list of generators: ",L,"  "//trim(List_gen(L))
         call Group_Constructor(List_gen(L),SubG(n))
         if (Err_CFML%Ierr /= 0) then
           write(*,"(a)") " => Error in Group_Constructor "//trim(Err_CFML%Msg)
           write(*,"(a)") "    Continuing with the next list "
           n=max(1,n-1)
           cycle
         end if
         do i=n-1,1,-1
            if (SubG(n) == SubG(i)) then
               n=max(1,n-1)
               exit
            end if
         end do
      end do do_L
      if(present(printd)) write(unit=*,fmt="(a,i8)") " Number of generator lists: ",nsg
      nsg=n
      !Generate the Hall symbol if d=4
      if(d == 4) then
        do L=1,nsg
          call Get_Generators(SubG(L)%generators_list, d, gen, ngen)
          SubG(L)%Hall=Get_Hall_from_Generators(ngen,Gen(1:ngen))
        end do
      end if
      if (present(point)) then
         point=.false.
         do j=1,nsg
            L=1
            do i=1,SpG%multip
               do k=L,SubG(j)%multip
                  if (SubG(j)%Symb_Op(k) == SpG%Symb_Op(i)) then
                     point(i,j) = .true.
                     L=k+1
                     exit
                  end if
               end do
            end do
         end do
      end if
   End Subroutine Get_SubGroups_gen

   Module Subroutine Get_SubGroups_full(SpG, SubG, nsg, indexg, point,printd)
      !---- Arguments ----!
      type(Spg_Type),                    intent( in) :: SpG
      type(Spg_Type),dimension(:),       intent(out) :: SubG
      integer,                           intent(out) :: nsg
      integer,                  optional,intent(in)  :: indexg
      logical, dimension(:,:),  optional,intent(out) :: point
      logical,                  optional,intent(in)  :: printd

      !--- Local variables ---!
      integer  :: i,L,j,k,d, nc, mp,ngen,nla,n,nop,idx,ng,i1,i2,nalloc,navoid
      character (len=80), dimension(:),allocatable :: gen
      character (len=80), dimension(Spg%Num_Lat)   :: gen_lat
      character (len=256),dimension(:),allocatable :: list_gen
      character (len=80)                           :: gen_cent
      type(Symm_Oper_Type)                         :: Op_cent, Op_aux
      type(Symm_Oper_Type), dimension(Spg%Num_Lat) :: Op_lat
      type(Spg_Type),dimension(size(SubG))         :: Sub_tmp

      !> Trivial P1: x,y,z
      if (Spg%Multip == 1) then
         SubG(1)=SpG
         nsg=1
         return
      else if (Spg%Multip == 2) then !> Trivial P-1: x,y,z; -x,-y,-z
         Op_aux=Get_Op_from_Symb(SpG%Symb_Op(2))
         if (Is_OP_Inversion_Centre(Op_aux)) then
           SubG(1)=SpG
           nsg=1
           return
         end if
      end if

      d=SpG%d
      nc=0
      nop=SpG%numops !number of symmetry operators excluding lattice centrings & centre of symmetry
      if (SpG%centred /= 1) then
         nop=nop*2        ! number of symmetry operators excluding lattice centrings
         nc=SpG%Numops+1  ! Position of the centre of symmetry if it exist
         gen_cent=SpG%Symb_Op(nc)
         call Allocate_Op(SpG%d,Op_cent)
         Op_cent=SpG%Op(nc)  !Operator corresponding to the centre of symmetry
      end if

      ng=0; nla=0
      if (SpG%num_lat > 0) then
         do i=1,SpG%num_lat
            ng=ng+1
            gen_lat(ng)= SpG%Symb_Op(1+nop*i)
            Op_lat(ng)= SpG%Op(1+nop*i)  !Operators corresponding to the lattice centring vectors
         end do
         nla=ng
      end if

      allocate(gen(SpG%Multip))
      do i=2,SpG%Numops
        gen(i-1) = SpG%Symb_Op(i)
      end do
      ngen=SpG%Numops-1
      if (SpG%centred /= 1) then
        do i=SpG%Numops+1,2*SpG%Numops
           ngen=ngen+1
           gen(ngen) = SpG%Symb_Op(i)
        end do
      end if
      if (SpG%centred /= 1) then
        ngen=ngen+1
        gen(ngen)=gen_cent
      end if


      !Construct the lists of  possible generators to be tested
      !mp=max(1,ngen-2)*max(1,ngen-1)*ngen*max(1,ngen-1)*ngen*ngen*max(1,SpG%num_lat)**2
      nalloc=min((ngen**4)*max(1,SpG%num_lat)**2, 1800000)+2
      if (allocated(list_gen)) deallocate(list_gen)
      allocate(list_gen(nalloc))
      L=0
      ! Start with the simplest lists: 1 generator
      if (ngen >= 1) then
         do i=1,ngen
            L=L+1
            list_gen(L)=trim(gen(i))
         end do
      end if
      ! Continue with the lists : 2 generators
      if (ngen >= 2) then
         do i=1,ngen-1
            do j=i+1,ngen
               if(trim(gen(i)) == trim(gen(j))) cycle
               L=L+1
               list_gen(L)=trim(gen(i))//";"//trim(gen(j))
            end do
         end do
      end if
      ! Continue with the lists : 3 generators (it is enough for general crystallographic groups)
      if(ngen >= 3) then
         do_3g: do i=1,ngen-2
            do j=i+1,ngen-1
               do k=j+1,ngen
                  if(trim(gen(i)) == trim(gen(j)) .or. trim(gen(i)) == trim(gen(k)) .or. trim(gen(k)) == trim(gen(j))) cycle
                  L=L+1
                  if(L > nalloc) then
                    L=nalloc
                    write(*,"(a)") "Warning: Maximum number of generator lists reached at 3-generators loop"
                    exit do_3g
                  end if
                  list_gen(L)=trim(gen(i))//";"//trim(gen(j))//";"//trim(gen(k))
               end do
            end do
         end do do_3g
      end if
      mp=L

      if (SpG%num_lat > 0)  then
         do_jl: do j=1,SpG%num_lat
            do i=1,mp
               if(index(list_gen(i),trim(gen_lat(j))) /= 0) cycle
               L=L+1
               if(L > nalloc) then
                 L=nalloc
                 write(*,"(a)") "Warning: Maximum number of generator lists reached at first Lattice-generators loop"
                 exit do_jl
               end if
               list_gen(L)=trim(list_gen(i))//";"//trim(gen_lat(j))
            end do
         end do  do_jl
      end if
      i1=mp+1
      i2=i1+mp
      if (SpG%num_lat > 1)  then
         do_jla: do j=1,SpG%num_lat
            do i=i1,i2   !1,mp
               if(index(list_gen(i),trim(gen_lat(j))) /= 0) cycle
               L=L+1
               if(L > nalloc) then
                 L=nalloc
                 write(*,"(a)") "Warning: Maximum number of generator lists reached at second Lattice-generators loop"
                 exit do_jla
               end if
               list_gen(L)=trim(list_gen(i))//";"//trim(gen_lat(j))
            end do
         end do do_jla
      end if

      nsg=L !Number of lists with generators

      if(present(printd)) then
        write(unit=*,fmt="(a,i7)") " Number of allocated lists: ",nalloc
        write(unit=*,fmt="(a,i7)") " Number of generator lists: ",nsg
        open(unit=55,file="generator_list_subgroup_full.txt",status="replace",action="write")
        do i=1,nsg
          write(unit=55,fmt="(a,i6,a)") " Generator list #",i,"  "//trim(List_gen(i))
        end do
        close(unit=55)
      end if
      !> Generate the subgroups corresponding to the above lists
      n=1
      if(present(printd)) write(*,"(a,2(i4,a))") " => Generating subgroup #",n," with list of generators: ",1,"  "//trim(List_gen(1))
      call Group_Constructor(List_gen(1),Sub_tmp(1))
      navoid=0
      do_L:do L=2,nsg
         do i=n-1,1,-1
           if(Is_in_Group(list_gen(L),Sub_tmp(i))) cycle do_L !Prevent generation of repeated groups
         end do
         n=n+1 !new group
         if(present(printd)) write(*,"(a,2(i8,a))") " => Generating subgroup #",n," with list of generators: ",L,"  "//trim(List_gen(L))
         call Group_Constructor(List_gen(L),Sub_tmp(n))
         if (Err_CFML%Ierr /= 0) then
           write(*,"(a)") " => Error in Group_Constructor "//trim(Err_CFML%Msg)
           write(*,"(a)") "    Continuing with the next list "
           n=max(1,n-1)
           cycle do_L
         end if
         do i=n-1,1,-1  !Check for repetitions in any case
            if (Sub_tmp(n) == Sub_tmp(i)) then
               n=max(1,n-1)
               if(present(printd))  write(*,"(a,i4)") "     Repeated group, new 'n' ",n
               cycle do_L
            end if
         end do
      end do do_L

      if(present(printd)) write(unit=*,fmt="(a,i8)") " Number of generator lists: ",nsg
      L=0
      if (present(indexg)) then
        do i=1,n
          idx=SpG%multip/Sub_tmp(i)%multip
          if (idx == indexg) then
            L=L+1
            SubG(L)=Sub_tmp(i)
          end if
        end do
        nsg=L
      else
        nsg=n
        do i=1,n
          SubG(i)=Sub_tmp(i)
        end do
      end if

      !Generate the Hall symbol if d=4
      if(d == 4) then
        do L=1,nsg
          call Get_Generators(SubG(L)%generators_list, d, gen, ngen)
          SubG(L)%Hall=Get_Hall_from_Generators(ngen,Gen(1:ngen))
        end do
      end if

      if (present(point)) then
         point=.false.
         do j=1,nsg
            L=1
            do i=1,SpG%multip
               do k=L,SubG(j)%multip
                  if (SubG(j)%Symb_Op(k) == SpG%Symb_Op(i)) then
                     point(i,j) = .true.
                     L=k+1
                     exit
                  end if
               end do
            end do
         end do
      end if

   End Subroutine Get_SubGroups_full

   Function Is_in_Group(list_gen,Spg) result(it_is)
     character(len=*),intent(in) :: list_gen
     type(Spg_Type),  intent(in) :: SpG
     logical                     :: it_is
     !
     integer :: i,ngen,d
     character(len=80), dimension(:),allocatable :: gen
     character(len=:),allocatable :: all_ops

     all_ops=trim(Spg%Symb_Op(2))//";"
     do i=3,Spg%multip
       all_ops=trim(all_ops)//trim(Spg%Symb_Op(i))//";"
     end do
     it_is=.true.
     call Get_Generators(list_gen, d, gen, ngen)
     do i=1,ngen
       if(index(all_ops,trim(gen(i))) == 0) then
         it_is=.false.
         return
       end if
     end do
   End Function Is_in_Group

End SubModule gS_Get_SubGrp