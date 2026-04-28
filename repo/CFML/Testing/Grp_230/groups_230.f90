!!----
!!---- Test 230 Standard Groups
!!----
!!----
!!
Program test230_groups
   !---- Use Modules ----!
   use CFML_GlobalDeps
   use CFML_Symmetry_Tables
   use CFML_Strings
   use CFML_gSpaceGroups

   !---- Local Variables ----!
   character(len=256)                  :: generatorList
   character(len=5)                    :: aux
   character(len=15)                   :: forma,hm
   integer                             :: i,j,L,nsg,lun,nc,inig,fing,ier,minuts
   integer, dimension(:), allocatable  :: cosets
   real(kind=cp)                       :: start, fin,par,secs

   type(Spg_Type)                      :: Grp
   type(Spg_Type), dimension(4096)     :: sGrp

   !> Init
   call Set_Conditions_NumOp_EPS(2048) ! Maximum admissible multiplicity
   write(*,"(a)",advance="no") " => Enter the numbers of the space groups to be analysed (two integers <=230): "
   read(*,*,iostat=ier) inig,fing
   if(ier /= 0) then
     inig=1
     fing=230
   end if

   !> Start Time
   call CPU_TIME(start)

   !> Output results
   open(newunit=lun,file="test_230groups.out",status="replace",action="write")
   do i=inig,fing
      call CPU_TIME(par)
      write(unit=aux,fmt="(i3)") i
      generatorList=" "
      generatorList=adjustl(Get_IT_Generators(aux))
      if (len_trim(generatorList) == 0) exit
      !write(*,'(a)') " => Calling Group_Constructor "//"for group: "//trim(aux)
      !write(*,"(a)")  " => Generator List: "//trim(generatorList)
      call Group_Constructor(generatorList,Grp)
      !write(*,'(a)') " => Out of Group_Constructor "//"for group: "//trim(aux)

      Grp%numspg=i
      call Get_SpaceGroup_Symbols(aux,HM=hm)
      Grp%spg_symb=hm(1:1)//l_case(hm(2:))
      if (Err_CFML%Ierr /= 0) then
          write(lun,'(i4,a)') i," "//trim(Err_CFML%Msg)
          cycle
      else
          call Identify_Group(Grp)
          if (Err_CFML%Ierr /= 0) then
              write(lun,'(/,a,i4,a)') "  Group number: ",i, &
                                      " => Error in the identification of the group: "//trim(Err_CFML%Msg)
          end if
          write(lun,"(/,a)") "  ------------------------------------------------------------------------------"
          write(lun,"(a,i4, a20,a)") "  Group number: ",i,"  Symb: "//trim(Grp%Spg_symb),&
                                     "  Transf. to standard: "//trim(Grp%mat2std_shu)
          write(lun,"(a)")   "  ------------------------------------------------------------------------------"
      end if

      !write(*,"(a)")  " => Grp%spg_symb: "//trim(Grp%spg_symb)
      !call Write_SpaceGroup_Info(Grp)
      call Get_SubGroups_full(Grp, sGrp, nsg)

      if (Err_CFML%Ierr /= 0) then
         write(lun,'(/,a)') " => Error in the generation of subgroups: "//trim(Err_CFML%Msg)
         write(*,'(/,a)') " => Error in the generation of subgroups: "//trim(Err_CFML%Msg)
      end if
      if (nsg > 0) Then
         write(lun,"(a,i4)") "  Total number of subgroups: ",nsg
         do L=1,nsg
            call Identify_Group(sGrp(L))
            if (Err_CFML%Ierr /= 0) then
               write(lun,'(/,a,i4,a)') " => Error in the identification of the sub-group: ",L,"  => "//trim(Err_CFML%Msg)
               write(*,'(a,i4,a)') " => Error in the identification of the sub-group: ",L,"  => "//trim(Err_CFML%Msg)
            end if !else
               write(lun,"(/,2(a,i4),a20,a)") "  Sub-Group Number #",L, " of index: ",Grp%multip/sGrp(L)%multip, &
               "  Symb: "//trim(sGrp(L)%BNS_symb),"  Transf. to standard: "//trim(sGrp(L)%mat2std_shu)//         &
               "    Generators: "//trim(sGrp(L)%generators_list)

               !> Write the coset decomposition of Grp with respect to the current subgroup
               call Get_Cosets(Grp,sGrp(L),cosets)
               nc=size(cosets)
               if (nc > 0) then
                  !     12345678901234
                  forma="(a,i3,a,    a)"
                  write(forma(9:12),"(i4)") nc
                  write(lun,forma) "  Coset decomposition of  G: "//trim(Grp%Spg_symb)//&
                                   "(",nc+1,") =  H("//trim(sGrp(L)%BNS_symb)//")  + ",("{"//&
                                   trim(Grp%Symb_Op(cosets(j)))//"} H + ",j=1,nc-1), &
                                   "{"//trim(Grp%Symb_Op(cosets(nc)))//"} H"
               end if
            !end if
         end do
      end if
      call CPU_TIME(fin)
      write(*,"(a,i4,t25,a,t45,a,t95,i4,a,t115,a,f8.3,a)") " Group number: ",i,"Symb: "//trim(Grp%Spg_symb),        &
                                                       "Transf. to standard: "//trim(Grp%mat2std_shu)//" with ",&
                                                       nsg," subgroups","CPU-time: ",fin-par," seconds"
   end do
   call CPU_TIME(fin)
   minuts=int((fin-start)/60.0)
   secs=((fin-start)/60.0-real(minuts))*60.0
   write(*,"(/,a,i3,a,f9.5,a)")   "  CPU_Time for all calculations: ",minuts," minutes, ",secs," seconds"
   write(lun,"(/,a,i3,a,f9.5,a)") "  CPU_Time for all calculations: ",minuts," minutes, ",secs," seconds"

   contains
   !
   !This subroutine is not used in this program. It is written here for testing purposes
   !
   Subroutine Get_SubGroups_n(SpG, SubG, nsg, indexg, point)
      !---- Arguments ----!
      type(Spg_Type),                    intent( in) :: SpG
      type(Spg_Type),dimension(:),       intent(out) :: SubG
      integer,                           intent(out) :: nsg
      integer,                  optional,intent(in)  :: indexg
      logical, dimension(:,:),  optional,intent(out) :: point

      !--- Local variables ---!
      integer  :: i,L,j,k,d, nc, mp,ngen,nla,n,nop,idx,ng
      character (len=80), dimension(:),allocatable :: gen,gen_min
      character (len=80), dimension(30)            :: gen_lat
      character (len=256),dimension(:),allocatable :: list_gen
      character (len=80)                           :: gen_cent, gen_aux

      type(Symm_Oper_Type)                         :: Op_cent, Op_aux
      type(Symm_Oper_Type), dimension(30)          :: Op_lat

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

      allocate(gen(SpG%multip))
      d=SpG%d
      ng=0; nc=0
      nop=SpG%numops !number of symmetry operators excluding lattice centrings & centre of symmetry
      if (SpG%centred /= 1) then
         nop=nop*2        ! number of symmetry operators excluding lattice centrings
         nc=SpG%Numops+1  ! Position of the centre of symmetry if it exists
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

      !> Take as generators the list of the operators of the input group up to SpG%numops
      allocate(gen_min(SpG%numops))
      do i=1,SpG%numops
        gen_min(i) = SpG%Symb_Op(i)
      end do
      ngen=SpG%numops

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
      !Construct the lists of  possible generators to be tested
      mp=2*max(1,ngen-2)*max(1,ngen-1)*ngen*max(1,ngen-1)*ngen*ngen
      if (allocated(list_gen)) deallocate(list_gen)
      allocate(list_gen(mp))

      ! Start with the simplest lists: 1 generator
      L=0
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
               L=L+1
               list_gen(L)=trim(gen(i))//";"//trim(gen(j))
            end do
         end do
      end if

      ! Continue with the lists : 3 generators (it is enough for general crystallographic groups)
      if(ngen >= 3) then
         do i=1,ngen-2
            do j=i+1,ngen-1
               do k=j+1,ngen
                  L=L+1
                  list_gen(L)=trim(gen(i))//";"//trim(gen(j))//";"//trim(gen(k))
               end do
            end do
         end do
      end if

      mp=L

      !Now add the lattice translations
      if (SpG%num_lat > 0)  then
         do j=1,SpG%num_lat
            do i=1,mp
               L=L+1
               list_gen(L)=trim(list_gen(i))//";"//trim(gen_lat(j))
            end do
         end do
      end if
      nsg=L !Number of lists with generators

      !> Generate the subgroups corresponding to the above lists
      n=1
      call Group_Constructor(List_gen(1),SubG(1))
      do_L:do L=2,nsg
         do i=1,n-1
           if(Is_in_Group(list_gen(L),SubG(i))) cycle do_L !Prevent generation of repeated groups
         end do
         n=n+1 !new group
         !write(*,"(a,i3,a)") " => Generating subgroup #",n," with generators: "//trim(List_gen(L))
         call Group_Constructor(List_gen(L),SubG(n))
         if (present(indexg)) then
            idx=SpG%multip/SubG(n)%multip
            if (idx /= indexg) then
               n=max(0,n-1)
               cycle do_L
            end if
         end if
         do i=1,n-1  !Check for repetitions in any case
            if (SubG(n) == SubG(i)) then
               n=max(0,n-1)
               exit
            end if
         end do
      end do do_L
      nsg=n

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

   End Subroutine Get_SubGroups_n

   Function Is_in_Group(list_gen,Spg) result(it_is)
     character(len=*),intent(in) :: list_gen
     type(Spg_Type),  intent(in) :: SpG
     logical                     :: it_is
     !
     integer :: i,ngen,d
     character(len=40), dimension(:),allocatable :: gen
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

End Program test230_groups