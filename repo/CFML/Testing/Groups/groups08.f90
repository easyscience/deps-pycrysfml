 Module Get_gSpG
   ! In this module there are some subroutines for testing
   ! They will be incorporate to CrysFML08 as soon as they work properly
   use CFML_GlobalDeps
   use CFML_gSpaceGroups
   use CFML_Rational
   use CFML_Maths
   implicit none
   private
   Public :: Set_gSpG, SubGroups
   integer,          dimension(0:2), parameter :: CENT=[2,1,2]
   character(len=1), dimension(10),  parameter :: XYZ=["x","y","z","t","u","v","w","p","q","r"]
   logical, public :: to_std=.false.
   contains

     Subroutine Set_gSpG(Str_tmp,SpG,Mode,Setting)
       character(len=*),             intent(in) :: Str_tmp
       Class(SpG_Type),allocatable,  intent(out):: SpG
       Character(len=*),             intent(in) :: Mode
       Character(len=*),optional,    intent(in) :: Setting
       ! --- Local Variables ---!
       Character(len=:), allocatable :: Sett
       Select Case (trim(Mode))
         Case("SHUBN","SUPER")
           if(present(setting)) then
             if(to_std) then
               call Set_SpaceGroup(Str_tmp,Mode,SpG,Setting=Setting,trn_to=.true.)
             else
               call Set_SpaceGroup(Str_tmp,Mode,SpG,Setting=Setting)
             end if
           else
             call Set_SpaceGroup(Str_tmp,Mode,SpG)
           end if
         Case default
           call Set_SpaceGroup(Str_tmp,SpG)
           if(present(setting)) then
              if(to_std) then
                sett=inverse_setting(setting,SpG%D)
              else
                sett=setting
              end if
             call Change_Set_SpaceG(sett,SpG)
             if(err_CFML%Ierr /= 0) then
               write(*,"(a)") " => "//trim(err_CFML%Msg)
               write(*,"(a)") " => Provided setting not applied!"
             end if
           end if
       End Select
     End Subroutine Set_gSpG

   Subroutine Change_Set_SpaceG(setting, SpaceG,xyz_type)
      !---- Arguments ----!
      character(len=*),           intent(in )    :: setting
      class(spg_type),            intent(in out) :: SpaceG
      character(len=*), optional, intent(in )    :: xyz_type

      !---- Local variables ----!
      Type(rational), dimension(SpaceG%D,SpaceG%D)     :: Pmat,invPmat
      Type(rational), dimension(SpaceG%D-1,SpaceG%D-1) :: rot,roti,identd
      Type(rational), dimension(SpaceG%D-1)            :: v
      Type(rational), dimension(:,:),allocatable       :: newLat
      Type(rational) :: det
      integer :: i,j,k,l,n,m,Npos,d,Dd !,im
      character(len=6) :: Strcode
      character(len=80), dimension(:),allocatable :: gen_lat
      type(spg_type)   :: SpG, P1_t !Auxiliary space groups
      logical          :: centring

      Dd=SpaceG%D
      d=Dd-1
      call Get_Mat_From_Symb_Int(setting, Pmat)
      if(err_CFML%Ierr /= 0) return
      rot=Pmat(1:d,1:d)
      det=Rational_Determ(rot)
      if(det < 0_LI ) then
         err_CFML%Ierr=1
         err_CFML%Msg ="The determinant of the transformation matrix should be positive"
         return
      end if
      L=(SpaceG%Num_Lat+d)*nint(det)
      allocate(newLat(SpaceG%D-1,L))
      newLat=0//1
      invPmat=Rational_Inverse_Matrix(Pmat)
      SpaceG%setting=trim(setting)
      SpaceG%matfrom=trim(setting)
      SpaceG%mat2std=Get_Symb_From_Mat(invPmat,StrCode="abc" )
      SpaceG%mat2std_shu=SpaceG%mat2std
      centring=.false.
      Strcode="xyz"
      if(present(xyz_type)) Strcode=trim(xyz_type)
      roti=Rational_Inverse_Matrix(rot)
      call rational_identity_matrix(identd)

      L=0
      if (SpaceG%Num_Lat > 0) then  !Original lattice is centered
         do_i:do i=1,SpaceG%Num_Lat      !Transform the centring vectors to the new lattice
            v=Rational_Modulo_Lat(matmul(roti,SpaceG%Lat_tr(:,i)))
            if (sum(v) == 0_LI) cycle
            do j=1,L
               if(Rational_Equal(v,newlat(:,j))) cycle do_i
            end do
            L=L+1
            newlat(:,L)=v
            !write(*,"(i8,a,10a)") L," -> ",(trim(rational_string(v(j)))//"  ",j=1,d)
         end do do_i
      end if

      do_i2:do i=1,d  !Test also the basis vectors of the original setting
        v=Rational_Modulo_Lat(roti(1:d,i))
        if (sum(v) == 0_LI) cycle
            do j=1,L
               if(Rational_Equal(v,newlat(:,j))) cycle do_i2
            end do
        L=L+1
        newlat(:,L)=v
        !write(*,"(i8,a,10a)") L," -> ",(trim(rational_string(v(j)))//"  ",j=1,d)
      end do do_i2

      if(L > 0) then !Generate the group P1 with the primitive set of lattice centring in order to get its total number
        allocate(gen_lat(L))
        do i=1,L
          gen_lat(i)=" "
          do j=1,d
            gen_lat(i)=trim(gen_lat(i))//XYZ(j)//"+"//trim(rational_string(newlat(j,i)))//","
          end do
          n=len_trim(gen_lat(i))
          gen_lat(i)(n+1:n+1)="1"
        end do
        call group_constructor(gen_lat,P1_t)
        L=P1_t%Num_Lat
        do i=1,L
          newlat(:,i)=P1_t%Lat_tr(:,i)
        end do
      end if

      SpG%Num_Lat=L
      if(SpG%Num_Lat > 0) centring=.true.

      Npos=SpaceG%Numops*cent(SpaceG%centred)
      SpG%Multip=Npos*(1+SpG%Num_Lat)
      Spg%NumOps=SpaceG%NumOps

      call Allocate_SpaceGroup(Dd, SpG%Multip, SpG)

      if(SpG%Num_Lat > 0) then
        if(allocated(SpG%Lat_tr)) deallocate(SpG%Lat_tr)
        allocate(SpG%Lat_tr(d,SpG%Num_Lat))
        SpG%Lat_tr(:,:)=newlat(:,1:L)
      end if

      do i=1,Npos          !Transform the first Npos operators
        SpG%Op(i)%Mat=matmul(matmul(invPmat,SpaceG%Op(i)%Mat),Pmat)
        SpG%Op(i)%Mat(1:d,Dd)=Rational_Modulo_Lat(SpG%Op(i)%Mat(1:d,Dd))
        SpG%Op(i)%time_inv=SpaceG%Op(i)%time_inv
        SpG%Op(i)%dt=SpaceG%Op(i)%dt
        SpG%Symb_Op(i)=Get_Symb_from_Mat(SpG%Op(i)%Mat, Strcode,SpG%Op(i)%time_inv)
      end do

      if(centring) then
        n=Npos
        do L=1,SpG%Num_Lat
           do k=1,Npos
             SpG%Op(k+n)%Mat=SpG%Op(k)%Mat             !Same matrix as the first NumOps operators
             SpG%Op(k+n)%time_inv=SpG%Op(k)%time_inv   !Same time inversion
             SpG%Op(k+n)%Mat(1:d,Dd)=Rational_Modulo_Lat(SpG%Op(k)%Mat(1:d,Dd)+SpG%Lat_tr(:,L)) !Different translation
             SpG%Op(k+n)%dt=SpG%Op(k)%dt
             SpG%Symb_Op(k+n)=Get_Symb_from_Mat(SpG%Op(k+n)%Mat, Strcode,SpG%Op(k+n)%time_inv)
           end do
           n=n+Npos
        end do
      end if

      !Reallocate the array components of the initial space group
      call Allocate_SpaceGroup(Dd, SpG%Multip, SpaceG)
      SpaceG%Multip=SpG%Multip
      SpaceG%Op=SpG%Op             !Copy the array components keeping the names,labels, etc.
      SpaceG%Symb_Op=SpG%Symb_Op
      SpaceG%Num_Lat=Spg%Num_lat
      SpaceG%Num_aLat=SpaceG%Num_aLat*(1+SpG%Num_Lat)

      if(SpaceG%Num_Lat > 0) then
         if(allocated(SpaceG%Lat_tr)) deallocate(SpaceG%Lat_tr)
         allocate(SpaceG%Lat_tr(d,SpaceG%Num_Lat))     ! Centring vectors
         SpaceG%Lat_tr=SpG%Lat_tr                      ! Direct assignment of vectors
      end if

      if(SpaceG%Num_aLat > 0) then
         if(allocated(SpaceG%aLat_tr)) deallocate(SpaceG%aLat_tr)
         allocate(SpaceG%aLat_tr(d,SpaceG%Num_aLat))     ! Anti-translation vectors
         m=0
         if(SpaceG%Mag_Type /= 2) then
           do k=1,SpaceG%multip
             if(rational_equal(SpaceG%Op(k)%Mat(1:d,1:d),identd) .and. SpaceG%Op(k)%time_inv == -1) then
               m=m+1
               SpaceG%aLat_tr(:,m) = SpaceG%Op(k)%Mat(1:d,Dd)
             end if
           end do
         end if
         SpaceG%Num_aLat=m
      end if
      !Transform the symmetry operators of the list of generators !TO DO

   End Subroutine Change_Set_SpaceG

   Subroutine SubGroups(SpG, SubG, nsg, indexg, point, printd)
      !---- Arguments ----!
      type(Spg_Type),                    intent( in) :: SpG
      type(Spg_Type),dimension(:),       intent(out) :: SubG
      integer,                           intent(out) :: nsg
      integer,                  optional,intent(in)  :: indexg
      logical, dimension(:,:),  optional,intent(out) :: point
      logical,                  optional,intent(in)  :: printd

      !--- Local variables ---!
      integer  :: i,L,j,k,d, nc, mp,ngen,nla,n,nop,idx,ng,nalloc,navoid !,i1,i2
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
      !write(*,"(a,i7)") " Number of allocated lists: ",nalloc
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
      !i1=mp+1
      !i2=i1+mp
      !if (SpG%num_lat > 1)  then
      !   do_jla: do j=1,SpG%num_lat
      !      do i=i1,i2   !1,mp
      !         if(index(list_gen(i),trim(gen_lat(j))) /= 0) cycle
      !         L=L+1
      !         if(L > nalloc) then
      !           L=nalloc
      !           write(*,"(a)") "Warning: Maximum number of generator lists reached at second Lattice-generators loop"
      !           exit do_jla
      !         end if
      !         list_gen(L)=trim(list_gen(i))//";"//trim(gen_lat(j))
      !      end do
      !   end do do_jla
      !end if

      nsg=L !Number of lists with generators

      if(present(printd)) then
        write(unit=*,fmt="(a,i6)") " Number of generator lists: ",nsg
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
               if(present(printd))  write(*,"(a,i4)") " Repeated group, new 'n' ",n
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

   End Subroutine SubGroups

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
 End Module Get_gSpG

!!----
!!----
!!----
!!----
 Program Test_Groups
    !---- Use Modules ----!
    use CFML_Globaldeps, only: cp, err_cfml, set_CFML_debug, CFML_debug
    use CFML_Symmetry_Tables
    use CFML_gSpaceGroups
    use Get_gSpG
    implicit none
    character(len=2048)                 :: generatorList
    character(len=180)                  :: setting
    character(len=25)                   :: forma="(i5,tr2,a,   i4,a,i8)"
    character(len=25)                   :: mode
    character(len=5)                    :: aux
    class(Spg_Type), allocatable        :: Grp
    type(Spg_Type), dimension(512)      :: sGrp
    !integer, dimension(:,:),allocatable :: table
    !integer, dimension(:,:),allocatable :: G
    !integer, dimension(:),  allocatable :: ord
    integer, dimension(:), allocatable  :: cosets
    integer :: i, j, L, nsg, indexg, num_group, ier,minu, nc !lun !, ind
    real(kind=cp) :: start, fin,tini,tfin, secnd
    logical :: set_given, sup_given, datb_given, full, shub_given, set_inv=.true.

    !> Init
    call CPU_TIME(tini)
    call Set_Conditions_NumOp_EPS(4096) !Maximum admissible multiplicity
    !call set_CFML_debug(.true.)
    !open(newunit=lun,file="groups.out",status="replace",action="write")
    do
       set_given=.false.; sup_given=.false.; datb_given=.false.; shub_given=.false.
       write(*,'(/,a,/)') " => Examples of input to the next question:"
       write(*,'(a)') "1236   :: a,b,c;0,0,0      shub        <-- Shubnikov group type 1236 in standard setting"
       write(*,'(a)') "123    :: a,-c,b;1/2,0,0               <-- Space group type 123 in non-standard setting"
       write(*,'(a)') "Pn'ma                                  <-- Shubnikov group type Pn'ma in standard setting"
       write(*,'(a)') "UNI Pnna.1'_a[Pncm]                    <-- Shubnikov group given in unified notation"
       write(*,'(a)') "UNI Pnna.1'_a[Pncm] :: -c,b,a;0,0,0    <-- Shubnikov group given in unified notation in non-stardard setting"
       write(*,'(a)') "Pn'ma  :: -c,b,a;0,0,0     shub        <-- Shubnikov group type Pn'ma in the non-standard setting Pcmn'"
       write(*,'(a)') "Pn'ma  :: -c/2,b,a/2;0,0,0 shub trn_to <-- Shubnikov group type Pn'ma in a non-standard giving the inverse tranformation"
       write(*,'(a)') "B 2 C B                                <-- space #41 of standard symbol Aba2"
       write(*,'(a)') "13232  :: sup                          <-- SuperSpace group 13232 in standard setting"
       write(*,'(a)') "221    :: a2,-a1,a3,a4;0,0,1/2,0  sup  <-- SuperSpace group #221 in non-standard setting"
       write(*,'(a)') "Pnma(0,0,g)0s0  :: sup                 <-- SuperSpace group of standard symbol"
       write(*,'(a)') "x,-y,z,t,1;x,y,z,t+1/2,-1              <-- The generators of a Magnetic SuperSpace group"
       write(*,'(a)') "x1,-x2,x3,x4,1;x1,x2,x3,x4+1/2,-1      <-- The generators of a Magnetic SuperSpace group"
       write(*,'(/,a)',advance='no') " => Introduce generators, HM or Hall symbol, or the number of a standard group as indicated above: "
       read(*,'(a)') generatorList
       if (len_trim(generatorList) == 0) exit

       !> Determine if it is a number
       aux=" "; to_std=.false.
       j=index(generatorList,"trn_to")
       if(j /= 0) then
          generatorList=generatorList(1:j-1)
          to_std=.true.
       end if
       read(unit=generatorList,fmt=*,iostat=ier) num_group
       if (ier == 0) then
          !Now determine if setting is appearing in which case it uses data bases
          i=index(generatorList,"::")

          if(i /= 0) then
             j=index(generatorList,"sup")  !Superspace
             if(j /= 0) then
               sup_given=.true.
               if(index(generatorList,"a1") /= 0 .or. index(generatorList,"d") /= 0) then
                 setting= adjustl(generatorlist(i+2:j-1))
                 set_given=.true.
               end if
             else
               j=index(generatorList,"shub")  !Shubnikov group
               if(j /= 0) then
                 shub_given=.true.
                 setting= adjustl(generatorlist(i+2:j-1))
                 if(len_trim(setting) /= 0) set_given=.true.
               else
                 setting= adjustl(generatorlist(i+2:))
                 if(len_trim(setting) /= 0) set_given=.true.

               end if
             end if
             generatorlist=generatorlist(1:i-1)
          end if
       else
          i=index(generatorList,"::")
          if(i /= 0) then
             j=index(generatorList,"sup")  !Superspace
             if(j /= 0) then
               sup_given=.true.
               if(index(generatorList,"a1") /= 0  .or. index(generatorList,"d") /= 0) then
                 setting= adjustl(generatorlist(i+2:j-1))
                 set_given=.true.
               end if
             else
               j=index(generatorList,"shub")  !Shubnikov group
               if(j /= 0) then
                 shub_given=.true.
                 setting= adjustl(generatorlist(i+2:j-1))
                 if(len_trim(setting) /= 0) set_given=.true.
               else
                 setting= adjustl(generatorlist(i+2:))
                 if(len_trim(setting) /= 0) set_given=.true.
               end if
             end if
             generatorlist=generatorlist(1:i-1)
          end if
          i=index(generatorList,"'")
          j=index(generatorList,"_")
          if(i /= 0 .or. j /= 0) datb_given=.true.
       end if

       call CPU_TIME(start)
       if(sup_given) then
          if(set_given) then
             call Set_gSpG(generatorList,Grp,"SUPER",Setting)
          else
             call Set_gSpG(generatorList,Grp,"SUPER")
          end if
       else
         if(shub_given) then
            if(set_given ) then
               call Set_gSpG(generatorList,Grp,"SHUBN",Setting)
            else if (datb_given) then
               call Set_gSpG(generatorList,Grp,"SHUBN")
            end if
         else
            if(set_given ) then
               call Set_gSpG(generatorList,Grp,"GEN",Setting)
            else
              if(index(generatorList,"x") /= 0) then
                call Set_gSpG(generatorList,Grp,"GEN")
              else
                call Set_gSpG(generatorList,Grp," ")
              end if
            end if
         end if
       end if
       if (Err_CFML%Ierr /= 0) then
          write(*,'(/,4x,a)') trim(Err_CFML%Msg)
          cycle
       else
          if(set_inv) Grp%inv=get_Inv_OP(Grp%Op)
          call Write_SpaceGroup_Info(Grp)
       end if

       full=.false.
       do
          write(*,'(/,a)',advance='no') "Introduce the index of subgroups (if = 0, no restriction, if < 0 no calculation): "
          read(*,"(a)") mode
          if(index(mode,"full") /= 0) full=.true.
          read(mode,*,iostat=ier) indexg
          if(ier == 0) exit
       end do
       Select Case(indexg)
         Case(:-1)
           cycle
         Case(0)
           if(full) then
             call Get_SubGroups_full(Grp,sGrp,nsg,printd=.true.)
           else
             call Get_SubGroups_gen(Grp,sGrp,nsg,printd=.true.)
           end if
           !call SubGroups(Grp,sGrp,nsg,printd=.true.)
         Case Default
           call Get_SubGroups_full(Grp,sGrp,nsg,indexg,printd=.true.)
           !call SubGroups(Grp,sGrp,nsg,indexg,printd=.true.)
       End Select
       if (Err_CFML%Ierr /= 0) then
          write(*,'(/,4x,a)') trim(Err_CFML%Msg)
          cycle
       end if

       if (nsg > 0) Then
          do L=1,nsg
             !write(*,"(/2(a,i3))") "  SUB-GROUP NUMBER #",L, " of index: ",Grp%multip/sGrp(L)%multip
             !write(lun,"(/2(a,i3))") "  SUB-GROUP NUMBER #",L, " of index: ",Grp%multip/sGrp(L)%multip
             call Identify_Group(sGrp(L)) !.false.
             if (Err_CFML%Ierr /= 0) then
                write(*,'(/,4x,"=> Error in the identification of the group: ",a)') trim(Err_CFML%Msg)
             end if
             call Write_SpaceGroup_Info(sGrp(L))
             !call Write_SpaceGroup_Info(sGrp(L),lun)
             !> Write the coset decomposition of Grp with respect to the current subgroup
             call Get_Cosets(Grp,sGrp(L),cosets)
             nc=size(cosets)
             if (nc > 0) then
                !     12345678901234
                forma="(/,a,i3,a,    a,/)"
                write(forma(11:14),"(i4)") nc
                write(*,forma) "  Coset decomposition of  G: "//trim(Grp%BNS_symb)//&
                                 "(",nc+1,") =  H("//trim(sGrp(L)%BNS_symb)//")  + ",("{"//&
                                 trim(Grp%Symb_Op(cosets(j)))//"} H + ",j=1,nc-1), &
                                 "{"//trim(Grp%Symb_Op(cosets(nc)))//"} H"
             end if
          end do
       end if

       call CPU_TIME(fin)
       write(*,"(a,f12.3,a)") "CPU_TIME for this calculation: ",fin-start," seconds"
       !write(lun,"(a,f12.3,a)") "CPU_TIME for this calculation: ",fin-start," seconds"
    end do
    call CPU_TIME(tfin)
    secnd=tfin-tini
    minu=int(secnd/60.0)
    secnd=secnd-real(minu*60)
    write(*,"(/,a,i6,a,f12.3,a)") "Total CPU_TIME for this calculation: ",minu," minutes and ",secnd," seconds"

End Program Test_Groups