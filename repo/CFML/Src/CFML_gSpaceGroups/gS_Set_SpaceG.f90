!!----
!!----
!!----
SubModule (CFML_gSpaceGroups) gS_Set_SpaceG
   implicit none
   Contains


   !!----Module Function Inverse_Setting(setting,D) Result(inv_sett)
   !!----   character(len=*), intent(in) :: setting
   !!----   integer,          intent(in) :: D
   !!----   Character(len=:), allocatable:: inv_sett
   !!----
   !!----
   !!----   This routine invert a provided setting in the form
   !!----     m1a+m2b+m3c...,m4a+m5b+m6c...,m7a+m8b+m9c...,...;o1,o2,o3, ...
   !!----   into a similar form representing the inverse operation.
   !!----   The integer D represents the dimension of the matrices
   !!----
   Module Function Inverse_Setting(setting,D) Result(inv_sett)
      Character(len=*), intent(in) :: setting
      integer,          intent(in) :: D
      Character(len=:), allocatable:: inv_sett
      !Local variables
      Type(rational), dimension(D,D)     :: Pmat,invPmat

      inv_sett = setting
      call Get_Mat_From_Symb_Int(setting, Pmat)
      if(err_CFML%Ierr /= 0) return
      invPmat=Rational_Inverse_Matrix(Pmat)
      inv_sett=Get_Symb_From_Mat(invPmat,StrCode="abc" )
   End Function Inverse_Setting

   !!---- Module Function Get_MagPG_from_BNS(BNS_Symb,mag_type) Result(mag_pg)
   !!----   character(len=*), intent(in) :: BNS_Symb
   !!----   integer,          intent(in) :: mag_type
   !!----   character(len=:), allocatable:: mag_pg
   !!----
   !!----   Construct the symbol of the magnetic point group from the BNS notation
   !!----
   !!----
   Module Function Get_MagPG_from_BNS(BNS_Symb,mag_type) Result(mag_pg)
      character(len=*), intent(in) :: BNS_Symb
      integer,          intent(in) :: mag_type
      character(len=:), allocatable:: mag_pg
      !---- Local variables ----!
      integer :: i,j, m

      !Setting the magnetic point group symbol from the BNS label
      m=0
      mag_pg=bns_symb(2:)
      j=2
      if(mag_type == 4) then
        mag_pg=bns_symb(4:)
        j=4
      end if
      do i=j,len_trim(bns_symb)
        m=m+1
        if(  bns_symb(i:i) == "a" .or. bns_symb(i:i) == "b"  &
        .or. bns_symb(i:i) == "c" .or. bns_symb(i:i) == "d"  &
        .or. bns_symb(i:i) == "e" .or. bns_symb(i:i) == "g"  &
        .or. bns_symb(i:i) == "n") mag_pg(m:m)="m"
        if(bns_symb(i:i) == "_") mag_pg(m:m+1)=" "
      end do
      mag_pg=pack_string(mag_pg)
      if(mag_type == 4) mag_pg=trim(mag_pg)//"1'"

   End Function Get_MagPG_from_BNS

   Module Function Get_Multip_Pos(x,SpG) Result(mult)
      !---- Arguments ----!
      real(kind=cp), dimension(:), intent(in) :: x !Vector position in superspace
      class(SPG_Type),             intent(in) :: SpG
      integer                                 :: mult
      !----Local Variables ---!
      real(kind=cp), dimension(size(x)+1)          :: xd,xx
      real(kind=cp), dimension(size(x))            :: v
      real(kind=cp), dimension(4,4)                :: Omat
      real(kind=cp), dimension(size(x),SpG%Multip) :: u
      integer :: i, D, nt, dm


      D=size(x)
      dm=Spg%d
      xd=[x,1.0_cp]
      mult=1
      u(:,1)=x
      if(D == 3) Then  !This is for the average structure
        Omat(4,1:4) = [0.0_cp,0.0_cp,0.0_cp,1.0_cp]
        do_ext1: do i=2,Spg%multip
           Omat(1:3,1:3) = real(Spg%Op(i)%Mat(1:3,1:3))
           Omat(1:3,4)   = real(Spg%Op(i)%Mat(1:3,dm))
           xx=matmul(Omat,xd)
           xx=modulo_lat(xx)
           do nt=1,mult
              v(1:d)=u(:,nt)-xx(1:D)
              if(Spg%Num_lat > 0) then
                if (is_Lattice_vec(v,Spg%Lat_tr)) cycle do_ext1
              else
                if (Zbelong(v)) cycle do_ext1
              end if
           end do
           mult=mult+1
           u(:,mult)=xx(1:D)
        end do do_ext1
      else
        do_ext: do i=2,Spg%multip  !This is for superspace
           xx=matmul(real(Spg%Op(i)%Mat),xd)
           xx=modulo_lat(xx)
           do nt=1,mult
              v(1:d)=u(:,nt)-xx(1:D)
              if(Spg%Num_lat > 0) then
                if (is_Lattice_vec(v,Spg%Lat_tr)) cycle do_ext
              else
                if (Zbelong(v)) cycle do_ext
              end if
           end do
           mult=mult+1
           u(:,mult)=xx(1:D)
        end do do_ext
      end if
      mult=mult*(1+Spg%Num_lat) !The multiplicity was up to now for primitive cell, now it coincides with ITC

   End Function Get_Multip_Pos

  !!----
  !!---- Set_SpaceGroup_from_DBase
  !!----    Function version of subroutine set_spacegroup_dbase.
  !!----    Used for the python wrapper.
  !!----
  !!---- 25/09/2024
  !!
  Module Function Set_SpaceGroup_from_DBase(str,mode,xyz_type,setting,keepdb,parent,database_path,trn_to) result(spaceg)
    ! Set a Shubnikov or a superspacegroup from the database
    character(len=*),                   intent(in ) :: str            ! string specifying the Shubnikov or superspace group
    character(len=*),                   intent(in ) :: mode           ! type of group: 'shubn' | 'super'
    character(len=*), target, optional, intent(in ) :: xyz_type       ! coordinates notation: 'xyz' (default) | 'abc' | 'x1x2x3'
    character(len=*), target, optional, intent(in ) :: setting        ! setting transformation
    logical,          target, optional, intent(in ) :: keepdb         ! if not given, deallocate the database after the group generation
    character(len=*), target, optional, intent(in ) :: parent         ! if given, set the parent group symbol
    character(len=*), target, optional, intent(in ) :: database_path  ! full path of the database file
    logical,          target, optional, intent(in ) :: trn_to         ! if given, transformation matrices between standard and new setting (if given) are computed
    class(spg_type),  allocatable                   :: spaceg         ! Shubnikov or superspace group

    ! Local variables
    character(len=:), pointer :: p_xyz_type => null()
    character(len=:), pointer :: p_setting => null()
    logical,          pointer :: p_keepdb => null()
    character(len=:), pointer :: p_parent => null()
    character(len=:), pointer :: p_database_path => null()
    logical,          pointer :: p_trn_to => null()

    if (present(xyz_type)) p_xyz_type => xyz_type
    if (present(setting)) p_setting => setting
    if (present(keepdb)) p_keepdb => keepdb
    if (present(parent)) p_parent => parent
    if (present(database_path)) p_database_path => database_path
    if (present(trn_to)) p_trn_to => trn_to

    call set_spacegroup(str,mode,spaceg,p_xyz_type,p_setting,p_keepdb,p_parent,p_database_path,p_trn_to)

  End Function Set_SpaceGroup_from_DBase

  !!----
  !!---- Set_SpaceGroup_from_Generators
  !!----    Function version (simplified) of subroutine set_spacegroup_gen.
  !!----    Used for the python wrapper.
  !!----
  !!---- 26/09/2024
  !!
  Module Function Set_SpaceGroup_from_Generators(Str, set_inv, database_path) result(spaceg)
    ! Set a Shubnikov or a superspacegroup from a list of generators
    character(len=*),                   intent(in ) :: Str     ! generators list
    logical,          target, optional, intent(in ) :: set_inv ! if given, the pointer to the inverse of each operator will be set
    character(len=*), target, optional, intent(in ) :: database_path  ! full path of the database file
    class(spg_type),  allocatable                   :: spaceg  ! Shubnikov or superspace group

    ! Local variables
    logical, pointer :: p_set_inv => null()
    character(len=:), pointer :: p_database_path => null()

    if (present(set_inv)) p_set_inv => set_inv
    if (present(database_path)) p_database_path => database_path

    call set_spacegroup(str,spaceg,set_inv=p_set_inv,database_path=p_database_path)

  End Function Set_SpaceGroup_from_Generators

   !!----
   !!---- Module Subroutine Change_Setting_Generators(setting,ngen,gen,xyz_type)
   !!----   character(len=*),               intent(in )    :: setting   !String with change of basis
   !!----   integer,                        intent(in )    :: ngen      !Number of generators
   !!----   character(len=*), dimension(:), intent(in out) :: gen       !String array with generators in Jones faithful form
   !!----   character(len=*), optional,     intent(in )    :: xyz_type
   !!----
   !!----   This subroutine applies the basis transformation encoded in setting to the
   !!----   generators provided. No generation of group is performed
   !!----
   !!----
   !!----
   Module Subroutine Change_Setting_Generators(setting,ngen,gen,xyz_type)
      !---- Arguments ----!
      character(len=*),               intent(in )    :: setting
      integer,                        intent(in )    :: ngen
      character(len=*), dimension(:), intent(in out) :: gen
      character(len=*), optional,     intent(in )    :: xyz_type

      !---- Local variables ----!
      Type(rational), dimension(:,:), allocatable    :: Pmat,invPmat !(Dd,Dd)
      Type(rational)                                 :: det
      integer                                        :: i,d,Dd
      character(len=6)                               :: Strcode
      type(Symm_Oper_Type), dimension(ngen)          :: Op

      call clear_error()
      Dd=Get_Dimension_SymmOp(gen(1))
      d=Dd-1
      allocate(Pmat(Dd,Dd),invPmat(Dd,Dd))
      call Get_Mat_From_Symb_Int(setting, Pmat)
      if(err_CFML%Ierr /= 0) return
      det=Rational_Determ(Pmat(1:d,1:d))
      if(det < 0_LI ) then
         err_CFML%Ierr=1
         err_CFML%Msg ="The determinant of the transformation matrix should be positive"
         return
      end if
      invPmat=Rational_Inverse_Matrix(Pmat)
      Strcode="xyz"
      if(present(xyz_type)) Strcode=trim(xyz_type)

      do i=1,ngen          !Transform the generators
        Op(i)=Get_Op_from_Symb(gen(i))
        Op(i)%Mat=matmul(matmul(invPmat,Op(i)%Mat),Pmat)
        Op(i)%Mat(1:d,Dd)=Rational_Modulo_Lat(Op(i)%Mat(1:d,Dd))
        gen(i)=Get_Symb_from_Mat(Op(i)%Mat, Strcode,Op(i)%time_inv)
      end do

   End Subroutine Change_Setting_Generators

   Module Subroutine Change_Setting_SpaceG(setting, SpaceG,xyz_type)
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
      integer :: i,j,k,l,n,m,Npos,d,Dd,ng
      character(len=6) :: Strcode
      real             :: latn
      character(len=80), dimension(:),allocatable :: gen_lat, gen_new
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
      !L=(SpaceG%Num_Lat+1+d)*nint(det)
      latn=SpaceG%Num_Lat+1+d
      L=nint(latn*real(det))
      allocate(newLat(SpaceG%D-1,L))
      newLat=0//1
      invPmat=Rational_Inverse_Matrix(Pmat)
      SpaceG%setting=trim(setting)
      SpaceG%mat2std=Get_Symb_From_Mat(invPmat,StrCode="abc" )
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

      allocate(gen_new(SpaceG%Numops+1+SpG%Num_Lat))

      Npos=SpaceG%Numops*cent(SpaceG%centred)
      SpG%Multip=Npos*(1+SpG%Num_Lat)
      Spg%NumOps=SpaceG%NumOps

      call Allocate_SpaceGroup(Dd, SpG%Multip, SpG)

      if(SpG%Num_Lat > 0) then
        if(allocated(SpG%Lat_tr)) deallocate(SpG%Lat_tr)
        allocate(SpG%Lat_tr(d,SpG%Num_Lat))
        SpG%Lat_tr(:,:)=newlat(:,1:L)
      end if

      k=0
      do i=1,Npos          !Transform the first Npos operators
        SpG%Op(i)%Mat=matmul(matmul(invPmat,SpaceG%Op(i)%Mat),Pmat)
        SpG%Op(i)%Mat(1:d,Dd)=Rational_Modulo_Lat(SpG%Op(i)%Mat(1:d,Dd))
        SpG%Op(i)%time_inv=SpaceG%Op(i)%time_inv
        SpG%Op(i)%dt=SpaceG%Op(i)%dt
        SpG%Symb_Op(i)=Get_Symb_from_Mat(SpG%Op(i)%Mat, Strcode,SpG%Op(i)%time_inv)
        if(i > 1 .and. i <= SpaceG%Numops) then
          k=k+1
          gen_new(k)=SpG%Symb_Op(i)
        end if
      end do
      if(SpaceG%centred /= 1) then
        k=k+1
        gen_new(k)=SpG%Symb_Op(SpaceG%Numops+1)
      end if
      ng=k


      if(centring) then
        n=Npos
        do L=1,SpG%Num_Lat
           do k=1,Npos
             SpG%Op(k+n)%Mat=SpG%Op(k)%Mat             !Same matrix as the first NumOps operators
             SpG%Op(k+n)%time_inv=SpG%Op(k)%time_inv   !Same time inversion
             SpG%Op(k+n)%Mat(1:d,Dd)=Rational_Modulo_Lat(SpG%Op(k)%Mat(1:d,Dd)+SpG%Lat_tr(:,L)) !Different translation
             SpG%Op(k+n)%dt=SpG%Op(k)%dt
             SpG%Symb_Op(k+n)=Get_Symb_from_Mat(SpG%Op(k+n)%Mat, Strcode,SpG%Op(k+n)%time_inv)
             if(k == 1) then
                ng=ng+1
                gen_new(ng) = SpG%Symb_Op(k+n)
             end if
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

      SpaceG%centred = 1
      do i=1,SpaceG%Multip !Looking for -1
        if(rational_equal(SpaceG%Op(i)%Mat(1:d,1:d),-identd) .and. SpaceG%Op(i)%time_inv ==  1) then
          SpaceG%Centre_coord=rational(1_LI,2_LI) * SpaceG%Op(i)%Mat(1:d,Dd)
          if(sum(abs(SpaceG%Centre_coord)) > 0_LI) then
            SpaceG%centred = 0
            SpaceG%Centre="                                                                                                                           "
            write(unit=SpaceG%Centre,fmt="(a,10a)") "Centrosymmetric with centre at: [ ",(trim(Rational_String(SpaceG%Centre_coord(j)))//" ",j=1,d),"]"
          else
            SpaceG%centred = 2
            SpaceG%Centre_coord=0_LI//1_LI
            SpaceG%Centre="Centrosymmetric with centre at origin"
          end if
          exit
        end if
      end do

      SpaceG%anticentred=1
      do i=1,Npos !Looking for anticentring
        if(rational_equal(SpaceG%Op(i)%Mat(1:d,1:d),-identd) .and. SpaceG%Op(i)%time_inv == -1) then
          SpaceG%AntiCentre_coord=rational(1_LI,2_LI) * SpaceG%Op(i)%Mat(1:d,Dd)
          if(sum(abs(SpaceG%AntiCentre_coord)) > 0_LI) then
            SpaceG%anticentred = 0
            SpaceG%Centre="                                                                                                   "
            write(unit=SpaceG%Centre,fmt="(a,10a)") "Non-centrosymmetric, Anti-centric with -1' @ : [ ",(trim(Rational_String(SpaceG%AntiCentre_coord(j)))//" ",j=1,d),"]"
          else
            SpaceG%anticentred = 2
            SpaceG%AntiCentre_coord=0_LI//1_LI
            SpaceG%Centre="Non-centrosymmetric, Anti-centric with -1' @ origin"
          end if
          exit
        end if
      end do


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
      !Transform the symmetry operators of the list of generators
      !The generators have been constructed above
      SpaceG%generators_list= " "
      do i=1,ng
        SpaceG%generators_list=trim(SpaceG%generators_list)//trim(gen_new(i))//";"
      end do
      i=index(SpaceG%generators_list,";",back=.true.)
      if( i /= 0) then
        SpaceG%generators_list=SpaceG%generators_list(1:i-1)
      end if
   End Subroutine Change_Setting_SpaceG

   Subroutine transf_E_groups(str)
     character(len=*), intent(in out) :: str
     ! --- Local variables ---!
     character(len=len(str)) :: e_group
     integer :: p2,pm,pm2,pe

     e_group = pack_string(u_case(str))
     p2 = index(e_group,"2")
     pm2= index(e_group,"M",back=.true.)
     pm = index(e_group,"M")
     pe = index(e_group,"E")
     if(pe == 0) return
     ! Space groups 39 and 41
     if(p2 /= 0) then
       if(pm /= 0) then ! Space group 39

          Select Case(pm)

           Case(2)

             if(e_group(1:1) == "B") then  ! e-> a
               str="B M A 2"  ! Bme2
             else
               str="C M 2 A"  ! Cm2e
             end if

           Case(3)

             if(e_group(1:1) == "A") then  ! e-> b
               str="A B M 2"  !Aem2
             else
               str="C 2 M B"  !C2me
             end if

           Case(4)

            if(e_group(1:1) == "A") then  ! e-> c
              str="A C 2 M"  !Ae2m
            else
              str="B 2 C M"  !B2em
            end if

          End Select

       else  ! Space group 41

          Select Case(p2)

            Case(4)

              if(e_group(1:1) == "A") then  ! e-> b,a
                str="A B A 2"  !Aea2
              else
                str="B B A 2"  !Bbe2
              end if

            Case(3)

              if(e_group(1:1) == "A") then  ! e-> c,a
                str="A C 2 A"  !Ae2a
              else
                str="C C 2 A"  !Cc2e
              end if

            Case(2)

              if(e_group(1:1) == "B") then  ! e-> c,b
                str="B 2 C B"  !B2eb
              else
                str="C 2 C B"  !C2ce
              end if

          End Select

       end if

     else ! Centrosymmetric groups

       if(pm /= 0) then

         if(pm2 == pm) then !Group 64

            Select Case(pm)

              Case(2) ! Cmce Bmeb

                if(e_group(1:1) == "C") then
                  str="C M C A"  !Cmce
                else
                  str="B M A B"  !Bmeb
                end if

              Case(3)  !Ccme Aema

                if(e_group(1:1) == "C") then
                  if(index(str,":") /= 0) then
                    str="C C M B"  !Cmce
                  else
                    str="A C M B:1"  !Cmce:1
                  end if
                else
                  str="A B M A"  !Aema
                end if

              Case(4) !Aeam Bbem

                if(e_group(1:1) == "A") then
                  str="A C A M"  !Aeam
                else
                  str="B B C M"  !Bbem
                end if

            End Select
         else  ! Group 67

            Select Case (pe)
              case(2) !Aemm Ambiguous notation because the two groups Abmm and Acmm are not distinguished
                str="A B M M"  !another option is str="A C M M"
              case(3) !Bmem
                str="B M C M"  !another option is str="B M A M"
              case(4)
                str="C M M A"  !another option is str="C M M B"
            End Select
         end if

       else !Group 68 Ccca

         Select Case (pe) !
           case(2) !Aeaa Ambiguous notation because the two groups Abmm and Acmm are not distinguished
             str="A B A A"  !another option is str="A C A A"
           case(3) !Bbcb
             str="B B C B"  !another option is str="B B A B"
           case(4)
             str="C C C A"  !another option is str="C C C B"
         End Select

       end if
     end if

   End Subroutine transf_E_groups

   !!----
   !!---- Set_SpaceGroup_DBase
   !!----    For general Space groups (Shubnikov or Superspace)
   !!----
   !!---- 05/02/2020
   !!
   Module Subroutine Set_SpaceGroup_DBase(Str,mode,SpaceG,xyz_type,Setting,keepdb,parent,database_path,trn_to)
      !---- Arguments ----!
      character(len=*),           intent(in ) :: Str
      character(len=*),           intent(in ) :: mode
      class(spg_type),allocatable,intent(out) :: SpaceG
      character(len=*), optional, intent(in ) :: xyz_type
      character(len=*), optional, intent(in ) :: Setting
      logical,          optional, intent(in ) :: keepdb
      character(len=*), optional, intent(in ) :: parent
      character(len=*), optional, intent(in ) :: database_path
      logical,          optional, intent(in ) :: trn_to

      !---- Local Variables ----!
      integer                         :: i,j,k,d, Dd, L,La, idem,ier, num, n,m, iclass, nmod
      character(len=5)                :: data_typ
      character(len=6)                :: xyz_typ
      character(len=256)              :: line
      character(len=:), allocatable   :: sett,loc_str
      logical                         :: change_setting,centring
      type(rational), dimension(4,4)  :: identity
      type(rational), dimension(3,3)  :: mag_mat,ident3
      type(rational), dimension(:,:) ,allocatable  ::inv
      type(Symm_Oper_Type)            :: transla

      call clear_error()

      !> Check
      if (len_trim(Str) <= 0) then
         err_CFML%Ierr=1
         Err_CFML%Msg="Set_SpaceGroup_DBase@GSPACEGROUPS: Argument not valid to set Space Group!"
         return
      end if
      data_typ=u_case(mode)
      Select Case (trim(data_typ))
        case ("SHUBN")
           allocate(SpG_Type :: SpaceG)
        case ("SUPER")
           allocate(SuperSpaceGroup_Type :: SpaceG)
      End Select
      call Init_SpaceGroup(SpaceG)
      call Rational_Identity_Matrix(Identity)
      call Rational_Identity_Matrix(Ident3)
      change_setting=.false.
      Sett=" "
      if(present(setting)) then
        if(len_trim(setting) == 0) then
          change_setting=.false.
        else
          change_setting=.true.
          sett=setting
        end if
      end if
      loc_str=Str
      i=index(Str,"::")
      if(i /= 0) then
        loc_str=Str(1:i-1)
        sett=adjustl(Str(i+2:))
        change_setting=.true.
      end if
      xyz_typ="xyz"
      if(present(xyz_type)) xyz_typ=xyz_type

      Select Case (trim(data_typ))

        case ("SHUBN")
          if (present(database_path)) then
            call Read_Magnetic_Data(database_path)
          else
            call Read_Magnetic_Data()
          end if
          if (err_cfml%ierr /= 0) return
          read(unit=loc_str,fmt=*,iostat=ier) num
          if(ier /= 0) then
            num=0 !It is supposed that a symbol has been provided
            do i=1,magcount
              !write(*,"(i5,tr5,a)") i, spacegroup_label_bns(i)
              if(trim(loc_str) == trim(spacegroup_label_bns(i)) .or. &
                 trim(loc_str) == trim(spacegroup_label_og(i))  .or. &
                 trim(loc_str) == trim(spacegroup_label_unified(i))) then
                 num=i
                 exit
              end if
            end do
            if(num == 0) then
               Err_CFML%Msg=" => The BNS/UNI/OG symbol: "//trim(loc_str)//" is illegal! "
               Err_CFML%Ierr=1
               if(.not. present(keepdb)) call Deallocate_Magnetic_DBase()
               return
            end if
          else
            if(num < 1 .or. num > MAGCOUNT) then !magcount=1651
               write(unit=Err_CFML%Msg,fmt="(a,i4,a)") " => The number of the Shubnikov group: ",num," is illegal!"
               Err_CFML%Ierr=1
               if(.not. present(keepdb)) call Deallocate_Magnetic_DBase()
               return
            end if
          end if
          Dd=4
          d=3
          SpaceG%D=4
          SpaceG%numspg=0
          SpaceG%numshu=num
          SpaceG%BNS_num=nlabel_bns(num)
          SpaceG%OG_num= nlabel_og(num)
          SpaceG%BNS_symb=spacegroup_label_bns(num)
          SpaceG%OG_symb =spacegroup_label_og(num)
          SpaceG%mag_type=magtype(num)
          SpaceG%init_label=Str
          ! Crystal system
          Select Case (num)
            case(1:7)
              SpaceG%CrystalSys="Triclinic"
            case(8:98)
              SpaceG%CrystalSys="Monoclinic"
            case(99:660)
              SpaceG%CrystalSys="Orthorhombic"
            case(661:1230)
              SpaceG%CrystalSys="Tetragonal"
            case(1231:1338)
              SpaceG%CrystalSys="Trigonal"
            case(1339:1502)
              SpaceG%CrystalSys="Hexagonal"
            case(1503:1651)
              SpaceG%CrystalSys="Cubic"
            case default
              SpaceG%CrystalSys="Unknown"
          End Select

         !Setting the magnetic point group symbol from the BNS label

          SpaceG%mag_pg = Get_MagPG_from_BNS(SpaceG%bns_symb,SpaceG%mag_type)
          SpaceG%magnetic=.true.

          !Setting the parent group symbol and setting from argument "parent" if present
          if(present(parent)) then
            !Parent should be of the form  Xnnn  num  tfrom_parent
            line=adjustl(parent)
            i=index(line," ")
            SpaceG%Parent_spg=parent(1:i-1)
            line=adjustl(line(i:))
            i=index(line," ")
            read(unit=line(1:i),fmt=*,iostat=ier) SpaceG%Parent_num
            if(ier /= 0) then
               SpaceG%Parent_num=0
               SpaceG%tfrom_parent=line(1:i)
            else
               line=adjustl(line(i:))
               i=index(line," ")
               SpaceG%tfrom_parent=line(1:i-1)
            end if
          else
            !Try to deduce the parent space group from the BNS/OG numbers
            line=SpaceG%BNS_num
            i=index(line,".")
            line=line(1:i-1)
            read(unit=line,fmt=*) SpaceG%Parent_num
            if(SpaceG%mag_type < 4) then
              SpaceG%Parent_spg=SpaceG%BNS_symb
              if(SpaceG%mag_type == 2) SpaceG%Parent_spg=SpaceG%Parent_spg(1:len_trim(SpaceG%Parent_spg)-2)
              do i=1,len_trim(SpaceG%Parent_spg)
                if(SpaceG%Parent_spg(i:i) == "'") SpaceG%Parent_spg(i:i) = " "
              end do
              SpaceG%Parent_spg=Pack_String(SpaceG%Parent_spg)
            else
              line=SpaceG%OG_num
              i=index(line,".")
              line=line(1:i-1)
              read(unit=line,fmt=*) SpaceG%Parent_num
              SpaceG%Parent_spg=SpaceG%OG_symb
              if(SpaceG%Parent_spg(3:3) == "2") then
                 SpaceG%Parent_spg(2:4)=" "
              else
                 SpaceG%Parent_spg(2:3)=" "
              end if
              do i=1,len_trim(SpaceG%Parent_spg)
                if(SpaceG%Parent_spg(i:i) == "'") SpaceG%Parent_spg(i:i) = " "
              end do
              SpaceG%Parent_spg=Pack_String(SpaceG%Parent_spg)
            end if
          end if
          SpaceG%standard_setting = .true.
          if(SpaceG%Mag_Type == 4) then
            SpaceG%shu_lat(1)=spacegroup_label_bns(num)(1:1)
            SpaceG%shu_lat(2)=spacegroup_label_bns(num)(3:3)
          else
            SpaceG%shu_lat=spacegroup_label_bns(num)(1:1)
            SpaceG%spg_lat=spacegroup_label_bns(num)(1:1)
          end if
          centring=.false.

          SpaceG%NumOps=wyckoff_pos_count(1,num)  !this is provisory, to be divided by two if centrosymmetric
          SpaceG%Multip=wyckoff_mult(1,num)
          SpaceG%Num_Lat=lattice_bns_vectors_count(num)-3  ! Number of lattice points in a cell minus the three conventional basis vectors

          if(SpaceG%Num_Lat > 0) then
             centring=.true.
             if(allocated(SpaceG%Lat_tr)) deallocate(SpaceG%Lat_tr)
             allocate(SpaceG%Lat_tr(d,SpaceG%Num_Lat))
             SpaceG%Lat_tr=0
             m=0
             do j=4,lattice_bns_vectors_count(num)
                m=m+1
                do i=1,d
                  SpaceG%Lat_tr(i,m)=rational(lattice_bns_vectors(i,j,num), lattice_bns_vectors_denom(j,num))
                end do
             end do
          end if

          if(SpaceG%mag_type == 2) SpaceG%Multip=2*SpaceG%Multip
          call Allocate_SpaceGroup(Dd, SpaceG%Multip, SpaceG)

          m=0
          !write(*,"(3(a,i5))") "Shubnikov number: ",num,"Wyckoff position count: ",wyckoff_pos_count(j,num)," Multiplicity: ",SpaceG%Multip
          SpaceG%anticentred=1
          Do k=1,wyckoff_pos_count(1,num)
            idem=wyckoff_bns_fract_denom(k,1,num)
            SpaceG%Op(k)%Mat=identity
            do i=1,d
              SpaceG%Op(k)%Mat(i,Dd) = rational(wyckoff_bns_fract(i,k,1,num),idem)
            end do
            SpaceG%Op(k)%Mat(1:d,1:d)= wyckoff_bns_xyz(:,:,k,1,num)
            mag_mat = wyckoff_bns_mag(:,:,k,1,num)
            !inv_time=ops_bns_timeinv(k,num)  !Errors in the Database ... to be explored
            SpaceG%Op(k)%dt=rational_determ(SpaceG%Op(k)%Mat(1:d,1:d))

            if(SpaceG%Op(k)%dt > 0) then
               if(rational_equal(mag_mat,SpaceG%Op(k)%Mat(1:d,1:d))) then
                  SpaceG%Op(k)%time_inv=1
               else
                  SpaceG%Op(k)%time_inv=-1
               end if
            else
               if(rational_equal(mag_mat,-SpaceG%Op(k)%Mat(1:d,1:d))) then
                  SpaceG%Op(k)%time_inv=1
               else
                  SpaceG%Op(k)%time_inv=-1
               end if
            end if
            if(SpaceG%Mag_Type == 2) cycle
            if(rational_equal(SpaceG%Op(k)%Mat(1:3,1:3), ident3) .and. SpaceG%Op(k)%time_inv == -1) m=m+1 !counting anti-translations
            if(rational_equal(SpaceG%Op(k)%Mat(1:3,1:3),-ident3) .and. SpaceG%Op(k)%time_inv == -1) then
              SpaceG%anticentred=0
              La=k
            end if
          End Do
          if( SpaceG%anticentred == 0) then
            SpaceG%anticentre_coord = rational(1_LI,2_LI) * SpaceG%Op(La)%Mat(1:d,Dd)
            if(sum(abs(SpaceG%anticentre_coord)) == 0_LI ) SpaceG%anticentred = 2
          end if
          SpaceG%Num_aLat=m       ! Number of anti-centring in a cell

          if(centring) then
            SpaceG%Num_aLat=SpaceG%Num_aLat*(1+SpaceG%Num_Lat)
            n=wyckoff_pos_count(1,num)
            do L=1,SpaceG%Num_Lat
             do k=1,wyckoff_pos_count(1,num)
               SpaceG%Op(k+n)%Mat=SpaceG%Op(k)%Mat
               SpaceG%Op(k+n)%time_inv=SpaceG%Op(k)%time_inv
               SpaceG%Op(k+n)%Mat(1:d,Dd)=Rational_Modulo_Lat(SpaceG%Op(k)%Mat(1:d,Dd)+SpaceG%Lat_tr(:,L))
             end do
             n=n+wyckoff_pos_count(1,num)
            end do
          end if

          if(allocated(SpaceG%aLat_tr)) deallocate(SpaceG%aLat_tr)
          allocate(SpaceG%aLat_tr(3,SpaceG%Num_aLat))     ! Anti-translation vectors
          m=0
          if(SpaceG%Mag_Type /= 2) then
            do k=1,SpaceG%multip
              if(rational_equal(SpaceG%Op(k)%Mat(1:d,1:d),ident3) .and. SpaceG%Op(k)%time_inv == -1) then
                m=m+1
                SpaceG%aLat_tr(:,m) = SpaceG%Op(k)%Mat(1:d,Dd)
              end if
            end do
          end if

          SpaceG%Centred=1        ! Centric or Acentric [ =0 Centric(-1 no at origin),=1 Acentric,=2 Centric(-1 at origin)]
          SpaceG%Centre_coord=0_LI//1_LI ! Fractional coordinates of the inversion centre
          do k=1,wyckoff_pos_count(1,num) !j=1 multiplicity of the general position
            if(rational_equal(SpaceG%Op(k)%Mat(1:d,1:d),-ident3) .and. SpaceG%Op(k)%time_inv == 1) then
              m=k
              SpaceG%Centred=0
              if(sum(abs(SpaceG%Op(k)%Mat(1:d,Dd))) == 0_LI) then
                SpaceG%Centred=2
                exit
              end if
            end if
          end do
          SpaceG%Centre="Non-Centrosymmetric"                          ! Alphanumeric information about the center of symmetry
          if(SpaceG%Centred == 0) then
            SpaceG%Centre_coord=rational(1_LI,2_LI) * SpaceG%Op(m)%Mat(1:d,Dd)
            SpaceG%Centre="                                                                                                              "
            write(unit=SpaceG%Centre,fmt="(a,10a)") "Centrosymmetric (-1 not @the origin), centre at: [ ",(trim(Rational_String(SpaceG%Centre_coord(j)))//" ",j=1,d),"]"
            SpaceG%NumOps=SpaceG%NumOps/2
          else if(SpaceG%Centred == 2) then
            SpaceG%Centre="Centrosymmetric with centre at origin"           ! Alphanumeric information about the center of symmetry
            SpaceG%NumOps=SpaceG%NumOps/2
          end if

          if(SpaceG%Centred == 1 .and. SpaceG%anticentred /= 1) then
             if(SpaceG%anticentred == 0) then
               SpaceG%Centre="                                                                                               "
               write(unit=SpaceG%Centre,fmt="(a,10a)") "Non-centrosymmetric, Anti-centric with -1' @ : [ ",(trim(Rational_String(SpaceG%AntiCentre_coord(j)))//" ",j=1,d),"]"
             else
                SpaceG%Centre="Non-centrosymmetric, Anti-centric with -1' at origin "
                SpaceG%AntiCentre_coord=0_LI//1_LI
             end if
          end if
          !If the magnetic group is paramagnetic duplicate the number of operators
          if(SpaceG%mag_type == 2) then
            m=SpaceG%multip/2
            do i=1,SpaceG%multip/2
              m=m+1
              SpaceG%Op(m)=SpaceG%Op(i)
              SpaceG%Op(i)%time_inv=-SpaceG%Op(i)%time_inv
            end do
          else if(SpaceG%mag_type == 4) then
             SpaceG%Anticentred=1
          end if

          call set_Shubnikov_info()
          SpaceG%Hall=adjustl(shubnikov_info(Litvin2IT(num))%MHall)
          SpaceG%UNI=adjustl(shubnikov_info(Litvin2IT(num))%STD)
          if(trim(SpaceG%UNI) /= trim(spacegroup_label_unified(num))) then
            write(*,"(a)") " Warning! two different values of UNI symbol: "//trim(SpaceG%UNI)//"  <=>  "//trim(spacegroup_label_unified(num))
          end if
          write(SpaceG%UNI_num,"(i4)") Litvin2IT(num)



          if(change_setting) then
              if(present(trn_to)) then
                 SpaceG%Mat2std_Shu=setting
                 sett=inverse_setting(setting,SpaceG%D)
              end if
              SpaceG%Matfrom=sett
              SpaceG%setting=sett
              call Change_Setting_SpaceG(sett, SpaceG)
              SpaceG%standard_setting = .false.
              if(Err_CFML%Ierr /= 0) then
                if(.not. present(keepdb)) call Deallocate_Magnetic_DBase()
                return
              end if
          else
              SpaceG%Matfrom="a,b,c;0,0,0"
              SpaceG%setting="a,b,c;0,0,0"
              SpaceG%Mat2std_Shu="a,b,c;0,0,0"
          end if
          if(.not. present(keepdb)) call Deallocate_Magnetic_DBase()
          do i=1,SpaceG%multip
            SpaceG%Symb_Op(i)=Get_Symb_from_Rational_Mat(SpaceG%Op(i)%Mat,StrCode=xyz_typ,invt=SpaceG%Op(i)%time_inv)
          end do

        !---------------------------------------
        case ("SUPER")  !Read the superspace Database
        !---------------------------------------
          if(present(database_path)) then
             call Read_single_SSG(loc_str,num,database_path)
          else
             call Read_single_SSG(loc_str,num)
          end if
          if(Err_CFML%Ierr /= 0)  return
          iclass=igroup_class(num)
          nmod=iclass_nmod(iclass)
          D=3+nmod
          Dd=D+1
          m=igroup_nops(num)* max(iclass_ncentering(iclass),1)
          !write(*,"(2(a,i4))") " => Multiplicity: ",m,"   3+d = ",D
          call Allocate_SpaceGroup(Dd,m,SpaceG)  !Allocate operators, Centre_coord,etc

          SpaceG%standard_setting=.true.
          SpaceG%tfrom_parent="a,b,c;0,0,0"
          Select Case(d)
            Case(4)
               SpaceG%mat2std="a,b,c,d;0,0,0,0"
            Case(5)
               SpaceG%mat2std="a,b,c,d,e;0,0,0,0,0"
            Case(6)
               SpaceG%mat2std="a,b,c,d,e,f;0,0,0,0,0,0"
          End Select
          SpaceG%magnetic=.false.
          SpaceG%Centre="Acentric"                    ! Alphanumeric information about the center of symmetry
          SpaceG%Parent_num=igroup_spacegroup(num)    ! Number of the parent Group


          SpaceG%Num_Lat=iclass_ncentering(iclass)-1  ! Number of centring points in a cell (notice that in the data base what is stored is the number of lattice points per cell: Num_lat+1)
          SpaceG%SPG_Lat="P"                          ! Assume initially that the cell is primitive
          SpaceG%CrystalSys=" "                       ! Crystal System
          Select Case(igroup_spacegroup(num))
            case(1:2)
              SpaceG%CrystalSys="Triclinic"
            case(3:15)
              SpaceG%CrystalSys="Monoclinic"
            case(16:74)
              SpaceG%CrystalSys="Orthorhombic"
            case(75:142)
              SpaceG%CrystalSys="Tetragonal"
            case(143:167)
              SpaceG%CrystalSys="Trigonal"
            case(168:194)
              SpaceG%CrystalSys="Hexagonal"
            case(195:230)
              SpaceG%CrystalSys="Cubic"
            case default
              SpaceG%CrystalSys="Unknown"
          End Select
          SpaceG%Pg=" "                               ! Point group
          SpaceG%init_label=Str
          SpaceG%Mag_Pg=" "                           ! Magnetic Point group
          SpaceG%Laue=" "                             ! laue class
          SpaceG%setting=" "                          ! setting
          SpaceG%generators_list=" "                  ! generators
          SpaceG%anticentred = 1
          SpaceG%spg_symb=group_label(num)
          i=index(SpaceG%spg_symb,"(")
          SpaceG%Parent_spg=SpaceG%spg_symb(1:i-1)

          xyz_typ="xyz"
          if(present(xyz_type)) xyz_typ=xyz_type
          SpaceG%mag_type= 1                         ! No time-reversal is associated with the symmetry operators
          SpaceG%Num_aLat= 0                         ! Number of anti-lattice points in a cell (here is always zero because the database if for crystallographic groups)
          SpaceG%Centred = 1                         ! Centric or Acentric [ =0 Centric(-1 no at origin),=1 Acentric,=2 Centric(-1 at origin)]
          SpaceG%numspg  = num                       ! Number of the superspace group
          SpaceG%NumOps  = igroup_nops(num)          ! Number of reduced set of S.O. (here is only removing lattice centring), divided by two if centrosymmetric
          SpaceG%Multip  = m                         ! General multiplicity

          Select Type (Grp => SpaceG)
            type is (SuperSpaceGroup_Type)
                !write(*,"(a,i3)") " => Allocating SuperSpace for : ",nmod
                Grp%SSG_symb=group_label(num)
                Grp%SSG_Bravais=class_label(iclass)
                i=index(Grp%SSG_symb,"(")
                Grp%Parent_spg=Grp%SSG_symb(1:i-1)
                Grp%Bravais_num=igroup_class(num)        ! Number of the Bravais class
                Grp%SSG_nlabel=group_nlabel(num)
                if( Grp%Num_Lat > 0 .and. Grp%SSG_symb(1:1) == "P" ) then
                  Grp%SPG_Lat="X"
                else
                  Grp%SPG_Lat=Grp%SSG_symb(1:1)
                end if
                Grp%nk=nmod                              !(d=1,2,3, ...) number of q-vectors
                if(Allocated(Grp%kv)) deallocate(Grp%kv)
                if(Allocated(Grp%sintlim)) deallocate(Grp%sintlim)
                Allocate(Grp%kv(3,nmod),Grp%sintlim(nmod))
                !write(*,"(a,i6,a)") " => Allocating Om for : ",Dd*Dd*Grp%Multip, "  elements"
                Allocate(Grp%Rot(3,3,m),Grp%t(3,m),Grp%tI(nmod,m),Grp%M(nmod,3,m),Grp%Ep(nmod,nmod,m))

                Grp%kv=0.0; Grp%sintlim=0.0; Grp%Rot=0; Grp%t=0.0; Grp%tI=0.0; Grp%M=0; Grp%Ep=0
                !Grp%nq=0     !  This is not allowed in gfortran, probably because they are not
                !Grp%nharm=0  !  allocatable components and are not under the pointer Grp => SpaceG
                !the components q_coeff(nk,nq) cannot be allocated until experimental data are read
          End Select

          !write(*,"(a,i3)") " => Allocating Lattice centring vectors : ",SpaceG%Num_Lat
          if(SpaceG%Num_Lat > 0) then
            Allocate(SpaceG%Lat_tr(D,SpaceG%Num_Lat))  !This is not allocated in Allocate_SpaceGroup
            SpaceG%Lat_tr=0_LI//1_LI
            do j=1,SpaceG%Num_Lat
               SpaceG%Lat_tr(1:D,j)= rational_simplify(iclass_centering(1:D,j+1,iclass)//iclass_centering(Dd,j+1,iclass))
            end do
          end if
          do i=1,SpaceG%NumOps
             SpaceG%Op(i)%Mat= rational_simplify(igroup_ops(1:Dd,1:Dd,i,num)//igroup_ops(Dd,Dd,i,num))
             SpaceG%Symb_Op(i)=Get_Symb_from_Rational_Mat(SpaceG%Op(i)%Mat,StrCode=xyz_typ) !The data base has no magnetic components
          end do

          !write(*,"(a,i4)") " => Loaded operators : ",SpaceG%NumOps
          !Look for a centre, or anticentre, of symmetry

          allocate(Inv(D,D),transla%Mat(Dd,Dd))
          call Rational_Identity_Matrix(transla%Mat)
          call Rational_Identity_Matrix(Inv)
          Inv=-Inv

          do i=1,SpaceG%NumOps
            if(Rational_Equal(Inv,SpaceG%Op(i)%Mat(1:D,1:D))) then
              if(sum(abs(SpaceG%Op(i)%Mat(1:D,Dd))) == 0_LI) then
                SpaceG%Centred=2
                SpaceG%Centre_coord=0//1
                SpaceG%Centre="Centrosymmetric with centre at origin"
              else
                SpaceG%Centred=0
                SpaceG%Centre_coord=SpaceG%Op(i)%Mat(1:D,Dd)/2_LI
                write(unit=SpaceG%Centre,fmt="(a)") "Centrosymmetric with centre at : "//Rational_String(SpaceG%Centre_coord)
              end if
              exit
            end if
          end do
          if(SpaceG%Centred == 1) then !Look for an anticentre
            do i=1,SpaceG%NumOps
              if(Rational_Equal(-Inv,SpaceG%Op(i)%Mat(1:D,1:D)) .and. SpaceG%Op(i)%time_inv == -1) then
                if(sum(abs(SpaceG%Op(i)%Mat(1:D,Dd))) == 0_LI) then
                  SpaceG%AntiCentred=2
                  SpaceG%AntiCentre_coord=0//1
                  SpaceG%Centre="Non-centrosymmetric, Anti-centric with -1' @ origin"
                else
                  SpaceG%AntiCentred=0
                  SpaceG%AntiCentre_coord=SpaceG%Op(i)%Mat(1:D,Dd)/2_LI
                  SpaceG%Centre="                                                                                                 "
                  write(unit=SpaceG%Centre,fmt="(a)") "Non-centrosymmetric, Anti-centric with -1' @ : "//Rational_String(SpaceG%AntiCentre_coord)
                end if
                exit
              end if
            end do
          end if

          !Extend the symmetry operators to the whole set of lattice centring
          !set the symmetry operators symbols
          m=SpaceG%NumOps
          do i=1,SpaceG%Num_Lat
            transla%Mat(1:D,Dd)=SpaceG%Lat_tr(1:D,i)
            do j=1,SpaceG%NumOps
               m=m+1
               SpaceG%Op(m)=SpaceG%Op(j)*transla
            end do
          end do
          if(m /= SpaceG%Multip) then
             Err_CFML%Ierr=1
             Err_CFML%Msg="Error extending the symmetry operators for a centred cell"
          end if
          !write(*,"(a,i4)") " => Extended operators to : ",SpaceG%Multip
          !Adjust ssg%NumOps to remove the centre of symmetry if Exist
          if(SpaceG%Centred == 2 .or. SpaceG%Centred == 0) SpaceG%NumOps=SpaceG%NumOps/2

          if(change_setting) then
              if(present(trn_to)) then
                 SpaceG%mat2std=setting
                 sett=inverse_setting(setting,SpaceG%D)
                 SpaceG%matfrom=sett
              else
                 SpaceG%setting=sett
              end if
              call Change_Setting_SpaceG(sett, SpaceG)
              SpaceG%standard_setting = .false.
              if(Err_CFML%Ierr /= 0) then
                if(.not. present(keepdb)) call Deallocate_SSG_DBase()
                return
              end if
          else
              Select Case(d)
                Case(4)
                   SpaceG%setting="a,b,c,d;0,0,0,0"
                Case(5)
                   SpaceG%setting="a,b,c,d,e;0,0,0,0,0"
                Case(6)
                   SpaceG%setting="a,b,c,d,e,f;0,0,0,0,0,0"
              End Select
          end if
          if(.not. present(keepdb)) then
             call Deallocate_SSG_DBase()
             !write(*,"(a)") " => Deallocated SSG_DBase"
          end if
          !Get the symmetry symbols
          do i=1,SpaceG%Multip
            SpaceG%Symb_Op(i)= Get_Symb_from_Rational_Mat(SpaceG%Op(i)%Mat,StrCode=xyz_typ)
          end do

          Select Type (Grp => SpaceG)
            type is (SuperSpaceGroup_Type)

                do i=1,Grp%Multip
                    Grp%Rot(:,:,i)=Grp%Op(i)%Mat(1:3,1:3)
                    Grp%t    (:,i)=Grp%Op(i)%Mat(1:3,dd)
                    Grp%tI   (:,i)=Grp%Op(i)%Mat(4:d,dd)
                    Grp%M  (:,:,i)=Grp%Op(i)%Mat(4:d,1:3)
                    Grp%Ep (:,:,i)=Grp%Op(i)%Mat(4:d,4:d)
                end do

          End Select
      End Select

   End Subroutine Set_SpaceGroup_DBase
   !!----
   !!---- SET_SPACEGROUP_GEN
   !!----    For general Space groups (Crystallographic, Shubnikov or Superspace)
   !!----    In this case Superspace groups are generated without knowing the symbol
   !!----
   !!---- 05/02/2020
   !!
   Module Subroutine Set_SpaceGroup_gen(Str, SpaceG, NGen, Gen, set_inv, database_path)
      !---- Arguments ----!
      character(len=*),                          intent(in ) :: Str
      class(spg_type),allocatable,               intent(out) :: SpaceG
      integer,                         optional, intent(in ) :: NGen
      character(len=*),  dimension(:), optional, intent(in ) :: Gen
      logical,                         optional, intent(in ) :: set_inv
      character(len=*),                optional, intent(in ) :: database_path


      !---- Local Variables ----!
      integer                                      :: i,j,k,n_gen, n_it, d, ier !,j
      integer                                      :: n_laue, n_pg, lo !, nfin
      character(len=80), dimension(:), allocatable :: l_gen
      character(len=20)                            :: str_HM, str_HM_std, str_Hall, str_CHM
      character(len=5)                             :: car
      character(len=:), allocatable                :: gList,loc_str
      type(rational), dimension(3)                 :: ta,tb,tc,ti,tr1,tr2

      logical :: by_Gen, by_Hall, ok1, ok2, ok3, magnetic
      !> Init
      by_Gen=.false.; by_Hall=.false.
      ok1=.false.; ok2=.false.; ok3=.false.
      magnetic=.false.
      n_gen=0
      gList=" "
      n_it=0
      str_HM=" "
      str_Hall=" "
      str_CHM=" "
      str_HM_std=" "
      n_laue=0
      n_pg=0
      tc=[1.0/2.0,1.0/2.0,0.0]
      tb=[1.0/2.0,0.0,1.0/2.0]
      ta=[0.0,1.0/2.0,1.0/2.0]
      ti=[1.0/2.0,1.0/2.0,1.0/2.0]
      tr2=[1.0/3.0,2.0/3.0,2.0/3.0]
      tr1=[2.0/3.0,1.0/3.0,1.0/3.0]
      loc_str=Str
      call clear_error()

      if (present(ngen) .and. present(gen)) n_gen=ngen

      !if(CFML_DEBUG) write(*,"(a)") " Provided string to Set_SpaceGroup_gen: "//trim(Str)

      !> Check
      if (len_trim(Str) <= 0 .and. n_gen <= 0) then
         err_CFML%Ierr=1
         Err_CFML%Msg="Set_SpaceGroup@GSPACEGROUPS: Argument not valid to set Space Group!: len_trim(Str) <= 0 .and. n_gen <= 0"
         return
      end if

      !> Spacegroup from Str argument
      if (n_gen == 0) then
         !> Check if we are providing a generator list as the first argument
         i=index(Str,")")
         if (index(Str,";") > 4 .or. index(Str,",1") /= 0  .or. index(Str,",+1") /= 0 .or. index(Str,",-1") /= 0 .or. i /= 0 .or. index(Str,"UNI") /= 0) then !Call directly to the space group constructor

            if(i /= 0) then  !Format provided by Harold Stokes e.g. (x,y,-z+1/2)';(-x,y,z)
              !Transform the list of generators to the standard form in CrysFML
              loc_str=Str
              call ISO_to_jones_notation(loc_str)
              !write(*,"(a)") trim(Str)//"      "//trim(loc_str)
              magnetic=.true.
            end if

            j=index(loc_str,"trn_to")
            if(j /= 0) loc_str=loc_str(1:j-1)
            i=index(loc_str,"UNI")
            if(i /= 0) then
               if(i == 1) then
                  loc_str=adjustl(loc_str(i+3:))
               else
                  loc_str=loc_str(1:i-1)
               end if
               magnetic=.true.
               call Set_Shubnikov_Info()
               do i=1,NUM_SHUBNIKOV
                  if (trim(loc_str) /= trim(Shubnikov_info(i)%STD)) cycle
                  loc_str=Shubnikov_info(i)%Generators
                  exit
               end do
            end if

            j = index(loc_str,'t')
            k = index(loc_str,'x4')
            if (j > 0 .or. k > 0) then
               allocate(superspacegroup_type :: SpaceG)
            else
               allocate(spg_type :: SpaceG)
            end if
            call Init_SpaceGroup(SpaceG)
            if(present(set_inv)) then
              call Group_Constructor(loc_Str,SpaceG,set_inv=set_inv)
            else
              call Group_Constructor(loc_Str,SpaceG)
            end if

            if (SpaceG%D == 4) then
               if (present(database_path)) then
                  call Identify_Group(SpaceG,database_path)
               else
                  call Identify_Group(SpaceG)
               end if
               if (Err_CFML%Ierr == 1) then
                  write(unit=*,fmt="(a)") "  WARNING: "//Err_CFML%Msg
                  call clear_error()
               end if
               call set_Shubnikov_info()
               if (len_trim(SpaceG%BNS_num) /= 0 .and. SpaceG%numshu /= 0 .and. len_trim(SpaceG%BNS_symb) == 0) then
                  call set_Shubnikov_info()
                  SpaceG%BNS_symb=Shubnikov_Info(Litvin2IT(SpaceG%numshu))%BNS
               end if
               if(SpaceG%mag_type == 4) SpaceG%Anticentred=1
               SpaceG%mag_pg = Get_MagPG_from_BNS(SpaceG%bns_symb,SpaceG%mag_type)
            end if

            return
         end if

         !Check if it is the group P1
         if(trim(Str) == "P1" .or. trim(Str) == "P 1") then
            allocate(spg_type :: SpaceG)
           call P1_Constructor(4, .false., SpaceG)
           return
         end if
         !> Init Spgr_Info
         call Set_Spgr_Info()

         !> Is a IT  number?
         read(unit=str, fmt=*, iostat=ier) n_it
         if (ier == 0) then
            gList=get_IT_Generators(str)  ! IT take our default choice for SpaceGroup
            call Get_SpaceGroup_Symbols(Str, str_HM, str_Hall)
            do i=1,NUM_SPGR_INFO
               if (trim(str_HM) /= trim(spgr_info(i)%hm)) cycle
               n_laue=spgr_info(i)%laue
               n_pg=  spgr_info(i)%pg
               magnetic=.false.
               exit
            end do
            str_HM_std=trim(str_HM)
         else
            n_it=0
         end if

         !> Is HM symbol defined in SPGR_INFO?
         if (n_it == 0) then
            str_HM=u_case(trim(Str))
            !Check if the new nomenclature of some orthorhombic groups are being used
            if(index(str_HM,"E") /= 0) then !Convert to the traditional nomenclature to use the appropriate generators
              !The new notation cannot handle all possible settings for the space groups 67 and 68
              call transf_E_groups(str_HM)
            end if
            do i=1,NUM_SPGR_INFO
               if (trim(str_HM) /= trim(spgr_info(i)%hm)) cycle
               n_it    = spgr_info(i)%n
               str_hall= spgr_info(i)%hall
               n_laue  = spgr_info(i)%laue
               n_pg    = spgr_info(i)%pg

               !> Get generators list from standard
               write(unit=car, fmt='(i3)') n_it
               call Get_SpaceGroup_Symbols(car, str_HM_std)
               if (trim(str_HM_std) == trim(str_HM)) then
                  gList=get_IT_Generators(car)
               end if
               magnetic=.false.
               exit
            end do
            !if(CFML_DEBUG) then
            !  write(*,"(a,i3)") " Looking for SPGR_INFO, n_it= ",n_it
            !  write(*,"(a)")    " IT generators: "//trim(gList)
            !end if
         end if

         !> Is Hall symbol defined in SPGR_INFO?
         if (n_it == 0) then
            Str_Hall=l_case(trim(Str))
            if (str_hall(1:1) == '-') then
               str_hall(2:2)=u_case(str_hall(2:2))
            else
               str_hall(1:1)=u_case(str_hall(1:1))
            end if
            do i=1,NUM_SPGR_INFO
               if (trim(str_Hall) /= trim(spgr_info(i)%hall)) cycle
               n_it=spgr_info(i)%n
               str_hm=spgr_info(i)%hm
               n_laue=spgr_info(i)%laue
               n_pg=  spgr_info(i)%pg

               !> Get generators list from standard
               write(unit=car,fmt="(i4)") n_it
               call Get_SpaceGroup_Symbols(car, str_HM_std)
               if (trim(str_HM_std)==trim(str_HM)) then
                  gList=get_IT_Generators(car)
               end if
               magnetic=.false.
               exit
            end do
         end if

         !> Is compact HM symbol?
         if (n_it == 0) then
            Str_CHM=u_case(trim(Str))
            call Get_SpaceGroup_Symbols(str_CHM, str_HM)
            do i=1,NUM_SPGR_INFO
               if (trim(str_HM) /= trim(spgr_info(i)%hm)) cycle
               n_it=spgr_info(i)%n
               str_hall=spgr_info(i)%hall
               n_laue=spgr_info(i)%laue
               n_pg=  spgr_info(i)%pg
               magnetic=.false.
               !> Get generators list from standard
               write(unit=car,fmt="(i4)") n_it
               call Get_SpaceGroup_Symbols(car, str_HM_std)
               if (trim(str_HM_std)==trim(str_HM)) then
                  gList=get_IT_Generators(car)
               end if
               exit
            end do
         end if

         !> Is it a compact BNS symbol?
         if (n_it == 0) then
            call Set_Shubnikov_Info()
            do i=1,NUM_SHUBNIKOV
               if (trim(str) /= trim(Shubnikov_info(i)%BNS)) cycle
               str_hall=Shubnikov_info(i)%Mhall
               exit
            end do
         end if

         if (len_trim(gList) == 0) then   !This is the case when we provide a non-standard symbol (e.g. P b n m instead of P n m a)
            call Get_Generators(str_hall,l_gen,n_gen)
            if (Err_CFML%Ierr /= 0) return
            by_Hall=.true.
         else
            call Get_Generators(gList, d, l_gen, n_gen)
            !if(CFML_DEBUG) then
            !  write(*,"(a,i3)") " List of Generators: ",n_gen
            !  do i=1,n_gen
            !    write(*,"(i3,a)")   i, " Generator: "//l_gen(i)
            !  end do
            !end if
         end if
         if(.not. allocated(l_gen)) then
            allocate(l_gen(1))
            l_gen="x,y,z"
         end if
      else
         by_Gen=.true.
         allocate(l_gen(n_gen))
         l_gen=gen(1:n_gen)
         do i=1,n_gen
            j=index(l_gen(i),",",back=.true.)
            if(trim(l_gen(i)(j+1:)) == "-1" .or. trim(l_gen(i)(j+1:)) == "1" .or. trim(l_gen(i)(j+1:)) == "+1") then
                magnetic=.true.
            end if
         end do
      end if

      !> Generate the spacegroup
      j = index(l_gen(1),'t')
      k = index(l_gen(1),'x4')
      if (j > 0 .or. k > 0) then
         allocate(superspacegroup_type :: SpaceG)
      else
         allocate(spg_type :: SpaceG)
      end if
      call Init_SpaceGroup(SpaceG)
      SpaceG%magnetic=magnetic
      if(present(set_inv)) then
        call Group_Constructor(l_gen,SpaceG,set_inv=set_inv)
      else
        call Group_Constructor(l_gen,SpaceG)
      end if
      SpaceG%init_label=Str
      if (Err_CFML%Ierr /= 0) return
      if(n_it /= 0) then
        SpaceG%spg_symb= str_HM_std(1:1)//l_case(str_HM_std(2:))
        SpaceG%numspg=n_it
        SpaceG%pg= Get_PointGroup_Str(n_pg)
        SpaceG%Laue=Get_Laue_Str(n_laue)
        call Identify_Crystal_System(SpaceG)
      end if
      if(len_trim(str_hall) /= 0) SpaceG%Hall=str_hall
      !Write(*,*) " Num_lat:",SpaceG%Num_lat
      Select Case (SpaceG%Num_lat)
        Case(0)
           SpaceG%SPG_lat="P"
        Case(1)
           if(Rational_Equal(SpaceG%Lat_tr(1:3,1),tc)) SpaceG%SPG_lat="C"
           if(Rational_Equal(SpaceG%Lat_tr(1:3,1),tb)) SpaceG%SPG_lat="B"
           if(Rational_Equal(SpaceG%Lat_tr(1:3,1),ta)) SpaceG%SPG_lat="A"
           if(Rational_Equal(SpaceG%Lat_tr(1:3,1),ti)) SpaceG%SPG_lat="I"
        Case(2)
           if(Rational_Equal(SpaceG%Lat_tr(1:3,1),tr1) .and. &
              Rational_Equal(SpaceG%Lat_tr(1:3,2),tr2)) then
              SpaceG%SPG_lat="R"
           else if(Rational_Equal(SpaceG%Lat_tr(1:3,2),tr2) .and. &
                   Rational_Equal(SpaceG%Lat_tr(1:3,1),tr1)) then
              SpaceG%SPG_lat="R"
           end if
        Case(3)
           ok1=.false.; ok2=.false.; ok3=.false.
           do i=1,3
             if(Rational_Equal(SpaceG%Lat_tr(1:3,i),ta)) ok1=.true.
           end do
           do i=1,3
             if(Rational_Equal(SpaceG%Lat_tr(1:3,i),tb)) ok2=.true.
           end do
           do i=1,3
             if(Rational_Equal(SpaceG%Lat_tr(1:3,i),tc)) ok3=.true.
           end do
           if(ok1 .and. ok2 .and. ok3) SpaceG%SPG_lat="F"
        Case Default
           SpaceG%SPG_lat="X"
      End Select

      !> Identify Group Only for Crystallographic or Shubnikov groups
      if (SpaceG%D == 4 .and. n_it == 0) then
        if (present(database_path)) then
          call Identify_Group(SpaceG,database_path)
        else
          call Identify_Group(SpaceG)
        end if
        if(Err_CFML%Ierr == 1) then
           write(unit=*,fmt="(a)") "  WARNING: "//Err_CFML%Msg
           call clear_error()
        end if
        if(len_trim(SpaceG%BNS_num) /= 0 .and. SpaceG%numshu /= 0 .and. len_trim(SpaceG%BNS_symb) == 0) then
          call set_Shubnikov_info()
          SpaceG%BNS_symb=Shubnikov_Info(Litvin2IT(SpaceG%numshu))%BNS
          SpaceG%UNI=Shubnikov_Info(Litvin2IT(SpaceG%numshu))%STD
          write(SpaceG%UNI_num,"(i4)") Litvin2IT(SpaceG%numshu)
        end if
        SpaceG%mag_pg = Get_MagPG_from_BNS(SpaceG%bns_symb,SpaceG%mag_type)
        if(by_Hall) then
           SpaceG%spg_symb = str(1:1)//l_case(str(2:))
        else if (.not. by_Gen) then
           str_HM = Get_HM_Standard(SpaceG%numspg)
           SpaceG%spg_symb = str_HM(1:1)//l_case(str_HM(2:))
           !if(n_it > 0 .and. len_trim(SpaceG%spg_symb) == 0) SpaceG%spg_symb=trim(spgr_info(n_it)%hm) !str_HM(1:1)//l_case(str_HM(2:))
        end if
      end if
      if(SpaceG%mag_type == 4) SpaceG%Anticentred=1

      !Eliminate the information about time reversal in the symbol of operators
      !when the space group is non-magnetic
      if(.not. SpaceG%magnetic) then
        do i=1,SpaceG%multip
          lo=len_trim(SpaceG%Symb_Op(i))
          if(SpaceG%Symb_Op(i)(lo-1:lo) == ",1") &
             SpaceG%Symb_Op(i)=SpaceG%Symb_Op(i)(1:lo-2)
          if(SpaceG%Symb_Op(i)(lo-2:lo) == ",+1") &
             SpaceG%Symb_Op(i)=SpaceG%Symb_Op(i)(1:lo-3)
        end do
      end if

   End Subroutine Set_SpaceGroup_gen

   !!---- Module Subroutine Set_gSpG_from_string(str,SpG,Setting_Search)
   !!----   character(len=*), intent(in) :: str
   !!----   Class(SpG_Type),  intent(out):: SpG
   !!----   logical,optional, intent(in) :: Setting_Search
   !!----
   !!----   Subroutine for selecting the proper interface for constructing
   !!----   a general Space Group (including superspace group)
   !!----   The string str may contain the setting an other instructions
   !!----   for selecting the proper interface to generate the general
   !!----   space group. The string may have whatever of the following forms:
   !!----
   !!----   1236   :: a,b,c;0,0,0      shub        <-- Shubnikov group type 1236 in standard setting"
   !!----   123    :: a,-c,b;1/2,0,0               <-- Space group type 123 in non-standard setting"
   !!----   Pn'ma                                  <-- Shubnikov group type Pn'ma in standard setting"
   !!----   UNI Pnna.1'_a[Pncm]                    <-- Shubnikov group given in unified notation"
   !!----   UNI Pnna.1'_a[Pncm] :: -c,b,a;0,0,0    <-- Shubnikov group given in unified notation in non-stardard setting"
   !!----   Pn'ma  :: -c,b,a;0,0,0     shub        <-- Shubnikov group type Pn'ma in the non-standard setting Pcmn'"
   !!----   Pn'ma  :: c/2,b,-a/2;0,0,0 shub trn_to <-- Shubnikov group type Pn'ma in a supercell with transformation to standard"
   !!----   B 2 C B                                <-- space #41 of standard symbol Aba2"
   !!----   13232  :: sup                          <-- SuperSpace group 13232 in standard setting"
   !!----   221    :: a2,-a1,a3,a4;0,0,1/2,0  sup  <-- SuperSpace group #221 in non-standard setting"
   !!----   Pnma(0,0,g)0s0  :: sup                 <-- SuperSpace group of standard symbol"
   !!----   x,-y,z,t,1;x,y,z,t+1/2,-1              <-- The generators of a Magnetic SuperSpace group"
   !!----   x1,-x2,x3,x4,1;x1,x2,x3,x4+1/2,-1      <-- The generators of a Magnetic SuperSpace group"
   !!----
   !!----
   !!----
   !!----
   Module Subroutine Set_gSpG_from_string(str,SpG,Setting_Search,database_path)
      !---- Arguments ----!
      character(len=*),            intent(in) :: str
      Class(SpG_Type), allocatable,intent(out):: SpG
      logical, optional,           intent(in) :: Setting_Search
      character(len=*), optional,  intent(in) :: database_path
      !--- Local Variables ---!
      !character(len=:),allocatable :: sett, strs,aux
      character(len=len(str)) :: sett, strs,aux
      character(len=5) :: Mode
      integer :: ier, num_group, i, j
      logical :: set_given, setting, to_std

       setting=.false.; to_std=.false.
       if(present(Setting_Search)) setting=.true.
       Mode=" "
       strs=trim(str)
       set_given=.false.
       !Strip the input string to remove eventual SPGR, SSPG, SPG, GEN, Generators, etc
       i=index(strs," ")

       if(i /= 0) then
         aux=u_case(strs(1:i-1))
         Select Case(aux)
            Case("SHUB")
                if(index(strs(i:),"shub") == 0) then
                    strs=adjustl(strs(i:))//"  shub"
                else
                    strs=adjustl(strs(i:))
                end if
                allocate(SpG_Type :: SpG)
                call Init_SpaceGroup(SpG)
            Case("SSG","SUPER","SSPG")
                if(index(strs(i:),"sup") == 0) then
                    strs=adjustl(strs(i:))//"  sup"
                else
                    strs=adjustl(strs(i:))
                end if
                allocate(SuperSpaceGroup_Type :: SpG)
                call Init_SpaceGroup(SpG)
            Case("HALL","SPGR","SPACEG","MHALL", &
                 "GEN","GENR","SYMM","GENLIST","GENERATORS","LIST")
                  strs=adjustl(strs(i:))
                  if(trim(aux) == "SPGR" .or. trim(aux) == "SPACEG" .or. trim(aux) == "HALL" .or. trim(aux) == "MHALL") then
                    Mode="Symb"
                    allocate(SpG_Type :: SpG)
                    call Init_SpaceGroup(SpG)
                  end if
          End Select
       end if
       read(unit=strs,fmt=*,iostat=ier) num_group
       if (ier == 0) then
          !Now determine if setting is appearing in which case it uses data bases
          i=index(strs,"::"); j=index(strs,"trn_to")
          if(j /= 0) to_std=.true.
          if(i /= 0) then
             j=index(str,"sup")  !Superspace
             if(j /= 0) then
               Mode="SUPER"
               if((index(strs,"a1") /= 0 .or. index(strs,"d") /= 0) .and. setting) then
                 sett= adjustl(strs(i+2:j-1))
                 set_given=.true.
               end if
             else
               j=index(strs,"shub")  !Shubnikov group
               if(j /= 0) then
                 Mode="SHUBN"
                 if(setting) then
                   sett= adjustl(strs(i+2:j-1))
                   if(len_trim(sett) /= 0) set_given=.true.
                 end if
               else
                 if(setting) then
                   sett= adjustl(strs(i+2:))
                   if(len_trim(sett) /= 0) set_given=.true.
                 end if
               end if
             end if
             strs=strs(1:i-1)
          end if
       else

          i=index(strs,"::"); j=index(strs,"trn_to")
          if(j /= 0) to_std=.true.
          if(i /= 0) then
             j=index(str,"sup")  !Superspace
             if(j /= 0) then
               Mode="SUPER"
               if(index(strs,"a1") /= 0  .or. index(strs,"d") /= 0 .and. setting) then
                 sett= adjustl(str(i+2:j-1))
                 set_given=.true.
               end if
             else
               j=index(strs,"shub")  !Shubnikov group
               if(j /= 0) then
                 Mode="SHUBN"
                 if(setting) then
                   sett= adjustl(str(i+2:j-1))
                   if(len_trim(sett) /= 0) set_given=.true.
                 end if
               else
                 if(setting) then
                   sett= adjustl(str(i+2:))
                   if(len_trim(sett) /= 0) set_given=.true.
                 end if
               end if
             end if
             strs=strs(1:i-1)
          end if
       end if

       Select Case (trim(Mode))
         Case("SHUBN","SUPER")
           if(set_given) then
             if(to_std) then
                if (present(database_path)) then
                  call Set_SpaceGroup(Strs,Mode,SpG,Setting=Sett,trn_to=.true.,database_path=database_path)
                else
                  call Set_SpaceGroup(Strs,Mode,SpG,Setting=Sett,trn_to=.true.)
                end if
             else
                if (present(database_path)) then
                  call Set_SpaceGroup(Strs,Mode,SpG,Setting=Sett,database_path=database_path)
                else
                  call Set_SpaceGroup(Strs,Mode,SpG,Setting=Sett)
                end if
             end if
           else
             if (present(database_path)) then
               call Set_SpaceGroup(Strs,Mode,SpG,database_path=database_path)
             else
               call Set_SpaceGroup(Strs,Mode,SpG)
             end if
           end if
         Case default
           if (present(database_path)) then
              call Set_SpaceGroup(Strs,SpG,database_path=database_path)
           else
              call Set_SpaceGroup(Strs,SpG)
           end if
           if(Err_CFML%Ierr /= 0) then
              write(*,*) "ERROR: "//trim(Err_CFML%Msg)
           end if
           if(set_given) then
             if(to_std) then
                sett=inverse_setting(sett,SpG%D)
             end if
             call Change_Setting_SpaceG(sett,SpG)
           end if
       End Select

   End Subroutine Set_gSpG_from_string

End SubModule gS_Set_SpaceG
