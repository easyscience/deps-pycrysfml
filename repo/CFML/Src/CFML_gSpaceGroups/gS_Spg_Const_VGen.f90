!!----
!!----
!!----
SubModule (CFML_gSpaceGroups) gs_Spg_Const_VGen
   implicit none
   Contains



   !!----
   !!---- P1_CONSTRUCTOR
   !!---- Constructor of the magnetic space group P1 of different dimensions
   !!---- 31/10/25
   !!
   Module Subroutine P1_Constructor(d, mag, Spg)
     integer,         intent(in)     :: d !Dimension of matrix operators
     logical,         intent(in)     :: mag
     Class(SpG_Type), intent(in out) :: SpG

     !Setting SpG as P1-like space group
     SpG%magnetic=mag
     SpG%mag_type= 1
     SpG%numspg  = 1       ! Spacegroup number (IT if standard)
     SpG%numshu  = 1       ! Shubnikov group number
     SpG%numops  = 1       ! Number of total symmetry operators
     SpG%centred = 1       ! 0: Centric(-1 no at origin),  1: Acentric, 2: Centric(-1 at origin)
     SpG%anticentred=0     ! 0: Anti-Centric(-1' no at origin), 1: Anti-Acentric, 2: Anti-Centric(-1' at origin)
     SpG%mag_type = 1      ! Type of magnetic group: 1 (Colorless) 2 (Paramagnetic) 3 (Black & White:1) 4(Black & White: 2)
     SpG%num_lat  = 0      ! Number of lattice points in cell
     SpG%num_alat = 0      ! Number of centring anti-translations
     SpG%Parent_num=1      ! Number of the parent Group
     SpG%Bravais_num=0     ! Number of the Bravais class of the superspace (Stokes & Campbell database)
     SpG%spg_lat  ="P"     ! Lattice type
     SpG%shu_lat  =" "     ! Shubnikov lattice type
     SpG%BNS_num  =" "     ! Numerical Label in the data base of Stokes-Campbell
     SpG%OG_num   =" "     ! Numerical Label in the data base of D. Litvin
     SpG%BNS_symb ="P1"    ! Belonov-Neronova-Smirnova Shubnikov group symbol
     SpG%OG_symb  ="P1"    ! Opechowski-Guccione Shubnikov group symbol
     SpG%Hall     =" "     ! Hall symbol
     SpG%Centre   ="Non-centrosymmetric"
     SpG%Init_label="  "
     SpG%Parent_Spg="  "
     SpG%tfrom_parent="  "
     SpG%Parent_Spg="  "
     if(mag) then
        SpG%UNI      ="P1.1"  ! Unified Symbol
     else
        SpG%UNI      =" "     ! Unified Symbol
     end if
     SpG%UNI_num     =" "         ! Unified number
     SpG%crystalsys  ="Triclinic" ! Crystal system
     SpG%pg          ="1"         ! Point group
     SpG%mag_pg      ="1.1"   ! Magnetic PG
     SpG%laue        ="-1"    ! Laue group
     SpG%setting     =" "     ! Operators transformed by "setting" (e.g. -a+b,a+b,-c;1/2,0,0)
     SpG%mat2std     =" "     ! To standard space group (Parent)
     SpG%mat2std_shu =" "     ! To standard Shubnikov space group
     SpG%matfrom     =" "     ! From standard space group

     if(d == 4) then
       call Allocate_SpaceGroup(d, 1, SpG)
       if(mag) then
         SpG%spg_symb="P1.1"
       else
         SpG%spg_symb="P 1"
       end if
       SpG%generators_list="x,y,z,+1"
       SpG%Symb_Op(1)="x,y,z,+1"
       return
     end if

     Select Type(SpG)
       Type is(SuperSpaceGroup_Type)
         Select Case(d)
           case(5)
               call Allocate_SpaceGroup(d, 1, SpG)
               SpG%SSG_symb="P1.1(a,b,g)0"
               SpG%generators_list="x,y,z,t,+1"
               SpG%Symb_Op(1)="x,y,z,t,+1"
           case(6)
               call Allocate_SpaceGroup(d, 1, SpG)
               SpG%SSG_symb="P1.1(a1,b1,g1)0(a2,b2,g2)0"
               SpG%generators_list="x,y,z,t,u,+1"
               SpG%Symb_Op(1)="x,y,z,t,u,+1"
           case(7)
               call Allocate_SpaceGroup(d, 1, SpG)
               SpG%SSG_symb="P1.1(a1,b1,g1)0(a2,b2,g2)0(a3,b3,g3)0"
               SpG%generators_list="x,y,z,t,u,v,+1"
               SpG%Symb_Op(1)="x,y,z,t,u,v,+1"
           case(8)
               call Allocate_SpaceGroup(d, 1, SpG)
               SpG%SSG_symb="P1.1(a1,b1,g1)0(a2,b2,g2)0(a3,b3,g3)0(a4,b4,g4)0"
               SpG%generators_list="x,y,z,t,u,v,w,+1"
               SpG%Symb_Op(1)="x,y,z,t,u,v,w,+1"
           case(9)
               call Allocate_SpaceGroup(d, 1, SpG)
               SpG%SSG_symb="P1.1(a1,b1,g1)0(a2,b2,g2)0(a3,b3,g3)0(a4,b4,g4)0(a5,b5,g5)0"
               SpG%generators_list="x,y,z,t,u,v,w,p,+1"
               SpG%Symb_Op(1)="x,y,z,t,u,v,w,p,+1"
           case(10)
               call Allocate_SpaceGroup(d, 1, SpG)
               SpG%SSG_symb="P1.1(a1,b1,g1)0(a2,b2,g2)0(a3,b3,g3)0(a4,b4,g4)0(a5,b5,g5)0(a6,b6,g6)0"
               SpG%generators_list="x,y,z,t,u,v,w,p,q,+1"
               SpG%Symb_Op(1)="x,y,z,t,u,v,w,p,q,+1"
           case default
               Err_CFML%Ierr = 1
               Err_CFML%flag = .true.
               Err_CFML%Msg = "The maximum dimension of a superspace group is 9!"
         End Select
     End Select

   End Subroutine P1_Constructor

   !!----
   !!---- SPACEG_CONSTRUCTOR_GENV
   !!----
   !!---- 20/04/19
   !!
   Module Subroutine SpaceG_Constructor_GenV(GenV, Spg, StrCode, set_inv)
      !---- Arguments ----!
      character(len=*),dimension(:),intent(in)     :: GenV
      class(Spg_Type),              intent(in out) :: Spg
      character(len=*),optional,    intent(in)     :: StrCode
      logical,         optional,    intent(in)     :: set_inv

      !--- Local variables ---!
      character(len=80),    dimension(:),  allocatable :: gen
      type(Symm_Oper_Type), dimension(:),  allocatable :: Op
      type(rational),       dimension(:),  allocatable :: centre_coord,anticentre_coord
      type(rational),       dimension(:,:),allocatable :: Lat_tr, aLat_tr
      type(rational),       dimension(:,:),allocatable :: Mat

      integer :: d,i,ngen,invt,multip,centred,anticentred,Numops,num_lat,num_alat,mag_type
      logical :: magnetic

      !> Init
      call Clear_Error()

      !> Initializes Grp
      d=Get_Dimension_SymmOp(genV(1))
      i=index(genV(1),",",back=.true.)
      magnetic=.false.
      if(trim(genV(1)(i+1:)) == "-1" .or. trim(genV(1)(i+1:)) == "1" .or. trim(genV(1)(i+1:)) == "+1") then
          magnetic=.true.
      end if

      !> This routine delete the generator: x,y,z
      call Check_Gener(GenV,gen,ngen)
      if (Err_CFML%Ierr /= 0) return
      if(ngen == 0) then !If there is only the identity, construct P1-like group
        call P1_Constructor(d,magnetic,SpG)
        return
      end if

      call Init_SpaceGroup(Spg)

      !> check
      ngen=0
      if (allocated(gen)) then
         d=Get_Dimension_SymmOp(gen(1))
         ngen = size(gen)
      end if

      Spg%generators_list="  "
      do i=1,ngen
         Spg%generators_list=trim(Spg%generators_list)//trim(gen(i))//";"
      end do
      Spg%generators_list=Spg%generators_list(1:len_trim(Spg%generators_list)-1)

      allocate(Op(maxnum_op))
      do i=1,maxnum_op
         call allocate_op(d,Op(i))
      end do

      !> Construct the list of the generators on top of Op.
      !> The identity is always the first operator
      allocate(Mat(d,d))
      if(CFML_DEBUG) write(*,"(a)") " Subroutine SpaceG_Constructor_GenV"
      do i=1,ngen
         if(CFML_DEBUG) write(*,"(i3,a)") i," -> "//trim(gen(i))
         call Get_Mat_From_Symb_Int(gen(i),Mat,invt)
         if (Err_CFML%Ierr /= 0) return
         Op(i+1)%Mat=Mat
         Op(i+1)%time_inv=invt
         if(CFML_DEBUG) write(*,"(i3,2a)") i," -> ", String_from_Op(Op(i+1))
      end do
      ngen=ngen+1

      !> Construct the raw Group
      call Get_OPS_from_Generators(ngen,Op,multip)
      if (Err_CFML%Ierr /= 0) return
      !if(CFML_DEBUG) write(*,"(a,i3)")" Multiplicity -> ",multip


      ! Allocate provisionally to Multip the lattice translations and anti-Translations
      allocate(Lat_tr(d-1,multip), aLat_tr(d-1,multip))
      allocate(centre_coord(d-1),anticentre_coord(d-1))

      call Reorder_Operators(multip, Op, centred, centre_coord, anticentred, anticentre_coord, &
                             Numops, num_lat, num_alat, Lat_tr, aLat_tr, mag_type)
      if (Err_CFML%Ierr /= 0) return

      Spg%multip=multip
      Spg%d=d
      if (allocated(Spg%Op)) deallocate(Spg%Op)
      call Allocate_Operators(d,multip,Spg%Op)
      Spg%Op(1:multip)=Op(1:multip)

      if (allocated(Spg%Symb_Op)) Deallocate(Spg%Symb_Op)
      allocate(Spg%Symb_Op(multip))
      do i=1,multip
         Spg%Symb_Op(i)=trim(Get_Symb_from_Op(Op(i)))
      end do

      if(present(set_inv)) then
        if(set_inv) Spg%inv = Get_Inv_OP(Op) !Construct the pointer of each operator to its inverse
      end if

      if (num_lat > 0) then
         if (allocated(Spg%Lat_tr)) Deallocate(Spg%Lat_tr)
         allocate(Spg%Lat_tr(1:d-1,1:Num_Lat))
      end if

      if (num_alat > 0) then
         if (allocated(Spg%aLat_tr)) Deallocate(Spg%aLat_tr)
         allocate(Spg%aLat_tr(1:d-1,1:Num_aLat))
      end if

      if (allocated(Spg%centre_coord)) Deallocate(Spg%centre_coord)
      if (allocated(Spg%anticentre_coord)) Deallocate(Spg%anticentre_coord)
      allocate(Spg%centre_coord(1:d-1))
      allocate(Spg%anticentre_coord(1:d-1))
      Spg%Numops           = Numops
      Spg%centred          = centred
      Spg%anticentred      = anticentred
      Spg%mag_type         = mag_type
      Spg%num_lat          = num_lat
      Spg%num_alat         = num_alat
      Spg%centre_coord     = centre_coord
      Spg%anticentre_coord = anticentre_coord
      if (num_lat  > 0)  Spg%Lat_tr = Lat_tr(1:d-1,1:Num_Lat)
      if (num_alat > 0)  Spg%aLat_tr=aLat_tr(1:d-1,1:Num_aLat)

      if (present(StrCode)) then
         if (trim(StrCode) /= 'xyz') then
            do i=1,Spg%Multip
               Spg%Symb_Op(i)=Get_Symb_from_Op(Spg%Op(i),StrCode)
            end do
         end if
      end if
   End Subroutine SpaceG_Constructor_GenV

End SubModule gs_Spg_Const_VGen