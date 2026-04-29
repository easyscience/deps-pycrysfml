!!----
!!----
!!----
!!
SubModule (CFML_gSpaceGroups) gs_Spg_Const_Str
   implicit none
   Contains

   !!----
   !!---- SPACEG_CONSTRUCTOR_STR
   !!----
   !!---- 20/04/19
   !!
   Module Subroutine SpaceG_Constructor_Str(ListGen, Spg, Strcode, set_inv)
      !---- Arguments ----!
      character(len=*),             intent(in)     :: ListGen
      class(Spg_Type),              intent(in out) :: Spg
      character(len=*),   optional, intent(in)     :: Strcode
      logical,            optional, intent(in)     :: set_inv

      !--- Local variables ---!
      character(len=80),    dimension(:),  allocatable :: gen1, gen
      type(Symm_Oper_Type), dimension(:),  allocatable :: Op
      type(rational),       dimension(:),  allocatable :: centre_coord,anticentre_coord
      type(rational),       dimension(:,:),allocatable :: Lat_tr, aLat_tr
      type(rational),       dimension(:,:),allocatable :: Mat
      integer :: d,i,j,ngen,invt,multip,centred,anticentred,Numops,num_lat,num_alat,mag_type
      logical :: time_given, magnetic
      class(Spg_Type), allocatable :: Spg_temp

      !> Init
      call Clear_Error()
      allocate(gen1(maxnum_op))
      call Get_Generators(ListGen, d, gen, ngen,time_given)
      if (Err_CFML%Ierr /= 0) return
      i=index(ListGen,",",back=.true.)
      magnetic=.false.
      if(trim(ListGen(i+1:)) == "-1" .or. trim(ListGen(i+1:)) == "1" .or. trim(ListGen(i+1:)) == "+1") then
          magnetic=.true.
      end if
      if(ngen == 0) then !Only the identity has been provided, construct then P1-like group
        call P1_Constructor(d,magnetic,SpG)
        return
      end if
      call Set_Identity_Matrix(d)
      allocate(Mat(d,d))
      !Remove the identity from the list of generators
      j=0
      do i=1,ngen
         call Get_Mat_From_Symb_Int(gen(i), Mat, invt)
         if(Err_CFML%Ierr /= 0) return
         if(rational_equal(Mat, Identity_Matrix) .and. invt == 1) cycle
         j=j+1
         gen(j) = gen(i)
      end do
      ngen=j
      if(CFML_Debug) then
        write(*,"(a)") " Obtained generators from the string list provided:"
        do i=1,ngen
            write(*,"(i4,2a)") i," -> ", gen(i)
        end do
      end if
      call Init_SpaceGroup(Spg)
      do i=1,ngen
         Spg%generators_list=trim(Spg%generators_list)//trim(gen(i))//";"
      end do
      Spg%generators_list=Spg%generators_list(1:len_trim(Spg%generators_list)-1)

      allocate(Op(maxnum_op))
      do i=1,maxnum_op
         call Allocate_Op(d,Op(i))
      end do

      !> Construct the list of the generators on top of Op.
      !> The identity is always the first operator
      do i=1,ngen
         call Get_Mat_From_Symb_Int(gen(i), Mat, invt)
         Op(i+1)%Mat=Mat
         Op(i+1)%time_inv=invt
      end do
      ngen=ngen+1

      !> Construct the raw Group
      call Get_OPS_from_Generators(ngen,Op,multip)
      if(Err_CFML%Ierr /= 0) return

      !> Allocate provisionally to Multip the lattice translations
      !> and anti-Translations
      allocate(Lat_tr(d-1,multip), aLat_tr(d-1,multip))
      allocate(centre_coord(d-1),anticentre_coord(d-1))

      call Reorder_Operators(multip, Op, centred, centre_coord, anticentred, anticentre_coord, &
                             Numops, num_lat, num_alat, Lat_tr, aLat_tr, mag_type)
      if(Err_CFML%Ierr /= 0) return

      Spg%multip=multip
      Spg%d=d
      if(allocated(Spg%Op)) deallocate(Spg%Op)
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
      if (num_lat  > 0) Spg%Lat_tr = Lat_tr(1:d-1,1:Num_Lat)
      if (num_alat > 0) Spg%aLat_tr=aLat_tr(1:d-1,1:Num_aLat)

      if (present(Strcode)) then
         if (trim(Strcode) /= 'xyz') then
            do i=1,Spg%Multip
               Spg%Symb_Op(i)=Get_Symb_from_Op(Spg%Op(i),Strcode)
            end do
         end if
      end if
      if(.not. time_given) Spg%magnetic=.false.

   End Subroutine SpaceG_Constructor_Str

End SubModule gs_Spg_Const_Str

