!!----
!!----
!!----
SubModule (CFML_gSpaceGroups) gS_is_LattCentring
   implicit none
   Contains

   !!----
   !!---- IS_LATTICE_VEC
   !!----
   !!---- 19/04/19
   !!
   Module Function Is_Lattice_Vec_rat(V,Ltr) Result(Lattice)
      !---- Argument ----!
      type(rational), dimension(:),   intent( in) :: v
      type(rational), dimension(:,:), intent( in) :: Ltr
      logical                                     :: Lattice

      !---- Local variables ----!
      type(rational), dimension(size(v)) :: vec
      integer                            :: i, NLat,d

      !> Init
      Lattice=.false.
      d=size(v)
      nlat=size(Ltr,dim=2)
      !> if V is an integral vector then V is a lattice vector
      if (Rational_Is_Integer(v)) then
         Lattice=.true.
         return
      end if

      do i=1, nlat
         vec=Ltr(1:d,i)-v
         if (Rational_Is_Integer(vec)) then
            Lattice=.true.
            return
         end if
      end do
   End Function Is_Lattice_Vec_rat

   Module Function is_Lattice_vec_real(v,Ltr) Result(Lattice)
      !---- Arguments ----!
      real(kind=cp), dimension(:),   intent(in) :: v
      type(rational),dimension(:,:), intent(in) :: Ltr
      logical                                   :: Lattice
      !---- Local Variables ----!
      integer :: i,nlat,d
      real(kind=cp), dimension(size(v)) :: vec,vl
      !> Init
      Lattice=.false.
      d=size(v)
      nlat=size(Ltr,dim=2)
       if (Zbelong(v)) then       ! if v is an integral vector =>  v is a lattice vector
          Lattice=.true.
       else                       ! if not look for lattice type
          do i=1,nlat
            vl=Ltr(1:d,i)
            vec=vl-v
            if (Zbelong(vec)) then
              Lattice=.true.
              exit
            end if
          end do
       end if
   End Function is_Lattice_vec_real

   !!----
   !!---- Is_OP_Lattice_Centring
   !!----
   !!---- 19/04/19
   !!
   Module Function Is_OP_Lattice_Centring(Op) Result(Info)
      !---- Arguments ----!
      type(Symm_Oper_Type), intent(in) :: Op
      logical                          :: info

      !---- Local Variables ----!
      integer :: dr

      !> Init
      info=.false.

      if (.not. allocated(identity_matrix)) return
      dr=size(Op%Mat(:,1))-1
      if (Rational_Equal(identity_matrix(1:dr,1:dr),Op%Mat(1:dr,1:dr)) .and. Op%time_inv == 1) info=.true.
   End Function Is_OP_Lattice_Centring

   !!----
   !!---- Is_Vec_Lattice_Centring
   !!----
   !!---- 5/02/2020
   !!
   Pure Module Function Is_Vec_Lattice_Centring(vec,SpG,Prim) Result(Info)
      !---- Arguments ----!
      real(kind=cp), dimension(:), intent(in) :: Vec
      Class(SpG_Type), optional,   intent(in) :: SpG
      logical, optional,           intent(in) :: Prim
      logical                                 :: info

      !---- Local Variables ----!
      integer :: i
      real(kind=cp), dimension(size(vec)) :: Lat

      !> Init
      info=.false.
      if(present(Prim)) then
        if(Zbelong(vec)) info=.true.
        return
      else
        if(Zbelong(vec)) then
          info=.true.
          return
        else
          !Check if the vector corresponds to a lattice centring of the space group
          if(present(SpG)) then
            do i=1,Spg%Num_lat
              Lat=SpG%Lat_tr(:,i)
              Lat=vec-Lat
              if(Zbelong(Lat)) then
                info=.true.
                return
              end if
            end do
          end if
        end if
      end if
   End Function Is_Vec_Lattice_Centring

End SubModule gS_is_LattCentring