!!----
!!----
!!----
SubModule (CFML_Reflections) Refl_H_Absent
   implicit none
   Contains
   !!----
   !!---- H_ABSENT
   !!----   .True. if the reflection is absent for this Space group
   !!----
   !!---- 20/06/2019
   !!
   Module Function H_Absent(H, SpG) Result(Info)
      !---- Arguments ----!
      integer, dimension(:), intent (in) :: H
      class(SpG_Type),       intent (in) :: SpG
      logical                            :: info

      !---- Local Variables ----!
      integer                            :: i, Dd
      integer, dimension(size(h))        :: k
      integer,dimension(size(h),size(h)) :: Mat
      real(kind=cp)                      :: r1,r2
      real(kind=cp),dimension(size(h))   :: tr

      !> Init
      info=.false.

      Dd=size(h)
      do i=1,SpG%Multip
         Mat=SpG%Op(i)%Mat(1:Dd,1:Dd)

         k=matmul(h, Mat)
         if (h_equal(h,k)) then
            tr=SpG%Op(i)%Mat(1:Dd, Dd+1)
            r1=dot_product(tr,real(h,kind=cp))
            r2=nint(r1)
            if (abs(r1-r2) > EPS_REF) then
               info=.true.
               exit
            end if
         end if
      end do
   End Function H_Absent

   !!----
   !!---- MH_ABSENT
   !!----   .True. if the magnetic reflection is absent for this Space group
   !!----
   !!---- 20/06/2019
   !!
   Module Function mH_Absent(H,SpG) Result(Info)
      !---- Arguments ----!
      integer, dimension(:), intent (in) :: H
      class(SpG_Type),       intent (in) :: SpG
      logical                            :: info

      !---- Local Variables ----!
      integer,       dimension(size(h))        :: k
      integer,       dimension(size(h),size(h)):: Mat
      real(kind=cp), dimension(size(h))        :: tr
      integer                                  :: i,n_id,Dd
      real(kind=cp)                            :: r1

      !> Init
      info=.false.
      Dd=size(h)

      select case(SpG%mag_type)
         case(1,3)
            n_id=0
            do i=1,SpG%Multip
               Mat=SpG%Op(i)%Mat(1:Dd,1:Dd)
               tr=SpG%Op(i)%Mat(1:Dd,Dd+1)
               k =matmul(h, Mat)
               if (h_equal(h,k)) then
                  r1=cos(TPI*dot_product(Tr,real(h)))
                  n_id=n_id + Trace(Mat*nint(r1))
               end if
            end do
            if (n_id == 0) info=.true.

         case(2)
            info=.true.

         case(4)
            info=.true.
            do i=1,SpG%Num_aLat
               tr=SpG%aLat_tr(:,i)
               r1=cos(TPI*dot_product(tr,real(h)))
               if (nint(r1) == -1) info=.false.
            end do
      end Select
   End Function mH_Absent

   !!----
   !!---- H_LAT_ABSENT
   !!----  .True. if reflection is absent for Lattice conditions
   !!----
   !!---- 20/06/2019
   !!
   Module Function H_Latt_Absent(H, Latt, n) Result(info)
      !---- Arguments ----!
      integer,        dimension(:),  intent (in) :: h
      type(rational), dimension(:,:),intent (in) :: Latt
      integer,                       intent (in) :: n
      logical                                    :: info

      !---- Local Variables ----!
      integer                          :: k,i
      real(kind=cp),dimension(size(h)) :: Lat
      real(kind=cp)                    :: r1,r2

      !> Init
      info=.false.

      do i=1,n
         Lat=Latt(:,i)
         r1=dot_product(Lat,real(h,kind=cp))
         r2=nint(r1)
         k=nint(2.0_cp*r1)
         if (mod(k,2) /= 0) info=.true.
         exit
      end do
   End Function H_Latt_Absent

   Module Function Get_h_info(h,SpG,mag) Result(info)
      integer, dimension(:), intent (in) :: h
      class(SpG_Type),       intent (in) :: SpG
      logical,               intent (in) :: mag
      integer, dimension(4)              :: info

      info=0  ! Reflection allowed (lattice, Nuclear, Magnetic, Ref-character)
              ! For the three initial componentes 0:allowed, 1:absent
              ! Ref-character Info(4): 0:pure nuclear, 1:pure magnetic, 2:nuclear+magnetic

      if(SpG%Num_Lat > 0) then
        if (H_Latt_Absent(h,SpG%Lat_tr,SpG%Num_Lat)) then
          info(1:3)=1  ! lattice absence and, as a consequence, nuclear and magnetic absence
          info(4)=2
          return
        end if
      end if
      if(H_Absent(h,SpG)) then
        info(2)=1  ! Nuclear absence
        info(4)=1  ! This implies pure magnetic is mH false
        if(SpG%Mag_type /= 2 .and. mag) then
          if(mH_Absent(h,SpG)) then
            info(3)=1 ! Magnetic absence
          else
            info(4)=1   ! Pure magnetic
          end if
        end if
      else
        info(2)=0
        info(4)=2
        if(SpG%Mag_type /= 2) then
          if(mH_Absent(h,SpG)) then
            info(4)=0  ! pure nuclear
          else
            info(4)=2
          end if
        end if
      end if
   End Function Get_h_info

End SubModule Refl_H_Absent