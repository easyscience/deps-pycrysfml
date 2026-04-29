!!----
!!----
!!----
SubModule (CFML_EoS) EoS_PTVTable
   implicit none
   Contains

   !!--++
   !!--++ FUNCTION MURN_INTERPOLATE_PTVTABLE
   !!--++
   !!--++ Returns ...
   !!--++
   !!--++ Date: 25/012/2016 RJA
   !!
   Module Function Murn_Interpolate_PTV_Table(I, J, P, Eos) Result(V)
      !---- Arguments ----!
      integer,        intent(in) :: I      ! pointers to table point just beyond P,T required
      integer,        intent(in) :: J      ! pointers to table point just beyond P,T required
      real(kind=cp),  intent(in) :: P      ! pressure required
      type(Eos_Type), intent(in) :: EoS    ! Eos Parameter
      real(kind=cp)              :: V

      !---- Local Variables ----!
      integer         :: is
      real(kind=cp)   :: k12,K23,KP,V0,K0

      !> Set up pointer
      is=i-2
      if (is < 1) is=1

      !> Estimate K and Kp
      k12=-0.5_cp*(eos%table%ptv(is+1,j,3) + eos%table%ptv(is,j,3))*(eos%table%ptv(is+1,j,1) - &
           eos%table%ptv(is,j,1))/(eos%table%ptv(is+1,j,3)-eos%table%ptv(is,j,3))
      k23=-0.5_cp*(eos%table%ptv(is+2,j,3) + eos%table%ptv(is+1,j,3))*(eos%table%ptv(is+2,j,1) - &
           eos%table%ptv(is+1,j,1))/(eos%table%ptv(is+2,j,3)-eos%table%ptv(is+1,j,3))

      kp=2.0_cp*(k23-k12)/(eos%table%ptv(is+2,j,1)-eos%table%ptv(is,j,1))         ! Kp at point is+1
      if (abs(kp) < 0.0001) kp=4.0_cp
      k0=k12+kp*(eos%table%ptv(is+1,j,1)-eos%table%ptv(is+1,j,1))/2.0_cp          ! K at point is+1
      v0=eos%table%ptv(is+1,j,3)                                                  ! V0 at point is+1

      !> Value
      v=v0*(1.0_cp+kp/k0*(p-eos%table%ptv(is+1,j,1)))**(-1.0_cp/kp)

   End Function Murn_Interpolate_PTV_Table

   !!--++
   !!--++ FUNCTION MURN_PTV_TABLE
   !!--++
   !!--++ Calculate ....
   !!--++
   !!--++ Date: 17/03/2017
   !!
   Module Function Murn_PTV_Table(I, J, Eos) Result(Eosm)
      !---- Arguments ----!
      integer,        intent(in)  :: i      ! pointers to table point
      integer,        intent(in)  :: j      ! pointers to table point
      type(Eos_Type), intent(in)  :: EoS    ! Eos Parameter in (with ptvtable)
      type(Eos_Type)              :: EoSm   ! Eos Parameter of Murn output for point i,j

      !---- Local Variables ----!
      integer         :: im                   ! normally i but if edge point it will be shifted
      real(kind=cp)   :: k2,k4

      !> Set up Murn in Eosm
      call Init_EoS_Type(Eosm)

      eosm%imodel=1
      call set_eos_names(eosm)      ! sets names for params
      eosm%title='Murnaghan for interpolation'

      !> Set up pointer
      im=i
      if (i < 3) im=3
      if (i > eos%table%np-2) im=eos%table%np-2

      !>Vo
      eosm%params(1)=eos%table%ptv(i,j,3)

      !>Estimate K0 at point i,j
      eosm%params(2)=-1.0_cp*eos%table%ptv(im,j,3)*(eos%table%ptv(im+1,j,1)-eos%table%ptv(im-1,j,1))/ &
                     (eos%table%ptv(im+1,j,3)-eos%table%ptv(im-1,j,3))

      !> Kp
      k2=-1.0_cp*eos%table%ptv(im-1,j,3)*(eos%table%ptv(im,j,1)-eos%table%ptv(im-2,j,1))/ &
         (eos%table%ptv(im,j,3)-eos%table%ptv(im-2,j,3))

      k4=-1.0_cp*eos%table%ptv(im+1,j,3)*(eos%table%ptv(im+2,j,1)-eos%table%ptv(im,j,1))/ &
         (eos%table%ptv(im+2,j,3)-eos%table%ptv(im,j,3))

      eosm%params(3)=(k4-k2)/(eos%table%ptv(im+1,j,1)-eos%table%ptv(im-1,j,1))       ! Kp at point im

      if (abs(eosm%params(3)) < 0.0001) eosm%params(3)=4.0_cp

      !> adjust K now if im /= i; Kp should be constant for a Murn
      if (i /= im) eosm%params(2)=eosm%params(2)+eosm%params(3)*(eos%table%ptv(i,j,1)-eos%table%ptv(im,j,1))

   End Function Murn_PTV_Table

End SubModule EoS_PTVTable