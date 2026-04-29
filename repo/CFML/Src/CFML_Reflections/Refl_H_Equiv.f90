!!----
!!----
!!----
SubModule (CFML_Reflections) Refl_H_Equiv
   implicit none
   Contains
   !!----
   !!---- H_EQUIV
   !!----     .True. if the reflection is equivalente by symmetry
   !!----
   !!---- 20/06/2019
   !!
   Module Function H_Equiv(H, K, SpG, Friedel) Result (Info)
      !---- Arguments ----!
      integer, dimension(:),        intent(in)  :: H
      integer, dimension(:),        intent(in)  :: K
      class(Spg_Type),              intent(in)  :: SpG
      logical, optional,            intent(in)  :: Friedel
      logical                                   :: info

      !---- Local Variables ----!
      integer                                      :: i, nops,Dd
      integer, dimension(size(h))                  :: hh
      Integer, dimension(size(h),size(h))          :: Mat

      !> Init
      info=.false.

      nops= SpG%NumOps
      if (SpG%centred /= 1) nops=min(nops*2,SpG%Multip)

      Dd=size(h)

      do i=1,nops
         Mat=SpG%Op(i)%Mat(1:Dd,1:Dd)
         hh = matmul(h,Mat)
         if (H_equal(k,hh)) then
            info=.true.
            exit
         end if

         if (present(Friedel)) then
            if (Friedel) then
               if (h_equal(k,-hh)) then
                  info=.true.
                  exit
               end if
            end if
         end if
      end do
   End Function H_Equiv

End SubModule Refl_H_Equiv