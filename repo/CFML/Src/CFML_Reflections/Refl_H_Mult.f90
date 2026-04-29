!!----
!!----
!!----
SubModule (CFML_Reflections) Refl_H_Mult
   implicit none
   Contains

   !!----
   !!---- H_MULT
   !!----    Calculate the multiplicity of the reflection
   !!----
   !!---- 20/06/2019
   !!
   Module Function H_Mult(H, SpG, Friedel) Result(N)
      !---- Arguments ----!
      integer, dimension(:),  intent (in)  :: H
      class(SpG_Type),        intent (in)  :: SpG
      Logical,                intent (in)  :: Friedel
      integer                              :: N

      !---- Local Variables ----!
      logical                                      :: esta
      integer, dimension(size(h))                  :: k
      integer, dimension(size(h),size(h))          :: Mat
      integer                                      :: i,j,ng,Dd
      integer, dimension(size(h),SpG%numops)       :: klist

      !> Init
      N=0

      n=1
      Dd=size(h)
      ng=SpG%numops
      if (ng > 1) then
         klist(:,1)=h

         do i=2,ng
            Mat=SpG%Op(i)%Mat(1:Dd,1:Dd)
            k=matmul(h,Mat)
            esta=.false.

            do j=1,n
               if (h_equal(k,klist(:,j)) .or. h_equal(-k,klist(:,j))) then
                  esta=.true.
                  exit
               end if
            end do
            if (esta) cycle

            n=n+1
            klist(:,n) = k
         end do
      end if
      if (Friedel .or. SpG%centred == 2) then
         n=n*2
      end if
   End Function H_Mult

End SubModule Refl_H_Mult