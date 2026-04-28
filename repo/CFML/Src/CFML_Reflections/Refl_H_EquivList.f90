!!----
!!----
!!----
SubModule (CFML_Reflections) Refl_H_EquivList
   implicit none
   Contains
   !!----
   !!>   Subroutine H_Equiv_List(H, SpG, Friedel, Mult, H_List,ipos)
   !!      integer, dimension(:),    intent (in)  :: H
   !!      class(SpG_Type),          intent (in)  :: SpG
   !!      Logical,                  intent (in)  :: Friedel
   !!      integer,                  intent (out) :: mult
   !!      integer, dimension(:,:),  intent (out) :: h_list
   !!      integer, optional,        intent (out) :: ipos
   !!
   !!   Calculates the multiplicity of the reflection H and the list of all
   !!   equivalent reflections. Friedel law assumed if Friedel=.true.
   !!   If present, ipos contains the number in the list of a reflection
   !!   with the maximum number of positive indices
   !!----
   !!---- 24/06/2019
   !!
   Module Subroutine H_Equiv_List(H, SpG, Friedel, Mult, H_List,ipos)
      !---- Arguments ----!
      integer, dimension(:),    intent (in)  :: H
      class(SpG_Type),          intent (in)  :: SpG
      Logical,                  intent (in)  :: Friedel
      integer,                  intent (out) :: mult
      integer, dimension(:,:),  intent (out) :: h_list
      integer, optional,        intent (out) :: ipos

      !---- Local Variables ----!
      integer, dimension(size(h))       :: k
      integer                           :: i,j,ng,mp,D
      logical                           :: esta
      integer,dimension(size(h),size(h)):: Op

      !> Init
      D=size(h)
      h_list = 0
      mult=1
      h_list(1:D,1)=h(:)
      if(present(ipos)) then
        mp=count(h(1:3) > 0)
        ipos=1
      end if
      ng=SpG%multip
      do i=2,ng
         Op=SpG%Op(i)%Mat(1:D,1:D)
         k = matmul(h,Op)

         esta=.false.
         do j=1,mult
            if (h_equal(k,h_list(1:D,j)) .or. (h_equal(-k,h_list(1:D,j)) .and. Friedel)) then
               esta=.true.
               exit
            end if
         end do
         if (esta) cycle
         mult=mult+1
         h_list(1:D,mult) = k
         if(present(ipos)) then
           j=count(h_list(1:3,i) > 0)
           if( j > mp) then
             mp=j
             ipos=mult
           end if
         end if
      end do

      if (Friedel .and. SpG%centred == 1) then
         j=mult
         mult=mult*2
         do i=j+1,mult
            h_list(:,i)=-h_list(:,i-j)
         end do
      end if

   End Subroutine H_Equiv_List

End SubModule Refl_H_EquivList