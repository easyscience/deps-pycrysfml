Submodule (CFML_Reflections) Refl_H_Convent
   !---- Variables ----!
   implicit none

 Contains
   !!--++
   !!--++ SUBROUTINE  HKL_UNI
   !!--++    Calculate unique reflections between two values (value1,value2)
   !!--++    of sin_theta/lambda. If no_order is present and .true. the sort subroutine
   !!--++    is not invoked.
   !!--++
   !!--++ Update: April - 2022
   !!
   Module Subroutine H_Uni(Cell, Spg, Friedel, Vmin, Vmax, Code, MaxRef, Reflex, &
                           no_order, check_ok, hlim, Ref_typ)
      !---- Arguments ----!
      type (Cell_G_Type),               intent(in)     :: cell
      class (SpG_type) ,                intent(in)     :: Spg
      logical,                          intent(in)     :: Friedel
      real(kind=cp),                    intent(in)     :: vmin,vmax ! Range in sintheta/Lambda
      character(len=1),                 intent(in)     :: code      ! If code="r", d-spacing are input
      integer,                          intent(in)     :: MaxRef    ! Maximum Number of reflections to be generated
      class (RefList_Type),             intent(out)    :: reflex
      logical,                optional, intent(in)     :: no_order
      logical,                optional, intent(out)    :: check_ok
      integer, dimension(3,2),optional, intent(in)     :: hlim
      character(len=*),       optional, intent(in)     :: Ref_typ   !This is compulsory if structure factors have to be calculated afterwards

      !---- Local variables ----!
      real(kind=cp)                       :: v1,v2,sval
      integer                             :: h,k,l,hmin,kmin,lmin,hmax,kmax,lmax, i, num_ref
      integer, dimension(Spg%d-1)         :: hh,kk,nulo
      integer,  dimension(  MaxRef)       :: ind
      integer,  dimension(  MaxRef)       :: mul
      integer,  dimension(Spg%d-1,MaxRef) :: hkl
      real(kind=cp),dimension(MaxRef)     :: sv
      character(len=2)                    :: inf
      character(len=5)                    :: tipo

      !> Init
      nulo=0
      v1=min(vmin,vmax)
      v2=max(vmin,vmax)
      if (code =="r" .or. code=="R") then
         v1=1.0/(2.0*max(vmin,vmax))
         v2=1.0/(2.0*min(vmin,vmax))
      end if

      if (present(check_ok)) check_ok=.true.
      hmax=nint(Cell%cell(1)*2.0*v2+1.0)
      kmax=nint(Cell%cell(2)*2.0*v2+1.0)
      lmax=nint(Cell%cell(3)*2.0*v2+1.0)
      lmin= 0  !l positive or zero except for -3 1 m (see below)

      !> Select approximate region to generate reflections depending
      !> on the space group. This allows a faster generation.
      select case(SpG%NumSpg)
         case (1:2)                 ! -1    -> hkl: l >=0; hk0: h >=0; 0k0: k >=0
            hmin=-hmax
            kmin=-kmax
            if(SpG%NumSpg == 1 .and. .not. Friedel) lmin=-lmax

         case (3:15)                ! 2/m
            inf='b'
            if (abs(cell%ang(2) -90.0) > 0.01) then
               inf='b'
            else if (abs(cell%ang(3) -90.0) > 0.01) then
               inf='c'
            else if (abs(cell%ang(1)-90.0) > 0.01) then
               inf='a'
            end if
            select case (inf(1:1))
               case ("b")     !       -> hkl: k >=0, l >=0; hk0: h >=0
                  hmin=-hmax
                  kmin=0
               case ("c")     !       -> hkl: k >=0, l >=0; h0l: h >=0
                  hmin=-hmax
                  kmin=0
               case ("a")     !       -> hkl: h >=0, l >=0; 0kl: l >=0  Provisional (to be tested)
                  kmin=-kmax
                  hmin=0
            end select

         case (16:74)         ! mmm   -> hkl: h >=0, k >=0, l >=0
            hmin=0
            kmin=0

         case (75:88)         ! 4/m   -> hkl: h >=0, l >=0, k >=0 if h = 0
                              !                             k > 0 if h > 0
            hmin=0
            kmin=0

         case (89:142)        ! 4/mmm -> hkl: h >=0, k>=0, l>=0, h >=k
            hmin=0
            kmin=0

         case (143:148)       ! -3    -> hkl: h+k>0, l>0 ;  hk0: h>0, k>=0
            hmin=0
            kmin=-kmax

         case (149,151,153,157,159,162,163) ! -3 1 m  -> hkl: h>=0,h>=k>0 ; h0l: h>=0,l>=0
            hmin=0
            kmin=0
            lmin=-lmax

         case (150,152,154,155,156,158,160,161,164,165,166,167)
                             ! -3 m   -> hkl: h>=0 h>=k ; hhl: h>=0,l>=0
            hmin=0
            kmin=0

         case (168:176)    ! 6/m   -> hkl: h>0,k>0,l>=0;  0kl k>=0,l>=0
            hmin=0
            kmin=0

         case (177:194)    ! 6/mmm -> hkl: h >=0, k >=0, l >=0, h >=k
            hmin=0
            kmin=0

         case (195:206)    ! m-3   -> hkl: h > l, k > l, l >=0 ; hkk: k>=0 h>=k
            hmin=0
            kmin=0

         case (207:230)    ! m-3m  -> hkl: h >=0, k >=0, l >=0, h >=k, k >=l
            hmin=0
            kmin=0

         case default      ! Assumed -1
            hmin=-hmax
            kmin=-kmax
      end select

      if (present(hlim)) then
         if (hmin < hlim(1,1)) hmin=hlim(1,1)
         if (hmax > hlim(1,2)) hmax=hlim(1,2)
         if (kmin < hlim(2,1)) kmin=hlim(2,1)
         if (kmax > hlim(2,2)) kmax=hlim(2,2)
         if (lmin < hlim(3,1)) lmin=hlim(3,1)
         if (lmax > hlim(3,2)) lmax=hlim(3,2)
      end if

      num_ref=0
      hkl=0
      !if(CFML_DEBUG) then
      !  write(*,"(a,2f10.4)")  "     v1, v2: ",v1, v2
      !  write(*,"(a,i4)")      " SpG%NumSpg: ",SpG%NumSpg
      !  write(*,"(a,2i4)")     " hmin, hmax: ",hmin, hmax
      !  write(*,"(a,2i4)")     " kmin, kmax: ",kmin, kmax
      !  write(*,"(a,2i4)")     " lmin, lmax: ",lmin, lmax
      !end if
      ext_do: do h=hmin,hmax
         do k=kmin,kmax
            do l=lmin,lmax

               hh(1)=h
               hh(2)=k
               hh(3)=l

               if (h_equal(hh,nulo)) cycle
               sval=h_s(hh,cell)
               if (sval > v2 .or. sval < v1) cycle
               if (h_absent(hh,Spg)) cycle

               kk=asu_h(hh,Spg)
               if (h_equal(kk,nulo)) cycle
               if (h_equal(kk,-hh) .and. Friedel) cycle

               num_ref=num_ref+1
               if(num_ref > maxref) then
                  num_ref=maxref
                  if (present(check_ok)) check_ok=.false.
                  exit ext_do
               end if
               hkl(:,num_ref)=kk
               mul(num_ref)  =h_mult(kk, Spg, Friedel)
               sv(num_ref)   = sval
            end do
         end do
      end do ext_do

      if (present(no_order)) then
         if (no_order) then
            ind=[(i,i=1,num_ref)]
         else
            ind = sort(sv,num_ref)
         end if

      else
         ind(1:num_ref) = sort(sv,num_ref)
      end if

      if(present(Ref_Typ)) then

         tipo = Ref_Typ

      else

         select type (r => Reflex%ref)
            type is (refl_type)
               tipo = 'refl'

            type is (srefl_type)
               tipo = 'srefl'

            type is (mrefl_type)
               tipo = 'mrefl'

         end select

      end if

      call Initialize_Reflist(Num_Ref, Reflex, tipo, Spg%d-1)

      !if(CFML_DEBUG) then
      !  write(*,"(a,i4)")      "    Num_Ref: ",Num_Ref
      !  write(*,"(a)")         "  Type-Refl: "//trim(tipo)
      !end if

      do i=1,num_ref
         reflex%Ref(i)%h    = hkl(:,ind(i))
         reflex%Ref(i)%hr   = real(hkl(:,ind(i)),kind=cp)
         reflex%Ref(i)%mult = mul(ind(i))
         reflex%Ref(i)%S    = sv(ind(i))
      end do

   End Subroutine H_Uni

   !!----
   !!---- Module Subroutine Hkl_Gen_Sxtal(Crystalcell,Spacegroup,stlmin,stlmax,Reflex,ord,hlim)
   !!----    Type (Cell_G_Type),                intent(in) :: CrystalCell     !Unit cell object
   !!----    Type (SPG_Type) ,                  intent(in) :: SpaceGroup      !Space Group object
   !!----    real(kind=cp),                     intent(in) :: stlmin,stlmax   !Minimum and Maximum SinTheta/Lambda
   !!----    class(ReflList_Type),              intent(out):: reflex          !Generated set of reflections
   !!----    Integer, dimension(3),   optional, intent(in) :: ord             !Order for loop of hkl-indices
   !!----    Integer, dimension(3,2), optional, intent(in) :: hlim            !hkl-limits
   !!----
   !!----
   !!----    Calculate all allowed reflections between a minimum and a maximum value of sin_theta/lambda.
   !!----    If the limits of indices are provided in hlim, only the reflections verifying the prescription
   !!----    are finally kept. hlim(:,1) and hlim(:,2) contain the minimum and maximum values respectively.
   !!----    The reflections are stored in the scalar object Reflex of type: Reflection_List_Type
   !!----    The output is not ordered but the user can obtain the reflections generated
   !!----    in a particular way by providing the integer vector "ord", containing a permutation
   !!----    of the three numbers 1,2,3. By default the loop generating the hkl-indices uses
   !!----    the vector ord=[3,2,1], this means that the inner loop (more rapidly changing index)
   !!----    is the l-index, then the k-index and finally the h-index.
   !!----
   !!---- Update: May - 2022
   !!
   Module Subroutine Hkl_Gen_Sxtal(Crystalcell,Spacegroup,stlmin,stlmax,Reflex,ord,hlim)
      !---- Arguments ----!
      Type (Cell_G_Type),                intent(in)  :: crystalcell
      type (SPG_Type),                   intent(in)  :: spacegroup
      real(kind=cp),                     intent(in)  :: stlmin,stlmax
      class(RefList_Type),               intent(out) :: reflex
      Integer, dimension(3),   optional, intent(in)  :: ord
      Integer, dimension(3,2), optional, intent(in)  :: hlim

      !---- Local variables ----!
      real(kind=cp)         :: sval
      integer               :: h,k,l,hmax,kmax,lmax, maxref,i, num_ref
      integer, dimension(3) :: hh,nulo,od,imin,imax

      integer,       dimension(:,:), allocatable :: hkl
      real(kind=cp), dimension(:),   allocatable :: sv
      logical :: Lcentred

      Lcentred=.false.
      nulo=0
      if(Spacegroup%num_lat > 0) Lcentred=.true.
      hmax=nint(CrystalCell%cell(1)*2.0*stlmax+1.0)
      kmax=nint(CrystalCell%cell(2)*2.0*stlmax+1.0)
      lmax=nint(CrystalCell%cell(3)*2.0*stlmax+1.0)
      if(present(hlim)) then
        imin=hlim(:,1)
        imax=hlim(:,2)
      else
        imin=[-hmax,-kmax,-lmax]
        imax=[ hmax, kmax, lmax]
      end if
      od=[3,2,1]
      if(present(ord)) od=ord

      maxref=(2*hmax+1)*(2*kmax+1)*(2*lmax+1)
      allocate(hkl(3,maxref),sv(maxref))

      num_ref=0
      ext_do: do h=imin(od(3)),imax(od(3))
         do k=imin(od(2)),imax(od(2))
            do l=imin(od(1)),imax(od(1))
               hh(od(3))=h
               hh(od(2))=k
               hh(od(1))=l
               if (h_equal(hh,nulo)) cycle
               sval=h_s(hh,crystalcell)
               if (sval > stlmax .or. sval < stlmin) cycle
               if (h_absent(hh,Spacegroup)) cycle
               num_ref=num_ref+1
               if(num_ref > maxref) then
                  num_ref=maxref
                  exit ext_do
               end if
               hkl(:,num_ref) = hh
               sv(num_ref)    = sval
            end do
         end do
      end do ext_do

      call Initialize_RefList(Num_ref, reflex, 'SRefl', Spacegroup%d-1)

      associate (r => reflex%ref)
         select type (r)
           class is (Refl_Type)
             do i=1,num_ref
                r(i)%h    = hkl(:,i)
                r(i)%hr   = real(hkl(:,i),kind=cp)
                r(i)%s    = sv(i)
                r(i)%mult = h_mult(hh,Spacegroup,.false.)
                r(i)%imag = 0
                r(i)%Pcoeff=0
             end do
         end select
         select type (r)
           class is (SRefl_Type)
             do i=1,num_ref
                r(i)%Fo    =0.0_cp  ! Observed Structure Factor
                r(i)%sFo   =0.0_cp  ! Sigma of  Fo
                r(i)%Fc    =0.0_cp  ! Calculated Structure Factor
                r(i)%w     =1.0_cp  ! weight factor
                r(i)%phase =0.0_cp  ! Phase in degrees
                r(i)%a     =0.0_cp  ! real part of the Structure Factor
                r(i)%b     =0.0_cp  ! Imaginary part of the Structure Factor
             end do
         end select
      end associate
   End Subroutine Hkl_Gen_Sxtal

End Submodule Refl_H_Convent