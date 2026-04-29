!!----
!!----
!!----
SubModule (CFML_Reflections) Refl_Generate
   implicit none
   Contains
   !!----
   !!
   !!---- Module Function Generate_Reflections(Cell,Slmin,Slmax,Reflex,SpG,iphase,MagExt,kinfo,order,Unique,seqindx,mag_only,Friedel,Ref_typ,kout) Returns
   !!----    !---- Arguments ----!
   !!----    class(Cell_G_Type),                intent(in)     :: Cell     !Unit cell object
   !!----    real(kind=cp),                     intent(in)     :: Slmin    !Minimum SinTheta/Lambda
   !!----    real(kind=cp),                     intent(in)     :: Slmax    !Maximum SinTheta/Lambda
   !!----    type(RefList_Type),                intent(in out) :: Reflex   !Reflection List
   !!----    class(Spg_Type) ,                  intent(in)     :: SpG      !General Space Group
   !!----    integer,                 optional, intent(in)     :: iphase   !Number of the current phase (if absent, a single phase is considered)
   !!----    logical,                 optional, intent(in)     :: MagExt   !Magnetic extinctions taken into account if true
   !!----    type(kvect_info_type),   optional, intent(in)     :: Kinfo    !Modulation vector information
   !!----    logical,                 optional, intent(in)     :: Order    !If true the reflections are ordered by increasing sinTheta/Lambda
   !!----    logical,                 optional, intent(in)     :: Unique   !Ordered unique reflections are generated
   !!----    integer, dimension(3),   optional, intent(in)     :: seqindx  !Sequence of indices change
   !!----    integer, dimension(3,2), optional, intent(in)     :: hlim     !Index limits
   !!----    logical,                 optional, intent(in)     :: Mag_only !Only magnetic reflections are generated
   !!----    logical,                 optional, intent(in)     :: Friedel
   !!----    character(len=*),        optional, intent(in)     :: Ref_typ  !Refl, SRefl, or MRefl
   !!----    type(kvect_info_type),   optional, intent(out)    :: kout     !Info on k-vectors extracted from SpG
   !!----
   !!----    Function version of the subroutine Gener_Reflections. To be called from PyCrysFML.
   !!----
   !!----  11/03/2025
   !!----
   Module Function Generate_Reflections(Cell,Slmin,Slmax,SpG,MagExt,kinfo,Order,Unique,seqindx,hlim,mag_only,Friedel,Ref_typ) Result(Reflex)
      !---- Arguments ----!
      class(Cell_G_Type),                intent(in)     :: Cell
      real(kind=cp),                     intent(in)     :: Slmin
      real(kind=cp),                     intent(in)     :: Slmax
      class(Spg_Type),                   intent(in)     :: SpG
      logical,                 optional, intent(in)     :: MagExt
      type(kvect_info_type),   optional, intent(in)     :: Kinfo
      logical,                 optional, intent(in)     :: Order
      logical,                 optional, intent(in)     :: Unique
      integer, dimension(3),   optional, intent(in)     :: seqindx
      integer, dimension(3,2), optional, intent(in)     :: hlim
      logical,                 optional, intent(in)     :: Mag_only
      logical,                 optional, intent(in)     :: Friedel
      character(len=*),        optional, intent(in)     :: Ref_typ
      type(RefList_Type)                                :: Reflex

      !---- Local variables ----!
      logical,                  target  :: MagExt_
      type(kvect_info_type),    target  :: Kinfo_
      logical,                  target  :: Order_
      logical,                  target  :: Unique_
      integer, dimension(3),    target  :: seqindx_
      integer, dimension(3,2),  target  :: hlim_
      logical,                  target  :: Mag_only_
      logical,                  target  :: Friedel_
      character(len=5),         target  :: Ref_typ_
      integer,                  pointer :: ptr_iphase   => null()
      logical,                  pointer :: ptr_MagExt   => null()
      type(kvect_info_type),    pointer :: ptr_Kinfo    => null()
      logical,                  pointer :: ptr_Order    => null()
      logical,                  pointer :: ptr_Unique   => null()
      integer, dimension(:),    pointer :: ptr_seqindx  => null()
      integer, dimension(:,:),  pointer :: ptr_hlim     => null()
      logical,                  pointer :: ptr_Mag_only => null()
      logical,                  pointer :: ptr_Friedel  => null()
      character(len=:),         pointer :: ptr_Ref_typ  => null()
      type(kvect_info_type),    pointer :: ptr_kout     => null()

      if (present(magext)) then
         magext_ = magext
         ptr_magext => magext_
      end if
      if (present(kinfo)) then
         kinfo_ = kinfo
         ptr_kinfo => kinfo_
      end if
      if (present(order)) then
         order_ = order
         ptr_order => order_
      end if
      if (present(unique)) then
         unique_ = unique
         ptr_unique => unique_
      end if
      if (present(seqindx)) then
         seqindx_ = seqindx
         ptr_seqindx => seqindx_
      end if
      if (present(hlim)) then
         hlim_ = hlim
         ptr_hlim => hlim_
      end if
      if (present(mag_only)) then
         mag_only_ = mag_only
         ptr_mag_only => mag_only_
      end if
      if (present(friedel)) then
         friedel_ = friedel
         ptr_friedel => friedel_
      end if
      if (present(ref_typ)) then
         ref_typ_ = ref_typ
         ptr_ref_typ => ref_typ_
      end if

      call Gener_Reflections(Cell,Slmin,Slmax,Reflex,SpG,ptr_iphase,ptr_MagExt,ptr_kinfo,&
         ptr_order,ptr_Unique,ptr_seqindx,ptr_hlim,ptr_mag_only,ptr_Friedel,ptr_Ref_typ,ptr_kout)

   End Function Generate_Reflections
   !!
   !!---- Module Subroutine Gener_Reflections(Cell,Slmin,Slmax,Reflex,SpG,iphase,MagExt,kinfo,order,Unique,seqindx,hlim,mag_only,Friedel,Ref_typ,kout)
   !!----    !---- Arguments ----!
   !!----    class(Cell_G_Type),                intent(in)     :: Cell     !Unit cell object
   !!----    real(kind=cp),                     intent(in)     :: Slmin    !Minimum SinTheta/Lambda
   !!----    real(kind=cp),                     intent(in)     :: Slmax    !Maximum SinTheta/Lambda
   !!----    type(RefList_Type),                intent(in out) :: Reflex   !Reflection List
   !!----    class(Spg_Type) ,        optional, intent(in)     :: SpG      !General Space Group
   !!----    integer,                 optional, intent(in)     :: iphase   !Number of the current phase (if absent, a single phase is considered)
   !!----    logical,                 optional, intent(in)     :: MagExt   !Magnetic extinctions taken into account if true
   !!----    type(kvect_info_type),   optional, intent(in)     :: Kinfo    !Modulation vector information
   !!----    logical,                 optional, intent(in)     :: Order    !If true the reflections are ordered by increasing sinTheta/Lambda
   !!----    logical,                 optional, intent(in)     :: Unique   !Ordered unique reflections are generated
   !!----    integer, dimension(3),   optional, intent(in)     :: seqindx  !Sequence of indices change
   !!----    integer, dimension(3,2), optional, intent(in)     :: hlim     !Index limits
   !!----    logical,                 optional, intent(in)     :: Mag_only !Only magnetic reflections are generated
   !!----    logical,                 optional, intent(in)     :: Friedel
   !!----    character(len=*),        optional, intent(in)     :: Ref_typ  !Refl, SRefl, or MRefl
   !!----    type(kvect_info_type),   optional, intent(out)    :: kout     !Info on k-vectors extracted from SpG
   !!----
   !!----    Calculate reflections between the sin_theta/lambda shells defined by (Slmin,Slmax)
   !!----    The output is not ordered if order or Unique is not present. Valid for all type of space groups.
   !!----
   !!----  21/06/2019, Updated 24/04/2023
   !!----
   Module Subroutine Gener_Reflections(Cell,Slmin,Slmax,Reflex,SpG,iphase,MagExt,kinfo,Order,Unique,seqindx,hlim,mag_only,Friedel,Ref_typ,kout)
      !---- Arguments ----!
      class(Cell_G_Type),                intent(in)     :: Cell
      real(kind=cp),                     intent(in)     :: Slmin
      real(kind=cp),                     intent(in)     :: Slmax
      class(RefList_Type),               intent(in out) :: Reflex
      class(Spg_Type) ,        optional, intent(in)     :: SpG
      integer,                 optional, intent(in)     :: iphase
      logical,                 optional, intent(in)     :: MagExt
      type(kvect_info_type),   optional, intent(in)     :: Kinfo
      logical,                 optional, intent(in)     :: Order
      logical,                 optional, intent(in)     :: Unique
      integer, dimension(3),   optional, intent(in)     :: seqindx
      integer, dimension(3,2), optional, intent(in)     :: hlim
      logical,                 optional, intent(in)     :: Mag_only
      logical,                 optional, intent(in)     :: Friedel
      character(len=*),        optional, intent(in)     :: Ref_typ
      type(kvect_info_type),   optional, intent(out)    :: kout

      !---- Local variables ----!
      real(kind=cp)         :: epsr=0.00001, delt=0.0001
      real(kind=cp)         :: sval,max_s !,vmin,vmax
      integer               :: h,k,l,hmin,kmin,lmin,hmax,kmax,lmax, maxref,i,j,indp,indj, &
                               maxpos, mp, iprev,Dd, ia, i0, nk, nharm,n,num_ref, nrf

      integer,      dimension(:),   allocatable :: hh,kk,nulo
      integer,      dimension(:,:), allocatable :: hkl,hklm
      integer,      dimension(:),   allocatable :: indx,indtyp,ind,ini,fin,itreat

      real(kind=cp),dimension(:),   allocatable :: sv,sm
      logical                                   :: kvect,ordering,magg,Frd,Mag
      type(kvect_info_type)                     :: kinf
      integer, dimension(3)                     :: od,imin,imax
      real(kind=cp), dimension(3)               :: Hr,Hinc

      !> Init
      Dd=3
      ordering=.false.; Frd=.true.
      magg=.false.; mag=.false.; kvect=.false.
      if (present(MagExt)) Mag=MagExt
      if (present(Friedel)) Frd=Friedel
      if (present(mag_only)) magg=mag_only
      if (present(order)) ordering=order
      if (present(unique)) ordering=unique
      if (present(kinfo)) then
         nk=kinfo%nk
         nharm=kinfo%nq
         kvect=.true.
         kinf=kinfo !automatic allocation
      else
         if(present(SpG)) then
            Select Type(SpG)
               class is (SuperSpaceGroup_Type)
                  kvect=.true.
                  call Allocate_kvector(Spg%nk,Spg%nq,kinf)
                  kinf%kv=Spg%kv
                  kinf%q_coeff=Spg%q_coeff
                  nk=Spg%nk
                  nharm=Spg%nq
            End Select
         end if
      end if
      if (kvect) then
        Dd=3+nk ! total dimension of the reciprocal space
        if(present(kout)) kout=kinf
      end if
      hmax=nint(Cell%cell(1)*2.0*slmax+1.0)
      kmax=nint(Cell%cell(2)*2.0*slmax+1.0)
      lmax=nint(Cell%cell(3)*2.0*slmax+1.0)
      hmin=-hmax; kmin=-kmax; lmin= -lmax
      maxref= (2*hmax+1)*(2*kmax+1)*(2*lmax+1)
      if(present(hlim)) then
        imin=hlim(:,1)
        imax=hlim(:,2)
      else
        imin=[-hmax,-kmax,-lmax]
        imax=[ hmax, kmax, lmax]
      end if
      od=[1,2,3]
      if(present(seqindx)) then
        od=seqindx
        ordering=.false.
      end if

      if (kvect) then
         do k=1,nk
            maxref=maxref*(1+2*nharm)
         end do
         if (present(kinfo)) then
            max_s=maxval(kinfo%sintlim)
         else
            max_s=slmax
         end if
      end if

      allocate(hkl(Dd,maxref), indx(maxref), indtyp(maxref), ind(maxref), sv(maxref))
      allocate(hh(Dd), kk(Dd), nulo(Dd))
      nulo=0
      indtyp=0
      num_ref=0

      !> Generation of fundamental reflections
      i0=0
      ext_do: do h=imin(od(1)),imax(od(1))
         do k=imin(od(2)),imax(od(2))
            do l=imin(od(3)),imax(od(3))
               hh=0
               hh(od(1))=h
               hh(od(2))=k
               hh(od(3))=l
               sval=H_S(hh,Cell)
               if (sval > slmax .or. sval < slmin) cycle
               num_ref=num_ref+1
               if (num_ref > maxref) then
                  num_ref=maxref
                  exit ext_do
               end if
               if (sval < epsr) i0=num_ref !localization of 0 0 0  reflection
               sv(num_ref)=sval
               hkl(:,num_ref)=hh
            end do
         end do
      end do ext_do

      !> Generation of satellites
      !> The generated satellites corresponds to those obtained from the list
      !> of +/-kinfo%q_qcoeff
      nrf=num_ref
      if (kvect) then
         do_ex: do i=1,nrf
            hh=hkl(:,i)
            do n=1,kinf%nq
               do ia=-1,1,2
                  hh(4:3+nk)=ia*kinf%q_coeff(1:nk,n)
                  sval=H_S(hh, Cell, nk, kinf%kv)
                  if (sval > max_s .or. sval < slmin) cycle
                  num_ref=num_ref+1
                  if (num_ref > maxref) then
                     num_ref=maxref
                     exit do_ex
                  end if
                  sv(num_ref)=sval
                  hkl(:,num_ref)=hh
               end do !ia
            end do  !kinf%nq
         end do do_ex
      end if

      !> elimination of the reflection (0000..) if it is present in the list (i0 >=1)
      if(i0 >= 1) then
         do i=i0+1,num_ref
            sv(i-1)=sv(i)
            hkl(:,i-1)=hkl(:,i)
         end do
         num_ref=num_ref-1
      end if

      !> Determination of reflection character and extinctions (Lattice + others)
      n=0
      do i=1,num_ref
         hh=hkl(:,i)
         mp=0
         if (present(SpG)) then
            if (SpG%Num_Lat /= 0) then
               if (H_Latt_Absent(hh,SpG%Lat_tr,SpG%Num_Lat)) then
                  !write(*,"(a,6i4)")    "   Lattice absence: ", hh
                  cycle
               end if
            end if
            if (H_Absent(hh,SpG)) then
               if (SpG%Mag_type /= 2 .and. Mag) then
                  if (mH_Absent(hh,SpG)) then
                     !write(*,"(a,6i4)") "  Magnetic absence: ", hh
                     cycle
                  else
                     mp=1   !pure magnetic
                  end if
               else
                  !write(*,"(a,6i4)")   "           Absence: ", hh
                  cycle
               end if
            else
               if (SpG%Mag_type /= 2 .and. Mag) then
                  if (mH_Absent(hh,SpG)) then
                     mp=0  !pure nuclear
                  else
                     mp=2
                  end if
               end if
            end if
         end if

         n=n+1
         indtyp(n)=mp
         hkl(:,n)=hkl(:,i)
         sv(n)=sv(i)
      end do
      num_ref=n

      if (ordering) then
         indx=sort(sv,num_ref)
         allocate(hklm(Dd,num_ref),sm(num_ref))
         do i=1,num_ref
            j=indx(i)
            hklm(:,i)=hkl(:,j)
            sm(i)=sv(j)
            ind(i)=indtyp(j)
         end do

         !> contains now the type of reflection in the proper order
         indtyp(1:num_ref)=ind(1:num_ref)
         hkl(:,1:num_ref)=hklm(:,1:num_ref)
         sv(1:num_ref)=sm(1:num_ref)
         if (present(SpG) .and. present(Unique)) then
            deallocate(hkl,sv,indx,ind)
            allocate(ini(num_ref),fin(num_ref),itreat(num_ref))
            itreat=0; ini=0; fin=0
            indp=0
            do i=1,num_ref       !Loop over all reflections
               if (itreat(i) == 0) then   !If not yet treated do the following
                  hh(:)=hklm(:,i)
                  indp=indp+1  !update the number of independent reflections
                  itreat(i)=i  !Make this reflection treated
                  ini(indp)=i  !put pointers for initial and final equivalent reflections
                  fin(indp)=i
                  do j=i+1,num_ref  !look for equivalent reflections to the current (i) in the list
                     if (abs(sm(i)-sm(j)) > delt) exit
                     kk=hklm(:,j)
                     if (h_equiv(hh,kk,SpG,Frd)) then ! if  hh eqv kk (Friedel law according to Frd)
                        itreat(j) = i                 ! add kk to the list equivalent to i
                        fin(indp) = j
                     end if
                  end do
               end if !itreat
            end do

            !> Selection of the most convenient independent reflections
            allocate(hkl(Dd,indp),sv(indp),ind(indp))
            do i=1,indp
               maxpos=0
               indj=ini(i)
               iprev=itreat(indj)
               do j=ini(i),fin(i)
                  if (iprev /= itreat(j)) cycle
                  hh=hklm(:,j)
                  mp=count(hh > 0)
                  if (mp > maxpos) then
                     indj=j
                     maxpos=mp
                  end if
               end do !j
               hkl(:,i)=hklm(:,indj)
               if (hkl(1,i) < 0) hkl(:,i)=-hkl(:,i)
               sv(i)=sm(indj)
               ind(i)=indtyp(indj)
            end do
            indtyp(1:indp)=ind(1:indp)
            num_ref=indp
         end if  !SpG and Unique

      end if !ordering

      !> Final assignments
      if(present(Ref_Typ)) then

        select case (l_case(Ref_Typ))

            case ('srefl')
               call Initialize_RefList(Num_ref, reflex, 'SRefl', Dd)

            case ('mrefl')
               call Initialize_RefList(Num_ref, reflex, 'MRefl', Dd)

            case default
               call Initialize_RefList(Num_ref, reflex, 'Refl', Dd)

        end select

      else
        call Initialize_RefList(Num_ref, reflex, 'Refl', Dd)
      end if

      if(present(iphase)) reflex%i_ph=iphase

      do i=1,num_ref
         reflex%Ref(i)%h = hkl(:,i)
         if(dd > 3) then
           kk = abs(hkl(4:3+nk,i))
           reflex%Ref(i)%pcoeff = 0
           do n=1,kinf%nq
              if (equal_vector(kk(1:nk),abs(kinf%q_coeff(1:nk,n))))  then
                 reflex%Ref(i)%pcoeff=n
                 exit
              end if
           end do
           hr=real(hkl(1:3,i),kind=cp); hinc=0.0 ! Calculation of the real indices of a reflection when incommensurate
           do j=1,kinf%nk
              hinc=hinc+hkl(3+j,i)*kinf%kv(:,j) ! Hr= h a* + k b* + l c*  +  m1 k1 + m2 k2 + ...+ mnk knk
           end do
           hr=hr+hinc
           reflex%Ref(i)%hr=hr
         else
           reflex%Ref(i)%hr=real(hkl(1:3,i),kind=cp)
         end if
         reflex%Ref(i)%s = sv(i)
         if (present(SpG)) then
            reflex%Ref(i)%mult = h_mult(reflex%Ref(i)%h,SpG,Frd)
         else
            reflex%Ref(i)%mult = 1
         end if
         reflex%Ref(i)%imag = indtyp(i)
      end do

   End Subroutine Gener_Reflections

   !!----
   !!---- SUBROUTINE GENER_REFLECTIONS_SHUB
   !!----
   !!----    Calculate unique reflections below the maximum
   !!----    sin_theta/lambda provided.  The output is ordered.
   !!----
   !!---- Updated: April 2022
   !!
   Module Subroutine Gener_Reflections_Shub(Cell,SpG, Smax, Reflex,Friedel)
      !---- Arguments ----!
      type (Cell_G_Type),    intent(in)     :: Cell
      type (SpG_Type) ,      intent(in)     :: SpG
      real(kind=cp),         intent(in)     :: Smax        ! maximum SinTheta/Lambda
      type (RefList_Type),   intent(in out) :: Reflex
      logical, optional,     intent(in)     :: Friedel

      !---- Local variables ----!
      real(kind=cp)         :: sval
      integer               :: h,k,l,hmin,kmin,lmin,hmax,kmax,lmax, maxref,i,j,indp,indj, &
                               maxpos, mp, iprev,num_ref
      integer,       dimension(3) :: hh,kk,nulo
      integer,       dimension(:,:), allocatable :: hkl,hklm
      integer,       dimension(:),   allocatable :: indx,ini,fin,itreat
      real(kind=cp), dimension(:),   allocatable :: sv,sm
      logical :: Frd,Lcentred

      Frd=.true.; Lcentred=.false.
      if(present(Friedel)) Frd=Friedel
      !> Init
      nulo=0
      hmax=nint(Cell%cell(1)*2.0*smax+1.0)
      kmax=nint(Cell%cell(2)*2.0*smax+1.0)
      lmax=nint(Cell%cell(3)*2.0*smax+1.0)
      hmin=-hmax; kmin=-kmax; lmin = 0  !-lmax
      maxref= (2*hmax+1)*(2*kmax+1)*(lmax+1) !(2*lmax+1)
      allocate(hkl(3,maxref),indx(maxref),sv(maxref))

      if(Spg%num_lat > 0) Lcentred=.true.
      num_ref=0
      ext_do: do h=hmin,hmax
         do k=kmin,kmax
            do l=lmin,lmax

               hh=[h,k,l]
               if (h_equal(hh,nulo)) cycle
               sval=h_s(hh,cell)
               if (sval > smax) cycle
               if(Lcentred) then
                  if (H_Latt_Absent(hh,Spg%Lat_tr,Spg%Num_Lat)) cycle
               end if
               num_ref=num_ref+1
               if (num_ref > maxref) then
                  num_ref=maxref
                  exit ext_do
               end if
               sv(num_ref)=sval
               hkl(:,num_ref)=hh
            end do
         end do
      end do ext_do

      indx = sort(sv,num_ref)

      allocate(hklm(3,num_ref),sm(num_ref),ini(num_ref),fin(num_ref),itreat(num_ref))
      do i=1,num_ref
         j=indx(i)
         hklm(:,i)=hkl(:,j)
         sm(i)=sv(j)
      end do

      deallocate(hkl,sv,indx)

      itreat=0; ini=0; fin=0
      indp=0
      do i=1,num_ref ! Loop over all reflections
         if (itreat(i) == 0) then   !If not yet treated do the following
            hh(:)=hklm(:,i)
            indp=indp+1  !update the number of independent reflections
            itreat(i)=i  !Make this reflection treated
            ini(indp)=i  !put pointers for initial and final equivalent reflections
            fin(indp)=i

            do j=i+1,num_ref  !look for equivalent reflections to the current (i) in the list
               if (abs(sm(i)-sm(j)) > 0.001) exit
               kk=hklm(:,j)
               if (h_equiv(hh,kk,SpG,Frd)) then ! if  hh eqv kk
                  itreat(j) = i                 ! add kk to the list equivalent to i
                  fin(indp)=j
               end if
            end do
         end if !itreat
      end do

      !> Selection of the most convenient independent reflections
      allocate(hkl(3,indp),sv(indp),indx(indp))

      indx=2 !nuclear and magnetic contribution by default
      do i=1,indp
         maxpos=0
         indj=ini(i)
         iprev=itreat(indj)
         do j=ini(i),fin(i)
            if (iprev /= itreat(j)) cycle
            hh=hklm(:,j)
            mp=count(hh > 0)
            if (mp > maxpos) then
               indj=j
               maxpos=mp
            end if
         end do !j
         hkl(:,i)=hklm(:,indj)
         if (hkl(1,i) < 0) hkl(:,i)=-hkl(:,i)
         sv(i)=sm(indj)
      end do

      !> Now apply systematic absences other than lattice type
      num_ref=0
      do i=1,indp
         hh=hkl(:,i)
         if (H_Absent(hh,SpG)) then
            if (mH_Absent(hh,SpG)) then
               cycle
            else
               indx(i)=1   !pure magnetic
            end if
         else
            if (mH_Absent(hh,SpG)) indx(i)=0  !pure nuclear
         end if
         num_ref=num_ref+1
         hklm(:,num_ref)=hh
         sm(num_ref) = sv(i)
         indx(num_ref)=indx(i)
      end do

      !> Final assignments
      select type (r => Reflex%ref)
         type is (Srefl_type)
            call Initialize_RefList(Num_ref, reflex, 'SRefl', SpG%d-1)
         type is (MRefl_type)
            call Initialize_RefList(Num_ref, reflex, 'MRefl', SpG%d-1)
         class default
            call Initialize_RefList(Num_ref, reflex, 'Refl', SpG%d-1)
      end select
      do i=1,num_ref
         hh=hkl(:,i)
         reflex%ref(i)%h    = hh
         reflex%ref(i)%s    = sm(i)
         reflex%ref(i)%mult = h_mult(hh,SpG,Frd)
         reflex%ref(i)%imag = indx(i)
      end do
   End Subroutine Gener_Reflections_Shub

End SubModule Refl_Generate