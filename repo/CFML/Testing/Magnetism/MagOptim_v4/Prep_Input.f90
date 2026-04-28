 Module prep_input
 !---- Use Modules ----!
 use CFML_GlobalDeps,             only: sp, cp, err_CFML, eps
 use CFML_Atoms,                  only: AtList_Type, mAtom_list_Type, allocate_mAtom_list
 use CFML_Strings,                only: L_Case, U_Case, Get_words,file_type
 use CFML_Reflections,            only: RefList_Type, H_s, Srefl_type
 use CFML_Structure_Factors,      only: Structure_Factors, Init_Structure_Factors
 use CFML_Propagation_vectors,    only: K_Equiv_Minus_K
 use CFML_gSpaceGroups,           only: SPG_type
 use CFML_Metrics,                only: Cell_G_Type
 use CFML_KeyCodes
 use CFML_kvec_Symmetry
 use CFML_kvec_Structure_Factors
 use CFML_kvec_Polarimetry

 implicit none

   !---- List of public subroutines ----!
 public :: Readn_Set_Job, Copy_mAtom_List, Copy_MagDom_List, Write_ObsCalc, MagDom_to_Dataset

 integer, save   :: lun=1,lan=2,lin=3
 integer, save   :: Nobs,Nset,Nref,Nf2,Npol,MNset
 real, public    :: Scalef,cost,costPol,costF2

   !---- OLD types are declared below the NEW types
   !---- NEW types
   !!----    mOBSERVATION_TYPE
   !!----    mOBSERVATION_LIST_TYPE
   !!----    Multidata_type  !NEW 02/12

    !!----
    !!---- TYPE :: mOBSERVATION_TYPE
    !!--..
    !!---- Type, public :: mObservation_Type
    !!----    integer                 :: ncont ! number of contributing reflections
    !!-----   integer,dimension(3,12) :: Hd    ! Indices of contributing reflections
    !!-----   integer,dimension(  12) :: icod  ! Pointer to the number of domain
    !!-----   integer,dimension(  12) :: p     ! Pointer to the list of independent reflections
    !!----    real(kind=sp)           :: Gobs  ! Observed  Magnetic Intensity
    !!----    real(kind=sp)           :: SGobs ! Sigma of  Gobs
    !!---- End Type mObservation_Type
    !!----
    !!---- Created: January - 2012
    !!
    Type, public :: mObservation_Type
       integer                                  :: ncont ! number of contributing reflections
       real(kind=sp),allocatable,dimension(:,:) :: Hd    ! (3,ncont) Indices of contributing reflections
       integer,allocatable,dimension(:)         :: icod  ! (ncont)   Pointer to the number of domain
       integer,allocatable,dimension(:)         :: p     ! (ncont)   Pointer to the list of independent reflections
       real(kind=sp)                            :: Gobs  ! Observed  Magnetic Intensity
       real(kind=sp)                            :: sGobs ! Sigma of  Gobs
       real(kind=sp)                            :: wGobs ! Weight of Gobs, 1/sGobs^2
    End Type mObservation_Type

    !!----
    !!---- TYPE :: mOBSERVATION_LIST_TYPE
    !!--..
    !!---- Type, public :: mObservation_List_Type
    !!----    integer                                          :: Nobs  ! Number of Observations
    !!----    type(mobservation_type),allocatable,dimension(:) :: Ob    ! Observation (F2, etc...)
    !!---- End Type mObservation_List_Type
    !!----
    !!---- Created: January - 2012
    !!
    Type, public :: mObservation_List_Type
       integer                                          :: Nobs  ! Number of Observations
       type(mObservation_type),allocatable,dimension(:) :: Ob    ! Observation (F2, etc...)
    End Type mObservation_List_Type

    !!
    !!---- TYPE :: Multidata_type
    !!---- integer                                              :: Nset      ! Number of Datasets
    !!---- integer,allocatable, dimension(:)                    :: MNset     ! To Match MN Dataset to Pol Dataset
    !!---- integer,allocatable, dimension(:)                    :: Nobs      ! Number of Observations in the Dataset
    !!---- character(len=128),allocatable, dimension(:)         :: Datfile   ! Name of the File with the Dataset
    !!---- logical,allocatable, dimension(:)                    :: SNP       ! True if the polarisation Dataset
    !!---- logical,allocatable, dimension(:)                    :: f2mag     ! True if Magnetic Intensity Dataset
    !!---- logical,allocatable, dimension(:)                    :: f2nuc     ! True if Nuclear Intensity Dataset
    !!---- logical,allocatable, dimension(:)                    :: f2nm      ! True if Nuclear Intensity Dataset for NM interference
    !!---- type(Magnetic_Domain_type),allocatable, dimension(:) :: MagDom    ! Domains for the Dataset
    !!
    !!----
    !!---- Created: February - 2012, Modified: October 2012
    !!
    Type, public :: Multidata_type
       integer                                              :: Nset      ! Number of Datasets
       integer,allocatable, dimension(:)                    :: MNset     ! To Match MN Dataset to Pol Dataset
       integer,allocatable, dimension(:)                    :: Nobs      ! Number of Observations in the Dataset
       character(len=128),allocatable, dimension(:)         :: Datfile   ! Name of the File with the Dataset
       logical,allocatable, dimension(:)                    :: SNP       ! True if Polarisation Dataset
       logical,allocatable, dimension(:)                    :: f2mag     ! True if Magnetic Intensity Dataset
       logical,allocatable, dimension(:)                    :: f2nuc     ! True if Nuclear Intensity Dataset for scale
       logical,allocatable, dimension(:)                    :: f2mn      ! True if Nuclear Intensity Dataset for NM interference
       type(Magnetic_Domain_type),allocatable, dimension(:) :: MagDom    ! Domains for the Dataset
    End Type Multidata_type

    !!----
    !!----  MAGHD_MULTILIST_TYPE
    !!----
    Type, Public  :: MagHD_MultiList_Type
       integer                                         :: Nset
       Type(MagHD_List_Type),allocatable, dimension(:) :: Mhlist
    End Type MagHD_MultiList_Type
    !!----
    !!---- Created: February - 2012 OZ

 type(file_type)                       :: file_cfl
 class(spg_type), allocatable          :: SpG
 type(MagSymm_k_Type)                  :: MGp
 type(MagHD_Type)                      :: Mh
 type(MagHD_List_Type)                 :: Mhlist
 type(RefList_Type)                    :: Nhkl,MNhkl
 type(mObservation_Type)               :: Ob
 type(mObservation_List_Type)          :: Oblist,MNOblist
 type(Cell_G_Type)                     :: Cell
 type(Atlist_Type)                     :: A
 type(mAtom_List_Type),public          :: mA,mA_clone
 type(Magnetic_Domain_type),public,save:: Mag_Dom,AllMag_Dom,Mag_dom_clone
 type(Multidata_type),save             :: Multidata
 type(MagHD_MultiList_Type)            :: MhMultilist
 type(Polar_calc_type)                 :: Polari
 type(Polar_Calc_List_type)            :: Polarilist
 type(Polar_CalcMulti_List_type),public:: PolariMultilist
 type(Polar_calc_sVs_type)             :: PolarisVs
 type(Polar_Calc_sVs_List_type)        :: PolarisVslist
 type(Polar_obs_type)                  :: Polaro
 type(Polar_Obs_List_type)             :: Polarolist
 type(Polar_ObsMulti_List_type),public :: PolaroMultilist
 type(Polar_CalcMulti_sVs_List_type),public:: PolariMultisVslist

!---- List of public arrays
! public :: Mag_Dom, mA, PolaroMultilist, PolariMultilist, PolariMultisVslist

contains

!---- Subroutines ----!
!******************************************!
    Subroutine Readn_Set_Data(file_cfl)
!******************************************!
!     Analyses and Reads observations given in different files
!     Nobs - total number of reflections
!     multi k is implemented October 2012

      !---- Arguments ----!
      type(file_type),   intent(in) :: file_cfl

      !---- Local variables ----!
      character(len=50),dimension(25):: label
      character(len=256)             :: title,formt
      character(len=256)             :: lowline
      character(len=1)               :: sig
      integer                        :: ier,ic,num_k,m,ih,ik,il,ncont,iv,i,j,ityp,ipow,iset,&
                                        iobs,ind,nd
      integer                        :: n_ini, n_end
      real,allocatable,dimension(:,:):: vk
      real                           :: lambda,k11,k22,k12,kdif,k0(3)
      logical                        :: kvect_begin=.true.

      Nset=0; MNset=0; Npol=0
      k0=[0.0,0.0,0.0]

      !---
      !--- Read the wave vector as in Readn_Set_Magnetic_Structure
      !---
      do i=1,file_cfl%nlines
        if(len_trim(file_cfl%line(i)%Str_tmp) == 0) cycle
        lowline=l_case(adjustl(file_cfl%line(i)%Str_tmp))
        if (lowline(1:13) == "mag_structure") then
           n_ini=i
        end if
        if (lowline(1:7) =="end_mag" ) then
           n_end=i
           exit
        end if
      end do

      num_k=0
      i=n_ini
      do
        i=i+1
        if(i >= n_end) exit
        if(len_trim(file_cfl%line(i)%Str_tmp) == 0) cycle
        lowline=l_case(adjustl(file_cfl%line(i)%Str_tmp))
        ! Read propagation vectors
        if(lowline(1:5) == "kvect" .and. kvect_begin) then
          num_k=num_k+1
          read(unit=lowline(6:),fmt=*,iostat=ier) MGp%kvec(:,num_k)
          if (ier /= 0) then
              err_CFML%flag=.true.
              err_CFML%Msg=" Error reading propagation vectors"
              return
          end if
          do !repeat reading until continuous KVECT lines are exhausted
             i=i+1
             if(len_trim(file_cfl%line(i)%Str_tmp) == 0) cycle
             lowline=adjustl(l_case(file_cfl%line(i)%Str_tmp))
             if (lowline(1:1) == "!" .or. lowline(1:1) == "#") cycle
             if (lowline(1:5) == "kvect") then
                read(unit=lowline(6:),fmt=*,iostat=ier) MGp%kvec(:,num_k+1)
                ! check if -k is given, skip if yes
                k11=sum(MGp%kvec(:,(num_k))*MGp%kvec(:,num_k))
                k22=sum(MGp%kvec(:,(num_k+1))*MGp%kvec(:,(num_k+1)))
                k12=sum(MGp%kvec(:,(num_k+1))*MGp%kvec(:,num_k))
                kdif=k12/sqrt(k11*k22)
                if (kdif < -0.99) then
                   write(unit=*,fmt="(a)") " Skip -k propagation vectors"
                   stop
                end if
                num_k=num_k+1
                if (ier /= 0) then
                   err_CFML%flag=.true.
                   err_CFML%Msg=" Error reading propagation vectors"
                   return
                end if
             else
                i=i-1
                kvect_begin=.false.
                exit
             end if
          end do
          cycle
        end if
      end do
      MGp%nkv   =num_k
      allocate(vk(1:3,num_k))

      !---
      !--- Analyse how many data are provided in cfl file
      !---
      do i=1,file_cfl%nlines
        if(len_trim(file_cfl%line(i)%Str_tmp) == 0) cycle
        lowline=l_case(adjustl(file_cfl%line(i)%Str_tmp))
        if(lowline(1:10) == "f2mag_data") then
          Nset=Nset+1
        end if

        if(lowline(1:10) == "f2nuc_data") then
          Nset=Nset+1
        end if

        if(lowline(1:13) == "f2magnuc_data") then
          Nset=Nset+1
          MNset=MNset+1
        end if

        if(lowline(1:12) == "cryopad_data") then
          Nset=Nset+1
          Npol=Npol+1
        end if

        if(lowline(1:10) == "mupad_data") then
          Nset=Nset+1
          Npol=Npol+1
        end if

      end do

      if(allocated(Multidata%datfile)) deallocate(Multidata%datfile)
      allocate(Multidata%datfile(Nset))
      Multidata%Nset= Nset

      if(allocated(Multidata%MagDom)) deallocate(Multidata%MagDom)
      allocate(Multidata%MagDom(Nset))
      if(allocated(Multidata%Nobs)) deallocate(Multidata%Nobs)
      allocate(Multidata%Nobs(Nset))
      if(allocated(Multidata%MNset)) deallocate(Multidata%MNset)
      allocate(Multidata%MNset(Nset))
      if(allocated(Multidata%SNP)) deallocate(Multidata%SNP)
      allocate(Multidata%SNP(Nset))
      if(allocated(Multidata%f2nuc)) deallocate(Multidata%f2nuc)
      allocate(Multidata%f2nuc(Nset))
      if(allocated(Multidata%f2mag)) deallocate(Multidata%f2mag)
      allocate(Multidata%f2mag(Nset))
      if(allocated(Multidata%f2mn)) deallocate(Multidata%f2mn)
      allocate(Multidata%f2mn(Nset))

      if(allocated(PolaroMultilist%Polarolist)) deallocate(PolaroMultilist%Polarolist)
      allocate(PolaroMultilist%Polarolist(Npol))

      if(allocated(MhMultilist%Mhlist)) deallocate(MhMultilist%Mhlist)
      allocate(MhMultilist%Mhlist(Nset))
      MhMultilist%Nset= Nset
      Multidata%MNset=0

      Nset=0; MNset=0
      !---
      !--- Write Multidata%datfile,Multidata%MagDom,etc
      !---
      do i=1,file_cfl%nlines
        if(len_trim(file_cfl%line(i)%Str_tmp) == 0) cycle
        lowline=l_case(adjustl(file_cfl%line(i)%Str_tmp))

        if(lowline(1:10) == "f2mag_data") then
           Nset=Nset+1
           lowline=adjustl(lowline(12:))
           call get_words(lowline,label,ic)
           Multidata%datfile(Nset)=l_case(label(1))
           nd=ic-1 !number of domains read from f2mag_data line

           !--- Here I assume that two chiral domains are given in Mag_data
           if(nd >= 2 .and. modulo(nd,2) ==0) then
             Multidata%MagDom(Nset)%nd=nd/2
             ind=1
             do j=2,nd,2
               Multidata%MagDom(Nset)%Lab(1,ind)=l_case(label(j))
               Multidata%MagDom(Nset)%Lab(2,ind)=l_case(label(j+1))
               ind=ind+1
             end do
           else
              write(unit=*,fmt="(a)") " => All domains should be given in magdom and _data! "
              stop
           end if

           call Countref_f2mag_file(Multidata%datfile(Nset),Nf2)
           Multidata%Nset=Nset
           Multidata%Nobs(Nset)=Nf2
           Multidata%SNP(Nset)=.false.
           Multidata%f2nuc(Nset)=.false.
           Multidata%f2mn(Nset) =.false.
           Multidata%f2mag(Nset)=.true.
        end if

        if(lowline(1:10) == "f2nuc_data") then
          Nset=Nset+1
          lowline=adjustl(lowline(12:))
          call get_words(lowline,label,ic)
          Multidata%datfile(Nset)=l_case(label(1))

          call Countref_f2nuc_file(Multidata%datfile(Nset),Nf2)
          Multidata%Nset=Nset
          Multidata%Nobs(Nset) =Nf2
          Multidata%SNP(Nset)  =.false.
          Multidata%f2mag(Nset)=.false.
          Multidata%f2mn(Nset) =.false.
          Multidata%f2nuc(Nset)=.true.
        end if

        if(lowline(1:13) == "f2magnuc_data") then
          Nset=Nset+1
          MNset=MNset+1
          lowline=adjustl(lowline(15:))
          call get_words(lowline,label,ic)
          Multidata%datfile(Nset)=l_case(label(1))

          call Countref_f2mag_file(Multidata%datfile(Nset),Nf2)
          Multidata%Nset=Nset
          Multidata%Nobs(Nset) =Nf2
          Multidata%SNP(Nset)  =.false.
          Multidata%f2mag(Nset)=.false.
          Multidata%f2mn(Nset) =.true.
          Multidata%f2nuc(Nset)=.false.
        end if

        if(lowline(1:12) == "cryopad_data") then
          Nset=Nset+1
          lowline=adjustl(lowline(14:))
          call get_words(lowline,label,ic)
          Multidata%datfile(Nset)=label(1)
          nd=ic-1 !number of domains read from cryopad_data line

          !--- Here I assume that two chiral domains are given in Mag_data
          if(nd >= 2 .and. modulo(nd,2) ==0) then
             Multidata%MagDom(Nset)%nd=nd/2
             ind=1
             do j=2,nd,2
               Multidata%MagDom(Nset)%Lab(1,ind)=l_case(label(j))
               Multidata%MagDom(Nset)%Lab(2,ind)=l_case(label(j+1))
               ind=ind+1
             end do
          else
              write(unit=*,fmt="(a)") " => All domains should be given in magdom and _data! "
              stop
          end if

          call Countref_Pol_file(Multidata%datfile(Nset),Npol)
          Multidata%Nset=Nset
          Multidata%Nobs(Nset) = Npol
          Multidata%SNP(Nset)  =.true.
          Multidata%f2mag(Nset)=.false.
          Multidata%f2mn(Nset) =.false.
          Multidata%f2nuc(Nset)=.false.
        end if

        if(lowline(1:10) == "mupad_data") then
          Nset=Nset+1
          lowline=adjustl(lowline(12:))
          call get_words(lowline,label,ic)
          Multidata%datfile(Nset)=label(1)
          nd=ic-1 !number of domains read from mupad_data line

          !--- Here I assume that two chiral domains are given in Mag_data
          if(nd >= 2 .and. modulo(nd,2) ==0) then
             Multidata%MagDom(Nset)%nd=nd/2
             ind=1
             do j=2,nd,2
               Multidata%MagDom(Nset)%Lab(1,ind)=l_case(label(j))
               Multidata%MagDom(Nset)%Lab(2,ind)=l_case(label(j+1))
               ind=ind+1
             end do
          else
             write(unit=*,fmt="(a)") " => All domains should be given in magdom and _data! "
             stop
          end if
          call Countref_Pol_file(Multidata%datfile(Nset),Npol)
          Multidata%Nset=Nset
          Multidata%Nobs(Nset) = Npol
          Multidata%SNP(Nset)  =.true.
          Multidata%f2mag(Nset)=.false.
          Multidata%f2mn(Nset) =.false.
          Multidata%f2nuc(Nset)=.false.
        end if

      end do

      do iset=1,Nset !Loop on all data

        if(Multidata%SNP(iset)) then
          !---
          !--- Polarisation data
          !---

          PolaroMultilist%Nset= iset
          open(unit=lan,file=trim(Multidata%datfile(iset)),status="old",action="read")

          Npol=Multidata%Nobs(iset)
          if(allocated(PolaroMultilist%Polarolist(iset)%Polaro)) deallocate(PolaroMultilist%Polarolist(iset)%Polaro)
          allocate(PolaroMultilist%Polarolist(iset)%Polaro(Npol))
          PolaroMultilist%Polarolist(iset)%Nref= Npol

          if(allocated(MhMultilist%Mhlist(iset)%Mh)) deallocate(MhMultilist%Mhlist(iset)%Mh)
          allocate(MhMultilist%Mhlist(iset)%Mh(Npol))
          MhMultilist%Mhlist(iset)%Nref= Npol

          if(all(abs(MGp%kvec(:,iv)-k0(:)) < 0.001)) then
            call Initialize_RefList(Npol, MNhkl, "SRefl", 3)
          end if

          do iobs=1,Npol !Loop on polarised observations
            read(unit=lan,fmt=*,iostat=ier) ih,ik,il,m,PolaroMultilist%Polarolist(iset)%Polaro(iobs)%SPV, &
               PolaroMultilist%Polarolist(iset)%Polaro(iobs)%P  !2 vectors in scattering plane, Pin
            if(ier /= 0) exit
            read(unit=lan,fmt=*) PolaroMultilist%Polarolist(iset)%Polaro(iobs)%oPij
            read(unit=lan,fmt=*) PolaroMultilist%Polarolist(iset)%Polaro(iobs)%soPij

            ! calculate weight Polarolist%Polaro(i)%woPij 1/sigma**2
            PolaroMultilist%Polarolist(iset)%Polaro(iobs)%woPij= 1.0 / max(eps, &
            PolaroMultilist%Polarolist(iset)%Polaro(iobs)%soPij**2)

            ! construct partially the object Mh
            j=sign(1,m)
            sig="+"
            if( j == -1) sig="-"
            Mh%signp=real(-j)  ! sign "+" for H-k and "-" for H+k

            if(all(abs(MGp%kvec(:,iv)-k0(:)) < 0.001)) &
            MNhkl%Ref(iobs)%H=[ih,ik,il]

            iv=abs(m)
            ! kvec from cfl is taken
            Mh%num_k=iv
            Mh%h= real([ih,ik,il]) - Mh%signp*MGp%kvec(:,iv)
            Mh%s= h_s(Mh%h,Cell)
            Mh%keqv_minus=K_Equiv_Minus_K(MGp%kvec(:,iv),MGp%latt)
            MhMultilist%Mhlist(iset)%Mh(iobs)=Mh
            PolaroMultilist%Polarolist(iset)%Polaro(iobs)%H=MhMultilist%Mhlist(iset)%Mh(iobs)%h
          end do !Loop on polarised observations

          if(all(abs(MGp%kvec(:,iv)-k0(:)) < 0.001)) then
            if(iv> 1) write(unit=*,fmt="(a)") "Presently for k=0 only one propagation vector is forseen"

            do iobs=1,Npol !Loop on polarised and MN observations
              call Init_Structure_Factors(MNhkl,A,Spg,mode="NUC",lun=lun)
              call Structure_Factors(MNhkl,A,SpG,mode="NUC")
            end do !Loop on polarised and MN observations
          end if

          close(lan)

        end if !Multidata%SNP(iset)

        if(Multidata%f2mag(iset)) then

          !---
          ! Integrated magnetic intensity data
          !---

          open(unit=lin,file=trim(Multidata%datfile(iset)),status="old",action="read")
          Nf2=Multidata%Nobs(iset)

          read(unit=lin,fmt=*) title
          read(unit=lin,fmt=*) formt
          read(unit=lin,fmt=*) lambda,ityp,ipow
          read(unit=lin,fmt=*) num_k

          do iv=1,num_k
            read(unit=lin,fmt=*) m,vk(:,iv)
            ! check if -k is given, skip if yes
            if(iv > 1) then
              k11=sum(vk(:,(iv-1))*vk(:,(iv-1)))
              k22=sum(vk(:,iv)*vk(:,iv))
              k12=sum(vk(:,(iv-1))*vk(:,iv))
              kdif=k12/sqrt(k11*k22)
              if (kdif < -0.99) then
                 write(unit=*,fmt="(a)") " Skip -k propagation vectors in .int"
                 stop
              end if
            end if
          end do

          ! check if vk equal to kvec from cfl
          do iv=1,num_k
            if(ANY( abs(MGp%kvec(:,iv)-vk(:,iv)) > 0.01) ) then
              write(unit=*,fmt="(a)") "Propagation vectors in cfl and f2mag_data do not match"
              stop
            end if
          end do

          if(allocated(Oblist%Ob)) deallocate(Oblist%Ob)
          allocate(Oblist%Ob(Nf2))

          ! temporarily programmed for ncont=1
          do iobs=1,Nf2
            allocate(Oblist%Ob(iobs)%Hd(1:3,1))
          end do
          Oblist%Nobs= Nf2

          ! Multidata%Nobs(iset) should be the same as Oblist%Nobs
          if(allocated(MhMultilist%Mhlist(iset)%Mh)) deallocate(MhMultilist%Mhlist(iset)%Mh)
          allocate(MhMultilist%Mhlist(iset)%Mh(Nf2))
          MhMultilist%Mhlist(iset)%Nref= Nf2

          do iobs=1,Nf2
            read(unit=lin,fmt=*,iostat=ier) ih,ik,il,m,Oblist%Ob(iobs)%Gobs,Oblist%Ob(iobs)%SGobs,ncont
            if(ier /= 0) exit
            j=sign(1,m)
            iv=abs(m)
            sig="+"
            Oblist%Ob(iobs)%Hd(:,1)=real([ih,ik,il]) + vk(:,iv)

            if(j== -1) then
              sig="-"
              Oblist%Ob(iobs)%Hd(:,1)=real([ih,ik,il]) - vk(:,iv)
            end if

            ! Construct partially the object Mh
            Mh%signp=real(-j)  ! sign "+" for H-k and "-" for H+k
            Mh%num_k=iv
            Mh%h= real([ih,ik,il]) - Mh%signp*MGp%kvec(:,iv)
            Mh%s = h_s(Mh%h,Cell)
            Mh%keqv_minus=K_Equiv_Minus_K(MGp%kvec(:,iv),MGp%latt)
            MhMultilist%Mhlist(iset)%Mh(iobs)=Mh
          end do

          ! calculate weight Oblist%Ob(i)%wGobs 1/sGobs
          Oblist%Ob(:)%wGobs=1.0/max([(Oblist%Ob(i)%sGobs**2,i=1,Oblist%Nobs)],eps)

          close(lin)
        end if !Multidata%f2mag(iset)

        if(Multidata%f2nuc(iset)) then
          !---
          ! Integrated nuclear intensity data for scaling
          !---

          open(unit=lin,file=trim(Multidata%datfile(iset)),status="old",action="read")
          Nf2=Multidata%Nobs(iset)

          call Initialize_RefList(Nf2, Nhkl, "SRefl", 3)

          read(unit=lin,fmt=*) title
          read(unit=lin,fmt=*) formt
          read(unit=lin,fmt=*) lambda,ityp,ipow

          Select Type (nr => Nhkl%Ref)
            Type is (SRefl_Type)
               do iobs=1,Nf2 ! Int are stored, not structure factors
                 read(unit=lin,fmt=*,iostat=ier) nr(iobs)%H,nr(iobs)%Fo,nr(iobs)%sFo,ncont
                 if(ier /= 0) exit
               end do
               call Init_Structure_Factors(Nhkl,A,Spg,mode="NUC",lun=lun)
               call Structure_Factors(Nhkl,A,SpG,mode="NUC")
               do iobs=1,Nf2 ! Int are stored, not structure factors
                 nr(iobs)%Fc=nr(iobs)%Fc**2
               end do
              ! calculate weight Nhkl%Ref(i)%w 1/sFo
              nr(:)%w=1.00/max([(nr(i)%sFo**2,i=1,Nhkl%Nref)],eps)
              Scalef=sum( [(nr(i)%Fc*nr(i)%Fo *nr(i)%w,i=1,Nhkl%NRef)])/ &
                     sum( [(nr(i)%Fc**2 *nr(i)%w,i=1,Nhkl%NRef)] )
            class default
               err_CFML%Ierr=1
               err_CFML%flag=.true.
               err_CFML%Msg="You have to use SRef_Type class as minimum for this calculation!"
               return
          End select

        close(lin)
        end if !Multidata%f2nuc(iset)

        if(Multidata%f2mn(iset)) then
          !---
          ! Integrated nuclear-magnetic intensity data assuming magnetic contribution =0
          !---

          open(unit=lin,file=trim(Multidata%datfile(iset)),status="old",action="read")
          Nf2=Multidata%Nobs(iset)
          ! sort out to which Pol the NM dataset belongs
          do i=1,Nset
            if(Multidata%Nobs(i)==Nf2 .and. Multidata%SNP(i) .and. .not. Multidata%f2mn(i)) &
              Multidata%MNset(i)=iset
          end do

          read(unit=lin,fmt=*) title
          read(unit=lin,fmt=*) formt
          read(unit=lin,fmt=*) lambda,ityp,ipow
          read(unit=lin,fmt=*) num_k

          do iv=1,num_k
            read(unit=lin,fmt=*) m,vk(:,iv)
            ! check if -k is given, skip if yes
            if(iv > 1) then
              k11=sum(vk(:,(iv-1))*vk(:,(iv-1)))
              k22=sum(vk(:,iv)*vk(:,iv))
              k12=sum(vk(:,(iv-1))*vk(:,iv))
              kdif=k12/sqrt(k11*k22)
              if (kdif < -0.99 ) then
                write(unit=*,fmt="(a)") " Skip -k propagation vectors in .int"
                stop
              end if
            end if
          end do
          ! check if vk equal to kvec from cfl
          do iv=1,num_k
            if(ANY( abs(MGp%kvec(:,iv)-vk(:,iv)) > 0.01) ) then
              write(unit=*,fmt="(a)") "Propagation vectors in cfl and f2mag_data do not match"
              stop
            end if
          end do

          if(allocated(MNOblist%Ob)) deallocate(MNOblist%Ob)
          allocate(MNOblist%Ob(Nf2))

          ! temporarily programmed for ncont=1
          do iobs=1,Nf2
            allocate(MNOblist%Ob(iobs)%Hd(1:3,1))
          end do
          MNOblist%Nobs=Nf2

          do iobs=1,Nf2
            read(unit=lin,fmt=*,iostat=ier) ih,ik,il,m,MNOblist%Ob(iobs)%Gobs,MNOblist%Ob(iobs)%SGobs,ncont
            if(ier /= 0) exit

            j=sign(1,m)
            iv=abs(m)
            sig="+"
            MNOblist%Ob(iobs)%Hd(:,1)=real([ih,ik,il]) + vk(:,iv)

            if(j== -1) then
              sig="-"
              MNOblist%Ob(iobs)%Hd(:,1)=real([ih,ik,il]) - vk(:,iv)
            end if
          end do

          close(lin)
        end if !Multidata%f2mn(iset)

        ! do nothing, if f2nuc and f2mn. they are not counted
        if(.not. MultiData%f2nuc(iset).and. .not.MultiData%f2mn(iset)) Nobs=Nobs+MultiData%Nobs(iset)

      end do !Loop on all data

    End Subroutine Readn_Set_Data

!******************************************!
    Subroutine Countref_Pol_file(datfile,Npol)
!******************************************!
! reads file with polarization data (counts entries)
!
      !---- Argument ----!
      character(len=*),intent(in) :: datfile
      integer,intent(out)         :: Npol

      !---- Local variables ----!
      integer                     :: ih,ik,il,m,ier
      logical                     :: pesta
      real                        :: vpl, pol

     inquire(file=trim(datfile),exist=pesta)
     if( .not. pesta) then
       write(unit=*,fmt="(a)") " File: "//trim(datfile)//" doesn't exist!"
       stop
     end if
     open(unit=lan,file=trim(datfile), status="old",action="read")
     Npol=0

     do !just to get Npol
      read(unit=lan,fmt=*,iostat=ier) ih,ik,il,m,vpl,pol  !2 vectors in scattering plane, Pin
      if(ier /= 0) exit
      read(unit=lan,fmt=*) Polaro%oPij
      read(unit=lan,fmt=*) Polaro%soPij
      Npol=Npol+1
     end do

     close(lan)

    End Subroutine Countref_Pol_File

!******************************************!
    Subroutine Countref_f2mag_File(datfile,Nf2)
!******************************************!
! reads file with intensity data (counts entries)
!
      !---- Argument ----!
      character(len=*),intent(in) :: datfile
      integer,intent(out)         :: Nf2

      !---- Local variables ----!
      character(len=256)          :: title,formt,lambda
      integer                     :: ier,num_k,m,ih,ik,il,ncont,i,ityp,ipow
      real,   dimension(3)        :: vk
      real                        :: Gobs,SGobs
      logical                     :: pesta

      inquire(file=trim(datfile),exist=pesta)
      if( .not. pesta) then
        write(unit=*,fmt="(a)") " File: "//trim(datfile)//" does'nt exist!"
        stop
      end if
      open(unit=lin,file=trim(datfile), status="old",action="read")

      read(unit=lin,fmt=*) title
      read(unit=lin,fmt=*) formt
      read(unit=lin,fmt=*) lambda,ityp,ipow
      read(unit=lin,fmt=*) num_k

      do i=1,num_k
        read(unit=lin,fmt=*) m,vk
      end do

      !if(num_k /= MGp%nkv) then  !Not needed at this stage
      !  write(unit=*,fmt="(a)") " => Mismatch between number of wavevectors in .cfl and .int files! "
      !  stop
      !end if

      Nf2=0
      do !just to get Nf2, which wave vector is not important
       read(unit=lin,fmt=*,iostat=ier) ih,ik,il,m,Gobs,SGobs,ncont
       if(ier /= 0) exit
       Nf2=Nf2+1
      end do

     close(lin)

    End Subroutine Countref_f2mag_File

!******************************************!
    Subroutine Countref_f2nuc_File(datfile,Nf2)
!******************************************!
! reads file with intensity data (counts entries)
!
      !---- Argument ----!
      character(len=*),intent(in) :: datfile
      integer,intent(out)         :: Nf2

      !---- Local variables ----!
      character(len=256)          :: title,formt,lambda
      integer                     :: ier,ih,ik,il,ncont,ityp,ipow
      real                        :: Gobs,SGobs
      logical                     :: pesta

      inquire(file=trim(datfile),exist=pesta)
      if( .not. pesta) then
        write(unit=*,fmt="(a)") " File: "//trim(datfile)//" does'nt exist!"
        stop
      end if
      open(unit=lin,file=trim(datfile), status="old",action="read")

      read(unit=lin,fmt=*) title
      read(unit=lin,fmt=*) formt
      read(unit=lin,fmt=*) lambda,ityp,ipow

      Nf2=0
      do !just to get Nf2
       read(unit=lin,fmt=*,iostat=ier) ih,ik,il,Gobs,SGobs,ncont
       if(ier /= 0) exit
       Nf2=Nf2+1
      end do

      close(lin)

    End Subroutine Countref_f2nuc_File

!******************************************!
    Subroutine Readn_Set_Job(file_cfl)
!******************************************!
       !---- Arguments ----!
       Type(file_type),   intent( in)  :: file_cfl

       !---- Local variables ----!
       character(len=132)   :: line
       integer              :: i,j

       i=0

       do j=1,file_cfl%nlines
          if(len_trim(file_cfl%line(i)%Str_tmp) == 0) cycle
          line=adjustl(file_cfl%line(j)%Str_tmp)
          line=l_case(line)
          if (line(1:1) ==" ") cycle
          if (line(1:1) =="!") cycle
          i=index(line,"!")
          if( i /= 0) line=trim(line(1:i-1))

          select case (line(1:4))
          case ("opti")    !Optimization
           call Readn_Set_SubJob(line)

          case ("simu")     !Simulation
           call Readn_Set_SubJob(line)
           stop
          end select
       end do
    End Subroutine Readn_Set_Job

!******************************************!
    Subroutine Readn_Set_SubJob(line)
!******************************************!
       !---- Local variables ----!
       character(len=132)   :: line
       integer              :: i,iset,i1,i2

       i=0; i1=0; i2=0

       i=index(line,"f2mag")
       if(i /= 0) then

         i1=index(line,"f2mag_cryopad")
         if(i1 /= 0) then
            do iset=1,Nset
              if(Multidata%SNP(iset)) then
                if(allocated(PolariMultilist%Polarilist)) deallocate(PolariMultilist%Polarilist)
                allocate(PolariMultilist%Polarilist(iset))
                call Calc_Polar_Dom_Data(iset,lun)
              end if
              if(Multidata%f2mag(iset)) call Calc_sqMiV_Data(iset,lun)
            end do
         end if

         i2=index(line,"f2mag_mupad")
         if(i2 /= 0) then
           do iset=1,Nset
             if(Multidata%SNP(iset)) then
                if(allocated(PolariMultisVslist%PolarisVslist)) deallocate(PolariMultisVslist%PolarisVslist)
                allocate(PolariMultisVslist%PolarisVslist(iset))
                call Calc_Polar_CrSec_Data(iset,lun)
             end if
             if(Multidata%f2mag(iset)) call Calc_sqMiV_Data(iset,lun)
           end do
         end if

         if(i1 == 0 .and. i2 == 0) then
           do iset=1,Nset
             if(Multidata%f2mag(iset) .and. .not. Multidata%f2mn(iset)) call Calc_sqMiV_Data(iset,lun)
           end do
         end if

       else

         i1=index(line,"cryopad")
         if(i1 /= 0) then
           do iset=1,Nset
             if(Multidata%SNP(iset)) then
               if(allocated(PolariMultilist%Polarilist)) deallocate(PolariMultilist%Polarilist)
               allocate(PolariMultilist%Polarilist(iset))
               call Calc_Polar_Dom_Data(iset,lun)
             end if
           end do
         end if

         i2=index(line,"mupad")
         if(i2 /= 0) then
           do iset=1,Nset
             if(Multidata%SNP(iset)) then
               if(allocated(PolariMultisVslist%PolarisVslist)) deallocate(PolariMultisVslist%PolarisVslist)
               allocate(PolariMultisVslist%PolarisVslist(iset))
               call Calc_Polar_CrSec_Data(iset,lun)
             end if
           end do
         end if

       end if

    End Subroutine Readn_Set_SubJob

!******************************************!
    Subroutine Calc_sqMiV_Data(iset,lun)
!******************************************!
! gets magn Str_tmp Factor and Miv from Calc_Magnetic_StrF_MiV
! lun is used only at 0-cycle !!
       !---- Argument ----!
       integer, optional,           intent(in) :: iset,lun

       !---- Local variables ----!
       integer                     :: j

       Nf2=MhMultilist%MhList(iset)%Nref
       Mag_Dom=Multidata%MagDom(iset)

       do j=1,Nf2 ! This is loop over reflections with Int

         !Calculate magnetic structure factor and magnetic interaction vector
         ! as mode='Car' MiV w.r.t. cartesian crystallographic frame
         Mh=MhMultilist%MhList(iset)%Mh(j)
         call Calc_Magnetic_StrF_MiV_Dom(Cell,MGp,mA,Mag_Dom,Mh) !CFML_Msfac, sums domains

         !---- without mode components with respect to direct cell system {e1,e2,e3}
         !---- with mode components with respect to the cartesian frame
         MhMultilist%MhList(iset)%Mh(j)%sqMiV=Mh%sqMiV

       end do !end loop over reflections

       !---  Cost_sqMiV(cost,Scalef)
       if (present(lun)) then
         if(any(Multidata%f2nuc)) then

         else
          Scalef=sum( [(MhMultilist%MhList(iset)%Mh(j)%sqMiV*Oblist%Ob(j)%Gobs *Oblist%Ob(j)%wGobs,j=1,Nf2)])/ &
                sum( [(MhMultilist%MhList(iset)%Mh(j)%sqMiV**2 *Oblist%Ob(j)%wGobs,j=1,Nf2)] )

         end if

         cost=sum( ([(Oblist%Ob(j)%wGobs* (Oblist%Ob(j)%Gobs-Scalef*MhMultilist%MhList(iset)%Mh(j)%sqMiV)**2, &
                                                                                 j=1,Nf2)]) ) /Nf2 !-NP_Refi)
         write(unit=lun,fmt="(/,a)")    "================================================================================"
         write(unit=lun,fmt="(a,f12.0)") 'Initial partial Cost(F2mag): Sum({|IObs-Scale*ICalc|/Sigma}^2)) / Nobs =',cost
         write(unit=lun,fmt="(a,/)")    "================================================================================"
         write(lun,'(a,i2,1x,a)') 'iset= ',iset, MultiData%datfile(iset)
         write(lun,'(a)') '     h      k       l          Imag        sImag      Icalc'

         do j=1,Nf2 ! This is loop over reflections with f2mag
          write(unit=lun,fmt="(3f8.4, 3f12.4)") Oblist%Ob(j)%Hd(:,1),&
                                        Oblist%Ob(j)%Gobs,Oblist%Ob(j)%SGobs,MhMultilist%MhList(iset)%Mh(j)%sqMiV
         end do !end loop over reflections

       end if

    End Subroutine Calc_sqMiV_Data

!******************************************!
    Subroutine Calc_Polar_Dom_Data(iset,lun)
!******************************************!
! gets polarization matrices from Calc_Polar_Dom (via nc, mxyz,tc), domains are considered in Calc_Polar_Dom
! lun is used only at 0-cycle !!
      !---- Argument ----!
      integer, optional, intent(in) :: iset,lun

      !---- Local variables ----!
      integer                     :: j,k0(3)
      real(kind=cp)               :: Pin
      real(kind=cp),dimension(3)  :: SPV
      complex                     :: NSF
      logical                     :: ok
      character(len=80)           :: mess

      k0=[0.00,0.00,0.00]
      Npol=Multidata%Nobs(iset)
      Mag_dom=Multidata%MagDom(iset)

      if(allocated(PolariMultilist%Polarilist(iset)%Polari)) deallocate(PolariMultilist%Polarilist(iset)%Polari)
      allocate(PolariMultilist%Polarilist(iset)%Polari(Npol))
      PolariMultilist%Polarilist(iset)%Nref= Npol

      do j=1,Npol ! Loop over hkl observations
         NSF=cmplx(0.00,0.00)
         SPV=PolaroMultilist%Polarolist(iset)%Polaro(j)%SPV
         Pin=PolaroMultilist%Polarolist(iset)%Polaro(j)%P
         if(Multidata%MNset(iset) /= 0) NSF=cmplx(sqrt(MNOblist%Ob(j)%Gobs),0.00)
         !here iv=1
         Select Type (nr => Nhkl%Ref)
            Type is (SRefl_Type)
              if(all(abs(MGp%kvec(:,1)-k0(:)) < 0.00100)) NSF=cmplx(nr(j)%A,nr(j)%B)
         End Select
         !Calculate magnetic structure factor and magnetic interaction vector
         ! as mode='y' MiV w.r.t. cartesian crystallographic frame
         Mh=MhMultilist%MhList(iset)%Mh(j)
         call Calc_Magnetic_StrF_MiV_Dom(Cell,MGp,mA,Mag_Dom,Mh) !CFML_Msfac
         !call Calc_Polar_Dom(Cell,Mh%h,SPV,Pin,NSF,Mag_Dom,Mh,PolariMultilist%Polarilist(iset)%Polari(j)) !CFML_Polar
         call Calc_Polar_Dom(Cell,Mh%h,SPV,Pin,NSF,Mag_Dom,Mh,PolariMultilist%Polarilist(iset)%Polari(j),ok,mess)
         if(.not. ok) write(unit=*,fmt="(a)") " => "//trim(mess)
         !---- without mode components with respect to direct cell system {e1,e2,e3}
         !---- with mode components with respect to the cartesian frame
         MhMultilist%MhList(iset)%Mh(j)%sqMiV=Mh%sqMiV

      end do !end loop over hkl observations

      !--- Cost_cryopad(cost)
      if (present(lun)) then

        cost=0.0
        do j=1,MultiData%Nobs(iset) !loop over observations
          cost =  cost + sum(PolaroMultilist%Polarolist(iset)%Polaro(j)%woPij * &
                 ( (PolariMultilist%Polarilist(iset)%Polari(j)%Pij - &
                    PolaroMultilist%Polarolist(iset)%Polaro(j)%oPij)**2))
        end do !end loop over observations

        cost=cost/(9*MultiData%Nobs(iset)) !-NP_Refi)

        write(unit=lun,fmt="(/,a)")    "==========================================================================="
        write(unit=lun,fmt="(a,f12.0)") 'Initial partial-Cost(cryopad): Sum({|PObs-PCalc|/sigma}^2) / Nobs =',cost
        write(unit=lun,fmt="(a,/)")    "==========================================================================="
        write(lun,'(a,i2,1x,a)') 'iset= ',iset, MultiData%datfile(iset)
        write(lun,'(a)') '          Pobs                   Pcalc'

        do j=1,MultiData%Nobs(iset) !loop over observations
          write(unit=lun,fmt='(a,3(f10.6),a)')  "  (qH  qK  qL) = (",  PolaroMultilist%Polarolist(iset)%Polaro(j)%H," )"
          write(unit=lun,fmt='(2(1x,3f8.4))') PolaroMultilist%Polarolist(iset)%Polaro(j)%oPij(:,1), &
          PolariMultilist%Polarilist(iset)%Polari(j)%Pij(:,1)
          write(unit=lun,fmt='(2(1x,3f8.4))') PolaroMultilist%Polarolist(iset)%Polaro(j)%oPij(:,2), &
          PolariMultilist%Polarilist(iset)%Polari(j)%Pij(:,2)
          write(unit=lun,fmt='(2(1x,3f8.4))') PolaroMultilist%Polarolist(iset)%Polaro(j)%oPij(:,3), &
          PolariMultilist%Polarilist(iset)%Polari(j)%Pij(:,3)
        end do !end loop over observations

      end if

    End Subroutine Calc_Polar_Dom_Data

!******************************************!
    Subroutine Calc_Polar_CrSec_Data(iset,lun)
!******************************************!
! gets polarized cross sections from Calc_Polar_CrSec (via sVs)
!
      !---- Argument ----!
      integer, optional,           intent(in) :: iset,lun

      !---- Local variables ----!
      integer                     :: i,j,iobs,nd,ich,nch,k0(3)
      real(kind=cp)               :: Pin
      real(kind=cp),dimension(3)  :: SPV
      real(kind=cp),dimension(3,3):: Ipp,Ipm,Imp,Imm
      complex                     :: NSF

      Npol=Multidata%Nobs(iset)
      Mag_dom=Multidata%MagDom(iset)
      k0=[0.00,0.00,0.00]

      if(allocated(PolariMultisVslist%PolarisVslist(iset)%PolarisVs)) deallocate(PolariMultisVslist%PolarisVslist(iset)%PolarisVs)
      allocate(PolariMultisVslist%PolarisVslist(iset)%PolarisVs(Npol))
      PolariMultisVslist%PolarisVslist(iset)%Nref= Npol

      do iobs=1,Npol ! Loop over hkl observations
         NSF=cmplx(0.00,0.00)
         SPV=PolaroMultilist%Polarolist(iset)%Polaro(iobs)%SPV
         Pin=PolaroMultilist%Polarolist(iset)%Polaro(iobs)%P
         if(Multidata%MNset(iset) /= 0) NSF=cmplx(sqrt(MNOblist%Ob(iobs)%Gobs),0.00)
         !here iv=1
         Select Type (nr => Nhkl%Ref)
            Type is (SRefl_Type)
              if(all(abs(MGp%kvec(:,1)-k0(:)) < 0.001)) NSF=cmplx(nr(j)%A,nr(j)%B)
         End Select
         nch=1
         if(Mag_Dom%chir) nch=2

        do nd=1,Mag_Dom%nd !loop over S-domains
         do ich=1,nch !loop over chiral domains
           !Calculate magnetic structure factor and magnetic interaction vector
           ! as mode='y' MiV w.r.t. cartesian crystallographic frame
           Mh=MhMultilist%MhList(iset)%Mh(iobs)
           call Calc_Magnetic_StrF_MiV_Dom(Cell,MGp,mA,Mag_Dom,Mh) !CFML_Msfac
           call Calc_Polar_CrSec(Cell,Mh%h,SPV,Pin,NSF,Mag_Dom,Mh,Ipp,Ipm,Imp,Imm) !CFML_Polar
           !---- without mode components with respect to direct cell system {e1,e2,e3}
           !---- with mode components with respect to the cartesian frame
           MhMultilist%MhList(iset)%Mh(iobs)%sqMiV=Mh%sqMiV
          end do !loop over chiral domains
        end do !loop over S-domains

        if(Pin > 0.00) then
          do i=1,3
            do j=1,3
              if((Ipp(i,j)+Imp(i,j)) <= eps) then
                PolariMultisVslist%PolarisVslist(iset)%PolarisVs(iobs)%Pij(i,j) = 0.00
              else
                PolariMultisVslist%PolarisVslist(iset)%PolarisVs(iobs)%Pij(i,j) = &
                        (Ipp(i,j)-Imp(i,j))/(Ipp(i,j)+Imp(i,j))
              end if
            end do
          end do
        end if

        if(Pin < 0.00) then
          do i=1,3
            do j=1,3
              if((Imm(i,j)+Ipm(i,j)) <= eps) then
                PolariMultisVslist%PolarisVslist(iset)%PolarisVs(iobs)%Pij(i,j) = 0.00
              else
                PolariMultisVslist%PolarisVslist(iset)%PolarisVs(iobs)%Pij(i,j) = &
                          -(Imm(i,j)-Ipm(i,j))/(Imm(i,j)+Ipm(i,j))
              end if
            end do
          end do
        end if

      end do !end loop over hkl observations

      !--- Cost_mupad(cost)
      if (present(lun)) then

       cost=0.0
       do j=1,MultiData%Nobs(iset) !loop over observations
         cost =  cost + sum(PolaroMultilist%Polarolist(iset)%Polaro(j)%woPij * &
                 ( (PolariMultisVslist%PolarisVslist(iset)%PolarisVs(j)%Pij - &
                    PolaroMultilist%Polarolist(iset)%Polaro(j)%oPij)**2))
       end do !end loop over observations
       cost=cost/(9*MultiData%Nobs(iset)) !-NP_Refi)

        write(unit=lun,fmt="(/,a)")    "==========================================================================="
        write(unit=lun,fmt="(a,f12.0)") 'Initial partial-Cost(mupad): Sum({|PObs-PCalc|/sigma}^2) / Nobs =',cost
        write(unit=lun,fmt="(a,/)")    "==========================================================================="
        write(lun,'(a,i2,1x,a)') 'iset= ',iset, MultiData%datfile(iset)
        write(lun,'(a)') '          Pobs                   Pcalc'

        do j=1,MultiData%Nobs(iset) !loop over observations
         write(unit=lun,fmt='(a,3(f10.6),a)')  "  (qH  qK  qL) = (",  PolaroMultilist%Polarolist(iset)%Polaro(j)%H," )"
         write(unit=lun,fmt='(2(1x,3f8.4))') PolaroMultilist%Polarolist(iset)%Polaro(j)%oPij(:,1), &
         PolariMultisVslist%PolarisVslist(iset)%PolarisVs(j)%Pij(:,1)
         write(unit=lun,fmt='(2(1x,3f8.4))') PolaroMultilist%Polarolist(iset)%Polaro(j)%oPij(:,2), &
         PolariMultisVslist%Polarisvslist(iset)%PolarisVs(j)%Pij(:,2)
         write(unit=lun,fmt='(2(1x,3f8.4))') PolaroMultilist%Polarolist(iset)%Polaro(j)%oPij(:,3), &
         PolariMultisVslist%PolarisVslist(iset)%PolarisVs(j)%Pij(:,3)

        end do !end loop over observations

       end if

    End Subroutine Calc_Polar_CrSec_Data

!******************************************!
    !!----
    !!---- Subroutine Copy_mAtom_List(mA, mA_clone)
    !!----    type(mAtom_list_type),   intent(in)  :: mA      !  In -> Atom list
    !!----    type(mAtom_list_type),   intent(out) :: mA_clone!   Out -> Atom list
    !!----
    !!----    Subroutine to copy an atom list to another one
    !!----
    !!---- Created: January - 2012
    !!
!******************************************!
    Subroutine Copy_mAtom_List(mA, mA_clone)
!******************************************!
       !---- Arguments ----!
       type(mAtom_list_type),   intent(in)  :: mA
       type(mAtom_list_type),   intent(out) :: mA_clone
       !---- Local variables ----!
       integer                    :: n

       n=mA%natoms
       call Allocate_mAtom_List(n,mA_clone)
       mA_clone%atom(1:n)=mA%atom(1:n)
       return
    End Subroutine Copy_mAtom_List

!******************************************!
    !!----
    !!---- Subroutine Copy_MagDom_List(Mag_dom, Mag_dom_clone)
    !!----    type(mAtom_list_type),   intent(in)  :: Mag_dom      !  In -> Atom list
    !!----    type(mAtom_list_type),   intent(out) :: Mag_dom_clone!   Out -> Atom list
    !!----
    !!----    Subroutine to copy an mag domain list to another one
    !!----
    !!---- Created: January - 2012
    !!
!******************************************!
    Subroutine Copy_MagDom_List(Mag_dom, Mag_dom_clone)
!******************************************!
       !---- Arguments ----!
       type(Magnetic_Domain_type),   intent(in)  :: Mag_dom
       type(Magnetic_Domain_type),   intent(out) :: Mag_dom_clone
       !---- Local Variables ----!

       Mag_dom_clone%nd=Mag_dom%nd
       Mag_dom_clone%Chir=Mag_dom%Chir
       Mag_dom_clone%DMat=Mag_dom%DMat
       Mag_dom_clone%Pop=Mag_dom%Pop
       Mag_dom_clone%Mpop=Mag_dom%Mpop
       Mag_dom_clone%Lpop=Mag_dom%Lpop
       Mag_dom_clone%Lab=Mag_dom%Lab

       return
    End Subroutine Copy_MagDom_List

!******************************************!
    Subroutine Write_ObsCalc(file_cfl)
!******************************************!

      !---- Arguments ----!
      type(file_type), intent (in) :: file_cfl
      !---- Local variables ----!
      character(len=132)   :: line
      integer              :: i,j,n,iset,iobs,i1,i2

      i=0
      do j=1,file_cfl%nlines
        line=adjustl(file_cfl%line(j)%Str_tmp)
        line=l_case(line)
        if (line(1:1) ==" ") cycle
        if (line(1:1) =="!") cycle
        i=index(line,"!")

        if( i /= 0) line=trim(line(1:i-1))

        select case (line(1:4))
        case ("opti":"simu")    !Optimization and Simulation

        i=index(line,"f2mag")
         if(i /= 0) then
           i1=index(line,"f2mag_cryopad")
           if(i1 /= 0) then
             do iset=1,MultiData%Nset
                write(lun,'(a,i2,1x,a)') 'iset= ',iset, MultiData%datfile(iset)
                if(Multidata%SNP(iset)) then
                  write(lun,'(a)') '          Pobs                   Pcalc'
                  do iobs=1,MultiData%Nobs(iset) !loop over observations
                  write(unit=lun,fmt='(a,3(f10.6),a)') "  (qH  qK  qL) = (",PolaroMultilist%Polarolist(iset)%Polaro(iobs)%H," )"
                  write(unit=lun,fmt='(2(1x,3f8.4))') PolaroMultilist%Polarolist(iset)%Polaro(iobs)%oPij(:,1), &
                  PolariMultilist%Polarilist(iset)%Polari(iobs)%Pij(:,1)
                  write(unit=lun,fmt='(2(1x,3f8.4))') PolaroMultilist%Polarolist(iset)%Polaro(iobs)%oPij(:,2), &
                  PolariMultilist%Polarilist(iset)%Polari(iobs)%Pij(:,2)
                  write(unit=lun,fmt='(2(1x,3f8.4))') PolaroMultilist%Polarolist(iset)%Polaro(iobs)%oPij(:,3), &
                  PolariMultilist%Polarilist(iset)%Polari(iobs)%Pij(:,3)

                  end do !end loop over observations
                end if

                if(Multidata%f2mag(iset)) then
                  write(lun,'(a)') '     h      k       l          Imag        sImag      Icalc'
                  Nf2=MhMultilist%MhList(iset)%Nref
                  do n=1,Nf2 ! This is loop over reflections with f2mag
                    write(unit=lun,fmt="(3f8.4,3f12.4)") MhMultilist%MhList(iset)%Mh(n)%H,Oblist%Ob(n)%Gobs, &
                                    Oblist%Ob(n)%SGobs,Scalef*MhMultilist%MhList(iset)%Mh(n)%sqMiV
                  end do
                end if

                if(Multidata%f2nuc(iset)) then
                  write(lun,'(a)') '     h        k        l          Inuc        sInuc      Icalc'
                  Nf2=Nhkl%Nref
                  do n=1,Nf2 ! This is loop over reflections with f2nuc
                   Select Type (nr => Nhkl%Ref)
                      Type is (SRefl_Type)
                       write(unit=lun,fmt="(3(2x,i4,3x),3f12.4)") nr(n)%H,nr(n)%Fo, &
                                    nr(n)%sFo,Scalef*nr(n)%Fc
                   end select
                  end do
                end if

             end do
           end if

           i2=index(line,"f2mag_mupad")
           if(i2 /= 0) then
             do iset=1,MultiData%Nset
                write(lun,'(a,i2,1x,a)') 'iset= ',iset, MultiData%datfile(iset)

                if(Multidata%SNP(iset)) then
                  write(lun,'(a)') '          Pobs                   Pcalc'
                  do iobs=1,MultiData%Nobs(iset) !loop over observations
                    write(unit=lun,fmt='(a,3(f10.6),a)') "  (qH  qK  qL) = (",PolaroMultilist%Polarolist(iset)%Polaro(iobs)%H," )"
                    write(unit=lun,fmt='(2(1x,3f8.4))') PolaroMultilist%Polarolist(iset)%Polaro(iobs)%oPij(:,1), &
                    PolariMultisVslist%PolarisVslist(iset)%PolarisVs(iobs)%Pij(:,1)
                    write(unit=lun,fmt='(2(1x,3f8.4))') PolaroMultilist%Polarolist(iset)%Polaro(iobs)%oPij(:,2), &
                    PolariMultisVslist%Polarisvslist(iset)%PolarisVs(iobs)%Pij(:,2)
                    write(unit=lun,fmt='(2(1x,3f8.4))') PolaroMultilist%Polarolist(iset)%Polaro(iobs)%oPij(:,3), &
                    PolariMultisVslist%PolarisVslist(iset)%PolarisVs(iobs)%Pij(:,3)

                  end do !end loop over observations
                end if

                if(Multidata%f2mag(iset)) then
                  write(lun,'(a)') '     h      k       l          Imag        sImag      Icalc'
                  Nf2=MhMultilist%MhList(iset)%Nref
                  do n=1,Nf2 ! This is loop over reflections with f2mag
                    write(unit=lun,fmt="(3f8.4,3f12.4)") MhMultilist%MhList(iset)%Mh(n)%H,Oblist%Ob(n)%Gobs, &
                                    Oblist%Ob(n)%SGobs,Scalef*MhMultilist%MhList(iset)%Mh(n)%sqMiV
                  end do
                end if

                if(Multidata%f2nuc(iset)) then
                  write(lun,'(a)') '     h        k        l          Inuc        sInuc      Icalc'
                  Nf2=Nhkl%Nref
                  do n=1,Nf2 ! This is loop over reflections with f2nuc
                   Select Type (nr => Nhkl%Ref)
                      Type is (SRefl_Type)
                        write(unit=lun,fmt="(3(2x,i4,3x),3f12.4)") nr(n)%H,nr(n)%Fo, &
                                    nr(n)%sFo,Scalef*nr(n)%Fc
                   end select
                  end do
                end if

             end do
           end if

           if(i1 == 0 .and. i2 == 0) then !f2mag pure case
             do iset=1,MultiData%Nset

               if(Multidata%f2mag(iset)) then
                 write(lun,'(a,i2,1x,a)') 'iset= ',iset, MultiData%datfile(iset)
                 write(lun,'(a)') '     h      k       l          Imag        sImag      Icalc'
                 Nf2=MhMultilist%MhList(iset)%Nref
                 do n=1,Nf2 ! This is loop over reflections with f2mag
                   write(unit=lun,fmt="(3f8.4,3f12.4)") MhMultilist%MhList(iset)%Mh(n)%h,Oblist%Ob(n)%Gobs, &
                                    Oblist%Ob(n)%SGobs,Scalef*MhMultilist%MhList(iset)%Mh(n)%sqMiV
                 end do
               end if

               if(Multidata%f2nuc(iset)) then
                 write(lun,'(a)') '     h        k        l          Inuc        sInuc      Icalc'
                 Nf2=Nhkl%Nref
                 do n=1,Nf2 ! This is loop over reflections with f2nuc
                   Select Type (nr => Nhkl%Ref)
                      Type is (SRefl_Type)
                       write(unit=lun,fmt="(3(2x,i4,3x),3f12.4)") nr(n)%H,nr(n)%Fo, &
                                    nr(n)%sFo,Scalef*nr(n)%Fc
                   end select
                 end do
               end if

             end do
           end if

         else

           i1=index(line,"cryopad")
           if(i1 /= 0) then
             do iset=1,MultiData%Nset
                if(Multidata%SNP(iset)) then
                  write(lun,'(a,i2,1x,a)') 'iset= ',iset, MultiData%datfile(iset)
                  write(lun,'(a)') '          Pobs                   Pcalc'
                  do iobs=1,MultiData%Nobs(iset) !loop over observations

                    write(unit=lun,fmt='(a,3(f10.6),a)') "  (qH  qK  qL) = (",PolaroMultilist%Polarolist(iset)%Polaro(iobs)%H," )"
                    write(unit=lun,fmt='(3(f10.6))')                             PolaroMultilist%Polarolist(iset)%Polaro(iobs)%H
                    write(unit=lun,fmt='(2(1x,3f8.4))') PolaroMultilist%Polarolist(iset)%Polaro(iobs)%oPij(:,1), &
                    PolariMultilist%Polarilist(iset)%Polari(iobs)%Pij(:,1)
                    write(unit=lun,fmt='(2(1x,3f8.4))') PolaroMultilist%Polarolist(iset)%Polaro(iobs)%oPij(:,2), &
                    PolariMultilist%Polarilist(iset)%Polari(iobs)%Pij(:,2)
                    write(unit=lun,fmt='(2(1x,3f8.4))') PolaroMultilist%Polarolist(iset)%Polaro(iobs)%oPij(:,3), &
                    PolariMultilist%Polarilist(iset)%Polari(iobs)%Pij(:,3)

                  end do !end loop over observations
                end if
             end do
           end if

           i2=index(line,"mupad")
           if(i2 /= 0) then
             do iset=1,MultiData%Nset
                if(Multidata%SNP(iset)) then
                  write(lun,'(a,i2,1x,a)') 'iset= ',iset, MultiData%datfile(iset)
                  write(lun,'(a)') '          Pobs                   Pcalc'
                  do iobs=1,MultiData%Nobs(iset) !loop over observations

                     write(unit=lun,fmt='(a,3(f10.6),a)') "  (qH  qK  qL) = (",PolaroMultilist%Polarolist(iset)%Polaro(iobs)%H," )"
                     write(unit=lun,fmt='(2(1x,3f8.4))') PolaroMultilist%Polarolist(iset)%Polaro(iobs)%oPij(:,1), &
                     PolariMultisVslist%PolarisVslist(iset)%PolarisVs(iobs)%Pij(:,1)
                     write(unit=lun,fmt='(2(1x,3f8.4))') PolaroMultilist%Polarolist(iset)%Polaro(iobs)%oPij(:,2), &
                     PolariMultisVslist%Polarisvslist(iset)%PolarisVs(iobs)%Pij(:,2)
                     write(unit=lun,fmt='(2(1x,3f8.4))') PolaroMultilist%Polarolist(iset)%Polaro(iobs)%oPij(:,3), &
                     PolariMultisVslist%PolarisVslist(iset)%PolarisVs(iobs)%Pij(:,3)

                  end do !end loop over observations
                end if
             end do
            end if
         end if

        end select
        end do

   End Subroutine Write_ObsCalc

!******************************************!
    Subroutine MagDom_to_Dataset(Mag_Dom)
!******************************************!
    !!--++ writes Mag_Dom (read from *cfl file or updated by VState_to_AtomsPar after the SA cycle)
    !!--++ into Multidata%MagDom(iset)
    !!--++ Created: February - 2012
    !!--++ Works when both chiral domains given

     !---- Arguments ----!
     Type(Magnetic_Domain_type),   intent(in out)  :: Mag_Dom

     !---- Local variables ----!
     integer              :: iset,i1,j1,i2,j2,idom,dpast,ndomset
     integer              :: place(2)
     real                 :: domsum
     character(len=10)    :: lab

     !----If not all domains are given
     if(Sum(Multidata%MagDom(1:Nset)%nd) /= Mag_dom%nd) then
       write(unit=*,fmt="(a)") " => Mismatch between domains in Mag_Structure and Mag_data! "
       stop
     end if

     do iset=1,Nset

        do i1=1,Mag_Dom%nd
          do j1=1,2 !ich1
            lab=Mag_dom%lab(j1,i1)
            idom = 0

            dom:do i2=1,Multidata%MagDom(iset)%nd
            do j2=1,2 !ich2
              if(Multidata%MagDom(iset)%lab(j2,i2) == lab) then
                idom=1
                place=[j2,i2]
                exit dom
              end if
            end do
          end do dom

          if (idom == 1) then
            j2=place(1); i2=place(2)
            Multidata%MagDom(iset)%DMat(:,:,i2)=Mag_dom%DMat(:,:,i1)
            Multidata%MagDom(iset)%Chir=Mag_dom%Chir
            Multidata%MagDom(iset)%pop(j2,i2) =Mag_dom%pop(j1,i1)
            Multidata%MagDom(iset)%Mpop(j2,i2)=Mag_dom%Mpop(j1,i1)
            Multidata%MagDom(iset)%Lpop(j2,i2)=Mag_dom%Lpop(j1,i1)
          end if

          end do
        end do

     end do !on iset

     !---- Normalization to Sum_of_domains_onedataset=1
     dpast=0
     do iset=1,Nset
       ndomset=Multidata%MagDom(iset)%nd
       domsum=sum(Multidata%MagDom(iset)%pop)

       do i1=1,ndomset
         do j1=1,2 !ich1
           Multidata%MagDom(iset)%pop(j1,i1)=Multidata%MagDom(iset)%pop(j1,i1)/domsum
           Mag_dom%pop(j1,(i1+dpast))=Multidata%MagDom(iset)%pop(j1,i1)
         end do
       end do
       dpast=dpast+ndomset
     end do !on iset

   End Subroutine MagDom_to_Dataset

 End module prep_input