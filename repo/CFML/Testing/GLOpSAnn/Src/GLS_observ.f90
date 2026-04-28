  Module GLS_observ
    Use CFML_GlobalDeps,                 only: sp,cp,dp
    Use CFML_Maths,                      only: sort
    use CFML_Reflections,                only: RefList_Type, h_mult, h_equiv,h_s, Initialize_RefList, &
                                               SRefl_Type
    use CFML_Strings,                    only: number_lines, u_Case
    use CFML_gSpaceGroups,               only: SPG_Type
    use CFML_Metrics,                    only: Cell_G_Type

    implicit none

    private

    public :: Read_observations, Write_ObsCalc_SFactors, Write_FoFc_Powder, Read_Profile_Type


    logical,             public ::    err_observ=.false.
    character (len=256), public ::    err_mess_observ="  "
    Real(kind=cp), save, public ::    SumGobs, ScaleFact=1.0, wavel_int
    Integer,             public ::    Nselr=0, iwgt=0
    Logical,             public ::    Weight_Sim=.false.
    logical,             public ::    fullprof_int=.false.

    !!----
    !!---- TYPE :: OBSERVATION_TYPE
    !!--..
    !!---- Type, public :: Observation_Type
    !!----    integer                 :: ncont ! number of contributing reflections
    !!-----   integer,dimension(3,12) :: Hd    ! Indices of contributing reflections
    !!-----   integer,dimension(  12) :: icod  ! Pointer to the number of domain
    !!-----   integer,dimension(  12) :: p     ! Pointer to the list of independent reflections
    !!----    real(kind=sp)           :: Gobs  ! Observed Structure Factor squared
    !!----    real(kind=sp)           :: SGobs ! Sigma of  Gobs
    !!---- End Type Observation_Type
    !!----
    !!---- Update: April - 2005
    !!
    Type, public :: Observation_Type
       integer                            :: ncont ! number of contributing reflections
       integer,allocatable,dimension(:,:) :: Hd    ! (3,ncont)Indices of contributing reflections
       integer,allocatable,dimension(:)   :: icod  ! (ncont)  Pointer to the number of domain
       integer,allocatable,dimension(:)   :: p     ! (ncont)  Pointer to the list of independent reflections
       real(kind=sp)                      :: Gobs  ! Observed Structure Factor squared
       real(kind=sp)                      :: SGobs ! Sigma of  Gobs
    End Type Observation_Type

    !!----
    !!---- TYPE :: OBSERVATION_LIST_TYPE
    !!--..
    !!---- Type, public :: Observation_List_Type
    !!----    integer                                         :: Nobs  ! Number of Observations
    !!----    type(observation_type),allocatable,dimension(:) :: Ob    ! Observation (F2, etc...)
    !!---- End Type Observation_List_Type
    !!----
    !!---- Update: April - 2005
    !!
    Type, public :: Observation_List_Type
       integer                                         :: Nobs  ! Number of Observations
       type(observation_type),allocatable,dimension(:) :: Ob    ! Observation (F2, etc...)
    End Type Observation_List_Type

    Interface Read_observations
       Module Procedure Read_observations_clusters
       Module Procedure Read_observations_reflections
    End Interface Read_observations

    type, public :: profile_point_type
      real(kind=cp)                            :: yobs     !observed intensity
      real(kind=cp)                            :: ycal     !calculated intensity
      real(kind=cp)                            :: bac      !background level
      real(kind=cp)                            :: var      !variance of the intensity
      real(kind=cp)                            :: scv      !value of the scattering variable
      integer                                  :: nref     !number of reflections contributing to the point
      integer                                  :: ref_ini  !number of the first contributing reflection
      integer                                  :: ref_end  !number of the last contributing reflection
      real(kind=cp), dimension(:), allocatable :: omegap   !Profile contibutions of the reflections other than structure factor squared
    end type profile_point_type

    type, public :: profile_type
      integer          :: npts
      integer          :: nactive
      real(kind=cp)    :: sumap,meanvar
      character(len=6) :: scattvar
      type (profile_point_type), dimension(:), allocatable :: prf
    end type profile_type

    type(profile_type),            public :: sprof
     real,allocatable,dimension(:),public :: ttheta

    Contains

    !!----
    !!---- Subroutine Read_observations_reflections(file_hkl,Cell,Spg,Friedel,Rf)
    !!----   character(len=*),    intent (in)  :: file_hkl  !Name of the hkl-file
    !!----   type(Cell_G_Type),   intent (in)  :: Cell
    !!----   type(SPG_Type),      intent (in)  :: Spg
    !!----   logical,             intent (in)  :: Friedel
    !!----   type(RefList_Type),  intent (out) :: Rf
    !!----
    !!----   Subroutine for reading a list of independent reflections
    !!----   The unit cell and space group need to be set before calling this subroutine.
    !!----   Construct the RefList_Type variable "Rf", containing the experimental
    !!----   data and the value of hkl, s, etc. The reflections are reordered by increasing
    !!----   sinTheta/Lambda before return.
    !!----
    !!---- Update: April - 2005
    !!
    Subroutine Read_observations_reflections(file_hkl,Cell,Spg,Friedel,Rf)
      character(len=*),    intent (in)  :: file_hkl
      type(Cell_G_Type),   intent (in)  :: Cell
      type(SPG_Type),      intent (in)  :: Spg
      logical,             intent (in)  :: Friedel
      type(RefList_Type),  intent (out) :: Rf
      !--- Local Variables ---!
      integer          :: ier, nref, i,j,nlines,i_hkl, itemp
      real(kind=cp)    :: a1,a2,a3
      integer,       allocatable,dimension(:,:) :: hkl
      integer,       allocatable,dimension(  :) :: ic
      real(kind=cp), allocatable,dimension(  :) :: sv,fobs,sigma
      character(len=132) :: line



      open(newunit=i_hkl, file=trim(file_hkl), status="old", action="read",position="rewind",iostat=ier)
      if(ier /= 0) then
        err_observ=.true.
        err_mess_observ="  Error opening the file: "//trim(file_hkl)
        return
      end if
      Fullprof_int=.false.
      i=index(file_hkl,".int")
      if(i /= 0) Fullprof_int=.true.
      nlines= number_lines(trim(file_hkl))
      nref=nlines
      if(FullProf_int) nref=nlines-3
      !Allocate local types to the maximum possible value

      if(allocated(ttheta)) deallocate(ttheta)
      allocate(ttheta(nref))
      if(allocated(sv)) deallocate(sv)
      allocate(sv(nref))
      if(allocated(fobs)) deallocate(fobs)
      allocate(fobs(nref))
      if(allocated(sigma)) deallocate(sigma)
      allocate(sigma(nref))
      if(allocated(ic)) deallocate(ic)
      allocate(ic(nref))
      if(allocated(hkl)) deallocate(hkl)
      allocate(hkl(3,nref))

      if(FullProf_int) then
         read(unit=i_hkl,fmt="(a)", iostat=ier) line
         read(unit=i_hkl,fmt="(a)", iostat=ier) line
         read(unit=i_hkl,fmt=*) wavel_int
         do i=1,nref
             read(unit=i_hkl,fmt=*, iostat=ier) hkl(:,i),fobs(i),sigma(i),a1,a2,a3,ttheta(i)
             if(fobs(i) < 0.0) fobs(i)=0.00001
             if(ier /= 0) then
                nref=i-1
                exit
             end if
         end do
      else
         do i=1,nref
             read(unit=i_hkl,fmt=*, iostat=ier) hkl(:,i),fobs(i),sigma(i)
             if(ier /= 0) exit
         end do
      end if
      close(unit=i_hkl)


      if(.not. FullProf_int) then
         !Now ordering of all reflections
         do i=1,nref
          sv(i)=h_s(hkl(:,i),Cell)
         end do
         ic=sort(sv,nref) !use ic for pointer ordering

         open(newunit=itemp,status="scratch",form="unformatted",action="readwrite")
         do i=1,nref
           j=ic(i)
           write(itemp) hkl(:,j),sv(j),fobs(j),sigma(j)
         end do
         rewind(unit=itemp)
         do i=1,nref
           read(itemp) hkl(:,i),sv(i),fobs(i),sigma(i)
         end do
      else
         do i=1,nref
          sv(i)=sind(ttheta(i)*0.5)/wavel_int
         end do
      end if


      call Initialize_RefList(nref, Rf, "SRefl")
      SumGobs=0.0
      Select Type(R => Rf%Ref)
        type is (SRefl_Type)
          do i=1,nref
            SumGobs=SumGobs+abs(fobs(i))
            R(i)%h    = hkl(:,i)
            R(i)%Mult = h_mult(hkl(:,i),Spg,Friedel)
            R(i)%Fc   = 0.0
            R(i)%Fo   = fobs(i)  !This is integrated intensities in the case we read FullProf *.int files (all three components)
            R(i)%SFo  = sigma(i)
            R(i)%S    = sv(i)
            R(i)%W    = 0.0
            R(i)%Phase= 0.0
            R(i)%A    = 0.0
            R(i)%B    = 0.0
          end do
      End Select

    End Subroutine Read_observations_reflections


    !!----
    !!---- Subroutine Read_observations_clusters(file_hkl,Cell,Spg,Friedel,Obs,Rf)
    !!----   character(len=*),            intent (in)  :: file_hkl  !Name of the hkl-file
    !!----   type(Cell_G_Type),           intent (in)  :: Cell
    !!----   type(SPG_Type),              intent (in)  :: Spg
    !!----   logical,                     intent (in)  :: Friedel
    !!----   type(Observation_List_Type), intent (out) :: Obs
    !!----   type(RefList_Type),  intent (out) :: Rf
    !!----
    !!----   Subroutine for reading a list of observations (cluster of reflections)
    !!----   The unit cell and space group need to be set before calling this subroutine.
    !!----   Construct the Observation_List_Type variable "Obs", containing the experimental
    !!----   data and pointers to the strictly independent reflections that are gathered in
    !!----   the intent out RefList_Type variable "Rf". This last variable is suited
    !!----   for using the calculation of structure factors.
    !!----
    !!---- Update: April - 2005
    !!
    Subroutine Read_observations_clusters(file_hkl,Cell,Spg,Friedel,Obs,Rf)
      character(len=*),            intent (in)  :: file_hkl
      type(Cell_G_Type),           intent (in)  :: Cell
      type(SPG_Type),              intent (in)  :: Spg
      logical,                     intent (in)  :: Friedel
      type(Observation_List_Type), intent (out) :: Obs
      type(RefList_Type),          intent (out) :: Rf  !Independent reflections
      !--- Local Variables ---!
      integer :: ier, i,j,nlines,i_hkl,icod, nover, itemp, nri, iobs,nr,n
      integer,       dimension(3)               :: h
      integer,       allocatable,dimension(  :) :: ic
      integer,       allocatable,dimension(:,:) :: hov
      integer,       allocatable,dimension(:,:) :: hkl
      integer,       allocatable,dimension(:,:) :: point
      real(kind=cp), allocatable,dimension(  :) :: sv,sq
      real(kind=cp)                             :: fobs,sigma
      type(Observation_List_Type):: O


      open(newunit=i_hkl, file=trim(file_hkl), status="old", action="read",position="rewind",iostat=ier)
      if(ier /= 0) then
        err_observ=.true.
        err_mess_observ="  Error opening the file: "//trim(file_hkl)
        return
      end if
      nlines= number_lines(trim(file_hkl))

      !Allocate local types to the maximum possible value

      if(allocated(O%Ob)) deallocate(O%Ob)
      allocate(O%Ob(nlines))
      if(allocated(ic)) deallocate(ic)
      allocate(ic(nlines))
      if(allocated(sv)) deallocate(sv)
      allocate(sv(nlines))
      if(allocated(sq)) deallocate(sq)
      allocate(sq(nlines))
      if(allocated(hov)) deallocate(hov)
      allocate(hov(3,nlines))
      if(allocated(hkl)) deallocate(hkl)
      allocate(hkl(3,nlines))
      if(allocated(point)) deallocate(point)
      allocate(point(2,nlines))

      if(index(file_hkl,".int") /= 0 .or. index(file_hkl,".INT") /= 0) then
          fullprof_int=.true.
          read(unit=i_hkl,fmt=*)
          read(unit=i_hkl,fmt=*)
          read(unit=i_hkl,fmt=*) wavel_int
      end if


      iobs=1
      nr=0
      do_lines:do i=1,nlines
        nover=0                       !updated for each new observation (positive Fobs)
        do
          read(unit=i_hkl,fmt=*, iostat=ier) h,fobs,sigma,icod
          if(ier /= 0) exit do_lines
          nr          = nr+1
          nover       = nover+1
          point(1,nr) = iobs
          point(2,nr) = nover
          hov(:,nover)= h
          hkl(:,nr)   = h
          ic(nover)   = icod

          if(fobs > 0 ) then
            O%Ob(iobs)%Gobs  = fobs   !total corrected intensity
            O%Ob(iobs)%SGobs = sigma  !sigma of corrected intensity
            O%Ob(iobs)%ncont = nover  !number of contribution to the current observation
            allocate(O%Ob(iobs)%hd(3,nover),O%Ob(iobs)%icod(nover))
            do j=1,nover
              O%Ob(iobs)%hd(:,j) = hov(:,j)
              O%Ob(iobs)%icod(j) = ic(j)
            end do
            iobs=iobs+1
            exit
          end if

        end do

      end do do_lines


      iobs=iobs-1
     close(unit=i_hkl)

      Obs%Nobs=iobs
      if(allocated(Obs%Ob)) deallocate(Obs%Ob)
      allocate(Obs%Ob(iobs))

      SumGobs=0.0
      do i=1,iobs
        SumGobs=SumGobs+O%Ob(i)%Gobs
        Obs%Ob(i)%Gobs =O%Ob(i)%Gobs
        Obs%Ob(i)%SGobs=O%Ob(i)%SGobs
        Obs%Ob(i)%ncont=O%Ob(i)%ncont
        n=O%Ob(i)%ncont
        allocate(Obs%Ob(i)%hd(3,n))
        allocate(Obs%Ob(i)%icod(n))
        allocate(Obs%Ob(i)%p(n))
        do j=1,n
         Obs%Ob(i)%hd(:,j)=O%Ob(i)%hd(:,j)
         Obs%Ob(i)%icod(j)=O%Ob(i)%icod(j)
        end do
      end do

      !Now ordering of all reflections read and put the equivalents
      do i=1,nr
       sv(i)=h_s(hkl(:,i),Cell)
      end do
      ic=0
      ic=sort(sv,nr) !use ic for pointer ordering

      open(newunit=itemp,status="scratch",form="unformatted",action="readwrite")
      do i=1,nr
        j=ic(i)
        write(itemp) hkl(:,j),sv(j),point(:,j)
      end do
      rewind(unit=itemp)
      do i=1,nr
        read(itemp) hkl(:,i),sv(i),point(:,i)
      end do
      close(unit=itemp)

      ic=0 !nullify these vector for another use
      hov=0
      i=0
     nri=0
      do
        i=i+1
        if(i >= nr) exit
        iobs = point(1,i)
        nover= point(2,i)
        if(ic(i) == 0) then
          ic(i) = 1
          Obs%Ob(iobs)%p(nover)=i
          nri=nri+1
          hov(:,nri) = hkl(:,i)
           sq(  nri) = sv(i)
        end if

        do j=i+1,nr
          iobs = point(1,j)
          nover= point(2,j)
          if( h_equiv(hkl(:,j),hkl(:,i),Spg,Friedel) ) then
            Obs%Ob(iobs)%p(nover)=i
            ic(j)=1
          else
            Obs%Ob(iobs)%p(nover)=j
            ic(j)=1
            i=j
            nri=nri+1
            hov(:,nri) = hkl(:,j)
            sq(  nri) = sv(j)
            exit
          end if
        end do

      end do

      call Initialize_RefList(nri, Rf, "SRefl")
      Select Type(R => Rf%Ref)
        Type is (SRefl_Type)
            do i=1,nri
              R(i)%h    = hov(:,i)
              R(i)%Mult = h_mult(hov(:,i),Spg,Friedel)
              R(i)%Fo   = 0.0
              R(i)%Fc   = 0.0
              R(i)%SFo  = 0.0
              R(i)%S    = sq(i)
              R(i)%W    = 0.0
              R(i)%Phase= 0.0
              R(i)%A    = 0.0
              R(i)%B    = 0.0
            end do
      End Select

     ! open(newunit=itemp,file="debug_Observ.hkl",status="replace",action="write")
     !
     ! do i=1,Obs%Nobs
     !   n=O%Ob(i)%ncont
     !   do j=1,n
     !     k=Obs%Ob(i)%p(j)
     !     write(unit=itemp,fmt="(3i4,2i6,6x,3i4)") Obs%Ob(i)%hd(:,j), Obs%Ob(i)%icod(j),k, Rf%Ref(k)%h
     !   end do
     !   write(unit=itemp,fmt="(i8,2f14.4)") i, Obs%Ob(i)%Gobs, Obs%Ob(i)%SGobs
     ! end do
     ! call flush(itemp)
     ! close(unit=itemp)
    End Subroutine Read_observations_clusters
    !!----
    !!---- Subroutine Write_ObsCalc_SFactors(lun,Reflex,Mode)
    !!----    integer,                    intent(in) :: lun
    !!----    type(RefList_Type),         intent(in) :: Reflex
    !!----    Character(len=*), optional, intent(in) :: Mode
    !!----
    !!----    Writes in logical unit=lun the list of observed versus
    !!----    calculated structure factors contained in the array hkl
    !!----
    !!---- Update: April - 2005
    !!
    Subroutine Write_ObsCalc_SFactors(lun,Reflex,Mode)
       !---- Argument ----!
       integer,                    intent(in) :: lun
       type(RefList_Type),         intent(in) :: Reflex
       Character(len=*), optional, intent(in) :: Mode
       !---- Local Variables ----!
       integer       :: i,ir
       real(kind=cp) :: delta, Rfact

       ir=1
       If(present(mode)) then
         Select Case (mode(1:3))
           Case("NUC","nuc")
             write(unit=lun,fmt="(/,/,a)") "    LIST OF REFLECTIONS AND STRUCTURE FACTORS(NEUTRONS)"
             write(unit=lun,fmt="(a)")     "    ==================================================="
             ir=2
           Case default
             write(unit=lun,fmt="(/,/,a)") "    LIST OF REFLECTIONS AND STRUCTURE FACTORS(X-RAYS)"
             write(unit=lun,fmt="(a)")     "    ================================================="
         End Select
       else
         write(unit=lun,fmt="(a)")   "    LIST OF REFLECTIONS AND STRUCTURE FACTORS(X-RAYS)"
         write(unit=lun,fmt="(a)")   "    ================================================="
       end if

       write(unit=lun,fmt="(/,a,/)") "   H   K   L   Mult  SinTh/Lda    |Fobs|      SFobs        |Fc|       Delta     Delta/Sigma"
       Rfact=0.0
       Select Type(R => reflex%ref)
         Type is(SRefl_Type)
          do i=1,reflex%Nref
           delta=R(i)%Fo(ir)-ScaleFact*R(i)%Fc(ir)
           Rfact=Rfact+abs(delta)
           write(unit=lun,fmt="(3i4,i5,6f12.5)") R(i)%h, R(i)%mult,     &
                 R(i)%S, R(i)%Fo(ir)/ScaleFact,R(i)%SFo(ir), R(i)%Fc(ir),   &
                 delta, delta/max(0.0001,R(i)%SFo(ir))
          end do
          Rfact=100.0*Rfact/sum(R(1:reflex%Nref)%Fo(ir))
       End Select
       write(unit=lun,fmt="(a,f12.5)") "  =>  R-Factor(%) (Sum {|Fo-Fc|}/Sum{|Fo|} = ",Rfact

    End Subroutine Write_ObsCalc_SFactors

    !!----
    !!---- Subroutine Write_FoFc_Powder(lun,Oh,Reflex,Mode)
    !!----    integer,                            intent(in) :: lun
    !!----    type(Observation_List_Type),        intent(in) :: Oh
    !!----    type(RefList_Type),                 intent(in) :: Reflex
    !!----    Character(len=*), optional,         intent(in) :: Mode
    !!----
    !!----    Writes in logical unit=lun the list of observed versus
    !!----    calculated structure factors contained in the arrays Obs & Reflex
    !!----
    !!---- Update: April - 2005
    !!
    Subroutine Write_FoFc_Powder(lun,Oh,Reflex,Mode)
       !---- Argument ----!
       integer,                      intent(in) :: lun
       type(Observation_List_Type),  intent(in) :: Oh
       type(RefList_Type),           intent(in) :: Reflex
       Character(len=*), optional,   intent(in) :: Mode
       !---- Local Variables ----!
       integer       :: i,j,k,n,ir
       real(kind=cp) :: delta, Rfact, over, sumcal, suma, obs, sobs

       ir=1
       If(present(mode)) then
         Select Case (mode(1:3))
           Case("NUC","nuc")
             write(unit=lun,fmt="(/,/,a)") "    LIST OF OBSERVATIONS Gobs=Sum{|Fo|} AND STRUCTURE FACTORS(NEUTRONS)"
             write(unit=lun,fmt="(a)")     "    ==================================================================="
             ir=2
           Case default
             write(unit=lun,fmt="(/,/,a)") "    LIST OF OBSERVATIONS Gobs=Sum{|Fo|} AND STRUCTURE FACTORS(X-RAYS)"
             write(unit=lun,fmt="(a)")     "    ================================================================="
         End Select
       else
         write(unit=lun,fmt="(a)")   "    LIST OF OBSERVATIONS Gobs=Sum{|Fo|} AND STRUCTURE FACTORS(X-RAYS)"
         write(unit=lun,fmt="(a)")   "    ================================================================="
       end if

       write(unit=lun,fmt="(/,a,/)") "   H   K   L   Mult  SinTh/Lda  |Gobs|/Sc    SGobs/Sc      |Fc|       Delta    Delta/Sigma"

       n=reflex%Nref
       Select Type (R => reflex%ref)
         Type is (SRefl_Type)
           sumcal=sum(abs(R(1:n)%Fc(ir)))
       End Select
       ScaleFact=1.0
       if(sumcal > 0.0000001) ScaleFact=SumGobs/sumcal
       Rfact=0.0
       suma=0.0
       Select Type (R => reflex%ref)
         Type is (SRefl_Type)
           do i=1,Oh%Nobs
             over=0.0
             k=Oh%Ob(i)%ncont
             do j=1,k
               n=Oh%Ob(i)%p(j)
               over=over+R(n)%Fc(ir)
               write(unit=lun,fmt="(3i4,i5,f12.5,tr24,f12.5)") R(n)%h, R(n)%mult, R(n)%S, R(n)%Fc(ir)
             end do
             obs=Oh%Ob(i)%Gobs/ScaleFact
             sobs=Oh%Ob(i)%SGobs/ScaleFact
             delta=obs-over
             write(unit=lun,fmt="(tr29,5f12.5)") obs,sobs, over, delta, delta/max(0.0001,sobs)
             suma=suma+obs
             Rfact=Rfact+abs(delta)
           end do
       End Select
       Rfact=100.0*Rfact/Suma
       write(unit=lun,fmt="(a,f12.5)") "  =>  R-Factor(%) (Sum {|Gobs-Sum{|Fc|}}/Sum{Gobs} = ",Rfact

    End Subroutine Write_FoFc_Powder

    Subroutine Read_Profile_Type(fileprof)
      character(len=*), intent(in) :: fileprof
      !-----
      integer :: i,lun,j,jk,k,jj,ier,i1,i2,nr,nmr,nlines
      character(len=180) :: line,filen

      i=index(fileprof,".",back=.true.)
      if(i == 0) then
         filen=trim(fileprof)//".spr"
      else
         filen=trim(fileprof)
      end if
      open(newunit=lun, file=trim(filen), status="old", action="read", position="rewind",iostat=ier)
      if(ier /= 0) then
          err_observ=.true.
          err_mess_observ=" Error reading the profile intensity file: "//trim(filen)
          return
      end if
      read(unit=lun,fmt="(a)") line
      read(unit=lun,fmt="(a)") line
      read(unit=line,fmt=*) nr, sprof%nactive
      if(Nselr == 0) Nselr = sprof%nactive  !In case no limit of reflections are given
      !Check that sprof%nactive is compatible with "nselr" (number of selected reflections)
      sprof%npts=nr
      i=index(line," ",back=.true.)
      sprof%scattvar=adjustl(line(i:))
      if(allocated(sprof%prf)) deallocate(sprof%prf)
      allocate(sprof%prf(nr))
      read(unit=lun,fmt="(a)") line  !reading the two comments
      read(unit=lun,fmt="(a)") line
      sprof%sumap=0.0
      sprof%meanvar=0.0
      do i=1,nr
        read(unit=lun,fmt=*,iostat=ier) j,j,i1,i2,sprof%prf(i)%yobs,sprof%prf(i)%var, &
                                        sprof%prf(i)%ycal,sprof%prf(i)%bac,sprof%prf(i)%scv
        k=i2-i1+1
        sprof%meanvar = sprof%meanvar +  sprof%prf(i)%var
        if(allocated(sprof%prf(i)%omegap) ) deallocate(sprof%prf(i)%omegap)
        allocate(sprof%prf(i)%omegap(k))
        sprof%prf(i)%omegap(:)=0.0
        sprof%prf(i)%nref=k

        sprof%prf(i)%ref_ini=i1
        sprof%prf(i)%ref_end=i2

        !Check that sprof%nactive is compatible with "nselr" (number of selected reflections)
         if(i2 > Nselr) then
           sprof%npts=i-1
           exit
         else
           sprof%sumap=sprof%sumap+sprof%prf(i)%yobs    !replaces commented line above
         end if                                         !
        jk=0
        if(mod(k,8) == 0) then
           nlines=k/8
        else
           nlines=k/8+1
        end if
        DO jj=1,nlines
          nmr=MIN(8,k-jk)
          read(unit=lun,fmt="(24x,8f12.4)") (sprof%prf(i)%omegap(jk+j),j=1,nmr)
          jk=jk+8
        END DO
      end do
      sprof%meanvar=sprof%meanvar/real(nr)
      close(unit=lun)
      !The object sprof has been constructed completely on return

    End Subroutine read_profile_type

  End Module GLS_observ
