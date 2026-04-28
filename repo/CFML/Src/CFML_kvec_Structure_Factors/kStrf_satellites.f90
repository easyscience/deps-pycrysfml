SubModule (CFML_kvec_Structure_Factors) kStrf_satellites
   implicit none
   Contains
    !!----
    !!---- Module Subroutine Gen_Satellites(Cell,Grp,Smax,H,Ord,Powder,hkl)
    !!----    type(Cell_G_Type),               intent(in)     :: cell
    !!----    type(MagSymm_k_Type),                  intent(in)     :: Grp
    !!----    real(kind=cp),                         intent(in)     :: smax
    !!----    type(MagH_List_Type),                  intent(in out) :: H
    !!----    logical, optional,                     intent(in)     :: ord
    !!----    logical, optional,                     intent(in)     :: powder
    !!----    type (Reflection_List_Type), optional, intent(in)     :: hkl
    !!----
    !!----    Generates half reciprocal sphere of integer reflections and
    !!----    add satellites according to the information given in Grp.
    !!----    If Ord is given the reflections are reordered
    !!----    by increasing sinTheta/Lambda.
    !!----    If Powder is given, the unique reflections for a powder pattern
    !!----    are generated. The extinctions are obtained from a calculation of
    !!----    a random magnetic structure respecting the symmetry provided in Grp.
    !!----    If hkl is provided, the call to HKL_GEN is avoided.
    !!----    The subroutine constructs partially the object H.
    !!----
    !!---- Created:    April - 2005
    !!---- Updated: December - 2011, November 2014 (JRC)
    !!
    Module Subroutine Gen_Satellites(Cell,Grp,Smax,H,Ord,Powder,hkl)
       !---- Arguments ----!
       type(Cell_G_Type),                intent(in)     :: cell
       type(MagSymm_k_Type),             intent(in)     :: Grp
       real(kind=cp),                    intent(in)     :: smax
       type(MagH_List_Type),             intent(in out) :: H
       logical, optional,                intent(in)     :: ord
       logical, optional,                intent(in)     :: powder
       class (RefList_Type), optional,   intent(in)     :: hkl

       !---- Local variables ----!
       integer                                       :: i,j,k, numref,num_ref,n, ng, lu, addk,nmat,ik
       real(kind=cp)                                 :: smgen, maxr, epv,s1,s2,sqmiv1,sqmiv2, epm
       class(SPG_Type), allocatable                  :: G
       type(Reflist_Type)                            :: reflex
       type(MagH_List_Type)                          :: Hloc
       character(len=20)                             :: Symb
       real(kind=cp),dimension(3)                    :: kv,hr,ks
       logical                                       :: keq
       integer,            dimension(:), allocatable :: ind
       logical,            dimension(:), allocatable :: treated
       type(Matom_list_type)                         :: Am   !useful only the the powder case
       character(len=*),parameter, dimension(8)      :: label=(/"Ho","Er","Gd","Dy","Mn","Fe","Co","Ni"/)
       character(len=*),parameter, dimension(8)      :: sfac= (/"JHO3","JER3","MGD3","JDY3","MMN3","MFE3","MCO2","MNI2"/)

       epv=0.0001
       epm=0.01
       Symb=Grp%latt//" -1"
       Call set_spacegroup(symb,G)

       ! Determine the higher reciprocal cell parameter to add it to the given smax
       maxr=maxval(Cell%rcell)
       smgen=smax+maxr  !generate reflections up to smgen
       if(present(hkl)) then
         numref=hkl%nref
       else
         numref= Get_MaxNumRef(smgen, Cell%Vol)
       end if

       if(present(hkl)) then
         num_ref=numref
         call Initialize_RefList(Num_ref, reflex, 'SRefl', G%d-1)
         do i=1,num_ref
          reflex%Ref(i)%h=hkl%Ref(i)%h
          reflex%Ref(i)%s=hkl%Ref(i)%s
          reflex%Ref(i)%Mult=hkl%Ref(i)%Mult
         end do
       else
         call Hkl_Gen_Sxtal(Cell,G,0.0_cp,smgen,Reflex)
       end if

       !calculate the total number of satellites
       n=Grp%nkv
       ng=0
       do i=1,n
          kv=Grp%kvec(:,i)
          ng=ng+1
          if( .not. K_Equiv_Minus_K(kv,Grp%latt) ) ng=ng+1
       end do

       !Adding the 000 satellites is an additional 000 reflection, allocate also
       !the reflections of the form (-h,k,0) + kvect with h<=1 that have to be explicitly generated
       !when kvect is equivalent to -kvect
       addk = 2*(nint(smgen/Cell%rcell(2)) + 1)
       Hloc%nref= (num_ref+1+addk) * ng

       if(allocated(Hloc%mh)) deallocate(Hloc%mh)
       allocate(Hloc%mh(Hloc%nref))

       ng=0
       !Generate the 000 satellites, except for k=(000)
       do i=1,n
          kv=Grp%kvec(:,i)
          ng=ng+1
          keq = K_Equiv_Minus_K(kv,Grp%latt)
          hloc%Mh(ng)%keqv_minus= keq
          hloc%Mh(ng)%num_k= i
          hloc%Mh(ng)%signp=-1.0
          ks=kv
          hloc%Mh(ng)%s = h_s(ks,Cell)
          if (hloc%Mh(ng)%s < 0.0000001) then
             ng=ng-1
             cycle
          end if
          hloc%Mh(ng)%h=ks
       end do

       !rest of reflections
       outer: do j=1,num_ref
          hr=real(reflex%Ref(j)%h,kind=cp)
          do i=1,n
             kv=Grp%kvec(:,i)
             ng=ng+1
             if (ng > Hloc%nref) exit outer
             ks=hr+kv
             hloc%Mh(ng)%s = h_s(ks,Cell)
             if ( hloc%Mh(ng)%s > smax) then     !avoid reflection with s>smax
                ng=ng-1
                cycle
             end if
             keq = K_Equiv_Minus_K(kv,Grp%latt)
             hloc%Mh(ng)%keqv_minus= keq
             hloc%Mh(ng)%num_k= i
             hloc%Mh(ng)%signp=-1.0
             hloc%Mh(ng)%h=ks
             if ( .not. keq) then
                ng=ng+1
                if (ng > Hloc%nref) exit outer
                ks=hr-kv
                hloc%Mh(ng)%s = h_s(ks,Cell)
                if ( hloc%Mh(ng)%s > smax) then  !avoid reflection with s>smax
                   ng=ng-1
                   cycle
                end if
                hloc%Mh(ng)%keqv_minus= keq
                hloc%Mh(ng)%num_k= i
                hloc%Mh(ng)%signp= 1.0
                hloc%Mh(ng)%h=ks
             else if ( abs(hr(3)) < epv .and. abs(hr(1)) <= 1.0+epv) then   !complete with the reflections (-h,k,0)+kvec
                ng=ng+1
                if (ng > Hloc%nref) exit outer
                ks=(/-hr(1),-hr(2),0.0_cp/)+kv
                hloc%Mh(ng)%s = h_s(ks,Cell)
                if( hloc%Mh(ng)%s > smax) then     !avoid reflection with s>smax
                  ng=ng-1
                  cycle
                end if
                keq = K_Equiv_Minus_K(kv,Grp%latt)
                hloc%Mh(ng)%keqv_minus= keq
                hloc%Mh(ng)%num_k= i
                hloc%Mh(ng)%signp=-1.0
                hloc%Mh(ng)%h=ks
             end if
          end do
       end do outer

       Hloc%nref=ng  !update to the real number of effectively generated reflections

       if (present(powder)) then   !assumed ord=.true. even if not present

          !Generates a fictive magnetic structure with the provided symmetry to test the equivalent
          !reflections, get the real multiplicity and eliminate the systematic absences.
          if (allocated(ind)) deallocate(ind)
          allocate(ind(ng))
          if (allocated(treated)) deallocate(treated)
          allocate(treated(ng))

          treated=.false.
          ind= sort(hloc%Mh(:)%s,ng)

          ! Re-order the local reflections

          open(newunit=lu,status="scratch",form="unformatted",action="readwrite")
          do i=1,ng
             j=ind(i)
             write(unit=lu) hloc%Mh(j)%keqv_minus,hloc%Mh(j)%num_k,hloc%Mh(j)%signp,hloc%Mh(j)%s,hloc%Mh(j)%h
          end do
          rewind(unit=lu)

          do j=1,ng
             read(unit=lu)  hloc%Mh(j)%keqv_minus,hloc%Mh(j)%num_k,hloc%Mh(j)%signp,hloc%Mh(j)%s,hloc%Mh(j)%h
             hloc%Mh(j)%mult=2
          end do
          close(unit=lu)

          !Calculate the magnetic structure factors for an arbitrary structure of the same symmetry as that
          !provided. First construct a magnetic atom list with Ho+3 form factors
          nmat = Grp%nmsym
          Call Allocate_mAtom_list(nmat,Am)
          call RANDOM_SEED()
          do i=1,nmat
             Am%atom(i)%lab=label(i)       !Label
             Am%atom(i)%SfacSymb=sfac(i)   !Formfactor label
             call random_number(ks)
             Am%atom(i)%x= ks              !Fract. coord.
             Am%atom(i)%Biso=0.3           !Is. Temp. Fact.
             Am%atom(i)%occ=1.0            !occupation
             call random_number(maxr)
             Am%atom(i)%nvk= max(1,nint(maxr*n))
             do ik=1,n
                call random_number(maxr)
                Am%atom(i)%imat(ik)= max(1,nint(maxr*n))
                call random_number(ks)
                Am%atom(i)%Skr(:,ik)= ks(:)*8.0
                Am%atom(i)%Ski(:,ik)= 0.0
                call random_number(maxr)
                Am%atom(i)%mphas(ik)= maxr
             end do
          end do

          call Mag_Structure_Factors(Cell,Am,Grp,hloc)
          call Calc_Mag_Interaction_vector(hloc,cell)
          maxr=maxval(hloc%Mh(:)%sqMiV)
          epm=maxr*0.00001

          !Lines for debugging

          open(newunit=lu,file="powder_test.sfa",status="replace",action="write")
          call Write_Magnetic_Structure(lu,Grp,Am)
          call Write_Mag_Structure_Factors(lu,hloc,Grp)
          !End Lines for debugging
          !Start analysis
          ind(:) = 0

          do i=1,ng
             if(treated(i)) cycle
             sqmiv1= hloc%Mh(i)%sqMiV
             ind(i)=2
             s1= hloc%Mh(i)%s
             treated(i) = .true.
             if(sqmiv1 < epm ) ind(i)=0
             do j=i+1,ng
                s2= hloc%Mh(j)%s
                if( abs(s1-s2) > epv) exit
                sqmiv2= hloc%Mh(j)%sqMiV
                if( abs(sqmiv1-sqmiv2) > epm) exit
                !Passing here give an equivalent reflection
                ind(j) = 0
                if(sqmiv2 > epm) ind(i)=ind(i)+2
                treated(j)=.true.
             end do
          end do

          ! Determine the number of independent reflections
          k=0
          do i=1,ng
             if(ind(i) /= 0) k=k+1
          end do

          !Allocate the strictly needed magnetic reflections
          H%nref=k
          if(allocated(H%mh)) deallocate(H%mh)
          allocate(H%mh(k))
          k=0
          do j=1,ng
             if (ind(j) /= 0) then
                k=k+1
                h%Mh(k)%keqv_minus = hloc%Mh(j)%keqv_minus
                h%Mh(k)%num_k      = hloc%Mh(j)%num_k
                h%Mh(k)%signp      = hloc%Mh(j)%signp
                h%Mh(k)%s          = hloc%Mh(j)%s
                h%Mh(k)%h          = hloc%Mh(j)%h
                h%Mh(k)%mult       = ind(j)
                h%Mh(k)%sqMiV      = hloc%Mh(j)%sqMiV
                h%Mh(k)%MiV        = cmplx(0.0,0.0)
                h%Mh(k)%MsF        = cmplx(0.0,0.0)
             end if
          end do
          call Write_Mag_Structure_Factors(lu,h,Grp)

       else  !  present(powder)=.false.

          H%nref=ng
          if(allocated(H%mh)) deallocate(H%mh)
          allocate(H%mh(ng))

          if (present(ord)) then
             if (allocated(ind)) deallocate(ind)
             allocate(ind(ng))

             if (ord) then !Reordering reflections by increasing sinTheta/Lambda
                ind= sort(hloc%Mh(:)%s,ng)
             else
                do j=1,ng
                   ind(j)=j
                end do
             end if

             do j=1,ng
                i=ind(j)
                h%Mh(j)%keqv_minus = hloc%Mh(i)%keqv_minus
                h%Mh(j)%num_k      = hloc%Mh(i)%num_k
                h%Mh(j)%signp      = hloc%Mh(i)%signp
                h%Mh(j)%s          = hloc%Mh(i)%s
                h%Mh(j)%h          = hloc%Mh(i)%h
                h%Mh(j)%mult       = 2
                h%Mh(j)%sqMiV      = 0.0
                h%Mh(j)%MiV        = cmplx(0.0,0.0)
                h%Mh(j)%MsF        = cmplx(0.0,0.0)
             end do

          else   ! present(ord)=.false.
             h%nref=ng-1
             do j=1,h%nref
                h%Mh(j)%keqv_minus = hloc%Mh(j)%keqv_minus
                h%Mh(j)%num_k      = hloc%Mh(j)%num_k
                h%Mh(j)%signp      = hloc%Mh(j)%signp
                h%Mh(j)%s          = hloc%Mh(j)%s
                h%Mh(j)%h          = hloc%Mh(j)%h
                h%Mh(j)%mult       = 2
                h%Mh(j)%sqMiV      = 0.0
                h%Mh(j)%MiV        = cmplx(0.0,0.0)
                h%Mh(j)%MsF        = cmplx(0.0,0.0)
             end do
          end if

       end if

    End Subroutine Gen_Satellites

End SubModule kStrf_satellites
