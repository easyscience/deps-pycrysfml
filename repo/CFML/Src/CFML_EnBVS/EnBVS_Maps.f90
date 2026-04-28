 Submodule (CFML_EnBVS) EnBVS_Maps
  implicit none
   contains

    !!----
    !!---- Module Subroutine Calc_Map_BVEL(A,Spg,Cell,Filecod,ndimx,ndimy,ndimz,atname,drmax,delta,vol,emin,npix,outp,bvel_map)
    !!----    !---- Arguments ----!
    !!----    type (Atoms_Conf_List_type), intent(in) :: A
    !!----    class (SpG_Type),      intent(in) :: SpG
    !!----    Type (Cell_G_Type),          intent(in) :: Cell
    !!----    character(len=*),            intent(in) :: Filecod
    !!----    integer,                     intent(in) :: ndimx
    !!----    integer,                     intent(in) :: ndimy
    !!----    integer,                     intent(in) :: ndimz
    !!----    character(len=*),            intent(in) :: atname  !Species of the probe atom e.g. LI+1
    !!----    real(kind=cp),               intent(in) :: drmax
    !!----    real(kind=cp), optional,     intent(in) :: delta
    !!----    real(kind=cp), optional,     intent(out):: vol
    !!----    real(kind=cp), optional,     intent(out):: emin
    !!----    integer,       optional,     intent(out):: npix
    !!----    real(kind=cp), optional,  dimension(:,:,:), allocatable, intent(out) :: bvel_map
    !!----
    !!----    Calculate Bond-Valence energy landscape map where the energy at each point of the grid
    !!----    is determined by a species representative defined in atname. The BV-site Energy value
    !!----    is evaluated for distances below drmax value. If delta is present only the points with
    !!----    energy below EMin+delta .
    !!----
    !!---- Created: January 2015
    !!
    Module Subroutine Calc_Map_BVEL(A,Spg,Cell,Filecod,ndimx,ndimy,ndimz,atname,drmax,delta,vol,emin,npix,outp,bvel_map)
       !---- Arguments ----!
       type (Atoms_Conf_List_type), intent(in) :: A
       class(SpG_Type),       intent(in) :: SpG
       Type (Cell_G_Type),          intent(in) :: Cell
       character(len=*),            intent(in) :: Filecod
       integer,                     intent(in) :: ndimx
       integer,                     intent(in) :: ndimy
       integer,                     intent(in) :: ndimz
       character(len=*),            intent(in) :: atname  !Species of the probe atom e.g. LI+1
       real(kind=cp),               intent(in) :: drmax
       real(kind=cp), optional,     intent(in) :: delta
       real(kind=cp), optional,     intent(out):: vol
       real(kind=cp), optional,     intent(out):: emin
       integer,       optional,     intent(out):: npix
       logical,       optional,     intent(in) :: outp
       real(kind=cp), optional,  dimension(:,:,:), allocatable, intent(out) :: bvel_map

       !---- Local variables ----!
       character(len=4)                             :: car,atm
       integer                                      :: i,j,k,n,n1,n2,np,jbvs
       integer                                      :: nx1,nx2,ny1,ny2,nz1,nz2
       integer                                      :: i1,j1,k1,sig1,sig2,ncont
       real(kind=cp)                                :: rx1,ry1,rz1,qval,q1,q2,rep,p,s,cose
       real(kind=cp)                                :: sbvs, dd, occ, radius, rho, dmin, &
                                                       dzero, alpha,c_rep,c_atr !, d_cutoff
       real(kind=cp),  dimension(3)                 :: pto,pta,step,extend
       real(kind=cp),  dimension(:,:,:), allocatable:: map_bvs
       integer(kind=2),dimension(:,:,:), allocatable:: contrib
       type (AtList_Type)                           :: At1, At2
       logical                                      :: anion,cation
       !Coulomb constant (1/4pie0) to get energies in eV, use electron charges and distances in angstroms
       real(kind=dp), parameter :: ke=14.399644850445155254866066
       !Principal quantum numbers of the test=ion  and the species of all the atoms of the list
       real(kind=cp)                            :: n_tion, ferfc
       real(kind=cp), dimension(:), allocatable :: n_j
       logical :: all_present


       !---- Initial conditions ----!
       if (A%natoms <= 0) return
       if (ndimx <= 0 .or. ndimy <= 0 .or. ndimz <= 0) return

       all_present=present(delta) .and. present(vol) .and. present(npix) .and. present(emin)
       !---- Preparing the Variables and calculations ----!
       call Allocate_Atom_List(A%natoms,At1,"Atm_Std_Type",0)
       At1%atom=A%atom  !Atom list A%Atom(:)%ind_ff(1) contains the pointer to the species
                        !coming from A (Atoms_Conf_List_type, set in the main program)
       atm=u_case(atname)
       n1=0
       allocate(n_j(A%N_Spec))
       n_j=0.0
       if (.not. allocated(Ap_Table)) call Set_Atomic_Properties_Table()
       do j=1,A%N_Spec
         car=u_case(A%species(j))
         do i=1,Ap_species_n
           if(Ap_Table(i)%Symb == car) then
              n_j(j)=real(Ap_Table(i)%n)
              exit
           end if
         end do
         if(car == atm) n_tion=n_j(j)
       end do

       do i=1,At1%natoms  !This exclude all atoms with the same species as Atname
          n2=A%Atom(i)%ind_ff(1)
          if (trim(u_case(A%species(n2))) == trim(atm)) then
             At1%atom(i)%active=.false.
             q1=At1%atom(i)%charge
             sig1=nint(SIGN(1.0_cp,q1))
             n1=n2
             radius=A%radius(n1)
             car=A%Species(n1)
          end if
       end do
       if (n1 ==0) then
          Err_CFML%Ierr=1
          Err_CFML%Msg="The point atom "//atname//" is not in the Species Table"
          return
       end if

       call Extend_Atom_List(At1,At2,Spg,"Atm_Std",.true.)
       !check that all species are well set in the list
       !Write(unit=*,fmt="(a)") " => List of atoms for calculating BVEL (Calc_Map_BVEL)"
       do n=1,At2%natoms
           n2=At2%Atom(n)%ind_ff(1)
           !write(unit=*,fmt="(a,a,3f12.5)") At2%Atom(n)%Lab,At2%Atom(n)%SfacSymb,At2%Atom(n)%x
           if (n2 ==0) then
              Err_CFML%Ierr=1
              Err_CFML%Msg="The atom "//trim(At2%Atom(n)%lab)//" is not in the Species Table"
              return
           end if
       end do

       allocate(map_bvs(ndimx,ndimy,ndimz),contrib(ndimx,ndimy,ndimz))
       map_bvs=0.0
       contrib=0
       ! Determination of the valence expected for a good position
       i=index(car,"+")
       cation=.false.
       anion=.false.
       if( i /= 0) then
         read(unit=car(i+1:),fmt=*) qval
         cation=.true.
       else
         i=index(car,"-")
         read(unit=car(i+1:),fmt=*) qval
         anion=.true.
       end if
       step=[ 1.0/real(ndimx),  1.0/real(ndimy), 1.0/real(ndimz) ]
       extend=[ drmax/cell%cell(1),  drmax/cell%cell(2), drmax/cell%cell(3) ]

       np=0; npix=0
       !---- Map Calculation ----!
       do k=1,ndimz
          pto(3)=(k-1)*step(3)
          do j=1,ndimy
             pto(2)=(j-1)*step(2)
             do i=1,ndimx
                pto(1)=(i-1)*step(1)

                rx1=pto(1)-extend(1)
                if (rx1 <= 0.0) then
                    nx1=int(rx1)-1
                else
                    nx1=int(rx1)
                end if
                nx2=int(pto(1)+extend(1))

                ry1=pto(2)-extend(2)
                if (ry1 <= 0.0) then
                    ny1=int(ry1)-1
                else
                    ny1=int(ry1)
                end if
                ny2=int(pto(2)+extend(2))

                rz1=pto(3)-extend(3)
                if (rz1 <= 0.0) then
                    nz1=int(rz1)-1
                else
                    nz1=int(rz1)
                end if
                nz2=int(pto(3)+extend(3))

                sbvs=0.0
                rep=0.0
                ncont=0
                do n=1,At2%natoms
                   n2=At2%Atom(n)%ind_ff(1)
                   q2=At2%Atom(n)%charge
                   sig2= nint(SIGN(1.0_cp,q2))
                   rho=(radius+A%radius(n2))*0.74
                   dzero=Table_Dzero(n1,n2)
                    dmin=Table_Rmin(n1,n2)
                   alpha=Table_Alpha(n1,n2)
                   !d_cutoff=Table_Rcutoff(n1,n2) !not used, see below
                   occ=At2%Atom(n)%VarF(1)
                   c_rep=occ*q1*q2/sqrt(n_tion*n_j(n2))
                   c_atr=occ*dzero
                   ferfc=erfc(drmax/rho)/drmax !always below 10^(-9) when drmax/rho > 4.2
                   do k1=nz1,nz2
                      do j1=ny1,ny2
                         do i1=nx1,nx2
                            pta=At2%Atom(n)%x+real([i1,j1,k1])
                            dd=max(Distance(pto,pta,Cell),0.0001) !To avoid division by zero
                            if (dd > drmax) cycle
                            contrib(i,j,k)=contrib(i,j,k)+1
                            if (sig1 == sig2) then
                                rep=rep + c_rep*(erfc(dd/rho)/dd-ferfc)
                            else
                               !if(dd > d_cutoff) cycle  !local d_cutoff not used, only drmax is used!
                               sbvs=sbvs+ c_atr*((exp(alpha*(dmin-dd))-1.0)**2-1.0)
                            end if
                         end do
                      end do
                   end do

                end do
                !Multiply the repulsion term by the Coulomb constant to convert to eV
                map_bvs(i,j,k)=sbvs+ke*rep
             end do
          end do
       end do
       !Calculation of the volume available for mobility path
       if(all_present) then
          emin=minval(map_bvs)
          npix=count(map_bvs <= emin+delta)
          p=1.0; s=1.0
          do i=1,3
             cose=cosd(Cell%ang(i))
             p=p*cose
             s=s-cose*cose
          end do
          vol=sqrt(abs(s+2.0*p))
          do i=1,3
             p=Cell%cell(i)*step(i)
             vol=vol*p
          end do
          vol=vol*real(npix)
       end if

       !---- Export a File ----!
       if(present(outp)) then

          open(newunit=jbvs,file=trim(filecod)//".map",status="replace",action="write")

          write (unit=jbvs, fmt='(a)') "BVEL Map Calculations using Bond_STR Program"
          write (unit=jbvs, fmt='(a)') "BVEL Map for species "//trim(car)
          write (unit=jbvs, fmt='(a,3f12.4,3x,3f8.3)') "CELL ",cell%cell,cell%ang
          write (unit=jbvs, fmt='(a)') "SPGR  "//trim(SpG%spg_symb)
          write (unit=jbvs, fmt='(a,f10.4,a)')" => Global distance cutoff:",drmax," angstroms"
          write (unit=jbvs,fmt="(a,/,a,/)")  &
          " Bond-Valence Energy parameters (D0,Rmin,alpha) for Morse Potential:  D0*[{exp(alpha(dmin-d))-1}^2-1]", &
                "   (data read from internal table or provided by the user)"
          do n1=1,A%N_Cations
             do j=1,A%N_Anions
                n2=A%N_Cations+j
                write(unit=jbvs,fmt="(2(a,i3,a,a4),/,3(a,f9.5),/,3(a,f9.5),a)")           &
                      "   Type",n1,": ",A%Species(n1)," with type",n2,": ",A%Species(n2), &
                      "    D0  =",Table_Dzero(n1,n2),"       Rmin =",Table_Rmin(n1,n2),   &
                      "  Alpha =",Table_Alpha(n1,n2),"  Av. Coord.=",Table_Avcoor(n1,n2), &
                      "    R0  =",Table_Rzero(n1,n2),"   R-cutoff =",Table_Rcutoff(n1,n2),&
                      "   => Reference: "//trim(REF_BVS(Table_ref(n1,n2)))
                write(unit=jbvs,fmt="(2(a,a,a,f6.3,a),/)") &
                      "   Cation (Eff. radius): ",A%Species(n1),"(",A%Radius(n1),")   ",  &
                      "   Anion  (Eff. radius): ",A%Species(n2),"(",A%Radius(n2),")"
             end do
          end do
          write(unit=jbvs,fmt="(a,i3,a)") &
           "   Principal Quantum Number (test ion): ",nint(n_tion)," for species: "//trim(atname)
          do j=1,A%N_Spec
                write(unit=jbvs,fmt="(a,i3,a)") &
                      "   Principal Quantum Number: ",nint(n_j(j))," for species: "//A%Species(j)
          end do
          write (unit=jbvs, fmt='(a)')     " List of atoms  "
          write (unit=jbvs, fmt='(a)')     "      Label  Species       X           Y           Z         Biso     OccFactor    Occupancy "
          do i=1,At1%natoms
            if(At1%atom(i)%active) then
                write(unit=jbvs, fmt='(a5,2a6,t20,6f12.5)')"Atom ",trim(A%Atom(i)%lab)," "//A%Atom(i)%SfacSymb, &
                                       A%Atom(i)%x, A%Atom(i)%U_iso, A%Atom(i)%occ, A%Atom(i)%VarF(1)
            end if
          end do
          write (unit=jbvs, fmt='(/,a)')     " Expanded List of atoms  "
          write (unit=jbvs, fmt='(a)')     "    #      Label  Species     X           Y           Z         Biso     OccFactor    Occupancy "
          do i=1,At2%natoms
                write(unit=jbvs, fmt='(i5,3a6,t23,6f12.5)') i," Atom ",trim(At2%Atom(i)%lab)," "//At2%Atom(i)%SfacSymb, &
                                       At2%Atom(i)%x, At2%Atom(i)%U_iso, At2%Atom(i)%occ, At2%Atom(i)%VarF(1)
          end do
          if(present(delta) .and. present(vol)) then
             write (unit=jbvs, fmt='(/,a,f10.4,a)')   "Value of delta for volumen calculation:",delta," eV"
             write (unit=jbvs, fmt='(a,f10.4,a)')     "Available volume for ion mobility in the unit cell:",vol," angstroms^3"
             write (unit=jbvs, fmt='(a,f10.4,a)')     "Volume  fraction for ion mobility in the unit cell:",vol/Cell%Vol*100.0, "%"
             write (unit=jbvs, fmt='(a,f10.4,a,i8)')  "Minum Energy (in eV):", emin,"  Number of pixels with Emin < Energy < Emin+Delta: ",npix
          end if
          write (unit=jbvs, fmt='(a)')     "! Grid values: ndimx,ndimy,ndimz [0,ndimx-1] [0,ndimy-1] [0,ndimz-1]  "
          write (unit=jbvs, fmt='(a,9i6)') "GRID ",ndimx,ndimy,ndimz,0,ndimx-1,0,ndimy-1,0,ndimz-1
          if(.not. outp) then
              write (unit=jbvs, fmt='(a)')     "    X         Y         Z        Energy(eV)   #Contr.Terms"
              do k=1,ndimz
                 pto(3)=(k-1)*step(3)
                 do j=1,ndimy
                    pto(2)=(j-1)*step(2)
                    do i=1,ndimx
                       pto(1)=(i-1)*step(1)
                       ncont=contrib(i,j,k)
                       write(unit=jbvs, fmt='(3f10.5,f14.6,i12)') pto,map_bvs(i,j,k),ncont
                    end do
                 end do
              end do
          else
             write (unit=jbvs, fmt='(a)')     "DENSITY_MAP"
             write (unit=jbvs,fmt='(10g14.5)') map_bvs
          end if
         close(unit=jbvs)
       end if
       if(present(bvel_map)) then
         allocate(bvel_map(ndimx,ndimy,ndimz))
         bvel_map=map_bvs
       end if
       call write_grid_VESTA(map_bvs,cell,"Bond Valence Energy Landscape",trim(filecod)//"_bvel","P")

    End Subroutine Calc_Map_BVEL

    !!----
    !!---- Module Subroutine Calc_Map_BVS(A,Spg,Cell,Filecod,ndimx,ndimy,ndimz,atname,drmax,delta,vol)
    !!----    type (Atoms_Conf_List_type), intent(in) :: A
    !!----    class(SpG_Type),       intent(in) :: SpG
    !!----    Type (Cell_G_Type),          intent(in) :: Cell
    !!----    character(len=*),            intent(in) :: Filecod
    !!----    integer,                     intent(in) :: ndimx
    !!----    integer,                     intent(in) :: ndimy
    !!----    integer,                     intent(in) :: ndimz
    !!----    character(len=*),            intent(in) :: atname
    !!----    real(kind=cp),               intent(in) :: drmax
    !!----    real(kind=cp), optional,     intent(in) :: delta !Tolerance in v.u. for output the map
    !!----    real(kind=cp), optional,     intent(out):: vol
    !!----
    !!----    Calculate a map of BVS values at each point of the grid is determined
    !!----    by a species representative defined in atname. The BVS value is evaluated
    !!----    for distances below drmax value. If delta is present only the points with valence
    !!----    close ( q-delta < BVS < q+delta) to that of the atname are output in the map.
    !!----
    !!---- Update: December - 2007, December 2014 ( JRC,change the order of indices and VESTA output)
    !!
    Module Subroutine Calc_Map_BVS(A,Spg,Cell,Filecod,ndimx,ndimy,ndimz,atname,drmax,delta,vol)
       !---- Arguments ----!
       type (Atoms_Conf_List_type), intent(in) :: A
       class(SpG_Type),       intent(in) :: SpG
       Type (Cell_G_Type),          intent(in) :: Cell
       character(len=*),            intent(in) :: Filecod
       integer,                     intent(in) :: ndimx
       integer,                     intent(in) :: ndimy
       integer,                     intent(in) :: ndimz
       character(len=*),            intent(in) :: atname  !Species of the probe atom e.g. LI+1
       real(kind=cp),               intent(in) :: drmax
       real(kind=cp), optional,     intent(in) :: delta
       real(kind=cp), optional,     intent(out):: vol

       !---- Local variables ----!
       character(len=4)                             :: car,atm
       integer                                      :: i,j,k,n,n1,n2,np,L,nL
       integer                                      :: nx1,nx2,ny1,ny2,nz1,nz2
       integer                                      :: i1,j1,k1,sig1,sig2
       integer                                      :: jbvs,npix
       integer,      dimension(:),     allocatable  :: ind
       real(kind=cp)                                :: rx1,ry1,rz1,qval,dif,q1,q2,rep,p,s
       real(kind=cp)                                :: sbvs, dd, occ, radius,sig, cose
       real(kind=cp), dimension(3)                  :: pto,pta,step,extend
       real(kind=cp), dimension(:,:,:), allocatable :: map_bvs
       real(kind=cp), dimension(:,:),   allocatable :: peaks
       real(kind=cp), dimension(:),     allocatable :: VD_peaks
       type (AtList_Type)                           :: At1, At2
       logical                                      :: new_peak,anion,cation

       !---- Initial conditions ----!
       if (A%natoms <= 0) return
       if (ndimx <= 0 .or. ndimy <= 0 .or. ndimz <= 0) return

       !---- Preparing the Variables and calculations ----!
       call Allocate_Atom_List(A%natoms,At1,"Atm_Std_Type",0)
       At1%atom=A%atom  !Atom list A%Atom(:)%ind_ff(1) contains the pointer to the species
                        !coming from A (Atoms_Conf_List_type, set in the main program)
       atm=u_case(atname)
       n1=0
       do i=1,At1%natoms  !This exclude all atoms with the same species as Atname
          n2=A%Atom(i)%ind_ff(1)
          if (trim(u_case(A%species(n2))) == trim(atm)) then
             At1%atom(i)%active=.false.
             q1=At1%atom(i)%charge
             sig1=nint(SIGN(1.0_cp,q1))
             n1=n2
             radius=A%radius(n1)
             car=A%Species(n1)
          end if
       end do
       if (n1 ==0) then
          Err_CFML%Ierr=1
          Err_CFML%Msg="The point atom "//atname//" is not in the Species Table"
          return
       end if

       call Extend_Atom_List(At1,At2,Spg,"Atm_Std_Type",.true.)
       call Allocate_atom_list(0,At1,"Atm_Std_Type",0) !Deallocate atom list
       !check that all species are well set in the list
       !write(unit=*,fmt="(a)") " => List of atoms for calculating BVS map (Calc_Map_BVS)"
       do n=1,At2%natoms
           n2=At2%Atom(n)%ind_ff(1)
           !write(unit=*,fmt="(a,a,3f12.5)") At2%Atom(n)%Lab,At2%Atom(n)%SfacSymb,At2%Atom(n)%x
           if (n2 ==0) then
              Err_CFML%Ierr=1
              Err_CFML%Msg="The atom "//trim(At2%Atom(n)%lab)//" is not in the Species Table"
              return
           end if
       end do

       allocate(map_bvs(ndimx,ndimy,ndimz))
       map_bvs=0.0

       if(.not. present(delta)) then
          allocate(peaks(4,ndimx)) ! A maximum of ndimx peaks will be stored
          peaks=0.0
          allocate(VD_peaks(ndimx)) ! Vector holding the differences of bond-valences
          VD_peaks=0.0
          allocate(ind(ndimx))      ! Vector pointer for ordering the peaks
          ind=0
       end if

       ! Determination of the valence expected for a good position
       i=index(car,"+")
       cation=.false.
       anion=.false.
       if( i /= 0) then
         read(unit=car(i+1:),fmt=*) qval
         cation=.true.
       else
         i=index(car,"-")
         read(unit=car(i+1:),fmt=*) qval
         anion=.true.
       end if
       step=[ 1.0/real(ndimx),  1.0/real(ndimy), 1.0/real(ndimz) ]
       extend=[ drmax/cell%cell(1),  drmax/cell%cell(2), drmax/cell%cell(3) ]

       np=0; npix=0
       !---- Map Calculation ----!
       do k=1,ndimz
          pto(3)=(k-1)*step(3)
          do j=1,ndimy
             pto(2)=(j-1)*step(2)
             do i=1,ndimx
                pto(1)=(i-1)*step(1)

                rx1=pto(1)-extend(1)
                if (rx1 <= 0.0) then
                    nx1=int(rx1)-1
                else
                    nx1=int(rx1)
                end if
                nx2=int(pto(1)+extend(1))

                ry1=pto(2)-extend(2)
                if (ry1 <= 0.0) then
                    ny1=int(ry1)-1
                else
                    ny1=int(ry1)
                end if
                ny2=int(pto(2)+extend(2))

                rz1=pto(3)-extend(3)
                if (rz1 <= 0.0) then
                    nz1=int(rz1)-1
                else
                    nz1=int(rz1)
                end if
                nz2=int(pto(3)+extend(3))

                sbvs=0.0
                rep=0.0
                do n=1,At2%natoms
                   n2=At2%Atom(n)%ind_ff(1)
                   q2=At2%Atom(n)%charge
                   sig2= nint(SIGN(1.0_cp,q2))
                   sig=(radius+A%radius(n2))*0.99
                   do k1=nz1,nz2
                      do j1=ny1,ny2
                         do i1=nx1,nx2
                            pta=At2%Atom(n)%x+real([i1,j1,k1])
                            occ=At2%Atom(n)%VarF(1)
                            dd=max(Distance(pto,pta,Cell),0.0001) !To avoid division by zero
                            if (dd > drmax) cycle
                            if (sig1 == sig2) then
                               rep=rep + (sig/dd)**18
                               cycle
                            end if
                            sbvs=sbvs+occ*exp((table_d0(n1,n2)-dd)/table_b(n1,n2))
                         end do
                      end do
                   end do
                end do
                dif=abs(sbvs-qval)
                if(present(delta)) then
                  if(dif > delta .or. rep > 0.01) cycle
                else
                  !Algorithm for selecting the optimum positions
                  if(dif < 0.15 .and. np < ndimx) then
                    new_peak=.true.
                    do L=1,np
                       dd=Distance(peaks(1:3,L),pto,Cell)
                       if( dd < 0.8 ) then
                         new_peak=.false.
                         nL=L
                         exit
                       end if
                    end do
                    if(new_peak) then
                      np=np+1
                      peaks(1:3,np)= pto
                      peaks(4,np)  = sbvs
                      VD_peaks(np) = dif
                    else  !now compare with the peak stored at nL and interchange them if sbvs is more favourable
                      if( dif < abs(qval-peaks(4,nL)) ) then
                        peaks(1:3,nL) = pto
                        peaks(  4,nL) = sbvs
                        VD_peaks(nL)  = dif
                      end if
                    end if
                  end if
                end if
                !end of peaks construction
                npix=npix+1
                map_bvs(i,j,k)=sbvs
             end do
          end do
       end do
       !Calculation of the volume available for mobility path
       if(present(delta) .and. present(vol))then
          p=1.0; s=1.0
          do i=1,3
             cose=cosd(Cell%ang(i))
             p=p*cose
             s=s-cose*cose
          end do
          vol=sqrt(abs(s+2.0*p))
          do i=1,3
             p=Cell%cell(i)*step(i)
             vol=vol*p
          end do
          vol=vol*real(npix)
       end if
       !Sorting the favourable peak positions for inserting the species of atom Atname
       if(.not. present(delta)) ind=sort(VD_peaks,np)
       !---- Export a File ----!

       open(newunit=jbvs,file=trim(filecod)//".map",status="replace",action="write")
       write (unit=jbvs, fmt='(a)') "BVS Map Calculations using Bond_STR Program"
       write (unit=jbvs, fmt='(a)') "BVS Map for species "//trim(car)
       write (unit=jbvs, fmt='(a,3f12.4,3x,3f8.3)') "CELL ",cell%cell,cell%ang
       write (unit=jbvs, fmt='(a)')     "SPGR  "//trim(SpG%spg_symb)
       write (unit=jbvs, fmt='(a,f10.4,a)')" => Global distance cutoff:",drmax," angstroms"
       write (unit=jbvs,fmt="(/,a,/,a,/)")  &
       " Bond-Valence parameters (d0,B0) for Zachariasen formula:  s= exp{(d0-d)/B0}", &
             "   (data read from internal table or provided by the user)"
       do n1=1,A%N_Cations
          do j=1,A%N_Anions
             n2=A%N_Cations+j
                write(unit=jbvs,fmt="(2(a,i3,a,a4),2(a,f6.3),a)") &
                      "   Type",n1,": ",A%Species(n1)," with type",n2,": ",A%Species(n2),&
                      " d0=",Table_d0(n1,n2),"    B0=",Table_b(n1,n2), "   => Reference: "//trim(REF_BVS(Table_ref(n1,n2)))
                write(unit=jbvs,fmt="(2(a,a,a,f6.3,a),/)") &
                      "   Cation (Eff. radius): ",A%Species(n1),"(",A%Radius(n1),")   ", &
                      "   Anion  (Eff. radius): ",A%Species(n2),"(",A%Radius(n2),")"
          end do
       end do
       write (unit=jbvs, fmt='(a)')     "! List ot atoms  "
       do i=1,A%natoms
         write(unit=jbvs, fmt='(a,t20,5f12.5)')"Atom "//trim(A%Atom(i)%lab)//"  "//A%Atom(i)%SfacSymb, &
                                                A%Atom(i)%x, A%Atom(i)%U_iso, A%Atom(i)%occ
       end do
       if(present(delta) .and. present(vol))then
          write (unit=jbvs, fmt='(/,a,f10.4,a)')  "Value of delta for volumen calculation:",delta," eV"
          write (unit=jbvs, fmt='(a,f10.4,a)')    "Available volume for ion mobility in the unit cell:",vol," angstroms^3"
          write (unit=jbvs, fmt='(a,f10.4,a)')    "Volume  fraction for ion mobility in the unit cell:",vol/Cell%Vol*100.0, "%"
       end if
       if(.not. present(delta)) then
         write (unit=jbvs, fmt='(a)')     "! List ot favourable positions for inserting the species "//trim(car)
         do i=1,np
           j=ind(i)
           write(unit=jbvs, fmt='(a,i4,a,3f10.5,a,f8.3)')"#",i,"  Position: (",peaks(1:3,j),")  Valence: ",peaks(4,j)
         end do
       end if
       write (unit=jbvs, fmt='(a)')     "! Grid values: ndimx,ndimy,ndimz [0,ndimx-1] [0,ndimy-1] [0,ndimz-1]  "
       write (unit=jbvs, fmt='(a,9i6)') "GRID ",ndimx,ndimy,ndimz,0,ndimx-1,0,ndimy-1,0,ndimz-1
       write (unit=jbvs, fmt='(a)')     "DENSITY_MAP"
       write (unit=jbvs,fmt='(10g14.5)') map_bvs
       close(unit=jbvs)
       call write_grid_VESTA(map_bvs,cell,"Bond Valence Map",trim(filecod)//"_bvs","P")

       !---- End Procedure ----!
       if (allocated(map_bvs)) deallocate(map_bvs)
       call Allocate_atom_list(0,At2,"Atm_type",0) !Deallocate atom list

    End Subroutine Calc_Map_BVS

 End Submodule EnBVS_Maps