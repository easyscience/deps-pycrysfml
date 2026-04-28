 Submodule (CFML_EnBVS) EnBVS_CostF
  implicit none
   contains

    !!--++
    !!--++ Module Subroutine Bond_Valence(d0,b0,d,sd,bv,sbv)
    !!--++    real(kind=cp), intent(in)  :: d0    ! BVS parameter
    !!--++    real(kind=cp), intent(in)  :: b0    ! BVS parameter
    !!--++    real(kind=cp), intent(in)  :: d     ! Distance
    !!--++    real(kind=cp), intent(in)  :: sd    ! Sigma(d)
    !!--++    real(kind=cp), intent(out) :: bv    ! BVS
    !!--++    real(kind=cp), intent(out) :: sbv   ! Sigma(bv)
    !!--++
    !!--++    (Private)
    !!--++    Zachariasen exponential expression of Bond Valence
    !!--++
    !!--++ Update: Dic - 2007
    !!
    Module Subroutine Bond_Valence(D0,B0,D,Sd,Bv,Sbv)
       !---- Arguments ----!
       real(kind=cp),  intent(in)  :: d0,b0  !Bond-valence parameters
       real(kind=cp),  intent(in)  :: d,sd   !Bond distance and sigma
       real(kind=cp),  intent(out) :: bv,sbv !Bond-valence and sigma

       bv=EXP((d0-d)/b0)
       sbv=bv*sd/b0

    End Subroutine Bond_Valence

    !!----
    !!---- Module Subroutine Calc_BVS(A, Ipr, N_BVSm, BVS_M, Filecod,info_string)
    !!----    type (Atoms_Conf_List_type),              intent(in)   :: A            !  In -> Object of Atoms_Conf_List_type
    !!----    integer,                        optional, intent(in)   :: Ipr
    !!----    integer,                        optional, intent(in)   :: n_bvsm       !  In -> Number of modifications
    !!----    character(len=*), dimension(:), optional, intent(in)   :: BVS_M        ! In -> Text with BVS parameters
    !!----    character(len=*),               optional, intent(in)   :: Filecod
    !!----    character(len=*),               optional, intent(out)  :: info_string
    !!----
    !!----    Subroutine to calculate Bond-Valence sums.
    !!----    Before calling this subroutine it is the responsibility of the calling
    !!----    program to make a previous call to "Calc_Dist_Angle_Sigma" in order
    !!----    to update the internal private variables related to distance/angle calculations.
    !!----    Needs as input the object A (of type atom_Conf_list_type, that
    !!----    should be allocated in the calling program).
    !!----    Control for error is present.
    !!----
    !!---- Update: March - 2005, July 2018
    !!
    Module Subroutine Calc_BVS(A, Ipr, N_Bvsm, Bvs_M, Filecod, info_string)
       !---- Arguments ----!
       type (Atoms_Conf_List_type),            intent(in)  :: A      !  In -> Object of Atoms_Conf_List_type
       integer,                      optional, intent(in)  :: Ipr
       integer,                      optional, intent(in)  :: N_bvsm
       character(len=*),dimension(:),optional, intent(in)  :: bvs_m
       character(len=*),             optional, intent(in)  :: Filecod
       character(len=*),             optional, intent(out) :: info_string
       !---- Local variables ----!
       integer           :: i,j,k,n1,n2,icm,l,icn,isoc,kk,is0, &
                            isstr,isdav,isigt,ibvs,sig1,sig2
       real(kind=cp)     :: tol,fact,del2,s0,q2,  &
                            dd,sigtot,efcn,sums,dav,sdav,q1,d2,  &
                            str2,sstr,ric,r_2,del,perc,spred,disp,  &
                            str,rg1,dist,gii_a,gii_b,gii_c
       character(len=4)  :: rnatom

       call clear_error()

       tol=A%tol*0.01
       if (tol <= 0.001) tol=0.20


       if (present(ipr)) then
          write(unit=ipr,fmt="(/,a)")   "  ------------------------------------------------"
          write(unit=ipr,fmt="(a)")     "  {--- BOND-VALENCE AND POLYHEDRA DISTORTIONS ---}"
          write(unit=ipr,fmt="(a,/)")   "  ------------------------------------------------"
       end if


       if (present(n_bvsm).and. present(ipr)) then
          write(unit=ipr,fmt="(a,/,a,/)")  &
          " Bond-Valence parameters (d0,B0) for Zachariasen formula:  s= exp{(d0-d)/B0}", &
                "   (read from external conditions)"
       else
          write(unit=ipr,fmt="(a,/,a,/)")  &
          " Bond-Valence parameters (d0,B0) for Zachariasen formula:  s= exp{(d0-d)/B0}", &
                "   (data read from internal table)"
       end if

       !---- Each line: Cation Anion d0 b
       if (present(n_bvsm) .and. present(bvs_m)) then
          call Complete_Table(A,N_bvsm,bvs_m)
       end if

       do n1=1,A%N_Cations
          do j=1,A%N_Anions
             n2=A%N_Cations+j
             if (present(ipr)) then
                write(unit=ipr,fmt="(2(a,i3,a,a4),2(a,f6.3),a)") &
                      "   Type",n1,": ",A%Species(n1)," with type",n2,": ",A%Species(n2),&
                      " d0=",Table_d0(n1,n2),"    B0=",Table_b(n1,n2), "   => Reference: "//trim(REF_BVS(Table_ref(n1,n2)))
                write(unit=ipr,fmt="(2(a,a,a,f6.3,a),/)") &
                      "   Cation (Eff. radius): ",A%Species(n1),"(",A%Radius(n1),")   ", &
                      "   Anion  (Eff. radius): ",A%Species(n2),"(",A%Radius(n2),")"
             end if
          end do
       end do

       del2=0.0

       if (present(filecod)) then
          open(newunit=ibvs,file=trim(filecod)//"_sum.bvs",status="replace",action="write", position="append")
          write(unit=ibvs,fmt="(a)") "  Subroutine Calc_BVS (JRC-LLB, version: March-2005)"
          write(unit=ibvs,fmt="(a)") "  Title: Summary of Bond-Valence calculations for file: "//trim(filecod)//".cfl"
          write(unit=ibvs,fmt="(a)") "   Atom      Coord  D_aver Sigm   Distort(x10-4)    Valence    BVSum(Sigma)"
       else
          open(newunit=ibvs,file="summary.bvs",status="replace",action="write", position="append")
          write(unit=ibvs,fmt="(a)") "  Subroutine Calc_BVS (JRC-LLB, version: March-2005)"
          write(unit=ibvs,fmt="(a)") "  Title: Summary of Bond-Valence calculations "
          write(unit=ibvs,fmt="(a)") "   Atom      Coord  D_aver Sigm   Distort(x10-4)    Valence    BVSum(Sigma)"
       end if

       !----
       !---- CAUTION: Subroutine Calc_Dist_Angle_Sigma need
       !----          to be called before using this routine
       gii_a=0.0
       gii_b=0.0
       gii_c=0.0
       do i=1,A%natoms
          icm=coord_info%coord_num(i)
          l=A%Atom(i)%ind_ff(1)
          q1=A%Atom(i)%charge
          sig1=SIGN(1.0_cp,q1)
          icn=0
          efcn=0.0
          sums=0.0
          sigtot=0.0
          dav=0.0
          sdav=0.0
          str2=0.0
          d2=0.0
          isoc=INT(A%atom(i)%VarF(2)*1000.0+0.5)
          if (present(ipr)) then
             write(unit=ipr,fmt="(/,/,a,/,a,a4,a,f6.3,a,i3,a,/,a,/)") &
                  "    -------------------------------------------------------------------",  &
                  " => Bond-valence and coordination of atom: ",A%atom(i)%lab ," occupancy: ",A%atom(i)%VarF(1),"(",isoc,")",  &
                  "    -------------------------------------------------------------------"
          end if
          do j=1,icm
             k=A%Atom(coord_info%n_cooatm(j,i))%ind_ff(1)
             q2=A%Atom(coord_info%n_cooatm(j,i))%charge
             sig2= SIGN(1.0_cp,q2)
             if (sig1 == sig2) cycle
             dd=coord_info%dist(j,i)
             if (dd > (A%Radius(l)+A%Radius(k))*(1.0+tol)) cycle
             icn=icn+1
             efcn=efcn+A%Atom(coord_info%n_cooatm(j,i))%VarF(1) !/A%Atom(i)%VarF(1)
             kk=k
             d2=d2+dd*dd
             s0=coord_info%s_dist(j,i)
             dav=dav+dd
             sdav=sdav+s0*s0
             rnatom=A%atom(coord_info%n_cooatm(j,i))%lab
             call Bond_valence(Table_d0(l,k),Table_b(l,k),dd,s0,str,sstr)
             str=str*A%Atom(coord_info%n_cooatm(j,i))%VarF(1)             !/A%Atom(i)%VarF(1)
             sstr=sstr*A%Atom(coord_info%n_cooatm(j,i))%VarF(1)           !/A%Atom(i)%VarF(1)
             sums=sums+str
             str2=str2+str*str
             sigtot=sigtot+sstr*sstr
             is0=nint(s0*10000)
             isstr=nint(sstr*1000)
             if (present(ipr)) then
                write(unit=ipr,fmt="(a,a4,a,a4,a,f8.4,a,i4,a,f6.3,a,i3,a)")  &
                     " (",A%atom(i)%lab ,")-(",rnatom,") :",dd,"(",is0,")  ",str,"(",isstr,")"
             end if
          end do

          ric=real(icn)
          if (icn == 0) then
            if (present(ipr) ) then
                write(unit=ipr,fmt=*) " => Warning!! atom: ",A%atom(i)%lab ," is non-coordinated"
                write(unit=ipr,fmt=*) "    Increase the tolerance factor for ionic radii"
             end if
             cycle
          end if
          d2=d2/ric
          sigtot=SQRT(sigtot)
          dav=dav/ric
          sdav=SQRT(sdav)/ric

          isdav=INT(sdav*10000+0.5)
          isigt=INT(sigtot*1000+0.5)
          dist=10000.0*(d2/(dav*dav)-1.0)
          r_2=sums/ric
          r_2=SQRT(ABS(str2/ric-r_2*r_2))
          del=sums-ABS(q1)
          del2=del2+del*del
          perc=100.0*ABS(del/q1)

          fact=A%Atom(i)%VarF(1)*real(A%atom(i)%mult)
          gii_a=gii_a+abs(del)*fact
          gii_b=gii_b+perc*fact
          gii_c=gii_c+del*del*fact

          !efcn=efcn/A%Atom(i)%VarF(1)                    !Division by the site occupancy
          spred=ABS(q1)/efcn                              !Predicted valence of a single bond
          disp=table_d0(l,kk)-table_b(l,kk)*LOG(spred)    !Pred. distance
          if (present(ipr)) then
             write(unit=ipr,fmt="(/,a,i5,a,f5.2,a,a4)") " Coordination number: ",icn, &
                 "      Eff.Coor. number: ",efcn,"  for atom: ",A%atom(i)%lab
             write(unit=ipr,fmt="(a,f8.4,a,i4,a,f8.3,a)")  &
                  " Average distance  :",dav,"(",isdav,")  Distortion:  ",dist," xE-04"
             write(unit=ipr,fmt="(a,f8.4,a,f6.3)") " Predicted distance:",disp, &
                  "        Single bond-valence S=",spred
             write(unit=ipr,fmt="(a,f8.3,/,a,f8.3,a,i3,a)") &
                  "                                      Valence: ",q1,  &
                  "                                         Sums: ",sums, "(",isigt,")"
             write(unit=ipr,fmt="(a,2f8.3,/,a)") &
                  " Deviation from the Valence Sum Rule (r1,%dev):",del,perc,  &
                  " {r1=Sumj(sij)-Vi, %dev=100abs(r1)/Vi} "
             write(unit=ipr,fmt="(a,f8.3,/,a)")  &
                  " Deviation from the Equal Valence Rule    (r2):",r_2,  &
                  " {r2=<sij-<sij>>rms}"
             write(unit=ibvs,fmt="(tr4,a4,tr4,f6.2,f8.4,a,i4,a,f14.3,2f12.3,a,i3,a)") &
                  A%atom(i)%lab,efcn,dav,"(",isdav,")",dist,q1,sums, "(",isigt,")"

          end if
       end do

       rg1=SQRT(del2/real(A%natoms))*100.0
       gii_a=gii_a*100.0/A%totatoms
       gii_b=gii_b/A%totatoms  !*100.0 already multiplied
       gii_c=sqrt(gii_c/A%totatoms)*100.0

       if (present(ipr)) then
          write(unit=ipr,fmt="(/,6(a,/))")  &
             " => Lines concerning predicted average distances and single",  &
             "    bond-valence values, as well as the deviations from the",  &
             "    Equal Valence Rule,  apply only to those central atoms",  &
             "    having N coordination-atoms of the same chemical species. ",  &
             "    (The term 'single bond-valence' refers to the valence value",  &
             "    of a single bond for a regular polyhedron, so S=Valence/N)"
           write(unit=ipr,fmt="(/,4(a,/))")  &
             " => The Old Global Instability Index (GII) is calculated with the atoms of the asymetric unit (Num_Atoms).",&
             "    The normalized GII(a,b,c) below are calculated using the sum over asymmetric unit but multiplying ",&
             "    differences by the multiplicity of the site. N_Atoms_UCell is the total number of atoms in the ", &
             "    conventional unit cell. In all cases the result of the different expressions is multiplied by 100.0"
          write(unit=ipr,fmt="(a,f7.2,a)") " =>  Old Global Instability Index (  GII=SQRT{SUM{|BVS-abs(q)|^2}/Num_Atoms} ) =", &
               rg1," /100"
          write(unit=ipr,fmt="(a,f7.2,a)") " =>  Normalized   GII(a)=       SUM {|BVS-abs(q)|  *mult}       /N_Atoms_UCell =", &
               gii_a," /100"
          write(unit=ipr,fmt="(a,f7.2,a)") " =>  Normalized   GII(b)=       SUM {|BVS-abs(q)|  *mult/abs(q)}/N_Atoms_UCell =", &
               gii_b," %"
          write(unit=ipr,fmt="(a,f7.2,a)") " =>  Normalized   GII(c)= SQRT{ SUM {|BVS-abs(q)|^2*mult}       /N_Atoms_UCell}=", &
               gii_c," /100"
       end if

       write(unit=ibvs,fmt="(a,f7.2,a)")" =>  Old Global Instability Index (  GII=SQRT{SUM{|BVS-abs(q)|^2}/Num_Atoms} ) =", &
            rg1," /100"
       write(unit=ibvs,fmt="(a,f7.2,a)")" =>  Normalized   GII(a)=       SUM {|BVS-abs(q)|  *mult}       /N_Atoms_UCell =", &
            gii_a," /100"
       write(unit=ibvs,fmt="(a,f7.2,a)")" =>  Normalized   GII(b)=       SUM {|BVS-abs(q)|  *mult/abs(q)}/N_Atoms_UCell =", &
            gii_b," %"
       write(unit=ibvs,fmt="(a,f7.2,a)")" =>  Normalized   GII(c)= SQRT{ SUM {|BVS-abs(q)|^2*mult}       /N_Atoms_UCell}=", &
            gii_c," /100"
       if(present(info_string)) write(unit=info_string,fmt="(a,f7.2)") " Normalized   GII(c): ",gii_c
       call flush(ibvs)
       close (unit=ibvs)

    End Subroutine Calc_BVS

    !!----
    !!---- Module Subroutine Cost_BVS(A, GII, ERep, gic)
    !!----    type (Atoms_Conf_List_type),  intent(in out) :: A    !  In  -> Object of Atoms_Conf_List_type
    !!----    real(kind=cp),                intent(out)    :: GII  !  OUT -> Global instability index
    !!----    real(kind=cp),      optional, intent(out)    :: ERep !  OUT -> Repulstion term from soft spheres
    !!----    character(len=*),   optional, intent(in)     :: gic  ! If present GII_c is put in GII
    !!----
    !!----    Subroutine to calculate the Global Instability Index.
    !!----    Before calling this subroutine it is the responsibility of the calling
    !!----    program to make a previous call to "Set_TDist_Coordination" or "Set_TDist_Partial_Coordination" in order
    !!----    to update the internal private variables related to distance/angle calculations.
    !!----    Needs as input the object A (of type atom_Conf_list_type, that
    !!----    should be allocated in the calling program).
    !!----    All items corresponding to the bond-valence parameters contained in A have to
    !!----    be properly set before calling this procedure.
    !!----
    !!---- Update: April - 2005, July -2104 (JRC, calculation only for the subset having VarF(5) = 0)
    !!
    Module Subroutine Cost_BVS(A, GII, ERep,gic)
       !---- Arguments ----!
       type (Atoms_Conf_List_type),  intent(in out)  :: A    !  In -> Object of Atoms_Conf_List_type
       real(kind=cp),                intent(out)     :: GII  !  GII_a
       real(kind=cp),      optional, intent(out)     :: ERep !  Repulsion term due to shoft spheres
       character(len=*),   optional, intent(in)      :: gic  !  If present GII_c is put in GII

       !---- Local variables ----!
       integer       :: i,j,k,icm,l,sig1,sig2
       real(kind=cp) :: tol,fact,q2,dd,sums,q1, del, bv,gii_a,gii_c,efcn,sig,rep

       tol=A%tol*0.01
       if (tol <= 0.001) tol=0.20
       !----
       !---- CAUTION: Subroutine Calc_Dist_Angle_Sigma need
       !----          to be called before using this routine
       gii_a=0.0
       gii_c=0.0
       rep=0.0
       do i=1,A%natoms
          if(A%Atom(i)%VarF(5) > 0.01) cycle
          icm=coord_info%coord_num(i)
          l=A%Atom(i)%ind_ff(1)
          q1=A%Atom(i)%charge
          sig1=SIGN(1.0_cp,q1)
          sums=0.0
          efcn=0.0
          do j=1,icm
             k=A%Atom(coord_info%n_cooatm(j,i))%ind_ff(1)
             q2=A%Atom(coord_info%n_cooatm(j,i))%charge
             sig2= SIGN(1.0_cp,q2)
             sig=A%radius(l)+A%radius(k)
             dd=coord_info%dist(j,i)
             if (sig1 == sig2) then
                rep=rep + (0.8*sig/dd)**18
                cycle
             end if
             if (dd > sig*(1.0+tol)) cycle
             efcn=efcn+A%Atom(coord_info%n_cooatm(j,i))%VarF(1)
             bv=EXP((Table_d0(l,k)-dd)/Table_b(l,k))
             bv=bv*A%Atom(coord_info%n_cooatm(j,i))%VarF(1) !Occupancy
             sums=sums+bv
          end do
          A%Atom(i)%varF(3)=efcn
          del=sums-ABS(q1)

          fact=A%Atom(i)%VarF(1)*real(A%atom(i)%mult)
          gii_a=gii_a+abs(del)*fact
          gii_c=gii_c+del*del*fact

       end do
       gii_a=gii_a*100.0/A%totatoms
       gii_c=sqrt(gii_c/A%totatoms)*100.0
       GII=gii_a
       if(present(ERep)) ERep=rep
       if(present(gic)) GII=gii_c

    End Subroutine Cost_BVS

    !!----
    !!---- Module Subroutine Cost_BVS_CoulombRep(A, GII, ERep)
    !!----    type (Atoms_Conf_List_type),  intent(in out):: A     !  In  -> Object of Atoms_Conf_List_type
    !!----    real(kind=cp),                intent(out)   :: GII   !  OUT -> Global instability index
    !!----    real(kind=cp),                intent(out)   :: ERep   !  Pseudo Repulsion Coulomb "energy"
    !!----
    !!----
    !!----    Subroutine to calculate the Global Instability Index Gii_a and
    !!----    a pseudo Coulomb repulsion energy useful to avoid cation-cation and
    !!----    anion-anion overlap when using this cost function for predicting or
    !!----    solving a ionic crystal structure. It was used in the old program PiXSA,
    !!----    by J. Pannetier, J. Bassas-Alsina, J.Rodriguez-Carvajal and V. Caignaert,
    !!----    in "Prediction of Crystal Structures from Crystal Chemistry Rules by
    !!----    Simulated Annealing", Nature 346, 343-345 (1990).
    !!----    An additional repulsive term (soft sphere) of the form Esph=(sig/d)**18,
    !!----    with sig equal to the sum of ionic radii of the pair, has been introduced.
    !!----
    !!----    Before calling this subroutine it is the responsibility of the calling
    !!----    program to make a previous call to "Set_TDist_Coordination" in order
    !!----    to update the internal Coord_Info variables related to distance and
    !!----    angle calculations.
    !!----    Needs as input the object A (of type atom_Conf_list_type, that
    !!----    should be allocated in the calling program).
    !!----    All items corresponding to the bond-valence parameters contained in
    !!----    "A" have to be properly set before calling this procedure.
    !!----    The repulsion term has been renormalized to a single bond dividing by
    !!----    the total number of repulsive bonds and the sum of the charges products.
    !!----
    !!---- Update: April - 2026
    !!
    Module Subroutine Cost_BVS_CoulombRep(A, GII, ERep)
       !---- Arguments ----!
       type (Atoms_Conf_List_type),  intent(in out):: A      !  In -> Object of Atoms_Conf_List_type
       real(kind=cp),                intent(out)   :: GII    !  GII_a
       real(kind=cp),                intent(out)   :: ERep   !  Pseudo Repulsion Coulomb "energy"

       !---- Local variables ----!
       integer        :: i,j,k,icm,l,sig1,sig2,efcoor
       real(kind=cp)  :: tol,fact,q2,dd,sums,q1, del, bv,efcn,sig,qeff,qprod

       tol=A%tol*0.01
       if (tol <= 0.001) tol=0.20

       !----
       !---- CAUTION: Subroutine Calc_Dist_Angle_Sigma need
       !----          to be called before using this routine
       gii=0.0
       Erep=0.0
       efcoor=0
       do i=1,A%natoms
          icm=coord_info%coord_num(i)
          l=A%Atom(i)%ind_ff(1)
          q1=A%Atom(i)%charge
          sig1=SIGN(1.0_cp,q1)
          sums=0.0
          efcn=0.0
          do j=1,icm
             k=A%Atom(coord_info%n_cooatm(j,i))%ind_ff(1)
             q2=A%Atom(coord_info%n_cooatm(j,i))%charge
             sig2= SIGN(1.0_cp,q2)
             dd=coord_info%dist(j,i)
             sig=A%radius(l)+A%radius(k) !sum of ionic radii of the current pair
             if (sig1 == sig2) then
                efcoor=efcoor+1
                qprod=q1*q2
                qeff=qeff+qprod
                Erep=Erep+ qprod/dd + (sig/dd)**18  !Coulomb potential + soft sphere repulsion (avoid short distances)
                cycle
             end if
             if (dd > sig*(1.0+tol)) cycle
             efcn=efcn+A%Atom(coord_info%n_cooatm(j,i))%VarF(1)
             bv=EXP((Table_d0(l,k)-dd)/Table_b(l,k))
             bv=bv*A%Atom(coord_info%n_cooatm(j,i))%VarF(1)
             sums=sums+bv
          end do
          del=sums-ABS(q1)
          fact=A%Atom(i)%VarF(1)*real(A%atom(i)%mult)
          gii=gii+abs(del)*fact
          A%Atom(i)%varF(3)=efcn
       end do
       gii=gii*100.0/A%totatoms
       Erep=Erep/max(1,efcoor)/max(1.0,qeff)

    End Subroutine Cost_BVS_CoulombRep

 End Submodule EnBVS_CostF
