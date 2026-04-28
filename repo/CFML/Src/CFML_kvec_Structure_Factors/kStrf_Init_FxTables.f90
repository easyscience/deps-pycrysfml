SubModule (CFML_kvec_Structure_Factors) kStrf_Init_FxTables
   implicit none
   Contains
    !!--++
    !!--++ Module Function Mfj(S,Coeff)
    !!--++    real(kind=cp),             intent(in) :: s !sin Theta/Lambda
    !!--++    real(kind=cp),dimension(7),intent(in) :: coeff
    !!--++
    !!--++    (Private)
    !!--++    Magnetic form factor calculation according to:
    !!--++    Fj(s)=Sum_i{1,6,2}[Coeff(i)*exp(-Coeff(i+1)*s*s)] + Coeff(7)
    !!--++
    !!--++ Update: April - 2005
    !!
    Module Function Mfj(S,Coeff) Result(Res)
       !---- Arguments ----!
       real(kind=cp),             intent(in) :: s  !sin Theta/Lambda
       real(kind=cp),dimension(7),intent(in) :: coeff
       real(kind=cp)                         :: res

       !---- Local variables ----!
       integer :: i

       res=coeff(7)
       do i=1,6,2
          res=res + coeff(i)*exp(-coeff(i+1)*s*s)
       end do
    End Function Mfj

    !!----
    !!---- Module Subroutine Init_Mag_Structure_Factors(Reflex,Atm,Grp,Lun)
    !!----    type(MagH_List_Type),           intent(in) :: Reflex
    !!----    type(Matom_list_type),          intent(in) :: Atm
    !!----    type(MagSymm_k_Type),           intent(in) :: Grp
    !!----    integer,              optional, intent(in) :: lun
    !!----
    !!----    Allocates and initializes arrays for Magnetic Structure Factors calculations.
    !!----    A calculation of fixed tables is also performed.
    !!----
    !!---- Update: April - 2005
    !!
    Module Subroutine Init_Mag_Structure_Factors(Reflex,Atm,Grp,lun)
       !---Arguments ---!
       type(MagH_List_Type),           intent(in) :: Reflex
       type(Matom_list_type),          intent(in) :: Atm
       type(MagSymm_k_Type),           intent(in) :: Grp
       integer,              optional, intent(in) :: lun

       !--- Local variables ---!
       integer :: Natm, Multr
       integer :: ierr

       call Clear_Error()

       Natm = Atm%natoms
       Multr= Grp%Numops

       !---- Magnetic Scattering factor tables ----!
       if (allocated(mFR)) deallocate(mFR)
       allocate(mFR(Natm,Reflex%Nref),stat=ierr)
       if (ierr /=0) then
          err_CFML%Flag=.true.
          err_CFML%Msg=" Unable to allocate mFR"
          return
       end if
       mFR=0.0

       !---- HR Table ----!
       if (allocated(HR)) deallocate(HR)
       allocate(HR(Multr,Reflex%Nref),stat=ierr)
       if (ierr /=0) then
          err_CFML%Flag=.true.
          err_CFML%Msg=" Unable to allocate HR"
          return
       end if
       HR=HR_Type(0)

       !---- HT Table ----!
       if (allocated(HT)) deallocate(HT)
       allocate(HT(Multr,Reflex%Nref),stat=ierr)
       if (ierr /=0) then
          err_CFML%Flag=.true.
          err_CFML%Msg=" Unable to allocate HTR"
          return
       end if
       HT=0.0

       if (allocated(TH)) deallocate(TH)
       allocate(TH(Natm,Reflex%Nref),stat=ierr)
       if (ierr /=0) then
          err_CFML%Flag=.true.
          err_CFML%Msg=" Unable to allocate HTR"
          return
       end if
       TH=0.0

       if (allocated(Ajh)) deallocate(Ajh)
       allocate(Ajh(3,Natm,Reflex%Nref), stat=ierr)
       if (ierr /=0) then
          err_CFML%Flag=.true.
          err_CFML%Msg=" Unable to allocate Aj(h)"
          return
       end if
       Ajh=0.0

       if (allocated(Bjh)) deallocate(Bjh)
       allocate(Bjh(3,Natm,Reflex%Nref), stat=ierr)
       if (ierr /=0) then
          err_CFML%Flag=.true.
          err_CFML%Msg=" Unable to allocate Bj(h)"
          return
       end if
       Bjh=0.0

       if (present(lun)) then
          call Set_Fixed_Tables(Reflex,Atm,Grp,lun=lun)
       else
          call Set_Fixed_Tables(Reflex,Atm,Grp)
       end if

       if (.not. err_CFML%Flag) MSF_Initialized=.true.

    End Subroutine Init_Mag_Structure_Factors

    !!--++
    !!--++ Module Subroutine Create_Table_mFR(Reflex,Atm,Lun)
    !!--++    type(MagH_List_Type),   intent(in) :: Reflex
    !!--++    type(Matom_list_type),  intent(in) :: Atm
    !!--++    integer, optional,      intent(in) :: lun
    !!--++
    !!--++    (Private)
    !!--++    Calculate a Table of Magnetic form Factors
    !!--..     mFR(Natoms,Nref)
    !!--++
    !!--++ Update: April - 2005
    !!
    Module Subroutine Create_Table_mFR(Reflex,Atm,lun)
       !---- Arguments ----!
       type(MagH_List_Type),   intent(in) :: Reflex
       type(Matom_list_type),  intent(in) :: Atm
       integer, optional,      intent(in) :: lun

       !---- Local Variables ----!
       character(len=4)               :: symbcar
       integer                        :: i,j, k,n,L
       integer, dimension(atm%natoms) :: ix,jx,ia

       !---- Init ----!
       err_CFML%Flag=.false.

       !---- Load form factor values for Magnetic Scattering ----!
       call Set_Magnetic_Form()

       !---- Find Species in Magnetic_Form ----!
       ix=0
       jx=0
       n=0
       do i=1,atm%natoms
          symbcar=u_case(atm%atom(i)%SfacSymb)
          do j=1,num_mag_form
             if (symbcar /= Magnetic_Form(j)%Symb) cycle
             ix(i)=j
             if(any(jx == j) ) exit
             n=n+1
             jx(n)=j
             ia(n)=i
             exit
          end do
       end do

       if (present(lun)) then
          write(unit=lun,fmt="(/,a)") "  INFORMATION FROM TABULATED MAGNETIC FORM FACTORS"
          write(unit=lun,fmt="(a,/)") "  ================================================"
       End if

       if (any(ix==0)) then
          err_CFML%Flag=.true.
          err_CFML%Msg="The Species "//symbcar//" was not found"
          return
       else
          !---- Fill mFR Table ----!
          do j=1,reflex%nref
             do i=1,atm%natoms
                mFR(i,j)=mfj(reflex%Mh(j)%s,Magnetic_Form(ix(i))%SctM)
             end do
          end do
       end if

       !---- Printing Information ----!
       if (present(lun)) then
          write(unit=lun,fmt="(/,a,/)")    "   MAGNETIC FORM FACTOR COEFFICIENTS: {A(i),B(i),I=1,3},C  "
          write(unit=lun,fmt="(a,i3)")     "   Number of chemically different species: ",n
          write(unit=lun,fmt="(/,a)") &
               "     Atom     a1       b1       a2       b2       a3       b3       c      "
          do k=1,n
             j = jx(k)
             i = ia(k)
             write(unit=lun,fmt="(a,7F9.5)")    &
                           "     "//atm%atom(i)%SfacSymb, (Magnetic_Form(j)%SctM(L), L=1,7)
          end do
          write(unit=lun,fmt="(/,/)")
       end if

       call Remove_Magnetic_Form()

    End Subroutine Create_Table_mFR

    !!--++
    !!--++ Subroutine Set_Fixed_Tables(Reflex,Atm,Grp,lun)
    !!--++    type(MagH_List_Type),         intent(in) :: Reflex
    !!--++    type(Matom_list_type),        intent(in) :: Atm
    !!--++    type(MagSymm_k_Type),         intent(in) :: Grp
    !!--++    integer, optional,            intent(in) :: lun
    !!--++
    !!--++    (Private)
    !!--++    Calculates arrays that are fixed during all further
    !!--++    calculations
    !!--++
    !!--++ Update: April - 2005
    !!
    Module Subroutine Set_Fixed_Tables(Reflex,Atm,Grp,lun)
       !---- Arguments ----!
       type(MagH_List_Type),         intent(in) :: Reflex
       type(Matom_list_type),        intent(in) :: Atm
       type(MagSymm_k_Type),         intent(in) :: Grp
       integer, optional,            intent(in) :: lun

       !---- Local variables ----!

       !---- Table HR - HT ----!
       call Create_Table_HR_HT(Reflex,Grp)

       !---- Table mFR ----!
       if (present(lun)) then
          call Create_Table_mFR(Reflex,Atm,lun=lun)
       else
          call Create_Table_mFR(Reflex,Atm)
       end if

       !---- Modify the scattering factor tables to include the
       !---- multipliers factors concerning centre of symmetry and
       !---- centred translations
       if (Grp%mCentred == 2) mFR=2.0*mFR
       if (Grp%Num_Lat  > 1)  mFR=Grp%Num_Lat*mFR
    End Subroutine Set_Fixed_Tables

    !!--++
    !!--++ Module Subroutine Calc_Table_MAB(Cell,Mlist,Atm,Mgp)
    !!--++    type(Cell_G_Type),intent(in) :: Cell
    !!--++    type(MagH_List_Type),   intent(in) :: MList
    !!--++    type(Matom_list_type),  intent(in) :: Atm
    !!--++    type(MagSymm_k_type),   intent(in) :: MGp
    !!--++
    !!--++    (Private)
    !!--++    Calculate Table with Aj(h) and Bj(h) values
    !!--++
    !!--++ Update: April - 2005, November 2014 (JRC)
    !!
    Module Subroutine Calc_Table_Mab(Cell,Mlist,Atm,Mgp)
       !---- Arguments ----!
       type(Cell_G_Type),      intent(in) :: Cell
       type(MagH_List_Type),   intent(in) :: MList
       type(Matom_list_type),  intent(in) :: Atm
       type(MagSymm_k_type),   intent(in) :: MGp

       !---- Local Variables ----!
       integer                       :: i,j,k,n,nvk,m, NRef
       real(kind=cp)                 :: arg,anis,onh,ph, x, isig
       real(kind=cp),dimension(3)    :: h
       complex(kind=cp),dimension(3) :: Sk,GMh
       complex(kind=cp)              :: ci
       real(kind=cp),dimension(6)    :: beta
       real(kind=cp),dimension(3,3)  :: Mcos,Msin
       real(kind=cp), dimension(3)   :: ar,ai,br,bi

       Ajh=0.0
       Bjh=0.0
       Nref=MList%Nref
       if (MGp%Centred == 2) then
          do j=1,Nref
             if (.not. Mlist%Mh(j)%keqv_minus ) then
                onh=0.5
             else
                onh=1.0
             end if
             nvk= Mlist%Mh(j)%num_k
             isig=Mlist%Mh(j)%signp

             do i=1,Atm%natoms
                m= Atm%Atom(i)%imat(nvk)
                if(m == 0) cycle
                Mcos=0.0
                do k=1,MgP%NumOps
                   h=hr(k,j)%h
                   ph= isig * (Atm%atom(i)%Mphas(m) + MGp%MSymOp(k,m)%Phas)
                   arg=tpi*(dot_product(h,Atm%atom(i)%x)+ht(k,j) + ph)
                   anis=1.0
                   if(Atm%atom(i)%thtype == "aniso") then
                      beta=Atm%atom(i)%u(1:6)
                      anis=     h(1)*h(1)*beta(1)+     h(2)*h(2)*beta(2)+    h(3)*h(3)*beta(3) &
                           +2.0*h(1)*h(2)*beta(4)+ 2.0*h(1)*h(3)*beta(5)+2.0*h(2)*h(3)*beta(6)
                      anis=exp(-anis)
                   end if
                   Mcos(:,:)=Mcos(:,:)+cos(arg)*anis*MGp%MSymOp(k,m)%Rot(:,:)
                end do ! symmetry
                Ajh(:,i,j) = onh*matmul(Mcos,Atm%atom(i)%SkR(:,nvk)/Cell%cell)*REAL(MGp%mcentred,kind=cp)*Cell%cell
             end do ! Atoms
          end do ! Reflections

       else

          if (MGp%nirreps == 0) then
             do j=1,Nref
                if (.not. Mlist%Mh(j)%keqv_minus ) then
                   onh=0.5
                else
                   onh=1.0
                end if
                isig=Mlist%Mh(j)%signp
                nvk= Mlist%Mh(j)%num_k

                do i=1,Atm%natoms
                   m= Atm%Atom(i)%imat(nvk)
                   if (m == 0) cycle

                   Mcos=0.0 ; Msin=0.0
                   do k=1,MGp%NumOps
                      h=hr(k,j)%h
                      ph = isig * (Atm%atom(i)%Mphas(m) + MGp%MSymOp(k,m)%Phas)
                      arg=tpi*(dot_product(h,Atm%atom(i)%x)+ht(k,j) + ph)
                      anis=1.0
                      if (Atm%atom(i)%thtype == "aniso") then
                         beta=Atm%atom(i)%u(1:6)
                         anis=     h(1)*h(1)*beta(1)+     h(2)*h(2)*beta(2)+    h(3)*h(3)*beta(3) &
                              +2.0*h(1)*h(2)*beta(4)+ 2.0*h(1)*h(3)*beta(5)+2.0*h(2)*h(3)*beta(6)
                         anis=exp(-anis)
                      end if
                      Mcos(:,:)=Mcos(:,:)+cos(arg)*anis*MGp%MSymOp(k,m)%Rot(:,:)
                      Msin(:,:)=Msin(:,:)+sin(arg)*anis*MGp%MSymOp(k,m)%Rot(:,:)
                   end do ! symmetry
                   ar =       onh*matmul(Mcos,Atm%atom(i)%SkR(:,nvk)/Cell%cell)*Cell%cell
                   ai = -isig*onh*matmul(Mcos,Atm%atom(i)%SkI(:,nvk)/Cell%cell)*Cell%cell
                   br =       onh*matmul(Msin,Atm%atom(i)%SkR(:,nvk)/Cell%cell)*Cell%cell
                   bi = -isig*onh*matmul(Msin,Atm%atom(i)%SkI(:,nvk)/Cell%cell)*Cell%cell

                   Ajh(:,i,j) = ar(:) - bi(:)
                   Bjh(:,i,j) = br(:) + ai(:)
                end do ! Atoms
             end do ! Reflections

          else   !now magnetic structure described in terms of basis functions

             do j=1,Nref
                if (.not. Mlist%Mh(j)%keqv_minus ) then
                   onh=0.5
                else
                   onh=1.0
                end if
                nvk= Mlist%Mh(j)%num_k
                isig=Mlist%Mh(j)%signp

                do i=1,Atm%natoms
                   m= Atm%Atom(i)%imat(nvk)
                   if(m == 0) cycle
                   GMh=cmplx(0.0,0.0)
                   do k=1,MGp%NumOps
                      h=hr(k,j)%h
                      ph= isig* Atm%atom(i)%Mphas(m)
                      arg=tpi*(dot_product(h,Atm%atom(i)%x)+ht(k,j) + ph)
                      anis=1.0
                      if (Atm%atom(i)%thtype == "aniso") then
                         beta=Atm%atom(i)%u(1:6)
                         anis=     h(1)*h(1)*beta(1)+     h(2)*h(2)*beta(2)+    h(3)*h(3)*beta(3) &
                              +2.0*h(1)*h(2)*beta(4)+ 2.0*h(1)*h(3)*beta(5)+2.0*h(2)*h(3)*beta(6)
                         anis=exp(-anis)
                      end if
                      Sk(:) = cmplx(0.0,0.0)
                      do n=1,abs(MGp%nbas(m)) !cannot be greater than 12 at present
                         x=real(MGp%icomp(n,m),kind=cp)
                         ci=cmplx(1.0-x,-isig*x)
                         Sk(:)=Sk(:)+ Atm%atom(i)%cbas(n,nvk)*ci*cmplx(  Real(MGp%basf(:,n,k,m)), -isig*aimag(MGp%basf(:,n,k,m))  )
                      end do
                      GMh(:)=GMh(:) + onh*anis*Sk(:)*CMPLX(COS(arg),SIN(arg))  !Atomic contribution to geometric Magnetic Structure Factor
                   end do ! symmetry

                   Ajh(:,i,j) = real(GMh)
                   Bjh(:,i,j) = aimag(GMh)

                end do ! Atoms
             end do ! Reflections
          end if
       end if

    End Subroutine Calc_Table_Mab

    !!--++
    !!--++ Module Subroutine Calc_Table_Th(Reflex,Atm)
    !!--++    type(MagH_List_Type),   intent(in) :: Reflex
    !!--++    type(Matom_list_type),  intent(in) :: Atm
    !!--++
    !!--++    (Private)
    !!--++    Calculate the Table of Isotropic Thermal contribution and occupation
    !!--..    TH(Natoms,Nref) multiplied by pn= 0.2695420113693928312
    !!--++
    !!--++ Update: April - 2005
    !!
    Module Subroutine Calc_Table_Th(Reflex,Atm)
       !---- Argument ----!
       type(MagH_List_Type),   intent(in) :: Reflex
       type(Matom_list_type),  intent(in) :: Atm

       !---- Local variables ----!
       integer          :: i,j
       real(kind=cp)    :: b,s

       !---- Isotropic Debye-Waller factor * occupation * p=0.5*re*gamma ----!
       do j=1,reflex%nref
          s=reflex%Mh(j)%s
          do i=1,atm%natoms
             b=atm%atom(i)%biso
             th(i,j)= pn*atm%atom(i)%occ*exp(-b*s*s)
          end do
       end do
    End Subroutine Calc_Table_Th

    !!--++
    !!--++ Module Subroutine Create_Table_Hr_Ht(Reflex,Grp)
    !!--++    type(MagH_List_Type),   intent(in) :: Reflex
    !!--++    type(MagSymm_k_type),   intent(in) :: Grp
    !!--++
    !!--++    (Private)
    !!--++    Calculate a Table with HR and HT values
    !!--..       Hr(Grp%Numops,Reflex%Nref)
    !!--..       HT(Grp%Numops,Reflex%Nref)
    !!--++
    !!--++ Update: April - 2005
    !!
    Module Subroutine Create_Table_Hr_Ht(Reflex,Grp)
       !---- Arguments ----!
       type(MagH_List_Type),   intent(in) :: Reflex
       type(MagSymm_k_type),   intent(in) :: Grp

       !---- Local Variables ----!
       integer :: i,j

       do j=1,reflex%nref
          do i=1,grp%NumOps
             hr(i,j)%h=matmul(reflex%Mh(j)%h,Grp%symop(i)%Rot)
             ht(i,j)=dot_product(real(reflex%Mh(j)%h,kind=cp),Grp%SymOp(i)%Tr)
          end do
       end do

    End Subroutine Create_Table_HR_HT

    !!--++
    !!--++ Module Subroutine Sum_MAB(Reflex,Natm,icent)
    !!--++    type(MagH_List_Type),   intent(in out) :: Reflex
    !!--++    integer,                intent(in)     :: Natm
    !!--++    integer,                intent(in)     :: icent
    !!--++
    !!--++    (Private)
    !!--++    Calculate the Final Sum for Structure Factors calculations
    !!--++
    !!--++ Update: April - 2005
    !!
    Module Subroutine Sum_MAB(Reflex,Natm,icent)
       !---- Arguments ----!
       type(MagH_List_Type), intent(in out)  :: Reflex
       integer,              intent(in)      :: Natm
       integer,              intent(in)      :: icent

       !---- Local Variables ----!
       integer                      :: i,j
       real(kind=cp), dimension(3)  :: aa,bb


       !---- Fj(h)*Aj(h) ----!
       if (icent == 2) then    !Calculation for centrosymmetric structures
          do j=1,reflex%nref
             aa=0.0
             do i=1,Natm
                aa(:)= aa(:) + mFR(i,j)*th(i,j)*ajh(:,i,j)
             end do
             Reflex%Mh(j)%MsF(:)=cmplx(aa(:))
          end do

       else       !Calculation for non-centrosymmetric structures
          !---- Final Sum ----!
          do j=1,reflex%Nref
             aa=0.0
             bb=0.0
             do i=1,Natm
                aa(:)= aa(:) + mFR(i,j)*th(i,j)*ajh(:,i,j)
                bb(:)= bb(:) + mFR(i,j)*th(i,j)*bjh(:,i,j)
             end do
             Reflex%Mh(j)%MsF(:)=cmplx(aa(:),bb(:))
          end do
       end if
    End Subroutine Sum_MAB

    !!----
    !!---- Module Subroutine Mag_Structure_Factors(Cell,Atm,Grp,Reflex)
    !!----    !---- Arguments ----!
    !!----    type(Cell_G_Type),  intent(in)     :: Cell
    !!----    type(Matom_list_type),    intent(in)     :: Atm
    !!----    type(MagSymm_k_Type),     intent(in)     :: Grp
    !!----    type(MagH_List_Type),     intent(in out) :: Reflex
    !!----
    !!----    Calculate the Magnetic Structure Factors from a list of magnetic Atoms
    !!----    and a set of reflections. A call to Init_Mag_Structure_Factors
    !!----    is a pre-requisite for using this subroutine. In any case
    !!----    the subroutine calls Init_Mag_Structure_Factors if SF_initialized=.false.
    !!----    The argument "Cell" has been added in order to consider whatever kind of
    !!----    settings for symmetry operators.
    !!----
    !!---- Update: April - 2005, November 2014 (JRC)
    !!
    Module Subroutine Mag_Structure_Factors(Cell,Atm,Grp,Reflex)
       !---- Arguments ----!
       type(Cell_G_Type),        intent(in)     :: Cell
       type(Matom_list_type),    intent(in)     :: Atm
       type(MagSymm_k_Type),     intent(in)     :: Grp
       type(MagH_List_Type),     intent(in out) :: Reflex


       call Clear_Error()
       if(.not. MSF_Initialized) call Init_Mag_Structure_Factors(Reflex,Atm,Grp)

       !---- Table TH ----!
       Call Calc_Table_TH(Reflex,Atm)

       !---- Table AB ----!
       call Calc_Table_MAB(Cell,Reflex,Atm,Grp)

       !---- Final Calculation ----!
       call Sum_MAB(Reflex,Atm%Natoms,Grp%Centred)

    End Subroutine Mag_Structure_Factors

End SubModule kStrf_Init_FxTables
