SubModule (CFML_kvec_Structure_Factors) kStrf_MiV
   implicit none
   Contains
    !!----
    !!---- Module Subroutine Calc_Mag_Interaction_Vector(Reflex,Cell)
    !!----    type(MagH_List_Type),     intent(in out) :: Reflex
    !!----    type(Cell_G_Type),  intent(in)     :: Cell
    !!----
    !!----    Calculate the Magnetic Interaction vector from Magnetic
    !!----    Structure factors, reflections and cell parameters.
    !!----    The components are given with respect to the crystallographic
    !!----    unitary direct cell system: {e1,e2,e3} and with respect to
    !!----    the Cartesian frame defined in Cell.
    !!----
    !!---- Updated: April - 2005, June-2012 (JRC,"mode" removed)
    !!
    Module Subroutine Calc_Mag_Interaction_Vector(Reflex,Cell)
       !---- Argument ----!
       type(MagH_List_Type), intent(in out) :: Reflex
       type(Cell_G_Type),    intent(in)     :: Cell

       !---- Local variables ----!
       integer                       :: j
       real(kind=cp)                 :: s
       real(kind=cp),dimension(3)    :: ed,er
       complex(kind=cp),dimension(3) :: M, MiV,MiVC

       !---- Calculation of the Magnetic Interaction vector (in unitary and Cartesian Crystal Frames)----!
       do j=1,reflex%nref
          s  = 2.0*reflex%Mh(j)%s  !1/d=r*, M = M// + Mp   => Mp = M - M// = M - (M.e)e
          er = reflex%Mh(j)%h/s    !unitary vector referred to the reciprocal basis
          ed = matmul(cell%GR,er)  !  "        "       "             direct    "
          M  = reflex%Mh(j)%MsF / Cell%cell    !Magnetic structure factor in basis {a,b,c}
          MiV = M - dot_product(er,M) * ed     !Magnetic interaction vector in basis {a,b,c}
          reflex%Mh(j)%MiV =  MiV * Cell%cell  !Magnetic interaction vector in basis {e1,e2,e3}
          MiVC  = matmul(Cell%Cr_Orth_cel,MiV) !Magnetic interaction vector in Cartesian components
          reflex%Mh(j)%MiVC =  MiVC
          reflex%Mh(j)%sqMiV= dot_product(MiVC, MiVC)
       end do
    End Subroutine Calc_Mag_Interaction_Vector

    !!----
    !!---- Module Subroutine Calc_Magnetic_Strf_Miv(Cell,Mgp,Atm,Mh)
    !!----    type(Cell_G_Type),  intent(in)     :: Cell
    !!----    type(MagSymm_k_Type),     intent(in)     :: MGp
    !!----    type(Matom_list_type),    intent(in)     :: Atm
    !!----    type(MagH_Type),          intent(in out) :: Mh
    !!----
    !!----    Calculate the Magnetic Interaction vector from Magnetic
    !!----    Structure factors, reflections and cell parameters.
    !!----    Whatever kind of settings for symmetry operators is allowed.
    !!----    The components are given with respect to the crystallographic
    !!----    unitary direct cell system: {e1,e2,e3} and with respect to the
    !!----    Cartesian frame defined in Cell.
    !!----
    !!---- Updated: April - 2005, June 2012, November 2014 (JRC)
    !!
    Module Subroutine Calc_Magnetic_Strf_Miv(Cell,Mgp,Atm,Mh)
       !---- Arguments ----!
       type(Cell_G_Type),        intent(in)     :: Cell
       type(MagSymm_k_Type),     intent(in)     :: MGp
       type(Matom_list_type),    intent(in)     :: Atm
       type(MagH_Type),          intent(in out) :: Mh

       !---- Local Variables ----!
       integer                            :: i,j,k,nvk,m, n
       real(kind=cp)                      :: arg,anis,onh,ph,s,b,ht,mFF,tho, isig, x
       real(kind=cp),    dimension(3)     :: h,ed,er
       real(kind=cp),    dimension(6)     :: beta
       real(kind=cp),    dimension(3,3)   :: Mcos,Msin
       real(kind=cp),    dimension(3)     :: ar,ai,br,bi,Ajh,Bjh,aa,bb
       complex(kind=cp)                   :: ci
       complex(kind=cp), dimension(3)     :: Mc, MiV, Sk, GMh

       s=Mh%s
       if (.not. Mh%keqv_minus ) then
          onh=0.5
       else
          onh=1.0
       end if
       nvk= Mh%num_k
       aa=0.0; bb=0.0
       isig=Mh%signp

       if (MGp%nirreps == 0) then

          do i=1,Atm%natoms
             m= Atm%Atom(i)%imat(nvk)
             if(m == 0) cycle  !Calculate only with contributing atoms

             !---- Isotropic Debye-Waller factor * occupation * p=0.5*re*gamma * Magnetic form-factors mFF
             b=atm%atom(i)%biso
             j=atm%atom(i)%ind(2)  !pointer to the magnetic form factor coefficients
             mFF=mfj(s,Magnetic_Form(j)%SctM)
             tho= pn*atm%atom(i)%occ*mFF*exp(-b*s*s)

             Mcos=0.0 ; Msin=0.0

             do k=1,MGp%NumOps
                !h=Hkl_R(Mh%h,MGp%symop(k))
                h=matmul(Mh%h,MGp%symop(k)%Rot)
                ht=dot_product(Mh%h,MGp%SymOp(k)%Tr)
                ph= isig * (Atm%atom(i)%Mphas(m) + MGp%MSymOp(k,m)%Phas)
                arg=tpi*(dot_product(h,Atm%atom(i)%x)+ht + ph)
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

             ar =       onh*matmul(Mcos,Atm%atom(i)%SkR(:,nvk)/Cell%cell)*Cell%cell  !The introduction of Cell%cell
             ai = -isig*onh*matmul(Mcos,Atm%atom(i)%SkI(:,nvk)/Cell%cell)*Cell%cell  !is for handling the possibility
             br =       onh*matmul(Msin,Atm%atom(i)%SkR(:,nvk)/Cell%cell)*Cell%cell  !of using non conventional settings for
             bi = -isig*onh*matmul(Msin,Atm%atom(i)%SkI(:,nvk)/Cell%cell)*Cell%cell  !symmetry operators
             Ajh(:) = ar(:) - bi(:)
             Bjh(:) = br(:) + ai(:)

             aa(:)= aa(:) + tho*ajh(:)
             bb(:)= bb(:) + tho*bjh(:)

          end do ! Atoms
          Mh%MsF(:)=cmplx(aa(:),bb(:)) * MGp%Num_Lat * MGp%Centred

       else  !Now magnetic structure described in terms of basis functions (No magnetic rotation matrices are provided)

          Mh%MsF(:)=cmplx(0.0,0.0)
          do i=1,Atm%natoms
             m= Atm%Atom(i)%imat(nvk)
             if(m == 0) cycle  !Calculate only with contributing atoms
             !---- Isotropic Debye-Waller factor * occupation * p=0.5*re*gamma * Magnetic form-factors mFF
             b=atm%atom(i)%biso
             j=atm%atom(i)%ind(2)  !pointer to the magnetic form factor coefficients
             mFF=mfj(s,Magnetic_Form(j)%SctM)
             tho= pn*atm%atom(i)%occ*mFF*exp(-b*s*s)

             GMh(:)=cmplx(0.0,0.0)

             do k=1,MGp%NumOps
                !h=Hkl_R(Mh%h,MGp%symop(k))
                h=matmul(Mh%h,MGp%symop(k)%Rot)
                ht=dot_product(Mh%h,MGp%SymOp(k)%Tr)
                ph= isig*Atm%atom(i)%Mphas(m)
                arg=tpi*(dot_product(h,Atm%atom(i)%x)+ht + ph)
                anis=1.0
                if (Atm%atom(i)%thtype == "aniso") then
                   beta=Atm%atom(i)%u(1:6)
                   anis=     h(1)*h(1)*beta(1)+     h(2)*h(2)*beta(2)+    h(3)*h(3)*beta(3) &
                        +2.0*h(1)*h(2)*beta(4)+ 2.0*h(1)*h(3)*beta(5)+2.0*h(2)*h(3)*beta(6)
                   anis=exp(-anis)
                end if
                Sk(:) = cmplx(0.0,0.0)
                do n=1,abs(MGp%nbas(m)) !cannot be greater than 12 at present
                   x=real(MGp%icomp(n,m))
                   ci=cmplx(1.0-x,-isig*x)
                   Sk(:)=Sk(:)+ Atm%atom(i)%cbas(n,nvk)*ci* cmplx(  Real(MGp%basf(:,n,k,m)), -isig*aimag(MGp%basf(:,n,k,m))  )
                end do
                GMh(:)=GMh(:) + anis*Sk(:)*CMPLX(COS(arg),SIN(arg))  !Atomic contribution to geometric Magnetic Structure Factor
             end do ! symmetry
             Mh%MsF(:)=Mh%MsF(:) + tho*onh*GMh(:)
          end do ! Atoms
       end if
       Mh%MsF(:)=Mh%MsF(:) * MGp%Num_Lat * MGp%Centred
       !---- Calculation of the Magnetic Interaction vector ----!
       s  = 2.0*Mh%s            !1/d=r*, M = M// + Mp   => Mp = M - M// = M - (M.e)e
       er = Mh%h/s              !unitary vector referred to the reciprocal basis
       ed = matmul(cell%GR,er)  !  "        "       "             direct    "
       Mc  = Mh%MsF / Cell%cell                !Magnetic structure factor in basis {a,b,c}
       MiV = Mc - dot_product(er,Mc) * ed      !Magnetic interaction vector in basis {a,b,c}
       Mh%MiV  =  MiV * Cell%cell              !Magnetic interaction vector in basis {e1,e2,e3}
       Mh%MiVC = matmul(Cell%Cr_Orth_cel,MiV)  !Magnetic interaction vector in Cartesian components
       Mh%sqMiV= dot_product(Mh%MiVC, Mh%MiVC)
    End Subroutine Calc_Magnetic_StrF_MiV

    !!----
    !!---- Module Subroutine Calc_Magnetic_Strf_Miv_Dom(Cell,Mgp,Atm,Mag_Dom,Mh)
    !!----    type(Cell_G_Type),   intent(in)     :: Cell
    !!----    type(MagSymm_k_Type),      intent(in)     :: MGp
    !!----    type(Matom_list_type),     intent(in)     :: Atm
    !!----    type(Magnetic_Domain_type),intent(in)     :: Mag_Dom
    !!----    type(MagHD_Type),          intent(in out) :: Mh
    !!----
    !!----    Calculate the Magnetic Interaction vector from Magnetic
    !!----    Structure factors, reflections and cell parameters.
    !!----    Whatever kind of settings for symmetry operators is allowed.
    !!----    The components are given with respect to the crystallographic
    !!----    unitary direct cell system: {e1,e2,e3} and with respect to
    !!----    Cartesian frame defined in Cell.
    !!----    In this subroutine the presence of magnetic domains is
    !!----    taken into account
    !!----
    !!---- Updated: September - 2010, July-2012, November 2014 (JRC)
    !!
    Module Subroutine Calc_Magnetic_Strf_Miv_Dom(Cell,Mgp,Atm,Mag_Dom,Mh)
       !---- Arguments ----!
       type(Cell_G_Type),      intent(in)      :: Cell
       type(MagSymm_k_Type),      intent(in)      :: MGp
       type(Matom_list_type),     intent(in)      :: Atm
       type(Magnetic_Domain_type),intent(in)      :: Mag_Dom
       type(MagHD_Type),          intent(in out)  :: Mh

       !---- Local Variables ----!
       integer                            :: i,j,k,nvk,m, n, nd, ich, nch
       real(kind=cp)                      :: arg,anis,onh,ph,s,b,ht,mFF,tho, isig, x
       real(kind=cp),    dimension(3)     :: h,ed,er,h_dom,xpos
       real(kind=cp),    dimension(6)     :: beta
       real(kind=cp),    dimension(3,3)   :: Mcos,Msin
       real(kind=cp),    dimension(3)     :: ar,ai,br,bi,Ajh,Bjh,aa,bb,Skr,Ski

       complex(kind=cp)                   :: ci
       complex(kind=cp), dimension(3)     :: Mc, MiV, Sk, GMh
       real(kind=cp),dimension(2), parameter :: ch=(/1.0,-1.0/)

       s=Mh%s
       if (.not. Mh%keqv_minus ) then
          onh=0.5
       else
          onh=1.0
       end if
       nvk= Mh%num_k

       isig=Mh%signp
       nch=1
       if (Mag_Dom%chir) nch=2

       if (MGp%nirreps == 0) then

          do nd=1,Mag_Dom%nd
             Mh%MsF(:,:,nd)=cmplx(0.0,0.0)
             do ich=1,nch
                aa=0.0; bb=0.0
                if(Mag_Dom%twin) then
                  h_dom=matmul(Mh%h,real(Mag_Dom%Dmat(:,:,nd)))
                else
                  h_dom=Mh%h
                end if
                do i=1,Atm%natoms
                   m= Atm%Atom(i)%imat(nvk)
                   if (m == 0) cycle  !Calculate only with contributing atoms
                   if(Mag_Dom%twin) then
                     Skr=Atm%atom(i)%SkR(:,nvk)
                     Ski=Atm%atom(i)%SkI(:,nvk)
                   else
                     Skr= matmul(Mag_Dom%Dmat(:,:,nd),Atm%atom(i)%SkR(:,nvk))
                     Ski= matmul(Mag_Dom%Dmat(:,:,nd),ch(ich)*Atm%atom(i)%SkI(:,nvk))
                   end if
                   !---- Isotropic Debye-Waller factor * occupation * p=0.5*re*gamma * Magnetic form-factors mFF
                   b=atm%atom(i)%biso
                   j=atm%atom(i)%ind(2)  !pointer to the magnetic form factor coefficients
                   mFF=mfj(s,Magnetic_Form(j)%SctM)
                   tho= pn*atm%atom(i)%occ*mFF*exp(-b*s*s)

                   Mcos=0.0
                   Msin=0.0
                   xpos=Atm%atom(i)%x
                   if(Mag_Dom%trans) xpos=matmul(Mag_Dom%Dmat(:,:,nd),xpos) + Mag_Dom%Dt(:,nd)

                   do k=1,MGp%NumOps
                      !h=Hkl_R(h_dom,MGp%symop(k))
                      h=matmul(h_dom,MGp%symop(k)%Rot)
                      ht=dot_product(h_dom,MGp%SymOp(k)%Tr)
                      ph= isig * (Atm%atom(i)%Mphas(m) + MGp%MSymOp(k,m)%Phas)
                      arg=tpi*(dot_product(h,xpos)+ht + ph)
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

                   ar =       onh*matmul(Mcos,SkR(:)/Cell%cell)*Cell%cell  !The introduction of Cell%cell
                   ai = -isig*onh*matmul(Mcos,SkI(:)/Cell%cell)*Cell%cell  !is for handling the possibility
                   br =       onh*matmul(Msin,SkR(:)/Cell%cell)*Cell%cell  !of using non conventional settings for
                   bi = -isig*onh*matmul(Msin,SkI(:)/Cell%cell)*Cell%cell  !symmetry operators
                   Ajh(:) = ar(:) - bi(:)
                   Bjh(:) = br(:) + ai(:)

                   aa(:)= aa(:) + tho*ajh(:)
                   bb(:)= bb(:) + tho*bjh(:)

                end do ! Atoms
                Mh%MsF(:,ich,nd)=cmplx(aa(:),bb(:)) * MGp%Num_Lat * MGp%Centred
             end do ! Chirality Domains
          end do !Domains

       else  !Now magnetic structure described in terms of basis functions (No magnetic rotation matrices are provided)

          do nd=1,Mag_dom%nd
             Mh%MsF(:,:,nd)=cmplx(0.0,0.0)
             do ich=1,nch
                aa=0.0; bb=0.0
                if(Mag_Dom%twin) then
                  h_dom=matmul(Mh%h,real(Mag_Dom%Dmat(:,:,nd)))
                else
                  h_dom=Mh%h
                end if

                do i=1,Atm%natoms
                   m= Atm%Atom(i)%imat(nvk)
                   if (m == 0) cycle  !Calculate only with contributing atoms
                   xpos=Atm%atom(i)%x
                   if(Mag_Dom%trans) xpos=matmul(Mag_Dom%Dmat(:,:,nd),xpos) + Mag_Dom%Dt(:,nd)

                   !---- Isotropic Debye-Waller factor * occupation * p=0.5*re*gamma * Magnetic form-factors mFF
                   b=atm%atom(i)%biso
                   j=atm%atom(i)%ind(2)  !pointer to the magnetic form factor coefficients
                   mFF=mfj(s,Magnetic_Form(j)%SctM)
                   tho= pn*atm%atom(i)%occ*mFF*exp(-b*s*s)

                   GMh(:)=cmplx(0.0,0.0)

                   do k=1,MGp%NumOps
                      !h=Hkl_R(h_dom,MGp%symop(k))
                      h=matmul(h_dom,MGp%symop(k)%Rot)
                      ht=dot_product(h_dom,MGp%SymOp(k)%Tr)
                      ph= isig*Atm%atom(i)%Mphas(m)
                      arg=tpi*(dot_product(h,xpos)+ht + ph)
                      anis=1.0
                      if (Atm%atom(i)%thtype == "aniso") then
                         beta=Atm%atom(i)%u(1:6)
                         anis=     h(1)*h(1)*beta(1)+     h(2)*h(2)*beta(2)+    h(3)*h(3)*beta(3) &
                              +2.0*h(1)*h(2)*beta(4)+ 2.0*h(1)*h(3)*beta(5)+2.0*h(2)*h(3)*beta(6)
                         anis=exp(-anis)
                      end if
                      Sk(:) = cmplx(0.0,0.0)
                      do n=1,abs(MGp%nbas(m)) !cannot be greater than 12 at present
                         x=real(MGp%icomp(n,m))
                         ci=cmplx(1.0-x,-isig*x)
                         Sk(:)=Sk(:)+ Atm%atom(i)%cbas(n,nvk)*ci* cmplx(Real(MGp%basf(:,n,k,m)), -isig*aimag(MGp%basf(:,n,k,m)))
                      end do
                      Sk(:) = cmplx (real(Sk),ch(ich)*aimag(Sk))
                      if(.not. Mag_Dom%twin) then
                        Sk(:)=matmul(real(Mag_Dom%Dmat(:,:,nd)),Sk(:))
                      end if
                      GMh(:)=GMh(:) + anis*Sk(:)*CMPLX(COS(arg),SIN(arg))  !Atomic contribution to geometric Magnetic Structure Factor
                   end do ! symmetry
                   Mh%MsF(:,ich,nd)=Mh%MsF(:,ich,nd) + tho*onh*GMh(:)
                end do ! Atoms
                Mh%MsF(:,ich,nd)=Mh%MsF(:,ich,nd) * MGp%Num_Lat * MGp%Centred
             end do ! Chirality Domains
          end do ! Domains
       end if

       !---- Calculation of the Magnetic Interaction vectors ----!
       s  = 2.0*Mh%s            !1/d=r*  M = M// + Mp   => Mp = M - M// = M - (M.e)e
       er = Mh%h/s              !unitary vector referred to the reciprocal basis
       ed = matmul(cell%GR,er)  !  "        "       "             direct    "
       Mh%AMiV(:) = cmplx(0.0,0.0)
       Mh%sqMiV = 0.0
       do nd=1,Mag_dom%nd
          do ich=1,nch
             Mc  = Mh%MsF(:,ich,nd) / Cell%cell    !Magnetic structure factor in basis {a,b,c}
             MiV = Mc - dot_product(er,Mc) * ed    !Magnetic interaction vector in basis {a,b,c}
             Mh%MiVC(:,ich,nd)  = matmul(Cell%Cr_Orth_cel,MiV)  !Magnetic interaction vector in Cartesian components
             Mh%MiV(:,ich,nd) =  MiV * Cell%cell  !Magnetic interaction vector in basis {e1,e2,e3}
             Mh%AMiV(:)=Mh%AMiV(:)+ Mh%MiVC(:,ich,nd) * Mag_Dom%Pop(ich,nd)
             Mh%sqMiV = Mh%sqMiV + dot_product(Mh%MiVC(:,ich,nd), Mh%MiVC(:,ich,nd))* Mag_Dom%Pop(ich,nd)
          end do ! Chirality Domains
       end do ! Domains
       Mh%sqAMiV= dot_product(Mh%AMiV, Mh%AMiV)

    End Subroutine Calc_Magnetic_Strf_Miv_Dom

End SubModule kStrf_MiV
