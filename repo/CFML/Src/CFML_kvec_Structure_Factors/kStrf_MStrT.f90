SubModule (CFML_kvec_Structure_Factors) kStrf_MStrT
   implicit none
   Contains
    !!----
    !!---- Module Subroutine Modify_MSF(Reflex,Atm,Grp,List,Nlist)
    !!----    !---- Arguments ----!
    !!----    type(MagH_List_Type),         intent(in) :: Reflex
    !!----    type(Matom_list_type),        intent(in) :: Atm
    !!----    type(MagSymm_k_Type),         intent(in) :: Grp
    !!----    integer,dimension(:),         intent(in) :: List
    !!----    integer,                      intent(in) :: NList
    !!----
    !!----    Recalculation of Magnetic Structure Factors because a
    !!----    list of Atoms parameters were modified. The "List" variable
    !!----    contains the numbers in the list of the atoms to be changed.
    !!----
    !!---- Update: April - 2005
    !!
    Module Subroutine Modify_MSF(Reflex,Atm,Grp,List,Nlist)
       !---- Arguments ----!
       type(MagH_List_Type),         intent(in out) :: Reflex
       type(Matom_list_type),        intent(in)     :: Atm
       type(MagSymm_k_Type),         intent(in)     :: Grp
       integer,dimension(:),         intent(in)     :: List
       integer,                      intent(in)     :: NList

       !---- Local variables ----!
       integer                       :: i,j,k,ii,nvk,m, n
       real(kind=cp)                 :: arg,onh,anis,ph, isig, x
       real(kind=cp),dimension(3,3)  :: Mcos,Msin
       complex(kind=cp),dimension(3) :: Sk,GMh
       complex(kind=cp)              :: ci
       real(kind=cp),dimension(3)    :: ar,ai,br,bi,h
       real(kind=cp),dimension(6)    :: beta

       if (Grp%Centred == 2) then

          do j=1,Reflex%Nref
            if (.not. Reflex%Mh(j)%keqv_minus ) then
               onh=0.5
            else
               onh=1.0
            end if
            nvk= Reflex%Mh(j)%num_k
            isig=Reflex%Mh(j)%signp
             do ii=1,Nlist
                i=list(ii)
                m= Atm%Atom(i)%imat(nvk)
                arg=0.0
                Mcos=0.0
                do k=1,grp%NumOps
                   h=hr(k,j)%h
                   ph= isig*(Atm%atom(i)%Mphas(m) + Grp%MSymOp(k,m)%Phas)
                   arg=tpi*(dot_product(h,Atm%atom(i)%x)+ht(k,j) + ph)
                   anis=1.0
                   if (Atm%atom(i)%thtype == "aniso") then
                      beta=Atm%atom(i)%u(1:6)
                      anis=     h(1)*h(1)*beta(1)+     h(2)*h(2)*beta(2)+    h(3)*h(3)*beta(3) &
                           +2.0*h(1)*h(2)*beta(4)+ 2.0*h(1)*h(3)*beta(5)+2.0*h(2)*h(3)*beta(6)
                      anis=exp(-anis)
                   end if
                   Mcos(:,:)=Mcos(:,:)+cos(arg)*anis*Grp%MSymOp(k,m)%Rot(:,:)
                end do ! symmetry
                ar =  onh*matmul(Mcos,Atm%atom(i)%SkR(:,nvk))
                Ajh(:,i,j)= ar(:)
             end do ! NList
          end do ! Reflections

       else

          if (Grp%nirreps == 0) then
             do j=1,Reflex%Nref
                if (.not. Reflex%Mh(j)%keqv_minus ) then
                   onh=0.5
                else
                   onh=1.0
                end if
                nvk= Reflex%Mh(j)%num_k
                isig=Reflex%Mh(j)%signp
                do ii=1,Nlist
                   i=list(ii)
                   m= Atm%Atom(i)%imat(nvk)
                   arg=0.0
                   Mcos=0.0 ; Msin=0.0
                   do k=1,grp%NumOps
                      h=hr(k,j)%h
                      ph=isig*(Atm%atom(i)%Mphas(m) + Grp%MSymOp(k,m)%Phas)
                      arg=tpi*(dot_product(h,Atm%atom(i)%x)+ht(k,j) + ph)
                      anis=1.0
                      if (Atm%atom(i)%thtype == "aniso") then
                         beta=Atm%atom(i)%u(1:6)
                         anis=     h(1)*h(1)*beta(1)+     h(2)*h(2)*beta(2)+    h(3)*h(3)*beta(3) &
                              +2.0*h(1)*h(2)*beta(4)+ 2.0*h(1)*h(3)*beta(5)+2.0*h(2)*h(3)*beta(6)
                         anis=exp(-anis)
                      end if
                      Mcos(:,:)=Mcos(:,:)+cos(arg)*anis*Grp%MSymOp(k,m)%Rot(:,:)
                      Msin(:,:)=Msin(:,:)+sin(arg)*anis*Grp%MSymOp(k,m)%Rot(:,:)
                   end do ! symmetry
                   ar =       onh*matmul(Mcos,Atm%atom(i)%SkR(:,nvk))
                   ai = -isig*onh*matmul(Mcos,Atm%atom(i)%SkI(:,nvk))
                   br =       onh*matmul(Msin,Atm%atom(i)%SkR(:,nvk))
                   bi = -isig*onh*matmul(Msin,Atm%atom(i)%SkI(:,nvk))
                   Ajh(:,i,j)= ar(:) - bi(:)
                   Bjh(:,i,j)= br(:) + ai(:)
                end do ! NList
             end do ! Reflections

          else

             do j=1,Reflex%Nref
                if (.not. Reflex%Mh(j)%keqv_minus ) then
                   onh=0.5
                else
                   onh=1.0
                end if
                nvk= Reflex%Mh(j)%num_k
                isig=Reflex%Mh(j)%signp

                do ii=1,Nlist
                   i=List(ii)
                   m= Atm%Atom(i)%imat(nvk)
                   if(m == 0) cycle
                   GMh=cmplx(0.0,0.0)
                   do k=1,Grp%NumOps
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
                      do n=1,abs(Grp%nbas(m)) !cannot be greater than 12 at present
                         x=real(Grp%icomp(n,m),kind=cp)
                         ci=cmplx(1.0-x,-isig*x)
                         Sk=Sk+Atm%atom(i)%cbas(n,nvk)*ci*cmplx(Real(Grp%basf(:,n,k,m),kind=cp),-isig*aimag(Grp%basf(:,n,k,m)))
                      end do
                      GMh(:)=GMh(:) + onh*anis*Sk(:)*CMPLX(COS(arg),SIN(arg))  !Atomic contribution to geometric Magnetic Structure Factor
                   end do ! symmetry
                   Ajh(:,i,j) = real(GMh)
                   Bjh(:,i,j) = aimag(GMh)
                end do ! Nlist
             end do ! Reflections
          end if
       end if

       !---- Recalculation of MSF ----!
       call Sum_MAB(Reflex,Atm%Natoms,Grp%Centred)

    End Subroutine Modify_MSF

    !!----
    !!---- Module Subroutine Calc_Magnetic_Strf_Tensor(SpG,Atm,Mh)
    !!----    type(SPG_Type),   intent(in)     :: SpG
    !!----    type(Matom_list_type),    intent(in)     :: Atm
    !!----    type(MagH_Type),          intent(in out) :: Mh
    !!----
    !!----    Calculate the Tensorial Magnetic Structure factor of the
    !!----    reflection provided in Mh. Only reasonable settings for symmetry
    !!----    operators are allowed to get correct values in this subroutine.
    !!----    The components are given with respect to the crystallographic
    !!----    unitary direct cell system: {e1,e2,e3} and with respect to the
    !!----    Cartesian frame defined in Cell.
    !!----
    !!---- Created: June - 2014 (JRC)
    !!
    Module Subroutine Calc_Magnetic_Strf_Tensor(SpG,Atm,Mh)
       !---- Arguments ----!
       type(SPG_Type),           intent(in)     :: SpG
       type(Matom_list_type),    intent(in)     :: Atm
       type(MagH_Type),          intent(in out) :: Mh

       !---- Local Variables ----!
       integer                            :: i,j,k
       real(kind=cp)                      :: arg,anis,s,b,ht,mFF,tho
       real(kind=cp),    dimension(3)     :: h,tr
       real(kind=cp),    dimension(6)     :: beta
       real(kind=cp),    dimension(3,3)   :: Mcos,Msin,chi,chit,Rot

       s=Mh%s
       Mh%TMsF=0.0

       do i=1,Atm%natoms
          !---- Isotropic Debye-Waller factor * occupation * p=0.5*re*gamma * Magnetic form-factors mFF
          b=atm%atom(i)%biso
          j=atm%atom(i)%ind(2)  !pointer to the magnetic form factor coefficients
          mFF=mfj(s,Magnetic_Form(j)%SctM)
          tho= pn*atm%atom(i)%occ*mFF*exp(-b*s*s)
          chi=reshape((/atm%atom(i)%chi(1),atm%atom(i)%chi(4), atm%atom(i)%chi(5), &
                        atm%atom(i)%chi(4),atm%atom(i)%chi(2), atm%atom(i)%chi(6), &
                        atm%atom(i)%chi(6),atm%atom(i)%chi(6), atm%atom(i)%chi(3) /),(/3,3/))
          Mcos=0.0 ; Msin=0.0
          do k=1,SpG%Numops
             !h=Hkl_R(Mh%h,SpG%symop(k))
             Rot=SpG%Op(k)%Mat(1:3,1:3)
             tr =SpG%Op(k)%Mat(1:3,4)
             h=matmul(Mh%h,Rot)
             ht=dot_product(Mh%h,Tr)
             arg=tpi*(dot_product(h,Atm%atom(i)%x)+ht)
             anis=1.0
             if (Atm%atom(i)%thtype == "aniso") then
                beta=Atm%atom(i)%u(1:6)
                anis=     h(1)*h(1)*beta(1)+     h(2)*h(2)*beta(2)+    h(3)*h(3)*beta(3) &
                     +2.0*h(1)*h(2)*beta(4)+ 2.0*h(1)*h(3)*beta(5)+2.0*h(2)*h(3)*beta(6)
                anis=exp(-anis)
             end if
             chit=matmul(Rot,chi)
             chit=matmul(chit,transpose(Rot))
             Mcos(:,:)=Mcos(:,:)+cos(arg)*anis*chit
             if(SpG%Centred /= 2) Msin(:,:)=Msin(:,:)+sin(arg)*anis*chit
             !write(*,"(a,10f10.4)") "  arg, Mcos_part: ",arg,cos(arg)*anis*chit
          end do ! symmetry
          Mh%TMsF=Mh%TMsF+tho*cmplx(Mcos,Msin)
       end do ! Atoms
       Mh%TMsF=Mh%TMsF*SpG%Centred*SpG%Num_Lat
    End Subroutine Calc_Magnetic_StrF_Tensor

End SubModule kStrf_MStrT
