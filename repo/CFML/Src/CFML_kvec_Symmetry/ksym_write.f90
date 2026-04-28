SubModule (CFML_kvec_Symmetry) ksym_write
   implicit none
   Contains
    !!----
    !!---- Module Subroutine Write_Magnetic_Structure(Ipr,MGp,Am,Mag_Dom)
    !!----    Integer,                    intent(in)           :: Ipr
    !!----    type(MagSymm_k_Type),       intent(in)           :: MGp
    !!----    type(mAtom_List_Type),      intent(in)           :: Am
    !!----    type(Magnetic_Domain_Type), intent(in), optional :: Mag_Dom
    !!----
    !!----    Subroutine to write out the information about the magnetic symmetry
    !!----    and mangnetic structure in unit Ipr.
    !!----
    !!---- Updated: November 2006, June 2014
    !!
    Module Subroutine Write_Magnetic_Structure(Ipr,MGp,Am,Mag_Dom,cell)
       !---- Arguments ----!
       Integer,                    intent(in)           :: Ipr
       type(MagSymm_k_Type),       intent(in)           :: MGp
       type(mAtom_List_Type),      intent(in)           :: Am
       type(Magnetic_Domain_Type), intent(in), optional :: Mag_Dom
       type(Cell_G_Type),          intent(in), optional :: cell

       !---- Local Variables ----!
       character (len=100), dimension( 4):: texto
       character (len=80)                :: aux
       integer :: i,j,k,l, nlines,n,m,mult,nt
       real(kind=cp)                  :: x
       complex                        :: ci
       real(kind=cp), dimension(3)    :: xp,xo,u_vect,Mom,v
       real(kind=cp), dimension(3,3)  :: chi,chit
       real(kind=cp), dimension(3,48) :: orb
       complex, dimension(3)          :: Sk


       Write(unit=ipr,fmt="(/,a)")  "==================================="
       Write(unit=ipr,fmt="(  a)")  "== Magnetic Symmetry Information =="
       Write(unit=ipr,fmt="(a,/)")  "==================================="

       write(unit=ipr,fmt="(a)")    " => Magnetic  model name: "//trim(MGp%MagModel)
       write(unit=ipr,fmt="(a)")    " => Crystal lattice type: "//MGp%Latt
       if (MGp%nirreps == 0) then
          write(unit=ipr,fmt="(a,i2)") " => Number of Magnetic operators/Crystallographic operator: ",MGp%nmsym
       else
          write(unit=ipr,fmt="(a,i2)") " => Number of Irreducible Representations: ",MGp%nirreps
          do i=1,MGp%nirreps
             write(unit=ipr,fmt="(2(a,i3),a,12i2)") " => Number of basis functions of Irreducible Representation #",i," :", &
                                 MGp%nbas(i),"  Indicators for real(0)/imaginary(1): ", MGp%icomp(1:abs(MGp%nbas(i)),i)
          end do
       end if

       If(Am%Natoms > 0) then
         If (MGp%Centred == 2) then
            write(unit=ipr,fmt="(a)")    " => The crystallographic structure is centric (-1 at origin) "
         else
            write(unit=ipr,fmt="(a)")    " => The crystallographic structure is acentric  "
         End if
         if (MGp%MCentred == 2) then
            write(unit=ipr,fmt="(a)")    " => The magnetic structure is centric "
         else
            if (MGp%Centred == 2) then
               write(unit=ipr,fmt="(a)")    " => The magnetic structure is anti-centric  "
            else
               write(unit=ipr,fmt="(a)")    " => The magnetic structure is acentric  "
            end if
         End if
       End if
       write(unit=ipr,fmt="(a,i2)") " => Number of propagation vectors: ",MGp%nkv
       do i=1,MGp%nkv
          write(unit=ipr,fmt="(a,i2,a,3f8.4,a)") " => Propagation vectors #",i," = (",MGp%kvec(:,i)," )"
       end do
       if (MGp%Num_lat > 1) then
          write(unit=ipr,fmt="(a,i3)")  " => Centring vectors:",MGp%Num_lat-1
          nlines=1
          texto(:) (1:100) = " "
          do i=2,MGp%Num_lat
             aux= Frac_Trans_2Dig(MGp%Ltr(:,i))
             if (mod(i-1,2) == 0) then
                write(unit=texto(nlines)(51:100),fmt="(a,i2,a,a)") " => Latt(",i-1,"): ",trim(aux)
                nlines=nlines+1
             else
                write(unit=texto(nlines)( 1:50),fmt="(a,i2,a,a)") " => Latt(",i-1,"): ",trim(aux)
             end if
          end do
          do i=1,nlines
             write(unit=ipr,fmt="(a)") texto(i)
          end do
       end if

       If(MGp%Numops > 0) then
         write(unit=ipr,fmt="(/,a,/)")        " => List of all Symmetry Operators and Symmetry Symbols"

         do i=1,MGp%Numops
            texto(1)=" "
            texto(1)=Symmetry_Symbol(MGp%SymOp(i)%Rot,MGp%SymOp(i)%tr)
            write(unit=ipr,fmt="(a,i3,2a,t50,2a)") " => SYMM(",i,"): ",trim(MGp%SymopSymb(i)), &
                                                            "Symbol: ",trim(texto(1))
            if (MGp%nirreps == 0) then
              do j=1,MGp%NMSym
                 write(unit=ipr,fmt="(a,2(i2,a))")      "    MSYMM(",i,",",j,"): "//trim(MGp%MSymopSymb(i,j))
              end do
            else
              do j=1,MGp%nirreps
                write(unit=ipr,fmt="(a,2(i2,a),12(3f9.4,tr2))")"    BASR(",i,",",j,"): ",real(MGp%Basf(:,1:abs(MGp%nbas(j)),i,j))
                if (MGp%nbas(j) < 0) &
                write(unit=ipr,fmt="(a,2(i2,a),12(3f9.4,tr2))")"    BASI(",i,",",j,"): ",AIMAG(MGp%Basf(:,1:abs(MGp%nbas(j)),i,j))
              end do
            end if
         end do
       End if  !MGp%Numops > 0

       If(Am%Natoms > 0) then
         Write(unit=ipr,fmt="(/,a)")  "===================================="
         Write(unit=ipr,fmt="(  a)")  "== Magnetic Structure Information =="
         Write(unit=ipr,fmt="(a,/)")  "===================================="

         Write(unit=ipr,fmt="(a)")    " "
         Write(unit=ipr,fmt="(  a)")  "== Magnetic Asymmetric Unit Data =="
         Write(unit=ipr,fmt="(a,/)")  " "

         if (MGp%nirreps == 0) then

            if(Am%suscept) then
               Write(unit=ipr,fmt="(a,f8.3,a)")  &
               "  The magnetic structure is induced by an applied magnetic field of ",Am%MagField," Tesla"
               Write(unit=ipr,fmt="(a,3f8.3,a)")  &
               "  The direction of the applied magnetic field is: [",Am%dir_MField," ] in crystal space"
               do i=1,Am%Natoms
                  Write(unit=ipr,fmt="(a,a,5f10.5)")  &
                    "   Atom "//Am%Atom(i)%Lab, Am%Atom(i)%SfacSymb, Am%Atom(i)%x,Am%Atom(i)%Biso,Am%Atom(i)%occ
                  Write(unit=ipr,fmt="(a,6f10.5,a)")  &
                        "     Chi-Tensor( Chi11,Chi22,Chi33,Chi12,Chi13,Chi23) =  (", Am%Atom(i)%chi(:),")"
               end do

            else

               if(MGp%Fcoef_type == "S") then
                  Write(unit=ipr,fmt="(a)")  &
                  "  The Fourier coefficients are of the form: Sk(j) = 1/2 { Rk(j) + i Ik(j) } exp {-2pi i Mphask(j)}"
                  Write(unit=ipr,fmt="(a)")  &
                  "  They are written for each atom j as Sk(j)= 1/2 {(Rx Ry Rz)+i( Ix Iy Iz)} exp{-2pi i Mphask} -> MagMatrix # imat"
                  Write(unit=ipr,fmt="(a)")  "  In case of k=2H (H reciprocal lattice vector) Sk(j)= [ Rx(j) Ry(j) Rz(j) ]"
                  Write(unit=ipr,fmt="(2a)")  &
                  "  The Fourier Series for calculating the moment of atom j in cell L is written as:", &
                  "      m(j,L) = Sum{k} [ Sk(j) exp{-2pi i k. R(L)} ] "
               else
                  Write(unit=ipr,fmt="(a)")  &
                  "  The Fourier coefficients are of the form: Tk(j) = 1/2 { Mcosk(j) + i Msink(j) } "
                  Write(unit=ipr,fmt="(a)")  &
                  "  They are written for each atom j as Tk(j)= 1/2 {(Mcoskx Mcosky Mcoskz) + i (Msinkx Msinky Msinkz) } -> MagMatrix # imat"
                  Write(unit=ipr,fmt="(a)")  "  In case of 2k=H (H reciprocal lattice vector) Tk(j)= [ Mx(j) My(j) Mz(j) ]"
                  Write(unit=ipr,fmt="(2a)")  &
                  "  The Fourier Series for calculating the moment of atom j in cell L is written as:", &
                  "      m(j,L) = Sum{k} [ Tk(j) exp{-2pi i k. (R(L)+r(j)) } ]      iif 2k /= H"
                  Write(unit=ipr,fmt="(a)")  "  In case of 2k=H the vector r(j) is absent in the exponential"
               end if
               Write(unit=ipr,fmt="(a,/)")  " "
               do i=1,Am%Natoms
                  Write(unit=ipr,fmt="(a,a,5f10.5)")  &
                    "   Atom "//Am%Atom(i)%Lab, Am%Atom(i)%SfacSymb, Am%Atom(i)%x,Am%Atom(i)%Biso,Am%Atom(i)%occ
                  do j=1,MGp%nkv !Am%Atom(i)%nvk
                     m=Am%Atom(i)%imat(j)
                     if( m == 0 ) cycle
                     !if (K_Equiv_Minus_K(MGp%kvec(:,j),MGp%latt)) then
                     if (MGp%k_eqv_minusK(j)) then
                        Write(unit=ipr,fmt="(a,i2,a,3f11.5,a,i4)")  &
                        "     Sk(",j,") =  (", Am%Atom(i)%Skr(:,j),")  -> MagMatrix #", m
                     else
                        Write(unit=ipr,fmt="(a,i2,a,2(3f11.5,a),f9.5,a,i4)")  &
                        "     Sk(",j,") = 1/2 {(", Am%Atom(i)%Skr(:,j),") + i (",Am%Atom(i)%Ski(:,j),")}  exp { -2pi i ",&
                        Am%Atom(i)%MPhas(j),"}  -> MagMatrix #", Am%Atom(i)%imat(j)
                     end if
                  end do
               end do
            end if

         else

            Write(unit=ipr,fmt="(a)")  &
            "  The Fourier coefficients are of the form: Sk(j) = 1/2 Sum(i){Ci* Basf(i,imat)} exp {-2pi i Mphask(j)}"
            Write(unit=ipr,fmt="(a)")  &
            "  Where Ci are coefficients given below, Basf are the basis functions given above -> Irrep# imat"

            do i=1,Am%Natoms
               Write(unit=ipr,fmt="(a,a,5f10.5)")  &
                 "   Atom "//Am%Atom(i)%Lab, Am%Atom(i)%SfacSymb, Am%Atom(i)%x,Am%Atom(i)%Biso,Am%Atom(i)%occ
               do j=1,MGp%nkv  !Am%Atom(i)%nvk
                  m=Am%Atom(i)%imat(j)
                  if( m == 0 ) cycle
                  n=abs(MGp%nbas(m))
                  aux="(a,i2,a,  f11.5,a,f9.5,a,i4)"
                  write(unit=aux(9:10),fmt="(i2)") n
                  Write(unit=ipr,fmt=aux)  &
                     "  Coef_BasF(",j,") = 1/2 {(", Am%Atom(i)%cbas(1:n,j),")}  exp { -2pi i ",&
                     Am%Atom(i)%MPhas(j),"}  -> Irrep #", m
               end do
            end do
         end if

         ! Complete list of all atoms per primitive cell
         Write(unit=ipr,fmt="(/,a)")  " "
         Write(unit=ipr,fmt="(  a)")  "== List of all atoms and Fourier coefficients in the primitive cell =="
         Write(unit=ipr,fmt="(a,/)")  " "

         ! Construct the Fourier coefficients in case of Irreps
         if (MGp%nirreps /= 0 ) then
            do i=1,Am%natoms
               xo=Am%Atom(i)%x
               mult=0
               orb=0.0
               SOps: do k=1,MGp%NumOps
                  xp=ApplySO(MGp%SymOp(k),xo)
                  do nt=1,mult
                    v=orb(:,nt)-xp(:)
                    if (Lattice_trans(v,MGp%latt)) cycle SOps
                  end do
                  mult=mult+1
                  orb(:,mult)=xp(:)
                  Write(unit=ipr,fmt="(a,i2,a,3f9.5)") " =>  Atom "//Am%Atom(i)%lab//"(",k,") :",xp
                  do j=1,MGp%nkv
                     m=Am%Atom(i)%imat(j)
                     if( m == 0 ) cycle
                     n=abs(MGp%nbas(m))
                     Sk(:) = cmplx(0.0,0.0)
                     do l=1,n !cannot be greater than 12 at present
                        x=real(MGp%icomp(l,m))
                        ci=cmplx(1.0-x,x)
                        Sk(:)=Sk(:)+ Am%atom(i)%cbas(l,m)*ci* MGp%basf(:,l,k,m)
                     end do
                     x=-tpi*Am%atom(i)%mphas(j)
                     Sk=Sk*cmplx(cos(x),sin(x))
                     if (MGp%k_eqv_minusK(j) ) then
                        Write(unit=ipr,fmt="(a,i2,a,2(3f11.5,a),f9.5,a)")  &
                        "     Sk(",j,") = (", real(Sk),")  => Moment ..."
                     else
                        Write(unit=ipr,fmt="(a,i2,a,2(3f11.5,a),f9.5,a)")  &
                        "     Sk(",j,") = 1/2 {(", real(Sk),") + i (",aimag(Sk),")}"
                     end if
                  end do
               end do  SOps !Ops
               Write(unit=ipr,fmt="(a)") "  "
            end do  !atoms

         else !MGp%nirreps == 0

            if(Am%suscept .and. present(cell)) then
                u_vect=Am%MagField * Am%dir_MField / Veclength(Cell%Cr_Orth_cel,Am%dir_MField)
                do i=1,Am%natoms
                  xo=Am%Atom(i)%x
                  xo=modulo_lat(xo)
                  chi=reshape([am%atom(i)%chi(1),am%atom(i)%chi(4), am%atom(i)%chi(5), &
                                am%atom(i)%chi(4),am%atom(i)%chi(2), am%atom(i)%chi(6), &
                                am%atom(i)%chi(6),am%atom(i)%chi(6), am%atom(i)%chi(3) ],[3,3])
                  mult=0
                  orb=0.0
                  sym: do k=1,MGp%Numops
                     xp=ApplySO(MGp%SymOp(k),xo)
                     xp=modulo_lat(xp)
                     do nt=1,mult
                       v=orb(:,nt)-xp(:)
                       if (Lattice_trans(v,MGp%latt)) cycle sym
                     end do
                     mult=mult+1
                     orb(:,mult)=xp(:)
                     chit=matmul(MGp%SymOp(k)%Rot,chi)
                     chit=matmul(chit,transpose(MGp%SymOp(k)%Rot))
                     Mom=matmul(Chit,u_vect)

                     Write(unit=ipr,fmt="(a,i2,2(a,3f11.5),a)") " =>  Atom "//Am%Atom(i)%lab//"(",k,") :",xp, &
                                                                "   Induced moment: [",Mom," ]"
                     Write(unit=ipr,fmt="(a)")            "             Local Susceptibility Tensor: "
                     do j=1,3
                        Write(unit=ipr,fmt="(a,3f14.5)")  "                            ",chit(j,:)
                     end do
                  end do sym ! symmetry
                end do ! Atoms

            else !suscept

              do i=1,Am%natoms
                 xo=Am%Atom(i)%x
                 mult=0
                 orb=0.0
                 Ops: do k=1,MGp%NumOps
                    xp=ApplySO(MGp%SymOp(k),xo)
                    do nt=1,mult
                      v=orb(:,nt)-xp(:)
                      if (Lattice_trans(v,MGp%latt)) cycle Ops
                    end do
                    mult=mult+1
                    orb(:,mult)=xp(:)
                    Write(unit=ipr,fmt="(a,i2,a,3f8.5)") " =>  Atom "//Am%Atom(i)%lab//"(",k,") :",xp
                    do j=1,MGp%nkv !Am%Atom(i)%nvk
                       m=Am%Atom(i)%imat(j)
                       if( m == 0 ) cycle
                       !n=abs(MGp%nbas(m))
                       x=-tpi*Am%atom(i)%mphas(j)
                       Sk=cmplx(Am%Atom(i)%Skr(:,j),Am%Atom(i)%Ski(:,j)) * cmplx (cos(x),sin(x))
                       Sk= ApplyMSO(MGp%MSymOp(k,m),Sk)
                       if(MGp%k_eqv_minusk(j)) then
                          Write(unit=ipr,fmt="(a,i2,a,2(3f10.5,a),f9.5,a)")  &
                            "     Sk(",j,") = (", real(Sk),") => Moment ..."
                       else
                          Write(unit=ipr,fmt="(a,i2,a,2(3f10.5,a),f9.5,a)")  &
                            "     Sk(",j,") = 1/2 {(", real(Sk),") + i (",aimag(Sk),")}"
                       end if
                    end do
                 end do Ops
                 Write(unit=ipr,fmt="(a)") "  "
              end do  !atoms
            end if !suscept
         end if

       End If !Am%Natoms > 0

       ! Writing information about domains (like in FullProf)
       if (present(Mag_Dom)) then
          write(unit=ipr,fmt="(a)") " => Magnetic S-Domains are present"
          if(Mag_Dom%chir) write(unit=ipr,fmt="(a)")"    Chirality domains are also present                     Chir-1      Chir-2"
          do i=1,Mag_Dom%nd
             if (Mag_Dom%chir) then
                write(unit=ipr,fmt="(a,i2,1(a,2f12.4))")"      Matrix of Magnetic Domain #:",i, &
                   " -> Populations: ",Mag_Dom%Pop(1:2,i) !,'  Codes:',MagDom(iom)%MPop(1:2,i)
             else
                write(unit=ipr,fmt="(a,i2,1(a,f12.4))")"      Matrix of Magnetic Domain #:",i,  &
                   " -> Population: ",Mag_Dom%Pop(1,i) !,'  Code:',MagDom(iom)%MPop(1,i)
             end if
             do j=1,3
                write(unit=ipr,fmt="(a,3i4)")  "                    ",Mag_Dom%Dmat(j,:,i)
            end do
          end do
       end if

    End Subroutine Write_Magnetic_Structure

End SubModule ksym_write