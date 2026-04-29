SubModule (CFML_kvec_Symmetry) ksym_auxsub
   implicit none
   Contains
    !---------------------!
    !---- Functions   ----!
    !---------------------!
    Module Function get_kvec_info(MGp) result(kinfo)
      type(MagSymm_k_Type), intent(in) :: MGp
      type(Kvect_Info_Type)            :: kinfo
      !---- Local variables ----!
      integer     :: i, j, k, m, nk
      real(kind=cp), dimension(3,MGp%nkv) :: kv, kcon

      !Analysis of k-vectors contained in MGp
      j=0; m = 0
      do i=1,MGp%nkv
        if(Zbelong(2.0_cp*MGp%kvec(:,i))) then
          j=j+1
          kcon(:,j)=MGp%kvec(:,i)
        else
          m=m+1
          kv(:,m)=MGp%kvec(:,i)
        end if
      end do
      nk=m

      Select Case (nk)

        Case(0)
            kinfo%nk=0

        Case(1)
            kinfo%nk=1; kinfo%nq=1
            allocate(kinfo%kv(3,1),kinfo%kv_std(3,1),kinfo%sintlim(1),kinfo%nharm(1),kinfo%q_coeff(1,1))
            kinfo%kv(:,1)=kv(:,1)
            kinfo%kv_std(:,1)=0.0_cp
            kinfo%q_coeff(1,1)=1
            kinfo%sintlim(1)=1.0_cp
            kinfo%nharm(1)=1

        Case(2)
            !Check if both wavevectors are independent or one is integer multiple of the other

            if(Co_linear(kv(:,1),kv(:,2))) then
              k=0; m=0
              do i=1,12
                 if(sum(abs(kv(:,2)-real(i)*kv(:,1))) < 0.0002) then
                   k=i
                   exit
                 end if
              end do

              do i=1,12
                 if(sum(abs(kv(:,1)-real(i)*kv(:,2))) < 0.0002) then
                   m=i
                   exit
                 end if
              end do

              if(k /= 0) then !A single k-vector and one harmonic
                kinfo%nk=1; kinfo%nq=2
                allocate(kinfo%kv(3,1),kinfo%kv_std(3,1),kinfo%sintlim(1),kinfo%nharm(1),kinfo%q_coeff(1,2))
                kinfo%kv(:,1)=kv(:,1)
                kinfo%kv_std(:,1)=0.0_cp
                kinfo%q_coeff(1,1)=1
                kinfo%q_coeff(1,2)=k
                kinfo%sintlim(1)=1.0_cp
                kinfo%nharm(1)=2
              else if(m /= 0) then !A single k-vector and one harmonic
                kinfo%nk=1; kinfo%nq=2
                allocate(kinfo%kv(3,1),kinfo%kv_std(3,1),kinfo%sintlim(1),kinfo%nharm(1),kinfo%q_coeff(1,2))
                kinfo%kv(:,1)=kv(:,2)
                kinfo%kv_std(:,1)=0.0_cp
                kinfo%q_coeff(1,1)=1
                kinfo%q_coeff(1,2)=m
                kinfo%sintlim(1)=1.0_cp
                kinfo%nharm(1)=2
              else !Independent vectors
                kinfo%nk=1; kinfo%nq=2
                allocate(kinfo%kv(3,2),kinfo%kv_std(3,2),kinfo%sintlim(2),kinfo%nharm(2),kinfo%q_coeff(2,2))
                kinfo%kv(:,1)=kv(:,1)
                kinfo%kv(:,2)=kv(:,2)
                kinfo%kv_std(:,1:2)=0.0_cp
                kinfo%q_coeff(1,1)=1
                kinfo%q_coeff(1,2)=0
                kinfo%q_coeff(2,1)=0
                kinfo%q_coeff(2,2)=1
                kinfo%sintlim(1:2)=1.0_cp
                kinfo%nharm(1:2)=1
              end if

            end if

        Case(3)

      End Select
    End Function get_kvec_info

    !---------------------!
    !---- Subroutines ----!
    !---------------------!
    Module Subroutine Get_kv_Orbit_ZeroCell(Am,MGp,orbit,sig,modl)
       !---- Arguments ----!
       type(mAtom_List_Type),        intent(in)  :: Am
       Type(MagSymm_k_Type),         intent(in)  :: MGp
       type(Mod_Orbit),              intent(out) :: orbit
       real(kind=cp), optional,      intent(in)  :: sig
       logical,       optional,      intent(in)  :: modl

       !---- Local variables ----!
       integer                         :: i, j, k, m, nt, mult, ncon, nd, kcon
       real(kind=cp)                   :: sng, ph, kL, kr, pha, onh
       real(kind=cp),    dimension(3,MGp%NumOps*(1+MGp%Num_Lat)*Am%natoms)           :: Orb,morb
       integer,          dimension(3,MGp%NumOps*(1+MGp%Num_Lat)*Am%natoms)           :: Latt
       integer,          dimension(MGp%NumOps*(1+MGp%Num_Lat)*Am%natoms)             :: ptr
       character(len=15),dimension(MGp%NumOps*(1+MGp%Num_Lat)*Am%natoms)             :: Labs
       character(len=2), dimension(MGp%NumOps*(1+MGp%Num_Lat)*Am%natoms)             :: Chem
       complex(kind=cp), dimension(3,0:MGp%nkv,MGp%NumOps*(1+MGp%Num_Lat)*Am%natoms) :: TFour !TFourier !(3,0:nd,Mult)
       integer,          dimension(3)                                                :: Lat
       complex(kind=cp), dimension(3)                                                :: Sk, Tk
       real(kind=cp),    dimension(3)  :: xo,xp,xpp,v

       mult=0; sng=-1.0; onh=1.0; Tfour=0.0
       if(present(sig)) sng=sig
       ncon=0; kcon=0
       do j=1,MGp%nkv
          if(Zbelong(MGp%kvec(:,j))) then
            ncon=ncon+1
            kcon=j
            exit
          end if
       end do
       nd=MGp%nkv-ncon

       do i=1,Am%natoms

         xo=Am%atom(i)%x

         Ops: do k=1,MGp%NumOps
            xp=ApplySO(MGp%SymOp(k),xo)
            xpp=xp; Lat=0
            if(present(modl)) then
               if(modl) call Lat_Modulo(xp,xpp,Lat) !Modify xp => xpp in the interval [0,1) and calculates Lat
            end if                                  !so that xpp = xp - Lat, or Lat = xp - xpp (Lat is the returning vector)
            do nt=1,mult
              v=orb(:,nt)-xpp
              if (Lattice_trans(v,MGp%latt)) cycle Ops
            end do
            mult=mult+1
            orb(:,mult)=xpp(:)
            latt(:,mult)=Lat
            morb(:,mult) = 0.0
            write(unit=Labs(mult),fmt="(a,i2.2)") trim(Am%Atom(i)%Lab)//"_",k
            Chem(mult)=Am%Atom(i)%ChemSymb
            ptr(mult) = k   !Pointer to symmetry operator

            if(Mgp%Fcoef_Type == "S") then

               do j=1,MGp%nkv
                  m=Am%Atom(i)%imat(j)
                  if(m == 0) cycle
                  kL=dot_product(-real(Lat),MGp%kvec(:,j)) !  => kL= k.R_L, Lat is the correction of Sk for changing the position
                  kr=dot_product(xo,MGp%kvec(:,j)) ! k.xo

                  if(j == kcon) then
                    ph=sng*tpi*kL
                    onh=1.0_cp
                    Sk=cmplx(Am%Atom(i)%Skr(:,j),0.0_cp)
                  else
                    !The phase ph is for calculating the moment it does not affect the Fourier coefficients
                    ph=sng*tpi*(kL+Am%Atom(i)%mphas(j)) ! Phase factor exp{-2pi i (k.R_L + phase(n,j) + glb_phase)}
                    onh=0.5_cp
                    Sk=onh*cmplx(Am%Atom(i)%Skr(:,j),Am%Atom(i)%Ski(:,j))*exp(cmplx(0.0_cp,ph))
                  end if
                  Sk= ApplyMSO(MGp%MSymOp(k,m),Sk)
                  morb(:,mult)=morb(:,mult)+ (Sk + conjg(Sk))

                  if(j == kcon) then
                    Tfour(:,0,mult)=cmplx(real(Sk),0.0_cp)
                  else
                    pha=sng*tpi*kr
                    if(j < kcon) then
                       Tfour(:,j,mult)=Sk*exp(cmplx(0.0_cp,-pha))
                    else
                       Tfour(:,j-1,mult)=Sk*exp(cmplx(0.0_cp,-pha))
                    end if
                  end if
               end do

            else if(Mgp%Fcoef_Type == "T") then

               do j=1,MGp%nkv
                  m=Am%Atom(i)%imat(j)
                  if(m == 0) cycle
                  kL=dot_product(-real(Lat),MGp%kvec(:,j))
                  kr=dot_product(xo,MGp%kvec(:,j)) ! k.xo
                  if(j == kcon) then  !This is equivalent to Fcoef_Type="S"
                    ph=sng*tpi*kL
                    Tk=cmplx(Am%Atom(i)%Skr(:,j),0.0_cp)
                    Tk= ApplyMSO(MGp%MSymOp(k,m),Tk)
                    pha=0.0_cp
                    onh=1.0_cp
                  else
                    !The phase ph is for calculating the moment it does not affect the Fourier coefficients
                    onh=0.5_cp
                    ph=sng*tpi*(kL+kr+Am%Atom(i)%mphas(j)) ! Phase factor exp{-2pi i (k.R_L + phase(n,j) + glb_phase)}
                    Tk=onh*cmplx(Am%Atom(i)%Skr(:,j),Am%Atom(i)%Ski(:,j))
                    pha=sng*tpi*kr
                    Sk=Tk*cmplx(cos(pha),sin(pha))
                    Sk= ApplyMSO(MGp%MSymOp(k,m),Sk)    !Transformed Fourier coefficient by operator m
                    Tk=Sk*cmplx(cos(pha),-sin(pha))
                  end if
                  morb(:,mult)=morb(:,mult) +  (real(Tk)*cos(ph) - aimag(Tk)*sin(ph))/onh

                  if(j == kcon) then
                    Tfour(:,0,mult)=cmplx(real(Tk),0.0_cp)
                  else
                    !pha=sng*tpi*kr
                    if(j < kcon) then
                       Tfour(:,j,mult)=Tk
                    else
                       Tfour(:,j-1,mult)=Tk
                    end if
                  end if
                end do
            else
               err_CFML%Ierr = 1
               err_CFML%flag = .true.
               err_CFML%Msg  = "Undefined type of complex Fourier coefficients, they must be 'S' or 'T'"
               return
            end if
         end do  Ops

       end do ! i=1,Am%natoms

       orbit%Mult=mult
       orbit%nd=nd
       orbit%pos=orb(:,1:mult)   !Automatic allocation
       orbit%pts=ptr(1:mult)
       orbit%Labs=Labs(1:mult)
       orbit%Latt=Latt(:,1:mult)
       orbit%Latr=0
       orbit%ChemSymb=Chem(1:mult)
       orbit%mom=morb(:,1:mult)
       if(allocated(orbit%Tfourier)) deallocate(orbit%Tfourier)
       allocate(orbit%Tfourier(3,0:nd,mult))
       orbit%Tfourier=Tfour(:,0:nd,1:mult)

    End Subroutine Get_kv_Orbit_ZeroCell


    Module Subroutine Write_CIF_P1_ssg(fname,Cell,SpG,A,orbit,kinfo)
      character(len=*),     intent(in) :: fname
      class(Cell_G_Type),   intent(in) :: Cell
      class(SPG_type),      intent(in) :: SpG
      type (Atlist_Type),   intent(in) :: A
      type(Mod_Orbit),      intent(in) :: orbit
      type(Kvect_Info_Type),intent(in) :: kinfo
      !--- Local variables ---!
      integer                      :: i, j, k, m, Ipr, nd
      real(kind=cp)                :: xv,xv_std
      real(kind=cp), dimension(3)  :: v
      character(len=256)           :: line
      character(len=1)             :: axis
      type(Orbit_Type)             :: orb,orb3D
      type(Orbit_List)             :: OL
      character(len=15), dimension(orbit%mult) :: mLabs

        !i=index(fname,".",back=.true.)
        !if(i == 0) i=len_trim(fname)+1
        open(newunit=Ipr,file=trim(fname)//"_P1_ssg.mcif", status="replace",action="write")
        write(unit=Ipr,fmt="(a)")    '#\#CIF_2.0'
        write(unit=Ipr,fmt="(a)") "#  --------------------------------------------------"
        write(unit=Ipr,fmt="(a)") "#  Magnetic SSG CIF file generated by CrysFML08 in P1"
        write(unit=Ipr,fmt="(a)") "#  --------------------------------------------------"
        nd=kinfo%nk
        Select Case(nd)
          case(1)
            write(unit=Ipr,fmt="(a)") "# This is a simple mCIF in magnetic SuperSpace group P1(abg)0"
          case(2)
            write(unit=Ipr,fmt="(a)") "# This is a simple mCIF in magnetic SuperSpace group P1(a1,b1,g1)0(a2,b2,g2)0"
          case(3)
            write(unit=Ipr,fmt="(a)") "# This is a simple mCIF in magnetic SuperSpace group P1(a1,b1,g1)0(a2,b2,g2)0(a3,b3,g3)0"
        End Select
        write(unit=Ipr,fmt="(a)") "# MagCIF file name: "//trim(fname)//"_P1_ssg.mcif"
        write(unit=Ipr,fmt="(a)") " "
        write(unit=Ipr,fmt="(a)")
        line=Get_DateTime()
        write(unit=ipr,fmt="(a)") "#============================================================================="
        write(unit=ipr,fmt="(a)") "data_global"
        write(unit=ipr,fmt="(a)") "#============================================================================="
        write(unit=ipr,fmt="(a)") " "
        write(unit=ipr,fmt="(a)") "_audit_creation_date "//' "'//trim(line)//' "'
        write(unit=ipr,fmt="(a)") "_audit_creation_method  'FST_to_mCIF' "
        write(unit=Ipr,fmt="(a)") " "
        write(unit=Ipr,fmt="(a,i1)") "_cell_modulation_dimension  ",nd
        write(unit=Ipr,fmt="(a)") " "
        Select Case(nd)
          Case(1)
              write(unit=Ipr,fmt="(a)")    '_space_group_magn.ssg_number       "  1.1.1.1 "'
              write(unit=Ipr,fmt="(a)")    '_space_group_magn.ssg_name         "  P1(abg)0 " '
          Case(2)
              write(unit=Ipr,fmt="(a)")    '_space_group_magn.ssg_number       "  1.2.1.1 "'
              write(unit=Ipr,fmt="(a)")    '_space_group_magn.ssg_name         "  P1(a1,b1,g1)0(a2,b2,g2)0 " '
          Case(3)
              write(unit=Ipr,fmt="(a)")    '_space_group_magn.ssg_number       "  1.3.1.1 "'
              write(unit=Ipr,fmt="(a)")    '_space_group_magn.ssg_name         "  P1(a1,b1,g1)0(a2,b2,g2)0(a3,b3,g3)0 " '
        End Select

        write(unit=Ipr,fmt="(a)")    '_space_group_magn.point_group_name "    1 "'
        write(unit=Ipr,fmt="(a)")    "_parent_space_group.IT_number           1 "
        write(unit=Ipr,fmt="(a)")
        write(unit=Ipr,fmt="(a)")  " "
        write(unit=Ipr,fmt="(a)")  " "
        write(unit=Ipr,fmt="(a)")  "loop_"
        write(unit=Ipr,fmt="(a)")  "    _space_group_symop_magn_ssg_operation.id"
        write(unit=Ipr,fmt="(a)")  "    _space_group_symop_magn_ssg_operation.algebraic"
        Select Case(nd)
          Case(1)
             write(unit=Ipr,fmt="(i4,a)") 1,"   x1,x2,x3,x4,+1"
          Case(2)
             write(unit=Ipr,fmt="(i4,a)") 1,"   x1,x2,x3,x4,x5,+1"
          Case(3)
             write(unit=Ipr,fmt="(i4,a)") 1,"   x1,x2,x3,x4,x5,x6,+1"
        End Select


        write(unit=Ipr,fmt="(a)")
        write(unit=Ipr,fmt="(a)")  "loop_"
        write(unit=Ipr,fmt="(a)")  "    _space_group_symop_magn_ssg_centering.id"
        write(unit=Ipr,fmt="(a)")  "    _space_group_symop_magn_ssg_centering.algebraic"
        Select Case(nd)
          Case(1)
             write(unit=Ipr,fmt="(i4,a)") 1,"   x1,x2,x3,x4,+1"
          Case(2)
             write(unit=Ipr,fmt="(i4,a)") 1,"   x1,x2,x3,x4,x5,+1"
          Case(3)
             write(unit=Ipr,fmt="(i4,a)") 1,"   x1,x2,x3,x4,x5,x6,+1"
        End Select

        write(unit=Ipr,fmt="(a)")
        write(unit=Ipr,fmt="(a)")
        write(unit=Ipr,fmt="(a)")
        write(unit=Ipr,fmt="(a,f10.5)") "_cell_length_a    ",cell%cell(1)
        write(unit=Ipr,fmt="(a,f10.5)") "_cell_length_b    ",cell%cell(2)
        write(unit=Ipr,fmt="(a,f10.5)") "_cell_length_c    ",cell%cell(3)
        write(unit=Ipr,fmt="(a,f10.5)") "_cell_angle_alpha ",cell%ang(1)
        write(unit=Ipr,fmt="(a,f10.5)") "_cell_angle_beta  ",cell%ang(2)
        write(unit=Ipr,fmt="(a,f10.5)") "_cell_angle_gamma ",cell%ang(3)
        write(unit=Ipr,fmt="(a,f10.5)")
        write(unit=Ipr,fmt="(a)")
        write(unit=Ipr,fmt="(a)")
        write(unit=Ipr,fmt="(a)") "loop_"
        write(unit=Ipr,fmt="(a)") "    _atom_site_label"
        write(unit=Ipr,fmt="(a)") "    _atom_site_type_symbol"
        write(unit=Ipr,fmt="(a)") "    _atom_site_fract_x"
        write(unit=Ipr,fmt="(a)") "    _atom_site_fract_y"
        write(unit=Ipr,fmt="(a)") "    _atom_site_fract_z"

        !Generate all the atoms (magnetic and non-magnetic) in the zero unit cell
        mLabs=" "
        do i=1,A%natoms
           call Get_Orbit(A%Atom(i)%x,Spg,orb,A%Atom(i)%Lab, &
                       A%Atom(i)%ChemSymb,A%Atom(i)%moment,orb3D)
           do j=1,orb3D%Mult
             write(Ipr,"(a15,tr5,a,10f12.5)") orb3D%Lab(j),orb3D%ChemSymb,orb3D%pos(:,j)
           end do
           !Compare the orbits to attribute the proper label to magnetic atoms
           do j=1,Orbit%Mult
              do k=1,orb3D%Mult
                v=Orbit%pos(:,j)-orb3D%pos(:,k)
                if(Zbelong(v) .and. index(u_case(Orbit%Labs(j)),u_case(orb3D%ChemSymb)) /= 0 ) then
                  mLabs(j)=orb3D%lab(k)
                  exit
                end if
              end do
           end do

        end do

        write(unit=Ipr,fmt="(a)")
        write(unit=Ipr,fmt="(a)")
        write(unit=Ipr,fmt="(a)") "loop_"
        write(unit=Ipr,fmt="(a)") "    _cell_wave_vector_seq_id"
        write(unit=Ipr,fmt="(a)") "    _cell_wave_vector_x"
        write(unit=Ipr,fmt="(a)") "    _cell_wave_vector_y"
        write(unit=Ipr,fmt="(a)") "    _cell_wave_vector_z"
        do i=1,kinfo%nk
           line=' '
           do j=1,3
              line=trim(line)//"    "//string_Numstd(Kinfo%Kv(j,i),Kinfo%Kv_std(j,i))
              line=adjustl(line)
           end do
           write(unit=Ipr,fmt='(2x,i4,3x, a)') i, trim(line)
        end do
        write(unit=Ipr,fmt="(a)")
        write(unit=Ipr,fmt="(a)")
        write(unit=Ipr,fmt="(a)") "loop_"
        write(unit=Ipr,fmt="(a)") "    _atom_site_Fourier_wave_vector.seq_id"
        write(unit=Ipr,fmt="(a)") "    _atom_site_Fourier_wave_vector.x"
        write(unit=Ipr,fmt="(a)") "    _atom_site_Fourier_wave_vector.y"
        write(unit=Ipr,fmt="(a)") "    _atom_site_Fourier_wave_vector.z"
        select case (kinfo%nk)
           case (1)
              write(unit=Ipr,fmt="(a)") "    _atom_site_Fourier_wave_vector.q1_coeff"
           case (2)
              write(unit=Ipr,fmt="(a)") "    _atom_site_Fourier_wave_vector.q1_coeff"
              write(unit=Ipr,fmt="(a)") "    _atom_site_Fourier_wave_vector.q2_coeff"
           case (3)
              write(unit=Ipr,fmt="(a)") "    _atom_site_Fourier_wave_vector.q1_coeff"
              write(unit=Ipr,fmt="(a)") "    _atom_site_Fourier_wave_vector.q2_coeff"
              write(unit=Ipr,fmt="(a)") "    _atom_site_Fourier_wave_vector.q3_coeff"
        end select
        do i=1,kinfo%nq
           v=0.0_cp
           do j=1,kinfo%nk
              v=v + kinfo%q_coeff(j,i)*kinfo%kv(:,j)
           end do
           line=' '
           write(unit=line, fmt='(3i4)') kinfo%q_coeff(:,i)
           write(unit=Ipr,fmt='(2x,i4, 3f12.6,3x,a)') i, v, trim(line)
        end do
        write(unit=ipr,fmt="(a)") " "

        write(unit=Ipr,fmt="(a)") "loop_"
        write(unit=Ipr,fmt="(a)") "    _atom_site_moment.label"
        write(unit=Ipr,fmt="(a)") "    _atom_site_moment.crystalaxis_x"
        write(unit=Ipr,fmt="(a)") "    _atom_site_moment.crystalaxis_y"
        write(unit=Ipr,fmt="(a)") "    _atom_site_moment.crystalaxis_z"
        do i=1,orbit%Mult
           !write(Ipr,"(a15,tr5,10f12.5)") orbit%Labs(i),real(orbit%Tfourier(:,0,i))
           write(Ipr,"(a,tr5,10f12.5)") mLabs(i),real(orbit%Tfourier(:,0,i))
        end do
        write(unit=Ipr,fmt="(a)")
        write(unit=Ipr,fmt="(a)")
        write(unit=Ipr,fmt="(a)") "loop_"
        write(unit=Ipr,fmt="(a)") "    _atom_site_moment_Fourier.id"
        write(unit=Ipr,fmt="(a)") "    _atom_site_moment_Fourier.atom_site_label"
        write(unit=Ipr,fmt="(a)") "    _atom_site_moment_Fourier.wave_vector_seq_id"
        write(unit=Ipr,fmt="(a)") "    _atom_site_moment_Fourier.axis"
        write(unit=Ipr,fmt="(a)") "    _atom_site_moment_Fourier_param.cos"
        write(unit=Ipr,fmt="(a)") "    _atom_site_moment_Fourier_param.sin"
        j=0
        xv_std=0.0_cp
        do i=1,orbit%mult
           do m=1,orbit%nd
              do k=1,3
                 line=" "
                 xv=real(orbit%Tfourier(k,m,i))
                 line=string_NumStd(xv,xv_std)
                 xv=aimag(orbit%Tfourier(k,m,i))
                 line=trim(line)//"   "//string_NumStd(xv,xv_std)
                 select case (k)
                    case (1); axis="x"
                    case (2); axis="y"
                    case (3); axis="z"
                 end select
                 j=j+1
                 !write(unit=Ipr,fmt="(2x,i4,2x,a,t16,i4,t22,a,t30,a)") j, trim(orbit%Labs(i)), m, axis, trim(line)
                 write(unit=Ipr,fmt="(2x,i4,2x,a,t16,i4,t22,a,t30,a)") j, trim(mLabs(i)), m, axis, trim(line)
              end do
           end do
        end do
        close(unit=Ipr)
    End Subroutine Write_CIF_P1_ssg

    !!---- Module Subroutine Calc_Magnetic_Moment_kvec(Am,MGp,atm_lab,num_op,R_latt,vp,moment,ssig,glb_phase,modl)
    !!----    !---- Arguments ----!
    !!----    type(mAtom_List_Type),      intent(in) :: Am      !List of magnetic atoms
    !!----    type(MagSymm_k_Type),       intent(in) :: MGp     !Symmetry of the magntic structure in terms of k-vectors and symm/msymm operators
    !!----    character(len=*),           intent(in) :: atm_lab !Label of the atom in the magnetic asymmetric unit
    !!----    Integer,                    intent(in) :: num_op  !Number of the operator to be used
    !!----    real(kind=cp), dimension(3),intent(in) :: R_latt  !Lattice vector (it is real for centred cells)
    !!----    real(kind=cp), dimension(3),intent(out):: vp      !Vector position in fractional coordinates of the atom
    !!----    real(kind=cp), dimension(3),intent(out):: moment  !Magnetic moment referred to the unitary crystal basis
    !!----    real(kind=cp), optional,    intent(in) :: sig     !sign of the exponential exp{ sig *2pi i R_Latt . kvec}
    !!----                                                      !(if not provided, the used sign is negative sng=-1)
    !!----    real(kind=cp), optional,    intent(in) :: glb_phase
    !!----    logical,       optional,    intent(in) :: modl    !Applies Modulo_Lat if modl=.true.
    !!----
    !!----
    !!----   This subroutine calculates the position and the magnetic moment of an atom in whatever place in the crystal.
    !!----   The kvec-symmetry and the list of magnetic atoms are supposed to be known. The calculation needs a lattice
    !!----   vector, the number of a symmetry operator of the list and the label of a magnetic atom. The program calculates
    !!----   the position (vp) of the equivalent atom at lattice point R_latt, after applying the symmetry operator num_op,
    !!----   and its corresponding magnetic moment (moment).
    !!----
    !!----   April-2025 (JRC)
    !!----
    Module Subroutine Calc_Magnetic_Moment_kvec(Am,MGp,atm_lab,num_op,R_latt,vp,moment,sig,glb_phase,modl)
       !---- Arguments ----!
       type(mAtom_List_Type),      intent(in) :: Am
       type(MagSymm_k_Type),       intent(in) :: MGp
       character(len=*),           intent(in) :: atm_lab
       Integer,                    intent(in) :: num_op
       real(kind=cp), dimension(3),intent(in) :: R_latt
       real(kind=cp), dimension(3),intent(out):: vp
       real(kind=cp), dimension(3),intent(out):: moment
       real(kind=cp), optional,    intent(in) :: sig
       real(kind=cp), optional,    intent(in) :: glb_phase
       logical,       optional,    intent(in) :: modl
       !
       integer        :: i,j,n,m
       logical        :: found
       real(kind=cp)  :: ph, sng, gph, kL, kr, pha, onh
       real(kind=cp), dimension(3) :: xo,xp,xpp
       integer,       dimension(3) :: Lat
       complex,       dimension(3) :: Sk, Tk

       moment=0.0; found=.false.; vp=0.0; sng=-1.0; gph=0.0; onh=1.0_cp; n=0
       if(present(sig)) sng=sig
       if(present(glb_phase)) gph=glb_phase

       !Identify the atom and the symmetry operator to generate the coordinates within the asymmetric unit

       do i=1,Am%natoms
         if(trim(atm_lab) == trim(Am%Atom(i)%Lab)) then
           found=.true.
           n=i
           exit
         end if
       end do
       if(found) then
         xo=Am%Atom(n)%x                          !Coordinates in the asymmetric unit of the selected atom
         xp=ApplySO(MGp%SymOp(num_op),xo)
         Lat=0; xpp=xp
         if(present(modl)) then
            if(modl) call Lat_Modulo(xp,xpp,Lat) !Modify xp => xpp in the interval [0,1) and calculates Lat
         end if                                  !so that xpp = xp - Lat, or Lat = xp - xpp (Lat is the returning vector)


         vp=xpp+R_latt  !Final position of the equivalent atom in the crystal
         !Calculation of the magnetic moment taking into account all propagation vectors
         if(MGp%Fcoef_type == "S") then
            do j=1,MGp%nkv
               m=Am%Atom(n)%imat(j)
               if(m == 0) cycle
               kL=dot_product(R_latt-real(Lat),MGp%kvec(:,j)) !  => kL= k.R_L
               if(Zbelong(2*MGp%kvec(:,j))) then
                 ph=sng*tpi*kL
                 Sk=cmplx(Am%Atom(n)%Skr(:,j),[0.0_cp,0.0_cp,0.0_cp])
                 onh=1.0_cp
               else
                 ph=sng*tpi*(kL+Am%atom(n)%mphas(j)+gph) ! Phase factor exp{-2pi i (k.R_L + phase(n,j) + glb_phase)}
                 onh=0.5_cp
                 Sk=onh*cmplx(Am%Atom(n)%Skr(:,j),Am%Atom(n)%Ski(:,j))
               end if
               Sk= ApplyMSO(MGp%MSymOp(num_op,m),Sk)    !Transformed Fourier coefficient by operator num_op
               moment=moment + (real(Sk)*cos(ph) - aimag(Sk)*sin(ph))/onh
            end do
         else if(MGp%Fcoef_type == "T") then
            do j=1,MGp%nkv
               m=Am%Atom(n)%imat(j)
               if(m == 0) cycle
               kr=dot_product(xo,MGp%kvec(:,j)) ! k.xo
               pha=sng*tpi*kr
               kL=dot_product(vp-real(Lat),MGp%kvec(:,j)) ! vp= {R|t}xo+R_L => kL= k.({R|t}xo+R_L), Lat: returning vector
               if(Zbelong(2*MGp%kvec(:,j))) then
                 ph=sng*tpi*kL
                 onh=1.0_cp
               else
                 ph=sng*tpi*(kL+Am%atom(n)%mphas(j)+gph) ! Phase factor exp{-2pi i (k.Rl + phase(n,j) + glb_phase)}
                 onh=0.5_cp
               end if
               Tk=onh*cmplx(Am%Atom(n)%Skr(:,j),Am%Atom(n)%Ski(:,j))
               Sk=Tk*cmplx(cos(pha),sin(pha))
               Sk= ApplyMSO(MGp%MSymOp(num_op,m),Sk)    !Transformed Fourier coefficient by operator num_op
               Tk=Sk*cmplx(cos(pha),-sin(pha))
               moment=moment +  (real(Tk)*cos(ph) - aimag(Tk)*sin(ph))/onh
            end do
         else
            err_CFML%Ierr = 1
            err_CFML%flag = .true.
            err_CFML%Msg  = "Undefined type of complex Fourier coefficients, they must be 'S' or 'T'"
            return
         end if
       Else
          write(*,"(a)") "  ATOM: "//trim(atm_lab)//"  not found!"
       end if
    End Subroutine Calc_Magnetic_Moment_kvec

    !!----
    !!---- Module Subroutine Latsym(Symb,Numl,Latc)
    !!----    character (len=*),                       intent(in)  :: SYMB  !  In -> Space Group H-M/Hall symbol
    !!----    integer, optional,                       intent(in)  :: numL  !  Number of centring vectors
    !!----    real(kind=cp),optional, dimension(:,:),  intent(in)  :: latc  !  Centering vectors
    !!----
    !!--<<        Inlat  Lattice type & associated translations
    !!----          1     P: { 000 }
    !!----          2     A: { 000;  0  1/2 1/2 }+
    !!----          3     B: { 000; 1/2  0  1/2 }+
    !!----          4     C: { 000; 1/2 1/2  0  }+
    !!----          5     I: { 000; 1/2 1/2 1/2 }+
    !!----          6     R: { 000; 2/3 1/3 1/3; 1/3 2/3 2/3   } +
    !!----          7     F: { 000;  0  1/2 1/2; 1/2  0  1/2; 1/2 1/2  0 } +
    !!----          8     Z: { 000;  user-given centring vectors } +
    !!-->>
    !!----    Provides the Lattice type of the S.G. SYMB. Also gives the index (Inlat)
    !!----    of the lattice, the multiplicity (Nlat) and the fractionnal lattice translations
    !!----    ((Ltr(in,j)j=1,3),in=1,Nlat).
    !!----
    !!---- Update: February - 2005, January 2014 (JRC)
    !!
    Module Subroutine LatSym(SYMB,numL,latc)
       !---- Argument ----!
       character(len=*),                        intent(in)  :: SYMB
       integer, optional,                       intent(in)  :: numL
       real(kind=cp),optional, dimension(:,:),  intent(in)  :: latc  !general vector (JRC, Jan2014)

       !---- Local variables ----!
       character(len=1)                        :: LAT
       character(len=:), allocatable           :: SYMBB
       integer                                 :: i

       call Clear_Error()
       symbb=adjustl(symb)
       do i=1,len_trim(symbb)
          if (symbb(i:i) == "-" .or. symbb(i:i) == " ") cycle
          lat=symbb(i:i)
          exit
       end do

       nlat=1
       ltr(:,1)=0.0
       select case (lat)
          case ("P","p")
             lat="P"
             nlat=1
             inlat=1

          case ("A","a")
             lat="A"
             nlat=2
             inlat=2
             ltr(1,2)=0.0
             ltr(2,2)=0.5
             ltr(3,2)=0.5

          case ("B","b")
             lat="B"
             nlat=2
             inlat=3
             ltr(1,2)=0.5
             ltr(2,2)=0.0
             ltr(3,2)=0.5

          case ("C","c")
             lat="C"
             nlat=2
             inlat=4
             ltr(1,2)=0.5
             ltr(2,2)=0.5
             ltr(3,2)=0.0

          case ("I","i")
             lat="I"
             nlat=2
             inlat=5
             ltr(:,2)=0.5

          case ("R","r")
             lat="R"
             nlat=3
             inlat=6
             ltr(1,2)=2.0/3.0
             ltr(2,2)=1.0/3.0
             ltr(3,2)=1.0/3.0
             ltr(1,3)=1.0/3.0
             ltr(2,3)=2.0/3.0
             ltr(3,3)=2.0/3.0

          case ("F","f")
             lat="F"
             nlat=4
             inlat=7
             ltr(1,2)=0.5
             ltr(2,2)=0.5
             ltr(3,2)=0.0
             ltr(1,3)=0.5
             ltr(2,3)=0.0
             ltr(3,3)=0.5
             ltr(1,4)=0.0
             ltr(2,4)=0.5
             ltr(3,4)=0.5

          case ("Z","z","X","x")
             if(present(numL) .and. present(latc)) then
              lat="Z"
              nlat=numL+1
              !nlat=min(nlat,12) !restriction removed in January 2014
              inlat=8
              do i=2,nlat
                ltr(:,i)=latc(:,i-1)
              end do
             else
               Err_CFML%flag=.true.
               Err_CFML%Msg="Unconventional Lattice Symbol Z needs centring vectors"
             end if
          case default
             Err_CFML%flag=.true.
             Err_CFML%Msg="Wrong Lattice Symbol "//LAT
       end select
    End Subroutine Latsym

    !!----
    !!---- Module Subroutine Read_Xsym(Info,Istart,Sim,Tt,ctrl)
    !!----    character (len=*),                     intent( in)    :: Info   !  In -> String with the symmetry symbol
    !!----                                                                             in the form: SYMM  x,-y+1/2,z
    !!----    integer,                               intent(in)     :: istart !  In -> Starting index of info to read in.
    !!----    integer, dimension(3,3),               intent(out)    :: sim    ! Out -> Rotational part of S.O.
    !!----    real(kind=cp), optional, dimension(3), intent(out)    :: tt     ! Out -> Traslational part of S.O.
    !!----
    !!----
    !!----    Read symmetry or transformation operators in the form X,Y,Z, etc...
    !!----    Provides the rotational matrix and translation associated a to SYMM symbol
    !!----    in the Jones Faithful representation.
    !!----
    !!---- Update: June - 2011 (JRC, adding ctrl for controlling if a real symmetry operator is needed)
    !!
    Module Subroutine Read_Xsym(Info,Istart,Sim,Tt,ctrl)
       !---- Arguments ----!
       character (len=*),                     intent(in)     :: Info
       integer,                               intent(in)     :: istart
       integer, dimension(3,3),               intent(out)    :: sim
       real(kind=cp), optional, dimension(3), intent(out)    :: tt
       logical,       optional,               intent(in)     :: ctrl

       !---- Local variables ----!
       character (len=*), dimension(10), parameter :: ANUM=["1","2","3","4","5","6","7","8","9","0"]
       integer, dimension(10), parameter           :: NUM =[1,2,3,4,5,6,7,8,9,0]
       integer :: i,imax,nop,s,np,isl,ifound,ip,k,mod_istart,ST=0,I_P,ist
       real(kind=cp) :: t,a
       logical       :: control

       control=.true.
       if(present(ctrl)) control=ctrl
       call Clear_Error()
       imax=len_trim(info)
       if (present(tt)) tt=0.0
       sim = 0
       ist=istart
       do nop=1,3
          s=1
          t=0.0
          ip=0
          i_p=1
          np=0
          isl=0
          ifound=0
          mod_istart=0
          loop_string: do i=ist,imax
             if (info(i:i) == " ") cycle
             if (info(i:i) == "," .or. info(i:i) == "*") then
                mod_istart=1
                exit
             end if
             ifound=1
             if (info(i:i) == "X" .or. info(i:i) == "x") then
                sim(nop,1)=s*i_p
                i_p=1
                s=1
             else if (info(i:i) == "Y" .or. info(i:i) == "y") then
                sim(nop,2)=s*i_p
                i_p=1
                s=1
             else if(info(i:i) == "Z" .or. info(i:i) == "z") then
                sim(nop,3)=s*i_p
                i_p=1
                s=1
             else if(info(i:i) == "+") then
                s=1
             else if(info(i:i) == "-") then
                s=-1
             else if(info(i:i) == "/") then
                isl=1
             else if(info(i:i) == ".") then
                ip=1
             else
                st=s
                do k=1,10
                   if (info(i:i) == anum(k))  then
                      if (is_xyz(info(i+1:i+1))) then
                         i_p=num(k)
                         cycle loop_string
                      else
                         a=num(k)
                         if (isl == 1) then
                            t=t/a
                         else if(ip == 1) then
                            np=np+1
                            t=t+a/10**np
                         else
                            t=10.0*t+a
                         end if
                         cycle loop_string
                      end if
                   end if
                end do
                Err_CFML%flag=.true.
                Err_CFML%Msg=" Invalid character... "//INFO(I:I)//" in operator string"
                return
             end if
          end do  loop_string   !end loop through the string (index:i= ist,imax)

          if (mod_istart == 1) then
             ist=i+1
          end if

          t=t*st
          if (present(tt)) tt(nop)=t

          if (ifound == 0) then
             Err_CFML%flag=.true.
             Err_CFML%Msg=" Blank operator field"
             return
          end if

       end do    !End external loop over the three expected items (index:NOP)

       if (determ3D(sim) == 0 .and. control) then      !Verify it is a suitable s.o.
          Err_CFML%flag=.true.
          Err_CFML%Msg=" The above operator is wrong: "//info
          return
       end if

       if (ifound == 1) return

       Err_CFML%flag=.true.
       Err_CFML%Msg=" The above operator is wrong: "//info

    End Subroutine Read_Xsym

    !!----
    !!---- Module Subroutine Read_Msymm(Info,Sim,P_Mag,ctrl)
    !!----    character (len=*),       intent( in) :: Info   !  In -> Input string with S.Op.
    !!----                                                            in the form: MSYM  u,w,w,p_mag
    !!----    integer, dimension(3,3), intent(out) :: sim    ! Out -> Rotation matrix
    !!----    real(kind=cp),           intent(out) :: p_mag  ! Out -> magnetic phase
    !!----    logical, optional,       intent(in)  :: ctrl   ! in  -> If provided and .true. an error condition
    !!----                                                            is raised if the det(Sim)=0
    !!----    Read magnetic symmetry operators in the form U,V,W, etc...
    !!----    Provides the magnetic rotational matrix and phase associated to a MSYM symbol
    !!----
    !!---- Update: February - 2005
    !!
    Module Subroutine Read_Msymm(Info,Sim,P_Mag,ctrl)
       !---- Arguments ----!
       character (len=*),       intent( in) :: Info
       integer, dimension(3,3), intent(out) :: sim
       real(kind=cp),           intent(out) :: p_mag
       logical, optional,       intent(in)  :: ctrl

       !---- Local variables ----!
       integer ::  i,imax,nop,s,ifound,j,ioerr,istart,mod_istart
       character(len=:), allocatable :: aux
       logical :: control

       control=.false.
       if(present(ctrl)) control=ctrl
       call Clear_Error()
       do j=len(Info),1,-1
          if (info(j:j) == ",") exit
       end do
       p_mag=0.0
       imax=j-1
       read(unit=info(j+1:),fmt=*,iostat=ioerr) p_mag
       if (ioerr /= 0) then
          p_mag=0.0
       end if
       sim = 0
       aux=adjustl(l_case(Info))
       if(aux(1:4) == "msym" .or. aux(1:4) == "dsym") then
         istart=6
       else
         istart=1
       end if

       do nop=1,3
          s=1
          mod_istart=0
          ifound=0
          do i=istart,imax
             if (aux(i:i) == " ") cycle
             if (aux(i:i) == "," .or. info(i:i) == "*") then
                mod_istart=1
                exit
             end if
             ifound=1
             if (aux(i:i) == "u" ) then
                sim(nop,1)=s
                s=1
             else if (aux(i:i) == "v") then
                sim(nop,2)=s
                s=1
             else if(aux(i:i) == "w") then
                sim(nop,3)=s
                s=1
             else if(aux(i:i) == "+") then
                s=1
             else if(aux(i:i) == "-") then
                s=-1
             else
                Err_CFML%flag=.true.
                Err_CFML%Msg=" Invalid character... "//aux(I:I)//" in Sym. Op."
                return
             end if
          end do    !End loop through the string

          if (mod_istart == 1) then
            istart=i+1
          end if

          if (ifound == 0) then
             Err_CFML%flag=.true.
             Err_CFML%Msg=" Blank operator field "//info
             return
          end if
       end do    !End external loop over the three expected items

       if (determ3D(sim) == 0 .and. control) then      !Verify it is a suitable s.o.
          Err_CFML%flag=.true.
          Err_CFML%Msg=" The above operator is wrong "//info
          return
       end if

       if (ifound == 1) return

       Err_CFML%flag=.true.
       Err_CFML%Msg=" The above operator is wrong "//info

    End Subroutine Read_Msymm

    !!---- Module Subroutine Get_kv_Orbit(At,MGp,R_Latt,orbit,sig,modl)
    !!----   !---- Arguments ----!
    !!----   Type(mAtom_Type),             intent(in)  :: At     !Magnetic atom type
    !!----   Type(MagSymm_k_Type),         intent(in)  :: MGp    !Symmetry description of the magnetic structure
    !!----   real(kind=cp), dimension(3),  intent(in)  :: R_latt !Lattice point for calculating the orbit of a primitive cell
    !!----   type(Point_Orbit_kv),         intent(out) :: orbit  !Point_orbit_kv type
    !!----   real(kind=cp), optional,      intent(in)  :: sig    !sign of the exponential exp{ sig *2pi i R_Latt . kvec}
    !!----                                                       !(if not provided, the used sign is negative sng=-1)
    !!----   logical,       optional,      intent(in)  :: modl   !If present and .true. applies Lat_Modulo
    !!----
    !!----
    !!----   This procedure construct the orbit of an atom around the lattice point R_Latt
    !!----   by calculating the positions and the corresponding magnetic moment. This works
    !!----   only when the magnetic structure is described in terms of Fourier coefficients
    !!----   of magnetic moments.
    !!----
    !!----   April-2025 (JRC)
    !!----
    Module Subroutine Get_kv_Orbit(At,MGp,R_Latt,orbit,sig,modl)
       !---- Arguments ----!
       Type(mAtom_Type),             intent(in)  :: At
       Type(MagSymm_k_Type),         intent(in)  :: MGp
       real(kind=cp), dimension(3),  intent(in)  :: R_latt
       type(Point_Orbit_kv),         intent(out) :: orbit
       real(kind=cp), optional,      intent(in)  :: sig
       logical,       optional,      intent(in)  :: modl

       !---- Local variables ----!
       integer                                                   :: j, k, m, nt,mult
       real(kind=cp)                                             :: sng, ph, kL, kr, pha, onh
       real(kind=cp),    dimension(3)                            :: xo,xp,xpp,v
       real(kind=cp),    dimension(3,MGp%NumOps*(1+MGp%Num_Lat)) :: Orb,morb
       integer,          dimension(3,MGp%NumOps*(1+MGp%Num_Lat)) :: Latt
       integer,          dimension(3)                            :: Lat
       integer,          dimension(MGp%NumOps*(1+MGp%Num_Lat))   :: ptr
       character(len=15),dimension(MGp%NumOps*(1+MGp%Num_Lat))   :: Labs
       character(len=2), dimension(MGp%NumOps*(1+MGp%Num_Lat))   :: Chem
       complex,          dimension(3)                            :: Sk, Tk

       mult=0; sng=-1.0; onh=1.0; morb=0.0; latt=0; orb=0.0; chem=" "; Labs=" "
       if(present(sig)) sng=sig
       xo=At%x

       Ops: do k=1,MGp%NumOps
          xp=ApplySO(MGp%SymOp(k),xo)
          xpp=xp; Lat=0
          if(present(modl)) then
             if(modl) call Lat_Modulo(xp,xpp,Lat) !Modify xp => xpp in the interval [0,1) and calculates Lat
          end if                                  !so that xpp = xp - Lat, or Lat = xp - xpp (Lat is the returning vector)
          xpp=xpp+R_latt
          do nt=1,mult
            v=orb(:,nt)-xpp
            if (Lattice_trans(v,MGp%latt)) cycle Ops
          end do
          mult=mult+1
          orb(:,mult)=xpp(:)
          latt(:,mult)=Lat
          morb(:,mult) = 0.0
          chem(mult)=At%ChemSymb
          write(unit=Labs(mult),fmt="(a,i2.2)") trim(At%Lab)//"_",k
          ptr(mult) = k   !Pointer to symmetry operator

          if(Mgp%Fcoef_Type == "S") then

             do j=1,MGp%nkv
                m=At%imat(j)
                if(m == 0) cycle
                kL=dot_product(R_latt-real(Lat),MGp%kvec(:,j)) !  => kL= k.R_L, Lat is the correction of Sk for changing the position
                if(Zbelong(2*MGp%kvec(:,j))) then
                  ph=sng*tpi*kL
                  Sk=cmplx(At%Skr(:,j),0.0_cp)
                  onh=1.0_cp
                else
                  ph=sng*tpi*(kL+At%mphas(j)) ! Phase factor exp{-2pi i (k.R_L + phase(n,j) + glb_phase)}
                  onh=0.5_cp
                  Sk=onh*cmplx(At%Skr(:,j),At%Ski(:,j))
                end if
                Sk= ApplyMSO(MGp%MSymOp(k,m),Sk)
                morb(:,mult)=morb(:,mult)+ (real(Sk)*cos(ph) - aimag(Sk)*sin(ph))/onh
             end do

          else if(Mgp%Fcoef_Type == "T") then

             do j=1,MGp%nkv
                m=At%imat(j)
                if(m == 0) cycle
                kL=dot_product(R_latt-real(Lat),MGp%kvec(:,j))
                kr=dot_product(xo,MGp%kvec(:,j)) ! k.xo
                if(Zbelong(2*MGp%kvec(:,j))) then  !This is equivalent to Fcoef_Type="S"
                  ph=sng*tpi*kL
                  Tk=cmplx(At%Skr(:,j),0.0_cp)
                  Tk= ApplyMSO(MGp%MSymOp(k,m),Tk)
                  pha=0.0_cp
                  onh=1.0_cp
                else
                  ph=sng*tpi*(kL+kr+At%mphas(j)) ! Phase factor exp{-2pi i (k.R_L + phase(n,j) + glb_phase)}
                  pha=sng*tpi*kr
                  onh=0.5_cp
                  Tk=onh*cmplx(At%Skr(:,j),At%Ski(:,j))
                  Sk=Tk*cmplx(cos(pha),sin(pha))
                  Sk= ApplyMSO(MGp%MSymOp(k,m),Sk)    !Transformed Fourier coefficient by operator m
                  Tk=Sk*cmplx(cos(pha),-sin(pha))
                end if
                morb(:,mult)=morb(:,mult) + (real(Tk)*cos(ph) - aimag(Tk)*sin(ph))/onh
             end do

          else
             err_CFML%Ierr = 1
             err_CFML%flag = .true.
             err_CFML%Msg  = "Undefined type of complex Fourier coefficients, they must be 'S' or 'T'"
             return
          end if

       end do  Ops

       orbit%Mult=mult
       orbit%Latr=R_Latt
       if(allocated(orbit%pos)) deallocate(orbit%pos)
       allocate(orbit%pos(3,mult))

       if(allocated(orbit%pts)) deallocate(orbit%pts)
       allocate(orbit%pts(mult))

       if(allocated(orbit%Labs)) deallocate(orbit%Labs)
       allocate(orbit%Labs(mult))

       if(allocated(orbit%ChemSymb)) deallocate(orbit%ChemSymb)
       allocate(orbit%ChemSymb(mult))

       if(allocated(orbit%Latt)) deallocate(orbit%Latt)
       allocate(orbit%Latt(3,mult))

       if(allocated(orbit%mom)) deallocate(orbit%mom)
       allocate(orbit%mom(3,mult))

       orbit%pos=orb(:,1:mult)
       orbit%pts=ptr(1:mult)
       orbit%Labs=Labs(1:mult)
       orbit%ChemSymb=Chem(1:mult)
       orbit%Latt=Latt(:,1:mult)
       orbit%mom=morb(:,1:mult)

    End Subroutine Get_kv_Orbit


    !!----
    !!---- Module Subroutine Get_SymSymb(Sim,Tt,Strsym)
    !!----    real(kind=cp)/integer, dimension(3,3), intent( in)    :: sim      !  In -> Rotational part of the S.O.
    !!----    real(kind=cp), dimension( 3),          intent( in)    :: tt       !  In -> Translational part of the S.O.
    !!----    character (len=*),                     intent(out)    :: Strsym   ! Out -> String in th form X,Y,-Z, ...
    !!----
    !!----    Obtain the Jones Faithful representation of a symmetry operator
    !!----
    !!---- Update: February - 2005
    !!

    !!--++
    !!--++ Module Subroutine Get_SymsymbI(Sim,Tt,Strsym)
    !!--++    integer, dimension(3,3),      intent( in)    :: sim      !  In -> Rotational part of the S.O.
    !!--++    real(kind=cp), dimension( 3), intent( in)    :: tt       !  In -> Translational part of the S.O.
    !!--++    character (len=*),            intent(out)    :: Strsym   ! Out -> String in th form X,Y,-Z, ...
    !!--++
    !!--++    (OVERLOADED)
    !!--++    Obtain the Jones Faithful representation of a symmetry operator
    !!--++
    !!--++ Update: February - 2005, January-2014 (changed for a more robust algorithm,JRC)
    !!
    Module Subroutine Get_SymSymbI(X,T,Symb)
       !---- Arguments ----!
       integer,       dimension(3,3), intent( in) :: x
       real(kind=cp), dimension(3),   intent( in) :: t
       character (len=*),          intent(out) :: symb

       !---- Local Variables ----!
       character(len=*),dimension(3),parameter :: xyz=["x","y","z"]
       character(len= 30)              :: car
       character(len= 30),dimension(3) :: sym
       integer           :: i,j

       !---- Main ----!
       symb=" "
       do i=1,3
          sym(i)=" "
          do j=1,3
             if(x(i,j) == 1) then
                sym(i) = trim(sym(i))//"+"//xyz(j)
             else if(x(i,j) == -1) then
                sym(i) =  trim(sym(i))//"-"//xyz(j)
             else if(x(i,j) /= 0) then
               car=" "
               write(unit=car,fmt="(i3,a)") x(i,j),xyz(j)
               if(x(i,j) > 0) car="+"//trim(car)
               sym(i)=trim(sym(i))//pack_string(car)
             end if
          end do
          if (abs(t(i)) > eps_symm ) then
             car= string_fraction_2Dig(t(i))
             sym(i)=trim(sym(i))//trim(car)
          end if
          sym(i)=adjustl(sym(i))
          if(sym(i)(1:1) == "+")  then
            sym(i)(1:1) = " "
            sym(i)=adjustl(sym(i))
          end if
          sym(i)=pack_string(sym(i))
       end do
       symb=trim(sym(1))//","//trim(sym(2))//","//trim(sym(3))
    End Subroutine Get_SymSymbI

    !!--++
    !!--++  Module Subroutine Get_SymSymbR(X,T,Symb)
    !!--++     real(kind=cp),    dimension(3,3),    intent( in) :: x
    !!--++     real(kind=cp),    dimension(3),      intent( in) :: t
    !!--++     character (len=*),                   intent(out) :: symb
    !!--++
    !!--++     (OVERLOADED)
    !!--++     Returning a string for symmetry operators or for points, axes or plane give as
    !!--++     written in fractional form
    !!--++
    !!--++ Update: February - 2005
    !!
    Module Subroutine Get_SymSymbR(X,T,Symb)
       !---- Arguments ----!
       real(kind=cp),    dimension(3,3), intent( in) :: x
       real(kind=cp),    dimension(3),   intent( in) :: t
       character (len=*),                intent(out) :: symb

       !---- Local Variables ----!
       character(len= 30):: car
       integer           :: i,j,k, np,npp,npos
       real(kind=cp)     :: suma

       !---- Main ----!
       symb=" "
       npos=1
       do i=1,3
          npp=0
          do j=1,3
             if (abs(x(i,j)) > 0.0 ) then
                car = string_fraction_2Dig(x(i,j))
                car=adjustl(car)
                if (abs(abs(x(i,j))-1.0) <= eps_symm) then
                     if (npp == 0) then
                        select case (car(1:2))
                           case ("-1")
                              car(2:)=car(3:)//"  "
                           case ("+1")
                              car=car(3:)//"  "
                        end select
                     else
                        car(2:)=car(3:)//"  "
                     end if
                else
                   if (npp == 0) then
                      if (car(1:1) =="+") then
                         car=car(2:)//"  "
                      end if
                   end if
                end if

                np=len_trim(car)
                select case (j)
                   case (1)
                      k=index(car(1:np),"/")
                      if( k /= 0) then
                        if(car(k-1:k-1) == "1") then
                          car(k-1:k-1) = "x"
                          symb(npos:)=car(1:np)
                        else
                          symb(npos:)=car(1:k-1)//"x"//car(k:np)
                        end if
                      else
                        symb(npos:)=car(1:np)//"x"
                      end if
                   case (2)
                      k=index(car(1:np),"/")
                      if( k /= 0) then
                        if(car(k-1:k-1) == "1") then
                          car(k-1:k-1) = "y"
                          symb(npos:)=car(1:np)
                        else
                          symb(npos:)=car(1:k-1)//"y"//car(k:np)
                        end if
                      else
                        symb(npos:)=car(1:np)//"y"
                      end if
                   case (3)
                      k=index(car(1:np),"/")
                      if( k /= 0) then
                        if(car(k-1:k-1) == "1") then
                          car(k-1:k-1) = "z"
                          symb(npos:)=car(1:np)
                        else
                          symb(npos:)=car(1:k-1)//"z"//car(k:np)
                        end if
                      else
                        symb(npos:)=car(1:np)//"z"
                      end if
                end select
                npos=len_trim(symb)+1
                npp=npos
             end if
          end do

          if (abs(t(i)) <= eps_symm .and. npp /= 0) then
             if (i < 3) then
                symb(npos:)=", "
                npos=len_trim(symb)+2
             end if
             cycle
          end if

          car= string_fraction_2Dig(t(i))
          car=adjustl(car)
          suma=0.0
          do j=1,3
             suma=suma+abs(x(i,j))
          end do
          np=len_trim(car)
          if (suma <= 3.0*eps_symm) then
             if (car(1:1) == "+") car=car(2:np)//" "
          end if

          if (i < 3) then
             symb(npos:)=car(1:np)//", "
             npos=len_trim(symb)+2
          else
             symb(npos:)=car(1:np)
          end if
       end do

       symb=pack_string(symb)

    End Subroutine Get_SymSymbR

End SubModule ksym_auxsub
