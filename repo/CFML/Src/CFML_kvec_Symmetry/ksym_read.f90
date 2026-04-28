SubModule (CFML_kvec_Symmetry) ksym_read
   implicit none
   Contains
    !!----
    !!---- Module Subroutine Readn_Set_Magnetic_Kv_Structure(file_cfl,n_ini,n_end,MGp,Am,Mag_dom,Cell)
    !!----    type(file_list_type),                intent (in)     :: file_cfl
    !!----    integer,                             intent (in out) :: n_ini, n_end
    !!----    type(MagSymm_k_Type),                intent (out)    :: MGp
    !!----    type(mAtom_List_Type),               intent (out)    :: Am
    !!----    type(Magnetic_Domain_type),optional, intent (out)    :: Mag_dom
    !!----    type(Cell_G_Type),   optional, intent (in)     :: Cell
    !!----
    !!----    Subroutine for reading and construct the MagSymm_k_Type variable MGp.
    !!----    It is supposed that the CFL file is included in the file_list_type
    !!----    variable file_cfl. On output n_ini, n_end hold the lines with the
    !!----    starting and ending lines with information about a magnetic phase.
    !!----    Optionally the Magnetig space group (Shubnikov group) may be obtained
    !!----    separately for further use.
    !!----    Magnetic S-domains are also read in case of providing the optional variable Mag_dom.
    !!----
    !!---- Updates: November-2006, December-2011, July-2012 (JRC)
    !!
    Module Subroutine Readn_Set_Magnetic_Kv_Structure(file_cfl,n_ini,n_end,MGp,Am,Mag_dom,Cell,database_path)
       !---- Arguments ----!
       type(file_list_type),                intent (in)     :: file_cfl
       integer,                             intent (in out) :: n_ini, n_end
       type(MagSymm_k_Type),                intent (out)    :: MGp
       type(mAtom_List_Type),               intent (out)    :: Am
       type(Magnetic_Domain_type),optional, intent (out)    :: Mag_dom
       type(Cell_G_Type),         optional, intent (in)     :: Cell
       character(len=*),          optional, intent (in)     :: database_path

       !---- Local Variables ----!
       integer :: i,no_iline,no_eline, num_k, num_xsym, num_irrep, num_dom, num_defdom, &
                  num_msym, ier, j, m, n, num_matom, num_skp, ik,im, ip, ncar
       integer,      dimension(5)      :: pos
       real(kind=cp), parameter        :: epsi=0.00001
       real(kind=cp)                   :: ph
       real(kind=cp),dimension(3)      :: rsk,isk,car,side
       real(kind=cp),dimension(3,12)   :: br,bi
       real(kind=cp),dimension(3,3)    :: cart_to_cryst,Rot
       real(kind=cp),dimension(12)     :: coef
       character(len=132)              :: lowline,line
       character(len=50)               :: magmod
       character(len=2)                :: lattice, chardom
       character(len=4)                :: symbcar
       character(len=50)               :: msyr
       character(len=16),dimension(10) :: dire
       logical                         :: msym_begin, kvect_begin, skp_begin, irreps_given, &
                                          irreps_begin, bfcoef_begin, magdom_begin, done, spg_given, lattice_given, &
                                          kvec_given
       class(SPG_Type), allocatable    :: SpG

       call Clear_Error()

       if(n_ini == 0) n_ini=1
       if(n_end == 0) n_end= file_cfl%nlines

       no_iline=0
       no_eline=0

       if(present(Cell)) then
         side(:)=Cell%cell
         cart_to_cryst=Cell%Orth_Cr_cel
       end if

       do i=n_ini,n_end
          ! Read comment
          if(len_trim(file_cfl%line(i)) == 0) cycle
          if (index(file_cfl%line(i)(1:1),"!")/=0 .or. index(file_cfl%line(i)(1:1),"#")/=0) cycle
          lowline=adjustl(l_case(file_cfl%line(i)))

          if (lowline(1:13) == "mag_structure" .or. lowline(1:1) == "{") then
             no_iline=i
          end if
          if (lowline(1:7) =="end_mag" .or. lowline(1:1) == "}") then
             no_eline=i
             exit
          end if
       end do

       n_ini=no_iline
       n_end=no_eline

       if (n_ini == 0 .or. n_end == 0) then
          Err_CFML%flag=.true.
          Err_CFML%Msg=" No magnetic phase found in file!"
          return
       end if
       call Init_MagSymm_k_Type(MGp)

       !Determine the number of symmetry operators existing the magnetic part of the file
       !This is for allocating the dimension of allocatable arrays in MagSymm_k_Type object
       !We will allocate the double for taking into account the possible centring of the magnetic structure
       n=0
       done=.false.
       do i=n_ini,n_end
          lowline=l_case(adjustl(file_cfl%line(i)))
          if (index(lowline(1:4),"symm") == 0 ) cycle
          n=n+1

          !determine now the number of msym cards per symm card
          if(.not. done) then
            m=0
            do j=i+1,i+8
               lowline=l_case(adjustl(file_cfl%line(j)))
               if (index(lowline(1:4),"msym") /= 0 ) then
                 m=m+1
                 cycle
               end if
               done=.true.
               exit
            end do
          end if

       end do
       n=2*n !if it is centred we will need this space
       if(n > 0) then
          !Allocate the allocatable components of MagSymm_k_Type
          if(allocated(MGp%Symop))      deallocate(MGp%Symop)
          if(allocated(MGp%SymopSymb))  deallocate(MGp%SymopSymb)
          if(allocated(MGp%MSymop))     deallocate(MGp%MSymop)
          if(allocated(MGp%MSymopSymb)) deallocate(MGp%MSymopSymb)
          allocate(MGp%Symop(n))
          allocate(MGp%SymopSymb(n))
          MGp%SymopSymb(:) = ''
          if(m > 0) then
             allocate(MGp%MSymop(n,m))
             allocate(MGp%MSymopSymb(n,m))
             MGp%MSymopSymb(:,:) = ''
          end if
       end if


       num_matom=0
       do i=n_ini,n_end
          lowline=l_case(adjustl(file_cfl%line(i)))
          if (index(lowline(1:5),"matom") == 0 ) cycle
          num_matom=num_matom+1
       end do

       Call Allocate_mAtom_list(num_matom,Am)  !Am contains Am%natoms = num_matom
       num_matom=0

       num_k=0
       num_dom=0
       num_defdom=0
       num_xsym=0
       kvect_begin=.true.
       magdom_begin=.true.
       i=n_ini
       irreps_given=.false.
       irreps_begin=.false.
       msym_begin  =.false.
       skp_begin   =.false.
       bfcoef_begin=.false.
       spg_given   =.false.
       lattice_given=.false.
       kvec_given=.false.
       if (present(mag_dom)) then  !Initialise Mag_dom
          Mag_dom%nd=1
          Mag_dom%Chir=.false.
          Mag_dom%Twin=.false.
          Mag_dom%trans=.false.
          Mag_dom%DMat=0
          do j=1,3
           Mag_dom%DMat(j,j,1)=1
          end do
          Mag_dom%Dt=0.0
          Mag_dom%pop=0.0
          Mag_dom%pop(1,1)=1.0 !one domain is always present
          Mag_dom%Lab=" "
       end if

       do
          i=i+1
          if(i >= n_end) exit

          ! Read comment
          if( len_trim(file_cfl%line(i)) == 0) cycle
          lowline=adjustl(l_case(file_cfl%line(i)))
          if (lowline(1:1) == "!" .or. lowline(1:1)=="#") cycle

          ! Detect keywords

          ! Read magnetic model
          ! write(unit=*,fmt="(i6,a)") i,"  -> "//trim(lowline)
          if (lowline(1:6) == "magmod") then
             read(unit=lowline(8:),fmt=*,iostat=ier) magmod
             if (ier /= 0) then
                Err_CFML%flag=.true.
                Err_CFML%Msg=" Error reading magnetic model name in magnetic phase"
                return
             end if
             MGp%MagModel= adjustl(magmod)
             cycle
          end if

          ! Read magnetic field for paramagnetic-induced magnetic moments
          ! The first item is the strength of the magnetic field in Tesla and the three other
          ! items correspond to the vector (in crystallographic space) of the direction of applied field.
          if (lowline(1:9) == "mag_field") then
             read(unit=lowline(10:),fmt=*,iostat=ier) Am%MagField, Am%dir_MField
             if (ier /= 0) then
                Err_CFML%flag=.true.
                Err_CFML%Msg=" Error reading magnetic field in magnetic phase"
                return
             end if
             if( Am%MagField > 0.0001) Am%suscept=.true.
             cycle
          end if

          ! Read lattice
          if (lowline(1:7) == "lattice") then
             read(unit=lowline(9:),fmt=*,iostat=ier) lattice
             if (ier /= 0) then
                Err_CFML%flag=.true.
                Err_CFML%Msg=" Error reading lattice type in magnetic phase"
                return
             end if
             lattice=adjustl(lattice)
             if (lattice(1:1)=="-") then
                MGp%centred = 2
                MGp%latt=u_case(lattice(2:2))
             else
                MGp%centred = 1
                MGp%Latt= u_case(lattice(1:1))
             end if
             lattice_given=.true.
             cycle
          end if

          ! Read type of Fourier coefficients
          if (lowline(1:10) == "fcoef_type") then
             MGp%Fcoef_type = u_case(adjustl(lowline(11:)))
             if(MGp%Fcoef_type /= "T") MGp%Fcoef_type = "S"
             cycle
          end if

          ! Read frame of Fourier coefficients
          if (lowline(1:9) == "spherical") then
             if(.not. present(Cell)) then
               Err_CFML%flag=.true.
               Err_CFML%Msg=" Cell argument is needed when Spherical components are used for Fourier Coefficients!"
             end if
             MGp%Sk_type = "Spherical_Frame"
             cycle
          end if

          ! Read magnetic centrig
          if (lowline(1:7) == "magcent") then
             MGp%mcentred = 2
             cycle
          end if

          ! Read propagation vectors
          if ((lowline(1:5) == "kvect" .or. lowline(1:2) == "k " .or. lowline(1:4) == "kvec" .or. lowline(1:2) == "kv") .and. kvect_begin) then
             num_k=num_k+1
             read(unit=lowline(6:),fmt=*,iostat=ier) MGp%kvec(:,num_k)
             if (ier /= 0) then
                Err_CFML%flag=.true.
                Err_CFML%Msg=" Error reading propagation vectors"
                return
             end if
             ! Try to read the label of the propagation vector
             call Get_Words(file_cfl%line(i),dire,ip)
             if(ip >= 5) MGp%kv_label(num_k)= dire(5)
             if(lattice_given)  MGp%k_eqv_minusK(num_k) =   K_Equiv_Minus_K(MGp%kvec(:,num_k),MGp%latt)
             do !repeat reading until continuous KVECT lines are exhausted
                i=i+1
                lowline=adjustl(l_case(file_cfl%line(i)))
                ! write(unit=*,fmt="(i6,a)") i,"  -> "//trim(lowline)
                if (lowline(1:1) == "!" .or. lowline(1:1) == "#") cycle
                if (lowline(1:5) == "kvect" .or. lowline(1:2) == "k " .or. lowline(1:4) == "kvec" .or. lowline(1:2) == "kv") then
                   num_k=num_k+1
                   read(unit=lowline(6:),fmt=*,iostat=ier) MGp%kvec(:,num_k)
                   if (ier /= 0) then
                      Err_CFML%flag=.true.
                      Err_CFML%Msg=" Error reading propagation vectors"
                      return
                   end if
                   ! Try to read the label of the propagation vector
                   call Get_Words(file_cfl%line(i),dire,ip)
                   if(ip >= 5) MGp%kv_label(num_k)= dire(5)
                   if(lattice_given)  MGp%k_eqv_minusK(num_k) =   K_Equiv_Minus_K(MGp%kvec(:,num_k),MGp%latt)
                else
                   i=i-1
                   kvect_begin=.false.
                   exit
                end if
             end do
             kvec_given=.true.
             cycle
          end if

          ! Read magnetic S-domains
          if (present(mag_dom)) then
             if (lowline(1:6) == "magdom" .and. magdom_begin) then
                num_dom=num_dom+1
                num_defdom=num_defdom+1
                if(index(lowline,"twin") /= 0) Mag_Dom%twin=.true.
                ip=index(lowline,":")
                if(index(lowline,"magdomt") == 0) then
                  msyr=lowline(8:ip-1)
                  call read_msymm(msyr,Mag_Dom%Dmat(:,:,num_dom),ph)
                  Mag_Dom%Dt(:,num_dom)=0.0
                  Mag_Dom%trans=.false.
                else
                  msyr=lowline(9:ip-1)
                  Call Get_Separator_Pos(msyr,",",pos,ncar)
                  if(ncar == 3) then
                    read(unit=msyr(pos(3)+1:),fmt=*,iostat=ier) ph
                    if(ier /= 0) ph=0.0
                    msyr=msyr(1:pos(3)-1)
                  else
                    ph=0.0
                  end if
                  call read_xsym(msyr,1,Mag_Dom%Dmat(:,:,num_dom),Mag_Dom%Dt(:,num_dom))
                  Mag_Dom%Dt(:,num_dom)=0.0
                  Mag_Dom%trans=.true.
                end if
                if (ph > 0.001) then
                  Mag_Dom%chir=.true.
                else
                  Mag_Dom%chir=.false.
                end if
                 if (Mag_Dom%chir) then
                   read(unit=lowline(ip+1:),fmt=*, iostat=ier) Mag_Dom%Pop(1:2,num_dom)
                   write(chardom,"(i2.2)") num_defdom
                   Mag_Dom%Lab(1,num_dom)="magdom"//chardom
                   num_defdom=num_defdom+1
                   write(chardom,"(i2.2)") num_defdom
                   Mag_Dom%Lab(2,num_dom)="magdom"//chardom
                else
                   read(unit=lowline(ip+1:),fmt=*, iostat=ier) Mag_Dom%Pop(1,num_dom)  !, Mag_Dom%MPop(1,num_dom)
                   write(chardom,"(i2.2)") num_defdom
                   Mag_Dom%Lab(1,num_dom)="magdom"//chardom
                end if
                if (ier /= 0) then
                   Err_CFML%flag=.true.
                   Err_CFML%Msg=" Error reading magnetic S-domains"
                   return
                end if
                Mag_Dom%nd = num_dom

                do  !repeat reading until continuous MAGDOM lines are exhausted
                   i=i+1
                   lowline=adjustl(l_case(file_cfl%line(i)))
                   ! write(unit=*,fmt="(i6,a)") i,"  -> "//trim(lowline)
                   if (lowline(1:1) == "!" .or. lowline(1:1) == "#") cycle
                   if (lowline(1:6) == "magdom") then
                      if(index(lowline,"twin") /= 0) Mag_Dom%twin=.true.
                      num_dom=num_dom+1
                      num_defdom=num_defdom+1
                      ip=index(lowline,":")
                      if(index(lowline,"magdomt") == 0) then
                        msyr=lowline(8:ip-1)
                        call read_msymm(msyr,Mag_Dom%Dmat(:,:,num_dom),ph)
                        Mag_Dom%Dt(:,num_dom)=0.0
                        Mag_Dom%trans=.false.
                      else
                        msyr=lowline(9:ip-1)
                        Call Get_Separator_Pos(msyr,",",pos,ncar)
                        if(ncar == 3) then
                          read(unit=msyr(pos(3)+1:),fmt=*,iostat=ier) ph
                          if(ier /= 0) ph=0.0
                          msyr=msyr(1:pos(3)-1)
                        else
                          ph=0.0
                        end if
                        call read_xsym(msyr,1,Mag_Dom%Dmat(:,:,num_dom),Mag_Dom%Dt(:,num_dom))
                        Mag_Dom%Dt(:,num_dom)=0.0
                        Mag_Dom%trans=.true.
                      end if
                      if (ph > 0.001) then
                        Mag_Dom%chir=.true.
                      else
                         Mag_Dom%chir=.false.
                      end if
                      if (Mag_Dom%chir) then
                         read(unit=lowline(ip+1:),fmt=*, iostat=ier) Mag_Dom%Pop(1:2,num_dom) !, Mag_Dom%MPop(1:2,num_dom)
                         write(chardom,"(i2.2)") num_defdom
                         Mag_Dom%Lab(1,num_dom)="magdom"//chardom
                         num_defdom=num_defdom+1
                         write(chardom,"(i2.2)") num_defdom
                         Mag_Dom%Lab(2,num_dom)="magdom"//chardom
                      else
                         read(unit=lowline(ip+1:),fmt=*, iostat=ier) Mag_Dom%Pop(1,num_dom) !, Mag_Dom%MPop(1,num_dom)
                         write(chardom,"(i2.2)") num_defdom
                         Mag_Dom%Lab(1,num_dom)="magdom"//chardom
                      end if
                      if (ier /= 0) then
                         Err_CFML%flag=.true.
                         Err_CFML%Msg=" Error reading magnetic S-domains"
                         return
                      end if
                      Mag_Dom%nd = num_dom
                   else
                      i=i-1
                      magdom_begin=.false.
                      exit
                   end if
                end do
                cycle
             end if
          end if

          ! Read number of irreducible representations and number of basis functions for each
          if (lowline(1:6) == "irreps") then
             read(unit=lowline(7:),fmt=*,iostat=ier) MGp%nirreps
             if (ier /= 0) then
                Err_CFML%flag=.true.
                Err_CFML%Msg=" Error reading number of irreducible representations"
                return
             end if
             read(unit=lowline(7:),fmt=*,iostat=ier) n, (MGp%nbas(j),j=1,MGp%nirreps)
             if (ier /= 0) then
                Err_CFML%flag=.true.
                Err_CFML%Msg=" Error reading number of basis functions of irreducible representations"
                return
             end if
             irreps_given=.true.
             cycle
          end if

          ! Read the indicator real(0)/imaginary(1) of coefficients for basis functions of
          ! irreducible representations
          if (lowline(1:5) == "icomp" .and. irreps_given) then
             num_irrep=1
             n=MGp%nbas(num_irrep)
             read(unit=lowline(6:),fmt=*,iostat=ier) MGp%icomp(1:abs(n),num_irrep)
             if (ier /= 0) then
                Err_CFML%flag=.true.
                Err_CFML%Msg=" Error reading real/imaginary indicators of BF coeff. of irreducible representations"
                return
             end if
             do  !repeat reading until continuous icoebf lines are exhausted
                i=i+1
                lowline=adjustl(l_case(file_cfl%line(i)))
                ! write(unit=*,fmt="(i6,a)") i,"  -> "//trim(lowline)
                if (lowline(1:1) == "!" .or. lowline(1:1) == "#") cycle
                if (lowline(1:5) == "icomp") then
                   num_irrep=num_irrep+1
                   n=MGp%nbas(num_irrep)
                   read(unit=lowline(6:),fmt=*,iostat=ier) MGp%icomp(1:abs(n),num_irrep)
                   if (ier /= 0) then
                      Err_CFML%flag=.true.
                      Err_CFML%Msg=" Error reading real/imaginary indicators of BF coeff. of irreducible representations"
                      return
                   end if
                else
                   i=i-1
                   irreps_given=.false.
                   exit
                end if
             end do
             cycle
          end if

          ! Read SYMM operators
          if (lowline(1:4) == "symm") then
             num_xsym=num_xsym+1
             num_msym=0
             num_irrep=0
             read(unit=lowline(5:),fmt="(a)") MGp%SymopSymb(num_xsym)
             msym_begin=.true.
             irreps_begin=.true.
          end if

          ! Read MSYM operators
          if (lowline(1:4) == "msym" .and. msym_begin) then
             num_msym=num_msym+1
             read(unit=lowline(5:),fmt="(a)") MGp%MSymopSymb(num_xsym,num_msym)
             do  !repeat reading until continuous MSYM lines are exhausted
                i=i+1
                lowline=adjustl(l_case(file_cfl%line(i)))
                ! write(unit=*,fmt="(i6,a)") i,"  -> "//trim(lowline)
                if (lowline(1:1) == "!" .or. lowline(1:1) == "#") cycle
                if (lowline(1:4) == "msym") then
                   num_msym=num_msym+1
                   read(unit=lowline(5:),fmt="(a)") MGp%MSymopSymb(num_xsym,num_msym)
                else
                   i=i-1
                   msym_begin=.false.
                   exit
                end if
             end do
             cycle
          end if

          ! Read basis functions of irreducible representations
          if (lowline(1:4) == "basr" .and. irreps_begin) then
             num_irrep=num_irrep+1
             n=MGp%nbas(num_irrep)
             br=0.0; bi=0.0
             read(unit=lowline(5:),fmt=*,iostat=ier) (br(:,j),j=1,abs(n))
             if (ier /= 0) then
                Err_CFML%flag=.true.
                write(unit=Err_CFML%Msg,fmt="(2(a,i3))")" Error reading basis fuctions (BASR) of irrep ",num_irrep,&
                                                           " for symmetry operator # ",num_xsym
                return
             end if
             if (n < 0) then  !Read the imaginary part of the basis functions
                i=i+1
                lowline=adjustl(l_case(file_cfl%line(i)))
                !write(unit=*,fmt="(i6,a)") i,"  -> "//trim(lowline)
                if (lowline(1:4) == "basi") then
                   read(unit=lowline(5:),fmt=*,iostat=ier) (bi(:,j),j=1,abs(n))
                   if (ier /= 0) then
                      Err_CFML%flag=.true.
                      write(unit=Err_CFML%Msg,fmt="(2(a,i3))")" Error reading basis fuctions (BASI) of irrep ",num_irrep,&
                                                                 " for symmetry operator # ",num_xsym
                      return
                   end if
                else
                   Err_CFML%flag=.true.
                   write(unit=Err_CFML%Msg,fmt="(2(a,i3))")" Lacking BASI keyword of irrep ",num_irrep,&
                                                               " for symmetry operator # ",num_xsym
                   return
                end if
             end if
             do j=1,abs(n)
                MGp%basf(:,j,num_xsym,num_irrep)=cmplx( br(:,j),bi(:,j) )
             end do

             do  !repeat reading until continuous BASR or BASI lines are exhausted
                i=i+1
                lowline=adjustl(l_case(file_cfl%line(i)))
                !write(unit=*,fmt="(i6,a)") i,"  -> "//trim(lowline)
                if (lowline(1:1) == "!" .or. lowline(1:1) == "#") cycle
                if (lowline(1:4) == "basr") then
                   num_irrep=num_irrep+1
                   n=MGp%nbas(num_irrep)
                   br=0.0; bi=0.0
                   read(unit=lowline(5:),fmt=*,iostat=ier) (br(:,j),j=1,abs(n))
                   if (ier /= 0) then
                      Err_CFML%flag=.true.
                      write(unit=Err_CFML%Msg,fmt="(2(a,i3))")" Error reading basis fuctions (BASR) of irrep ",num_irrep,&
                                                                 " for symmetry operator # ",num_xsym
                      return
                   end if
                   if (n < 0) then  !Read the imaginary part of the basis functions
                      i=i+1
                      lowline=adjustl(l_case(file_cfl%line(i)))
                      !write(unit=*,fmt="(i6,a)") i,"  -> "//trim(lowline)
                      if (lowline(1:4) == "basi") then
                         read(unit=lowline(5:),fmt=*,iostat=ier) (bi(:,j),j=1,abs(n))
                         if (ier /= 0) then
                            Err_CFML%flag=.true.
                            write(unit=Err_CFML%Msg,fmt="(2(a,i3))")" Error reading basis fuctions (BASI) of irrep ",num_irrep,&
                                                                       " for symmetry operator # ",num_xsym
                            return
                         end if
                      else
                         Err_CFML%flag=.true.
                         write(unit=Err_CFML%Msg,fmt="(2(a,i3))")" Lacking BASI keyword of irrep ",num_irrep,&
                                                                    " for symmetry operator # ",num_xsym
                         return
                      end if
                   end if
                   do j=1,abs(n)
                      MGp%basf(:,j,num_xsym,num_irrep)=cmplx( br(:,j),bi(:,j) )
                   end do
                else
                   i=i-1
                   irreps_begin=.false.
                   exit
                end if
             end do
             cycle
          end if

          ! Read magnetic atoms:  label, magnetic form factor label,x,y,z,Biso,occ
          if (lowline(1:5) == "matom") then
             num_matom=num_matom+1
             num_skp=0
             line=adjustl(file_cfl%line(i))
             j=index(lowline,"scale")
             if(j /= 0) then
                line=line(1:j-1)
                read(unit=line(6:),fmt=*,iostat=ier) Am%atom(num_matom)%lab,      & !Label
                                                     Am%atom(num_matom)%SfacSymb, & !Formfactor label
                                                     Am%atom(num_matom)%x           !Fract. coord.
             else
                j=index(lowline,"group")
                if(j /= 0) then
                  line=line(1:j-1)
                  read(unit=line(6:),fmt=*,iostat=ier) Am%atom(num_matom)%lab,    & !Label
                                                     Am%atom(num_matom)%SfacSymb, & !Formfactor label
                                                     Am%atom(num_matom)%x           !Fract. coord.
                else
                  call Get_Words(line(6:),dire,ip)
                  Select case(ip)
                     case(5)
                       read(unit=line(6:),fmt=*,iostat=ier) Am%atom(num_matom)%lab,    & !Label
                                                          Am%atom(num_matom)%SfacSymb, & !Formfactor label
                                                          Am%atom(num_matom)%x           !Fract. coord.

                     case(6)
                       read(unit=line(6:),fmt=*,iostat=ier) Am%atom(num_matom)%lab,    & !Label
                                                          Am%atom(num_matom)%SfacSymb, & !Formfactor label
                                                          Am%atom(num_matom)%x,        & !Fract. coord.
                                                          Am%atom(num_matom)%Biso        !Is. Temp. Fact.
                     case(7:)
                       read(unit=line(6:),fmt=*,iostat=ier) Am%atom(num_matom)%lab,    & !Label
                                                          Am%atom(num_matom)%SfacSymb, & !Formfactor label
                                                          Am%atom(num_matom)%x,        & !Fract. coord.
                                                          Am%atom(num_matom)%Biso,     & !Is. Temp. Fact.
                                                          Am%atom(num_matom)%occ         !occupation
                     case default
                        Err_CFML%flag=.true.
                        Err_CFML%Msg=" Error reading magnetic atom, not enough items in MATOM line!"
                        return
                  end Select
                end if
             end if
             if (ier /= 0) then
                Err_CFML%flag=.true.
                write(unit=Err_CFML%Msg,fmt="(a,i4)")" Error reading magnetic atom #",num_matom
                return
             end if
             skp_begin=.true.
             bfcoef_begin=.true.
             cycle
          end if

          ! Read Fourier coefficients in cryst. axes and phase
          if (lowline(1:3) == "skp" .and. skp_begin) then
             num_skp=num_skp+1
             read(unit=lowline(4:),fmt=*,iostat=ier) ik,im,rsk,isk,ph
             if (ier /= 0) then
                Err_CFML%flag=.true.
                write(unit=Err_CFML%Msg,fmt="(a,i3)") " Error reading Fourier Coefficient #", num_skp
                return
             end if
               Am%atom(num_matom)%nvk= num_skp
               Am%atom(num_matom)%imat(ik)= im
               Am%atom(num_matom)%mphas(ik)= ph

             if(MGp%Sk_type == "Spherical_Frame") then
               Am%atom(num_matom)%Spher_Skr(:,ik)= rsk(:)
               Am%atom(num_matom)%Spher_Ski(:,ik)= isk(:)
               !Transform from Cartesian coordinates to unitary Crystallographic frame
               car = Get_Cart_from_Spher([rsk(1),rsk(3),rsk(2)],"D")
               Am%atom(num_matom)%Skr(:,ik)=matmul(cart_to_cryst,car)*side(:)
               car = Get_Cart_from_Spher([isk(1),isk(3),isk(2)],"D")
               Am%atom(num_matom)%Ski(:,ik)=matmul(cart_to_cryst,car)*side(:)
             else  !In this case, the Cell argument may be not given
                   !so no transformation is done. This can be done in other parts of the calling program
               Am%atom(num_matom)%Skr(:,ik)= rsk(:)
               Am%atom(num_matom)%Ski(:,ik)= isk(:)
             end if

             do  !repeat reading until continuous SKP lines are exhausted
                i=i+1
                lowline=adjustl(l_case(file_cfl%line(i)))
                !write(unit=*,fmt="(i6,a)") i,"  -> "//trim(lowline)
                if (lowline(1:1) == "!" .or. lowline(1:1) == "#") cycle
                if (lowline(1:3) == "skp") then
                   num_skp=num_skp+1
                   if (num_skp > 12) then
                      Err_CFML%flag=.true.
                      Err_CFML%Msg= " Too many Fourier Coefficients, the maximum allowed is 12! "
                      return
                   end if
                   read(unit=lowline(4:),fmt=*,iostat=ier) ik,im,rsk,isk,ph
                   if (ier /= 0) then
                      Err_CFML%flag=.true.
                      write(unit=Err_CFML%Msg,fmt="(a,i3)") " Error reading Fourier Coefficient #", num_skp
                      return
                   end if
                   Am%atom(num_matom)%nvk= num_skp
                   Am%atom(num_matom)%imat(ik)= im
                   Am%atom(num_matom)%Skr(:,ik)= rsk(:)
                   Am%atom(num_matom)%Ski(:,ik)= isk(:)
                   Am%atom(num_matom)%mphas(ik)= ph
                else
                   i=i-1
                   skp_begin=.false.
                   Am%atom(num_matom)%nvk= num_skp
                   exit
                end if
             end do
          end if

          ! Read Local Susceptibility coefficients in cryst. axes
          if (lowline(1:3) == "chi") then
             read(unit=lowline(4:),fmt=*,iostat=ier) coef(1:6)
             if (ier /= 0) then
                Err_CFML%flag=.true.
                write(unit=Err_CFML%Msg,fmt="(a,i3)") " Error reading Local Susceptibility Coefficient for atom #", num_matom
                return
             end if
               Am%atom(num_matom)%chi= coef(1:6)
               if(abs(coef(1)-coef(2)) < epsi .and. abs(coef(2)-coef(3)) < epsi .and. sum(abs(coef(4:6))) < epsi ) then
                 Am%atom(num_matom)%chitype="isotr"
               else
                 Am%atom(num_matom)%chitype="aniso"
               end if
          end if

          if (lowline(1:6) == "bfcoef" .and. bfcoef_begin) then
             num_skp=num_skp+1
             read(unit=lowline(7:),fmt=*,iostat=ier) ik,im
             n=abs(MGp%nbas(im))
             read(unit=lowline(7:),fmt=*,iostat=ier) ik,im,coef(1:n),ph
             if (ier /= 0) then
                Err_CFML%flag=.true.
                write(unit=Err_CFML%Msg,fmt="(a,i3)") " Error reading Coefficient of Basis Functions #", num_skp
                return
             end if
             Am%atom(num_matom)%nvk= num_skp
             Am%atom(num_matom)%imat(ik)= im
             Am%atom(num_matom)%cbas(1:n,ik)= coef(1:n)
             Am%atom(num_matom)%mphas(ik)= ph

             do  !repeat reading until continuous bfcoef lines are exhausted
                i=i+1
                lowline=adjustl(l_case(file_cfl%line(i)))
                if (lowline(1:1) == "!" .or. lowline(1:1) == "#") cycle
                !write(unit=*,fmt="(i6,a)") i,"  -> "//trim(lowline)
                if (lowline(1:6) == "bfcoef" ) then
                   num_skp=num_skp+1
                   if (num_skp > 12) then
                      Err_CFML%flag=.true.
                      Err_CFML%Msg= " Too many sets of Coefficients, the maximum allowed is 12! "
                      return
                   end if
                   read(unit=lowline(7:),fmt=*,iostat=ier) ik,im
                   n=abs(MGp%nbas(im))
                   read(unit=lowline(7:),fmt=*,iostat=ier) ik,im,coef(1:n),ph
                   if (ier /= 0) then
                      Err_CFML%flag=.true.
                      write(unit=Err_CFML%Msg,fmt="(a,i3)") " Error reading Coefficient of Basis Functions #", num_skp
                      return
                   end if
                   Am%atom(num_matom)%nvk= num_skp
                   Am%atom(num_matom)%imat(ik)= im
                   Am%atom(num_matom)%cbas(1:n,ik)= coef(1:n)
                   Am%atom(num_matom)%mphas(ik)= ph
                else
                   i=i-1
                   bfcoef_begin=.false.
                   Am%atom(num_matom)%nvk= num_skp
                   exit
                end if
             end do
          end if
       end do

       !Arriving here we have exhausted reading magnetic phase
       if(num_matom == 0 .and. .not. kvec_given) then ! No information on magnetic structure is really provided
         Err_CFML%flag=.true.
         Err_CFML%Msg= " No information on magnetic structure is really provided! "
         return
       end if

       !Check if it is an induced paramagnetic magnetic structure due to an applied magnetic field
       !In such a case use the crystal space group to construct the magnetic matrices. If the symbol
       !of the space group is not provided it is supposed that the symmetry operators have been provided
       !together with th SYMM and MSYM matrices
       if(Am%suscept) then
         do i=1,file_cfl%nlines
            lowline=adjustl(l_case(file_cfl%line(i)))
            if (lowline(1:4) == "spgr" .or. lowline(1:3) == "spg" .or. lowline(1:6) == "spaceg") then
               lowline=adjustl(file_cfl%line(i))
               j=index(lowline," ")
               lowline=lowline(j+1:)
               if (present(database_path)) then
                  call Set_SpaceGroup(trim(lowline),SpG,database_path=database_path)
               else
                  call Set_SpaceGroup(trim(lowline),SpG)
               end if
               spg_given   =.true.
               exit
            end if
         end do
         if(spg_given) then
            n=SpG%Numops * SpG%Centred
            MGp%Centred=SpG%Centred
            MGp%MCentred=1  !Same rotation matrices as that of the space group
            MGp%Latt=SpG%SPG_Symb(1:1)
            num_xsym=SpG%Numops
            num_msym=1
            num_k=1
            if(allocated(MGp%Symop))      deallocate(MGp%Symop)
            if(allocated(MGp%SymopSymb))  deallocate(MGp%SymopSymb)
            if(allocated(MGp%MSymop))     deallocate(MGp%MSymop)
            if(allocated(MGp%MSymopSymb)) deallocate(MGp%MSymopSymb)
            allocate(MGp%Symop(n))
            allocate(MGp%SymopSymb(n))
            allocate(MGp%MSymop(n,1))
            allocate(MGp%MSymopSymb(n,1))
            MGp%SymopSymb(:) = ''
            MGp%MSymopSymb(:,:) = ''

            do i=1,num_xsym
              lowline=" "
              MGp%SymopSymb(i)=SpG%Symb_Op(i)
              Rot=Spg%Op(i)%Mat(1:3,1:3)
              call Get_SymSymb(Rot,[0.0,0.0,0.0],lowline)
              do j=1,len_trim(lowline)
                 if(lowline(j:j) == "x") lowline(j:j) = "u"
                 if(lowline(j:j) == "y") lowline(j:j) = "v"
                 if(lowline(j:j) == "z") lowline(j:j) = "w"
              end do
              MGp%MSymopSymb(i,1) = trim(lowline)//", 0.0"
            end do
         else
           !No action to be taken ... the symmetry operators are read in SYMM cards
         end if
       end if

       !Get pointers to the magnetic form factors
       !Stored for each atom in the component ind(1)
       call Set_Magnetic_Form()

       !---- Find Species in Magnetic_Form ----!
       do i=1,Am%natoms
          symbcar=u_case(Am%atom(i)%SfacSymb)
          do j=1,num_mag_form
             if (symbcar /= Magnetic_Form(j)%Symb) cycle
             Am%atom(i)%ind(2)=j
             exit
          end do
       end do

       !Now construct the rest of magnetic symmetry type variable MGp
       MGp%nmsym =num_msym
       MGp%Numops=num_xsym
       MGp%nkv   =num_k

       !Construct the numerical symmetry operators
       do i=1,MGp%Numops
          Call Read_Xsym(MGp%SymopSymb(i),1,MGp%Symop(i)%Rot,MGp%Symop(i)%tr)
          do j=1,MGp%nmsym
             Call Read_Msymm(MGp%MSymopSymb(i,j),MGp%MSymop(i,j)%Rot,MGp%MSymop(i,j)%Phas)
          end do
       end do
       if (Err_CFML%flag) then
          Err_CFML%flag=.true.
          write(unit=Err_CFML%Msg,fmt="(a)") " Error reading symmetry: "//trim(Err_CFML%Msg)
          return
       end if

       !Complete the set of symmetry operators with the centre of symmetry
       m=MGp%Numops
       if (MGp%centred == 2) then
          do i=1,MGp%Numops
             m=m+1
             MGp%Symop(m)%Rot(:,:) = -MGp%Symop(i)%Rot(:,:)
             MGp%Symop(m)%tr(:)    =  modulo_lat(-MGp%Symop(m)%tr(:))
             call Get_SymSymb(MGp%Symop(m)%Rot(:,:), &
                              MGp%Symop(m)%tr(:), MGp%SymopSymb(m))
             if (Mgp%mcentred == 1) then  !Anticentre in the magnetic structure
                do j=1,MGp%nmsym
                   MGp%MSymop(m,j)%Rot(:,:) = -MGp%MSymop(i,j)%Rot(:,:)
                   MGp%MSymop(m,j)%Phas     = -MGp%MSymop(i,j)%Phas
                end do
             else if(Mgp%mcentred == 2) then
                do j=1,MGp%nmsym
                   MGp%MSymop(m,j)%Rot(:,:) =  MGp%MSymop(i,j)%Rot(:,:)
                   MGp%MSymop(m,j)%Phas     =  MGp%MSymop(i,j)%Phas
                end do
             end if
          end do
       end if

       !Get the centring lattice translations of the crystallographic structure
       !and calculate the general multiplicity of the group.
       Mgp%Num_Lat=1
       MGp%Ltr(:,:) = 0.0
       Select Case(MGp%Latt)
          case ("A")
             Mgp%Num_Lat=2
             MGp%Ltr(:,1:2)=Ltr_a(:,1:2)
          case ("B")
             Mgp%Num_Lat=2
             MGp%Ltr(:,1:2)=Ltr_b(:,1:2)
          case ("C")
             Mgp%Num_Lat=2
             MGp%Ltr(:,1:2)=Ltr_c(:,1:2)
          case ("I")
             Mgp%Num_Lat=2
             MGp%Ltr(:,1:2)=Ltr_i(:,1:2)
          case ("R")
             Mgp%Num_Lat=3
             MGp%Ltr(:,1:3)=Ltr_r(:,1:3)
          case ("F")
             Mgp%Num_Lat=4
             MGp%Ltr(:,1:4)=Ltr_f(:,1:4)
       End Select

       select case (MGp%centred)
          case (1)
             MGp%Multip =   MGp%Numops * Mgp%Num_Lat
          case (2)
             MGp%Multip = 2 * MGp%Numops * Mgp%Num_Lat
       end select

    End Subroutine Readn_Set_Magnetic_Kv_Structure

End SubModule ksym_read