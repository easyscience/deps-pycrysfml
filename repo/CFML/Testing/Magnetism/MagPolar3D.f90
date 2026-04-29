Program MagPolar3D
 use CFML_GlobalDeps
 use CFML_gSpaceGroups,       only: SPG_type, Write_SpaceGroup_info
 use CFML_Atoms,              only: Atlist_type, Write_Atom_List, MAtom_list_Type
 use CFML_metrics,            only: Cell_G_Type, Write_Crystal_Cell, Get_basis_from_uvw,Zone_Axis_Type
 use CFML_Reflections,        only: H_s
 use CFML_Strings,            only: l_case,file_list_type
 use CFML_IOForm,             only: Read_Xtal_Structure
 use CFML_Propagation_vectors,only: K_Equiv_Minus_K
 use CFML_SXTAL_Geom,         only: GenUB, Get_UB_from_uvw_hkl_omega, &
                                    Get_UB_from_hkl_hkl_omega
 use CFML_Structure_Factors,  only: Calc_hkl_StrFactor, Init_Calc_hkl_StrFactors
 use CFML_Maths,              only: co_linear
 use CFML_kvec_Symmetry
 use CFML_kvec_Structure_Factors
 use CFML_kvec_Polarimetry

 implicit none

 type (file_list_type)          :: fich_cfl
 class(SPG_type), allocatable   :: SpG
 type (MagSymm_k_Type)          :: MGp
 type (Atlist_type)             :: A
 type (MAtom_list_Type)         :: Am
 class(Cell_G_Type),allocatable :: Cell
 type (MagHD_Type)              :: Mh
 type (Magnetic_domain_type)    :: Mag_Dom
 Type (Polar_calc_type)         :: polari
 Type (Zone_Axis_Type)          :: zone_axis

 character(len=256)          :: filcod,line,mess     !Name of the input file
 character(len=1)            :: sig
 real                        :: sn,sf2,omega, wave=0.8
 real, dimension(3)          :: vpl,created_pol,pin,pf,h1,h2
 real, dimension(3,3)        :: UB, polar_tensor,pseudoT
 integer                     :: lun=1, ier,i,j,m,ih,ik,il,iv, n_ini,n_end, &
                                ich, nd, nch
 complex                     :: NSF

 integer                     :: narg
 Logical                     :: esta, arggiven=.false., ub_given=.false., uvw_given=.false., &
                                addref_given=.false., wave_given=.false., ok ,hklp_given=.false.

      !---- Arguments on the command line ----!
      narg=COMMAND_ARGUMENT_COUNT()

      if(narg > 0) then
              call GET_COMMAND_ARGUMENT(1,filcod)
              i=index(filcod,".cfl")
              if( i /= 0 ) filcod=filcod(1:i-1)
              arggiven=.true.
      end if

     write(unit=*,fmt="(/,/,8(a,/))")                                                  &
           "              ------ P r o g r a m    M a g P o l a r (DOMAINS) ------"  , &
           "                    ---- Version 0.1 November-2006 ----"                 , &
           "    ******************************************************************"  , &
           "    * Calculates magnetic structure factors, and magnetic interaction*"  , &
           "    * vectors from magnetic structures by reading a *.CFL file       *"  , &
           "    * Magnetic S-domains and chirality domains are considered        *"  , &
           "    ******************************************************************"  , &
           "                    (JRC- November 2006, testing stage )"
    write(unit=*,fmt=*) " "

     if(.not. arggiven) then
       write(unit=*,fmt="(a)",advance="no") " => Code of the file xx.cfl (give xx): "
       read(unit=*,fmt="(a)") filcod
       if(len_trim(filcod) == 0) stop
       i=index(filcod,".",back=.true.)
       if(i /= 0) filcod=filcod(1:i-1)
     end if

     open(unit=lun,file=trim(filcod)//".cal", status="replace",action="write")
     write(unit=lun,fmt="(/,/,8(a,/))")                                                &
           "              ------ P r o g r a m   M a g P o l a r  (DOMAINS) ------"  , &
           "                    ---- Version 0.2 June-2012 ----"                 , &
           "    ******************************************************************"  , &
           "    * Calculates magnetic structure factors, and magnetic interaction*"  , &
           "    * vectors from magnetic structures by reading a *.CFL file       *"  , &
           "    * The polarisation matrix is also calculated when the orientation*"  , &
           "    * is provided either in the CFL-file or interactively.           *"  , &
           "    * Magnetic S-domains and chirality domains are considered        *"  , &
           "    ******************************************************************"  , &
           "                 (JRC- November 2006, updated: June 2012 )"

     inquire(file=trim(filcod)//".cfl",exist=esta)
     if( .not. esta) then
       write(unit=*,fmt="(a)") " File: "//trim(filcod)//".cfl does'nt exist!"
       stop
     end if
     call Read_Xtal_Structure(trim(filcod)//".cfl",Cell,SpG,A,FileList=fich_cfl)

     If(err_CFML%flag) then
       write(unit=*,fmt="(a)") trim(err_CFML%Msg)
     else

       !Test of the type "file_list_type" by writing at the end of the file
       write(unit=lun,fmt="(/,a)") "    =========================="
       write(unit=lun,fmt="( a )") "    Text of the input CFL file"
       write(unit=lun,fmt="(a,/)") "    =========================="
       write(unit=*,fmt="(/,a)") "    =========================="
       write(unit=*,fmt="( a )") "    Text of the input CFL file"
       write(unit=*,fmt="(a,/)") "    =========================="
       do i=1,fich_cfl%nlines
         write(unit=lun,fmt="(a,i5,a)") " Line:",i,"  "//trim(fich_cfl%line(i))
         write(unit=*,fmt="(a,i5,a)")   " Line:",i,"  "//trim(fich_cfl%line(i))
       end do


       call Write_Crystal_Cell(Cell,lun)
       call Write_SpaceGroup_info(SpG,lun)
       call Write_Atom_List(A,Iunit=lun)

       n_ini=1
       n_end=fich_cfl%nlines
       call Readn_Set_Magnetic_Kv_Structure(fich_cfl,n_ini,n_end,MGp,Am,Mag_dom=Mag_dom,cell=cell)
       if(err_CFML%flag .or. err_CFML%Ierr /= 0) then
         write(unit=*,fmt="(a)") "   "//err_CFML%Msg
         stop
       end if
       !---- looking for orientation information and wavelength
       do i=1,n_end
           line=l_case(adjustl(fich_cfl%line(i)))
           if(line(1:6) == "lambda" .or. line(1:6) == "wave  ") then
             read(unit=line(7:),fmt=*,iostat=ier) wave
             if(ier /= 0) then
                write(unit=*,fmt="(a,i3)") " => Error reading the wavelength at line: ",i
                stop
             end if
             wave_given=.true.
           end if
           if(line(1:5) == "ubmat") then
             m=0
             do j=i+1,i+3
                m=m+1
                read(unit=fich_cfl%line(j),fmt=*,iostat=ier) UB(m,:)
                if(ier /= 0) then
                   write(unit=*,fmt="(a,i3)") " => Error reading the UB-matrix at line: ",j
                   stop
                end if
             end do
             ub_given=.true.
           end if
           if(line(1:15) == "zone_axis_omega") then
             read(unit=line(16:),fmt=*,iostat=ier) Zone_axis%uvw,vpl,omega
             if(ier /= 0) then
                write(unit=*,fmt="(a,i3)") " => Error reading the zone axis uvw, reflection hkl and omega at line: ",i
                stop
             end if
             uvw_given=.true.
           end if
           if(line(1:15) == "hkl_plane_omega") then
             read(unit=line(16:),fmt=*,iostat=ier) h1,h2,omega
             if(ier /= 0) then
                write(unit=*,fmt="(a,i3)") " => Error reading the hkl_plane_omega instruction: reflections hkl-1 hkl-2 and omega-2 at line: ",i
                stop
             end if
             vpl=h1
             hklp_given=.true.
           end if
           if(line(1:14) == "add_reflection") then
             read(unit=line(15:),fmt=*,iostat=ier) vpl
             if(ier /= 0) then
                write(unit=*,fmt="(a,i3)") " => Error reading additional reflection at line: ",i
                stop
             end if
             if(dot_product(vpl,vpl) < 0.00001) then
              write(unit=*,  fmt="(a)") "    The given vector has module = 0, please provide a non-zero vector!"
              stop
             end if
             addref_given=.true.
           end if
           if(wave_given .and. (addref_given .or. uvw_given .or. ub_given)) exit
       end do

       if(uvw_given .and. wave_given) then !Calculate UB matrix from the given information and a reflection of the horizontal plane
         addref_given=.true.
         UB= Get_UB_from_uvw_hkl_omega(wave,Cell,Zone_Axis,vpl,omega)
         if(err_CFML%flag .or. err_CFML%ierr /= 0) then
            write(unit=*,fmt="(a)") " => "//trim(err_CFML%Msg)
            stop
         end if
         UB_given=.true.
         write(unit=*,fmt="(2(a,3i4),a,f8.4)") " => UB-Matrix, deduced from [uvw]= ",Zone_axis%uvw,&
                                               " (hkl)= ",nint(vpl)," and omega=",omega
         write(unit=*,fmt="(tr6,3f10.5)") ub(1,:)
         write(unit=*,fmt="(tr6,3f10.5)") ub(2,:)
         write(unit=*,fmt="(tr6,3f10.5)") ub(3,:)
       end if

       if(hklp_given .and. wave_given) then !Calculate UB matrix from two reflections in the horizontal plane
         addref_given=.true.
         UB= Get_UB_from_hkl_hkl_omega(wave,Cell,h1,h2,omega)

         if(err_CFML%Ierr /= 0) then
            write(unit=*,fmt="(a)") " => "//trim(err_CFML%Msg)
            stop
         end if
         UB_given=.true.
         write(unit=*,fmt="(2(a,3i4),a,f8.4)") " => UB-Matrix, deduced from (hkl)-1= ",nint(h1),&
                                               " (hkl)-2= ",nint(h2)," and omega-2=",omega
         write(unit=*,fmt="(tr6,3f10.5)") ub(1,:)
         write(unit=*,fmt="(tr6,3f10.5)") ub(2,:)
         write(unit=*,fmt="(tr6,3f10.5)") ub(3,:)
       end if

       if(Mag_dom%nd == 0) then
          Mag_dom%nd=1
          Mag_Dom%chir=.false.
          do i=1,3
           Mag_Dom%DMat(i,i,1)=1.0
          end do
          Mag_Dom%pop(1,1)= 1.0
       end if

       call Write_Magnetic_Structure(lun,MGp,Am,Mag_dom=Mag_dom)

       nch=1
       if(Mag_Dom%chir) nch=2

       !  Set nuclear form factorsfor all the atoms
       !  in the structure
       call Init_Calc_hkl_StrFactors(A,SpG,"NUC",wave,lun)

      do

         write(unit=*,fmt="(/a,i2,a)",advance="no") &
                            " => Enter a magnetic reflection as 4 integers -> (h,k,l,m)=H+sign(m)*k(abs(m)): "
         read(unit=*,fmt=*) ih,ik,il,m
         if( m == 0) exit
         !construct partially the object Mh
         j=sign(1,m)
         sig="+"
         if( j == -1) sig="-"
         Mh%signp=real(-j)  ! sign "+" for H-k and "-" for H+k
         iv=abs(m)
         Mh%num_k=iv
         Mh%h= real((/ih,ik,il/)) - Mh%signp*MGp%kvec(:,iv)
         Mh%s = h_s(Mh%h,Cell) !SinTheta/Lambda
         sn=Mh%s*Mh%s            !(SinTheta/Lambda)**2
         Mh%keqv_minus=K_Equiv_Minus_K(MGp%kvec(:,iv),MGp%latt)


         !Calculate magnetic structure factor and magnetic interaction vector
         call Calc_Magnetic_StrF_MiV_Dom(Cell,MGp,Am,Mag_Dom,Mh)

         NSF=(0.0,0.0)
         if( sum(abs(MGp%kvec(:,iv))) <= 0.001) then !Possible nuclear contribution
           call Calc_hkl_StrFactor(mode="SXtal",rad="Neutrons",hn=nint(Mh%h),sn=sn,Atm=A,Grp=SpG,sf2=sf2,fc=NSF)
         end if
         write(unit=lun,fmt="(/,a,3i4,a,3f8.4,a)") "  Reflection: (",ih,ik,il,") "//sig//" (",MGp%kvec(:,iv),")"
         write(unit=lun,fmt="(a,3f8.4,a)")         "              (",Mh%h,")"
         write(unit=lun,fmt="(2(a,f9.5),a)")       "  Nuclear Structure Factor: (", real(NSF)," + i ",aimag(NSF),")"
         write(unit=*,  fmt="(/,a,3i4,a,3f8.4,a)") "  Reflection: (",ih,ik,il,") "//sig//" (",MGp%kvec(:,iv),")"
         write(unit=*,  fmt="(a,3f8.4,a)")         "              (",Mh%h,")"
         write(unit=*,  fmt="(2(a,f9.5),a)")       "  Nuclear Structure Factor: (", real(NSF)," + i ",aimag(NSF),")"

         do nd=1,Mag_Dom%nd
           do ich=1,nch
              write(unit=lun,fmt="(/2(a,i3))")      "  => Magnetic Domain #",nd,"  Chirality Domain #",ich
              write(unit=lun,fmt="(a,2(3f9.5,a))") "  Magnetic Structure   Factor : (",real(Mh%MsF(:,ich,nd)),")+i(",&
                                                      aimag(Mh%MsF(:,ich,nd)),") "
              write(unit=lun,fmt="(a,2(3f9.5,a))") "  Magnetic Interaction Vector : (",real(Mh%MiV(:,ich,nd)),")+i(",&
                                                      aimag(Mh%MiV(:,ich,nd)),") "
              write(unit=lun,fmt="(a,2(3f8.4,a))") "  Magnetic Interaction Vector in Cartesian Coordinates: (",real(Mh%MiVC(:,ich,nd)),")+i(",&
                                                      aimag(Mh%MiVC(:,ich,nd)),") "

              write(unit=*,fmt="(/2(a,i3))")      "  => Magnetic Domain #",nd,"  Chirality Domain #",ich
              write(unit=*,fmt="(a,2(3f8.4,a))") "  Magnetic Structure   Factor : (",real(Mh%MsF(:,ich,nd)),")+i(",&
                                                      aimag(Mh%MsF(:,ich,nd)),") "
              write(unit=*,fmt="(a,2(3f8.4,a))") "  Magnetic Interaction Vector : (",real(Mh%MiV(:,ich,nd)),")+i(",&
                                                      aimag(Mh%MiV(:,ich,nd)),") "
              write(unit=*,fmt="(a,2(3f8.4,a))") "  Magnetic Interaction Vector in Cartesian Coordinates: (",real(Mh%MiVC(:,ich,nd)),")+i(",&
                                                      aimag(Mh%MiVC(:,ich,nd)),") "
            end do
         end do

         write(unit=lun,fmt="(/a,2(3f8.4,a))") "  Average Magnetic Interaction Vector (Cartesian): (",real(Mh%AMiV(:)),")+i(",&
                                                  aimag(Mh%AMiV(:)),") "
         write(unit=lun,fmt="(a,f12.5 )")      "  Square of Average Mag. int.  Vector : ",Mh%sqAMiV
         write(unit=lun,fmt="(a,f12.5 )")      "  Average Square of Mag. int.  Vectors: ",Mh%sqMiV

         write(unit=*,  fmt="(/a,2(3f8.4,a))") "  Average Magnetic Interaction Vector (Cartesian): (",real(Mh%AMiV(:)),")+i(",&
                                                  aimag(Mh%AMiV(:)),") "
         write(unit=*,  fmt="(a,f12.5 )")      "  Square of Average Mag. int.  Vector : ",Mh%sqAMiV
         write(unit=*,  fmt="(a,f12.5 )")      "  Average Square of Mag. int.  Vectors: ",Mh%sqMiV

         write(unit=*,  fmt="(/a)") "    Polarisation Matrix calculation: "

         if(.not. addref_given) then
           do
            write(unit=*,  fmt="( a)",advance="no") " => Enter another reciprocal lattice vector in the horizontal plane: "
            read(unit=*,fmt=*,iostat=ier) vpl
            if(ier /= 0) vpl=(/0.0,0.0,0.0/)
            if(dot_product(vpl,vpl) < 0.00001) then
              write(unit=*,  fmt="(a)") "    The given vector has module = 0, please provide a non-zero vector!"
              cycle
            end if
            exit
           end do
         end if

         call Calc_Polar_Dom(Cell, Mh%h, vpl, 1.0, NSF, Mag_dom, Mh, Polari,ok,mess)
         if(.not. ok) then
          write(*,*) trim(mess)
         end if
         call Write_Polar_Info(Polari, Mag_Dom, info="p")
         call Write_Polar_line(Polari)
         !
         if(wave_given .and. ub_given) then
           pin=[1.0,0.0,0.0]
           call Calc_Polar("BM",wave,Cell,UB, Pin, NSF, Mag_dom, Mh, Pf,ok,line)
           if(.not. ok) then
              write(unit=*,fmt="(a)") " => "//trim(line)
              cycle
           else
              write(unit=lun,fmt="(a,2(3f9.5,tr4))") " => Incident & final polarisation in Blume-Maleyev frame: ",pin,pf
              write(unit=*,  fmt="(a,2(3f9.5,tr4))") " => Incident & final polarisation in Blume-Maleyev frame: ",pin,pf
           end if
           pin=[0.0,1.0,0.0]
           call Calc_Polar("BM",wave,Cell,UB, Pin, NSF, Mag_dom, Mh, Pf,ok,line)
           if(.not. ok) then
              write(unit=*,fmt="(a)") " => "//trim(line)
           else
              write(unit=lun,fmt="(a,2(3f9.5,tr4))") " => Incident & final polarisation in Blume-Maleyev frame: ",pin,pf
              write(unit=*,  fmt="(a,2(3f9.5,tr4))") " => Incident & final polarisation in Blume-Maleyev frame: ",pin,pf
           end if
           pin=[0.0,0.0,1.0]
           call Calc_Polar("BM",wave,Cell,UB, Pin, NSF, Mag_dom, Mh, Pf,ok,line)
           if(.not. ok) then
              write(unit=*,fmt="(a)") " => "//trim(line)
           else
              write(unit=lun,fmt="(a,2(3f9.5,tr4))") " => Incident & final polarisation in Blume-Maleyev frame: ",pin,pf
              write(unit=*,  fmt="(a,2(3f9.5,tr4))") " => Incident & final polarisation in Blume-Maleyev frame: ",pin,pf
           end if

           call Get_Pol_Tensor_Pc("BM",wave,Cell,UB,Pin, NSF, Mag_dom, Mh, Polar_tensor, Created_Pol,ok,line)

           if(.not. ok) then
              write(unit=*,fmt="(a)") " => "//trim(line)
           else
              write(unit=lun,fmt="(/,2(a,3f9.5))") " => Polar Tensor: ",polar_tensor(1,:)," Created Polar: ",Created_Pol
              write(unit=lun,fmt="(a,3f9.5)")      "                  ",polar_tensor(2,:)
              write(unit=lun,fmt="(a,3f9.5)")      "                  ",polar_tensor(3,:)
              write(unit=*,fmt="(2(a,3f9.5))") " => Polar Tensor: ",polar_tensor(1,:)," Created Polar: ",Created_Pol
              write(unit=*,fmt="(a,3f9.5)")    "                  ",polar_tensor(2,:)
              write(unit=*,fmt="(a,3f9.5)")    "                  ",polar_tensor(3,:)
           end if
           !Calculation of the polarization pseudo-tensor
           do j=1,3
             pseudoT(:,j)=polar_tensor(:,j)+Created_Pol
           end do
           write(*,"(/,a)") " => Polarization pseudo-tensor:"
           do j=1,3
              write(unit=*,fmt="(tr6,3f10.5)") pseudoT(j,:)
           end do
        end if

       end do


       write(unit=*,fmt="(a)") " Normal End of program: MagPolar3D"
       write(unit=*,fmt="(a)") " Results in File: "//trim(filcod)//".cal"
     end if

     close(unit=lun)

End Program MagPolar3D
