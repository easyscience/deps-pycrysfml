Module Ref_Gen

    use CFML_GlobalDeps
    use CFML_Maths,           only: Co_Prime_Vector,cross_product, set_eps_math
    use CFML_ILL_Instrm_data, only:  &
                                    SXTAL_Orient_type, Current_Orient, diffractometer_type, &
                                    Current_Instrm, Read_Current_Instrm, Update_Current_Instrm_UB,&
                                    Set_default_Instrument,Write_Current_Instrm_data
    use CFML_Strings,         only: l_case, file_List_type
    use CFML_metrics,         only: Cell_G_Type,Set_Crystal_Cell, Write_Crystal_Cell,Zone_Axis_type, &
                                    Get_basis_from_uvw
    use CFML_SXTAL_Geom

    implicit none

    private
    public :: read_sxtal_geom,  calc_angles

    character(len=256),  public :: cfl_file
    character(len=256),dimension(:), allocatable,  public :: hkl_file

    !UB matrices of the different domains
    integer,                    public :: n_twins=1, n_frames=0, n_framr=30   !max=6
    real,    dimension(3,3,6),  public :: ub_matrix, ub_matrix_ini
    real,    dimension(3,3),    public :: R_UB,Chi_matx,Phi_matx
    integer,                    public :: n_phi, n_chi
    real, dimension(20),        public :: phi_val,chi_val
    real,                       public :: delta_ang,time_ref=3.0,sec_frame=4.0
    logical,                    public :: optimize=.false., scann=.false.,scan_chi=.false.,scan_phi=.false.,unique=.false., strict=.false.
    real, parameter, dimension(3,3), public :: Identity = reshape([1.0,0.0,0.0,  0.0,1.0,0.0,  0.0,0.0,1.0],[3,3])

 contains

    Subroutine read_sxtal_geom(ipr,cell,file_dat,ok,mess,ord,spher,schwinger,hlim,sgiven,smin,smax,opgiven,iop_ord)
        !---- Arguments ----!
        integer,                intent( in)    :: ipr
        type(Cell_G_Type),      intent(in out) :: cell
        Type(file_List_type),   intent( in)    :: file_dat
        logical,                intent(out)    :: ok
        character(len=*),       intent(out)    :: mess
        integer,dimension(3),   intent(out)    :: ord
        logical,                intent(out)    :: spher
        logical,                intent(out)    :: schwinger
        integer,dimension(3,2), intent(out)    :: hlim
        logical, optional,      intent(out)    :: sgiven,opgiven
        real,    optional,      intent(out)    :: smin,smax
        integer, optional,      intent(out)    :: iop_ord
        !---- Local variables ----!
        character(len=132)   :: line, file_inst
        real                 :: wave,wav,tmin,tmax,omeg,chi,phi,dmin!,s1,s2,ang12,tet1,tet2
        real, dimension(3,3) :: ub
        integer, dimension(3):: uvw
        real,    dimension(3):: h1,h2,hv
        real, dimension(6)   :: dcel,incel
        integer              :: i,ier,j, n, i_def, igeom !, lun
        logical              :: esta, ub_read, wave_read, inst_read, trang_read, mod_ub
        type(Zone_Axis_Type) :: zone_axis

        ok=.false.
        ub_read=.false.
        mod_ub=.false.
        wave_read=.false.
        inst_read=.false.
        spher=.false.
        schwinger=.false.
        trang_read=.false.
        igeom=0
        mess= " "
        ord=[3,2,1]
        incel(1:3)=cell%cell
        incel(4:6)=cell%ang
        ub=reshape([1.0,0.0,0.0,  0.0,1.0,0.0,  0.0,0.0,1.0],[3,3])
        hlim=0
        if(present(opgiven)) opgiven=.false. !Initialise to false, put to .true. if ANGOR is provided

        do j=1,file_dat%nlines
            line=adjustl(file_dat%line(j))
            line=l_case(line)
            if (line(1:1) ==" ") cycle
            if (line(1:1) =="!") cycle
            i=index(line,"!")
            if( i /= 0) line=line(1:i-1)
            i=index(line," ")-1
            select case (line(1:i))

                case("srang")
                    if(present(sgiven) .and. present(smin) .and. present(smax)) then
                        read(unit=line(6:),fmt=*,iostat=ier) smin,smax
                        if(ier /= 0) then
                            mess="Error reading Smin and Smax in CFL file: "//trim(cfl_file)
                            sgiven=.false.
                            return
                        else
                            sgiven=.true.
                        end if
                    end if

                case("unique")
                    unique=.true.

                case("strict")
                    strict=.true.

                case("dmin")
                    if(present(sgiven) .and. present(smax)) then
                        read(unit=line(6:),fmt=*,iostat=ier) dmin
                        if(ier /= 0) then
                            mess="Error reading dmin in CFL file: "//trim(cfl_file)
                            sgiven=.false.
                            return
                        else
                            smin=0.0
                            smax=0.5/dmin
                            sgiven=.true.
                        end if
                    end if

                case("trang")
                    if(present(sgiven) .and. present(smin) .and. present(smax) .and. wave_read) then
                        read(unit=line(6:),fmt=*,iostat=ier) tmin,tmax
                        if(ier /= 0) then
                            mess="Error reading 2theta_min and 2Theta_max in CFL file: "//trim(cfl_file)
                            sgiven=.false.
                            return
                        else
                            smin=sind(0.5*tmin)/wave
                            smax=sind(0.5*tmax)/wave
                            sgiven=.true.
                            trang_read=.true.
                        end if
                    end if

                case("hlim")
                    ! hmin hmax  kmin kmax  lmin lmax
                    read(unit=line(6:),fmt=*,iostat=ier) ((hlim(i,n),n=1,2),i=1,3)
                    if(ier /= 0 ) then
                        mess="Error reading hkl-limits in CFL file: "//trim(cfl_file)
                        return
                    end if

                case("instr")
                    line=file_dat%line(j)
                    i=index(line,"!")
                    if(i /= 0) line=line(1:i-1)
                    i=index(trim(line)," ",back=.true.)
                    file_inst=trim(adjustl(line(i:)))
                    inquire(file=trim(file_inst), exist=esta)
                    if(.not. esta) then
                        mess="File "//trim(file_inst)//" not found!"
                        return
                    end if
                    call Read_Current_Instrm(trim(file_inst))
                    if(Current_Orient%orient_set .and. .not. ub_read) then
                      ub=Current_Orient%ub
                      ub_read=.true.
                    end if
                    inst_read=.true.

                case("ubmat","ubmat1")
                    do i=1,3
                        read(unit=file_dat%line(j+i),fmt=*,iostat=ier) ub(i,:)
                        if(ier /= 0 ) then
                            mess="Error reading the orientation matrix in CFL file: "//trim(cfl_file)
                            return
                        end if
                    end do
                    !call Set_Current_Orient(wave,ub)
                    ub_read=.true.
                    ub_matrix(:,:,1)=ub
                    n_twins=1

                case("ubmat2")
                    do i=1,3
                        read(unit=file_dat%line(j+i),fmt=*,iostat=ier) ub_matrix(i,:,2)
                        if(ier /= 0 ) then
                            mess="Error reading the orientation matrix of twin 2 in CFL file: "//trim(cfl_file)
                            return
                        end if
                    end do
                    n_twins=2

                case("ubmat3")
                    do i=1,3
                        read(unit=file_dat%line(j+i),fmt=*,iostat=ier) ub_matrix(i,:,3)
                        if(ier /= 0 ) then
                            mess="Error reading the orientation matrix of twin 3 in CFL file: "//trim(cfl_file)
                            return
                        end if
                    end do
                    n_twins=3

                case("ubmat4")
                    do i=1,3
                        read(unit=file_dat%line(j+i),fmt=*,iostat=ier) ub_matrix(i,:,4)
                        if(ier /= 0 ) then
                            mess="Error reading the orientation matrix of twin 4 in CFL file: "//trim(cfl_file)
                            return
                        end if
                    end do
                    n_twins=4

                case("ubmat5")
                    do i=1,3
                        read(unit=file_dat%line(j+i),fmt=*,iostat=ier) ub_matrix(i,:,5)
                        if(ier /= 0 ) then
                            mess="Error reading the orientation matrix of twin 5 in CFL file: "//trim(cfl_file)
                            return
                        end if
                    end do
                    n_twins=5

                case("ubmat6")
                    do i=1,3
                        read(unit=file_dat%line(j+i),fmt=*,iostat=ier) ub_matrix(i,:,6)
                        if(ier /= 0 ) then
                            mess="Error reading the orientation matrix of twin 6 in CFL file: "//trim(cfl_file)
                            return
                        end if
                    end do
                    n_twins=6

                case("geom")
                    read(unit=line(6:),fmt=*,iostat=ier) igeom
                    if(ier /= 0 ) then
                        mess="Error reading the diffraction geometry in CFL file: "//trim(cfl_file)
                        return
                    end if

                case("wave")
                    read(unit=line(6:),fmt=*,iostat=ier) wave
                    if(ier /= 0 ) then
                        mess="Error reading the wavelength in CFL file: "//trim(cfl_file)
                        return
                    end if
                    wave_read=.true.

                case("order")
                    read(unit=line(6:),fmt=*,iostat=ier) ord
                    if(ier /= 0 ) then
                        mess="Error reading hkl-ordering in CFL file: "//trim(cfl_file)
                        return
                    end if

                case("angor")

                    if(present(iop_ord)) then
                        iop_ord=0
                        i=index(line,"2theta")
                        if(i /= 0) iop_ord=1
                        i=index(line,"gamma")
                        if(i /= 0) iop_ord=1
                        i=index(line,"omega")
                        if(i /= 0) iop_ord=2
                        i=index(line," chi")
                        if(i /= 0) iop_ord=3
                        i=index(line,"nu")
                        if(i /= 0) iop_ord=3
                        i=index(line," phi")
                        if(i /= 0) iop_ord=4
                        i=index(line,"mix_phi")
                        if(i /= 0) then
                          iop_ord=5
                          read(line(i+7:),*,iostat=ier) delta_ang
                          if(ier /= 0) delta_ang=5.0
                        end if
                        i=index(line,"mix_chi")
                        if(i /= 0) then
                          iop_ord=6
                          read(line(i+7:),*,iostat=ier) delta_ang
                          if(ier /= 0) delta_ang=5.0
                        end if
                        i=index(line,"mix_ome")
                        if(i /= 0) then
                          iop_ord=7
                          read(line(i+7:),*,iostat=ier) delta_ang
                          if(ier /= 0) delta_ang=5.0
                        end if

                        if(present(opgiven)) opgiven=.true.

                    end if

                case("optimize")
                    optimize=.true.

                case("time_ref")
                    read(line(9:),*,iostat=ier) time_ref
                    if(ier /= 0) time_ref=3.0

                case("n_framr")
                    read(line(8:),*,iostat=ier) n_framr,sec_frame
                    if(ier /= 0) then
                      n_framr=30
                      sec_frame=3.0
                    end if
                    time_ref=(n_framr*sec_frame)/60.0

                case("n_frames")
                    read(line(8:),*,iostat=ier) n_frames,sec_frame
                    if(ier /= 0) then
                      n_frames=1500
                      sec_frame=3.0
                    end if

                case("spher")
                    spher=.true.

                case("schwinger")
                    schwinger=.true.

                case("scan_chi")
                    read(unit=line(9:),fmt=*,iostat=ier) n_chi, (chi_val(i),i=1,n_chi)
                    if(ier /= 0) then
                        mess="Error reading the values of Chi positions in CFL file: "//trim(cfl_file)
                        return
                    else
                      scan_chi =.true.
                    end if

                case("scan_phi")
                    read(unit=line(9:),fmt=*,iostat=ier) n_phi, (phi_val(i),i=1,n_phi)
                    if(ier /= 0) then
                        mess="Error reading the values of Phi positions in CFL file: "//trim(cfl_file)
                        return
                    else
                      scan_phi =.true.
                    end if

                case("orient_hh")
                    if(.not. wave_read .and. .not. inst_read) then
                      write(unit=*,fmt="(a)") " => Wavelength shoud be provided!"
                      mess="Wavelength shoud be provided in CFL file: "//trim(cfl_file)
                      return
                    else if(.not. wave_read) then
                      wave=Current_Orient%wave
                    end if
                    read(unit=line(10:),fmt=*) h1,h2,omeg
                    hv=cross_product(h1,h2)
                    call Co_Prime_Vector(nint(hv),uvw)
                    Zone_Axis=Get_basis_from_uvw(0.5,uvw,cell)
                    if(err_CFML%ierr == 0) then
                      UB=Get_UB_from_uvw_hkl_omega(wave,Cell,Zone_Axis,h1,omeg)
                      if(err_CFML%ierr /= 0 .or. err_CFML%flag) then
                        write(unit=*,fmt="(a)") " => UB cannot be calculated from ORIENT_HH instruction "
                        write(unit=*,fmt="(a)") " => "//trim(err_CFML%Msg)
                        return
                      end if
                    else
                      write(unit=*,fmt="(a)") " => UB cannot be calculated from ORIENT_HH instruction "
                      write(unit=*,fmt="(a,3i4,tr3,3i4,a,f7.2,a,3i4)") " => Problem with reflections:",nint(h1),nint(h2), &
                                       " with omega(h1)=",omeg," and calculated [uvw]:",uvw
                      return
                    end if
                    ub_read=.true.

                case("orient_vh")
                    if(.not. wave_read .and. .not. inst_read) then
                      write(unit=*,fmt="(a)") " => Wavelength shoud be provided!"
                      return
                    else if(.not. wave_read) then
                      wave=Current_Orient%wave
                    end if

                    read(unit=line(10:),fmt=*) uvw,h1,omeg
                    Zone_Axis=Get_basis_from_uvw(0.5,uvw,cell)
                    if(err_CFML%ierr == 0) then
                      UB=Get_UB_from_uvw_hkl_omega(wave,Cell,Zone_Axis,h1,omeg)
                      if(err_CFML%ierr /= 0 .or. err_CFML%flag) then
                        write(unit=*,fmt="(a)") " => UB cannot be calculated from ORIENT_VH instruction "
                        write(unit=*,fmt="(a)") " => "//trim(err_CFML%Msg)
                        return
                      end if
                    else
                      write(unit=*,fmt="(a)") " => UB cannot be calculated from ORIENT_VH instruction "
                      write(unit=*,fmt="(a,6f7.3,a,f7.2)") " => Problem with Zone axis and reflection: ",uvw,h1, " with omega(h1)=",omeg
                      return
                    end if
                    ub_read=.true.

                 case("mod_ub")  !Should be given after all ubmatn

                    if(.not. ub_read .and. .not. inst_read) then
                      write(unit=*,fmt="(a)") " => UB shoud be provided before modification!"
                      return
                    else
                      read(unit=line(7:),fmt=*,iostat=ier) chi,phi
                      if(ier /= 0) then
                        write(unit=*,fmt="(a)") " => Error reading Chi-Phi, UB cannot be modified!"
                        cycle
                      end if
                      Chi_matx=Chi_mat(Chi)
                      Phi_matx=Phi_mat(Phi)
                      do i=1,n_twins
                        ub_matrix_ini(:,:,i)=ub_matrix(:,:,i)
                        ub_matrix(:,:,i)=matmul(Chi_matx,matmul(Phi_matx,ub_matrix_ini(:,:,i)))
                      end do
                      mod_ub=.true.
                    end if
            end select
        end do

        if(.not. inst_read ) then
            file_inst="default_instrument.geom"
            call Set_default_Instrument()
            write(unit=*,fmt="(a)") " => No Instrument file has been provided!"
            write(unit=*,fmt="(a)") " => A default instrument file (default_instrm.geom) has been written"
            open(newunit=i_def, file=trim(file_inst), status="replace",action="write")
            call Write_Current_Instrm_data(i_def,trim(file_inst))
            call flush(i_def)
            close(unit=i_def)
        end if

        if(igeom /= 0 .and. inst_read) Current_Instrm%igeom=igeom !overrides the instrument geometry

        if(.not. ub_read) then                   !There is currently no UB-Matrix, try to read it from ubfrom.raf
          inquire(file="ubfrom.raf",exist=esta)  !checking if "ubfrom.raf" is present in the current directory
          if(.not. esta) then
             mess="No UB-matrix in instrument/CFL files, no ubfrom.raf available!  "
             return
          else  !Read everything from ubfrom.raf only in case the UB-matrix and wavelength are not read
             open(newunit=i_def, file="ubfrom.raf", status="old",action="read",position="rewind")
             do i=1,3
               read(unit=i_def,fmt="(a)",iostat=ier) line
               read(unit=line(1:50),fmt=*,iostat=ier) ub(i,:)
               if(ier /= 0 ) then
                   mess="Error reading the orientation matrix in file: ubfrom.raf "
                   return
               end if
             end do
             ub_matrix(:,:,1)=ub
             if(mod_ub) then
               ub_matrix_ini(:,:,1)=ub
               ub_matrix(:,:,1)=matmul(Chi_matx,matmul(Phi_matx,ub_matrix_ini(:,:,1)))
             end if
             n_twins=1
             ub_read=.true.
             read(unit=i_def,fmt=*,iostat=ier) wav  !Read wavelength from file ubfrom.raf
             if(ier == 0 ) then
               wave=wav
               wave_read=.true.
             end if
             read(unit=i_def,fmt=*,iostat=ier) dcel !Read unit cell from file ubfrom.raf
             if(ier == 0 ) then
               if(sum(abs(dcel-incel)) > 0.001) then  !Output information is the unit cell in ubfrom.raf
                 incel=dcel                           !is different from the values provided in CFL file
                 !Reconstruct the unit cell type
                 call Set_Crystal_Cell(incel(1:3),incel(4:6),Cell,"A")
                 write(unit=ipr,fmt="(/,/,a/)")  "  => New unit cell read from ubfrom.raf !"
                 call Write_Crystal_Cell(Cell,ipr)
               end if
             end if
             if(trang_read) then !re-calculate smin,smax for the new wavelength is 2theta-range has been provided
                smin=sind(0.5*tmin)/wave
                smax=sind(0.5*tmax)/wave
             end if
             close(unit=i_def)
          end if

        Else  !Write the  UB-matrices for all domains in case of mod_ub by chi-phi

          if(mod_ub) then
             write(unit=ipr,fmt="(/,/,2(a,f8.4))")  "  => The original UB-matrix has been modified by Chi =",chi, &
             " and Phi =",Phi
             do j=1,n_twins
               write(unit=ipr,fmt="(/,a,i3)") "       ORIGINAL UB-Matrix                 TRANSFORMED UB-Matrix    for domain: ",j
               do i=1,3
                  write(unit=ipr,fmt="(3f10.6,tr8,3f10.6)")  ub_matrix_ini(i,:,j), ub_matrix(i,:,j)
               end do
             end do
          else
             if(n_twins > 1) then
               write(unit=ipr,fmt="(/,a)") " => Orientation matrices for all domains:"
               do j=1,n_twins
                 write(unit=ipr,fmt="(/,a,i3)") "       UB-Matrix for domain: ",j
                 do i=1,3
                    write(unit=ipr,fmt="(tr2,3f10.6)")  ub_matrix(i,:,j)
                 end do
               end do
             end if
          end if

        end if

        if(ub_read .and. wave_read) then
            call Update_Current_Instrm_UB(trim(file_inst),UB,wave)
            if(Err_CFML%Ierr /= 0) then
               mess="Error updating the instrument with the given UB-matrix in "//trim(file_inst)
               return
            end if
        end if
        !Check that the UB-matrices are consistent with the cell parameters
        call set_eps_Math(1.0e-12)
        do i=1,n_twins
          call cell_fr_UB(ub_matrix(:,:,i),ipr,dcel)
          if(Err_CFML%Ierr /= 0) then
             mess="Error calculating the cell parameters from UB-matrix in "//trim(file_inst)
             return
          end if
          if(sum(abs(dcel-incel)) > 1.0) then
              mess="Incompatible UB-matrix and cell parameters in file "//trim(file_inst)
              write(unit=*,fmt="(a,i3)") trim(mess//"  for twin:"),i
              write(unit=*,fmt="(a,6f11.4)") "  Input cell: ",incel
              write(unit=*,fmt="(a,6f11.4)") "  UB    cell: ",dcel
              return
          end if
        end do
        call set_eps_Math()
        if(Err_CFML%Ierr == 0) ok=.true.
    End Subroutine read_sxtal_geom

    Subroutine check_limits(ang,ok_ang,cur_chi)
        real, dimension(:),    intent (in) :: ang
        real,                  intent (in) :: cur_chi
        logical, dimension(:), intent(out) :: ok_ang
        integer :: i

        ok_ang=.true.
        do i=1,min(Current_Instrm%num_ang,size(ang))
            if(.not. (ang(i) >=  Current_Instrm%ang_Limits(i,1) .and.  ang(i) <=  Current_Instrm%ang_Limits(i,2)) ) ok_ang(i) =.false.
        end do
        if(scann .and. index(l_case(Current_Instrm%name_inst), "d19") /= 0 ) Then
            if(cur_chi < 130.0) then
              if(.not. (ang(2) >=  -25.0 .and.  ang(2) <=  39.0 ) ) ok_ang(2) =.false.
            end if
        end if
    End subroutine check_limits

    Subroutine calc_angles(geom,sig,UBM,hr,ang,comment,cur_chi)
        integer,              intent (in)    :: geom
        integer,              intent (in out):: sig
        real, dimension(3,3), intent (in)    :: UBM
        real, dimension(3),   intent (in)    :: hr
        real, dimension(:),   intent (out)   :: ang
        character(len=*),     intent (out)   :: comment
        real,                 intent (in)    :: cur_chi
        !---- Local variables ----!
        real                  :: tteta,om,ch,ph
        integer               :: ierr
        logical, dimension(4) :: ok_ang

        comment=" "

        Select Case (geom)

            Case(1,2,4)
                call calang(hr,tteta,om,ch,ph,UBM=ubm)
                if(Err_CFMl%ierr /= 0) comment="Outside 2Theta limits"
                ang= [tteta,om,ch,ph]
                call check_limits(ang,ok_ang,cur_chi)
                if(.not. ok_ang(2))    comment="Outside Omega limits"
                if(.not. ok_ang(3))    comment="Outside Chi limits"
                if(.not. ok_ang(4))    comment="Outside Phi limits"

            Case(3,-3)
                if(geom == 3) then
                    call Normal_Beam_angles(Current_Orient%wave,UBm,hr,sig,ang,ierr)
                else
                    call Normal_Beam_angles(Current_Orient%wave,UBm,hr,sig,ang,ierr,nusign=-1)
                end if
                if(ierr /= 0) then
                    if(ierr == 1) comment="Outside resolution sphere"
                    if(ierr == 2) comment="Blind zone for Nu"
                    if(ierr == 3) comment="Blind zone for Gamma"
                else
                    call check_limits(ang,ok_ang,cur_chi)
                    if(.not. ok_ang(1)) comment="Outside Gamma limits"
                    if(.not. ok_ang(2)) comment="Outside Omega limits"
                    if(.not. ok_ang(3)) comment="Outside Nu limits"
                    if(.not. ok_ang(4)) comment="Outside 2Theta limits"
                end if

        End Select

        return

    End subroutine calc_angles

End Module Ref_Gen

Program Sxtal_Ref_Gen

    use CFML_GlobalDeps
    use CFML_Maths,             only: sort,Linear_interpol,cross_product
    use CFML_Strings,           only: file_List_type,cut_string,u_case,pack_string
    use CFML_gSpaceGroups,      only: SPG_type, Write_SpaceGroup_info, Set_SpaceGroup
    use CFML_Atoms,             only: AtList_Type, Write_Atom_List,MAtom_list_Type
    use CFML_metrics,           only: Cell_G_Type, Write_Crystal_Cell
    use CFML_Reflections,       only: RefList_Type, Hkl_Gen_Sxtal, get_maxnumref, H_Equiv_List, &
                                      Gener_Reflections,SRefl_Type,Refl_Type,MRefl_Type
    use CFML_IOForm,            only: Read_Xtal_Structure
    use CFML_Structure_Factors, only: Structure_Factors, Write_Structure_Factors, &
                                      Init_Structure_Factors,Calc_General_StrFactor,&
                                      Scattering_Species_Type, Allocate_Scattering_Species, &
                                      Additional_Scattering_Factors, Set_Form_Factors
    use CFML_ILL_Instrm_data,   only: SXTAL_Orient_type, Current_Orient, diffractometer_type, &
                                      Current_Instrm, Write_Current_Instrm_data
    use CFML_SXTAL_Geom
    use CFML_kvec_Symmetry
    use CFML_kvec_Structure_Factors

    use Ref_Gen

    implicit none

    type (file_List_type)          :: fich_cfl
    class(SPG_type),    allocatable:: SpG
    class(SPG_type),    allocatable:: SpGT
    type (Atlist_Type)             :: A
    class(Cell_G_Type), allocatable:: Cell
    type (RefList_Type)            :: hkl
    type (MagSymm_k_Type)          :: MGp
    type (MAtom_list_Type)         :: Am
    type (MagH_List_Type)          :: Mhkl

    character(len=256)                  :: filcod,cmdline,cputim_string     !Name of the input file and command line
    character(len=256)                  :: mess       !Message after reading the CFL file for ref_gen
    character(len=15)                   :: sinthlamb  !String with stlmax (2nd cmdline argument)
    character(len=30)                   :: comment
    character(len=1)                    :: keyv
    real                                :: stlmin,stlmax     !Minimum and Maximum Sin(Theta)/Lambda
    real                                :: tteta, ss, dspc, SqMiV
    real,    dimension(3)               :: hr, vk, R_latt !, z1
    real                                :: sn,s2,theta,flip_right,flip_left,up,down,Nuc,time_scan
    real                                :: start, tend, angle_road, duration,total_time,current_d19_chi,Frac_max,Frac_res
    integer, dimension(3)               :: h, ord
    integer, dimension(3,2)             :: hlim
    integer, dimension(3,48)            :: hlist
    real, dimension(50)                 :: time_options=0.0
    character(len=40),dimension(50)     :: time_conf=" "
    real, dimension(4)                  :: ang
    real, dimension(:,:),allocatable    :: angles,mangles,chiphi_vals
    real, dimension(:,:),allocatable    :: reflx
    integer, dimension(:),  allocatable :: ind,itwin
    real,    dimension(:),  allocatable :: fst
    integer                             :: MaxNumRef, lun=1, ier,i,j,k,i_hkl=2, n,nt,nmt,n3, iop, nlong, nopt !, Num, ierr
    integer                             :: narg, mul, sig, n_ini, n_end, nm, nv, maxref, n_pass, m_pass !, mu
    logical                             :: esta, arggiven=.false.,sthlgiven=.false., ok=.false., schwinger=.false.,&
                                           spher=.false.,lim, iop_given=.false., mag_structure=.false. , wait_end=.true.
    complex                             :: fn,fx,fe,fsru,fsrd,fslu,fsld
    Type(Scattering_Species_Type)       :: Scattf, add_Scat
    real(kind=dp), parameter            :: schw= -0.00014699
    integer, dimension(:),allocatable   :: measured,mag_measured
    integer                             :: nr_resol,nr_max, mult


    !---- Arguments on the command line ----!
    narg=COMMAND_ARGUMENT_COUNT()
    call Get_Command(Command=Cmdline,Length=nlong)
    call cut_string(cmdline,nlong) !Eliminate the name of the program
    cmdline=u_case(trim(adjustl(cmdline))) !Capitalize the keywords
    if(index(cmdline,"NOWAIT") /= 0) wait_end=.false.
    stlmin=0.0

    if(narg > 0) then
        call GET_COMMAND_ARGUMENT(1,filcod)
        arggiven=.true.
        i=index(filcod,'.cfl',back=.true.)
        if( i /= 0) filcod=filcod(1:i-1)
    end if

    if(narg > 1) then
        call GET_COMMAND_ARGUMENT(2,sinthlamb)
        read(unit=sinthlamb,fmt=*,iostat=ier) stlmax
        if(ier == 0) sthlgiven=.true.
    end if

    if(narg > 2) then
        call GET_COMMAND_ARGUMENT(3,sinthlamb)
        read(unit=sinthlamb,fmt=*,iostat=ier) stlmin
        if(ier /= 0) stlmin=0.0
        if(stlmin > stlmax) then
            tteta=stlmin
            stlmin=stlmax
            stlmax=tteta
        end if
    end if

    if(narg > 3) then
        call GET_COMMAND_ARGUMENT(4,sinthlamb)
        read(unit=sinthlamb,fmt=*,iostat=ier) iop
        if(ier /= 0) iop=0
        iop_given=.true.
    end if

    write(unit=*,fmt="(/,/,7(a,/))")                                                   &
          "                      ------ PROGRAM SXTAL_REFGEN ------"                 , &
          "                      ---- Version  0.7 March-2024  ----"                 , &
          "    *******************************************************************"  , &
          "    * Generates single crystal reflections and nuc. structure factors *"  , &
          "    * (if atoms are given ) reading a *.CFL file (4C & NB geometries) *"  , &
          "    *******************************************************************"  , &
          "                     (JRC- April-2007, updated March-2024)"
    write(unit=*,fmt=*) " "

    if(.not. arggiven) then
        write(unit=*,fmt="(a)",advance="no") " => Code of the file xx.cfl (give xx): "
        read(unit=*,fmt="(a)") filcod
        if(len_trim(filcod) == 0) call finish()
    end if

    open(unit=lun,file=trim(filcod)//".sfa", status="replace",action="write")

    write(unit=lun,fmt="(/,/,7(a,/))")                                                 &
          "                      ------ PROGRAM SXTAL_REFGEN ------"                 , &
          "                      ---- Version  0.7 March-2024  ----"                 , &
          "    *******************************************************************"  , &
          "    * Generates single crystal reflections and nuc. structure factors *"  , &
          "    * (if atoms are given ) reading a *.CFL file (4C & NB geometries) *"  , &
          "    *******************************************************************"  , &
          "                  (JRC- April-2007, updated March-2024)"

    cfl_file=trim(filcod)//".cfl"
    inquire(file=trim(cfl_file),exist=esta)
    if( .not. esta) then
        write(unit=*,fmt="(a)") " File: "//trim(cfl_file)//" doesn't exist!"
        call finish()
    end if

    call Read_Xtal_Structure(trim(cfl_file),Cell,SpG,A,FileList=fich_cfl)

    call cpu_time(start)

    If(err_CFML%Ierr /= 0) then

        write(unit=*,fmt="(a)") "  "//trim(err_CFML%Msg)

    else

        call Write_Crystal_Cell(Cell,lun)
        call Write_SpaceGroup_Info(SpG,lun)
        if(A%natoms > 0) call Write_Atom_List(A,Iunit=lun)


        n_ini=1
        n_end=fich_cfl%nlines
        call Readn_Set_Magnetic_Kv_Structure(fich_cfl,n_ini,n_end,MGp,Am)
        if(err_CFML%Ierr /= 0 .or. err_CFML%flag) then
            write(unit=*,fmt="(a)") " =>"//err_CFML%Msg
            write(unit=lun,fmt="(/a/)") " =>"//err_CFML%Msg
            mag_structure=.false.
        else
            mag_structure=.true.
            call Write_Magnetic_Structure(lun,MGp,Am)
        end if

        !re-read the input file searching for INSTRM, UBM, GEOM, WAVE, ORDER, SPHER items
        if(.not. sthlgiven) then
            call read_sxtal_geom(lun,cell,fich_cfl,ok,mess,ord,spher,schwinger,hlim, &
                                 sgiven=sthlgiven,smin=stlmin,smax=stlmax,opgiven=iop_given,iop_ord=iop)
        else
            if(.not. iop_given) then
                call read_sxtal_geom(lun,cell,fich_cfl,ok,mess,ord,spher,schwinger,hlim,  &
                                     opgiven=iop_given,iop_ord=iop)
            else
                call read_sxtal_geom(lun,cell,fich_cfl,ok,mess,ord,spher,schwinger,hlim)
            end if
        end if

        if(.not. ok) then
            write(unit=*,fmt="(a)") "Error in read_sxtal_geom -> Mess: "//trim(mess)
            if(Err_CFML%Flag) write(unit=*,fmt="(a)") " ILL_data: "//trim(Err_CFML%Msg)
            call finish()
        end if

        if(.not. sthlgiven) then
            write(unit=*,fmt="(a)",advance="no") " => Minimum and Maximum sinTheta/Lambda: "
            read(unit=*,fmt=*) stlmin,stlmax
        end if

        !Check that stlmax is compatible with the given wavelengh
        if(stlmax > 1.0/Current_Orient%wave) Then
          stlmax = 1.0/Current_Orient%wave
          write(unit=*,fmt="(a,f8.4)") " => WARNING: the provided SinTheta/Lambda exceed resolution sphere, changed to: ",stlmax
        end if

        !Check that stlmax is compatible with the given angular limites of the instrument
        Select Case (Current_Instrm%igeom)
            Case(1,2,4)
               tteta=sind(0.5*Current_Instrm%ang_Limits(1,2))/Current_Orient%wave !sinTheta/Lambda (max)
            Case(3,-3)
               tteta=cosd(Current_Instrm%ang_Limits(1,2))*cosd(Current_Instrm%ang_Limits(3,2)) !cos(gamma)*cos(nu)=cos(2theta)
               tteta=sin(0.5*acos(tteta))/Current_Orient%wave !sinTheta/Lambda (max)
        End Select

        if(stlmax > tteta) Then
          stlmax = tteta
          write(unit=*,fmt="(a,f8.4)") " => WARNING: the provided SinTheta/Lambda exceed the instrumental maximum, changed to: ",stlmax
        end if
        !store in tteta -> 2sinTheta/Lambda (max)= r*(max)
        tteta=2.0/Current_Orient%wave
        MaxNumRef = get_maxnumref(stlmax,Cell%Vol,multip=SpG%Multip)
        if(spher) MaxNumRef=MaxNumRef*Spg%NumOps*max(Spg%Centred,1)

        nr_max=nint(4.0/3.0 * pi * (2.0/Current_Orient%wave)**3 * Cell%Vol)
        if(.not. spher) nr_max=nr_max/Spg%NumOps/max(Spg%Centred,1)

        !write(unit=*,fmt="(a,i6,a)") " Generate at least ",MaxNumRef," reflections"
        lim=.false.
        if(sum(abs(hlim)) /= 0) lim=.true.

        if(spher) then
            if(lim) then
                call Hkl_gen_sxtal(cell,SpG,stlmin,stlmax,hkl,ord,hlim)
            else
                call Hkl_gen_sxtal(cell,SpG,stlmin,stlmax,hkl,ord)
            end if
        else
            !call Gener_Reflections(Cell,Slmin,Slmax,Reflex,SpG,IPhase,MagExt,kinfo,Order,Unique,seqindx,hlim,mag_only,Friedel,Ref_typ,kout)
            call Gener_Reflections(Cell,stlmin,stlmax,hkl,SpG,1,.true.,Ref_typ="SRefl",hlim=hlim)
        end if
        nr_resol=hkl%nref
        !Calculation of Structure factors for neutron scattering
        if(A%natoms /= 0) then
            call Init_Structure_Factors(hkl,A,Spg,mode="NUC",lun=lun)
            call Structure_Factors(hkl,A,SpG,mode="NUC")
        end if

        !Allocation of memory and initialization of arrays
        if(spher) then
            n=hkl%nref*n_twins
        else
           n=hkl%nref*Spg%NumOps*max(Spg%Centred,1)*n_twins
        end if
        maxref=2*n
        write(*,*) " => Maximum extend for angles, twins, etc.:",maxref

        !Allocating logical indicating that a reflection has been recorded
        allocate(measured(maxref))   !
        measured=0

        if(allocated(angles)) deallocate (angles)
        allocate(angles(4,n))
        angles=0.0

        if(allocated(mangles)) deallocate (mangles)
        allocate(mangles(4,n))
        mangles=0.0

        if(allocated(reflx)) deallocate (reflx)
        allocate(reflx(3,n))
        reflx=0.0

        if(allocated(fst)) deallocate (fst)
        allocate(fst(n))
        fst=0.0

        if(allocated(itwin)) deallocate(itwin)
        allocate(itwin(n))
        itwin=1

        if(allocated(ind)) deallocate(ind)
        allocate(ind(n))

        ind=[(i,i=1,n)]

        sig=1
        if(Current_instrm%BL_frame == "z-down") sig=-1

        call Write_Current_Instrm_data(lun)

        if(.not. iop_given .and. .not. optimize) then
            write(unit=*,fmt="(a)") " => Order of reflections: "
            write(unit=*,fmt="(a)") "              0: As set on CFL file (hkl-running) "
            write(unit=*,fmt="(a)") "              1: Ascending order on 2Theta "
            write(unit=*,fmt="(a)") "              2: Ascending order on Omega "
            write(unit=*,fmt="(a)") "              3: Ascending order on Chi/Nu "
            write(unit=*,fmt="(a)") "              4: Ascending order on Phi "
            write(unit=*,fmt="(a)") "              5: Ascending order on 2Theta/Gamma + Block order on Phi "
            write(unit=*,fmt="(a)") "              6: Ascending order on 2Theta/Gamma + Block order on Chi "
            write(unit=*,fmt="(a)") "              7: Ascending order on 2Theta/Gamma + Block order on Omega "
            write(unit=*,fmt="(a)",advance="no") " => Enter option: "
            read(unit=*,fmt=*,iostat=ier) iop
            if(iop > 4) then
              write(unit=*,fmt="(a)",advance="no") " => Enter the Delta(2Theta/Gamma) value for block ordering: "
              read(unit=*,fmt=*) delta_ang
            end if
            if(ier /= 0) iop=0
        end if

        if(scan_chi .or. scan_phi .and. abs(Current_Instrm%igeom) == 3) then
          m_pass=max(n_chi,1) * max(n_phi,1)
          allocate(chiphi_vals(2,m_pass))
          allocate(hkl_file(m_pass))
          n_pass=0
          do_inf:do
            do i=1,n_chi
              do j=1,n_phi
                n_pass=n_pass+1
                if(n_pass > m_pass) exit do_inf
                chiphi_vals(1,n_pass)=phi_val(j)
                chiphi_vals(2,n_pass)=chi_val(i)
                write(unit=hkl_file(n_pass),fmt="(2(a,i7))") trim(filcod)//"_phi_",nint(phi_val(j)*100.0),"_chi_",nint(chi_val(i)*100.0)
                hkl_file(n_pass)=pack_string(hkl_file(n_pass))
              end do
            end do
          end do do_inf
          n_pass=0
          scann=.true.
        end if

        Chi_matx=identity
        Phi_matx=identity
        nt=0; nmt=0; total_time=0.0; time_scan=0.0
        do_ext: do   !External do use for different omega scans with variable chi Phi
          n=0
          if(scann .and. n_pass > 0) then
            write(unit=lun,fmt="(/,a,2f8.5,2(a,f8.3)/)")" => Reflection generation between sinTheta/Lambda min and max: ", &
                                                 stlmin,stlmax, " angstroms^-1, for setting:  Phi=",chiphi_vals(1,n_pass), &
                                                 "  and Chi=",chiphi_vals(2,n_pass)
            current_d19_chi=chiphi_vals(2,n_pass)
          else
            write(unit=lun,fmt="(/,a,2f8.5,a/)")" => Reflection generation between sinTheta/Lambda min and max: ", &
                                                     stlmin,stlmax, " angstroms^-1"
          end if
          if(Current_instrm%igeom == 3 .or. Current_instrm%igeom == -3) then
              write(unit=lun,fmt="(/,/,a)")"-------------------------------------------------------------------------------------"
              write(unit=lun,fmt="(a)")    "   h   k   l       F2        Re(F)       Im(F)       Gamma     Omega     Nu      TWIN"
              write(unit=lun,fmt="(a)")    "-------------------------------------------------------------------------------------"
          else
              write(unit=lun,fmt="(/,/,a)")"-----------------------------------------------------------------------------------------------"
              write(unit=lun,fmt="(    a)")"   h   k   l       F2        Re(F)       Im(F)      2Theta     Omega     Chi       Phi     TWIN"
              write(unit=lun,fmt="(    a)")"-----------------------------------------------------------------------------------------------"
          end if

          if(spher) then

              do i=1,hkl%nref
                hr=hkl%Ref(i)%h
                do j=1,n_twins
                  R_UB= matmul(Chi_matx,matmul(Phi_matx,ub_matrix(:,:,j)))
                  call calc_angles(Current_Instrm%igeom,sig,R_UB,hr,ang,comment,current_d19_chi)

                  Select Type(r => hkl%ref)
                    Type is (sRefl_Type)
                      Select Case (Current_Instrm%igeom)
                          Case(1,2,4)
                              write(unit=lun,fmt="(3i4,3f12.5,4f10.3,i5,tr2,a)") r(i)%h,r(i)%Fc(2),r(i)%A(2),r(i)%B(2),&
                                                                                 ang,j,trim(comment)
                          Case(3,-3)
                              write(unit=lun,fmt="(3i4,3f12.5,3f10.3,i5,tr2,a)") r(i)%h,r(i)%Fc(2),r(i)%A(2),r(i)%B(2),&
                                                                                 ang(1:3),j,trim(comment)
                      End Select
                  End Select

                  if(len_trim(comment) == 0) then
                      n=n+1
                      itwin(n)     = j
                      angles(:,n)  = ang(:)
                      Select Type(r => hkl%ref)
                        Type is (sRefl_Type)
                          reflx(:,n)   = r(i)%h
                          fst(n)       = r(i)%Fc(2)
                      End Select
                      measured(i)  = measured(i)+1
                  end if
                end do
              end do
              if(n == 0) then
                 write(*,"(a)") " => None of the generated reflections can be measured with the provided conditions"
                 write(*,"(a)") "    Look at the *.sfa file to see the comments ...."
                 call finish()
              end if
          else

              do i=1,hkl%nref
                 h=hkl%Ref(i)%h
                 call H_Equiv_List(h,SpG,.true.,Mul,Hlist)
                 do_mult:do j=1,mul
                    hr=Hlist(:,j)
                    if(strict) then
                       if(hr(1) < hlim(1,1))  cycle
                       if(hr(1) > hlim(1,2))  cycle
                       if(hr(2) < hlim(2,1))  cycle
                       if(hr(2) > hlim(2,2))  cycle
                       if(hr(3) < hlim(3,1))  cycle
                       if(hr(3) > hlim(3,2))  cycle
                    end if
                    !z1=matmul(Current_Orient%UB,hr)
                    do k=1,n_twins
                      R_UB= matmul(Chi_matx,matmul(Phi_matx,ub_matrix(:,:,k)))
                      call calc_angles(Current_Instrm%igeom,sig,R_UB,hr,ang,comment,current_d19_chi)
                      Select Type( r => hkl%Ref)
                        Type is(SRefl_type)
                          Select Case (Current_Instrm%igeom)
                              Case(1,2,4)
                                  write(unit=lun,fmt="(3i4,3f12.5,4f10.3,i5,tr2,a)") hlist(:,j),r(i)%Fc(2),r(i)%A(2),r(i)%B(2),&
                                                                                  ang,k,trim(comment)
                              Case(3,-3)
                                  write(unit=lun,fmt="(3i4,3f12.5,3f10.3,i5,tr2,a)") r(i)%h,r(i)%Fc(2),r(i)%A(2),r(i)%B(2),&
                                                                                  ang(1:3),k,trim(comment)
                          End Select
                      End Select

                      if(len_trim(comment) == 0) then
                          n=n+1
                          itwin(n)     = k
                          angles(:,n)  = ang
                          reflx(:,n)   = hlist(:,j)
                          Select Type (r => hkl%ref)
                            Type is (SRefl_Type)
                              fst(n)   = r(i)%Fc(2)
                          End Select
                          measured(i)  = measured(i)+1
                          if(unique) exit do_mult
                      end if
                    end do
                 end do do_mult

              end do
          end if

          !Try optimization if needed
          if(optimize) then
            !Try first the six possible hkl-ordering  123 132 231 213 312 321
            !Then sort single angles (max. 4 options) 2theta, omega,chi, phi  or gamma,omega,nu
            !Then combined with blocks  gamma + phi (blocks 5, 10, 15, 20),  gamma + chi/nu (blocks 5, 10, 15, 20), gamma + omega (blocks 5, 10, 15, 20)
            time_options=0.000
            ! First option hkl-ord = 1,2,3
            angle_road=0.0
            do j=1,n-1
               call get_duration(abs(angles(:,j+1)-angles(:,j)),duration)
               angle_road=angle_road + duration
            end do
            nopt=1
            time_options(nopt)=angle_road
            !write(time_conf(nopt),"(3i3)") ord
            n3=4
            if(abs(Current_Instrm%igeom) == 3) n3=3
            mangles=angles !Save Initial ordering according to hkl-generation
            do k=1,n3
              ind=0
              ind(1:n)= sort(angles(k,:),n)
              angles(:,ind(1:n))=angles(:,ind(1:n)) !ordered
              angle_road=0.0
              do j=1,n-1
                 call get_duration(abs(angles(:,j+1)-angles(:,j)),duration)
                 angle_road=angle_road + duration
              end do
              nopt=k+1
              time_options(nopt)=angle_road
              time_conf(nopt)=Current_Instrm%ang_names(k)
              angles=mangles
            end do

            do i=1,3 !Testing coupled orderings
              do k=5,30,5  !block sizes
                call block_order(i+4,k)  !Modify only the ordering of angles
                angle_road=0.0
                do j=1,n-1
                   call get_duration(abs(angles(:,j+1)-angles(:,j)),duration)
                   angle_road=angle_road + duration
                end do
                angles=mangles
                nopt=nopt+1
                time_options(nopt)=angle_road
                write(time_conf(nopt),"(a,i2,a)") trim(Current_Instrm%ang_names(1))//" + Block ",k," degrees "//trim(Current_Instrm%ang_names(5-i))
                write(*,"(3i4,a)") i,k,nopt,"  "//trim(time_conf(nopt))
              end do
            end do

            do i=1,nopt
              call get_hms(time_options(i),cputim_string)
              write(*,"(i5,tr5,a,t45,a)") i, trim(time_conf(i)),cputim_string
            end do
            stop
          end if

          !write final output file with a selected ordering according to a motor
          if(iop /= 0) then
              k=iop
              if(iop > 4) k=1
              if( n > 0) then
                 ind(1:n)=sort(angles(k,:),n)
                 !Reordering the arrays
                 reflx(:,1:n) = reflx(:,ind(1:n))
                 angles(:,1:n)=angles(:,ind(1:n))
                 fst(1:n)=fst(ind(1:n))
                 itwin(1:n)=itwin(ind(1:n))
              else
                 write(*,"(a)") " => None of the generated reflections can be measured with the provided conditions"
                 write(*,"(a)") "    Look at the *.sfa file to see the comments ...."
                 call finish()
              end if
          end if

          if(iop > 4) then
             call block_order()
          end if
          if(scann .and. n_pass > 0) then
            open(unit=i_hkl, file=trim(hkl_file(n_pass))//".hkl", status="replace",action="write")
          else
            open(unit=i_hkl, file=trim(filcod)//".hkl", status="replace",action="write")
          end if

          Select Case (Current_Instrm%igeom)
            Case(1,2,4)
              write(unit=i_hkl,fmt="(a)") "   h   k   l   SFactor^2     Gamma     Omega      Chi      Phi   Twin_number"
              angle_road=0.0
              do j=1,n-1
                 call get_duration(abs(angles(:,j+1)-angles(:,j)),duration)
                 angle_road=angle_road + duration
                 write(unit=i_hkl,fmt="(3i4,f12.5,4f10.3,a,i4)") nint(reflx(:,j)),fst(j), angles(:,j),"   !domain:",itwin(j)
              end do
              write(unit=i_hkl,fmt="(3i4,f12.5,4f10.3,a,i4)") nint(reflx(:,n)),fst(n), angles(:,n),"   !domain:",itwin(n)
            Case(3,-3)
              write(unit=i_hkl,fmt="(a)") "   h   k   l   SFactor^2     Gamma     Omega      Nu     Twin_number"
              angle_road=0.0
              do j=1,n-1
                 call get_duration(abs(angles(1:3,j+1)-angles(1:3,j)),duration)
                 angle_road=angle_road + duration
                 write(unit=i_hkl,fmt="(3i4,f12.5,3f10.3,a,i4)") nint(reflx(:,j)),fst(j), angles(1:3,j),"   !domain:",itwin(j)
              end do
              write(unit=i_hkl,fmt="(3i4,f12.5,3f10.3,a,i4)") nint(reflx(:,n)),fst(n), angles(1:3,n),"   !domain:",itwin(n)
          End Select
          call get_hms(angle_road,cputim_string)
          if(n_frames /= 0) time_scan=time_scan+n_frames*sec_frame
          total_time=total_time+angle_road
          write(unit=lun,  fmt="(a)") " => Total time spent in motor movements: "//trim(cputim_string)
          write(unit=i_hkl,fmt="(a)") " => Total time spent in motor movements: "//trim(cputim_string)
          write(unit=*,    fmt="(a)") " => Total time spent in motor movements: "//trim(cputim_string)
          write(unit=*,fmt="(a,i5)")  "    Number of accessible nuclear reflections: ",n
          close(unit=i_hkl)
          nt=nt+n

          ! Generation of magnetic satellites if needed
          nm=0
          if(mag_structure) then
              !First generate the full set of reflections according to the lattice of the
              !space group.
              comment=SpG%SPG_lat//" -1"
              call Set_SpaceGroup(comment,SpGT)
              if(lim) then
                  call Hkl_gen_sxtal(cell,SpGT,stlmin,stlmax,hkl,ord,hlim)
              else
                  call Hkl_gen_sxtal(cell,SpGT,stlmin,stlmax,hkl,ord)
              end if

              if(scann .and. n_pass > 0) then
                open(unit=i_hkl, file=trim(hkl_file(n_pass))//".mhkl", status="replace",action="write")
              else
                open(unit=i_hkl, file=trim(filcod)//".mhkl", status="replace",action="write")
              end if

              call Gen_Satellites(Cell,MGp,stlmax,Mhkl,hkl=hkl)  !Mhkl magnetic satellites
              write(unit=lun,fmt="(/,/,a)") " "

              if(Am%natoms > 0) then
                  call Init_Mag_Structure_Factors(Mhkl,Am,MGp,lun)
                  if(err_CFML%Ierr /= 0) then
                      write(unit=*,fmt="(a)")  "  "//err_CFML%Msg
                      call finish()
                  end if
                  call Mag_Structure_Factors(Cell,Am,MGp,Mhkl)
                  call Calc_Mag_Interaction_Vector(Mhkl,Cell)  !in {e1,e2,e3} basis (complete Mhkl)
              end if

              nm=Mhkl%nref
              if(allocated(angles)) deallocate (angles)
              allocate(angles(4,nm))
              angles=0.0
              if(allocated(reflx)) deallocate (reflx)
              allocate(reflx(3,nm))
              reflx=0.0
              if(allocated(fst)) deallocate (fst)
              allocate(fst(nm))
              fst=0.0
              if(allocated(ind)) deallocate(ind)
              allocate(ind(nm))

              ind=(/(i,i=1,nm)/)

              write(unit=lun,fmt="(/,a)")   "    LIST OF REFLECTIONS AND MAGNETIC STRUCTURE FACTORS"
              write(unit=lun,fmt="(a,/)")   "    =================================================="
              write(unit=lun,fmt="(a,i9)") " => Total number of reflections  : ",Mhkl%Nref
              write(unit=lun,fmt="(a,i9)") " => Number of propagation vectors: ",MGp%nkv
              do i=1,MGp%nkv
                  write(unit=lun,fmt="(a,i2,a,3f8.4,a)") " => Propagation vectors #",i," = (",MGp%kvec(:,i)," )"
              end do

              if(Current_instrm%igeom == 3 .or. Current_instrm%igeom == -3) then
                  write(unit=lun,fmt="(/,/,a)")"-------------------------------------------------------------------------"// &
                                               "-------------------------------------------------------------------------"//&
                                               "-------------------------------------------------------------------------"
                  write(unit=lun,fmt="(a)") &
                  "    Hr      Kr      Lr       H   K   L   nvk   Mult   dspc      |MiV|^2     Mrx      Mry      Mrz      "// &
                  "Mix      Miy      Miz     MiVrx    MiVry    MiVrz    MiVix    MiViy    MiViz        Gamma         Omega          Nu  "

                  write(unit=lun,fmt="(a)")"-------------------------------------------------------------------------"// &
                                           "-------------------------------------------------------------------------"//&
                                           "-------------------------------------------------------------------------"
              else
                  write(unit=lun,fmt="(/,/,a)")"-------------------------------------------------------------------------"// &
                                               "-------------------------------------------------------------------------"//&
                                               "--------------------------------------------------------------------------------------"
                  write(unit=lun,fmt="(a)") &
                  "    Hr      Kr      Lr       H   K   L   nvk   Mult   dspc      |MiV|^2     Mrx      Mry      Mrz      "// &
                  "Mix      Miy      Miz     MiVrx    MiVry    MiVrz    MiVix    MiViy    MiViz    2Theta        Omega          Chi           Phi"
                  write(unit=lun,fmt="(    a)")"-------------------------------------------------------------------------"// &
                                               "-------------------------------------------------------------------------"//&
                                               "--------------------------------------------------------------------------------------"
              end if


              nm=0
              if(allocated(mag_measured)) deallocate(mag_measured)
              allocate(mag_measured(Mhkl%nref))
              mag_measured=0
              do i=1,Mhkl%nref
                  hr=Mhkl%Mh(i)%h
                  ss=Mhkl%Mh(i)%signp
                  mul=Mhkl%Mh(i)%mult
                  nv =Mhkl%Mh(i)%Num_k
                  vk=MGp%kvec(:,nv)
                  h=nint(hr+sig*vk)
                  sqMiV= Mhkl%Mh(i)%sqMiV
                  dspc=0.5/Mhkl%Mh(i)%S
                  call calc_angles(Current_Instrm%igeom,sig,ub_matrix(:,:,1),hr,ang,comment,current_d19_chi)

                  Select Case (Current_Instrm%igeom)
                      Case(1,2,4)
                          write(unit=lun,fmt="(3f8.3,tr2,3i4,i5,i6,f9.4,f13.5,12f9.4,4g14.6,tr2,a)") hr,h, -sig*nv, mul, dspc,sqMiV, &
                          real(Mhkl%Mh(i)%MsF),aimag(Mhkl%Mh(i)%MsF), real(Mhkl%Mh(i)%MiV),aimag(Mhkl%Mh(i)%MiV), &
                          ang,trim(comment)
                      Case(3,-3)
                          write(unit=lun,fmt="(3f8.3,tr2,3i4,i5,i6,f9.4,f13.5,12f9.4,3g14.6,tr2,a)") hr,h, -sig*nv, mul, dspc,sqMiV, &
                          real(Mhkl%Mh(i)%MsF),aimag(Mhkl%Mh(i)%MsF), real(Mhkl%Mh(i)%MiV),aimag(Mhkl%Mh(i)%MiV), &
                          ang(1:3),trim(comment)
                  End Select

                  if(len_trim(comment) == 0) then
                      nm=nm+1
                      if(nm > Mhkl%nref) exit
                     ! write(*,*) nm
                      angles(:,nm)  = ang
                      reflx(:,nm)   = hr
                      fst(nm)       = Mhkl%Mh(i)%sqMiV
                      mag_measured(i)=mag_measured(i)+1
                  end if
              end do
              !write final output file with a selected ordering according to a motor
              if(iop /= 0) then
                  ind(1:nm)= sort(angles(iop,1:nm),nm)
              end if
              reflx(:,1:nm) = reflx(:,ind(1:nm))
              angles(:,1:nm)=angles(:,ind(1:nm))
              fst(1:nm)=fst(ind(1:nm))
              angle_road=0.0
              do i=1,nm-1
                  call get_duration(abs(angles(:,i+1)-angles(:,i)),duration)
                  angle_road=angle_road+ duration
                  write(unit=i_hkl,fmt="(3f9.4,f12.5,4f10.3)") reflx(:,i),fst(i), angles(:,i)
              end do
              write(unit=i_hkl,fmt="(3f9.4,f12.5,4f10.3)") reflx(:,nm),fst(nm), angles(:,nm)
              if(n_frames /= 0) time_scan=time_scan+n_frames*sec_frame
              total_time=total_time+angle_road
              call get_hms(angle_road,cputim_string)
              write(unit=lun,  fmt="(a)") " => Total time spent in motor movements(mag.refls): "//trim(cputim_string)
              write(unit=i_hkl,fmt="(a)") " => Total time spent in motor movements(mag.refls): "//trim(cputim_string)
              write(unit=*,    fmt="(a)") " => Total time spent in motor movements(mag.refls): "//trim(cputim_string)
              write(unit=*,fmt="(a,i5)")  "    Number of accessible magnetic reflections: ",nm
              close(unit=i_hkl)
          end if
          nmt=nmt+nm
          if(.not. scann) exit
          n_pass=n_pass+1
          if(n_pass > m_pass ) exit do_ext
          !Change now the rotation matrix
          Chi_matx=Chi_mat(ChiPhi_vals(2,n_pass))
          Phi_matx=Phi_mat(ChiPhi_vals(1,n_pass))
        end do do_ext


        if(Schwinger .and. A%natoms /= 0) then

            call Additional_Scattering_Factors(fich_cfl,add_Scat)
            if(Err_CFML%Ierr /= 0) then
              write(unit=*,fmt="(a)") " => "//trim(Err_CFML%Msg)
              call finish()
            end if
            !  Set nuclear, x-ray, neutron and magnetic form factors coefficients for all the atoms
            !  in the structure
            if(add_Scat%Num_Species > 0) then
              call Set_Form_Factors(A,Scattf,Current_Orient%wave,add_Scat,lun=lun)
            else
              call Set_Form_Factors(A,Scattf,Current_Orient%wave,lun=lun)
            end if
            if(Err_CFML%Ierr /= 0 .or. Err_CFML%flag) then
              write(unit=*,fmt="(a)") " => "//trim(Err_CFML%Msg)
              call finish()
            end if
           write(unit=lun,fmt="(/,a)") &
           "   H   K   L   sinT/L  Intensity     FNr       FNi           FXr       FXi           FEr       FEi      Flip_left  Flip_right"// &
                                    "  SRUr      SRUi          SRDr      SRDi          SLUr      SLUi          SLDr      SLDi"
           if(allocated(angles)) deallocate (angles)
           allocate(angles(6,maxref))
           angles=0.0

           n=0
           ind=[(i,i=1,hkl%NRef)]
           do i=1,hkl%NRef
              hr=real(hkl%ref(i)%h)
              sn = hkl%ref(i)%s
              theta=Current_Orient%wave*sn
              theta=asin(theta)
              s2=sn*sn
              call Calc_General_StrFactor(hr,s2,A,SpG,Scattf,fn,fx,fe)
              Nuc=fn*conjg(fn)
              fsru=Schwinger_Amplitude(hr,[0.0,0.0, 1.0],theta,fe,Left=.false.)
              fslu=Schwinger_Amplitude(hr,[0.0,0.0, 1.0],theta,fe,Left=.true.)
              fsrd=Schwinger_Amplitude(hr,[0.0,0.0,-1.0],theta,fe,Left=.false.)
              fsld=Schwinger_Amplitude(hr,[0.0,0.0,-1.0],theta,fe,Left=.true.)
              up= (fn+fsru)*conjg(fn+fsru); down= (fn+fsrd)*conjg(fn+fsrd)
              if(up < 1.0e-6 .and. down < 1.0e-6) then
                 flip_right=1.0
              else
                 flip_right=up/down
              end if
              up=(fn+fslu)*conjg(fn+fslu); down= (fn+fsld)*conjg(fn+fsld)
              if(up < 1.0e-6 .and. down < 1.0e-6) then
                 flip_left=1.0
              else
                 flip_left=up/down
              end if

              call calc_angles(Current_Instrm%igeom,sig,ub_matrix(:,:,1),hr,ang,comment,current_d19_chi)

              if(len_trim(comment) == 0) then
                  Select Case (Current_Instrm%igeom)

                      Case(1,2,4)
                          write(unit=lun,fmt="(3i4,f9.5,f10.4,3(2f10.4,tr4),2f10.5,4(2f10.6,tr4),4f10.3,tr2,a)") &
                                hkl%Ref(i)%h, hkl%Ref(i)%s,Nuc,fn,fx,fe,flip_left,flip_right,&
                                fsru,fsrd,fslu,fsld,ang,trim(comment)

                      Case(3,-3)
                          write(unit=lun,fmt="(3i4,f9.5,f10.4,3(2f10.4,tr4),2f10.5,4(2f10.6,tr4),3f10.3,tr2,a)") &
                                hkl%Ref(i)%h, hkl%Ref(i)%s,Nuc,fn,fx,fe,flip_left,flip_right,&
                                fsru,fsrd,fslu,fsld,ang(1:3),trim(comment)
                  End Select

                  n=n+1
                  angles(1:6,n)  = [ flip_right,flip_left,abs(fsru),abs(fslu),abs(fsrd),abs(fsld) ]
                  reflx(:,n)   = hr
                  fst(n)       = Nuc
              end if

           end do
            !write final output file with a selected ordering according maximum right flipping ratio

            ind=sort(angles(1,:),n)

            open(unit=i_hkl, file=trim(filcod)//".shkl", status="replace",action="write")
            write(unit=i_hkl,fmt="(a)") &
            "     H        K        L          sF    flip_right flip_left     fsru      fslu      fsrd      fsldrd      fsld"
            do i=n,1,-1
                j=ind(i)
                write(unit=i_hkl,fmt="(3f9.4,f12.5,6f10.5)") reflx(:,j),fst(j), angles(:,j)
            end do

            close(unit=i_hkl)
        end if

        !Calculation of the completitude
        n=count(measured > 0)
        Frac_max=100.0*real(n)/real(nr_max)
        Frac_res=100.0*real(n)/real(nr_resol)

        if(n_frames /= 0) then
          total_time=total_time+ time_scan
        else
          total_time=total_time+ (nt+nmt)*time_ref*60.0
        end if
        call get_hms(total_time,cputim_string)
        if(spher) then
          write(unit=*,fmt="(//a,i8)")            " => Maximum number of reflections for the current wavelength              :",nr_max
          write(unit=*,fmt="(a,i8)")              " => Maximum number of reflections for the current resolution              :",nr_resol
          write(unit=*,fmt="(a,f8.5,a,f7.2,a)")   " => Measured Fraction of reflections (up to r*=2/lambda=",tteta,")         :",Frac_max,"%"
          write(unit=*,fmt="(a,f8.5,a,f7.2,a)")   " => Measured Fraction of reflections (up to r*=1/dmin  =",2.0*stlmax,")         :",Frac_res,"%"
          write(unit=lun,fmt="(//a,i8)")          " => Maximum number of reflections for the current wavelength              :",nr_max
          write(unit=lun,fmt="(a,i8)")            " => Maximum number of reflections for the current resolution              :",nr_resol
          write(unit=lun,fmt="(a,f8.5,a,f7.2,a)") " => Measured Fraction of reflections (up to r*=2/lambda=",tteta,     ")         :",Frac_max,"%"
          write(unit=lun,fmt="(a,f8.5,a,f7.2,a)") " => Measured Fraction of reflections (up to r*=1/dmin  =",2.0*stlmax,")         :",Frac_res,"%"
        else
          write(unit=*,fmt="(//a,i8)")            " => Maximum number of unique reflections for the current wavelength       :",nr_max
          write(unit=*,fmt="(a,i8)")              " => Maximum number of unique reflections for the current resolution       :",nr_resol
          write(unit=*,fmt="(a,f8.5,a,f6.2,a)")   " => Measured Fraction of unique reflections (up to r*=2/lambda=",tteta," ) : ",Frac_max,"%"
          write(unit=*,fmt="(a,f8.5,a,f6.2,a)")   " => Measured Fraction of unique reflections (up to r*=1/dmin  =",2.0*stlmax," ) : ",Frac_res,"%"
          write(unit=lun,fmt="(//a,i8)")          " => Maximum number of unique reflections for the current wavelength       :",nr_max
          write(unit=lun,fmt="(a,i8)")            " => Maximum number of unique reflections for the current resolution       :",nr_resol
          write(unit=lun,fmt="(a,f8.5,a,f6.2,a)") " => Measured Fraction of reflections (up to r*=2/lambda=",tteta," )        : ",Frac_max,"%"
          write(unit=lun,fmt="(a,f8.5,a,f6.2,a)") " => Measured Fraction of reflections (up to r*=1/dmin  =",2.0*stlmax," )        : ",Frac_res,"%"
        end if

        write(unit=lun,fmt="(a,i8)")                   " => Total Number of accessible nuclear  reflections (including redundancy):",nt
        if(mag_structure) write(unit=lun,fmt="(a,i8)") " => Total Number of accessible magnetic reflections (including redundancy):",nmt
        write(unit=lun,fmt="(a)")                      " => Total time for full data collection: "//trim(cputim_string)
        write(unit=*,fmt="(a,i8)")                     " => Total Number of accessible nuclear  reflections (including redundancy):",nt
        if(mag_structure) write(unit=*,fmt="(a,i8)")   " => Total Number of accessible magnetic reflections (including redundancy):",nmt
        write(unit=*,fmt="(a)")                        " => Total time for full data collection: "//trim(cputim_string)
        write(unit=*,fmt="(a)")                        " => Results in Files: "//trim(filcod)//".sfa  and "//trim(filcod)//".hkl"
        if(mag_structure) write(unit=*,fmt="(a)")      " => Magnetic Satellites in: "//trim(filcod)//".mhkl"
        if(Schwinger) write(unit=*,fmt="(a)")          " => Schwinger scatering in: "//trim(filcod)//".shkl"
    end if

    close(unit=lun)

    !Output the nuclear reflections redundancy
    open(unit=lun,file="redundancy.list",status="replace",action="write")
      n=0
      nm=0
      do i=1,hkl%Nref
         if(measured(i) == 0) cycle
         n=n+1
         if(n > nt) then
           n=nt
           exit
         end if
         nm=nm + measured(i)
         write(unit=lun,fmt="(3i4,f12.5,tr4,i5,i8)") hkl%Ref(i)%h,hkl%ref(i)%s, measured(i),n
      end do
      write(unit=lun,fmt="(a,i5)") " => Average redundancy: ",nm/n
      write(unit=*,fmt="(a,i5)")   " => Average redundancy: ",nm/n
    close(unit=lun)
    call cpu_time(tend)
    write(unit=*,fmt="(/,a)")        " => Normal End of: PROGRAM SXTAL_REFGEN "
    write(unit=*,fmt="(a,f10.2,a)")  " => CPU-Time: ", tend-start," seconds"

    call finish()

  contains

    Subroutine finish()
      if(wait_end) then
        write(unit=*,fmt="(a)",advance="no") " => Please, press <cr> to finish the program"
        read(unit=*,fmt="(a)") keyv
        stop
      else
        stop
      end if
    End Subroutine finish

    subroutine get_hms(times,timechar)
      real,            intent(in) :: times
      character(len=*),intent(out):: timechar
      real :: minutes,seconds,hours
      hours=times/3600.0
      minutes=(hours-int(hours))*60.0
      seconds=(minutes-int(minutes))*60.0
      write(timechar,"(i4,a,i3,a,f5.2,a)") nint(hours)," hours, ", nint(minutes)," minutes and ",seconds, " seconds"
    end subroutine get_hms

    Subroutine get_duration(delta,duration)
      real,  dimension(:), intent(in)  :: delta
      real,                intent(out) :: duration
      real    :: dur
      integer :: motor

      duration=0.0
      if(Current_Instrm%rangtim) then !Use information of empirical velocities to determine the duration
         !write(*,*) delta(1:4)
         do motor=1,size(delta)
           n3=count(Current_Instrm%range_time(1,:,motor) > 0.0001)
           dur=Linear_Interpol(delta(motor),Current_Instrm%range_time(1,1:n3,motor),Current_Instrm%range_time(2,1:n3,motor))
           if(dur > duration) duration=dur
           !write(*,*) motor, dur, duration
         end do

      else
         do motor=1,size(delta)
           if(abs(Current_Instrm%ang_velocity(motor)) < 0.000001) then
             dur=0.0
           else
             dur=delta(motor)/Current_Instrm%ang_velocity(motor)
           end if
           if(dur > duration) duration=dur  !This assumes motors move simultaneously
           !duration=duration + dur !This assumes that motors move sequentially
         end do
      end if
    End Subroutine get_duration


    Subroutine block_order(iopt,nbb)
        integer, optional, intent(in) :: iopt,nbb
        integer :: i,j,k,n_b,n_block,np,ioption
        real    :: ang1, ang2, deltang
        integer, dimension(:,:),allocatable :: indp
        real,    dimension(:,:),allocatable :: cref,c_angl
        real,    dimension(:),allocatable   :: cfst
        integer, dimension(:),allocatable   :: ctwin,indl

        n_b=0
        deltang=delta_ang
        if(present(nbb)) deltang=real(nbb)
        n_block= int((angles(1,n)-angles(1,1))/deltang)+1

        allocate(indp(2,n_block))
        indp=0
        ang1=angles(1,1)
        ang2= ang1+deltang
        indp(1,1)=1
        i=1
        do
          i=i+1
          if(i > n) exit
           if(angles(1,i) <= ang2) then
             cycle
           else
             n_b=n_b+1
             indp(2,n_b)=i-1
             indp(1,n_b+1)=i
             ang1=ang2
             ang2=ang1+deltang
           end if
        end do
        indp(2,n_block)=n

        ioption=iop
        if(present(iopt)) ioption=iopt
        if(ioption == 5) then
          k=4     !Phi
        else if(ioption == 6) then
          k=3     !Chi
        else if(ioption == 7) then
          k=2     !Omega
        end if

        if(present(iopt)) then !just order the angles

           do n_b=1,n_block
              i=indp(1,n_b); j=indp(2,n_b)
              np=j-i+1
              allocate(c_angl(4,np),indl(np))
              indl=0
              c_angl=angles(:,i:j)
              indl=sort(c_angl(k,:),np)
              c_angl=c_angl(:,indl)
              angles(:,i:j)= c_angl
              deallocate(c_angl,indl)
           end do

        else

           do n_b=1,n_block
              i=indp(1,n_b); j=indp(2,n_b)
              np=j-i+1
              allocate(c_angl(4,np),cref(3,np),cfst(np),ctwin(np),indl(np))
              indl=0
              cfst=fst(i:j); cref=reflx(:,i:j); ctwin=itwin(i:j); c_angl=angles(:,i:j)
              indl=sort(c_angl(k,:),np)
              cfst=cfst(indl); c_angl=c_angl(:,indl); cref(:,:)=cref(:,indl); ctwin=ctwin(indl)
              reflx(:,i:j) = cref
              angles(:,i:j)= c_angl
              fst(i:j)=cfst
              itwin(i:j)=ctwin
              deallocate(c_angl,cref,cfst,ctwin,indl)
           end do

        end if

    End Subroutine block_order

    Function Schwinger_Amplitude(hn,pol,theta,fe,Left,UB)  result(Schwinger)
      real(kind=cp), dimension(3),            intent(in) :: hn,pol
      real(kind=cp),                          intent(in) :: theta !in radians
      complex(kind=cp),                       intent(in) :: fe
      logical,                      optional, intent(in) :: left
      real(kind=cp), dimension(3,3),optional, intent(in) :: UB
      complex(kind=cp)                                   :: Schwinger

      real(kind=cp),dimension(3) :: uvect, hc

      if(present(left)) then
        if(left) then
          uvect=[0.0,0.0,-1.0]
        else
          uvect=[0.0,0.0,1.0]
        end if
      else if(present(UB)) then
        hc=matmul(UB,hn)
        uvect=cross_product([0.0,1.0,0.0],hc)
        uvect=-uvect/sqrt(dot_product(uvect,uvect))
      else
        Schwinger=0.0
        return
      end if
      !change of sign of "i" because the convention used by crystallographers is Q=Kf-Ki
      Schwinger= schw*cmplx(0.0,-1.0)*dot_product(pol,uvect)*fe/abs(tan(theta))
    End Function Schwinger_Amplitude

End Program Sxtal_Ref_Gen
