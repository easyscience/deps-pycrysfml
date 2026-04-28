
SubModule(CFML_Powder) Pow_IRF
  !>
  !> Derived from FullProf Module: Instrumental_resolution
  !>
  implicit none

  Contains

    Module Subroutine Init_IRF(irf,np)
      class(IRF_type),   intent(in out) :: irf
      integer, optional, intent(in)     :: np

       irf%Nam="  "
       irf%geom="  "
       irf%file_Nam="  "
       irf%JOBT="  "
       irf%irftype="   "
       irf%Num_Patt=0   ! Number of the corresponding pattern
       irf%N_points=0   ! If /= a list for tof, sigma, gamma, alpha, beta and shifts are provided
       if(present(np)) then
          if(np > 4) then
             irf%N_points=np
             allocate(irf%shift(np), irf%corr_int(np))
             irf%shift=0.0_cp; irf%corr_int=1.0_cp
          end if
       end if
       irf%tmin  =0.0_cp; irf%step=0.0_cp; irf%tmax=0.0_cp      ! range of scattering variable

       Select Type (irf)

        type is (IRF_CW_Type)
          irf%lambda=0.0_cp     ! Wavelengths
          irf%ratio =0.0_cp     ! Lambda(2)/Lambda(1) intensity ratio
          irf%rkk   =0.0_cp     ! Polarization factor (synchrotron))
          irf%alpsd =0.0_cp     ! Incident angle for fixed flat-plate
          irf%cthm  =0.0_cp     ! Cos(2th_Monok)^2
          irf%U_i  = 0.0_cp;  irf%V_i = 0.0_cp; irf%W_i = 0.0_cp ! Gaussian FWHM parameters for Lambda(1,2)
          irf%X_i  = 0.0_cp;  irf%Y_i = 0.0_cp; irf%Z_i = 0.0_cp ! Lorentzian FWHM parametersa for Lambda(1,2)
          irf%sl_i = 0.0_cp; irf%dl_i = 0.0_cp   ! Asymmetry parameters

          if(irf%N_points > 0 .and. present(np)) then
             allocate(irf%ttheta(np), irf%HG(np), irf%HL(np))
             irf%ttheta=0.0_cp; irf%HG=0.0_cp; irf%HL=0.0_cp
          end if

        type is (IRF_TOF_Type)

          irf%TOFTET_i = 0.0_cp     ! 2Theta of the detector bank in TOF
          irf%zero_i   = 0.0_cp     ! Epithermal zero
          irf%zerot_i  = 0.0_cp     ! Thermal zero
          irf%DTT1_i   = 0.0_cp     ! TOF =Zeroe + DTT1*d + DTT2 *d^2 (for NPRO=10, dtt2=0)
          irf%DTT2_i   = 0.0_cp     !
          irf%DTT_1overd_i= 0.0_cp  ! TOF =Zeroe + DTT1*d + DTT2 *d^2 + DTT_1overd/d (for NPRO=14)
          irf%DTT1t_i  = 0.0_cp     ! TOF =Zerot + DTT1t*d - DTT2t/d  (only for NPRO=10)
          irf%DTT2t_i  = 0.0_cp     !

          irf%alfa0_i  = 0.0_cp     ! Instrumental constants for
          irf%alfa1_i  = 0.0_cp     ! Time of Flight diffractometers
          irf%alfaq_i  = 0.0_cp     !
          irf%beta0_i  = 0.0_cp     !
          irf%betaq_i  = 0.0_cp     ! beta=bet0+beta1/d^4+ betaq/d^2
          irf%beta1_i  = 0.0_cp     ! All these parameters are read from
          irf%alfa0t_i = 0.0_cp     ! the instrumental resolution function
          irf%alfa1t_i = 0.0_cp     ! file characteristics of the instrument.
          irf%beta0t_i = 0.0_cp     !
          irf%beta1t_i = 0.0_cp     ! They are additive with respect to refined
          irf%xcross_i = 0.0_cp     ! parameters. The sum is performed in the
          irf%wcross_i = 0.0_cp     ! subroutine calling the calculation of TOF profile

          irf%sig0_i   = 0.0_cp     ! Gaussian width
          irf%sig1_i   = 0.0_cp     !
          irf%sig2_i   = 0.0_cp     !
          irf%sigq_i   = 0.0_cp     !
          irf%gamma0_i = 0.0_cp     ! Lorentzian width
          irf%gamma1_i = 0.0_cp     !
          irf%gamma2_i = 0.0_cp     !

          if(irf%N_points > 0  .and. present(np)) then
             allocate(irf%tof(np), irf%sigma(np), irf%gamma(np), irf%alpha(np), irf%beta(np))
             irf%tof=0.0_cp; irf%sigma=0.0_cp; irf%gamma=0.0_cp; irf%alpha=0.0_cp; irf%beta=0.0_cp
          end if

       End Select

    End Subroutine Init_IRF

    Module Subroutine Read_Patt_IRF(ffile,N_ini,N_end,mode,IRF,ipat)
       Type(file_type),              intent(in)   :: ffile
       integer,                      intent(in)   :: n_ini
       integer,                      intent(in)   :: n_end
       character(len=*),             intent(in)   :: mode
       class(IRF_Type), allocatable, intent(out)  :: IRF
       integer, optional,            intent(in)   :: ipat
       !---- Local Variables ----!
       integer                       :: i, j, ip
       character(len=:),allocatable  :: line, irf_file


       !> Init
       call clear_error()
       irf_file = "  "; ip = 1
       if(present(ipat)) ip=ipat
       do i=N_ini,N_end
          line=l_case(adjustl(ffile%line(i)%str))
          if (len_trim(line) == 0) cycle
          if (line(1:1) =="!") cycle
          j=index(line,'!')
          if (j > 0) line=line(:j-1)
          j=index(line,'#')
          if (j > 0) line=line(:j-1)
          j=index(line,'irf_file')
          if(j /= 0) then
            irf_file=adjustl(trim(line(j+8:)))
            exit
          end if
       end do
       if(len_trim(irf_file) > 4) then
         call Read_IRF(irf_file, IRF, mode, ip)
       end if
    End Subroutine Read_Patt_IRF

    !!----  Subroutine Read_IRF_CW(filename, IRF)
    !!----     character(len=*),  intent(in)  :: filename
    !!----     type(IRF_CW_Type), intent(out) :: IRF
    !
    Module Subroutine Read_IRF(filename, IRF, mode, ipat)
       character(len=*),             intent(in)  :: filename
       class(IRF_Type), allocatable, intent(out) :: IRF
       character(len=*),             intent(in)  :: mode
       integer,            optional, intent(in)  :: ipat
       !!
       integer                       :: i, j, k, ip, iv,ier
       real(kind=cp),dimension(7)    :: vet
       integer,      dimension(7)    :: ivet
       character (len=:),allocatable :: line, key, lin
       type(File_type)               :: Ft      ! File and lines information

       call clear_error()

       Select Case(mode)

         Case("CW")
           allocate(IRF_CW_Type :: IRF)

         Case("TF")
           allocate(IRF_TOF_Type :: IRF)

         Case Default
           call set_error(1,"The type (CW or TF) of the expected IRF file is not provided!")
           return

       End Select

       Ft=Reading_File(filename)
       if (Err_CFML%Ierr /=0) then
          write(unit=*,fmt="(a)") trim(Err_CFML%Msg)
          return
       end if
       ip=1
       if(present(ipat)) ip=ipat
       call Init_IRF(IRF)
       IRF%Num_Patt=ip
       IRF%file_nam=trim(filename)

       Select Type (IRF)

         type is(IRF_CW_Type)

            do i=1,Ft%nlines
               line=adjustl(Ft%line(i)%str)
               if (len_trim(line) == 0) cycle
               if (line(1:1) =="!" .or. line(1:1) =="#") cycle
               j=index(line,'!')
               if (j > 0) line=line(:j-1)
               j=index(line,'#')
               if (j > 0) line=line(:j-1)

               j=index(line," ")
               key=u_case(line(1:j-1))

               Select Case (key)

                  Case("JOBT")
                     IRF%JOBT= adjustl(trim(line(j:)))

                  Case("TITLE")
                     IRF%Nam=adjustl(trim(line(j:)))

                  Case("IRFTYPE")
                     IRF%irftype=adjustl(trim(line(j:)))

                  Case("WAVE")
                     call Get_Num(line(j:), vet, ivet, iv)
                    if(iv == 1) then
                      IRF%lambda(1)=vet(1)
                      IRF%lambda(2)=vet(1)
                      IRF%ratio=0.0
                    else
                      IRF%lambda(1)=vet(1)
                      IRF%lambda(2)=vet(2)
                      IRF%ratio=vet(3)
                    end if

                  Case("THRG","ENRG")
                     read(line(j:),*,iostat=ier) vet(1:3)
                     if(ier == 0) then
                        IRF%tmin=vet(1)
                      IRF%step=vet(2)
                      IRF%tmax=vet(3)
                     end if

                  Case("CTHM")
                     read(line(j:),*,iostat=ier) vet(1)
                     if(ier == 0) IRF%cthm=vet(1)

                  Case("RKK")
                     read(line(j:),*,iostat=ier) vet(1)
                     if(ier == 0) IRF%rkk=vet(1)

                  Case("ASYMM")
                     read(line(j:),*,iostat=ier) vet(1:2)
                     if(ier == 0) then
                        IRF%rkk=vet(1)
                        irf%sl_i = vet(1); irf%dl_i = vet(2)
                     end if

                  Case("GEOM")
                     IRF%geom=adjustl(trim(line(j:)))

                  Case("PSD")
                     read(line(j:),*,iostat=ier) vet(1)
                     if(ier == 0) IRF%alpsd=vet(1)

                  Case("UVWXYZ","UVWXYZ1")
                     read(line(j:),*,iostat=ier) vet(1:6)
                     if(ier == 0) then
                       IRF%U_i(1:2)=vet(1)
                       IRF%V_i(1:2)=vet(2)
                       IRF%W_i(1:2)=vet(3)
                       IRF%X_i(1:2)=vet(4)
                       IRF%Y_i(1:2)=vet(5)
                       IRF%Z_i(1:2)=vet(6)
                     end if

                  Case("UVWXYZ2")
                     read(line(j:),*,iostat=ier) vet(1:6)
                     if(ier == 0) then
                       IRF%U_i(2)=vet(1)
                       IRF%V_i(2)=vet(2)
                       IRF%W_i(2)=vet(3)
                       IRF%X_i(2)=vet(4)
                       IRF%Y_i(2)=vet(5)
                       IRF%Z_i(2)=vet(6)
                     end if

                  Case("N_POINTS")
                     read(line(j:),*,iostat=ier) ip
                     if(ier == 0) then
                       IRF%N_Points=ip
                       allocate(irf%ttheta(ip), irf%HG(ip), irf%HL(ip), irf%shift(ip), irf%corr_int(ip) )
                       irf%ttheta=0.0_cp
                       irf%HG=0.0_cp ; irf%HL=0.0_cp
                       irf%shift=0.0_cp ; irf%corr_int=1.0_cp
                       j=0; k=0
                       do
                         k=k+1
                         if(i+k > Ft%nlines) exit
                         lin=adjustl(Ft%line(i+k)%str)
                         if (lin(1:1) =="!" .or. lin(1:1) =="#" .or. len_trim(lin) == 0)  cycle
                         j=j+1
                         if(j > ip) exit
                         call Get_Num(lin, vet, ivet, iv)
                         Select Case (iv)
                           Case(3)
                             irf%ttheta(j)=vet(1)
                             irf%HG(j) = vet(2)
                             irf%HL(j) = vet(3)
                             irf%n_items=3
                           Case(4)
                             irf%ttheta(j)=vet(1)
                             irf%HG(j) = vet(2)
                             irf%HL(j) = vet(3)
                             irf%shift(j) = vet(4)
                             irf%n_items=4
                           Case(5)
                             irf%ttheta(j)=vet(1)
                             irf%HG(j) = vet(2)
                             irf%HL(j) = vet(3)
                             irf%shift(j) = vet(4)
                             irf%corr_int(j) = vet(5)
                             irf%n_items=5
                           Case Default
                             call Set_Error(1, "Error reading numerical IRF file (at least 3 columns are needed!)")
                             return
                         End Select
                       end do
                     else
                       call Set_Error(1, "Error reading the number of points in IRF file")
                       return
                     end if
               End Select

            end do

         type is(IRF_TOF_Type)

            do i=1,Ft%nlines
               line=adjustl(Ft%line(i)%str)
               if (len_trim(line) == 0) cycle
               if (line(1:1) =="!" .or. line(1:1) =="#") cycle
               j=index(line,'!')
               if (j > 0) line=line(:j-1)
               j=index(line,'#')
               if (j > 0) line=line(:j-1)

               j=index(line," ")
               key=u_case(line(1:j-1))

               Select Case (key)

                  Case("JOBT")
                     IRF%JOBT= adjustl(trim(line(j:)))

                  Case("TITLE")
                     IRF%Nam=adjustl(trim(line(j:)))

                  Case("IRFTYPE")
                     IRF%irftype=adjustl(trim(line(j:)))

                  Case("TOFRG")
                     read(line(j:),*,iostat=ier) vet(1:3)
                     if(ier == 0) then
                       IRF%tmin=vet(1)
                       IRF%step=vet(2)
                       IRF%tmax=vet(3)
                     end if

                  Case("TWOTH")
                     read(line(j:),*,iostat=ier) vet(1)
                     if(ier == 0) IRF%TofTet_i=vet(1)

                  Case("D2TOF")
                     call Get_Num(line(j:), vet, ivet, iv)
                     Select Case(iv)
                       Case(1)
                           irf%dtt1_i=vet(1)
                       Case(2)
                           irf%dtt1_i=vet(1)
                           irf%dtt2_i=vet(2)
                       Case(3)
                           irf%dtt1_i=vet(1)
                           irf%dtt2_i=vet(2)
                           irf%zero_i=vet(3)
                       Case(4)
                           irf%dtt1_i=vet(1)
                           irf%dtt2_i=vet(2)
                           irf%dtt_1overd_i=vet(3)
                           irf%zero_i=vet(4)
                       Case Default
                           call Set_Error(1, "Error reading D2TOF in IRF file (at least 1 value is needed!)")
                           return
                     End Select

                  Case("SIGMA")
                     call Get_Num(line(j:), vet, ivet, iv)
                     Select Case(iv)
                       Case(3)
                           irf%sig2_i=vet(1)
                           irf%sig1_i=vet(2)
                           irf%sig0_i=vet(3)
                       Case(4)
                           irf%sig2_i=vet(1)
                           irf%sig1_i=vet(2)
                           irf%sig0_i=vet(3)
                           irf%sigQ_i=vet(4)
                       Case Default
                           call Set_Error(1, "Error reading SIGMA in IRF file (at least 3 values are needed!)")
                           return
                     End Select


                  Case("GAMMA")
                     call Get_Num(line(j:), vet, ivet, iv)
                     Select Case(iv)
                       Case(3)
                           irf%gamma2_i=vet(1)
                           irf%gamma1_i=vet(2)
                           irf%gamma0_i=vet(3)
                       Case Default
                           call Set_Error(1, "Error reading GAMMA in IRF file (3 values are needed!)")
                           return
                     End Select

                  Case("ALFBE")
                     call Get_Num(line(j:), vet, ivet, iv)
                     Select Case(iv)
                       Case(4)
                          irf%alfa0_i=vet(1)
                          irf%beta0_i=vet(2)
                          irf%alfa1_i=vet(3)
                          irf%beta1_i=vet(4)
                       Case(6)
                          irf%alfa0_i=vet(1)
                          irf%beta0_i=vet(2)
                          irf%alfa1_i=vet(3)
                          irf%beta1_i=vet(4)
                          irf%alfaq_i=vet(5)
                          irf%betaq_i=vet(6)
                       Case Default
                           call Set_Error(1, "Error reading ALFBE in IRF file (at least 4 values are needed!)")
                           return
                     End Select

                  Case("LIST_SIG_GAM","LIST_SIG_GAM_SHIFT","LIST_SIG_GAM_ALF_BET","LIST_SIG_GAM_ALF_BET_SHIFT","LIST_SIG_GAM_ALF_BET_SHIFT_CORR")
                     call Get_Num(line(j:), vet, ivet, iv)
                     Select Case(iv)
                       Case(1)
                          ip=vet(1)
                       Case Default
                           call Set_Error(1, "Error reading ALFBE in IRF file (at least 4 values are needed!)")
                           return
                     End Select
                     IRF%N_Points=ip
                     allocate(irf%tof(ip), irf%sigma(ip), irf%gamma(ip), irf%alpha(ip), irf%beta(ip), irf%shift(ip), irf%corr_int(ip) )
                     irf%tof=0.0_cp
                     irf%sigma=0.0_cp ; irf%gamma=0.0_cp
                     irf%alpha=0.0_cp ; irf%beta=0.0_cp
                     irf%shift=0.0_cp ; irf%corr_int=1.0_cp
                     j=0; k=0
                     do
                       k=k+1
                       if(i+k > Ft%nlines) exit
                       lin=adjustl(Ft%line(i+k)%str)
                       if (lin(1:1) =="!" .or. lin(1:1) =="#" .or. len_trim(lin) == 0)  cycle
                       j=j+1
                       if(j > ip) exit
                       call Get_Num(lin, vet, ivet, iv)
                       Select Case (iv)
                         Case(3)
                           irf%tof(j)   = vet(1)
                           irf%sigma(j) = vet(2)
                           irf%gamma(j) = vet(3)
                           irf%n_items=3

                         Case(4)
                           irf%tof(j)   = vet(1)
                           irf%sigma(j) = vet(2)
                           irf%gamma(j) = vet(3)
                           irf%shift(j) = vet(4)
                           irf%n_items=4

                         Case(5)
                           irf%tof(j)   = vet(1)
                           irf%sigma(j) = vet(2)
                           irf%gamma(j) = vet(3)
                           irf%alpha(j) = vet(4)
                           irf%beta(j)  = vet(5)
                           irf%n_items=5

                         Case(6)
                           irf%tof(j)   = vet(1)
                           irf%sigma(j) = vet(2)
                           irf%gamma(j) = vet(3)
                           irf%alpha(j) = vet(4)
                           irf%beta(j)  = vet(5)
                           irf%shift(j) = vet(6)
                           irf%n_items=6

                         Case(7)
                           irf%tof(j)   = vet(1)
                           irf%sigma(j) = vet(2)
                           irf%gamma(j) = vet(3)
                           irf%alpha(j) = vet(4)
                           irf%beta(j)  = vet(5)
                           irf%shift(j) = vet(6)
                           irf%corr_int(j)=vet(7)
                           irf%n_items=7

                         Case Default
                           call Set_Error(1, "Error reading numerical IRF file (at least 3 columns are needed!)")
                           return
                       End Select
                     end do
               End Select
            End do
       End Select  !Type of IRF is

    End Subroutine Read_IRF

    Module Subroutine Write_IRF(IRF,iprint)
       class(IRF_Type),   intent(in) :: IRF
       integer, optional, intent(in) :: iprint
       !--- Local variables ---!
       integer :: i, lun
       logical :: add_uvw
       real(kind=cp), parameter :: ep=0.0001_cp
       lun=6
       if(present(iprint)) lun=iprint

       write(lun,"(/a)")   "   ==============================================================="
       write(lun,"(a,i3)") "   Content of the Instrumental Resolution Function for Pattern ",IRF%Num_Patt
       write(lun,"(a/)")   "   ==============================================================="
       write(lun,"(a)")    "   Title "//trim(irf%Nam)
       write(lun,"(a)")    "    Jobt "//trim(irf%Jobt)
       write(lun,"(a)")    "    Geom "//trim(irf%geom)
       write(lun,"(a)")    " IRFtype "//trim(irf%irftype)
       write(lun,"(a)")    "!   File "//trim(IRF%file_nam)
       write(lun,"(a,i4)") "!   Pattern #",IRF%Num_Patt

       Select Type(IRF)

         type is(IRF_CW_Type)

             write(lun,"(a)")        "!          Lambda-1    Lambda-2       Ratio"
             write(lun,"(a,3f12.6)") "  Wave ",irf%lambda, irf%ratio
             if(irf%step >ep) then
                write(lun,"(a)")        "!        2Theta-min       Step   2Theta-max"
                write(lun,"(a,3f12.6)") "  Thrg ",irf%tmin, irf%step,irf%tmax
             end if
             if(irf%cthm > ep) then
                write(lun,"(a,f12.6)")  "  Cthm ",irf%cthm
                write(lun,"(a)")        "!         cos^2(2Theta_monok) "
             end if
             if(irf%Rkk > ep) then
                write(lun,"(a)")        "!         Polarisation "
                write(lun,"(a,f12.6)")  "   Rkk ",irf%rkk
             end if
             if(irf%alpsd > ep) then
                write(lun,"(a)")        "!         PSD-inc-angle "
                write(lun,"(a,f12.6)")  "   PSD ",irf%alpsd
             end if
             if(irf%sl_i > ep) then
                write(lun,"(a)")        "!            S_L         D_L   "
                write(lun,"(a,2f12.6)") " Asymm ",irf%sl_i,irf%dl_i
             end if

             if(irf%N_POINTS > 0) then
                write(lun,"(a,i4)") "N_POINTS",IRF%n_points
                Select Case(IRF%n_items)
                  Case(3)
                    write(lun,"(a)")        "!       2Theta      Gaussian(HG)  Lorentzian(HL)"
                    do i=1,irf%N_Points
                        write(lun,"(3F16.6)") irf%ttheta(i), irf%hg(i), irf%hl(i)
                    end do
                  Case(4)
                    write(lun,"(a)")        "!       2Theta      Gaussian(HG)  Lorentzian(HL)         Shift"
                    do i=1,irf%N_Points
                        write(lun,"(4F16.6)") irf%ttheta(i), irf%hg(i), irf%hl(i), irf%shift(i)
                    end do
                  Case(5)
                    write(lun,"(a)")        "!       2Theta      Gaussian(HG)  Lorentzian(HL)         Shift          Corr_Int"
                    do i=1,irf%N_Points
                        write(lun,"(5F16.6)") irf%ttheta(i), irf%hg(i), irf%hl(i), irf%shift(i), irf%corr_int(i)
                    end do
                End Select

             else
                Select Case(trim(irf%irftype))
                   Case("TCH_pV_IRF1")
                      write(lun,"(a)")  "!   HG**2 = (U_i tan(theta)+V_i)*tan(theta)+W_i,   HL = X_i*tan(theta)+Y_i/cos(theta)+Z_i    "
                      write(lun,"(a)")  "!            U_instr     V_instr     W_instr     X_instr     Y_instr     Z_instr"

                   Case("pV_IRF")
                      write(lun,"(a)")  "!  H**2 = (U_i tan(theta)+V_i)*tan(theta)+W_i, Eta= Eta_X + Eta_Y * (2theta/100.0) + Eta_Z * (2theta/100.0)^2"
                      write(lun,"(a)")  "!  HG and HL should be calculated by the calling program"
                      write(lun,"(a)")  "!            U_instr     V_instr     W_instr      Eta_X       Eta_Y      Eta_Z"

                   Case("TCH_pV_IRF2")
                      write(lun,"(a)")  "!  HG**2 = (U_i tan(theta)+V_i)*tan(theta)+W_i, HL=(X_i*(2theta/100.0)+Y_i)*(2theta/100.0)+Z_i"
                      write(lun,"(a)")  "!            U_instr     V_instr     W_instr     X_instr     Y_instr     Z_instr"

                   Case("TCH_pV_IRF3")
                      write(lun,"(a)")  "!  HG**2 = {U_i*(2theta/100.0)+V_i}*(2theta/100.0)+W_i, HL={X_i*(2theta/100.0)+Y_i}*(2theta/100.0)+Z_i"
                      write(lun,"(a)")  "!            U_instr     V_instr     W_instr     X_instr     Y_instr     Z_instr"

                   Case Default
                      write(lun,"(a)")  "!   HG**2 = (U_i tan(theta)+V_i)*tan(theta)+W_i,   HL = X_i*tan(theta)+Y_i/cos(theta)+Z_i    "
                      write(lun,"(a)")  "!            U_instr     V_instr     W_instr     X_instr     Y_instr     Z_instr"

                End Select
                write(lun,"(a,6f12.6)") " UVWXYZ ",irf%u_i(1),irf%v_i(1),irf%w_i(1),irf%x_i(1),irf%y_i(1),irf%z_i(1)
                add_uvw=.false.
                if(abs(irf%u_i(1)-irf%u_i(2)) > ep) add_uvw=.true.
                if(abs(irf%v_i(1)-irf%v_i(2)) > ep) add_uvw=.true.
                if(abs(irf%w_i(1)-irf%w_i(2)) > ep) add_uvw=.true.
                if(abs(irf%x_i(1)-irf%x_i(2)) > ep) add_uvw=.true.
                if(abs(irf%y_i(1)-irf%y_i(2)) > ep) add_uvw=.true.
                if(abs(irf%z_i(1)-irf%z_i(2)) > ep) add_uvw=.true.
                if(add_uvw) then
                   write(lun,"(a,6f16.6)") "UVWXYZ2 ",irf%u_i(1),irf%v_i(1),irf%w_i(1),irf%x_i(1),irf%y_i(1),irf%z_i(1)
                end if
             end if

         type is(IRF_TOF_Type)

             write(lun,"(a)")        "!           dtt1        dtt2        dtt_1overD      Zero "
             write(lun,"(a,4f12.6)") "  D2TOF ",irf%dtt1_i,irf%dtt2_i,irf%dtt_1overd_i,irf%Zero_i
             write(lun,"(a)")        "!         2-theta-BANK "
             write(lun,"(a,f12.6)") "  TWOTH ",irf%TofTet_i
             if(irf%step > ep) then
                write(lun,"(a)")        "!        TOF-min       Step      TOF-max "
                write(lun,"(a,3f16.6)") "  Tofrg ",irf%tmin, irf%step,irf%tmax
             end if

             if(irf%N_points > 0) then
                write(lun,"(a,i4)") "N_POINTS ",IRF%n_points
                Select Case (irf%n_items)
                   Case(3)
                      write(lun,"(a,i4)") "LIST_SIG_GAM ",IRF%n_points
                      do i=1,IRF%n_points
                         write(lun,"(3F16.6)") irf%tof(i), irf%sigma(i), irf%gamma(i)
                      end do

                   Case(4)
                      write(lun,"(a,i4)") "LIST_SIG_GAM_ALF_SHIFT ",IRF%n_points
                      do i=1,IRF%n_points
                         write(lun,"(4F16.6)") irf%tof(i), irf%sigma(i), irf%gamma(i),irf%shift(i)
                      end do
                   Case(5)
                      write(lun,"(a,i4)") "LIST_SIG_GAM_ALF_BET ",IRF%n_points
                      do i=1,IRF%n_points
                         write(lun,"(5F16.6)") irf%tof(i), irf%sigma(i), irf%gamma(i),irf%alpha(i),irf%beta(i)
                      end do
                   Case(6)
                      write(lun,"(a,i4)") "LIST_SIG_GAM_ALF_BET_SHIFT ",IRF%n_points
                      do i=1,IRF%n_points
                         write(lun,"(6F16.6)") irf%tof(i), irf%sigma(i), irf%gamma(i),irf%alpha(i),irf%beta(i),irf%shift(i)
                      end do
                   Case(7)
                      write(lun,"(a,i4)") "LIST_SIG_GAM_ALF_BET_SHIFT_CORR ",IRF%n_points
                      do i=1,IRF%n_points
                         write(lun,"(6F16.6)") irf%tof(i), irf%sigma(i), irf%gamma(i),irf%alpha(i),irf%beta(i),irf%shift(i),irf%corr_int(i)
                      end do
                End Select

             else

                write(lun,"(a)")        "!          Sig2_instr     Sig1_instr    Sig0_instr   SigQ_instr"
                write(lun,"(a,4f12.6)") " SIGMA ",irf%sig2_i,irf%sig1_i,irf%sig0_i,irf%sigQ_i

                write(lun,"(a)")        "!          Gam2_instr     Gam1_instr    Gam0_instr"
                write(lun,"(a,3f12.6)") " GAMMA ",irf%gamma2_i,irf%gamma1_i,irf%gamma0_i

                write(lun,"(a)")        "!         alfa0_instr     beta0_instr   alfa1_instr  beta1_instr  alfaQ_instr betaQ_instr "
                write(lun,"(a,6f12.6)") " ALFBE ", irf%alfa0_i,irf%beta0_i,irf%alfa1_i,irf%beta1_i,irf%alfaq_i,irf%betaq_i

             End if ! N_Points

       End Select  !Type IRF

    End Subroutine Write_IRF

    Module Subroutine Corr_Int(npcorr,corrv,postt,xcorr,corrp)
      integer,                              intent(in)  :: npcorr !> Number of points in the correction array, or polynomial order
      real(kind=cp), dimension(:),          intent(in)  :: corrv  !> Value array, or polynomial coefficients
      real(kind=cp),                        intent(in)  :: postt  !> Position to correct for intensity
      real(kind=cp),                        intent(out) :: xcorr  !> Correction factor to apply to the intensity
      real(kind=cp), dimension(:),optional, intent(in)  :: corrp  !> Position array for linear interpolation
      !--- Local variables
      integer :: i
      real(kind=cp) :: dp

      if(present(corrp)) then
        i=locate(corrp(:),postt,npcorr)
        if(i == 0) i=1
        dp=(postt-corrp(i))/(corrp(i+1)-corrp(i))
        xcorr=corrv(i)+dp*(corrv(i+1)-corrv(i))
      else
        xcorr=corrv(1)
        do i=2,npcorr
          xcorr=xcorr+corrv(i)*postt**(i-1)
        end do
      end if
    End Subroutine Corr_Int



    !subroutine write_irf_info(n_pat)
    !   integer, intent (in) :: n_pat
    !   integer :: i
    !   write(i_out, '(a,a)')     ' => Instrumental Resolution read from file: ',trim(fileres(n_pat))
    !   write(i_out, '(a,a/)')    ' => Title of data: ', trim(instru)
    !   write(i_out,'(2(a,i4))')  ' => The resolution function is IRESOL:', ireso(n_pat), ' for profile function #',nprof(n_pat)
    !   write(i_out, '(a)')       '    Input resolution parameters:'
    !   if (ireso(n_pat) /= 4 .and. ireso(n_pat) /= 8) then
    !
    !    if(ixunit(n_pat) == 1) then
    !       Select case(ireso(n_pat))
    !
    !          Case(5)
    !
    !            write(i_out,"(/a)")      '        Dtt1_i       Dtt2_i     TwoThetaBank      Zero_i'
    !            write(i_out,"(4f14.5)")  Dtt1_i(n_pat), Dtt2_i(n_pat), toftet_i(n_pat), zero_i(n_pat)
    !            write(i_out,"(/a)")      '        Sig2_i        Sig1_i        Sig0_i'
    !            write(i_out,"(3f14.5)")  Sig2_i(n_pat), Sig1_i(n_pat), Sig0_i(n_pat)
    !            write(i_out,"(/a)")      '      Gamma2_i      Gamma1_i      Gamma0_i'
    !            write(i_out,"(3f14.5)")  Gamma2_i(n_pat), Gamma1_i(n_pat), Gamma0_i(n_pat)
    !            if(nprof(n_pat) == 13) &
    !              write(i_out,"(/a)")      '      Alpha0_i       Beta0_i       Alpha1_i      Kappa_i'
    !
    !            if (nprof(n_pat) /= 7) then
    !
    !              if(.not. alfbet_g(n_pat) .and. .not. sigam_g(n_pat)) then
    !                  write(i_out,"(/a)")      '      Alpha0_i       Beta0_i       Alpha1_i      Beta1_i      AlphaQ_i      BetaQ_i'
    !                  write(i_out,"(6f14.5/)")  Alfa0_i(n_pat), Beta0_i(n_pat), Alfa1_i(n_pat), Beta1_i(n_pat), AlfaQ_i(n_pat), BetaQ_i(n_pat)
    !              else
    !
    !                if(alfbet_g(n_pat) .and. sigam_g(n_pat)) then
    !                   !Nullify all instrumetal parameters
    !                   Sig2_i(n_pat)=0.0; Sig1_i(n_pat)=0.0; Sig0_i(n_pat)=0.0
    !                   Gamma2_i(n_pat)=0.0;  Gamma1_i(n_pat)=0.0; Gamma0_i(n_pat)=0.0
    !                   Alfa0_i(n_pat)=0.0; Beta0_i(n_pat)=0.0; Alfa1_i(n_pat)=0.0; Beta1_i(n_pat)=0.0
    !
    !                   write(i_out,"(/a)")      '      Variation of sigma, gamma, alpha, beta and shift parameters with d-spacing:'
    !                   write(i_out,"( a)")&
    !                            '          d-spacing           Sigma          Gamma           Alpha           Beta       Shift(us)'
    !                   Do i = 1, npoins(n_pat)
    !                    write (i_out, "(f19.5,5('  ',f14.6))") tins(i,n_pat),gins(i,n_pat),cins(i,n_pat),alfins(i,n_pat),&
    !                                                           betins(i,n_pat),shiftins(i,n_pat)
    !                   end do
    !                   write(i_out,'(a)') ' '
    !                   write(i_out,'(a)') '  The instrumental coefficients: sig_i,gamma_i, alpha_i and beta_i have been put to zero! '
    !                   write(i_out,'(a)') '  The calculation of the instrumental parameters are done by interpolating the above'// &
    !                                      '  values for each reflection'
    !
    !                else if (alfbet_g(n_pat)) then
    !
    !                   Alfa0_i(n_pat)=0.0; Beta0_i(n_pat)=0.0; Alfa1_i(n_pat)=0.0; Beta1_i(n_pat)=0.0
    !                   write(i_out,"(/a)")      '      Variation of alpha and beta decay parameters with d-spacing:'
    !                   write(i_out,"( a)")      '          d-spacing          Alpha            Beta'
    !                   Do i = 1, npoins(n_pat)
    !                     write (i_out, "(f19.5,2('  ',f14.6))") tins(i,n_pat), alfins(i,n_pat), betins(i,n_pat)
    !                   end do
    !                   write(i_out,'(a)') ' '
    !                   write(i_out,'(a)') '  The instrumental coefficients: Alfa0_i, Beta0_i, Alfa1_i & Beta1_i have been put to zero! '
    !                   write(i_out,'(a)') '  The calculation of the instrumental Alpha and Beta are done by interpolating the above'// &
    !                                      '  values for each reflection'
    !
    !                else if (sigam_g(n_pat)) then
    !
    !                   Sig2_i(n_pat)=0.0; Sig1_i(n_pat)=0.0; Sig0_i(n_pat)=0.0
    !                   Gamma2_i(n_pat)=0.0;  Gamma1_i(n_pat)=0.0; Gamma0_i(n_pat)=0.0
    !                   write(i_out,"(/a)")      '      Variation of Sigma and Gamma FWHM parameters with d-spacing:'
    !                   write(i_out,"( a)")      '          d-spacing          Sigma            Gamma'
    !                   Do i = 1, npoins(n_pat)
    !                     write (i_out, "(f19.5,2('  ',f14.6))") tins(i,n_pat), gins(i,n_pat), cins(i,n_pat)
    !                   end do
    !                   write(i_out,'(a)') ' '
    !                   write(i_out,'(a)') '  The instrumental coefficients: sig_i and gamma_i have been put to zero! '
    !                   write(i_out,'(a)') '  The calculation of the instrumental Sigma and Gamma are done by interpolating the above'// &
    !                                      '  values for each reflection'
    !
    !                end if
    !              end if
    !
    !            end if
    !
    !          case (6)
    !
    !            write(i_out,"(/a)")      '        Dtt1_i  TwoThetaBank'
    !            write(i_out,"(2f14.5)")  Dtt1_i(n_pat), toftet_i(n_pat)
    !            write(i_out,"(/a)")      '       Dtt1t_i       Dtt2t_i      xcross_i      wcross_i'
    !            write(i_out,"(4f14.5)")  Dtt1t_i(n_pat), Dtt2t_i(n_pat), xcross_i(n_pat), wcross_i(n_pat)
    !            write(i_out,"(/a)")      '        Sig2_i        Sig1_i        Sig0_i'
    !            write(i_out,"(3f14.5)")  Sig2_i(n_pat), Sig1_i(n_pat), Sig0_i(n_pat)
    !            write(i_out,"(/a)")      '      Gamma2_i      Gamma1_i      Gamma0_i'
    !            write(i_out,"(3f14.5)")  Gamma2_i(n_pat), Gamma1_i(n_pat), Gamma0_i(n_pat)
    !            write(i_out,"(/a)")      '      Alpha0_i       Beta0_i       Alpha1_i      Beta1_i'
    !            write(i_out,"(4f14.5/)")  Alfa0_i(n_pat), Beta0_i(n_pat), Alfa1_i(n_pat), Beta1_i(n_pat)
    !            write(i_out,"(/a)")      '     Alpha0t_i      Beta0t_i     Alpha1t_i      Beta1t_i'
    !            write(i_out,"(4f14.5/)")  Alfa0t_i(n_pat), Beta0t_i(n_pat), Alfa1t_i(n_pat), Beta1t_i(n_pat)
    !
    !       End Select
    !
    !       if(range_g(n_pat)) then
    !         write(i_out,"(/a,f12.2,a,f12.2,a)")      '  => Time of flight limited to the range: [',&
    !             low_range(n_pat),' , ',high_range(n_pat),'] microseconds'
    !       end if
    !
    !    else  !(ixunit(n_pat) == 1)
    !
    !      if(ixunit(n_pat) == 2) then
    !        write(i_out,"(a)")      ' => The resolution function is for Energy Dispersive mode'
    !        Write(i_out,'(a,f10.5,a)')' =>    Zero-inst: ', zero_i(n_pat),' |'
    !        Write(i_out,'(a,f10.5,a)')' =>    StE1-inst: ', dtt1_i(n_pat),'  > En = Zero-inst + StE1_inst * S + StE2_inst * S^2'
    !        Write(i_out,'(a,f10.5,a)')' =>    StE2-inst: ', dtt2_i(n_pat),' |'
    !        write(i_out,"(a)")      '    The U,V,W parameters correspond to HG^2 = U * En^2 + V * En + W'
    !        write(i_out,"(a)")      '    The X,Y,Z parameters correspond to HL = X * En + Y  (Z is not used)'
    !        if(nprof(n_pat) == 7) then
    !          write(i_out,"(a)")    '    Strains affect U and Sizes affect W in energy space'
    !        end if
    !      end if
    !
    !      if(ireso(n_pat) == -1) then
    !         write(i_out,"(/a)")  '   U-inst    V-inst     W-inst     Eta-inst    X-inst   '
    !         write(i_out,"(5f11.5/5f11.5/)") uins(1,n_pat), vins(1,n_pat), wins(1,n_pat), xins(1,n_pat), yins(1,n_pat), &
    !           uins(2,n_pat), vins(2,n_pat), wins(2,n_pat), xins(2,n_pat), yins(2,n_pat)
    !      else
    !         write(i_out,"(/a)")  '     U-inst     V-inst     W-inst     X-inst     Y-inst     Z-inst'
    !         write(i_out,"(6f11.5/6f11.5/)") uins(1,n_pat), vins(1,n_pat), wins(1,n_pat), xins(1,n_pat), yins(1,n_pat), &
    !         zins(1,n_pat), uins(2,n_pat), vins(2,n_pat), wins(2,n_pat), xins(2,n_pat), yins(2,n_pat), zins(2,n_pat)
    !      end if
    !
    !    end if
    !
    !   else !(ireso(n_pat) /= 4 .and. ireso(n_pat) /= 8)
    !
    !      if(ixunit(n_pat) == 2) then   !ireso = 4 (8 does not apply because asymmetry is not applicable)
    !        write(i_out,"(a)")      ' => The resolution function is for Energy Dispersive mode'
    !        Write(i_out,'(a,f10.5)')' =>    Zero-inst: ', zero_i(n_pat),' |'
    !        Write(i_out,'(a,f10.5)')' =>    StE1-inst: ', dtt1_i(n_pat),'  > En(k)= Zero-inst + StE1-inst * S + StE2-inst * S^2'
    !        Write(i_out,'(a,f10.5)')' =>    StE2-inst: ', dtt2_i(n_pat),' |'
    !      end if
    !
    !     if(ireso(n_pat) == 4) then
    !       write(i_out,'(/a/)') '   2-Theta/Energy    Gaussian-HW   Lorentzian-HW'
    !       Do i = 1, npoins(n_pat)
    !          write (i_out, "(f12.4,'    ',f12.6,'   ',f12.6)") tins(i,n_pat), gins(i,n_pat), cins(i,n_pat)
    !       end do
    !       write(i_out,'(a)') ' '
    !     else
    !       write(i_out,'(/a/)') '   2-Theta/Energy    Gaussian-HW   Lorentzian-HW      S_L(ins)    D_L(ins)'
    !       Do i = 1, npoins(n_pat)
    !          write (i_out, "(f12.4,'  ',4('  ',f12.6))") tins(i,n_pat), gins(i,n_pat), cins(i,n_pat),sl_ins(i,n_pat),dl_ins(i,n_pat)
    !       end do
    !       write(i_out,'(a)') ' '
    !     end if
    !
    !   end if
    !   return
    !end subroutine write_irf_info
    !
    !
    !subroutine apply_irf_cw(n_pat,ilam,ire,thet2,tan2,tanx,csqh)
    !   integer,       intent (in) :: n_pat,ilam,ire
    !   real(kind=cp), intent (in) :: tan2,tanx,thet2,csqh !csqh=1.0 for Energy dispersive
    !   real(kind=cp)              :: dtt, dgg, dcc,h,eta,hg,hl,dsl,ddl
    !   integer                    :: i, ip
    !   !character(len=132)         :: text
    !     Select Case (IRESO(n_pat))
    !      case (-1)
    !        h=sqrt(uins(ilam,n_pat)*tan2 + vins(ilam,n_pat)*tanx + wins(ilam,n_pat))
    !        eta=xins(ilam,n_pat)+yins(ilam,n_pat)*thet2
    !        call keijser(h,eta,hg,hl)
    !        ggi(ire,n_pat) = hg*hg
    !        cci(ire,n_pat) = hl
    !        if(asym_g(n_pat)) then
    !              sl_i(ire,n_pat)=sl_ins(1,n_pat)
    !              dl_i(ire,n_pat)=dl_ins(1,n_pat)
    !        end if
    !      case (1)
    !        ggi(ire,n_pat) = uins(ilam,n_pat)*tan2 + vins(ilam,n_pat)*tanx + wins(ilam,n_pat)
    !        cci(ire,n_pat) = xins(ilam,n_pat)*tanx + yins(ilam,n_pat)/csqh + zins(ilam,n_pat)
    !        !write(unit=text,fmt="(a,5f12.5)")" tan2,tanx,thet2,gg,cc: ", tan2,tanx,thet2,sqrt(ggi(ire,n_pat)),cci(ire,n_pat)
    !        !call mess_cons(text)
    !        if(asym_g(n_pat)) then
    !              sl_i(ire,n_pat)=sl_ins(1,n_pat)
    !              dl_i(ire,n_pat)=dl_ins(1,n_pat)
    !        end if
    !      case (2)
    !        ggi(ire,n_pat) = uins(ilam,n_pat)*tan2 + vins(ilam,n_pat)*tanx + wins(ilam,n_pat)
    !        cci(ire,n_pat) = (xins(ilam,n_pat)*thet2+yins(ilam,n_pat))*thet2 + zins(ilam,n_pat)
    !        if(asym_g(n_pat)) then
    !              sl_i(ire,n_pat)=sl_ins(1,n_pat)
    !              dl_i(ire,n_pat)=dl_ins(1,n_pat)
    !        end if
    !      case (3)
    !        ggi(ire,n_pat) = (uins(ilam,n_pat)*thet2+vins(ilam,n_pat))*thet2 + wins(ilam,n_pat)
    !        cci(ire,n_pat) = (xins(ilam,n_pat)*thet2+yins(ilam,n_pat))*thet2 + zins(ilam,n_pat)
    !        if(asym_g(n_pat)) then
    !              sl_i(ire,n_pat)=sl_ins(1,n_pat)
    !              dl_i(ire,n_pat)=dl_ins(1,n_pat)
    !        end if
    !      case (4,8)
    !        !
    !        !  Instrumental function calculated by interpolation
    !        !
    !        if (thet2 < tins(1,n_pat)) then
    !           ggi(ire,n_pat) = gins(1,n_pat)*gins(1,n_pat)
    !           cci(ire,n_pat) = cins(1,n_pat)
    !           if(ireso(n_pat) == 8) then
    !              sl_i(ire,n_pat)=sl_ins(1,n_pat)
    !              dl_i(ire,n_pat)=dl_ins(1,n_pat)
    !           end if
    !           return
    !        end if
    !        if (thet2 >= tins(npoins(n_pat),n_pat)) then
    !           ggi(ire,n_pat) = gins(npoins(n_pat),n_pat)*gins(npoins(n_pat),n_pat)
    !           cci(ire,n_pat) = cins(npoins(n_pat),n_pat)
    !           if(ireso(n_pat) == 8) then
    !              sl_i(ire,n_pat)=sl_ins(npoins(n_pat),n_pat)
    !              dl_i(ire,n_pat)=dl_ins(npoins(n_pat),n_pat)
    !           end if
    !           return
    !        end if
    !        do i = 1, npoins(n_pat) - 1
    !           if (thet2<tins(i,n_pat) .or. thet2>=tins(i+1,n_pat)) cycle
    !           ip = i
    !           dtt = tins(i+1,n_pat) - tins(i,n_pat)
    !           dgg = gins(i+1,n_pat) - gins(i,n_pat)
    !           dcc = cins(i+1,n_pat) - cins(i,n_pat)
    !           ggi(ire,n_pat) = gins(ip,n_pat) + (thet2 - tins(ip,n_pat))*dgg/dtt
    !           ggi(ire,n_pat) = ggi(ire,n_pat)*ggi(ire,n_pat)
    !           cci(ire,n_pat) = cins(ip,n_pat) + (thet2 - tins(ip,n_pat))*dcc/dtt
    !           if(ireso(n_pat) == 8) then
    !              dsl=sl_ins(i+1,n_pat)-sl_ins(i,n_pat)
    !              ddl=dl_ins(i+1,n_pat)-dl_ins(i,n_pat)
    !              sl_i(ire,n_pat)=sl_ins(ip,n_pat)+ (thet2 - tins(ip,n_pat))*dsl/dtt
    !              dl_i(ire,n_pat)=dl_ins(ip,n_pat)+ (thet2 - tins(ip,n_pat))*ddl/dtt
    !           end if
    !           return
    !        end do
    !     End Select
    !
    !end subroutine apply_irf_cw
    !
    !subroutine apply_irf_tof(n_pat,ire,dsp)
    !   integer,       intent (in) :: n_pat,ire
    !   real(kind=cp), intent (in) :: dsp
    !   real(kind=cp)              :: dtt, dgg, dcc
    !   integer                    :: i
    !   !
    !   !  Calculation of Sigma and Gamma for a particular reflection of d-spacing = dsp
    !   !
    !   if(sigam_g(n_pat)) then
    !     if (dsp < tins(1,n_pat)) then
    !        ggi(ire,n_pat) = gins(1,n_pat)
    !        cci(ire,n_pat) = cins(1,n_pat)
    !     else if (dsp >= tins(npoins(n_pat),n_pat)) then
    !        ggi(ire,n_pat) = gins(npoins(n_pat),n_pat)
    !        cci(ire,n_pat) = cins(npoins(n_pat),n_pat)
    !     else
    !        do i = 1, npoins(n_pat) - 1
    !           if (dsp < tins(i,n_pat) .or. dsp >= tins(i+1,n_pat)) cycle
    !           dtt = tins(i+1,n_pat) - tins(i,n_pat)
    !           dgg = gins(i+1,n_pat) - gins(i,n_pat)
    !           dcc = cins(i+1,n_pat) - cins(i,n_pat)
    !           ggi(ire,n_pat) = gins(i,n_pat) + (dsp - tins(i,n_pat))*dgg/dtt
    !           cci(ire,n_pat) = cins(i,n_pat) + (dsp - tins(i,n_pat))*dcc/dtt
    !           exit
    !        end do
    !     end if
    !   end if
    !   !
    !   !  Calculation of alpha and beta for a particular reflection of d-spacing = dsp
    !   !
    !   if(alfbet_g(n_pat)) then
    !     if (dsp < tins(1,n_pat)) then
    !        alfi(ire,n_pat) = alfins(1,n_pat)
    !        beti(ire,n_pat) = betins(1,n_pat)
    !     else if (dsp >= tins(npoins(n_pat),n_pat)) then
    !        alfi(ire,n_pat) = alfins(npoins(n_pat),n_pat)
    !        beti(ire,n_pat) = betins(npoins(n_pat),n_pat)
    !     else
    !        do i = 1, npoins(n_pat) - 1
    !           if (dsp < tins(i,n_pat) .or. dsp >= tins(i+1,n_pat)) cycle
    !           dtt = tins(i+1,n_pat) - tins(i,n_pat)
    !           dgg = alfins(i+1,n_pat) - alfins(i,n_pat)
    !           dcc = betins(i+1,n_pat) - betins(i,n_pat)
    !           alfi(ire,n_pat) = alfins(i,n_pat) + (dsp - tins(i,n_pat))*dgg/dtt
    !           beti(ire,n_pat) = betins(i,n_pat) + (dsp - tins(i,n_pat))*dcc/dtt
    !           exit
    !        end do
    !     end if
    !   end if
    !   !
    !   !  Calculation of Shift for a particular reflection of d-spacing = dsp
    !   !
    !   if(shift_g(n_pat)) then
    !     if (dsp < tins(1,n_pat)) then
    !        sl_i(ire,n_pat) = shiftins(1,n_pat)
    !     else if (dsp >= tins(npoins(n_pat),n_pat)) then
    !        sl_i(ire,n_pat) = shiftins(npoins(n_pat),n_pat)
    !     else
    !        do i = 1, npoins(n_pat) - 1
    !           if (dsp < tins(i,n_pat) .or. dsp >= tins(i+1,n_pat)) cycle
    !           dtt = tins(i+1,n_pat) - tins(i,n_pat)
    !           dgg = shiftins(i+1,n_pat) - shiftins(i,n_pat)
    !           sl_i(ire,n_pat) = shiftins(i,n_pat) + (dsp - tins(i,n_pat))*dgg/dtt
    !           exit
    !        end do
    !     end if
    !   end if
    !
    !End Subroutine apply_irf_tof

End SubModule Pow_IRF
