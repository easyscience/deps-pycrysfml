SubModule (CFML_kvec_Polarimetry) Polar_Write
   implicit none
   Contains
    !!----
    !!----  Module Subroutine Write_Polar_Info(Polari, Mag_Dom, Lun, info)
    !!----    !---- Arguments ----!
    !!----    type(Polar_calc_type),     intent(in)  :: Polari     !  in -> Type with all information about polarisation in one point hkl
    !!----    type(Magnetic_Domain_type),intent(in)  :: Mag_Dom    !  in -> Magnetic domains
    !!----    integer,         optional, intent(in)  :: Lun        !  In -> Unit to write
    !!----    character(len=*),optional, intent(in)  :: info       !  in -> if info "P" also print information about coordinate frame                                                          !        if info "C" also print information about crystal
    !!----                                                         !        if info "B" also print information about both
    !!----
    !!----    Outputs the polarisation info type in nice form
    !!----
    !!---- Created: April - 2005
    !!---- Modified March - 2009 OZ for multidomain case
    !!
    Module Subroutine Write_Polar_Info(Polari, Mag_Dom, Lun, info)
       !---- Arguments ----!
       type(Polar_calc_type),     intent(in)  :: Polari
       type(Magnetic_Domain_type),intent(in)  :: Mag_Dom
       integer,         optional, intent(in)  :: Lun
       character(len=*),optional, intent(in)  :: info
       !---- Local variables ----!
       integer            :: iunit,nd,nch,ich,lfmt_out
       character(32)   :: tit
       character(132)  :: fmt_out

       iunit=6
       if (present(lun)) iunit=lun

       Write(unit=iunit,fmt="(/,a)")            "        Polar information:"
       Write(unit=iunit,fmt="(a,/)")            "        -------------------"
       Write(unit=iunit,fmt="(a,/)")            " => Initial parameters:"
       Write(unit=iunit,fmt="(3(a,f12.3), a)")  "    Scattering vector  (Qh,Qh,Ql) =  (", Polari%h(1) ,", ", Polari%h(2) , &
                                                ", ", Polari%h(3), ")"
       Write(unit=iunit,fmt="(3(a,f12.3), a)")  "    Add. in-plane vector      SPV =  (", Polari%SPV(1) ,", ", Polari%SPV(2) ,&
                                                ", ", Polari%SPV(3), ")"
       Write(unit=iunit,fmt="(a,f12.3)")        "              Polarisation degree =   ", Polari%P
       Write(unit=iunit,fmt="(a,/)")            " "
       IF (present(info)) THEN
         IF (info == "C" .OR. info == "c" .OR. info == "B" .OR. info == "b") THEN
           Write(unit=iunit,fmt="(a,/)")        " => Crystal information:"
           Write(unit=iunit,fmt="(3(a,f12.4))") "      a = ", Polari%Cell%cell(1),"      b = ", Polari%Cell%cell(2), "      c = ",&
                                                Polari%Cell%cell(3)
           Write(unit=iunit,fmt="(3(a,f12.3))") "  alpha = ", Polari%Cell%ang(1) ,"   beta = ", Polari%Cell%ang(2) , "  gamma = ",&
                                                Polari%Cell%ang(3)
           Write(unit=iunit,fmt="(a,f12.4)")    "                     Direct Cell Volume = ",Polari%Cell%Vol
           Write(unit=iunit,fmt="(a,/)")        ""
        End if
         IF (info == "P" .OR. info == "p" .OR. info == "B" .OR. info == "b") THEN
           Write(unit=iunit,fmt="(a,/)")  " => Polarisation coordinate frame:"
           Write(unit=iunit,fmt="(a,/)")  " "
           Write(unit=iunit,fmt="(a,/)")  "    Polarisation coordinate frame according to Blume"
           Write(unit=iunit,fmt="(a,a,/)")"    X  || scattering vector Q    ",&
                                          " (where Q is the scattering Vector in cartesian real space coordinates)"
           Write(unit=iunit,fmt="(a,/)")  "    Y _|_ scattering vector Q in scattering plane"
           Write(unit=iunit,fmt="(a,/)")  "    Z _|_ scattering vector Q out of scattering plane"
           Write(unit=iunit,fmt="(a,/)")  "    (ATTENTION: This choice is not non-ambiguous, there are always two possible choices"
           Write(unit=iunit,fmt="(a,/)")  "    for a right handed coordinate frame which will fullfils this condition!!!)"
           Write(unit=iunit,fmt="(a,/)")  " "
           Write(unit=iunit,fmt="(a,/)")  "    Therefore the right handed coordinate frame is explicitly chosen like this:"
           Write(unit=iunit,fmt="(a,a,/)")"    X := Q/|Q|                 where Q is the scattering Vector in ", &
                                          "cartesian real space coordinates"
           Write(unit=iunit,fmt="(a,a,/)")"    Z := (Q x SVP)/|(Q x SVP)| where SVP is a second vector in the scattering ",&
                                          "plane in cartesian real space coordinates"
           Write(unit=iunit,fmt="(a,/)")  "    Y := (Z x X)"
           Write(unit=iunit,fmt="(a,/)")  " "
           Write(unit=iunit,fmt="(a)")    "                            Y"
           Write(unit=iunit,fmt="(a)")    "                          /|\"
           Write(unit=iunit,fmt="(a)")    "                           |"
           Write(unit=iunit,fmt="(a)")    "                   Q       | Z "
           Write(unit=iunit,fmt="(a)")    "            ____________ _\o_____\ X"
           Write(unit=iunit,fmt="(a)")    "            \             /      /"
           Write(unit=iunit,fmt="(a)")    "             \           / "
           Write(unit=iunit,fmt="(a)")    "              \         /"
           Write(unit=iunit,fmt="(a)")    "               \       /"
           Write(unit=iunit,fmt="(a)")    "                \     /  K_f"
           Write(unit=iunit,fmt="(a)")    "             K_i \   /"
           Write(unit=iunit,fmt="(a)")    "                 _\|/_"
           Write(unit=iunit,fmt="(a,/)")  ""
         END IF
       END IF
       Write(unit=iunit,fmt="(a,/)")         " => Interaction potentials:"
       Write(unit=iunit,fmt="(2(a,f7.3))")   "       NSF = ", real(Polari%NSF)," + i ", AIMAG(Polari%NSF)

       nch=1
       if(Mag_Dom%chir) nch=2
       do nd=1,Mag_Dom%nd
         do ich=1,nch
           Write(unit=iunit,fmt="(2(a,i2),2(a,3f7.3),a)") "   Domain # =",nd," Chiral Dom. =",ich," MiV = (",&
                         real(Polari%MiV(:,ich,nd)), ") + i(",AIMAG(Polari%MiV(:,ich,nd)),")"
         end do
       end do
       Write(unit=iunit,fmt="(a,/)")         " "
       Write(unit=iunit,fmt="(a,/)")         " => Different contributions to the cross-section:"
       Write(unit=iunit,fmt="(a,f12.3)")     "         Nuclear Contribution = ", Polari%NC
       Write(unit=iunit,fmt="(a,/)")         "             Different S-domains (Pop=100%), after : chiral counterparts)"

       nch=1
       if(Mag_Dom%chir) nch=2

       ! format for variable number of domains

       fmt_out(1:3)='(a,'
       write(fmt_out(4:5),'(i2.2)') Mag_Dom%nd

       tit="             Magnetic along y = "
       if(nch==1) then
         fmt_out(6:17)='(f12.3))'
         lfmt_out=17
         write(unit=iunit,fmt=fmt_out(1:lfmt_out))   tit, Polari%MY(1,1:Mag_Dom%nd)
       else if(nch==2) then
         fmt_out(6:16)='(f12.3)'
         fmt_out(17:19)=',a,'
         write(fmt_out(20:21),'(i2.2)') Mag_Dom%nd
         fmt_out(22:29)='(f12.3))'
         lfmt_out=29
         write(unit=iunit,fmt=fmt_out(1:lfmt_out))   tit, Polari%MY(1,1:Mag_Dom%nd),' :',Polari%MY(2,1:Mag_Dom%nd)
       end if

       tit="             Magnetic along z = "
       if(nch==1) then
         fmt_out(6:17)='(f12.3))'
         lfmt_out=17
         write(unit=iunit,fmt=fmt_out(1:lfmt_out))   tit, Polari%MZ(1,1:Mag_Dom%nd)
       else if(nch==2) then
         fmt_out(6:16)='(f12.3)'
         fmt_out(17:19)=',a,'
         write(fmt_out(20:21),'(i2.2)') Mag_Dom%nd
         fmt_out(22:29)='(f12.3))'
         lfmt_out=29
         write(unit=iunit,fmt=fmt_out(1:lfmt_out))   tit, Polari%MZ(1,1:Mag_Dom%nd),' :',Polari%MZ(2,1:Mag_Dom%nd)
       end if

       tit="  Real nuclear magnetic al. y = "
       if(nch==1) then
         fmt_out(6:17)='(f12.3))'
         lfmt_out=17
         write(unit=iunit,fmt=fmt_out(1:lfmt_out))   tit, Polari%RY(1,1:Mag_Dom%nd)
       else if(nch==2) then
         fmt_out(6:16)='(f12.3)'
         fmt_out(17:19)=',a,'
         write(fmt_out(20:21),'(i2.2)') Mag_Dom%nd
         fmt_out(22:29)='(f12.3))'
         lfmt_out=29
         write(unit=iunit,fmt=fmt_out(1:lfmt_out))   tit, Polari%RY(1,1:Mag_Dom%nd),' :',Polari%RY(2,1:Mag_Dom%nd)
       end if

       tit="  Real nuclear magnetic al. z = "
       if(nch==1) then
         fmt_out(6:17)='(f12.3))'
         lfmt_out=17
         write(unit=iunit,fmt=fmt_out(1:lfmt_out))   tit, Polari%RZ(1,1:Mag_Dom%nd)
       else if(nch==2) then
         fmt_out(6:16)='(f12.3)'
         fmt_out(17:19)=',a,'
         write(fmt_out(20:21),'(i2.2)') Mag_Dom%nd
         fmt_out(22:29)='(f12.3))'
         lfmt_out=29
         write(unit=iunit,fmt=fmt_out(1:lfmt_out))   tit, Polari%RZ(1,1:Mag_Dom%nd),' :',Polari%RZ(2,1:Mag_Dom%nd)
       end if

       tit="  Imag nuclear magnetic al. y = "
       if(nch==1) then
         fmt_out(6:17)='(f12.3))'
         lfmt_out=17
         write(unit=iunit,fmt=fmt_out(1:lfmt_out))   tit, Polari%IY(1,1:Mag_Dom%nd)
       else if(nch==2) then
         fmt_out(6:16)='(f12.3)'
         fmt_out(17:19)=',a,'
         write(fmt_out(20:21),'(i2.2)') Mag_Dom%nd
         fmt_out(22:29)='(f12.3))'
         lfmt_out=29
         write(unit=iunit,fmt=fmt_out(1:lfmt_out))   tit, Polari%IY(1,1:Mag_Dom%nd),' :',Polari%IY(2,1:Mag_Dom%nd)
       end if

       tit="  Imag nuclear magnetic al. z = "
       if(nch==1) then
         fmt_out(6:17)='(f12.3))'
         lfmt_out=17
         write(unit=iunit,fmt=fmt_out(1:lfmt_out))   tit, Polari%IZ(1,1:Mag_Dom%nd)
       else if(nch==2) then
         fmt_out(6:16)='(f12.3)'
         fmt_out(17:19)=',a,'
         write(fmt_out(20:21),'(i2.2)') Mag_Dom%nd
         fmt_out(22:29)='(f12.3))'
         lfmt_out=29
         write(unit=iunit,fmt=fmt_out(1:lfmt_out))   tit, Polari%IZ(1,1:Mag_Dom%nd),' :',Polari%IZ(2,1:Mag_Dom%nd)
       end if

       tit="          Chiral contribution = "
       if(nch==1) then
         fmt_out(6:17)='(f12.3))'
         lfmt_out=17
         write(unit=iunit,fmt=fmt_out(1:lfmt_out))   tit, Polari%TC(1,1:Mag_Dom%nd)
       else if(nch==2) then
         fmt_out(6:16)='(f12.3)'
         fmt_out(17:19)=',a,'
         write(fmt_out(20:21),'(i2.2)') Mag_Dom%nd
         fmt_out(22:29)='(f12.3))'
         lfmt_out=29
         write(unit=iunit,fmt=fmt_out(1:lfmt_out))   tit, Polari%TC(1,1:Mag_Dom%nd),' :',Polari%TC(2,1:Mag_Dom%nd)
       end if

       tit="            Magnetic Magnetic = "
       if(nch==1) then
         fmt_out(6:17)='(f12.3))'
         lfmt_out=17
         write(unit=iunit,fmt=fmt_out(1:lfmt_out))   tit, Polari%MM(1,1:Mag_Dom%nd)
       else if(nch==2) then
         fmt_out(6:16)='(f12.3)'
         fmt_out(17:19)=',a,'
         write(fmt_out(20:21),'(i2.2)') Mag_Dom%nd
         fmt_out(22:29)='(f12.3))'
         lfmt_out=29
         write(unit=iunit,fmt=fmt_out(1:lfmt_out))   tit, Polari%MM(1,1:Mag_Dom%nd),' :',Polari%MM(2,1:Mag_Dom%nd)
       end if

       Write(unit=iunit,fmt="(a,/)")   " "
       Write(unit=iunit,fmt="(a,/)")   " => Cross-section for initial polar vector:"
       Write(unit=iunit,fmt="(a,/)")   "             Different S-domains (Pop=100%), after : chiral counterparts)"


       tit="                      along x = "
       if(nch==1) then
         fmt_out(6:17)='(f12.3))'
         lfmt_out=17
         write(unit=iunit,fmt=fmt_out(1:lfmt_out))   tit, Polari%CS(1,1,1:Mag_Dom%nd)
       else if(nch==2) then
         fmt_out(6:16)='(f12.3)'
         fmt_out(17:19)=',a,'
         write(fmt_out(20:21),'(i2.2)') Mag_Dom%nd
         fmt_out(22:29)='(f12.3))'
         lfmt_out=29
         write(unit=iunit,fmt=fmt_out(1:lfmt_out))   tit, Polari%CS(1,1,1:Mag_Dom%nd),' :',Polari%CS(1,2,1:Mag_Dom%nd)
       end if
       tit="                      along y = "
       if(nch==1) then
         fmt_out(6:17)='(f12.3))'
         lfmt_out=17
         write(unit=iunit,fmt=fmt_out(1:lfmt_out))   tit, Polari%CS(2,1,1:Mag_Dom%nd)
       else if(nch==2) then
         fmt_out(6:16)='(f12.3)'
         fmt_out(17:19)=',a,'
         write(fmt_out(20:21),'(i2.2)') Mag_Dom%nd
         fmt_out(22:29)='(f12.3))'
         lfmt_out=29
         write(unit=iunit,fmt=fmt_out(1:lfmt_out))   tit, Polari%CS(2,1,1:Mag_Dom%nd),' :',Polari%CS(2,2,1:Mag_Dom%nd)
       end if
       tit="                      along z = "
       if(nch==1) then
         fmt_out(6:17)='(f12.3))'
         lfmt_out=17
         write(unit=iunit,fmt=fmt_out(1:lfmt_out))   tit, Polari%CS(3,1,1:Mag_Dom%nd)
       else if(nch==2) then
         fmt_out(6:16)='(f12.3)'
         fmt_out(17:19)=',a,'
         write(fmt_out(20:21),'(i2.2)') Mag_Dom%nd
         fmt_out(22:29)='(f12.3))'
         lfmt_out=29
         write(unit=iunit,fmt=fmt_out(1:lfmt_out))   tit, Polari%CS(3,1,1:Mag_Dom%nd),' :',Polari%CS(3,2,1:Mag_Dom%nd)
       end if

       Write(unit=iunit,fmt="(a,/)")           " "
       Write(unit=iunit,fmt="(a,/)")           " => Polarisation tensor as it will be measured:"
       Write(unit=iunit,fmt="(a,/)")           " "
       Write(unit=iunit,fmt="(3(a,f12.4), a)") "         /", Polari%Pij(1,1),"  ", Polari%Pij(1,2) , "  ", Polari%Pij(1,3), "  \"
       Write(unit=iunit,fmt="(3(a,f12.4), a)") "  PT  = | ", Polari%Pij(2,1),"  ", Polari%Pij(2,2) , "  ", Polari%Pij(2,3), "   |"
       Write(unit=iunit,fmt="(3(a,f12.4), a)") "         \", Polari%Pij(3,1),"  ", Polari%Pij(3,2) , "  ", Polari%Pij(3,3), "  /"

    End Subroutine Write_Polar_Info

    !!----
    !!---- Module Subroutine Write_Polar_Line(Polari, Lun)
    !!----    Type (Polar_calc_type), intent( in)     :: Polrari !  in ->type with all information about polarization in one point hkl
    !!----    integer, optional,      intent(in)      :: lun     !  In -> Unit to write
    !!----
    !!----    Outputs the polarization info type in line form, so you can write it to a file
    !!----
    !!---- Update: May - 2005
    !!
    Module Subroutine Write_Polar_Line(Polari, Lun)
       !---- Arguments ----!
       Type (Polar_calc_type), intent( in)     :: Polari !
       integer, optional,      intent(in)      :: Lun

       !---- Local variables ----!
       integer            :: iunit

       iunit=6
       if (present(lun)) iunit=lun

       Write(unit=iunit,fmt="(/,a)")        "     H         K         L         NSF^2     NSF_r     NSF_i"
       Write(unit=iunit,fmt="(6(f10.6))")   Polari%H(1), Polari%H(2), Polari%H(3), Polari%NC, real(Polari%NSF), AIMAG(Polari%NSF)
       Write(unit=iunit,fmt="(/,a)")        "       Pix       Piy       Piz       Pfx       Pfy       Pfz"
       Write(unit=iunit,fmt="(6(f10.3))")   Polari%P, 0.0, 0.0, Polari%Pij(1,1), Polari%Pij(2,1), Polari%Pij(3,1)
       Write(unit=iunit,fmt="(6(f10.3))")   0.0,Polari%P, 0.0, Polari%Pij(1,2), Polari%Pij(2,2), Polari%Pij(3,2)
       Write(unit=iunit,fmt="(6(f10.3))")   0.0, 0.0, Polari%P, Polari%Pij(1,3), Polari%Pij(2,3), Polari%Pij(3,3)
       Write(unit=iunit,fmt="(a,/)")        "-------------------------------------------------------------------------"

    End Subroutine Write_Polar_line

End SubModule Polar_Write