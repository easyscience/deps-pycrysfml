SubModule(CFML_Powder) Pow_Microstructure

   implicit none

   contains

   Module Subroutine Calc_Anisotropic_Strain(str_model,Laue,mode,par,hkl,sq,dst2,dtt,pst,cell,der)
     character(len=*),                      intent(in)   :: str_model  !Model for strains: QUARTIC_FORM, UNIAXIAL_STRAIN, SPHERICAL_HARMONICS
     character(len=*),                      intent(in)   :: Laue !> Laue class
     character(len=*),                      intent(in)   :: mode !> CW or TF (Constant Wavelength or Time-of-Flight)
     real(kind=cp), dimension(:),           intent(in)   :: par  !> Values of parameters
     real(kind=cp), dimension(3),           intent(in)   :: hkl  !> (h,k,l)
     real(kind=cp),                         intent(in)   :: sq   !> 1/d^2
     real(kind=cp),                         intent(out)  :: dst2 !> Contribution to broadening due to strain effects
     real(kind=cp),                optional,intent(in)   :: dtt  !> dtt1 for TOF case
     real(kind=cp), dimension(4),  optional,intent(in)   :: pst  !> [first three components is the axis for uniaxial strain, 4 component is the modulus]
     class(cell_G_type),           optional,intent(in)   :: cell !> cell (Needed for spherical harmonics and uniaxial strains)
     real(kind=cp), dimension(:),  optional,intent(out)  :: der  !> Derivatives of dst2 w.r.t. the provided parameters

     real(kind=cp)               :: h,k,l, prmul, pr, ds,dst, scatv,xx,yy,zz
     real(kind=cp)               :: h400, h040, h004,  h220, h202, h022,  h211, h121, h112, &
                                    h310, h301, h130,  h103, h013, h031
     real(kind=cp), dimension(3) :: uv
     real(kind=cp), dimension(9) :: dv
     integer                     :: i, i_fin

     h=hkl(1); k=hkl(2); l=hkl(3)

     dst2=0.0

     !DST2= prmul* (sigma(M)/M)^2,   prmul= 1.0E-08 * 8Ln2 * (180/PI)**2
     Select Case (trim(mode))
        Case("CW")
           prmul=1.820374372668E-04
        Case("TF")         !
           if(present(dtt)) then
             prmul=0.25 * dtt * dtt * 1.0E-08  !prmul=1/4 * 1.0E-08 * Dtt^2
           else
             Call Set_Error(1,"For Time-of-Flight the optional argument dtt should be present!")
             return
           end if
     End Select

     Select Case (trim(u_case(str_model)))
        !> Model based in JRC et al., J.Phys.Cond. Matter 3 (1991), 3215-3234
        !> Sigma^2(M_hkl) = Sum{i,j}{ S_ij DM/Da_i DM/Da_j }, being M=1/D^2 (= sq here)
        !> Sigma^2(2Theta)_strain = [Sigma^2(M_hkl)/M_hkl^2] tan^(theta)
        !> Notation by P. Stephens, J.App.Cryst. 32 (1999) 281-289:
        !>    dst2= prmul* d^4 * Sum{H+K+L=4} { S_HKL h^H k^K l^L}
        Case ("QUARTIC_FORM")
           Select case(trim(Laue))
             case("-1")  ! -1      All 15 parameters are free
                 ! Order:
                 !   S_400         S_040         S_004         S_220         S_202
                 !   S_022         S_211         S_121         S_112         S_310
                 !   S_301         S_130         S_103         S_013         S_031
              h400=h*h*h*h
              h040=k*k*k*k
              h004=l*l*l*l
              h220=h*h*k*k
              h202=h*h*l*l
              h022=k*k*l*l
              h211=h*h*k*l
              h121=h*k*k*l
              h112=h*k*l*l
              h310=h*h*h*k
              h301=h*h*h*l
              h130=h*k*k*k
              h103=h*l*l*l
              h013=k*l*l*l
              h031=k*k*k*l
              dst2=par(1)*h400 + par(2)*h040 + par(3)*h004  &
                  +par(4)*h220 + par(5)*h202 + par(6)*h022  &
                  +par(7)*h211 + par(8)*h121 + par(9)*h112  &
                  +par(10)*h310 + par(11)*h301 + par(12)*h130  &
                  +par(13)*h103 + par(14)*h013 + par(15)*h031
              pr=prmul/(sq*sq)
              dst2=pr*dst2
              if(present(der)) then
                der( 1)=pr*h400
                der( 2)=pr*h040
                der( 3)=pr*h004
                der( 4)=pr*h220
                der( 5)=pr*h202
                der( 6)=pr*h022
                der( 7)=pr*h211
                der( 8)=pr*h121
                der( 9)=pr*h112
                der(10)=pr*h310
                der(11)=pr*h301
                der(12)=pr*h130
                der(13)=pr*h103
                der(14)=pr*h013
                der(15)=pr*h031
              end if
              return

             case("2/m")  ! 2/m  //b    S_400,S_040,S_004,S_220,S_202,S_022,S_121,S_301,S_103   (9 parameters)
                ! Order:
                !   S_400         S_040         S_004         S_220         S_202
                !   S_022         S_121         S_301         S_103
              h400=h*h*h*h   !  18
              h040=k*k*k*k   !  19
              h004=l*l*l*l   !  20
              h220=h*h*k*k   !  21
              h202=h*h*l*l   !  22
              h022=k*k*l*l   !  23
              h121=h*k*k*l   !  24
              h301=h*h*h*l   !  25
              h103=h*l*l*l   !  26
              dst2=par(1)*h400 + par(2)*h040 + par(3)*h004  &
                  +par(4)*h220 + par(5)*h202 + par(6)*h022  &
                  +par(7)*h121 + par(8)*h301 + par(9)*h103
              pr=prmul/(sq*sq)
              dst2=pr*dst2
              if(present(der)) then
                der( 1)=pr*h400
                der( 2)=pr*h040
                der( 3)=pr*h004
                der( 4)=pr*h220
                der( 5)=pr*h202
                der( 6)=pr*h022
                der( 7)=pr*h121
                der( 8)=pr*h301
                der( 9)=pr*h103
              end if
              return

             case("2/m c")  ! 2/m  //c    S_400,S_040,S_004,S_220,S_202,S_022,S_112,S_310,S_130   (9 parameters)
                !Order:
                !   S_400         S_040         S_004         S_220         S_202
                !   S_022         S_112         S_310         S_130
              h400=h*h*h*h   !  18
              h040=k*k*k*k   !  19
              h004=l*l*l*l   !  20
              h220=h*h*k*k   !  21
              h202=h*h*l*l   !  22
              h022=k*k*l*l   !  23
              h112=h*k*l*l   !  24
              h310=h*h*h*k   !  25
              h130=h*k*k*k   !  26
              dst2=par(1)*h400 + par(2)*h040 + par(3)*h004  &
                  +par(4)*h220 + par(5)*h202 + par(6)*h022  &
                  +par(7)*h112 + par(8)*h310 + par(9)*h130
              pr=prmul/(sq*sq)
              dst2=pr*dst2
              if(present(der)) then
                der( 1)=pr*h400
                der( 2)=pr*h040
                der( 3)=pr*h004
                der( 4)=pr*h220
                der( 5)=pr*h202
                der( 6)=pr*h022
                der( 7)=pr*h112
                der( 8)=pr*h310
                der( 9)=pr*h130
              end if
              return

             case("mmm")  ! mmm   S_400,S_040,S_004,S_220,S_202,S_022   (6 parameters)
                !Order:
                !   S_400        S_040        S_004        S_220        S_202        S_022
              h400=h*h*h*h   !  18
              h040=k*k*k*k   !  19
              h004=l*l*l*l   !  20
              h220=h*h*k*k   !  21
              h202=h*h*l*l   !  22
              h022=k*k*l*l   !  23
              dst2=par(1)*h400 + par(2)*h040 + par(3)*h004  &
                  +par(4)*h220 + par(5)*h202 + par(6)*h022
              pr=prmul/(sq*sq)
              dst2=pr*dst2
              if(present(der)) then
                der( 1)=pr*h400
                der( 2)=pr*h040
                der( 3)=pr*h004
                der( 4)=pr*h220
                der( 5)=pr*h202
                der( 6)=pr*h022
              end if
              return

             case("4/m")  ! 4/m    S_400=S_040,S_004,S_220,S_202=S_022   (4 parameters)
                !Order:
                !   S_400         S_004         S_220         S_202
              h400=h*h*h*h   !  18
              h040=k*k*k*k   !  18
              h004=l*l*l*l   !  19
              h220=h*h*k*k   !  20
              h202=h*h*l*l   !  21
              h022=k*k*l*l   !  21
              dst2=par(1)*(h400+h040) + par(2)*h004+ par(3)*h220  &
                  +par(4)*(h202+h022)
              pr=prmul/(sq*sq)
              dst2=pr*dst2
              if(present(der)) then
                der( 1)=pr*(h400+h040)
                der( 2)=pr*h004
                der( 3)=pr*h220
                der( 4)=pr*(h202+h022)
              end if
              return

             case("4/mmm")  ! 4/mmm    S_400=S_040,S_004,S_220,S_202=S_022   (4 parameters)
                !Order:
                !   S_400         S_004         S_220         S_202
              h400=h*h*h*h   !  18
              h040=k*k*k*k   !  18
              h004=l*l*l*l   !  19
              h220=h*h*k*k   !  20
              h202=h*h*l*l   !  21
              h022=k*k*l*l   !  21
              dst2=par(1)*(h400+h040) + par(2)*h004+ par(3)*h220  &
                  +par(4)*(h202+h022)
              pr=prmul/(sq*sq)
              dst2=pr*dst2
              if(present(der)) then
                der( 1)=pr*(h400+h040)
                der( 2)=pr*h004
                der( 3)=pr*h220
                der( 4)=pr*(h202+h022)
              end if
              return

             case("-3 R")  ! -3 R     (1) S_400=S_040=S_310/2=S_130/2=S_220/3 ; (2) S_004
                       !              (3) S_202=S_022=S_112 ;  (4) S_301/2=-S_031/2=S_211/3=-S_121/3
                !Order:  S_400         S_004         S_112         S_211
              h400=h*h*h*h       !  18
              h040=k*k*k*k       !  18
              h310=2.0*h*h*h*k   !  18
              h130=2.0*h*k*k*k   !  18
              h220=3.0*h*h*k*k   !  18
              h004=l*l*l*l       !  19
              h202=h*h*l*l       !  20
              h022=k*k*l*l       !  20
              h112=h*k*l*l       !  20
              h211= h*h*k*l      !  21
              h121=-h*k*k*l      !  21
              h301= h*h*h*l/1.5  !  21
              h031=-k*k*k*l/1.5  !  21
              dst2=par(1)*(h400+h040+h310+h130+h220) + par(2)*h004  &
                  +par(3)*(h202+h022+h112) + par(4)*(h211+h121+h301+h031)
              pr=prmul/(sq*sq)
              dst2=pr*dst2
              if(present(der)) then
                der( 1)=pr*(h400+h040+h310+h130+h220)
                der( 2)=pr*h004
                der( 3)=pr*(h202+h022+h112)
                der( 4)=pr*(h211+h121+h301+h031)
              end if
              return

             case("-3m R")  ! -3m R
                !Order:  S_400         S_004         S_112         S_211
              h400=h*h*h*h       !  18
              h040=k*k*k*k       !  18
              h310=2.0*h*h*h*k   !  18
              h130=2.0*h*k*k*k   !  18
              h220=3.0*h*h*k*k   !  18
              h004=l*l*l*l       !  19
              h202=h*h*l*l       !  20
              h022=k*k*l*l       !  20
              h112=h*k*l*l       !  20
              h211= h*h*k*l      !  21
              h121=-h*k*k*l      !  21
              h301= h*h*h*l/1.5  !  21
              h031=-k*k*k*l/1.5  !  21

              dst2=par(1)*(h400+h040+h310+h130+h220) + par(2)*h004  &
                  +par(3)*(h202+h022+h112) + par(4)*(h211+h121+h301+h031)
              pr=prmul/(sq*sq)
              dst2=pr*dst2
              if(present(der)) then
                der( 1)=pr*(h400+h040+h310+h130+h220)
                der( 2)=pr*h004
                der( 3)=pr*(h202+h022+h112)
                der( 4)=pr*(h211+h121+h301+h031)
              end if
              return

             case("-3 H", "-3")  ! -3 H
                !Order:    S_400         S_004         S_112
              h400=h*h*h*h       !  18
              h040=k*k*k*k       !  18
              h310=2.0*h*h*h*k   !  18
              h130=2.0*h*k*k*k   !  18
              h220=3.0*h*h*k*k   !  18
              h004=l*l*l*l   !  19
              h202=h*h*l*l   !  20
              h022=k*k*l*l   !  20
              h112=h*k*l*l   !  20
              dst2=par(1)*(h400+h040+h310+h130+h220) + par(2)*h004  &
                  +par(3)*(h202+h022+h112)
              pr=prmul/(sq*sq)
              dst2=pr*dst2
              if(present(der)) then
                der( 1)=pr*(h400+h040+h310+h130+h220)
                der( 2)=pr*h004
                der( 3)=pr*(h202+h022+h112)
              end if
              return

             case("-3m1")  ! -3m1
                !Order:    S_400         S_004         S_112
              h400=h*h*h*h       !  18
              h040=k*k*k*k       !  18
              h310=2.0*h*h*h*k   !  18
              h130=2.0*h*k*k*k   !  18
              h220=3.0*h*h*k*k   !  18

              h004=l*l*l*l   !  19

              h202=h*h*l*l   !  20
              h022=k*k*l*l   !  20
              h112=h*k*l*l   !  20

              dst2=par(1)*(h400+h040+h310+h130+h220) + par(2)*h004  &
                  +par(3)*(h202+h022+h112)
              pr=prmul/(sq*sq)
              dst2=pr*dst2
              if(present(der)) then
                der( 1)=pr*(h400+h040+h310+h130+h220)
                der( 2)=pr*h004
                der( 3)=pr*(h202+h022+h112)
              end if
              return

             case("-31m")  ! -31m
                !Order:    S_400         S_004         S_112
              h400=h*h*h*h       !  18
              h040=k*k*k*k       !  18
              h310=2.0*h*h*h*k   !  18
              h130=2.0*h*k*k*k   !  18
              h220=3.0*h*h*k*k   !  18

              h004=l*l*l*l   !  19

              h202=h*h*l*l   !  20
              h022=k*k*l*l   !  20
              h112=h*k*l*l   !  20

              dst2=par(1)*(h400+h040+h310+h130+h220) + par(2)*h004  &
                  +par(3)*(h202+h022+h112)
              pr=prmul/(sq*sq)
              dst2=pr*dst2
              if(present(der)) then
                der( 1)=pr*(h400+h040+h310+h130+h220)
                der( 2)=pr*h004
                der( 3)=pr*(h202+h022+h112)
              end if
              Return

             case("6/m")  ! 6/m
                !Order:    S_400         S_004         S_112
              h400=h*h*h*h       !  18
              h040=k*k*k*k       !  18
              h310=2.0*h*h*h*k   !  18
              h130=2.0*h*k*k*k   !  18
              h220=3.0*h*h*k*k   !  18

              h004=l*l*l*l   !  19

              h202=h*h*l*l   !  20
              h022=k*k*l*l   !  20
              h112=h*k*l*l   !  20

              dst2=par(1)*(h400+h040+h310+h130+h220) + par(2)*h004  &
                  +par(3)*(h202+h022+h112)
              pr=prmul/(sq*sq)
              dst2=pr*dst2
              if(present(der)) then
                der( 1)=pr*(h400+h040+h310+h130+h220)
                der( 2)=pr*h004
                der( 3)=pr*(h202+h022+h112)
              end if
              return

             case("6/mmm")  ! 6/mmm
                !Order:    S_400         S_004         S_112
              h400=h*h*h*h       !  18
              h040=k*k*k*k       !  18
              h310=2.0*h*h*h*k   !  18
              h130=2.0*h*k*k*k   !  18
              h220=3.0*h*h*k*k   !  18

              h004=l*l*l*l   !  19

              h202=h*h*l*l   !  20
              h022=k*k*l*l   !  20
              h112=h*k*l*l   !  20

              dst2=par(1)*(h400+h040+h310+h130+h220) + par(2)*h004  &
                  +par(3)*(h202+h022+h112)
              pr=prmul/(sq*sq)
              dst2=pr*dst2
              if(present(der)) then
                der( 1)=pr*(h400+h040+h310+h130+h220)
                der( 2)=pr*h004
                der( 3)=pr*(h202+h022+h112)
              end if
              return

             case("m3")  ! m3
                !Order:    S_400         S_220
              h400=h*h*h*h   !  18
              h040=k*k*k*k   !  18
              h004=l*l*l*l   !  18

              h220=h*h*k*k   !  19
              h202=h*h*l*l   !  19
              h022=k*k*l*l   !  19

              dst2=par(1)*(h400+h040+h004)+ par(2)*(h202+h022+h220)
              pr=prmul/(sq*sq)
              dst2=pr*dst2
              if(present(der)) then
                der( 1)=pr*(h400+h040+h004)
                der( 2)=pr*(h202+h022+h220)
              end if
              return

             case("m3m")  ! m3m
                !Order:    S_400         S_220
              h400=h*h*h*h   !  18
              h040=k*k*k*k   !  18
              h004=l*l*l*l   !  18

              h220=h*h*k*k   !  19
              h202=h*h*l*l   !  19
              h022=k*k*l*l   !  19

              dst2=par(1)*(h400+h040+h004)+ par(2)*(h202+h022+h220)
              pr=prmul/(sq*sq)
              dst2=pr*dst2
              if(present(der)) then
                der( 1)=pr*(h400+h040+h004)
                der( 2)=pr*(h202+h022+h220)
              end if
              return
           End Select

        Case("SPHERICAL_HARMONICS")

           If(present(cell)) then
              scatv = SQRT(sq)
              uv=cart_vector('r',hkl,cell)/scatv
           Else
              Call Set_Error(1,"Uniaxial anisotropic strain need a input the cell object!")
              return
           End if

            xx=uv(1)
            yy=uv(2)
            zz=uv(3)

           Select case(trim(Laue))
             case("-1")  ! -1      6 free param
                  ! Compute Y00,Y20,Y21+,Y21-,Y22+,Y22-  in that order
                  dv(1) = 1.0
                  dv(2) = 0.5*(3.0*zz**2-1.0)
                  dv(3) = 2.0*zz*xx
                  dv(4) = 2.0*zz*yy
                  dv(5) = xx**2-yy**2
                  dv(6) = 2.0*xx*yy
                  i_fin=6

             case("2/m")  ! 2/m      9 free parameters
                  yy=uv(1)
                  zz=uv(2)
                  xx=uv(3)
                  ! Compute Y00,Y20,Y22+,Y22-,Y40,Y42+,Y42-,Y44+,Y44- in that order
                  dv(1) = 1.0
                  dv(2) = 0.5*(3.0*zz**2-1.0)
                  dv(3) = xx**2-yy**2
                  dv(4) = 2.0*xx*yy
                  dv(5) = 0.12500*(35.0*zz**4-30.0*zz**2+3.0)
                  dv(6) = 0.77778*(7.0*zz**2-1.0)*dv(3)
                  dv(7) = 0.77778*(7.0*zz**2-1.0)*dv(4)
                  dv(8) = dv(3)**2-dv(4)**2
                  dv(9) = 2.0*dv(3)*dv(4)
                  i_fin=9

             case("2/m c")   ! 1 1 2/m
                  ! Compute Y00,Y20,Y22+,Y22-,Y40,Y42+,Y42-,Y44+,Y44- in that order
                  dv(1) = 1.0
                  dv(2) = 0.5*(3.0*zz**2-1.0)
                  dv(3) = xx**2-yy**2
                  dv(4) = 2.0*xx*yy
                  dv(5) = 0.12500*(35.0*zz**4-30.0*zz**2+3.0)
                  dv(6) = 0.77778*(7.0*zz**2-1.0)*dv(3)
                  dv(7) = 0.77778*(7.0*zz**2-1.0)*dv(4)
                  dv(8) = dv(3)**2-dv(4)**2
                  dv(9) = 2.0*dv(3)*dv(4)
                  i_fin=9

             case("mmm")   ! m m m
                  ! Compute Y00,Y20,Y22+,Y40,Y42+,Y44+ in that order
                  dv(1) = 1.0
                  dv(2) = 0.5*(3.0*zz**2-1.0)
                  dv(3) = xx**2-yy**2
                  dv(4) = 0.125*(35.0*zz**4-30*zz**2+3.0)
                  dv(5) = 0.77778*(7*zz**2-1.0)*dv(3)
                  dv(6) = dv(3)**2-4.0*xx**2*yy**2
                  i_fin=6

             case("4/m")   ! 4/m   Tetragonal - Spacegroups 75-88
                        !       Ylm's up to 6th order:
                       !       Y00,Y20,Y40,Y44+,Y44-,Y60,Y64+,Y64-
                  dv(1) = 1.0
                  dv(2) = 0.5*(3.0*zz**2-1.0)
                  dv(3) = 0.125*(35.0*zz**4-30*zz**2+3.0)
                  dv(4) = (xx**4+yy**4-6.0*xx*yy)
                  dv(5) = 4.0*xx*yy*(xx**2-yy**2)
                  dv(6) = 231.0*zz**6-315.0*zz**4+105.0*zz**2-5.0
                  dv(6) = 0.06250*dv(6)
                  dv(7) = 0.81675*(11.0*zz**2-1.0)*dv(4)
                  dv(8) = 0.81675*(11.0*zz**2-1.0)*dv(5)
                  i_fin=8

             case("4/mmm")   ! 4/m m m  Tetragonal - Spacegroups 89-142
                        !          Ylm's up to 6th order:
                        !          Y00,Y20,Y40,Y44+,Y60,Y64+
                  dv(1) = 1.0
                  dv(2) = 0.5*(3.0*zz**2-1.0)
                  dv(3) = 0.125*(35.0*zz**4-30*zz**2+3.0)
                  dv(4) = (xx**4+yy**4-6.0*xx*yy)
                  dv(5) = 0.06250*(231.0*zz**6-315.0*zz**4+105.0*zz**2-5.0)
                  dv(6) = 0.81675*(11.0*zz**2-1.0)*dv(4)
                  i_fin=6

             case("-3 R")   ! -3 R    Ylm's up to 4th order:  Y00,Y20,Y40,Y43+,Y43-
                  ! Compute Y00,Y20,Y40,Y43+,Y43- in that order
                  dv(1) = 1.0
                  dv(2) = 0.5*(3.0*zz*zz-1.0)
                  dv(3) = 0.125*(35.0*zz**4-30.0*zz**2+3.0)
                  dv(4) = 3.07920*xx*zz*(xx**2-3.0*yy**2)
                  dv(5) = 3.07920*yy*zz*(3.0*xx**2-yy**2)
                  i_fin=5

             case( "-3m1","-31m")   ! -3 m R (Hexagonal setting, unique axis c, up to 6-th order)  ! -3 m 1 ! -3 1 m
                  ! Ylm's up to 6th order:  Y00,Y20,Y40,Y43-,Y60,Y63-,Y66+
                  !  Compute Y00,Y20,Y40,Y43-,Y60,Y63-,Y66+ in that order
                  dv(1) = 1.0
                  dv(2) = 0.5*(3.0*zz*zz-1.0)
                  dv(3) = 0.125*(35.0*zz*zz*zz*zz - 30.0*zz*zz + 3.0)
                  dv(4) = 3.07920*yy*zz*(3.0*xx*xx-yy*yy)
                  dv(5) = 231.0*zz*zz*zz*zz*zz*zz-315.0*zz*zz*zz*zz+105.0*zz*zz-5.0
                  dv(5) = 0.06250*dv(5)
                  dv(6) = 1.41685*(11.0*zz*zz-3.0)*yy*zz*(3.0*xx*xx-yy*yy)
                  dv(7) = (xx*xx-yy*yy)*(xx*xx*xx*xx+yy*yy*yy*yy-14.0*xx*xx*yy*yy)
                  i_fin=7

             case("6/m")   ! 6/m  spacegroups 168-176  Ylm's up to 6th order: Y00,Y20,Y40,Y60,Y66+,Y66-
                  !  Compute Y00,Y20,Y40,Y60,Y66+,Y66- in that order
                  dv(1) = 1.0
                  dv(2) = 0.5*(3.0*zz**2-1.0)
                  dv(3) = 0.125*(35.0*zz**4-30.0*zz**2+3.0)
                  dv(4) = 231.0*zz**6-315.0*zz**4+105.0*zz**2-5.0
                  dv(4) = 0.06250*dv(5)
                  dv(5) = (xx**2-yy**2)*(xx**4+yy**4-14.0*xx**2*yy**2)
                  dv(6) = 3.0*(xx**4+yy**4)-10.0*xx**2*yy**2
                  dv(6) = 2.0*xx*yy*dv(6)
                  i_fin=6

             case("6/mmm")   ! 6/m m m spacegroups 177-194 Ylm's up to 6th order: Y00,Y20,Y40,Y60,Y66+
                  !  Compute Y00,Y20,Y40,Y60,Y66+ in that order
                  dv(1) = 1.0
                  dv(2) = 0.5*(3.0*zz**2-1.0)
                  dv(3) = 0.125*(35.0*zz**4-30.0*zz**2+3.0)
                  dv(4) = 231.0*zz**6-315.0*zz**4+105.0*zz**2-5.0
                  dv(4) = 0.06250*dv(5)
                  dv(5) = (xx**2-yy**2)*(xx**4+yy**4-14.0*xx**2*yy**2)
                  i_fin=5

             case("m3")   ! m 3  spacegroups 195-206
                        ! Cubic harmonics Klm's up to 8th order: K00,K41,K61,K62,K81
                  dv(1) =  1.0
                  dv(2) =  2.5    *(xx**4 + yy**4 + zz**4) -  1.5
                  dv(3) = 21.65625*(xx**6 + yy**6 + zz**6) - 29.53125*(xx**4 + yy**4 + zz**4) + 8.43750
                  dv(4) = xx**4*yy**2 - yy**4*xx**2 + yy**4*zz**2 - zz**4*yy**2 + zz**4*xx**2 - xx**4*zz**2
                  dv(4) = dv(4)*6.0*sqrt(3.0)
                  dv(5) =  97.5*(xx**8 + yy**8 + zz**8)- 182.0*(xx**6 + yy**6 + zz**6)+ 105.0*(xx**4 + yy**4 + zz**4)-  17.5
                  dv(5) = dv(5)/3.0
                  i_fin=5

             case("m3m")   ! m 3 m  spacegroups 207-230
                        ! Cubic harmonics Klm's up to 8th order: K00,K41,K61,K81
                  dv(1) =  1.0
                  dv(2) =  2.5    *(xx**4 + yy**4 + zz**4) -  1.5
                  dv(3) = 21.65625*(xx**6 + yy**6 + zz**6) - 29.53125*(xx**4 + yy**4 + zz**4) + 8.43750
                  dv(4) =  97.5*(xx**8 + yy**8 + zz**8)- 182.0*(xx**6 + yy**6 + zz**6)+ 105.0*(xx**4 + yy**4 + zz**4)-  17.5
                  dv(4) = dv(4)/3.0
                  dst=0.0
                  i_fin=4

           End Select
            ! Compute FHWM
            dst=0.0
            DO i=1,i_fin  !
              dst = dst+dv(i)*par(i)
            END DO
            pr=prmul*sq*sq  ! Here we should multiply by sq^2 because the calculation is
                            ! done with directions
            dst2=prmul*dst*dst
            if(present(der)) then
              der(1:i_fin)=2.0*dst*dv(i_fin)*pr
            end if
            return

        !------------------------------------------------------------
        ! [S1,S2,S3,S4]=PST   Uniaxial strain along h=[S1,S2,S3] S4=h.h
        !------------------------------------------------------------
        Case("UNIAXIAL_STRAIN")
           If(present(pst)) then
              If(present(cell)) then
                 ds=dot_product(pst(1:3), matmul(cell%Gr,hkl))
              Else
                 Call Set_Error(1,"Uniaxial anisotropic strain needs, as input, the CELL object!")
                 return
              End if
           Else
              Call Set_Error(1,"Uniaxial anisotropic strain needs as input the reciprocal space direction PST!")
              return
           End if
           ds=ds*ds/(pst(4)*sq)
           dst=ds*par(1)
           if(present(der)) der(1)=2.0*dst*ds*prmul
           dst2=dst*dst*prmul
           Return

       Case Default
          Call Set_Error(1,"The provided model for anisotropic strain is not supported!")
          return

     End Select

   End Subroutine Calc_Anisotropic_Strain

   Module Subroutine Calc_Anisotropic_Size(size_model,Laue,mode,par,hkl,sq,wav,dsiz,psz,nv,cell,der)
     character(len=*),                      intent(in)   :: size_model  !Model for size: !Model for size: "QUADRATIC_FORM", "NEEDLE", "PLATELET", "SPHERICAL_HARMONICS", "HKL RULE", "HKL_CONDIT"
     character(len=*),                      intent(in)   :: Laue !> Laue class
     character(len=*),                      intent(in)   :: mode !> CW or TF (Constant Wavelength or Time-of-Flight)
     real(kind=cp), dimension(:),           intent(in)   :: par  !> Values of parameters
     real(kind=cp), dimension(3),           intent(in)   :: hkl  !> (h,k,l)
     real(kind=cp),                         intent(in)   :: sq   !> 1/d^2
     real(kind=cp),                         intent(in)   :: wav  !> Lambda for CW or dtt1 for TOF case
     real(kind=cp),                         intent(out)  :: dsiz !> Contribution to broadening due to size effects
     real(kind=cp), dimension(4),  optional,intent(in)   :: psz  !> [first three components is the axis for uniaxial size (Platelet/Needle), 4 component is the modulus squared]
     integer      , dimension(:,:),optional,intent(in)   :: nv   !> (5,:) Sets of 5 integers for the size model HKL_CONDIT: n1 H + n2 K + n3 L = n4 n + n5
     class(cell_G_type),           optional,intent(in)   :: cell !> cell (Needed for spherical harmonics and uniaxial size)
     real(kind=cp), dimension(:),  optional,intent(out)  :: der  !> Derivatives of dsiz w.r.t. the provided parameters

      !--- Local variables ---!
      Character(len=:), allocatable :: size_mod  !Upper case size_model without complement
      Character(len=:), allocatable :: size_comp !Complement of size_model e.g.  HKL  00L
      real(kind=cp), dimension(3)   :: uv
      real(kind=cp), dimension(21)  :: dv !auxiliar derivatives / spherical harmonics
      real(kind=cp) :: h2,k2,l2,kl,ds,scatv,xx,yy,zz,cte,hk,hl,quad,h,k,l !, ctep  , vstar
      integer              :: i, mleft,ih,ik,il,i_fin
      integer,dimension(3) :: ihkl
      logical              :: satel
      real(kind=cp), parameter :: twooverpi=0.63661977236758134307553505349006
      real(kind=cp), parameter :: cte_siz=0.03647562611 !0.001*360/pi^2  (HL= 360 Lambda/pi^2/D/Costheta)
                                                        ! dsiz=cte*1000/D = cte*f(h,k,l,...) D in same units as Lambda
                                                        ! SZ=1000/D => D= 1000/SZ
                                                        ! Contribution to HL = (Y+dsiz)/cosTheta
                                                        ! pi    =3.1415926536, &    ! ln2=0.69314718055
                                                        ! sqrpil=2.1289340388, &    ! sqrt(pi/Ln2)
                                                        ! ln2_8_sqrt = 2.354820045031 !sqrt(8Ln2)

      size_comp="                "
      size_mod=adjustl(size_model)
      i=index(size_mod," ")
      if(i /= 0) then
         size_mod=u_case(trim(size_mod(1:i)))
         size_comp=trim(adjustl(u_case(size_model(i+1:))))
      else
        size_mod=u_case(trim(size_mod))
      end if

      Select Case (trim(mode))
         Case("CW")
            cte=cte_siz*wav
         Case("TF")
            cte=wav*twooverpi*1.0e-03
         Case Default
            Call Set_Error(1,"The mode argument should be 'CW' or 'TF' ...")
            return
      End Select

      h=hkl(1); k=hkl(2); l=hkl(3)
      ih=nint(hkl(1)); ik=nint(hkl(2)); il=nint(hkl(3))
      ihkl=[ih,ik,il]
      satel=.false.
      if(sum(abs(hkl-ihkl)) > 0.001) satel=.true.
      dsiz=0.0
      if(present(der)) der=0.0

      Select Case (size_mod)

        Case("QUADRATIC_FORM")

         ds=cte/sq
         h2=h*h
         k2=k*k
         l2=l*l
         hk=2.0*h*k
         hl=2.0*h*l
         kl=2.0*k*l
         quad=par(1)*h2+par(2)*k2+par(3)*l2+par(4)*hk+par(5)*hl+par(6)*kl
         dsiz=ds*quad  ! cte*quad*d^2
         if(present(der)) then
           der(1)=ds*h2
           der(2)=ds*k2
           der(3)=ds*l2
           der(4)=ds*kl
           der(5)=ds*hl
           der(6)=ds*hk
         end if
         return

        !-------------------------------------------------------------------
        ! Platelet-Needle particles. Instead of using an exact profile shape
        ! the lorentzian component of the assumed Voigt function is allowed
        ! to vary according to the expression:
        !    HL=(Y+Dsiz*cos(q))/cos(theta) or   HL=(Y+Dsiz*sin(q))/cos(theta)
        ! where Y is the isotropic lorentzian-like size parameter    (JRC)
        !-------------------------------------------------------------------
        Case("PLATELET")

           If(present(psz)) then
              If(present(cell)) then
                 ds=dot_product(psz(1:3), matmul(cell%Gr,hkl))
              Else
                 Call Set_Error(1,"Platelet anisotropic size needs, as input, the CELL object!")
                 return
              End if
           Else
              Call Set_Error(1,"Platelet anisotropic size needs as input the reciprocal space direction PSZ!")
              return
           End if
           ds=cte*ABS(ds)/SQRT(psz(4)*sq)
           dsiz=ds*par(1)
           if(present(der)) der(1)=ds
           return

        Case("NEEDLE")
           If(present(psz)) then
              If(present(cell)) then
                 ds=dot_product(psz(1:3), matmul(cell%Gr,hkl))
              Else
                 Call Set_Error(1,"Platelet anisotropic size needs, as input, the CELL object!")
                 return
              End if
           Else
              Call Set_Error(1,"Platelet anisotropic size needs as input the reciprocal space direction PSZ!")
              return
           End if
           ds=ABS(ds)/SQRT(psz(4)*sq)
           ds=cte*SQRT(abs(1.0-ds*ds))
           dsiz=ds*par(1)
           if(present(der)) der(1)=ds
           return

        Case("CONDIT_HKL")
           if(present(nv)) then
             do i=1,size(nv)
               !Condition that hkl is perpendicular to the zone axis nv(1:3,i) ... in this case n4 should be equal to n5
               mleft= dot_product(nv(1:3,i),ihkl)
               if(nv(4,i) == nv(5,i) .and. mleft == 0) then  !mleft=0, n4=n5 it is applied
                  dsiz=par(i)*cte
                  if(present(der)) der(i)=cte
                  return
               end if
               !Another meaning
               mleft=mleft-nv(5,i)
               if(mod(mleft,nv(4,i)) == 0) then
                  dsiz=par(i)*cte
                  if(present(der)) der(i)=cte
                  return
               end if
             end do
           end if

        Case("HKL")

           Select Case (size_comp)

             Case("00L")  ! cylindrical particles
                IF(.not.(ih == 0 .AND. ik == 0 .AND. il /= 0)) return
             Case("0K0")
                IF(.not.(ih == 0 .AND. ik /= 0 .AND. il == 0)) return
             Case("H00")
                IF(.not.(ih /= 0 .AND. ik == 0 .AND. il == 0)) return
             Case("HK0")
                IF(.not.(ih /= 0 .AND. ik /= 0 .AND. il == 0)) return
             Case("H0L")
                IF(.not.(ih /= 0 .AND. ik == 0 .AND. il /= 0)) return
             Case("0KL")
                IF(.not.(ih == 0 .AND. ik /= 0 .AND. il /= 0)) return
             Case("SATEL")     !correlation length for satellites
                IF(.not. satel) return
             Case("H&K_ODD")   !H=2n+1 and K=2m+1
                IF(.not.(MOD(ih,2) /= 0 .AND. MOD(ik,2) /= 0)) return
             Case("H0L_EVEN")  !H0L (H+L=2n)
                IF(.not.(MOD(ih+il,2) == 0 .AND. ik == 0)) return
             Case("HKL_NOHHL") !HKL (except HHL)
                IF(ih == ik) return
             Case("HKL_HODD")  !HKL (h=2n+1 )
                IF(MOD(ih,2) == 0) return
             Case("HKL_HEVEN_KODD")
                IF(.not.(MOD(ih,2) == 0 .AND. MOD(ik,2) /= 0)) return
             Case("HKL_HORK_ODD_LODD")  !HKL (h=2n+1 or k=2m+1 and l=2n+1)
                IF(.not.( (MOD(ih,2) /= 0 .OR. MOD(ik,2) /= 0)  .AND. MOD(il,2) /= 0)) return
             Case("HKL_NOTSAMEPAR")
                IF( MOD(ih,2) == 0 .and. MOD(ik,2) == 0 .and. MOD(il,2) == 0) return
                IF( MOD(ih,2) /= 0 .and. MOD(ik,2) /= 0 .and. MOD(il,2) /= 0) return
           End Select
           dsiz=cte*par(1)
           if(present(der)) der(1)=cte

        Case("SPHERICAL_HARMONICS")
          !     Anisotropic Size Broadening using Spherical Harmonics up
          !     to n-th order (n depend of the Laue class).
          !     Written by RJP/LLB on January 15th, 1998
          !     Step 1: Compute XX,YY,ZZ
          !     SCATV = K / (2.0*PI)
          !     SCATV = AL(1,1)*H*H+AL(2,2)*K*K+AL(3,3)*L*L+2.0*H*L*AL(1,3)
          !     AL(i,j) Upper triangular reciprocal Quadratic form
          !     AL(i,i) = GR(i,i), AL(i,j)= 2.0 * GR(i,j)
          !     Ref: M. Jarvinen, J. Appl. C. (1993),p.525
          !     (see Table 5 and Fig. 4 in particular)
          !
           scatv = SQRT(sq)
           uv=cart_vector('r',hkl,cell)/scatv
           yy=uv(1)
           zz=uv(2)
           xx=uv(3)
           Select Case (Laue)
              Case("2/m")
                 !   Compute Y00,Y20,Y22+,Y22-,Y40,Y42+,Y42-,Y44+,Y44- in that order
                 dv(1) = 1.0
                 dv(2) = 0.5*(3.0*zz**2-1.0)
                 dv(3) = xx**2-yy**2
                 dv(4) = 2.0*xx*yy
                 dv(5) = 0.12500*(35.0*zz**4-30.0*zz**2+3.0)
                 dv(6) = 0.77778*(7.0*zz**2-1.0)*dv(3)
                 dv(7) = 0.77778*(7.0*zz**2-1.0)*dv(4)
                 dv(8) = dv(3)**2-dv(4)**2
                 dv(9) = 2.0*dv(3)*dv(4)
                 i_fin=9

              Case ("-3m")
                 !
                 !     Trigonal Anisotropic Broadening using Spherical Harmonics up
                 !     to 6-th order (Hexagonal setting, unique axis c)
                 !
                 !     Compute Ylm's up to 6th order:  Y00,Y20,Y40,Y43-,Y60,Y63-,Y66+
                 !
                 dv(1) = 1.0
                 dv(2) = 0.5*(3.0*zz*zz-1.0)
                 dv(3) = 0.125*(35.0*zz*zz*zz*zz - 30.0*zz*zz + 3.0)
                 dv(4) = 3.07920*yy*zz*(3.0*xx*xx-yy*yy)
                 dv(5) = 231.0*zz*zz*zz*zz*zz*zz-315.0*zz*zz*zz*zz+105.0*zz*zz-5.0
                 dv(5) = 0.06250*dv(5)
                 dv(6) = 1.41685*(11.0*zz*zz-3.0)*yy*zz*(3.0*xx*xx-yy*yy)
                 dv(7) = (xx*xx-yy*yy)*(xx*xx*xx*xx+yy*yy*yy*yy-14.0*xx*xx*yy*yy)
                 i_fin=7

              Case ("m3m","m-3m")
                !
                !     Cubic Anisotropic Broadening using Spherical Harmonics up
                !     to 8-th order
                !     x,y,z along a,b,c
                !
                !     Beware ! For spacegroups 207-230, K62 should be set to zero.
                !                  Laue class = m -3 m
                !              No restriction for spacegroups 195-206
                !                  Laue class = m -3
                !
                !     Compute Cubic harmonics Klm's up to 8th order: K00,K41,K61,K62,K81 in that order
                 dv(1) =  1.0
                 dv(2) =  2.5    *(xx**4 + yy**4 + zz**4) -  1.5
                 dv(3) = 21.65625*(xx**6 + yy**6 + zz**6) - 29.53125*(xx**4 + yy**4 + zz**4) + 8.43750
                 dv(4) = xx**4*yy**2 - yy**4*xx**2 + yy**4*zz**2 - zz**4*yy**2 + zz**4*xx**2 - xx**4*zz**2
                 dv(4) = dv(4)*6.0*sqrt(3.0)
                 dv(5) =  97.5*(xx**8 + yy**8 + zz**8)- 182.0*(xx**6 + yy**6 + zz**6)+ 105.0*(xx**4 + yy**4 + zz**4)-  17.5
                 dv(5) = dv(5)/3.0
                 i_fin=5

              Case ("mmm")
                !
                !    Compute  Orthorhombic Anisotropic Broadening using Spherical Harmonics
                !    up to 4-th order: Y00,Y20,Y22+,Y40,Y42+,Y44+
                dv(1) = 1.0
                dv(2) = 0.5*(3.0*zz**2-1.0)
                dv(3) = xx**2-yy**2
                dv(4) = 0.125*(35.0*zz**4-30*zz**2+3.0)
                dv(5) = 0.77778*(7*zz**2-1.0)*dv(3)
                dv(6) = dv(3)**2-4.0*xx**2*yy**2
                i_fin=6
                !                                                                      =
              Case ("6/mmm")
               !
               !     Hexagonal Anisotropic Broadening using Spherical Harmonics up
               !     to 6-th order.
               !
               !     Compute Ylm's up to 6th order:  Y00,Y20,Y40,Y60,Y66+,Y66-
               !
               !     Ref: M. Jarvinen, J. Appl. C. (1993),p.525
               !                           (see Table 4 and Fig. 4 in particular)
               !
               dv(1) = 1.0
               dv(2) = 0.5*(3.0*zz**2-1.0)
               !     .. Next line corrected 28 December 1998
               dv(3) = 0.125*(35.0*zz**4-30.0*zz**2+3.0)
               dv(4) = 231.0*zz**6-315.0*zz**4+105.0*zz**2-5.0
               dv(4) = 0.06250*dv(4)   !Correction 3/6/2016 it was 5!!!!
               dv(5) = (xx**2-yy**2)*(xx**4+yy**4-14.0*xx**2*yy**2)
               !     .. Next line corrected 28 December 1998
               dv(6) = 3.0*(xx**4+yy**4)-10.0*xx**2*yy**2
               dv(6) = 2.0*xx*yy*dv(6)
               i_fin=6
                !                                                                      =
              Case ("-3")
                !
                !     Trigonal Anisotropic Broadening using Spherical Harmonics up
                !     to 4-th order (Hexagonal setting, unique axis c)
                !
                !     Compute Ylm's up to 4th order:  Y00,Y20,Y40,Y43+,Y43-
                !
                !     Ref: M. Jarvinen, J. Appl. C. (1993),p.525
                !                           (see Table 5 and Fig. 4 in particular)
                !
                dv(1) = 1.0
                dv(2) = 0.5*(3.0*zz**2-1.0)
                !     .. Next line corrected 28 December 1998
                dv(3) = 0.125*(35.0*zz**4-30.0*zz**2+3.0)
                dv(4) = 3.07920*xx*zz*(xx**2-3.0*yy**2)
                dv(5) = 3.07920*yy*zz*(3.0*xx**2-yy**2)
                i_fin=5
                !                                                                    =
              Case ("4/mmm")
                !
                !     Tetragonal Anisotropic Broadening using Spherical Harmonics up
                !     to 6-th order.
                !
                !     Compute Ylm's up to 4th order:  Y00,Y20,Y40,Y44+,Y44-,Y60,Y64+,Y64-
                !
                !     Ref: M. Jarvinen, J. Appl. C. (1993),p.525
                !                           (see Tables 2 and 4 in particular)
                !
                dv(1) = 1.0
                dv(2) = 0.5*(3.0*zz**2-1.0)
                dv(3) = 0.125*(35.0*zz**4-30*zz**2+3.0)
                dv(4) = (xx**4+yy**4-6.0*xx*yy)
                dv(5) = 4.0*xx*yy*(xx**2-yy**2)
                dv(6) = 231.0*zz**6-315.0*zz**4+105.0*zz**2-5.0
                dv(6) = 0.06250*dv(6)
                dv(7) = 0.81675*(11.0*zz**2-1.0)*dv(4)
                dv(8) = 0.81675*(11.0*zz**2-1.0)*dv(5)
                i_fin=8
                !                                                                      =
              Case("-1")
                !
                !     Trigonal Anisotropic Broadening using Spherical Harmonics up
                !     to 2nd-order ()
                !
                !     Compute Ylm's up to 2nd order:  Y00,Y20,Y21+,Y21-,Y22+,Y22-
                !
                !     Ref: M. Jarvinen, J. Appl. C. (1993),p.525
                !                           (see Table 5 and Fig. 4 in particular)
                !
                dv(1) = 1.0
                dv(2) = 0.5*(3.0*zz**2-1.0)
                dv(3) = 2.0*zz*xx
                dv(4) = 2.0*zz*yy
                dv(5) = xx**2-yy**2
                dv(6) = 2.0*xx*yy
                i_fin=6
           End Select !Laue
           !        Compute FHWM
           !         dv(1:9)=cte*dv(1:9)
           dsiz=0.0
           do i=1,i_fin
             dsiz = dsiz+dv(i)*par(i)
           end do
           dsiz=dsiz*cte
           if(present(der)) der(1:i_fin)=dv(1:i_fin)*cte

        Case Default
          call Set_Error(1,"Unknown Size Model: "//trim(size_model))
          return
      End Select

   End Subroutine Calc_Anisotropic_Size

!    !---------------------------------------------------------------------------
!    !   HKL-dependent asymmetry and shift for stacking faults and other
!    !   defects can be modeled by using up to three parameters stored
!    !   in AS=PAR(NPHT,38), and Shift1=PAR(NPHT,39) and Shift2=PAR(NPHT,40)
!    !---------------------------------------------------------------------------
!    Module Subroutine shifhkl(ix,icx,h,k,l,tanth,shv,sqv,al,der)
!      integer,       intent(in)  :: ix
!      integer,       intent(in)  :: icx
!      real(kind=cp), intent(in)  :: h
!      real(kind=cp), intent(in)  :: k
!      real(kind=cp), intent(in)  :: l
!      integer,       intent(in)  :: ip
!      real(kind=cp), intent(out) :: shv  !1/d^2
!      real(kind=cp), intent(in)  :: sqv, tanth
!      real(kind=cp), intent(in), dimension(3,3):: al
!      real(kind=cp), intent(out),optional, dimension(:) :: der
!      !--- Local Variables ---!
!      real(kind=cp), dimension(3) :: hh
!      real(kind=cp) :: ds, sin2,tant, prmul
!      integer       :: i,j,mleft
!      real(kind=cp) :: h400,h040,h004,  h220,h202,h022,  h211,h121,h112, &  !variables of quartic form
!                       h310, h301, h130, h103, h013, h031, d4
!      real(kind=cp) :: h200, h020, h002, h110, h101, h011, d2    !variables of quadratic form
!
!      shv=0.0
!      if(present(der)) then
!        der(:)=0.0
!      end if
!      IF(ishif(ip) == 0) RETURN
!      IF(ixunit(n_pat) == 0) THEN
!        prmul=2.0*tanth/sqv*1.0E-02
!      ELSE IF(ixunit(n_pat) == 1) THEN
!        prmul=-dtt1(n_pat)*tanth/sqv*1.0E-02   !here tanth=d-spacing
!      ELSE IF(ixunit(n_pat) == 2) THEN
!        prmul=0.5*glb(2,n_pat)/tanth*1.0E-02   !here tanth=1/d=s
!      END IF
!      Select Case(ishif(ip))
!       Case(1,-1)
!        !-------------------------------------------------------------------
!        ! Shift of Bragg reflections of the form Shv= K * cos(q) or / K * sin(q)
!        !-------------------------------------------------------------------
!          ds=0.0
!          hh(1)=h
!          hh(2)=k
!          hh(3)=l
!          do i=1,3
!            do j=i,3
!              ds=ds+al(i,j)*(psh(ip,i)*hh(j)+psh(ip,j)*hh(i))/2.
!            end do
!          end do
!          ds=ABS(ds)/SQRT(psh(ip,4)*sqv)
!          IF(ishif(ip) == -1) THEN
!            ds=SQRT(abs(1.0-ds*ds))
!          END IF
!          shv=ds*par(1)
!          if(present(der)) der(1)=ds
!          return
!       Case(2)
!          sin2=sqv/4.0*lamda(icx,n_pat)*lamda(icx,n_pat)
!          tant=SQRT(abs(sin2/(1.0-sin2)))
!          ds=phasen(ix,n_pat)*tant
!          shv=ds*par(1)
!          if(present(der)) der(1)=ds
!          return
!       Case(-2,-3,-4,-5,-6,-7,-8,-9,-10)
!          do i=1,abs(ishif(ip))-nshft(ip)
!            mleft=nint(shft(1,i,ip)*h+shft(2,i,ip)*k+shft(3,i,ip)*l)
!            if(shft(4,i,ip) == shft(5,i,ip) .and. mleft == 0) then
!               shv=par(ip,63+i,n_pat)*prmul
!               if(present(der)) der(i)=prmul
!               return
!            end if
!            if(mleft == 0) return
!            mleft=mleft-shft(5,i,ip)
!            if(mod(mleft,shft(4,i,ip)) == 0) then
!               shv=par(ip,63+i,n_pat)*prmul
!               if(present(der)) der(i)=prmul
!               return
!            end if
!          end do
!          return
!
!       Case(101)  ! -1      All 21 parameters free
!              ! Order:
!              !   D2_200     D2_020     D2_002    D2_011    D2_101    D2_110
!              !
!           h200=h*h       !  64
!           h020=k*k       !  65
!           h002=l*l       !  66
!           h011=k*l       !  67
!           h101=h*l       !  68
!           h110=h*k       !  69
!           d2 = par(1)*h200 + par(2)*h020 + par(3)*h002  &
!               +par(4)*h011 + par(5)*h101 + par(6)*h110
!
!              ! Order:
!              !   D4_400 70      D4_040  71     D4_004  72     D4_220  73     D4_202  74
!              !   D4_022 75      D4_211  76     D4_121  77     D4_112  78     D4_310  79
!              !   D4_301 80      D4_130  81     D4_103  82     D4_013  83     D4_031  84
!           h400=h*h*h*h   !  70
!           h040=k*k*k*k   !  71
!           h004=l*l*l*l   !  72
!           h220=h*h*k*k   !  73
!           h202=h*h*l*l   !  74
!           h022=k*k*l*l   !  75
!           h211=h*h*k*l   !  76
!           h121=h*k*k*l   !  77
!           h112=h*k*l*l   !  78
!           h310=h*h*h*k   !  79
!           h301=h*h*h*l   !  80
!           h130=h*k*k*k   !  81
!           h103=h*l*l*l   !  82
!           h013=k*l*l*l   !  83
!           h031=k*k*k*l   !  84
!           d4 = par(7)*h400 + par(8)*h040 + par(9)*h004  &
!               +par(10)*h220 + par(11)*h202 + par(12)*h022  &
!               +par(13)*h211 + par(14)*h121 + par(15)*h112  &
!               +par(16)*h310 + par(17)*h301 + par(18)*h130  &
!               +par(19)*h103 + par(20)*h013 + par(21)*h031
!           shv=prmul*(d2+d4)
!           if(present(der)) then
!             der( 1)=prmul*h200
!             der( 2)=prmul*h020
!             der( 3)=prmul*h002
!             der( 4)=prmul*h011
!             der( 5)=prmul*h101
!             der( 6)=prmul*h110
!
!             der( 7)=prmul*h400
!             der( 8)=prmul*h040
!             der( 9)=prmul*h004
!             der(10)=prmul*h220
!             der(11)=prmul*h202
!             der(12)=prmul*h022
!             der(13)=prmul*h211
!             der(14)=prmul*h121
!             der(15)=prmul*h112
!             der(16)=prmul*h310
!             der(17)=prmul*h301
!             der(18)=prmul*h130
!             der(19)=prmul*h103
!             der(20)=prmul*h013
!             der(21)=prmul*h031
!           end if
!           return
!        Case( 102)  ! 2/m  //b     (13 free parameters)
!
!              ! Order:
!              !   D2_200     D2_020     D2_002      D2_101
!              !
!           h200=h*h       !  64
!           h020=k*k       !  65
!           h002=l*l       !  66
!           h101=h*l       !  67
!           d2 = par(1)*h200 + par(2)*h020 + par(3)*h002  &
!               +par(4)*h101
!             ! Order:
!             !   D4_400         D4_040         D4_004         D4_220         D4_202
!             !   D4_022         D4_121         D4_301         D4_103
!           h400=h*h*h*h   !  70
!           h040=k*k*k*k   !  71
!           h004=l*l*l*l   !  72
!           h220=h*h*k*k   !  73
!           h202=h*h*l*l   !  74
!           h022=k*k*l*l   !  75
!           h121=h*k*k*l   !  76
!           h301=h*h*h*l   !  77
!           h103=h*l*l*l   !  78
!           d4 = par(7)*h400 + par(8)*h040 + par(9)*h004  &
!               +par(10)*h220 + par(11)*h202 + par(12)*h022  &
!               +par(13)*h121 + par(14)*h301 + par(15)*h103
!           shv=prmul*(d2+d4)
!           if(present(der)) then
!             der( 1)=prmul*h200
!             der( 2)=prmul*h020
!             der( 3)=prmul*h002
!             der( 4)=prmul*h101
!             der( 5)=prmul*h400
!             der( 6)=prmul*h040
!             der( 7)=prmul*h004
!             der( 8)=prmul*h220
!             der( 9)=prmul*h202
!             der(10)=prmul*h022
!             der(11)=prmul*h121
!             der(12)=prmul*h301
!             der(13)=prmul*h103
!           end if
!           return
!        Case(-102)  ! 2/m  //c   (13 free parameters)
!              ! Order:
!              !   D2_200     D2_020     D2_002      D2_110
!              !
!           h200=h*h       !  64
!           h020=k*k       !  65
!           h002=l*l       !  66
!           h110=h*k       !  67
!           d2 = par(1)*h200 + par(2)*h020 + par(3)*h002 + par(4)*h110
!             !Order:
!             !   D4_400         D4_040         D4_004         D4_220         D4_202
!             !   D4_022         D4_112         D4_310         D4_130
!           h400=h*h*h*h   !  70
!           h040=k*k*k*k   !  71
!           h004=l*l*l*l   !  72
!           h220=h*h*k*k   !  73
!           h202=h*h*l*l   !  74
!           h022=k*k*l*l   !  75
!           h112=h*k*l*l   !  76
!           h310=h*h*h*k   !  77
!           h130=h*k*k*k   !  78
!           d4 = par(7)*h400 + par(8)*h040 + par(9)*h004  &
!               +par(10)*h220 + par(11)*h202 + par(12)*h022  &
!               +par(13)*h112 + par(14)*h310 + par(15)*h130
!           shv=prmul*(d2+d4)
!           if(present(der)) then
!             der( 1)=prmul*h200
!             der( 2)=prmul*h020
!             der( 3)=prmul*h002
!             der( 4)=prmul*h110
!             der( 5)=prmul*h400
!             der( 6)=prmul*h040
!             der( 7)=prmul*h004
!             der( 8)=prmul*h220
!             der( 9)=prmul*h202
!             der(10)=prmul*h022
!             der(11)=prmul*h112
!             der(12)=prmul*h310
!             der(13)=prmul*h130
!           end if
!           return
!        Case( 103)  ! mmm      (9 free parameters)
!              ! Order:
!              !   D2_200     D2_020     D2_002
!              !
!           h200=h*h       !  64
!           h020=k*k       !  65
!           h002=l*l       !  66
!           d2 = par(1)*h200 + par(2)*h020 + par(3)*h002
!             !Order:
!             !   D4_400        D4_040        D4_004        D4_220        D4_202        D4_022
!           h400=h*h*h*h   !  70
!           h040=k*k*k*k   !  71
!           h004=l*l*l*l   !  72
!           h220=h*h*k*k   !  73
!           h202=h*h*l*l   !  74
!           h022=k*k*l*l   !  75
!           d4 = par(7)*h400 + par(8)*h040 + par(9)*h004  &
!               +par(10)*h220 + par(11)*h202 + par(12)*h022
!           shv=prmul*(d2+d4)
!           if(present(der)) then
!             der( 1)=prmul*h200
!             der( 2)=prmul*h020
!             der( 3)=prmul*h002
!             der( 4)=prmul*h400
!             der( 5)=prmul*h040
!             der( 6)=prmul*h004
!             der( 7)=prmul*h220
!             der( 8)=prmul*h202
!             der( 9)=prmul*h022
!           end if
!           return
!        Case( 104)  ! 4/m   D2_200=D2_020; D4_400=D4_040,D4_004,D4_220,D4_202=D4_022   (6 free parameters)
!              ! Order:
!              !   D2_200 = D2_020     D2_002
!              !
!           h200=h*h       !  64
!           h020=k*k       !  64
!           h002=l*l       !  65
!           d2 = par(1)* (h200 + h020) + par(2)*h002
!             !Order:
!             !   D4_400         D4_004         D4_220         D4_202
!           h400=h*h*h*h   !  70
!           h040=k*k*k*k   !  70
!           h004=l*l*l*l   !  71
!           h220=h*h*k*k   !  72
!           h202=h*h*l*l   !  73
!           h022=k*k*l*l   !  73
!           d4 = par(7)*(h400+h040) + par(8)*h004+ par(9)*h220  &
!               +par(10)*(h202+h022)
!           shv=prmul*(d2+d4)
!           if(present(der)) then
!             der( 1)=prmul*(h200+h020)
!             der( 2)=prmul*h002
!             der( 3)=prmul*(h400+h040)
!             der( 4)=prmul*h004
!             der( 5)=prmul*h220
!             der( 6)=prmul*(h202+h022)
!           end if
!           return
!        Case( 105)  ! 4/mmm   D2_200=D2_020 D4_400=D4_040,D4_004,D4_220,D4_202=D4_022   (6 parameters)
!           h200=h*h       !  64
!           h020=k*k       !  64
!           h002=l*l       !  65
!           d2 = par(1)* (h200 + h020) + par(2)*h002
!             !Order:
!             !   D4_400         D4_004         D4_220         D4_202
!           h400=h*h*h*h   !  70
!           h040=k*k*k*k   !  70
!           h004=l*l*l*l   !  71
!           h220=h*h*k*k   !  72
!           h202=h*h*l*l   !  73
!           h022=k*k*l*l   !  73
!           d4 =par(7)*(h400+h040) + par(8)*h004+ par(9)*h220  &
!               +par(10)*(h202+h022)
!           shv=prmul*(d2+d4)
!           if(present(der)) then
!             der( 1)=prmul*(h200+h020)
!             der( 2)=prmul*h002
!             der( 3)=prmul*(h400+h040)
!             der( 4)=prmul*h004
!             der( 5)=prmul*h220
!             der( 6)=prmul*(h202+h022)
!           end if
!           return
!        Case( 106)  ! -3 R     D2_200=D2_020=D2_110 (hexagonal setting);  (6 free parameters)
!                    !
!                    !          (1) D4_400=D4_040=D4_310/2=D4_130/2=D4_220/3 ; (2) D4_004
!                    !          (3) D4_202=D4_022=D4_112 ;  (4) D4_301/2=-D4_031/2=D4_211/3=-D4_121/3
!           h200=h*h       !  64
!           h020=k*k       !  64
!           h110=h*k       !  64
!           h002=l*l       !  65
!           d2 = par(1)* (h200 + h020 + h110) + par(2)*h002
!             !Order:  D4_400         D4_004         D4_112         D4_211
!           h400=h*h*h*h       !  70
!           h040=k*k*k*k       !  70
!           h310=2.0*h*h*h*k   !  70
!           h130=2.0*h*k*k*k   !  70
!           h220=3.0*h*h*k*k   !  70
!           h004=l*l*l*l   !  71
!           h202=h*h*l*l   !  72
!           h022=k*k*l*l   !  72
!           h112=h*k*l*l   !  72
!           h211= h*h*k*l       !  73
!           h121=-h*k*k*l       !  73
!           h301= h*h*h*l/1.5   !  73
!           h031=-k*k*k*l/1.5   !  73
!           d4 = par(7)*(h400+h040+h310+h130+h220) + par(8)*h004  &
!               +par(9)*(h202+h022+h112) + par(10)*(h211+h121+h301+h031)
!           shv=prmul*(d2+d4)
!           if(present(der)) then
!             der( 1)=prmul*(h200+h020+h110)
!             der( 2)=prmul*h002
!             der( 3)=prmul*(h400+h040+h310+h130+h220)
!             der( 4)=prmul*h004
!             der( 5)=prmul*(h202+h022+h112)
!             der( 6)=prmul*(h211+h121+h301+h031)
!           end if
!           return
!        Case( 107)  ! -3m R
!           h200=h*h       !  64
!           h020=k*k       !  64
!           h110=h*k       !  64
!           h002=l*l       !  65
!           d2 = par(1)* (h200 + h020 + h110) + par(2)*h002
!             !Order:  D4_400         D4_004         D4_112         D4_211
!           h400=h*h*h*h       !  70
!           h040=k*k*k*k       !  70
!           h310=2.0*h*h*h*k   !  70
!           h130=2.0*h*k*k*k   !  70
!           h220=3.0*h*h*k*k   !  70
!           h004=l*l*l*l   !  71
!           h202=h*h*l*l   !  72
!           h022=k*k*l*l   !  72
!           h112=h*k*l*l   !  72
!           h211= h*h*k*l       !  73
!           h121=-h*k*k*l       !  73
!           h301= h*h*h*l/1.5   !  73
!           h031=-k*k*k*l/1.5   !  73
!           d4 = par(7)*(h400+h040+h310+h130+h220) + par(8)*h004  &
!               +par(9)*(h202+h022+h112) + par(10)*(h211+h121+h301+h031)
!           shv=prmul*(d2+d4)
!           if(present(der)) then
!             der( 1)=prmul*(h200+h020+h110)
!             der( 2)=prmul*h002
!             der( 3)=prmul*(h400+h040+h310+h130+h220)
!             der( 4)=prmul*h004
!             der( 5)=prmul*(h202+h022+h112)
!             der( 6)=prmul*(h211+h121+h301+h031)
!           end if
!           return
!        Case( 108)  ! -3 H
!
!           h200=h*h       !  64
!           h020=k*k       !  64
!           h110=h*k       !  64
!           h002=l*l       !  65
!           d2 = par(1)* (h200 + h020 + h110) + par(2)*h002
!             !Order:    D4_400         D4_004         D4_112
!           h400=h*h*h*h       !  70
!           h040=k*k*k*k       !  70
!           h310=2.0*h*h*h*k   !  70
!           h130=2.0*h*k*k*k   !  70
!           h220=3.0*h*h*k*k   !  70
!           h004=l*l*l*l   !  71
!           h202=h*h*l*l   !  72
!           h022=k*k*l*l   !  72
!           h112=h*k*l*l   !  72
!           d4 = par(7)*(h400+h040+h310+h130+h220) + par(8)*h004  &
!               +par(9)*(h202+h022+h112)
!           shv=prmul*(d2+d4)
!           if(present(der)) then
!             der( 1)=prmul*(h200+h020+h110)
!             der( 2)=prmul*h002
!             der( 3)=prmul*(h400+h040+h310+h130+h220)
!             der( 4)=prmul*h004
!             der( 5)=prmul*(h202+h022+h112)
!           end if
!           return
!        Case( 109)  ! -3m1
!           h200=h*h       !  64
!           h020=k*k       !  64
!           h110=h*k       !  64
!           h002=l*l       !  65
!           d2 = par(1)* (h200 + h020 + h110) + par(2)*h002
!             !Order:    D4_400         D4_004         D4_112
!           h400=h*h*h*h       !  70
!           h040=k*k*k*k       !  70
!           h310=2.0*h*h*h*k   !  70
!           h130=2.0*h*k*k*k   !  70
!           h220=3.0*h*h*k*k   !  70
!           h004=l*l*l*l   !  71
!           h202=h*h*l*l   !  72
!           h022=k*k*l*l   !  72
!           h112=h*k*l*l   !  72
!           d4 = par(7)*(h400+h040+h310+h130+h220) + par(8)*h004  &
!               +par(9)*(h202+h022+h112)
!           shv=prmul*(d2+d4)
!           if(present(der)) then
!             der( 1)=prmul*(h200+h020+h110)
!             der( 2)=prmul*h002
!             der( 3)=prmul*(h400+h040+h310+h130+h220)
!             der( 4)=prmul*h004
!             der( 5)=prmul*(h202+h022+h112)
!           end if
!           return
!        Case(110)  ! -31m
!           h200=h*h       !  64
!           h020=k*k       !  64
!           h110=h*k       !  64
!           h002=l*l       !  65
!           d2 = par(1)* (h200 + h020 + h110) + par(2)*h002
!             !Order:    D4_400         D4_004         D4_112
!           h400=h*h*h*h       !  70
!           h040=k*k*k*k       !  70
!           h310=2.0*h*h*h*k   !  70
!           h130=2.0*h*k*k*k   !  70
!           h220=3.0*h*h*k*k   !  70
!           h004=l*l*l*l   !  71
!           h202=h*h*l*l   !  72
!           h022=k*k*l*l   !  72
!           h112=h*k*l*l   !  72
!           d4 = par(7)*(h400+h040+h310+h130+h220) + par(8)*h004  &
!               +par(9)*(h202+h022+h112)
!           shv=prmul*(d2+d4)
!           if(present(der)) then
!             der( 1)=prmul*(h200+h020+h110)
!             der( 2)=prmul*h002
!             der( 3)=prmul*(h400+h040+h310+h130+h220)
!             der( 4)=prmul*h004
!             der( 5)=prmul*(h202+h022+h112)
!           end if
!           return
!        Case(111)  ! 6/m
!           h200=h*h       !  64
!           h020=k*k       !  64
!           h110=h*k       !  64
!           h002=l*l       !  65
!           d2 = par(1)* (h200 + h020 + h110) + par(2)*h002
!             !Order:    D4_400         D4_004         D4_112
!           h400=h*h*h*h       !  70
!           h040=k*k*k*k       !  70
!           h310=2.0*h*h*h*k   !  70
!           h130=2.0*h*k*k*k   !  70
!           h220=3.0*h*h*k*k   !  70
!           h004=l*l*l*l   !  71
!           h202=h*h*l*l   !  72
!           h022=k*k*l*l   !  72
!           h112=h*k*l*l   !  72
!           d4 = par(7)*(h400+h040+h310+h130+h220) + par(8)*h004  &
!               +par(9)*(h202+h022+h112)
!           shv=prmul*(d2+d4)
!           if(present(der)) then
!             der( 1)=prmul*(h200+h020+h110)
!             der( 2)=prmul*h002
!             der( 3)=prmul*(h400+h040+h310+h130+h220)
!             der( 4)=prmul*h004
!             der( 5)=prmul*(h202+h022+h112)
!           end if
!           return
!        Case(112)  ! 6/mmm
!           h200=h*h       !  64
!           h020=k*k       !  64
!           h110=h*k       !  64
!           h002=l*l       !  65
!           d2 = par(1)* (h200 + h020 + h110) + par(2)*h002
!             !Order:    D4_400         D4_004         D4_112
!           h400=h*h*h*h       !  70
!           h040=k*k*k*k       !  70
!           h310=2.0*h*h*h*k   !  70
!           h130=2.0*h*k*k*k   !  70
!           h220=3.0*h*h*k*k   !  70
!           h004=l*l*l*l   !  71
!           h202=h*h*l*l   !  72
!           h022=k*k*l*l   !  72
!           h112=h*k*l*l   !  72
!           d4 = par(7)*(h400+h040+h310+h130+h220) + par(8)*h004  &
!               +par(9)*(h202+h022+h112)
!           shv=prmul*(d2+d4)
!           if(present(der)) then
!             der( 1)=prmul*(h200+h020+h110)
!             der( 2)=prmul*h002
!             der( 3)=prmul*(h400+h040+h310+h130+h220)
!             der( 4)=prmul*h004
!             der( 5)=prmul*(h202+h022+h112)
!           end if
!           return
!        Case(113)  ! m3
!           h200=h*h       !  64
!           h020=k*k       !  64
!           h110=h*k       !  64
!           h011=k*l       !  64
!           h101=h*l       !  64
!           h002=l*l       !  64
!           d2 = par(1)*(h200+h020+h002+h110+h101+h011)
!             !Order:    D4_400         D4_220
!           h400=h*h*h*h   !  70
!           h040=k*k*k*k   !  70
!           h004=l*l*l*l   !  70
!           h220=h*h*k*k   !  71
!           h202=h*h*l*l   !  71
!           h022=k*k*l*l   !  71
!           d4 = par(7)*(h400+h040+h004)+ par(8)*(h202+h022+h220)
!           shv=prmul*(d2+d4)
!           if(present(der)) then
!             der( 1)=prmul*(h200+h020+h002+h110+h101+h011)
!             der( 2)=prmul*(h400+h040+h004)
!             der( 3)=prmul*(h202+h022+h220)
!           end if
!           return
!        Case(114)  ! m3m
!           h200=h*h       !  64
!           h020=k*k       !  64
!           h110=h*k       !  64
!           h011=k*l       !  64
!           h101=h*l       !  64
!           h002=l*l       !  64
!           d2 = par(1)*(h200+h020+h002+h110+h101+h011)
!             !Order:    D4_400         D4_220
!           h400=h*h*h*h   !  70
!           h040=k*k*k*k   !  70
!           h004=l*l*l*l   !  70
!           h220=h*h*k*k   !  71
!           h202=h*h*l*l   !  71
!           h022=k*k*l*l   !  71
!           d4 = par(7)*(h400+h040+h004)+ par(8)*(h202+h022+h220)
!           shv=prmul*(d2+d4)
!           if(present(der)) then
!             der( 1)=prmul*(h200+h020+h002+h110+h101+h011)
!             der( 2)=prmul*(h400+h040+h004)
!             der( 3)=prmul*(h202+h022+h220)
!           end if
!           return
!       Case default
!          return
!      End Select
!          return
!       Case(114)  ! m3m
!          h200=h*h       !  64
!          h020=k*k       !  64
!          h110=h*k       !  64
!          h011=k*l       !  64
!          h101=h*l       !  64
!          h002=l*l       !  64
!          d2 = par(1)*(h200+h020+h002+h110+h101+h011)
!            !Order:    D4_400         D4_220
!          h400=h*h*h*h   !  70
!          h040=k*k*k*k   !  70
!          h004=l*l*l*l   !  70
!          h220=h*h*k*k   !  71
!          h202=h*h*l*l   !  71
!          h022=k*k*l*l   !  71
!          d4 = par(7)*(h400+h040+h004)+ par(8)*(h202+h022+h220)
!          shv=prmul*(d2+d4)
!          if(present(der)) then
!            der( 1)=prmul*(h200+h020+h002+h110+h101+h011)
!            der( 2)=prmul*(h400+h040+h004)
!            der( 3)=prmul*(h202+h022+h220)
!          end if
!          return
!      Case default
!         return
!     End Select
!
!    End Subroutine shifhkl

End SubModule Pow_Microstructure