Program Schwinger
 use CFML_GlobalDeps
 use CFML_Maths,                    only: cross_product,sort
 use CFML_gSpaceGroups,             only: spg_type, Write_SpaceGroup_Info
 use CFML_Atoms,                    only: AtList_Type, Write_Atom_List
 use CFML_metrics,                  only: Cell_G_Type, Write_Crystal_Cell
 use CFML_Reflections,              only: H_s, RefList_Type,Hkl_gen_sxtal,get_maxnumref
 use CFML_Strings,                  only: l_case,u_case,file_type
 use CFML_IOForm,                   only: Read_Xtal_Structure
 use CFML_Scattering_Tables
 use CFML_Structure_Factors,        only: Scattering_Species_Type, Allocate_Scattering_Species, &
                                          Additional_Scattering_Factors, Calc_General_StrFactor,&
                                          Set_Form_Factors
 implicit none

 type (file_type)               :: fich_cfl
 class(SpG_Type),   allocatable :: SpG
 type (Atlist_Type)             :: A
 class(Cell_G_Type),allocatable :: Cell
 type (RefList_Type)            :: hkl

 character(len=256)           :: line,filcod
 character(len=1)             :: indv
 real(kind=cp)                :: sn,s2,theta,lambda,flip_right,flip_left,up,down,stlmax,Nuc
 real(kind=cp), dimension(3)  :: hn
 integer                      :: lun=1, ier,i,j,ih,ik,iil, MaxNumRef
 complex(kind=cp)             :: fn,fx,fe,fsru,fsrd,fslu,fsld
 real(kind=cp), dimension(6)  :: extc
 integer,  dimension(:), allocatable :: ind

 integer                     :: narg,iext
 Logical                     :: esta, arggiven=.false.,ext=.false. !,ubgiven=.false.,left=.false.

 Type(Scattering_Species_Type) :: Scattf, add_Scat

 real(kind=dp), parameter :: pn  =  0.2695420113693928312
 real(kind=dp), parameter :: schw= -0.00014699

      !---- Arguments on the command line ----!
      narg=COMMAND_ARGUMENT_COUNT()

      if(narg > 0) then
              call GET_COMMAND_ARGUMENT(1,filcod)
              arggiven=.true.
      end if

      write(unit=*,fmt="(/,/,8(a,/))")                                                 &
           "              ------ P r o g r a m   S c h w i n g e r  ------"          , &
           "                  ---- Version 1.0 May-2022 ----"                    , &
           "    ********************************************************************", &
           "    *    Calculates the amplitude of the Schwinger Scattering for      *", &
           "    * individual reflections or all reflections in a reciprocal sphere.*", &
           "    *           The structure is read from a *.CFL file                *", &
           "    ********************************************************************", &
           "                 (JRC- October 2015, May-2022)"
    write(unit=*,fmt=*) " "

     if(.not. arggiven) then
       write(unit=*,fmt="(a)",advance="no") " => Code of the file xx.cfl (give xx): "
       read(unit=*,fmt="(a)") filcod
       if(len_trim(filcod) == 0) stop
     end if
     i=index(filcod,".",back=.true.)
     if( i /= 0) filcod=filcod(1:i-1)

     open(unit=lun,file=trim(filcod)//".cal", status="replace",action="write")
     write(unit=lun,fmt="(/,/,8(a,/))")                                                &
           "              ------ P r o g r a m   S c h w i n g e r  ------"          , &
           "                  ---- Version 0.0 October-2015 ----"                    , &
           "    ********************************************************************", &
           "    *    Calculates the amplitude of the Schwinger Scattering for      *", &
           "    * individual reflections or all reflections in a reciprocal sphere.*", &
           "    *           The structure is read from a *.CFL file                *", &
           "    ********************************************************************", &
           "                            (JRC- October 2015)"

     inquire(file=trim(filcod)//".cfl",exist=esta)
     if( .not. esta) then
       write(unit=*,fmt="(a)") " File: "//trim(filcod)//".cfl does'nt exist!"
       stop
     end if
     call Read_Xtal_Structure(trim(filcod)//".cfl",Cell,SpG,A,FType=fich_cfl)
     !call Read_Xtal_Structure(filenam, Cell, Spg, Atm, IPhase, FType)
     If(err_CFML%ierr /= 0) then
       write(unit=*,fmt="(a)") trim(err_CFML%Msg)
     else

       !Test of the type "file_list_type" by writing at the end of the file
       write(unit=lun,fmt="(/,a)") "    =========================="
       write(unit=lun,fmt="( a )") "    Text of the input CFL file"
       write(unit=lun,fmt="(a,/)") "    =========================="
       do i=1,fich_cfl%nlines
         write(unit=lun,fmt="(a,i5,a)") " Line:",i,"  "//fich_cfl%line(i)%str
       end do


       call Write_Crystal_Cell(Cell,lun)
       call Write_SpaceGroup_Info(SpG,lun)
       call Write_Atom_List(A,Iunit=lun)
       lambda=0.8
       do i=1,fich_cfl%nlines
         line=adjustl(fich_cfl%line(i)%str)
         if(U_Case(line(1:6)) == "LAMBDA") then
           read(unit=line(7:),fmt=*,iostat=ier) lambda
           if(ier /= 0) lambda=0.8
         end if
         if(U_Case(line(1:4)) == "EXTI") then
           read(unit=line(5:),fmt=*,iostat=ier) extc, iext
           if(ier /= 0) then
              read(unit=line(5:),fmt=*,iostat=ier) extc(1)
              if(ier /= 0) extc(:) = 0.0
              iext=1
           end if
           if(any(extc > 0.0001)) ext=.true.
         end if
       end do

       call Additional_Scattering_Factors(fich_cfl,add_Scat)
       if(err_CFML%IErr /= 0) then
         write(unit=*,fmt="(a)") " => "//trim(err_CFML%Msg)
         stop
       end if

       !  Set nuclear, x-ray, neutron and magnetic form factors coefficients for all the atoms
       !  in the structure
       if(add_Scat%Num_Species > 0) then
         call Set_Form_Factors(A,Scattf,lambda,add_Scat,Lun=lun)
       else
         call Set_Form_Factors(A,Scattf,lambda,Lun=lun)
       end if
       if(err_CFML%IErr /= 0) then
         write(unit=*,fmt="(a)") " => "//trim(err_CFML%Msg)
         stop
       end if
       write(unit=*,fmt="(a)",advance="no")  " => Calculate individual(i) or all(a) reflections in a sphere (<cr>=i): "
       read(unit=*,fmt="(a)",iostat=ier) indv
       if(ier /= 0) then
         indv="i"
       else
         if(indv /= "i" .and. indv /= "a" .or. len_trim(indv) == 0) indv="i"
       end if
       if(indv /= "i") then
         write(unit=*,fmt="(a)")  " => All reflections in a reciprocal sphere will be calculated ... "
         write(unit=lun,fmt="(a)")  " => All reflections in a reciprocal sphere will be calculated ... "
         write(unit=*,fmt="(a)",advance="no")  " => Enter minimum d-spacing: "
         read(unit=*,fmt=*,iostat=ier) stlmax
         if(ier /= 0) stlmax=2.0
         write(unit=*,  fmt="(a,f8.4,a)") "    The minimum d-spacing is ",stlmax, " Angstroms"
         write(unit=lun,fmt="(a,f8.4,a)") "    The minimum d-spacing is ",stlmax, " Angstroms"
         stlmax=0.5/stlmax
         write(unit=*,fmt="(a,f8.4,a)") "    Corresponding to a (sinTheta/Lambda)max ",stlmax, " Angstroms^-1"
         write(unit=lun,fmt="(a,f8.4,a)") "    Corresponding to a (sinTheta/Lambda)max ",stlmax, " Angstroms^-1"
         MaxNumRef = get_maxnumref(stlmax,Cell%Vol,multip=SpG%Multip)
         MaxNumRef=MaxNumRef*Spg%NumOps*max(Spg%Centred,1)
         call Hkl_Gen_Sxtal(cell,SpG,0.0,stlmax,hkl)
         write(unit=*,  fmt="(a,i6)")   " => The total number of reflections is ",hkl%Nref
         write(unit=lun,fmt="(a,i6)")   " => The total number of reflections is ",hkl%Nref
         allocate(ind(hkl%nref)); ind=[(i,i=1,hkl%Nref)]
         ind = sort(hkl%ref(:)%s,hkl%Nref)
       end if

       do
         if(indv == "i") then
           write(unit=*,fmt="(a)",advance="no")  " => Enter a reflection as 3 integers -> (h,k,l): "
           read(unit=*,fmt=*,iostat=ier) ih,ik,iil
           if(ier /= 0) cycle
           if(abs(ih)+abs(ik)+abs(iil) == 0 ) exit
           hn=real([ih,ik,iil])
           sn = h_s(hn,Cell)
           theta=lambda*sn
           theta=asin(theta)
           s2=sn*sn
           call Calc_General_StrFactor(hn,s2,A,SpG,Scattf,fn,fx,fe)
           Nuc=fn*conjg(fn)
           fsru=Schwinger_Amplitude(hn,[0.0,0.0, 1.0],theta,fe,Left=.false.)
           fslu=Schwinger_Amplitude(hn,[0.0,0.0, 1.0],theta,fe,Left=.true.)
           fsrd=Schwinger_Amplitude(hn,[0.0,0.0,-1.0],theta,fe,Left=.false.)
           fsld=Schwinger_Amplitude(hn,[0.0,0.0,-1.0],theta,fe,Left=.true.)
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
           write(unit=*,fmt="(/,a,3i4,a,f8.5)") "  Reflection: (",ih,ik,iil,") sinTheta/Lambda=",sn
           write(unit=*,fmt="(a,2(f10.5,a))")   "  Nuclear     Structure   Factor : (",real(fn),  " ) + i (",aimag(fn),  " ) "
           write(unit=*,fmt="(a,2(f10.5,a))")   "  X-rays      Structure   Factor : (",real(fx),  " ) + i (",aimag(fx),  " ) "
           write(unit=*,fmt="(a,2(f10.5,a))")   "  Electrostatic Structure Factor : (",real(fe),  " ) + i (",aimag(fe),  " ) "
           write(unit=*,fmt="(a,2(f10.5,a))")   "  Schwinger Amplitude  right-up  : (",real(fsru)," ) + i (",aimag(fsru)," ) "
           write(unit=*,fmt="(a,2(f10.5,a))")   "  Schwinger Amplitude  right-down: (",real(fsrd)," ) + i (",aimag(fsrd)," ) "
           write(unit=*,fmt="(a,2(f10.5,a))")   "  Schwinger Amplitude  left-up   : (",real(fslu)," ) + i (",aimag(fslu)," ) "
           write(unit=*,fmt="(a,2(f10.5,a))")   "  Schwinger Amplitude  left-down : (",real(fsld)," ) + i (",aimag(fsld)," ) "
           write(unit=*,fmt="(a, f12.6)")      "            Flipping ratio right : ", flip_right
           write(unit=*,fmt="(a, f12.6)")      "            Flipping ratio left  : ", flip_left
           write(unit=*,fmt="(a, f12.6)")      "         Nuclear Intensity(Fn^2) : ", Nuc

           write(unit=lun,fmt="(/,a,3i4,a,f8.5)") "  Reflection: (",ih,ik,iil,") sinTheta/Lambda=",sn
           write(unit=lun,fmt="(a,2(f10.5,a))")   "  Nuclear     Structure   Factor : (",real(fn),  " ) +i (",aimag(fn),  " ) "
           write(unit=lun,fmt="(a,2(f10.5,a))")   "  X-rays      Structure   Factor : (",real(fx),  " ) +i (",aimag(fx),  " ) "
           write(unit=lun,fmt="(a,2(f10.5,a))")   "  Electrostatic Structure Factor : (",real(fe),  " ) +i (",aimag(fe),  " ) "
           write(unit=lun,fmt="(a,2(f10.5,a))")   "  Schwinger Amplitude  right-up  : (",real(fsru)," ) +i (",aimag(fsru)," ) "
           write(unit=lun,fmt="(a,2(f10.5,a))")   "  Schwinger Amplitude  right-down: (",real(fsrd)," ) +i (",aimag(fsrd)," ) "
           write(unit=lun,fmt="(a,2(f10.5,a))")   "  Schwinger Amplitude  left-up   : (",real(fslu)," ) +i (",aimag(fslu)," ) "
           write(unit=lun,fmt="(a,2(f10.5,a))")   "  Schwinger Amplitude  leftt-down: (",real(fsld)," ) +i (",aimag(fsld)," ) "
           write(unit=lun,fmt="(a, f12.6)")      "            Flipping ratio right : ", flip_right
           write(unit=lun,fmt="(a, f12.6)")      "            Flipping ratio left  : ", flip_left
           write(unit=lun,fmt="(a, f12.6)")      "         Nuclear Intensity(Fn^2) : ", Nuc
         else
           write(unit=lun,fmt="(/,a)") &
           "   H   K   L   sinT/L  Intensity     FNr       FNi           FXr       FXi           FEr       FEi      Flip_left  Flip_right"// &
                                    "  SRUr      SRUi          SRDr      SRDi          SLUr      SLUi          SLDr      SLDi"
           do j=1,hkl%NRef
              i=ind(j)
              hn = real(hkl%ref(i)%h,kind=cp)
              sn = hkl%ref(i)%s
              theta=lambda*sn
              theta=asin(theta)
              s2=sn*sn
              call Calc_General_StrFactor(hn,s2,A,SpG,Scattf,fn,fx,fe)
              Nuc=fn*conjg(fn)
              fsru=Schwinger_Amplitude(hn,[0.0,0.0, 1.0],theta,fe,Left=.false.)
              fslu=Schwinger_Amplitude(hn,[0.0,0.0, 1.0],theta,fe,Left=.true.)
              fsrd=Schwinger_Amplitude(hn,[0.0,0.0,-1.0],theta,fe,Left=.false.)
              fsld=Schwinger_Amplitude(hn,[0.0,0.0,-1.0],theta,fe,Left=.true.)
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
              write(unit=lun,fmt="(3i4,f9.5,f10.4,3(2f10.4,tr4),2f10.5,4(2f10.6,tr4))") &
              hkl%Ref(i)%h, hkl%Ref(i)%s,Nuc,fn,fx,fe,flip_left,flip_right,fsru,fsrd,fslu,fsld
           end do
           exit
         end if
       end do
       write(unit=*,fmt="(a)") " Normal End of program: Schwinger "
       write(unit=*,fmt="(a)") " Results in File: "//trim(filcod)//".cal"
     end if

     close(unit=lun)

    contains

    Subroutine Mpol_Ffactor(coeff,ac,sn,mff)
       real(kind=cp), dimension(4), intent(in) :: coeff
       real(kind=cp), dimension(:), intent(in) :: ac
       real(kind=cp), intent(in) :: sn
       real(kind=cp), intent(out):: mff

       ! Local variables
       integer :: i
       real(kind=cp)    :: j0,j2,j4,j6
       j0=ac(7); j2=ac(14); j4=ac(21); j6=ac(28)
       do i=1,5,2
         j0=j0+ac(i)*EXP(-ac(i+1)*sn)      !<j0>      value for Q=H+k
         j2=j2+ac(i+7)*EXP(-ac(i+8)*sn)    !<j2>/sn value for Q=H+k
         j4=j4+ac(i+14)*EXP(-ac(i+15)*sn)  !<j4>/sn value for Q=H+k
         j6=j6+ac(i+21)*EXP(-ac(i+22)*sn)  !<j6>/sn value for Q=H+k
       end do
       j2=j2*sn  !<j2> value for Q=H+k
       j4=j4*sn  !<j4> value for Q=H+k
       j6=j6*sn  !<j6> value for Q=H+k
       mff= pn*coeff(1)*j0 + coeff(2)*j2 + coeff(3)*j4 + coeff(4)*j6
    End Subroutine Mpol_Ffactor

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


End Program Schwinger

