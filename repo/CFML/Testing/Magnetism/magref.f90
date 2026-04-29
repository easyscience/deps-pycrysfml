Program MagRef
 use CFML_GlobalDeps
 use CFML_gSpaceGroups,             only: SPG_type, Write_SpaceGroup_info, get_stabilizer
 use CFML_Atoms,                    only: Atlist_type, Write_Atom_List, MAtom_list_Type
 use CFML_metrics,                  only: Cell_G_Type, Write_Crystal_Cell
 use CFML_Reflections,              only: H_s
 use CFML_Strings,                  only: file_type, file_list_type
 use CFML_IOForm,                   only: Read_Xtal_Structure
 use CFML_Propagation_vectors,      only: K_Equiv_Minus_K
 use CFML_kvec_Symmetry
 use CFML_kvec_Structure_Factors

 implicit none

 type (file_type)               :: fich_cfl
 type (file_list_type)          :: fichL_cfl
 class(SPG_type),    allocatable:: SpG
 type (MagSymm_k_Type)          :: MGp
 type (Atlist_type)             :: A
 type (MAtom_list_Type)         :: Am
 class(Cell_G_Type), allocatable:: Cell
 type (MagH_Type)               :: Mh

 character(len=256)          :: filcod     !Name of the input file
 character(len=1)            :: sig
 real, dimension(3)          :: u_vect
 integer                     :: lun=1, i,j,m,ih,ik,iil,iv, n_ini,n_end

 integer                     :: narg
 Logical                     :: esta, arggiven=.false.
      !---- Arguments on the command line ----!
      narg=COMMAND_ARGUMENT_COUNT()

      if(narg > 0) then
              call GET_COMMAND_ARGUMENT(1,filcod)
              arggiven=.true.
      end if

      write(unit=*,fmt="(/,/,7(a,/))")                                                 &
           "              ------ P r o g r a m     M a g R e f  ------"               , &
           "                    ---- Version 0.3 June-2014 ----"                    , &
           "    ******************************************************************"  , &
           "    * Calculates magnetic structure factors, and magnetic interaction*"  , &
           "    * vectors from magnetic structures by reading a *.CFL file       *"  , &
           "    * Calculates also Induced Magnetic Structure Factor Tensors      *"  , &
           "    ******************************************************************"  , &
           "                            (JRC- June 2014)"
    write(unit=*,fmt=*) " "

     if(.not. arggiven) then
       write(unit=*,fmt="(a)",advance="no") " => Code of the file xx.cfl (give xx): "
       read(unit=*,fmt="(a)") filcod
       if(len_trim(filcod) == 0) stop
     end if
     i=index(filcod,".",back=.true.)
     if( i /= 0) filcod=filcod(1:i-1)

     open(unit=lun,file=trim(filcod)//".cal", status="replace",action="write")
     write(unit=lun,fmt="(/,/,7(a,/))")                                                &
           "              ------ P r o g r a m     M a g R e f  ------"               , &
           "                    ---- Version 0.3 June-2014 ----"                    , &
           "    ******************************************************************"  , &
           "    * Calculates magnetic structure factors, and magnetic interaction*"  , &
           "    * vectors from magnetic structures by reading a *.CFL file       *"  , &
           "    * Calculates also Induced Magnetic Structure Factor Tensors      *"  , &
           "    ******************************************************************"  , &
           "                            (JRC- June 2014)"

     inquire(file=trim(filcod)//".cfl",exist=esta)
     if( .not. esta) then
       write(unit=*,fmt="(a)") " File: "//trim(filcod)//".cfl does'nt exist!"
       stop
     end if
     call Read_Xtal_Structure(trim(filcod)//".cfl",Cell,SpG,A,Ftype=fich_cfl)
     If(err_CFML%flag .or. err_CFML%ierr /= 0) then
       write(unit=*,fmt="(a)") trim(err_CFML%Msg)
     else

     !Test of the type "file_list_type" by writing at the end of the file
     write(unit=lun,fmt="(/,a)") "    =========================="
     write(unit=lun,fmt="( a )") "    Text of the input CFL file"
     write(unit=lun,fmt="(a,/)") "    =========================="
     write(unit=*,fmt="(/,a)") "    =========================="
     write(unit=*,fmt="( a )") "    Text of the input CFL file"
     write(unit=*,fmt="(a,/)") "    =========================="
     fichL_cfl%nlines=fich_cfl%nlines
     allocate(fichL_cfl%line(fich_cfl%nlines))
     do i=1,fich_cfl%nlines
       write(unit=lun,fmt="(a,i5,a)") " Line:",i,"  "//fich_cfl%line(i)%Str
       write(unit=*,fmt="(a,i5,a)") " Line:",i,"  "//fich_cfl%line(i)%Str
       fichL_cfl%line(i)=fich_cfl%line(i)%Str
     end do


       call Write_Crystal_Cell(Cell,lun)
       call Write_SpaceGroup_info(SpG,lun)
       call Write_Atom_List(A,Iunit=lun)

       n_ini=1
       n_end=fich_cfl%nlines
       call Readn_Set_Magnetic_Kv_Structure(fichL_cfl,n_ini,n_end,MGp,Am)
       if(err_CFML%flag) then
         write(unit=*,fmt="(a)") "   "//err_CFML%Msg
         stop
       end if
       if(Am%suscept) then
          call Write_Magnetic_Structure(lun,MGp,Am,cell=Cell)
       else
          call Write_Magnetic_Structure(lun,MGp,Am)
       end if

      do
         if(Am%suscept) then
           write(unit=*,fmt="(a)",advance="no") &
           " => Enter a reflections as 3 integers -> (h,k,l) : "
           read(unit=*,fmt=*) ih,ik,iil
           m=1
         else
           write(unit=*,fmt="(a)",advance="no") &
           " => Enter a magnetic reflection as 4 integers -> (h,k,l,m)=H+sign(m)*k(abs(m)): "
           read(unit=*,fmt=*) ih,ik,iil,m
         end if
         if( m == 0 .or. abs(ih)+abs(ik)+abs(iil) == 0 ) exit
         !construct partially the object Mh
         j=sign(1,m)
         sig="+"
         if( j == -1) sig="-"
         Mh%signp=real(-j)  ! sign "+" for H-k and "-" for H+k
         iv=abs(m)
         Mh%num_k=iv
         Mh%h= real([ih,ik,iil]) - Mh%signp*MGp%kvec(:,iv)
         Mh%s = h_s(Mh%h,Cell)
         Mh%keqv_minus=K_Equiv_Minus_K(MGp%kvec(:,iv),MGp%latt)

         if(Am%suscept) then
            !write(unit=*,fmt="(a,i2,a)",advance="no") &
            !" => Enter the strength(in Tesla) and direction of applied magnetic field: "
            !read(unit=*,fmt=*) Am%MagField, Am%dir_MField
            call Calc_Induced_Sk(cell,SpG,Am%MagField,Am%dir_MField,Am,6)
            call Calc_Magnetic_Strf_Tensor(SpG,Am,Mh)
            write(unit=lun,fmt="(/,a,3i4,a)")  "  Reflection: (",ih,ik,iil,") "
            write(unit=*,  fmt="(/,a,3i4,a)")  "  Reflection: (",ih,ik,iil,") "
            write(unit=lun,fmt="(a)")          "  Real part of Tensorial Magnetic Structure Factor: "
            write(unit=*,  fmt="(a)")          "  Real part of Tensorial Magnetic Structure Factor: "
            do i=1,3
              write(unit=lun,fmt="(a,3f12.5)") "       ",real(Mh%TMsF(i,:))
              write(unit=*  ,fmt="(a,3f12.5)") "       ",real(Mh%TMsF(i,:))
            end do
            write(unit=lun,fmt="(a)")          "  Imaginary part of Tensorial Magnetic Structure Factor: "
            write(unit=*  ,fmt="(a)")          "  Imaginary part of Tensorial Magnetic Structure Factor: "
            do i=1,3
              write(unit=lun,fmt="(a,3f12.5)") "       ",aimag(Mh%TMsF(i,:))
              write(unit=*  ,fmt="(a,3f12.5)") "       ",aimag(Mh%TMsF(i,:))
            end do
            call Calc_Induced_MsF_MiV(cell,Am%MagField,Am%dir_MField,Mh)
            write(unit=lun,fmt="(a,2(3f8.4,a))") "  Magnetic Structure   Factor : (",real(Mh%MsF),")+i(",aimag(Mh%MsF),") "
            write(unit=lun,fmt="(a,2(3f8.4,a))") "  Magnetic Interaction Vector : (",real(Mh%MiV),")+i(",aimag(Mh%MiV),") "
            write(unit=*,  fmt="(a,2(3f8.4,a))") "  Magnetic Structure   Factor : (",real(Mh%MsF),")+i(",aimag(Mh%MsF),") "
            write(unit=*,  fmt="(a,2(3f8.4,a))") "  Magnetic Interaction Vector : (",real(Mh%MiV),")+i(",aimag(Mh%MiV),") "
            write(unit=lun,fmt="(a,f12.5 )")     "  Square of Mag. int.  Vector : ",Mh%sqMiV
            write(unit=*  ,fmt="(a,f12.5 )")     "  Square of Mag. int.  Vector : ",Mh%sqMiV

         else

            !Calculate magnetic structure factor and magnetic interaction vector
            call Calc_Magnetic_StrF_MiV(Cell,MGp,Am,Mh)
            write(unit=lun,fmt="(/,a,3i4,a,3f8.4,a)") "  Reflection: (",ih,ik,iil,") "//sig//" (",MGp%kvec(:,iv),")"
            write(unit=lun,fmt="(a,3f8.4,a)")         "              (",Mh%h,")"
            write(unit=*,  fmt="(/,a,3i4,a,3f8.4,a)") "  Reflection: (",ih,ik,iil,") "//sig//" (",MGp%kvec(:,iv),")"
            write(unit=*,  fmt="(a,3f8.4,a)")         "              (",Mh%h,")"
            write(unit=lun,fmt="(a,2(3f8.4,a))") "  Magnetic Structure   Factor : (",real(Mh%MsF),")+i(",aimag(Mh%MsF),") "
            write(unit=lun,fmt="(a,2(3f8.4,a))") "  Magnetic Interaction Vector : (",real(Mh%MiV),")+i(",aimag(Mh%MiV),") "
            write(unit=*,  fmt="(a,2(3f8.4,a))") "  Magnetic Structure   Factor : (",real(Mh%MsF),")+i(",aimag(Mh%MsF),") "
            write(unit=*,  fmt="(a,2(3f8.4,a))") "  Magnetic Interaction Vector : (",real(Mh%MiV),")+i(",aimag(Mh%MiV),") "
            write(unit=lun,fmt="(a,f12.5 )")     "  Square of Mag. int.  Vector : ",Mh%sqMiV
            write(unit=*  ,fmt="(a,f12.5 )")     "  Square of Mag. int.  Vector : ",Mh%sqMiV
         end if
       end do


       write(unit=*,fmt="(a)") " Normal End of program: MagRef "
       write(unit=*,fmt="(a)") " Results in File: "//trim(filcod)//".cal"
     end if

     close(unit=lun)

    contains

    !!---- Subroutine Calc_Induced_MsF_MiV(cell,MField,dir_MField,Mh)
    !!----    !---- Arguments ----!
    !!----    type(Crystal_Cell_type),    intent(in)     :: Cell
    !!----    real(kind=cp),              intent(in)     :: MField
    !!----    real(kind=cp),dimension(3), intent(in)     :: dir_MField
    !!----    type(MagH_Type),            intent(in out) :: Mh
    !!----
    !!----  This subroutine completes the object Mh when the tensorial magnetic structure
    !!----  factor has been previously calculated.
    !!----
    !!----  Created: June 2014 (JRC)
    !!----
    Subroutine Calc_Induced_MsF_MiV(cell,MField,dir_MField,Mh)
       !---- Arguments ----!
       type(Cell_G_Type),          intent(in)     :: Cell
       real(kind=cp),              intent(in)     :: MField
       real(kind=cp),dimension(3), intent(in)     :: dir_MField
       type(MagH_Type),            intent(in out) :: Mh
       !--- Local variables ---!
       real(kind=cp)                  :: s
       real(kind=cp),    dimension(3) :: er,ed
       complex(kind=cp), dimension(3) :: Mc, MiV

       !
       u_vect=MField * dir_MField / Veclength(Cell%Cr_Orth_cel,dir_MField)
       Mh%MsF=matmul(Mh%TMsF,u_vect)  !Magnetic structure factor from TMsF tensor
       !---- Calculation of the Magnetic Interaction vector ----!
       s  = 2.0*Mh%s            !1/d=r*, M = M// + Mp   => Mp = M - M// = M - (M.e)e
       er = Mh%h/s              !unitary vector referred to the reciprocal basis
       ed = matmul(cell%GR,er)  !  "        "       "             direct    "
       Mc  = Mh%MsF / Cell%cell                !Magnetic structure factor in basis {a,b,c}
       MiV = Mc - dot_product(er,Mc) * ed      !Magnetic interaction vector in basis {a,b,c}
       Mh%MiV  =  MiV * Cell%cell              !Magnetic interaction vector in basis {e1,e2,e3}
       Mh%MiVC = matmul(Cell%Cr_Orth_cel,MiV)  !Magnetic interaction vector in Cartesian components
       Mh%sqMiV= dot_product(Mh%MiVC, Mh%MiVC)
       return
    End Subroutine Calc_Induced_MsF_MiV

End Program MagRef

