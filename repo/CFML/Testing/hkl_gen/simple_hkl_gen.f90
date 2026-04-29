!!----
!!---- Program: HKL_GEN
!!----          Example of simple program using CFML
!!----
!!---- Author: Juan Rodriguez-Carvajal
!!---- Revision: May-2022
!!
Program Test_HKL_GEN
   !---- Use Modules ----!
   use CFML_GlobalDeps
   use CFML_gSpaceGroups,  only: SpG_Type, set_SpaceGroup, Set_Gspg_From_String, &
                                 Write_SpaceGroup_info, SuperSpaceGroup_Type,    &
                                 Set_Conditions_NumOp_EPS,kvect_info_type
   use CFML_Metrics,       only: Cell_G_Type,set_crystal_cell,Write_Crystal_Cell
   use CFML_Reflections,   only: Refl_Type, get_maxnumref, Write_Info_RefList, Reflist_type, Gener_Reflections
   use CFML_Rational

   !---- Variables ----!
   implicit none

   character(len=1)             :: car
   character(len=40)            :: name_file
   character(len=4)             :: Ref_typ="Refl"
   character(len=180)           :: texto
   class(SpG_Type), allocatable :: grp_espacial
   type (Cell_G_Type)           :: cell
   integer                      :: i,j,k,num,nk, ier, dr, i_out, Mult
   real(kind=cp),dimension(3)   :: celda, angulo
   integer, dimension(3,2)      :: hlim
   real(kind=cp)                :: sintlmax,lambda
   logical                      :: info, mag
   type(RefList_Type)           :: reflections
   type(kvect_info_type)        :: kinfo

   !---- Initializing ----!
   info=.true.
   num=0

   hlim(:,1)=[-2,-1,-6]
   hlim(:,2)=[8,8,8]

   !---- Procedure ----!
   call Set_Conditions_NumOp_EPS(1024) !Maximum admissible multiplicity
   do
      write(unit=*,fmt="(/,a)") " ==================================="
      write(unit=*,fmt="(a)")   "       Program Simple_HKL_gen       "
      write(unit=*,fmt="(a)")   " ==================================="
      write(unit=*,fmt="(a)")   " "

      if (info) then
         write(*,'(/,a,/)') " => Examples of input to the next question:"
         write(*,'(a)') "1236   :: a,b,c;0,0,0      shub        <-- Shubnikov group type 1236 in standard setting"
         write(*,'(a)') "123    :: a,-c,b;1/2,0,0               <-- Space group type 123 in non-standard setting"
         write(*,'(a)') "Pn'ma                                  <-- Shubnikov group type Pn'ma in standard setting"
         write(*,'(a)') "UNI Pnna.1'_a[Pncm]                    <-- Shubnikov group given in unified notation"
         write(*,'(a)') "UNI Pnna.1'_a[Pncm] :: -c,b,a;0,0,0    <-- Shubnikov group given in unified notation in non-stardard setting"
         write(*,'(a)') "Pn'ma  :: -c,b,a;0,0,0     shub        <-- Shubnikov group type Pn'ma in the non-standard setting Pcmn'"
         write(*,'(a)') "B 2 C B                                <-- space #41 of standard symbol Aba2"
         write(*,'(a)') "13232  :: sup                          <-- SuperSpace group 13232 in standard setting"
         write(*,'(a)') "221    :: a2,-a1,a3,a4;0,0,1/2,0  sup  <-- SuperSpace group #221 in non-standard setting"
         write(*,'(a)') "Pnma(0,0,g)0s0  :: sup                 <-- SuperSpace group of standard symbol"
         write(*,'(a)') "x,-y,z,t,1;x,y,z,t+1/2,-1              <-- The generators of a Magnetic SuperSpace group"
         write(*,'(a)') "x1,-x2,x3,x4,1;x1,x2,x3,x4+1/2,-1      <-- The generators of a Magnetic SuperSpace group"
         write(*,'(/,a)') " => Introduce generators, HM or Hall symbol, or the number of a standard group as indicated above: "
         write(unit=*,fmt="(a)",advance="no") " => Enter the string to construct the Space Group: "
         read(unit=*,fmt="(a)") texto
         if (len_trim(texto)==0) exit

         !> Set the SpaceGroup Information from the supplied text
         i=index(texto,"sup")
         j=index(texto,",t")
         k=index(texto,"x4")
         if(i /= 0 .or. j /= 0 .or. k /= 0) then
            allocate(SuperSpaceGroup_Type :: grp_espacial)
         else
            allocate(SpG_Type :: grp_espacial)
         end if
         call Set_gSpG_from_string(texto,grp_espacial,.true.)
         if(err_CFML%ierr /= 0 .or. err_CFML%flag) then
            write(*,"(a)") " => Problem interpreting the given string !"
            write(*,"(a)") " Error Message: "//trim(err_CFML%Msg)
            cycle
         end if

         !> Load Cell Parameters according to the Crystal System
         !> defined by the Space Group
         ier=0
         Select Case(grp_espacial%CrystalSys)
            case("Triclinic")
               write(unit=*,fmt="(a)",advance="no") " => Cell parameters (a,b,c,alpha,beta,gamma): "
               read(unit=*,fmt=*,iostat=ier) celda(1),celda(2),celda(3),angulo(1),angulo(2),angulo(3)

            case("Monoclinic")
               write(unit=*,fmt="(a)",advance="no") " => Cell parameters (a,b,c,beta): "
               angulo(1)=90.0
               angulo(3)=90.0
               read(unit=*,fmt=*,iostat=ier) celda(1),celda(2),celda(3),angulo(2)

            case("Orthorhombic")
               write(unit=*,fmt="(a)",advance="no") " => Cell parameters (a,b,c): "
               angulo(1:3)=90.0
               read(unit=*,fmt=*,iostat=ier) celda(1),celda(2),celda(3)

            case("Tetragonal")
               write(unit=*,fmt="(a)",advance="no") " => Cell parameters (a,c): "
               angulo(1:3)=90.0
               read(unit=*,fmt=*,iostat=ier) celda(1),celda(3)
               celda(2)=celda(1)

            case("Trigonal","Hexagonal")
               write(unit=*,fmt="(a)",advance="no") " => Cell parameters (a,c): "
               angulo(1:2)=90.0
               angulo(3)=120.0
               read(unit=*,fmt=*,iostat=ier) celda(1),celda(3)
               celda(2)=celda(1)

            case("Cubic")
               write(unit=*,fmt="(a)",advance="no") " => Cell parameter (a): "
               angulo(1:3)=90.0
               read(unit=*,fmt=*,iostat=ier) celda(1)
               celda(2)=celda(1)
               celda(3)=celda(1)
         End Select
         if( ier /= 0 ) cycle

         !> Set the Cell parameters
         call set_crystal_cell(celda,angulo,cell)

         !> Wavelength
         info=.false.
         write(unit=*,fmt="(a)",advance="no") " => Give the wavelength: "
         read(unit=*,fmt=*) lambda
         !Resolution sphere d*(max)=2.0/Lambda => maximum admissible sinTheta/Lambda =1/Lambda
      end if

      !> Set the range for HKL calculation
      write(unit=*,fmt="(a)",advance="no") " => Maximum Sin_Theta/Lambda (1 reals, if val < 0 => stops): "
      read(unit=*,fmt=*,iostat=ier) sintlmax
      if(ier /= 0) cycle
      if (sintlmax < 0.0) exit
      if(sintlmax > 1.0/Lambda ) then
        sintlmax = 1.0/Lambda
        write(unit=*,fmt="(a,f8.4)")  " => The maximum Sin_Theta/Lambda available for the current wavelength is: ",sintlmax
        write(unit=*,fmt="(a,f8.4,a)") " => The input value has been changed to: (",sintlmax,")"
      end if

      texto = "  1/Angtroms (Sin_Theta/Lambda)"
      dr=3
      nk=0
      !In case of SuperSpace Group ask for modulation vectors and q-coefficients
      write(unit=*,fmt="(a)",advance="no") " => Consider absences for magnetic reflections? "
      read(unit=*,fmt=*,iostat=ier) car
      if(ier /= 0) cycle
      mag=.false.
      if(car == "y" .or. car == "Y") mag=.true.

      Select Type (grp_espacial)
         Class is (SuperSpaceGroup_Type)
            dr=grp_espacial%D-1
            nk=dr-3
            Mult=grp_espacial%Multip
            if (allocated(grp_espacial%kv))      deallocate (grp_espacial%kv)
            if (allocated(grp_espacial%nharm))   deallocate (grp_espacial%nharm)
            if (allocated(grp_espacial%sintlim)) deallocate (grp_espacial%sintlim)
            if (allocated(grp_espacial%q_coeff)) deallocate (grp_espacial%q_coeff)
            if (allocated(grp_espacial%Rot))     deallocate (grp_espacial%Rot)
            if (allocated(grp_espacial%t))       deallocate (grp_espacial%t)
            if (allocated(grp_espacial%M))       deallocate (grp_espacial%M)
            if (allocated(grp_espacial%tI))      deallocate (grp_espacial%tI)
            if (allocated(grp_espacial%Ep))      deallocate (grp_espacial%Ep)

            allocate(grp_espacial%Rot(3,3,Mult),grp_espacial%t(3,Mult),grp_espacial%tI(nk,Mult), &
                     grp_espacial%M(nk,3,Mult),grp_espacial%Ep(nk,nk,Mult))

            do i=1,Mult
                grp_espacial%Rot(:,:,i)=grp_espacial%Op(i)%Mat(1:3,1:3)
                grp_espacial%t    (:,i)=grp_espacial%Op(i)%Mat(1:3,dr)
                grp_espacial%tI   (:,i)=grp_espacial%Op(i)%Mat(4:dr,dr)
                grp_espacial%M  (:,:,i)=grp_espacial%Op(i)%Mat(4:dr,1:3)
                grp_espacial%Ep (:,:,i)=grp_espacial%Op(i)%Mat(4:dr,4:dr)
            end do
            grp_espacial%nk = nk
            allocate(grp_espacial%kv(3,grp_espacial%nk))

            do i=1,grp_espacial%nk
              write(unit=*,fmt="(a,i2,a)",advance="no") " => Enter modulation vector #",i," : "
              read(unit=*,fmt=*) grp_espacial%kv(:,i)
            end do

            write(unit=*,fmt="(a)",advance="no") " => Enter the number of Q_coefficients: "
            read(unit=*,fmt=*) grp_espacial%nq
            allocate(grp_espacial%q_coeff(grp_espacial%nk,grp_espacial%nq))
            allocate(grp_espacial%nharm(grp_espacial%nk), grp_espacial%sintlim(grp_espacial%nk))
            grp_espacial%nharm(:)=grp_espacial%nq
            grp_espacial%sintlim(:)=sintlmax
            do i=1,grp_espacial%nq
              write(unit=*,fmt="(a,i2,a)",advance="no") " => Enter set of Q_coefficient #",i," : "
              read(unit=*,fmt=*) grp_espacial%q_coeff(:,i)
            end do

      End select



      !> Output Information
      write(unit=*,fmt="(a)",advance="no") " => Name of the output file: "
      read(unit=*,fmt="(a)") name_file

      open(newunit=i_out,file=trim(name_file),status="REPLACE",action="WRITE")
      write(unit=i_out,fmt="(a)")   "    =========================="
      write(unit=i_out,fmt="(a)")   "      PROGRAM SIMPLE_HKL_GEN"
      write(unit=i_out,fmt="(a,/)") "    =========================="

      !> Write the SpaceGroup information
      call Write_SpaceGroup_info(grp_espacial,i_out)

      !> Write the Cell parameters information
      call Write_Crystal_Cell(Cell,i_out)

      !> Write the Reflections

      !> Procedure to calculation all reflections
      !call Gener_Reflections(Cell,Sintlmax,Mag,Reflex,SpG,kinfo,order,powder,mag_only,Friedel,Ref_typ)

      write(unit=i_out,fmt="(/,a)") " ==========================================================================="
      write(unit=i_out,fmt="(a)")   " GENERATION OF REFLECTIONS WITH INTERFACE: (Cell,Slmin,Slmax,Reflex,SpG,Mag)"
      write(unit=i_out,fmt="(a,/)") " ==========================================================================="
      if(nk /= 0) then
        call Gener_Reflections(Cell,0.0,sintlmax,reflections,grp_espacial,1,Mag,Ref_typ=Ref_typ,kout=kinfo)
        call Write_Info_RefList(reflections, i_out,kinfo=kinfo)
      else
        call Gener_Reflections(Cell,0.0,sintlmax,reflections,grp_espacial,1,Mag,Ref_typ=Ref_typ)
        call Write_Info_RefList(reflections, i_out)
      end if

     write(unit=i_out,fmt="(/,a)") " ========================================================================================"
     write(unit=i_out,fmt="(a)")   " GENERATION OF REFLECTIONS WITH INTERFACE: (Cell,Slmin,Slmax,Reflex,SpG,Mag,order=.true.)"
     write(unit=i_out,fmt="(a,/)") " ========================================================================================"
     if(nk /= 0) then
       call Gener_Reflections(Cell,0.0,sintlmax,reflections,grp_espacial,1,Mag,order=.true.,Ref_typ=Ref_typ,kout=kinfo)
       call Write_Info_RefList(reflections, i_out,kinfo=kinfo)
     else
       call Gener_Reflections(Cell,0.0,sintlmax,reflections,grp_espacial,1,Mag,order=.true.,Ref_typ=Ref_typ)
       call Write_Info_RefList(reflections, i_out)
     end if

     write(unit=i_out,fmt="(/,a)") " ========================================================================================="
     write(unit=i_out,fmt="(a)")   " GENERATION OF REFLECTIONS WITH INTERFACE: (Cell,Slmin,Slmax,Reflex,SpG,Mag,Unique=.true.)"
     write(unit=i_out,fmt="(a,/)") " ========================================================================================="
     if(nk /= 0) then
        call Gener_Reflections(Cell,0.0,sintlmax,reflections,grp_espacial,1,Mag, Unique=.true.,Ref_typ=Ref_typ,kout=kinfo)
        call Write_Info_RefList(reflections, i_out,kinfo=kinfo)
     else
        call Gener_Reflections(Cell,0.0,sintlmax,reflections,grp_espacial,1,Mag, Unique=.true.,Ref_typ=Ref_typ)
        call Write_Info_RefList(reflections, i_out)
     end if

     write(unit=i_out,fmt="(/,a)") " ==========================================================================================="
     write(unit=i_out,fmt="(a)")   " GENERATION OF REFLECTIONS WITH INTERFACE: (Cell,Slmin,Slmax,Reflex,SpG,Mag, Friedel=.true.)"
     write(unit=i_out,fmt="(a,/)") " ==========================================================================================="
     if(nk /= 0) then
        call Gener_Reflections(Cell,0.0,sintlmax,reflections,grp_espacial,1,Mag, Friedel=.true.,Ref_typ=Ref_typ,kout=kinfo)
        call Write_Info_RefList(reflections, i_out,kinfo=kinfo)
     else
        call Gener_Reflections(Cell,0.0,sintlmax,reflections,grp_espacial,1,Mag, Friedel=.true.,Ref_typ=Ref_typ)
        call Write_Info_RefList(reflections, i_out)
     end if

     write(unit=i_out,fmt="(/,a)") " ===================================================================================================="
     write(unit=i_out,fmt="(a)")   " GENERATION OF REFLECTIONS WITH INTERFACE: (Cell,Sintlmax,Mag,Reflex,SpG,order=.true.,Friedel=.true.)"
     write(unit=i_out,fmt="(a,/)") " ===================================================================================================="
     if(nk /= 0) then
        call Gener_Reflections(Cell,0.0,sintlmax,reflections,grp_espacial,1,Mag, order=.true., Friedel=.true.,Ref_typ=Ref_typ,kout=kinfo)
        call Write_Info_RefList(reflections, i_out,kinfo=kinfo)
     else
        call Gener_Reflections(Cell,0.0,sintlmax,reflections,grp_espacial,1,Mag, order=.true., Friedel=.true.,Ref_typ=Ref_typ)
        call Write_Info_RefList(reflections, i_out)
     end if

     write(unit=i_out,fmt="(/,a)") " =============================================================================================="
     write(unit=i_out,fmt="(a)")   " GENERATION OF REFLECTIONS WITH INTERFACE: (Cell,Sintlmax,Mag,Reflex,SpG,seqindx=[2,1,3], hlim)"
     write(unit=i_out,fmt="(a,/)") " =============================================================================================="
     if(nk /= 0) then
        call Gener_Reflections(Cell,0.0,sintlmax,reflections,grp_espacial,seqindx=[2,1,3],Ref_typ=Ref_typ,kout=kinfo,hlim=hlim)
        call Write_Info_RefList(reflections, i_out,kinfo=kinfo)
     else
        call Gener_Reflections(Cell,0.0,sintlmax,reflections,grp_espacial,seqindx=[2,1,3],Ref_typ=Ref_typ,hlim=hlim)
        call Write_Info_RefList(reflections, i_out)
     end if
      close(unit=i_out)
   end do

   write(unit=*,fmt="(a)") " => Program finished ... "
End Program Test_HKL_GEN
