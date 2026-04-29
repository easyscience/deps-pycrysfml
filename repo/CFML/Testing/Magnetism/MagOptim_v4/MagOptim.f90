Program Optimizing_MagStructure
   !---- Optimizing Magnetic Structure vs Integrated Intensity for a MultiDomain Case
   !---- Use Modules ----!
   use CFML_GlobalDeps
   use CFML_gSpaceGroups,        only: space_group_type, Write_SpaceGroup
   use CFML_strings,             only: u_case, ,file_type
   use CFML_Atoms,               only: AtList_Type, Write_Atom_List, mAtom_list_Type
   use CFML_Metrics,             only: Cell_G_Type, Write_Crystal_Cell
   use CFML_IOForm,              only: Read_Xtal_Structure,err_form_mess,err_form
   use CFML_KeyCodes,            only: NP_Max,NP_Refi,V_BCon,V_Bounds,V_Name,V_Vec,&
                                       Allocate_VParam,Init_RefCodes, Read_RefCodes_File, &
                                       Write_Info_RefCodes, Err_RefCodes, err_refcodes_mess, &
                                       VState_to_AtomsPar
   use CFML_Simulated_Annealing, only: SimAnn_Conditions_type, state_Vector_Type, Multistate_Vector_Type, &
                                       err_san_mess,err_SAN, Simanneal_Gen,Set_SimAnn_Cond, &
                                       Set_SimAnn_StateV,Write_SimAnn_Cond, Write_SimAnn_StateV, &
                                       Write_SimAnn_MStateV, SimAnneal_MultiConf,Set_SimAnn_MStateV, &
                                       Sann_Opt_MultiConf
   use CFML_kvec_Symmetry
   use CFML_kvec_Polarimetry
   use cost_magfunctions
   use prep_input

   implicit none

   !---- List of public subroutines ----!

   type (SimAnn_Conditions_type),save :: c
   type (state_Vector_Type),save      :: vs
   type (Multistate_Vector_Type)      :: mvs
!   type (Atom_list_Type)              :: A

   character(len=256)                 :: filcod     !Name of the input file
   character(len=256)                 :: line       !Text line
   integer                            :: i,n, i_cfl
   real                               :: start,fin
   integer                            :: narg,n_ini,n_end
   Logical                            :: esta, arggiven=.false.  !fst_out=.false.,sthlgiven=.false.
                                         ! local_opt=.false. , rest_file=.false.

    !---- Arguments on the command line ----!
    narg=COMMAND_ARGUMENT_COUNT()

    if(narg > 0) then
            call GET_COMMAND_ARGUMENT(1,filcod)
            arggiven=.true.
    end if

    write (unit=*,fmt="(/,/,6(a,/))")                                                  &
            "            ------ PROGRAM for OPTIMIZING Magnetic STRUCTURES ------"            , &
            "                     ---- Version 1.3 October-2012 ----"                        , &
            "    *******************************************************************************"  , &
            "    * This program optimizes a the magnetic structure given in a *.CFL file        "  , &
            "    * against 3D polarisation matrices and magnetic integrated intensities         "  , &
            "    *******************************************************************************"  , &
            "                           (OZ-JRC)-October-2012"
   write (unit=*,fmt=*) " "

   if(.not. arggiven) then
     write(unit=*,fmt="(a)",advance='no') " => Code of the file xx.cfl (give xx): "
     read(unit=*,fmt="(a)") filcod
     if(len_trim(filcod) == 0) stop
   end if

   open(unit=lun,file=trim(filcod)//".out", status="replace",action="write")
   write(unit=lun,fmt="(/,/,6(a,/))")                                                  &
            "            ------ PROGRAM for OPTIMIZING Magnetic STRUCTURES ------"            , &
            "                     ---- Version 1.3 October-2012 ----"                        , &
            "    *******************************************************************************"  , &
            "    * This program optimizes a the magnetic structure given in a *.CFL file        "  , &
            "    * against 3D polarisation matrices and magnetic integrated intensities         "  , &
            "    *******************************************************************************"  , &
            "                           (OZ-JRC)-October-2012"

   inquire(file=trim(filcod)//".cfl",exist=esta)
   if( .not. esta) then
     write(unit=*,fmt="(a)") " File: "//trim(filcod)//".cfl doesn't exist!"
     stop
   end if

!!----
!!---- Reading from CFL file
!!----
   call Readn_Set_Xtal_Structure(trim(filcod)//".cfl",Cell,SpG,A,Mode="CFL",file_list=file_cfl)

   If(err_form) then
     write(unit=*,fmt="(a)") trim(err_form_mess)
   Else
     call Write_Crystal_Cell(Cell,lun)
     call Write_SpaceGroup(SpG,lun)
     call Write_Atom_List(A,lun=lun)

     n_ini=1
     n_end=file_cfl%nlines

     !--- Read observations (needed before location of domains, scales)
     call Readn_Set_Data(file_cfl)
     !--- Read magnetic structure
     call Readn_Set_Magnetic_Structure(file_cfl,n_ini,n_end,MGp,mA,Mag_dom=AllMag_dom,Cell=Cell) !Cell is needed for Spherical components
       if(err_MagSym) then
         write(unit=*,fmt="(a)") "   "//err_MagSym_Mess
         stop
       end if
     call Write_Magnetic_Structure(lun,mGp,mA,AllMag_dom)

     NP_Max= mA%natoms*17+AllMag_dom%nd*2  ! Rxyz,Ixyz (also Rm, Rphi, Rth), mPhase, C1-12 + domains
     call Allocate_Vparam(NP_Max)

     !---Determine the refinement codes from vary/fix instructions
     call Init_RefCodes(FmAtom=mA)
     call Init_RefCodes(Mag_dom=mag_Dom)
     n_ini=1
     n_end=file_cfl%nlines
     call Read_RefCodes_File(file_cfl,n_ini,n_end,mA,AllMag_dom)
     call MagDom_to_Dataset(AllMag_Dom)   !Puts AllMagDom into Multidata%MagDom
     if(Err_RefCodes) then
       write(unit=*,fmt="(a)") trim(err_refcodes_mess)
     end if

     call Write_Info_RefCodes(FmAtom=mA,Mag_dom=AllMag_dom,MGp=mGp,Iunit=lun)

     !--- Read what cost functions will be minimized
     call Readn_Set_CostFunctPars(file_cfl)
     if(Err_cost) then
       write(unit=*,fmt="(a)") trim(Err_Mess_cost)
       stop
     end if
     call Write_CostFunctPars(lun)

     write(unit=*,fmt="(a)") " Normal End of Routine: Read_Cfl"
     write(unit=*,fmt="(a)") " Results in File: "//trim(filcod)//".out"

     !--- Read job and subjob (Opti, f2mag)
     call Readn_Set_Job(file_cfl)
     !--- Set up the simulated annealing conditions
     call Set_SimAnn_Cond(file_cfl,c)
      if(err_SAN) then
         write(unit=*,fmt="(a)") " => Error setting Simulated Annealing conditions"
         write(unit=*,fmt="(a)") trim(err_san_mess)
        stop
      end if
     call Write_SimAnn_Cond(lun,c)
     call cpu_time(start)
     if(c%num_conf == 1) then
         !--- Set up the Simulated Annealing State Vector
         call Set_SimAnn_StateV(NP_Refi,V_BCon,V_Bounds,V_Name,V_Vec,vs)
          if(err_SAN) then
             write(unit=*,fmt="(a)") " => Error setting Simulated Annealing State Vector"
             write(unit=*,fmt="(a)") trim(err_san_mess)
            stop
          end if
         call Write_SimAnn_StateV(lun,vs,"INITIAL STATE")
         !--- Here Simulated Annealing is performed
         call Simanneal_Gen(General_Cost_function,c,vs,lun)
         call Write_SimAnn_StateV(lun,vs,"FINAL STATE")
         !---Write a CFL file
         write(unit=line,fmt="(a,f12.2)") "  Best Configuration found by Simanneal_Gen,  cost=",vs%cost

         call VState_to_AtomsPar(mA,mode="V",MGp=MGp,Mag_dom=AllMag_dom) ! V-Passing Value
         call MagDom_to_Dataset(AllMag_Dom) !to normalize domains of each dataset
         call Copy_mAtom_List(mA, mA_clone)
         call Copy_MagDom_List(AllMag_dom, Mag_dom_clone)
         open(newunit=i_cfl,file=trim(filcod)//"_sol.cfl",status="replace",action="write")
         call Write_SOL_mCFL(i_cfl,file_cfl,mA,AllMag_dom,line)
         close(unit=i_cfl)

     else   ! Multi-configuration Simulated Annealing

         call Set_SimAnn_MStateV(NP_Refi,c%num_conf,V_BCon,V_Bounds,V_Name,V_Vec,mvs)
          if(err_SAN) then
             write(unit=*,fmt="(a)") " => Error setting Simulated Annealing State Vector"
             write(unit=*,fmt="(a)") trim(err_san_mess)
            stop
          end if
         call Write_SimAnn_MStateV(lun,mvs,"INITIAL STATE")
         !--- Here Simulated Annealing is performed
         call SAnn_Opt_MultiConf(General_Cost_function,c,mvs,lun)
         call Write_SimAnn_MStateV(lun,mvs,"FINAL STATE",1)
         !Write CFL files
         call MagDom_to_Dataset(AllMag_Dom) !to normalize domains of each dataset
         call Copy_mAtom_List(mA, mA_clone)
         call Copy_MagDom_List(AllMag_dom, Mag_dom_clone)
         n=mvs%npar
         do i=1,c%num_conf
           line=" "
           write(unit=line,fmt="(a,i2.2,a)") trim(filcod)//"_sol",i,".cfl"
           V_vec(1:n)=mvs%state(1:n,i)
           call VState_to_AtomsPar(mA,mode="V",MGp=Mgp,Mag_dom=AllMag_dom) ! V-Passing Value
           call MagDom_to_Dataset(AllMag_Dom) !to normalize domains of each dataset
           open(newunit=i_cfl,file=trim(line),status="replace",action="write")
           write(unit=line,fmt="(a,i2,a,f12.2)") "  Configuration #",i," found by SimAnneal_MultiConf,  cost=",mvs%cost(i)
           call Write_SOL_mCFL(i_cfl,file_cfl,mA,AllMag_dom,line)
           close(unit=i_cfl)
         end do
     end if

     call Write_FinalCost(lun)
     call Write_SOL_mCFL(lun,file_cfl,mA_Clone,Mag_dom_clone,line)

     !--- Here results are written
     call Write_ObsCalc(file_cfl)

     write(unit=*,fmt="(a)") " Normal End of: PROGRAM FOR OPTIMIZING magnetic STRUCTURES "
     write(unit=*,fmt="(a)") " Results in File: "//trim(filcod)//".out"
     call cpu_time(fin)
     write(unit=*,fmt="(a,f10.2,a)")  "  CPU-Time: ", fin-start," seconds"
     write(unit=*,fmt="(a,f10.2,a)")  "  CPU-Time: ", (fin-start)/60.0," minutes"
     write(unit=lun,fmt="(/,a,f10.2,a)")  "  CPU-Time: ", fin-start," seconds"
   end if

   close(unit=lun)
   stop
End Program Optimizing_MagStructure

Subroutine Write_FST(fst_file,v,cost) ! Is not used here, needed for CFML_optimization_san from libcrysfml
   !---- Arguments ----!
   use CFML_KeyCodes,     only: VState_to_AtomsPar
   use CFML_Atoms,        only: atom_type, atom_list_type
   use CFML_Metrics,      only: Crystal_Cell_Type
   use CFML_gSpaceGroups, only: spg_type

   character(len=*),     intent(in):: fst_file
   real,dimension(:),    intent(in):: v
   real,                 intent(in):: cost

   !----- Local variables -----!
   !type(Atom_Type)                    :: atom
   type (Atom_list_Type)              :: A
   type (Crystal_Cell_Type)           :: Cell
   type (space_group_type)            :: SpG

   integer :: lun,i,nc, ier
   character(len=132)                 :: file_fst,fst_cmd
   character(len=30), dimension(10)   :: cmds

   if(v(1) == 1.23456 .or. cost == v(2)) write(unit=*,fmt="(a)") " => This is nothing else that to avoid warnings!"

   i=index(fst_file,".fst")
   file_fst=fst_file(1:i+3)
   fst_cmd=adjustl(fst_file(i+4:))
   nc=0

   do
     i=index(fst_cmd,";")
     if(i /= 0) then
       nc=nc+1
       cmds(nc)=fst_cmd(1:i-1)
       fst_cmd=fst_cmd(i+1:)
     else
       nc=nc+1
       cmds(nc)=fst_cmd
       exit
     end if
   end do

   !Update the atom parameters
   call VState_to_AtomsPar(A,mode="V")

   open(newunit=lun, file=trim(file_fst), status="replace",action="write",position="rewind",iostat=ier)
   if (ier == 0) then
      write(unit=lun,fmt="(a)") "TITLE  FST-file generated with Write_FST"
      write(unit=lun,fmt="(a)") "SPACEG "//trim(Spg%SPG_Symb)
      write(unit=lun,fmt="(a,3f12.5,3f8.3)") "CELL ",cell%cell, cell%ang
      write(unit=lun,fmt="(a)") "BOX   -0.20  1.20   -0.20  1.20    -0.20  1.20 "
      do i=1,A%natoms
         write(unit=lun,fmt="(a,a,3f12.5,tr4,a)")"Atom "//A%atom(i)%lab, A%atom(i)%chemsymb, A%atom(i)%x, A%atom(i)%AtmInfo
      end do
      do i=1,nc
         write(unit=lun,fmt="(a)") trim(cmds(i))
      end do
      write(unit=lun,fmt="(a)") "!"
      call flush(lun)
      close(unit=lun)
   end if
End Subroutine Write_FST
