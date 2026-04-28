Submodule (CFML_Keywords_Code_Parser) KWC_Allocation
   !---- Variables ----!
   implicit none

 Contains
    !!--++
    !!--++ Subroutine Allocate_RestParam_Single(file_dat)
    !!--++    Type(file_list_type),     intent( in)    :: file_dat
    !!--++
    !!--++    Allocate vectors Ang_Rest, Dist_Rest, Tor_Rest. It is supposed
    !!--++    that a single phase is at work.
    !!--++
    !!--++ Update: March-2005, May-2015
    !!
    Module Subroutine Allocate_RestParam_Single(file_dat)
       !---- Arguments ----!
       Type(file_list_type),     intent( in) :: file_dat

       !---- Local variables ----!
       character(len=132)              :: line
       character(len=15),dimension(40) :: car
       integer                         :: i,nc,nr

       if (allocated(Ang_Rest)) deallocate(Ang_Rest)
       if (allocated(Dis_Rest)) deallocate(Dis_Rest)
       if (allocated(Tor_Rest)) deallocate(Tor_Rest)

       NP_Rest_Ang=0
       NP_Rest_Dis=0
       NP_Rest_Tor=0

       !---- Dimension for AFIX ----!
       nr=0
       do i=1,file_dat%nlines
          line=adjustl(file_dat%line(i))
          if (u_case(line(1:4)) /= "AFIX") cycle
          call Cut_string(line)
          call Get_Words(line,car,nc)
          nr=nr+nc/3
       end do
       if (nr >0) then
          allocate(Ang_Rest(nr))
          ang_rest%aobs =0.0
          ang_rest%acalc=0.0
          ang_rest%sigma=0.0
          ang_rest%p(1) = 0
          ang_rest%p(2) = 0
          ang_rest%STCode(1)=" "
          ang_rest%STCode(2)=" "
       end if

       !---- Dimension for DFIX ----!
       nr=0
       do i=1,file_dat%nlines
          line=adjustl(file_dat%line(i))
          if (u_case(line(1:4)) /= "DFIX") cycle
          call Cut_string(line)
          call Get_Words(line,car,nc)
          nr=nr+nc/2
          if (modulo(nc,2) == 0) nr=nr-1
       end do
       if (nr > 0) then
          allocate(Dis_Rest(nr))
          dis_rest%dobs =0.0
          dis_rest%dcalc=0.0
          dis_rest%sigma=0.0
          dis_rest%p(1) = 0
          dis_rest%p(2) = 0
          dis_rest%STCode=" "
       end if

       !---- Dimension for TFIX ----!
       nr=0
       do i=1,file_dat%nlines
          line=adjustl(file_dat%line(i))
          if (u_case(line(1:4)) /= "TFIX") cycle
          call Cut_string(line)
          call Get_Words(line,car,nc)
          nr=nr+nc/4
       end do
       if (nr > 0) then
          allocate(Tor_Rest(nr))
          tor_rest%tobs=0.0
          tor_rest%tcalc=0.0
          tor_rest%sigma=0.0
          tor_rest%p(1) = 0
          tor_rest%p(2) = 0
          tor_rest%STCode(1)=" "
          tor_rest%STCode(2)=" "
          tor_rest%STCode(3)=" "
       end if
    End Subroutine Allocate_RestParam_Single

    !!--++
    !!--++ Subroutine Allocate_RestParam_Mult(file_dat,ndis_rest,nang_rest,ntor_rest)
    !!--++   Type(file_list_type),dimension(:),  intent( in) :: file_dat
    !!==++   integer,              dimension(:), intent(out) :: ndis_rest
    !!==++   integer,              dimension(:), intent(out) :: nang_rest
    !!==++   integer,              dimension(:), intent(out) :: ntor_rest
    !!--++
    !!--++    Allocate vectors Ang_Rest, Dist_Rest, Tor_Rest.
    !!--++    This is for a context of multiple phases. All restrainst
    !!--++    are stored in single array Ang_Rest, Dist_Rest, Tor_Rest, however
    !!--++    the information about the phases is stored in xxx_rest integer
    !!--++    arrays, from which one can know to which phase belong a particular
    !!--++    restrain. This works only if there is a single line per restraint
    !!--++    in the restraint files. The arrays xxx_rest stores the number of
    !!--++    restraints of each type in each phase.
    !!--++
    !!--++ Update: March-2005, May-2015
    !!
    Module Subroutine Allocate_RestParam_Mult(file_dat,ndis_rest,nang_rest,ntor_rest)
       !---- Arguments ----!
       Type(file_list_type), dimension(:), intent( in) :: file_dat
       integer,              dimension(:), intent(out) :: ndis_rest
       integer,              dimension(:), intent(out) :: nang_rest
       integer,              dimension(:), intent(out) :: ntor_rest

       !---- Local variables ----!
       character(len=132)              :: line
       character(len=15),dimension(40) :: car
       integer                         :: i,nc,nr,nfiles,nf

       if (allocated(Ang_Rest)) deallocate(Ang_Rest)
       if (allocated(Dis_Rest)) deallocate(Dis_Rest)
       if (allocated(Tor_Rest)) deallocate(Tor_Rest)

       NP_Rest_Ang=0
       NP_Rest_Dis=0
       NP_Rest_Tor=0
       nfiles=size(ndis_rest)  !number of phases
       ndis_rest=0; nang_rest=0; ntor_rest=0
       !---- Dimension for AFIX ----!
       nr=0
       do nf=1,nfiles
         nang_rest(nf)=0
         do i=1,file_dat(nf)%nlines
            line=adjustl(file_dat(nf)%line(i))
            if (u_case(line(1:4)) /= "AFIX") cycle
            call Cut_string(line)
            call Get_Words(line,car,nc)
            nr=nr+nc/3
            nang_rest(nf)=nang_rest(nf)+1
         end do
       end do
       if (nr > 0) then
          allocate(Ang_Rest(nr))
          ang_rest%aobs =0.0
          ang_rest%acalc=0.0
          ang_rest%sigma=0.0
          ang_rest%p(1) = 0
          ang_rest%p(2) = 0
          ang_rest%STCode(1)=" "
          ang_rest%STCode(2)=" "
       end if

       !---- Dimension for DFIX ----!
       nr=0
       do nf=1,nfiles
         ndis_rest(nf)=0
         do i=1,file_dat(nf)%nlines
            line=adjustl(file_dat(nf)%line(i))
            if (u_case(line(1:4)) /= "DFIX") cycle
            call Cut_string(line)
            call Get_Words(line,car,nc)
            nr=nr+nc/2
            if (modulo(nc,2) == 0) nr=nr-1
            ndis_rest(nf)=ndis_rest(nf)+1
         end do
       end do
       if (nr > 0) then
          allocate(Dis_Rest(nr))
          dis_rest%dobs =0.0
          dis_rest%dcalc=0.0
          dis_rest%sigma=0.0
          dis_rest%p(1) = 0
          dis_rest%p(2) = 0
          dis_rest%STCode=" "
       end if

       !---- Dimension for TFIX ----!
       nr=0
       do nf=1,nfiles
         ntor_rest(nf)=0
         do i=1,file_dat(nf)%nlines
            line=adjustl(file_dat(nf)%line(i))
            if (u_case(line(1:4)) /= "TFIX") cycle
            call Cut_string(line)
            call Get_Words(line,car,nc)
            nr=nr+nc/4
            ntor_rest(nf)=ntor_rest(nf)+1
         end do
       end do
      if (nr > 0) then
          allocate(Tor_Rest(nr))
          tor_rest%tobs=0.0
          tor_rest%tcalc=0.0
          tor_rest%sigma=0.0
          tor_rest%p(1) = 0
          tor_rest%p(2) = 0
          tor_rest%STCode(1)=" "
          tor_rest%STCode(2)=" "
          tor_rest%STCode(3)=" "
       end if
    End Subroutine Allocate_RestParam_Mult

    !!---- Subroutine Allocate_VParam(N)
    !!----    integer, intent(in) :: N
    !!----
    !!----    Allocate vectors V_Vec, V_Bounds, V_Name, V_Bcon, V_Shift, V_list
    !!----    If N is equal zero it deallocates the vectors
    !!----
    !!---- Update: March - 2005
    !!
    Module Subroutine Allocate_VParam(N)
       !---- Arguments ----!
       integer, intent(in) :: N

       if (allocated(V_Vec))    deallocate(V_Vec)
       if (allocated(V_Save))   deallocate(V_Save)
       if (allocated(V_Vec_Std))deallocate(V_Vec_Std)
       if (allocated(V_Name))   deallocate(V_Name)
       if (allocated(V_Bounds)) deallocate(V_Bounds)
       if (allocated(V_BCon))   deallocate(V_BCon)
       if (allocated(V_Shift))  deallocate(V_Shift)
       if (allocated(V_List))   deallocate(V_List)

       if (N > 0) then
          allocate(V_Vec(n))
          V_Vec=0.0
          allocate(V_Save(n))
          V_Save=0.0
          allocate(V_Vec_Std(n))
          V_Vec_Std=0.0
          allocate(V_Name(n))
          V_Name=" "
          allocate(V_Bounds(3,n))
          V_Bounds=0.0
          allocate(V_BCon(n))
          V_BCon=0
          allocate(V_Shift(n))
          V_Shift=0.0
          allocate(V_List(n))
          V_list=0
          np_max=n
       else
          np_max=0
       end if
    End Subroutine Allocate_VParam

End Submodule KWC_Allocation
