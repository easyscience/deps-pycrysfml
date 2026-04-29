  Program Cell_Relations
    !use CFML_GlobalDeps,       only: err_CFML
    use CFML_Metrics,          only: Cell_G_Type,   &
                                     Change_Setting_Cell,Set_Crystal_Cell, Write_Crystal_Cell,   &
                                     get_primitive_cell
    use CFML_gSpaceGroups,     only: SPG_type, Set_SpaceGroup, Get_Symb_From_Mat
    use CFML_Strings,          only: l_case, Get_Separator_Pos
    use CFML_Maths,            only: Inverse_Matrix,determ3D
    use CFML_Reflections,      only: RefList_Type,Gener_Reflections !Hkl_Gen_Sxtal
    implicit none
    integer, parameter          :: n_ref=300, nk=24
    integer, dimension(2)       :: pos
    real,    dimension(3)       :: cel1,ang1,cel2,ang2,cel,ang,pcel2,pang2,k1=0.0,k2=0.0
    real,    dimension(6)       :: bestsol
    integer, dimension(3,3)     :: mat,bestmat
    real,    dimension(3,nk)    :: k_ind
    real,    dimension(3,3)     :: base,trans,newc,gn,tp,stp,itp,istp,trans_inv,ttrans,ttrans_inv
    real,    dimension(3)       :: hkl_bas,k_bas,hkl
    type(RefList_Type)          :: hkl_sup
    class(SPG_type),allocatable :: SpGr
    character(len=1)            :: lat_type,slat_type
    character(len=50)           :: abc_symb,iabc_symb
    type (Cell_G_Type)          :: cell, supercell, pcell, psupercell
    logical :: possible,centred,scentred,k1_given=.false.,k2_given=.false.

    !Use the information about the centring lattice to obtain the primitive cell
    !of the original sublattice. Generates supercells by integer linear combinations
    !Take the primitive lattice of the target supercell and get the reduced cell parameters
    !Compare the generated cells with the target supercell within a tolerance for each cell
    !parameter and each angle. Output the matrices giving cells within tolerance.

    character(len=256) :: fileinp, fileout, line
    integer :: i,j,lun=1,lout=2, ier,i1,i2,i3,i4,i5,i6,i7,i8,i9,iratio,n,irvol,ncar
    integer :: ia1,ia2,ib1,ib2,ic1,ic2, narg,isol,nn,im, neq,Num_Ref,n_kind=0
    real    :: tol1,tol2,rvol,rfac,bestr,sm,start,fin


    narg=COMMAND_ARGUMENT_COUNT()
    if(narg /= 0) then
      call GET_COMMAND_ARGUMENT(1,fileinp)
      open(unit=lun,file=trim(fileinp),status="old",action="read",position="rewind",iostat=ier)
    end if
    if(ier /=0 .or. narg == 0) then
    ! reading data
      do
        write(unit=*,fmt="(a)") " => Enter the name of the input file: "
        read(unit=*,fmt="(a)") fileinp
        open(unit=lun,file=trim(fileinp),status="old",action="read",position="rewind",iostat=ier)
        if(ier /= 0) cycle
        exit
      end do
      write(unit=*,fmt="(a)") " => Enter the name of the output file: "
      read(unit=*,fmt="(a)") fileout
    else if (narg == 2) then
      call GET_COMMAND_ARGUMENT(2,fileout)
    else if (narg == 1) then
      i=index(fileinp,".", back=.true.)
      if(i /= 0) then
        fileout=fileinp(1:i)//"out"
      else
        fileout=trim(fileinp)//".out"
      end if
    end if

    open(unit=lout,file=trim(fileout),status="replace",action="write")
    do
      read(unit=lun,fmt="(a)",iostat=ier) line
      if(ier /= 0) exit
      line=l_case(line)
      i=index(line,"cell")
      if( i /= 0) read(unit=line(5:),fmt=*) cel1,ang1, lat_type
      i=index(line,"super")
      if( i /= 0) read(unit=line(6:),fmt=*) cel2,ang2, slat_type
      i=index(line,"tol")
      if( i /= 0) read(unit=line(4:),fmt=*) tol1,tol2
      i=index(line,"indices")
      if( i /= 0) read(unit=line(8:),fmt=*) ia1,ia2,ib1,ib2,ic1,ic2
      i=index(line,"k1")
      if( i /= 0) then
      	read(unit=line(i+3:),fmt=*) k1
      	k1_given=.true.
      end if
      i=index(line,"k2")
      if( i /= 0) then
      	read(unit=line(i+3:),fmt=*) k2
      	k2_given=.true.
      end if
    end do
    centred=.true.
    scentred=.true.
    if(lat_type == "P" .or. lat_type == "p") centred=.false.
    if(slat_type == "P" .or. slat_type == "p") scentred=.false.
    call Set_Crystal_Cell(cel1,ang1,Cell)
    write(unit=lout,fmt="(a)") "  ------------------------------"
    write(unit=lout,fmt="(a)") "  PROGRAM: Search_Cell_Relations"
    write(unit=lout,fmt="(a)") "  ------------------------------"
    write(unit=*,fmt="(/a)") "  ------------------------------"
    write(unit=*,fmt="(a)") "  PROGRAM: Search_Cell_Relations"
    write(unit=*,fmt="(a)") "  ------------------------------"
    write(unit=lout,fmt="(/,a)") "  => INPUT SUBCELL DATA"
    call Write_Crystal_Cell(Cell,Lout)
    call Get_Primitive_Cell(lat_type,cell,pcell,tp)
    if(centred) then
      itp=Inverse_Matrix(tp)
      write(unit=lout,fmt="(/,a)") "  => Input subcell data in primitive setting"
      write(unit=lout,fmt="(/,a)") "  => Transformation matrix to primitive cell and inverse:"
      do i=1,3
         write(unit=lout,fmt="(2(a,3f10.4))")"                 ",tp(i,:),"                 ",itp(i,:)
      end do
      call Write_Crystal_Cell(pCell,Lout)
    else
      itp=tp
    end if
    base=transpose(pCell%Cr_Orth_cel)   !Provides a matrix with rows equal to the basis vectors in cartesian components
    call Set_Crystal_Cell(cel2,ang2,SuperCell)   !SuperCell%Cr_Orth_cel
    write(unit=lout,fmt="(/,a)") "  => INPUT SUPERCELL DATA"
    call Write_Crystal_Cell(SuperCell,Lout)
    call Get_Primitive_Cell(slat_type,SuperCell,pSuperCell,stp)
    if(scentred) then
      istp=Inverse_Matrix(stp)
      write(unit=lout,fmt="(/,a)") "  => Input supercell data in primitive setting"
      write(unit=lout,fmt="(/,a)") "  => Transformation matrix to primitive cell and inverse:"
      do i=1,3
         write(unit=lout,fmt="(2(a,3f10.4))")"                 ",stp(i,:),"                 ",istp(i,:)
      end do
      call Write_Crystal_Cell(pSuperCell,Lout)
    else
      istp=stp
    end if
    call Set_SpaceGroup(slat_type//" 1",SpGr)
    pcel2=pSuperCell%cell
    pang2=pSuperCell%ang
    rvol=pSuperCell%Vol/pcell%Vol
    irvol=nint(rvol)
    iratio= (ia2-ia1+1)*(ia2-ia1+1)*(ia2-ia1+1)*(ib2-ib1+1)*(ib2-ib1+1)*(ib2-ib1+1)*(ic2-ic1+1)*(ic2-ic1+1)*(ic2-ic1+1)
    write(unit=*,fmt="(a,i3)")  " => The volume  ratio between cells is ",irvol
    write(unit=*,fmt="(a,i10)") " => The maximum number of cell calculations is ",iratio
    im=iratio/2000
    n=0
    bestr=1000000.0
    nn=0
    isol=0
    call cpu_time(start)
    do i1=ia1,ia2                        !         |i1  i2  i3|
     do i2=ib1,ib2                       ! Trans = |i4  i5  i6|  = mat
      do i3=ic1,ic2                      !         |i7  i8  i9|
       do i4=ia1,ia2
        do i5=ib1,ib2                    !   |A|   |i1  i2  i3| |a|
         do i6=ic1,ic2                   !   |B| = |i4  i5  i6| |b|
          do i7=ia1,ia2                  !   |C|   |i7  i8  i9| |c|
           do i8=ib1,ib2
            do i9=ic1,ic2
             j=i1*i5*i9+i4*i8*i3+i2*i6*i7-i3*i5*i7-i8*i6*i1-i2*i4*i9     !determinant (much faster than calling determ3D)
             !if( j /= irvol) cycle
             if( abs(j-irvol) > 1) cycle
             mat=transpose(reshape((/i1,i2,i3,i4,i5,i6,i7,i8,i9/),(/3,3/)))
             trans=real(mat)
             newc=matmul(trans,base)
             gn=matmul(newc,transpose(newc)) !Metric tensor
             possible=.true.
             rfac=0.0
             !---- Calculate new cell parameters from the new metric tensor
             do i=1,3
                Cel(i)=sqrt(gn(i,i))
                sm=abs(cel(i)-pcel2(i))
                rfac=rfac+sm
                if( sm > tol1) possible=.false.
             end do
             nn=nn+1
             if(mod(nn,im) == 0)  write(unit=*,fmt="(a)",advance="no") "."
             if(.not. possible) cycle
             ang(1)=acosd(Gn(2,3)/(cel(2)*cel(3)))
             ang(2)=acosd(Gn(1,3)/(cel(1)*cel(3)))
             ang(3)=acosd(Gn(1,2)/(cel(1)*cel(2)))

             do i=1,3
               sm=abs(ang(i)-pang2(i))
               rfac=rfac+sm
               if( sm > tol2) possible=.false.
             end do
             rfac=rfac/6.0
             if(possible) then  !Found a candidate to the good cell
               n=n+1

               if(rfac < bestr) then
                 neq=0
                 bestr=rfac
                 bestsol(1:3)=cel
                 bestsol(4:6)=ang
                 isol=n
                 bestmat=mat
                 write(unit=*,fmt="(/,a,i10,a,f10.5)") " => The best solution for the moment is for the calculation number ",nn,&
                                                    " with average deviation: ",rfac
                 write(unit=*,fmt="(a)") " => The transformation matrix between primitive cells is: "
                 do i=1,3
                   write(unit=*,fmt="(a, 3i4)") "                           ",mat(i,:)
                 end do
                 write(unit=*,fmt="(a,3f8.4,3f8.2)") " => Observed   primitive super cell: ",pcel2,pang2
                 write(unit=*,fmt="(a,3f8.4,3f8.2)") " => Calculated primitive unit  cell: ",cel,ang
               else if(rfac == bestr) then
                 neq=neq+1
                 write(unit=*,fmt="(/,a,i3,a,i10)") " => Another equivalent to the best solution",neq, " calculation number: ",nn
               end if

               write(unit=lout,fmt="(/,a,i4)") " => Solution number:",n
               write(unit=lout,fmt="(a)") " => Transformation matrix between primitive cells: "
               do i=1,3
                 write(unit=lout,fmt="(a, 3i4)") "                           ",mat(i,:)
               end do
               write(unit=lout,fmt="(a,3f8.4,3f8.2)") " => Observed   primitive super cell: ",pcel2,pang2
               write(unit=lout,fmt="(a,3f8.4,3f8.2)") " => Calculated primitive unit  cell: ",cel,ang
               write(unit=lout,fmt="(a,f10.5)")       " => Average deviation: ",rfac
               if(scentred .or. centred) then
                 write(unit=lout,fmt="(a)") " => Transformation matrix between conventional cells: "
                 gn=matmul(istp,matmul(trans,tp))
                 do i=1,3
                   write(unit=lout,fmt="(a, 3f8.4)") "                           ",gn(i,:)
                 end do
               end if
               write(unit=lout,fmt="(a)") "    ----------------------------------------------------------------------"
             end if
            end do    !i9
           end do     !i8
          end do      !i7
         end do       !i6
        end do        !i5
       end do         !i4
      end do          !i3
     end do           !i2
    end do            !i1

    if(isol == 0) then
      write(unit=lout,fmt="(/,a)") " => No solution found! "
      write(unit=*,fmt="(/,/,a)")  " => No solution found! "
    else
      write(unit=lout,fmt="(a,i6,a,f10.5)") " => The best solution is the number ",isol,&
                                                       " with average deviation: ",bestr
      write(unit=lout,fmt="(a,i3,a)")  " => There are other ",neq," equivalent solutions in the list"
      write(unit=lout,fmt="(a,3f8.4,3f8.2)")  " => The corresponding primitive cell parameters are: ",bestsol

      call Set_Crystal_Cell(bestsol(1:3),bestsol(4:6),pSuperCell) !use primitive for convenience
      base=transpose(pSuperCell%Cr_Orth_cel)  !basis vectors of the primitive cell
      newc=matmul(real(istp),base)            !Transformation to conventional cell
      gn=matmul(newc,transpose(newc))         !Metric tensor of the conventional cell
      do i=1,3
         Cel(i)=sqrt(gn(i,i))
      end do
      ang(1)=acosd(Gn(2,3)/(cel(2)*cel(3)))
      ang(2)=acosd(Gn(1,3)/(cel(1)*cel(3)))
      ang(3)=acosd(Gn(1,2)/(cel(1)*cel(2)))

      write(unit=lout,fmt="(a,3f8.4,3f8.2)")  " => The corresponding conventional cell parameters are: ",cel,ang
      write(unit=lout,fmt="(a,3f8.4,3f8.2)")  " => The observed      conventional cell parameters are: ",cel2,ang2
      write(unit=lout,fmt="(a)") " => Transformation matrix between conventional cells: "
      !!--..    List Of Matrix Relationships For Crystallographic Applications
      !!--..
      !!--..    Small "t" is for transpose, inv(F) is the inverse of matrix F
      !!--..
      !!--..    Basis vectors as symbolic matrices
      !!--..       At = (a,b,c)  At'=(a',b',c') ;  At* = (a*,b*,c*)  At*'=(a*',b*',c*')
      !!--..
      !!--..    Direct and reciprocal metric tensors: G, G*=inv(G)
      !!--..    X  column vector in     direct space, referred to basis A
      !!--..    X* column vector in reciprocal space, referred to basis A*
      !!--..
      !!--..       A'  = M  A           X'  = inv(Mt) X
      !!--..       A*  = G* A           X*  =   G     X
      !!--..       A*' = inv(Mt) A*     X*' =   M     X*
      !!--..
      !!--..       G' = M G Mt          G*' = inv(Mt) G* inv(M)


      trans=matmul(istp,matmul(real(bestmat),tp))
      trans_inv=Inverse_Matrix(trans)
      !call Get_Symb_From_Mat(trans,abc_symb,(/"a","b","c"/))
      abc_symb=Get_Symb_From_Mat(trans,[0.0,0.0,0.0],.true.)
      call Get_Separator_Pos(abc_symb,",",pos,ncar)
      abc_symb=" A="//abc_symb(1:pos(1)-1)//"  B="//abc_symb(pos(1)+1:pos(2)-1)//"  C="//abc_symb(pos(2)+1:)
      !call Get_Symb_From_Mat(trans_inv,iabc_symb,(/"A","B","C"/))
      iabc_symb=Get_Symb_From_Mat(trans_inv,[0.0,0.0,0.0],.true.)
      call Get_Separator_Pos(iabc_symb,",",pos,ncar)
      iabc_symb=" a="//iabc_symb(1:pos(1)-1)//"  b="//iabc_symb(pos(1)+1:pos(2)-1)//"  c="//iabc_symb(pos(2)+1:)

      write(unit=lout,fmt="(/t17,a,t64,a/)") trim(abc_symb),trim(iabc_symb)
      write(unit=lout,fmt="(a, 3f8.4,a,3f8.4,a)")      "       | A |    /",trans(1,:)," \ | a |      | a |   /",trans_inv(1,:)," \ | A |"
      write(unit=lout,fmt="(a, 3f8.4,a,3f8.4,a,f8.4)") "       | B | = | ",trans(2,:),"  || b |      | b |= | ",trans_inv(2,:),"  || B |       determinant:",determ3D(trans)
      write(unit=lout,fmt="(a, 3f8.4,a,3f8.4,a)")      "       | C |    \",trans(3,:)," / | c |      | c |   \",trans_inv(3,:)," / | C |"

      write(unit=*,fmt="(//a)") " => FINAL conventional cell transformation:"
      write(unit=*,fmt="(/t17,a,t64,a/)") trim(abc_symb),trim(iabc_symb)
      write(unit=*,fmt="(a, 3f8.4,a,3f8.4,a)")      "       | A |    /",trans(1,:)," \ | a |      | a |   /",trans_inv(1,:)," \ | A |"
      write(unit=*,fmt="(a, 3f8.4,a,3f8.4,a,f8.4)") "       | B | = | ",trans(2,:),"  || b |      | b |= | ",trans_inv(2,:),"  || B |       determinant:",determ3D(trans)
      write(unit=*,fmt="(a, 3f8.4,a,3f8.4,a)")      "       | C |    \",trans(3,:)," / | c |      | c |   \",trans_inv(3,:)," / | C |"
      ttrans=transpose(trans)
      ttrans_inv=transpose(trans_inv)

      write(unit=lout,fmt="(/,a, 3f8.4,a,3f8.4,a)")    "       | A*|    /",ttrans(1,:)," \ | a*|      | a*|   /",ttrans_inv(1,:)," \ | A*|"
      write(unit=lout,fmt="(a, 3f8.4,a,3f8.4,a,f8.4)") "       | B*| = | ",ttrans(2,:),"  || b*|      | b*|= | ",ttrans_inv(2,:),"  || B*|       determinant:",determ3D(trans_inv)
      write(unit=lout,fmt="(a, 3f8.4,a,3f8.4,a)")      "       | C*|    \",ttrans(3,:)," / | c*|      | c*|   \",ttrans_inv(3,:)," / | C*|"

      !Search propagation vectors relating the two unit cells
      !Generate allowed reflections in the superstructure cell up to s=0.5
      !call Hkl_Gen_Sxtal(supercell,SpGr,0.0,0.25,Num_Ref,hkl_sup)
      !call Gener_Reflections(supercell,Sintlmax,Mag,Reflex,SpG,kinfo,order,powder,mag_only,Friedel)
      call Gener_Reflections(supercell,0.0,0.25,hkl_sup,SpGr)
      Num_ref=hkl_sup%Nref
      write(unit=lout,fmt="(/,a)") " ==============================================================="
      write(unit=lout,fmt="(a)")   " Indexing of superstructure reflection in the substructure basis"
      write(unit=lout,fmt="(a,/)") " ==============================================================="
      write (unit=lout,fmt="(a)") "         Num_Ref   Hs    Ks    Ls         Hb      Kb      Lb     hb    kb    lb           k-vector"
      do i=1,Num_ref
      	hkl_bas=matmul(trans_inv,hkl_sup%Ref(i)%h)
      	k_bas=hkl_bas-real(nint(hkl_bas))
      	call k_independent(k_bas)
      	write(unit=lout,fmt="(t8,i8,3i6,tr4,3f8.4,3i6,tr4,3f9.5)") i, hkl_sup%Ref(i)%h,hkl_bas,nint(hkl_bas),k_bas
      end do

      if(k1_given) then
      	k2=matmul(trans,k1)
        write(unit=lout,fmt="(/,2(a,3f9.4),a)") "       k-vector in basis cell: (",k1,")  Transformed to supercell: (",k2,")"
      else if(k2_given) then
      	k1=matmul(trans_inv,k2)
        write(unit=lout,fmt="(/,2(a,3f8.4),a)") "       k-vector in supercell: (",k2,")  Transformed to basis cell: (",k1,")"
        write(unit=lout,fmt="(/,a)") " ==============================================================="
        write(unit=lout,fmt="(a)")   " Indexing of superstructure satellites in the substructure basis"
        write(unit=lout,fmt="(a,/)") " ==============================================================="
        write(unit=lout,fmt="(a)") "         Num_Ref     Hs      Ks      Ls         Hb      Kb      Lb     hb    kb    lb           k-vector"

        do i=1,Num_ref
        	hkl=hkl_sup%Ref(i)%h+k2
        	hkl_bas=matmul(trans_inv,hkl)
        	k_bas=hkl_bas-real(nint(hkl_bas))
        	call k_independent(k_bas)
        	write(unit=lout,fmt="(t4,a,i8,3f8.4,tr4,3f8.4,3i6,tr4,3f9.5)") " +k ",i, hkl,hkl_bas,nint(hkl_bas),k_bas
        	hkl=hkl_sup%Ref(i)%h-k2
        	hkl_bas=matmul(trans_inv,hkl)
        	k_bas=hkl_bas-real(nint(hkl_bas))
        	call k_independent(k_bas)
        	write(unit=lout,fmt="(t4,a,i8,3f8.4,tr4,3f8.4,3i6,tr4,3f9.5)") " -k ",i, hkl,hkl_bas,nint(hkl_bas),k_bas
        end do
      end if
      write(unit=lout,fmt="(/,a,i3)") " => Number of independent k-vectors in the basic cell: ",n_kind
      do i=1,n_kind
      		write(unit=lout,fmt="(a,i2,a,3f9.5,a)") "  k-vector # ",i,"  (",k_ind(:,i)," )"
      end do
    end if

    call cpu_time(fin)
    write(unit=*,fmt="(/,a,f10.2,a)")     " => CPU-Time: ", fin-start," seconds"
    write(unit=lout,fmt="(/,a,f10.2,a)")  " => CPU-Time: ", fin-start," seconds"

    contains
    	subroutine k_independent(k)
    		real, dimension(3), intent(in):: k
    		real :: del=0.001
    		integer :: n,neg
    		if(sum(abs(k)) < del) return
    		!write(*,*) n_kind,k
    		if(n_kind == 0) then
    			n_kind=n_kind+1
    			neg=count(k+del < 0.0)
    			if(neg >= 2 .or. (neg == 1 .and. k(1)+del < 0.0)) then
       		  k_ind(:,n_kind) = -k
       		else
       		  k_ind(:,n_kind) =  k
       		end if
    		else
    			do n=1,n_kind
    				if(sum(abs(k-k_ind(:,n))) < del) return
    				if(sum(abs(k+k_ind(:,n))) < del) return
    			end do
    		  n_kind=n_kind+1
    			neg=count(k+del < 0.0)
    			if(neg >= 2 .or. (neg == 1 .and. k(1)+del < 0.0)) then
       		  k_ind(:,n_kind) = -k
       		else
       		  k_ind(:,n_kind) =  k
       		end if
    		end if
    	end subroutine k_independent

  End Program Cell_Relations
