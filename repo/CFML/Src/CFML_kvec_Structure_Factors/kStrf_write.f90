SubModule (CFML_kvec_Structure_Factors) kStrf_write
   implicit none
   Contains
    !!----
    !!---- Module Subroutine Write_Structure_Factors(Lun,Reflex,Grp)
    !!----    integer,               intent(in) :: Lun
    !!----    type(MagH_List_Type),  intent(in) :: Reflex
    !!----    type(MagSymm_k_Type),  intent(in) :: Grp
    !!----
    !!----    Writes in logical unit=lun the list of structure factors
    !!----    contained in the array hkl
    !!----
    !!---- Update: April - 2005
    !!
    Module Subroutine Write_Mag_Structure_Factors(Lun,Reflex,Grp)
       !---- Argument ----!
       integer,               intent(in) :: lun
       type(MagH_List_Type),  intent(in) :: Reflex
       type(MagSymm_k_Type),  intent(in) :: Grp

       !---- Local Variables ----!
       integer                     :: i,nv, sig, mul
       real(kind=cp)               :: sqMiV, dspc
       integer,dimension(3)        :: h
       real(kind=cp), dimension(3) :: vk,hr

       write(unit=lun,fmt="(/,a)")   "    LIST OF REFLECTIONS AND MAGNETIC STRUCTURE FACTORS"
       write(unit=lun,fmt="(a,/)")   "    =================================================="
       write(unit=lun,fmt="(a,i6)") " => Total number of reflections  : ",reflex%Nref
       write(unit=lun,fmt="(a,i6)") " => Number of propagation vectors: ",Grp%nkv
       do i=1,Grp%nkv
          write(unit=lun,fmt="(a,i2,a,3f8.4,a)") " => Propagation vectors #",i," = (",Grp%kvec(:,i)," )"
       end do

       write(unit=lun,fmt="(/,a,/)") &
        "    Hr      Kr      Lr       H   K   L   nvk   Mult   dspc      |MiV|^2     Mrx      Mry      Mrz      "// &
        "Mix      Miy      Miz     MiVrx    MiVry    MiVrz    MiVix    MiViy    MiViz"
       do i=1,reflex%Nref
          hr=reflex%Mh(i)%h
          sig=reflex%Mh(i)%signp
          mul=reflex%Mh(i)%mult
          nv =reflex%Mh(i)%Num_k
          vk=Grp%kvec(:,nv)
          h=nint(hr+sig*vk)
          sqMiV= reflex%Mh(i)%sqMiV
          dspc=0.5/reflex%Mh(i)%S
          write(unit=lun,fmt="(3f8.3,tr2,3i4,i5,i6,f9.4,f13.5,12f9.4)") hr,h, -sig*nv, mul, dspc,sqMiV, &
               real(reflex%Mh(i)%MsF),aimag(reflex%Mh(i)%MsF), real(reflex%Mh(i)%MiV),aimag(reflex%Mh(i)%MiV)
       end do

    End Subroutine Write_Mag_Structure_Factors

End SubModule kStrf_write
