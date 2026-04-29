!!----
!!---- SUBMODULE CFML_Math_General
!!----
!!----
!!
Submodule (CFML_Maths) Maths_Linear_Dependent
 implicit none
 Contains

    !!----
    !!--++ LINEAR_DEPENDENT_C
    !!--++
    !!--++    (OVERLOADED)
    !!--++    Provides the value .TRUE. if the vector A is linear dependent of the
    !!--++    vectors constituting the rows (columns) of the matrix B. In input nb & mb
    !!--++    are the number of rows and columns of B to be considered. The actual
    !!--++    dimension of vector a should be na=max(nb,mb).
    !!--++    The problem is equivalent to determine the rank (in algebraic sense)
    !!--++    of the composite matrix C(nb+1,mb)=(B/A) or C(nb,mb+1)=(B|A). In the first
    !!--++    case it is supposed that na = mb and in the second na = nb.
    !!--++    and the rank of B is min(nb, mb). If na /= nb and na /= mb an error condition
    !!--++    is generated
    !!--++
    !!--++    For the case of complex vectors in Cn the problem can be reduced to real vectors
    !!--++    of dimension R2n. Each complex vector contributes as two real vectors of dimension
    !!--++    2n: (R,I) and (-I,R). A complex vector V is linearly dependent on n complex vectors
    !!--++    if V can be written as: V = Sigma{j=1,n}(Cj.Vj), with Cj complex numbers and Vj
    !!--++    having n complex components. One may write:
    !!--++
    !!--++     V = Sigma{j=1,n}(Cj.Vj)
    !!--++     (R,I) = Sigma{j=1,n} (Cjr Vj + i Cji Vj) = Sigma{j=1,n} (Cjr (Rj,Ij) +  Cji (-Ij,Rj) )
    !!--++     (R,I) = Sigma{j=1,n} (aj (Rj,Ij) + bj (-Ij,Rj) )  = Sigma{j=1,2n} (Aj.Uj)
    !!--++     Were Uj=(Rj,Ij) and U(j+1)= (-Ij,Rj)
    !!--++
    !!--++    The function uses floating arithmetic for all types.
    !!--++
    !!--++ Update: February - 2005
    !!
    Module Function Linear_Dependent_C(A,na,B,nb,mb) Result(info)
       !---- Arguments ----!
       complex(kind=cp), dimension(:),   intent(in)  :: a
       complex(kind=cp), dimension(:,:), intent(in)  :: b
       integer,                          intent(in)  :: na,nb,mb
       logical                                       :: info

       !---- Local variables ----!
       integer                                                     :: r,n1
       real(kind=cp), parameter                                    :: tol= 100.0*epsilon(1.0_cp)
       real(kind=cp), dimension(2*max(nb+1,mb+1),2*max(nb+1,mb+1)) :: c

       !> Init
       info=.true.

       if (nb > size(b,1) .or. mb > size(b,2) .or. na > size(a) ) then
          err_cfml%ierr=1
          err_cfml%msg="MATHS@LINEAR_DEPENDENT_C: Error in dimension of input matrix or vector"
          return
       end if

       c=0.0_cp
       if ( na == mb) then
          n1=2*nb+1
          if (n1+1 > 2*mb) return !the vector is linear dependent
          c(1:nb,           1:mb) =  real(b(1:nb,1:mb))
          c(1:nb,     mb+1:mb+na) = aimag(b(1:nb,1:mb))
          c(nb+1:2*nb,      1:mb) =-aimag(b(1:nb,1:mb))
          c(nb+1:2*nb,mb+1:mb+na) =  real(b(1:nb,1:mb))
          c(n1,             1:mb) =  real(a(1:na))
          c(n1,      mb+1:mb+na ) = aimag(a(1:na))
          c(n1+1,           1:mb) =-aimag(a(1:na))
          c(n1+1,    mb+1:mb+na ) =  real(a(1:na))
          r=mrank(c,tol)
          if (r == min(n1+1,2*mb)) info=.false.

       else if( na == nb) then
          n1=2*mb+1
          if(n1+1 > 2*nb) return !the vector is linear dependent
          c(1:nb,           1:mb) =  real(b(1:nb,1:mb))
          c(nb+1:nb+na,     1:mb) = aimag(b(1:nb,1:mb))
          c(1:nb,      mb+1:2*mb) =-aimag(b(1:nb,1:mb))
          c(nb+1:nb+na,mb+1:2*mb) =  real(b(1:nb,1:mb))
          c(1:na,             n1) =  real(a(1:na))
          c(nb+1:nb+na,       n1) = aimag(a(1:na))
          c(1:na,           1+n1) =-aimag(a(1:na))
          c(nb+1:nb+na,     1+n1) =  real(a(1:na))
          r=mrank(c,tol)
          if (r == min(n1+1,2*nb)) info=.false.

       else
          err_cfml%ierr=1
          err_cfml%msg="MATHS@LINEAR_DEPENDENT_C: input dimension of vector incompatible with matrix"
       end if

    End Function Linear_Dependent_C

    !!----
    !!--++ Linear_Dependent_I
    !!--++
    !!--++    (OVERLOADED)
    !!--++    Provides the value .TRUE. if the vector A is linear dependent of the
    !!--++    vectors constituting the rows (columns) of the matrix B. In input nb & mb
    !!--++    are the number of rows and columns of B to be considered. The actual
    !!--++    dimension of vector a should be na=max(nb,mb).
    !!--++    The problem is equivalent to determine the rank (in algebraic sense)
    !!--++    of the composite matrix C(nb+1,mb)=(B/A) or C(nb,mb+1)=(B|A). In the first
    !!--++    case it is supposed that na = mb and in the second na = nb.
    !!--++    and the rank of B is min(nb, mb). If na /= nb and na /= mb an error condition
    !!--++    is generated
    !!--++    The function uses floating arithmetic for all types.
    !!--++
    !!--++ 03/04/2019
    !!
    Module Function Linear_Dependent_I(A,na,B,nb,mb) Result(info)
       !---- Arguments ----!
       integer, dimension(:),   intent(in)  :: a
       integer, dimension(:,:), intent(in)  :: b
       integer,                 intent(in)  :: na,nb,mb
       logical                              :: info

       !---- Local variables ----!
       integer                                                 :: r,n1
       real(kind=cp), parameter                                :: tol= 100.0*epsilon(1.0_cp)
       real(kind=cp), dimension(max(nb+1,mb+1),max(nb+1,mb+1)) :: c

       !> Init
       info=.true.

       if (nb > size(b,1) .or. mb > size(b,2) .or. na > size(a) ) then
          err_cfml%Ierr=2
          err_cfml%msg="MATHS@LINEAR_DEPENDENT_I: Error in dimension of input matrix or vector"
          return
       end if

       c=0.0_cp
       if ( na == mb) then
          n1=nb+1
          if (n1 > mb) return !the vector is linear dependent
          c(1:nb,1:mb)=real(b(1:nb,1:mb))
          c(n1,  1:mb)=real(a(1:na))      !C(nb+1,mb)
          r=mrank(c,tol)
          if (r == min(n1,mb)) info=.false.

       else if( na == nb) then
          n1=mb+1
          if(n1 > nb) return !the vector is linear dependent
          c(1:nb,1:mb)=real(b(1:nb,1:mb))
          c(1:nb,  n1)=real(a(1:na))     !C(nb,mb+1)
          r=mrank(c,tol)
          if (r == min(n1,nb)) info=.false.

       else
          err_cfml%ierr=1
          err_cfml%msg="MATHS@LINEAR_DEPENDENT_I: input dimension of vector incompatible with matrix"
       end if

    End Function Linear_Dependent_I

    !!----
    !!--++ LINEAR_DEPENDENT_R
    !!--++
    !!--++    (OVERLOADED)
    !!--++    Provides the value .TRUE. if the vector A is linear dependent of the
    !!--++    vectors constituting the rows (columns) of the matrix B. In input nb & mb
    !!--++    are the number of rows and columns of B to be considered. The actual
    !!--++    dimension of vector a should be na=max(nb,mb).
    !!--++    The problem is equivalent to determine the rank (in algebraic sense)
    !!--++    of the composite matrix C(nb+1,mb)=(B/A) or C(nb,mb+1)=(B|A). In the first
    !!--++    case it is supposed that na = mb and in the second na = nb.
    !!--++    and the rank of B is min(nb, mb). If na /= nb and na /= mb an error condition
    !!--++    is generated
    !!--++    The function uses floating arithmetic for all types.
    !!--++
    !!--++ 03/04/2019
    !!
    Module Function Linear_Dependent_R(A,na,B,nb,mb) Result(info)
       !---- Arguments ----!
       real(kind=cp), dimension(:),   intent(in)  :: a
       real(kind=cp), dimension(:,:), intent(in)  :: b
       integer,                       intent(in)  :: na,nb,mb
       logical                                    :: info

       !---- Local Variables ----!
       integer                                                 :: r,n1
       real(kind=cp), parameter                                :: tol= 100.0*epsilon(1.0_cp)
       real(kind=cp), dimension(max(nb+1,mb+1),max(nb+1,mb+1)) :: c

       !> Init
       info=.true.

       if (nb > size(b,1) .or. mb > size(b,2) .or. na > size(a) ) then
          err_cfml%ierr=1
          err_cfml%msg="MATHS@LINEAR_DEPENDENT_R: Error in dimension of input matrix or vector"
          return
       end if

       c=0.0_cp
       if ( na == mb) then    !Vector added as an additional row
          n1=nb+1
          if(n1 > mb) return !the vector is linear dependent
          c(1:nb,1:mb)=b(1:nb,1:mb)
          c(n1,  1:mb)=a(1:na)      !C(nb+1,mb)
          r=mrank(c,tol)
          if (r == min(n1,mb)) info=.false.

       else if( na == nb) then   !Vector added as an additional column
          n1=mb+1
          if(n1 > nb) return !the vector is linear dependent
          c(1:nb,1:mb)=b(1:nb,1:mb)
          c(1:nb,  n1)=a(1:na)     !C(nb,mb+1)
          r=mrank(c,tol)
          if (r == min(n1,nb)) info=.false.
       else
          err_cfml%ierr=1
          err_cfml%msg="MATHS@LINEAR_DEPENDENT_R: input dimension of vector incompatible with matrix"
       end if

    End Function Linear_Dependent_R


End Submodule Maths_Linear_Dependent
