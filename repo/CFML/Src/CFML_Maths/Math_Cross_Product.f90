!!----
!!---- SUBMODULE CFML_Math_3D
!!----
!!----
!!
Submodule (CFML_Maths) Math_Cross_Product
 Contains

    !!----
    !!---- CROSS_PRODUCT_C
    !!----    Calculates the cross product of the complex vectors u and v
    !!----    Vectors, w = u x v, are given in cartesian components.
    !!----
    !!---- 04/04/2019
    !!
    Pure Module Function Cross_Product_C(u,v) Result(w)
       !---- Argument ----!
       complex(kind=cp), dimension(3), intent( in) :: u  ! Vector 1
       complex(kind=cp), dimension(3), intent( in) :: v  ! Vector 2
       complex(kind=cp), dimension(3)              :: w  ! u x v

       w(1)=u(2)*v(3)-u(3)*v(2)
       w(2)=u(3)*v(1)-u(1)*v(3)
       w(3)=u(1)*v(2)-u(2)*v(1)

    End Function Cross_Product_C

    !!----
    !!---- CROSS_PRODUCT_R
    !!----    Calculates the cross product of vectors u and v
    !!----    Vectors, w= u x v, are given in cartesian components.
    !!----
    !!---- 04/04/2019
    !!
    Pure Module Function Cross_Product_R(u,v) Result(w)
       !---- Argument ----!
       real(kind=cp), dimension(3), intent( in) :: u   ! Vector 1
       real(kind=cp), dimension(3), intent( in) :: v   ! Vector 2
       real(kind=cp), dimension(3)              :: w   ! u x v

       w(1)=u(2)*v(3)-u(3)*v(2)
       w(2)=u(3)*v(1)-u(1)*v(3)
       w(3)=u(1)*v(2)-u(2)*v(1)

    End Function Cross_Product_R

    !!----
    !!---- CROSS_PRODUCT_I
    !!----    Calculates the cross product of integer vectors u and v
    !!----    In the indices are givent w.r.t the direct lattice, the cross product
    !!----    are indices w.r.t. reciprocal lattice and viceversa.
    !!----
    !!---- 04/04/2019
    !!
    Pure Module Function Cross_Product_I(u,v) Result(w)
       !---- Argument ----!
       integer, dimension(3), intent( in) :: u    ! Vector 1
       integer, dimension(3), intent( in) :: v    ! Vector 2
       integer, dimension(3)              :: w    ! u x v

       w(1)=u(2)*v(3)-u(3)*v(2)  ! i  j   k !
       w(2)=u(3)*v(1)-u(1)*v(3)  !u1  u2  u3! = (u2.v3 - u3.v2)i + (v1.u3 - u1.v3)j + (u1.v2-u2.v1)k
       w(3)=u(1)*v(2)-u(2)*v(1)  !v1  v2  v3!

    End Function Cross_Product_I

End Submodule Math_Cross_Product
