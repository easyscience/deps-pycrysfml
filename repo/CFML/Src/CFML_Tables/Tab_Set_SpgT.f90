!!----
!!----
!!----
SubModule (CFML_Symmetry_Tables) TAB_Set_SpgT
   Implicit none
   Contains
   !!----
   !!----
   !!----
   !!----
   !!----
   !!----
   !!
  Module Subroutine Set_Shubnikov_Info()

      if(shubnikov_info_loaded) return
      if (.not. allocated(shubnikov_info) ) allocate(shubnikov_info(NUM_SHUBNIKOV) )

     Shubnikov_info(   1)= Shub_Spgr_Info_Type("1.1    ","P1          ","1.1.1      ","P1            ","P1.1                       ","P 1                 ",  &
                                               "x,y,z,1                                                                                                 ")
     Shubnikov_info(   2)= Shub_Spgr_Info_Type("1.2    ","P11'        ","1.2.2      ","P11'          ","P1.1'                      ","P 1'                ",  &
                                               "x,y,z,-1                                                                                                ")
     Shubnikov_info(   3)= Shub_Spgr_Info_Type("1.3    ","P_S1        ","1.3.3      ","P_2s1         ","P1.1'_c[P1]                ","P 1 1'c             ",  &
                                               "x,y,z+1/2,-1                                                                                            ")
     Shubnikov_info(   4)= Shub_Spgr_Info_Type("2.4    ","P-1         ","2.1.4      ","P-1           ","P-1.1                      ","-P 1                ",  &
                                               "-x,-y,-z,1                                                                                              ")
     Shubnikov_info(   5)= Shub_Spgr_Info_Type("2.5    ","P-11'       ","2.2.5      ","P-11'         ","P-1.1'                     ","-P 1'               ",  &
                                               "-x,-y,-z,1;x,y,z,-1                                                                                     ")
     Shubnikov_info(   6)= Shub_Spgr_Info_Type("2.6    ","P-1'        ","2.3.6      ","P-1'          ","P-1'                       ","P -1'               ",  &
                                               "-x,-y,-z,-1                                                                                             ")
     Shubnikov_info(   7)= Shub_Spgr_Info_Type("2.7    ","P_S-1       ","2.4.7      ","P_2s-1        ","P-1.1'_c[P-1]              ","-P 1 1'c            ",  &
                                               "-x,-y,-z,1;x,y,z+1/2,-1                                                                                 ")
     Shubnikov_info(   8)= Shub_Spgr_Info_Type("3.1    ","P2          ","3.1.8      ","P2            ","P2.1                       ","P 2y                ",  &
                                               "-x,y,-z,1                                                                                               ")
     Shubnikov_info(   9)= Shub_Spgr_Info_Type("3.2    ","P21'        ","3.2.9      ","P21'          ","P2.1'                      ","P 2y 1'             ",  &
                                               "-x,y,-z,1;x,y,z,-1                                                                                      ")
     Shubnikov_info(  10)= Shub_Spgr_Info_Type("3.3    ","P2'         ","3.3.10     ","P2'           ","P2'                        ","P 2y'               ",  &
                                               "-x,y,-z,-1                                                                                              ")
     Shubnikov_info(  11)= Shub_Spgr_Info_Type("3.4    ","P_a2        ","3.4.11     ","P_2a2         ","P2.1'_a[P2]                ","P 2y 1'a            ",  &
                                               "-x,y,-z,1;x+1/2,y,z,-1                                                                                  ")
     Shubnikov_info(  12)= Shub_Spgr_Info_Type("3.5    ","P_b2        ","3.5.12     ","P_2b2         ","P2.1'_b[P2]                ","P 2y 1'b            ",  &
                                               "-x,y,-z,1;x,y+1/2,z,-1                                                                                  ")
     Shubnikov_info(  13)= Shub_Spgr_Info_Type("5.17   ","C_a2        ","3.6.13     ","P_C2          ","C2.1'_a[P2]                ","C 2y 1'a            ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,-z,1;x+1/2,y,z,-1                                                                  ")
     Shubnikov_info(  14)= Shub_Spgr_Info_Type("4.11   ","P_b2_1      ","3.7.14     ","P_2b2'        ","P2_1.1'_b[P2]              ","P 2yb 1'b           ",  &
                                               "-x,y+1/2,-z,1;x,y+1/2,z,-1                                                                              ")
     Shubnikov_info(  15)= Shub_Spgr_Info_Type("4.7    ","P2_1        ","4.1.15     ","P2_1          ","P2_1.1                     ","P 2yb               ",  &
                                               "-x,y+1/2,-z,1                                                                                           ")
     Shubnikov_info(  16)= Shub_Spgr_Info_Type("4.8    ","P2_11'      ","4.2.16     ","P2_11'        ","P2_1.1'                    ","P 2yb 1'            ",  &
                                               "-x,y+1/2,-z,1;x,y,z,-1                                                                                  ")
     Shubnikov_info(  17)= Shub_Spgr_Info_Type("4.9    ","P2_1'       ","4.3.17     ","P2_1'         ","P2_1'                      ","P 2yb'              ",  &
                                               "-x,y+1/2,-z,-1                                                                                          ")
     Shubnikov_info(  18)= Shub_Spgr_Info_Type("4.10   ","P_a2_1      ","4.4.18     ","P_2a2_1       ","P2_1.1'_a[P2_1]            ","P 2yb 1'a           ",  &
                                               "-x,y+1/2,-z,1;x+1/2,y,z,-1                                                                              ")
     Shubnikov_info(  19)= Shub_Spgr_Info_Type("5.13   ","C2          ","5.1.19     ","C2            ","C2.1                       ","C 2y                ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,-z,1                                                                               ")
     Shubnikov_info(  20)= Shub_Spgr_Info_Type("5.14   ","C21'        ","5.2.20     ","C21'          ","C2.1'                      ","C 2y 1'             ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,-z,1;x,y,z,-1                                                                      ")
     Shubnikov_info(  21)= Shub_Spgr_Info_Type("5.15   ","C2'         ","5.3.21     ","C2'           ","C2'                        ","C 2y'               ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,-z,-1                                                                              ")
     Shubnikov_info(  22)= Shub_Spgr_Info_Type("5.16   ","C_c2        ","5.4.22     ","C_2c2         ","C2.1'_c[C2]                ","C 2y 1'c            ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,-z,1;x,y,z+1/2,-1                                                                  ")
     Shubnikov_info(  23)= Shub_Spgr_Info_Type("3.6    ","P_C2        ","5.5.23     ","C_P2          ","P2.1'_C[C2]                ","P 2y 1'C            ",  &
                                               "-x,y,-z,1;x+1/2,y+1/2,z,-1                                                                              ")
     Shubnikov_info(  24)= Shub_Spgr_Info_Type("4.12   ","P_C2_1      ","5.6.24     ","C_P2'         ","P2_1.1'_C[C2]              ","P 2yb 1'C           ",  &
                                               "-x,y+1/2,-z,1;x+1/2,y+1/2,z,-1                                                                          ")
     Shubnikov_info(  25)= Shub_Spgr_Info_Type("6.18   ","Pm          ","6.1.25     ","Pm            ","Pm.1                       ","P -2y               ",  &
                                               "x,-y,z,1                                                                                                ")
     Shubnikov_info(  26)= Shub_Spgr_Info_Type("6.19   ","Pm1'        ","6.2.26     ","Pm1'          ","Pm.1'                      ","P -2y 1'            ",  &
                                               "x,-y,z,1;x,y,z,-1                                                                                       ")
     Shubnikov_info(  27)= Shub_Spgr_Info_Type("6.20   ","Pm'         ","6.3.27     ","Pm'           ","Pm'                        ","P -2y'              ",  &
                                               "x,-y,z,-1                                                                                               ")
     Shubnikov_info(  28)= Shub_Spgr_Info_Type("6.21   ","P_am        ","6.4.28     ","P_2am         ","Pm.1'_a[Pm]                ","P -2y 1'a           ",  &
                                               "x,-y,z,1;x+1/2,y,z,-1                                                                                   ")
     Shubnikov_info(  29)= Shub_Spgr_Info_Type("6.22   ","P_bm        ","6.5.29     ","P_2bm         ","Pm.1'_b[Pm]                ","P -2y 1'b           ",  &
                                               "x,-y,z,1;x,y+1/2,z,-1                                                                                   ")
     Shubnikov_info(  30)= Shub_Spgr_Info_Type("8.36   ","C_am        ","6.6.30     ","P_Cm          ","Cm.1'_a[Pm]                ","C -2y 1'a           ",  &
                                               "x+1/2,y+1/2,z,1;x,-y,z,1;x+1/2,y,z,-1                                                                   ")
     Shubnikov_info(  31)= Shub_Spgr_Info_Type("7.28   ","P_cc        ","6.7.31     ","P_2cm'        ","Pc.1'_c[Pm]                ","P -2yc 1'c          ",  &
                                               "x,-y,z+1/2,1;x,y,z+1/2,-1                                                                               ")
     Shubnikov_info(  32)= Shub_Spgr_Info_Type("7.24   ","Pc          ","7.1.32     ","Pc            ","Pc.1                       ","P -2yc              ",  &
                                               "x,-y,z+1/2,1                                                                                            ")
     Shubnikov_info(  33)= Shub_Spgr_Info_Type("7.25   ","Pc1'        ","7.2.33     ","Pc1'          ","Pc.1'                      ","P -2yc 1'           ",  &
                                               "x,-y,z+1/2,1;x,y,z,-1                                                                                   ")
     Shubnikov_info(  34)= Shub_Spgr_Info_Type("7.26   ","Pc'         ","7.3.34     ","Pc'           ","Pc'                        ","P -2yc'             ",  &
                                               "x,-y,z+1/2,-1                                                                                           ")
     Shubnikov_info(  35)= Shub_Spgr_Info_Type("7.27   ","P_ac        ","7.4.35     ","P_2ac         ","Pc.1'_a[Pc]                ","P -2yc 1'a          ",  &
                                               "x,-y,z+1/2,1;x+1/2,y,z,-1                                                                               ")
     Shubnikov_info(  36)= Shub_Spgr_Info_Type("7.29   ","P_bc        ","7.5.36     ","P_2bc         ","Pc.1'_b[Pc]                ","P -2yc 1'b          ",  &
                                               "x,-y,z+1/2,1;x,y+1/2,z,-1                                                                               ")
     Shubnikov_info(  37)= Shub_Spgr_Info_Type("9.41   ","C_ac        ","7.6.37     ","P_Cc          ","Cc.1'_a[Pc]                ","C -2yc 1'a          ",  &
                                               "x+1/2,y+1/2,z,1;x,-y,z+1/2,1;x+1/2,y,z,-1                                                               ")
     Shubnikov_info(  38)= Shub_Spgr_Info_Type("8.32   ","Cm          ","8.1.38     ","Cm            ","Cm.1                       ","C -2y               ",  &
                                               "x+1/2,y+1/2,z,1;x,-y,z,1                                                                                ")
     Shubnikov_info(  39)= Shub_Spgr_Info_Type("8.33   ","Cm1'        ","8.2.39     ","Cm1'          ","Cm.1'                      ","C -2y 1'            ",  &
                                               "x+1/2,y+1/2,z,1;x,-y,z,1;x,y,z,-1                                                                       ")
     Shubnikov_info(  40)= Shub_Spgr_Info_Type("8.34   ","Cm'         ","8.3.40     ","Cm'           ","Cm'                        ","C -2y'              ",  &
                                               "x+1/2,y+1/2,z,1;x,-y,z,-1                                                                               ")
     Shubnikov_info(  41)= Shub_Spgr_Info_Type("8.35   ","C_cm        ","8.4.41     ","C_2cm         ","Cm.1'_c[Cm]                ","C -2y 1'c           ",  &
                                               "x+1/2,y+1/2,z,1;x,-y,z,1;x,y,z+1/2,-1                                                                   ")
     Shubnikov_info(  42)= Shub_Spgr_Info_Type("6.23   ","P_Cm        ","8.5.42     ","C_Pm          ","Pm.1'_C[Cm]                ","P -2y 1'C           ",  &
                                               "x,-y,z,1;x+1/2,y+1/2,z,-1                                                                               ")
     Shubnikov_info(  43)= Shub_Spgr_Info_Type("9.40   ","C_cc        ","8.6.43     ","C_2cm'        ","Cc.1'_c[Cm]                ","C -2yc 1'c          ",  &
                                               "x+1/2,y+1/2,z,1;x,-y,z+1/2,1;x,y,z+1/2,-1                                                               ")
     Shubnikov_info(  44)= Shub_Spgr_Info_Type("7.31   ","P_Ac        ","8.7.44     ","C_Pm'         ","Pc.1'_A[Am]                ","P -2yc 1'A          ",  &
                                               "x,-y,z+1/2,1;x,y+1/2,z+1/2,-1                                                                           ")
     Shubnikov_info(  45)= Shub_Spgr_Info_Type("9.37   ","Cc          ","9.1.45     ","Cc            ","Cc.1                       ","C -2yc              ",  &
                                               "x+1/2,y+1/2,z,1;x,-y,z+1/2,1                                                                            ")
     Shubnikov_info(  46)= Shub_Spgr_Info_Type("9.38   ","Cc1'        ","9.2.46     ","Cc1'          ","Cc.1'                      ","C -2yc 1'           ",  &
                                               "x+1/2,y+1/2,z,1;x,-y,z+1/2,1;x,y,z,-1                                                                   ")
     Shubnikov_info(  47)= Shub_Spgr_Info_Type("9.39   ","Cc'         ","9.3.47     ","Cc'           ","Cc'                        ","C -2yc'             ",  &
                                               "x+1/2,y+1/2,z,1;x,-y,z+1/2,-1                                                                           ")
     Shubnikov_info(  48)= Shub_Spgr_Info_Type("7.30   ","P_Cc        ","9.4.48     ","C_Pc          ","Pc.1'_C[Cc]                ","P -2yc 1'C          ",  &
                                               "x,-y,z+1/2,1;x+1/2,y+1/2,z,-1                                                                           ")
     Shubnikov_info(  49)= Shub_Spgr_Info_Type("10.42  ","P2/m        ","10.1.49    ","P2/m          ","P2/m.1                     ","-P 2y               ",  &
                                               "-x,y,-z,1;x,-y,z,1                                                                                      ")
     Shubnikov_info(  50)= Shub_Spgr_Info_Type("10.43  ","P2/m1'      ","10.2.50    ","P2/m1'        ","P2/m.1'                    ","-P 2y 1'            ",  &
                                               "-x,y,-z,1;x,-y,z,1;x,y,z,-1                                                                             ")
     Shubnikov_info(  51)= Shub_Spgr_Info_Type("10.44  ","P2'/m       ","10.3.51    ","P2'/m         ","P2'/m                      ","P 2y' -1'           ",  &
                                               "-x,y,-z,-1;x,-y,z,1                                                                                     ")
     Shubnikov_info(  52)= Shub_Spgr_Info_Type("10.45  ","P2/m'       ","10.4.52    ","P2/m'         ","P2/m'                      ","P 2y -1'            ",  &
                                               "-x,y,-z,1;x,-y,z,-1                                                                                     ")
     Shubnikov_info(  53)= Shub_Spgr_Info_Type("10.46  ","P2'/m'      ","10.5.53    ","P2'/m'        ","P2'/m'                     ","-P 2y'              ",  &
                                               "-x,y,-z,-1;x,-y,z,-1                                                                                    ")
     Shubnikov_info(  54)= Shub_Spgr_Info_Type("10.47  ","P_a2/m      ","10.6.54    ","P_2a2/m       ","P2/m.1'_a[P2/m]            ","-P 2y 1'a           ",  &
                                               "-x,y,-z,1;x,-y,z,1;x+1/2,y,z,-1                                                                         ")
     Shubnikov_info(  55)= Shub_Spgr_Info_Type("10.48  ","P_b2/m      ","10.7.55    ","P_2b2/m       ","P2/m.1'_b[P2/m]            ","-P 2y 1'b           ",  &
                                               "-x,y,-z,1;x,-y,z,1;x,y+1/2,z,-1                                                                         ")
     Shubnikov_info(  56)= Shub_Spgr_Info_Type("12.64  ","C_a2/m      ","10.8.56    ","P_C2/m        ","C2/m.1'_a[P2/m]            ","-C 2y 1'a           ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,-z,1;x,-y,z,1;x+1/2,y,z,-1                                                         ")
     Shubnikov_info(  57)= Shub_Spgr_Info_Type("11.56  ","P_b2_1/m    ","10.9.57    ","P_2b2'/m      ","P2_1/m.1'_b[P2/m]          ","-P 2yb 1'b          ",  &
                                               "-x,y+1/2,-z,1;x,-y+1/2,z,1;x,y+1/2,z,-1                                                                 ")
     Shubnikov_info(  58)= Shub_Spgr_Info_Type("13.72  ","P_c2/c      ","10.10.58   ","P_2c2/m'      ","P2/c.1'_c[P2/m]            ","-P 2yc 1'c          ",  &
                                               "-x,y,-z+1/2,1;x,-y,z+1/2,1;x,y,z+1/2,-1                                                                 ")
     Shubnikov_info(  59)= Shub_Spgr_Info_Type("11.50  ","P2_1/m      ","11.1.59    ","P2_1/m        ","P2_1/m.1                   ","-P 2yb              ",  &
                                               "-x,y+1/2,-z,1;x,-y+1/2,z,1                                                                              ")
     Shubnikov_info(  60)= Shub_Spgr_Info_Type("11.51  ","P2_1/m1'    ","11.2.60    ","P2_1/m1'      ","P2_1/m.1'                  ","-P 2yb 1'           ",  &
                                               "-x,y+1/2,-z,1;x,-y+1/2,z,1;x,y,z,-1                                                                     ")
     Shubnikov_info(  61)= Shub_Spgr_Info_Type("11.52  ","P2_1'/m     ","11.3.61    ","P2_1'/m       ","P2_1'/m                    ","P 2yb' -1'          ",  &
                                               "-x,y+1/2,-z,-1;x,-y+1/2,z,1                                                                             ")
     Shubnikov_info(  62)= Shub_Spgr_Info_Type("11.53  ","P2_1/m'     ","11.4.62    ","P2_1/m'       ","P2_1/m'                    ","P 2yb -1'           ",  &
                                               "-x,y+1/2,-z,1;x,-y+1/2,z,-1                                                                             ")
     Shubnikov_info(  63)= Shub_Spgr_Info_Type("11.54  ","P2_1'/m'    ","11.5.63    ","P2_1'/m'      ","P2_1'/m'                   ","-P 2yb'             ",  &
                                               "-x,y+1/2,-z,-1;x,-y+1/2,z,-1                                                                            ")
     Shubnikov_info(  64)= Shub_Spgr_Info_Type("11.55  ","P_a2_1/m    ","11.6.64    ","P_2a2_1/m     ","P2_1/m.1'_a[P2_1/m]        ","-P 2yb 1'a          ",  &
                                               "-x,y+1/2,-z,1;x,-y+1/2,z,1;x+1/2,y,z,-1                                                                 ")
     Shubnikov_info(  65)= Shub_Spgr_Info_Type("14.82  ","P_c2_1/c    ","11.7.65    ","P_2c2_1/m'    ","P2_1/c.1'_c[P2_1/m]        ","-P 2ybc 1'c         ",  &
                                               "-x,y+1/2,-z+1/2,1;x,-y+1/2,z+1/2,1;x,y,z+1/2,-1                                                         ")
     Shubnikov_info(  66)= Shub_Spgr_Info_Type("12.58  ","C2/m        ","12.1.66    ","C2/m          ","C2/m.1                     ","-C 2y               ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,-z,1;x,-y,z,1                                                                      ")
     Shubnikov_info(  67)= Shub_Spgr_Info_Type("12.59  ","C2/m1'      ","12.2.67    ","C2/m1'        ","C2/m.1'                    ","-C 2y 1'            ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,-z,1;x,-y,z,1;x,y,z,-1                                                             ")
     Shubnikov_info(  68)= Shub_Spgr_Info_Type("12.60  ","C2'/m       ","12.3.68    ","C2'/m         ","C2'/m                      ","C 2y' -1'           ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,-z,-1;x,-y,z,1                                                                     ")
     Shubnikov_info(  69)= Shub_Spgr_Info_Type("12.61  ","C2/m'       ","12.4.69    ","C2/m'         ","C2/m'                      ","C 2y -1'            ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,-z,1;x,-y,z,-1                                                                     ")
     Shubnikov_info(  70)= Shub_Spgr_Info_Type("12.62  ","C2'/m'      ","12.5.70    ","C2'/m'        ","C2'/m'                     ","-C 2y'              ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,-z,-1;x,-y,z,-1                                                                    ")
     Shubnikov_info(  71)= Shub_Spgr_Info_Type("12.63  ","C_c2/m      ","12.6.71    ","C_2c2/m       ","C2/m.1'_c[C2/m]            ","-C 2y 1'c           ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,-z,1;x,-y,z,1;x,y,z+1/2,-1                                                         ")
     Shubnikov_info(  72)= Shub_Spgr_Info_Type("10.49  ","P_C2/m      ","12.7.72    ","C_P2/m        ","P2/m.1'_C[C2/m]            ","-P 2y 1'C           ",  &
                                               "-x,y,-z,1;x,-y,z,1;x+1/2,y+1/2,z,-1                                                                     ")
     Shubnikov_info(  73)= Shub_Spgr_Info_Type("15.90  ","C_c2/c      ","12.8.73    ","C_2c2/m'      ","C2/c.1'_c[C2/m]            ","-C 2yc 1'c          ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,-z+1/2,1;x,-y,z+1/2,1;x,y,z+1/2,-1                                                 ")
     Shubnikov_info(  74)= Shub_Spgr_Info_Type("11.57  ","P_C2_1/m    ","12.9.74    ","C_P2'/m       ","P2_1/m.1'_C[C2/m]          ","-P 2yb 1'C          ",  &
                                               "-x,y+1/2,-z,1;x,-y+1/2,z,1;x+1/2,y+1/2,z,-1                                                             ")
     Shubnikov_info(  75)= Shub_Spgr_Info_Type("13.73  ","P_A2/c      ","12.10.75   ","C_P2/m'       ","P2/c.1'_A[A2/m]            ","-P 2yc 1'A          ",  &
                                               "-x,y,-z+1/2,1;x,-y,z+1/2,1;x,y+1/2,z+1/2,-1                                                             ")
     Shubnikov_info(  76)= Shub_Spgr_Info_Type("14.83  ","P_A2_1/c    ","12.11.76   ","C_P2'/m'      ","P2_1/c.1'_A[A2/m]          ","-P 2ybc 1'A         ",  &
                                               "-x,y+1/2,-z+1/2,1;x,-y+1/2,z+1/2,1;x,y+1/2,z+1/2,-1                                                     ")
     Shubnikov_info(  77)= Shub_Spgr_Info_Type("13.65  ","P2/c        ","13.1.77    ","P2/c          ","P2/c.1                     ","-P 2yc              ",  &
                                               "-x,y,-z+1/2,1;x,-y,z+1/2,1                                                                              ")
     Shubnikov_info(  78)= Shub_Spgr_Info_Type("13.66  ","P2/c1'      ","13.2.78    ","P2/c1'        ","P2/c.1'                    ","-P 2yc 1'           ",  &
                                               "-x,y,-z+1/2,1;x,-y,z+1/2,1;x,y,z,-1                                                                     ")
     Shubnikov_info(  79)= Shub_Spgr_Info_Type("13.67  ","P2'/c       ","13.3.79    ","P2'/c         ","P2'/c                      ","P 2yc' -1'          ",  &
                                               "-x,y,-z+1/2,-1;x,-y,z+1/2,1                                                                             ")
     Shubnikov_info(  80)= Shub_Spgr_Info_Type("13.68  ","P2/c'       ","13.4.80    ","P2/c'         ","P2/c'                      ","P 2yc -1'           ",  &
                                               "-x,y,-z+1/2,1;x,-y,z+1/2,-1                                                                             ")
     Shubnikov_info(  81)= Shub_Spgr_Info_Type("13.69  ","P2'/c'      ","13.5.81    ","P2'/c'        ","P2'/c'                     ","-P 2yc'             ",  &
                                               "-x,y,-z+1/2,-1;x,-y,z+1/2,-1                                                                            ")
     Shubnikov_info(  82)= Shub_Spgr_Info_Type("13.70  ","P_a2/c      ","13.6.82    ","P_2a2/c       ","P2/c.1'_a[P2/c]            ","-P 2yc 1'a          ",  &
                                               "-x,y,-z+1/2,1;x,-y,z+1/2,1;x+1/2,y,z,-1                                                                 ")
     Shubnikov_info(  83)= Shub_Spgr_Info_Type("13.71  ","P_b2/c      ","13.7.83    ","P_2b2/c       ","P2/c.1'_b[P2/c]            ","-P 2yc 1'b          ",  &
                                               "-x,y,-z+1/2,1;x,-y,z+1/2,1;x,y+1/2,z,-1                                                                 ")
     Shubnikov_info(  84)= Shub_Spgr_Info_Type("15.91  ","C_a2/c      ","13.8.84    ","P_C2/c        ","C2/c.1'_a[P2/c]            ","-C 2yc 1'a          ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,-z+1/2,1;x,-y,z+1/2,1;x+1/2,y,z,-1                                                 ")
     Shubnikov_info(  85)= Shub_Spgr_Info_Type("14.81  ","P_b2_1/c    ","13.9.85    ","P_2b2'/c      ","P2_1/c.1'_b[P2/c]          ","-P 2ybc 1'b         ",  &
                                               "-x,y+1/2,-z+1/2,1;x,-y+1/2,z+1/2,1;x,y+1/2,z,-1                                                         ")
     Shubnikov_info(  86)= Shub_Spgr_Info_Type("14.75  ","P2_1/c      ","14.1.86    ","P2_1/c        ","P2_1/c.1                   ","-P 2ybc             ",  &
                                               "-x,y+1/2,-z+1/2,1;x,-y+1/2,z+1/2,1                                                                      ")
     Shubnikov_info(  87)= Shub_Spgr_Info_Type("14.76  ","P2_1/c1'    ","14.2.87    ","P2_1/c1'      ","P2_1/c.1'                  ","-P 2ybc 1'          ",  &
                                               "-x,y+1/2,-z+1/2,1;x,-y+1/2,z+1/2,1;x,y,z,-1                                                             ")
     Shubnikov_info(  88)= Shub_Spgr_Info_Type("14.77  ","P2_1'/c     ","14.3.88    ","P2_1'/c       ","P2_1'/c                    ","P 2ybc' -1'         ",  &
                                               "-x,y+1/2,-z+1/2,-1;x,-y+1/2,z+1/2,1                                                                     ")
     Shubnikov_info(  89)= Shub_Spgr_Info_Type("14.78  ","P2_1/c'     ","14.4.89    ","P2_1/c'       ","P2_1/c'                    ","P 2ybc -1'          ",  &
                                               "-x,y+1/2,-z+1/2,1;x,-y+1/2,z+1/2,-1                                                                     ")
     Shubnikov_info(  90)= Shub_Spgr_Info_Type("14.79  ","P2_1'/c'    ","14.5.90    ","P2_1'/c'      ","P2_1'/c'                   ","-P 2ybc'            ",  &
                                               "-x,y+1/2,-z+1/2,-1;x,-y+1/2,z+1/2,-1                                                                    ")
     Shubnikov_info(  91)= Shub_Spgr_Info_Type("14.80  ","P_a2_1/c    ","14.6.91    ","P_2a2_1/c     ","P2_1/c.1'_a[P2_1/c]        ","-P 2ybc 1'a         ",  &
                                               "-x,y+1/2,-z+1/2,1;x,-y+1/2,z+1/2,1;x+1/2,y,z,-1                                                         ")
     Shubnikov_info(  92)= Shub_Spgr_Info_Type("15.85  ","C2/c        ","15.1.92    ","C2/c          ","C2/c.1                     ","-C 2yc              ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,-z+1/2,1;x,-y,z+1/2,1                                                              ")
     Shubnikov_info(  93)= Shub_Spgr_Info_Type("15.86  ","C2/c1'      ","15.2.93    ","C2/c1'        ","C2/c.1'                    ","-C 2yc 1'           ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,-z+1/2,1;x,-y,z+1/2,1;x,y,z,-1                                                     ")
     Shubnikov_info(  94)= Shub_Spgr_Info_Type("15.87  ","C2'/c       ","15.3.94    ","C2'/c         ","C2'/c                      ","C 2yc' -1'          ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,-z+1/2,-1;x,-y,z+1/2,1                                                             ")
     Shubnikov_info(  95)= Shub_Spgr_Info_Type("15.88  ","C2/c'       ","15.4.95    ","C2/c'         ","C2/c'                      ","C 2yc -1'           ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,-z+1/2,1;x,-y,z+1/2,-1                                                             ")
     Shubnikov_info(  96)= Shub_Spgr_Info_Type("15.89  ","C2'/c'      ","15.5.96    ","C2'/c'        ","C2'/c'                     ","-C 2yc'             ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,-z+1/2,-1;x,-y,z+1/2,-1                                                            ")
     Shubnikov_info(  97)= Shub_Spgr_Info_Type("13.74  ","P_C2/c      ","15.6.97    ","C_P2/c        ","P2/c.1'_C[C2/c]            ","-P 2yc 1'C          ",  &
                                               "-x,y,-z+1/2,1;x,-y,z+1/2,1;x+1/2,y+1/2,z,-1                                                             ")
     Shubnikov_info(  98)= Shub_Spgr_Info_Type("14.84  ","P_C2_1/c    ","15.7.98    ","C_P2'/c       ","P2_1/c.1'_C[C2/c]          ","-P 2ybc 1'C         ",  &
                                               "-x,y+1/2,-z+1/2,1;x,-y+1/2,z+1/2,1;x+1/2,y+1/2,z,-1                                                     ")
     Shubnikov_info(  99)= Shub_Spgr_Info_Type("16.1   ","P222        ","16.1.99    ","P222          ","P222.1                     ","P 2 2               ",  &
                                               "x,-y,-z,1;-x,y,-z,1;-x,-y,z,1                                                                           ")
     Shubnikov_info( 100)= Shub_Spgr_Info_Type("16.2   ","P2221'      ","16.2.100   ","P2221'        ","P222.1'                    ","P 2 2 1'            ",  &
                                               "x,-y,-z,1;-x,y,-z,1;-x,-y,z,1;x,y,z,-1                                                                  ")
     Shubnikov_info( 101)= Shub_Spgr_Info_Type("16.3   ","P2'2'2      ","16.3.101   ","P2'2'2        ","P2'2'2                     ","P 2' 2'             ",  &
                                               "x,-y,-z,-1;-x,y,-z,-1;-x,-y,z,1                                                                         ")
     Shubnikov_info( 102)= Shub_Spgr_Info_Type("16.4   ","P_a222      ","16.4.102   ","P_2a222       ","P222.1'_a[P222]            ","P 2 2 1'a           ",  &
                                               "x,-y,-z,1;-x,y,-z,1;-x,-y,z,1;x+1/2,y,z,-1                                                              ")
     Shubnikov_info( 103)= Shub_Spgr_Info_Type("21.43  ","C_a222      ","16.5.103   ","P_C222        ","C222.1'_a[P222]            ","C 2 2 1'a           ",  &
                                               "x+1/2,y+1/2,z,1;x,-y,-z,1;-x,y,-z,1;-x,-y,z,1;x+1/2,y,z,-1                                              ")
     Shubnikov_info( 104)= Shub_Spgr_Info_Type("22.48  ","F_S222      ","16.6.104   ","P_I222        ","F222.1'_I[P222]            ","F 2 2 1'n           ",  &
                                               "x+1/2,y+1/2,z,1;x+1/2,y,z+1/2,1;x,-y,-z,1;-x,y,-z,1;-x,-y,z,1;x+1/2,y+1/2,z+1/2,-1                      ")
     Shubnikov_info( 105)= Shub_Spgr_Info_Type("17.12  ","P_c222_1    ","16.7.105   ","P_2c22'2'     ","P222_1.1'_c[P222]          ","P 2c 2 1'c          ",  &
                                               "x,-y,-z,1;-x,y,-z+1/2,1;-x,-y,z+1/2,1;x,y,z+1/2,-1                                                      ")
     Shubnikov_info( 106)= Shub_Spgr_Info_Type("17.7   ","P222_1      ","17.1.106   ","P222_1        ","P222_1.1                   ","P 2c 2              ",  &
                                               "x,-y,-z,1;-x,y,-z+1/2,1;-x,-y,z+1/2,1                                                                   ")
     Shubnikov_info( 107)= Shub_Spgr_Info_Type("17.8   ","P222_11'    ","17.2.107   ","P222_11'      ","P222_1.1'                  ","P 2c 2 1'           ",  &
                                               "x,-y,-z,1;-x,y,-z+1/2,1;-x,-y,z+1/2,1;x,y,z,-1                                                          ")
     Shubnikov_info( 108)= Shub_Spgr_Info_Type("17.9   ","P2'2'2_1    ","17.3.108   ","P2'2'2_1      ","P2'2'2_1                   ","P 2c 2'             ",  &
                                               "x,-y,-z,-1;-x,y,-z+1/2,-1;-x,-y,z+1/2,1                                                                 ")
     Shubnikov_info( 109)= Shub_Spgr_Info_Type("17.10  ","P22'2_1'    ","17.4.109   ","P22'2_1'      ","P22'2_1'                   ","P 2c' 2             ",  &
                                               "x,-y,-z,1;-x,y,-z+1/2,-1;-x,-y,z+1/2,-1                                                                 ")
     Shubnikov_info( 110)= Shub_Spgr_Info_Type("17.11  ","P_a222_1    ","17.5.110   ","P_2a222_1     ","P222_1.1'_a[P222_1]        ","P 2c 2 1'a          ",  &
                                               "x,-y,-z,1;-x,y,-z+1/2,1;-x,-y,z+1/2,1;x+1/2,y,z,-1                                                      ")
     Shubnikov_info( 111)= Shub_Spgr_Info_Type("20.36  ","C_a222_1    ","17.6.111   ","P_C222_1      ","C222_1.1'_a[P222_1]        ","C 2c 2 1'a          ",  &
                                               "x+1/2,y+1/2,z,1;x,-y,-z,1;-x,y,-z+1/2,1;-x,-y,z+1/2,1;x+1/2,y,z,-1                                      ")
     Shubnikov_info( 112)= Shub_Spgr_Info_Type("18.20  ","P_b2_12_12  ","17.7.112   ","P_2a2'2'2_1   ","P2_12_12.1'_b[P2_122]      ","P 2 2ab 1'b         ",  &
                                               "x+1/2,-y+1/2,-z,1;-x+1/2,y+1/2,-z,1;-x,-y,z,1;x,y+1/2,z,-1                                              ")
     Shubnikov_info( 113)= Shub_Spgr_Info_Type("18.16  ","P2_12_12    ","18.1.113   ","P2_12_12      ","P2_12_12.1                 ","P 2 2ab             ",  &
                                               "x+1/2,-y+1/2,-z,1;-x+1/2,y+1/2,-z,1;-x,-y,z,1                                                           ")
     Shubnikov_info( 114)= Shub_Spgr_Info_Type("18.17  ","P2_12_121'  ","18.2.114   ","P2_12_121'    ","P2_12_12.1'                ","P 2 2ab 1'          ",  &
                                               "x+1/2,-y+1/2,-z,1;-x+1/2,y+1/2,-z,1;-x,-y,z,1;x,y,z,-1                                                  ")
     Shubnikov_info( 115)= Shub_Spgr_Info_Type("18.18  ","P2_1'2_1'2  ","18.3.115   ","P2_1'2_1'2    ","P2_1'2_1'2                 ","P 2 2ab'            ",  &
                                               "x+1/2,-y+1/2,-z,-1;-x+1/2,y+1/2,-z,-1;-x,-y,z,1                                                         ")
     Shubnikov_info( 116)= Shub_Spgr_Info_Type("18.19  ","P2_12_1'2'  ","18.4.116   ","P2_12_1'2'    ","P2_12_1'2'                 ","P 2' 2ab            ",  &
                                               "x+1/2,-y+1/2,-z,1;-x+1/2,y+1/2,-z,-1;-x,-y,z,-1                                                         ")
     Shubnikov_info( 117)= Shub_Spgr_Info_Type("18.21  ","P_c2_12_12  ","18.5.117   ","P_2c2_12_12   ","P2_12_12.1'_c[P2_12_12]    ","P 2 2ab 1'c         ",  &
                                               "x+1/2,-y+1/2,-z,1;-x+1/2,y+1/2,-z,1;-x,-y,z,1;x,y,z+1/2,-1                                              ")
     Shubnikov_info( 118)= Shub_Spgr_Info_Type("19.28  ","P_c2_12_12_1","18.6.118   ","P_2c2_12_1'2' ","P2_12_12_1.1'_c[P2_12_12]  ","P 2ac 2ab 1'c       ",  &
                                               "x+1/2,-y+1/2,-z,1;-x,y+1/2,-z+1/2,1;-x+1/2,-y,z+1/2,1;x,y,z+1/2,-1                                      ")
     Shubnikov_info( 119)= Shub_Spgr_Info_Type("19.25  ","P2_12_12_1  ","19.1.119   ","P2_12_12_1    ","P2_12_12_1.1               ","P 2ac 2ab           ",  &
                                               "x+1/2,-y+1/2,-z,1;-x,y+1/2,-z+1/2,1;-x+1/2,-y,z+1/2,1                                                   ")
     Shubnikov_info( 120)= Shub_Spgr_Info_Type("19.26  ","P2_12_12_11'","19.2.120   ","P2_12_12_11'  ","P2_12_12_1.1'              ","P 2ac 2ab 1'        ",  &
                                               "x+1/2,-y+1/2,-z,1;-x,y+1/2,-z+1/2,1;-x+1/2,-y,z+1/2,1;x,y,z,-1                                          ")
     Shubnikov_info( 121)= Shub_Spgr_Info_Type("19.27  ","P2_1'2_1'2_1","19.3.121   ","P2_1'2_1'2_1  ","P2_1'2_1'2_1               ","P 2ac 2ab'          ",  &
                                               "x+1/2,-y+1/2,-z,-1;-x,y+1/2,-z+1/2,-1;-x+1/2,-y,z+1/2,1                                                 ")
     Shubnikov_info( 122)= Shub_Spgr_Info_Type("20.31  ","C222_1      ","20.1.122   ","C222_1        ","C222_1.1                   ","C 2c 2              ",  &
                                               "x+1/2,y+1/2,z,1;x,-y,-z,1;-x,y,-z+1/2,1;-x,-y,z+1/2,1                                                   ")
     Shubnikov_info( 123)= Shub_Spgr_Info_Type("20.32  ","C222_11'    ","20.2.123   ","C222_11'      ","C222_1.1'                  ","C 2c 2 1'           ",  &
                                               "x+1/2,y+1/2,z,1;x,-y,-z,1;-x,y,-z+1/2,1;-x,-y,z+1/2,1;x,y,z,-1                                          ")
     Shubnikov_info( 124)= Shub_Spgr_Info_Type("20.33  ","C2'2'2_1    ","20.3.124   ","C2'2'2_1      ","C2'2'2_1                   ","C 2c 2'             ",  &
                                               "x+1/2,y+1/2,z,1;x,-y,-z,-1;-x,y,-z+1/2,-1;-x,-y,z+1/2,1                                                 ")
     Shubnikov_info( 125)= Shub_Spgr_Info_Type("20.34  ","C22'2_1'    ","20.4.125   ","C22'2_1'      ","C22'2_1'                   ","C 2c' 2             ",  &
                                               "x+1/2,y+1/2,z,1;x,-y,-z,1;-x,y,-z+1/2,-1;-x,-y,z+1/2,-1                                                 ")
     Shubnikov_info( 126)= Shub_Spgr_Info_Type("17.14  ","P_C222_1    ","20.5.126   ","C_P222_1      ","P222_1.1'_C[C222_1]        ","P 2c 2 1'C          ",  &
                                               "x,-y,-z,1;-x,y,-z+1/2,1;-x,-y,z+1/2,1;x+1/2,y+1/2,z,-1                                                  ")
     Shubnikov_info( 127)= Shub_Spgr_Info_Type("19.29  ","P_C2_12_12_1","20.6.127   ","C_P2'2'2_1    ","P2_12_12_1.1'_C[C222_1]    ","P 2ac 2ab 1'C       ",  &
                                               "x+1/2,-y+1/2,-z,1;-x,y+1/2,-z+1/2,1;-x+1/2,-y,z+1/2,1;x+1/2,y+1/2,z,-1                                  ")
     Shubnikov_info( 128)= Shub_Spgr_Info_Type("18.22  ","P_B2_12_12  ","20.7.128   ","C_P22'2_1'    ","P2_12_12.1'_B[B22_12]      ","P 2 2ab 1'B         ",  &
                                               "x+1/2,-y+1/2,-z,1;-x+1/2,y+1/2,-z,1;-x,-y,z,1;x+1/2,y,z+1/2,-1                                          ")
     Shubnikov_info( 129)= Shub_Spgr_Info_Type("21.38  ","C222        ","21.1.129   ","C222          ","C222.1                     ","C 2 2               ",  &
                                               "x+1/2,y+1/2,z,1;x,-y,-z,1;-x,y,-z,1;-x,-y,z,1                                                           ")
     Shubnikov_info( 130)= Shub_Spgr_Info_Type("21.39  ","C2221'      ","21.2.130   ","C2221'        ","C222.1'                    ","C 2 2 1'            ",  &
                                               "x+1/2,y+1/2,z,1;x,-y,-z,1;-x,y,-z,1;-x,-y,z,1;x,y,z,-1                                                  ")
     Shubnikov_info( 131)= Shub_Spgr_Info_Type("21.40  ","C2'2'2      ","21.3.131   ","C2'2'2        ","C2'2'2                     ","C 2 2'              ",  &
                                               "x+1/2,y+1/2,z,1;x,-y,-z,-1;-x,y,-z,-1;-x,-y,z,1                                                         ")
     Shubnikov_info( 132)= Shub_Spgr_Info_Type("21.41  ","C22'2'      ","21.4.132   ","C22'2'        ","C22'2'                     ","C 2' 2              ",  &
                                               "x+1/2,y+1/2,z,1;x,-y,-z,1;-x,y,-z,-1;-x,-y,z,-1                                                         ")
     Shubnikov_info( 133)= Shub_Spgr_Info_Type("21.42  ","C_c222      ","21.5.133   ","C_2c222       ","C222.1'_c[C222]            ","C 2 2 1'c           ",  &
                                               "x+1/2,y+1/2,z,1;x,-y,-z,1;-x,y,-z,1;-x,-y,z,1;x,y,z+1/2,-1                                              ")
     Shubnikov_info( 134)= Shub_Spgr_Info_Type("16.5   ","P_C222      ","21.6.134   ","C_P222        ","P222.1'_C[C222]            ","P 2 2 1'C           ",  &
                                               "x,-y,-z,1;-x,y,-z,1;-x,-y,z,1;x+1/2,y+1/2,z,-1                                                          ")
     Shubnikov_info( 135)= Shub_Spgr_Info_Type("23.52  ","I_c222      ","21.7.135   ","C_I222        ","I222.1'_c[C222]            ","I 2 2 1'c           ",  &
                                               "x+1/2,y+1/2,z+1/2,1;x,-y,-z,1;-x,y,-z,1;-x,-y,z,1;x,y,z+1/2,-1                                          ")
     Shubnikov_info( 136)= Shub_Spgr_Info_Type("20.35  ","C_c222_1    ","21.8.136   ","C_2c22'2'     ","C222_1.1'_c[C222]          ","C 2c 2 1'c          ",  &
                                               "x+1/2,y+1/2,z,1;x,-y,-z,1;-x,y,-z+1/2,1;-x,-y,z+1/2,1;x,y,z+1/2,-1                                      ")
     Shubnikov_info( 137)= Shub_Spgr_Info_Type("18.23  ","P_C2_12_12  ","21.9.137   ","C_P2'2'2      ","P2_12_12.1'_C[C222]        ","P 2 2ab 1'C         ",  &
                                               "x+1/2,-y+1/2,-z,1;-x+1/2,y+1/2,-z,1;-x,-y,z,1;x+1/2,y+1/2,z,-1                                          ")
     Shubnikov_info( 138)= Shub_Spgr_Info_Type("17.13  ","P_B222_1    ","21.10.138  ","C_P22'2'      ","P222_1.1'_B[B222]          ","P 2c 2 1'B          ",  &
                                               "x,-y,-z,1;-x,y,-z+1/2,1;-x,-y,z+1/2,1;x+1/2,y,z+1/2,-1                                                  ")
     Shubnikov_info( 139)= Shub_Spgr_Info_Type("24.56  ","I_c2_12_12_1","21.11.139  ","C_I2'22'      ","I2_12_12_1.1'_c[C222]      ","I 2ac 2ab 1'c       ",  &
                                               "x+1/2,y+1/2,z+1/2,1;x,-y,-z+1/2,1;-x+1/2,y,-z,1;-x,-y+1/2,z,1;x,y,z+1/2,-1                              ")
     Shubnikov_info( 140)= Shub_Spgr_Info_Type("22.45  ","F222        ","22.1.140   ","F222          ","F222.1                     ","F 2 2               ",  &
                                               "x+1/2,y+1/2,z,1;x+1/2,y,z+1/2,1;x,-y,-z,1;-x,y,-z,1;-x,-y,z,1                                           ")
     Shubnikov_info( 141)= Shub_Spgr_Info_Type("22.46  ","F2221'      ","22.2.141   ","F2221'        ","F222.1'                    ","F 2 2 1'            ",  &
                                               "x+1/2,y+1/2,z,1;x+1/2,y,z+1/2,1;x,-y,-z,1;-x,y,-z,1;-x,-y,z,1;x,y,z,-1                                  ")
     Shubnikov_info( 142)= Shub_Spgr_Info_Type("22.47  ","F2'2'2      ","22.3.142   ","F2'2'2        ","F2'2'2                     ","F 2 2'              ",  &
                                               "x+1/2,y+1/2,z,1;x+1/2,y,z+1/2,1;x,-y,-z,-1;-x,y,-z,-1;-x,-y,z,1                                         ")
     Shubnikov_info( 143)= Shub_Spgr_Info_Type("21.44  ","C_A222      ","22.4.143   ","F_C222        ","C222.1'_A[F222]            ","C 2 2 1'A           ",  &
                                               "x+1/2,y+1/2,z,1;x,-y,-z,1;-x,y,-z,1;-x,-y,z,1;x,y+1/2,z+1/2,-1                                          ")
     Shubnikov_info( 144)= Shub_Spgr_Info_Type("20.37  ","C_A222_1    ","22.5.144   ","F_C22'2'      ","C222_1.1'_A[F222]          ","C 2c 2 1'A          ",  &
                                               "x+1/2,y+1/2,z,1;x,-y,-z,1;-x,y,-z+1/2,1;-x,-y,z+1/2,1;x,y+1/2,z+1/2,-1                                  ")
     Shubnikov_info( 145)= Shub_Spgr_Info_Type("23.49  ","I222        ","23.1.145   ","I222          ","I222.1                     ","I 2 2               ",  &
                                               "x+1/2,y+1/2,z+1/2,1;x,-y,-z,1;-x,y,-z,1;-x,-y,z,1                                                       ")
     Shubnikov_info( 146)= Shub_Spgr_Info_Type("23.50  ","I2221'      ","23.2.146   ","I2221'        ","I222.1'                    ","I 2 2 1'            ",  &
                                               "x+1/2,y+1/2,z+1/2,1;x,-y,-z,1;-x,y,-z,1;-x,-y,z,1;x,y,z,-1                                              ")
     Shubnikov_info( 147)= Shub_Spgr_Info_Type("23.51  ","I2'2'2      ","23.3.147   ","I2'2'2        ","I2'2'2                     ","I 2 2'              ",  &
                                               "x+1/2,y+1/2,z+1/2,1;x,-y,-z,-1;-x,y,-z,-1;-x,-y,z,1                                                     ")
     Shubnikov_info( 148)= Shub_Spgr_Info_Type("16.6   ","P_I222      ","23.4.148   ","I_P222        ","P222.1'_I[I222]            ","P 2 2 1'I           ",  &
                                               "x,-y,-z,1;-x,y,-z,1;-x,-y,z,1;x+1/2,y+1/2,z+1/2,-1                                                      ")
     Shubnikov_info( 149)= Shub_Spgr_Info_Type("18.24  ","P_I2_12_12  ","23.5.149   ","I_P2'2'2      ","P2_12_12.1'_I[I222]        ","P 2 2ab 1'I         ",  &
                                               "x+1/2,-y+1/2,-z,1;-x+1/2,y+1/2,-z,1;-x,-y,z,1;x+1/2,y+1/2,z+1/2,-1                                      ")
     Shubnikov_info( 150)= Shub_Spgr_Info_Type("24.53  ","I2_12_12_1  ","24.1.150   ","I2_12_12_1    ","I2_12_12_1.1               ","I 2b 2c             ",  &
                                               "x+1/2,y+1/2,z+1/2,1;x,-y,-z+1/2,1;-x+1/2,y,-z,1;-x,-y+1/2,z,1                                           ")
     Shubnikov_info( 151)= Shub_Spgr_Info_Type("24.54  ","I2_12_12_11'","24.2.151   ","I2_12_12_11'  ","I2_12_12_1.1'              ","I 2b 2c 1'          ",  &
                                               "x+1/2,y+1/2,z+1/2,1;x,-y,-z+1/2,1;-x+1/2,y,-z,1;-x,-y+1/2,z,1;x,y,z,-1                                  ")
     Shubnikov_info( 152)= Shub_Spgr_Info_Type("24.55  ","I2_1'2_1'2_1","24.3.152   ","I2_1'2_1'2_1  ","I2_1'2_1'2_1               ","I 2b 2c'            ",  &
                                               "x+1/2,y+1/2,z+1/2,1;x,-y,-z+1/2,-1;-x+1/2,y,-z,-1;-x,-y+1/2,z,1                                         ")
     Shubnikov_info( 153)= Shub_Spgr_Info_Type("19.30  ","P_I2_12_12_1","24.4.153   ","I_P2_12_12_1  ","P2_12_12_1.1'_I[I2_12_12_1]","P 2ac 2ab 1'I       ",  &
                                               "x+1/2,-y+1/2,-z,1;-x,y+1/2,-z+1/2,1;-x+1/2,-y,z+1/2,1;x+1/2,y+1/2,z+1/2,-1                              ")
     Shubnikov_info( 154)= Shub_Spgr_Info_Type("17.15  ","P_I222_1    ","24.5.154   ","I_P2_1'2_1'2_1","P222_1.1'_I[I2_12_12_1]    ","P 2c 2 1'I          ",  &
                                               "x,-y,-z,1;-x,y,-z+1/2,1;-x,-y,z+1/2,1;x+1/2,y+1/2,z+1/2,-1                                              ")
     Shubnikov_info( 155)= Shub_Spgr_Info_Type("25.57  ","Pmm2        ","25.1.155   ","Pmm2          ","Pmm2.1                     ","P 2 -2              ",  &
                                               "-x,y,z,1;x,-y,z,1;-x,-y,z,1                                                                             ")
     Shubnikov_info( 156)= Shub_Spgr_Info_Type("25.58  ","Pmm21'      ","25.2.156   ","Pmm21'        ","Pmm2.1'                    ","P 2 -2 1'           ",  &
                                               "-x,y,z,1;x,-y,z,1;-x,-y,z,1;x,y,z,-1                                                                    ")
     Shubnikov_info( 157)= Shub_Spgr_Info_Type("25.59  ","Pm'm2'      ","25.3.157   ","Pm'm2'        ","Pm'm2'                     ","P 2' -2'            ",  &
                                               "-x,y,z,-1;x,-y,z,1;-x,-y,z,-1                                                                           ")
     Shubnikov_info( 158)= Shub_Spgr_Info_Type("25.60  ","Pm'm'2      ","25.4.158   ","Pm'm'2        ","Pm'm'2                     ","P 2 -2'             ",  &
                                               "-x,y,z,-1;x,-y,z,-1;-x,-y,z,1                                                                           ")
     Shubnikov_info( 159)= Shub_Spgr_Info_Type("25.61  ","P_cmm2      ","25.5.159   ","P_2cmm2       ","Pmm2.1'_c[Pmm2]            ","P 2 -2 1'c          ",  &
                                               "-x,y,z,1;x,-y,z,1;-x,-y,z,1;x,y,z+1/2,-1                                                                ")
     Shubnikov_info( 160)= Shub_Spgr_Info_Type("25.62  ","P_amm2      ","25.6.160   ","P_2amm2       ","Pmm2.1'_a[Pmm2]            ","P 2 -2 1'a          ",  &
                                               "-x,y,z,1;x,-y,z,1;-x,-y,z,1;x+1/2,y,z,-1                                                                ")
     Shubnikov_info( 161)= Shub_Spgr_Info_Type("35.170 ","C_amm2      ","25.7.161   ","P_Cmm2        ","Cmm2.1'_a[Pmm2]            ","C 2 -2 1'a          ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x,-y,z,1;-x,-y,z,1;x+1/2,y,z,-1                                                ")
     Shubnikov_info( 162)= Shub_Spgr_Info_Type("38.193 ","A_bmm2      ","25.8.162   ","P_Amm2        ","Amm2.1'_b[Pmm2]            ","A 2 -2 1'b          ",  &
                                               "x,y+1/2,z+1/2,1;-x,y,z,1;x,-y,z,1;-x,-y,z,1;x,y,z+1/2,-1                                                ")
     Shubnikov_info( 163)= Shub_Spgr_Info_Type("42.223 ","F_Smm2      ","25.9.163   ","P_Imm2        ","Fmm2.1'_I[Pmm2]            ","F 2 -2 1'n          ",  &
                                               "x+1/2,y+1/2,z,1;x+1/2,y,z+1/2,1;-x,y,z,1;x,-y,z,1;-x,-y,z,1;x+1/2,y+1/2,z+1/2,-1                        ")
     Shubnikov_info( 164)= Shub_Spgr_Info_Type("26.73  ","P_cmc2_1    ","25.10.164  ","P_2cmm'2'     ","Pmc2_1.1'_c[Pmm2]          ","P 2c -2 1'c         ",  &
                                               "-x,y,z,1;x,-y,z+1/2,1;-x,-y,z+1/2,1;x,y,z+1/2,-1                                                        ")
     Shubnikov_info( 165)= Shub_Spgr_Info_Type("27.82  ","P_ccc2      ","25.11.165  ","P_2cm'm'2     ","Pcc2.1'_c[Pmm2]            ","P 2 -2c 1'c         ",  &
                                               "-x,y,z+1/2,1;x,-y,z+1/2,1;-x,-y,z,1;x,y,z+1/2,-1                                                        ")
     Shubnikov_info( 166)= Shub_Spgr_Info_Type("28.92  ","P_ama2      ","25.12.166  ","P_2am'm'2     ","Pma2.1'_a[Pmm2]            ","P 2 -2a 1'a         ",  &
                                               "-x+1/2,y,z,1;x+1/2,-y,z,1;-x,-y,z,1;x+1/2,y,z,-1                                                        ")
     Shubnikov_info( 167)= Shub_Spgr_Info_Type("39.201 ","A_bbm2      ","25.13.167  ","P_Am'm'2      ","Abm2.1'_b[Pmm2]            ","A 2 -2c 1'b         ",  &
                                               "x,y+1/2,z+1/2,1;-x,y,z+1/2,1;x,-y,z+1/2,1;-x,-y,z,1;x,y,z+1/2,-1                                        ")
     Shubnikov_info( 168)= Shub_Spgr_Info_Type("26.66  ","Pmc2_1      ","26.1.168   ","Pmc2_1        ","Pmc2_1.1                   ","P 2c -2             ",  &
                                               "-x,y,z,1;x,-y,z+1/2,1;-x,-y,z+1/2,1                                                                     ")
     Shubnikov_info( 169)= Shub_Spgr_Info_Type("26.67  ","Pmc2_11'    ","26.2.169   ","Pmc2_11'      ","Pmc2_1.1'                  ","P 2c -2 1'          ",  &
                                               "-x,y,z,1;x,-y,z+1/2,1;-x,-y,z+1/2,1;x,y,z,-1                                                            ")
     Shubnikov_info( 170)= Shub_Spgr_Info_Type("26.68  ","Pm'c2_1'    ","26.3.170   ","Pm'c2_1'      ","Pm'c2_1'                   ","P 2c' -2'           ",  &
                                               "-x,y,z,-1;x,-y,z+1/2,1;-x,-y,z+1/2,-1                                                                   ")
     Shubnikov_info( 171)= Shub_Spgr_Info_Type("26.69  ","Pmc'2_1'    ","26.4.171   ","Pmc'2_1'      ","Pmc'2_1'                   ","P 2c' -2            ",  &
                                               "-x,y,z,1;x,-y,z+1/2,-1;-x,-y,z+1/2,-1                                                                   ")
     Shubnikov_info( 172)= Shub_Spgr_Info_Type("26.70  ","Pm'c'2_1    ","26.5.172   ","Pm'c'2_1      ","Pm'c'2_1                   ","P 2c -2'            ",  &
                                               "-x,y,z,-1;x,-y,z+1/2,-1;-x,-y,z+1/2,1                                                                   ")
     Shubnikov_info( 173)= Shub_Spgr_Info_Type("26.71  ","P_amc2_1    ","26.6.173   ","P_2amc2_1     ","Pmc2_1.1'_a[Pmc2_1]        ","P 2c -2 1'a         ",  &
                                               "-x,y,z,1;x,-y,z+1/2,1;-x,-y,z+1/2,1;x+1/2,y,z,-1                                                        ")
     Shubnikov_info( 174)= Shub_Spgr_Info_Type("26.72  ","P_bmc2_1    ","26.7.174   ","P_2bmc2_1     ","Pmc2_1.1'_b[Pmc2_1]        ","P 2c -2 1'b         ",  &
                                               "-x,y,z,1;x,-y,z+1/2,1;-x,-y,z+1/2,1;x,y+1/2,z,-1                                                        ")
     Shubnikov_info( 175)= Shub_Spgr_Info_Type("36.178 ","C_amc2_1    ","26.8.175   ","P_Cmc2_1      ","Cmc2_1.1'_a[Pmc2_1]        ","C 2c -2 1'a         ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x,-y,z+1/2,1;-x,-y,z+1/2,1;x+1/2,y,z,-1                                        ")
     Shubnikov_info( 176)= Shub_Spgr_Info_Type("31.128 ","P_amn2_1    ","26.9.176   ","P_2amc'2_1'   ","Pmn2_1.1'_a[Pmc2_1]        ","P 2ac -2 1'a        ",  &
                                               "-x,y,z,1;x+1/2,-y,z+1/2,1;-x+1/2,-y,z+1/2,1;x+1/2,y,z,-1                                                ")
     Shubnikov_info( 177)= Shub_Spgr_Info_Type("29.104 ","P_aca2_1    ","26.10.177  ","P_2bm'c'2_1   ","Pca2_1.1'_a[Pcm2_1]        ","P 2c -2ac 1'a       ",  &
                                               "-x+1/2,y,z+1/2,1;x+1/2,-y,z,1;-x,-y,z+1/2,1;x+1/2,y,z,-1                                                ")
     Shubnikov_info( 178)= Shub_Spgr_Info_Type("27.78  ","Pcc2        ","27.1.178   ","Pcc2          ","Pcc2.1                     ","P 2 -2c             ",  &
                                               "-x,y,z+1/2,1;x,-y,z+1/2,1;-x,-y,z,1                                                                     ")
     Shubnikov_info( 179)= Shub_Spgr_Info_Type("27.79  ","Pcc21'      ","27.2.179   ","Pcc21'        ","Pcc2.1'                    ","P 2 -2c 1'          ",  &
                                               "-x,y,z+1/2,1;x,-y,z+1/2,1;-x,-y,z,1;x,y,z,-1                                                            ")
     Shubnikov_info( 180)= Shub_Spgr_Info_Type("27.80  ","Pc'c2'      ","27.3.180   ","Pc'c2'        ","Pc'c2'                     ","P 2' -2c            ",  &
                                               "-x,y,z+1/2,-1;x,-y,z+1/2,1;-x,-y,z,-1                                                                   ")
     Shubnikov_info( 181)= Shub_Spgr_Info_Type("27.81  ","Pc'c'2      ","27.4.181   ","Pc'c'2        ","Pc'c'2                     ","P 2 -2c'            ",  &
                                               "-x,y,z+1/2,-1;x,-y,z+1/2,-1;-x,-y,z,1                                                                   ")
     Shubnikov_info( 182)= Shub_Spgr_Info_Type("27.83  ","P_acc2      ","27.5.182   ","P_2acc2       ","Pcc2.1'_a[Pcc2]            ","P 2 -2c 1'a         ",  &
                                               "-x,y,z+1/2,1;x,-y,z+1/2,1;-x,-y,z,1;x+1/2,y,z,-1                                                        ")
     Shubnikov_info( 183)= Shub_Spgr_Info_Type("37.185 ","C_acc2      ","27.6.183   ","P_Ccc2        ","Ccc2.1'_a[Pcc2]            ","C 2 -2c 1'a         ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z+1/2,1;x,-y,z+1/2,1;-x,-y,z,1;x+1/2,y,z,-1                                        ")
     Shubnikov_info( 184)= Shub_Spgr_Info_Type("30.117 ","P_bnc2      ","27.7.184   ","P_2bc'c2'     ","Pnc2.1'_b[Pcc2]            ","P 2 -2bc 1'b        ",  &
                                               "-x,y+1/2,z+1/2,1;x,-y+1/2,z+1/2,1;-x,-y,z,1;x,y+1/2,z,-1                                                ")
     Shubnikov_info( 185)= Shub_Spgr_Info_Type("28.87  ","Pma2        ","28.1.185   ","Pma2          ","Pma2.1                     ","P 2 -2a             ",  &
                                               "-x+1/2,y,z,1;x+1/2,-y,z,1;-x,-y,z,1                                                                     ")
     Shubnikov_info( 186)= Shub_Spgr_Info_Type("28.88  ","Pma21'      ","28.2.186   ","Pma21'        ","Pma2.1'                    ","P 2 -2a 1'          ",  &
                                               "-x+1/2,y,z,1;x+1/2,-y,z,1;-x,-y,z,1;x,y,z,-1                                                            ")
     Shubnikov_info( 187)= Shub_Spgr_Info_Type("28.89  ","Pm'a2'      ","28.3.187   ","Pm'a2'        ","Pm'a2'                     ","P 2' -2a'           ",  &
                                               "-x+1/2,y,z,-1;x+1/2,-y,z,1;-x,-y,z,-1                                                                   ")
     Shubnikov_info( 188)= Shub_Spgr_Info_Type("28.90  ","Pma'2'      ","28.4.188   ","Pma'2'        ","Pma'2'                     ","P 2' -2a            ",  &
                                               "-x+1/2,y,z,1;x+1/2,-y,z,-1;-x,-y,z,-1                                                                   ")
     Shubnikov_info( 189)= Shub_Spgr_Info_Type("28.91  ","Pm'a'2      ","28.5.189   ","Pm'a'2        ","Pm'a'2                     ","P 2 -2a'            ",  &
                                               "-x+1/2,y,z,-1;x+1/2,-y,z,-1;-x,-y,z,1                                                                   ")
     Shubnikov_info( 190)= Shub_Spgr_Info_Type("28.93  ","P_bma2      ","28.6.190   ","P_2bma2       ","Pma2.1'_b[Pma2]            ","P 2 -2a 1'b         ",  &
                                               "-x+1/2,y,z,1;x+1/2,-y,z,1;-x,-y,z,1;x,y+1/2,z,-1                                                        ")
     Shubnikov_info( 191)= Shub_Spgr_Info_Type("28.94  ","P_cma2      ","28.7.191   ","P_2cma2       ","Pma2.1'_c[Pma2]            ","P 2 -2a 1'c         ",  &
                                               "-x+1/2,y,z,1;x+1/2,-y,z,1;-x,-y,z,1;x,y,z+1/2,-1                                                        ")
     Shubnikov_info( 192)= Shub_Spgr_Info_Type("40.209 ","A_bma2      ","28.8.192   ","P_Ama2        ","Ama2.1'_b[Pma2]            ","A 2 -2a 1'b         ",  &
                                               "x,y+1/2,z+1/2,1;-x+1/2,y,z,1;x+1/2,-y,z,1;-x,-y,z,1;x,y,z+1/2,-1                                        ")
     Shubnikov_info( 193)= Shub_Spgr_Info_Type("32.140 ","P_bba2      ","28.9.193   ","P_2bm'a2'     ","Pba2.1'_b[Pma2]            ","P 2 -2ab 1'b        ",  &
                                               "-x+1/2,y+1/2,z,1;x+1/2,-y+1/2,z,1;-x,-y,z,1;x,y+1/2,z,-1                                                ")
     Shubnikov_info( 194)= Shub_Spgr_Info_Type("29.106 ","P_cca2_1    ","28.10.194  ","P_2cm'a2'     ","Pca2_1.1'_c[Pma2]          ","P 2c -2ac 1'c       ",  &
                                               "-x+1/2,y,z+1/2,1;x+1/2,-y,z,1;-x,-y,z+1/2,1;x,y,z+1/2,-1                                                ")
     Shubnikov_info( 195)= Shub_Spgr_Info_Type("31.130 ","P_cmn2_1    ","28.11.195  ","P_2cma'2'     ","Pmn2_1.1'_c[Pma2]          ","P 2ac -2 1'c        ",  &
                                               "-x,y,z,1;x+1/2,-y,z+1/2,1;-x+1/2,-y,z+1/2,1;x,y,z+1/2,-1                                                ")
     Shubnikov_info( 196)= Shub_Spgr_Info_Type("30.118 ","P_cnc2      ","28.12.196  ","P_2cm'a'2     ","Pnc2.1'_c[Pbm2]            ","P 2 -2bc 1'c        ",  &
                                               "-x,y+1/2,z+1/2,1;x,-y+1/2,z+1/2,1;-x,-y,z,1;x,y,z+1/2,-1                                                ")
     Shubnikov_info( 197)= Shub_Spgr_Info_Type("41.217 ","A_bba2      ","28.13.197  ","P_Am'a'2      ","Aba2.1'_b[Pma2]            ","A 2 -2ac 1'b        ",  &
                                               "x,y+1/2,z+1/2,1;-x+1/2,y,z+1/2,1;x+1/2,-y,z+1/2,1;-x,-y,z,1;x,y,z+1/2,-1                                ")
     Shubnikov_info( 198)= Shub_Spgr_Info_Type("29.99  ","Pca2_1      ","29.1.198   ","Pca2_1        ","Pca2_1.1                   ","P 2c -2ac           ",  &
                                               "-x+1/2,y,z+1/2,1;x+1/2,-y,z,1;-x,-y,z+1/2,1                                                             ")
     Shubnikov_info( 199)= Shub_Spgr_Info_Type("29.100 ","Pca2_11'    ","29.2.199   ","Pca2_11'      ","Pca2_1.1'                  ","P 2c -2ac 1'        ",  &
                                               "-x+1/2,y,z+1/2,1;x+1/2,-y,z,1;-x,-y,z+1/2,1;x,y,z,-1                                                    ")
     Shubnikov_info( 200)= Shub_Spgr_Info_Type("29.101 ","Pc'a2_1'    ","29.3.200   ","Pc'a2_1'      ","Pc'a2_1'                   ","P 2c' -2ac'         ",  &
                                               "-x+1/2,y,z+1/2,-1;x+1/2,-y,z,1;-x,-y,z+1/2,-1                                                           ")
     Shubnikov_info( 201)= Shub_Spgr_Info_Type("29.102 ","Pca'2_1'    ","29.4.201   ","Pca'2_1'      ","Pca'2_1'                   ","P 2c' -2ac          ",  &
                                               "-x+1/2,y,z+1/2,1;x+1/2,-y,z,-1;-x,-y,z+1/2,-1                                                           ")
     Shubnikov_info( 202)= Shub_Spgr_Info_Type("29.103 ","Pc'a'2_1    ","29.5.202   ","Pc'a'2_1      ","Pc'a'2_1                   ","P 2c -2ac'          ",  &
                                               "-x+1/2,y,z+1/2,-1;x+1/2,-y,z,-1;-x,-y,z+1/2,1                                                           ")
     Shubnikov_info( 203)= Shub_Spgr_Info_Type("29.105 ","P_bca2_1    ","29.6.203   ","P_2bca2_1     ","Pca2_1.1'_b[Pca2_1]        ","P 2c -2ac 1'b       ",  &
                                               "-x+1/2,y,z+1/2,1;x+1/2,-y,z,1;-x,-y,z+1/2,1;x,y+1/2,z,-1                                                ")
     Shubnikov_info( 204)= Shub_Spgr_Info_Type("33.150 ","P_bna2_1    ","29.7.204   ","P_2bc'a'2_1   ","Pna2_1.1'_b[Pca2_1]        ","P 2c -2n 1'b        ",  &
                                               "-x+1/2,y+1/2,z+1/2,1;x+1/2,-y+1/2,z,1;-x,-y,z+1/2,1;x,y+1/2,z,-1                                        ")
     Shubnikov_info( 205)= Shub_Spgr_Info_Type("30.111 ","Pnc2        ","30.1.205   ","Pnc2          ","Pnc2.1                     ","P 2 -2bc            ",  &
                                               "-x,y+1/2,z+1/2,1;x,-y+1/2,z+1/2,1;-x,-y,z,1                                                             ")
     Shubnikov_info( 206)= Shub_Spgr_Info_Type("30.112 ","Pnc21'      ","30.2.206   ","Pnc21'        ","Pnc2.1'                    ","P 2 -2bc 1'         ",  &
                                               "-x,y+1/2,z+1/2,1;x,-y+1/2,z+1/2,1;-x,-y,z,1;x,y,z,-1                                                    ")
     Shubnikov_info( 207)= Shub_Spgr_Info_Type("30.113 ","Pn'c2'      ","30.3.207   ","Pn'c2'        ","Pn'c2'                     ","P 2' -2bc'          ",  &
                                               "-x,y+1/2,z+1/2,-1;x,-y+1/2,z+1/2,1;-x,-y,z,-1                                                           ")
     Shubnikov_info( 208)= Shub_Spgr_Info_Type("30.114 ","Pnc'2'      ","30.4.208   ","Pnc'2'        ","Pnc'2'                     ","P 2' -2bc           ",  &
                                               "-x,y+1/2,z+1/2,1;x,-y+1/2,z+1/2,-1;-x,-y,z,-1                                                           ")
     Shubnikov_info( 209)= Shub_Spgr_Info_Type("30.115 ","Pn'c'2      ","30.5.209   ","Pn'c'2        ","Pn'c'2                     ","P 2 -2bc'           ",  &
                                               "-x,y+1/2,z+1/2,-1;x,-y+1/2,z+1/2,-1;-x,-y,z,1                                                           ")
     Shubnikov_info( 210)= Shub_Spgr_Info_Type("30.116 ","P_anc2      ","30.6.210   ","P_2anc2       ","Pnc2.1'_a[Pnc2]            ","P 2 -2bc 1'a        ",  &
                                               "-x,y+1/2,z+1/2,1;x,-y+1/2,z+1/2,1;-x,-y,z,1;x+1/2,y,z,-1                                                ")
     Shubnikov_info( 211)= Shub_Spgr_Info_Type("34.160 ","P_ann2      ","30.7.211   ","P_2anc'2'     ","Pnn2.1'_a[Pnc2]            ","P 2 -2n 1'a         ",  &
                                               "-x+1/2,y+1/2,z+1/2,1;x+1/2,-y+1/2,z+1/2,1;-x,-y,z,1;x+1/2,y,z,-1                                        ")
     Shubnikov_info( 212)= Shub_Spgr_Info_Type("31.123 ","Pmn2_1      ","31.1.212   ","Pmn2_1        ","Pmn2_1.1                   ","P 2ac -2            ",  &
                                               "-x,y,z,1;x+1/2,-y,z+1/2,1;-x+1/2,-y,z+1/2,1                                                             ")
     Shubnikov_info( 213)= Shub_Spgr_Info_Type("31.124 ","Pmn2_11'    ","31.2.213   ","Pmn2_11'      ","Pmn2_1.1'                  ","P 2ac -2 1'         ",  &
                                               "-x,y,z,1;x+1/2,-y,z+1/2,1;-x+1/2,-y,z+1/2,1;x,y,z,-1                                                    ")
     Shubnikov_info( 214)= Shub_Spgr_Info_Type("31.125 ","Pm'n2_1'    ","31.3.214   ","Pm'n2_1'      ","Pm'n2_1'                   ","P 2ac' -2'          ",  &
                                               "-x,y,z,-1;x+1/2,-y,z+1/2,1;-x+1/2,-y,z+1/2,-1                                                           ")
     Shubnikov_info( 215)= Shub_Spgr_Info_Type("31.126 ","Pmn'2_1'    ","31.4.215   ","Pmn'2_1'      ","Pmn'2_1'                   ","P 2ac' -2           ",  &
                                               "-x,y,z,1;x+1/2,-y,z+1/2,-1;-x+1/2,-y,z+1/2,-1                                                           ")
     Shubnikov_info( 216)= Shub_Spgr_Info_Type("31.127 ","Pm'n'2_1    ","31.5.216   ","Pm'n'2_1      ","Pm'n'2_1                   ","P 2ac -2'           ",  &
                                               "-x,y,z,-1;x+1/2,-y,z+1/2,-1;-x+1/2,-y,z+1/2,1                                                           ")
     Shubnikov_info( 217)= Shub_Spgr_Info_Type("31.129 ","P_bmn2_1    ","31.6.217   ","P_2bmn2_1     ","Pmn2_1.1'_b[Pmn2_1]        ","P 2ac -2 1'b        ",  &
                                               "-x,y,z,1;x+1/2,-y,z+1/2,1;-x+1/2,-y,z+1/2,1;x,y+1/2,z,-1                                                ")
     Shubnikov_info( 218)= Shub_Spgr_Info_Type("33.149 ","P_ana2_1    ","31.7.218   ","P_2bm'n2_1'   ","Pna2_1.1'_a[Pnm2_1]        ","P 2c -2n 1'a        ",  &
                                               "-x+1/2,y+1/2,z+1/2,1;x+1/2,-y+1/2,z,1;-x,-y,z+1/2,1;x+1/2,y,z,-1                                        ")
     Shubnikov_info( 219)= Shub_Spgr_Info_Type("32.135 ","Pba2        ","32.1.219   ","Pba2          ","Pba2.1                     ","P 2 -2ab            ",  &
                                               "-x+1/2,y+1/2,z,1;x+1/2,-y+1/2,z,1;-x,-y,z,1                                                             ")
     Shubnikov_info( 220)= Shub_Spgr_Info_Type("32.136 ","Pba21'      ","32.2.220   ","Pba21'        ","Pba2.1'                    ","P 2 -2ab 1'         ",  &
                                               "-x+1/2,y+1/2,z,1;x+1/2,-y+1/2,z,1;-x,-y,z,1;x,y,z,-1                                                    ")
     Shubnikov_info( 221)= Shub_Spgr_Info_Type("32.137 ","Pb'a2'      ","32.3.221   ","Pb'a2'        ","Pb'a2'                     ","P 2' -2ab           ",  &
                                               "-x+1/2,y+1/2,z,-1;x+1/2,-y+1/2,z,1;-x,-y,z,-1                                                           ")
     Shubnikov_info( 222)= Shub_Spgr_Info_Type("32.138 ","Pb'a'2      ","32.4.222   ","Pb'a'2        ","Pb'a'2                     ","P 2 -2ab'           ",  &
                                               "-x+1/2,y+1/2,z,-1;x+1/2,-y+1/2,z,-1;-x,-y,z,1                                                           ")
     Shubnikov_info( 223)= Shub_Spgr_Info_Type("32.139 ","P_cba2      ","32.5.223   ","P_2cba2       ","Pba2.1'_c[Pba2]            ","P 2 -2ab 1'c        ",  &
                                               "-x+1/2,y+1/2,z,1;x+1/2,-y+1/2,z,1;-x,-y,z,1;x,y,z+1/2,-1                                                ")
     Shubnikov_info( 224)= Shub_Spgr_Info_Type("33.151 ","P_cna2_1    ","32.6.224   ","P_2cb'a2'     ","Pna2_1.1'_c[Pba2]          ","P 2c -2n 1'c        ",  &
                                               "-x+1/2,y+1/2,z+1/2,1;x+1/2,-y+1/2,z,1;-x,-y,z+1/2,1;x,y,z+1/2,-1                                        ")
     Shubnikov_info( 225)= Shub_Spgr_Info_Type("34.161 ","P_cnn2      ","32.7.225   ","P_2cb'a'2     ","Pnn2.1'_c[Pba2]            ","P 2 -2n 1'c         ",  &
                                               "-x+1/2,y+1/2,z+1/2,1;x+1/2,-y+1/2,z+1/2,1;-x,-y,z,1;x,y,z+1/2,-1                                        ")
     Shubnikov_info( 226)= Shub_Spgr_Info_Type("33.144 ","Pna2_1      ","33.1.226   ","Pna2_1        ","Pna2_1.1                   ","P 2c -2n            ",  &
                                               "-x+1/2,y+1/2,z+1/2,1;x+1/2,-y+1/2,z,1;-x,-y,z+1/2,1                                                     ")
     Shubnikov_info( 227)= Shub_Spgr_Info_Type("33.145 ","Pna2_11'    ","33.2.227   ","Pna2_11'      ","Pna2_1.1'                  ","P 2c -2n 1'         ",  &
                                               "-x+1/2,y+1/2,z+1/2,1;x+1/2,-y+1/2,z,1;-x,-y,z+1/2,1;x,y,z,-1                                            ")
     Shubnikov_info( 228)= Shub_Spgr_Info_Type("33.146 ","Pn'a2_1'    ","33.3.228   ","Pn'a2_1'      ","Pn'a2_1'                   ","P 2c' -2n'          ",  &
                                               "-x+1/2,y+1/2,z+1/2,-1;x+1/2,-y+1/2,z,1;-x,-y,z+1/2,-1                                                   ")
     Shubnikov_info( 229)= Shub_Spgr_Info_Type("33.147 ","Pna'2_1'    ","33.4.229   ","Pna'2_1'      ","Pna'2_1'                   ","P 2c' -2n           ",  &
                                               "-x+1/2,y+1/2,z+1/2,1;x+1/2,-y+1/2,z,-1;-x,-y,z+1/2,-1                                                   ")
     Shubnikov_info( 230)= Shub_Spgr_Info_Type("33.148 ","Pn'a'2_1    ","33.5.230   ","Pn'a'2_1      ","Pn'a'2_1                   ","P 2c -2n'           ",  &
                                               "-x+1/2,y+1/2,z+1/2,-1;x+1/2,-y+1/2,z,-1;-x,-y,z+1/2,1                                                   ")
     Shubnikov_info( 231)= Shub_Spgr_Info_Type("34.156 ","Pnn2        ","34.1.231   ","Pnn2          ","Pnn2.1                     ","P 2 -2n             ",  &
                                               "-x+1/2,y+1/2,z+1/2,1;x+1/2,-y+1/2,z+1/2,1;-x,-y,z,1                                                     ")
     Shubnikov_info( 232)= Shub_Spgr_Info_Type("34.157 ","Pnn21'      ","34.2.232   ","Pnn21'        ","Pnn2.1'                    ","P 2 -2n 1'          ",  &
                                               "-x+1/2,y+1/2,z+1/2,1;x+1/2,-y+1/2,z+1/2,1;-x,-y,z,1;x,y,z,-1                                            ")
     Shubnikov_info( 233)= Shub_Spgr_Info_Type("34.158 ","Pn'n2'      ","34.3.233   ","Pn'n2'        ","Pn'n2'                     ","P 2' -2n            ",  &
                                               "-x+1/2,y+1/2,z+1/2,-1;x+1/2,-y+1/2,z+1/2,1;-x,-y,z,-1                                                   ")
     Shubnikov_info( 234)= Shub_Spgr_Info_Type("34.159 ","Pn'n'2      ","34.4.234   ","Pn'n'2        ","Pn'n'2                     ","P 2 -2n'            ",  &
                                               "-x+1/2,y+1/2,z+1/2,-1;x+1/2,-y+1/2,z+1/2,-1;-x,-y,z,1                                                   ")
     Shubnikov_info( 235)= Shub_Spgr_Info_Type("43.228 ","F_Sdd2      ","34.5.235   ","P_Inn2        ","Fdd2.1'_I[Pnn2]            ","F 2 -2d 1'n         ",  &
                                               "x+1/2,y+1/2,z,1;x+1/2,y,z+1/2,1;-x+1/4,y+1/4,z+1/4,1;x+1/4,-y+1/4,z+1/4,1;-x,-y,z,1;x+1/2,y+1/2,z+1/2,-1")
     Shubnikov_info( 236)= Shub_Spgr_Info_Type("35.165 ","Cmm2        ","35.1.236   ","Cmm2          ","Cmm2.1                     ","C 2 -2              ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x,-y,z,1;-x,-y,z,1                                                             ")
     Shubnikov_info( 237)= Shub_Spgr_Info_Type("35.166 ","Cmm21'      ","35.2.237   ","Cmm21'        ","Cmm2.1'                    ","C 2 -2 1'           ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x,-y,z,1;-x,-y,z,1;x,y,z,-1                                                    ")
     Shubnikov_info( 238)= Shub_Spgr_Info_Type("35.167 ","Cm'm2'      ","35.3.238   ","Cm'm2'        ","Cm'm2'                     ","C 2' -2             ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,-1;x,-y,z,1;-x,-y,z,-1                                                           ")
     Shubnikov_info( 239)= Shub_Spgr_Info_Type("35.168 ","Cm'm'2      ","35.4.239   ","Cm'm'2        ","Cm'm'2                     ","C 2 -2'             ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,-1;x,-y,z,-1;-x,-y,z,1                                                           ")
     Shubnikov_info( 240)= Shub_Spgr_Info_Type("35.169 ","C_cmm2      ","35.5.240   ","C_2cmm2       ","Cmm2.1'_c[Cmm2]            ","C 2 -2 1'c          ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x,-y,z,1;-x,-y,z,1;x,y,z+1/2,-1                                                ")
     Shubnikov_info( 241)= Shub_Spgr_Info_Type("25.63  ","P_Cmm2      ","35.6.241   ","C_Pmm2        ","Pmm2.1'_C[Cmm2]            ","P 2 -2 1'C          ",  &
                                               "-x,y,z,1;x,-y,z,1;-x,-y,z,1;x+1/2,y+1/2,z,-1                                                            ")
     Shubnikov_info( 242)= Shub_Spgr_Info_Type("44.233 ","I_cmm2      ","35.7.242   ","C_Imm2        ","Imm2.1'_c[Cmm2]            ","I 2 -2 1'c          ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z,1;x,-y,z,1;-x,-y,z,1;x,y,z+1/2,-1                                            ")
     Shubnikov_info( 243)= Shub_Spgr_Info_Type("36.177 ","C_cmc2_1    ","35.8.243   ","C_2cm'm2'     ","Cmc2_1.1'_c[Cmm2]          ","C 2c -2 1'c         ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x,-y,z+1/2,1;-x,-y,z+1/2,1;x,y,z+1/2,-1                                        ")
     Shubnikov_info( 244)= Shub_Spgr_Info_Type("37.184 ","C_ccc2      ","35.9.244   ","C_2cm'm'2     ","Ccc2.1'_c[Cmm2]            ","C 2 -2c 1'c         ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z+1/2,1;x,-y,z+1/2,1;-x,-y,z,1;x,y,z+1/2,-1                                        ")
     Shubnikov_info( 245)= Shub_Spgr_Info_Type("28.97  ","P_Cma2      ","35.10.245  ","C_Pm'm2'      ","Pma2.1'_C[Cmm2]            ","P 2 -2a 1'C         ",  &
                                               "-x+1/2,y,z,1;x+1/2,-y,z,1;-x,-y,z,1;x+1/2,y+1/2,z,-1                                                    ")
     Shubnikov_info( 246)= Shub_Spgr_Info_Type("32.141 ","P_Cba2      ","35.11.246  ","C_Pm'm'2      ","Pba2.1'_C[Cmm2]            ","P 2 -2ab 1'C        ",  &
                                               "-x+1/2,y+1/2,z,1;x+1/2,-y+1/2,z,1;-x,-y,z,1;x+1/2,y+1/2,z,-1                                            ")
     Shubnikov_info( 247)= Shub_Spgr_Info_Type("46.246 ","I_cma2      ","35.12.247  ","C_Im'm2'      ","Ima2.1'_c[Cmm2]            ","I 2 -2a 1'c         ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x+1/2,y,z,1;x+1/2,-y,z,1;-x,-y,z,1;x,y,z+1/2,-1                                    ")
     Shubnikov_info( 248)= Shub_Spgr_Info_Type("45.239 ","I_cba2      ","35.13.248  ","C_Im'm'2      ","Iba2.1'_c[Cmm2]            ","I 2 -2c 1'c         ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z+1/2,1;x,-y,z+1/2,1;-x,-y,z,1;x,y,z+1/2,-1                                    ")
     Shubnikov_info( 249)= Shub_Spgr_Info_Type("36.172 ","Cmc2_1      ","36.1.249   ","Cmc2_1        ","Cmc2_1.1                   ","C 2c -2             ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x,-y,z+1/2,1;-x,-y,z+1/2,1                                                     ")
     Shubnikov_info( 250)= Shub_Spgr_Info_Type("36.173 ","Cmc2_11'    ","36.2.250   ","Cmc2_11'      ","Cmc2_1.1'                  ","C 2c -2 1'          ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x,-y,z+1/2,1;-x,-y,z+1/2,1;x,y,z,-1                                            ")
     Shubnikov_info( 251)= Shub_Spgr_Info_Type("36.174 ","Cm'c2_1'    ","36.3.251   ","Cm'c2_1'      ","Cm'c2_1'                   ","C 2c' -2'           ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,-1;x,-y,z+1/2,1;-x,-y,z+1/2,-1                                                   ")
     Shubnikov_info( 252)= Shub_Spgr_Info_Type("36.175 ","Cmc'2_1'    ","36.4.252   ","Cmc'2_1'      ","Cmc'2_1'                   ","C 2c' -2            ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x,-y,z+1/2,-1;-x,-y,z+1/2,-1                                                   ")
     Shubnikov_info( 253)= Shub_Spgr_Info_Type("36.176 ","Cm'c'2_1    ","36.5.253   ","Cm'c'2_1      ","Cm'c'2_1                   ","C 2c -2'            ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,-1;x,-y,z+1/2,-1;-x,-y,z+1/2,1                                                   ")
     Shubnikov_info( 254)= Shub_Spgr_Info_Type("26.76  ","P_Cmc2_1    ","36.6.254   ","C_Pmc2_1      ","Pmc2_1.1'_C[Cmc2_1]        ","P 2c -2 1'C         ",  &
                                               "-x,y,z,1;x,-y,z+1/2,1;-x,-y,z+1/2,1;x+1/2,y+1/2,z,-1                                                    ")
     Shubnikov_info( 255)= Shub_Spgr_Info_Type("29.109 ","P_Cca2_1    ","36.7.255   ","C_Pm'c2_1'    ","Pca2_1.1'_C[Ccm2_1]        ","P 2c -2ac 1'C       ",  &
                                               "-x+1/2,y,z+1/2,1;x+1/2,-y,z,1;-x,-y,z+1/2,1;x+1/2,y+1/2,z,-1                                            ")
     Shubnikov_info( 256)= Shub_Spgr_Info_Type("31.133 ","P_Cmn2_1    ","36.8.256   ","C_Pmc'2_1'    ","Pmn2_1.1'_C[Cmc2_1]        ","P 2ac -2 1'C        ",  &
                                               "-x,y,z,1;x+1/2,-y,z+1/2,1;-x+1/2,-y,z+1/2,1;x+1/2,y+1/2,z,-1                                            ")
     Shubnikov_info( 257)= Shub_Spgr_Info_Type("33.154 ","P_Cna2_1    ","36.9.257   ","C_Pm'c'2_1    ","Pna2_1.1'_C[Ccm2_1]        ","P 2c -2n 1'C        ",  &
                                               "-x+1/2,y+1/2,z+1/2,1;x+1/2,-y+1/2,z,1;-x,-y,z+1/2,1;x+1/2,y+1/2,z,-1                                    ")
     Shubnikov_info( 258)= Shub_Spgr_Info_Type("37.180 ","Ccc2        ","37.1.258   ","Ccc2          ","Ccc2.1                     ","C 2 -2c             ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z+1/2,1;x,-y,z+1/2,1;-x,-y,z,1                                                     ")
     Shubnikov_info( 259)= Shub_Spgr_Info_Type("37.181 ","Ccc21'      ","37.2.259   ","Ccc21'        ","Ccc2.1'                    ","C 2 -2c 1'          ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z+1/2,1;x,-y,z+1/2,1;-x,-y,z,1;x,y,z,-1                                            ")
     Shubnikov_info( 260)= Shub_Spgr_Info_Type("37.182 ","Cc'c2'      ","37.3.260   ","Cc'c2'        ","Cc'c2'                     ","C 2' -2c            ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z+1/2,-1;x,-y,z+1/2,1;-x,-y,z,-1                                                   ")
     Shubnikov_info( 261)= Shub_Spgr_Info_Type("37.183 ","Cc'c'2      ","37.4.261   ","Cc'c'2        ","Cc'c'2                     ","C 2 -2c'            ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z+1/2,-1;x,-y,z+1/2,-1;-x,-y,z,1                                                   ")
     Shubnikov_info( 262)= Shub_Spgr_Info_Type("27.84  ","P_Ccc2      ","37.5.262   ","C_Pcc2        ","Pcc2.1'_C[Ccc2]            ","P 2 -2c 1'C         ",  &
                                               "-x,y,z+1/2,1;x,-y,z+1/2,1;-x,-y,z,1;x+1/2,y+1/2,z,-1                                                    ")
     Shubnikov_info( 263)= Shub_Spgr_Info_Type("30.121 ","P_Cnc2      ","37.6.263   ","C_Pc'c2'      ","Pnc2.1'_C[Ccc2]            ","P 2 -2bc 1'C        ",  &
                                               "-x,y+1/2,z+1/2,1;x,-y+1/2,z+1/2,1;-x,-y,z,1;x+1/2,y+1/2,z,-1                                            ")
     Shubnikov_info( 264)= Shub_Spgr_Info_Type("34.163 ","P_Cnn2      ","37.7.264   ","C_Pc'c'2      ","Pnn2.1'_C[Ccc2]            ","P 2 -2n 1'C         ",  &
                                               "-x+1/2,y+1/2,z+1/2,1;x+1/2,-y+1/2,z+1/2,1;-x,-y,z,1;x+1/2,y+1/2,z,-1                                    ")
     Shubnikov_info( 265)= Shub_Spgr_Info_Type("38.187 ","Amm2        ","38.1.265   ","Amm2          ","Amm2.1                     ","A 2 -2              ",  &
                                               "x,y+1/2,z+1/2,1;-x,y,z,1;x,-y,z,1;-x,-y,z,1                                                             ")
     Shubnikov_info( 266)= Shub_Spgr_Info_Type("38.188 ","Amm21'      ","38.2.266   ","Amm21'        ","Amm2.1'                    ","A 2 -2 1'           ",  &
                                               "x,y+1/2,z+1/2,1;-x,y,z,1;x,-y,z,1;-x,-y,z,1;x,y,z,-1                                                    ")
     Shubnikov_info( 267)= Shub_Spgr_Info_Type("38.189 ","Am'm2'      ","38.3.267   ","Am'm2'        ","Am'm2'                     ","A 2' -2'            ",  &
                                               "x,y+1/2,z+1/2,1;-x,y,z,-1;x,-y,z,1;-x,-y,z,-1                                                           ")
     Shubnikov_info( 268)= Shub_Spgr_Info_Type("38.190 ","Amm'2'      ","38.4.268   ","Amm'2'        ","Amm'2'                     ","A 2' -2             ",  &
                                               "x,y+1/2,z+1/2,1;-x,y,z,1;x,-y,z,-1;-x,-y,z,-1                                                           ")
     Shubnikov_info( 269)= Shub_Spgr_Info_Type("38.191 ","Am'm'2      ","38.5.269   ","Am'm'2        ","Am'm'2                     ","A 2 -2'             ",  &
                                               "x,y+1/2,z+1/2,1;-x,y,z,-1;x,-y,z,-1;-x,-y,z,1                                                           ")
     Shubnikov_info( 270)= Shub_Spgr_Info_Type("38.192 ","A_amm2      ","38.6.270   ","A_2amm2       ","Amm2.1'_a[Amm2]            ","A 2 -2 1'a          ",  &
                                               "x,y+1/2,z+1/2,1;-x,y,z,1;x,-y,z,1;-x,-y,z,1;x+1/2,y,z,-1                                                ")
     Shubnikov_info( 271)= Shub_Spgr_Info_Type("25.64  ","P_Amm2      ","38.7.271   ","A_Pmm2        ","Pmm2.1'_A[Amm2]            ","P 2 -2 1'A          ",  &
                                               "-x,y,z,1;x,-y,z,1;-x,-y,z,1;x,y+1/2,z+1/2,-1                                                            ")
     Shubnikov_info( 272)= Shub_Spgr_Info_Type("44.234 ","I_amm2      ","38.8.272   ","A_Imm2        ","Imm2.1'_a[Amm2]            ","I 2 -2 1'a          ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z,1;x,-y,z,1;-x,-y,z,1;x+1/2,y,z,-1                                            ")
     Shubnikov_info( 273)= Shub_Spgr_Info_Type("40.208 ","A_ama2      ","38.9.273   ","A_2amm'2'     ","Ama2.1'_a[Amm2]            ","A 2 -2a 1'a         ",  &
                                               "x,y+1/2,z+1/2,1;-x+1/2,y,z,1;x+1/2,-y,z,1;-x,-y,z,1;x+1/2,y,z,-1                                        ")
     Shubnikov_info( 274)= Shub_Spgr_Info_Type("31.132 ","P_Bmn2_1    ","38.10.274  ","A_Pm'm2'      ","Pmn2_1.1'_B[Bmm2]          ","P 2ac -2 1'B        ",  &
                                               "-x,y,z,1;x+1/2,-y,z+1/2,1;-x+1/2,-y,z+1/2,1;x+1/2,y,z+1/2,-1                                            ")
     Shubnikov_info( 275)= Shub_Spgr_Info_Type("26.74  ","P_Amc2_1    ","38.11.275  ","A_Pmm'2'      ","Pmc2_1.1'_A[Amm2]          ","P 2c -2 1'A         ",  &
                                               "-x,y,z,1;x,-y,z+1/2,1;-x,-y,z+1/2,1;x,y+1/2,z+1/2,-1                                                    ")
     Shubnikov_info( 276)= Shub_Spgr_Info_Type("30.119 ","P_Anc2      ","38.12.276  ","A_Pm'm'2      ","Pnc2.1'_A[Amm2]            ","P 2 -2bc 1'A        ",  &
                                               "-x,y+1/2,z+1/2,1;x,-y+1/2,z+1/2,1;-x,-y,z,1;x,y+1/2,z+1/2,-1                                            ")
     Shubnikov_info( 277)= Shub_Spgr_Info_Type("46.247 ","I_ama2      ","38.13.277  ","A_Im'm'2      ","Ima2.1'_a[Amm2]            ","I 2 -2a 1'a         ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x+1/2,y,z,1;x+1/2,-y,z,1;-x,-y,z,1;x+1/2,y,z,-1                                    ")
     Shubnikov_info( 278)= Shub_Spgr_Info_Type("39.195 ","Abm2        ","39.1.278   ","Abm2          ","Abm2.1                     ","A 2 -2c             ",  &
                                               "x,y+1/2,z+1/2,1;-x,y,z+1/2,1;x,-y,z+1/2,1;-x,-y,z,1                                                     ")
     Shubnikov_info( 279)= Shub_Spgr_Info_Type("39.196 ","Abm21'      ","39.2.279   ","Abm21'        ","Abm2.1'                    ","A 2 -2c 1'          ",  &
                                               "x,y+1/2,z+1/2,1;-x,y,z+1/2,1;x,-y,z+1/2,1;-x,-y,z,1;x,y,z,-1                                            ")
     Shubnikov_info( 280)= Shub_Spgr_Info_Type("39.197 ","Ab'm2'      ","39.3.280   ","Ab'm2'        ","Ab'm2'                     ","A 2' -2c'           ",  &
                                               "x,y+1/2,z+1/2,1;-x,y,z+1/2,-1;x,-y,z+1/2,1;-x,-y,z,-1                                                   ")
     Shubnikov_info( 281)= Shub_Spgr_Info_Type("39.198 ","Abm'2'      ","39.4.281   ","Abm'2'        ","Abm'2'                     ","A 2' -2c            ",  &
                                               "x,y+1/2,z+1/2,1;-x,y,z+1/2,1;x,-y,z+1/2,-1;-x,-y,z,-1                                                   ")
     Shubnikov_info( 282)= Shub_Spgr_Info_Type("39.199 ","Ab'm'2      ","39.5.282   ","Ab'm'2        ","Ab'm'2                     ","A 2 -2c'            ",  &
                                               "x,y+1/2,z+1/2,1;-x,y,z+1/2,-1;x,-y,z+1/2,-1;-x,-y,z,1                                                   ")
     Shubnikov_info( 283)= Shub_Spgr_Info_Type("39.200 ","A_abm2      ","39.6.283   ","A_2abm2       ","Abm2.1'_a[Abm2]            ","A 2 -2c 1'a         ",  &
                                               "x,y+1/2,z+1/2,1;-x,y,z+1/2,1;x,-y,z+1/2,1;-x,-y,z,1;x+1/2,y,z,-1                                        ")
     Shubnikov_info( 284)= Shub_Spgr_Info_Type("28.96  ","P_Bma2      ","39.7.284   ","A_Pbm2        ","Pma2.1'_B[Bma2]            ","P 2 -2a 1'B         ",  &
                                               "-x+1/2,y,z,1;x+1/2,-y,z,1;-x,-y,z,1;x+1/2,y,z+1/2,-1                                                    ")
     Shubnikov_info( 285)= Shub_Spgr_Info_Type("46.248 ","I_bma2      ","39.8.285   ","A_Ibm2        ","Ima2.1'_b[Bma2]            ","I 2 -2a 1'b         ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x+1/2,y,z,1;x+1/2,-y,z,1;-x,-y,z,1;x,y+1/2,z,-1                                    ")
     Shubnikov_info( 286)= Shub_Spgr_Info_Type("41.216 ","A_aba2      ","39.9.286   ","A_2ab'm'2     ","Aba2.1'_a[Abm2]            ","A 2 -2ac 1'a        ",  &
                                               "x,y+1/2,z+1/2,1;-x+1/2,y,z+1/2,1;x+1/2,-y,z+1/2,1;-x,-y,z,1;x+1/2,y,z,-1                                ")
     Shubnikov_info( 287)= Shub_Spgr_Info_Type("26.75  ","P_Bmc2_1    ","39.10.287  ","A_Pb'm2'      ","Pmc2_1.1'_B[Bma2]          ","P 2c -2 1'B         ",  &
                                               "-x,y,z,1;x,-y,z+1/2,1;-x,-y,z+1/2,1;x+1/2,y,z+1/2,-1                                                    ")
     Shubnikov_info( 288)= Shub_Spgr_Info_Type("29.108 ","P_Bca2_1    ","39.11.288  ","A_Pbm'2'      ","Pca2_1.1'_B[Bma2]          ","P 2c -2ac 1'B       ",  &
                                               "-x+1/2,y,z+1/2,1;x+1/2,-y,z,1;-x,-y,z+1/2,1;x+1/2,y,z+1/2,-1                                            ")
     Shubnikov_info( 289)= Shub_Spgr_Info_Type("27.85  ","P_Acc2      ","39.12.289  ","A_Pb'm'2      ","Pcc2.1'_A[Abm2]            ","P 2 -2c 1'A         ",  &
                                               "-x,y,z+1/2,1;x,-y,z+1/2,1;-x,-y,z,1;x,y+1/2,z+1/2,-1                                                    ")
     Shubnikov_info( 290)= Shub_Spgr_Info_Type("45.240 ","I_aba2      ","39.13.290  ","A_Ib'm'2      ","Iba2.1'_a[Abm2]            ","I 2 -2c 1'a         ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z+1/2,1;x,-y,z+1/2,1;-x,-y,z,1;x+1/2,y,z,-1                                    ")
     Shubnikov_info( 291)= Shub_Spgr_Info_Type("40.203 ","Ama2        ","40.1.291   ","Ama2          ","Ama2.1                     ","A 2 -2a             ",  &
                                               "x,y+1/2,z+1/2,1;-x+1/2,y,z,1;x+1/2,-y,z,1;-x,-y,z,1                                                     ")
     Shubnikov_info( 292)= Shub_Spgr_Info_Type("40.204 ","Ama21'      ","40.2.292   ","Ama21'        ","Ama2.1'                    ","A 2 -2a 1'          ",  &
                                               "x,y+1/2,z+1/2,1;-x+1/2,y,z,1;x+1/2,-y,z,1;-x,-y,z,1;x,y,z,-1                                            ")
     Shubnikov_info( 293)= Shub_Spgr_Info_Type("40.205 ","Am'a2'      ","40.3.293   ","Am'a2'        ","Am'a2'                     ","A 2' -2a'           ",  &
                                               "x,y+1/2,z+1/2,1;-x+1/2,y,z,-1;x+1/2,-y,z,1;-x,-y,z,-1                                                   ")
     Shubnikov_info( 294)= Shub_Spgr_Info_Type("40.206 ","Ama'2'      ","40.4.294   ","Ama'2'        ","Ama'2'                     ","A 2' -2a            ",  &
                                               "x,y+1/2,z+1/2,1;-x+1/2,y,z,1;x+1/2,-y,z,-1;-x,-y,z,-1                                                   ")
     Shubnikov_info( 295)= Shub_Spgr_Info_Type("40.207 ","Am'a'2      ","40.5.295   ","Am'a'2        ","Am'a'2                     ","A 2 -2a'            ",  &
                                               "x,y+1/2,z+1/2,1;-x+1/2,y,z,-1;x+1/2,-y,z,-1;-x,-y,z,1                                                   ")
     Shubnikov_info( 296)= Shub_Spgr_Info_Type("28.95  ","P_Ama2      ","40.6.296   ","A_Pma2        ","Pma2.1'_A[Ama2]            ","P 2 -2a 1'A         ",  &
                                               "-x+1/2,y,z,1;x+1/2,-y,z,1;-x,-y,z,1;x,y+1/2,z+1/2,-1                                                    ")
     Shubnikov_info( 297)= Shub_Spgr_Info_Type("33.152 ","P_Ana2_1    ","40.7.297   ","A_Pm'a2'      ","Pna2_1.1'_A[Ama2]          ","P 2c -2n 1'A        ",  &
                                               "-x+1/2,y+1/2,z+1/2,1;x+1/2,-y+1/2,z,1;-x,-y,z+1/2,1;x,y+1/2,z+1/2,-1                                    ")
     Shubnikov_info( 298)= Shub_Spgr_Info_Type("31.131 ","P_Amn2_1    ","40.8.298   ","A_Pma'2'      ","Pmn2_1.1'_A[Ama2]          ","P 2ac -2 1'A        ",  &
                                               "-x,y,z,1;x+1/2,-y,z+1/2,1;-x+1/2,-y,z+1/2,1;x,y+1/2,z+1/2,-1                                            ")
     Shubnikov_info( 299)= Shub_Spgr_Info_Type("34.162 ","P_Ann2      ","40.9.299   ","A_Pm'a'2      ","Pnn2.1'_A[Ama2]            ","P 2 -2n 1'A         ",  &
                                               "-x+1/2,y+1/2,z+1/2,1;x+1/2,-y+1/2,z+1/2,1;-x,-y,z,1;x,y+1/2,z+1/2,-1                                    ")
     Shubnikov_info( 300)= Shub_Spgr_Info_Type("41.211 ","Aba2        ","41.1.300   ","Aba2          ","Aba2.1                     ","A 2 -2ac            ",  &
                                               "x,y+1/2,z+1/2,1;-x+1/2,y,z+1/2,1;x+1/2,-y,z+1/2,1;-x,-y,z,1                                             ")
     Shubnikov_info( 301)= Shub_Spgr_Info_Type("41.212 ","Aba21'      ","41.2.301   ","Aba21'        ","Aba2.1'                    ","A 2 -2ac 1'         ",  &
                                               "x,y+1/2,z+1/2,1;-x+1/2,y,z+1/2,1;x+1/2,-y,z+1/2,1;-x,-y,z,1;x,y,z,-1                                    ")
     Shubnikov_info( 302)= Shub_Spgr_Info_Type("41.213 ","Ab'a2'      ","41.3.302   ","Ab'a2'        ","Ab'a2'                     ","A 2' -2ac'          ",  &
                                               "x,y+1/2,z+1/2,1;-x+1/2,y,z+1/2,-1;x+1/2,-y,z+1/2,1;-x,-y,z,-1                                           ")
     Shubnikov_info( 303)= Shub_Spgr_Info_Type("41.214 ","Aba'2'      ","41.4.303   ","Aba'2'        ","Aba'2'                     ","A 2' -2ac           ",  &
                                               "x,y+1/2,z+1/2,1;-x+1/2,y,z+1/2,1;x+1/2,-y,z+1/2,-1;-x,-y,z,-1                                           ")
     Shubnikov_info( 304)= Shub_Spgr_Info_Type("41.215 ","Ab'a'2      ","41.5.304   ","Ab'a'2        ","Ab'a'2                     ","A 2 -2ac'           ",  &
                                               "x,y+1/2,z+1/2,1;-x+1/2,y,z+1/2,-1;x+1/2,-y,z+1/2,-1;-x,-y,z,1                                           ")
     Shubnikov_info( 305)= Shub_Spgr_Info_Type("32.142 ","P_Aba2      ","41.6.305   ","A_Pba2        ","Pba2.1'_A[Aba2]            ","P 2 -2ab 1'A        ",  &
                                               "-x+1/2,y+1/2,z,1;x+1/2,-y+1/2,z,1;-x,-y,z,1;x,y+1/2,z+1/2,-1                                            ")
     Shubnikov_info( 306)= Shub_Spgr_Info_Type("29.107 ","P_Aca2_1    ","41.7.306   ","A_Pb'a2'      ","Pca2_1.1'_A[Aba2]          ","P 2c -2ac 1'A       ",  &
                                               "-x+1/2,y,z+1/2,1;x+1/2,-y,z,1;-x,-y,z+1/2,1;x,y+1/2,z+1/2,-1                                            ")
     Shubnikov_info( 307)= Shub_Spgr_Info_Type("33.153 ","P_Bna2_1    ","41.8.307   ","A_Pba'2'      ","Pna2_1.1'_B[Bba2]          ","P 2c -2n 1'B        ",  &
                                               "-x+1/2,y+1/2,z+1/2,1;x+1/2,-y+1/2,z,1;-x,-y,z+1/2,1;x+1/2,y,z+1/2,-1                                    ")
     Shubnikov_info( 308)= Shub_Spgr_Info_Type("30.120 ","P_Bnc2      ","41.9.308   ","A_Pb'a'2      ","Pnc2.1'_B[Bba2]            ","P 2 -2bc 1'B        ",  &
                                               "-x,y+1/2,z+1/2,1;x,-y+1/2,z+1/2,1;-x,-y,z,1;x+1/2,y,z+1/2,-1                                            ")
     Shubnikov_info( 309)= Shub_Spgr_Info_Type("42.219 ","Fmm2        ","42.1.309   ","Fmm2          ","Fmm2.1                     ","F 2 -2              ",  &
                                               "x+1/2,y+1/2,z,1;x+1/2,y,z+1/2,1;-x,y,z,1;x,-y,z,1;-x,-y,z,1                                             ")
     Shubnikov_info( 310)= Shub_Spgr_Info_Type("42.220 ","Fmm21'      ","42.2.310   ","Fmm21'        ","Fmm2.1'                    ","F 2 -2 1'           ",  &
                                               "x+1/2,y+1/2,z,1;x+1/2,y,z+1/2,1;-x,y,z,1;x,-y,z,1;-x,-y,z,1;x,y,z,-1                                    ")
     Shubnikov_info( 311)= Shub_Spgr_Info_Type("42.221 ","Fm'm2'      ","42.3.311   ","Fm'm2'        ","Fm'm2'                     ","F 2' -2             ",  &
                                               "x+1/2,y+1/2,z,1;x+1/2,y,z+1/2,1;-x,y,z,-1;x,-y,z,1;-x,-y,z,-1                                           ")
     Shubnikov_info( 312)= Shub_Spgr_Info_Type("42.222 ","Fm'm'2      ","42.4.312   ","Fm'm'2        ","Fm'm'2                     ","F 2 -2'             ",  &
                                               "x+1/2,y+1/2,z,1;x+1/2,y,z+1/2,1;-x,y,z,-1;x,-y,z,-1;-x,-y,z,1                                           ")
     Shubnikov_info( 313)= Shub_Spgr_Info_Type("35.171 ","C_Amm2      ","42.5.313   ","F_Cmm2        ","Cmm2.1'_A[Fmm2]            ","C 2 -2 1'A          ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x,-y,z,1;-x,-y,z,1;x,y+1/2,z+1/2,-1                                            ")
     Shubnikov_info( 314)= Shub_Spgr_Info_Type("38.194 ","A_Bmm2      ","42.6.314   ","F_Amm2        ","Amm2.1'_B[Fmm2]            ","A 2 -2 1'B          ",  &
                                               "x,y+1/2,z+1/2,1;-x,y,z,1;x,-y,z,1;-x,-y,z,1;x+1/2,y+1/2,z,-1                                            ")
     Shubnikov_info( 315)= Shub_Spgr_Info_Type("36.179 ","C_Amc2_1    ","42.7.315   ","F_Cmm'2'      ","Cmc2_1.1'_A[Fmm2]          ","C 2c -2 1'A         ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x,-y,z+1/2,1;-x,-y,z+1/2,1;x,y+1/2,z+1/2,-1                                    ")
     Shubnikov_info( 316)= Shub_Spgr_Info_Type("37.186 ","C_Acc2      ","42.8.316   ","F_Cm'm'2      ","Ccc2.1'_A[Fmm2]            ","C 2 -2c 1'A         ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z+1/2,1;x,-y,z+1/2,1;-x,-y,z,1;x,y+1/2,z+1/2,-1                                    ")
     Shubnikov_info( 317)= Shub_Spgr_Info_Type("39.202 ","A_Bbm2      ","42.9.317   ","F_Am'm2'      ","Abm2.1'_B[Fmm2]            ","A 2 -2c 1'B         ",  &
                                               "x,y+1/2,z+1/2,1;-x,y,z+1/2,1;x,-y,z+1/2,1;-x,-y,z,1;x+1/2,y+1/2,z,-1                                    ")
     Shubnikov_info( 318)= Shub_Spgr_Info_Type("40.210 ","A_Bma2      ","42.10.318  ","F_Amm'2'      ","Ama2.1'_B[Fmm2]            ","A 2 -2a 1'B         ",  &
                                               "x,y+1/2,z+1/2,1;-x+1/2,y,z,1;x+1/2,-y,z,1;-x,-y,z,1;x+1/2,y+1/2,z,-1                                    ")
     Shubnikov_info( 319)= Shub_Spgr_Info_Type("41.218 ","A_Bba2      ","42.11.319  ","F_Am'm'2      ","Aba2.1'_B[Fmm2]            ","A 2 -2ac 1'B        ",  &
                                               "x,y+1/2,z+1/2,1;-x+1/2,y,z+1/2,1;x+1/2,-y,z+1/2,1;-x,-y,z,1;x+1/2,y+1/2,z,-1                            ")
     Shubnikov_info( 320)= Shub_Spgr_Info_Type("43.224 ","Fdd2        ","43.1.320   ","Fdd2          ","Fdd2.1                     ","F 2 -2d             ",  &
                                               "x+1/2,y+1/2,z,1;x+1/2,y,z+1/2,1;-x+1/4,y+1/4,z+1/4,1;x+1/4,-y+1/4,z+1/4,1;-x,-y,z,1                     ")
     Shubnikov_info( 321)= Shub_Spgr_Info_Type("43.225 ","Fdd21'      ","43.2.321   ","Fdd21'        ","Fdd2.1'                    ","F 2 -2d 1'          ",  &
                                               "x+1/2,y+1/2,z,1;x+1/2,y,z+1/2,1;-x+1/4,y+1/4,z+1/4,1;x+1/4,-y+1/4,z+1/4,1;-x,-y,z,1;x,y,z,-1            ")
     Shubnikov_info( 322)= Shub_Spgr_Info_Type("43.226 ","Fd'd2'      ","43.3.322   ","Fd'd2'        ","Fd'd2'                     ","F 2' -2d            ",  &
                                               "x+1/2,y+1/2,z,1;x+1/2,y,z+1/2,1;-x+1/4,y+1/4,z+1/4,-1;x+1/4,-y+1/4,z+1/4,1;-x,-y,z,-1                   ")
     Shubnikov_info( 323)= Shub_Spgr_Info_Type("43.227 ","Fd'd'2      ","43.4.323   ","Fd'd'2        ","Fd'd'2                     ","F 2 -2d'            ",  &
                                               "x+1/2,y+1/2,z,1;x+1/2,y,z+1/2,1;-x+1/4,y+1/4,z+1/4,-1;x+1/4,-y+1/4,z+1/4,-1;-x,-y,z,1                   ")
     Shubnikov_info( 324)= Shub_Spgr_Info_Type("44.229 ","Imm2        ","44.1.324   ","Imm2          ","Imm2.1                     ","I 2 -2              ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z,1;x,-y,z,1;-x,-y,z,1                                                         ")
     Shubnikov_info( 325)= Shub_Spgr_Info_Type("44.230 ","Imm21'      ","44.2.325   ","Imm21'        ","Imm2.1'                    ","I 2 -2 1'           ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z,1;x,-y,z,1;-x,-y,z,1;x,y,z,-1                                                ")
     Shubnikov_info( 326)= Shub_Spgr_Info_Type("44.231 ","Im'm2'      ","44.3.326   ","Im'm2'        ","Im'm2'                     ","I 2' -2             ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z,-1;x,-y,z,1;-x,-y,z,-1                                                       ")
     Shubnikov_info( 327)= Shub_Spgr_Info_Type("44.232 ","Im'm'2      ","44.4.327   ","Im'm'2        ","Im'm'2                     ","I 2 -2'             ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z,-1;x,-y,z,-1;-x,-y,z,1                                                       ")
     Shubnikov_info( 328)= Shub_Spgr_Info_Type("25.65  ","P_Imm2      ","44.5.328   ","I_Pmm2        ","Pmm2.1'_I[Imm2]            ","P 2 -2 1'I          ",  &
                                               "-x,y,z,1;x,-y,z,1;-x,-y,z,1;x+1/2,y+1/2,z+1/2,-1                                                        ")
     Shubnikov_info( 329)= Shub_Spgr_Info_Type("31.134 ","P_Imn2_1    ","44.6.329   ","I_Pmm'2'      ","Pmn2_1.1'_I[Imm2]          ","P 2ac -2 1'I        ",  &
                                               "-x,y,z,1;x+1/2,-y,z+1/2,1;-x+1/2,-y,z+1/2,1;x+1/2,y+1/2,z+1/2,-1                                        ")
     Shubnikov_info( 330)= Shub_Spgr_Info_Type("34.164 ","P_Inn2      ","44.7.330   ","I_Pm'm'2      ","Pnn2.1'_I[Imm2]            ","P 2 -2n 1'I         ",  &
                                               "-x+1/2,y+1/2,z+1/2,1;x+1/2,-y+1/2,z+1/2,1;-x,-y,z,1;x+1/2,y+1/2,z+1/2,-1                                ")
     Shubnikov_info( 331)= Shub_Spgr_Info_Type("45.235 ","Iba2        ","45.1.331   ","Iba2          ","Iba2.1                     ","I 2 -2c             ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z+1/2,1;x,-y,z+1/2,1;-x,-y,z,1                                                 ")
     Shubnikov_info( 332)= Shub_Spgr_Info_Type("45.236 ","Iba21'      ","45.2.332   ","Iba21'        ","Iba2.1'                    ","I 2 -2c 1'          ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z+1/2,1;x,-y,z+1/2,1;-x,-y,z,1;x,y,z,-1                                        ")
     Shubnikov_info( 333)= Shub_Spgr_Info_Type("45.237 ","Ib'a2'      ","45.3.333   ","Ib'a2'        ","Ib'a2'                     ","I 2' -2c'           ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z+1/2,-1;x,-y,z+1/2,1;-x,-y,z,-1                                               ")
     Shubnikov_info( 334)= Shub_Spgr_Info_Type("45.238 ","Ib'a'2      ","45.4.334   ","Ib'a'2        ","Ib'a'2                     ","I 2 -2c'            ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z+1/2,-1;x,-y,z+1/2,-1;-x,-y,z,1                                               ")
     Shubnikov_info( 335)= Shub_Spgr_Info_Type("27.86  ","P_Icc2      ","45.5.335   ","I_Pba2        ","Pcc2.1'_I[Iba2]            ","P 2 -2c 1'I         ",  &
                                               "-x,y,z+1/2,1;x,-y,z+1/2,1;-x,-y,z,1;x+1/2,y+1/2,z+1/2,-1                                                ")
     Shubnikov_info( 336)= Shub_Spgr_Info_Type("29.110 ","P_Ica2_1    ","45.6.336   ","I_Pba'2'      ","Pca2_1.1'_I[Iba2]          ","P 2c -2ac 1'I       ",  &
                                               "-x+1/2,y,z+1/2,1;x+1/2,-y,z,1;-x,-y,z+1/2,1;x+1/2,y+1/2,z+1/2,-1                                        ")
     Shubnikov_info( 337)= Shub_Spgr_Info_Type("32.143 ","P_Iba2      ","45.7.337   ","I_Pb'a'2      ","Pba2.1'_I[Iba2]            ","P 2 -2ab 1'I        ",  &
                                               "-x+1/2,y+1/2,z,1;x+1/2,-y+1/2,z,1;-x,-y,z,1;x+1/2,y+1/2,z+1/2,-1                                        ")
     Shubnikov_info( 338)= Shub_Spgr_Info_Type("46.241 ","Ima2        ","46.1.338   ","Ima2          ","Ima2.1                     ","I 2 -2a             ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x+1/2,y,z,1;x+1/2,-y,z,1;-x,-y,z,1                                                 ")
     Shubnikov_info( 339)= Shub_Spgr_Info_Type("46.242 ","Ima21'      ","46.2.339   ","Ima21'        ","Ima2.1'                    ","I 2 -2a 1'          ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x+1/2,y,z,1;x+1/2,-y,z,1;-x,-y,z,1;x,y,z,-1                                        ")
     Shubnikov_info( 340)= Shub_Spgr_Info_Type("46.243 ","Im'a2'      ","46.3.340   ","Im'a2'        ","Im'a2'                     ","I 2' -2a'           ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x+1/2,y,z,-1;x+1/2,-y,z,1;-x,-y,z,-1                                               ")
     Shubnikov_info( 341)= Shub_Spgr_Info_Type("46.244 ","Ima'2'      ","46.4.341   ","Ima'2'        ","Ima'2'                     ","I 2' -2a            ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x+1/2,y,z,1;x+1/2,-y,z,-1;-x,-y,z,-1                                               ")
     Shubnikov_info( 342)= Shub_Spgr_Info_Type("46.245 ","Im'a'2      ","46.5.342   ","Im'a'2        ","Im'a'2                     ","I 2 -2a'            ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x+1/2,y,z,-1;x+1/2,-y,z,-1;-x,-y,z,1                                               ")
     Shubnikov_info( 343)= Shub_Spgr_Info_Type("28.98  ","P_Ima2      ","46.6.343   ","I_Pma2        ","Pma2.1'_I[Ima2]            ","P 2 -2a 1'I         ",  &
                                               "-x+1/2,y,z,1;x+1/2,-y,z,1;-x,-y,z,1;x+1/2,y+1/2,z+1/2,-1                                                ")
     Shubnikov_info( 344)= Shub_Spgr_Info_Type("33.155 ","P_Ina2_1    ","46.7.344   ","I_Pm'a2'      ","Pna2_1.1'_I[Ima2]          ","P 2c -2n 1'I        ",  &
                                               "-x+1/2,y+1/2,z+1/2,1;x+1/2,-y+1/2,z,1;-x,-y,z+1/2,1;x+1/2,y+1/2,z+1/2,-1                                ")
     Shubnikov_info( 345)= Shub_Spgr_Info_Type("26.77  ","P_Imc2_1    ","46.8.345   ","I_Pma'2'      ","Pmc2_1.1'_I[Ima2]          ","P 2c -2 1'I         ",  &
                                               "-x,y,z,1;x,-y,z+1/2,1;-x,-y,z+1/2,1;x+1/2,y+1/2,z+1/2,-1                                                ")
     Shubnikov_info( 346)= Shub_Spgr_Info_Type("30.122 ","P_Inc2      ","46.9.346   ","I_Pm'a'2      ","Pnc2.1'_I[Ima2]            ","P 2 -2bc 1'I        ",  &
                                               "-x,y+1/2,z+1/2,1;x,-y+1/2,z+1/2,1;-x,-y,z,1;x+1/2,y+1/2,z+1/2,-1                                        ")
     Shubnikov_info( 347)= Shub_Spgr_Info_Type("47.249 ","Pmmm        ","47.1.347   ","Pmmm          ","Pmmm.1                     ","-P 2 2              ",  &
                                               "-x,y,z,1;x,-y,z,1;x,y,-z,1                                                                              ")
     Shubnikov_info( 348)= Shub_Spgr_Info_Type("47.250 ","Pmmm1'      ","47.2.348   ","Pmmm1'        ","Pmmm.1'                    ","-P 2 2 1'           ",  &
                                               "-x,y,z,1;x,-y,z,1;x,y,-z,1;x,y,z,-1                                                                     ")
     Shubnikov_info( 349)= Shub_Spgr_Info_Type("47.251 ","Pm'mm       ","47.3.349   ","Pm'mm         ","Pm'mm                      ","P 2' 2 -1'          ",  &
                                               "-x,y,z,-1;x,-y,z,1;x,y,-z,1                                                                             ")
     Shubnikov_info( 350)= Shub_Spgr_Info_Type("47.252 ","Pm'm'm      ","47.4.350   ","Pm'm'm        ","Pm'm'm                     ","-P 2 2'             ",  &
                                               "-x,y,z,-1;x,-y,z,-1;x,y,-z,1                                                                            ")
     Shubnikov_info( 351)= Shub_Spgr_Info_Type("47.253 ","Pm'm'm'     ","47.5.351   ","Pm'm'm'       ","Pm'm'm'                    ","P 2 2 -1'           ",  &
                                               "-x,y,z,-1;x,-y,z,-1;x,y,-z,-1                                                                           ")
     Shubnikov_info( 352)= Shub_Spgr_Info_Type("47.254 ","P_ammm      ","47.6.352   ","P_2ammm       ","Pmmm.1'_a[Pmmm]            ","-P 2 2 1'a          ",  &
                                               "-x,y,z,1;x,-y,z,1;x,y,-z,1;x+1/2,y,z,-1                                                                 ")
     Shubnikov_info( 353)= Shub_Spgr_Info_Type("65.489 ","C_ammm      ","47.7.353   ","P_Cmmm        ","Cmmm.1'_a[Pmmm]            ","-C 2 2 1'a          ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x,-y,z,1;x,y,-z,1;x+1/2,y,z,-1                                                 ")
     Shubnikov_info( 354)= Shub_Spgr_Info_Type("69.526 ","F_Smmm      ","47.8.354   ","P_Immm        ","Fmmm.1'_I[Pmmm]            ","-F 2 2 1'n          ",  &
                                               "x+1/2,y+1/2,z,1;x+1/2,y,z+1/2,1;-x,y,z,1;x,-y,z,1;x,y,-z,1;x+1/2,y+1/2,z+1/2,-1                         ")
     Shubnikov_info( 355)= Shub_Spgr_Info_Type("51.298 ","P_amma      ","47.9.355   ","P_2ammm'      ","Pmma.1'_a[Pmmm]            ","-P 2a 2a 1'a        ",  &
                                               "-x+1/2,y,z,1;x,-y,z,1;x+1/2,y,-z,1;x+1/2,y,z,-1                                                         ")
     Shubnikov_info( 356)= Shub_Spgr_Info_Type("49.273 ","P_cccm      ","47.10.356  ","P_2cm'm'm     ","Pccm.1'_c[Pmmm]            ","-P 2 2c 1'c         ",  &
                                               "-x,y,z+1/2,1;x,-y,z+1/2,1;x,y,-z,1;x,y,z+1/2,-1                                                         ")
     Shubnikov_info( 357)= Shub_Spgr_Info_Type("67.509 ","C_amma      ","47.11.357  ","P_Cmmm'       ","Cmma.1'_a[Pmmm]            ","-C 2b 2 1'a         ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x+1/2,-y,z,1;x+1/2,y,-z,1;x+1/2,y,z,-1                                         ")
     Shubnikov_info( 358)= Shub_Spgr_Info_Type("48.257 ","Pnnn        ","48.1.358   ","Pnnn          ","Pnnn.1                     ","-P 2ab 2bc          ",  &
                                               "-x,y+1/2,z+1/2,1;x+1/2,-y,z+1/2,1;x+1/2,y+1/2,-z,1                                                      ")
     Shubnikov_info( 359)= Shub_Spgr_Info_Type("48.258 ","Pnnn1'      ","48.2.359   ","Pnnn1'        ","Pnnn.1'                    ","-P 2ab 2bc 1'       ",  &
                                               "-x,y+1/2,z+1/2,1;x+1/2,-y,z+1/2,1;x+1/2,y+1/2,-z,1;x,y,z,-1                                             ")
     Shubnikov_info( 360)= Shub_Spgr_Info_Type("48.259 ","Pn'nn       ","48.3.360   ","Pn'nn         ","Pn'nn                      ","P 2ab' 2bc -1'      ",  &
                                               "-x,y+1/2,z+1/2,-1;x+1/2,-y,z+1/2,1;x+1/2,y+1/2,-z,1                                                     ")
     Shubnikov_info( 361)= Shub_Spgr_Info_Type("48.260 ","Pn'n'n      ","48.4.361   ","Pn'n'n        ","Pn'n'n                     ","-P 2ab 2bc'         ",  &
                                               "-x,y+1/2,z+1/2,-1;x+1/2,-y,z+1/2,-1;x+1/2,y+1/2,-z,1                                                    ")
     Shubnikov_info( 362)= Shub_Spgr_Info_Type("48.261 ","Pn'n'n'     ","48.5.362   ","Pn'n'n'       ","Pn'n'n'                    ","P 2ab 2bc -1'       ",  &
                                               "-x,y+1/2,z+1/2,-1;x+1/2,-y,z+1/2,-1;x+1/2,y+1/2,-z,-1                                                   ")
     Shubnikov_info( 363)= Shub_Spgr_Info_Type("70.532 ","F_Sddd      ","48.6.363   ","P_Innn        ","Fddd.1'_I[Pnnn]            ","-F 2uv 2vw 1'n      ",  &
                                               "x+1/2,y+1/2,z,1;x+1/2,y,z+1/2,1;-x,y+1/4,z+1/4,1;x+1/4,-y,z+1/4,1;x+1/4,y+1/4,-z,1;x+1/2,y+1/2,z+1/2,-1 ")
     Shubnikov_info( 364)= Shub_Spgr_Info_Type("49.265 ","Pccm        ","49.1.364   ","Pccm          ","Pccm.1                     ","-P 2 2c             ",  &
                                               "-x,y,z+1/2,1;x,-y,z+1/2,1;x,y,-z,1                                                                      ")
     Shubnikov_info( 365)= Shub_Spgr_Info_Type("49.266 ","Pccm1'      ","49.2.365   ","Pccm1'        ","Pccm.1'                    ","-P 2 2c 1'          ",  &
                                               "-x,y,z+1/2,1;x,-y,z+1/2,1;x,y,-z,1;x,y,z,-1                                                             ")
     Shubnikov_info( 366)= Shub_Spgr_Info_Type("49.267 ","Pc'cm       ","49.3.366   ","Pc'cm         ","Pc'cm                      ","P 2' 2c -1'         ",  &
                                               "-x,y,z+1/2,-1;x,-y,z+1/2,1;x,y,-z,1                                                                     ")
     Shubnikov_info( 367)= Shub_Spgr_Info_Type("49.268 ","Pccm'       ","49.4.367   ","Pccm'         ","Pccm'                      ","P 2 2c' -1'         ",  &
                                               "-x,y,z+1/2,1;x,-y,z+1/2,1;x,y,-z,-1                                                                     ")
     Shubnikov_info( 368)= Shub_Spgr_Info_Type("49.269 ","Pc'c'm      ","49.5.368   ","Pc'c'm        ","Pc'c'm                     ","-P 2 2c'            ",  &
                                               "-x,y,z+1/2,-1;x,-y,z+1/2,-1;x,y,-z,1                                                                    ")
     Shubnikov_info( 369)= Shub_Spgr_Info_Type("49.270 ","Pc'cm'      ","49.6.369   ","Pc'cm'        ","Pc'cm'                     ","-P 2' 2c'           ",  &
                                               "-x,y,z+1/2,-1;x,-y,z+1/2,1;x,y,-z,-1                                                                    ")
     Shubnikov_info( 370)= Shub_Spgr_Info_Type("49.271 ","Pc'c'm'     ","49.7.370   ","Pc'c'm'       ","Pc'c'm'                    ","P 2 2c -1'          ",  &
                                               "-x,y,z+1/2,-1;x,-y,z+1/2,-1;x,y,-z,-1                                                                   ")
     Shubnikov_info( 371)= Shub_Spgr_Info_Type("49.272 ","P_accm      ","49.8.371   ","P_2accm       ","Pccm.1'_a[Pccm]            ","-P 2 2c 1'a         ",  &
                                               "-x,y,z+1/2,1;x,-y,z+1/2,1;x,y,-z,1;x+1/2,y,z,-1                                                         ")
     Shubnikov_info( 372)= Shub_Spgr_Info_Type("66.499 ","C_accm      ","49.9.372   ","P_Cccm        ","Cccm.1'_a[Pccm]            ","-C 2 2c 1'a         ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z+1/2,1;x,-y,z+1/2,1;x,y,-z,1;x+1/2,y,z,-1                                         ")
     Shubnikov_info( 373)= Shub_Spgr_Info_Type("54.346 ","P_acca      ","49.10.373  ","P_2accm'      ","Pcca.1'_a[Pccm]            ","-P 2a 2ac 1'a       ",  &
                                               "-x+1/2,y,z+1/2,1;x,-y,z+1/2,1;x+1/2,y,-z,1;x+1/2,y,z,-1                                                 ")
     Shubnikov_info( 374)= Shub_Spgr_Info_Type("53.332 ","P_cmna      ","49.11.374  ","P_2ac'c'm     ","Pmna.1'_c[Pmaa]            ","-P 2ac 2 1'c        ",  &
                                               "-x,y,z,1;x+1/2,-y,z+1/2,1;x+1/2,y,-z+1/2,1;x,y,z+1/2,-1                                                 ")
     Shubnikov_info( 375)= Shub_Spgr_Info_Type("50.284 ","P_aban      ","49.12.375  ","P_2ac'c'm'    ","Pban.1'_a[Pbmb]            ","-P 2ab 2b 1'a       ",  &
                                               "-x,y+1/2,z,1;x+1/2,-y,z,1;x+1/2,y+1/2,-z,1;x+1/2,y,z,-1                                                 ")
     Shubnikov_info( 376)= Shub_Spgr_Info_Type("68.519 ","C_acca      ","49.13.376  ","P_Cccm'       ","Ccca.1'_a[Pccm]            ","-C 2b 2bc 1'a       ",  &
                                               "x+1/2,y+1/2,z,1;-x+1/2,y,z+1/2,1;x+1/2,-y+1/2,z+1/2,1;x+1/2,y,-z,1;x+1/2,y,z,-1                         ")
     Shubnikov_info( 377)= Shub_Spgr_Info_Type("50.277 ","Pban        ","50.1.377   ","Pban          ","Pban.1                     ","-P 2ab 2b           ",  &
                                               "-x,y+1/2,z,1;x+1/2,-y,z,1;x+1/2,y+1/2,-z,1                                                              ")
     Shubnikov_info( 378)= Shub_Spgr_Info_Type("50.278 ","Pban1'      ","50.2.378   ","Pban1'        ","Pban.1'                    ","-P 2ab 2b 1'        ",  &
                                               "-x,y+1/2,z,1;x+1/2,-y,z,1;x+1/2,y+1/2,-z,1;x,y,z,-1                                                     ")
     Shubnikov_info( 379)= Shub_Spgr_Info_Type("50.279 ","Pb'an       ","50.3.379   ","Pb'an         ","Pb'an                      ","P 2ab' 2b -1'       ",  &
                                               "-x,y+1/2,z,-1;x+1/2,-y,z,1;x+1/2,y+1/2,-z,1                                                             ")
     Shubnikov_info( 380)= Shub_Spgr_Info_Type("50.280 ","Pban'       ","50.4.380   ","Pban'         ","Pban'                      ","P 2ab 2b' -1'       ",  &
                                               "-x,y+1/2,z,1;x+1/2,-y,z,1;x+1/2,y+1/2,-z,-1                                                             ")
     Shubnikov_info( 381)= Shub_Spgr_Info_Type("50.281 ","Pb'a'n      ","50.5.381   ","Pb'a'n        ","Pb'a'n                     ","-P 2ab 2b'          ",  &
                                               "-x,y+1/2,z,-1;x+1/2,-y,z,-1;x+1/2,y+1/2,-z,1                                                            ")
     Shubnikov_info( 382)= Shub_Spgr_Info_Type("50.282 ","Pb'an'      ","50.6.382   ","Pb'an'        ","Pb'an'                     ","-P 2ab' 2b'         ",  &
                                               "-x,y+1/2,z,-1;x+1/2,-y,z,1;x+1/2,y+1/2,-z,-1                                                            ")
     Shubnikov_info( 383)= Shub_Spgr_Info_Type("50.283 ","Pb'a'n'     ","50.7.383   ","Pb'a'n'       ","Pb'a'n'                    ","P 2ab 2b -1'        ",  &
                                               "-x,y+1/2,z,-1;x+1/2,-y,z,-1;x+1/2,y+1/2,-z,-1                                                           ")
     Shubnikov_info( 384)= Shub_Spgr_Info_Type("50.285 ","P_cban      ","50.8.384   ","P_2cban       ","Pban.1'_c[Pban]            ","-P 2ab 2b 1'c       ",  &
                                               "-x,y+1/2,z,1;x+1/2,-y,z,1;x+1/2,y+1/2,-z,1;x,y,z+1/2,-1                                                 ")
     Shubnikov_info( 385)= Shub_Spgr_Info_Type("52.315 ","P_bnna      ","50.9.385   ","P_2cb'an      ","Pnna.1'_b[Pcna]            ","-P 2a 2bc 1'b       ",  &
                                               "-x,y+1/2,z+1/2,1;x+1/2,-y+1/2,z+1/2,1;x+1/2,y,-z,1;x,y+1/2,z,-1                                         ")
     Shubnikov_info( 386)= Shub_Spgr_Info_Type("48.262 ","P_cnnn      ","50.10.386  ","P_2cb'a'n     ","Pnnn.1'_c[Pban]            ","-P 2ab 2bc 1'c      ",  &
                                               "-x,y+1/2,z+1/2,1;x+1/2,-y,z+1/2,1;x+1/2,y+1/2,-z,1;x,y,z+1/2,-1                                         ")
     Shubnikov_info( 387)= Shub_Spgr_Info_Type("51.289 ","Pmma        ","51.1.387   ","Pmma          ","Pmma.1                     ","-P 2a 2a            ",  &
                                               "-x+1/2,y,z,1;x,-y,z,1;x+1/2,y,-z,1                                                                      ")
     Shubnikov_info( 388)= Shub_Spgr_Info_Type("51.290 ","Pmma1'      ","51.2.388   ","Pmma1'        ","Pmma.1'                    ","-P 2a 2a 1'         ",  &
                                               "-x+1/2,y,z,1;x,-y,z,1;x+1/2,y,-z,1;x,y,z,-1                                                             ")
     Shubnikov_info( 389)= Shub_Spgr_Info_Type("51.291 ","Pm'ma       ","51.3.389   ","Pm'ma         ","Pm'ma                      ","P 2a' 2a -1'        ",  &
                                               "-x+1/2,y,z,-1;x,-y,z,1;x+1/2,y,-z,1                                                                     ")
     Shubnikov_info( 390)= Shub_Spgr_Info_Type("51.292 ","Pmm'a       ","51.4.390   ","Pmm'a         ","Pmm'a                      ","P 2a' 2a' -1'       ",  &
                                               "-x+1/2,y,z,1;x,-y,z,-1;x+1/2,y,-z,1                                                                     ")
     Shubnikov_info( 391)= Shub_Spgr_Info_Type("51.293 ","Pmma'       ","51.5.391   ","Pmma'         ","Pmma'                      ","P 2a 2a' -1'        ",  &
                                               "-x+1/2,y,z,1;x,-y,z,1;x+1/2,y,-z,-1                                                                     ")
     Shubnikov_info( 392)= Shub_Spgr_Info_Type("51.294 ","Pm'm'a      ","51.6.392   ","Pm'm'a        ","Pm'm'a                     ","-P 2a 2a'           ",  &
                                               "-x+1/2,y,z,-1;x,-y,z,-1;x+1/2,y,-z,1                                                                    ")
     Shubnikov_info( 393)= Shub_Spgr_Info_Type("51.295 ","Pmm'a'      ","51.7.393   ","Pmm'a'        ","Pmm'a'                     ","-P 2a' 2a           ",  &
                                               "-x+1/2,y,z,1;x,-y,z,-1;x+1/2,y,-z,-1                                                                    ")
     Shubnikov_info( 394)= Shub_Spgr_Info_Type("51.296 ","Pm'ma'      ","51.8.394   ","Pm'ma'        ","Pm'ma'                     ","-P 2a' 2a'          ",  &
                                               "-x+1/2,y,z,-1;x,-y,z,1;x+1/2,y,-z,-1                                                                    ")
     Shubnikov_info( 395)= Shub_Spgr_Info_Type("51.297 ","Pm'm'a'     ","51.9.395   ","Pm'm'a'       ","Pm'm'a'                    ","P 2a 2a -1'         ",  &
                                               "-x+1/2,y,z,-1;x,-y,z,-1;x+1/2,y,-z,-1                                                                   ")
     Shubnikov_info( 396)= Shub_Spgr_Info_Type("51.299 ","P_bmma      ","51.10.396  ","P_2bmma       ","Pmma.1'_b[Pmma]            ","-P 2a 2a 1'b        ",  &
                                               "-x+1/2,y,z,1;x,-y,z,1;x+1/2,y,-z,1;x,y+1/2,z,-1                                                         ")
     Shubnikov_info( 397)= Shub_Spgr_Info_Type("51.300 ","P_cmma      ","51.11.397  ","P_2cmma       ","Pmma.1'_c[Pmma]            ","-P 2a 2a 1'c        ",  &
                                               "-x+1/2,y,z,1;x,-y,z,1;x+1/2,y,-z,1;x,y,z+1/2,-1                                                         ")
     Shubnikov_info( 398)= Shub_Spgr_Info_Type("63.467 ","C_amcm      ","51.12.398  ","P_Amma        ","Cmcm.1'_a[Pmcm]            ","-C 2c 2 1'a         ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x,-y,z+1/2,1;x,y,-z+1/2,1;x+1/2,y,z,-1                                         ")
     Shubnikov_info( 399)= Shub_Spgr_Info_Type("57.388 ","P_cbcm      ","51.13.399  ","P_2bm'ma      ","Pbcm.1'_c[Pbmm]            ","-P 2c 2b 1'c        ",  &
                                               "-x,y+1/2,z,1;x,-y+1/2,z+1/2,1;x,y,-z+1/2,1;x,y,z+1/2,-1                                                 ")
     Shubnikov_info( 400)= Shub_Spgr_Info_Type("59.412 ","P_bmmn      ","51.14.400  ","P_2bmma'      ","Pmmn.1'_b[Pmma]            ","-P 2ab 2a 1'b       ",  &
                                               "-x+1/2,y,z,1;x,-y+1/2,z,1;x+1/2,y+1/2,-z,1;x,y+1/2,z,-1                                                 ")
     Shubnikov_info( 401)= Shub_Spgr_Info_Type("53.330 ","P_amna      ","51.15.401  ","P_2bm'ma'     ","Pmna.1'_a[Pmcm]            ","-P 2ac 2 1'a        ",  &
                                               "-x,y,z,1;x+1/2,-y,z+1/2,1;x+1/2,y,-z+1/2,1;x+1/2,y,z,-1                                                 ")
     Shubnikov_info( 402)= Shub_Spgr_Info_Type("55.360 ","P_abam      ","51.16.402  ","P_2cm'ma      ","Pbam.1'_a[Pbmm]            ","-P 2 2ab 1'a        ",  &
                                               "-x+1/2,y+1/2,z,1;x+1/2,-y+1/2,z,1;x,y,-z,1;x+1/2,y,z,-1                                                 ")
     Shubnikov_info( 403)= Shub_Spgr_Info_Type("57.387 ","P_bbcm      ","51.17.403  ","P_2cmm'a      ","Pbcm.1'_b[Pmcm]            ","-P 2c 2b 1'b        ",  &
                                               "-x,y+1/2,z,1;x,-y+1/2,z+1/2,1;x,y,-z+1/2,1;x,y+1/2,z,-1                                                 ")
     Shubnikov_info( 404)= Shub_Spgr_Info_Type("54.348 ","P_ccca      ","51.18.404  ","P_2cm'm'a     ","Pcca.1'_c[Pmma]            ","-P 2a 2ac 1'c       ",  &
                                               "-x+1/2,y,z+1/2,1;x,-y,z+1/2,1;x+1/2,y,-z,1;x,y,z+1/2,-1                                                 ")
     Shubnikov_info( 405)= Shub_Spgr_Info_Type("64.479 ","C_amca      ","51.19.405  ","P_Am'ma       ","Cmca.1'_a[Pmcm]            ","-C 2bc 2 1'a        ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x+1/2,-y,z+1/2,1;x+1/2,y,-z+1/2,1;x+1/2,y,z,-1                                 ")
     Shubnikov_info( 406)= Shub_Spgr_Info_Type("52.305 ","Pnna        ","52.1.406   ","Pnna          ","Pnna.1                     ","-P 2a 2bc           ",  &
                                               "-x,y+1/2,z+1/2,1;x+1/2,-y+1/2,z+1/2,1;x+1/2,y,-z,1                                                      ")
     Shubnikov_info( 407)= Shub_Spgr_Info_Type("52.306 ","Pnna1'      ","52.2.407   ","Pnna1'        ","Pnna.1'                    ","-P 2a 2bc 1'        ",  &
                                               "-x,y+1/2,z+1/2,1;x+1/2,-y+1/2,z+1/2,1;x+1/2,y,-z,1;x,y,z,-1                                             ")
     Shubnikov_info( 408)= Shub_Spgr_Info_Type("52.307 ","Pn'na       ","52.3.408   ","Pn'na         ","Pn'na                      ","P 2a' 2bc -1'       ",  &
                                               "-x,y+1/2,z+1/2,-1;x+1/2,-y+1/2,z+1/2,1;x+1/2,y,-z,1                                                     ")
     Shubnikov_info( 409)= Shub_Spgr_Info_Type("52.308 ","Pnn'a       ","52.4.409   ","Pnn'a         ","Pnn'a                      ","P 2a' 2bc' -1'      ",  &
                                               "-x,y+1/2,z+1/2,1;x+1/2,-y+1/2,z+1/2,-1;x+1/2,y,-z,1                                                     ")
     Shubnikov_info( 410)= Shub_Spgr_Info_Type("52.309 ","Pnna'       ","52.5.410   ","Pnna'         ","Pnna'                      ","P 2a 2bc' -1'       ",  &
                                               "-x,y+1/2,z+1/2,1;x+1/2,-y+1/2,z+1/2,1;x+1/2,y,-z,-1                                                     ")
     Shubnikov_info( 411)= Shub_Spgr_Info_Type("52.310 ","Pn'n'a      ","52.6.411   ","Pn'n'a        ","Pn'n'a                     ","-P 2a 2bc'          ",  &
                                               "-x,y+1/2,z+1/2,-1;x+1/2,-y+1/2,z+1/2,-1;x+1/2,y,-z,1                                                    ")
     Shubnikov_info( 412)= Shub_Spgr_Info_Type("52.311 ","Pnn'a'      ","52.7.412   ","Pnn'a'        ","Pnn'a'                     ","-P 2a' 2bc          ",  &
                                               "-x,y+1/2,z+1/2,1;x+1/2,-y+1/2,z+1/2,-1;x+1/2,y,-z,-1                                                    ")
     Shubnikov_info( 413)= Shub_Spgr_Info_Type("52.312 ","Pn'na'      ","52.8.413   ","Pn'na'        ","Pn'na'                     ","-P 2a' 2bc'         ",  &
                                               "-x,y+1/2,z+1/2,-1;x+1/2,-y+1/2,z+1/2,1;x+1/2,y,-z,-1                                                    ")
     Shubnikov_info( 414)= Shub_Spgr_Info_Type("52.313 ","Pn'n'a'     ","52.9.414   ","Pn'n'a'       ","Pn'n'a'                    ","P 2a 2bc -1'        ",  &
                                               "-x,y+1/2,z+1/2,-1;x+1/2,-y+1/2,z+1/2,-1;x+1/2,y,-z,-1                                                   ")
     Shubnikov_info( 415)= Shub_Spgr_Info_Type("53.321 ","Pmna        ","53.1.415   ","Pmna          ","Pmna.1                     ","-P 2ac 2            ",  &
                                               "-x,y,z,1;x+1/2,-y,z+1/2,1;x+1/2,y,-z+1/2,1                                                              ")
     Shubnikov_info( 416)= Shub_Spgr_Info_Type("53.322 ","Pmna1'      ","53.2.416   ","Pmna1'        ","Pmna.1'                    ","-P 2ac 2 1'         ",  &
                                               "-x,y,z,1;x+1/2,-y,z+1/2,1;x+1/2,y,-z+1/2,1;x,y,z,-1                                                     ")
     Shubnikov_info( 417)= Shub_Spgr_Info_Type("53.323 ","Pm'na       ","53.3.417   ","Pm'na         ","Pm'na                      ","P 2ac' 2 -1'        ",  &
                                               "-x,y,z,-1;x+1/2,-y,z+1/2,1;x+1/2,y,-z+1/2,1                                                             ")
     Shubnikov_info( 418)= Shub_Spgr_Info_Type("53.324 ","Pmn'a       ","53.4.418   ","Pmn'a         ","Pmn'a                      ","P 2ac' 2' -1'       ",  &
                                               "-x,y,z,1;x+1/2,-y,z+1/2,-1;x+1/2,y,-z+1/2,1                                                             ")
     Shubnikov_info( 419)= Shub_Spgr_Info_Type("53.325 ","Pmna'       ","53.5.419   ","Pmna'         ","Pmna'                      ","P 2ac 2' -1'        ",  &
                                               "-x,y,z,1;x+1/2,-y,z+1/2,1;x+1/2,y,-z+1/2,-1                                                             ")
     Shubnikov_info( 420)= Shub_Spgr_Info_Type("53.326 ","Pm'n'a      ","53.6.420   ","Pm'n'a        ","Pm'n'a                     ","-P 2ac 2'           ",  &
                                               "-x,y,z,-1;x+1/2,-y,z+1/2,-1;x+1/2,y,-z+1/2,1                                                            ")
     Shubnikov_info( 421)= Shub_Spgr_Info_Type("53.327 ","Pmn'a'      ","53.7.421   ","Pmn'a'        ","Pmn'a'                     ","-P 2ac' 2           ",  &
                                               "-x,y,z,1;x+1/2,-y,z+1/2,-1;x+1/2,y,-z+1/2,-1                                                            ")
     Shubnikov_info( 422)= Shub_Spgr_Info_Type("53.328 ","Pm'na'      ","53.8.422   ","Pm'na'        ","Pm'na'                     ","-P 2ac' 2'          ",  &
                                               "-x,y,z,-1;x+1/2,-y,z+1/2,1;x+1/2,y,-z+1/2,-1                                                            ")
     Shubnikov_info( 423)= Shub_Spgr_Info_Type("53.329 ","Pm'n'a'     ","53.9.423   ","Pm'n'a'       ","Pm'n'a'                    ","P 2ac 2 -1'         ",  &
                                               "-x,y,z,-1;x+1/2,-y,z+1/2,-1;x+1/2,y,-z+1/2,-1                                                           ")
     Shubnikov_info( 424)= Shub_Spgr_Info_Type("53.331 ","P_bmna      ","53.10.424  ","P_2bmna       ","Pmna.1'_b[Pmna]            ","-P 2ac 2 1'b        ",  &
                                               "-x,y,z,1;x+1/2,-y,z+1/2,1;x+1/2,y,-z+1/2,1;x,y+1/2,z,-1                                                 ")
     Shubnikov_info( 425)= Shub_Spgr_Info_Type("60.428 ","P_cbcn      ","53.11.425  ","P_2bm'na      ","Pbcn.1'_c[Pbmn]            ","-P 2n 2ab 1'c       ",  &
                                               "-x+1/2,y+1/2,z,1;x,-y,z+1/2,1;x+1/2,y+1/2,-z+1/2,1;x,y,z+1/2,-1                                         ")
     Shubnikov_info( 426)= Shub_Spgr_Info_Type("58.400 ","P_annm      ","53.12.426  ","P_2bmna'      ","Pnnm.1'_a[Pncm]            ","-P 2 2n 1'a         ",  &
                                               "-x+1/2,y+1/2,z+1/2,1;x+1/2,-y+1/2,z+1/2,1;x,y,-z,1;x+1/2,y,z,-1                                         ")
     Shubnikov_info( 427)= Shub_Spgr_Info_Type("52.314 ","P_anna      ","53.13.427  ","P_2bm'na'     ","Pnna.1'_a[Pncm]            ","-P 2a 2bc 1'a       ",  &
                                               "-x,y+1/2,z+1/2,1;x+1/2,-y+1/2,z+1/2,1;x+1/2,y,-z,1;x+1/2,y,z,-1                                         ")
     Shubnikov_info( 428)= Shub_Spgr_Info_Type("54.337 ","Pcca        ","54.1.428   ","Pcca          ","Pcca.1                     ","-P 2a 2ac           ",  &
                                               "-x+1/2,y,z+1/2,1;x,-y,z+1/2,1;x+1/2,y,-z,1                                                              ")
     Shubnikov_info( 429)= Shub_Spgr_Info_Type("54.338 ","Pcca1'      ","54.2.429   ","Pcca1'        ","Pcca.1'                    ","-P 2a 2ac 1'        ",  &
                                               "-x+1/2,y,z+1/2,1;x,-y,z+1/2,1;x+1/2,y,-z,1;x,y,z,-1                                                     ")
     Shubnikov_info( 430)= Shub_Spgr_Info_Type("54.339 ","Pc'ca       ","54.3.430   ","Pc'ca         ","Pc'ca                      ","P 2a' 2ac -1'       ",  &
                                               "-x+1/2,y,z+1/2,-1;x,-y,z+1/2,1;x+1/2,y,-z,1                                                             ")
     Shubnikov_info( 431)= Shub_Spgr_Info_Type("54.340 ","Pcc'a       ","54.4.431   ","Pcc'a         ","Pcc'a                      ","P 2a' 2ac' -1'      ",  &
                                               "-x+1/2,y,z+1/2,1;x,-y,z+1/2,-1;x+1/2,y,-z,1                                                             ")
     Shubnikov_info( 432)= Shub_Spgr_Info_Type("54.341 ","Pcca'       ","54.5.432   ","Pcca'         ","Pcca'                      ","P 2a 2ac' -1'       ",  &
                                               "-x+1/2,y,z+1/2,1;x,-y,z+1/2,1;x+1/2,y,-z,-1                                                             ")
     Shubnikov_info( 433)= Shub_Spgr_Info_Type("54.342 ","Pc'c'a      ","54.6.433   ","Pc'c'a        ","Pc'c'a                     ","-P 2a 2ac'          ",  &
                                               "-x+1/2,y,z+1/2,-1;x,-y,z+1/2,-1;x+1/2,y,-z,1                                                            ")
     Shubnikov_info( 434)= Shub_Spgr_Info_Type("54.343 ","Pcc'a'      ","54.7.434   ","Pcc'a'        ","Pcc'a'                     ","-P 2a' 2ac          ",  &
                                               "-x+1/2,y,z+1/2,1;x,-y,z+1/2,-1;x+1/2,y,-z,-1                                                            ")
     Shubnikov_info( 435)= Shub_Spgr_Info_Type("54.344 ","Pc'ca'      ","54.8.435   ","Pc'ca'        ","Pc'ca'                     ","-P 2a' 2ac'         ",  &
                                               "-x+1/2,y,z+1/2,-1;x,-y,z+1/2,1;x+1/2,y,-z,-1                                                            ")
     Shubnikov_info( 436)= Shub_Spgr_Info_Type("54.345 ","Pc'c'a'     ","54.9.436   ","Pc'c'a'       ","Pc'c'a'                    ","P 2a 2ac -1'        ",  &
                                               "-x+1/2,y,z+1/2,-1;x,-y,z+1/2,-1;x+1/2,y,-z,-1                                                           ")
     Shubnikov_info( 437)= Shub_Spgr_Info_Type("54.347 ","P_bcca      ","54.10.437  ","P_2bcca       ","Pcca.1'_b[Pcca]            ","-P 2a 2ac 1'b       ",  &
                                               "-x+1/2,y,z+1/2,1;x,-y,z+1/2,1;x+1/2,y,-z,1;x,y+1/2,z,-1                                                 ")
     Shubnikov_info( 438)= Shub_Spgr_Info_Type("60.426 ","P_abcn      ","54.11.438  ","P_2bc'ca      ","Pbcn.1'_a[Pbcb]            ","-P 2n 2ab 1'a       ",  &
                                               "-x+1/2,y+1/2,z,1;x,-y,z+1/2,1;x+1/2,y+1/2,-z+1/2,1;x+1/2,y,z,-1                                         ")
     Shubnikov_info( 439)= Shub_Spgr_Info_Type("56.372 ","P_bccn      ","54.12.439  ","P_2bcca'      ","Pccn.1'_b[Pcca]            ","-P 2ab 2ac 1'b      ",  &
                                               "-x+1/2,y,z+1/2,1;x,-y+1/2,z+1/2,1;x+1/2,y+1/2,-z,1;x,y+1/2,z,-1                                         ")
     Shubnikov_info( 440)= Shub_Spgr_Info_Type("52.316 ","P_cnna      ","54.13.440  ","P_2bc'ca'     ","Pnna.1'_c[Pbaa]            ","-P 2a 2bc 1'c       ",  &
                                               "-x,y+1/2,z+1/2,1;x+1/2,-y+1/2,z+1/2,1;x+1/2,y,-z,1;x,y,z+1/2,-1                                         ")
     Shubnikov_info( 441)= Shub_Spgr_Info_Type("55.353 ","Pbam        ","55.1.441   ","Pbam          ","Pbam.1                     ","-P 2 2ab            ",  &
                                               "-x+1/2,y+1/2,z,1;x+1/2,-y+1/2,z,1;x,y,-z,1                                                              ")
     Shubnikov_info( 442)= Shub_Spgr_Info_Type("55.354 ","Pbam1'      ","55.2.442   ","Pbam1'        ","Pbam.1'                    ","-P 2 2ab 1'         ",  &
                                               "-x+1/2,y+1/2,z,1;x+1/2,-y+1/2,z,1;x,y,-z,1;x,y,z,-1                                                     ")
     Shubnikov_info( 443)= Shub_Spgr_Info_Type("55.355 ","Pb'am       ","55.3.443   ","Pb'am         ","Pb'am                      ","P 2' 2ab -1'        ",  &
                                               "-x+1/2,y+1/2,z,-1;x+1/2,-y+1/2,z,1;x,y,-z,1                                                             ")
     Shubnikov_info( 444)= Shub_Spgr_Info_Type("55.356 ","Pbam'       ","55.4.444   ","Pbam'         ","Pbam'                      ","P 2 2ab' -1'        ",  &
                                               "-x+1/2,y+1/2,z,1;x+1/2,-y+1/2,z,1;x,y,-z,-1                                                             ")
     Shubnikov_info( 445)= Shub_Spgr_Info_Type("55.357 ","Pb'a'm      ","55.5.445   ","Pb'a'm        ","Pb'a'm                     ","-P 2 2ab'           ",  &
                                               "-x+1/2,y+1/2,z,-1;x+1/2,-y+1/2,z,-1;x,y,-z,1                                                            ")
     Shubnikov_info( 446)= Shub_Spgr_Info_Type("55.358 ","Pb'am'      ","55.6.446   ","Pb'am'        ","Pb'am'                     ","-P 2' 2ab'          ",  &
                                               "-x+1/2,y+1/2,z,-1;x+1/2,-y+1/2,z,1;x,y,-z,-1                                                            ")
     Shubnikov_info( 447)= Shub_Spgr_Info_Type("55.359 ","Pb'a'm'     ","55.7.447   ","Pb'a'm'       ","Pb'a'm'                    ","P 2 2ab -1'         ",  &
                                               "-x+1/2,y+1/2,z,-1;x+1/2,-y+1/2,z,-1;x,y,-z,-1                                                           ")
     Shubnikov_info( 448)= Shub_Spgr_Info_Type("55.361 ","P_cbam      ","55.8.448   ","P_2cbam       ","Pbam.1'_c[Pbam]            ","-P 2 2ab 1'c        ",  &
                                               "-x+1/2,y+1/2,z,1;x+1/2,-y+1/2,z,1;x,y,-z,1;x,y,z+1/2,-1                                                 ")
     Shubnikov_info( 449)= Shub_Spgr_Info_Type("62.451 ","P_bnma      ","55.9.449   ","P_2cb'am      ","Pnma.1'_b[Pcma]            ","-P 2ac 2n 1'b       ",  &
                                               "-x+1/2,y+1/2,z+1/2,1;x,-y+1/2,z,1;x+1/2,y,-z+1/2,1;x,y+1/2,z,-1                                         ")
     Shubnikov_info( 450)= Shub_Spgr_Info_Type("58.401 ","P_cnnm      ","55.10.450  ","P_2cb'a'm     ","Pnnm.1'_c[Pbam]            ","-P 2 2n 1'c         ",  &
                                               "-x+1/2,y+1/2,z+1/2,1;x+1/2,-y+1/2,z+1/2,1;x,y,-z,1;x,y,z+1/2,-1                                         ")
     Shubnikov_info( 451)= Shub_Spgr_Info_Type("56.365 ","Pccn        ","56.1.451   ","Pccn          ","Pccn.1                     ","-P 2ab 2ac          ",  &
                                               "-x+1/2,y,z+1/2,1;x,-y+1/2,z+1/2,1;x+1/2,y+1/2,-z,1                                                      ")
     Shubnikov_info( 452)= Shub_Spgr_Info_Type("56.366 ","Pccn1'      ","56.2.452   ","Pccn1'        ","Pccn.1'                    ","-P 2ab 2ac 1'       ",  &
                                               "-x+1/2,y,z+1/2,1;x,-y+1/2,z+1/2,1;x+1/2,y+1/2,-z,1;x,y,z,-1                                             ")
     Shubnikov_info( 453)= Shub_Spgr_Info_Type("56.367 ","Pc'cn       ","56.3.453   ","Pc'cn         ","Pc'cn                      ","P 2ab' 2ac -1'      ",  &
                                               "-x+1/2,y,z+1/2,-1;x,-y+1/2,z+1/2,1;x+1/2,y+1/2,-z,1                                                     ")
     Shubnikov_info( 454)= Shub_Spgr_Info_Type("56.368 ","Pccn'       ","56.4.454   ","Pccn'         ","Pccn'                      ","P 2ab 2ac' -1'      ",  &
                                               "-x+1/2,y,z+1/2,1;x,-y+1/2,z+1/2,1;x+1/2,y+1/2,-z,-1                                                     ")
     Shubnikov_info( 455)= Shub_Spgr_Info_Type("56.369 ","Pc'c'n      ","56.5.455   ","Pc'c'n        ","Pc'c'n                     ","-P 2ab 2ac'         ",  &
                                               "-x+1/2,y,z+1/2,-1;x,-y+1/2,z+1/2,-1;x+1/2,y+1/2,-z,1                                                    ")
     Shubnikov_info( 456)= Shub_Spgr_Info_Type("56.370 ","Pc'cn'      ","56.6.456   ","Pc'cn'        ","Pc'cn'                     ","-P 2ab' 2ac'        ",  &
                                               "-x+1/2,y,z+1/2,-1;x,-y+1/2,z+1/2,1;x+1/2,y+1/2,-z,-1                                                    ")
     Shubnikov_info( 457)= Shub_Spgr_Info_Type("56.371 ","Pc'c'n'     ","56.7.457   ","Pc'c'n'       ","Pc'c'n'                    ","P 2ab 2ac -1'       ",  &
                                               "-x+1/2,y,z+1/2,-1;x,-y+1/2,z+1/2,-1;x+1/2,y+1/2,-z,-1                                                   ")
     Shubnikov_info( 458)= Shub_Spgr_Info_Type("57.377 ","Pbcm        ","57.1.458   ","Pbcm          ","Pbcm.1                     ","-P 2c 2b            ",  &
                                               "-x,y+1/2,z,1;x,-y+1/2,z+1/2,1;x,y,-z+1/2,1                                                              ")
     Shubnikov_info( 459)= Shub_Spgr_Info_Type("57.378 ","Pbcm1'      ","57.2.459   ","Pbcm1'        ","Pbcm.1'                    ","-P 2c 2b 1'         ",  &
                                               "-x,y+1/2,z,1;x,-y+1/2,z+1/2,1;x,y,-z+1/2,1;x,y,z,-1                                                     ")
     Shubnikov_info( 460)= Shub_Spgr_Info_Type("57.379 ","Pb'cm       ","57.3.460   ","Pb'cm         ","Pb'cm                      ","P 2c' 2b -1'        ",  &
                                               "-x,y+1/2,z,-1;x,-y+1/2,z+1/2,1;x,y,-z+1/2,1                                                             ")
     Shubnikov_info( 461)= Shub_Spgr_Info_Type("57.380 ","Pbc'm       ","57.4.461   ","Pbc'm         ","Pbc'm                      ","P 2c' 2b' -1'       ",  &
                                               "-x,y+1/2,z,1;x,-y+1/2,z+1/2,-1;x,y,-z+1/2,1                                                             ")
     Shubnikov_info( 462)= Shub_Spgr_Info_Type("57.381 ","Pbcm'       ","57.5.462   ","Pbcm'         ","Pbcm'                      ","P 2c 2b' -1'        ",  &
                                               "-x,y+1/2,z,1;x,-y+1/2,z+1/2,1;x,y,-z+1/2,-1                                                             ")
     Shubnikov_info( 463)= Shub_Spgr_Info_Type("57.382 ","Pb'c'm      ","57.6.463   ","Pb'c'm        ","Pb'c'm                     ","-P 2c 2b'           ",  &
                                               "-x,y+1/2,z,-1;x,-y+1/2,z+1/2,-1;x,y,-z+1/2,1                                                            ")
     Shubnikov_info( 464)= Shub_Spgr_Info_Type("57.383 ","Pbc'm'      ","57.7.464   ","Pbc'm'        ","Pbc'm'                     ","-P 2c' 2b           ",  &
                                               "-x,y+1/2,z,1;x,-y+1/2,z+1/2,-1;x,y,-z+1/2,-1                                                            ")
     Shubnikov_info( 465)= Shub_Spgr_Info_Type("57.384 ","Pb'cm'      ","57.8.465   ","Pb'cm'        ","Pb'cm'                     ","-P 2c' 2b'          ",  &
                                               "-x,y+1/2,z,-1;x,-y+1/2,z+1/2,1;x,y,-z+1/2,-1                                                            ")
     Shubnikov_info( 466)= Shub_Spgr_Info_Type("57.385 ","Pb'c'm'     ","57.9.466   ","Pb'c'm'       ","Pb'c'm'                    ","P 2c 2b -1'         ",  &
                                               "-x,y+1/2,z,-1;x,-y+1/2,z+1/2,-1;x,y,-z+1/2,-1                                                           ")
     Shubnikov_info( 467)= Shub_Spgr_Info_Type("57.386 ","P_abcm      ","57.10.467  ","P_2abcm       ","Pbcm.1'_a[Pbcm]            ","-P 2c 2b 1'a        ",  &
                                               "-x,y+1/2,z,1;x,-y+1/2,z+1/2,1;x,y,-z+1/2,1;x+1/2,y,z,-1                                                 ")
     Shubnikov_info( 468)= Shub_Spgr_Info_Type("62.452 ","P_cnma      ","57.11.468  ","P_2abc'm      ","Pnma.1'_c[Pbma]            ","-P 2ac 2n 1'c       ",  &
                                               "-x+1/2,y+1/2,z+1/2,1;x,-y+1/2,z,1;x+1/2,y,-z+1/2,1;x,y,z+1/2,-1                                         ")
     Shubnikov_info( 469)= Shub_Spgr_Info_Type("61.438 ","P_abca      ","57.12.469  ","P_2abcm'      ","Pbca.1'_a[Pbcm]            ","-P 2ac 2ab 1'a      ",  &
                                               "-x+1/2,y+1/2,z,1;x,-y+1/2,z+1/2,1;x+1/2,y,-z+1/2,1;x+1/2,y,z,-1                                         ")
     Shubnikov_info( 470)= Shub_Spgr_Info_Type("60.427 ","P_bbcn      ","57.13.470  ","P_2abc'm'     ","Pbcn.1'_b[Pmca]            ","-P 2n 2ab 1'b       ",  &
                                               "-x+1/2,y+1/2,z,1;x,-y,z+1/2,1;x+1/2,y+1/2,-z+1/2,1;x,y+1/2,z,-1                                         ")
     Shubnikov_info( 471)= Shub_Spgr_Info_Type("58.393 ","Pnnm        ","58.1.471   ","Pnnm          ","Pnnm.1                     ","-P 2 2n             ",  &
                                               "-x+1/2,y+1/2,z+1/2,1;x+1/2,-y+1/2,z+1/2,1;x,y,-z,1                                                      ")
     Shubnikov_info( 472)= Shub_Spgr_Info_Type("58.394 ","Pnnm1'      ","58.2.472   ","Pnnm1'        ","Pnnm.1'                    ","-P 2 2n 1'          ",  &
                                               "-x+1/2,y+1/2,z+1/2,1;x+1/2,-y+1/2,z+1/2,1;x,y,-z,1;x,y,z,-1                                             ")
     Shubnikov_info( 473)= Shub_Spgr_Info_Type("58.395 ","Pn'nm       ","58.3.473   ","Pn'nm         ","Pn'nm                      ","P 2' 2n -1'         ",  &
                                               "-x+1/2,y+1/2,z+1/2,-1;x+1/2,-y+1/2,z+1/2,1;x,y,-z,1                                                     ")
     Shubnikov_info( 474)= Shub_Spgr_Info_Type("58.396 ","Pnnm'       ","58.4.474   ","Pnnm'         ","Pnnm'                      ","P 2 2n' -1'         ",  &
                                               "-x+1/2,y+1/2,z+1/2,1;x+1/2,-y+1/2,z+1/2,1;x,y,-z,-1                                                     ")
     Shubnikov_info( 475)= Shub_Spgr_Info_Type("58.397 ","Pn'n'm      ","58.5.475   ","Pn'n'm        ","Pn'n'm                     ","-P 2 2n'            ",  &
                                               "-x+1/2,y+1/2,z+1/2,-1;x+1/2,-y+1/2,z+1/2,-1;x,y,-z,1                                                    ")
     Shubnikov_info( 476)= Shub_Spgr_Info_Type("58.398 ","Pnn'm'      ","58.6.476   ","Pnn'm'        ","Pnn'm'                     ","-P 2' 2n            ",  &
                                               "-x+1/2,y+1/2,z+1/2,1;x+1/2,-y+1/2,z+1/2,-1;x,y,-z,-1                                                    ")
     Shubnikov_info( 477)= Shub_Spgr_Info_Type("58.399 ","Pn'n'm'     ","58.7.477   ","Pn'n'm'       ","Pn'n'm'                    ","P 2 2n -1'          ",  &
                                               "-x+1/2,y+1/2,z+1/2,-1;x+1/2,-y+1/2,z+1/2,-1;x,y,-z,-1                                                   ")
     Shubnikov_info( 478)= Shub_Spgr_Info_Type("59.405 ","Pmmn        ","59.1.478   ","Pmmn          ","Pmmn.1                     ","-P 2ab 2a           ",  &
                                               "-x+1/2,y,z,1;x,-y+1/2,z,1;x+1/2,y+1/2,-z,1                                                              ")
     Shubnikov_info( 479)= Shub_Spgr_Info_Type("59.406 ","Pmmn1'      ","59.2.479   ","Pmmn1'        ","Pmmn.1'                    ","-P 2ab 2a 1'        ",  &
                                               "-x+1/2,y,z,1;x,-y+1/2,z,1;x+1/2,y+1/2,-z,1;x,y,z,-1                                                     ")
     Shubnikov_info( 480)= Shub_Spgr_Info_Type("59.407 ","Pm'mn       ","59.3.480   ","Pm'mn         ","Pm'mn                      ","P 2ab' 2a -1'       ",  &
                                               "-x+1/2,y,z,-1;x,-y+1/2,z,1;x+1/2,y+1/2,-z,1                                                             ")
     Shubnikov_info( 481)= Shub_Spgr_Info_Type("59.408 ","Pmmn'       ","59.4.481   ","Pmmn'         ","Pmmn'                      ","P 2ab 2a' -1'       ",  &
                                               "-x+1/2,y,z,1;x,-y+1/2,z,1;x+1/2,y+1/2,-z,-1                                                             ")
     Shubnikov_info( 482)= Shub_Spgr_Info_Type("59.409 ","Pm'm'n      ","59.5.482   ","Pm'm'n        ","Pm'm'n                     ","-P 2ab 2a'          ",  &
                                               "-x+1/2,y,z,-1;x,-y+1/2,z,-1;x+1/2,y+1/2,-z,1                                                            ")
     Shubnikov_info( 483)= Shub_Spgr_Info_Type("59.410 ","Pmm'n'      ","59.6.483   ","Pmm'n'        ","Pmm'n'                     ","-P 2ab' 2a          ",  &
                                               "-x+1/2,y,z,1;x,-y+1/2,z,-1;x+1/2,y+1/2,-z,-1                                                            ")
     Shubnikov_info( 484)= Shub_Spgr_Info_Type("59.411 ","Pm'm'n'     ","59.7.484   ","Pm'm'n'       ","Pm'm'n'                    ","P 2ab 2a -1'        ",  &
                                               "-x+1/2,y,z,-1;x,-y+1/2,z,-1;x+1/2,y+1/2,-z,-1                                                           ")
     Shubnikov_info( 485)= Shub_Spgr_Info_Type("59.413 ","P_cmmn      ","59.8.485   ","P_2cmmn       ","Pmmn.1'_c[Pmmn]            ","-P 2ab 2a 1'c       ",  &
                                               "-x+1/2,y,z,1;x,-y+1/2,z,1;x+1/2,y+1/2,-z,1;x,y,z+1/2,-1                                                 ")
     Shubnikov_info( 486)= Shub_Spgr_Info_Type("62.450 ","P_anma      ","59.9.486   ","P_2cm'mn      ","Pnma.1'_a[Pnmm]            ","-P 2ac 2n 1'a       ",  &
                                               "-x+1/2,y+1/2,z+1/2,1;x,-y+1/2,z,1;x+1/2,y,-z+1/2,1;x+1/2,y,z,-1                                         ")
     Shubnikov_info( 487)= Shub_Spgr_Info_Type("56.373 ","P_cccn      ","59.10.487  ","P_2cm'm'n     ","Pccn.1'_c[Pmmn]            ","-P 2ab 2ac 1'c      ",  &
                                               "-x+1/2,y,z+1/2,1;x,-y+1/2,z+1/2,1;x+1/2,y+1/2,-z,1;x,y,z+1/2,-1                                         ")
     Shubnikov_info( 488)= Shub_Spgr_Info_Type("60.417 ","Pbcn        ","60.1.488   ","Pbcn          ","Pbcn.1                     ","-P 2n 2ab           ",  &
                                               "-x+1/2,y+1/2,z,1;x,-y,z+1/2,1;x+1/2,y+1/2,-z+1/2,1                                                      ")
     Shubnikov_info( 489)= Shub_Spgr_Info_Type("60.418 ","Pbcn1'      ","60.2.489   ","Pbcn1'        ","Pbcn.1'                    ","-P 2n 2ab 1'        ",  &
                                               "-x+1/2,y+1/2,z,1;x,-y,z+1/2,1;x+1/2,y+1/2,-z+1/2,1;x,y,z,-1                                             ")
     Shubnikov_info( 490)= Shub_Spgr_Info_Type("60.419 ","Pb'cn       ","60.3.490   ","Pb'cn         ","Pb'cn                      ","P 2n' 2ab -1'       ",  &
                                               "-x+1/2,y+1/2,z,-1;x,-y,z+1/2,1;x+1/2,y+1/2,-z+1/2,1                                                     ")
     Shubnikov_info( 491)= Shub_Spgr_Info_Type("60.420 ","Pbc'n       ","60.4.491   ","Pbc'n         ","Pbc'n                      ","P 2n' 2ab' -1'      ",  &
                                               "-x+1/2,y+1/2,z,1;x,-y,z+1/2,-1;x+1/2,y+1/2,-z+1/2,1                                                     ")
     Shubnikov_info( 492)= Shub_Spgr_Info_Type("60.421 ","Pbcn'       ","60.5.492   ","Pbcn'         ","Pbcn'                      ","P 2n 2ab' -1'       ",  &
                                               "-x+1/2,y+1/2,z,1;x,-y,z+1/2,1;x+1/2,y+1/2,-z+1/2,-1                                                     ")
     Shubnikov_info( 493)= Shub_Spgr_Info_Type("60.422 ","Pb'c'n      ","60.6.493   ","Pb'c'n        ","Pb'c'n                     ","-P 2n 2ab'          ",  &
                                               "-x+1/2,y+1/2,z,-1;x,-y,z+1/2,-1;x+1/2,y+1/2,-z+1/2,1                                                    ")
     Shubnikov_info( 494)= Shub_Spgr_Info_Type("60.423 ","Pbc'n'      ","60.7.494   ","Pbc'n'        ","Pbc'n'                     ","-P 2n' 2ab          ",  &
                                               "-x+1/2,y+1/2,z,1;x,-y,z+1/2,-1;x+1/2,y+1/2,-z+1/2,-1                                                    ")
     Shubnikov_info( 495)= Shub_Spgr_Info_Type("60.424 ","Pb'cn'      ","60.8.495   ","Pb'cn'        ","Pb'cn'                     ","-P 2n' 2ab'         ",  &
                                               "-x+1/2,y+1/2,z,-1;x,-y,z+1/2,1;x+1/2,y+1/2,-z+1/2,-1                                                    ")
     Shubnikov_info( 496)= Shub_Spgr_Info_Type("60.425 ","Pb'c'n'     ","60.9.496   ","Pb'c'n'       ","Pb'c'n'                    ","P 2n 2ab -1'        ",  &
                                               "-x+1/2,y+1/2,z,-1;x,-y,z+1/2,-1;x+1/2,y+1/2,-z+1/2,-1                                                   ")
     Shubnikov_info( 497)= Shub_Spgr_Info_Type("61.433 ","Pbca        ","61.1.497   ","Pbca          ","Pbca.1                     ","-P 2ac 2ab          ",  &
                                               "-x+1/2,y+1/2,z,1;x,-y+1/2,z+1/2,1;x+1/2,y,-z+1/2,1                                                      ")
     Shubnikov_info( 498)= Shub_Spgr_Info_Type("61.434 ","Pbca1'      ","61.2.498   ","Pbca1'        ","Pbca.1'                    ","-P 2ac 2ab 1'       ",  &
                                               "-x+1/2,y+1/2,z,1;x,-y+1/2,z+1/2,1;x+1/2,y,-z+1/2,1;x,y,z,-1                                             ")
     Shubnikov_info( 499)= Shub_Spgr_Info_Type("61.435 ","Pb'ca       ","61.3.499   ","Pb'ca         ","Pb'ca                      ","P 2ac' 2ab -1'      ",  &
                                               "-x+1/2,y+1/2,z,-1;x,-y+1/2,z+1/2,1;x+1/2,y,-z+1/2,1                                                     ")
     Shubnikov_info( 500)= Shub_Spgr_Info_Type("61.436 ","Pb'c'a      ","61.4.500   ","Pb'c'a        ","Pb'c'a                     ","-P 2ac 2ab'         ",  &
                                               "-x+1/2,y+1/2,z,-1;x,-y+1/2,z+1/2,-1;x+1/2,y,-z+1/2,1                                                    ")
     Shubnikov_info( 501)= Shub_Spgr_Info_Type("61.437 ","Pb'c'a'     ","61.5.501   ","Pb'c'a'       ","Pb'c'a'                    ","P 2ac 2ab -1'       ",  &
                                               "-x+1/2,y+1/2,z,-1;x,-y+1/2,z+1/2,-1;x+1/2,y,-z+1/2,-1                                                   ")
     Shubnikov_info( 502)= Shub_Spgr_Info_Type("62.441 ","Pnma        ","62.1.502   ","Pnma          ","Pnma.1                     ","-P 2ac 2n           ",  &
                                               "-x+1/2,y+1/2,z+1/2,1;x,-y+1/2,z,1;x+1/2,y,-z+1/2,1                                                      ")
     Shubnikov_info( 503)= Shub_Spgr_Info_Type("62.442 ","Pnma1'      ","62.2.503   ","Pnma1'        ","Pnma.1'                    ","-P 2ac 2n 1'        ",  &
                                               "-x+1/2,y+1/2,z+1/2,1;x,-y+1/2,z,1;x+1/2,y,-z+1/2,1;x,y,z,-1                                             ")
     Shubnikov_info( 504)= Shub_Spgr_Info_Type("62.443 ","Pn'ma       ","62.3.504   ","Pn'ma         ","Pn'ma                      ","P 2ac' 2n -1'       ",  &
                                               "-x+1/2,y+1/2,z+1/2,-1;x,-y+1/2,z,1;x+1/2,y,-z+1/2,1                                                     ")
     Shubnikov_info( 505)= Shub_Spgr_Info_Type("62.444 ","Pnm'a       ","62.4.505   ","Pnm'a         ","Pnm'a                      ","P 2ac' 2n' -1'      ",  &
                                               "-x+1/2,y+1/2,z+1/2,1;x,-y+1/2,z,-1;x+1/2,y,-z+1/2,1                                                     ")
     Shubnikov_info( 506)= Shub_Spgr_Info_Type("62.445 ","Pnma'       ","62.5.506   ","Pnma'         ","Pnma'                      ","P 2ac 2n' -1'       ",  &
                                               "-x+1/2,y+1/2,z+1/2,1;x,-y+1/2,z,1;x+1/2,y,-z+1/2,-1                                                     ")
     Shubnikov_info( 507)= Shub_Spgr_Info_Type("62.446 ","Pn'm'a      ","62.6.507   ","Pn'm'a        ","Pn'm'a                     ","-P 2ac 2n'          ",  &
                                               "-x+1/2,y+1/2,z+1/2,-1;x,-y+1/2,z,-1;x+1/2,y,-z+1/2,1                                                    ")
     Shubnikov_info( 508)= Shub_Spgr_Info_Type("62.447 ","Pnm'a'      ","62.7.508   ","Pnm'a'        ","Pnm'a'                     ","-P 2ac' 2n          ",  &
                                               "-x+1/2,y+1/2,z+1/2,1;x,-y+1/2,z,-1;x+1/2,y,-z+1/2,-1                                                    ")
     Shubnikov_info( 509)= Shub_Spgr_Info_Type("62.448 ","Pn'ma'      ","62.8.509   ","Pn'ma'        ","Pn'ma'                     ","-P 2ac' 2n'         ",  &
                                               "-x+1/2,y+1/2,z+1/2,-1;x,-y+1/2,z,1;x+1/2,y,-z+1/2,-1                                                    ")
     Shubnikov_info( 510)= Shub_Spgr_Info_Type("62.449 ","Pn'm'a'     ","62.9.510   ","Pn'm'a'       ","Pn'm'a'                    ","P 2ac 2n -1'        ",  &
                                               "-x+1/2,y+1/2,z+1/2,-1;x,-y+1/2,z,-1;x+1/2,y,-z+1/2,-1                                                   ")
     Shubnikov_info( 511)= Shub_Spgr_Info_Type("63.457 ","Cmcm        ","63.1.511   ","Cmcm          ","Cmcm.1                     ","-C 2c 2             ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x,-y,z+1/2,1;x,y,-z+1/2,1                                                      ")
     Shubnikov_info( 512)= Shub_Spgr_Info_Type("63.458 ","Cmcm1'      ","63.2.512   ","Cmcm1'        ","Cmcm.1'                    ","-C 2c 2 1'          ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x,-y,z+1/2,1;x,y,-z+1/2,1;x,y,z,-1                                             ")
     Shubnikov_info( 513)= Shub_Spgr_Info_Type("63.459 ","Cm'cm       ","63.3.513   ","Cm'cm         ","Cm'cm                      ","C 2c' 2 -1'         ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,-1;x,-y,z+1/2,1;x,y,-z+1/2,1                                                     ")
     Shubnikov_info( 514)= Shub_Spgr_Info_Type("63.460 ","Cmc'm       ","63.4.514   ","Cmc'm         ","Cmc'm                      ","C 2c' 2' -1'        ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x,-y,z+1/2,-1;x,y,-z+1/2,1                                                     ")
     Shubnikov_info( 515)= Shub_Spgr_Info_Type("63.461 ","Cmcm'       ","63.5.515   ","Cmcm'         ","Cmcm'                      ","C 2c 2' -1'         ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x,-y,z+1/2,1;x,y,-z+1/2,-1                                                     ")
     Shubnikov_info( 516)= Shub_Spgr_Info_Type("63.462 ","Cm'c'm      ","63.6.516   ","Cm'c'm        ","Cm'c'm                     ","-C 2c 2'            ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,-1;x,-y,z+1/2,-1;x,y,-z+1/2,1                                                    ")
     Shubnikov_info( 517)= Shub_Spgr_Info_Type("63.463 ","Cmc'm'      ","63.7.517   ","Cmc'm'        ","Cmc'm'                     ","-C 2c' 2            ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x,-y,z+1/2,-1;x,y,-z+1/2,-1                                                    ")
     Shubnikov_info( 518)= Shub_Spgr_Info_Type("63.464 ","Cm'cm'      ","63.8.518   ","Cm'cm'        ","Cm'cm'                     ","-C 2c' 2'           ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,-1;x,-y,z+1/2,1;x,y,-z+1/2,-1                                                    ")
     Shubnikov_info( 519)= Shub_Spgr_Info_Type("63.465 ","Cm'c'm'     ","63.9.519   ","Cm'c'm'       ","Cm'c'm'                    ","C 2c 2 -1'          ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,-1;x,-y,z+1/2,-1;x,y,-z+1/2,-1                                                   ")
     Shubnikov_info( 520)= Shub_Spgr_Info_Type("51.301 ","P_Amma      ","63.10.520  ","C_Pmcm        ","Pmma.1'_A[Amma]            ","-P 2a 2a 1'A        ",  &
                                               "-x+1/2,y,z,1;x,-y,z,1;x+1/2,y,-z,1;x,y+1/2,z+1/2,-1                                                     ")
     Shubnikov_info( 521)= Shub_Spgr_Info_Type("57.391 ","P_Cbcm      ","63.11.521  ","C_Pm'cm       ","Pbcm.1'_C[Cmcm]            ","-P 2c 2b 1'C        ",  &
                                               "-x,y+1/2,z,1;x,-y+1/2,z+1/2,1;x,y,-z+1/2,1;x+1/2,y+1/2,z,-1                                             ")
     Shubnikov_info( 522)= Shub_Spgr_Info_Type("59.414 ","P_Bmmn      ","63.12.522  ","C_Pmc'm       ","Pmmn.1'_B[Bmmb]            ","-P 2ab 2a 1'B       ",  &
                                               "-x+1/2,y,z,1;x,-y+1/2,z,1;x+1/2,y+1/2,-z,1;x+1/2,y,z+1/2,-1                                             ")
     Shubnikov_info( 523)= Shub_Spgr_Info_Type("62.453 ","P_Anma      ","63.13.523  ","C_Pmcm'       ","Pnma.1'_A[Amma]            ","-P 2ac 2n 1'A       ",  &
                                               "-x+1/2,y+1/2,z+1/2,1;x,-y+1/2,z,1;x+1/2,y,-z+1/2,1;x,y+1/2,z+1/2,-1                                     ")
     Shubnikov_info( 524)= Shub_Spgr_Info_Type("62.454 ","P_Bnma      ","63.14.524  ","C_Pm'c'm      ","Pnma.1'_B[Bbmm]            ","-P 2ac 2n 1'B       ",  &
                                               "-x+1/2,y+1/2,z+1/2,1;x,-y+1/2,z,1;x+1/2,y,-z+1/2,1;x+1/2,y,z+1/2,-1                                     ")
     Shubnikov_info( 525)= Shub_Spgr_Info_Type("58.402 ","P_Bnnm      ","63.15.525  ","C_Pmc'm'      ","Pnnm.1'_B[Bbmm]            ","-P 2 2n 1'B         ",  &
                                               "-x+1/2,y+1/2,z+1/2,1;x+1/2,-y+1/2,z+1/2,1;x,y,-z,1;x+1/2,y,z+1/2,-1                                     ")
     Shubnikov_info( 526)= Shub_Spgr_Info_Type("60.431 ","P_Cbcn      ","63.16.526  ","C_Pm'cm'      ","Pbcn.1'_C[Cmcm]            ","-P 2n 2ab 1'C       ",  &
                                               "-x+1/2,y+1/2,z,1;x,-y,z+1/2,1;x+1/2,y+1/2,-z+1/2,1;x+1/2,y+1/2,z,-1                                     ")
     Shubnikov_info( 527)= Shub_Spgr_Info_Type("52.318 ","P_Bnna      ","63.17.527  ","C_Pm'c'm'     ","Pnna.1'_B[Bbmm]            ","-P 2a 2bc 1'B       ",  &
                                               "-x,y+1/2,z+1/2,1;x+1/2,-y+1/2,z+1/2,1;x+1/2,y,-z,1;x+1/2,y,z+1/2,-1                                     ")
     Shubnikov_info( 528)= Shub_Spgr_Info_Type("64.469 ","Cmca        ","64.1.528   ","Cmca          ","Cmca.1                     ","-C 2bc 2            ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x+1/2,-y,z+1/2,1;x+1/2,y,-z+1/2,1                                              ")
     Shubnikov_info( 529)= Shub_Spgr_Info_Type("64.470 ","Cmca1'      ","64.2.529   ","Cmca1'        ","Cmca.1'                    ","-C 2bc 2 1'         ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x+1/2,-y,z+1/2,1;x+1/2,y,-z+1/2,1;x,y,z,-1                                     ")
     Shubnikov_info( 530)= Shub_Spgr_Info_Type("64.471 ","Cm'ca       ","64.3.530   ","Cm'ca         ","Cm'ca                      ","C 2bc' 2 -1'        ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,-1;x+1/2,-y,z+1/2,1;x+1/2,y,-z+1/2,1                                             ")
     Shubnikov_info( 531)= Shub_Spgr_Info_Type("64.472 ","Cmc'a       ","64.4.531   ","Cmc'a         ","Cmc'a                      ","C 2bc' 2' -1'       ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x+1/2,-y,z+1/2,-1;x+1/2,y,-z+1/2,1                                             ")
     Shubnikov_info( 532)= Shub_Spgr_Info_Type("64.473 ","Cmca'       ","64.5.532   ","Cmca'         ","Cmca'                      ","C 2bc 2' -1'        ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x+1/2,-y,z+1/2,1;x+1/2,y,-z+1/2,-1                                             ")
     Shubnikov_info( 533)= Shub_Spgr_Info_Type("64.474 ","Cm'c'a      ","64.6.533   ","Cm'c'a        ","Cm'c'a                     ","-C 2bc 2'           ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,-1;x+1/2,-y,z+1/2,-1;x+1/2,y,-z+1/2,1                                            ")
     Shubnikov_info( 534)= Shub_Spgr_Info_Type("64.475 ","Cmc'a'      ","64.7.534   ","Cmc'a'        ","Cmc'a'                     ","-C 2bc' 2           ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x+1/2,-y,z+1/2,-1;x+1/2,y,-z+1/2,-1                                            ")
     Shubnikov_info( 535)= Shub_Spgr_Info_Type("64.476 ","Cm'ca'      ","64.8.535   ","Cm'ca'        ","Cm'ca'                     ","-C 2bc' 2'          ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,-1;x+1/2,-y,z+1/2,1;x+1/2,y,-z+1/2,-1                                            ")
     Shubnikov_info( 536)= Shub_Spgr_Info_Type("64.477 ","Cm'c'a'     ","64.9.536   ","Cm'c'a'       ","Cm'c'a'                    ","C 2bc 2 -1'         ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,-1;x+1/2,-y,z+1/2,-1;x+1/2,y,-z+1/2,-1                                           ")
     Shubnikov_info( 537)= Shub_Spgr_Info_Type("55.362 ","P_Abam      ","64.10.537  ","C_Pmca        ","Pbam.1'_A[Acam]            ","-P 2 2ab 1'A        ",  &
                                               "-x+1/2,y+1/2,z,1;x+1/2,-y+1/2,z,1;x,y,-z,1;x,y+1/2,z+1/2,-1                                             ")
     Shubnikov_info( 538)= Shub_Spgr_Info_Type("54.349 ","P_Acca      ","64.11.538  ","C_Pm'ca       ","Pcca.1'_A[Abma]            ","-P 2a 2ac 1'A       ",  &
                                               "-x+1/2,y,z+1/2,1;x,-y,z+1/2,1;x+1/2,y,-z,1;x,y+1/2,z+1/2,-1                                             ")
     Shubnikov_info( 539)= Shub_Spgr_Info_Type("62.455 ","P_Cnma      ","64.12.539  ","C_Pmc'a       ","Pnma.1'_C[Ccmb]            ","-P 2ac 2n 1'C       ",  &
                                               "-x+1/2,y+1/2,z+1/2,1;x,-y+1/2,z,1;x+1/2,y,-z+1/2,1;x+1/2,y+1/2,z,-1                                     ")
     Shubnikov_info( 540)= Shub_Spgr_Info_Type("57.390 ","P_Bbcm      ","64.13.540  ","C_Pmca'       ","Pbcm.1'_B[Bbcm]            ","-P 2c 2b 1'B        ",  &
                                               "-x,y+1/2,z,1;x,-y+1/2,z+1/2,1;x,y,-z+1/2,1;x+1/2,y,z+1/2,-1                                             ")
     Shubnikov_info( 541)= Shub_Spgr_Info_Type("56.374 ","P_Accn      ","64.14.541  ","C_Pm'c'a      ","Pccn.1'_A[Abma]            ","-P 2ab 2ac 1'A      ",  &
                                               "-x+1/2,y,z+1/2,1;x,-y+1/2,z+1/2,1;x+1/2,y+1/2,-z,1;x,y+1/2,z+1/2,-1                                     ")
     Shubnikov_info( 542)= Shub_Spgr_Info_Type("53.335 ","P_Cmna      ","64.15.542  ","C_Pmc'a'      ","Pmna.1'_C[Cmca]            ","-P 2ac 2 1'C        ",  &
                                               "-x,y,z,1;x+1/2,-y,z+1/2,1;x+1/2,y,-z+1/2,1;x+1/2,y+1/2,z,-1                                             ")
     Shubnikov_info( 543)= Shub_Spgr_Info_Type("61.439 ","P_Cbca      ","64.16.543  ","C_Pm'ca'      ","Pbca.1'_C[Cmca]            ","-P 2ac 2ab 1'C      ",  &
                                               "-x+1/2,y+1/2,z,1;x,-y+1/2,z+1/2,1;x+1/2,y,-z+1/2,1;x+1/2,y+1/2,z,-1                                     ")
     Shubnikov_info( 544)= Shub_Spgr_Info_Type("60.429 ","P_Abcn      ","64.17.544  ","C_Pm'c'a'     ","Pbcn.1'_A[Abma]            ","-P 2n 2ab 1'A       ",  &
                                               "-x+1/2,y+1/2,z,1;x,-y,z+1/2,1;x+1/2,y+1/2,-z+1/2,1;x,y+1/2,z+1/2,-1                                     ")
     Shubnikov_info( 545)= Shub_Spgr_Info_Type("65.481 ","Cmmm        ","65.1.545   ","Cmmm          ","Cmmm.1                     ","-C 2 2              ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x,-y,z,1;x,y,-z,1                                                              ")
     Shubnikov_info( 546)= Shub_Spgr_Info_Type("65.482 ","Cmmm1'      ","65.2.546   ","Cmmm1'        ","Cmmm.1'                    ","-C 2 2 1'           ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x,-y,z,1;x,y,-z,1;x,y,z,-1                                                     ")
     Shubnikov_info( 547)= Shub_Spgr_Info_Type("65.483 ","Cm'mm       ","65.3.547   ","Cm'mm         ","Cm'mm                      ","C 2' 2 -1'          ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,-1;x,-y,z,1;x,y,-z,1                                                             ")
     Shubnikov_info( 548)= Shub_Spgr_Info_Type("65.484 ","Cmmm'       ","65.4.548   ","Cmmm'         ","Cmmm'                      ","C 2 2' -1'          ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x,-y,z,1;x,y,-z,-1                                                             ")
     Shubnikov_info( 549)= Shub_Spgr_Info_Type("65.485 ","Cm'm'm      ","65.5.549   ","Cm'm'm        ","Cm'm'm                     ","-C 2 2'             ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,-1;x,-y,z,-1;x,y,-z,1                                                            ")
     Shubnikov_info( 550)= Shub_Spgr_Info_Type("65.486 ","Cmm'm'      ","65.6.550   ","Cmm'm'        ","Cmm'm'                     ","-C 2' 2             ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x,-y,z,-1;x,y,-z,-1                                                            ")
     Shubnikov_info( 551)= Shub_Spgr_Info_Type("65.487 ","Cm'm'm'     ","65.7.551   ","Cm'm'm'       ","Cm'm'm'                    ","C 2 2 -1'           ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,-1;x,-y,z,-1;x,y,-z,-1                                                           ")
     Shubnikov_info( 552)= Shub_Spgr_Info_Type("65.488 ","C_cmmm      ","65.8.552   ","C_2cmmm       ","Cmmm.1'_c[Cmmm]            ","-C 2 2 1'c          ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x,-y,z,1;x,y,-z,1;x,y,z+1/2,-1                                                 ")
     Shubnikov_info( 553)= Shub_Spgr_Info_Type("47.255 ","P_Cmmm      ","65.9.553   ","C_Pmmm        ","Pmmm.1'_C[Cmmm]            ","-P 2 2 1'C          ",  &
                                               "-x,y,z,1;x,-y,z,1;x,y,-z,1;x+1/2,y+1/2,z,-1                                                             ")
     Shubnikov_info( 554)= Shub_Spgr_Info_Type("71.538 ","I_cmmm      ","65.10.554  ","C_Immm        ","Immm.1'_c[Cmmm]            ","-I 2 2 1'c          ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z,1;x,-y,z,1;x,y,-z,1;x,y,z+1/2,-1                                             ")
     Shubnikov_info( 555)= Shub_Spgr_Info_Type("66.498 ","C_cccm      ","65.11.555  ","C_2cm'm'm     ","Cccm.1'_c[Cmmm]            ","-C 2 2c 1'c         ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z+1/2,1;x,-y,z+1/2,1;x,y,-z,1;x,y,z+1/2,-1                                         ")
     Shubnikov_info( 556)= Shub_Spgr_Info_Type("63.466 ","C_cmcm      ","65.12.556  ","C_2cmm'm'     ","Cmcm.1'_c[Cmmm]            ","-C 2c 2 1'c         ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x,-y,z+1/2,1;x,y,-z+1/2,1;x,y,z+1/2,-1                                         ")
     Shubnikov_info( 557)= Shub_Spgr_Info_Type("51.302 ","P_Bmma      ","65.13.557  ","C_Pm'mm       ","Pmma.1'_B[Bmmm]            ","-P 2a 2a 1'B        ",  &
                                               "-x+1/2,y,z,1;x,-y,z,1;x+1/2,y,-z,1;x+1/2,y,z+1/2,-1                                                     ")
     Shubnikov_info( 558)= Shub_Spgr_Info_Type("59.415 ","P_Cmmn      ","65.14.558  ","C_Pmmm'       ","Pmmn.1'_C[Cmmm]            ","-P 2ab 2a 1'C       ",  &
                                               "-x+1/2,y,z,1;x,-y+1/2,z,1;x+1/2,y+1/2,-z,1;x+1/2,y+1/2,z,-1                                             ")
     Shubnikov_info( 559)= Shub_Spgr_Info_Type("55.363 ","P_Cbam      ","65.15.559  ","C_Pm'm'm      ","Pbam.1'_C[Cmmm]            ","-P 2 2ab 1'C        ",  &
                                               "-x+1/2,y+1/2,z,1;x+1/2,-y+1/2,z,1;x,y,-z,1;x+1/2,y+1/2,z,-1                                             ")
     Shubnikov_info( 560)= Shub_Spgr_Info_Type("53.334 ","P_Bmna      ","65.16.560  ","C_Pmm'm'      ","Pmna.1'_B[Bmmm]            ","-P 2ac 2 1'B        ",  &
                                               "-x,y,z,1;x+1/2,-y,z+1/2,1;x+1/2,y,-z+1/2,1;x+1/2,y,z+1/2,-1                                             ")
     Shubnikov_info( 561)= Shub_Spgr_Info_Type("50.287 ","P_Cban      ","65.17.561  ","C_Pm'm'm'     ","Pban.1'_C[Cmmm]            ","-P 2ab 2b 1'C       ",  &
                                               "-x,y+1/2,z,1;x+1/2,-y,z,1;x+1/2,y+1/2,-z,1;x+1/2,y+1/2,z,-1                                             ")
     Shubnikov_info( 562)= Shub_Spgr_Info_Type("74.562 ","I_bmma      ","65.18.562  ","C_Im'mm       ","Imma.1'_b[Bmmm]            ","-I 2b 2 1'b         ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z,1;x,-y+1/2,z,1;x,y+1/2,-z,1;x,y+1/2,z,-1                                     ")
     Shubnikov_info( 563)= Shub_Spgr_Info_Type("72.546 ","I_cbam      ","65.19.563  ","C_Im'm'm      ","Ibam.1'_c[Cmmm]            ","-I 2 2c 1'c         ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z+1/2,1;x,-y,z+1/2,1;x,y,-z,1;x,y,z+1/2,-1                                     ")
     Shubnikov_info( 564)= Shub_Spgr_Info_Type("66.491 ","Cccm        ","66.1.564   ","Cccm          ","Cccm.1                     ","-C 2 2c             ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z+1/2,1;x,-y,z+1/2,1;x,y,-z,1                                                      ")
     Shubnikov_info( 565)= Shub_Spgr_Info_Type("66.492 ","Cccm1'      ","66.2.565   ","Cccm1'        ","Cccm.1'                    ","-C 2 2c 1'          ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z+1/2,1;x,-y,z+1/2,1;x,y,-z,1;x,y,z,-1                                             ")
     Shubnikov_info( 566)= Shub_Spgr_Info_Type("66.493 ","Cc'cm       ","66.3.566   ","Cc'cm         ","Cc'cm                      ","C 2' 2c -1'         ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z+1/2,-1;x,-y,z+1/2,1;x,y,-z,1                                                     ")
     Shubnikov_info( 567)= Shub_Spgr_Info_Type("66.494 ","Cccm'       ","66.4.567   ","Cccm'         ","Cccm'                      ","C 2 2c' -1'         ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z+1/2,1;x,-y,z+1/2,1;x,y,-z,-1                                                     ")
     Shubnikov_info( 568)= Shub_Spgr_Info_Type("66.495 ","Cc'c'm      ","66.5.568   ","Cc'c'm        ","Cc'c'm                     ","-C 2 2c'            ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z+1/2,-1;x,-y,z+1/2,-1;x,y,-z,1                                                    ")
     Shubnikov_info( 569)= Shub_Spgr_Info_Type("66.496 ","Ccc'm'      ","66.6.569   ","Ccc'm'        ","Ccc'm'                     ","-C 2' 2c            ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z+1/2,1;x,-y,z+1/2,-1;x,y,-z,-1                                                    ")
     Shubnikov_info( 570)= Shub_Spgr_Info_Type("66.497 ","Cc'c'm'     ","66.7.570   ","Cc'c'm'       ","Cc'c'm'                    ","C 2 2c -1'          ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z+1/2,-1;x,-y,z+1/2,-1;x,y,-z,-1                                                   ")
     Shubnikov_info( 571)= Shub_Spgr_Info_Type("49.275 ","P_Cccm      ","66.8.571   ","C_Pccm        ","Pccm.1'_C[Cccm]            ","-P 2 2c 1'C         ",  &
                                               "-x,y,z+1/2,1;x,-y,z+1/2,1;x,y,-z,1;x+1/2,y+1/2,z,-1                                                     ")
     Shubnikov_info( 572)= Shub_Spgr_Info_Type("53.333 ","P_Amna      ","66.9.572   ","C_Pc'cm       ","Pmna.1'_A[Amaa]            ","-P 2ac 2 1'A        ",  &
                                               "-x,y,z,1;x+1/2,-y,z+1/2,1;x+1/2,y,-z+1/2,1;x,y+1/2,z+1/2,-1                                             ")
     Shubnikov_info( 573)= Shub_Spgr_Info_Type("56.375 ","P_Cccn      ","66.10.573  ","C_Pccm'       ","Pccn.1'_C[Cccm]            ","-P 2ab 2ac 1'C      ",  &
                                               "-x+1/2,y,z+1/2,1;x,-y+1/2,z+1/2,1;x+1/2,y+1/2,-z,1;x+1/2,y+1/2,z,-1                                     ")
     Shubnikov_info( 574)= Shub_Spgr_Info_Type("58.403 ","P_Cnnm      ","66.11.574  ","C_Pc'c'm      ","Pnnm.1'_C[Cccm]            ","-P 2 2n 1'C         ",  &
                                               "-x+1/2,y+1/2,z+1/2,1;x+1/2,-y+1/2,z+1/2,1;x,y,-z,1;x+1/2,y+1/2,z,-1                                     ")
     Shubnikov_info( 575)= Shub_Spgr_Info_Type("52.317 ","P_Anna      ","66.12.575  ","C_Pcc'm'      ","Pnna.1'_A[Amaa]            ","-P 2a 2bc 1'A       ",  &
                                               "-x,y+1/2,z+1/2,1;x+1/2,-y+1/2,z+1/2,1;x+1/2,y,-z,1;x,y+1/2,z+1/2,-1                                     ")
     Shubnikov_info( 576)= Shub_Spgr_Info_Type("48.263 ","P_Cnnn      ","66.13.576  ","C_Pc'c'm'     ","Pnnn.1'_C[Cccm]            ","-P 2ab 2bc 1'C      ",  &
                                               "-x,y+1/2,z+1/2,1;x+1/2,-y,z+1/2,1;x+1/2,y+1/2,-z,1;x+1/2,y+1/2,z,-1                                     ")
     Shubnikov_info( 577)= Shub_Spgr_Info_Type("67.501 ","Cmma        ","67.1.577   ","Cmma          ","Cmma.1                     ","-C 2b 2             ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x+1/2,-y,z,1;x+1/2,y,-z,1                                                      ")
     Shubnikov_info( 578)= Shub_Spgr_Info_Type("67.502 ","Cmma1'      ","67.2.578   ","Cmma1'        ","Cmma.1'                    ","-C 2b 2 1'          ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x+1/2,-y,z,1;x+1/2,y,-z,1;x,y,z,-1                                             ")
     Shubnikov_info( 579)= Shub_Spgr_Info_Type("67.503 ","Cm'ma       ","67.3.579   ","Cm'ma         ","Cm'ma                      ","C 2b' 2 -1'         ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,-1;x+1/2,-y,z,1;x+1/2,y,-z,1                                                     ")
     Shubnikov_info( 580)= Shub_Spgr_Info_Type("67.504 ","Cmma'       ","67.4.580   ","Cmma'         ","Cmma'                      ","C 2b 2' -1'         ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x+1/2,-y,z,1;x+1/2,y,-z,-1                                                     ")
     Shubnikov_info( 581)= Shub_Spgr_Info_Type("67.505 ","Cm'm'a      ","67.5.581   ","Cm'm'a        ","Cm'm'a                     ","-C 2b 2'            ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,-1;x+1/2,-y,z,-1;x+1/2,y,-z,1                                                    ")
     Shubnikov_info( 582)= Shub_Spgr_Info_Type("67.506 ","Cmm'a'      ","67.6.582   ","Cmm'a'        ","Cmm'a'                     ","-C 2b' 2            ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x+1/2,-y,z,-1;x+1/2,y,-z,-1                                                    ")
     Shubnikov_info( 583)= Shub_Spgr_Info_Type("67.507 ","Cm'm'a'     ","67.7.583   ","Cm'm'a'       ","Cm'm'a'                    ","C 2b 2 -1'          ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,-1;x+1/2,-y,z,-1;x+1/2,y,-z,-1                                                   ")
     Shubnikov_info( 584)= Shub_Spgr_Info_Type("67.508 ","C_cmma      ","67.8.584   ","C_2cmma       ","Cmma.1'_c[Cmma]            ","-C 2b 2 1'c         ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x+1/2,-y,z,1;x+1/2,y,-z,1;x,y,z+1/2,-1                                         ")
     Shubnikov_info( 585)= Shub_Spgr_Info_Type("49.274 ","P_Bccm      ","67.9.585   ","C_Pmma        ","Pccm.1'_B[Bmcm]            ","-P 2 2c 1'B         ",  &
                                               "-x,y,z+1/2,1;x,-y,z+1/2,1;x,y,-z,1;x+1/2,y,z+1/2,-1                                                     ")
     Shubnikov_info( 586)= Shub_Spgr_Info_Type("72.547 ","I_bbam      ","67.10.586  ","C_Imma        ","Ibam.1'_b[Bmcm]            ","-I 2 2c 1'b         ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z+1/2,1;x,-y,z+1/2,1;x,y,-z,1;x,y+1/2,z,-1                                     ")
     Shubnikov_info( 587)= Shub_Spgr_Info_Type("64.478 ","C_cmca      ","67.11.587  ","C_2cm'ma      ","Cmca.1'_c[Cmmb]            ","-C 2bc 2 1'c        ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x+1/2,-y,z+1/2,1;x+1/2,y,-z+1/2,1;x,y,z+1/2,-1                                 ")
     Shubnikov_info( 588)= Shub_Spgr_Info_Type("68.518 ","C_ccca      ","67.12.588  ","C_2cm'm'a     ","Ccca.1'_c[Cmmb]            ","-C 2b 2bc 1'c       ",  &
                                               "x+1/2,y+1/2,z,1;-x+1/2,y,z+1/2,1;x+1/2,-y+1/2,z+1/2,1;x+1/2,y,-z,1;x,y,z+1/2,-1                         ")
     Shubnikov_info( 589)= Shub_Spgr_Info_Type("54.350 ","P_Bcca      ","67.13.589  ","C_Pm'ma       ","Pcca.1'_B[Bmcm]            ","-P 2a 2ac 1'B       ",  &
                                               "-x+1/2,y,z+1/2,1;x,-y,z+1/2,1;x+1/2,y,-z,1;x+1/2,y,z+1/2,-1                                             ")
     Shubnikov_info( 590)= Shub_Spgr_Info_Type("51.303 ","P_Cmma      ","67.14.590  ","C_Pmm'a       ","Pmma.1'_C[Cmma]            ","-P 2a 2a 1'C        ",  &
                                               "-x+1/2,y,z,1;x,-y,z,1;x+1/2,y,-z,1;x+1/2,y+1/2,z,-1                                                     ")
     Shubnikov_info( 591)= Shub_Spgr_Info_Type("57.389 ","P_Abcm      ","67.15.591  ","C_Pmma'       ","Pbcm.1'_A[Acmm]            ","-P 2c 2b 1'A        ",  &
                                               "-x,y+1/2,z,1;x,-y+1/2,z+1/2,1;x,y,-z+1/2,1;x,y+1/2,z+1/2,-1                                             ")
     Shubnikov_info( 592)= Shub_Spgr_Info_Type("74.561 ","I_cmma      ","67.16.592  ","C_Imm'a       ","Imma.1'_c[Cmma]            ","-I 2b 2 1'c         ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z,1;x,-y+1/2,z,1;x,y+1/2,-z,1;x,y,z+1/2,-1                                     ")
     Shubnikov_info( 593)= Shub_Spgr_Info_Type("73.553 ","I_cbca      ","67.17.593  ","C_Im'ma'      ","Ibca.1'_c[Cmma]            ","-I 2b 2c 1'c        ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z+1/2,1;x+1/2,-y,z,1;x,y+1/2,-z,1;x,y,z+1/2,-1                                 ")
     Shubnikov_info( 594)= Shub_Spgr_Info_Type("68.511 ","Ccca        ","68.1.594   ","Ccca          ","Ccca.1                     ","-C 2b 2bc           ",  &
                                               "x+1/2,y+1/2,z,1;-x+1/2,y,z+1/2,1;x+1/2,-y+1/2,z+1/2,1;x+1/2,y,-z,1                                      ")
     Shubnikov_info( 595)= Shub_Spgr_Info_Type("68.512 ","Ccca1'      ","68.2.595   ","Ccca1'        ","Ccca.1'                    ","-C 2b 2bc 1'        ",  &
                                               "x+1/2,y+1/2,z,1;-x+1/2,y,z+1/2,1;x+1/2,-y+1/2,z+1/2,1;x+1/2,y,-z,1;x,y,z,-1                             ")
     Shubnikov_info( 596)= Shub_Spgr_Info_Type("68.513 ","Cc'ca       ","68.3.596   ","Cc'ca         ","Cc'ca                      ","C 2b' 2bc -1'       ",  &
                                               "x+1/2,y+1/2,z,1;-x+1/2,y,z+1/2,-1;x+1/2,-y+1/2,z+1/2,1;x+1/2,y,-z,1                                     ")
     Shubnikov_info( 597)= Shub_Spgr_Info_Type("68.514 ","Ccca'       ","68.4.597   ","Ccca'         ","Ccca'                      ","C 2b 2bc' -1'       ",  &
                                               "x+1/2,y+1/2,z,1;-x+1/2,y,z+1/2,1;x+1/2,-y+1/2,z+1/2,1;x+1/2,y,-z,-1                                     ")
     Shubnikov_info( 598)= Shub_Spgr_Info_Type("68.515 ","Cc'c'a      ","68.5.598   ","Cc'c'a        ","Cc'c'a                     ","-C 2b 2bc'          ",  &
                                               "x+1/2,y+1/2,z,1;-x+1/2,y,z+1/2,-1;x+1/2,-y+1/2,z+1/2,-1;x+1/2,y,-z,1                                    ")
     Shubnikov_info( 599)= Shub_Spgr_Info_Type("68.516 ","Ccc'a'      ","68.6.599   ","Ccc'a'        ","Ccc'a'                     ","-C 2b' 2bc          ",  &
                                               "x+1/2,y+1/2,z,1;-x+1/2,y,z+1/2,1;x+1/2,-y+1/2,z+1/2,-1;x+1/2,y,-z,-1                                    ")
     Shubnikov_info( 600)= Shub_Spgr_Info_Type("68.517 ","Cc'c'a'     ","68.7.600   ","Cc'c'a'       ","Cc'c'a'                    ","C 2b 2bc -1'        ",  &
                                               "x+1/2,y+1/2,z,1;-x+1/2,y,z+1/2,-1;x+1/2,-y+1/2,z+1/2,-1;x+1/2,y,-z,-1                                   ")
     Shubnikov_info( 601)= Shub_Spgr_Info_Type("50.286 ","P_Aban      ","68.8.601   ","C_Pcca        ","Pban.1'_A[Acaa]            ","-P 2ab 2b 1'A       ",  &
                                               "-x,y+1/2,z,1;x+1/2,-y,z,1;x+1/2,y+1/2,-z,1;x,y+1/2,z+1/2,-1                                             ")
     Shubnikov_info( 602)= Shub_Spgr_Info_Type("54.351 ","P_Ccca      ","68.9.602   ","C_Pc'ca       ","Pcca.1'_C[Cccb]            ","-P 2a 2ac 1'C       ",  &
                                               "-x+1/2,y,z+1/2,1;x,-y,z+1/2,1;x+1/2,y,-z,1;x+1/2,y+1/2,z,-1                                             ")
     Shubnikov_info( 603)= Shub_Spgr_Info_Type("60.430 ","P_Bbcn      ","68.10.603  ","C_Pcca'       ","Pbcn.1'_B[Bbcb]            ","-P 2n 2ab 1'B       ",  &
                                               "-x+1/2,y+1/2,z,1;x,-y,z+1/2,1;x+1/2,y+1/2,-z+1/2,1;x+1/2,y,z+1/2,-1                                     ")
     Shubnikov_info( 604)= Shub_Spgr_Info_Type("52.319 ","P_Cnna      ","68.11.604  ","C_Pcc'a'      ","Pnna.1'_C[Ccca]            ","-P 2a 2bc 1'C       ",  &
                                               "-x,y+1/2,z+1/2,1;x+1/2,-y+1/2,z+1/2,1;x+1/2,y,-z,1;x+1/2,y+1/2,z,-1                                     ")
     Shubnikov_info( 605)= Shub_Spgr_Info_Type("69.521 ","Fmmm        ","69.1.605   ","Fmmm          ","Fmmm.1                     ","-F 2 2              ",  &
                                               "x+1/2,y+1/2,z,1;x+1/2,y,z+1/2,1;-x,y,z,1;x,-y,z,1;x,y,-z,1                                              ")
     Shubnikov_info( 606)= Shub_Spgr_Info_Type("69.522 ","Fmmm1'      ","69.2.606   ","Fmmm1'        ","Fmmm.1'                    ","-F 2 2 1'           ",  &
                                               "x+1/2,y+1/2,z,1;x+1/2,y,z+1/2,1;-x,y,z,1;x,-y,z,1;x,y,-z,1;x,y,z,-1                                     ")
     Shubnikov_info( 607)= Shub_Spgr_Info_Type("69.523 ","Fm'mm       ","69.3.607   ","Fm'mm         ","Fm'mm                      ","F 2' 2 -1'          ",  &
                                               "x+1/2,y+1/2,z,1;x+1/2,y,z+1/2,1;-x,y,z,-1;x,-y,z,1;x,y,-z,1                                             ")
     Shubnikov_info( 608)= Shub_Spgr_Info_Type("69.524 ","Fm'm'm      ","69.4.608   ","Fm'm'm        ","Fm'm'm                     ","-F 2 2'             ",  &
                                               "x+1/2,y+1/2,z,1;x+1/2,y,z+1/2,1;-x,y,z,-1;x,-y,z,-1;x,y,-z,1                                            ")
     Shubnikov_info( 609)= Shub_Spgr_Info_Type("69.525 ","Fm'm'm'     ","69.5.609   ","Fm'm'm'       ","Fm'm'm'                    ","F 2 2 -1'           ",  &
                                               "x+1/2,y+1/2,z,1;x+1/2,y,z+1/2,1;-x,y,z,-1;x,-y,z,-1;x,y,-z,-1                                           ")
     Shubnikov_info( 610)= Shub_Spgr_Info_Type("65.490 ","C_Ammm      ","69.6.610   ","F_Cmmm        ","Cmmm.1'_A[Fmmm]            ","-C 2 2 1'A          ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x,-y,z,1;x,y,-z,1;x,y+1/2,z+1/2,-1                                             ")
     Shubnikov_info( 611)= Shub_Spgr_Info_Type("63.468 ","C_Amcm      ","69.7.611   ","F_Cm'mm       ","Cmcm.1'_A[Fmmm]            ","-C 2c 2 1'A         ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x,-y,z+1/2,1;x,y,-z+1/2,1;x,y+1/2,z+1/2,-1                                     ")
     Shubnikov_info( 612)= Shub_Spgr_Info_Type("67.510 ","C_Amma      ","69.8.612   ","F_Cmmm'       ","Cmma.1'_A[Fmmm]            ","-C 2b 2 1'A         ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x+1/2,-y,z,1;x+1/2,y,-z,1;x,y+1/2,z+1/2,-1                                     ")
     Shubnikov_info( 613)= Shub_Spgr_Info_Type("66.500 ","C_Accm      ","69.9.613   ","F_Cm'm'm      ","Cccm.1'_A[Fmmm]            ","-C 2 2c 1'A         ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z+1/2,1;x,-y,z+1/2,1;x,y,-z,1;x,y+1/2,z+1/2,-1                                     ")
     Shubnikov_info( 614)= Shub_Spgr_Info_Type("64.480 ","C_Amca      ","69.10.614  ","F_Cmm'm'      ","Cmca.1'_A[Fmmm]            ","-C 2bc 2 1'A        ",  &
                                               "x+1/2,y+1/2,z,1;-x,y,z,1;x+1/2,-y,z+1/2,1;x+1/2,y,-z+1/2,1;x,y+1/2,z+1/2,-1                             ")
     Shubnikov_info( 615)= Shub_Spgr_Info_Type("68.520 ","C_Acca      ","69.11.615  ","F_Cm'm'm'     ","Ccca.1'_A[Fmmm]            ","-C 2b 2bc 1'A       ",  &
                                               "x+1/2,y+1/2,z,1;-x+1/2,y,z+1/2,1;x+1/2,-y+1/2,z+1/2,1;x+1/2,y,-z,1;x,y+1/2,z+1/2,-1                     ")
     Shubnikov_info( 616)= Shub_Spgr_Info_Type("70.527 ","Fddd        ","70.1.616   ","Fddd          ","Fddd.1                     ","-F 2uv 2vw          ",  &
                                               "x+1/2,y+1/2,z,1;x+1/2,y,z+1/2,1;-x,y+1/4,z+1/4,1;x+1/4,-y,z+1/4,1;x+1/4,y+1/4,-z,1                      ")
     Shubnikov_info( 617)= Shub_Spgr_Info_Type("70.528 ","Fddd1'      ","70.2.617   ","Fddd1'        ","Fddd.1'                    ","-F 2uv 2vw 1'       ",  &
                                               "x+1/2,y+1/2,z,1;x+1/2,y,z+1/2,1;-x,y+1/4,z+1/4,1;x+1/4,-y,z+1/4,1;x+1/4,y+1/4,-z,1;x,y,z,-1             ")
     Shubnikov_info( 618)= Shub_Spgr_Info_Type("70.529 ","Fd'dd       ","70.3.618   ","Fd'dd         ","Fd'dd                      ","F 2uv' 2vw -1'      ",  &
                                               "x+1/2,y+1/2,z,1;x+1/2,y,z+1/2,1;-x,y+1/4,z+1/4,-1;x+1/4,-y,z+1/4,1;x+1/4,y+1/4,-z,1                     ")
     Shubnikov_info( 619)= Shub_Spgr_Info_Type("70.530 ","Fd'd'd      ","70.4.619   ","Fd'd'd        ","Fd'd'd                     ","-F 2uv 2vw'         ",  &
                                               "x+1/2,y+1/2,z,1;x+1/2,y,z+1/2,1;-x,y+1/4,z+1/4,-1;x+1/4,-y,z+1/4,-1;x+1/4,y+1/4,-z,1                    ")
     Shubnikov_info( 620)= Shub_Spgr_Info_Type("70.531 ","Fd'd'd'     ","70.5.620   ","Fd'd'd'       ","Fd'd'd'                    ","F 2uv 2vw -1'       ",  &
                                               "x+1/2,y+1/2,z,1;x+1/2,y,z+1/2,1;-x,y+1/4,z+1/4,-1;x+1/4,-y,z+1/4,-1;x+1/4,y+1/4,-z,-1                   ")
     Shubnikov_info( 621)= Shub_Spgr_Info_Type("71.533 ","Immm        ","71.1.621   ","Immm          ","Immm.1                     ","-I 2 2              ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z,1;x,-y,z,1;x,y,-z,1                                                          ")
     Shubnikov_info( 622)= Shub_Spgr_Info_Type("71.534 ","Immm1'      ","71.2.622   ","Immm1'        ","Immm.1'                    ","-I 2 2 1'           ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z,1;x,-y,z,1;x,y,-z,1;x,y,z,-1                                                 ")
     Shubnikov_info( 623)= Shub_Spgr_Info_Type("71.535 ","Im'mm       ","71.3.623   ","Im'mm         ","Im'mm                      ","I 2' 2 -1'          ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z,-1;x,-y,z,1;x,y,-z,1                                                         ")
     Shubnikov_info( 624)= Shub_Spgr_Info_Type("71.536 ","Im'm'm      ","71.4.624   ","Im'm'm        ","Im'm'm                     ","-I 2 2'             ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z,-1;x,-y,z,-1;x,y,-z,1                                                        ")
     Shubnikov_info( 625)= Shub_Spgr_Info_Type("71.537 ","Im'm'm'     ","71.5.625   ","Im'm'm'       ","Im'm'm'                    ","I 2 2 -1'           ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z,-1;x,-y,z,-1;x,y,-z,-1                                                       ")
     Shubnikov_info( 626)= Shub_Spgr_Info_Type("47.256 ","P_Immm      ","71.6.626   ","I_Pmmm        ","Pmmm.1'_I[Immm]            ","-P 2 2 1'I          ",  &
                                               "-x,y,z,1;x,-y,z,1;x,y,-z,1;x+1/2,y+1/2,z+1/2,-1                                                         ")
     Shubnikov_info( 627)= Shub_Spgr_Info_Type("59.416 ","P_Immn      ","71.7.627   ","I_Pm'mm       ","Pmmn.1'_I[Immm]            ","-P 2ab 2a 1'I       ",  &
                                               "-x+1/2,y,z,1;x,-y+1/2,z,1;x+1/2,y+1/2,-z,1;x+1/2,y+1/2,z+1/2,-1                                         ")
     Shubnikov_info( 628)= Shub_Spgr_Info_Type("58.404 ","P_Innm      ","71.8.628   ","I_Pm'm'm      ","Pnnm.1'_I[Immm]            ","-P 2 2n 1'I         ",  &
                                               "-x+1/2,y+1/2,z+1/2,1;x+1/2,-y+1/2,z+1/2,1;x,y,-z,1;x+1/2,y+1/2,z+1/2,-1                                 ")
     Shubnikov_info( 629)= Shub_Spgr_Info_Type("48.264 ","P_Innn      ","71.9.629   ","I_Pm'm'm'     ","Pnnn.1'_I[Immm]            ","-P 2ab 2bc 1'I      ",  &
                                               "-x,y+1/2,z+1/2,1;x+1/2,-y,z+1/2,1;x+1/2,y+1/2,-z,1;x+1/2,y+1/2,z+1/2,-1                                 ")
     Shubnikov_info( 630)= Shub_Spgr_Info_Type("72.539 ","Ibam        ","72.1.630   ","Ibam          ","Ibam.1                     ","-I 2 2c             ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z+1/2,1;x,-y,z+1/2,1;x,y,-z,1                                                  ")
     Shubnikov_info( 631)= Shub_Spgr_Info_Type("72.540 ","Ibam1'      ","72.2.631   ","Ibam1'        ","Ibam.1'                    ","-I 2 2c 1'          ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z+1/2,1;x,-y,z+1/2,1;x,y,-z,1;x,y,z,-1                                         ")
     Shubnikov_info( 632)= Shub_Spgr_Info_Type("72.541 ","Ib'am       ","72.3.632   ","Ib'am         ","Ib'am                      ","I 2' 2c -1'         ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z+1/2,-1;x,-y,z+1/2,1;x,y,-z,1                                                 ")
     Shubnikov_info( 633)= Shub_Spgr_Info_Type("72.542 ","Ibam'       ","72.4.633   ","Ibam'         ","Ibam'                      ","I 2 2c' -1'         ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z+1/2,1;x,-y,z+1/2,1;x,y,-z,-1                                                 ")
     Shubnikov_info( 634)= Shub_Spgr_Info_Type("72.543 ","Ib'a'm      ","72.5.634   ","Ib'a'm        ","Ib'a'm                     ","-I 2 2c'            ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z+1/2,-1;x,-y,z+1/2,-1;x,y,-z,1                                                ")
     Shubnikov_info( 635)= Shub_Spgr_Info_Type("72.544 ","Iba'm'      ","72.6.635   ","Iba'm'        ","Iba'm'                     ","-I 2' 2c            ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z+1/2,1;x,-y,z+1/2,-1;x,y,-z,-1                                                ")
     Shubnikov_info( 636)= Shub_Spgr_Info_Type("72.545 ","Ib'a'm'     ","72.7.636   ","Ib'a'm'       ","Ib'a'm'                    ","I 2 2c -1'          ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z+1/2,-1;x,-y,z+1/2,-1;x,y,-z,-1                                               ")
     Shubnikov_info( 637)= Shub_Spgr_Info_Type("49.276 ","P_Iccm      ","72.8.637   ","I_Pbam        ","Pccm.1'_I[Ibam]            ","-P 2 2c 1'I         ",  &
                                               "-x,y,z+1/2,1;x,-y,z+1/2,1;x,y,-z,1;x+1/2,y+1/2,z+1/2,-1                                                 ")
     Shubnikov_info( 638)= Shub_Spgr_Info_Type("57.392 ","P_Ibcm      ","72.9.638   ","I_Pb'am       ","Pbcm.1'_I[Ibam]            ","-P 2c 2b 1'I        ",  &
                                               "-x,y+1/2,z,1;x,-y+1/2,z+1/2,1;x,y,-z+1/2,1;x+1/2,y+1/2,z+1/2,-1                                         ")
     Shubnikov_info( 639)= Shub_Spgr_Info_Type("56.376 ","P_Iccn      ","72.10.639  ","I_Pbam'       ","Pccn.1'_I[Ibam]            ","-P 2ab 2ac 1'I      ",  &
                                               "-x+1/2,y,z+1/2,1;x,-y+1/2,z+1/2,1;x+1/2,y+1/2,-z,1;x+1/2,y+1/2,z+1/2,-1                                 ")
     Shubnikov_info( 640)= Shub_Spgr_Info_Type("55.364 ","P_Ibam      ","72.11.640  ","I_Pb'a'm      ","Pbam.1'_I[Ibam]            ","-P 2 2ab 1'I        ",  &
                                               "-x+1/2,y+1/2,z,1;x+1/2,-y+1/2,z,1;x,y,-z,1;x+1/2,y+1/2,z+1/2,-1                                         ")
     Shubnikov_info( 641)= Shub_Spgr_Info_Type("60.432 ","P_Ibcn      ","72.12.641  ","I_Pb'am'      ","Pbcn.1'_I[Ibam]            ","-P 2n 2ab 1'I       ",  &
                                               "-x+1/2,y+1/2,z,1;x,-y,z+1/2,1;x+1/2,y+1/2,-z+1/2,1;x+1/2,y+1/2,z+1/2,-1                                 ")
     Shubnikov_info( 642)= Shub_Spgr_Info_Type("50.288 ","P_Iban      ","72.13.642  ","I_Pb'a'm'     ","Pban.1'_I[Ibam]            ","-P 2ab 2b 1'I       ",  &
                                               "-x,y+1/2,z,1;x+1/2,-y,z,1;x+1/2,y+1/2,-z,1;x+1/2,y+1/2,z+1/2,-1                                         ")
     Shubnikov_info( 643)= Shub_Spgr_Info_Type("73.548 ","Ibca        ","73.1.643   ","Ibca          ","Ibca.1                     ","-I 2b 2c            ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z+1/2,1;x+1/2,-y,z,1;x,y+1/2,-z,1                                              ")
     Shubnikov_info( 644)= Shub_Spgr_Info_Type("73.549 ","Ibca1'      ","73.2.644   ","Ibca1'        ","Ibca.1'                    ","-I 2b 2c 1'         ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z+1/2,1;x+1/2,-y,z,1;x,y+1/2,-z,1;x,y,z,-1                                     ")
     Shubnikov_info( 645)= Shub_Spgr_Info_Type("73.550 ","Ib'ca       ","73.3.645   ","Ib'ca         ","Ib'ca                      ","I 2b' 2c -1'        ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z+1/2,-1;x+1/2,-y,z,1;x,y+1/2,-z,1                                             ")
     Shubnikov_info( 646)= Shub_Spgr_Info_Type("73.551 ","Ib'c'a      ","73.4.646   ","Ib'c'a        ","Ib'c'a                     ","-I 2b 2c'           ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z+1/2,-1;x+1/2,-y,z,-1;x,y+1/2,-z,1                                            ")
     Shubnikov_info( 647)= Shub_Spgr_Info_Type("73.552 ","Ib'c'a'     ","73.5.647   ","Ib'c'a'       ","Ib'c'a'                    ","I 2b 2c -1'         ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z+1/2,-1;x+1/2,-y,z,-1;x,y+1/2,-z,-1                                           ")
     Shubnikov_info( 648)= Shub_Spgr_Info_Type("61.440 ","P_Ibca      ","73.6.648   ","I_Pbca        ","Pbca.1'_I[Icab]            ","-P 2ac 2ab 1'I      ",  &
                                               "-x+1/2,y+1/2,z,1;x,-y+1/2,z+1/2,1;x+1/2,y,-z+1/2,1;x+1/2,y+1/2,z+1/2,-1                                 ")
     Shubnikov_info( 649)= Shub_Spgr_Info_Type("54.352 ","P_Icca      ","73.7.649   ","I_Pb'ca       ","Pcca.1'_I[Icab]            ","-P 2a 2ac 1'I       ",  &
                                               "-x+1/2,y,z+1/2,1;x,-y,z+1/2,1;x+1/2,y,-z,1;x+1/2,y+1/2,z+1/2,-1                                         ")
     Shubnikov_info( 650)= Shub_Spgr_Info_Type("74.554 ","Imma        ","74.1.650   ","Imma          ","Imma.1                     ","-I 2b 2             ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z,1;x,-y+1/2,z,1;x,y+1/2,-z,1                                                  ")
     Shubnikov_info( 651)= Shub_Spgr_Info_Type("74.555 ","Imma1'      ","74.2.651   ","Imma1'        ","Imma.1'                    ","-I 2b 2 1'          ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z,1;x,-y+1/2,z,1;x,y+1/2,-z,1;x,y,z,-1                                         ")
     Shubnikov_info( 652)= Shub_Spgr_Info_Type("74.556 ","Im'ma       ","74.3.652   ","Im'ma         ","Im'ma                      ","I 2b' 2 -1'         ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z,-1;x,-y+1/2,z,1;x,y+1/2,-z,1                                                 ")
     Shubnikov_info( 653)= Shub_Spgr_Info_Type("74.557 ","Imma'       ","74.4.653   ","Imma'         ","Imma'                      ","I 2b 2' -1'         ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z,1;x,-y+1/2,z,1;x,y+1/2,-z,-1                                                 ")
     Shubnikov_info( 654)= Shub_Spgr_Info_Type("74.558 ","Im'm'a      ","74.5.654   ","Im'm'a        ","Im'm'a                     ","-I 2b 2'            ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z,-1;x,-y+1/2,z,-1;x,y+1/2,-z,1                                                ")
     Shubnikov_info( 655)= Shub_Spgr_Info_Type("74.559 ","Imm'a'      ","74.6.655   ","Imm'a'        ","Imm'a'                     ","-I 2b' 2            ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z,1;x,-y+1/2,z,-1;x,y+1/2,-z,-1                                                ")
     Shubnikov_info( 656)= Shub_Spgr_Info_Type("74.560 ","Im'm'a'     ","74.7.656   ","Im'm'a'       ","Im'm'a'                    ","I 2b 2 -1'          ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,y,z,-1;x,-y+1/2,z,-1;x,y+1/2,-z,-1                                               ")
     Shubnikov_info( 657)= Shub_Spgr_Info_Type("51.304 ","P_Imma      ","74.8.657   ","I_Pmma        ","Pmma.1'_I[Immb]            ","-P 2a 2a 1'I        ",  &
                                               "-x+1/2,y,z,1;x,-y,z,1;x+1/2,y,-z,1;x+1/2,y+1/2,z+1/2,-1                                                 ")
     Shubnikov_info( 658)= Shub_Spgr_Info_Type("52.320 ","P_Inna      ","74.9.658   ","I_Pm'm'a      ","Pnna.1'_I[Immb]            ","-P 2a 2bc 1'I       ",  &
                                               "-x,y+1/2,z+1/2,1;x+1/2,-y+1/2,z+1/2,1;x+1/2,y,-z,1;x+1/2,y+1/2,z+1/2,-1                                 ")
     Shubnikov_info( 659)= Shub_Spgr_Info_Type("53.336 ","P_Imna      ","74.10.659  ","I_Pmm'a'      ","Pmna.1'_I[Imma]            ","-P 2ac 2 1'I        ",  &
                                               "-x,y,z,1;x+1/2,-y,z+1/2,1;x+1/2,y,-z+1/2,1;x+1/2,y+1/2,z+1/2,-1                                         ")
     Shubnikov_info( 660)= Shub_Spgr_Info_Type("62.456 ","P_Inma      ","74.11.660  ","I_Pm'ma'      ","Pnma.1'_I[Imma]            ","-P 2ac 2n 1'I       ",  &
                                               "-x+1/2,y+1/2,z+1/2,1;x,-y+1/2,z,1;x+1/2,y,-z+1/2,1;x+1/2,y+1/2,z+1/2,-1                                 ")
     Shubnikov_info( 661)= Shub_Spgr_Info_Type("75.1   ","P4          ","75.1.661   ","P4            ","P4.1                       ","P 4                 ",  &
                                               "-y,x,z,1                                                                                                ")
     Shubnikov_info( 662)= Shub_Spgr_Info_Type("75.2   ","P41'        ","75.2.662   ","P41'          ","P4.1'                      ","P 4 1'              ",  &
                                               "-y,x,z,1;x,y,z,-1                                                                                       ")
     Shubnikov_info( 663)= Shub_Spgr_Info_Type("75.3   ","P4'         ","75.3.663   ","P4'           ","P4'                        ","P 4'                ",  &
                                               "-y,x,z,-1                                                                                               ")
     Shubnikov_info( 664)= Shub_Spgr_Info_Type("75.4   ","P_c4        ","75.4.664   ","P_2c4         ","P4.1'_c[P4]                ","P 4 1'c             ",  &
                                               "-y,x,z,1;x,y,z+1/2,-1                                                                                   ")
     Shubnikov_info( 665)= Shub_Spgr_Info_Type("75.5   ","P_C4        ","75.5.665   ","P_P4          ","P4.1'_C[rP4]               ","P 4 1'C             ",  &
                                               "-y,x,z,1;x+1/2,y+1/2,z,-1                                                                               ")
     Shubnikov_info( 666)= Shub_Spgr_Info_Type("79.28  ","I_c4        ","75.6.666   ","P_I4          ","I4.1'_c[rP4]               ","I 4 1'c             ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,1;x,y,z+1/2,-1                                                               ")
     Shubnikov_info( 667)= Shub_Spgr_Info_Type("77.16  ","P_c4_2      ","75.7.667   ","P_2c4'        ","P4_2.1'_c[P4]              ","P 4c 1'c            ",  &
                                               "-y,x,z+1/2,1;x,y,z+1/2,-1                                                                               ")
     Shubnikov_info( 668)= Shub_Spgr_Info_Type("76.7   ","P4_1        ","76.1.668   ","P4_1          ","P4_1.1                     ","P 4w                ",  &
                                               "-y,x,z+1/4,1                                                                                            ")
     Shubnikov_info( 669)= Shub_Spgr_Info_Type("76.8   ","P4_11'      ","76.2.669   ","P4_11'        ","P4_1.1'                    ","P 4w 1'             ",  &
                                               "-y,x,z+1/4,1;x,y,z,-1                                                                                   ")
     Shubnikov_info( 670)= Shub_Spgr_Info_Type("76.9   ","P4_1'       ","76.3.670   ","P4_1'         ","P4_1'                      ","P 4w'               ",  &
                                               "-y,x,z+1/4,-1                                                                                           ")
     Shubnikov_info( 671)= Shub_Spgr_Info_Type("76.11  ","P_C4_1      ","76.4.671   ","P_P4_1        ","P4_1.1'_C[rP4_1]           ","P 4w 1'C            ",  &
                                               "-y,x,z+1/4,1;x+1/2,y+1/2,z,-1                                                                           ")
     Shubnikov_info( 672)= Shub_Spgr_Info_Type("77.13  ","P4_2        ","77.1.672   ","P4_2          ","P4_2.1                     ","P 4c                ",  &
                                               "-y,x,z+1/2,1                                                                                            ")
     Shubnikov_info( 673)= Shub_Spgr_Info_Type("77.14  ","P4_21'      ","77.2.673   ","P4_21'        ","P4_2.1'                    ","P 4c 1'             ",  &
                                               "-y,x,z+1/2,1;x,y,z,-1                                                                                   ")
     Shubnikov_info( 674)= Shub_Spgr_Info_Type("77.15  ","P4_2'       ","77.3.674   ","P4_2'         ","P4_2'                      ","P 4c'               ",  &
                                               "-y,x,z+1/2,-1                                                                                           ")
     Shubnikov_info( 675)= Shub_Spgr_Info_Type("76.10  ","P_c4_1      ","77.4.675   ","P_2c4_2       ","P4_1.1'_c[P4_2]            ","P 4w 1'c            ",  &
                                               "-y,x,z+1/4,1;x,y,z+1/2,-1                                                                               ")
     Shubnikov_info( 676)= Shub_Spgr_Info_Type("77.17  ","P_C4_2      ","77.5.676   ","P_P4_2        ","P4_2.1'_C[rP4_2]           ","P 4c 1'C            ",  &
                                               "-y,x,z+1/2,1;x+1/2,y+1/2,z,-1                                                                           ")
     Shubnikov_info( 677)= Shub_Spgr_Info_Type("80.32  ","I_c4_1      ","77.6.677   ","P_I4_2        ","I4_1.1'_c[rP4_2]           ","I 4bw 1'c           ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x+1/2,z+1/4,1;x,y,z+1/2,-1                                                       ")
     Shubnikov_info( 678)= Shub_Spgr_Info_Type("78.22  ","P_c4_3      ","77.7.678   ","P_2c4_2'      ","P4_3.1'_c[P4_2]            ","P 4cw 1'c           ",  &
                                               "-y,x,z+3/4,1;x,y,z+1/2,-1                                                                               ")
     Shubnikov_info( 679)= Shub_Spgr_Info_Type("78.19  ","P4_3        ","78.1.679   ","P4_3          ","P4_3.1                     ","P 4cw               ",  &
                                               "-y,x,z+3/4,1                                                                                            ")
     Shubnikov_info( 680)= Shub_Spgr_Info_Type("78.20  ","P4_31'      ","78.2.680   ","P4_31'        ","P4_3.1'                    ","P 4cw 1'            ",  &
                                               "-y,x,z+3/4,1;x,y,z,-1                                                                                   ")
     Shubnikov_info( 681)= Shub_Spgr_Info_Type("78.21  ","P4_3'       ","78.3.681   ","P4_3'         ","P4_3'                      ","P 4cw'              ",  &
                                               "-y,x,z+3/4,-1                                                                                           ")
     Shubnikov_info( 682)= Shub_Spgr_Info_Type("78.23  ","P_C4_3      ","78.4.682   ","P_P4_3        ","P4_3.1'_C[rP4_3]           ","P 4cw 1'C           ",  &
                                               "-y,x,z+3/4,1;x+1/2,y+1/2,z,-1                                                                           ")
     Shubnikov_info( 683)= Shub_Spgr_Info_Type("79.25  ","I4          ","79.1.683   ","I4            ","I4.1                       ","I 4                 ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,1                                                                            ")
     Shubnikov_info( 684)= Shub_Spgr_Info_Type("79.26  ","I41'        ","79.2.684   ","I41'          ","I4.1'                      ","I 4 1'              ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,1;x,y,z,-1                                                                   ")
     Shubnikov_info( 685)= Shub_Spgr_Info_Type("79.27  ","I4'         ","79.3.685   ","I4'           ","I4'                        ","I 4'                ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,-1                                                                           ")
     Shubnikov_info( 686)= Shub_Spgr_Info_Type("75.6   ","P_I4        ","79.4.686   ","I_P4          ","P4.1'_I[I4]                ","P 4 1'I             ",  &
                                               "-y,x,z,1;x+1/2,y+1/2,z+1/2,-1                                                                           ")
     Shubnikov_info( 687)= Shub_Spgr_Info_Type("77.18  ","P_I4_2      ","79.5.687   ","I_P4'         ","P4_2.1'_I[I4]              ","P 4c 1'I            ",  &
                                               "-y,x,z+1/2,1;x+1/2,y+1/2,z+1/2,-1                                                                       ")
     Shubnikov_info( 688)= Shub_Spgr_Info_Type("80.29  ","I4_1        ","80.1.688   ","I4_1          ","I4_1.1                     ","I 4bw               ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x+1/2,z+1/4,1                                                                    ")
     Shubnikov_info( 689)= Shub_Spgr_Info_Type("80.30  ","I4_11'      ","80.2.689   ","I4_11'        ","I4_1.1'                    ","I 4bw 1'            ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x+1/2,z+1/4,1;x,y,z,-1                                                           ")
     Shubnikov_info( 690)= Shub_Spgr_Info_Type("80.31  ","I4_1'       ","80.3.690   ","I4_1'         ","I4_1'                      ","I 4bw'              ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x+1/2,z+1/4,-1                                                                   ")
     Shubnikov_info( 691)= Shub_Spgr_Info_Type("76.12  ","P_I4_1      ","80.4.691   ","I_P4_1        ","P4_1.1'_I[I4_1]            ","P 4w 1'I            ",  &
                                               "-y,x,z+1/4,1;x+1/2,y+1/2,z+1/2,-1                                                                       ")
     Shubnikov_info( 692)= Shub_Spgr_Info_Type("78.24  ","P_I4_3      ","80.5.692   ","I_P4_1'       ","P4_3.1'_I[I4_1]            ","P 4cw 1'I           ",  &
                                               "-y,x,z+3/4,1;x+1/2,y+1/2,z+1/2,-1                                                                       ")
     Shubnikov_info( 693)= Shub_Spgr_Info_Type("81.33  ","P-4         ","81.1.693   ","P-4           ","P-4.1                      ","P -4                ",  &
                                               "y,-x,-z,1                                                                                               ")
     Shubnikov_info( 694)= Shub_Spgr_Info_Type("81.34  ","P-41'       ","81.2.694   ","P-41'         ","P-4.1'                     ","P -4 1'             ",  &
                                               "y,-x,-z,1;x,y,z,-1                                                                                      ")
     Shubnikov_info( 695)= Shub_Spgr_Info_Type("81.35  ","P-4'        ","81.3.695   ","P-4'          ","P-4'                       ","P -4'               ",  &
                                               "y,-x,-z,-1                                                                                              ")
     Shubnikov_info( 696)= Shub_Spgr_Info_Type("81.36  ","P_c-4       ","81.4.696   ","P_2c-4        ","P-4.1'_c[P-4]              ","P -4 1'c            ",  &
                                               "y,-x,-z,1;x,y,z+1/2,-1                                                                                  ")
     Shubnikov_info( 697)= Shub_Spgr_Info_Type("81.37  ","P_C-4       ","81.5.697   ","P_P-4         ","P-4.1'_C[rP-4]             ","P -4 1'C            ",  &
                                               "y,-x,-z,1;x+1/2,y+1/2,z,-1                                                                              ")
     Shubnikov_info( 698)= Shub_Spgr_Info_Type("82.42  ","I_c-4       ","81.6.698   ","P_I-4         ","I-4.1'_c[rP-4]             ","I -4 1'c            ",  &
                                               "x+1/2,y+1/2,z+1/2,1;y,-x,-z,1;x,y,z+1/2,-1                                                              ")
     Shubnikov_info( 699)= Shub_Spgr_Info_Type("82.39  ","I-4         ","82.1.699   ","I-4           ","I-4.1                      ","I -4                ",  &
                                               "x+1/2,y+1/2,z+1/2,1;y,-x,-z,1                                                                           ")
     Shubnikov_info( 700)= Shub_Spgr_Info_Type("82.40  ","I-41'       ","82.2.700   ","I-41'         ","I-4.1'                     ","I -4 1'             ",  &
                                               "x+1/2,y+1/2,z+1/2,1;y,-x,-z,1;x,y,z,-1                                                                  ")
     Shubnikov_info( 701)= Shub_Spgr_Info_Type("82.41  ","I-4'        ","82.3.701   ","I-4'          ","I-4'                       ","I -4'               ",  &
                                               "x+1/2,y+1/2,z+1/2,1;y,-x,-z,-1                                                                          ")
     Shubnikov_info( 702)= Shub_Spgr_Info_Type("81.38  ","P_I-4       ","82.4.702   ","I_P-4         ","P-4.1'_I[I-4]              ","P -4 1'I            ",  &
                                               "y,-x,-z,1;x+1/2,y+1/2,z+1/2,-1                                                                          ")
     Shubnikov_info( 703)= Shub_Spgr_Info_Type("83.43  ","P4/m        ","83.1.703   ","P4/m          ","P4/m.1                     ","-P 4                ",  &
                                               "-y,x,z,1;x,y,-z,1                                                                                       ")
     Shubnikov_info( 704)= Shub_Spgr_Info_Type("83.44  ","P4/m1'      ","83.2.704   ","P4/m1'        ","P4/m.1'                    ","-P 4 1'             ",  &
                                               "-y,x,z,1;x,y,-z,1;x,y,z,-1                                                                              ")
     Shubnikov_info( 705)= Shub_Spgr_Info_Type("83.45  ","P4'/m       ","83.3.705   ","P4'/m         ","P4'/m                      ","-P 4'               ",  &
                                               "-y,x,z,-1;x,y,-z,1                                                                                      ")
     Shubnikov_info( 706)= Shub_Spgr_Info_Type("83.46  ","P4/m'       ","83.4.706   ","P4/m'         ","P4/m'                      ","P 4 -1'             ",  &
                                               "-y,x,z,1;x,y,-z,-1                                                                                      ")
     Shubnikov_info( 707)= Shub_Spgr_Info_Type("83.47  ","P4'/m'      ","83.5.707   ","P4'/m'        ","P4'/m'                     ","P 4' -1'            ",  &
                                               "-y,x,z,-1;x,y,-z,-1                                                                                     ")
     Shubnikov_info( 708)= Shub_Spgr_Info_Type("83.48  ","P_c4/m      ","83.6.708   ","P_2c4/m       ","P4/m.1'_c[P4/m]            ","-P 4 1'c            ",  &
                                               "-y,x,z,1;x,y,-z,1;x,y,z+1/2,-1                                                                          ")
     Shubnikov_info( 709)= Shub_Spgr_Info_Type("83.49  ","P_C4/m      ","83.7.709   ","P_P4/m        ","P4/m.1'_C[rP4/m]           ","-P 4 1'C            ",  &
                                               "-y,x,z,1;x,y,-z,1;x+1/2,y+1/2,z,-1                                                                      ")
     Shubnikov_info( 710)= Shub_Spgr_Info_Type("87.80  ","I_c4/m      ","83.8.710   ","P_I4/m        ","I4/m.1'_c[rP4/m]           ","-I 4 1'c            ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,1;x,y,-z,1;x,y,z+1/2,-1                                                      ")
     Shubnikov_info( 711)= Shub_Spgr_Info_Type("84.56  ","P_c4_2/m    ","83.9.711   ","P_2c4'/m      ","P4_2/m.1'_c[P4/m]          ","-P 4c 1'c           ",  &
                                               "-y,x,z+1/2,1;x,y,-z,1;x,y,z+1/2,-1                                                                      ")
     Shubnikov_info( 712)= Shub_Spgr_Info_Type("85.65  ","P_C4/n      ","83.10.712  ","P_P4/m'       ","P4/n.1'_C[rP4/m]           ","-P 4a 1'C           ",  &
                                               "-y+1/2,x,z,1;x+1/2,y+1/2,-z,1;x+1/2,y+1/2,z,-1                                                          ")
     Shubnikov_info( 713)= Shub_Spgr_Info_Type("84.51  ","P4_2/m      ","84.1.713   ","P4_2/m        ","P4_2/m.1                   ","-P 4c               ",  &
                                               "-y,x,z+1/2,1;x,y,-z,1                                                                                   ")
     Shubnikov_info( 714)= Shub_Spgr_Info_Type("84.52  ","P4_2/m1'    ","84.2.714   ","P4_2/m1'      ","P4_2/m.1'                  ","-P 4c 1'            ",  &
                                               "-y,x,z+1/2,1;x,y,-z,1;x,y,z,-1                                                                          ")
     Shubnikov_info( 715)= Shub_Spgr_Info_Type("84.53  ","P4_2'/m     ","84.3.715   ","P4_2'/m       ","P4_2'/m                    ","-P 4c'              ",  &
                                               "-y,x,z+1/2,-1;x,y,-z,1                                                                                  ")
     Shubnikov_info( 716)= Shub_Spgr_Info_Type("84.54  ","P4_2/m'     ","84.4.716   ","P4_2/m'       ","P4_2/m'                    ","P 4c -1'            ",  &
                                               "-y,x,z+1/2,1;x,y,-z,-1                                                                                  ")
     Shubnikov_info( 717)= Shub_Spgr_Info_Type("84.55  ","P4_2'/m'    ","84.5.717   ","P4_2'/m'      ","P4_2'/m'                   ","P 4c' -1'           ",  &
                                               "-y,x,z+1/2,-1;x,y,-z,-1                                                                                 ")
     Shubnikov_info( 718)= Shub_Spgr_Info_Type("84.57  ","P_C4_2/m    ","84.6.718   ","P_P4_2/m      ","P4_2/m.1'_C[rP4_2/m]       ","-P 4c 1'C           ",  &
                                               "-y,x,z+1/2,1;x,y,-z,1;x+1/2,y+1/2,z,-1                                                                  ")
     Shubnikov_info( 719)= Shub_Spgr_Info_Type("86.73  ","P_C4_2/n    ","84.7.719   ","P_P4_2/m'     ","P4_2/n.1'_C[rP4_2/m]       ","-P 4bc 1'C          ",  &
                                               "-y,x+1/2,z+1/2,1;x+1/2,y+1/2,-z,1;x+1/2,y+1/2,z,-1                                                      ")
     Shubnikov_info( 720)= Shub_Spgr_Info_Type("85.59  ","P4/n        ","85.1.720   ","P4/n          ","P4/n.1                     ","-P 4a               ",  &
                                               "-y+1/2,x,z,1;x+1/2,y+1/2,-z,1                                                                           ")
     Shubnikov_info( 721)= Shub_Spgr_Info_Type("85.60  ","P4/n1'      ","85.2.721   ","P4/n1'        ","P4/n.1'                    ","-P 4a 1'            ",  &
                                               "-y+1/2,x,z,1;x+1/2,y+1/2,-z,1;x,y,z,-1                                                                  ")
     Shubnikov_info( 722)= Shub_Spgr_Info_Type("85.61  ","P4'/n       ","85.3.722   ","P4'/n         ","P4'/n                      ","-P 4a'              ",  &
                                               "-y+1/2,x,z,-1;x+1/2,y+1/2,-z,1                                                                          ")
     Shubnikov_info( 723)= Shub_Spgr_Info_Type("85.62  ","P4/n'       ","85.4.723   ","P4/n'         ","P4/n'                      ","P 4a -1'            ",  &
                                               "-y+1/2,x,z,1;x+1/2,y+1/2,-z,-1                                                                          ")
     Shubnikov_info( 724)= Shub_Spgr_Info_Type("85.63  ","P4'/n'      ","85.5.724   ","P4'/n'        ","P4'/n'                     ","P 4a' -1'           ",  &
                                               "-y+1/2,x,z,-1;x+1/2,y+1/2,-z,-1                                                                         ")
     Shubnikov_info( 725)= Shub_Spgr_Info_Type("85.64  ","P_c4/n      ","85.6.725   ","P_2c4/n       ","P4/n.1'_c[P4/n]            ","-P 4a 1'c           ",  &
                                               "-y+1/2,x,z,1;x+1/2,y+1/2,-z,1;x,y,z+1/2,-1                                                              ")
     Shubnikov_info( 726)= Shub_Spgr_Info_Type("86.72  ","P_c4_2/n    ","85.7.726   ","P_2c4'/n      ","P4_2/n.1'_c[P4/n]          ","-P 4bc 1'c          ",  &
                                               "-y,x+1/2,z+1/2,1;x+1/2,y+1/2,-z,1;x,y,z+1/2,-1                                                          ")
     Shubnikov_info( 727)= Shub_Spgr_Info_Type("86.67  ","P4_2/n      ","86.1.727   ","P4_2/n        ","P4_2/n.1                   ","-P 4bc              ",  &
                                               "-y,x+1/2,z+1/2,1;x+1/2,y+1/2,-z,1                                                                       ")
     Shubnikov_info( 728)= Shub_Spgr_Info_Type("86.68  ","P4_2/n1'    ","86.2.728   ","P4_2/n1'      ","P4_2/n.1'                  ","-P 4bc 1'           ",  &
                                               "-y,x+1/2,z+1/2,1;x+1/2,y+1/2,-z,1;x,y,z,-1                                                              ")
     Shubnikov_info( 729)= Shub_Spgr_Info_Type("86.69  ","P4_2'/n     ","86.3.729   ","P4_2'/n       ","P4_2'/n                    ","-P 4bc'             ",  &
                                               "-y,x+1/2,z+1/2,-1;x+1/2,y+1/2,-z,1                                                                      ")
     Shubnikov_info( 730)= Shub_Spgr_Info_Type("86.70  ","P4_2/n'     ","86.4.730   ","P4_2/n'       ","P4_2/n'                    ","P 4bc -1'           ",  &
                                               "-y,x+1/2,z+1/2,1;x+1/2,y+1/2,-z,-1                                                                      ")
     Shubnikov_info( 731)= Shub_Spgr_Info_Type("86.71  ","P4_2'/n'    ","86.5.731   ","P4_2'/n'      ","P4_2'/n'                   ","P 4bc' -1'          ",  &
                                               "-y,x+1/2,z+1/2,-1;x+1/2,y+1/2,-z,-1                                                                     ")
     Shubnikov_info( 732)= Shub_Spgr_Info_Type("88.86  ","I_c4_1/a    ","86.6.732   ","P_I4_2/n      ","I4_1/a.1'_c[rP4_2/n]       ","-I 4ad 1'c          ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y+3/4,x+1/4,z+1/4,1;x,y+1/2,-z,1;x,y,z+1/2,-1                                      ")
     Shubnikov_info( 733)= Shub_Spgr_Info_Type("87.75  ","I4/m        ","87.1.733   ","I4/m          ","I4/m.1                     ","-I 4                ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,1;x,y,-z,1                                                                   ")
     Shubnikov_info( 734)= Shub_Spgr_Info_Type("87.76  ","I4/m1'      ","87.2.734   ","I4/m1'        ","I4/m.1'                    ","-I 4 1'             ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,1;x,y,-z,1;x,y,z,-1                                                          ")
     Shubnikov_info( 735)= Shub_Spgr_Info_Type("87.77  ","I4'/m       ","87.3.735   ","I4'/m         ","I4'/m                      ","-I 4'               ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,-1;x,y,-z,1                                                                  ")
     Shubnikov_info( 736)= Shub_Spgr_Info_Type("87.78  ","I4/m'       ","87.4.736   ","I4/m'         ","I4/m'                      ","I 4 -1'             ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,1;x,y,-z,-1                                                                  ")
     Shubnikov_info( 737)= Shub_Spgr_Info_Type("87.79  ","I4'/m'      ","87.5.737   ","I4'/m'        ","I4'/m'                     ","I 4' -1'            ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,-1;x,y,-z,-1                                                                 ")
     Shubnikov_info( 738)= Shub_Spgr_Info_Type("83.50  ","P_I4/m      ","87.6.738   ","I_P4/m        ","P4/m.1'_I[I4/m]            ","-P 4 1'I            ",  &
                                               "-y,x,z,1;x,y,-z,1;x+1/2,y+1/2,z+1/2,-1                                                                  ")
     Shubnikov_info( 739)= Shub_Spgr_Info_Type("84.58  ","P_I4_2/m    ","87.7.739   ","I_P4'/m       ","P4_2/m.1'_I[I4/m]          ","-P 4c 1'I           ",  &
                                               "-y,x,z+1/2,1;x,y,-z,1;x+1/2,y+1/2,z+1/2,-1                                                              ")
     Shubnikov_info( 740)= Shub_Spgr_Info_Type("85.66  ","P_I4/n      ","87.8.740   ","I_P4/m'       ","P4/n.1'_I[I4/m]            ","-P 4a 1'I           ",  &
                                               "-y+1/2,x,z,1;x+1/2,y+1/2,-z,1;x+1/2,y+1/2,z+1/2,-1                                                      ")
     Shubnikov_info( 741)= Shub_Spgr_Info_Type("86.74  ","P_I4_2/n    ","87.9.741   ","I_P4'/m'      ","P4_2/n.1'_I[I4/m]          ","-P 4bc 1'I          ",  &
                                               "-y,x+1/2,z+1/2,1;x+1/2,y+1/2,-z,1;x+1/2,y+1/2,z+1/2,-1                                                  ")
     Shubnikov_info( 742)= Shub_Spgr_Info_Type("88.81  ","I4_1/a      ","88.1.742   ","I4_1/a        ","I4_1/a.1                   ","-I 4ad              ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y+3/4,x+1/4,z+1/4,1;x,y+1/2,-z,1                                                   ")
     Shubnikov_info( 743)= Shub_Spgr_Info_Type("88.82  ","I4_1/a1'    ","88.2.743   ","I4_1/a1'      ","I4_1/a.1'                  ","-I 4ad 1'           ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y+3/4,x+1/4,z+1/4,1;x,y+1/2,-z,1;x,y,z,-1                                          ")
     Shubnikov_info( 744)= Shub_Spgr_Info_Type("88.83  ","I4_1'/a     ","88.3.744   ","I4_1'/a       ","I4_1'/a                    ","-I 4ad'             ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y+3/4,x+1/4,z+1/4,-1;x,y+1/2,-z,1                                                  ")
     Shubnikov_info( 745)= Shub_Spgr_Info_Type("88.84  ","I4_1/a'     ","88.4.745   ","I4_1/a'       ","I4_1/a'                    ","I 4ad -1'           ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y+3/4,x+1/4,z+1/4,1;x,y+1/2,-z,-1                                                  ")
     Shubnikov_info( 746)= Shub_Spgr_Info_Type("88.85  ","I4_1'/a'    ","88.5.746   ","I4_1'/a'      ","I4_1'/a'                   ","I 4ad' -1'          ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y+3/4,x+1/4,z+1/4,-1;x,y+1/2,-z,-1                                                 ")
     Shubnikov_info( 747)= Shub_Spgr_Info_Type("89.87  ","P422        ","89.1.747   ","P422          ","P422.1                     ","P 4 2               ",  &
                                               "-y,x,z,1;x,-y,-z,1;-y,-x,-z,1                                                                           ")
     Shubnikov_info( 748)= Shub_Spgr_Info_Type("89.88  ","P4221'      ","89.2.748   ","P4221'        ","P422.1'                    ","P 4 2 1'            ",  &
                                               "-y,x,z,1;x,-y,-z,1;-y,-x,-z,1;x,y,z,-1                                                                  ")
     Shubnikov_info( 749)= Shub_Spgr_Info_Type("89.89  ","P4'22'      ","89.3.749   ","P4'22'        ","P4'22'                     ","P 4' 2              ",  &
                                               "-y,x,z,-1;x,-y,-z,1;-y,-x,-z,-1                                                                         ")
     Shubnikov_info( 750)= Shub_Spgr_Info_Type("89.90  ","P42'2'      ","89.4.750   ","P42'2'        ","P42'2'                     ","P 4 2'              ",  &
                                               "-y,x,z,1;x,-y,-z,-1;-y,-x,-z,-1                                                                         ")
     Shubnikov_info( 751)= Shub_Spgr_Info_Type("89.91  ","P4'2'2      ","89.5.751   ","P4'2'2        ","P4'2'2                     ","P 4' 2'             ",  &
                                               "-y,x,z,-1;x,-y,-z,-1;-y,-x,-z,1                                                                         ")
     Shubnikov_info( 752)= Shub_Spgr_Info_Type("89.92  ","P_c422      ","89.6.752   ","P_2c422       ","P422.1'_c[P422]            ","P 4 2 1'c           ",  &
                                               "-y,x,z,1;x,-y,-z,1;-y,-x,-z,1;x,y,z+1/2,-1                                                              ")
     Shubnikov_info( 753)= Shub_Spgr_Info_Type("89.93  ","P_C422      ","89.7.753   ","P_P422        ","P422.1'_C[rP422]           ","P 4 2 1'C           ",  &
                                               "-y,x,z,1;x,-y,-z,1;-y,-x,-z,1;x+1/2,y+1/2,z,-1                                                          ")
     Shubnikov_info( 754)= Shub_Spgr_Info_Type("97.156 ","I_c422      ","89.8.754   ","P_I422        ","I422.1'_c[rP422]           ","I 4 2 1'c           ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,1;x,-y,-z,1;-y,-x,-z,1;x,y,z+1/2,-1                                          ")
     Shubnikov_info( 755)= Shub_Spgr_Info_Type("93.124 ","P_c4_222    ","89.9.755   ","P_2c4'22'     ","P4_222.1'_c[P422]          ","P 4c 2 1'c          ",  &
                                               "-y,x,z+1/2,1;x,-y,-z,1;-y,-x,-z+1/2,1;x,y,z+1/2,-1                                                      ")
     Shubnikov_info( 756)= Shub_Spgr_Info_Type("90.101 ","P_C42_12    ","89.10.756  ","P_P4'22'      ","P42_12.1'_C[rP422]         ","P 4ab 2ab 1'C       ",  &
                                               "-y+1/2,x+1/2,z,1;x+1/2,-y+1/2,-z,1;-y,-x,-z,1;x+1/2,y+1/2,z,-1                                          ")
     Shubnikov_info( 757)= Shub_Spgr_Info_Type("90.95  ","P42_12      ","90.1.757   ","P42_12        ","P42_12.1                   ","P 4ab 2ab           ",  &
                                               "-y+1/2,x+1/2,z,1;x+1/2,-y+1/2,-z,1;-y,-x,-z,1                                                           ")
     Shubnikov_info( 758)= Shub_Spgr_Info_Type("90.96  ","P42_121'    ","90.2.758   ","P42_121'      ","P42_12.1'                  ","P 4ab 2ab 1'        ",  &
                                               "-y+1/2,x+1/2,z,1;x+1/2,-y+1/2,-z,1;-y,-x,-z,1;x,y,z,-1                                                  ")
     Shubnikov_info( 759)= Shub_Spgr_Info_Type("90.97  ","P4'2_12'    ","90.3.759   ","P4'2_12'      ","P4'2_12'                   ","P 4ab' 2ab          ",  &
                                               "-y+1/2,x+1/2,z,-1;x+1/2,-y+1/2,-z,1;-y,-x,-z,-1                                                         ")
     Shubnikov_info( 760)= Shub_Spgr_Info_Type("90.98  ","P42_1'2'    ","90.4.760   ","P42_1'2'      ","P42_1'2'                   ","P 4ab 2ab'          ",  &
                                               "-y+1/2,x+1/2,z,1;x+1/2,-y+1/2,-z,-1;-y,-x,-z,-1                                                         ")
     Shubnikov_info( 761)= Shub_Spgr_Info_Type("90.99  ","P4'2_1'2    ","90.5.761   ","P4'2_1'2      ","P4'2_1'2                   ","P 4ab' 2ab'         ",  &
                                               "-y+1/2,x+1/2,z,-1;x+1/2,-y+1/2,-z,-1;-y,-x,-z,1                                                         ")
     Shubnikov_info( 762)= Shub_Spgr_Info_Type("90.100 ","P_c42_12    ","90.6.762   ","P_2c42_12     ","P42_12.1'_c[P42_12]        ","P 4ab 2ab 1'c       ",  &
                                               "-y+1/2,x+1/2,z,1;x+1/2,-y+1/2,-z,1;-y,-x,-z,1;x,y,z+1/2,-1                                              ")
     Shubnikov_info( 763)= Shub_Spgr_Info_Type("94.132 ","P_c4_22_12  ","90.7.763   ","P_2c4'2_1'2   ","P4_22_12.1'_c[P42_12]      ","P 4n 2n 1'c         ",  &
                                               "-y+1/2,x+1/2,z+1/2,1;x+1/2,-y+1/2,-z+1/2,1;-y,-x,-z,1;x,y,z+1/2,-1                                      ")
     Shubnikov_info( 764)= Shub_Spgr_Info_Type("91.103 ","P4_122      ","91.1.764   ","P4_122        ","P4_122.1                   ","P 4w 2c             ",  &
                                               "-y,x,z+1/4,1;x,-y,-z+1/2,1;-y,-x,-z+1/4,1                                                               ")
     Shubnikov_info( 765)= Shub_Spgr_Info_Type("91.104 ","P4_1221'    ","91.2.765   ","P4_1221'      ","P4_122.1'                  ","P 4w 2c 1'          ",  &
                                               "-y,x,z+1/4,1;x,-y,-z+1/2,1;-y,-x,-z+1/4,1;x,y,z,-1                                                      ")
     Shubnikov_info( 766)= Shub_Spgr_Info_Type("91.105 ","P4_1'22'    ","91.3.766   ","P4_1'22'      ","P4_1'22'                   ","P 4w' 2c            ",  &
                                               "-y,x,z+1/4,-1;x,-y,-z+1/2,1;-y,-x,-z+1/4,-1                                                             ")
     Shubnikov_info( 767)= Shub_Spgr_Info_Type("91.106 ","P4_12'2'    ","91.4.767   ","P4_12'2'      ","P4_12'2'                   ","P 4w 2c'            ",  &
                                               "-y,x,z+1/4,1;x,-y,-z+1/2,-1;-y,-x,-z+1/4,-1                                                             ")
     Shubnikov_info( 768)= Shub_Spgr_Info_Type("91.107 ","P4_1'2'2    ","91.5.768   ","P4_1'2'2      ","P4_1'2'2                   ","P 4w' 2c'           ",  &
                                               "-y,x,z+1/4,-1;x,-y,-z+1/2,-1;-y,-x,-z+1/4,1                                                             ")
     Shubnikov_info( 769)= Shub_Spgr_Info_Type("91.109 ","P_C4_122    ","91.6.769   ","P_P4_122      ","P4_122.1'_C[rP4_122]       ","P 4w 2c 1'C         ",  &
                                               "-y,x,z+1/4,1;x,-y,-z+1/2,1;-y,-x,-z+1/4,1;x+1/2,y+1/2,z,-1                                              ")
     Shubnikov_info( 770)= Shub_Spgr_Info_Type("92.117 ","P_C4_12_12  ","91.7.770   ","P_P4_1'22'    ","P4_12_12.1'_C[rP4_122]     ","P 4abw 2nw 1'C      ",  &
                                               "-y+1/2,x+1/2,z+1/4,1;x+1/2,-y+1/2,-z+3/4,1;-y,-x,-z+1/2,1;x+1/2,y+1/2,z,-1                              ")
     Shubnikov_info( 771)= Shub_Spgr_Info_Type("92.111 ","P4_12_12    ","92.1.771   ","P4_12_12      ","P4_12_12.1                 ","P 4abw 2nw          ",  &
                                               "-y+1/2,x+1/2,z+1/4,1;x+1/2,-y+1/2,-z+3/4,1;-y,-x,-z+1/2,1                                               ")
     Shubnikov_info( 772)= Shub_Spgr_Info_Type("92.112 ","P4_12_121'  ","92.2.772   ","P4_12_121'    ","P4_12_12.1'                ","P 4abw 2nw 1'       ",  &
                                               "-y+1/2,x+1/2,z+1/4,1;x+1/2,-y+1/2,-z+3/4,1;-y,-x,-z+1/2,1;x,y,z,-1                                      ")
     Shubnikov_info( 773)= Shub_Spgr_Info_Type("92.113 ","P4_1'2_12'  ","92.3.773   ","P4_1'2_12'    ","P4_1'2_12'                 ","P 4abw' 2nw         ",  &
                                               "-y+1/2,x+1/2,z+1/4,-1;x+1/2,-y+1/2,-z+3/4,1;-y,-x,-z+1/2,-1                                             ")
     Shubnikov_info( 774)= Shub_Spgr_Info_Type("92.114 ","P4_12_1'2'  ","92.4.774   ","P4_12_1'2'    ","P4_12_1'2'                 ","P 4abw 2nw'         ",  &
                                               "-y+1/2,x+1/2,z+1/4,1;x+1/2,-y+1/2,-z+3/4,-1;-y,-x,-z+1/2,-1                                             ")
     Shubnikov_info( 775)= Shub_Spgr_Info_Type("92.115 ","P4_1'2_1'2  ","92.5.775   ","P4_1'2_1'2    ","P4_1'2_1'2                 ","P 4abw' 2nw'        ",  &
                                               "-y+1/2,x+1/2,z+1/4,-1;x+1/2,-y+1/2,-z+3/4,-1;-y,-x,-z+1/2,1                                             ")
     Shubnikov_info( 776)= Shub_Spgr_Info_Type("93.119 ","P4_222      ","93.1.776   ","P4_222        ","P4_222.1                   ","P 4c 2              ",  &
                                               "-y,x,z+1/2,1;x,-y,-z,1;-y,-x,-z+1/2,1                                                                   ")
     Shubnikov_info( 777)= Shub_Spgr_Info_Type("93.120 ","P4_2221'    ","93.2.777   ","P4_2221'      ","P4_222.1'                  ","P 4c 2 1'           ",  &
                                               "-y,x,z+1/2,1;x,-y,-z,1;-y,-x,-z+1/2,1;x,y,z,-1                                                          ")
     Shubnikov_info( 778)= Shub_Spgr_Info_Type("93.121 ","P4_2'22'    ","93.3.778   ","P4_2'22'      ","P4_2'22'                   ","P 4c' 2             ",  &
                                               "-y,x,z+1/2,-1;x,-y,-z,1;-y,-x,-z+1/2,-1                                                                 ")
     Shubnikov_info( 779)= Shub_Spgr_Info_Type("93.122 ","P4_22'2'    ","93.4.779   ","P4_22'2'      ","P4_22'2'                   ","P 4c 2'             ",  &
                                               "-y,x,z+1/2,1;x,-y,-z,-1;-y,-x,-z+1/2,-1                                                                 ")
     Shubnikov_info( 780)= Shub_Spgr_Info_Type("93.123 ","P4_2'2'2    ","93.5.780   ","P4_2'2'2      ","P4_2'2'2                   ","P 4c' 2'            ",  &
                                               "-y,x,z+1/2,-1;x,-y,-z,-1;-y,-x,-z+1/2,1                                                                 ")
     Shubnikov_info( 781)= Shub_Spgr_Info_Type("91.108 ","P_c4_122    ","93.6.781   ","P_2c4_222     ","P4_122.1'_c[P4_222]        ","P 4w 2c 1'c         ",  &
                                               "-y,x,z+1/4,1;x,-y,-z+1/2,1;-y,-x,-z+1/4,1;x,y,z+1/2,-1                                                  ")
     Shubnikov_info( 782)= Shub_Spgr_Info_Type("93.125 ","P_C4_222    ","93.7.782   ","P_P4_222      ","P4_222.1'_C[rP4_222]       ","P 4c 2 1'C          ",  &
                                               "-y,x,z+1/2,1;x,-y,-z,1;-y,-x,-z+1/2,1;x+1/2,y+1/2,z,-1                                                  ")
     Shubnikov_info( 783)= Shub_Spgr_Info_Type("98.162 ","I_c4_122    ","93.8.783   ","P_I4_222      ","I4_122.1'_c[rP4_222]       ","I 4bw 2bw 1'c       ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x+1/2,z+1/4,1;x,-y+1/2,-z+1/4,1;-y,-x,-z,1;x,y,z+1/2,-1                          ")
     Shubnikov_info( 784)= Shub_Spgr_Info_Type("95.140 ","P_c4_322    ","93.9.784   ","P_2c4_2'22'   ","P4_322.1'_c[P4_222]        ","P 4cw 2c 1'c        ",  &
                                               "-y,x,z+3/4,1;x,-y,-z+1/2,1;-y,-x,-z+3/4,1;x,y,z+1/2,-1                                                  ")
     Shubnikov_info( 785)= Shub_Spgr_Info_Type("94.133 ","P_C4_22_12  ","93.10.785  ","P_P4_2'22'    ","P4_22_12.1'_C[rP4_222]     ","P 4n 2n 1'C         ",  &
                                               "-y+1/2,x+1/2,z+1/2,1;x+1/2,-y+1/2,-z+1/2,1;-y,-x,-z,1;x+1/2,y+1/2,z,-1                                  ")
     Shubnikov_info( 786)= Shub_Spgr_Info_Type("94.127 ","P4_22_12    ","94.1.786   ","P4_22_12      ","P4_22_12.1                 ","P 4n 2n             ",  &
                                               "-y+1/2,x+1/2,z+1/2,1;x+1/2,-y+1/2,-z+1/2,1;-y,-x,-z,1                                                   ")
     Shubnikov_info( 787)= Shub_Spgr_Info_Type("94.128 ","P4_22_121'  ","94.2.787   ","P4_22_121'    ","P4_22_12.1'                ","P 4n 2n 1'          ",  &
                                               "-y+1/2,x+1/2,z+1/2,1;x+1/2,-y+1/2,-z+1/2,1;-y,-x,-z,1;x,y,z,-1                                          ")
     Shubnikov_info( 788)= Shub_Spgr_Info_Type("94.129 ","P4_2'2_12'  ","94.3.788   ","P4_2'2_12'    ","P4_2'2_12'                 ","P 4n' 2n            ",  &
                                               "-y+1/2,x+1/2,z+1/2,-1;x+1/2,-y+1/2,-z+1/2,1;-y,-x,-z,-1                                                 ")
     Shubnikov_info( 789)= Shub_Spgr_Info_Type("94.130 ","P4_22_1'2'  ","94.4.789   ","P4_22_1'2'    ","P4_22_1'2'                 ","P 4n 2n'            ",  &
                                               "-y+1/2,x+1/2,z+1/2,1;x+1/2,-y+1/2,-z+1/2,-1;-y,-x,-z,-1                                                 ")
     Shubnikov_info( 790)= Shub_Spgr_Info_Type("94.131 ","P4_2'2_1'2  ","94.5.790   ","P4_2'2_1'2    ","P4_2'2_1'2                 ","P 4n' 2n'           ",  &
                                               "-y+1/2,x+1/2,z+1/2,-1;x+1/2,-y+1/2,-z+1/2,-1;-y,-x,-z,1                                                 ")
     Shubnikov_info( 791)= Shub_Spgr_Info_Type("92.116 ","P_c4_12_12  ","94.6.791   ","P_2c4_22_12   ","P4_12_12.1'_c[P4_22_12]    ","P 4abw 2nw 1'c      ",  &
                                               "-y+1/2,x+1/2,z+1/4,1;x+1/2,-y+1/2,-z+3/4,1;-y,-x,-z+1/2,1;x,y,z+1/2,-1                                  ")
     Shubnikov_info( 792)= Shub_Spgr_Info_Type("96.148 ","P_c4_32_12  ","94.7.792   ","P_2c4_2'2_1'2 ","P4_32_12.1'_c[P4_22_12]    ","P 4nw 2abw 1'c      ",  &
                                               "-y+1/2,x+1/2,z+3/4,1;x+1/2,-y+1/2,-z+1/4,1;-y,-x,-z+1/2,1;x,y,z+1/2,-1                                  ")
     Shubnikov_info( 793)= Shub_Spgr_Info_Type("95.135 ","P4_322      ","95.1.793   ","P4_322        ","P4_322.1                   ","P 4cw 2c            ",  &
                                               "-y,x,z+3/4,1;x,-y,-z+1/2,1;-y,-x,-z+3/4,1                                                               ")
     Shubnikov_info( 794)= Shub_Spgr_Info_Type("95.136 ","P4_3221'    ","95.2.794   ","P4_3221'      ","P4_322.1'                  ","P 4cw 2c 1'         ",  &
                                               "-y,x,z+3/4,1;x,-y,-z+1/2,1;-y,-x,-z+3/4,1;x,y,z,-1                                                      ")
     Shubnikov_info( 795)= Shub_Spgr_Info_Type("95.137 ","P4_3'22'    ","95.3.795   ","P4_3'22'      ","P4_3'22'                   ","P 4cw' 2c           ",  &
                                               "-y,x,z+3/4,-1;x,-y,-z+1/2,1;-y,-x,-z+3/4,-1                                                             ")
     Shubnikov_info( 796)= Shub_Spgr_Info_Type("95.138 ","P4_32'2'    ","95.4.796   ","P4_32'2'      ","P4_32'2'                   ","P 4cw 2c'           ",  &
                                               "-y,x,z+3/4,1;x,-y,-z+1/2,-1;-y,-x,-z+3/4,-1                                                             ")
     Shubnikov_info( 797)= Shub_Spgr_Info_Type("95.139 ","P4_3'2'2    ","95.5.797   ","P4_3'2'2      ","P4_3'2'2                   ","P 4cw' 2c'          ",  &
                                               "-y,x,z+3/4,-1;x,-y,-z+1/2,-1;-y,-x,-z+3/4,1                                                             ")
     Shubnikov_info( 798)= Shub_Spgr_Info_Type("95.141 ","P_C4_322    ","95.6.798   ","P_P4_322      ","P4_322.1'_C[rP4_322]       ","P 4cw 2c 1'C        ",  &
                                               "-y,x,z+3/4,1;x,-y,-z+1/2,1;-y,-x,-z+3/4,1;x+1/2,y+1/2,z,-1                                              ")
     Shubnikov_info( 799)= Shub_Spgr_Info_Type("96.149 ","P_C4_32_12  ","95.7.799   ","P_P4_3'22'    ","P4_32_12.1'_C[rP4_322]     ","P 4nw 2abw 1'C      ",  &
                                               "-y+1/2,x+1/2,z+3/4,1;x+1/2,-y+1/2,-z+1/4,1;-y,-x,-z+1/2,1;x+1/2,y+1/2,z,-1                              ")
     Shubnikov_info( 800)= Shub_Spgr_Info_Type("96.143 ","P4_32_12    ","96.1.800   ","P4_32_12      ","P4_32_12.1                 ","P 4nw 2abw          ",  &
                                               "-y+1/2,x+1/2,z+3/4,1;x+1/2,-y+1/2,-z+1/4,1;-y,-x,-z+1/2,1                                               ")
     Shubnikov_info( 801)= Shub_Spgr_Info_Type("96.144 ","P4_32_121'  ","96.2.801   ","P4_32_121'    ","P4_32_12.1'                ","P 4nw 2abw 1'       ",  &
                                               "-y+1/2,x+1/2,z+3/4,1;x+1/2,-y+1/2,-z+1/4,1;-y,-x,-z+1/2,1;x,y,z,-1                                      ")
     Shubnikov_info( 802)= Shub_Spgr_Info_Type("96.145 ","P4_3'2_12'  ","96.3.802   ","P4_3'2_12'    ","P4_3'2_12'                 ","P 4nw' 2abw         ",  &
                                               "-y+1/2,x+1/2,z+3/4,-1;x+1/2,-y+1/2,-z+1/4,1;-y,-x,-z+1/2,-1                                             ")
     Shubnikov_info( 803)= Shub_Spgr_Info_Type("96.146 ","P4_32_1'2'  ","96.4.803   ","P4_32_1'2'    ","P4_32_1'2'                 ","P 4nw 2abw'         ",  &
                                               "-y+1/2,x+1/2,z+3/4,1;x+1/2,-y+1/2,-z+1/4,-1;-y,-x,-z+1/2,-1                                             ")
     Shubnikov_info( 804)= Shub_Spgr_Info_Type("96.147 ","P4_3'2_1'2  ","96.5.804   ","P4_3'2_1'2    ","P4_3'2_1'2                 ","P 4nw' 2abw'        ",  &
                                               "-y+1/2,x+1/2,z+3/4,-1;x+1/2,-y+1/2,-z+1/4,-1;-y,-x,-z+1/2,1                                             ")
     Shubnikov_info( 805)= Shub_Spgr_Info_Type("97.151 ","I422        ","97.1.805   ","I422          ","I422.1                     ","I 4 2               ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,1;x,-y,-z,1;-y,-x,-z,1                                                       ")
     Shubnikov_info( 806)= Shub_Spgr_Info_Type("97.152 ","I4221'      ","97.2.806   ","I4221'        ","I422.1'                    ","I 4 2 1'            ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,1;x,-y,-z,1;-y,-x,-z,1;x,y,z,-1                                              ")
     Shubnikov_info( 807)= Shub_Spgr_Info_Type("97.153 ","I4'22'      ","97.3.807   ","I4'22'        ","I4'22'                     ","I 4' 2              ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,-1;x,-y,-z,1;-y,-x,-z,-1                                                     ")
     Shubnikov_info( 808)= Shub_Spgr_Info_Type("97.154 ","I42'2'      ","97.4.808   ","I42'2'        ","I42'2'                     ","I 4 2'              ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,1;x,-y,-z,-1;-y,-x,-z,-1                                                     ")
     Shubnikov_info( 809)= Shub_Spgr_Info_Type("97.155 ","I4'2'2      ","97.5.809   ","I4'2'2        ","I4'2'2                     ","I 4' 2'             ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,-1;x,-y,-z,-1;-y,-x,-z,1                                                     ")
     Shubnikov_info( 810)= Shub_Spgr_Info_Type("89.94  ","P_I422      ","97.6.810   ","I_P422        ","P422.1'_I[I422]            ","P 4 2 1'I           ",  &
                                               "-y,x,z,1;x,-y,-z,1;-y,-x,-z,1;x+1/2,y+1/2,z+1/2,-1                                                      ")
     Shubnikov_info( 811)= Shub_Spgr_Info_Type("93.126 ","P_I4_222    ","97.7.811   ","I_P4'22'      ","P4_222.1'_I[I422]          ","P 4c 2 1'I          ",  &
                                               "-y,x,z+1/2,1;x,-y,-z,1;-y,-x,-z+1/2,1;x+1/2,y+1/2,z+1/2,-1                                              ")
     Shubnikov_info( 812)= Shub_Spgr_Info_Type("90.102 ","P_I42_12    ","97.8.812   ","I_P42'2'      ","P42_12.1'_I[I422]          ","P 4ab 2ab 1'I       ",  &
                                               "-y+1/2,x+1/2,z,1;x+1/2,-y+1/2,-z,1;-y,-x,-z,1;x+1/2,y+1/2,z+1/2,-1                                      ")
     Shubnikov_info( 813)= Shub_Spgr_Info_Type("94.134 ","P_I4_22_12  ","97.9.813   ","I_P4'2'2      ","P4_22_12.1'_I[I422]        ","P 4n 2n 1'I         ",  &
                                               "-y+1/2,x+1/2,z+1/2,1;x+1/2,-y+1/2,-z+1/2,1;-y,-x,-z,1;x+1/2,y+1/2,z+1/2,-1                              ")
     Shubnikov_info( 814)= Shub_Spgr_Info_Type("98.157 ","I4_122      ","98.1.814   ","I4_122        ","I4_122.1                   ","I 4bw 2bw           ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x+1/2,z+1/4,1;x,-y+1/2,-z+1/4,1;-y,-x,-z,1                                       ")
     Shubnikov_info( 815)= Shub_Spgr_Info_Type("98.158 ","I4_1221'    ","98.2.815   ","I4_1221'      ","I4_122.1'                  ","I 4bw 2bw 1'        ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x+1/2,z+1/4,1;x,-y+1/2,-z+1/4,1;-y,-x,-z,1;x,y,z,-1                              ")
     Shubnikov_info( 816)= Shub_Spgr_Info_Type("98.159 ","I4_1'22'    ","98.3.816   ","I4_1'22'      ","I4_1'22'                   ","I 4bw' 2bw          ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x+1/2,z+1/4,-1;x,-y+1/2,-z+1/4,1;-y,-x,-z,-1                                     ")
     Shubnikov_info( 817)= Shub_Spgr_Info_Type("98.160 ","I4_12'2'    ","98.4.817   ","I4_12'2'      ","I4_12'2'                   ","I 4bw 2bw'          ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x+1/2,z+1/4,1;x,-y+1/2,-z+1/4,-1;-y,-x,-z,-1                                     ")
     Shubnikov_info( 818)= Shub_Spgr_Info_Type("98.161 ","I4_1'2'2    ","98.5.818   ","I4_1'2'2      ","I4_1'2'2                   ","I 4bw' 2bw'         ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x+1/2,z+1/4,-1;x,-y+1/2,-z+1/4,-1;-y,-x,-z,1                                     ")
     Shubnikov_info( 819)= Shub_Spgr_Info_Type("91.110 ","P_I4_122    ","98.6.819   ","I_P4_122      ","P4_122.1'_I[I4_122]        ","P 4w 2c 1'I         ",  &
                                               "-y,x,z+1/4,1;x,-y,-z+1/2,1;-y,-x,-z+1/4,1;x+1/2,y+1/2,z+1/2,-1                                          ")
     Shubnikov_info( 820)= Shub_Spgr_Info_Type("95.142 ","P_I4_322    ","98.7.820   ","I_P4_1'22'    ","P4_322.1'_I[I4_122]        ","P 4cw 2c 1'I        ",  &
                                               "-y,x,z+3/4,1;x,-y,-z+1/2,1;-y,-x,-z+3/4,1;x+1/2,y+1/2,z+1/2,-1                                          ")
     Shubnikov_info( 821)= Shub_Spgr_Info_Type("92.118 ","P_I4_12_12  ","98.8.821   ","I_P4_12'2'    ","P4_12_12.1'_I[I4_122]      ","P 4abw 2nw 1'I      ",  &
                                               "-y+1/2,x+1/2,z+1/4,1;x+1/2,-y+1/2,-z+3/4,1;-y,-x,-z+1/2,1;x+1/2,y+1/2,z+1/2,-1                          ")
     Shubnikov_info( 822)= Shub_Spgr_Info_Type("96.150 ","P_I4_32_12  ","98.9.822   ","I_P4_1'2'2    ","P4_32_12.1'_I[I4_122]      ","P 4nw 2abw 1'I      ",  &
                                               "-y+1/2,x+1/2,z+3/4,1;x+1/2,-y+1/2,-z+1/4,1;-y,-x,-z+1/2,1;x+1/2,y+1/2,z+1/2,-1                          ")
     Shubnikov_info( 823)= Shub_Spgr_Info_Type("99.163 ","P4mm        ","99.1.823   ","P4mm          ","P4mm.1                     ","P 4 -2              ",  &
                                               "-y,x,z,1;-x,y,z,1;y,x,z,1                                                                               ")
     Shubnikov_info( 824)= Shub_Spgr_Info_Type("99.164 ","P4mm1'      ","99.2.824   ","P4mm1'        ","P4mm.1'                    ","P 4 -2 1'           ",  &
                                               "-y,x,z,1;-x,y,z,1;y,x,z,1;x,y,z,-1                                                                      ")
     Shubnikov_info( 825)= Shub_Spgr_Info_Type("99.165 ","P4'm'm      ","99.3.825   ","P4'm'm        ","P4'm'm                     ","P 4' -2'            ",  &
                                               "-y,x,z,-1;-x,y,z,-1;y,x,z,1                                                                             ")
     Shubnikov_info( 826)= Shub_Spgr_Info_Type("99.166 ","P4'mm'      ","99.4.826   ","P4'mm'        ","P4'mm'                     ","P 4' -2             ",  &
                                               "-y,x,z,-1;-x,y,z,1;y,x,z,-1                                                                             ")
     Shubnikov_info( 827)= Shub_Spgr_Info_Type("99.167 ","P4m'm'      ","99.5.827   ","P4m'm'        ","P4m'm'                     ","P 4 -2'             ",  &
                                               "-y,x,z,1;-x,y,z,-1;y,x,z,-1                                                                             ")
     Shubnikov_info( 828)= Shub_Spgr_Info_Type("99.168 ","P_c4mm      ","99.6.828   ","P_2c4mm       ","P4mm.1'_c[P4mm]            ","P 4 -2 1'c          ",  &
                                               "-y,x,z,1;-x,y,z,1;y,x,z,1;x,y,z+1/2,-1                                                                  ")
     Shubnikov_info( 829)= Shub_Spgr_Info_Type("99.169 ","P_C4mm      ","99.7.829   ","P_P4mm        ","P4mm.1'_C[rP4mm]           ","P 4 -2 1'C          ",  &
                                               "-y,x,z,1;-x,y,z,1;y,x,z,1;x+1/2,y+1/2,z,-1                                                              ")
     Shubnikov_info( 830)= Shub_Spgr_Info_Type("107.232","I_c4mm      ","99.8.830   ","P_I4mm        ","I4mm.1'_c[rP4mm]           ","I 4 -2 1'c          ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,1;-x,y,z,1;y,x,z,1;x,y,z+1/2,-1                                              ")
     Shubnikov_info( 831)= Shub_Spgr_Info_Type("101.184","P_c4_2cm    ","99.9.831   ","P_2c4'm'm     ","P4_2cm.1'_c[P4mm]          ","P 4c -2c 1'c        ",  &
                                               "-y,x,z+1/2,1;-x,y,z+1/2,1;y,x,z,1;x,y,z+1/2,-1                                                          ")
     Shubnikov_info( 832)= Shub_Spgr_Info_Type("105.216","P_c4_2mc    ","99.10.832  ","P_2c4'mm'     ","P4_2mc.1'_c[P4mm]          ","P 4c -2 1'c         ",  &
                                               "-y,x,z+1/2,1;-x,y,z,1;y,x,z+1/2,1;x,y,z+1/2,-1                                                          ")
     Shubnikov_info( 833)= Shub_Spgr_Info_Type("103.200","P_c4cc      ","99.11.833  ","P_2c4m'm'     ","P4cc.1'_c[P4mm]            ","P 4 -2c 1'c         ",  &
                                               "-y,x,z,1;-x,y,z+1/2,1;y,x,z+1/2,1;x,y,z+1/2,-1                                                          ")
     Shubnikov_info( 834)= Shub_Spgr_Info_Type("100.177","P_C4bm      ","99.12.834  ","P_P4'mm'      ","P4bm.1'_C[rP4mm]           ","P 4 -2ab 1'C        ",  &
                                               "-y,x,z,1;-x+1/2,y+1/2,z,1;y+1/2,x+1/2,z,1;x+1/2,y+1/2,z,-1                                              ")
     Shubnikov_info( 835)= Shub_Spgr_Info_Type("108.238","I_c4cm      ","99.13.835  ","P_I4m'm'      ","I4cm.1'_c[rP4mm]           ","I 4 -2c 1'c         ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,1;-x,y,z+1/2,1;y,x,z+1/2,1;x,y,z+1/2,-1                                      ")
     Shubnikov_info( 836)= Shub_Spgr_Info_Type("100.171","P4bm        ","100.1.836  ","P4bm          ","P4bm.1                     ","P 4 -2ab            ",  &
                                               "-y,x,z,1;-x+1/2,y+1/2,z,1;y+1/2,x+1/2,z,1                                                               ")
     Shubnikov_info( 837)= Shub_Spgr_Info_Type("100.172","P4bm1'      ","100.2.837  ","P4bm1'        ","P4bm.1'                    ","P 4 -2ab 1'         ",  &
                                               "-y,x,z,1;-x+1/2,y+1/2,z,1;y+1/2,x+1/2,z,1;x,y,z,-1                                                      ")
     Shubnikov_info( 838)= Shub_Spgr_Info_Type("100.173","P4'b'm      ","100.3.838  ","P4'b'm        ","P4'b'm                     ","P 4' -2ab'          ",  &
                                               "-y,x,z,-1;-x+1/2,y+1/2,z,-1;y+1/2,x+1/2,z,1                                                             ")
     Shubnikov_info( 839)= Shub_Spgr_Info_Type("100.174","P4'bm'      ","100.4.839  ","P4'bm'        ","P4'bm'                     ","P 4' -2ab           ",  &
                                               "-y,x,z,-1;-x+1/2,y+1/2,z,1;y+1/2,x+1/2,z,-1                                                             ")
     Shubnikov_info( 840)= Shub_Spgr_Info_Type("100.175","P4b'm'      ","100.5.840  ","P4b'm'        ","P4b'm'                     ","P 4 -2ab'           ",  &
                                               "-y,x,z,1;-x+1/2,y+1/2,z,-1;y+1/2,x+1/2,z,-1                                                             ")
     Shubnikov_info( 841)= Shub_Spgr_Info_Type("100.176","P_c4bm      ","100.6.841  ","P_2c4bm       ","P4bm.1'_c[P4bm]            ","P 4 -2ab 1'c        ",  &
                                               "-y,x,z,1;-x+1/2,y+1/2,z,1;y+1/2,x+1/2,z,1;x,y,z+1/2,-1                                                  ")
     Shubnikov_info( 842)= Shub_Spgr_Info_Type("102.192","P_c4_2nm    ","100.7.842  ","P_2c4'b'm     ","P4_2nm.1'_c[P4bm]          ","P 4n -2n 1'c        ",  &
                                               "-y+1/2,x+1/2,z+1/2,1;-x+1/2,y+1/2,z+1/2,1;y,x,z,1;x,y,z+1/2,-1                                          ")
     Shubnikov_info( 843)= Shub_Spgr_Info_Type("106.224","P_c4_2bc    ","100.8.843  ","P_2c4'bm'     ","P4_2bc.1'_c[P4bm]          ","P 4c -2ab 1'c       ",  &
                                               "-y,x,z+1/2,1;-x+1/2,y+1/2,z,1;y+1/2,x+1/2,z+1/2,1;x,y,z+1/2,-1                                          ")
     Shubnikov_info( 844)= Shub_Spgr_Info_Type("104.208","P_c4nc      ","100.9.844  ","P_2c4b'm'     ","P4nc.1'_c[P4bm]            ","P 4 -2n 1'c         ",  &
                                               "-y,x,z,1;-x+1/2,y+1/2,z+1/2,1;y+1/2,x+1/2,z+1/2,1;x,y,z+1/2,-1                                          ")
     Shubnikov_info( 845)= Shub_Spgr_Info_Type("101.179","P4_2cm      ","101.1.845  ","P4_2cm        ","P4_2cm.1                   ","P 4c -2c            ",  &
                                               "-y,x,z+1/2,1;-x,y,z+1/2,1;y,x,z,1                                                                       ")
     Shubnikov_info( 846)= Shub_Spgr_Info_Type("101.180","P4_2cm1'    ","101.2.846  ","P4_2cm1'      ","P4_2cm.1'                  ","P 4c -2c 1'         ",  &
                                               "-y,x,z+1/2,1;-x,y,z+1/2,1;y,x,z,1;x,y,z,-1                                                              ")
     Shubnikov_info( 847)= Shub_Spgr_Info_Type("101.181","P4_2'c'm    ","101.3.847  ","P4_2'c'm      ","P4_2'c'm                   ","P 4c' -2c'          ",  &
                                               "-y,x,z+1/2,-1;-x,y,z+1/2,-1;y,x,z,1                                                                     ")
     Shubnikov_info( 848)= Shub_Spgr_Info_Type("101.182","P4_2'cm'    ","101.4.848  ","P4_2'cm'      ","P4_2'cm'                   ","P 4c' -2c           ",  &
                                               "-y,x,z+1/2,-1;-x,y,z+1/2,1;y,x,z,-1                                                                     ")
     Shubnikov_info( 849)= Shub_Spgr_Info_Type("101.183","P4_2c'm'    ","101.5.849  ","P4_2c'm'      ","P4_2c'm'                   ","P 4c -2c'           ",  &
                                               "-y,x,z+1/2,1;-x,y,z+1/2,-1;y,x,z,-1                                                                     ")
     Shubnikov_info( 850)= Shub_Spgr_Info_Type("105.217","P_C4_2mc    ","101.6.850  ","P_P4_2cm      ","P4_2mc.1'_C[rP4_2cm]       ","P 4c -2 1'C         ",  &
                                               "-y,x,z+1/2,1;-x,y,z,1;y,x,z+1/2,1;x+1/2,y+1/2,z,-1                                                      ")
     Shubnikov_info( 851)= Shub_Spgr_Info_Type("106.225","P_C4_2bc    ","101.7.851  ","P_P4_2'cm'    ","P4_2bc.1'_C[rP4_2cm]       ","P 4c -2ab 1'C       ",  &
                                               "-y,x,z+1/2,1;-x+1/2,y+1/2,z,1;y+1/2,x+1/2,z+1/2,1;x+1/2,y+1/2,z,-1                                      ")
     Shubnikov_info( 852)= Shub_Spgr_Info_Type("102.187","P4_2nm      ","102.1.852  ","P4_2nm        ","P4_2nm.1                   ","P 4n -2n            ",  &
                                               "-y+1/2,x+1/2,z+1/2,1;-x+1/2,y+1/2,z+1/2,1;y,x,z,1                                                       ")
     Shubnikov_info( 853)= Shub_Spgr_Info_Type("102.188","P4_2nm1'    ","102.2.853  ","P4_2nm1'      ","P4_2nm.1'                  ","P 4n -2n 1'         ",  &
                                               "-y+1/2,x+1/2,z+1/2,1;-x+1/2,y+1/2,z+1/2,1;y,x,z,1;x,y,z,-1                                              ")
     Shubnikov_info( 854)= Shub_Spgr_Info_Type("102.189","P4_2'n'm    ","102.3.854  ","P4_2'n'm      ","P4_2'n'm                   ","P 4n' -2n'          ",  &
                                               "-y+1/2,x+1/2,z+1/2,-1;-x+1/2,y+1/2,z+1/2,-1;y,x,z,1                                                     ")
     Shubnikov_info( 855)= Shub_Spgr_Info_Type("102.190","P4_2'nm'    ","102.4.855  ","P4_2'nm'      ","P4_2'nm'                   ","P 4n' -2n           ",  &
                                               "-y+1/2,x+1/2,z+1/2,-1;-x+1/2,y+1/2,z+1/2,1;y,x,z,-1                                                     ")
     Shubnikov_info( 856)= Shub_Spgr_Info_Type("102.191","P4_2n'm'    ","102.5.856  ","P4_2n'm'      ","P4_2n'm'                   ","P 4n -2n'           ",  &
                                               "-y+1/2,x+1/2,z+1/2,1;-x+1/2,y+1/2,z+1/2,-1;y,x,z,-1                                                     ")
     Shubnikov_info( 857)= Shub_Spgr_Info_Type("109.244","I_c4_1md    ","102.6.857  ","P_I4_2nm      ","I4_1md.1'_c[rP4_2nm]       ","I 4bw -2 1'c        ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x+1/2,z+1/4,1;-x,y,z,1;y,x+1/2,z+1/4,1;x,y,z+1/2,-1                              ")
     Shubnikov_info( 858)= Shub_Spgr_Info_Type("110.250","I_c4_1cd    ","102.7.858  ","P_I4_2n'm'    ","I4_1cd.1'_c[rP4_2nm]       ","I 4bw -2c 1'c       ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x+1/2,z+1/4,1;-x,y,z+1/2,1;y+1/2,x,z+1/4,1;x,y,z+1/2,-1                          ")
     Shubnikov_info( 859)= Shub_Spgr_Info_Type("103.195","P4cc        ","103.1.859  ","P4cc          ","P4cc.1                     ","P 4 -2c             ",  &
                                               "-y,x,z,1;-x,y,z+1/2,1;y,x,z+1/2,1                                                                       ")
     Shubnikov_info( 860)= Shub_Spgr_Info_Type("103.196","P4cc1'      ","103.2.860  ","P4cc1'        ","P4cc.1'                    ","P 4 -2c 1'          ",  &
                                               "-y,x,z,1;-x,y,z+1/2,1;y,x,z+1/2,1;x,y,z,-1                                                              ")
     Shubnikov_info( 861)= Shub_Spgr_Info_Type("103.197","P4'c'c      ","103.3.861  ","P4'c'c        ","P4'c'c                     ","P 4' -2c'           ",  &
                                               "-y,x,z,-1;-x,y,z+1/2,-1;y,x,z+1/2,1                                                                     ")
     Shubnikov_info( 862)= Shub_Spgr_Info_Type("103.198","P4'cc'      ","103.4.862  ","P4'cc'        ","P4'cc'                     ","P 4' -2c            ",  &
                                               "-y,x,z,-1;-x,y,z+1/2,1;y,x,z+1/2,-1                                                                     ")
     Shubnikov_info( 863)= Shub_Spgr_Info_Type("103.199","P4c'c'      ","103.5.863  ","P4c'c'        ","P4c'c'                     ","P 4 -2c'            ",  &
                                               "-y,x,z,1;-x,y,z+1/2,-1;y,x,z+1/2,-1                                                                     ")
     Shubnikov_info( 864)= Shub_Spgr_Info_Type("103.201","P_C4cc      ","103.6.864  ","P_P4cc        ","P4cc.1'_C[rP4cc]           ","P 4 -2c 1'C         ",  &
                                               "-y,x,z,1;-x,y,z+1/2,1;y,x,z+1/2,1;x+1/2,y+1/2,z,-1                                                      ")
     Shubnikov_info( 865)= Shub_Spgr_Info_Type("104.209","P_C4nc      ","103.7.865  ","P_P4'cc'      ","P4nc.1'_C[rP4cc]           ","P 4 -2n 1'C         ",  &
                                               "-y,x,z,1;-x+1/2,y+1/2,z+1/2,1;y+1/2,x+1/2,z+1/2,1;x+1/2,y+1/2,z,-1                                      ")
     Shubnikov_info( 866)= Shub_Spgr_Info_Type("104.203","P4nc        ","104.1.866  ","P4nc          ","P4nc.1                     ","P 4 -2n             ",  &
                                               "-y,x,z,1;-x+1/2,y+1/2,z+1/2,1;y+1/2,x+1/2,z+1/2,1                                                       ")
     Shubnikov_info( 867)= Shub_Spgr_Info_Type("104.204","P4nc1'      ","104.2.867  ","P4nc1'        ","P4nc.1'                    ","P 4 -2n 1'          ",  &
                                               "-y,x,z,1;-x+1/2,y+1/2,z+1/2,1;y+1/2,x+1/2,z+1/2,1;x,y,z,-1                                              ")
     Shubnikov_info( 868)= Shub_Spgr_Info_Type("104.205","P4'n'c      ","104.3.868  ","P4'n'c        ","P4'n'c                     ","P 4' -2n'           ",  &
                                               "-y,x,z,-1;-x+1/2,y+1/2,z+1/2,-1;y+1/2,x+1/2,z+1/2,1                                                     ")
     Shubnikov_info( 869)= Shub_Spgr_Info_Type("104.206","P4'nc'      ","104.4.869  ","P4'nc'        ","P4'nc'                     ","P 4' -2n            ",  &
                                               "-y,x,z,-1;-x+1/2,y+1/2,z+1/2,1;y+1/2,x+1/2,z+1/2,-1                                                     ")
     Shubnikov_info( 870)= Shub_Spgr_Info_Type("104.207","P4n'c'      ","104.5.870  ","P4n'c'        ","P4n'c'                     ","P 4 -2n'            ",  &
                                               "-y,x,z,1;-x+1/2,y+1/2,z+1/2,-1;y+1/2,x+1/2,z+1/2,-1                                                     ")
     Shubnikov_info( 871)= Shub_Spgr_Info_Type("105.211","P4_2mc      ","105.1.871  ","P4_2mc        ","P4_2mc.1                   ","P 4c -2             ",  &
                                               "-y,x,z+1/2,1;-x,y,z,1;y,x,z+1/2,1                                                                       ")
     Shubnikov_info( 872)= Shub_Spgr_Info_Type("105.212","P4_2mc1'    ","105.2.872  ","P4_2mc1'      ","P4_2mc.1'                  ","P 4c -2 1'          ",  &
                                               "-y,x,z+1/2,1;-x,y,z,1;y,x,z+1/2,1;x,y,z,-1                                                              ")
     Shubnikov_info( 873)= Shub_Spgr_Info_Type("105.213","P4_2'm'c    ","105.3.873  ","P4_2'm'c      ","P4_2'm'c                   ","P 4c' -2'           ",  &
                                               "-y,x,z+1/2,-1;-x,y,z,-1;y,x,z+1/2,1                                                                     ")
     Shubnikov_info( 874)= Shub_Spgr_Info_Type("105.214","P4_2'mc'    ","105.4.874  ","P4_2'mc'      ","P4_2'mc'                   ","P 4c' -2            ",  &
                                               "-y,x,z+1/2,-1;-x,y,z,1;y,x,z+1/2,-1                                                                     ")
     Shubnikov_info( 875)= Shub_Spgr_Info_Type("105.215","P4_2m'c'    ","105.5.875  ","P4_2m'c'      ","P4_2m'c'                   ","P 4c -2'            ",  &
                                               "-y,x,z+1/2,1;-x,y,z,-1;y,x,z+1/2,-1                                                                     ")
     Shubnikov_info( 876)= Shub_Spgr_Info_Type("101.185","P_C4_2cm    ","105.6.876  ","P_P4_2mc      ","P4_2cm.1'_C[rP4_2mc]       ","P 4c -2c 1'C        ",  &
                                               "-y,x,z+1/2,1;-x,y,z+1/2,1;y,x,z,1;x+1/2,y+1/2,z,-1                                                      ")
     Shubnikov_info( 877)= Shub_Spgr_Info_Type("102.193","P_C4_2nm    ","105.7.877  ","P_P4_2'mc'    ","P4_2nm.1'_C[rP4_2mc]       ","P 4n -2n 1'C        ",  &
                                               "-y+1/2,x+1/2,z+1/2,1;-x+1/2,y+1/2,z+1/2,1;y,x,z,1;x+1/2,y+1/2,z,-1                                      ")
     Shubnikov_info( 878)= Shub_Spgr_Info_Type("106.219","P4_2bc      ","106.1.878  ","P4_2bc        ","P4_2bc.1                   ","P 4c -2ab           ",  &
                                               "-y,x,z+1/2,1;-x+1/2,y+1/2,z,1;y+1/2,x+1/2,z+1/2,1                                                       ")
     Shubnikov_info( 879)= Shub_Spgr_Info_Type("106.220","P4_2bc1'    ","106.2.879  ","P4_2bc1'      ","P4_2bc.1'                  ","P 4c -2ab 1'        ",  &
                                               "-y,x,z+1/2,1;-x+1/2,y+1/2,z,1;y+1/2,x+1/2,z+1/2,1;x,y,z,-1                                              ")
     Shubnikov_info( 880)= Shub_Spgr_Info_Type("106.221","P4_2'b'c    ","106.3.880  ","P4_2'b'c      ","P4_2'b'c                   ","P 4c' -2ab'         ",  &
                                               "-y,x,z+1/2,-1;-x+1/2,y+1/2,z,-1;y+1/2,x+1/2,z+1/2,1                                                     ")
     Shubnikov_info( 881)= Shub_Spgr_Info_Type("106.222","P4_2'bc'    ","106.4.881  ","P4_2'bc'      ","P4_2'bc'                   ","P 4c' -2ab          ",  &
                                               "-y,x,z+1/2,-1;-x+1/2,y+1/2,z,1;y+1/2,x+1/2,z+1/2,-1                                                     ")
     Shubnikov_info( 882)= Shub_Spgr_Info_Type("106.223","P4_2b'c'    ","106.5.882  ","P4_2b'c'      ","P4_2b'c'                   ","P 4c -2ab'          ",  &
                                               "-y,x,z+1/2,1;-x+1/2,y+1/2,z,-1;y+1/2,x+1/2,z+1/2,-1                                                     ")
     Shubnikov_info( 883)= Shub_Spgr_Info_Type("107.227","I4mm        ","107.1.883  ","I4mm          ","I4mm.1                     ","I 4 -2              ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,1;-x,y,z,1;y,x,z,1                                                           ")
     Shubnikov_info( 884)= Shub_Spgr_Info_Type("107.228","I4mm1'      ","107.2.884  ","I4mm1'        ","I4mm.1'                    ","I 4 -2 1'           ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,1;-x,y,z,1;y,x,z,1;x,y,z,-1                                                  ")
     Shubnikov_info( 885)= Shub_Spgr_Info_Type("107.229","I4'm'm      ","107.3.885  ","I4'm'm        ","I4'm'm                     ","I 4' -2'            ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,-1;-x,y,z,-1;y,x,z,1                                                         ")
     Shubnikov_info( 886)= Shub_Spgr_Info_Type("107.230","I4'mm'      ","107.4.886  ","I4'mm'        ","I4'mm'                     ","I 4' -2             ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,-1;-x,y,z,1;y,x,z,-1                                                         ")
     Shubnikov_info( 887)= Shub_Spgr_Info_Type("107.231","I4m'm'      ","107.5.887  ","I4m'm'        ","I4m'm'                     ","I 4 -2'             ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,1;-x,y,z,-1;y,x,z,-1                                                         ")
     Shubnikov_info( 888)= Shub_Spgr_Info_Type("99.170 ","P_I4mm      ","107.6.888  ","I_P4mm        ","P4mm.1'_I[I4mm]            ","P 4 -2 1'I          ",  &
                                               "-y,x,z,1;-x,y,z,1;y,x,z,1;x+1/2,y+1/2,z+1/2,-1                                                          ")
     Shubnikov_info( 889)= Shub_Spgr_Info_Type("102.194","P_I4_2nm    ","107.7.889  ","I_P4'm'm      ","P4_2nm.1'_I[I4mm]          ","P 4n -2n 1'I        ",  &
                                               "-y+1/2,x+1/2,z+1/2,1;-x+1/2,y+1/2,z+1/2,1;y,x,z,1;x+1/2,y+1/2,z+1/2,-1                                  ")
     Shubnikov_info( 890)= Shub_Spgr_Info_Type("105.218","P_I4_2mc    ","107.8.890  ","I_P4'mm'      ","P4_2mc.1'_I[I4mm]          ","P 4c -2 1'I         ",  &
                                               "-y,x,z+1/2,1;-x,y,z,1;y,x,z+1/2,1;x+1/2,y+1/2,z+1/2,-1                                                  ")
     Shubnikov_info( 891)= Shub_Spgr_Info_Type("104.210","P_I4nc      ","107.9.891  ","I_P4m'm'      ","P4nc.1'_I[I4mm]            ","P 4 -2n 1'I         ",  &
                                               "-y,x,z,1;-x+1/2,y+1/2,z+1/2,1;y+1/2,x+1/2,z+1/2,1;x+1/2,y+1/2,z+1/2,-1                                  ")
     Shubnikov_info( 892)= Shub_Spgr_Info_Type("108.233","I4cm        ","108.1.892  ","I4cm          ","I4cm.1                     ","I 4 -2c             ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,1;-x,y,z+1/2,1;y,x,z+1/2,1                                                   ")
     Shubnikov_info( 893)= Shub_Spgr_Info_Type("108.234","I4cm1'      ","108.2.893  ","I4cm1'        ","I4cm.1'                    ","I 4 -2c 1'          ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,1;-x,y,z+1/2,1;y,x,z+1/2,1;x,y,z,-1                                          ")
     Shubnikov_info( 894)= Shub_Spgr_Info_Type("108.235","I4'c'm      ","108.3.894  ","I4'c'm        ","I4'c'm                     ","I 4' -2c'           ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,-1;-x,y,z+1/2,-1;y,x,z+1/2,1                                                 ")
     Shubnikov_info( 895)= Shub_Spgr_Info_Type("108.236","I4'cm'      ","108.4.895  ","I4'cm'        ","I4'cm'                     ","I 4' -2c            ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,-1;-x,y,z+1/2,1;y,x,z+1/2,-1                                                 ")
     Shubnikov_info( 896)= Shub_Spgr_Info_Type("108.237","I4c'm'      ","108.5.896  ","I4c'm'        ","I4c'm'                     ","I 4 -2c'            ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,1;-x,y,z+1/2,-1;y,x,z+1/2,-1                                                 ")
     Shubnikov_info( 897)= Shub_Spgr_Info_Type("100.178","P_I4bm      ","108.6.897  ","I_P4cm        ","P4bm.1'_I[I4cm]            ","P 4 -2ab 1'I        ",  &
                                               "-y,x,z,1;-x+1/2,y+1/2,z,1;y+1/2,x+1/2,z,1;x+1/2,y+1/2,z+1/2,-1                                          ")
     Shubnikov_info( 898)= Shub_Spgr_Info_Type("101.186","P_I4_2cm    ","108.7.898  ","I_P4'c'm      ","P4_2cm.1'_I[I4cm]          ","P 4c -2c 1'I        ",  &
                                               "-y,x,z+1/2,1;-x,y,z+1/2,1;y,x,z,1;x+1/2,y+1/2,z+1/2,-1                                                  ")
     Shubnikov_info( 899)= Shub_Spgr_Info_Type("106.226","P_I4_2bc    ","108.8.899  ","I_P4'cm'      ","P4_2bc.1'_I[I4cm]          ","P 4c -2ab 1'I       ",  &
                                               "-y,x,z+1/2,1;-x+1/2,y+1/2,z,1;y+1/2,x+1/2,z+1/2,1;x+1/2,y+1/2,z+1/2,-1                                  ")
     Shubnikov_info( 900)= Shub_Spgr_Info_Type("103.202","P_I4cc      ","108.9.900  ","I_P4c'm'      ","P4cc.1'_I[I4cm]            ","P 4 -2c 1'I         ",  &
                                               "-y,x,z,1;-x,y,z+1/2,1;y,x,z+1/2,1;x+1/2,y+1/2,z+1/2,-1                                                  ")
     Shubnikov_info( 901)= Shub_Spgr_Info_Type("109.239","I4_1md      ","109.1.901  ","I4_1md        ","I4_1md.1                   ","I 4bw -2            ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x+1/2,z+1/4,1;-x,y,z,1;y,x+1/2,z+1/4,1                                           ")
     Shubnikov_info( 902)= Shub_Spgr_Info_Type("109.240","I4_1md1'    ","109.2.902  ","I4_1md1'      ","I4_1md.1'                  ","I 4bw -2 1'         ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x+1/2,z+1/4,1;-x,y,z,1;y,x+1/2,z+1/4,1;x,y,z,-1                                  ")
     Shubnikov_info( 903)= Shub_Spgr_Info_Type("109.241","I4_1'm'd    ","109.3.903  ","I4_1'm'd      ","I4_1'm'd                   ","I 4bw' -2'          ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x+1/2,z+1/4,-1;-x,y,z,-1;y,x+1/2,z+1/4,1                                         ")
     Shubnikov_info( 904)= Shub_Spgr_Info_Type("109.242","I4_1'md'    ","109.4.904  ","I4_1'md'      ","I4_1'md'                   ","I 4bw' -2           ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x+1/2,z+1/4,-1;-x,y,z,1;y,x+1/2,z+1/4,-1                                         ")
     Shubnikov_info( 905)= Shub_Spgr_Info_Type("109.243","I4_1m'd'    ","109.5.905  ","I4_1m'd'      ","I4_1m'd'                   ","I 4bw -2'           ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x+1/2,z+1/4,1;-x,y,z,-1;y,x+1/2,z+1/4,-1                                         ")
     Shubnikov_info( 906)= Shub_Spgr_Info_Type("110.245","I4_1cd      ","110.1.906  ","I4_1cd        ","I4_1cd.1                   ","I 4bw -2c           ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x+1/2,z+1/4,1;-x,y,z+1/2,1;y+1/2,x,z+1/4,1                                       ")
     Shubnikov_info( 907)= Shub_Spgr_Info_Type("110.246","I4_1cd1'    ","110.2.907  ","I4_1cd1'      ","I4_1cd.1'                  ","I 4bw -2c 1'        ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x+1/2,z+1/4,1;-x,y,z+1/2,1;y+1/2,x,z+1/4,1;x,y,z,-1                              ")
     Shubnikov_info( 908)= Shub_Spgr_Info_Type("110.247","I4_1'c'd    ","110.3.908  ","I4_1'c'd      ","I4_1'c'd                   ","I 4bw' -2c'         ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x+1/2,z+1/4,-1;-x,y,z+1/2,-1;y+1/2,x,z+1/4,1                                     ")
     Shubnikov_info( 909)= Shub_Spgr_Info_Type("110.248","I4_1'cd'    ","110.4.909  ","I4_1'cd'      ","I4_1'cd'                   ","I 4bw' -2c          ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x+1/2,z+1/4,-1;-x,y,z+1/2,1;y+1/2,x,z+1/4,-1                                     ")
     Shubnikov_info( 910)= Shub_Spgr_Info_Type("110.249","I4_1c'd'    ","110.5.910  ","I4_1c'd'      ","I4_1c'd'                   ","I 4bw -2c'          ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x+1/2,z+1/4,1;-x,y,z+1/2,-1;y+1/2,x,z+1/4,-1                                     ")
     Shubnikov_info( 911)= Shub_Spgr_Info_Type("111.251","P-42m       ","111.1.911  ","P-42m         ","P-42m.1                    ","P -4 2              ",  &
                                               "y,-x,-z,1;x,-y,-z,1;y,x,z,1                                                                             ")
     Shubnikov_info( 912)= Shub_Spgr_Info_Type("111.252","P-42m1'     ","111.2.912  ","P-42m1'       ","P-42m.1'                   ","P -4 2 1'           ",  &
                                               "y,-x,-z,1;x,-y,-z,1;y,x,z,1;x,y,z,-1                                                                    ")
     Shubnikov_info( 913)= Shub_Spgr_Info_Type("111.253","P-4'2'm     ","111.3.913  ","P-4'2'm       ","P-4'2'm                    ","P -4' 2'            ",  &
                                               "y,-x,-z,-1;x,-y,-z,-1;y,x,z,1                                                                           ")
     Shubnikov_info( 914)= Shub_Spgr_Info_Type("111.254","P-4'2m'     ","111.4.914  ","P-4'2m'       ","P-4'2m'                    ","P -4' 2             ",  &
                                               "y,-x,-z,-1;x,-y,-z,1;y,x,z,-1                                                                           ")
     Shubnikov_info( 915)= Shub_Spgr_Info_Type("111.255","P-42'm'     ","111.5.915  ","P-42'm'       ","P-42'm'                    ","P -4 2'             ",  &
                                               "y,-x,-z,1;x,-y,-z,-1;y,x,z,-1                                                                           ")
     Shubnikov_info( 916)= Shub_Spgr_Info_Type("111.256","P_c-42m     ","111.6.916  ","P_2c-42m      ","P-42m.1'_c[P-42m]          ","P -4 2 1'c          ",  &
                                               "y,-x,-z,1;x,-y,-z,1;y,x,z,1;x,y,z+1/2,-1                                                                ")
     Shubnikov_info( 917)= Shub_Spgr_Info_Type("115.289","P_C-4m2     ","111.7.917  ","P_P-42m       ","P-4m2.1'_C[rP-42m]         ","P -4 -2 1'C         ",  &
                                               "y,-x,-z,1;-x,y,z,1;-y,-x,-z,1;x+1/2,y+1/2,z,-1                                                          ")
     Shubnikov_info( 918)= Shub_Spgr_Info_Type("119.320","I_c-4m2     ","111.8.918  ","P_I-42m       ","I-4m2.1'_c[rP-42m]         ","I -4 -2 1'c         ",  &
                                               "x+1/2,y+1/2,z+1/2,1;y,-x,-z,1;-x,y,z,1;-y,-x,-z,1;x,y,z+1/2,-1                                          ")
     Shubnikov_info( 919)= Shub_Spgr_Info_Type("112.264","P_c-42c     ","111.9.919  ","P_2c-42'm'    ","P-42c.1'_c[P-42m]          ","P -4 2c 1'c         ",  &
                                               "y,-x,-z,1;x,-y,-z+1/2,1;y,x,z+1/2,1;x,y,z+1/2,-1                                                        ")
     Shubnikov_info( 920)= Shub_Spgr_Info_Type("117.305","P_C-4b2     ","111.10.920 ","P_P-4'2m'     ","P-4b2.1'_C[rP-42m]         ","P -4 -2ab 1'C       ",  &
                                               "y,-x,-z,1;-x+1/2,y+1/2,z,1;-y+1/2,-x+1/2,-z,1;x+1/2,y+1/2,z,-1                                          ")
     Shubnikov_info( 921)= Shub_Spgr_Info_Type("120.326","I_c-4c2     ","111.11.921 ","P_I-4'2m'     ","I-4c2.1'_c[rP-42m]         ","I -4 -2c 1'c        ",  &
                                               "x+1/2,y+1/2,z+1/2,1;y,-x,-z,1;-x,y,z+1/2,1;-y,-x,-z+1/2,1;x,y,z+1/2,-1                                  ")
     Shubnikov_info( 922)= Shub_Spgr_Info_Type("112.259","P-42c       ","112.1.922  ","P-42c         ","P-42c.1                    ","P -4 2c             ",  &
                                               "y,-x,-z,1;x,-y,-z+1/2,1;y,x,z+1/2,1                                                                     ")
     Shubnikov_info( 923)= Shub_Spgr_Info_Type("112.260","P-42c1'     ","112.2.923  ","P-42c1'       ","P-42c.1'                   ","P -4 2c 1'          ",  &
                                               "y,-x,-z,1;x,-y,-z+1/2,1;y,x,z+1/2,1;x,y,z,-1                                                            ")
     Shubnikov_info( 924)= Shub_Spgr_Info_Type("112.261","P-4'2'c     ","112.3.924  ","P-4'2'c       ","P-4'2'c                    ","P -4' 2c'           ",  &
                                               "y,-x,-z,-1;x,-y,-z+1/2,-1;y,x,z+1/2,1                                                                   ")
     Shubnikov_info( 925)= Shub_Spgr_Info_Type("112.262","P-4'2c'     ","112.4.925  ","P-4'2c'       ","P-4'2c'                    ","P -4' 2c            ",  &
                                               "y,-x,-z,-1;x,-y,-z+1/2,1;y,x,z+1/2,-1                                                                   ")
     Shubnikov_info( 926)= Shub_Spgr_Info_Type("112.263","P-42'c'     ","112.5.926  ","P-42'c'       ","P-42'c'                    ","P -4 2c'            ",  &
                                               "y,-x,-z,1;x,-y,-z+1/2,-1;y,x,z+1/2,-1                                                                   ")
     Shubnikov_info( 927)= Shub_Spgr_Info_Type("116.297","P_C-4c2     ","112.6.927  ","P_P-42c       ","P-4c2.1'_C[rP-42c]         ","P -4 -2c 1'C        ",  &
                                               "y,-x,-z,1;-x,y,z+1/2,1;-y,-x,-z+1/2,1;x+1/2,y+1/2,z,-1                                                  ")
     Shubnikov_info( 928)= Shub_Spgr_Info_Type("118.313","P_C-4n2     ","112.7.928  ","P_P-4'2c'     ","P-4n2.1'_C[rP-42c]         ","P -4 -2n 1'C        ",  &
                                               "y,-x,-z,1;-x+1/2,y+1/2,z+1/2,1;-y+1/2,-x+1/2,-z+1/2,1;x+1/2,y+1/2,z,-1                                  ")
     Shubnikov_info( 929)= Shub_Spgr_Info_Type("113.267","P-42_1m     ","113.1.929  ","P-42_1m       ","P-42_1m.1                  ","P -4 2ab            ",  &
                                               "y,-x,-z,1;x+1/2,-y+1/2,-z,1;y+1/2,x+1/2,z,1                                                             ")
     Shubnikov_info( 930)= Shub_Spgr_Info_Type("113.268","P-42_1m1'   ","113.2.930  ","P-42_1m1'     ","P-42_1m.1'                 ","P -4 2ab 1'         ",  &
                                               "y,-x,-z,1;x+1/2,-y+1/2,-z,1;y+1/2,x+1/2,z,1;x,y,z,-1                                                    ")
     Shubnikov_info( 931)= Shub_Spgr_Info_Type("113.269","P-4'2_1'm   ","113.3.931  ","P-4'2_1'm     ","P-4'2_1'm                  ","P -4' 2ab'          ",  &
                                               "y,-x,-z,-1;x+1/2,-y+1/2,-z,-1;y+1/2,x+1/2,z,1                                                           ")
     Shubnikov_info( 932)= Shub_Spgr_Info_Type("113.270","P-4'2_1m'   ","113.4.932  ","P-4'2_1m'     ","P-4'2_1m'                  ","P -4' 2ab           ",  &
                                               "y,-x,-z,-1;x+1/2,-y+1/2,-z,1;y+1/2,x+1/2,z,-1                                                           ")
     Shubnikov_info( 933)= Shub_Spgr_Info_Type("113.271","P-42_1'm'   ","113.5.933  ","P-42_1'm'     ","P-42_1'm'                  ","P -4 2ab'           ",  &
                                               "y,-x,-z,1;x+1/2,-y+1/2,-z,-1;y+1/2,x+1/2,z,-1                                                           ")
     Shubnikov_info( 934)= Shub_Spgr_Info_Type("113.272","P_c-42_1m   ","113.6.934  ","P_2c-42_1m    ","P-42_1m.1'_c[P-42_1m]      ","P -4 2ab 1'c        ",  &
                                               "y,-x,-z,1;x+1/2,-y+1/2,-z,1;y+1/2,x+1/2,z,1;x,y,z+1/2,-1                                                ")
     Shubnikov_info( 935)= Shub_Spgr_Info_Type("114.280","P_c-42_1c   ","113.7.935  ","P_2c-4'2_1m'  ","P-42_1c.1'_c[P-42_1m]      ","P -4 2n 1'c         ",  &
                                               "y,-x,-z,1;x+1/2,-y+1/2,-z+1/2,1;y+1/2,x+1/2,z+1/2,1;x,y,z+1/2,-1                                        ")
     Shubnikov_info( 936)= Shub_Spgr_Info_Type("114.275","P-42_1c     ","114.1.936  ","P-42_1c       ","P-42_1c.1                  ","P -4 2n             ",  &
                                               "y,-x,-z,1;x+1/2,-y+1/2,-z+1/2,1;y+1/2,x+1/2,z+1/2,1                                                     ")
     Shubnikov_info( 937)= Shub_Spgr_Info_Type("114.276","P-42_1c1'   ","114.2.937  ","P-42_1c1'     ","P-42_1c.1'                 ","P -4 2n 1'          ",  &
                                               "y,-x,-z,1;x+1/2,-y+1/2,-z+1/2,1;y+1/2,x+1/2,z+1/2,1;x,y,z,-1                                            ")
     Shubnikov_info( 938)= Shub_Spgr_Info_Type("114.277","P-4'2_1'c   ","114.3.938  ","P-4'2_1'c     ","P-4'2_1'c                  ","P -4' 2n'           ",  &
                                               "y,-x,-z,-1;x+1/2,-y+1/2,-z+1/2,-1;y+1/2,x+1/2,z+1/2,1                                                   ")
     Shubnikov_info( 939)= Shub_Spgr_Info_Type("114.278","P-4'2_1c'   ","114.4.939  ","P-4'2_1c'     ","P-4'2_1c'                  ","P -4' 2n            ",  &
                                               "y,-x,-z,-1;x+1/2,-y+1/2,-z+1/2,1;y+1/2,x+1/2,z+1/2,-1                                                   ")
     Shubnikov_info( 940)= Shub_Spgr_Info_Type("114.279","P-42_1'c'   ","114.5.940  ","P-42_1'c'     ","P-42_1'c'                  ","P -4 2n'            ",  &
                                               "y,-x,-z,1;x+1/2,-y+1/2,-z+1/2,-1;y+1/2,x+1/2,z+1/2,-1                                                   ")
     Shubnikov_info( 941)= Shub_Spgr_Info_Type("115.283","P-4m2       ","115.1.941  ","P-4m2         ","P-4m2.1                    ","P -4 -2             ",  &
                                               "y,-x,-z,1;-x,y,z,1;-y,-x,-z,1                                                                           ")
     Shubnikov_info( 942)= Shub_Spgr_Info_Type("115.284","P-4m21'     ","115.2.942  ","P-4m21'       ","P-4m2.1'                   ","P -4 -2 1'          ",  &
                                               "y,-x,-z,1;-x,y,z,1;-y,-x,-z,1;x,y,z,-1                                                                  ")
     Shubnikov_info( 943)= Shub_Spgr_Info_Type("115.285","P-4'm'2     ","115.3.943  ","P-4'm'2       ","P-4'm'2                    ","P -4' -2'           ",  &
                                               "y,-x,-z,-1;-x,y,z,-1;-y,-x,-z,1                                                                         ")
     Shubnikov_info( 944)= Shub_Spgr_Info_Type("115.286","P-4'm2'     ","115.4.944  ","P-4'm2'       ","P-4'm2'                    ","P -4' -2            ",  &
                                               "y,-x,-z,-1;-x,y,z,1;-y,-x,-z,-1                                                                         ")
     Shubnikov_info( 945)= Shub_Spgr_Info_Type("115.287","P-4m'2'     ","115.5.945  ","P-4m'2'       ","P-4m'2'                    ","P -4 -2'            ",  &
                                               "y,-x,-z,1;-x,y,z,-1;-y,-x,-z,-1                                                                         ")
     Shubnikov_info( 946)= Shub_Spgr_Info_Type("115.288","P_c-4m2     ","115.6.946  ","P_2c-4m2      ","P-4m2.1'_c[P-4m2]          ","P -4 -2 1'c         ",  &
                                               "y,-x,-z,1;-x,y,z,1;-y,-x,-z,1;x,y,z+1/2,-1                                                              ")
     Shubnikov_info( 947)= Shub_Spgr_Info_Type("111.257","P_C-42m     ","115.7.947  ","P_P-4m2       ","P-42m.1'_C[rP-4m2]         ","P -4 2 1'C          ",  &
                                               "y,-x,-z,1;x,-y,-z,1;y,x,z,1;x+1/2,y+1/2,z,-1                                                            ")
     Shubnikov_info( 948)= Shub_Spgr_Info_Type("121.332","I_c-42m     ","115.8.948  ","P_I-4m2       ","I-42m.1'_c[rP-4m2]         ","I -4 2 1'c          ",  &
                                               "x+1/2,y+1/2,z+1/2,1;y,-x,-z,1;x,-y,-z,1;y,x,z,1;x,y,z+1/2,-1                                            ")
     Shubnikov_info( 949)= Shub_Spgr_Info_Type("116.296","P_c-4c2     ","115.9.949  ","P_2c-4'm'2    ","P-4c2.1'_c[P-4m2]          ","P -4 -2c 1'c        ",  &
                                               "y,-x,-z,1;-x,y,z+1/2,1;-y,-x,-z+1/2,1;x,y,z+1/2,-1                                                      ")
     Shubnikov_info( 950)= Shub_Spgr_Info_Type("113.273","P_C-42_1m   ","115.10.950 ","P_P-4'm2'     ","P-42_1m.1'_C[rP-4m2]       ","P -4 2ab 1'C        ",  &
                                               "y,-x,-z,1;x+1/2,-y+1/2,-z,1;y+1/2,x+1/2,z,1;x+1/2,y+1/2,z,-1                                            ")
     Shubnikov_info( 951)= Shub_Spgr_Info_Type("116.291","P-4c2       ","116.1.951  ","P-4c2         ","P-4c2.1                    ","P -4 -2c            ",  &
                                               "y,-x,-z,1;-x,y,z+1/2,1;-y,-x,-z+1/2,1                                                                   ")
     Shubnikov_info( 952)= Shub_Spgr_Info_Type("116.292","P-4c21'     ","116.2.952  ","P-4c21'       ","P-4c2.1'                   ","P -4 -2c 1'         ",  &
                                               "y,-x,-z,1;-x,y,z+1/2,1;-y,-x,-z+1/2,1;x,y,z,-1                                                          ")
     Shubnikov_info( 953)= Shub_Spgr_Info_Type("116.293","P-4'c'2     ","116.3.953  ","P-4'c'2       ","P-4'c'2                    ","P -4' -2c'          ",  &
                                               "y,-x,-z,-1;-x,y,z+1/2,-1;-y,-x,-z+1/2,1                                                                 ")
     Shubnikov_info( 954)= Shub_Spgr_Info_Type("116.294","P-4'c2'     ","116.4.954  ","P-4'c2'       ","P-4'c2'                    ","P -4' -2c           ",  &
                                               "y,-x,-z,-1;-x,y,z+1/2,1;-y,-x,-z+1/2,-1                                                                 ")
     Shubnikov_info( 955)= Shub_Spgr_Info_Type("116.295","P-4c'2'     ","116.5.955  ","P-4c'2'       ","P-4c'2'                    ","P -4 -2c'           ",  &
                                               "y,-x,-z,1;-x,y,z+1/2,-1;-y,-x,-z+1/2,-1                                                                 ")
     Shubnikov_info( 956)= Shub_Spgr_Info_Type("112.265","P_C-42c     ","116.6.956  ","P_P-4c2       ","P-42c.1'_C[rP-4c2]         ","P -4 2c 1'C         ",  &
                                               "y,-x,-z,1;x,-y,-z+1/2,1;y,x,z+1/2,1;x+1/2,y+1/2,z,-1                                                    ")
     Shubnikov_info( 957)= Shub_Spgr_Info_Type("114.281","P_C-42_1c   ","116.7.957  ","P_P-4'c2'     ","P-42_1c.1'_C[rP-4c2]       ","P -4 2n 1'C         ",  &
                                               "y,-x,-z,1;x+1/2,-y+1/2,-z+1/2,1;y+1/2,x+1/2,z+1/2,1;x+1/2,y+1/2,z,-1                                    ")
     Shubnikov_info( 958)= Shub_Spgr_Info_Type("117.299","P-4b2       ","117.1.958  ","P-4b2         ","P-4b2.1                    ","P -4 -2ab           ",  &
                                               "y,-x,-z,1;-x+1/2,y+1/2,z,1;-y+1/2,-x+1/2,-z,1                                                           ")
     Shubnikov_info( 959)= Shub_Spgr_Info_Type("117.300","P-4b21'     ","117.2.959  ","P-4b21'       ","P-4b2.1'                   ","P -4 -2ab 1'        ",  &
                                               "y,-x,-z,1;-x+1/2,y+1/2,z,1;-y+1/2,-x+1/2,-z,1;x,y,z,-1                                                  ")
     Shubnikov_info( 960)= Shub_Spgr_Info_Type("117.301","P-4'b'2     ","117.3.960  ","P-4'b'2       ","P-4'b'2                    ","P -4' -2ab'         ",  &
                                               "y,-x,-z,-1;-x+1/2,y+1/2,z,-1;-y+1/2,-x+1/2,-z,1                                                         ")
     Shubnikov_info( 961)= Shub_Spgr_Info_Type("117.302","P-4'b2'     ","117.4.961  ","P-4'b2'       ","P-4'b2'                    ","P -4' -2ab          ",  &
                                               "y,-x,-z,-1;-x+1/2,y+1/2,z,1;-y+1/2,-x+1/2,-z,-1                                                         ")
     Shubnikov_info( 962)= Shub_Spgr_Info_Type("117.303","P-4b'2'     ","117.5.962  ","P-4b'2'       ","P-4b'2'                    ","P -4 -2ab'          ",  &
                                               "y,-x,-z,1;-x+1/2,y+1/2,z,-1;-y+1/2,-x+1/2,-z,-1                                                         ")
     Shubnikov_info( 963)= Shub_Spgr_Info_Type("117.304","P_c-4b2     ","117.6.963  ","P_2c-4b2      ","P-4b2.1'_c[P-4b2]          ","P -4 -2ab 1'c       ",  &
                                               "y,-x,-z,1;-x+1/2,y+1/2,z,1;-y+1/2,-x+1/2,-z,1;x,y,z+1/2,-1                                              ")
     Shubnikov_info( 964)= Shub_Spgr_Info_Type("118.312","P_c-4n2     ","117.7.964  ","P_2c-4'b'2    ","P-4n2.1'_c[P-4b2]          ","P -4 -2n 1'c        ",  &
                                               "y,-x,-z,1;-x+1/2,y+1/2,z+1/2,1;-y+1/2,-x+1/2,-z+1/2,1;x,y,z+1/2,-1                                      ")
     Shubnikov_info( 965)= Shub_Spgr_Info_Type("118.307","P-4n2       ","118.1.965  ","P-4n2         ","P-4n2.1                    ","P -4 -2n            ",  &
                                               "y,-x,-z,1;-x+1/2,y+1/2,z+1/2,1;-y+1/2,-x+1/2,-z+1/2,1                                                   ")
     Shubnikov_info( 966)= Shub_Spgr_Info_Type("118.308","P-4n21'     ","118.2.966  ","P-4n21'       ","P-4n2.1'                   ","P -4 -2n 1'         ",  &
                                               "y,-x,-z,1;-x+1/2,y+1/2,z+1/2,1;-y+1/2,-x+1/2,-z+1/2,1;x,y,z,-1                                          ")
     Shubnikov_info( 967)= Shub_Spgr_Info_Type("118.309","P-4'n'2     ","118.3.967  ","P-4'n'2       ","P-4'n'2                    ","P -4' -2n'          ",  &
                                               "y,-x,-z,-1;-x+1/2,y+1/2,z+1/2,-1;-y+1/2,-x+1/2,-z+1/2,1                                                 ")
     Shubnikov_info( 968)= Shub_Spgr_Info_Type("118.310","P-4'n2'     ","118.4.968  ","P-4'n2'       ","P-4'n2'                    ","P -4' -2n           ",  &
                                               "y,-x,-z,-1;-x+1/2,y+1/2,z+1/2,1;-y+1/2,-x+1/2,-z+1/2,-1                                                 ")
     Shubnikov_info( 969)= Shub_Spgr_Info_Type("118.311","P-4n'2'     ","118.5.969  ","P-4n'2'       ","P-4n'2'                    ","P -4 -2n'           ",  &
                                               "y,-x,-z,1;-x+1/2,y+1/2,z+1/2,-1;-y+1/2,-x+1/2,-z+1/2,-1                                                 ")
     Shubnikov_info( 970)= Shub_Spgr_Info_Type("122.338","I_c-42d     ","118.6.970  ","P_I-4n2       ","I-42d.1'_c[rP-4n2]         ","I -4 2bw 1'c        ",  &
                                               "x+1/2,y+1/2,z+1/2,1;y,-x,-z,1;x,-y+1/2,-z+1/4,1;y,x+1/2,z+1/4,1;x,y,z+1/2,-1                            ")
     Shubnikov_info( 971)= Shub_Spgr_Info_Type("119.315","I-4m2       ","119.1.971  ","I-4m2         ","I-4m2.1                    ","I -4 -2             ",  &
                                               "x+1/2,y+1/2,z+1/2,1;y,-x,-z,1;-x,y,z,1;-y,-x,-z,1                                                       ")
     Shubnikov_info( 972)= Shub_Spgr_Info_Type("119.316","I-4m21'     ","119.2.972  ","I-4m21'       ","I-4m2.1'                   ","I -4 -2 1'          ",  &
                                               "x+1/2,y+1/2,z+1/2,1;y,-x,-z,1;-x,y,z,1;-y,-x,-z,1;x,y,z,-1                                              ")
     Shubnikov_info( 973)= Shub_Spgr_Info_Type("119.317","I-4'm'2     ","119.3.973  ","I-4'm'2       ","I-4'm'2                    ","I -4' -2'           ",  &
                                               "x+1/2,y+1/2,z+1/2,1;y,-x,-z,-1;-x,y,z,-1;-y,-x,-z,1                                                     ")
     Shubnikov_info( 974)= Shub_Spgr_Info_Type("119.318","I-4'm2'     ","119.4.974  ","I-4'm2'       ","I-4'm2'                    ","I -4' -2            ",  &
                                               "x+1/2,y+1/2,z+1/2,1;y,-x,-z,-1;-x,y,z,1;-y,-x,-z,-1                                                     ")
     Shubnikov_info( 975)= Shub_Spgr_Info_Type("119.319","I-4m'2'     ","119.5.975  ","I-4m'2'       ","I-4m'2'                    ","I -4 -2'            ",  &
                                               "x+1/2,y+1/2,z+1/2,1;y,-x,-z,1;-x,y,z,-1;-y,-x,-z,-1                                                     ")
     Shubnikov_info( 976)= Shub_Spgr_Info_Type("115.290","P_I-4m2     ","119.6.976  ","I_P-4m2       ","P-4m2.1'_I[I-4m2]          ","P -4 -2 1'I         ",  &
                                               "y,-x,-z,1;-x,y,z,1;-y,-x,-z,1;x+1/2,y+1/2,z+1/2,-1                                                      ")
     Shubnikov_info( 977)= Shub_Spgr_Info_Type("118.314","P_I-4n2     ","119.7.977  ","I_P-4'm'2     ","P-4n2.1'_I[I-4m2]          ","P -4 -2n 1'I        ",  &
                                               "y,-x,-z,1;-x+1/2,y+1/2,z+1/2,1;-y+1/2,-x+1/2,-z+1/2,1;x+1/2,y+1/2,z+1/2,-1                              ")
     Shubnikov_info( 978)= Shub_Spgr_Info_Type("120.321","I-4c2       ","120.1.978  ","I-4c2         ","I-4c2.1                    ","I -4 -2c            ",  &
                                               "x+1/2,y+1/2,z+1/2,1;y,-x,-z,1;-x,y,z+1/2,1;-y,-x,-z+1/2,1                                               ")
     Shubnikov_info( 979)= Shub_Spgr_Info_Type("120.322","I-4c21'     ","120.2.979  ","I-4c21'       ","I-4c2.1'                   ","I -4 -2c 1'         ",  &
                                               "x+1/2,y+1/2,z+1/2,1;y,-x,-z,1;-x,y,z+1/2,1;-y,-x,-z+1/2,1;x,y,z,-1                                      ")
     Shubnikov_info( 980)= Shub_Spgr_Info_Type("120.323","I-4'c'2     ","120.3.980  ","I-4'c'2       ","I-4'c'2                    ","I -4' -2c'          ",  &
                                               "x+1/2,y+1/2,z+1/2,1;y,-x,-z,-1;-x,y,z+1/2,-1;-y,-x,-z+1/2,1                                             ")
     Shubnikov_info( 981)= Shub_Spgr_Info_Type("120.324","I-4'c2'     ","120.4.981  ","I-4'c2'       ","I-4'c2'                    ","I -4' -2c           ",  &
                                               "x+1/2,y+1/2,z+1/2,1;y,-x,-z,-1;-x,y,z+1/2,1;-y,-x,-z+1/2,-1                                             ")
     Shubnikov_info( 982)= Shub_Spgr_Info_Type("120.325","I-4c'2'     ","120.5.982  ","I-4c'2'       ","I-4c'2'                    ","I -4 -2c'           ",  &
                                               "x+1/2,y+1/2,z+1/2,1;y,-x,-z,1;-x,y,z+1/2,-1;-y,-x,-z+1/2,-1                                             ")
     Shubnikov_info( 983)= Shub_Spgr_Info_Type("116.298","P_I-4c2     ","120.6.983  ","I_P-4c2       ","P-4c2.1'_I[I-4c2]          ","P -4 -2c 1'I        ",  &
                                               "y,-x,-z,1;-x,y,z+1/2,1;-y,-x,-z+1/2,1;x+1/2,y+1/2,z+1/2,-1                                              ")
     Shubnikov_info( 984)= Shub_Spgr_Info_Type("117.306","P_I-4b2     ","120.7.984  ","I_P-4c'2'     ","P-4b2.1'_I[I-4c2]          ","P -4 -2ab 1'I       ",  &
                                               "y,-x,-z,1;-x+1/2,y+1/2,z,1;-y+1/2,-x+1/2,-z,1;x+1/2,y+1/2,z+1/2,-1                                      ")
     Shubnikov_info( 985)= Shub_Spgr_Info_Type("121.327","I-42m       ","121.1.985  ","I-42m         ","I-42m.1                    ","I -4 2              ",  &
                                               "x+1/2,y+1/2,z+1/2,1;y,-x,-z,1;x,-y,-z,1;y,x,z,1                                                         ")
     Shubnikov_info( 986)= Shub_Spgr_Info_Type("121.328","I-42m1'     ","121.2.986  ","I-42m1'       ","I-42m.1'                   ","I -4 2 1'           ",  &
                                               "x+1/2,y+1/2,z+1/2,1;y,-x,-z,1;x,-y,-z,1;y,x,z,1;x,y,z,-1                                                ")
     Shubnikov_info( 987)= Shub_Spgr_Info_Type("121.329","I-4'2'm     ","121.3.987  ","I-4'2'm       ","I-4'2'm                    ","I -4' 2'            ",  &
                                               "x+1/2,y+1/2,z+1/2,1;y,-x,-z,-1;x,-y,-z,-1;y,x,z,1                                                       ")
     Shubnikov_info( 988)= Shub_Spgr_Info_Type("121.330","I-4'2m'     ","121.4.988  ","I-4'2m'       ","I-4'2m'                    ","I -4' 2             ",  &
                                               "x+1/2,y+1/2,z+1/2,1;y,-x,-z,-1;x,-y,-z,1;y,x,z,-1                                                       ")
     Shubnikov_info( 989)= Shub_Spgr_Info_Type("121.331","I-42'm'     ","121.5.989  ","I-42'm'       ","I-42'm'                    ","I -4 2'             ",  &
                                               "x+1/2,y+1/2,z+1/2,1;y,-x,-z,1;x,-y,-z,-1;y,x,z,-1                                                       ")
     Shubnikov_info( 990)= Shub_Spgr_Info_Type("111.258","P_I-42m     ","121.6.990  ","I_P-42m       ","P-42m.1'_I[I-42m]          ","P -4 2 1'I          ",  &
                                               "y,-x,-z,1;x,-y,-z,1;y,x,z,1;x+1/2,y+1/2,z+1/2,-1                                                        ")
     Shubnikov_info( 991)= Shub_Spgr_Info_Type("113.274","P_I-42_1m   ","121.7.991  ","I_P-4'2'm     ","P-42_1m.1'_I[I-42m]        ","P -4 2ab 1'I        ",  &
                                               "y,-x,-z,1;x+1/2,-y+1/2,-z,1;y+1/2,x+1/2,z,1;x+1/2,y+1/2,z+1/2,-1                                        ")
     Shubnikov_info( 992)= Shub_Spgr_Info_Type("112.266","P_I-42c     ","121.8.992  ","I_P-4'2m'     ","P-42c.1'_I[I-42m]          ","P -4 2c 1'I         ",  &
                                               "y,-x,-z,1;x,-y,-z+1/2,1;y,x,z+1/2,1;x+1/2,y+1/2,z+1/2,-1                                                ")
     Shubnikov_info( 993)= Shub_Spgr_Info_Type("114.282","P_I-42_1c   ","121.9.993  ","I_P-42'm'     ","P-42_1c.1'_I[I-42m]        ","P -4 2n 1'I         ",  &
                                               "y,-x,-z,1;x+1/2,-y+1/2,-z+1/2,1;y+1/2,x+1/2,z+1/2,1;x+1/2,y+1/2,z+1/2,-1                                ")
     Shubnikov_info( 994)= Shub_Spgr_Info_Type("122.333","I-42d       ","122.1.994  ","I-42d         ","I-42d.1                    ","I -4 2bw            ",  &
                                               "x+1/2,y+1/2,z+1/2,1;y,-x,-z,1;x,-y+1/2,-z+1/4,1;y,x+1/2,z+1/4,1                                         ")
     Shubnikov_info( 995)= Shub_Spgr_Info_Type("122.334","I-42d1'     ","122.2.995  ","I-42d1'       ","I-42d.1'                   ","I -4 2bw 1'         ",  &
                                               "x+1/2,y+1/2,z+1/2,1;y,-x,-z,1;x,-y+1/2,-z+1/4,1;y,x+1/2,z+1/4,1;x,y,z,-1                                ")
     Shubnikov_info( 996)= Shub_Spgr_Info_Type("122.335","I-4'2'd     ","122.3.996  ","I-4'2'd       ","I-4'2'd                    ","I -4' 2bw'          ",  &
                                               "x+1/2,y+1/2,z+1/2,1;y,-x,-z,-1;x,-y+1/2,-z+1/4,-1;y,x+1/2,z+1/4,1                                       ")
     Shubnikov_info( 997)= Shub_Spgr_Info_Type("122.336","I-4'2d'     ","122.4.997  ","I-4'2d'       ","I-4'2d'                    ","I -4' 2bw           ",  &
                                               "x+1/2,y+1/2,z+1/2,1;y,-x,-z,-1;x,-y+1/2,-z+1/4,1;y,x+1/2,z+1/4,-1                                       ")
     Shubnikov_info( 998)= Shub_Spgr_Info_Type("122.337","I-42'd'     ","122.5.998  ","I-42'd'       ","I-42'd'                    ","I -4 2bw'           ",  &
                                               "x+1/2,y+1/2,z+1/2,1;y,-x,-z,1;x,-y+1/2,-z+1/4,-1;y,x+1/2,z+1/4,-1                                       ")
     Shubnikov_info( 999)= Shub_Spgr_Info_Type("123.339","P4/mmm      ","123.1.999  ","P4/mmm        ","P4/mmm.1                   ","-P 4 2              ",  &
                                               "-y,x,z,1;x,y,-z,1;-x,y,z,1;y,x,z,1                                                                      ")
     Shubnikov_info(1000)= Shub_Spgr_Info_Type("123.340","P4/mmm1'    ","123.2.1000 ","P4/mmm1'      ","P4/mmm.1'                  ","-P 4 2 1'           ",  &
                                               "-y,x,z,1;x,y,-z,1;-x,y,z,1;y,x,z,1;x,y,z,-1                                                             ")
     Shubnikov_info(1001)= Shub_Spgr_Info_Type("123.341","P4/m'mm     ","123.3.1001 ","P4/m'mm       ","P4/m'mm                    ","P 4 2' -1'          ",  &
                                               "-y,x,z,1;x,y,-z,-1;-x,y,z,1;y,x,z,1                                                                     ")
     Shubnikov_info(1002)= Shub_Spgr_Info_Type("123.342","P4'/mm'm    ","123.4.1002 ","P4'/mm'm      ","P4'/mm'm                   ","-P 4' 2'            ",  &
                                               "-y,x,z,-1;x,y,-z,1;-x,y,z,-1;y,x,z,1                                                                    ")
     Shubnikov_info(1003)= Shub_Spgr_Info_Type("123.343","P4'/mmm'    ","123.5.1003 ","P4'/mmm'      ","P4'/mmm'                   ","-P 4' 2             ",  &
                                               "-y,x,z,-1;x,y,-z,1;-x,y,z,1;y,x,z,-1                                                                    ")
     Shubnikov_info(1004)= Shub_Spgr_Info_Type("123.344","P4'/m'm'm   ","123.6.1004 ","P4'/m'm'm     ","P4'/m'm'm                  ","P 4' 2 -1'          ",  &
                                               "-y,x,z,-1;x,y,-z,-1;-x,y,z,-1;y,x,z,1                                                                   ")
     Shubnikov_info(1005)= Shub_Spgr_Info_Type("123.345","P4/mm'm'    ","123.7.1005 ","P4/mm'm'      ","P4/mm'm'                   ","-P 4 2'             ",  &
                                               "-y,x,z,1;x,y,-z,1;-x,y,z,-1;y,x,z,-1                                                                    ")
     Shubnikov_info(1006)= Shub_Spgr_Info_Type("123.346","P4'/m'mm'   ","123.8.1006 ","P4'/m'mm'     ","P4'/m'mm'                  ","P 4' 2' -1'         ",  &
                                               "-y,x,z,-1;x,y,-z,-1;-x,y,z,1;y,x,z,-1                                                                   ")
     Shubnikov_info(1007)= Shub_Spgr_Info_Type("123.347","P4/m'm'm'   ","123.9.1007 ","P4/m'm'm'     ","P4/m'm'm'                  ","P 4 2 -1'           ",  &
                                               "-y,x,z,1;x,y,-z,-1;-x,y,z,-1;y,x,z,-1                                                                   ")
     Shubnikov_info(1008)= Shub_Spgr_Info_Type("123.348","P_c4/mmm    ","123.10.1008","P_2c4/mmm     ","P4/mmm.1'_c[P4/mmm]        ","-P 4 2 1'c          ",  &
                                               "-y,x,z,1;x,y,-z,1;-x,y,z,1;y,x,z,1;x,y,z+1/2,-1                                                         ")
     Shubnikov_info(1009)= Shub_Spgr_Info_Type("123.349","P_C4/mmm    ","123.11.1009","P_P4/mmm      ","P4/mmm.1'_C[rP4/mmm]       ","-P 4 2 1'C          ",  &
                                               "-y,x,z,1;x,y,-z,1;-x,y,z,1;y,x,z,1;x+1/2,y+1/2,z,-1                                                     ")
     Shubnikov_info(1010)= Shub_Spgr_Info_Type("139.540","I_c4/mmm    ","123.12.1010","P_I4/mmm      ","I4/mmm.1'_c[rP4/mmm]       ","-I 4 2 1'c          ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,1;x,y,-z,1;-x,y,z,1;y,x,z,1;x,y,z+1/2,-1                                     ")
     Shubnikov_info(1011)= Shub_Spgr_Info_Type("132.456","P_c4_2/mcm  ","123.13.1011","P_2c4'/mm'm   ","P4_2/mcm.1'_c[P4/mmm]      ","-P 4c 2c 1'c        ",  &
                                               "-y,x,z+1/2,1;x,y,-z,1;-x,y,z+1/2,1;y,x,z,1;x,y,z+1/2,-1                                                 ")
     Shubnikov_info(1012)= Shub_Spgr_Info_Type("131.444","P_c4_2/mmc  ","123.14.1012","P_2c4'/mmm'   ","P4_2/mmc.1'_c[P4/mmm]      ","-P 4c 2 1'c         ",  &
                                               "-y,x,z+1/2,1;x,y,-z,1;-x,y,z,1;y,x,z+1/2,1;x,y,z+1/2,-1                                                 ")
     Shubnikov_info(1013)= Shub_Spgr_Info_Type("124.360","P_c4/mcc    ","123.15.1013","P_2c4/mm'm'   ","P4/mcc.1'_c[P4/mmm]        ","-P 4 2c 1'c         ",  &
                                               "-y,x,z,1;x,y,-z,1;-x,y,z+1/2,1;y,x,z+1/2,1;x,y,z+1/2,-1                                                 ")
     Shubnikov_info(1014)= Shub_Spgr_Info_Type("129.421","P_C4/nmm    ","123.16.1014","P_P4/m'mm     ","P4/nmm.1'_C[rP4/mmm]       ","-P 4a 2a 1'C        ",  &
                                               "-y+1/2,x,z,1;x+1/2,y+1/2,-z,1;-x+1/2,y,z,1;y,x,z,1;x+1/2,y+1/2,z,-1                                     ")
     Shubnikov_info(1015)= Shub_Spgr_Info_Type("127.397","P_C4/mbm    ","123.17.1015","P_P4'/mmm'    ","P4/mbm.1'_C[rP4/mmm]       ","-P 4 2ab 1'C        ",  &
                                               "-y,x,z,1;x,y,-z,1;-x+1/2,y+1/2,z,1;y+1/2,x+1/2,z,1;x+1/2,y+1/2,z,-1                                     ")
     Shubnikov_info(1016)= Shub_Spgr_Info_Type("125.373","P_C4/nbm    ","123.18.1016","P_P4'/m'mm'   ","P4/nbm.1'_C[rP4/mmm]       ","-P 4a 2b 1'C        ",  &
                                               "-y+1/2,x,z,1;x+1/2,y+1/2,-z,1;-x,y+1/2,z,1;y+1/2,x+1/2,z,1;x+1/2,y+1/2,z,-1                             ")
     Shubnikov_info(1017)= Shub_Spgr_Info_Type("140.550","I_c4/mcm    ","123.19.1017","P_I4/mm'm'    ","I4/mcm.1'_c[rP4/mmm]       ","-I 4 2c 1'c         ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,1;x,y,-z,1;-x,y,z+1/2,1;y,x,z+1/2,1;x,y,z+1/2,-1                             ")
     Shubnikov_info(1018)= Shub_Spgr_Info_Type("124.351","P4/mcc      ","124.1.1018 ","P4/mcc        ","P4/mcc.1                   ","-P 4 2c             ",  &
                                               "-y,x,z,1;x,y,-z,1;-x,y,z+1/2,1;y,x,z+1/2,1                                                              ")
     Shubnikov_info(1019)= Shub_Spgr_Info_Type("124.352","P4/mcc1'    ","124.2.1019 ","P4/mcc1'      ","P4/mcc.1'                  ","-P 4 2c 1'          ",  &
                                               "-y,x,z,1;x,y,-z,1;-x,y,z+1/2,1;y,x,z+1/2,1;x,y,z,-1                                                     ")
     Shubnikov_info(1020)= Shub_Spgr_Info_Type("124.353","P4/m'cc     ","124.3.1020 ","P4/m'cc       ","P4/m'cc                    ","P 4 2c' -1'         ",  &
                                               "-y,x,z,1;x,y,-z,-1;-x,y,z+1/2,1;y,x,z+1/2,1                                                             ")
     Shubnikov_info(1021)= Shub_Spgr_Info_Type("124.354","P4'/mc'c    ","124.4.1021 ","P4'/mc'c      ","P4'/mc'c                   ","-P 4' 2c'           ",  &
                                               "-y,x,z,-1;x,y,-z,1;-x,y,z+1/2,-1;y,x,z+1/2,1                                                            ")
     Shubnikov_info(1022)= Shub_Spgr_Info_Type("124.355","P4'/mcc'    ","124.5.1022 ","P4'/mcc'      ","P4'/mcc'                   ","-P 4' 2c            ",  &
                                               "-y,x,z,-1;x,y,-z,1;-x,y,z+1/2,1;y,x,z+1/2,-1                                                            ")
     Shubnikov_info(1023)= Shub_Spgr_Info_Type("124.356","P4'/m'c'c   ","124.6.1023 ","P4'/m'c'c     ","P4'/m'c'c                  ","P 4' 2c -1'         ",  &
                                               "-y,x,z,-1;x,y,-z,-1;-x,y,z+1/2,-1;y,x,z+1/2,1                                                           ")
     Shubnikov_info(1024)= Shub_Spgr_Info_Type("124.357","P4/mc'c'    ","124.7.1024 ","P4/mc'c'      ","P4/mc'c'                   ","-P 4 2c'            ",  &
                                               "-y,x,z,1;x,y,-z,1;-x,y,z+1/2,-1;y,x,z+1/2,-1                                                            ")
     Shubnikov_info(1025)= Shub_Spgr_Info_Type("124.358","P4'/m'cc'   ","124.8.1025 ","P4'/m'cc'     ","P4'/m'cc'                  ","P 4' 2c' -1'        ",  &
                                               "-y,x,z,-1;x,y,-z,-1;-x,y,z+1/2,1;y,x,z+1/2,-1                                                           ")
     Shubnikov_info(1026)= Shub_Spgr_Info_Type("124.359","P4/m'c'c'   ","124.9.1026 ","P4/m'c'c'     ","P4/m'c'c'                  ","P 4 2c -1'          ",  &
                                               "-y,x,z,1;x,y,-z,-1;-x,y,z+1/2,-1;y,x,z+1/2,-1                                                           ")
     Shubnikov_info(1027)= Shub_Spgr_Info_Type("124.361","P_C4/mcc    ","124.10.1027","P_P4/mcc      ","P4/mcc.1'_C[rP4/mcc]       ","-P 4 2c 1'C         ",  &
                                               "-y,x,z,1;x,y,-z,1;-x,y,z+1/2,1;y,x,z+1/2,1;x+1/2,y+1/2,z,-1                                             ")
     Shubnikov_info(1028)= Shub_Spgr_Info_Type("130.433","P_C4/ncc    ","124.11.1028","P_P4/m'cc     ","P4/ncc.1'_C[rP4/mcc]       ","-P 4a 2ac 1'C       ",  &
                                               "-y+1/2,x,z,1;x+1/2,y+1/2,-z,1;-x+1/2,y,z+1/2,1;y,x,z+1/2,1;x+1/2,y+1/2,z,-1                             ")
     Shubnikov_info(1029)= Shub_Spgr_Info_Type("128.409","P_C4/mnc    ","124.12.1029","P_P4'/mcc'    ","P4/mnc.1'_C[rP4/mcc]       ","-P 4 2n 1'C         ",  &
                                               "-y,x,z,1;x,y,-z,1;-x+1/2,y+1/2,z+1/2,1;y+1/2,x+1/2,z+1/2,1;x+1/2,y+1/2,z,-1                             ")
     Shubnikov_info(1030)= Shub_Spgr_Info_Type("126.385","P_C4/nnc    ","124.13.1030","P_P4'/m'cc'   ","P4/nnc.1'_C[rP4/mcc]       ","-P 4a 2bc 1'C       ",  &
                                               "-y+1/2,x,z,1;x+1/2,y+1/2,-z,1;-x,y+1/2,z+1/2,1;y+1/2,x+1/2,z+1/2,1;x+1/2,y+1/2,z,-1                     ")
     Shubnikov_info(1031)= Shub_Spgr_Info_Type("125.363","P4/nbm      ","125.1.1031 ","P4/nbm        ","P4/nbm.1                   ","-P 4a 2b            ",  &
                                               "-y+1/2,x,z,1;x+1/2,y+1/2,-z,1;-x,y+1/2,z,1;y+1/2,x+1/2,z,1                                              ")
     Shubnikov_info(1032)= Shub_Spgr_Info_Type("125.364","P4/nbm1'    ","125.2.1032 ","P4/nbm1'      ","P4/nbm.1'                  ","-P 4a 2b 1'         ",  &
                                               "-y+1/2,x,z,1;x+1/2,y+1/2,-z,1;-x,y+1/2,z,1;y+1/2,x+1/2,z,1;x,y,z,-1                                     ")
     Shubnikov_info(1033)= Shub_Spgr_Info_Type("125.365","P4/n'bm     ","125.3.1033 ","P4/n'bm       ","P4/n'bm                    ","P 4a 2b' -1'        ",  &
                                               "-y+1/2,x,z,1;x+1/2,y+1/2,-z,-1;-x,y+1/2,z,1;y+1/2,x+1/2,z,1                                             ")
     Shubnikov_info(1034)= Shub_Spgr_Info_Type("125.366","P4'/nb'm    ","125.4.1034 ","P4'/nb'm      ","P4'/nb'm                   ","-P 4a' 2b'          ",  &
                                               "-y+1/2,x,z,-1;x+1/2,y+1/2,-z,1;-x,y+1/2,z,-1;y+1/2,x+1/2,z,1                                            ")
     Shubnikov_info(1035)= Shub_Spgr_Info_Type("125.367","P4'/nbm'    ","125.5.1035 ","P4'/nbm'      ","P4'/nbm'                   ","-P 4a' 2b           ",  &
                                               "-y+1/2,x,z,-1;x+1/2,y+1/2,-z,1;-x,y+1/2,z,1;y+1/2,x+1/2,z,-1                                            ")
     Shubnikov_info(1036)= Shub_Spgr_Info_Type("125.368","P4'/n'b'm   ","125.6.1036 ","P4'/n'b'm     ","P4'/n'b'm                  ","P 4a' 2b -1'        ",  &
                                               "-y+1/2,x,z,-1;x+1/2,y+1/2,-z,-1;-x,y+1/2,z,-1;y+1/2,x+1/2,z,1                                           ")
     Shubnikov_info(1037)= Shub_Spgr_Info_Type("125.369","P4/nb'm'    ","125.7.1037 ","P4/nb'm'      ","P4/nb'm'                   ","-P 4a 2b'           ",  &
                                               "-y+1/2,x,z,1;x+1/2,y+1/2,-z,1;-x,y+1/2,z,-1;y+1/2,x+1/2,z,-1                                            ")
     Shubnikov_info(1038)= Shub_Spgr_Info_Type("125.370","P4'/n'bm'   ","125.8.1038 ","P4'/n'bm'     ","P4'/n'bm'                  ","P 4a' 2b' -1'       ",  &
                                               "-y+1/2,x,z,-1;x+1/2,y+1/2,-z,-1;-x,y+1/2,z,1;y+1/2,x+1/2,z,-1                                           ")
     Shubnikov_info(1039)= Shub_Spgr_Info_Type("125.371","P4/n'b'm'   ","125.9.1039 ","P4/n'b'm'     ","P4/n'b'm'                  ","P 4a 2b -1'         ",  &
                                               "-y+1/2,x,z,1;x+1/2,y+1/2,-z,-1;-x,y+1/2,z,-1;y+1/2,x+1/2,z,-1                                           ")
     Shubnikov_info(1040)= Shub_Spgr_Info_Type("125.372","P_c4/nbm    ","125.10.1040","P_2c4/nbm     ","P4/nbm.1'_c[P4/nbm]        ","-P 4a 2b 1'c        ",  &
                                               "-y+1/2,x,z,1;x+1/2,y+1/2,-z,1;-x,y+1/2,z,1;y+1/2,x+1/2,z,1;x,y,z+1/2,-1                                 ")
     Shubnikov_info(1041)= Shub_Spgr_Info_Type("134.480","P_c4_2/nnm  ","125.11.1041","P_2c4'/nb'm   ","P4_2/nnm.1'_c[P4/nbm]      ","-P 4ac 2bc 1'c      ",  &
                                               "-y+1/2,x,z+1/2,1;x+1/2,y+1/2,-z,1;-x,y+1/2,z+1/2,1;y+1/2,x+1/2,z,1;x,y,z+1/2,-1                         ")
     Shubnikov_info(1042)= Shub_Spgr_Info_Type("133.468","P_c4_2/nbc  ","125.12.1042","P_2c4'/nbm'   ","P4_2/nbc.1'_c[P4/nbm]      ","-P 4ac 2b 1'c       ",  &
                                               "-y+1/2,x,z+1/2,1;x+1/2,y+1/2,-z,1;-x,y+1/2,z,1;y+1/2,x+1/2,z+1/2,1;x,y,z+1/2,-1                         ")
     Shubnikov_info(1043)= Shub_Spgr_Info_Type("126.384","P_c4/nnc    ","125.13.1043","P_2c4/nb'm'   ","P4/nnc.1'_c[P4/nbm]        ","-P 4a 2bc 1'c       ",  &
                                               "-y+1/2,x,z,1;x+1/2,y+1/2,-z,1;-x,y+1/2,z+1/2,1;y+1/2,x+1/2,z+1/2,1;x,y,z+1/2,-1                         ")
     Shubnikov_info(1044)= Shub_Spgr_Info_Type("126.375","P4/nnc      ","126.1.1044 ","P4/nnc        ","P4/nnc.1                   ","-P 4a 2bc           ",  &
                                               "-y+1/2,x,z,1;x+1/2,y+1/2,-z,1;-x,y+1/2,z+1/2,1;y+1/2,x+1/2,z+1/2,1                                      ")
     Shubnikov_info(1045)= Shub_Spgr_Info_Type("126.376","P4/nnc1'    ","126.2.1045 ","P4/nnc1'      ","P4/nnc.1'                  ","-P 4a 2bc 1'        ",  &
                                               "-y+1/2,x,z,1;x+1/2,y+1/2,-z,1;-x,y+1/2,z+1/2,1;y+1/2,x+1/2,z+1/2,1;x,y,z,-1                             ")
     Shubnikov_info(1046)= Shub_Spgr_Info_Type("126.377","P4/n'nc     ","126.3.1046 ","P4/n'nc       ","P4/n'nc                    ","P 4a 2bc' -1'       ",  &
                                               "-y+1/2,x,z,1;x+1/2,y+1/2,-z,-1;-x,y+1/2,z+1/2,1;y+1/2,x+1/2,z+1/2,1                                     ")
     Shubnikov_info(1047)= Shub_Spgr_Info_Type("126.378","P4'/nn'c    ","126.4.1047 ","P4'/nn'c      ","P4'/nn'c                   ","-P 4a' 2bc'         ",  &
                                               "-y+1/2,x,z,-1;x+1/2,y+1/2,-z,1;-x,y+1/2,z+1/2,-1;y+1/2,x+1/2,z+1/2,1                                    ")
     Shubnikov_info(1048)= Shub_Spgr_Info_Type("126.379","P4'/nnc'    ","126.5.1048 ","P4'/nnc'      ","P4'/nnc'                   ","-P 4a' 2bc          ",  &
                                               "-y+1/2,x,z,-1;x+1/2,y+1/2,-z,1;-x,y+1/2,z+1/2,1;y+1/2,x+1/2,z+1/2,-1                                    ")
     Shubnikov_info(1049)= Shub_Spgr_Info_Type("126.380","P4'/n'n'c   ","126.6.1049 ","P4'/n'n'c     ","P4'/n'n'c                  ","P 4a' 2bc -1'       ",  &
                                               "-y+1/2,x,z,-1;x+1/2,y+1/2,-z,-1;-x,y+1/2,z+1/2,-1;y+1/2,x+1/2,z+1/2,1                                   ")
     Shubnikov_info(1050)= Shub_Spgr_Info_Type("126.381","P4/nn'c'    ","126.7.1050 ","P4/nn'c'      ","P4/nn'c'                   ","-P 4a 2bc'          ",  &
                                               "-y+1/2,x,z,1;x+1/2,y+1/2,-z,1;-x,y+1/2,z+1/2,-1;y+1/2,x+1/2,z+1/2,-1                                    ")
     Shubnikov_info(1051)= Shub_Spgr_Info_Type("126.382","P4'/n'nc'   ","126.8.1051 ","P4'/n'nc'     ","P4'/n'nc'                  ","P 4a' 2bc' -1'      ",  &
                                               "-y+1/2,x,z,-1;x+1/2,y+1/2,-z,-1;-x,y+1/2,z+1/2,1;y+1/2,x+1/2,z+1/2,-1                                   ")
     Shubnikov_info(1052)= Shub_Spgr_Info_Type("126.383","P4/n'n'c'   ","126.9.1052 ","P4/n'n'c'     ","P4/n'n'c'                  ","P 4a 2bc -1'        ",  &
                                               "-y+1/2,x,z,1;x+1/2,y+1/2,-z,-1;-x,y+1/2,z+1/2,-1;y+1/2,x+1/2,z+1/2,-1                                   ")
     Shubnikov_info(1053)= Shub_Spgr_Info_Type("127.387","P4/mbm      ","127.1.1053 ","P4/mbm        ","P4/mbm.1                   ","-P 4 2ab            ",  &
                                               "-y,x,z,1;x,y,-z,1;-x+1/2,y+1/2,z,1;y+1/2,x+1/2,z,1                                                      ")
     Shubnikov_info(1054)= Shub_Spgr_Info_Type("127.388","P4/mbm1'    ","127.2.1054 ","P4/mbm1'      ","P4/mbm.1'                  ","-P 4 2ab 1'         ",  &
                                               "-y,x,z,1;x,y,-z,1;-x+1/2,y+1/2,z,1;y+1/2,x+1/2,z,1;x,y,z,-1                                             ")
     Shubnikov_info(1055)= Shub_Spgr_Info_Type("127.389","P4/m'bm     ","127.3.1055 ","P4/m'bm       ","P4/m'bm                    ","P 4 2ab' -1'        ",  &
                                               "-y,x,z,1;x,y,-z,-1;-x+1/2,y+1/2,z,1;y+1/2,x+1/2,z,1                                                     ")
     Shubnikov_info(1056)= Shub_Spgr_Info_Type("127.390","P4'/mb'm    ","127.4.1056 ","P4'/mb'm      ","P4'/mb'm                   ","-P 4' 2ab'          ",  &
                                               "-y,x,z,-1;x,y,-z,1;-x+1/2,y+1/2,z,-1;y+1/2,x+1/2,z,1                                                    ")
     Shubnikov_info(1057)= Shub_Spgr_Info_Type("127.391","P4'/mbm'    ","127.5.1057 ","P4'/mbm'      ","P4'/mbm'                   ","-P 4' 2ab           ",  &
                                               "-y,x,z,-1;x,y,-z,1;-x+1/2,y+1/2,z,1;y+1/2,x+1/2,z,-1                                                    ")
     Shubnikov_info(1058)= Shub_Spgr_Info_Type("127.392","P4'/m'b'm   ","127.6.1058 ","P4'/m'b'm     ","P4'/m'b'm                  ","P 4' 2ab -1'        ",  &
                                               "-y,x,z,-1;x,y,-z,-1;-x+1/2,y+1/2,z,-1;y+1/2,x+1/2,z,1                                                   ")
     Shubnikov_info(1059)= Shub_Spgr_Info_Type("127.393","P4/mb'm'    ","127.7.1059 ","P4/mb'm'      ","P4/mb'm'                   ","-P 4 2ab'           ",  &
                                               "-y,x,z,1;x,y,-z,1;-x+1/2,y+1/2,z,-1;y+1/2,x+1/2,z,-1                                                    ")
     Shubnikov_info(1060)= Shub_Spgr_Info_Type("127.394","P4'/m'bm'   ","127.8.1060 ","P4'/m'bm'     ","P4'/m'bm'                  ","P 4' 2ab' -1'       ",  &
                                               "-y,x,z,-1;x,y,-z,-1;-x+1/2,y+1/2,z,1;y+1/2,x+1/2,z,-1                                                   ")
     Shubnikov_info(1061)= Shub_Spgr_Info_Type("127.395","P4/m'b'm'   ","127.9.1061 ","P4/m'b'm'     ","P4/m'b'm'                  ","P 4 2ab -1'         ",  &
                                               "-y,x,z,1;x,y,-z,-1;-x+1/2,y+1/2,z,-1;y+1/2,x+1/2,z,-1                                                   ")
     Shubnikov_info(1062)= Shub_Spgr_Info_Type("127.396","P_c4/mbm    ","127.10.1062","P_2c4/mbm     ","P4/mbm.1'_c[P4/mbm]        ","-P 4 2ab 1'c        ",  &
                                               "-y,x,z,1;x,y,-z,1;-x+1/2,y+1/2,z,1;y+1/2,x+1/2,z,1;x,y,z+1/2,-1                                         ")
     Shubnikov_info(1063)= Shub_Spgr_Info_Type("136.504","P_c4_2/mnm  ","127.11.1063","P_2c4'/mb'm   ","P4_2/mnm.1'_c[P4/mbm]      ","-P 4n 2n 1'c        ",  &
                                               "-y+1/2,x+1/2,z+1/2,1;x,y,-z,1;-x+1/2,y+1/2,z+1/2,1;y,x,z,1;x,y,z+1/2,-1                                 ")
     Shubnikov_info(1064)= Shub_Spgr_Info_Type("135.492","P_c4_2/mbc  ","127.12.1064","P_2c4'/mbm'   ","P4_2/mbc.1'_c[P4/mbm]      ","-P 4c 2ab 1'c       ",  &
                                               "-y,x,z+1/2,1;x,y,-z,1;-x+1/2,y+1/2,z,1;y+1/2,x+1/2,z+1/2,1;x,y,z+1/2,-1                                 ")
     Shubnikov_info(1065)= Shub_Spgr_Info_Type("128.408","P_c4/mnc    ","127.13.1065","P_2c4/mb'm'   ","P4/mnc.1'_c[P4/mbm]        ","-P 4 2n 1'c         ",  &
                                               "-y,x,z,1;x,y,-z,1;-x+1/2,y+1/2,z+1/2,1;y+1/2,x+1/2,z+1/2,1;x,y,z+1/2,-1                                 ")
     Shubnikov_info(1066)= Shub_Spgr_Info_Type("128.399","P4/mnc      ","128.1.1066 ","P4/mnc        ","P4/mnc.1                   ","-P 4 2n             ",  &
                                               "-y,x,z,1;x,y,-z,1;-x+1/2,y+1/2,z+1/2,1;y+1/2,x+1/2,z+1/2,1                                              ")
     Shubnikov_info(1067)= Shub_Spgr_Info_Type("128.400","P4/mnc1'    ","128.2.1067 ","P4/mnc1'      ","P4/mnc.1'                  ","-P 4 2n 1'          ",  &
                                               "-y,x,z,1;x,y,-z,1;-x+1/2,y+1/2,z+1/2,1;y+1/2,x+1/2,z+1/2,1;x,y,z,-1                                     ")
     Shubnikov_info(1068)= Shub_Spgr_Info_Type("128.401","P4/m'nc     ","128.3.1068 ","P4/m'nc       ","P4/m'nc                    ","P 4 2n' -1'         ",  &
                                               "-y,x,z,1;x,y,-z,-1;-x+1/2,y+1/2,z+1/2,1;y+1/2,x+1/2,z+1/2,1                                             ")
     Shubnikov_info(1069)= Shub_Spgr_Info_Type("128.402","P4'/mn'c    ","128.4.1069 ","P4'/mn'c      ","P4'/mn'c                   ","-P 4' 2n'           ",  &
                                               "-y,x,z,-1;x,y,-z,1;-x+1/2,y+1/2,z+1/2,-1;y+1/2,x+1/2,z+1/2,1                                            ")
     Shubnikov_info(1070)= Shub_Spgr_Info_Type("128.403","P4'/mnc'    ","128.5.1070 ","P4'/mnc'      ","P4'/mnc'                   ","-P 4' 2n            ",  &
                                               "-y,x,z,-1;x,y,-z,1;-x+1/2,y+1/2,z+1/2,1;y+1/2,x+1/2,z+1/2,-1                                            ")
     Shubnikov_info(1071)= Shub_Spgr_Info_Type("128.404","P4'/m'n'c   ","128.6.1071 ","P4'/m'n'c     ","P4'/m'n'c                  ","P 4' 2n -1'         ",  &
                                               "-y,x,z,-1;x,y,-z,-1;-x+1/2,y+1/2,z+1/2,-1;y+1/2,x+1/2,z+1/2,1                                           ")
     Shubnikov_info(1072)= Shub_Spgr_Info_Type("128.405","P4/mn'c'    ","128.7.1072 ","P4/mn'c'      ","P4/mn'c'                   ","-P 4 2n'            ",  &
                                               "-y,x,z,1;x,y,-z,1;-x+1/2,y+1/2,z+1/2,-1;y+1/2,x+1/2,z+1/2,-1                                            ")
     Shubnikov_info(1073)= Shub_Spgr_Info_Type("128.406","P4'/m'nc'   ","128.8.1073 ","P4'/m'nc'     ","P4'/m'nc'                  ","P 4' 2n' -1'        ",  &
                                               "-y,x,z,-1;x,y,-z,-1;-x+1/2,y+1/2,z+1/2,1;y+1/2,x+1/2,z+1/2,-1                                           ")
     Shubnikov_info(1074)= Shub_Spgr_Info_Type("128.407","P4/m'n'c'   ","128.9.1074 ","P4/m'n'c'     ","P4/m'n'c'                  ","P 4 2n -1'          ",  &
                                               "-y,x,z,1;x,y,-z,-1;-x+1/2,y+1/2,z+1/2,-1;y+1/2,x+1/2,z+1/2,-1                                           ")
     Shubnikov_info(1075)= Shub_Spgr_Info_Type("129.411","P4/nmm      ","129.1.1075 ","P4/nmm        ","P4/nmm.1                   ","-P 4a 2a            ",  &
                                               "-y+1/2,x,z,1;x+1/2,y+1/2,-z,1;-x+1/2,y,z,1;y,x,z,1                                                      ")
     Shubnikov_info(1076)= Shub_Spgr_Info_Type("129.412","P4/nmm1'    ","129.2.1076 ","P4/nmm1'      ","P4/nmm.1'                  ","-P 4a 2a 1'         ",  &
                                               "-y+1/2,x,z,1;x+1/2,y+1/2,-z,1;-x+1/2,y,z,1;y,x,z,1;x,y,z,-1                                             ")
     Shubnikov_info(1077)= Shub_Spgr_Info_Type("129.413","P4/n'mm     ","129.3.1077 ","P4/n'mm       ","P4/n'mm                    ","P 4a 2a' -1'        ",  &
                                               "-y+1/2,x,z,1;x+1/2,y+1/2,-z,-1;-x+1/2,y,z,1;y,x,z,1                                                     ")
     Shubnikov_info(1078)= Shub_Spgr_Info_Type("129.414","P4'/nm'm    ","129.4.1078 ","P4'/nm'm      ","P4'/nm'm                   ","-P 4a' 2a'          ",  &
                                               "-y+1/2,x,z,-1;x+1/2,y+1/2,-z,1;-x+1/2,y,z,-1;y,x,z,1                                                    ")
     Shubnikov_info(1079)= Shub_Spgr_Info_Type("129.415","P4'/nmm'    ","129.5.1079 ","P4'/nmm'      ","P4'/nmm'                   ","-P 4a' 2a           ",  &
                                               "-y+1/2,x,z,-1;x+1/2,y+1/2,-z,1;-x+1/2,y,z,1;y,x,z,-1                                                    ")
     Shubnikov_info(1080)= Shub_Spgr_Info_Type("129.416","P4'/n'm'm   ","129.6.1080 ","P4'/n'm'm     ","P4'/n'm'm                  ","P 4a' 2a -1'        ",  &
                                               "-y+1/2,x,z,-1;x+1/2,y+1/2,-z,-1;-x+1/2,y,z,-1;y,x,z,1                                                   ")
     Shubnikov_info(1081)= Shub_Spgr_Info_Type("129.417","P4/nm'm'    ","129.7.1081 ","P4/nm'm'      ","P4/nm'm'                   ","-P 4a 2a'           ",  &
                                               "-y+1/2,x,z,1;x+1/2,y+1/2,-z,1;-x+1/2,y,z,-1;y,x,z,-1                                                    ")
     Shubnikov_info(1082)= Shub_Spgr_Info_Type("129.418","P4'/n'mm'   ","129.8.1082 ","P4'/n'mm'     ","P4'/n'mm'                  ","P 4a' 2a' -1'       ",  &
                                               "-y+1/2,x,z,-1;x+1/2,y+1/2,-z,-1;-x+1/2,y,z,1;y,x,z,-1                                                   ")
     Shubnikov_info(1083)= Shub_Spgr_Info_Type("129.419","P4/n'm'm'   ","129.9.1083 ","P4/n'm'm'     ","P4/n'm'm'                  ","P 4a 2a -1'         ",  &
                                               "-y+1/2,x,z,1;x+1/2,y+1/2,-z,-1;-x+1/2,y,z,-1;y,x,z,-1                                                   ")
     Shubnikov_info(1084)= Shub_Spgr_Info_Type("129.420","P_c4/nmm    ","129.10.1084","P_2c4/nmm     ","P4/nmm.1'_c[P4/nmm]        ","-P 4a 2a 1'c        ",  &
                                               "-y+1/2,x,z,1;x+1/2,y+1/2,-z,1;-x+1/2,y,z,1;y,x,z,1;x,y,z+1/2,-1                                         ")
     Shubnikov_info(1085)= Shub_Spgr_Info_Type("138.528","P_c4_2/ncm  ","129.11.1085","P_2c4'/nm'm   ","P4_2/ncm.1'_c[P4/nmm]      ","-P 4ac 2ac 1'c      ",  &
                                               "-y+1/2,x,z+1/2,1;x+1/2,y+1/2,-z,1;-x+1/2,y,z+1/2,1;y,x,z,1;x,y,z+1/2,-1                                 ")
     Shubnikov_info(1086)= Shub_Spgr_Info_Type("137.516","P_c4_2/nmc  ","129.12.1086","P_2c4'/nmm'   ","P4_2/nmc.1'_c[P4/nmm]      ","-P 4ac 2a 1'c       ",  &
                                               "-y+1/2,x,z+1/2,1;x+1/2,y+1/2,-z,1;-x+1/2,y,z,1;y,x,z+1/2,1;x,y,z+1/2,-1                                 ")
     Shubnikov_info(1087)= Shub_Spgr_Info_Type("130.432","P_c4/ncc    ","129.13.1087","P_2c4/nm'm'   ","P4/ncc.1'_c[P4/nmm]        ","-P 4a 2ac 1'c       ",  &
                                               "-y+1/2,x,z,1;x+1/2,y+1/2,-z,1;-x+1/2,y,z+1/2,1;y,x,z+1/2,1;x,y,z+1/2,-1                                 ")
     Shubnikov_info(1088)= Shub_Spgr_Info_Type("130.423","P4/ncc      ","130.1.1088 ","P4/ncc        ","P4/ncc.1                   ","-P 4a 2ac           ",  &
                                               "-y+1/2,x,z,1;x+1/2,y+1/2,-z,1;-x+1/2,y,z+1/2,1;y,x,z+1/2,1                                              ")
     Shubnikov_info(1089)= Shub_Spgr_Info_Type("130.424","P4/ncc1'    ","130.2.1089 ","P4/ncc1'      ","P4/ncc.1'                  ","-P 4a 2ac 1'        ",  &
                                               "-y+1/2,x,z,1;x+1/2,y+1/2,-z,1;-x+1/2,y,z+1/2,1;y,x,z+1/2,1;x,y,z,-1                                     ")
     Shubnikov_info(1090)= Shub_Spgr_Info_Type("130.425","P4/n'cc     ","130.3.1090 ","P4/n'cc       ","P4/n'cc                    ","P 4a 2ac' -1'       ",  &
                                               "-y+1/2,x,z,1;x+1/2,y+1/2,-z,-1;-x+1/2,y,z+1/2,1;y,x,z+1/2,1                                             ")
     Shubnikov_info(1091)= Shub_Spgr_Info_Type("130.426","P4'/nc'c    ","130.4.1091 ","P4'/nc'c      ","P4'/nc'c                   ","-P 4a' 2ac'         ",  &
                                               "-y+1/2,x,z,-1;x+1/2,y+1/2,-z,1;-x+1/2,y,z+1/2,-1;y,x,z+1/2,1                                            ")
     Shubnikov_info(1092)= Shub_Spgr_Info_Type("130.427","P4'/ncc'    ","130.5.1092 ","P4'/ncc'      ","P4'/ncc'                   ","-P 4a' 2ac          ",  &
                                               "-y+1/2,x,z,-1;x+1/2,y+1/2,-z,1;-x+1/2,y,z+1/2,1;y,x,z+1/2,-1                                            ")
     Shubnikov_info(1093)= Shub_Spgr_Info_Type("130.428","P4'/n'c'c   ","130.6.1093 ","P4'/n'c'c     ","P4'/n'c'c                  ","P 4a' 2ac -1'       ",  &
                                               "-y+1/2,x,z,-1;x+1/2,y+1/2,-z,-1;-x+1/2,y,z+1/2,-1;y,x,z+1/2,1                                           ")
     Shubnikov_info(1094)= Shub_Spgr_Info_Type("130.429","P4/nc'c'    ","130.7.1094 ","P4/nc'c'      ","P4/nc'c'                   ","-P 4a 2ac'          ",  &
                                               "-y+1/2,x,z,1;x+1/2,y+1/2,-z,1;-x+1/2,y,z+1/2,-1;y,x,z+1/2,-1                                            ")
     Shubnikov_info(1095)= Shub_Spgr_Info_Type("130.430","P4'/n'cc'   ","130.8.1095 ","P4'/n'cc'     ","P4'/n'cc'                  ","P 4a' 2ac' -1'      ",  &
                                               "-y+1/2,x,z,-1;x+1/2,y+1/2,-z,-1;-x+1/2,y,z+1/2,1;y,x,z+1/2,-1                                           ")
     Shubnikov_info(1096)= Shub_Spgr_Info_Type("130.431","P4/n'c'c'   ","130.9.1096 ","P4/n'c'c'     ","P4/n'c'c'                  ","P 4a 2ac -1'        ",  &
                                               "-y+1/2,x,z,1;x+1/2,y+1/2,-z,-1;-x+1/2,y,z+1/2,-1;y,x,z+1/2,-1                                           ")
     Shubnikov_info(1097)= Shub_Spgr_Info_Type("131.435","P4_2/mmc    ","131.1.1097 ","P4_2/mmc      ","P4_2/mmc.1                 ","-P 4c 2             ",  &
                                               "-y,x,z+1/2,1;x,y,-z,1;-x,y,z,1;y,x,z+1/2,1                                                              ")
     Shubnikov_info(1098)= Shub_Spgr_Info_Type("131.436","P4_2/mmc1'  ","131.2.1098 ","P4_2/mmc1'    ","P4_2/mmc.1'                ","-P 4c 2 1'          ",  &
                                               "-y,x,z+1/2,1;x,y,-z,1;-x,y,z,1;y,x,z+1/2,1;x,y,z,-1                                                     ")
     Shubnikov_info(1099)= Shub_Spgr_Info_Type("131.437","P4_2/m'mc   ","131.3.1099 ","P4_2/m'mc     ","P4_2/m'mc                  ","P 4c 2' -1'         ",  &
                                               "-y,x,z+1/2,1;x,y,-z,-1;-x,y,z,1;y,x,z+1/2,1                                                             ")
     Shubnikov_info(1100)= Shub_Spgr_Info_Type("131.438","P4_2'/mm'c  ","131.4.1100 ","P4_2'/mm'c    ","P4_2'/mm'c                 ","-P 4c' 2'           ",  &
                                               "-y,x,z+1/2,-1;x,y,-z,1;-x,y,z,-1;y,x,z+1/2,1                                                            ")
     Shubnikov_info(1101)= Shub_Spgr_Info_Type("131.439","P4_2'/mmc'  ","131.5.1101 ","P4_2'/mmc'    ","P4_2'/mmc'                 ","-P 4c' 2            ",  &
                                               "-y,x,z+1/2,-1;x,y,-z,1;-x,y,z,1;y,x,z+1/2,-1                                                            ")
     Shubnikov_info(1102)= Shub_Spgr_Info_Type("131.440","P4_2'/m'm'c ","131.6.1102 ","P4_2'/m'm'c   ","P4_2'/m'm'c                ","P 4c' 2 -1'         ",  &
                                               "-y,x,z+1/2,-1;x,y,-z,-1;-x,y,z,-1;y,x,z+1/2,1                                                           ")
     Shubnikov_info(1103)= Shub_Spgr_Info_Type("131.441","P4_2/mm'c'  ","131.7.1103 ","P4_2/mm'c'    ","P4_2/mm'c'                 ","-P 4c 2'            ",  &
                                               "-y,x,z+1/2,1;x,y,-z,1;-x,y,z,-1;y,x,z+1/2,-1                                                            ")
     Shubnikov_info(1104)= Shub_Spgr_Info_Type("131.442","P4_2'/m'mc' ","131.8.1104 ","P4_2'/m'mc'   ","P4_2'/m'mc'                ","P 4c' 2' -1'        ",  &
                                               "-y,x,z+1/2,-1;x,y,-z,-1;-x,y,z,1;y,x,z+1/2,-1                                                           ")
     Shubnikov_info(1105)= Shub_Spgr_Info_Type("131.443","P4_2/m'm'c' ","131.9.1105 ","P4_2/m'm'c'   ","P4_2/m'm'c'                ","P 4c 2 -1'          ",  &
                                               "-y,x,z+1/2,1;x,y,-z,-1;-x,y,z,-1;y,x,z+1/2,-1                                                           ")
     Shubnikov_info(1106)= Shub_Spgr_Info_Type("132.457","P_C4_2/mcm  ","131.10.1106","P_P4_2/mmc    ","P4_2/mcm.1'_C[rP4_2/mmc]   ","-P 4c 2c 1'C        ",  &
                                               "-y,x,z+1/2,1;x,y,-z,1;-x,y,z+1/2,1;y,x,z,1;x+1/2,y+1/2,z,-1                                             ")
     Shubnikov_info(1107)= Shub_Spgr_Info_Type("138.529","P_C4_2/ncm  ","131.11.1107","P_P4_2/m'mc   ","P4_2/ncm.1'_C[rP4_2/mmc]   ","-P 4ac 2ac 1'C      ",  &
                                               "-y+1/2,x,z+1/2,1;x+1/2,y+1/2,-z,1;-x+1/2,y,z+1/2,1;y,x,z,1;x+1/2,y+1/2,z,-1                             ")
     Shubnikov_info(1108)= Shub_Spgr_Info_Type("136.505","P_C4_2/mnm  ","131.12.1108","P_P4_2/mm'c'  ","P4_2/mnm.1'_C[rP4_2/mmc]   ","-P 4n 2n 1'C        ",  &
                                               "-y+1/2,x+1/2,z+1/2,1;x,y,-z,1;-x+1/2,y+1/2,z+1/2,1;y,x,z,1;x+1/2,y+1/2,z,-1                             ")
     Shubnikov_info(1109)= Shub_Spgr_Info_Type("134.481","P_C4_2/nnm  ","131.13.1109","P_P4_2'/m'mc' ","P4_2/nnm.1'_C[rP4_2/mmc]   ","-P 4ac 2bc 1'C      ",  &
                                               "-y+1/2,x,z+1/2,1;x+1/2,y+1/2,-z,1;-x,y+1/2,z+1/2,1;y+1/2,x+1/2,z,1;x+1/2,y+1/2,z,-1                     ")
     Shubnikov_info(1110)= Shub_Spgr_Info_Type("132.447","P4_2/mcm    ","132.1.1110 ","P4_2/mcm      ","P4_2/mcm.1                 ","-P 4c 2c            ",  &
                                               "-y,x,z+1/2,1;x,y,-z,1;-x,y,z+1/2,1;y,x,z,1                                                              ")
     Shubnikov_info(1111)= Shub_Spgr_Info_Type("132.448","P4_2/mcm1'  ","132.2.1111 ","P4_2/mcm1'    ","P4_2/mcm.1'                ","-P 4c 2c 1'         ",  &
                                               "-y,x,z+1/2,1;x,y,-z,1;-x,y,z+1/2,1;y,x,z,1;x,y,z,-1                                                     ")
     Shubnikov_info(1112)= Shub_Spgr_Info_Type("132.449","P4_2/m'cm   ","132.3.1112 ","P4_2/m'cm     ","P4_2/m'cm                  ","P 4c 2c' -1'        ",  &
                                               "-y,x,z+1/2,1;x,y,-z,-1;-x,y,z+1/2,1;y,x,z,1                                                             ")
     Shubnikov_info(1113)= Shub_Spgr_Info_Type("132.450","P4_2'/mc'm  ","132.4.1113 ","P4_2'/mc'm    ","P4_2'/mc'm                 ","-P 4c' 2c'          ",  &
                                               "-y,x,z+1/2,-1;x,y,-z,1;-x,y,z+1/2,-1;y,x,z,1                                                            ")
     Shubnikov_info(1114)= Shub_Spgr_Info_Type("132.451","P4_2'/mcm'  ","132.5.1114 ","P4_2'/mcm'    ","P4_2'/mcm'                 ","-P 4c' 2c           ",  &
                                               "-y,x,z+1/2,-1;x,y,-z,1;-x,y,z+1/2,1;y,x,z,-1                                                            ")
     Shubnikov_info(1115)= Shub_Spgr_Info_Type("132.452","P4_2'/m'c'm ","132.6.1115 ","P4_2'/m'c'm   ","P4_2'/m'c'm                ","P 4c' 2c -1'        ",  &
                                               "-y,x,z+1/2,-1;x,y,-z,-1;-x,y,z+1/2,-1;y,x,z,1                                                           ")
     Shubnikov_info(1116)= Shub_Spgr_Info_Type("132.453","P4_2/mc'm'  ","132.7.1116 ","P4_2/mc'm'    ","P4_2/mc'm'                 ","-P 4c 2c'           ",  &
                                               "-y,x,z+1/2,1;x,y,-z,1;-x,y,z+1/2,-1;y,x,z,-1                                                            ")
     Shubnikov_info(1117)= Shub_Spgr_Info_Type("132.454","P4_2'/m'cm' ","132.8.1117 ","P4_2'/m'cm'   ","P4_2'/m'cm'                ","P 4c' 2c' -1'       ",  &
                                               "-y,x,z+1/2,-1;x,y,-z,-1;-x,y,z+1/2,1;y,x,z,-1                                                           ")
     Shubnikov_info(1118)= Shub_Spgr_Info_Type("132.455","P4_2/m'c'm' ","132.9.1118 ","P4_2/m'c'm'   ","P4_2/m'c'm'                ","P 4c 2c -1'         ",  &
                                               "-y,x,z+1/2,1;x,y,-z,-1;-x,y,z+1/2,-1;y,x,z,-1                                                           ")
     Shubnikov_info(1119)= Shub_Spgr_Info_Type("131.445","P_C4_2/mmc  ","132.10.1119","P_P4_2/mcm    ","P4_2/mmc.1'_C[rP4_2/mcm]   ","-P 4c 2 1'C         ",  &
                                               "-y,x,z+1/2,1;x,y,-z,1;-x,y,z,1;y,x,z+1/2,1;x+1/2,y+1/2,z,-1                                             ")
     Shubnikov_info(1120)= Shub_Spgr_Info_Type("137.517","P_C4_2/nmc  ","132.11.1120","P_P4_2/m'cm   ","P4_2/nmc.1'_C[rP4_2/mcm]   ","-P 4ac 2a 1'C       ",  &
                                               "-y+1/2,x,z+1/2,1;x+1/2,y+1/2,-z,1;-x+1/2,y,z,1;y,x,z+1/2,1;x+1/2,y+1/2,z,-1                             ")
     Shubnikov_info(1121)= Shub_Spgr_Info_Type("135.493","P_C4_2/mbc  ","132.12.1121","P_P4_2'/mcm'  ","P4_2/mbc.1'_C[rP4_2/mcm]   ","-P 4c 2ab 1'C       ",  &
                                               "-y,x,z+1/2,1;x,y,-z,1;-x+1/2,y+1/2,z,1;y+1/2,x+1/2,z+1/2,1;x+1/2,y+1/2,z,-1                             ")
     Shubnikov_info(1122)= Shub_Spgr_Info_Type("133.469","P_C4_2/nbc  ","132.13.1122","P_P4_2'/m'cm' ","P4_2/nbc.1'_C[rP4_2/mcm]   ","-P 4ac 2b 1'C       ",  &
                                               "-y+1/2,x,z+1/2,1;x+1/2,y+1/2,-z,1;-x,y+1/2,z,1;y+1/2,x+1/2,z+1/2,1;x+1/2,y+1/2,z,-1                     ")
     Shubnikov_info(1123)= Shub_Spgr_Info_Type("133.459","P4_2/nbc    ","133.1.1123 ","P4_2/nbc      ","P4_2/nbc.1                 ","-P 4ac 2b           ",  &
                                               "-y+1/2,x,z+1/2,1;x+1/2,y+1/2,-z,1;-x,y+1/2,z,1;y+1/2,x+1/2,z+1/2,1                                      ")
     Shubnikov_info(1124)= Shub_Spgr_Info_Type("133.460","P4_2/nbc1'  ","133.2.1124 ","P4_2/nbc1'    ","P4_2/nbc.1'                ","-P 4ac 2b 1'        ",  &
                                               "-y+1/2,x,z+1/2,1;x+1/2,y+1/2,-z,1;-x,y+1/2,z,1;y+1/2,x+1/2,z+1/2,1;x,y,z,-1                             ")
     Shubnikov_info(1125)= Shub_Spgr_Info_Type("133.461","P4_2/n'bc   ","133.3.1125 ","P4_2/n'bc     ","P4_2/n'bc                  ","P 4ac 2b' -1'       ",  &
                                               "-y+1/2,x,z+1/2,1;x+1/2,y+1/2,-z,-1;-x,y+1/2,z,1;y+1/2,x+1/2,z+1/2,1                                     ")
     Shubnikov_info(1126)= Shub_Spgr_Info_Type("133.462","P4_2'/nb'c  ","133.4.1126 ","P4_2'/nb'c    ","P4_2'/nb'c                 ","-P 4ac' 2b'         ",  &
                                               "-y+1/2,x,z+1/2,-1;x+1/2,y+1/2,-z,1;-x,y+1/2,z,-1;y+1/2,x+1/2,z+1/2,1                                    ")
     Shubnikov_info(1127)= Shub_Spgr_Info_Type("133.463","P4_2'/nbc'  ","133.5.1127 ","P4_2'/nbc'    ","P4_2'/nbc'                 ","-P 4ac' 2b          ",  &
                                               "-y+1/2,x,z+1/2,-1;x+1/2,y+1/2,-z,1;-x,y+1/2,z,1;y+1/2,x+1/2,z+1/2,-1                                    ")
     Shubnikov_info(1128)= Shub_Spgr_Info_Type("133.464","P4_2'/n'b'c ","133.6.1128 ","P4_2'/n'b'c   ","P4_2'/n'b'c                ","P 4ac' 2b -1'       ",  &
                                               "-y+1/2,x,z+1/2,-1;x+1/2,y+1/2,-z,-1;-x,y+1/2,z,-1;y+1/2,x+1/2,z+1/2,1                                   ")
     Shubnikov_info(1129)= Shub_Spgr_Info_Type("133.465","P4_2/nb'c'  ","133.7.1129 ","P4_2/nb'c'    ","P4_2/nb'c'                 ","-P 4ac 2b'          ",  &
                                               "-y+1/2,x,z+1/2,1;x+1/2,y+1/2,-z,1;-x,y+1/2,z,-1;y+1/2,x+1/2,z+1/2,-1                                    ")
     Shubnikov_info(1130)= Shub_Spgr_Info_Type("133.466","P4_2'/n'bc' ","133.8.1130 ","P4_2'/n'bc'   ","P4_2'/n'bc'                ","P 4ac' 2b' -1'      ",  &
                                               "-y+1/2,x,z+1/2,-1;x+1/2,y+1/2,-z,-1;-x,y+1/2,z,1;y+1/2,x+1/2,z+1/2,-1                                   ")
     Shubnikov_info(1131)= Shub_Spgr_Info_Type("133.467","P4_2/n'b'c' ","133.9.1131 ","P4_2/n'b'c'   ","P4_2/n'b'c'                ","P 4ac 2b -1'        ",  &
                                               "-y+1/2,x,z+1/2,1;x+1/2,y+1/2,-z,-1;-x,y+1/2,z,-1;y+1/2,x+1/2,z+1/2,-1                                   ")
     Shubnikov_info(1132)= Shub_Spgr_Info_Type("134.471","P4_2/nnm    ","134.1.1132 ","P4_2/nnm      ","P4_2/nnm.1                 ","-P 4ac 2bc          ",  &
                                               "-y+1/2,x,z+1/2,1;x+1/2,y+1/2,-z,1;-x,y+1/2,z+1/2,1;y+1/2,x+1/2,z,1                                      ")
     Shubnikov_info(1133)= Shub_Spgr_Info_Type("134.472","P4_2/nnm1'  ","134.2.1133 ","P4_2/nnm1'    ","P4_2/nnm.1'                ","-P 4ac 2bc 1'       ",  &
                                               "-y+1/2,x,z+1/2,1;x+1/2,y+1/2,-z,1;-x,y+1/2,z+1/2,1;y+1/2,x+1/2,z,1;x,y,z,-1                             ")
     Shubnikov_info(1134)= Shub_Spgr_Info_Type("134.473","P4_2/n'nm   ","134.3.1134 ","P4_2/n'nm     ","P4_2/n'nm                  ","P 4ac 2bc' -1'      ",  &
                                               "-y+1/2,x,z+1/2,1;x+1/2,y+1/2,-z,-1;-x,y+1/2,z+1/2,1;y+1/2,x+1/2,z,1                                     ")
     Shubnikov_info(1135)= Shub_Spgr_Info_Type("134.474","P4_2'/nn'm  ","134.4.1135 ","P4_2'/nn'm    ","P4_2'/nn'm                 ","-P 4ac' 2bc'        ",  &
                                               "-y+1/2,x,z+1/2,-1;x+1/2,y+1/2,-z,1;-x,y+1/2,z+1/2,-1;y+1/2,x+1/2,z,1                                    ")
     Shubnikov_info(1136)= Shub_Spgr_Info_Type("134.475","P4_2'/nnm'  ","134.5.1136 ","P4_2'/nnm'    ","P4_2'/nnm'                 ","-P 4ac' 2bc         ",  &
                                               "-y+1/2,x,z+1/2,-1;x+1/2,y+1/2,-z,1;-x,y+1/2,z+1/2,1;y+1/2,x+1/2,z,-1                                    ")
     Shubnikov_info(1137)= Shub_Spgr_Info_Type("134.476","P4_2'/n'n'm ","134.6.1137 ","P4_2'/n'n'm   ","P4_2'/n'n'm                ","P 4ac' 2bc -1'      ",  &
                                               "-y+1/2,x,z+1/2,-1;x+1/2,y+1/2,-z,-1;-x,y+1/2,z+1/2,-1;y+1/2,x+1/2,z,1                                   ")
     Shubnikov_info(1138)= Shub_Spgr_Info_Type("134.477","P4_2/nn'm'  ","134.7.1138 ","P4_2/nn'm'    ","P4_2/nn'm'                 ","-P 4ac 2bc'         ",  &
                                               "-y+1/2,x,z+1/2,1;x+1/2,y+1/2,-z,1;-x,y+1/2,z+1/2,-1;y+1/2,x+1/2,z,-1                                    ")
     Shubnikov_info(1139)= Shub_Spgr_Info_Type("134.478","P4_2'/n'nm' ","134.8.1139 ","P4_2'/n'nm'   ","P4_2'/n'nm'                ","P 4ac' 2bc' -1'     ",  &
                                               "-y+1/2,x,z+1/2,-1;x+1/2,y+1/2,-z,-1;-x,y+1/2,z+1/2,1;y+1/2,x+1/2,z,-1                                   ")
     Shubnikov_info(1140)= Shub_Spgr_Info_Type("134.479","P4_2/n'n'm' ","134.9.1140 ","P4_2/n'n'm'   ","P4_2/n'n'm'                ","P 4ac 2bc -1'       ",  &
                                               "-y+1/2,x,z+1/2,1;x+1/2,y+1/2,-z,-1;-x,y+1/2,z+1/2,-1;y+1/2,x+1/2,z,-1                                   ")
     Shubnikov_info(1141)= Shub_Spgr_Info_Type("141.560","I_c4_1/amd  ","134.10.1141","P_I4_2/nnm    ","I4_1/amd.1'_c[rP4_2/nnm]   ","-I 4bd 2 1'c        ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y+1/4,x+3/4,z+1/4,1;x,y+1/2,-z,1;-x,y,z,1;y+3/4,x+3/4,z+1/4,1;x,y,z+1/2,-1         ")
     Shubnikov_info(1142)= Shub_Spgr_Info_Type("142.570","I_c4_1/acd  ","134.11.1142","P_I4_2/nn'm'  ","I4_1/acd.1'_c[rP4_2/nnm]   ","-I 4bd 2c 1'c       ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y+1/4,x+3/4,z+1/4,1;x,y+1/2,-z,1;-x,y,z+1/2,1;y+1/4,x+1/4,z+1/4,1;x,y,z+1/2,-1     ")
     Shubnikov_info(1143)= Shub_Spgr_Info_Type("135.483","P4_2/mbc    ","135.1.1143 ","P4_2/mbc      ","P4_2/mbc.1                 ","-P 4c 2ab           ",  &
                                               "-y,x,z+1/2,1;x,y,-z,1;-x+1/2,y+1/2,z,1;y+1/2,x+1/2,z+1/2,1                                              ")
     Shubnikov_info(1144)= Shub_Spgr_Info_Type("135.484","P4_2/mbc1'  ","135.2.1144 ","P4_2/mbc1'    ","P4_2/mbc.1'                ","-P 4c 2ab 1'        ",  &
                                               "-y,x,z+1/2,1;x,y,-z,1;-x+1/2,y+1/2,z,1;y+1/2,x+1/2,z+1/2,1;x,y,z,-1                                     ")
     Shubnikov_info(1145)= Shub_Spgr_Info_Type("135.485","P4_2/m'bc   ","135.3.1145 ","P4_2/m'bc     ","P4_2/m'bc                  ","P 4c 2ab' -1'       ",  &
                                               "-y,x,z+1/2,1;x,y,-z,-1;-x+1/2,y+1/2,z,1;y+1/2,x+1/2,z+1/2,1                                             ")
     Shubnikov_info(1146)= Shub_Spgr_Info_Type("135.486","P4_2'/mb'c  ","135.4.1146 ","P4_2'/mb'c    ","P4_2'/mb'c                 ","-P 4c' 2ab'         ",  &
                                               "-y,x,z+1/2,-1;x,y,-z,1;-x+1/2,y+1/2,z,-1;y+1/2,x+1/2,z+1/2,1                                            ")
     Shubnikov_info(1147)= Shub_Spgr_Info_Type("135.487","P4_2'/mbc'  ","135.5.1147 ","P4_2'/mbc'    ","P4_2'/mbc'                 ","-P 4c' 2ab          ",  &
                                               "-y,x,z+1/2,-1;x,y,-z,1;-x+1/2,y+1/2,z,1;y+1/2,x+1/2,z+1/2,-1                                            ")
     Shubnikov_info(1148)= Shub_Spgr_Info_Type("135.488","P4_2'/m'b'c ","135.6.1148 ","P4_2'/m'b'c   ","P4_2'/m'b'c                ","P 4c' 2ab -1'       ",  &
                                               "-y,x,z+1/2,-1;x,y,-z,-1;-x+1/2,y+1/2,z,-1;y+1/2,x+1/2,z+1/2,1                                           ")
     Shubnikov_info(1149)= Shub_Spgr_Info_Type("135.489","P4_2/mb'c'  ","135.7.1149 ","P4_2/mb'c'    ","P4_2/mb'c'                 ","-P 4c 2ab'          ",  &
                                               "-y,x,z+1/2,1;x,y,-z,1;-x+1/2,y+1/2,z,-1;y+1/2,x+1/2,z+1/2,-1                                            ")
     Shubnikov_info(1150)= Shub_Spgr_Info_Type("135.490","P4_2'/m'bc' ","135.8.1150 ","P4_2'/m'bc'   ","P4_2'/m'bc'                ","P 4c' 2ab' -1'      ",  &
                                               "-y,x,z+1/2,-1;x,y,-z,-1;-x+1/2,y+1/2,z,1;y+1/2,x+1/2,z+1/2,-1                                           ")
     Shubnikov_info(1151)= Shub_Spgr_Info_Type("135.491","P4_2/m'b'c' ","135.9.1151 ","P4_2/m'b'c'   ","P4_2/m'b'c'                ","P 4c 2ab -1'        ",  &
                                               "-y,x,z+1/2,1;x,y,-z,-1;-x+1/2,y+1/2,z,-1;y+1/2,x+1/2,z+1/2,-1                                           ")
     Shubnikov_info(1152)= Shub_Spgr_Info_Type("136.495","P4_2/mnm    ","136.1.1152 ","P4_2/mnm      ","P4_2/mnm.1                 ","-P 4n 2n            ",  &
                                               "-y+1/2,x+1/2,z+1/2,1;x,y,-z,1;-x+1/2,y+1/2,z+1/2,1;y,x,z,1                                              ")
     Shubnikov_info(1153)= Shub_Spgr_Info_Type("136.496","P4_2/mnm1'  ","136.2.1153 ","P4_2/mnm1'    ","P4_2/mnm.1'                ","-P 4n 2n 1'         ",  &
                                               "-y+1/2,x+1/2,z+1/2,1;x,y,-z,1;-x+1/2,y+1/2,z+1/2,1;y,x,z,1;x,y,z,-1                                     ")
     Shubnikov_info(1154)= Shub_Spgr_Info_Type("136.497","P4_2/m'nm   ","136.3.1154 ","P4_2/m'nm     ","P4_2/m'nm                  ","P 4n 2n' -1'        ",  &
                                               "-y+1/2,x+1/2,z+1/2,1;x,y,-z,-1;-x+1/2,y+1/2,z+1/2,1;y,x,z,1                                             ")
     Shubnikov_info(1155)= Shub_Spgr_Info_Type("136.498","P4_2'/mn'm  ","136.4.1155 ","P4_2'/mn'm    ","P4_2'/mn'm                 ","-P 4n' 2n'          ",  &
                                               "-y+1/2,x+1/2,z+1/2,-1;x,y,-z,1;-x+1/2,y+1/2,z+1/2,-1;y,x,z,1                                            ")
     Shubnikov_info(1156)= Shub_Spgr_Info_Type("136.499","P4_2'/mnm'  ","136.5.1156 ","P4_2'/mnm'    ","P4_2'/mnm'                 ","-P 4n' 2n           ",  &
                                               "-y+1/2,x+1/2,z+1/2,-1;x,y,-z,1;-x+1/2,y+1/2,z+1/2,1;y,x,z,-1                                            ")
     Shubnikov_info(1157)= Shub_Spgr_Info_Type("136.500","P4_2'/m'n'm ","136.6.1157 ","P4_2'/m'n'm   ","P4_2'/m'n'm                ","P 4n' 2n -1'        ",  &
                                               "-y+1/2,x+1/2,z+1/2,-1;x,y,-z,-1;-x+1/2,y+1/2,z+1/2,-1;y,x,z,1                                           ")
     Shubnikov_info(1158)= Shub_Spgr_Info_Type("136.501","P4_2/mn'm'  ","136.7.1158 ","P4_2/mn'm'    ","P4_2/mn'm'                 ","-P 4n 2n'           ",  &
                                               "-y+1/2,x+1/2,z+1/2,1;x,y,-z,1;-x+1/2,y+1/2,z+1/2,-1;y,x,z,-1                                            ")
     Shubnikov_info(1159)= Shub_Spgr_Info_Type("136.502","P4_2'/m'nm' ","136.8.1159 ","P4_2'/m'nm'   ","P4_2'/m'nm'                ","P 4n' 2n' -1'       ",  &
                                               "-y+1/2,x+1/2,z+1/2,-1;x,y,-z,-1;-x+1/2,y+1/2,z+1/2,1;y,x,z,-1                                           ")
     Shubnikov_info(1160)= Shub_Spgr_Info_Type("136.503","P4_2/m'n'm' ","136.9.1160 ","P4_2/m'n'm'   ","P4_2/m'n'm'                ","P 4n 2n -1'         ",  &
                                               "-y+1/2,x+1/2,z+1/2,1;x,y,-z,-1;-x+1/2,y+1/2,z+1/2,-1;y,x,z,-1                                           ")
     Shubnikov_info(1161)= Shub_Spgr_Info_Type("137.507","P4_2/nmc    ","137.1.1161 ","P4_2/nmc      ","P4_2/nmc.1                 ","-P 4ac 2a           ",  &
                                               "-y+1/2,x,z+1/2,1;x+1/2,y+1/2,-z,1;-x+1/2,y,z,1;y,x,z+1/2,1                                              ")
     Shubnikov_info(1162)= Shub_Spgr_Info_Type("137.508","P4_2/nmc1'  ","137.2.1162 ","P4_2/nmc1'    ","P4_2/nmc.1'                ","-P 4ac 2a 1'        ",  &
                                               "-y+1/2,x,z+1/2,1;x+1/2,y+1/2,-z,1;-x+1/2,y,z,1;y,x,z+1/2,1;x,y,z,-1                                     ")
     Shubnikov_info(1163)= Shub_Spgr_Info_Type("137.509","P4_2/n'mc   ","137.3.1163 ","P4_2/n'mc     ","P4_2/n'mc                  ","P 4ac 2a' -1'       ",  &
                                               "-y+1/2,x,z+1/2,1;x+1/2,y+1/2,-z,-1;-x+1/2,y,z,1;y,x,z+1/2,1                                             ")
     Shubnikov_info(1164)= Shub_Spgr_Info_Type("137.510","P4_2'/nm'c  ","137.4.1164 ","P4_2'/nm'c    ","P4_2'/nm'c                 ","-P 4ac' 2a'         ",  &
                                               "-y+1/2,x,z+1/2,-1;x+1/2,y+1/2,-z,1;-x+1/2,y,z,-1;y,x,z+1/2,1                                            ")
     Shubnikov_info(1165)= Shub_Spgr_Info_Type("137.511","P4_2'/nmc'  ","137.5.1165 ","P4_2'/nmc'    ","P4_2'/nmc'                 ","-P 4ac' 2a          ",  &
                                               "-y+1/2,x,z+1/2,-1;x+1/2,y+1/2,-z,1;-x+1/2,y,z,1;y,x,z+1/2,-1                                            ")
     Shubnikov_info(1166)= Shub_Spgr_Info_Type("137.512","P4_2'/n'm'c ","137.6.1166 ","P4_2'/n'm'c   ","P4_2'/n'm'c                ","P 4ac' 2a -1'       ",  &
                                               "-y+1/2,x,z+1/2,-1;x+1/2,y+1/2,-z,-1;-x+1/2,y,z,-1;y,x,z+1/2,1                                           ")
     Shubnikov_info(1167)= Shub_Spgr_Info_Type("137.513","P4_2/nm'c'  ","137.7.1167 ","P4_2/nm'c'    ","P4_2/nm'c'                 ","-P 4ac 2a'          ",  &
                                               "-y+1/2,x,z+1/2,1;x+1/2,y+1/2,-z,1;-x+1/2,y,z,-1;y,x,z+1/2,-1                                            ")
     Shubnikov_info(1168)= Shub_Spgr_Info_Type("137.514","P4_2'/n'mc' ","137.8.1168 ","P4_2'/n'mc'   ","P4_2'/n'mc'                ","P 4ac' 2a' -1'      ",  &
                                               "-y+1/2,x,z+1/2,-1;x+1/2,y+1/2,-z,-1;-x+1/2,y,z,1;y,x,z+1/2,-1                                           ")
     Shubnikov_info(1169)= Shub_Spgr_Info_Type("137.515","P4_2/n'm'c' ","137.9.1169 ","P4_2/n'm'c'   ","P4_2/n'm'c'                ","P 4ac 2a -1'        ",  &
                                               "-y+1/2,x,z+1/2,1;x+1/2,y+1/2,-z,-1;-x+1/2,y,z,-1;y,x,z+1/2,-1                                           ")
     Shubnikov_info(1170)= Shub_Spgr_Info_Type("138.519","P4_2/ncm    ","138.1.1170 ","P4_2/ncm      ","P4_2/ncm.1                 ","-P 4ac 2ac          ",  &
                                               "-y+1/2,x,z+1/2,1;x+1/2,y+1/2,-z,1;-x+1/2,y,z+1/2,1;y,x,z,1                                              ")
     Shubnikov_info(1171)= Shub_Spgr_Info_Type("138.520","P4_2/ncm1'  ","138.2.1171 ","P4_2/ncm1'    ","P4_2/ncm.1'                ","-P 4ac 2ac 1'       ",  &
                                               "-y+1/2,x,z+1/2,1;x+1/2,y+1/2,-z,1;-x+1/2,y,z+1/2,1;y,x,z,1;x,y,z,-1                                     ")
     Shubnikov_info(1172)= Shub_Spgr_Info_Type("138.521","P4_2/n'cm   ","138.3.1172 ","P4_2/n'cm     ","P4_2/n'cm                  ","P 4ac 2ac' -1'      ",  &
                                               "-y+1/2,x,z+1/2,1;x+1/2,y+1/2,-z,-1;-x+1/2,y,z+1/2,1;y,x,z,1                                             ")
     Shubnikov_info(1173)= Shub_Spgr_Info_Type("138.522","P4_2'/nc'm  ","138.4.1173 ","P4_2'/nc'm    ","P4_2'/nc'm                 ","-P 4ac' 2ac'        ",  &
                                               "-y+1/2,x,z+1/2,-1;x+1/2,y+1/2,-z,1;-x+1/2,y,z+1/2,-1;y,x,z,1                                            ")
     Shubnikov_info(1174)= Shub_Spgr_Info_Type("138.523","P4_2'/ncm'  ","138.5.1174 ","P4_2'/ncm'    ","P4_2'/ncm'                 ","-P 4ac' 2ac         ",  &
                                               "-y+1/2,x,z+1/2,-1;x+1/2,y+1/2,-z,1;-x+1/2,y,z+1/2,1;y,x,z,-1                                            ")
     Shubnikov_info(1175)= Shub_Spgr_Info_Type("138.524","P4_2'/n'c'm ","138.6.1175 ","P4_2'/n'c'm   ","P4_2'/n'c'm                ","P 4ac' 2ac -1'      ",  &
                                               "-y+1/2,x,z+1/2,-1;x+1/2,y+1/2,-z,-1;-x+1/2,y,z+1/2,-1;y,x,z,1                                           ")
     Shubnikov_info(1176)= Shub_Spgr_Info_Type("138.525","P4_2/nc'm'  ","138.7.1176 ","P4_2/nc'm'    ","P4_2/nc'm'                 ","-P 4ac 2ac'         ",  &
                                               "-y+1/2,x,z+1/2,1;x+1/2,y+1/2,-z,1;-x+1/2,y,z+1/2,-1;y,x,z,-1                                            ")
     Shubnikov_info(1177)= Shub_Spgr_Info_Type("138.526","P4_2'/n'cm' ","138.8.1177 ","P4_2'/n'cm'   ","P4_2'/n'cm'                ","P 4ac' 2ac' -1'     ",  &
                                               "-y+1/2,x,z+1/2,-1;x+1/2,y+1/2,-z,-1;-x+1/2,y,z+1/2,1;y,x,z,-1                                           ")
     Shubnikov_info(1178)= Shub_Spgr_Info_Type("138.527","P4_2/n'c'm' ","138.9.1178 ","P4_2/n'c'm'   ","P4_2/n'c'm'                ","P 4ac 2ac -1'       ",  &
                                               "-y+1/2,x,z+1/2,1;x+1/2,y+1/2,-z,-1;-x+1/2,y,z+1/2,-1;y,x,z,-1                                           ")
     Shubnikov_info(1179)= Shub_Spgr_Info_Type("139.531","I4/mmm      ","139.1.1179 ","I4/mmm        ","I4/mmm.1                   ","-I 4 2              ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,1;x,y,-z,1;-x,y,z,1;y,x,z,1                                                  ")
     Shubnikov_info(1180)= Shub_Spgr_Info_Type("139.532","I4/mmm1'    ","139.2.1180 ","I4/mmm1'      ","I4/mmm.1'                  ","-I 4 2 1'           ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,1;x,y,-z,1;-x,y,z,1;y,x,z,1;x,y,z,-1                                         ")
     Shubnikov_info(1181)= Shub_Spgr_Info_Type("139.533","I4/m'mm     ","139.3.1181 ","I4/m'mm       ","I4/m'mm                    ","I 4 2' -1'          ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,1;x,y,-z,-1;-x,y,z,1;y,x,z,1                                                 ")
     Shubnikov_info(1182)= Shub_Spgr_Info_Type("139.534","I4'/mm'm    ","139.4.1182 ","I4'/mm'm      ","I4'/mm'm                   ","-I 4' 2'            ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,-1;x,y,-z,1;-x,y,z,-1;y,x,z,1                                                ")
     Shubnikov_info(1183)= Shub_Spgr_Info_Type("139.535","I4'/mmm'    ","139.5.1183 ","I4'/mmm'      ","I4'/mmm'                   ","-I 4' 2             ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,-1;x,y,-z,1;-x,y,z,1;y,x,z,-1                                                ")
     Shubnikov_info(1184)= Shub_Spgr_Info_Type("139.536","I4'/m'm'm   ","139.6.1184 ","I4'/m'm'm     ","I4'/m'm'm                  ","I 4' 2 -1'          ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,-1;x,y,-z,-1;-x,y,z,-1;y,x,z,1                                               ")
     Shubnikov_info(1185)= Shub_Spgr_Info_Type("139.537","I4/mm'm'    ","139.7.1185 ","I4/mm'm'      ","I4/mm'm'                   ","-I 4 2'             ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,1;x,y,-z,1;-x,y,z,-1;y,x,z,-1                                                ")
     Shubnikov_info(1186)= Shub_Spgr_Info_Type("139.538","I4'/m'mm'   ","139.8.1186 ","I4'/m'mm'     ","I4'/m'mm'                  ","I 4' 2' -1'         ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,-1;x,y,-z,-1;-x,y,z,1;y,x,z,-1                                               ")
     Shubnikov_info(1187)= Shub_Spgr_Info_Type("139.539","I4/m'm'm'   ","139.9.1187 ","I4/m'm'm'     ","I4/m'm'm'                  ","I 4 2 -1'           ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,1;x,y,-z,-1;-x,y,z,-1;y,x,z,-1                                               ")
     Shubnikov_info(1188)= Shub_Spgr_Info_Type("123.350","P_I4/mmm    ","139.10.1188","I_P4/mmm      ","P4/mmm.1'_I[I4/mmm]        ","-P 4 2 1'I          ",  &
                                               "-y,x,z,1;x,y,-z,1;-x,y,z,1;y,x,z,1;x+1/2,y+1/2,z+1/2,-1                                                 ")
     Shubnikov_info(1189)= Shub_Spgr_Info_Type("129.422","P_I4/nmm    ","139.11.1189","I_P4/m'mm     ","P4/nmm.1'_I[I4/mmm]        ","-P 4a 2a 1'I        ",  &
                                               "-y+1/2,x,z,1;x+1/2,y+1/2,-z,1;-x+1/2,y,z,1;y,x,z,1;x+1/2,y+1/2,z+1/2,-1                                 ")
     Shubnikov_info(1190)= Shub_Spgr_Info_Type("136.506","P_I4_2/mnm  ","139.12.1190","I_P4'/mm'm    ","P4_2/mnm.1'_I[I4/mmm]      ","-P 4n 2n 1'I        ",  &
                                               "-y+1/2,x+1/2,z+1/2,1;x,y,-z,1;-x+1/2,y+1/2,z+1/2,1;y,x,z,1;x+1/2,y+1/2,z+1/2,-1                         ")
     Shubnikov_info(1191)= Shub_Spgr_Info_Type("131.446","P_I4_2/mmc  ","139.13.1191","I_P4'/mmm'    ","P4_2/mmc.1'_I[I4/mmm]      ","-P 4c 2 1'I         ",  &
                                               "-y,x,z+1/2,1;x,y,-z,1;-x,y,z,1;y,x,z+1/2,1;x+1/2,y+1/2,z+1/2,-1                                         ")
     Shubnikov_info(1192)= Shub_Spgr_Info_Type("134.482","P_I4_2/nnm  ","139.14.1192","I_P4'/m'm'm   ","P4_2/nnm.1'_I[I4/mmm]      ","-P 4ac 2bc 1'I      ",  &
                                               "-y+1/2,x,z+1/2,1;x+1/2,y+1/2,-z,1;-x,y+1/2,z+1/2,1;y+1/2,x+1/2,z,1;x+1/2,y+1/2,z+1/2,-1                 ")
     Shubnikov_info(1193)= Shub_Spgr_Info_Type("128.410","P_I4/mnc    ","139.15.1193","I_P4/mm'm'    ","P4/mnc.1'_I[I4/mmm]        ","-P 4 2n 1'I         ",  &
                                               "-y,x,z,1;x,y,-z,1;-x+1/2,y+1/2,z+1/2,1;y+1/2,x+1/2,z+1/2,1;x+1/2,y+1/2,z+1/2,-1                         ")
     Shubnikov_info(1194)= Shub_Spgr_Info_Type("137.518","P_I4_2/nmc  ","139.16.1194","I_P4'/m'mm'   ","P4_2/nmc.1'_I[I4/mmm]      ","-P 4ac 2a 1'I       ",  &
                                               "-y+1/2,x,z+1/2,1;x+1/2,y+1/2,-z,1;-x+1/2,y,z,1;y,x,z+1/2,1;x+1/2,y+1/2,z+1/2,-1                         ")
     Shubnikov_info(1195)= Shub_Spgr_Info_Type("126.386","P_I4/nnc    ","139.17.1195","I_P4/m'm'm'   ","P4/nnc.1'_I[I4/mmm]        ","-P 4a 2bc 1'I       ",  &
                                               "-y+1/2,x,z,1;x+1/2,y+1/2,-z,1;-x,y+1/2,z+1/2,1;y+1/2,x+1/2,z+1/2,1;x+1/2,y+1/2,z+1/2,-1                 ")
     Shubnikov_info(1196)= Shub_Spgr_Info_Type("140.541","I4/mcm      ","140.1.1196 ","I4/mcm        ","I4/mcm.1                   ","-I 4 2c             ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,1;x,y,-z,1;-x,y,z+1/2,1;y,x,z+1/2,1                                          ")
     Shubnikov_info(1197)= Shub_Spgr_Info_Type("140.542","I4/mcm1'    ","140.2.1197 ","I4/mcm1'      ","I4/mcm.1'                  ","-I 4 2c 1'          ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,1;x,y,-z,1;-x,y,z+1/2,1;y,x,z+1/2,1;x,y,z,-1                                 ")
     Shubnikov_info(1198)= Shub_Spgr_Info_Type("140.543","I4/m'cm     ","140.3.1198 ","I4/m'cm       ","I4/m'cm                    ","I 4 2c' -1'         ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,1;x,y,-z,-1;-x,y,z+1/2,1;y,x,z+1/2,1                                         ")
     Shubnikov_info(1199)= Shub_Spgr_Info_Type("140.544","I4'/mc'm    ","140.4.1199 ","I4'/mc'm      ","I4'/mc'm                   ","-I 4' 2c'           ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,-1;x,y,-z,1;-x,y,z+1/2,-1;y,x,z+1/2,1                                        ")
     Shubnikov_info(1200)= Shub_Spgr_Info_Type("140.545","I4'/mcm'    ","140.5.1200 ","I4'/mcm'      ","I4'/mcm'                   ","-I 4' 2c            ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,-1;x,y,-z,1;-x,y,z+1/2,1;y,x,z+1/2,-1                                        ")
     Shubnikov_info(1201)= Shub_Spgr_Info_Type("140.546","I4'/m'c'm   ","140.6.1201 ","I4'/m'c'm     ","I4'/m'c'm                  ","I 4' 2c -1'         ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,-1;x,y,-z,-1;-x,y,z+1/2,-1;y,x,z+1/2,1                                       ")
     Shubnikov_info(1202)= Shub_Spgr_Info_Type("140.547","I4/mc'm'    ","140.7.1202 ","I4/mc'm'      ","I4/mc'm'                   ","-I 4 2c'            ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,1;x,y,-z,1;-x,y,z+1/2,-1;y,x,z+1/2,-1                                        ")
     Shubnikov_info(1203)= Shub_Spgr_Info_Type("140.548","I4'/m'cm'   ","140.8.1203 ","I4'/m'cm'     ","I4'/m'cm'                  ","I 4' 2c' -1'        ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,-1;x,y,-z,-1;-x,y,z+1/2,1;y,x,z+1/2,-1                                       ")
     Shubnikov_info(1204)= Shub_Spgr_Info_Type("140.549","I4/m'c'm'   ","140.9.1204 ","I4/m'c'm'     ","I4/m'c'm'                  ","I 4 2c -1'          ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,1;x,y,-z,-1;-x,y,z+1/2,-1;y,x,z+1/2,-1                                       ")
     Shubnikov_info(1205)= Shub_Spgr_Info_Type("124.362","P_I4/mcc    ","140.10.1205","I_P4/mcm      ","P4/mcc.1'_I[I4/mcm]        ","-P 4 2c 1'I         ",  &
                                               "-y,x,z,1;x,y,-z,1;-x,y,z+1/2,1;y,x,z+1/2,1;x+1/2,y+1/2,z+1/2,-1                                         ")
     Shubnikov_info(1206)= Shub_Spgr_Info_Type("130.434","P_I4/ncc    ","140.11.1206","I_P4/m'cm     ","P4/ncc.1'_I[I4/mcm]        ","-P 4a 2ac 1'I       ",  &
                                               "-y+1/2,x,z,1;x+1/2,y+1/2,-z,1;-x+1/2,y,z+1/2,1;y,x,z+1/2,1;x+1/2,y+1/2,z+1/2,-1                         ")
     Shubnikov_info(1207)= Shub_Spgr_Info_Type("135.494","P_I4_2/mbc  ","140.12.1207","I_P4'/mc'm    ","P4_2/mbc.1'_I[I4/mcm]      ","-P 4c 2ab 1'I       ",  &
                                               "-y,x,z+1/2,1;x,y,-z,1;-x+1/2,y+1/2,z,1;y+1/2,x+1/2,z+1/2,1;x+1/2,y+1/2,z+1/2,-1                         ")
     Shubnikov_info(1208)= Shub_Spgr_Info_Type("132.458","P_I4_2/mcm  ","140.13.1208","I_P4'/mcm'    ","P4_2/mcm.1'_I[I4/mcm]      ","-P 4c 2c 1'I        ",  &
                                               "-y,x,z+1/2,1;x,y,-z,1;-x,y,z+1/2,1;y,x,z,1;x+1/2,y+1/2,z+1/2,-1                                         ")
     Shubnikov_info(1209)= Shub_Spgr_Info_Type("133.470","P_I4_2/nbc  ","140.14.1209","I_P4'/m'c'm   ","P4_2/nbc.1'_I[I4/mcm]      ","-P 4ac 2b 1'I       ",  &
                                               "-y+1/2,x,z+1/2,1;x+1/2,y+1/2,-z,1;-x,y+1/2,z,1;y+1/2,x+1/2,z+1/2,1;x+1/2,y+1/2,z+1/2,-1                 ")
     Shubnikov_info(1210)= Shub_Spgr_Info_Type("127.398","P_I4/mbm    ","140.15.1210","I_P4/mc'm'    ","P4/mbm.1'_I[I4/mcm]        ","-P 4 2ab 1'I        ",  &
                                               "-y,x,z,1;x,y,-z,1;-x+1/2,y+1/2,z,1;y+1/2,x+1/2,z,1;x+1/2,y+1/2,z+1/2,-1                                 ")
     Shubnikov_info(1211)= Shub_Spgr_Info_Type("138.530","P_I4_2/ncm  ","140.16.1211","I_P4'/m'cm'   ","P4_2/ncm.1'_I[I4/mcm]      ","-P 4ac 2ac 1'I      ",  &
                                               "-y+1/2,x,z+1/2,1;x+1/2,y+1/2,-z,1;-x+1/2,y,z+1/2,1;y,x,z,1;x+1/2,y+1/2,z+1/2,-1                         ")
     Shubnikov_info(1212)= Shub_Spgr_Info_Type("125.374","P_I4/nbm    ","140.17.1212","I_P4/m'c'm'   ","P4/nbm.1'_I[I4/mcm]        ","-P 4a 2b 1'I        ",  &
                                               "-y+1/2,x,z,1;x+1/2,y+1/2,-z,1;-x,y+1/2,z,1;y+1/2,x+1/2,z,1;x+1/2,y+1/2,z+1/2,-1                         ")
     Shubnikov_info(1213)= Shub_Spgr_Info_Type("141.551","I4_1/amd    ","141.1.1213 ","I4_1/amd      ","I4_1/amd.1                 ","-I 4bd 2            ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y+1/4,x+3/4,z+1/4,1;x,y+1/2,-z,1;-x,y,z,1;y+3/4,x+3/4,z+1/4,1                      ")
     Shubnikov_info(1214)= Shub_Spgr_Info_Type("141.552","I4_1/amd1'  ","141.2.1214 ","I4_1/amd1'    ","I4_1/amd.1'                ","-I 4bd 2 1'         ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y+1/4,x+3/4,z+1/4,1;x,y+1/2,-z,1;-x,y,z,1;y+3/4,x+3/4,z+1/4,1;x,y,z,-1             ")
     Shubnikov_info(1215)= Shub_Spgr_Info_Type("141.553","I4_1/a'md   ","141.3.1215 ","I4_1/a'md     ","I4_1/a'md                  ","I 4bd 2' -1'        ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y+1/4,x+3/4,z+1/4,1;x,y+1/2,-z,-1;-x,y,z,1;y+3/4,x+3/4,z+1/4,1                     ")
     Shubnikov_info(1216)= Shub_Spgr_Info_Type("141.554","I4_1'/am'd  ","141.4.1216 ","I4_1'/am'd    ","I4_1'/am'd                 ","-I 4bd' 2'          ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y+1/4,x+3/4,z+1/4,-1;x,y+1/2,-z,1;-x,y,z,-1;y+3/4,x+3/4,z+1/4,1                    ")
     Shubnikov_info(1217)= Shub_Spgr_Info_Type("141.555","I4_1'/amd'  ","141.5.1217 ","I4_1'/amd'    ","I4_1'/amd'                 ","-I 4bd' 2           ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y+1/4,x+3/4,z+1/4,-1;x,y+1/2,-z,1;-x,y,z,1;y+3/4,x+3/4,z+1/4,-1                    ")
     Shubnikov_info(1218)= Shub_Spgr_Info_Type("141.556","I4_1'/a'm'd ","141.6.1218 ","I4_1'/a'm'd   ","I4_1'/a'm'd                ","I 4bd' 2 -1'        ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y+1/4,x+3/4,z+1/4,-1;x,y+1/2,-z,-1;-x,y,z,-1;y+3/4,x+3/4,z+1/4,1                   ")
     Shubnikov_info(1219)= Shub_Spgr_Info_Type("141.557","I4_1/am'd'  ","141.7.1219 ","I4_1/am'd'    ","I4_1/am'd'                 ","-I 4bd 2'           ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y+1/4,x+3/4,z+1/4,1;x,y+1/2,-z,1;-x,y,z,-1;y+3/4,x+3/4,z+1/4,-1                    ")
     Shubnikov_info(1220)= Shub_Spgr_Info_Type("141.558","I4_1'/a'md' ","141.8.1220 ","I4_1'/a'md'   ","I4_1'/a'md'                ","I 4bd' 2' -1'       ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y+1/4,x+3/4,z+1/4,-1;x,y+1/2,-z,-1;-x,y,z,1;y+3/4,x+3/4,z+1/4,-1                   ")
     Shubnikov_info(1221)= Shub_Spgr_Info_Type("141.559","I4_1/a'm'd' ","141.9.1221 ","I4_1/a'm'd'   ","I4_1/a'm'd'                ","I 4bd 2 -1'         ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y+1/4,x+3/4,z+1/4,1;x,y+1/2,-z,-1;-x,y,z,-1;y+3/4,x+3/4,z+1/4,-1                   ")
     Shubnikov_info(1222)= Shub_Spgr_Info_Type("142.561","I4_1/acd    ","142.1.1222 ","I4_1/acd      ","I4_1/acd.1                 ","-I 4bd 2c           ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y+1/4,x+3/4,z+1/4,1;x,y+1/2,-z,1;-x,y,z+1/2,1;y+1/4,x+1/4,z+1/4,1                  ")
     Shubnikov_info(1223)= Shub_Spgr_Info_Type("142.562","I4_1/acd1'  ","142.2.1223 ","I4_1/acd1'    ","I4_1/acd.1'                ","-I 4bd 2c 1'        ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y+1/4,x+3/4,z+1/4,1;x,y+1/2,-z,1;-x,y,z+1/2,1;y+1/4,x+1/4,z+1/4,1;x,y,z,-1         ")
     Shubnikov_info(1224)= Shub_Spgr_Info_Type("142.563","I4_1/a'cd   ","142.3.1224 ","I4_1/a'cd     ","I4_1/a'cd                  ","I 4bd 2c' -1'       ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y+1/4,x+3/4,z+1/4,1;x,y+1/2,-z,-1;-x,y,z+1/2,1;y+1/4,x+1/4,z+1/4,1                 ")
     Shubnikov_info(1225)= Shub_Spgr_Info_Type("142.564","I4_1'/ac'd  ","142.4.1225 ","I4_1'/ac'd    ","I4_1'/ac'd                 ","-I 4bd' 2c'         ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y+1/4,x+3/4,z+1/4,-1;x,y+1/2,-z,1;-x,y,z+1/2,-1;y+1/4,x+1/4,z+1/4,1                ")
     Shubnikov_info(1226)= Shub_Spgr_Info_Type("142.565","I4_1'/acd'  ","142.5.1226 ","I4_1'/acd'    ","I4_1'/acd'                 ","-I 4bd' 2c          ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y+1/4,x+3/4,z+1/4,-1;x,y+1/2,-z,1;-x,y,z+1/2,1;y+1/4,x+1/4,z+1/4,-1                ")
     Shubnikov_info(1227)= Shub_Spgr_Info_Type("142.566","I4_1'/a'c'd ","142.6.1227 ","I4_1'/a'c'd   ","I4_1'/a'c'd                ","I 4bd' 2c -1'       ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y+1/4,x+3/4,z+1/4,-1;x,y+1/2,-z,-1;-x,y,z+1/2,-1;y+1/4,x+1/4,z+1/4,1               ")
     Shubnikov_info(1228)= Shub_Spgr_Info_Type("142.567","I4_1/ac'd'  ","142.7.1228 ","I4_1/ac'd'    ","I4_1/ac'd'                 ","-I 4bd 2c'          ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y+1/4,x+3/4,z+1/4,1;x,y+1/2,-z,1;-x,y,z+1/2,-1;y+1/4,x+1/4,z+1/4,-1                ")
     Shubnikov_info(1229)= Shub_Spgr_Info_Type("142.568","I4_1'/a'cd' ","142.8.1229 ","I4_1'/a'cd'   ","I4_1'/a'cd'                ","I 4bd' 2c' -1'      ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y+1/4,x+3/4,z+1/4,-1;x,y+1/2,-z,-1;-x,y,z+1/2,1;y+1/4,x+1/4,z+1/4,-1               ")
     Shubnikov_info(1230)= Shub_Spgr_Info_Type("142.569","I4_1/a'c'd' ","142.9.1230 ","I4_1/a'c'd'   ","I4_1/a'c'd'                ","I 4bd 2c -1'        ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y+1/4,x+3/4,z+1/4,1;x,y+1/2,-z,-1;-x,y,z+1/2,-1;y+1/4,x+1/4,z+1/4,-1               ")
     Shubnikov_info(1231)= Shub_Spgr_Info_Type("143.1  ","P3          ","143.1.1231 ","P3            ","P3.1                       ","P 3                 ",  &
                                               "-y,x-y,z,1                                                                                              ")
     Shubnikov_info(1232)= Shub_Spgr_Info_Type("143.2  ","P31'        ","143.2.1232 ","P31'          ","P3.1'                      ","P 3 1'              ",  &
                                               "-y,x-y,z,1;x,y,z,-1                                                                                     ")
     Shubnikov_info(1233)= Shub_Spgr_Info_Type("143.3  ","P_c3        ","143.3.1233 ","P_2c3         ","P3.1'_c[P3]                ","P 3 1'c             ",  &
                                               "-y,x-y,z,1;x,y,z+1/2,-1                                                                                 ")
     Shubnikov_info(1234)= Shub_Spgr_Info_Type("144.4  ","P3_1        ","144.1.1234 ","P3_1          ","P3_1.1                     ","P 31                ",  &
                                               "-y,x-y,z+1/3,1                                                                                          ")
     Shubnikov_info(1235)= Shub_Spgr_Info_Type("144.5  ","P3_11'      ","144.2.1235 ","P3_11'        ","P3_1.1'                    ","P 31 1'             ",  &
                                               "-y,x-y,z+1/3,1;x,y,z,-1                                                                                 ")
     Shubnikov_info(1236)= Shub_Spgr_Info_Type("145.9  ","P_c3_2      ","144.3.1236 ","P_2c3_2       ","P3_2.1'_c[P3_1]            ","P 32 1'c            ",  &
                                               "-y,x-y,z+2/3,1;x,y,z+1/2,-1                                                                             ")
     Shubnikov_info(1237)= Shub_Spgr_Info_Type("145.7  ","P3_2        ","145.1.1237 ","P3_2          ","P3_2.1                     ","P 32                ",  &
                                               "-y,x-y,z+2/3,1                                                                                          ")
     Shubnikov_info(1238)= Shub_Spgr_Info_Type("145.8  ","P3_21'      ","145.2.1238 ","P3_21'        ","P3_2.1'                    ","P 32 1'             ",  &
                                               "-y,x-y,z+2/3,1;x,y,z,-1                                                                                 ")
     Shubnikov_info(1239)= Shub_Spgr_Info_Type("144.6  ","P_c3_1      ","145.3.1239 ","P_2c3_1       ","P3_1.1'_c[P3_2]            ","P 31 1'c            ",  &
                                               "-y,x-y,z+1/3,1;x,y,z+1/2,-1                                                                             ")
     Shubnikov_info(1240)= Shub_Spgr_Info_Type("146.10 ","R3          ","146.1.1240 ","R3            ","R3.1                       ","R 3                 ",  &
                                               "x+2/3,y+1/3,z+1/3,1;-y,x-y,z,1                                                                          ")
     Shubnikov_info(1241)= Shub_Spgr_Info_Type("146.11 ","R31'        ","146.2.1241 ","R31'          ","R3.1'                      ","R 3 1'              ",  &
                                               "x+2/3,y+1/3,z+1/3,1;-y,x-y,z,1;x,y,z,-1                                                                 ")
     Shubnikov_info(1242)= Shub_Spgr_Info_Type("146.12 ","R_I3        ","146.3.1242 ","R_R3          ","R3.1'_c[R3]                ","R 3 1'I             ",  &
                                               "x+2/3,y+1/3,z+1/3,1;-y,x-y,z,1;x,y,z+1/2,-1                                                             ")
     Shubnikov_info(1243)= Shub_Spgr_Info_Type("147.13 ","P-3         ","147.1.1243 ","P-3           ","P-3.1                      ","-P 3                ",  &
                                               "y,-x+y,-z,1                                                                                             ")
     Shubnikov_info(1244)= Shub_Spgr_Info_Type("147.14 ","P-31'       ","147.2.1244 ","P-31'         ","P-3.1'                     ","-P 3 1'             ",  &
                                               "y,-x+y,-z,1;x,y,z,-1                                                                                    ")
     Shubnikov_info(1245)= Shub_Spgr_Info_Type("147.15 ","P-3'        ","147.3.1245 ","P-3'          ","P-3'                       ","P -3'               ",  &
                                               "y,-x+y,-z,-1                                                                                            ")
     Shubnikov_info(1246)= Shub_Spgr_Info_Type("147.16 ","P_c-3       ","147.4.1246 ","P_2c-3        ","P-3.1'_c[P-3]              ","-P 3 1'c            ",  &
                                               "y,-x+y,-z,1;x,y,z+1/2,-1                                                                                ")
     Shubnikov_info(1247)= Shub_Spgr_Info_Type("148.17 ","R-3         ","148.1.1247 ","R-3           ","R-3.1                      ","-R 3                ",  &
                                               "x+2/3,y+1/3,z+1/3,1;y,-x+y,-z,1                                                                         ")
     Shubnikov_info(1248)= Shub_Spgr_Info_Type("148.18 ","R-31'       ","148.2.1248 ","R-31'         ","R-3.1'                     ","-R 3 1'             ",  &
                                               "x+2/3,y+1/3,z+1/3,1;y,-x+y,-z,1;x,y,z,-1                                                                ")
     Shubnikov_info(1249)= Shub_Spgr_Info_Type("148.19 ","R-3'        ","148.3.1249 ","R-3'          ","R-3'                       ","R -3'               ",  &
                                               "x+2/3,y+1/3,z+1/3,1;y,-x+y,-z,-1                                                                        ")
     Shubnikov_info(1250)= Shub_Spgr_Info_Type("148.20 ","R_I-3       ","148.4.1250 ","R_R-3         ","R-3.1'_c[R-3]              ","-R 3 1'I            ",  &
                                               "x+2/3,y+1/3,z+1/3,1;y,-x+y,-z,1;x,y,z+1/2,-1                                                            ")
     Shubnikov_info(1251)= Shub_Spgr_Info_Type("149.21 ","P312        ","149.1.1251 ","P312          ","P312.1                     ","P 3 2               ",  &
                                               "-y,x-y,z,1;-y,-x,-z,1                                                                                   ")
     Shubnikov_info(1252)= Shub_Spgr_Info_Type("149.22 ","P3121'      ","149.2.1252 ","P3121'        ","P312.1'                    ","P 3 2 1'            ",  &
                                               "-y,x-y,z,1;-y,-x,-z,1;x,y,z,-1                                                                          ")
     Shubnikov_info(1253)= Shub_Spgr_Info_Type("149.23 ","P312'       ","149.3.1253 ","P312'         ","P312'                      ","P 3 2'              ",  &
                                               "-y,x-y,z,1;-y,-x,-z,-1                                                                                  ")
     Shubnikov_info(1254)= Shub_Spgr_Info_Type("149.24 ","P_c312      ","149.4.1254 ","P_2c312       ","P312.1'_c[P312]            ","P 3 2 1'c           ",  &
                                               "-y,x-y,z,1;-y,-x,-z,1;x,y,z+1/2,-1                                                                      ")
     Shubnikov_info(1255)= Shub_Spgr_Info_Type("150.25 ","P321        ","150.1.1255 ","P321          ","P321.1                     ","P 3 2""             ",  &
                                               "-y,x-y,z,1;-x,-x+y,-z,1                                                                                 ")
     Shubnikov_info(1256)= Shub_Spgr_Info_Type("150.26 ","P3211'      ","150.2.1256 ","P3211'        ","P321.1'                    ","P 3 2"" 1'          ",  &
                                               "-y,x-y,z,1;-x,-x+y,-z,1;x,y,z,-1                                                                        ")
     Shubnikov_info(1257)= Shub_Spgr_Info_Type("150.27 ","P32'1       ","150.3.1257 ","P32'1         ","P32'1                      ","P 3 2""'            ",  &
                                               "-y,x-y,z,1;-x,-x+y,-z,-1                                                                                ")
     Shubnikov_info(1258)= Shub_Spgr_Info_Type("150.28 ","P_c321      ","150.4.1258 ","P_2c321       ","P321.1'_c[P321]            ","P 3 2"" 1'c         ",  &
                                               "-y,x-y,z,1;-x,-x+y,-z,1;x,y,z+1/2,-1                                                                    ")
     Shubnikov_info(1259)= Shub_Spgr_Info_Type("151.29 ","P3_112      ","151.1.1259 ","P3_112        ","P3_112.1                   ","P 31 2c (0 0 1)     ",  &
                                               "-y,x-y,z+1/3,1;-y,-x,-z+2/3,1                                                                           ")
     Shubnikov_info(1260)= Shub_Spgr_Info_Type("151.30 ","P3_1121'    ","151.2.1260 ","P3_1121'      ","P3_112.1'                  ","P 31 2c 1' (0 0 1)  ",  &
                                               "-y,x-y,z+1/3,1;-y,-x,-z+2/3,1;x,y,z,-1                                                                  ")
     Shubnikov_info(1261)= Shub_Spgr_Info_Type("151.31 ","P3_112'     ","151.3.1261 ","P3_112'       ","P3_112'                    ","P 31 2c' (0 0 1)    ",  &
                                               "-y,x-y,z+1/3,1;-y,-x,-z+2/3,-1                                                                          ")
     Shubnikov_info(1262)= Shub_Spgr_Info_Type("153.40 ","P_c3_212    ","151.4.1262 ","P_2c3_212     ","P3_212.1'_c[P3_112]        ","P 32 2c 1'c (0 0 -1)",  &
                                               "-y,x-y,z+2/3,1;-y,-x,-z+1/3,1;x,y,z+1/2,-1                                                              ")
     Shubnikov_info(1263)= Shub_Spgr_Info_Type("152.33 ","P3_121      ","152.1.1263 ","P3_121        ","P3_121.1                   ","P 31 2""            ",  &
                                               "-y,x-y,z+1/3,1;-x,-x+y,-z+1/3,1                                                                         ")
     Shubnikov_info(1264)= Shub_Spgr_Info_Type("152.34 ","P3_1211'    ","152.2.1264 ","P3_1211'      ","P3_121.1'                  ","P 31 2"" 1'         ",  &
                                               "-y,x-y,z+1/3,1;-x,-x+y,-z+1/3,1;x,y,z,-1                                                                ")
     Shubnikov_info(1265)= Shub_Spgr_Info_Type("152.35 ","P3_12'1     ","152.3.1265 ","P3_12'1       ","P3_12'1                    ","P 31 2""'           ",  &
                                               "-y,x-y,z+1/3,1;-x,-x+y,-z+1/3,-1                                                                        ")
     Shubnikov_info(1266)= Shub_Spgr_Info_Type("154.44 ","P_c3_221    ","152.4.1266 ","P_2c3_221     ","P3_221.1'_c[P3_121]        ","P 32 2"" 1'c        ",  &
                                               "-y,x-y,z+2/3,1;-x,-x+y,-z+2/3,1;x,y,z+1/2,-1                                                            ")
     Shubnikov_info(1267)= Shub_Spgr_Info_Type("153.37 ","P3_212      ","153.1.1267 ","P3_212        ","P3_212.1                   ","P 32 2c (0 0 -1)    ",  &
                                               "-y,x-y,z+2/3,1;-y,-x,-z+1/3,1                                                                           ")
     Shubnikov_info(1268)= Shub_Spgr_Info_Type("153.38 ","P3_2121'    ","153.2.1268 ","P3_2121'      ","P3_212.1'                  ","P 32 2c 1' (0 0 -1) ",  &
                                               "-y,x-y,z+2/3,1;-y,-x,-z+1/3,1;x,y,z,-1                                                                  ")
     Shubnikov_info(1269)= Shub_Spgr_Info_Type("153.39 ","P3_212'     ","153.3.1269 ","P3_212'       ","P3_212'                    ","P 32 2c' (0 0 -1)   ",  &
                                               "-y,x-y,z+2/3,1;-y,-x,-z+1/3,-1                                                                          ")
     Shubnikov_info(1270)= Shub_Spgr_Info_Type("151.32 ","P_c3_112    ","153.4.1270 ","P_2c3_112     ","P3_112.1'_c[P3_212]        ","P 31 2c 1'c (0 0 1) ",  &
                                               "-y,x-y,z+1/3,1;-y,-x,-z+2/3,1;x,y,z+1/2,-1                                                              ")
     Shubnikov_info(1271)= Shub_Spgr_Info_Type("154.41 ","P3_221      ","154.1.1271 ","P3_221        ","P3_221.1                   ","P 32 2""            ",  &
                                               "-y,x-y,z+2/3,1;-x,-x+y,-z+2/3,1                                                                         ")
     Shubnikov_info(1272)= Shub_Spgr_Info_Type("154.42 ","P3_2211'    ","154.2.1272 ","P3_2211'      ","P3_221.1'                  ","P 32 2"" 1'         ",  &
                                               "-y,x-y,z+2/3,1;-x,-x+y,-z+2/3,1;x,y,z,-1                                                                ")
     Shubnikov_info(1273)= Shub_Spgr_Info_Type("154.43 ","P3_22'1     ","154.3.1273 ","P3_22'1       ","P3_22'1                    ","P 32 2""'           ",  &
                                               "-y,x-y,z+2/3,1;-x,-x+y,-z+2/3,-1                                                                        ")
     Shubnikov_info(1274)= Shub_Spgr_Info_Type("152.36 ","P_c3_121    ","154.4.1274 ","P_2c3_121     ","P3_121.1'_c[P3_221]        ","P 31 2"" 1'c        ",  &
                                               "-y,x-y,z+1/3,1;-x,-x+y,-z+1/3,1;x,y,z+1/2,-1                                                            ")
     Shubnikov_info(1275)= Shub_Spgr_Info_Type("155.45 ","R32         ","155.1.1275 ","R32           ","R32.1                      ","R 3 2""             ",  &
                                               "x+2/3,y+1/3,z+1/3,1;-y,x-y,z,1;-x,-x+y,-z,1                                                             ")
     Shubnikov_info(1276)= Shub_Spgr_Info_Type("155.46 ","R321'       ","155.2.1276 ","R321'         ","R32.1'                     ","R 3 2"" 1'          ",  &
                                               "x+2/3,y+1/3,z+1/3,1;-y,x-y,z,1;-x,-x+y,-z,1;x,y,z,-1                                                    ")
     Shubnikov_info(1277)= Shub_Spgr_Info_Type("155.47 ","R32'        ","155.3.1277 ","R32'          ","R32'                       ","R 3 2""'            ",  &
                                               "x+2/3,y+1/3,z+1/3,1;-y,x-y,z,1;-x,-x+y,-z,-1                                                            ")
     Shubnikov_info(1278)= Shub_Spgr_Info_Type("155.48 ","R_I32       ","155.4.1278 ","R_R32         ","R32.1'_c[R32]              ","R 3 2"" 1'I         ",  &
                                               "x+2/3,y+1/3,z+1/3,1;-y,x-y,z,1;-x,-x+y,-z,1;x,y,z+1/2,-1                                                ")
     Shubnikov_info(1279)= Shub_Spgr_Info_Type("156.49 ","P3m1        ","156.1.1279 ","P3m1          ","P3m1.1                     ","P 3 -2""            ",  &
                                               "-y,x-y,z,1;x,x-y,z,1                                                                                    ")
     Shubnikov_info(1280)= Shub_Spgr_Info_Type("156.50 ","P3m11'      ","156.2.1280 ","P3m11'        ","P3m1.1'                    ","P 3 -2"" 1'         ",  &
                                               "-y,x-y,z,1;x,x-y,z,1;x,y,z,-1                                                                           ")
     Shubnikov_info(1281)= Shub_Spgr_Info_Type("156.51 ","P3m'1       ","156.3.1281 ","P3m'1         ","P3m'1                      ","P 3 -2""'           ",  &
                                               "-y,x-y,z,1;x,x-y,z,-1                                                                                   ")
     Shubnikov_info(1282)= Shub_Spgr_Info_Type("156.52 ","P_c3m1      ","156.4.1282 ","P_2c3m1       ","P3m1.1'_c[P3m1]            ","P 3 -2"" 1'c        ",  &
                                               "-y,x-y,z,1;x,x-y,z,1;x,y,z+1/2,-1                                                                       ")
     Shubnikov_info(1283)= Shub_Spgr_Info_Type("158.60 ","P_c3c1      ","156.5.1283 ","P_2c3m'1      ","P3c1.1'_c[P3m1]            ","P 3 -2""c 1'c       ",  &
                                               "-y,x-y,z,1;x,x-y,z+1/2,1;x,y,z+1/2,-1                                                                   ")
     Shubnikov_info(1284)= Shub_Spgr_Info_Type("157.53 ","P31m        ","157.1.1284 ","P31m          ","P31m.1                     ","P 3 -2              ",  &
                                               "-y,x-y,z,1;y,x,z,1                                                                                      ")
     Shubnikov_info(1285)= Shub_Spgr_Info_Type("157.54 ","P31m1'      ","157.2.1285 ","P31m1'        ","P31m.1'                    ","P 3 -2 1'           ",  &
                                               "-y,x-y,z,1;y,x,z,1;x,y,z,-1                                                                             ")
     Shubnikov_info(1286)= Shub_Spgr_Info_Type("157.55 ","P31m'       ","157.3.1286 ","P31m'         ","P31m'                      ","P 3 -2'             ",  &
                                               "-y,x-y,z,1;y,x,z,-1                                                                                     ")
     Shubnikov_info(1287)= Shub_Spgr_Info_Type("157.56 ","P_c31m      ","157.4.1287 ","P_2c31m       ","P31m.1'_c[P31m]            ","P 3 -2 1'c          ",  &
                                               "-y,x-y,z,1;y,x,z,1;x,y,z+1/2,-1                                                                         ")
     Shubnikov_info(1288)= Shub_Spgr_Info_Type("159.64 ","P_c31c      ","157.5.1288 ","P_2c31m'      ","P31c.1'_c[P31m]            ","P 3 -2c 1'c         ",  &
                                               "-y,x-y,z,1;y,x,z+1/2,1;x,y,z+1/2,-1                                                                     ")
     Shubnikov_info(1289)= Shub_Spgr_Info_Type("158.57 ","P3c1        ","158.1.1289 ","P3c1          ","P3c1.1                     ","P 3 -2""c           ",  &
                                               "-y,x-y,z,1;x,x-y,z+1/2,1                                                                                ")
     Shubnikov_info(1290)= Shub_Spgr_Info_Type("158.58 ","P3c11'      ","158.2.1290 ","P3c11'        ","P3c1.1'                    ","P 3 -2""c 1'        ",  &
                                               "-y,x-y,z,1;x,x-y,z+1/2,1;x,y,z,-1                                                                       ")
     Shubnikov_info(1291)= Shub_Spgr_Info_Type("158.59 ","P3c'1       ","158.3.1291 ","P3c'1         ","P3c'1                      ","P 3 -2""c'          ",  &
                                               "-y,x-y,z,1;x,x-y,z+1/2,-1                                                                               ")
     Shubnikov_info(1292)= Shub_Spgr_Info_Type("159.61 ","P31c        ","159.1.1292 ","P31c          ","P31c.1                     ","P 3 -2c             ",  &
                                               "-y,x-y,z,1;y,x,z+1/2,1                                                                                  ")
     Shubnikov_info(1293)= Shub_Spgr_Info_Type("159.62 ","P31c1'      ","159.2.1293 ","P31c1'        ","P31c.1'                    ","P 3 -2c 1'          ",  &
                                               "-y,x-y,z,1;y,x,z+1/2,1;x,y,z,-1                                                                         ")
     Shubnikov_info(1294)= Shub_Spgr_Info_Type("159.63 ","P31c'       ","159.3.1294 ","P31c'         ","P31c'                      ","P 3 -2c'            ",  &
                                               "-y,x-y,z,1;y,x,z+1/2,-1                                                                                 ")
     Shubnikov_info(1295)= Shub_Spgr_Info_Type("160.65 ","R3m         ","160.1.1295 ","R3m           ","R3m.1                      ","R 3 -2""            ",  &
                                               "x+2/3,y+1/3,z+1/3,1;-y,x-y,z,1;x,x-y,z,1                                                                ")
     Shubnikov_info(1296)= Shub_Spgr_Info_Type("160.66 ","R3m1'       ","160.2.1296 ","R3m1'         ","R3m.1'                     ","R 3 -2"" 1'         ",  &
                                               "x+2/3,y+1/3,z+1/3,1;-y,x-y,z,1;x,x-y,z,1;x,y,z,-1                                                       ")
     Shubnikov_info(1297)= Shub_Spgr_Info_Type("160.67 ","R3m'        ","160.3.1297 ","R3m'          ","R3m'                       ","R 3 -2""'           ",  &
                                               "x+2/3,y+1/3,z+1/3,1;-y,x-y,z,1;x,x-y,z,-1                                                               ")
     Shubnikov_info(1298)= Shub_Spgr_Info_Type("160.68 ","R_I3m       ","160.4.1298 ","R_R3m         ","R3m.1'_c[R3m]              ","R 3 -2"" 1'I        ",  &
                                               "x+2/3,y+1/3,z+1/3,1;-y,x-y,z,1;x,x-y,z,1;x,y,z+1/2,-1                                                   ")
     Shubnikov_info(1299)= Shub_Spgr_Info_Type("161.72 ","R_I3c       ","160.5.1299 ","R_R3m'        ","R3c.1'_c[R3m]              ","R 3 -2""c 1'I       ",  &
                                               "x+2/3,y+1/3,z+1/3,1;-y,x-y,z,1;x,x-y,z+1/2,1;x,y,z+1/2,-1                                               ")
     Shubnikov_info(1300)= Shub_Spgr_Info_Type("161.69 ","R3c         ","161.1.1300 ","R3c           ","R3c.1                      ","R 3 -2""c           ",  &
                                               "x+2/3,y+1/3,z+1/3,1;-y,x-y,z,1;x,x-y,z+1/2,1                                                            ")
     Shubnikov_info(1301)= Shub_Spgr_Info_Type("161.70 ","R3c1'       ","161.2.1301 ","R3c1'         ","R3c.1'                     ","R 3 -2""c 1'        ",  &
                                               "x+2/3,y+1/3,z+1/3,1;-y,x-y,z,1;x,x-y,z+1/2,1;x,y,z,-1                                                   ")
     Shubnikov_info(1302)= Shub_Spgr_Info_Type("161.71 ","R3c'        ","161.3.1302 ","R3c'          ","R3c'                       ","R 3 -2""c'          ",  &
                                               "x+2/3,y+1/3,z+1/3,1;-y,x-y,z,1;x,x-y,z+1/2,-1                                                           ")
     Shubnikov_info(1303)= Shub_Spgr_Info_Type("162.73 ","P-31m       ","162.1.1303 ","P-31m         ","P-31m.1                    ","-P 3 2              ",  &
                                               "y,-x+y,-z,1;y,x,z,1                                                                                     ")
     Shubnikov_info(1304)= Shub_Spgr_Info_Type("162.74 ","P-31m1'     ","162.2.1304 ","P-31m1'       ","P-31m.1'                   ","-P 3 2 1'           ",  &
                                               "y,-x+y,-z,1;y,x,z,1;x,y,z,-1                                                                            ")
     Shubnikov_info(1305)= Shub_Spgr_Info_Type("162.75 ","P-3'1m      ","162.3.1305 ","P-3'1m        ","P-3'1m                     ","P 3 2' -1'          ",  &
                                               "y,-x+y,-z,-1;y,x,z,1                                                                                    ")
     Shubnikov_info(1306)= Shub_Spgr_Info_Type("162.76 ","P-3'1m'     ","162.4.1306 ","P-3'1m'       ","P-3'1m'                    ","P 3 2 -1'           ",  &
                                               "y,-x+y,-z,-1;y,x,z,-1                                                                                   ")
     Shubnikov_info(1307)= Shub_Spgr_Info_Type("162.77 ","P-31m'      ","162.5.1307 ","P-31m'        ","P-31m'                     ","-P 3 2'             ",  &
                                               "y,-x+y,-z,1;y,x,z,-1                                                                                    ")
     Shubnikov_info(1308)= Shub_Spgr_Info_Type("162.78 ","P_c-31m     ","162.6.1308 ","P_2c-31m      ","P-31m.1'_c[P-31m]          ","-P 3 2 1'c          ",  &
                                               "y,-x+y,-z,1;y,x,z,1;x,y,z+1/2,-1                                                                        ")
     Shubnikov_info(1309)= Shub_Spgr_Info_Type("163.84 ","P_c-31c     ","162.7.1309 ","P_2c-31m'     ","P-31c.1'_c[P-31m]          ","-P 3 2c 1'c         ",  &
                                               "y,-x+y,-z,1;y,x,z+1/2,1;x,y,z+1/2,-1                                                                    ")
     Shubnikov_info(1310)= Shub_Spgr_Info_Type("163.79 ","P-31c       ","163.1.1310 ","P-31c         ","P-31c.1                    ","-P 3 2c             ",  &
                                               "y,-x+y,-z,1;y,x,z+1/2,1                                                                                 ")
     Shubnikov_info(1311)= Shub_Spgr_Info_Type("163.80 ","P-31c1'     ","163.2.1311 ","P-31c1'       ","P-31c.1'                   ","-P 3 2c 1'          ",  &
                                               "y,-x+y,-z,1;y,x,z+1/2,1;x,y,z,-1                                                                        ")
     Shubnikov_info(1312)= Shub_Spgr_Info_Type("163.81 ","P-3'1c      ","163.3.1312 ","P-3'1c        ","P-3'1c                     ","P 3 2c' -1'         ",  &
                                               "y,-x+y,-z,-1;y,x,z+1/2,1                                                                                ")
     Shubnikov_info(1313)= Shub_Spgr_Info_Type("163.82 ","P-3'1c'     ","163.4.1313 ","P-3'1c'       ","P-3'1c'                    ","P 3 2c -1'          ",  &
                                               "y,-x+y,-z,-1;y,x,z+1/2,-1                                                                               ")
     Shubnikov_info(1314)= Shub_Spgr_Info_Type("163.83 ","P-31c'      ","163.5.1314 ","P-31c'        ","P-31c'                     ","-P 3 2c'            ",  &
                                               "y,-x+y,-z,1;y,x,z+1/2,-1                                                                                ")
     Shubnikov_info(1315)= Shub_Spgr_Info_Type("164.85 ","P-3m1       ","164.1.1315 ","P-3m1         ","P-3m1.1                    ","-P 3 2""            ",  &
                                               "y,-x+y,-z,1;x,x-y,z,1                                                                                   ")
     Shubnikov_info(1316)= Shub_Spgr_Info_Type("164.86 ","P-3m11'     ","164.2.1316 ","P-3m11'       ","P-3m1.1'                   ","-P 3 2"" 1'         ",  &
                                               "y,-x+y,-z,1;x,x-y,z,1;x,y,z,-1                                                                          ")
     Shubnikov_info(1317)= Shub_Spgr_Info_Type("164.87 ","P-3'm1      ","164.3.1317 ","P-3'm1        ","P-3'm1                     ","P 3 2""' -1'        ",  &
                                               "y,-x+y,-z,-1;x,x-y,z,1                                                                                  ")
     Shubnikov_info(1318)= Shub_Spgr_Info_Type("164.88 ","P-3'm'1     ","164.4.1318 ","P-3'm'1       ","P-3'm'1                    ","P 3 2"" -1'         ",  &
                                               "y,-x+y,-z,-1;x,x-y,z,-1                                                                                 ")
     Shubnikov_info(1319)= Shub_Spgr_Info_Type("164.89 ","P-3m'1      ","164.5.1319 ","P-3m'1        ","P-3m'1                     ","-P 3 2""'           ",  &
                                               "y,-x+y,-z,1;x,x-y,z,-1                                                                                  ")
     Shubnikov_info(1320)= Shub_Spgr_Info_Type("164.90 ","P_c-3m1     ","164.6.1320 ","P_2c-3m1      ","P-3m1.1'_c[P-3m1]          ","-P 3 2"" 1'c        ",  &
                                               "y,-x+y,-z,1;x,x-y,z,1;x,y,z+1/2,-1                                                                      ")
     Shubnikov_info(1321)= Shub_Spgr_Info_Type("165.96 ","P_c-3c1     ","164.7.1321 ","P_2c-3m'1     ","P-3c1.1'_c[P-3m1]          ","-P 3 2""c 1'c       ",  &
                                               "y,-x+y,-z,1;x,x-y,z+1/2,1;x,y,z+1/2,-1                                                                  ")
     Shubnikov_info(1322)= Shub_Spgr_Info_Type("165.91 ","P-3c1       ","165.1.1322 ","P-3c1         ","P-3c1.1                    ","-P 3 2""c           ",  &
                                               "y,-x+y,-z,1;x,x-y,z+1/2,1                                                                               ")
     Shubnikov_info(1323)= Shub_Spgr_Info_Type("165.92 ","P-3c11'     ","165.2.1323 ","P-3c11'       ","P-3c1.1'                   ","-P 3 2""c 1'        ",  &
                                               "y,-x+y,-z,1;x,x-y,z+1/2,1;x,y,z,-1                                                                      ")
     Shubnikov_info(1324)= Shub_Spgr_Info_Type("165.93 ","P-3'c1      ","165.3.1324 ","P-3'c1        ","P-3'c1                     ","P 3 2""c' -1'       ",  &
                                               "y,-x+y,-z,-1;x,x-y,z+1/2,1                                                                              ")
     Shubnikov_info(1325)= Shub_Spgr_Info_Type("165.94 ","P-3'c'1     ","165.4.1325 ","P-3'c'1       ","P-3'c'1                    ","P 3 2""c -1'        ",  &
                                               "y,-x+y,-z,-1;x,x-y,z+1/2,-1                                                                             ")
     Shubnikov_info(1326)= Shub_Spgr_Info_Type("165.95 ","P-3c'1      ","165.5.1326 ","P-3c'1        ","P-3c'1                     ","-P 3 2""c'          ",  &
                                               "y,-x+y,-z,1;x,x-y,z+1/2,-1                                                                              ")
     Shubnikov_info(1327)= Shub_Spgr_Info_Type("166.97 ","R-3m        ","166.1.1327 ","R-3m          ","R-3m.1                     ","-R 3 2""            ",  &
                                               "x+2/3,y+1/3,z+1/3,1;y,-x+y,-z,1;x,x-y,z,1                                                               ")
     Shubnikov_info(1328)= Shub_Spgr_Info_Type("166.98 ","R-3m1'      ","166.2.1328 ","R-3m1'        ","R-3m.1'                    ","-R 3 2"" 1'         ",  &
                                               "x+2/3,y+1/3,z+1/3,1;y,-x+y,-z,1;x,x-y,z,1;x,y,z,-1                                                      ")
     Shubnikov_info(1329)= Shub_Spgr_Info_Type("166.99 ","R-3'm       ","166.3.1329 ","R-3'm         ","R-3'm                      ","R 3 2""' -1'        ",  &
                                               "x+2/3,y+1/3,z+1/3,1;y,-x+y,-z,-1;x,x-y,z,1                                                              ")
     Shubnikov_info(1330)= Shub_Spgr_Info_Type("166.100","R-3'm'      ","166.4.1330 ","R-3'm'        ","R-3'm'                     ","R 3 2"" -1'         ",  &
                                               "x+2/3,y+1/3,z+1/3,1;y,-x+y,-z,-1;x,x-y,z,-1                                                             ")
     Shubnikov_info(1331)= Shub_Spgr_Info_Type("166.101","R-3m'       ","166.5.1331 ","R-3m'         ","R-3m'                      ","-R 3 2""'           ",  &
                                               "x+2/3,y+1/3,z+1/3,1;y,-x+y,-z,1;x,x-y,z,-1                                                              ")
     Shubnikov_info(1332)= Shub_Spgr_Info_Type("166.102","R_I-3m      ","166.6.1332 ","R_R-3m        ","R-3m.1'_c[R-3m]            ","-R 3 2"" 1'I        ",  &
                                               "x+2/3,y+1/3,z+1/3,1;y,-x+y,-z,1;x,x-y,z,1;x,y,z+1/2,-1                                                  ")
     Shubnikov_info(1333)= Shub_Spgr_Info_Type("167.108","R_I-3c      ","166.7.1333 ","R_R-3m'       ","R-3c.1'_c[R-3m]            ","-R 3 2""c 1'I       ",  &
                                               "x+2/3,y+1/3,z+1/3,1;y,-x+y,-z,1;x,x-y,z+1/2,1;x,y,z+1/2,-1                                              ")
     Shubnikov_info(1334)= Shub_Spgr_Info_Type("167.103","R-3c        ","167.1.1334 ","R-3c          ","R-3c.1                     ","-R 3 2""c           ",  &
                                               "x+2/3,y+1/3,z+1/3,1;y,-x+y,-z,1;x,x-y,z+1/2,1                                                           ")
     Shubnikov_info(1335)= Shub_Spgr_Info_Type("167.104","R-3c1'      ","167.2.1335 ","R-3c1'        ","R-3c.1'                    ","-R 3 2""c 1'        ",  &
                                               "x+2/3,y+1/3,z+1/3,1;y,-x+y,-z,1;x,x-y,z+1/2,1;x,y,z,-1                                                  ")
     Shubnikov_info(1336)= Shub_Spgr_Info_Type("167.105","R-3'c       ","167.3.1336 ","R-3'c         ","R-3'c                      ","R 3 2""c' -1'       ",  &
                                               "x+2/3,y+1/3,z+1/3,1;y,-x+y,-z,-1;x,x-y,z+1/2,1                                                          ")
     Shubnikov_info(1337)= Shub_Spgr_Info_Type("167.106","R-3'c'      ","167.4.1337 ","R-3'c'        ","R-3'c'                     ","R 3 2""c -1'        ",  &
                                               "x+2/3,y+1/3,z+1/3,1;y,-x+y,-z,-1;x,x-y,z+1/2,-1                                                         ")
     Shubnikov_info(1338)= Shub_Spgr_Info_Type("167.107","R-3c'       ","167.5.1338 ","R-3c'         ","R-3c'                      ","-R 3 2""c'          ",  &
                                               "x+2/3,y+1/3,z+1/3,1;y,-x+y,-z,1;x,x-y,z+1/2,-1                                                          ")
     Shubnikov_info(1339)= Shub_Spgr_Info_Type("168.109","P6          ","168.1.1339 ","P6            ","P6.1                       ","P 6                 ",  &
                                               "x-y,x,z,1                                                                                               ")
     Shubnikov_info(1340)= Shub_Spgr_Info_Type("168.110","P61'        ","168.2.1340 ","P61'          ","P6.1'                      ","P 6 1'              ",  &
                                               "x-y,x,z,1;x,y,z,-1                                                                                      ")
     Shubnikov_info(1341)= Shub_Spgr_Info_Type("168.111","P6'         ","168.3.1341 ","P6'           ","P6'                        ","P 6'                ",  &
                                               "x-y,x,z,-1                                                                                              ")
     Shubnikov_info(1342)= Shub_Spgr_Info_Type("168.112","P_c6        ","168.4.1342 ","P_2c6         ","P6.1'_c[P6]                ","P 6 1'c             ",  &
                                               "x-y,x,z,1;x,y,z+1/2,-1                                                                                  ")
     Shubnikov_info(1343)= Shub_Spgr_Info_Type("173.132","P_c6_3      ","168.5.1343 ","P_2c6'        ","P6_3.1'_c[P6]              ","P 6c 1'c            ",  &
                                               "x-y,x,z+1/2,1;x,y,z+1/2,-1                                                                              ")
     Shubnikov_info(1344)= Shub_Spgr_Info_Type("169.113","P6_1        ","169.1.1344 ","P6_1          ","P6_1.1                     ","P 61                ",  &
                                               "x-y,x,z+1/6,1                                                                                           ")
     Shubnikov_info(1345)= Shub_Spgr_Info_Type("169.114","P6_11'      ","169.2.1345 ","P6_11'        ","P6_1.1'                    ","P 61 1'             ",  &
                                               "x-y,x,z+1/6,1;x,y,z,-1                                                                                  ")
     Shubnikov_info(1346)= Shub_Spgr_Info_Type("169.115","P6_1'       ","169.3.1346 ","P6_1'         ","P6_1'                      ","P 61'               ",  &
                                               "x-y,x,z+1/6,-1                                                                                          ")
     Shubnikov_info(1347)= Shub_Spgr_Info_Type("170.117","P6_5        ","170.1.1347 ","P6_5          ","P6_5.1                     ","P 65                ",  &
                                               "x-y,x,z+5/6,1                                                                                           ")
     Shubnikov_info(1348)= Shub_Spgr_Info_Type("170.118","P6_51'      ","170.2.1348 ","P6_51'        ","P6_5.1'                    ","P 65 1'             ",  &
                                               "x-y,x,z+5/6,1;x,y,z,-1                                                                                  ")
     Shubnikov_info(1349)= Shub_Spgr_Info_Type("170.119","P6_5'       ","170.3.1349 ","P6_5'         ","P6_5'                      ","P 65'               ",  &
                                               "x-y,x,z+5/6,-1                                                                                          ")
     Shubnikov_info(1350)= Shub_Spgr_Info_Type("171.121","P6_2        ","171.1.1350 ","P6_2          ","P6_2.1                     ","P 62                ",  &
                                               "x-y,x,z+1/3,1                                                                                           ")
     Shubnikov_info(1351)= Shub_Spgr_Info_Type("171.122","P6_21'      ","171.2.1351 ","P6_21'        ","P6_2.1'                    ","P 62 1'             ",  &
                                               "x-y,x,z+1/3,1;x,y,z,-1                                                                                  ")
     Shubnikov_info(1352)= Shub_Spgr_Info_Type("171.123","P6_2'       ","171.3.1352 ","P6_2'         ","P6_2'                      ","P 62'               ",  &
                                               "x-y,x,z+1/3,-1                                                                                          ")
     Shubnikov_info(1353)= Shub_Spgr_Info_Type("169.116","P_c6_1      ","171.4.1353 ","P_2c6_2       ","P6_1.1'_c[P6_2]            ","P 61 1'c            ",  &
                                               "x-y,x,z+1/6,1;x,y,z+1/2,-1                                                                              ")
     Shubnikov_info(1354)= Shub_Spgr_Info_Type("172.128","P_c6_4      ","171.5.1354 ","P_2c6_2'      ","P6_4.1'_c[P6_2]            ","P 64 1'c            ",  &
                                               "x-y,x,z+2/3,1;x,y,z+1/2,-1                                                                              ")
     Shubnikov_info(1355)= Shub_Spgr_Info_Type("172.125","P6_4        ","172.1.1355 ","P6_4          ","P6_4.1                     ","P 64                ",  &
                                               "x-y,x,z+2/3,1                                                                                           ")
     Shubnikov_info(1356)= Shub_Spgr_Info_Type("172.126","P6_41'      ","172.2.1356 ","P6_41'        ","P6_4.1'                    ","P 64 1'             ",  &
                                               "x-y,x,z+2/3,1;x,y,z,-1                                                                                  ")
     Shubnikov_info(1357)= Shub_Spgr_Info_Type("172.127","P6_4'       ","172.3.1357 ","P6_4'         ","P6_4'                      ","P 64'               ",  &
                                               "x-y,x,z+2/3,-1                                                                                          ")
     Shubnikov_info(1358)= Shub_Spgr_Info_Type("171.124","P_c6_2      ","172.4.1358 ","P_2c6_4       ","P6_2.1'_c[P6_4]            ","P 62 1'c            ",  &
                                               "x-y,x,z+1/3,1;x,y,z+1/2,-1                                                                              ")
     Shubnikov_info(1359)= Shub_Spgr_Info_Type("170.120","P_c6_5      ","172.5.1359 ","P_2c6_4'      ","P6_5.1'_c[P6_4]            ","P 65 1'c            ",  &
                                               "x-y,x,z+5/6,1;x,y,z+1/2,-1                                                                              ")
     Shubnikov_info(1360)= Shub_Spgr_Info_Type("173.129","P6_3        ","173.1.1360 ","P6_3          ","P6_3.1                     ","P 6c                ",  &
                                               "x-y,x,z+1/2,1                                                                                           ")
     Shubnikov_info(1361)= Shub_Spgr_Info_Type("173.130","P6_31'      ","173.2.1361 ","P6_31'        ","P6_3.1'                    ","P 6c 1'             ",  &
                                               "x-y,x,z+1/2,1;x,y,z,-1                                                                                  ")
     Shubnikov_info(1362)= Shub_Spgr_Info_Type("173.131","P6_3'       ","173.3.1362 ","P6_3'         ","P6_3'                      ","P 6c'               ",  &
                                               "x-y,x,z+1/2,-1                                                                                          ")
     Shubnikov_info(1363)= Shub_Spgr_Info_Type("174.133","P-6         ","174.1.1363 ","P-6           ","P-6.1                      ","P -6                ",  &
                                               "-x+y,-x,-z,1                                                                                            ")
     Shubnikov_info(1364)= Shub_Spgr_Info_Type("174.134","P-61'       ","174.2.1364 ","P-61'         ","P-6.1'                     ","P -6 1'             ",  &
                                               "-x+y,-x,-z,1;x,y,z,-1                                                                                   ")
     Shubnikov_info(1365)= Shub_Spgr_Info_Type("174.135","P-6'        ","174.3.1365 ","P-6'          ","P-6'                       ","P -6'               ",  &
                                               "-x+y,-x,-z,-1                                                                                           ")
     Shubnikov_info(1366)= Shub_Spgr_Info_Type("174.136","P_c-6       ","174.4.1366 ","P_2c-6        ","P-6.1'_c[P-6]              ","P -6 1'c            ",  &
                                               "-x+y,-x,-z,1;x,y,z+1/2,-1                                                                               ")
     Shubnikov_info(1367)= Shub_Spgr_Info_Type("175.137","P6/m        ","175.1.1367 ","P6/m          ","P6/m.1                     ","-P 6                ",  &
                                               "x-y,x,z,1;x,y,-z,1                                                                                      ")
     Shubnikov_info(1368)= Shub_Spgr_Info_Type("175.138","P6/m1'      ","175.2.1368 ","P6/m1'        ","P6/m.1'                    ","-P 6 1'             ",  &
                                               "x-y,x,z,1;x,y,-z,1;x,y,z,-1                                                                             ")
     Shubnikov_info(1369)= Shub_Spgr_Info_Type("175.139","P6'/m       ","175.3.1369 ","P6'/m         ","P6'/m                      ","P 6' -1'            ",  &
                                               "x-y,x,z,-1;x,y,-z,1                                                                                     ")
     Shubnikov_info(1370)= Shub_Spgr_Info_Type("175.140","P6/m'       ","175.4.1370 ","P6/m'         ","P6/m'                      ","P 6 -1'             ",  &
                                               "x-y,x,z,1;x,y,-z,-1                                                                                     ")
     Shubnikov_info(1371)= Shub_Spgr_Info_Type("175.141","P6'/m'      ","175.5.1371 ","P6'/m'        ","P6'/m'                     ","-P 6'               ",  &
                                               "x-y,x,z,-1;x,y,-z,-1                                                                                    ")
     Shubnikov_info(1372)= Shub_Spgr_Info_Type("175.142","P_c6/m      ","175.6.1372 ","P_2c6/m       ","P6/m.1'_c[P6/m]            ","-P 6 1'c            ",  &
                                               "x-y,x,z,1;x,y,-z,1;x,y,z+1/2,-1                                                                         ")
     Shubnikov_info(1373)= Shub_Spgr_Info_Type("176.148","P_c6_3/m    ","175.7.1373 ","P_2c6'/m      ","P6_3/m.1'_c[P6/m]          ","-P 6c 1'c           ",  &
                                               "x-y,x,z+1/2,1;x,y,-z+1/2,1;x,y,z+1/2,-1                                                                 ")
     Shubnikov_info(1374)= Shub_Spgr_Info_Type("176.143","P6_3/m      ","176.1.1374 ","P6_3/m        ","P6_3/m.1                   ","-P 6c               ",  &
                                               "x-y,x,z+1/2,1;x,y,-z+1/2,1                                                                              ")
     Shubnikov_info(1375)= Shub_Spgr_Info_Type("176.144","P6_3/m1'    ","176.2.1375 ","P6_3/m1'      ","P6_3/m.1'                  ","-P 6c 1'            ",  &
                                               "x-y,x,z+1/2,1;x,y,-z+1/2,1;x,y,z,-1                                                                     ")
     Shubnikov_info(1376)= Shub_Spgr_Info_Type("176.145","P6_3'/m     ","176.3.1376 ","P6_3'/m       ","P6_3'/m                    ","P 6c' -1'           ",  &
                                               "x-y,x,z+1/2,-1;x,y,-z+1/2,1                                                                             ")
     Shubnikov_info(1377)= Shub_Spgr_Info_Type("176.146","P6_3/m'     ","176.4.1377 ","P6_3/m'       ","P6_3/m'                    ","P 6c -1'            ",  &
                                               "x-y,x,z+1/2,1;x,y,-z+1/2,-1                                                                             ")
     Shubnikov_info(1378)= Shub_Spgr_Info_Type("176.147","P6_3'/m'    ","176.5.1378 ","P6_3'/m'      ","P6_3'/m'                   ","-P 6c'              ",  &
                                               "x-y,x,z+1/2,-1;x,y,-z+1/2,-1                                                                            ")
     Shubnikov_info(1379)= Shub_Spgr_Info_Type("177.149","P622        ","177.1.1379 ","P622          ","P622.1                     ","P 6 2               ",  &
                                               "x-y,x,z,1;-x,-x+y,-z,1;-y,-x,-z,1                                                                       ")
     Shubnikov_info(1380)= Shub_Spgr_Info_Type("177.150","P6221'      ","177.2.1380 ","P6221'        ","P622.1'                    ","P 6 2 1'            ",  &
                                               "x-y,x,z,1;-x,-x+y,-z,1;-y,-x,-z,1;x,y,z,-1                                                              ")
     Shubnikov_info(1381)= Shub_Spgr_Info_Type("177.151","P6'2'2      ","177.3.1381 ","P6'2'2        ","P6'2'2                     ","P 6' 2              ",  &
                                               "x-y,x,z,-1;-x,-x+y,-z,-1;-y,-x,-z,1                                                                     ")
     Shubnikov_info(1382)= Shub_Spgr_Info_Type("177.152","P6'22'      ","177.4.1382 ","P6'22'        ","P6'22'                     ","P 6' 2'             ",  &
                                               "x-y,x,z,-1;-x,-x+y,-z,1;-y,-x,-z,-1                                                                     ")
     Shubnikov_info(1383)= Shub_Spgr_Info_Type("177.153","P62'2'      ","177.5.1383 ","P62'2'        ","P62'2'                     ","P 6 2'              ",  &
                                               "x-y,x,z,1;-x,-x+y,-z,-1;-y,-x,-z,-1                                                                     ")
     Shubnikov_info(1384)= Shub_Spgr_Info_Type("177.154","P_c622      ","177.6.1384 ","P_2c622       ","P622.1'_c[P622]            ","P 6 2 1'c           ",  &
                                               "x-y,x,z,1;-x,-x+y,-z,1;-y,-x,-z,1;x,y,z+1/2,-1                                                          ")
     Shubnikov_info(1385)= Shub_Spgr_Info_Type("182.184","P_c6_322    ","177.7.1385 ","P_2c6'22'     ","P6_322.1'_c[P622]          ","P 6c 2c 1'c         ",  &
                                               "x-y,x,z+1/2,1;-x,-x+y,-z,1;-y,-x,-z+1/2,1;x,y,z+1/2,-1                                                  ")
     Shubnikov_info(1386)= Shub_Spgr_Info_Type("178.155","P6_122      ","178.1.1386 ","P6_122        ","P6_122.1                   ","P 61 2 (0 0 -1)     ",  &
                                               "x-y,x,z+1/6,1;-x,-x+y,-z+2/3,1;-y,-x,-z+5/6,1                                                           ")
     Shubnikov_info(1387)= Shub_Spgr_Info_Type("178.156","P6_1221'    ","178.2.1387 ","P6_1221'      ","P6_122.1'                  ","P 61 2 1' (0 0 -1)  ",  &
                                               "x-y,x,z+1/6,1;-x,-x+y,-z+2/3,1;-y,-x,-z+5/6,1;x,y,z,-1                                                  ")
     Shubnikov_info(1388)= Shub_Spgr_Info_Type("178.157","P6_1'2'2    ","178.3.1388 ","P6_1'2'2      ","P6_1'2'2                   ","P 61' 2 (0 0 -1)    ",  &
                                               "x-y,x,z+1/6,-1;-x,-x+y,-z+2/3,-1;-y,-x,-z+5/6,1                                                         ")
     Shubnikov_info(1389)= Shub_Spgr_Info_Type("178.158","P6_1'22'    ","178.4.1389 ","P6_1'22'      ","P6_1'22'                   ","P 61' 2' (0 0 -1)   ",  &
                                               "x-y,x,z+1/6,-1;-x,-x+y,-z+2/3,1;-y,-x,-z+5/6,-1                                                         ")
     Shubnikov_info(1390)= Shub_Spgr_Info_Type("178.159","P6_12'2'    ","178.5.1390 ","P6_12'2'      ","P6_12'2'                   ","P 61 2' (0 0 -1)    ",  &
                                               "x-y,x,z+1/6,1;-x,-x+y,-z+2/3,-1;-y,-x,-z+5/6,-1                                                         ")
     Shubnikov_info(1391)= Shub_Spgr_Info_Type("179.161","P6_522      ","179.1.1391 ","P6_522        ","P6_522.1                   ","P 65 2 (0 0 1)      ",  &
                                               "x-y,x,z+5/6,1;-x,-x+y,-z+1/3,1;-y,-x,-z+1/6,1                                                           ")
     Shubnikov_info(1392)= Shub_Spgr_Info_Type("179.162","P6_5221'    ","179.2.1392 ","P6_5221'      ","P6_522.1'                  ","P 65 2 1' (0 0 1)   ",  &
                                               "x-y,x,z+5/6,1;-x,-x+y,-z+1/3,1;-y,-x,-z+1/6,1;x,y,z,-1                                                  ")
     Shubnikov_info(1393)= Shub_Spgr_Info_Type("179.163","P6_5'2'2    ","179.3.1393 ","P6_5'2'2      ","P6_5'2'2                   ","P 65' 2 (0 0 1)     ",  &
                                               "x-y,x,z+5/6,-1;-x,-x+y,-z+1/3,-1;-y,-x,-z+1/6,1                                                         ")
     Shubnikov_info(1394)= Shub_Spgr_Info_Type("179.164","P6_5'22'    ","179.4.1394 ","P6_5'22'      ","P6_5'22'                   ","P 65' 2' (0 0 1)    ",  &
                                               "x-y,x,z+5/6,-1;-x,-x+y,-z+1/3,1;-y,-x,-z+1/6,-1                                                         ")
     Shubnikov_info(1395)= Shub_Spgr_Info_Type("179.165","P6_52'2'    ","179.5.1395 ","P6_52'2'      ","P6_52'2'                   ","P 65 2' (0 0 1)     ",  &
                                               "x-y,x,z+5/6,1;-x,-x+y,-z+1/3,-1;-y,-x,-z+1/6,-1                                                         ")
     Shubnikov_info(1396)= Shub_Spgr_Info_Type("180.167","P6_222      ","180.1.1396 ","P6_222        ","P6_222.1                   ","P 62 2c (0 0 1)     ",  &
                                               "x-y,x,z+1/3,1;-x,-x+y,-z+1/3,1;-y,-x,-z+2/3,1                                                           ")
     Shubnikov_info(1397)= Shub_Spgr_Info_Type("180.168","P6_2221'    ","180.2.1397 ","P6_2221'      ","P6_222.1'                  ","P 62 2c 1' (0 0 1)  ",  &
                                               "x-y,x,z+1/3,1;-x,-x+y,-z+1/3,1;-y,-x,-z+2/3,1;x,y,z,-1                                                  ")
     Shubnikov_info(1398)= Shub_Spgr_Info_Type("180.169","P6_2'2'2    ","180.3.1398 ","P6_2'2'2      ","P6_2'2'2                   ","P 62' 2c (0 0 1)    ",  &
                                               "x-y,x,z+1/3,-1;-x,-x+y,-z+1/3,-1;-y,-x,-z+2/3,1                                                         ")
     Shubnikov_info(1399)= Shub_Spgr_Info_Type("180.170","P6_2'22'    ","180.4.1399 ","P6_2'22'      ","P6_2'22'                   ","P 62' 2c' (0 0 1)   ",  &
                                               "x-y,x,z+1/3,-1;-x,-x+y,-z+1/3,1;-y,-x,-z+2/3,-1                                                         ")
     Shubnikov_info(1400)= Shub_Spgr_Info_Type("180.171","P6_22'2'    ","180.5.1400 ","P6_22'2'      ","P6_22'2'                   ","P 62 2c' (0 0 1)    ",  &
                                               "x-y,x,z+1/3,1;-x,-x+y,-z+1/3,-1;-y,-x,-z+2/3,-1                                                         ")
     Shubnikov_info(1401)= Shub_Spgr_Info_Type("178.160","P_c6_122    ","180.6.1401 ","P_2c6_222     ","P6_122.1'_c[P6_222]        ","P 61 2 1'c (0 0 -1) ",  &
                                               "x-y,x,z+1/6,1;-x,-x+y,-z+2/3,1;-y,-x,-z+5/6,1;x,y,z+1/2,-1                                              ")
     Shubnikov_info(1402)= Shub_Spgr_Info_Type("181.178","P_c6_422    ","180.7.1402 ","P_2c6_2'22'   ","P6_422.1'_c[P6_222]        ","P 64 2c 1'c (0 0 -1)",  &
                                               "x-y,x,z+2/3,1;-x,-x+y,-z+2/3,1;-y,-x,-z+1/3,1;x,y,z+1/2,-1                                              ")
     Shubnikov_info(1403)= Shub_Spgr_Info_Type("181.173","P6_422      ","181.1.1403 ","P6_422        ","P6_422.1                   ","P 64 2c (0 0 -1)    ",  &
                                               "x-y,x,z+2/3,1;-x,-x+y,-z+2/3,1;-y,-x,-z+1/3,1                                                           ")
     Shubnikov_info(1404)= Shub_Spgr_Info_Type("181.174","P6_4221'    ","181.2.1404 ","P6_4221'      ","P6_422.1'                  ","P 64 2c 1' (0 0 -1) ",  &
                                               "x-y,x,z+2/3,1;-x,-x+y,-z+2/3,1;-y,-x,-z+1/3,1;x,y,z,-1                                                  ")
     Shubnikov_info(1405)= Shub_Spgr_Info_Type("181.175","P6_4'2'2    ","181.3.1405 ","P6_4'2'2      ","P6_4'2'2                   ","P 64' 2c (0 0 -1)   ",  &
                                               "x-y,x,z+2/3,-1;-x,-x+y,-z+2/3,-1;-y,-x,-z+1/3,1                                                         ")
     Shubnikov_info(1406)= Shub_Spgr_Info_Type("181.176","P6_4'22'    ","181.4.1406 ","P6_4'22'      ","P6_4'22'                   ","P 64' 2c' (0 0 -1)  ",  &
                                               "x-y,x,z+2/3,-1;-x,-x+y,-z+2/3,1;-y,-x,-z+1/3,-1                                                         ")
     Shubnikov_info(1407)= Shub_Spgr_Info_Type("181.177","P6_42'2'    ","181.5.1407 ","P6_42'2'      ","P6_42'2'                   ","P 64 2c' (0 0 -1)   ",  &
                                               "x-y,x,z+2/3,1;-x,-x+y,-z+2/3,-1;-y,-x,-z+1/3,-1                                                         ")
     Shubnikov_info(1408)= Shub_Spgr_Info_Type("180.172","P_c6_222    ","181.6.1408 ","P_2c6_422     ","P6_222.1'_c[P6_422]        ","P 62 2c 1'c (0 0 1) ",  &
                                               "x-y,x,z+1/3,1;-x,-x+y,-z+1/3,1;-y,-x,-z+2/3,1;x,y,z+1/2,-1                                              ")
     Shubnikov_info(1409)= Shub_Spgr_Info_Type("179.166","P_c6_522    ","181.7.1409 ","P_2c6_4'2'2   ","P6_522.1'_c[P6_422]        ","P 65 2 1'c (0 0 1)  ",  &
                                               "x-y,x,z+5/6,1;-x,-x+y,-z+1/3,1;-y,-x,-z+1/6,1;x,y,z+1/2,-1                                              ")
     Shubnikov_info(1410)= Shub_Spgr_Info_Type("182.179","P6_322      ","182.1.1410 ","P6_322        ","P6_322.1                   ","P 6c 2c             ",  &
                                               "x-y,x,z+1/2,1;-x,-x+y,-z,1;-y,-x,-z+1/2,1                                                               ")
     Shubnikov_info(1411)= Shub_Spgr_Info_Type("182.180","P6_3221'    ","182.2.1411 ","P6_3221'      ","P6_322.1'                  ","P 6c 2c 1'          ",  &
                                               "x-y,x,z+1/2,1;-x,-x+y,-z,1;-y,-x,-z+1/2,1;x,y,z,-1                                                      ")
     Shubnikov_info(1412)= Shub_Spgr_Info_Type("182.181","P6_3'2'2    ","182.3.1412 ","P6_3'2'2      ","P6_3'2'2                   ","P 6c' 2c            ",  &
                                               "x-y,x,z+1/2,-1;-x,-x+y,-z,-1;-y,-x,-z+1/2,1                                                             ")
     Shubnikov_info(1413)= Shub_Spgr_Info_Type("182.182","P6_3'22'    ","182.4.1413 ","P6_3'22'      ","P6_3'22'                   ","P 6c' 2c'           ",  &
                                               "x-y,x,z+1/2,-1;-x,-x+y,-z,1;-y,-x,-z+1/2,-1                                                             ")
     Shubnikov_info(1414)= Shub_Spgr_Info_Type("182.183","P6_32'2'    ","182.5.1414 ","P6_32'2'      ","P6_32'2'                   ","P 6c 2c'            ",  &
                                               "x-y,x,z+1/2,1;-x,-x+y,-z,-1;-y,-x,-z+1/2,-1                                                             ")
     Shubnikov_info(1415)= Shub_Spgr_Info_Type("183.185","P6mm        ","183.1.1415 ","P6mm          ","P6mm.1                     ","P 6 -2              ",  &
                                               "x-y,x,z,1;x,x-y,z,1;y,x,z,1                                                                             ")
     Shubnikov_info(1416)= Shub_Spgr_Info_Type("183.186","P6mm1'      ","183.2.1416 ","P6mm1'        ","P6mm.1'                    ","P 6 -2 1'           ",  &
                                               "x-y,x,z,1;x,x-y,z,1;y,x,z,1;x,y,z,-1                                                                    ")
     Shubnikov_info(1417)= Shub_Spgr_Info_Type("183.187","P6'm'm      ","183.3.1417 ","P6'm'm        ","P6'm'm                     ","P 6' -2             ",  &
                                               "x-y,x,z,-1;x,x-y,z,-1;y,x,z,1                                                                           ")
     Shubnikov_info(1418)= Shub_Spgr_Info_Type("183.188","P6'mm'      ","183.4.1418 ","P6'mm'        ","P6'mm'                     ","P 6' -2'            ",  &
                                               "x-y,x,z,-1;x,x-y,z,1;y,x,z,-1                                                                           ")
     Shubnikov_info(1419)= Shub_Spgr_Info_Type("183.189","P6m'm'      ","183.5.1419 ","P6m'm'        ","P6m'm'                     ","P 6 -2'             ",  &
                                               "x-y,x,z,1;x,x-y,z,-1;y,x,z,-1                                                                           ")
     Shubnikov_info(1420)= Shub_Spgr_Info_Type("183.190","P_c6mm      ","183.6.1420 ","P_2c6mm       ","P6mm.1'_c[P6mm]            ","P 6 -2 1'c          ",  &
                                               "x-y,x,z,1;x,x-y,z,1;y,x,z,1;x,y,z+1/2,-1                                                                ")
     Shubnikov_info(1421)= Shub_Spgr_Info_Type("185.202","P_c6_3cm    ","183.7.1421 ","P_2c6'm'm     ","P6_3cm.1'_c[P6mm]          ","P 6c -2 1'c         ",  &
                                               "x-y,x,z+1/2,1;x,x-y,z+1/2,1;y,x,z,1;x,y,z+1/2,-1                                                        ")
     Shubnikov_info(1422)= Shub_Spgr_Info_Type("186.208","P_c6_3mc    ","183.8.1422 ","P_2c6'mm'     ","P6_3mc.1'_c[P6mm]          ","P 6c -2c 1'c        ",  &
                                               "x-y,x,z+1/2,1;x,x-y,z,1;y,x,z+1/2,1;x,y,z+1/2,-1                                                        ")
     Shubnikov_info(1423)= Shub_Spgr_Info_Type("184.196","P_c6cc      ","183.9.1423 ","P_2c6m'm'     ","P6cc.1'_c[P6mm]            ","P 6 -2c 1'c         ",  &
                                               "x-y,x,z,1;x,x-y,z+1/2,1;y,x,z+1/2,1;x,y,z+1/2,-1                                                        ")
     Shubnikov_info(1424)= Shub_Spgr_Info_Type("184.191","P6cc        ","184.1.1424 ","P6cc          ","P6cc.1                     ","P 6 -2c             ",  &
                                               "x-y,x,z,1;x,x-y,z+1/2,1;y,x,z+1/2,1                                                                     ")
     Shubnikov_info(1425)= Shub_Spgr_Info_Type("184.192","P6cc1'      ","184.2.1425 ","P6cc1'        ","P6cc.1'                    ","P 6 -2c 1'          ",  &
                                               "x-y,x,z,1;x,x-y,z+1/2,1;y,x,z+1/2,1;x,y,z,-1                                                            ")
     Shubnikov_info(1426)= Shub_Spgr_Info_Type("184.193","P6'c'c      ","184.3.1426 ","P6'c'c        ","P6'c'c                     ","P 6' -2c            ",  &
                                               "x-y,x,z,-1;x,x-y,z+1/2,-1;y,x,z+1/2,1                                                                   ")
     Shubnikov_info(1427)= Shub_Spgr_Info_Type("184.194","P6'cc'      ","184.4.1427 ","P6'cc'        ","P6'cc'                     ","P 6' -2c'           ",  &
                                               "x-y,x,z,-1;x,x-y,z+1/2,1;y,x,z+1/2,-1                                                                   ")
     Shubnikov_info(1428)= Shub_Spgr_Info_Type("184.195","P6c'c'      ","184.5.1428 ","P6c'c'        ","P6c'c'                     ","P 6 -2c'            ",  &
                                               "x-y,x,z,1;x,x-y,z+1/2,-1;y,x,z+1/2,-1                                                                   ")
     Shubnikov_info(1429)= Shub_Spgr_Info_Type("185.197","P6_3cm      ","185.1.1429 ","P6_3cm        ","P6_3cm.1                   ","P 6c -2             ",  &
                                               "x-y,x,z+1/2,1;x,x-y,z+1/2,1;y,x,z,1                                                                     ")
     Shubnikov_info(1430)= Shub_Spgr_Info_Type("185.198","P6_3cm1'    ","185.2.1430 ","P6_3cm1'      ","P6_3cm.1'                  ","P 6c -2 1'          ",  &
                                               "x-y,x,z+1/2,1;x,x-y,z+1/2,1;y,x,z,1;x,y,z,-1                                                            ")
     Shubnikov_info(1431)= Shub_Spgr_Info_Type("185.199","P6_3'c'm    ","185.3.1431 ","P6_3'c'm      ","P6_3'c'm                   ","P 6c' -2            ",  &
                                               "x-y,x,z+1/2,-1;x,x-y,z+1/2,-1;y,x,z,1                                                                   ")
     Shubnikov_info(1432)= Shub_Spgr_Info_Type("185.200","P6_3'cm'    ","185.4.1432 ","P6_3'cm'      ","P6_3'cm'                   ","P 6c' -2'           ",  &
                                               "x-y,x,z+1/2,-1;x,x-y,z+1/2,1;y,x,z,-1                                                                   ")
     Shubnikov_info(1433)= Shub_Spgr_Info_Type("185.201","P6_3c'm'    ","185.5.1433 ","P6_3c'm'      ","P6_3c'm'                   ","P 6c -2'            ",  &
                                               "x-y,x,z+1/2,1;x,x-y,z+1/2,-1;y,x,z,-1                                                                   ")
     Shubnikov_info(1434)= Shub_Spgr_Info_Type("186.203","P6_3mc      ","186.1.1434 ","P6_3mc        ","P6_3mc.1                   ","P 6c -2c            ",  &
                                               "x-y,x,z+1/2,1;x,x-y,z,1;y,x,z+1/2,1                                                                     ")
     Shubnikov_info(1435)= Shub_Spgr_Info_Type("186.204","P6_3mc1'    ","186.2.1435 ","P6_3mc1'      ","P6_3mc.1'                  ","P 6c -2c 1'         ",  &
                                               "x-y,x,z+1/2,1;x,x-y,z,1;y,x,z+1/2,1;x,y,z,-1                                                            ")
     Shubnikov_info(1436)= Shub_Spgr_Info_Type("186.205","P6_3'm'c    ","186.3.1436 ","P6_3'm'c      ","P6_3'm'c                   ","P 6c' -2c           ",  &
                                               "x-y,x,z+1/2,-1;x,x-y,z,-1;y,x,z+1/2,1                                                                   ")
     Shubnikov_info(1437)= Shub_Spgr_Info_Type("186.206","P6_3'mc'    ","186.4.1437 ","P6_3'mc'      ","P6_3'mc'                   ","P 6c' -2c'          ",  &
                                               "x-y,x,z+1/2,-1;x,x-y,z,1;y,x,z+1/2,-1                                                                   ")
     Shubnikov_info(1438)= Shub_Spgr_Info_Type("186.207","P6_3m'c'    ","186.5.1438 ","P6_3m'c'      ","P6_3m'c'                   ","P 6c -2c'           ",  &
                                               "x-y,x,z+1/2,1;x,x-y,z,-1;y,x,z+1/2,-1                                                                   ")
     Shubnikov_info(1439)= Shub_Spgr_Info_Type("187.209","P-6m2       ","187.1.1439 ","P-6m2         ","P-6m2.1                    ","P -6 2              ",  &
                                               "-x+y,-x,-z,1;x,x-y,z,1;-y,-x,-z,1                                                                       ")
     Shubnikov_info(1440)= Shub_Spgr_Info_Type("187.210","P-6m21'     ","187.2.1440 ","P-6m21'       ","P-6m2.1'                   ","P -6 2 1'           ",  &
                                               "-x+y,-x,-z,1;x,x-y,z,1;-y,-x,-z,1;x,y,z,-1                                                              ")
     Shubnikov_info(1441)= Shub_Spgr_Info_Type("187.211","P-6'm'2     ","187.3.1441 ","P-6'm'2       ","P-6'm'2                    ","P -6' 2             ",  &
                                               "-x+y,-x,-z,-1;x,x-y,z,-1;-y,-x,-z,1                                                                     ")
     Shubnikov_info(1442)= Shub_Spgr_Info_Type("187.212","P-6'm2'     ","187.4.1442 ","P-6'm2'       ","P-6'm2'                    ","P -6' 2'            ",  &
                                               "-x+y,-x,-z,-1;x,x-y,z,1;-y,-x,-z,-1                                                                     ")
     Shubnikov_info(1443)= Shub_Spgr_Info_Type("187.213","P-6m'2'     ","187.5.1443 ","P-6m'2'       ","P-6m'2'                    ","P -6 2'             ",  &
                                               "-x+y,-x,-z,1;x,x-y,z,-1;-y,-x,-z,-1                                                                     ")
     Shubnikov_info(1444)= Shub_Spgr_Info_Type("187.214","P_c-6m2     ","187.6.1444 ","P_2c-6m2      ","P-6m2.1'_c[P-6m2]          ","P -6 2 1'c          ",  &
                                               "-x+y,-x,-z,1;x,x-y,z,1;-y,-x,-z,1;x,y,z+1/2,-1                                                          ")
     Shubnikov_info(1445)= Shub_Spgr_Info_Type("188.220","P_c-6c2     ","187.7.1445 ","P_2c-6'm'2    ","P-6c2.1'_c[P-6m2]          ","P -6c 2 1'c         ",  &
                                               "-x+y,-x,-z+1/2,1;x,x-y,z+1/2,1;-y,-x,-z,1;x,y,z+1/2,-1                                                  ")
     Shubnikov_info(1446)= Shub_Spgr_Info_Type("188.215","P-6c2       ","188.1.1446 ","P-6c2         ","P-6c2.1                    ","P -6c 2             ",  &
                                               "-x+y,-x,-z+1/2,1;x,x-y,z+1/2,1;-y,-x,-z,1                                                               ")
     Shubnikov_info(1447)= Shub_Spgr_Info_Type("188.216","P-6c21'     ","188.2.1447 ","P-6c21'       ","P-6c2.1'                   ","P -6c 2 1'          ",  &
                                               "-x+y,-x,-z+1/2,1;x,x-y,z+1/2,1;-y,-x,-z,1;x,y,z,-1                                                      ")
     Shubnikov_info(1448)= Shub_Spgr_Info_Type("188.217","P-6'c'2     ","188.3.1448 ","P-6'c'2       ","P-6'c'2                    ","P -6c' 2            ",  &
                                               "-x+y,-x,-z+1/2,-1;x,x-y,z+1/2,-1;-y,-x,-z,1                                                             ")
     Shubnikov_info(1449)= Shub_Spgr_Info_Type("188.218","P-6'c2'     ","188.4.1449 ","P-6'c2'       ","P-6'c2'                    ","P -6c' 2'           ",  &
                                               "-x+y,-x,-z+1/2,-1;x,x-y,z+1/2,1;-y,-x,-z,-1                                                             ")
     Shubnikov_info(1450)= Shub_Spgr_Info_Type("188.219","P-6c'2'     ","188.5.1450 ","P-6c'2'       ","P-6c'2'                    ","P -6c 2'            ",  &
                                               "-x+y,-x,-z+1/2,1;x,x-y,z+1/2,-1;-y,-x,-z,-1                                                             ")
     Shubnikov_info(1451)= Shub_Spgr_Info_Type("189.221","P-62m       ","189.1.1451 ","P-62m         ","P-62m.1                    ","P -6 -2             ",  &
                                               "-x+y,-x,-z,1;-x,-x+y,-z,1;y,x,z,1                                                                       ")
     Shubnikov_info(1452)= Shub_Spgr_Info_Type("189.222","P-62m1'     ","189.2.1452 ","P-62m1'       ","P-62m.1'                   ","P -6 -2 1'          ",  &
                                               "-x+y,-x,-z,1;-x,-x+y,-z,1;y,x,z,1;x,y,z,-1                                                              ")
     Shubnikov_info(1453)= Shub_Spgr_Info_Type("189.223","P-6'2'm     ","189.3.1453 ","P-6'2'm       ","P-6'2'm                    ","P -6' -2            ",  &
                                               "-x+y,-x,-z,-1;-x,-x+y,-z,-1;y,x,z,1                                                                     ")
     Shubnikov_info(1454)= Shub_Spgr_Info_Type("189.224","P-6'2m'     ","189.4.1454 ","P-6'2m'       ","P-6'2m'                    ","P -6' -2'           ",  &
                                               "-x+y,-x,-z,-1;-x,-x+y,-z,1;y,x,z,-1                                                                     ")
     Shubnikov_info(1455)= Shub_Spgr_Info_Type("189.225","P-62'm'     ","189.5.1455 ","P-62'm'       ","P-62'm'                    ","P -6 -2'            ",  &
                                               "-x+y,-x,-z,1;-x,-x+y,-z,-1;y,x,z,-1                                                                     ")
     Shubnikov_info(1456)= Shub_Spgr_Info_Type("189.226","P_c-62m     ","189.6.1456 ","P_2c-62m      ","P-62m.1'_c[P-62m]          ","P -6 -2 1'c         ",  &
                                               "-x+y,-x,-z,1;-x,-x+y,-z,1;y,x,z,1;x,y,z+1/2,-1                                                          ")
     Shubnikov_info(1457)= Shub_Spgr_Info_Type("190.232","P_c-62c     ","189.7.1457 ","P_2c-6'2m'    ","P-62c.1'_c[P-62m]          ","P -6c -2c 1'c       ",  &
                                               "-x+y,-x,-z+1/2,1;-x,-x+y,-z,1;y,x,z+1/2,1;x,y,z+1/2,-1                                                  ")
     Shubnikov_info(1458)= Shub_Spgr_Info_Type("190.227","P-62c       ","190.1.1458 ","P-62c         ","P-62c.1                    ","P -6c -2c           ",  &
                                               "-x+y,-x,-z+1/2,1;-x,-x+y,-z,1;y,x,z+1/2,1                                                               ")
     Shubnikov_info(1459)= Shub_Spgr_Info_Type("190.228","P-62c1'     ","190.2.1459 ","P-62c1'       ","P-62c.1'                   ","P -6c -2c 1'        ",  &
                                               "-x+y,-x,-z+1/2,1;-x,-x+y,-z,1;y,x,z+1/2,1;x,y,z,-1                                                      ")
     Shubnikov_info(1460)= Shub_Spgr_Info_Type("190.229","P-6'2'c     ","190.3.1460 ","P-6'2'c       ","P-6'2'c                    ","P -6c' -2c          ",  &
                                               "-x+y,-x,-z+1/2,-1;-x,-x+y,-z,-1;y,x,z+1/2,1                                                             ")
     Shubnikov_info(1461)= Shub_Spgr_Info_Type("190.230","P-6'2c'     ","190.4.1461 ","P-6'2c'       ","P-6'2c'                    ","P -6c' -2c'         ",  &
                                               "-x+y,-x,-z+1/2,-1;-x,-x+y,-z,1;y,x,z+1/2,-1                                                             ")
     Shubnikov_info(1462)= Shub_Spgr_Info_Type("190.231","P-62'c'     ","190.5.1462 ","P-62'c'       ","P-62'c'                    ","P -6c -2c'          ",  &
                                               "-x+y,-x,-z+1/2,1;-x,-x+y,-z,-1;y,x,z+1/2,-1                                                             ")
     Shubnikov_info(1463)= Shub_Spgr_Info_Type("191.233","P6/mmm      ","191.1.1463 ","P6/mmm        ","P6/mmm.1                   ","-P 6 2              ",  &
                                               "x-y,x,z,1;x,y,-z,1;x,x-y,z,1;y,x,z,1                                                                    ")
     Shubnikov_info(1464)= Shub_Spgr_Info_Type("191.234","P6/mmm1'    ","191.2.1464 ","P6/mmm1'      ","P6/mmm.1'                  ","-P 6 2 1'           ",  &
                                               "x-y,x,z,1;x,y,-z,1;x,x-y,z,1;y,x,z,1;x,y,z,-1                                                           ")
     Shubnikov_info(1465)= Shub_Spgr_Info_Type("191.235","P6/m'mm     ","191.3.1465 ","P6/m'mm       ","P6/m'mm                    ","P 6 2' -1'          ",  &
                                               "x-y,x,z,1;x,y,-z,-1;x,x-y,z,1;y,x,z,1                                                                   ")
     Shubnikov_info(1466)= Shub_Spgr_Info_Type("191.236","P6'/mm'm    ","191.4.1466 ","P6'/mm'm      ","P6'/mm'm                   ","P 6' 2' -1'         ",  &
                                               "x-y,x,z,-1;x,y,-z,1;x,x-y,z,-1;y,x,z,1                                                                  ")
     Shubnikov_info(1467)= Shub_Spgr_Info_Type("191.237","P6'/mmm'    ","191.5.1467 ","P6'/mmm'      ","P6'/mmm'                   ","P 6' 2 -1'          ",  &
                                               "x-y,x,z,-1;x,y,-z,1;x,x-y,z,1;y,x,z,-1                                                                  ")
     Shubnikov_info(1468)= Shub_Spgr_Info_Type("191.238","P6'/m'm'm   ","191.6.1468 ","P6'/m'm'm     ","P6'/m'm'm                  ","-P 6' 2             ",  &
                                               "x-y,x,z,-1;x,y,-z,-1;x,x-y,z,-1;y,x,z,1                                                                 ")
     Shubnikov_info(1469)= Shub_Spgr_Info_Type("191.239","P6'/m'mm'   ","191.7.1469 ","P6'/m'mm'     ","P6'/m'mm'                  ","-P 6' 2'            ",  &
                                               "x-y,x,z,-1;x,y,-z,-1;x,x-y,z,1;y,x,z,-1                                                                 ")
     Shubnikov_info(1470)= Shub_Spgr_Info_Type("191.240","P6/mm'm'    ","191.8.1470 ","P6/mm'm'      ","P6/mm'm'                   ","-P 6 2'             ",  &
                                               "x-y,x,z,1;x,y,-z,1;x,x-y,z,-1;y,x,z,-1                                                                  ")
     Shubnikov_info(1471)= Shub_Spgr_Info_Type("191.241","P6/m'm'm'   ","191.9.1471 ","P6/m'm'm'     ","P6/m'm'm'                  ","P 6 2 -1'           ",  &
                                               "x-y,x,z,1;x,y,-z,-1;x,x-y,z,-1;y,x,z,-1                                                                 ")
     Shubnikov_info(1472)= Shub_Spgr_Info_Type("191.242","P_c6/mmm    ","191.10.1472","P_2c6/mmm     ","P6/mmm.1'_c[P6/mmm]        ","-P 6 2 1'c          ",  &
                                               "x-y,x,z,1;x,y,-z,1;x,x-y,z,1;y,x,z,1;x,y,z+1/2,-1                                                       ")
     Shubnikov_info(1473)= Shub_Spgr_Info_Type("193.262","P_c6_3/mcm  ","191.11.1473","P_2c6'/mm'm   ","P6_3/mcm.1'_c[P6/mmm]      ","-P 6c 2 1'c         ",  &
                                               "x-y,x,z+1/2,1;x,y,-z+1/2,1;x,x-y,z+1/2,1;y,x,z,1;x,y,z+1/2,-1                                           ")
     Shubnikov_info(1474)= Shub_Spgr_Info_Type("194.272","P_c6_3/mmc  ","191.12.1474","P_2c6'/mmm'   ","P6_3/mmc.1'_c[P6/mmm]      ","-P 6c 2c 1'c        ",  &
                                               "x-y,x,z+1/2,1;x,y,-z+1/2,1;x,x-y,z,1;y,x,z+1/2,1;x,y,z+1/2,-1                                           ")
     Shubnikov_info(1475)= Shub_Spgr_Info_Type("192.252","P_c6/mcc    ","191.13.1475","P_2c6/mm'm'   ","P6/mcc.1'_c[P6/mmm]        ","-P 6 2c 1'c         ",  &
                                               "x-y,x,z,1;x,y,-z,1;x,x-y,z+1/2,1;y,x,z+1/2,1;x,y,z+1/2,-1                                               ")
     Shubnikov_info(1476)= Shub_Spgr_Info_Type("192.243","P6/mcc      ","192.1.1476 ","P6/mcc        ","P6/mcc.1                   ","-P 6 2c             ",  &
                                               "x-y,x,z,1;x,y,-z,1;x,x-y,z+1/2,1;y,x,z+1/2,1                                                            ")
     Shubnikov_info(1477)= Shub_Spgr_Info_Type("192.244","P6/mcc1'    ","192.2.1477 ","P6/mcc1'      ","P6/mcc.1'                  ","-P 6 2c 1'          ",  &
                                               "x-y,x,z,1;x,y,-z,1;x,x-y,z+1/2,1;y,x,z+1/2,1;x,y,z,-1                                                   ")
     Shubnikov_info(1478)= Shub_Spgr_Info_Type("192.245","P6/m'cc     ","192.3.1478 ","P6/m'cc       ","P6/m'cc                    ","P 6 2c' -1'         ",  &
                                               "x-y,x,z,1;x,y,-z,-1;x,x-y,z+1/2,1;y,x,z+1/2,1                                                           ")
     Shubnikov_info(1479)= Shub_Spgr_Info_Type("192.246","P6'/mc'c    ","192.4.1479 ","P6'/mc'c      ","P6'/mc'c                   ","P 6' 2c' -1'        ",  &
                                               "x-y,x,z,-1;x,y,-z,1;x,x-y,z+1/2,-1;y,x,z+1/2,1                                                          ")
     Shubnikov_info(1480)= Shub_Spgr_Info_Type("192.247","P6'/mcc'    ","192.5.1480 ","P6'/mcc'      ","P6'/mcc'                   ","P 6' 2c -1'         ",  &
                                               "x-y,x,z,-1;x,y,-z,1;x,x-y,z+1/2,1;y,x,z+1/2,-1                                                          ")
     Shubnikov_info(1481)= Shub_Spgr_Info_Type("192.248","P6'/m'c'c   ","192.6.1481 ","P6'/m'c'c     ","P6'/m'c'c                  ","-P 6' 2c            ",  &
                                               "x-y,x,z,-1;x,y,-z,-1;x,x-y,z+1/2,-1;y,x,z+1/2,1                                                         ")
     Shubnikov_info(1482)= Shub_Spgr_Info_Type("192.249","P6'/m'cc'   ","192.7.1482 ","P6'/m'cc'     ","P6'/m'cc'                  ","-P 6' 2c'           ",  &
                                               "x-y,x,z,-1;x,y,-z,-1;x,x-y,z+1/2,1;y,x,z+1/2,-1                                                         ")
     Shubnikov_info(1483)= Shub_Spgr_Info_Type("192.250","P6/mc'c'    ","192.8.1483 ","P6/mc'c'      ","P6/mc'c'                   ","-P 6 2c'            ",  &
                                               "x-y,x,z,1;x,y,-z,1;x,x-y,z+1/2,-1;y,x,z+1/2,-1                                                          ")
     Shubnikov_info(1484)= Shub_Spgr_Info_Type("192.251","P6/m'c'c'   ","192.9.1484 ","P6/m'c'c'     ","P6/m'c'c'                  ","P 6 2c -1'          ",  &
                                               "x-y,x,z,1;x,y,-z,-1;x,x-y,z+1/2,-1;y,x,z+1/2,-1                                                         ")
     Shubnikov_info(1485)= Shub_Spgr_Info_Type("193.253","P6_3/mcm    ","193.1.1485 ","P6_3/mcm      ","P6_3/mcm.1                 ","-P 6c 2             ",  &
                                               "x-y,x,z+1/2,1;x,y,-z+1/2,1;x,x-y,z+1/2,1;y,x,z,1                                                        ")
     Shubnikov_info(1486)= Shub_Spgr_Info_Type("193.254","P6_3/mcm1'  ","193.2.1486 ","P6_3/mcm1'    ","P6_3/mcm.1'                ","-P 6c 2 1'          ",  &
                                               "x-y,x,z+1/2,1;x,y,-z+1/2,1;x,x-y,z+1/2,1;y,x,z,1;x,y,z,-1                                               ")
     Shubnikov_info(1487)= Shub_Spgr_Info_Type("193.255","P6_3/m'cm   ","193.3.1487 ","P6_3/m'cm     ","P6_3/m'cm                  ","P 6c 2' -1'         ",  &
                                               "x-y,x,z+1/2,1;x,y,-z+1/2,-1;x,x-y,z+1/2,1;y,x,z,1                                                       ")
     Shubnikov_info(1488)= Shub_Spgr_Info_Type("193.256","P6_3'/mc'm  ","193.4.1488 ","P6_3'/mc'm    ","P6_3'/mc'm                 ","P 6c' 2' -1'        ",  &
                                               "x-y,x,z+1/2,-1;x,y,-z+1/2,1;x,x-y,z+1/2,-1;y,x,z,1                                                      ")
     Shubnikov_info(1489)= Shub_Spgr_Info_Type("193.257","P6_3'/mcm'  ","193.5.1489 ","P6_3'/mcm'    ","P6_3'/mcm'                 ","P 6c' 2 -1'         ",  &
                                               "x-y,x,z+1/2,-1;x,y,-z+1/2,1;x,x-y,z+1/2,1;y,x,z,-1                                                      ")
     Shubnikov_info(1490)= Shub_Spgr_Info_Type("193.258","P6_3'/m'c'm ","193.6.1490 ","P6_3'/m'c'm   ","P6_3'/m'c'm                ","-P 6c' 2            ",  &
                                               "x-y,x,z+1/2,-1;x,y,-z+1/2,-1;x,x-y,z+1/2,-1;y,x,z,1                                                     ")
     Shubnikov_info(1491)= Shub_Spgr_Info_Type("193.259","P6_3'/m'cm' ","193.7.1491 ","P6_3'/m'cm'   ","P6_3'/m'cm'                ","-P 6c' 2'           ",  &
                                               "x-y,x,z+1/2,-1;x,y,-z+1/2,-1;x,x-y,z+1/2,1;y,x,z,-1                                                     ")
     Shubnikov_info(1492)= Shub_Spgr_Info_Type("193.260","P6_3/mc'm'  ","193.8.1492 ","P6_3/mc'm'    ","P6_3/mc'm'                 ","-P 6c 2'            ",  &
                                               "x-y,x,z+1/2,1;x,y,-z+1/2,1;x,x-y,z+1/2,-1;y,x,z,-1                                                      ")
     Shubnikov_info(1493)= Shub_Spgr_Info_Type("193.261","P6_3/m'c'm' ","193.9.1493 ","P6_3/m'c'm'   ","P6_3/m'c'm'                ","P 6c 2 -1'          ",  &
                                               "x-y,x,z+1/2,1;x,y,-z+1/2,-1;x,x-y,z+1/2,-1;y,x,z,-1                                                     ")
     Shubnikov_info(1494)= Shub_Spgr_Info_Type("194.263","P6_3/mmc    ","194.1.1494 ","P6_3/mmc      ","P6_3/mmc.1                 ","-P 6c 2c            ",  &
                                               "x-y,x,z+1/2,1;x,y,-z+1/2,1;x,x-y,z,1;y,x,z+1/2,1                                                        ")
     Shubnikov_info(1495)= Shub_Spgr_Info_Type("194.264","P6_3/mmc1'  ","194.2.1495 ","P6_3/mmc1'    ","P6_3/mmc.1'                ","-P 6c 2c 1'         ",  &
                                               "x-y,x,z+1/2,1;x,y,-z+1/2,1;x,x-y,z,1;y,x,z+1/2,1;x,y,z,-1                                               ")
     Shubnikov_info(1496)= Shub_Spgr_Info_Type("194.265","P6_3/m'mc   ","194.3.1496 ","P6_3/m'mc     ","P6_3/m'mc                  ","P 6c 2c' -1'        ",  &
                                               "x-y,x,z+1/2,1;x,y,-z+1/2,-1;x,x-y,z,1;y,x,z+1/2,1                                                       ")
     Shubnikov_info(1497)= Shub_Spgr_Info_Type("194.266","P6_3'/mm'c  ","194.4.1497 ","P6_3'/mm'c    ","P6_3'/mm'c                 ","P 6c' 2c' -1'       ",  &
                                               "x-y,x,z+1/2,-1;x,y,-z+1/2,1;x,x-y,z,-1;y,x,z+1/2,1                                                      ")
     Shubnikov_info(1498)= Shub_Spgr_Info_Type("194.267","P6_3'/mmc'  ","194.5.1498 ","P6_3'/mmc'    ","P6_3'/mmc'                 ","P 6c' 2c -1'        ",  &
                                               "x-y,x,z+1/2,-1;x,y,-z+1/2,1;x,x-y,z,1;y,x,z+1/2,-1                                                      ")
     Shubnikov_info(1499)= Shub_Spgr_Info_Type("194.268","P6_3'/m'm'c ","194.6.1499 ","P6_3'/m'm'c   ","P6_3'/m'm'c                ","-P 6c' 2c           ",  &
                                               "x-y,x,z+1/2,-1;x,y,-z+1/2,-1;x,x-y,z,-1;y,x,z+1/2,1                                                     ")
     Shubnikov_info(1500)= Shub_Spgr_Info_Type("194.269","P6_3'/m'mc' ","194.7.1500 ","P6_3'/m'mc'   ","P6_3'/m'mc'                ","-P 6c' 2c'          ",  &
                                               "x-y,x,z+1/2,-1;x,y,-z+1/2,-1;x,x-y,z,1;y,x,z+1/2,-1                                                     ")
     Shubnikov_info(1501)= Shub_Spgr_Info_Type("194.270","P6_3/mm'c'  ","194.8.1501 ","P6_3/mm'c'    ","P6_3/mm'c'                 ","-P 6c 2c'           ",  &
                                               "x-y,x,z+1/2,1;x,y,-z+1/2,1;x,x-y,z,-1;y,x,z+1/2,-1                                                      ")
     Shubnikov_info(1502)= Shub_Spgr_Info_Type("194.271","P6_3/m'm'c' ","194.9.1502 ","P6_3/m'm'c'   ","P6_3/m'm'c'                ","P 6c 2c -1'         ",  &
                                               "x-y,x,z+1/2,1;x,y,-z+1/2,-1;x,x-y,z,-1;y,x,z+1/2,-1                                                     ")
     Shubnikov_info(1503)= Shub_Spgr_Info_Type("195.1  ","P23         ","195.1.1503 ","P23           ","P23.1                      ","P 2 2 3             ",  &
                                               "-x,-y,z,1;z,x,y,1                                                                                       ")
     Shubnikov_info(1504)= Shub_Spgr_Info_Type("195.2  ","P231'       ","195.2.1504 ","P231'         ","P23.1'                     ","P 2 2 3 1'          ",  &
                                               "-x,-y,z,1;z,x,y,1;x,y,z,-1                                                                              ")
     Shubnikov_info(1505)= Shub_Spgr_Info_Type("196.6  ","F_S23       ","195.3.1505 ","P_F23         ","F23.1'_I[P23]              ","F 2 2 3 1'n         ",  &
                                               "x+1/2,y+1/2,z,1;-x,-y,z,1;z,x,y,1;x+1/2,y+1/2,z+1/2,-1                                                  ")
     Shubnikov_info(1506)= Shub_Spgr_Info_Type("196.4  ","F23         ","196.1.1506 ","F23           ","F23.1                      ","F 2 2 3             ",  &
                                               "x+1/2,y+1/2,z,1;-x,-y,z,1;z,x,y,1                                                                       ")
     Shubnikov_info(1507)= Shub_Spgr_Info_Type("196.5  ","F231'       ","196.2.1507 ","F231'         ","F23.1'                     ","F 2 2 3 1'          ",  &
                                               "x+1/2,y+1/2,z,1;-x,-y,z,1;z,x,y,1;x,y,z,-1                                                              ")
     Shubnikov_info(1508)= Shub_Spgr_Info_Type("197.7  ","I23         ","197.1.1508 ","I23           ","I23.1                      ","I 2 2 3             ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,-y,z,1;z,x,y,1                                                                   ")
     Shubnikov_info(1509)= Shub_Spgr_Info_Type("197.8  ","I231'       ","197.2.1509 ","I231'         ","I23.1'                     ","I 2 2 3 1'          ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,-y,z,1;z,x,y,1;x,y,z,-1                                                          ")
     Shubnikov_info(1510)= Shub_Spgr_Info_Type("195.3  ","P_I23       ","197.3.1510 ","I_P23         ","P23.1'_I[I23]              ","P 2 2 3 1'I         ",  &
                                               "-x,-y,z,1;z,x,y,1;x+1/2,y+1/2,z+1/2,-1                                                                  ")
     Shubnikov_info(1511)= Shub_Spgr_Info_Type("198.9  ","P2_13       ","198.1.1511 ","P2_13         ","P2_13.1                    ","P 2ac 2ab 3         ",  &
                                               "-x+1/2,-y,z+1/2,1;z,x,y,1                                                                               ")
     Shubnikov_info(1512)= Shub_Spgr_Info_Type("198.10 ","P2_131'     ","198.2.1512 ","P2_131'       ","P2_13.1'                   ","P 2ac 2ab 3 1'      ",  &
                                               "-x+1/2,-y,z+1/2,1;z,x,y,1;x,y,z,-1                                                                      ")
     Shubnikov_info(1513)= Shub_Spgr_Info_Type("199.12 ","I2_13       ","199.1.1513 ","I2_13         ","I2_13.1                    ","I 2b 2c 3           ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,-y+1/2,z,1;z,x,y,1                                                               ")
     Shubnikov_info(1514)= Shub_Spgr_Info_Type("199.13 ","I2_131'     ","199.2.1514 ","I2_131'       ","I2_13.1'                   ","I 2b 2c 3 1'        ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-x,-y+1/2,z,1;z,x,y,1;x,y,z,-1                                                      ")
     Shubnikov_info(1515)= Shub_Spgr_Info_Type("198.11 ","P_I2_13     ","199.3.1515 ","I_P2_13       ","P2_13.1'_I[I2_13]          ","P 2ac 2ab 3 1'I     ",  &
                                               "-x+1/2,-y,z+1/2,1;z,x,y,1;x+1/2,y+1/2,z+1/2,-1                                                          ")
     Shubnikov_info(1516)= Shub_Spgr_Info_Type("200.14 ","Pm-3        ","200.1.1516 ","Pm-3          ","Pm-3.1                     ","-P 2 2 3            ",  &
                                               "x,y,-z,1;-z,-x,-y,1                                                                                     ")
     Shubnikov_info(1517)= Shub_Spgr_Info_Type("200.15 ","Pm-31'      ","200.2.1517 ","Pm-31'        ","Pm-3.1'                    ","-P 2 2 3 1'         ",  &
                                               "x,y,-z,1;-z,-x,-y,1;x,y,z,-1                                                                            ")
     Shubnikov_info(1518)= Shub_Spgr_Info_Type("200.16 ","Pm'-3'      ","200.3.1518 ","Pm'-3'        ","Pm'-3'                     ","P 2 2 3 -1'         ",  &
                                               "x,y,-z,-1;-z,-x,-y,-1                                                                                   ")
     Shubnikov_info(1519)= Shub_Spgr_Info_Type("202.25 ","F_Sm-3      ","200.4.1519 ","P_Fm-3        ","Fm-3.1'_I[Pm-3]            ","-F 2 2 3 1'n        ",  &
                                               "x+1/2,y+1/2,z,1;x,y,-z,1;-z,-x,-y,1;x+1/2,y+1/2,z+1/2,-1                                                ")
     Shubnikov_info(1520)= Shub_Spgr_Info_Type("201.18 ","Pn-3        ","201.1.1520 ","Pn-3          ","Pn-3.1                     ","-P 2ab 2bc 3        ",  &
                                               "x+1/2,y+1/2,-z,1;-z,-x,-y,1                                                                             ")
     Shubnikov_info(1521)= Shub_Spgr_Info_Type("201.19 ","Pn-31'      ","201.2.1521 ","Pn-31'        ","Pn-3.1'                    ","-P 2ab 2bc 3 1'     ",  &
                                               "x+1/2,y+1/2,-z,1;-z,-x,-y,1;x,y,z,-1                                                                    ")
     Shubnikov_info(1522)= Shub_Spgr_Info_Type("201.20 ","Pn'-3'      ","201.3.1522 ","Pn'-3'        ","Pn'-3'                     ","P 2ab 2bc 3 -1'     ",  &
                                               "x+1/2,y+1/2,-z,-1;-z,-x,-y,-1                                                                           ")
     Shubnikov_info(1523)= Shub_Spgr_Info_Type("203.29 ","F_Sd-3      ","201.4.1523 ","P_Fn-3        ","Fd-3.1'_I[Pn-3]            ","-F 2uv 2vw 3 1'n    ",  &
                                               "x+1/2,y+1/2,z,1;x+1/4,y+1/4,-z,1;-z,-x,-y,1;x+1/2,y+1/2,z+1/2,-1                                        ")
     Shubnikov_info(1524)= Shub_Spgr_Info_Type("202.22 ","Fm-3        ","202.1.1524 ","Fm-3          ","Fm-3.1                     ","-F 2 2 3            ",  &
                                               "x+1/2,y+1/2,z,1;x,y,-z,1;-z,-x,-y,1                                                                     ")
     Shubnikov_info(1525)= Shub_Spgr_Info_Type("202.23 ","Fm-31'      ","202.2.1525 ","Fm-31'        ","Fm-3.1'                    ","-F 2 2 3 1'         ",  &
                                               "x+1/2,y+1/2,z,1;x,y,-z,1;-z,-x,-y,1;x,y,z,-1                                                            ")
     Shubnikov_info(1526)= Shub_Spgr_Info_Type("202.24 ","Fm'-3'      ","202.3.1526 ","Fm'-3'        ","Fm'-3'                     ","F 2 2 3 -1'         ",  &
                                               "x+1/2,y+1/2,z,1;x,y,-z,-1;-z,-x,-y,-1                                                                   ")
     Shubnikov_info(1527)= Shub_Spgr_Info_Type("203.26 ","Fd-3        ","203.1.1527 ","Fd-3          ","Fd-3.1                     ","-F 2uv 2vw 3        ",  &
                                               "x+1/2,y+1/2,z,1;x+1/4,y+1/4,-z,1;-z,-x,-y,1                                                             ")
     Shubnikov_info(1528)= Shub_Spgr_Info_Type("203.27 ","Fd-31'      ","203.2.1528 ","Fd-31'        ","Fd-3.1'                    ","-F 2uv 2vw 3 1'     ",  &
                                               "x+1/2,y+1/2,z,1;x+1/4,y+1/4,-z,1;-z,-x,-y,1;x,y,z,-1                                                    ")
     Shubnikov_info(1529)= Shub_Spgr_Info_Type("203.28 ","Fd'-3'      ","203.3.1529 ","Fd'-3'        ","Fd'-3'                     ","F 2uv 2vw 3 -1'     ",  &
                                               "x+1/2,y+1/2,z,1;x+1/4,y+1/4,-z,-1;-z,-x,-y,-1                                                           ")
     Shubnikov_info(1530)= Shub_Spgr_Info_Type("204.30 ","Im-3        ","204.1.1530 ","Im-3          ","Im-3.1                     ","-I 2 2 3            ",  &
                                               "x+1/2,y+1/2,z+1/2,1;x,y,-z,1;-z,-x,-y,1                                                                 ")
     Shubnikov_info(1531)= Shub_Spgr_Info_Type("204.31 ","Im-31'      ","204.2.1531 ","Im-31'        ","Im-3.1'                    ","-I 2 2 3 1'         ",  &
                                               "x+1/2,y+1/2,z+1/2,1;x,y,-z,1;-z,-x,-y,1;x,y,z,-1                                                        ")
     Shubnikov_info(1532)= Shub_Spgr_Info_Type("204.32 ","Im'-3'      ","204.3.1532 ","Im'-3'        ","Im'-3'                     ","I 2 2 3 -1'         ",  &
                                               "x+1/2,y+1/2,z+1/2,1;x,y,-z,-1;-z,-x,-y,-1                                                               ")
     Shubnikov_info(1533)= Shub_Spgr_Info_Type("200.17 ","P_Im-3      ","204.4.1533 ","I_Pm-3        ","Pm-3.1'_I[Im-3]            ","-P 2 2 3 1'I        ",  &
                                               "x,y,-z,1;-z,-x,-y,1;x+1/2,y+1/2,z+1/2,-1                                                                ")
     Shubnikov_info(1534)= Shub_Spgr_Info_Type("201.21 ","P_In-3      ","204.5.1534 ","I_Pm'-3'      ","Pn-3.1'_I[Im-3]            ","-P 2ab 2bc 3 1'I    ",  &
                                               "x+1/2,y+1/2,-z,1;-z,-x,-y,1;x+1/2,y+1/2,z+1/2,-1                                                        ")
     Shubnikov_info(1535)= Shub_Spgr_Info_Type("205.33 ","Pa-3        ","205.1.1535 ","Pa-3          ","Pa-3.1                     ","-P 2ac 2ab 3        ",  &
                                               "x+1/2,y,-z+1/2,1;-z,-x,-y,1                                                                             ")
     Shubnikov_info(1536)= Shub_Spgr_Info_Type("205.34 ","Pa-31'      ","205.2.1536 ","Pa-31'        ","Pa-3.1'                    ","-P 2ac 2ab 3 1'     ",  &
                                               "x+1/2,y,-z+1/2,1;-z,-x,-y,1;x,y,z,-1                                                                    ")
     Shubnikov_info(1537)= Shub_Spgr_Info_Type("205.35 ","Pa'-3'      ","205.3.1537 ","Pa'-3'        ","Pa'-3'                     ","P 2ac 2ab 3 -1'     ",  &
                                               "x+1/2,y,-z+1/2,-1;-z,-x,-y,-1                                                                           ")
     Shubnikov_info(1538)= Shub_Spgr_Info_Type("206.37 ","Ia-3        ","206.1.1538 ","Ia-3          ","Ia-3.1                     ","-I 2b 2c 3          ",  &
                                               "x+1/2,y+1/2,z+1/2,1;x,y+1/2,-z,1;-z,-x,-y,1                                                             ")
     Shubnikov_info(1539)= Shub_Spgr_Info_Type("206.38 ","Ia-31'      ","206.2.1539 ","Ia-31'        ","Ia-3.1'                    ","-I 2b 2c 3 1'       ",  &
                                               "x+1/2,y+1/2,z+1/2,1;x,y+1/2,-z,1;-z,-x,-y,1;x,y,z,-1                                                    ")
     Shubnikov_info(1540)= Shub_Spgr_Info_Type("206.39 ","Ia'-3'      ","206.3.1540 ","Ia'-3'        ","Ia'-3'                     ","I 2b 2c 3 -1'       ",  &
                                               "x+1/2,y+1/2,z+1/2,1;x,y+1/2,-z,-1;-z,-x,-y,-1                                                           ")
     Shubnikov_info(1541)= Shub_Spgr_Info_Type("205.36 ","P_Ia-3      ","206.4.1541 ","I_Pa-3'       ","Pa-3.1'_I[Ia-3]            ","-P 2ac 2ab 3 1'I    ",  &
                                               "x+1/2,y,-z+1/2,1;-z,-x,-y,1;x+1/2,y+1/2,z+1/2,-1                                                        ")
     Shubnikov_info(1542)= Shub_Spgr_Info_Type("207.40 ","P432        ","207.1.1542 ","P432          ","P432.1                     ","P 4 2 3             ",  &
                                               "-y,x,z,1;z,x,y,1;-y,-x,-z,1                                                                             ")
     Shubnikov_info(1543)= Shub_Spgr_Info_Type("207.41 ","P4321'      ","207.2.1543 ","P4321'        ","P432.1'                    ","P 4 2 3 1'          ",  &
                                               "-y,x,z,1;z,x,y,1;-y,-x,-z,1;x,y,z,-1                                                                    ")
     Shubnikov_info(1544)= Shub_Spgr_Info_Type("207.42 ","P4'32'      ","207.3.1544 ","P4'32'        ","P4'32'                     ","P 4' 2 3            ",  &
                                               "-y,x,z,-1;z,x,y,1;-y,-x,-z,-1                                                                           ")
     Shubnikov_info(1545)= Shub_Spgr_Info_Type("209.51 ","F_S432      ","207.4.1545 ","P_F432        ","F432.1'_I[P432]            ","F 4 2 3 1'n         ",  &
                                               "x+1/2,y+1/2,z,1;-y,x,z,1;z,x,y,1;-y,-x,-z,1;x+1/2,y+1/2,z+1/2,-1                                        ")
     Shubnikov_info(1546)= Shub_Spgr_Info_Type("208.44 ","P4_232      ","208.1.1546 ","P4_232        ","P4_232.1                   ","P 4n 2 3            ",  &
                                               "-y+1/2,x+1/2,z+1/2,1;z,x,y,1;-y+1/2,-x+1/2,-z+1/2,1                                                     ")
     Shubnikov_info(1547)= Shub_Spgr_Info_Type("208.45 ","P4_2321'    ","208.2.1547 ","P4_2321'      ","P4_232.1'                  ","P 4n 2 3 1'         ",  &
                                               "-y+1/2,x+1/2,z+1/2,1;z,x,y,1;-y+1/2,-x+1/2,-z+1/2,1;x,y,z,-1                                            ")
     Shubnikov_info(1548)= Shub_Spgr_Info_Type("208.46 ","P4_2'32'    ","208.3.1548 ","P4_2'32'      ","P4_2'32'                   ","P 4n' 2 3           ",  &
                                               "-y+1/2,x+1/2,z+1/2,-1;z,x,y,1;-y+1/2,-x+1/2,-z+1/2,-1                                                   ")
     Shubnikov_info(1549)= Shub_Spgr_Info_Type("210.55 ","F_S4_132    ","208.4.1549 ","P_F4_232      ","F4_132.1'_I[P4_232]        ","F 4d 2 3 1'n        ",  &
                                               "x+1/2,y+1/2,z,1;-y+1/4,x+1/4,z+1/4,1;z,x,y,1;-y+1/4,-x+1/4,-z+1/4,1;x+1/2,y+1/2,z+1/2,-1                ")
     Shubnikov_info(1550)= Shub_Spgr_Info_Type("209.48 ","F432        ","209.1.1550 ","F432          ","F432.1                     ","F 4 2 3             ",  &
                                               "x+1/2,y+1/2,z,1;-y,x,z,1;z,x,y,1;-y,-x,-z,1                                                             ")
     Shubnikov_info(1551)= Shub_Spgr_Info_Type("209.49 ","F4321'      ","209.2.1551 ","F4321'        ","F432.1'                    ","F 4 2 3 1'          ",  &
                                               "x+1/2,y+1/2,z,1;-y,x,z,1;z,x,y,1;-y,-x,-z,1;x,y,z,-1                                                    ")
     Shubnikov_info(1552)= Shub_Spgr_Info_Type("209.50 ","F4'32'      ","209.3.1552 ","F4'32'        ","F4'32'                     ","F 4' 2 3            ",  &
                                               "x+1/2,y+1/2,z,1;-y,x,z,-1;z,x,y,1;-y,-x,-z,-1                                                           ")
     Shubnikov_info(1553)= Shub_Spgr_Info_Type("210.52 ","F4_132      ","210.1.1553 ","F4_132        ","F4_132.1                   ","F 4d 2 3            ",  &
                                               "x+1/2,y+1/2,z,1;-y+1/4,x+1/4,z+1/4,1;z,x,y,1;-y+1/4,-x+1/4,-z+1/4,1                                     ")
     Shubnikov_info(1554)= Shub_Spgr_Info_Type("210.53 ","F4_1321'    ","210.2.1554 ","F4_1321'      ","F4_132.1'                  ","F 4d 2 3 1'         ",  &
                                               "x+1/2,y+1/2,z,1;-y+1/4,x+1/4,z+1/4,1;z,x,y,1;-y+1/4,-x+1/4,-z+1/4,1;x,y,z,-1                            ")
     Shubnikov_info(1555)= Shub_Spgr_Info_Type("210.54 ","F4_1'32'    ","210.3.1555 ","F4_1'32'      ","F4_1'32'                   ","F 4d' 2 3           ",  &
                                               "x+1/2,y+1/2,z,1;-y+1/4,x+1/4,z+1/4,-1;z,x,y,1;-y+1/4,-x+1/4,-z+1/4,-1                                   ")
     Shubnikov_info(1556)= Shub_Spgr_Info_Type("211.56 ","I432        ","211.1.1556 ","I432          ","I432.1                     ","I 4 2 3             ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,1;z,x,y,1;-y,-x,-z,1                                                         ")
     Shubnikov_info(1557)= Shub_Spgr_Info_Type("211.57 ","I4321'      ","211.2.1557 ","I4321'        ","I432.1'                    ","I 4 2 3 1'          ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,1;z,x,y,1;-y,-x,-z,1;x,y,z,-1                                                ")
     Shubnikov_info(1558)= Shub_Spgr_Info_Type("211.58 ","I4'32'      ","211.3.1558 ","I4'32'        ","I4'32'                     ","I 4' 2 3            ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y,x,z,-1;z,x,y,1;-y,-x,-z,-1                                                       ")
     Shubnikov_info(1559)= Shub_Spgr_Info_Type("207.43 ","P_I432      ","211.4.1559 ","I_P432        ","P432.1'_I[I432]            ","P 4 2 3 1'I         ",  &
                                               "-y,x,z,1;z,x,y,1;-y,-x,-z,1;x+1/2,y+1/2,z+1/2,-1                                                        ")
     Shubnikov_info(1560)= Shub_Spgr_Info_Type("208.47 ","P_I4_232    ","211.5.1560 ","I_P4'32'      ","P4_232.1'_I[I432]          ","P 4n 2 3 1'I        ",  &
                                               "-y+1/2,x+1/2,z+1/2,1;z,x,y,1;-y+1/2,-x+1/2,-z+1/2,1;x+1/2,y+1/2,z+1/2,-1                                ")
     Shubnikov_info(1561)= Shub_Spgr_Info_Type("212.59 ","P4_332      ","212.1.1561 ","P4_332        ","P4_332.1                   ","P 4acd 2ab 3        ",  &
                                               "-y+3/4,x+1/4,z+3/4,1;z,x,y,1;-y+1/4,-x+1/4,-z+1/4,1                                                     ")
     Shubnikov_info(1562)= Shub_Spgr_Info_Type("212.60 ","P4_3321'    ","212.2.1562 ","P4_3321'      ","P4_332.1'                  ","P 4acd 2ab 3 1'     ",  &
                                               "-y+3/4,x+1/4,z+3/4,1;z,x,y,1;-y+1/4,-x+1/4,-z+1/4,1;x,y,z,-1                                            ")
     Shubnikov_info(1563)= Shub_Spgr_Info_Type("212.61 ","P4_3'32'    ","212.3.1563 ","P4_3'32'      ","P4_3'32'                   ","P 4acd' 2ab 3       ",  &
                                               "-y+3/4,x+1/4,z+3/4,-1;z,x,y,1;-y+1/4,-x+1/4,-z+1/4,-1                                                   ")
     Shubnikov_info(1564)= Shub_Spgr_Info_Type("213.63 ","P4_132      ","213.1.1564 ","P4_132        ","P4_132.1                   ","P 4bd 2ab 3         ",  &
                                               "-y+1/4,x+3/4,z+1/4,1;z,x,y,1;-y+3/4,-x+3/4,-z+3/4,1                                                     ")
     Shubnikov_info(1565)= Shub_Spgr_Info_Type("213.64 ","P4_1321'    ","213.2.1565 ","P4_1321'      ","P4_132.1'                  ","P 4bd 2ab 3 1'      ",  &
                                               "-y+1/4,x+3/4,z+1/4,1;z,x,y,1;-y+3/4,-x+3/4,-z+3/4,1;x,y,z,-1                                            ")
     Shubnikov_info(1566)= Shub_Spgr_Info_Type("213.65 ","P4_1'32'    ","213.3.1566 ","P4_1'32'      ","P4_1'32'                   ","P 4bd' 2ab 3        ",  &
                                               "-y+1/4,x+3/4,z+1/4,-1;z,x,y,1;-y+3/4,-x+3/4,-z+3/4,-1                                                   ")
     Shubnikov_info(1567)= Shub_Spgr_Info_Type("214.67 ","I4_132      ","214.1.1567 ","I4_132        ","I4_132.1                   ","I 4bd 2c 3          ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y+1/4,x+3/4,z+1/4,1;z,x,y,1;-y+1/4,-x+1/4,-z+1/4,1                                 ")
     Shubnikov_info(1568)= Shub_Spgr_Info_Type("214.68 ","I4_1321'    ","214.2.1568 ","I4_1321'      ","I4_132.1'                  ","I 4bd 2c 3 1'       ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y+1/4,x+3/4,z+1/4,1;z,x,y,1;-y+1/4,-x+1/4,-z+1/4,1;x,y,z,-1                        ")
     Shubnikov_info(1569)= Shub_Spgr_Info_Type("214.69 ","I4_1'32'    ","214.3.1569 ","I4_1'32'      ","I4_1'32'                   ","I 4bd' 2c 3         ",  &
                                               "x+1/2,y+1/2,z+1/2,1;-y+1/4,x+3/4,z+1/4,-1;z,x,y,1;-y+1/4,-x+1/4,-z+1/4,-1                               ")
     Shubnikov_info(1570)= Shub_Spgr_Info_Type("212.62 ","P_I4_332    ","214.4.1570 ","I_P4_132      ","P4_332.1'_I[I4_132]        ","P 4acd 2ab 3 1'I    ",  &
                                               "-y+3/4,x+1/4,z+3/4,1;z,x,y,1;-y+1/4,-x+1/4,-z+1/4,1;x+1/2,y+1/2,z+1/2,-1                                ")
     Shubnikov_info(1571)= Shub_Spgr_Info_Type("213.66 ","P_I4_132    ","214.5.1571 ","I_P4_1'32'    ","P4_132.1'_I[I4_132]        ","P 4bd 2ab 3 1'I     ",  &
                                               "-y+1/4,x+3/4,z+1/4,1;z,x,y,1;-y+3/4,-x+3/4,-z+3/4,1;x+1/2,y+1/2,z+1/2,-1                                ")
     Shubnikov_info(1572)= Shub_Spgr_Info_Type("215.70 ","P-43m       ","215.1.1572 ","P-43m         ","P-43m.1                    ","P -4 2 3            ",  &
                                               "y,-x,-z,1;z,x,y,1;y,x,z,1                                                                               ")
     Shubnikov_info(1573)= Shub_Spgr_Info_Type("215.71 ","P-43m1'     ","215.2.1573 ","P-43m1'       ","P-43m.1'                   ","P -4 2 3 1'         ",  &
                                               "y,-x,-z,1;z,x,y,1;y,x,z,1;x,y,z,-1                                                                      ")
     Shubnikov_info(1574)= Shub_Spgr_Info_Type("215.72 ","P-4'3m'     ","215.3.1574 ","P-4'3m'       ","P-4'3m'                    ","P -4' 2 3           ",  &
                                               "y,-x,-z,-1;z,x,y,1;y,x,z,-1                                                                             ")
     Shubnikov_info(1575)= Shub_Spgr_Info_Type("216.77 ","F_S-43m     ","215.4.1575 ","P_F-43m       ","F-43m.1'_I[P-43m]          ","F -4 2 3 1'n        ",  &
                                               "x+1/2,y+1/2,z,1;y,-x,-z,1;z,x,y,1;y,x,z,1;x+1/2,y+1/2,z+1/2,-1                                          ")
     Shubnikov_info(1576)= Shub_Spgr_Info_Type("219.88 ","F_S-43c     ","215.5.1576 ","P_F-4'3m'     ","F-43c.1'_I[P-43m]          ","F -4c 2 3 1'n       ",  &
                                               "x+1/2,y+1/2,z,1;y+1/2,-x+1/2,-z+1/2,1;z,x,y,1;y+1/2,x+1/2,z+1/2,1;x+1/2,y+1/2,z+1/2,-1                  ")
     Shubnikov_info(1577)= Shub_Spgr_Info_Type("216.74 ","F-43m       ","216.1.1577 ","F-43m         ","F-43m.1                    ","F -4 2 3            ",  &
                                               "x+1/2,y+1/2,z,1;y,-x,-z,1;z,x,y,1;y,x,z,1                                                               ")
     Shubnikov_info(1578)= Shub_Spgr_Info_Type("216.75 ","F-43m1'     ","216.2.1578 ","F-43m1'       ","F-43m.1'                   ","F -4 2 3 1'         ",  &
                                               "x+1/2,y+1/2,z,1;y,-x,-z,1;z,x,y,1;y,x,z,1;x,y,z,-1                                                      ")
     Shubnikov_info(1579)= Shub_Spgr_Info_Type("216.76 ","F-4'3m'     ","216.3.1579 ","F-4'3m'       ","F-4'3m'                    ","F -4' 2 3           ",  &
                                               "x+1/2,y+1/2,z,1;y,-x,-z,-1;z,x,y,1;y,x,z,-1                                                             ")
     Shubnikov_info(1580)= Shub_Spgr_Info_Type("217.78 ","I-43m       ","217.1.1580 ","I-43m         ","I-43m.1                    ","I -4 2 3            ",  &
                                               "x+1/2,y+1/2,z+1/2,1;y,-x,-z,1;z,x,y,1;y,x,z,1                                                           ")
     Shubnikov_info(1581)= Shub_Spgr_Info_Type("217.79 ","I-43m1'     ","217.2.1581 ","I-43m1'       ","I-43m.1'                   ","I -4 2 3 1'         ",  &
                                               "x+1/2,y+1/2,z+1/2,1;y,-x,-z,1;z,x,y,1;y,x,z,1;x,y,z,-1                                                  ")
     Shubnikov_info(1582)= Shub_Spgr_Info_Type("217.80 ","I-4'3m'     ","217.3.1582 ","I-4'3m'       ","I-4'3m'                    ","I -4' 2 3           ",  &
                                               "x+1/2,y+1/2,z+1/2,1;y,-x,-z,-1;z,x,y,1;y,x,z,-1                                                         ")
     Shubnikov_info(1583)= Shub_Spgr_Info_Type("215.73 ","P_I-43m     ","217.4.1583 ","I_P-43m       ","P-43m.1'_I[I-43m]          ","P -4 2 3 1'I        ",  &
                                               "y,-x,-z,1;z,x,y,1;y,x,z,1;x+1/2,y+1/2,z+1/2,-1                                                          ")
     Shubnikov_info(1584)= Shub_Spgr_Info_Type("218.84 ","P_I-43n     ","217.5.1584 ","I_P-4'3m'     ","P-43n.1'_I[I-43m]          ","P -4n 2 3 1'I       ",  &
                                               "y+1/2,-x+1/2,-z+1/2,1;z,x,y,1;y+1/2,x+1/2,z+1/2,1;x+1/2,y+1/2,z+1/2,-1                                  ")
     Shubnikov_info(1585)= Shub_Spgr_Info_Type("218.81 ","P-43n       ","218.1.1585 ","P-43n         ","P-43n.1                    ","P -4n 2 3           ",  &
                                               "y+1/2,-x+1/2,-z+1/2,1;z,x,y,1;y+1/2,x+1/2,z+1/2,1                                                       ")
     Shubnikov_info(1586)= Shub_Spgr_Info_Type("218.82 ","P-43n1'     ","218.2.1586 ","P-43n1'       ","P-43n.1'                   ","P -4n 2 3 1'        ",  &
                                               "y+1/2,-x+1/2,-z+1/2,1;z,x,y,1;y+1/2,x+1/2,z+1/2,1;x,y,z,-1                                              ")
     Shubnikov_info(1587)= Shub_Spgr_Info_Type("218.83 ","P-4'3n'     ","218.3.1587 ","P-4'3n'       ","P-4'3n'                    ","P -4n' 2 3          ",  &
                                               "y+1/2,-x+1/2,-z+1/2,-1;z,x,y,1;y+1/2,x+1/2,z+1/2,-1                                                     ")
     Shubnikov_info(1588)= Shub_Spgr_Info_Type("219.85 ","F-43c       ","219.1.1588 ","F-43c         ","F-43c.1                    ","F -4c 2 3           ",  &
                                               "x+1/2,y+1/2,z,1;y+1/2,-x+1/2,-z+1/2,1;z,x,y,1;y+1/2,x+1/2,z+1/2,1                                       ")
     Shubnikov_info(1589)= Shub_Spgr_Info_Type("219.86 ","F-43c1'     ","219.2.1589 ","F-43c1'       ","F-43c.1'                   ","F -4c 2 3 1'        ",  &
                                               "x+1/2,y+1/2,z,1;y+1/2,-x+1/2,-z+1/2,1;z,x,y,1;y+1/2,x+1/2,z+1/2,1;x,y,z,-1                              ")
     Shubnikov_info(1590)= Shub_Spgr_Info_Type("219.87 ","F-4'3c'     ","219.3.1590 ","F-4'3c'       ","F-4'3c'                    ","F -4c' 2 3          ",  &
                                               "x+1/2,y+1/2,z,1;y+1/2,-x+1/2,-z+1/2,-1;z,x,y,1;y+1/2,x+1/2,z+1/2,-1                                     ")
     Shubnikov_info(1591)= Shub_Spgr_Info_Type("220.89 ","I-43d       ","220.1.1591 ","I-43d         ","I-43d.1                    ","I -4bd 2c 3         ",  &
                                               "x+1/2,y+1/2,z+1/2,1;y+1/4,-x+3/4,-z+1/4,1;z,x,y,1;y+1/4,x+1/4,z+1/4,1                                   ")
     Shubnikov_info(1592)= Shub_Spgr_Info_Type("220.90 ","I-43d1'     ","220.2.1592 ","I-43d1'       ","I-43d.1'                   ","I -4bd 2c 3 1'      ",  &
                                               "x+1/2,y+1/2,z+1/2,1;y+1/4,-x+3/4,-z+1/4,1;z,x,y,1;y+1/4,x+1/4,z+1/4,1;x,y,z,-1                          ")
     Shubnikov_info(1593)= Shub_Spgr_Info_Type("220.91 ","I-4'3d'     ","220.3.1593 ","I-4'3d'       ","I-4'3d'                    ","I -4bd' 2c 3        ",  &
                                               "x+1/2,y+1/2,z+1/2,1;y+1/4,-x+3/4,-z+1/4,-1;z,x,y,1;y+1/4,x+1/4,z+1/4,-1                                 ")
     Shubnikov_info(1594)= Shub_Spgr_Info_Type("221.92 ","Pm-3m       ","221.1.1594 ","Pm-3m         ","Pm-3m.1                    ","-P 4 2 3            ",  &
                                               "x,y,-z,1;-z,-x,-y,1;y,x,z,1                                                                             ")
     Shubnikov_info(1595)= Shub_Spgr_Info_Type("221.93 ","Pm-3m1'     ","221.2.1595 ","Pm-3m1'       ","Pm-3m.1'                   ","-P 4 2 3 1'         ",  &
                                               "x,y,-z,1;-z,-x,-y,1;y,x,z,1;x,y,z,-1                                                                    ")
     Shubnikov_info(1596)= Shub_Spgr_Info_Type("221.94 ","Pm'-3'm     ","221.3.1596 ","Pm'-3'm       ","Pm'-3'm                    ","P 4' 2 3 -1'        ",  &
                                               "x,y,-z,-1;-z,-x,-y,-1;y,x,z,1                                                                           ")
     Shubnikov_info(1597)= Shub_Spgr_Info_Type("221.95 ","Pm-3m'      ","221.4.1597 ","Pm-3m'        ","Pm-3m'                     ","-P 4' 2 3           ",  &
                                               "x,y,-z,1;-z,-x,-y,1;y,x,z,-1                                                                            ")
     Shubnikov_info(1598)= Shub_Spgr_Info_Type("221.96 ","Pm'-3'm'    ","221.5.1598 ","Pm'-3'm'      ","Pm'-3'm'                   ","P 4 2 3 -1'         ",  &
                                               "x,y,-z,-1;-z,-x,-y,-1;y,x,z,-1                                                                          ")
     Shubnikov_info(1599)= Shub_Spgr_Info_Type("225.121","F_Sm-3m     ","221.6.1599 ","P_Fm-3m       ","Fm-3m.1'_I[Pm-3m]          ","-F 4 2 3 1'n        ",  &
                                               "x+1/2,y+1/2,z,1;x,y,-z,1;-z,-x,-y,1;y,x,z,1;x+1/2,y+1/2,z+1/2,-1                                        ")
     Shubnikov_info(1600)= Shub_Spgr_Info_Type("226.127","F_Sm-3c     ","221.7.1600 ","P_Fm-3m'      ","Fm-3c.1'_I[Pm-3m]          ","-F 4c 2 3 1'n       ",  &
                                               "x+1/2,y+1/2,z,1;x,y,-z,1;-z,-x,-y,1;y+1/2,x+1/2,z+1/2,1;x+1/2,y+1/2,z+1/2,-1                            ")
     Shubnikov_info(1601)= Shub_Spgr_Info_Type("222.98 ","Pn-3n       ","222.1.1601 ","Pn-3n         ","Pn-3n.1                    ","-P 4a 2bc 3         ",  &
                                               "x+1/2,y+1/2,-z,1;-z,-x,-y,1;y+1/2,x+1/2,z+1/2,1                                                         ")
     Shubnikov_info(1602)= Shub_Spgr_Info_Type("222.99 ","Pn-3n1'     ","222.2.1602 ","Pn-3n1'       ","Pn-3n.1'                   ","-P 4a 2bc 3 1'      ",  &
                                               "x+1/2,y+1/2,-z,1;-z,-x,-y,1;y+1/2,x+1/2,z+1/2,1;x,y,z,-1                                                ")
     Shubnikov_info(1603)= Shub_Spgr_Info_Type("222.100","Pn'-3'n     ","222.3.1603 ","Pn'-3'n       ","Pn'-3'n                    ","P 4a' 2bc 3 -1'     ",  &
                                               "x+1/2,y+1/2,-z,-1;-z,-x,-y,-1;y+1/2,x+1/2,z+1/2,1                                                       ")
     Shubnikov_info(1604)= Shub_Spgr_Info_Type("222.101","Pn-3n'      ","222.4.1604 ","Pn-3n'        ","Pn-3n'                     ","-P 4a' 2bc 3        ",  &
                                               "x+1/2,y+1/2,-z,1;-z,-x,-y,1;y+1/2,x+1/2,z+1/2,-1                                                        ")
     Shubnikov_info(1605)= Shub_Spgr_Info_Type("222.102","Pn'-3'n'    ","222.5.1605 ","Pn'-3'n'      ","Pn'-3'n'                   ","P 4a 2bc 3 -1'      ",  &
                                               "x+1/2,y+1/2,-z,-1;-z,-x,-y,-1;y+1/2,x+1/2,z+1/2,-1                                                      ")
     Shubnikov_info(1606)= Shub_Spgr_Info_Type("223.104","Pm-3n       ","223.1.1606 ","Pm-3n         ","Pm-3n.1                    ","-P 4n 2 3           ",  &
                                               "x,y,-z,1;-z,-x,-y,1;y+1/2,x+1/2,z+1/2,1                                                                 ")
     Shubnikov_info(1607)= Shub_Spgr_Info_Type("223.105","Pm-3n1'     ","223.2.1607 ","Pm-3n1'       ","Pm-3n.1'                   ","-P 4n 2 3 1'        ",  &
                                               "x,y,-z,1;-z,-x,-y,1;y+1/2,x+1/2,z+1/2,1;x,y,z,-1                                                        ")
     Shubnikov_info(1608)= Shub_Spgr_Info_Type("223.106","Pm'-3'n     ","223.3.1608 ","Pm'-3'n       ","Pm'-3'n                    ","P 4n' 2 3 -1'       ",  &
                                               "x,y,-z,-1;-z,-x,-y,-1;y+1/2,x+1/2,z+1/2,1                                                               ")
     Shubnikov_info(1609)= Shub_Spgr_Info_Type("223.107","Pm-3n'      ","223.4.1609 ","Pm-3n'        ","Pm-3n'                     ","-P 4n' 2 3          ",  &
                                               "x,y,-z,1;-z,-x,-y,1;y+1/2,x+1/2,z+1/2,-1                                                                ")
     Shubnikov_info(1610)= Shub_Spgr_Info_Type("223.108","Pm'-3'n'    ","223.5.1610 ","Pm'-3'n'      ","Pm'-3'n'                   ","P 4n 2 3 -1'        ",  &
                                               "x,y,-z,-1;-z,-x,-y,-1;y+1/2,x+1/2,z+1/2,-1                                                              ")
     Shubnikov_info(1611)= Shub_Spgr_Info_Type("224.110","Pn-3m       ","224.1.1611 ","Pn-3m         ","Pn-3m.1                    ","-P 4bc 2bc 3        ",  &
                                               "x+1/2,y+1/2,-z,1;-z,-x,-y,1;y,x,z,1                                                                     ")
     Shubnikov_info(1612)= Shub_Spgr_Info_Type("224.111","Pn-3m1'     ","224.2.1612 ","Pn-3m1'       ","Pn-3m.1'                   ","-P 4bc 2bc 3 1'     ",  &
                                               "x+1/2,y+1/2,-z,1;-z,-x,-y,1;y,x,z,1;x,y,z,-1                                                            ")
     Shubnikov_info(1613)= Shub_Spgr_Info_Type("224.112","Pn'-3'm     ","224.3.1613 ","Pn'-3'm       ","Pn'-3'm                    ","P 4bc' 2bc 3 -1'    ",  &
                                               "x+1/2,y+1/2,-z,-1;-z,-x,-y,-1;y,x,z,1                                                                   ")
     Shubnikov_info(1614)= Shub_Spgr_Info_Type("224.113","Pn-3m'      ","224.4.1614 ","Pn-3m'        ","Pn-3m'                     ","-P 4bc' 2bc 3       ",  &
                                               "x+1/2,y+1/2,-z,1;-z,-x,-y,1;y,x,z,-1                                                                    ")
     Shubnikov_info(1615)= Shub_Spgr_Info_Type("224.114","Pn'-3'm'    ","224.5.1615 ","Pn'-3'm'      ","Pn'-3'm'                   ","P 4bc 2bc 3 -1'     ",  &
                                               "x+1/2,y+1/2,-z,-1;-z,-x,-y,-1;y,x,z,-1                                                                  ")
     Shubnikov_info(1616)= Shub_Spgr_Info_Type("227.133","F_Sd-3m     ","224.6.1616 ","P_Fn-3m       ","Fd-3m.1'_I[Pn-3m]          ","-F 4vw 2vw 3 1'n    ",  &
                                               "x+1/2,y+1/2,z,1;x+1/4,y+1/4,-z,1;-z,-x,-y,1;y,x,z,1;x+1/2,y+1/2,z+1/2,-1                                ")
     Shubnikov_info(1617)= Shub_Spgr_Info_Type("228.139","F_Sd-3c     ","224.7.1617 ","P_Fn-3m'      ","Fd-3c.1'_I[Pn-3m]          ","-F 4cvw 2vw 3 1'n   ",  &
                                               "x+1/2,y+1/2,z,1;x+1/4,y+1/4,-z,1;-z,-x,-y,1;y+1/2,x+1/2,z+1/2,1;x+1/2,y+1/2,z+1/2,-1                    ")
     Shubnikov_info(1618)= Shub_Spgr_Info_Type("225.116","Fm-3m       ","225.1.1618 ","Fm-3m         ","Fm-3m.1                    ","-F 4 2 3            ",  &
                                               "x+1/2,y+1/2,z,1;x,y,-z,1;-z,-x,-y,1;y,x,z,1                                                             ")
     Shubnikov_info(1619)= Shub_Spgr_Info_Type("225.117","Fm-3m1'     ","225.2.1619 ","Fm-3m1'       ","Fm-3m.1'                   ","-F 4 2 3 1'         ",  &
                                               "x+1/2,y+1/2,z,1;x,y,-z,1;-z,-x,-y,1;y,x,z,1;x,y,z,-1                                                    ")
     Shubnikov_info(1620)= Shub_Spgr_Info_Type("225.118","Fm'-3'm     ","225.3.1620 ","Fm'-3'm       ","Fm'-3'm                    ","F 4' 2 3 -1'        ",  &
                                               "x+1/2,y+1/2,z,1;x,y,-z,-1;-z,-x,-y,-1;y,x,z,1                                                           ")
     Shubnikov_info(1621)= Shub_Spgr_Info_Type("225.119","Fm-3m'      ","225.4.1621 ","Fm-3m'        ","Fm-3m'                     ","-F 4' 2 3           ",  &
                                               "x+1/2,y+1/2,z,1;x,y,-z,1;-z,-x,-y,1;y,x,z,-1                                                            ")
     Shubnikov_info(1622)= Shub_Spgr_Info_Type("225.120","Fm'-3'm'    ","225.5.1622 ","Fm'-3'm'      ","Fm'-3'm'                   ","F 4 2 3 -1'         ",  &
                                               "x+1/2,y+1/2,z,1;x,y,-z,-1;-z,-x,-y,-1;y,x,z,-1                                                          ")
     Shubnikov_info(1623)= Shub_Spgr_Info_Type("226.122","Fm-3c       ","226.1.1623 ","Fm-3c         ","Fm-3c.1                    ","-F 4c 2 3           ",  &
                                               "x+1/2,y+1/2,z,1;x,y,-z,1;-z,-x,-y,1;y+1/2,x+1/2,z+1/2,1                                                 ")
     Shubnikov_info(1624)= Shub_Spgr_Info_Type("226.123","Fm-3c1'     ","226.2.1624 ","Fm-3c1'       ","Fm-3c.1'                   ","-F 4c 2 3 1'        ",  &
                                               "x+1/2,y+1/2,z,1;x,y,-z,1;-z,-x,-y,1;y+1/2,x+1/2,z+1/2,1;x,y,z,-1                                        ")
     Shubnikov_info(1625)= Shub_Spgr_Info_Type("226.124","Fm'-3'c     ","226.3.1625 ","Fm'-3'c       ","Fm'-3'c                    ","F 4c' 2 3 -1'       ",  &
                                               "x+1/2,y+1/2,z,1;x,y,-z,-1;-z,-x,-y,-1;y+1/2,x+1/2,z+1/2,1                                               ")
     Shubnikov_info(1626)= Shub_Spgr_Info_Type("226.125","Fm-3c'      ","226.4.1626 ","Fm-3c'        ","Fm-3c'                     ","-F 4c' 2 3          ",  &
                                               "x+1/2,y+1/2,z,1;x,y,-z,1;-z,-x,-y,1;y+1/2,x+1/2,z+1/2,-1                                                ")
     Shubnikov_info(1627)= Shub_Spgr_Info_Type("226.126","Fm'-3'c'    ","226.5.1627 ","Fm'-3'c'      ","Fm'-3'c'                   ","F 4c 2 3 -1'        ",  &
                                               "x+1/2,y+1/2,z,1;x,y,-z,-1;-z,-x,-y,-1;y+1/2,x+1/2,z+1/2,-1                                              ")
     Shubnikov_info(1628)= Shub_Spgr_Info_Type("227.128","Fd-3m       ","227.1.1628 ","Fd-3m         ","Fd-3m.1                    ","-F 4vw 2vw 3        ",  &
                                               "x+1/2,y+1/2,z,1;x+1/4,y+1/4,-z,1;-z,-x,-y,1;y,x,z,1                                                     ")
     Shubnikov_info(1629)= Shub_Spgr_Info_Type("227.129","Fd-3m1'     ","227.2.1629 ","Fd-3m1'       ","Fd-3m.1'                   ","-F 4vw 2vw 3 1'     ",  &
                                               "x+1/2,y+1/2,z,1;x+1/4,y+1/4,-z,1;-z,-x,-y,1;y,x,z,1;x,y,z,-1                                            ")
     Shubnikov_info(1630)= Shub_Spgr_Info_Type("227.130","Fd'-3'm     ","227.3.1630 ","Fd'-3'm       ","Fd'-3'm                    ","F 4vw' 2vw 3 -1'    ",  &
                                               "x+1/2,y+1/2,z,1;x+1/4,y+1/4,-z,-1;-z,-x,-y,-1;y,x,z,1                                                   ")
     Shubnikov_info(1631)= Shub_Spgr_Info_Type("227.131","Fd-3m'      ","227.4.1631 ","Fd-3m'        ","Fd-3m'                     ","-F 4vw' 2vw 3       ",  &
                                               "x+1/2,y+1/2,z,1;x+1/4,y+1/4,-z,1;-z,-x,-y,1;y,x,z,-1                                                    ")
     Shubnikov_info(1632)= Shub_Spgr_Info_Type("227.132","Fd'-3'm'    ","227.5.1632 ","Fd'-3'm'      ","Fd'-3'm'                   ","F 4vw 2vw 3 -1'     ",  &
                                               "x+1/2,y+1/2,z,1;x+1/4,y+1/4,-z,-1;-z,-x,-y,-1;y,x,z,-1                                                  ")
     Shubnikov_info(1633)= Shub_Spgr_Info_Type("228.134","Fd-3c       ","228.1.1633 ","Fd-3c         ","Fd-3c.1                    ","-F 4cvw 2vw 3       ",  &
                                               "x+1/2,y+1/2,z,1;x+1/4,y+1/4,-z,1;-z,-x,-y,1;y+1/2,x+1/2,z+1/2,1                                         ")
     Shubnikov_info(1634)= Shub_Spgr_Info_Type("228.135","Fd-3c1'     ","228.2.1634 ","Fd-3c1'       ","Fd-3c.1'                   ","-F 4cvw 2vw 3 1'    ",  &
                                               "x+1/2,y+1/2,z,1;x+1/4,y+1/4,-z,1;-z,-x,-y,1;y+1/2,x+1/2,z+1/2,1;x,y,z,-1                                ")
     Shubnikov_info(1635)= Shub_Spgr_Info_Type("228.136","Fd'-3'c     ","228.3.1635 ","Fd'-3'c       ","Fd'-3'c                    ","F 4cvw' 2vw 3 -1'   ",  &
                                               "x+1/2,y+1/2,z,1;x+1/4,y+1/4,-z,-1;-z,-x,-y,-1;y+1/2,x+1/2,z+1/2,1                                       ")
     Shubnikov_info(1636)= Shub_Spgr_Info_Type("228.137","Fd-3c'      ","228.4.1636 ","Fd-3c'        ","Fd-3c'                     ","-F 4cvw' 2vw 3      ",  &
                                               "x+1/2,y+1/2,z,1;x+1/4,y+1/4,-z,1;-z,-x,-y,1;y+1/2,x+1/2,z+1/2,-1                                        ")
     Shubnikov_info(1637)= Shub_Spgr_Info_Type("228.138","Fd'-3'c'    ","228.5.1637 ","Fd'-3'c'      ","Fd'-3'c'                   ","F 4cvw 2vw 3 -1'    ",  &
                                               "x+1/2,y+1/2,z,1;x+1/4,y+1/4,-z,-1;-z,-x,-y,-1;y+1/2,x+1/2,z+1/2,-1                                      ")
     Shubnikov_info(1638)= Shub_Spgr_Info_Type("229.140","Im-3m       ","229.1.1638 ","Im-3m         ","Im-3m.1                    ","-I 4 2 3            ",  &
                                               "x+1/2,y+1/2,z+1/2,1;x,y,-z,1;-z,-x,-y,1;y,x,z,1                                                         ")
     Shubnikov_info(1639)= Shub_Spgr_Info_Type("229.141","Im-3m1'     ","229.2.1639 ","Im-3m1'       ","Im-3m.1'                   ","-I 4 2 3 1'         ",  &
                                               "x+1/2,y+1/2,z+1/2,1;x,y,-z,1;-z,-x,-y,1;y,x,z,1;x,y,z,-1                                                ")
     Shubnikov_info(1640)= Shub_Spgr_Info_Type("229.142","Im'-3'm     ","229.3.1640 ","Im'-3'm       ","Im'-3'm                    ","I 4' 2 3 -1'        ",  &
                                               "x+1/2,y+1/2,z+1/2,1;x,y,-z,-1;-z,-x,-y,-1;y,x,z,1                                                       ")
     Shubnikov_info(1641)= Shub_Spgr_Info_Type("229.143","Im-3m'      ","229.4.1641 ","Im-3m'        ","Im-3m'                     ","-I 4' 2 3           ",  &
                                               "x+1/2,y+1/2,z+1/2,1;x,y,-z,1;-z,-x,-y,1;y,x,z,-1                                                        ")
     Shubnikov_info(1642)= Shub_Spgr_Info_Type("229.144","Im'-3'm'    ","229.5.1642 ","Im'-3'm'      ","Im'-3'm'                   ","I 4 2 3 -1'         ",  &
                                               "x+1/2,y+1/2,z+1/2,1;x,y,-z,-1;-z,-x,-y,-1;y,x,z,-1                                                      ")
     Shubnikov_info(1643)= Shub_Spgr_Info_Type("221.97 ","P_Im-3m     ","229.6.1643 ","I_Pm-3m       ","Pm-3m.1'_I[Im-3m]          ","-P 4 2 3 1'I        ",  &
                                               "x,y,-z,1;-z,-x,-y,1;y,x,z,1;x+1/2,y+1/2,z+1/2,-1                                                        ")
     Shubnikov_info(1644)= Shub_Spgr_Info_Type("224.115","P_In-3m     ","229.7.1644 ","I_Pm'-3'm     ","Pn-3m.1'_I[Im-3m]          ","-P 4bc 2bc 3 1'I    ",  &
                                               "x+1/2,y+1/2,-z,1;-z,-x,-y,1;y,x,z,1;x+1/2,y+1/2,z+1/2,-1                                                ")
     Shubnikov_info(1645)= Shub_Spgr_Info_Type("223.109","P_Im-3n     ","229.8.1645 ","I_Pm-3m'      ","Pm-3n.1'_I[Im-3m]          ","-P 4n 2 3 1'I       ",  &
                                               "x,y,-z,1;-z,-x,-y,1;y+1/2,x+1/2,z+1/2,1;x+1/2,y+1/2,z+1/2,-1                                            ")
     Shubnikov_info(1646)= Shub_Spgr_Info_Type("222.103","P_In-3n     ","229.9.1646 ","I_Pm'-3'm'    ","Pn-3n.1'_I[Im-3m]          ","-P 4a 2bc 3 1'I     ",  &
                                               "x+1/2,y+1/2,-z,1;-z,-x,-y,1;y+1/2,x+1/2,z+1/2,1;x+1/2,y+1/2,z+1/2,-1                                    ")
     Shubnikov_info(1647)= Shub_Spgr_Info_Type("230.145","Ia-3d       ","230.1.1647 ","Ia-3d         ","Ia-3d.1                    ","-I 4bd 2c 3         ",  &
                                               "x+1/2,y+1/2,z+1/2,1;x,y+1/2,-z,1;-z,-x,-y,1;y+1/4,x+1/4,z+1/4,1                                         ")
     Shubnikov_info(1648)= Shub_Spgr_Info_Type("230.146","Ia-3d1'     ","230.2.1648 ","Ia-3d1'       ","Ia-3d.1'                   ","-I 4bd 2c 3 1'      ",  &
                                               "x+1/2,y+1/2,z+1/2,1;x,y+1/2,-z,1;-z,-x,-y,1;y+1/4,x+1/4,z+1/4,1;x,y,z,-1                                ")
     Shubnikov_info(1649)= Shub_Spgr_Info_Type("230.147","Ia'-3'd     ","230.3.1649 ","Ia'-3'd       ","Ia'-3'd                    ","I 4bd' 2c 3 -1'     ",  &
                                               "x+1/2,y+1/2,z+1/2,1;x,y+1/2,-z,-1;-z,-x,-y,-1;y+1/4,x+1/4,z+1/4,1                                       ")
     Shubnikov_info(1650)= Shub_Spgr_Info_Type("230.148","Ia-3d'      ","230.4.1650 ","Ia-3d'        ","Ia-3d'                     ","-I 4bd' 2c 3        ",  &
                                               "x+1/2,y+1/2,z+1/2,1;x,y+1/2,-z,1;-z,-x,-y,1;y+1/4,x+1/4,z+1/4,-1                                        ")
     Shubnikov_info(1651)= Shub_Spgr_Info_Type("230.149","Ia'-3'd'    ","230.5.1651 ","Ia'-3'd'      ","Ia'-3'd'                   ","I 4bd 2c 3 -1'      ",  &
                                               "x+1/2,y+1/2,z+1/2,1;x,y+1/2,-z,-1;-z,-x,-y,-1;y+1/4,x+1/4,z+1/4,-1                                      ")
     Shubnikov_Info_loaded=.true.

  End Subroutine Set_Shubnikov_Info

   !!----
   !!---- SET_SPGR_INFO
   !!----    Number of the Space Group
   !!----    Hermann-Mauguin Symbol
   !!----    Hall symbol
   !!----    Laue Group                                                                                                 ----
   !!----    Point Group
   !!----    Asymmetric unit in direct space.
   !!----    Miscellaneous Information depending on crystal system:
   !!----        Monoclinic         b           c           a
   !!----                        abc  c-ba   abc  ba-c   abc -acb
   !!----                        ---------   ---------   --------
   !!----        cell choice 1    b1   -b1    c1   -c1    a1  -a1
   !!----        cell choice 2    b2   -b2    c2   -c2    a2  -a2
   !!----        cell choice 3    b3   -b3    c3   -c3    a3  -a3
   !!----        Orthorhombic     ba-c   change of basis abc -> ba-c
   !!----                         1      origin choice 1
   !!----                         2ba-c  origin choice 2, change basis
   !!----                                abc -> ba-c
   !!----        Tetragonal       1      origin choice 1
   !!----        Cubic            2      origin choice 2
   !!----        Trigonal         H      hexagonal axes
   !!----                         R      rhombohedral axes
   !!----
   !!----    Set Information on Spgr_info array
   !!----
   !!---- 23/04/2019
   !!
   Module Subroutine Set_Spgr_Info()
      if(Spgr_Info_loaded) return
      if (.not. allocated(spgr_info) ) allocate(spgr_info(NUM_SPGR_INFO) )

      !---- Triclinic ----!
      spgr_info(1:14)= [                                           &
           spgr_info_type(  1,"P 1         ","P 1             ", 1, 1, [ 0, 0, 0, 24, 24, 24],"     ") , &
           spgr_info_type(  1,"A 1         ","A 1             ", 1, 1, [ 0, 0, 0, 24, 24, 24],"     ") , &
           spgr_info_type(  1,"B 1         ","B 1             ", 1, 1, [ 0, 0, 0, 24, 24, 24],"     ") , &
           spgr_info_type(  1,"C 1         ","C 1             ", 1, 1, [ 0, 0, 0, 24, 24, 24],"     ") , &
           spgr_info_type(  1,"I 1         ","I 1             ", 1, 1, [ 0, 0, 0, 24, 24, 24],"     ") , &
           spgr_info_type(  1,"R 1         ","R 1             ", 1, 1, [ 0, 0, 0, 24, 24, 24],"     ") , &
           spgr_info_type(  1,"F 1         ","F 1             ", 1, 1, [ 0, 0, 0, 24, 24, 24],"     ") , &
           spgr_info_type(  2,"P -1        ","-P 1            ", 1, 2, [ 0, 0, 0, 12, 24, 24],"     ") , &
           spgr_info_type(  2,"A -1        ","-A 1            ", 1, 2, [ 0, 0, 0, 12, 24, 24],"     ") , &
           spgr_info_type(  2,"B -1        ","-B 1            ", 1, 2, [ 0, 0, 0, 12, 24, 24],"     ") , &
           spgr_info_type(  2,"C -1        ","-C 1            ", 1, 2, [ 0, 0, 0, 12, 24, 24],"     ") , &
           spgr_info_type(  2,"I -1        ","-I 1            ", 1, 2, [ 0, 0, 0, 12, 24, 24],"     ") , &
           spgr_info_type(  2,"R -1        ","-R 1            ", 1, 2, [ 0, 0, 0, 12, 24, 24],"     ") , &
           spgr_info_type(  2,"F -1        ","-F 1            ", 1, 2, [ 0, 0, 0, 12, 24, 24],"     ") ]

      !---- Monoclinic ----!
      spgr_info(15:44)= [                                           &
           spgr_info_type(  3,"P 1 2 1     ","P 2y            ", 2, 3, [ 0, 0, 0, 24, 24, 12],"b    ") , &
           spgr_info_type(  3,"P 2         ","P 2y            ", 2, 3, [ 0, 0, 0, 24, 24, 12],"b    ") , &
           spgr_info_type(  3,"P 1 1 2     ","P 2             ", 2, 3, [ 0, 0, 0, 12, 24, 24],"c    ") , &
           spgr_info_type(  3,"P 2 1 1     ","P 2x            ", 2, 3, [ 0, 0, 0, 24, 12, 24],"a    ") , &
           spgr_info_type(  4,"P 1 21 1    ","P 2yb           ", 2, 3, [ 0, 0, 0, 24, 24, 12],"b    ") , &
           spgr_info_type(  4,"P 21        ","P 2yb           ", 2, 3, [ 0, 0, 0, 24, 24, 12],"b    ") , &
           spgr_info_type(  4,"P 1 1 21    ","P 2c            ", 2, 3, [ 0, 0, 0, 12, 24, 24],"c    ") , &
           spgr_info_type(  4,"P 21 1 1    ","P 2xa           ", 2, 3, [ 0, 0, 0, 24, 12, 24],"a    ") , &
           spgr_info_type(  5,"C 1 2 1     ","C 2y            ", 2, 3, [ 0, 0, 0, 12, 12, 24],"b1   ") , &
           spgr_info_type(  5,"C 2         ","C 2y            ", 2, 3, [ 0, 0, 0, 12, 12, 24],"b1   ") , &
           spgr_info_type(  5,"A 1 2 1     ","A 2y            ", 2, 3, [ 0, 0, 0, 12, 12, 24],"b2   ") , &
           spgr_info_type(  5,"A 2         ","A 2y            ", 2, 3, [ 0, 0, 0, 12, 12, 24],"b2   ") , &
           spgr_info_type(  5,"I 1 2 1     ","I 2y            ", 2, 3, [ 0, 0, 0, 12, 12, 24],"b2   ") , &
           spgr_info_type(  5,"I 2         ","I 2y            ", 2, 3, [ 0, 0, 0, 12, 12, 24],"b2   ") , &
           spgr_info_type(  5,"A 1 1 2     ","A 2             ", 2, 3, [ 0, 0, 0, 24, 12, 12],"c1   ") , &
           spgr_info_type(  5,"B 1 1 2     ","B 2             ", 2, 3, [ 0, 0, 0, 24, 12, 12],"c2   ") , &
           spgr_info_type(  5,"I 1 1 2     ","I 2             ", 2, 3, [ 0, 0, 0, 24, 12, 12],"c3   ") , &
           spgr_info_type(  5,"B 2 1 1     ","B 2x            ", 2, 3, [ 0, 0, 0, 12, 24, 12],"a1   ") , &
           spgr_info_type(  5,"C 2 1 1     ","C 2x            ", 2, 3, [ 0, 0, 0, 12, 24, 12],"a2   ") , &
           spgr_info_type(  5,"I 2 1 1     ","I 2x            ", 2, 3, [ 0, 0, 0, 12, 24, 12],"a3   ") , &
           spgr_info_type(  6,"P 1 M 1     ","P -2y           ", 2, 4, [ 0, 0, 0, 24, 12, 24],"b    ") , &
           spgr_info_type(  6,"P M         ","P -2y           ", 2, 4, [ 0, 0, 0, 24, 12, 24],"b    ") , &
           spgr_info_type(  6,"P 1 1 M     ","P -2            ", 2, 4, [ 0, 0, 0, 24, 24, 12],"c    ") , &
           spgr_info_type(  6,"P M 1 1     ","P -2x           ", 2, 4, [ 0, 0, 0, 12, 24, 24],"a    ") , &
           spgr_info_type(  7,"P 1 C 1     ","P -2yc          ", 2, 4, [ 0, 0, 0, 24, 12, 24],"b1   ") , &
           spgr_info_type(  7,"P C         ","P -2yc          ", 2, 4, [ 0, 0, 0, 24, 12, 24],"b1   ") , &
           spgr_info_type(  7,"P 1 N 1     ","P -2yac         ", 2, 4, [ 0, 0, 0, 24, 12, 24],"b2   ") , &
           spgr_info_type(  7,"P N         ","P -2yac         ", 2, 4, [ 0, 0, 0, 24, 12, 24],"b2   ") , &
           spgr_info_type(  7,"P 1 A 1     ","P -2ya          ", 2, 4, [ 0, 0, 0, 24, 12, 24],"b3   ") , &
           spgr_info_type(  7,"P A         ","P -2ya          ", 2, 4, [ 0, 0, 0, 24, 12, 24],"b3   ") ]

      spgr_info(45:74)= [                                           &
           spgr_info_type(  7,"P 1 1 A     ","P -2a           ", 2, 4, [ 0, 0, 0, 24, 24, 12],"c1   ") , &
           spgr_info_type(  7,"P 1 1 N     ","P -2ab          ", 2, 4, [ 0, 0, 0, 24, 24, 12],"c2   ") , &
           spgr_info_type(  7,"P 1 1 B     ","P -2b           ", 2, 4, [ 0, 0, 0, 24, 24, 12],"c3   ") , &
           spgr_info_type(  7,"P B 1 1     ","P -2xb          ", 2, 4, [ 0, 0, 0, 12, 24, 24],"a1   ") , &
           spgr_info_type(  7,"P N 1 1     ","P -2xbc         ", 2, 4, [ 0, 0, 0, 12, 24, 24],"a2   ") , &
           spgr_info_type(  7,"P C 1 1     ","P -2xc          ", 2, 4, [ 0, 0, 0, 12, 24, 24],"a3   ") , &
           spgr_info_type(  8,"C 1 M 1     ","C -2y           ", 2, 4, [ 0, 0, 0, 24,  6, 24],"b1   ") , &
           spgr_info_type(  8,"C M         ","C -2y           ", 2, 4, [ 0, 0, 0, 24,  6, 24],"b1   ") , &
           spgr_info_type(  8,"A 1 M 1     ","A -2y           ", 2, 4, [ 0, 0, 0, 24,  6, 24],"b2   ") , &
           spgr_info_type(  8,"A M         ","A -2y           ", 2, 4, [ 0, 0, 0, 24,  6, 24],"b2   ") , &
           spgr_info_type(  8,"I 1 M 1     ","I -2y           ", 2, 4, [ 0, 0, 0, 24,  6, 24],"b3   ") , &
           spgr_info_type(  8,"I M         ","I -2y           ", 2, 4, [ 0, 0, 0, 24,  6, 24],"b3   ") , &
           spgr_info_type(  8,"A 1 1 M     ","A -2            ", 2, 4, [ 0, 0, 0, 24, 24,  6],"c1   ") , &
           spgr_info_type(  8,"B 1 1 M     ","B -2            ", 2, 4, [ 0, 0, 0, 24, 24,  6],"c2   ") , &
           spgr_info_type(  8,"I 1 1 M     ","I -2            ", 2, 4, [ 0, 0, 0, 24, 24,  6],"c3   ") , &
           spgr_info_type(  8,"B M 1 1     ","B -2x           ", 2, 4, [ 0, 0, 0,  6, 24, 24],"a1   ") , &
           spgr_info_type(  8,"C M 1 1     ","C -2x           ", 2, 4, [ 0, 0, 0,  6, 24, 24],"a2   ") , &
           spgr_info_type(  8,"I M 1 1     ","I -2x           ", 2, 4, [ 0, 0, 0,  6, 24, 24],"a3   ") , &
           spgr_info_type(  9,"C 1 C 1     ","C -2yc          ", 2, 4, [ 0, 0, 0, 24,  6, 24],"b1   ") , &
           spgr_info_type(  9,"C C         ","C -2yc          ", 2, 4, [ 0, 0, 0, 24,  6, 24],"b1   ") , &
           spgr_info_type(  9,"A 1 N 1     ","A -2yac         ", 2, 4, [ 0, 0, 0, 24,  6, 24],"b2   ") , &
           spgr_info_type(  9,"A N         ","A -2yac         ", 2, 4, [ 0, 0, 0, 24,  6, 24],"b2   ") , &
           spgr_info_type(  9,"I 1 A 1     ","I -2ya          ", 2, 4, [ 0, 0, 0, 24,  6, 24],"b3   ") , &
           spgr_info_type(  9,"I A         ","I -2ya          ", 2, 4, [ 0, 0, 0, 24,  6, 24],"b3   ") , &
           spgr_info_type(  9,"A 1 A 1     ","A -2ya          ", 2, 4, [ 0, 0, 0, 24,  6, 24],"-b1  ") , &
           spgr_info_type(  9,"A A         ","A -2ya          ", 2, 4, [ 0, 0, 0, 24,  6, 24],"-b1  ") , &
           spgr_info_type(  9,"C 1 N 1     ","C -2ybc         ", 2, 4, [ 0, 0, 0, 24,  6, 24],"-b2  ") , &
           spgr_info_type(  9,"C N         ","C -2ybc         ", 2, 4, [ 0, 0, 0, 24,  6, 24],"-b2  ") , &
           spgr_info_type(  9,"I 1 C 1     ","I -2yc          ", 2, 4, [ 0, 0, 0, 24,  6, 24],"-b3  ") , &
           spgr_info_type(  9,"I C         ","I -2yc          ", 2, 4, [ 0, 0, 0, 24,  6, 24],"-b3  ") ]

      spgr_info(75:104)= [                                           &
           spgr_info_type(  9,"A 1 1 A     ","A -2a           ", 2, 4, [ 0, 0, 0, 24, 24,  6],"c1   ") , &
           spgr_info_type(  9,"B 1 1 N     ","B -2bc          ", 2, 4, [ 0, 0, 0, 24, 24,  6],"c2   ") , &
           spgr_info_type(  9,"I 1 1 B     ","I -2b           ", 2, 4, [ 0, 0, 0, 24, 24,  6],"c3   ") , &
           spgr_info_type(  9,"B 1 1 B     ","B -2b           ", 2, 4, [ 0, 0, 0, 24, 24,  6],"-c1  ") , &
           spgr_info_type(  9,"A 1 1 N     ","A -2ac          ", 2, 4, [ 0, 0, 0, 24, 24,  6],"-c2  ") , &
           spgr_info_type(  9,"I 1 1 A     ","I -2a           ", 2, 4, [ 0, 0, 0, 24, 24,  6],"-c3  ") , &
           spgr_info_type(  9,"B B 1 1     ","B -2xb          ", 2, 4, [ 0, 0, 0,  6, 24, 24],"a1   ") , &
           spgr_info_type(  9,"C N 1 1     ","C -2xbc         ", 2, 4, [ 0, 0, 0,  6, 24, 24],"a2   ") , &
           spgr_info_type(  9,"I C 1 1     ","I -2xc          ", 2, 4, [ 0, 0, 0,  6, 24, 24],"a3   ") , &
           spgr_info_type(  9,"C C 1 1     ","C -2xc          ", 2, 4, [ 0, 0, 0,  6, 24, 24],"-a1  ") , &
           spgr_info_type(  9,"B N 1 1     ","B -2xbc         ", 2, 4, [ 0, 0, 0,  6, 24, 24],"-a2  ") , &
           spgr_info_type(  9,"I B 1 1     ","I -2xb          ", 2, 4, [ 0, 0, 0,  6, 24, 24],"-a3  ") , &
           spgr_info_type( 10,"P 1 2/M 1   ","-P 2y           ", 2, 5, [ 0, 0, 0, 12, 12, 24],"b    ") , &
           spgr_info_type( 10,"P 2/M       ","-P 2y           ", 2, 5, [ 0, 0, 0, 12, 12, 24],"b    ") , &
           spgr_info_type( 10,"P 1 1 2/M   ","-P 2            ", 2, 5, [ 0, 0, 0, 24, 12, 12],"c    ") , &
           spgr_info_type( 10,"P 2/M 1 1   ","-P 2x           ", 2, 5, [ 0, 0, 0, 12, 24, 12],"a    ") , &
           spgr_info_type( 11,"P 1 21/M 1  ","-P 2yb          ", 2, 5, [ 0, 0, 0, 24,  6, 24],"b    ") , &
           spgr_info_type( 11,"P 21/M      ","-P 2yb          ", 2, 5, [ 0, 0, 0, 24,  6, 24],"b    ") , &
           spgr_info_type( 11,"P 1 1 21/M  ","-P 2c           ", 2, 5, [ 0, 0, 0, 24, 24,  6],"c    ") , &
           spgr_info_type( 11,"P 21/M 1 1  ","-P 2xa          ", 2, 5, [ 0, 0, 0,  6, 24, 24],"a    ") , &
           spgr_info_type( 11,"B 1 21/M 1  ","-B 2yb          ", 2, 5, [ 0, 0, 0, 24,  6, 24],"b    ") , &
           spgr_info_type( 11,"B 21/M      ","-B 2yb          ", 2, 5, [ 0, 0, 0, 24,  6, 24],"b    ") , &
           spgr_info_type( 12,"C 1 2/M 1   ","-C 2y           ", 2, 5, [ 0, 0, 0, 12,  6, 24],"b1   ") , &
           spgr_info_type( 12,"C 2/M       ","-C 2y           ", 2, 5, [ 0, 0, 0, 12,  6, 24],"b1   ") , &
           spgr_info_type( 12,"A 1 2/M 1   ","-A 2y           ", 2, 5, [ 0, 0, 0, 12,  6, 24],"b2   ") , &
           spgr_info_type( 12,"A 2/M       ","-A 2y           ", 2, 5, [ 0, 0, 0, 12,  6, 24],"b2   ") , &
           spgr_info_type( 12,"I 1 2/M 1   ","-I 2y           ", 2, 5, [ 0, 0, 0, 12,  6, 24],"b3   ") , &
           spgr_info_type( 12,"I 2/M       ","-I 2y           ", 2, 5, [ 0, 0, 0, 12,  6, 24],"b3   ") , &
           spgr_info_type( 12,"A 1 1 2/M   ","-A 2            ", 2, 5, [ 0, 0, 0, 24, 12,  6],"c1   ") , &
           spgr_info_type( 12,"B 1 1 2/M   ","-B 2            ", 2, 5, [ 0, 0, 0, 24, 12,  6],"c2   ") ]

      spgr_info(105:134)= [                                           &
           spgr_info_type( 12,"I 1 1 2/M   ","-I 2            ", 2, 5, [ 0, 0, 0, 24, 12,  6],"c3   ") , &
           spgr_info_type( 12,"B 2/M 1 1   ","-B 2x           ", 2, 5, [ 0, 0, 0,  6, 24, 12],"a1   ") , &
           spgr_info_type( 12,"C 2/M 1 1   ","-C 2x           ", 2, 5, [ 0, 0, 0,  6, 24, 12],"a2   ") , &
           spgr_info_type( 12,"I 2/M 1 1   ","-I 2x           ", 2, 5, [ 0, 0, 0,  6, 24, 12],"a3   ") , &
           spgr_info_type( 12,"F 1 2/M 1   ","-F 2y           ", 2, 5, [ 0, 0, 0, 12,  6, 24],"b1   ") , &
           spgr_info_type( 12,"F 2/M       ","-F 2y           ", 2, 5, [ 0, 0, 0, 12,  6, 24],"b1   ") , &
           spgr_info_type( 13,"P 1 2/C 1   ","-P 2yc          ", 2, 5, [ 0, 0, 0, 12, 24, 12],"b1   ") , &
           spgr_info_type( 13,"P 2/C       ","-P 2yc          ", 2, 5, [ 0, 0, 0, 12, 24, 12],"b1   ") , &
           spgr_info_type( 13,"P 1 2/C 1   ","-P 2yc          ", 2, 5, [ 0, 0, 0, 12, 24, 12],"b1   ") , &
           spgr_info_type( 13,"P 1 2/N 1   ","-P 2yac         ", 2, 5, [ 0, 0, 0, 24, 24,  6],"b2   ") , &
           spgr_info_type( 13,"P 2/N       ","-P 2yac         ", 2, 5, [ 0, 0, 0, 24, 24,  6],"b2   ") , &
           spgr_info_type( 13,"P 1 2/A 1   ","-P 2ya          ", 2, 5, [ 0, 0, 0, 12, 24, 12],"b3   ") , &
           spgr_info_type( 13,"P 2/A       ","-P 2ya          ", 2, 5, [ 0, 0, 0, 12, 24, 12],"b3   ") , &
           spgr_info_type( 13,"P 1 1 2/A   ","-P 2a           ", 2, 5, [ 0, 0, 0, 12, 12, 24],"c1   ") , &
           spgr_info_type( 13,"C 1 1 2/A   ","-C 2a           ", 2, 5, [ 0, 0, 0, 12, 12, 24],"c1   ") , &
           spgr_info_type( 13,"P 1 1 2/N   ","-P 2ab          ", 2, 5, [ 0, 0, 0,  6, 24, 24],"c2   ") , &
           spgr_info_type( 13,"P 1 1 2/B   ","-P 2b           ", 2, 5, [ 0, 0, 0, 12, 12, 24],"c3   ") , &
           spgr_info_type( 13,"P 2/B 1 1   ","-P 2xb          ", 2, 5, [ 0, 0, 0, 24, 12, 12],"a1   ") , &
           spgr_info_type( 13,"P 2/N 1 1   ","-P 2xbc         ", 2, 5, [ 0, 0, 0, 24,  6, 24],"a2   ") , &
           spgr_info_type( 13,"P 2/C 1 1   ","-P 2xc          ", 2, 5, [ 0, 0, 0, 24, 12, 12],"a3   ") , &
           spgr_info_type( 14,"P 1 21/C 1  ","-P 2ybc         ", 2, 5, [ 0, 0, 0, 24,  6, 24],"b1   ") , &
           spgr_info_type( 14,"P 21/C      ","-P 2ybc         ", 2, 5, [ 0, 0, 0, 24,  6, 24],"b1   ") , &
           spgr_info_type( 14,"B 1 21/C 1  ","-B 2ybc         ", 2, 5, [ 0, 0, 0, 24,  6, 24],"b1   ") , &
           spgr_info_type( 14,"B 21/C      ","-B 2ybc         ", 2, 5, [ 0, 0, 0, 24,  6, 24],"b1   ") , &
           spgr_info_type( 14,"P 1 21/N 1  ","-P 2yn          ", 2, 5, [ 0, 0, 0, 24,  6, 24],"b2   ") , &
           spgr_info_type( 14,"P 21/N      ","-P 2yn          ", 2, 5, [ 0, 0, 0, 24,  6, 24],"b2   ") , &
           spgr_info_type( 14,"P 1 21/A 1  ","-P 2yab         ", 2, 5, [ 0, 0, 0, 24,  6, 24],"b3   ") , &
           spgr_info_type( 14,"P 21/A      ","-P 2yab         ", 2, 5, [ 0, 0, 0, 24,  6, 24],"b3   ") , &
           spgr_info_type( 14,"P 1 1 21/A  ","-P 2ac          ", 2, 5, [ 0, 0, 0, 24, 24,  6],"c1   ") , &
           spgr_info_type( 14,"P 1 1 21/N  ","-P 2n           ", 2, 5, [ 0, 0, 0, 24, 24,  6],"c2   ") ]

      spgr_info(135:162)= [                                           &
           spgr_info_type( 14,"P 1 1 21/B  ","-P 2bc          ", 2, 5, [ 0, 0, 0, 24, 24,  6],"c3   ") , &
           spgr_info_type( 14,"P 21/B 1 1  ","-P 2xab         ", 2, 5, [ 0, 0, 0,  6, 24, 24],"a1   ") , &
           spgr_info_type( 14,"P 21/N 1 1  ","-P 2xn          ", 2, 5, [ 0, 0, 0,  6, 24, 24],"a2   ") , &
           spgr_info_type( 14,"P 21/C 1 1  ","-P 2xac         ", 2, 5, [ 0, 0, 0,  6, 24, 24],"a3   ") , &
           spgr_info_type( 15,"C 1 2/C 1   ","-C 2yc          ", 2, 5, [ 0, 0, 0, 12, 12, 12],"b1   ") , &
           spgr_info_type( 15,"C 2/C       ","-C 2yc          ", 2, 5, [ 0, 0, 0, 12, 12, 12],"b1   ") , &
           spgr_info_type( 15,"A 1 2/N 1   ","-A 2yac         ", 2, 5, [ 0, 0, 0, 12, 24,  6],"b2   ") , &
           spgr_info_type( 15,"A 2/N       ","-A 2yac         ", 2, 5, [ 0, 0, 0, 12, 24,  6],"b2   ") , &
           spgr_info_type( 15,"I 1 2/A 1   ","-I 2ya          ", 2, 5, [ 0, 0, 0, 24, 12,  6],"b3   ") , &
           spgr_info_type( 15,"I 2/A       ","-I 2ya          ", 2, 5, [ 0, 0, 0, 24, 12,  6],"b3   ") , &
           spgr_info_type( 15,"A 1 2/A 1   ","-A 2ya          ", 2, 5, [ 0, 0, 0, 12, 12, 12],"-b1  ") , &
           spgr_info_type( 15,"A 2/A       ","-A 2ya          ", 2, 5, [ 0, 0, 0, 12, 12, 12],"-b1  ") , &
           spgr_info_type( 15,"C 1 2/N 1   ","-C 2ybc         ", 2, 5, [ 0, 0, 0,  6, 24, 12],"-b2  ") , &
           spgr_info_type( 15,"C 2/N       ","-C 2ybc         ", 2, 5, [ 0, 0, 0,  6, 24, 12],"-b2  ") , &
           spgr_info_type( 15,"I 1 2/C 1   ","-I 2yc          ", 2, 5, [ 0, 0, 0,  6, 12, 24],"-b3  ") , &
           spgr_info_type( 15,"I 2/C       ","-I 2yc          ", 2, 5, [ 0, 0, 0,  6, 12, 24],"-b3  ") , &
           spgr_info_type( 15,"A 1 1 2/A   ","-A 2a           ", 2, 5, [ 0, 0, 0, 12, 12, 12],"c1   ") , &
           spgr_info_type( 15,"B 1 1 2/N   ","-B 2bc          ", 2, 5, [ 0, 0, 0,  6, 12, 24],"c2   ") , &
           spgr_info_type( 15,"I 1 1 2/B   ","-I 2b           ", 2, 5, [ 0, 0, 0,  6, 24, 12],"c3   ") , &
           spgr_info_type( 15,"B 1 1 2/B   ","-B 2b           ", 2, 5, [ 0, 0, 0, 12, 12, 12],"-c1  ") , &
           spgr_info_type( 15,"A 1 1 2/N   ","-A 2ac          ", 2, 5, [ 0, 0, 0, 12,  6, 24],"-c2  ") , &
           spgr_info_type( 15,"I 1 1 2/A   ","-I 2a           ", 2, 5, [ 0, 0, 0, 24,  6, 12],"-c3  ") , &
           spgr_info_type( 15,"B 2/B 1 1   ","-B 2xb          ", 2, 5, [ 0, 0, 0, 12, 12, 12],"a1   ") , &
           spgr_info_type( 15,"C 2/N 1 1   ","-C 2xbc         ", 2, 5, [ 0, 0, 0, 24,  6, 12],"a2   ") , &
           spgr_info_type( 15,"I 2/C 1 1   ","-I 2xc          ", 2, 5, [ 0, 0, 0, 24,  6, 12],"a3   ") , &
           spgr_info_type( 15,"C 2/C 1 1   ","-C 2xc          ", 2, 5, [ 0, 0, 0, 12, 12, 12],"-a1  ") , &
           spgr_info_type( 15,"B 2/N 1 1   ","-B 2xbc         ", 2, 5, [ 0, 0, 0, 24, 12,  6],"-a2  ") , &
           spgr_info_type( 15,"I 2/B 1 1   ","-I 2xb          ", 2, 5, [ 0, 0, 0, 24, 12,  6],"-a3  ") ]

      !---- Orthorhombic ----!
      spgr_info(163:192)= [                                           &
           spgr_info_type( 16,"P 2 2 2     ","P 2 2           ", 3, 6, [ 0, 0, 0, 12, 12, 24],"     ") , &
           spgr_info_type( 17,"P 2 2 21    ","P 2c 2          ", 3, 6, [ 0, 0, 0, 12, 12, 24],"     ") , &
           spgr_info_type( 17,"P 21 2 2    ","P 2a 2a         ", 3, 6, [ 0, 0, 0, 24, 12, 12],"cab  ") , &
           spgr_info_type( 17,"P 2 21 2    ","P 2 2b          ", 3, 6, [ 0, 0, 0, 12, 24, 12],"bca  ") , &
           spgr_info_type( 18,"P 21 21 2   ","P 2 2ab         ", 3, 6, [ 0, 0, 0, 12, 12, 24],"     ") , &
           spgr_info_type( 18,"P 2 21 21   ","P 2bc 2         ", 3, 6, [ 0, 0, 0, 24, 12, 12],"cab  ") , &
           spgr_info_type( 18,"P 21 2 21   ","P 2ac 2ac       ", 3, 6, [ 0, 0, 0, 12, 24, 12],"bca  ") , &
           spgr_info_type( 19,"P 21 21 21  ","P 2ac 2ab       ", 3, 6, [ 0, 0, 0, 12, 12, 24],"     ") , &
           spgr_info_type( 20,"C 2 2 21    ","C 2c 2          ", 3, 6, [ 0, 0, 0, 12, 12, 12],"     ") , &
           spgr_info_type( 20,"A 21 2 2    ","A 2a 2a         ", 3, 6, [ 0, 0, 0, 12, 12, 12],"cab  ") , &
           spgr_info_type( 20,"B 2 21 2    ","B 2 2b          ", 3, 6, [ 0, 0, 0, 12, 12, 12],"bca  ") , &
           spgr_info_type( 21,"C 2 2 2     ","C 2 2           ", 3, 6, [ 0, 0, 0,  6, 12, 24],"     ") , &
           spgr_info_type( 21,"A 2 2 2     ","A 2 2           ", 3, 6, [ 0, 0, 0, 24,  6, 12],"cab  ") , &
           spgr_info_type( 21,"B 2 2 2     ","B 2 2           ", 3, 6, [ 0, 0, 0, 12, 24,  6],"bca  ") , &
           spgr_info_type( 22,"F 2 2 2     ","F 2 2           ", 3, 6, [ 0, 0, 0,  6,  6, 24],"     ") , &
           spgr_info_type( 23,"I 2 2 2     ","I 2 2           ", 3, 6, [ 0, 0, 0, 12, 12, 12],"     ") , &
           spgr_info_type( 24,"I 21 21 21  ","I 2b 2c         ", 3, 6, [ 0, 0, 0, 12, 12, 12],"     ") , &
           spgr_info_type( 25,"P M M 2     ","P 2 -2          ", 3, 7, [ 0, 0, 0, 12, 12, 24],"     ") , &
           spgr_info_type( 25,"P 2 M M     ","P -2 2          ", 3, 9, [ 0, 0, 0, 24, 12, 12],"cab  ") , &
           spgr_info_type( 25,"P M 2 M     ","P -2 -2         ", 3, 8, [ 0, 0, 0, 12, 24, 12],"bca  ") , &
           spgr_info_type( 26,"P M C 21    ","P 2c -2         ", 3, 7, [ 0, 0, 0, 12, 12, 24],"     ") , &
           spgr_info_type( 26,"P C M 21    ","P 2c -2c        ", 3, 7, [ 0, 0, 0, 12, 12, 24],"ba-c ") , &
           spgr_info_type( 26,"P 21 M A    ","P -2a 2a        ", 3, 9, [ 0, 0, 0, 24, 12, 12],"cab  ") , &
           spgr_info_type( 26,"P 21 A M    ","P -2 2a         ", 3, 9, [ 0, 0, 0, 24, 12, 12],"-cba ") , &
           spgr_info_type( 26,"P B 21 M    ","P -2 -2b        ", 3, 8, [ 0, 0, 0, 12, 24, 12],"bca  ") , &
           spgr_info_type( 26,"P M 21 B    ","P -2b -2        ", 3, 8, [ 0, 0, 0, 12, 24, 12],"a-cb ") , &
           spgr_info_type( 27,"P C C 2     ","P 2 -2c         ", 3, 7, [ 0, 0, 0, 12, 12, 24],"     ") , &
           spgr_info_type( 27,"P 2 A A     ","P -2a 2         ", 3, 9, [ 0, 0, 0, 24, 12, 12],"cab  ") , &
           spgr_info_type( 27,"P B 2 B     ","P -2b -2b       ", 3, 8, [ 0, 0, 0, 12, 24, 12],"bca  ") , &
           spgr_info_type( 28,"P M A 2     ","P 2 -2a         ", 3, 7, [ 0, 0, 0,  6, 24, 24],"     ") ]

      spgr_info(193:222)= [                                           &
           spgr_info_type( 28,"P B M 2     ","P 2 -2b         ", 3, 7, [ 0, 0, 0, 24,  6, 24],"ba-c ") , &
           spgr_info_type( 28,"P 2 M B     ","P -2b 2         ", 3, 9, [ 0, 0, 0, 24,  6, 24],"cab  ") , &
           spgr_info_type( 28,"P 2 C M     ","P -2c 2         ", 3, 9, [ 0, 0, 0, 24, 24,  6],"-cba ") , &
           spgr_info_type( 28,"P C 2 M     ","P -2c -2c       ", 3, 8, [ 0, 0, 0, 24, 24,  6],"bca  ") , &
           spgr_info_type( 28,"P M 2 A     ","P -2a -2a       ", 3, 8, [ 0, 0, 0,  6, 24, 24],"a-cb ") , &
           spgr_info_type( 29,"P C A 21    ","P 2c -2ac       ", 3, 7, [ 0, 0, 0,  6, 24, 24],"     ") , &
           spgr_info_type( 29,"P B C 21    ","P 2c -2b        ", 3, 7, [ 0, 0, 0, 24,  6, 24],"ba-c ") , &
           spgr_info_type( 29,"P 21 A B    ","P -2b 2a        ", 3, 9, [ 0, 0, 0, 24,  6, 24],"cab  ") , &
           spgr_info_type( 29,"P 21 C A    ","P -2ac 2a       ", 3, 9, [ 0, 0, 0, 24, 24,  6],"-cba ") , &
           spgr_info_type( 29,"P C 21 B    ","P -2bc -2c      ", 3, 8, [ 0, 0, 0, 24, 24,  6],"bca  ") , &
           spgr_info_type( 29,"P B 21 A    ","P -2a -2ab      ", 3, 8, [ 0, 0, 0,  6, 24, 24],"a-cb ") , &
           spgr_info_type( 30,"P N C 2     ","P 2 -2bc        ", 3, 7, [ 0, 0, 0, 12, 24, 12],"     ") , &
           spgr_info_type( 30,"P C N 2     ","P 2 -2ac        ", 3, 7, [ 0, 0, 0, 24, 12, 12],"ba-c ") , &
           spgr_info_type( 30,"P 2 N A     ","P -2ac 2        ", 3, 9, [ 0, 0, 0, 12, 12, 24],"cab  ") , &
           spgr_info_type( 30,"P 2 A N     ","P -2ab 2        ", 3, 9, [ 0, 0, 0, 12, 24, 12],"-cba ") , &
           spgr_info_type( 30,"P B 2 N     ","P -2ab -2ab     ", 3, 8, [ 0, 0, 0, 24, 12, 12],"bca  ") , &
           spgr_info_type( 30,"P N 2 B     ","P -2bc -2bc     ", 3, 8, [ 0, 0, 0, 12, 12, 24],"a-cb ") , &
           spgr_info_type( 31,"P M N 21    ","P 2ac -2        ", 3, 7, [ 0, 0, 0, 12, 12, 24],"     ") , &
           spgr_info_type( 31,"P N M 21    ","P 2bc -2bc      ", 3, 7, [ 0, 0, 0, 12, 12, 24],"ba-c ") , &
           spgr_info_type( 31,"P 21 M N    ","P -2ab 2ab      ", 3, 9, [ 0, 0, 0, 24, 12, 12],"cab  ") , &
           spgr_info_type( 31,"P 21 N M    ","P -2 2ac        ", 3, 9, [ 0, 0, 0, 24, 12, 12],"-cba ") , &
           spgr_info_type( 31,"P N 21 M    ","P -2 -2bc       ", 3, 8, [ 0, 0, 0, 12, 24, 12],"bca  ") , &
           spgr_info_type( 31,"P M 21 N    ","P -2ab -2       ", 3, 8, [ 0, 0, 0, 12, 24, 12],"a-cb ") , &
           spgr_info_type( 32,"P B A 2     ","P 2 -2ab        ", 3, 7, [ 0, 0, 0, 12, 12, 24],"     ") , &
           spgr_info_type( 32,"P 2 C B     ","P -2bc 2        ", 3, 9, [ 0, 0, 0, 24, 12, 12],"cab  ") , &
           spgr_info_type( 32,"P C 2 A     ","P -2ac -2ac     ", 3, 8, [ 0, 0, 0, 12, 24, 12],"bca  ") , &
           spgr_info_type( 33,"P N A 21    ","P 2c -2n        ", 3, 7, [ 0, 0, 0, 12, 12, 24],"     ") , &
           spgr_info_type( 33,"P B N 21    ","P 2c -2ab       ", 3, 7, [ 0, 0, 0, 12, 12, 24],"ba-c ") , &
           spgr_info_type( 33,"P 21 N B    ","P -2bc 2a       ", 3, 9, [ 0, 0, 0, 24, 12, 12],"cab  ") , &
           spgr_info_type( 33,"P 21 C N    ","P -2n 2a        ", 3, 9, [ 0, 0, 0, 24, 12, 12],"-cba ") ]

      spgr_info(223:252)= [                                           &
           spgr_info_type( 33,"P C 21 N    ","P -2n -2ac      ", 3, 8, [ 0, 0, 0, 12, 24, 12],"bca  ") , &
           spgr_info_type( 33,"P N 21 A    ","P -2ac -2n      ", 3, 8, [ 0, 0, 0, 12, 24, 12],"a-cb ") , &
           spgr_info_type( 34,"P N N 2     ","P 2 -2n         ", 3, 7, [ 0, 0, 0, 12, 12, 24],"     ") , &
           spgr_info_type( 34,"P 2 N N     ","P -2n 2         ", 3, 9, [ 0, 0, 0, 24, 12, 12],"cab  ") , &
           spgr_info_type( 34,"P N 2 N     ","P -2n -2n       ", 3, 8, [ 0, 0, 0, 12, 24, 12],"bca  ") , &
           spgr_info_type( 35,"C M M 2     ","C 2 -2          ", 3, 7, [ 0, 0, 0,  6, 12, 24],"     ") , &
           spgr_info_type( 35,"A 2 M M     ","A -2 2          ", 3, 9, [ 0, 0, 0, 24,  6, 12],"cab  ") , &
           spgr_info_type( 35,"B M 2 M     ","B -2 -2         ", 3, 8, [ 0, 0, 0, 12, 24,  6],"bca  ") , &
           spgr_info_type( 36,"C M C 21    ","C 2c -2         ", 3, 7, [ 0, 0, 0, 12, 12, 12],"     ") , &
           spgr_info_type( 36,"C C M 21    ","C 2c -2c        ", 3, 7, [ 0, 0, 0, 12, 12, 12],"ba-c ") , &
           spgr_info_type( 36,"A 21 M A    ","A -2a 2a        ", 3, 9, [ 0, 0, 0, 12, 12, 12],"cab  ") , &
           spgr_info_type( 36,"A 21 A M    ","A -2 2a         ", 3, 9, [ 0, 0, 0, 12, 12, 12],"-cba ") , &
           spgr_info_type( 36,"B B 21 M    ","B -2 -2b        ", 3, 8, [ 0, 0, 0, 12, 12, 12],"bca  ") , &
           spgr_info_type( 36,"B M 21 B    ","B -2b -2        ", 3, 8, [ 0, 0, 0, 12, 12, 12],"a-cb ") , &
           spgr_info_type( 37,"C C C 2     ","C 2 -2c         ", 3, 7, [ 0, 0, 0,  6, 12, 24],"     ") , &
           spgr_info_type( 37,"A 2 A A     ","A -2a 2         ", 3, 9, [ 0, 0, 0, 24,  6, 12],"cab  ") , &
           spgr_info_type( 37,"B B 2 B     ","B -2b -2b       ", 3, 8, [ 0, 0, 0, 12, 24,  6],"bca  ") , &
           spgr_info_type( 38,"A M M 2     ","A 2 -2          ", 3, 7, [ 0, 0, 0, 12, 12, 12],"     ") , &
           spgr_info_type( 38,"B M M 2     ","B 2 -2          ", 3, 7, [ 0, 0, 0, 12, 12, 12],"ba-c ") , &
           spgr_info_type( 38,"B 2 M M     ","B -2 2          ", 3, 9, [ 0, 0, 0, 12, 12, 12],"cab  ") , &
           spgr_info_type( 38,"C 2 M M     ","C -2 2          ", 3, 9, [ 0, 0, 0, 12, 12, 12],"-cba ") , &
           spgr_info_type( 38,"C M 2 M     ","C -2 -2         ", 3, 8, [ 0, 0, 0, 12, 12, 12],"bca  ") , &
           spgr_info_type( 38,"A M 2 M     ","A -2 -2         ", 3, 8, [ 0, 0, 0, 12, 12, 12],"a-cb ") , &
           spgr_info_type( 39,"A B M 2     ","A 2 -2c         ", 3, 7, [ 0, 0, 0, 12,  6, 24],"     ") , &
           spgr_info_type( 39,"B M A 2     ","B 2 -2c         ", 3, 7, [ 0, 0, 0,  6, 12, 24],"ba-c ") , &
           spgr_info_type( 39,"B 2 C M     ","B -2c 2         ", 3, 9, [ 0, 0, 0, 24, 12,  6],"cab  ") , &
           spgr_info_type( 39,"C 2 M B     ","C -2b 2         ", 3, 9, [ 0, 0, 0, 24,  6, 12],"-cba ") , &
           spgr_info_type( 39,"C M 2 A     ","C -2b -2b       ", 3, 8, [ 0, 0, 0,  6, 24, 12],"bca  ") , &
           spgr_info_type( 39,"A C 2 M     ","A -2c -2c       ", 3, 8, [ 0, 0, 0, 12, 24,  6],"a-cb ") , &
           spgr_info_type( 40,"A M A 2     ","A 2 -2a         ", 3, 7, [ 0, 0, 0,  6, 12, 24],"     ") ]

      spgr_info(253:282)= [                                           &
           spgr_info_type( 40,"B B M 2     ","B 2 -2b         ", 3, 7, [ 0, 0, 0, 12,  6, 24],"ba-c ") , &
           spgr_info_type( 40,"B 2 M B     ","B -2b 2         ", 3, 9, [ 0, 0, 0, 24,  6, 12],"cab  ") , &
           spgr_info_type( 40,"C 2 C M     ","C -2c 2         ", 3, 9, [ 0, 0, 0, 24, 12,  6],"-cba ") , &
           spgr_info_type( 40,"C C 2 M     ","C -2c -2c       ", 3, 8, [ 0, 0, 0, 12, 24,  6],"bca  ") , &
           spgr_info_type( 40,"A M 2 A     ","A -2a -2a       ", 3, 8, [ 0, 0, 0,  6, 24, 12],"a-cb ") , &
           spgr_info_type( 41,"A B A 2     ","A 2 -2ac        ", 3, 7, [ 0, 0, 0, 12, 12, 12],"     ") , &
           spgr_info_type( 41,"B B A 2     ","B 2 -2bc        ", 3, 7, [ 0, 0, 0, 12, 12, 12],"ba-c ") , &
           spgr_info_type( 41,"B 2 C B     ","B -2bc 2        ", 3, 9, [ 0, 0, 0, 12, 12, 12],"cab  ") , &
           spgr_info_type( 41,"C 2 C B     ","C -2bc 2        ", 3, 9, [ 0, 0, 0, 12, 12, 12],"-cba ") , &
           spgr_info_type( 41,"C C 2 A     ","C -2bc -2bc     ", 3, 8, [ 0, 0, 0, 12, 12, 12],"bca  ") , &
           spgr_info_type( 41,"A C 2 A     ","A -2ac -2ac     ", 3, 8, [ 0, 0, 0, 12, 12, 12],"a-cb ") , &
           spgr_info_type( 42,"F M M 2     ","F 2 -2          ", 3, 7, [ 0, 0, 0,  6,  6, 24],"     ") , &
           spgr_info_type( 42,"F 2 M M     ","F -2 2          ", 3, 9, [ 0, 0, 0, 24,  6,  6],"cab  ") , &
           spgr_info_type( 42,"F M 2 M     ","F -2 -2         ", 3, 8, [ 0, 0, 0,  6, 24,  6],"bca  ") , &
           spgr_info_type( 43,"F D D 2     ","F 2 -2d         ", 3, 7, [ 0, 0, 0,  6,  6, 24],"     ") , &
           spgr_info_type( 43,"F 2 D D     ","F -2d 2         ", 3, 9, [ 0, 0, 0, 24,  6,  6],"cab  ") , &
           spgr_info_type( 43,"F D 2 D     ","F -2d -2d       ", 3, 8, [ 0, 0, 0,  6, 24,  6],"bca  ") , &
           spgr_info_type( 44,"I M M 2     ","I 2 -2          ", 3, 7, [ 0, 0, 0, 12, 12, 12],"     ") , &
           spgr_info_type( 44,"I 2 M M     ","I -2 2          ", 3, 9, [ 0, 0, 0, 12, 12, 12],"cab  ") , &
           spgr_info_type( 44,"I M 2 M     ","I -2 -2         ", 3, 8, [ 0, 0, 0, 12, 12, 12],"bca  ") , &
           spgr_info_type( 45,"I B A 2     ","I 2 -2c         ", 3, 7, [ 0, 0, 0, 12, 12, 12],"     ") , &
           spgr_info_type( 45,"I 2 C B     ","I -2a 2         ", 3, 9, [ 0, 0, 0, 12, 12, 12],"cab  ") , &
           spgr_info_type( 45,"I C 2 A     ","I -2b -2b       ", 3, 8, [ 0, 0, 0, 12, 12, 12],"bca  ") , &
           spgr_info_type( 46,"I M A 2     ","I 2 -2a         ", 3, 7, [ 0, 0, 0,  6, 24, 12],"     ") , &
           spgr_info_type( 46,"I B M 2     ","I 2 -2b         ", 3, 7, [ 0, 0, 0, 24,  6, 12],"ba-c ") , &
           spgr_info_type( 46,"I 2 M B     ","I -2b 2         ", 3, 9, [ 0, 0, 0, 12,  6, 24],"cab  ") , &
           spgr_info_type( 46,"I 2 C M     ","I -2c 2         ", 3, 9, [ 0, 0, 0, 12, 24,  6],"-cba ") , &
           spgr_info_type( 46,"I C 2 M     ","I -2c -2c       ", 3, 8, [ 0, 0, 0, 24, 12,  6],"bca  ") , &
           spgr_info_type( 46,"I M 2 A     ","I -2a -2a       ", 3, 8, [ 0, 0, 0,  6, 12, 12],"a-cb ") , &
           spgr_info_type( 47,"P M M M     ","-P 2 2          ", 3,10, [ 0, 0, 0, 12, 12, 12],"     ") ]

      spgr_info(283:312)= [                                           &
           spgr_info_type( 48,"P N N N:1   ","P 2 2 -1n       ", 3,10, [ 0, 0, 0,  6, 12, 24],"1    ") , &
           spgr_info_type( 48,"P N N N     ","-P 2ab 2bc      ", 3,10, [ 0,-6, 0,  6,  6, 24],"2    ") , &
           spgr_info_type( 49,"P C C M     ","-P 2 2c         ", 3,10, [ 0, 0, 0, 12, 12, 12],"     ") , &
           spgr_info_type( 49,"P M A A     ","-P 2a 2         ", 3,10, [ 0, 0, 0, 12, 12, 12],"cab  ") , &
           spgr_info_type( 49,"P B M B     ","-P 2b 2b        ", 3,10, [ 0, 0, 0, 12, 12, 12],"bca  ") , &
           spgr_info_type( 50,"P B A N:1   ","P 2 2 -1ab      ", 3,10, [ 0, 0, 0, 12, 12, 12],"1    ") , &
           spgr_info_type( 50,"P B A N     ","-P 2ab 2b       ", 3,10, [ 0, 0, 0,  6, 24, 12],"2    ") , &
           spgr_info_type( 50,"P N C B:1   ","P 2 2 -1bc      ", 3,10, [ 0, 0, 0, 12, 12, 12],"1cab ") , &
           spgr_info_type( 50,"P N C B     ","-P 2b 2bc       ", 3,10, [ 0, 0, 0, 12,  6, 24],"2cab ") , &
           spgr_info_type( 50,"P C N A:1   ","P 2 2 -1ac      ", 3,10, [ 0, 0, 0, 12, 12, 12],"1bca ") , &
           spgr_info_type( 50,"P C N A     ","-P 2a 2c        ", 3,10, [ 0, 0, 0, 24, 12,  6],"2bca ") , &
           spgr_info_type( 51,"P M M A     ","-P 2a 2a        ", 3,10, [ 0, 0, 0,  6, 12, 24],"     ") , &
           spgr_info_type( 51,"P M M B     ","-P 2b 2         ", 3,10, [ 0, 0, 0, 12,  6, 24],"ba-c ") , &
           spgr_info_type( 51,"P B M M     ","-P 2 2b         ", 3,10, [ 0, 0, 0, 24,  6, 12],"cab  ") , &
           spgr_info_type( 51,"P C M M     ","-P 2c 2c        ", 3,10, [ 0, 0, 0, 24, 12,  6],"-cba ") , &
           spgr_info_type( 51,"P M C M     ","-P 2c 2         ", 3,10, [ 0, 0, 0, 12, 24,  6],"bca  ") , &
           spgr_info_type( 51,"P M A M     ","-P 2 2a         ", 3,10, [ 0, 0, 0,  6, 24, 12],"a-cb ") , &
           spgr_info_type( 52,"P N N A     ","-P 2a 2bc       ", 3,10, [ 0, 0, 0, 24,  6, 12],"     ") , &
           spgr_info_type( 52,"P N N B     ","-P 2b 2n        ", 3,10, [ 0, 0, 0,  6, 24, 12],"ba-c ") , &
           spgr_info_type( 52,"P B N N     ","-P 2n 2b        ", 3,10, [ 0, 0, 0, 12, 24,  6],"cab  ") , &
           spgr_info_type( 52,"P C N N     ","-P 2ab 2c       ", 3,10, [ 0, 0, 0, 12,  6, 24],"-cba ") , &
           spgr_info_type( 52,"P N C N     ","-P 2ab 2n       ", 3,10, [ 0, 0, 0,  6, 12, 24],"bca  ") , &
           spgr_info_type( 52,"P N A N     ","-P 2n 2bc       ", 3,10, [ 0, 0, 0, 24, 12,  6],"a-cb ") , &
           spgr_info_type( 53,"P M N A     ","-P 2ac 2        ", 3,10, [ 0, 0, 0, 12, 24,  6],"     ") , &
           spgr_info_type( 53,"P N M B     ","-P 2bc 2bc      ", 3,10, [ 0, 0, 0, 24, 12,  6],"ba-c ") , &
           spgr_info_type( 53,"P B M N     ","-P 2ab 2ab      ", 3,10, [ 0, 0, 0,  6, 12, 24],"cab  ") , &
           spgr_info_type( 53,"P C N M     ","-P 2 2ac        ", 3,10, [ 0, 0, 0,  6, 24, 12],"-cba ") , &
           spgr_info_type( 53,"P N C M     ","-P 2 2bc        ", 3,10, [ 0, 0, 0, 24,  6, 12],"bca  ") , &
           spgr_info_type( 53,"P M A N     ","-P 2ab 2        ", 3,10, [ 0, 0, 0, 12,  6, 24],"a-cb ") , &
           spgr_info_type( 54,"P C C A     ","-P 2a 2ac       ", 3,10, [ 0, 0, 0, 12, 12, 12],"     ") ]

      spgr_info(313:342)= [                                           &
           spgr_info_type( 54,"P C C B     ","-P 2b 2c        ", 3,10, [ 0, 0, 0, 12, 12, 12],"ba-c ") , &
           spgr_info_type( 54,"P B A A     ","-P 2a 2b        ", 3,10, [ 0, 0, 0, 12, 12, 12],"cab  ") , &
           spgr_info_type( 54,"P C A A     ","-P 2ac 2c       ", 3,10, [ 0, 0, 0, 12, 12, 12],"-cba ") , &
           spgr_info_type( 54,"P B C B     ","-P 2bc 2b       ", 3,10, [ 0, 0, 0, 12, 12, 12],"bca  ") , &
           spgr_info_type( 54,"P B A B     ","-P 2b 2ab       ", 3,10, [ 0, 0, 0, 12, 12, 12],"a-cb ") , &
           spgr_info_type( 55,"P B A M     ","-P 2 2ab        ", 3,10, [ 0, 0, 0, 12, 12, 12],"     ") , &
           spgr_info_type( 55,"P M C B     ","-P 2bc 2        ", 3,10, [ 0, 0, 0, 12, 12, 12],"cab  ") , &
           spgr_info_type( 55,"P C M A     ","-P 2ac 2ac      ", 3,10, [ 0, 0, 0, 12, 12, 12],"bca  ") , &
           spgr_info_type( 56,"P C C N     ","-P 2ab 2ac      ", 3,10, [ 0, 0, 0,  6, 24, 12],"     ") , &
           spgr_info_type( 56,"P N A A     ","-P 2ac 2bc      ", 3,10, [ 0, 0, 0, 12,  6, 24],"cab  ") , &
           spgr_info_type( 56,"P B N B     ","-P 2bc 2ab      ", 3,10, [ 0, 0, 0, 24, 12,  6],"bca  ") , &
           spgr_info_type( 57,"P B C M     ","-P 2c 2b        ", 3,10, [ 0, 0, 0, 12, 24,  6],"     ") , &
           spgr_info_type( 57,"P C A M     ","-P 2c 2ac       ", 3,10, [ 0, 0, 0, 24, 12,  6],"ba-c ") , &
           spgr_info_type( 57,"P M C A     ","-P 2ac 2a       ", 3,10, [ 0, 0, 0,  6, 12, 24],"cab  ") , &
           spgr_info_type( 57,"P M A B     ","-P 2b 2a        ", 3,10, [ 0, 0, 0,  6, 24, 12],"-cba ") , &
           spgr_info_type( 57,"P B M A     ","-P 2a 2ab       ", 3,10, [ 0, 0, 0, 24,  6, 12],"bca  ") , &
           spgr_info_type( 57,"P C M B     ","-P 2bc 2c       ", 3,10, [ 0, 0, 0, 12,  6, 24],"a-cb ") , &
           spgr_info_type( 58,"P N N M     ","-P 2 2n         ", 3,10, [ 0, 0, 0, 12, 12, 12],"     ") , &
           spgr_info_type( 58,"P M N N     ","-P 2n 2         ", 3,10, [ 0, 0, 0, 12, 12, 12],"cab  ") , &
           spgr_info_type( 58,"P N M N     ","-P 2n 2n        ", 3,10, [ 0, 0, 0, 12, 12, 12],"bca  ") , &
           spgr_info_type( 59,"P M M N:1   ","P 2 2ab -1ab    ", 3,10, [ 0, 0, 0, 12, 12, 12],"1    ") , &
           spgr_info_type( 59,"P M M N     ","-P 2ab 2a       ", 3,10, [ 0,-6, 0,  6,  6, 24],"2    ") , &
           spgr_info_type( 59,"P N M M:1   ","P 2bc 2 -1bc    ", 3,10, [ 0, 0, 0, 12, 12, 12],"1cab ") , &
           spgr_info_type( 59,"P N M M     ","-P 2c 2bc       ", 3,10, [ 0, 0,-6, 24,  6,  6],"2cab ") , &
           spgr_info_type( 59,"P M N M:1   ","P 2ac 2ac -1ac  ", 3,10, [ 0, 0, 0, 12, 12, 12],"1bca ") , &
           spgr_info_type( 59,"P M N M     ","-P 2c 2a        ", 3,10, [-6, 0, 0,  6, 24,  6],"2bca ") , &
           spgr_info_type( 60,"P B C N     ","-P 2n 2ab       ", 3,10, [ 0, 0, 0, 12, 12, 12],"     ") , &
           spgr_info_type( 60,"P C A N     ","-P 2n 2c        ", 3,10, [ 0, 0, 0, 12, 12, 12],"ba-c ") , &
           spgr_info_type( 60,"P N C A     ","-P 2a 2n        ", 3,10, [ 0, 0, 0, 12, 12, 12],"cab  ") , &
           spgr_info_type( 60,"P N A B     ","-P 2bc 2n       ", 3,10, [ 0, 0, 0, 12, 12, 12],"-cba ") ]

      spgr_info(343:372)= [                                           &
           spgr_info_type( 60,"P B N A     ","-P 2ac 2b       ", 3,10, [ 0, 0, 0, 12, 12, 12],"bca  ") , &
           spgr_info_type( 60,"P C N B     ","-P 2b 2ac       ", 3,10, [ 0, 0, 0, 12, 12, 12],"a-cb ") , &
           spgr_info_type( 61,"P B C A     ","-P 2ac 2ab      ", 3,10, [ 0, 0, 0, 12, 12, 12],"     ") , &
           spgr_info_type( 61,"P C A B     ","-P 2bc 2ac      ", 3,10, [ 0, 0, 0, 12, 12, 12],"ba-c ") , &
           spgr_info_type( 62,"P N M A     ","-P 2ac 2n       ", 3,10, [ 0, 0, 0, 12,  6, 24],"     ") , &
           spgr_info_type( 62,"P M N B     ","-P 2bc 2a       ", 3,10, [ 0, 0, 0,  6, 12, 24],"ba-c ") , &
           spgr_info_type( 62,"P M N B:1   ","P 2ac 2ab -1ab  ", 3,10, [ 0, 0, 0,  6, 12, 24],"     ") , &
           spgr_info_type( 62,"P B N M     ","-P 2c 2ab       ", 3,10, [ 0, 0, 0, 24, 12,  6],"cab  ") , &
           spgr_info_type( 62,"P B N M:1   ","P 2c 2n -1c     ", 3,10, [ 0, 0, 0, 24, 12,  6],"     ") , &
           spgr_info_type( 62,"P C M N     ","-P 2n 2ac       ", 3,10, [ 0, 0, 0, 24,  6, 12],"-cba ") , &
           spgr_info_type( 62,"P M C N     ","-P 2n 2a        ", 3,10, [ 0, 0, 0,  6, 24, 12],"bca  ") , &
           spgr_info_type( 62,"P M C N:1   ","P 2bc 2a -1a    ", 3,10, [ 0, 0, 0,  6, 24, 12],"     ") , &
           spgr_info_type( 62,"P N A M     ","-P 2c 2n        ", 3,10, [ 0, 0, 0, 12, 24,  6],"a-cb ") , &
           spgr_info_type( 63,"C M C M     ","-C 2c 2         ", 3,10, [ 0, 0, 0, 12, 12, 24],"     ") , &
           spgr_info_type( 63,"C C M M     ","-C 2c 2c        ", 3,10, [ 0, 0, 0, 12, 12, 24],"ba-c ") , &
           spgr_info_type( 63,"A M M A     ","-A 2a 2a        ", 3,10, [ 0, 0, 0, 24, 12, 12],"cab  ") , &
           spgr_info_type( 63,"A M A M     ","-A 2 2a         ", 3,10, [ 0, 0, 0, 24, 12, 12],"-cba ") , &
           spgr_info_type( 63,"B B M M     ","-B 2 2b         ", 3,10, [ 0, 0, 0, 12, 24, 12],"bca  ") , &
           spgr_info_type( 63,"B M M B     ","-B 2b 2         ", 3,10, [ 0, 0, 0, 12, 24, 12],"a-cb ") , &
           spgr_info_type( 63,"B M M B:1   ","B 2ab 2c -1ac   ", 3,10, [ 0, 0, 0, 12, 24, 12],"     ") , &
           spgr_info_type( 64,"C M C A     ","-C 2bc 2        ", 3,10, [ 0, 0, 0,  6, 12, 12],"     ") , &
           spgr_info_type( 64,"C C M B     ","-C 2bc 2bc      ", 3,10, [ 0, 0, 0, 12,  6, 12],"ba-c ") , &
           spgr_info_type( 64,"C C M B:1   ","C 2bc 2n -1ab   ", 3,10, [ 0, 0, 0, 12,  6, 12],"     ") , &
           spgr_info_type( 64,"A B M A     ","-A 2ac 2ac      ", 3,10, [ 0, 0, 0, 12,  6, 12],"cab  ") , &
           spgr_info_type( 64,"A C A M     ","-A 2 2ac        ", 3,10, [ 0, 0, 0, 12, 12,  6],"-cba ") , &
           spgr_info_type( 64,"B B C M     ","-B 2 2bc        ", 3,10, [ 0, 0, 0, 12, 12,  6],"bca  ") , &
           spgr_info_type( 64,"B M A B     ","-B 2bc 2        ", 3,10, [ 0, 0, 0,  6, 12, 12],"a-cb ") , &
           spgr_info_type( 65,"C M M M     ","-C 2 2          ", 3,10, [ 0, 0, 0,  6, 12, 12],"     ") , &
           spgr_info_type( 65,"A M M M     ","-A 2 2          ", 3,10, [ 0, 0, 0, 12,  6, 12],"cab  ") , &
           spgr_info_type( 65,"B M M M     ","-B 2 2          ", 3,10, [ 0, 0, 0, 12, 12,  6],"bca  ") ]

      spgr_info(373:402)= [                                           &
           spgr_info_type( 66,"C C C M     ","-C 2 2c         ", 3,10, [ 0, 0, 0,  6, 12, 12],"     ") , &
           spgr_info_type( 66,"A M A A     ","-A 2a 2         ", 3,10, [ 0, 0, 0, 12,  6, 12],"cab  ") , &
           spgr_info_type( 66,"B B M B     ","-B 2b -2xb      ", 3,10, [ 0, 0, 0, 12, 12,  6],"bca  ") , &
           spgr_info_type( 67,"C M M A     ","-C 2b 2         ", 3,10, [ 0, 0, 0, 12,  6, 12],"     ") , &
           spgr_info_type( 67,"C M M B     ","-C 2b 2b        ", 3,10, [ 0, 0, 0,  6, 12, 12],"ba-c ") , &
           spgr_info_type( 67,"A B M M     ","-A 2c 2c        ", 3,10, [ 0, 0, 0, 12, 12,  6],"cab  ") , &
           spgr_info_type( 67,"A C M M     ","-A 2 2c         ", 3,10, [ 0, 0, 0, 12,  6, 12],"-cba ") , &
           spgr_info_type( 67,"B M C M     ","-B 2 2c         ", 3,10, [ 0, 0, 0,  6, 12, 12],"bca  ") , &
           spgr_info_type( 67,"B M A M     ","-B 2c 2         ", 3,10, [ 0, 0, 0, 12, 12,  6],"a-cb ") , &
           spgr_info_type( 68,"C C C A:1   ","C 2 2 -1bc      ", 3,10, [ 0, 0, 0,  6, 12, 12],"1    ") , &
           spgr_info_type( 68,"C C C A     ","-C 2b 2bc       ", 3,10, [ 0, 0, 0, 12,  6, 12],"2    ") , &
           spgr_info_type( 68,"C C C B:1   ","C 2 2 -1bc      ", 3,10, [ 0, 0, 0, 12,  6, 12],"1ba-c") , &
           spgr_info_type( 68,"C C C B     ","-C 2b 2c        ", 3,10, [ 0, 0, 0,  6, 12, 12],"2ba-c") , &
           spgr_info_type( 68,"A B A A:1   ","A 2 2 -1ac      ", 3,10, [ 0, 0, 0, 12, 12,  6],"1cab ") , &
           spgr_info_type( 68,"A B A A     ","-A 2a 2c        ", 3,10, [ 0, 0, 0, 12, 12,  6],"2cab ") , &
           spgr_info_type( 68,"A C A A:1   ","A 2 2 -1ac      ", 3,10, [ 0, 0, 0, 12,  6, 12],"1-cba") , &
           spgr_info_type( 68,"A C A A     ","-A 2ac 2c       ", 3,10, [ 0, 0, 0, 12,  6, 12],"2-cba") , &
           spgr_info_type( 68,"B B C B:1   ","B 2 2 -1bc      ", 3,10, [ 0, 0, 0, 12, 12,  6],"1bca ") , &
           spgr_info_type( 68,"B B C B     ","-B 2bc 2b       ", 3,10, [ 0, 0, 0,  6, 12, 12],"2bca ") , &
           spgr_info_type( 68,"B B A B:1   ","B 2 2 -1bc      ", 3,10, [ 0, 0, 0,  6, 12, 12],"1a-cb") , &
           spgr_info_type( 68,"B B A B     ","-B 2b 2bc       ", 3,10, [ 0, 0, 0, 12, 12,  6],"2a-cb") , &
           spgr_info_type( 69,"F M M M     ","-F 2 2          ", 3,10, [ 0, 0, 0,  6,  6, 12],"     ") , &
           spgr_info_type( 70,"F D D D:1   ","F 2 2 -1d       ", 3,10, [ 0, 0, 0,  3,  6, 24],"1    ") , &
           spgr_info_type( 70,"F D D D     ","-F 2uv 2vw      ", 3,10, [ 0,-3, 0,  3,  3, 24],"2    ") , &
           spgr_info_type( 71,"I M M M     ","-I 2 2          ", 3,10, [ 0, 0, 0,  6, 12, 12],"     ") , &
           spgr_info_type( 72,"I B A M     ","-I 2 2ab        ", 3,10, [ 0, 0, 0,  6, 12, 12],"     ") , &
           spgr_info_type( 72,"I M C B     ","-I 2a 2         ", 3,10, [ 0, 0, 0, 12,  6, 12],"cab  ") , &
           spgr_info_type( 72,"I C M A:1   ","I 2 2 -1b       ", 3,10, [ 0, 0, 0, 12, 12,  6],"     ") , &
           spgr_info_type( 72,"I C M A     ","-I 2b 2b        ", 3,10, [ 0, 0, 0, 12, 12,  6],"bca  ") , &
           spgr_info_type( 73,"I B C A     ","-I 2b 2c        ", 3,10, [ 0, 0, 0,  6, 12, 12],"     ") ]

      spgr_info(403:409)= [                                           &
           spgr_info_type( 73,"I C A B     ","-I 2a 2b        ", 3,10, [ 0, 0, 0, 12,  6, 12],"ba-c ") , &
           spgr_info_type( 74,"I M M A     ","-I 2b 2         ", 3,10, [ 0, 0, 0,  6,  6, 24],"     ") , &
           spgr_info_type( 74,"I M M B     ","-I 2a 2a        ", 3,10, [ 0, 0, 0,  6,  6, 24],"ba-c ") , &
           spgr_info_type( 74,"I B M M     ","-I 2c 2c        ", 3,10, [ 0, 0, 0, 24,  6,  6],"cab  ") , &
           spgr_info_type( 74,"I C M M     ","-I 2 2b         ", 3,10, [ 0, 0, 0, 24,  6,  6],"-cba ") , &
           spgr_info_type( 74,"I M C M     ","-I 2 2a         ", 3,10, [ 0, 0, 0,  6, 24,  6],"bca  ") , &
           spgr_info_type( 74,"I M A M     ","-I 2c 2         ", 3,10, [ 0, 0, 0,  6, 24,  6],"a-cb ") ]

      !---- Tetragonal ----!
      spgr_info(410:439)= [                                           &
           spgr_info_type( 75,"P 4         ","P 4             ", 4,11, [ 0, 0, 0, 12, 12, 24],"     ") , &
           spgr_info_type( 76,"P 41        ","P 4w            ", 4,11, [ 0, 0, 0, 12, 12, 24],"     ") , &
           spgr_info_type( 77,"P 42        ","P 4c            ", 4,11, [ 0, 0, 0, 12, 12, 24],"     ") , &
           spgr_info_type( 78,"P 43        ","P 4cw           ", 4,11, [ 0, 0, 0, 12, 12, 24],"     ") , &
           spgr_info_type( 79,"I 4         ","I 4             ", 4,11, [ 0, 0, 0, 12, 12, 12],"     ") , &
           spgr_info_type( 80,"I 41        ","I 4bw           ", 4,11, [ 0, 0, 0, 12, 24,  6],"     ") , &
           spgr_info_type( 81,"P -4        ","P -4            ", 4,12, [ 0, 0, 0, 12, 12, 24],"     ") , &
           spgr_info_type( 82,"I -4        ","I -4            ", 4,12, [ 0, 0, 0, 12, 12, 12],"     ") , &
           spgr_info_type( 83,"P 4/M       ","-P 4            ", 4,13, [ 0, 0, 0, 12, 12, 12],"     ") , &
           spgr_info_type( 84,"P 42/M      ","-P 4c           ", 4,13, [ 0, 0, 0, 12, 12, 12],"     ") , &
           spgr_info_type( 85,"P 4/N:1     ","P 4ab -1ab      ", 4,13, [ 0, 0, 0, 12, 12, 12],"1    ") , &
           spgr_info_type( 85,"P 4/N       ","-P 4a           ", 4,13, [-6,-6, 0,  6,  6, 12],"2    ") , &
           spgr_info_type( 86,"P 42/N:1    ","P 4n -1n        ", 4,13, [ 0, 0, 0, 12, 24,  6],"1    ") , &
           spgr_info_type( 86,"P 42/N      ","-P 4bc          ", 4,13, [-6,-6, 0,  6,  6, 12],"2    ") , &
           spgr_info_type( 87,"I 4/M       ","-I 4            ", 4,13, [ 0, 0, 0, 12, 12,  6],"     ") , &
           spgr_info_type( 88,"I 41/A:1    ","I 4bw -1bw      ", 4,13, [ 0, 0, 0,  6,  6, 24],"1    ") , &
           spgr_info_type( 88,"I 41/A      ","-I 4ad          ", 4,13, [ 0, 0, 0,  6,  6, 24],"2    ") , &
           spgr_info_type( 89,"P 4 2 2     ","P 4 2           ", 5,14, [ 0, 0, 0, 12, 12, 12],"     ") , &
           spgr_info_type( 90,"P 4 21 2    ","P 4ab 2ab       ", 5,14, [ 0, 0, 0, 12, 12, 12],"     ") , &
           spgr_info_type( 90,"C 4 2 21    ","C 4b 2          ", 5,14, [ 0, 0, 0, 12, 12, 12],"     ") , &
           spgr_info_type( 91,"P 41 2 2    ","P 4w 2c         ", 5,14, [ 0, 0, 0, 24, 24,  3],"     ") , &
           spgr_info_type( 92,"P 41 21 2   ","P 4abw 2nw      ", 5,14, [ 0, 0, 0, 24, 24,  3],"     ") , &
           spgr_info_type( 93,"P 42 2 2    ","P 4c 2          ", 5,14, [ 0, 0, 0, 12, 24,  6],"     ") , &
           spgr_info_type( 94,"P 42 21 2   ","P 4n 2n         ", 5,14, [ 0, 0, 0, 12, 12, 12],"     ") , &
           spgr_info_type( 95,"P 43 2 2    ","P 4cw 2c        ", 5,14, [ 0, 0, 0, 24, 24,  3],"     ") , &
           spgr_info_type( 96,"P 43 21 2   ","P 4nw 2abw      ", 5,14, [ 0, 0, 0, 24, 24,  3],"     ") , &
           spgr_info_type( 97,"I 4 2 2     ","I 4 2           ", 5,14, [ 0, 0, 0, 12, 12,  6],"     ") , &
           spgr_info_type( 98,"I 41 2 2    ","I 4bw 2bw       ", 5,14, [ 0, 0, 0, 12, 24,  3],"     ") , &
           spgr_info_type( 99,"P 4 M M     ","P 4 -2          ", 5,15, [ 0, 0, 0, 12, 12, 24],"     ") , &
           spgr_info_type(100,"P 4 B M     ","P 4 -2ab        ", 5,15, [ 0, 0, 0, 12, 12, 24],"     ") ]

      spgr_info(440:469)= [                                           &
           spgr_info_type(101,"P 42 C M    ","P 4c -2c        ", 5,15, [ 0, 0, 0, 12, 12, 24],"     ") , &
           spgr_info_type(102,"P 42 N M    ","P 4n -2n        ", 5,15, [ 0, 0, 0, 12, 12, 24],"     ") , &
           spgr_info_type(103,"P 4 C C     ","P 4 -2c         ", 5,15, [ 0, 0, 0, 12, 12, 12],"     ") , &
           spgr_info_type(104,"P 4 N C     ","P 4 -2n         ", 5,15, [ 0, 0, 0, 12, 12, 12],"     ") , &
           spgr_info_type(105,"P 42 M C    ","P 4c -2         ", 5,15, [ 0, 0, 0, 12, 12, 12],"     ") , &
           spgr_info_type(106,"P 42 B C    ","P 4c -2ab       ", 5,15, [ 0, 0, 0, 12, 12, 12],"     ") , &
           spgr_info_type(107,"I 4 M M     ","I 4 -2          ", 5,15, [ 0, 0, 0, 12, 12, 12],"     ") , &
           spgr_info_type(108,"I 4 C M     ","I 4 -2c         ", 5,15, [ 0, 0, 0, 12, 12, 12],"     ") , &
           spgr_info_type(109,"I 41 M D    ","I 4bw -2        ", 5,15, [ 0, 0, 0, 12, 12,  6],"     ") , &
           spgr_info_type(110,"I 41 C D    ","I 4bw -2c       ", 5,15, [ 0, 0, 0, 12, 12,  6],"     ") , &
           spgr_info_type(111,"P -4 2 M    ","P -4 2          ", 5,16, [ 0, 0, 0, 12, 12, 24],"     ") , &
           spgr_info_type(112,"P -4 2 C    ","P -4 2c         ", 5,16, [ 0, 0, 0, 12, 12, 12],"     ") , &
           spgr_info_type(113,"P -4 21 M   ","P -4 2ab        ", 5,16, [ 0, 0, 0, 12, 12, 24],"     ") , &
           spgr_info_type(114,"P -4 21 C   ","P -4 2n         ", 5,16, [ 0, 0, 0, 12, 12, 12],"     ") , &
           spgr_info_type(115,"P -4 M 2    ","P -4 -2         ", 5,17, [ 0, 0, 0, 12, 12, 12],"     ") , &
           spgr_info_type(116,"P -4 C 2    ","P -4 -2c        ", 5,17, [ 0, 0, 0, 12, 24,  6],"     ") , &
           spgr_info_type(117,"P -4 B 2    ","P -4 -2ab       ", 5,17, [ 0, 0, 0, 12, 12, 12],"     ") , &
           spgr_info_type(117,"C -4 D 2    ","C -4 2b         ", 5,17, [ 0, 0, 0, 12, 12, 12],"     ") , &
           spgr_info_type(118,"P -4 N 2    ","P -4 -2n        ", 5,17, [ 0, 0, 0, 12, 12,  6],"     ") , &
           spgr_info_type(119,"I -4 M 2    ","I -4 -2         ", 5,17, [ 0, 0, 0, 12, 12,  6],"     ") , &
           spgr_info_type(120,"I -4 C 2    ","I -4 -2c        ", 5,17, [ 0, 0, 0, 12, 12,  6],"     ") , &
           spgr_info_type(121,"I -4 2 M    ","I -4 2          ", 5,16, [ 0, 0, 0, 12, 12, 12],"     ") , &
           spgr_info_type(122,"I -4 2 D    ","I -4 2bw        ", 5,16, [ 0, 0, 0, 12, 24,  3],"     ") , &
           spgr_info_type(122,"F -4 D 2    ","F -4 -2cd       ", 5,16, [ 0, 0, 0, 12, 24,  3],"     ") , &
           spgr_info_type(123,"P 4/M M M   ","-P 4 2          ", 5,18, [ 0, 0, 0, 12, 12, 12],"     ") , &
           spgr_info_type(124,"P 4/M C C   ","-P 4 2c         ", 5,18, [ 0, 0, 0, 12, 12,  6],"     ") , &
           spgr_info_type(125,"P 4/N B M:1 ","P 4 2 -1ab      ", 5,18, [ 0, 0, 0, 12, 12, 12],"1    ") , &
           spgr_info_type(125,"P 4/N B M   ","-P 4a 2b        ", 5,18, [-6,-6, 0,  6,  6, 12],"2    ") , &
           spgr_info_type(126,"P 4/N N C:1 ","P 4 2 -1n       ", 5,18, [ 0, 0, 0, 12, 12,  6],"1    ") , &
           spgr_info_type(126,"P 4/N N C   ","-P 4a 2bc       ", 5,18, [-6,-6, 0,  6,  6,  6],"2    ") ]

      spgr_info(470:494)= [                                           &
           spgr_info_type(127,"P 4/M B M   ","-P 4 2ab        ", 5,18, [ 0, 0, 0, 12, 12, 12],"     ") , &
           spgr_info_type(128,"P 4/M N C   ","-P 4 2n         ", 5,18, [ 0, 0, 0, 12, 12,  6],"     ") , &
           spgr_info_type(129,"P 4/N M M:1 ","P 4ab 2ab -1ab  ", 5,18, [ 0, 0, 0, 12, 12, 12],"1    ") , &
           spgr_info_type(129,"P 4/N M M   ","-P 4a 2a        ", 5,18, [-6,-6, 0,  6,  6, 12],"2    ") , &
           spgr_info_type(130,"P 4/N C C:1 ","P 4ab 2n -1ab   ", 5,18, [ 0, 0, 0, 12, 12,  6],"1    ") , &
           spgr_info_type(130,"P 4/N C C   ","-P 4a 2ac       ", 5,18, [-6,-6, 0,  6,  6,  6],"2    ") , &
           spgr_info_type(131,"P 42/M M C  ","-P 4c 2         ", 5,18, [ 0, 0, 0, 12, 12,  6],"     ") , &
           spgr_info_type(132,"P 42/M C M  ","-P 4c 2c        ", 5,18, [ 0, 0, 0, 12, 12,  6],"     ") , &
           spgr_info_type(133,"P 42/N B C:1","P 4n 2c -1n     ", 5,18, [ 0, 0, 0, 12, 12,  6],"1    ") , &
           spgr_info_type(133,"P 42/N B C  ","-P 4ac 2b       ", 5,18, [-6,-6, 0,  6,  6,  6],"2    ") , &
           spgr_info_type(134,"P 42/N N M:1","P 4n 2 -1n      ", 5,18, [ 0, 0, 0, 12, 24,  6],"1    ") , &
           spgr_info_type(134,"P 42/N N M  ","-P 4ac 2bc      ", 5,18, [-6,-6, 0,  6,  6, 12],"2    ") , &
           spgr_info_type(135,"P 42/M B C  ","-P 4c 2ab       ", 5,18, [ 0, 0, 0, 12, 12,  6],"     ") , &
           spgr_info_type(136,"P 42/M N M  ","-P 4n 2n        ", 5,18, [ 0, 0, 0, 12, 12, 12],"     ") , &
           spgr_info_type(137,"P 42/N M C:1","P 4n 2n -1n     ", 5,18, [ 0, 0, 0, 12, 12,  6],"1    ") , &
           spgr_info_type(137,"P 42/N M C  ","-P 4ac 2a       ", 5,18, [-6,-6, 0,  6,  6,  6],"2    ") , &
           spgr_info_type(138,"P 42/N C M:1","P 4n 2ab -1n    ", 5,18, [ 0, 0, 0,  6, 12, 24],"1    ") , &
           spgr_info_type(138,"P 42/N C M  ","-P 4ac 2ac      ", 5,18, [-6,-6, 0,  6,  6, 12],"2    ") , &
           spgr_info_type(139,"I 4/M M M   ","-I 4 2          ", 5,18, [ 0, 0, 0, 12, 12,  6],"     ") , &
           spgr_info_type(139,"F 4/M M M   ","-F 4 2          ", 5,18, [ 0, 0, 0, 12, 12,  6],"     ") , &
           spgr_info_type(140,"I 4/M C M   ","-I 4 2c         ", 5,18, [ 0, 0, 0, 12, 12,  6],"     ") , &
           spgr_info_type(141,"I 41/A M D:1","I 4bw 2bw -1bw  ", 5,18, [ 0, 0, 0, 12, 12,  3],"1    ") , &
           spgr_info_type(141,"I 41/A M D  ","-I 4bd 2        ", 5,18, [ 0,-6, 0, 12,  6,  3],"2    ") , &
           spgr_info_type(142,"I 41/A C D:1","I 4bw 2aw -1bw  ", 5,18, [ 0, 0, 0, 12, 12,  3],"1    ") , &
           spgr_info_type(142,"I 41/A C D  ","-I 4bd 2c       ", 5,18, [ 0,-6, 0, 12,  6,  3],"2    ") ]

      !---- Trigonal/Rhombohedral ----!
      spgr_info(495:526)= [                                           &
           spgr_info_type(143,"P 3         ","P 3             ", 8,19, [ 0, 0, 0, 16, 16, 24],"     ") , &
           spgr_info_type(144,"P 31        ","P 31            ", 8,19, [ 0, 0, 0, 24, 24,  8],"     ") , &
           spgr_info_type(145,"P 32        ","P 32            ", 8,19, [ 0, 0, 0, 24, 24,  8],"     ") , &
           spgr_info_type(146,"R 3         ","R 3             ", 8,19, [ 0, 0, 0, 16, 16,  8],"H    ") , &
           spgr_info_type(146,"R 3:R       ","P 3*            ", 6,19, [ 0, 0, 0, 24, 24, 24],"R    ") , &
           spgr_info_type(147,"P -3        ","-P 3            ", 8,20, [ 0, 0, 0, 16, 16, 12],"     ") , &
           spgr_info_type(148,"R -3        ","-R 3            ", 8,20, [ 0, 0, 0, 16, 16,  4],"H    ") , &
           spgr_info_type(148,"R -3:R      ","-P 3*           ", 6,20, [ 0, 0, 0, 24, 24, 12],"R    ") , &
           spgr_info_type(149,"P 3 1 2     ","P 3 2           ",10,24, [ 0, 0, 0, 16, 16, 12],"     ") , &
           spgr_info_type(150,"P 3 2 1     ","P 3 2""         ", 9,21, [ 0, 0, 0, 16, 16, 12],"     ") , &
           spgr_info_type(151,"P 31 1 2    ","P 31 2c (0 0 1) ",10,24, [ 0, 0, 0, 24, 24,  4],"     ") , &
           spgr_info_type(152,"P 31 2 1    ","P 31 2""        ", 9,21, [ 0, 0, 0, 24, 24,  4],"     ") , &
           spgr_info_type(153,"P 32 1 2    ","P 32 2c (0 0 -1)",10,24, [ 0, 0, 0, 24, 24,  4],"     ") , &
           spgr_info_type(154,"P 32 2 1    ","P 32 2""        ", 9,21, [ 0, 0, 0, 24, 24,  4],"     ") , &
           spgr_info_type(155,"R 3 2       ","R 3 2""         ", 9,21, [ 0, 0, 0, 16, 16,  4],"H    ") , &
           spgr_info_type(155,"R 3 2:R     ","P 3* 2          ", 7,21, [ 0, 0, 0, 24, 24, 12],"R    ") , &
           spgr_info_type(156,"P 3 M 1     ","P 3 -2""        ", 9,22, [ 0, 0, 0, 16, 16, 24],"     ") , &
           spgr_info_type(157,"P 3 1 M     ","P 3 -2          ",10,25, [ 0, 0, 0, 16, 12, 24],"     ") , &
           spgr_info_type(158,"P 3 C 1     ","P 3 -2""c       ", 9,22, [ 0, 0, 0, 16, 16, 12],"     ") , &
           spgr_info_type(159,"P 3 1 C     ","P 3 -2c         ",10,25, [ 0, 0, 0, 16, 16, 12],"     ") , &
           spgr_info_type(160,"R 3 M       ","R 3 -2""        ", 9,22, [ 0, 0, 0, 16, 16,  8],"H    ") , &
           spgr_info_type(160,"R 3 M:R     ","P 3* -2         ", 7,22, [ 0, 0, 0, 24, 24, 24],"R    ") , &
           spgr_info_type(161,"R 3 C       ","R 3 -2""c       ", 9,22, [ 0, 0, 0, 16, 16,  4],"H    ") , &
           spgr_info_type(161,"R 3 N:R     ","P 3* -2n        ", 7,22, [ 0, 0, 0, 24, 24, 24],"R    ") , &
           spgr_info_type(162,"P -3 1 M    ","-P 3 2          ",10,26, [ 0, 0, 0, 16, 12, 12],"     ") , &
           spgr_info_type(163,"P -3 1 C    ","-P 3 2c         ",10,26, [ 0, 0, 0, 16, 16,  6],"     ") , &
           spgr_info_type(164,"P -3 M 1    ","-P 3 2""        ", 9,23, [ 0, 0, 0, 16,  8, 24],"     ") , &
           spgr_info_type(165,"P -3 C 1    ","-P 3 2""c       ", 9,23, [ 0, 0, 0, 16, 16,  6],"     ") , &
           spgr_info_type(166,"R -3 M      ","-R 3 2""        ", 9,23, [ 0, 0, 0, 16, 16,  4],"H    ") , &
           spgr_info_type(166,"R -3 M:R    ","-P 3* 2         ", 7,23, [ 0, 0, 0, 24, 24, 12],"R    ") , &
           spgr_info_type(167,"R -3 C      ","-R 3 2""c       ", 9,23, [ 0, 0, 0, 16, 16,  2],"H    ") , &
           spgr_info_type(167,"R -3 N:R    ","-P 3* 2n        ", 7,23, [ 6, 6, 6, 30, 30, 18],"R    ") ]

      !---- Hexagonal ----!
      spgr_info(527:553)= [                                           &
           spgr_info_type(168,"P 6         ","P 6             ",11,27, [ 0, 0, 0, 16, 12, 24],"     ") , &
           spgr_info_type(169,"P 61        ","P 61            ",11,27, [ 0, 0, 0, 24, 24,  4],"     ") , &
           spgr_info_type(170,"P 65        ","P 65            ",11,27, [ 0, 0, 0, 24, 24,  4],"     ") , &
           spgr_info_type(171,"P 62        ","P 62            ",11,27, [ 0, 0, 0, 24, 24,  8],"     ") , &
           spgr_info_type(172,"P 64        ","P 64            ",11,27, [ 0, 0, 0, 24, 24,  8],"     ") , &
           spgr_info_type(173,"P 63        ","P 6c            ",11,27, [ 0, 0, 0, 16, 16, 12],"     ") , &
           spgr_info_type(174,"P -6        ","P -6            ",11,28, [ 0, 0, 0, 16, 16, 12],"     ") , &
           spgr_info_type(175,"P 6/M       ","-P 6            ",11,29, [ 0, 0, 0, 16, 12, 12],"     ") , &
           spgr_info_type(176,"P 63/M      ","-P 6c           ",11,29, [ 0, 0, 0, 16, 16,  6],"     ") , &
           spgr_info_type(177,"P 6 2 2     ","P 6 2           ",12,30, [ 0, 0, 0, 16, 12, 12],"     ") , &
           spgr_info_type(178,"P 61 2 2    ","P 61 2 (0 0 -1) ",12,30, [ 0, 0, 0, 24, 24,  2],"     ") , &
           spgr_info_type(179,"P 65 2 2    ","P 65 2 (0 0 1)  ",12,30, [ 0, 0, 0, 24, 24,  2],"     ") , &
           spgr_info_type(180,"P 62 2 2    ","P 62 2c (0 0 1) ",12,30, [ 0, 0, 0, 24, 24,  4],"     ") , &
           spgr_info_type(181,"P 64 2 2    ","P 64 2c (0 0 -1)",12,30, [ 0, 0, 0, 24, 24,  4],"     ") , &
           spgr_info_type(182,"P 63 2 2    ","P 6c 2c         ",12,30, [ 0, 0, 0, 16, 16,  3],"     ") , &
           spgr_info_type(183,"P 6 M M     ","P 6 -2          ",12,31, [ 0, 0, 0, 16,  8, 24],"     ") , &
           spgr_info_type(184,"P 6 C C     ","P 6 -2c         ",12,31, [ 0, 0, 0, 16, 12, 12],"     ") , &
           spgr_info_type(185,"P 63 C M    ","P 6c -2         ",12,31, [ 0, 0, 0, 16, 12, 12],"     ") , &
           spgr_info_type(186,"P 63 M C    ","P 6c -2c        ",12,31, [ 0, 0, 0, 16,  8, 24],"     ") , &
           spgr_info_type(187,"P -6 M 2    ","P -6 2          ",12,33, [ 0, 0, 0, 16, 16, 12],"     ") , &
           spgr_info_type(188,"P -6 C 2    ","P -6c 2         ",12,33, [ 0, 0, 0, 16, 16,  6],"     ") , &
           spgr_info_type(189,"P -6 2 M    ","P -6 -2         ",12,32, [ 0, 0, 0, 16, 12, 12],"     ") , &
           spgr_info_type(190,"P -6 2 C    ","P -6c -2c       ",12,32, [ 0, 0, 0, 16, 16,  6],"     ") , &
           spgr_info_type(191,"P 6/M M M   ","-P 6 2          ",12,34, [ 0, 0, 0, 16,  8, 12],"     ") , &
           spgr_info_type(192,"P 6/M C C   ","-P 6 2c         ",12,34, [ 0, 0, 0, 16, 12,  6],"     ") , &
           spgr_info_type(193,"P 63/M C M  ","-P 6c 2         ",12,34, [ 0, 0, 0, 16, 12,  6],"     ") , &
           spgr_info_type(194,"P 63/M M C  ","-P 6c 2c        ",12,34, [ 0, 0, 0, 16, 16,  6],"     ") ]

      !---- Cubic ----!
      spgr_info(554:583)= [                                           &
           spgr_info_type(195,"P 2 3       ","P 2 2 3         ",13,35, [ 0, 0,  0, 24, 24, 12],"     ") , &
           spgr_info_type(196,"F 2 3       ","F 2 2 3         ",13,35, [ 0, 0, -6, 12, 12,  6],"     ") , &
           spgr_info_type(197,"I 2 3       ","I 2 2 3         ",13,35, [ 0, 0,  0, 24, 12, 12],"     ") , &
           spgr_info_type(198,"P 21 3      ","P 2ac 2ab 3     ",13,35, [ 0, 0,-12, 12, 12, 12],"     ") , &
           spgr_info_type(199,"I 21 3      ","I 2b 2c 3       ",13,35, [ 0, 0,  0, 12, 12, 12],"     ") , &
           spgr_info_type(200,"P M -3      ","-P 2 2 3        ",13,36, [ 0, 0,  0, 12, 12, 12],"     ") , &
           spgr_info_type(200,"P M 3       ","-P 2 2 3        ",13,36, [ 0, 0,  0, 12, 12, 12],"     ") , &
           spgr_info_type(201,"P N -3:1    ","P 2 2 3 -1n     ",13,36, [ 0, 0, 0, 24, 12,  12],"1    ") , &
           spgr_info_type(201,"P N -3      ","-P 2ab 2bc 3    ",13,36, [-6,-6,-6, 18,  6,   6],"2    ") , &
           spgr_info_type(201,"P N 3       ","-P 2ab 2bc 3    ",13,36, [-6,-6,-6, 18,  6,   6],"2    ") , &
           spgr_info_type(202,"F M -3      ","-F 2 2 3        ",13,36, [ 0, 0, 0, 12, 12,   6],"     ") , &
           spgr_info_type(202,"F M 3       ","-F 2 2 3        ",13,36, [ 0, 0, 0, 12, 12,   6],"     ") , &
           spgr_info_type(203,"F D -3:1    ","F 2 2 3 -1d     ",13,36, [ 0, 0,-6, 12,  6,   6],"1    ") , &
           spgr_info_type(203,"F D -3      ","-F 2uv 2vw 3    ",13,36, [-3,-3,-9,  9,  3,   3],"2    ") , &
           spgr_info_type(203,"F D 3       ","-F 2uv 2vw 3    ",13,36, [-3,-3,-9,  9,  3,   3],"2    ") , &
           spgr_info_type(204,"I M -3      ","-I 2 2 3        ",13,36, [ 0, 0, 0, 12, 12,  12],"     ") , &
           spgr_info_type(204,"I M 3       ","-I 2 2 3        ",13,36, [ 0, 0, 0, 12, 12,  12],"     ") , &
           spgr_info_type(205,"P A -3      ","-P 2ac 2ab 3    ",13,36, [ 0, 0, 0, 12, 12,  12],"     ") , &
           spgr_info_type(205,"P A 3       ","-P 2ac 2ab 3    ",13,36, [ 0, 0, 0, 12, 12,  12],"     ") , &
           spgr_info_type(206,"I A -3      ","-I 2b 2c 3      ",13,36, [ 0, 0, 0, 12, 12,   6],"     ") , &
           spgr_info_type(206,"I A 3       ","-I 2b 2c 3      ",13,36, [ 0, 0, 0, 12, 12,   6],"     ") , &
           spgr_info_type(207,"P 4 3 2     ","P 4 2 3         ",14,37, [ 0, 0, 0, 24, 12,  12],"     ") , &
           spgr_info_type(208,"P 42 3 2    ","P 4n 2 3        ",14,37, [ 0, 0,-6, 12, 12,   6],"     ") , &
           spgr_info_type(209,"F 4 3 2     ","F 4 2 3         ",14,37, [ 0, 0,-6, 12,  6,   6],"     ") , &
           spgr_info_type(210,"F 41 3 2    ","F 4d 2 3        ",14,37, [ 0,-3,-3, 12,  3,   3],"     ") , &
           spgr_info_type(211,"I 4 3 2     ","I 4 2 3         ",14,37, [ 0, 0, 0, 12, 12,   6],"     ") , &
           spgr_info_type(212,"P 43 3 2    ","P 4acd 2ab 3    ",14,37, [ 0, 0,-12, 12, 18,  6],"     ") , &
           spgr_info_type(213,"P 41 3 2    ","P 4bd 2ab 3     ",14,37, [-6, 0, 0, 12, 18,  12],"     ") , &
           spgr_info_type(214,"I 41 3 2    ","I 4bd 2c 3      ",14,37, [-9,-3,-3,  3,  3,   9],"     ") , &
           spgr_info_type(215,"P -4 3 M    ","P -4 2 3        ",14,38, [ 0, 0, 0, 24, 12,  12],"     ") ]

      spgr_info(584:612)= [                                           &
           spgr_info_type(216,"F -4 3 M    ","F -4 2 3        ",14,38, [ 0, 0,-6, 12,  6,  6],"     ") , &
           spgr_info_type(217,"I -4 3 M    ","I -4 2 3        ",14,38, [ 0, 0, 0, 12, 12, 12],"     ") , &
           spgr_info_type(218,"P -4 3 N    ","P -4n 2 3       ",14,38, [ 0, 0, 0, 12, 12, 12],"     ") , &
           spgr_info_type(219,"F -4 3 C    ","F -4c 2 3       ",14,38, [ 0, 0,-6, 12,  6,  6],"     ") , &
           spgr_info_type(220,"I -4 3 D    ","I -4bd 2c 3     ",14,38, [ 6, 6, 0, 12, 12, 12],"     ") , &
           spgr_info_type(221,"P M -3 M    ","-P 4 2 3        ",14,39, [ 0, 0, 0, 12, 12, 12],"     ") , &
           spgr_info_type(221,"P M 3 M     ","-P 4 2 3        ",14,39, [ 0, 0, 0, 12, 12, 12],"     ") , &
           spgr_info_type(222,"P N -3 N:1  ","P 4 2 3 -1n     ",14,39, [ 0, 0, 0, 12, 12, 12],"1    ") , &
           spgr_info_type(222,"P N -3 N    ","-P 4a 2bc 3     ",14,39, [ 6, 6, 6, 18, 18, 18],"2    ") , &
           spgr_info_type(222,"P N 3 N     ","-P 4a 2bc 3     ",14,39, [ 6, 6, 6, 18, 18, 18],"2    ") , &
           spgr_info_type(223,"P M -3 N    ","-P 4n 2 3       ",14,39, [ 0, 0, 0, 12, 12,  6],"     ") , &
           spgr_info_type(223,"P M 3 N     ","-P 4n 2 3       ",14,39, [ 0, 0, 0, 12, 12,  6],"     ") , &
           spgr_info_type(224,"P N -3 M:1  ","P 4n 2 3 -1n    ",14,39, [ 0, 0,-6, 12, 12,  6],"1    ") , &
           spgr_info_type(224,"P N -3 M    ","-P 4bc 2bc 3    ",14,39, [ 6, 6, 0, 18, 18, 12],"2    ") , &
           spgr_info_type(224,"P N 3 M     ","-P 4bc 2bc 3    ",14,39, [ 6, 6, 0, 18, 18, 12],"2    ") , &
           spgr_info_type(225,"F M -3 M    ","-F 4 2 3        ",14,39, [ 0, 0, 0, 12,  6,  6],"     ") , &
           spgr_info_type(225,"F M 3 M     ","-F 4 2 3        ",14,39, [ 0, 0, 0, 12,  6,  6],"     ") , &
           spgr_info_type(226,"F M -3 C    ","-F 4c 2 3       ",14,39, [ 0, 0, 0, 12,  6,  6],"     ") , &
           spgr_info_type(226,"F M 3 C     ","-F 4c 2 3       ",14,39, [ 0, 0, 0, 12,  6,  6],"     ") , &
           spgr_info_type(227,"F D -3 M:1  ","F 4d 2 3 -1d    ",14,39, [ 0, 0,-3, 12,  3,  3],"1    ") , &
           spgr_info_type(227,"F D -3 M    ","-F 4vw 2vw 3    ",14,39, [-3,-3,-6,  9,  0,  0],"2    ") , &
           spgr_info_type(227,"F D 3 M     ","-F 4vw 2vw 3    ",14,39, [-3,-3,-6,  9,  0,  0],"2    ") , &
           spgr_info_type(228,"F D -3 C:1  ","F 4d 2 3 -1cd   ",14,39, [ 0, 0,-3, 12,  3,  3],"1    ") , &
           spgr_info_type(228,"F D -3 C    ","-F 4cvw 2vw 3   ",14,39, [-3,-3,-6,  9,  0,  0],"2    ") , &
           spgr_info_type(228,"F D 3 C     ","-F 4cvw 2vw 3   ",14,39, [-3,-3,-6,  9,  0,  0],"2    ") , &
           spgr_info_type(229,"I M -3 M    ","-I 4 2 3        ",14,39, [ 0, 0, 0, 12, 12,  6],"     ") , &
           spgr_info_type(229,"I M 3 M     ","-I 4 2 3        ",14,39, [ 0, 0, 0, 12, 12,  6],"     ") , &
           spgr_info_type(230,"I A -3 D    ","-I 4bd 2c 3     ",14,39, [-3,-3, 0,  3,  3,  6],"     ") , &
           spgr_info_type(230,"I A 3 D     ","-I 4bd 2c 3     ",14,39, [-3,-3, 0,  3,  3,  6],"     ") ]
           Spgr_Info_loaded=.true.
   End Subroutine Set_Spgr_Info

   !!----
   !!---- SET_SYSTEM_EQUIV
   !!----
   !!----    Conversion Table    IT - ML - Kov - BC - Zak
   !!----
   !!--..   The information given in this file corresponds to that of TABLE 6 of
   !!--..   "Isotropy Subgroups of the 230 Crystallographic Space Groups", by
   !!--..   Harold T Stokes and Dorian M Hatch, World Scientific, Singapore (1988).
   !!--..
   !!--..   The transformation operators that take space group elements in the
   !!--..   International setting (International Tables of Crystallography, Hahn 1983)
   !!--..   to space-groups elements in the Miller and Love ( ML, 1967), Kovalev
   !!--..   (Kov,1986) Bradley anb Cracknell (BC, 1972) and Zak (Zak, 1969) settings.
   !!--..
   !!--..   In the international setting the basis vectors are always those of the
   !!--..   conventional unit cell. In the Trigonal system the primitive basis
   !!--..   vectors are in an obverse relationship given by (2/3 1/3 1/3),
   !!--..   (-1/3 1/3 1/3) and (-1/3, -2/3 1/3).
   !!--..   In ML the same basis vectors are chosen except that for trigonal/rhombohedral
   !!--..   system the reverse setting is adopted, so the primitive basis vectors
   !!--..   are: t1=(1/3 -1/3 1/3), t2=(1/3, 2/3 1/3) and t3=(2/3 1/3 1/3)
   !!--..   In Kovalev the a,b,c axes of the coordinate system are along the
   !!--..   conventional basis vectors of the lattice, however in the trigonal
   !!--..   system an hexagonal system is chosen so that the primitive basis vectors
   !!--..   are a1=(-1 -1 1/3), a2=(1 0 1/3) and a3=(0 1 1/3).
   !!--..   In the setting of BC the axes a,b,c of the coordinate system are chosen
   !!--..   to be the primitive basis vectors t1,t2,t3 as defined in their book.
   !!--..   The setting of Zak the basis vectors are as in the international setting,
   !!--..   but for trigonal/rhombohedral system the primitive basis vectors w.r.t. the selected
   !!--..   hexagonal coordinate system are given by: (1/3 2/3 1) (1/3 -1/3 1)
   !!--..   (-2/3 -1/3 1)
   !!--..
   !!--..   Symmetry and transformation operators of Space Groups can be given as
   !!--..   4 x 4 Seitz matrices or as a character string called Jones Faithful
   !!--..   representation. This last representation is that used in this file.
   !!--..
   !!--..   To transform a symmetry operator "gI" in the international setting into
   !!--..   a symmetry element "g" in one of the other settings, we simply perform
   !!--..   the following operation:  g = gT gI gT(-1), where gT is the transformation
   !!--..   given tabulated below.
   !!----
   !!---- 23/04/2019
   !!
   Module Subroutine Set_System_Equiv()
      if(System_Equiv_loaded) return
      if (.not. allocated(system_equiv) ) allocate(system_equiv(230))

      system_equiv(1:10) = [         &
         table_equiv_type("C1_1  ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("C1_i  ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("C1_2  ","z,x,y            ","-z,x,-y           ",        &
                     "-x,z,y                          "," z,x,y            "), &
         table_equiv_type("C2_2  ","z,x,y            ","-z,x,-y           ",        &
                     "-x,z,y                          "," z,x,y            "), &
         table_equiv_type("C3_2  ","z,x,y            ","-z,x,-y           ",        &
                     " z,-x+y,-x-y                    ","-x,z,y            "), &
         table_equiv_type("C1_s  ","z,x,y            ","-z,x,-y           ",        &
                     "-x,z,y                          "," z,x,y            "), &
         table_equiv_type("C2_s  ","z,x,y            ","-z,x,-y           ",        &
                     " z,-x,-y                        "," z,x,y            "), &
         table_equiv_type("C3_s  ","z,x,y            ","-z,x,-y           ",        &
                     " z,-x+y,-x-y                    ","-x,z,y            "), &
         table_equiv_type("C4_s  ","z,x,y            ","-z,x,-y           ",        &
                     " z,-x+y,-x-y                    ","-x,z,y            "), &
         table_equiv_type("C1_2h ","z,x,y            ","-z,x,-y           ",        &
                     "-x,z,y                          "," z,x,y            ") ]

      system_equiv(11:20)= [         &
         table_equiv_type("C2_2h ","z,x,y            ","-z,x,-y+1/4       ",        &
                     "-x,z,y+1/4                      "," z,x,y            "), &
         table_equiv_type("C3_2h ","z,x,y            ","-z,x,-y           ",        &
                     " z,-x+y,-x-y                    ","-x,z,y            "), &
         table_equiv_type("C4_2h ","z,x,y            ","-z+1/4,x,-y       ",        &
                     " z-1/4,-x,-y                    ","-x,z,y            "), &
         table_equiv_type("C5_2h ","z,x,y            ","-z+1/4,x,-y+1/4   ",        &
                     " z-1/4,-x,-y+1/4                ","-x,z,y            "), &
         table_equiv_type("C6_2h ","z,x,y            ","-z+1/4,x,-y       ",        &
                     " z-1/4,-x+y,-x-y                ","-x,z,y            "), &
         table_equiv_type("D1_2  ","x,y,z            "," x,y,z            ",        &
                     "-y,x,z                          "," x,y,z            "), &
         table_equiv_type("D2_2  ","x,y,z            "," x,y,z            ",        &
                     "-y,x,z+1/4                      "," x,y,z            "), &
         table_equiv_type("D3_2  ","x,y,z            "," x,y,z            ",        &
                     "-y,x,z                          "," x,y,z            "), &
         table_equiv_type("D4_2  ","x,y,z            "," x,y,z            ",        &
                     "-x,-y,z                         "," x,y,z            "), &
         table_equiv_type("D5_2  ","x,y,z            "," x,y,z+1/4        ",        &
                     " x-y,x+y,z                      "," x,y,z            ")]

      system_equiv(21:30)= [         &
         table_equiv_type("D6_2  ","x,y,z            "," x,y,z            ",        &
                     " x-y,x+y,z                      "," x,y,z            "), &
         table_equiv_type("D7_2  ","x,y,z            "," x,y,z            ",        &
                     " x+y+z,-x-y+z,x-y-z             "," x,y,z            "), &
         table_equiv_type("D8_2  ","x,y,z            "," x,y,z            ",        &
                     " x+z,-y+z,x-y                   "," x,y,z            "), &
         table_equiv_type("D9_2  ","x,y,z            "," x,y,z            ",        &
                     "-y+z,-x+z,-x-y                  "," x,y,z            "), &
         table_equiv_type("C1_2v ","x,y,z            "," x,y,z            ",        &
                     "-y,x,z                          "," x,y,z            "), &
         table_equiv_type("C2_2v ","x,y,z            "," y,x,z            ",        &
                     "-y,x,z                          "," x,y,z            "), &
         table_equiv_type("C3_2v ","x,y,z            "," x,y,z            ",        &
                     "-y,x,z                          "," x,y,z            "), &
         table_equiv_type("C4_2v ","x,y,z            "," x+1/4,y,z        ",        &
                     "-x-1/4,-y,z                     "," x,y,z            "), &
         table_equiv_type("C5_2v ","x,y,z            "," x+1/4,y,z        ",        &
                     "-x-1/4,-y,z                     "," x,y,z            "), &
         table_equiv_type("C6_2v ","x,y,z            "," y+1/4,x,z        ",        &
                     "-y-1/4,x,z                      "," x,y,z            ") ]

      system_equiv(31:40)= [         &
         table_equiv_type("C7_2v ","x,y,z            "," x,y,z            ",        &
                     "-x,-y,z                         "," x,y,z            "), &
         table_equiv_type("C8_2v ","x,y,z            "," x+1/4,y+1/4,z    ",        &
                     "-y-1/4,x+1/4,z                  "," x,y,z            "), &
         table_equiv_type("C9_2v ","x,y,z            "," x+1/4,y+1/4,z    ",        &
                     "-x-1/4,-y+1/4,z                 "," x,y,z            "), &
         table_equiv_type("C10_2v","x,y,z            "," x+1/4,y+1/4,z    ",        &
                     "-y-1/4,x+1/4,z                  "," x,y,z            "), &
         table_equiv_type("C11_2v","x,y,z            "," x,y,z            ",        &
                     " x-y,x+y,z                      "," x,y,z            "), &
         table_equiv_type("C12_2v","x,y,z            "," x,y,z            ",        &
                     "-x-y,x-y,z                      "," x,y,z            "), &
         table_equiv_type("C13_2v","x,y,z            "," x,y,z            ",        &
                     " x-y,x+y,z                      "," x,y,z            "), &
         table_equiv_type("C14_2v","-z,y,x           "," -z,y,x           ",        &
                     "-y+z,-y-z,x                     ","-y,-z,x           "), &
         table_equiv_type("C15_2v","-z,y,x           "," -z,y,x           ",        &
                     "-y+z,-y-z,x                     ","-y,-z,x           "), &
         table_equiv_type("C16_2v","-z,y,x           "," -z,y,x           ",        &
                     "-y+z,-y-z,x                     ","-y,-z,x           ") ]

      system_equiv(41:50)= [         &
         table_equiv_type("C17_2v","-z,y,x           "," -z,y,x           ",        &
                     "-y+z,-y-z,x                     ","-y,-z,x           "), &
         table_equiv_type("C18_2v","x,y,z            "," x,y,z            ",        &
                     " x+y+z,-x-y+z,x-y-z             "," x,y,z            "), &
         table_equiv_type("C19_2v","x,y,z            "," x-1/8,y-1/8,z    ",        &
                     " x+y+z+1/2,-x-y+z-1/2,x-y-z-1/4 "," x,y,z            "), &
         table_equiv_type("C20_2v","x,y,z            "," x,y,z            ",        &
                     " x+z,-y+z,x-y                   "," x,y,z            "), &
         table_equiv_type("C21_2v","x,y,z            "," x,y,z            ",        &
                     " x+z,-y+z,x-y                   "," x,y,z            "), &
         table_equiv_type("C22_2v","x,y,z            "," x,y,z            ",        &
                     "-y+z,-x+z,-x-y                  "," x,y,z            "), &
         table_equiv_type("D1_2h ","x,y,z            "," x,y,z            ",        &
                     "-y,x,z                          "," x,y,z            "), &
         table_equiv_type("D2_2h ","x-1/4,y-1/4,z-1/4"," x-1/4,y-1/4,z-1/4",        &
                     "-y+1/4,x-1/4,z-1/4              "," x-1/4,y-1/4,z-1/4"), &
         table_equiv_type("D3_2h ","x,y,z            "," x,y,z+1/4        ",        &
                     "-y,x,z+1/4                      "," x,y,z            "), &
         table_equiv_type("D4_2h ","x-1/4,y-1/4,z    "," x-1/4,y-1/4,z    ",        &
                     "-y+1/4,x-1/4,z                  "," x-1/4,y-1/4,z    ") ]

      system_equiv(51:60)= [         &
         table_equiv_type("D5_2h ","x,y,z            "," y,z,x            ",        &
                     "-y,z,-x                         "," x,y,z            "), &
         table_equiv_type("D6_2h ","x,y,z            "," z+1/4,x+1/4,y    ",        &
                     " z-1/4,x+1/4,y                  "," x,y,z            "), &
         table_equiv_type("D7_2h ","x,y,z            "," x-1/4,y,z        ",        &
                     "-x-1/4,-y,z                     "," x,y,z            "), &
         table_equiv_type("D8_2h ","x,y,z            "," y,z+1/4,x        ",        &
                     "-y,z+1/4,-x                     "," x,y,z            "), &
         table_equiv_type("D9_2h ","x,y,z            "," x,y,z            ",        &
                     "-y,x,z                          "," x,y,z            "), &
         table_equiv_type("D10_2h","x,y,z            "," x+1/4,y+1/4,z+1/4",        &
                     "-y-1/4,x+1/4,z+1/4              "," x,y,z            "), &
         table_equiv_type("D11_2h","x,y,z            "," -z,-y-1/4,-x     ",        &
                     "-z,y+1/4,x                      "," x,y,z            "), &
         table_equiv_type("D12_2h","x,y,z            "," x,y,z-1/4        ",        &
                     "-y,x,z+1/4                      "," x,y,z            "), &
         table_equiv_type("D13_2h","x-1/4,y-1/4,z    "," x-1/4,y-1/4,z    ",        &
                     "-y+1/4,x-1/4,z                  "," x-1/4,y-1/4,z+1/4"), &
         table_equiv_type("D14_2h","x,y,z            "," z+1/4,x,y+1/4    ",        &
                     " z-1/4,x,y+1/4                  "," x,y,z            ") ]

      system_equiv(61:70)= [         &
         table_equiv_type("D15_2h","x,y,z            "," x,y,z            ",        &
                     "-x,-y,z                         "," x,y,z            "), &
         table_equiv_type("D16_2h","x,y,z            "," y+1/4,x+1/4,z    ",        &
                     "-y-1/4,x+1/4,z                  "," x,y,z            "), &
         table_equiv_type("D17_2h","x,y,z            "," y,x,z            ",        &
                     " x-y,x+y,z                      "," x,y,z            "), &
         table_equiv_type("D18_2h","x,y,z            "," y,x+1/4,z        ",        &
                     " x-y+1/4,x+y+1/4,z              "," x,y,z            "), &
         table_equiv_type("D19_2h","x,y,z            "," x,y,z            ",        &
                     " x-y,x+y,z                      "," x,y,z            "), &
         table_equiv_type("D20_2h","x,y,z            "," x,y,z+1/4        ",        &
                     " x-y,x+y,z+1/4                  "," x,y,z            "), &
         table_equiv_type("D21_2h","x,y,z            "," x+1/4,y,z        ",        &
                     " x-y+1/4,x+y+1/4,z              "," x,y,z            "), &
         table_equiv_type("D22_2h","x,y-1/4,z-1/4    "," x,y-1/4,z-1/4    ",        &
                     " x-y+1/4,x+y-1/4,z-1/4          "," x,y-1/4,z-1/4    "), &
         table_equiv_type("D23_2h","x,y,z            "," x,y,z            ",        &
                     " x+y+z,-x-y+z,x-y-z             "," x,y,z            "), &
         table_equiv_type("D24_2h","x-7/8,y-7/8,z-7/8"," x-7/8,y-7/8,z-7/8",        &
                     " x+y+z-15/8,-x-y+z+5/8,x-y-z+5/8"," x-7/8,y-7/8,z-7/8") ]

      system_equiv(71:80)= [         &
         table_equiv_type("D25_2h","x,y,z            "," x,y,z            ",        &
                     " x+z,-y+z,x-y                   "," x,y,z            "), &
         table_equiv_type("D26_2h","x,y,z            "," x,y,z-1/4        ",        &
                     " x+z+1/4,-y+z+1/4,x-y           "," x,y,z            "), &
         table_equiv_type("D27_2h","x,y,z            "," x,y,z            ",        &
                     " x+z+1/2,-y+z,x-y               "," x,y,z            "), &
         table_equiv_type("D28_2h","x,y,z            "," x,y,z+1/4        ",        &
                     " x+z+1/4,-y+z-1/4,x-y           "," x,y,z            "), &
         table_equiv_type("C1_4  ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("C2_4  ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("C3_4  ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("C4_4  ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("C5_4  ","x,y,z            "," x,y,z            ",        &
                     " y+z,x+z,x+y                    "," x,y,z            "), &
         table_equiv_type("C6_4  ","x,y,z            "," x,y,z            ",        &
                     " y+z,x+z,x+y                    "," x,y,z            ") ]

      system_equiv(81:90)= [         &
         table_equiv_type("S1_4  ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("S2_4  ","x,y,z            "," x,y,z            ",        &
                     " y+z,x+z,x+y                    "," x,y,z            "), &
         table_equiv_type("C1_4h ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("C2_4h ","x,y,z            "," x,y,z+1/4        ",        &
                     " x,y,z+1/4                      "," x,y,z            "), &
         table_equiv_type("C3_4h ","x-3/4,y-1/4,z    "," x-3/4,y-1/4,z    ",        &
                     " x-3/4,y-1/4,z                  "," x-3/4,y-1/4,z    "), &
         table_equiv_type("C4_4h ","x-3/4,y-3/4,z-3/4"," x-3/4,y-3/4,z-3/4",        &
                     " x-3/4,y-3/4,z-3/4              "," x-3/4,y-3/4,z-3/4"), &
         table_equiv_type("C5_4h ","x,y,z            "," x,y,z            ",        &
                     " y+z,x+z,x+y                    "," x,y,z            "), &
         table_equiv_type("C6_4h ","x,y-3/4,z-7/8    "," x,y-3/4,z-7/8    ",        &
                     " y+z-13/8,x+z-7/8,x+y-3/4       "," x,y-3/4,z-7/8    "), &
         table_equiv_type("D1_4  ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("D2_4  ","x,y,z            "," x,y-1/2,z        ",        &
                     " x+1/2,y,z                      "," x,y,z            ") ]

      system_equiv(91:100) = [         &
         table_equiv_type("D3_4  ","x,y,z            "," x,y,z+1/4        ",        &
                     " x,y,z+1/4                      "," x,y,z            "), &
         table_equiv_type("D4_4  ","x,y,z            "," x,y-1/2,z+1/8    ",        &
                     " x+1/2,y,z+1/8                  "," x,y,z            "), &
         table_equiv_type("D5_4  ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("D6_4  ","x,y,z            "," x,y+1/2,z+1/4    ",        &
                     " x+1/2,y,z+1/4                  "," x,y,z            "), &
         table_equiv_type("D7_4  ","x,y,z            "," x,y,z+1/4        ",        &
                     " x,y,z+1/4                      "," x,y,z            "), &
         table_equiv_type("D8_4  ","x,y,z            "," x,y-1/2,z-1/8    ",        &
                     " x+1/2,y,z+3/8                  "," x,y,z            "), &
         table_equiv_type("D9_4  ","x,y,z            "," x,y,z            ",        &
                     " y+z,x+z,x+y                    "," x,y,z            "), &
         table_equiv_type("D10_4 ","x,y,z            "," x,y,z            ",        &
                     " y+z+1/8,x+z+1/8,x+y            "," x,y,z            "), &
         table_equiv_type("C1_4v ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("C2_4v ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            ") ]

      system_equiv(101:110)= [         &
         table_equiv_type("C3_4v ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("C4_4v ","x,y,z            "," x,y-1/2,z        ",        &
                     " x+1/2,y,z                      "," x,y,z            "), &
         table_equiv_type("C5_4v ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("C6_4v ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("C7_4v ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("C8_4v ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("C9_4v ","x,y,z            "," x,y,z            ",        &
                     " y+z,x+z,x+y                    "," x,y,z            "), &
         table_equiv_type("C10_4v","x,y,z            "," x,y,z            ",        &
                     " y+z,x+z,x+y                    "," x,y,z            "), &
         table_equiv_type("C11_4v","x,y,z            "," x,y,z            ",        &
                     " y+z,x+z,x+y                    "," x,y,z            "), &
         table_equiv_type("C12_4v","x,y,z            "," x,y,z            ",        &
                     " y+z,x+z,x+y                    "," x,y,z            ") ]

      system_equiv(111:120)= [         &
         table_equiv_type("D1_2d ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("D2_2d ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("D3_2d ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("D4_2d ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("D5_2d ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("D6_2d ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("D7_2d ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("D8_2d ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("D9_2d ","x,y,z            "," x,y,z            ",        &
                     " y+z,x+z,x+y                    "," x,y,z            "), &
         table_equiv_type("D10_2d","x,y,z            "," x,y,z            ",        &
                     " y+z,x+z,x+y                    "," x,y,z            ")  ]

      system_equiv(121:130)= [         &
         table_equiv_type("D11_2d","x,y,z            "," x,y,z            ",        &
                     " y+z,x+z,x+y                    "," x,y,z            "), &
         table_equiv_type("D12_2d","x,y,z            "," x+1/2,y,z+1/4    ",        &
                     " x+z,-y+z,x-y                   "," x,y,z            "), &
         table_equiv_type("D1_4h ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("D2_4h ","x,y,z            "," x,y,z-1/4        ",        &
                     " x,y,z+1/4                      "," x,y,z            "), &
         table_equiv_type("D3_4h ","x-3/4,y-3/4,z    "," x-3/4,y-3/4,z    ",        &
                     " x-1/4,y-3/4,z                  "," x-3/4,y-3/4,z    "), &
         table_equiv_type("D4_4h ","x-3/4,y-3/4,z-3/4"," x-3/4,y-3/4,z-3/4",        &
                     " x-1/4,y-3/4,z-3/4              "," x-3/4,y-3/4,z-3/4"), &
         table_equiv_type("D5_4h ","x,y,z            "," x,y,z            ",        &
                     " x+1/2,y,z                      "," x,y,z            "), &
         table_equiv_type("D6_4h ","x,y,z            "," x,y,z+1/4        ",        &
                     " x+1/2,y,z+1/4                  "," x,y,z            "), &
         table_equiv_type("D7_4h ","x-3/4,y-1/4,z    "," x-3/4,y+1/4,z    ",        &
                     " x-1/4,y-1/4,z                  "," x-3/4,y-1/4,z    "), &
         table_equiv_type("D8_4h ","x-3/4,y-1/4,z    "," x-3/4,y+1/4,z+1/4",        &
                     " x-1/4,y-1/4,z+1/4              "," x,y,z            ") ]

      system_equiv(131:140)= [         &
         table_equiv_type("D9_4d ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("D10_4d","x,y,z            "," x,y,z-1/4        ",        &
                     " x,y,z+1/4                      "," x,y,z            "), &
         table_equiv_type("D11_4d","x-3/4,y-1/4,z-3/4"," x-3/4,y+1/4,z-1/2",        &
                     " x-3/4,y-1/4,z-1/2              "," x,y,z            "), &
         table_equiv_type("D12_4d","x-3/4,y-1/4,z-3/4"," x-3/4,y+1/4,z-3/4",        &
                     " x-3/4,y-1/4,z-3/4              "," x-3/4,y-1/4,z-3/4"), &
         table_equiv_type("D13_4d","x,y,z            "," x,y,z            ",        &
                     " x+1/2,y,z                      "," x,y,z            "), &
         table_equiv_type("D14_4d","x,y,z            "," x,y+1/2,z+1/4    ",        &
                     " x,y,z+1/4                      "," x+1/2,y,z        "), &
         table_equiv_type("D15_4d","x-3/4,y-1/4,z-3/4"," x-3/4,y+1/4,z-1/2",        &
                     " x-1/4,y-1/4,z-1/2              "," x,y,z            "), &
         table_equiv_type("D16_4d","x-3/4,y-1/4,z-3/4"," x-3/4,y+1/4,z-3/4",        &
                     " x-1/4,y-1/4,z-3/4              "," x,y,z            "), &
         table_equiv_type("D17_4d","x,y,z            "," x,y,z            ",        &
                     " y+z,x+z,x+y                    "," x,y,z            "), &
         table_equiv_type("D18_4d","x,y,z            "," x,y,z+1/4        ",        &
                     " y+z+1/4,x+z+3/4,x+y+1/2        "," x,y,z            ") ]

      system_equiv(141:150)= [         &
         table_equiv_type("D19_4d","x,y-1/4,z-7/8    "," x,y-1/4,z-7/8    ",        &
                     " y+z-3/4,x+z-3/4,x+y            "," x,y-1/4,z-7/8    "), &
         table_equiv_type("D20_4d","x,y-1/4,z-7/8    "," x,y-1/4,z-9/8    ",        &
                     " y+z,x+z-1/2,x+y+1/2            "," x,y,z            "), &
         table_equiv_type("C1_3  ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("C2_3  ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("C3_3  ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("C4_3  ","y,-x+y,z         ","-2x+y,-x-y,z      ",        &
                     " x+z,-x+y+z,-y+z                "," y,-x+y,3z        "), &
         table_equiv_type("C1_3i ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("C2_3i ","y,-x+y,z         ","-2x+y,-x-y,z      ",        &
                     " x+z,-x+y+z,-y+z                "," y,-x+y,3z        "), &
         table_equiv_type("D1_3  ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("D2_3  ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            ") ]

      system_equiv(151:160)= [         &
         table_equiv_type("D3_3  ","x,y,z-1/6        "," x,y,z+1/6        ",        &
                     " x,y,z+1/6                      "," x,y,z+1/6        "), &
         table_equiv_type("D4_3  ","x,y,z            "," x,y,z+1/3        ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("D5_3  ","x,y,z-5/6        "," x,y,z+1/3        ",        &
                     " x,y,z-1/6                      "," x,y,z-1/6        "), &
         table_equiv_type("D6_3  ","x,y,z-1/6        "," x,y,z+1/6        ",        &
                     " x,y,z+1/2                      "," x,y,z+1/2        "), &
         table_equiv_type("D7_3  ","y,-x+y,z         ","-2x+y,-x-y,z      ",        &
                     " x+z,-x+y+z,-y+z                "," y,-x+y,3z        "), &
         table_equiv_type("C1_3v ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("C2_3v ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("C3_3v ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("C4_3v ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("C5_3v ","y,-x+y,z         ","-2x+y,-x-y,z      ",        &
                     " x+z,-x+y+z,-y+z                "," y,-x+y,3z        ")]

      system_equiv(161:170)= [         &
         table_equiv_type("C6_3v ","y,-x+y,z         ","-2x+y,-x-y,z      ",        &
                     " x+z,-x+y+z,-y+z                "," y,-x+y,3z        "), &
         table_equiv_type("D1_3d ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("D2_3d ","x,y,z            "," x,y,z+1/4        ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("D3_3d ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("D4_3d ","x,y,z            "," x,y,z+1/4        ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("D5_3d ","y,-x+y,z         ","-2x+y,-x-y,z      ",        &
                     " x+z,-x+y+z,-y+z                "," y,-x+y,3z        "), &
         table_equiv_type("D6_3d ","y,-x+y,z         ","-2x+y,-x-y,z      ",        &
                     " x+z,-x+y+z,-y+z                "," y,-x+y,3z        "), &
         table_equiv_type("C1_6  ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("C2_6  ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("C3_6  ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            ") ]

      system_equiv(171:180)= [         &
         table_equiv_type("C4_6  ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("C5_6  ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("C6_6  ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("C7_6  ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("C1_6h ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("C2_6h ","x,y,z            "," x,y,z            ",        &
                     " x,y,z+1/4                      "," x,y,z            "), &
         table_equiv_type("D1_6  ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("D2_6  ","x,y,z            "," x,y,z+1/6        ",        &
                     " x,y,z+1/4                      "," x,y,z            "), &
         table_equiv_type("D3_6  ","x,y,z            "," x,y,z+1/3        ",        &
                     " x,y,z+1/4                      "," x,y,z            "), &
         table_equiv_type("D4_6  ","x,y,z            "," x,y,z+1/3        ",        &
                     " x,y,z                          "," x,y,z            ") ]

      system_equiv(181:190)= [         &
         table_equiv_type("D5_6  ","x,y,z            "," x,y,z-1/3        ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("D6_6  ","x,y,z            "," x,y,z            ",        &
                     " x,y,z+1/4                      "," x,y,z            "), &
         table_equiv_type("C1_6v ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("C2_6v ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("C3_6v ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("C4_6v ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("D1_3h ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("D2_3h ","x,y,z            "," x,y,z            ",        &
                     " x,y,z+1/4                      "," x,y,z            "), &
         table_equiv_type("D3_3h ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("D4_3h ","x,y,z            "," x,y,z            ",        &
                     " x,y,z+1/4                      "," x,y,z            ") ]

      system_equiv(191:200)= [         &
         table_equiv_type("D1_6h ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("D2_6h ","x,y,z            "," x,y,z-1/4        ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("D3_6h ","x,y,z            "," x,y,z-1/4        ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("D4_6h ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("T1    ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("T2    ","x,y,z            "," x,y,z            ",        &
                     "-x+y+z,x-y+z,x+y-z              "," x,y,z            "), &
         table_equiv_type("T3    ","x,y,z            "," x,y,z            ",        &
                     " y+z,x+z,x+y                    "," x,y,z            "), &
         table_equiv_type("T4    ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("T5    ","x,y,z            "," x,y,z            ",        &
                     " y+z,x+z,x+y                    "," x,y,z            "), &
         table_equiv_type("T1_h  ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            ") ]

      system_equiv(201:210)= [         &
         table_equiv_type("T2_h  ","x-3/4,y-3/4,z-3/4"," x-3/4,y-3/4,z-3/4",        &
                     " x-3/4,y-3/4,z-3/4              "," x-3/4,y-3/4,z-3/4"), &
         table_equiv_type("T3_h  ","x,y,z            "," x,y,z            ",        &
                     "-x+y+z,x-y+z,x+y-z              "," x,y,z            "), &
         table_equiv_type("T4_h  ","x-7/8,y-7/8,z-7/8"," x-7/8,y-7/8,z-7/8",        &
                     "-x+y+z-7/8,x-y+z-7/8,x+y-z-7/8  "," x-7/8,y-7/8,z-7/8"), &
         table_equiv_type("T5_h  ","x,y,z            "," x,y,z            ",        &
                     " y+z,x+z,x+y                    "," x,y,z            "), &
         table_equiv_type("T6_h  ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("T7_h  ","x,y,z            "," x,y,z            ",        &
                     " y+z,x+z,x+y                    "," x,y,z            "), &
         table_equiv_type("O1    ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("O2    ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("O3    ","x,y,z            "," x,y,z            ",        &
                     "-x+y+z,x-y+z,x+y-z              "," x,y,z            "), &
         table_equiv_type("O4    ","x,y,z            "," x,y,z            ",        &
                     "-x+y+z,x-y+z,x+y-z              "," x,y,z            ") ]

      system_equiv(211:220)= [         &
         table_equiv_type("O5    ","x,y,z            "," x,y,z            ",        &
                     " y+z,x+z,x+y                    "," x,y,z            "), &
         table_equiv_type("O6    ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("O7    ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("O8    ","x,y,z            "," x,y,z            ",        &
                     " y+z,x+z,x+y                    "," x,y,z            "), &
         table_equiv_type("T1_d  ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("T2_d  ","x,y,z            "," x,y,z            ",        &
                     "-x+y+z,x-y+z,x+y-z              "," x,y,z            "), &
         table_equiv_type("T3_d  ","x,y,z            "," x,y,z            ",        &
                     " y+z,x+z,x+y                    "," x,y,z            "), &
         table_equiv_type("T4_d  ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("T5_d  ","x,y,z            "," x,y,z            ",        &
                     "-x+y+z,x-y+z,x+y-z              "," x,y,z            "), &
         table_equiv_type("T6_d  ","x,y,z            "," x,y,z            ",        &
                     " y+z,x+z,x+y                    "," x,y,z            ") ]

      system_equiv(221:230)= [         &
         table_equiv_type("O1_h  ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("O2_h  ","x-3/4,y-3/4,z-3/4"," x-3/4,y-3/4,z-3/4",        &
                     " x-3/4,y-3/4,z-3/4              "," x-3/4,y-3/4,z-3/4"), &
         table_equiv_type("O3_h  ","x,y,z            "," x,y,z            ",        &
                     " x,y,z                          "," x,y,z            "), &
         table_equiv_type("O4_h  ","x-3/4,y-3/4,z-3/4"," x-3/4,y-3/4,z-3/4",        &
                     " x-3/4,y-3/4,z-3/4              "," x-3/4,y-3/4,z-3/4"), &
         table_equiv_type("O5_h  ","x,y,z            "," x,y,z            ",        &
                     "-x+y+z,x-y+z,x+y-z              "," x,y,z            "), &
         table_equiv_type("O6_h  ","x,y,z            "," x-1/4,y-1/4,z-1/4",        &
                     "-x+y+z+1/4,x-y+z+1/4,x+y-z+1/4  "," x,y,z            "), &
         table_equiv_type("O7_h  ","x,y,z            "," x+1/8,y+1/8,z+1/8",        &
                     "-x+y+z+1/8,x-y+z+1/8,x+y-z+1/8  "," x+1/8,y+1/8,z+1/8"), &
         table_equiv_type("O8_h  ","x,y,z            "," x+3/8,y+3/8,z+3/8",        &
                     "-x+y+z+3/8,x-y+z+3/8,x+y-z+3/8  "," x+3/8,y+3/8,z+3/8"), &
         table_equiv_type("O9_h  ","x,y,z            "," x,y,z            ",        &
                     " y+z,x+z,x+y                    "," x,y,z            "), &
         table_equiv_type("O10_h ","x,y,z            "," x,y,z            ",        &
                     " y+z,x+z,x+y                    "," x,y,z            ") ]

      System_Equiv_loaded=.true.
   End Subroutine Set_System_Equiv

   !!----
   !!---- Set_Wyckoff_Info()
   !!----
   !!----    Set Information on Wyckoff_info array
   !!----
   !!---- 23/04/2019
   !!
   Module Subroutine Set_Wyckoff_Info()
      if(Wyckoff_Info_loaded) return
      if (.not. allocated(wyckoff_info) ) allocate(wyckoff_info(273) )

      wyckoff_info(  1)= wyck_info_type("P 1         ", 0,     &
                   ["               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(  2)= wyck_info_type("P -1        ", 8,     &
                   ["0,0,0          ", "0,0,1/2        ", "0,1/2,0        ",    &
                     "1/2,0,0        ", "1/2,1/2,0      ", "1/2,0,1/2      ",    &
                     "0,1/2,1/2      ", "1/2,1/2,1/2    ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(  3)= wyck_info_type("P 2         ", 4,     &
                   ["0,y,0          ", "0,y,1/2        ", "1/2,y,0        ",    &
                     "1/2,y,1/2      ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(  4)= wyck_info_type("P 21        ", 0,     &
                   ["               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(  5)= wyck_info_type("C 2         ", 2,     &
                   ["0,y,0          ", "0,y,1/2        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(  6)= wyck_info_type("A 2         ", 2,     &
                   ["0,y,0          ", "1/2,y,1/2      ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(  7)= wyck_info_type("I 2         ", 2,     &
                   ["0,y,0          ", "1/2,y,0        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(  8)= wyck_info_type("P M         ", 2,     &
                   ["x,0,z          ", "x,1/2,z        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(  9)= wyck_info_type("P C         ", 0,     &
                   ["               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 10)= wyck_info_type("C M         ", 1,     &
                   ["x,0,z          ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 11)= wyck_info_type("A M         ", 1,     &
                   ["x,0,z          ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 12)= wyck_info_type("I M         ", 1,     &
                   ["x,0,z          ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 13)= wyck_info_type("C C         ", 0,     &
                   ["               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 14)= wyck_info_type("P 2/M       ",14,     &
                   ["0,0,0          ", "0,1/2,0        ", "0,0,1/2        ",    &
                     "1/2,0,0        ", "1/2,1/2,0      ", "0,1/2,1/2      ",    &
                     "1/2,0,1/2      ", "1/2,1/2,1/2    ", "0,y,0          ",    &
                     "1/2,y,0        ", "0,y,1/2        ", "1/2,y,1/2      ",    &
                     "x,0,z          ", "x,1/2,z        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 15)= wyck_info_type("P 21/M      ", 5,     &
                   ["0,0,0          ", "1/2,0,0        ", "0,0,1/2        ",    &
                     "1/2,0,1/2      ", "x,1/4,z        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 16)= wyck_info_type("C 2/M       ", 9,     &
                   ["0,0,0          ", "0,1/2,0        ", "0,0,1/2        ",    &
                     "0,1/2,1/2      ", "1/4,1/4,0      ", "1/4,1/4,1/2    ",    &
                     "0,y,0          ", "0,y,1/2        ", "x,0,z          ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 17)= wyck_info_type("A 2/M       ", 9,     &
                   ["0,0,0          ", "0,1/2,0        ", "1/2,0,1/2      ",    &
                     "1/2,1/2,1/2    ", "0,1/4,1/4      ", "1/2,1/4,3/4    ",    &
                     "0,y,0          ", "1/2,y,1/2      ", "x,0,z          ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 18)= wyck_info_type("I 2/M       ", 9,     &
                   ["0,0,0          ", "0,1/2,0        ", "1/2,0,0        ",    &
                     "1/2,1/2,0      ", "3/4,1/4,3/4    ", "1/4,1/4,3/4    ",    &
                     "0,y,0          ", "1/2,y,0        ", "x,0,z          ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 19)= wyck_info_type("P 2/C       ", 6,     &
                   ["0,0,0          ", "1/2,1/2,0      ", "0,1/2,0        ",    &
                     "1/2,0,0        ", "0,y,1/4        ", "1/2,y,1/4      ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 20)= wyck_info_type("P 2/N       ", 6,     &
                   ["0,0,0          ", "0,1/2,1/2      ", "0,1/2,0        ",    &
                     "0,0,1/2        ", "3/4,y,3/4      ", "3/4,y,1/4      ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 21)= wyck_info_type("P 2/A       ", 6,     &
                   ["0,0,0          ", "1/2,1/2,1/2    ", "0,1/2,0        ",    &
                     "1/2,0,1/2      ", "1/4,y,0        ", "3/4,y,1/2      ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 22)= wyck_info_type("P 21/C      ", 4,     &
                   ["0,0,0          ", "1/2,0,0        ", "0,0,1/2        ",    &
                     "1/2,0,1/2      ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 23)= wyck_info_type("P 21/N      ", 4,     &
                   ["0,0,0          ", "0,0,1/2        ", "1/2,0,1/2      ",    &
                     "1/2,0,0        ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 24)= wyck_info_type("P 21/A      ", 4,     &
                   ["0,0,0          ", "1/2,0,1/2      ", "1/2,0,0        ",    &
                     "0,0,1/2        ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 25)= wyck_info_type("C 2/C       ", 5,     &
                   ["0,0,0          ", "0,1/2,0        ", "1/4,1/4,0      ",    &
                     "1/4,1/4,1/2    ", "0,y,1/4        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 26)= wyck_info_type("A 2/N       ", 5,     &
                   ["0,0,0          ", "0,1/2,0        ", "0,1/4,1/4      ",    &
                     "1/2,1/4,3/4    ", "3/4,y,3/4      ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 27)= wyck_info_type("I 2/A       ", 5,     &
                   ["0,0,0          ", "0,1/2,0        ", "3/4,1/4,3/4    ",    &
                     "1/4,1/4,3/4    ", "1/4,y,0        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 28)= wyck_info_type("P 2 2 2     ",20,     &
                   ["0,0,0          ", "1/2,0,0        ", "0,1/2,0        ",    &
                     "0,0,1/2        ", "1/2,1/2,0      ", "1/2,0,1/2      ",    &
                     "0,1/2,1/2      ", "1/2,1/2,1/2    ", "x,0,0          ",    &
                     "x,0,1/2        ", "x,1/2,0        ", "x,1/2,1/2      ",    &
                     "0,y,0          ", "0,y,1/2        ", "1/2,y,0        ",    &
                     "1/2,y,1/2      ", "0,0,z          ", "1/2,0,z        ",    &
                     "0,1/2,z        ", "1/2,1/2,z      ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 29)= wyck_info_type("P 2 2 21    ", 4,     &
                   ["x,0,0          ", "x,1/2,0        ", "0,y,1/4        ",    &
                     "1/2,y,1/4      ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 30)= wyck_info_type("P 21 21 2   ", 2,     &
                   ["0,0,z          ", "0,1/2,z        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 31)= wyck_info_type("P 21 21 21  ", 0,     &
                   ["               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 32)= wyck_info_type("C 2 2 21    ", 2,     &
                   ["x,0,0          ", "0,y,1/4        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 33)= wyck_info_type("C 2 2 2     ",11,     &
                   ["0,0,0          ", "0,1/2,0        ", "1/2,0,1/2      ",    &
                     "0,0,1/2        ", "x,0,0          ", "x,0,1/2        ",    &
                     "0,y,0          ", "0,y,1/2        ", "0,0,z          ",    &
                     "0,1/2,z        ", "1/4,1/4,z      ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 34)= wyck_info_type("F 2 2 2     ",10,     &
                   ["0,0,0          ", "0,0,1/2        ", "1/4,1/4,1/4    ",    &
                     "1/4,1/4,3/4    ", "x,0,0          ", "0,y,0          ",    &
                     "0,0,z          ", "1/4,1/4,z      ", "1/4,y,1/4      ",    &
                     "x,1/4,1/4      ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 35)= wyck_info_type("I 2 2 2     ",10,     &
                   ["0,0,0          ", "1/2,0,0        ", "0,0,1/2        ",    &
                     "0,1/2,0        ", "x,0,0          ", "x,0,1/2        ",    &
                     "0,y,0          ", "1/2,y,0        ", "0,0,z          ",    &
                     "0,1/2,z        ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 36)= wyck_info_type("I 21 21 21  ", 3,     &
                   ["x,0,1/4        ", "1/4,y,0        ", "0,1/4,z        ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 37)= wyck_info_type("P M M 2     ", 8,     &
                   ["0,0,z          ", "0,1/2,z        ", "1/2,0,z        ",    &
                     "1/2,1/2,z      ", "x,0,z          ", "x,1/2,z        ",    &
                     "0,y,z          ", "1/2,y,z        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 38)= wyck_info_type("P M C 21    ", 2,     &
                   ["0,y,z          ", "1/2,y,z        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 39)= wyck_info_type("P C C 2     ", 4,     &
                   ["0,0,z          ", "0,1/2,z        ", "1/2,0,z        ",    &
                     "1/2,1/2,z      ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 40)= wyck_info_type("P M A 2     ", 3,     &
                   ["0,0,z          ", "0,1/2,z        ", "1/4,y,z        ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 41)= wyck_info_type("P C A 21    ", 0,     &
                   ["               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 42)= wyck_info_type("P N C 2     ", 2,     &
                   ["0,0,z          ", "1/2,0,z        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 43)= wyck_info_type("P M N 21    ", 1,     &
                   ["0,y,z          ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 44)= wyck_info_type("P B A 2     ", 2,     &
                   ["0,0,z          ", "0,1/2,z        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 45)= wyck_info_type("P N A 21    ", 0,     &
                   ["               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 46)= wyck_info_type("P N N 2     ", 2,     &
                   ["0,0,z          ", "0,1/2,z        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 47)= wyck_info_type("C M M 2     ", 5,     &
                   ["0,0,z          ", "0,1/2,z        ", "1/4,1/4,z      ",    &
                     "x,0,z          ", "0,y,z          ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 48)= wyck_info_type("C M C 21    ", 1,     &
                   ["0,y,z          ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 49)= wyck_info_type("C C C 2     ", 3,     &
                   ["0,0,z          ", "0,1/2,z        ", "1/4,1/4,z      ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 50)= wyck_info_type("A M M 2     ", 5,     &
                   ["0,0,z          ", "1/2,0,z        ", "x,0,z          ",    &
                     "0,y,z          ", "1/2,y,z        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 51)= wyck_info_type("A B M 2     ", 3,     &
                   ["0,0,z          ", "1/2,0,z        ", "x,1/4,z        ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 52)= wyck_info_type("A M A 2     ", 2,     &
                   ["0,0,z          ", "1/4,y,z        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 53)= wyck_info_type("A B A 2     ", 1,     &
                   ["0,0,z          ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 54)= wyck_info_type("F M M 2     ", 4,     &
                   ["0,0,z          ", "1/4,1/4,z      ", "0,y,z          ",    &
                     "x,0,z          ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 55)= wyck_info_type("F D D 2     ", 1,     &
                   ["0,0,z          ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 56)= wyck_info_type("I M M 2     ", 4,     &
                   ["0,0,z          ", "0,1/2,z        ", "x,0,z          ",    &
                     "0,y,z          ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 57)= wyck_info_type("I B A 2     ", 2,     &
                   ["0,0,z          ", "0,1/2,z        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 58)= wyck_info_type("I M A 2     ", 2,     &
                   ["0,0,z          ", "1/4,y,z        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 59)= wyck_info_type("P M M M     ",26,     &
                   ["0,0,0          ", "1/2,0,0        ", "0,0,1/2        ",    &
                     "1/2,0,1/2      ", "0,1/2,0        ", "1/2,1/2,0      ",    &
                     "0,1/2,1/2      ", "1/2,1/2,1/2    ", "x,0,0          ",    &
                     "x,0,1/2        ", "x,1/2,0        ", "x,1/2,1/2      ",    &
                     "0,y,0          ", "0,y,1/2        ", "1/2,y,0        ",    &
                     "1/2,y,1/2      ", "0,0,z          ", "0,1/2,z        ",    &
                     "1/2,0,z        ", "1/2,1/2,z      ", "0,y,z          ",    &
                     "1/2,y,z        ", "x,0,z          ", "x,1/2,z        ",    &
                     "x,y,0          ", "x,y,1/2        "] )
      wyckoff_info( 60)= wyck_info_type("P N N N:1   ",12,     &
                   ["0,0,0          ", "1/2,0,0        ", "0,0,1/2        ",    &
                     "0,1/2,0        ", "1/4,1/4,1/4    ", "3/4,3/4,3/4    ",    &
                     "x,0,0          ", "x,0,1/2        ", "0,y,0          ",    &
                     "1/2,y,0        ", "0,0,z          ", "0,1/2,z        ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 61)= wyck_info_type("P N N N     ",12,     &
                   ["1/4,1/4,1/4    ", "3/4,1/4,1/4    ", "1/4,1/4,3/4    ",    &
                     "1/4,3/4,1/4    ", "1/2,1/2,1/2    ", "0,0,0          ",    &
                     "x,1/4,1/4      ", "x,1/4,3/4      ", "1/4,y,1/4      ",    &
                     "3/4,y,1/4      ", "1/4,1/4,z      ", "1/4,3/4,z      ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 62)= wyck_info_type("P C C M     ",17,     &
                   ["0,0,0          ", "1/2,1/2,0      ", "0,1/2,0        ",    &
                     "1/2,0,0        ", "0,0,1/4        ", "1/2,0,1/4      ",    &
                     "0,1/2,1/4      ", "1/2,1/2,1/4    ", "x,0,1/4        ",    &
                     "x,1/2,1/4      ", "0,y,1/4        ", "1/2,y,1/4      ",    &
                     "0,0,z          ", "1/2,1/2,z      ", "0,1/2,z        ",    &
                     "1/2,0,z        ", "x,y,0          ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 63)= wyck_info_type("P B A N:1   ",12,     &
                   ["0,0,0          ", "1/2,0,0        ", "1/2,0,1/2      ",    &
                     "0,0,1/2        ", "1/4,1/4,0      ", "1/4,1/4,1/2    ",    &
                     "x,0,0          ", "x,0,1/2        ", "0,y,0          ",    &
                     "0,y,1/2        ", "0,0,z          ", "0,1/2,z        ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 64)= wyck_info_type("P B A N     ",12,     &
                   ["1/4,1/4,0      ", "3/4,1/4,0      ", "3/4,1/4,1/2    ",    &
                     "1/4,1/4,1/2    ", "0,0,0          ", "0,0,1/2        ",    &
                     "x,1/4,0        ", "x,1/4,1/2      ", "1/4,y,0        ",    &
                     "1/4,y,1/2      ", "1/4,1/4,z      ", "1/4,3/4,z      ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 65)= wyck_info_type("P M M A     ",11,     &
                   ["0,0,0          ", "0,1/2,0        ", "0,0,1/2        ",    &
                     "0,1/2,1/2      ", "1/4,0,z        ", "1/4,1/2,z      ",    &
                     "0,y,0          ", "0,y,1/2        ", "x,0,z          ",    &
                     "x,1/2,z        ", "1/4,y,z        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 66)= wyck_info_type("P N N A     ", 4,     &
                   ["0,0,0          ", "0,0,1/2        ", "1/4,0,z        ",    &
                     "x,1/4,1/4      ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 67)= wyck_info_type("P M N A     ", 8,     &
                   ["0,0,0          ", "1/2,0,0        ", "1/2,1/2,0      ",    &
                     "0,1/2,0        ", "x,0,0          ", "x,1/2,0        ",    &
                     "1/4,y,1/4      ", "0,y,z          ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 68)= wyck_info_type("P C C A     ", 5,     &
                   ["0,0,0          ", "0,1/2,0        ", "0,y,1/4        ",    &
                     "1/4,0,z        ", "1/4,1/2,z      ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 69)= wyck_info_type("P B A M     ", 8,     &
                   ["0,0,0          ", "0,0,1/2        ", "0,1/2,0        ",    &
                     "0,1/2,1/2      ", "0,0,z          ", "0,1/2,z        ",    &
                     "x,y,0          ", "x,y,1/2        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 70)= wyck_info_type("P C C N     ", 4,     &
                   ["0,0,0          ", "0,0,1/2        ", "1/4,1/4,z      ",    &
                     "1/4,3/4,z      ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 71)= wyck_info_type("P B C M     ", 4,     &
                   ["0,0,0          ", "1/2,0,0        ", "x,1/4,0        ",    &
                     "x,y,1/4        ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 72)= wyck_info_type("P N N M     ", 7,     &
                   ["0,0,0          ", "0,0,1/2        ", "0,1/2,0        ",    &
                     "0,1/2,1/2      ", "0,0,z          ", "0,1/2,z        ",    &
                     "x,y,0          ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 73)= wyck_info_type("P M M N:1   ", 6,     &
                   ["0,0,z          ", "0,1/2,z        ", "1/4,1/4,0      ",    &
                     "1/4,1/4,1/2    ", "0,y,z          ", "x,0,z          ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 74)= wyck_info_type("P M M N     ", 6,     &
                   ["1/4,1/4,z      ", "1/4,3/4,z      ", "0,0,0          ",    &
                     "0,0,1/2        ", "1/4,y,z        ", "x,1/4,z        ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 75)= wyck_info_type("P B C N     ", 3,     &
                   ["0,0,0          ", "0,1/2,0        ", "0,y,1/4        ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 76)= wyck_info_type("P B C A     ", 2,     &
                   ["0,0,0          ", "0,0,1/2        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 77)= wyck_info_type("P N M A     ", 3,     &
                   ["0,0,0          ", "0,0,1/2        ", "x,1/4,z        ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 78)= wyck_info_type("C M C M     ", 7,     &
                   ["0,0,0          ", "0,1/2,0        ", "0,y,1/4        ",    &
                     "1/4,1/4,0      ", "x,0,0          ", "0,y,z          ",    &
                     "x,y,1/4        ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 79)= wyck_info_type("C M C A     ", 6,     &
                   ["0,0,0          ", "1/2,0,0        ", "1/4,1/4,0      ",    &
                     "x,0,0          ", "1/4,y,1/4      ", "0,y,z          ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 80)= wyck_info_type("C M M M     ",17,     &
                   ["0,0,0          ", "1/2,0,0        ", "1/2,0,1/2      ",    &
                     "0,0,1/2        ", "1/4,1/4,0      ", "1/4,1/4,1/2    ",    &
                     "x,0,0          ", "x,0,1/2        ", "0,y,0          ",    &
                     "0,y,1/2        ", "0,0,z          ", "0,1/2,z        ",    &
                     "1/4,1/4,z      ", "0,y,z          ", "x,0,z          ",    &
                     "x,y,0          ", "x,y,1/2        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 81)= wyck_info_type("C C C M     ",12,     &
                   ["0,0,1/4        ", "0,1/2,1/4      ", "0,0,0          ",    &
                     "0,1/2,0        ", "1/4,1/4,0      ", "1/4,3/4,0      ",    &
                     "x,0,1/4        ", "0,y,1/4        ", "0,0,z          ",    &
                     "0,1/2,z        ", "1/4,1/4,z      ", "x,y,0          ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 82)= wyck_info_type("C M M A     ",14,     &
                   ["1/4,0,0        ", "1/4,0,1/2      ", "0,0,0          ",    &
                     "0,0,1/2        ", "1/4,1/4,0      ", "1/4,1/4,1/2    ",    &
                     "0,1/4,z        ", "x,0,0          ", "x,0,1/2        ",    &
                     "1/4,y,0        ", "1/4,y,1/2      ", "1/4,0,z        ",    &
                     "0,y,z          ", "x,1/4,z        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 83)= wyck_info_type("C C C A:1   ", 8,     &
                   ["0,0,0          ", "0,0,1/2        ", "1/4,0,1/4      ",    &
                     "0,1/4,1/4      ", "x,0,0          ", "0,y,0          ",    &
                     "0,0,z          ", "1/4,1/4,z      ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 84)= wyck_info_type("C C C A     ", 8,     &
                   ["0,1/4,1/4      ", "0,1/4,3/2      ", "1/4,3/4,0      ",    &
                     "0,0,0          ", "x,1/4,1/4      ", "0,y,1/4        ",    &
                     "0,1/4,z        ", "1/4,0,z        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 85)= wyck_info_type("F M M M     ",15,     &
                   ["0,0,0          ", "0,0,1/2        ", "0,1/4,1/4      ",    &
                     "1/4,0,1/4      ", "1/4,1/4,0      ", "1/4,1/4,1/4    ",    &
                     "x,0,0          ", "0,y,0          ", "0,0,z          ",    &
                     "1/4,1/4,z      ", "1/4,y,1/4      ", "x,1/4,1/4      ",    &
                     "0,y,z          ", "x,0,z          ", "x,y,0          ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 86)= wyck_info_type("F D D D:1   ", 7,     &
                   ["0,0,0          ", "0,0,1/2        ", "1/8,1/8,1/8    ",    &
                     "5/8,5/8,5/8    ", "x,0,0          ", "0,y,0          ",    &
                     "0,0,z          ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 87)= wyck_info_type("F D D D     ", 7,     &
                   ["1/8,1/8,1/8    ", "1/8,1/8,5/8    ", "0,0,0          ",    &
                     "1/2,1/2,1/2    ", "x,1/8,1/8      ", "1/8,y,1/8      ",    &
                     "1/8,1/8,z      ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 88)= wyck_info_type("I M M M     ",14,     &
                   ["0,0,0          ", "0,1/2,1/2      ", "1/2,1/2,0      ",    &
                     "1/2,0,1/2      ", "x,0,0          ", "x,1/2,0        ",    &
                     "0,y,0          ", "0,y,1/2        ", "0,0,z          ",    &
                     "1/2,0,z        ", "1/4,1/4,1/4    ", "0,y,z          ",    &
                     "x,0,z          ", "x,y,0          ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 89)= wyck_info_type("I B A M     ",10,     &
                   ["0,0,1/4        ", "1/2,0,1/4      ", "0,0,0          ",    &
                     "1/2,0,0        ", "1/4,1/4,1/4    ", "x,0,1/4        ",    &
                     "0,y,1/4        ", "0,0,z          ", "0,1/2,z        ",    &
                     "x,y,0          ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 90)= wyck_info_type("I B C A     ", 5,     &
                   ["0,0,0          ", "1/4,1/4,1/4    ", "x,0,1/4        ",    &
                     "1/4,y,0        ", "0,1/4,z        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 91)= wyck_info_type("I M M A     ", 9,     &
                   ["0,0,0          ", "0,0,1/2        ", "1/4,1/4,1/4    ",    &
                     "1/4,1/4,3/4    ", "0,1/4,z        ", "x,0,0          ",    &
                     "1/4,y,1/4      ", "0,y,z          ", "x,1/4,z        ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 92)= wyck_info_type("P 4         ", 3,     &
                   ["0,0,z          ", "1/2,1/2,z      ", "0,1/2,z        ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 93)= wyck_info_type("P 41        ", 0,     &
                   ["               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 94)= wyck_info_type("P 42        ", 3,     &
                   ["0,0,z          ", "1/2,1/2,z      ", "0,1/2,z        ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 95)= wyck_info_type("P 43        ", 0,     &
                   ["               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 96)= wyck_info_type("I 4         ", 2,     &
                   ["0,0,z          ", "0,1/2,z        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 97)= wyck_info_type("I 41        ", 1,     &
                   ["0,0,z          ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 98)= wyck_info_type("P -4        ", 7,     &
                   ["0,0,0          ", "0,0,1/2        ", "1/2,1/2,0      ",    &
                     "1/2,1/2,1/2    ", "0,0,z          ", "1/2,1/2,z      ",    &
                     "0,1/2,z        ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info( 99)= wyck_info_type("I -4        ", 6,     &
                   ["0,0,0          ", "0,0,1/2        ", "0,1/2,1/4      ",    &
                     "0,1/2,3/4      ", "0,0,z          ", "0,1/2,z        ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(100)= wyck_info_type("P 4/M       ",11,     &
                   ["0,0,0          ", "0,0,1/2        ", "1/2,1/2,0      ",    &
                     "1/2,1/2,1/2    ", "0,1/2,0        ", "0,1/2,1/2      ",    &
                     "0,0,z          ", "1/2,1/2,z      ", "0,1/2,z        ",    &
                     "x,y,0          ", "x,y,1/2        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(101)= wyck_info_type("P 42/M      ",10,     &
                   ["0,0,0          ", "1/2,1/2,0      ", "0,1/2,0        ",    &
                     "0,1/2,1/2      ", "0,0,1/4        ", "1/2,1/2,1/4    ",    &
                     "0,0,z          ", "1/2,1/2,z      ", "0,1/2,z        ",    &
                     "x,y,0          ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(102)= wyck_info_type("P 4/N:1     ", 6,     &
                   ["0,0,0          ", "0,0,1/2        ", "0,1/2,z        ",    &
                     "1/4,1/4,0      ", "1/4,1/4,1/2    ", "0,0,z          ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(103)= wyck_info_type("P 4/N       ", 6,     &
                   ["1/4,3/4,0      ", "1/4,3/4,1/2    ", "1/4,1/4,z      ",    &
                     "0,0,0          ", "0,0,1/2        ", "1/4,3/4,z      ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(104)= wyck_info_type("P 42/N:1    ", 6,     &
                   ["0,0,0          ", "0,0,1/2        ", "1/4,1/4,1/4    ",    &
                     "1/4,1/4,3/4    ", "0,1/2,z        ", "0,0,z          ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(105)= wyck_info_type("P 42/N      ", 6,     &
                   ["1/4,1/4,1/4    ", "1/4,1/4,3/4    ", "0,0,0          ",    &
                     "0,0,1/2        ", "3/4,1/4,z      ", "1/4,1/4,z      ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(106)= wyck_info_type("I 4/M       ", 8,     &
                   ["0,0,0          ", "0,0,1/2        ", "0,1/2,0        ",    &
                     "0,1/2,1/4      ", "0,0,z          ", "1/4,1/4,1/4    ",    &
                     "0,1/2,z        ", "x,y,0          ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(107)= wyck_info_type("I 41/A:1    ", 5,     &
                   ["0,0,0          ", "0,0,1/2        ", "0,1/4,1/8      ",    &
                     "0,1/4,5/8      ", "0,0,z          ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(108)= wyck_info_type("I 41/A      ", 5,     &
                   ["0,1/4,1/8      ", "0,1/4,5/8      ", "0,0,0          ",    &
                     "0,0,1/2        ", "0,1/4,z        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(109)= wyck_info_type("P 4 2 2     ",15,     &
                   ["0,0,0          ", "0,0,1/2        ", "1/2,1/2,0      ",    &
                     "1/2,1/2,1/2    ", "1/2,0,0        ", "1/2,0,1/2      ",    &
                     "0,0,z          ", "1/2,1/2,z      ", "0,1/2,z        ",    &
                     "x,x,0          ", "x,x,1/2        ", "x,0,0          ",    &
                     "x,1/2,1/2      ", "x,0,1/2        ", "x,1/2,0        ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(110)= wyck_info_type("P 4 21 2    ", 6,     &
                   ["0,0,0          ", "0,0,1/2        ", "0,1/2,z        ",    &
                     "0,0,z          ", "x,x,0          ", "x,x,1/2        ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(111)= wyck_info_type("P 41 2 2    ", 3,     &
                   ["0,y,0          ", "1/2,y,0        ", "x,x,3/8        ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(112)= wyck_info_type("P 41 21 2   ", 1,     &
                   ["x,x,0          ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(113)= wyck_info_type("P 42 2 2    ",15,     &
                   ["0,0,0          ", "1/2,1/2,0      ", "0,1/2,0        ",    &
                     "0,1/2,1/2      ", "0,0,1/4        ", "1/2,1/2,1/4    ",    &
                     "0,0,z          ", "1/2,1/2,z      ", "0,1/2,z        ",    &
                     "x,0,0          ", "x,1/2,1/2      ", "x,0,1/2        ",    &
                     "x,1/2,0        ", "x,x,1/4        ", "x,x,3/4        ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(114)= wyck_info_type("P 42 21 2   ", 6,     &
                   ["0,0,0          ", "0,0,1/2        ", "0,0,z          ",    &
                     "0,1/2,z        ", "x,x,0          ", "x,x,1/2        ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(115)= wyck_info_type("P 43 2 2    ", 3,     &
                   ["0,y,0          ", "1/2,y,0        ", "x,x,5/8        ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(116)= wyck_info_type("P 43 21 2   ", 1,     &
                   ["x,x,0          ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(117)= wyck_info_type("I 4 2 2     ",10,     &
                   ["0,0,0          ", "0,0,1/2        ", "0,1/2,0        ",    &
                     "0,1/2,1/4      ", "0,0,z          ", "0,1/2,z        ",    &
                     "x,x,0          ", "x,0,0          ", "x,0,1/2        ",    &
                     "x,x+1/2,1/4    ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(118)= wyck_info_type("I 41 2 2    ", 6,     &
                   ["0,0,0          ", "0,0,1/2        ", "0,0,z          ",    &
                     "x,x,0          ", "-x,x,0         ", "x,1/4,1/8      ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )

      wyckoff_info(119)= wyck_info_type("P 4 M M     ", 6,     &
                   ["0,0,z          ", "1/2,1/2,z      ", "1/2,0,z        ",    &
                     "x,x,z          ", "x,0,z          ", "x,1/2,z        ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(120)= wyck_info_type("P 4 B M     ", 3,     &
                   ["0,0,z          ", "0,1/2,z        ", "x,x+1/2,z      ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(121)= wyck_info_type("P 42 C M    ", 4,     &
                   ["0,0,z          ", "1/2,1/2,z      ", "0,1/2,z        ",    &
                     "x,x,z          ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(122)= wyck_info_type("P 42 N M    ", 3,     &
                   ["0,0,z          ", "0,1/2,z        ", "x,x,z          ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(123)= wyck_info_type("P 4 C C     ", 3,     &
                   ["0,0,z          ", "1/2,1/2,z      ", "0,1/2,z        ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(124)= wyck_info_type("P 4 N C     ", 2,     &
                   ["0,0,z          ", "0,1/2,z        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(125)= wyck_info_type("P 42 M C    ", 5,     &
                   ["0,0,z          ", "1/2,1/2,z      ", "0,1/2,z        ",    &
                     "x,0,z          ", "x,1/2,z        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(126)= wyck_info_type("P 42 B C    ", 2,     &
                   ["0,0,z          ", "0,1/2,z        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(127)= wyck_info_type("I 4 M M     ", 4,     &
                   ["0,0,z          ", "0,1/2,z        ", "x,x,z          ",    &
                     "x,0,z          ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(128)= wyck_info_type("I 4 C M     ", 3,     &
                   ["0,0,z          ", "1/2,0,z        ", "x,x+1/2,z      ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(129)= wyck_info_type("I 41 M D    ", 2,     &
                   ["0,0,z          ", "0,y,z          ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(130)= wyck_info_type("I 41 C D    ", 1,     &
                   ["0,0,z          ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(131)= wyck_info_type("P -4 2 M    ",14,     &
                   ["0,0,0          ", "1/2,1/2,1/2    ", "0,0,1/2        ",    &
                     "1/2,1/2,0      ", "1/2,0,0        ", "1/2,0,1/2      ",    &
                     "0,0,z          ", "1/2,1/2,z      ", "x,0,0          ",    &
                     "x,1/2,1/2      ", "x,0,1/2        ", "x,1/2,0        ",    &
                     "0,1/2,z        ", "x,x,z          ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(132)= wyck_info_type("P -4 2 C    ",13,     &
                   ["0,0,1/4        ", "1/2,0,1/4      ", "1/2,1/2,1/4    ",    &
                     "0,1/2,1/4      ", "0,0,0          ", "1/2,1/2,0      ",    &
                     "x,0,1/4        ", "1/2,y,1/4      ", "x,1/2,1/4      ",    &
                     "0,y,1/4        ", "0,0,z          ", "1/2,1/2,z      ",    &
                     "0,1/2,z        ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(133)= wyck_info_type("P -4 21 M   ", 5,     &
                   ["0,0,0          ", "0,0,1/2        ", "0,1/2,z        ",    &
                     "0,0,z          ", "x,x+1/2,z      ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(134)= wyck_info_type("P -4 21 C   ", 4,     &
                   ["0,0,0          ", "0,0,1/2        ", "0,0,z          ",    &
                     "0,1/2,z        ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(135)= wyck_info_type("P -4 M 2    ",11,     &
                   ["0,0,0          ", "1/2,1/2,0      ", "1/2,1/2,1/2    ",    &
                     "0,0,1/2        ", "0,0,z          ", "1/2,1/2,z      ",    &
                     "0,1/2,z        ", "x,x,0          ", "x,x,1/2        ",    &
                     "x,0,z          ", "x,1/2,z        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(136)= wyck_info_type("P -4 C 2    ", 9,     &
                   ["0,0,1/4        ", "1/2,1/2,1/4    ", "0,0,0          ",    &
                     "1/2,1/2,0      ", "x,x,1/4        ", "x,x,3/4        ",    &
                     "0,0,z          ", "1/2,1/2,z      ", "0,1/2,z        ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(137)= wyck_info_type("P -4 B 2    ", 8,     &
                   ["0,0,0          ", "0,0,1/2        ", "0,1/2,0        ",    &
                     "0,1/2,1/2      ", "0,0,z          ", "0,1/2,z        ",    &
                     "x,x+1/2,0      ", "x,x+1/2,1/2    ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(138)= wyck_info_type("P -4 N 2    ", 8,     &
                   ["0,0,0          ", "0,0,1/2        ", "0,1/2,1/4      ",    &
                     "0,1/2,3/4      ", "0,0,z          ", "x,-x+1/2,1/4   ",    &
                     "x,x+1/2,1/4    ", "0,1/2,z        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(139)= wyck_info_type("I -4 M 2    ", 9,     &
                   ["0,0,0          ", "0,0,1/2        ", "0,1/2,1/4      ",    &
                     "0,1/2,3/4      ", "0,0,z          ", "0,1/2,z        ",    &
                     "x,x,0          ", "x,x+1/2,1/4    ", "x,0,z          ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(140)= wyck_info_type("I -4 C 2    ", 8,     &
                   ["0,0,1/4        ", "0,0,0          ", "0,1/2,1/4      ",    &
                     "0,1/2,0        ", "x,x,1/4        ", "0,0,z          ",    &
                     "0,1/2,z        ", "x,x+1/2,0      ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(141)= wyck_info_type("I -4 2 M    ", 9,     &
                   ["0,0,0          ", "0,0,1/2        ", "0,1/2,0        ",    &
                     "0,1/2,1/4      ", "0,0,z          ", "x,0,0          ",    &
                     "x,0,1/2        ", "0,1/2,z        ", "x,x,z          ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(142)= wyck_info_type("I -4 2 D    ", 4,     &
                   ["0,0,0          ", "0,0,1/2        ", "0,0,z          ",    &
                     "x,1/4,1/8      ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(143)= wyck_info_type("P 4/M M M   ",20,     &
                   ["0,0,0          ", "0,0,1/2        ", "1/2,1/2,0      ",    &
                     "1/2,1/2,1/2    ", "0,1/2,1/2      ", "0,1/2,0        ",    &
                     "0,0,z          ", "1/2,1/2,z      ", "0,1/2,z        ",    &
                     "x,x,0          ", "x,x,1/2        ", "x,0,0          ",    &
                     "x,0,1/2        ", "x,1/2,0        ", "x,1/2,1/2      ",    &
                     "x,y,0          ", "x,y,1/2        ", "x,x,z          ",    &
                     "x,0,z          ", "x,1/2,z        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(144)= wyck_info_type("P 4/M C C   ",13,     &
                   ["0,0,1/4        ", "0,0,0          ", "1/2,1/2,1/4    ",    &
                     "1/2,1/2,0      ", "0,1/2,0        ", "0,1/2,1/4      ",    &
                     "0,0,z          ", "1/2,1/2,z      ", "0,1/2,z        ",    &
                     "x,x,1/4        ", "x,0,1/4        ", "x,1/2,1/4      ",    &
                     "x,y,0          ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(145)= wyck_info_type("P 4/N B M:1 ",13,     &
                   ["0,0,0          ", "0,0,1/2        ", "0,1/2,0        ",    &
                     "0,1/2,1/2      ", "1/4,1/4,0      ", "1/4,1/4,1/2    ",    &
                     "0,0,z          ", "0,1/2,z        ", "x,x,0          ",    &
                     "x,x,1/2        ", "x,0,0          ", "x,0,1/2        ",    &
                     "x,x+1/2,z      ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(146)= wyck_info_type("P 4/N B M   ",13,     &
                   ["1/4,1/4,0      ", "1/4,1/4,1/2    ", "3/4,1/4,0      ",    &
                     "3/4,1/4,1/2    ", "0,0,0          ", "0,0,1/2        ",    &
                     "1/4,1/4,z      ", "3/4,1/4,z      ", "x,x,0          ",    &
                     "x,x,1/2        ", "x,1/4,0        ", "x,1/4,1/2      ",    &
                     "x,-x,z         ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(147)= wyck_info_type("P 4/N N C:1 ",10,     &
                   ["0,0,0          ", "0,0,1/2        ", "1/2,0,0        ",    &
                     "1/2,0,1/4      ", "0,0,z          ", "1/4,1/4,1/4    ",    &
                     "1/2,0,z        ", "x,x,0          ", "x,0,0          ",    &
                     "x,0,1/2        ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(148)= wyck_info_type("P 4/N N C   ",10,     &
                   ["1/4,1/4,1/4    ", "1/4,1/4,3/4    ", "1/4,3/4,3/4    ",    &
                     "1/4,1/4,0      ", "1/4,1/4,z      ", "0,0,0          ",    &
                     "1/4,3/4,z      ", "x,x,1/4        ", "x,1/4,1/4      ",    &
                     "x,3/4,1/4      ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(149)= wyck_info_type("P 4/M B M   ",11,     &
                   ["0,0,0          ", "0,0,1/2        ", "0,1/2,1/2      ",    &
                     "0,1/2,0        ", "0,0,z          ", "0,1/2,z        ",    &
                     "x,x+1/2,0      ", "x,x+1/2,1/2    ", "x,y,0          ",    &
                     "x,y,1/2        ", "x,x+1/2,z      ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(150)= wyck_info_type("P 4/M N C   ", 8,     &
                   ["0,0,0          ", "0,0,1/2        ", "0,1/2,0        ",    &
                     "0,1/2,1/4      ", "0,0,z          ", "0,1/2,z        ",    &
                     "x,x+1/2,1/4    ", "x,y,0          ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(151)= wyck_info_type("P 4/N M M:1 ",10,     &
                   ["0,0,0          ", "0,0,1/2        ", "0,1/2,z        ",    &
                     "1/4,1/4,0      ", "1/4,1/4,1/2    ", "0,0,z          ",    &
                     "x,x,0          ", "x,x,1/2        ", "0,y,z          ",    &
                     "x,x+1/2,z      ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(152)= wyck_info_type("P 4/N M M   ",10,     &
                   ["3/4,1/4,0      ", "3/4,1/4,1/2    ", "1/4,1/4,z      ",    &
                     "0,0,0          ", "0,0,1/2        ", "3/4,1/4,z      ",    &
                     "x,-x,0         ", "x,-x,1/2       ", "1/4,y,z        ",    &
                     "x,x,z          ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(153)= wyck_info_type("P 4/N C C:1 ", 6,     &
                   ["0,0,1/4        ", "0,0,0          ", "0,1/2,z        ",    &
                     "1/4,1/4,0      ", "0,0,z          ", "x,x,1/4        ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(154)= wyck_info_type("P 4/N C C   ", 6,     &
                   ["3/4,1/4,1/4    ", "3/4,1/4,0      ", "1/4,1/4,z      ",    &
                     "0,0,0          ", "3/4,1/4,z      ", "x,-x,1/4       ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(155)= wyck_info_type("P 42/M M C  ",17,     &
                   ["0,0,0          ", "1/2,1/2,0      ", "0,1/2,0        ",    &
                     "0,1/2,1/2      ", "0,0,1/4        ", "1/2,1/2,1/4    ",    &
                     "0,0,z          ", "1/2,1/2,z      ", "0,1/2,z        ",    &
                     "x,0,0          ", "x,1/2,1/2      ", "x,0,1/2        ",    &
                     "x,1/2,0        ", "x,x,1/4        ", "0,y,z          ",    &
                     "1/2,y,z        ", "x,y,0          ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(156)= wyck_info_type("P 42/M C M  ",15,     &
                   ["0,0,0          ", "0,0,1/4        ", "1/2,1/2,0      ",    &
                     "1/2,1/2,1/4    ", "0,1/2,1/4      ", "0,1/2,0        ",    &
                     "0,0,z          ", "1/2,1/2,z      ", "x,x,0          ",    &
                     "x,x,1/2        ", "0,1/2,z        ", "x,0,1/4        ",    &
                     "x,1/2,1/4      ", "x,y,0          ", "x,x,z          ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(157)= wyck_info_type("P 42/N B C:1",10,     &
                   ["0,1/2,1/4      ", "0,0,1/4        ", "0,1/2,0        ",    &
                     "0,0,0          ", "1/4,1/4,1/4    ", "0,1/2,z        ",    &
                     "0,0,z          ", "x,0,1/4        ", "x,0,3/4        ",    &
                     "x,x+1/2,0      ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(158)= wyck_info_type("P 42/N B C  ",10,     &
                   ["1/4,1/4,0      ", "3/4,1/4,0      ", "1/4,1/4,1/4    ",    &
                     "3/4,1/4,3/4    ", "0,0,0          ", "1/4,1/4,z      ",    &
                     "3/4,1/4,z      ", "x,1/4,0        ", "x,1/4,1/2      ",    &
                     "x,x,1/4        ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(159)= wyck_info_type("P 42/N N M:1",13,     &
                   ["0,0,0          ", "0,0,1/2        ", "0,1/2,0        ",    &
                     "0,1/2,1/4      ", "1/4,1/4,1/4    ", "3/4,3/4,3/4    ",    &
                     "0,0,z          ", "0,1/2,z        ", "x,0,0          ",    &
                     "x,0,1/2        ", "x,x+1/2,1/4    ", "x,x+1/2,3/4    ",    &
                     "x,x,z          ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(160)= wyck_info_type("P 42/N N M  ",13,     &
                   ["1/4,3/4,1/4    ", "3/4,1/4,1/4    ", "1/4,1/4,1/4    ",    &
                     "1/4,1/4,0      ", "0,0,1/2        ", "0,0,0          ",    &
                     "3/4,1/4,z      ", "1/4,1/4,z      ", "x,1/4,3/4      ",    &
                     "x,1/4,1/4      ", "x,x,0          ", "x,x,1/2        ",    &
                     "x,-x,z         ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(161)= wyck_info_type("P 42/M B C  ", 8,     &
                   ["0,0,0          ", "0,0,1/4        ", "0,1/2,0        ",    &
                     "0,1/2,1/4      ", "0,0,z          ", "0,1/2,z        ",    &
                     "x,x+1/2,1/4    ", "x,y,0          ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(162)= wyck_info_type("P 42/M N M  ",10,     &
                   ["0,0,0          ", "0,0,1/2        ", "0,1/2,0        ",    &
                     "0,1/2,1/4      ", "0,0,z          ", "x,x,0          ",    &
                     "x,-x,0         ", "0,1/2,z        ", "x,y,0          ",    &
                     "x,x,z          ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(163)= wyck_info_type("P 42/N M C:1", 7,     &
                   ["0,0,0          ", "0,0,1/2        ", "0,0,z          ",    &
                     "0,1/2,z        ", "1/4,1/4,1/4    ", "x,x,0          ",    &
                     "0,y,z          ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(164)= wyck_info_type("P 42/N M C  ", 7,     &
                   ["3/4,1/4,3/4    ", "3/4,1/4,1/4    ", "3/4,1/4,z      ",    &
                     "1/4,1/4,z      ", "0,0,0          ", "x,-x,1/4       ",    &
                     "1/4,y,z        ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(165)= wyck_info_type("P 42/N C M:1", 9,     &
                   ["0,0,1/4        ", "0,0,0          ", "1/4,1/4,1/4    ",    &
                     "1/4,1/4,3/4    ", "0,1/2,z        ", "0,0,z          ",    &
                     "x,x,1/4        ", "x,x,3/4        ", "x,x+1/2,z      ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(166)= wyck_info_type("P 42/N C M  ", 9,     &
                   ["3/4,1/4,0      ", "3/4,1/4,3/4    ", "0,0,1/2        ",    &
                     "0,0,0          ", "1/4,1/4,z      ", "3/4,1/4,z      ",    &
                     "x,-x,1/2       ", "x,-x,0         ", "x,x,z          ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(167)= wyck_info_type("I 4/M M M   ",14,     &
                   ["0,0,0          ", "0,0,1/2        ", "0,1/2,0        ",    &
                     "0,1/2,1/4      ", "0,0,z          ", "1/4,1/4,1/4    ",    &
                     "0,1/2,z        ", "x,x,0          ", "x,0,0          ",    &
                     "x,1/2,0        ", "x,x+1/2,1/4    ", "x,y,0          ",    &
                     "x,x,z          ", "0,y,z          ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(168)= wyck_info_type("I 4/M C M   ",12,     &
                   ["0,0,1/4        ", "0,1/2,1/4      ", "0,0,0          ",    &
                     "0,1/2,0        ", "1/4,1/4,1/4    ", "0,0,z          ",    &
                     "0,1/2,z        ", "x,x+1/2,0      ", "x,x,1/4        ",    &
                     "x,0,1/4        ", "x,y,0          ", "x,x+1/2,z      ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(169)= wyck_info_type("I 41/A M D:1", 8,     &
                   ["0,0,0          ", "0,0,1/2        ", "0,1/4,1/8      ",    &
                     "0,1/4,5/8      ", "0,0,z          ", "x,1/4,1/8      ",    &
                     "x,x,0          ", "0,y,z          ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(170)= wyck_info_type("I 41/A M D  ", 8,     &
                   ["0,3/4,1/8      ", "0,1/4,3/8      ", "0,0,0          ",    &
                     "0,0,1/2        ", "0,1/4,z        ", "x,0,0          ",    &
                     "x,x+1/4,7/8    ", "0,y,z          ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(171)= wyck_info_type("I 41/A C D:1", 6,     &
                   ["0,0,0          ", "0,0,1/4        ", "0,1/4,1/8      ",    &
                     "0,0,z          ", "1/4,y,1/8      ", "x,x,1/4        ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(172)= wyck_info_type("I 41/A C D  ", 6,     &
                   ["0,1/4,3/8      ", "0,1/4,1/8      ", "0,0,0          ",    &
                     "0,1/4,z        ", "x,0,1/4        ", "x,x+1/4,1/8    ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(173)= wyck_info_type("P 3         ", 3,     &
                   ["0,0,z          ", "1/3,2/3,z      ", "2/3,1/3,z      ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(174)= wyck_info_type("P 31        ", 0,     &
                   ["               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(175)= wyck_info_type("P 32        ", 0,     &
                   ["               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(176)= wyck_info_type("R 3         ", 1,     &
                   ["0,0,z          ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(177)= wyck_info_type("R 3:H       ", 1,     &
                   ["x,x,x          ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(178)= wyck_info_type("P -3        ", 6,     &
                   ["0,0,0          ", "0,0,1/2        ", "0,0,z          ",    &
                     "1/3,2/3,z      ", "1/2,0,0        ", "1/2,0,1/2      ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(179)= wyck_info_type("R -3        ", 5,     &
                   ["0,0,0          ", "0,0,1/2        ", "0,0,z          ",    &
                     "1/2,0,1/2      ", "1/2,0,0        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(180)= wyck_info_type("R -3:H      ", 5,     &
                   ["0,0,0          ", "1/2,1/2,1/2    ", "x,x,x          ",    &
                     "1/2,0,0        ", "0,1/2,1/2      ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(181)= wyck_info_type("P 3 1 2     ",11,     &
                   ["0,0,0          ", "0,0,1/2        ", "1/3,2/3,0      ",    &
                     "1/3,2/3,1/2    ", "2/3,1/3,0      ", "2/3,1/3,1/2    ",    &
                     "0,0,z          ", "1/3,2/3,z      ", "2/3,1/3,z      ",    &
                     "x,-x,0         ", "x,-x,1/2       ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(182)= wyck_info_type("P 3 2 1     ", 6,     &
                   ["0,0,0          ", "0,0,1/2        ", "0,0,z          ",    &
                     "1/3,2/3,z      ", "x,0,0          ", "x,0,1/2        ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(183)= wyck_info_type("P 31 1 2    ", 2,     &
                   ["x,-x,1/3       ", "x,-x,5/6       ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(184)= wyck_info_type("P 31 2 1    ", 2,     &
                   ["x,0,1/3        ", "x,0,5/6        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(185)= wyck_info_type("P 32 1 2    ", 2,     &
                   ["x,-x,2/3       ", "x,-x,1/6       ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(186)= wyck_info_type("P 32 2 1    ", 2,     &
                   ["x,0,2/3        ", "x,0,1/6        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(187)= wyck_info_type("R 3 2       ", 5,     &
                   ["0,0,0          ", "0,0,1/2        ", "0,0,z          ",    &
                     "x,0,0          ", "x,0,1/2        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(188)= wyck_info_type("R 3 2:R     ", 5,     &
                   ["0,0,0          ", "1/2,1/2,1/2    ", "x,x,x          ",    &
                     "0,y,-y         ", "1/2,y,-y       ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(189)= wyck_info_type("P 3 M 1     ", 4,     &
                   ["0,0,z          ", "1/3,2/3,z      ", "2/3,1/3,z      ",    &
                     "x,-x,z         ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(190)= wyck_info_type("P 3 1 M     ", 3,     &
                   ["0,0,z          ", "1/3,2/3,z      ", "x,0,z          ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(191)= wyck_info_type("P 3 C 1     ", 3,     &
                   ["0,0,z          ", "1/3,2/3,z      ", "2/3,1/3,z      ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(192)= wyck_info_type("P 3 1 C     ", 2,     &
                   ["0,0,z          ", "1/3,2/3,z      ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(193)= wyck_info_type("R 3 M       ", 2,     &
                   ["0,0,z          ", "x,-x,z         ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(194)= wyck_info_type("R 3 M:R     ", 2,     &
                   ["x,x,x          ", "x,x,z          ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(195)= wyck_info_type("R 3 C       ", 1,     &
                   ["0,0,z          ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(196)= wyck_info_type("R 3 C:R     ", 1,     &
                   ["x,x,x          ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(197)= wyck_info_type("P -3 1 M    ",11,     &
                   ["0,0,0          ", "0,0,1/2        ", "1/3,2/3,0      ",    &
                     "1/3,2/3,1/2    ", "0,0,z          ", "1/2,0,0        ",    &
                     "1/2,0,1/2      ", "1/3,2/3,z      ", "x,-x,0         ",    &
                     "x,-x,1/2       ", "x,0,z          ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(198)= wyck_info_type("P -3 1 C    ", 8,     &
                   ["0,0,1/4        ", "0,0,0          ", "1/3,2/3,1/4    ",    &
                     "2/3,1/3,1/4    ", "0,0,z          ", "1/3,2/3,z      ",    &
                     "1/2,0,0        ", "x,-x,1/4       ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(199)= wyck_info_type("P -3 M 1    ", 9,     &
                   ["0,0,0          ", "0,0,1/2        ", "0,0,z          ",    &
                     "1/3,2/3,z      ", "1/2,0,0        ", "1/2,0,1/2      ",    &
                     "x,0,0          ", "x,0,1/2        ", "x,-x,z         ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(200)= wyck_info_type("P -3 C 1    ", 6,     &
                   ["0,0,1/4        ", "0,0,0          ", "0,0,z          ",    &
                     "1/3,2/3,z      ", "1/2,0,0        ", "x,0,1/4        ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(201)= wyck_info_type("R -3 M      ", 8,     &
                   ["0,0,0          ", "0,0,1/2        ", "0,0,z          ",    &
                     "1/2,0,1/2      ", "1/2,0,0        ", "x,0,0          ",    &
                     "x,0,1/2        ", "x,-x,z         ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(202)= wyck_info_type("R -3 M:R    ", 8,     &
                   ["0,0,0          ", "1/2,1/2,1/2    ", "x,x,x          ",    &
                     "1/2,0,0        ", "0,1/2,1/2      ", "x,-x,0         ",    &
                     "x,-x,1/2       ", "x,x,z          ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(203)= wyck_info_type("R -3 C      ", 5,     &
                   ["0,0,1/4        ", "0,0,0          ", "0,0,z          ",    &
                     "1/2,0,0        ", "x,0,1/4        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(204)= wyck_info_type("R -3 C:R    ", 5,     &
                   ["1/4,1/4,1/4    ", "0,0,0          ", "x,x,x          ",    &
                     "1/2,0,0        ", "x,-x+1/2,1/4   ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(205)= wyck_info_type("P 6         ", 3,     &
                   ["0,0,z          ", "1/3,2/3,z      ", "1/2,0,z        ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(206)= wyck_info_type("P 61        ", 0,     &
                   ["               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(207)= wyck_info_type("P 65        ", 0,     &
                   ["               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(208)= wyck_info_type("P 62        ", 2,     &
                   ["0,0,z          ", "1/2,1/2,z      ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(209)= wyck_info_type("P 64        ", 2,     &
                   ["0,0,z          ", "1/2,1/2,z      ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(210)= wyck_info_type("P 63        ", 2,     &
                   ["0,0,z          ", "1/3,2/3,z      ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(211)= wyck_info_type("P -6        ",11,     &
                   ["0,0,0          ", "0,0,1/2        ", "1/3,2/3,0      ",    &
                     "1/3,2/3,1/2    ", "2/3,1/3,0      ", "2/3,1/3,1/2    ",    &
                     "0,0,z          ", "1/3,2/3,z      ", "2/3,1/3,z      ",    &
                     "x,y,0          ", "x,y,1/2        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(212)= wyck_info_type("P 6/M       ",11,     &
                   ["0,0,0          ", "0,0,1/2        ", "1/3,2/3,0      ",    &
                     "1/3,2/3,1/2    ", "0,0,z          ", "1/2,0,0        ",    &
                     "1/2,0,1/2      ", "1/3,2/3,z      ", "1/2,0,z        ",    &
                     "x,y,0          ", "x,y,1/2        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(213)= wyck_info_type("P 63/M      ", 8,     &
                   ["0,0,1/4        ", "0,0,0          ", "1/3,2/3,1/4    ",    &
                     "2/3,1/3,1/4    ", "0,0,z          ", "1/3,2/3,z      ",    &
                     "1/2,0,0        ", "x,y,1/4        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(214)= wyck_info_type("P 6 2 2     ",13,     &
                   ["0,0,0          ", "0,0,1/2        ", "1/3,2/3,0      ",    &
                     "1/3,2/3,1/2    ", "0,0,z          ", "1/2,0,0        ",    &
                     "1/2,0,1/2      ", "1/3,2/3,z      ", "1/2,0,z        ",    &
                     "x,0,0          ", "x,0,1/2        ", "x,-x,0         ",    &
                     "x,-x,1/2       ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(215)= wyck_info_type("P 61 2 2    ", 2,     &
                   ["x,0,0          ", "x,2x,1/4       ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(216)= wyck_info_type("P 65 2 2    ", 2,     &
                   ["x,0,0          ", "x,2x,3/4       ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(217)= wyck_info_type("P 62 2 2    ",10,     &
                   ["0,0,0          ", "0,0,1/2        ", "1/2,0,0        ",    &
                     "1/2,0,1/2      ", "0,0,z          ", "1/2,0,z        ",    &
                     "x,0,0          ", "x,0,1/2        ", "x,2x,0         ",    &
                     "x,2x,1/2       ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(218)= wyck_info_type("P 64 2 2    ",10,     &
                   ["0,0,0          ", "0,0,1/2        ", "1/2,0,0        ",    &
                     "1/2,0,/1,2     ", "0,0,z          ", "1/2,0,z        ",    &
                     "x,0,0          ", "x,0,1/2        ", "x,2x,0         ",    &
                     "x,2x,1/2       ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(219)= wyck_info_type("P 63 2 2    ", 8,     &
                   ["0,0,0          ", "0,0,1/4        ", "1/3,2/3,1/4    ",    &
                     "1/3,2/3,3/4    ", "0,0,z          ", "1/3,2/3,z      ",    &
                     "x,0,0          ", "x,2x,1/4       ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(220)= wyck_info_type("P 6 M M     ", 5,     &
                   ["0,0,z          ", "1/3,2/3,z      ", "1/2,0,z        ",    &
                     "x,0,z          ", "x,-x,z         ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(221)= wyck_info_type("P 6 C C     ", 3,     &
                   ["0,0,z          ", "1/3,2/3,z      ", "1/2,0,z        ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(222)= wyck_info_type("P 63 C M    ", 3,     &
                   ["0,0,z          ", "1/3,2/3,z      ", "x,0,z          ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(223)= wyck_info_type("P 63 M C    ", 3,     &
                   ["0,0,z          ", "1/3,2/3,z      ", "x,-x,z         ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(224)= wyck_info_type("P -6 M 2    ",14,     &
                   ["0,0,0          ", "0,0,1/2        ", "1/3,2/3,0      ",    &
                     "1/3,2/3,1/2    ", "2/3,1/3,0      ", "2/3,1/3,1/2    ",    &
                     "0,0,z          ", "1/3,2/3,z      ", "2/3,1/3,z      ",    &
                     "x,-x,0         ", "x,-x,1/2       ", "x,y,0          ",    &
                     "x,y,1/2        ", "x,-x,z         ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(225)= wyck_info_type("P -6 C 2    ",11,     &
                   ["0,0,0          ", "0,0,1/4        ", "1/3,2/3,0      ",    &
                     "1/3,2/3,1/4    ", "2/3,1/3,0      ", "2/3,1/3,1/4    ",    &
                     "0,0,z          ", "1/3,2/3,z      ", "2/3,1/3,z      ",    &
                     "x,-x,0         ", "x,y,1/4        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(226)= wyck_info_type("P -6 2 M    ",11,     &
                   ["0,0,0          ", "0,0,1/2        ", "1/3,2/3,0      ",    &
                     "1/3,2/3,1/2    ", "0,0,z          ", "x,0,0          ",    &
                     "x,0,1/2        ", "1/3,2/3,z      ", "x,0,z          ",    &
                     "x,y,0          ", "x,y,1/2        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(227)= wyck_info_type("P -6 2 C    ", 8,     &
                   ["0,0,0          ", "0,0,1/4        ", "1/3,2/3,1/4    ",    &
                     "2/3,1/3,1/4    ", "0,0,z          ", "1/3,2/3,z      ",    &
                     "x,0,0          ", "x,y,1/4        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(228)= wyck_info_type("P 6/M M M   ",17,     &
                   ["0,0,0          ", "0,0,1/2        ", "1/3,2/3,0      ",    &
                     "1/3,2/3,1/2    ", "0,0,z          ", "1/2,0,0        ",    &
                     "1/2,0,1/2      ", "1/3,2/3,z      ", "1/2,0,z        ",    &
                     "x,0,0          ", "x,0,1/2        ", "x,2x,0         ",    &
                     "x,2x,1/2       ", "x,0,z          ", "x,2x,z         ",    &
                     "x,y,0          ", "x,y,1/2        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(229)= wyck_info_type("P 6/M C C   ",12,     &
                   ["0,0,1/4        ", "0,0,0          ", "1/3,2/3,1/4    ",    &
                     "1/3,2/3,0      ", "0,0,z          ", "1/2,0,1/4      ",    &
                     "1/2,0,0        ", "1/3,2/3,z      ", "1/2,0,z        ",    &
                     "x,0,1/4        ", "x,2x,1/4       ", "x,y,0          ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(230)= wyck_info_type("P 63/M C M  ",11,     &
                   ["0,0,1/4        ", "0,0,0          ", "1/3,2/3,1/4    ",    &
                     "1/3,2/3,0      ", "0,0,z          ", "1/2,0,0        ",    &
                     "x,0,1/4        ", "1/3,2/3,z      ", "x,2x,0         ",    &
                     "x,y,1/4        ", "x,0,z          ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(231)= wyck_info_type("P 63/M M C  ",11,     &
                   ["0,0,0          ", "0,0,1/4        ", "1/3,2/3,1/4    ",    &
                     "1/3,2/3,3/4    ", "0,0,z          ", "1/3,2/3,z      ",    &
                     "1/2,0,0        ", "x,2x,1/4       ", "x,0,0          ",    &
                     "x,y,1/4        ", "x,2x,z         ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(232)= wyck_info_type("P 2 3       ", 9,     &
                   ["0,0,0          ", "1/2,1/2,1/2    ", "0,1/2,1/2      ",    &
                     "1/2,0,0        ", "x,x,x          ", "x,0,0          ",    &
                     "x,0,1/2        ", "x,1/2,0        ", "x,1/2,1/2      ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(233)= wyck_info_type("F 2 3       ", 7,     &
                   ["0,0,0          ", "1/2,1/2,1/2    ", "1/4,1/4,1/4    ",    &
                     "3/4,3/4,3/4    ", "x,x,x          ", "x,0,0          ",    &
                     "x,1/4,1/4      ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(234)= wyck_info_type("I 2 3       ", 5,     &
                   ["0,0,0          ", "0,1/2,1/2      ", "x,x,x          ",    &
                     "x,0,0          ", "x,1/2,0        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(235)= wyck_info_type("P 21 3      ", 1,     &
                   ["x,x,x          ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(236)= wyck_info_type("I 21 3      ", 2,     &
                   ["x,x,x          ", "x,0,1/4        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(237)= wyck_info_type("P M -3      ",11,     &
                   ["0,0,0          ", "1/2,1/2,1/2    ", "0,1/2,1/2      ",    &
                     "1/2,0,0        ", "x,0,0          ", "x,0,1/2        ",    &
                     "x,1/2,0        ", "x,1/2,1/2      ", "x,x,x          ",    &
                     "0,y,z          ", "1/2,y,z        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(238)= wyck_info_type("P N -3:1    ", 7,     &
                   ["0,0,0          ", "1/4,1/4,1/4    ", "3/4,3/4,3/4    ",    &
                     "0,1/2,1/2      ", "x,x,x          ", "x,0,0          ",    &
                     "x,1/2,0        ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(239)= wyck_info_type("P N -3      ", 7,     &
                   ["1/4,1/4,1/4    ", "0,0,0          ", "1/2,1/2,1/2    ",    &
                     "1/4,3/4,3/4    ", "x,x,x          ", "x,1/4,1/4      ",    &
                     "x,3/4,1/4      ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(240)= wyck_info_type("F M -3      ", 8,     &
                   ["0,0,0          ", "1/2,1/2,1/2    ", "1/4,1/4,1/4    ",    &
                     "0,1/4,1/4      ", "x,0,0          ", "x,x,x          ",    &
                     "x,1/4,1/4      ", "0,y,z          ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(241)= wyck_info_type("F D -3:1    ", 6,     &
                   ["0,0,0          ", "1/2,1/2,1/2    ", "1/8,1/8,1/8    ",    &
                     "5/8,5/8,5/8    ", "x,x,x          ", "x,0,0          ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(242)= wyck_info_type("F D -3      ", 6,     &
                   ["1/8,1/8,1/8    ", "5/8,5/8,5/8    ", "0,0,0          ",    &
                     "1/2,1/2,1/2    ", "x,x,x          ", "x,1/8,1/8      ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(243)= wyck_info_type("I M -3      ", 7,     &
                   ["0,0,0          ", "0,1/2,1/2      ", "1/4,1/4,1/4    ",    &
                     "x,0,0          ", "x,0,1/2        ", "x,x,x          ",    &
                     "0,y,z          ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(244)= wyck_info_type("P A -3      ", 3,     &
                   ["0,0,0          ", "1/2,1/2,1/2    ", "x,x,x          ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(245)= wyck_info_type("I A -3      ", 4,     &
                   ["0,0,0          ", "1/4,1/4,1/4    ", "x,x,x          ",    &
                     "x,0,1/4        ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(246)= wyck_info_type("P 4 3 2     ",10,     &
                   ["0,0,0          ", "1/2,1/2,1/2    ", "0,1/2,1/2      ",    &
                     "1/2,0,0        ", "x,0,0          ", "x,1/2,1/2      ",    &
                     "x,x,x          ", "x,1/2,0        ", "0,y,y          ",    &
                     "1/2,y,y        ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(247)= wyck_info_type("P 42 3 2    ",12,     &
                   ["0,0,0          ", "1/4,1/4,1/4    ", "3/4,3/4,3/4    ",    &
                     "0,1/2,1/2      ", "1/4,0,1/2      ", "1/4,1/2,0      ",    &
                     "x,x,x          ", "x,0,0          ", "x,0,1/2        ",    &
                     "x,1/2,0        ", "1/4,y,-y+1/2   ", "1/4,y,y+1/2    ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(248)= wyck_info_type("F 4 3 2    ", 9,     &
                   ["0,0,0          ", "1/2,1/2,1/2    ", "1/4,1/4,1/4    ",    &
                     "0,1/4,1/4      ", "x,0,0          ", "x,x,x          ",    &
                     "0,y,y          ", "1/2,y,y        ", "x,1/4,1/4      ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(249)= wyck_info_type("F 41 3 2    ", 7,     &
                   ["0,0,0          ", "1/2,1/2,1/2    ", "1/8,1/8,1/8    ",    &
                     "5/8,5/8,5/8    ", "x,x,x          ", "x,0,0          ",    &
                     "1/8,y,-y+1/4   ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(250)= wyck_info_type("I 4 3 2     ", 9,     &
                   ["0,0,0          ", "0,1/2,1/2      ", "1/4,1/4,1/4    ",    &
                     "1/4,1/2,0      ", "x,0,0          ", "x,x,x          ",    &
                     "x,1/2,0        ", "0,y,y          ", "1/4,y,-y+1/2   ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(251)= wyck_info_type("P 43 3 2    ", 4,     &
                   ["1/8,1/8,1/8    ", "5/8,5/8,5/8    ", "x,x,x          ",    &
                     "1/8,y,-y+1/4   ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(252)= wyck_info_type("P 41 3 2    ", 4,     &
                   ["3/8,3/8,3/8    ", "7/8,7/8,7/8    ", "x,x,x          ",    &
                     "1/8,y,y+1/4    ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(253)= wyck_info_type("I 41 3 2    ", 8,     &
                   ["1/8,1/8,1/8    ", "7/8,7/8,7/8    ", "1/8,0,1/4      ",    &
                     "5/8,0,1/4      ", "x,x,x          ", "x,0,1/4        ",    &
                     "1/8,y,y+1/4    ", "1/8,y,-y+1/4   ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(254)= wyck_info_type("P -4 3 M    ", 9,     &
                   ["0,0,0          ", "1/2,1/2,1/2    ", "0,1/2,1/2      ",    &
                     "1/2,0,0        ", "x,x,x          ", "x,0,0          ",    &
                     "x,1/2,1/2      ", "x,1/2,0        ", "x,x,z          ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(255)= wyck_info_type("F -4 3 M    ", 8,     &
                   ["0,0,0          ", "1/2,1/2,1/2    ", "1/4,1/4,1/4    ",    &
                     "3/4,3/4,3/4    ", "x,x,x          ", "x,0,0          ",    &
                     "x,1/4,1/4      ", "x,x,z          ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(256)= wyck_info_type("I -4 3 M    ", 7,     &
                   ["0,0,0          ", "0,1/2,1/2      ", "x,x,x          ",    &
                     "1/4,1/2,0      ", "x,0,0          ", "x,1/2,0        ",    &
                     "x,x,z          ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(257)= wyck_info_type("P -4 3 N    ", 8,     &
                   ["0,0,0          ", "0,1/2,1/2      ", "1/4,1/2,0      ",    &
                     "1/4,0,1/2      ", "x,x,x          ", "x,0,0          ",    &
                     "x,1/2,0        ", "x,0,1/2        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(258)= wyck_info_type("F -4 3 C    ", 7,     &
                   ["0,0,0          ", "1/4,1/4,1/4    ", "0,1/4,1/4      ",    &
                     "1/4,0,0        ", "x,x,x          ", "x,0,0          ",    &
                     "x,1/4,1/4      ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(259)= wyck_info_type("I -4 3 D    ", 4,     &
                   ["3/8,0,1/4      ", "7/8,0,1/4      ", "x,x,x          ",    &
                     "x,0,1/4        ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(260)= wyck_info_type("P M -3 M    ",13,     &
                   ["0,0,0          ", "1/2,1/2,1/2    ", "0,1/2,1/2      ",    &
                     "1/2,0,0        ", "x,0,0          ", "x,1/2,1/2      ",    &
                     "x,x,x          ", "x,1/2,0        ", "0,y,y          ",    &
                     "1/2,y,y        ", "0,y,z          ", "1/2,y,z        ",    &
                     "x,x,z          ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(261)= wyck_info_type("P N -3 N:1  ", 8,     &
                   ["0,0,0          ", "0,1/2,1/2      ", "1/4,1/4,1/4    ",    &
                     "1/4,0,1/2      ", "x,0,0          ", "x,x,x          ",    &
                     "x,0,1/2        ", "0,y,y          ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(262)= wyck_info_type("P N -3 N    ", 8,     &
                   ["1/4,1/4,1/4    ", "3/4,1/4,1/4    ", "0,0,0          ",    &
                     "0,3/4,1/4      ", "x,1/4,1/4      ", "x,x,x          ",    &
                     "x,3/4,1/4      ", "1/4,y,y        ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(263)= wyck_info_type("P M -3 N    ",11,     &
                   ["0,0,0          ", "0,1/2,1/2      ", "1/4,0,1/2      ",    &
                     "1/4,1/2,0      ", "1/4,1/4,1/4    ", "x,0,0          ",    &
                     "x,0,1/2        ", "x,1/2,0        ", "x,x,x          ",    &
                     "1/4,y,y+1/2    ", "0,y,z          ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(264)= wyck_info_type("P N -3 M:1  ",11,     &
                   ["0,0,0          ", "1/4,1/4,1/4    ", "3/4,3/4,3/4    ",    &
                     "0,1/2,1/2      ", "x,x,x          ", "1/4,0,1/2      ",    &
                     "x,0,0          ", "x,0,1/2        ", "1/4,y,-y+1/2   ",    &
                     "1/4,y,y+1/2    ", "x,x,z          ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(265)= wyck_info_type("P N -3 M    ",11,     &
                   ["1/4,1/4,1/4    ", "0,0,0          ", "1/2,1/2,1/2    ",    &
                     "1/4,3/4,3/4    ", "x,x,x          ", "1/2,1/4,3/4    ",    &
                     "x,1/4,1/4      ", "x,1/4,3/4      ", "1/2,y,y+1/2    ",    &
                     "1/2,y,-y       ", "x,x,z          ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(266)= wyck_info_type("F M -3 M    ",11,     &
                   ["0,0,0          ", "1/2,1/2,1/2    ", "1/4,1/4,1/4    ",    &
                     "0,1/4,1/4      ", "x,0,0          ", "x,x,x          ",    &
                     "x,1/4,1/4      ", "0,y,y          ", "1/2,y,y        ",    &
                     "0,y,z          ", "x,x,z          ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(267)= wyck_info_type("F M -3 C    ", 9,     &
                   ["1/4,1/4,1/4    ", "0,0,0          ", "1/4,0,0        ",    &
                     "0,1/4,1/4      ", "x,0,0          ", "x,1/4,1/4      ",    &
                     "x,x,x          ", "1/4,y,y        ", "0,y,z          ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(268)= wyck_info_type("F D -3 M:1  ", 8,     &
                   ["0,0,0          ", "1/2,1/2,1/2    ", "1/8,1/8,1/8    ",    &
                     "5/8,5/8,5/8    ", "x,x,x          ", "x,0,0          ",    &
                     "x,x,z          ", "1/8,y,-y+1/4   ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(269)= wyck_info_type("F D -3 M    ", 8,     &
                   ["1/8,1/8,1/8    ", "3/8,3/8,3/8    ", "0,0,0          ",    &
                     "1/2,1/2,1/2    ", "x,x,x          ", "x,1/8,1/8      ",    &
                     "x,x,z          ", "0,y,-y         ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(270)= wyck_info_type("F D -3 C:1  ", 7,     &
                   ["0,0,0          ", "1/8,1/8,1/8    ", "3/8,3/8,3/8    ",    &
                     "1/4,0,0        ", "x,x,x          ", "x,0,0          ",    &
                     "1/8,y,-y+1/4   ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(271)= wyck_info_type("F D -3 C    ", 7,     &
                   ["1/8,1/8,1/8    ", "1/4,1/4,1/4    ", "0,0,0          ",    &
                     "7/8,1/8,1/8    ", "x,x,x          ", "x,1/8,1/8      ",    &
                     "1/4,y,-y       ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
     wyckoff_info(272)= wyck_info_type("I M -3 M    ",11,     &
                   ["0,0,0          ", "0,1/2,1/2      ", "1/4,1/4,1/4    ",    &
                     "1/4,0,1/2      ", "x,0,0          ", "x,x,x          ",    &
                     "x,0,1/2        ", "0,y,y          ", "1/4,y,-y+1/2   ",    &
                     "0,y,z          ", "x,x,z          ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )
      wyckoff_info(273)= wyck_info_type("I A -3 D    ", 7,     &
                   ["0,0,0          ", "1/8,1/8,1/8    ", "1/8,0,1/4      ",    &
                     "3/8,0,1/4      ", "x,x,x          ", "x,0,1/4        ",    &
                     "1/8,y,-y+1/4   ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               ", "               ",    &
                     "               ", "               "] )


      wyckoff_info_loaded=.true.
   End Subroutine Set_Wyckoff_Info

   !!--++
   !!--++ SUBROUTINE SET_IT_GEN
   !!--++
   !!--++    Fills the components of the Spg_Gen character variable
   !!--++    Called once by the public subroutine Get_Generators
   !!--++
   !!--++ 20/04/2019
   !!
   Module Subroutine Set_IT_Gen()

    it_spg_gen(  1) =  "P 1       : x,y,z "
    it_spg_gen(  2) =  "P -1      : -x,-y,-z "
    it_spg_gen(  3) =  "P 2       : -x,y,-z "
    it_spg_gen(  4) =  "P 21      : -x,y+1/2,-z "
    it_spg_gen(  5) =  "C 2       : x+1/2,y+1/2,z; -x,y,-z "
    it_spg_gen(  6) =  "P m       : x,-y,z "
    it_spg_gen(  7) =  "P c       : x,-y,z+1/2 "
    it_spg_gen(  8) =  "C m       : x+1/2,y+1/2,z; x,-y,z "
    it_spg_gen(  9) =  "C c       : x+1/2,y+1/2,z; x,-y,z+1/2 "
    it_spg_gen( 10) =  "P 2/m     : -x,y,-z; -x,-y,-z "
    it_spg_gen( 11) =  "P 21/m    : -x,y+1/2,-z; -x,-y,-z "
    it_spg_gen( 12) =  "C 2/m     : x+1/2,y+1/2,z; -x,y,-z; -x,-y,-z "
    it_spg_gen( 13) =  "P 2/c     : -x,y,-z+1/2; -x,-y,-z "
    it_spg_gen( 14) =  "P 21/c    : -x,y+1/2,-z+1/2; -x,-y,-z "
    it_spg_gen( 15) =  "C 2/c     : x+1/2,y+1/2,z; -x,y,-z+1/2; -x,-y,-z "
    it_spg_gen( 16) =  "P 2 2 2   : -x,-y,z; -x,y,-z "
    it_spg_gen( 17) =  "P 2 2 21  : -x,-y,z+1/2; -x,y,-z+1/2 "
    it_spg_gen( 18) =  "P 21 21 2 : -x,-y,z; -x+1/2,y+1/2,-z "
    it_spg_gen( 19) =  "P 21 21 21: -x+1/2,-y,z+1/2; -x,y+1/2,-z+1/2 "
    it_spg_gen( 20) =  "C 2 2 21  : x+1/2,y+1/2,z; -x,-y,z+1/2; -x,y,-z+1/2 "
    it_spg_gen( 21) =  "C 2 2 2   : x+1/2,y+1/2,z; -x,-y,z; -x,y,-z "
    it_spg_gen( 22) =  "F 2 2 2   : x+1/2,y+1/2,z; x+1/2,y,z+1/2; -x,-y,z; -x,y,-z "
    it_spg_gen( 23) =  "I 2 2 2   : x+1/2,y+1/2,z+1/2; -x,-y,z; -x,y,-z "
    it_spg_gen( 24) =  "I 21 21 21: x+1/2,y+1/2,z+1/2; -x+1/2,-y,z+1/2; -x,y+1/2,-z+1/2 "
    it_spg_gen( 25) =  "P m m 2   : -x,-y,z; x,-y,z "
    it_spg_gen( 26) =  "P m c 21  : -x,-y,z+1/2; x,-y,z+1/2 "
    it_spg_gen( 27) =  "P c c 2   : -x,-y,z; x,-y,z+1/2 "
    it_spg_gen( 28) =  "P m a 2   : -x,-y,z; x+1/2,-y,z "
    it_spg_gen( 29) =  "P c a 21  : -x,-y,z+1/2; x+1/2,-y,z "
    it_spg_gen( 30) =  "P n c 2   : -x,-y,z; x,-y+1/2,z+1/2 "
    it_spg_gen( 31) =  "P m n 21  : -x+1/2,-y,z+1/2; x+1/2,-y,z+1/2 "
    it_spg_gen( 32) =  "P b a 2   : -x,-y,z; x+1/2,-y+1/2,z "
    it_spg_gen( 33) =  "P n a 21  : -x,-y,z+1/2; x+1/2,-y+1/2,z "
    it_spg_gen( 34) =  "P n n 2   : -x,-y,z; x+1/2,-y+1/2,z+1/2 "
    it_spg_gen( 35) =  "C m m 2   : x+1/2,y+1/2,z; -x,-y,z; x,-y,z "
    it_spg_gen( 36) =  "C m c 21  : x+1/2,y+1/2,z; -x,-y,z+1/2; x,-y,z+1/2 "
    it_spg_gen( 37) =  "C c c 2   : x+1/2,y+1/2,z; -x,-y,z; x,-y,z+1/2 "
    it_spg_gen( 38) =  "A m m 2   : x,y+1/2,z+1/2; -x,-y,z; x,-y,z "
    it_spg_gen( 39) =  "A b m 2   : x,y+1/2,z+1/2; -x,-y,z; x,-y+1/2,z "
    it_spg_gen( 40) =  "A m a 2   : x,y+1/2,z+1/2; -x,-y,z; x+1/2,-y,z "
    it_spg_gen( 41) =  "A b a 2   : x,y+1/2,z+1/2; -x,-y,z; x+1/2,-y+1/2,z "
    it_spg_gen( 42) =  "F m m 2   : x+1/2,y+1/2,z; x+1/2,y,z+1/2; -x,-y,z; x,-y,z "
    it_spg_gen( 43) =  "F d d 2   : x+1/2,y+1/2,z; x+1/2,y,z+1/2; -x,-y,z; x+1/4,-y+1/4,z+1/4 "
    it_spg_gen( 44) =  "I m m 2   : x+1/2,y+1/2,z+1/2; -x,-y,z; x,-y,z "
    it_spg_gen( 45) =  "I b a 2   : x+1/2,y+1/2,z+1/2; -x,-y,z; x+1/2,-y+1/2,z "
    it_spg_gen( 46) =  "I m a 2   : x+1/2,y+1/2,z+1/2; -x,-y,z; x+1/2,-y,z "
    it_spg_gen( 47) =  "P m m m   : -x,-y,z; -x,y,-z; -x,-y,-z "
    it_spg_gen( 48) =  "P n n n   : -x+1/2,-y+1/2,z; -x+1/2,y,-z+1/2; -x,-y,-z "
    it_spg_gen( 49) =  "P c c m   : -x,-y,z; -x,y,-z+1/2; -x,-y,-z "
    it_spg_gen( 50) =  "P b a n   : -x+1/2,-y+1/2,z; -x+1/2,y,-z; -x,-y,-z "
    it_spg_gen( 51) =  "P m m a   : -x+1/2,-y,z; -x,y,-z; -x,-y,-z "
    it_spg_gen( 52) =  "P n n a   : -x+1/2,-y,z; -x+1/2,y+1/2,-z+1/2; -x,-y,-z "
    it_spg_gen( 53) =  "P m n a   : -x+1/2,-y,z+1/2; -x+1/2,y,-z+1/2; -x,-y,-z "
    it_spg_gen( 54) =  "P c c a   : -x+1/2,-y,z; -x,y,-z+1/2; -x,-y,-z "
    it_spg_gen( 55) =  "P b a m   : -x,-y,z; -x+1/2,y+1/2,-z; -x,-y,-z "
    it_spg_gen( 56) =  "P c c n   : -x+1/2,-y+1/2,z; -x,y+1/2,-z+1/2; -x,-y,-z "
    it_spg_gen( 57) =  "P b c m   : -x,-y,z+1/2; -x,y+1/2,-z+1/2; -x,-y,-z "
    it_spg_gen( 58) =  "P n n m   : -x,-y,z; -x+1/2,y+1/2,-z+1/2; -x,-y,-z "
    it_spg_gen( 59) =  "P m m n   : -x+1/2,-y+1/2,z; -x,y+1/2,-z; -x,-y,-z "
    it_spg_gen( 60) =  "P b c n   : -x+1/2,-y+1/2,z+1/2; -x,y,-z+1/2; -x,-y,-z "
    it_spg_gen( 61) =  "P b c a   : -x+1/2,-y,z+1/2; -x,y+1/2,-z+1/2; -x,-y,-z "
    it_spg_gen( 62) =  "P n m a   : -x+1/2,-y,z+1/2; -x,y+1/2,-z; -x,-y,-z "
    it_spg_gen( 63) =  "C m c m   : x+1/2,y+1/2,z; -x,-y,z+1/2; -x,y,-z+1/2; -x,-y,-z "
    it_spg_gen( 64) =  "C m c a   : x+1/2,y+1/2,z; -x,-y+1/2,z+1/2; -x,y+1/2,-z+1/2; -x,-y,-z "
    it_spg_gen( 65) =  "C m m m   : x+1/2,y+1/2,z; -x,-y,z; -x,y,-z; -x,-y,-z "
    it_spg_gen( 66) =  "C c c m   : x+1/2,y+1/2,z; -x,-y,z; -x,y,-z+1/2; -x,-y,-z "
    it_spg_gen( 67) =  "C m m a   : x+1/2,y+1/2,z; -x,-y+1/2,z; -x,y+1/2,-z; -x,-y,-z "
    it_spg_gen( 68) =  "C c c a   : x+1/2,y+1/2,z; -x+1/2,-y,z; -x,y,-z+1/2; -x,-y,-z "
    it_spg_gen( 69) =  "F m m m   : x+1/2,y+1/2,z; x+1/2,y,z+1/2; -x,-y,z; -x,y,-z; -x,-y,-z "
    it_spg_gen( 70) =  "F d d d   : x+1/2,y+1/2,z; x+1/2,y,z+1/2; -x+3/4,-y+3/4,z; -x+3/4,y,-z+3/4; -x,-y,-z "
    it_spg_gen( 71) =  "I m m m   : x+1/2,y+1/2,z+1/2; -x,-y,z; -x,y,-z; -x,-y,-z "
    it_spg_gen( 72) =  "I b a m   : x+1/2,y+1/2,z+1/2; -x,-y,z; -x+1/2,y+1/2,-z; -x,-y,-z "
    it_spg_gen( 73) =  "I b c a   : x+1/2,y+1/2,z+1/2; -x+1/2,-y,z+1/2; -x,y+1/2,-z+1/2; -x,-y,-z "
    it_spg_gen( 74) =  "I m m a   : x+1/2,y+1/2,z+1/2; -x,-y+1/2,z; -x,y+1/2,-z; -x,-y,-z "
    it_spg_gen( 75) =  "P 4       : -x,-y,z; -y,x,z "
    it_spg_gen( 76) =  "P 41      : -x,-y,z+1/2; -y,x,z+1/4 "
    it_spg_gen( 77) =  "P 42      : -x,-y,z; -y,x,z+1/2 "
    it_spg_gen( 78) =  "P 43      : -x,-y,z+1/2; -y,x,z+3/4 "
    it_spg_gen( 79) =  "I 4       : x+1/2,y+1/2,z+1/2; -x,-y,z; -y,x,z "
    it_spg_gen( 80) =  "I 41      : x+1/2,y+1/2,z+1/2; -x+1/2,-y+1/2,z+1/2; -y,x+1/2,z+1/4 "
    it_spg_gen( 81) =  "P -4      : -x,-y,z; y,-x,-z "
    it_spg_gen( 82) =  "I -4      : x+1/2,y+1/2,z+1/2; -x,-y,z; y,-x,-z "
    it_spg_gen( 83) =  "P 4/m     : -x,-y,z; -y,x,z; -x,-y,-z "
    it_spg_gen( 84) =  "P 42/m    : -x,-y,z; -y,x,z+1/2; -x,-y,-z "
    it_spg_gen( 85) =  "P 4/n     : -x+1/2,-y+1/2,z; -y+1/2,x,z; -x,-y,-z "
    it_spg_gen( 86) =  "P 42/n    : -x+1/2,-y+1/2,z; -y,x+1/2,z+1/2; -x,-y,-z "
    it_spg_gen( 87) =  "I 4/m     : x+1/2,y+1/2,z+1/2; -x,-y,z; -y,x,z; -x,-y,-z "
    it_spg_gen( 88) =  "I 41/a    : x+1/2,y+1/2,z+1/2; -x+1/2,-y,z+1/2; -y+3/4,x+1/4,z+1/4; -x,-y,-z "
    it_spg_gen( 89) =  "P 4 2 2   : -x,-y,z; -y,x,z; -x,y,-z "
    it_spg_gen( 90) =  "P 4 21 2  : -x,-y,z; -y+1/2,x+1/2,z; -x+1/2,y+1/2,-z "
    it_spg_gen( 91) =  "P 41 2 2  : -x,-y,z+1/2; -y,x,z+1/4; -x,y,-z "
    it_spg_gen( 92) =  "P 41 21 2 : -x,-y,z+1/2; -y+1/2,x+1/2,z+1/4; -x+1/2,y+1/2,-z+1/4 "
    it_spg_gen( 93) =  "P 42 2 2  : -x,-y,z; -y,x,z+1/2; -x,y,-z "
    it_spg_gen( 94) =  "P 42 21 2 : -x,-y,z; -y+1/2,x+1/2,z+1/2; -x+1/2,y+1/2,-z+1/2 "
    it_spg_gen( 95) =  "P 43 2 2  : -x,-y,z+1/2; -y,x,z+3/4; -x,y,-z "
    it_spg_gen( 96) =  "P 43 21 2 : -x,-y,z+1/2; -y+1/2,x+1/2,z+3/4; -x+1/2,y+1/2,-z+3/4 "
    it_spg_gen( 97) =  "I 4 2 2   : x+1/2,y+1/2,z+1/2; -x,-y,z; -y,x,z; -x,y,-z "
    it_spg_gen( 98) =  "I 41 2 2  : x+1/2,y+1/2,z+1/2; -x+1/2,-y+1/2,z+1/2; -y,x+1/2,z+1/4; -x+1/2,y,-z+3/4 "
    it_spg_gen( 99) =  "P 4 m m   : -x,-y,z; -y,x,z; x,-y,z "
    it_spg_gen(100) =  "P 4 b m   : -x,-y,z; -y,x,z; x+1/2,-y+1/2,z "
    it_spg_gen(101) =  "P 42 c m  : -x,-y,z; -y,x,z+1/2; x,-y,z+1/2 "
    it_spg_gen(102) =  "P 42 n m  : -x,-y,z; -y+1/2,x+1/2,z+1/2; x+1/2,-y+1/2,z+1/2 "
    it_spg_gen(103) =  "P 4 c c   : -x,-y,z; -y,x,z; x,-y,z+1/2 "
    it_spg_gen(104) =  "P 4 n c   : -x,-y,z; -y,x,z; x+1/2,-y+1/2,z+1/2 "
    it_spg_gen(105) =  "P 42 m c  : -x,-y,z; -y,x,z+1/2; x,-y,z "
    it_spg_gen(106) =  "P 42 b c  : -x,-y,z; -y,x,z+1/2; x+1/2,-y+1/2,z "
    it_spg_gen(107) =  "I 4 m m   : x+1/2,y+1/2,z+1/2; -x,-y,z; -y,x,z; x,-y,z "
    it_spg_gen(108) =  "I 4 c m   : x+1/2,y+1/2,z+1/2; -x,-y,z; -y,x,z; x,-y,z+1/2 "
    it_spg_gen(109) =  "I 41 m d  : x+1/2,y+1/2,z+1/2; -x+1/2,-y+1/2,z+1/2; -y,x+1/2,z+1/4; x,-y,z "
    it_spg_gen(110) =  "I 41 c d  : x+1/2,y+1/2,z+1/2; -x+1/2,-y+1/2,z+1/2; -y,x+1/2,z+1/4; x,-y,z+1/2 "
    it_spg_gen(111) =  "P -4 2 m  : -x,-y,z; y,-x,-z; -x,y,-z "
    it_spg_gen(112) =  "P -4 2 c  : -x,-y,z; y,-x,-z; -x,y,-z+1/2 "
    it_spg_gen(113) =  "P -4 21 m : -x,-y,z; y,-x,-z; -x+1/2,y+1/2,-z "
    it_spg_gen(114) =  "P -4 21 c : -x,-y,z; y,-x,-z; -x+1/2,y+1/2,-z+1/2 "
    it_spg_gen(115) =  "P -4 m 2  : -x,-y,z; y,-x,-z; x,-y,z "
    it_spg_gen(116) =  "P -4 c 2  : -x,-y,z; y,-x,-z; x,-y,z+1/2 "
    it_spg_gen(117) =  "P -4 b 2  : -x,-y,z; y,-x,-z; x+1/2,-y+1/2,z "
    it_spg_gen(118) =  "P -4 n 2  : -x,-y,z; y,-x,-z; x+1/2,-y+1/2,z+1/2 "
    it_spg_gen(119) =  "I -4 m 2  : x+1/2,y+1/2,z+1/2; -x,-y,z; y,-x,-z; x,-y,z "
    it_spg_gen(120) =  "I -4 c 2  : x+1/2,y+1/2,z+1/2; -x,-y,z; y,-x,-z; x,-y,z+1/2 "
    it_spg_gen(121) =  "I -4 2 m  : x+1/2,y+1/2,z+1/2; -x,-y,z; y,-x,-z; -x,y,-z "
    it_spg_gen(122) =  "I -4 2 d  : x+1/2,y+1/2,z+1/2; -x,-y,z; y,-x,-z; -x+1/2,y,-z+3/4 "
    it_spg_gen(123) =  "P 4/m m m : -x,-y,z; -y,x,z; -x,y,-z; -x,-y,-z "
    it_spg_gen(124) =  "P 4/m c c : -x,-y,z; -y,x,z; -x,y,-z+1/2; -x,-y,-z "
    it_spg_gen(125) =  "P 4/n b m : -x+1/2,-y+1/2,z; -y+1/2,x,z; -x+1/2,y,-z; -x,-y,-z "
    it_spg_gen(126) =  "P 4/n n c : -x+1/2,-y+1/2,z; -y+1/2,x,z; -x+1/2,y,-z+1/2; -x,-y,-z "
    it_spg_gen(127) =  "P 4/m b m : -x,-y,z; -y,x,z; -x+1/2,y+1/2,-z; -x,-y,-z "
    it_spg_gen(128) =  "P 4/m n c : -x,-y,z; -y,x,z; -x+1/2,y+1/2,-z+1/2; -x,-y,-z "
    it_spg_gen(129) =  "P 4/n m m : -x+1/2,-y+1/2,z; -y+1/2,x,z; -x,y+1/2,-z; -x,-y,-z "
    it_spg_gen(130) =  "P 4/n c c : -x+1/2,-y+1/2,z; -y+1/2,x,z; -x,y+1/2,-z+1/2; -x,-y,-z "
    it_spg_gen(131) =  "P 42/m m c: -x,-y,z; -y,x,z+1/2; -x,y,-z; -x,-y,-z "
    it_spg_gen(132) =  "P 42/m c m: -x,-y,z; -y,x,z+1/2; -x,y,-z+1/2; -x,-y,-z "
    it_spg_gen(133) =  "P 42/n b c: -x+1/2,-y+1/2,z; -y+1/2,x,z+1/2; -x+1/2,y,-z; -x,-y,-z "
    it_spg_gen(134) =  "P 42/n n m: -x+1/2,-y+1/2,z; -y+1/2,x,z+1/2; -x+1/2,y,-z+1/2; -x,-y,-z "
    it_spg_gen(135) =  "P 42/m b c: -x,-y,z; -y,x,z+1/2; -x+1/2,y+1/2,-z; -x,-y,-z "
    it_spg_gen(136) =  "P 42/m n m: -x,-y,z; -y+1/2,x+1/2,z+1/2; -x+1/2,y+1/2,-z+1/2; -x,-y,-z "
    it_spg_gen(137) =  "P 42/n m c: -x+1/2,-y+1/2,z; -y+1/2,x,z+1/2; -x,y+1/2,-z; -x,-y,-z "
    it_spg_gen(138) =  "P 42/n c m: -x+1/2,-y+1/2,z; -y+1/2,x,z+1/2; -x,y+1/2,-z+1/2; -x,-y,-z "
    it_spg_gen(139) =  "I 4/m m m : x+1/2,y+1/2,z+1/2; -x,-y,z; -y,x,z; -x,y,-z; -x,-y,-z "
    it_spg_gen(140) =  "I 4/m c m : x+1/2,y+1/2,z+1/2; -x,-y,z; -y,x,z; -x,y,-z+1/2; -x,-y,-z "
    it_spg_gen(141) =  "I 41/a m d: x+1/2,y+1/2,z+1/2; -x+1/2,-y,z+1/2; -y+1/4,x+3/4,z+1/4; -x+1/2,y,-z+1/2; -x,-y,-z "
    it_spg_gen(142) =  "I 41/a c d: x+1/2,y+1/2,z+1/2; -x+1/2,-y,z+1/2; -y+1/4,x+3/4,z+1/4; -x+1/2,y,-z; -x,-y,-z "
    it_spg_gen(143) =  "P 3       : -y,x-y,z "
    it_spg_gen(144) =  "P 31      : -y,x-y,z+1/3 "
    it_spg_gen(145) =  "P 32      : -y,x-y,z+2/3 "
    it_spg_gen(146) =  "R 3       : x+1/3,y+2/3,z+2/3; -y,x-y,z "
    it_spg_gen(147) =  "P -3      : -y,x-y,z; -x,-y,-z "
    it_spg_gen(148) =  "R -3      : x+1/3,y+2/3,z+2/3; -y,x-y,z; -x,-y,-z "
    it_spg_gen(149) =  "P 3 1 2   : -y,x-y,z; -y,-x,-z "
    it_spg_gen(150) =  "P 3 2 1   : -y,x-y,z; y,x,-z "
    it_spg_gen(151) =  "P 31 1 2  : -y,x-y,z+1/3; -y,-x,-z+2/3 "
    it_spg_gen(152) =  "P 31 2 1  : -y,x-y,z+1/3; y,x,-z "
    it_spg_gen(153) =  "P 32 1 2  : -y,x-y,z+2/3; -y,-x,-z+1/3 "
    it_spg_gen(154) =  "P 32 2 1  : -y,x-y,z+2/3; y,x,-z "
    it_spg_gen(155) =  "R 3 2     : x+1/3,y+2/3,z+2/3; -y,x-y,z; y,x,-z "
    it_spg_gen(156) =  "P 3 m 1   : -y,x-y,z; -y,-x,z "
    it_spg_gen(157) =  "P 3 1 m   : -y,x-y,z; y,x,z "
    it_spg_gen(158) =  "P 3 c 1   : -y,x-y,z; -y,-x,z+1/2 "
    it_spg_gen(159) =  "P 3 1 c   : -y,x-y,z; y,x,z+1/2 "
    it_spg_gen(160) =  "R 3 m     : x+1/3,y+2/3,z+2/3; -y,x-y,z; -y,-x,z "
    it_spg_gen(161) =  "R 3 c     : x+1/3,y+2/3,z+2/3; -y,x-y,z; -y,-x,z+1/2 "
    it_spg_gen(162) =  "P -3 1 m  : -y,x-y,z; -y,-x,-z; -x,-y,-z "
    it_spg_gen(163) =  "P -3 1 c  : -y,x-y,z; -y,-x,-z+1/2; -x,-y,-z "
    it_spg_gen(164) =  "P -3 m 1  : -y,x-y,z; y,x,-z; -x,-y,-z "
    it_spg_gen(165) =  "P -3 c 1  : -y,x-y,z; y,x,-z+1/2; -x,-y,-z "
    it_spg_gen(166) =  "R -3 m    : x+1/3,y+2/3,z+2/3; -y,x-y,z; y,x,-z; -x,-y,-z "
    it_spg_gen(167) =  "R -3 c    : x+1/3,y+2/3,z+2/3; -y,x-y,z; y,x,-z+1/2; -x,-y,-z "
    it_spg_gen(168) =  "P 6       : -y,x-y,z; -x,-y,z "
    it_spg_gen(169) =  "P 61      : -y,x-y,z+1/3; -x,-y,z+1/2 "
    it_spg_gen(170) =  "P 65      : -y,x-y,z+2/3; -x,-y,z+1/2 "
    it_spg_gen(171) =  "P 62      : -y,x-y,z+2/3; -x,-y,z "
    it_spg_gen(172) =  "P 64      : -y,x-y,z+1/3; -x,-y,z "
    it_spg_gen(173) =  "P 63      : -y,x-y,z; -x,-y,z+1/2 "
    it_spg_gen(174) =  "P -6      : -y,x-y,z; x,y,-z "
    it_spg_gen(175) =  "P 6/m     : -y,x-y,z; -x,-y,z; -x,-y,-z "
    it_spg_gen(176) =  "P 63/m    : -y,x-y,z; -x,-y,z+1/2; -x,-y,-z "
    it_spg_gen(177) =  "P 6 2 2   : -y,x-y,z; -x,-y,z; y,x,-z "
    it_spg_gen(178) =  "P 61 2 2  : -y,x-y,z+1/3; -x,-y,z+1/2; y,x,-z+1/3 "
    it_spg_gen(179) =  "P 65 2 2  : -y,x-y,z+2/3; -x,-y,z+1/2; y,x,-z+2/3 "
    it_spg_gen(180) =  "P 62 2 2  : -y,x-y,z+2/3; -x,-y,z; y,x,-z+2/3 "
    it_spg_gen(181) =  "P 64 2 2  : -y,x-y,z+1/3; -x,-y,z; y,x,-z+1/3 "
    it_spg_gen(182) =  "P 63 2 2  : -y,x-y,z; -x,-y,z+1/2; y,x,-z "
    it_spg_gen(183) =  "P 6 m m   : -y,x-y,z; -x,-y,z; -y,-x,z "
    it_spg_gen(184) =  "P 6 c c   : -y,x-y,z; -x,-y,z; -y,-x,z+1/2 "
    it_spg_gen(185) =  "P 63 c m  : -y,x-y,z; -x,-y,z+1/2; -y,-x,z+1/2 "
    it_spg_gen(186) =  "P 63 m c  : -y,x-y,z; -x,-y,z+1/2; -y,-x,z "
    it_spg_gen(187) =  "P -6 m 2  : -y,x-y,z; x,y,-z; -y,-x,z "
    it_spg_gen(188) =  "P -6 c 2  : -y,x-y,z; x,y,-z+1/2; -y,-x,z+1/2 "
    it_spg_gen(189) =  "P -6 2 m  : -y,x-y,z; x,y,-z; y,x,-z "
    it_spg_gen(190) =  "P -6 2 c  : -y,x-y,z; x,y,-z+1/2; y,x,-z "
    it_spg_gen(191) =  "P 6/m m m : -y,x-y,z; -x,-y,z; y,x,-z; -x,-y,-z "
    it_spg_gen(192) =  "P 6/m c c : -y,x-y,z; -x,-y,z; y,x,-z+1/2; -x,-y,-z "
    it_spg_gen(193) =  "P 63/m c m: -y,x-y,z; -x,-y,z+1/2; y,x,-z+1/2; -x,-y,-z "
    it_spg_gen(194) =  "P 63/m m c: -y,x-y,z; -x,-y,z+1/2; y,x,-z; -x,-y,-z "
    it_spg_gen(195) =  "P 2 3     : -x,-y,z; -x,y,-z; z,x,y "
    it_spg_gen(196) =  "F 2 3     : x+1/2,y+1/2,z; x+1/2,y,z+1/2; -x,-y,z; -x,y,-z; z,x,y "
    it_spg_gen(197) =  "I 2 3     : x+1/2,y+1/2,z+1/2; -x,-y,z; -x,y,-z; z,x,y "
    it_spg_gen(198) =  "P 21 3    : -x+1/2,-y,z+1/2; -x,y+1/2,-z+1/2; z,x,y "
    it_spg_gen(199) =  "I 21 3    : x+1/2,y+1/2,z+1/2; -x+1/2,-y,z+1/2; -x,y+1/2,-z+1/2; z,x,y "
    it_spg_gen(200) =  "P m -3    : -x,-y,z; -x,y,-z; z,x,y; -x,-y,-z "
    it_spg_gen(201) =  "P n -3    : -x+1/2,-y+1/2,z; -x+1/2,y,-z+1/2; z,x,y; -x,-y,-z "
    it_spg_gen(202) =  "F m -3    : x+1/2,y+1/2,z; x+1/2,y,z+1/2; -x,-y,z; -x,y,-z; z,x,y; -x,-y,-z "
    it_spg_gen(203) =  "F d -3    : x+1/2,y+1/2,z; x+1/2,y,z+1/2; -x+1/4,-y+1/4,z; -x+1/4,y,-z+1/4; z,x,y; -x,-y,-z "
    it_spg_gen(204) =  "I m -3    : x+1/2,y+1/2,z+1/2; -x,-y,z; -x,y,-z; z,x,y; -x,-y,-z "
    it_spg_gen(205) =  "P a -3    : -x+1/2,-y,z+1/2; -x,y+1/2,-z+1/2; z,x,y; -x,-y,-z "
    it_spg_gen(206) =  "I a -3    : x+1/2,y+1/2,z+1/2; -x+1/2,-y,z+1/2; -x,y+1/2,-z+1/2; z,x,y; -x,-y,-z "
    it_spg_gen(207) =  "P 4 3 2   : -x,-y,z; -x,y,-z; z,x,y; y,x,-z "
    it_spg_gen(208) =  "P 42 3 2  : -x,-y,z; -x,y,-z; z,x,y; y+1/2,x+1/2,-z+1/2 "
    it_spg_gen(209) =  "F 4 3 2   : x+1/2,y+1/2,z; x+1/2,y,z+1/2; -x,-y,z; -x,y,-z; z,x,y; y,x,-z "
    it_spg_gen(210) =  "F 41 3 2  : x+1/2,y+1/2,z; x+1/2,y,z+1/2; -x,-y+1/2,z+1/2; -x+1/2,y+1/2,-z; z,x,y; y+3/4,x+1/4,-z+3/4 "
    it_spg_gen(211) =  "I 4 3 2   : x+1/2,y+1/2,z+1/2; -x,-y,z; -x,y,-z; z,x,y; y,x,-z "
    it_spg_gen(212) =  "P 43 3 2  : -x+1/2,-y,z+1/2; -x,y+1/2,-z+1/2; z,x,y; y+1/4,x+3/4,-z+3/4 "
    it_spg_gen(213) =  "P 41 3 2  : -x+1/2,-y,z+1/2; -x,y+1/2,-z+1/2; z,x,y; y+3/4,x+1/4,-z+1/4 "
    it_spg_gen(214) =  "I 41 3 2  : x+1/2,y+1/2,z+1/2; -x+1/2,-y,z+1/2; -x,y+1/2,-z+1/2; z,x,y; y+3/4,x+1/4,-z+1/4 "
    it_spg_gen(215) =  "P -4 3 m  : -x,-y,z; -x,y,-z; z,x,y; y,x,z "
    it_spg_gen(216) =  "F -4 3 m  : x+1/2,y+1/2,z; x+1/2,y,z+1/2; -x,-y,z; -x,y,-z; z,x,y; y,x,z "
    it_spg_gen(217) =  "I -4 3 m  : x+1/2,y+1/2,z+1/2; -x,-y,z; -x,y,-z; z,x,y; y,x,z "
    it_spg_gen(218) =  "P -4 3 n  : -x,-y,z; -x,y,-z; z,x,y; y+1/2,x+1/2,z+1/2 "
    it_spg_gen(219) =  "F -4 3 c  : x+1/2,y+1/2,z; x+1/2,y,z+1/2; -x,-y,z; -x,y,-z; z,x,y; y+1/2,x+1/2,z+1/2 "
    it_spg_gen(220) =  "I -4 3 d  : x+1/2,y+1/2,z+1/2; -x+1/2,-y,z+1/2; -x,y+1/2,-z+1/2; z,x,y; y+1/4,x+1/4,z+1/4 "
    it_spg_gen(221) =  "P m -3 m  : -x,-y,z; -x,y,-z; z,x,y; y,x,-z; -x,-y,-z "
    it_spg_gen(222) =  "P n -3 n  : -x+1/2,-y+1/2,z; -x+1/2,y,-z+1/2; z,x,y; y,x,-z+1/2; -x,-y,-z "
    it_spg_gen(223) =  "P m -3 n  : -x,-y,z; -x,y,-z; z,x,y; y+1/2,x+1/2,-z+1/2; -x,-y,-z "
    it_spg_gen(224) =  "P n -3 m  : -x+1/2,-y+1/2,z; -x+1/2,y,-z+1/2; z,x,y; y+1/2,x+1/2,-z; -x,-y,-z "
    it_spg_gen(225) =  "F m -3 m  : x+1/2,y+1/2,z; x+1/2,y,z+1/2; -x,-y,z; -x,y,-z; z,x,y; y,x,-z; -x,-y,-z "
    it_spg_gen(226) =  "F m -3 c  : x+1/2,y+1/2,z; x+1/2,y,z+1/2; -x,-y,z; -x,y,-z; z,x,y; y+1/2,x+1/2,-z+1/2; -x,-y,-z "

    it_spg_gen(227) =  &
    "F d -3 m  : x+1/2,y+1/2,z; x+1/2,y,z+1/2;-x+3/4,-y+1/4,z+1/2;-x+1/4,y+1/2,-z+3/4;z,x,y;y+3/4,x+1/4,-z+1/2;-x,-y,-z"

    it_spg_gen(228) =  &
    "F d -3 c  : x+1/2,y+1/2,z; x+1/2,y,z+1/2;-x+1/4,-y+3/4,z+1/2;-x+3/4,y+1/2,-z+1/4;z,x,y;y+3/4,x+1/4,-z;-x,-y,-z"

    it_spg_gen(229) =  "I m -3 m  : x+1/2,y+1/2,z+1/2; -x,-y,z; -x,y,-z; z,x,y; y,x,-z; -x,-y,-z "
    it_spg_gen(230) =  "I a -3 d  : x+1/2,y+1/2,z+1/2; -x+1/2,-y,z+1/2; -x,y+1/2,-z+1/2; z,x,y; y+3/4,x+1/4,-z+1/4; -x,-y,-z "

 End Subroutine Set_IT_Gen

End SubModule TAB_Set_SpgT