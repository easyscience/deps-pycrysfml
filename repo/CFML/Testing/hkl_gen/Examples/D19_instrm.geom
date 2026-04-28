!  File with an example of instrument SXTAL file                                                                        
!  Case of D19  (2.50 mm horizontally and 1.56 mm vertically; 640 x 256 pixels; 0.19° x 0.12° at 76 cm).                
!                                                                                                                       
INFO          Four circle diffractometer + 2D cylindrical detector                                                      
NAME          D19                                                                                                       
GEOM          3   Eulerian cradle     !Geom=3 if for using the calculation of normal beam angles gamma,omega,nu         
BLFR          z-up                                                                                                      
DIST_UNITS    mm                                                                                                        
ANGL_UNITS    deg                                                                                                       
DET_TYPE      Cylindrical_Banana  ipsd 3                                                                                
DIST_DET      760.0                                                                                                     
WAVE    0.94602                                                                                                         
DIM_XZ        1600.0  400.0     640  256                                                                                
GAPS_DET      1.5625 2.5000                                                                                             
                                                                                                                        
                                                                                                                        
UBMAT                                                                                                                   
     0.132268995     0.111722998     0.034655999                                                                        
    -0.127693996     0.124035001     0.000000000                                                                        
    -0.035441000    -0.029936001     0.129337996                                                                        
                                                                                                                        
!0.946018   -0.0500    0.0139                                                                                           
!UB Matrix                                                                                                              
!    -0.077635474   -0.007748365   -0.043887280         Final [UB] matrix                                               
!     0.105111900   -0.070285551   -0.023786094            *** rafd19 ***                                               
!    -0.055269334   -0.122894710    0.016371910        25-May-18 18:11:28                                               
!Xoff    Yoff   Zoff    Radius  Ncat Nano  Cgap Agap    IPSD IORD DetGam DetNu                                          
!3.4850  0.0  -5.2481    764.247   640  256   2.50 1.5625   3   1    0.0    0.0                                         
                                                                                                                        
                                                                                                                        
SETTING       1 0 0   0 1 0   0 0 1                                                                                     
NUM_ANG  6                                                                                                              
ANG_LIMITS         Min      Max    Offset  Velocity(deg/s)                                                              
        Gamma      2.0    130.0     0.0      0.0                                                                        
        Omega    -31.0     49.0     0.0      1.0                                                                        
        Nu       -15.0     15.0     0.0      0.0                                                                        
        2Theta     0.0    180.0     0.0      0.0                                                                        
        Chi       80.0    180.0     0.0      1.0                                                                        
        Phi     -179.0    180.0     0.0      1.0                                                                        
                                                                                                                        
DET_OFF      0       0      0                                                                                           
