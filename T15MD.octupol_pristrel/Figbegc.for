                SUBROUTINE FIGbeg(DU)
      COMMON H,A1,R,RDIAF,IPO,IVIT
      COMMON /NNN/NCO1,NCO2,NCO3,NCHO,NCOR,NCOZ,NccR,NccZ
      COMMON /WIGU/RE(200),ZE(200),WIE(200),
     *RP(10000),ZP(10000),WIP(10000),HBRS,HBZS
c^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        DIMENSION DU(6),XVIT(4),YVIT(4),NV(4)
        REAL    IPO,IVIT,IV
        DATA PI/3.141592654/
        SDVIG=0.

C       поле тока плазмы
       X=DU(1)
       Y=DU(2)
       Z=DU(3)

       VX=DU(4)
       VY=DU(5)
       VZ=DU(6)
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
      RX=R+X+SDVIG

      BFI=ATAN(-Z/RX)
      BR=SQRT(Z**2+RX**2)
      BZ=Y

C                           [CM]
c      Поправка на тороидальность
      RBR=R/BR

      COF=COS(BFI)
      SIF=SIN(BFI)
C                       PLASMA CURRENT FIELD
      X1=BR-R
      Y1=Y
      RTK=SQRT(X1**2+Y1**2)
c     w1=ATAN(Y1/X1)

c     Sw1=Y1/RTK
c     Cw1=X1/RTK
      RA=RTK/RDIAF



C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
C                   ПОЛЕ ПЛАЗМЫ


      HBRS=0.
      HBZS=0.

       DO 16 I=1,nco3
       ZVIT=BZ-ZP(I)
       RCOIL=rP(I)
       IV=WIP(i)*1.E6

       CALL COIL(RCOIL,IV,BR,ZVIT,HBR,HBZ)
C       PRINT*,HBR,HBZ,I
       HBRS=HBRS+HBR
       HBZS=HBZS+HBZ
  16   CONTINUE


c       print*,'figbeg hbrs=',hbrs,'hbzs=',hbzs,'x=',x,'y=',y
C       Компоненты результирующего магнитного поля [эрстед]
       RETURN
       END

