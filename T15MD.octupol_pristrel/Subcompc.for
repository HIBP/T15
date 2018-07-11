        SUBROUTINE INTST(NN,HH,XX,Y)
C      COMMON/HTAT/HTAR(30,50),HTAZ(30,50)
C      common/setka/xpse(30),ypse(50)
       COMMON/RT/RT12
        DIMENSION Y(20),G(20),O(20),Z(20)
C        print*,'I am intst'
        N=NN
        H=HH
        H1=0.5*H
        H2=H/6.
        X=XX
        XHH=X+H1
        XH=X+H
        CALL FIG(X,Y,G)
        DO 2 J=1,N
 2      Z(J)=Y(J)+H1*G(J)
        CALL FIG(XHH,Z,O)
        DO 3 J=1,N
        G(J)=G(J)+2.0*O(J)
   3    Z(J)=Y(J)+H1*O(J)
        CALL FIG(XHH,Z,O)
        DO 5 J=1,N
        G(J)=G(J)+2.0*O(J)
   5    Z(J)=Y(J)+H*O(J)
        CALL FIG(XH,Z,O)
        DO 6 J=1,N
   6    Y(J)=Y(J)+H2*(G(J)+O(J))
        XX=XH
C        print*,'I am intst end'
        RETURN
        END
        SUBROUTINE  INSAU(N,X,Y1,P,EPS1,T,R)
C      COMMON/HTAT/HTAR(30,50),HTAZ(30,50)
C      common/setka/xpse(30),ypse(50)
       COMMON/RT/RT12
        DIMENSION Y1(N),F(20),Y(20),A(20)
        H=P
C        print*,'I am insau'
        DO 1 I=1,N
 1      Y(I)=Y1(I)
 2      KR=1
        CALL INTST(N,H,X,Y)
  3     X=X-KR*H
        DO 4 I=1,N
 4      A(I)=Y(I)
        DO 5 I=1,N
 5      Y(I)=Y1(I)
        H=0.5*H
        KR=2*KR
C        print*,'I am insau before 6'
        DO 6 I=1,KR
C        print*,'kr=',kr
        CALL INTST(N,H,X,Y)
  6     CONTINUE
        DO 7 I=1,N
        IF(R.EQ.0.0) GO TO 17
        R1=ABS(VRLTV(Y(I),A(I)))
C       print*,R1,EPS1,i,Y(I),A(I)
        GO TO 18
  17    R1=ABS(Y(I)-A(I))
C       print*,'R1=',R1,'EPS1=',EPS1,'i=',i
  18    IF(R1.GT.EPS1) GO TO 3
  7     CONTINUE
        H=2.0*H
        IF(T.LT.1E-10) GO TO 15
        IF(ABS(X-T).LE.1E-10) GO TO 15
        IF(ABS(H).GE.ABS(T-X)) GO TO 11
        DO 8 I=1,N
        IF(ABS(Y(I)-A(I)).GT.EPS1/10.0) GO TO 12
  8     CONTINUE
        IF(2.0*H+X.LT.T) H=H*2.0
  12    DO 14 I=1,N
  14    Y1(I)=Y(I)
        GO TO 2
  11    H=T-X
        GO TO 12
  15    DO 16 I=1,N
  16    Y1(I)=Y(I)
C        print*,'I am insau end'
        RETURN
  21    CALL INTST(N,H,X,Y1)
        R=T-X
        IF(R.LE.H) GO TO 22
        GO TO 21
  22    H=R
        CALL INTST(N,H,X,Y1)
C        print*,'I am insau end'
        RETURN
        END
        FUNCTION VRLTV(A,B)
        A1=A-B
        IF(ABS(A).LT.ABS(B)) GO TO 2
  1     AM=ABS(A)
        GO TO 3
  2     AM=ABS(B)
  3     IF (AM.EQ.0.0) GO TO 4
        VRLTV=A1/AM
        GO TO 5
  4     VRLTV=0.
  5     CONTINUE
        RETURN
        END







      SUBROUTINE RECORD(X,Y,z,i,K)
      common/G/XOLD,YOLD,zold,XTEK,YTEK,ztek
      common/xx/xmin,xmax,ymin,ymax,zmin,zmax,x1x,y1y,x2x,z2z,z3z,y3y
      xoli=XOLD
      yoli=YOLD
      zoli=zOLD
      xtel=XTEK
      ytel=YTEK
      ztel=zTEK

      call DECORD(xoli,yoli,zoli,4,1)
      call DECORD(xtel,ytel,ztel,4,1)
      call DECORD(x,y,z,I,K)
      return
      END

      SUBROUTINE decord(X,Y,z,i,k)
      common/G/XOLD,YOLD,zold,XTEK,YTEK,ztek
      common/xx/xmin,xmax,ymin,ymax,zmin,zmax,x1x,y1y,x2x,z2z,z3z,y3y
C***************SHIFTS IN COMMON**************
      XOLD=XTEK
      YOLD=YTEK
      zOLD=zTEK
      XTEK=X
      YTEK=Y
      zTEK=z
C************************************************
c     CALL WHERE(X1X,Y1Y,CC)
c     CALL SETPEN(3)
c     CALL REGION(30.,0.,40.,40.,0,0,1)
c     CALL LIMITS(xmin,xmax,zmin,zmax)
c     CALL move(x2X,z2z,0)
c     call tmf(X,z,XI,zI)
c     CALL SETPEN(i)
c     CALL move(xI,zI,k)


c     CALL WHERE(X2X,Z2Z,CC)
c     CALL SETPEN(3)
C      CALL REGION(15.,0.,15.,30.,0,0,1)
C      CALL LIMITS(ZMIN,ZMAX,YMIN,YMAX)
C      CALL move(Z3Z,Y3Y,0)
C      call tmf(Z,Y,ZZI,YYI)
C      CALL SETPEN(i)
C      CALL move(ZZI,YYI,k)


c     CALL WHERE(Z3Z,Y3Y,CC)
c     CALL SETPEN(3)
c     CALL REGION(0.,0.,30.,40.,0,0,1)
c     CALL LIMITS(xmin,xmax,ymin,ymax)
c     CALL move(x1X,y1y,0)
c     call tmf(X,Y,XI,YI)
c     CALL SETPEN(i)
c     CALL move(xI,yI,k)



      return
      END






