      SUBROUTINE COIL(A,RVIT,BR,BZ,HBR,HBZ)
      DATA PI5 /0.6283185/
C***
C   B     TOK
C        -  (XVIT)
C       RVIT- â®ª,¯®«®¦¨â¥«ì­ë© â¥ç¥â ¯à®â¨¢ ç á®¢®© áâà¥«ª¨
C       HBR,HBZ - ¨áª®¬®¥ ¯®«¥.
C        §¬¥à­®áâì:
C       ã£«ë-à ¤¨ ­
C       à ááâ®ï­¨¥ - 
C       ¯®«¥ - «
C       â®ª  - MA
C  CM.  ­¤ ã - ¨¢è¨æ .
C
      AA=A*A
      BZZ=BZ*BZ
      IF (BR.EQ.0.) GO TO 1
      BRR=BR*BR
      D1=(A+BR)**2+BZZ
      D2=(A-BR)**2+BZZ
      D3=BRR+BZZ
      D4=SQRT(D1)
      DK2=4.*A*BR/D1
      CALL ELLIPS(DK2,DE,DK)
      D5=DE/D2
      DI1=RVIT*0.2
      HBR=DI1*BZ*((AA+D3)*D5-DK)/(BR*D4)
      HBZ=DI1*(DK+(AA-D3)*D5)/D4


      RETURN
  1   CONTINUE
      D6=AA+BZZ
      HBZ=RVIT*PI5*AA/(SQRT(D6)*D6)
      HBR=0.


      RETURN
      END
