      SUBROUTINE COIL(A,RVIT,BR,BZ,HBR,HBZ)
      DATA PI5 /0.6283185/
C***
C   BЫЧИСЛЕНИЕ ПОЛЯ ТОНКОГО КОЛЬЦА С TOKОМ
C       А - РАДИУС (XVIT)
C       RVIT- ток,положительный течет против часовой стрелки
C       HBR,HBZ - искомое поле.
C       Размерность:
C       углы-радиан
C       расстояние - М
C       поле - Тл
C       ток  - MA
C  CM. Ландау - Лившиц .
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