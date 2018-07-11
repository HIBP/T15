

      subroutine ORCPASS03()
c яюфсюЁ Ubeta1
	use opengl
      use msfwin

   
      COMMON H,A1,RTOK,RPL,IPO,IVIT,IOH,
     *XIT1(1,700),XIT2(1,2000),YIT1(1,900),YIT2(1,2000),
     *XIT3(1,700),YIT3(1,700),ZIT3(1,700),
     *IION2(10),IWYX(10),VO(10),X11(700),Z11(700),Y11(700),
     *X12(700),Y12(700),Z12(700),ALAM(10),IION1(10),X21(700)
c     *hzapx1(100),hzapy1(100),hzapz1(100),
c     *hzapx2(100),hzapy2(100),hzapz2(100)
      COMMON/H/HVX,HVY,HVZ,HOHX,HOHY,HOHZ,HPLX,HPLY,HPLZ,
     *HMX,HMY,HMZ,HX,HY,HZ
      COMMON /NNN/NCO1,NCO2,NCO3,NCHO,NCOR,NCOZ,NccR,NccZ
      COMMON /WIGU/RE(200),ZE(200),WIE(200),
C^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     *RP(10000),ZP(10000),WIP(10000),HBRS,HBZS
      COMMON/HTAT/HTAR(500,500),HTAZ(500,500)
      common/setka/xpse(500),ypse(500)
	common/plasma/xpl(51),ypl(51),BplR(51,51),BplZ(51,51)
      common/RKATUSHKA/RKAT1,RKAT2,DRKAT
C^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      common/G/XOLD,YOLD,zold,XTEK,YTEK,ztek,G(6)
      common/xx/xmin,xmax,ymin,ymax,zmin,zmax,x1x,y1y,x2x,z2z,Z3Z,Y3Y
      COMMON/RT/rt12
	COMMON/Hmain/xfm(78),yfm(82),zfm(39),
     *  HxFM(78,82,39),HyFM(78,82,39),HzFM(78,82,39),
	*  indxmax,indymax,indzmax
      common/shotparameters/H0,CurTot,E,ugolA,xinj,yinj,xdet,ydet,dt_ext
	*                      ,zi_ext,beta_ext,alfa_ext
	common /kz/ kz
      common/griddata/xg(10000),yg(10000),zg(10000),
     *                inter(10000),Ebeam(10000),ugolAlfa(10000),Ng
      common /elon/ elon
	common /p5v/x5bf(820),z5bf(500)
	*             ,FIUcf1(820,500),FIUcf2(820,500),qm
      common /ionoprovod/ xalfa,yalfa,zalfa,aalfa,balfa,
     *xbeta,ybeta,zbeta,abeta,bbeta,
     *xbeta1,ybeta1,zbeta1,abeta1,bbeta1,
     *xfar,yfar,zfar,afar,bfar,
	*xUcf,yUcf,zUcf,aUcf,bUcf,
	*x2blpc,y2blpc,z2blpc,a2blpc,b2blpc,
	*xanc,yanc,zanc,banc,
	*xi0,yi0,zi_entered,
     *beamshift,beta0,angle_of_injection(1) 

      common/plate_voltages/Uplate1,Uplate2,Ufar,Ucorr1,Ucorr2
     *                    ,Ubf1,Ubf2,Ener,U2blp,currInt,ExtCurr,HcCurr
	*                    ,Ubeta1_1,Ubeta1_2


      DIMENSION XDELTA(10),YDELTA(10),ZDELTA(10),X22(700),DU1(6),
     *DU2(6),LT(6),P(7),ZIT1(1,1000),EE(21),ZDEF(21),X13(100),Y13(100),
     *Z13(100),IV(10),JV(10),JAXV(10),JA1V(10),ZIT2(1,2000),XITI(500),
     *YITI(500),ZITI(500),XITJ(500),YITJ(500),ZITJ(500),
     *rtpp(4),dalf(4),
     *dbet(4),xc(110),yc(110),yc1(110),XQ(5),YQ(5),DU(6),
     *RBON(50),ZBON(50),POB(2),POK(2),ENERGY(32),
     *KZarr(32),DTarr(32),xzon(1000),yzon(1000),zzon(1000)
      real radioniz(5),xyshift(5),uvl(5)
      REAL IPO,IVIT,IOH
      character*12 SNAME1
      character*12 sname2
      character*6 CONTRO

C***************************************************************
C SWEEPING PLATES COORDINATES
      DATA RTPP/1000.,1000.,1000.,1000./
C***************************************************************
C INITIALISATION OF BEAM DEVIATION ON PLATES
      DATA DALF/0.,0.,0.,0./
      DATA DBET/0.,0.,0.,0./
C*****************************************************************
      DATA POB/110.,276.67/
      DATA POK/-1.833,-3.333/

      DATA DEGRAD/57.29578/
      DATA PI/3.141592564/



	zapas=0.
      y_border=15.+zapas

c      print *, 'xplate2=',xplate2
c	read(*,*)


 1550 CONTINUE

       beta0= beta_ext
c       zi_entered= zi_ext !1.25-2.5*beta0
       beta0step=0.
       dt1=dt_ext
      dt_small_primary=dt1
	dt_small_primary=dt_small_primary*1.E-6
      dt_small_secondary=dt1	   
	dt_small_secondary=dt_small_secondary*1.E-6
c       kz=999
      greshki=0.
      greshkix=0.
        isok=1
        knum=0

C idflag=0 если не нужна оптимизация по beta для intersect=1
       idflag=0
C*****FILE FIELD.DAT (FIELD NETWORK) EXISTANSE PARAMETER*****
      IFOP=0
      ifMntFldOpnd=0

C ╓хэЄЁ яюы Ёэющ ёшёЄхь√ ъююЁфшэрЄ
      Xnc=-1160.
	Ync= 30.



 7877 CONTINUE

      isan=0
      isfirst=1
      issec=0
      first=1.
      isignal=0
      IP=1
  122 IP=IP+0

       DT1=DT1*1E-6
      intersect=0
      

C*************DISTANCE FROM INITIAL POINT TO BELLOW CENTER*******
         SMESH=0.
         RRL=100.
C************* select Cs (шыш Tl) *******************************
      NN=3
C************* select tf value [T]*******************************
C        H=0.6
C**************select injection angle ***************************
C         ALFA=75.
C         beta=-2.5
C**************select repeat param.******************************
      it=1
c****************************************************************

      beta_step = beta0step

C     if (E.gt.260.) beta_step=0.5

C     beta_step=1.
      ized=0

 1825 CONTINUE

      intersect=0
      fi_new=100.
      if (beta_step.lt.0.00001) beta_step=0.259
      isfirst=1


C
C         TCV     P A R A M E T E R S
C
      elon=1.
      sdvig=0.
      alfa0=angle_of_injection(1)

c	do k=1,2
c	do j=1,15
c      do i=1,50
c	 rro=(0.001+(i-1)*40./49.)*(-1.)**k
c	 ang=-89.999+(j-1)*180./14.
c	 ang=ang*3.14159265/180.
c       G(1)=rro*cos(ang)
c       G(2)=rro*sin(ang)
c	 G(3)=0.
c       G(4)=0.
c       G(5)=0.
c       G(6)=0.
c  	 print *, j,i
c       CALL FIG(0.,G,G)
c	enddo 
c	 open (85,FILE='pole.dat',STATUS='UNKNOWN',ACCESS='APPEND')
c       write(85,*) '-- -- -- -- -- -- -- --'
c       close(85)
c      enddo
c	 open (85,FILE='pole.dat',STATUS='UNKNOWN',ACCESS='APPEND')
c       write(85,*) '-- -- -- -- -- -- -- --'
c       close(85)
c	enddo
c      read(*,*)













C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
C$$$$$$$$$$                                       $$$$$$$$$$$$$$$$$
C$$$$$$$$$$      TRAJECTORIES CALCULATION         $$$$$$$$$$$$$$$$$
C$$$$$$$$$$                                       $$$$$$$$$$$$$$$$$
C$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
  92  CONTINUE

C     READ(*,*)

C     OPEN(UNIT=12,FILE=SNAME,STATUS='UNKNOWN',FORM='FORMATTED')
c     OPEN(UNIT=3,file='BEAM.DATA',STATUS='UNKNOWN',FORM='FORMATTED')
 4200 CONTINUE
      RK=5*RTOK
      BETA0=BETA0/DEGRAD
      ALFA0=ALFA0/DEGRAD
      dalfa=dalfa/degrad
      BINI=BETA
      AINI=ALFA

C***********************ION SOURSE POSITION************************
       XPH=40.
       YPH=0.
C      XPA=12.5
C      YPA=34.
       XPA=25.5
       YPA=20.
       XBEL=XPa
       YBEL=YPa
C       XBEL=14.
C       YBEL=54.
       XBEL1=Xbel-smesh*cos(alfa)
       YBEL1=Ybel+smesh*sin(alfa)
        PO1LX=60.
        PO1LY=0.
        PO1UX=0.
        PO1UY=110.
        PO2LX=80.
		PO2LY=10.
        PO2UX=50.
		PO2UY=110.
C       ZI=-RRL*SIN(ALFA)*SIN(BETA)
C       XI=XBEL1+RRL*SIN(ALFA)*COS(BETA)
C       YI=YBEL1+RRL*COS(ALFA)
C       ZI=0.

C       YI=110.
C      YI=80.


C****     work now ***********************
         smeshenie0=1.0 !0.5
         beamshift=0. !-2.5
        alfarazl0=0. !0.05*(3.14159265/180.)
        xi00=xinj-beamshift*cos(farang) !151.69 + greshkix  
        yi00=yinj+beamshift*sin(farang) !297.55 + greshki  
        zi00=zi_entered

c      do kk=1,1000
c         G(1)=-40.+(kk-1)*xi/999.
c	   G(2)=(yi/xi)*G(1)
c	call FIG(0,G,G)
c	enddo
c      stop	   

c        xi=103.96       !72.88
c        yi=218.90       !169.84-greshki
c        zi=zi_entered
      iPlates=0
      do ivertex=3,3
      do jvertex=1,1

      SNAME1='TRAJ0000.DAT'
      WRITE(SNAME1(5:8),'(1I4)') kz
      WRITE(*,10) SNAME1     

      SNAME2='zone0000.DAT'
      WRITE(SNAME2(5:8),'(1I4)') kz
      WRITE(*,10) SNAME2
  10  FORMAT(' ',A12)

      if (ivertex.eq.1) then
	  smeshenie=smeshenie0
	  alfarazl=alfarazl0
	  	elseif (ivertex.eq.2) then
	  smeshenie=smeshenie0/2.
	  alfarazl=alfarazl0/2.
     	elseif (ivertex.eq.3) then
	  smeshenie=0.
	  alfarazl=0.
    	elseif (ivertex.eq.4) then
	  smeshenie=-smeshenie0/2.
	  alfarazl=-alfarazl0/2.
    	elseif (ivertex.eq.5) then
	  smeshenie=-smeshenie0
	  alfarazl=-alfarazl0
      endif
      xi=xi00-smeshenie*cos(farang)
	yi=yi00+smeshenie*sin(farang)
	alfa=alfa0+alfarazl

      zsmeshenie=0. !0.5
	betarazl=0. !0.05*(3.14159265/180.)

      if (jvertex.eq.1) then
	zi=zi00+zsmeshenie
	beta=beta0+betarazl
	else
	zi=zi00-zsmeshenie
	beta=beta0-betarazl
	endif


 
        								 
c*****************************************

 55     CONTINUE

C       zI=0.
C*******************DETECTORS COORDINATES****[SM]*********************
C            XD=5.
C            YD=37.5
c               XD=60.
c              XD=104.8
C              YD=23.
c              YD=102.
C              ZD=-15.
               apertura=0.
               sdvigdet=0.
               detangle=aalfa
               xd0=xalfa  + sdvigdet*sin(detangle) !+ Xplate2
               yd0=yalfa   - sdvigdet*cos(detangle) !+ greshki
c              yd=yd-(apertura/2.)*(-1)**idet			   
               ZD=0.

               do kvertex=1,1
	if (kvertex.eq.1) then
	xd=xd0-(apertura/2.)*cos(detangle)
      yd=yd0+(apertura/2.)*sin(detangle)
	else
	xd=xd0+(apertura/2.)*cos(detangle)
      yd=yd0-(apertura/2.)*sin(detangle)
	endif
               Z_DELTA=0.5
               z_upper=zd+z_delta/2.
               z_lower=zd-z_delta/2.
C%%%%%%%ЦЕНТР НОВОЙ СИСТЕМЫ КООРД. И КООРДИНАТЫ В ЭТОЙ СИСТЕМЕ  %%%%%%
C%%%%%%%%%%%%%(ИСП. ДЛЯ КОНТРОЛЯ ПОПАД. В АПЕРТУРУ АНАЛИЗАТОРА) %%%%%%
1500        CONTINUE 
            if (intersect.eq.0) then
            XN=Xnc
            YN=Ync

            RDNO=SQRT((XD-XN)**2+(YD-YN)**2)
            FINO=ATAN((YD-YN)/(XD-XN))
c           apertfi=atan(apertura/rdno)/2.
c           fino=fino-apertfi*(-1)**idet
            DFI=ATAN(.2/RDNO)
            FDL=FINO+DFI
            FDP=FINO-DFI
            
            else
            xn=Xnc
            yn=Ync

            rdno=sqrt((xd-xn)**2+(yd-yn)**2)
            fino=atan((yn-yd)/(xd-xn))
c           apertfi=atan(apertura/rdno)/2.
c           fino=fino-apertfi*(-1)**idet
            dfi=atan(.2/rdno)
            fdp=fino-dfi
            fdl=fino+dfi
            endif
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            if (isfirst.eq.0) goto 1


C****************PARAMETERS******************************************
c       DT1=0.2
C      dt1=0.1
C                              [MCSEC]
       EPS1=0.1
C      eps1=0.001

           xfig=50
           yfig=30
            YPER=-25.
            IPRI=0
            IPM=IP
            T=0.
            IION2(IP)=0
            IION1(IP)=0
            IND=0
            isplates=0
            ISION=0
            IBACK=0.
            INDRP=0
            ITPP=2
             NR=0
             NR1=0
             imko=0
             imka=0
             IMKI=0
             inn1flag=0
             inn2flag=0
C*********************************************************************
C                                    [sec]
   8       CONTINUE



C                         CHOICE OF BEAM IONS
          IF(NN-2) 51,52,53
   51       A=419.
          VO(IP)=6.9E6*SQRT(1.7*E)
C                                      Na
          go to 54
   52       A=247.6
            VO(IP)=6.9E6*SQRT(E)
C                                      K
          go to 54
   53       A=72.6
            VO(IP)=3.8E6*SQRT(E)
C                                      Cs
c           A=47.065177783185                      ! Tl 205
c          A=47.5288741160247216                  ! Tl 203
c           Vo(IP)=3.068080608349118E6*sqrt(E)	    ! Tl 205

c           qmTl=1.41097853338E12
c          Vo(IP)=3.08315726232824214E6*sqrt(E)	! Tl 203

           A=1608.0602409255						! Li 6
           Vo(IP)=17.9336244568789E6*sqrt(E)	    ! Li 6
           qmLi=4.820843E13

c		 A=72.6
c            VO(IP)=3.8E6*SQRT(E)
c		qmLi=4.820843E13*(6./133.)
C                                      Cs


          A=419.
          VO(IP)=6.9E6*SQRT(1.7*E) !Na
	    qmNa=1.257611309E13

            A=247.6
            VO(IP)=6.9E6*SQRT(E)	 ! K
	      qmNa=7.41668203489477E12


            A=72.6
            VO(IP)=3.8E6*SQRT(E)
		qmNa=2.17481666738544E12
C                                      Cs


           A=47.065177783185                      ! Tl 205
           Vo(IP)=3.068080608349118E6*sqrt(E)	    ! Tl 205
           qmTl=1.41097853338E12




   54     continue
C
C                    INITIAL POINT OF TRAJECTORY










      CALL PATRUBOK()
	 CALL AXES (xmin,ymin,xmax,ymax,10.,10.)
      call fglBegin(GL_quads)
      call fglColor3f (1.0, 0.0, 0.0)

      call fglVertex2f(xd+0.5, yd+0.5)
	call fglVertex2f(xd+0.5, yd-0.5)
	call fglVertex2f(xd-0.5, yd-0.5)
	call fglVertex2f(xd-0.5, yd+0.5)


	call fglEnd()
	
      CALL DECORD(XI,YI,ZI,1,0)





	Ustep= -0.5
	iplus=0
	iminus=0
 193  CONTINUE




         
         I=1
          iCounter=0
c iCT.eq.0 только при первом проходе
          iCT=0

          DU1(1)=XI
          DU1(2)=YI
          DU1(3)=ZI
	    xoldgr=xi
	    yoldgr=yi
	    zoldgr=zi
	    xnewgr=du1(1)
	    ynewgr=du1(2)
	    znewgr=du1(3)
	    call fglLineWidth (1)

          DU1(4)=-VO(IP)*SIN(ALFA)*COS(BETA)
          DU1(5)=-VO(IP)*COS(ALFA)
          DU1(6)=VO(IP)*SIN(BETA)*SIN(ALFA)
c         if (itochno.eq.1) then
c          write(34,416) DU1(6),VO(IP),ZI
c          CLOSE(34)
c         endif
          XIT1(IP,I)=DU1(1)
          YIT1(IP,I)=DU1(2)
          ZIT1(IP,I)=DU1(3)
          DT2=DT1
          EPSO=EPS1
          print*, 'ALFA=',ALFA*DEGRAD,' E=',E,'DT1=',DT1
          GOTO 1
C&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&
  812   CONTINUE
         fi_old=fi_new
c         if (icounter.eq.2) goto 9999
c         icounter=icounter+1
         XXO=DU1(1)
         YYO=DU1(2)
         ZZO=DU1(3)
        xnewgr=xit1(ip,i)

	    ynewgr=yit1(ip,i)
	    znewgr=zit1(ip,i)
	    xoldgr=xit1(ip,i-1)

	    yoldgr=yit1(ip,i-1)
	    zoldgr=zit1(ip,i-1)

         CALL RECORD (XXO,YYO,ZZO,1,0)


C                    PRIMARY ORBIT


 1        CONTINUE



        call fglBegin(GL_LINE_STRIP)
             call fglColor3f (1.0, 1.0, 0.0)
             call fglVertex2f(xoldgr,yoldgr)
             call fglVertex2f(xnewgr,ynewgr)
 	  call fglEnd()

        call fglBegin(GL_LINE_STRIP)
             call fglColor3f (1.0, 1.0, 0.0)
             call fglVertex2f(xoldgr+200.,-zoldgr)
             call fglVertex2f(xnewgr+200.,-znewgr)
 	  call fglEnd()


	  call fglFlush()
	  xoldgr=xnewgr
	  yoldgr=ynewgr
	  zoldgr=znewgr

          A1=A
	    qm=qmTl
          VXIT1=DU1(4)
          VYIT1=DU1(5)
          VZIT1=DU1(6)
       RT11=SQRT((DU1(1)-XBEL)**2+(DU1(2)-YBEL)**2+DU1(3)**2)
       rt12=rt11*sign(1.,(du1(2)-YBEL))
       RT1=SQRT(DU1(1)**2+(DU1(2)/elon)**2)
         if (imka.eq.0.and.ision.eq.0) then
	   	  if (RT1.lt.RPL) then
	         dt2=dt_small_primary
	      else
	         dt2=dt1
	      endif
	   endif

       CALL INSAU(6,T,DU1,DT2,EPSO,0.,5.)

c       print*, 'z:', dU1(3)
c	read(*,*)



       RT1=SQRT(DU1(1)**2+(DU1(2)/elon)**2)
          if (isplates.eq.1) goto 4125
 4125   CONTINUE
        if (rt1.gt.350.) then
          isOK=0
          print*, '2'
          goto 9999
        endif
       RT11=SQRT((DU1(1)-XBEL)**2+(DU1(2)-YBEL)**2+DU1(3)**2)
       rt12=rt11*sign(1.,du1(2)-YBEL)
C*******************************************************************

  81      I=I+1
          XIT1(IP,I)=DU1(1)
          YIT1(IP,I)=DU1(2)
          ZIT1(IP,I)=DU1(3)
c          if (xit1(ip,i).gt.(xd+25.).and.ision.eq.0.) then
c             print*, 'primary away'
c             isOK=0
c 		   goto 9999
c	    endif

C x<>y
          if (i.ge.2) then
          if (yit1(ip,i).le.yd.and.yit1(ip,i-1).ge.yd) then
          zresult=(yd-yit1(ip,i-1))*(zit1(ip,i)-zit1(ip,i-1))/
     *            (yit1(ip,i)-yit1(ip,i-1)) + zit1(ip,i-1)
          print *, 'z:',zresult, zd
	      print *, 'y:',yit1(ip,i),yit1(ip,i-1),yd
          goto 8739
	    endif
	    endif




          if (xit1(ip,i).lt.-27.5) then
             print*, 'too far'
	       isOK=0
	       goto 9999
	    endif
c          if (xit1(ip,i).gt.31..and.
c     *        xit1(ip,i).gt.xit1(ip,i-1).and.
c	*        abs(yit1(ip,i)).gt.4.
c	*     ) then
c             print*, 'hit the camera'
c	       isOK=0
c	       goto 9999
c	    endif


          xnewgr=du1(1)
	      ynewgr=du1(2)
	       znewgr=du1(3)


c          hzapx1(i)=hx/1.E4
c          hzapy1(i)=hy/1.E4
c          hzapz1(i)=hz/1.E4
          XXO=DU1(1)
		  YYO=DU1(2)
          ZZO=DU1(3)
          CALL DECORD(XXO,YYO,ZZO,3,1)
          HTOT=SQRT(HOHX**2+HOHY**2+HOHZ**2)

          IF ((rt1.lt.rpl).and.(inn1flag.eq.0)) then
             NN1=I
             inn1flag=1
          endif


c          if(rt1.ge.(rpl+zapas).and.ision.eq.0.) goto 1
c          if (yit1(ip,i).gt.y_border) goto 1

c	      print *, xit1(ip,i),xit1(ip,i-1),xinj0
c          read(*,*)
	    goto 1





    9 CONTINUE
          IBACK=1
c          IF(RT1.GT.RK) GO TO 7
          J=0
          T1=0.
          DU2(1)=DU1(1)
          DU2(2)=DU1(2)
          DU2(3)=DU1(3)
          DU2(4)=DU1(4)
          DU2(5)=DU1(5)
          DU2(6)=DU1(6)
          A1=2.*A
		  qm=2.*qmTl

          EPSO2=EPS1
          DT=DT1
          inn2flag=0
C               SECONDARY ORBIT
  2     CONTINUE
        call fglBegin(GL_LINE_STRIP)
             call fglColor3f (0.0, 1.0, 0.0)
             call fglVertex2f(xoldgr,yoldgr)
             call fglVertex2f(xnewgr,ynewgr)
	  call fglEnd()

        call fglBegin(GL_LINE_STRIP)
             call fglColor3f (0.0, 1.0, 0.0)
             call fglVertex2f(xoldgr+200.,-zoldgr)
             call fglVertex2f(xnewgr+200.,-znewgr)
 	  call fglEnd()


	  call fglFlush()
        xoldgr=xnewgr
	  yoldgr=ynewgr
	  zoldgr=znewgr
	    issec=1
          VXIT2=DU2(4)
          VYIT2=DU2(5)
          VZIT2=DU2(6)
C         DT=0.4E-6
C        DT=DT1
C         DT=0.2E-6
C         IF(R2.GT.(RDNO-3.).AND.R2.LT.(RDNO+3.))  DT=.5E-8
         Rt13=sqrt(DU2(1)**2+(DU2(2)/elon)**2)
       if (imki.eq.0) then
	   if (RT13.lt.RPL+zapas) then
	      DT=DT_small_secondary
	   else
	      DT=DT1
	   endif
	 endif
c       if (isan.eq.1) dt=dt1/18.

         CALL INSAU(6,T1,DU2,DT,EPSO2,0.,5.)
c       print*, 'z2:', dU2(3)
c	read(*,*)

          R2=SQRT((DU2(1)-XN)**2+(DU2(2)-YN)**2)
C          PRINT*,R2,RDNO
          r3=sqrt(DU2(1)**2+(DU2(2)/elon)**2)
          IF ((r3.gt.rpl).and.(inn2flag.eq.0)) then
C           print *, r3,'rpl=',rpl,'I=',I,'J=',J
C           read(*,*)
            NN2=J+I
            inn2flag=1
          endif
          J=J+1
	    if (j.gt.2000) then
	       print * , 'Overflow...'
             goto 9999
	    endif
	    XIT2(IP,J)=DU2(1)
          YIT2(IP,J)=DU2(2)
          ZIT2(IP,J)=DU2(3)
	xnewgr=du2(1)
	ynewgr=du2(2)
	znewgr=du2(3)
c          hzapx2(j)=hx/1.E4
c          hzapy2(j)=hy/1.E4
c          hzapz2(j)=hz/1.E4
          XXO=DU2(1)
          YYO=DU2(2)
          ZZO=DU2(3)
          CALL DECORD(XXO,YYO,ZZO,5,1)

C проверка на попадание в P5
C      if (((xit2(ip,j).gt.P5x_lower-RTOK).and.
C     *      (xit2(ip,j).lt.P5x_upper-RTOK)
C     *  .and.(yit2(ip,j).gt.P5y_lower).and.(yit2(ip,j).lt.P5y_upper))
C     *  .or.
C     *       ((xit2(ip,j).gt.P5x_lower-RTOK).and.
C     *        (xit2(ip,j).lt.P5x_upper-RTOK)
C     *  .and.(yit2(ip,j).lt.-P5y_lower).and.(yit2(ip,j).gt.-P5y_upper)))
C     *                  goto 812

c проверка не забрались ли мы слишком высоко
c        if ((yit2(ip,j).gt.(yd+50.)).and.(isan.eq.0.)) goto 812
c        if (xit2(ip,1).gt.31.and.abs(yit2(ip,1)).gt.4.) goto 812
        if (j.gt.1) then
        if (xit2(ip,j).gt.31..and.xit2(ip,j-1).lt.31.
     *   .and. abs(yit2(ip,j)).gt.4.) then
         if (ision.eq.0) then
             goto 812 !чряЁхЄшЄ№ яхЁхёхърЄ№ °шЁьє
	   else
             print*, 'bl3'
             isbadluck=1
	       xion2=xzon(icount)
	       yion2=yzon(icount)
	       zion2=zzon(icount)
	       goto 1999
	   endif
	endif

c      ╩ююЁфшэрЄ√ яырёЄшэ	юъЄєяюы 
       xpoA1=31. !89.891
	 ypoA1=4. !75.246
	 xpoB1=53. !97.343
	 ypoB1=4. !90.526

	 xpoA2=31. !96.182
	 ypoA2=-4. !72.178
	 xpoB2=53. !103.634
	 ypob2=-4. !87.457
c	if (isintersect(xpoA1,YpoA1,xpoB1,ypoB1,
c     *       xit2(ip,j-1),yit2(ip,j-1),xit2(ip,j),yit2(ip,j)).eq.1
c     *       .and.ision.eq.0) then
cc         if (ision.eq.0) then
c             print *, 'plate3'
c             goto 812 !чряЁхЄшЄ№ яхЁхёхърЄ№ яырёЄшэє
cc	   else
cc             print*, 'bl2a'
cc             isbadluck=1
cc	       goto 1999
cc	   endif
c	endif
c      if (isintersect(xpoA2,YpoA2,xpoB2,ypoB2,
c     *       xit2(ip,j-1),yit2(ip,j-1),xit2(ip,j),yit2(ip,j)).eq.1
c     *       .and.ision.eq.0) then
cc         if (ision.eq.0) then
c             print *, 'plate2'
c             goto 812 !чряЁхЄшЄ№ яхЁхёхърЄ№ яырёЄшэє
cc	   else
cc             print*, 'bl2b'
cc             isbadluck=1
cc	       goto 1999
cc	   endif
c	endif
	endif

c  Important to rem out
c        if (j.gt.20) then
c           if ((xit2(ip,j).lt.xit2(ip,j-1)).and.ision.eq.0) goto 812
c        endif

  217    CONTINUE
         IWYX(IP)=J
c         if(du2(1).le.10.) goto 2
         IF(R2.GE.(RDNO-0.001))  GOTO 129
          if(imkI.eq.0) GOTO 2
c           print*, 'imki=1'
           DT=DT/2
           EPSO2=0.5*EPSO2
            GOTO 2

 129       IF(r2.LE.(RDNO+0.001)) GOTO 3
            imkI=1
            DT=DT/2
            EPSO2=0.5*EPSO2
c           print*, 'perelyot',DT
            J=J-1
            DU2(1)=XIT2(IP,j)
            DU2(2)=YIT2(IP,j)
            DU2(3)=ZIT2(IP,j)
            DU2(4)=VXIT2
            DU2(5)=VYIT2
            DU2(6)=VZIT2

          
           GOTO 2

    3 CONTINUE
       EPSO2=EPS1
c         print *, 'after 3'
         DT=DT1
         IMKI=0
C        GO TO 515

c        if (xit2(ip,j).gt.xfm(indxmax).or.
c     *      xit2(ip,j).gt.xpse(nnetr)) then
c         if (xit2(ip,j).gt.xd+25..or.yit2(ip,j).lt.yd-100.
c	*      .or. yit1(ip,i).lt.yd-100.) then
c            isOK=0
c	        print*, '3'
c            goto 9999 !515
c	  endif



        if (intersect.eq.0) then
        FINO=ATAN((DU2(2)-YN)/(DU2(1)-XN))
        else
        fino=atan((yn-du2(2))/(du2(1)-xn))
        endif
C       IF((DU2(1)-XN).LE.O) FINO=1.57

        fi_new=fino
C Не допускаем попадания в P5 (нижн.)
C       if ((fi_new.gt.fi_old).and.(fi_new.gt.fdl).and.(intersect.eq.1)
C    *      .and.(imka.eq.0)) goto 9999

         if (ision.eq.1) goto 1997

        if (iCT.eq.0) then
            iCT=1
           if (fino.lt.fdl) then
c            if (yit2(ip,j).lt.(yd+0.01)) then
              intersect=1
	        print *, 'intersect=1'

              xn=Xnc
              yn=Ync

              rdno=sqrt((xd-xn)**2+(yd-yn)**2)
              fino=atan((yn-yd)/(xd-xn))
c             apertfi=atan(apertura/rdno)/2.
c             fino=fino-apertfi*(-1)**idet
              dfi=atan(.2/rdno)
              fdp=fino-dfi
              fdl=fino+dfi
              fino=atan((yn-du2(2))/(du2(1)-xn))
              fi_new=100.
             endif
           endif
c       print *, 'zd= ',zit2(ip,j)
c       print *, fino,fdp,fdl
  11   IF(FINO-FDL) 12,12,671

  671     CONTINUE
c          XXO=DU1(1)
c          YYO=DU1(2)
c          ZZO=DU1(3)

          
c          CALL RECORD(XXO,YYO,ZZO,1,0)
          if(imka.eq.0) GOTO 812
            DT2=DT2/2
c           epso=epso/2
            GOTO 812

  12   if ((fino.gt.fdp).and.((idflag.eq.0).or.(intersect.eq.0)))
     *    go to 14
  13     CONTINUE
            if ((idflag.eq.1).and.(intersect.eq.1)) then
C              print *, 'Возникла интересная ситуация.'
               imka=0
               if (fi_old.lt.fi_new) then
C                 print *, 'даже через чур...',fi_old,fi_new,intersect
                  intersect=0
                  XN=Xnc
                  YN=Ync

                  RDNO=SQRT((XD-XN)**2+(YD-YN)**2)
                  FINO=ATAN((YD-YN)/(XD-XN))
c                 apertfi=atan(apertura/rdno)/2.
c                 fino=fino-apertfi*(-1)**idet
                  DFI=ATAN(.2/RDNO)
                  FDL=FINO+DFI
                  FDP=FINO-DFI
c                 I=I-1
c                 DU1(1)=XIT1(IP,I)
c                 DU1(2)=YIT1(IP,I)
c                 DU1(3)=ZIT1(IP,I)
c                 DU1(4)=VXIT1
c                 DU1(5)=VYIT1
c                 DU1(6)=VZIT1
               endif
               GOTO 812
            endif
            imka=1
            DT2=DT2/2
c            epso=EPSO/2
            I=I-1
            
            DU1(1)=XIT1(IP,I)
            DU1(2)=YIT1(IP,I)
            DU1(3)=ZIT1(IP,I)
            DU1(4)=VXIT1
            DU1(5)=VYIT1
            DU1(6)=VZIT1
c          XXO=DU1(1)
c          YYO=DU1(2)
c          ZZO=DU1(3)
c          CALL RECORD(XXO,YYO,ZZO,1,0)
          
            GOTO 812
                      
   14     CONTINUE

c          XXO=DU1(1)
c          YYO=DU1(2)

C**************************************************************
          NR=0
          DO 196 JJ=1,J
          NR=NR+1
          XITI(JJ)=XIT2(IP,JJ)
          YITI(JJ)=YIT2(IP,JJ)
          ZITI(JJ)=ZIT2(IP,JJ)
 196      CONTINUE

 734  CONTINUE

c         CALL SETPEN(2)
c         CALL LINEO(XITI,YITI,NR)
C***************************************************************

c          CALL RECORD(XXO,YYO,ZZO,1,0)

c следущая строчка нужна для обхода z-оптимизации          
  218    CONTINUE


      itransfer=i


c ╬сїюф ёў╕Єр чюэ√ шюэшчрЎшш
c       xion1=xit1(ip,I)
c       yion1=yit1(ip,I)
c       zion1=zit1(ip,I)
c       xion2=xit1(ip,I)
c       yion2=yit1(ip,I)
c       zion2=zit1(ip,I)
c       GOTO 369

c      if (intersect.eq.1.and.
c     *    sqrt(xit1(ip,i)**2+yit1(ip,i)**2).gt.40.) then
c         goto 589
c	endif

 6398    CONTINUE

c      if (intersect.eq.0) then
 4765 FORMAT (5F10.5)
c	endif

	i=i-1
      du1(1)=xit1(ip,i)
      du1(2)=yit1(ip,i)
      du1(3)=zit1(ip,i)
      du1(4)=vxit1
      du1(5)=vyit1
      du1(6)=vzit1
      if (E.le.90.) then
       dt2=0.001*1E-6
      else
       if (E.le.140.) then
        dt2=0.00075*1E-6
       else
        if (E.le.200.) then
         dt2=0.00045*1E-6
        else
         if (E.le.260.) then
          dt2=0.00035*1E-6
         else
          if (E.le.420.) then
           dt2=0.00025*1E-6
          else
           dt2=0.0002*1E-6
          endif
         endif
        endif
       endif
      endif
c      print *, 'dt2=',dt2
 7592 CONTINUE
      epstmp=eps1
      eps1=eps1/30.
      icount=0
      fiold2=fino !2000.
      fiold1=fino !1000.
      ision=1
      imka=0
      imki=0
      imko=0
      goto 812


 1997    CONTINUE
c     if (fino.lt.fdp) print *, fino,fdp
c     if (fino.gt.fiold1) print *, 'DANGER'
c      if ((ind.eq.1).and.(fino.lt.fdl)) goto 1998
      if (ind.eq.1) goto 1998
      if (fino.lt.fdl) then 
c      print *, 'In analyser'
      ind=1
      xion1=xit1(ip,i)
      yion1=yit1(ip,i)
      zion1=zit1(ip,i)
      icount=1
      xion2=xion1
	yion2=yion1
	zion2=zion1
      xzon(icount)=xit1(ip,I)
      yzon(icount)=yit1(ip,I)
      zzon(icount)=zit1(ip,I) 
      else
      continue
      endif
c      print *, 'Not yet'
      goto 812

 1998 CONTINUE
      icount=icount+1
      xzon(icount)=xit1(ip,I)
      yzon(icount)=yit1(ip,I)
      zzon(icount)=zit1(ip,I) 
      print*, 'Computing ionZone: ',icount
c      print*, fino,fdl,fdp
	print*, fino,fiold1,fiold2
c	print*, fdl,fdp
c	read(*,*)
c      if ((fino.lt.fdp).or.
c     *((fino.gt.fiold1).and.(fiold1.gt.fiold2))) then
      if (fino.lt.fdp.and.fiold1.lt.fdp.and.fiold2.lt.fdp) then
c	     if (icount.eq.2) then
c              dt2=dt2/2
c		    goto 7592
c	     endif
           icount=icount-2
           xion2=xzon(icount)
           yion2=yzon(icount)
           zion2=zzon(icount)
           goto 1999
      endif
c      if ((fino.gt.fiold1).and.(fiold1.gt.fiold2)
c	*  .and.intersect.eq.1) then
c           icount=icount-2
c           xion2=xzon(icount)
c           yion2=yzon(icount)
c           zion2=zzon(icount)
c           goto 1999
c	endif
      if (fino.gt.fdl.and.fiold1.gt.fdl.and.fiold2.gt.fdl
	*               .and.fino.gt.fiold1.and.intersect.eq.1) then
c	     if (icount.eq.2) then
c              dt2=dt2/2
c		    goto 7592
c	     endif
           icount=icount-2
           xion2=xzon(icount)
           yion2=yzon(icount)
           zion2=zzon(icount)
           print*, 'bl1'
           isbadluck=1
	     goto 1999
	endif
      fiold2=fiold1
      fiold1=fino
      goto 812

 1999 CONTINUE
 369  CONTINUE
      eps1=epstmp


c     print *, intersect,fdl,fdp,fino
C     read(*,*)
c      write (20,412) E,dALFA*degrad,BETA*degrad,xion1,
c     *yion1,zion1,icount,intersect
c      write (20,412) E,dALFA*degrad,BETA*degrad,xion2,
c     *yion2,zion2,icount,intersect
c      write (20,*) '--   --   --   --   --   --   --   --'
c      CLOSE (20)
C epselon - минимальное расстояние между зонами ионизации
C     epselon=0.5
C     y_border=yion2-epselon
      idflag=1
      beta_step=beta0step
      radion = sqrt(((xion2+xion1)/2.)**2+((yion2+yion1)/2.)**2
     +               +((zion2+zion1)/2.)**2)
c      write (77,509) icount,radion
c      write(77,507) (xzon(kk),yzon(kk),zzon(kk),kk=1,icount)
 507  format(F5.2,3X,F5.2,3X,F5.2)
 509  FORMAT (I3,3X,F10.6)
c      close(77)

 682  FORMAT (6F20.10)
 689   FORMAT (7F20.10)


 589  CONTINUE

      idflag=1
      XXO=DU1(1)
      YYO=DU1(2)
      ZZO=DU1(3)
      CALL RECORD(XXO,YYO,ZZO,1,0)

      frad=sqrt(((xion1+xion2)/2)**2+((yion1+yion2)/2.)**2)
	flam=sqrt((xion1-xion2)**2+(yion1-yion2)**2)
c      if (((flam.le.0.1.and.icount.le.10).or.flam.ge.5.)
c	*                               .and.intersect.eq.1) then
c         isOK=-1
c      endif


      print *, 'Work complete...' 
      if (isOK.eq.1) then
      if (intersect.eq.1) then
        if (isbadluck.eq.1) then
	    isintluck=0
	  else
	    isintluck=1
	  endif
	else
	  isintluck=0
	endif

c      open (1,FILE='grid.dat',STATUS='UNKNOWN',ACCESS='APPEND')
c      write (1,799) E,alfa*degrad, 
c     *    (xion2+xion1)/2.,(yion2+yion1)/2.,(zion2+zion1)/2.,isintluck
 799  FORMAT (5F15.5,3X,i1)
c	close(1)
c      open (1,FILE='zones.dat',STATUS='UNKNOWN',ACCESS='APPEND')
c      write (1,*) '(',xion1*10.,yion1*10.,zion1*10.,')'
c      write (1,*) '(',xion2*10.,yion2*10.,zion2*10.,')'
	close(1)
      if (isbadluck.eq.1.and.intersect.eq.1) then
      NG=1
      xg(NG)=(xzon(1)+xzon(icount/2))/2.
	yg(NG)=(yzon(1)+yzon(icount/2))/2.
	zg(NG)=(zzon(1)+zzon(icount/2))/2.
      inter(NG)=1
      Ebeam(NG)=E
      ugolAlfa(NG)=Alfa*degrad
c      open (1,FILE='griddata.dat',ACCESS='APPEND')
c	write(1,769) xg(Ng),yg(Ng),zg(NG),Ebeam(Ng),ugolalfa(Ng),inter(NG)
c      close(1)      
      NG=1
      xg(NG)=(xzon(icount)+xzon(icount/2))/2.
	yg(NG)=(yzon(icount)+yzon(icount/2))/2.
	zg(NG)=(zzon(icount)+zzon(icount/2))/2.
      inter(NG)=0
      Ebeam(NG)=E
      ugolAlfa(NG)=Alfa*degrad
c      open (1,FILE='griddata.dat',ACCESS='APPEND')
c	write(1,769) xg(Ng),yg(Ng),zg(NG),Ebeam(Ng),ugolalfa(Ng),inter(NG)
c      close(1)      
	else
      NG=1
      xg(NG)=(xion2+xion1)/2.
	yg(NG)=(yion2+yion1)/2.
	zg(NG)=(zion2+zion1)/2.
      inter(NG)=intersect
      Ebeam(NG)=E
      ugolAlfa(NG)=Alfa*degrad
c      open (1,FILE='griddata.dat',ACCESS='APPEND')
c	write(1,769) xg(Ng),yg(Ng),zg(NG),Ebeam(Ng),ugolalfa(Ng),isintluck
c      close(1)
	endif
 769  FORMAT (5F15.5,3X,I1)
	endif
c         print *, intersect
c        if (isfirst.eq.0) CALL ENDPG(0)

c         i=itransfer
c         xit1(ip,i)=xtransfer
c         yit1(ip,i)=ytransfer
c         zit1(ip,i)=ztransfer
         du1(1)=xit1(ip,i)
         du1(2)=yit1(ip,i)
         du1(3)=zit1(ip,i)
         i=itransfer
c         du1(4)=vxit1
c         du1(5)=vyit1
c         du1(6)=vzit1
         xit1(ip,i)=du1(1)
         yit1(ip,i)=du1(2)
         zit1(ip,i)=du1(3)





          if (isbadluck.eq.1) then
	       print*, 'badluck'
	       isbadluck=0
	       goto 515
	    endif

C Следюущие 2-е строчки следует стереть при изменении схемы зондирования
          if (intersect.eq.1) goto 520
          goto 515


 520    CONTINUE
	  xnewgr=xit1(ip,i)
	    ynewgr=yit1(ip,i)
	    znewgr=zit1(ip,i)
	    xoldgr=xit1(ip,i-1)
	    yoldgr=yit1(ip,i-1)
	    zoldgr=zit1(ip,i-1)
        intersect=0
        ision=0
        ind=0
	  isbadluck=0
c        i=i+1
c        xit1(ip,i)=du1(1)
c        yit1(ip,i)=du1(2)
c        zit1(ip,i)=du1(3)
         NR=0
c        i=i-1
c        du1(1)=xit1(ip,i)
c        du1(2)=yit1(ip,i)
c        du1(3)=zit1(ip,i)
        epso=eps1
c        dt2=5*dt1
        dt2=dt1
        isfirst=0
        imka=0
        imko=1
        imki=0
        inn1flag=0
        inn2flag=0
        isan=0
        isOK=1
	  isOK2=0
	  ision=0
        GOTO 1500


 
 400  FORMAT(3G15.6)
 402  FORMAT(F12.5)
 403  FORMAT(2F12.5)
 404  FORMAT(4G15.6)
 405  FORMAT(6G15.7)
 408  FORMAT(F5.1,3X,F10.6,3X,F10.6,3X,F6.2,3X,F6.2,3X,F6.2,3X,I1)
 409  FORMAT(F5.1,3X,F10.6,3X,F10.6,3X,F6.2,3X,F6.2,3X,F6.2,3X,I1,3X
     *                                                ,F6.2,3X,I3)
 412  FORMAT(F6.1,3X,F10.6,3X,F10.6,3X,F6.2,3X,F6.2,3X,F6.2,3X,I2,3X,I1)
 416  FORMAT(F20.7,3X,F20.7,3X,F20.7)
 418  FORMAT(F12.5,3X,F12.5,3X,F12.5,3X,F12.5)
 419  FORMAT(F12.5,3X,F12.5,3X,F12.5,3X,F12.5,3X,F12.5,3X,F12.5)

 515  CONTINUE


 9999 CONTINUE

         isan=0
c	     alfa=alfa-alfarazl*(-1)**ivertex
         iPlates=0
	   isfirst=1
         if (isOK.eq.1) kz=kz-1
         dalfa=dalfa*degrad
	   alfa=alfa*degrad

c      if (isOK.eq.-1) then
c     	  zapas=zapas+5.
c	  y_border=y_border+5.
c        isOK=1
c        goto 1550
c	endif
        iCT=0
        DT1=DT1*1E6
        idflag=0
        isplates=0
        beta0=beta0*degrad
c       print*, 'Кричи: "Ура!"'

      enddo
      enddo
      enddo



 8739   CONTINUE
           zgoal=zbeta

           print *, 'z=',zresult,zgoal,' U: ',Ubeta1_2
        if (zresult.lt.zgoal-0.0001) then
           iplus=1
	     if (iminus.eq.1) Ustep=Ustep/2.
	     Ubeta1_2=Ubeta1_2-Ustep
c		   read (*,*)
	     goto 193
	  endif
        if (zresult.gt.zgoal+0.0001) then
           iminus=1
	     if (iplus.eq.1) Ustep=Ustep/2.
	     Ubeta1_2=Ubeta1_2+Ustep
c		   read(*,*)
	     goto 193
	  endif




        alfa=alfa*degrad

        return
        END
