

      subroutine ORCPASS3(Uneeded
     *,xbeg,ybeg,zbeg,vxbeg,vybeg,vzbeg
     *,dt1,dt_small_primary,dt_small_secondary,isOK)
c ÔÓ„‡ÏÏ‡ ÔÓ‰·Ë‡ÂÚ Ì‡Ô. Ì‡ Ucorr2 ‰Îˇ ÔÓÔ‡‰‡ÌËˇ ‚ zd
c
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
	common /anal/Xan(2000),Yan(2000),FIan(2000,2000)
     *            ,NXan,NYan,xdconst,ydconst,Uan,analangle
      common/setka/xpse(500),ypse(500)
	common/plasma/xpl(51),ypl(51),BplR(51,51),BplZ(51,51)
      common/RKATUSHKA/RKAT1,RKAT2,DRKAT
C^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      common/G/XOLD,YOLD,zold,XTEK,YTEK,ztek,G(6)
	common/zd_known_/zd_known_
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
	common/cef/cef1,cef2
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
	common /beamfocus/ smeshenie00,smeshenie0,alfarazl0,alfarazl00
	*                  ,farang



      DIMENSION XDELTA(10),YDELTA(10),ZDELTA(10),X22(700),DU1(6),
     *DU2(6),LT(6),P(7),ZIT1(1,10000),EE(21),ZDEF(21),X13(100),Y13(100),
     *Z13(100),IV(10),JV(10),JAXV(10),JA1V(10),ZIT2(1,10000),XITI(10000)
     *,YITI(10000),ZITI(10000),XITJ(10000),YITJ(10000),ZITJ(10000),
     *rtpp(4),dalf(4),
     *dbet(4),xc(110),yc(110),yc1(110),XQ(5),YQ(5),DU(6),
     *RBON(50),ZBON(50),POB(2),POK(2),ENERGY(32),
     *KZarr(32),DTarr(32),xzon(1000),yzon(1000),zzon(1000)
      real radioniz(1000),xyshift(1000),uvl(1000),index
      REAL IPO,IVIT,IOH,fradArr(100),flamArr(100)
      character*12 SNAME1
      character*12 sname2
      character*6 CONTRO
      integer(4)      ret
      integer(2)      pattern

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
 


      index=1.


       Uplate2Max=Uplate2		 
       Uplate2Min=Uplate2
       NN_alfa=1
      Emax=E
      Emin=Emax
      NN_E=1
       beta0step=0.
	zapas=15.
      y_border=74.+zapas
	Rmax=40.
c       kz=999
      greshki=0.
      greshkix=0.
        isok=1
        knum=0



  
c	NN_E=1
c	NN_alfa=19
      i_E=1
	if (NN_alfa.eq.1.or.Uplate2Max.eq.Uplate2Min) then
	  stepU=0.
	else
	  stepU=(Uplate2Max-Uplate2Min)/(NN_alfa-1)
	endif
	if (NN_E.eq.1.or.EMax.eq.EMin) then
	  stepE=0.
	else
	  stepE=(EMax-EMin)/(NN_E-1)
	endif

C idflag=0 •·´® ≠• ≠„¶≠† ÆØ‚®¨®ß†Ê®Ô ØÆ beta §´Ô intersect=1
       idflag=0
C*****FILE FIELD.DAT (FIELD NETWORK) EXISTANSE PARAMETER*****
      IFOP=0
c      ifMntFldOpnd=0

C ÷ÂÌÚ ÔÓÎˇÌÓÈ ÒËÒÚÂÏ˚ ÍÓÓ‰ËÌ‡Ú
      Xnc=-1160.
	Ync= 30.



      iU=1
	E=Emin
 1111 CONTINUE
      alfa0=angle_of_injection(1)
      i_alfa=1
	iU=1
 2222 CONTINUE
 7877 CONTINUE
	 Uplate2=Uplate2Min+(iU-1)*stepU	!kV
	 iU=iU+1
      isan=1
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
C************* select Cs (ËÎË Tl) *******************************
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
C         T-10     P A R A M E T E R S
C
      RKAT1=55.5
      RKAT2=78.2
      DRKAT=RKAT2-RKAT1
c      RPL=30.
      RDIAF=40.
      NR=20
c     elon=2.0802
      elon=1.
      sdvig=0.
c      nvvi=4
c      zapas=25.







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
c         smeshenie00=-0.75 !0.7
c         smeshenie0=-0.75 !0.7
c        alfarazl0=0. !-0.36*(3.14159265/180.)
c        alfarazl00=0. !-0.36*(3.14159265/180.)
c        farang=58.75*3.14159265/180.
        xi00=xi0-beamshift*sin(farang) !151.69 + greshkix  
        yi00=yi0+beamshift*cos(farang) !297.55 + greshki  
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
      isBeamLoss=1
      iBeam=0
 5386 CONTINUE

      if (isBeamLoss.eq.0) then
	   NBegin=1
	   NBeam=2
	else
         NBegin=1 !2
c Ì‡‰Ó ‚ÂÌÛÚ¸Òˇ Í 2-ÍÂ, ÂÒÎË ‚˚¯Â Ì‡ 7 ÒÚÓ˜ÂÍ ÒÚÓËÚ isBeamLoss=0
	   NBeam=10
	endif

      ivertex=1 !do ivertex=NBegin,NBegin !NBeam
c      index=ivertex*0.1
      jvertex=1 !do jvertex=1,1

c      SNAME1='TRAJ0000.DAT'
c      WRITE(SNAME1(6:8),'(1I3)') kz
c      WRITE(*,10) SNAME1     

c      SNAME2='zone0000.DAT'
c      WRITE(SNAME2(6:8),'(1I3)') kz
c      WRITE(*,10) SNAME2
c  10  FORMAT(' ',A12)

c      if (ivertex.eq.1) then
c	  smeshenie=smeshenie0
c	  alfarazl=alfarazl0
c	elseif (ivertex.eq.2) then
c	  smeshenie=smeshenie0/2.
c	  alfarazl=alfarazl0/2.
c	elseif (ivertex.eq.3) then
c	  smeshenie=0.
c	  alfarazl=0.
c	elseif (ivertex.eq.4) then
c	  smeshenie=-smeshenie0/2.
c	  alfarazl=-alfarazl0/2.
c	elseif (ivertex.eq.5) then
c	  smeshenie=-smeshenie0
c	  alfarazl=-alfarazl0
c      endif

      smeshenie=-smeshenie00+(ivertex-1)*
	*                    (smeshenie0+smeshenie00)/(NBeam-1)
      alfarazl=-alfarazl00+(ivertex-1)*
	*                    (alfarazl0+alfarazl00)/(NBeam-1)

      xi=xi00-smeshenie*sin(farang)
	yi=yi00+smeshenie*cos(farang)
	alfa=alfa0+alfarazl

      zsmeshenie=0.
	betarazl=0.*(3.14159265/180.)

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
               detangle=(60.+analangle)*(3.14159265/180.)
               xdconst=xanc !120.221
	         ydconst=yanc !129.179
               apertura=0.
               sdvigdet=0. !+1.2
               xd0=xdconst + sdvigdet*sin(detangle)
               yd0=ydconst - sdvigdet*cos(detangle) !+ greshki
c              yd=yd-(apertura/2.)*(-1)**idet			   
               ZD= zd_known_ !zanc !-30. !-40.67628

               kvertex=1 !do kvertex=1,1

      flam1=0.
	flam0=0.

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

C%%%%%%%ñÖçíê çéÇéâ ëàëíÖåõ äééêÑ. à äééêÑàçÄíõ Ç ùíéâ ëàëíÖåÖ  %%%%%%
C%%%%%%%%%%%%%(àëè. Ñãü äéçíêéãü èéèÄÑ. Ç ÄèÖêíìêì ÄçÄãàáÄíéêÄ) %%%%%%
1500        CONTINUE
            print*, 'alfa=',alfa*degrad
		  print*, 'Usweep=',Uplate2-Uplate1
		  print*, 'E=',E
		  print*, 'smeshenie=',smeshenie 
              XN=Xnc
              YN=Ync
              RDNO=SQRT((XD-XN)**2+(YD-YN)**2)
              FINO=ATAN((YD-YN)/(XD-XN))
              DFI=ATAN(.0005/RDNO)
              FDL=FINO+DFI
              FDP=FINO-DFI

C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
c      OPEN(20,FILE='i_zones.DAT',STATUS='UNKNOWN',ACCESS='APPEND')
C      OPEN(17,FILE='ionpoint.DAT',STATUS='UNKNOWN')
C      OPEN(20,FILE='ionzones.DAT',STATUS='UNKNOWN')
C      OPEN(33,FILE='TEMP.DAT',STATUS='UNKNOWN')
c      OPEN(77,FILE=SNAME2,STATUS='UNKNOWN')
c      OPEN(30,FILE='z.dat',STATUS='UNKNOWN',ACCESS='APPEND')
c      OPEN(32,FILE='detline.dat',STATUS='UNKNOWN',ACCESS='APPEND')
c      OPEN(34,FILE='vz.dat',STATUS='UNKNOWN',ACCESS='APPEND')
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
            if (isfirst.eq.0) goto 1
C****************PARAMETERS******************************************
c       DT1=0.2
C      dt1=0.1
c                              [MCSEC]
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
	      qmCs=2.17481666738544E12
C                                      Cs
           A=47.065177783185                      ! Tl 205
c          A=47.5288741160247216                  ! Tl 203
           Vo(IP)=3.068080608349118E6*sqrt(E)	    ! Tl 205
           qmTl=1.4020739E12
c          Vo(IP)=3.08315726232824214E6*sqrt(E)	! Tl 203
C                                      Tl
   54     continue


C
C                    INITIAL POINT OF TRAJECTORY









C*****************OPEN 1st WINDOW (X-Y WIEV)****************
c	CALL AXES (xminG,yminG,xmaxG,ymaxG,10.,10.)

      call fglBegin(GL_quads)
      call fglColor3f (1.0, 0.0, 0.0)


      call fglVertex2f(xd+0.5, yd+0.5)
	call fglVertex2f(xd+0.5, yd-0.5)
      call fglVertex2f(xd-0.5, yd-0.5)
      call fglVertex2f(xd-0.5, yd+0.5)


	call fglEnd()

      call fglBegin(GL_quads)
      call fglColor3f (1.0, 0.0, 0.0)
	call fglVertex2f(xd+0.5+300., -zd*2.+0.5)
	call fglVertex2f(xd+0.5+300., -zd*2.-0.5)
	call fglVertex2f(xd-0.5+300., -zd*2.-0.5)
	call fglVertex2f(xd-0.5+300., -zd*2.+0.5)
	call fglEnd()

	
      CALL DECORD(XI,YI,ZI,1,0)









c      U2blp=0. !Ì‡˜‡Î¸ÌÓÂ ÔË·ÎËÊÂÌËÂ
	Ustep= 0.5
	iplus=0
	iminus=0
 196  CONTINUE
         
         I=1
          iCounter=0
c iCT.eq.0 ‚Æ´Ï™Æ Ø‡® Ø•‡¢Æ¨ Ø‡ÆÂÆ§•
          iCT=0

          DU1(1)=Xbeg
          DU1(2)=Ybeg
          DU1(3)=Zbeg
	    xoldgr=xbeg
	    yoldgr=ybeg
	    zoldgr=zbeg
	    xnewgr=du1(1)
	    ynewgr=du1(2)
	    znewgr=du1(3)
	    call fglLineWidth (1)

          DU1(4)=Vxbeg
          DU1(5)=Vybeg
          DU1(6)=Vzbeg
c         if (itochno.eq.1) then
c          write(34,416) DU1(6),VO(IP),ZI
c          CLOSE(34)
c         endif
          XIT1(IP,I)=DU1(1)
          YIT1(IP,I)=DU1(2)
          ZIT1(IP,I)=DU1(3)
          DT2=DT1
          EPSO=EPS1
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

c The flag to alert port beam loss
       isLost=0



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


          A1=2.*A	!Ò‡ÁÛ ‚ÚÓË˜Ì‡ˇ
	    qm=2.*qmTl !Ò‡ÁÛ ‚ÚÓË˜Ì‡ˇ
          VXIT1=DU1(4)
          VYIT1=DU1(5)
          VZIT1=DU1(6)
       RT11=SQRT((DU1(1)-XBEL)**2+(DU1(2)-YBEL)**2+DU1(3)**2)
       rt12=rt11*sign(1.,(du1(2)-YBEL))
       RT1=SQRT(DU1(1)**2+(DU1(2)/elon)**2)

       CALL INSAU(6,T,DU1,DT2,EPSO,0.,5.)



       RT1=SQRT(DU1(1)**2+(DU1(2)/elon)**2)
       RT11=SQRT((DU1(1)-XBEL)**2+(DU1(2)-YBEL)**2+DU1(3)**2)
       rt12=rt11*sign(1.,du1(2)-YBEL)
C*******************************************************************

  81      I=I+1
          XIT1(IP,I)=DU1(1)
          YIT1(IP,I)=DU1(2)
          ZIT1(IP,I)=DU1(3)
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

         if (sqrt(du1(1)**2+Du1(2)**2+du1(3)**2).gt.1000.) then
	     isOK=0
	     goto 342
	   endif
	   


          R2=SQRT((DU1(1)-XN)**2+(DU1(2)-YN)**2)




	  cc=tan((-30.+analangle)*3.14159265/180.)
        analdistY=51.84 *cos(banc)
	  analXd=xdconst+analdistY*sin(-analangle*3.14159265/180.)
	  analYd=ydconst+analdistY*cos(-analangle*3.14159265/180.)
	  bb=analYd-cc*analXd
        call fglBegin(GL_LINE_STRIP)
             call fglColor3f (1.0, 0.0, 0.0)
             call fglVertex2f(analXd+2.,bb+cc*(analXd+2.))
             call fglVertex2f(analXd-2.,bb+cc*(analXd-2.))
 	  call fglEnd()
	  call fglFlush()
       if (xit1(ip,i).gt. xanc) then
          x1=xit1(ip,i-1)
          y1=yit1(ip,i-1)
          z1=zit1(ip,i-1)
          x2=xit1(ip,i)
          y2=yit1(ip,i)
          z2=zit1(ip,i)
          zlast=z1+(xanc-x1)*(z2-z1)/(x2-x1)
       else
         goto 812
	 endif
c       if (yit1(ip,i).lt.bb+cc*xit1(ip,i)) then
c          x1=xit1(ip,i-1)
c          y1=yit1(ip,i-1)
c          z1=zit1(ip,i-1)
c          x2=xit1(ip,i)
c          y2=yit1(ip,i)
c          z2=zit1(ip,i)
c          cc1=(y2-y1)/(x2-x1)
c	    bb1=y1-cc1*x1
c	    xlast=(bb-bb1)/(cc1-cc)
c	    ylast=cc*xlast+bb
c	    zlast=(xlast-x1)*(z2-z1)/(x2-x1)+z1
c          if (ylast.gt.analYd) then
c             xylast=sqrt((xlast-analXd)**2+(ylast-analYd)**2)
c          else
c             xylast=-sqrt((xlast-analXd)**2+(ylast-analYd)**2)
c	    endif
c          frad=sqrt(xit1(ip,I)**2+yit1(ip,I)**2+zit1(ip,I)**2)
c	    fomega=atan_new(yit1(ip,I),xit1(ip,I))*180./3.14159265
c        else
c	    goto 812
c	  endif

        if (abs(Ucorr2).gt.30..or.abs(Ucorr1).gt.30.) then
	     isOK=0
	     goto 342
	  endif


        FINO=ATAN((DU1(2)-YN)/(DU1(1)-XN))
        if (abs(ustep).lt.1.E-6) goto 342

        if (zlast.gt.zd+0.01) then
	     Ucorr2=Ucorr2+Ustep*cef2
	     Ucorr1=Ucorr1-Ustep*cef1
	     iplus=1
           print *, 'U: ',Ucorr1,Ucorr2,' z:',zlast,zd
	     if (iminus.eq.1) Ustep=Ustep/2.
c	     read (*,*)
	     goto 196
	  endif
        if (zlast.lt.zd-0.01) then
	     Ucorr2=Ucorr2-Ustep*cef2
	     Ucorr1=Ucorr1+Ustep*cef1
	     iminus=1
           print *, 'U: ',Ucorr1,Ucorr2,' z:',zlast,zd
	     if (iplus.eq.1) Ustep=Ustep/2.
c	     read (*,*)
	     goto 196
	  endif


 342  CONTINUE



        DT1=DT1*1E6
        idflag=0
        isplates=0
           beta0=beta0*degrad
           alfa0=alfa0*degrad


        return
        END







