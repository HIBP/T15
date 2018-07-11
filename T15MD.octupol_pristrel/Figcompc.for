                SUBROUTINE FIG(T,DU,F)
      use msfwin
      use opengl
	COMMON H,A1,R,RDIAF,IPO,IVIT
      COMMON/RT/rt12
      COMMON /NNN/NCO1,NCO2,NCO3,NCHO,NCOR,NCOZ,NccR,NccZ
      COMMON /WIGU/RE(200),ZE(200),WIE(200),
C^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
     *RP(10000),ZP(10000),WIP(10000),HBRS,HBZS
C^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      COMMON/H/HVX,HVY,HVZ,HOHX,HOHY,HOHZ,HPLX,HPLY,HPLZ,
     *HMX,HMY,HMZ,HX,HY,HZ
C^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      COMMON/HTAT/HTAR(500,500),HTAZ(500,500)
      common/setka/xpse(500),ypse(500)
	common/plasma/xpl(51),ypl(51),BplR(51,51),BplZ(51,51)
      common/shotparameters/H0,CurTot,E,ugolA,xinj,yinj,xdet,ydet,dt_ext
	*                      ,zi_ext,beta_ext,alfa_ext
	COMMON/Hmain/xfm(78),yfm(82),zfm(39),
     *  HxFM(78,82,39),HyFM(78,82,39),HzFM(78,82,39),
	*  indxmax,indymax,indzmax
      common /elon/ elon
	common /csc/ Xcsc(300),Ycsc(300),HxCSC(300,300),HyCSC(300,300)
     *                 ,iCSC,jCSC
	common /csd/ Xcsd(300),Ycsd(300),HxCSD(300,300),HyCSD(300,300)
     *                 ,iCSD,jCSD
	common /csu/ Xcsu(300),Ycsu(300),HxCSU(300,300),HyCSU(300,300)
     *                 ,iCSU,jCSU
	common /hfcd/Xhfcd(300),Yhfcd(300),HxHFCD(300,300),HyHFCD(300,300)
     *                 ,iHFCD,jHFCD
	common /hfcu/Xhfcu(300),Yhfcu(300),HxHFCU(300,300),HyHFCU(300,300)
     *                 ,iHFCU,jHFCU
	common /pf1/ Xpf1(300),Ypf1(300),HxPF1(300,300),HyPF1(300,300)
     *                 ,iPF1,jPF1
	common /pf2/ Xpf2(300),Ypf2(300),HxPF2(300,300),HyPF2(300,300)
     *                 ,iPF2,jPF2
	common /pf3/ Xpf3(300),Ypf3(300),HxPF3(300,300),HyPF3(300,300)
     *                 ,iPF3,jPF3
	common /pf4/ Xpf4(300),Ypf4(300),HxPF4(300,300),HyPF4(300,300)
     *                 ,iPF4,jPF4
	common /pf5/ Xpf5(300),Ypf5(300),HxPF5(300,300),HyPF5(300,300)
     *                 ,iPF5,jPF5
	common /pf6/ Xpf6(300),Ypf6(300),HxPF6(300,300),HyPF6(300,300)
     *                 ,iPF6,jPF6

	common/currents/ CurCSC,CurCSD,CurCSU,
     *                 CurPF1,CurPF2,CurPF3,CurPF4,CurPF5,CurPF6,
     *                 CurHFCD,CurHFCU 

	common /p5v/x5bf(820),z5bf(500)
	*             ,FIUcf1(820,500),FIUcf2(820,500),qm
c	common /beta3/X5(180),Y5(180),Z5(180)
c	*           ,FI5_1(180,180,180),FI5_2(180,180,180),NX,NY,NZ
      common/faradey/ xf(1000),yf(1000),fif(1000,1000),nxf,nyf
	common/zsdvig_cor/ zsdvig_cor
c      common/octupol/ x2blp(1000),y2blp(1000),fi2blp(1000,1000)
c	*               ,nx2blp,ny2blp
	common/plates/x1pl(1000),y1pl(1000)
	*             ,FIpl1(1000,1000),FIpl2(1000,1000),nxpl,nypl 
	common/betafld/x1bf(820),z1bf(500)
	*             ,FIbf1(820,500),FIbf2(820,500),nxbf,nzbf 
	common /anal/Xan(2000),Yan(2000),FIan(2000,2000)
     *            ,NXan,NYan,xd,yd,Uan,analangle
	common/y_2blp_shift/ y_2blp_shift,y_beta3_shift
      common /kach/ xalfa,yalfa,plang
      common/oct/ x_oct(401),y_oct(171),z_oct(171),fi_oct(401,171,171),
     *            Nx_oct,Ny_oct,Nz_oct
      common/isNoField/ isNoField
 

      common /ionoprovod/ xa,ya,za,aalfa,balfa,
     *xbf,ybf,zbf,abeta,bbeta,
     *xbeta1,ybeta1,zbeta1,abeta1,bbeta1,
     *xfc,yfc,zfc,afar,bfar,
	*xUcf,yUcf,zUcf,aUcf,bUcf,
	*x2blpc,y2blpc,z2blpc,a2blpc,b2blpc,
	*xanc,yanc,zanc,banc,
	*xi0,yi0,zi_entered,
     *beamshift,beta0,angle_of_injection(1) 
	common/teta_2blp/teta_2blp,teta_an



      common/plate_voltages/Uplate1,Uplate2,Ufar,Ucorr1,Ucorr2
     *                    ,Ubf1,Ubf2,Ener,U2blp,currInt,ExtCurr,HcCurr
	*                    ,Ubeta1_1,Ubeta1_2


C^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      DIMENSION DU(6),F(6),XVIT(4),YVIT(4),NV(4),XTEK(1),YTEK(1),
     *HTEK(1)
        REAL    IPO,IVIT,IV, Pot_oct(8)
        LOGICAL BO
        DATA PI/3.141592654/
        NNETR=ncor
        NNETZ=ncoz
       X=DU(1)
       Y=DU(2)
       Z=DU(3)
       VX=DU(4)
       VY=DU(5)
       VZ=DU(6)
       SDVIG=0.

      isNoField=0
C%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


      NNETR=ncor
      NNETZ=ncoz


      RX=R+X
      BFI=ATAN(-Z/RX)
      BR=SQRT(Z**2+RX**2)
      COF=COS(BFI)
      SIF=SIN(BFI)
C                       PLASMA CURRENT FIELD
      X1=BR-R
      Y1=Y



      BO=X1.GE.XPSE(1).AND.Y1.GE.YPSE(1).AND.X1.LE.XPSE(NNETR).AND
     *.Y1.LE.YPSE(NNETZ)

      IF(BO) THEN				  
         HBR=QD2VL(x1,y1,NNETR,XPSE,NNETZ,YPSE,HTAR,500,.TRUE.)
         HBZ=QD2VL(x1,y1,NNETR,XPSE,NNETZ,YPSE,HTAZ,500,.TRUE.)
      ELSE
       HBR=0.
	 HBZ=0.
	 isNoField=1
      END IF



      HPLX=HBR*COF
      HPLY=HBZ
      HPLZ=-HBR*SIF

      CurTrans=CurTot/(1000.)
	HplX=CurTrans*HplX
	HplY=CurTrans*HplY
	HplZ=CurTrans*HplZ


	fiang=atan_new(y,x)
	fiang2=fiang-3.14159265
	if (fiang.gt.pi/2..and.fiang.lt.3.*pi/2.) then
	  radi=-sqrt(x**2+y**2)
	else
	  radi=sqrt(x**2+y**2)
	endif
	HPLR=HplX*cos(fiang)+HplY*sin(fiang)
	Hplfi=hplx*sin(fiang)-hply*cos(fiang)
c      prinT*,'Ј« ў­л© дЁЈ hbr=',hbr
c      prinT*,'Ј« ў­л© дЁЈ hbz=',hbz
c      prinT*,'Ј« ў­л© дЁЈ hplx=',hplx
c      prinT*,'Ј« ў­л© дЁЈ hply=',hply
c      print*,'Ј« ў­л© дЁЈ hplz=',hplz
c      read (*,*)
c      open (13,FILE='pole.dat',STATUS='UNKNOWN',ACCESS='APPEND')
c      write(13,200) x,y,radi,hplx,hply,hplr,hplfi,fiang*180./3.14159265,
c     *                        hplz
c      close(13)
200   FORMAT (9F12.5)



C поле управляющих витков и первичной обмотки
      xv=sqrt((x+R)**2+z**2)-R
	yv=y

      Curtrans=CurCSC/(1000.)
	HxCSC_=qd2vl(xv,yv,iCSC,xCSC,jCSC,yCSC,HxCSC,300,.TRUE.)
      HxCSC_=Curtrans*HxCSC_
	HyCSC_=qd2vl(xv,yv,iCSC,xCSC,jCSC,yCSC,HyCSC,300,.TRUE.)
      HyCSC_=Curtrans*HyCSC_

      Curtrans=CurCSU/(1000.)
	HxCSU_=qd2vl(xv,yv,iCSU,xCSU,jCSU,yCSU,HxCSU,300,.TRUE.)
      HxCSU_=Curtrans*HxCSU_
	HyCSU_=qd2vl(xv,yv,iCSU,xCSU,jCSU,yCSU,HyCSU,300,.TRUE.)
      HyCSU_=Curtrans*HyCSU_

      Curtrans=CurCSD/(1000.)
	HxCSD_=qd2vl(xv,yv,iCSD,xCSD,jCSD,yCSD,HxCSD,300,.TRUE.)
      HxCSD_=Curtrans*HxCSD_
	HyCSD_=qd2vl(xv,yv,iCSD,xCSD,jCSD,yCSD,HyCSD,300,.TRUE.)
      HyCSD_=Curtrans*HyCSD_

      Curtrans=CurPF1/(1000.)
	HxPF1_=qd2vl(xv,yv,iPF1,xPF1,jPF1,yPF1,HxPF1,300,.TRUE.)
      HxPF1_=Curtrans*HxPF1_
	HyPF1_=qd2vl(xv,yv,iPF1,xPF1,jPF1,yPF1,HyPF1,300,.TRUE.)
      HyPF1_=Curtrans*HyPF1_

      Curtrans=CurPF2/(1000.)
	HxPF2_=qd2vl(xv,yv,iPF2,xPF2,jPF2,yPF2,HxPF2,300,.TRUE.)
      HxPF2_=Curtrans*HxPF2_
	HyPF2_=qd2vl(xv,yv,iPF2,xPF2,jPF2,yPF2,HyPF2,300,.TRUE.)
      HyPF2_=Curtrans*HyPF2_

      Curtrans=CurPF3/(1000.)
	HxPF3_=qd2vl(xv,yv,iPF3,xPF3,jPF3,yPF3,HxPF3,300,.TRUE.)
      HxPF3_=Curtrans*HxPF3_
	HyPF3_=qd2vl(xv,yv,iPF3,xPF3,jPF3,yPF3,HyPF3,300,.TRUE.)
      HyPF3_=Curtrans*HyPF3_

      Curtrans=CurPF4/(1000.)
	HxPF4_=qd2vl(xv,yv,iPF4,xPF4,jPF4,yPF4,HxPF4,300,.TRUE.)
      HxPF4_=Curtrans*HxPF4_
	HyPF4_=qd2vl(xv,yv,iPF4,xPF4,jPF4,yPF4,HyPF4,300,.TRUE.)
      HyPF4_=Curtrans*HyPF4_

      Curtrans=CurPF5/(1000.)
	HxPF5_=qd2vl(xv,yv,iPF5,xPF5,jPF5,yPF5,HxPF5,300,.TRUE.)
      HxPF5_=Curtrans*HxPF5_
	HyPF5_=qd2vl(xv,yv,iPF5,xPF5,jPF5,yPF5,HyPF5,300,.TRUE.)
      HyPF5_=Curtrans*HyPF5_

      Curtrans=CurPF6/(1000.)
	HxPF6_=qd2vl(xv,yv,iPF6,xPF6,jPF6,yPF6,HxPF6,300,.TRUE.)
      HxPF6_=Curtrans*HxPF6_
	HyPF6_=qd2vl(xv,yv,iPF6,xPF6,jPF6,yPF6,HyPF6,300,.TRUE.)
      HyPF6_=Curtrans*HyPF6_

      Curtrans=CurHFCD/(1000.)
	HxHFCD_=qd2vl(xv,yv,iHFCD,xHFCD,jHFCD,yHFCD,HxHFCD,300,.TRUE.)
      HxHFCD_=Curtrans*HxHFCD_
	HyHFCD_=qd2vl(xv,yv,iHFCD,xHFCD,jHFCD,yHFCD,HyHFCD,300,.TRUE.)
      HyHFCD_=Curtrans*HyHFCD_

      Curtrans=CurHFCU/(1000.)
	HxHFCU_=qd2vl(xv,yv,iHFCU,xHFCU,jHFCU,yHFCU,HxHFCU,300,.TRUE.)
      HxHFCU_=Curtrans*HxHFCU_
	HyHFCU_=qd2vl(xv,yv,iHFCU,xHFCU,jHFCU,yHFCU,HyHFCU,300,.TRUE.)
      HyHFCU_=Curtrans*HyHFCU_




      HVR= HxCSC_+HxCSU_+HxCSD_+
	+     HxPF1_+HxPF2_+HxPF3_+HxPF4_+HxPF5_+HxPF6_+
	+     HxHFCD_+HxHFCU_
      HVZ= HyCSC_+HyCSU_+HyCSD_+
     +     HyPF1_+HyPF2_+HyPF3_+HyPF4_+HyPF5_+HyPF6_+
	+     HyHFCD_+HyHFCU_


      HVX=HVR*COF
      HVY=HVZ
      HVZ=-HVR*SIF



c предположим наклон камеры
       camang=90.*(3.14159265/180.)
       xc=R*sin(camang) - R
       yc=R*cos(camang)
	 x1= (x-xc)*sin(camang)+(y-yc)*cos(camang)
	 y1=-(x-xc)*cos(camang)+(y-yc)*sin(camang)


C       “¤Ґа¦Ёў ойҐҐ Ї®«Ґ
      bo=x1.ge.xfm(1).and.y1.ge.yfm(1).and.z.ge.zfm(1).and.
     *   x1.le.xfm(indxmax).and.
     *   y1.le.yfm(indymax).and.
     *   z.le.zfm(indzmax)
	if (bo) then	!.and.sqrt(x**2+y**2).lt.120.
	  HXMtmp=qd3vl(x1,y1,z,indxmax,xfm,indymax,yfm,indzmax,zfm,
     *            HxFM,indxmax,indymax,.TRUE.)
	  HYMtmp=qd3vl(x1,y1,z,indxmax,xfm,indymax,yfm,indzmax,zfm,
     *            HyFM,indxmax,indymax,.TRUE.)
	  HZM=qd3vl(x1,y1,z,indxmax,xfm,indymax,yfm,indzmax,zfm,
     *            HzFM,indxmax,indymax,.TRUE.)
	  HXM=Hxmtmp*sin(camang)-Hymtmp*cos(camang)
	  HYM=Hxmtmp*cos(camang)+Hymtmp*sin(camang)
        IF(abs(Z).eq.0.) THEN
        HXM=0.
        HYM=0.
        END IF
	else
c	  BR=0.01*BR
c        BZ=0.01*BZ
c        CALL COBFIL(ALFA,DRK,RO,N,M,BR,BFI,BZ,DI,
c     *  HFIM,HRM,HZZM,HH)
c        HXM=1E4*(HRM*COF-HFIM*SIF)
c        HYM=1E4*HZZM
c        HZM=-1E4*(HRM*SIF+HFIM*COF)
c        IF(Z.EQ.0.0) THEN
c        HXM=0.
c        HYM=0.
c        END IF
         HXM=0.
	   HYM=0.
	   HZM=0.
	   print *, 'net polja'
	   print *, x1,y1,z
c	   read (*,*)
         isNoField=1
	endif
      


c Удерживающее поле на оси [в Теслах]
	  Htrans=H0/1.0
	  HXM=Htrans*HXM
	  HYM=Htrans*HYM
	  HZM=Htrans*HZM
	  	  
c      open (10,FILE='testfield.dat',STATUS='UNKNOWN',ACCESS='APPEND')
c      write (10,467) x,y,sqrt(x**2+y**2),HXM,HYM,HZM
c	close(10)
c 467  FORMAT(6F20.10)


c       GO TO 2
c       open (19,FILE='HZ.dat',STATUS='UNKNOWN',ACCESS='APPEND')
c       write(19,200) BR,BZ,HXM,HYM,HZM
c       print *, br,bz,hxm,hym,hzm
c       close (19)
c200    FORMAT (5F20.10)
C      Љ®¬Ї®­Ґ­вл аҐ§г«мвЁагойҐЈ® ¬ Ј­Ёв­®Ј® Ї®«п [набвҐ¤]
   2    HX=HXM+HPLX +HVX
        HY=HYM+HPLY +HVY
        HZ=HZM+HPLZ +HVZ

c        print *, HXM,HYM,HZM
c	  read(*,*)


c     prinT*,'Ј« ў­л© дЁЈ hx=',hx
c     prinT*,'Ј« ў­л© дЁЈ hy=',hy
c     prinT*,'Ј« ў­л© дЁЈ hz=',hz
c     prinT*,'            hr=',sqrt(hx**2+hy**2)
c     prinT*,'           hfi=',atan(hz/hx)
c     prinT*,'Ј« ў­л© дЁЈ hxm=',hxm
c     prinT*,'Ј« ў­л© дЁЈ hym=',hym
c     prinT*,'Ј« ў­л© дЁЈ hzm=',hzm
c     prinT*,'Ј« ў­л© дЁЈ hvx=',hvx
c     prinT*,'Ј« ў­л© дЁЈ hvy=',hvy
c     prinT*,'Ј« ў­л© дЁЈ hvz=',hvz


C Поле анализатора
      anang=analangle*(3.14159265/180.)
c	x1= (x-xd)*cos(anang)+(y-yd)*sin(anang)
c	y1=-(x-xd)*sin(anang)+(y-yd)*cos(anang)
      call shift(xanc,yanc,zanc,x,y,z,x3,y3,z3)
	call rotate(teta_an,y3,z3,y1,z1)
	x1=x3
	call rotate(banc,z1,x1,z2,x2)
	y2=y1
	call rotate(anang,x2,y2,x1,y1)
	z1=z2

	if (x1.lt.5.and.x1.gt.-17..and.y1.lt.60..and.y1.gt.-5.) then
c	  read (*,*)
	  dx=0.3
	  dy=0.3
	  fi1x=qd2vl(x1+dx,y1,NXan,xan,NYan,yan,FIan,2000,.TRUE.)
	  fi2x=qd2vl(x1-dx,y1,NXan,xan,NYan,yan,FIan,2000,.TRUE.)
	  fi1y=qd2vl(x1,y1+dx,NXan,xan,NYan,yan,FIan,2000,.TRUE.)
        fi2y=qd2vl(x1,y1-dx,NXan,xan,NYan,yan,FIan,2000,.TRUE.)
	  Exantmp=-(fi1x-fi2x)/(2.*dx)
	  Eyantmp=-(fi1y-fi2y)/(2.*dy)
        Ezantmp=0.
	call rotate(-anang,Exantmp,Eyantmp,Exantmp1,Eyantmp1)
	Ezantmp1=Ezantmp
	call rotate(-banc,Ezantmp1,Exantmp1,Ezantmp,Exantmp)
	Eyantmp=Eyantmp1
      call rotate(-teta_an,Eyantmp,Ezantmp,Eyan,Ezan)
	Exan=Exantmp
c	  Exan=Exantmp*cos(anang)-Eyantmp*sin(anang)
c	  Eyan=Exantmp*sin(anang)+Eyantmp*cos(anang)
	  Exan=(Uan/50.)*Exan
	  Eyan=(Uan/50.)*Eyan
	  Ezan=(Uan/50.)*Ezan
c        print *, 'An:',Exan,Eyan,Ezan,banc*180./3.14159265
c	  read(*,*)
	else
	  Exan=0.
	  Eyan=0.
	  Ezan=0.
	endif
      




C Поле пластин "альфа-фарадея"
      call shift (xfc,yfc,zfc,x,y,z,x1,y1,z1)
	call rotate (bfar,z1,x1,z2,x2)
	y2=y1
	call rotate (afar,x2,y2,x1,y1)
	z1=z2

	if (x1.lt.20..and.x1.gt.-20..and.y1.lt.10..and.y1.gt.-10.) then
c        print *, 'faradey'
c	  read (*,*)
	  dx=0.3
	  dy=0.3
	  fi1x=qd2vl(x1+dx,y1,NXf,xf,NYf,yf,FIf,1000,.TRUE.)
	  fi2x=qd2vl(x1-dx,y1,NXf,xf,NYf,yf,FIf,1000,.TRUE.)
	  fi1y=qd2vl(x1,y1+dx,NXf,xf,NYf,yf,FIf,1000,.TRUE.)
        fi2y=qd2vl(x1,y1-dx,NXf,xf,NYf,yf,FIf,1000,.TRUE.)
	  Exftmp=-(fi1x-fi2x)/(2.*dx)
	  Eyftmp=-(fi1y-fi2y)/(2.*dy)
        Ezftmp=0.
	  call rotate(-afar,Exftmp,Eyftmp,Exftmp1,Eyftmp1)
	  Ezftmp1=Ezftmp
	  call rotate(-bfar,Ezftmp1,Exftmp1,Ezf,Exf)
	  Eyf=Eyftmp1
	  Exf=(Ufar/1.)*Exf
	  Eyf=(Ufar/1.)*Eyf
	  Ezf=(Ufar/1.)*Ezf
	else
	  Exf=0.
	  Eyf=0.
	  Ezf=0.
	endif


cC Поле пластин вторичного бимлайна
cc      ang2blp=0.*(3.14159265/180.)
cc      x2blpc=91.165 !91.064 !90.667
cc	y2blpc=69.874 !69.667 !69.302
cc	x11=x-x2blpc
cc	y11=y-y2blpc ! перенос центра координат
cc      x1= x11*cos(ang2blp)+y11*sin(ang2blp)
cc	y1=-x11*sin(ang2blp)+y11*cos(ang2blp) !поворот на угол
c      call shift (x2blpc,y2blpc,z2blpc,x,y,z,x3,y3,z3)
c	call rotate(teta_2blp,y3,z3,y1,z1)
c	x1=x3
c	call rotate(b2blpc,z1,x1,z2,x2)
c	y2=y1
c	call rotate(a2blpc,x2,y2,x1,y1)
c	z1=z2
c	y1=y1-y_2blp_shift
cc      print *, x,x2blpc
cc	print *, y,y2blpc
cc	read(*,*)
c	if (x1.lt.20..and.x1.gt.-20..and.y1.lt.10.0.and.y1.gt.-10.0) then
cc        print *, '2nd beamline plates'
cc	  read (*,*)

c        call fglBegin(GL_POINTS)
c         if (abs(x1).lt.10.) then
c	      call fglColor3f(1.,0.,0.)
c	   else
c	      call fglColor3f(0.,0.,1.)
c	   endif
c	   if (y1.gt.5.0) then
c	      call fglColor3f(1.,1.,1.)
c	   endif
c	   if (y1.lt.-5.0) then
c	      call fglColor3f(1.,1.,0.)
c	   endif
c	   call fglVertex2f (x,y+0.1)
c	   call fglvertex2f (x,y-0.1)
c	   call fglVertex2f (x+0.1,y)
c	   call fglvertex2f (x-0.1,y)
c 	   call fglEnd()
c	  call fglFlush()

c	   if (abs(x1).lt.10..and.abs(y1).gt.4.9) then
c	      isA3Flag=0
c	   else
c	      isA3Flag=1
c	   endif


c	 dx=0.1
c	 dy=0.1
c	 fi1x=qd2vl(x1+dx,y1,NX2blp,x2blp,NY2blp,y2blp,FI2blp,1000,.TRUE.)
c	 fi2x=qd2vl(x1-dx,y1,NX2blp,x2blp,NY2blp,y2blp,FI2blp,1000,.TRUE.)
c	 fi1y=qd2vl(x1,y1+dx,NX2blp,x2blp,NY2blp,y2blp,FI2blp,1000,.TRUE.)
c       fi2y=qd2vl(x1,y1-dx,NX2blp,x2blp,NY2blp,y2blp,FI2blp,1000,.TRUE.)
c	 Ex2blptmp=-(fi1x-fi2x)/(2.*dx)
c	 Ey2blptmp=-(fi1y-fi2y)/(2.*dy)
c       Ez2blptmp=0.
c	 call rotate(-a2blpc,Ex2blptmp,Ey2blptmp,Ex2blptmp1,Ey2blptmp1)
c	 Ez2blptmp1=Ez2blptmp
c	 call rotate(-b2blpc,Ez2blptmp1,Ex2blptmp1,Ez2blp0,Ex2blp0)
c	 Ey2blp0=Ey2blptmp1
c       call rotate(-teta_2blp,Ey2blp0,Ez2blp0,Ey2blp,Ez2blp)
c	 Ex2blp=Ex2blp0

cc	 Ex2blp= Ex2blptmp*cos(-ang2blp)-Ey2blptmp*sin(-ang2blp)
cc	 Ey2blp=-Ex2blptmp*sin(-ang2blp)+Ey2blptmp*cos(-ang2blp)
c	 Ex2blp=(U2blp/3.)*Ex2blp
c	 Ey2blp=(U2blp/3.)*Ey2blp
c	 Ez2blp=(U2blp/3.)*Ez2blp

c	else
c	 Ex2blp=0.
c	 Ey2blp=0.
c	 Ez2blp=0.
c	endif


C Поле бета-пластин

c      angbf=14.*(3.14159265/180.)
c      xbf=70.03 + greshkix
c	ybf=162.99 + greshki
c	x1= (x-xbf)*sin(angbf)+(y-ybf)*cos(angbf)
c	y1=-(x-xbf)*cos(angbf)+(y-ybf)*sin(angbf)
c	z1= z - zbf
      call shift(xbf,ybf,zbf,x,y,z,x1,y1,z1)
	call rotate(bbeta,z1,x1,z2,x2)
	y2=y1
	call rotate(abeta,x2,y2,x1,y1)
	z1=z2
	if (x1.lt.20.and.x1.gt.-20..and.y1.lt.10..and.y1.gt.-10.
	*.and.z1.lt.10..and.z1.gt.-10.) then
        call fglBegin(GL_POINTS)
         if (abs(x1).lt.5.5) then
	      call fglColor3f(1.,0.,0.)
	   else
	      call fglColor3f(0.,0.,1.)
	   endif
	   if (z1.gt.1.5) then
	      call fglColor3f(1.,1.,1.)
	   endif
	   if (z1.lt.-1.5) then
	      call fglColor3f(0.,1.,1.)
	   endif
	   call fglVertex2f (x,y+0.1)
	   call fglvertex2f (x,y-0.1)
	   call fglVertex2f (x+0.1,y)
	   call fglvertex2f (x-0.1,y)
 	   call fglEnd()
	  call fglFlush()
c        print *, 'beta'
c	  read(*,*)
	  dx=0.3
	  dz=0.3
	  fi1x_1=qd2vl(x1+dx,z1,NXbf,x1bf,NZbf,z1bf,FIbf1,820,.TRUE.)
	  fi1x_2=qd2vl(x1+dx,z1,NXbf,x1bf,NZbf,z1bf,FIbf2,820,.TRUE.)
        fi1x=(Ubf1/1.)*fi1x_1+(Ubf2/1.)*fi1x_2
	  fi2x_1=qd2vl(x1-dx,z1,NXbf,x1bf,NZbf,z1bf,FIbf1,820,.TRUE.)
	  fi2x_2=qd2vl(x1-dx,z1,NXbf,x1bf,NZbf,z1bf,FIbf2,820,.TRUE.)
        fi2x=(Ubf1/1.)*fi2x_1+(Ubf2/1.)*fi2x_2
	  fi1z_1=qd2vl(x1,z1+dz,NXbf,x1bf,Nzbf,z1bf,FIbf1,820,.TRUE.)
	  fi1z_2=qd2vl(x1,z1+dz,NXbf,x1bf,Nzbf,z1bf,FIbf2,820,.TRUE.)
        fi1z=(Ubf1/1.)*fi1z_1+(Ubf2/1.)*fi1z_2
        fi2z_1=qd2vl(x1,z1-dz,NXbf,x1bf,Nzbf,z1bf,FIbf1,820,.TRUE.)
        fi2z_2=qd2vl(x1,z1-dz,NXbf,x1bf,Nzbf,z1bf,FIbf2,820,.TRUE.)
        fi2z=(Ubf1/1.)*fi2z_1+(Ubf2/1.)*fi2z_2
	  Exbftmp=-(fi1x-fi2x)/(2.*dx)
	  Ezbftmp=-(fi1z-fi2z)/(2.*dz)
        Eybftmp=0.
	  call rotate(-abeta,Exbftmp,Eybftmp,Exbftmp1,Eybftmp1)
	  Ezbftmp1=Ezbftmp
        call rotate(-bbeta,Ezbftmp1,Exbftmp1,Ezbf,Exbf)
	  Eybf=Eybftmp1
c	  Exbf=Exbftmp*sin(angbf)
c	  Eybf=Exbftmp*cos(angbf)
c	  Ezbf=Ezbftmp
	else
	  Exbf=0.
	  Eybf=0.
	  Ezbf=0.
	endif



C Поле бета-пластин #1

      call shift(xbeta1,ybeta1,zbeta1,x,y,z,x1,y1,z1)
	call rotate(bbeta1,z1,x1,z2,x2)
	y2=y1
	call rotate(abeta1,x2,y2,x1,y1)
	z1=z2
	if (x1.lt.20.and.x1.gt.-20..and.y1.lt.10..and.y1.gt.-10.
	*.and.z1.lt.10..and.z1.gt.-10.) then
        call fglBegin(GL_POINTS)
         if (abs(x1).lt.5.5) then
	      call fglColor3f(1.,0.,0.)
	   else
	      call fglColor3f(0.,0.,1.)
	   endif
	   if (z1.gt.1.5) then
	      call fglColor3f(1.,1.,1.)
	   endif
	   if (z1.lt.-1.5) then
	      call fglColor3f(0.,1.,1.)
	   endif
	   call fglVertex2f (x,y+0.1)
	   call fglvertex2f (x,y-0.1)
	   call fglVertex2f (x+0.1,y)
	   call fglvertex2f (x-0.1,y)
 	   call fglEnd()
	  call fglFlush()
	  dx=0.3
	  dz=0.3
	  fi1x_1=qd2vl(x1+dx,z1,NXbf,x1bf,NZbf,z1bf,FIbf1,820,.TRUE.)
	  fi1x_2=qd2vl(x1+dx,z1,NXbf,x1bf,NZbf,z1bf,FIbf2,820,.TRUE.)
        fi1x=(Ubeta1_1/1.)*fi1x_1+(Ubeta1_2/1.)*fi1x_2
	  fi2x_1=qd2vl(x1-dx,z1,NXbf,x1bf,NZbf,z1bf,FIbf1,820,.TRUE.)
	  fi2x_2=qd2vl(x1-dx,z1,NXbf,x1bf,NZbf,z1bf,FIbf2,820,.TRUE.)
        fi2x=(Ubeta1_1/1.)*fi2x_1+(Ubeta1_2/1.)*fi2x_2
	  fi1z_1=qd2vl(x1,z1+dz,NXbf,x1bf,Nzbf,z1bf,FIbf1,820,.TRUE.)
	  fi1z_2=qd2vl(x1,z1+dz,NXbf,x1bf,Nzbf,z1bf,FIbf2,820,.TRUE.)
        fi1z=(Ubeta1_1/1.)*fi1z_1+(Ubeta1_2/1.)*fi1z_2
        fi2z_1=qd2vl(x1,z1-dz,NXbf,x1bf,Nzbf,z1bf,FIbf1,820,.TRUE.)
        fi2z_2=qd2vl(x1,z1-dz,NXbf,x1bf,Nzbf,z1bf,FIbf2,820,.TRUE.)
        fi2z=(Ubeta1_1/1.)*fi2z_1+(Ubeta1_2/1.)*fi2z_2
	  Exbftmp=-(fi1x-fi2x)/(2.*dx)
	  Ezbftmp=-(fi1z-fi2z)/(2.*dz)
        Eybftmp=0.
	  call rotate(-abeta1,Exbftmp,Eybftmp,Exbftmp1,Eybftmp1)
	  Ezbftmp1=Ezbftmp
        call rotate(-bbeta1,Ezbftmp1,Exbftmp1,Ezb1,Exb1)
	  Eyb1=Eybftmp1
c        print *, 'beta1'
c        print *, Exb1,Eyb1,Ezb1
c		print *, Ubeta1_1,Ubeta1_2
c	  read(*,*)

	else
	  Exb1=0.
	  Eyb1=0.
	  Ezb1=0.
	endif



C Поле пластин "альфа-качалка"
c      kachshift=0.
c      plang=14.*(3.14159265/180.)
c      xalfa=xa - kachshift*sin(plang)
c	yalfa=ya + kachshift*cos(plang)
c      xa=44.44 	+ greshkix
c	ya=121.67 + greshki 
c	x1= (x-xalfa)*sin(plang)+(y-yalfa)*cos(plang)
c	y1=-(x-xalfa)*cos(plang)+(y-yalfa)*sin(plang)
      call shift(xa,ya,za,x,y,z,x1,y1,z1)
	call rotate(balfa,z1,x1,z2,x2)
	y2=y1
	call rotate(aalfa,x2,y2,x1,y1)
	z1=z2

	if (x1.lt.17..and.x1.gt.-25..and.y1.lt.10..and.y1.gt.-10.) then
c        print *, 'kachalka'
c		print *, x,y,z
c		print *, x1,y1,z1
c	  read(*,*)
        call fglBegin(GL_POINTS)
         if (x1.lt.4..and.x1.gt.-12.) then
	      call fglColor3f(1.,0.,0.)
	   else
	      call fglColor3f(0.,0.,1.)
	   endif
	   if (y1.gt.1.5) then
	      call fglColor3f(1.,1.,1.)
	   endif
	   if (y1.lt.-1.5) then
	      call fglColor3f(0.,1.,1.)
	   endif
	   call fglVertex2f (x,y+0.15)
	   call fglvertex2f (x,y-0.15)
	   call fglVertex2f (x+0.15,y)
	   call fglvertex2f (x-0.15,y)
 	   call fglEnd()
	  call fglFlush()
	  dx=0.3
	  dy=0.3
	  fi1x_1=qd2vl(x1+dx,y1,NXpl,x1pl,NYpl,y1pl,FIpl1,1000,.TRUE.)
	  fi1x_2=qd2vl(x1+dx,y1,NXpl,x1pl,NYpl,y1pl,FIpl2,1000,.TRUE.)
        fi1x=(Uplate1/3.)*fi1x_1+(Uplate2/3.)*fi1x_2
	  fi2x_1=qd2vl(x1-dx,y1,NXpl,x1pl,NYpl,y1pl,FIpl1,1000,.TRUE.)
	  fi2x_2=qd2vl(x1-dx,y1,NXpl,x1pl,NYpl,y1pl,FIpl2,1000,.TRUE.)
        fi2x=(Uplate1/3.)*fi2x_1+(Uplate2/3.)*fi2x_2
	  fi1y_1=qd2vl(x1,y1+dx,NXpl,x1pl,NYpl,y1pl,FIpl1,1000,.TRUE.)
	  fi1y_2=qd2vl(x1,y1+dx,NXpl,x1pl,NYpl,y1pl,FIpl2,1000,.TRUE.)
        fi1y=(Uplate1/3.)*fi1y_1+(Uplate2/3.)*fi1y_2
        fi2y_1=qd2vl(x1,y1-dx,NXpl,x1pl,NYpl,y1pl,FIpl1,1000,.TRUE.)
        fi2y_2=qd2vl(x1,y1-dx,NXpl,x1pl,NYpl,y1pl,FIpl2,1000,.TRUE.)
        fi2y=(Uplate1/3.)*fi2y_1+(Uplate2/3.)*fi2y_2
	  Expltmp=-(fi1x-fi2x)/(2.*dx)
	  Eypltmp=-(fi1y-fi2y)/(2.*dy)
	  Ezpltmp=0.
	  call rotate (-aalfa,Expltmp,Eypltmp,Expltmp1,Eypltmp1)
	  Ezpltmp1=Ezpltmp
        call rotate (-balfa,Ezpltmp1,Expltmp1,Ezpl,Expl)
	  Eypl=Eypltmp1
c	  Expl=Expltmp*sin(plang)-Eypltmp*cos(plang)
c	  Eypl=Expltmp*cos(plang)+Eypltmp*sin(plang)

	else
	  Expl=0.
	  Eypl=0.
	  Ezpl=0.
	endif





cC     поле пластин Ucorr

cc      angUcf=0.*(3.14159265/180.)
cc      xbf=70.03 + greshkix
cc	ybf=162.99 + greshki
cc	x1= (x-xUcf)*cos(angUcf)+(y-yUcf)*sin(angUcf)
cc	y1=-(x-xUcf)*sin(angUcf)+(y-yUcf)*cos(angUcf)
cc	z1= z - zUcf

c      call shift(xUcf,yUcf,zUcf-zsdvig_cor,x,y,z,x3,y3,z3)
c	call rotate(teta_2blp,y3,z3,y1,z1)
c	x1=x3
c	call rotate(bUcf,z1,x1,z2,x2)
c	y2=y1
c	call rotate(aUcf,x2,y2,x1,y1)
c	z1=z2

c      y1=y1-y_beta3_shift

c      bo=x1.ge.x5(1).and.y1.ge.y5(1).and.z1.ge.z5(1).and.
c     *   x1.le.x5(Nx).and.y1.le.y5(NY).and.z1.le.z5(NZ)

c      if (bo) then
cc	  read (*,*)

c        call fglBegin(GL_POINTS)
c         if (abs(x1).lt.15.) then
c	      call fglColor3f(0.5,0.3,0.)
c	   else
c	      call fglColor3f(0.,0.,1.)
c	   endif
c	   if (y1.gt.10.0) then
c	      call fglColor3f(1.,1.,1.)
c	   endif
c	   if (y1.lt.-10.0) then
c	      call fglColor3f(1.,1.,0.)
c	   endif
c	   if (z1.lt.-7.4) then
c	      call fglColor3f(0.5,0.5,0.5)
c	   endif
c	   if (z1.gt.7.4) then
c	      call fglColor3f(0.2,0.9,0.4)
c	   endif
c	   call fglVertex2f (x,y+0.1)
c	   call fglvertex2f (x,y-0.1)
c	   call fglVertex2f (x+0.1,y)
c	   call fglvertex2f (x-0.1,y)
c 	   call fglEnd()
c	  call fglFlush()

c	   if (abs(x1).lt.15..and.abs(z1).gt.7.4) then
c	      isCorrFlag=0
c	   else
c	      isCorrFlag=1
c	   endif


c	  dx=0.3
c	  dy=0.3
c	  dz=0.3
c	  fi1x_1=qd3vl(x1+dx,y1,z1,NX,x5,NY,y5,NZ,z5,FI5_1,180,180,.TRUE.)
c	  fi1x_2=qd3vl(x1+dx,y1,z1,NX,x5,NY,y5,NZ,z5,FI5_2,180,180,.TRUE.)
c        fi1x=(Ucorr1/10.)*fi1x_1+(Ucorr2/10.)*fi1x_2
c	  fi2x_1=qd3vl(x1-dx,y1,z1,NX,x5,NY,y5,NZ,z5,FI5_1,180,180,.TRUE.)
c	  fi2x_2=qd3vl(x1-dx,y1,z1,NX,x5,NY,y5,NZ,z5,FI5_2,180,180,.TRUE.)
c        fi2x=(Ucorr1/10.)*fi2x_1+(Ucorr2/10.)*fi2x_2
c	  fi1y_1=qd3vl(x1,y1+dy,z1,NX,x5,NY,y5,NZ,z5,FI5_1,180,180,.TRUE.)
c	  fi1y_2=qd3vl(x1,y1+dy,z1,NX,x5,NY,y5,NZ,z5,FI5_2,180,180,.TRUE.)
c        fi1y=(Ucorr1/10.)*fi1y_1+(Ucorr2/10.)*fi1y_2
c        fi2y_1=qd3vl(x1,y1-dy,z1,NX,x5,NY,y5,NZ,z5,FI5_1,180,180,.TRUE.)
c        fi2y_2=qd3vl(x1,y1-dy,z1,NX,x5,NY,y5,NZ,z5,FI5_2,180,180,.TRUE.)
c        fi2y=(Ucorr1/10.)*fi2y_1+(Ucorr2/10.)*fi2y_2
c	  fi1z_1=qd3vl(x1,y1,z1+dz,NX,x5,NY,y5,NZ,z5,FI5_1,180,180,.TRUE.)
c	  fi1z_2=qd3vl(x1,y1,z1+dz,NX,x5,NY,y5,NZ,z5,FI5_2,180,180,.TRUE.)
c        fi1z=(Ucorr1/10.)*fi1z_1+(Ucorr2/10.)*fi1z_2
c	  fi2z_1=qd3vl(x1,y1,z1-dz,NX,x5,NY,y5,NZ,z5,FI5_1,180,180,.TRUE.)
c	  fi2z_2=qd3vl(x1,y1,z1-dz,NX,x5,NY,y5,NZ,z5,FI5_2,180,180,.TRUE.)
c        fi2z=(Ucorr1/10.)*fi2z_1+(Ucorr2/10.)*fi2z_2
c	  Ex5tmp=-(fi1x-fi2x)/(2.*dx)
c	  Ey5tmp=-(fi1y-fi2y)/(2.*dy)
c	  Ez5tmp=-(fi1z-fi2z)/(2.*dz)
c	  call rotate(-aUcf,Ex5tmp,Ey5tmp,Ex5tmp1,Ey5tmp1)
c	  Ez5tmp1=Ez5tmp
c	  call rotate(-bUcf,Ez5tmp1,Ex5tmp1,Ez5tmp,Ex5tmp)
c	  Ey5tmp=Ey5tmp1
c        call rotate(-teta_2blp,Ey5tmp,Ez5tmp,Ey5,Ez5)
c	  Ex5=Ex5tmp
c	else
c	  Ex5=0.
c	  Ey5=0.
c	  Ez5=0.
c	endif




C     поле октуполя


      Pot_oct(1)=U2blp
	Pot_oct(3)=Ucorr2
	Pot_oct(5)=0.
	Pot_oct(7)=Ucorr1

	Pot_oct(2)=(Pot_oct(1)+Pot_oct(3))/sqrt(2.)
	Pot_oct(4)=(Pot_oct(3)+Pot_oct(5))/sqrt(2.)
	Pot_oct(6)=(Pot_oct(5)+Pot_oct(7))/sqrt(2.)
	Pot_oct(8)=(Pot_oct(7)+Pot_oct(1))/sqrt(2.)


      E_Sum_X=0.
	E_Sum_Y=0.
	E_Sum_Z=0.

      do ll=1,8

      teta_add=0.+ 3.14159265/4.*(ll-1)

      call shift(xUcf,yUcf,zUcf-zsdvig_cor,x,y,z,x3,y3,z3)
	call rotate(teta_2blp+teta_add,y3,z3,y1,z1)
	x1=x3
	call rotate(bUcf,z1,x1,z2,x2)
	y2=y1
	call rotate(aUcf,x2,y2,x1,y1)
	z1=z2


      bo=x1.ge.x_oct(1).and.y1.ge.y_oct(1).and.z1.ge.z_oct(1).and.
     *   x1.le.x_oct(Nx_oct).and.
     *   y1.le.y_oct(NY_oct).and.
     *   z1.le.z_oct(NZ_oct)

      if (bo) then
c	  read (*,*)

        if (ll.eq.1) then
        call fglBegin(GL_POINTS)
         if (abs(x1).lt.15.) then
	      call fglColor3f(0.5,0.3,0.)
	   else
	      call fglColor3f(0.,0.,1.)
	   endif
	   if (y1.gt.7.5) then
	      call fglColor3f(1.,1.,1.)
	   endif
	   if (y1.lt.-7.5) then
	      call fglColor3f(1.,1.,0.)
	   endif
	   if (z1.lt.-7.5) then
	      call fglColor3f(0.5,0.5,0.5)
	   endif
	   if (z1.gt.7.5) then
	      call fglColor3f(0.2,0.9,0.4)
	   endif
	   call fglVertex2f (x,y+0.1)
	   call fglvertex2f (x,y-0.1)
	   call fglVertex2f (x+0.1,y)
	   call fglvertex2f (x-0.1,y)
 	   call fglEnd()
	  call fglFlush()
	  endif

	   if (abs(x1).lt.15..and.sqrt(y1**2+z1**2).gt.7.0) then
	      isCorrFlag=0
	   else
            isCorrFlag=1
	   endif


	  dx=0.15
	  dy=0.15
	  dz=0.15
	  fi1x=qd3vl(x1+dx,y1,z1,NX_oct,x_oct,NY_oct,y_oct,
     *          NZ_oct,z_oct,FI_oct,401,171,.TRUE.)*(Pot_oct(ll)/30.)
	  fi2x=qd3vl(x1-dx,y1,z1,NX_oct,x_oct,NY_oct,y_oct,
     *          NZ_oct,z_oct,FI_oct,401,171,.TRUE.)*(Pot_oct(ll)/30.)
	  fi1y=qd3vl(x1,y1+dy,z1,NX_oct,x_oct,NY_oct,y_oct,
     *          NZ_oct,z_oct,FI_oct,401,171,.TRUE.)*(Pot_oct(ll)/30.)
	  fi2y=qd3vl(x1,y1-dy,z1,NX_oct,x_oct,NY_oct,y_oct,
     *          NZ_oct,z_oct,FI_oct,401,171,.TRUE.)*(Pot_oct(ll)/30.)
	  fi1z=qd3vl(x1,y1,z1+dz,NX_oct,x_oct,NY_oct,y_oct,
     *          NZ_oct,z_oct,FI_oct,401,171,.TRUE.)*(Pot_oct(ll)/30.)
	  fi2z=qd3vl(x1,y1,z1-dz,NX_oct,x_oct,NY_oct,y_oct,
     *          NZ_oct,z_oct,FI_oct,401,171,.TRUE.)*(Pot_oct(ll)/30.)

	  ExOcttmp=-(fi1x-fi2x)/(2.*dx)
	  EyOcttmp=-(fi1y-fi2y)/(2.*dy)
	  EzOcttmp=-(fi1z-fi2z)/(2.*dz)
	  call rotate(-aUcf,ExOcttmp,EyOcttmp,Ex5tmp1,Ey5tmp1)
	  Ez5tmp1=EzOcttmp
	  call rotate(-bUcf,Ez5tmp1,Ex5tmp1,Ez5tmp,Ex5tmp)
	  Ey5tmp=Ey5tmp1
        call rotate(-(teta_2blp+teta_add),Ey5tmp,Ez5tmp,EyOct,EzOct)
	  ExOct=Ex5tmp
	else
	  ExOct=0.
	  EyOct=0.
	  EzOct=0.
	endif

	  E_Sum_X=E_Sum_X+ExOct
	  E_Sum_Y=E_Sum_Y+EyOct
	  E_Sum_Z=E_Sum_Z+EzOct


      enddo !конец цикла по пластинам



      Ex=Exf+Expl+Exbf+Exan+Exb1+E_Sum_X !+Ex2blp !+Ex5
	Ey=Eyf+Eypl+Eybf+Eyan+Eyb1+E_Sum_Y !+Ey2blp !+Ey5
	Ez=Ezf+Ezpl+Ezbf+Ezan+Ezb1+E_Sum_Z !+Ez2blp !+Ez5





      
   3    continue
C      Џа ў п з бвм га ў­Ґ­Ёп ¤ўЁ¦Ґ­Ёп
       F(1)=VX
       F(2)=VY
       F(3)=VZ
       F(4)=A1*(VY*HZ-VZ*HY) +qm*Ex
       F(5)=A1*(VZ*HX-VX*HZ) +qm*Ey
       F(6)=A1*(VX*HY-VY*HX) +qm*Ez
C      PRINT*,'RT12=',RT12
c      PRINT*,'HX=',HX
c      PRINT*,'HY=',HY
c      PRINT*,'HZ=',HZ
C      PRINT*,F(4),F(5),F(6)
       RETURN
       END
