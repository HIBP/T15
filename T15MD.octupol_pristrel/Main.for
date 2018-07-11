	PROGRAM main
	use opengl
      use msfwin

      COMMON H,A1,RTOK,RPL,IPO,IVIT,IOH,
     *XIT1(1,700),XIT2(1,2000),YIT1(1,900),YIT2(1,2000),
     *XIT3(1,700),YIT3(1,700),ZIT3(1,700),
     *IION2(10),IWYX(10),VO(10),X11(700),Z11(700),Y11(700),
     *X12(700),Y12(700),Z12(700),ALAM(10),IION1(10),X21(700)
c     *hzapx1(100),hzapy1(100),hzapz1(100),
c     *hzapx2(100),hzapy2(100),hzapz2(100)
      common/shotparameters/H0,CurTot,E,ugolA,xinj,yinj,xdet,ydet,dt_ext
	*                      ,zi_ext,beta_ext,alfa_ext
	common /kz/ kz
      common/griddata/xg(10000),yg(10000),zg(10000),
     *                inter(10000),Ebeam(10000),ugolAlfa(10000),Ng
	common/currents/ CurCSC,CurCSD,CurCSU,
     *                 CurPF1,CurPF2,CurPF3,CurPF4,CurPF5,CurPF6,
     *                 CurHFCD,CurHFCU 
      common/plate_voltages/Uplate1,Uplate2,Ufar,Ucorr1,Ucorr2
     *                    ,Ubf1,Ubf2,Energy,U2blp,currInt,ExtCurr,HcCurr
	*                    ,Ubeta1_1,Ubeta1_2
      common /ionoprovod/ xalfa,yalfa,zalfa,aalfa,balfa,
     *xbeta,ybeta,zbeta,abeta,bbeta,
     *xbeta1,ybeta1,zbeta1,abeta1,bbeta1,
     *xfar,yfar,zfar,afar,bfar,
	*xUcf,yUcf,zUcf,aUcf,bUcf,
	*x2blpc,y2blpc,z2blpc,a2blpc,b2blpc,
	*xanc,yanc,zanc,banc,
	*xi0,yi0,zi_entered,
     *beamshift,beta0,angle_of_injection(1) 



      integer(4)      ret
      character*12 sname
      real aold(10000),eold(10000),tmp(6)
	real  tccsu(10000),ccsu_arr(10000),
     *      tccsc(10000),ccsc_arr(10000),
     *      tccsd(10000),ccsd_arr(10000),
     *      tcpf1(10000),cpf1(10000),
     *      tcpf2(10000),cpf2(10000),
     *      tcpf3(10000),cpf3(10000),
     *      tcpf4(10000),cpf4(10000),
     *      tcpf5(10000),cpf5(10000),
     *      tcpf6(10000),cpf6(10000),
     *       time(10000),chfc(10000),tpl(10000),curpl(10000)

      integer iccsu,iccsc,iccsd,icpf1,icpf2,icpf3,icpf4,icpf5,icpf6


 	call openall 


      open (1,file='input.ini',STATUS='OLD')
c       print *, 'Enter H0 [Tesla]: '
	 read (1,*) H0
c	 print *, 'Enter total plasma current [kA] (negative): '
	 read (1,*) CurTot
c	 print*, 'enter Alfamax [deg]:'
	 read (1,*) alfamax
c	 print*, 'enter Alfamin [deg]:'
	 read (1,*) alfamin
c       print *, 'enter Nalfa [deg]:'
	 read (1,*)	NN_alfa
c	 print*, 'enter Emax [keV]:'
	 read (1,*) Emax
c	 print*, 'enter Emin [keV]:'
	 read (1,*) Emin
c       print *, 'enter NE:'
	 read (1,*)	NN_E
c       print *, 'Injector coordinates (xi, yi) [cm]:'
c	 read (1,*) xinj
c	 read (1,*) yinj
c       print *, 'Detector coordinates (xd, yd) [cm]:'
c	 read (1,*) xdet
c	 read (1,*) ydet
       read (1,*) zi_ext
	 read (1,*) alfa_ext
	 read (1,*) beta_ext
       read (1,*) kz
	 read (1,*) dt_ext
	 dt1=dt_ext
	close(1)
      print *, 'H0=',H0
      print *, 'AlfaMax=',alfamax
      print *, 'Alfamin=',alfamin
      print *, 'NN_alfa=',NN_Alfa
      print *, 'Emax=',Emax
      print *, 'Emin=',Emin
      print *, 'NN_E=',NN_E
	print *, 'alfa, beta = ',alfa_ext,beta_ext
	print *, '(xi,yi):', xinj,yinj
	print *, '(xd,yd):', xdet,ydet


      call primarybeamline
	 xinj=xi0
       yinj=yi0
       xdet=xanc !+100.
       ydet=yanc !+100.

c      open (11,file='ipf_scen.dat',status='old')
c      read(11,*)
c	read(11,*)
c	read(11,*)
c      ipf=0
c 11   continue
c      if (EOF(11)) goto 12
c	   ipf=ipf+1
c	   read(11,*) time(ipf),ccsu_arr(ipf),ccsc_arr(ipf),ccsd_arr(ipf),
c     *      cpf1(ipf),cpf2(ipf),cpf3(ipf),cpf4(ipf),cpf5(ipf),cpf6(ipf),
c     *      chfc(ipf)
c      goto 11
c 12   continue
c      close (11)


      open (11,file='icsu.dat',status='old')
	read(11,*)
      iccsu=0
 11   continue
      if (EOF(11)) goto 12
	   iccsu=iccsu+1
	   read(11,*) tccsu(iccsu),ccsu_arr(iccsu) !в килоамперах
      goto 11
 12   continue
      close (11)


      open (11,file='icsc.dat',status='old')
	read(11,*)
      iccsc=0
 13   continue
      if (EOF(11)) goto 14
	   iccsc=iccsc+1
	   read(11,*) tccsc(iccsc),ccsc_arr(iccsc) !в килоамперах
      goto 13
 14   continue
      close (11)

      open (11,file='icsl.dat',status='old')
	read(11,*)
      iccsd=0
 15   continue
      if (EOF(11)) goto 16
	   iccsd=iccsd+1
	   read(11,*) tccsd(iccsd),ccsd_arr(iccsd) !в килоамперах
      goto 15
 16   continue
      close (11)

      open (11,file='ipf1.dat',status='old')
	read(11,*)
      icpf1=0
 17   continue
      if (EOF(11)) goto 18
	   icpf1=icpf1+1
	   read(11,*) tcpf1(icpf1),cpf1(icpf1) !в килоамперах
      goto 17
 18   continue
      close (11)


      open (11,file='ipf2.dat',status='old')
	read(11,*)
      icpf2=0
 19   continue
      if (EOF(11)) goto 20
	   icpf2=icpf2+1
	   read(11,*) tcpf2(icpf2),cpf2(icpf2) !в килоамперах
      goto 19
 20   continue
      close (11)

      open (11,file='ipf3.dat',status='old')
	read(11,*)
      icpf3=0
 21   continue
      if (EOF(11)) goto 22
	   icpf3=icpf3+1
	   read(11,*) tcpf3(icpf3),cpf3(icpf3) !в килоамперах
      goto 21
 22   continue
      close (11)



      open (11,file='ipf4.dat',status='old')
	read(11,*)
      icpf4=0
 23   continue
      if (EOF(11)) goto 24
	   icpf4=icpf4+1
	   read(11,*) tcpf4(icpf4),cpf4(icpf4) !в килоамперах
      goto 23
 24   continue
      close (11)



      open (11,file='ipf5.dat',status='old')
	read(11,*)
      icpf5=0
 25   continue
      if (EOF(11)) goto 26
	   icpf5=icpf5+1
	   read(11,*) tcpf5(icpf5),cpf5(icpf5) !в килоамперах
      goto 25
 26   continue
      close (11)


      open (11,file='ipf6.dat',status='old')
	read(11,*)
      icpf6=0
 27   continue
      if (EOF(11)) goto 28
	   icpf6=icpf6+1
	   read(11,*) tcpf6(icpf6),cpf6(icpf6) !в килоамперах
      goto 27
 28   continue
      close (11)







c      open (11,file='plasma_scen.dat',status='old')
c      read(11,*)
c	read(11,*)
c	read(11,*)
c      iplasma=0
c 13   continue
c      if (EOF(11)) goto 14
c         iplasma=iplasma+1
c	   read(11,*) tpl(iplasma), curpl(iplasma)
c	goto 13
c 14   continue
c      close(11)



      t=1.1 !1.5
	!CurTot=-1000. !-500.
      print *, 'Ipl=',CurTot
      CurCSC=-qdval(t, iccsc, tccsc, ccsc_arr,.true.) *442 !kA*turns	 
      CurCSD=-qdval(t, iccsd, tccsd, ccsd_arr,.true.) *154 !kA*turns
	CurCSU=-qdval(t, iccsu, tccsu, ccsu_arr,.true.) *154 !kA*turns
	CurPF1=-qdval(t, icpf1, tcpf1, cpf1,.true.)	   *100 !kA*turns
	CurPF2=-qdval(t, icpf2, tcpf2, cpf2,.true.)	   *100 !kA*turns
	CurPF3=-qdval(t, icpf3, tcpf3, cpf3,.true.)	   *48 !kA*turns
	CurPF4=-qdval(t, icpf4, tcpf4, cpf4,.true.)	   *60 !kA*turns
	CurPF5=-qdval(t, icpf5, tcpf5, cpf5,.true.)	   *80 !kA*turns
	CurPF6=-qdval(t, icpf6, tcpf6, cpf6,.true.)	   *216 !kA*turns
	CurHFCD=0.
	CurHFCU=CurHFCD
c	read(*,*)
c      T=0.
c      tmp(1)=0.1
c	tmp(2)=0.1
c	tmp(3)=0.03
c	call fig(T,tmp,tmp)

c      GRAPHIC PARAMETERS
      XMIN=-50.
      YMIN=-140.
      ZMIN=-20.
      XMAX=400.
      YMAX=180.
      ZMAX=20.
      call fauxInitDisplayMode (IOR(AUX_SINGLE , AUX_RGB))
      call fauxInitPosition (0, 0, 1000, 500)
	ret = fauxInitWindow ("HIBP for T-15 MD tokamak"C)
 	call fglClearColor (0.0, 0.0, 0.0, 0.0)
      call fglClear(GL_COLOR_BUFFER_BIT)
      call fglColor3f(1.0, 1.0, 1.0)
      call fglMatrixMode (GL_PROJECTION)
      call fglLoadIdentity ()
      call fglOrtho(DBLE(xmin), DBLE(xmax), DBLE(ymin), DBLE(ymax),
     *              DBLE(zmin), DBLE(zmax))

c      kz=999
      
      NG=0.
      do i=1,NN_alfa
       if (NN_alfa.gt.1) then
	  Uplate2=alfamin+(i-1)*(alfamax-alfamin)/(NN_alfa-1)      
       else
	  Uplate2=alfamin
	 endif
      do j=1,NN_E
       if (NN_E.gt.1) then
	  E=Emin+(j-1)*(Emax-Emin)/(NN_E-1)      
       else
	  E=Emin
	 endif

	 Ufar=0.
       Uplate1=0.
       Ubeta1_1=0.
	 Ubeta1_2=0.
	 Ubf1=0.
	 Ubf2=0.
	 Uan=0.	!(E/2.) /2.97
	 U2blp=0.
	 Ucorr1=0.
	 Ucorr2=0.
       call OrcPass0() ! подбор Ufar alpha1
	 call OrcPass03() ! подбор Ubeta1_1	 beta1
c	 Ubf1=9. !6. !17.
	 CALL AXES (xmin,ymin,xmax,ymax,10.,10.)
       call OrcPass05(isOK)	! подбор бета2
	      call fglClear(GL_COLOR_BUFFER_BIT)

	 CALL AXES (xmin,ymin,xmax,ymax,10.,10.)
       if (isOK.eq.1) then
	 call orcpass1(x,y,z,vx,vy,vz,dt1,
     *              dt1/2.,dt1/2.,isOK)
	      call fglClear(GL_COLOR_BUFFER_BIT)  
	 CALL AXES (xmin,ymin,xmax,ymax,10.,10.)

c       Ubf1=0. !Ub(i)
       if (isOK.eq.1) then
         do k=1,7 !5
	      Uan=(E/2.) /2.97
            call orcpass2(u,x,y,z,vx,vy,vz,dt1,
     *              dt1/2.,dt1/2.,isOK)	!подбор Альфа3
	      if (isOK.eq.0) goto 283
c	      Uan=(E/2.) /2.97
            call orcpass3(u,x,y,z,vx,vy,vz,dt1,  !подбор Бета3
     *                 dt1/2.,dt1/2.,isOK)
	      if (isOK.eq.0) goto 283
c           U2blp=0. !U2(i)

c       Ucorr2=0. !Uc(i)
	      call fglClear(GL_COLOR_BUFFER_BIT)
	 CALL AXES (xmin,ymin,xmax,ymax,10.,10.)
         enddo
	   call orcpass2(u,x,y,z,vx,vy,vz,dt1,	   ! подбор Альфа3
     *              dt1/2.,dt1/2.,isOK)
	      if (isOK.eq.0) goto 283
	      call fglClear(GL_COLOR_BUFFER_BIT)
	 CALL AXES (xmin,ymin,xmax,ymax,10.,10.)

c	   Uan=0.
c         print *, dt1
c	   print *, alfa0,beta0
c	   read(*,*)
	   call orcpass()  ! финальный расчёт
       endif

       if (isOK.eq.1) then
          open (12,file='output\voltages.dat'
     *            ,status='unknown',access='append')
           write (12,132) E,Ufar,Ubeta1_1-Ubeta1_2,Ubf1-Ubf2
	*           ,Uplate2-Uplate1,Ucorr1-Ucorr2,U2blp
	    close(12)
	 endif
 132      Format (7F20.10)
 283      CONTINUE

	 endif
	 call fglClear(GL_COLOR_BUFFER_BIT)
      enddo
	enddo

 
      open (20,file='output\griddata.dat',STATUS='OLD')
      NG=0 
 1    CONTINUE
       if (EOF(20)) goto 2
       NG=NG+1
       read(20,769)xg(Ng),yg(Ng),zg(NG),Ebeam(Ng),ugolalfa(Ng),inter(NG) 
       goto 1
 2    CONTINUE
      close(20) 
 769  FORMAT (5F15.5,3X,I1)



 
 
        kz=999
        sname='i1aeq000.dat'
      do i=1,NG
        if (inter(i).eq.1) then
          if (isExists(ugolalfa(i),aold,999-kz).eq.0) then
             write(sname(6:8),'(1I3)') kz
		   aold(1000-kz)=ugolalfa(i)          
             kz=kz-1
          else
	       write(sname(6:8),'(1I3)') 
     *            1000-isExists(ugolalfa(i),aold,999-kz)
		endif
	    open(1,file=sname,ACCESS='APPEND')
          write(1,*) '(',xg(i)*10.,yg(i)*10.,zg(i)*10.,')'
	    close(1)
        endif
	enddo

        kz=999
        sname='i0aeq000.dat'
      do i=1,NG
        if (inter(i).eq.0) then
          if (isExists(ugolalfa(i),aold,999-kz).eq.0) then
             write(sname(6:8),'(1I3)') kz
		   aold(1000-kz)=ugolalfa(i)          
             kz=kz-1
          else
	       write(sname(6:8),'(1I3)') 
     *            1000-isExists(ugolalfa(i),aold,999-kz)
		endif
	    open(1,file=sname,ACCESS='APPEND')
          write(1,*) '(',xg(i)*10.,yg(i)*10.,zg(i)*10.,')'
	    close(1)
        endif
	enddo


        kz=999
        sname='i1eeq000.dat'
      do i=1,NG
        if (inter(i).eq.1) then
          if (isExists(Ebeam(i),Eold,999-kz).eq.0) then
             write(sname(6:8),'(1I3)') kz
		   Eold(1000-kz)=Ebeam(i)          
             kz=kz-1
          else
	       write(sname(6:8),'(1I3)') 
     *            1000-isExists(Ebeam(i),Eold,999-kz)
		endif
	    open(1,file=sname,ACCESS='APPEND')
          write(1,*) '(',xg(i)*10.,yg(i)*10.,zg(i)*10.,')'
	    close(1)
        endif
	enddo

        kz=999
        sname='i0eeq000.dat'
      do i=1,NG
        if (inter(i).eq.0) then
          if (isExists(Ebeam(i),Eold,999-kz).eq.0) then
             write(sname(6:8),'(1I3)') kz
		   Eold(1000-kz)=Ebeam(i)          
             kz=kz-1
          else
	       write(sname(6:8),'(1I3)') 
     *            1000-isExists(Ebeam(i),Eold,999-kz)
		endif
	    open(1,file=sname,ACCESS='APPEND')
          write(1,*) '(',xg(i)*10.,yg(i)*10.,zg(i)*10.,')'
	    close(1)
        endif
	enddo

 	stop
	end

      function isExists (x,xarr,N)
	real xarr(10000)

	if (N.eq.0) then
	   isExists=0
	   return
	endif

      do i=1,N
         if (x.eq.xarr(i)) then
	      isExists=i
	      return
	   endif
	enddo
     
      isExists=0

	return
	end




	subroutine openall
      COMMON H,A1,RTOK,RPL
      COMMON /WIGU/RE(200),ZE(200),WIE(200),
     *RP(10000),ZP(10000),WIP(10000),HBRS,HBZS
      COMMON/HTAT/HTAR(500,500),HTAZ(500,500)
      common/setka/xpse(500),ypse(500)
	common/plasma/xpl(51),ypl(51),BplR(51,51),BplZ(51,51)
	COMMON/Hmain/xfm(78),yfm(82),zfm(39),
     *  HxFM(78,82,39),HyFM(78,82,39),HzFM(78,82,39),
	*  indxmax,indymax,indzmax
	COMMON/PARAM/psialfa,drk,ro,N,M,DI
      common/shotparameters/H0,CurTot,E,ugolA,xinj,yinj,xdet,ydet,dt_ext
	*                      ,zi_ext,beta_ext,alfa_ext
      COMMON /NNN/NCO1,NCO2,NCO3,NCHO,NCOR,NCOZ,NccR,NccZ
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
	common /p5v/x5bf(820),z5bf(500)
	*             ,FIUcf1(820,500),FIUcf2(820,500),qm
      common/faradey/ xf(1000),yf(1000),fif(1000,1000),nxf,nyf
c      common/octupol/ x2blp(1000),y2blp(1000),fi2blp(1000,1000)
c	*               ,nx2blp,ny2blp
	common/plates/x1pl(1000),y1pl(1000)
	*             ,FIpl1(1000,1000),FIpl2(1000,1000),nxpl,nypl
     
      common/oct/ x_oct(401),y_oct(171),z_oct(171),fi_oct(401,171,171),
     *            Nx_oct,Ny_oct,Nz_oct 
	
	common/betafld/x1bf(820),z1bf(500)
	*             ,FIbf1(820,500),FIbf2(820,500),nxbf,nzbf 
      common /kach/ xa,ya,plang
c	common /beta3/X5(180),Y5(180),Z5(180)
c	*           ,FI5_1(180,180,180),FI5_2(180,180,180),NX,NY,NZ
      common /ionoprovod/ xalfa,yalfa,zalfa,aalfa,balfa,
     *xbeta,ybeta,zbeta,abeta,bbeta,
     *xbeta1,ybeta1,zbeta1,abeta1,bbeta1,
     *xfar,yfar,zfar,afar,bfar,
	*xUcf,yUcf,zUcf,aUcf,bUcf,
	*x2blpc,y2blpc,z2blpc,a2blpc,b2blpc,
	*xanc,yanc,zanc,banc,
	*xi0,yi0,zi_entered,
     *beamshift,beta0,angle_of_injection(1) 
	common /anal/ Xan(2000),Yan(2000),FIan(2000,2000)
     *            ,NXan,NYan,xd,yd,Uan,analangle



      REAL DU(6)

      RTOK=155.
      RPL=49.


cc следующие файлы должны быть подсчитаны отдельно
c      open (10,file='p5v_0_10.dat',FORM='BINARY',STATUS='OLD') !z<0
c	open (11,file='p5v_10_0.dat',FORM='BINARY',STATUS='OLD') !z>0

c	read (10) Nx,Ny,Nz
c	read (11) Nx,Ny,Nz
cC Понятное дело, что это считывать не нужно, но надо ...
c      read (10) x5min,x5max
c      read (10) y5min,y5max
c      read (10) z5min,z5max
c      read (11) x5min,x5max
c      read (11) y5min,y5max
c      read (11) z5min,z5max

c	do i=1,Nx
c	   x5(i)=x5min+(i-1)*(x5max-x5min)/(Nx-1)
c	enddo
c	do j=1,Ny
c	   y5(j)=y5min+(j-1)*(y5max-y5min)/(Ny-1)
c	enddo
c	do k=1,Nz
c	   z5(k)=z5min+(k-1)*(z5max-z5min)/(Nz-1)
c	enddo

c	do i=1,Nx
c	do j=1,Ny
c	do k=1,Nz
c	  read (10) FI5_1(i,j,k) 
c        read (11) FI5_2(i,j,k)
c	enddo
c	enddo
c	enddo
c	close(10)
c	close(11)



c следующие файлы должны быть подсчитаны отдельно
      open (10,file='BF_0_1.dat',FORM='BINARY',STATUS='OLD') !z пластины >0
	open (11,file='BF_1_0.dat',FORM='BINARY',STATUS='OLD') !z пластины <0 (что является особенностью данной версии)

	read (10) Nxbf,Nzbf
	read (11) Nxbf,Nzbf
C Понятное дело, что не важно откуда читать

	do i=1,Nxbf
	do j=1,Nzbf
	  read (10) x1bf(i),z1bf(j),FIbf1(i,j) 
        read (11) x1bf(i),z1bf(j),FIbf2(i,j)
	enddo
	enddo
	close(10)
	close(11)



      open (7, FILE='potbin.dat',FORM='BINARY',STATUS='OLD')

      read (7) NXF,NYF
	do i=1,NXF
	do j=1,NYF
	   read (7) xf(i),yf(j),fif(i,j)
	enddo
	enddo
	close(7)


c      open (7, FILE='2BLP_3_0.dat',FORM='BINARY',STATUS='OLD')

c      read (7) Nx2blp,Ny2blp
c	do i=1,Nx2blp
c	do j=1,Ny2blp
c	   read (7) x2blp(i),y2blp(j),fi2blp(i,j)
c	enddo
c	enddo
c	close(7)



      open (7, FILE='up_G_plate_30.dat',FORM='BINARY',STATUS='OLD')
      read(7) Nz_oct,Ny_oct,Nx_oct
	print *, Nx_oct,Ny_oct,Nz_oct
	read(7) Zmin_oct,Zmax_oct
	print *,Zmin_oct,Zmax_oct
	 do iii=1,Nz_oct
          z_oct(iii)=Zmin_oct+(iii-1)*(Zmax_oct-Zmin_oct)/(Nz_oct-1)
	 enddo
	read(7) Ymin_oct,Ymax_oct
	print *,Ymin_oct,Ymax_oct
	 do iii=1,Ny_oct
          y_oct(iii)=Ymin_oct+(iii-1)*(Ymax_oct-Ymin_oct)/(Ny_oct-1)
	 enddo
	read(7) Xmin_oct,Xmax_oct
	print *,Xmin_oct,Xmax_oct
	 do iii=1,Nx_oct
          x_oct(iii)=Xmin_oct+(iii-1)*(Xmax_oct-Xmin_oct)/(Nx_oct-1)
	 enddo
	do kkk=1,Nz_oct
	do jjj=1,Ny_oct
	do iii=1,Nx_oct
        read (7) fi_oct(iii,jjj,kkk)
	enddo
	enddo
	enddo

	print*, 'octupol is ready'

	close(7)


      open (7, FILE='fi_anal.dat',FORM='BINARY',STATUS='OLD')

      read (7) NXan,NYan
	do i=1,NXan
	do j=1,NYan
	   read (7) xan(i),yan(j),FIan(i,j)
	enddo
	enddo
	close(7)

c следующие файлы должны быть подсчитаны отдельно
      open (10,file='FI_0_3.dat',FORM='BINARY',STATUS='OLD') !верхняя (нетипично)
	open (11,file='FI_3_0.dat',FORM='BINARY',STATUS='OLD') !нижняя (тоже нетипично)

	read (10) Nxpl,Nypl
	read (11) Nxpl,Nypl
C Понятное дело, что не важно откуда читать

	do i=1,Nxpl
	do j=1,Nypl
	  read (10) x1pl(i),y1pl(j),FIpl1(i,j) 
        read (11) x1pl(i),y1pl(j),FIpl2(i,j)
	enddo
	enddo
	close(10)
	close(11)





c следующий файл должен быть подсчитан отдельно
      open (12,file='CSC.dat',FORM='BINARY',STATUS='OLD')
      read (12) iCSC, jCSC
	do i=1,iCSC
	do j=1,jCSC
        read (12)	xCSC(i),yCSC(j),HxCSC(i,j),HyCSC(i,j)
	enddo
	enddo
      close(12)

c следующий файл должен быть подсчитан отдельно
      open (12,file='CSD.dat',FORM='BINARY',STATUS='OLD')
      read (12) iCSD, jCSD
	do i=1,iCSD
	do j=1,jCSD
        read (12)	xCSD(i),yCSD(j),HxCSD(i,j),HyCSD(i,j)
	enddo
	enddo
      close(12)

c следующий файл должен быть подсчитан отдельно
      open (12,file='CSU.dat',FORM='BINARY',STATUS='OLD')
      read (12) iCSU, jCSU
	do i=1,iCSU
	do j=1,jCSU
        read (12)	xCSU(i),yCSU(j),HxCSU(i,j),HyCSU(i,j)
	enddo
	enddo
      close(12)

c следующий файл должен быть подсчитан отдельно
      open (12,file='hfcd.dat',FORM='BINARY',STATUS='OLD')
      read (12) iHFCD, jHFCD
	do i=1,iHFCD
	do j=1,jHFCD
        read (12)	xHFCD(i),yHFCD(j),HxHFCD(i,j),HyHFCD(i,j)
	enddo
	enddo
      close(12)

c следующий файл должен быть подсчитан отдельно
      open (12,file='HFCU.dat',FORM='BINARY',STATUS='OLD')
      read (12) iHFCU, jHFCU
	do i=1,iHFCU
	do j=1,jHFCU
        read (12)	xHFCU(i),yHFCU(j),HxHFCU(i,j),HyHFCU(i,j)
	enddo
	enddo
      close(12)

c следующий файл должен быть подсчитан отдельно
      open (12,file='PF1.dat',FORM='BINARY',STATUS='OLD')
      read (12) iPF1, jPF1
	do i=1,iPF1
	do j=1,jPF1
        read (12)	xPF1(i),yPF1(j),HxPF1(i,j),HyPF1(i,j)
	enddo
	enddo
      close(12)

c следующий файл должен быть подсчитан отдельно
      open (12,file='PF2.dat',FORM='BINARY',STATUS='OLD')
      read (12) iPF2, jPF2
	do i=1,iPF2
	do j=1,jPF2
        read (12)	xPF2(i),yPF2(j),HxPF2(i,j),HyPF2(i,j)
	enddo
	enddo
      close(12)

c следующий файл должен быть подсчитан отдельно
      open (12,file='PF3.dat',FORM='BINARY',STATUS='OLD')
      read (12) iPF3, jPF3
	do i=1,iPF3
	do j=1,jPF3
        read (12)	xPF3(i),yPF3(j),HxPF3(i,j),HyPF3(i,j)
	enddo
	enddo
      close(12)

c следующий файл должен быть подсчитан отдельно
      open (12,file='PF4.dat',FORM='BINARY',STATUS='OLD')
      read (12) iPF4, jPF4
	do i=1,iPF4
	do j=1,jPF4
        read (12)	xPF4(i),yPF4(j),HxPF4(i,j),HyPF4(i,j)
	enddo
	enddo
      close(12)

c следующий файл должен быть подсчитан отдельно
      open (12,file='PF5.dat',FORM='BINARY',STATUS='OLD')
      read (12) iPF5, jPF5
	do i=1,iPF5
	do j=1,jPF5
        read (12)	xPF5(i),yPF5(j),HxPF5(i,j),HyPF5(i,j)
	enddo
	enddo
      close(12)

c следующий файл должен быть подсчитан отдельно
      open (12,file='PF6.dat',FORM='BINARY',STATUS='OLD')
      read (12) iPF6, jPF6
	do i=1,iPF6
	do j=1,jPF6
        read (12)	xPF6(i),yPF6(j),HxPF6(i,j),HyPF6(i,j)
	enddo
	enddo
      close(12)



C*************************************************************
C     FIELD NETWORK DIMENSION
C     change values in common-block
C      NNETR=25
c^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      nnetr=100
      NNETZ=120
	indxmax=78
	indymax=82
	indzmax=39
C Удерживающее поле
        open(62,FILE='Mntfld.dat',FORM='BINARY',STATUS='OLD')
	  call grid()
        READ(62) (((HxFM(i,j,k),HyFM(i,j,k),HzFM(i,j,k),
     *       k=1,indzmax),j=1,indymax),i=1,indxmax)

       CLOSE(62)


c^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
      NNET=NNETR*NNETZ
      ncor=nnetr
      ncoz=nnetz
C*************************************************************

 

 
       OPEN(15,FILE='DATA.DAT',STATUS='OLD')
       OPEN(16,FILE='FIELDout.DAT',STATUS='OLD',FORM='BINARY',ERR=33)
C Если поле посчитано, то считываем поле плазмы
       
       READ(16) ((xpse(i),ypse(k),HTAR(I,K),HTAZ(I,K),
     *k=1,NNETZ),i=1,NNETR)
      
C       write(*,404) ((xpse(i),ypse(k),HTAR(I,K),HTAZ(I,K),
C     *k=1,NNETZ),i=1,NNETR)
       IFOP=1
  33   NNRZ=0
C Если поля плазмы нет, то оно считается
c       DO 401 ICC=1,NVVI
c       READ(15,*) NNR,NNZ
c       READ(15,400) (RE(K),ZE(K),WIE(K),K=1+NNRZ,NNRZ+NNR*NNZ)
c 401   NNRZ=NNRZ+NNR*NNZ
       NCO1=NNRZ

C       IF (NCO1.GT.200) STOP
C       READ(15,*) CONTRO
C       IF(CONTRO.NE.'PLASMA') THEN
C       PRINT*,'DATA ERROR'
C       STOP
C       END IF

C  PLASMA CURRENT DISTRIBUTION
       OPEN(16,FILE='data2.DAT',STATUS='OLD',FORM='BINARY')

       read(16) nnetr,nnetz

	do i=1,nnetr
	do k=1,nnetz
        READ(16) xpse(i),ypse(k),HTAR(I,K),HTAZ(I,K)
	enddo
	enddo
      

       CLOSE(16)

      ncor=nnetr
      ncoz=nnetz


 400  FORMAT(3G15.6)


	return
	end


	subroutine grid ()
      COMMON/Hmain/xfm(78),yfm(82),zfm(39),
     *  HxFM(78,82,39),HyFM(78,82,39),HzFM(78,82,39),
	*  indxmax,indymax,indzmax

      xstep=290./77.
	ystep=570./81.
	zstep=100./38.
      do i=1,78
         xfm(i)=- 90.+(i-1)*xstep
      enddo
	do j=1,82
	   yfm(j)=-350.+(j-1)*ystep
	enddo
      do k=1,39
	   zfm(k)=- 50.+(k-1)*zstep 
	enddo


	return
	end



      subroutine primarybeamline ()
      common /ionoprovod/ xalfa,yalfa,zalfa,aalfa,balfa,
     *xbeta,ybeta,zbeta,abeta,bbeta,
     *xbeta1,ybeta1,zbeta1,abeta1,bbeta1,
     *xfar,yfar,zfar,afar,bfar,
	*xUcf,yUcf,zUcf,aUcf,bUcf,
	*x2blpc,y2blpc,z2blpc,a2blpc,b2blpc,
	*xanc,yanc,zanc,banc,
	*xi0,yi0,zi_entered,
     *beamshift,beta0,angle_of_injection(1) 
	common /anal/ Xan(2000),Yan(2000),FIan(2000,2000)
     *            ,NXan,NYan,xdconst,ydconst,Uan,analangle
      common/shotparameters/H0,CurTot,E,ugolA,xinj,yinj,xdet,ydet,dt_ext
	*                      ,zi_ext,beta_ext,alfa_ext
	common /zsdvig/ zsdvig
	common/aim/ xaim,yaim,zaim
	common/ysdvig_/ysdvig_,ysdvig_2blp
	common/zd_known_/zd_known_
	common/angulo_de_caretxa/ angulo_de_caretxa_a,angulo_de_caretxa_b
	common/teta_2blp/teta_2blp,teta_an
	common/y_2blp_shift/ y_2blp_shift,y_beta3_shift
	common/cef/cef1,cef2
      common/plate_voltages/Uplate1,Uplate2,Ufar,Ucorr1,Ucorr2
     *                    ,Ubf1,Ubf2,Ener,U2blp,currInt,ExtCurr,HcCurr
	*                    ,Ubeta1_1,Ubeta1_2
	common/Ucorr_start/Ucorr1_start,Ucorr2_start



      real tmp(6)

      angresh = 0.0 !-2.7 !-1.3 !-0.4 !-0.6 !0.3 !-1.5
	bngresh = 0.0 !8. !18. !7.

       beta0 = beta_ext !0 + bngresh  


      NumerodeAnalisador = 1 !2

      xpatr = 70.14 !63.06 !77.21 !     
	ypatr = 105.04 !115.53 !94.55 !    ! координаты центра входного патрубка (точка I1)
      zpatr = 0.0 !+ 1. -8.

	alfa = (alfa_ext + angresh)*(3.14159265/180.) ! альфа-угол патрубка
      beta = (beta_ext + bngresh)*(3.14159265/180.) ! бета-угол бимлайна	 !-20.571 -12.


c ------------------
      dist=0. !30.6 ! от патрубка до кардана	!0, т.к. на кардане всё равно нет изменения угла

	xkard=xpatr+dist*cos(alfa)*cos(beta)
	ykard=ypatr+dist*sin(alfa)
	zkard=zpatr-dist*cos(alfa)*sin(beta)  ! координаты кардана

      print *, 'kard: ', xkard,ykard,zkard
c	read(*,*)
c ------------------      



c ------------------
	dist=30. ! от кардана до пластин альфа-качалка

	xalfa=xkard+dist*cos(alfa)*cos(beta) 
	yalfa=ykard+dist*sin(alfa)
	zalfa=zkard-dist*cos(alfa)*sin(beta)  ! координаты качалки
      aalfa=alfa
	balfa=beta

      print *, 'alfa: ', xalfa,yalfa,zalfa
c	read(*,*)
c ------------------      


c ------------------
	dist=18.0 ! от пластин альфа-качалка до вторых бета-пластин

	xbeta=xalfa+dist*cos(alfa)*cos(beta)
	ybeta=yalfa+dist*sin(alfa)
	zbeta=zalfa-dist*cos(alfa)*sin(beta)  ! координаты вторых бета-пластин
	abeta=alfa
	bbeta=beta

      print *, 'beta2: ', xbeta,ybeta,zbeta
c	read(*,*)
c ------------------      

c ------------------
	dist=20. !39.0 ! от вторых бета-пластин до первых бета-пластин

	xbeta1=xbeta+dist*cos(alfa)*cos(beta)
	ybeta1=ybeta+dist*sin(alfa)
	zbeta1=zbeta-dist*cos(alfa)*sin(beta)  ! координаты первых бета-пластин
      abeta1=alfa
	bbeta1=beta

      print *, 'beta1: ', xbeta1,ybeta1,zbeta1
c	read(*,*)
c ------------------      

c ------------------
	dist=15. !18.0 ! от первых бета-пластин до пластин альфа-фарадея

	xfar=xbeta1+dist*cos(alfa)*cos(beta)
	yfar=ybeta1+dist*sin(alfa)
	zfar=zbeta1-dist*cos(alfa)*sin(beta)  ! координаты пластин альфа-фарадея
      afar=alfa
	bfar=beta

      print *, 'far: ', xfar,yfar,zfar
c	read(*,*)
c ------------------      

c ------------------
	dist=20. ! от пластин альфа-фарадея до первой точки траектории



      angresh1=0. !1.5 

      angle_of_injection(1)= 56.   -angresh -angresh1 ! 14.8 
      alfa1=alfa+angresh1*(3.14159265/180.)


	xi0=xfar+dist*cos(alfa1)*cos(beta)
	yi0=yfar+dist*sin(alfa1)
	zi_entered=zfar-dist*cos(alfa1)*sin(beta)  ! координаты первой точки траектории 

      print *, 'Inj: ', xi0,yi0,zi_entered
c	print *, sqrt((xalfa-xi0)**2+(yalfa-yi0)**2+(zalfa-zi_entered)**2)
c	read(*,*)
c ------------------      

      beamshift = 0.



c  Secondary beamline



      if (NumerodeAnalisador.eq.1) then
        ysdvig_ = -0.		  ! -7   -17.3
	else
        ysdvig_ = -17.3
	endif

      xpatr= 108.87 !143.6   !-1.8 
	ypatr= -20.64 +ysdvig_  ! координаты центра выходного патрубка (точка I2)	5.13
      zpatr=0.0 !0.5

	alfa=10.*(3.14159265/180.) ! альфа-угол патрубка  40.


      beta=0.*(3.14159265/180.) ! бета-угол патрубка
c ------------------
      dist=0. ! от патрубка до 15.5-градусного патрубка

	xpatr=xpatr+dist*cos(alfa)*cos(beta)
	ypatr=ypatr+dist*sin(alfa)
	zpatr=zpatr-dist*cos(alfa)*sin(beta)  ! координаты начала 15.5-градусного направления

      print *, 'patr: ', xpatr,ypatr,zpatr
c	read(*,*)
c ------------------      




      beta= 0.*(3.14159265/180.) ! бета-угол бимлайна  
c ------------------
      dist=0. ! от 15.5-градусного патрубка до кардана

	xkard=xpatr+dist*cos(alfa)*cos(beta)
	ykard=ypatr+dist*sin(alfa)
	zkard=zpatr-dist*cos(alfa)*sin(beta)  ! координаты кардана

      print *, 'kard: ', xkard,ykard,zkard			  
c	read(*,*)
c ------------------      


      beta= 0.*(3.14159265/180.) ! бета-угол бимлайна	30 20
c ------------------



      dist=10. !27. !0. !5.7 ! от кардана до точки пристрелки

	xaim=xkard+dist*cos(alfa)*cos(beta)
	yaim=ykard+dist*sin(alfa)
	zaim=zkard-dist*cos(alfa)*sin(beta) 


      print*, 'aim: ', xaim,yaim,zaim
c	read (*,*)



      dist=37. !10. !10.7 ! от кардана до альфа-пластин вторичного бимлайна

      zsdvig=0. !-2.
      ysdvig_2blp= 0. !сдвиг центра альфа-пластин вторичного бимлайна относительно точки пристрелки 0. 0.

	x2blpc=xkard+dist*cos(alfa)*cos(beta) 
	y2blpc=ykard+dist*sin(alfa)	+ ysdvig_2blp
	z2blpc=zkard-dist*cos(alfa)*sin(beta)  ! координаты Ucorr
      a2blpc=alfa
	b2blpc=beta
	teta_2blp= 0. !20.*(3.14159265/180.)

      print *, 'u2blpc: ', x2blpc,y2blpc,z2blpc
c	read(*,*)
c ------------------      

c ------------------
      dist= 10. !27. !12. ! от альфа-пластин вторичного бимлайна до корректирующих пластин


      if (NumerodeAnalisador.eq.1) then
	  alfa=10.*(3.14159265/180.) ! альфа-угол после 2blp    20. -3.  20 
        beta= 0.*(3.14159265/180.)	! beta-угол после 2blp  30. 0.  12. 15.5 20.
        y_2blp_shift=0. !компенсация несовпадения осей вращения на -20 градусов в чертеже и расчёте
	else
	  alfa= -3.*(3.14159265/180.) ! альфа-угол после 2blp    0. -3.
        beta= 0.0*(3.14159265/180.)	! beta-угол после 2blp    12. 15.5
        y_2blp_shift=-0.308 !компенсация несовпадения осей вращения на -20 градусов в чертеже и расчёте
	endif
      angulo_de_caretxa_a=alfa


	xUcf=xkard+dist*cos(alfa)*cos(beta)
	yUcf=ykard+dist*sin(alfa)
	zUcf=zkard-dist*cos(alfa)*sin(beta)  ! координаты of octupol centre
      aUcf=alfa
	bUcf=beta

      if (NumerodeAnalisador.eq.1) then
        zsdvig_cor= 0. !  4. 0.
	  cef1=1.
	  cef2=0.
	  Ucorr1_start=-0.
	  Ucorr2_start=-0.
	else
        zsdvig_cor= 0.
	  cef1=0.
	  cef2=1.
	  Ucorr1_start=-4.
	  Ucorr2_start=-4.
	endif


      print *, 'Ucf: ', xUcf,yUcf,zUcf

c	tmp(1)=xUcf
c	tmp(2)=yUcf
c	tmp(3)=zUcf

c                call FIG(0,tmp,tmp)


c	read(*,*)
c ------------------      

c ------------------
      dist=87. !60. !63 !81.5 ! от корректирующих пластин до щели анализатора

      if (NumerodeAnalisador.eq.1) then
	  beta= 0.*(3.14159265/180.)	!бета-угол после коррекции 4 23	
        zsdvig_beamline = 0.
	  teta_an= 0.*(3.14159265/180.)
	  y_beta3_shift=0. !сдвиг центра корректирующих пластин
	else
	  beta= 0.*(3.14159265/180.)	!бета-угол после коррекции 4 23
	  zsdvig_beamline = 0.
	  teta_an= 0. !15.*(3.14159265/180.)
	  y_beta3_shift=-4.786 !сдвиг центра корректирующих пластин
      endif
      angulo_de_caretxa_b=beta
        xtmp=xUcf+zsdvig_beamline*sin(beta)
	  ytmp=yUcf
	  ztmp=zUcf+zsdvig_beamline*cos(beta)




	xanc=xtmp+dist*cos(alfa)*cos(beta)
	yanc=ytmp+dist*sin(alfa)
	zanc=ztmp-dist*cos(alfa)*sin(beta)  ! координаты щели анализатора
	banc=beta


c	tmp(1)=xanc
c	tmp(2)=yanc
c	tmp(3)=zanc

c	                call FIG(0,tmp,tmp)



	analangle=-120. + alfa*(180./3.14159265)




      print *, 'Analyser: ', xanc,yanc,zanc
c ------------------      




      dist=37.82 !от щели до детектора в плоскости xz
      dist2=26.3 !от оси анализатора до щели по перпендикуляру

	zd_known_=zanc -dist*cos(alfa)*sin(beta) -
     -                           dist2*sin(teta_an)*cos(beta)
	print*, 'zd:', zd_known_

c	read(*,*)


      return
      end



      subroutine rotate (angle, x1, y1, x2, y2)
C Преобразование координат:
C поворот на угол angle
C x1,y1 - input, x2,y2 -output

      x2= x1*cos(angle)+y1*sin(angle)
	y2=-x1*sin(angle)+y1*cos(angle)

      return
	end


      subroutine shift (x0,y0,z0,x1,y1,z1,x2,y2,z2)
C Преобразование координат:
C перенос центра координат в точку x0,y0,z0
C x1,y1,z1 - input, x2,y2,z2 -output

      x2=x1-x0
	y2=y1-y0
	z2=z1-z0

	return
	end





