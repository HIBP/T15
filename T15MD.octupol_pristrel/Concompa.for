        subroutine patrubok()
	use opengl
      use msfwin
	common H,A1,RTOK,RPL
C катушк
      call fglLineWidth (1)

      call fglBegin (GL_LINE_STRIP)
        call fglColor3f(0.,1.,0.)
	  call fglVertex2f(-60.,0.)
	  call fglVertex2f(-60.,83.)
	call fglEnd()

	call fglColor3f(0.,1.,0.)

      call Arc(55.5,25.,180.,-4.5,83.)
	call Arc(253.5469,12.,18.,-178.547,-11.339)
	call Arc(253.5469,-12.,-18.,-178.547,-11.339)


      call fglColor3f(0.,0.,1.)
	call Arc(83.4397,80.,180.,-6.37,92.43)
	call Arc(55.8397,80.,180.,-6.37,92.43)
      call Arc(121.6203,25.,80.,-13.,54.829)
	call Arc(94.0203,25.,80.,-13.,54.829)
      call Arc(278.3,-25.,25.,-155.,-11.386)
      call Arc(250.7,-25.,25.,-155.,-11.386)


      call fglFlush()
	return
      end


      subroutine Arc (R,angBeg,AngEnd,xc,yc)
      use opengl
      call fglBegin(GL_LINE_STRIP)
	do i=1,30
	  fi=(angbeg+(i-1)*(angEnd-angBeg)/29.)*(3.14159265/180.)
	  x=xc+R*cos(fi)
	  y=yc+R*sin(fi)
	  call fglVertex2f(x,y)
	enddo
	call fglEnd()
      return
	end


