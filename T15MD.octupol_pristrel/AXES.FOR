    	 subroutine axes (xmin,ymin,xmax,ymax,dx,dy)
	 use opengl
	 use msfwin
	 
	 real xmin,ymin,xmax,ymax,dx,dy

	 call fglColor3f(1.0,0.0,0.5)

	 call fglBegin(GL_LINE_STRIP)
        call fglVertex2f(xmin,0) 
	  call fglVertex2f(xmax,0)
       call fglEnd()

 	 call fglBegin(GL_LINE_STRIP)
        call fglVertex2f(0.,ymin) 
	  call fglVertex2f(0.,ymax)
       call fglEnd()

	 nx=int((xmax-xmin)/dx)
	 ny=int((ymax-ymin)/dx)
       do i= 1, nx
	        xtek=xmin+(i-1)*dx
	    call fglBegin (GL_LINES)
	        call fglVertex2f (xtek,-2.)
	        call fglVertex2f (xtek, 2.)
	    call fglEnd()
	 enddo

       do i= 1, ny
	        ytek=ymin+(i-1)*dy
	    call fglBegin (GL_LINES)
	        call fglVertex2f (-2.,ytek)
	        call fglVertex2f ( 2.,ytek)
	    call fglEnd()
	 enddo

	 call fglFlush()

	 return
	 end