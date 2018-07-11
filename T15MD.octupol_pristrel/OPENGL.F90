! **************************************************************************
!  This file is not guaranteed by Microsoft to be error free. Every effort
!  has been made to ensure proper data-types and declarations, but no
!  testing has been performed using this include file. Additionally, some
!  API's though listed, may not be compatible with the Microsoft Fortran
!  language.
! **************************************************************************
module opengl

use openglty

!ms$objcomment lib:"opengl32.lib"
!ms$objcomment lib:"glaux.lib"
!ms$objcomment lib:"glu32.lib"

!                           ****OPENGL*****
!                           **** FGL ****        

interface
subroutine  fglAccum  (op ,value ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glAccum@8' :: fglAccum
integer*4 op 
real*4 value 
end subroutine fglAccum
end interface

interface
subroutine  fglAlphaFunc  (func ,ref ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glAlphaFunc@8' :: fglAlphaFunc
integer*4 func 
real*4 ref 
end subroutine fglAlphaFunc
end interface

interface
subroutine  fglBegin  (mode ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glBegin@4' :: fglBegin
integer*4 mode 
end subroutine fglBegin
end interface

interface
subroutine  fglBitmap  (width ,height ,xorig ,yorig ,xmove ,ymove ,bitmap ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glBitmap@28' :: fglBitmap
integer*4 width 
integer*4 height 
real*4 xorig 
real*4 yorig 
real*4 xmove 
real*4 ymove 
integer*4   bitmap 
end subroutine fglBitmap
end interface

interface
subroutine  fglBlendFunc  (sfactor ,dfactor ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glBlendFunc@8' :: fglBlendFunc
integer*4 sfactor 
integer*4 dfactor 
end subroutine fglBlendFunc
end interface

interface
subroutine  fglCallList  (list ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glCallList@4' :: fglCallList
integer*4 list 
end subroutine fglCallList
end interface

interface
subroutine  fglCallLists  (n ,type ,lists ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glCallLists@12' :: fglCallLists
integer*4 n 
integer*4 type 
integer*4   lists 
end subroutine fglCallLists
end interface

interface
subroutine  fglClear  (mask ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glClear@4' :: fglClear
integer*4 mask 
end subroutine fglClear
end interface

interface
subroutine  fglClearAccum  (red ,green ,blue ,alpha ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glClearAccum@16' :: fglClearAccum
real*4 red 
real*4 green 
real*4 blue 
real*4 alpha 
end subroutine fglClearAccum
end interface

interface
subroutine  fglClearColor  (red ,green ,blue ,alpha ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glClearColor@16' :: fglClearColor
real*4 red 
real*4 green 
real*4 blue 
real*4 alpha 
end subroutine fglClearColor
end interface

interface
subroutine  fglClearDepth  (depth ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glClearDepth@8' :: fglClearDepth
real*8 depth 
end subroutine fglClearDepth
end interface

interface
subroutine  fglClearIndex  (c ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glClearIndex@4' :: fglClearIndex
real*4 c 
end subroutine fglClearIndex
end interface

interface
subroutine  fglClearStencil  (s ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glClearStencil@4' :: fglClearStencil
integer*4 s 
end subroutine fglClearStencil
end interface

interface
subroutine  fglClipPlane  (plane , equation ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glClipPlane@8' :: fglClipPlane
integer*4 plane 
integer*4 equation 
end subroutine fglClipPlane
end interface

interface
subroutine fglColor3b ( red, green, blue)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glColor3b@12' :: fglColor3b
integer(4)  red
integer(4)  green
integer(4)  blue
end subroutine fglColor3b
end interface

interface
subroutine fglColor3bv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glColor3bv@4' :: fglColor3bv
integer*4 v
end subroutine fglColor3bv
end interface

interface
subroutine fglColor3d (red, green, blue)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glColor3d@24' :: fglColor3d
real(8)  red
real(8)  green
real(8)  blue
end subroutine fglColor3d
end interface

interface
subroutine fglColor3dv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glColor3dv@4' :: fglColor3dv
integer*4 v
end subroutine fglColor3dv
end interface

interface
subroutine fglColor3f (red, green, blue)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glColor3f@12' :: fglColor3f
real*4 red
real*4 green
real*4 blue
end subroutine fglColor3f
end interface

interface
subroutine fglColor3fv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glColor3fv@4' :: fglColor3fv
integer*4 v
end subroutine fglColor3fv
end interface

interface
subroutine fglColor3i (red, green, blue)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glColor3i@12' :: fglColor3i
integer*4 red
integer*4 green
integer*4 blue
end subroutine fglColor3i
end interface

interface
subroutine fglColor3iv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glColor3iv@4' :: fglColor3iv
integer*4 v
end subroutine fglColor3iv
end interface

interface
subroutine fglColor3s (red, green, blue)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glColor3s@12' :: fglColor3s
integer*4 red
integer*4 green
integer*4 blue
end subroutine fglColor3s
end interface

interface
subroutine fglColor3sv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glColor3sv@4' :: fglColor3sv
integer*4 v
end subroutine fglColor3sv
end interface

interface
subroutine fglColor3ub (red, green, blue)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glColor3ub@12' :: fglColor3ub
integer*4 red
integer*4 green
integer*4 blue
end subroutine fglColor3ub
end interface

interface
subroutine fglColor3ubv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glColor3ubv@4' :: fglColor3ubv
integer*4 v
end subroutine fglColor3ubv
end interface

interface
subroutine fglColor3ui (red, green, blue)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glColor3ui@12' :: fglColor3ui
integer*4 red
integer*4 green
integer*4 blue
end subroutine fglColor3ui
end interface

interface
subroutine fglColor3uiv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glColor3uiv@4' :: fglColor3uiv
integer*4 v
end subroutine fglColor3uiv
end interface

interface
subroutine fglColor3us (red, green, blue)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glColor3us@12' :: fglColor3us
integer*2 red
integer*2 green
integer*2 blue
end subroutine fglColor3us
end interface

interface
subroutine fglColor3usv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glColor3usv@4' :: fglColor3usv
integer*2 v
end subroutine fglColor3usv
end interface

interface
subroutine fglColor4b (red, green, blue, alpha)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glColor4b@16' :: fglColor4b
character*1 red
character*1 green
character*1 blue
character*1 alpha
end subroutine fglColor4b
end interface

interface
subroutine fglColor4bv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glColor4bv@4' :: fglColor4bv
character*1 v
end subroutine fglColor4bv
end interface

interface
subroutine fglColor4d (red, green, blue, alpha)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glColor4d@32' :: fglColor4d
real*8  red
real*8  green
real*8  blue
real*8  alpha
end subroutine fglColor4d
end interface

interface
subroutine fglColor4dv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glColor4dv@4' :: fglColor4dv
real*4 v
end subroutine fglColor4dv
end interface

interface
subroutine fglColor4f (red, green, blue, alpha)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glColor4f@16' :: fglColor4f
real*4 red
real*4 green
real*4 blue
real*4 alpha
end subroutine fglColor4f
end interface

interface
subroutine fglColor4fv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glColor4fv@4' :: fglColor4fv
integer(4)  v
end subroutine fglColor4fv
end interface

interface
subroutine fglColor4i (red, green, blue, alpha)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glColor4i@16' :: fglColor4i
integer*4 red
integer*4 green
integer*4 blue
integer*4 alpha
end subroutine fglColor4i
end interface

interface
subroutine fglColor4iv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glColor4iv@4' :: fglColor4iv
integer*4 v
end subroutine fglColor4iv
end interface

interface
subroutine fglColor4s (red, green, blue, alpha)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glColor4s@16' :: fglColor4s
integer*2 red
integer*2 green
integer*2 blue
integer*2 alpha
end subroutine fglColor4s
end interface

interface
subroutine fglColor4sv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glColor4sv@4' :: fglColor4sv
integer*2 v
end subroutine fglColor4sv
end interface

interface
subroutine fglColor4ub (red, green, blue, alpha)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glColor4ub@16' :: fglColor4ub
character*1 red
character*1 green
character*1 blue
character*1 alpha
end subroutine fglColor4ub
end interface

interface
subroutine fglColor4ubv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glColor4ubv@4' :: fglColor4ubv
character*1 v
end subroutine fglColor4ubv
end interface

interface
subroutine fglColor4ui (red, green, blue, alpha)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glColor4ui@16' :: fglColor4ui
integer*4 red
integer*4 green
integer*4 blue
integer*4 alpha
end subroutine fglColor4ui
end interface

interface
subroutine fglColor4uiv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glColor4uiv@4' :: fglColor4uiv
integer*4 v
end subroutine fglColor4uiv
end interface

interface
subroutine fglColor4us (red, green, blue, alpha)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glColor4us@16' :: fglColor4us
integer*2 red
integer*2 green
integer*2 blue
integer*2 alpha
end subroutine fglColor4us
end interface

interface
subroutine fglColor4usv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glColor4usv@4' :: fglColor4usv
integer*2 v
end subroutine fglColor4usv
end interface

interface
subroutine  fglColorMask  (red ,green ,blue ,alpha ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glColorMask@16' :: fglColorMask
character*1 red 
character*1 green 
character*1 blue 
character*1 alpha 
end subroutine fglColorMask
end interface

interface
subroutine  fglColorMaterial  (face ,mode ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glColorMaterial@8' :: fglColorMaterial
integer*4 face 
integer*4 mode 
end subroutine fglColorMaterial
end interface

interface
subroutine  fglCopyPixels  (x ,y ,width ,height ,type ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glCopyPixels@20' :: fglCopyPixels
integer*4 x 
integer*4 y 
integer*4 width 
integer*4 height 
integer*4 type 
end subroutine fglCopyPixels
end interface

interface
subroutine  fglCullFace  (mode ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glCullFace@4' :: fglCullFace
integer*4 mode 
end subroutine fglCullFace
end interface

interface
subroutine  fglDeleteLists  (list ,range ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glDeleteLists@8' :: fglDeleteLists
integer*4 list 
integer*4 range 
end subroutine fglDeleteLists
end interface

interface
subroutine  fglDepthFunc  (func ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glDepthFunc@4' :: fglDepthFunc
integer*4 func 
end subroutine fglDepthFunc
end interface

interface
subroutine  fglDepthMask  (flag ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glDepthMask@4' :: fglDepthMask
character*1 flag 
end subroutine fglDepthMask
end interface

interface
subroutine  fglDepthRange  (zNear ,zFar ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glDepthRange@16' :: fglDepthRange
real*8 zNear 
real*8 zFar 
end subroutine fglDepthRange
end interface

interface
subroutine  fglDisable  (cap ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glDisable@4' :: fglDisable
integer*4 cap 
end subroutine fglDisable
end interface

interface
subroutine  fglDrawBuffer  (mode ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glDrawBuffer@4' :: fglDrawBuffer
integer*4 mode 
end subroutine fglDrawBuffer
end interface

interface
subroutine  fglDrawPixels  (width ,height ,format ,type, pixels ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glDrawPixels@20' :: fglDrawPixels
integer*4 width 
integer*4 height 
integer*4 format 
integer*4 type 
integer*4 pixels 
end subroutine fglDrawPixels
end interface

interface
subroutine  fglEdgeFlag  (flag ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glEdgeFlag@4' :: fglEdgeFlag
character*1 flag 
end subroutine fglEdgeFlag
end interface

interface
subroutine  fglEdgeFlagv  (flag ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glEdgeFlagv@4' :: fglEdgeFlagv
character*1 flag 
end subroutine fglEdgeFlagv
end interface

interface
subroutine  fglEnable  (cap ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glEnable@4' :: fglEnable
integer*4 cap 
end subroutine fglEnable
end interface

interface
subroutine  fglEnd  () 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glEnd@0' :: fglEnd
end subroutine fglEnd
end interface

interface
subroutine  fglEndList  () 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glEndList@0' :: fglEndList
end subroutine fglEndList
end interface

interface
subroutine fglEvalCoord1d (u)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glEvalCoord1d@8' :: fglEvalCoord1d
real*8   u
end subroutine fglEvalCoord1d
end interface

interface
subroutine fglEvalCoord1dv (u)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glEvalCoord1dv@4' :: fglEvalCoord1dv
integer(4)  u
end subroutine fglEvalCoord1dv
end interface

interface
subroutine fglEvalCoord1f (u)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glEvalCoord1f@4' :: fglEvalCoord1f
real*4   u
end subroutine fglEvalCoord1f
end interface

interface
subroutine fglEvalCoord1fv (u)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glEvalCoord1fv@4' :: fglEvalCoord1fv
integer(4)     u
end subroutine fglEvalCoord1fv
end interface

interface
subroutine fglEvalCoord2d (u, v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glEvalCoord2d@16' :: fglEvalCoord2d
real*8   u
real*8   v
end subroutine fglEvalCoord2d
end interface

interface
subroutine fglEvalCoord2dv (u)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glEvalCoord2dv@4' :: fglEvalCoord2dv
integer(4)     u
end subroutine fglEvalCoord2dv
end interface

interface
subroutine fglEvalCoord2f (u, v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glEvalCoord2f@8' :: fglEvalCoord2f
real*4   u
real*4   v
end subroutine fglEvalCoord2f
end interface

interface
subroutine fglEvalCoord2fv (u)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glEvalCoord2fv@4' :: fglEvalCoord2fv
integer(4)     u
end subroutine fglEvalCoord2fv
end interface

interface
subroutine fglEvalMesh1 (mode, i1, i2)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glEvalMesh1@12' :: fglEvalMesh1
integer*4 mode
integer*4 i1
integer*4 i2
end subroutine fglEvalMesh1
end interface

interface
subroutine fglEvalMesh2 (mode, i1, i2, j1, j2)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glEvalMesh2@20' :: fglEvalMesh2
integer*4 mode
integer*4 i1
integer*4 i2
integer*4 j1
integer*4 j2
end subroutine fglEvalMesh2
end interface

interface
subroutine fglEvalPoint1 (i)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glEvalPoint1@4' :: fglEvalPoint1
integer*4 i
end subroutine fglEvalPoint1
end interface

interface
subroutine fglEvalPoint2 (i, j)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glEvalPoint2@8' :: fglEvalPoint2
integer*4 i
integer*4 j
end subroutine fglEvalPoint2
end interface

interface
subroutine  fglFeedbackBuffer  (size ,type ,buffer ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glFeedbackBuffer@12' :: fglFeedbackBuffer
integer*4 size  
integer*4 type  
integer*4 buffer  
end subroutine fglFeedbackBuffer
end interface

interface
subroutine  fglFinish  () 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glFinish@0' :: fglFinish
end subroutine fglFinish
end interface

interface
subroutine  fglFlush  () 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glFlush@0' :: fglFlush
end subroutine fglFlush
end interface

interface
subroutine  fglFogf  (pname ,param ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glFogf@8' :: fglFogf
integer*4 pname  
real*4 param  
end subroutine fglFogf
end interface

interface
subroutine  fglFogfv  (pname , params ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glFogfv@8' :: fglFogfv
integer*4 pname  
integer(4)   params  
end subroutine fglFogfv
end interface

interface
subroutine  fglFogi  (pname ,param ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glFogi@8' :: fglFogi
integer*4 pname  
integer*4 param  
end subroutine fglFogi
end interface

interface
subroutine  fglFogiv  (pname ,params ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glFogiv@8' :: fglFogiv
integer*4 pname  
integer*4 params  
end subroutine fglFogiv
end interface

interface
subroutine  fglFrontFace  (mode ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glFrontFace@4' :: fglFrontFace
integer*4 mode  
end subroutine fglFrontFace
end interface

interface
subroutine  fglFrustum  (left ,right ,bottom ,top ,zNear ,zFar ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glFrustum@48' :: fglFrustum
real*8 left  
real*8 right  
real*8 bottom  
real*8 top  
real*8 zNear  
real*8 zFar  
end subroutine fglFrustum
end interface

interface
integer(4) function  fglGenLists  (range ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glGenLists@4' :: fglGenLists
integer*4 range  
end function fglGenLists
end interface

interface
subroutine  fglGetBooleanv  (pname ,params ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glGetBooleanv@8' :: fglGetBooleanv
integer*4 pname  
integer*4 params  
end subroutine fglGetBooleanv
end interface

interface
subroutine  fglGetClipPlane  (plane ,equation ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glGetClipPlane@8' :: fglGetClipPlane
integer*4 plane  
integer*4 equation  
end subroutine fglGetClipPlane
end interface

interface
subroutine  fglGetDoublev  (pname ,params ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glGetDoublev@8' :: fglGetDoublev
integer*4 pname  
integer*4 params  
end subroutine fglGetDoublev
end interface

interface
integer(4) function  fglGetError  () 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glGetError@0' :: fglGetError
end function fglGetError
end interface

interface
subroutine  fglGetFloatv  (pname ,params ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glGetFloatv@8' :: fglGetFloatv
integer*4 pname  
integer*4 params  
end subroutine fglGetFloatv
end interface

interface
subroutine  fglGetIntegerv  (pname ,params ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glGetIntegerv@8' :: fglGetIntegerv
integer*4 pname  
integer*4 params  
end subroutine fglGetIntegerv
end interface

interface
subroutine  fglGetLightfv  (light ,pname ,params ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glGetLightfv@12' :: fglGetLightfv
integer*4 light  
integer*4 pname  
integer*4 params  
end subroutine fglGetLightfv
end interface

interface
subroutine  fglGetLightiv  (light ,pname ,params ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glGetLightiv@12' :: fglGetLightiv
integer*4 light  
integer*4 pname  
integer*4 params  
end subroutine fglGetLightiv
end interface

interface
subroutine  fglGetMapdv  (target ,query ,v ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glGetMapdv@12' :: fglGetMapdv
integer*4 target  
integer*4 query  
integer*4 v  
end subroutine fglGetMapdv
end interface

interface
subroutine  fglGetMapfv  (target ,query ,v ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glGetMapfv@12' :: fglGetMapfv
integer*4 target  
integer*4 query  
integer*4 v  
end subroutine fglGetMapfv
end interface

interface
subroutine  fglGetMapiv  (target ,query ,v ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glGetMapiv@12' :: fglGetMapiv
integer*4 target  
integer*4 query  
integer*4 v  
end subroutine fglGetMapiv
end interface

interface
subroutine  fglGetMaterialfv  (face ,pname ,params ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glGetMaterialfv@12' :: fglGetMaterialfv
integer*4 face  
integer*4 pname  
integer*4 params  
end subroutine fglGetMaterialfv
end interface

interface
subroutine  fglGetMaterialiv  (face ,pname ,params ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glGetMaterialiv@12' :: fglGetMaterialiv
integer*4 face  
integer*4 pname  
integer*4 params  
end subroutine fglGetMaterialiv
end interface

interface
subroutine  fglGetPixelMapfv  (map ,values ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glGetPixelMapfv@8' :: fglGetPixelMapfv
integer*4 map  
integer*4 values  
end subroutine fglGetPixelMapfv
end interface

interface
subroutine  fglGetPixelMapuiv  (map ,values ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glGetPixelMapuiv@8' :: fglGetPixelMapuiv
integer*4 map  
integer*4 values  
end subroutine fglGetPixelMapuiv
end interface

interface
subroutine  fglGetPixelMapusv  (map ,values ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glGetPixelMapusv@8' :: fglGetPixelMapusv
integer*4 map  
integer*4 values  
end subroutine fglGetPixelMapusv
end interface

interface
subroutine  fglGetPolygonStipple  (mask ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glGetPolygonStipple@4' :: fglGetPolygonStipple
integer*4 mask  
end subroutine fglGetPolygonStipple
end interface

interface
integer*4 function fglGetString (String, name) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glGetString@8' :: fglGetString
character*(*)  String
integer*4      name
end function fglGetString
end interface

interface
subroutine  fglGetTexEnvfv  (target ,pname ,params ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glGetTexEnvfv@12' :: fglGetTexEnvfv
integer*4 target  
integer*4 pname  
integer*4 params  
end subroutine fglGetTexEnvfv
end interface

interface
subroutine  fglGetTexEnviv  (target ,pname ,params ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glGetTexEnviv@12' :: fglGetTexEnviv
integer*4 target  
integer*4 pname  
integer*4 params  
end subroutine fglGetTexEnviv
end interface

interface
subroutine  fglGetTexGendv  (coord ,pname ,params ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glGetTexGendv@12' :: fglGetTexGendv
integer*4 coord  
integer*4 pname  
integer*4 params  
end subroutine fglGetTexGendv
end interface

interface
subroutine  fglGetTexGenfv  (coord ,pname ,params ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glGetTexGenfv@12' :: fglGetTexGenfv
integer*4 coord  
integer*4 pname  
integer*4 params  
end subroutine fglGetTexGenfv
end interface

interface
subroutine  fglGetTexGeniv  (coord ,pname ,params ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glGetTexGeniv@12' :: fglGetTexGeniv
integer*4 coord  
integer*4 pname  
integer*4 params  
end subroutine fglGetTexGeniv
end interface

interface
subroutine  fglGetTexImage  (target ,level ,format ,type ,pixels ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glGetTexImage@20' :: fglGetTexImage
integer*4 target  
integer*4 level  
integer*4 format  
integer*4 type  
integer*4 pixels  
end subroutine fglGetTexImage
end interface

interface
subroutine  fglGetTexLevelParameterfv  (target ,level ,pname ,params ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glGetTexLevelParameterfv@16' :: fglGetTexLevelParameterfv
integer*4 target  
integer*4 level  
integer*4 pname  
integer*4 params  
end subroutine fglGetTexLevelParameterfv
end interface

interface
subroutine  fglGetTexLevelParameteriv  (target ,level ,pname ,params ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glGetTexLevelParameteriv@16' :: fglGetTexLevelParameteriv
integer*4 target  
integer*4 level  
integer*4 pname  
integer*4 params  
end subroutine fglGetTexLevelParameteriv
end interface

interface
subroutine  fglGetTexParameterfv  (target ,pname ,params ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glGetTexParameterfv@12' :: fglGetTexParameterfv
integer*4 target  
integer*4 pname  
integer*4 params  
end subroutine fglGetTexParameterfv
end interface

interface
subroutine  fglGetTexParameteriv  (target ,pname ,params ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glGetTexParameteriv@12' :: fglGetTexParameteriv
integer*4 target  
integer*4 pname  
integer*4 params  
end subroutine fglGetTexParameteriv
end interface

interface
subroutine  fglHint  (target ,mode ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glHint@8' :: fglHint
integer*4 target  
integer*4 mode  
end subroutine fglHint
end interface

interface
subroutine  fglIndexMask  (mask ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glIndexMask@4' :: fglIndexMask
integer*4 mask  
end subroutine fglIndexMask
end interface

interface
subroutine  fglIndexd  (c ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glIndexd@4' :: fglIndexd
real*4 c  
end subroutine fglIndexd
end interface

interface
subroutine  fglIndexdv  (c) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glIndexdv@4' :: fglIndexdv
integer(4)   c  
end subroutine fglIndexdv
end interface

interface
subroutine  fglIndexf  (c ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glIndexf@4' :: fglIndexf
real*4 c  
end subroutine fglIndexf
end interface

interface
subroutine  fglIndexfv  (c) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glIndexfv@4' :: fglIndexfv
integer(4)   c  
end subroutine fglIndexfv
end interface

interface
subroutine  fglIndexi  (c ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glIndexi@4' :: fglIndexi
integer*4 c  
end subroutine fglIndexi
end interface

interface
subroutine  fglIndexiv  (c) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glIndexiv@4' :: fglIndexiv
integer*4 c  
end subroutine fglIndexiv
end interface

interface
subroutine  fglIndexs  (c ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glIndexs@4' :: fglIndexs
integer*2 c  
end subroutine fglIndexs
end interface

interface
subroutine  fglIndexsv  (c) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glIndexsv@4' :: fglIndexsv
integer*4 c  
end subroutine fglIndexsv
end interface

interface
subroutine  fglInitNames  () 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glInitNames@0' :: fglInitNames
end subroutine fglInitNames
end interface

interface
logical(4) function  fglIsEnabled  (cap ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glIsEnabled@4' :: fglIsEnabled
integer*4 cap  
end function fglIsEnabled
end interface

interface
logical(4) function  fglIsList  (list ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glIsList@4' :: fglIsList
integer*4 list  
end function fglIsList
end interface

interface
subroutine  fglLightModelf  (pname ,param ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glLightModelf@8' :: fglLightModelf
integer*4 pname  
real*4 param  
end subroutine fglLightModelf
end interface

interface
subroutine  fglLightModelfv  (pname ,params ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glLightModelfv@8' :: fglLightModelfv
integer*4 pname  
integer(4)   params  
end subroutine fglLightModelfv
end interface

interface
subroutine  fglLightModeli  (pname ,param ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glLightModeli@8' :: fglLightModeli
integer*4 pname  
integer*4 param  
end subroutine fglLightModeli
end interface

interface
subroutine  fglLightModeliv  (pname ,params ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glLightModeliv@8' :: fglLightModeliv
integer*4 pname  
integer*4 params  
end subroutine fglLightModeliv
end interface

interface
subroutine  fglLightf  (light ,pname ,param ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glLightf@12' :: fglLightf
integer*4 light  
integer*4 pname  
real*4 param  
end subroutine fglLightf
end interface

interface
subroutine  fglLightfv  (light ,pname ,params ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glLightfv@12' :: fglLightfv
integer*4 light  
integer*4 pname  
integer(4)   params  
end subroutine fglLightfv
end interface

interface
subroutine  fglLighti  (light ,pname ,param ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glLighti@12' :: fglLighti
integer*4 light  
integer*4 pname  
integer*4 param  
end subroutine fglLighti
end interface

interface
subroutine  fglLightiv  (light ,pname ,params ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glLightiv@12' :: fglLightiv
integer*4 light  
integer*4 pname  
integer*4 params  
end subroutine fglLightiv
end interface

interface
subroutine  fglLineStipple  (factor ,pattern ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glLineStipple@8' :: fglLineStipple
integer*4 factor  
integer*2 pattern  
end subroutine fglLineStipple
end interface

interface
subroutine  fglLineWidth  (width ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glLineWidth@4' :: fglLineWidth
real*4 width  
end subroutine fglLineWidth
end interface

interface
subroutine  fglListBase  (base ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glListBase@4' :: fglListBase
integer*4 base  
end subroutine fglListBase
end interface

interface
subroutine  fglLoadIdentity  () 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glLoadIdentity@0' :: fglLoadIdentity
end subroutine fglLoadIdentity
end interface

interface
subroutine  fglLoadMatrixd  (m ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glLoadMatrixd@4' :: fglLoadMatrixd
integer(4)  m  
end subroutine fglLoadMatrixd
end interface

interface
subroutine  fglLoadMatrixf  (m ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glLoadMatrixf@4' :: fglLoadMatrixf
integer(4) m  
end subroutine fglLoadMatrixf
end interface

interface
subroutine  fglLoadName  (name ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glLoadName@4' :: fglLoadName
integer*4 name  
end subroutine fglLoadName
end interface

interface
subroutine  fglLogicOp  (opcode ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glLogicOp@4' :: fglLogicOp
integer*4 opcode  
end subroutine fglLogicOp
end interface

interface 
subroutine fglMap1d (target, u1, u2, stride, order, points)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glMap1d@32' :: fglMap1d
integer*4   target
real*8      u1
real*8      u2
integer*4   stride
integer*4   order
integer*4   points
end subroutine fglMap1d
end interface

interface 
subroutine fglMap1f (target, u1, u2, stride, order, points)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glMap1f@24' :: fglMap1f
integer*4   target
real*4      u1
real*4      u2
integer*4   stride
integer*4   order
integer(4)  points
end subroutine fglMap1f
end interface

interface 
subroutine fglMap2d (target, u1, u2, ustride, uorder, v1, v2, vstride, vorder, points)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glMap2d@56' :: fglMap2d
integer*4 target 
real*8      u1
real*8      u2
integer*4   ustride
integer*4   uorder
real*8      v1
real*8      v2
integer*4   vstride
integer*4   vorder
integer*4   points
end subroutine fglMap2d
end interface

interface 
subroutine fglMap2f (target, u1, u2, ustride, uorder, v1, v2, vstride, vorder, points)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glMap2f@40' :: fglMap2f
integer*4 target 
real*4 u1
real*4 u2
integer*4 ustride
integer*4 uorder
real*4 v1
real*4 v2
integer*4 vstride
integer*4 vorder
integer*4 points
end subroutine fglMap2f
end interface

interface 
subroutine fglMapGrid1d (un, u1, u2)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glMapGrid1d@20' :: fglMapGrid1d
integer*4 un 
real*8 u1
real*8 u2
end subroutine fglMapGrid1d
end interface

interface 
subroutine fglMapGrid1f (un, u1, u2)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glMapGrid1f@12' :: fglMapGrid1f
integer*4 un 
real*4 u1
real*4 u2
end subroutine fglMapGrid1f
end interface

interface 
subroutine fglMapGrid2d (un, u1, u2, vn, v1, v2)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glMapGrid2d@40' :: fglMapGrid2d
integer*4 un
real*8 u1
real*8 u2
integer*4 vn
real*8 v1
real*8 v2
end subroutine fglMapGrid2d
end interface

interface 
subroutine fglMapGrid2f (un, u1, u2, vn, v1, v2)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glMapGrid2f@24' :: fglMapGrid2f
integer*4 un
real*4 u1
real*4 u2
integer*4 vn
real*4 v1
real*4 v2
end subroutine fglMapGrid2f
end interface

interface
subroutine  fglMaterialf  (face ,pname ,param ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glMaterialf@12' :: fglMaterialf
integer*4 face  
integer*4 pname  
real*4 param  
end subroutine fglMaterialf
end interface

interface
subroutine  fglMaterialfv  (face ,pname ,params ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glMaterialfv@12' :: fglMaterialfv
integer*4 face  
integer*4 pname  
integer*4 params  
end subroutine fglMaterialfv
end interface

interface
subroutine  fglMateriali  (face ,pname ,param ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glMateriali@12' :: fglMateriali
integer*4 face  
integer*4 pname  
integer*4 param  
end subroutine fglMateriali
end interface
             
interface
subroutine  fglMaterialiv  (face ,pname ,params ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glMaterialiv@12' :: fglMaterialiv
integer*4 face  
integer*4 pname  
integer*4 params  
end subroutine fglMaterialiv
end interface

interface
subroutine  fglMatrixMode  (mode ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glMatrixMode@4' :: fglMatrixMode
integer*4 mode  
end subroutine fglMatrixMode
end interface
             
interface
subroutine  fglMultMatrixd  (m ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glMultMatrixd@4' :: fglMultMatrixd
integer(4) m  
end subroutine fglMultMatrixd
end interface
             
interface
subroutine  fglMultMatrixf  (m ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glMultMatrixf@4' :: fglMultMatrixf
integer(4) m  
end subroutine fglMultMatrixf
end interface
             
interface
subroutine  fglNewList  (list ,mode ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glNewList@8' :: fglNewList
integer*4 list  
integer*4 mode  
end subroutine fglNewList
end interface
             
interface 
subroutine fglNormal3b (nx, ny, nz)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glNormal3b@12' :: fglNormal3b
character*1 nx 
character*1 ny
character*1 nz
end subroutine fglNormal3b
end interface

interface
subroutine fglNormal3bv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glNormal3bv@' :: fglNormal3bv
integer(4) v
end subroutine fglNormal3bv
end interface

interface
subroutine fglNormal3d (nx, ny, nz)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glNormal3d@24' :: fglNormal3d
real*8 nx
real*8 ny
real*8 nz
end subroutine fglNormal3d
end interface

interface
subroutine fglNormal3dv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glNormal3dv@4' :: fglNormal3dv
integer(4)     v
end subroutine fglNormal3dv
end interface

interface
subroutine fglNormal3f (nx, ny, nz)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glNormal3f@12' :: fglNormal3f
real*4 nx
real*4 ny
real*4 nz
end subroutine fglNormal3f
end interface

interface
subroutine fglNormal3fv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glNormal3fv@4' :: fglNormal3fv
integer(4)     v
end subroutine fglNormal3fv
end interface

interface
subroutine fglNormal3i (nx, ny, nz)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glNormal3i@12' :: fglNormal3i
integer*4 nx 
integer*4 ny
integer*4 nz
end subroutine fglNormal3i
end interface

interface
subroutine fglNormal3iv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glNormal3iv@4' :: fglNormal3iv
integer(4)     v
end subroutine fglNormal3iv
end interface

interface
subroutine fglNormal3s (nx, ny, nz)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glNormal3s@12' :: fglNormal3s
integer*2 nx
integer*2 ny 
integer*2 nz
end subroutine fglNormal3s
end interface

interface
subroutine fglNormal3sv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glNormal3sv@' :: fglNormal3sv
integer(4)  v
end subroutine fglNormal3sv
end interface

interface
subroutine  fglOrtho  (left ,right ,bottom ,top ,zNear ,zFar ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glOrtho@48' :: fglOrtho
real*8 left  
real*8 right  
real*8 bottom  
real*8 top  
real*8 zNear  
real*8 zFar  
end subroutine fglOrtho
end interface

interface
subroutine  fglPassThrough  (token ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glPassThrough@4' :: fglPassThrough
real*4 token  
end subroutine fglPassThrough
end interface

interface
subroutine  fglPixelMapfv  (map ,mapsize ,values ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glPixelMapfv@12' :: fglPixelMapfv
integer*4 map  
integer*4 mapsize  
integer(4)   values  
end subroutine fglPixelMapfv
end interface
             
interface
subroutine  fglPixelMapuiv  (map ,mapsize ,values ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glPixelMapuiv@12' :: fglPixelMapuiv
integer*4 map  
integer*4 mapsize  
integer*4 values  
end subroutine fglPixelMapuiv
end interface
             
interface
subroutine  fglPixelMapusv  (map ,mapsize ,values ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glPixelMapusv@12' :: fglPixelMapusv
integer*4 map  
integer*4 mapsize  
integer(4)  values  
end subroutine fglPixelMapusv
end interface

interface
subroutine  fglPixelStoref  (pname ,param ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glPixelStoref@8' :: fglPixelStoref
integer*4 pname  
real*4 param  
end subroutine fglPixelStoref
end interface

interface
subroutine  fglPixelStorei  (pname ,param ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glPixelStorei@8' :: fglPixelStorei
integer*4 pname  
integer*4 param  
end subroutine fglPixelStorei
end interface

interface
subroutine  fglPixelTransferf  (pname ,param ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glPixelTransferf@8' :: fglPixelTransferf
integer*4 pname  
real*4 param  
end subroutine fglPixelTransferf
end interface

interface
subroutine  fglPixelTransferi  (pname ,param ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glPixelTransferi@8' :: fglPixelTransferi
integer*4 pname  
integer*4 param  
end subroutine fglPixelTransferi
end interface

interface
subroutine  fglPixelZoom  (xfactor ,yfactor ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glPixelZoom@8' :: fglPixelZoom
real*4 xfactor  
real*4 yfactor  
end subroutine fglPixelZoom
end interface

interface
subroutine  fglPointSize  (size ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glPointSize@4' :: fglPointSize
real*4 size  
end subroutine fglPointSize
end interface

interface
subroutine  fglPolygonMode  (face ,mode ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glPolygonMode@8' :: fglPolygonMode
integer*4 face  
integer*4 mode  
end subroutine fglPolygonMode
end interface

interface
subroutine  fglPolygonStipple  (mask ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glPolygonStipple@4' :: fglPolygonStipple
integer(4)  mask  
end subroutine fglPolygonStipple
end interface

interface
subroutine  fglPopAttrib  () 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glPopAttrib@0' :: fglPopAttrib
end subroutine fglPopAttrib
end interface

interface
subroutine  fglPopMatrix  () 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glPopMatrix@0' :: fglPopMatrix
end subroutine fglPopMatrix
end interface

interface
subroutine  fglPopName  () 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glPopName@0' :: fglPopName
end subroutine fglPopName
end interface

interface
subroutine  fglPushAttrib  (mask ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glPushAttrib@4' :: fglPushAttrib
integer*4 mask  
end subroutine fglPushAttrib
end interface

interface
subroutine  fglPushMatrix  () 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glPushMatrix@0' :: fglPushMatrix
end subroutine fglPushMatrix
end interface

interface
subroutine  fglPushName  (name ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glPushName@4' :: fglPushName
integer*4 name  
end subroutine fglPushName
end interface

interface 
subroutine fglRasterPos2d (x, y)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glRasterPos2d@16' :: fglRasterPos2d
real*8 x 
real*8 y
end subroutine fglRasterPos2d
end interface

interface 
subroutine fglRasterPos2dv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glRasterPos2dv@4' :: fglRasterPos2dv
integer(4)  v
end subroutine fglRasterPos2dv
end interface

interface 
subroutine fglRasterPos2f (x, y)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glRasterPos2f@8' :: fglRasterPos2f
real*4 x 
real*4 y
end subroutine fglRasterPos2f
end interface

interface 
subroutine fglRasterPos2fv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glRasterPos2fv@4' :: fglRasterPos2fv
integer(4)   v
end subroutine fglRasterPos2fv
end interface

interface 
subroutine fglRasterPos2i (x, y)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glRasterPos2i@8' :: fglRasterPos2i
integer*4 x
integer*4 y
end subroutine fglRasterPos2i
end interface

interface 
subroutine fglRasterPos2iv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glRasterPos2iv@' :: fglRasterPos2iv
integer*4 v
end subroutine fglRasterPos2iv
end interface

interface 
subroutine fglRasterPos2s (x, y)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glRasterPos2s@8' :: fglRasterPos2s
integer*2 x 
integer*2 y
end subroutine fglRasterPos2s
end interface

interface 
subroutine fglRasterPos2sv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glRasterPos2sv@4' :: fglRasterPos2sv
integer(4)  v
end subroutine fglRasterPos2sv
end interface

interface 
subroutine fglRasterPos3d (x, y, z)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glRasterPos3d@24' :: fglRasterPos3d
real(8)  x
real(8)  y
real(8)  z
end subroutine fglRasterPos3d
end interface

interface 
subroutine fglRasterPos3dv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glRasterPos3dv@4' :: fglRasterPos3dv
integer(4)   v
end subroutine fglRasterPos3dv
end interface

interface 
subroutine fglRasterPos3f (x, y, z)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glRasterPos3f@12' :: fglRasterPos3f
real*4 x
real*4 y
real*4 z
end subroutine fglRasterPos3f
end interface

interface 
subroutine fglRasterPos3fv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glRasterPos3fv@4' :: fglRasterPos3fv
integer(4)   v
end subroutine fglRasterPos3fv
end interface

interface 
subroutine fglRasterPos3i (x, y, z)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glRasterPos3i@12' :: fglRasterPos3i
integer*4 x
integer*4 y
integer*4 z
end subroutine fglRasterPos3i
end interface

interface 
subroutine fglRasterPos3iv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glRasterPos3iv@4' :: fglRasterPos3iv
integer*4 v
end subroutine fglRasterPos3iv
end interface

interface 
subroutine fglRasterPos3s (x, y, z)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glRasterPos3s@12' :: fglRasterPos3s
integer*2 x
integer*2 y
integer*2 z
end subroutine fglRasterPos3s
end interface

interface 
subroutine fglRasterPos3sv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glRasterPos3sv@' :: fglRasterPos3sv
integer(4)   v
end subroutine fglRasterPos3sv
end interface

interface 
subroutine fglRasterPos4d (x, y, z, w)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glRasterPos4d@32' :: fglRasterPos4d
real*8 x
real*8 y
real*8 z
real*8 w
end subroutine fglRasterPos4d
end interface

interface 
subroutine fglRasterPos4dv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glRasterPos4dv@4' :: fglRasterPos4dv
integer(4)     v
end subroutine fglRasterPos4dv
end interface

interface 
subroutine fglRasterPos4f (x, y, z, w)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glRasterPos4f@16' :: fglRasterPos4f
real*4 x
real*4 y
real*4 z
real*4 w
end subroutine fglRasterPos4f
end interface

interface 
subroutine fglRasterPos4fv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glRasterPos4fv@4' :: fglRasterPos4fv
integer(4)   v
end subroutine fglRasterPos4fv
end interface

interface 
subroutine fglRasterPos4i (x, y, z, w)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glRasterPos4i@16' :: fglRasterPos4i
integer*4 x
integer*4 y
integer*4 z
integer*4 w
end subroutine fglRasterPos4i
end interface

interface 
subroutine fglRasterPos4iv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glRasterPos4iv@' :: fglRasterPos4iv
integer*4 v
end subroutine fglRasterPos4iv
end interface

interface 
subroutine fglRasterPos4s (x, y, z, w)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glRasterPos4s@16' :: fglRasterPos4s
integer*2 x
integer*2 y
integer*2 z
integer*2 w
end subroutine fglRasterPos4s
end interface

interface 
subroutine fglRasterPos4sv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glRasterPos4sv@4' :: fglRasterPos4sv
integer(4)  v
end subroutine fglRasterPos4sv
end interface

interface
subroutine  fglReadBuffer  (mode ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glReadBuffer@4' :: fglReadBuffer
integer*4 mode  
end subroutine fglReadBuffer
end interface

interface
subroutine  fglReadPixels  (x ,y ,width ,height ,format ,type ,pixels ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glReadPixels@28' :: fglReadPixels
integer*4 x  
integer*4 y  
integer*4 width  
integer*4 height  
integer*4 format  
integer*4 type  
integer*4 pixels  
end subroutine fglReadPixels
end interface

interface
subroutine  fglRectd  (x1 ,y1 ,x2 ,y2 ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glRectd@16' :: fglRectd
real*4 x1  
real*4 y1  
real*4 x2  
real*4 y2  
end subroutine fglRectd
end interface

interface
subroutine  fglRectdv  (va ,vb ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glRectdv@8' :: fglRectdv
integer(4)   va  
integer(4)   vb  
end subroutine fglRectdv
end interface

interface
subroutine  fglRectf  (x1 ,y1 ,x2 ,y2 ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glRectf@16' :: fglRectf
real*4 x1  
real*4 y1  
real*4 x2  
real*4 y2  
end subroutine fglRectf
end interface

interface
subroutine  fglRectfv  (va ,vb ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glRectfv@8' :: fglRectfv
integer(4)   va  
integer(4)   vb  
end subroutine fglRectfv
end interface

interface
subroutine  fglRecti  (x1 ,y1 ,x2 ,y2 ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glRecti@16' :: fglRecti
integer*4 x1  
integer*4 y1  
integer*4 x2  
integer*4 y2  
end subroutine fglRecti
end interface

interface
subroutine  fglRectiv  (va ,vb ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glRectiv@8' :: fglRectiv
integer*4 va  
integer*4 vb  
end subroutine fglRectiv
end interface

interface
subroutine  fglRects  (x1 ,y1 ,x2 ,y2 ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glRects@16' :: fglRects
integer*2 x1  
integer*2 y1  
integer*2 x2  
integer*2 y2  
end subroutine fglRects
end interface

interface
subroutine  fglRectsv  (va ,vb ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glRectsv@8' :: fglRectsv
integer(4)  va  
integer(4)  vb  
end subroutine fglRectsv
end interface

interface
subroutine  fglRenderMode  (mode ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glRenderMode@4' :: fglRenderMode
integer*4 mode  
end subroutine fglRenderMode
end interface

interface
subroutine  fglRotated  (angle ,x ,y ,z ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glRotated@16' :: fglRotated
real*4 angle  
real*4 x  
real*4 y  
real*4 z  
end subroutine fglRotated
end interface

interface
subroutine  fglRotatef  (angle ,x ,y ,z ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glRotatef@16' :: fglRotatef
real*4 angle  
real*4 x  
real*4 y  
real*4 z  
end subroutine fglRotatef
end interface

interface
subroutine  fglScaled  (x ,y ,z ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glScaled@12' :: fglScaled
real*4 x  
real*4 y  
real*4 z  
end subroutine fglScaled
end interface

interface
subroutine  fglScalef  (x ,y ,z ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glScalef@12' :: fglScalef
real*4 x  
real*4 y  
real*4 z  
end subroutine fglScalef
end interface

interface
subroutine  fglScissor  (x ,y ,width ,height ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glScissor@16' :: fglScissor
integer*4 x  
integer*4 y  
integer*4 width  
integer*4 height  
end subroutine fglScissor
end interface

interface
subroutine  fglSelectBuffer  (size ,buffer ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glSelectBuffer@8' :: fglSelectBuffer
integer*4 size  
integer*4 buffer  
end subroutine fglSelectBuffer
end interface

interface
subroutine  fglShadeModel  (mode ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glShadeModel@4' :: fglShadeModel
integer*4 mode  
end subroutine fglShadeModel
end interface

interface
subroutine  fglStencilFunc  (func ,ref ,mask ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glStencilFunc@12' :: fglStencilFunc
integer*4 func  
integer*4 ref  
integer*4 mask  
end subroutine fglStencilFunc
end interface

interface
subroutine  fglStencilMask  (mask ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glStencilMask@4' :: fglStencilMask
integer*4 mask  
end subroutine fglStencilMask
end interface

interface
subroutine  fglStencilOp  (fail ,zfail ,zpass ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glStencilOp@12' :: fglStencilOp
integer*4 fail  
integer*4 zfail  
integer*4 zpass  
end subroutine fglStencilOp
end interface

interface 
subroutine fglTexCoord1d (s)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexCoord1d@8' :: fglTexCoord1d
real*8   s
end subroutine fglTexCoord1d
end interface

interface 
subroutine fglTexCoord1dv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexCoord1dv@4' :: fglTexCoord1dv
integer(4)   v
end subroutine fglTexCoord1dv
end interface

interface 
subroutine fglTexCoord1f (s)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexCoord1f@4' :: fglTexCoord1f
real*4 s
end subroutine fglTexCoord1f
end interface

interface 
subroutine fglTexCoord1fv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexCoord1fv@4' :: fglTexCoord1fv
integer(4)   v
end subroutine fglTexCoord1fv
end interface

interface 
subroutine fglTexCoord1i (s)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexCoord1i@4' :: fglTexCoord1i
integer*4 s
end subroutine fglTexCoord1i
end interface

interface 
subroutine fglTexCoord1iv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexCoord1iv@4' :: fglTexCoord1iv
integer*4 v
end subroutine fglTexCoord1iv
end interface

interface 
subroutine fglTexCoord1s (s)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexCoord1s@4' :: fglTexCoord1s
integer*2 s
end subroutine fglTexCoord1s
end interface

interface 
subroutine fglTexCoord1sv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexCoord1sv@4' :: fglTexCoord1sv
integer(4)  v
end subroutine fglTexCoord1sv
end interface

interface 
subroutine fglTexCoord2d (s, t)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexCoord2d@16' :: fglTexCoord2d
real*8 s
real*8 t
end subroutine fglTexCoord2d
end interface

interface 
subroutine fglTexCoord2dv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexCoord2dv@4' :: fglTexCoord2dv
integer(4)   v
end subroutine fglTexCoord2dv
end interface

interface 
subroutine fglTexCoord2f (s, t)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexCoord2f@8' :: fglTexCoord2f
real*4 s
real*4 t
end subroutine fglTexCoord2f
end interface

interface 
subroutine fglTexCoord2fv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexCoord2fv@4' :: fglTexCoord2fv
integer(4)   v
end subroutine fglTexCoord2fv
end interface

interface 
subroutine fglTexCoord2i (s, t)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexCoord2i@8' :: fglTexCoord2i
integer*4 s
integer*4 t
end subroutine fglTexCoord2i
end interface

interface 
subroutine fglTexCoord2iv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexCoord2iv@4' :: fglTexCoord2iv
integer*4 v
end subroutine fglTexCoord2iv
end interface

interface 
subroutine fglTexCoord2s (s, t)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexCoord2s@8' :: fglTexCoord2s
integer*2 s 
integer*2 t
end subroutine fglTexCoord2s
end interface

interface 
subroutine fglTexCoord2sv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexCoord2sv@4' :: fglTexCoord2sv
integer(4)  v
end subroutine fglTexCoord2sv
end interface

interface 
subroutine fglTexCoord3d (s, t, r)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexCoord3d@24' :: fglTexCoord3d
real*8 s 
real*8 t
real*8 r
end subroutine fglTexCoord3d
end interface

interface 
subroutine fglTexCoord3dv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexCoord3dv@4' :: fglTexCoord3dv
integer(4)   v
end subroutine fglTexCoord3dv
end interface

interface 
subroutine fglTexCoord3f (s, t, r)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexCoord3f@12' :: fglTexCoord3f
real*4 s
real*4 t
real*4 r
end subroutine fglTexCoord3f
end interface

interface 
subroutine fglTexCoord3fv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexCoord3fv@4' :: fglTexCoord3fv
integer(4)   v
end subroutine fglTexCoord3fv
end interface

interface 
subroutine fglTexCoord3i (s, t, r)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexCoord3i@12' :: fglTexCoord3i
integer*4 s
integer*4 t
integer*4 r
end subroutine fglTexCoord3i
end interface

interface 
subroutine fglTexCoord3iv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexCoord3iv@4' :: fglTexCoord3iv
integer*4 v
end subroutine fglTexCoord3iv
end interface

interface 
subroutine fglTexCoord3s (s, t, r)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexCoord3s@12' :: fglTexCoord3s
integer*2 s
integer*2 t
integer*2 r
end subroutine fglTexCoord3s
end interface

interface 
subroutine fglTexCoord3sv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexCoord3sv@4' :: fglTexCoord3sv
integer(4)  v
end subroutine fglTexCoord3sv
end interface

interface 
subroutine fglTexCoord4d (s, t, r, q)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexCoord4d@32' :: fglTexCoord4d
real*8 s
real*8 t
real*8 r
real*8 q
end subroutine fglTexCoord4d
end interface

interface 
subroutine fglTexCoord4dv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexCoord4dv@4' :: fglTexCoord4dv
integer(4)   v
end subroutine fglTexCoord4dv
end interface

interface 
subroutine fglTexCoord4f (s, t, r, q)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexCoord4f@16' :: fglTexCoord4f
real*4 s
real*4 t
real*4 r
real*4 q
end subroutine fglTexCoord4f
end interface

interface 
subroutine fglTexCoord4fv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexCoord4fv@4' :: fglTexCoord4fv
integer(4)   v
end subroutine fglTexCoord4fv
end interface

interface 
subroutine fglTexCoord4i (s, t, r, q)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexCoord4i@16' :: fglTexCoord4i
integer*4 s
integer*4 t
integer*4 r
integer*4 q
end subroutine fglTexCoord4i
end interface

interface 
subroutine fglTexCoord4iv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexCoord4iv@4' :: fglTexCoord4iv
integer*4 v
end subroutine fglTexCoord4iv
end interface

interface 
subroutine fglTexCoord4s (s, t, r, q)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexCoord4s@16' :: fglTexCoord4s
integer*2 s
integer*2 t
integer*2 r
integer*2 q
end subroutine fglTexCoord4s
end interface

interface 
subroutine fglTexCoord4sv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexCoord4sv@4' :: fglTexCoord4sv
integer(4)  v
end subroutine fglTexCoord4sv
end interface



interface
subroutine  fglTexEnvf  (target ,pname ,param ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexEnvf@12' :: fglTexEnvf
integer*4 target  
integer*4 pname  
real*4 param  
end subroutine fglTexEnvf
end interface

interface
subroutine  fglTexEnvfv  (target ,pname , params ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexEnvfv@12' :: fglTexEnvfv
integer*4 target  
integer*4 pname  
integer(4)   params  
end subroutine fglTexEnvfv
end interface

interface
subroutine  fglTexEnvi  (target ,pname ,param ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexEnvi@12' :: fglTexEnvi
integer*4 target  
integer*4 pname  
integer*4 param  
end subroutine fglTexEnvi
end interface

interface
subroutine  fglTexEnviv  (target ,pname ,params ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexEnviv@12' :: fglTexEnviv
integer*4 target  
integer*4 pname  
integer*4 params  
end subroutine fglTexEnviv
end interface

interface
subroutine  fglTexGend  (coord ,pname ,param ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexGend@12' :: fglTexGend
integer*4 coord  
integer*4 pname  
real*4 param  
end subroutine fglTexGend
end interface

interface
subroutine  fglTexGendv  (coord ,pname ,params ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexGendv@12' :: fglTexGendv
integer*4 coord  
integer*4 pname  
integer(4)   params  
end subroutine fglTexGendv
end interface

interface
subroutine  fglTexGenf  (coord ,pname ,param ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexGenf@12' :: fglTexGenf
integer*4 coord  
integer*4 pname  
real*4 param  
end subroutine fglTexGenf
end interface

interface
subroutine  fglTexGenfv  (coord ,pname ,params ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexGenfv@12' :: fglTexGenfv
integer*4 coord  
integer*4 pname  
integer(4)   params  
end subroutine fglTexGenfv
end interface

interface
subroutine  fglTexGeni  (coord ,pname ,param ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexGeni@12' :: fglTexGeni
integer*4 coord  
integer*4 pname  
integer*4 param  
end subroutine fglTexGeni
end interface

interface
subroutine  fglTexGeniv  (coord ,pname ,params ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexGeniv@12' :: fglTexGeniv
integer*4 coord  
integer*4 pname  
integer*4 params  
end subroutine fglTexGeniv
end interface

interface
subroutine fglTexImage1D (target, level, components, width, border, format, type, pixels)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexImage1D@32' :: fglTexImage1D
integer*4 target
integer*4 level
integer*4 components
integer*4 width
integer*4 border
integer*4 format
integer*4 type
integer*4 pixels
end subroutine fglTexImage1d
end interface

interface
subroutine fglTexImage2D (target, level, components, width, height, border, format, type, pixels)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexImage2D@36' :: fglTexImage2D
integer*4 target
integer*4 level
integer*4 components
integer*4 width
integer*4 height
integer*4 border
integer*4 format
integer*4 type 
integer*4 pixels
end subroutine fglTexImage2D
end interface

interface
subroutine  fglTexParameterf  (target ,pname ,param ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexParameterf@12' :: fglTexParameterf
integer*4 target  
integer*4 pname  
real*4 param  
end subroutine fglTexParameterf
end interface

interface
subroutine  fglTexParameterfv  (target ,pname ,params ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexParameterfv@12' :: fglTexParameterfv
integer*4 target  
integer*4 pname  
integer(4)   params  
end subroutine fglTexParameterfv
end interface

interface
subroutine  fglTexParameteri  (target ,pname ,param ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexParameteri@12' :: fglTexParameteri
integer*4 target  
integer*4 pname  
integer*4 param  
end subroutine fglTexParameteri
end interface

interface
subroutine  fglTexParameteriv  (target ,pname ,params ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTexParameteriv@12' :: fglTexParameteriv
integer*4 target  
integer*4 pname  
integer*4 params  
end subroutine fglTexParameteriv
end interface

interface
subroutine  fglTranslated  (x ,y ,z ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTranslated@12' :: fglTranslated
real*4 x  
real*4 y  
real*4 z  
end subroutine fglTranslated
end interface

interface
subroutine  fglTranslatef  (x ,y ,z ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glTranslatef@12' :: fglTranslatef
real*4 x  
real*4 y  
real*4 z  
end subroutine fglTranslatef
end interface

interface 
subroutine fglVertex2d (x, y)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glVertex2d@16' :: fglVertex2d
real*8 x
real*8 y
end subroutine fglVertex2d
end interface

interface 
subroutine fglVertex2dv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glVertex2dv@4' :: fglVertex2dv
integer(4)   v
end subroutine fglVertex2dv
end interface

interface 
subroutine fglVertex2f (x, y)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glVertex2f@8' :: fglVertex2f
real*4 x
real*4 y
end subroutine fglVertex2f
end interface

interface 
subroutine fglVertex2fv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glVertex2fv@4' :: fglVertex2fv
integer(4)   v
end subroutine fglVertex2fv
end interface

interface 
subroutine fglVertex2i (x, y)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glVertex2i@8' :: fglVertex2i
integer*4 x
integer*4 y
end subroutine fglVertex2i
end interface

interface 
subroutine fglVertex2iv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glVertex2iv@4' :: fglVertex2iv
integer*4 v
end subroutine fglVertex2iv
end interface

interface 
subroutine fglVertex2s (x, y)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glVertex2s@8' :: fglVertex2s
integer*2 x
integer*2 y
end subroutine fglVertex2s
end interface

interface 
subroutine fglVertex2sv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glVertex2sv@4' :: fglVertex2sv
integer(4)  v
end subroutine fglVertex2sv
end interface

interface 
subroutine fglVertex3d (x, y, z)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glVertex3d@24' :: fglVertex3d
real*8 x
real*8 y
real*8 z
end subroutine fglVertex3d
end interface

interface 
subroutine fglVertex3dv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glVertex3dv@4' :: fglVertex3dv
integer(4)   v
end subroutine fglVertex3dv
end interface

interface 
subroutine fglVertex3f (x, y, z)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glVertex3f@12' :: fglVertex3f
real*4 x
real*4 y
real*4 z
end subroutine fglVertex3f
end interface

interface 
subroutine fglVertex3fv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glVertex3fv@4' :: fglVertex3fv
integer(4)   v
end subroutine fglVertex3fv
end interface

interface 
subroutine fglVertex3i (x)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glVertex3i@12' :: fglVertex3i
end subroutine fglVertex3i
end interface

interface 
subroutine fglVertex3iv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glVertex3iv@4' :: fglVertex3iv
integer(4)  v
end subroutine fglVertex3iv
end interface

interface 
subroutine fglVertex3s (x, y, z)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glVertex3s@12' :: fglVertex3s
integer*2   x 
integer*2   y 
integer*2   z
end subroutine fglVertex3s
end interface

interface 
subroutine fglVertex3sv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glVertex3sv@4' :: fglVertex3sv
integer(4)    v
end subroutine fglVertex3sv
end interface

interface 
subroutine fglVertex4d (x, y, z, w)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glVertex4d@32' :: fglVertex4d
real*8 x 
real*8 y 
real*8 z 
real*8 w
end subroutine fglVertex4d
end interface

interface 
subroutine fglVertex4dv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glVertex4dv@4' :: fglVertex4dv
integer(4) v
end subroutine fglVertex4dv
end interface

interface 
subroutine fglVertex4f (x, y, z, w)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glVertex4f@16' :: fglVertex4f
real*4 x
real*4 y
real*4 z
real*4 w
end subroutine fglVertex4f
end interface

interface 
subroutine fglVertex4fv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glVertex4fv@4' :: fglVertex4fv
integer(4)     v
end subroutine fglVertex4fv
end interface

interface 
subroutine fglVertex4i (x, y, z, w)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glVertex4i@16' :: fglVertex4i
integer*4   x
integer*4   y
integer*4   z
integer*4   w
end subroutine fglVertex4i
end interface

interface 
subroutine fglVertex4iv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glVertex4iv@4' :: fglVertex4iv
integer(4)  v
end subroutine fglVertex4iv
end interface

interface 
subroutine fglVertex4s (x, y, z, w)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glVertex4s@16' :: fglVertex4s
integer*2 x
integer*2 y
integer*2 z
integer*2 w
end subroutine fglVertex4s
end interface

interface 
subroutine fglVertex4sv (v)
!MS$ATTRIBUTES STDCALL, ALIAS : '_glVertex4sv@4' :: fglVertex4sv
integer(4)  v
end subroutine fglVertex4sv
end interface


interface
subroutine  fglViewport  (x ,y ,width ,height ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_glViewport@16' :: fglViewport
integer*4 x  
integer*4 y  
integer*4 width  
integer*4 height  
end subroutine fglViewport
end interface


!                           **** FGLU ****

interface
integer*4 function fgluErrorString (String, errCode)
!MS$ATTRIBUTES STDCALL, ALIAS : '_gluErrorString@8' :: fgluErrorString
character*(*)  String
integer(4)     errCode
end function fgluErrorString
end interface

interface
subroutine fgluOrtho2D (left, right, bottom, top)
!MS$ATTRIBUTES STDCALL, ALIAS : '_gluOrtho2D@32' :: fgluOrtho2D
real*8 left
real*8 right
real*8 bottom
real*8 top
end subroutine fgluOrtho2D
end interface

interface
subroutine  fgluPerspective  (fovy ,aspect ,zNear ,zFar ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_gluPerspective@32' :: fgluPerspective
real*8 fovy 
real*8 aspect 
real*8 zNear 
real*8 zFar 
end subroutine fgluPerspective
end interface
             
interface
subroutine  fgluPickMatrix  (x ,y ,width ,height ,viewport )
!MS$ATTRIBUTES STDCALL, ALIAS : '_gluPickMatrix@20' :: fgluPickMatrix
real*8 x 
real*8 y 
real*8 width 
real*8 height 
integer*4 viewport(4) 
end subroutine fgluPickMatrix
end interface

interface
subroutine  fgluLookAt  (eyex ,eyey ,eyez ,centerx ,centery ,centerz ,upx ,upy ,upz ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_gluLookAt@72' :: fgluLookAt
real*8 eyex 
real*8 eyey 
real*8 eyez 
real*8 centerx 
real*8 centery 
real*8 centerz 
real*8 upx 
real*8 upy 
real*8 upz 
end subroutine fgluLookAt
end interface

interface
integer(4) function  fgluProject  (objx ,objy ,objz ,modelMatrix ,projMatrix ,viewport ,winx ,winy ,winz ) 
!MS$ ATTRIBUTES STDCALL, ALIAS : '_gluProject@48' :: fgluProject
!MS$ ATTRIBUTES REFERENCE :: winx, winy, winz
real*8 objx 
real*8 objy 
real*8 objz 
real*8 modelMatrix(16) 
real*8 projMatrix(16) 
integer*4 viewport(4) 
integer*4 winx 
integer*4 winy 
integer*4 winz 
end function fgluProject
end interface

interface
integer(4) function  fgluUnProject  (winx ,winy ,winz , modelMatrix ,projMatrix ,viewport ,objx ,objy ,objz ) 
!MS$ ATTRIBUTES STDCALL, ALIAS : '_gluUnProject@48' :: fgluUnProject
!MS$ ATTRIBUTES REFERENCE :: objx, objy, objz
real*8 winx 
real*8 winy 
real*8 winz 
real*8 modelMatrix(16) 
real*8 projMatrix(16) 
integer*4 viewport(4) 
integer*4 objx 
integer*4 objy 
integer*4 objz 
end function fgluUnProject
end interface

interface
integer(4) function  fgluScaleImage  (format ,widthin ,heightin ,typein ,datain ,widthout ,heightout ,typeout ,dataout ) 
!MS$ ATTRIBUTES STDCALL, ALIAS : '_gluScaleImage@36' :: fgluScaleImage
!MS$ ATTRIBUTES REFERENCE :: datain, dataout
integer*4 format 
integer*4 widthin 
integer*4 heightin 
integer*4 typein 
character*8 datain 
integer*4 widthout 
integer*4 heightout 
integer*4 typeout 
character*8 dataout 
end function fgluScaleImage
end interface

interface
function fgluBuild1DMipmaps (target, components, width, format, type, data)
!MS$ATTRIBUTES STDCALL, ALIAS : '_gluBuild1DMipmaps@24' :: fgluBuild1DMipmaps
integer*4   target
integer*4   components
integer*4   width
integer*4   format
integer*4   type
character*8 data
end function fgluBuild1DMipmaps
end interface
             
interface
integer(4) function fgluBuild2DMipmaps (target, components, width, height, format, type, data)
!MS$ATTRIBUTES STDCALL, ALIAS : '_gluBuild2DMipmaps@28' :: fgluBuild2DMipmaps
integer*4 target
integer*4 components
integer*4 width
integer*4 height
integer*4 format
integer*4 type
character*8 data
end function fgluBuild2DMipmaps
end interface
             
interface
character*8 function fgluNewQuadric ()
!MS$ATTRIBUTES STDCALL, ALIAS : '_gluNewQuadric@0' :: fgluNewQuadric
end function fgluNewQuadric
end interface
             
interface
subroutine  fgluDeleteQuadric  (state ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_gluDeleteQuadric@4' :: fgluDeleteQuadric
integer*4 state 
end subroutine fgluDeleteQuadric
end interface

interface
subroutine  fgluQuadricNormals  (quadObject ,normals ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_gluQuadricNormals@8' :: fgluQuadricNormals
integer*4 quadObject 
integer*4 normals 
end subroutine fgluQuadricNormals
end interface

interface
subroutine  fgluQuadricTexture  (quadObject ,textureCoords ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_gluQuadricTexture@8' :: fgluQuadricTexture
integer*4 quadObject 
character textureCoords 
end subroutine fgluQuadricTexture
end interface
             
interface
subroutine  fgluQuadricOrientation  (quadObject ,orientation ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_gluQuadricOrientation@8' :: fgluQuadricOrientation
integer*4 quadObject 
integer*4 orientation 
end subroutine fgluQuadricOrientation
end interface
             
interface
subroutine  fgluQuadricDrawStyle  (quadObject ,drawStyle ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_gluQuadricDrawStyle@8' :: fgluQuadricDrawStyle
integer*4 quadObject 
integer*4 drawStyle 
end subroutine fgluQuadricDrawStyle
end interface

interface
subroutine  fgluCylinder  (qobj ,baseRadius ,topRadius ,height ,slices ,stacks ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_gluCylinder@36' :: fgluCylinder
integer*4 qobj 
real*8 baseRadius 
real*8 topRadius 
real*8 height 
integer*4 slices 
integer*4 stacks 
end subroutine fgluCylinder
end interface

interface
subroutine  fgluDisk  (qobj ,innerRadius ,outerRadius ,slices ,loops ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_gluDisk@28' :: fgluDisk
integer*4 qobj 
real*8 innerRadius 
real*8 outerRadius 
integer*4 slices 
integer*4 loops 
end subroutine fgluDisk
end interface

interface
subroutine  fgluPartialDisk  (qobj ,innerRadius ,outerRadius ,slices ,loops ,startAngle ,sweepAngle ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_gluPartialDisk@44' :: fgluPartialDisk
integer*4 qobj 
real*8 innerRadius 
real*8 outerRadius 
integer*4 slices 
integer*4 loops 
real*8 startAngle 
real*8 sweepAngle 
end subroutine fgluPartialDisk
end interface

interface
subroutine  fgluSphere  (qobj ,radius ,slices ,stacks ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_gluSphere@20' :: fgluSphere
integer*4 qobj 
real*8 radius 
integer*4 slices 
integer*4 stacks 
end subroutine fgluSphere
end interface

interface
subroutine  fgluQuadricCallback  (qobj ,which ,fn ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_gluQuadricCallback@12' :: fgluQuadricCallback
integer*4 qobj 
integer*4 which 
character*8 fn 
end subroutine fgluQuadricCallback
end interface

interface
character*8 function fgluNewTess ()
!MS$ATTRIBUTES STDCALL, ALIAS : '_gluNewTess@0' :: fgluNewTess
end function fgluNewTess
end interface

interface
subroutine  fgluTessCallback  (tobj ,which , fn ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_gluTessCallback@12' :: fgluTessCallback
integer*4 tobj 
integer*4 which 
character*8 fn 
end subroutine fgluTessCallback
end interface

interface
subroutine  fgluDeleteTess  (tobj ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_gluDeleteTess@4' :: fgluDeleteTess
integer*4 tobj 
end subroutine fgluDeleteTess
end interface

interface
subroutine  fgluBeginPolygon  (tobj ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_gluBeginPolygon@4' :: fgluBeginPolygon
integer*4 tobj 
end subroutine fgluBeginPolygon
end interface

interface
subroutine  fgluEndPolygon  (tobj ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_gluEndPolygon@4' :: fgluEndPolygon
integer*4 tobj 
end subroutine fgluEndPolygon
end interface

interface
subroutine  fgluNextContour  (tobj ,type ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_gluNextContour@8' :: fgluNextContour
integer*4 tobj 
integer*4 type 
end subroutine fgluNextContour
end interface

interface
subroutine  fgluTessVertex  (tobj, v, data ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_gluTessVertex@12' :: fgluTessVertex
character*8 tobj 
real*8 v(3) 
character*8 data 
end subroutine fgluTessVertex
end interface

interface
character*8 function fgluNewNurbsRenderer ()
!MS$ATTRIBUTES STDCALL, ALIAS : '_gluNewNurbsRenderer@0' :: fgluNewNurbsRenderer
end function fgluNewNurbsRenderer
end interface

interface
subroutine  fgluDeleteNurbsRenderer  (nobj ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_gluDeleteNurbsRenderer@4' :: fgluDeleteNurbsRenderer
integer*4 nobj 
end subroutine fgluDeleteNurbsRenderer
end interface

interface
subroutine  fgluBeginSurface  (nobj ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_gluBeginSurface@4' :: fgluBeginSurface
integer*4 nobj 
end subroutine fgluBeginSurface
end interface

interface
subroutine  fgluBeginCurve  (nobj ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_gluBeginCurve@4' :: fgluBeginCurve
integer*4 nobj 
end subroutine fgluBeginCurve
end interface

interface
subroutine  fgluEndCurve  (nobj ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_gluEndCurve@4' :: fgluEndCurve
integer*4 nobj 
end subroutine fgluEndCurve
end interface

interface
subroutine  fgluEndSurface  (nobj ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_gluEndSurface@4' :: fgluEndSurface
integer*4 nobj 
end subroutine fgluEndSurface
end interface

interface
subroutine  fgluBeginTrim  (nobj ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_gluBeginTrim@4' :: fgluBeginTrim
integer*4 nobj 
end subroutine fgluBeginTrim
end interface

interface
subroutine  fgluEndTrim  (nobj ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_gluEndTrim@4' :: fgluEndTrim
integer*4 nobj 
end subroutine fgluEndTrim
end interface

interface
subroutine  fgluPwlCurve  (nobj ,count ,array ,stride ,type ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_gluPwlCurve@20' :: fgluPwlCurve
integer*4 nobj 
integer*4 count 
integer*4 array 
integer*4 stride 
integer*4 type 
end subroutine fgluPwlCurve
end interface

interface
subroutine  fgluNurbsCurve  (nobj ,nknots ,knot ,stride ,ctlarray ,order ,type ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_gluNurbsCurve@28' :: fgluNurbsCurve
integer*4 nobj 
integer*4 nknots 
integer*4 knot 
integer*4 stride 
integer*4 ctlarray 
integer*4 order 
integer*4 type 
end subroutine fgluNurbsCurve
end interface

interface
subroutine  fgluNurbsSurface  (nobj ,sknot_count ,sknot ,tknot_count ,tknot ,s_stride ,t_stride ,ctlarray ,sorder ,torder ,type ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_gluNurbsSurface@44' :: fgluNurbsSurface
integer*4 nobj 
integer*4 sknot_count 
integer*4 sknot 
integer*4 tknot_count 
integer*4 tknot 
integer*4 s_stride 
integer*4 t_stride 
integer*4 ctlarray 
integer*4 sorder 
integer*4 torder 
integer*4 type 
end subroutine fgluNurbsSurface
end interface

interface
subroutine  fgluLoadSamplingMatrices  (nobj , modelMatrix, projMatrix, viewport )
!MS$ATTRIBUTES STDCALL, ALIAS : '_gluLoadSamplingMatrices@16' :: fgluLoadSamplingMatrices
character*8 nobj 
real*4 modelMatrix(16) 
real*4 projMatrix(16) 
integer*4 viewport(4) 
end subroutine fgluLoadSamplingMatrices
end interface

interface
subroutine  fgluNurbsProperty  (nobj ,property ,value ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_gluNurbsProperty@12' :: fgluNurbsProperty
character*8 nobj 
integer*4 property 
real*4 value 
end subroutine fgluNurbsProperty
end interface

interface
subroutine  fgluGetNurbsProperty  (nobj ,property ,value ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_gluGetNurbsProperty@12' :: fgluGetNurbsProperty
integer*4 nobj 
integer*4 property 
integer*4 value 
end subroutine fgluGetNurbsProperty
end interface

interface
subroutine  fgluNurbsCallback  (nobj ,which , fn ) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_gluNurbsCallback@12' :: fgluNurbsCallback
character*8 nobj 
integer*4 which 
character*8 fn 
end subroutine fgluNurbsCallback
end interface

!                           **** FGLAUX ****        

! **************************************************************************
!  This file is not guaranteed by Microsoft to be error free. Every effort
!  has been made to ensure proper data-types and declarations, but no
!  testing has been performed using this include file. Additionally, some
!  API's though listed, may not be compatible with the Microsoft Fortran
!  language.
! **************************************************************************

interface
subroutine  fauxInitDisplayMode (dummy0) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxInitDisplayMode@4' :: fauxInitDisplayMode
integer*4 dummy0 
end subroutine fauxInitDisplayMode
end interface

interface
subroutine  fauxInitPosition (dummy0,dummy1,dummy2,dummy3) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxInitPosition@16' :: fauxInitPosition
integer dummy0 
integer dummy1 
integer dummy2 
integer dummy3 
end subroutine fauxInitPosition
end interface

interface
integer(4) function  fauxInitWindow (dummy0) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxInitWindowA@' :: fauxInitWindow
!MS$ATTRIBUTES REFERENCE :: dummy0
character*(*) dummy0 
end function fauxInitWindow
end interface

interface
subroutine  fauxCloseWindow (dummy0) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxCloseWindow@4' :: fauxCloseWindow
integer*4 dummy0 
end subroutine fauxCloseWindow
end interface

interface
subroutine  fauxQuit () 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxQuit@0' :: fauxQuit
end subroutine fauxQuit
end interface

interface
subroutine  fauxSwapBuffers () 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxSwapBuffers@0' :: fauxSwapBuffers
end subroutine fauxSwapBuffers
end interface

interface
subroutine  fauxMainLoop (dummy0) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxMainLoop@4' :: fauxMainLoop
integer*4  dummy0 
end subroutine fauxMainLoop
end interface

interface
subroutine  fauxExposeFunc (dummy0) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxExposeFunc@4' :: fauxExposeFunc
integer*4 dummy0 
end subroutine fauxExposeFunc
end interface

interface
subroutine  fauxReshapeFunc (dummy0) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxReshapeFunc@4' :: fauxReshapeFunc
integer*4 dummy0 
end subroutine fauxReshapeFunc
end interface

interface
subroutine  fauxIdleFunc (dummy0) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxIdleFunc@4' :: fauxIdleFunc
integer*4 dummy0 
end subroutine fauxIdleFunc
end interface

interface
subroutine  fauxKeyFunc (dummy0,dummy1) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxKeyFunc@8' :: fauxKeyFunc
integer(4)  dummy0 
integer*4 dummy1 
end subroutine fauxKeyFunc
end interface

interface
subroutine  fauxMouseFunc (dummy0,dummy1,dummy2) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxMouseFunc@12' :: fauxMouseFunc
integer dummy0 
integer dummy1 
external dummy2 
end subroutine fauxMouseFunc
end interface

interface
integer(4) function  fauxGetColorMapSize () 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxGetColorMapSize@0' :: fauxGetColorMapSize
end function fauxGetColorMapSize
end interface

interface
subroutine  fauxGetMouseLoc (dummy0,dummy1) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxGetMouseLoc@8' :: fauxGetMouseLoc
integer*4 dummy0 
integer*4 dummy1 
end subroutine fauxGetMouseLoc
end interface

interface
subroutine  fauxSetOneColor (dummy0,dummy1,dummy2,dummy3) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxSetOneColor@16' :: fauxSetOneColor
integer dummy0 
real*4 dummy1 
real*4 dummy2 
real*4 dummy3 
end subroutine fauxSetOneColor
end interface

interface
subroutine  fauxSetFogRamp (dummy0,dummy1) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxSetFogRamp@8' :: fauxSetFogRamp
integer dummy0 
integer dummy1 
end subroutine fauxSetFogRamp
end interface
             
interface
subroutine  fauxSetGreyRamp (dummy0) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxSetGreyRamp@4' :: fauxSetGreyRamp
integer*4 dummy0 
end subroutine fauxSetGreyRamp
end interface

interface
subroutine  fauxSetRGBMap (dummy0,dummy1) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxSetRGBMap@8' :: fauxSetRGBMap
integer dummy0 
integer*4 dummy1 
end subroutine fauxSetRGBMap
end interface

interface
subroutine  fauxCreateFont (dummy0) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxCreateFont@' :: fauxCreateFont
integer*4 dummy0 
end subroutine fauxCreateFont
end interface

interface
subroutine  fauxDrawStr (dummy0) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxDrawStrA@4' :: fauxDrawStr
integer*4 dummy0 
end subroutine fauxDrawStr
end interface

interface
subroutine  fauxWireSphere (dummy0) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxWireSphere@8' :: fauxWireSphere
real*8 dummy0 
end subroutine fauxWireSphere
end interface

interface
subroutine  fauxSolidSphere (dummy0) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxSolidSphere@8' :: fauxSolidSphere
real*8 dummy0 
end subroutine fauxSolidSphere
end interface

interface
subroutine  fauxWireCube (dummy0) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxWireCube@8' :: fauxWireCube
real*8 dummy0 
end subroutine fauxWireCube
end interface

interface
subroutine  fauxSolidCube (dummy0) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxSolidCube@8' :: fauxSolidCube
real*8 dummy0 
end subroutine fauxSolidCube
end interface

interface
subroutine  fauxWireBox (dummy0,dummy1,dummy2) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxWireBox@24' :: fauxWireBox
real*8 dummy0 
real*8 dummy1 
real*8 dummy2 
end subroutine fauxWireBox
end interface

interface
subroutine  fauxSolidBox (dummy0,dummy1,dummy2) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxSolidBox@24' :: fauxSolidBox
real*8 dummy0 
real*8 dummy1 
real*8 dummy2 
end subroutine fauxSolidBox
end interface

interface
subroutine  fauxWireTorus (dummy0,dummy1) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxWireTorus@16' :: fauxWireTorus
real*8 dummy0 
real*8 dummy1 
end subroutine fauxWireTorus
end interface

interface
subroutine  fauxSolidTorus (dummy0,dummy1) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxSolidTorus@16' :: fauxSolidTorus
real*8 dummy0 
real*8 dummy1 
end subroutine fauxSolidTorus
end interface

interface
subroutine  fauxWireCylinder (dummy0,dummy1) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxWireCylinder@16' :: fauxWireCylinder
real*8 dummy0 
real*8 dummy1 
end subroutine fauxWireCylinder
end interface

interface
subroutine  fauxSolidCylinder (dummy0,dummy1) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxSolidCylinder@16' :: fauxSolidCylinder
real*8 dummy0 
real*8 dummy1 
end subroutine fauxSolidCylinder
end interface

interface
subroutine  fauxWireIcosahedron (dummy0) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxWireIcosahedron@8' :: fauxWireIcosahedron
real*8 dummy0 
end subroutine fauxWireIcosahedron
end interface
             
interface
subroutine  fauxSolidIcosahedron (dummy0) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxSolidIcosahedron@8' :: fauxSolidIcosahedron
real*8 dummy0 
end subroutine fauxSolidIcosahedron
end interface

interface
subroutine  fauxWireOctahedron (dummy0) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxWireOctahedron@8' :: fauxWireOctahedron
real*8 dummy0 
end subroutine fauxWireOctahedron
end interface

interface
subroutine  fauxSolidOctahedron (dummy0) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxSolidOctahedron@8' :: fauxSolidOctahedron
real*8 dummy0 
end subroutine fauxSolidOctahedron
end interface

interface
subroutine  fauxWireTetrahedron (dummy0) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxWireTetrahedron@8' :: fauxWireTetrahedron
real*8 dummy0 
end subroutine fauxWireTetrahedron
end interface

interface
subroutine  fauxSolidTetrahedron (dummy0) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxSolidTetrahedron@8' :: fauxSolidTetrahedron
real*8 dummy0 
end subroutine fauxSolidTetrahedron
end interface

interface
subroutine  fauxWireDodecahedron (dummy0) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxWireDodecahedron@8' :: fauxWireDodecahedron
real*8 dummy0 
end subroutine fauxWireDodecahedron
end interface

interface
subroutine  fauxSolidDodecahedron (dummy0) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxSolidDodecahedron@8' :: fauxSolidDodecahedron
real*8 dummy0 
end subroutine fauxSolidDodecahedron
end interface

interface
subroutine  fauxWireCone (dummy0,dummy1) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxWireCone@16' :: fauxWireCone
real*8 dummy0 
real*8 dummy1 
end subroutine fauxWireCone
end interface

interface
subroutine  fauxSolidCone (dummy0,dummy1) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxSolidCone@16' :: fauxSolidCone
real*8 dummy0 
real*8 dummy1 
end subroutine fauxSolidCone
end interface

interface
subroutine  fauxWireTeapot (dummy0) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxWireTeapot@8' :: fauxWireTeapot
real*8 dummy0 
end subroutine fauxWireTeapot
end interface

interface
subroutine  fauxSolidTeapot (dummy0) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxSolidTeapot@8' :: fauxSolidTeapot
real*8 dummy0 
end subroutine fauxSolidTeapot
end interface

! 
! ** Window specific functions
! ** hwnd, hdc, and hglrc valid after auxInitWindow()
! 

interface
integer(4) function  fauxGetHWND (dummy0) 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxGetHWND@' :: fauxGetHWND
integer*4 dummy0 
end function fauxGetHWND
end interface

interface
integer(4) function  fauxGetHDC () 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxGetHDC@0' :: fauxGetHDC
end function fauxGetHDC
end interface

interface
integer(4) function  fauxGetHGLRC () 
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxGetHGLRC@' :: fauxGetHGLRC
end function fauxGetHGLRC
end interface

!*
!** Viewperf support functions 
!*
!* Display Mode Selection Criteria 

interface
subroutine fauxInitDisplayModePolicy (dummy0)
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxInitDisplayModePolicy@4' :: fauxInitDisplayModePolicy
integer(4)  dummy0
end subroutine fauxInitDisplayModePolicy
end interface

interface
integer(4)  function fauxInitDisplayModeID (dummy0)
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxInitDisplayModeID@4' :: fauxInitDisplayModeID
integer(4)  dummy0
end function fauxInitDisplayModeID
end interface

interface
integer(4) function fauxGetDisplayModePolicy ()
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxGetDisplayModePolicy@0' :: fauxGetDisplayModePolicy
end function fauxGetDisplayModePolicy
end interface

interface
integer(4) function fauxGetDisplayModeID ()
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxGetDisplayModeID@0' :: fauxGetDisplayModeID
end function fauxGetDisplayModeID
end interface

interface
integer(4) function fauxGetDisplayMode ()
!MS$ATTRIBUTES STDCALL, ALIAS : '_auxGetDisplayMode@0' :: fauxGetDisplayMode
end function fauxGetDisplayMode
end interface

end module opengl
