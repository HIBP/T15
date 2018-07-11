! **************************************************************************
!  This file is not guaranteed by Microsoft to be error free. Every effort
!  has been made to ensure proper data-types and declarations, but no
!  testing has been performed using this include file. Additionally, some
!  API's though listed, may not be compatible with the Microsoft Fortran
!  language.
! **************************************************************************
module openglty

!                           *****OPENGL*****
!                           *******FGL******

!     Procedure declarations, constant definitions and macros for the OpenGL
!     component.
! 

! 
! ** Copyright 1991-1993, Silicon Graphics, Inc.
! ** All Rights Reserved.
! ** 
! ** This is UNPUBLISHED PROPRIETARY SOURCE CODE of Silicon Graphics, Inc.;
! ** the contents of this file may not be disclosed to third parties, copied or
! ** duplicated in any form, in whole or in part, without the prior written
! ** permission of Silicon Graphics, Inc.
! ** 
! ** RESTRICTED RIGHTS LEGEND:
! ** Use, duplication or disclosure by the Government is subject to restrictions
! ** as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data
! ** and Computer Software clause at DFARS 252.227-7013, and/or in similar or
! ** successor clauses in the FAR, DOD or NASA FAR Supplement. Unpublished -
! ** rights reserved under the Copyright Laws of the United States.
!

! NULL pointer
character*8, parameter, public :: FGL_NULL = char(0) // char(0) // char(0) // char(0) // char(0) // char(0) // char(0) // char(0)

!   AccumOp   
integer, parameter, public :: GL_ACCUM  =  #0100 
integer, parameter, public :: GL_LOAD   =  #0101 
integer, parameter, public :: GL_RETURN =  #0102 
integer, parameter, public :: GL_MULT   =  #0103 
integer, parameter, public :: GL_ADD    =  #0104 

!   AlphaFunction   
integer, parameter, public :: GL_NEVER      =  #0200 
integer, parameter, public :: GL_LESS       =  #0201 
integer, parameter, public :: GL_EQUAL      =  #0202 
integer, parameter, public :: GL_LEQUAL     =  #0203 
integer, parameter, public :: GL_GREATER    =  #0204 
integer, parameter, public :: GL_NOTEQUAL   =  #0205 
integer, parameter, public :: GL_GEQUAL     =  #0206 
integer, parameter, public :: GL_ALWAYS     =  #0207 

!   AttribMask   
integer, parameter, public :: GL_CURRENT_BIT            =  #00000001 
integer, parameter, public :: GL_POINT_BIT              =  #00000002 
integer, parameter, public :: GL_LINE_BIT               =  #00000004 
integer, parameter, public :: GL_POLYGON_BIT            =  #00000008 
integer, parameter, public :: GL_POLYGON_STIPPLE_BIT    =  #00000010 
integer, parameter, public :: GL_PIXEL_MODE_BIT         =  #00000020 
integer, parameter, public :: GL_LIGHTING_BIT           =  #00000040 
integer, parameter, public :: GL_FOG_BIT                =  #00000080 
integer, parameter, public :: GL_DEPTH_BUFFER_BIT       =  #00000100 
integer, parameter, public :: GL_ACCUM_BUFFER_BIT       =  #00000200 
integer, parameter, public :: GL_STENCIL_BUFFER_BIT     =  #00000400 
integer, parameter, public :: GL_VIEWPORT_BIT           =  #00000800 
integer, parameter, public :: GL_TRANSFORM_BIT          =  #00001000 
integer, parameter, public :: GL_ENABLE_BIT             =  #00002000 
integer, parameter, public :: GL_COLOR_BUFFER_BIT       =  #00004000 
integer, parameter, public :: GL_HINT_BIT               =  #00008000 
integer, parameter, public :: GL_EVAL_BIT               =  #00010000 
integer, parameter, public :: GL_LIST_BIT               =  #00020000 
integer, parameter, public :: GL_TEXTURE_BIT            =  #00040000 
integer, parameter, public :: GL_SCISSOR_BIT            =  #00080000 
integer, parameter, public :: GL_ALL_ATTRIB_BITS        =  #000fffff 

!   BeginMode   
integer, parameter, public :: GL_POINTS         =  #0000 
integer, parameter, public :: GL_LINES          =  #0001 
integer, parameter, public :: GL_LINE_LOOP      =  #0002 
integer, parameter, public :: GL_LINE_STRIP     =  #0003 
integer, parameter, public :: GL_TRIANGLES      =  #0004 
integer, parameter, public :: GL_TRIANGLE_STRIP =  #0005 
integer, parameter, public :: GL_TRIANGLE_FAN   =  #0006 
integer, parameter, public :: GL_QUADS          =  #0007 
integer, parameter, public :: GL_QUAD_STRIP     =  #0008 
integer, parameter, public :: GL_POLYGON        =  #0009 

!   BlendingFactorDest   
integer, parameter, public :: GL_ZERO                   =  0 
integer, parameter, public :: GL_ONE                    =  1 
integer, parameter, public :: GL_SRC_COLOR              =  #0300 
integer, parameter, public :: GL_ONE_MINUS_SRC_COLOR    =  #0301 
integer, parameter, public :: GL_SRC_ALPHA              =  #0302 
integer, parameter, public :: GL_ONE_MINUS_SRC_ALPHA    =  #0303 
integer, parameter, public :: GL_DST_ALPHA              =  #0304 
integer, parameter, public :: GL_ONE_MINUS_DST_ALPHA    =  #0305 

!   BlendingFactorSrc   
!        GL_ZERO   
!        GL_ONE   
integer, parameter, public :: GL_DST_COLOR              =  #0306 
integer, parameter, public :: GL_ONE_MINUS_DST_COLOR    =  #0307 
integer, parameter, public :: GL_SRC_ALPHA_SATURATE     =  #0308 
!        GL_SRC_ALPHA   
!        GL_ONE_MINUS_SRC_ALPHA   
!        GL_DST_ALPHA   
!        GL_ONE_MINUS_DST_ALPHA   

!   Boolean   
integer, parameter, public :: GL_TRUE   = 1 
integer, parameter, public :: GL_FALSE  = 0 
!   ClearBufferMask   
!        GL_COLOR_BUFFER_BIT   
!        GL_ACCUM_BUFFER_BIT   
!        GL_STENCIL_BUFFER_BIT   
!        GL_DEPTH_BUFFER_BIT   

!   ClipPlaneName   
integer, parameter, public :: GL_CLIP_PLANE0 =  #3000 
integer, parameter, public :: GL_CLIP_PLANE1 =  #3001 
integer, parameter, public :: GL_CLIP_PLANE2 =  #3002 
integer, parameter, public :: GL_CLIP_PLANE3 =  #3003 
integer, parameter, public :: GL_CLIP_PLANE4 =  #3004 
integer, parameter, public :: GL_CLIP_PLANE5 =  #3005 

!   ColorMaterialFace   
!        GL_FRONT   
!        GL_BACK   
!        GL_FRONT_AND_BACK   

!   ColorMaterialParameter   
!        GL_AMBIENT   
!        GL_DIFFUSE   
!        GL_SPECULAR   
!        GL_EMISSION   
!        GL_AMBIENT_AND_DIFFUSE   

!   CullFaceMode   
!        GL_FRONT   
!        GL_BACK   
!        GL_FRONT_AND_BACK   

!   DepthFunction   
!        GL_NEVER   
!        GL_LESS   
!        GL_EQUAL   
!        GL_LEQUAL   
!        GL_GREATER   
!        GL_NOTEQUAL   
!        GL_GEQUAL   
!        GL_ALWAYS   

!   DrawBufferMode   
integer, parameter, public :: GL_NONE           =  0 
integer, parameter, public :: GL_FRONT_LEFT     =  #0400 
integer, parameter, public :: GL_FRONT_RIGHT    =  #0401 
integer, parameter, public :: GL_BACK_LEFT      =  #0402 
integer, parameter, public :: GL_BACK_RIGHT     =  #0403 
integer, parameter, public :: GL_FRONT          =  #0404 
integer, parameter, public :: GL_BACK           =  #0405 
integer, parameter, public :: GL_LEFT           =  #0406 
integer, parameter, public :: GL_RIGHT          =  #0407 
integer, parameter, public :: GL_FRONT_AND_BACK =  #0408 
integer, parameter, public :: GL_AUX0           =  #0409 
integer, parameter, public :: GL_AUX1           =  #040A 
integer, parameter, public :: GL_AUX2           =  #040B 
integer, parameter, public :: GL_AUX3           =  #040C 

!   Enable   
!        GL_FOG   
!        GL_LIGHTING   
!        GL_TEXTURE_1D   
!        GL_TEXTURE_2D   
!        GL_LINE_STIPPLE   
!        GL_POLYGON_STIPPLE   
!        GL_CULL_FACE   
!        GL_ALPHA_TEST   
!        GL_BLEND   
!        GL_LOGIC_OP   
!        GL_DITHER   
!        GL_STENCIL_TEST   
!        GL_DEPTH_TEST   
!        GL_CLIP_PLANE0   
!        GL_CLIP_PLANE1   
!        GL_CLIP_PLANE2   
!        GL_CLIP_PLANE3   
!        GL_CLIP_PLANE4   
!        GL_CLIP_PLANE5   
!        GL_LIGHT0   
!        GL_LIGHT1   
!        GL_LIGHT2   
!        GL_LIGHT3   
!        GL_LIGHT4   
!        GL_LIGHT5   
!        GL_LIGHT6   
!        GL_LIGHT7   
!        GL_TEXTURE_GEN_S   
!        GL_TEXTURE_GEN_T   
!        GL_TEXTURE_GEN_R   
!        GL_TEXTURE_GEN_Q   
!        GL_MAP1_VERTEX_3   
!        GL_MAP1_VERTEX_4   
!        GL_MAP1_COLOR_4   
!        GL_MAP1_INDEX   
!        GL_MAP1_NORMAL   
!        GL_MAP1_TEXTURE_COORD_1   
!        GL_MAP1_TEXTURE_COORD_2   
!        GL_MAP1_TEXTURE_COORD_3   
!        GL_MAP1_TEXTURE_COORD_4   
!        GL_MAP2_VERTEX_3   
!        GL_MAP2_VERTEX_4   
!        GL_MAP2_COLOR_4   
!        GL_MAP2_INDEX   
!        GL_MAP2_NORMAL   
!        GL_MAP2_TEXTURE_COORD_1   
!        GL_MAP2_TEXTURE_COORD_2   
!        GL_MAP2_TEXTURE_COORD_3   
!        GL_MAP2_TEXTURE_COORD_4   
!        GL_POINT_SMOOTH   
!        GL_LINE_SMOOTH   
!        GL_POLYGON_SMOOTH   
!        GL_SCISSOR_TEST   
!        GL_COLOR_MATERIAL   
!        GL_NORMALIZE   
!        GL_AUTO_NORMAL   

!   ErrorCode   
integer, parameter, public :: GL_NO_ERROR           = 0
integer, parameter, public :: GL_INVALID_ENUM       =  #0500 
integer, parameter, public :: GL_INVALID_VALUE      =  #0501 
integer, parameter, public :: GL_INVALID_OPERATION  =  #0502 
integer, parameter, public :: GL_STACK_OVERFLOW     =  #0503 
integer, parameter, public :: GL_STACK_UNDERFLOW    =  #0504 
integer, parameter, public :: GL_OUT_OF_MEMORY      =  #0505 

!   FeedBackMode   
integer, parameter, public :: GL_2D                 =  #0600 
integer, parameter, public :: GL_3D                 =  #0601 
integer, parameter, public :: GL_3D_COLOR           =  #0602 
integer, parameter, public :: GL_3D_COLOR_TEXTURE   =  #0603 
integer, parameter, public :: GL_4D_COLOR_TEXTURE   =  #0604 

!   FeedBackToken   
integer, parameter, public :: GL_PASS_THROUGH_TOKEN =  #0700 
integer, parameter, public :: GL_POINT_TOKEN        =  #0701 
integer, parameter, public :: GL_LINE_TOKEN         =  #0702 
integer, parameter, public :: GL_POLYGON_TOKEN      =  #0703 
integer, parameter, public :: GL_BITMAP_TOKEN       =  #0704 
integer, parameter, public :: GL_DRAW_PIXEL_TOKEN   =  #0705 
integer, parameter, public :: GL_COPY_PIXEL_TOKEN   =  #0706 
integer, parameter, public :: GL_LINE_RESET_TOKEN   =  #0707 

!   FogMode   
!        GL_LINEAR   
integer, parameter, public :: GL_EXP    =  #0800 
integer, parameter, public :: GL_EXP2   =  #0801 

!   FogParameter   
!        GL_FOG_COLOR   
!        GL_FOG_DENSITY   
!        GL_FOG_END   
!        GL_FOG_INDEX   
!        GL_FOG_MODE   
!        GL_FOG_START   

!   FrontFaceDirection   
integer, parameter, public :: GL_CW     =  #0900 
integer, parameter, public :: GL_CCW    =  #0901 

!   GetMapTarget   
integer, parameter, public :: GL_COEFF  =  #0A00 
integer, parameter, public :: GL_ORDER  =  #0A01 
integer, parameter, public :: GL_DOMAIN =  #0A02 

!   GetPixelMap   
!        GL_PIXEL_MAP_I_TO_I   
!        GL_PIXEL_MAP_S_TO_S   
!        GL_PIXEL_MAP_I_TO_R   
!        GL_PIXEL_MAP_I_TO_G   
!        GL_PIXEL_MAP_I_TO_B   
!        GL_PIXEL_MAP_I_TO_A   
!        GL_PIXEL_MAP_R_TO_R   
!        GL_PIXEL_MAP_G_TO_G   
!        GL_PIXEL_MAP_B_TO_B   
!        GL_PIXEL_MAP_A_TO_A   

!   GetTarget   
integer, parameter, public :: GL_CURRENT_COLOR                  =  #0B00 
integer, parameter, public :: GL_CURRENT_INDEX                  =  #0B01 
integer, parameter, public :: GL_CURRENT_NORMAL                 =  #0B02 
integer, parameter, public :: GL_CURRENT_TEXTURE_COORDS         =  #0B03 
integer, parameter, public :: GL_CURRENT_RASTER_COLOR           =  #0B04 
integer, parameter, public :: GL_CURRENT_RASTER_INDEX           =  #0B05 
integer, parameter, public :: GL_CURRENT_RASTER_TEXTURE_COORD   =  #0B06 
integer, parameter, public :: GL_CURRENT_RASTER_POSITION        =  #0B07 
integer, parameter, public :: GL_CURRENT_RASTER_POSITION_VALD   =  #0B08 
integer, parameter, public :: GL_CURRENT_RASTER_DISTANCE        =  #0B09 
integer, parameter, public :: GL_POINT_SMOOTH                   =  #0B10 
integer, parameter, public :: GL_POINT_SIZE                     =  #0B11 
integer, parameter, public :: GL_POINT_SIZE_RANGE               =  #0B12 
integer, parameter, public :: GL_POINT_SIZE_GRANULARITY         =  #0B13 
integer, parameter, public :: GL_LINE_SMOOTH                    =  #0B20 
integer, parameter, public :: GL_LINE_WIDTH                     =  #0B21 
integer, parameter, public :: GL_LINE_WIDTH_RANGE               =  #0B22 
integer, parameter, public :: GL_LINE_WIDTH_GRANULARITY         =  #0B23 
integer, parameter, public :: GL_LINE_STIPPLE                   =  #0B24 
integer, parameter, public :: GL_LINE_STIPPLE_PATTERN           =  #0B25 
integer, parameter, public :: GL_LINE_STIPPLE_REPEAT            =  #0B26 
integer, parameter, public :: GL_LIST_MODE                      =  #0B30 
integer, parameter, public :: GL_MAX_LIST_NESTING               =  #0B31 
integer, parameter, public :: GL_LIST_BASE                      =  #0B32 
integer, parameter, public :: GL_LIST_INDEX                     =  #0B33 
integer, parameter, public :: GL_POLYGON_MODE                   =  #0B40 
integer, parameter, public :: GL_POLYGON_SMOOTH                 =  #0B41 
integer, parameter, public :: GL_POLYGON_STIPPLE                =  #0B42 
integer, parameter, public :: GL_EDGE_FLAG                      =  #0B43 
integer, parameter, public :: GL_CULL_FACE                      =  #0B44 
integer, parameter, public :: GL_CULL_FACE_MODE                 =  #0B45 
integer, parameter, public :: GL_FRONT_FACE                     =  #0B46 
integer, parameter, public :: GL_LIGHTING                       =  #0B50 
integer, parameter, public :: GL_LIGHT_MODEL_LOCAL_VIEWER       =  #0B51 
integer, parameter, public :: GL_LIGHT_MODEL_TWO_SIDE           =  #0B52 
integer, parameter, public :: GL_LIGHT_MODEL_AMBIENT            =  #0B53 
integer, parameter, public :: GL_SHADE_MODEL                    =  #0B54 
integer, parameter, public :: GL_COLOR_MATERIAL_FACE            =  #0B55 
integer, parameter, public :: GL_COLOR_MATERIAL_PARAMETER       =  #0B56 
integer, parameter, public :: GL_COLOR_MATERIAL                 =  #0B57 
integer, parameter, public :: GL_FOG                            =  #0B60 
integer, parameter, public :: GL_FOG_INDEX                      =  #0B61 
integer, parameter, public :: GL_FOG_DENSITY                    =  #0B62 
integer, parameter, public :: GL_FOG_START                      =  #0B63 
integer, parameter, public :: GL_FOG_END                        =  #0B64 
integer, parameter, public :: GL_FOG_MODE                       =  #0B65 
integer, parameter, public :: GL_FOG_COLOR                      =  #0B66 
integer, parameter, public :: GL_DEPTH_RANGE                    =  #0B70 
integer, parameter, public :: GL_DEPTH_TEST                     =  #0B71 
integer, parameter, public :: GL_DEPTH_WRITEMASK                =  #0B72 
integer, parameter, public :: GL_DEPTH_CLEAR_VALUE              =  #0B73 
integer, parameter, public :: GL_DEPTH_FUNC                     =  #0B74 
integer, parameter, public :: GL_ACCUM_CLEAR_VALUE              =  #0B80 
integer, parameter, public :: GL_STENCIL_TEST                   =  #0B90 
integer, parameter, public :: GL_STENCIL_CLEAR_VALUE            =  #0B91 
integer, parameter, public :: GL_STENCIL_FUNC                   =  #0B92 
integer, parameter, public :: GL_STENCIL_VALUE_MASK             =  #0B93 
integer, parameter, public :: GL_STENCIL_FAIL                   =  #0B94 
integer, parameter, public :: GL_STENCIL_PASS_DEPTH_FAIL        =  #0B95 
integer, parameter, public :: GL_STENCIL_PASS_DEPTH_PASS        =  #0B96 
integer, parameter, public :: GL_STENCIL_REF                    =  #0B97 
integer, parameter, public :: GL_STENCIL_WRITEMASK              =  #0B98 
integer, parameter, public :: GL_MATRIX_MODE                    =  #0BA0 
integer, parameter, public :: GL_NORMALIZE                      =  #0BA1 
integer, parameter, public :: GL_VIEWPORT                       =  #0BA2 
integer, parameter, public :: GL_MODELVIEW_STACK_DEPTH          =  #0BA3 
integer, parameter, public :: GL_PROJECTION_STACK_DEPTH         =  #0BA4 
integer, parameter, public :: GL_TEXTURE_STACK_DEPTH            =  #0BA5 
integer, parameter, public :: GL_MODELVIEW_MATRIX               =  #0BA6 
integer, parameter, public :: GL_PROJECTION_MATRIX              =  #0BA7 
integer, parameter, public :: GL_TEXTURE_MATRIX                 =  #0BA8 
integer, parameter, public :: GL_ATTRIB_STACK_DEPTH             =  #0BB0 
integer, parameter, public :: GL_ALPHA_TEST                     =  #0BC0 
integer, parameter, public :: GL_ALPHA_TEST_FUNC                =  #0BC1 
integer, parameter, public :: GL_ALPHA_TEST_REF                 =  #0BC2 
integer, parameter, public :: GL_DITHER                         =  #0BD0 
integer, parameter, public :: GL_BLEND_DST                      =  #0BE0 
integer, parameter, public :: GL_BLEND_SRC                      =  #0BE1 
integer, parameter, public :: GL_BLEND                          =  #0BE2 
integer, parameter, public :: GL_LOGIC_OP_MODE                  =  #0BF0 
integer, parameter, public :: GL_LOGIC_OP                       =  #0BF1 
integer, parameter, public :: GL_AUX_BUFFERS                    =  #0C00 
integer, parameter, public :: GL_DRAW_BUFFER                    =  #0C01 
integer, parameter, public :: GL_READ_BUFFER                    =  #0C02 
integer, parameter, public :: GL_SCISSOR_BOX                    =  #0C10 
integer, parameter, public :: GL_SCISSOR_TEST                   =  #0C11 
integer, parameter, public :: GL_INDEX_CLEAR_VALUE              =  #0C20 
integer, parameter, public :: GL_INDEX_WRITEMASK                =  #0C21 
integer, parameter, public :: GL_COLOR_CLEAR_VALUE              =  #0C22 
integer, parameter, public :: GL_COLOR_WRITEMASK                =  #0C23 
integer, parameter, public :: GL_INDEX_MODE                     =  #0C30 
integer, parameter, public :: GL_RGBA_MODE                      =  #0C31 
integer, parameter, public :: GL_DOUBLEBUFFER                   =  #0C32 
integer, parameter, public :: GL_STEREO                         =  #0C33 
integer, parameter, public :: GL_RENDER_MODE                    =  #0C40 
integer, parameter, public :: GL_PERSPECTIVE_CORRECTION_HINT    =  #0C50 
integer, parameter, public :: GL_POINT_SMOOTH_HINT              =  #0C51 
integer, parameter, public :: GL_LINE_SMOOTH_HINT               =  #0C52 
integer, parameter, public :: GL_POLYGON_SMOOTH_HINT            =  #0C53 
integer, parameter, public :: GL_FOG_HINT                       =  #0C54 
integer, parameter, public :: GL_TEXTURE_GEN_S                  =  #0C60 
integer, parameter, public :: GL_TEXTURE_GEN_T                  =  #0C61 
integer, parameter, public :: GL_TEXTURE_GEN_R                  =  #0C62 
integer, parameter, public :: GL_TEXTURE_GEN_Q                  =  #0C63 
integer, parameter, public :: GL_PIXEL_MAP_I_TO_I               =  #0C70 
integer, parameter, public :: GL_PIXEL_MAP_S_TO_S               =  #0C71 
integer, parameter, public :: GL_PIXEL_MAP_I_TO_R               =  #0C72 
integer, parameter, public :: GL_PIXEL_MAP_I_TO_G               =  #0C73 
integer, parameter, public :: GL_PIXEL_MAP_I_TO_B               =  #0C74 
integer, parameter, public :: GL_PIXEL_MAP_I_TO_A               =  #0C75 
integer, parameter, public :: GL_PIXEL_MAP_R_TO_R               =  #0C76 
integer, parameter, public :: GL_PIXEL_MAP_G_TO_G               =  #0C77 
integer, parameter, public :: GL_PIXEL_MAP_B_TO_B               =  #0C78 
integer, parameter, public :: GL_PIXEL_MAP_A_TO_A               =  #0C79 
integer, parameter, public :: GL_PIXEL_MAP_I_TO_I_SIZE          =  #0CB0 
integer, parameter, public :: GL_PIXEL_MAP_S_TO_S_SIZE          =  #0CB1 
integer, parameter, public :: GL_PIXEL_MAP_I_TO_R_SIZE          =  #0CB2 
integer, parameter, public :: GL_PIXEL_MAP_I_TO_G_SIZE          =  #0CB3 
integer, parameter, public :: GL_PIXEL_MAP_I_TO_B_SIZE          =  #0CB4 
integer, parameter, public :: GL_PIXEL_MAP_I_TO_A_SIZE          =  #0CB5 
integer, parameter, public :: GL_PIXEL_MAP_R_TO_R_SIZE          =  #0CB6 
integer, parameter, public :: GL_PIXEL_MAP_G_TO_G_SIZE          =  #0CB7 
integer, parameter, public :: GL_PIXEL_MAP_B_TO_B_SIZE          =  #0CB8 
integer, parameter, public :: GL_PIXEL_MAP_A_TO_A_SIZE          =  #0CB9 
integer, parameter, public :: GL_UNPACK_SWAP_BYTES              =  #0CF0 
integer, parameter, public :: GL_UNPACK_LSB_FIRST               =  #0CF1 
integer, parameter, public :: GL_UNPACK_ROW_LENGTH              =  #0CF2 
integer, parameter, public :: GL_UNPACK_SKIP_ROWS               =  #0CF3 
integer, parameter, public :: GL_UNPACK_SKIP_PIXELS             =  #0CF4 
integer, parameter, public :: GL_UNPACK_ALIGNMENT               =  #0CF5 
integer, parameter, public :: GL_PACK_SWAP_BYTES                =  #0D00 
integer, parameter, public :: GL_PACK_LSB_FIRST                 =  #0D01 
integer, parameter, public :: GL_PACK_ROW_LENGTH                =  #0D02 
integer, parameter, public :: GL_PACK_SKIP_ROWS                 =  #0D03 
integer, parameter, public :: GL_PACK_SKIP_PIXELS               =  #0D04 
integer, parameter, public :: GL_PACK_ALIGNMENT                 =  #0D05 
integer, parameter, public :: GL_MAP_COLOR                      =  #0D10 
integer, parameter, public :: GL_MAP_STENCIL                    =  #0D11 
integer, parameter, public :: GL_INDEX_SHIFT                    =  #0D12 
integer, parameter, public :: GL_INDEX_OFFSET                   =  #0D13 
integer, parameter, public :: GL_RED_SCALE                      =  #0D14 
integer, parameter, public :: GL_RED_BIAS                       =  #0D15 
integer, parameter, public :: GL_ZOOM_X                         =  #0D16 
integer, parameter, public :: GL_ZOOM_Y                         =  #0D17 
integer, parameter, public :: GL_GREEN_SCALE                    =  #0D18 
integer, parameter, public :: GL_GREEN_BIAS                     =  #0D19 
integer, parameter, public :: GL_BLUE_SCALE                     =  #0D1A 
integer, parameter, public :: GL_BLUE_BIAS                      =  #0D1B 
integer, parameter, public :: GL_ALPHA_SCALE                    =  #0D1C 
integer, parameter, public :: GL_ALPHA_BIAS                     =  #0D1D 
integer, parameter, public :: GL_DEPTH_SCALE                    =  #0D1E 
integer, parameter, public :: GL_DEPTH_BIAS                     =  #0D1F 
integer, parameter, public :: GL_MAX_EVAL_ORDER                 =  #0D30 
integer, parameter, public :: GL_MAX_LIGHTS                     =  #0D31 
integer, parameter, public :: GL_MAX_CLIP_PLANES                =  #0D32 
integer, parameter, public :: GL_MAX_TEXTURE_SIZE               =  #0D33 
integer, parameter, public :: GL_MAX_PIXEL_MAP_TABLE            =  #0D34 
integer, parameter, public :: GL_MAX_ATTRIB_STACK_DEPTH         =  #0D35 
integer, parameter, public :: GL_MAX_MODELVIEW_STACK_DEPTH      =  #0D36 
integer, parameter, public :: GL_MAX_NAME_STACK_DEPTH           =  #0D37 
integer, parameter, public :: GL_MAX_PROJECTION_STACK_DEPTH     =  #0D38 
integer, parameter, public :: GL_MAX_TEXTURE_STACK_DEPTH        =  #0D39 
integer, parameter, public :: GL_MAX_VIEWPORT_DIMS              =  #0D3A 
integer, parameter, public :: GL_SUBPIXEL_BITS                  =  #0D50 
integer, parameter, public :: GL_INDEX_BITS                     =  #0D51 
integer, parameter, public :: GL_RED_BITS                       =  #0D52 
integer, parameter, public :: GL_GREEN_BITS                     =  #0D53 
integer, parameter, public :: GL_BLUE_BITS                      =  #0D54 
integer, parameter, public :: GL_ALPHA_BITS                     =  #0D55 
integer, parameter, public :: GL_DEPTH_BITS                     =  #0D56 
integer, parameter, public :: GL_STENCIL_BITS                   =  #0D57 
integer, parameter, public :: GL_ACCUM_RED_BITS                 =  #0D58 
integer, parameter, public :: GL_ACCUM_GREEN_BITS               =  #0D59 
integer, parameter, public :: GL_ACCUM_BLUE_BITS                =  #0D5A 
integer, parameter, public :: GL_ACCUM_ALPHA_BITS               =  #0D5B 
integer, parameter, public :: GL_NAME_STACK_DEPTH               =  #0D70 
integer, parameter, public :: GL_AUTO_NORMAL                    =  #0D80 
integer, parameter, public :: GL_MAP1_COLOR_4                   =  #0D90 
integer, parameter, public :: GL_MAP1_INDEX                     =  #0D91 
integer, parameter, public :: GL_MAP1_NORMAL                    =  #0D92 
integer, parameter, public :: GL_MAP1_TEXTURE_COORD_1           =  #0D93 
integer, parameter, public :: GL_MAP1_TEXTURE_COORD_2           =  #0D94 
integer, parameter, public :: GL_MAP1_TEXTURE_COORD_3           =  #0D95 
integer, parameter, public :: GL_MAP1_TEXTURE_COORD_4           =  #0D96 
integer, parameter, public :: GL_MAP1_VERTEX_3                  =  #0D97 
integer, parameter, public :: GL_MAP1_VERTEX_4                  =  #0D98 
integer, parameter, public :: GL_MAP2_COLOR_4                   =  #0DB0 
integer, parameter, public :: GL_MAP2_INDEX                     =  #0DB1 
integer, parameter, public :: GL_MAP2_NORMAL                    =  #0DB2 
integer, parameter, public :: GL_MAP2_TEXTURE_COORD_1           =  #0DB3 
integer, parameter, public :: GL_MAP2_TEXTURE_COORD_2           =  #0DB4 
integer, parameter, public :: GL_MAP2_TEXTURE_COORD_3           =  #0DB5 
integer, parameter, public :: GL_MAP2_TEXTURE_COORD_4           =  #0DB6 
integer, parameter, public :: GL_MAP2_VERTEX_3                  =  #0DB7 
integer, parameter, public :: GL_MAP2_VERTEX_4                  =  #0DB8 
integer, parameter, public :: GL_MAP1_GRID_DOMAIN               =  #0DD0 
integer, parameter, public :: GL_MAP1_GRID_SEGMENTS             =  #0DD1 
integer, parameter, public :: GL_MAP2_GRID_DOMAIN               =  #0DD2 
integer, parameter, public :: GL_MAP2_GRID_SEGMENTS             =  #0DD3 
integer, parameter, public :: GL_TEXTURE_1D                     =  #0DE0 
integer, parameter, public :: GL_TEXTURE_2D                     =  #0DE1 

!   GetTextureParameter   
!        GL_TEXTURE_MAG_FILTER   
!        GL_TEXTURE_MIN_FILTER   
!        GL_TEXTURE_WRAP_S   
!        GL_TEXTURE_WRAP_T   
integer, parameter, public :: GL_TEXTURE_WIDTH          =  #1000 
integer, parameter, public :: GL_TEXTURE_HEIGHT         =  #1001 
integer, parameter, public :: GL_TEXTURE_COMPONENTS     =  #1003 
integer, parameter, public :: GL_TEXTURE_BORDER_COLOR   =  #1004 
integer, parameter, public :: GL_TEXTURE_BORDER         =  #1005 

!   HintMode   
integer, parameter, public :: GL_DONT_CARE  =  #1100 
integer, parameter, public :: GL_FASTEST    =  #1101 
integer, parameter, public :: GL_NICEST     =  #1102 

!   HintTarget   
!        GL_PERSPECTIVE_CORRECTION_HINT   
!        GL_POINT_SMOOTH_HINT   
!        GL_LINE_SMOOTH_HINT   
!        GL_POLYGON_SMOOTH_HINT   
!        GL_FOG_HINT   

!   LightModelParameter   
!        GL_LIGHT_MODEL_AMBIENT   
!        GL_LIGHT_MODEL_LOCAL_VIEWER   
!        GL_LIGHT_MODEL_TWO_SIDE   

!   LightName   
integer, parameter, public :: GL_LIGHT0 =  #4000 
integer, parameter, public :: GL_LIGHT1 =  #4001 
integer, parameter, public :: GL_LIGHT2 =  #4002 
integer, parameter, public :: GL_LIGHT3 =  #4003 
integer, parameter, public :: GL_LIGHT4 =  #4004 
integer, parameter, public :: GL_LIGHT5 =  #4005 
integer, parameter, public :: GL_LIGHT6 =  #4006 
integer, parameter, public :: GL_LIGHT7 =  #4007 

!   LightParameter   
integer, parameter, public :: GL_AMBIENT                =  #1200 
integer, parameter, public :: GL_DIFFUSE                =  #1201 
integer, parameter, public :: GL_SPECULAR               =  #1202 
integer, parameter, public :: GL_POSITION               =  #1203 
integer, parameter, public :: GL_SPOT_DIRECTION         =  #1204 
integer, parameter, public :: GL_SPOT_EXPONENT          =  #1205 
integer, parameter, public :: GL_SPOT_CUTOFF            =  #1206 
integer, parameter, public :: GL_CONSTANT_ATTENUATION   =  #1207 
integer, parameter, public :: GL_LINEAR_ATTENUATION     =  #1208 
integer, parameter, public :: GL_QUADRATIC_ATTENUATION  =  #1209 

!   ListMode   
integer, parameter, public :: GL_COMPILE                =  #1300 
integer, parameter, public :: GL_COMPILE_AND_EXECUTE    =  #1301 

!   ListNameType   
integer, parameter, public :: GL_BYTE           =  #1400 
integer, parameter, public :: GL_UNSIGNED_BYTE  =  #1401 
integer, parameter, public :: GL_SHORT          =  #1402 
integer, parameter, public :: GL_UNSIGNED_SHORT =  #1403 
integer, parameter, public :: GL_INT            =  #1404 
integer, parameter, public :: GL_UNSIGNED_INT   =  #1405 
integer, parameter, public :: GL_FLOAT          =  #1406 
integer, parameter, public :: GL_2_BYTES        =  #1407 
integer, parameter, public :: GL_3_BYTES        =  #1408 
integer, parameter, public :: GL_4_BYTES        =  #1409 

!   LogicOp   
integer, parameter, public :: GL_CLEAR          =  #1500 
integer, parameter, public :: GL_AND            =  #1501 
integer, parameter, public :: GL_AND_REVERSE    =  #1502 
integer, parameter, public :: GL_COPY           =  #1503 
integer, parameter, public :: GL_AND_INVERTED   =  #1504 
integer, parameter, public :: GL_NOOP           =  #1505 
integer, parameter, public :: GL_XOR            =  #1506 
integer, parameter, public :: GL_OR             =  #1507 
integer, parameter, public :: GL_NOR            =  #1508 
integer, parameter, public :: GL_EQUIV          =  #1509 
integer, parameter, public :: GL_INVERT         =  #150A 
integer, parameter, public :: GL_OR_REVERSE     =  #150B 
integer, parameter, public :: GL_COPY_INVERTED  =  #150C 
integer, parameter, public :: GL_OR_INVERTED    =  #150D 
integer, parameter, public :: GL_NAND           =  #150E 
integer, parameter, public :: GL_SET            =  #150F 

!   MapTarget   
!        GL_MAP1_COLOR_4   
!        GL_MAP1_INDEX   
!        GL_MAP1_NORMAL   
!        GL_MAP1_TEXTURE_COORD_1   
!        GL_MAP1_TEXTURE_COORD_2   
!        GL_MAP1_TEXTURE_COORD_3   
!        GL_MAP1_TEXTURE_COORD_4   
!        GL_MAP1_VERTEX_3   
!        GL_MAP1_VERTEX_4   
!        GL_MAP2_COLOR_4   
!        GL_MAP2_INDEX   
!        GL_MAP2_NORMAL   
!        GL_MAP2_TEXTURE_COORD_1   
!        GL_MAP2_TEXTURE_COORD_2   
!        GL_MAP2_TEXTURE_COORD_3   
!        GL_MAP2_TEXTURE_COORD_4   
!        GL_MAP2_VERTEX_3   
!        GL_MAP2_VERTEX_4   

!   MaterialFace   
!        GL_FRONT   
!        GL_BACK   
!        GL_FRONT_AND_BACK   

!   MaterialParameter   
integer, parameter, public :: GL_EMISSION               =  #1600 
integer, parameter, public :: GL_SHININESS              =  #1601 
integer, parameter, public :: GL_AMBIENT_AND_DIFFUSE    =  #1602 
integer, parameter, public :: GL_COLOR_INDEXES          =  #1603 
!        GL_AMBIENT   
!        GL_DIFFUSE   
!        GL_SPECULAR   

!   MatrixMode   
integer, parameter, public :: GL_MODELVIEW  =  #1700 
integer, parameter, public :: GL_PROJECTION =  #1701 
integer, parameter, public :: GL_TEXTURE    =  #1702 

!   MeshMode1   
!        GL_POINT   
!        GL_LINE   

!   MeshMode2   
!        GL_POINT   
!        GL_LINE   
!        GL_FILL   

!   PixelCopyType   
integer, parameter, public :: GL_COLOR      =  #1800 
integer, parameter, public :: GL_DEPTH      =  #1801 
integer, parameter, public :: GL_STENCIL    =  #1802 

!   PixelFormat   
integer, parameter, public :: GL_COLOR_INDEX        =  #1900 
integer, parameter, public :: GL_STENCIL_INDEX      =  #1901 
integer, parameter, public :: GL_DEPTH_COMPONENT    =  #1902 
integer, parameter, public :: GL_RED                =  #1903 
integer, parameter, public :: GL_GREEN              =  #1904 
integer, parameter, public :: GL_BLUE               =  #1905 
integer, parameter, public :: GL_ALPHA              =  #1906 
integer, parameter, public :: GL_RGB                =  #1907 
integer, parameter, public :: GL_RGBA               =  #1908 
integer, parameter, public :: GL_LUMINANCE          =  #1909 
integer, parameter, public :: GL_LUMINANCE_ALPHA    =  #190A 

!   PixelMap   
!        GL_PIXEL_MAP_I_TO_I   
!        GL_PIXEL_MAP_S_TO_S   
!        GL_PIXEL_MAP_I_TO_R   
!        GL_PIXEL_MAP_I_TO_G   
!        GL_PIXEL_MAP_I_TO_B   
!        GL_PIXEL_MAP_I_TO_A   
!        GL_PIXEL_MAP_R_TO_R   
!        GL_PIXEL_MAP_G_TO_G   
!        GL_PIXEL_MAP_B_TO_B   
!        GL_PIXEL_MAP_A_TO_A   

!   PixelStore   
!        GL_UNPACK_SWAP_BYTES   
!        GL_UNPACK_LSB_FIRST   
!        GL_UNPACK_ROW_LENGTH   
!        GL_UNPACK_SKIP_ROWS   
!        GL_UNPACK_SKIP_PIXELS   
!        GL_UNPACK_ALIGNMENT   
!        GL_PACK_SWAP_BYTES   
!        GL_PACK_LSB_FIRST   
!        GL_PACK_ROW_LENGTH   
!        GL_PACK_SKIP_ROWS   
!        GL_PACK_SKIP_PIXELS   
!        GL_PACK_ALIGNMENT   

!   PixelTransfer   
!        GL_MAP_COLOR   
!        GL_MAP_STENCIL   
!        GL_INDEX_SHIFT   
!        GL_INDEX_OFFSET   
!        GL_RED_SCALE   
!        GL_RED_BIAS   
!        GL_GREEN_SCALE   
!        GL_GREEN_BIAS   
!        GL_BLUE_SCALE   
!        GL_BLUE_BIAS   
!        GL_ALPHA_SCALE   
!        GL_ALPHA_BIAS   
!        GL_DEPTH_SCALE   
!        GL_DEPTH_BIAS   

!   PixelType   
integer, parameter, public :: GL_BITMAP =  #1A00 
!        GL_BYTE   
!        GL_UNSIGNED_BYTE   
!        GL_SHORT   
!        GL_UNSIGNED_SHORT   
!        GL_INT   
!        GL_UNSIGNED_INT   
!        GL_FLOAT   

!   PolygonMode   
integer, parameter, public :: GL_POINT  =  #1B00 
integer, parameter, public :: GL_LINE   =  #1B01 
integer, parameter, public :: GL_FILL   =  #1B02 

!   ReadBufferMode   
!        GL_FRONT_LEFT   
!        GL_FRONT_RIGHT   
!        GL_BACK_LEFT   
!        GL_BACK_RIGHT   
!        GL_FRONT   
!        GL_BACK   
!        GL_LEFT   
!        GL_RIGHT   
!        GL_AUX0   
!        GL_AUX1   
!        GL_AUX2   
!        GL_AUX3   

!   RenderingMode   
integer, parameter, public :: GL_RENDER     =  #1C00 
integer, parameter, public :: GL_FEEDBACK   =  #1C01 
integer, parameter, public :: GL_SELECT     =  #1C02 

!   ShadingModel   
integer, parameter, public :: GL_FLAT   =  #1D00 
integer, parameter, public :: GL_SMOOTH =  #1D01 

!   StencilFunction   
!        GL_NEVER   
!        GL_LESS   
!        GL_EQUAL   
!        GL_LEQUAL   
!        GL_GREATER   
!        GL_NOTEQUAL   
!        GL_GEQUAL   
!        GL_ALWAYS   

!   StencilOp   
!        GL_ZERO   
integer, parameter, public :: GL_KEEP       =  #1E00 
integer, parameter, public :: GL_REPLACE    =  #1E01 
integer, parameter, public :: GL_INCR       =  #1E02 
integer, parameter, public :: GL_DECR       =  #1E03 
!        GL_INVERT   

!   StringName   
integer, parameter, public :: GL_VENDOR     =  #1F00 
integer, parameter, public :: GL_RENDERER   =  #1F01 
integer, parameter, public :: GL_VERSION    =  #1F02 
integer, parameter, public :: GL_EXTENSIONS =  #1F03 

!   TextureCoordName   
integer, parameter, public :: GL_S =  #2000 
integer, parameter, public :: GL_T =  #2001 
integer, parameter, public :: GL_R =  #2002 
integer, parameter, public :: GL_Q =  #2003 

!   TextureEnvMode   
integer, parameter, public :: GL_MODULATE   =  #2100 
integer, parameter, public :: GL_DECAL      =  #2101 
!        GL_BLEND   

!   TextureEnvParameter   
integer, parameter, public :: GL_TEXTURE_ENV_MODE   =  #2200 
integer, parameter, public :: GL_TEXTURE_ENV_COLOR  =  #2201 

!   TextureEnvTarget   
integer, parameter, public :: GL_TEXTURE_ENV =  #2300 

!   TextureGenMode   
integer, parameter, public :: GL_EYE_LINEAR     =  #2400 
integer, parameter, public :: GL_OBJECT_LINEAR  =  #2401 
integer, parameter, public :: GL_SPHERE_MAP     =  #2402 

!   TextureGenParameter   
integer, parameter, public :: GL_TEXTURE_GEN_MODE   =  #2500 
integer, parameter, public :: GL_OBJECT_PLANE       =  #2501 
integer, parameter, public :: GL_EYE_PLANE          =  #2502 

!   TextureMagFilter   
integer, parameter, public :: GL_NEAREST    =  #2600 
integer, parameter, public :: GL_LINEAR     =  #2601 

!   TextureMinFilter   
!        GL_NEAREST   
!        GL_LINEAR   
integer, parameter, public :: GL_NEAREST_MIPMAP_NEAREST =  #2700 
integer, parameter, public :: GL_LINEAR_MIPMAP_NEAREST  =  #2701 
integer, parameter, public :: GL_NEAREST_MIPMAP_LINEAR  =  #2702 
integer, parameter, public :: GL_LINEAR_MIPMAP_LINEAR   =  #2703 

!   TextureParameterName   
integer, parameter, public :: GL_TEXTURE_MAG_FILTER =  #2800 
integer, parameter, public :: GL_TEXTURE_MIN_FILTER =  #2801 
integer, parameter, public :: GL_TEXTURE_WRAP_S     =  #2802 
integer, parameter, public :: GL_TEXTURE_WRAP_T     =  #2803 

!   TextureTarget   
!        GL_TEXTURE_1D   
!        GL_TEXTURE_2D   

!   TextureWrapMode   
integer, parameter, public :: GL_CLAMP  =  #2900 
integer, parameter, public :: GL_REPEAT =  #2901 

!                           **** FGLU ****        

! 
!     Procedure declarations, constant definitions and macros for the OpenGL
!     Utility Library.
!


! 
! ** Copyright 1991-1993, Silicon Graphics, Inc.
! ** All Rights Reserved.
! ** 
! ** This is UNPUBLISHED PROPRIETARY SOURCE CODE of Silicon Graphics, Inc.;
! ** the contents of this file may not be disclosed to third parties, copied or
! ** duplicated in any form, in whole or in part, without the prior written
! ** permission of Silicon Graphics, Inc.
! ** 
! ** RESTRICTED RIGHTS LEGEND:
! ** Use, duplication or disclosure by the Government is subject to restrictions
! ** as set forth in subdivision (c)(1)(ii) of the Rights in Technical Data
! ** and Computer Software clause at DFARS 252.227-7013, and/or in similar or
! ** successor clauses in the FAR, DOD or NASA FAR Supplement. Unpublished -
! ** rights reserved under the Copyright Laws of the United States.
! 
!


!               Generic constants

!   Errors: (return value 0 = no error)   
integer, parameter, public :: GLU_INVALID_ENUM  = 100900 
integer, parameter, public :: GLU_INVALID_VALUE = 100901 
integer, parameter, public :: GLU_OUT_OF_MEMORY = 100902 

!   For laughs:   
integer, parameter, public :: GLU_TRUE  = GL_TRUE
integer, parameter, public :: GLU_FALSE = GL_FALSE

!               Quadric constants               

!   Types of normals:   
integer, parameter, public :: GLU_SMOOTH    = 100000 
integer, parameter, public :: GLU_FLAT      = 100001 
integer, parameter, public :: GLU_NONE      = 100002 
!   DrawStyle types:   
integer, parameter, public :: GLU_POINT         = 100010 
integer, parameter, public :: GLU_LINE          = 100011 
integer, parameter, public :: GLU_FILL          = 100012 
integer, parameter, public :: GLU_SILHOUETTE    = 100013 
!   Orientation types:   
integer, parameter, public :: GLU_OUTSIDE   = 100020 
integer, parameter, public :: GLU_INSIDE    = 100021 
!   Callback types:   
!        GLU_ERROR               100103   


!    *           Tesselation constants           ****/

!   Callback types:   
integer, parameter, public :: GLU_BEGIN     = 100100        ! void (*)(integer*4)        
integer, parameter, public :: GLU_VERTEX    = 100101       ! void (*)(void *)        
integer, parameter, public :: GLU_END       = 100102          ! void (*)(void)          
integer, parameter, public :: GLU_ERROR     = 100103        ! void (*)(integer*4)      */
integer, parameter, public :: GLU_EDGE_FLAG = 100104    ! void (*)(character)  */

!   Contours types:   
integer, parameter, public :: GLU_CW            = 100120 
integer, parameter, public :: GLU_CCW           = 100121 
integer, parameter, public :: GLU_INTERIOR      = 100122 
integer, parameter, public :: GLU_EXTERIOR      = 100123 
integer, parameter, public :: GLU_UNKNOWN       = 100124 
integer, parameter, public :: GLU_TESS_ERROR1   = 100151 
integer, parameter, public :: GLU_TESS_ERROR2   = 100152 
integer, parameter, public :: GLU_TESS_ERROR3   = 100153 
integer, parameter, public :: GLU_TESS_ERROR4   = 100154 
integer, parameter, public :: GLU_TESS_ERROR5   = 100155
integer, parameter, public :: GLU_TESS_ERROR6   = 100156
integer, parameter, public :: GLU_TESS_ERROR7   = 100157
integer, parameter, public :: GLU_TESS_ERROR8   = 100158

!    *           NURBS constants                 ****/

!   Properties:   
integer, parameter, public :: GLU_AUTO_LOAD_MATRIX      = 100200 
integer, parameter, public :: GLU_CULLING               = 100201 
integer, parameter, public :: GLU_SAMPLING_TOLERANCE    = 100203 
integer, parameter, public :: GLU_DISPLAY_MODE          = 100204 
!   Trimming curve types   
integer, parameter, public :: GLU_MAP1_TRIM_2 = 100210 
integer, parameter, public :: GLU_MAP1_TRIM_3 = 100211 
!   Display modes:   
!        GLU_FILL                100012   
integer, parameter, public :: GLU_OUTLINE_POLYGON   = 100240 
integer, parameter, public :: GLU_OUTLINE_PATCH     = 100241 
!   Callbacks:   
!        GLU_ERROR               100103   

!   Errors:   
integer, parameter, public :: GLU_NURBS_ERROR1 = 100251 
integer, parameter, public :: GLU_NURBS_ERROR2 = 100252 
integer, parameter, public :: GLU_NURBS_ERROR3 = 100253 
integer, parameter, public :: GLU_NURBS_ERROR4 = 100254 
integer, parameter, public :: GLU_NURBS_ERROR5 = 100255
integer, parameter, public :: GLU_NURBS_ERROR6 = 100256
integer, parameter, public :: GLU_NURBS_ERROR7 = 100257
integer, parameter, public :: GLU_NURBS_ERROR8 = 100258
integer, parameter, public :: GLU_NURBS_ERROR9 = 100259
integer, parameter, public :: GLU_NURBS_ERROR10= 100260
integer, parameter, public :: GLU_NURBS_ERROR11= 100261
integer, parameter, public :: GLU_NURBS_ERROR12= 100262
integer, parameter, public :: GLU_NURBS_ERROR13= 100263
integer, parameter, public :: GLU_NURBS_ERROR14= 100264
integer, parameter, public :: GLU_NURBS_ERROR15= 100265
integer, parameter, public :: GLU_NURBS_ERROR16= 100266
integer, parameter, public :: GLU_NURBS_ERROR17= 100267
integer, parameter, public :: GLU_NURBS_ERROR18= 100268
integer, parameter, public :: GLU_NURBS_ERROR19= 100269
integer, parameter, public :: GLU_NURBS_ERROR20= 100270
integer, parameter, public :: GLU_NURBS_ERROR21= 100271
integer, parameter, public :: GLU_NURBS_ERROR22= 100272
integer, parameter, public :: GLU_NURBS_ERROR23= 100273
integer, parameter, public :: GLU_NURBS_ERROR24= 100274
integer, parameter, public :: GLU_NURBS_ERROR25= 100275
integer, parameter, public :: GLU_NURBS_ERROR26= 100276
integer, parameter, public :: GLU_NURBS_ERROR27= 100277
integer, parameter, public :: GLU_NURBS_ERROR28= 100278
integer, parameter, public :: GLU_NURBS_ERROR29= 100279
integer, parameter, public :: GLU_NURBS_ERROR30= 100280
integer, parameter, public :: GLU_NURBS_ERROR31= 100281
integer, parameter, public :: GLU_NURBS_ERROR32= 100282
integer, parameter, public :: GLU_NURBS_ERROR33= 100283
integer, parameter, public :: GLU_NURBS_ERROR34= 100284
integer, parameter, public :: GLU_NURBS_ERROR35= 100285
integer, parameter, public :: GLU_NURBS_ERROR36= 100286
integer, parameter, public :: GLU_NURBS_ERROR37= 100287


!                           **** FGLAUX ****        

! 
!     Procedure declarations, constant definitions and macros for the OpenGL
!     Auxiliary Library.
! 

! 
!  * (c) Copyright 1993, Silicon Graphics, Inc.
!  * ALL RIGHTS RESERVED 
!  * Permission to use, copy, modify, and distribute this software for 
!  * any purpose and without fee is hereby granted, provided that the above
!  * copyright notice appear in all copies and that both the copyright notice
!  * and this permission notice appear in supporting documentation, and that 
!  * the name of Silicon Graphics, Inc. not be used in advertising
!  * or publicity pertaining to distribution of the software without specific,
!  * written prior permission. 
!  *
!  * THE MATERIAL EMBODIED ON THIS SOFTWARE IS PROVIDED TO YOU "AS-IS"
!  * AND WITHOUT WARRANTY OF ANY KIND, EXPRESS, IMPLIED OR OTHERWISE,
!  * INCLUDING WITHOUT LIMITATION, ANY WARRANTY OF MERCHANTABILITY OR
!  * FITNESS FOR A PARTICULAR PURPOSE.  IN NO EVENT SHALL SILICON
!  * GRAPHICS, INC.  BE LIABLE TO YOU OR ANYONE ELSE FOR ANY DIRECT,
!  * SPECIAL, INCIDENTAL, INDIRECT OR CONSEQUENTIAL DAMAGES OF ANY
!  * KIND, OR ANY DAMAGES WHATSOEVER, INCLUDING WITHOUT LIMITATION,
!  * LOSS OF PROFIT, LOSS OF USE, SAVINGS OR REVENUE, OR THE CLAIMS OF
!  * THIRD PARTIES, WHETHER OR NOT SILICON GRAPHICS, INC.  HAS BEEN
!  * ADVISED OF THE POSSIBILITY OF SUCH LOSS, HOWEVER CAUSED AND ON
!  * ANY THEORY OF LIABILITY, ARISING OUT OF OR IN CONNECTION WITH THE
!  * POSSESSION, USE OR PERFORMANCE OF THIS SOFTWARE.
!  * 
!  * US Government Users Restricted Rights 
!  * Use, duplication, or disclosure by the Government is subject to
!  * restrictions set forth in FAR 52.227.19(c)(2) or subparagraph
!  * (c)(1)(ii) of the Rights in Technical Data and Computer Software
!  * clause at DFARS 252.227-7013 and/or in similar or successor
!  * clauses in the FAR or the DOD or NASA FAR Supplement.
!  * Unpublished-- rights reserved under the copyright laws of the
!  * United States.  Contractor/manufacturer is Silicon Graphics,
!  * Inc., 2011 N.  Shoreline Blvd., Mountain View, CA 94039-7311.
!  *
!  * OpenGL(TM) is a trademark of Silicon Graphics, Inc.
!  
!


! 
! ** ToolKit Window Types
! ** In the future, AUX_RGBA may be a combination of both RGB and ALPHA
! 
!

integer, parameter, public :: AUX_RGB       = 0 
integer, parameter, public :: AUX_RGBA      = AUX_RGB
integer, parameter, public :: AUX_INDEX     = 1 
integer, parameter, public :: AUX_SINGLE    = 0 
integer, parameter, public :: AUX_DOUBLE    = 2 
integer, parameter, public :: AUX_DIRECT    = 0
integer, parameter, public :: AUX_INDIRECT  = 4

integer, parameter, public :: AUX_ACCUM         = 8 
integer, parameter, public :: AUX_ALPHA         = 16 
integer, parameter, public :: AUX_DEPTH         = 32 
integer, parameter, public :: AUX_STENCIL       = 64 
integer, parameter, public :: AUX_AUX           = 128 
integer, parameter, public :: AUX_DEPTH16       = 256     ! 16-bit depth buffer
integer, parameter, public :: AUX_FIXED_332_PAL = 512

!  
! ** Window Masks
! 


!#define AUX_WIND_IS_RGB(x)      (((x) & AUX_INDEX) == 0)
!#define AUX_WIND_IS_INDEX(x)    (((x) & AUX_INDEX) != 0)
!#define AUX_WIND_IS_SINGLE(x)   (((x) & AUX_DOUBLE) == 0)
!#define AUX_WIND_IS_DOUBLE(x)   (((x) & AUX_DOUBLE) != 0)
!#define AUX_WIND_IS_INDIRECT(x) (((x) & AUX_INDIRECT) != 0)
!#define AUX_WIND_IS_DIRECT(x)   (((x) & AUX_INDIRECT) == 0)
!#define AUX_WIND_HAS_ACCUM(x)   (((x) & AUX_ACCUM) != 0)
!#define AUX_WIND_HAS_ALPHA(x)   (((x) & AUX_ALPHA) != 0)
!#define AUX_WIND_HAS_DEPTH(x)   (((x) & AUX_DEPTH) != 0)
!#define AUX_WIND_HAS_STENCIL(x) (((x) & AUX_STENCIL) != 0)

! 
! ** ToolKit Event Structure
! 
!
type  AUX_EVENTREC
    integer*4 event 
    integer*4 data (4 )
end type  AUX_EVENTREC

!  
! ** ToolKit Event Types
! 
!
integer, parameter, public :: AUX_EXPOSE    = 1 
integer, parameter, public :: AUX_CONFIG    = 2 
integer, parameter, public :: AUX_DRAW      = 4 
integer, parameter, public :: AUX_KEYEVENT  = 8 
integer, parameter, public :: AUX_MOUSEDOWN = 16 
integer, parameter, public :: AUX_MOUSEUP   = 32
integer, parameter, public :: AUX_MOUSELOC  = 64
! 
! ** Toolkit Event Data Indices
! 
!

integer, parameter, public :: AUX_WINDOWX       = 0 
integer, parameter, public :: AUX_WINDOWY       = 1 
integer, parameter, public :: AUX_MOUSEX        = 0 
integer, parameter, public :: AUX_MOUSEY        = 1 
integer, parameter, public :: AUX_MOUSESTATUS   = 3 
integer, parameter, public :: AUX_KEY           = 0
integer, parameter, public :: AUX_KEYSTATUS     = 1

! 
! ** ToolKit Event Status Messages
! 
!

integer, parameter, public :: AUX_LEFTBUTTON    = 1 
integer, parameter, public :: AUX_RIGHTBUTTON   = 2 
integer, parameter, public :: AUX_MIDDLEBUTTON  = 4 
integer, parameter, public :: AUX_SHIFT         = 1 
integer, parameter, public :: AUX_CONTROL       = 2 
!  
! ** ToolKit Key Codes
! 
!

integer, parameter, public :: AUX_RETURN    =  #0D 
integer, parameter, public :: AUX_ESCAPE    =  #1B 
integer, parameter, public :: AUX_SPACE     =  #20 
integer, parameter, public :: AUX_LEFT      =  #25 
integer, parameter, public :: AUX_UP        =  #26 
integer, parameter, public :: AUX_RIGHT     =  #27 
integer, parameter, public :: AUX_DOWN      =  #28 
integer, parameter, public :: AUX_A = #41
integer, parameter, public :: AUX_B = #42
integer, parameter, public :: AUX_C = #43
integer, parameter, public :: AUX_D = #44
integer, parameter, public :: AUX_E = #45
integer, parameter, public :: AUX_F = #46
integer, parameter, public :: AUX_G = #47
integer, parameter, public :: AUX_H = #48
integer, parameter, public :: AUX_I = #49
integer, parameter, public :: AUX_J = #4A
integer, parameter, public :: AUX_K = #4B
integer, parameter, public :: AUX_L = #4C
integer, parameter, public :: AUX_M = #4D
integer, parameter, public :: AUX_N = #4E
integer, parameter, public :: AUX_O = #4F
integer, parameter, public :: AUX_P = #50
integer, parameter, public :: AUX_Q = #51
integer, parameter, public :: AUX_R = #52
integer, parameter, public :: AUX_S = #53
integer, parameter, public :: AUX_T = #54
integer, parameter, public :: AUX_U = #55
integer, parameter, public :: AUX_V = #56
integer, parameter, public :: AUX_W = #57
integer, parameter, public :: AUX_X = #58
integer, parameter, public :: AUX_Y = #59
integer, parameter, public :: AUX_Z = #5A
integer, parameter, public :: AUX_aa = #61
integer, parameter, public :: AUX_bb = #62
integer, parameter, public :: AUX_cc = #63
integer, parameter, public :: AUX_dd = #64
integer, parameter, public :: AUX_ee = #65
integer, parameter, public :: AUX_ff = #66
integer, parameter, public :: AUX_gg = #67
integer, parameter, public :: AUX_hh = #68
integer, parameter, public :: AUX_ii = #69
integer, parameter, public :: AUX_jj = #6A
integer, parameter, public :: AUX_kk = #6B
integer, parameter, public :: AUX_ll = #6C
integer, parameter, public :: AUX_mm = #6D
integer, parameter, public :: AUX_nn = #6E
integer, parameter, public :: AUX_oo = #6F
integer, parameter, public :: AUX_pp = #70
integer, parameter, public :: AUX_qq = #71
integer, parameter, public :: AUX_rr = #72
integer, parameter, public :: AUX_ss = #73
integer, parameter, public :: AUX_tt = #74
integer, parameter, public :: AUX_uu = #75
integer, parameter, public :: AUX_vv = #76
integer, parameter, public :: AUX_ww = #77
integer, parameter, public :: AUX_xx = #78
integer, parameter, public :: AUX_yy = #79
integer, parameter, public :: AUX_zz = #7A
integer, parameter, public :: AUX_0 = #30
integer, parameter, public :: AUX_1 = #31
integer, parameter, public :: AUX_2 = #32
integer, parameter, public :: AUX_3 = #33
integer, parameter, public :: AUX_4 = #34
integer, parameter, public :: AUX_5 = #35
integer, parameter, public :: AUX_6 = #36
integer, parameter, public :: AUX_7 = #37
integer, parameter, public :: AUX_8 = #38
integer, parameter, public :: AUX_9 = #39

! 
! ** ToolKit Gets and Sets
! 
!
integer, parameter, public :: AUX_FD            = 1  !  return fd (long)    
integer, parameter, public :: AUX_COLORMAP      = 3  !  pass buf of r, g and b (unsigned char)    
integer, parameter, public :: AUX_GREYSCALEMAP  = 4 
integer, parameter, public :: AUX_FOGMAP        = 5
integer, parameter, public :: AUX_ONECOLOR      = 6 ! pass index, r, g, and b (long) 
  
! 
! ** Color Macros
! 
!

integer, parameter, public :: AUX_BLACK     = 0 
integer, parameter, public :: AUX_RED       = 13 
integer, parameter, public :: AUX_GREEN     = 14 
integer, parameter, public :: AUX_YELLOW    = 15 
integer, parameter, public :: AUX_BLUE      = 16 
integer, parameter, public :: AUX_MAGENTA   = 17
integer, parameter, public :: AUX_CYAN      = 18
integer, parameter, public :: AUX_WHITE     = 19
!extern float auxRGBMap[20][3];

!#define AUX_SETCOLOR(x, y) (AUX_WIND_IS_RGB((x)) ? \
!                           glColor3fv(auxRGBMap[(y)]) : glIndexf((y)))

! 
! ** RGB Image Structure
! 
!
type  AUX_RGBImageRec
    integer*4 sizeX,  sizeY 
    character*8 data 
end type  AUX_RGBImageRec

!*
!* Viewperf support constants
!*
!* Display Mode Selection Criteria 

integer, parameter, public :: AUX_USE_ID            = 1
integer, parameter, public :: AUX_EXACT_MATCH       = 2
integer, parameter, public :: AUX_MINIMUM_CRITERIA  = 3


end module openglty
