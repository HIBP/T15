# Microsoft Developer Studio Generated NMAKE File, Format Version 4.00
# ** DO NOT EDIT **

# TARGTYPE "Win32 (x86) Console Application" 0x0103

!IF "$(CFG)" == ""
CFG=main - Win32 Debug
!MESSAGE No configuration specified.  Defaulting to main - Win32 Debug.
!ENDIF 

!IF "$(CFG)" != "main - Win32 Release" && "$(CFG)" != "main - Win32 Debug"
!MESSAGE Invalid configuration "$(CFG)" specified.
!MESSAGE You can specify a configuration when running NMAKE on this makefile
!MESSAGE by defining the macro CFG on the command line.  For example:
!MESSAGE 
!MESSAGE NMAKE /f "MAIN.MAK" CFG="main - Win32 Debug"
!MESSAGE 
!MESSAGE Possible choices for configuration are:
!MESSAGE 
!MESSAGE "main - Win32 Release" (based on "Win32 (x86) Console Application")
!MESSAGE "main - Win32 Debug" (based on "Win32 (x86) Console Application")
!MESSAGE 
!ERROR An invalid configuration is specified.
!ENDIF 

!IF "$(OS)" == "Windows_NT"
NULL=
!ELSE 
NULL=nul
!ENDIF 
################################################################################
# Begin Project
# PROP Target_Last_Scanned "main - Win32 Debug"
F90=fl32.exe
RSC=rc.exe

!IF  "$(CFG)" == "main - Win32 Release"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 0
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 0
OUTDIR=.
INTDIR=.

ALL : "$(OUTDIR)\MAIN.exe"

CLEAN : 
	-@erase ".\MAIN.exe"
	-@erase ".\Mast0.obj"
	-@erase ".\Subcompc.obj"
	-@erase ".\Ellint.obj"
	-@erase ".\Mast3.obj"
	-@erase ".\mast03.obj"
	-@erase ".\Axes.obj"
	-@erase ".\Figcompc.obj"
	-@erase ".\Mast2.obj"
	-@erase ".\Concompa.obj"
	-@erase ".\Figbegc.obj"
	-@erase ".\Coil.obj"
	-@erase ".\Mast.obj"
	-@erase ".\Mast1.obj"
	-@erase ".\main.obj"
	-@erase ".\Mast05.obj"

# ADD BASE F90 /Ox /c /nologo
# ADD F90 /Ox /c /nologo
F90_PROJ=/Ox /c /nologo 
# ADD BASE RSC /l 0x419 /d "NDEBUG"
# ADD RSC /l 0x419 /d "NDEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/MAIN.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /machine:I386
# ADD LINK32 kernel32.lib maths.lib /nologo /subsystem:console /machine:I386
LINK32_FLAGS=kernel32.lib maths.lib /nologo /subsystem:console /incremental:no\
 /pdb:"$(OUTDIR)/MAIN.pdb" /machine:I386 /out:"$(OUTDIR)/MAIN.exe" 
LINK32_OBJS= \
	"$(INTDIR)/Mast0.obj" \
	"$(INTDIR)/Subcompc.obj" \
	"$(INTDIR)/Ellint.obj" \
	"$(INTDIR)/Mast3.obj" \
	"$(INTDIR)/mast03.obj" \
	"$(INTDIR)/Axes.obj" \
	"$(INTDIR)/Figcompc.obj" \
	"$(INTDIR)/Mast2.obj" \
	"$(INTDIR)/Concompa.obj" \
	"$(INTDIR)/Figbegc.obj" \
	"$(INTDIR)/Coil.obj" \
	"$(INTDIR)/Mast.obj" \
	"$(INTDIR)/Mast1.obj" \
	"$(INTDIR)/main.obj" \
	"$(INTDIR)/Mast05.obj"

"$(OUTDIR)\MAIN.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ELSEIF  "$(CFG)" == "main - Win32 Debug"

# PROP BASE Use_MFC 0
# PROP BASE Use_Debug_Libraries 1
# PROP Use_MFC 0
# PROP Use_Debug_Libraries 1
OUTDIR=.
INTDIR=.

ALL : "$(OUTDIR)\MAIN.exe"

CLEAN : 
	-@erase ".\MAIN.exe"
	-@erase ".\Mast0.obj"
	-@erase ".\Subcompc.obj"
	-@erase ".\Ellint.obj"
	-@erase ".\Mast3.obj"
	-@erase ".\mast03.obj"
	-@erase ".\Axes.obj"
	-@erase ".\Figcompc.obj"
	-@erase ".\Mast2.obj"
	-@erase ".\Concompa.obj"
	-@erase ".\Figbegc.obj"
	-@erase ".\Coil.obj"
	-@erase ".\Mast.obj"
	-@erase ".\Mast1.obj"
	-@erase ".\main.obj"
	-@erase ".\Mast05.obj"
	-@erase ".\MAIN.ilk"
	-@erase ".\MAIN.pdb"

# ADD BASE F90 /Zi /c /nologo
# ADD F90 /Zi /c /nologo
F90_PROJ=/Zi /c /nologo /Fd"MAIN.pdb" 
# ADD BASE RSC /l 0x419 /d "_DEBUG"
# ADD RSC /l 0x419 /d "_DEBUG"
BSC32=bscmake.exe
# ADD BASE BSC32 /nologo
# ADD BSC32 /nologo
BSC32_FLAGS=/nologo /o"$(OUTDIR)/MAIN.bsc" 
BSC32_SBRS=
LINK32=link.exe
# ADD BASE LINK32 kernel32.lib /nologo /subsystem:console /debug /machine:I386
# ADD LINK32 kernel32.lib maths.lib /nologo /subsystem:console /debug /machine:I386
LINK32_FLAGS=kernel32.lib maths.lib /nologo /subsystem:console /incremental:yes\
 /pdb:"$(OUTDIR)/MAIN.pdb" /debug /machine:I386 /out:"$(OUTDIR)/MAIN.exe" 
LINK32_OBJS= \
	"$(INTDIR)/Mast0.obj" \
	"$(INTDIR)/Subcompc.obj" \
	"$(INTDIR)/Ellint.obj" \
	"$(INTDIR)/Mast3.obj" \
	"$(INTDIR)/mast03.obj" \
	"$(INTDIR)/Axes.obj" \
	"$(INTDIR)/Figcompc.obj" \
	"$(INTDIR)/Mast2.obj" \
	"$(INTDIR)/Concompa.obj" \
	"$(INTDIR)/Figbegc.obj" \
	"$(INTDIR)/Coil.obj" \
	"$(INTDIR)/Mast.obj" \
	"$(INTDIR)/Mast1.obj" \
	"$(INTDIR)/main.obj" \
	"$(INTDIR)/Mast05.obj"

"$(OUTDIR)\MAIN.exe" : "$(OUTDIR)" $(DEF_FILE) $(LINK32_OBJS)
    $(LINK32) @<<
  $(LINK32_FLAGS) $(LINK32_OBJS)
<<

!ENDIF 

.for.obj:
   $(F90) $(F90_PROJ) $<  

.f.obj:
   $(F90) $(F90_PROJ) $<  

.f90.obj:
   $(F90) $(F90_PROJ) $<  

################################################################################
# Begin Target

# Name "main - Win32 Release"
# Name "main - Win32 Debug"

!IF  "$(CFG)" == "main - Win32 Release"

!ELSEIF  "$(CFG)" == "main - Win32 Debug"

!ENDIF 

################################################################################
# Begin Source File

SOURCE=.\main.for

"$(INTDIR)\main.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Figcompc.for

"$(INTDIR)\Figcompc.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Coil.for

"$(INTDIR)\Coil.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Concompa.for

"$(INTDIR)\Concompa.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Ellint.for

"$(INTDIR)\Ellint.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Figbegc.for

"$(INTDIR)\Figbegc.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Axes.for

"$(INTDIR)\Axes.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Mast.for

"$(INTDIR)\Mast.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Subcompc.for

"$(INTDIR)\Subcompc.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Mast0.for

"$(INTDIR)\Mast0.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\mast03.for

"$(INTDIR)\mast03.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Mast05.for

"$(INTDIR)\Mast05.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Mast3.for

"$(INTDIR)\Mast3.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Mast2.for

"$(INTDIR)\Mast2.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
################################################################################
# Begin Source File

SOURCE=.\Mast1.for

"$(INTDIR)\Mast1.obj" : $(SOURCE) "$(INTDIR)"


# End Source File
# End Target
# End Project
################################################################################
