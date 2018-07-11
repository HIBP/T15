      module plotlib

      save

      interface
        subroutine Figure(FigNo, Params)
       !DEC$ ATTRIBUTES REFERENCE :: Params
            integer*4 FigNo
		  character Params
	  end subroutine Figure
      end interface

      interface
        subroutine IPlot(FigNo, N, Xarray, Yarray, Params)
       !DEC$ ATTRIBUTES REFERENCE :: FigNo, N, Params, XArray, YArray
	      integer*4 FigNo, N
	      integer*4 Xarray(:), Yarray(:)
            character Params
	  end subroutine IPlot
      end interface

      interface
        subroutine RPlot(FigNo, N, Xarray, Yarray, Params)
       !DEC$ ATTRIBUTES REFERENCE :: FigNo, N, Params, XArray, YArray
            integer*4 FigNo, N
	      real*4 Xarray(:), Yarray(:)
            character Params
	  end subroutine RPlot
      end interface
          
      interface
        subroutine PlotFrom(FigNo, StartIdx, N, Xarray, Yarray, Params)
       !DEC$ ATTRIBUTES REFERENCE :: FigNo, StartIdx, N, Params, XArray, YArray
            integer*4 FigNo, StartIdx, N
	      real*4 Xarray(:), Yarray(:)
            character Params
	  end subroutine PlotFrom
      end interface

		! ALIAS RPlot  
          interface
            subroutine Plot(FigNo, N, Xarray, Yarray, Params)
          !DEC$ ATTRIBUTES REFERENCE :: FigNo, N, Params, XArray, YArray
                integer*4 FigNo, N
	          real*4 Xarray(:), Yarray(:)
                character Params
            end subroutine Plot
          end interface

      interface
        subroutine DPlot(FigNo, N, Xarray, Yarray, Params)
       !DEC$ ATTRIBUTES REFERENCE :: FigNo, N, Params, XArray, YArray
            integer*4 FigNo, N
	      real*8 Xarray(:), Yarray(:)
            character Params
	  end subroutine DPlot
      end interface

      interface
        subroutine IParam(FigNo, Value, Params)
       !DEC$ ATTRIBUTES REFERENCE :: FigNo, Value, Params
            integer*4 FigNo
	      integer*4 Value
            character Params
	  end subroutine IParam
      end interface

      interface
        subroutine DParam(FigNo, Value, Params)
       !DEC$ ATTRIBUTES REFERENCE :: FigNo, Value, Params
            integer*4 FigNo
	      real*8 Value
            character Params
	  end subroutine DParam
      end interface

      interface
        subroutine RParam(FigNo, Value, Params)
       !DEC$ ATTRIBUTES REFERENCE :: FigNo, Value, Params
            integer*4 FigNo
	      real*4 Value
            character Params
	  end subroutine RParam
      end interface
           
		! ALIAS RParam   
          interface
            subroutine Param(FigNo, Value, Params)
           !DEC$ ATTRIBUTES REFERENCE :: FigNo, Value, Params
                integer*4 FigNo
	          real*4 Value
                character Params
            end subroutine Param
          end interface

      interface
        subroutine CParam(FigNo, Value, Params)
       !DEC$ ATTRIBUTES REFERENCE :: FigNo, Value, Params
            integer FigNo
	      character Value
            character Params
	  end subroutine CParam
      end interface



      interface
        subroutine PlotFile(FigNo, FileName, XCol, YCol, Params)
       !DEC$ ATTRIBUTES REFERENCE :: FigNo, FileName, Params, XCol, YCol
            integer*4 FigNo, N
	      integer*4 XCol, YCol
            character Params, FileName
	  end subroutine PlotFile
      end interface

      interface
        function PlotIonTemp(FigNo, N, Xarray, Yarray, Params)
       !DEC$ ATTRIBUTES REFERENCE :: FigNo, N, Params, XArray, YArray
            integer*4 FigNo, N
	      real*4 Xarray(:), Yarray(:), PlotIonTemp
            character Params
	  end function PlotIonTemp
      end interface



      pointer (Figure_ptr, Figure)
      pointer (IPlot_ptr,  IPlot),(DPlot_ptr, DPlot)  
	pointer (RPlot_ptr,  RPlot)
	pointer (PlotFrom_ptr, PlotFrom) 

      pointer (IParam_ptr, IParam),(RParam_ptr, RParam)
	pointer (DParam_ptr, DParam),(CParam_ptr, CParam)
	pointer (PlotFile_ptr, PlotFile)
        pointer (PlotIonTemp_ptr, PlotIonTemp)
      ! ALIASES
	pointer (Plot_ptr, Plot)
      pointer (Param_ptr,  Param)


      contains 
	
      !---------------------------------------

      subroutine PlotInit
      use MSWIN
      common /plotdata/ hLib
      integer hLib
	pointer (p_, UniPlot)
 
        hLib = LoadLibrary("plotlib.dll"C)
        
        if (hLib.eq.0) then 
          ! for debug
          hLib = LoadLibrary("g:\\mydelphi\\plotdll\\plotlib.dll"C)
        end if 

        if (hLib.eq.0) then 
          ! for debug
       hLib = LoadLibrary("e:\\delirium\\delphi\\plotdll\\plotlib.dll"C)
        end if 

        if (hLib.eq.0) then 
          print *, 'ERROR: Library "plotlib.dll" not found' 
		call ExitProcess(1) 
        end if 

           
        Figure_ptr = GetProcAddress (hLib, "Figure"C)

        p_ = GetProcAddress (hLib, "_Plot"C)
        IPlot_ptr = GetProcAddress (hLib, "IPlot"C)
        RPlot_ptr = GetProcAddress (hLib, "FPlot"C)
        DPlot_ptr = GetProcAddress (hLib, "DPlot"C)
	  PlotFrom_ptr= GetProcAddress (hLib, "FPlotEx"C)

        IParam_ptr = GetProcAddress (hLib, "IParam"C)
        RParam_ptr = GetProcAddress (hLib, "FParam"C)
        DParam_ptr = GetProcAddress (hLib, "DParam"C)
        CParam_ptr = GetProcAddress (hLib, "CParam"C)

	  PlotFile_ptr = GetProcAddress (hLib, "PlotFile"C)
          PlotIonTemp_ptr = GetProcAddress (hLib, "FPlotIonTemp"C)

	  Plot_ptr   = RPlot_ptr
        Param_ptr  = RParam_ptr

	end subroutine PlotInit

      !---------------------------------------

      subroutine PlotFin
      use DFWIN
      common /plotdata/ hLib
      integer ok

        ok = FreeLibrary(hLib)

	end subroutine PlotFin

      !---------------------------------------
    
    	end module plotlib
