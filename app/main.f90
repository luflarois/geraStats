!=============================================================================================
program check
    !# Program to perform check for GFS data
    !#
    !# @note
    !# ![](http://brams.cptec.inpe.br/wp-content/uploads/2015/11/logo-brams.navigation.png "")
    !#
    !# **Brief**: This program get data from GRIB2 data from GFS Model and check if data are 
    !# inside max and min values. If not, it will be reported in a log file. The program can also read CAMS data and check if the species are present in the GFS data. 
    !#
    !#
    !# **Author(s)**: Luiz Flavio Rodrigues **&#9993;**<luiz.rodrigues@inpe.br>
    !#
    !# **Date**: 13 Jul 2026
    !# @endnote
    !#
    !# @changes
    !# &#9744; <br/>
    !# @endchanges
    !# @bug
    !#
    !#@endbug
    !#
    !#@todo
    !#  &#9744; <br/>
    !# @endtodo
    !#
    !# @warning
    !# Now is under CC-GPL License, please see
    !# &copy; <https://creativecommons.org/licenses/GPL/3.0/legalcode.pt>
    !# @endwarning
    !#
    
    !Use area
    use dump
    
    use utilsMod, only: &
      bramsHeader, &
      fileExist, &
      initAll, &
      stepsBetweenDates, &
      to_upper, &
      getUnit, &
      releaseUnit, &
      gerar_lista

    use filesMod, only: &
      readNamelist, &
      readAtmosGrib2

    use memoryMod, only: &
      init_year, &
      init_month, &
      init_day, &
      init_hour, &
      final_year, &
      final_month, &
      final_day, &
      final_hour, &
      step, &
      chemDate, &
      atmosDate, &
      atmos_Type, atmosNx,atmosNy,atmosNz, lat, atmosValues, &
      atmos_idir, &
      grib2FilesNames, &
      MaxAtmosValues, MinAtmosValues, atmosNv, atmosVarNames

    implicit none

    include "constants.h"
    character(len=*),parameter :: sourceName='pre.f90' !Name of source code
    character(len=*),parameter :: procedureName='**pre**' !Name of this procedure
    !
    !Local Parameters
    character(len=*), parameter :: namelistFile='stat.nml'
    !# Namelist Filename
    character(len=*), parameter :: revision='0.1'
    !# Program revision
    integer, parameter :: gfs=1
    !# GFS type id
    integer, parameter :: era5=2
    !# era5 type id
    

    !Local variables
    integer :: stepsBetDates, n, f, s
    !# Total of steps between initial and final dates
    logical :: monthBetDates(12)
    integer :: its,i,j,k,iTime
    integer, allocatable :: validDates(:)
    !# time counter
    character(len=15) :: warningFileName
    character(len=8) :: dateNow
    character(len=10) :: timeNow
    character(len=5) :: zoneNow
    integer :: valuesNow(8)
    integer :: lunit

    character(len=3), dimension(4), parameter :: seasons = (/'DJF','MAM','JJA','SON'/)


    !Code

    !Initialize utils module
    iErrNumber=initAll()
    
    !Check if namelist file is in current folder
    !then, if exist, read the namelist
    if(.not. fileExist(namelistFile)) then 
      iErrNumber=dumpMessage(c_tty,c_yes,sourceName,procedureName &
              ,c_fatal,'File '//namelistFile &
              //' not found. Please, verify and solve it!')
    else
      iErrNumber=dumpMessage(c_tty,c_yes,'','',c_notice,'Reading '//namelistFile//'!')
      call readNamelist(namelistFile)
    endif

    print *, 'Initial Year: ', init_year
    print *, 'Final Year: ', final_year

    !Create the name of files input for GFS data
    grib2FilesNames =  gerar_lista(trim(atmos_idir), init_year, final_year)
    stepsBetDates = size(grib2FilesNames)

    print *, 'Total of files between initial and final dates: ', stepsBetDates
    
    !Read and compute max and min values for each variable in GFS data
    do its=1,stepsBetDates
        call readAtmosGrib2(its,stepsBetDates)
        if(.not. allocated(atmosValues)) cycle
    enddo

    do n=1,5
      do s=1,4
        do f = 1,6
          write(*,fmt='("max: ",2(A3,","),I1," - ",33(E13.5,","))') atmosVarNames(n),seasons(s),f, MaxAtmosValues(n,s,f,1:33)
          write(*,fmt='("min: ",2(A3,","),I1," - ",33(E13.5,","))') atmosVarNames(n),seasons(s),f, MinAtmosValues(n,s,f,1:33)
        end do
      end do
    end do


end program check
