!=============================================================================================
module filesMod
    !# Module to manipulate namelist file
    !#
    !# @note
    !# ![](http://brams.cptec.inpe.br/wp-content/uploads/2015/11/logo-brams.navigation.png "")
    !#
    !# **Brief**: manipulate namelist file
    !#
    !# **Documentation**: <http://brams.cptec.inpe.br/documentation/>
    !#
    !# **Author(s)**: Luiz Flavio Rodrigues **&#9993;**<luiz.rodrigues@inpe.br>
    !#
    !# **Date**: 26 August 2020 (Wednesday)
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
    !# &copy; <https://creativecommons.org/licenses/GPL/2.0/legalcode.pt>
    !# @endwarning
    !#
    
    !Use area
    use dump

    implicit none

    include "constants.h"
    character(len=*),parameter :: sourceName='filesMod.f90' !Name of source code
    character(len=*),parameter :: procedureName='**namelist**' !Name of this procedure
    !
    !Local Parameters

    !Local variables
    integer :: chemjIni,chemjFin,chemnpj
    integer :: chemiIni,chemiFin,chemnpi

    contains

    !=============================================================================================
    subroutine readNamelist(namelistFile)
        !# read namelist file from input
        !#
        !# @note
        !# ![](http://brams.cptec.inpe.br/wp-content/uploads/2015/11/logo-brams.navigation.png "")
        !#
        !# **Brief**: Read the namelist file, pre.nml and return data
        !#
        !# **Documentation**: <http://brams.cptec.inpe.br/documentation/>
        !#
        !# **Author(s)**: Luiz Flavio Rodrigues **&#9993;**<luiz.rodrigues@inpe.br>
        !#
        !# **Date**: 26 August 2020 (Wednesday)
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
        !# &copy; <https://creativecommons.org/licenses/GPL/2.0/legalcode.pt>
        !# @endwarning
        !#
        
        !Use area
        use dump
        use memoryMod, only: &
                  init_year ,&
                  final_year, atmos_idir, &
                  levels, initial_latitude,final_latitude,         &
                  initial_longitude,final_longitude,               &
                  chem_type, chem_idir, &
                  chem1_prefix,chem1_sufix,                        &
                  out_type, out_prefix, out_sufix, out_dir

        use utilsMod, only: &
                  getUnit, &
                  releaseUnit
    
        implicit none
    
        include "constants.h"
        character(len=*),parameter :: procedureName='**readNamelist**' !Name of this procedure
        !
        !Local Parameters
    
        !Input/Output variables
        character(len=*), intent(in) :: namelistFile
    
        !Local variables
        integer :: lunit
    
        !Code
        NAMELIST/args_input/init_year , final_year, atmos_idir, &
                            levels, initial_latitude,final_latitude, &
                            initial_longitude,final_longitude

         lunit=getUnit()
         open(unit=lunit, file=trim(namelistFile), status='old')
         read(lunit, args_input)
         ierrNumber=releaseUnit(lunit)


    end subroutine readNamelist 

    !=============================================================================================


    !=============================================================================================
    subroutine readAtmosGrib2(iStep,stepsBetDates)
        !# Open a grib2 file and read the contents
        !#
        !# @note
        !# ![](http://brams.cptec.inpe.br/wp-content/uploads/2015/11/logo-brams.navigation.png "")
        !#
        !# **Brief**: Open GFS Grib2 file, read the contents
        !#
        !# **Documentation**: <http://brams.cptec.inpe.br/documentation/>
        !#
        !# **Author(s)**: Luiz Flavio Rodrigues **&#9993;**<luiz.rodrigues@inpe.br>
        !#
        !# **Date**: 31 July 2020 (Friday)
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
        !# &copy; <https://creativecommons.org/licenses/GPL/2.0/legalcode.pt>
        !# @endwarning
        !#
        
        !Use area
        use dump

        use wgrib2api

        use memoryMod, only: &
            atmos_idir, &
            grib2FilesNames, &
            grib2InvFilesNames, &
            initial_latitude, &
            final_latitude, &
            initial_longitude, &
            final_longitude, &
            atmosNz, &
            atmosNx, &
            atmosNy, &
            atmosLon, &
            atmosLat, &
            atmosXs, &
            atmosYs, &
            atmosNv, &
            atmosLevels, &
            atmosVarNames,&
            atmosValues  , &
            atmosDate, &
            atmosHoursCount, &
            atmosErrors, &
            levels, &
            lat, &
            lon, &
            minAtmosValues, &
            maxAtmosValues

        use utilsMod, only: &
                  getUnit, &
                  releaseUnit, &
                  fileExist, &
                  date_add_to_dble


        implicit none
    
        include "constants.h"
        character(len=*),parameter :: procedureName='**openatmosGrib2**' 
        !# Name of this procedure
        !
        !Local Parameters
        character(len=*),parameter :: wind_u_varname='UGRD'
        !# Name of wind u var in grib2 (1)
        character(len=*),parameter :: wind_v_varname='VGRD'
        !# Name of wind v var in grib2 (2)
        character(len=*),parameter :: temperature_varname='TMP'
        !# Name of temperature var in grib2 (3)
        character(len=*),parameter :: geo_varname='HGT'
        !# Name of geopotential var in grib2 (4)
        character(len=*),parameter :: ur_varname='RH'
        !# Name of relative humidity var in grib2 (5)
        real,dimension(5),parameter :: scale_factor = (/1.,1.,1.,1.,0.01/)
        !# Factor to multiplie each var (1,2,3,4,5)
        real,dimension(5), parameter :: limi=(/-100.,-100.,180., -500.,  0./)
        !# inferior limits for each variable
        real,dimension(5), parameter :: lims=(/ 100., 100.,333.,80000.,100./)
        !# inferior limits for each variable
    
        integer, parameter :: maxpr=100
    
        !Input/Output variables
        integer, intent(in)         :: iStep
        !# timestep
        integer, intent(in)         :: stepsBetDates
        !# total of timesteps
    
        !Local variables
        integer :: nprz
        !# Total of vertical levels
        character(len=32) :: varName(5)
        !# Name os GFS variables
        character(len=200) :: metadata
        !# Metadata from grib2 information
        character(len=30), allocatable :: slevs(:)
        !# Levels description from grib2 file
        character(len=30), allocatable :: slevsInv(:)
        !# Levels description INverted
        real,allocatable :: levpr_grib2(:)
        !# levels values from grib2 file
        character (len=300) :: grid_info
        !# grid information of grib2 file
        character (len=99) :: invline
        !# Var information from grib2 file
        real, allocatable :: var(:,:)
        !# Variable value for each (x,y) point
        integer :: nxGrib
        !# Size of grib data in lon direction
        integer :: nyGrib
        !# Size of grib data in lat direction
        real :: dx
        !# deltaX longitude increment
        real :: dy
        !# deltaY longitude increment 
        integer :: iyear,imonth,iday,ihour
        !# year, month, day and hour read from grib2 file
        real :: latIni,latFin
        !# Initial and final valid latittudes
        real :: lonIni,lonFin
        !# Initial and final valid longitudes
        integer :: jIni,jFin
        !# first and last latittude valid position
        integer :: iIni,Ifin
        !# first and last longitude valid position
        integer :: npi,npj
        !# Total of valid points i,j
        character(len=3) :: clvi
        integer :: iyy,imm,idd,ihh
        integer :: season
    
        integer :: i,j,k,lv,nvar,iCount,jCount,ii,jj,lvi
        integer :: lunit

        character(len=256) :: fName,fNameI,fNameB
        character(len=3) :: ncl

        fName=trim(grib2FilesNames(iStep))
        print *,'Opening/reading Grib2 file: '//trim(fName)
        fNameI=get_basename_noext(trim(grib2FilesNames(iStep)))//'.inv'
        fNameB=get_basename_noext(trim(grib2FilesNames(iStep)))//'.blow'
    
        ! Read the header of input pressure file.
        varName(1)=':'//temperature_varname//':'
    
        !Code

        if(.not. fileExist(trim(fName))) then
          iErrNumber=dumpMessage(c_tty,c_yes,sourceName,procedureName &
              ,c_warning,'File '//trim(fName) &
              //' not found. Please, verify and solve it!')
              return
        endif

        iErrNumber=dumpMessage(c_tty,c_yes,'','',c_notice,'Opening/reading Grib2 file: '//trim(fName))
        
        !Making inventory using a wgrib2 function grb2_mk_inv   
        iErrNumber = grb2_mk_inv(fName,fNameI)
        !print *,iErrNumber
        !if (iErrNumber.ne.1) iErrNumber=dumpMessage(c_tty,c_yes,sourceName,procedureName &
        !           ,c_fatal,' Error making grib2 inventory, check it!')
    
        ! get number of first var levels using grb2_inq from wgrib2 lib
        nprz = grb2_inq(fName,fNameI,trim(varname(1)),' mb:')
        allocate(slevs(nprz),levpr_grib2(nprz),slevsInv(nprz))
        !
        print *,'nprz=',nprz
        !
        do i = 1,nprz
          ! get pressure leves and levels labels using grb2_inq from wgrib2 lib
          iErrNumber=grb2_inq(fName,fNameI,trim(varname(1)),' mb:' &
                   ,sequential=i-1,desc=metadata)
          if (iErrNumber.ne.1) iErrNumber=dumpMessage(c_tty,c_yes,sourceName,procedureName &
                   ,c_fatal,' Error getting pressure levels from grib2 file, check it!')
    
          j = index(metadata,trim(varname(1))) + len(trim(varname(1)))
          k = index(metadata," mb:") + len(" mb:")-1
          !Transform text values to real for each level
          read(metadata(j:),*) levpr_grib2(i)
          !Fill array with level name+suffix
          slevs(i) = metadata(j-1:k)
        enddo
        !
        !print *,index(metadata," mb:"),index(metadata,'hour')
        !print *,'hour: ',metadata(index(metadata," mb:")+4:index(metadata,'hour')-1)
        !print *,'slevs=',slevs
        print *,'APssei aqui levpr='
        !
        !Getting size and geo information in GRIB2 file. Using the first variable (U)
        iErrNumber = grb2_inq(fName,fNameI,trim(varName(1)) &
                   , data2=var &
                   , lat=lat, lon=lon, grid_desc=grid_info, desc=invline)

        !Closing grib2 file to save memory
        iErrNumber = grb2_free_file(fName)
    
        !print *,grid_info
        !print *,invline
    
        print *,'Passei no var'
    
        nxGrib=size(var,1)
        nyGrib=size(var,2)
    
        print *,nxGrib,nyGrib
    
        ! Increments lat and lon
        dy=(lat(1,nyGrib)-lat(1,1))/(nyGrib-1)
        dx=(lon(nxGrib,1)-lon(1,1))/(nxGrib-1)
    
        print *,dx,dy
    
        do j=1,nyGrib
            if(lat(1,j)>initial_latitude) then
                latIni=lat(1,j)
                jIni=j
                exit 
            endif
        enddo
        do j=jINi,nyGrib
            if(lat(1,j)<final_latitude) then
                latFin=lat(1,j)
                jFin=j 
            endif
        enddo
        do i=1,nxGrib
            if(lon(i,1)>initial_longitude) then
                lonIni=lon(i,1)
                iIni=i
                exit 
            endif
        enddo
        do i=iINi,nxGrib
            if(lon(i,1)<final_longitude) then
                lonFin=lon(i,1)
                iFin=i 
            endif
        enddo
        npi=iFin-iIni+1
        npj=jFin-jIni+1
        print *,jIni,jFin,latIni,latFin,npj
        print *,iIni,iFin,lonIni,lonFin,npi
    
        !convert date string got from grib2 file to integer
        read(invline( 3: 6),*,iostat=iErrNumber)  iyear
        read(invline( 7: 8),*,iostat=iErrNumber)  imonth
        read(invline( 9:10),*,iostat=iErrNumber)  iday
        read(invline(11:12),*,iostat=iErrNumber)  ihour

        call date_add_to_dble(iyear,imonth,iday,ihour,dble(atmosHoursCount(iStep)),'h' &
                       ,iyy,imm,idd,ihh)
        print *,'Date added'
        atmosDate(iStep)%year=iyy
        atmosDate(iStep)%month=imm
        atmosDate(iStep)%day=idd
        atmosDate(iStep)%hour=ihh/10000
        Print *,'AtmosDate filled'
        select case(atmosDate(iStep)%month)
            case(12,1,2)
                season = 1
            case(3,4,5)
                season = 2
            case(6,7,8)
                season = 3
            case(9,10,11)
                season = 4
        end select
        print *,'season=',season
        !Adjustin dpatmos type vars
        atmosNz=levels
        atmosNx=npi
        atmosNy=npj                  
        atmosLon(1)=lonIni-360.0
        atmosLat(1)=latIni
        atmosXs=dx
        atmosYs=dy
        print *,'Passo novo'
        if(.not. allocated(atmosLevels)) allocate(atmosLevels(atmosNz))
        do lv=1,nprz
          lvi=nprz-lv+1
          if(lvi>atmosNz) cycle
          atmosLevels(lvi)=levpr_grib2(lv)
          slevsInv(lvi)=slevs(lv)
        enddo
        print *,'atmosLevels allocated'
        if(iStep==1) then
            atmosLat(2) = atmosLat(1) + (atmosYs*atmosNy)
            atmosLon(2) = atmosLon(1) + (atmosXs*atmosNx)
            iErrNumber=dumpMessage(c_tty,c_yes,'','',c_notice,'Grib2 sizes and levels inventory: ')
            write(*,fmt='("Ntimes: ",I3.3," Nvars: ",I3.3)') 1,5
            write(*,fmt='("NLons : ",I3.3," NLats: ",I3.3," NLevs : ",I3.3)') atmosNx,atmosNy,atmosNz
            write(*,fmt='("LatI  : ",F8.2," LatF : ",F8.2," DeltaY: ",F6.3)') atmosLat(1),atmosLat(2),atmosYs
            write(*,fmt='("LonI  : ",F8.2," LonF : ",F8.2," DeltaX: ",F6.3)') atmosLon(1),atmosLon(2),atmosXs
            write(ncl,fmt='(I3.3)') atmosNz
            write(*,fmt='("Levels: ",'//ncl//'(F6.1,1X))') atmosLevels             
        endif        
        
        atmosVarNames = (/wind_u_varname,wind_v_varname,temperature_varname &
                             ,geo_varname,ur_varname/)
        print *,'atmosVarNames filled' 
        if(.not. allocated(atmosValues)) then
            print *,'Allocating atmosValues, MinAtmosValues and MaxAtmosValues arrays'
            allocate(atmosValues(atmosNv,atmosNx, atmosNy, atmosNz), &
                MinAtmosValues(atmosNv,4,6,atmosNz),MaxAtmosValues(atmosNv,4,6,atmosNz))
                MaxAtmosValues = 1.0e-36
                MinAtmosValues = 1.0e+36
        end if
        
        

!        lunit=getUnit()
!        open(unit=lunit,file=fNameB,action='write',status='replace')
    
        do lv=1,levels!nprz
          !lvi=lv !nprz-lv+1
          !if(lvi>atmosnZ) cycle
          write(clvi,fmt='(I3.3)') lv
          !Compose name of var to search inside grib2
          varName(1)=':'//trim(wind_u_varname)//trim(slevsInv(lv))    
          varName(2)=':'//trim(wind_v_varname)//trim(slevsInv(lv)) 
          varName(3)=':'//trim(temperature_varname)//trim(slevsInv(lv)) 
          varName(4)=':'//trim(geo_varname)//trim(slevsInv(lv))   
          varName(5)=':'//trim(ur_varname)//trim(slevsInv(lv)) 
          write(*,fmt='(I2.2,5(A15,1X))') lv,trim(varName(1)),trim(varname(2)),trim(varName(3)),trim(varName(4)),trim(varName(5))    
          do nvar=1,5 
            !iErrNumber=dumpMessage(c_tty,c_yes,'','',c_notice,'Getting: '//varName(nvar))
            !Getting values for variable nvar 
            iErrNumber = grb2_inq(fName,fNameI,trim(varName(nVar)) &
                   ,data2=var, lat=lat, lon=lon)
            ii=0
            jj=0
            !Fill values only inside valid area
            do i=iIni,iFin
              ii=ii+1
              jj=0
              do j=jIni,jFin
                jj=jj+1
                atmosValues(nVar,ii,jj,lv)=var(i,j)
                if (lat(ii,jj)<60.0) then !Faxia 1 -90 a -60.25    
                    if(atmosValues(nVar,ii,jj,lv)<MinAtmosValues(nVar,season,1,lv)) &
                    MinAtmosValues(nVar,season,1,lv)=atmosValues(nVar,ii,jj,lv)
                    if(atmosValues(nVar,ii,jj,lv)>MaxAtmosValues(nVar,season,1,lv)) &
                    MaxAtmosValues(nVar,season,1,lv)=atmosValues(nVar,ii,jj,lv)
                elseif(lat(ii,jj)<30.0) then !Faxia 2 -60 a -30.25
                    if(atmosValues(nVar,ii,jj,lv)<MinAtmosValues(nVar,season,2,lv)) &
                    MinAtmosValues(nVar,season,2,lv)=atmosValues(nVar,ii,jj,lv)
                    if(atmosValues(nVar,ii,jj,lv)>MaxAtmosValues(nVar,season,2,lv)) &
                    MaxAtmosValues(nVar,season,2,lv)=atmosValues(nVar,ii,jj,lv)
                elseif(lat(ii,jj)<0.0) then !Faxia 3 -30 a 0.25
                    if(atmosValues(nVar,ii,jj,lv)<MinAtmosValues(nVar,season,3,lv)) &
                    MinAtmosValues(nVar,season,3,lv)=atmosValues(nVar,ii,jj,lv)
                    if(atmosValues(nVar,ii,jj,lv)>MaxAtmosValues(nVar,season,3,lv)) &
                    MaxAtmosValues(nVar,season,3,lv)=atmosValues(nVar,ii,jj,lv)
                elseif(lat(ii,jj)<30.0) then !Faxia 4 0 a 29.75
                    if(atmosValues(nVar,ii,jj,lv)<MinAtmosValues(nVar,season,4,lv)) &
                    MinAtmosValues(nVar,season,4,lv)=atmosValues(nVar,ii,jj,lv)
                    if(atmosValues(nVar,ii,jj,lv)>MaxAtmosValues(nVar,season,4,lv)) &
                    MaxAtmosValues(nVar,season,4,lv)=atmosValues(nVar,ii,jj,lv)
                elseif(lat(ii,jj)<60.0) then !Faxia 5 30 a 59.75
                    if(atmosValues(nVar,ii,jj,lv)<MinAtmosValues(nVar,season,5,lv)) &
                    MinAtmosValues(nVar,season,5,lv)=atmosValues(nVar,ii,jj,lv)
                    if(atmosValues(nVar,ii,jj,lv)>MaxAtmosValues(nVar,season,5,lv)) &
                    MaxAtmosValues(nVar,season,5,lv)=atmosValues(nVar,ii,jj,lv)
                else !Faxia 6 60 a 90
                    if(atmosValues(nVar,ii,jj,lv)<MinAtmosValues(nVar,season,6,lv)) &
                    MinAtmosValues(nVar,season,6,lv)=atmosValues(nVar,ii,jj,lv)
                    if(atmosValues(nVar,ii,jj,lv)>MaxAtmosValues(nVar,season,6,lv)) &
                    MaxAtmosValues(nVar,season,6,lv)=atmosValues(nVar,ii,jj,lv)
                endif
              enddo
            enddo
          enddo
        end do
    
        !Closing grib2 file to save memory
        !print *, 'Closing ',trim(file)
        iErrNumber = grb2_free_file(fName)
!        iErrNumber = releaseUnit(lunit)
    
    end subroutine readAtmosGrib2 

    function get_basename_noext(fullpath) result(basename)
        implicit none
        character(len=*), intent(in) :: fullpath
        character(len=:), allocatable :: basename

        integer :: last_slash, last_dot, start_pos

        ! 1. Encontrar a última barra
        last_slash = scan(fullpath, '/', back=.true.)
        if (last_slash == 0) then
            start_pos = 1               ! não há barra, começa do início
        else
            start_pos = last_slash + 1  ! posição do primeiro caractere após a barra
        end if

        ! 2. Encontrar o último ponto a partir da posição start_pos
        last_dot = scan(fullpath(start_pos:), '.', back=.true.)
        if (last_dot == 0) then
            ! Sem extensão, retorna toda a substring a partir de start_pos
            basename = fullpath(start_pos:)
        else
            ! Ajustar last_dot para índice global
            last_dot = last_dot + start_pos - 1
            basename = fullpath(start_pos:last_dot-1)
        end if

    end function get_basename_noext

    !=============================================================================================
    subroutine writeGradsCtlFile(iTime,nSpc,values,nx,ny,nz,lon,lat,dlon,dlat,levs)
        !# Write the output in grads and ctl files
        !#
        !# @note
        !# ![](http://brams.cptec.inpe.br/wp-content/uploads/2015/11/logo-brams.navigation.png "")
        !#
        !# **Brief**: write output in grads and ctl file with meteorological and chem data
        !#
        !# **Documentation**: <http://brams.cptec.inpe.br/documentation/>
        !#
        !# **Author(s)**: Luiz Flavio Rodrigues **&#9993;**<luiz.rodrigues@inpe.br>
        !#
        !# **Date**: 27 August 2020 (Thursday)
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
        !# &copy; <https://creativecommons.org/licenses/GPL/2.0/legalcode.pt>
        !# @endwarning
        !#
        
        !Use area
        use dump

        use memoryMod, only: &
            out_type, &
            out_prefix, &
            out_sufix, &
            out_dir, &
            atmosNx, &
            atmosNy, &
            atmosNz, &
            atmosLat, &
            atmosLon, &
            atmosXs, &
            atmosYs, &
            atmosErrors, &
            atmosVarNames, &
            atmosDate, &
            atmosNv, &
            atmosLevels , &
            spcName, &
            chem_type 

        use utilsMod, only: &
            getUnit , &
            releaseUnit, &
            outRealSize
    
        implicit none
    
        include "constants.h"
        character(len=*),parameter :: procedureName='**writeGradsCtlFile**' !Name of this procedure
        !
        !Local Parameters
    
        !Input/Output variables
        integer :: iTime
        !#
        integer, intent(in) :: nSpc
        !#
        real, intent(in) :: values(nSpc,atmosNx,atmosNy,atmosNz)
        !#
        integer, intent(in) :: nx,ny,nz
        !#
        real,intent(in) :: lon
        !#
        real,intent(in) :: lat
        !#
        real,intent(in) :: dlon
        !#
        real,intent(in) :: dlat
        !#
        real,intent(in) :: levs(nz)
        !#

        !Local variables
        character(len=256) :: ctlFileName
        character(len=256) :: binFileName
        character(len=15) :: tDef
        integer :: TotalVars,nVar,lunit,recordLen
        integer :: iRec,nnz
    
        !Code
        !SUm the amount of vars
        !if(chem_type>=1) then
        !    TotalVars=atmosNv+nSpc
        !else
        TotalVars=atmosNv
        !endif
        !Creating output names
        out_prefix="gfs"
        out_sufix="errors"
        out_dir="./"
        write(ctlFileName,fmt='(A,I4.4,I2.2,I2.2,I2.2,A)') trim(out_prefix),atmosDate(ITime)%year,atmosDate(Itime)%month &
                                    ,atmosDate(iTime)%day,atmosDate(iTime)%hour,trim(out_sufix)//'.ctl'
        write(binFileName,fmt='(A,I4.4,I2.2,I2.2,I2.2,A)') trim(out_prefix),atmosDate(ITime)%year,atmosDate(Itime)%month &
                                    ,atmosDate(iTime)%day,atmosDate(iTime)%hour,trim(out_sufix)//'.gra'

        write(tDef,fmt='(I2.2,":00z",I2.2,A3,I4.4)')  atmosDate(iTime)%hour,atmosDate(iTime)%day &
                                                      ,month_Name(atmosDate(Itime)%month),atmosDate(ITime)%year
        !Writing ctl file
        lunit=getUnit()
        open(unit=lunit, file=trim(out_dir)//trim(ctlFileName), action='write', status='replace')

        !writing the name of grads file
        write(lunit,*) 'dset ^'//trim(binFileName)
        !writing others infos to ctl
        write(lunit,*) 'undef -0.9990000E+34'
        write(lunit,*) 'title Check GFS'
        write(lunit,*) 'xdef ',Nx-1,' linear ',lon+dlon,dlon
        write(lunit,*) 'ydef ',Ny,' linear ',lat,dlat
        write(lunit,*) 'zdef ',Nz,'levels',levs
        write(lunit,*) 'tdef 1 linear '//tDef//' 1mo'
        write(lunit,*) 'vars ',TotalVars


        do nvar=1,atmosNv
            write(lunit,*) atmosVarNames(nvar),atmosNz,'99 ',atmosVarNames(nvar)
        enddo
        !if(chem_type>=1) then
        !    do nvar=1,nSpc
        !        write(lunit,*) spcName(nvar),nz,'99 ',spcName(nvar)
        !    enddo
        !endif
        write(lunit,*) 'endvars'
        ierrNumber=releaseUnit(lunit)

        iErrNumber=dumpMessage(c_tty,c_yes,'','',c_notice,'writing grads/ctl file for '//tDef)
        recordLen=outRealSize()*(nx-1)*ny
        iErrNumber=dumpMessage(c_tty,c_yes,'','',c_notice,'dimensions nx,ny,recordLen ',(/nx,ny,recordLen/),"I10")
        lunit=getUnit()
        open(unit=lunit,file=trim(out_dir)//trim(binFileName),action='WRITE',status='REPLACE' &
             ,form='UNFORMATTED',access='DIRECT',recl=recordLen)

        irec=1

        do nvar=1,atmosNv
            do nnz=1,atmosNz
                write(lunit,rec=irec) atmosErrors(iTime,nvar,2:nx,:,nnz)
                irec=irec+1
            enddo
        enddo
        
        !if(chem_type>=1) then
        !    do nvar=1,nSpc
        !        do nnz=1,atmosNz
        !            write(lunit,rec=irec) values(nvar,2:nx,:,nnz)
        !            irec=irec+1
        !        enddo
        !    enddo
        !endif

    end subroutine writeGradsCtlFile 
  
  !lê o arquivo e preenche data_stat
  ! data_stat(1:4, 1:5)  -> 1:DJF, 2:MAM, 3:JJA, 4:SON
  !                        -> 1:TMP, 2:UME, 3:UZN, 4:VMD, 5:GEO
  ! --------------------------------------------------------------------
      subroutine read_era5_data(filename)
        use memoryMod, only: &
            data_stat

        implicit none
        character(len=*), intent(in) :: filename

        integer :: unit, ios
        integer :: idx_s, idx_v
        character(len=3) :: season, var
        real    :: level, lat_min, lat_max, min_value, max_value
        character(len=200) :: line

        ! Abre o arquivo
        open(newunit=unit, file=filename, status='old', action='read', iostat=ios)
        if (ios /= 0) then
           write(*,*) 'Erro ao abrir arquivo: ', trim(filename)
           return
        end if

        ! Pula a linha de cabeçalho
        read(unit, *, iostat=ios)
        if (ios /= 0) then
           write(*,*) 'Erro ao ler cabeçalho'
           close(unit)
           return
        end if

        ! Leitura linha a linha
        do
           read(unit, '(A)', iostat=ios) line
           if (ios /= 0) exit
           if (len_trim(line) == 0) cycle

           ! Leitura dos campos via list-directed (vírgulas são separadores)
           read(line, *, iostat=ios) season, var, level, lat_min, lat_max, min_value, max_value
           if (ios /= 0) then
              write(*,*) 'Aviso: linha ignorada (erro de leitura): ', trim(line)
              cycle
           end if

           ! Mapeia estação e variável para índices
           idx_s = season_index(season)
           idx_v = var_index(var)
           if (idx_s == -1 .or. idx_v == -1) then
              write(*,*) 'Aviso: estação/variável desconhecida: ', season, var
              cycle
           end if

           ! Armazena os valores nos arrays alocáveis da combinação correspondente
           call append_real(data_stat(idx_s, idx_v)%level, level)
           call append_real(data_stat(idx_s, idx_v)%lat_min, lat_min)
           call append_real(data_stat(idx_s, idx_v)%lat_max, lat_max)
           call append_real(data_stat(idx_s, idx_v)%min_value, min_value)
           call append_real(data_stat(idx_s, idx_v)%max_value, max_value)
        end do

        close(unit)

      contains

        ! ----------------------------------------------------------------
        ! Retorna o índice da estação (1..4) ou -1 se desconhecida
        ! ----------------------------------------------------------------
        function season_index(s) result(idx)
          character(len=3), intent(in) :: s
          integer :: idx
          select case(s)
          case('DJF'); idx = 1
          case('MAM'); idx = 2
          case('JJA'); idx = 3
          case('SON'); idx = 4
          case default; idx = -1
          end select
        end function season_index

        ! ----------------------------------------------------------------
        ! Retorna o índice da variável (1..5) ou -1 se desconhecida
        ! ----------------------------------------------------------------
        function var_index(s) result(idx)
          character(len=3), intent(in) :: s
          integer :: idx
          select case(s)
          case('TMP'); idx = 1
          case('UME'); idx = 2
          case('UZN'); idx = 3
          case('VMD'); idx = 4
          case('GEO'); idx = 5
          case default; idx = -1
          end select
        end function var_index

        ! ----------------------------------------------------------------
        ! Adiciona um valor real a um array alocável (realocação automática)
        ! ----------------------------------------------------------------
        subroutine append_real(arr, val)
          real, allocatable, intent(inout) :: arr(:)
          real, intent(in) :: val
          if (.not. allocated(arr)) then
             allocate(arr(1))
             arr(1) = val
          else
             arr = [arr, val]   ! concatenação com realocação implícita
          end if
        end subroutine append_real

      end subroutine read_era5_data



end module filesMod 
