!=============================================================================================
module utilsMod
    !# Module with a sort of utilities
    !#
    !# @note
    !# ![](http://brams.cptec.inpe.br/wp-content/uploads/2015/11/logo-brams.navigation.png "")
    !#
    !# **Brief**: utilities are
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
    character(len=*),parameter :: sourceName='utils.f90' !Name of source code
    character(len=*),parameter :: procedureName='**utils**' !Name of this procedure
    !
    !Local Parameters
    !integer, parameter :: i8 = selected_int_kind(14)   !Kind for 64-bits Integer Numbers
    !integer, parameter :: r8 = selected_real_kind(15)  !Kind for 64-bits Real Numbers
    !Local variables

    logical :: fileUnits(20:99)
    logical :: firstTime

    Contains

    ! --------------------------------------------------------------
    ! Verifica se um ano é bissexto (calendário gregoriano)
    ! --------------------------------------------------------------
    pure logical function bissexto(ano)
        integer, intent(in) :: ano
        bissexto = (mod(ano, 400) == 0) .or. (mod(ano, 4) == 0 .and. mod(ano, 100) /= 0)
    end function bissexto

    ! --------------------------------------------------------------
    ! Retorna o número de dias de um determinado mês/ano
    ! --------------------------------------------------------------
    pure integer function dias_mes(ano, mes)
        integer, intent(in) :: ano, mes
        integer, dimension(12), parameter :: dias_por_mes = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
        if (mes == 2 .and. bissexto(ano)) then
            dias_mes = 29
        else
            dias_mes = dias_por_mes(mes)
        end if
    end function dias_mes

    ! --------------------------------------------------------------
    ! Função principal: retorna um array com todos os caminhos
    ! --------------------------------------------------------------
    function gerar_lista(prefixo, ano_inicio, ano_fim) result(lista)
        character(len=*), intent(in) :: prefixo
        integer, intent(in) :: ano_inicio, ano_fim
        character(len=256), allocatable :: lista(:)

        integer :: ano, mes, dia, hora, idx
        integer :: total
        character(len=4)  :: ano_str
        character(len=2)  :: mes_str, dia_str, hora_str
        character(len=8)  :: data_str
        character(len=10) :: data_hora_str

        ! Calcula o número total de arquivos (4 horários por dia)
        total = 0
        do ano = ano_inicio, ano_fim
            do mes = 1, 12
                total = total + dias_mes(ano, mes) * 4
            end do
        end do

        allocate(lista(total))

        ! Preenche a lista
        idx = 0
        do ano = ano_inicio, ano_fim
            write(ano_str, '(I4.4)') ano
            do mes = 1, 12
                write(mes_str, '(I2.2)') mes
                do dia = 1, dias_mes(ano, mes)
                    write(dia_str, '(I2.2)') dia
                    write(data_str, '(I4.4,I2.2,I2.2)') ano, mes, dia
                    do hora = 0, 18, 6   ! 00, 06, 12, 18
                        write(hora_str, '(I2.2)') hora
                        write(data_hora_str, '(I4.4,I2.2,I2.2,I2.2)') ano, mes, dia, hora
                        idx = idx + 1
                        lista(idx) = trim(prefixo) // '/' // ano_str // '/' // mes_str // '/' // dia_str // '/' // hora_str // &
                                     '/gfs.t' // hora_str // 'z.pgrb2.0p25.f000.' // data_hora_str // '.grib2'
                    end do
                end do
            end do
        end do

    end function gerar_lista


    !=============================================================================================
    integer function initAll()
        !# Initialize utils
        !#
        !# @note
        !# ![](http://brams.cptec.inpe.br/wp-content/uploads/2015/11/logo-brams.navigation.png "")
        !#
        !# **Brief**: initialize utils
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
        character(len=*),parameter :: sourceName='utils.f90' !Name of source code
        character(len=*),parameter :: procedureName='**initAll**' !Name of this procedure
        !
        !Local Parameters
    
        !Input/Output variables
    
        !Local variables
    
        !Code
        fileUnits=.false.
        firstTime=.true.

        initAll=0
    
    end function initAll 

    !=============================================================================================
    integer function getUnit()
        !# Get a free unit to use
        !#
        !# @note
        !# ![](http://brams.cptec.inpe.br/wp-content/uploads/2015/11/logo-brams.navigation.png "")
        !#
        !# **Brief**: Get a free unit to open file. 
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
        character(len=*),parameter :: sourceName='utils.f90' !Name of source code
        character(len=*),parameter :: procedureName='**getUnit**' !Name of this procedure
        !
        !Local Parameters
    
        !Input/Output variables
    
        !Local variables
        integer :: i
    
        !Code
        do i=20,99
            if(.not. fileUnits(i)) then
               getUnit=i
               fileUnits(i)=.true.
               exit
            endif
         enddo

    
    end function getUnit 

    !=============================================================================================
    integer function releaseUnit(unitNum)
        !# Release a unit for file
        !#
        !# @note
        !# ![](http://brams.cptec.inpe.br/wp-content/uploads/2015/11/logo-brams.navigation.png "")
        !#
        !# **Brief**: release a unit for file and close the file unitNum
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
        character(len=*),parameter :: sourceName='utils.f90' !Name of source code
        character(len=*),parameter :: procedureName='**releaseUnit**' !Name of this procedure
        !
        !Local Parameters
    
        !Input/Output variables
        integer, intent(in) :: unitNum
    
        !Local variables
    
        !Code
        if(.not. fileUnits(unitNum)) iErrNumber=dumpMessage(c_tty,c_yes,sourceName,procedureName &
              ,c_fatal,'Unit not used. Please, verify and solve it!',unitNum,"I2.2")
        close(unitNum)
        fileUnits(unitNum)=.false.
    
    end function releaseUnit 


    !=============================================================================================
    integer function bramsHeader(rev) 
        !# write a header in screen
        !#
        !# @note
        !# ![](http://brams.cptec.inpe.br/wp-content/uploads/2015/11/logo-brams.navigation.png "")
        !#
        !# **Brief**: write a header in screen
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
        character(len=*),parameter :: sourceName='utils.f90' !Name of source code
        character(len=*),parameter :: procedureName='**bramsHeader**' !Name of this procedure
        !
        !Input/Output variables
        character(len=*),intent(in) :: rev
    
        !Code
         write (*,fmt='(A)') ''
         write (*,fmt='(A)') '            ######  ######     #    #     #  #####'
         write (*,fmt='(A)') '            #     # #     #   # #   ##   ## #     #'
         write (*,fmt='(A)') '            #     # #     #  #   #  # # # # #'
         write (*,fmt='(A)') '            ######  ######  #     # #  #  #  #####'
         write (*,fmt='(A)') '            #     # #   #   ####### #     #       #'
         write (*,fmt='(A)') '            #     # #    #  #     # #     # #     #'
         write (*,fmt='(A)') '            ######  #     # #     # #     #  #####'
         write (*,fmt='(A)') '------------------------------------------------------------------'
         write (*,fmt='(A)') 'Brazilian developments on the Regional Atmospheric Modeling System'
         write (*,fmt='(A)') '                   PREP CHEM APP - Rev. '//rev
         write (*,fmt='(A)') '------------------------------------------------------------------' 
         write (*,fmt='(A)') ''

         bramsHeader=0
    
    end function bramsHeader 

    !=============================================================================================
    function to_upper(strIn) result(strOut)
        !# Convert case to upper case
        !#
        !# @note
        !# ![](http://brams.cptec.inpe.br/wp-content/uploads/2015/11/logo-brams.navigation.png "")
        !#
        !# **Brief**: Convert case to upper
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
        character(len=*),parameter :: sourceName='utils.f90' !Name of source code
        character(len=*),parameter :: procedureName='**to_upper**' !Name of this procedure
        !
        !Local Parameters
    
        !Input/Output variables
        character(*), intent(in) :: strIn
        !# String to be converted
        character(len=len(strIn)) :: strOut
        !# Return string converted
     
        !Local variables
        integer :: i
        !Code
        do i = 1, len(strIn)
            select case(strIn(i:i))
            case("a":"z")
               strOut(i:i) = achar(iachar(strIn(i:i))-32)
            end select
         end do
    
    end function to_upper 

    !=============================================================================================
    function to_lower(strIn) result(strOut)
        !# Convert case to lower case
        !#
        !# @note
        !# ![](http://brams.cptec.inpe.br/wp-content/uploads/2015/11/logo-brams.navigation.png "")
        !#
        !# **Brief**: Convert case to lower case
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
        character(len=*),parameter :: sourceName='utils.f90' !Name of source code
        character(len=*),parameter :: procedureName='**to_lower**' !Name of this procedure
        !
        !Local Parameters
    
        !Input/Output variables
        character(*), intent(in) :: strIn
        !# String to be converted
        character(len=len(strIn)) :: strOut
        !# Return string converted
     
        !Local variables
        integer :: i
        !Code
        do i = 1, len(strIn)
            select case(strIn(i:i))
            case("A":"Z")
               strOut(i:i) = achar(iachar(strIn(i:i))+32)
            end select
         end do
    
    end function to_lower

   !=============================================================================================
   integer function julday (imonth,iday,iyear)
      !# returns which day of the year is the input date
      !#
      !# @note
      !# ![](http://brams.cptec.inpe.br/wp-content/uploads/2015/11/logo-brams.navigation.png "")
      !#
      !# **Brief**: returns which day of the year is the input date
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
        
      ! 

      integer, intent(in) :: imonth
      integer, intent(in) :: iday
      integer, intent(in) :: iyear
   
      julday= iday  &
           + min(1,max(0,imonth-1))*31  &
           + min(1,max(0,imonth-2))*(28+(1-min(1,mod(iyear,4))))  &
           + min(1,max(0,imonth-3))*31  &
           + min(1,max(0,imonth-4))*30  &
           + min(1,max(0,imonth-5))*31  &
           + min(1,max(0,imonth-6))*30  &
           + min(1,max(0,imonth-7))*31  &
           + min(1,max(0,imonth-8))*31  &
           + min(1,max(0,imonth-9))*30  &
           + min(1,max(0,imonth-10))*31  &
           + min(1,max(0,imonth-11))*30  &
           + min(1,max(0,imonth-12))*31
   
   end function julday

   !=============================================================================================
   subroutine date_make_big (inyear,inmonth,indate,inhour,outdate)
      !# convert integers year, month, date and hour into YYYYMMDDHHHHHH
      !#
      !# @note
      !# ![](http://brams.cptec.inpe.br/wp-content/uploads/2015/11/logo-brams.navigation.png "")
      !#
      !# **Brief**: convert integers representing year, month, date and hour into
      !#  a character string YYYYMMDDHHHHHH
      !# input hour is an integer with 6 digits in the form HHMMSS(hour, minute, second)
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

      integer,           intent(in ) :: inyear
      integer,           intent(in ) :: inmonth
      integer,           intent(in ) :: indate
      integer,           intent(in ) :: inhour
      character(len=14), intent(out) :: outdate
   
      write(outdate(1:4), "(i4.4)") inyear
      write(outdate(5:6), "(i2.2)") inmonth
      write(outdate(7:8), "(i2.2)") indate
      write(outdate(9:14),"(i6.6)") inhour
   
   end subroutine date_make_big

   !=============================================================================================
   subroutine date_unmake_big (inyear,inmonth,indate,inhour,outdate)
      !# Convert a character string YYYYMMDDHHHHHH into integers
      !#
      !# @note
      !# ![](http://brams.cptec.inpe.br/wp-content/uploads/2015/11/logo-brams.navigation.png "")
      !#
      !# **Brief**: convert a character string YYYYMMDDHHHHHH into integers
      !# representing year, month, date and hour.
      !# output hour is an integer with 6 digits in the form HHMMSS
      !# (hour, minute, second)
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

      integer,           intent(out) :: inyear
      integer,           intent(out) :: inmonth
      integer,           intent(out) :: indate
      integer,           intent(out) :: inhour
      character(len=14), intent(in ) :: outdate
   
      read(outdate(1:4), "(i4)") inyear
      read(outdate(5:6), "(i2)") inmonth
      read(outdate(7:8), "(i2)") indate
      read(outdate(9:14),"(i6)") inhour
   
   end subroutine date_unmake_big

   !=============================================================================================
   subroutine date_abs_secs(indate1, seconds)
      !# compute number of seconds past 1 January 1900 12:00 am to string
      !#
      !# @note
      !# ![](http://brams.cptec.inpe.br/wp-content/uploads/2015/11/logo-brams.navigation.png "")
      !#
      !# **Brief**: compute number of seconds past 1 January 1900 12:00 am
      !# from an input string in the form YYYYMMDDHHHHHH
      !# returns a real(kind=r8)
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

      use dump, only: &
         dumpMessage

      implicit none
   
      include "constants.h"
      ! 
   
      character(len=14), intent(in ) :: indate1
      real(kind=r8),     intent(out) :: seconds
   
      real(kind=r8) :: s1, s2, s3, s4
      integer :: year1, month1, date1, hour1, iy, ndays
      character(len=8) :: c0
      character(len=*), parameter :: h="**(date_abs_secs)**"
   
      call date_unmake_big(year1, month1, date1, hour1, indate1)
   
      if (year1 < 1900) then
         write(c0,"(i8)") year1
         !call fatal_error(h//" input year should be <= 1970; it was "&
         !      &//trim(adjustl(c0)))
         iErrNumber=dumpMessage(c_tty,c_yes,h,modelVersion,c_fatal, &
                   " input year should be <= 1970; it was "&
                        &//trim(adjustl(c0)))
      end if
   
      iy      = year1 - 1900
      ndays   = iy*365 + max(0,(iy-1)/4) + julday(month1,date1,iy)
      s1      = dble(ndays)*86400.
      s2      = dble(hour1/10000)*3600.
      s3      = dble(mod(hour1,10000)/100)*60.
      s4      = dble(mod(hour1,100))
      seconds = s1 + s2 + s3 + s4
  
   end subroutine date_abs_secs

   !=============================================================================================
   subroutine date_abs_secs2 (year1,month1,date1,hour1,seconds)
      !# compute number of seconds past 1 January 1900 12:00 am to integer
      !#
      !# @note
      !# ![](http://brams.cptec.inpe.br/wp-content/uploads/2015/11/logo-brams.navigation.png "")
      !#
      !# **Brief**: compute number of seconds past 1 January 1900 12:00 am
      !# from integers representing year, month, date and hour
      !# returns a real(kind=r8)
      !# input hour is an integer with 6 digits in the form HHMMSS
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

      use dump, only: &
         dumpMessage

      implicit none
   
      include "constants.h"
      ! 
   
      integer,       intent(in ) :: year1
      integer,       intent(in ) :: month1
      integer,       intent(in ) :: date1
      integer,       intent(in ) :: hour1
      real(kind=r8), intent(out) :: seconds
   
      real(kind=r8) :: s1,s2,s3,s4
      integer :: iy,ndays
      character(len=8) :: c0
      character(len=*), parameter :: h="**(date_abs_secs2)**"
   
      if (year1 < 1900) then
         write(c0,"(i8)") year1
         !call fatal_error(h//" input year should be <= 1970; it was "&
        !      &//trim(adjustl(c0)))
         iErrNumber=dumpMessage(c_tty,c_yes,h,modelVersion,c_fatal, &
                        " input year should be <= 1970; it was "&
                             &//trim(adjustl(c0)))
      end if
   
      iy = year1 - 1900
      ndays = iy * 365 + max(0,(iy-1)/4) + julday(month1,date1,iy)
      s1= dble(ndays) *86400.
      s2= dble(hour1/10000)*3600.
      s3= dble(mod(hour1,10000)/100)*60.
      s4= dble(mod(hour1,100))
      seconds= s1+s2+s3+s4
   
   end subroutine date_abs_secs2

   !=============================================================================================
   subroutine date_secs_ymdt (seconds,iyear1,imonth1,idate1,ihour1)
      !# compute real time given number of seconds past 1 January 1900 12:00 am
      !#
      !# @note
      !# ![](http://brams.cptec.inpe.br/wp-content/uploads/2015/11/logo-brams.navigation.png "")
      !#
      !# **Brief**: compute real time given number of seconds past 1 January 1900 12:00 am
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
      real(kind=r8), intent(in ) :: seconds
      integer,       intent(out) :: iyear1
      integer,       intent(out) :: imonth1
      integer,       intent(out) :: idate1
      integer,       intent(out) :: ihour1

      integer, parameter :: mondays(12)=&
           (/31,28,31,30,31,30,31,31,30,31,30,31/)
      integer :: ny,nyr,ileap,nm,nd,ihr,imn,isc
      real(kind=r8) :: s1
   
      ! Get what year it is
   
      s1=seconds
      do ny=0,10000
         ileap=0
         if(mod(1900+ny,4) == 0) ileap=1
         s1=s1-(365.+ileap)*86400.
         if(s1 < 0.) then
            nyr=ny
            s1=s1+(365.+ileap)*86400.
            exit
         endif
      enddo
      iyear1=1900+nyr
   
      ! s1 is now number of secs into the year
      !   Get month
   
      do nm=1,12
         ileap=0
         if(mod(1900+ny,4) == 0 .and. nm == 2) ileap=1
         s1=s1-(mondays(nm)+ileap)*86400.
         if(s1 < 0.) then
            s1=s1+(mondays(nm)+ileap)*86400.
            exit
         endif
      enddo
      imonth1=nm
   
      ! s1 is now number of secs into the month
      !   Get date and time
   
      idate1=int(s1/86400.)
      s1=s1-idate1*86400.
      idate1=idate1+1 ! Since date starts at 1
   
      ihr=int(s1/3600.)
      s1=s1-ihr*3600.
      imn=int(s1/60.)
      s1=s1-imn*60.
      isc=s1
      ihour1=ihr*10000+imn*100+isc

   end subroutine date_secs_ymdt

   !=============================================================================================
   subroutine date_add_to_big (cindate,tinc,tunits,coutdate)
      !# adds/subtracts a time increment to a date and output new date
      !#
      !# @note
      !# ![](http://brams.cptec.inpe.br/wp-content/uploads/2015/11/logo-brams.navigation.png "")
      !#
      !# **Brief**: adds/subtracts a time increment to a date and output new date
      !#   uses hhmmss for hours, 4 digit year
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
      character(len=14), intent(in ) :: cindate
      real,              intent(in ) :: tinc
      character(len=1),  intent(in ) :: tunits
      character(len=14), intent(out) :: coutdate
   
      real(kind=8) :: ttinc,secs
      integer :: inyear,inmonth,indate,inhour  &
           ,outyear,outmonth,outdate,outhour
   
      ! convert input increment to seconds
   
      select case(tunits)
      case("d","D")
         ttinc = tinc*86400.0
      case("h","H")
         ttinc = tinc*3600.0
      case("m","M")
         ttinc = tinc*60.0
      case default
         ttinc = tinc
      end select
   
      ! convert input time to seconds
   
      call date_unmake_big(inyear,inmonth,indate,inhour,cindate)
   
      call date_abs_secs2(inyear,inmonth,indate,inhour,secs)
   
      secs=secs+ttinc
   
      call date_secs_ymdt(secs,outyear,outmonth,outdate,outhour)
      call date_make_big(outyear,outmonth,outdate,outhour,coutdate)

   end subroutine date_add_to_big

   !=============================================================================================
   subroutine date_add_to (inyear,inmonth,indate,inhour,  &
       tinc,tunits,outyear,outmonth,outdate,outhour)
      !# convert input increment to seconds
      !#
      !# @note
      !# ![](http://brams.cptec.inpe.br/wp-content/uploads/2015/11/logo-brams.navigation.png "")
      !#
      !# **Brief**: convert input increment to seconds
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
      integer,          intent(in ) :: inyear
      integer,          intent(in ) :: inmonth
      integer,          intent(in ) :: indate
      integer,          intent(in ) :: inhour
      real,             intent(in ) :: tinc
      character(len=1), intent(in ) :: tunits
      integer,          intent(out) :: outyear
      integer,          intent(out) :: outmonth
      integer,          intent(out) :: outdate
      integer,          intent(out) :: outhour
   
      real(kind=8) :: ttinc,secs
   
      ! convert input increment to seconds
   
      select case(tunits)
      case("d","D")
         ttinc = tinc*86400.0
      case("h","H")
         ttinc = tinc*3600.0
      case("m","M")
         ttinc = tinc*60.0
      case default
         ttinc = tinc
      end select
   
      ! convert input time to seconds
   
      call date_abs_secs2(inyear,inmonth,indate,inhour,secs)
   
      ! add increment
   
      secs=secs+ttinc
   
      ! convert seconds into date
   
      call date_secs_ymdt(secs,outyear,outmonth,outdate,outhour)
  
   end subroutine date_add_to

   !=============================================================================================
   subroutine date_add_to_dble (inyear,inmonth,indate,inhour,  &
       tinc,tunits,outyear,outmonth,outdate,outhour)
      !# convert input increment to seconds
      !#
      !# @note
      !# ![](http://brams.cptec.inpe.br/wp-content/uploads/2015/11/logo-brams.navigation.png "")
      !#
      !# **Brief**: convert input increment to seconds
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
      integer,          intent(in ) :: inyear
      integer,          intent(in ) :: inmonth
      integer,          intent(in ) :: indate
      integer,          intent(in ) :: inhour
      real(kind=8),             intent(in ) :: tinc
      character(len=1), intent(in ) :: tunits
      integer,          intent(out) :: outyear
      integer,          intent(out) :: outmonth
      integer,          intent(out) :: outdate
      integer,          intent(out) :: outhour
   
      real(kind=8) :: ttinc,secs
   
      ! 
   
      select case(tunits)
      case("d","D")
         ttinc = tinc*86400.0
      case("h","H")
         ttinc = tinc*3600.0
      case("m","M")
         ttinc = tinc*60.0
      case default
         ttinc = tinc
      end select
   
      ! convert input time to seconds
   
      call date_abs_secs2(inyear,inmonth,indate,inhour,secs)
   
      ! add increment
   
      secs=secs+ttinc
   
      ! convert seconds into date
   
      call date_secs_ymdt(secs,outyear,outmonth,outdate,outhour)
   
   end subroutine date_add_to_dble

   !=============================================================================================
   logical function fileExist(fileName)
       !# Check if fileName exist
       !#
       !# @note
       !# ![](http://brams.cptec.inpe.br/wp-content/uploads/2015/11/logo-brams.navigation.png "")
       !#
       !# **Brief**: Check if fileName exist
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
       character(len=*),parameter :: sourceName='utils.f90' !Name of source code
       character(len=*),parameter :: procedureName='**fileExist**' !Name of this procedure
       !
       !Local Parameters
   
       !Input/Output variables
       character(len=*), intent(in) :: fileName
   
       !Local variables
   
       !Code

       inquire(file=fileName(1:len_trim(fileName)),exist=fileExist)

   
   end function fileExist 

   !=============================================================================================
   integer function stepsBetweenDates(iY,iM,iD,iH,fY,fM,fD,fH,step,cTime)
       !# Calc the number of steps between two dates
       !#
       !# @note
       !# ![](http://brams.cptec.inpe.br/wp-content/uploads/2015/11/logo-brams.navigation.png "")
       !#
       !# **Brief**: Cal the numer of steps between two dates in intial and final
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
       character(len=*),parameter :: sourceName='utils.f90' !Name of source code
       character(len=*),parameter :: procedureName='**stepsBetweenDates**' !Name of this procedure
       !
       !Local Parameters
   
       !Input/Output variables
       integer, intent(in) :: iY
       !# initial year
       integer, intent(in) :: iM
       !# initial month
       integer, intent(in) :: iD
       !# initial day
       integer, intent(in) :: iH
       !# initial hour
       integer, intent(in) :: fY
       !# final year
       integer, intent(in) :: fM
       !# final month
       integer, intent(in) :: fD
       !# final day
       integer, intent(in) :: fH
       !# final hour
       integer, intent(in) :: step
       !# step to be calulated
       character, intent(in) :: cTime
       !# unit of time (h,m,s)
       
       !Local variables
       integer :: iyy,imm,idd,ihh
   
       if(mod(fH,step)/=0) iErrNumber=dumpMessage(c_tty,c_yes,sourceName,procedureName &
              ,c_fatal,' time difference isnt divided by step: ',step,"I2.2")

       stepsBetweenDates=0
       !Code
       do while(.true.)
         stepsBetweenDates=stepsBetweenDates+1
         call date_add_to_dble(iY,iM,iD,iH,dble(step)*(stepsBetweenDates-1),cTime &
                       ,iyy,imm,idd,ihh)

         if(iyy==fY .and. imm==fM .and. idd==fD .and. ihh/10000==fH) exit
       enddo 
   
   end function stepsBetweenDates 

   ! !=============================================================================================
   ! function validateDates(iY,iM,iD,iH,fY,fM,fD,fH,step,cTime,stepsBetDates) result(validDates)
   !     !# Validate dates inside given dates and step in hours
   !     !#
   !     !# @note
   !     !# ![](http://brams.cptec.inpe.br/wp-content/uploads/2015/11/logo-brams.navigation.png "")
   !     !#
   !     !# **Brief**: validate date array inside two given dates and step hour
   !     !#
   !     !# **Documentation**: <http://brams.cptec.inpe.br/documentation/>
   !     !#
   !     !# **Author(s)**: Luiz Flavio Rodrigues **&#9993;**<luiz.rodrigues@inpe.br>
   !     !#
   !     !# **Date**: 28 August 2020 (Friday)
   !     !# @endnote
   !     !#
   !     !# @changes
   !     !# &#9744; <br/>
   !     !# @endchanges
   !     !# @bug
   !     !#
   !     !#@endbug
   !     !#
   !     !#@todo
   !     !#  &#9744; <br/>
   !     !# @endtodo
   !     !#
   !     !# @warning
   !     !# Now is under CC-GPL License, please see
   !     !# &copy; <https://creativecommons.org/licenses/GPL/2.0/legalcode.pt>
   !     !# @endwarning
   !     !#
       
   !     !Use area
   !     use dump
   
   !     implicit none
   
   !     include "constants.h"
   !     character(len=*),parameter :: procedureName='**validateDates**' !Name of this procedure
   !     !
   !     !Local Parameters
   
   !     !Input/Output variables
   !     integer, intent(in) :: iY
   !     !# initial year
   !     integer, intent(in) :: iM
   !     !# initial month
   !     integer, intent(in) :: iD
   !     !# initial day
   !     integer, intent(in) :: iH
   !     !# initial hour
   !     integer, intent(in) :: fY
   !     !# final year
   !     integer, intent(in) :: fM
   !     !# final month
   !     integer, intent(in) :: fD
   !     !# final day
   !     integer, intent(in) :: fH
   !     !# final hour
   !     integer, intent(in) :: step
   !     !# step to be calulated
   !     character, intent(in) :: cTime
   !     !# unit of time (h,m,s)  

   !     integer, intent(in) :: stepsBetDates
   !     !# Number of steps between dates 
   
   !     !Local variables
   !     integer :: validDates(stepsBetDates)
   
   !     !Code
   !     do while(.true.)
   !       sbd=sbd+1
   !       call date_add_to_dble(iY,iM,iD,iH,dble(step)*(sbd-1),cTime &
   !                     ,iyy,imm,idd,ihh)

   !       if(iyy==fY .and. imm==fM .and. idd==fD .and. ihh/10000==fH) exit
   !       lmonth(imm)=.true.

   !     enddo 
   
   ! end function validateDates 


  
   !=============================================================================================
   integer function outRealSize()
       !# get the output byte size accordingly the machine
       !#
       !# @note
       !# ![](http://brams.cptec.inpe.br/wp-content/uploads/2015/11/logo-brams.navigation.png "")
       !#
       !# **Brief**: Return the output real size (1,4,8,etc) accordingly the machine
       !#
       !# **Documentation**: <http://brams.cptec.inpe.br/documentation/>
       !#
       !# **Author(s)**: Luiz Flavio Rodrigues **&#9993;**<luiz.rodrigues@inpe.br>
       !#
       !# **Date**: 28 July 2020 (Tuesday)
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
       character(len=*),parameter :: sourceName='generic.f90' !Name of source code
       character(len=*),parameter :: procedureName='**getOutputByteSize**' !Name of this procedure
       !
       !Local Parameters
   
       !Input/Output variables
   
       !Local variables
       integer :: output_byte_size
       !# output_byte_size
       real :: bytes_in_float
       !# bytes_in_float
   
       !Code
       inquire(iolength=output_byte_size) bytes_in_float
   
       outRealSize=output_byte_size
   
   end function outRealSize 

   !=============================================================================================
   subroutine sortz(qtLevels, levels, pLevel, order, outIndex)
       !# A simple sort that returns the 'order' indexes nearest of 'plevel'
       !#
       !# @note
       !# ![](http://brams.cptec.inpe.br/wp-content/uploads/2015/11/logo-brams.navigation.png "")
       !#
       !# **Brief**: simple sort that returns the 'order' indexes nearest of 'plevel' 
       !#
       !# **Documentation**: <http://brams.cptec.inpe.br/documentation/>
       !#
       !# **Author(s)**: Luiz Flavio Rodrigues **&#9993;**<luiz.rodrigues@inpe.br>
       !#
       !# **Date**: 01 September 2020 (Tuesday)
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
       character(len=*),parameter :: procedureName='**sortz**' !Name of this procedure
       !
       !Local Parameters
   
       !Input/Output variables
       integer, intent(in) :: qtLevels
       !#
       real, intent(in)  :: levels(qtlevels)
       !#
       real, intent(in) :: pLevel
       !#
       integer, intent(in) :: order
       !#
       integer, intent(out) :: outIndex(order)
   
       !Local variables
       integer :: i,j,minidx
       real                      :: currmin
       real, dimension(qtLevels) :: ztemp
   
       !Code
       ztemp = levels
       do j = 1, order
          currMin = abs(ztemp(1) - pLevel)
          minIdx  = 1
          do i = 2, qtLevels
             if( abs(ztemp(i) - pLevel) .le. currMin)then
                currMin = abs(ztemp(i) - pLevel)
                minIdx  = i
             end if
          end do            
          outIndex(j) = minIdx
          ztemp(minIdx) = -9E5
       end do   
   
   end subroutine sortz 

end module utilsMod 
