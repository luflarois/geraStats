! =====================================================================
! Módulo com a função geradora de lista de arquivos
! =====================================================================
module gfs_utils
    implicit none
contains

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

end module gfs_utils

! =====================================================================
! Programa de teste
! =====================================================================
program testa_gerador
    use gfs_utils, only: gerar_lista
    implicit none
    character(len=256), allocatable :: arquivos(:)
    integer :: i, n

    ! Gera os nomes para os anos 2020 e 2021
    arquivos = gerar_lista('/dados/gfs', 2015, 2025)

    n = size(arquivos)
    print *, 'Total de arquivos gerados: ', n

    ! Imprime os 5 primeiros para conferência
    do i = 1, min(n, 1000)
        print *, trim(arquivos(i))
    end do

end program testa_gerador
