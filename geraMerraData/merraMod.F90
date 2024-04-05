!=============================================================================================
module merra
    !# Processa os dados merra
    !#
    !# @note
    !# ![](http://brams.cptec.inpe.br/wp-content/uploads/2015/11/logo-brams.navigation.png "")
    !#
    !# **Brief**: module com rotinas para ler e processar os dados merra fazendo uma correlacao 
    !# temporal entre cada estacao AERONET e os dados
    !#
    !# Este modulo usa o API grib2 do NWS/NOAA. 
    !# Para instalar siga as instrucoes do Manual de instalacao do BRAMS, item 4.
    !# 
    !# Os dados do Merra estao em formato NetCDF. Para recorta-los e converte-los usar os 
    !# comandos (exemplo para a data de 20190913):
    !# ncdump -bf -ff -v time,lat,lon,TOTEXTTAU MERRA2_400.tavg1_2d_aer_Nx.20190913.nc4 >& 20190913.txt
    !#
    !# **Documentation**: <http://brams.cptec.inpe.br/documentation/>
    !#
    !# **Author(s)**: Luiz Flavio Rodrigues **&#9993;**<luiz.rodrigues@inpe.br>
    !#
    !# **Date**: 13 August 2020 (Thursday)
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

    include "constants.f90"
    character(len=*),parameter :: sourceName='merraMod.F90' !Name of source code
    character(len=*),parameter :: procedureName='**merra**' !Name of this procedure
    !
    !Module Parameters

    !Module variables
    logical :: inicializado
    !# Se 1 signfica que foram lidos lats e lons e alocado aot
    real, allocatable :: var(:,:,:)
    !# 
    integer :: nLons
    !#
    integer :: nLats
    !#
    real :: lon(2)
    !#
    real :: dlon
    !#
    real :: lat(2)
    !#
    real :: dlat
    !#
    integer :: ncLats
    !# Numero de latitudes recortadas
    integer :: ncLons
    !# Numero de longitudes recortadas 
    real :: clon(2)
    !# Longitudes iniciais e finais recortadas
    real :: cLat(2)
    !# latitudes inicias e finais recortadas
    real, allocatable :: cVar(:,:,:)


    contains

    !=============================================================================================
    subroutine leMerra(year,month,day)
         !# Le os dados merra fornecidos
         !#
         !# @note
         !# ![](http://brams.cptec.inpe.br/wp-content/uploads/2015/11/logo-brams.navigation.png "")
         !#
         !# **Brief**: Le usando grib2 api os dados merra fornecidos
         !#
         !# **Documentation**: <http://brams.cptec.inpe.br/documentation/>
         !#
         !# **Author(s)**: Luiz Flavio Rodrigues **&#9993;**<luiz.rodrigues@inpe.br>
         !#
         !# **Date**: 13 August 2020 (Thursday)
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
         use utilsMod, only: fileExist
        
         include "constants.f90"
         character(len=*),parameter :: procedureName='**leMerra**' !Name of this procedure
         !
         !Local Parameters
         character(len=*), parameter :: varName=':var255_4_0_98_255_1:'

         !Input/Output variables
         integer, intent(in) :: year
         integer, intent(in) :: month
         integer, intent(in) :: day
         !# Nome do arquivo a ser lido
    
         !Local variables
         character(len=8) :: verfDate
         character(len=12) :: fileInp
         character(len=256) :: line,lixo,longName
         character(len=64) :: MerraFile
         integer :: i
         character(len=5):: cLat
         integer :: itime,llat,llon
         character(len=2) chour
         character(len=256) :: comando

         !Code

         !Escrevendo o nome do arquivo
         write(fileInp,fmt='(I4.4,I2.2,I2.2,".txt")') year,month,day
         write(verfDate,fmt='(I4.4,I2.2,I2.2)') year,month,day

         comando="ncdump -bf -ff -v time,lat,lon,TOTEXTTAU MERRA2_400.tavg1_2d_aer_Nx."//verfDate//".nc4 >& "//verfDate//".txt"

         iErrNumber=dumpMessage(c_tty,c_yes,'','' &
              ,c_notice,'Lendo arquivo '//fileInp)


         if(.not. fileExist(fileInp)) then 
            iErrNumber=dumpMessage(c_tty,c_yes,'','' &
              ,c_warning,'Arquivo '//fileInp &
              //' nao encontrado. Para gerar o arquivo use o comando abaixo!')
            iErrNumber=dumpMessage(c_tty,c_yes,'','' &
              ,c_fatal,trim(comando))
         endif
         open(unit=33,file=fileInp,action='read',status='old')

         read(33,*) lixo,MerraFile
         !iErrNumber=dumpMessage(c_tty,c_yes,sourceName,procedureName &
         !     ,c_notice,'Cabecalho '//MerraFile)
         read(33,*) lixo
         read(33,*) lixo,lixo,nLons
         read(33,*) lixo,lixo,nLats

         !Aloca var somente uma vez
         if(.not. inicializado) then
            allocate(var(nLons,nLats,24))
            inicializado=.true.
         endif

         !iErrNumber=dumpMessage(c_tty,c_yes,'','' &
         !     ,c_notice,'Nlons, Nlats ',(/nlons,nlats/),"I4.4")
         
         !# Longitudes
         do while(.true.)
            read(33,*) line
            !print *,'#',trim(lixo),'# - #',trim(line),'#'
            if(line(1:5)=='data:') exit
         enddo
         read(33,*) lixo,lixo,lon(1)
         read(33,*) lon(2)
         dlon=lon(2)-lon(1)

         !iErrNumber=dumpMessage(c_tty,c_yes,'','' &
         !     ,c_notice,'Longitudes: ',(/lon(1),dlon/),"F8.2")
         
         !# Latitudes
         do while(.true.)
            read(33,*) line,lixo,cLat
            !print *,'#',trim(lixo),'# - #',trim(line),'#'
            if(line(1:3)=='lat') exit
         enddo
         read(cLat,fmt='(F6.0)') lat(1)
         read(33,*) lat(2)
         dlat=lat(2)-lat(1)

         !iErrNumber=dumpMessage(c_tty,c_yes,'','' &
         !     ,c_notice,'Latitudes: ',(/lat(1),dlat/),"F8.2")
         
         !# TOTEXTTAU
         do while(.true.)
            read(33,*) line
            !print *,'#',trim(lixo),'# - #',trim(line),'#'
            if(line(1:9)=='TOTEXTTAU') exit
         enddo
         iErrNumber=dumpMessage(c_tty,c_yes,'','' &
              ,c_notice,'Lendo hora:')
         do itime=1,24
            write(*, fmt='(I2.2,1X)', advance="no") iTime
            do llat=1,nlats
                do llon=1,nLons
                    if(llon==1) read(33,*) lixo
                    read(33,*) var(llon,llat,itime)
                enddo
            enddo
        enddo
        write(*,*) ''

        close(unit=33)


    end subroutine leMerra 

    !=============================================================================================
    subroutine corteMerra(latIni,latFin,lonINi,lonFin)
        !# Recorta os dados para uma regiao
        !#
        !# @note
        !# ![](http://brams.cptec.inpe.br/wp-content/uploads/2015/11/logo-brams.navigation.png "")
        !#
        !# **Brief**: recorta os dados para uma regiao
        !#
        !# **Documentation**: <http://brams.cptec.inpe.br/documentation/>
        !#
        !# **Author(s)**: Luiz Flavio Rodrigues **&#9993;**<luiz.rodrigues@inpe.br>
        !#
        !# **Date**: 03 September 2020 (Thursday)
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
    
        include "constants.f90"
        character(len=*),parameter :: procedureName='**corteMerra**' !Name of this procedure
        !
        !Local Parameters
    
        !Input/Output variables
        real,intent(in) :: latINi
        !# latitude inicial
        real,intent(in) :: latFin
        !# Latitude final
        real,intent(in) :: lonINi
        !# Longitude inicial
        real,intent(in) :: lonFin
        !# Longitude final
    
        !Local variables
        integer :: i,j
        integer :: pLonIni,pLonFin
        integer :: pLatIni,pLatFin
    
        !Code
        do i=1,nLons
            if(lon(1)+(i-1)*dlon >= lonINi) then
                pLonIni=i
                cLon(1)=lon(1)+(i-1)*dlon
                exit
            endif
        enddo
        do i=pLonIni,nLons
            if(lon(1)+(i-1)*dlon >= lonFin) then
                pLonFin=i
                cLon(2)=lon(1)+(i-1)*dlon
                exit
            endif
        enddo  
        !print *, pLonIni,cLon(1),pLonFin,cLon(2)  
        
        do i=1,nLats
            if(lat(1)+(i-1)*dlat >= latINi) then
                pLatIni=i
                cLat(1)=lat(1)+(i-1)*dlat
                exit
            endif
        enddo
        do i=pLatIni,nLats
            if(lat(1)+(i-1)*dlat >= latFin) then
                pLatFin=i
                cLat(2)=lat(1)+(i-1)*dlat
                exit
            endif
        enddo 
        !print *, pLatIni,cLat(1),pLatFin,cLat(2)  

        ncLats=pLatFin-pLatIni+1
        ncLons=pLonFin-pLonIni+1

        allocate(cVar(ncLats,ncLons,24))

        cVar(:,:,:)=var(pLonIni:pLonFin,pLatIni:pLatFin,:)
    
    end subroutine corteMerra

end module merra 

program teste
   use merra

   use utilsMod, only: &
        date_add_to_dble, &
        fileExist

   character(len=8) :: gradsFileName
   character(len=15) :: gradsDate
   integer :: recordLen,iRec,t
   type dt 
    integer :: year
    integer :: month
    integer :: day
   end type dt 
   type(dt) :: dataIni,dataFin
   integer :: dayCount
   integer :: iyy,imm,idd,ihh
   character(len=4) :: arg(6)
   character(len=8) :: verfDate
   integer :: istat
   character(len=256) :: comando

   inicializado=.false.
   

   do i = 1,6
        CALL getarg(i, arg(i))
   enddo
   
   read(arg(1),fmt='(I4.4)') dataIni%year
   read(arg(2),fmt='(I4.4)') dataIni%month
   read(arg(3),fmt='(I4.4)') dataIni%day
   read(arg(4),fmt='(I4.4)') dataFin%year
   read(arg(5),fmt='(I4.4)') dataFin%month
   read(arg(6),fmt='(I4.4)') dataFin%day


   dayCount=0
   do while(.true.)

        call date_add_to_dble(dataIni%year,dataIni%month,dataIni%day,00,dble(dayCount),'d' &
                       ,iyy,imm,idd,ihh)

        iErrNumber=dumpMessage(c_tty,c_yes,'','' &
              ,c_notice,'##### Trabalhando com a data: ',(/iyy,imm,idd/),"I4.4")

         write(verfDate,fmt='(I4.4,I2.2,I2.2)') iyy,imm,idd
         comando="ncdump -bf -ff -v time,lat,lon,TOTEXTTAU MERRA2_400.tavg1_2d_aer_Nx."//verfDate//".nc4 > "//verfDate//".txt"

         if(.not. fileExist('MERRA2_400.tavg1_2d_aer_Nx.'//verfDate//'.nc4')) & 
              iErrNumber=dumpMessage(c_tty,c_yes,'merraMod.f90','Main program' &
              ,c_fatal,'File '//'MERRA2_400.tavg1_2d_aer_Nx.'//verfDate//'.nc4' &
              //' not found. Please, verify and solve it!')

         iErrNumber=dumpMessage(c_tty,c_yes,'','' &
              ,c_notice,'Executando comando '//trim(comando))
         CALL system(trim(comando), status=istat)
         if(istat/=0) iErrNumber=dumpMessage(c_tty,c_yes,'','' &
              ,c_fatal,'Erro no comando. Erro numero ',istat,'I2.2')

        call leMerra(iyy,imm,idd)
        call corteMerra(-60.0,20.0,-100.0,0.0)

        write(gradsFileName,fmt='(I4.4,I2.2,I2.2)') iyy,imm,idd
        write(gradsDate,fmt='("00:00z",I2.2,A3,I4.4)') idd,month_name(imm),iyy

        iErrNumber=dumpMessage(c_tty,c_yes,'','' &
              ,c_notice,'Escrevendo data '//gradsDate//', para arquivo '//trim(gradsFileName)//'.gra')

        open(unit=33,file=trim(gradsFileName)//'.ctl' &
            ,action='WRITE',status='replace',form='FORMATTED')

         !writing the name of grads file
         write(33,*) 'dset ^'//trim(gradsFileName)//'.gra'
         !writing others infos to ctl
         write(33,*) 'undef -0.9990000E+34'
         write(33,*) 'title Merra AOT'
         write(33,*) 'xdef ',ncLons,' linear ',clon(1),dlon
         write(33,*) 'ydef ',ncLats,' linear ',clat(1),dlat
         write(33,*) 'zdef ',1,'levels',1000
         write(33,*) 'tdef 24 linear ',gradsDate,' 1hr'
         write(33,*) 'vars ',1
         write(33,*) 'tau',1,'99 ','TOTEXTTAU 1'
         write(33,*) 'endvars'

         close(unit=33)

         recordLen=4*ncLons*ncLats
         open(unit=33,file=trim(gradsFileName)//'.gra',&
           action='WRITE',status='REPLACE',form='UNFORMATTED',access='DIRECT', &
           recl=recordLen)
         irec=1
         do t=1,24
             write (33,rec=irec) cvar(:,:,t)
             irec=irec+1
         enddo

         if(iyy==dataFin%year .and. imm==dataFin%month .and. idd==dataFin%day) exit
         deallocate(cVar)

         dayCount=dayCount+1
         write(*,*) ''
         write(*,*) ''
         write(*,*) ''

    enddo


end program teste