!=============================================================================================
program correlacao
   !# programa para fazer a correlacao temporal com dados do Merra
   !#
   !# @note
   !# ![](http://brams.cptec.inpe.br/wp-content/uploads/2015/11/logo-brams.navigation.png "")
   !#
   !# **Brief**: programa para fazer a correlacao temporal com dados do Merra
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

   use utilsMod, only: &
      date_add_to_dble, &
      stepsBetweenDates, &
      isValid

   use filesMod, only: &
      readCtlAndSetSizes, &
      readGra, &
      readSites

   use memoryMod, only: &
      nx, &
      ny, &
      latIni, &
      lonIni, &
      dLat, &
      dLon, &
      var, &
      nTimes, &
      nDays, &
      correlation, &
      site, &
      nSites, &
      inicializado, &
      aerodata, &
      outdata
   
   implicit none
   include "constants.f90"
   character(len=*),parameter :: sourceName='correlacao.f90' !Name of source code
   character(len=*),parameter :: procedureName='**correlacao**' !Name of this procedure
   !
   !Local Parameters
   real, parameter :: undef=-9999.0

   !Local variables
   type dt 
    integer :: year
    integer :: month
    integer :: day
   end type dt 
   type(dt) :: dataIni,dataFin
   character(len=4) :: arg(6)
   integer :: i,dayCount,iyy,imm,idd,ihh
   integer :: j,s,it,ct,isp,jsp
   character(len=12) :: ctlFileName,graFileName
   real :: ex,ex2,ey,ey2,exy,r1,r2
   real :: maxinf,maxsite
   integer :: recn,ii,jj
   character(len=2) :: csit
   logical, allocatable :: valid(:,:)
   integer, allocatable :: xlim(:,:),ylim(:,:)

   !Code
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

   write(*,*) 'Data Inicial  : ',dataIni%year,dataIni%month,dataIni%day
   write(*,*) 'Data Final    : ',dataFin%year,dataFin%month,dataFin%day

   !Calcula quantos dias existe ente o inicio e o fim
   nDays=stepsBetweenDates(dataIni%year,dataIni%month,dataIni%day,0 &
                          ,dataFin%year,dataFin%month,dataFin%day,0,24,'h')

   nTimes=nDays*24
   write(*,*) 'Total de horas: ',nTimes
   dayCount=0

   do while(.true.)

      !Incrementa a data em dias e cria o nome dos arquivos
      call date_add_to_dble(dataIni%year,dataIni%month,dataIni%day,00,dble(dayCount),'d' &
                     ,iyy,imm,idd,ihh)
      write(ctlFileName,fmt='(I4.4,I2.2,I2.2,".ctl")') iyy,imm,idd
      write(graFileName,fmt='(I4.4,I2.2,I2.2,".gra")') iyy,imm,idd

      if(.not. inicializado) then !SE for a primeira vez aloca as variaveis
          call readCtlAndSetSizes(ctlFileName,nx,lonIni,dLon,ny,latIni,dLat)
          iErrNumber=dumpMessage(c_tty,c_yes,'','',c_notice,'Inventario da Grade:')
          write(*,fmt='("Nx=",I3.3,", LonINi=",F8.2,", Dlon=",F8.2)') nx,lonINi,dLon
          write(*,fmt='("Ny=",I3.3,", LatINi=",F8.2,", Dlat=",F8.2)') ny,latINi,dLat
          allocate(var(nx,ny,nTimes))
          allocate(correlation(nx,ny))
          allocate(outData(nx,ny))
          allocate(xlim(ny,2))
          allocate(ylim(nx,2))
          inicializado=.true.
          call readSites("aeronet_brams_cases_sites_name_coordinates.csv")
          allocate(aerodata(nx,ny,nSites))
          allocate(valid(nx,ny))
          valid=.false.
      endif

      !Le o arquivo grads para a data. A variavel eh preenchida por hora
      iErrNumber=dumpMessage(c_tty,c_yes,'','',c_notice,'Lendo arquivo '//graFileName)
      call readGra(graFileName,nx,ny,nTimes,dayCount,var)

      !Se acabou o tempo cai fora
      if(iyy==dataFin%year .and. imm==dataFin%month .and. idd==dataFin%day) exit
      dayCount=dayCount+1

   end do

   open(unit=33,file='corr.gra',access='direct',recl=nx*ny*4,status='replace')
   recn=1
   do s = 1, nsites
      correlation=undef
      do i=1,nx
         do j=1,ny 
            !correlation
            ct  = 0
            ex  = 0
            ex2 = 0
            ey  = 0
            ey2 = 0
            exy = 0
            do it=1,nTimes-1
               if(var(i,j,it)>undef .and. var(site(s)%px,site(s)%py,it)>undef)then 
                  ex  = var(i,j,it) + ex
                  ex2 = var(i,j,it)**2 + ex2
                  ey  = var(site(s)%px,site(s)%py,it) + ey
                  ey2 = var(site(s)%px,site(s)%py,it)**2 + ey2
                  exy = (var(i,j,it) * var(site(s)%px,site(s)%py,it)) + exy
                  ct  = 1 + ct
               end if
            end do
   
            if(ct .gt. 3)then
               ex  = ex/ct
               ex2 = ex2/ct
               ey  = ey/ct
               ey2 = ey2/ct
               exy = exy/ct            
               r1 = ( exy - (ex* ey) )
               r2 = ( sqrt(ex2 - (ex**2)) * sqrt(ey2 - (ey**2)) )
               if(r2 .ne. 0)correlation(i,j) = r1/r2
            end if
         end do      
      end do  
      write(33,rec=recn) correlation
      recn = recn + 1

      aerodata(:,:,s)=correlation
   end do

   open(unit=33,file='corr.ctl' &
            ,action='WRITE',status='replace',form='FORMATTED')

   !writing the name of grads file
   write(33,*) 'dset ^corr.gra'
   !writing others infos to ctl
   write(33,*) 'undef -0.9990000E+34'
   write(33,*) 'title correlacao'
   write(33,*) 'xdef ',nx,' linear ',lonIni,dlon
   write(33,*) 'ydef ',ny,' linear ',latIni,dlat
   write(33,*) 'zdef ',1,'levels',1000
   write(33,*) 'tdef 1 linear 00:00z01jan2019 1hr'
   write(33,*) 'vars ',nSites
   do s=1,nSites
      write(csit,fmt='(I2.2)') s
      write(33,*) "sit"//csit,1,'99 ',trim(site(s)%name)
   enddo
   write(33,*) 'endvars'

   close(unit=33)

   do i=1,nx
      do j=1,ny
         maxinf  = 0.
         maxsite = 0    
         do s=1,nsites        
            if(aerodata(i,j,s)> maxinf) then            
               maxinf = aerodata(i,j,s)
               maxsite = s
            end if         
         end do    
         if(maxinf>0.5) outdata(i,j)=int(maxsite)     
      end do
   end do

   open(unit=33,file='aotMap.gra',access='direct',recl=nx*ny*4,status='replace')
   recn=1
   write(33,rec=recn) outData
   close(unit=33)

   open(unit=33,file='aotMap.ctl' &
            ,action='WRITE',status='replace',form='FORMATTED')

   !writing the name of grads file
   write(33,*) 'dset ^aotMap.gra'
   !writing others infos to ctl
   write(33,*) 'undef -0.9990000E+34'
   write(33,*) 'title AotMap'
   write(33,*) 'xdef ',nx,' linear ',lonIni,dlon
   write(33,*) 'ydef ',ny,' linear ',latIni,dlat
   write(33,*) 'zdef ',1,'levels',1000
   write(33,*) 'tdef 1 linear 00:00z01jan2019 1hr'
   write(33,*) 'vars ',1
   write(33,*) "aotmap",1,'99 ',"aotmap"
   write(33,*) 'endvars'
   close(unit=33)


end program correlacao 