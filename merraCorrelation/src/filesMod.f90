!=============================================================================================
module filesMod
   !# Modulo para manipular arquivos
   !#
   !# @note
   !# ![](http://brams.cptec.inpe.br/wp-content/uploads/2015/11/logo-brams.navigation.png "")
   !#
   !# **Brief**: modulo para manipular arquivos
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
   character(len=*),parameter :: sourceName='filesMod.f90' !Name of source code
   character(len=*),parameter :: procedureName='**filesMod**' !Name of this procedure
   !
   !Local Parameters
   !Local variables
   
   contains
      
   !=============================================================================================
   subroutine readCtlAndSetSizes(ctlFileName,xdef,lonI,dLon,ydef,latI,dLat)
      !# Le arquivo ctl passado e ajusta as variaveis globais
      !#
      !# @note
      !# ![](http://brams.cptec.inpe.br/wp-content/uploads/2015/11/logo-brams.navigation.png "")
      !#
      !# **Brief**: le arquivo ctl passado e ajusta variavies globais
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
       fileExist
      
      implicit none
      
      include "constants.f90"
      character(len=*),parameter :: sourceName='filesMod.f90' !Name of source code
      character(len=*),parameter :: procedureName='**readCtlAndSetSizes**' !Name of this procedure
      !
      !Local Parameters
      
      !Input/Output variables
      character(len=*),intent(in) :: ctlFileName     
      !  
      integer, intent(out) :: xdef
      integer, intent(out) :: ydef
      real,    intent(out) :: latI
      real,    intent(out) :: lonI
      real,    intent(out) :: dLat
      real,    intent(out) :: dLon
      
      !Local variables
      integer :: funit=33
      character(len=256) :: lixo
      
      !Code
      if(.not. fileExist(ctlFileName)) & 
            iErrNumber=dumpMessage(c_tty,c_yes,sourceName,procedureName &
            ,c_fatal,'File '//ctlFileName &
            //' not found. Please, verify and solve it!')
      
      open(unit=funit,file=ctlFileName,action='read',status='old')
      read(funit,*) lixo
      read(funit,*) lixo
      read(funit,*) lixo
      read(funit,*) lixo,xdef,lixo,lonI,dLon
      read(funit,*) lixo,ydef,lixo,latI,dLat
      close(unit=funit)

    
   end subroutine readCtlAndSetSizes 

   !=============================================================================================
   subroutine readGra(graFileName,nx,ny,nTimes,dayCount,var)
      !# Le o arquivo grads da fornecida e preenche var
      !#
      !# @note
      !# ![](http://brams.cptec.inpe.br/wp-content/uploads/2015/11/logo-brams.navigation.png "")
      !#
      !# **Brief**: Le o arquivo Gras fornecido e preenche var
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
         fileExist

      implicit none

      include "constants.f90"
      character(len=*),parameter :: sourceName='filesMod.f90' !Name of source code
      character(len=*),parameter :: procedureName='**readGra**' !Name of this procedure
      !
      !Local Parameters
      integer,parameter :: funit=33

      !Input/Output variables
      character(len=*),intent(in) :: graFileName
      integer,intent(in) :: nx
      !# nx
      integer,intent(in) :: ny
      !# ny
      integer,intent(in) :: nTimes
      !# nTimes  
      integer,intent(in) :: dayCount
      !# Dia
      real,intent(out) :: var(nx,ny,nTimes)
      !# variavel

      !Local variables
      integer :: recordLen, irec, t, pos

      !Code
      if(.not. fileExist(graFileName)) & 
            iErrNumber=dumpMessage(c_tty,c_yes,sourceName,procedureName &
            ,c_fatal,'File '//graFileName &
            //' not found. Please, verify and solve it!')

      recordLen=4*nx*ny
      open(unit=funit,file=graFileName,&
         action='read',status='old',form='UNFORMATTED',access='DIRECT', &
         recl=recordLen)

      pos=dayCount*24
      irec=1

      iErrNumber=dumpMessage(c_tty,c_yes,'','',c_notice,'Lendo hora:')
      do t=1,24
        pos=pos+1
        write(*, fmt='(I4.4,1X)', advance="no") pos
        read (funit,rec=irec) var(:,:,pos)
        irec=irec+1
      enddo
      write(*,*) ''

      close(unit=funit)
   
   end subroutine readGra 

   !=============================================================================================
   subroutine readSites(siteFile)
      !# Le os sites aeronet
      !#
      !# @note
      !# ![](http://brams.cptec.inpe.br/wp-content/uploads/2015/11/logo-brams.navigation.png "")
      !#
      !# **Brief**: Le os sites aeronet
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
        fileExist

      use memoryMod, only: &
         site, &
         nSites, &
         lonINi, &
         latIni, &
         dLat, &
         dLon

      implicit none
         include "constants.f90"
      character(len=*),parameter :: sourceName='filesMod.f90' !Name of source code
      character(len=*),parameter :: procedureName='**readSites**' !Name of this procedure
      !
      !Local Parameters
      
      !Input/Output variables
      character(len=*), intent(in) :: siteFile
      !# Arquivo com os dados
      
      !Local variables
      integer :: funit=33
      character(len=256) :: lixo
      integer :: ns

      !Code
      if(.not. fileExist(siteFile)) & 
           iErrNumber=dumpMessage(c_tty,c_yes,sourceName,procedureName &
           ,c_fatal,'File '//siteFile &
           //' not found. Please, verify and solve it!')
      open(unit=funit,file=siteFile,action='read',status='old')
      read(funit,*) lixo
      
      nSites=0
      do while(.true.)
         read(funit,*,IOSTAT=iErrNumber) lixo
         if(iErrNumber/=0) exit 
         nSites=nSites+1
      enddo
      rewind(funit)
      read(funit,*) lixo
      allocate(site(nSites))
      write(*,fmt='(A,F8.2,1X,F8.2)') "                    SITES AERONET "
      write(*,fmt='(A)') "+--+-------------------------+--------+--------+--+---+---+"
      write(*,fmt='("|",A2,"|",A25,"|",A8,"|",A8,"|",A2,"|",A3,"|",A3,"|")') '#S','Nome' &
                ,'  lon','  lat','Cs','px','py'
      write(*,fmt='(A)') "+--+-------------------------+--------+--------+--+---+---+"
      do ns=1,nSites
         read(funit,*) site(ns)%name,site(ns)%lon,site(ns)%lat,site(ns)%sCase
         site(ns)%px=nint((site(ns)%lon - lonIni)/dLon)+1
         site(ns)%py=nint((site(ns)%lat - latIni)/dLat)+1
         write(*,fmt='("|",I2.2,"|",A25,"|",F8.2,"|",F8.2,"|",I2.2,"|",I3.3,"|",I3.3,"|")') ns,site(ns)%name &
                ,site(ns)%lon,site(ns)%lat,site(ns)%sCase,site(ns)%px,site(ns)%py
         write(*,fmt='(A)') "+--+-------------------------+--------+--------+--+---+---+"
      enddo
      close(unit=funit)
   
   end subroutine readSites 
   


end module filesMod 