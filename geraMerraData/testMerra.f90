

module modMerra

  type st
    character(len=64) :: siteName
    real :: lon
    real :: lat
    integer :: iPos
    integer :: jPos
    logical :: found
  end type st
  type(st), allocatable :: sites(:)
  integer :: nSites

  contains

  subroutine readSites()
  
    integer :: i,sn 
    open(unit=33,file='sites.txt',status='old',action='read')
    read(33,*) nSites
    write(*,fmt='(A,I3)') 'Total de sites: ',nSites
    allocate(sites(nSites))
    do i=1,nSites
      read(33,*) sn,sites(i)%siteName,sites(i)%lat,sites(i)%lon
      write(*,fmt='(I3.3,1X,A32,1X,2(F8.2,1X))') sn,trim(sites(i)%siteName) &
             ,sites(i)%lat,sites(i)%lon
    enddo
    close(unit=33)

  end subroutine readSites

  subroutine leMerra(nnx,nny,nnt,file,values,llat,llon,dlat,dlon)
  	use netcdf
  
      implicit none
      include "constants.f90"
      integer, intent(in) :: nnx,nny,nnt
      character(len=*), intent(in) :: file
      real,intent(out) :: values(nnx,nny,nnt)
      real,intent(out) :: llat(nny)
      real,intent(out) :: llon(nnx)
      real,intent(out) :: dlat
      real,intent(out) :: dlon
  
      character(len=*), parameter :: varC='TOTEXTTAU'
      
  	  integer :: ncid,nvars
      integer :: i,ndims,nx,ny,nt,an,xtype,ilen,varPos
      integer :: latVarN,lonVarn,timVarN
      character(len=32) :: name,atName
      integer, allocatable :: nat(:),varDim(:),lenDim(:)
      character(len=32), allocatable :: varName(:)
      character(len=256) :: long_name
      real :: fmissing_value
      real :: scale_factor 
      real :: add_offset    
      !real, allocatable :: values(:,:,:)
  
      integer, allocatable :: time(:)
      integer :: recordLen,irec,it,imn
      character(len=15) :: date
print *,'Size of llat/llon: ',size(llat),size(llon)  
      !Criando a data para escrita do grads
      read(file(32:33),*) imn
      date='00:00z'//file(34:35)//month_name(imn)//file(28:31)
  
  	!Open NetCDF file
      print *,'fileName=',file//'.nc4'
      iErrNumber = nf90_open(path =file//'.nc4', mode = nf90_nowrite, ncid = ncid)
      if (iErrNumber /= nf90_noerr) then
        print *,'erro1',iErrNumber
        print *, nf90_strerror(-51)
      endif
          print *, nf90_strerror(-51)
      ! get info about netCDF file 
      iErrNumber=nf90_inquire(ncid, ndims, nvars)  ! get info about netCDF file 
  
      allocate(lenDim(nvars))
      allocate(varName(nVars))
      allocate(nat(nVars))
      allocate(varDim(nVars))
      
      !Get merra dimensions
      do i=1,nvars
          iErrNumber = nf90_Inquire_Dimension(ncid, i, name, lenDim(i))
          print *,'Variable ',i,'name ',trim(name),lenDim(i)
          if(trim(name)=='lon') then
              nx=lenDim(i)
              lonVarn=i
          elseif(trim(name)=='lat') then
              ny=lenDim(i)
              latVarn=i
          elseif(trim(name)=='time') then
              nt=lenDim(i)
              timVarn=i
          endif
      enddo
  
      if(nnx/=nx .or. nny/=ny .or. nnt/=nt) then
          print *,'V','G','L'
          print *,'X: ',nnx,nx
          print *,'Y: ',nny,ny
          print *,'T:',nnt,nt
          print *,'Erro em alguma dimensao. Veja acima (X,Y ou T).Saindo'
          return
      endif
  
      allocate(time(nt))
  
      !get lon and lat info
      iErrNumber = nf90_get_var(ncid, latVarN, llat)
      iErrNumber = nf90_get_var(ncid, lonVarN, llon)
      dlat=llat(2)-llat(1)
      dlon=llon(2)-llon(1)
  
      !Get time
      iErrNumber = nf90_get_var(ncid, timVarN, time)
      !print *,time
  
      do i=1,nVars
          iErrNumber=nf90_Inquire_Variable(ncid, i, name=varName(i) &
                 ,ndims=varDim(i), nAtts=nat(i))
          !print *,i,varName(i),nat(i)
      enddo
  
      !Pegando atributos para ajustar valores
      do i=1,nvars
          do an=1,nat(i)
              iErrNumber=nf90_inq_attname(ncid, i, an, atName)
              iErrNumber=nf90_Inquire_Attribute(ncid, i, trim(atName), xtype=xtype, len=iLen)
              if(trim(varName(i))==varC) then
                  varPos=i
                  !print *,an,atName
                  if(atName=='long_name')  iErrNumber = nf90_get_att(ncid, i, atName, long_name)
                  !if(atName=='units')                 
                  !if(atName=='_FillValue')                 
                  !if(atName=='missing_value')                 
                  if(atName=='fmissing_value') iErrNumber = nf90_get_att(ncid, i, atName,fmissing_value)              
                  if(atName=='scale_factor') iErrNumber = nf90_get_att(ncid, i, atName  , scale_factor)               
                  if(atName=='add_offset') iErrNumber = nf90_get_att(ncid, i, atName    , add_offset)               
                  !if(atName=='standard_name')                
                  !if(atName=='vmax')                 
                  !if(atName=='vmin')         
                  !if(atName=='valid_range')  
             endif
          enddo
      enddo
      
      !Pegando os valores de aot
      iErrNumber = nf90_get_var(ncid, varPos, values)
      
      !Ajustando os valores
      values=values*scale_factor+add_offset
  
      !Escrevendo grads para veriicacao
       recordLen=4*nx*ny
       open(unit=33,file=file//'.gra',&
           action='WRITE',status='REPLACE',form='UNFORMATTED',access='DIRECT', &
           recl=recordLen)
      
       !# writing grads binary and fill variables
       irec=1
       do it=1,24
          write (33,rec=irec) values(:,:,it)
          irec=irec+1
       enddo
      
       close(33) 
     
       open(unit=33,file=file//'.ctl' &
            ,action='WRITE',status='replace',form='FORMATTED')
     
       !writing the name of grads file
       write(33,*) 'dset ^'//file//'.gra'
       !writing others infos to ctl
       write(33,*) 'undef -0.9990000E+34'
       write(33,*) 'title MERRA AOT'
       write(33,*) 'xdef ',nx,' linear ',llon(1),dlon
       write(33,*) 'ydef ',ny,' linear ',llat(1),dlat
       write(33,*) 'zdef ',1,'levels ',1000
       write(33,*) 'tdef 24 linear ',date,' 1hr'
       write(33,*) 'vars 1'
       write(33,*) 'AOT ',1,'99 AOT'
       write(33,*) 'endvars'
       close(33)
  
  end subroutine leMerra

  subroutine getSitePos(nnx,nny,llat,llon)
      implicit none
      
      integer, intent(in) :: nnx,nny
      real, intent(in) :: llon(nnx)
      real, intent(in) :: llat(nny)

      integer :: i,j,s 

      do i=1, nnx-1
        do j=1,nny-1
          do s=1,nSites
            if(sites(s)%lat>llat(j) .and. sites(s)%lat<=llat(j+1)) then
              if(sites(s)%lon>llon(i) .and. sites(s)%lon<=llon(i+1)) then
                sites(s)%iPos=i+1 
                sites(s)%jpos=j+1 
                sites(s)%found=.true.
              endif
            endif
          enddo
        enddo
      enddo

        write(*,*) ''
        write(*,*) 'Posicao do ponto ip,jp no Merra para posicao do ponto da estacao:'
        write(*,fmt='(A2,1X,A32,1X,2(A3,1X),5(A8,1X))') 'Es','Nome da Estacao' &
             ,'ip','jp','site lon','Merr lon' &
             ,'site lat','Merr lat','Found'
      do s=1,nSites
        write(*,fmt='(I2.2,1X,A32,1X,2(I3.3,1X),4(F8.2,1X),L1)') s, sites(s)%siteName &
             ,sites(s)%iPos,sites(s)%jpos,sites(s)%lon,llon(sites(s)%iPos) &
             ,sites(s)%lat,llat(sites(s)%jpos),sites(s)%found
      enddo
  end subroutine getSitePos
 
end module modMerra

program testMerra
  use modMerra
    !  
  integer, parameter :: nnx=576
  integer, parameter :: nny=361
  integer, parameter :: nnt=24
  real :: values(nnx,nny,nnt)  
  real :: llat(nny)
  real :: llon(nnx)   
  real :: dlat,dlon                                    
  character(len=*) ,parameter :: file='MERRA2_400.tavg1_2d_aer_Nx.20190913'

  call readSites()
  call leMerra(nnx,nny,nnt,file,values,llat,llon,dlat,dlon)
  call getSitePos(nnx,nny,llat,llon)

end program testMerra