      program Quad_Pre

c     This is a pre-processor program that reads NGA-West2 ground motion files
c     and produces the required inputs for Quad4MU all in one output file 
c     (i.e., npts, dt, prinput)
      
      implicit none
      include 'max_dims.h'  
      include 'declare.h'

c       read in run file 
        runfile = "run_fortran.txt"
        open (a,file=runfile,status='old')
        read (a,'( a80)') lsrockfile
        open (b,file=lsrockfile,status='old')
        read (a,*) nFiles  
        read (a,*) printflag    

c       write headers for output files
        open (c,file="prinput.txt",status='unknown')
        write (c,*) '      pga', '   npts', '          dt', '       m_fas', '     prinput'
        
c       loop over number of files (rock time histories)    
        do iFile=1,nFiles

c         read in rock time history (NGA-West2)
          read (b,'( a80)') rockfile
          write (*,'( a80)') rockfile
          open (d,file=rockfile,status='old')
          do j=1,3
            read (d,'( a1)') dummy
          enddo
          read (d,'( 5x,i7,8x,f5.4)') npts, dt                     
          if ( npts .gt. 30000) npts=30000
          read (d,*) (rockTH(i),i=1,npts)
          close (d)

c         find PGA
          pga = 0.
          do i=1,npts
            if ( abs( rockTH(i)) .gt. pga ) pga = abs(rockTH(i))
          enddo
        
          npts1 = i-1     
 
c         compute FFT
          call calcFFT (rockTH, nPts, dt, df, mRock, fasRock, cuRock, npts1)

c         find period corresponding to max Sa from rock TH
          Nyquist = 0.5*(1./dt)
          iNyq = nint(Nyquist/df+1)
          m_fas = 0.
          do i=1,iNyq
            if ( abs( fasRock(i)) .gt. m_fas ) then
              m_fas = abs(fasRock(i))
              frinput = df*(i-1)
              prinput = 1./frinput              
            endif              
          enddo

          if (printflag .eq. 1) then
c           print raw fas to file
            write (9+iFile,*) rockfile, pga
            write (9+iFile,*) 'freq(Hz) ', 'fasRock '
            do i=1, npts1
              write (9+iFile,'( f10.4,f10.4)') df*(i-1), fasRock(i)
            enddo
            close(9+iFile)
          endif

c         print all required inputs for Quad4MU to one file
          write (c,'(f10.5,2x,i5,2x,f10.5,2x,f10.5,2x,f10.5)') pga, npts, dt, m_fas, prinput
        enddo        
      end
