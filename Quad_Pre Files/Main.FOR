      program Quad_Pre

c     This is a pre-processor program that reads NGA-West2 ground motion files
c     and produces the required inputs for Quad4MU (
      
      implicit none
      include 'max_dims.H'  
      include 'declare.h'

c       read in run file 
        write (*,'( 2x,''Enter run file'')')
        read (*,'( a80)') runfile
        open (a,file=runfile,status='old')
        read (a,'( a80)') lsrockfile
        open (b,file=lsrockfile,status='old')
        read (a,*) nFiles      

c       write headers for output files
        write (c,*) 'pga ', 'npts ', 'dt ', 'max_fasrock ', 'prinput '
        
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
          max_fasrock = 0.
          do i=1,iNyq
            if ( abs( fasRock(i)) .gt. max_fasrock ) then
              max_fasrock = abs(fasRock(i))
              frinput = df*(i-1)
              prinput = 1./frinput              
            endif              
          enddo

c         print raw fas to file
          write (iFile,*) rockfile, pga
          write (iFile,*) 'freq(Hz) ', 'fasRock '
          do i=1, npts1
            write (iFile,'( f10.4,f10.4)') df*(i-1), fasRock(i)
          enddo
          close(iFile)

c         print all required inputs for Quad4MU to one file
          write (c,*) pga, npts, dt, max_fasrock, prinput
        enddo        
      end

c -------------------------------------------------------
 
      subroutine coeff (w, beta1, dt1)
      
      implicit none
      real beta1, dt1, w
      real a11, a12, a21, a22, b11, b12, b21, b22
      real beta, dt, t1, t2, t3, t4, s1, s2
      common /coef/a11,a12,a21,a22,b11,b12,b21,b22

      beta = beta1
      dt = dt1

c     Set up repeated terms
      t1 = sqrt(1.-beta**2)
      t2 = sin (w*t1*dt)
      t3 = cos (w*t1*dt)
      t4 = exp (-beta*w*dt)
      s1 = (2.*beta**2-1.) / (w**2*dt)
      s2 = 2.*beta / (w**3*dt)

c     calculate the as
      a11 = t4*(beta*t2/t1+t3)
      a12 = t4*t2 / (w*t1)
      a21 = -t4*w*t2 / t1
      a22 = t4*(t3-beta*t2/t1)
c
c     calculate the bs
      b11 = t4*((s1+beta/w)*t2 / (w*t1) + (s2+1./w**2)*t3) - s2
      b12 = -t4*(s1*t2/(w*t1)+s2*t3) - 1./w**2 + s2
      b21 = (s1+beta/w) * (t3-beta*t2/t1)
      b21 = t4*(b21 - (s2+1./w**2)*(w*t1*t2+beta*w*t3)) + 1./(w**2*dt)
      b22 = s1*(t3-beta*t2/t1)
      b22 = -t4*(b22 - s2*(w*t1*t2+beta*w*t3)) - 1./(w**2*dt)
      
      return
      end

c -------------------------------------------------------
       
      subroutine brs (x,w,beta,npts,rsp)
      
       implicit none
       real x(1), rsp(1), beta, w
       real d, v, a, z, ap1, dp1, vp1, t1, t2
       real a11, a12, a21, a22, b11, b12, b21, b22
       integer npts, i
       common /coef/ a11,a12,a21,a22,b11,b12,b21,b22

c      initialize
       t1 = 2.*beta*w
       t2 = w**2
       d = 0.
       v = 0.
       a = 0.
c
c      calculate the response
       do 10 i=1,npts
         ap1 = x(i)
         dp1 = a11*d + a12*v + b11*a + b12*ap1
         vp1 = a21*d + a22*v + b21*a + b22*ap1
         z = -(t1*vp1 + t2*dp1)  !spectral acceleration
 	!  z = w**2 * dp1         !pseudo-spectral acceleration
         rsp(i) = z
         a = ap1
         v = vp1
         d = dp1
  10  continue
 
      return
      end

c -------------------------------------------------------
       
      subroutine mode(TFSm, df, fTFmax, ffmax, flow, fhigh, sTFmax, sfmax, slow, shigh)
      
       implicit none
       include 'max_dims.H'
       
       real TFSm(MAXPTS), df, fTFmax, ffmax, flow, fhigh, sTFmax, sfmax, 
     1      slow, shigh, dTFmax, dfmax, dTF_half, dlow, dhigh, ndlow, ndhigh,
     2      TFmax1, TFmax2, fmax1, fmax2, ndTFmax, ndfmax, ndTF_half
       integer i, j, k, m, n, p, q, Hz30
       
c      limit search to maximum frequency of 30 Hz
       Hz30 = nint(30./df + 1)
       
c      find dominant frequency 
        dTFmax = 0.
        do i=2,Hz30
          if (TFSm(i) .gt. dTFmax ) then
            dTFmax = TFSm(i)
            dfmax = df*(i-1)
            m = i
          endif
        enddo

c       find frequency halfway down peak, lower 
        dTF_half = dTFmax / 2.
        do i=m,2,-1
          if ( TFSm(i) .lt. dTF_half) then
            dlow = df*(i-1)
            j = i
            goto 10
          endif
        enddo   

c       find frequency halfway down peak, upper  
   10   do i=m,Hz30
          if ( TFSm(i) .lt. dTF_half) then
            dhigh = df*(i-1)
            k = i
            goto 20
          endif
        enddo   
   20   continue            
       
c      find the next highest frequency, first try higher 
        TFmax1 = 0.
        do i=k,Hz30
          if (TFSm(i) .gt. TFmax1 ) then
            TFmax1 = TFSm(i)
            fmax1 = df*(i-1)
            n = i
          endif
        enddo
        if (fmax1 .eq. dhigh) then
          fmax1 = -999
          TFmax1 = -999
        endif

c      next try lower
        TFmax2 = 0.
        do i=2,j
          if (TFSm(i) .gt. TFmax2 ) then
            TFmax2 = TFSm(i)
            fmax2 = df*(i-1)
            p = i
          endif
        enddo
        if (fmax2 .eq. dlow) then
          fmax2 = -999
          TFmax2 = -999
        endif

c       larger of two test values is next dominant mode
        if (TFmax1 .gt. TFmax2) then
          ndTFmax = TFmax1
          ndfmax = fmax1
          q = n
        else if (TFmax2 .gt. TFmax1) then
          ndTFmax =  TFmax2
          ndfmax = fmax2
          q = p
        else if (TFmax1 .eq. TFmax2) then
          ndTFmax = 999
          ndfmax = 999
        endif

c       find frequency halfway down peak, lower 
        ndTF_half = ndTFmax / 2.
        do i=q,2,-1
          if ( TFSm(i) .lt. ndTF_half) then
            ndlow = df*(i-1)
            goto 30
          endif
        enddo   

c       find frequency halfway down peak, upper  
   30   do i=q,Hz30
          if ( TFSm(i) .lt. ndTF_half) then
            ndhigh = df*(i-1)
            goto 40
          endif
        enddo   
   40   continue  

c       lower frequency is first mode, higher frequency is second
        if (dfmax .lt. ndfmax) then
          fTFmax = dTFmax
          ffmax = dfmax
          flow = dlow
          fhigh = dhigh
          sTFmax = ndTFmax
          sfmax = ndfmax
          slow = ndlow
          shigh = ndhigh
        else if (ndfmax .lt. dfmax) then
          fTFmax = ndTFmax
          ffmax = ndfmax
          flow = ndlow
          fhigh = ndhigh
          sTFmax = dTFmax
          sfmax = dfmax
          slow = dlow
          shigh = dhigh
        endif

      return
      end
