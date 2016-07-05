      program DamResp
      
      implicit none
      include 'max_dims.H'
 
      integer a, b, c, d, e, f, g, h
      parameter (a=10, b=11, c=12, d=13, e=14, f=37, g=38, h=39)
      integer WinType, Win_len, Win_len0, Win_len1, Win_len2, Win_len3
      integer Win_len4, xflag1, xflag2, xflag3, xflag4   
      integer nFiles, iFile, i, j, k, l, npts, npts1, npts2, loopmax
      real rockTH(MAXPTS), dam1(MAXPTS), dam2(MAXPTS), dam3(MAXPTS), dam4(MAXPTS)
      real fasRock(MAXPTS), fasDam1(MAXPTS), fasDam2(MAXPTS), fasDam3(MAXPTS), fasDam4(MAXPTS)
      real fasRockSm(MAXPTS), lnfasRockSm(MAXPTS)
      real fasDam1Sm(MAXPTS), lnfasDam1Sm(MAXPTS)
      real fasDam2Sm(MAXPTS), lnfasDam2Sm(MAXPTS)
      real fasDam3Sm(MAXPTS), lnfasDam3Sm(MAXPTS)
      real fasDam4Sm(MAXPTS), lnfasDam4Sm(MAXPTS)     
      real TF1Sm(MAXPTS), TF2Sm(MAXPTS), TF3Sm(MAXPTS), TF4Sm(MAXPTS)
      real TF1(MAXPTS), TF2(MAXPTS), TF3(MAXPTS), TF4(MAXPTS), dt, time, df, pga
      real fTFmax1, fTFmax2, fTFmax3, fTFmax4
      real sTFmax1, sTFmax2, sTFmax3, sTFmax4
      real sfmax1, sfmax2, sfmax3, sfmax4
      real ffmax1, ffmax2, ffmax3, ffmax4
      real flow1, flow2, flow3, flow4
      real fhigh1, fhigh2, fhigh3, fhigh4
      real slow1, slow2, slow3, slow4
      real shigh1, shigh2, shigh3, shigh4
      real sigmax, pi, omega, alpha
      real varN1(MAXPTS), varN2(MAXPTS), varN3(MAXPTS), varN4(MAXPTS)
      real sigmaN1(MAXPTS), sigmaN2(MAXPTS), sigmaN3(MAXPTS), sigmaN4(MAXPTS)
      real accel(MAXPTS), damping, response(MAXPTS), TF1SDOF(MAXPTS)
      parameter (pi=3.14159)
      character*80 runfile, lsrockfile, lsdamfile, rockfile, damfile, dummy   

c       read in run file 
        write (*,'( 2x,''Enter run file'')')
        read (*,'( a80)') runfile
        open (a,file=runfile,status='old')
        read (a,'( a80)') lsrockfile
        read (a,'( a80)') lsdamfile
        open (b,file=lsrockfile,status='old')
        open (c,file=lsdamfile,status='old')
        read (a,*) nFiles   
        read (a,*) WinType
        read (a,*) Win_len0  
        read (a,*) sigmax
        read (a,*) loopmax       

c       write headers for first and second mode info files
        write (f,*) 'pga ', 'fmfMax1 ', 'fmfMax2 ', 'fmfMax3 ', 'fmfMax4 '
        write (g,*) 'pga ', 'smfMax1 ', 'smfMax2 ', 'smfMax3 ', 'smfMax4 '
        write (h,*) 'pga ', 'Win_len1 ', 'Win_len2 ', 'Win_len3 ', 'Win_len4 '
        
c       loop over number of files (rock time histories)    
        do iFile=1,nFiles

c         read in rock time history
          read (b,'( a80)') rockfile
          write (*,'( a80)') rockfile
          open (d,file=rockfile,status='old')
          do j=1,6
            read (d,'( a1)') dummy
          enddo
          read (d,'( 5x,i6,5x,f7.3)') npts, dt                     
          if ( npts .gt. 30000) npts=30000
          read (d,*) (rockTH(i),i=1,npts)
          close (d)

c         find PGA
          pga = 0.
          do i=1,npts
            if ( abs( rockTH(i)) .gt. pga ) pga = abs(rockTH(i))
          enddo

c         read in dam time history
          read (c,'( a80)') damfile
          write (*,'( i5, a80)') iFIle, damfile
          open (e,file=damfile,status='old')
          do j=1,3
            read (e,'( a1)') dummy
          enddo
          do i=1,npts
            read (e,*,end=100) time, dam1(i), dam2(i), dam3(i), dam4(i)
          enddo
 100      close (e)
        
          npts1 = i-1     
 
c         compute FFT
          call calcFFT (rockTH, nPts, dt, df, fasRock, npts1)
          call calcFFT (dam1, nPts, dt, df, fasDam1, npts1)
          call calcFFT (dam2, nPts, dt, df, fasDam2, npts1)
          call calcFFT (dam3, nPts, dt, df, fasDam3, npts1)
          call calcFFT (dam4, nPts, dt, df, fasDam4, npts1)

c         compute raw transfer function
          do i=1,npts1/2
            TF1(i) = fasDam1(i) / fasRock(i)
            TF2(i) = fasDam2(i) / fasRock(i)
            TF3(i) = fasDam3(i) / fasRock(i)
            TF4(i) = fasDam4(i) / fasRock(i)
          enddo

c         smooth FFT, dam1
          Win_len = Win_len0
          do i=1, loopmax
            call smooth(WinType, Win_len, fasRock, npts1, fasRockSm, lnfasRockSm)
            call smooth(WinType, Win_len, fasDam1, npts1, fasDam1Sm, lnfasDam1Sm)
          
c           compute smoothed transfer function
            do j=1,npts1/2
              TF1Sm(j) = fasDam1Sm(j) / fasRockSm(j)
            enddo

c           compute variance to determine if more smoothing is necessary
            call variance(Win_len, df, TF1Sm, sigmax, varN1, sigmaN1, xflag1)
        
c           increase window length and smooth again
            if (xflag1 .eq. 1) then 
              Win_len = Win_len + 2
              if (i .eq. loopmax) then
                write (*,*) 'increase loops for smoothing '
                pause 
              endif
            else
              Win_len1 = Win_len
              goto 10              
            endif 
          enddo  
   10   continue   

c         smooth FFT, dam2
          Win_len = Win_len0
          do i=1, loopmax
            call smooth(WinType, Win_len, fasRock, npts1, fasRockSm, lnfasRockSm)
            call smooth(WinType, Win_len, fasDam2, npts1, fasDam2Sm, lnfasDam2Sm)
          
c           compute smoothed transfer function
            do j=1,npts1/2
              TF2Sm(j) = fasDam2Sm(j) / fasRockSm(j)
            enddo

c           compute variance to determine if more smoothing is necessary
            call variance(Win_len, df, TF2Sm, sigmax, varN2, sigmaN2, xflag2)

c           increase window length and smooth again
            if (xflag2 .eq. 1) then 
              Win_len = Win_len + 2 
              if (i .eq. loopmax) then
                write (*,*) 'increase loops for smoothing '
                pause 
              endif
            else
              Win_len2 = Win_len
              goto 20 
            endif 
          enddo  
   20   continue

c         smooth FFT, dam3
          Win_len = Win_len0
          do i=1, loopmax
            call smooth(WinType, Win_len, fasRock, npts1, fasRockSm, lnfasRockSm)
            call smooth(WinType, Win_len, fasDam3, npts1, fasDam3Sm, lnfasDam3Sm)
          
c           compute smoothed transfer function
            do j=1,npts1/2
              TF3Sm(j) = fasDam3Sm(j) / fasRockSm(j)
            enddo

c           compute variance to determine if more smoothing is necessary
            call variance(Win_len, df, TF3Sm, sigmax, varN3, sigmaN3, xflag3)

c           increase window length and smooth again
            if (xflag3 .eq. 1) then 
              Win_len = Win_len + 2 
              if (i .eq. loopmax) then
                write (*,*) 'increase loops for smoothing '
                pause 
              endif
            else
              Win_len3 = Win_len
              goto 30 
            endif 
          enddo  
   30   continue

c         smooth FFT, dam4
          Win_len = Win_len0
          do i=1, loopmax
            call smooth(WinType, Win_len, fasRock, npts1, fasRockSm, lnfasRockSm)
            call smooth(WinType, Win_len, fasDam4, npts1, fasDam4Sm, lnfasDam4Sm)
          
c           compute smoothed transfer function
            do j=1,npts1/2
              TF4Sm(j) = fasDam4Sm(j) / fasRockSm(j)  
            enddo

c           compute variance to determine if more smoothing is necessary
            call variance(Win_len, df, TF4Sm, sigmax, varN4, sigmaN4, xflag4)

c           increase window length and smooth again
            if (xflag4 .eq. 1) then 
              Win_len = Win_len + 2 
              if (i .eq. loopmax) then
                write (*,*) 'increase loops for smoothing '
                pause 
              endif
            else
              Win_len4 = Win_len
              goto 40 
            endif 
          enddo  
   40   continue 

c         find first and second mode
          call mode(TF1Sm, df, fTFmax1, ffmax1, flow1, fhigh1, sTFmax1, sfmax1, slow1, shigh1)
          call mode(TF2Sm, df, fTFmax2, ffmax2, flow2, fhigh2, sTFmax2, sfmax2, slow2, shigh2)
          call mode(TF3Sm, df, fTFmax3, ffmax3, flow3, fhigh3, sTFmax3, sfmax3, slow3, shigh3)
          call mode(TF4Sm, df, fTFmax4, ffmax4, flow4, fhigh4, sTFmax4, sfmax4, slow4, shigh4)

c         fit to SDOF oscillator
          do i=1,npts
            accel(i) = 0.
          enddo
          accel(100) = 1.
          omega = ffmax1 * 2.*pi
c         loop over damping from 0.06 to 0.12
          do k=1,7
            damping(k) = 0.05 +0.01*k
c           calculate SDOF response TH
            call coeff (omega, damping(k), dt)
            call brs (accel, omega, damping(k), npts, response) 
c           multiply response by factor alpha
c           loop over alpha from 1.1 to 1.5
            do l=1,5
              alpha(l) = 1. + 0.1*l
              response = response*alpha(l)
c             calculate SDOF FFT / TF
              call calcFFT (response, npts, dt, df, TF1SDOF, npts2)
              
              do 
              rms(k,l) = 
              
c             print SDOF transfer functions to file, freq domain
c              write (59+l,*) rockfile
c              write (59+l,*) 'damping ', damping
c              write (59+l,*) 'alpha ', alpha
c              write (59+l,*) 'freq(Hz) ', 'TF1SDOF '
c              do i=1, npts2/2
c                write (59+l,'( f10.4,4f10.4)') df*(i-1), TF1SDOF(i)
c              enddo
c              close(59+l) 
              
            enddo
          enddo
 
c         print raw transfer functions to file, freq domain
c          write (39+iFile,*) rockfile, pga
c          write (39+iFile,*) 'freq(Hz) ', 'TF1 ', 'TF2 ', 'TF3 ', 'TF4'
c          do i=1, npts1/2
c            write (39+iFile,'( f10.4,4f10.4)') df*(i-1), TF1(i), TF2(i), TF3(i), TF4(i)
c          enddo
c          close(39+iFile)

c         print smooth transfer functions to file, freq domain
          write (49+iFile,*) rockfile
          write (49+iFile,*) 'freq(Hz) ', 'TF1Sm ', 'TF2Sm ', 'TF3Sm ', 'TF4Sm'
          do i=1, npts1/2
            write (49+iFile,'( f10.4,4f10.4)') df*(i-1), TF1Sm(i), TF2Sm(i), TF3Sm(i), TF4Sm(i)
          enddo
          close(49+iFile) 

c         print all pga, TFmax, freqMax, freqlow, freqhigh to one file
          write (f,*) pga, ffmax1, ffmax2, ffmax3, ffmax4
          write (g,*) pga, sfmax1, sfmax2, sfmax3, sfmax4
          write (h,*) pga, Win_len1, Win_len2, Win_len3, Win_len4
        enddo        
      end

c ------------------------------------------------------------------

      subroutine ft ( u, npts, nmin, dt, df, tb, te, cu1, mpad)
      real u(1), dt, df
      integer tb,te, npts,nmin,mpad
      complex cu1(1)

c     REMOVE THE DC
      call rdc ( u, npts, 0 )

c     TAPER
      call cosTaper ( u, npts, tb, te )

c     PAD TO POWER OF 2
      call pad ( u, npts, nmin, mpad )

c     FILL COMPLEX ARRAY
      do 10 i=1,npts
        cu1(i) = cmplx(u(i),0.0)
  10  continue

c     CALCULATE FORWARD FFT
      call cool ( -1., mpad, cu1 )

      do i=1,npts
	    cu1(i) = cu1(i) * dt
	  enddo

      df = 1./(float(npts)*dt)
      return
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
     1      slow, shigh, dTFmax, dfmax, dTF_half, dflow, dfhigh,
     2      TFmax1, TFmax2, fmax1, fmax2, ndTFmax, ndfmax
       integer i, j, k, m, Hz30
       
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
            dflow = df*(i-1)
            j = i
            goto 10
          endif
        enddo   

c       find frequency halfway down peak, upper  
   10   do i=m,Hz30
          if ( TFSm(i) .lt. dTF_half) then
            dfhigh = df*(i-1)
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
          endif
        enddo
        if (fmax1 .eq. dfhigh) then
          fmax1 = -999
          TFmax1 = -999
        endif

c      next try lower
        TFmax2 = 0.
        do i=2,j
          if (TFSm(i) .gt. TFmax2 ) then
            TFmax2 = TFSm(i)
            fmax2 = df*(i-1)
          endif
        enddo
        if (fmax2 .eq. dflow) then
          fmax2 = -999
          TFmax2 = -999
        endif

c       larger of two test values is next dominant mode
        if (TFmax1 .gt. TFmax2) then
          ndTFmax = TFmax1
          ndfmax = fmax1
        else if (TFmax2 .gt. TFmax1) then
          ndTFmax =  TFmax2
          ndfmax = fmax2
        else if (TFmax1 .eq. TFmax2) then
          ndTFmax = 999
          ndfmax = 999
        endif

c       lower frequency is first mode, higher frequency is second
        if (dfmax .lt. ndfmax) then
          fTFmax = dTFmax
          ffmax = dfmax
          sTFmax = ndTFmax
          sfmax = ndfmax
        else if (ndfmax .lt. dfmax) then
          fTFmax = ndTFmax
          ffmax = ndfmax
          sTFmax = dTFmax
          sfmax = dfmax
        endif

      return
      end
