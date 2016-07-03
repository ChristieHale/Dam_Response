      program DamResp
      
      implicit none
      include 'max_dims.H'
 
      integer a, b, c, d, e, f, g
      parameter (a=10, b=11, c=12, d=13, e=14, f=37, g=38)
      integer WinType, Win_len, test    
      integer nFiles, iFile, j, npts, i, npts1, count1, count2
      real rockTH(MAXPTS), dam1(MAXPTS), dam2(MAXPTS), dam3(MAXPTS), dam4(MAXPTS)
      real fasRock(MAXPTS), fasDam1(MAXPTS), fasDam2(MAXPTS), fasDam3(MAXPTS), fasDam4(MAXPTS)
      real fasRockSm(MAXPTS), lnfasRockSm(MAXPTS)
      real fasDam1Sm(MAXPTS), lnfasDam1Sm(MAXPTS)
      real fasDam2Sm(MAXPTS), lnfasDam2Sm(MAXPTS)
      real fasDam3Sm(MAXPTS), lnfasDam3Sm(MAXPTS)
      real fasDam4Sm(MAXPTS), lnfasDam4Sm(MAXPTS)
      real fasRockSmx(MAXPTS), lnfasRockSmx(MAXPTS)
      real fasDam1Smx(MAXPTS), lnfasDam1Smx(MAXPTS)
      real fasDam2Smx(MAXPTS), lnfasDam2Smx(MAXPTS)
      real fasDam3Smx(MAXPTS), lnfasDam3Smx(MAXPTS)
      real fasDam4Smx(MAXPTS), lnfasDam4Smx(MAXPTS)      
      real TF1Sm(MAXPTS), TF2Sm(MAXPTS), TF3Sm(MAXPTS), TF4Sm(MAXPTS)
      real TF1Smx(MAXPTS), TF2Smx(MAXPTS), TF3Smx(MAXPTS), TF4Smx(MAXPTS)
      real TF1(MAXPTS), TF2(MAXPTS), TF3(MAXPTS), TF4(MAXPTS), dt, time, df, pga, pi
      real fTFmax1, fTFmax2, fTFmax3, fTFmax4, ffmax1, ffmax2, ffmax3, ffMax4
      real fTFmax1x, fTFmax2x, fTFmax3x, fTFmax4x, ffmax1x, ffmax2x, ffmax3x, ffMax4x
      real flow1, flow2, flow3, flow4
      real flow1x, flow2x, flow3x, flow4x
      real fhigh1, fhigh2, fhigh3, fhigh4
      real fhigh1x, fhigh2x, fhigh3x, fhigh4x
      real sTFmax1, sTFmax2, sTFmax3, sTFmax4
      real sTFmax1x, sTFmax2x, sTFmax3x, sTFmax4x
      real sfMax1, sfMax2, sfMax3, sfMax4
      real sfMax1x, sfMax2x, sfMax3x, sfMax4x
      real slow1, slow2, slow3, slow4
      real slow1x, slow2x, slow3x, slow4x
      real shigh1, shigh2, shigh3, shigh4
      real shigh1x, shigh2x, shigh3x, shigh4x
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
        read (a,*) Win_len         

c       write headers for first and second mode info files
        write (f,*) 'pga ', 'fmfMax1 ', 'fmfMax2 ', 'fmfMax3 ', 'fmfMax4 '
        write (g,*) 'pga ', 'smfMax1 ', 'smfMax2 ', 'smfMax3 ', 'smfMax4 '
        
c       loop over number of files (rock time histories)    
        count1 = 0
        count2 = 0
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

c         compute unsmooth transfer function
          do i=1,npts1/2
            TF1(i) = fasDam1(i) / fasRock(i)
            TF2(i) = fasDam2(i) / fasRock(i)
            TF3(i) = fasDam3(i) / fasRock(i)
            TF4(i) = fasDam4(i) / fasRock(i)
          enddo

c         smooth FFT, first pass
          call smooth(WinType, Win_len, fasRock, npts1, fasRockSm, lnfasRockSm)
          call smooth(WinType, Win_len, fasDam1, npts1, fasDam1Sm, lnfasDam1Sm)
          call smooth(WinType, Win_len, fasDam2, npts1, fasDam2Sm, lnfasDam2Sm)
          call smooth(WinType, Win_len, fasDam3, npts1, fasDam3Sm, lnfasDam3Sm)
          call smooth(WinType, Win_len, fasDam4, npts1, fasDam4Sm, lnfasDam4Sm)

c         smooth FFT, second pass
          call smooth(WinType, Win_len, fasRockSm, npts1, fasRockSmx, lnfasRockSmx)
          call smooth(WinType, Win_len, fasDam1Sm, npts1, fasDam1Smx, lnfasDam1Smx)
          call smooth(WinType, Win_len, fasDam2Sm, npts1, fasDam2Smx, lnfasDam2Smx)
          call smooth(WinType, Win_len, fasDam3Sm, npts1, fasDam3Smx, lnfasDam3Smx)
          call smooth(WinType, Win_len, fasDam4Sm, npts1, fasDam4Smx, lnfasDam4Smx)
          
c         compute smoothed transfer function, from first pass
          do i=1,npts1/2
            TF1Sm(i) = fasDam1Sm(i) / fasRockSm(i)
            TF2Sm(i) = fasDam2Sm(i) / fasRockSm(i)
            TF3Sm(i) = fasDam3Sm(i) / fasRockSm(i)
            TF4Sm(i) = fasDam4Sm(i) / fasRockSm(i)
          enddo

c         compute smoothed transfer function, from second pass
          do i=1,npts1/2
            TF1Smx(i) = fasDam1Smx(i) / fasRockSmx(i)
            TF2Smx(i) = fasDam2Smx(i) / fasRockSmx(i)
            TF3Smx(i) = fasDam3Smx(i) / fasRockSmx(i)
            TF4Smx(i) = fasDam4Smx(i) / fasRockSmx(i)
          enddo   

c         find first and second mode, from first pass
          call mode(TF1Sm, df, fTFmax1, ffmax1, flow1, fhigh1, sTFmax1, sfmax1, slow1, shigh1)
          call mode(TF2Sm, df, fTFmax2, ffmax2, flow2, fhigh2, sTFmax2, sfmax2, slow2, shigh2)
          call mode(TF3Sm, df, fTFmax3, ffmax3, flow3, fhigh3, sTFmax3, sfmax3, slow3, shigh3)
          call mode(TF4Sm, df, fTFmax4, ffmax4, flow4, fhigh4, sTFmax4, sfmax4, slow4, shigh4)

c         find first and second mode, from second pass
          call mode(TF1Smx, df, fTFmax1x, ffmax1x, flow1x, fhigh1x, sTFmax1x, sfmax1x, slow1x, shigh1x)
          call mode(TF2Smx, df, fTFmax2x, ffmax2x, flow2x, fhigh2x, sTFmax2x, sfmax2x, slow2x, shigh2x)
          call mode(TF3Smx, df, fTFmax3x, ffmax3x, flow3x, fhigh3x, sTFmax3x, sfmax3x, slow3x, shigh3x)
          call mode(TF4Smx, df, fTFmax4x, ffmax4x, flow4x, fhigh4x, sTFmax4x, sfmax4x, slow4x, shigh4x)

c         throw out this record if no second mode found (temporary), first pass
          if (sfmax1 .eq. 999 .or. sfmax2 .eq. 999 .or. sfmax3 .eq. 999 .or. sfmax4 .eq. 999) then
            count1 = count1 + 1
            test = 1
          endif
          
c         throw out this record if no second mode found (temporary), second pass
          if (sfmax1x .eq. 999 .or. sfmax2x .eq. 999 .or. sfmax3x .eq. 999 .or. sfmax4x .eq. 999) then
            count2 = count2 + 1
            test = 1
          endif
          
          if (test .eq. 1) then
            goto 10
          endif
 
c         print smooth transfer functions to file, freq domain
          write (39+iFile,*) rockfile
          write (39+iFile,*) 'freq(Hz) ', 'TF1Sm ', 'TF2Sm ', 'TF3Sm ', 'TF4Sm'
          do i=1, npts1/2
            write (39+iFile,'( f10.4,4f10.4)') df*(i-1), TF1Sm(i), TF2Sm(i), TF3Sm(i), TF4Sm(i)
          enddo
          close(39+iFile) 

c         print unsmoothed transfer functions to file, freq domain
          write (49+iFile,*) rockfile, pga
          write (49+iFile,*) 'freq(Hz) ', 'TF1 ', 'TF2 ', 'TF3 ', 'TF4'
          do i=1, npts1/2
            write (49+iFile,'( f10.4,4f10.4)') df*(i-1), TF1(i), TF2(i), TF3(i), TF4(i)
          enddo
          close(49+iFile) 

c        print all pga, TFmax, freqMax, freqlow, freqhigh to one file
          write (f,*) pga, ffmax1, ffmax2, ffmax3, ffmax4
          write (g,*) pga, sfmax1, sfmax2, sfmax3, sfmax4

   10   continue   
        enddo
        
        write (*,*) 'count1 ', count1
        write (*,*) 'count2 ', count2
        pause
        
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
