      program DamResp
      
      implicit none
      include 'max_dims.h'  
      include 'declare.h'

c       read in run file 
        write (*,'( 2x,''Enter run file'')')
        read (*,'( a80)') runfile
        open (a,file=runfile,status='old')
        read (a,'( a80)') lsrockfile
        read (a,'( a80)') lsdamfile
        read (a,'( a80)') lssurface
        open (b,file=lsrockfile,status='old')
        open (c,file=lsdamfile,status='old')
        open (bb,file=lssurface,status='old')
        read (a,*) nFiles   
        read (a,*) WinType
        read (a,*) Win_len0  
        read (a,*) sigmax
        read (a,*) loopmax   
        read (a,*) Ky1
        read (a,*) Ky2    

c       write headers for output files
        write (f,*) 'pga ', 'ffmax5 ', 'ffmax6 '
        write (g,*) 'pga ', 'pga_c '
        write (h,*) 'pga ', 'alpha5 ', 'damping5 ', 'alpha6 ', 'damping6 '
        write (aa,*) 'pga ', 'dis1p ', 'disSDOF5p ', 'dis1n ', 'disSDOF5n ', 'dis2p ', 'disSDOF6p ',
     1               'dis2n ', 'disSDOF6n ' 
        write (dd,*) 'pga ', 'Win_len5 ', 'Win_len6 '
        
c       loop over number of files (rock time histories)    
        do iFile=1,nFiles

c         initialize arrays
          call init(rockTH, surf1, surf2, fasRock, fasSurf1, fasSurf2, TF5, 
     1              TF5Sm, TF6, TF6Sm, vel1p, dis1p, vel2p, dis2p, vel1n, 
     2              dis1n, vel2n, dis2n, velSDOF5p, disSDOF5p, velSDOF5n, 
     3              disSDOF5n, velSDOF6p, disSDOF6p, velSDOF6n, disSDOF6n)

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

c         read in dam time history - nodes
          read (c,'( a80)') damfile
          write (*,'( i5, a80)') iFile, damfile
          open (e,file=damfile,status='old')
          do j=1,3
            read (e,'( a1)') dummy
          enddo
          do i=1,npts
            read (e,*,end=100) dum1, dam1(i), dam2(i), dam3(i), dam4(i)
          enddo
 100      close (e)
 
c         find PGA crest
          pga_c = 0.
          do i=1,npts
            if ( abs( dam4(i)) .gt. pga_c ) pga_c = abs(dam4(i))
          enddo
 
c         read in dam time history - failure surfaces
          read (bb,'( a80)') surfacefile
          write (*,'( i5, a80)') iFile, surfacefile
          open (cc,file=surfacefile,status='old')
          do j=1,3
            read (cc,'( a1)') dummy
          enddo
          do i=1,npts
            read (cc,*,end=101) dum2, surf1(i), surf2(i)
          enddo
 101      close (cc)

c         Newmark method on failure surface time histories, positive acc
          call Newmark (surf1, npts, dt, Ky1, vel1p, dis1p)
          call Newmark (surf2, npts, dt, Ky2, vel2p, dis2p) 
          
          do i=1,npts
            surf1n(i) = surf1(i)*(-1.)
            surf2n(i) = surf2(i)*(-1.)
          enddo     
          
c         Newmark method on failure surface time histories, negative acc
          call Newmark (surf1n, npts, dt, Ky1, vel1n, dis1n)
          call Newmark (surf2n, npts, dt, Ky2, vel2n, dis2n)
   
          npts1 = i-1     
 
c         compute FFT
          call calcFFT (rockTH, npts, dt, df, mRock, fasRock, cuRock, npts1)
          call calcFFT (surf1, npts, dt, df, msurf1, fasSurf1, cuSurf1, npts1)
          call calcFFT (surf2, npts, dt, df, msurf2, fasSurf2, cuSurf2, npts1)

c         compute raw transfer function
          do i=1,npts1/2
            TF5(i) = fasSurf1(i) / fasRock(i)
            TF6(i) = fasSurf2(i) / fasRock(i)
          enddo

c         smooth FFT with one pass of smoothing
c           call once_smooth(Win_len0, WinType, fasRock, npts1, fasSurf1, TF5Sm)
c           call once_smooth(Win_len0, WinType, fasRock, npts1, fasSurf2, TF6Sm)

c         smooth FFT with a variable smoothing routine    
          call var_smooth(Win_len0, loopmax, WinType, fasRock, npts1, 
     1                    fasSurf1, df, sigmax, TF5Sm, Win_len5)     
          call var_smooth(Win_len0, loopmax, WinType, fasRock, npts1, 
     1                    fasSurf2, df, sigmax, TF6Sm, Win_len6)     

c        no smoothing for now
c         do i=1,npts1/2
c           TF5Sm(i) = TF5(i)
c           TF6Sm(i) = TF6(i)
c         enddo

c         find first mode only
          call mode1(TF5Sm, df, npts1, fTFmax5, ffmax5, flow5, flow15, fhigh5)
          call mode1(TF6Sm, df, npts1, fTFmax6, ffmax6, flow6, flow16, fhigh6)

c         fit first mode to SDOF oscillator    
          call SDOF(npts, dt, df, TF5Sm, ffmax5, flow5, flow15, fhigh5, npts2, damping5, alpha5, 
     1              response5, TFSDOF5, cuSDOF5)
          call SDOF(npts, dt, df, TF6Sm, ffmax6, flow6, flow16, fhigh6, npts2, damping6, alpha6, 
     1              response6, TFSDOF6, cuSDOF6)   

c         Newmark method on SDOF oscillator response (first mode)
          call respTH (cuRock, cuSDOF5, npts1, npts, mRock, respTH5)
          call respTH (cuRock, cuSDOF6, npts1, npts, mRock, respTH6)
          call Newmark (respTH5, npts, dt, Ky1, velSDOF5p, disSDOF5p)
          call Newmark (respTH6, npts, dt, Ky2, velSDOF6p, disSDOF6p)
          
          do i=1,npts
            respTH5n(i) = respTH5(i)*(-1.)
            respTH6n(i) = respTH6(i)*(-1.)
          enddo
          
          call Newmark (respTH5n, npts, dt, Ky1, velSDOF5n, disSDOF5n)
          call Newmark (respTH6n, npts, dt, Ky2, velSDOF6n, disSDOF6n)

c         print acceleration time histories to file
c          write (19+iFile,*) rockfile, pga
c          write (19+iFile,*) 'time(s) ', 'surf1 ' 
c          do i=1, npts
c            write (19+iFile,'( f10.4,f15.10)') dt*i, surf1(i)
c          enddo
c          close(19+iFile)

c         print velocity time histories to file
c          write (29+iFile,*) rockfile, pga
c          write (29+iFile,*) 'time(s) ', 'vel1(cm/s) '
c          do i=1, npts
c            write (29+iFile,'( f10.4,f15.10)') dt*i, vel1(i)
c          enddo
c          close(29+iFile)

c         print displacement time histories to file
c          write (39+iFile,*) rockfile, pga
c          write (39+iFile,*) 'time(s) ', 'dis1(cm) '
c          do i=1, npts
c            write (39+iFile,'( f10.4,f15.10)') dt*i, dis1(i)
c          enddo
c          close(39+iFile)

c         print raw fas to file
c          write (29+iFile,*) rockfile, pga
c          write (29+iFile,*) 'freq(Hz) ', 'fasRock ', 'fasDam1 ', 'fasDam2 ', 'fasDam3 ', 'fasDam4 '
c          do i=1, npts1
c            write (29+iFile,'( f10.4,5f10.4)') df*(i-1), fasRock(i), fasDam1(i), fasDam2(i), fasDam3(i), fasDam4(i)
c          enddo
c          close(29+iFile)

c         print raw transfer functions to file
          write (39+iFile,*) rockfile, pga
          write (39+iFile,*) 'freq(Hz) ', 'TF5 ', 'TF6 ' 
          do i=1, npts1/2
            write (39+iFile,'(3f10.4)') df*(i-1), TF5(i), TF6(i)
          enddo
          close(39+iFile)
          
c         print complex to file
c          write (49+iFile,*) rockfile, pga
c          write (49+iFile,*) 'freq(Hz) ', 'real5 ', 'imag5 ', 'TFSDOF5 ', 'real6 ', 'imag6 ', 'TFSDOF6 '
c          do i=1, npts1
c            write (49+iFile,'(7f10.4)') df*(i-1), real(cuSDOF5(i)), aimag(cuSDOF5(i)), TFSDOF5(i),
c     1             real(cuSDOF6(i)), aimag(cuSDOF6(i)), TFSDOF6(i)
c          enddo
c          close(49+iFile)
          
c         print smoothed transfer functions to file
          write (59+iFile,*) rockfile, pga
          write (59+iFile,*) 'freq(Hz) ', 'TF5Sm ', 'TF6Sm ' 
          do i=1, npts1/2
            write (59+iFile,'(3f10.4)') df*(i-1), TF5Sm(i), TF6Sm(i)
          enddo
          close(59+iFile)
          
c         print SDOF transfer functions to file, first mode
          write (79+iFile,*) rockfile
          write (79+iFile,*) 'damping5 ', 'alpha5 ', 'damping6 ', 'alpha6 '
          write (79+iFile,*) damping5, alpha5, damping6, alpha6
          write (79+iFile,*) 'freq(Hz) ', 'TFSDOF5 ', 'TFSDOF6 '
          do i=1, npts2/2
            write (79+iFile,'(3f10.4)') df*(i-1), TFSDOF5(i), TFSDOF6(i)
          enddo
          close(79+iFile)
          
c         print all pga, ffmax, alpha, damping to one file
          write (f,*) pga, ffmax5, ffmax6
          write (g,*) pga, pga_c
          write (h,*) pga, alpha5, damping5, alpha6, damping6
          write (aa,*) pga, dis1p(npts), disSDOF5p(npts), dis1n(npts), disSDOF5n(npts),
     1                 dis2p(npts), disSDOF6p(npts), dis2n(npts), disSDOF6n(npts)
          write (dd,*) pga, Win_len5, Win_len6
        enddo   

        close(f)
        close(g)
        close(h)   
        close(aa)   
        close(dd)
     
        write (*,*) 'Finished'
        pause
     
      end

c ----------------------------------------------------------------------

      subroutine init (rockTH, surf1, surf2, fasRock, fasSurf1, fasSurf2, TF5, 
     1                 TF5Sm, TF6, TF6Sm, vel1p, dis1p, vel2p, dis2p, vel1n, 
     2                 dis1n, vel2n, dis2n, velSDOF5p, disSDOF5p, velSDOF5n, 
     3                 disSDOF5n, velSDOF6p, disSDOF6p, velSDOF6n, disSDOF6n)
      
      implicit none
      include 'max_dims.h'
      
      integer i
      real rockTH(MAXPTS), surf1(MAXPTS), surf2(MAXPTS), fasRock(MAXPTS), 
     1     fasSurf1(MAXPTS), fasSurf2(MAXPTS), TF5(MAXPTS), TF5Sm(MAXPTS),  
     2     TF6(MAXPTS), TF6Sm(MAXPTS), vel1p(MAXPTS), dis1p(MAXPTS), 
     3     vel2p(MAXPTS), dis2p(MAXPTS), vel1n(MAXPTS), dis1n(MAXPTS),  
     4     vel2n(MAXPTS), dis2n(MAXPTS), velSDOF5p(MAXPTS), disSDOF5p(MAXPTS), 
     5     velSDOF5n(MAXPTS), disSDOF5n(MAXPTS), velSDOF6p(MAXPTS),
     6     disSDOF6p(MAXPTS), velSDOF6n(MAXPTS), disSDOF6n(MAXPTS)
      
      do i=1, MAXPTS
        rockTH(i) = 0.0
        surf1(i) = 0.0
        surf2(i) = 0.0
        fasRock(i) = 0.0
        fasSurf1(i) = 0.0
        fasSurf2(i) = 0.0
        TF5(i) = 0.0
        TF5Sm(i) = 0.0
        TF6(i) = 0.0
        TF6Sm(i) = 0.0
        vel1p(i) = 0.0
        dis1p(i) = 0.0
        vel2p(i) = 0.0
        dis2p(i) = 0.0
        vel1n(i) = 0.0
        dis1n(i) = 0.0
        vel2n(i) = 0.0
        dis2n(i) = 0.0
        velSDOF5p(i) = 0.0
        disSDOF5p(i) = 0.0
        velSDOF5n(i) = 0.0
        disSDOF5n(i) = 0.0
        velSDOF6p(i) = 0.0
        disSDOF6p(i) = 0.0
        velSDOF6n(i) = 0.0
        disSDOF6n(i) = 0.0
      enddo

      return
      end

c ----------------------------------------------------------------------
 
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
       
      subroutine mode(TFSm, df, npts1, fTFmax, ffmax, flow, fhigh, sTFmax, sfmax, slow, shigh, mflag)
      
       implicit none
       include 'max_dims.h'
       
       real TFSm(MAXPTS), df, fTFmax, ffmax, flow, fhigh, sTFmax, sfmax, 
     1      slow, shigh, dTFmax, dfmax, dTF_half, dlow, dhigh, ndlow, ndhigh,
     2      TFmax1, TFmax2, fmax1, fmax2, ndTFmax, ndfmax, ndTF_half
       integer i, j, k, m, n, p, q, Hz30, imax_freq, npts1, mflag, imin_freq, Hz0p5
       
       fTFmax = 0.0
       ffmax = 0.0
       flow = 0.0
       fhigh = 0.0
       sTFmax = 0.0
       sfmax = 0.0
       slow = 0.0
       shigh = 0.0
       
c      limit search to maximum frequency of 30 Hz, or Nyquist, whichever is lower
       Hz30 = nint(30./df + 1)
       imax_freq = min(npts1/2,Hz30)
       Hz0p5 = nint(0.5/df + 1)
       imin_freq = Hz0p5
       
c      find dominant frequency 
        dTFmax = 0.
        do i=imin_freq,imax_freq
          if (TFSm(i) .gt. dTFmax ) then
            dTFmax = TFSm(i)
            dfmax = df*(i-1)
            m = i
          endif
        enddo

c       find frequency halfway down peak, lower 
        dTF_half = ((dTFmax-1) / 2.)+1
        do i=m,2,-1
          if ( TFSm(i) .lt. dTF_half) then
            dlow = df*(i-1)
            j = i
            goto 10
          endif
        enddo  

c       find frequency halfway down peak, upper  
   10   do i=m,imax_freq
          if ( TFSm(i) .lt. dTF_half) then
            dhigh = df*(i-1)
            k = i
            goto 20
          endif
        enddo   
   20   continue   
         
c      find the next highest frequency, first try higher 
        TFmax1 = 0.
        do i=k,imax_freq
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
        do i=imin_freq,j
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
        else if (TFmax1 .eq. -999 .and. TFmax2 .eq. -999) then
          fTFmax = dTFmax
          ffmax = dfmax
          flow = dlow
          fhigh = dhigh
          sTFmax = -999
          sfmax = -999
          slow = -999
          shigh = -999
          mflag = 1
          goto 50
        endif

c       find frequency halfway down peak, lower 
        ndTF_half = ((ndTFmax-1) / 2.)+1
        do i=q,2,-1
          if ( TFSm(i) .lt. ndTF_half) then
            ndlow = df*(i-1)
            goto 30
          endif
        enddo   

c       find frequency halfway down peak, upper  
   30   do i=q,imax_freq
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
          mflag = 2
        else if (ndfmax .lt. dfmax) then
          fTFmax = ndTFmax
          ffmax = ndfmax
          flow = ndlow
          fhigh = ndhigh
          sTFmax = dTFmax
          sfmax = dfmax
          slow = dlow
          shigh = dhigh
          mflag = 2
        endif
        
   50   continue     

      return
      end

c -------------------------------------------------------
       
      subroutine mode1(TFSm, df, npts1, fTFmax, ffmax, flow, flow1, fhigh)
      
c     rewrite subroutine mode with only one mode (don't search for second peak) 
      
       implicit none
       include 'max_dims.h'
       
       real TFSm(MAXPTS), df, fTFmax, ffmax, flow, flow1, fhigh, fTF_half, fTF_56
       integer i, m, Hz30, imax_freq, npts1, imin_freq, Hz0p5
       
       fTFmax = 0.0
       ffmax = 0.0
       flow = 0.0
       flow1 = 0.0
       fhigh = 0.0
       
c      limit search to maximum frequency of 30 Hz, or Nyquist, whichever is lower
       Hz30 = nint(30./df + 1)
       imax_freq = min(npts1/2,Hz30)
       Hz0p5 = nint(0.5/df + 1)
       imin_freq = Hz0p5
       
c       find dominant frequency, assume this is first mode 
        fTFmax = 0.
        do i=imin_freq,imax_freq
          if (TFSm(i) .gt. fTFmax ) then
            fTFmax = TFSm(i)
            ffmax = df*(i-1)
            m = i
          endif
        enddo

c       find frequency halfway down peak, lower 
        fTF_half = ((fTFmax-1) / 2.)+1
        do i=m,2,-1
          if ( TFSm(i) .lt. fTF_half) then
            flow = df*(i-1)
            goto 10
          endif
        enddo  
        
c       find frequency 5/6 down peak, lower 
   10   fTF_56 = ((fTFmax-1) / 6.)+1
        do i=m,2,-1
          if ( TFSm(i) .lt. fTF_56) then
            flow1 = df*(i-1)
            goto 20
          endif
        enddo  

c       find frequency halfway down peak, upper  
   20   do i=m,imax_freq
          if ( TFSm(i) .lt. fTF_half) then
            fhigh = df*(i-1)
            goto 30
          endif
        enddo   
   30   continue       

      return
      end
