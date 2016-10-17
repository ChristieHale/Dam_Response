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
c        write (f,*) 'pga ', 'fmfMax1 ', 'fmfMax2 ', 'fmfMax3 ', 'fmfMax4 ', 'fmfMax5 ', 'fmfMax6 '
c        write (g,*) 'pga ', 'smfMax1 ', 'smfMax2 ', 'smfMax3 ', 'smfMax4 ', 'smfMax5 ', 'smfMax6 '
        write (f,*) 'pga ', 'fmfMax1 ', 'fmfMax2 ', 'fmfMax3 ', 'fmfMax4 ', 'fmfMax5 '
        write (g,*) 'pga ', 'smfMax1 ', 'smfMax2 ', 'smfMax3 ', 'smfMax4 ', 'smfMax5 '
        write (aa,*) 'pga ', 'dis1 ', 'disSDOF5 ', 'alpha5 ', 'damping5 '
c        write (h,*) 'pga ', 'Win_len1 ', 'Win_len2 ', 'Win_len3 ', 'Win_len4 '
        
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
 
c         read in dam time history - failure surfaces
          read (bb,'( a80)') surfacefile
          write (*,'( i5, a80)') iFile, surfacefile
          open (cc,file=surfacefile,status='old')
          do j=1,3
            read (cc,'( a1)') dummy
          enddo
          do i=1,npts
c            read (cc,*,end=101) dum2, surf1(i), surf2(i)
            read (cc,*,end=101) dum2, surf1(i)
          enddo
 101      close (cc)

c         Newmark method on failure surface time histories
          call Newmark (surf1, nPts, dt, Ky1, vel1, dis1)
c          call Newmark (surf2, nPts, dt, Ky2, vel2, dis2) 
        
          npts1 = i-1     
 
c         compute FFT
          call calcFFT (rockTH, nPts, dt, df, mRock, fasRock, cuRock, npts1)
          call calcFFT (dam1, nPts, dt, df, mDam1, fasDam1, cuDam1, npts1)
          call calcFFT (dam2, nPts, dt, df, mDam2, fasDam2, cuDam2, npts1)
          call calcFFT (dam3, nPts, dt, df, mDam3, fasDam3, cuDam3, npts1)
          call calcFFT (dam4, nPts, dt, df, mDam4, fasDam4, cuDam4, npts1)
          call calcFFT (surf1, nPts, dt, df, msurf1, fasSurf1, cuSurf1, npts1)
c          call calcFFT (surf2, nPts, dt, df, msurf2, fasSurf2, cuSurf2, npts1)

c         compute raw transfer function
          do i=1,npts1/2
            TF1(i) = fasDam1(i) / fasRock(i)
            TF2(i) = fasDam2(i) / fasRock(i)
            TF3(i) = fasDam3(i) / fasRock(i)
            TF4(i) = fasDam4(i) / fasRock(i)
            TF5(i) = fasSurf1(i) / fasRock(i)
c            TF6(i) = fasSurf2(i) / fasRock(i)
          enddo

c         smooth FFT with a variable smoothing routine
c          call var_smooth(Win_len0, loopmax, WinType, Win_len, fasRock, npts1, 
c     1                    fasDam1, df, sigmax, TF1Sm, Win_len1)     
c          call var_smooth(Win_len0, loopmax, WinType, Win_len, fasRock, npts1, 
c     1                    fasDam2, df, sigmax, TF2Sm, Win_len2)
c          call var_smooth(Win_len0, loopmax, WinType, Win_len, fasRock, npts1, 
c     1                    fasDam3, df, sigmax, TF3Sm, Win_len3)     
c          call var_smooth(Win_len0, loopmax, WinType, Win_len, fasRock, npts1, 
c     1                    fasDam4, df, sigmax, TF4Sm, Win_len4)     
c          call var_smooth(Win_len0, loopmax, WinType, Win_len, fasRock, npts1, 
c     1                    fasSurf1, df, sigmax, TF5Sm, Win_len5)     
c          call var_smooth(Win_len0, loopmax, WinType, Win_len, fasRock, npts1, 
c     1                    fasSurf2, df, sigmax, TF6Sm, Win_len6)     

c        no smoothing for now
         do i=1,npts1/2
           TF1Sm(i) = TF1(i)
           TF2Sm(i) = TF2(i)
           TF3Sm(i) = TF3(i)
           TF4Sm(i) = TF4(i)
           TF5Sm(i) = TF5(i)
c           TF6Sm(i) = TF6(i)
         enddo

c         find first and second mode
          call mode(TF1Sm, df, npts1, fTFmax1, ffmax1, flow1, fhigh1, sTFmax1, sfmax1, slow1, shigh1, mflag1)
          call mode(TF2Sm, df, npts1, fTFmax2, ffmax2, flow2, fhigh2, sTFmax2, sfmax2, slow2, shigh2, mflag2)
          call mode(TF3Sm, df, npts1, fTFmax3, ffmax3, flow3, fhigh3, sTFmax3, sfmax3, slow3, shigh3, mflag3)
          call mode(TF4Sm, df, npts1, fTFmax4, ffmax4, flow4, fhigh4, sTFmax4, sfmax4, slow4, shigh4, mflag4)
          call mode(TF5Sm, df, npts1, fTFmax5, ffmax5, flow5, fhigh5, sTFmax5, sfmax5, slow5, shigh5, mflag5)          
c          call mode(TF6Sm, df, npts1, fTFmax6, ffmax6, flow6, fhigh6, sTFmax6, sfmax6, slow6, shigh6, mflag6)

c         fit first mode to SDOF oscillator
c          call SDOF(npts, dt, df, TF1Sm, ffmax1, flow1, fhigh1, npts2, damping1, alpha1, response1, TFSDOF1, cuSDOF1)
c          call SDOF(npts, dt, df, TF2Sm, ffmax2, flow2, fhigh2, npts2, damping2, alpha2, response2, TFSDOF2, cuSDOF2)
c          call SDOF(npts, dt, df, TF3Sm, ffmax3, flow3, fhigh3, npts2, damping3, alpha3, response3, TFSDOF3, cuSDOF3)
c          call SDOF(npts, dt, df, TF4Sm, ffmax4, flow4, fhigh4, npts2, damping4, alpha4, response4, TFSDOF4, cuSDOF4)    
          call SDOF(npts, dt, df, TF5Sm, ffmax5, flow5, fhigh5, npts2, damping5, alpha5, response5, TFSDOF5, cuSDOF5)
c          call SDOF(npts, dt, df, TF6Sm, ffmax6, flow6, fhigh6, npts2, damping6, alpha6, response6, TFSDOF6, cuSDOF6)
       
c         fit second mode to SDOF oscillator
c         fit first and second mode together
c          if (mflag1 .eq. 2) then   
c            call SDOF(npts, dt, df, TF1Sm, sfmax1, slow1, shigh1, npts2, damping12, alpha12, response12, TFSDOF12, cuSDOF12)
c            call SDOF2(npts, dt, df, TF1Sm, response1, response12, flow1, shigh1, npts3, responsef1, TF2mode1, cuSDOFf1)
c          endif
c          if (mflag2 .eq. 2) then
c            call SDOF(npts, dt, df, TF2Sm, sfmax2, slow2, shigh2, npts2, damping22, alpha22, response22, TFSDOF22, cuSDOF22)
c            call SDOF2(npts, dt, df, TF2Sm, response2, response22, flow2, shigh2, npts3, responsef2, TF2mode2, cuSDOFf2)          
c          endif
c          if (mflag3 .eq. 2) then           
c            call SDOF(npts, dt, df, TF3Sm, sfmax3, slow3, shigh3, npts2, damping32, alpha32, response32, TFSDOF32, cuSDOF32)
c            call SDOF2(npts, dt, df, TF3Sm, response3, response32, flow3, shigh3, npts3, responsef3, TF2mode3, cuSDOFf3)
c          endif
c          if (mflag4 .eq. 2) then           
c            call SDOF(npts, dt, df, TF4Sm, sfmax4, slow4, shigh4, npts2, damping42, alpha42, response42, TFSDOF42, cuSDOF42)
c            call SDOF2(npts, dt, df, TF4Sm, response4, response42, flow4, shigh4, npts3, responsef4, TF2mode4, cuSDOFf4)
c          endif
c          if (mflag5 .eq. 2) then
c            call SDOF(npts, dt, df, TF5Sm, sfmax5, slow5, shigh5, npts2, damping52, alpha52, response52, TFSDOF52, cuSDOF52)
c            call SDOF2(npts, dt, df, TF5Sm, response5, response52, flow5, shigh5, npts3, responsef5, TF2mode5, cuSDOFf5)
c          endif
c          if (mflag6 .eq. 2) then
c            call SDOF(npts, dt, df, TF6Sm, sfmax6, slow6, shigh6, npts2, damping62, alpha62, response62, TFSDOF62, cuSDOF62)
c            call SDOF2(npts, dt, df, TF6Sm, response6, response62, flow6, shigh6, npts3, responsef6, TF2mode6, cuSDOFf6)  
c          endif    

c         Play with filter, fc = 1.5, nPole = 2
c          cuSDOF1_f1 = cuSDOF1 
c          call hipass(1.5, 2, df, npts2, cuSDOF1_f1)          
c         Compute FAS of filtered
c          do i=1,npts2
c	    SDOF1_f1(i) = cabs(cuSDOF1_f1(i))
c          enddo
          
c         Newmark method on SDOF oscillator response (first mode), Crest only, w/ hipass filter
c          call respTH (cuRock, cuSDOF1_f1, npts1, npts, mRock, respTH1_f1) 
c          call Newmark (respTH1_f1, nPts, dt, Ky1, velSDOF1_f1, disSDOF1_f1)        
          
c         Newmark method on SDOF oscillator response (first mode), Crest only
c          call respTH (cuRock, cuSDOF1, npts1, npts, mRock, respTH1)
c          call respTH2 (RockTH, response1, npts, respTH1)
c          call Newmark (respTH1, nPts, dt, Ky1, velSDOF1, disSDOF1)

c         Newmark method on SDOF oscillator response (first mode), surface1
          call respTH (cuRock, cuSDOF5, npts1, npts, mRock, respTH5)
          call Newmark (respTH5, nPts, dt, Ky1, velSDOF5, disSDOF5)
          
c         Newmark method on SDOF oscillator response (first and second mode together), Crest only
c          call respTH (cuRock, cuSDOFf1, npts1, npts, mRock, respTHf1)
c          call respTH2 (RockTH, responsef1, npts, respTHf1)
c          call Newmark (respTHf1, nPts, dt, Ky1, velSDOFf1, disSDOFf1)

c         print acceleration time histories to file
c          write (19,*) 'time(s) ', 'dam1 ', 'dam2 ', 'dam3 ', 'dam4 ', 'rockTH ' 
c          do i=1, npts
c            write (19,'( f10.4,5f15.10)') dt*i, dam1(i), dam2(i), dam3(i), dam4(i), rockTH(i)
c          enddo
c          close(19)

c         print velocity time histories to file
c          write (20,*) 'time(s) ', 'vel1(cm/s) ', 'velSDOF1(cm/s) ', 'velSDOFf1(cm/s) '
c          do i=1, npts
c            write (20,'( f10.4,3f10.4)') dt*i, vel1(i), velSDOF1(i), velSDOFf1(i)
c          enddo
c          close(20)

c         print displacement time histories to file
c          write (21,*) 'time(s) ', 'dis1(cm) ', 'disSDOF1(cm) ', 'disSDOFf1(cm) ', 'disSDOF1_f1(cm) '
c          do i=1, npts
c            write (21,'( f10.4,4f10.4)') dt*i, dis1(i), disSDOF1(i), disSDOFf1(i), disSDOF1_f1(i)
c          enddo
c          close(21)

c         print raw fas to file
c          write (29+iFile,*) rockfile, pga
c          write (29+iFile,*) 'freq(Hz) ', 'fasRock ', 'fasDam1 ', 'fasDam2 ', 'fasDam3 ', 'fasDam4 '
c          do i=1, npts1
c            write (29+iFile,'( f10.4,5f10.4)') df*(i-1), fasRock(i), fasDam1(i), fasDam2(i), fasDam3(i), fasDam4(i)
c          enddo
c          close(29+iFile)
 
c         print raw transfer functions to file
c          write (39+iFile,*) rockfile, pga
c          write (39+iFile,*) 'freq(Hz) ', 'TF1 ', 'TF2 ', 'TF3 ', 'TF4 ', 'TF5 ', 'TF6 ' 
c          write (39+iFile,*) 'freq(Hz) ', 'TF1 ', 'TF2 ', 'TF3 ', 'TF4 ', 'TF5 ' 
c          do i=1, npts1/2
c           write (39+iFile,'( f10.4,6f10.4)') df*(i-1), TF1(i), TF2(i), TF3(i), TF4(i), TF5(i), TF6(i)
c            write (39+iFile,'( f10.4,5f10.4)') df*(i-1), TF1(i), TF2(i), TF3(i), TF4(i), TF5(i)
c          enddo
c          close(39+iFile)

c         print raw transfer functions to file
          write (39+iFile,*) rockfile, pga
          write (39+iFile,*) 'freq(Hz) ', 'TF5 ' 
          do i=1, npts1/2
            write (39+iFile,'(2f10.4)') df*(i-1), TF5(i)
          enddo
          close(39+iFile)

c         print smooth transfer functions to file
c          write (49+iFile,*) rockfile
c          write (49+iFile,*) 'freq(Hz) ', 'TF1Sm ', 'TF2Sm ', 'TF3Sm ', 'TF4Sm ', 'TF5Sm ', 'TF6Sm ' 
c          do i=1, npts1/2
c            write (49+iFile,'( f10.4,6f10.4)') df*(i-1), TF1Sm(i), TF2Sm(i), TF3Sm(i), TF4Sm(i), TF5Sm(i), TF6Sm(i)
c          enddo
c          close(49+iFile) 
          
c         print SDOF transfer functions to file, first mode
c          write (59+iFile,*) rockfile
c          write (59+iFile,*) 'damping1 ', 'damping2 ', 'damping3 ', 'damping4 '
c          write (59+iFile,*) damping1, damping2, damping3, damping4
c          write (59+iFile,*) 'alpha1 ', 'alpha2 ', 'alpha3 ', 'alpha4 '
c          write (59+iFile,*) alpha1, alpha2, alpha3, alpha4
c          write (59+iFile,*) 'freq(Hz) ', 'TFSDOF1 ', 'TFSDOF2 ', 'TFSDOF3 ', 'TFSDOF4 '
c          do i=1, npts2/2
c            write (59+iFile,'( f10.4,4f10.4)') df*(i-1), TFSDOF1(i), TFSDOF2(i), TFSDOF3(i), TFSDOF4(i)
c          enddo
c          close(59+iFile) 
          
c         print SDOF transfer functions to file, first mode
          write (59+iFile,*) rockfile
          write (59+iFile,*) 'damping5 '
          write (59+iFile,*) damping5
          write (59+iFile,*) 'alpha5 '
          write (59+iFile,*) alpha5
          write (59+iFile,*) 'freq(Hz) ', 'TFSDOF5 '
          do i=1, npts2/2
            write (59+iFile,'(2f10.4)') df*(i-1), TFSDOF5(i)
          enddo
          close(59+iFile)
          
c         print SDOF transfer functions to file, second mode
c          write (69+iFile,*) rockfile
c          write (69+iFile,*) 'damping12 ', 'damping22 ', 'damping32 ', 'damping42 '
c          write (69+iFile,*) damping12, damping22, damping32, damping42
c          write (69+iFile,*) 'alpha12 ', 'alpha22 ', 'alpha32 ', 'alpha42 '
c          write (69+iFile,*) alpha12, alpha22, alpha32, alpha42
c          write (69+iFile,*) 'freq(Hz) ', 'TFSDOF12 ', 'TFSDOF22 ', 'TFSDOF32 ', 'TFSDOF42 '
c          do i=1, npts2/2
c            write (69+iFile,'( f10.4,4f10.4)') df*(i-1), TFSDOF12(i), TFSDOF22(i), TFSDOF32(i), TFSDOF42(i)
c          enddo
c          close(69+iFile) 
          
c         print SDOF transfer functions to file, both modes
c          write (79+iFile,*) rockfile
c          write (79+iFile,*) 'freq(Hz) ', 'TF2mode1 ', 'TF2mode2 ', 'TF2mode3 ', 'TF2mode4 '
c          do i=1, npts3/2
c            write (79+iFile,'( f10.4,4f10.4)') df*(i-1), TF2mode1(i), TF2mode2(i), TF2mode3(i), TF2mode4(i)
c          enddo
c          close(79+iFile) 

c         print filtered SDOF transfer functions to file, first mode
c          write (89+iFile,*) 'freq(Hz) ', 'TFSDOF1 ', 'SDOF1_f1 '
c          do i=1, npts2/2
c            write (89+iFile,'( f10.4,2f10.4)') df*(i-1), TFSDOF1(i), SDOF1_f1(i)
c          enddo
c          close(89+iFile) 

c         print all pga, ffmax, sfmax, Win_len, damping, alpha to one file
c          write (f,*) pga, ffmax1, ffmax2, ffmax3, ffmax4, ffmax5, ffmax6
c          write (g,*) pga, sfmax1, sfmax2, sfmax3, sfmax4, sfmax5, sfmax6
          write (f,*) pga, ffmax1, ffmax2, ffmax3, ffmax4, ffmax5
          write (g,*) pga, sfmax1, sfmax2, sfmax3, sfmax4, sfmax5
          write (aa,*) pga, dis1(npts), disSDOF5(npts), alpha5, damping5
c          write (h,*) pga, Win_len1, Win_len2, Win_len3, Win_len4, Win_len5, Win_len6
        enddo   

        close(f)
        close(g)
        close(aa)        
     
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
       
      subroutine mode(TFSm, df, npts1, fTFmax, ffmax, flow, fhigh, sTFmax, sfmax, slow, shigh, mflag)
      
       implicit none
       include 'max_dims.h'
       
       real TFSm(MAXPTS), df, fTFmax, ffmax, flow, fhigh, sTFmax, sfmax, 
     1      slow, shigh, dTFmax, dfmax, dTF_half, dlow, dhigh, ndlow, ndhigh,
     2      TFmax1, TFmax2, fmax1, fmax2, ndTFmax, ndfmax, ndTF_half
       integer i, j, k, m, n, p, q, Hz30, imax_freq, npts1, mflag, imin_freq, Hz0p5
       
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
