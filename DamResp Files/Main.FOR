      program DamResp
      
      implicit none
      include 'max_dims.H'
 
      integer a, b, c, d, e, f, g, h, aa
      parameter (a=10, b=11, c=12, d=13, e=14, f=15, g=16, h=17, aa=18)
      integer WinType, Win_len, Win_len0, Win_len1, Win_len2, Win_len3
      integer Win_len4, xflag1, xflag2, xflag3, xflag4   
      integer nFiles, iFile, i, j, npts, npts1, npts2, npts3, loopmax
      integer mRock, mDam1, mDam2, mDam3, mDam4
      real rockTH(MAXPTS), dam1(MAXPTS), dam2(MAXPTS), dam3(MAXPTS), dam4(MAXPTS), Ky
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
      real sigmax
      real sigmaN1(MAXPTS), sigmaN2(MAXPTS), sigmaN3(MAXPTS), sigmaN4(MAXPTS)
      real damping1, damping2, damping3, damping4
      real alpha1, alpha2, alpha3, alpha4
      real response1(MAXPTS), response2(MAXPTS), response3(MAXPTS), response4(MAXPTS)
      real response12(MAXPTS), response22(MAXPTS), response32(MAXPTS), response42(MAXPTS)
      real responsef1(MAXPTS), responsef2(MAXPTS), responsef3(MAXPTS), responsef4(MAXPTS)
      real damping12, damping22, damping32, damping42
      real alpha12, alpha22, alpha32, alpha42
      real TFSDOF1(MAXPTS), TFSDOF2(MAXPTS), TFSDOF3(MAXPTS), TFSDOF4(MAXPTS) 
      real TFSDOF12(MAXPTS), TFSDOF22(MAXPTS), TFSDOF32(MAXPTS), TFSDOF42(MAXPTS)
      real TF2mode1(MAXPTS), TF2mode2(MAXPTS), TF2mode3(MAXPTS), TF2mode4(MAXPTS) 
      real respTH1(MAXPTS), respTHf1(MAXPTS), respTH1_f1(MAXPTS) 
      real vel1(MAXPTS), dis1(MAXPTS), velSDOF1(MAXPTS), disSDOF1(MAXPTS)
      real velSDOFf1(MAXPTS), disSDOFf1(MAXPTS), velSDOF1_f1(MAXPTS), disSDOF1_f1(MAXPTS) 
      real SDOF1_f1(MAXPTS), SDOF1_f2(MAXPTS), SDOF1_f3(MAXPTS)
      real max_fasrock, frinput, prinput
      complex cuRock(MAXPTS), cuDam1(MAXPTS), cuDam2(MAXPTS), cuDam3(MAXPTS), cuDam4(MAXPTS)
      complex cuSDOF1(MAXPTS), cuSDOF2(MAXPTS), cuSDOF3(MAXPTS), cuSDOF4(MAXPTS)
      complex cuSDOF12(MAXPTS), cuSDOF22(MAXPTS), cuSDOF32(MAXPTS), cuSDOF42(MAXPTS)
      complex cuSDOFf1(MAXPTS), cuSDOFf2(MAXPTS), cuSDOFf3(MAXPTS), cuSDOFf4(MAXPTS)
      complex cuSDOF1_f1(MAXPTS), cuSDOF1_f2(MAXPTS), cuSDOF1_f3(MAXPTS)
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
        read (a,*) Ky    

c       write headers for output files
        write (f,*) 'pga ', 'fmfMax1 ', 'fmfMax2 ', 'fmfMax3 ', 'fmfMax4 '
        write (g,*) 'pga ', 'smfMax1 ', 'smfMax2 ', 'smfMax3 ', 'smfMax4 '
        write (h,*) 'pga ', 'Win_len1 ', 'Win_len2 ', 'Win_len3 ', 'Win_len4 '
        write (aa,*) 'pga ', 'max_fasrock ', 'prinput '
        
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
          
          write (*,*) 'npts ', npts
          write (*,*) 'dt ', dt
          pause

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

c         Newmark method on Crest time history
          call Newmark (dam1, nPts, dt, Ky, vel1, dis1) 
        
          npts1 = i-1     
 
c         compute FFT
          call calcFFT (rockTH, nPts, dt, df, mRock, fasRock, cuRock, npts1)
          call calcFFT (dam1, nPts, dt, df, mDam1, fasDam1, cuDam1, npts1)
          call calcFFT (dam2, nPts, dt, df, mDam2, fasDam2, cuDam2, npts1)
          call calcFFT (dam3, nPts, dt, df, mDam3, fasDam3, cuDam3, npts1)
          call calcFFT (dam4, nPts, dt, df, mDam4, fasDam4, cuDam4, npts1)

c         find period corresponding to max Sa from rock TH
          max_fasrock = 0.
          do i=1,npts1
            if ( abs( fasRock(i)) .gt. max_fasrock ) then
              max_fasrock = abs(fasRock(i))
              frinput = df*(i-1)
              prinput = 1./frinput              
            endif              
          enddo

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
            call variance(Win_len, df, TF1Sm, sigmax, sigmaN1, xflag1)
        
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
            call variance(Win_len, df, TF2Sm, sigmax, sigmaN2, xflag2)

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
            call variance(Win_len, df, TF3Sm, sigmax, sigmaN3, xflag3)

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
            call variance(Win_len, df, TF4Sm, sigmax, sigmaN4, xflag4)

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

c         fit first mode to SDOF oscillator
          call SDOF(npts, dt, df, TF1Sm, ffmax1, flow1, fhigh1, npts2, damping1, alpha1, response1, TFSDOF1, cuSDOF1)
          call SDOF(npts, dt, df, TF2Sm, ffmax2, flow2, fhigh2, npts2, damping2, alpha2, response2, TFSDOF2, cuSDOF2)
          call SDOF(npts, dt, df, TF3Sm, ffmax3, flow3, fhigh3, npts2, damping3, alpha3, response3, TFSDOF3, cuSDOF3)
          call SDOF(npts, dt, df, TF4Sm, ffmax4, flow4, fhigh4, npts2, damping4, alpha4, response4, TFSDOF4, cuSDOF4)    

c         fit second mode to SDOF oscillator
          call SDOF(npts, dt, df, TF1Sm, sfmax1, slow1, shigh1, npts2, damping12, alpha12, response12, TFSDOF12, cuSDOF12)
          call SDOF(npts, dt, df, TF2Sm, sfmax2, slow2, shigh2, npts2, damping22, alpha22, response22, TFSDOF22, cuSDOF22)
          call SDOF(npts, dt, df, TF3Sm, sfmax3, slow3, shigh3, npts2, damping32, alpha32, response32, TFSDOF32, cuSDOF32)
          call SDOF(npts, dt, df, TF4Sm, sfmax4, slow4, shigh4, npts2, damping42, alpha42, response42, TFSDOF42, cuSDOF42)

c         first and second mode together
          call SDOF2(npts, dt, df, TF1Sm, response1, response12, flow1, shigh1, npts3, responsef1, TF2mode1, cuSDOFf1)
          call SDOF2(npts, dt, df, TF2Sm, response2, response22, flow2, shigh2, npts3, responsef2, TF2mode2, cuSDOFf2)
          call SDOF2(npts, dt, df, TF3Sm, response3, response32, flow3, shigh3, npts3, responsef3, TF2mode3, cuSDOFf3)
          call SDOF2(npts, dt, df, TF4Sm, response4, response42, flow4, shigh4, npts3, responsef4, TF2mode4, cuSDOFf4)

c         Play with filter, fc = 1.5, nPole = 2
          cuSDOF1_f1 = cuSDOF1 
          call hipass(1.5, 2, df, npts2, cuSDOF1_f1)          
c         Compute FAS of filtered
          do i=1,npts2
	    SDOF1_f1(i) = cabs(cuSDOF1_f1(i))
          enddo
c         Newmark method on SDOF oscillator response (first mode), Crest only, w/ hipass filter
          call respTH (cuRock, cuSDOF1_f1, npts1, npts, mRock, respTH1_f1) 
          call Newmark (respTH1_f1, nPts, dt, Ky, velSDOF1_f1, disSDOF1_f1)        
          
c         Newmark method on SDOF oscillator response (first mode), Crest only
          call respTH (cuRock, cuSDOF1, npts1, npts, mRock, respTH1)
c          call respTH2 (RockTH, response1, npts, respTH1)
          call Newmark (respTH1, nPts, dt, Ky, velSDOF1, disSDOF1)
          
c         Newmark method on SDOF oscillator response (first and second mode together), Crest only
          call respTH (cuRock, cuSDOFf1, npts1, npts, mRock, respTHf1)
c          call respTH2 (RockTH, responsef1, npts, respTHf1)
          call Newmark (respTHf1, nPts, dt, Ky, velSDOFf1, disSDOFf1)

c         print acceleration time histories to file
          write (19,*) 'time(s) ', 'dam1 ', 'respTH1 ', 'respTHf1 '
          do i=1, npts
            write (19,'( f10.4,3f10.4)') dt*i, dam1(i), respTH1(i), respTHf1(i)
          enddo
          close(19)

c         print velocity time histories to file
          write (20,*) 'time(s) ', 'vel1(cm/s) ', 'velSDOF1(cm/s) ', 'velSDOFf1(cm/s) '
          do i=1, npts
            write (20,'( f10.4,3f10.4)') dt*i, vel1(i), velSDOF1(i), velSDOFf1(i)
          enddo
          close(20)

c         print displacement time histories to file
          write (21,*) 'time(s) ', 'dis1(cm) ', 'disSDOF1(cm) ', 'disSDOFf1(cm) ', 'disSDOF1_f1(cm) '
          do i=1, npts
            write (21,'( f10.4,4f10.4)') dt*i, dis1(i), disSDOF1(i), disSDOFf1(i), disSDOF1_f1(i)
          enddo
          close(21)

c         print raw fas to file
          write (29+iFile,*) rockfile, pga
          write (29+iFile,*) 'freq(Hz) ', 'fasRock ', 'fasDam1 ', 'fasDam2 ', 'fasDam3 ', 'fasDam4 '
          do i=1, npts1
            write (29+iFile,'( f10.4,5f10.4)') df*(i-1), fasRock(i), fasDam1(i), fasDam2(i), fasDam3(i), fasDam4(i)
          enddo
          close(29+iFile)
 
c         print raw transfer functions to file
          write (39+iFile,*) rockfile, pga
          write (39+iFile,*) 'freq(Hz) ', 'TF1 ', 'TF2 ', 'TF3 ', 'TF4'
          do i=1, npts1/2
            write (39+iFile,'( f10.4,4f10.4)') df*(i-1), TF1(i), TF2(i), TF3(i), TF4(i)
          enddo
          close(39+iFile)

c         print smooth transfer functions to file
          write (49+iFile,*) rockfile
          write (49+iFile,*) 'freq(Hz) ', 'TF1Sm ', 'TF2Sm ', 'TF3Sm ', 'TF4Sm'
          do i=1, npts1/2
            write (49+iFile,'( f10.4,4f10.4)') df*(i-1), TF1Sm(i), TF2Sm(i), TF3Sm(i), TF4Sm(i)
          enddo
          close(49+iFile) 
          
c         print SDOF transfer functions to file, first mode
          write (59+iFile,*) rockfile
          write (59+iFile,*) 'damping1 ', 'damping2 ', 'damping3 ', 'damping4 '
          write (59+iFile,*) damping1, damping2, damping3, damping4
          write (59+iFile,*) 'alpha1 ', 'alpha2 ', 'alpha3 ', 'alpha4 '
          write (59+iFile,*) alpha1, alpha2, alpha3, alpha4
          write (59+iFile,*) 'freq(Hz) ', 'TFSDOF1 ', 'TFSDOF2 ', 'TFSDOF3 ', 'TFSDOF4 '
          do i=1, npts2/2
            write (59+iFile,'( f10.4,4f10.4)') df*(i-1), TFSDOF1(i), TFSDOF2(i), TFSDOF3(i), TFSDOF4(i)
          enddo
          close(59+iFile) 
          
c         print SDOF transfer functions to file, second mode
          write (69+iFile,*) rockfile
          write (69+iFile,*) 'damping12 ', 'damping22 ', 'damping32 ', 'damping42 '
          write (69+iFile,*) damping12, damping22, damping32, damping42
          write (69+iFile,*) 'alpha12 ', 'alpha22 ', 'alpha32 ', 'alpha42 '
          write (69+iFile,*) alpha12, alpha22, alpha32, alpha42
          write (69+iFile,*) 'freq(Hz) ', 'TFSDOF12 ', 'TFSDOF22 ', 'TFSDOF32 ', 'TFSDOF42 '
          do i=1, npts2/2
            write (69+iFile,'( f10.4,4f10.4)') df*(i-1), TFSDOF12(i), TFSDOF22(i), TFSDOF32(i), TFSDOF42(i)
          enddo
          close(69+iFile) 
          
c         print SDOF transfer functions to file, both modes
          write (79+iFile,*) rockfile
          write (79+iFile,*) 'freq(Hz) ', 'TF2mode1 ', 'TF2mode2 ', 'TF2mode3 ', 'TF2mode4 '
          do i=1, npts3/2
            write (79+iFile,'( f10.4,4f10.4)') df*(i-1), TF2mode1(i), TF2mode2(i), TF2mode3(i), TF2mode4(i)
          enddo
          close(79+iFile) 

c         print filtered SDOF transfer functions to file, first mode
          write (89+iFile,*) 'freq(Hz) ', 'TFSDOF1 ', 'SDOF1_f1 '
          do i=1, npts2/2
            write (89+iFile,'( f10.4,2f10.4)') df*(i-1), TFSDOF1(i), SDOF1_f1(i)
          enddo
          close(89+iFile) 

c         print all pga, ffmax, sfmax, Win_len, damping, alpha to one file
          write (f,*) pga, ffmax1, ffmax2, ffmax3, ffmax4
          write (g,*) pga, sfmax1, sfmax2, sfmax3, sfmax4
          write (h,*) pga, Win_len1, Win_len2, Win_len3, Win_len4
          write (aa,*) pga, max_fasrock, prinput
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
