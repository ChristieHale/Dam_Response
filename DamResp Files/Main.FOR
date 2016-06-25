      program DamResp
      
      implicit none
      include 'max_dims.H'
 
      integer a, b, c, d, e
      parameter (a=10, b=11, c=12, d=13, e=14)
      integer WinType, Win_len    
      integer nFiles, iFile, j, npts, i, npts1
      real rockTH(MAXPTS), dam1(MAXPTS), dam2(MAXPTS), dam3(MAXPTS), dam4(MAXPTS)
      real fasRock(MAXPTS), fasDam1(MAXPTS), fasDam2(MAXPTS), fasDam3(MAXPTS), fasDam4(MAXPTS)
      real fasRockSm(MAXPTS), lnfasRockSm(MAXPTS)
      real fasDam1Sm(MAXPTS), lnfasDam1Sm(MAXPTS)
      real fasDam2Sm(MAXPTS), lnfasDam2Sm(MAXPTS)
      real fasDam3Sm(MAXPTS), lnfasDam3Sm(MAXPTS)
      real fasDam4Sm(MAXPTS), lnfasDam4Sm(MAXPTS)
      real TF1Sm(MAXPTS), TF2Sm(MAXPTS), TF3Sm(MAXPTS), TF4Sm(MAXPTS)
      real TF1(MAXPTS), TF2(MAXPTS), TF3(MAXPTS), TF4(MAXPTS), dt, time, df, pga, pi
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

c         smooth FFT
          call smooth(WinType, Win_len, fasRock, npts1, fasRockSm, lnfasRockSm)
          call smooth(WinType, Win_len, fasDam1, npts1, fasDam1Sm, lnfasDam1Sm)
          call smooth(WinType, Win_len, fasDam2, npts1, fasDam2Sm, lnfasDam2Sm)
          call smooth(WinType, Win_len, fasDam3, npts1, fasDam3Sm, lnfasDam3Sm)
          call smooth(WinType, Win_len, fasDam4, npts1, fasDam4Sm, lnfasDam4Sm)

c         compute transfer function
          do i=1,npts1/2
            TF1Sm(i) = fasDam1Sm(i) / fasRockSm(i)
            TF2Sm(i) = fasDam2Sm(i) / fasRockSm(i)
            TF3Sm(i) = fasDam3Sm(i) / fasRockSm(i)
            TF4Sm(i) = fasDam4Sm(i) / fasRockSm(i)
          enddo
          
c         print smooth transfer functions to file, freq domain
          write (39+iFile,*) rockfile
          write (39+iFile,*) 'freq(Hz) ', 'TF1Sm ', 'TF2Sm ', 'TF3Sm ', 'TF4Sm'
          do i=1, npts1/2
            write (39+iFile,'( f10.4,4f10.4)') df*(i-1), TF1Sm(i), TF2Sm(i), TF3Sm(i), TF4Sm(i)
          enddo
          close(39+iFile) 

c         unsmooth for check
          do i=1,npts1/2
            TF1(i) = fasDam1(i) / fasRock(i)
            TF2(i) = fasDam2(i) / fasRock(i)
            TF3(i) = fasDam3(i) / fasRock(i)
            TF4(i) = fasDam4(i) / fasRock(i)
          enddo

c         print unsmoothed transfer functions to file, freq domain
          write (49+iFile,*) rockfile, pga
          write (49+iFile,*) 'freq(Hz) ', 'TF1 ', 'TF2 ', 'TF3 ', 'TF4'
          do i=1, npts1/2
            write (49+iFile,'( f10.4,4f10.4)') df*(i-1), TF1(i), TF2(i), TF3(i), TF4(i)
          enddo
          close(49+iFile) 

          
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
      end subroutine coeff

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
      end subroutine brs 

      
