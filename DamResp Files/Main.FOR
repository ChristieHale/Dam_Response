      program DamResp
      
      implicit none
 
      integer n1(10000), iBin, nf_fix, npts2, ic, i1, nf8   
      integer nFiles, iFile, j, k, MAXPTS, MAXFILES, npts, i, npts1, n8
      PARAMETER (MAXPTS=40000, MAXFILES=1200)
      real acc1(MAXPTS), acc2(MAXPTS), acc3(MAXPTS), acc4(MAXPTS)
      real rockTH(MAXPTS), FASRock(MAXPTS)
      real FASDAM1(MAXPTS), FASDAM2(MAXPTS), FASDAM3(MAXPTS), FASDAM4(MAXPTS)
      real FasSDOF1(4,MAXPTS), fasSDOF(MAXPTS), w
      real TF(4,MAXPTS), TF_ave(4,MAXPTS), TF_smooth(500,4,MAXPTS)
      real dt, time, dt1 , df, df_fix
      real acc(MAXPTS), rsp(MAXPTS), damping
      real pga, fasmax, freq1, freq2, freqmax, fas_half
      real pi
      parameter (pi=3.14159)
      character*80 runfile, lsrockfile, lsdamfile, rockfile, damfile, dummy

christie      
      integer n, f, g, m, gg, p, q, u
      parameter (n=22, m=23, p=24, q=25, u=26)
      real freq(MAXPTS), freq1all(MAXFILES,4)
christie

c      include 'rspMatch.h'     
 
      write (*,'( 2x,''Enter run file'')')
      read (*,'( a80)') runfile
      open (10,file=runfile,status='old')

      read (10,'( a80)') lsrockfile
      read (10,'( a80)') lsdamfile
      open (11,file=lsrockfile,status='old')
      open (12,file=lsdamfile,status='old')

c     Set fixed Freq step for averaging
      df_fix = 0.1
      nf_fix = int(20./df_fix) 

        do iBin=1,nf_fix
          n1(iBin) = 0
          do k=1,4
            TF_ave(k,iBin) = 0
          enddo
        enddo

      read (10,*) nFiles    
            
      do iFile=1,nFiles

c       Read rock TH
        read (11,'( a80)') rockfile
        write (*,'( a80)') rockfile
        open (20,file=rockfile,status='old')

        do j=1,6
          read (20,'( a1)') dummy
        enddo
        read (20,'( 5x,i6,5x,f7.3)') npts, dt     
                
        if ( npts .gt. 30000) npts=30000
        read (20,*) (rockTH(i),i=1,npts)
        close (20)

c       find PGA
        pga = 0.
        do i=1,npts
          if ( abs( rockTH(i)) .gt. pga ) pga = abs(rockTH(i))
        enddo

c       Read Dam TH
        read (12,'( a80)') damfile
        write (*,'( i5, a80)') iFIle, damfile
        open (30,file=damfile,status='old')
        do j=1,3
          read (30,'( a1)') dummy
        enddo
        do i=1,npts
          read (30,*,end=100) time, acc1(i), acc2(i), acc3(i), acc4(i)
        enddo
 100    close (30)
        npts1 = i-1
        dt1 = time /npts

        write (80,'( 3i7,f10.4,2a60)') ifile, npts, npts1, dt, rockfile, damfile
 
c       Compute FFT
        call calcFFT ( rockTH, nPts, dt, df, fasRock, npts1  )
        call calcFFT ( acc1, nPts, dt, df, fasDam1, npts1  )
        call calcFFT ( acc2, nPts, dt, df, fasDam2, npts1  )
        call calcFFT ( acc3, nPts, dt, df, fasDam3, npts1  )
        call calcFFT ( acc4, nPts, dt, df, fasDam4, npts1  )

christie
c put FFT in an output file so I can plot it
c        freq(1) = 0.0
c        do f = 2,npts1-1
c	    freq(f) = (f-1)*df
c	enddo
c        open (n, file='FFT.csv', access = 'append',
c     1       status ='new')
c        write(n,*) rockfile
c        write(n,*) 'frequency, fasRock, fasDam1, fasDam2, fasDam3, fasDam4'
c        do g=1, npts1-1
c          write(n,*) freq(g), fasRock(g), fasDam1(g), fasDam2(g), fasDam3(g), fasDam4(g)
c        enddo
c        close(n)
c        pause
christie

c       Compute transfer function
        do i=1,npts1/2
            TF(1,i) = fasDam1(i) / fasRock(i)
            TF(2,i) = fasDam2(i) / fasRock(i)
            TF(3,i) = fasDam3(i) / fasRock(i)
            TF(4,i) = fasDam4(i) / fasRock(i)
        enddo
        write (*,'( i5,f10.4)') ifile, tf(1,100)

christie
c put TF in an output file so I can plot it
c        open (m, file='TF.csv', access = 'append',
c     1       status ='new')
c        write(m,*) rockfile
c        write(m,*) 'frequency, TF1, TF2, TF3, TF4'
c        do gg=1, npts1/2
c          write(m,*) freq(gg), TF(1,gg), TF(2,gg), TF(3,gg), TF(4,gg)
c        enddo
c        close(m)
c        pause
christie

        n8 = int(10./df)
         fasmax = 0.
          do i=2,n8
            if (TF(2,i) .gt. fasmax ) then
              fasmax = TF(2,i)
            endif
          enddo
          write (*,'( 2x,''FAS max ='',f10.4)') fasmax        

        npts2 = 20./df

c       Average TF over fixed freq steps
c       initialize
        do iBin=1,nf_fix
          n1(iBin) = 0
          do k=1,4
            TF_smooth(iFile,k,iBin) = 0
          enddo
        enddo

        do i=2,npts1/2.
          freq1 = df*(i-1)
          iBin = int(freq1 / df_fix + 0.5)           
          if ( iBin .eq. 0 ) iBin = 1
          if ( iBin .gt. nf_fix) iBin = nf_fix
          do k=1,4          
            TF_smooth(iFile,k,iBin) = TF_smooth(iFile,k,iBin) + alog(TF(k,i))
            n1(iBin) = n1(iBin) + 1
          enddo

  200     continue
        enddo

christie
c put TF_smooth in an output file so I can plot it
c        open (p, file='TF_smooth.csv', access = 'append',
c     1       status ='new')
c        write(p,*) rockfile
c        write(p,*) 'iBin/10, TF_smooth1, TF_smooth2, TF_smooth3, TF_smooth4'
c        do iBin=1, nf_fix
c          write(p,*) iBin/10., TF_smooth(iFile,1,iBin), TF_smooth(iFile,2,iBin), TF_smooth(iFile,3,iBin), 
c     1               TF_smooth(iFile,4,iBin)
c        enddo
c        close(p)
c        pause
christie

c       Average
        do iBin=1,nf_fix
          if ( n1(iBin) .ge. 1 ) then
            do k=1,4
              TF_smooth(iFile,k,iBin) = TF_smooth(iFIle,k,iBin) / n1(iBin)
            enddo
          endif
        enddo

christie
c put TF_smooth in an output file so I can plot it
c        open (q, file='TF_smooth2.csv', access = 'append',
c     1       status ='new')
c        write(q,*) rockfile
c        write(q,*) 'iBin/10, TF_smooth1, TF_smooth2, TF_smooth3, TF_smooth4'
c        do iBin=1, nf_fix
c          write(q,*) iBin/10., TF_smooth(iFile,1,iBin), TF_smooth(iFile,2,iBin), TF_smooth(iFile,3,iBin), 
c     1               TF_smooth(iFile,4,iBin)
c        enddo
c        close(q)
c        pause
christie

c       Add to total
        do iBin=1,nf_fix
          if ( n1(iBin) .ge. 1 ) then
            do k=1,4
              TF_ave(k,iBin) = TF_ave(k,iBin) + TF_smooth(iFile,k,iBin)
            enddo
          endif
        enddo        

c       Find freq of peak
        do ic=1,4
          fasmax = 0.
          nf8 = int(10./df_fix)
          do i=2,nf8
            if (TF_smooth(iFIle,ic,i) .gt. fasmax ) then
              fasmax = TF_smooth(iFile,ic,i)
              freqMax = df_fix *i
            endif
          enddo
        
c         find first freq > half width
          fas_half = fasmax / 2.
          do i=2,nf_fix
            if ( TF_smooth(iFile,ic,i) .gt. fas_half) then
              freq1 = i*df_fix
              i1 = i
              goto 10
             endif
          enddo   

c         find first freq < half width
  10      do i=i1+1,nf_fix
            if ( TF_smooth(iFIle,ic,i) .lt. fas_half) then
              freq2 = i*df_fix
              goto 20
             endif
          enddo   
  20      continue     
          write (*,'( 5f10.4)') PGA, fasmax, freqMax, freq1, freq2
          write (60+ic,'( 5f10.4)') PGA, fasmax, freqMax, freq1, freq2

christie
c         keep freq1 to compute omega later          
c          freq1all(iFile,ic)=freqMax
christie

        enddo

 300   continue

      enddo

christie
c put freq1 values in an output file so I can compare
c        open (u, file='freq1all.csv', access = 'append',
c     1       status ='new')
c        write(u,*) 'iFile, ic, freq1all'
c        do iFile=1, nFiles
c          do ic=1, 4
c            write(u,*) iFile, ic, freq1all(iFile,ic) 
c          enddo
c        enddo  
c        close(u)
c        pause
christie

c     Write results
      do i=1,nf_fix
        write (40,'( f10.4,4f10.4)') df_fix*(i), (exp(TF_ave(k,i)/nFiles),k=1,4)
        do ic=1,4
          write (41+ic,'( f10.4,500f10.4)') df_fix*(i), (exp(TF_smooth(k,ic,i)),k=1,nFiles)
        enddo
      enddo

c     compute impulse response of SDOF
      dt = 0.002
      npts = npts1      
      do i=1,npts
        acc(i) = 0.
      enddo
      acc(100) = 1.
      w = 6.4 * 2. * pi
      do k=1,4
        damping = 0.1 +0.01*k
        call coeff (w, damping, dt)
        call brs (acc, w, damping, npts, rsp) 
c        call CalcRspTH ( acc, npts, dt, w, damping, rsp )
        if (k .eq. 1 ) write (33,'( 2f10.4)') (i*dt,rsp(i),i=1,npts)
c        if (k .eq. 2 ) write (34,'( 2f10.4)') (i*dt,rsp(i),i=1,npts)
c        if (k .eq. 3 ) write (35,'( 2f10.4)') (i*dt,rsp(i),i=1,npts)
c        if (k .eq. 4 ) write (36,'( 2f10.4)') (i*dt,rsp(i),i=1,npts)
        call calcFFT ( rsp, nPts, dt, df, fasSDOF, npts1  )
        do i=1,npts1
          fasSDOF1(k,i) = fasSDOF(i)
        enddo
      enddo
       do i=1,npts2
          write (41,'( f10.4,4e12.4)') df*(i-1), (fasSDOF1(k,i) ,k=1,4)
        enddo


      stop
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

c     calculate the a's
      a11 = t4*(beta*t2/t1+t3)
      a12 = t4*t2 / (w*t1)
      a21 = -t4*w*t2 / t1
      a22 = t4*(t3-beta*t2/t1)
c
c     calculate the b's
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

      
