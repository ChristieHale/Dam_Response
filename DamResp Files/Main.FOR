      program DamResp
      
      implicit none
 
      integer n1(10000), iBin, nf_fix, npts2, ic, i1, nf8     
      integer nFiles, iFile, j, k, MAXPTS, npts, i, npts1, n8
      PARAMETER (MAXPTS=40000)
      real acc1(MAXPTS), acc2(MAXPTS), acc3(MAXPTS), acc4(MAXPTS)
      real rockTH(MAXPTS), FASRock(MAXPTS)
      real FASDAM1(MAXPTS), FASDAM2(MAXPTS), FASDAM3(MAXPTS), FASDAM4(MAXPTS)
      real FasSDOF1(4,MAXPTS), fasSDOF(MAXPTS), w
      real TF(4,MAXPTS),  TF_ave(4,MAXPTS), TF_smooth(500,4,MAXPTS)
      real dt, time, dt1 , df, df_fix
      real acc(MAXPTS), rsp(MAXPTS), damping
      real pga, fasmax, freq1, freq2, freqmax, fas_half
      character*80 file1, dummy, file2, file3

c      include 'rspMatch.h'     
 
      write (*,'( 2x,''Enter run file'')')
      read (*,'( a80)') file1
      open (10,file=file1,status='old')

      read (10,'( a80)') file2
      read (10,'( a80)') file3
      open (11,file=file2,status='old')
      open (12,file=file3,status='old')

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
        read (11,'( a80)') file1
        write (*,'( a80)') file1
        open (20,file=file1,status='old')

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
        read (12,'( a80)') file2
        write (*,'( i5, a80)') iFIle, file2
        open (30,file=file2,status='old')
        do j=1,3
          read (30,'( a1)') dummy
        enddo
        npts1 = MAXPTS
        do i=1,npts
          read (30,*,end=100) time, acc1(i), acc2(i), acc3(i), acc4(i)
        enddo
 100    close (30)
        npts1 = i-1
        dt1 = time /npts

c        write (*,'( 3i7,f10.4,2a60)') ifile, npts, npts1, dt, file1, file2

        write (80,'( 3i7,f10.4,2a60)') ifile, npts, npts1, dt, file1, file2
 
c       Compute FFT
        call calcFFT ( rockTH, nPts, dt, df, fasROck, npts1  )
        call calcFFT ( acc1, nPts, dt, df, fasDam1, npts1  )
        call calcFFT ( acc2, nPts, dt, df, fasDam2, npts1  )
        call calcFFT ( acc3, nPts, dt, df, fasDam3, npts1  )
        call calcFFT ( acc4, nPts, dt, df, fasDam4, npts1  )

c       Compute transfer function
        do i=1,npts1/2
            TF(1,i) = fasDam1(i) / fasRock(i)
            TF(2,i) = fasDam2(i) / fasRock(i)
            TF(3,i) = fasDam3(i) / fasRock(i)
            TF(4,i) = fasDam4(i) / fasRock(i)
        enddo
        write (*,'( i5,f10.4)') ifile, tf(1,100)

        n8 = int(10./df)
         fasmax = 0.
          do i=2,n8
            if (TF(2,i) .gt. fasmax ) then
              fasmax = TF(2,i)
            endif
          enddo
          write (*,'( 2x,''FAS max ='',f10.4)') fasmax        

        npts2 = 20./df
c       do i=1,npts2
c         write (50+ifile,'( f10.4,4f10.4)') df*(i-1), (TF(k,i),k=1,4)
c       enddo

c       Average TF over fixed freq steps
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

c       Average
        do iBin=1,nf_fix
          if ( n1(iBin) .ge. 1 ) then
            do k=1,4
              TF_smooth(iFile,k,iBin) = TF_smooth(iFIle,k,iBin) / n1(iBin)
            enddo
          endif
        enddo

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
        enddo

 300   continue

      enddo

c     Write results
      do i=1,nf_fix
        write (40,'( f10.4,4f10.4)') df_fix*(i), (exp(TF_ave(k,i)/nFiles),k=1,4)
        do ic=1,4
          write (41+ic,'( f10.4,500f10.4)') df_fix*(i), (exp(TF_smooth(k,ic,i)),k=1,nFiles)
        enddo
      enddo

c     compute impulse response of SDOF
c      npts = npts1
c      do i=1,npts
c        acc(i) = 0.
c      enddo
c      acc(100) = 1.
c      w = 6.4 * 6.28
c      do k=1,4
c        damping = 0.1 +0.01*k
c        call CalcRspTH ( acc, npts, dt, w, damping, rsp )
c        if (k .eq. 1 ) write (33,'( 2f10.4)') (i*dt,rsp(i),i=1,npts)
c        call calcFFT ( rsp, nPts, dt, df, fasSDOF, npts1  )
c        do i=1,npts1
c          fasSDOF1(k,i) = fasSDOF(i)
c        enddo
c      enddo
c       do i=1,npts2
c          write (41,'( f10.4,4e12.4)') df*(i-1), (fasSDOF1(k,i) ,k=1,4)
c        enddo


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

c ----------------------------------------------------------------


      
