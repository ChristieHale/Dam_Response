c-----------------------------------------------------------------------

      subroutine Newmark (dam1, nPts, dt, Ky, vel, dis) 
      
       implicit none
       include 'max_dims.H'

       integer nPts, i, j, k       
       real dam1(MAXPTS), dt, Ky, vel(MAXPTS), dis(MAXPTS)  
       real grav     
       
       grav = 9.80665

c      initialize velocity values to zero
       do k=1,npts
         vel(k) = 0.0
       enddo

c      numerical integration over acceleration to calculate velocity       
       i = 1
       do while (i .lt. nPts)       
         if (dam1(i) .ge. Ky) then         
           do j=i, nPts           
             if (vel(j-1) .ge. 0.0) then
               vel(j) = vel(j-1) + ((((dam1(j)-Ky) + (dam1(j+1)-Ky))/2.)*dt)               
             else
               i = j
               goto 10  
             endif
           enddo      
         elseif (dam1(i) .lt. Ky) then
           i = i + 1
         endif 
   10    continue
       enddo

c      force negative values to be zero?
       do k=1,npts
         if (vel(k) .lt. 0.0) then
           vel(k) = 0.0
         endif
       enddo        

c      convert velocity from g to cm/s
       do k=1,npts
         vel(k) = vel(k)*grav*100.0
       enddo

c      initialize displacement values to zero
       do k=1,npts
         dis(k) = 0.0
       enddo

c      numerical integration over velocity to calculate displacement
       do i=2,nPts                       
         dis(i) = dis(i-1) + (((vel(i) + vel(i+1))/2.)*dt)               
       enddo
       
      return
      end
      
c-----------------------------------------------------------------------

      subroutine respTH (cuRock, cuSDOF, npts1, npts, m, resp)
      
       implicit none
       include 'max_dims.H'
       
       integer npts1, npts, i, m
       real resp(MAXPTS)
       complex cuSDOF(MAXPTS), cuRock(MAXPTS), mult(MAXPTS)

       do i=1,npts1
         mult(i) = cuSDOF(i)*cuRock(i)
       enddo
          
c      calculate inverse FFT
       call cool ( 1., m, mult )
      
       do i=1,npts
	 resp(i) = real(mult(i))/npts1
       enddo
       
      return
      end
      
c-----------------------------------------------------------------------

      subroutine respTH2 (RockTH, respTH, npts, resp)
      
       implicit none
       include 'max_dims.H'
       
       integer npts, i, k, c, j
       real RockTH(MAXPTS), respTH(MAXPTS), Conv(MAXPTS), resp(MAXPTS)

c       convolve rock time history with tf response time history       
        do i=1,2*npts          
c         point in the first half 
          if (i .le. npts) then                        
            Conv(i) = 0.0
            do k=1,i
              c = k+npts-i
              Conv(i) = Conv(i) + RockTH(k)*respTH(c)              
            enddo
c         point in the second half
          elseif (i .gt. npts) then
            Conv(i) = 0.0
            do k=i-npts+1,npts
              c = npts-i+k
              Conv(i) = Conv(i) + RockTH(k)*respTH(c)
            enddo
          endif
        enddo

c       keep second half
        do j=1, npts  
          resp(j) = Conv(j+npts)
        enddo
     
      return
      end
