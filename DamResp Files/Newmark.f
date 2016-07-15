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

      subroutine respTH (cuSDOF, cuRock, npts, dt, resp)
      
       implicit none
       include 'max_dims.H'
       
       integer npts, i
       real resp(MAXPTS), dt
       complex cuSDOF(MAXPTS), cuRock(MAXPTS), mult(MAXPTS)

       do i=1,npts
         mult(i) = cuSDOF(i)*cuRock(i)
       enddo
                     
c      calculate inverse FFT
       call cool ( 1., npts, mult )
       
       do i=1,npts
	 resp(i) = real(mult(i))
       enddo
       
      return
      end
      
c-----------------------------------------------------------------------

      subroutine respTH2 (RockTH, respTF, npts, resp)
      
       implicit none
       include 'max_dims.H'
       
       integer npts, i, j
       real sum, RockTH(MAXPTS), respTF(MAXPTS), resp(MAXPTS)

c      convolve rock time history with tf response time history
       do i=1,npts
         sum = 0.0
         do j=1,npts
           sum = sum + RockTH(i)*respTF(j)
         enddo
         resp(i) = sum
       enddo
       
      return
      end
