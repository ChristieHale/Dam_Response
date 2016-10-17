#This code creates the required input files for running QUAD4MU.
#Three files and one fortran executable are needed:
#1) GeoStudio .xml file (directly from GeoStudio project)
#2) specifics .xml file (created by user)
#3) gmlist .txt file (created by user)
#4) Quad_Pre1 fortran executable

require 'nokogiri'

class FormattingScript

  def perform
    open_damfiles
    read_geo
    read_specifics
    calc_prinput
    calc_center
    loop_gm
  end

#-------------------------------------------------------------------------------

  def open_damfiles
    @geo_file = Nokogiri::XML(File.open("Dam2_rev1.xml"))
    @specs_file = Nokogiri::XML(File.open("specifics.xml"))
  end

#-------------------------------------------------------------------------------

  def read_geo
    @Reg_mats = []
    @geo_file.xpath("//RegionUsesMaterial").each_with_index do |reg,j|
      reg_mat = reg.attribute('UsesID').value
      @Reg_mats[j] = reg_mat
    end
    unit_w = []
    @geo_file.xpath("//Material//UnitWeight").each_with_index do |uw,k|
      dens = uw.content
      dens = format_f10_output(dens)
      unit_w[k+1] = dens
    end
    el_tot = @geo_file.xpath("//Elements")
    nelm = el_tot.attribute('Len').value
    @NELM = format_i5_output(nelm)
    nodes_tot = @geo_file.xpath("//Nodes")
    ndpt = nodes_tot.attribute('Len').value
    @NDPT = format_i5_output(ndpt)
    @NP = []
    @N_els = []
    @El_integs = []
    @DENS = []
    @El_mats = []
    @TYPEs = []
    @geo_file.xpath("//EL").each_with_index do |el,i|
      el_num = el.attribute('ID').value
      el_num = format_i5_output(el_num)
      el_reg_num = el.attribute('Region').value.to_i
      el_mat = @Reg_mats[el_reg_num-1].to_i
      el_type = @Reg_mats[el_reg_num-1]
      el_dens = unit_w[el_mat]
      enodes = el.attribute('Node').value.split(',')
      if enodes.length == 3
        enodes[3] = enodes[2]
      end
      el_integ = el.attribute('Integ').value.to_i
      @NP[i] = enodes.map{ |enode| format_i5_output(enode) }
      @N_els[i] = el_num
      @El_integs[i] = el_integ
      @DENS[i] = el_dens
      @El_mats[i] = el_mat
      @TYPEs[i] = format_i5_output(el_type)
    end
    @N_nodes = []
    @XORDS = []
    @YORDS = []
    @geo_file.xpath("//N").each_with_index do |node,j|
      n_node = node.attribute('ID').value
      n_node = format_i5_output(n_node)
      xord = node.attribute('X').value
      xord = format_f10_output(xord)
      yord = node.attribute('Y').value
      yord = format_f10_output(yord)
      @N_nodes[j] = n_node
      @XORDS[j] = xord
      @YORDS[j] = yord
    end
    #find minimum yord value, assume this is base level
    @base = @YORDS.map(&:to_f).min
    #collect node numbers for all nodes at base level for boundary condition 4
    @BC4s = []
    @YORDS.each_with_index do |yord,i|
      if yord.to_i == @base
        @BC4s << i+1
      end
    end
  end

#-------------------------------------------------------------------------------

  def read_specifics
    line1 = @specs_file.xpath("//Line1")
    @TITLE = line1.attribute('TITLE')
    line2 = @specs_file.xpath("//Line2")
    @Comment2 = line2.attribute('Comment')
    line3 = @specs_file.xpath("//Line3")
    @UNITS = line3.attribute('UNITS')
    line4 = @specs_file.xpath("//Line4")
    @Comment4 = line4.attribute('Comment')
    line5 = @specs_file.xpath("//Line5")
    drf = line5.attribute('DRF').value
    @DRF = format_f10_output(drf)
    prm = line5.attribute('PRM').value
    @PRM = format_f10_output(prm)
    rockvp = line5.attribute('ROCKVP').value
    @ROCKVP = format_f10_output(rockvp)
    rockvs = line5.attribute('ROCKVS').value
    @ROCKVS = format_f10_output(rockvs)
    rockrho = line5.attribute('ROCKRHO').value
    @ROCKRHO = format_f10_output(rockrho)
    line6 = @specs_file.xpath("//Line6")
    @Comment6 = line6.attribute('Comment')
    line7 = @specs_file.xpath("//Line7")
    nslp = line7.attribute('NSLP').value
    @NSLP = format_i5_output(nslp)
    line8 = @specs_file.xpath("//Line8")
    @Comment8 = line8.attribute('Comment')
    line9 = @specs_file.xpath("//Line9")
    n1eq = line9.attribute('N1EQ').value
    @N1EQ = format_i5_output(n1eq)
    n2eq = line9.attribute('N2EQ').value
    @N2EQ = format_i5_output(n2eq)
    numb = line9.attribute('NUMB').value
    @NUMB = format_i5_output(numb)
    kv = line9.attribute('KV').value
    @KV = format_i5_output(kv)
    ksav = line9.attribute('KSAV').value
    @KSAV = format_i5_output(ksav)
    line10 = @specs_file.xpath("//Line10")
    @Comment10 = line10.attribute('Comment')
    line11 = @specs_file.xpath("//Line11")
    eqmul1 = line11.attribute('EQMUL1').value
    @EQMUL1 = format_f10_output(eqmul1)
    eqmul2 = line11.attribute('EQMUL2').value
    @EQMUL2 = format_f10_output(eqmul2)
    ugmax1 = line11.attribute('UGMAX1').value
    @UGMAX1 = format_f10_output(ugmax1)
    ugmax2 = line11.attribute('UGMAX2').value
    @UGMAX2 = format_f10_output(ugmax2)
    hdrx = line11.attribute('HDRX').value
    @HDRX = format_i5_output(hdrx)
    hdry = line11.attribute('HDRY').value
    @HDRY = format_i5_output(hdry)
    nplx = line11.attribute('NPLX').value
    @NPLX = format_i5_output(nplx)
    nply = line11.attribute('NPLY').value
    @NPLY = format_i5_output(nply)
    line12 = @specs_file.xpath("//Line12")
    @Comment12 = line12.attribute('Comment')
    line14 = @specs_file.xpath("//Line14")
    @EQINPFMT = line14.attribute('EQINPFMT')
    line17 = @specs_file.xpath("//Line17")
    @Comment17 = line17.attribute('Comment')
    line18 = @specs_file.xpath("//Line18")
    sout = line18.attribute('SOUT').value
    @SOUT = format_i5_output(sout)
    aout = line18.attribute('AOUT').value
    @AOUT = format_i5_output(aout)
    kout = line18.attribute('KOUT').value
    @KOUT = format_i5_output(kout)
    line23 = @specs_file.xpath("//Line23")
    @Comment23 = line23.attribute('Comment')
    line24 = @specs_file.xpath("//Line24")
    @AHISTFMT = line24.attribute('AHISTFMT')
    line26 = @specs_file.xpath("//Line26")
    @ASUFFIX = line26.attribute('ASUFFIX')
    line27 = @specs_file.xpath("//Line27")
    @Comment27 = line27.attribute('Comment')
    line28 = @specs_file.xpath("//Line28")
    @KHISTFMT = line28.attribute('KHISTFMT')
    line30 = @specs_file.xpath("//Line30")
    @KSUFFIX = line30.attribute('KSUFFIX')
    line33 = @specs_file.xpath("//Line33")
    @Comment33 = line33.attribute('Comment')
    line35 = @specs_file.xpath("//Line35")
    @Comment35 = line35.attribute('Comment')
    @NOSEGS = []
    @NSEGS = []
    line36s = @specs_file.xpath("//Line36").each_with_index do |line36,i|
      nosegs = line36.attribute('NOSEG').value.split(',')
      nosegs.each_with_index do |noseg,j|
        @NOSEGS[i] ||= []
        @NOSEGS[i][j] = format_i5_output(noseg)
      end
      nseg = nosegs.length.to_s
      @NSEGS[i] = format_i5_output(nseg)
    end
    line37 = @specs_file.xpath("//Line37")
    @Comment37 = line37.attribute('Comment')
    line39 = @specs_file.xpath("//Line39")
    @Comment39 = line39.attribute('Comment')
    water = @specs_file.xpath("//Water")
    waterx = water.attribute('Xwat').value.split(',')
    @Water_x = waterx.map!(&:to_f)
    watery = water.attribute('Ywat').value.split(',')
    @Water_y = watery.map!(&:to_f)
    @POWs = []
    @PODs = []
    @GMXs = []
    @Gs = []
    @XLs = []
    @specs_file.xpath("//Material").each do |mat|
      mat_num = mat.attribute('ID').value.to_i
      pow = mat.attribute('POW').value
      @POWs[mat_num] = format_f10_output(pow)
      pod = mat.attribute('POD').value
      @PODs[mat_num] = format_f10_output(pod)
      gmx = mat.attribute('GMX').value
      @GMXs[mat_num] = format_f10_output(gmx)
      g = mat.attribute('G').value
      @Gs[mat_num] = format_f10_output(g)
      xl = mat.attribute('XL').value
      @XLs[mat_num] = format_f10_output(xl)
    end
    line41 = @specs_file.xpath("//Line41")
    @Comment41 = line41.attribute('Comment')
    line42 = @specs_file.xpath("//Line42")
    @OUTs = line42.attribute('OUT').value.split(',')
    @OUTs.map!(&:to_i)
  end

#-------------------------------------------------------------------------------

  def calc_prinput
    file=File.open("gmlist.txt","r")
    lines = file.readlines.size
    temp_file = File.new('run_fortran.txt', 'w')
    temp_file << "gmlist.txt"
    temp_file << "\n"
    temp_file << lines
    temp_file << "\n"
    temp_file << 0
    temp_file << "\n"
    temp_file.close
    # calling fortran script to calculate prinput
    `./Quad_Pre1`
    File.delete "run_fortran.txt"
  end

#-------------------------------------------------------------------------------

  def calc_center
#   extend the first and last point of the water line up by 100 feet above the dam crest
    above = @YORDS.map(&:to_f).max + 100
    firstx = @Water_x[0]
    lastx = @Water_x[-1]
    @Water_x.insert(0,firstx)
    @Water_x.insert(-1,lastx)
    @Water_y.insert(0,above)
    @Water_y.insert(-1,above)
#   repeat the new first point at the end of the array to create a polygon
    firstx = @Water_x[0]
    firsty = @Water_y[0]
    @Water_x.insert(-1,firstx)
    @Water_y.insert(-1,firsty)
    nwater = (@Water_x.length)-1
#   get the x and y coordinates of the line that defines the slip surface
    @Slip_x = []
    @Slip_y = []
    nslip = []
    @NOSEGS.each_with_index do |ss,i|
      @NOSEGS[i].each_with_index do |ss_nodes,j|
        @Slip_x[i] ||= []
        @Slip_y[i] ||= []
        @Slip_x[i][j] = @XORDS[ss_nodes.to_i-1].to_f
        @Slip_y[i][j] = @YORDS[ss_nodes.to_i-1].to_f
      end
#     extend the first and last point of the slip surface
      firstx = @Slip_x[i][0]
      lasty = @Slip_y[i][-1]
      lastx_extend = @Slip_x[i][-1] + 100
      @Slip_x[i].insert(0,firstx)
      @Slip_x[i].insert(-1,lastx_extend)
      @Slip_y[i].insert(0,above)
      @Slip_y[i].insert(-1,lasty)
#     repeat the new first point at the end of the array to create a polygon
      firstx = @Slip_x[i][0]
      firsty = @Slip_y[i][0]
      @Slip_x[i].insert(-1,firstx)
      @Slip_y[i].insert(-1,firsty)
      nslip[i] = (@Slip_x[i].length)-1
    end
#   calculate the center of each element
    cent_x = []
    cent_y = []
    for j in 0..(@NELM.to_i-1)
      cent_x[j] = 0.0
      cent_y[j] = 0.0
      count = 0
      @NP[j].each do |enode|
        count = count + 1
        if count <= @El_integs[j]
          i_enode = enode.to_i-1
          cent_x[j] = cent_x[j] + @XORDS[i_enode].to_f/@El_integs[j]
          cent_y[j] = cent_y[j] + @YORDS[i_enode].to_f/@El_integs[j]
        end
      end
    end
#   determine if element is dry
    d_flag = []
    @DRYs = []
    for j in 0..(@NELM.to_i-1)
      d_flag[j] = inside_outside(nwater, @Water_x, @Water_y, cent_x[j], cent_y[j])
      if d_flag[j] == 1
        @DRYs << @N_els[j].to_i
      end
    end
#   determine if element is inside slip surface
    @NOSEGS.each_with_index do |ss,i|
    s_flag = []
    @forESEGS = []
    @ELSEGS = []
    @ESEGS = []
      for j in 0..(@NELM.to_i-1)
        @forESEGS[i] ||= []
        @ELSEGS[i] ||= []
        s_flag[j] = inside_outside(nslip[i], @Slip_x[i], @Slip_y[i], cent_x[j], cent_y[j])
        if s_flag[j] == 1
          @forESEGS[i] << @N_els[j].to_i
          @ELSEGS[i] << format_i5_output(@N_els[j])
        end
      end
      eseg = @forESEGS[i].length.to_s
      @ESEGS[i] = format_i5_output(eseg)
    end
  end

#-------------------------------------------------------------------------------

  def loop_gm
    folder_in = "in"
    if Dir.exist?(folder_in)
      raise 'folder already exists'
    end
    Dir.mkdir(folder_in)
    folder_sc = "sc"
    if Dir.exist?(folder_sc)
      raise 'folder already exists'
    end
    Dir.mkdir(folder_sc)
    fname_bat = "Dam2_rev1_560gm.bat"
    if File.exist?(fname_bat)
      raise '.bat file already exists'
    end
    bat_file = File.new(fname_bat, 'w')
    if File.exist?('surfaces.txt')
      raise 'surfaces file already exists'
    end
    surf_file = File.new('surfaces.txt', 'w')
    if File.exist?('damresponse.txt')
      raise 'damresponse file already exists'
    end
    damr_file = File.new('damresponse.txt', 'w')
    count = 0
    File.open('gmlist.txt', 'r').each_line do |gm_name|
      count = count + 1
      gm_name.gsub!(/\r\n?|\n/, "");
      #modifying path of ground motion file for pc compatibility
      @EARTHQH = gm_name.gsub("/","\\")
      linenumber = 4
      @gm_file = File.open(gm_name).each_with_index do |line,ind|
        if  ind+1 == linenumber
          line_4 = line.split(/[\s,]+/)
          npts = line_4[1]
          @npts = format_i5_output(npts)
          dteq = line_4[3]
          @dteq = format_f10_output(dteq)
        end
      end
      @pr_file = File.open('prinput.txt').each_with_index do |line,k|
        if  k == count
          pr_data = line.split
          prinput = pr_data[4]
          @prinput = format_f10_output(prinput)
        end
      end
      countf = "%.4d" % count
      f_in = "B#{countf}.in"
      f_out = "B#{countf}.out"
      f_qsc = "B#{countf}.QSC"
      f_acc = "B#{countf}.acc"
      @AFILEOUT = "B#{countf}"
      @KFILEOUT = "B#{countf}"
      fname_in = File.join(folder_in,f_in)
      fname_out = File.join()
      if File.exist?(fname_in)
        raise '.in file already exists'
      end
      @output_file = File.new(fname_in, 'w')
      write_infile
      close_file
      f_sc = "Quad4MU-B#{countf}.sc"
      fname_sc = File.join(folder_sc,f_sc)
      if File.exist?(fname_sc)
        raise '.sc file already exists'
      end
      sc_file = File.new(fname_sc,'w')
      #modifying path of input file for pc compatibility
      fname_in_pc = fname_in.gsub("/","\\")
      sc_file << "#{fname_in_pc}"
      sc_file << "\r\n"
      sc_file << "Mat.o8"
      sc_file << "\r\n"
      sc_file << "out\\"
      sc_file << "\r\n"
      sc_file << "#{f_out}"
      sc_file << "\r\n"
      sc_file << "q"
      sc_file << "\r\n
      \n"
      sc_file.close
      bat_file << "Quad4MU.exe < sc\\#{f_sc}"
      bat_file << "\r\n"
      surf_file << "../out/#{f_qsc}"
      surf_file << "\n"
      damr_file << "../out/#{f_acc}"
      damr_file << "\n"
    end
    bat_file.close
    surf_file.close
    damr_file.close
  end

#-------------------------------------------------------------------------------

  def write_infile
    @output_file << @TITLE
    @output_file << "\n"
    @output_file << @Comment2
    @output_file << "\n"
    @output_file << @UNITS
    @output_file << "\n"
    @output_file << @Comment4
    @output_file << "\n"
    @output_file << @DRF
    @output_file << @PRM
    @output_file << @ROCKVP
    @output_file << @ROCKVS
    @output_file << @ROCKRHO
    @output_file << "\n"
    @output_file << @Comment6
    @output_file << "\n"
    @output_file << @NELM
    @output_file << @NDPT
    @output_file << @NSLP
    @output_file << "\n"
    @output_file << @Comment8
    @output_file << "\n"
    @output_file << @npts
    @output_file << @npts
    @output_file << @N1EQ
    @output_file << @N2EQ
    @output_file << @npts
    @output_file << @NUMB
    @output_file << @KV
    @output_file << @KSAV
    @output_file << "\n"
    @output_file << @Comment10
    @output_file << "\n"
    @output_file << @dteq
    @output_file << @EQMUL1
    @output_file << @EQMUL2
    @output_file << @UGMAX1
    @output_file << @UGMAX2
    @output_file << @HDRX
    @output_file << @HDRY
    @output_file << @NPLX
    @output_file << @NPLY
    @output_file << @prinput
    @output_file << "\n"
    @output_file << @Comment12
    @output_file << "\n"
    @output_file << @EARTHQH
    @output_file << "\n"
    @output_file << @EQINPFMT
    @output_file << "\n"
    @output_file << @Comment17
    @output_file << "\n"
    @output_file << @SOUT
    @output_file << @AOUT
    @output_file << @KOUT
    @output_file << "\n"
    @output_file << @Comment23
    @output_file << "\n"
    @output_file << @AHISTFMT
    @output_file << "\n"
    @output_file << @AFILEOUT
    @output_file << "\n"
    @output_file << @ASUFFIX
    @output_file << "\n"
    @output_file << @Comment27
    @output_file << "\n"
    @output_file << @KHISTFMT
    @output_file << "\n"
    @output_file << @KFILEOUT
    @output_file << "\n"
    @output_file << @KSUFFIX
    @output_file << "\n"
    for j in 0..(@NSLP.to_i-1)
      @output_file << @Comment33
      @output_file << "\n"
      @output_file << @NSEGS[j]
      @output_file << @ESEGS[j]
      @output_file << "\n"
      @output_file << @Comment35
      @output_file << "\n"
      @NOSEGS[j].each_with_index do |noseg,i|
        @output_file << noseg
        if (i+1) % 15 == 0
          @output_file << "\n"
        end
      end
      @output_file << "\n"
      @output_file << @Comment37
      @output_file << "\n"
      @ELSEGS[j].each_with_index do |elseg,i|
        @output_file << elseg
        if (i+1) % 15 == 0
          @output_file << "\n"
        end
      end
      @output_file << "\n"
    end
    @output_file << @Comment39
    @output_file << "\n"
    for j in 0..(@NELM.to_i-1)
      @output_file << @N_els[j]
      @NP[j].each do |enode|
        @output_file << enode
      end
      @output_file << @TYPEs[j]
      @output_file << @DENS[j]
      for k in 1..2
        if @El_mats[j] == k
          if @DRYs.include?(j+1)
            @output_file << @PODs[k]
          else
            @output_file << @POWs[k]
          end
          @output_file << @GMXs[k]
          @output_file << @Gs[k]
          @output_file << @XLs[k]
          @output_file << "\n"
        end
      end
    end
    @output_file << @Comment41
    @output_file << "\n"
    for j in 0..(@NDPT.to_i-1)
      @output_file << @N_nodes[j]
      @output_file << @XORDS[j]
      @output_file << @YORDS[j]
      if @BC4s.include?(j+1)
        @output_file << "    4"
      else
        @output_file << "     "
      end
      if @OUTs.include?(j+1)
        @output_file << "    1"
      end
      @output_file << "\n"
    end
  end

#-------------------------------------------------------------------------------

  def format_i5_output(number)
    if number.length > 5
      raise 'Too many digits'
    end
    number.rjust(5)
  end

#-------------------------------------------------------------------------------

  def format_f10_output(number)
    if number.length > 9
      number = truncate(number, 9)
    end
    if number.length > 9
      raise 'Too many digits'
    end
    number.rjust(10)
  end

#-------------------------------------------------------------------------------

  def truncate(string, max)
    string.length > max ? "#{string[0...max]}" : string
  end

#-------------------------------------------------------------------------------

  def close_file
    @output_file.close
  end

#-------------------------------------------------------------------------------

  def inside_outside(npoly, xpoly, ypoly, xpoint, ypoint)
    pi = 4 * Math.atan(1)
    twopi = 2 * pi
    sumtheta = 0.0

    for i in 0..(npoly-1)
#     compute azimuth to ends of segments
      dy1 = ypoly[i] - ypoint
      dy2 = ypoly[i+1] - ypoint
      dx1 = xpoly[i] - xpoint
      dx2 = xpoly[i+1] - xpoint
      theta1 = Math.atan2(dy1,dx1)
      theta2 = Math.atan2(dy2,dx2)
      dtheta = theta2 - theta1
#     check if theta range is greater than pi (wrap around)
      if dtheta > pi
        dtheta = dtheta - twopi
      elsif dtheta < -pi
        dtheta = dtheta + twopi
      end
#     compute sum of azimuth ranges
      sumtheta = sumtheta + dtheta
    end

#   determine if point is inside polygon
    test1 = (sumtheta.abs - twopi).abs
    tol = 0.01
    if test1 < tol
      flag = 1
    else
      flag = 0
    end
  end

#-------------------------------------------------------------------------------

end

script = FormattingScript.new
script.perform
