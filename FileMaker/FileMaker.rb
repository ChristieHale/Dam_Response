#This code creates the required input files for running QUAD4MU.
#Three files are needed:
#1) GeoStudio .xml file (directly from GeoStudio project)
#2) specifics .xml file (created by user)
#3) gmlist .txt file (created by user)

require 'nokogiri'

class FormattingScript

  def perform
    open_damfiles
    read_geo
    read_specifics
    calc_prinput
    loop_gm
  end

  def open_damfiles
    @geo_file = Nokogiri::XML(File.open("Dam1_50ft.xml"))
    @specs_file = Nokogiri::XML(File.open("specifics.xml"))
  end

  def read_geo
    el_tot = @geo_file.xpath("//Elements")
    nelm = el_tot.attribute('Len').value
    @NELM = format_i5_output(nelm)
    nodes_tot = @geo_file.xpath("//Nodes")
    ndpt = nodes_tot.attribute('Len').value
    @NDPT = format_i5_output(ndpt)
    @N_els = []
    @NP = []
    @geo_file.xpath("//EL").each_with_index do |el,i|
      el_num = el.attribute('ID').value
      el_num = format_i5_output(el_num)
      enodes = el.attribute('Node').value.split(',')
      if enodes.length == 3
        enodes[3] = enodes[2]
      end
      @NP[i] = enodes.map{ |enode| format_i5_output(enode) }
      @N_els[i] = el_num
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
    @base = @YORDS.min
    #collect node numbers for all nodes at base level for boundary condition 4
    #excluding first and last node in array, which are boundary condition 2
    @BC4s = []
    @YORDS.each_with_index do |yord,i|
      if yord == @base
        @BC4s[i+1] = i+1
      end
    end
    @BC4s.compact!
    @BC2s = [@BC4s[0],@BC4s[-1]]
    @BC4s = @BC4s.drop(1)
    @BC4s = @BC4s[0...-1]
  end

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
    @NSEGS = []
    @ESEGS = []
    line34s = @specs_file.xpath("//Line34").each_with_index do |line34,i|
      nseg = line34.attribute('NSEG').value
      nseg = format_i5_output(nseg)
      @NSEGS[i] = nseg
      eseg = line34.attribute('ESEG').value
      eseg = format_i5_output(eseg)
      @ESEGS[i] = eseg
    end
    line35 = @specs_file.xpath("//Line35")
    @Comment35 = line35.attribute('Comment')
    @NOSEGS = []
    line36s = @specs_file.xpath("//Line36").each_with_index do |line36,i|
      nosegs = line36.attribute('NOSEG').value.split(',')
      @NOSEGS[i] = nosegs.map{ |noseg| format_i5_output(noseg) }
    end
    line37 = @specs_file.xpath("//Line37")
    @Comment37 = line37.attribute('Comment')
    @ELSEGS = []
    line38s = @specs_file.xpath("//Line38").each_with_index do |line38,i|
      elsegs = line38.attribute('ELSEG').value.split(',')
      @ELSEGS[i] = elsegs.map{ |elseg| format_i5_output(elseg) }
    end
    line39 = @specs_file.xpath("//Line39")
    @Comment39 = line39.attribute('Comment')
    line40 = @specs_file.xpath("//Line40")
    type = line40.attribute('TYPE').value
    @TYPE = format_i5_output(type)
    dens = line40.attribute('DENS').value
    @DENS = format_f10_output(dens)
    pow = line40.attribute('POW').value
    @POW = format_f10_output(pow)
    pod = line40.attribute('POD').value
    @POD = format_f10_output(pod)
    @DRYs = line40.attribute('DRY').value.split(',')
    @DRYs.map!(&:to_i)
    gmx = line40.attribute('GMX').value
    @GMX = format_f10_output(gmx)
    g = line40.attribute('G').value
    @G = format_f10_output(g)
    xl = line40.attribute('XL').value
    @XL = format_f10_output(xl)
    line41 = @specs_file.xpath("//Line41")
    @Comment41 = line41.attribute('Comment')
    line42 = @specs_file.xpath("//Line42")
    @OUTs = line42.attribute('OUT').value.split(',')
    @OUTs.map!(&:to_i)
  end

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

  def loop_gm
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
      fname = "A#{countf}.in"
      @AFILEOUT = "A#{countf}"
      @KFILEOUT = "A#{countf}"
      if File.exist?(fname)
        raise 'Output file already exists'
      end
      @output_file = File.new(fname, 'w')
      write_file
      close_file
    end
  end

  def write_file
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
      @output_file << @TYPE
      @output_file << @DENS
      if @DRYs.include?(j+1)
        @output_file << @POD
      else
        @output_file << @POW
      end
      @output_file << @GMX
      @output_file << @G
      @output_file << @XL
      @output_file << "\n"
    end
    @output_file << @Comment41
    @output_file << "\n"
    for j in 0..(@NDPT.to_i-1)
      @output_file << @N_nodes[j]
      @output_file << @XORDS[j]
      @output_file << @YORDS[j]
      if @BC4s.include?(j+1)
        @output_file << "    4"
      elsif @BC2s.include?(j+1)
        @output_file << "    2"
      else
        @output_file << "     "
      end
      if @OUTs.include?(j+1)
        @output_file << "    1"
      end
      @output_file << "\n"
    end
  end

#  def calc_prinput
    # calling fortran script
    # input_string = "..\\in\\#{filename}.in\n Mat.o8\n ..\\out\\\n #{filename}.out\n q"
    # result = `quad4MU.exe < #{input_string}`
    # puts "system said: #{result}"
#  end


  def format_i5_output(number)
    if number.length > 5
      raise 'Too many digits'
    end
    number.rjust(5)
  end


  def format_f10_output(number)
    if number.length > 10
      raise 'Too many digits'
    end
    number.rjust(10)
  end

  def close_file
    @output_file.close
  end

end

script = FormattingScript.new
script.perform
