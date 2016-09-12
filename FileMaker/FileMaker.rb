require 'nokogiri'

class FormattingScript

  def perform
    open_damfiles
    read_geo
#    read_specifics
#    write_file
#    close_files
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
    @geo_file.xpath("//EL").each do |el,i|
      n_el[i] = el.attribute('ID').value
      @N_el[i] = format_i5_output(n)
      nps[i] = el.attribute('Node').value.split(',')
      if nps.length == 3
        nps[3] = nps[2]
      end
      nps.each do |np|
        @NP = format_i5_output(np)
      end
    end
    @geo_file.xpath("//N").each do |node|
      n_node = node.attribute('ID').value
      @N_node = format_i5_output(n_node)
      xcord = node.attribute('X').value
      @XCORD = format_f10_output(xcord)
      ycord = node.attribute('Y').value
      @YCORD = format_f10_output(ycord)
    end
  end

#  def read_specifics
#    line1 = @spec_file.xpath("//Line1")
#    title = line1.attribute('TITLE')
#    puts title
#  end

#  def write_file
#    @output_file << @TITLE
#  end

#  def calc_prinput
    # calling fortran script
    # input_string = "..\\in\\#{filename}.in\n Mat.o8\n ..\\out\\\n #{filename}.out\n q"
    # result = `quad4MU.exe < #{input_string}`
    # puts "system said: #{result}"
#  end


#  def loop_gm
#    count = 0
#    File.open('gmlist.txt', 'r').each_line do |gm_name|
#      count = count + 1
#      gm_name.gsub!("\n",'')
#      linenumber = 4
#      @gm_file = File.open(gm_name).each_with_index do |line,ind|
#        if  ind+1 == linenumber
#          line_4 = line.split(/[\s,]+/)
#          npts = line_4[1]
#          @npts = format_i5_output(npts)
#          dteq = line_4[3]
#          @dteq = format_f10_output(dteq)
#        end
#      end
#      countf = "%.4d" % count
#      fname = "A#{countf}"
#      if File.exist?(fname)
#        raise 'Output file already exists'
#      end
#      @output_file = File.new(fname, 'w')
#      dam_info
#    end
#  end




  def format_i5_output(number)
    if number.length > 5
      raise 'Too many digits'
    end
    number.rjust(5)
  end


  def format_f10_output(number)
    number.rjust(10)
  end

#  def close_files
#    @output_file.close
#  end

end

script = FormattingScript.new
script.perform
