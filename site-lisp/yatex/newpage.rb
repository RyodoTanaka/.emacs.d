#!/usr/bin/env ruby
# THIS is very very tentative.  Insufficient examination of function.
# Create new HTML file referring other HTML file in the same directory.
# (C)2010 by HIROSE Yuuji [yuuji@yatex.org]
# Last modified Mon Sep  6 16:16:33 2010 on firestorm
# $Id: newpage.rb,v 33c8875f52f9 2012-01-12 12:26 +0900 yuuji $
# http://www.yatex.org
# Example:
#	newpage.rb		Create new index.html by copying template.
#	newpage.rb foo.html	Create new foo.html whose by copying header
#				and footer from index.html.
#	newpage.rb d/sub.html	Create new directory d (if necessary) and
#				d/sub.html by copying header/footer from
#				index.html in a same directory or parent
#				directory rewriting href to css file
#				considering relative path.
#	newpage.rb -o [file]	Forcibly overwrite existing file.
#	newpage.rb -c cssfile	Set `cssfile' as defualt css.
#	newpage.rb -t template	Set `template' as HTML template.
require 'fileutils'

mydir=File.dirname($0)
myname=File.basename($0, ".rb")


index = 'index.html'
cssdefault = nil
overwrite = nil
template = __FILE__ #File.expand_path(myname+".html", mydir)

def guesscss(dir)
  
end

while ARGV[0] && /^-/ =~ (a0=ARGV[0].dup) && ARGV.shift
  break if /^--$/ =~ a0
  while /^-[A-Za-z]/ =~ a0
    case a0
    when "-c"                   # css
      ARGV.shift; cssdefault = ARGV[0]
    when "-t"                   # template
      ARGV.shift; cssdefault = ARGV[0]
    when "-o"                   # overwrite
      overwrite = true
    end
    a0.sub!(/-.(.*)/, '-\\1')
  end
end

outfile = ARGV[0]||index
if !overwrite && test(?s, outfile) then
  STDERR.printf("File \`%s' exists.  Use -o option to overwrite.\n", outfile)
  exit 1
end

# set css default file
dots = 0
of = outfile
dots+=1 while "." != (of=File.dirname(of))
cssdir = "../"*dots

# set copy source
outdir = File.dirname(outfile)
if "index.html" == File.basename(outfile)
  src = (dots == 0 ? template : "index.html")
elsif test(?s, outdir+"/index.html")
  src = outdir+"/index.html"
else
  src = template
end

FileUtils.mkdir_p(outdir)

cssfile = cssdir+"main.css"
name = File.basename(outfile, ".html")
begin
  open(outfile, "w") do |out|
    #IO.foreach(src) do |line|
    if src == __FILE__
      input = DATA
    else
      input = open(src, "r")
    end
    begin
      html = input.readlines.join
      html.sub!(%r|^<h1.*<\/h1>|i, sprintf("<h1>%s</h1>\n", name))
      if !html.gsub!("__CSSFILE__", cssfile)
        html.gsub!(/href=(['\"])(.*\.css)\1/, 'href="'+cssdir+'\2"')
      end
      html.gsub!("__TITLE__", name)
      out.print html
    ensure
      input.close
    end
  end
  printf(<<_EOS_, outfile, name)
<a href="%s">%s</a>
_EOS_
rescue
  p $!
  STDERR.printf(<<'_EOS_', outfile, outfile)
Cannot output to [%s].  Do
  chmod +w %s
or
  chmod +w .
or change output directory.
_EOS_
  exit 1
end

__END__
<html>
<head>
<title>__TITLE__</title>
<style type="text/css">
<!--
/* Local CSS here */
-->
</style>
<link rel="stylesheet" type="text/css" href="__CSSFILE__">
</head>

<body>
<h1>__TITLE__</h1>

<!--#include virtual="/~yuuji/signature.html"-->
</body>
</html>
