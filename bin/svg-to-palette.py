#!/usr/bin/python

import os, re, sys
from xml.dom import minidom

if len(sys.argv) < 3:
  print '''Usage:
  svg-to-palette.py THE-SVG-FILE A-NAME-FOR-THE-PALETTE
  Note that the name of the palette should not have spaces in it.
'''
  exit(1)
svg_file = sys.argv[1]
palette_name = sys.argv[2]
if not svg_file or not palette_name:
  exit(1)

dom = minidom.parse(svg_file)
colors = []
color_re = re.compile('.*(#[0-9a-fA-F]+).*')
for x in dom.getElementsByTagName('rect'):
  style_attrs = x.getAttribute('style').split(';')
  fill_color = filter(lambda x: x.startswith('fill:'), style_attrs)[0]
  m = color_re.search(fill_color)
  if m:
    rgb_hex = m.groups()[0]
    rgb = int(rgb_hex[1:3], 16), int(rgb_hex[3:5], 16), int(rgb_hex[5:7], 16)
    colors.append(rgb)

palette = 'GIMP Palette\nName: %s\n#\n\n' % palette_name
for (r, g, b) in colors:
  palette += '%3d %3d %3d #%02x%02x%02x\n' % (r, g, b, r, g, b)
palette_filename = '~/.config/inkscape/palettes/%s.gpl' % palette_name
with open(os.path.expanduser(palette_filename), 'w') as palette_file:
  palette_file.write(palette)
