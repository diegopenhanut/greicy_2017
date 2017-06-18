#!/usr/bin/env bash
cd tabelas_sisvan
echo "removing csv"
rm -f tabelas_sisvan/*.csv
echo "converting xls to csv"
# Sudo needed for conversion. Not sure why.
sudo su -c 'for i in *.xls; do  echo "processing $i"; libreoffice --headless --convert-to csv "$i" ; chown $USER:$USER "$i"; done'
cd -
echo "done"
