#! /bin/sh
#

echo "Content-Type: image/jpg"
echo
cat /var/www/adabroker/adabroker.jpg

/usr/bin/nslookup $REMOTE_HOST | 
  /usr/bin/Mail -s "AdaBroker access" \
  sam@ada.eu.org pautet@inf.enst.fr quinot@inf.enst.fr \
  > /dev/null 2> /dev/null
