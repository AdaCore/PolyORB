#! /bin/sh
#

echo "Content-Type: image/jpg"
echo
cat /var/www/adabroker/adabroker.jpg

(echo $REMOTE_HOST | /usr/bin/Mail -s "adabroker" sam@ada.eu.org) > /dev/null 2> /dev/null
(echo $REMOTE_HOST | /usr/bin/Mail -s "adabroker" pautet@inf.enst.fr) > /dev/null 2> /dev/null
(echo $REMOTE_HOST | /usr/bin/Mail -s "AdaBroker access" thomas@cuivre.fr.eu.org) > /dev/null 2> /dev/null
