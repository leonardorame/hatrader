SERVER=www.ceciliastrada.com.ar
USER=cecilia2
PASSW=qQ5Qo5c0h3

ftp -p -v -n $SERVER <<END_OF_SESSION
user $USER $PASSW
$FILETYPE
binary
cd /public_html/quotes
mput *.csv
bye
END_OF_SESSION
