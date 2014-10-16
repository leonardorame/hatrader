while true; do
#  curl "http://bolsa.baicom.com/2" | ../baicom/baicomparser | ./rttodaily
  curl "http://www.ceciliastrada.com.ar/cgi-bin/get_quotes.bf/quotes?url=bolsa.baicom.com" 
  sleep 10
  curl "http://www.ceciliastrada.com.ar/cgi-bin/get_quotes.bf/quotes?url=bolsa.baicom.com/2" 
#  curl "http://bolsa.baicom.com/" | ../baicom/baicomparser | ./rttodaily
  sleep 30
done
