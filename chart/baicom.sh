while true; do
  curl "http://www.ceciliastrada.com.ar/cgi-bin/get_quotes.bf/quotes?url=bolsa.baicom.com" 
  sleep 10
  curl "http://www.ceciliastrada.com.ar/cgi-bin/get_quotes.bf/quotes?url=bolsa.baicom.com/2" 
  sleep 30
done
