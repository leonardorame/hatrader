curl -b cookies.txt -c cookies.txt 'http://www.bolsar.com/WebServices/AcreditadorWS.asmx/Refresh' \
  -H 'Host: www.bolsar.com' \
  -H 'User-Agent: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:33.0) Gecko/20100101 Firefox/33.0' \
  -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8' \
  -H 'Accept-Language: es-ar,es;q=0.8,en-us;q=0.5,en;q=0.3' \
  -H 'Accept-Encoding: gzip, deflate' \
  -H 'Content-Type: application/json; charset=utf-8' \
  -H 'Referer: http://www.bolsar.com/VistasTR/PaginaFuturosOpciones.aspx' \
  -H 'Content-Length: 0' \
  -H 'Connection: keep-alive' \
  -H 'Pragma: no-cache' \
  -H 'Cache-Control: no-cache'

