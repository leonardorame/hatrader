curl -b cookies.txt \
  --data '{"aEstadoTabla":[{"TablaNombre":"tbFuturos","FiltroEspecies":"","PagActualNro":"1","FilasxPagina":10,"MensajeNro":0,"HashCode":0},{"TablaNombre":"tbOpciones","FiltroEspecies":"","PagActualNro":"1","FilasxPagina":-1,"MensajeNro":0,"HashCode":0}]}' \
  'http://www.bolsar.com/VistasDL/PaginaFuturosOpciones.aspx/GetDataPack' \
  -H 'Host: www.bolsar.com' \
  -H 'User-Agent: Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.1; WOW64; Trident/6.0)' \
  -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8' \
  -H 'Accept-Language: en-US,en;q=0.5' \
  -H 'Accept-Encoding: gzip, deflate' \
  -H 'Content-Type: application/json; charset=utf-8' \
  -H 'Referer: http://www.bolsar.com/VistasDL/PaginaFuturosOpciones.aspx' \
  -H 'Connection: keep-alive' \
  -H 'Pragma: no-cache' \
  -H 'Cache-Control: no-cache' > opcionesprecios.txt

