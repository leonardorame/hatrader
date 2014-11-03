curl -b cookies.txt \
  --data '{"aEstadoTabla":[{"TablaNombre":"tbFuturos","FiltroEspecies":"","PagActualNro":"1","FilasxPagina":10,"MensajeNro":0,"HashCode":0},{"TablaNombre":"tbOpciones","FiltroEspecies":"","PagActualNro":"1","FilasxPagina":50,"MensajeNro":0,"HashCode":0}]}' \
  'http://www.bolsar.com/VistasDL/PaginaFuturosOpciones.aspx/GetDataPack' \
  -H 'Host: www.bolsar.com' \
  -H 'User-Agent: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:33.0) Gecko/20100101 Firefox/33.0' \
  -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8' \
  -H 'Accept-Language: en-US,en;q=0.5' \
  -H 'Accept-Encoding: gzip, deflate' \
  -H 'Content-Type: application/json; charset=utf-8' \
  -H 'Referer: http://www.bolsar.com/VistasDL/PaginaFuturosOpciones.aspx' \
  -H 'Content-Length: 245' \
  -H 'Cookie: __utma=133838749.498664200.1413974479.1413974479.1414966658.2; __utmz=133838749.1413974479.1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none); ASP.NET_SessionId=m1eyfe45dfubxqztpxhz4y55; ckLng=ESP; __utmc=133838749; ckAA=1' \
  -H 'Connection: keep-alive' \
  -H 'Pragma: no-cache' \
  -H 'Cache-Control: no-cache' > opcionesprecios.txt

curl -b cookies.txt \
  --data '{"aEstadoTabla":[{"TablaNombre":"tbFuturos","FiltroEspecies":"","PagActualNro":"1","FilasxPagina":10,"MensajeNro":0,"HashCode":0},{"TablaNombre":"tbOpciones","FiltroEspecies":"","PagActualNro":"2","FilasxPagina":50,"MensajeNro":0,"HashCode":0}]}' \
  'http://www.bolsar.com/VistasDL/PaginaFuturosOpciones.aspx/GetDataPack' \
  -H 'Host: www.bolsar.com' \
  -H 'User-Agent: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:33.0) Gecko/20100101 Firefox/33.0' \
  -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8' \
  -H 'Accept-Language: en-US,en;q=0.5' \
  -H 'Accept-Encoding: gzip, deflate' \
  -H 'Content-Type: application/json; charset=utf-8' \
  -H 'Referer: http://www.bolsar.com/VistasDL/PaginaFuturosOpciones.aspx' \
  -H 'Content-Length: 245' \
  -H 'Cookie: __utma=133838749.498664200.1413974479.1413974479.1414966658.2; __utmz=133838749.1413974479.1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none); ASP.NET_SessionId=m1eyfe45dfubxqztpxhz4y55; ckLng=ESP; __utmc=133838749; ckAA=1' \
  -H 'Connection: keep-alive' \
  -H 'Pragma: no-cache' \
  -H 'Cache-Control: no-cache' > opcionesprecios2.txt
