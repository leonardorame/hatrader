curl -b cookies.txt \
  --data '{"aEstadoTabla":[{"TablaNombre":"tbAcciones","FiltroVto":"72","FiltroEspecies":"","FilasxPagina":-1,"MensajeNro":0,"HashCode":0},{"TablaNombre":"tbMontos","FiltroVto":"","FiltroEspecies":"","PagActualNro":"1","FilasxPagina":-1,"MensajeNro":0,"HashCode":0},{"TablaNombre":"tbIndices","FiltroVto":"","FiltroEspecies":"","PagActualNro":"1","FilasxPagina":-1,"MensajeNro":0,"HashCode":0}]}' \
  'http://www.bolsar.com/VistasDL/PaginaLideres.aspx/GetDataPack' \
  -H 'Host: www.bolsar.com' \
  -H 'User-Agent: Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.1; WOW64; Trident/6.0)' \
  -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8' \
  -H 'Accept-Language: en-US,en;q=0.5' \
  -H 'Accept-Encoding: gzip, deflate' \
  -H 'Content-Type: application/json; charset=utf-8' \
  -H 'Referer: http://www.bolsar.com/VistasDL/PaginaLideres.aspx' \
  -H 'Content-Length: 385' \
  -H 'Cookie: __utma=133838749.498664200.1413974479.1413974479.1414966658.2; __utmz=133838749.1413974479.1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none); ASP.NET_SessionId=m1eyfe45dfubxqztpxhz4y55; ckLng=ESP; __utmb=133838749.7.9.1414966696539; __utmc=133838749; ckAA=1; __utmt=1' \
  -H 'Connection: keep-alive' \
  -H 'Pragma: no-cache' \
  -H 'Cache-Control: no-cache' > lideres.txt
