
curl 'http://www.bolsar.com/VistasDL/PaginaPrincipal.aspx/GetFechaYMercado' \
  --data '{"bDemora":"True"}' \
  -b cookies.txt \
  -H 'Host: www.bolsar.com' \
  -H 'User-Agent: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:33.0) Gecko/20100101 Firefox/33.0' \
  -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8' \
  -H 'Accept-Language: en-US,en;q=0.5' \
  -H 'Accept-Encoding: gzip, deflate' \
  -H 'Content-Type: application/json; charset=utf-8' \
  -H 'Referer: http://www.bolsar.com/VistasDL/PaginaPrincipal.aspx' \
  -H 'Content-Length: 18' \
  -H 'Cookie: __utma=133838749.498664200.1413974479.1413974479.1414966658.2; __utmz=133838749.1413974479.1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none); ASP.NET_SessionId=m1eyfe45dfubxqztpxhz4y55; ckLng=ESP; __utmb=133838749.2.9.1414966659335; __utmc=133838749; __utmt=1; .RAWR=FDC7E7B74AA8EDEB86065C8A3C16E5A06D75B2E7F49811F021A78AEA0337D9250514FE22BD46EE54A9ABBE7A4AE7E2EEE727E93EB18DA0BA37411B8BDAC5E612031F3FEBE2CB4D993E3498AC72F1CFC93CBCB375877F044D195E1C28E84D7A7F3A8AACD253FB5E40FE265D24D446C66A' \
  -H 'Connection: keep-alive' \
  -H 'Pragma: no-cache' \
  -H 'Cache-Control: no-cache' > fechaymercado.txt

curl -b cookies.txt \
  --data '{"intPanelId":"0"}' \
  'http://www.bolsar.com/VistasDL/PaginaLideres.aspx/GetEspecies' \
  -H 'Host: www.bolsar.com' \
  -H 'User-Agent: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:33.0) Gecko/20100101 Firefox/33.0' \
  -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8' \
  -H 'Accept-Language: en-US,en;q=0.5' \
  -H 'Accept-Encoding: gzip, deflate' \
  -H 'Content-Type: application/json; charset=utf-8' \
  -H 'Referer: http://www.bolsar.com/VistasDL/PaginaLideres.aspx' \
  -H 'Content-Length: 18' \
  -H 'Cookie: __utma=133838749.498664200.1413974479.1413974479.1414966658.2; __utmz=133838749.1413974479.1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none); ASP.NET_SessionId=m1eyfe45dfubxqztpxhz4y55; ckLng=ESP; __utmb=133838749.7.9.1414966696539; __utmc=133838749; ckAA=1; __utmt=1' \
  -H 'Connection: keep-alive' \
  -H 'Pragma: no-cache' \
  -H 'Cache-Control: no-cache' > especies.txt

curl -b cookies.txt \
  --data '{"aEstadoTabla":[{"TablaNombre":"tbAcciones","FiltroVto":"72","FiltroEspecies":"","FilasxPagina":-1,"MensajeNro":0,"HashCode":0},{"TablaNombre":"tbMontos","FiltroVto":"","FiltroEspecies":"","PagActualNro":"1","FilasxPagina":-1,"MensajeNro":0,"HashCode":0},{"TablaNombre":"tbIndices","FiltroVto":"","FiltroEspecies":"","PagActualNro":"1","FilasxPagina":-1,"MensajeNro":0,"HashCode":0}]}' \
  'http://www.bolsar.com/VistasDL/PaginaLideres.aspx/GetDataPack' \
  -H 'Host: www.bolsar.com' \
  -H 'User-Agent: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:33.0) Gecko/20100101 Firefox/33.0' \
  -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8' \
  -H 'Accept-Language: en-US,en;q=0.5' \
  -H 'Accept-Encoding: gzip, deflate' \
  -H 'Content-Type: application/json; charset=utf-8' \
  -H 'Referer: http://www.bolsar.com/VistasDL/PaginaLideres.aspx' \
  -H 'Content-Length: 385' \
  -H 'Cookie: __utma=133838749.498664200.1413974479.1413974479.1414966658.2; __utmz=133838749.1413974479.1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none); ASP.NET_SessionId=m1eyfe45dfubxqztpxhz4y55; ckLng=ESP; __utmb=133838749.7.9.1414966696539; __utmc=133838749; ckAA=1; __utmt=1' \
  -H 'Connection: keep-alive' \
  -H 'Pragma: no-cache' \
  -H 'Cache-Control: no-cache' > acciones.txt

curl -b cookies.txt \
  --data '{"bDemora":"True"}' \
  'http://www.bolsar.com/VistasDL/PaginaFuturosOpciones.aspx/GetOpciones' \
  -H 'Host: www.bolsar.com' \
  -H 'User-Agent: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:33.0) Gecko/20100101 Firefox/33.0' \
  -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8' \
  -H 'Accept-Language: en-US,en;q=0.5' \
  -H 'Accept-Encoding: gzip, deflate' \
  -H 'Content-Type: application/json; charset=utf-8' \
  -H 'Referer: http://www.bolsar.com/VistasDL/PaginaFuturosOpciones.aspx' \
  -H 'Content-Length: 18' \
  -H 'Cookie: __utma=133838749.498664200.1413974479.1413974479.1414966658.2; __utmz=133838749.1413974479.1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none); ASP.NET_SessionId=m1eyfe45dfubxqztpxhz4y55; ckLng=ESP; __utmb=133838749.12.9.1414968156195; __utmc=133838749; ckAA=1; __utmt=1' \
  -H 'Connection: keep-alive' \
  -H 'Pragma: no-cache' \
  -H 'Cache-Control: no-cache' > opciones.txt

