curl -b cookies.txt \
  --data '{"aEstadoTabla":[{"TablaNombre":"tbPymes","FiltroVto":"72","FiltroEspecies":"","PagActualNro":"1","FilasxPagina":-1,"MensajeNro":0,"HashCode":0},{"TablaNombre":"tbGeneral","FiltroVto":"72","FiltroEspecies":"","PagActualNro":"1","FilasxPagina":-1,"MensajeNro":0,"HashCode":0}]}' \
  'http://www.bolsar.com/VistasDL/PaginaGeneral.aspx/GetDataPack' \
  -H 'Host: www.bolsar.com' \
  -H 'User-Agent: Mozilla/5.0 (compatible; MSIE 10.0; Windows NT 6.1; WOW64; Trident/6.0)' \
  -H 'Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8' \
  -H 'Accept-Language: en-US,en;q=0.5' \
  -H 'Accept-Encoding: gzip, deflate' \
  -H 'Content-Type: application/json; charset=utf-8' \
  -H 'Referer: http://www.bolsar.com/VistasDL/PaginaGeneral.aspx' \
  -H 'Content-Length: 276' \
  -H 'Cookie: usr=martinrame; __utma=133838749.498664200.1413974479.1414975519.1415010489.4; __utmz=133838749.1413974479.1.1.utmcsr=(direct)|utmccn=(direct)|utmcmd=(none); BB_fltEspecie_martinrame_tbOpciones=GFGC; ASP.NET_SessionId=qjnklbe0t1e4c3mwcfcwgx55; ckLng=ESP; __utmb=133838749.3.9.1415010490757; __utmc=133838749; __utmt=1' \
  -H 'Connection: keep-alive' \
  -H 'Pragma: no-cache' \
  -H 'Cache-Control: no-cache' > general.txt
