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
  -H 'Connection: keep-alive' \
  -H 'Pragma: no-cache' \
  -H 'Cache-Control: no-cache' > opciones.txt

