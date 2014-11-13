var casper = require('casper').create({
         pageSettings: {
            userAgent: "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/30.0.1599.101 Safari/537.36"
        }
    });

casper.start();

casper.then(function(){
    var fs = require('fs')
    var data = fs.read("cookies.txt")
    phantom.cookies = JSON.parse(data)
});

casper.then(function(){
    var infoofertas = function(){
        casper.thenOpen('http://www.bolsar.com/Vistas/Cotizaciones/PanelOfertas.aspx/GetInfoOfertas',
            {
                method: "POST",
                headers: {
                    'Content-Type': 'application/json; charset=utf-8',
                },
                data: {
                    "obEstadoTabla":
                        {"TablaNombre":"PanelOfertas","FiltroVto":"","FiltroEspecies":"","PagActualNro":"1","FilasxPagina":-1,"MensajeNro":0,"HashCode":0},
                         "aFiltro":["ALUC7.00FE_0", "ERAC7.12DI_0"]
                }
            });
        casper.then(function(){
            casper.echo( this.getPageContent());
        });
    }

    casper.then(function(){
        infoofertas();
    });
});

casper.run();
