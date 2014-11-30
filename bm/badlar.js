var casper = require('casper').create({
         pageSettings: {
            userAgent: "Mozilla/5.0 (Windows NT 6.1; WOW64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/30.0.1599.101 Safari/537.36"
        }
    });

casper.start();

casper.then(function(){
    var lideres = function(){
        casper.thenOpen('http://www.ambito.com/economia/mercados/tasas/x_tasas_get_grafico.asp?ric=ARSBADPR1MD=RR&tipo=m',
            {
                method: "GET"
            });
        casper.then(function(){
            // retornamos la salida
            casper.echo( this.getPageContent());
        });
    }

    casper.then(function(){
        lideres();
    });
});

casper.run();
