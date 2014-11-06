var casper = require('casper').create();
var x = require('casper').selectXPath;
var firstUrl;
casper.start('http://www.bolsar.com/', function() {
    this.echo(this.getTitle());
});

function just_wait() {
    setTimeout(function() {
        casper.echo("in just_wait...");
        casper.page.evaluate(function(){
            var a = document.getElementById("btnOk");
            var e = document.createEvent('MouseEvents');
            e.initMouseEvent('click', true, true, window, 0, 0, 0, 0, 0, false, false, false, false, 0, null);
            a.dispatchEvent(e);
            waitforload = true;
            casper.echo("here");
        });
    }, 2000);
}

casper.then(function() {
    firstUrl = this.getCurrentUrl()
    this.echo(firstUrl);
    this.evaluate(function(){
        document.getElementById('txtUsuario').value = 'martinrame';
        document.getElementById('txtPassword').value = 'leonardo13';
    });
    just_wait();
});

casper.waitFor(function check() {
    return this.getCurrentUrl() != firstUrl;
}, function then() {
    console.log(this.getCurrentUrl());
});


casper.run();
