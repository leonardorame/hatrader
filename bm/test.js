var casper = require('casper').create();
var x = require('casper').selectXPath;
var firstUrl;
casper.start('http://www.bolsar.com/', function() {
    this.echo(this.getTitle());
});

casper.then(function() {
    firstUrl = this.getCurrentUrl()
    this.echo(firstUrl);
    this.echo(this.page.content);
    this.waitForSelector('txtUsuario

    casper.sendKeys('txtUsuario', 'martinrame');
    casper.sendKeys('txtPassword', 'leonardo13');
    casper.then(function() {
        page.evaluate(function(){
            var a = document.getElementById("btnOk");
            var e = document.createEvent('MouseEvents');
            e.initMouseEvent('click', true, true, window, 0, 0, 0, 0, 0, false, false, false, false, 0, null);
            a.dispatchEvent(e);
            waitforload = true;
        });
    });
});

casper.waitFor(function check() {
    return this.getCurrentUrl() != firstUrl;
}, function then() {
    console.log(this.getCurrentUrl());
});


casper.run();
