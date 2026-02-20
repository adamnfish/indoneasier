import './main.css';
import { Elm } from './Main.elm';

import shippingUrl from 'url:../public/images/shipping.png';
import riceUrl from 'url:../public/images/rice.png';
import spiceUrl from 'url:../public/images/spice.png';
import ricespiceUrl from 'url:../public/images/ricespice.png';
import siapfajiUrl from 'url:../public/images/siapfaji.png';
import rubberUrl from 'url:../public/images/rubber.png';
import oilUrl from 'url:../public/images/oil.png';

var app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: {
    assets: {
      shipping: shippingUrl,
      rice: riceUrl,
      spice: spiceUrl,
      ricespice: ricespiceUrl,
      siapfaji: siapfajiUrl,
      rubber: rubberUrl,
      oil: oilUrl
    }
  }
});

app.ports.scrollTop.subscribe(function(){
  window.scrollTo(0, 0);
});
