import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

var app = Elm.Main.init({
  node: document.getElementById('root')
});

app.ports.scrollTop.subscribe(function(){
  console.log("scrollTop");
  window.scrollTo(0, 0);
});

registerServiceWorker();
