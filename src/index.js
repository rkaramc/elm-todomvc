import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

const storedState = localStorage.getItem('elm-todo-save');
const startingState = storedState ? JSON.parse(storedState) : null;
const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: startingState
});

app.ports.setStorage.subscribe(state => {
  localStorage.setItem('elm-todo-save', JSON.stringify(state));
});

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
