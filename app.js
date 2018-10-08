import './src/styles.scss'
import './node_modules/font-awesome/css/font-awesome.css'
import 'bootstrap'
import { Elm } from './pomodoro'

Notification.requestPermission().then((result) => {
  console.log(`Request notificaiton permission: ${result}`);
})

let notify = (message) => {
  if (!("Notification" in window)) {
    alert("This browser does not support system notifications")
  }
  else {
    let notification = new Notification('Conductor', {
      body: message,
      requireInteraction: true
    })
  }
}

window.onload = () => {
  let app = Elm.Pomodoro.init({
    node: document.getElementById("app")
  })

  app.ports.notify.subscribe(notify)
}

