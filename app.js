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

let persistData = (data) => {
  try {
    localStorage.setItem('pomodoro', JSON.stringify(data))
  } catch (e) {
    console.log(e);
  }
}

let loadData = () => {
  let defaultData = {
    tasks: [],
    lastNewId: 0,
    selectedTaskId: 0
  }

  try {
    let data = localStorage.getItem('pomodoro')
    if (data == null || data == undefined) {
      return defaultData
    }

    return JSON.parse(data)
  } catch (e) {
    console.log(e);
    return defaultData;
  }
}


window.onload = () => {
  let app = Elm.Pomodoro.init({
  })

  app.ports.notify.subscribe(notify)
}

