import { VirtualAudioContext } from './elm-web-audio.js'
import { EnvelopeNode } from "./nodes/envelope-node.js";
import { TriggerNode } from "./nodes/trigger-node.js";
import "./elements/knob.js"
import "./elements/keys.js"

// Initialize our elm app right away, as it builds our whole UI.
const app = Elm.AudioModuleMap.init({
  node: document.getElementById("audio-module-map")
});

// For the audio context, we have to wait on a few things...
let audioContext = null;

(() => {
  // We want to wait for one of these events to occur before creating our context.
  // See link for list of events that count as an "activation triggering input event":
  // https://developer.mozilla.org/en-US/docs/Web/Security/User_activation
  const activationTriggeringEvents =
    ["keydown", "mousedown", "pointerdown", "pointerup", "touchend"];

  // We will set this in a moment.
  let resolve = null;

  // Flag for debouncing.
  let done = false;

  // Make sure we have a real function binding, so that we can remove the event
  // listeners.
  function initializeContextListener(e) {
    // The events above have some exceptions, like untrusted events, and certain
    // keys reserved by the browser (esc, tab, f keys... too many to enumerate).
    if (!navigator.userActivation.hasBeenActive) return;

    // Make sure this only happens once.
    if (done) return;
    done = true;

    // Construct our context, now that the user has interacted with the page.
    const ctx = new VirtualAudioContext();

    // Clear this listener and its buddies.
    activationTriggeringEvents.forEach(type =>
      document.removeEventListener(type, initializeContextListener)
    );

    // Pass on our new context.
    resolve(ctx);
  }

  // Since we are waiting on user interaction that might occur at some
  // unspecified time, we use a Promise.
  return new Promise(function (r) {
    resolve = r;
    activationTriggeringEvents.forEach(function (type) {
      document.addEventListener(type, initializeContextListener);
    });
  });
})()
  .then(ctx => {
    return Promise.all([
      // Pass our context to the next guy.
      Promise.resolve(ctx),
      // Instantiate our custom audio nodes.
      ctx.audioWorklet.addModule("./scripts/nodes/envelope-processor.js"),
      ctx.audioWorklet.addModule("./scripts/nodes/trigger-sender-processor.js")
    ])
  })
  .then(values => {
    // Finally, assign our real audioContext.
    audioContext = values[0];

    // Create factory methods for our custom nodes.
    audioContext.createEnvelope = function (parameterData) {
      return new EnvelopeNode(this, parameterData);
    }.bind(audioContext);
    audioContext.createTrigger = function (parameterData) {
      return new TriggerNode(this, parameterData);
    }.bind(audioContext);

    // Subscribe to our Elm ports.
    app.ports.toWebAudio.subscribe((nodes) => {
      audioContext.update(nodes);
    });
  });
