/* Converts MIDI note ids into frequencies. */
/* Assumes twelve-tone equal temperament. Different temperaments may be implemented in future */
function frequencyOf(noteId, a4 = 440) {
  return a4 * 2**((noteId - 69) / 12);
}

class KeysControl extends HTMLElement {
  constructor() {
    super();

    const templateContent = document.getElementById("keys-template").content;
    const shadowRoot = this.attachShadow({ mode: 'open' });
    shadowRoot.appendChild(templateContent.cloneNode(true));

    this._heldKeys = [];
    this._keyMap = (function () {
      let pianoKeys = ['z', 's', 'x', 'd', 'c', 'v', 'g', 'b', 'h', 'n', 'j', 'm', ',', 'l', '.', ';', '/'];
      let map = new Map();
      pianoKeys.forEach((k, i) => {
        map.set(k, i + 60); // MIDI key values
      });
      return map;
    })();
  }

  connectedCallback() {
    document.addEventListener("keydown", this);
    document.addEventListener("keyup", this);
  }

  disconnectedCallback() {
    document.removeEventListener("keydown", this);
    document.removeEventListener("keyup", this);
  }

  handleEvent(e) {
    if (document.activeElement && document.activeElement.tagName === "INPUT") return;
    if (e.type === "keydown") this._keyDownHandler(e);
    else if (e.type === "keyup") this._keyUpHandler(e);
  }

  _keyDownHandler(e) {
    const key = e.key.toLowerCase();

    if (this._keyMap.has(key) && !this._heldKeys.includes(key)) {
      e.preventDefault();
      e.stopPropagation();

      this._hold(key);
      this._send(frequencyOf(this._keyMap.get(key)));
    }
  }

  _keyUpHandler(e) {
    const key = e.key.toLowerCase();

    if (this._keyMap.has(key) && this._heldKeys.includes(key)) {
      e.preventDefault();
      e.stopPropagation();

      const lastPressed = this._release(key);

      if (this._heldKeys.length > 0) {
        if (lastPressed) {
          this._send(frequencyOf(this._keyMap.get(this._heldKeys[this._heldKeys.length - 1])));
        }
      } else {
        this._send(0);
      }
    }
  }

  _hold(key) {
    this._heldKeys.push(key);
    let keyDiv = this.shadowRoot.getElementById(key);
    if (keyDiv) {
      keyDiv.classList.add("held");
    }
  }

  // Returns whether the key we are releasing was also the last one pressed
  _release(key) {
    const idx = this._heldKeys.indexOf(key);
    this._heldKeys.splice(idx, 1);

    let keyDiv = this.shadowRoot.getElementById(key);
    if (keyDiv) {
      keyDiv.classList.remove("held");
    }

    return idx == this._heldKeys.length;
  }

  _send(freq) {
    this.dispatchEvent(new CustomEvent(
      "input", {
      bubbles: true,
      cancelable: true,
      detail: freq
    }));
  }
}

customElements.define("keys-control", KeysControl);
