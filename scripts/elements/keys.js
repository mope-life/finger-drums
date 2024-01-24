class KeysControl extends HTMLElement {
  constructor() {
    super();

    const template = document.getElementById("keys-template");
    const shadowRoot = this.attachShadow({ mode: 'open' });
    shadowRoot.appendChild(template.content.cloneNode(true));

    this._heldKeys = [];
    this._keyMap = (function () {
      let pianoKeys = [
        'z', 's', 'x', 'd', 'c', 'v', 'g', 'b', 'h',
        'n', 'j', 'm', ',', 'l', '.', ';', '/'
      ];
      let map = new Map();
      pianoKeys.forEach((k, i) => {
        // Map keys to their MIDI codes, starting from middle C.
        map.set(k, i + 60);
      });
      return map;
    })();
  }

  // This implies that this element needs to either have a tabindex, or
  // otherwise be fed keydown/keyup events.
  connectedCallback() {
    this.addEventListener("keydown", this);
    this.addEventListener("keyup", this);
  }

  disconnectedCallback() {
    this.removeEventListener("keydown", this);
    this.removeEventListener("keyup", this);
  }

  handleEvent(e) {
    if (e.type === "keydown") {
      this._keyDownHandler(e)
    }
    else if (e.type === "keyup") {
      this._keyUpHandler(e);
    }
  }

  _keyDownHandler(e) {
    const key = e.key.toLowerCase();

    if (this._keyMap.has(key)) {
      e.preventDefault();
      e.stopPropagation();

      if (!this._heldKeys.includes(key)) {
        this._heldKeys.unshift(key);

        let keyDiv = this.shadowRoot.getElementById(key);
        if (keyDiv) {
          keyDiv.classList.add("held");
        }

        this._sendKeys();
      }
    }
  }

  _keyUpHandler(e) {
    const key = e.key.toLowerCase();

    if (this._keyMap.has(key)) {
      e.preventDefault();
      e.stopPropagation();

      // since we will need the index anyway, use this instead of `includes`
      const index = this._heldKeys.indexOf(key);
      if (index >= 0) {
        this._heldKeys.splice(index, 1);

        let keyDiv = this.shadowRoot.getElementById(key);
        if (keyDiv) {
          keyDiv.classList.remove("held");
        }

        this._sendKeys();
      }
    }
  }

  _sendKeys() {
    this.dispatchEvent(new CustomEvent(
      "input", {
      bubbles: true,
      cancelable: true,
      detail: this._heldKeys
    }));
  }
}

customElements.define("keys-control", KeysControl);
