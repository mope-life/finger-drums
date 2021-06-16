customElements.define(
  'control-knob',
  class extends HTMLElement {
	constructor() {
	  super();

	  const templateContent = document.getElementById("knob-template").content;
	  const shadowRoot = this.attachShadow({ mode: 'open' }); // closed could work too
	  shadowRoot.appendChild(templateContent.cloneNode(true));

	  const inputs = shadowRoot.querySelectorAll('input');
	  this._minInput = inputs[0];
	  this._maxInput = inputs[1];
	  this._precInput = inputs[2];
	  this._valueInput = inputs[3];
	  this._g = shadowRoot.querySelector('g');

	  this._currentRotation = 0;
	  this._rotationMax = 0;
	  this._rotationMin = -1.75 * Math.PI;

	  this.shadowRoot.addEventListener('input', e => {
		e.stopPropagation();
		e.preventDefault();
		const attr = e.target.dataset.attr;
		this[attr] = e.target.value;
	  });

	  this.shadowRoot.addEventListener('pointerdown', e => {
		if (e.target.tagName === "INPUT" || e.target.tagName === "LABEL") {
		  e.stopPropagation();
		}
	  });
	}

	get min() { return this.hasAttribute("min") ? +this.getAttribute("min") : 0; }
	get max() { return this.hasAttribute("max") ? +this.getAttribute("max") : 1; }
	get prec() { return this.hasAttribute("prec") ? +this.getAttribute("prec") : -2; }
	get value() { return this.hasAttribute("value") ? +this.getAttribute("value") : 0; }

	set min(newValue) { this.setAttribute("min", newValue); }
	set max(newValue) { this.setAttribute("max", newValue); }
	set prec(newValue) { this.setAttribute("prec", newValue); }
	set value(newValue) { this.setAttribute("value", newValue); }

	static get observedAttributes() {
	  return ["min", "max", "prec", "value"];
	}

	attributeChangedCallback(name, oldValue, newValue) {
	  let fixedValue = this._validateAttribute(name, newValue);
	  if (fixedValue != newValue) {
		this[name] = fixedValue;
	  } else if (oldValue != fixedValue) {
		this["_" + name + "Input"].value = fixedValue;

		this._currentRotation = this._getRotationFromValue(this.value);
		this._setTransform();

		this.dispatchEvent(new CustomEvent(
		  'input', {
			bubbles: true,
			cancelable: true,
			detail: this.value
		  }));
	  }
	}

	connectedCallback() {
	  this._precInput.setAttribute("min", -3);
	  this._precInput.setAttribute("max", 3);

	  this.prec = this.prec;
	  this.max = this.max;
	  this.min = this.min;
	  this.value = this.value;

	  this._g.addEventListener('pointerdown', this);
	  this._g.addEventListener('wheel', this);
	}

	disconnectedCallback() {
	  this._g.removeEventListener('pointerdown', this);
	  this._g.removeEventListener('wheel', this);
	}

	_applyPrecision(n) {
	  const prec = this.prec;
	  if (prec > 0) {
		return +((n * 0.0001).toFixed(-1 * (prec - 4)) * 10000).toFixed(0);
	  } else {
		return +((+n).toFixed(-1 * prec));
	  }
	}

	_validateAttribute(attr, newValue) {
	  switch (attr) {
	  case "min":
		newValue = this._applyPrecision(newValue);

		// Allow manual input of out-of-bounds values
		if (newValue >= this.max) {
		  this.max = newValue + 10**this.prec;
		}
		if (newValue > this.value) {
		  this.value = newValue;
		}
		this._maxInput.setAttribute("min", newValue);
		this._valueInput.setAttribute("min", newValue);
		break;
	  case "max":
		newValue = this._applyPrecision(newValue);

		// Allow manual input of out-of-bounds values
		if (newValue <= this.min) {
		  this.min = newValue - 10**this.prec;
		}
		if (newValue < this.value) {
		  this.value = newValue;
		}
		this._minInput.setAttribute("max", newValue);
		this._valueInput.setAttribute("max", newValue);
		break;
	  case "prec":
		newValue = newValue >= -3 ? (newValue <= 3 ? (Math.round(newValue)) : 3) : -3;
		let step = 10**newValue;
		this._minInput.setAttribute("step", step);
		this._maxInput.setAttribute("step", step);
		this._valueInput.setAttribute("step", step);
		break;
	  case "value":
		// Allow manual input of out-of-bounds values
		if (newValue < this.min) {
		  this.min = newValue;
		} else if (newValue > this.max) {
		  this.max = newValue;
		}
		break;
	  }
	  return newValue;
	}

	handleEvent(e) {
	  if (e.type === "pointerdown") this._handlePointerDown(e);
	  else if (e.type === "wheel") this._handleWheel(e);
	}

	_handlePointerDown(e1) {
	  let target = this;
	  let canceled = false;
	  target.style.cursor = "grabbing";

	  let callback = function (e2, lastAngle) {
		e1.preventDefault();
		e1.stopPropagation();

		let thisAngle = target._cursorAngle(e2.clientX, e2.clientY);
		let offset = thisAngle - lastAngle;

		// We crossed negative x axis going counterclockwise
		if (lastAngle > 0.5*Math.PI && thisAngle < -0.5*Math.PI) {
		  offset += 2*Math.PI;
		}
		// We crossed the negative x axis going clockwise
		else if (lastAngle < -0.5*Math.PI && thisAngle > 0.5*Math.PI) {
		  offset -= 2*Math.PI;
		}

		const newValue = target._getValueFromOffset(offset);
		const fixedValue = target._applyPrecision(newValue);

		// We haven't moved far enough to trigger a change at current precision
		if (fixedValue == target.value) {
		  thisAngle = lastAngle;
		}
		// Update the value from the new rotation
		else {
		  target.value = fixedValue;
		  const overshoot = target._getRotationFromValue(fixedValue) - target._getRotationFromValue(newValue);
		  thisAngle += overshoot;
		}

		if (!canceled) {
		  document.addEventListener(
			'pointermove',
			e3 => requestAnimationFrame( _ => callback(e3, thisAngle) ),
			{once: true}
		  );
		} else target.style.cursor = "";
	  };

	  document.addEventListener(
		'pointerup', () => canceled = true, {once: true}
	  );

	  callback(e1, target._cursorAngle(e1.clientX, e1.clientY));
	}

	_handleWheel(e) {
      const sign = e.deltaY > 0 ? 1 : -1;
      const offset = sign * 10**this.prec * (this._rotationMax - this._rotationMin) / (this.max - this.min);
	  const newValue = this._getValueFromOffset(offset);
	  this.value = this._applyPrecision(newValue);
	}

	_getValueFromOffset(offset) {
	  let rotation = this._currentRotation;
	  rotation += offset;

	  // We're going counterclockwise and passed the maximum
	  if (offset > 0 && this._currentRotation > this._rotationMax) {
		// lock at this point until we go back up
		rotation = this._rotationMax;
	  }
	  // We're going clockwise and passed the minimum
	  else if (offset < 0 && this._currentRotation < this._rotationMin) {
		// lock at this point until we go back down
		rotation = this._rotationMin;
	  }

	  const min = this.min;
	  const max = this.max;

	  let amt = (rotation - this._rotationMax) / (this._rotationMin - this._rotationMax);
	  if (amt == -0) {
		amt = 0;
	  }

	  let newValue = min + amt*(max - min);
	  if (newValue < min) newValue = min;
	  if (newValue > max) newValue = max;

	  return newValue;
	}

	_getRotationFromValue(value) {
	  const min = this.min;
	  const max = this.max;

	  let amt = (value - min) / (max - min);

	  return this._rotationMax + amt*(this._rotationMin - this._rotationMax);
	}

	_setTransform() {
	  let degrees = this._currentRotation * -180 / Math.PI;
	  this._g.setAttribute("transform", "rotate("+ degrees +")");
	}

	_cursorAngle(xPos, yPos) {
	  let rect = this._g.getBoundingClientRect();
	  return Math.atan2(
		(rect.top+rect.bottom)/2 - yPos,
		xPos - (rect.left+rect.right)/2
	  );
	}
  }
);
