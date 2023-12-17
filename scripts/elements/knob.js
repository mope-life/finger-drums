class KnobControl extends HTMLElement {
    constructor() {
        super();

        const template = document.getElementById("knob-template");
        const shadowRoot = this.attachShadow({ mode: "closed" });
        shadowRoot.appendChild(template.content.cloneNode(true));

        this._g = shadowRoot.querySelector("g");
        this._g.addEventListener("pointerdown", this);
        this._g.addEventListener("wheel", this);

        this._currentRotation = 0;
        this._rotationMax = 0;
        this._rotationMin = -1.75 * Math.PI;
    }

    static get observedAttributes() {
        return ["min", "max", "step", "value"];
    }

    connectedCallback() {}
    disconnectedCallback() {}
    adoptedCallback() {}

    attributeChangedCallback(name, oldv, newv) {
        if (oldv != newv) {
            this._changedCallbacks[name](newv);
        }
    }

    _changedCallbacks = {
        "min": this._minChanged.bind(this),
        "max": this._maxChanged.bind(this),
        "step": this._stepChanged.bind(this),
        "value": this._valueChanged.bind(this)
    }

    _minChanged(newv) {
        const fixed = this._applyStep(newv);
        if (fixed != newv) {
            this.min = fixed;
        }
        else {
            if (fixed >= this.max) {
                this.max = fixed;
            }
            if (fixed > this.value) {
                this.value = fixed;
            }
        }
    }

    _maxChanged(newv) {
        const fixed = this._applyStep(newv);
        if (fixed != newv) {
            this.max = fixed;
        }
        else {
            if (fixed <= this.min) {
                this.min = fixed;
            }
            if (fixed < this.value) {
                this.value = fixed;
            }
        }
    }

    _stepChanged(newv) {
        if (newv < 0) {
            this.step = 0;
        }
        else {
            // a different step might have caused any of these to change
            this.min = this.min;
            this.max = this.max;
            this.value = this.value;
        }
    }

    _valueChanged(newv) {
        const clamped = newv < this.min ? this.min
            : (newv > this.max ? this.max
                : newv);
        const fixed = this._applyStep(clamped)

        if (fixed != newv) {
            this.value = fixed;
        }
        else {
            this._currentRotation = this._rotationFromValue(this.value);
            this._setTransform();

            this.dispatchEvent(new CustomEvent(
                'input', {
                bubbles: true,
                cancelable: true,
                detail: this.value
            }));
        }
    }

    _applyStep(n) {
        return Math.round(n / this.step) * this.step;
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
            e2.preventDefault();
            e2.stopPropagation();

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

            const rotation = target._rotationFromOffset(offset);
            const newValue = target._valueFromRotation(rotation);
            const fixedValue = target._applyStep(newValue);

            // We haven't moved far enough to trigger a change at current step
            if (fixedValue == target.value) {
                thisAngle = lastAngle;
            }
            // Update the value from the new rotation
            else {
                target.value = fixedValue;
                const overshoot =
                    target._rotationFromValue(fixedValue)
                    - target._rotationFromValue(newValue);
                thisAngle += overshoot;
            }

            if (!canceled) {
                document.addEventListener(
                    'pointermove',
                    e3 => requestAnimationFrame(_ => callback(e3, thisAngle)),
                    { once: true }
                );
            }
            else {
                target.style.cursor = "";
            }
        };

        document.addEventListener(
            'pointerup', () => canceled = true, {once: true}
        );

        callback(e1, target._cursorAngle(e1.clientX, e1.clientY));
    }

    _handleWheel(e) {
        e.stopPropagation();
        const sign = e.deltaY > 0 ? 1 : -1;
        const offset = sign
            * this.step
            * (this._rotationMax - this._rotationMin)
            / (this.max - this.min);
        const rotation = this._rotationFromOffset(offset);
        const newValue = this._valueFromRotation(rotation);
        this.value = newValue;
    }

    _rotationFromOffset(offset) {
        // We're going counterclockwise and passed the maximum
        if (offset > 0 && this._currentRotation > this._rotationMax) {
            // lock at this point until we go back up
            return this._rotationMax;
        }
        // We're going clockwise and passed the minimum
        else if (offset < 0 && this._currentRotation < this._rotationMin) {
            // lock at this point until we go back down
            return this._rotationMin;
        }
        else {
            return this._currentRotation + offset;
        }
    }

    _valueFromRotation(rotation) {
        const min = this.min;
        const max = this.max;

        const amt =
            (rotation - this._rotationMax)
            / (this._rotationMin - this._rotationMax);

        const newValue = min + amt * (max - min);
        if (newValue < min) {
            return min;
        }
        else if (newValue > max) {
            return max;
        }
        else {
            return newValue;
        }
    }

    _rotationFromValue(value) {
        const min = this.min;
        const max = this.max;
        const amt = (value - min) / (max - min);
        return this._rotationMax
            + amt * (this._rotationMin - this._rotationMax);
    }

    _setTransform() {
        const degrees = this._currentRotation * -180 / Math.PI;
        this._g.setAttribute("transform", "rotate("+ degrees +")");
    }

    _cursorAngle(xPos, yPos) {
        const rect = this._g.getBoundingClientRect();
        return Math.atan2(
            (rect.top + rect.bottom) / 2 - yPos,
            xPos - (rect.left+rect.right) / 2
        );
    }

    get min() {
        return this.hasAttribute("min") ? +this.getAttribute("min") : 0;
    }
    set min(newv) {
        this.setAttribute("min", newv);
    }

    get max() {
        return this.hasAttribute("max") ? +this.getAttribute("max") : 1;
    }
    set max(newv) {
        this.setAttribute("max", newv);
    }

    get step() {
        return this.hasAttribute("step") ? +this.getAttribute("step") : 0.001;
    }
    set step(newv) {
        this.setAttribute("step", newv);
    }

    get value() {
        return this.hasAttribute("value") ? +this.getAttribute("value") : 0;
    }
    set value(newv) {
        this.setAttribute("value", newv);
    }
}

customElements.define("knob-control", KnobControl);
