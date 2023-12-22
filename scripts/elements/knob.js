class KnobControl extends HTMLElement {
    constructor() {
        super();

        const template = document.getElementById("knob-template");
        const shadowRoot = this.attachShadow({ mode: "closed" });
        shadowRoot.appendChild(template.content.cloneNode(true));

        this._g = shadowRoot.querySelector("g");
        this._g.addEventListener("pointerdown", this);
        this._g.addEventListener("wheel", this);

        // beware that angles increase in the counter-clockwise direction, while
        // the 'value' of the knob naturally increases in the clockwise
        // direction
        this._knobAngleMax = 0;
        this._knobAngleMin = -1.75 * Math.PI;

        // helper for wheel handler
        this._scrollOvershoot = 0;

        // for memoizing transform on the svg element
        this._currentTransform = null;

        this._value = 0;
        this._max = 1;
        this._min = 0;
        this._intervals = "none";
    }

    static get observedAttributes() {
        return ["min", "max", "intervals"];
    }

    connectedCallback() {
        if (this.hasAttribute("value")) {
            this.value = this.getAttribute("value");
        }
        else {
            // ensure that value is valid for current parameters
            this.value = this._value;
        }
    }

    disconnectedCallback() { }
    adoptedCallback() { }

    attributeChangedCallback(name, oldv, newv) {
        if (newv === null || oldv === newv) {
            // no-op
        }
        else {
            switch (name) {
                case "min":
                    this._minChanged(newv);
                    break;
                case "max":
                    this._maxChanged(newv);
                    break;
                case "intervals":
                    this._intervalsChanged(newv);
                    break;
            }

            // force a recomputation of value with new parameters
            this.value = this._value;
        }
    }

    _minChanged(newv) {
        const n = +newv;
        if (!isNaN(n)) {
            this._min = n;
            if (n > this._max) {
                this._max = n;
            }
        }
    }

    _maxChanged(newv) {
        const n = +newv;
        if (!isNaN(n)) {
            this._max = n;
            if (n < this._min) {
                this._min = n;
            }
        }
    }

    _intervalsChanged(newv) {
        if (newv === "none")
            this._intervals = "none";
        else {
            const n = +newv;
            if (!isNaN(n)) {
                this._intervals = newv < 1 ? 1 : newv;
            }
        }
    }

    handleEvent(e) {
        if (e.type === "pointerdown") {
            this._handlePointerDown(e);
        }
        else if (e.type === "wheel") {
            this._handleWheel(e);
        }
    }

    _handlePointerDown(e0) {
        let canceled = false;

        let callback = function (e1, v0, thetaPrev, deltaTheta) {
            e1.preventDefault();
            e1.stopPropagation();

            const theta = this._cursorAngle(e1.clientX, e1.clientY);

            // total the pending cumulative delta with the new one...
            // this is all well and good until the user starts "winding" around
            // after reaching the minimum or maximum knob angles; we get around
            // that by clamping the total
            const deltaThetaNext = this._clamp(
                deltaTheta + this._getCursorDelta(theta, thetaPrev),
                this._angleDeltaFromValueDelta(this._max - v0),
                this._angleDeltaFromValueDelta(this._min - v0)
            );

            // convert angle delta to a value delta and apply
            const deltaV = this._valueDeltaFromAngleDelta(deltaThetaNext);
            this.value = v0 + deltaV;

            if (!canceled) {
                document.addEventListener(
                    'pointermove',
                    e2 => requestAnimationFrame(_ =>
                        callback.apply(this,
                            [e2, v0, theta, deltaThetaNext]
                        )
                    ),
                    { once: true }
                );
            }
        };

        document.addEventListener('pointerup', () =>
            canceled = true,
            { once: true }
        );
        const theta0 = this._cursorAngle(e0.clientX, e0.clientY);
        callback.apply(this, [e0, this.value, theta0, 0]);
    }

    _handleWheel(e) {
        e.stopPropagation();
        e.preventDefault();
        if (this._max === this._min) {
            this.value = this._min;
        }
        else {
            clearTimeout(this._timeoutId);

            // determined experimentally for a good "feel"
            const sensitivity = 1 / 60;

            // we normalize on knob angle to get consistent visual change
            // regardless of range
            const deltaTheta = -e.deltaY * sensitivity;
            const deltaV = this._valueDeltaFromAngleDelta(deltaTheta);
            const newValue = this.value + deltaV + this._scrollOvershoot;
            this.value = newValue;

            // if our scroll didn't result in a full click (when intervals is
            // set), we reserve the unused movement for a moment; otherwise,
            // large intervals could only be overcome in a single wheel event,
            // possibly necessitating a very vigorous scroll
            this._scrollOvershoot = newValue - this.value;

            // if we already hit the bounds, don't keep the overshoot
            if (this.value === this._max && this._scrollOvershoot > 0) {
                this._scrollOvershoot = 0;
            }
            else if (this.value === this._min && this._scrollOvershoot < 0) {
                this._scrollOvershoot = 0;
            }
            else {
                this._timeoutId = setTimeout(() => {
                    this._scrollOvershoot = 0;
                }, 0.2 * 1000
                );
            }
        }
    }

    _getCursorDelta(theta, thetaPrev) {
        const halfPi = Math.PI / 2;

        // we crossed the negative x-axis while going clockwise
        if (thetaPrev < -halfPi && theta > halfPi) {
            return theta - thetaPrev - 2 * Math.PI;
        }
        // we crossed the negative x-axis while going counter-clockwise
        else if (thetaPrev > halfPi && theta < -halfPi) {
            return theta - thetaPrev + 2 * Math.PI;
        }
        else {
            return theta - thetaPrev;
        }
    }

    _angleDeltaFromValueDelta(value) {
        return this._max === this._min ? 0 :
        value
            * (this._knobAngleMin - this._knobAngleMax)
            / (this._max - this._min);
    }

    _valueDeltaFromAngleDelta(theta) {
        return theta
            * (this._max - this._min)
            / (this._knobAngleMin - this._knobAngleMax);
    }

    _cursorAngle(xPos, yPos) {
        const rect = this._g.getBoundingClientRect();
        return Math.atan2(
            (rect.top + rect.bottom) / 2 - yPos,
            xPos - (rect.left + rect.right) / 2
        );
    }

    _clamp(x, min, max) {
        if (x < min) return min
        else if (x > max) return max;
        else return x;
    }

    _applyStep(value) {
        if (this._intervals === "none") {
            return value;
        }
        else {
            const range = this._max - this._min;
            if (range === 0) {
                return this._min;
            }
            const step = range / this._intervals;
            return Math.round((value - this._min) / step) * step + this._min;
        }
    }

    _setTransform(value) {
        const knobAngle = (this._min === this._max) ? 0 :
            this._knobAngleMax
            - this._angleDeltaFromValueDelta(value - this._min);

        if (knobAngle != this._currentTransform) {
            this._currentTransform = knobAngle;
            const degrees = knobAngle * 180 / Math.PI;
            this._g.setAttribute("transform", `rotate(${degrees})`);
        }
    }

    set value(newv) {
        const clamped = this._clamp(newv, this._min, this._max);
        const fixed = this._applyStep(clamped);

        // we always call setTransform, because sometimes we need to update the
        // knob rotation regardless of whether the value has changed
        this._setTransform(fixed);

        if (fixed === this._value) {
            return;
        }
        else {
            this._value = fixed;
            this.dispatchEvent(new CustomEvent(
                'input', {
                bubbles: true,
                cancelable: true,
                detail: fixed
            }));
        }
    }

    get value() {
        return this._value;
    }

    get min() {
        return this.hasAttribute("min") ? this.getAttribute("min") : "";
    }

    set min(newv) {
        this.setAttribute("min", newv);
    }

    get max() {
        return this.hasAttribute("max") ? this.getAttribute("max") : "";
    }

    set max(newv) {
        this.setAttribute("max", newv);
    }

    get intervals() {
        return this.hasAttribute("intervals") ? this.getAttribute("intervals") : "";
    }

    set intervals(newv) {
        this.setAttribute("intervals", newv);
    }
}

customElements.define("knob-control", KnobControl);
