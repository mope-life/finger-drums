// For use with AudioParam.setTargetAtTime:
// https://developer.mozilla.org/en-US/docs/Web/API/AudioParam/setTargetAtTime
const ENVELOPE_TIME_FACTOR = 1.0 / 3.0;

const MESSAGE_TYPE = {
  GATE_ON: 0,
  GATE_OFF: 1,
  TRIGGER_ON: 2,
  TRIGGER_OFF: 3
}

class EnvelopeNode extends AudioWorkletNode {
  constructor(audioContext, parameterData) {
    super(audioContext, 'envelope-processor', {
      numberOfInputs: 0,
      numberOfOutputs: 1,
      parameterData: parameterData
    });

    this._output = this.parameters.get('output');

    this._attackEndTime = 0;
    this._gateState = false;

    this.port.onmessage = message => {
      const messageType = message.data[0];
      const startTime = message.data[1];

      switch (messageType) {
        case MESSAGE_TYPE.GATE_ON:
          this._gateState = true;
          this._setAttackAndDecay(startTime);
          break;
        case MESSAGE_TYPE.GATE_OFF:
          this._gateState = false;
          this._setRelease(startTime);
          break;
        case MESSAGE_TYPE.TRIGGER_ON:
          this._setAttackAndDecay(startTime);
          break;
        case MESSAGE_TYPE.TRIGGER_OFF:
          if (!this._gateState) {
            this._setRelease(this._attackEndTime);
          }
          break;
      }
    }
  }

  get gate() { return this.parameters.get('gate') }
  get trigger() { return this.parameters.get('trigger') }
  get attack() { return this.parameters.get('attack') }
  get decay() { return this.parameters.get('decay') }
  get sustain() { return this.parameters.get('sustain') }
  get release() { return this.parameters.get('release') }

  _setAttackAndDecay(startTime) {
    const attack = this.attack.value;
    const decay = this.decay.value;
    const sustain = this.sustain.value;

    this._output.cancelScheduledValues(startTime);

    // To avoid cliffs, we approach the sustain volume rather than peak volume
    // if there is no decay time.
    let attackTarget = decay > 0 ? 1 : sustain;

    if (attack != 0) {
      this._attackEndTime = startTime + attack;
      this._output.setTargetAtTime(
        attackTarget, startTime, attack * ENVELOPE_TIME_FACTOR
      );
    } else {
      this._attackEndTime = startTime;
      this._output.setValueAtTime(1, startTime);
    }
    if (decay != 0) {
      this._output.setTargetAtTime(
        sustain, startTime + attack, decay * ENVELOPE_TIME_FACTOR
      );
    } else {
      this._output.setValueAtTime(sustain, startTime + attack);
    }
  }

  _setRelease(startTime) {
    this._output.cancelScheduledValues(startTime);

    const release = this.release.value;

    if (release != 0) {
      this._output.setTargetAtTime(
        0, startTime, release * ENVELOPE_TIME_FACTOR
      );
    } else {
      this._output.setValueAtTime(0, startTime);
    }
  }
}

export { EnvelopeNode }
