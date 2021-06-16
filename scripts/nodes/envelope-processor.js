const MESSAGE_TYPE = {
  GATE_ON: 0,
  GATE_OFF: 1,
  TRIGGER_ON: 2,
  TRIGGER_OFF: 3
}

class EnvelopeProcessor extends AudioWorkletProcessor  {
  static get parameterDescriptors() {
    return [
      {
        name: 'attack',
        minValue: 0,
        defaultValue: 0,
        automationRate: 'k-rate'
      },
      {
        name: 'decay',
        minValue: 0,
        defaultValue: 0,
        automationRate: 'k-rate'
      },
      {
        name: 'sustain',
        minValue: 0,
        maxValue: 1,
        defaultValue: 1,
        automationRate: 'k-rate'
      },
      {
        name: 'release',
        minValue: 0,
        defaultValue: 0,
        automationRate: 'k-rate'
      },
      {
        name: 'trigger',
        defaultValue: 0,
        automationRate: 'a-rate'
      },
      {
        name: 'gate',
        defaultValue: 0,
        automationRate: 'a-rate'
      },
      {
        name: 'output',
        defaultValue: 0,
        automationRate: 'a-rate'
      }
    ];
  }

  constructor() {
    super(...arguments);

    this._maintain = true;
    this._lastGateState = false;
    this._lastTriggerState = false;

	// This gives us a way to destroy the envelope when we have to
    this.port.onmessage = message => {
      this._maintain = message.data;
    }
  }

  process(inputs, outputs, parameters) {
    for (let i = 0; i < parameters.trigger.length; ++i) {
      if (parameters.trigger[i] != 0 && this._lastTriggerState == false) {
        this._lastTriggerState = true;
        this.port.postMessage([MESSAGE_TYPE.TRIGGER_ON, currentTime + i / sampleRate]);
      } else if (parameters.trigger[i] == 0 && this._lastTriggerState == true) {
        this._lastTriggerState = false;
        this.port.postMessage([MESSAGE_TYPE.TRIGGER_OFF, currentTime + i / sampleRate]);
      }
    }

    for (let i = 0; i < parameters.gate.length; ++i) {
      if (parameters.gate[i] != 0 && this._lastGateState == false) {
        this._lastGateState = true;
        this.port.postMessage([MESSAGE_TYPE.GATE_ON, currentTime + i / sampleRate]);
      } else if (parameters.gate[i] == 0 && this._lastGateState == true) {
        this._lastGateState = false;
        this.port.postMessage([MESSAGE_TYPE.GATE_OFF, currentTime + i / sampleRate]);
      }
    }

    if (parameters.output.length == 1) {
      for (let i = 0; i < outputs.length; ++i) {
        for (let j = 0; j < outputs[i].length; ++j) {
          outputs[i][j].fill(parameters.output[0]);
        }
      }
    } else {
      for (let i = 0; i < outputs.length; ++i) {
        for (let j = 0; j < outputs[i].length; ++j) {
          outputs[i][j].set(parameters.output);
        }
      }
    }

    return this._maintain;
  }
}

registerProcessor('envelope-processor', EnvelopeProcessor);
