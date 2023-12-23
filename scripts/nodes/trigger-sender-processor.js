class TriggerSenderProcessor extends AudioWorkletProcessor {
  static get parameterDescriptors() {
    return [
      {
        name: 'pulseWidth',
        minValue: 1, // number of samples
        defaultValue: 1,
        automationRate: 'k-rate'
      }
    ]
  }

  constructor() {
    super();

    this._pendingMessage = false;
    this._pendingSamples = 0;

    this.port.onmessage = e => {
      this._pendingMessage = true;
    }
  }

  process(inputs, outputs, parameters) {
    if (this._pendingMessage) {
      this._pendingMessage = false;
      this._pendingSamples = parameters.pulseWidth[0];
    }

    let samplesWritten = 0;
    for (let i = 0; i < outputs.length; ++i) {
      for (let j = 0; j < outputs[i].length; ++j) {
        let samplesToWrite = Math.min(outputs[i][j].length || 0, this._pendingSamples);
        samplesWritten = Math.max(samplesToWrite, samplesWritten);
        for (let k = 0; k < samplesToWrite; ++k) {
          outputs[i][j][k] = 1;
        }
      }
    }

    this._pendingSamples -= samplesWritten;

    return true;
  }
}

registerProcessor('trigger-sender-processor', TriggerSenderProcessor);
