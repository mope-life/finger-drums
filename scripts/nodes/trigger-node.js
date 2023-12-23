class TriggerNode extends AudioWorkletNode {
  constructor(audioContext, parameterData) {
	super(audioContext, 'trigger-sender-processor', {
	  numberOfInputs: 0,
      numberOfOutputs: 1,
      outputChannelCount: [1],
      parameterData: parameterData
	});
  }

  trigger() {
	this.port.postMessage(null);
  }
}

export { TriggerNode }
