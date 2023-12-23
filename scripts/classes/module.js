import { EnvelopeNode } from "../nodes/envelope-node.js";
import { TriggerNode } from "../nodes/trigger-node.js";

const audioContext = new AudioContext();

audioContext.audioWorklet.addModule("./scripts/nodes/envelope-processor.js").then(() => {
  audioContext.createEnvelope = function(parameterData) {
    return new EnvelopeNode(audioContext, parameterData);
  };
});

audioContext.audioWorklet.addModule("./scripts/nodes/trigger-sender-processor.js").then(() => {
  audioContext.createTrigger = function(parameterData) {
    return new TriggerNode(audioContext, parameterData);
  };
});


class Module {
  constructor(elt, parentMap) {
    this.elt = elt;
    this.parentMap = parentMap;
    this.endpoints = [];
	this.endpointDescriptors = [];
	this.controlDescriptors = [];
  }

  forEachEndpoint(callback) {
    for (let endpoint of this.endpoints) {
      callback(endpoint);
    }
  }

  // Called when one of this modules controls have received input
  // "i" is the index of the control that received input, and "e" is the event it sent
  input(i, e) { }

  destroySelf() {
    this.forEachEndpoint(endpoint => endpoint.destroySelf());
	this.endpoints = [];
	this.endpointDescriptors = [];
	this.controlDescriptors = [];
    this.elt.remove();
  }
}


class VCAModule extends Module {
  constructor() {
    super(...arguments);

    let vca = audioContext.createGain();
    vca.gain.value = 0;

    this.endpointDescriptors = [
      {
        name: "signal_in",
        direction: 'in',
        audioNode: vca
      }, {
        name: "signal_out",
        direction: 'out',
        audioNode: vca
      }, {
        name: "cv_in",
        direction: 'in',
        audioNode: vca.gain,
        maxConnections: 1
      }
    ];
  }
}

class OscillatorModule extends Module {
  constructor() {
    super(...arguments);

    this.osc = audioContext.createOscillator();
    this.osc.frequency.value = 0;
    this.osc.start();

    this.endpointDescriptors = [
      {
        name: "freq_in",
        direction: 'in',
        audioNode: this.osc.frequency,
        maxConnections: 1
      }, {
        name: "detune_in",
        direction: 'in',
        audioNode: this.osc.detune,
        maxConnection: 1
      }, {
        name: "signal_out",
        direction: 'out',
        audioNode: this.osc
      }
    ];

	this.controlDescriptors = [
	  {
		controlType: "radio",
		initParameters: [
		  [ "name", "osc-type"]
		],
		label: "sin"
	  }, {
		controlType: "radio",
		initParameters: [
		  [ "name", "osc-type"]
		],
		label: "sqr"
	  }, {
		controlType: "radio",
		initParameters: [
		  [ "name", "osc-type"]
		],
		label: "saw"
	  }, {
		controlType: "radio",
		initParameters: [
		  [ "name", "osc-type"]
		],
		label: "tri"
	  }
	];
  }

  input (i, e) {
	let oscType;
	switch(i) {
	case 0:
	  oscType = "sine";
	  break;
	case 1:
	  oscType = "square";
	  break;
	case 2:
	  oscType = "sawtooth";
	  break;
	case 3:
	  oscType = "triangle";
	  break;
	}
	this.osc.type = oscType;
  }
}

class EnvelopeModule extends Module {
  constructor() {
    super(...arguments);

    this.env = audioContext.createEnvelope();

	this.endpointDescriptors = [
	  {
		name: "gate_in",
		direction: 'in',
		audioNode: this.env.gate,
		maxConnections: 1
	  }, {
		name: "trigger_in",
		direction: 'in',
		audioNode: this.env.trigger
	  }, {
		name: "env_out",
		direction: 'out',
		audioNode: this.env
	  }
	];

	this.controlDescriptors = [
	  {
		controlType: "number",
		initParameters: [
		  [ "min", "0" ],
		  [ "max", "10"],
		  [ "step", "0.001"],
		  [ "value", "0"]
		],
		label: "attack: "
	  }, {
		controlType: "number",
		initParameters: [
		  [ "min", "0" ],
		  [ "max", "10"],
		  [ "step", "0.001"],
		  [ "value", "0"]
		],
		label: "decay: "
	  }, {
		controlType: "number",
		initParameters: [
		  [ "min", "0" ],
		  [ "max", "1"],
		  [ "step", "0.001"],
		  [ "value", "0"]
		],
		label: "sustain: "
	  }, {
		controlType: "number",
		initParameters: [
		  [ "min", "0" ],
		  [ "max", "10"],
		  [ "step", "0.001"],
		  [ "value", "0"]
		],
		label: "release: "
	  }
	];
  }

  input(i, e) {
	const val = e.target.value;
	switch(i) {
	case 0:
	  this.env.attack.setValueAtTime(val, 0);
	  break;
	case 1:
      this.env.decay.setValueAtTime(val, 0);
	  break;
	case 2:
      this.env.sustain.setValueAtTime(val, 0);
	  break;
	case 3:
      this.env.release.setValueAtTime(val, 0);
	  break;
	}
  }
}

class ConstantModule extends Module {
  constructor() {
    super(...arguments);

    this.constant = audioContext.createConstantSource();
    this.constant.start();
	this.constant.offset.value = 0.5;

	this.endpointDescriptors = [
	  {
		name: "cv_out",
		direction: 'out',
		audioNode: this.constant
	  }
	];

	this.controlDescriptors = [
	  {
		controlType: "knob",
		initParameters: []
	  }
	];
  }

  input(i, e) {
	this.constant.offset.value = e.detail;
  }
}

class KeyboardModule extends Module {
  constructor() {
    super(...arguments);

    this.frequency = audioContext.createConstantSource();
    this.frequency.offset.value = 0;
    this.frequency.start();

    this.gate = audioContext.createConstantSource();
    this.gate.offset.value = 0;
    this.gate.start();

    this.trigger = audioContext.createTrigger();

	this.gateState = false;

	this.endpointDescriptors = [
	  {
		name: "freq_out",
		direction: 'out',
		audioNode: this.frequency
	  }, {
		name: "gate_out",
		direction: 'out',
		audioNode: this.gate
	  }, {
		name: "trigger_out",
		direction: 'out',
		audioNode: this.trigger
	  }
	];

	this.controlDescriptors = [
	  {
		controlType: "keys",
		initParameters: []
	  }
	];
  }

  input(i, e) {
	const freq = e.detail;
	if (freq == 0) {
	  this.gate.offset.value = 0;
	  this.gateState = false;
	} else {
	  if (!this.gateState) {
		this.gate.offset.value = 1;
		this.gateState = true;
	  }
	  this.trigger.trigger();
	  this.frequency.offset.value = e.detail;
	}
  }
}

class DestinationModule extends Module {
  constructor() {
    super(...arguments);

	this.endpointDescriptors = [
	  {
		name: "signal_in",
		direction: 'in',
		audioNode: audioContext.destination
	  }
	];
  }
}


let ModuleClassMap = new Map([
  ["Const", ConstantModule],
  ["Keys", KeyboardModule],
  ["VCO", OscillatorModule],
  ["VCA", VCAModule],
  ["Env", EnvelopeModule],
  ["Dest", DestinationModule],
]);

export { Module, ModuleClassMap };
