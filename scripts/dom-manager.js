const MODULE_MAP_HTML_ID = "module-map";
const CONNECTION_MAP_HTML_ID = "connection-map";
const TRASH_CAN_HTML_ID = "trash-can";

const MODULE_PROTOTYPE_HTML_CLASS = "module-prototype";
const MODULE_WRAPPER_HTML_CLASS = "module-wrapper";
const ENDPOINT_BANK_HTML_CLASS = "endpoint-bank";
const CONTROL_BANK_HTML_CLASS = "control-bank";

const ENDPOINT_WRAPPER_HTML_CLASS  = "endpoint-wrapper";
const ENDPOINT_SOCKET_HTML_CLASS = "endpoint-socket";
const ENDPOINT_LABEL_HTML_CLASS = "endpoint-label";

const GRABBABLE_HTML_CLASS = "grabbable";

// import "./elements/knob.js";
import "./elements/keys.js";

// simple helper function for cleanliness
function classedElement(type, ...classes) {
  let elem = document.createElement(type);
  elem.classList.add(...classes);
  return elem;
}

const DOMManager = {
  get moduleMapElement() {
	return document.getElementById(MODULE_MAP_HTML_ID);
  },

  get trashCanElement() {
	return document.getElementById(TRASH_CAN_HTML_ID);
  },

  get connectionMapElement() {
	return document.getElementById(CONNECTION_MAP_HTML_ID);
  },

  get hovered() {
	return Array.from(document.querySelectorAll(':hover'));
  },

  createModulePrototypeElement: function(moduleClassKey) {
	return classedElement('div', MODULE_PROTOTYPE_HTML_CLASS, GRABBABLE_HTML_CLASS);
  },

  createModuleElement: function() {
	let elt = classedElement('div', MODULE_WRAPPER_HTML_CLASS, GRABBABLE_HTML_CLASS);
	elt.append(
	  classedElement('div', ENDPOINT_BANK_HTML_CLASS),
	  classedElement('div', CONTROL_BANK_HTML_CLASS),
	  classedElement('div', ENDPOINT_BANK_HTML_CLASS)
	);

	return elt;
  },

  createEndpointElement: function() {
	let elt = classedElement('div', ENDPOINT_WRAPPER_HTML_CLASS);
	elt.append(
	  classedElement('div', ENDPOINT_SOCKET_HTML_CLASS, GRABBABLE_HTML_CLASS),
	  classedElement('label', ENDPOINT_LABEL_HTML_CLASS)
	);

	return elt;
  },

  createLine: function() {
	const line = document.createElementNS("http://www.w3.org/2000/svg", 'line');
	line.setAttribute("stroke-linecap", "round");
	return line;
  },

  createControl: function(controlType, initParameters, label) {
	let el;

	switch(controlType) {
	case "knob":
	  el = document.createElement("control-knob");
	  break;
	case "keys":
	  el = document.createElement("control-keys");
	  break;
	case "number":
	  el = document.createElement("input");
	  el.type = "number";
	  el.addEventListener("pointerdown", e => e.stopPropagation() );
	  el.addEventListener("wheel", _ => {
		// no-op allows incrementing with wheel
		// Some browsers won't register wheel events if there is no listener
	  });

	  break;
	case "radio":
	  el = document.createElement("input");
	  el.type = "radio";
	  el.addEventListener("pointerdown", e => e.stopPropagation() );

	  break;
	}

	for ( let [ param, value ] of initParameters ) {
 	  el[param] = value;
	}

	if (label) {
	  const labelElement = classedElement("label", controlType + "-label");
	  labelElement.innerHTML = label;
	  labelElement.appendChild(el);
	  labelElement.addEventListener("pointerdown", e => e.stopPropagation() );
	}

	return el;
  }
};

export { DOMManager };
