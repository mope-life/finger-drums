const GRABBING_HTML_CLASS = "grabbing";

import { DOMManager } from "./dom-manager.js";
import { Module, ModuleClassMap } from "./classes/module.js";
import { EndpointIn, EndpointOut } from "./classes/endpoint.js";


/* Store and find the modules or endpoints associated with DOM elements */
const moduleMap = new Map();
const endpointMap = new Map();

var app = Elm.AudioModuleMap.init({
  node: document.getElementById("audio-module-map")
});

/* Helper functions for moving items during clicks-and-drags */
function elementMidpoint(elt) {
  let rect = elt.getBoundingClientRect();
  let x = 0.5 * (rect.right - rect.left);
  let y = 0.5 * (rect.bottom - rect.top);
  return { x, y };
}

function clientMidpoint(elt) {
  let rect = elt.getBoundingClientRect();
  let x = 0.5 * (rect.left + rect.right);
  let y = 0.5 * (rect.top + rect.bottom);
  return { x, y };
}

function moveTo(elt, x, y, offsetX = 0, offsetY = 0) {
  let midpoint = elementMidpoint(elt);
  elt.style.left = (x - (midpoint.x + offsetX)) + "px";
  elt.style.top = (y - (midpoint.y + offsetY)) + "px";
}

function snapLineEnd(elt, x, y, whichEnd) {
  switch(whichEnd) {
  case 'in':
	elt.setAttribute('x2', x);
	elt.setAttribute('y2', y);
	break;
  case 'out':
	elt.setAttribute('x1', x);
	elt.setAttribute('y1', y);
	break;
  }
}


Module.prototype.addEndpoints = function() {
  for (let { name, direction, audioNode, maxConnections = Number.MAX_SAFE_INTEGER } of this.endpointDescriptors) {
	let endpointElt = DOMManager.createEndpointElement();
	endpointElt.dataset.direction = direction;
	endpointElt.lastElementChild.innerHTML = name;
	endpointElt.firstElementChild.addEventListener('pointerdown', endpointPointerDownHandler);

	let endpoint;
	if (direction == 'in') {
	  this.elt.firstElementChild.appendChild(endpointElt);
	  endpoint = new EndpointIn(endpointElt, this, audioNode, maxConnections);
	} else if (direction == 'out') {
	  this.elt.lastElementChild.appendChild(endpointElt);
	  endpoint = new EndpointOut(endpointElt, this, audioNode, maxConnections);
	}
	this.endpoints.push(endpoint);
	endpointMap.set(endpointElt, endpoint);
  }
};

Module.prototype.addControls = function() {
  const controlDiv = this.elt.firstElementChild.nextElementSibling;

  for (let i = 0; i < this.controlDescriptors.length; ++i) {
	let { controlType, initParameters, label } = this.controlDescriptors[i];
	const controlElt = DOMManager.createControl(controlType, initParameters, label);

	if (label) {
	  controlDiv.appendChild(controlElt.parentElement);
	} else {
	  controlDiv.appendChild(controlElt);
	}

	controlElt.addEventListener('input', e => this.input(i, e));
  }
};

/* Creates and drags new Module upon clicking a prototype. */
function prototypePointerDownHandler(e) {
  e.preventDefault();
  e.stopPropagation();

  let moduleClassKey = this.dataset.moduleClassKey;
  let moduleElt = DOMManager.createModuleElement();
  let mod = new (ModuleClassMap.get(moduleClassKey))(moduleElt, moduleMap);

  mod.addEndpoints();
  mod.addControls();

  moduleMap.set(moduleElt, mod);

  DOMManager.moduleMapElement.appendChild(moduleElt);

  moveTo(moduleElt, e.clientX, e.clientY);

  moduleElt.addEventListener('pointerdown', modulePointerDownHandler, false);

  // Initiate click-and-drag on the new Module
  moduleElt.dispatchEvent(new PointerEvent('pointerdown', {
    clientX: e.clientX,
    clientY: e.clientY,
    cancelable: true
  }));
}


/* Click-and-drag handler for Module elements. */
function modulePointerDownHandler(e) {
  const originalTarget = this;
  const targetModule = moduleMap.get(originalTarget);

  originalTarget.classList.add(GRABBING_HTML_CLASS);

  // Move this module to the front
  DOMManager.moduleMapElement.appendChild(originalTarget);

  const midpoint = clientMidpoint(originalTarget);
  const offsetX = e.clientX - midpoint.x;
  const offsetY = e.clientY - midpoint.y;

  let canceled = false;
  let callback = function (e1) {
	e1.stopPropagation();
	e1.preventDefault();
    if (!canceled) {
	  moveTo(originalTarget, e1.clientX, e1.clientY, offsetX, offsetY);
	  targetModule.forEachEndpoint(endpoint => {
		endpoint.forEachConnection((other, lineElt) => {
		  let midpoint = clientMidpoint(endpoint.elt.firstElementChild);
		  snapLineEnd(lineElt, midpoint.x, midpoint.y, endpoint.direction);
		});
	  });
      document.addEventListener('pointermove', e2 => {
        requestAnimationFrame( _ => callback(e2) );
      }, { once: true });
    }
    else {
      originalTarget.classList.remove(GRABBING_HTML_CLASS);
      if (DOMManager.trashCanElement.matches(":hover")) {
        targetModule.destroySelf();
      }
    }
  };

  document.addEventListener('pointerup', e1 => {
    canceled = true;
  }, { once: true });

  callback(e);
}


/* Click and drag handler for Endpoint elements. */
function endpointPointerDownHandler(e) {
  const originalTarget = this.parentNode;
  const targetEndpoint = endpointMap.get(originalTarget);

  let lineElt = DOMManager.createLine();

  originalTarget.classList.add(GRABBING_HTML_CLASS);
  lineElt.classList.add(GRABBING_HTML_CLASS);

  let midpoint = clientMidpoint(originalTarget.firstElementChild);
  snapLineEnd(lineElt, midpoint.x, midpoint.y, targetEndpoint.direction);

  DOMManager.connectionMapElement.appendChild(lineElt);

  let canceled = false;
  const callback = function (e1) {
    e1.preventDefault();
    e1.stopPropagation();

    if (!canceled) {
      snapLineEnd(lineElt, e1.clientX, e1.clientY, targetEndpoint.oppositeDirection);
      document.addEventListener('pointermove', e2 => {
        requestAnimationFrame( _ => callback(e2) );
      }, { once: true });
    }
    else {
      originalTarget.classList.remove(GRABBING_HTML_CLASS);
      lineElt.classList.remove(GRABBING_HTML_CLASS);
      lineElt.style.pointerEvents = 'all';

      const el = DOMManager.hovered.find(el => el.dataset.direction == targetEndpoint.oppositeDirection);
	  const other = el && endpointMap.get(el);

      if (other && targetEndpoint.canConnectTo(other)) {
        targetEndpoint.addConnection(other, lineElt);
		other.addConnection(targetEndpoint, lineElt);

		let otherMidpoint = clientMidpoint(other.elt.firstElementChild);
		snapLineEnd(lineElt, otherMidpoint.x, otherMidpoint.y, other.direction);

		lineElt.addEventListener('click', _ => {
		  targetEndpoint.removeConnection(other);
		  other.removeConnection(targetEndpoint);
		  lineElt.remove();
		});
      } else {
        lineElt.remove();
      }
    }
  };

  document.addEventListener('pointerup', e1 => {
    canceled = true;
  }, { once: true });

  callback(e);
}
