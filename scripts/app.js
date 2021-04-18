SPECTRUM_START = {r:255, g:100, b:100};
SPECTRUM_END = {r:100, g:100, b:255};

function interpolatedColorString(i, n) {
  let v = ['r','g','b'].map(h =>
    SPECTRUM_START[h] + (SPECTRUM_END[h]-SPECTRUM_START[h]) * i / (n-1));
  return `rgb(${v[0]}, ${v[1]}, ${v[2]})`;
}

function getMidpoint(elem) {
  let rect = elem.getBoundingClientRect();
  let x = rect.left + (rect.right - rect.left) / 2;
  let y = rect.top + (rect.bottom - rect.top) / 2;
  return { x:x, y:y };
}

function snapToPoint(line, x, y, direction) {
  switch(direction) {
    case 'out':
    line.setAttribute('x1', x);
    line.setAttribute('y1', y);
    break;
    case 'in':
    line.setAttribute('x2', x);
    line.setAttribute('y2', y);
    break;
  }
}

function opposite(direction) {
  return direction == 'in'? 'out': 'in';
}

class Endpoint {
  constructor(direction, parent) {
    this.parentModule = parent;
    this.parentMap = parent.parentMap;
    this.direction = direction;
    this.connections = [];

    this.elem = document.createElement('div');
    this.elem.addEventListener('pointerdown', e => {
      if (e.which != 1) return e; // only catch left clicks
      dragEndpoint(e, this);
    });

    switch(direction) {
      case 'in':
        this.elem.classList.add('endpoint-in');
        break;
      case 'out':
        this.elem.classList.add('endpoint-out');
        break;
    }
    this.elem.classList.add('grabbable');

    parent.elem.appendChild(this.elem);
  }

  connectAtCursor(line) {
    let hovered = Array.from(document.querySelectorAll(':hover'));
    let targetClass = 'endpoint-' + opposite(this.direction);

    let destination;
    if (hovered.find(el => {
      if (el.classList.contains(targetClass)) {
        destination = this.parentMap.findEndpoint(el, opposite(this.direction));
        return this.isValidConnection(destination);
      } else {
        return false;
      }})) {
      this.connections.push({endpoint:destination, line:line});
      destination.connections.push({endpoint:this, line:line});
      destination.snapLines();

      line.addEventListener('click', e => {
        this.removeConnection(destination);
        destination.removeConnection(this);
        line.remove();
      });
    } else {
      line.remove();
    }
  }

  isValidConnection(destination) {
    return  destination
            && this.parentModule != destination.parentModule
            && !this.connectedTo(destination)
            && !destination.connectedTo(this);
  }

  connectedTo(endpoint) {
    return this.connections.find(c => c.endpoint == endpoint);
  }

  removeConnection(endpoint) {
    this.connections = this.connections.filter(c => c.endpoint != endpoint);
  }

  snapLines() {
    let midpoint = getMidpoint(this.elem);
    this.connections.forEach(c => {
      snapToPoint(c.line, midpoint.x, midpoint.y, this.direction);
    });
  }

  destroySelf() {
    this.connections.forEach(c => {
      c.endpoint.removeConnection(this);
      c.line.remove();
    });
  }
}

class Module {
  constructor(prototype, parent) {
    this.parentMap = parent;
    this.type = prototype;

    this.elem = document.createElement('div');
    this.elem.classList.add('module');
    this.elem.classList.add('grabbable');

    this.endpointIn = new Endpoint('in', this);
    this.endpointOut = new Endpoint('out', this);
  }

  snapLines() {
    this.endpointIn.snapLines();
    this.endpointOut.snapLines();
  }

  destroySelf() {
    this.endpointIn.destroySelf();
    this.endpointOut.destroySelf();
    this.elem.remove();
  }
}

class ModuleMap {
  constructor(elem) {
    this.elem = elem;
    this.modules = [];

    this.buildConnectionMap();
    this.buildModuleBank();
    this.buildTrash();
  }

  buildConnectionMap() {
    this.connectionMap = document.createElementNS("http://www.w3.org/2000/svg", 'svg');

    let styles = window.getComputedStyle(this.elem);
    this.connectionMap.setAttribute('width', styles.getPropertyValue('width'));
    this.connectionMap.setAttribute('height', styles.getPropertyValue('height'));

    this.elem.parentNode.appendChild(this.connectionMap);
  }

  buildTrash() { // ;)
    this.trash = document.createElement('div');
    this.trash.classList.add('trash');
    this.trash.innerHTML = 'trash';

    this.elem.parentNode.appendChild(this.trash);
  }

  buildModuleBank() {
    this.moduleBank = document.createElement('div');
    this.moduleBank.classList.add('module-bank');

    let modulePrototypes = this.elem.dataset.modulePrototypes.split(',');
    modulePrototypes.forEach((prototype, i) => {
      let protoElem = document.createElement('div');
      protoElem.innerHTML = prototype;
      protoElem.classList.add('module-prototype');
      protoElem.classList.add('grabbable');
      protoElem.style.backgroundColor = interpolatedColorString(i, modulePrototypes.length);
      this.moduleBank.appendChild(protoElem);

      protoElem.addEventListener('pointerdown', e => {
        if (e.which != 1) return e; // only catch left clicks
        this.makeModule(e, prototype)
      });
    });

    this.elem.appendChild(this.moduleBank);
  }

  makeModule(e, prototype) {
    let mod = new Module(prototype, this);
    this.modules.push(mod);

    let modElem = mod.elem;
    this.elem.appendChild(modElem);

    let rect = e.target.getBoundingClientRect();
    modElem.style.left = rect.left + "px";
    modElem.style.top = rect.top + "px";
    modElem.style.backgroundColor = e.target.style.backgroundColor;

    modElem.addEventListener('pointerdown', function(e) {
      if (e.which != 1) return e; // only catch left clicks
      dragModule(e, mod);
    });

    dragModule(e, mod);
  }

  findEndpoint(el, direction) {
    switch(direction) {
      case 'in':
      return this.modules.find(m => m.endpointIn.elem == el).endpointIn;
      case 'out':
      return this.modules.find(m => m.endpointOut.elem == el).endpointOut;
    }
  }
}

function toggleGrabbingCursor() {
  document.body.classList.toggle('grabbing');
  for (el of document.getElementsByClassName('grabbable')) {
    el.classList.toggle('grabbing');
  }
}

function dragModule(e, mod) {
  e.preventDefault();
  e.stopPropagation();
  toggleGrabbingCursor();

  rect = mod.elem.getBoundingClientRect();
  let xOffset = rect.left - e.clientX;
  let yOffset = rect.top - e.clientY;

  let drag = function(e) {
    e.preventDefault();
    mod.elem.style.left = (e.clientX + xOffset) + "px";
    mod.elem.style.top = (e.clientY + yOffset) + "px";
    mod.snapLines();
  }

  let endDrag = function(e) {
    e.preventDefault();
    document.removeEventListener('pointermove', drag);
    document.removeEventListener('pointerup', endDrag);
    toggleGrabbingCursor();

    if (document.querySelector('.trash:hover')) {
      mod.destroySelf();
    }
  }

  document.addEventListener('pointermove', drag);
  document.addEventListener('pointerup', endDrag);
}

function dragEndpoint(e, endpoint) {
  e.preventDefault();
  e.stopPropagation();
  toggleGrabbingCursor();

  let line = document.createElementNS("http://www.w3.org/2000/svg", 'line');
  let midpoint = getMidpoint(e.target);
  snapToPoint(line, midpoint.x, midpoint.y, endpoint.direction);

  let drag = function(e) {
    e.preventDefault();
    snapToPoint(line, e.clientX, e.clientY, opposite(endpoint.direction));
  }
  drag(e);
  endpoint.parentMap.connectionMap.appendChild(line);

  let endDrag = function(e) {
    e.preventDefault();
    document.removeEventListener('pointermove', drag);
    document.removeEventListener('pointerup', endDrag);
    toggleGrabbingCursor();

    line.style.pointerEvents = "all";
    endpoint.connectAtCursor(line);
  }

  document.addEventListener('pointermove', drag);
  document.addEventListener('pointerup', endDrag);
}

document.addEventListener('DOMContentLoaded', function() {
  maps = document.getElementsByClassName('module-map');
  for (m of maps) {
    new ModuleMap(m);
  }
});
