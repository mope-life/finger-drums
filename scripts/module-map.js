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
    this.elem.addEventListener('mousedown', e => dragEndpoint(e, this));

    switch(direction) {
      case 'in':
        this.elem.classList.add('endpoint-in');
        break;
      case 'out':
        this.elem.classList.add('endpoint-out');
        break;
    }

    parent.elem.appendChild(this.elem);
  }

  connectAtCursor(line) {
    let hovered = Array.from(document.querySelectorAll(':hover'));
    let targetClass = this.direction == 'in'? 'endpoint-out': 'endpoint-in';

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

  snapLines() {
    let midpoint = getMidpoint(this.elem);
    this.connections.forEach(c => {
      snapToPoint(c.line, midpoint.x, midpoint.y, this.direction);
    });
  }
}

class Module {
  constructor(prototype, parent) {
    this.parentMap = parent;
    this.type = prototype;

    this.elem = document.createElement('div');
    this.elem.classList.add('module');

    this.endpointIn = new Endpoint('in', this);
    this.endpointOut = new Endpoint('out', this);
  }

  snapLines() {
    this.endpointIn.snapLines();
    this.endpointOut.snapLines();
  }
}

class ModuleMap {
  constructor(elem) {
    this.elem = elem;
    this.modules = [];

    this.buildConnectionMap();
    this.buildModuleBank();
  }

  buildConnectionMap() {
    let styles = window.getComputedStyle(this.elem);
    this.connectionMap = document.createElementNS("http://www.w3.org/2000/svg", 'svg');
    this.connectionMap.setAttribute('width', styles.getPropertyValue('width'));
    this.connectionMap.setAttribute('height', styles.getPropertyValue('height'));

    this.elem.parentNode.appendChild(this.connectionMap);
  }

  buildModuleBank() {
    this.moduleBank = document.createElement('div');
    this.moduleBank.classList.add('module-bank');

    let modulePrototypes = this.elem.dataset.modulePrototypes.split(',');
    modulePrototypes.forEach((prototype, i) => {
      let protoElem = document.createElement('div');
      protoElem.innerHTML = prototype;
      protoElem.classList.add('module-prototype');
      protoElem.style.backgroundColor = interpolatedColorString(i, modulePrototypes.length);
      this.moduleBank.appendChild(protoElem);

      protoElem.addEventListener('mousedown', e => {
        if (e.which != 1) return e; // only catch left clicks
        e.preventDefault();
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

    modElem.addEventListener('mousedown', function(e) {
      if (e.which != 1) return e; // only catch left clicks
      e.preventDefault();
      dragModule(mod, e.clientX, e.clientY);
    });
    dragModule(mod, e.clientX, e.clientY);
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

function dragModule(mod, startX, startY) {
  rect = mod.elem.getBoundingClientRect();
  let xOffset = rect.left - startX;
  let yOffset = rect.top - startY;

  let drag = function(e) {
    e.preventDefault();
    mod.elem.style.left = (e.clientX + xOffset) + "px";
    mod.elem.style.top = (e.clientY + yOffset) + "px";
    mod.snapLines();
  }

  let endDrag = function(e) {
    e.preventDefault();
    document.removeEventListener('mousemove', drag);
    document.removeEventListener('mouseup', endDrag);
  }

  document.addEventListener('mousemove', drag);
  document.addEventListener('mouseup', endDrag);
}

function dragEndpoint(e, endpoint) {
  e.preventDefault();
  e.stopPropagation();
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
    document.removeEventListener('mousemove', drag);
    document.removeEventListener('mouseup', endDrag);

    endpoint.connectAtCursor(line);
  }

  document.addEventListener('mousemove', drag);
  document.addEventListener('mouseup', endDrag);
}

document.addEventListener('DOMContentLoaded', function() {
  maps = document.getElementsByClassName('module-map');
  for (m of maps) {
    new ModuleMap(m);
  }
});
