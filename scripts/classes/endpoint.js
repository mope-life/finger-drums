class Endpoint {
  constructor(elt, parentModule, audioNode, maxConnections) {
    this.elt = elt;
    this.parentModule = parentModule;
    this.audioNode = audioNode;

    this.connections = new Map();
    this.maxConnections = maxConnections;
  }

  forEachConnection(callback) {
    for (let [other, lineElt] of this.connections) {
      callback(other, lineElt);
    }
  }

  addConnection(other, lineElt) {
    this.connections.set(other, lineElt);
  }

  removeConnection(other) {
    this.connections.delete(other);
  }

  connectedTo(other) {
	return this.connections.has(other);
  }

  canConnectTo(other) {
    return (
      this.direction != other.direction
		&& this.maxConnections > this.connections.size
		&& other.maxConnections > other.connections.size
		&& this.parentModule != other.parentModule
		&& !this.connectedTo(other)
    );
  }

  destroySelf() {
    this.forEachConnection((other, lineElt) => {
      other.removeConnection(this);
      lineElt.remove();
    });
    this.connections.clear();

	this.elt.remove();

    if (typeof this.audioNode.stop === "function") {
      this.audioNode.stop();
    }
  }
}

class EndpointIn extends Endpoint {
  constructor() {
	super(...arguments);
	this.direction = "in";
	this.oppositeDirection = "out";
  }
}

class EndpointOut extends Endpoint {
  constructor() {
	super(...arguments);
	this.direction = "out";
	this.oppositeDirection = "in";
  }

  addConnection(other) {
	this.audioNode.connect(other.audioNode);
	super.addConnection(...arguments);
  }

  removeConnection(other) {
    this.audioNode.disconnect(other.audioNode);
	super.removeConnection(...arguments);
  }
}

export { EndpointIn, EndpointOut };
