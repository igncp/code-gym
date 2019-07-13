const assert = require("assert");

type NodeId = string;
type Weight = number;
type QueueElement = [NodeId, Weight];

class PriorityQueue {
  private collection: Array<QueueElement>;

  constructor() {
    this.collection = [];
  }

  enqueue(element: QueueElement) {
    const [_, weight] = element;

    for (let i = 1; i <= this.collection.length; i++) {
      if (weight < this.collection[i - 1][1]) {
        this.collection.splice(i - 1, 0, element);

        return;
      }
    }

    this.collection.push(element);
  }

  dequeue() {
    return this.collection.shift();
  }

  isEmpty() {
    return this.collection.length === 0;
  }
}

interface DijkstraResult {
  dequeueOrder: Array<NodeId>;
  paths: {
    [nodeId: string]: {
      path: Array<NodeId>;
      total: Weight;
    };
  };
}

class Graph {
  private nodes: Array<string>;
  private edges: { [k: string]: { [a: string]: Weight } };

  constructor() {
    this.nodes = [];
    this.edges = {};
  }

  addNodes(nodes: Array<NodeId>) {
    nodes.forEach(node => {
      this.nodes.push(node);
      this.edges[node] = {};
    });
  }

  addEdge(node1: NodeId, node2: NodeId, weight: Weight) {
    this.edges[node1][node2] = weight;
  }

  findPathWithDijkstra(startNode): DijkstraResult {
    let times: { [nodeId: string]: Weight } = {};
    let backtrace: { [nodeId: string]: NodeId } = {};
    let pq = new PriorityQueue();

    times[startNode] = 0;

    this.nodes.forEach(node => {
      if (node !== startNode) {
        times[node] = Infinity;
      }
    });

    pq.enqueue([startNode, 0]);
    const dequeueOrder: Array<NodeId> = [];

    while (!pq.isEmpty()) {
      const [currentNode] = pq.dequeue();
      dequeueOrder.push(currentNode);

      Object.keys(this.edges[currentNode]).forEach(neighborId => {
        const neighborWeight = this.edges[currentNode][neighborId];
        let time = times[currentNode] + neighborWeight;

        if (time < times[neighborId]) {
          times[neighborId] = time;
          backtrace[neighborId] = currentNode;

          pq.enqueue([neighborId, time]);
        }
      });
    }

    const paths = this.nodes.reduce((acc, endNode) => {
      acc[endNode] = { path: [], total: 0 };

      if (endNode !== startNode) {
        const path = [endNode];

        let lastStep = endNode;
        let fullPath = true;

        while (lastStep !== startNode) {
          lastStep = backtrace[lastStep];

          if (!lastStep) {
            fullPath = false;
            break;
          }

          path.unshift(lastStep);
        }

        if (fullPath) {
          acc[endNode] = {
            path,
            total: times[endNode]
          };
        } else {
          acc[endNode] = null;
        }
      }

      return acc;
    }, {});

    return {
      dequeueOrder: dequeueOrder.filter(
        (n, idx, arr) => idx === arr.indexOf(n)
      ),
      paths
    };
  }
}

const main = () => {
  const graph = new Graph();
  graph.addNodes(["A", "B", "C", "D", "E", "F", "G", "H", "I"]);

  graph.addEdge("A", "B", 15);
  graph.addEdge("A", "C", 13);
  graph.addEdge("A", "D", 5);

  graph.addEdge("B", "H", 12);

  graph.addEdge("C", "B", 2);
  graph.addEdge("C", "F", 6);
  graph.addEdge("C", "D", 18);

  graph.addEdge("D", "E", 4);
  graph.addEdge("D", "I", 99);

  graph.addEdge("E", "C", 3);
  graph.addEdge("E", "F", 1);
  graph.addEdge("E", "G", 9);
  graph.addEdge("E", "I", 14);

  graph.addEdge("F", "B", 8);
  graph.addEdge("F", "H", 17);

  graph.addEdge("G", "F", 16);
  graph.addEdge("G", "H", 7);
  graph.addEdge("G", "I", 10);

  graph.addEdge("I", "H", 11);

  const result = graph.findPathWithDijkstra("C");

  // This solutions are not confirmed with an official result
  assert.deepEqual(result.paths.A, null);
  assert.deepEqual(result.paths.D, { path: ["C", "D"], total: 18 });
  assert.deepEqual(result.dequeueOrder, [
    "C",
    "B",
    "F",
    "H",
    "D",
    "E",
    "G",
    "I"
  ]);
};

main();
