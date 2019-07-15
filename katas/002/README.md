# Shortest path in graph using Dijkstra

## Pseudo Code

1. Loop all the existing nodes and map their ids to `Infinite` weight (Times map)
1. Set the starting node weight to `0`
1. Create a priority queue
1. Enqueue the starting node
1. While the priority queue is not empty
    1. Get the first item of the priority queue
    1. Calculate the time for each neighbour by adding the current node weight plus the weight of the edge
    1. If the time is smaller than the registered one, override it and add the node to the queue (including the new time).
        - The node should be positioned in the queue depending on this time, with lower times to the left
        - If you need to return the path at the end, map the neighbour id to the current node

## Implementations

- [TypeScript](./ts/)
