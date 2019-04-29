<div
  on:mousemove={handleMouseMove}
  on:mouseup={handleMouseUp}
  class="wrapper"
  bind:this={wrapperNode}
>
  <p style="margin: 0px;">
    <button on:click={onCreateItem} disabled="{shapes.length > 9}">
      Create Item (max 10)
    </button>
    <button on:click={handleClear}>
        Clear
    </button>
  </p>
  {#each shapes as shape}
  <div
    class="shape"
    on:mousedown={() => handleMouseDown(shape)}
    style="left: {shape.x}px; top: {shape.y}px"
  >
    Item: {shape.id}
  </div>
  {/each}
</div>

<script>
let shapes = [];
let selectedItem;
let wrapperNode;

function onCreateItem() {
    const maxWidth = wrapperNode.offsetWidth - 100;
    const maxHeight = wrapperNode.offsetHeight - 100;
  const newShapes = shapes.concat([{x: Math.random() * maxWidth, y: Math.random() * maxHeight, id: shapes.length}]);

  shapes = newShapes.slice(0);
}

function handleMouseDown(item) {
  selectedItem = item;
}

function handleMouseMove(ev) {
  if (selectedItem) {
    const fn = () => {
      const {id} = selectedItem;
      const idx = shapes.map((s) => s.id).indexOf(id);

      shapes[idx].x = ev.clientX;
      shapes[idx].y = ev.clientY;

      shapes = shapes.slice(0);
    };

    window.requestAnimationFrame(fn);
  }
}

function handleMouseUp() {
  selectedItem = undefined;
}

function handleClear() {
  shapes = [];
}
</script>

<style>
  .shape {
    background: white;
    border: 1px solid #333;
    cursor: default;
    display: inline-block;
    padding: 20px;
    position: absolute;
  }

  .wrapper {
    height: 100%;
    user-select: none;
    width: 100%;
  }
</style>
