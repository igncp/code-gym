<template>
  <div>
    <h1>Dummy Component</h1>
    <component :is="dynamicComponent">Dynamic Component</component>
    <dummy-b></dummy-b>
  </div>
</template>

<script>
const CompSpan = {
  name: "comp-span",
  render(h) {
    return h("span", this.$slots.default);
  }
};

const DummyB = {
  name: "dummy-b",
  render(h) {
    return h("p", "DummyB Content");
  }
};

const DummyA = {
  name: "dummy-a",
  components: { DummyB },
  data: () => ({
    val1: 1,
    dynamicComponent: CompSpan
  }),
  computed: {
    double() {
      return this.val1 * 2;
    }
  }
};

export default DummyA;
</script>
