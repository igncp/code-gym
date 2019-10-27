// https://vue-test-utils.vuejs.org/api

import { mount } from "@vue/test-utils";

import Counter from "../counter";
import DummyA from "../DummyA.vue";
import ListComp from "../List.vue";

describe("Counter", () => {
  // Now mount the component and you have the wrapper
  const wrapper = mount(Counter);

  it("renders the correct markup", () => {
    expect(wrapper.html()).toContain('<span class="count">0</span>');
  });

  // it's also easy to check for the existence of elements
  it("has a button", () => {
    expect(wrapper.contains("button")).toBe(true);
  });

  it("button should increment the count", () => {
    expect(wrapper.vm.count).toBe(0);
    const button = wrapper.find("button");
    button.trigger("click");
    expect(wrapper.vm.count).toBe(1);
  });

  it("contains the Increment word", () => {
    expect(wrapper.html()).toContain("Increment");
  });
});

describe("DummyA", () => {
  const wrapper = mount(DummyA);

  it("renders the expected content", () => {
    expect(wrapper.html()).toContain("<h1>Dummy Component</h1>");
    expect(wrapper.html()).toContain("DummyB Content");
    expect(wrapper.html()).toContain("<span>Dynamic Component</span>");
  });

  it("has the expected computed dependency", () => {
    expect(wrapper.vm.val1).toEqual(1);
    expect(wrapper.vm.double).toEqual(2);

    wrapper.vm.val1 = 10;
    expect(wrapper.vm.double).toEqual(20);
  });
});

describe("ListComp", () => {
  let wrapper;

  beforeEach(() => {
    wrapper = mount(ListComp);
  });

  it("renders the expecte components", () => {
    expect(wrapper.html()).toContain("<h1>List:</h1>");
  });

  it("renders several items", () => {
    expect(wrapper.html()).not.toContain("Foo");
    expect(wrapper.html()).not.toContain("Bar");

    wrapper.setProps({
      items: [{ id: "foo", content: "Foo" }, { id: "bar", content: "Bar" }]
    });

    expect(wrapper.html()).toContain("Foo");
    expect(wrapper.html()).toContain("Bar");
  });

  it("has the expected classes", () => {
    expect(wrapper.classes()).toEqual([]);
  });

  it("has the expected emitted events", () => {
    expect(wrapper.emitted()).toEqual({});
  });

  it("renders a div as the top element", () => {
    expect(wrapper.is("div")).toEqual(true);
  });
});
