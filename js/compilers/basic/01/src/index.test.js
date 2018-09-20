const { compile } = require(".");

describe("compile", () => {
  it("returns the expected results", () => {
    expect(compile("").trim()).toEqual(
      '<svg width="100" height="100" viewBox="0 0 100 100" xmlns="http://www.w3.org/2000/svg" version="1.1"></svg>'.trim()
    );

    expect(compile("Paper 20").trim()).toEqual(
      `<svg width="100" height="100" viewBox="0 0 100 100" xmlns="http://www.w3.org/2000/svg" version="1.1">
<rect x="0" y="0" width="100" height="100" fill="rgb(80%,80%,80%)"></rect>
</svg>`.trim()
    );
  });

  it("throws when expected", () => {
    expect(() => compile("Paper 10 20")).toThrow(
      "Paper comman can only have one number"
    );
    expect(() => compile("Paper")).toThrow("Unexpected end of input");
    expect(() => compile("Paper foo")).toThrow(
      "Paper command must be followed by a number."
    );
  });
});
