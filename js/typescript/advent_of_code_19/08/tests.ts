import {
  getImageLayers,
  getImageVerificationNumber,
  getLayersResultImage
} from "./lib";

describe("getImageLayers", () => {
  it("returns the expected value", () => {
    expect(getImageLayers({ height: 2, width: 3 }, "123456789012")).toEqual([
      ["123", "456"],
      ["789", "012"]
    ]);
  });
});

describe("getImageVerificationNumber", () => {
  it("returns the expected value", () => {
    expect(
      getImageVerificationNumber({ height: 2, width: 3 }, "123456789012")
    ).toEqual(1);
  });
});

describe("getLayersResultImage", () => {
  it("returns the expected result", () => {
    expect(getLayersResultImage({ height: 2, width: 2 }, "0222112222120000"))
      .toEqual(`01
10`);
  });
});
