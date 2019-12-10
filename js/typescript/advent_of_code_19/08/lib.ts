type ImageDimensions = {
  width: number;
  height: number;
};

type ImageLayer = string[];

type ImageData = string;

type GetImageLayers = (i: ImageDimensions, data: ImageData) => ImageLayer[];

const getImageLayers: GetImageLayers = ({ height, width }, data) => {
  const layers: ImageLayer[] = [];
  const layerDigits = width * height;
  const layersNum = data.length / layerDigits;

  for (let layerIdx = 0; layerIdx < layersNum; layerIdx += 1) {
    const layer: ImageLayer = [];
    const digitsForThisLayer = data.substr(layerIdx * layerDigits, layerDigits);

    for (let layerRowIdx = 0; layerRowIdx < height; layerRowIdx += 1) {
      layer.push(digitsForThisLayer.substr(layerRowIdx * width, width));
    }

    layers.push(layer);
  }

  return layers;
};

type GetNumOfDigitsInLayer = (layer: ImageLayer, digit: string) => number;

const getNumOfDigitsInLayer: GetNumOfDigitsInLayer = (layer, digit) => {
  return layer.reduce((acc, row) => {
    let rowDigits = 0;

    for (let chIdx = 0; chIdx < row.length; chIdx += 1) {
      const ch = row[chIdx];

      if (ch === digit) {
        rowDigits += 1;
      }
    }

    return acc + rowDigits;
  }, 0);
};

type GetImageVerificationNumber = (
  dimensions: ImageDimensions,
  data: string
) => number;

const getImageVerificationNumber: GetImageVerificationNumber = (
  dimensions,
  data
) => {
  const layers = getImageLayers(dimensions, data.trim());
  const layerWithLeastZeros = layers.reduce(
    (acc, layer) => {
      const zeros = getNumOfDigitsInLayer(layer, "0");

      if (zeros < acc.zeros) {
        return {
          layer,
          zeros
        };
      }

      return acc;
    },
    { layer: layers[0], zeros: Infinity }
  ).layer;

  const oneDigits = getNumOfDigitsInLayer(layerWithLeastZeros, "1");
  const twoDigits = getNumOfDigitsInLayer(layerWithLeastZeros, "2");

  return oneDigits * twoDigits;
};

type GetLayersResultImage = (
  dimensions: ImageDimensions,
  data: string
) => string;

const getLayersResultImage: GetLayersResultImage = (dimensions, data) => {
  const layers = getImageLayers(dimensions, data);
  let result = "";

  for (let rowIdx = 0; rowIdx < dimensions.height; rowIdx += 1) {
    for (let columnIdx = 0; columnIdx < dimensions.width; columnIdx += 1) {
      const digit = layers.reduce((result, layer) => {
        if (result !== "2") {
          return result;
        }

        return layer[rowIdx][columnIdx];
      }, "2");

      if (columnIdx === 0) {
        result += "\n";
      }

      result += digit;
    }
  }

  return result.trim();
};

export { getImageLayers, getImageVerificationNumber, getLayersResultImage };
