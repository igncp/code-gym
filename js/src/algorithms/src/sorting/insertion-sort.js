module.exports = (values) => {

  for (let i = 1; i < values.length; ++i) {

    const temp = values[i]
    let j = i - 1

    for (; j >= 0 && values[j] > temp; --j) {

      values[j + 1] = values[j]

    }

    values[j + 1] = temp

  }

  return values

}
