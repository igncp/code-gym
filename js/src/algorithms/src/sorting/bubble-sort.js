module.exports = (arr) => {

  for (let i = 1; i < arr.length; ++i) {

    const temp = arr[i]
    let j = i - 1

    for (; j >= 0 && arr[j] > temp; --j) {

      arr[j + 1] = arr[j]

    }

    arr[j + 1] = temp

  }

  return arr

}
