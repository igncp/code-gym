function handleClick() {
  const {count} = this.get();

  this.set({count: count + 1});
}

const data = () => ({
  count: 0,
});

export {
  handleClick,
  data,
};
