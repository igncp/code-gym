<template>
  <div>
    <h1>List:</h1>
    <div v-for="item in items" :key="item.id">{{ item.content }}</div>
  </div>
</template>

<script>
const ListComp = {
  name: "list-comp",
  props: ["items"]
};

export default ListComp;
</script>
