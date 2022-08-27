import Header from "../components/common/header";

export default function Home({ orders }) {
  console.log("orders.js: orders", orders);
  return (
    <div>
      <Header />
      <h2>Orders</h2>
      <ul>
        {orders.map((order) => {
          return (
            <li key={order.id}>
              {order.total} {order.currencySymbol}
            </li>
          );
        })}
      </ul>
    </div>
  );
}

export const getServerSideProps = async () => {
  const { getWooClient } = await import("../lib/server/wooClient");
  const client = getWooClient();

  const orders = await client.getOrders();

  return {
    props: {
      orders,
    },
  };
};
