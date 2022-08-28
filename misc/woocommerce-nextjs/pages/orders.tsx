import Header from "../components/common/header";
import PageTitle from "../components/common/page-title";
import { OrderSummary } from "../lib/models";

type Props = {
  orders: OrderSummary[];
};

export default function Orders({ orders }: Props) {
  return (
    <div>
      <Header />
      <PageTitle text="Orders" />
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
  const { getServerModelClient } = await import("../lib/server/client");
  const client = getServerModelClient();

  const orders = await client.getOrders();
  const props: Props = {
    orders,
  };

  return {
    props,
  };
};
