import { GetServerSidePropsContext } from "next/types";
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

export const getServerSideProps = async (
  context: GetServerSidePropsContext
) => {
  const [{ getServerModelClient }, { getToken }] = await Promise.all([
    import("../lib/server/client"),
    import("../lib/server/auth"),
  ]);
  const token = getToken(context);
  const client = getServerModelClient({ token });

  const user = await client.getUserMe();
  const customer = await client.getCustomer({ email: user.email });

  const orders: Props["orders"] = [];

  console.log("orders.tsx: customer", customer);

  const props: Props = {
    orders,
  };

  return {
    props,
  };
};
