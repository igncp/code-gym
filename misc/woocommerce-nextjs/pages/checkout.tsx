import Header from "../components/common/header";
import { useRouter } from "next/router";
import { PaymentGateway } from "../lib/models";
import PageTitle from "../components/common/page-title";
import { getFEModelClient } from "../lib/frontend/client";
import { getCartItems } from "../lib/frontend/cart";

type Props = {
  paymentGateways: PaymentGateway[];
};

export default function Checkout({ paymentGateways }: Props) {
  const router = useRouter();

  const onPay = async () => {
    const client = getFEModelClient();
    const cart = getCartItems();

    const result = await client.postOrder({
      items: Object.keys(cart).map((productId) => ({
        productId: Number(productId),
        quantity: cart[productId],
      })),
    });

    console.log("checkout.tsx: result", result);

    router.push("/success");
  };

  return (
    <div>
      <Header />
      <PageTitle text="Checkout" />
      <div>Payment Methods</div>
      <ul>
        {paymentGateways.map((paymentGateway) => {
          return (
            <li key={paymentGateway.id}>
              <span
                dangerouslySetInnerHTML={{ __html: paymentGateway.title }}
              />
              - Enabled: {paymentGateway.enabled.toString()}
            </li>
          );
        })}
        <div onClick={onPay} style={{ cursor: "pointer" }}>
          <b>Pay</b>
        </div>
      </ul>
    </div>
  );
}

export const getServerSideProps = async () => {
  const { getServerModelClient } = await import("../lib/server/client");
  const client = getServerModelClient();

  const paymentGateways = await client.getPaymentGateways();

  const props: Props = {
    paymentGateways,
  };

  return {
    props,
  };
};
