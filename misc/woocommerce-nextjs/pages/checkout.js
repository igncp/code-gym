import Header from "../components/common/header";
import { useRouter } from "next/router";

export default function Home({ paymentGateways }) {
  const router = useRouter();

  const onPay = async () => {
    const response = await fetch("/api/pay");
    console.log("checkout.js: response", response);

    router.push("/success");
  };

  return (
    <div>
      <Header />
      <h2>Payment method</h2>
      <ul>
        {paymentGateways.map((paymentGateway) => {
          return (
            <li key={paymentGateway.id}>
              {paymentGateway.title} - Enabled:{" "}
              {paymentGateway.enabled.toString()}
            </li>
          );
        })}
        <div onClick={onPay} style={{ cursor: "pointer" }}>
          <b>Pay</b>
        </div>
      </ul>

      <main>
        <h2>Checkout</h2>
      </main>
    </div>
  );
}

export const getServerSideProps = async () => {
  const { getWooClient } = await import("../lib/server/wooClient");
  const client = getWooClient();

  const paymentGateways = await client.getPaymentGateways();

  return {
    props: {
      paymentGateways,
    },
  };
};
