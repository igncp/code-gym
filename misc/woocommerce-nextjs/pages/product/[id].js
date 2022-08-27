import Header from "../../components/common/header";

export default function Home({ product }) {
  return (
    <div>
      <Header />
      <main>
        <h2>Product</h2>
        <div>{product.name}</div>
      </main>
    </div>
  );
}

export const getServerSideProps = async (context) => {
  const { getWooClient } = await import("../../lib/server/wooClient");
  const client = getWooClient();

  const product = await client.getProduct(context.query.id);

  return {
    props: {
      product,
    },
  };
};
