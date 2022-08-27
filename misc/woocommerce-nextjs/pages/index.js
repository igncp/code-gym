import Link from "next/link";
import Header from "../components/common/header";

export default function Home({ products }) {
  return (
    <div>
      <Header />

      <main>
        <h2>Products</h2>
        {products.map((product) => {
          return (
            <div key={product.id}>
              <Link href={"/product/" + product.id}>{product.name}</Link>
            </div>
          );
        })}
        <div>
          <Link href={"/checkout"}>Checkout</Link>
        </div>
        <div>
          <Link href={"/orders"}>Orders</Link>
        </div>
      </main>
    </div>
  );
}

export const getServerSideProps = async () => {
  const { getWooClient } = await import("../lib/server/wooClient");
  const client = getWooClient();

  const products = await client.getProducts();

  return {
    props: {
      products,
    },
  };
};
