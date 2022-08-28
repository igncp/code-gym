import { useEffect, useState } from "react";
import Header from "../components/common/header";
import PageTitle from "../components/common/page-title";
import {
  clearCart,
  getCartItemsPopulated,
  PopulatedCart,
} from "../lib/frontend/cart";

export default function Home() {
  const [populatedCart, setPopulatedCart] = useState<PopulatedCart | null>(
    null
  );

  useEffect(() => {
    (async () => {
      setPopulatedCart(await getCartItemsPopulated());
    })();
  }, []);

  const productIds = Object.keys(populatedCart || {});

  const onClearCart = () => {
    clearCart();
    setPopulatedCart(null);
  };

  return (
    <div>
      <Header />
      <PageTitle text="Cart" />
      {productIds.map((productId) => {
        const { product, quantity } = populatedCart![productId];

        return (
          <div key={productId}>
            {product.name} - Quantity: {quantity}
          </div>
        );
      })}
      {!!productIds.length ? (
        <button onClick={onClearCart}>Clear</button>
      ) : (
        <div>No items in the cart</div>
      )}
    </div>
  );
}

export const getServerSideProps = async () => {
  return {
    props: {},
  };
};
